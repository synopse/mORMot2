/// Framework Core Cryptographic Process (Hashing and Cypher)
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.crypto;

{
  *****************************************************************************

   High-Performance Cryptographic features shared by all framework units
    - Low-Level Memory Buffers Helper Functions
    - AES Encoding/Decoding with optimized asm and AES-NI support
    - AES-256 Cryptographic Pseudorandom Number Generator (CSPRNG)
    - SHA-2 SHA-3 Secure Hashing
    - HMAC Authentication over SHA and CRC32C
    - PBKDF2 Key Derivation over SHA and CRC32C
    - Digest/Hash to Hexadecimal Text Conversion
    - Deprecated MD5 RC4 SHA-1 Algorithms
    - Deprecated Weak AES/SHA Process

   Optimized x86_64 or i386 asm stubs, featuring e.g. AES-NI, are included.

  *****************************************************************************

  Original Copyright Notices of some Open Source implementations:
  - keccak_mmx: (c) Eric Grange
  - md5_intel: (c) Maxim Masiutin - Ritlabs, SRL
  - sha512-x86: (c) Project Nayuki under MIT license
  - aes_pascal, keccak_pascal: (c) Wolfgang Ehrhardt under zlib license
  - sha512-x64sse4, sha256-sse4, crc32c64: (c) Intel Corporation w/ OS licence
  Maybe with (deep) refactoring. Other routines are our own coding.

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
  mormot.core.text;


type
  /// class of Exceptions raised by this unit
  ESynCrypto = class(ESynException);

{$ifdef ASMX64}
  {$ifdef HASAESNI}
    {$define USEAESNI}
    {$define USEAESNI64}
    {$define USECLMUL} // gf_mul_pclmulqdq() requires some complex opcodes
  {$endif HASAESNI}
  {$ifndef BSD}
    {$define CRC32C_X64} // external crc32_iscsi_01 for win64/lin64
    {$define SHA512_X64} // external sha512_sse4 for win64/lin64
  {$endif BSD}
{$endif ASMX64}

{$ifdef ASMX86}
  {$define USEAESNI}
  {$define USEAESNI32}
  {$ifdef HASAESNI}
    {$define USECLMUL} // gf_mul_pclmulqdq() requires some complex opcodes
  {$endif HASAESNI}
  {$ifndef BSD}
    {$define SHA512_X86} // external sha512-x86 for win32/lin32
  {$endif BSD}
{$endif ASMX86}

{$ifdef MSWINDOWS}
  // on Windows: enable Microsoft AES Cryptographic Provider (XP SP3 and up)
  // - even if those AES engines are slower and closed source (so should better
  // be avoided), we use it for TAesPrng.GetEntropy, as it can't hurt
  {$define USE_PROV_RSA_AES}
{$else}
  {$undef USE_PROV_RSA_AES}
{$endif MSWINDOWS}


{ ****************** Low-Level Memory Buffers Helper Functions }

type
  /// stores an array of THash128 to check for their unicity
  // - used e.g. to implement TAesAbstract.IVHistoryDepth property, but may be
  // also used to efficiently store a list of 128-bit IPv6 addresses
  {$ifdef USERECORDWITHMETHODS}
  THash128History = record
  {$else}
  THash128History = object
  {$endif USERECORDWITHMETHODS}
  private
    Previous: array of THash128Rec;
    Index: integer;
  public
    /// how many THash128 values can be stored
    Depth: integer;
    /// how many THash128 values are currently stored
    Count: integer;
    /// initialize the storage for a given history depth
    // - if Count reaches Depth, then older items will be removed
    procedure Init(size, maxsize: integer);
    /// O(n) fast search of a hash value in the stored entries
    // - returns true if the hash was found, or false if it did not appear
    function Exists(const hash: THash128): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// add a hash value to the stored entries, checking for duplicates
    // - returns true if the hash was added, or false if it did already appear
    function Add(const hash: THash128): boolean;
  end;

/// apply the XOR operation to the supplied binary buffers of 16 bytes
procedure XorBlock16(A, B: PPtrIntArray);
  {$ifdef HASINLINE}inline;{$endif} overload;

/// apply the XOR operation to the supplied binary buffers of 16 bytes
procedure XorBlock16(A, B, C: PPtrIntArray);
 {$ifdef HASINLINE}inline;{$endif} overload;

/// simple XOR encryption according to Cod - not Compression or Stream compatible
// - used in AESFull() for KeySize=32
// - Cod is used to derivate some pseudo-random content from internal constant
// tables, so encryption is weak but fast
procedure XorBlock(p: PIntegerArray; Count, Cod: integer);

/// simple XOR Cypher using Index (=Position in Dest Stream)
// - Compression not compatible with this function: should be applied after
// compress (e.g. as outStream for TAesWriteStream)
// - Stream compatible (with updated Index)
// - used in AES() and TAesWriteStream
// - Index is used to derivate some pseudo-random content from internal
// constant tables, so encryption is weak but fast
procedure XorOffset(P: PByteArray; Index, Count: PtrInt);

/// weak XOR Cypher changing by Count value
// - Compression compatible, since the XOR value is always the same, the
// compression rate will not change a lot
// - this encryption is very weak, so should be used only for basic
// obfuscation, not data protection
procedure XorConst(p: PIntegerArray; Count: integer);

// little endian fast conversion
// - 160 bits = 5 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap160(s, d: PIntegerArray);

// little endian fast conversion
// - 256-bit = 8 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap256(s, d: PIntegerArray);

/// low-level function able to derivate a 0..1 floating-point from 128-bit of data
// - used e.g. by TAesPrng.RandomExt
// - only the lower part of P^ will be used for derivation thanks to AES input
function Hash128ToExt(P: PHash128Rec): TSynExtended;
 {$ifdef FPC} inline; {$endif} { Delphi has troubles inlining floats results }

/// low-level function able to derivate a 0..1 64-bit floating-point from 128-bit of data
// - used e.g. by TAesPrng.RandomDouble
// - only the higher part of P^ will be used for derivation thanks to AES input
function Hash128ToDouble(P: PHash128Rec): double;
 {$ifdef FPC} inline; {$endif}

/// low-level function able to derivate a 0..1 32-bit floating-point from 128-bit of data
// - only the lower part of P^ will be used for derivation thanks to AES input
function Hash128ToSingle(P: PHash128Rec): single;
 {$ifdef FPC} inline; {$endif}

/// simple Adler32 implementation
// - a bit slower than Adler32Asm() version below, but shorter code size
function Adler32Pas(Adler: cardinal; p: pointer; Count: integer): cardinal;

/// fast Adler32 implementation
// - 16-bytes-chunck unrolled asm version
function Adler32Asm(Adler: cardinal; p: pointer; Count: integer): cardinal;
 {$ifndef CPUX86} inline; {$endif}

function Adler32SelfTest: boolean;

/// entry point of the raw MD5 transform function - may be used for low-level use
procedure RawMd5Compress(var Hash; Data: pointer);

/// entry point of the raw SHA-1 transform function - may be used for low-level use
procedure RawSha1Compress(var Hash; Data: pointer);

/// entry point of the raw SHA-256 transform function - may be used for low-level use
procedure RawSha256Compress(var Hash; Data: pointer);

/// entry point of the raw SHA-512 transform function - may be used for low-level use
procedure RawSha512Compress(var Hash; Data: pointer);



{ *************** AES Encoding/Decoding with optimized asm and AES-NI support }

const
  /// hide all AES Context complex code
  AES_CONTEXT_SIZE = 276 + SizeOf(pointer)
    {$ifdef USEAESNI32} + SizeOf(pointer) {$endif};

  /// power of two for a standard AES block size during cypher/uncypher
  // - to be used as 1 shl AesBlockShift or 1 shr AesBlockShift for fast div/mod
  AesBlockShift = 4;

  /// bit mask for fast modulo of AES block size
  AesBlockMod = 15;

  /// maximum AES key size (in bytes)
  AesKeySize = 256 div 8;

type
  /// 128-bit memory block for AES data cypher/uncypher
  TAesBlock = THash128;
  PAesBlock = ^TAesBlock;

  /// 256-bit memory block for maximum AES key storage
  TAesKey = THash256;

type
  /// handle AES cypher/uncypher
  // - this is the default Electronic codebook (ECB) mode
  // - this class will use AES-NI hardware instructions, if available
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  TAes = object
  private
    Context: packed array[1..AES_CONTEXT_SIZE] of byte;
  public
    /// Initialize AES contexts for cypher
    // - first method to call before using this object for encryption
    // - KeySize is in bits, i.e. 128, 192 or 256
    function EncryptInit(const Key; KeySize: cardinal): boolean;
    /// encrypt an AES data block into another data block
    procedure Encrypt(const BI: TAesBlock; var BO: TAesBlock); overload;
      {$ifdef FPC}inline;{$endif}
    /// encrypt an AES data block
    procedure Encrypt(var B: TAesBlock); overload;
      {$ifdef FPC}inline;{$endif}

    /// Initialize AES contexts for uncypher
    // - first method to call before using this object for decryption
    // - KeySize is in bits, i.e. 128, 192 or 256
    function DecryptInit(const Key; KeySize: cardinal): boolean;
    /// Initialize AES contexts for uncypher, from another TAes.EncryptInit
    function DecryptInitFrom(const Encryption: TAes;
      const Key; KeySize: cardinal): boolean;
    /// decrypt an AES data block
    procedure Decrypt(var B: TAesBlock); overload;
      {$ifdef FPC}inline;{$endif}
    /// decrypt an AES data block into another data block
    procedure Decrypt(const BI: TAesBlock; var BO: TAesBlock); overload;
      {$ifdef FPC}inline;{$endif}

    /// Finalize AES contexts for both cypher and uncypher
    // - would fill the TAes instance with zeros, for (paranoid) safety
    procedure Done;  {$ifdef FPC}inline;{$endif}

    /// generic initialization method for AES contexts
    // - call either EncryptInit() either DecryptInit() method
    function DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAesBlock; out oIn, oOut: PAesBLock;
      Count: integer; doEncrypt: boolean); overload;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAesBlock; Count: integer;
      doEncrypt: boolean); overload;
    /// performs AES-OFB encryption and decryption on whole blocks
    // - may be called instead of TAesOfb when only a raw TAes is available
    // - as used e.g. by mormot.db.raw.sqlite3.static.pas for its DB encryption
    // - this method is thread-safe, and is optimized for AES-NI on x86_64
    procedure DoBlocksOfb(iv: PAesBlock; src, dst: pointer;
      blockcount: PtrUInt);
    /// TRUE if the context was initialized via EncryptInit/DecryptInit
    function Initialized: boolean;
      {$ifdef FPC}inline;{$endif}
    /// return TRUE if the AES-NI instruction sets are available on this CPU
    function UsesAesni: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the key size in bits (128/192/256)
    function KeyBits: integer;
      {$ifdef FPC}inline;{$endif}
  end;

  /// points to a TAes encryption/decryption instance
  PAes = ^TAes;


  /// low-level AES-GCM processing
  // - implements standard AEAD (authenticated-encryption with associated-data)
  // algorithm, as defined by NIST Special Publication 800-38D
  // - will use AES-NI and CLMUL Intel/AMD opcodes if available
  TAesGcmEngine = object
  private
    /// standard AES encryption context
    actx: TAes;
    /// ghash value of the Authentication Data
    aad_ghv: TAesBlock;
    /// ghash value of the Ciphertext
    txt_ghv: TAesBlock;
    /// ghash H current value
    ghash_h: TAesBlock;
    /// number of Authentication Data bytes processed
    aad_cnt: TQWordRec;
    /// number of bytes of the Ciphertext
    atx_cnt: TQWordRec;
    /// initial 32-bit ctr val - to be reused in Final()
    y0_val: integer;
    /// current 0..15 position in encryption block
    blen: byte;
    /// the state of this context
    flags: set of (flagFinalComputed, flagFlushed, flagCLMUL);
    /// 4KB lookup table for fast Galois Finite Field multiplication
    // - is defined as last field of the object for better code generation
    gf_t4k: array[byte] of THash128Rec;
    /// build the gf_t4k[] internal table - assuming set to zero by caller
    procedure Make4K_Table;
    /// compute a * ghash_h in Galois Finite Field 2^128 using gf_t4k[]
    procedure gf_mul_h_pas(var a: TAesBlock);
    /// low-level AES-CTR encryption
    procedure internal_crypt(ptp, ctp: PByte; ILen: PtrUInt);
    /// low-level GCM authentication
    procedure internal_auth(ctp: PByte; ILen: PtrUInt;
      var ghv: TAesBlock; var gcnt: TQWordRec);
  public
    /// initialize the AES-GCM structure for the supplied Key
    function Init(const Key; KeyBits: PtrInt): boolean;
    /// start AES-GCM encryption with a given Initialization Vector
    // - IV_len is in bytes use 12 for exact IV setting, otherwise the
    // supplied buffer will be hashed using gf_mul_h()
    function Reset(pIV: pointer; IV_len: PtrInt): boolean;
    /// encrypt a buffer with AES-GCM, updating the associated authentication data
    function Encrypt(ptp, ctp: Pointer; ILen: PtrInt): boolean;
    /// decrypt a buffer with AES-GCM, updating the associated authentication data
    // - also validate the GMAC with the supplied ptag/tlen if ptag<>nil,
    // and skip the AES-CTR phase if the authentication doesn't match
    function Decrypt(ctp, ptp: Pointer; ILen: PtrInt;
      ptag: pointer = nil; tlen: PtrInt = 0): boolean;
    /// append some data to be authenticated, but not encrypted
    function Add_AAD(pAAD: pointer; aLen: PtrInt): boolean;
    /// finalize the AES-GCM encryption, returning the authentication tag
    // - will also flush the AES context to avoid forensic issues, unless
    // andDone is forced to false
    function Final(out tag: TAesBlock; andDone: boolean = true): boolean;
    /// flush the AES context to avoid forensic issues
    // - do nothing if Final() has been already called
    procedure Done;
    /// single call AES-GCM encryption and authentication process
    function FullEncryptAndAuthenticate(const Key; KeyBits: PtrInt;
      pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt;
      ptp, ctp: Pointer; pLen: PtrInt; out tag: TAesBlock): boolean;
    /// single call AES-GCM decryption and verification process
    function FullDecryptAndVerify(const Key; KeyBits: PtrInt;
      pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt;
      ctp, ptp: Pointer; pLen: PtrInt; ptag: pointer; tLen: PtrInt): boolean;
  end;


  /// class-reference type (metaclass) of an AES cypher/uncypher
  TAesAbstractClass = class of TAesAbstract;

  /// used internally by TAesAbstract to detect replay attacks
  // - when EncryptPkcs7/DecryptPkcs7 are used with IVAtBeginning=true, and
  // IVReplayAttackCheck property contains repCheckedIfAvailable or repMandatory
  // - EncryptPkcs7 will encrypt this record (using the global shared
  // AESIVCTR_KEY over AES-128) to create a random IV, as a secure
  // cryptographic pseudorandom number generator (CSPRNG), nonce and ctr
  // ensuring 96 bits of entropy
  // - DecryptPkcs7 will decode and ensure that the IV has an increasing CTR
  // - memory size matches an TAesBlock on purpose, for direct encryption
  TAesIVCtr = packed record
    /// 8 bytes of random value
    nonce: QWord;
    /// contains the crc32c hash of the block cipher mode (e.g. 'AESCFB')
    // - when magic won't match (i.e. in case of mORMot revision < 3063), the
    // check won't be applied in DecryptPkcs7: this security feature is
    // backward compatible if IVReplayAttackCheck is repCheckedIfAvailable,
    // but will fail for repMandatory
    magic: cardinal;
    /// an increasing counter, used to detect replay attacks
    // - is set to a 32-bit random value at initialization
    // - is increased by one for every EncryptPkcs7, so can be checked against
    // replay attack in DecryptPkcs7, and implement a safe CSPRNG for stored IV
    ctr: cardinal;
  end;

  /// how TAesAbstract.DecryptPkcs7 should detect replay attack
  // - repNoCheck and repCheckedIfAvailable will be compatible with older
  // versions of the protocol, but repMandatory will reject any encryption
  // without the TAesIVCtr algorithm
  TAesIVReplayAttackCheck = (
    repNoCheck,
    repCheckedIfAvailable,
    repMandatory);

  {$M+}

  /// handle AES cypher/uncypher with chaining
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAesEcb, TAesCbc, TAesCfb, TAesOfb and TAesCtr classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  TAesAbstract = class
  protected
    fKeySize: cardinal;
    fKeySizeBytes: cardinal;
    fKey: TAesKey;
    fIV: TAesBlock;
    fIVCtr: TAesIVCtr;
    fIVCtrState: (ctrUnknown, ctrUsed, ctrNotused);
    fIVHistoryDec: THash128History;
    fIVReplayAttackCheck: TAesIVReplayAttackCheck;
    procedure SetIVHistory(aDepth: integer);
    procedure SetIVCtr;
    function DecryptPkcs7Len(var InputLen, ivsize: integer; Input: pointer;
      IVAtBeginning, RaiseESynCryptoOnError: boolean): boolean;
  public
    /// Initialize AES context for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. either 128, 192 or 256
    // - warning: aKey is an untyped constant, i.e. expects a raw set of memory
    // bytes: do NOT use assign it with a string or a TBytes instance: you would
    // use the pointer to the data as key - either digest the string via
    // CreateFromPbkdf2 or use Create(TBytes)
    constructor Create(const aKey; aKeySizeBits: cardinal); reintroduce; overload; virtual;
    /// Initialize AES context for AES-128 cypher
    // - first method to call before using this class
    // - just a wrapper around Create(aKey,128);
    constructor Create(const aKey: THash128); reintroduce; overload;
    /// Initialize AES context for AES-256 cypher
    // - first method to call before using this class
    // - just a wrapper around Create(aKey,256);
    constructor Create(const aKey: THash256); reintroduce; overload;
    /// Initialize AES context for AES-256 cypher
    // - first method to call before using this class
    // - here, aKey is expected to be a 128-bit, 192-bit or 256-bit TBytes,
    // i.e. with 16, 24 or 32 bytes
    constructor Create(const aKey: TBytes); reintroduce; overload;
    /// Initialize AES context for cypher, from some TAesPrng random bytes
    // - may be used to hide some sensitive information from memory, like
    // CryptDataForCurrentUser but with a temporary key
    constructor CreateTemp(aKeySize: cardinal);
    {$ifndef PUREMORMOT2}
    /// Initialize AES context for cypher, from SHA-256 hash
    // - here the Key is supplied as a string, and will be hashed using SHA-256
    // via the Sha256Weak proprietary algorithm - to be used only for backward
    // compatibility of existing code
    // - since Sha256Weak() is deprecated, consider using the more secure
    // (and more standard and proven) CreateFromPbkdf2() constructor
    constructor CreateFromSha256(const aKey: RawUtf8); deprecated;
    {$endif PUREMORMOT2}
    /// Initialize AES context for cypher, from PBKDF2_HMAC_SHA256 derivation
    // - here the Key is supplied as a string, and will be hashed using
    // PBKDF2_HMAC_SHA256 with the specified salt and rounds
    constructor CreateFromPbkdf2(const aKey: RawUtf8; const aSalt: RawByteString;
      aRounds: integer);
    /// compute a class instance similar to this one
    // - could be used to have a thread-safe re-use of a given encryption key
    function Clone: TAesAbstract; virtual;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - this default implementation calls Clone, but CFB/OFB/CTR chaining modes
    // using only AES encryption (i.e. inheriting from TAesAbstractEncryptOnly)
    // will return self to avoid creating two instances
    // - warning: to be used only with IVAtBeginning=false
    function CloneEncryptDecrypt: TAesAbstract; virtual;
    /// release the used instance memory and resources
    // - also fill the secret fKey buffer with zeros, for safety
    destructor Destroy; override;

    /// perform the AES cypher in the corresponding mode
    // - when used in block chaining mode, you should have set the IV property
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); virtual; abstract;
    /// perform the AES un-cypher in the corresponding mode
    // - when used in block chaining mode, you should have set the IV property
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); virtual; abstract;

    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer - this IV may
    // contain an internal encrypted CTR, to detect any replay attack attempt,
    // if IVReplayAttackCheck is set to repCheckedIfAvailable or repMandatory
    function EncryptPkcs7(const Input: RawByteString;
      IVAtBeginning: boolean = false): RawByteString; overload;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, the Initialization Vector will be taken
    // from the beginning of the input binary buffer - if IVReplayAttackCheck is
    // set, this IV will be validated to contain an increasing encrypted CTR,
    // and raise an ESynCrypto when a replay attack attempt is detected
    // - if RaiseESynCryptoOnError=false, returns '' on any decryption error
    function DecryptPkcs7(const Input: RawByteString; IVAtBeginning: boolean = false;
      RaiseESynCryptoOnError: boolean = true): RawByteString; overload;
    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer - this IV may
    // contain an internal encrypted CTR, to detect any replay attack attempt,
    // if IVReplayAttackCheck is set to repCheckedIfAvailable or repMandatory
    function EncryptPkcs7(const Input: TBytes;
      IVAtBeginning: boolean = false): TBytes; overload;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, the Initialization Vector will be taken
    // from the beginning of the input binary buffer - if IVReplayAttackCheck is
    // set, this IV will be validated to contain an increasing encrypted CTR,
    // and raise an ESynCrypto when a replay attack attempt is detected
    // - if RaiseESynCryptoOnError=false, returns [] on any decryption error
    function DecryptPkcs7(const Input: TBytes; IVAtBeginning: boolean = false;
      RaiseESynCryptoOnError: boolean = true): TBytes; overload;

    /// compute how many bytes would be needed in the output buffer, when
    // encrypte using a PKCS7 padding pattern
    // - could be used to pre-compute the OutputLength for EncryptPkcs7Buffer()
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    function EncryptPkcs7Length(InputLen: cardinal; IVAtBeginning: boolean): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - use EncryptPkcs7Length() function to compute the actual needed length
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer - this IV will in
    // fact contain an internal encrypted CTR, to detect any replay attack attempt
    // - returns TRUE on success, FALSE if OutputLen is not correct - you should
    // use EncryptPkcs7Length() to compute the exact needed number of bytes
    function EncryptPkcs7Buffer(Input, Output: pointer; InputLen, OutputLen: cardinal;
      IVAtBeginning: boolean): boolean;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 padding is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer; note this method uses the padding only, not the whole
    // PKCS#7 Cryptographic Message Syntax
    // - if IVAtBeginning is TRUE, the Initialization Vector will be taken
    // from the beginning of the input binary buffer  - this IV will in fact
    // contain an internal encrypted CTR, to detect any replay attack attempt
    // - if RaiseESynCryptoOnError=false, returns '' on any decryption error
    function DecryptPkcs7Buffer(Input: pointer; InputLen: integer;
      IVAtBeginning: boolean; RaiseESynCryptoOnError: boolean = true): RawByteString;

    /// initialize AEAD (authenticated-encryption with associated-data) nonce
    // - i.e. setup 256-bit MAC computation during next Encrypt/Decrypt call
    // - may be used e.g. for AES-GCM or our custom AES-CTR modes
    // - default implementation, for a non AEAD protocol, returns false
    function MacSetNonce(const aKey: THash256; aAssociated: pointer = nil;
      aAssociatedLen: integer = 0): boolean; virtual;
    /// returns AEAD (authenticated-encryption with associated-data) MAC
    /// - i.e. optional 256-bit MAC computation during last Encrypt/Decrypt call
    // - may be used e.g. for AES-GCM or our custom AES-CTR modes
    // - default implementation, for a non AEAD protocol, returns false
    function MacGetLast(out aCRC: THash256): boolean; virtual;
    /// validate if the computed AEAD MAC matches the expected supplied value
    // - is just a wrapper around MacGetLast() and IsEqual() functions
    function MACEquals(const aCRC: THash256): boolean; virtual;
    /// validate if an encrypted buffer matches the stored AEAD MAC
    // - expects the 256-bit MAC, as returned by MacGetLast, to be stored after
    // the encrypted data
    // - default implementation, for a non AEAD protocol, returns false
    function MacCheckError(aEncrypted: pointer; Count: cardinal): boolean; virtual;
    /// perform one step PKCS7 encryption/decryption and authentication from
    // a given 256-bit key over a small memory block
    // - returns '' on any (MAC) issue during decryption (Encrypt=false) or if
    // this class does not support AEAD MAC
    // - supplied Data is expected to be small (<128 bytes), as used e.g. by
    // CryptDataForCurrentUser()
    // - do not use this abstract class method, but inherited TAesCfbCrc/TAesOfbCrc
    // - will store a header with its own CRC, so detection of most invalid
    // formats (e.g. from fuzzing input) will occur before any AES/MAC process
    class function MacEncrypt(const Data: RawByteString; const Key: THash256;
      Encrypt: boolean): RawByteString; overload;
    /// perform one step PKCS7 encryption/decryption and authentication from
    // a given 128-bit key over a small memory block
    // - returns '' on any (MAC) issue during decryption (Encrypt=false) or if
    // this class does not support AEAD MAC
    // - supplied Data is expected to be small (<128 bytes), as used e.g. by
    // CryptDataForCurrentUser()
    // - do not use this abstract class method, but inherited TAesCfbCrc/TAesOfbCrc
    // - will store a header with its own CRC, so detection of most invalid
    // formats (e.g. from fuzzing input) will occur before any AES/MAC process
    class function MacEncrypt(const Data: RawByteString; const Key: THash128;
      Encrypt: boolean): RawByteString; overload;
    /// perform one step PKCS7 encryption/decryption and authentication with
    // the curent AES instance over a small memory block
    // - returns '' on any (MAC) issue during decryption (Encrypt=false) or if
    // this class does not support AEAD MAC
    // - as used e.g. by CryptDataForCurrentUser()
    // - do not use this abstract class method, but inherited TAesCfbCrc/TAesOfbCrc
    // - will store a header with its own CRC, so detection of most invalid
    // formats (e.g. from fuzzing input) will occur before any AES/MAC process
    // - AEAD associated Data is expected to be small (up to 100 bytes)
    function MacAndCrypt(const Data: RawByteString; Encrypt: boolean): RawByteString;

    {$ifndef PUREMORMOT2}
    /// simple wrapper able to cypher/decypher any in-memory content
    // - here data variables could be text or binary
    // - use StringToUtf8() to define the Key parameter from a VCL string
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    // - will use Sha256Weak() and PKCS7 padding with the current class mode,
    // so is to be considered as deprecated
    class function SimpleEncrypt(const Input, Key: RawByteString;
      Encrypt: boolean; IVAtBeginning: boolean = false;
      RaiseESynCryptoOnError: boolean = true): RawByteString; overload;
    /// simple wrapper able to cypher/decypher any file content
    // - just a wrapper around SimpleEncrypt() and StringFromFile/FileFromString
    // - use StringToUtf8() to define the Key parameter from a VCL string
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    // - will use Sha256Weak() and PKCS7 padding with the current class mode,
    // so is marked as deprecated
    class function SimpleEncryptFile(const InputFile, OutputFile: TFileName;
      const Key: RawByteString; Encrypt: boolean; IVAtBeginning: boolean = false;
      RaiseESynCryptoOnError: boolean = true): boolean; overload; deprecated;
    {$endif PUREMORMOT2}
    /// simple wrapper able to cypher/decypher any in-memory content
    // - here data variables could be text or binary
    // - you could use e.g. THMAC_SHA256 to safely compute the Key/KeySize value
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    class function SimpleEncrypt(const Input: RawByteString; const Key;
      KeySize: integer; Encrypt: boolean; IVAtBeginning: boolean = false;
      RaiseESynCryptoOnError: boolean = true): RawByteString; overload;
    /// simple wrapper able to cypher/decypher any file content
    // - just a wrapper around SimpleEncrypt() and StringFromFile/FileFromString
    // - you could use e.g. THMAC_SHA256 to safely compute the Key/KeySize value
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    // - will use Sha256Weak() and PKCS7 padding with the current class mode
    class function SimpleEncryptFile(const InputFile, Outputfile: TFileName;
      const Key; KeySize: integer; Encrypt: boolean; IVAtBeginning: boolean = false;
      RaiseESynCryptoOnError: boolean = true): boolean; overload;
    //// returns e.g. 'aes128cfb' or '' if nil
    function AlgoName: TShort16;

    /// associated Key Size, in bits (i.e. 128,192,256)
    property KeySize: cardinal
      read fKeySize;
    /// associated Initialization Vector
    // - all modes (except ECB) do expect an IV to be supplied for chaining,
    // before any encryption or decryption is performed
    // - you could also use PKCS7 encoding with IVAtBeginning=true option
    property IV: TAesBlock
      read fIV write fIV;
    /// let IV detect replay attack for EncryptPkcs7 and DecryptPkcs7
    // - if IVAtBeginning=true and this property is set, EncryptPkcs7 will
    // store a random IV from an internal CTR, and DecryptPkcs7 will check this
    // incoming IV CTR consistency, and raise an ESynCrypto exception on failure
    // - leave it to its default repNoCheck if the very same TAesAbstract
    // instance is expected to be used with several sources, by which the IV CTR
    // will be unsynchronized
    // - security warning: by design, this is NOT cautious with CBC chaining:
    // you should use it only with CFB, OFB or CTR mode, since the IV sequence
    // will be predictable if you know the fixed AES private key of this unit,
    // but the IV sequence features uniqueness as it is generated by a good PRNG -
    // see http://crypto.stackexchange.com/q/3515
    property IVReplayAttackCheck: TAesIVReplayAttackCheck
      read fIVReplayAttackCheck write fIVReplayAttackCheck;
    /// maintains an history of previous IV, to avoid re-play attacks
    // - only useful when EncryptPkcs7/DecryptPkcs7 are used with
    // IVAtBeginning=true, and IVReplayAttackCheck is left to repNoCheck
    property IVHistoryDepth: integer
      read fIVHistoryDec.Depth write SetIVHistory;
  end;

  {$M-}

  /// handle AES cypher/uncypher with chaining with out own optimized code
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAesEcb, TAesCbc, TAesCfb, TAesOfb and TAesCtr classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  // - this class will use AES-NI hardware instructions, if available
  // - those classes are re-entrant, i.e. that you can call the Encrypt*
  // or Decrypt* methods on the same instance several times
  TAesAbstractSyn = class(TAesAbstract)
  protected
    fIn, fOut: PAesBlock;
    fCV: TAesBlock;
    fAes: TAes;
    fAesInit: (initNone, initEncrypt, initDecrypt);
    procedure EncryptInit;
    procedure DecryptInit;
    procedure TrailerBytes(count: cardinal);
  public
    /// creates a new instance with the very same values
    // - by design, our classes will use TAes stateless context, so this method
    // will just copy the current fields to a new instance, by-passing
    // the key creation step
    function Clone: TAesAbstract; override;
    /// release the used instance memory and resources
    // - also fill the TAes instance with zeros, for safety
    destructor Destroy; override;
    /// perform the AES cypher in the corresponding mode
    // - this abstract method will set CV from fIV property, and fIn/fOut
    // from BufIn/BufOut
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the corresponding mode
    // - this abstract method will set CV from fIV property, and fIn/fOut
    // from BufIn/BufOut
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// read-only access to the internal CV block, which may be have just been
    // used by Encrypt/Decrypt methods
    property CV: TAesBlock
      read fCV;
  end;

  /// handle AES cypher/uncypher without chaining (ECB)
  // - this mode is known to be less secure than the others
  // - IV property should be set to a fixed value to encode the trailing bytes
  // of the buffer by a simple XOR - but you should better use the PKC7 pattern
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! ECB128: 19.70ms in x86 optimized code, 6.97ms with AES-NI
  TAesEcb = class(TAesAbstractSyn)
  public
    /// perform the AES cypher in the ECB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Cipher-block chaining (CBC)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! CBC192: 24.91ms in x86 optimized code, 9.75ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  TAesCbc = class(TAesAbstractSyn)
  public
    /// perform the AES cypher in the CBC mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CBC mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// abstract parent class for chaining modes using only AES encryption
  TAesAbstractEncryptOnly = class(TAesAbstractSyn)
  public
    /// Initialize AES context for cypher
    // - will pre-generate the encryption key (aKeySize in bits, i.e. 128,192,256)
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - will return self to avoid creating two instances
    // - warning: to be used only with IVAtBeginning=false
    function CloneEncryptDecrypt: TAesAbstract; override;
  end;

  /// handle AES cypher/uncypher with Cipher feedback (CFB)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! CFB128: 22.25ms in x86 optimized code, 9.29ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  TAesCfb = class(TAesAbstractEncryptOnly)
  public
    /// perform the AES cypher in the CFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Output feedback (OFB)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! OFB256: 27.69ms in x86 optimized code, 9.94ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  // - TAesOfb 128/256 have an optimized asm version under x86_64 + AES_NI
  TAesOfb = class(TAesAbstractEncryptOnly)
  public
    /// perform the AES cypher in the OFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the OFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with 64-bit Counter mode (CTR)
  // - the CTR will use a counter in bytes 7..0 by default - which is safe
  // but not standard - call ComposeIV() to change e.g. to NIST behavior
  // - this class will use AES-NI hardware instructions, e.g.
  // ! CTR256: 28.13ms in x86 optimized code, 10.63ms with AES-NI
  // - expect IV to be set before process, or IVAtBeginning=true
  TAesCtr = class(TAesAbstractEncryptOnly)
  protected
    fCTROffset, fCTROffsetMin: PtrInt;
  public
    /// Initialize AES context for cypher
    // - will pre-generate the encryption key (aKeySize in bits, i.e. 128,192,256)
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// defines how the IV is set and updated in CTR mode
    // - default (if you don't call this method) uses a Counter in bytes 7..0
    // - you can specify startup Nonce and Counter, and the Counter position
    // - NonceLen + CounterLen should be 16 - otherwise it fails and returns false
    function ComposeIV(Nonce, Counter: PAesBlock; NonceLen, CounterLen: integer;
      LSBCounter: boolean): boolean; overload;
    /// defines how the IV is set and updated in CTR mode
    // - you can specify startup Nonce and Counter, and the Counter position
    // - Nonce + Counter lengths should add to 16 - otherwise returns false
    function ComposeIV(const Nonce, Counter: TByteDynArray;
      LSBCounter: boolean): boolean; overload;
    /// perform the AES cypher in the CTR mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CTR mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// internal 256-bit structure used for TAesAbstractAead MAC storage
  TAesMac256 = record
    /// the AES-encrypted MAC of the plain content
    // - plain text digital signature, to perform message authentication
    // and integrity
    plain: THash128;
    /// the plain MAC of the encrypted content
    // - encrypted text digital signature, to check for errors,
    // with no compromission of the plain content
    encrypted: THash128;
  end;

  /// AEAD (authenticated-encryption with associated-data) abstract class
  // - perform AES encryption and on-the-fly MAC computation, i.e. computes
  // a proprietary 256-bit MAC during AES cyphering, as 128-bit CRC of the
  // encrypted data and 128-bit CRC of the plain data, seeded from a Key
  // - the 128-bit CRC of the plain text is then encrypted using the current AES
  // engine, so returned 256-bit MAC value has cryptographic level, and ensure
  // data integrity, authenticity, and check against transmission errors
  TAesAbstractAead = class(TAesAbstractEncryptOnly)
  protected
    fMac, fMacKey: TAesMac256;
  public
    /// release the used instance memory and resources
    // - also fill the internal internal MAC hashes with zeros, for safety
    destructor Destroy; override;
    /// initialize 256-bit MAC computation for next Encrypt/Decrypt call
    // - initialize the internal fMacKey property, and returns true
    // - only the plain text crc is seeded from aKey - encrypted message crc
    // will use -1 as fixed seed, to avoid aKey compromission
    // - should be set with a new MAC key value before each message, to avoid
    // replay attacks (as called from TEcdheProtocol.SetKey)
    function MacSetNonce(const aKey: THash256; aAssociated: pointer = nil;
      aAssociatedLen: integer = 0): boolean; override;
    /// returns 256-bit MAC computed during last Encrypt/Decrypt call
    // - encrypt the internal fMac property value using the current AES cypher
    // on the plain content and returns true; only the plain content CRC-128 is
    // AES encrypted, to avoid reverse attacks against the known encrypted data
    function MacGetLast(out aCRC: THash256): boolean; override;
    /// validate if an encrypted buffer matches the stored MAC
    // - expects the 256-bit MAC, as returned by MacGetLast, to be stored after
    // the encrypted data
    // - returns true if the 128-bit CRC of the encrypted text matches the
    // supplied buffer, ignoring the 128-bit CRC of the plain data
    // - since it is easy to forge such 128-bit CRC, it will only indicate
    // that no transmission error occured, but won't be an integrity or
    // authentication proof (which will need full Decrypt + MacGetLast)
    // - may use any MacSetNonce() aAssociated value
    function MacCheckError(aEncrypted: pointer; Count: cardinal): boolean; override;
  end;

  /// AEAD combination of AES with Cipher feedback (CFB) and 256-bit MAC
  // - this class will use AES-NI and CRC32C hardware instructions, if available
  // - expect IV to be set before process, or IVAtBeginning=true
  TAesCfbCrc = class(TAesAbstractAead)
  public
    /// perform the AES cypher in the CFB mode, and compute a 256-bit MAC
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CFB mode, and compute 256-bit MAC
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// AEAD combination of AES with Output feedback (OFB) and 256-bit MAC
  // - this class will use AES-NI and CRC32C hardware instructions, if available
  // - expect IV to be set before process, or IVAtBeginning=true
  TAesOfbCrc = class(TAesAbstractAead)
  public
    /// perform the AES cypher in the OFB mode, and compute a 256-bit MAC
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the OFB mode, and compute a 256-bit MAC
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES-GCM cypher/uncypher with built-in authentication
  // - implements AEAD (authenticated-encryption with associated-data) methods
  // like MacEncrypt/MacCheckError
  // - this class will use AES-NI hardware instructions, if available
  TAesGcm = class(TAesAbstract)
  protected
    fAes: TAesGcmEngine;
    fContext: (ctxNone, ctxEncrypt, ctxDecrypt); // used to call AES.Reset()
  public
    /// Initialize the AES-GCM context for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// creates a new instance with the very same values
    // - by design, our classes will use TAesGcmEngine stateless context, so
    // this method will just copy the current fields to a new instance,
    // by-passing the key creation step
    function Clone: TAesAbstract; override;
    /// release the used instance memory and resources
    // - also fill the internal TAes instance with zeros, for safety
    destructor Destroy; override;
    /// perform the AES-GCM cypher and authentication
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher and authentication
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// prepare the AES-GCM process before Encrypt/Decrypt is called
    // - aKey is not used: AES-GCM has its own nonce setting algorithm, and
    // the IV will be set from random value by EncryptPkcs7()
    // - will just include any supplied associated data to the GMAC tag
    function MacSetNonce(const aKey: THash256; aAssociated: pointer = nil;
      aAssociatedLen: integer = 0): boolean; override;
    /// returns AEAD (authenticated-encryption with associated-data) MAC
    /// - only the lower 128-bit (THash256.Lo) of aCRC is filled with the GMAC
    function MacGetLast(out aCRC: THash256): boolean; override;
    /// validate if an encrypted buffer matches the stored AEAD MAC
    // - since AES-GCM is a one pass process, always assume the content is fine
    // and returns true - we don't know the IV at this time
    function MacCheckError(
      aEncrypted: pointer; Count: cardinal): boolean; override;
  end;


{$ifdef USE_PROV_RSA_AES}

type
  /// handle AES cypher/uncypher using Windows CryptoApi and the
  // official Microsoft AES Cryptographic Provider (PROV_RSA_AES)
  // - see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa386979
  // - timing of our optimized asm versions, for small (<=8KB) block processing
  // (similar to standard web pages or most typical JSON/XML content),
  // benchmarked on a Core i7 notebook and compiled as Win32 platform:
  // ! AES128 - ECB:79.33ms CBC:83.37ms CFB:80.75ms OFB:78.98ms CTR:80.45ms
  // ! AES192 - ECB:91.16ms CBC:96.06ms CFB:96.45ms OFB:92.12ms CTR:93.38ms
  // ! AES256 - ECB:103.22ms CBC:119.14ms CFB:111.59ms OFB:107.00ms CTR:110.13ms
  // - timing of the same process, using CryptoApi official PROV_RSA_AES provider:
  // ! AES128 - ECB_API:102.88ms CBC_API:124.91ms
  // ! AES192 - ECB_API:115.75ms CBC_API:129.95ms
  // ! AES256 - ECB_API:139.50ms CBC_API:154.02ms
  // - but the CryptoApi does not supports AES-NI, whereas our classes handle it,
  // with a huge speed benefit
  // - under Win64, the official CryptoApi is slower our x86_64 asm version,
  // and the Win32 version of CryptoApi itself, but slower than our AES-NI code
  // ! AES128 - ECB:107.95ms CBC:112.65ms CFB:109.62ms OFB:107.23ms CTR:109.42ms
  // ! AES192 - ECB:130.30ms CBC:133.04ms CFB:128.78ms OFB:127.25ms CTR:130.22ms
  // ! AES256 - ECB:145.33ms CBC:147.01ms CFB:148.36ms OFB:145.96ms CTR:149.67ms
  // ! AES128 - ECB_API:89.64ms CBC_API:100.84ms
  // ! AES192 - ECB_API:99.05ms CBC_API:105.85ms
  // ! AES256 - ECB_API:107.11ms CBC_API:118.04ms
  // - in practice, you could forget about using the CryptoApi, unless you are
  // required to do so, for legal/corporate reasons
  TAesAbstractApi = class(TAesAbstract)
  protected
    fKeyHeader: packed record
      bType: byte;
      bVersion: byte;
      reserved: word;
      aiKeyAlg: cardinal;
      dwKeyLength: cardinal;
    end;
    fKeyHeaderKey: TAesKey; // should be just after fKeyHeader record
    fKeyCryptoApi: pointer;
    fInternalMode: cardinal;
    procedure InternalSetMode; virtual; abstract;
    procedure EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal;
      DoEncrypt: boolean);
  public
    /// Initialize AES context for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// release the AES execution context
    destructor Destroy; override;
    /// perform the AES cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAesAbstractSyn classes: you should better use PKC7 padding instead
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAesAbstractSyn classes: you should better use PKC7 padding instead
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher without chaining (ECB) using Windows CryptoApi
  TAesEcbApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_ECB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher-block chaining (CBC) using Windows CryptoApi
  TAesCbcApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_CBC
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher feedback (CFB) using Windows CryptoApi
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not return
  // expected values for CFB
  TAesCfbApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_CFB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Output feedback (OFB) using Windows CryptoApi
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not implement
  // this mode, and returns a NTE_BAD_ALGID error
  TAesOfbApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_OFB
    procedure InternalSetMode; override;
  end;

{$endif USE_PROV_RSA_AES}

var
  /// 128-bit random AES-128 entropy key for TAesAbstract.IVReplayAttackCheck
  // - as used internally by AesIVCtrEncryptDecrypt() function
  // - you may customize this secret for your own project, but be aware that
  // it will affect all TAesAbstract instances, so should match on all ends
  AESIVCTR_KEY: TBlock128 = (
    $ce5d5e3e, $26506c65, $568e0092, $12cce480);

/// global shared function which may encrypt or decrypt any 128-bit block
// using AES-128 and the global AESIVCTR_KEY
procedure AesIVCtrEncryptDecrypt(const BI; var BO; DoEncrypt: boolean);

function ToText(chk: TAesIVReplayAttackCheck): PShortString; overload;

function AesTablesTest: boolean;

{$ifndef PUREMORMOT2}

var
  /// the AES-256 encoding class used by CompressShaAes() global function
  // - use any of the implementation classes, corresponding to the chaining
  // mode required - TAesEcb, TAesCbc, TAesCfb, TAesOfb and TAesCtr classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  // - set to the secure and efficient CFB mode by default
  CompressShaAesClass: TAesAbstractClass = TAesCfb;

/// set an text-based encryption key for CompressShaAes() global function
// - will compute the key via Sha256Weak() and set CompressShaAesKey
// - the key is global to the whole process
procedure CompressShaAesSetKey(const Key: RawByteString;
  AesClass: TAesAbstractClass = nil);

/// encrypt data content using the AES-256/CFB algorithm, after SynLZ compression
// - as expected by THttpSocket.RegisterCompress()
// - will return 'synshaaes' as ACCEPT-ENCODING: header parameter
// - will use global CompressShaAesKey / CompressShaAesClass variables to be set
// according to the expected algorithm and Key e.g. via a call to CompressShaAesSetKey()
// - if you want to change the chaining mode, you can customize the global
// CompressShaAesClass variable to the expected TAes* class name
// - will store a hash of both cyphered and clear stream: if the
// data is corrupted during transmission, will instantly return ''
function CompressShaAes(var Data: RawByteString; Compress: boolean): RawUtf8;

{$endif PUREMORMOT2}


{ ************* AES-256 Cryptographic Pseudorandom Number Generator (CSPRNG) }

type
  /// thread-safe class containing a TAes encryption/decryption engine
  TAesLocked = class
  protected
    fSafe: TRTLCriticalSection; // no need of TSynLocker padding
    fAes: TAes;
  public
    /// initialize the instance
    constructor Create; virtual;
    /// finalize all used memory and resources
    destructor Destroy; override;
    /// enter the associated mutex
    procedure Lock;
    /// leave the associated mutex
    procedure UnLock;
  end;

  /// cryptographic pseudorandom number generator (CSPRNG) based on AES-256
  // - use as a shared instance via TAesPrng.Fill() overloaded class methods
  // - this class is able to generate some random output by encrypting successive
  // values of a counter with AES-256 and a secret key
  // - this internal secret key is generated from PBKDF2 derivation of OS-supplied
  // entropy using HMAC over SHA-512
  // - by design, such a PRNG is as good as the cypher used - for reference, see
  // https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator
  // - FillRandom() is thread-safe, and its AES process is not blocking: only
  // the CTR is pre-computed inside a lock
  // - it would use fast hardware AES-NI opcode, if available
  TAesPrng = class(TAesLocked)
  protected
    fBytesSinceSeed: PtrUInt;
    fSeedAfterBytes: PtrUInt;
    fAesKeySize: integer;
    fSeedPbkdf2Round: cardinal;
    fTotalBytes: QWord;
    fSeeding: boolean;
  public
    /// initialize the internal secret key, using Operating System entropy
    // - entropy is gathered from the OS, using GetEntropy() method
    // - you can specify how many PBKDF2_HMAC_SHA512 rounds are applied to the
    // OS-gathered entropy - the higher, the better, but also the slower
    // - internal private key would be re-seeded after ReseedAfterBytes
    // bytes (1MB by default) are generated, using GetEntropy()
    // - by default, AES-256 will be used, unless AesKeySize is set to 128,
    // which may be slightly faster (especially if AES-NI is not available)
    constructor Create(Pbkdf2Round: integer = 16;
      ReseedAfterBytes: integer = 1024 * 1024;
      AesKeySize: integer = 256); reintroduce; virtual;
    /// fill a TAesBlock with some pseudorandom data
    // - could be used e.g. to compute an AES Initialization Vector (IV)
    // - this method is thread-safe
    procedure FillRandom(out Block: TAesBlock); overload; virtual;
    /// fill a 256-bit buffer with some pseudorandom data
    // - this method is thread-safe
    procedure FillRandom(out Buffer: THash256); overload;
    /// fill a binary buffer with some pseudorandom data
    // - this method is thread-safe, and its AES process is non blocking
    procedure FillRandom(Buffer: pointer; Len: PtrInt); overload; virtual;
    /// returns a binary buffer filled with some pseudorandom data
    // - this method is thread-safe, and its AES process is non blocking
    function FillRandom(Len: integer): RawByteString; overload;
    /// returns a binary buffer filled with some pseudorandom data
    // - this method is thread-safe, and its AES process is non blocking
    function FillRandomBytes(Len: integer): TBytes;
    /// returns an hexa-encoded binary buffer filled with some pseudorandom data
    // - this method is thread-safe, and its AES process is non blocking
    function FillRandomHex(Len: integer): RawUtf8;
    /// returns a 32-bit unsigned random number
    // - is twice slower than Lecuyer's Random32 of mormot.core.bas unit, but
    // is cryptographic secure
    function Random32: cardinal; overload;
    /// returns a 32-bit unsigned random number, with a maximum value
    // - is twice slower than Lecuyer's Random32 of mormot.core.bas unit, but
    // is cryptographic secure
    function Random32(max: cardinal): cardinal; overload;
    /// returns a 64-bit unsigned random number
    function Random64: QWord;
    /// returns a floating-point random number in range [0..1]
    function RandomExt: TSynExtended;
    /// returns a 64-bit floating-point random number in range [0..1]
    function RandomDouble: double;
    /// computes a random ASCII password
    // - will contain uppercase/lower letters, digits and $.:()?%!-+*/@#
    // excluding ;,= to allow direct use in CSV content
    function RandomPassword(Len: integer): RawUtf8;
    /// would force the internal generator to re-seed its private key
    // - avoid potential attacks on backward or forward security
    // - would be called by FillRandom() methods, according to SeedAfterBytes
    // - this method is thread-safe
    procedure Seed; virtual;
    /// retrieve some entropy bytes from the Operating System
    // - entropy comes from CryptGenRandom API on Windows, and /dev/urandom or
    // /dev/random on Linux/POSIX
    // - this system-supplied entropy is then XORed with the output of a SHA-3
    // cryptographic SHAKE-256 generator in XOF mode, from several sources
    // (timestamp, thread and system information, mormot.core.base XorEntropy)
    // - if SystemOnly=true, returned values come from system only, so may not
    // always be true randomness on closed systems like Windows
    // - to gather randomness, use TAesPrng.Main.FillRandom() or TAesPrng.Fill()
    // methods, NOT this class function (which will be much slower, BTW)
    class function GetEntropy(Len: integer;
      SystemOnly: boolean = false): RawByteString; virtual;
    /// returns a shared instance of a TAesPrng instance
    // - if you need to generate some random content, just call the
    // TAesPrng.Main.FillRandom() overloaded methods, or directly TAesPrng.Fill()
    class function Main: TAesPrng;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAesPrng.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAesPrng instance
    // if you need some custom entropy level
    class procedure Fill(Buffer: pointer; Len: integer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAesPrng.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAesPrng instance
    // if you need some custom entropy level
    class procedure Fill(out Block: TAesBlock); overload;
    /// just a wrapper around TAesPrng.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAesPrng instance
    // if you need some custom entropy level
    class procedure Fill(out Block: THash256); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAesPrng.Main.FillRandom() function
    // - this method is thread-safe, but you may use your own TAesPrng instance
    // if you need some custom entropy level
    class function Fill(Len: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a wrapper around TAesPrng.Main.FillRandomBytes() function
    // - this method is thread-safe, but you may use your own TAesPrng instance
    // if you need some custom entropy level
    class function Bytes(Len: integer): TBytes;
      {$ifdef HASINLINE}inline;{$endif}
    /// create an anti-forensic representation of a key for safe storage
    // - a binary buffer will be split into StripesCount items, ready to be
    // saved on disk; returned length is BufferBytes*(StripesCount+1) bytes
    // - AFSplit supports secure data destruction crucial for secure on-disk
    // key management. The key idea is to bloat information and therefore
    // improve the chance of destroying a single bit of it. The information
    // is bloated in such a way, that a single missing bit causes the original
    // information become unrecoverable.
    // - this implementation uses SHA-256 as diffusion element, and the current
    // TAesPrng instance to gather randomness
    // - for reference, see TKS1 as used for LUKS and defined in
    // @https://gitlab.com/cryptsetup/cryptsetup/wikis/TKS1-draft.pdf
    function AFSplit(const Buffer; BufferBytes,
      StripesCount: integer): RawByteString; overload;
    /// create an anti-forensic representation of a key for safe storage
    // - a binary buffer will be split into StripesCount items, ready to be
    // saved on disk; returned length is BufferBytes*(StripesCount+1) bytes
    // - just a wrapper around the other overloaded AFSplit() funtion
    function AFSplit(const Buffer: RawByteString;
      StripesCount: integer): RawByteString; overload;
    /// retrieve a key from its anti-forensic representation
    // - is the reverse function of AFSplit() method
    // - returns TRUE if the input buffer matches BufferBytes value
    class function AFUnsplit(const Split: RawByteString;
      out Buffer; BufferBytes: integer): boolean; overload;
    /// retrieve a key from its anti-forensic representation
    // - is the reverse function of AFSplit() method
    // - returns the un-splitted binary content
    // - returns '' if StripesCount is incorrect
    class function AFUnsplit(const Split: RawByteString;
      StripesCount: integer): RawByteString; overload;
    /// after how many generated bytes Seed method would be called
    // - default is 1 MB
    property SeedAfterBytes: PtrUInt
      read fSeedAfterBytes;
    /// how many PBKDF2_HMAC_SHA512 count is applied by Seed to the entropy
    // - default is 16 rounds, which is more than enough for entropy gathering,
    // since GetEntropy output comes from a SHAKE-256 generator in XOF mode
    property SeedPbkdf2Round: cardinal
      read fSeedPbkdf2Round;
    /// how many bits (128 or 256 - which is the default) are used for the AES
    property AesKeySize: integer
      read fAesKeySize;
    /// how many bytes this generator did compute
    property TotalBytes: QWord
      read fTotalBytes;
  end;

  /// TAesPrng-compatible class using Operating System pseudorandom source
  // - may be used instead of TAesPrng if a "standard" generator is required -
  // you could override MainAesPrng global variable
  // - will call /dev/urandom under POSIX, and CryptGenRandom API on Windows
  // - warning: may block on some BSD flavors, depending on /dev/urandom
  // - from the cryptographic point of view, our TAesPrng class doesn't suffer
  // from the "black-box" approach of Windows, give consistent randomness
  // over all supported cross-platform, and is indubitably faster
  TAesPrngSystem = class(TAesPrng)
  public
    /// initialize the Operating System PRNG
    constructor Create; reintroduce; virtual;
    /// fill a TAesBlock with some pseudorandom data
    // - this method is thread-safe
    procedure FillRandom(out Block: TAesBlock); override;
    /// fill a binary buffer with some pseudorandom data
    // - this method is thread-safe
    // - is just a wrapper around FillSystemRandom()
    procedure FillRandom(Buffer: pointer; Len: PtrInt); override;
    /// called to force the internal generator to re-seed its private key
    // - won't do anything for the Operating System pseudorandom source
    procedure Seed; override;
  end;

var
  /// the shared TAesPrng instance returned by TAesPrng.Main class function
  // - you may override this to a customized instance, e.g. if you expect
  // a specific random generator to be used, like TAesPrngSystem
  // - all TAesPrng.Fill() class functions will use this instance
  MainAesPrng: TAesPrng;


{$ifdef HASINLINE}
/// defined globally to initialize MainAesPrng from inlined TAesPrng.Main
function SetMainAesPrng: TAesPrng;
{$endif HASINLINE}

/// low-level function returning some random binary from then available
// Operating System pseudorandom source
// - will call /dev/urandom or /dev/random under POSIX, and CryptGenRandom API
// on Windows, and fallback to mormot.core.base.FillRandom if the system API
// failed - also for padding if more than Len>32 from /dev/urandom
// - you should not have to call this procedure, but faster and safer TAesPrng;
// also consider the TAesPrngSystem class
procedure FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean);

/// low-level anti-forensic diffusion of a memory buffer using SHA-256
// - as used by TAesPrng.AFSplit and TAesPrng.AFUnSplit
procedure AFDiffusion(buf, rnd: pointer; size: cardinal);


var
  /// salt for CryptDataForCurrentUser function
  // - is filled with some random bytes by default, but you may override
  // it for a set of custom processes calling CryptDataForCurrentUser
  CryptProtectDataEntropy: THash256 = (
    $19, $8E, $BA, $52, $FA, $D6, $56, $99, $7B, $73, $1B, $D0, $8B, $3A, $95, $AB,
    $94, $63, $C2, $C0, $78, $05, $9C, $8B, $85, $B7, $A1, $E3, $ED, $93, $27, $18);

/// protect some data via AES-256-CFB and a secret known by the current user only
// - the application can specify a secret salt text, which should reflect the
// current execution context, to ensure nobody could decrypt the data without
// knowing this application-specific AppSecret value
// - here data is cyphered using a random secret key, stored in a file located in
// ! GetSystemPath(spUserData)+sep+PBKDF2_HMAC_SHA256(CryptProtectDataEntropy,User)
// with sep='_' under Windows, and sep='.syn-' under Linux/Posix
// - under Windows, it will encode the secret file via CryptProtectData DPAPI,
// so has the same security level than plain CryptDataForCurrentUserDPAPI(),
// but will be much faster, since it won't call the API each time
// - under Linux/POSIX, access to the $HOME user's .xxxxxxxxxxx secret file with
// chmod 400 is considered to be a safe enough approach
// - this function is up to 100 times faster than CryptDataForCurrentUserDPAPI,
// generates smaller results, and is consistent on all Operating Systems
// - you can use this function over a specified variable, to cypher it in place,
// with try ... finally block to protect memory access of the plain data:
// !  constructor TMyClass.Create;
// !  ...
// !    fSecret := CryptDataForCurrentUser('Some Secret Value','appsalt',true);
// !  ...
// !  procedure TMyClass.DoSomething;
// !  var plain: RawByteString;
// !  begin
// !    plain := CryptDataForCurrentUser(fSecret,'appsalt',false);
// !    try
// !      // here plain = 'Some Secret Value'
// !    finally
// !      FillZero(plain); // safely erase uncyphered content from heap
// !    end;
// !  end;
function CryptDataForCurrentUser(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;


{ ****************** SHA-2 SHA-3 Secure Hashing }

const
  /// hide all SHA-1/SHA-2 complex code by storing the context as buffer
  SHAContextSize = 108;

  /// hide all SHA-3 complex code by storing the Keccak Sponge as buffer
  SHA3ContextSize = 412;

type
  /// 256-bit (32 bytes) memory block for SHA-256 hash digest storage
  TSha256Digest = THash256;
  PSha256Digest = ^TSha256Digest;

  /// 384 bits (64 bytes) memory block for SHA-384 hash digest storage
  TSha384Digest = THash384;
  PSha384Digest = ^TSha384Digest;

  /// 512 bits (64 bytes) memory block for SHA-512 hash digest storage
  TSha512Digest = THash512;
  PSha512Digest = ^TSha512Digest;

  /// implements SHA-256 hashing
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA256
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  TSha256 = object
  private
    Context: packed array[1..SHAContextSize] of byte;
  public
    /// initialize SHA-256 context for hashing
    procedure Init;
    /// update the SHA-256 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-256 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-256 hash Digest of all data
    // affected to Update() method
    procedure Final(out Digest: TSha256Digest; NoInit: boolean = false); overload;
    /// finalize and compute the resulting SHA-256 hash Digest of all data
    // affected to Update() method
    function Final(NoInit: boolean = false): TSha256Digest; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSha256Digest);
  end;

  /// points to SHA-256 hashing instance
  PSha256 = ^TSha256;

/// direct SHA-256 hash calculation of some binary data
// - result is returned in TSha256Digest binary format
// - since the result would be stored temporarly in the stack, it may be
// safer to use an explicit TSha256Digest variable, which would be filled
// with zeros by a ... finally FillZero(
function Sha256Digest(Data: pointer; Len: integer): TSha256Digest; overload;

/// direct SHA-256 hash calculation of some binary data
// - result is returned in TSha256Digest binary format
// - since the result would be stored temporarly in the stack, it may be
// safer to use an explicit TSha256Digest variable, which would be filled
// with zeros by a ... finally FillZero(
function Sha256Digest(const Data: RawByteString): TSha256Digest; overload;


type
  TSha512Hash = record
    a, b, c, d, e, f, g, h: QWord;
  end;

  /// implements SHA-384 hashing
  // - it is in fact a TSha512 truncated hash, with other initial hash values
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA384
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  TSha384 = object
  private
    Hash: TSha512Hash;
    MLen: QWord;
    Data: array[0..127] of byte;
    Index: integer;
  public
    /// initialize SHA-384 context for hashing
    procedure Init;
    /// update the SHA-384 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-384 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-384 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    procedure Final(out Digest: TSha384Digest; NoInit: boolean = false); overload;
    /// finalize and compute the resulting SHA-384 hash Digest of all data
    // affected to Update() method
    function Final(NoInit: boolean = false): TSha384Digest; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSha384Digest);
  end;

  /// points to SHA-384 hashing instance
  PSha384 = ^TSha384;

  /// implements SHA-512 hashing
  // - by design, this algorithm is expected to be much faster on 64-bit CPU,
  // since all internal process involves QWord - but we included a SSE3 asm
  // optimized version on 32-bit CPU under Windows and Linux, which is almost
  // as fast as on plain x64, and even faster than SHA-256 and SHA-3
  // - under x86/Delphi, plain pascal is 40MB/s, SSE3 asm 180MB/s
  // - on x64, pascal Delphi is 150MB/s, and FPC is 190MB/s (thanks to native
  // RorQWord intrinsic compiler function) - we also included a SSE4 asm version
  // which outperforms other cryptographic hashes to more than 380MB/s
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA512
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  TSha512 = object
  private
    Hash: TSha512Hash;
    MLen: QWord;
    Data: array[0..127] of byte;
    Index: integer;
  public
    /// initialize SHA-512 context for hashing
    procedure Init;
    /// update the SHA-512 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-512 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-512 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    procedure Final(out Digest: TSha512Digest; NoInit: boolean = false); overload;
    /// finalize and compute the resulting SHA-512 hash Digest of all data
    // affected to Update() method
    function Final(NoInit: boolean = false): TSha512Digest; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSha512Digest);
  end;

  /// points to SHA-512 hashing instance
  PSha512 = ^TSha512;

type
  /// SHA-3 instances, as defined by NIST Standard for Keccak sponge construction
  TSha3Algo = (
    SHA3_224,
    SHA3_256,
    SHA3_384,
    SHA3_512,
    SHAKE_128,
    SHAKE_256);

  /// implements SHA-3 (Keccak) hashing
  // - Keccak was the winner of the NIST hashing competition for a new hashing
  // algorithm to provide an alternative to SHA-256. It became SHA-3 and was
  // named by NIST a FIPS 180-4, then FIPS 202 hashing standard in 2015
  // - by design, SHA-3 doesn't need to be encapsulated into a HMAC algorithm,
  // since it already includes proper padding, so keys could be concatenated
  // - this implementation is based on Wolfgang Ehrhardt's and Eric Grange's,
  // with our own manually optimized x64 assembly
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. after InitCypher
  // - see TSynHasher if you expect to support more than one algorithm at runtime

  TSha3 = object
  private
    Context: packed array[1..SHA3ContextSize] of byte;
  public
    /// initialize SHA-3 context for hashing
    // - in practice, you may use SHA3_256 or SHA3_512 to return THash256
    // or THash512 digests
    procedure Init(Algo: TSha3Algo);
    /// update the SHA-3 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-3 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-3 hash 256-bit Digest
    procedure Final(out Digest: THash256; NoInit: boolean = false); overload;
    /// finalize and compute the resulting SHA-3 hash 512-bit Digest
    procedure Final(out Digest: THash512; NoInit: boolean = false); overload;
    /// finalize and compute the resulting SHA-3 hash 256-bit Digest
    function Final256(NoInit: boolean = false): THash256;
    /// finalize and compute the resulting SHA-3 hash 512-bit Digest
    function Final512(NoInit: boolean = false): THash512;
    /// finalize and compute the resulting SHA-3 hash Digest
    // - Digest destination buffer must contain enough bytes
    // - default DigestBits=0 will write the default number of bits to Digest
    // output memory buffer, according to the current TSha3Algo
    // - you can call this method several times, to use this SHA-3 hasher as
    // "Extendable-Output Function" (XOF), e.g. for stream encryption (ensure
    // NoInit is set to true, to enable recall)
    procedure Final(Digest: pointer; DigestBits: integer = 0;
      NoInit: boolean = false); overload;
    /// compute a SHA-3 hash 256-bit Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using SHA3_256 into a THash256
    procedure Full(Buffer: pointer; Len: integer; out Digest: THash256); overload;
    /// compute a SHA-3 hash 512-bit Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using SHA3_512 into a THash512
    procedure Full(Buffer: pointer; Len: integer; out Digest: THash512); overload;
    /// compute a SHA-3 hash Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using the supplied algorithm
    // - default DigestBits=0 will write the default number of bits to Digest
    // output memory buffer, according to the specified TSha3Algo
    procedure Full(Algo: TSha3Algo; Buffer: pointer; Len: integer;
      Digest: pointer; DigestBits: integer = 0); overload;
    /// compute a SHA-3 hash hexadecimal Digest from a buffer, in one call
    // - call Init, then Update(), then Final() using the supplied algorithm
    // - default DigestBits=0 will write the default number of bits to Digest
    // output memory buffer, according to the specified TSha3Algo
    function FullStr(Algo: TSha3Algo; Buffer: pointer; Len: integer;
      DigestBits: integer = 0): RawUtf8;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - there is no MAC stored in the resulting binary
    // - Source and Dest will have the very same DataLen size in bytes,
    // and Dest will be Source XORed with the XOF output, so encryption and
    // decryption are just obtained by the same symmetric call
    // - in this implementation, Source and Dest should point to two diverse buffers
    // - for safety, the Key should be a secret value, pre-pended with a random
    // salt/IV or a resource-specific identifier (e.g. a record ID or a S/N),
    // to avoid reverse composition of the cypher from known content - note that
    // concatenating keys with SHA-3 is as safe as computing a HMAC for SHA-2
    procedure Cypher(Key, Source, Dest: pointer; KeyLen, DataLen: integer;
      Algo: TSha3Algo = SHAKE_256); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - this overloaded function works with RawByteString content
    // - resulting string will have the very same size than the Source
    // - XOF is implemented as a symmetrical algorithm: use this Cypher()
    // method for both encryption and decryption of any buffer
    function Cypher(const Key, Source: RawByteString;
      Algo: TSha3Algo = SHAKE_256): RawByteString; overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - prepare the instance to further Cypher() calls
    // - you may reuse the very same TSha3 instance by copying it to a local
    // variable before calling this method (this copy is thread-safe)
    // - works with RawByteString content
    procedure InitCypher(Key: pointer; KeyLen: integer;
      Algo: TSha3Algo = SHAKE_256); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - prepare the instance to further Cypher() calls
    // - you may reuse the very same TSha3 instance by copying it to a local
    // variable before calling this method (this copy is thread-safe)
    // - works with RawByteString content
    procedure InitCypher(const Key: RawByteString;
      Algo: TSha3Algo = SHAKE_256); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - this overloaded function expects the instance to have been prepared
    // by previous InitCypher call
    // - resulting Dest buffer will have the very same size than the Source
    // - XOF is implemented as a symmetrical algorithm: use this Cypher()
    // method for both encryption and decryption of any buffer
    // - you can call this method several times, to work with a stream buffer;
    // but for safety, you should eventually call Done
    procedure Cypher(Source, Dest: pointer; DataLen: integer); overload;
    /// uses SHA-3 in "Extendable-Output Function" (XOF) to cypher some content
    // - this overloaded function expects the instance to have been prepared
    // by previous InitCypher call
    // - resulting string will have the very same size than the Source
    // - XOF is implemented as a symmetrical algorithm: use this Cypher()
    // method for both encryption and decryption of any buffer
    // - you can call this method several times, to work with a stream buffer;
    // but for safety, you should eventually call Done
    function Cypher(const Source: RawByteString): RawByteString; overload;
    /// returns the algorithm specified at Init()
    function Algorithm: TSha3Algo;
    /// fill all used memory context with zeros, for safety
    // - is necessary only when NoInit is set to true (e.g. after InitCypher)
    procedure Done;
  end;

  /// points to SHA-3 hashing instance
  PSha3 = ^TSha3;

function ToText(algo: TSha3Algo): PShortString; overload;



{ ****************** Deprecated MD5 RC4 SHA-1 Algorithms }

type
  /// 128-bit memory block for MD5 hash digest storage
  TMd5Digest = THash128;
  PMd5Digest = ^TMd5Digest;

  /// 160 bits memory block for SHA-1 hash digest storage
  TSha1Digest = THash160;
  PSha1Digest = ^TSha1Digest;

  TMd5In = array[0..15] of cardinal;
  PMd5In = ^TMd5In;
  TMd5Buf = TBlock128;

  /// implements MD5 hashing
  // - this algorithm has known weaknesses, so should not be considered as
  // cryptographic secure, but is available for other purposes
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  // - even if MD5 is now seldom used, it is still faster than SHA alternatives,
  // when you need a 128-bit cryptographic hash, but can afford some collisions
  // - this implementation has optimized x86 and x64 assembly, for processing
  // around 500MB/s, and a pure-pascal fallback code on other platforms
  TMd5 = object
  private
    in_: TMd5In;
    bytes: array[0..1] of cardinal;
  public
    buf: TMd5Buf;
    /// initialize MD5 context for hashing
    procedure Init;
    /// update the MD5 context with some data
    procedure Update(const buffer; Len: cardinal); overload;
    /// update the MD5 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize the MD5 hash process
    // - the resulting hash digest would be stored in buf public variable
    procedure Finalize;
    /// finalize and compute the resulting MD5 hash Digest of all data
    // affected to Update() method
    procedure Final(out result: TMd5Digest); overload;
    /// finalize and compute the resulting MD5 hash Digest of all data
    // affected to Update() method
    function Final: TMd5Digest; overload;
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TMd5Digest);
  end;
  PMd5 = ^TMd5;

  /// implements RC4 encryption/decryption
  // - this algorithm has known weaknesses, so should not be considered as
  // cryptographic secure, but is available for other purposes
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  // - you can also restore and backup any previous state of the RC4 encryption
  // by copying the whole TRC4 variable into another (stack-allocated) variable
  TRC4 = object
  private
    {$ifdef CPUINTEL}
    state: array[byte] of PtrInt; // PtrInt=270MB/s  byte=240MB/s on x86
    {$else}
    state: array[byte] of byte; // on ARM, keep the CPU cache usage low
    {$endif}
    currI, currJ: PtrInt;
  public
    /// initialize the RC4 encryption/decryption
    // - KeyLen is in bytes, and should be within 1..255 range
    // - warning: aKey is an untyped constant, i.e. expects a raw set of memory
    // bytes: do NOT use assign it with a string or a TBytes instance: you would
    // use the pointer to the data as key
    procedure Init(const aKey; aKeyLen: integer);
    /// initialize RC4-drop[3072] encryption/decryption after SHA-3 hashing
    // - will use SHAKE-128 generator in XOF mode to generate a 256 bytes key,
    // then drop the first 3072 bytes from the RC4 stream
    // - this initializer is much safer than plain Init, so should be considered
    // for any use on RC4 for new projects - even if AES-NI is 2 times faster,
    // and safer SHAKE-128 operates in XOF mode at a similar speed range
    procedure InitSha3(const aKey; aKeyLen: integer);
    /// drop the next Count bytes from the RC4 cypher state
    // - may be used in Stream mode, or to initialize in RC4-drop[n] mode
    procedure Drop(Count: cardinal);
    /// perform the RC4 cypher encryption/decryption on a buffer
    // - each call to this method shall be preceeded with an Init() call
    // - RC4 is a symmetrical algorithm: use this Encrypt() method
    // for both encryption and decryption of any buffer
    procedure Encrypt(const BufIn; var BufOut; Count: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// perform the RC4 cypher encryption/decryption on a buffer
    // - each call to this method shall be preceeded with an Init() call
    // - RC4 is a symmetrical algorithm: use this EncryptBuffer() method
    // for both encryption and decryption of any buffer
    procedure EncryptBuffer(BufIn, BufOut: PByte; Count: cardinal);
  end;

  /// implements SHA-1 hashing
  // - this algorithm has known weaknesses, so should not be considered as
  // cryptographic secure, but is available for other purposes
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance, e.g. for THMAC_SHA1
  // - see TSynHasher if you expect to support more than one algorithm at runtime
  TSha1 = object
  private
    Context: packed array[1..SHAContextSize] of byte;
  public
    /// initialize SHA-1 context for hashing
    procedure Init;
    /// update the SHA-1 context with some data
    procedure Update(Buffer: pointer; Len: integer); overload;
    /// update the SHA-1 context with some data
    procedure Update(const Buffer: RawByteString); overload;
    /// finalize and compute the resulting SHA-1 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    procedure Final(out Digest: TSha1Digest; NoInit: boolean = false); overload;
    /// finalize and compute the resulting SHA-1 hash Digest of all data
    // affected to Update() method
    // - will also call Init to reset all internal temporary context, for safety
    function Final(NoInit: boolean = false): TSha1Digest; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSha1Digest);
  end;

  /// points to SHA-1 hashing instance
  PSha1 = ^TSha1;

/// direct MD5 hash calculation of some data
function Md5Buf(const Buffer; Len: cardinal): TMd5Digest;

/// compute the HTDigest for a user and a realm, according to a supplied password
// - apache-compatible: 'agent007:download area:8364d0044ef57b3defcfa141e8f77b65'
function HTDigest(const user, realm, pass: RawByteString): RawUtf8;


{ ****************** HMAC Authentication over SHA and CRC32C }

{ ----------- HMAC over SHA-1 }

type
  /// compute the HMAC message authentication code using SHA-1 as hash function
  // - you may use HMAC_SHA1() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  THMAC_SHA1 = object
  private
    sha: TSha1;
    step7data: THash512Rec;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA1 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer);
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSha1Digest; NoInit: boolean = false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUtf8; NoInit: boolean = false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA1 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSha1Digest);
  end;

  /// points to a HMAC message authentication context using SHA-1
  PHMAC_SHA1 = ^THMAC_SHA1;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HMAC_SHA1(const key, msg: RawByteString;
  out result: TSha1Digest); overload;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HMAC_SHA1(const key: TSha1Digest; const msg: RawByteString;
  out result: TSha1Digest); overload;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HMAC_SHA1(key, msg: pointer; keylen, msglen: integer;
  out result: TSha1Digest); overload;


{ ----------- HMAC over SHA-256 }

type
  /// compute the HMAC message authentication code using SHA-256 as hash function
  // - you may use HMAC_SHA256() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  THMAC_SHA256 = object
  private
    sha: TSha256;
    step7data: THash512Rec;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA256 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: THash128); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: THash256); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: RawByteString); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSha256Digest; NoInit: boolean = false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUtf8; NoInit: boolean = false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA256 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSha256Digest);
  end;

  /// points to a HMAC message authentication context using SHA-256
  PHMAC_SHA256 = ^THMAC_SHA256;

/// compute the HMAC message authentication code using SHA-256 as hash function
procedure HMAC_SHA256(const key, msg: RawByteString;
  out result: TSha256Digest); overload;

/// compute the HMAC message authentication code using SHA-256 as hash function
procedure HMAC_SHA256(const key: TSha256Digest; const msg: RawByteString;
  out result: TSha256Digest); overload;

/// compute the HMAC message authentication code using SHA-256 as hash function
procedure HMAC_SHA256(key, msg: pointer; keylen, msglen: integer;
  out result: TSha256Digest); overload;


{ ----------- HMAC over SHA-384 }

type
  /// compute the HMAC message authentication code using SHA-384 as hash function
  // - you may use HMAC_SHA384() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  THMAC_SHA384 = object
  private
    sha: TSha384;
    step7data: array[0..31] of cardinal;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA384 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer);
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSha384Digest; NoInit: boolean = false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUtf8; NoInit: boolean = false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA384 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSha384Digest);
  end;

  /// points to a HMAC message authentication context using SHA-384
  PHMAC_SHA384 = ^THMAC_SHA384;

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HMAC_SHA384(const key, msg: RawByteString;
  out result: TSha384Digest); overload;

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HMAC_SHA384(const key: TSha384Digest; const msg: RawByteString;
  out result: TSha384Digest); overload;

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HMAC_SHA384(key, msg: pointer; keylen, msglen: integer;
  out result: TSha384Digest); overload;


{ ----------- HMAC over SHA-512 }

type
  /// compute the HMAC message authentication code using SHA-512 as hash function
  // - you may use HMAC_SHA512() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute(), e.g. for fast PBKDF2
  THMAC_SHA512 = object
  private
    sha: TSha512;
    step7data: array[0..31] of cardinal;
  public
    /// prepare the HMAC authentication with the supplied key
    // - content of this record is stateless, so you can prepare a HMAC for a
    // key using Init, then copy this THMAC_SHA512 instance to a local variable,
    // and use this local thread-safe copy for actual HMAC computing
    procedure Init(key: pointer; keylen: integer);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer);
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: TSha512Digest; NoInit: boolean = false); overload;
    /// computes the HMAC of all supplied message according to the key
    procedure Done(out result: RawUtf8; NoInit: boolean = false); overload;
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe on any shared THMAC_SHA512 instance
    procedure Compute(msg: pointer; msglen: integer; out result: TSha512Digest);
  end;

  /// points to a HMAC message authentication context using SHA-512
  PHMAC_SHA512 = ^THMAC_SHA512;

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HMAC_SHA512(const key, msg: RawByteString;
  out result: TSha512Digest); overload;

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HMAC_SHA512(const key: TSha512Digest; const msg: RawByteString;
  out result: TSha512Digest); overload;

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HMAC_SHA512(key, msg: pointer; keylen, msglen: integer;
  out result: TSha512Digest); overload;


{ ----------- HMAC over CRC-256C }

/// compute the HMAC message authentication code using crc256c as hash function
// - HMAC over a non cryptographic hash function like crc256c is known to be
// safe as MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - performs two crc32c hashes, so SSE 4.2 gives more than 2.2 GB/s on a Core i7
procedure HMAC_CRC256C(key, msg: pointer; keylen, msglen: integer;
  out result: THash256); overload;

/// compute the HMAC message authentication code using crc256c as hash function
// - HMAC over a non cryptographic hash function like crc256c is known to be
// safe as MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - performs two crc32c hashes, so SSE 4.2 gives more than 2.2 GB/s on a Core i7
procedure HMAC_CRC256C(const key: THash256; const msg: RawByteString; out result: THash256); overload;

/// compute the HMAC message authentication code using crc256c as hash function
// - HMAC over a non cryptographic hash function like crc256c is known to be
// safe as MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - performs two crc32c hashes, so SSE 4.2 gives more than 2.2 GB/s on a Core i7
procedure HMAC_CRC256C(const key, msg: RawByteString; out result: THash256); overload;


{ ----------- HMAC over CRC-32C }

type
  /// compute the HMAC message authentication code using crc32c as hash function
  // - HMAC over a non cryptographic hash function like crc32c is known to be a
  // safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
  // - SSE 4.2 will let MAC be computed at 13 GB/s on a Core i7 / x86_64
  // - you may use HMAC_CRC32C() overloaded functions for one-step process
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance via Compute()
  THMAC_CRC32C = object
  private
    seed: cardinal;
    step7data: THash512Rec;
  public
    /// prepare the HMAC authentication with the supplied key
    // - consider using Compute to re-use a prepared HMAC instance
    procedure Init(key: pointer; keylen: integer); overload;
    /// prepare the HMAC authentication with the supplied key
    // - consider using Compute to re-use a prepared HMAC instance
    procedure Init(const key: RawByteString); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(msg: pointer; msglen: integer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Done to retrieve the HMAC
    procedure Update(const msg: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// computes the HMAC of all supplied message according to the key
    function Done(NoInit: boolean = false): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// computes the HMAC of the supplied message according to the key
    // - expects a previous call on Init() to setup the shared key
    // - similar to a single Update(msg,msglen) followed by Done, but re-usable
    // - this method is thread-safe
    function Compute(msg: pointer; msglen: integer): cardinal;
  end;

  /// points to HMAC message authentication code using crc32c as hash function
  PHMAC_CRC32C = ^THMAC_CRC32C;

/// compute the HMAC message authentication code using crc32c as hash function
// - HMAC over a non cryptographic hash function like crc32c is known to be a
// safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - SSE 4.2 will let MAC be computed at 13 GB/s on a Core i7 / x86_64
function HMAC_CRC32C(key, msg: pointer; keylen, msglen: integer): cardinal; overload;

/// compute the HMAC message authentication code using crc32c as hash function
// - HMAC over a non cryptographic hash function like crc32c is known to be a
// safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - SSE 4.2 will let MAC be computed at 13 GB/s on a Core i7 / x86_64
function HMAC_CRC32C(const key: THash256; const msg: RawByteString): cardinal; overload;

/// compute the HMAC message authentication code using crc32c as hash function
// - HMAC over a non cryptographic hash function like crc32c is known to be a
// safe enough MAC, if the supplied key comes e.g. from cryptographic HMAC_SHA256
// - SSE 4.2 will let MAC be computed at 13 GB/s on a Core i7 / x86_64
function HMAC_CRC32C(const key, msg: RawByteString): cardinal; overload;


{ ****************** PBKDF2 Key Derivation over SHA and CRC32C }

/// compute the PBKDF2 derivation of a password using HMAC over SHA-1
// - this function expect the resulting key length to match SHA-1 digest size
procedure PBKDF2_HMAC_SHA1(const password, salt: RawByteString;
  count: integer; out result: TSha1Digest);

/// compute the PBKDF2 derivation of a password using HMAC over SHA-256
// - this function expect the resulting key length to match SHA-256 digest size
procedure PBKDF2_HMAC_SHA256(const password, salt: RawByteString;
  count: integer; out result: TSha256Digest;
  const saltdefault: RawByteString = ''); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-256, into
// several 256-bit items, so can be used to return any size of output key
// - this function expect the result array to have the expected output length
// - allows resulting key length to be more than one SHA-256 digest size, e.g.
// to be used for both Encryption and MAC
procedure PBKDF2_HMAC_SHA256(const password, salt: RawByteString;
  count: integer; var result: THash256DynArray;
  const saltdefault: RawByteString = ''); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-384
// - this function expect the resulting key length to match SHA-384 digest size
procedure PBKDF2_HMAC_SHA384(const password, salt: RawByteString;
  count: integer; out result: TSha384Digest);

/// compute the PBKDF2 derivation of a password using HMAC over SHA-512
// - this function expect the resulting key length to match SHA-512 digest size
procedure PBKDF2_HMAC_SHA512(const password, salt: RawByteString;
  count: integer; out result: TSha512Digest);

/// safe key derivation using iterated SHA-3 hashing
// - you can use SHA3_224, SHA3_256, SHA3_384, SHA3_512 algorithm to fill
// the result buffer with the default sized derivated key of 224,256,384 or 512
// bits (leaving resultbytes = 0)
// - or you may select SHAKE_128 or SHAKE_256, and specify any custom key size
// in resultbytes (used e.g. by PBKDF2_SHA3_Crypt)
procedure PBKDF2_SHA3(algo: TSha3Algo; const password, salt: RawByteString;
  count: integer; result: PByte; resultbytes: integer = 0);

/// encryption/decryption of any data using iterated SHA-3 hashing key derivation
// - specified algo is expected to be SHAKE_128 or SHAKE_256
// - expected the supplied data buffer to be small - for bigger content,
// consider using TAes Cypher after 256-bit PBKDF2_SHA3 key derivation
procedure PBKDF2_SHA3_Crypt(algo: TSha3Algo; const password, salt: RawByteString;
  count: integer; var data: RawByteString);


{ ****************** Digest/Hash to Hexadecimal Text Conversion }

const
  SHA1DIGESTSTRLEN = SizeOf(TSha1Digest) * 2;
  SHA256DIGESTSTRLEN = SizeOf(TSha256Digest) * 2;
  MD5DIGESTSTRLEN = SizeOf(TMd5Digest) * 2;

type
  /// 32-characters ASCII string, e.g. as returned by AesBlockToShortString()
  Short32 = string[32];

/// compute the hexadecial representation of an AES 16-byte block
// - returns a stack-allocated short string
function AesBlockToShortString(const block: TAesBlock): short32; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecial representation of an AES 16-byte block
// - fill a stack-allocated short string
procedure AesBlockToShortString(const block: TAesBlock; out result: short32); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the hexadecial representation of an AES 16-byte block
function AesBlockToString(const block: TAesBlock): RawUtf8;


/// direct MD5 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function Md5(const s: RawByteString): RawUtf8;

/// compute the hexadecimal representation of a MD5 digest
function Md5DigestToString(const D: TMd5Digest): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the MD5 digest from its hexadecimal representation
// - returns true on success (i.e. Source has the expected size and characters)
// - just a wrapper around mormot.core.text.HexToBin()
function Md5StringToDigest(const Source: RawUtf8; out Dest: TMd5Digest): boolean;


/// direct SHA-1 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function Sha1(const s: RawByteString): RawUtf8;

/// compute the hexadecimal representation of a SHA-1 digest
function Sha1DigestToString(const D: TSha1Digest): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the SHA-1 digest from its hexadecimal representation
// - returns true on success (i.e. Source has the expected size and characters)
// - just a wrapper around mormot.core.text.HexToBin()
function Sha1StringToDigest(const Source: RawUtf8; out Dest: TSha1Digest): boolean;
  {$ifdef HASINLINE}inline;{$endif}


/// direct SHA-256 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function Sha256(const s: RawByteString): RawUtf8; overload;

/// direct SHA-256 hash calculation of some binary data
// - result is returned in hexadecimal format
function Sha256(Data: pointer; Len: integer): RawUtf8; overload;

/// compute the hexadecimal representation of a SHA-256 digest
function Sha256DigestToString(const D: TSha256Digest): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the SHA-256 digest from its hexadecimal representation
// - returns true on success (i.e. Source has the expected size and characters)
// - just a wrapper around mormot.core.text.HexToBin()
function Sha256StringToDigest(const Source: RawUtf8; out Dest: TSha256Digest): boolean;
  {$ifdef HASINLINE}inline;{$endif}


/// direct SHA-384 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function Sha384(const s: RawByteString): RawUtf8;

/// compute the hexadecimal representation of a SHA-384 digest
function Sha384DigestToString(const D: TSha384Digest): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}


/// direct SHA-512 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function Sha512(const s: RawByteString): RawUtf8;

/// compute the hexadecimal representation of a SHA-512 digest
function Sha512DigestToString(const D: TSha512Digest): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// direct SHA-3 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
// - default DigestBits=0 will write the default number of bits to Digest
// output memory buffer, according to the specified TSha3Algo
function Sha3(Algo: TSha3Algo; const s: RawByteString;
  DigestBits: integer = 0): RawUtf8; overload;

/// direct SHA-3 hash calculation of some binary buffer
// - result is returned in hexadecimal format
// - default DigestBits=0 will write the default number of bits to Digest
// output memory buffer, according to the specified TSha3Algo
function Sha3(Algo: TSha3Algo; Buffer: pointer; Len: integer;
  DigestBits: integer = 0): RawUtf8; overload;



{ ****************** Deprecated Weak AES/SHA Process }

{$ifndef PUREMORMOT2}

type
  {$A-}
  /// internal header for storing our AES data with salt and CRC
  // - memory size matches an TAesBlock on purpose, for direct encryption
  // - TAesFull uses unsafe direct AES-ECB chain mode, so is considered deprecated
  TAesFullHeader = object
  public
    /// Len before compression (if any)
    OriginalLen,
    /// Len before AES encoding
    SourceLen,
    /// Random Salt for better encryption
    SomeSalt,
    /// CRC from header
    HeaderCheck: cardinal;
    /// computes the Key checksum, using Adler32 algorithm
    function Calc(const Key; KeySize: cardinal): cardinal;
  end;
  {$A+}

  PAesFull = ^TAesFull;
  /// AES and XOR encryption object for easy direct memory or stream access
  // - calls internaly TAes objet methods, and handle memory and streams for best speed
  // - a TAesFullHeader is encrypted at the begining, allowing fast Key validation,
  // but the resulting stream is not compatible with raw TAes object
  // - will use unsafe direct AES-ECB chain mode, so is considered deprecated
  TAesFull = object
  public
    /// header, stored at the beginning of struct -> 16-byte aligned
    Head: TAesFullHeader;
    /// this memory stream is used in case of EncodeDecode(outStream=bOut=nil)
    // method call
    outStreamCreated: TMemoryStream;
    /// main method of AES or XOR cypher/uncypher
    // - return out size, -1 if error on decoding (Key not correct)
    // - valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    // - if outStream is TMemoryStream -> auto-reserve space (no Realloc:)
    // - for normal usage, you just have to Assign one In and one Out
    // - if outStream AND bOut are both nil, an outStream is created via
    // TMemoryStream.Create
    // - if Encrypt -> OriginalLen can be used to store unCompressed Len
    function EncodeDecode(const Key; KeySize, inLen: cardinal; Encrypt: boolean;
      inStream, outStream: TStream; bIn, bOut: pointer; OriginalLen: cardinal = 0): integer;
  end;

  /// AES encryption stream
  // - encrypt the Data on the fly, in a compatible way with AES() - last bytes
  // are coded with XOR (not compatible with TAesFull format)
  // - not optimized for small blocks -> ok if used AFTER TBZCompressor/TZipCompressor
  // - warning: Write() will crypt Buffer memory in place -> use AFTER T*Compressor
  // - will use unsafe direct AES-ECB chain mode, so is considered deprecated
  TAesWriteStream = class(TStream)
  public
    Adler, // CRC from uncrypted compressed data - for Key check
    DestSize: cardinal;
  private
    Dest: TStream;
    Buf: TAesBlock; // very small buffer for remainging 0..15 bytes
    BufCount: integer; // number of pending bytes (0..15) in Buf
    AES: TAes;
    NoCrypt: boolean; // if KeySize=0
  public
    /// initialize the AES encryption stream for an output stream (e.g.
    // a TMemoryStream or a TFileStream)
    constructor Create(outStream: TStream; const Key; KeySize: cardinal);
    /// finalize the AES encryption stream
    // - internaly call the Finish method
    destructor Destroy; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Read(var Buffer; Count: Longint): Longint; override;
    /// append some data to the outStream, after encryption
    function Write(const Buffer; Count: Longint): Longint; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// write pending data
    // - should always be called before closeing the outStream (some data may
    // still be in the internal buffers)
    procedure Finish;
  end;


/// direct Encrypt/Decrypt of data using the TAes class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: integer;
  Encrypt: boolean); overload; deprecated;

/// direct Encrypt/Decrypt of data using the TAes class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean); overload; deprecated;

/// direct Encrypt/Decrypt of data using the TAes class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AES(const Key; KeySize: cardinal; const s: RawByteString;
  Encrypt: boolean): RawByteString; overload; deprecated;

/// direct Encrypt/Decrypt of data using the TAes class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal;
  Stream: TStream; Encrypt: boolean): boolean; overload; deprecated;

/// AES and XOR encryption using the TAesFull format
// - outStream will be larger/smaller than Len (full AES encrypted)
// - if KeySize is not in [128,192,256], will use a naive simple Xor Cypher
// - returns true if OK
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AESFull(const Key; KeySize: cardinal;
  bIn: pointer; Len: integer; outStream: TStream; Encrypt: boolean;
  OriginalLen: cardinal = 0): boolean; overload; deprecated;

/// AES and XOR encryption using the TAesFull format
// - bOut must be at least bIn+32/Encrypt bIn-16/Decrypt
// - if KeySize is not in [128,192,256], will use a naive simple Xor Cypher
// - returns outLength, -1 if error
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean; OriginalLen: cardinal = 0): integer; overload; deprecated;

/// AES and XOR decryption check using the TAesFull format
// - return true if the beginning of buff contains some data AESFull-encrypted
// with this Key
// - if not KeySize in [128,192,256], will always return true
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean; deprecated;

/// AES encryption using the TAes format with a supplied SHA-256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString;
  Encrypt: boolean); overload; deprecated;

/// AES encryption using the TAes format with a supplied SHA-256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString;
  Encrypt: boolean); overload; deprecated;

/// AES encryption using the TAes format with a supplied SHA-256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
function AESSHA256(const s, Password: RawByteString;
  Encrypt: boolean): RawByteString; overload; deprecated;

/// AES encryption using the TAesFull format with a supplied SHA-256 password
// - outStream will be larger/smaller than Len: this is a full AES version with
// a triming TAesFullHeader at the beginning
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
procedure AESSHA256Full(bIn: pointer; Len: integer; outStream: TStream;
  const Password: RawByteString; Encrypt: boolean); overload; deprecated;

{$endif PUREMORMOT2}


/// deprecated SHA-256 hash calculation of some data (string-encoded), with
// padding if incoming key text is shorter than 255 bytes
// - result is returned in hexadecimal format
// - WARNING: this algorithm is proprietary, and less secure (and standard)
// than the PBKDF2 algorithm, so it should be considered as deprecated; it
// is supplied only for backward compatibility of existing code:
// use PBKDF2_HMAC_SHA256 or similar functions for safer password derivation
procedure Sha256Weak(const s: RawByteString; out Digest: TSha256Digest);


implementation

{ ****************** Include Tuned INTEL/AMD Assembly }

{ we need to define now some shared types and constants used also from asm }

const
  AesMaxRounds = 14;

type
  TKeyArray = packed array[0..AesMaxRounds] of TAesBlock;

  /// low-level content of TAes.Context (AES_CONTEXT_SIZE bytes)
  // - is defined privately in the implementation section
  // - do NOT change this structure: it is fixed in the asm code
  TAesContext = packed record
    RK: TKeyArray;   // Key (encr. or decr.)
    iv: THash128Rec; // IV or CTR
    buf: TAesBlock;  // Work buffer
    DoBlock: procedure(const ctxt, Source, Dest); // main AES function
    {$ifdef USEAESNI32}
    AesNi32: pointer; // xmm7 AES-NI encoding
    {$endif USEAESNI32}
    Initialized: boolean;
    Rounds: byte;    // Number of rounds
    KeyBits: word;   // Number of bits in key (128/192/256)
  end;

  TSHAHash = packed record
    // will use A..E with TSha1, A..H with TSha256
    A, B, C, D, E, F, G, H: cardinal;
  end;

  TSHAContext = packed record
    // Working hash (TSha256.Init expect this field to be the first)
    Hash: TSHAHash;
    // 64bit msg length
    MLen: QWord;
    // Block buffer
    Buffer: array[0..63] of byte;
    // Index in buffer
    Index: integer;
  end;

// helper types for better code generation
type
  TWA4 = TBlock128;     // AES block as array of cardinal
  TAWk = packed array[0..4 * (AesMaxRounds + 1) - 1] of cardinal; // Key as array of cardinal
  PWA4 = ^TWA4;
  PAWk = ^TAWk;

const
  // used by AES
  RCon: array[0..9] of cardinal = (
    $01, $02, $04, $08, $10, $20, $40, $80, $1b, $36);

  // used by SHA-256
  K256: array[0..63] of cardinal = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4,
    $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe,
    $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f,
    $4a7484aa, $5cb0a9dc, $76f988da, $983e5152, $a831c66d, $b00327c8, $bf597fc7,
    $c6e00bf3, $d5a79147, $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc,
    $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
    $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070, $19a4c116,
    $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7,
    $c67178f2);

var
  // AES computed tables - don't change the order below! (used for weak Xor)
  Td0, Td1, Td2, Td3, Te0, Te1, Te2, Te3: array[byte] of cardinal;
  SBox, InvSBox: array[byte] of byte;

{$ifdef CPUX64}
  {$include mormot.core.crypto.asmx64.inc}
{$endif}

{$ifdef CPUX86}
  {$include mormot.core.crypto.asmx86.inc}
{$endif}


{ ****************** Low-Level Memory Buffers Helper Functions }

procedure XorBlock16(A, B: PPtrIntArray);
begin
  A[0] := A[0] xor B[0];
  A[1] := A[1] xor B[1];
  {$ifdef CPU32}
  A[2] := A[2] xor B[2];
  A[3] := A[3] xor B[3];
  {$endif CPU32}
end;

procedure XorBlock16(A, B, C: PPtrIntArray);
begin
  B[0] := A[0] xor C[0];
  B[1] := A[1] xor C[1];
  {$ifdef CPU32}
  B[2] := A[2] xor C[2];
  B[3] := A[3] xor C[3];
  {$endif CPU32}
end;

procedure XorBlock(P: PIntegerArray; Count, Cod: integer);
// very fast Xor() according to Cod - not Compression or Stream compatible
var
  i: integer;
begin
  for i := 1 to Count shr 4 do
  begin
    // proceed through 16 bytes blocs
    Cod := (Cod shl 11) xor integer(Td0[Cod shr 21]); // shr 21 -> 8*[byte] of cardinal
    P^[0] := P^[0] xor Cod;
    P^[1] := P^[1] xor Cod;
    P^[2] := P^[2] xor Cod;
    P^[3] := P^[3] xor Cod;
    inc(PByte(P), 16);
  end;
  Cod := (Cod shl 11) xor integer(Td0[Cod shr 21]);
  for i := 1 to (Count and AesBlockMod) shr 2 do
  begin
    // last 4 bytes blocs
    P^[0] := P^[0] xor Cod;
    inc(PByte(P), 4);
  end;
  for i := 1 to Count and 3 do
  begin
    PByte(P)^ := PByte(P)^ xor byte(Cod);
    inc(PByte(P));
  end;
end;

procedure XorOffset(P: PByteArray; Index, Count: PtrInt);
// XorOffset: fast and simple Cypher using Index (=Position in Dest Stream):
// Compression not OK -> apply after compress (e.g. TBZCompressor.withXor=true)
var
  Len: PtrInt;
  tab: PByteArray; // 2^13=$2000=8192 bytes of XOR tables ;)
begin
  tab := @Td0;
  if Count > 0 then
    repeat
      Index := Index and $1FFF;
      Len := $2000 - Index;
      if Len > Count then
        Len := Count;
      XorMemory(P, @tab[Index], Len);
      inc(P, Len);
      inc(Index, Len);
      Dec(Count, Len);
    until Count = 0;
end;

procedure XorConst(P: PIntegerArray; Count: integer);
// XorConst: fast Cypher changing by Count value (weak cypher but compression OK)
var
  i: integer;
  Code: integer;
begin
  // 1 to 3 bytes may stay unencrypted: not relevant
  Code := integer(Td0[Count and $3FF]);
  for i := 1 to (Count shr 4) do
  begin
    P^[0] := P^[0] xor Code;
    P^[1] := P^[1] xor Code;
    P^[2] := P^[2] xor Code;
    P^[3] := P^[3] xor Code;
    inc(PByte(P), 16);
  end;
  for i := 0 to ((Count and AesBlockMod) shr 2) - 1 do // last 4 bytes blocs
    P^[i] := P^[i] xor Code;
end;

procedure XorMemoryPtrInt(dest, source: PPtrInt; count: integer);
  {$ifdef HASINLINE}inline;{$endif}
begin
  if count > 0 then
    repeat
      dest^ := dest^ xor source^;
      inc(dest);
      inc(source);
      dec(count);
    until count = 0;
end;

function Hash128ToExt(P: PHash128Rec): TSynExtended;
const
  COEFF64: TSynExtended = (1.0 / $80000000) / $100000000;  // 2^-63
begin
  // no need to XOR with P.Hi since P input is from an AES permutation algorithm
  result := (P.Lo and $7fffffffffffffff) * COEFF64;
end;

function Hash128ToDouble(P: PHash128Rec): double;
const
  COEFF64: double = (1.0 / $80000000) / $100000000;  // 2^-63
begin
  // no need to XOR with P.Lo since P input is from an AES permutation algorithm
  result := (P.Hi and $7fffffffffffffff) * COEFF64;
end;

function Hash128ToSingle(P: PHash128Rec): single;
const
  COEFF64: single = (1.0 / $80000000) / $100000000;  // 2^-63
begin
  // no need to XOR with P.Hi since P input is from an AES permutation algorithm
  result := (P.Lo and $7fffffffffffffff) * COEFF64;
end;

function Adler32Pas(Adler: cardinal; p: pointer; Count: integer): cardinal;
// simple Adler32 implementation (twice slower than Asm, but shorter code size)
var
  s1, s2: cardinal;
  i, n: integer;
begin
  s1 := LongRec(Adler).Lo;
  s2 := LongRec(Adler).Hi;
  while Count > 0 do
  begin
    if Count < 5552 then
      n := Count
    else
      n := 5552;
    for i := 1 to n do
    begin
      inc(s1, PByte(p)^);
      inc(PByte(p));
      inc(s2, s1);
    end;
    s1 := s1 mod 65521;
    s2 := s2 mod 65521;
    dec(Count, n);
  end;
  result := (s1 and $ffff) + (s2 and $ffff) shl 16;
end;

{$ifndef CPUX86}

function Adler32Asm(Adler: cardinal; p: pointer; Count: integer): cardinal;
begin
  result := Adler32Pas(Adler, p, Count);
end;

{$endif CPUX86}

function Adler32SelfTest: boolean;
begin
  result :=
  {$ifndef PUREPASCAL}
    (Adler32Asm(1, @Te0, SizeOf(Te0)) = $BCBEFE10) and
    (Adler32Asm(7, @Te1, SizeOf(Te1) - 3) = $DA91FDBE) and
  {$endif}
    (Adler32Pas(1, @Te0, SizeOf(Te0)) = $BCBEFE10) and
    (Adler32Pas(7, @Te1, SizeOf(Te1) - 3) = $DA91FDBE);
end;

{$ifndef CPUINTEL}

procedure bswap256(s, d: PIntegerArray);
begin
  {$ifdef FPC} // use fast platform-specific function
  d[0] := SwapEndian(s[0]);
  d[1] := SwapEndian(s[1]);
  d[2] := SwapEndian(s[2]);
  d[3] := SwapEndian(s[3]);
  d[4] := SwapEndian(s[4]);
  d[5] := SwapEndian(s[5]);
  d[6] := SwapEndian(s[6]);
  d[7] := SwapEndian(s[7]);
  {$else}
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
  d[5] := bswap32(s[5]);
  d[6] := bswap32(s[6]);
  d[7] := bswap32(s[7]);
  {$endif FPC}
end;

procedure bswap160(s, d: PIntegerArray);
begin
  {$ifdef FPC} // use fast platform-specific function
  d[0] := SwapEndian(s[0]);
  d[1] := SwapEndian(s[1]);
  d[2] := SwapEndian(s[2]);
  d[3] := SwapEndian(s[3]);
  d[4] := SwapEndian(s[4]);
  {$else}
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
  {$endif FPC}
end;

{$endif CPUINTEL}

{ THash128History }

procedure THash128History.Init(size, maxsize: integer);
begin
  Depth := maxsize;
  SetLength(Previous, size);
  count := 0;
  Index := 0;
end;

function THash128History.Exists(const hash: THash128): boolean;
begin
  if count = 0 then
    result := false
  else
    result := Hash128Index(pointer(Previous), count, @hash) >= 0;
end;

function THash128History.Add(const hash: THash128): boolean;
var
  n: integer;
begin
  result := Hash128Index(pointer(Previous), count, @hash) < 0;
  if not result then
    exit;
  Previous[Index].B := hash;
  inc(Index);
  if Index >= length(Previous) then
    if Index = Depth then
      Index := 0
    else
    begin
      n := NextGrow(Index);
      if n >= Depth then
        n := Depth;
      SetLength(Previous, n);
    end;
  if count < Depth then
    inc(count);
end;


{ ********************* AES Encoding/Decoding }

procedure ComputeAesStaticTables;
var
  i, x, y: byte;
  j: PtrInt;
  pow, log: array[byte] of byte;
  c: cardinal;
begin
  // 744 bytes of x86_64 code to compute 4.5 KB of tables
  x := 1;
  for i := 0 to 255 do
  begin
    pow[i] := x;
    log[x] := i;
    if x and $80 <> 0 then
      x := x xor (x shl 1) xor $1B
    else
      x := x xor (x shl 1);
  end;
  SBox[0] := $63;
  InvSBox[$63] := 0;
  for j := 1 to 255 do
  begin
    x := pow[255 - log[j]];
    y := (x shl 1) + (x shr 7);
    x := x xor y;
    y := (y shl 1) + (y shr 7);
    x := x xor y;
    y := (y shl 1) + (y shr 7);
    x := x xor y;
    y := (y shl 1) + (y shr 7);
    x := x xor y xor $63;
    SBox[j] := x;
    InvSBox[x] := j;
  end;
  for j := 0 to 255 do
  begin
    x := SBox[j];
    y := x shl 1;
    if x and $80 <> 0 then
      y := y xor $1B;
    c := y + x shl 8 + x shl 16 + (y xor x) shl 24;
    Te0[j] := c;
    c := c shl 8 + c shr 24;
    Te1[j] := c;
    c := c shl 8 + c shr 24;
    Te2[j] := c;
    Te3[j] := c shl 8 + c shr 24;
    x := InvSBox[j];
    if x = 0 then
      continue;
    c := log[x]; // Td0[c] = Si[c].[0e,09,0d,0b] -> e.g. log[$0e]=223 below
    c := pow[(c + 223) mod 255] + pow[(c + 199) mod 255] shl 8 +
         pow[(c + 238) mod 255] shl 16 + pow[(c + 104) mod 255] shl 24;
    Td0[j] := c;
    c := c shl 8 + c shr 24;
    Td1[j] := c;
    c := c shl 8 + c shr 24;
    Td2[j] := c;
    c := c shl 8 + c shr 24;
    Td3[j] := c;
  end;
end;

{$ifndef ASMINTEL}

procedure aesencryptpas(const ctxt: TAesContext; bi, bo: PWA4);
{ AES_PASCAL version (c) Wolfgang Ehrhardt under zlib license:
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:
 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.
 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
 3. This notice may not be removed or altered from any source distribution.
 -> code has been refactored and tuned by AB especially for FPC x86_64 target }
var
  t: PCardinalArray;       // faster on a PIC system
  sb: PByteArray;
  s0, s1, s2, s3: PtrUInt; // TAesBlock s# as separate variables
  t0, t1, t2: cardinal;    // TAesBlock t# as separate variables
  pk: PWA4;
  i: integer;
begin
  pk := @ctxt.RK;
  s0 := bi[0] xor pk[0];
  s1 := bi[1] xor pk[1];
  s2 := bi[2] xor pk[2];
  s3 := bi[3] xor pk[3];
  inc(pk);
  t := @Te0;
  for i := 1 to ctxt.rounds - 1 do
  begin
    t0 := t[s0 and $ff] xor t[$100 + s1 shr 8 and $ff] xor
          t[$200 + s2 shr 16 and $ff] xor t[$300 + s3 shr 24];
    t1 := t[s1 and $ff] xor t[$100 + s2 shr 8 and $ff] xor
          t[$200 + s3 shr 16 and $ff] xor t[$300 + s0 shr 24];
    t2 := t[s2 and $ff] xor t[$100 + s3 shr 8 and $ff] xor
          t[$200 + s0 shr 16 and $ff] xor t[$300 + s1 shr 24];
    s3 := t[s3 and $ff] xor t[$100 + s0 shr 8 and $ff] xor
          t[$200 + s1 shr 16 and $ff] xor t[$300 + s2 shr 24] xor pk[3];
    s0 := t0 xor pk[0];
    s1 := t1 xor pk[1];
    s2 := t2 xor pk[2];
    inc(pk);
  end;
  sb := @SBox;
  bo[0] := ((sb[s0 and $ff]) xor (sb[s1 shr 8 and $ff]) shl 8 xor
     (sb[s2 shr 16 and $ff]) shl 16 xor (sb[s3 shr 24]) shl 24) xor pk[0];
  bo[1] := ((sb[s1 and $ff]) xor (sb[s2 shr 8 and $ff]) shl 8 xor
     (sb[s3 shr 16 and $ff]) shl 16 xor (sb[s0 shr 24]) shl 24) xor pk[1];
  bo[2] := ((sb[s2 and $ff]) xor (sb[s3 shr 8 and $ff]) shl 8 xor
     (sb[s0 shr 16 and $ff]) shl 16 xor (sb[s1 shr 24]) shl 24) xor pk[2];
  bo[3] := ((sb[s3 and $ff]) xor (sb[s0 shr 8 and $ff]) shl 8 xor
     (sb[s1 shr 16 and $ff]) shl 16 xor (sb[s2 shr 24]) shl 24) xor pk[3];
end;

{$endif ASMINTEL}

{$ifndef ASMX86}

procedure aesdecryptpas(const ctxt: TAesContext; bi, bo: PWA4);
var
  s0, s1, s2, s3: PtrUInt; // TAesBlock s# as separate variables
  t0, t1, t2: cardinal;    // TAesBlock t# as separate variables
  i: integer;
  pk: PWA4;
  t: PCardinalArray; // faster on a PIC system
  ib: PByteArray;
begin
  t := @Td0;
  // Setup key pointer
  pk := PWA4(@ctxt.RK[ctxt.Rounds]);
  // Initialize with input block
  s0 := bi[0] xor pk[0];
  s1 := bi[1] xor pk[1];
  s2 := bi[2] xor pk[2];
  s3 := bi[3] xor pk[3];
  dec(pk);
  for i := 1 to ctxt.Rounds - 1 do
  begin
    t0 := t[s0 and $ff] xor t[$100 + s3 shr 8 and $ff] xor
          t[$200 + s2 shr 16 and $ff] xor t[$300 + s1 shr 24];
    t1 := t[s1 and $ff] xor t[$100 + s0 shr 8 and $ff] xor
          t[$200 + s3 shr 16 and $ff] xor t[$300 + s2 shr 24];
    t2 := t[s2 and $ff] xor t[$100 + s1 shr 8 and $ff] xor
          t[$200 + s0 shr 16 and $ff] xor t[$300 + s3 shr 24];
    s3 := t[s3 and $ff] xor t[$100 + s2 shr 8 and $ff] xor
          t[$200 + s1 shr 16 and $ff] xor t[$300 + s0 shr 24] xor pk[3];
    s0 := t0 xor pk[0];
    s1 := t1 xor pk[1];
    s2 := t2 xor pk[2];
    dec(pk);
  end;
  ib := @InvSBox;
  bo[0] := ((ib[s0 and $ff]) xor (ib[s3 shr 8 and $ff]) shl 8 xor
    (ib[s2 shr 16 and $ff]) shl 16 xor (ib[s1 shr 24]) shl 24) xor pk[0];
  bo[1] := ((ib[s1 and $ff]) xor (ib[s0 shr 8 and $ff]) shl 8 xor
    (ib[s3 shr 16 and $ff]) shl 16 xor (ib[s2 shr 24]) shl 24) xor pk[1];
  bo[2] := ((ib[s2 and $ff]) xor (ib[s1 shr 8 and $ff]) shl 8 xor
    (ib[s0 shr 16 and $ff]) shl 16 xor (ib[s3 shr 24]) shl 24) xor pk[2];
  bo[3] := ((ib[s3 and $ff]) xor (ib[s2 shr 8 and $ff]) shl 8 xor
    (ib[s1 shr 16 and $ff]) shl 16 xor (ib[s0 shr 24]) shl 24) xor pk[3];
end;

{$endif ASMX86}

procedure ShiftPas(KeySize: cardinal; pk: PAWK);
var
  i: integer;
  temp: cardinal;
  sb: PByteArray;  // faster on PIC
begin
  sb := @SBox;
  case KeySize of
    128:
      for i := 0 to 9 do
      begin
        temp := pk^[3];
        // SubWord(RotWord(temp)) if "word" count mod 4 = 0
        pk^[4] := ((sb[(temp shr 8) and $ff])) xor
                  ((sb[(temp shr 16) and $ff]) shl 8) xor
                  ((sb[(temp shr 24)]) shl 16) xor
                  ((sb[(temp) and $ff]) shl 24) xor pk^[0] xor RCon[i];
        pk^[5] := pk^[1] xor pk^[4];
        pk^[6] := pk^[2] xor pk^[5];
        pk^[7] := pk^[3] xor pk^[6];
        inc(PByte(pk), 4 * 4);
      end;
    192:
      for i := 0 to 7 do
      begin
        temp := pk^[5];
        // SubWord(RotWord(temp)) if "word" count mod 6 = 0
        pk^[6] := ((sb[(temp shr 8) and $ff])) xor
                  ((sb[(temp shr 16) and $ff]) shl 8) xor
                  ((sb[(temp shr 24)]) shl 16) xor
                  ((sb[(temp) and $ff]) shl 24) xor pk^[0] xor RCon[i];
        pk^[7] := pk^[1] xor pk^[6];
        pk^[8] := pk^[2] xor pk^[7];
        pk^[9] := pk^[3] xor pk^[8];
        if i = 7 then
          exit;
        pk^[10] := pk^[4] xor pk^[9];
        pk^[11] := pk^[5] xor pk^[10];
        inc(PByte(pk), 6 * 4);
      end;
  else // 256
    for i := 0 to 6 do
    begin
      temp := pk^[7];
      // SubWord(RotWord(temp)) if "word" count mod 8 = 0
      pk^[8] := ((sb[(temp shr 8) and $ff])) xor
                ((sb[(temp shr 16) and $ff]) shl 8) xor
                ((sb[(temp shr 24)]) shl 16) xor
                ((sb[(temp) and $ff]) shl 24) xor pk^[0] xor RCon[i];
      pk^[9] := pk^[1] xor pk^[8];
      pk^[10] := pk^[2] xor pk^[9];
      pk^[11] := pk^[3] xor pk^[10];
      if i = 6 then
        exit;
      temp := pk^[11];
      // SubWord(temp) if "word" count mod 8 = 4
      pk^[12] := ((sb[(temp) and $ff])) xor
                 ((sb[(temp shr 8) and $ff]) shl 8) xor
                 ((sb[(temp shr 16) and $ff]) shl 16) xor
                 ((sb[(temp shr 24)]) shl 24) xor pk^[4];
      pk^[13] := pk^[5] xor pk^[12];
      pk^[14] := pk^[6] xor pk^[13];
      pk^[15] := pk^[7] xor pk^[14];
      inc(PByte(pk), 8 * 4);
    end;
  end;
end;

procedure MakeDecrKeyPas(rounds: integer; k: PAWk);
// compute AES decryption key from encryption key
var
  x: cardinal;
  t: PCardinalArray; // faster on a PIC system
  sb: PByteArray;
begin
  t := @Td0;
  sb := @SBox;
  repeat
    inc(PByte(k), 16);
    dec(rounds);
    x := k[0];
    k[0] := t[$300 + sb[x shr 24]] xor t[$200 + sb[x shr 16 and $ff]] xor
            t[$100 + sb[x shr 8 and $ff]] xor t[sb[x and $ff]];
    x := k[1];
    k[1] := t[$300 + sb[x shr 24]] xor t[$200 + sb[x shr 16 and $ff]] xor
            t[$100 + sb[x shr 8 and $ff]] xor t[sb[x and $ff]];
    x := k[2];
    k[2] := t[$300 + sb[x shr 24]] xor t[$200 + sb[x shr 16 and $ff]] xor
            t[$100 + sb[x shr 8 and $ff]] xor t[sb[x and $ff]];
    x := k[3];
    k[3] := t[$300 + sb[x shr 24]] xor t[$200 + sb[x shr 16 and $ff]] xor
            t[$100 + sb[x shr 8 and $ff]] xor t[sb[x and $ff]];
  until rounds = 1;
end;


{ TAes }

procedure TAes.Encrypt(var B: TAesBlock);
begin
  TAesContext(Context).DoBlock(Context, B, B);
end;

procedure TAes.Encrypt(const BI: TAesBlock; var BO: TAesBlock);
begin
  TAesContext(Context).DoBlock(Context, BI, BO);
end;

function TAes.EncryptInit(const Key; KeySize: cardinal): boolean;
var
  Nk: integer;
  ctx: TAesContext absolute Context;
begin
  result := true;
  ctx.Initialized := true;
  if (KeySize <> 128) and
     (KeySize <> 192) and
     (KeySize <> 256) then
  begin
    result := false;
    ctx.Initialized := false;
    exit;
  end;
  Nk := KeySize div 32;
  MoveFast(Key, ctx.RK, 4 * Nk);
  {$ifdef ASMINTEL}
  ctx.DoBlock := @aesencryptasm;
  {$else}
  ctx.DoBlock := @aesencryptpas;
  {$endif ASMINTEL}
  {$ifdef USEAESNI}
  if cfAESNI in CpuFeatures then
  begin
    case KeySize of
      128:
        ctx.DoBlock := @aesniencrypt128;
      192:
        ctx.DoBlock := @aesniencrypt192;
      256:
        ctx.DoBlock := @aesniencrypt256;
    end;
    {$ifdef USEAESNI32}
    case KeySize of
      128:
        ctx.AesNi32 := @AesNiEncryptXmm7_128;
      192:
        ctx.AesNi32 := @AesNiEncryptXmm7_192;
      256:
        ctx.AesNi32 := @AesNiEncryptXmm7_256;
    end;
    {$endif USEAESNI32}
  end;
  {$endif USEAESNI}
  ctx.Rounds := 6 + Nk;
  ctx.KeyBits := KeySize;
  // Calculate encryption round keys
  {$ifdef USEAESNI}
  // 192 is more complex and seldom used -> skip to pascal
  if (KeySize <> 192) and
     (cfAESNI in CpuFeatures) then
    ShiftAesNi(KeySize, @ctx.RK)
  else
  {$endif USEAESNI}
    ShiftPas(KeySize, pointer(@ctx.RK));
end;

function TAes.DecryptInitFrom(const Encryption: TAes;
  const Key; KeySize: cardinal): boolean;
var
  ctx: TAesContext absolute Context;
begin
  ctx.Initialized := false;
  if not Encryption.Initialized then
    // e.g. called from DecryptInit()
    EncryptInit(Key, KeySize)
  else // contains Initialized := true
    self := Encryption;
  result := ctx.Initialized;
  if not result then
    exit;
  {$ifdef ASMX86}
  ctx.DoBlock := @aesdecrypt386;
  {$else}
  ctx.DoBlock := @aesdecryptpas;
  {$endif ASMX86}
  {$ifdef USEAESNI}
  if cfAESNI in CpuFeatures then
  begin
    MakeDecrKeyAesNi(ctx.Rounds, @ctx.RK);
    case KeySize of
      128:
        ctx.DoBlock := @aesnidecrypt128;
      192:
        ctx.DoBlock := @aesnidecrypt192;
      256:
        ctx.DoBlock := @aesnidecrypt256;
    end;
  end
  else
  {$endif USEAESNI}
    MakeDecrKeyPas(ctx.Rounds, @ctx.RK);
end;

function TAes.DecryptInit(const Key; KeySize: cardinal): boolean;
begin
  result := DecryptInitFrom(self, Key, KeySize);
end;

procedure TAes.Decrypt(var B: TAesBlock);
begin
  TAesContext(Context).DoBlock(Context, B, B);
end;

procedure TAes.Decrypt(const BI: TAesBlock; var BO: TAesBlock);
begin
  TAesContext(Context).DoBlock(Context, BI, BO);
end;

procedure TAes.DoBlocks(pIn, pOut: PAesBlock; out oIn, oOut: PAesBLock;
  Count: integer; doEncrypt: boolean);
var
  ctx: TAesContext absolute Context;
begin
  if Count > 0 then
    repeat
      ctx.DoBlock(ctx, pIn^, pOut^);
      inc(pIn);
      inc(pOut);
      dec(Count);
    until Count = 0;
  oIn := pIn;
  oOut := pOut;
end;

function TAes.DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
begin
  if doEncrypt then
    result := EncryptInit(Key, KeySize)
  else
    result := DecryptInit(Key, KeySize);
end;

procedure TAes.DoBlocks(pIn, pOut: PAesBlock; Count: integer; doEncrypt: boolean);
begin
  DoBlocks(pIn, pOut, pIn, pOut, Count, doEncrypt);
end;

procedure TAes.DoBlocksOfb(iv: PAesBlock; src, dst: pointer;
  blockcount: PtrUInt);
var
  cv: TAesBlock;
begin
  {$ifdef USEAESNI64}
  if cfAESNI in CpuFeatures then
    case integer(TAesContext(Context).KeyBits) of
      128:
        begin
          AesNiEncryptOfb128(iv, @Context, src, dst, blockcount);
          exit;
        end;
      256:
        begin
          AesNiEncryptOfb256(iv, @Context, src, dst, blockcount);
          exit;
        end;
    end;
  {$endif USEAESNI64}
  cv := iv^;
  if blockcount > 0 then
    repeat
      TAesContext(Context).DoBlock(Context, cv, cv);
      XorBlock16(src, dst, pointer(@cv));
      inc(PByte(src), SizeOf(TAesBlock));
      inc(PByte(dst), SizeOf(TAesBlock));
      dec(blockcount);
    until blockcount = 0;
end;

function TAes.Initialized: boolean;
begin
  result := TAesContext(Context).Initialized;
end;

function TAes.UsesAesni: boolean;
begin
  {$ifdef ASMINTEL}
  result := cfAESNI in CpuFeatures;
  {$else}
  result := false;
  {$endif ASMINTEL}
end;

function TAes.KeyBits: integer;
begin
  result := TAesContext(Context).KeyBits;
end;

procedure TAes.Done;
var
  ctx: TAesContext absolute Context;
begin
  FillcharFast(ctx, SizeOf(ctx), 0); // always erase key in memory after use
end;

function AesTablesTest: boolean;
begin
  // ensure that we have $2000 bytes of contiguous XOR tables ;)
  result := (PtrUInt(@TD0) + $400 = PtrUInt(@TD1)) and
            (PtrUInt(@TD0) + $800 = PtrUInt(@TD2)) and
            (PtrUInt(@TD0) + $C00 = PtrUInt(@TD3)) and
            (PtrUInt(@TD0) + $1000 = PtrUInt(@TE0)) and
            (PtrUInt(@TD0) + $1400 = PtrUInt(@TE1)) and
            (PtrUInt(@TD0) + $1800 = PtrUInt(@TE2)) and
            (PtrUInt(@TD0) + $1C00 = PtrUInt(@TE3)) and
            (SBox[255] = $16) and
            (InvSBox[0] = $52) and
            (Te0[0] = $a56363c6) and
            (Te0[255] = $3a16162c) and
            (Te1[0] = $6363c6a5) and
            (Te1[255] = $16162c3a) and
            (Te3[0] = $c6a56363) and
            (Te3[255] = $2c3a1616) and
            (Td0[0] = $50a7f451) and
            (Td0[99] = 0) and
            (Td0[255] = $4257b8d0) and
            (Td3[0] = $5150a7f4) and
            (Td3[255] = $d04257b8);
end;


{ AES-GCM Support }

const
  // 512 bytes lookup table as used by mul_x/gf_mul/gf_mul_h
  gft_le: TByteToWord = (
     $0000, $c201, $8403, $4602, $0807, $ca06, $8c04, $4e05,
     $100e, $d20f, $940d, $560c, $1809, $da08, $9c0a, $5e0b,
     $201c, $e21d, $a41f, $661e, $281b, $ea1a, $ac18, $6e19,
     $3012, $f213, $b411, $7610, $3815, $fa14, $bc16, $7e17,
     $4038, $8239, $c43b, $063a, $483f, $8a3e, $cc3c, $0e3d,
     $5036, $9237, $d435, $1634, $5831, $9a30, $dc32, $1e33,
     $6024, $a225, $e427, $2626, $6823, $aa22, $ec20, $2e21,
     $702a, $b22b, $f429, $3628, $782d, $ba2c, $fc2e, $3e2f,
     $8070, $4271, $0473, $c672, $8877, $4a76, $0c74, $ce75,
     $907e, $527f, $147d, $d67c, $9879, $5a78, $1c7a, $de7b,
     $a06c, $626d, $246f, $e66e, $a86b, $6a6a, $2c68, $ee69,
     $b062, $7263, $3461, $f660, $b865, $7a64, $3c66, $fe67,
     $c048, $0249, $444b, $864a, $c84f, $0a4e, $4c4c, $8e4d,
     $d046, $1247, $5445, $9644, $d841, $1a40, $5c42, $9e43,
     $e054, $2255, $6457, $a656, $e853, $2a52, $6c50, $ae51,
     $f05a, $325b, $7459, $b658, $f85d, $3a5c, $7c5e, $be5f,
     $00e1, $c2e0, $84e2, $46e3, $08e6, $cae7, $8ce5, $4ee4,
     $10ef, $d2ee, $94ec, $56ed, $18e8, $dae9, $9ceb, $5eea,
     $20fd, $e2fc, $a4fe, $66ff, $28fa, $eafb, $acf9, $6ef8,
     $30f3, $f2f2, $b4f0, $76f1, $38f4, $faf5, $bcf7, $7ef6,
     $40d9, $82d8, $c4da, $06db, $48de, $8adf, $ccdd, $0edc,
     $50d7, $92d6, $d4d4, $16d5, $58d0, $9ad1, $dcd3, $1ed2,
     $60c5, $a2c4, $e4c6, $26c7, $68c2, $aac3, $ecc1, $2ec0,
     $70cb, $b2ca, $f4c8, $36c9, $78cc, $bacd, $fccf, $3ece,
     $8091, $4290, $0492, $c693, $8896, $4a97, $0c95, $ce94,
     $909f, $529e, $149c, $d69d, $9898, $5a99, $1c9b, $de9a,
     $a08d, $628c, $248e, $e68f, $a88a, $6a8b, $2c89, $ee88,
     $b083, $7282, $3480, $f681, $b884, $7a85, $3c87, $fe86,
     $c0a9, $02a8, $44aa, $86ab, $c8ae, $0aaf, $4cad, $8eac,
     $d0a7, $12a6, $54a4, $96a5, $d8a0, $1aa1, $5ca3, $9ea2,
     $e0b5, $22b4, $64b6, $a6b7, $e8b2, $2ab3, $6cb1, $aeb0,
     $f0bb, $32ba, $74b8, $b6b9, $f8bc, $3abd, $7cbf, $bebe);

procedure mul_x(var a: THash128Rec; const b: THash128Rec);
// {$ifdef HASINLINE}inline;{$endif} // inlining has no benefit here
var
  t: cardinal;
  y: TWA4 absolute b;
const
  MASK_80 = cardinal($80808080);
  MASK_7F = cardinal($7f7f7f7f);
begin
  t := gft_le[(y[3] shr 17) and MASK_80];
  a.c3 := ((y[3] shr 1) and MASK_7F) or
          (((y[3] shl 15) or (y[2] shr 17)) and MASK_80);
  a.c2 := ((y[2] shr 1) and MASK_7F) or
                (((y[2] shl 15) or (y[1] shr 17)) and MASK_80);
  a.c1 := ((y[1] shr 1) and MASK_7F) or
          (((y[1] shl 15) or (y[0] shr 17)) and MASK_80);
  a.c0 := (((y[0] shr 1) and MASK_7F) or
          ((y[0] shl 15) and MASK_80)) xor t;
end;

procedure gf_mul_pas(var a: TAesBlock; const b: TAesBlock);
var
  p: array[0 .. 7] of THash128Rec;
  x: THash128Rec;
  t: cardinal;
  i: PtrInt;
  j: integer;
  c: byte;
begin
  p[0].b := b;
  for i := 1 to 7 do
    mul_x(p[i], p[i - 1]);
  FillZero(x.b);
  for i := 0 to 15 do
  begin
    c := a[15 - i];
    if i > 0 then
    begin
      // inlined mul_x8()
      t := gft_le[x.c3 shr 24];
      x.c3 := ((x.c3 shl 8) or  (x.c2 shr 24));
      x.c2 := ((x.c2 shl 8) or  (x.c1 shr 24));
      x.c1 := ((x.c1 shl 8) or  (x.c0 shr 24));
      x.c0 := ((x.c0 shl 8) xor t);
    end;
    for j := 0 to 7 do
    begin
      if c and ($80 shr j) <> 0 then
      begin
        x.c3 := x.c3 xor p[j].c3;
        x.c2 := x.c2 xor p[j].c2;
        x.c1 := x.c1 xor p[j].c1;
        x.c0 := x.c0 xor p[j].c0;
      end;
    end;
  end;
  a := x.b;
end;

procedure gf_mul(var a: TAesBlock; const b: TAesBlock);
  {$ifdef HASINLINE} inline; {$endif}
begin
  {$ifdef USECLMUL}
  if cfCLMUL in CpuFeatures then
    gf_mul_pclmulqdq(@a, @b)
  else
  {$endif USECLMUL}
    gf_mul_pas(a, b);
end;

procedure gf_mul_h(const eng: TAesGcmEngine; var a: TAesBlock);
  {$ifdef HASINLINE} inline; {$endif}
begin
  {$ifdef USECLMUL}
  if flagCLMUL in eng.flags then
    gf_mul_pclmulqdq(@a, @eng.ghash_h)
  else
  {$endif USECLMUL}
    // use pure pascal efficient code with 4KB pre-computed table
    eng.gf_mul_h_pas(a);
end;


{ TAesGcmEngine }

procedure TAesGcmEngine.Make4K_Table;
var
  j, k: PtrInt;
begin
  gf_t4k[128].b := ghash_h;
  j := 64;
  while j > 0 do
  begin
    mul_x(gf_t4k[j], gf_t4k[j + j]);
    j := j shr 1;
  end;
  j := 2;
  while j < 256 do
  begin
    for k := 1 to j - 1 do
      XorBlock16(@gf_t4k[k], @gf_t4k[j + k], @gf_t4k[j]);
    inc(j, j);
  end;
end;

procedure TAesGcmEngine.gf_mul_h_pas(var a: TAesBlock);
var
  i: PtrUInt;
  x0, x1, x2, x3, t: cardinal; // will use registers on x86_64/arm
  p: PWA4;
begin
  with gf_t4k[a[15]] do
  begin
    x0 := c0;
    x1 := c1;
    x2 := c2;
    x3 := c3;
  end;
  for i := 14 downto 0 do
  begin
    p := @gf_t4k[a[i]];
    t := gft_le[x3 shr 24];
    // efficient mul_x8 and xor using pre-computed table entries
    x3 := ((x3 shl 8) or  (x2 shr 24)) xor p^[3];
    x2 := ((x2 shl 8) or  (x1 shr 24)) xor p^[2];
    x1 := ((x1 shl 8) or  (x0 shr 24)) xor p^[1];
    x0 := ((x0 shl 8) xor t) xor p^[0];
  end;
  with PHash128Rec(@a)^ do
  begin
    c0 := x0;
    c1 := x1;
    c2 := x2;
    c3 := x3;
  end;
end;

procedure GCM_IncCtr(var x: TAesBlock);
  {$ifdef HASINLINE} inline; {$endif}
begin
  // in AES-GCM, CTR covers only 32 LSB Big-Endian bits, i.e. x[15]..x[12]
  inc(x[15]);
  if x[15] <> 0 then
    exit;
  inc(x[14]);
  if x[14] <> 0 then
    exit;
  inc(x[13]);
  if x[13] = 0 then
    inc(x[12]);
end;

procedure TAesGcmEngine.internal_crypt(ptp, ctp: PByte; ILen: PtrUInt);
var
  b_pos: PtrUInt;
begin
  b_pos := blen;
  inc(blen, ILen);
  blen := blen and AesBlockMod;
  if b_pos = 0 then
    b_pos := SizeOf(TAesBlock)
  else
    while (ILen > 0) and
          (b_pos<SizeOf(TAesBlock)) do
    begin
      ctp^ := ptp^ xor TAesContext(actx).buf[b_pos];
      inc(b_pos);
      inc(ptp);
      inc(ctp);
      dec(ILen);
    end;
  while ILen >= SizeOf(TAesBlock) do
  begin
    GCM_IncCtr(TAesContext(actx).iv.b);
    actx.Encrypt(TAesContext(actx).iv.b, TAesContext(actx).buf); // maybe AES-NI
    XorBlock16(pointer(ptp), pointer(ctp), @TAesContext(actx).buf);
    inc(PAesBlock(ptp));
    inc(PAesBlock(ctp));
    dec(ILen, SizeOf(TAesBlock));
  end;
  while ILen > 0 do
  begin
    if b_pos = SizeOf(TAesBlock) then
    begin
      GCM_IncCtr(TAesContext(actx).iv.b);
      actx.Encrypt(TAesContext(actx).iv.b, TAesContext(actx).buf);
      b_pos := 0;
    end;
    ctp^ := TAesContext(actx).buf[b_pos] xor ptp^;
    inc(b_pos);
    inc(ptp);
    inc(ctp);
    dec(ILen);
  end;
end;

procedure TAesGcmEngine.internal_auth(ctp: PByte; ILen: PtrUInt;
  var ghv: TAesBlock; var gcnt: TQWordRec);
var
  b_pos: PtrUInt;
begin
  b_pos := gcnt.L and AesBlockMod;
  inc(gcnt.V, ILen);
  if (b_pos = 0) and
     (gcnt.V <> 0) then
    gf_mul_h(self, ghv);
  while (ILen > 0) and
        (b_pos < SizeOf(TAesBlock)) do
  begin
    ghv[b_pos] := ghv[b_pos] xor ctp^;
    inc(b_pos);
    inc(ctp);
    dec(ILen);
  end;
  while ILen >= SizeOf(TAesBlock) do
  begin
    gf_mul_h(self, ghv);
    XorBlock16(@ghv, pointer(ctp));
    inc(PAesBlock(ctp));
    dec(ILen, SizeOf(TAesBlock));
  end;
  while ILen > 0 do
  begin
    if b_pos = SizeOf(TAesBlock) then
    begin
      gf_mul_h(self, ghv);
      b_pos := 0;
    end;
    ghv[b_pos] := ghv[b_pos] xor ctp^;
    inc(b_pos);
    inc(ctp);
    dec(ILen);
  end;
end;

function TAesGcmEngine.Init(const Key; KeyBits: PtrInt): boolean;
begin
  FillcharFast(self,SizeOf(self), 0);
  result := actx.EncryptInit(Key, KeyBits);
  if not result then
    exit;
  actx.Encrypt(ghash_h, ghash_h);
  {$ifdef USECLMUL}
  if cfCLMUL in CpuFeatures then
    include(flags, flagCLMUL)
  else
  {$endif USECLMUL}
    Make4K_Table;
end;

const
  CTR_POS  = 12;

function TAesGcmEngine.Reset(pIV: pointer; IV_len: PtrInt): boolean;
var
  i, n_pos: PtrInt;
begin
  if (pIV = nil) or
     (IV_len = 0) then
  begin
    result := false;
    exit;
  end;
  if IV_len = CTR_POS then
  begin
    // Initialization Vector size matches perfect size of 12 bytes
    MoveFast(pIV^, TAesContext(actx).iv, CTR_POS);
    TAesContext(actx).iv.c3 := $01000000;
  end
  else
  begin
    // Initialization Vector is otherwise computed from GHASH(IV,H)
    n_pos := IV_len;
    FillZero(TAesContext(actx).iv.b);
    while n_pos >= SizeOf(TAesBlock) do
    begin
      XorBlock16(@TAesContext(actx).iv, pIV);
      inc(PAesBlock(pIV));
      dec(n_pos, SizeOf(TAesBlock));
      gf_mul_h(self, TAesContext(actx).iv.b);
    end;
    if n_pos > 0 then
    begin
      for i := 0 to n_pos - 1 do
        TAesContext(actx).iv.b[i] := TAesContext(actx).iv.b[i] xor PAesBlock(pIV)^[i];
      gf_mul_h(self, TAesContext(actx).iv.b);
    end;
    n_pos := IV_len shl 3;
    i := 15;
    while n_pos > 0 do
    begin
      TAesContext(actx).iv.b[i] := TAesContext(actx).iv.b[i] xor byte(n_pos);
      n_pos := n_pos shr 8;
      dec(i);
    end;
    gf_mul_h(self, TAesContext(actx).iv.b);
  end;
  // reset internal state and counters
  y0_val := TAesContext(actx).iv.c3;
  FillZero(aad_ghv);
  FillZero(txt_ghv);
  aad_cnt.V := 0;
  atx_cnt.V := 0;
  flags := [];
  {$ifdef USECLMUL}
  if cfCLMUL in CpuFeatures then
    include(flags, flagCLMUL);
  {$endif USECLMUL}
  result := true;
end;

function TAesGcmEngine.Encrypt(ptp, ctp: Pointer; ILen: PtrInt): boolean;
begin
  if ILen > 0 then
  begin
    if (ptp = nil) or
       (ctp = nil) or
       (flagFinalComputed in flags) then
    begin
      result := false;
      exit;
    end;
    if (ILen and AesBlockMod = 0) and
       (blen = 0) then
    begin
      inc(atx_cnt.V, ILen);
      ILen := ILen shr AesBlockShift;
      repeat
        // single-pass loop optimized e.g. for PKCS7 padding
        {%H-}GCM_IncCtr(TAesContext(actx).iv.b);
        TAesContext(actx).DoBlock(actx, TAesContext(actx).iv,
          TAesContext(actx).buf); // maybe AES-NI
        XorBlock16(ptp, ctp, @TAesContext(actx).buf);
        gf_mul_h(self, txt_ghv);
        XorBlock16(@txt_ghv, ctp);
        inc(PAesBlock(ptp));
        inc(PAesBlock(ctp));
        dec(ILen);
      until ILen = 0;
    end
    else
    begin
      // generic process in dual pass
      internal_crypt(ptp, ctp,iLen);
      internal_auth(ctp, ILen, txt_ghv, atx_cnt);
    end;
  end;
  result := true;
end;

function TAesGcmEngine.Decrypt(ctp, ptp: Pointer; ILen: PtrInt;
  ptag: pointer; tlen: PtrInt): boolean;
var
  tag: TAesBlock;
begin
  result := false;
  if ILen > 0 then
  begin
    if (ptp = nil) or
       (ctp = nil) or
       (flagFinalComputed in flags) then
      exit;
    if (ILen and AesBlockMod = 0) and
       (blen = 0) then
    begin
      inc(atx_cnt.V, ILen);
      ILen := ILen shr AesBlockShift;
      repeat
        // single-pass loop optimized e.g. for PKCS7 padding
        gf_mul_h(self, txt_ghv);
        XorBlock16(@txt_ghv, ctp);
        GCM_IncCtr(TAesContext(actx).iv.b);
        actx.Encrypt(TAesContext(actx).iv.b, TAesContext(actx).buf); // maybe AES-NI
        XorBlock16(ctp, ptp, @TAesContext(actx).buf);
        inc(PAesBlock(ptp));
        inc(PAesBlock(ctp));
        dec(ILen);
      until ILen = 0;
      if (ptag <> nil) and
         (tlen > 0) then
      begin
        Final(tag, {anddone=}false);
        if not IsEqual(tag, ptag^, tlen) then
          // check authentication after single pass encryption + auth
          exit;
      end;
    end
    else
    begin
      // generic process in dual steps
      internal_auth(ctp, ILen, txt_ghv, atx_cnt);
      if (ptag <> nil) and
         (tlen > 0) then
      begin
        Final(tag, {anddone=}false);
        if not IsEqual(tag, ptag^, tlen) then
          // check authentication before encryption
          exit;
      end;
      internal_crypt(ctp, ptp, iLen);
    end;
  end;
  result := true;
end;

function TAesGcmEngine.Add_AAD(pAAD: pointer; aLen: PtrInt): boolean;
begin
  if aLen > 0 then
  begin
    if (pAAD = nil) or
       (flagFinalComputed in flags) then
    begin
      result := false;
      exit;
    end;
    internal_auth(pAAD, aLen, aad_ghv, aad_cnt);
  end;
  result := true;
end;

function TAesGcmEngine.Final(out tag: TAesBlock; andDone: boolean): boolean;
var
  tbuf: TAesBlock;
  ln: cardinal;
begin
  if not (flagFinalComputed in flags) then
  begin
    include(flags, flagFinalComputed);
    // compute GHASH(H, AAD, ctp)
    gf_mul_h(self, aad_ghv);
    gf_mul_h(self, txt_ghv);
    // compute len(AAD) || len(ctp) with each len as 64-bit big-endian
    ln := (atx_cnt.V + AesBlockMod) shr AesBlockShift;
    if (aad_cnt.V > 0) and
       (ln <> 0) then
    begin
      tbuf := ghash_h;
      while ln <> 0 do
      begin
        if odd(ln) then
          gf_mul(aad_ghv, tbuf);
        ln := ln shr 1;
        if ln <> 0 then
          gf_mul(tbuf, tbuf);
      end;
    end;
    TWA4(tbuf)[0] := bswap32((aad_cnt.L shr 29) or (aad_cnt.H shl 3));
    TWA4(tbuf)[1] := bswap32((aad_cnt.L shl  3));
    TWA4(tbuf)[2] := bswap32((atx_cnt.L shr 29) or (atx_cnt.H shl 3));
    TWA4(tbuf)[3] := bswap32((atx_cnt.L shl  3));
    XorBlock16(@tbuf, @txt_ghv);
    XorBlock16(@aad_ghv, @tbuf);
    gf_mul_h(self, aad_ghv);
    // compute E(K,Y0)
    tbuf := TAesContext(actx).iv.b;
    TWA4(tbuf)[3] := y0_val;
    actx.Encrypt(tbuf);
    // GMAC = GHASH(H, AAD, ctp) xor E(K,Y0)
    XorBlock16(@aad_ghv, @tag, @tbuf);
    if andDone then
      Done;
    result := true;
  end
  else
  begin
    Done;
    result := false;
  end;
end;

procedure TAesGcmEngine.Done;
begin
  if flagFlushed in flags then
    exit;
  actx.Done;
  include(flags, flagFlushed);
end;

function TAesGcmEngine.FullEncryptAndAuthenticate(const Key; KeyBits: PtrInt;
  pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt; ptp, ctp: Pointer;
  pLen: PtrInt; out tag: TAesBlock): boolean;
begin
  result := Init(Key, KeyBits) and
            Reset(pIV, IV_len) and
            Add_AAD(pAAD, aLen) and
            Encrypt(ptp, ctp,pLen) and
            Final(tag);
  Done;
end;

function TAesGcmEngine.FullDecryptAndVerify(const Key; KeyBits: PtrInt;
  pIV: pointer; IV_len: PtrInt; pAAD: pointer; aLen: PtrInt; ctp, ptp: Pointer;
  pLen: PtrInt; ptag: pointer; tLen: PtrInt): boolean;
begin
  result := Init(Key, KeyBits) and
            Reset(pIV, IV_len) and
            Add_AAD(pAAD, aLen) and
            Decrypt(ctp,ptp, pLen, ptag, tlen);
  Done;
end;



{ TAesAbstract }

function SetMainAesPrng: TAesPrng;
begin
  GlobalLock;
  if MainAesPrng = nil then
    MainAesPrng := TAesPrng.Create;
  GlobalUnLock;
  result := MainAesPrng;
end;

// defined here for proper inlining :)
class function TAesPrng.Main: TAesPrng;
begin
  result := MainAesPrng;
  if result = nil then
    result := SetMainAesPrng;
end;

var
  aesivctr: array[boolean] of TAesLocked;

procedure AesIVCtrEncryptDecrypt(const BI; var BO; DoEncrypt: boolean);
begin
  if aesivctr[DoEncrypt] = nil then
  begin
    aesivctr[DoEncrypt] := TAesLocked.Create;
    with aesivctr[DoEncrypt].fAes do
      if DoEncrypt then
        EncryptInit(AESIVCTR_KEY, 128)
      else
        DecryptInit(AESIVCTR_KEY, 128);
  end;
  with aesivctr[DoEncrypt] do
  begin
    EnterCriticalSection(fSafe);
    TAesContext(fAes).DoBlock(fAes, BI, BO);
    LeaveCriticalSection(fSafe);
  end;
end;

function ToText(chk: TAesIVReplayAttackCheck): PShortString;
begin
  result := GetEnumName(TypeInfo(TAesIVReplayAttackCheck), ord(chk));
end;

constructor TAesAbstract.Create(const aKey; aKeySizeBits: cardinal);
begin
  if (aKeySizeBits <> 128) and
     (aKeySizeBits <> 192) and
     (aKeySizeBits <> 256) then
    raise ESynCrypto.CreateUtf8('%.Create(KeySize=%): 128/192/256 required',
      [self, aKeySizeBits]);
  fKeySize := aKeySizeBits;
  fKeySizeBytes := fKeySize shr 3;
  MoveFast(aKey, fKey, fKeySizeBytes);
end;

constructor TAesAbstract.Create(const aKey: THash128);
begin
  Create(aKey, 128);
end;

constructor TAesAbstract.Create(const aKey: THash256);
begin
  Create(aKey, 256);
end;

constructor TAesAbstract.Create(const aKey: TBytes);
begin
  Create(pointer(aKey)^, length(aKey) shl 3);
end;

constructor TAesAbstract.CreateTemp(aKeySize: cardinal);
var
  tmp: THash256;
begin
  TAesPrng.Main.FillRandom(tmp);
  Create(tmp, aKeySize);
  FillZero(tmp);
end;

{$ifndef PUREMORMOT2}

{$warn SYMBOL_DEPRECATED OFF} // we know it is deprecated

constructor TAesAbstract.CreateFromSha256(const aKey: RawUtf8);
var
  Digest: TSha256Digest;
begin
  Sha256Weak(aKey, Digest);
  Create(Digest, 256);
  FillZero(Digest);
end;

{$endif PUREMORMOT2}

constructor TAesAbstract.CreateFromPbkdf2(const aKey: RawUtf8;
  const aSalt: RawByteString; aRounds: integer);
var
  Digest: TSha256Digest;
begin
  PBKDF2_HMAC_SHA256(aKey, aSalt, aRounds, Digest, ToText(ClassType));
  Create(Digest, 256);
  FillZero(Digest);
end;

destructor TAesAbstract.Destroy;
begin
  inherited Destroy;
  FillZero(fKey);
end;

procedure TAesAbstract.SetIVCtr;
var
  tmp: PShortString; // temp variable to circumvent FPC bug
begin
  repeat
    TAesPrng.Main.FillRandom(TAesBLock(fIVCtr)); // set nonce + ctr
  until fIVCtr.nonce <> 0;
  tmp := ClassNameShort(self);
  fIVCtr.magic := crc32c($aba5aba5, @tmp^[2], 6); // TAesEcbApi -> 'AESECB'
end;

function TAesAbstract.AlgoName: TShort16;
const
  TXT: array[2..4] of array[0..7] of AnsiChar =
    (#9'aes128', #9'aes192', #9'aes256');
var
  s: PShortString;
begin
  if (self = nil) or
     (KeySize = 0) then
    result[0] := #0
  else
  begin
    PInt64(@result)^ := PInt64(@TXT[KeySize shr 6])^;
    s := ClassNameShort(self);
    if s^[0] < #7 then
      result[0] := #6
    else
    begin
      result[7] := NormToLower[s^[5]]; // TAesCbc -> 'aes128cbc'
      result[8] := NormToLower[s^[6]];
      result[9] := NormToLower[s^[7]];
    end;
  end;
end;

procedure TAesAbstract.SetIVHistory(aDepth: integer);
begin
  fIVHistoryDec.Init(aDepth, aDepth);
end;

function TAesAbstract.EncryptPkcs7(const Input: RawByteString;
  IVAtBeginning: boolean): RawByteString;
begin
  SetString(result, nil, EncryptPkcs7Length(length(Input), IVAtBeginning));
  EncryptPkcs7Buffer(pointer(Input), pointer(result),
    length(Input), length(result), IVAtBeginning);
end;

function TAesAbstract.EncryptPkcs7(const Input: TBytes;
  IVAtBeginning: boolean): TBytes;
begin
  result := nil;
  SetLength(result, EncryptPkcs7Length(length(Input), IVAtBeginning));
  EncryptPkcs7Buffer(pointer(Input), pointer(result),
    length(Input), length(result), IVAtBeginning);
end;

function TAesAbstract.EncryptPkcs7Length(InputLen: cardinal;
  IVAtBeginning: boolean): cardinal;
begin
  result := InputLen + SizeOf(TAesBlock) - (InputLen and AesBlockMod);
  if IVAtBeginning then
    inc(result, SizeOf(TAesBlock));
end;

function TAesAbstract.EncryptPkcs7Buffer(Input, Output: pointer;
  InputLen, OutputLen: cardinal; IVAtBeginning: boolean): boolean;
var
  padding, ivsize: cardinal;
begin
  padding := SizeOf(TAesBlock) - (InputLen and AesBlockMod);
  if IVAtBeginning then
    ivsize := SizeOf(TAesBlock)
  else
    ivsize := 0;
  if OutputLen <> ivsize + InputLen + padding then
  begin
    result := false;
    exit;
  end;
  if IVAtBeginning then
  begin
    if fIVReplayAttackCheck <> repNoCheck then
    begin
      if fIVCtr.nonce = 0 then
        SetIVCtr;
      AesIVCtrEncryptDecrypt(fIVCtr, fIV, true); // PRNG from fixed secret
      inc(fIVCtr.ctr); // replay attack protection
    end
    else
      TAesPrng.Main.FillRandom(fIV); // PRNG from real entropy
    PAesBlock(Output)^ := fIV;
  end;
  MoveFast(Input^, PByteArray(Output)^[ivsize], InputLen);
  FillcharFast(PByteArray(Output)^[ivsize + InputLen], padding, padding);
  Inc(PByte(Output), ivsize);
  Encrypt(Output, Output, InputLen + padding);
  result := true;
end;

function TAesAbstract.DecryptPkcs7Len(var InputLen, ivsize: integer;
  Input: pointer; IVAtBeginning, RaiseESynCryptoOnError: boolean): boolean;
var
  ctr: TAesIVCtr;
begin
  result := true;
  if (InputLen < SizeOf(TAesBlock)) or
     (InputLen and AesBlockMod <> 0) then
    if RaiseESynCryptoOnError then
      raise ESynCrypto.CreateUtf8('%.DecryptPkcs7: Invalid InputLen=%',
        [self, InputLen])
    else
      result := false;
  if result and IVAtBeginning then
  begin
    if (fIVReplayAttackCheck <> repNoCheck) and
       (fIVCtrState <> ctrNotUsed) then
    begin
      if fIVCtr.nonce = 0 then
        SetIVCtr;
      AesIVCtrEncryptDecrypt(Input^, ctr, false);
      if fIVCtrState = ctrUnknown then
        if ctr.magic = fIVCtr.magic then
        begin
          fIVCtr := ctr;
          fIVCtrState := ctrUsed;
          inc(fIVCtr.ctr);
        end
        else if fIVReplayAttackCheck = repMandatory then
          if RaiseESynCryptoOnError then
            raise ESynCrypto.CreateUtf8('%.DecryptPkcs7: IVCTR is not handled ' +
              'on encryption', [self])
          else
            result := false
        else
        begin
          fIVCtrState := ctrNotused;
          if fIVHistoryDec.Depth = 0 then
            SetIVHistory(64); // naive but efficient fallback
        end
      else if IsEqual(TAesBlock(ctr), TAesBlock(fIVCtr)) then
        inc(fIVCtr.ctr)
      else if RaiseESynCryptoOnError then
        raise ESynCrypto.CreateUtf8('%.DecryptPkcs7: wrong IVCTR %/% %/% -> ' +
          'potential replay attack', [self, ctr.magic, fIVCtr.magic, ctr.ctr, fIVCtr.ctr])
      else
        result := false;
    end;
    fIV := PAesBlock(Input)^;
    if result and
       (fIVHistoryDec.Depth > 0) and
       not fIVHistoryDec.Add(fIV) then
      if RaiseESynCryptoOnError then
        raise ESynCrypto.CreateUtf8('%.DecryptPkcs7: duplicated IV=% -> ' +
          'potential replay attack', [self, AesBlockToShortString(fIV)])
      else
        result := false;
    dec(InputLen, SizeOf(TAesBlock));
    ivsize := SizeOf(TAesBlock);
  end
  else
    ivsize := 0;
end;

function TAesAbstract.DecryptPkcs7Buffer(Input: pointer; InputLen: integer;
  IVAtBeginning, RaiseESynCryptoOnError: boolean): RawByteString;
var
  ivsize, padding: integer;
  P: PAnsiChar;
begin
  result := '';
  if not DecryptPkcs7Len(InputLen, ivsize, Input,
      IVAtBeginning, RaiseESynCryptoOnError) then
    exit;
  SetString(result, nil, InputLen);
  P := pointer(result);
  Decrypt(@PByteArray(Input)^[ivsize], P, InputLen);
  padding := ord(P[InputLen - 1]); // result[1..len]
  if padding > SizeOf(TAesBlock) then
    if RaiseESynCryptoOnError then
      raise ESynCrypto.CreateUtf8('%.DecryptPkcs7: Invalid Input', [self])
    else
      result := ''
  else
  begin
    // fast in-place set result length without any memory resize
    dec(InputLen, padding);
    if InputLen = 0 then
      result := ''
    else
    begin
      P[InputLen] := #0; // as SetString - needed if parsed e.g. as text/JSON
      PStrLen(P - _STRLEN)^ := InputLen;
    end;
  end;
end;

function TAesAbstract.DecryptPkcs7(const Input: RawByteString;
  IVAtBeginning, RaiseESynCryptoOnError: boolean): RawByteString;
begin
  result := DecryptPkcs7Buffer(pointer(Input), length(Input),
    IVAtBeginning, RaiseESynCryptoOnError);
end;

function TAesAbstract.DecryptPkcs7(const Input: TBytes;
  IVAtBeginning, RaiseESynCryptoOnError: boolean): TBytes;
var
  len, ivsize, padding: integer;
begin
  result := nil;
  len := length(Input);
  if not DecryptPkcs7Len(len, ivsize, pointer(Input),
      IVAtBeginning, RaiseESynCryptoOnError) then
    exit;
  SetLength(result, len);
  Decrypt(@PByteArray(Input)^[ivsize], pointer(result), len);
  padding := result[len - 1]; // result[0..len-1]
  if padding > SizeOf(TAesBlock) then
    if RaiseESynCryptoOnError then
      raise ESynCrypto.CreateUtf8('%.DecryptPkcs7: Invalid Input', [self])
    else
      result := nil
  else
    SetLength(result, len - padding); // fast in-place resize
end;

function TAesAbstract.MacSetNonce(const aKey: THash256; aAssociated: pointer;
  aAssociatedLen: integer): boolean;
begin
  result := false;
end;

function TAesAbstract.MacGetLast(out aCRC: THash256): boolean;
begin
  result := false;
end;

function TAesAbstract.MACEquals(const aCRC: THash256): boolean;
var
  mac: THash256;
begin
  result := MacGetLast(mac) and IsEqual(mac, aCRC);
end;

function TAesAbstract.MacCheckError(aEncrypted: pointer; Count: cardinal): boolean;
begin
  result := false;
end;

// minimum RecordSave/RecordLoad implementation to avoid mormot.core.data link

function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
begin
  result := 1;
  while Value > $7F do
  begin
    Value := Value shr 7;
    inc(result);
  end;
end;

function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;
begin
  while Value > $7F do
  begin
    Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
  end;
  Dest^ := Value;
  inc(Dest);
  result := Dest;
end;

function FromVarUInt32(var Source: PByte): cardinal;
var
  c, n: cardinal;
begin
  result := 0;
  n := 0;
  repeat
    c := Source^;
    inc(Source);
    if c <= $7f then
      break;
    c := c and $7F;
    result := result or (c shl n);
    inc(n, 7);
  until false;
  result := result or (c shl n);
end;

function TAesAbstract.MacAndCrypt(const Data: RawByteString;
  Encrypt: boolean): RawByteString;
type
  TCryptData = packed record
    nonce, mac: THash256;
    crc: cardinal; // crc32c(nonce+mac) to avoid naive fuzzing
    Data: RawByteString;
  end;
  PCryptData = ^TCryptData;
const
  VERSION = 1;
  CRCSIZ = SizeOf(THash256) * 2;
  SIZ = CRCSIZ + SizeOf(cardinal);
var
  len: cardinal;
  pcd: PCryptData absolute Data;
  rcd: PCryptData absolute result;
  rec: TCryptData;
  P: PByte;
begin
  result := ''; // e.g. MacSetNonce not supported
  try
    if Encrypt then
    begin
      TAesPrng.Main.FillRandom(rec.nonce);
      if not MacSetNonce(rec.nonce) then
        exit;
      rec.Data := EncryptPkcs7(Data, true);
      len := length(rec.Data);
      if not MacGetLast(rec.mac)  then
        exit;
      // inlined RecordSave()
      SetLength(result, SIZ + ToVarUInt32Length(len) + len);
      rcd.nonce := rec.nonce;
      rcd.mac := rec.mac;
      rcd.crc := crc32c(VERSION, @rcd.nonce, CRCSIZ);
      P := @rcd^.Data;
      MoveFast(pointer(rec.Data)^, ToVarUInt32(len, P)^, len);
    end
    else
    begin
      if (length(Data) <= SIZ) or
         (pcd^.crc <> crc32c(VERSION, @pcd.nonce, CRCSIZ)) then
        exit;
      // inlined RecordLoad() for safety
      P := @pcd^.Data;
      len := FromVarUInt32(P);
      if length(Data) - integer(len) <> PAnsiChar(P) - pointer(Data) then
        // to avoid buffer overflow
        exit;
      if MacSetNonce(pcd^.nonce) then
        result := DecryptPkcs7Buffer(P, len, true, false);
      if result <> '' then
        if not MACEquals(pcd^.mac) then
        begin
          FillZero(result);
          result := '';
        end;
    end;
  finally
    FillZero(rec.data);
  end;
end;

class function TAesAbstract.MacEncrypt(const Data: RawByteString;
  const Key: THash256; Encrypt: boolean): RawByteString;
var
  aes: TAesAbstract;
begin
  aes := Create(Key);
  try
    result := aes.MacAndCrypt(Data, Encrypt);
  finally
    aes.Free;
  end;
end;

class function TAesAbstract.MacEncrypt(const Data: RawByteString;
  const Key: THash128; Encrypt: boolean): RawByteString;
var
  aes: TAesAbstract;
begin
  aes := Create(Key);
  try
    result := aes.MacAndCrypt(Data, Encrypt);
  finally
    aes.Free;
  end;
end;

{$ifndef PUREMORMOT2}

class function TAesAbstract.SimpleEncrypt(const Input, Key: RawByteString;
  Encrypt, IVAtBeginning, RaiseESynCryptoOnError: boolean): RawByteString;
var
  instance: TAesAbstract;
begin
  instance := CreateFromSha256(Key){%H-};
  try
    if Encrypt then
      result := instance.EncryptPkcs7(Input, IVAtBeginning)
    else
      result := instance.DecryptPkcs7(Input, IVAtBeginning, RaiseESynCryptoOnError);
  finally
    instance.Free;
  end;
end;

class function TAesAbstract.SimpleEncryptFile(const InputFile, OutputFile: TFileName;
  const Key: RawByteString; Encrypt, IVAtBeginning, RaiseESynCryptoOnError: boolean): boolean;
var
  src, dst: RawByteString;
begin
  result := false;
  src := StringFromFile(InputFile);
  if src <> '' then
  begin
    dst := SimpleEncrypt(src, Key, Encrypt, IVAtBeginning, RaiseESynCryptoOnError){%H-};
    if dst <> '' then
      result := FileFromString(dst, OutputFile);
  end;
end;

{$endif PUREMORMOT2}

class function TAesAbstract.SimpleEncrypt(const Input: RawByteString;
  const Key; KeySize: integer; Encrypt, IVAtBeginning,
  RaiseESynCryptoOnError: boolean): RawByteString;
var
  instance: TAesAbstract;
begin
  instance := Create(Key, KeySize);
  try
    if Encrypt then
      result := instance.EncryptPkcs7(Input, IVAtBeginning)
    else
      result := instance.DecryptPkcs7(Input, IVAtBeginning, RaiseESynCryptoOnError);
  finally
    instance.Free;
  end;
end;

class function TAesAbstract.SimpleEncryptFile(
  const InputFile, Outputfile: TFileName; const Key; KeySize: integer;
  Encrypt, IVAtBeginning, RaiseESynCryptoOnError: boolean): boolean;
var
  src, dst: RawByteString;
begin
  result := false;
  src := StringFromFile(InputFile);
  if src <> '' then
  begin
    dst := SimpleEncrypt(src, Key, KeySize, Encrypt, IVAtBeginning, RaiseESynCryptoOnError);
    if dst <> '' then
      result := FileFromString(dst, Outputfile);
  end;
end;

function TAesAbstract.Clone: TAesAbstract;
begin
  result := TAesAbstractClass(ClassType).Create(fKey, fKeySize);
  result.IVHistoryDepth := IVHistoryDepth;
  result.IVReplayAttackCheck := IVReplayAttackCheck;
end;

function TAesAbstract.CloneEncryptDecrypt: TAesAbstract;
begin
  result := Clone;
end;


{ TAesAbstractSyn }

destructor TAesAbstractSyn.Destroy;
begin
  inherited Destroy;
  fAes.Done; // fill buffer with 0 for safety
  FillZero(fCV); // may contain sensitive data on some modes
end;

function TAesAbstractSyn.Clone: TAesAbstract;
begin
  if fIVHistoryDec.Count <> 0 then
    result := inherited Clone
  else
  begin
    result := NewInstance as TAesAbstractSyn;
    MoveFast(pointer(self)^, pointer(result)^, InstanceSize);
  end;
end;

procedure TAesAbstractSyn.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCV := fIV;
end;

procedure TAesAbstractSyn.DecryptInit;
begin
  if fAes.DecryptInit(fKey, fKeySize) then
    fAesInit := initDecrypt
  else
    raise ESynCrypto.CreateUtf8('%.DecryptInit', [self]);
end;

procedure TAesAbstractSyn.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCV := fIV;
end;

procedure TAesAbstractSyn.EncryptInit;
begin
  if fAes.EncryptInit(fKey, fKeySize) then
    fAesInit := initEncrypt
  else
    raise ESynCrypto.CreateUtf8('%.EncryptInit', [self]);
end;

procedure TAesAbstractSyn.TrailerBytes(count: cardinal);
begin
  if fAesInit <> initEncrypt then
    EncryptInit;
  TAesContext(fAes).DoBlock(fAes, fCV, fCV);
  XorMemory(pointer(fOut), pointer(fIn), @fCV, count);
end;


{ TAesEcb }

procedure TAesEcb.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  inherited; // CV := IV + set fIn,fOut
  if fAesInit <> initDecrypt then
    DecryptInit;
  for i := 1 to Count shr AesBlockShift do
  begin
    TAesContext(fAes).DoBlock(fAes, fIn^, fOut^);
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AesBlockMod;
  if Count <> 0 then
    TrailerBytes(Count);
end;

procedure TAesEcb.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  inherited; // CV := IV + set fIn,fOut
  if fAesInit <> initEncrypt then
    EncryptInit;
  for i := 1 to Count shr AesBlockShift do
  begin
    TAesContext(fAes).DoBlock(fAes, fIn^, fOut^);
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AesBlockMod;
  if Count <> 0 then
    TrailerBytes(Count);
end;


{ TAesCbc }

procedure TAesCbc.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
  tmp: TAesBlock;
begin
  inherited; // CV := IV + set fIn,fOut
  if Count >= SizeOf(TAesBlock) then
  begin
    if fAesInit <> initDecrypt then
      DecryptInit;
    for i := 1 to Count shr AesBlockShift do
    begin
      tmp := fIn^;
      TAesContext(fAes).DoBlock(fAes, fIn^, fOut^);
      XorBlock16(pointer(fOut), pointer(@fCV));
      fCV := tmp;
      inc(fIn);
      inc(fOut);
    end;
  end;
  Count := Count and AesBlockMod;
  if Count <> 0 then
    TrailerBytes(Count);
end;

procedure TAesCbc.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  inherited; // CV := IV + set fIn,fOut
  if fAesInit <> initEncrypt then
    EncryptInit;
  for i := 1 to Count shr AesBlockShift do
  begin
    XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
    TAesContext(fAes).DoBlock(fAes, fOut^, fOut^);
    fCV := fOut^;
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AesBlockMod;
  if Count <> 0 then
    TrailerBytes(Count);
end;


{ TAesAbstractEncryptOnly }

constructor TAesAbstractEncryptOnly.Create(const aKey; aKeySize: cardinal);
begin
  inherited Create(aKey, aKeySize);
  EncryptInit; // as expected by overriden Encrypt/Decrypt methods below
end;

function TAesAbstractEncryptOnly.CloneEncryptDecrypt: TAesAbstract;
begin
  result := self;
end;


{$ifdef FPC} // disable some paranoid warning with FPC about inlined asm blocks
  {$WARN 7121 off : Check size of memory operand }
{$endif FPC}

{ TAesCfb }

procedure TAesCfb.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
  tmp: TAesBlock;
begin
  {$ifdef USEAESNI32}
  if Assigned(TAesContext(fAes.Context).AesNi32) then
    asm
        push    esi
        push    edi
        mov     eax, self
        mov     ecx, Count
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[eax].TAesCfb.fIV
        lea     eax, [eax].TAesCfb.fAes
        push    ecx
        shr     ecx, AesBlockShift
        jz      @z
@s:     call    dword ptr[eax].TAesContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        movaps  xmm1, xmm0
        pxor    xmm0, xmm7
        movaps  xmm7, xmm1              // fCV := fIn
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        add     esi, 16
        add     edi, 16
        dec     ecx
        jnz     @s
@z:     pop     ecx
        and     ecx, 15
        jz      @0
        call    AesNiTrailer
@0:     pop     edi
        pop     esi
        pxor    xmm7, xmm7 // for safety
    end
  else
  {$endif USEAESNI32}
  begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr AesBlockShift do
    begin
      tmp := fIn^;
      TAesContext(fAes).DoBlock(fAes, fCV, fCV);
      XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
      fCV := tmp;
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AesBlockMod;
    if Count <> 0 then
      TrailerBytes(Count);
  end;
end;

procedure TAesCfb.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  {$ifdef USEAESNI32}
  if Assigned(TAesContext(fAes).AesNi32) then
    asm
        push    esi
        push    edi
        mov     eax, self
        mov     ecx, Count
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[eax].TAesCfb.fIV
        lea     eax, [eax].TAesCfb.fAes
        push    ecx
        shr     ecx, AesBlockShift
        jz      @z
@s:     call    dword ptr[eax].TAesContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm7, xmm0
        movups  dqword ptr[edi], xmm7  // fOut := fIn xor fCV
        add     esi, 16
        add     edi, 16
        dec     ecx
        jnz     @s
@z:     pop     ecx
        and     ecx, 15
        jz      @0
        call    AesNiTrailer
@0:     pop     edi
        pop     esi
        pxor    xmm7, xmm7 // for safety
    end
  else
  {$endif USEAESNI32}
  begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr AesBlockShift do
    begin
      TAesContext(fAes).DoBlock(fAes, fCV, fCV);
      XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
      fCV := fOut^;
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AesBlockMod;
    if Count <> 0 then
      TrailerBytes(Count);
  end;
end;


{ TAesAbstractAead }

destructor TAesAbstractAead.Destroy;
begin
  inherited Destroy;
  FillCharFast(fMacKey, SizeOf(fMacKey), 0);
  FillCharFast(fMac, SizeOf(fMac), 0);
end;

function TAesAbstractAead.MacSetNonce(const aKey: THash256; aAssociated: pointer;
  aAssociatedLen: integer): boolean;
var
  rec: THash256Rec absolute aKey;
begin
  // safe seed for plain text crc, before AES encryption
  // from TEcdheProtocol.SetKey, aKey is a public nonce to avoid replay attacks
  fMacKey.plain := rec.Lo;
  XorBlock16(@fMacKey.plain, @rec.Hi);
  // neutral seed for encrypted crc, to check for errors, with no compromission
  if (aAssociated <> nil) and
     (aAssociatedLen > 0) then
    crc128c(aAssociated, aAssociatedLen, fMacKey.encrypted)
  else
    FillcharFast(fMacKey.encrypted, SizeOf(THash128), 255);
  result := true;
end;

function TAesAbstractAead.MacGetLast(out aCRC: THash256): boolean;
var
  rec: THash256Rec absolute aCRC;
begin
  // encrypt the plain text crc, to perform message authentication and integrity
  fAes.Encrypt(fMac.plain, rec{%H-}.Lo);
  // store the encrypted text crc, to check for errors, with no compromission
  rec.Hi := fMac.encrypted;
  result := true;
end;

function TAesAbstractAead.MacCheckError(aEncrypted: pointer; Count: cardinal): boolean;
var
  crc: THash128;
begin
  result := false;
  if (Count < 32) or
     (Count and AesBlockMod <> 0) then
    exit;
  crc := fMacKey.encrypted;
  crcblocks(@crc, aEncrypted, Count shr 4 - 2);
  result := IsEqual(crc, PHash128(@PByteArray(aEncrypted)[Count - SizeOf(crc)])^);
end;


{ TAesCfbCrc }

procedure TAesCfbCrc.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
  tmp: TAesBlock;
begin
  if Count = 0 then
    exit;
  fMac := fMacKey; // reuse the same key until next MacSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAesContext(fAes).AesNi32) and
     (Count and AesBlockMod = 0) then
    asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAesCfbCrc.fIV
@s:     lea     eax, [ebx].TAesCfbCrc.fMac.encrypted
        mov     edx, esi
        call    crcblock // using SSE4.2 or fast tables
        lea     eax, [ebx].TAesCfbCrc.fAes
        call    dword ptr[eax].TAesContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        movaps  xmm1, xmm0
        pxor    xmm0, xmm7
        movaps  xmm7, xmm1              // fCV := fIn
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        lea     eax, [ebx].TAesCfbCrc.fMac.plain
        mov     edx, edi
        call    crcblock
        add     esi, 16
        add     edi, 16
        sub     dword ptr[Count], 16
        ja      @s
@z:     pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
    end
  else
  {$endif USEAESNI32}
  begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr AesBlockShift do
    begin
      tmp := fIn^;
      crcblock(@fMac.encrypted, pointer(fIn)); // fIn may be = fOut
      TAesContext(fAes).DoBlock(fAes, fCV, fCV);
      XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
      fCV := tmp;
      crcblock(@fMac.plain, pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AesBlockMod;
    if Count <> 0 then
    begin
      TrailerBytes(Count);
      with fMac do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^, pointer(fOut), Count);
    end;
  end;
end;

procedure TAesCfbCrc.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  if Count = 0 then
    exit;
  fMac := fMacKey; // reuse the same key until next MacSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAesContext(fAes).AesNi32) and
     (Count and AesBlockMod = 0) then
    asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAesCfbCrc.fIV
@s:     lea     eax, [ebx].TAesCfbCrc.fMac.plain
        mov     edx, esi
        call    crcblock
        lea     eax, [ebx].TAesCfbCrc.fAes
        call    dword ptr[eax].TAesContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm7, xmm0
        movups  dqword ptr[edi], xmm7  // fOut := fIn xor fCV  +  fCV := fOut^
        lea     eax, [ebx].TAesCfbCrc.fMac.encrypted
        mov     edx, edi
        call    crcblock
        add     esi, 16
        add     edi, 16
        sub     dword ptr[Count], 16
        ja      @s
        pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
    end
  else
  {$endif USEAESNI32}
  begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr AesBlockShift do
    begin
      TAesContext(fAes).DoBlock(fAes, fCV, fCV);
      crcblock(@fMac.plain, pointer(fIn)); // fOut may be = fIn
      XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
      fCV := fOut^;
      crcblock(@fMac.encrypted, pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AesBlockMod;
    if Count <> 0 then
    begin
      with fMac do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^, pointer(fIn), Count);
      TrailerBytes(Count);
    end;
  end;
end;


{ TAesOfbCrc }

procedure TAesOfbCrc.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  if Count = 0 then
    exit;
  fMac := fMacKey; // reuse the same key until next MacSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAesContext(fAes).AesNi32) and
     (Count and AesBlockMod = 0) then
    asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAesOfbCrc.fIV
@s:     lea     eax, [ebx].TAesOfbCrc.fMac.encrypted
        mov     edx, esi
        call    crcblock
        lea     eax, [ebx].TAesOfbCrc.fAes
        call    dword ptr[eax].TAesContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm0, xmm7
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        lea     eax, [ebx].TAesOfbCrc.fMac.plain
        mov     edx, edi
        call    crcblock
        add     esi, 16
        add     edi, 16
        sub     dword ptr[Count], 16
        ja      @s
        pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
    end
  else
  {$endif USEAESNI32}
  begin
    inherited Encrypt(BufIn, BufOut, Count); // CV := IV + set fIn,fOut
    for i := 1 to Count shr AesBlockShift do
    begin
      TAesContext(fAes).DoBlock(fAes, fCV, fCV);
      crcblock(@fMac.encrypted, pointer(fIn)); // fOut may be = fIn
      XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
      crcblock(@fMac.plain, pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AesBlockMod;
    if Count <> 0 then
    begin
      TrailerBytes(Count);
      with fMac do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^, pointer(fOut), Count);
    end;
  end;
end;

procedure TAesOfbCrc.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  if Count = 0 then
    exit;
  fMac := fMacKey; // reuse the same key until next MacSetNonce()
  {$ifdef USEAESNI32}
  if Assigned(TAesContext(fAes).AesNi32) and
     (Count and AesBlockMod = 0) then
    asm
        push    ebx
        push    esi
        push    edi
        mov     ebx, self
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[ebx].TAesOfbCrc.fIV
@s:     lea     eax, [ebx].TAesOfbCrc.fMac.plain
        mov     edx, esi
        call    crcblock
        lea     eax, [ebx].TAesOfbCrc.fAes
        call    dword ptr[eax].TAesContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm0, xmm7
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        lea     eax, [ebx].TAesOfbCrc.fMac.encrypted
        mov     edx, edi
        call    crcblock
        add     esi, 16
        add     edi, 16
        sub     dword ptr[Count], 16
        ja      @s
        pop     edi
        pop     esi
        pop     ebx
        pxor    xmm7, xmm7 // for safety
    end
  else
  {$endif USEAESNI32}
  begin
    inherited Encrypt(BufIn, BufOut, Count); // CV := IV + set fIn,fOut
    for i := 1 to Count shr AesBlockShift do
    begin
      TAesContext(fAes).DoBlock(fAes, fCV, fCV);
      crcblock(@fMac.plain, pointer(fIn)); // fOut may be = fIn
      XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
      crcblock(@fMac.encrypted, pointer(fOut));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AesBlockMod;
    if Count <> 0 then
    begin
      with fMac do // includes trailing bytes to the plain crc
        PCardinal(@plain)^ := crc32c(PCardinal(@plain)^, pointer(fIn), Count);
      TrailerBytes(Count);
    end;
  end;
end;


{ TAesOfb }

procedure TAesOfb.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;

procedure TAesOfb.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
begin
  {$ifdef USEAESNI64}
  if Count and AesBlockMod = 0 then
    fAes.DoBlocksOfb(@fCV, BufIn, BufOut, Count shr AesBlockShift)
  else
  {$endif USEAESNI64}
  {$ifdef USEAESNI32}
  if Assigned(TAesContext(fAes).AesNi32) then
    asm
        push    esi
        push    edi
        mov     eax, self
        mov     ecx, Count
        mov     esi, BufIn
        mov     edi, BufOut
        movups  xmm7, dqword ptr[eax].TAesOfb.fIV  // xmm7 = fCV
        lea     eax, [eax].TAesOfb.fAes
        push    ecx
        shr     ecx, AesBlockShift
        jz      @z
@s:     call    dword ptr[eax].TAesContext.AesNi32 // AES.Encrypt(fCV,fCV)
        movups  xmm0, dqword ptr[esi]
        pxor    xmm0, xmm7
        movups  dqword ptr[edi], xmm0  // fOut := fIn xor fCV
        add     esi, 16
        add     edi, 16
        dec     ecx
        jnz     @s
@z:     pop     ecx
        and     ecx, 15
        jz      @0
        call    AesNiTrailer
@0:     pop     edi
        pop     esi
        pxor    xmm7, xmm7 // for safety
    end
  else
  {$endif USEAESNI32}
  begin
    inherited; // CV := IV + set fIn,fOut
    for i := 1 to Count shr AesBlockShift do
    begin
      TAesContext(fAes).DoBlock(fAes, fCV, fCV);
      XorBlock16(pointer(fIn), pointer(fOut), pointer(@fCV));
      inc(fIn);
      inc(fOut);
    end;
    Count := Count and AesBlockMod;
    if Count <> 0 then
      TrailerBytes(Count);
  end;
end;


{ TAesCtr }

constructor TAesCtr.Create(const aKey; aKeySize: cardinal);
begin
  inherited Create(aKey, aKeySize);
  fCTROffset := 7; // counter is in the lower 64 bits, nonce in the upper 64 bits
end;

function TAesCtr.ComposeIV(Nonce, Counter: PAesBlock;
  NonceLen, CounterLen: integer; LSBCounter: boolean): boolean;
begin
  result := (NonceLen + CounterLen = 16) and
            (CounterLen > 0);
  if result then
    if LSBCounter then
    begin
      MoveFast(Nonce[0], fIV[0], NonceLen);
      MoveFast(Counter[0], fIV[NonceLen], CounterLen);
      fCTROffset := 15;
      fCTROffsetMin := 16 - CounterLen;
    end
    else
    begin
      MoveFast(Counter[0], fIV[0], CounterLen);
      MoveFast(Nonce[0], fIV[CounterLen], NonceLen);
      fCTROffset := CounterLen - 1;
      fCTROffsetMin := 0;
    end;
end;

function TAesCtr.ComposeIV(const Nonce, Counter: TByteDynArray;
  LSBCounter: boolean): boolean;
begin
  result := ComposeIV(pointer(Nonce), pointer(Counter),
    length(Nonce), length(Counter), LSBCounter);
end;

procedure TAesCtr.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var
  i: integer;
  offs: PtrInt;
  tmp: TAesBlock;
begin
  inherited; // CV := IV + set fIn,fOut
  for i := 1 to Count shr AesBlockShift do
  begin
    TAesContext(fAes).DoBlock(fAes, fCV, tmp{%H-});
    offs := fCTROffset;
    inc(fCV[offs]);
    if fCV[offs] = 0 then // manual big-endian increment
      repeat
        dec(offs);
        inc(fCV[offs]);
        if (fCV[offs] <> 0) or
           (offs = fCTROffsetMin) then
          break;
      until false;
    XorBlock16(pointer(fIn), pointer(fOut), pointer(@tmp));
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and AesBlockMod;
  if Count <> 0 then
  begin
    TAesContext(fAes).DoBlock(fAes, fCV, tmp);
    XorMemory(pointer(fOut), pointer(fIn), @tmp, Count);
  end;
end;

procedure TAesCtr.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;



{ TAesGcm }

constructor TAesGcm.Create(const aKey; aKeySize: cardinal);
begin
  inherited Create(aKey, aKeySize); // set fKey/fKeySize
  if not fAes.Init(aKey, aKeySize) then
    raise ESynCrypto.CreateUtf8('%.Create(keysize=%) failed', [self, aKeySize]);
end;

function TAesGcm.Clone: TAesAbstract;
begin
  result := NewInstance as TAesGcm;
  result.fKey := fKey;
  result.fKeySize := fKeySize;
  result.fKeySizeBytes := fKeySizeBytes;
  TAesGcm(result).fAes := fAes; // reuse the very same TAesGcmEngine memory
end;

destructor TAesGcm.Destroy;
begin
  inherited Destroy;
  fAes.Done;
  FillZero(fIV);
end;

procedure TAesGcm.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  if fContext <> ctxEncrypt then
    if fContext = ctxNone then
    begin
      fAes.Reset(@fIV, CTR_POS); // caller should have set the IV
      fContext := ctxEncrypt;
    end else
      raise ESynCrypto.CreateUtf8(
        '%.Encrypt after Decrypt', [self]);
  if not fAes.Encrypt(BufIn, BufOut, Count) then
    raise ESynCrypto.CreateUtf8(
      '%.Encrypt called after GCM final state', [self]);
end;

procedure TAesGcm.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  if fContext <> ctxDecrypt then
    if fContext = ctxNone then
    begin
      fAes.Reset(@fIV, CTR_POS);
      fContext := ctxDecrypt;
    end else
      raise ESynCrypto.CreateUtf8(
        '%.Decrypt after Encrypt', [self]);
  if not fAes.Decrypt(BufIn, BufOut, Count) then
    raise ESynCrypto.CreateUtf8(
      '%.Decrypt called after GCM final state', [self]);
end;

function TAesGcm.MacSetNonce(const aKey: THash256; aAssociated: pointer;
  aAssociatedLen: integer): boolean;
begin
  if fContext <> ctxNone then
  begin
    result := false; // should be called before Encrypt/Decrypt
    exit;
  end;
  // aKey is ignored since not used during GMAC computation
  if (aAssociated <> nil) and
     (aAssociatedLen > 0) then
    fAes.Add_AAD(aAssociated, aAssociatedLen);
  result := true;
end;

function TAesGcm.MacGetLast(out aCRC: THash256): boolean;
begin
  if fContext = ctxNone then
  begin
    result := false; // should be called after Encrypt/Decrypt
    exit;
  end;
  fAes.Final(THash256Rec(aCRC).Lo, {forreuse:anddone=}false);
  FillZero(THash256Rec(aCRC).Hi); // upper 128-bit are not used
  fContext := ctxNone; // allow reuse of this fAes instance
  result := true;
end;

function TAesGcm.MacCheckError(aEncrypted: pointer; Count: cardinal): boolean;
begin
  result := true; // AES-GCM requires the IV to be set -> will be checked later
end;


{$ifdef USE_PROV_RSA_AES}

var
  CryptoApiAesProvider: HCRYPTPROV = HCRYPTPROV_NOTTESTED;

procedure EnsureCryptoApiAesProviderAvailable;
begin
  if CryptoApiAesProvider = nil then
    raise ESynCrypto.Create('PROV_RSA_AES provider not installed')
  else if CryptoApiAesProvider = HCRYPTPROV_NOTTESTED then
  begin
    CryptoApiAesProvider := nil;
    if CryptoApi.Available then
    begin
      if not CryptoApi.AcquireContextA(CryptoApiAesProvider, nil, nil, PROV_RSA_AES, 0) then
        if (HRESULT(GetLastError) <> NTE_BAD_KEYSET) or
           not CryptoApi.AcquireContextA(CryptoApiAesProvider, nil, nil,
             PROV_RSA_AES, CRYPT_NEWKEYSET) then
          raise ESynCrypto.CreateLastOSError('in AcquireContext', []);
    end;
  end;
end;


{ TAesAbstractApi }

constructor TAesAbstractApi.Create(const aKey; aKeySize: cardinal);
begin
  EnsureCryptoApiAesProviderAvailable;
  inherited Create(aKey, aKeySize); // check and set fKeySize[Bytes]
  InternalSetMode;
  fKeyHeader.bType := PLAINTEXTKEYBLOB;
  fKeyHeader.bVersion := CUR_BLOB_VERSION;
  case fKeySize of
    128:
      fKeyHeader.aiKeyAlg := CALG_AES_128;
    192:
      fKeyHeader.aiKeyAlg := CALG_AES_192;
    256:
      fKeyHeader.aiKeyAlg := CALG_AES_256;
  end;
  fKeyHeader.dwKeyLength := fKeySizeBytes;
  fKeyHeaderKey := fKey;
end;

destructor TAesAbstractApi.Destroy;
begin
  if fKeyCryptoApi <> nil then
    CryptoApi.DestroyKey(fKeyCryptoApi);
  FillCharFast(fKeyHeaderKey, SizeOf(fKeyHeaderKey), 0);
  inherited;
end;

procedure TAesAbstractApi.EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal;
  DoEncrypt: boolean);
var
  n: cardinal;
begin
  if Count = 0 then
    exit; // nothing to do
  if fKeyCryptoApi <> nil then
  begin
    CryptoApi.DestroyKey(fKeyCryptoApi);
    fKeyCryptoApi := nil;
  end;
  if not CryptoApi.ImportKey(CryptoApiAesProvider, @fKeyHeader,
     SizeOf(fKeyHeader) + fKeySizeBytes, nil, 0, fKeyCryptoApi) then
    raise ESynCrypto.CreateLastOSError('in CryptImportKey for %', [self]);
  if not CryptoApi.SetKeyParam(fKeyCryptoApi, KP_IV, @fIV, 0) then
    raise ESynCrypto.CreateLastOSError('in CryptSetKeyParam(KP_IV) for %', [self]);
  if not CryptoApi.SetKeyParam(fKeyCryptoApi, KP_MODE, @fInternalMode, 0) then
    raise ESynCrypto.CreateLastOSError('in CryptSetKeyParam(KP_MODE,%) for %',
       [fInternalMode, self]);
  if BufOut <> BufIn then
    MoveFast(BufIn^, BufOut^, Count);
  n := Count and not AesBlockMod;
  if DoEncrypt then
  begin
    if not CryptoApi.Encrypt(fKeyCryptoApi, nil, false, 0, BufOut, n, Count) then
      raise ESynCrypto.CreateLastOSError('in Encrypt() for %', [self]);
  end
  else if not CryptoApi.Decrypt(fKeyCryptoApi, nil, false, 0, BufOut, n) then
    raise ESynCrypto.CreateLastOSError('in Decrypt() for %', [self]);
  dec(Count, n);
  if Count > 0 then // remaining bytes will be XORed with the supplied IV
    XorMemory(@PByteArray(BufOut)[n], @PByteArray(BufIn)[n], @fIV, Count);
end;

procedure TAesAbstractApi.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn, BufOut, Count, true);
end;

procedure TAesAbstractApi.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn, BufOut, Count, false);
end;

{ TAesEcbApi }

procedure TAesEcbApi.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_ECB;
end;

{ TAesCbcApi }

procedure TAesCbcApi.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_CBC;
end;

{ TAesCfbApi }

procedure TAesCfbApi.InternalSetMode;
begin
  raise ESynCrypto.CreateUtf8('%: CRYPT_MODE_CFB does not work', [self]);
  fInternalMode := CRYPT_MODE_CFB;
end;

{ TAesOfbApi }

procedure TAesOfbApi.InternalSetMode;
begin
  raise ESynCrypto.CreateUtf8('%: CRYPT_MODE_OFB not implemented by PROV_RSA_AES',
    [self]);
  fInternalMode := CRYPT_MODE_OFB;
end;

{$endif USE_PROV_RSA_AES}


{$ifndef PUREMORMOT2}

var
  /// the encryption key used by CompressShaAes() global function
  // - the key is global to the whole process
  // - use CompressShaAesSetKey() procedure to set this Key from text
  CompressShaAesKey: TSha256Digest;

procedure CompressShaAesSetKey(const Key: RawByteString;
  AesClass: TAesAbstractClass);
begin
  if Key = '' then
    FillZero(CompressShaAesKey)
  else
    Sha256Weak(Key, CompressShaAesKey);
end;

function CompressShaAes(var Data: RawByteString; Compress: boolean): RawUtf8;
begin
  if (Data <> '') and
     (CompressShaAesClass <> nil) then
  try
    with CompressShaAesClass.Create(CompressShaAesKey, 256) do
    try
      if Compress then
      begin
        CompressSynLZ(Data, true);
        Data := EncryptPkcs7(Data, true);
      end
      else
      begin
        Data := DecryptPkcs7(Data, true);
        if CompressSynLZ(Data, false) = '' then
        begin
          result := '';
          exit; // invalid content
        end;
      end;
    finally
      Free;
    end;
  except
    on Exception do
    begin
      // e.g. ESynCrypto in DecryptPkcs7(Data)
      result := '';
      exit; // invalid content
    end;
  end;
  result := 'synshaaes'; // mark success
end;

{$endif PUREMORMOT2}



{ ************* AES-256 Cryptographic Pseudorandom Number Generator (CSPRNG) }

{ TAesLocked }

constructor TAesLocked.Create;
begin
  InitializeCriticalSection(fSafe);
end;

destructor TAesLocked.Destroy;
begin
  inherited Destroy;
  DeleteCriticalSection(fSafe);
  fAes.Done; // fill AES buffer with 0 for safety
end;

procedure TAesLocked.Lock;
begin
  EnterCriticalSection(fSafe);
end;

procedure TAesLocked.UnLock;
begin
  LeaveCriticalSection(fSafe);
end;

{ TAesPrng }

constructor TAesPrng.Create(Pbkdf2Round, ReseedAfterBytes, AesKeySize: integer);
begin
  inherited Create;
  if Pbkdf2Round < 2 then
    Pbkdf2Round := 2;
  fSeedPbkdf2Round := Pbkdf2Round;
  fSeedAfterBytes := ReseedAfterBytes;
  fAesKeySize := AesKeySize;
  Seed;
end;

procedure FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean);
var
  fromos: boolean;
  i: integer;
  {$ifdef LINUX}
  dev: integer;
  {$endif}
  {$ifdef MSWINDOWS}
  prov: HCRYPTPROV;
  {$endif}
  tmp: array[byte] of byte;
begin
  fromos := false;
  {$ifdef LINUX}
  dev := FileOpen('/dev/urandom', fmOpenRead);
  if (dev <= 0) and
     AllowBlocking then
    dev := FileOpen('/dev/random', fmOpenRead);
  if dev > 0 then
  try
    i := Len;
    if i > 32 then
      i := 32; // up to 256 bits - see "man urandom" Usage paragraph
    fromos := (FileRead(dev, Buffer[0], i) = i) and
              (Len <= 32);
  finally
    FileClose(dev);
  end;
  {$endif LINUX}
  {$ifdef MSWINDOWS}
  // warning: on some Windows versions, this could take up to 30 ms!
  if CryptoApi.Available then
    if CryptoApi.AcquireContextA(prov, nil, nil,
      PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
    begin
      fromos := CryptoApi.GenRandom(prov, Len, Buffer);
      CryptoApi.ReleaseContext(prov, 0);
    end;
  {$endif MSWINDOWS}
  if fromos then
    exit;
  i := Len;
  repeat
    // call FillRandom() (i.e. RdRand32 and Lecuyer) as fallback/padding
    mormot.core.base.FillRandom(@tmp, SizeOf(tmp) shr 2);
    if i <= SizeOf(tmp) then
    begin
      XorMemory(@Buffer^[Len - i], @tmp, i);
      break;
    end;
    XorMemoryPtrInt(@Buffer^[Len - i], @tmp, SizeOf(tmp) shr POINTERSHR);
    dec(i, SizeOf(tmp));
  until false;
end;

class function TAesPrng.GetEntropy(Len: integer; SystemOnly: boolean): RawByteString;
var
  data: THash512Rec;
  fromos: RawByteString;
  sha3: TSha3;

  procedure sha3update;
  begin
    QueryPerformanceMicroSeconds(data.d2); // set data.h1 low 64-bit
    XorEntropy(@data.h2); // RdRand32+Rdtsc+Now+Random+CreateGUID
    XorEntropy(@data.h3);
    QueryPerformanceMicroSeconds(data.d3); // set data.h1 high 64-bit
    sha3.Update(@data, SizeOf(data));
  end;

begin
  try
    // retrieve some initial entropy from OS
    SetLength(fromos, Len);
    FillSystemRandom(pointer(fromos), Len, {allowblocking=}SystemOnly);
    if SystemOnly then
    begin
      result := fromos;
      fromos := '';
      exit;
    end;
    // xor some explicit entropy - it won't hurt
    sha3.Init(SHAKE_256); // used in XOF mode for variable-length output
    mormot.core.base.FillRandom(@data, SizeOf(data) shr 2); // gsl_rng_taus2
    sha3update;
    sha3.Update(ExeVersion.Host);
    sha3.Update(ExeVersion.User);
    sha3.Update(ExeVersion.ProgramFullSpec);
    data.h0 := ExeVersion.Hash.b;
    sha3update;
    data.i0 := integer(HInstance); // override data.d0d1/h0
    data.i1 := PtrInt(GetCurrentThreadId);
    data.i2 := PtrInt(MainThreadID);
    data.i3 := integer(UnixMSTimeUtcFast);
    {$ifdef FPC}
    ThreadSwitch; // non deterministic time shift
    {$else}
    SleepHiRes(0);
    {$endif FPC}
    sha3update;
    sha3.Update(OSVersionText);
    sha3.Update(@SystemInfo, SizeOf(SystemInfo));
    result := sha3.Cypher(fromos); // = xor OS entropy using SHA-3 in XOF mode
  finally
    sha3.Done;
    FillZero(fromos);
  end;
end;

procedure TAesPrng.Seed;
var
  alreadyseeding: boolean;
  key: THash512Rec;
  entropy: RawByteString;
begin
  EnterCriticalSection(fSafe);
  alreadyseeding := fSeeding;
  fSeeding := true;
  LeaveCriticalSection(fSafe);
  if not alreadyseeding then
    try
      entropy := GetEntropy(128); // 128 bytes is the HMAC_SHA512 key block size
      PBKDF2_HMAC_SHA512(entropy, ExeVersion.User, fSeedPbkdf2Round, key.b);
      EnterCriticalSection(fSafe);
      try
        fAes.EncryptInit(key.Lo, fAesKeySize);
        crcblocks(@TAesContext(fAes.Context).iv, @key.Hi, 2);
        fBytesSinceSeed := 0;
      finally
        LeaveCriticalSection(fSafe);
      end;
    finally
      FillZero(key.b); // avoid the key appear in clear on stack
      FillZero(entropy);
      fSeeding := false;
    end;
end;

procedure TAesPrng.FillRandom(out Block: TAesBlock);
begin
  if fBytesSinceSeed > fSeedAfterBytes then
    Seed;
  EnterCriticalSection(fSafe);
  with TAesContext(fAes.Context) do
  begin
    DoBlock(rk, iv, Block{%H-});
    inc(iv.L);
    if iv.L = 0 then
      inc(iv.H);
  end;
  inc(fBytesSinceSeed, 16);
  inc(fTotalBytes, 16);
  LeaveCriticalSection(fSafe);
end;

procedure TAesPrng.FillRandom(out Buffer: THash256);
begin
  if fBytesSinceSeed > fSeedAfterBytes then
    Seed;
  EnterCriticalSection(fSafe);
  with TAesContext(fAes.Context) do
  begin
    DoBlock(rk, iv, THash256Rec({%H-}Buffer).Lo);
    inc(iv.L);
    if iv.L = 0 then
      inc(iv.H);
    DoBlock(rk, iv, THash256Rec(Buffer).Hi);
    inc(iv.L);
    if iv.L = 0 then
      inc(iv.H);
  end;
  inc(fBytesSinceSeed, 32);
  inc(fTotalBytes, 32);
  LeaveCriticalSection(fSafe);
end;

procedure TAesPrng.FillRandom(Buffer: pointer; Len: PtrInt);
var
  main, remain: PtrUInt;
  aes: TAesContext; // local copy if Seed is called in another thread
begin
  // prepare the AES rounds
  if Len <= 0 then
    exit;
  main := Len shr AesBlockShift;
  remain := Len and AesBlockMod;
  if remain <> 0 then
    inc(main);
  if fBytesSinceSeed > fSeedAfterBytes then
    Seed;
  EnterCriticalSection(fSafe);
  MoveFast(fAes, aes, SizeOf(aes));
  with TAesContext(fAes.Context).iv do
  begin
    inc(L, main);
    if L < aes.iv.L then
      inc(H);
  end;
  Len := main shl AesBlockShift;
  inc(fBytesSinceSeed, Len);
  inc(fTotalBytes, Len);
  LeaveCriticalSection(fSafe);
  if remain <> 0 then
    dec(main);
  // thread-safe unlocked AES computation
  if main <> 0 then
    repeat
      aes.DoBlock(aes, aes.iv, Buffer^);
      {$ifdef CPU64}
      inc(aes.iv.L);
      if aes.iv.L = 0 then
        inc(aes.iv.H);
      {$else}
      inc(aes.iv.c0);
      if aes.iv.c0 = 0 then
      begin
        inc(aes.iv.c1);
        if aes.iv.c1 = 0 then
        begin
          inc(aes.iv.c2);
          if aes.iv.c2 = 0 then
            inc(aes.iv.c3);
        end;
      end;
      {$endif CPU64}
      inc(PAesBlock(Buffer));
      dec(main)
    until main = 0;
  if remain <> 0 then
  begin
    aes.DoBlock(aes, aes.iv, aes.iv);
    MoveFast(aes.iv, Buffer^, remain);
  end;
end;

function TAesPrng.FillRandom(Len: integer): RawByteString;
begin
  SetString(result, nil, Len);
  FillRandom(pointer(result), Len);
end;

function TAesPrng.FillRandomBytes(Len: integer): TBytes;
begin
  if Len <> length(result) then
    result := nil;
  SetLength(result, Len);
  FillRandom(pointer(result), Len);
end;

function TAesPrng.FillRandomHex(Len: integer): RawUtf8;
var
  bin: pointer;
begin
  FastSetString(result, nil, Len * 2);
  if Len = 0 then
    exit;
  bin := @PByteArray(result)[Len]; // temporary store random bytes at the end
  FillRandom(bin, Len);
  BinToHexLower(bin, pointer(result), Len);
end;

function TAesPrng.Random32: cardinal;
var
  block: THash128Rec;
begin
  FillRandom(block.b);
  result := block.c0; // no need to XOR with c1, c2, c3 for a permutation algo
end;

function TAesPrng.Random32(max: cardinal): cardinal;
var
  block: THash128Rec;
begin
  FillRandom(block.b);
  result := (QWord(block.c0) * max) shr 32; // no need to XOR with block.H
end;

function TAesPrng.Random64: QWord;
var
  block: THash128Rec;
begin
  FillRandom(block.b);
  result := block.L; // no need to XOR with block.H
end;

function TAesPrng.RandomExt: TSynExtended;
var
  block: THash128;
begin
  FillRandom(block);
  result := Hash128ToExt(@block);
end;

function TAesPrng.RandomDouble: double;
var
  block: THash128;
begin
  FillRandom(block);
  result := Hash128ToDouble(@block);
end;

function TAesPrng.RandomPassword(Len: integer): RawUtf8;
const
  CHARS: array[0..127] of AnsiChar =
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' +
    ':bcd.fgh(jklmn)pqrst?vwxyz+BCD%FGH!JKLMN/PQRST@VWX#Z$.:()?%!-+*/@#';
var
  i: integer;
  haspunct: boolean;
  P: PAnsiChar;
begin
  repeat
    result := FillRandom(Len);
    haspunct := false;
    P := pointer(result);
    for i := 1 to Len do
    begin
      P^ := CHARS[ord(P^) mod SizeOf(CHARS)];
      if not haspunct and
         not (ord(P^) in [ord('A')..ord('Z'), ord('a')..ord('z'), ord('0')..ord('9')]) then
        haspunct := true;
      inc(P);
    end;
  until (Len <= 4) or
        (haspunct and
         (LowerCase(result) <> result));
end;

procedure AFDiffusion(buf, rnd: pointer; size: cardinal);
var
  sha: TSha256;
  dig: TSha256Digest;
  last, iv: cardinal;
  i: integer;
begin
  XorMemory(buf, rnd, size);
  sha.Init;
  last := size div SizeOf(dig);
  for i := 0 to last - 1 do
  begin
    iv := bswap32(i); // host byte order independent hash IV (as in TKS1/LUKS)
    sha.Update(@iv, SizeOf(iv));
    sha.Update(buf, SizeOf(dig));
    sha.Final(PSha256Digest(buf)^);
    inc(PByte(buf), SizeOf(dig));
  end;
  dec(size, last * SizeOf(dig));
  if size = 0 then
    exit;
  iv := bswap32(last);
  sha.Update(@iv, SizeOf(iv));
  sha.Update(buf, size);
  sha.Final(dig);
  MoveSmall(@dig, buf, size);
end;

function TAesPrng.AFSplit(const Buffer;
  BufferBytes, StripesCount: integer): RawByteString;
var
  dst: pointer;
  tmp: TByteDynArray;
  i: integer;
begin
  result := '';
  if self <> nil then
    SetLength(result, BufferBytes * (StripesCount + 1));
  if result = '' then
    exit;
  dst := pointer(result);
  SetLength(tmp, BufferBytes);
  for i := 1 to StripesCount do
  begin
    FillRandom(dst, BufferBytes);
    AFDiffusion(pointer(tmp), dst, BufferBytes);
    inc(PByte(dst), BufferBytes);
  end;
  XorMemory(dst, @Buffer, pointer(tmp), BufferBytes);
end;

function TAesPrng.AFSplit(const Buffer: RawByteString;
  StripesCount: integer): RawByteString;
begin
  result := AFSplit(pointer(Buffer)^, length(Buffer), StripesCount);
end;

class function TAesPrng.AFUnsplit(const Split: RawByteString;
  out Buffer; BufferBytes: integer): boolean;
var
  len: cardinal;
  i: integer;
  src: pointer;
  tmp: TByteDynArray;
begin
  len := length(Split);
  result := (len <> 0) and
            (len mod cardinal(BufferBytes) = 0);
  if not result then
    exit;
  src := pointer(Split);
  SetLength(tmp, BufferBytes);
  for i := 2 to len div cardinal(BufferBytes) do
  begin
    AFDiffusion(pointer(tmp), src, BufferBytes);
    inc(PByte(src), BufferBytes);
  end;
  XorMemory(@Buffer, src, pointer(tmp), BufferBytes);
end;

class function TAesPrng.AFUnsplit(const Split: RawByteString;
  StripesCount: integer): RawByteString;
var
  len: cardinal;
begin
  result := '';
  len := length(Split);
  if (len = 0) or
     (len mod cardinal(StripesCount + 1) <> 0) then
    exit;
  len := len div cardinal(StripesCount + 1);
  SetLength(result, len);
  if not AFUnsplit(Split, pointer(result)^, len) then
    result := '';
end;

class procedure TAesPrng.Fill(Buffer: pointer; Len: integer);
begin
  Main.FillRandom(Buffer, Len);
end;

class procedure TAesPrng.Fill(out Block: TAesBlock);
begin
  Main.FillRandom(Block);
end;

class procedure TAesPrng.Fill(out Block: THash256);
begin
  Main.FillRandom(Block);
end;

class function TAesPrng.Fill(Len: integer): RawByteString;
begin
  result := Main.FillRandom(Len);
end;

class function TAesPrng.Bytes(Len: integer): TBytes;
begin
  result := Main.FillRandomBytes(Len);
end;


{ TAesPrngSystem }

constructor TAesPrngSystem.Create;
begin
  inherited Create(0, 0);
end;

procedure TAesPrngSystem.FillRandom(out Block: TAesBlock);
begin
  FillRandom(@Block, SizeOf(Block));
end;

procedure TAesPrngSystem.FillRandom(Buffer: pointer; Len: PtrInt);
begin
  FillSystemRandom(Buffer, Len, false);
end;

procedure TAesPrngSystem.Seed;
begin
  // do nothing
end;


var
  __h: THash256;
  __hmac: THMAC_SHA256; // initialized from CryptProtectDataEntropy salt

// don't use BinToBase64uri() to avoid linking mormot.core.buffers.pas

const
  _b64: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';

procedure RawBase64Uri(rp, sp: PAnsiChar; lendiv, lenmod: integer);
var
  i: integer;
  c: cardinal;
  b64: PAnsiChar;
begin
  b64 := @_b64;
  for i := 1 to lendiv do
  begin
    c := cardinal(sp[0]) shl 16 + cardinal(sp[1]) shl 8 + cardinal(sp[2]);
    rp[0] := b64[(c shr 18) and $3f];
    rp[1] := b64[(c shr 12) and $3f];
    rp[2] := b64[(c shr 6) and $3f];
    rp[3] := b64[c and $3f];
    inc(rp, 4);
    inc(sp, 3);
  end;
  case lenmod of
    1:
      begin
        c := cardinal(sp[0]) shl 16;
        rp[0] := b64[(c shr 18) and $3f];
        rp[1] := b64[(c shr 12) and $3f];
      end;
    2:
      begin
        c := cardinal(sp[0]) shl 16 + cardinal(sp[1]) shl 8;
        rp[0] := b64[(c shr 18) and $3f];
        rp[1] := b64[(c shr 12) and $3f];
        rp[2] := b64[(c shr 6) and $3f];
      end;
  end;
end;

function Base64Uri(P: pointer; len: integer): RawUtf8;
var
  blen, bdiv, bmod: integer;
begin
  bdiv := len div 3;
  bmod := len mod 3;
  blen := bdiv * 4;
  case bmod of
    1:
      inc(blen, 2);
    2:
      inc(blen, 3);
  end;
  FastSetString(result, nil, blen);
  RawBase64Uri(pointer(result), P, bdiv, bmod);
end;

procedure read__h__hmac;
var
  fn: TFileName;
  k256: THash256;
  key, key2, appsec: RawByteString;
begin
  // CryptProtectDataEntropy used as salt
  __hmac.Init(@CryptProtectDataEntropy, 32);
  // CryptProtectDataEntropy derivated for current user -> fn + k256 
  SetString(appsec, PAnsiChar(@CryptProtectDataEntropy), 32);
  PBKDF2_HMAC_SHA256(appsec, ExeVersion.User, 100, k256);
  FillZero(appsec);
  appsec := Base64Uri(@k256, 15); // =BinToBase64Uri()
  fn := FormatString({$ifdef MSWINDOWS}'%_%'{$else}'%.syn-%'{$endif},
    [GetSystemPath(spUserData), appsec]);  // .* files are hidden under Linux
  SetString(appsec, PAnsiChar(@k256[15]), 17); // use remaining bytes as key
  Sha256Weak(appsec, k256); // just a way to reduce to 256-bit
  try
    // extract private user key from local hidden file 
    key := StringFromFile(fn);
    if key <> '' then
    begin
      try
        key2 := TAesCfb.SimpleEncrypt(key, k256, 256, false, true);
      except
        key2 := ''; // handle decryption error
      end;
      FillZero(key);
      {$ifdef MSWINDOWS}
      // somewhat enhance privacy by using Windows API
      key := CryptDataForCurrentUserDPAPI(key2, appsec, false);
      {$else}
      // chmod 400 + AES-CFB + AFUnSplit is enough for privacy on POSIX 
      key := key2;
      {$endif MSWINDOWS}
      if TAesPrng.AFUnsplit(key, __h, SizeOf(__h)) then
        // successfully extracted secret key in __h
        exit;
    end;
    // persist the new private user key into local hidden file
    if FileExists(fn) then
      // allow rewrite of an invalid local file
      FileSetAttributes(fn, {secret=}false);
    TAesPrng.Main.FillRandom(__h);
    key := TAesPrng.Main.AFSplit(__h, SizeOf(__h), 126);
    {$ifdef MSWINDOWS}
    // 4KB local file, DPAPI-cyphered but with no DPAPI BLOB layout
    key2 := CryptDataForCurrentUserDPAPI(key, appsec, true);
    FillZero(key);
    {$else}
    // 4KB local chmod 400 hidden .file in $HOME folder under Linux/POSIX
    key2 := key;
    {$endif MSWINDOWS}
    key := TAesCfb.SimpleEncrypt(key2, k256, 256, true, true);
    if not FileFromString(key, fn) then
      ESynCrypto.CreateUtf8('Unable to write %', [fn]);
    FileSetAttributes(fn, {secret=}true); // chmod 400
  finally
    FillZero(key);
    FillZero(key2);
    FillZero(appsec);
    FillZero(k256);
  end;
end;

function CryptDataForCurrentUser(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;
var
  hmac: THMAC_SHA256;
  secret: THash256;
begin
  result := '';
  if Data = '' then
    exit;
  if IsZero(__h) then
    read__h__hmac;
  try
    hmac := __hmac; // thread-safe reuse of CryptProtectDataEntropy salt
    hmac.Update(AppSecret);
    hmac.Update(__h);
    hmac.Done(secret);
    result := TAesCfbCrc.MacEncrypt(Data, secret, Encrypt);
  finally
    FillZero(secret);
  end;
end;



{ ****************** SHA-2 SHA-3 Hashing }

{ --------- SHA-2 Hashing }

{$ifndef CPUINTEL}

procedure Sha256ExpandMessageBlocks(W, Buf: PIntegerArray);
var
  i: integer;
begin
  // bswap256() instead of "for i := 0 to 15 do W[i]:= bswap32(Buf[i]);"
  bswap256(@Buf[0], @W[0]);
  bswap256(@Buf[8], @W[8]);
  for i := 16 to 63 do
  {$ifdef FPC} // uses faster built-in right rotate intrinsic
    W[i] := (RorDWord(W[i - 2], 17) xor RorDWord(W[i - 2], 19) xor
            (W[i - 2] shr 10)) + W[i - 7] + (RorDWord(W[i - 15], 7) xor
            RorDWord(W[i - 15], 18) xor (W[i - 15] shr 3)) + W[i - 16];
  {$else}
    W[i] := (((W[i - 2] shr 17) or (W[i - 2] shl 15)) xor
            ((W[i - 2] shr 19) or (W[i - 2] shl 13)) xor
            (W[i - 2] shr 10)) + W[i - 7] +
            (((W[i - 15] shr 7) or (W[i - 15] shl 25)) xor
            ((W[i - 15] shr 18) or (W[i - 15] shl 14)) xor
            (W[i - 15] shr 3)) + W[i - 16];
  {$endif FPC}
end;

{$endif CPUINTEL}

// under Win32, with a Core i7 CPU: pure pascal: 152ms - x86: 112ms
// under Win64, with a Core i7 CPU: pure pascal: 202ms - SSE4: 78ms

procedure Sha256CompressPas(var Hash: TSHAHash; Data: pointer);
// Actual hashing function
var
  HW: packed record
    H: TSHAHash;
    W: array[0..63] of cardinal;
  end;
  {$ifndef ASMX86}
  i: PtrInt;
  t1, t2: cardinal;
  {$endif ASMX86}
begin
  // calculate "expanded message blocks"
  Sha256ExpandMessageBlocks(@HW.W, Data);
  // assign old working hash to local variables A..H
  HW.H.A := Hash.A;
  HW.H.B := Hash.B;
  HW.H.C := Hash.C;
  HW.H.D := Hash.D;
  HW.H.E := Hash.E;
  HW.H.F := Hash.F;
  HW.H.G := Hash.G;
  HW.H.H := Hash.H;
  {$ifdef ASMX86}
  // SHA-256 compression function - optimized by A.B. for pipelined CPU
  Sha256Compressx86(@HW);  // fast but PIC-incompatible code
  {$else}
  // SHA-256 compression function
  for i := 0 to high(HW.W) do
  begin
    {$ifdef FPC} // uses built-in right rotate intrinsic
    t1 := HW.H.H +
      (RorDWord(HW.H.E, 6) xor RorDWord(HW.H.E, 11) xor RorDWord(HW.H.E, 25)) +
      ((HW.H.E and HW.H.F) xor (not HW.H.E and HW.H.G)) + K256[i] + HW.W[i];
    t2 := (RorDWord(HW.H.A, 2) xor RorDWord(HW.H.A, 13) xor RorDWord(HW.H.A, 22)) +
          ((HW.H.A and HW.H.B) xor (HW.H.A and HW.H.C) xor (HW.H.B and HW.H.C));
    {$else}
    t1 := HW.H.H + (((HW.H.E shr 6) or (HW.H.E shl 26)) xor
      ((HW.H.E shr 11) or (HW.H.E shl 21)) xor
      ((HW.H.E shr 25) or (HW.H.E shl 7))) +
      ((HW.H.E and HW.H.F) xor (not HW.H.E and HW.H.G)) + K256[i] + HW.W[i];
    t2 := (((HW.H.A shr 2) or (HW.H.A shl 30)) xor
      ((HW.H.A shr 13) or (HW.H.A shl 19)) xor
      ((HW.H.A shr 22) xor (HW.H.A shl 10))) +
      ((HW.H.A and HW.H.B) xor (HW.H.A and HW.H.C) xor (HW.H.B and HW.H.C));
    {$endif FPC}
    HW.H.H := HW.H.G;
    HW.H.G := HW.H.F;
    HW.H.F := HW.H.E;
    HW.H.E := HW.H.D + t1;
    HW.H.D := HW.H.C;
    HW.H.C := HW.H.B;
    HW.H.B := HW.H.A;
    HW.H.A := t1 + t2;
  end;
  {$endif ASMX86}
  // calculate new working hash
  inc(Hash.A, HW.H.A);
  inc(Hash.B, HW.H.B);
  inc(Hash.C, HW.H.C);
  inc(Hash.D, HW.H.D);
  inc(Hash.E, HW.H.E);
  inc(Hash.F, HW.H.F);
  inc(Hash.G, HW.H.G);
  inc(Hash.H, HW.H.H);
end;

procedure RawSha256Compress(var Hash; Data: pointer);
begin
  {$ifdef ASMX64}
  if K256Aligned <> nil then
    // use optimized Intel's sha256_sse4.asm
    sha256_sse4(Data^, Hash, 1)
  else
  {$endif ASMX64}
    Sha256CompressPas(TSHAHash(Hash), Data);
end;


{ TSha256 }

procedure TSha256.Init;
var
  Data: TSHAContext absolute Context;
begin
  Data.Hash.A := $6a09e667;
  Data.Hash.B := $bb67ae85;
  Data.Hash.C := $3c6ef372;
  Data.Hash.D := $a54ff53a;
  Data.Hash.E := $510e527f;
  Data.Hash.F := $9b05688c;
  Data.Hash.G := $1f83d9ab;
  Data.Hash.H := $5be0cd19;
  FillcharFast(Data.MLen, SizeOf(Data) - SizeOf(Data.Hash), 0);
end;

procedure TSha256.Update(Buffer: pointer; Len: integer);
var
  Data: TSHAContext absolute Context;
  aLen: integer;
begin
  if Buffer = nil then
    exit; // avoid GPF
  inc(Data.MLen, QWord(cardinal(Len)) shl 3);
  {$ifdef ASMX64}
  if (K256AlignedStore <> '') and
     (Data.Index = 0) and
     (Len >= 64) then
  begin
    // use optimized Intel's sha256_sse4.asm for whole blocks
    sha256_sse4(Buffer^, Data.Hash, Len shr 6);
    inc(PByte(Buffer), Len);
    Len := Len and 63;
    dec(PByte(Buffer), Len);
  end;
  {$endif ASMX64}
  while Len > 0 do
  begin
    aLen := 64 - Data.Index;
    if aLen <= Len then
    begin
      if Data.Index <> 0 then
      begin
        MoveFast(Buffer^, Data.Buffer[Data.Index], aLen);
        RawSha256Compress(Data.Hash, @Data.Buffer);
        Data.Index := 0;
      end
      else
        RawSha256Compress(Data.Hash, Buffer); // avoid temporary copy
      dec(Len, aLen);
      inc(PtrInt(Buffer), aLen);
    end
    else
    begin
      MoveFast(Buffer^, Data.Buffer[Data.Index], Len);
      inc(Data.Index, Len);
      break;
    end;
  end;
end;

procedure TSha256.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer), length(Buffer));
end;

procedure TSha256.Final(out Digest: TSha256Digest; NoInit: boolean);
// finalize SHA-256 calculation, clear context
var
  Data: TSHAContext absolute Context;
begin
  // append bit '1' after Buffer
  Data.Buffer[Data.Index] := $80;
  FillcharFast(Data.Buffer[Data.Index + 1], 63 - Data.Index, 0);
  // compress if more than 448 bits (no space for 64 bit length storage)
  if Data.Index >= 56 then
  begin
    RawSha256Compress(Data.Hash, @Data.Buffer);
    FillcharFast(Data.Buffer, 56, 0);
  end;
  // write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PInteger(@Data.Buffer[56])^ := bswap32(TQWordRec(Data.MLen).h);
  PInteger(@Data.Buffer[60])^ := bswap32(TQWordRec(Data.MLen).L);
  RawSha256Compress(Data.Hash, @Data.Buffer);
  // Hash -> Digest to little endian format
  bswap256(@Data.Hash, @Digest);
  // clear Data and internally stored Digest
  if not NoInit then
    Init;
end;

function TSha256.Final(NoInit: boolean): TSha256Digest;
begin
  Final(result, NoInit);
end;

procedure TSha256.Full(Buffer: pointer; Len: integer;
  out Digest: TSha256Digest);
begin
  Init;
  Update(Buffer, Len);
  Final(Digest);
end;


function Sha256Digest(Data: pointer; Len: integer): TSha256Digest;
var
  SHA: TSha256;
begin
  SHA.Full(Data, Len, result);
end;

function Sha256Digest(const Data: RawByteString): TSha256Digest;
var
  SHA: TSha256;
begin
  SHA.Full(pointer(Data), Length(Data), result);
end;


{ SHA384/SHA512 hashing kernel }

const
  SHA512K: array[0..79] of QWord = (
    QWord($428a2f98d728ae22), QWord($7137449123ef65cd), QWord($b5c0fbcfec4d3b2f),
    QWord($e9b5dba58189dbbc), QWord($3956c25bf348b538), QWord($59f111f1b605d019),
    QWord($923f82a4af194f9b), QWord($ab1c5ed5da6d8118), QWord($d807aa98a3030242),
    QWord($12835b0145706fbe), QWord($243185be4ee4b28c), QWord($550c7dc3d5ffb4e2),
    QWord($72be5d74f27b896f), QWord($80deb1fe3b1696b1), QWord($9bdc06a725c71235),
    QWord($c19bf174cf692694), QWord($e49b69c19ef14ad2), QWord($efbe4786384f25e3),
    QWord($0fc19dc68b8cd5b5), QWord($240ca1cc77ac9c65), QWord($2de92c6f592b0275),
    QWord($4a7484aa6ea6e483), QWord($5cb0a9dcbd41fbd4), QWord($76f988da831153b5),
    QWord($983e5152ee66dfab), QWord($a831c66d2db43210), QWord($b00327c898fb213f),
    QWord($bf597fc7beef0ee4), QWord($c6e00bf33da88fc2), QWord($d5a79147930aa725),
    QWord($06ca6351e003826f), QWord($142929670a0e6e70), QWord($27b70a8546d22ffc),
    QWord($2e1b21385c26c926), QWord($4d2c6dfc5ac42aed), QWord($53380d139d95b3df),
    QWord($650a73548baf63de), QWord($766a0abb3c77b2a8), QWord($81c2c92e47edaee6),
    QWord($92722c851482353b), QWord($a2bfe8a14cf10364), QWord($a81a664bbc423001),
    QWord($c24b8b70d0f89791), QWord($c76c51a30654be30), QWord($d192e819d6ef5218),
    QWord($d69906245565a910), QWord($f40e35855771202a), QWord($106aa07032bbd1b8),
    QWord($19a4c116b8d2d0c8), QWord($1e376c085141ab53), QWord($2748774cdf8eeb99),
    QWord($34b0bcb5e19b48a8), QWord($391c0cb3c5c95a63), QWord($4ed8aa4ae3418acb),
    QWord($5b9cca4f7763e373), QWord($682e6ff3d6b2b8a3), QWord($748f82ee5defb2fc),
    QWord($78a5636f43172f60), QWord($84c87814a1f0ab72), QWord($8cc702081a6439ec),
    QWord($90befffa23631e28), QWord($a4506cebde82bde9), QWord($bef9a3f7b2c67915),
    QWord($c67178f2e372532b), QWord($ca273eceea26619c), QWord($d186b8c721c0c207),
    QWord($eada7dd6cde0eb1e), QWord($f57d4f7fee6ed178), QWord($06f067aa72176fba),
    QWord($0a637dc5a2c898a6), QWord($113f9804bef90dae), QWord($1b710b35131c471b),
    QWord($28db77f523047d84), QWord($32caab7b40c72493), QWord($3c9ebe0a15c9bebc),
    QWord($431d67c49c100d4c), QWord($4cc5d4becb3e42b6), QWord($597f299cfc657e2a),
    QWord($5fcb6fab3ad6faec), QWord($6c44198c4a475817));

procedure sha512_compresspas(var Hash: TSha512Hash; Data: PQWordArray);
var
  a, b, c, d, e, f, g, h, temp1, temp2: QWord; // to use registers on CPU64
  w: array[0..79] of QWord;
  i: integer;
begin
  bswap64array(Data, @w, 16);
  for i := 16 to 79 do
  {$ifdef FPC} // uses faster built-in right rotate intrinsic
    w[i] := (RorQWord(w[i - 2], 19) xor RorQWord(w[i - 2], 61) xor
      (w[i - 2] shr 6)) + w[i - 7] + (RorQWord(w[i - 15], 1) xor
      RorQWord(w[i - 15], 8) xor (w[i - 15] shr 7)) + w[i - 16];
  {$else}
    w[i] := (((w[i - 2] shr 19) or (w[i - 2] shl 45)) xor
      ((w[i - 2] shr 61) or (w[i - 2] shl 3)) xor (w[i - 2] shr 6)) +
      w[i - 7] + (((w[i - 15] shr 1) or (w[i - 15] shl 63)) xor
      ((w[i - 15] shr 8) or (w[i - 15] shl 56)) xor (w[i - 15] shr 7)) +
      w[i - 16];
  {$endif FPC}
  a := Hash.a;
  b := Hash.b;
  c := Hash.c;
  d := Hash.d;
  e := Hash.e;
  f := Hash.f;
  g := Hash.g;
  h := Hash.h;
  for i := 0 to 79 do
  begin
    {$ifdef FPC}
    temp1 := h + (RorQWord(e, 14) xor RorQWord(e, 18) xor RorQWord(e, 41)) +
      ((e and f) xor (not e and g)) + SHA512K[i] + w[i];
    temp2 := (RorQWord(a, 28) xor RorQWord(a, 34) xor RorQWord(a, 39)) +
      ((a and b) xor (a and c) xor (b and c));
    {$else}
    temp1 := h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor
      ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) +
      SHA512K[i] + w[i];
    temp2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor
      ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
    {$endif FPC}
    h := g;
    g := f;
    f := e;
    e := d + temp1;
    d := c;
    c := b;
    b := a;
    a := temp1 + temp2;
  end;
  inc(Hash.a, a);
  inc(Hash.b, b);
  inc(Hash.c, c);
  inc(Hash.d, d);
  inc(Hash.e, e);
  inc(Hash.f, f);
  inc(Hash.g, g);
  inc(Hash.h, h);
end;

procedure RawSha512Compress(var Hash; Data: pointer);
begin
  {$ifdef SHA512_X86}
  if cfSSSE3 in CpuFeatures then
    sha512_compress(@Hash, Data)
  else
  {$endif SHA512_X86}
  {$ifdef SHA512_X64}
  if cfSSE41 in CpuFeatures then
    sha512_sse4(Data, @Hash, 1)
  else
  {$endif SHA512_X64}
    sha512_compresspas(TSha512Hash(Hash), Data);
end;


{ TSha384 }

procedure TSha384.Init;
begin
  Hash.a := QWord($cbbb9d5dc1059ed8);
  Hash.b := QWord($629a292a367cd507);
  Hash.c := QWord($9159015a3070dd17);
  Hash.d := QWord($152fecd8f70e5939);
  Hash.e := QWord($67332667ffc00b31);
  Hash.f := QWord($8eb44a8768581511);
  Hash.g := QWord($db0c2e0d64f98fa7);
  Hash.h := QWord($47b5481dbefa4fa4);
  MLen := 0;
  Index := 0;
  FillcharFast(Data, SizeOf(Data), 0);
end;

procedure TSha384.Update(Buffer: pointer; Len: integer);
var
  aLen: integer;
begin
  if (Buffer = nil) or
     (Len <= 0) then
    exit; // avoid GPF
  inc(MLen, Len);
  repeat
    aLen := SizeOf(Data) - Index;
    if aLen <= Len then
    begin
      if Index <> 0 then
      begin
        MoveFast(Buffer^, Data[Index], aLen);
        RawSha512Compress(Hash, @Data);
        Index := 0;
      end
      else // avoid temporary copy
        RawSha512Compress(Hash, Buffer);
      dec(Len, aLen);
      inc(PByte(Buffer), aLen);
    end
    else
    begin
      MoveFast(Buffer^, Data[Index], Len);
      inc(Index, Len);
      break;
    end;
  until Len <= 0;
end;

procedure TSha384.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer), length(Buffer));
end;

procedure TSha384.Final(out Digest: TSha384Digest; NoInit: boolean);
begin
  Data[Index] := $80;
  FillcharFast(Data[Index + 1], 127 - Index, 0);
  if Index >= 112 then
  begin
    RawSha512Compress(Hash, @Data);
    FillcharFast(Data, 112, 0);
  end;
  PQWord(@Data[112])^ := bswap64(MLen shr 61);
  PQWord(@Data[120])^ := bswap64(MLen shl 3);
  RawSha512Compress(Hash, @Data);
  bswap64array(@Hash, @Digest, 6);
  if not NoInit then
    Init;
end;

function TSha384.Final(NoInit: boolean): TSha384Digest;
begin
  Final(result, NoInit);
end;

procedure TSha384.Full(Buffer: pointer; Len: integer; out Digest: TSha384Digest);
begin
  Init;
  Update(Buffer, Len); // final bytes
  Final(Digest);
end;


{ TSha512 }

procedure TSha512.Init;
begin
  Hash.a := $6a09e667f3bcc908;
  Hash.b := QWord($bb67ae8584caa73b);
  Hash.c := $3c6ef372fe94f82b;
  Hash.d := QWord($a54ff53a5f1d36f1);
  Hash.e := $510e527fade682d1;
  Hash.f := QWord($9b05688c2b3e6c1f);
  Hash.g := $1f83d9abfb41bd6b;
  Hash.h := $5be0cd19137e2179;
  MLen := 0;
  Index := 0;
  FillcharFast(Data, SizeOf(Data), 0);
end;

procedure TSha512.Update(Buffer: pointer; Len: integer);
var
  aLen: integer;
begin
  if (Buffer = nil) or
     (Len <= 0) then
    exit; // avoid GPF
  inc(MLen, Len);
  repeat
    aLen := SizeOf(Data) - Index;
    if aLen <= Len then
    begin
      if Index <> 0 then
      begin
        MoveFast(Buffer^, Data[Index], aLen);
        RawSha512Compress(Hash, @Data);
        Index := 0;
      end
      else
        // avoid temporary copy
        RawSha512Compress(Hash, Buffer);
      dec(Len, aLen);
      inc(PByte(Buffer), aLen);
    end
    else
    begin
      MoveFast(Buffer^, Data[Index], Len);
      inc(Index, Len);
      break;
    end;
  until Len <= 0;
end;

procedure TSha512.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer), length(Buffer));
end;

procedure TSha512.Final(out Digest: TSha512Digest; NoInit: boolean);
begin
  Data[Index] := $80;
  FillcharFast(Data[Index + 1], 127 - Index, 0);
  if Index >= 112 then
  begin
    RawSha512Compress(Hash, @Data);
    FillcharFast(Data, 112, 0);
  end;
  PQWord(@Data[112])^ := bswap64(MLen shr 61);
  PQWord(@Data[120])^ := bswap64(MLen shl 3);
  RawSha512Compress(Hash, @Data);
  bswap64array(@Hash, @Digest, 8);
  if not NoInit then
    Init;
end;

function TSha512.Final(NoInit: boolean): TSha512Digest;
begin
  Final(result, NoInit);
end;

procedure TSha512.Full(Buffer: pointer; Len: integer; out Digest: TSha512Digest);
begin
  Init;
  Update(Buffer, Len); // final bytes
  Final(Digest);
end;


{ --------- SHA-3 Hashing }

{ SHA-3 / Keccak original code (c) Wolfgang Ehrhardt under zlib license:
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:
 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.
 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
 3. This notice may not be removed or altered from any source distribution. }

const
  cKeccakPermutationSize = 1600;
  cKeccakMaximumRate = 1536;
  cKeccakPermutationSizeInBytes = cKeccakPermutationSize div 8;
  cKeccakMaximumRateInBytes = cKeccakMaximumRate div 8;
  cKeccakNumberOfRounds = 24;
  cRoundConstants: array[0..cKeccakNumberOfRounds - 1] of QWord = (
    QWord($0000000000000001), QWord($0000000000008082), QWord($800000000000808A),
    QWord($8000000080008000), QWord($000000000000808B), QWord($0000000080000001),
    QWord($8000000080008081), QWord($8000000000008009), QWord($000000000000008A),
    QWord($0000000000000088), QWord($0000000080008009), QWord($000000008000000A),
    QWord($000000008000808B), QWord($800000000000008B), QWord($8000000000008089),
    QWord($8000000000008003), QWord($8000000000008002), QWord($8000000000000080),
    QWord($000000000000800A), QWord($800000008000000A), QWord($8000000080008081),
    QWord($8000000000008080), QWord($0000000080000001), QWord($8000000080008008));

{$ifdef ASMINTEL}

procedure KeccakPermutation(A: PQWordArray);
var
  B: array[0..24] of QWord;
  C: array[0..4] of QWord;
  i: integer;
begin
  for i := 0 to 23 do
  begin
    KeccakPermutationKernel(@B, A, @C);
    A[00] := A[00] xor cRoundConstants[i];
  end;
  {$ifdef CPUX86}
  asm
     emms // reset MMX state after use
  end;
  {$endif CPUX86}
end;

{$else}

{$ifdef FPC} // RotL/RolQword are intrinsic functions under FPC :)

function RotL(const x: QWord; c: integer): QWord; inline;
begin
  result := RolQword(x, c);
end;

function RotL1(const x: QWord): QWord; inline;
begin
  result := RolQword(x);
end;

{$else} // Delphi has no ROL operators -> implemented as double shifts

function RotL(const x: QWord; c: integer): QWord;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := (x shl c) or (x shr (64 - c));
end;

function RotL1(var x: QWord): QWord;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := (x shl 1) or (x shr (64 - 1));
end;

{$endif FPC}

procedure KeccakPermutation(A: PQWordArray);
var
  B: array[0..24] of QWord;
  C0, C1, C2, C3, C4, D0, D1, D2, D3, D4: QWord;
  i: integer;
begin
  for i := 0 to 23 do
  begin
    C0 := A[00] xor A[05] xor A[10] xor A[15] xor A[20];
    C1 := A[01] xor A[06] xor A[11] xor A[16] xor A[21];
    C2 := A[02] xor A[07] xor A[12] xor A[17] xor A[22];
    C3 := A[03] xor A[08] xor A[13] xor A[18] xor A[23];
    C4 := A[04] xor A[09] xor A[14] xor A[19] xor A[24];
    D0 := RotL1(C0) xor C3;
    D1 := RotL1(C1) xor C4;
    D2 := RotL1(C2) xor C0;
    D3 := RotL1(C3) xor C1;
    D4 := RotL1(C4) xor C2;
    B[00] := A[00] xor D1;
    B[01] := RotL(A[06] xor D2, 44);
    B[02] := RotL(A[12] xor D3, 43);
    B[03] := RotL(A[18] xor D4, 21);
    B[04] := RotL(A[24] xor D0, 14);
    B[05] := RotL(A[03] xor D4, 28);
    B[06] := RotL(A[09] xor D0, 20);
    B[07] := RotL(A[10] xor D1, 3);
    B[08] := RotL(A[16] xor D2, 45);
    B[09] := RotL(A[22] xor D3, 61);
    B[10] := RotL(A[01] xor D2, 1);
    B[11] := RotL(A[07] xor D3, 6);
    B[12] := RotL(A[13] xor D4, 25);
    B[13] := RotL(A[19] xor D0, 8);
    B[14] := RotL(A[20] xor D1, 18);
    B[15] := RotL(A[04] xor D0, 27);
    B[16] := RotL(A[05] xor D1, 36);
    B[17] := RotL(A[11] xor D2, 10);
    B[18] := RotL(A[17] xor D3, 15);
    B[19] := RotL(A[23] xor D4, 56);
    B[20] := RotL(A[02] xor D3, 62);
    B[21] := RotL(A[08] xor D4, 55);
    B[22] := RotL(A[14] xor D0, 39);
    B[23] := RotL(A[15] xor D1, 41);
    B[24] := RotL(A[21] xor D2, 2);
    A[00] := B[00] xor ((not B[01]) and B[02]);
    A[01] := B[01] xor ((not B[02]) and B[03]);
    A[02] := B[02] xor ((not B[03]) and B[04]);
    A[03] := B[03] xor ((not B[04]) and B[00]);
    A[04] := B[04] xor ((not B[00]) and B[01]);
    A[05] := B[05] xor ((not B[06]) and B[07]);
    A[06] := B[06] xor ((not B[07]) and B[08]);
    A[07] := B[07] xor ((not B[08]) and B[09]);
    A[08] := B[08] xor ((not B[09]) and B[05]);
    A[09] := B[09] xor ((not B[05]) and B[06]);
    A[10] := B[10] xor ((not B[11]) and B[12]);
    A[11] := B[11] xor ((not B[12]) and B[13]);
    A[12] := B[12] xor ((not B[13]) and B[14]);
    A[13] := B[13] xor ((not B[14]) and B[10]);
    A[14] := B[14] xor ((not B[10]) and B[11]);
    A[15] := B[15] xor ((not B[16]) and B[17]);
    A[16] := B[16] xor ((not B[17]) and B[18]);
    A[17] := B[17] xor ((not B[18]) and B[19]);
    A[18] := B[18] xor ((not B[19]) and B[15]);
    A[19] := B[19] xor ((not B[15]) and B[16]);
    A[20] := B[20] xor ((not B[21]) and B[22]);
    A[21] := B[21] xor ((not B[22]) and B[23]);
    A[22] := B[22] xor ((not B[23]) and B[24]);
    A[23] := B[23] xor ((not B[24]) and B[20]);
    A[24] := B[24] xor ((not B[20]) and B[21]);
    A[00] := A[00] xor cRoundConstants[i];
  end;
end;

{$endif ASMINTEL}

{ TSha3Context }

type
  TSha3Context = object
  public
    State: packed array[0..cKeccakPermutationSizeInBytes - 1] of byte;
    DataQueue: packed array[0..cKeccakMaximumRateInBytes - 1] of byte;
    Algo: TSha3Algo;
    Squeezing: boolean;
    Rate: integer;
    Capacity: integer;
    BitsInQueue: integer;
    BitsAvailableForSqueezing: integer;
    procedure Init(aAlgo: TSha3Algo);
    procedure InitSponge(aRate, aCapacity: integer);
    procedure AbsorbQueue;
    procedure Absorb(Data: PByteArray; databitlen: integer);
    procedure AbsorbFinal(Data: PByteArray; databitlen: integer);
    procedure PadAndSwitchToSqueezingPhase;
    procedure Squeeze(output: PByteArray; outputLength: integer);
    procedure FinalBit_LSB(bits: byte; bitlen: integer; hashval: pointer;
      numbits: integer);
  end;
  PSha3Context = ^TSha3Context;

procedure TSha3Context.Init(aAlgo: TSha3Algo);
begin
  case aAlgo of
    SHA3_224:
      InitSponge(1152, 448);
    SHA3_256:
      InitSponge(1088, 512);
    SHA3_384:
      InitSponge(832, 768);
    SHA3_512:
      InitSponge(576, 1024);
    SHAKE_128:
      InitSponge(1344, 256);
    SHAKE_256:
      InitSponge(1088, 512);
  else
    raise ESynCrypto.CreateUtf8('Unexpected TSha3Context.Init(%)', [ord(aAlgo)]);
  end;
  Algo := aAlgo;
end;

procedure TSha3Context.InitSponge(aRate, aCapacity: integer);
begin
  if (aRate + aCapacity <> 1600) or
     (aRate <= 0) or
     (aRate >= 1600) or
     ((aRate and 63) <> 0) then
    raise ESynCrypto.CreateUtf8('Unexpected TSha3Context.Init(%,%)',
      [aRate, aCapacity]);
  FillCharFast(self, SizeOf(self), 0);
  Rate := aRate;
  Capacity := aCapacity;
end;

procedure TSha3Context.AbsorbQueue;
begin
  XorMemoryPtrInt(@State, @DataQueue, Rate shr POINTERSHRBITS);
  KeccakPermutation(@State);
end;

procedure TSha3Context.Absorb(data: PByteArray; databitlen: integer);
var
  i, j, wholeBlocks, partialBlock, partialByte: integer;
  curData: pointer;
begin
  if BitsInQueue and 7 <> 0 then
    raise ESynCrypto.Create('TSha3Context.Absorb: only last can be partial');
  if Squeezing then
    raise ESynCrypto.Create('TSha3Context.Absorb: already squeezed');
  i := 0;
  while i < databitlen do
  begin
    if (BitsInQueue = 0) and
       (databitlen >= Rate) and
       (i <= (databitlen - Rate)) then
    begin
      wholeBlocks := (databitlen - i) div Rate;
      curData := @data^[i shr 3];
      for j := 1 to wholeBlocks do
      begin
        XorMemoryPtrInt(@State, curData, Rate shr POINTERSHRBITS);
        KeccakPermutation(@State);
        inc(PByte(curData), Rate shr 3);
      end;
      inc(i, wholeBlocks * Rate);
    end
    else
    begin
      partialBlock := databitlen - i;
      if partialBlock + BitsInQueue > Rate then
        partialBlock := Rate - BitsInQueue;
      partialByte := partialBlock and 7;
      dec(partialBlock, partialByte);
      MoveFast(data^[i shr 3], DataQueue[BitsInQueue shr 3], partialBlock shr 3);
      inc(BitsInQueue, partialBlock);
      inc(i, partialBlock);
      if BitsInQueue = Rate then
      begin
        AbsorbQueue;
        BitsInQueue := 0;
      end;
      if partialByte > 0 then
      begin
        DataQueue[BitsInQueue shr 3] := data^[i shr 3] and ((1 shl partialByte) - 1);
        inc(BitsInQueue, partialByte);
        inc(i, partialByte);
      end;
    end;
  end;
end;

procedure TSha3Context.AbsorbFinal(data: PByteArray; databitlen: integer);
var
  lastByte: byte;
begin
  if databitlen and 7 = 0 then
    Absorb(data, databitlen)
  else
  begin
    Absorb(data, databitlen - (databitlen and 7));
    // Align the last partial byte to the least significant bits
    lastByte := data^[databitlen shr 3] shr (8 - (databitlen and 7));
    Absorb(@lastByte, databitlen and 7);
  end;
end;

procedure TSha3Context.PadAndSwitchToSqueezingPhase;
var
  i: integer;
begin
  // note: the bits are numbered from 0=LSB to 7=MSB
  if BitsInQueue + 1 = Rate then
  begin
    i := BitsInQueue shr 3;
    DataQueue[i] := DataQueue[i] or (1 shl (BitsInQueue and 7));
    AbsorbQueue;
    FillCharFast(DataQueue, Rate shr 3, 0);
  end
  else
  begin
    i := BitsInQueue shr 3;
    FillCharFast(DataQueue[(BitsInQueue + 7) shr 3],
      Rate shr 3 - (BitsInQueue + 7) shr 3, 0);
    DataQueue[i] := DataQueue[i] or (1 shl (BitsInQueue and 7));
  end;
  i := (Rate - 1) shr 3;
  DataQueue[i] := DataQueue[i] or (1 shl ((Rate - 1) and 7));
  AbsorbQueue;
  MoveFast(State, DataQueue, Rate shr 3);
  BitsAvailableForSqueezing := Rate;
  Squeezing := true;
end;

procedure TSha3Context.Squeeze(output: PByteArray; outputLength: integer);
var
  i: integer;
  partialBlock: integer;
begin
  if not Squeezing then
    PadAndSwitchToSqueezingPhase;
  if outputLength and 7 <> 0 then
    raise ESynCrypto.CreateUtf8('TSha3Context.Squeeze(%?)', [outputLength]);
  i := 0;
  while i < outputLength do
  begin
    if BitsAvailableForSqueezing = 0 then
    begin
      KeccakPermutation(@State);
      MoveFast(State, DataQueue, Rate shr 3);
      BitsAvailableForSqueezing := Rate;
    end;
    partialBlock := BitsAvailableForSqueezing;
    if partialBlock > outputLength - i then
      partialBlock := outputLength - i;
    MoveFast(DataQueue[(Rate - BitsAvailableForSqueezing) shr 3],
      output^[i shr 3], partialBlock shr 3);
    dec(BitsAvailableForSqueezing, partialBlock);
    inc(i, partialBlock);
  end;
end;

procedure TSha3Context.FinalBit_LSB(bits: byte; bitlen: integer;
  hashval: pointer; numbits: integer);
var
  ll: integer;
  lw: cardinal;
begin
  bitlen := bitlen and 7;
  if bitlen = 0 then
    lw := 0
  else
    lw := bits and Pred(cardinal(1) shl bitlen);
  // 'append' (in LSB language) the domain separation bits
  if Algo >= SHAKE_128 then
  begin
    // SHAKE: append four bits 1111
    lw := lw or (cardinal($F) shl bitlen);
    ll := bitlen + 4;
  end
  else
  begin
    // SHA-3: append two bits 01
    lw := lw or (cardinal($2) shl bitlen);
    ll := bitlen + 2;
  end;
  // update state with final bits
  if ll < 9 then
  begin
    // 0..8 bits, one call to update
    lw := lw shl (8 - ll);
    AbsorbFinal(@lw, ll);
    // squeeze the digits from the sponge
    Squeeze(hashval, numbits);
  end
  else
  begin
    // more than 8 bits, first a regular update with low byte
    AbsorbFinal(@lw, 8);
    // finally update remaining last bits
    dec(ll, 8);
    lw := lw shr ll;
    AbsorbFinal(@lw, ll);
    Squeeze(hashval, numbits);
  end;
end;


{ TSha3 }

procedure TSha3.Init(Algo: TSha3Algo);
begin
  PSha3Context(@Context)^.Init(Algo);
end;

function TSha3.Algorithm: TSha3Algo;
begin
  result := PSha3Context(@Context)^.algo;
end;

procedure TSha3.Update(const Buffer: RawByteString);
begin
  if Buffer <> '' then
    PSha3Context(@Context)^.Absorb(pointer(Buffer), Length(Buffer) shl 3);
end;

procedure TSha3.Update(Buffer: pointer; Len: integer);
begin
  if Len > 0 then
    PSha3Context(@Context)^.Absorb(Buffer, Len shl 3);
end;

procedure TSha3.Final(out Digest: THash256; NoInit: boolean);
begin
  Final(@Digest, 256, NoInit);
end;

procedure TSha3.Final(out Digest: THash512; NoInit: boolean);
begin
  Final(@Digest, 512, NoInit);
end;

const
  SHA3_DEF_LEN: array[TSha3Algo] of integer = (
    224, 256, 384, 512, 256, 512);

procedure TSha3.Final(Digest: pointer; DigestBits: integer; NoInit: boolean);
begin
  if DigestBits = 0 then
    DigestBits := SHA3_DEF_LEN[TSha3Context(Context).Algo];
  if TSha3Context(Context).Squeezing then // used as Extendable-Output Function
    PSha3Context(@Context)^.Squeeze(Digest, DigestBits)
  else
    PSha3Context(@Context)^.FinalBit_LSB(0, 0, Digest, DigestBits);
  if not NoInit then
    FillCharFast(Context, SizeOf(Context), 0);
end;

function TSha3.Final256(NoInit: boolean): THash256;
begin
  Final(result, NoInit);
end;

function TSha3.Final512(NoInit: boolean): THash512;
begin
  Final(result, NoInit);
end;

procedure TSha3.Full(Buffer: pointer; Len: integer; out Digest: THash256);
begin
  Full(SHA3_256, Buffer, Len, @Digest, 256);
end;

procedure TSha3.Full(Buffer: pointer; Len: integer; out Digest: THash512);
begin
  Full(SHA3_512, Buffer, Len, @Digest, 512);
end;

procedure TSha3.Full(Algo: TSha3Algo; Buffer: pointer; Len: integer;
  Digest: pointer; DigestBits: integer);
begin
  Init(Algo);
  Update(Buffer, Len);
  Final(Digest, DigestBits);
end;

function TSha3.FullStr(Algo: TSha3Algo; Buffer: pointer;
  Len, DigestBits: integer): RawUtf8;
var
  tmp: RawByteString;
begin
  if DigestBits = 0 then
    DigestBits := SHA3_DEF_LEN[Algo];
  SetLength(tmp, DigestBits shr 3);
  Full(Algo, Buffer, Len, pointer(tmp), DigestBits);
  result := mormot.core.text.BinToHex(tmp);
  FillZero(tmp);
end;

procedure TSha3.Cypher(Key, Source, Dest: pointer; KeyLen, DataLen: integer;
  Algo: TSha3Algo);
begin
  if DataLen <= 0 then
    exit;
  if Source = Dest then
    raise ESynCrypto.Create('Unexpected TSha3.Cypher(Source=Dest)');
  Full(Algo, Key, KeyLen, Dest, DataLen shl 3);
  XorMemory(Dest, Source, DataLen); // just as simple as that!
end;

function TSha3.Cypher(const Key, Source: RawByteString;
  Algo: TSha3Algo): RawByteString;
var
  len: integer;
begin
  len := length(Source);
  SetString(result, nil, len);
  Cypher(pointer(Key), pointer(Source), pointer(result), length(Key), len);
end;

procedure TSha3.InitCypher(Key: pointer; KeyLen: integer; Algo: TSha3Algo);
begin
  Init(Algo);
  Update(Key, KeyLen);
  PSha3Context(@Context)^.FinalBit_LSB(0, 0, nil, 0);
end;

procedure TSha3.InitCypher(const Key: RawByteString; Algo: TSha3Algo);
begin
  InitCypher(pointer(Key), length(Key), Algo);
end;

procedure TSha3.Cypher(Source, Dest: pointer; DataLen: integer);
begin
  Final(Dest, DataLen shl 3, true); // in XOF mode
  XorMemory(Dest, Source, DataLen);
end;

function TSha3.Cypher(const Source: RawByteString): RawByteString;
var
  len: integer;
begin
  len := length(Source);
  SetString(result, nil, len);
  Cypher(pointer(Source), pointer(result), len);
end;

procedure TSha3.Done;
begin
  FillCharFast(self, SizeOf(self), 0);
end;


function ToText(algo: TSha3Algo): PShortString;
begin
  result := GetEnumName(TypeInfo(TSha3Algo), ord(algo));
end;


{ ****************** HMAC Authentication over SHA and CRC32C }

{ THMAC_SHA1 }

procedure THMAC_SHA1.Init(key: pointer; keylen: integer);
var
  i: integer;
  k0, k0xorIpad: THash512Rec;
begin
  FillZero(k0.b);
  if keylen > SizeOf(k0) then
    SHA.Full(key, keylen, k0.b160)
  else
    MoveFast(key^, k0, keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  SHA.Init;
  SHA.Update(@k0xorIpad, SizeOf(k0xorIpad));
  FillZero(k0.b);
  FillZero(k0xorIpad.b);
end;

procedure THMAC_SHA1.Update(msg: pointer; msglen: integer);
begin
  SHA.Update(msg, msglen);
end;

procedure THMAC_SHA1.Done(out result: TSha1Digest; NoInit: boolean);
begin
  SHA.Final(result);
  SHA.Update(@step7data, SizeOf(step7data));
  SHA.Update(@result, SizeOf(result));
  SHA.Final(result, NoInit);
  if not NoInit then
    FillZero(step7data.b);
end;

procedure THMAC_SHA1.Done(out result: RawUtf8; NoInit: boolean);
var
  res: TSha1Digest;
begin
  Done(res, NoInit);
  result := Sha1DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA1.Compute(msg: pointer; msglen: integer;
  out result: TSha1Digest);
var
  temp: THMAC_SHA1;
begin
  temp := self; // thread-safe copy
  temp.Update(msg, msglen);
  temp.Done(result);
end;

procedure HMAC_SHA1(key, msg: pointer; keylen, msglen: integer;
  out result: TSha1Digest);
var
  mac: THMAC_SHA1;
begin
  mac.Init(key, keylen);
  mac.Update(msg, msglen);
  mac.Done(result);
end;

procedure HMAC_SHA1(const key, msg: RawByteString;
  out result: TSha1Digest);
begin
  HMAC_SHA1(pointer(key), pointer(msg), length(key), length(msg), result);
end;

procedure HMAC_SHA1(const key: TSha1Digest; const msg: RawByteString;
  out result: TSha1Digest);
begin
  HMAC_SHA1(@key, pointer(msg), SizeOf(key), length(msg), result);
end;


{ THMAC_SHA256 }

procedure THMAC_SHA256.Init(key: pointer; keylen: integer);
var
  i: integer;
  k0, k0xorIpad: THash512Rec;
begin
  FillZero(k0.b);
  if keylen > SizeOf(k0) then
    SHA.Full(key, keylen, k0.Lo)
  else
    MoveFast(key^, k0, keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  SHA.Init;
  SHA.Update(@k0xorIpad, SizeOf(k0xorIpad));
  FillZero(k0.b);
  FillZero(k0xorIpad.b);
end;

procedure THMAC_SHA256.Update(msg: pointer; msglen: integer);
begin
  SHA.Update(msg, msglen);
end;

procedure THMAC_SHA256.Update(const msg: THash128);
begin
  SHA.Update(@msg, SizeOf(msg));
end;

procedure THMAC_SHA256.Update(const msg: THash256);
begin
  SHA.Update(@msg, SizeOf(msg));
end;

procedure THMAC_SHA256.Update(const msg: RawByteString);
begin
  SHA.Update(pointer(msg), length(msg));
end;

procedure THMAC_SHA256.Done(out result: TSha256Digest; NoInit: boolean);
begin
  SHA.Final(result);
  SHA.Update(@step7data, SizeOf(step7data));
  SHA.Update(@result, SizeOf(result));
  SHA.Final(result, NoInit);
  if not NoInit then
    FillZero(step7data.b);
end;

procedure THMAC_SHA256.Done(out result: RawUtf8; NoInit: boolean);
var
  res: THash256;
begin
  Done(res, NoInit);
  result := Sha256DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA256.Compute(msg: pointer; msglen: integer;
  out result: TSha256Digest);
var
  temp: THMAC_SHA256;
begin
  temp := self; // thread-safe copy
  temp.Update(msg, msglen);
  temp.Done(result);
end;

procedure HMAC_SHA256(key, msg: pointer; keylen, msglen: integer;
  out result: TSha256Digest);
var
  mac: THMAC_SHA256;
begin
  mac.Init(key, keylen);
  mac.Update(msg, msglen);
  mac.Done(result);
end;

procedure HMAC_SHA256(const key, msg: RawByteString;
  out result: TSha256Digest);
begin
  HMAC_SHA256(pointer(key), pointer(msg), length(key), length(msg), result);
end;

procedure HMAC_SHA256(const key: TSha256Digest; const msg: RawByteString;
  out result: TSha256Digest);
begin
  HMAC_SHA256(@key, pointer(msg), SizeOf(key), length(msg), result);
end;


{ THMAC_SHA384 }

procedure THMAC_SHA384.Init(key: pointer; keylen: integer);
var
  i: integer;
  k0, k0xorIpad: array[0..31] of cardinal;
begin
  FillCharFast(k0, SizeOf(k0), 0);
  if keylen > SizeOf(k0) then
    SHA.Full(key, keylen, PSha384Digest(@k0)^)
  else
    MoveFast(key^, k0, keylen);
  for i := 0 to 31 do
    k0xorIpad[i] := k0[i] xor $36363636;
  for i := 0 to 31 do
    step7data[i] := k0[i] xor $5c5c5c5c;
  SHA.Init;
  SHA.Update(@k0xorIpad, SizeOf(k0xorIpad));
  FillCharFast(k0, SizeOf(k0), 0);
  FillCharFast(k0xorIpad, SizeOf(k0xorIpad), 0);
end;

procedure THMAC_SHA384.Update(msg: pointer; msglen: integer);
begin
  SHA.Update(msg, msglen);
end;

procedure THMAC_SHA384.Done(out result: TSha384Digest; NoInit: boolean);
begin
  SHA.Final(result);
  SHA.Update(@step7data, SizeOf(step7data));
  SHA.Update(@result, SizeOf(result));
  SHA.Final(result, NoInit);
  if not NoInit then
    FillCharFast(step7data, SizeOf(step7data), 0);
end;

procedure THMAC_SHA384.Done(out result: RawUtf8; NoInit: boolean);
var
  res: THash384;
begin
  Done(res, NoInit);
  result := Sha384DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA384.Compute(msg: pointer; msglen: integer;
  out result: TSha384Digest);
var
  temp: THMAC_SHA384;
begin
  temp := self; // thread-safe copy
  temp.Update(msg, msglen);
  temp.Done(result);
end;

procedure HMAC_SHA384(key, msg: pointer; keylen, msglen: integer;
  out result: TSha384Digest);
var
  mac: THMAC_SHA384;
begin
  mac.Init(key, keylen);
  mac.Update(msg, msglen);
  mac.Done(result);
end;

procedure HMAC_SHA384(const key, msg: RawByteString;
  out result: TSha384Digest);
begin
  HMAC_SHA384(pointer(key), pointer(msg), length(key), length(msg), result);
end;

procedure HMAC_SHA384(const key: TSha384Digest; const msg: RawByteString;
  out result: TSha384Digest);
begin
  HMAC_SHA384(@key, pointer(msg), SizeOf(key), length(msg), result);
end;


{ THMAC_SHA512 }

procedure THMAC_SHA512.Init(key: pointer; keylen: integer);
var
  i: integer;
  k0, k0xorIpad: array[0..31] of cardinal;
begin
  FillCharFast(k0, SizeOf(k0), 0);
  if keylen > SizeOf(k0) then
    SHA.Full(key, keylen, PSha512Digest(@k0)^)
  else
    MoveFast(key^, k0, keylen);
  for i := 0 to 31 do
    k0xorIpad[i] := k0[i] xor $36363636;
  for i := 0 to 31 do
    step7data[i] := k0[i] xor $5c5c5c5c;
  SHA.Init;
  SHA.Update(@k0xorIpad, SizeOf(k0xorIpad));
  FillCharFast(k0, SizeOf(k0), 0);
  FillCharFast(k0xorIpad, SizeOf(k0xorIpad), 0);
end;

procedure THMAC_SHA512.Update(msg: pointer; msglen: integer);
begin
  SHA.Update(msg, msglen);
end;

procedure THMAC_SHA512.Done(out result: TSha512Digest; NoInit: boolean);
begin
  SHA.Final(result);
  SHA.Update(@step7data, SizeOf(step7data));
  SHA.Update(@result, SizeOf(result));
  SHA.Final(result, NoInit);
  if not NoInit then
    FillCharFast(step7data, SizeOf(step7data), 0);
end;

procedure THMAC_SHA512.Done(out result: RawUtf8; NoInit: boolean);
var
  res: THash512;
begin
  Done(res, NoInit);
  result := Sha512DigestToString(res);
  if not NoInit then
    FillZero(res);
end;

procedure THMAC_SHA512.Compute(msg: pointer; msglen: integer;
  out result: TSha512Digest);
var
  temp: THMAC_SHA512;
begin
  temp := self; // thread-safe copy
  temp.Update(msg, msglen);
  temp.Done(result);
end;

procedure HMAC_SHA512(key, msg: pointer; keylen, msglen: integer;
  out result: TSha512Digest);
var
  mac: THMAC_SHA512;
begin
  mac.Init(key, keylen);
  mac.Update(msg, msglen);
  mac.Done(result);
end;

procedure HMAC_SHA512(const key, msg: RawByteString;
  out result: TSha512Digest);
begin
  HMAC_SHA512(pointer(key), pointer(msg), length(key), length(msg), result);
end;

procedure HMAC_SHA512(const key: TSha512Digest; const msg: RawByteString;
  out result: TSha512Digest);
begin
  HMAC_SHA512(@key, pointer(msg), SizeOf(key), length(msg), result);
end;


{ HMAC_CRC256C }

procedure crc256cmix(h1, h2: cardinal; h: PCardinalArray);
begin
  // see https://goo.gl/Pls5wi
  h^[0] := h1;
  inc(h1, h2);
  h^[1] := h1;
  inc(h1, h2);
  h^[2] := h1;
  inc(h1, h2);
  h^[3] := h1;
  inc(h1, h2);
  h^[4] := h1;
  inc(h1, h2);
  h^[5] := h1;
  inc(h1, h2);
  h^[6] := h1;
  inc(h1, h2);
  h^[7] := h1;
end;

procedure HMAC_CRC256C(key, msg: pointer; keylen, msglen: integer;
  out result: THash256);
var
  i: integer;
  h1, h2: cardinal;
  k0, k0xorIpad, step7data: THash512Rec;
begin
  FillCharFast(k0, SizeOf(k0), 0);
  if keylen > SizeOf(k0) then
    crc256c(key, keylen, k0.Lo)
  else
    MoveFast(key^, k0, keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  h1 := crc32c(crc32c(0, @k0xorIpad, SizeOf(k0xorIpad)), msg, msglen);
  h2 := crc32c(crc32c(h1, @k0xorIpad, SizeOf(k0xorIpad)), msg, msglen);
  crc256cmix(h1, h2, @result);
  h1 := crc32c(crc32c(0, @step7data, SizeOf(step7data)), @result, SizeOf(result));
  h2 := crc32c(crc32c(h1, @step7data, SizeOf(step7data)), @result, SizeOf(result));
  crc256cmix(h1, h2, @result);
  FillCharFast(k0, SizeOf(k0), 0);
  FillCharFast(k0xorIpad, SizeOf(k0), 0);
  FillCharFast(step7data, SizeOf(k0), 0);
end;

procedure HMAC_CRC256C(const key: THash256; const msg: RawByteString;
  out result: THash256);
begin
  HMAC_CRC256C(@key, pointer(msg), SizeOf(key), length(msg), result);
end;

procedure HMAC_CRC256C(const key, msg: RawByteString; out result: THash256);
begin
  HMAC_CRC256C(pointer(key), pointer(msg), length(key), length(msg), result);
end;


{ THMAC_CRC32C }

procedure THMAC_CRC32C.Init(const key: RawByteString);
begin
  Init(pointer(key), length(key));
end;

procedure THMAC_CRC32C.Init(key: pointer; keylen: integer);
var
  i: integer;
  k0, k0xorIpad: THash512Rec;
begin
  FillCharFast(k0, SizeOf(k0), 0);
  if keylen > SizeOf(k0) then
    crc256c(key, keylen, k0.Lo)
  else
    MoveFast(key^, k0, keylen);
  for i := 0 to 15 do
    k0xorIpad.c[i] := k0.c[i] xor $36363636;
  for i := 0 to 15 do
    step7data.c[i] := k0.c[i] xor $5c5c5c5c;
  seed := crc32c(0, @k0xorIpad, SizeOf(k0xorIpad));
  FillCharFast(k0, SizeOf(k0), 0);
  FillCharFast(k0xorIpad, SizeOf(k0xorIpad), 0);
end;

procedure THMAC_CRC32C.Update(msg: pointer; msglen: integer);
begin
  seed := crc32c(seed, msg, msglen);
end;

procedure THMAC_CRC32C.Update(const msg: RawByteString);
begin
  seed := crc32c(seed, pointer(msg), length(msg));
end;

function THMAC_CRC32C.Done(NoInit: boolean): cardinal;
begin
  result := crc32c(seed, @step7data, SizeOf(step7data));
  if not NoInit then
    FillcharFast(self, SizeOf(self), 0);
end;

function THMAC_CRC32C.Compute(msg: pointer; msglen: integer): cardinal;
begin
  result := crc32c(crc32c(seed, msg, msglen), @step7data, SizeOf(step7data));
end;

function HMAC_CRC32C(key, msg: pointer; keylen, msglen: integer): cardinal;
var
  mac: THMAC_CRC32C;
begin
  mac.Init(key, keylen);
  mac.Update(msg, msglen);
  result := mac.Done;
end;

function HMAC_CRC32C(const key: THash256; const msg: RawByteString): cardinal;
begin
  result := HMAC_CRC32C(@key, pointer(msg), SizeOf(key), length(msg));
end;

function HMAC_CRC32C(const key, msg: RawByteString): cardinal;
begin
  result := HMAC_CRC32C(pointer(key), pointer(msg), length(key), length(msg));
end;


{ ****************** PBKDF2 Key Derivation over SHA and CRC32C }

procedure PBKDF2_HMAC_SHA1(const password, salt: RawByteString; count: integer;
  out result: TSha1Digest);
var
  i: integer;
  tmp: TSha1Digest;
  mac: THMAC_SHA1;
  first: THMAC_SHA1;
begin
  HMAC_SHA1(password, salt + #0#0#0#1, result);
  if count < 2 then
    exit;
  tmp := result;
  first.Init(pointer(password), length(password));
  for i := 2 to count do
  begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp, SizeOf(tmp));
    mac.Done(tmp, true);
    XorMemory(@result, @tmp, SizeOf(result));
  end;
  FillcharFast(mac, SizeOf(mac), 0);
  FillcharFast(first, SizeOf(first), 0);
  FillZero(tmp);
end;

procedure PBKDF2_HMAC_SHA256(const password, salt: RawByteString; count: integer;
  out result: TSha256Digest; const saltdefault: RawByteString);
var
  i: integer;
  tmp: TSha256Digest;
  mac: THMAC_SHA256;
  first: THMAC_SHA256;
begin
  if salt = '' then
    HMAC_SHA256(password, saltdefault + #0#0#0#1, result)
  else
    HMAC_SHA256(password, salt + #0#0#0#1, result);
  if count < 2 then
    exit;
  tmp := result;
  first.Init(pointer(password), length(password));
  for i := 2 to count do
  begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp, SizeOf(tmp));
    mac.Done(tmp, true);
    XorMemoryPtrInt(@result, @tmp, SizeOf(result) shr POINTERSHR);
  end;
  FillcharFast(first, SizeOf(first), 0);
  FillcharFast(mac, SizeOf(mac), 0);
  FillZero(tmp);
end;

procedure PBKDF2_HMAC_SHA256(const password, salt: RawByteString; count: integer;
  var result: THash256DynArray; const saltdefault: RawByteString);
var
  n, i: integer;
  iter: RawByteString;
  tmp: TSha256Digest;
  mac: THMAC_SHA256;
  first: THMAC_SHA256;
begin
  first.Init(pointer(password), length(password));
  SetLength(iter, SizeOf(integer));
  for n := 0 to high(result) do
  begin
    PInteger(iter)^ := bswap32(n + 1); // U1 = PRF(Password, Salt || INT_32_BE(i))
    if salt = '' then
      HMAC_SHA256(password, saltdefault + iter, result[n])
    else
      HMAC_SHA256(password, salt + iter, result[n]);
    tmp := result[n];
    for i := 2 to count do
    begin
      mac := first; // re-use the very same SHA context for best performance
      mac.sha.Update(@tmp, SizeOf(tmp));
      mac.Done(tmp, true);
      XorMemoryPtrInt(@result[n], @tmp, SizeOf(result[n]) shr POINTERSHR);
    end;
  end;
  FillZero(tmp);
  FillcharFast(mac, SizeOf(mac), 0);
  FillcharFast(first, SizeOf(first), 0);
end;

procedure PBKDF2_HMAC_SHA384(const password, salt: RawByteString; count: integer;
  out result: TSha384Digest);
var
  i: integer;
  tmp: TSha384Digest;
  mac: THMAC_SHA384;
  first: THMAC_SHA384;
begin
  HMAC_SHA384(password, salt + #0#0#0#1, result);
  if count < 2 then
    exit;
  tmp := result;
  first.Init(pointer(password), length(password));
  for i := 2 to count do
  begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp, SizeOf(tmp));
    mac.Done(tmp, true);
    XorMemoryPtrInt(@result, @tmp, SizeOf(result) shr POINTERSHR);
  end;
  FillcharFast(mac, SizeOf(mac), 0);
  FillcharFast(first, SizeOf(first), 0);
  FillZero(tmp);
end;

procedure PBKDF2_HMAC_SHA512(const password, salt: RawByteString; count: integer;
  out result: TSha512Digest);
var
  i: integer;
  tmp: TSha512Digest;
  mac: THMAC_SHA512;
  first: THMAC_SHA512;
begin
  HMAC_SHA512(password, salt + #0#0#0#1, result);
  if count < 2 then
    exit;
  tmp := result;
  first.Init(pointer(password), length(password));
  for i := 2 to count do
  begin
    mac := first; // re-use the very same SHA context for best performance
    mac.sha.Update(@tmp, SizeOf(tmp));
    mac.Done(tmp, true);
    XorMemoryPtrInt(@result, @tmp, SizeOf(result) shr POINTERSHR);
  end;
  FillcharFast(mac, SizeOf(mac), 0);
  FillcharFast(first, SizeOf(first), 0);
  FillZero(tmp);
end;

procedure PBKDF2_SHA3(algo: TSha3Algo; const password, salt: RawByteString;
  count: integer; result: PByte; resultbytes: integer);
var
  i: integer;
  tmp: RawByteString;
  mac: TSha3;
  first: TSha3;
begin
  if resultbytes <= 0 then
    resultbytes := SHA3_DEF_LEN[algo] shr 3;
  SetLength(tmp, resultbytes);
  first.Init(algo);
  first.Update(password);
  mac := first;
  mac.Update(salt);
  mac.Final(pointer(tmp), resultbytes shl 3, true);
  MoveFast(pointer(tmp)^, result^, resultbytes);
  for i := 2 to count do
  begin
    mac := first;
    mac.Update(pointer(tmp), resultbytes);
    mac.Final(pointer(tmp), resultbytes shl 3, true);
    XorMemory(pointer(result), pointer(tmp), resultbytes);
  end;
  FillcharFast(mac, SizeOf(mac), 0);
  FillcharFast(first, SizeOf(first), 0);
  FillZero(tmp);
end;

procedure PBKDF2_SHA3_Crypt(algo: TSha3Algo; const password, salt: RawByteString;
  count: integer; var data: RawByteString);
var
  key: RawByteString;
  len: integer;
begin
  len := length(data);
  SetLength(key, len);
  PBKDF2_SHA3(algo, password, salt, count, pointer(key), len);
  XorMemory(pointer(data), pointer(key), len);
  FillZero(key);
end;


{ ****************** Deprecated MD5 RC4 SHA-1 Algorithms }

{$ifndef CPUINTEL}

procedure MD5Transform(var buf: TMd5Buf; const in_: TMd5In);
var
  a, b, c, d: cardinal; // unrolled -> compiler will only use cpu registers :)
// the code below is very fast, and can be compared proudly against C or ASM
begin
  a := buf[0];
  b := buf[1];
  c := buf[2];
  d := buf[3];
  {$ifdef FPC} // uses faster built-in right rotate intrinsic
  inc(a, in_[0] + $d76aa478 + (d xor (b and (c xor d))));
  a := RolDWord(a, 7) + b;
  inc(d, in_[1] + $e8c7b756 + (c xor (a and (b xor c))));
  d := RolDWord(d, 12) + a;
  inc(c, in_[2] + $242070db + (b xor (d and (a xor b))));
  c := RolDWord(c, 17) + d;
  inc(b, in_[3] + $c1bdceee + (a xor (c and (d xor a))));
  b := RolDWord(b, 22) + c;
  inc(a, in_[4] + $f57c0faf + (d xor (b and (c xor d))));
  a := RolDWord(a, 7) + b;
  inc(d, in_[5] + $4787c62a + (c xor (a and (b xor c))));
  d := RolDWord(d, 12) + a;
  inc(c, in_[6] + $a8304613 + (b xor (d and (a xor b))));
  c := RolDWord(c, 17) + d;
  inc(b, in_[7] + $fd469501 + (a xor (c and (d xor a))));
  b := RolDWord(b, 22) + c;
  inc(a, in_[8] + $698098d8 + (d xor (b and (c xor d))));
  a := RolDWord(a, 7) + b;
  inc(d, in_[9] + $8b44f7af + (c xor (a and (b xor c))));
  d := RolDWord(d, 12) + a;
  inc(c, in_[10] + $ffff5bb1 + (b xor (d and (a xor b))));
  c := RolDWord(c, 17) + d;
  inc(b, in_[11] + $895cd7be + (a xor (c and (d xor a))));
  b := RolDWord(b, 22) + c;
  inc(a, in_[12] + $6b901122 + (d xor (b and (c xor d))));
  a := RolDWord(a, 7) + b;
  inc(d, in_[13] + $fd987193 + (c xor (a and (b xor c))));
  d := RolDWord(d, 12) + a;
  inc(c, in_[14] + $a679438e + (b xor (d and (a xor b))));
  c := RolDWord(c, 17) + d;
  inc(b, in_[15] + $49b40821 + (a xor (c and (d xor a))));
  b := RolDWord(b, 22) + c;
  inc(a, in_[1] + $f61e2562 + (c xor (d and (b xor c))));
  a := RolDWord(a, 5) + b;
  inc(d, in_[6] + $c040b340 + (b xor (c and (a xor b))));
  d := RolDWord(d, 9) + a;
  inc(c, in_[11] + $265e5a51 + (a xor (b and (d xor a))));
  c := RolDWord(c, 14) + d;
  inc(b, in_[0] + $e9b6c7aa + (d xor (a and (c xor d))));
  b := RolDWord(b, 20) + c;
  inc(a, in_[5] + $d62f105d + (c xor (d and (b xor c))));
  a := RolDWord(a, 5) + b;
  inc(d, in_[10] + $02441453 + (b xor (c and (a xor b))));
  d := RolDWord(d, 9) + a;
  inc(c, in_[15] + $d8a1e681 + (a xor (b and (d xor a))));
  c := RolDWord(c, 14) + d;
  inc(b, in_[4] + $e7d3fbc8 + (d xor (a and (c xor d))));
  b := RolDWord(b, 20) + c;
  inc(a, in_[9] + $21e1cde6 + (c xor (d and (b xor c))));
  a := RolDWord(a, 5) + b;
  inc(d, in_[14] + $c33707d6 + (b xor (c and (a xor b))));
  d := RolDWord(d, 9) + a;
  inc(c, in_[3] + $f4d50d87 + (a xor (b and (d xor a))));
  c := RolDWord(c, 14) + d;
  inc(b, in_[8] + $455a14ed + (d xor (a and (c xor d))));
  b := RolDWord(b, 20) + c;
  inc(a, in_[13] + $a9e3e905 + (c xor (d and (b xor c))));
  a := RolDWord(a, 5) + b;
  inc(d, in_[2] + $fcefa3f8 + (b xor (c and (a xor b))));
  d := RolDWord(d, 9) + a;
  inc(c, in_[7] + $676f02d9 + (a xor (b and (d xor a))));
  c := RolDWord(c, 14) + d;
  inc(b, in_[12] + $8d2a4c8a + (d xor (a and (c xor d))));
  b := RolDWord(b, 20) + c;
  inc(a, in_[5] + $fffa3942 + (b xor c xor d));
  a := RolDWord(a, 4) + b;
  inc(d, in_[8] + $8771f681 + (a xor b xor c));
  d := RolDWord(d, 11) + a;
  inc(c, in_[11] + $6d9d6122 + (d xor a xor b));
  c := RolDWord(c, 16) + d;
  inc(b, in_[14] + $fde5380c + (c xor d xor a));
  b := RolDWord(b, 23) + c;
  inc(a, in_[1] + $a4beea44 + (b xor c xor d));
  a := RolDWord(a, 4) + b;
  inc(d, in_[4] + $4bdecfa9 + (a xor b xor c));
  d := RolDWord(d, 11) + a;
  inc(c, in_[7] + $f6bb4b60 + (d xor a xor b));
  c := RolDWord(c, 16) + d;
  inc(b, in_[10] + $bebfbc70 + (c xor d xor a));
  b := RolDWord(b, 23) + c;
  inc(a, in_[13] + $289b7ec6 + (b xor c xor d));
  a := RolDWord(a, 4) + b;
  inc(d, in_[0] + $eaa127fa + (a xor b xor c));
  d := RolDWord(d, 11) + a;
  inc(c, in_[3] + $d4ef3085 + (d xor a xor b));
  c := RolDWord(c, 16) + d;
  inc(b, in_[6] + $04881d05 + (c xor d xor a));
  b := RolDWord(b, 23) + c;
  inc(a, in_[9] + $d9d4d039 + (b xor c xor d));
  a := RolDWord(a, 4) + b;
  inc(d, in_[12] + $e6db99e5 + (a xor b xor c));
  d := RolDWord(d, 11) + a;
  inc(c, in_[15] + $1fa27cf8 + (d xor a xor b));
  c := RolDWord(c, 16) + d;
  inc(b, in_[2] + $c4ac5665 + (c xor d xor a));
  b := RolDWord(b, 23) + c;
  inc(a, in_[0] + $f4292244 + (c xor (b or (not d))));
  a := RolDWord(a, 6) + b;
  inc(d, in_[7] + $432aff97 + (b xor (a or (not c))));
  d := RolDWord(d, 10) + a;
  inc(c, in_[14] + $ab9423a7 + (a xor (d or (not b))));
  c := RolDWord(c, 15) + d;
  inc(b, in_[5] + $fc93a039 + (d xor (c or (not a))));
  b := RolDWord(b, 21) + c;
  inc(a, in_[12] + $655b59c3 + (c xor (b or (not d))));
  a := RolDWord(a, 6) + b;
  inc(d, in_[3] + $8f0ccc92 + (b xor (a or (not c))));
  d := RolDWord(d, 10) + a;
  inc(c, in_[10] + $ffeff47d + (a xor (d or (not b))));
  c := RolDWord(c, 15) + d;
  inc(b, in_[1] + $85845dd1 + (d xor (c or (not a))));
  b := RolDWord(b, 21) + c;
  inc(a, in_[8] + $6fa87e4f + (c xor (b or (not d))));
  a := RolDWord(a, 6) + b;
  inc(d, in_[15] + $fe2ce6e0 + (b xor (a or (not c))));
  d := RolDWord(d, 10) + a;
  inc(c, in_[6] + $a3014314 + (a xor (d or (not b))));
  c := RolDWord(c, 15) + d;
  inc(b, in_[13] + $4e0811a1 + (d xor (c or (not a))));
  b := RolDWord(b, 21) + c;
  inc(a, in_[4] + $f7537e82 + (c xor (b or (not d))));
  a := RolDWord(a, 6) + b;
  inc(d, in_[11] + $bd3af235 + (b xor (a or (not c))));
  d := RolDWord(d, 10) + a;
  inc(c, in_[2] + $2ad7d2bb + (a xor (d or (not b))));
  c := RolDWord(c, 15) + d;
  inc(b, in_[9] + $eb86d391 + (d xor (c or (not a))));
  b := RolDWord(b, 21) + c;
  {$else}
  inc(a, in_[0] + $d76aa478 + (d xor (b and (c xor d))));
  a := ((a shl 7) or (a shr (32 - 7))) + b;
  inc(d, in_[1] + $e8c7b756 + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr (32 - 12))) + a;
  inc(c, in_[2] + $242070db + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr (32 - 17))) + d;
  inc(b, in_[3] + $c1bdceee + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr (32 - 22))) + c;
  inc(a, in_[4] + $f57c0faf + (d xor (b and (c xor d))));
  a := ((a shl 7) or (a shr (32 - 7))) + b;
  inc(d, in_[5] + $4787c62a + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr (32 - 12))) + a;
  inc(c, in_[6] + $a8304613 + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr (32 - 17))) + d;
  inc(b, in_[7] + $fd469501 + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr (32 - 22))) + c;
  inc(a, in_[8] + $698098d8 + (d xor (b and (c xor d))));
  a := ((a shl 7) or (a shr (32 - 7))) + b;
  inc(d, in_[9] + $8b44f7af + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr (32 - 12))) + a;
  inc(c, in_[10] + $ffff5bb1 + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr (32 - 17))) + d;
  inc(b, in_[11] + $895cd7be + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr (32 - 22))) + c;
  inc(a, in_[12] + $6b901122 + (d xor (b and (c xor d))));
  a := ((a shl 7) or (a shr (32 - 7))) + b;
  inc(d, in_[13] + $fd987193 + (c xor (a and (b xor c))));
  d := ((d shl 12) or (d shr (32 - 12))) + a;
  inc(c, in_[14] + $a679438e + (b xor (d and (a xor b))));
  c := ((c shl 17) or (c shr (32 - 17))) + d;
  inc(b, in_[15] + $49b40821 + (a xor (c and (d xor a))));
  b := ((b shl 22) or (b shr (32 - 22))) + c;
  inc(a, in_[1] + $f61e2562 + (c xor (d and (b xor c))));
  a := ((a shl 5) or (a shr (32 - 5))) + b;
  inc(d, in_[6] + $c040b340 + (b xor (c and (a xor b))));
  d := ((d shl 9) or (d shr (32 - 9))) + a;
  inc(c, in_[11] + $265e5a51 + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr (32 - 14))) + d;
  inc(b, in_[0] + $e9b6c7aa + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr (32 - 20))) + c;
  inc(a, in_[5] + $d62f105d + (c xor (d and (b xor c))));
  a := ((a shl 5) or (a shr (32 - 5))) + b;
  inc(d, in_[10] + $02441453 + (b xor (c and (a xor b))));
  d := ((d shl 9) or (d shr (32 - 9))) + a;
  inc(c, in_[15] + $d8a1e681 + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr (32 - 14))) + d;
  inc(b, in_[4] + $e7d3fbc8 + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr (32 - 20))) + c;
  inc(a, in_[9] + $21e1cde6 + (c xor (d and (b xor c))));
  a := ((a shl 5) or (a shr (32 - 5))) + b;
  inc(d, in_[14] + $c33707d6 + (b xor (c and (a xor b))));
  d := ((d shl 9) or (d shr (32 - 9))) + a;
  inc(c, in_[3] + $f4d50d87 + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr (32 - 14))) + d;
  inc(b, in_[8] + $455a14ed + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr (32 - 20))) + c;
  inc(a, in_[13] + $a9e3e905 + (c xor (d and (b xor c))));
  a := ((a shl 5) or (a shr (32 - 5))) + b;
  inc(d, in_[2] + $fcefa3f8 + (b xor (c and (a xor b))));
  d := ((d shl 9) or (d shr (32 - 9))) + a;
  inc(c, in_[7] + $676f02d9 + (a xor (b and (d xor a))));
  c := ((c shl 14) or (c shr (32 - 14))) + d;
  inc(b, in_[12] + $8d2a4c8a + (d xor (a and (c xor d))));
  b := ((b shl 20) or (b shr (32 - 20))) + c;
  inc(a, in_[5] + $fffa3942 + (b xor c xor d));
  a := ((a shl 4) or (a shr (32 - 4))) + b;
  inc(d, in_[8] + $8771f681 + (a xor b xor c));
  d := ((d shl 11) or (d shr (32 - 11))) + a;
  inc(c, in_[11] + $6d9d6122 + (d xor a xor b));
  c := ((c shl 16) or (c shr (32 - 16))) + d;
  inc(b, in_[14] + $fde5380c + (c xor d xor a));
  b := ((b shl 23) or (b shr (32 - 23))) + c;
  inc(a, in_[1] + $a4beea44 + (b xor c xor d));
  a := ((a shl 4) or (a shr (32 - 4))) + b;
  inc(d, in_[4] + $4bdecfa9 + (a xor b xor c));
  d := ((d shl 11) or (d shr (32 - 11))) + a;
  inc(c, in_[7] + $f6bb4b60 + (d xor a xor b));
  c := ((c shl 16) or (c shr (32 - 16))) + d;
  inc(b, in_[10] + $bebfbc70 + (c xor d xor a));
  b := ((b shl 23) or (b shr (32 - 23))) + c;
  inc(a, in_[13] + $289b7ec6 + (b xor c xor d));
  a := ((a shl 4) or (a shr (32 - 4))) + b;
  inc(d, in_[0] + $eaa127fa + (a xor b xor c));
  d := ((d shl 11) or (d shr (32 - 11))) + a;
  inc(c, in_[3] + $d4ef3085 + (d xor a xor b));
  c := ((c shl 16) or (c shr (32 - 16))) + d;
  inc(b, in_[6] + $04881d05 + (c xor d xor a));
  b := ((b shl 23) or (b shr (32 - 23))) + c;
  inc(a, in_[9] + $d9d4d039 + (b xor c xor d));
  a := ((a shl 4) or (a shr (32 - 4))) + b;
  inc(d, in_[12] + $e6db99e5 + (a xor b xor c));
  d := ((d shl 11) or (d shr (32 - 11))) + a;
  inc(c, in_[15] + $1fa27cf8 + (d xor a xor b));
  c := ((c shl 16) or (c shr (32 - 16))) + d;
  inc(b, in_[2] + $c4ac5665 + (c xor d xor a));
  b := ((b shl 23) or (b shr (32 - 23))) + c;
  inc(a, in_[0] + $f4292244 + (c xor (b or (not d))));
  a := ((a shl 6) or (a shr (32 - 6))) + b;
  inc(d, in_[7] + $432aff97 + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr (32 - 10))) + a;
  inc(c, in_[14] + $ab9423a7 + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr (32 - 15))) + d;
  inc(b, in_[5] + $fc93a039 + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr (32 - 21))) + c;
  inc(a, in_[12] + $655b59c3 + (c xor (b or (not d))));
  a := ((a shl 6) or (a shr (32 - 6))) + b;
  inc(d, in_[3] + $8f0ccc92 + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr (32 - 10))) + a;
  inc(c, in_[10] + $ffeff47d + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr (32 - 15))) + d;
  inc(b, in_[1] + $85845dd1 + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr (32 - 21))) + c;
  inc(a, in_[8] + $6fa87e4f + (c xor (b or (not d))));
  a := ((a shl 6) or (a shr (32 - 6))) + b;
  inc(d, in_[15] + $fe2ce6e0 + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr (32 - 10))) + a;
  inc(c, in_[6] + $a3014314 + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr (32 - 15))) + d;
  inc(b, in_[13] + $4e0811a1 + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr (32 - 21))) + c;
  inc(a, in_[4] + $f7537e82 + (c xor (b or (not d))));
  a := ((a shl 6) or (a shr (32 - 6))) + b;
  inc(d, in_[11] + $bd3af235 + (b xor (a or (not c))));
  d := ((d shl 10) or (d shr (32 - 10))) + a;
  inc(c, in_[2] + $2ad7d2bb + (a xor (d or (not b))));
  c := ((c shl 15) or (c shr (32 - 15))) + d;
  inc(b, in_[9] + $eb86d391 + (d xor (c or (not a))));
  b := ((b shl 21) or (b shr (32 - 21))) + c;
  {$endif FPC}
  inc(buf[0], a);
  inc(buf[1], b);
  inc(buf[2], c);
  inc(buf[3], d);
end;

{$endif CPUINTEL}


{ TMd5 }

function TMd5.Final: TMd5Digest;
begin
  Finalize;
  result := TMd5Digest(buf);
end;

procedure TMd5.Final(out result: TMd5Digest);
begin
  Finalize;
  result := TMd5Digest(buf);
end;

procedure TMd5.Finalize;
var
  count: integer;
  p: PByte;
begin
  count := bytes[0] and $3f;  // number of pending bytes in
  p := @in_;
  Inc(p, count);
  // Set the first char of padding to 0x80.  There is always room
  p^ := $80;
  Inc(p);
  // Bytes of padding needed to make 56 bytes (-8..55)
  count := 55 - count;
  if count < 0 then
  begin
    //  Padding forces an extra block
    FillcharFast(p^, count + 8, 0);
    MD5Transform(buf, in_);
    p := @in_;
    count := 56;
  end;
  FillcharFast(p^, count, 0);
  // Append length in bits and transform
  in_[14] := bytes[0] shl 3;
  in_[15] := (bytes[1] shl 3) or (bytes[0] shr 29);
  MD5Transform(buf, in_);
end;

procedure TMd5.Full(Buffer: pointer; Len: integer; out Digest: TMd5Digest);
begin
  buf[0] := $67452301;
  buf[1] := $efcdab89;
  buf[2] := $98badcfe;
  buf[3] := $10325476;
  bytes[0] := Len;
  while Len >= SizeOf(TMd5In) do
  begin
    MD5Transform(buf, PMd5In(Buffer)^);
    inc(PMd5In(Buffer));
    dec(Len, SizeOf(TMd5In));
  end;
  MoveFast(Buffer^, in_, Len);
  Buffer := PAnsiChar(@in_) + Len;
  PByte(Buffer)^ := $80;
  inc(PByte(Buffer));
  Len := 55 - Len;
  if Len >= 0 then
    FillcharFast(Buffer^, Len, 0)
  else
  begin
    FillcharFast(Buffer^, Len + 8, 0);
    MD5Transform(buf, in_);
    FillcharFast(in_, 56, 0);
  end;
  Len := bytes[0];
  in_[14] := Len shl 3;
  in_[15] := Len shr 29;
  MD5Transform(buf, in_);
  Digest := TMd5Digest(buf);
end;

procedure TMd5.Init;
begin
  buf[0] := $67452301;
  buf[1] := $efcdab89;
  buf[2] := $98badcfe;
  buf[3] := $10325476;
  bytes[0] := 0;
  bytes[1] := 0;
end;

procedure TMd5.Update(const buffer; len: cardinal);
var
  p: ^TMd5In;
  t: cardinal;
  i: integer;
begin
  p := @buffer;
  // Update byte count
  t := bytes[0];
  Inc(bytes[0], len);
  if bytes[0] < t then
    // 64 bit carry from low to high
    Inc(bytes[1]);
  t := 64 - (t and 63);  // space available in in_ (at least 1)
  if t > len then
  begin
    MoveFast(p^, PAnsiChar(@in_)[64 - t], len);
    exit;
  end;
  // First chunk is an odd size
  MoveFast(p^, PAnsiChar(@in_)[64 - t], t);
  MD5Transform(buf, in_);
  inc(PByte(p), t);
  dec(len, t);
  // Process data in 64-byte chunks
  for i := 1 to len shr 6 do
  begin
    MD5Transform(buf, p^);
    inc(p);
  end;
  // Handle any remaining bytes of data.
  MoveFast(p^, in_, len and 63);
end;

procedure TMd5.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer)^, length(Buffer));
end;


function Md5Buf(const Buffer; Len: cardinal): TMd5Digest;
var
  MD5: TMd5;
begin
  MD5.Full(@Buffer, Len, result);
end;

function HTDigest(const user, realm, pass: RawByteString): RawUtf8;
// apache-compatible: agent007:download area:8364d0044ef57b3defcfa141e8f77b65
//    hash=`echo -n "$user:$realm:$pass" | md5sum | cut -b -32`
//    echo "$user:$realm:$hash"
var
  tmp: RawByteString;
begin
  tmp := user + ':' + realm + ':';
  result := tmp + Md5(tmp + pass);
end;


{ TSha1 }

procedure sha1Compress(var Hash: TSHAHash; Data: PByteArray);
var
  A, B, C, D, E, X: cardinal;
  W: array[0..79] of cardinal;
  i: integer;
begin
  // init W[] + A..E
  bswap256(@Data[0], @W[0]);
  bswap256(@Data[32], @W[8]);
  for i := 16 to 79 do
  begin
    X := W[i - 3] xor W[i - 8] xor W[i - 14] xor W[i - 16];
    W[i] := (X shl 1) or (X shr 31);
  end;
  A := Hash.A;
  B := Hash.B;
  C := Hash.C;
  D := Hash.D;
  E := Hash.E;
  // unrolled loop -> all is computed in cpu registers
  // note: FPC detects "(A shl 5) or (A shr 27)" pattern into "RolDWord(A,5)" :)
  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[0]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[1]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[2]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[3]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[4]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[5]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[6]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[7]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[8]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[9]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[10]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[11]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[12]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[13]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[14]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[15]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[16]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[17]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[18]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[19]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[20]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[21]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[22]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[23]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[24]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[25]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[26]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[27]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[28]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[29]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[30]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[31]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[32]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[33]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[34]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[35]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[36]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[37]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[38]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[39]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[40]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[41]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[42]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[43]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[44]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[45]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[46]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[47]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[48]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[49]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[50]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[51]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[52]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[53]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[54]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[55]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[56]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[57]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[58]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[59]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[60]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[61]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[62]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[63]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[64]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[65]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[66]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[67]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[68]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[69]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[70]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[71]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[72]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[73]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[74]);
  C := (C shl 30) or (C shr 2);
  Inc(E, ((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[75]);
  B := (B shl 30) or (B shr 2);
  Inc(D, ((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[76]);
  A := (A shl 30) or (A shr 2);
  Inc(C, ((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[77]);
  E := (E shl 30) or (E shr 2);
  Inc(B, ((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[78]);
  D := (D shl 30) or (D shr 2);
  Inc(A, ((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[79]);
  C := (C shl 30) or (C shr 2);
  // Calculate new working hash
  inc(Hash.A, A);
  inc(Hash.B, B);
  inc(Hash.C, C);
  inc(Hash.D, D);
  inc(Hash.E, E);
end;

procedure TSha1.Final(out Digest: TSha1Digest; NoInit: boolean);
var
  Data: TSHAContext absolute Context;
begin
  // 1. append bit '1' after Buffer
  Data.Buffer[Data.Index] := $80;
  FillcharFast(Data.Buffer[Data.Index + 1], 63 - Data.Index, 0);
  // 2. Compress if more than 448 bits, (no room for 64 bit length
  if Data.Index >= 56 then
  begin
    sha1Compress(Data.Hash, @Data.Buffer);
    FillcharFast(Data.Buffer, 56, 0);
  end;
  // Write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PCardinal(@Data.Buffer[56])^ := bswap32(TQWordRec(Data.MLen).h);
  PCardinal(@Data.Buffer[60])^ := bswap32(TQWordRec(Data.MLen).L);
  sha1Compress(Data.Hash, @Data.Buffer);
  // Hash -> Digest to little endian format
  bswap160(@Data.Hash, @Digest);
  // Clear Data
  if not NoInit then
    Init;
end;

function TSha1.Final(NoInit: boolean): TSha1Digest;
begin
  Final(result, NoInit);
end;

procedure TSha1.Full(Buffer: pointer; Len: integer; out Digest: TSha1Digest);
begin
  Init;
  Update(Buffer, Len);
  Final(Digest);
end;

procedure TSha1.Init;
var
  Data: TSHAContext absolute Context;
begin
  Data.Hash.A := $67452301;
  Data.Hash.B := $EFCDAB89;
  Data.Hash.C := $98BADCFE;
  Data.Hash.D := $10325476;
  Data.Hash.E := $C3D2E1F0;
  FillcharFast(Data.MLen, SizeOf(Data) - SizeOf(Data.Hash), 0);
end;

procedure TSha1.Update(Buffer: pointer; Len: integer);
var
  Data: TSHAContext absolute Context;
  aLen: integer;
begin
  if Buffer = nil then
    exit; // avoid GPF
  inc(Data.MLen, QWord(cardinal(Len)) shl 3);
  while Len > 0 do
  begin
    aLen := SizeOf(Data.Buffer) - Data.Index;
    if aLen <= Len then
    begin
      if Data.Index <> 0 then
      begin
        MoveFast(Buffer^, Data.Buffer[Data.Index], aLen);
        sha1Compress(Data.Hash, @Data.Buffer);
        Data.Index := 0;
      end
      else
        // direct compression to avoid uneeded temporary copy
        sha1Compress(Data.Hash, Buffer);
      dec(Len, aLen);
      inc(PByte(Buffer), aLen);
    end
    else
    begin
      MoveFast(Buffer^, Data.Buffer[Data.Index], Len);
      inc(Data.Index, Len);
      break;
    end;
  end;
end;

procedure TSha1.Update(const Buffer: RawByteString);
begin
  Update(pointer(Buffer), length(Buffer));
end;


{ TRC4 }

procedure TRC4.Init(const aKey; aKeyLen: integer);
var
  i, k: integer;
  j, tmp: PtrInt;
begin
  if aKeyLen <= 0 then
    raise ESynCrypto.CreateUtf8('TRC4.Init(invalid aKeyLen=%)', [aKeyLen]);
  dec(aKeyLen);
  for i := 0 to high(state) do
    state[i] := i;
  j := 0;
  k := 0;
  for i := 0 to high(state) do
  begin
    j := (j + state[i] + TByteArray(aKey)[k]) and $ff;
    tmp := state[i];
    state[i] := state[j];
    state[j] := tmp;
    if k >= aKeyLen then // avoid slow mod operation within loop
      k := 0
    else
      inc(k);
  end;
  currI := 0;
  currJ := 0;
end;

procedure TRC4.InitSha3(const aKey; aKeyLen: integer);
var
  sha: TSha3;
  dig: array[byte] of byte; // max RC4 state size is 256 bytes
begin
  sha.Full(SHAKE_128, @aKey, aKeyLen, @dig, SizeOf(dig) shl 3); // XOF mode
  Init(dig, SizeOf(dig));
  FillCharFast(dig, SizeOf(dig), 0);
  Drop(3072); // 3KB warmup
end;

procedure TRC4.EncryptBuffer(BufIn, BufOut: PByte; Count: cardinal);
var
  i, j, ki, kj: PtrInt;
  by4: array[0..3] of byte;
begin
  i := currI;
  j := currJ;
  while Count > 3 do
  begin
    dec(Count, 4);
    i := (i + 1) and $ff;
    ki := State[i];
    j := (j + ki) and $ff;
    kj := (ki + State[j]) and $ff;
    State[i] := State[j];
    i := (i + 1) and $ff;
    State[j] := ki;
    ki := State[i];
    by4[0] := State[kj];
    j := (j + ki) and $ff;
    kj := (ki + State[j]) and $ff;
    State[i] := State[j];
    i := (i + 1) and $ff;
    State[j] := ki;
    by4[1] := State[kj];
    ki := State[i];
    j := (j + ki) and $ff;
    kj := (ki + State[j]) and $ff;
    State[i] := State[j];
    i := (i + 1) and $ff;
    State[j] := ki;
    by4[2] := State[kj];
    ki := State[i];
    j := (j + ki) and $ff;
    kj := (ki + State[j]) and $ff;
    State[i] := State[j];
    State[j] := ki;
    by4[3] := State[kj];
    PCardinal(BufOut)^ := PCardinal(BufIn)^ xor cardinal(by4);
    inc(BufIn, 4);
    inc(BufOut, 4);
  end;
  while Count > 0 do
  begin
    dec(Count);
    i := (i + 1) and $ff;
    ki := State[i];
    j := (j + ki) and $ff;
    kj := (ki + State[j]) and $ff;
    State[i] := State[j];
    State[j] := ki;
    BufOut^ := BufIn^ xor State[kj];
    inc(BufIn);
    inc(BufOut);
  end;
  currI := i;
  currJ := j;
end;

procedure TRC4.Encrypt(const BufIn; var BufOut; Count: cardinal);
begin
  EncryptBuffer(@BufIn, @BufOut, Count);
end;

procedure TRC4.Drop(Count: cardinal);
var
  i, j, ki: PtrInt;
begin
  i := currI;
  j := currJ;
  while Count > 0 do
  begin
    dec(Count);
    i := (i + 1) and $ff;
    ki := state[i];
    j := (j + ki) and $ff;
    state[i] := state[j];
    state[j] := ki;
  end;
  currI := i;
  currJ := j;
end;


procedure RawMd5Compress(var Hash; Data: pointer);
begin
  MD5Transform(TMd5Buf(Hash), PMd5In(Data)^);
end;

procedure RawSha1Compress(var Hash; Data: pointer);
begin
  sha1Compress(TSHAHash(Hash), Data);
end;



{ ****************** Digest/Hash to Hexadecimal Text Conversion }

procedure AesBlockToShortString(const block: TAesBlock; out result: short32);
begin
  result[0] := #32;
  mormot.core.text.BinToHex(@block, @result[1], 16);
end;

function AesBlockToShortString(const block: TAesBlock): short32;
begin
  AesBlockToShortString(block, result);
end;

function AesBlockToString(const block: TAesBlock): RawUtf8;
begin
  FastSetString(result, nil, 32);
  mormot.core.text.BinToHex(@block, pointer(result), 16);
end;

function Md5(const s: RawByteString): RawUtf8;
var
  MD5: TMd5;
  D: TMd5Digest;
begin
  MD5.Full(pointer(s), Length(s), D);
  result := Md5DigestToString(D);
  FillZero(D);
end;

function Md5DigestToString(const D: TMd5Digest): RawUtf8;
begin
  BinToHexLower(@D, SizeOf(D), result);
end;

function Md5StringToDigest(const Source: RawUtf8; out Dest: TMd5Digest): boolean;
begin
  result := mormot.core.text.HexToBin(pointer(Source), @Dest, SizeOf(Dest));
end;


function Sha1(const s: RawByteString): RawUtf8;
var
  SHA: TSha1;
  Digest: TSha1Digest;
begin
  SHA.Full(pointer(s), length(s), Digest);
  result := Sha1DigestToString(Digest);
  FillZero(Digest);
end;

function Sha1DigestToString(const D: TSha1Digest): RawUtf8;
begin
  BinToHexLower(@D, SizeOf(D), result);
end;

function Sha1StringToDigest(const Source: RawUtf8; out Dest: TSha1Digest): boolean;
begin
  result := mormot.core.text.HexToBin(pointer(Source), @Dest, SizeOf(Dest));
end;


function Sha256(const s: RawByteString): RawUtf8;
var
  SHA: TSha256;
  Digest: TSha256Digest;
begin
  SHA.Full(pointer(s), length(s), Digest);
  result := Sha256DigestToString(Digest);
  FillZero(Digest);
end;

function Sha256(Data: pointer; Len: integer): RawUtf8;
var
  SHA: TSha256;
  Digest: TSha256Digest;
begin
  SHA.Full(Data, Len, Digest);
  result := Sha256DigestToString(Digest);
  FillZero(Digest);
end;

function Sha256DigestToString(const D: TSha256Digest): RawUtf8;
begin
  BinToHexLower(@D, SizeOf(D), result);
end;

function Sha256StringToDigest(const Source: RawUtf8; out Dest: TSha256Digest): boolean;
begin
  result := mormot.core.text.HexToBin(pointer(Source), @Dest, SizeOf(Dest));
end;


function Sha384DigestToString(const D: TSha384Digest): RawUtf8;
begin
  BinToHexLower(@D, SizeOf(D), result);
end;

function Sha384(const s: RawByteString): RawUtf8;
var
  SHA: TSha384;
  Digest: TSha384Digest;
begin
  SHA.Full(pointer(s), length(s), Digest);
  result := Sha384DigestToString(Digest);
  FillZero(Digest);
end;


function Sha512DigestToString(const D: TSha512Digest): RawUtf8;
begin
  BinToHexLower(@D, SizeOf(D), result);
end;

function Sha512(const s: RawByteString): RawUtf8;
var
  SHA: TSha512;
  Digest: TSha512Digest;
begin
  SHA.Full(pointer(s), length(s), Digest);
  result := Sha512DigestToString(Digest);
  FillZero(Digest);
end;

function Sha3(Algo: TSha3Algo; const s: RawByteString; DigestBits: integer): RawUtf8;
begin
  result := Sha3(Algo, pointer(s), length(s), DigestBits);
end;

function Sha3(Algo: TSha3Algo; Buffer: pointer; Len, DigestBits: integer): RawUtf8;
var
  instance: TSha3;
begin
  result := instance.FullStr(Algo, Buffer, Len, DigestBits);
end;




{ ****************** Deprecated Weak AES/SHA Process }

{$ifndef PUREMORMOT2}

procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: integer;
  Encrypt: boolean);
begin
  AES(Key, KeySize, buffer, buffer, Len, Encrypt);
end;

procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean);
var
  n: integer;
  pIn, pOut: PAesBlock;
  Crypt: TAes;
begin
  if (bIn = nil) or
     (bOut = nil) then
    exit;
  // 1. Init
  n := Len shr AesBlockShift;
  if n < 0 then
    exit
  else if n > 0 then
    if (KeySize > 4) and
       not Crypt.DoInit(Key, KeySize, Encrypt) then
      // if error in KeySize, use default fast XorOffset()
      KeySize := 4;
  if KeySize = 0 then
  begin
    // KeySize=0 -> no encryption -> direct copy
    MoveFast(bIn^, bOut^, Len);
    exit;
  end;
  if n < 1 then
  begin
    // too small for AES -> XorOffset() remaining 0..15 bytes
    MoveFast(bIn^, bOut^, Len);
    XorOffset(bOut, 0, Len);
    exit;
  end;
  // 2. All full blocks, with AES
  Crypt.DoBlocks(bIn, bOut, pIn, pOut, n, Encrypt);
  // 3. Last block, just XORed from Key
  // assert(KeySize div 8 >= AesBlockSize);
  n := cardinal(Len) and AesBlockMod;
  MoveFast(pIn^, pOut^, n); // pIn=pOut is tested in MoveFast()
  XorOffset(pointer(pOut), Len - n, n);
  Crypt.Done;
end;

const
  TmpSize = 65536;
  // Tmp buffer for AESFull -> Xor Crypt is TmpSize-dependent / use XorBlock()
  TmpSizeBlock = TmpSize shr AesBlockShift;

type
  TTmp = array[0..TmpSizeBlock - 1] of TAesBlock;

function AES(const Key; KeySize: cardinal; const s: RawByteString;
  Encrypt: boolean): RawByteString;
begin
  SetString(result, nil, length(s));
  if s <> '' then
    AES(Key, KeySize, pointer(s), pointer(result), length(s), Encrypt);
end;

function AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal;
  Stream: TStream; Encrypt: boolean): boolean;
var
  buf: pointer;
  last, b, n, i: cardinal;
  Crypt: TAes;
begin
  result := false;
  if buffer = nil then
    exit;
  if (KeySize > 4) and
     not Crypt.DoInit(Key, KeySize, Encrypt) then
    // if error in KeySize, use default fast XorOffset()
    KeySize := 4;
  if KeySize = 0 then
  begin
    // no Crypt -> direct write to dest Stream
    Stream.WriteBuffer(buffer^, Len);
    result := true;
    exit;
  end;
  getmem(buf, TmpSize);
  try
    last := Len and AesBlockMod;
    n := Len - last;
    i := 0;
    while n > 0 do
    begin
      // crypt/uncrypt all AesBlocks
      if n > TmpSize then
        b := TmpSize
      else
        b := n;
      assert(b and AesBlockMod = 0);
      if KeySize = 4 then
      begin
        MoveFast(buffer^, buf^, b);
        XorOffset(pointer(buf), i, b);
        inc(i, b);
      end
      else
        Crypt.DoBlocks(buffer, buf, b shr AesBlockShift, Encrypt);
      Stream.WriteBuffer(buf^, b);
      inc(PByte(buffer), b);
      dec(n, b);
    end;
    assert((KeySize > 4) or (i = Len - last));
    if last > 0 then
    begin
      // crypt/uncrypt (Xor) last 0..15 bytes
      MoveFast(buffer^, buf^, last);
      XorOffset(pointer(buf), Len - last, last);
      Stream.WriteBuffer(buf^, last);
    end;
    result := true;
  finally
    freemem(buf);
  end;
end;

function KeyFrom(const Key; KeySize: cardinal): cardinal;
begin
  case KeySize div 8 of
    0:
      result := 0;
    1:
      result := PByte(@Key)^;
    2, 3:
      result := PWord(@Key)^;
  else
    result := PInteger(@Key)^;
  end;
end;

function TAesFullHeader.Calc(const Key; KeySize: cardinal): cardinal;
begin
  result := Adler32Asm(KeySize, @Key, KeySize shr 3) xor
    Te0[OriginalLen and $FF] xor Te1[SourceLen and $FF] xor
    Td0[SomeSalt and $7FF];
end;

function TAesFull.EncodeDecode(const Key; KeySize, inLen: cardinal;
  Encrypt: boolean; inStream, outStream: TStream; bIn, bOut: pointer;
  OriginalLen: cardinal): integer;
var
  Tmp: ^TTmp;
  pIn, pOut: PAesBlock;
  Crypt: TAes;
  nBlock, XorCod: cardinal;

  procedure Read(Tmp: pointer; ByteCount: cardinal);
  begin
    if pIn = nil then
      inStream.Read(Tmp^, ByteCount)
    else
    begin
      MoveFast(pIn^, Tmp^, ByteCount);
      inc(PByte(pIn), ByteCount);
    end;
  end;

  procedure Write(Tmp: pointer; ByteCount: cardinal);
  begin
    if pOut = nil then
      outStream.WriteBuffer(Tmp^, ByteCount)
    else
    begin
      MoveFast(Tmp^, pOut^, ByteCount);
      inc(PByte(pOut), ByteCount);
    end;
  end;

  procedure SetOutLen(Len: cardinal);
  var
    P: cardinal;
  begin
    result := Len; // global EncodeDecode() result
    if outStream <> nil then
    begin
      if outStream.InheritsFrom(TMemoryStream) then
        with TMemoryStream(outStream) do
        begin
          P := Seek(0, soFromCurrent);
          size := P + Len; // auto-reserve space (no Realloc:)
          Seek(P + Len, soBeginning);
          bOut := PAnsiChar(Memory) + P;
          pOut := bOut;
          outStream := nil; //  OutStream is slower and use no thread
        end;
    end
    else if bOut = nil then
    begin
      outStreamCreated := TMemoryStream.Create;
      outStreamCreated.Size := Len; // auto-reserve space (no Realloc:)
      bOut := outStreamCreated.Memory;
      pOut := bOut; // OutStream is slower and use no thread
    end;
    if KeySize = 0 then
      exit; // no Tmp to be allocated on direct copy
    if (KeySize = 32) or
       (inStream <> nil) or
       (outStream <> nil) then
      New(Tmp);
  end;

  procedure DoBlock(BlockCount: integer);
  begin
    if BlockCount = 0 then
      exit;
    read(Tmp, BlockCount shl AesBlockShift);
    Crypt.DoBlocks(PAesBLock(Tmp), PAesBLock(Tmp), BlockCount, Encrypt);
    Write(Tmp, BlockCount shl AesBlockShift);
  end;

var
  n, LastLen: cardinal;
  i: integer;
  Last: TAesBlock;
begin
  result := 0; // makes FixInsight happy
  Tmp := nil;
  outStreamCreated := nil;
  Head.SourceLen := inLen;
  nBlock := Head.SourceLen shr AesBlockShift;
  if Encrypt and
     (OriginalLen <> 0) then
    Head.OriginalLen := OriginalLen
  else
    Head.OriginalLen := inLen;
  KeySize := KeySize div 8;
  if not (KeySize in [0, 4, 16, 24, 32]) then
    KeySize := 0
  else  // valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    KeySize := KeySize * 8;
  XorCod := inLen;
  if (inStream <> nil) and
     inStream.InheritsFrom(TMemoryStream) then
  begin
    bIn := TMemoryStream(inStream).Memory;
    inStream := nil;
  end;
  pIn := bIn;
  pOut := bOut;
  if (KeySize >= 128) and
     not Crypt.DoInit(Key, KeySize, Encrypt) then
    KeySize := 32;
  if KeySize = 32 then
    XorCod := KeyFrom(Key, KeySize) xor XorCod
  else if (KeySize = 0) and
          (inStream = nil) then
  begin
    SetOutLen(inLen);
    Write(bIn, inLen);  // no encryption -> direct write
    exit;
  end;
  try
    // 0. handle KeySize = 0:direct copy and 32:XorBlock
    if KeySize < 128 then
    begin
      SetOutLen(inLen);
      assert(Tmp <> nil);
      LastLen := inLen;
      while LastLen <> 0 do
      begin
        if LastLen > TmpSize then
          n := TmpSize
        else
          n := LastLen;
        read(Tmp, n);
        if KeySize > 0 then
          XorBlock(pointer(Tmp), n, XorCod);
        Write(Tmp, n);
        dec(LastLen, n);
      end;
    end
    else
    // now we do AES encryption:
    begin
      // 1. Header process
      if Encrypt then
      begin
        // encrypt data
        if (pIn = pOut) and
           (pIn <> nil) then
        begin
          assert(false); // Head in pOut^ will overflow data in pIn^
          result := 0;
          exit;
        end;
        LastLen := inLen and AesBlockMod;
        if LastLen = 0 then
          SetOutLen(inLen + SizeOf(TAesBlock))
        else
          SetOutLen((nBlock + 2) shl AesBlockShift);
        Head.SomeSalt := random(MaxInt);
        Head.HeaderCheck := Head.Calc(Key, KeySize);
        Crypt.Encrypt(TAesBlock(Head));
        Write(@Head, SizeOf(Head));
      end
      else
      begin
        // uncrypt data
        dec(nBlock); // Header is already done
        read(@Head, SizeOf(Head));
        Crypt.Decrypt(TAesBlock(Head));
        with Head do
        begin
          if HeaderCheck <> Head.Calc(Key, KeySize) then
          begin
            result := -1;
            exit; // wrong key
          end;
          SetOutLen(SourceLen);
          LastLen := SourceLen and AesBlockMod;
        end;
        if LastLen <> 0 then
          dec(nBlock); // the very last block is for the very last bytes
      end;
      // 2. All full blocks, with AES
      if Tmp = nil then
        Crypt.DoBlocks(pIn, pOut, pIn, pOut, nBlock, Encrypt)
      else
      begin
        for i := 1 to nBlock div TmpSizeBlock do
          DoBlock(TmpSizeBlock);
        DoBlock(nBlock mod TmpSizeBlock);
      end;
      // 3. Last block
      if LastLen <> 0 then
        if Encrypt then
        begin
          FillcharFast(Last, SizeOf(TAesBlock), 0);
          read(@Last, LastLen);
          Crypt.Encrypt(Last);
          Write(@Last, SizeOf(TAesBlock));
        end
        else
        begin
          read(@Last, SizeOf(TAesBlock));
          Crypt.Decrypt(Last);
          Write(@Last, LastLen);
        end;
      Crypt.Done;
    end;
  finally
    if Tmp <> nil then
      Freemem(Tmp);
  end;
end;


{ TAesWriteStream }

constructor TAesWriteStream.Create(outStream: TStream;
  const Key; KeySize: cardinal);
begin
  inherited Create;
  if KeySize = 0 then
    NoCrypt := true
  else
    AES.EncryptInit(Key, KeySize);
  Dest := outStream;
end;

destructor TAesWriteStream.Destroy;
begin
  Finish;
  AES.Done;
  inherited;
end;

procedure TAesWriteStream.Finish;
begin
  if BufCount = 0 then
    exit;
  if (BufCount >= SizeOf(TAesBlock)) or
     not AES.Initialized or NoCrypt then
    raise ESynCrypto.CreateUtf8('Unexpected %.Finish', [self]);
  XorOffset(@buf, DestSize, BufCount);
  Dest.WriteBuffer(buf, BufCount);
  BufCount := 0;
end;

function TAesWriteStream.{%H-}Read(var Buffer; Count: integer): Longint;
begin
  raise ESynCrypto.CreateUtf8('Unexpected %.Read', [self]);
end;

function TAesWriteStream.{%H-}Seek(Offset: integer; Origin: Word): Longint;
begin
  raise ESynCrypto.CreateUtf8('Unexpected %.Seek', [self]);
end;

function TAesWriteStream.Write(const Buffer; Count: integer): Longint;
// most of the time, a 64KB-buffered compressor have BufCount=0
// will crypt 'const Buffer' memory in place -> use AFTER T*Compressor
var
  B: TByteArray absolute Buffer;
  Len: integer;
begin
  result := Count;
  Adler := Adler32Asm(Adler, @Buffer, Count);
  if not NoCrypt then
    // KeySize=0 -> save as-is
    if not AES.Initialized then
      // if error in KeySize -> default fast XorOffset()
      XorOffset(@B, DestSize, Count)
    else
    begin
      if BufCount > 0 then
      begin
        Len := SizeOf(TAesBlock) - BufCount;
        if Len > Count then
          Len := Count;
        MoveFast(Buffer, buf[BufCount], Len);
        inc(BufCount, Len);
        if BufCount < SizeOf(TAesBlock) then
          exit;
        AES.Encrypt(buf);
        Dest.WriteBuffer(buf, SizeOf(TAesBlock));
        inc(DestSize, SizeOf(TAesBlock));
        Dec(Count, Len);
        AES.DoBlocks(@B[Len], @B[Len], cardinal(Count) shr AesBlockShift, true);
      end
      else
        AES.DoBlocks(@B, @B, cardinal(Count) shr AesBlockShift, true);
      BufCount := cardinal(Count) and AesBlockMod;
      if BufCount <> 0 then
      begin
        dec(Count, BufCount);
        MoveFast(B[Count], buf[0], BufCount);
      end;
    end;
  Dest.WriteBuffer(Buffer, Count);
  inc(DestSize, Count);
end;


function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;
var
  Crypt: TAes;
  Head: TAesFullHeader;
begin
  if KeySize < 128 then
    result := true
  else if not Crypt.DecryptInit(Key, KeySize) then
    result := false
  else
  begin
    Crypt.Decrypt(PAesBlock(buff)^, PAesBlock({%H-}@Head)^);
    result := Head.Calc(Key, KeySize) = Head.HeaderCheck;
    Crypt.Done;
  end;
end;

function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean; OriginalLen: cardinal): integer;
var
  A: TAesFull;
begin
  result := A.EncodeDecode(
    Key, KeySize, Len, Encrypt, nil, nil, bIn, bOut, OriginalLen);
end;

function AESFull(const Key; KeySize: cardinal; bIn: pointer; Len: integer;
  outStream: TStream; Encrypt: boolean; OriginalLen: cardinal): boolean;
var
  A: TAesFull;
begin
  result := A.EncodeDecode(
    Key, KeySize, Len, Encrypt, nil, outStream, bIn, nil, OriginalLen) >= 0;
end;

procedure AESSHA256(bIn, bOut: pointer; Len: integer;
  const Password: RawByteString; Encrypt: boolean);
var
  Digest: TSha256Digest;
begin
  Sha256Weak(Password, Digest);
  AES(Digest, SizeOf(Digest) * 8, bIn, bOut, Len, Encrypt);
  FillZero(Digest);
end;

function AESSHA256(const s, Password: RawByteString;
  Encrypt: boolean): RawByteString;
begin
  SetString(result, nil, length(s));
  AESSHA256(pointer(s), pointer(result), length(s), Password, Encrypt);
end;

procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString;
  Encrypt: boolean);
begin
  AESSHA256(Buffer, Buffer, Len, Password, Encrypt);
end;

procedure AESSHA256Full(bIn: pointer; Len: integer; outStream: TStream;
  const Password: RawByteString; Encrypt: boolean);
var
  Digest: TSha256Digest;
begin
  Sha256Weak(Password, Digest);
  AESFull(Digest, SizeOf(Digest) * 8, bIn, Len, outStream, Encrypt);
end;

{$endif PUREMORMOT2}

// required by read__h__hmac -> deprecated even if available with PUREMORMOT2
procedure Sha256Weak(const s: RawByteString; out Digest: TSha256Digest);
var
  L: integer;
  SHA: TSha256;
  p: PAnsiChar;
  tmp: array[0..255] of byte;
begin
  L := length(s);
  p := pointer(s);
  if L < SizeOf(tmp) then
  begin
    FillcharFast(tmp, SizeOf(tmp), L); // add some salt to unweak password
    if L > 0 then
      MoveFast(p^, tmp, L);
    SHA.Full(@tmp, SizeOf(tmp), Digest);
  end
  else
    SHA.Full(p, L, Digest);
end;



procedure InitializeUnit;
begin
  ComputeAesStaticTables;
  {$ifdef ASMX64}
  {$ifdef CRC32C_X64}
  if (cfSSE42 in CpuFeatures) and
     (cfAesNi in CpuFeatures) and
     (cfCLMUL in CpuFeatures) then
  begin
    // use SSE4.2+pclmulqdq instructions
    crc32c := @crc32c_sse42_aesni;
    DefaultHasher := @crc32c_sse42_aesni;
    InterningHasher := @crc32c_sse42_aesni;
  end;
  {$endif CRC32C_X64}
  if cfSSE41 in CpuFeatures then
  begin
    // optimized Intel's sha256_sse4.asm
    K256Aligned := @K256;
    if PtrUInt(K256Aligned) and 15 <> 0 then
    begin
      if K256AlignedStore = '' then
        GetMemAligned(K256AlignedStore, @K256, SizeOf(K256), K256Aligned);
      if PtrUInt(K256Aligned) and 15 <> 0 then
        K256Aligned := nil; // paranoid
    end;
  end;
  {$endif ASMX64}
  assert(SizeOf(TMd5Buf) = SizeOf(TMd5Digest));
  assert(SizeOf(TAes) = AES_CONTEXT_SIZE);
  assert(SizeOf(TAesContext) = AES_CONTEXT_SIZE);
  assert(AES_CONTEXT_SIZE <= 300); // see mormot.db.raw.sqlite3.static KEYLENGTH
  assert(SizeOf(TSHAContext) = SHAContextSize);
  assert(SizeOf(TSha3Context) = SHA3ContextSize);
  assert(1 shl AesBlockShift = SizeOf(TAesBlock));
  {$ifndef PUREMORMOT2}
  assert(SizeOf(TAesFullHeader) = SizeOf(TAesBlock));
  {$endif PUREMORMOT2}
  assert(SizeOf(TAesIVCtr) = SizeOf(TAesBlock));
  assert(SizeOf(TSha256) = SizeOf(TSha1));
  assert(SizeOf(TSha512) > SizeOf(TSha256));
  assert(SizeOf(TSha3) > SizeOf(TSha512));
  assert(SizeOf(TSha3) > SizeOf(THMAC_SHA512));
  assert((PtrUInt(@TD0) + $400 = PtrUInt(@TD1)) and
         (PtrUInt(@TD0) + $800 = PtrUInt(@TD2)) and
         (PtrUInt(@TD0) + $C00 = PtrUInt(@TD3)) and
         (PtrUInt(@TD0) + $1000 = PtrUInt(@TE0)) and
         (PtrUInt(@TD0) + $1400 = PtrUInt(@TE1)) and
         (PtrUInt(@TD0) + $1800 = PtrUInt(@TE2)) and
         (PtrUInt(@TD0) + $1C00 = PtrUInt(@TE3)) and
         (SBox[255] = $16) and (InvSBox[0] = $52) and
         (Te0[0] = $a56363c6) and (Te0[255] = $3a16162c) and
         (Te1[0] = $6363c6a5) and (Te1[255] = $16162c3a) and
         (Te3[0] = $c6a56363) and (Te3[255] = $2c3a1616) and
         (Td0[0] = $50a7f451) and (Td0[99] = 0) and (Td0[255] = $4257b8d0) and
         (Td3[0] = $5150a7f4) and (Td3[255] = $d04257b8));
end;

procedure FinalizeUnit;
begin
  FreeAndNil(aesivctr[false]);
  FreeAndNil(aesivctr[true]);
  FreeAndNil(MainAesPrng);
  {$ifdef USE_PROV_RSA_AES}
  if (CryptoApiAesProvider <> nil) and
     (CryptoApiAesProvider <> HCRYPTPROV_NOTTESTED) then
    CryptoApi.ReleaseContext(CryptoApiAesProvider, 0);
  {$endif USE_PROV_RSA_AES}
  FillZero(__h);
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.
