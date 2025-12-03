/// Framework Core Complementary Cryptographic Algorithms
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.other;

{
  *****************************************************************************

   Deprecated or Seldom Used Cryptographic Features
    - Deprecated MD4 and RC4 Support
    - Deprecated Low-Level Memory Buffers Helper Functions
    - Deprecated Weak AES/SHA Process
    - BlowFish Encryption
    - BCrypt Password-Hashing Function
    - SCrypt Password-Hashing Function

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.crypt.core;


{ **************** Deprecated MD4 and RC4 Support }

/// initialize a TMd5 instance to work with the legacy MD4 algorithm
// - will reuse the whole MD5 context but setup the MD4 transform function
// - MD4 is clearly deprecated, but available here for compatibility usage
procedure Md4Init(var Engine: TMd5);

/// direct MD4 hash calculation of some data
procedure Md4Buf(const Buffer; Len: cardinal; var Dig: TMd5Digest);

/// direct MD4 hash calculation of some data (string-encoded)
// - result is returned in lowercase hexadecimal format
function Md4(const s: RawByteString): RawUtf8;

type
  /// implements RC4 encryption/decryption
  // - this algorithm has known weaknesses, so should not be considered as
  // cryptographic secure, but is available for other purposes
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  // - you can also restore and backup any previous state of the RC4 encryption
  // by copying the whole TRC4 variable into another (stack-allocated) variable
  {$ifdef USERECORDWITHMETHODS}
  TRC4 = record
  {$else}
  TRC4 = object
  {$endif USERECORDWITHMETHODS}
  private
    {$ifdef CPUINTEL}
    state: array[byte] of PtrInt; // PtrInt=270MB/s  byte=240MB/s on x86
    {$else}
    state: TByteToByte; // on ARM, keep the CPU cache usage low
    {$endif CPUINTEL}
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
    // - each call to this method shall be preceded with an Init() call
    // - RC4 is a symmetrical algorithm: use this Encrypt() method
    // for both encryption and decryption of any buffer
    procedure Encrypt(const BufIn; var BufOut; Count: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// perform the RC4 cypher encryption/decryption on a buffer
    // - each call to this method shall be preceded with an Init() call
    // - RC4 is a symmetrical algorithm: use this EncryptBuffer() method
    // for both encryption and decryption of any buffer
    procedure EncryptBuffer(BufIn, BufOut: PByte; Count: cardinal);
  end;


{ ****************** Deprecated Low-Level Memory Buffers Helper Functions }

{$ifndef PUREMORMOT2}

/// simple XOR encryption according to Cod - not Compression or Stream compatible
// - used in deprecated AESFull() for KeySize=32
// - Cod is used to derivate some pseudo-random content from internal constant
// tables, so encryption is weak but fast
procedure XorBlock(p: PIntegerArray; Count, Cod: integer);

/// simple XOR Cypher using Index (=Position in Dest Stream)
// - Compression not compatible with this function: should be applied after
// compress (e.g. as outStream for TAesWriteStream)
// - Stream compatible (with updated Index)
// - used in deprecated AES() and TAesWriteStream
// - Index is used to derivate some pseudo-random content from internal
// constant tables, so encryption is weak but fast
procedure XorOffset(P: PByteArray; Index, Count: PtrInt);

/// weak XOR Cypher changing by Count value
// - Compression compatible, since the XOR value is always the same, the
// compression rate will not change a lot
// - this encryption is very weak, so should be used only for basic
// obfuscation, not data protection
procedure XorConst(p: PIntegerArray; Count: integer);

{$endif PUREMORMOT2}


{ ****************** Deprecated Weak AES/SHA Process }

{$ifndef PUREMORMOT2}

type
  {$A-}
  /// internal header for storing our AES data with salt and CRC
  // - memory size matches an TAesBlock on purpose, for direct encryption
  // - TAesFull uses unsafe direct AES-ECB chain mode, so is considered deprecated
  {$ifdef USERECORDWITHMETHODS}
  TAesFullHeader = record
  {$else}
  TAesFullHeader = object
  {$endif USERECORDWITHMETHODS}
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
  // - calls internally TAes objet methods, and handle memory and streams for best speed
  // - a TAesFullHeader is encrypted at the beginning, allowing fast Key validation,
  // but the resulting stream is not compatible with raw TAes object
  // - will use unsafe direct AES-ECB chain mode, so is considered deprecated
  {$ifdef USERECORDWITHMETHODS}
  TAesFull = record
  {$else}
  TAesFull = object
  {$endif USERECORDWITHMETHODS}
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

  /// AES encryption stream (deprecated)
  // - encrypt the Data on the fly, in a compatible way with AES() - last bytes
  // are coded with XOR (not compatible with TAesFull format)
  // - not optimized for small blocks -> ok if used AFTER TBZCompressor/TZipCompressor
  // - warning: Write() will crypt Buffer memory in place -> use AFTER T*Compressor
  // - will use unsafe direct AES-ECB chain mode, so is considered deprecated:
  // consider TAesPkcs7Writer and TAesPkcs7Reader instead
  TAesWriteStream = class(TStream)
  public
    Adler, // CRC from uncrypted compressed data - for Key check
    DestSize: cardinal;
  private
    fDest: TStream;
    fBuf: TAesBlock;    // very small buffer for remainging 0..15 bytes
    fBufCount: integer; // number of pending bytes (0..15) in Buf
    fAes: TAes;
    fNoCrypt: boolean;  // if KeySize=0
  public
    /// initialize the AES encryption stream for an output stream (e.g.
    // a TMemoryStream or a TFileStreamEx)
    constructor Create(outStream: TStream; const Key; KeySize: cardinal);
    /// finalize the AES encryption stream
    // - internally call the Finish method
    destructor Destroy; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Read(var Buffer; Count: Longint): Longint; override;
    /// append some data to the outStream, after encryption
    function Write(const Buffer; Count: Longint): Longint; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// write pending data
    // - should always be called before closing the outStream (some data may
    // still be in the internal buffers)
    procedure Finish;
  end;


/// direct Encrypt/Decrypt of data using the TAes class (deprecated)
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: integer;
  Encrypt: boolean); overload; deprecated;

/// direct Encrypt/Decrypt of data using the TAes class (deprecated)
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean); overload; deprecated;

/// direct Encrypt/Decrypt of data using the TAes class (deprecated)
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AES(const Key; KeySize: cardinal; const s: RawByteString;
  Encrypt: boolean): RawByteString; overload; deprecated;

/// direct Encrypt/Decrypt of data using the TAes class (deprecated)
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal;
  Stream: TStream; Encrypt: boolean): boolean; overload; deprecated;

/// AES and XOR encryption using the TAesFull format (deprecated)
// - outStream will be larger/smaller than Len (full AES encrypted)
// - if KeySize is not in [128,192,256], will use a naive simple Xor Cypher
// - returns true if OK
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AESFull(const Key; KeySize: cardinal;
  bIn: pointer; Len: integer; outStream: TStream; Encrypt: boolean;
  OriginalLen: cardinal = 0): boolean; overload; deprecated;

/// AES and XOR encryption using the TAesFull format (deprecated)
// - bOut must be at least bIn+32/Encrypt bIn-16/Decrypt
// - if KeySize is not in [128,192,256], will use a naive simple Xor Cypher
// - returns outLength, -1 if error
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean; OriginalLen: cardinal = 0): integer; overload; deprecated;

/// AES and XOR decryption check using the TAesFull format (deprecated)
// - return true if the beginning of buff contains some data AESFull-encrypted
// with this Key
// - if not KeySize in [128,192,256], will always return true
// - will use unsafe direct AES-ECB chain mode, so is marked as deprecated
function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean; deprecated;

/// AES encryption using the TAes format with a supplied password (deprecated)
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString;
  Encrypt: boolean); overload; deprecated;

/// AES encryption using the TAes format with a supplied password (deprecated)
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString;
  Encrypt: boolean); overload; deprecated;

/// AES encryption using the TAes format with a supplied password (deprecated)
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
function AESSHA256(const s, Password: RawByteString;
  Encrypt: boolean): RawByteString; overload; deprecated;

/// AES encryption using the TAesFull format with a supplied password (deprecated)
// - outStream will be larger/smaller than Len: this is a full AES version with
// a triming TAesFullHeader at the beginning
// - will use unsafe direct AES-ECB chain mode and weak direct SHA-256 (HMAC-256
// is preferred), so is marked as deprecated
procedure AESSHA256Full(bIn: pointer; Len: integer; outStream: TStream;
  const Password: RawByteString; Encrypt: boolean); overload; deprecated;

{$endif PUREMORMOT2}


{ **************** BlowFish Encryption }

type
  /// BlowFish Subkeys
  // - is 72 bytes, i.e. BCRYPT_MAXKEYLEN
  TPBox = array[0..17] of cardinal;
  /// BlowFish Subtitution Boxes
  TSBox = array[0..1023] of cardinal;

  /// the current BlowFish state
  // - stored as PBox[] / SBox[] so that all blocks could be encrypted in order
  TBlowFishState = record
    /// BlowFish Subkeys
    PBox: TPBox;
    /// BlowFish Subtitution Boxes
    SBox: TSBox;
  end;
  /// points to a TBlowFishState binary buffer
  PBlowFishState = ^TBlowFishState;

const
  BLOWFISH_SALTLEN   = 16;
  BLOWFISH_MAXKEYLEN = SizeOf(TPBox);

/// raw in-place encryption of one BlowFish 64-bit block
// - by design, this function is thread-safe, since TBlowFishState is untouched
procedure BlowFishEncrypt64(const s: TBlowFishState; block: PQWordRec);

/// BlowFish encryption using CTR block chain over the supplied IV
// - last len 1..7 bytes would be XORed from IV as per the CTR standard
// - by design, this function is thread-safe, since TBlowFishState is untouched
procedure BlowFishEncryptCtr(src, dest: PQWord; len: PtrUInt;
  const state: TBlowFishState; iv: PQWord);

// published for testing purposes
procedure BlowFishCtrInc(iv: PQWord); {$ifndef CPUINTEL} inline; {$endif}

/// regular BlowFish key setup with a given salt and UTF-8 password
// - Salt is expected to be 16 bytes = 128-bit, e.g. from Random128()
// - Password will be passed to BlowFishPrepareKey() - so trimmed to 72 bytes -
// before calling the overloaded BlowFishKeySetup() binary function
procedure BlowFishKeySetup(var State: TBlowFishState;
  Salt: PHash128Rec; const Password: RawUtf8); overload;

/// prepare a password into a binary key usable for BlowFishKeySetup()
// - Salt and Key will also be converted to big-endian
// - caller should call FillZero(key) once done with this sensitive buffer
// - return the number of 64-bit blocks of the padded key
// - by design, Password will be truncated to 72 bytes (BLOWFISH_MAXKEYLEN)
function BlowFishPrepareKey(const Password: RawUtf8; Salt: PHash128Rec;
  out Key: RawByteString): PtrInt;

/// raw BlowFish key setup with binary input parameters
// - salt is expected to be 16 bytes = 128-bit
// - key is expected to be already prepared with ending #0 and in 64-bit chunks
// - consider BCryptExpensiveKeySetup() for a safer (and slower) initialization
procedure BlowFishKeySetup(var State: TBlowFishState;
  Salt, Key: PQwordArray; KeyBlocks: PtrUInt); overload;

/// finalize a given BlowFish key state in memory
procedure BlowFishKeyClear(var State: TBlowFishState);

type
  /// a convenient way to use BlowFish-CTR encoding/decoding
  TBlowFishCtr = class
  protected
    fIV: QWord;
    fState: PBlowFishState;
  public
    /// setup the BlowFish-CTR context for cypher
    // - by design, BlowFish has its own safe password-hashing algorithm: no need
    // to use a cryptographic hash first (unless the password is > 72 bytes)
    // - with default Cost = 0, will use regular BlowFishKeySetup()
    // - Cost in [1..31] uses safer (and much slower) BCryptExpensiveKeySetup()
    // - if no Salt is supplied, a fixed value (from this unit) will be used
    // - in comparison e.g. with AES, key setup can be (very) slow: don't use
    // this class and algorithm for a short transient message
    constructor Create(const Password: RawByteString; Cost: byte = 0;
      Salt: PHash128Rec = nil); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// perform the actual encoding/decoding on binary buffers
    // - CTR is a reverse algorithm: apply once to cipher, and again to decipher
    // - Count may not be an exact multiple of 64-bit - will trim the IV bytes
    // - will update the internal IV - so you can call this method several times
    // but this method won't be thread-safe
    procedure EncryptBuffer(BufIn, BufOut: pointer; Count: cardinal);
    /// perform the actual encoding on a RawByteString with proper CTR padding
    // - if IVAtBeginning is TRUE, a random 64-bit Initialization Vector will be
    // generated by TAesPrng and stored at the beginning of the output buffer -
    // this will also make this method thread-safe
    // - if IVAtBeginning is FALSE, internal IV will be used (not thread-safe)
    function Encrypt(const Input: RawByteString;
      IVAtBeginning: boolean = false): RawByteString;
    /// perform the actual decoding on a RawByteString with proper CTR padding
    // - if IVAtBeginning is TRUE, a random 64-bit Initialization Vector is
    // expected to be stored at the beginning of the output buffer - this
    // will also make this method thread-safe
    // - if IVAtBeginning is FALSE, internal IV will be used (not thread-safe)
    function Decrypt(const Input: RawByteString;
      IVAtBeginning: boolean = false): RawByteString;
    /// access to the current 64-bit IV state
    property IV: QWord
      read fIV write fIV;
    /// raw access to the BlowFish expanded internal key context
    // - could be used e.g. for thread-safe process of the current secret key
    // using direct BlowFishEncryptCtr() call
    property State: PBlowFishState
      read fState;
  end;


{ **************** BCrypt Password-Hashing Function }

const
  BCRYPT_MAXKEYLEN = BLOWFISH_MAXKEYLEN;
  BCRYPT_SALTLEN   = BLOWFISH_SALTLEN;

/// BCrypt password hashing function as used on BSD systems
// - this adaptative algorithm has no known weaknesses, and there are reports
// that the more recent Argon2 is weaker (and less proven) for practical timing,
// and SCrypt requires N>=2^14 to be stronger (i.e. at least 16MB), so BCrypt
// seems still the best solution for server-side password hashing
// - Cost should be in range 4..31 for 2^Cost rounds (default value is 12, and
// takes 180ms on my computer)
// - Salt='' would generate one - or should be exactly 22 characters (16 bytes)
// - PreSha256 would HMAC-SHA-256 the password (returning $bcrypt-sha256$) to
// circumvent the initial password length limitation of 72 chars
// - assigned to mormot.crypt.core.pas BCrypt() redirection by this unit
// - returns e.g. '$2b$<cost>$<salt><checksum>' for the regular BSD format or
// '$bcrypt-sha256$v=2,t=2b,r=<cost>$<salt'$ for the passlib extended format
function BCryptHash(const Password: RawUtf8; const Salt: RawUtf8 = '';
  Cost: byte = 12; HashPos: PInteger = nil; PreSha256: boolean = false): RawUtf8;

/// prepare a BlockFish encryption with a given Salt, UTF-8 Password and Cost
// - Password is process using the BCrypt "Expensive Key Setup" algorithm
// - Cost should be in range 4..31
// - Salt is expected to be 16 bytes = 128-bit, e.g. from Random128()
procedure BCryptExpensiveKeySetup(var State: TBlowFishState;
  Cost: byte; Salt: PHash128Rec; const Password: RawUtf8);


{ **************** SCrypt Password-Hashing Function }

/// apply in-place Salsa20/8 transformation over a 64 bytes buffer
procedure Salsa20x8(B: PCardinalArray);

/// low-level SCrypt hash computation using our pure pascal code
// - the tuned SSE2 code of this unit is faster than mormot.lib.openssl11:
// $ on Win32:     RawSCrypt in 101ms, OpenSslScrypt in 157ms
// $ on Win64:     RawSCrypt in 92ms,  OpenSslScrypt in 124ms
// $ on Linux x64: RawSCrypt in 74ms,  OpenSslScrypt in 103ms
// - assigned to mormot.crypt.core.pas SCrypt() redirection by this unit
// - for password storage and interactive login, consider SCryptHash() from
// mormot.crypt.secure.pas with N=65536=2^16, R=8, P=2 (148ms and 64MB of RAM)
// - for local key derivation (e.g. file encryption) consider using this
// function directly with e.g. N=1048576=2^20, R=8, P=1 (1.23s and 1GB) to
// compute the binary encryption key
function RawSCrypt(const Password: RawUtf8; const Salt: RawByteString;
  N, R, P, DestLen: PtrUInt): RawByteString;

/// compute how much memory the SCrypt() function will allocate
// - could be used to tune the parameters (N, R, P) somewhat obfuscated meaning
// - return e.g. 16MB for SCrypt(16384, 8, 1) and 64MB for SCrypt(65536, 8, 1),
// i.e. equals roughtly N * R * 128 with some more bytes depending on P
// - SCrypt(16384, 8, 1) are the recommended minimal parameters value to have
// benefits against BCrypt() - but still consuming 16MB instead of 4KB so may
// not be ideal for password storage on server side, but fine for a client-side
// one-time key derivation function to unlock a resource
// - TL&WR: use N=65536=2^16, R=8, P=2 for safe interactive login (148ms and
// 64MB of RAM) or N=1048576=2^20, R=8, P=1 for file encryption (1.23s and 1GB)
function SCryptMemoryUse(N, R, P: QWord): QWord;


implementation


{ **************** Deprecated MD4 and RC4 Support }

{$ifndef FPC} // this operation is an intrinsic with the FPC compiler
function RolDWord(value: cardinal; count: integer): cardinal;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := (value shl count) or (value shr (32 - count));
end;
{$endif FPC}

procedure MD4Transform(var buf: TMd5Buf; const in_: TMd5In);
var
  a, b, c, d, e: cardinal;
begin // fast enough unrolled code - especially with FPC RolDWord() intrinsic
  a := buf[0];
  b := buf[1];
  c := buf[2];
  d := buf[3];
  a := RolDWord(a + (d xor (b and (c xor d))) + in_[ 0], 3);
  d := RolDWord(d + (c xor (a and (b xor c))) + in_[ 1], 7);
  c := RolDWord(c + (b xor (d and (a xor b))) + in_[ 2], 11);
  b := RolDWord(b + (a xor (c and (d xor a))) + in_[ 3], 19);
  a := RolDWord(a + (d xor (b and (c xor d))) + in_[ 4], 3);
  d := RolDWord(d + (c xor (a and (b xor c))) + in_[ 5], 7);
  c := RolDWord(c + (b xor (d and (a xor b))) + in_[ 6], 11);
  b := RolDWord(b + (a xor (c and (d xor a))) + in_[ 7], 19);
  a := RolDWord(a + (d xor (b and (c xor d))) + in_[ 8], 3);
  d := RolDWord(d + (c xor (a and (b xor c))) + in_[ 9], 7);
  c := RolDWord(c + (b xor (d and (a xor b))) + in_[10], 11);
  b := RolDWord(b + (a xor (c and (d xor a))) + in_[11], 19);
  a := RolDWord(a + (d xor (b and (c xor d))) + in_[12], 3);
  d := RolDWord(d + (c xor (a and (b xor c))) + in_[13], 7);
  c := RolDWord(c + (b xor (d and (a xor b))) + in_[14], 11);
  b := RolDWord(b + (a xor (c and (d xor a))) + in_[15], 19);
  e := $5a827999;
  a := RolDWord(a + ((b and c) or (b and d) or (c and d)) + in_[ 0] + e, 3);
  d := RolDWord(d + ((a and b) or (a and c) or (b and c)) + in_[ 4] + e, 5);
  c := RolDWord(c + ((d and a) or (d and b) or (a and b)) + in_[ 8] + e, 9);
  b := RolDWord(b + ((c and d) or (c and a) or (d and a)) + in_[12] + e, 13);
  a := RolDWord(a + ((b and c) or (b and d) or (c and d)) + in_[ 1] + e, 3);
  d := RolDWord(d + ((a and b) or (a and c) or (b and c)) + in_[ 5] + e, 5);
  c := RolDWord(c + ((d and a) or (d and b) or (a and b)) + in_[ 9] + e, 9);
  b := RolDWord(b + ((c and d) or (c and a) or (d and a)) + in_[13] + e, 13);
  a := RolDWord(a + ((b and c) or (b and d) or (c and d)) + in_[ 2] + e, 3);
  d := RolDWord(d + ((a and b) or (a and c) or (b and c)) + in_[ 6] + e, 5);
  c := RolDWord(c + ((d and a) or (d and b) or (a and b)) + in_[10] + e, 9);
  b := RolDWord(b + ((c and d) or (c and a) or (d and a)) + in_[14] + e, 13);
  a := RolDWord(a + ((b and c) or (b and d) or (c and d)) + in_[ 3] + e, 3);
  d := RolDWord(d + ((a and b) or (a and c) or (b and c)) + in_[ 7] + e, 5);
  c := RolDWord(c + ((d and a) or (d and b) or (a and b)) + in_[11] + e, 9);
  b := RolDWord(b + ((c and d) or (c and a) or (d and a)) + in_[15] + e, 13);
  e := $6ed9eba1;
  a := RolDWord(a + (b xor c xor d) + in_[ 0] + e, 3);
  d := RolDWord(d + (a xor b xor c) + in_[ 8] + e, 9);
  c := RolDWord(c + (d xor a xor b) + in_[ 4] + e, 11);
  b := RolDWord(b + (c xor d xor a) + in_[12] + e, 15);
  a := RolDWord(a + (b xor c xor d) + in_[ 2] + e, 3);
  d := RolDWord(d + (a xor b xor c) + in_[10] + e, 9);
  c := RolDWord(c + (d xor a xor b) + in_[ 6] + e, 11);
  b := RolDWord(b + (c xor d xor a) + in_[14] + e, 15);
  a := RolDWord(a + (b xor c xor d) + in_[ 1] + e, 3);
  d := RolDWord(d + (a xor b xor c) + in_[ 9] + e, 9);
  c := RolDWord(c + (d xor a xor b) + in_[ 5] + e, 11);
  b := RolDWord(b + (c xor d xor a) + in_[13] + e, 15);
  a := RolDWord(a + (b xor c xor d) + in_[ 3] + e, 3);
  d := RolDWord(d + (a xor b xor c) + in_[11] + e, 9);
  c := RolDWord(c + (d xor a xor b) + in_[ 7] + e, 11);
  b := RolDWord(b + (c xor d xor a) + in_[15] + e, 15);
  inc(buf[0], a);
  inc(buf[1], b);
  inc(buf[2], c);
  inc(buf[3], d);
end;

procedure Md4Init(var Engine: TMd5);
begin
  Engine.Init(@MD4Transform);
end;

procedure Md4Buf(const Buffer; Len: cardinal; var Dig: TMd5Digest);
var
  md: TMd5;
begin
  Md4Init(md);
  md.Update(Buffer, Len);
  md.Final(Dig);
end;

function Md4(const s: RawByteString): RawUtf8;
var
  dig: TMd5Digest;
begin
  Md4Buf(pointer(s)^, Length(s), dig);
  BinToHexLower(@dig, SizeOf(dig), result);
  FillZero(dig);
end;


{ TRC4 }

procedure TRC4.Init(const aKey; aKeyLen: integer);
var
  i, k: integer;
  j, tmp: PtrInt;
begin
  if aKeyLen <= 0 then
    ESynCrypto.RaiseUtf8('TRC4.Init(invalid aKeyLen=%)', [aKeyLen]);
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
  dig: TByteToByte; // max RC4 state size is 256 bytes
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


{ ****************** Deprecated Low-Level Memory Buffers Helper Functions }

{$ifndef PUREMORMOT2}

procedure XorBlock(P: PIntegerArray; Count, Cod: integer);
// very fast Xor() according to Cod - not Compression or Stream compatible
var
  i: integer;
  tab: PIntegerArray;
begin
  tab := AesTables; // = TD0[]
  for i := 1 to Count shr 4 do
  begin
    // proceed through 16 bytes blocs
    Cod := (Cod shl 11) xor tab[Cod shr 21]; // shr 21 -> 8*[byte] of cardinal
    P^[0] := P^[0] xor Cod;
    P^[1] := P^[1] xor Cod;
    P^[2] := P^[2] xor Cod;
    P^[3] := P^[3] xor Cod;
    inc(PByte(P), 16);
  end;
  Cod := (Cod shl 11) xor tab[Cod shr 21];
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
// XorOffset: fast and simple Cypher using Index (= Position in Dest Stream):
// Compression not OK -> apply after compress
var
  Len: PtrInt;
  tab: PByteArray; // 2^13=$2000=8192 bytes of XOR tables ;)
begin
  tab := AesTables; // = TD0[]
  if Count > 0 then
    repeat
      Index := Index and $1FFF;
      Len := $2000 - Index;
      if Len > Count then
        Len := Count;
      XorMemory(P, @tab[Index], Len);
      inc(P, Len);
      inc(Index, Len);
      dec(Count, Len);
    until Count = 0;
end;

procedure XorConst(P: PIntegerArray; Count: integer);
// XorConst: fast Cypher changing by Count value (weak cypher but compression OK)
var
  i: PtrInt;
  Code: integer;
begin
  // 1 to 3 bytes may stay unencrypted: not relevant
  Code := PIntegerArray(AesTables)[Count and $3FF];
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

{$endif PUREMORMOT2}


{ ****************** Deprecated Weak AES/SHA Process }

{$ifndef PUREMORMOT2}

procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: integer;
  Encrypt: boolean);
begin
  {%H-}AES(Key, KeySize, buffer, buffer, Len, Encrypt);
end;

procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean);
var
  n: integer;
  pi, po: PAesBlock;
  aes: TAes;
begin
  if (bIn = nil) or
     (bOut = nil) then
    exit;
  // 1. Init
  n := Len shr AesBlockShift;
  if n < 0 then
    exit;
  aes.InitOnStack;
  if n > 0 then
    if (KeySize > 4) and
       not aes.DoInit(Key, KeySize, Encrypt) then
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
  aes.DoBlocks(bIn, bOut, pi, po, n, Encrypt);
  // 3. Last block, just XORed from Key
  // assert(KeySize div 8 >= AesBlockSize);
  n := cardinal(Len) and AesBlockMod;
  MoveFast(pi^, po^, n); // pi=po is tested in MoveFast()
  XorOffset(pointer(po), Len - n, n);
  aes.Done;
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
  FastNewRawByteString(result, length(s));
  if s <> '' then
    {%H-}AES(Key, KeySize, pointer(s), pointer(result), length(s), Encrypt);
end;

function AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal;
  Stream: TStream; Encrypt: boolean): boolean;
var
  buf: pointer;
  last, b, n, i: cardinal;
  aes: TAes;
begin
  result := false;
  if buffer = nil then
    exit;
  aes.InitOnStack;
  if (KeySize > 4) and
     not aes.DoInit(Key, KeySize, Encrypt) then
    // if error in KeySize, use default fast XorOffset()
    KeySize := 4;
  if KeySize = 0 then
  begin
    // no aes -> direct write to dest Stream
    Stream.WriteBuffer(buffer^, Len);
    result := true;
    exit;
  end;
  GetMem(buf, TmpSize);
  try
    last := Len and AesBlockMod;
    n := Len - last;
    i := 0;
    while n > 0 do
    begin
      // aes/uncrypt all AesBlocks
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
        aes.DoBlocks(buffer, buf, b shr AesBlockShift, Encrypt);
      Stream.WriteBuffer(buf^, b);
      inc(PByte(buffer), b);
      dec(n, b);
    end;
    assert((KeySize > 4) or (i = Len - last));
    if last > 0 then
    begin
      // aes/uncrypt (Xor) last 0..15 bytes
      MoveFast(buffer^, buf^, last);
      XorOffset(pointer(buf), Len - last, last);
      Stream.WriteBuffer(buf^, last);
    end;
    result := true;
  finally
    FreeMem(buf);
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
var
  tab: PCardinalArray;
begin
  tab := AesTables;
  result := adler32(KeySize, @Key, KeySize shr 3) xor
            tab[$400 + OriginalLen and $ff] xor // = TE0[]
            tab[$500 + SourceLen and $ff] xor   // = TE1[]
            tab[SomeSalt and $7ff];             // = TD0[]
end;

function TAesFull.EncodeDecode(const Key; KeySize, inLen: cardinal;
  Encrypt: boolean; inStream, outStream: TStream; bIn, bOut: pointer;
  OriginalLen: cardinal): integer;
var
  tmp: ^TTmp;
  pi, po: PAesBlock;
  aes: TAes;
  blocks, cod: cardinal;

  procedure Read(tmp: pointer; ByteCount: cardinal);
  begin
    if pi = nil then
      inStream.ReadBuffer(tmp^, ByteCount)
    else
    begin
      MoveFast(pi^, tmp^, ByteCount);
      inc(PByte(pi), ByteCount);
    end;
  end;

  procedure Write(tmp: pointer; ByteCount: cardinal);
  begin
    if po = nil then
      outStream.WriteBuffer(tmp^, ByteCount)
    else
    begin
      MoveFast(tmp^, po^, ByteCount);
      inc(PByte(po), ByteCount);
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
        begin
          P := TMemoryStream(outStream).Seek(0, soCurrent);
          TMemoryStream(outStream).Size := P + Len; // auto-reserve space
          TMemoryStream(outStream).Seek(P + Len, soBeginning);
          bOut := PAnsiChar(TMemoryStream(outStream).Memory) + P;
          po := bOut;
          outStream := nil; //  OutStream is slower and use no thread
        end;
    end
    else if bOut = nil then
    begin
      outStreamCreated := TMemoryStream.Create;
      outStreamCreated.Size := Len; // auto-reserve space (no Realloc:)
      bOut := outStreamCreated.Memory;
      po := bOut; // OutStream is slower and use no thread
    end;
    if KeySize = 0 then
      exit; // no tmp to be allocated on direct copy
    if (KeySize = 32) or
       (inStream <> nil) or
       (outStream <> nil) then
      New(tmp);
  end;

  procedure DoBlock(BlockCount: integer);
  begin
    if BlockCount = 0 then
      exit;
    read(tmp, BlockCount shl AesBlockShift);
    aes.DoBlocks(PAesBLock(tmp), PAesBLock(tmp), BlockCount, Encrypt);
    Write(tmp, BlockCount shl AesBlockShift);
  end;

var
  n, LastLen: cardinal;
  i: integer;
  last: TAesBlock;
begin
  result := 0; // makes FixInsight happy
  tmp := nil;
  outStreamCreated := nil;
  aes.InitOnStack;
  Head.SourceLen := inLen;
  blocks := Head.SourceLen shr AesBlockShift;
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
  cod := inLen;
  if (inStream <> nil) and
     inStream.InheritsFrom(TCustomMemoryStream) then
  begin
    bIn := TCustomMemoryStream(inStream).Memory;
    inStream := nil;
  end;
  pi := bIn;
  po := bOut;
  if (KeySize >= 128) and
     not aes.DoInit(Key, KeySize, Encrypt) then
    KeySize := 32;
  if KeySize = 32 then
    cod := KeyFrom(Key, KeySize) xor cod
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
      assert(tmp <> nil);
      LastLen := inLen;
      while LastLen <> 0 do
      begin
        if LastLen > TmpSize then
          n := TmpSize
        else
          n := LastLen;
        read(tmp, n);
        if KeySize > 0 then
          XorBlock(pointer(tmp), n, cod);
        Write(tmp, n);
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
        if (pi = po) and
           (pi <> nil) then // Head in po^ will overflow data in pi^
        begin
          ESynCrypto.RaiseU('Unexpected EncodeDecode overflow');
          result := 0;
          exit;
        end;
        LastLen := inLen and AesBlockMod;
        if LastLen = 0 then
          SetOutLen(inLen + SizeOf(TAesBlock))
        else
          SetOutLen((blocks + 2) shl AesBlockShift);
        Head.SomeSalt := Random32Not0;
        Head.HeaderCheck := Head.Calc(Key, KeySize);
        aes.Encrypt(TAesBlock(Head));
        Write(@Head, SizeOf(Head));
      end
      else
      begin
        // uncrypt data
        dec(blocks); // Header is already done
        read(@Head, SizeOf(Head));
        aes.Decrypt(TAesBlock(Head));
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
          dec(blocks); // the very last block is for the very last bytes
      end;
      // 2. All full blocks, with AES
      if tmp = nil then
        aes.DoBlocks(pi, po, pi, po, blocks, Encrypt)
      else
      begin
        for i := 1 to blocks div TmpSizeBlock do
          DoBlock(TmpSizeBlock);
        DoBlock(blocks mod TmpSizeBlock);
      end;
      // 3. last block
      if LastLen <> 0 then
        if Encrypt then
        begin
          FillcharFast(last, SizeOf(TAesBlock), 0);
          read(@last, LastLen);
          aes.Encrypt(last);
          Write(@last, SizeOf(TAesBlock));
        end
        else
        begin
          read(@last, SizeOf(TAesBlock));
          aes.Decrypt(last);
          Write(@last, LastLen);
        end;
      aes.Done;
    end;
  finally
    if tmp <> nil then
      FreeMem(tmp);
  end;
end;


{ TAesWriteStream }

constructor TAesWriteStream.Create(outStream: TStream;
  const Key; KeySize: cardinal);
begin
  inherited Create;
  if KeySize = 0 then
    fNoCrypt := true
  else
    fAes.EncryptInit(Key, KeySize);
  fDest := outStream;
end;

destructor TAesWriteStream.Destroy;
begin
  Finish;
  fAes.Done;
  inherited;
end;

procedure TAesWriteStream.Finish;
begin
  if fBufCount = 0 then
    exit;
  if (fBufCount >= SizeOf(TAesBlock)) or
     fNoCrypt or
     not fAes.Initialized then
    ESynCrypto.RaiseUtf8('Unexpected %.Finish', [self]);
  XorOffset(@fBuf, DestSize, fBufCount);
  fDest.WriteBuffer(fBuf, fBufCount);
  fBufCount := 0;
end;

function TAesWriteStream.{%H-}Read(var Buffer; Count: integer): Longint;
begin
  ESynCrypto.RaiseUtf8('Unexpected %.Read', [self]);
  result := 0; // make compiler happy
end;

function TAesWriteStream.{%H-}Seek(Offset: integer; Origin: Word): Longint;
begin
  ESynCrypto.RaiseUtf8('Unexpected %.Seek', [self]);
  result := 0; // make compiler happy
end;

function TAesWriteStream.Write(const Buffer; Count: integer): Longint;
// most of the time, a 64KB-buffered compressor have BufCount=0
// will crypt 'const Buffer' memory in place -> use AFTER T*Compressor
var
  B: TByteArray absolute Buffer;
  len: integer;
begin
  result := Count;
  Adler := adler32(Adler, @Buffer, Count);
  if not fNoCrypt then
    // KeySize=0 -> save as-is
    if not fAes.Initialized then
      // if error in KeySize -> default fast XorOffset()
      XorOffset(@B, DestSize, Count)
    else
    begin
      len := 0;
      if fBufCount > 0 then // append to data pending in fBuf[fBufCount]
      begin
        len := SizeOf(fBuf) - fBufCount;
        if len > Count then
          len := Count;
        MoveFast(Buffer, fBuf[fBufCount], len);
        inc(fBufCount, len);
        if fBufCount < SizeOf(fBuf) then
          exit;
        fAes.Encrypt(fBuf);
        fDest.WriteBuffer(fBuf, SizeOf(fBuf));
        inc(DestSize, SizeOf(fBuf));
        dec(Count, len);
      end;
      fAes.DoBlocks(@B[len], @B[len], Count shr AesBlockShift, true);
      fBufCount := Count and AesBlockMod;
      if fBufCount <> 0 then
      begin
        dec(Count, fBufCount);
        MoveFast(B[Count], fBuf[0], fBufCount);
      end;
    end;
  fDest.WriteBuffer(Buffer, Count);
  inc(DestSize, Count);
end;


function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;
var
  aes: TAes;
  head: TAesFullHeader;
begin
  aes.InitOnStack;
  if KeySize < 128 then
    result := true
  else if not aes.DecryptInit(Key, KeySize) then
    result := false
  else
  begin
    aes.Decrypt(PAesBlock(buff)^, PAesBlock({%H-}@head)^);
    result := head.Calc(Key, KeySize) = head.HeaderCheck;
    aes.Done;
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
  dig: TSha256Digest;
begin
  Sha256Weak(Password, dig);
  {%H-}AES(dig, SizeOf(dig) * 8, bIn, bOut, Len, Encrypt);
  FillZero(dig);
end;

function AESSHA256(const s, Password: RawByteString;
  Encrypt: boolean): RawByteString;
begin
  FastNewRawByteString(result, length(s));
  {%H-}AESSHA256(pointer(s), pointer(result), length(s), Password, Encrypt);
end;

procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString;
  Encrypt: boolean);
begin
  {%H-}AESSHA256(Buffer, Buffer, Len, Password, Encrypt);
end;

procedure AESSHA256Full(bIn: pointer; Len: integer; outStream: TStream;
  const Password: RawByteString; Encrypt: boolean);
var
  dig: TSha256Digest;
begin
  Sha256Weak(Password, dig);
  {%H-}AESFull(dig, SizeOf(dig) shl 3, bIn, Len, outStream, Encrypt);
end;

{$endif PUREMORMOT2}


{ **************** BlowFish Encryption }

{
  In respect to existing pascal - or c - code around, our version:
  - use 64-bit process whenever possible - since BlowFish has 64-bit blocks
  - prepare the password to be a multiple of 64-bit chunks for consistency
  - allow PBox/SBox to be encrypted as one continuous chunk (in this order)
  - reduce the number of needed big-endian conversions as much as possible
  - resulting in much cleaner and shorter code, especially for the bcrypt part
  - profiling shows that most of the time is stil spent in BlowFishEncrypt64()
  - not included in mormot.crypt.core: less common and with lots of constants
}

const
  /// default BlowFish state
  // - this 4KB constant array can't be easily computed at runtime, since it
  // contains the PI digits in binary format
  BLOWFISH_INIT: TBlowFishState = (
    PBox: (
      $243f6a88, $85a308d3, $13198a2e, $03707344, $a4093822, $299f31d0, $082efa98,
      $ec4e6c89, $452821e6, $38d01377, $be5466cf, $34e90c6c, $c0ac29b7, $c97c50dd,
      $3f84d5b5, $b5470917, $9216d5d9, $8979fb1b);
    SBox: (
      $d1310ba6, $98dfb5ac, $2ffd72db, $d01adfb7, $b8e1afed, $6a267e96, $ba7c9045,
      $f12c7f99, $24a19947, $b3916cf7, $0801f2e2, $858efc16, $636920d8, $71574e69,
      $a458fea3, $f4933d7e, $0d95748f, $728eb658, $718bcd58, $82154aee, $7b54a41d,
      $c25a59b5, $9c30d539, $2af26013, $c5d1b023, $286085f0, $ca417918, $b8db38ef,
      $8e79dcb0, $603a180e, $6c9e0e8b, $b01e8a3e, $d71577c1, $bd314b27, $78af2fda,
      $55605c60, $e65525f3, $aa55ab94, $57489862, $63e81440, $55ca396a, $2aab10b6,
      $b4cc5c34, $1141e8ce, $a15486af, $7c72e993, $b3ee1411, $636fbc2a, $2ba9c55d,
      $741831f6, $ce5c3e16, $9b87931e, $afd6ba33, $6c24cf5c, $7a325381, $28958677,
      $3b8f4898, $6b4bb9af, $c4bfe81b, $66282193, $61d809cc, $fb21a991, $487cac60,
      $5dec8032, $ef845d5d, $e98575b1, $dc262302, $eb651b88, $23893e81, $d396acc5,
      $0f6d6ff3, $83f44239, $2e0b4482, $a4842004, $69c8f04a, $9e1f9b5e, $21c66842,
      $f6e96c9a, $670c9c61, $abd388f0, $6a51a0d2, $d8542f68, $960fa728, $ab5133a3,
      $6eef0b6c, $137a3be4, $ba3bf050, $7efb2a98, $a1f1651d, $39af0176, $66ca593e,
      $82430e88, $8cee8619, $456f9fb4, $7d84a5c3, $3b8b5ebe, $e06f75d8, $85c12073,
      $401a449f, $56c16aa6, $4ed3aa62, $363f7706, $1bfedf72, $429b023d, $37d0d724,
      $d00a1248, $db0fead3, $49f1c09b, $075372c9, $80991b7b, $25d479d8, $f6e8def7,
      $e3fe501a, $b6794c3b, $976ce0bd, $04c006ba, $c1a94fb6, $409f60c4, $5e5c9ec2,
      $196a2463, $68fb6faf, $3e6c53b5, $1339b2eb, $3b52ec6f, $6dfc511f, $9b30952c,
      $cc814544, $af5ebd09, $bee3d004, $de334afd, $660f2807, $192e4bb3, $c0cba857,
      $45c8740f, $d20b5f39, $b9d3fbdb, $5579c0bd, $1a60320a, $d6a100c6, $402c7279,
      $679f25fe, $fb1fa3cc, $8ea5e9f8, $db3222f8, $3c7516df, $fd616b15, $2f501ec8,
      $ad0552ab, $323db5fa, $fd238760, $53317b48, $3e00df82, $9e5c57bb, $ca6f8ca0,
      $1a87562e, $df1769db, $d542a8f6, $287effc3, $ac6732c6, $8c4f5573, $695b27b0,
      $bbca58c8, $e1ffa35d, $b8f011a0, $10fa3d98, $fd2183b8, $4afcb56c, $2dd1d35b,
      $9a53e479, $b6f84565, $d28e49bc, $4bfb9790, $e1ddf2da, $a4cb7e33, $62fb1341,
      $cee4c6e8, $ef20cada, $36774c01, $d07e9efe, $2bf11fb4, $95dbda4d, $ae909198,
      $eaad8e71, $6b93d5a0, $d08ed1d0, $afc725e0, $8e3c5b2f, $8e7594b7, $8ff6e2fb,
      $f2122b64, $8888b812, $900df01c, $4fad5ea0, $688fc31c, $d1cff191, $b3a8c1ad,
      $2f2f2218, $be0e1777, $ea752dfe, $8b021fa1, $e5a0cc0f, $b56f74e8, $18acf3d6,
      $ce89e299, $b4a84fe0, $fd13e0b7, $7cc43b81, $d2ada8d9, $165fa266, $80957705,
      $93cc7314, $211a1477, $e6ad2065, $77b5fa86, $c75442f5, $fb9d35cf, $ebcdaf0c,
      $7b3e89a0, $d6411bd3, $ae1e7e49, $00250e2d, $2071b35e, $226800bb, $57b8e0af,
      $2464369b, $f009b91e, $5563911d, $59dfa6aa, $78c14389, $d95a537f, $207d5ba2,
      $02e5b9c5, $83260376, $6295cfa9, $11c81968, $4e734a41, $b3472dca, $7b14a94a,
      $1b510052, $9a532915, $d60f573f, $bc9bc6e4, $2b60a476, $81e67400, $08ba6fb5,
      $571be91f, $f296ec6b, $2a0dd915, $b6636521, $e7b9f9b6, $ff34052e, $c5855664,
      $53b02d5d, $a99f8fa1, $08ba4799, $6e85076a, $4b7a70e9, $b5b32944, $db75092e,
      $c4192623, $ad6ea6b0, $49a7df7d, $9cee60b8, $8fedb266, $ecaa8c71, $699a17ff,
      $5664526c, $c2b19ee1, $193602a5, $75094c29, $a0591340, $e4183a3e, $3f54989a,
      $5b429d65, $6b8fe4d6, $99f73fd6, $a1d29c07, $efe830f5, $4d2d38e6, $f0255dc1,
      $4cdd2086, $8470eb26, $6382e9c6, $021ecc5e, $09686b3f, $3ebaefc9, $3c971814,
      $6b6a70a1, $687f3584, $52a0e286, $b79c5305, $aa500737, $3e07841c, $7fdeae5c,
      $8e7d44ec, $5716f2b8, $b03ada37, $f0500c0d, $f01c1f04, $0200b3ff, $ae0cf51a,
      $3cb574b2, $25837a58, $dc0921bd, $d19113f9, $7ca92ff6, $94324773, $22f54701,
      $3ae5e581, $37c2dadc, $c8b57634, $9af3dda7, $a9446146, $0fd0030e, $ecc8c73e,
      $a4751e41, $e238cd99, $3bea0e2f, $3280bba1, $183eb331, $4e548b38, $4f6db908,
      $6f420d03, $f60a04bf, $2cb81290, $24977c79, $5679b072, $bcaf89af, $de9a771f,
      $d9930810, $b38bae12, $dccf3f2e, $5512721f, $2e6b7124, $501adde6, $9f84cd87,
      $7a584718, $7408da17, $bc9f9abc, $e94b7d8c, $ec7aec3a, $db851dfa, $63094366,
      $c464c3d2, $ef1c1847, $3215d908, $dd433b37, $24c2ba16, $12a14d43, $2a65c451,
      $50940002, $133ae4dd, $71dff89e, $10314e55, $81ac77d6, $5f11199b, $043556f1,
      $d7a3c76b, $3c11183b, $5924a509, $f28fe6ed, $97f1fbfa, $9ebabf2c, $1e153c6e,
      $86e34570, $eae96fb1, $860e5e0a, $5a3e2ab3, $771fe71c, $4e3d06fa, $2965dcb9,
      $99e71d0f, $803e89d6, $5266c825, $2e4cc978, $9c10b36a, $c6150eba, $94e2ea78,
      $a5fc3c53, $1e0a2df4, $f2f74ea7, $361d2b3d, $1939260f, $19c27960, $5223a708,
      $f71312b6, $ebadfe6e, $eac31f66, $e3bc4595, $a67bc883, $b17f37d1, $018cff28,
      $c332ddef, $be6c5aa5, $65582185, $68ab9802, $eecea50f, $db2f953b, $2aef7dad,
      $5b6e2f84, $1521b628, $29076170, $ecdd4775, $619f1510, $13cca830, $eb61bd96,
      $0334fe1e, $aa0363cf, $b5735c90, $4c70a239, $d59e9e0b, $cbaade14, $eecc86bc,
      $60622ca7, $9cab5cab, $b2f3846e, $648b1eaf, $19bdf0ca, $a02369b9, $655abb50,
      $40685a32, $3c2ab4b3, $319ee9d5, $c021b8f7, $9b540b19, $875fa099, $95f7997e,
      $623d7da8, $f837889a, $97e32d77, $11ed935f, $16681281, $0e358829, $c7e61fd6,
      $96dedfa1, $7858ba99, $57f584a5, $1b227263, $9b83c3ff, $1ac24696, $cdb30aeb,
      $532e3054, $8fd948e4, $6dbc3128, $58ebf2ef, $34c6ffea, $fe28ed61, $ee7c3c73,
      $5d4a14d9, $e864b7e3, $42105d14, $203e13e0, $45eee2b6, $a3aaabea, $db6c4f15,
      $facb4fd0, $c742f442, $ef6abbb5, $654f3b1d, $41cd2105, $d81e799e, $86854dc7,
      $e44b476a, $3d816250, $cf62a1f2, $5b8d2646, $fc8883a0, $c1c7b6a3, $7f1524c3,
      $69cb7492, $47848a0b, $5692b285, $095bbf00, $ad19489d, $1462b174, $23820e00,
      $58428d2a, $0c55f5ea, $1dadf43e, $233f7061, $3372f092, $8d937e41, $d65fecf1,
      $6c223bdb, $7cde3759, $cbee7460, $4085f2a7, $ce77326e, $a6078084, $19f8509e,
      $e8efd855, $61d99735, $a969a7aa, $c50c06c2, $5a04abfc, $800bcadc, $9e447a2e,
      $c3453484, $fdd56705, $0e1e9ec9, $db73dbd3, $105588cd, $675fda79, $e3674340,
      $c5c43465, $713e38d8, $3d28f89e, $f16dff20, $153e21e7, $8fb03d4a, $e6e39f2b,
      $db83adf7, $e93d5a68, $948140f7, $f64c261c, $94692934, $411520f7, $7602d4f7,
      $bcf46b2e, $d4a20068, $d4082471, $3320f46a, $43b7d4b7, $500061af, $1e39f62e,
      $97244546, $14214f74, $bf8b8840, $4d95fc1d, $96b591af, $70f4ddd3, $66a02f45,
      $bfbc09ec, $03bd9785, $7fac6dd0, $31cb8504, $96eb27b3, $55fd3941, $da2547e6,
      $abca0a9a, $28507825, $530429f4, $0a2c86da, $e9b66dfb, $68dc1462, $d7486900,
      $680ec0a4, $27a18dee, $4f3ffea2, $e887ad8c, $b58ce006, $7af4d6b6, $aace1e7c,
      $d3375fec, $ce78a399, $406b2a42, $20fe9e35, $d9f385b9, $ee39d7ab, $3b124e8b,
      $1dc9faf7, $4b6d1856, $26a36631, $eae397b2, $3a6efa74, $dd5b4332, $6841e7f7,
      $ca7820fb, $fb0af54e, $d8feb397, $454056ac, $ba489527, $55533a3a, $20838d87,
      $fe6ba9b7, $d096954b, $55a867bc, $a1159a58, $cca92963, $99e1db33, $a62a4a56,
      $3f3125f9, $5ef47e1c, $9029317c, $fdf8e802, $04272f70, $80bb155c, $05282ce3,
      $95c11548, $e4c66d22, $48c1133f, $c70f86dc, $07f9c9ee, $41041f0f, $404779a4,
      $5d886e17, $325f51eb, $d59bc0d1, $f2bcc18f, $41113564, $257b7834, $602a9c60,
      $dff8e8a3, $1f636c1b, $0e12b4c2, $02e1329e, $af664fd1, $cad18115, $6b2395e0,
      $333e92e1, $3b240b62, $eebeb922, $85b2a20e, $e6ba0d99, $de720c8c, $2da2f728,
      $d0127845, $95b794fd, $647d0862, $e7ccf5f0, $5449a36f, $877d48fa, $c39dfd27,
      $f33e8d1e, $0a476341, $992eff74, $3a6f6eab, $f4f8fd37, $a812dc60, $a1ebddf8,
      $991be14c, $db6e6b0d, $c67b5510, $6d672c37, $2765d43b, $dcd0e804, $f1290dc7,
      $cc00ffa3, $b5390f92, $690fed0b, $667b9ffb, $cedb7d9c, $a091cf0b, $d9155ea3,
      $bb132f88, $515bad24, $7b9479bf, $763bd6eb, $37392eb3, $cc115979, $8026e297,
      $f42e312d, $6842ada7, $c66a2b3b, $12754ccc, $782ef11c, $6a124237, $b79251e7,
      $06a1bbe6, $4bfb6350, $1a6b1018, $11caedfa, $3d25bdd8, $e2e1c3c9, $44421659,
      $0a121386, $d90cec6e, $d5abea2a, $64af674e, $da86a85f, $bebfe988, $64e4c3fe,
      $9dbc8057, $f0f7c086, $60787bf8, $6003604d, $d1fd8346, $f6381fb0, $7745ae04,
      $d736fccc, $83426b33, $f01eab71, $b0804187, $3c005e5f, $77a057be, $bde8ae24,
      $55464299, $bf582e61, $4e58f48f, $f2ddfda2, $f474ef38, $8789bdc2, $5366f9c3,
      $c8b38e74, $b475f255, $46fcd9b9, $7aeb2661, $8b1ddf84, $846a0e79, $915f95e2,
      $466e598e, $20b45770, $8cd55591, $c902de4c, $b90bace1, $bb8205d0, $11a86248,
      $7574a99e, $b77f19b6, $e0a9dc09, $662d09a1, $c4324633, $e85a1f02, $09f0be8c,
      $4a99a025, $1d6efe10, $1ab93d1d, $0ba5a4df, $a186f20f, $2868f169, $dcb7da83,
      $573906fe, $a1e2ce9b, $4fcd7f52, $50115e01, $a70683fa, $a002b5c4, $0de6d027,
      $9af88c27, $773f8641, $c3604c06, $61a806b5, $f0177a28, $c0f586e0, $006058aa,
      $30dc7d62, $11e69ed7, $2338ea63, $53c2dd94, $c2c21634, $bbcbee56, $90bcb6de,
      $ebfc7da1, $ce591d76, $6f05e409, $4b7c0188, $39720a3d, $7c927c24, $86e3725f,
      $724d9db9, $1ac15bb4, $d39eb8fc, $ed545578, $08fca5b5, $d83d7cd3, $4dad0fc4,
      $1e50ef5e, $b161e6f8, $a28514d9, $6c51133c, $6fd5c7e7, $56e14ec4, $362abfce,
      $ddc6c837, $d79a3234, $92638212, $670efa8e, $406000e0, $3a39ce37, $d3faf5cf,
      $abc27737, $5ac52d1b, $5cb0679e, $4fa33742, $d3822740, $99bc9bbe, $d5118e9d,
      $bf0f7315, $d62d1c7e, $c700c47b, $b78c1b6b, $21a19045, $b26eb1be, $6a366eb4,
      $5748ab2f, $bc946e79, $c6a376d2, $6549c2c8, $530ff8ee, $468dde7d, $d5730a1d,
      $4cd04dc6, $2939bbdb, $a9ba4650, $ac9526e8, $be5ee304, $a1fad5f0, $6a2d519a,
      $63ef8ce2, $9a86ee22, $c089c2b8, $43242ef6, $a51e03aa, $9cf2d0a4, $83c061ba,
      $9be96a4d, $8fe51550, $ba645bd6, $2826a2f9, $a73a3ae1, $4ba99586, $ef5562e9,
      $c72fefd3, $f752f7da, $3f046f69, $77fa0a59, $80e4a915, $87b08601, $9b09e6ad,
      $3b3ee593, $e990fd5a, $9e34d797, $2cf0b7d9, $022b8b51, $96d5ac3a, $017da67d,
      $d1cf3ed6, $7c7d2d28, $1f9f25cf, $adf2b89b, $5ad6b472, $5a88f54c, $e029ac71,
      $e019a5e6, $47b0acfd, $ed93fa9b, $e8d3c48d, $283b57cc, $f8d56629, $79132e28,
      $785f0191, $ed756055, $f7960e44, $e3d35e8c, $15056dd4, $88f46dba, $03a16125,
      $0564f0bd, $c3eb9e15, $3c9057a2, $97271aec, $a93a072a, $1b3f6d9b, $1e6321f5,
      $f59c66fb, $26dcf319, $7533d928, $b155fdf5, $03563482, $8aba3cbb, $28517711,
      $c20ad9f8, $abcc5167, $ccad925f, $4de81751, $3830dc8e, $379d5862, $9320f991,
      $ea7a90c2, $fb3e7bce, $5121ce64, $774fbe32, $a8b6e37e, $c3293d46, $48de5369,
      $6413e680, $a2ae0810, $dd6db224, $69852dfd, $09072166, $b39a460a, $6445c0dd,
      $586cdecf, $1c20c8ae, $5bbef7dd, $1b588d40, $ccd2017f, $6bb4e3bb, $dda26a7e,
      $3a59ff45, $3e350a44, $bcb4cdd5, $72eacea8, $fa6484bb, $8d6612ae, $bf3c6f47,
      $d29be463, $542f5d9e, $aec2771b, $f64e6370, $740e0d8d, $e75b1357, $f8721671,
      $af537d5d, $4040cb08, $4eb4e2cc, $34d2466a, $0115af84, $e1b00428, $95983a1d,
      $06b89fb4, $ce6ea048, $6f3f3b82, $3520ab82, $011a1d4b, $277227f8, $611560b1,
      $e7933fdc, $bb3a792b, $344525bd, $a08839e1, $51ce794b, $2f32c9b7, $a01fbac9,
      $e01cc87e, $bcc7d1f6, $cf0111c3, $a1e8aac7, $1a908749, $d44fbd9a, $d0dadecb,
      $d50ada38, $0339c32a, $c6913667, $8df9317c, $e0b12b4f, $f79e59b7, $43f5bb3a,
      $f2d519ff, $27d9459c, $bf97222c, $15e6fc2a, $0f91fc71, $9b941525, $fae59361,
      $ceb69ceb, $c2a86459, $12baa8d1, $b6c1075e, $e3056a0c, $10d25065, $cb03a442,
      $e0ec6e0e, $1698db3b, $4c98a0be, $3278e964, $9f1f9532, $e0d392df, $d3a0342b,
      $8971f21e, $1b0a7441, $4ba3348c, $c5be7120, $c37632d8, $df359f8d, $9b992f2e,
      $e60b6f47, $0fe3f11d, $e54cda54, $1edad891, $ce6279cf, $cd3e7e6f, $1618b166,
      $fd2c1d05, $848fd2c5, $f6fb2299, $f523f357, $a6327623, $93a83531, $56cccd02,
      $acf08162, $5a75ebb5, $6e163697, $88d273cc, $de966292, $81b949d0, $4c50901b,
      $71c65614, $e6c6c7bd, $327a140a, $45e1d006, $c3f27b9a, $c9aa53fd, $62a80f00,
      $bb25bfe2, $35bdd2f6, $71126905, $b2040222, $b6cbcf7c, $cd769c2b, $53113ec0,
      $1640e3d3, $38abbd60, $2547adf0, $ba38209c, $f746ce76, $77afa1c5, $20756060,
      $85cbfe4e, $8ae88dd8, $7aaaf9b0, $4cf9aa7e, $1948c25c, $02fb8a8c, $01c36ae4,
      $d6ebe1f9, $90d4f869, $a65cdea0, $3f09252d, $c208e69f, $b74e6132, $ce77e25b,
      $578fdfe3, $3ac372e6));

{$ifdef OSLINUXX64} // this asm is only marginally faster than pure pascal code

// result := (((s[(x shr 24)] + s[$100 + ToByte(x shr 16)]) xor
//           s[$200 + ToByte(x shr 8)]) + s[$300 + ToByte(x)]);

function BlowFishStep(x: cardinal; s: PCardinalArray): cardinal;
{$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif}
        // edi=x rsi=s
        mov     ecx, edi
        mov     edx, edi
        shr     edi, 24
        shr     ecx, 16
        mov     eax, dword ptr [rsi + rdi * 4].TBlowFishState.SBox
        movzx   ecx, cl
        add     eax, dword ptr [rsi + rcx * 4 + $400].TBlowFishState.SBox
        movzx   ecx, dh
        xor     eax, dword ptr [rsi + rcx * 4 + $800].TBlowFishState.SBox
        movzx   ecx, dl
        add     eax, dword ptr [rsi + rcx * 4 + $c00].TBlowFishState.SBox
end;

procedure BlowFishEncrypt64(const s: TBlowFishState; block: PQWordRec); inline;
var
  L, R: cardinal;
  i: PtrUInt;
begin
  L := block.L;
  R := block.H;
  i := 0;
  repeat
    L := L xor s.PBox[i];
    inc(i);
    R := R xor BlowFishStep(L, @s);
    R := R xor s.PBox[i];
    inc(i);
    L := L xor BlowFishStep(R, @s);
  until i = 16;
  block.L := R xor s.PBox[17];
  block.H := L xor s.PBox[16];
end;

{$else}

procedure BlowFishEncrypt64(const s: TBlowFishState; block: PQWordRec);
var
  L, R: cardinal;
begin
  L := block.L xor s.PBox[0];
  R := block.H;
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[ 1];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[ 2];
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[ 3];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[ 4];
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[ 5];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[ 6];
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[ 7];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[ 8];
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[ 9];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[10];
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[11];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[12];
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[13];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[14];
  R := R xor (((s.SBox[(L shr 24)] + s.SBox[$100 + ToByte(L shr 16)]) xor
       s.SBox[$200 + ToByte(L shr 8)]) + s.SBox[$300 + ToByte(L)]) xor s.PBox[15];
  L := L xor (((s.SBox[(R shr 24)] + s.SBox[$100 + ToByte(R shr 16)]) xor
       s.SBox[$200 + ToByte(R shr 8)]) + s.SBox[$300 + ToByte(R)]) xor s.PBox[16];
  block.L := R xor s.PBox[17];
  block.H := L;
end;

{$endif OSLINUXX64}

// XOR all PBox[] with the encryption key - supplied as multiple of 64-bit
procedure ExpandKey(pbox: PQwordArray; key: PQwordArray; keyblocks: PtrUInt);
  {$ifdef HASINLINE} inline; {$endif}
var
  i, ndx: PtrUInt;
begin
  ndx := 0;
  for i := 0 to 8 do
  begin
    pbox[i] := pbox[i] xor key[ndx]; // key is already big-endian
    inc(ndx);
    if ndx = keyblocks then
      ndx := 0;
  end;
end;

procedure BlowFishKeySetup(var State: TBlowFishState;
  Salt, Key: PQwordArray; KeyBlocks: PtrUInt);
var
  i, ndx: PtrUInt;
  iv: QWord;
begin
  // fill PBox (Subkeys) and SBox (Subtitution Boxes) with the hex digits of pi
  MoveFast(BLOWFISH_INIT, State, SizeOf(State));
  // expand the Key to PBox
  ExpandKey(@State.PBox, Key, KeyBlocks);
  // expand the Salt to PBox + SBox (all at once)
  ndx := 0;
  iv := Salt[0];
  for i := 0 to 8 + 512 do
  begin
    BlowFishEncrypt64(State, @iv);
    PQWordArray(@State.PBox)[i] := iv;
    ndx := ndx xor 1; // toggle between 0/1/0/1 of the 2*64-bit Salt
    iv := iv xor Salt[ndx];
  end;
end;

function BlowFishPrepareKey(const Password: RawUtf8; Salt: PHash128Rec;
  out Key: RawByteString): PtrInt;
var
  p: PUtf8Char;
  plen, n: PtrInt;
begin
  // repeat password+#0 until it fits exactly in 64-bit chunks
  plen := length(Password);
  result := plen + 1;
  n := 1;
  while (result and 7 <> 0) and
        (result < BLOWFISH_MAXKEYLEN) do
  begin
    inc(n);
    inc(result, plen + 1);
  end;
  p := FastNewRawByteString(Key, result);
  repeat
    MoveFast(pointer(Password)^, p^, plen);
    p[plen] := #0;
    inc(p, plen + 1);
    dec(n);
  until n = 0;
  if result > BLOWFISH_MAXKEYLEN then
    result := BLOWFISH_MAXKEYLEN; // in-place truncation to 72 bytes
  // prepare Salt and Key to be in Big-Endian format
  bswap32array(pointer(Key), result shr 2);
  bswap32array(pointer(Salt), BLOWFISH_SALTLEN shr 2);
  result := result shr 3; // return the number of 64-bit blocks
end;

procedure BlowFishKeySetup(var State: TBlowFishState;
  Salt: PHash128Rec; const Password: RawUtf8);
var
  key: RawByteString;
  blocks: PtrInt;
begin
  blocks := BlowFishPrepareKey(Password, Salt, key);
  BlowFishKeySetup(State, pointer(Salt), pointer(key), blocks);
  FillZero(key); // anti-forensic
end;

procedure BlowFishKeyClear(var State: TBlowFishState);
begin
  FillCharFast(State.PBox, SizeOf(State.PBox), 0); // it is enough to fill PBox
end;

{$ifdef CPUINTEL}
{$ifdef CPUX86}
procedure BlowFishCtrInc(iv: PQWord);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
@1:     mov     ecx, dword ptr [eax]
        mov     edx, dword ptr [eax + 4]
        bswap   ecx
        bswap   edx
        add     edx, 1
        adc     ecx, 0
        bswap   ecx
        bswap   edx
        mov     dword ptr [eax], ecx
        mov     dword ptr [eax + 4], edx
end;
{$else}
procedure BlowFishCtrInc(iv: PQWord);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rax, qword ptr [iv]
        bswap   rax
        add     rax, 1
        bswap   rax
        mov     qword ptr [iv], rax
end;
{$endif CPUX86}
{$else}
procedure BlowFishCtrInc(iv: PQWord);
begin
  iv^ := bswap64(bswap64(iv^) + 1);
end;
{$endif CPUINTEL}

procedure BlowFishEncryptCtr(src, dest: PQWord; len: PtrUInt;
  const state: TBlowFishState; iv: PQWord);
var
  n: PtrUInt;
  tmp: TQWordRec;
begin
  if PtrInt(len) <= 0 then
    exit;
  n := len shr 3;
  repeat
    tmp.V := iv^;
    BlowFishEncrypt64(state, @tmp);
    BlowFishCtrInc(iv);
    if n = 0 then
    begin
      n := len and 7;
      if n <> 0 then
        repeat // trailing 1..7 bytes
          dec(n);
          PByteArray(dest)[n] := PByteArray(src)[n] xor tmp.B[n];
        until n = 0;
      tmp.V := 0;
      exit;
    end;
    dest^ := src^ xor tmp.V;
    inc(src);
    inc(dest);
    dec(n);
  until false;
end;


{ TBlowFishCtr }

const
  BLOWFISHCTR_DEFAULTSALT: TGuid = '{C3D8BE77-1038-4E88-BD92-4DC5A2853678}';

constructor TBlowFishCtr.Create(const Password: RawByteString; Cost: byte;
  Salt: PHash128Rec);
begin
  if Cost > 31 then
    ESynCrypto.RaiseUtf8('%.Create: out of range Cost=% (<=31)', [Cost]);
  if Salt = nil then
    Salt := @BLOWFISHCTR_DEFAULTSALT; // some fixed value
  GetMem(fState, SizeOf(fState^));
  if Cost = 0 then
     BlowFishKeySetup(fState^, Salt, Password)
  else
     BCryptExpensiveKeySetup(fState^, Cost, Salt, Password);
end;

destructor TBlowFishCtr.Destroy;
begin
  fIV := 0;
  if fState <> nil then
  begin
    BlowFishKeyClear(fState^); // anti-forensic
    Freemem(fState);
  end;
  inherited Destroy;
end;

procedure TBlowFishCtr.EncryptBuffer(BufIn, BufOut: pointer; Count: cardinal);
begin
  BlowFishEncryptCtr(BufIn, BufOut, Count, fState^, @fIV);
end;

function TBlowFishCtr.Encrypt(const Input: RawByteString;
  IVAtBeginning: boolean): RawByteString;
var
  len: PtrInt;
  d, piv: PQWord;
  tmpiv: QWord;
begin
  result := '';
  len := length(Input);
  if len = 0 then
    exit;
  d := FastNewRawByteString(result, len + PtrInt(ord(IVAtBeginning)) shl 3);
  piv := @fIV; // update the main IV by default
  if IVAtBeginning then
  begin
    piv := @tmpiv; // use a local IV on stack to be thread-safe
    d^ := TAesPrng.Main.Random64; // even a small IV benefits from CSPRNG
    piv^ := d^;
    inc(d);
  end;
  BlowFishEncryptCtr(pointer(Input), d, len, fState^, piv);
  if IVAtBeginning then
    tmpiv := 0; // anti-forensic
end;

function TBlowFishCtr.Decrypt(const Input: RawByteString;
  IVAtBeginning: boolean): RawByteString;
var
  len: PtrInt;
  s, piv: PQWord;
  tmpiv: QWord;
begin
  result := '';
  len := length(Input);
  if len = 0 then
    exit;
  s := pointer(Input);
  piv := @fIV; // update the main IV by default
  if IVAtBeginning then
  begin
    dec(len, SizeOf(s^));
    if len <= 0 then
      exit;
    piv := @tmpiv; // use a local IV on stack to be thread-safe
    piv^ := s^;
    inc(s);
  end;
  BlowFishEncryptCtr(s, FastSetString(RawUtf8(result), len), len, fState^, piv);
  if IVAtBeginning then
    tmpiv := 0; // anti-forensic
end;


{ **************** BCrypt Password-Hashing Function }

// dedicated BlowFishKeySetup() with zeros salt as used during bcrypt rounds
procedure BCryptExpensiveRound(var state: TBlowFishState;
  key: PQwordArray; keyblocks: PtrUInt);
var
  iv: QWord;
  i: PtrUInt;
begin
  ExpandKey(@state.PBox, key, keyblocks);
  iv := 0;
  for i := 0 to 8 + 512 do
  begin
    BlowFishEncrypt64(state, @iv);
    PQWordArray(@state.PBox)[i] := iv;
  end;
end;

procedure BCryptExpensiveKeySetup(var State: TBlowFishState;
  Cost: byte; Salt: PHash128Rec; const Password: RawUtf8);
var
  i, blocks: PtrUInt;
  key: RawByteString;
begin
  if (Cost < 4) or
     (Cost > 31) then
    ESynCrypto.RaiseUtf8('BCrypt: invalid Cost (4<=%<=31)', [Cost]);
  if Salt = nil then
    ESynCrypto.RaiseU('BCrypt: missing Salt');
  // prepare the 64-bit padded binary key from the supplied Password
  blocks := BlowFishPrepareKey(Password, Salt, key);
  // permute PBox and SBox based on the password and salt - the BlowFish setup
  BlowFishKeySetup(State, pointer(Salt), pointer(key), blocks);
  // this is the "Expensive" part of the "Expensive Key Setup"
  for i := 1 to (1 shl Cost) do
  begin
    BCryptExpensiveRound(State, pointer(key), blocks);
    BCryptExpensiveRound(State, pointer(Salt), BCRYPT_SALTLEN shr 3);
  end;
  // anti-forensic measure
  FillZero(key);
end;

const
  // big-endian 24 bytes 'OrpheanBeholderScryDoubt' message to be ciphered
  OBSD_MAGIC: array[0..5] of cardinal= (
    $4f727068, $65616e42, $65686f6c, $64657253, $63727944, $6f756274);

  // yet another base-64 alphabet!
  HASH64_ENC: TChar64 =
    './ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
var
  HASH64_DEC: TAnsiCharDec;

function BCryptHash(const Password, Salt: RawUtf8; Cost: byte;
  HashPos: PInteger; PreSha256: boolean): RawUtf8;
var
  state: TBlowFishState;
  dig: THash256Rec;
  n: cardinal;
  saltbin, saltb64: RawByteString;
  hash, pwd: RawUtf8;
begin
  FastAssignNew(result);
  // decode the supplied salt or generate a new one
  if HASH64_DEC[#255] = 0 then // check the last byte for thread-safe init
    FillBaseDecoder(@HASH64_ENC, @HASH64_DEC, high(HASH64_ENC));
  if not TAesPrng.Main.RandomSalt(
           saltbin, saltb64, BCRYPT_SALTLEN, Salt, @HASH64_ENC, @HASH64_DEC) or
         (length(saltbin) <> BCRYPT_SALTLEN) then // always 16 bytes
    exit;
  // initialize BlowFish state from Password using BCrypt "Expensive Key Setup"
  if Cost = 0 then
    Cost := 12; // default cost - e.g. from plain ModularCryptHash() wrapper
  if PreSha256 then
  begin
    // https://passlib.readthedocs.io/en/stable/lib/passlib.hash.bcrypt_sha256.html
    HmacSha256(saltb64, Password, dig.b);
    pwd := BinToBase64(@dig, SizeOf(dig)); // standard Base-64 encoding
    BCryptExpensiveKeySetup(state, Cost, pointer(saltbin), pwd);
    FillZero(pwd);
    Make(['$bcrypt-sha256$v=2,t=2b,r=', Cost, '$', saltb64, '$'], result);
    // note: PHP and Node.js use the regular $2b$/$2y$ format with pre-hashing
    // which does not fulfill the explicitness of the "Modular Crypt" format
  end
  else
  begin
    // regular BCrypt with password truncation to 72 bytes
    BCryptExpensiveKeySetup(state, Cost, pointer(saltbin), Password);
    Make(['$2b$', UInt2DigitsToShort(Cost), '$', saltb64], result);
  end;
  if HashPos <> nil then
    HashPos^ := length(result) + 1; // there is no '$' before the $2b$ {checksum}
  FillZero(saltbin);
  FillZero(saltb64);
  // encrypt the 'O..B..S..D..' magic text 64 times
  MoveFast(OBSD_MAGIC, dig.q, SizeOf(OBSD_MAGIC));
  for n := 1 to 64 do
  begin
    BlowFishEncrypt64(state, @dig.q[0]);
    BlowFishEncrypt64(state, @dig.q[1]);
    BlowFishEncrypt64(state, @dig.q[2]);
  end;
  BlowFishKeyClear(state); // paranoid
  bswap32array(@dig, SizeOf(OBSD_MAGIC) shr 2);
  // truncated to 23 bytes = 31 chars for compatibility with original OpenBSD
  Base64uriEncode(FastSetString(hash, 31), @dig, 23, @HASH64_ENC);
  Append(result, hash);
  FillZero(dig.b);
  FillZero(hash);
end;


{ **************** SCrypt Password-Hashing Function }

{$ifdef CPUINTEL}
{$ifdef CPUX64}

{$ifdef FPC}
  {$WARN 7105 off : Use of -offset(%esp) }
  {$WARN 7121 off : Check size of memory operand }
  {$WARN 7122 off : Check size of memory operand }
{$endif FPC}

procedure Salsa20x8(B: PCardinalArray);
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        push    rbp
        push    r15
        push    r14
        push    r13
        push    r12
        push    rbx
        sub     rsp, 40
        movups  xmm0, dqword ptr [B]
        movups  xmm1, dqword ptr [B + 16]
        movups  xmm2, dqword ptr [B + 32]
        movups  xmm3, dqword ptr [B + 48]
        mov     qword ptr [rsp - 32], B
        movaps  [rsp - 128], xmm0
        movaps  [rsp - 80],  xmm3
        movaps  [rsp - 112], xmm1
        movaps  [rsp - 96],  xmm2
        mov     r11d, dword ptr [rsp - 128]
        mov     r8d, dword ptr [rsp - 124]
        mov     ecx, dword ptr [rsp - 80]
        mov     edi, dword ptr [rsp - 76]
        mov     eax, dword ptr [rsp - 112]
        mov     r15d, dword ptr [rsp - 108]
        mov     r12d, dword ptr [rsp - 96]
        mov     ebp, dword ptr [rsp - 92]
        mov     ebx, dword ptr [rsp - 88]
        mov     r9d, dword ptr [rsp - 104]
        mov     r13d, dword ptr [rsp - 72]
        mov     edx, dword ptr [rsp - 120]
        mov     qword ptr [rsp - 48], rdx
        mov     r14d, dword ptr [rsp - 68]
        mov     r10d, dword ptr [rsp - 84]
        mov     edx, dword ptr [rsp - 116]
        mov     qword ptr [rsp - 56], rdx
        mov     qword ptr [rsp - 40], 4
        mov     esi, dword ptr [rsp - 100]
        {$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     lea     edx, [rcx + r11]
        rol     edx, 7
        xor     edx, eax
        mov     qword ptr [rsp], rdx
        lea     eax, [rdx + r11]
        rol     eax, 9
        xor     eax, r12d
        mov     r12, rax
        mov     qword ptr [rsp + 24], rax
        add     eax, edx
        rol     eax, 13
        xor     eax, ecx
        mov     qword ptr [rsp + 32], rax
        add     r12d, eax
        rol     r12d, 18
        lea     eax, [r8 + r15]
        rol     eax, 7
        xor     eax, ebp
        mov     qword ptr [rsp + 8], rax
        lea     ecx, [rax + r15]
        rol     ecx, 9
        xor     ecx, edi
        mov     qword ptr [rsp + 16], rcx
        add     eax, ecx
        rol     eax, 13
        xor     eax, r8d
        lea     ebp, [rax + rcx]
        rol     ebp, 18
        xor     r12d, r11d
        lea     ecx, [r9 + rbx]
        rol     ecx, 7
        xor     ecx, r13d
        mov     qword ptr [rsp - 8], rcx
        lea     r11d, [rcx + rbx]
        rol     r11d, 9
        xor     r11d, dword ptr [rsp - 48]
        add     ecx, r11d
        rol     ecx, 13
        xor     ecx, r9d
        lea     edx, [rcx + r11]
        rol     edx, 18
        xor     ebp, r15d
        lea     r9d, [r10 + r14]
        rol     r9d, 7
        xor     r9d, dword ptr [rsp - 56]
        lea     r15d, [r9 + r14]
        rol     r15d, 9
        xor     r15d, esi
        lea     edi, [r15 + r9]
        rol     edi, 13
        xor     edi, r10d
        lea     r13d, [rdi + r15]
        rol     r13d, 18
        xor     edx, ebx
        lea     r8d, [r9 + r12]
        rol     r8d, 7
        xor     r8d, eax
        lea     eax, [r8 + r12]
        rol     eax, 9
        xor     eax, r11d
        lea     esi, [rax + r8]
        rol     esi, 13
        xor     esi, r9d
        mov     qword ptr [rsp - 56], rsi
        mov     qword ptr [rsp - 48], rax
        lea     r11d, [rsi + rax]
        rol     r11d, 18
        xor     r13d, r14d
        mov     r10, qword ptr [rsp]
        lea     r9d, [r10 + rbp]
        rol     r9d, 7
        xor     r9d, ecx
        lea     esi, [r9 + rbp]
        mov     rcx, rbp
        rol     esi, 9
        xor     esi, r15d
        lea     eax, [rsi + r9]
        rol     eax, 13
        xor     eax, r10d
        lea     r15d, [rax + rsi]
        rol     r15d, 18
        xor     r11d, r12d
        mov     qword ptr [rsp - 16], rdx
        mov     rbx, qword ptr [rsp + 8]
        lea     r10d, [rdx + rbx]
        rol     r10d, 7
        xor     r10d, edi
        lea     r12d, [r10 + rdx]
        rol     r12d, 9
        xor     r12d, dword ptr [rsp + 24]
        lea     ebp, [r12 + r10]
        rol     ebp, 13
        xor     ebp, ebx
        lea     ebx, [r12 + rbp]
        rol     ebx, 18
        xor     r15d, ecx
        mov     rdx, qword ptr [rsp - 8]
        mov     qword ptr [rsp - 24], r13
        lea     ecx, [rdx + r13]
        rol     ecx, 7
        xor     ecx, dword ptr [rsp + 32]
        lea     edi, [rcx + r13]
        rol     edi, 9
        xor     edi, dword ptr [rsp + 16]
        lea     r13d, [rdi + rcx]
        rol     r13d, 13
        xor     r13d, edx
        lea     r14d, [rdi + r13]
        rol     r14d, 18
        xor     ebx, dword ptr [rsp - 16]
        xor     r14d, dword ptr [rsp - 24]
        dec     byte ptr [rsp - 40]
        jnz     @s
        mov     dword ptr [rsp - 128], r11d
        mov     dword ptr [rsp - 80], ecx
        mov     dword ptr [rsp - 112], eax
        mov     dword ptr [rsp - 96], r12d
        mov     dword ptr [rsp - 108], r15d
        mov     dword ptr [rsp - 124], r8d
        mov     dword ptr [rsp - 92], ebp
        mov     dword ptr [rsp - 76], edi
        mov     dword ptr [rsp - 88], ebx
        mov     dword ptr [rsp - 104], r9d
        mov     dword ptr [rsp - 72], r13d
        mov     rax, qword ptr [rsp - 48]
        mov     dword ptr [rsp - 120], eax
        mov     dword ptr [rsp - 68], r14d
        mov     dword ptr [rsp - 84], r10d
        mov     rax, qword ptr [rsp - 56]
        mov     dword ptr [rsp - 116], eax
        mov     dword ptr [rsp - 100], esi
        mov     rcx, qword ptr [rsp - 32]
        movups  xmm0, [rcx]
        movups  xmm1, [rcx + 16]
        movups  xmm2, [rcx + 32]
        paddd   xmm0, [rsp - 128]
        movups  xmm3, [rcx + 48]
        paddd   xmm1, [rsp + 16 - 128]
        paddd   xmm2, [rsp + 32 - 128]
        paddd   xmm3, [rsp + 48 - 128]
        movups  [rcx], xmm0
        movups  [rcx + 16], xmm1
        movups  [rcx + 32], xmm2
        movups  [rcx + 48], xmm3
        add     rsp, 40
        pop     rbx
        pop     r12
        pop     r13
        pop     r14
        pop     r15
        pop     rbp
end;
{$else}
procedure Salsa20x8(B: PCardinalArray); // old i386 version without SSE2
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebp
        push    edi
        push    esi
        push    ebx
        push    eax
        sub     esp, 156
        mov     ebx, eax
        mov     eax, [ebx + 16]
        mov     edi, [ebx + 36]
        mov     ebp, [ebx + 24]
        mov     edx, [ebx + 48]
        mov     [esp + 24], eax
        mov     eax, [ebx]
        mov     [esp + 36], edi
        mov     edi, [ebx + 20]
        mov     [esp + 16], eax
        mov     eax, [ebx + 4]
        mov     [esp + 28], edi
        mov     ecx, [ebx + 32]
        mov     [esp + 20], eax
        mov     eax, [ebx + 56]
        mov     edi, [ebx + 52]
        mov     [esp + 44], eax
        mov     eax, [ebx + 40]
        mov     [esp + 48], eax
        mov     eax, [ebx + 8]
        mov     [esp + 52], eax
        mov     eax, [ebx + 12]
        mov     [esp + 56], eax
        mov     eax, [ebx + 60]
        mov     byte ptr [esp + 76], 4
        mov     [esp + 12], eax
        mov     eax, [ebx + 44]
        mov     [esp + 64], eax
        mov     eax, [ebx + 28]
        mov     [esp + 60], eax
        mov     eax, ebp
        mov     ebp, [esp + 28]
{$ifdef FPC} align 8 {$else} {$ifdef HASALIGN} .align 8 {$endif}{$endif}
@s:     mov     ebx, [esp + 16]
        mov     esi, [esp + 24]
        add     ebx, edx
        rol     ebx, 7
        xor     esi, ebx
        mov     ebx, [esp + 16]
        mov     [esp + 24], esi
        add     ebx, esi
        rol     ebx, 9
        xor     ebx, ecx
        lea     ecx, [esi + ebx]
        mov     [esp + 28], ebx
        rol     ecx, 13
        mov     esi, ecx
        mov     ecx, [esp + 20]
        xor     esi, edx
        lea     edx, [ebx + esi]
        mov     ebx, [esp + 36]
        mov     [esp + 32], esi
        ror     edx, 14
        xor     edx, [esp + 16]
        mov     [esp + 16], edx
        lea     edx, [ecx + ebp]
        rol     edx, 7
        xor     ebx, edx
        lea     edx, [ebx + ebp]
        mov     [esp + 36], ebx
        rol     edx, 9
        mov     ecx, edx
        xor     ecx, edi
        lea     esi, [ebx + ecx]
        mov     [esp + 40], ecx
        rol     esi, 13
        xor     esi, [esp + 20]
        lea     edx, [ecx + esi]
        mov     ecx, [esp + 44]
        ror     edx, 14
        xor     edx, ebp
        mov     ebp, [esp + 48]
        mov     [esp + 68], edx
        lea     edx, [ebp + 0 + eax]
        rol     edx, 7
        xor     ecx, edx
        mov     edx, ebp
        mov     edi, ecx
        add     ecx, ebp
        mov     ebp, [esp + 52]
        rol     ecx, 9
        mov     [esp + 44], edi
        xor     ecx, ebp
        mov     ebp, edi
        add     ebp, ecx
        rol     ebp, 13
        xor     ebp, eax
        lea     eax, [ecx + ebp]
        mov     [esp + 48], ebp
        mov     ebp, [esp + 64]
        ror     eax, 14
        xor     edx, eax
        mov     [esp + 72], edx
        mov     edx, [esp + 12]
        lea     eax, [edx + ebp]
        rol     eax, 7
        xor     eax, [esp + 56]
        add     edx, eax
        rol     edx, 9
        xor     edx, [esp + 60]
        lea     ebx, [eax + edx]
        rol     ebx, 13
        xor     ebx, ebp
        mov     ebp, [esp + 12]
        lea     edi, [edx + ebx]
        ror     edi, 14
        xor     ebp, edi
        mov     [esp + 12], ebp
        mov     ebp, [esp + 16]
        lea     edi, [ebp + 0 + eax]
        rol     edi, 7
        xor     edi, esi
        lea     esi, [ebp + 0 + edi]
        mov     [esp + 20], edi
        rol     esi, 9
        xor     esi, ecx
        lea     ecx, [edi + esi]
        mov     [esp + 52], esi
        rol     ecx, 13
        mov     edi, ecx
        xor     edi, eax
        lea     eax, [esi + edi]
        mov     [esp + 56], edi
        mov     edi, [esp + 24]
        ror     eax, 14
        xor     ebp, eax
        mov     [esp + 16], ebp
        mov     ebp, [esp + 68]
        mov     esi, [esp + 72]
        lea     eax, [edi + ebp]
        rol     eax, 7
        xor     eax, [esp + 48]
        lea     ecx, [ebp + 0 + eax]
        rol     ecx, 9
        xor     ecx, edx
        lea     edx, [eax + ecx]
        mov     [esp + 60], ecx
        rol     edx, 13
        xor     edi, edx
        lea     edx, [ecx + edi]
        mov     [esp + 24], edi
        mov     edi, [esp + 36]
        ror     edx, 14
        xor     ebp, edx
        lea     edx, [edi + esi]
        rol     edx, 7
        xor     edx, ebx
        mov     ebx, [esp + 12]
        lea     ecx, [esi + edx]
        mov     [esp + 64], edx
        rol     ecx, 9
        xor     ecx, [esp + 28]
        lea     edx, [edx + ecx]
        rol     edx, 13
        xor     edi, edx
        lea     edx, [ecx + edi]
        mov     [esp + 36], edi
        ror     edx, 14
        xor     esi, edx
        mov     [esp + 48], esi
        mov     esi, [esp + 44]
        lea     edx, [esi + ebx]
        rol     edx, 7
        xor     edx, [esp + 32]
        lea     edi, [ebx + edx]
        rol     edi, 9
        xor     edi, [esp + 40]
        lea     ebx, [edx + edi]
        rol     ebx, 13
        xor     esi, ebx
        lea     ebx, [edi + esi]
        mov     [esp + 44], esi
        mov     esi, [esp + 12]
        ror     ebx, 14
        xor     esi, ebx
        mov     [esp + 12], esi
        dec     byte ptr [esp + 76]
        jne     @s
        mov     [esp + 28], ebp
        mov     [esp + 104], eax
        mov     eax, [esp + 20]
        mov     [esp + 112], ecx
        mov     [esp + 84], eax
        mov     eax, [esp + 52]
        mov     [esp + 132], edi
        mov     [esp + 88], eax
        mov     eax, [esp + 56]
        mov     [esp + 128], edx
        mov     [esp + 92], eax
        mov     eax, [esp + 16]
        mov     [esp + 80], eax
        mov     eax, [esp + 60]
        mov     [esp + 108], eax
        mov     eax, [esp + 24]
        mov     [esp + 96], eax
        mov     eax, [esp + 28]
        mov     [esp + 100], eax
        mov     eax, [esp + 64]
        mov     [esp + 124], eax
        mov     eax, [esp + 36]
        mov     [esp + 116], eax
        mov     eax, [esp + 48]
        mov     [esp + 120], eax
        mov     eax, [esp + 44]
        mov     [esp + 136], eax
        mov     eax, [esp + 12]
        mov     [esp + 140], eax
        lea     edx, [esp + 80]
        add     esp, 156
        pop     ebx
        xor     esi, esi
        {$ifdef FPC} align 8 {$else} {$ifdef HASALIGN} .align 8 {$endif}{$endif}
@1:     mov     eax, [edx + esi]
        add     [ebx + esi], eax
        add     esi, 4
        cmp     esi, 64
        jb      @1
        pop     ebx
        pop     esi
        pop     edi
        pop     ebp
end;
{$endif CPUX64}
{$else}
procedure Salsa20x8(B: PCardinalArray);
var
  x: TBlock512;
  i: PtrUInt;
begin // single B parameter keep the stack small and all offsets in [rsp+0..$7f]
  x := PBlock512(B)^;
  for i := 1 to 4 do
  begin
    x[4]  := x[4]  xor RolDWord(x[0]  + x[12], 7); // RoldDWord() intrinsic FPC
    x[8]  := x[8]  xor RolDWord(x[4]  + x[0],  9);
    x[12] := x[12] xor RolDWord(x[8]  + x[4],  13);
    x[0]  := x[0]  xor RolDWord(x[12] + x[8],  18);
    x[9]  := x[9]  xor RolDWord(x[5]  + x[1],  7);
    x[13] := x[13] xor RolDWord(x[9]  + x[5],  9);
    x[1]  := x[1]  xor RolDWord(x[13] + x[9],  13);
    x[5]  := x[5]  xor RolDWord(x[1]  + x[13], 18);
    x[14] := x[14] xor RolDWord(x[10] + x[6],  7);
    x[2]  := x[2]  xor RolDWord(x[14] + x[10], 9);
    x[6]  := x[6]  xor RolDWord(x[2]  + x[14], 13);
    x[10] := x[10] xor RolDWord(x[6]  + x[2],  18);
    x[3]  := x[3]  xor RolDWord(x[15] + x[11], 7);
    x[7]  := x[7]  xor RolDWord(x[3]  + x[15], 9);
    x[11] := x[11] xor RolDWord(x[7]  + x[3],  13);
    x[15] := x[15] xor RolDWord(x[11] + x[7],  18);
    x[1]  := x[1]  xor RolDWord(x[0]  + x[3],  7);
    x[2]  := x[2]  xor RolDWord(x[1]  + x[0],  9);
    x[3]  := x[3]  xor RolDWord(x[2]  + x[1],  13);
    x[0]  := x[0]  xor RolDWord(x[3]  + x[2],  18);
    x[6]  := x[6]  xor RolDWord(x[5]  + x[4],  7);
    x[7]  := x[7]  xor RolDWord(x[6]  + x[5],  9);
    x[4]  := x[4]  xor RolDWord(x[7]  + x[6],  13);
    x[5]  := x[5]  xor RolDWord(x[4]  + x[7],  18);
    x[11] := x[11] xor RolDWord(x[10] + x[9],  7);
    x[8]  := x[8]  xor RolDWord(x[11] + x[10], 9);
    x[9]  := x[9]  xor RolDWord(x[8]  + x[11], 13);
    x[10] := x[10] xor RolDWord(x[9]  + x[8],  18);
    x[12] := x[12] xor RolDWord(x[15] + x[14], 7);
    x[13] := x[13] xor RolDWord(x[12] + x[15], 9);
    x[14] := x[14] xor RolDWord(x[13] + x[12], 13);
    x[15] := x[15] xor RolDWord(x[14] + x[13], 18);
  end;
  for i := 0 to 15 do
    inc(B[i], x[i]);
end;
{$endif CPUINTEL}

{$ifdef CPUSSE2}

// our SSE2 optimized version for i386 and x86_64 - faster than OpenSSL
{
   Default layout:     SSE2 layout:
     0  1  2  3         0  5 10 15
     4  5  6  7        12  1  6 11
     8  9 10 11         8 13  2  7
    12 13 14 15         4  9 14  3
}
procedure SPrepareSse2(blocks: PCardinalArray; count: cardinal);
var
  c: cardinal;
begin
  repeat
    c := blocks[1]; blocks[1] := blocks[5];  blocks[5]  := c;
    c := blocks[2]; blocks[2] := blocks[10]; blocks[10] := c;
    c := blocks[3]; blocks[3] := blocks[15]; blocks[15] := c;
    c := blocks[4]; blocks[4] := blocks[12]; blocks[12] := c;
    c := blocks[7]; blocks[7] := blocks[11]; blocks[11] := c;
    c := blocks[9]; blocks[9] := blocks[13]; blocks[13] := c;
    blocks := @blocks[16];
    dec(count);
  until count = 0;
end;

{$ifdef CPUX64}
procedure SBlockMix(dst, src, bxor: pointer; R: PtrUInt);
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        // rcx/rdi=dst rdx/rsi=src r8/rdx=BXor r9/rcx=R
        {$ifdef WIN64ABI}
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        mov     rdi, rcx
        mov     rsi, rdx
        mov     rdx, r8
        mov     rcx, r9
        {$endif WIN64ABI}
        shl     rcx, 7
        lea     rax, [rsi + rcx - 40H]
        lea     r9,  [rdx + rcx - 40H]
        movaps  xmm0, [rax]
        movaps  xmm1, [rax + 10H]
        movaps  xmm2, [rax + 20H]
        movaps  xmm3, [rax + 30H]
        test    rdx, rdx
        jz      @no1
        pxor    xmm0, [r9]
        pxor    xmm1, [r9 + 10H]
        pxor    xmm2, [r9 + 20H]
        pxor    xmm3, [r9 + 30H]
@no1:   xor     r9, r9
        xor     r8, r8
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@loop:  pxor    xmm0, [rsi + r9]
        pxor    xmm1, [rsi + r9 + 10H]
        pxor    xmm2, [rsi + r9 + 20H]
        pxor    xmm3, [rsi + r9 + 30H]
        test     rdx, rdx
        jz      @no2
        pxor    xmm0, [rdx + r9]
        pxor    xmm1, [rdx + r9 + 10H]
        pxor    xmm2, [rdx + r9 + 20H]
        pxor    xmm3, [rdx + r9 + 30H]
@no2:   movaps  xmm8, xmm0
        movaps  xmm9, xmm1
        movaps  xmm10, xmm2
        movaps  xmm11, xmm3
        mov     rax, 8
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     movaps  xmm4, xmm1
        paddd   xmm4, xmm0
        movaps  xmm5, xmm4
        pslld   xmm4, 7
        psrld   xmm5, 25
        pxor    xmm3, xmm4
        movaps  xmm4, xmm0
        pxor    xmm3, xmm5
        paddd   xmm4, xmm3
        movaps  xmm5, xmm4
        pslld   xmm4, 9
        psrld   xmm5, 23
        pxor    xmm2, xmm4
        movaps  xmm4, xmm3
        pxor    xmm2, xmm5
        pshufd  xmm3, xmm3, 93H
        paddd   xmm4, xmm2
        movaps  xmm5, xmm4
        pslld   xmm4, 13
        psrld   xmm5, 19
        pxor    xmm1, xmm4
        movaps  xmm4, xmm2
        pxor    xmm1, xmm5
        pshufd  xmm2, xmm2, 4EH
        paddd   xmm4, xmm1
        movaps  xmm5, xmm4
        pslld   xmm4, 18
        psrld   xmm5, 14
        pxor    xmm0, xmm4
        movaps  xmm4, xmm3
        pxor    xmm0, xmm5
        pshufd  xmm1, xmm1, 39H
        paddd   xmm4, xmm0
        movaps  xmm5, xmm4
        pslld   xmm4, 7
        psrld   xmm5, 25
        pxor    xmm1, xmm4
        movaps  xmm4, xmm0
        pxor    xmm1, xmm5
        paddd   xmm4, xmm1
        movaps  xmm5, xmm4
        pslld   xmm4, 9
        psrld   xmm5, 23
        pxor    xmm2, xmm4
        movaps  xmm4, xmm1
        pxor    xmm2, xmm5
        pshufd  xmm1, xmm1, 93H
        paddd   xmm4, xmm2
        movaps  xmm5, xmm4
        pslld   xmm4, 13
        psrld   xmm5, 19
        pxor    xmm3, xmm4
        movaps  xmm4, xmm2
        pxor    xmm3, xmm5
        pshufd  xmm2, xmm2, 4EH
        paddd   xmm4, xmm3
        sub     rax, 2
        movaps  xmm5, xmm4
        pslld   xmm4, 18
        psrld   xmm5, 14
        pxor    xmm0, xmm4
        pshufd  xmm3, xmm3, 39H
        pxor    xmm0, xmm5
        ja      @s
        paddd   xmm0, xmm8
        paddd   xmm1, xmm9
        paddd   xmm2, xmm10
        paddd   xmm3, xmm11
        lea     rax, [r8 + r9]
        xor     r8, rcx
        and     rax, -128
        add     r9, 64
        shr     rax, 1
        add     rax, rdi
        cmp     r9, rcx
        movaps  [rax], xmm0
        movaps  [rax + 10H], xmm1
        movaps  [rax + 20H], xmm2
        movaps  [rax + 30H], xmm3
        jne     @loop
        {$ifdef WIN64ABI}
        pop     rdi
        pop     rsi
        {$endif WIN64ABI}
end;

{$else}
procedure SBlockMix(dst, src, bxor: pointer; R: PtrUInt);
var
  s2, s3: THash128; // temporary storage (no xmm8 and xmm9 on 32-bit)
asm
        // eax=dst edx=src ecx=bxor stack=R
        push    ebx
        push    esi
        push    edi
        mov     ebx, R
        mov     edi, eax
        mov     esi, edx
        mov     R, ecx
        // edi=dst esi=src R=BXor ebx=R
        shl     ebx, 7
        test    ecx, ecx
        lea     eax,  [esi + ebx - 40H]
        lea     ecx,  [ecx + ebx - 40H]
        movaps  xmm0, [eax]
        movaps  xmm1, [eax + 10H]
        movaps  xmm2, [eax + 20H]
        movaps  xmm3, [eax + 30H]
        jz      @no1
        pxor    xmm0, [ecx]
        pxor    xmm1, [ecx + 10H]
        pxor    xmm2, [ecx + 20H]
        pxor    xmm3, [ecx + 30H]
@no1:   xor     ecx, ecx
        xor     edx, edx
{$ifdef FPC} align 8 {$endif}
@loop:  mov     eax, R
        pxor    xmm0, [esi + ecx]
        pxor    xmm1, [esi + ecx + 10H]
        pxor    xmm2, [esi + ecx + 20H]
        pxor    xmm3, [esi + ecx + 30H]
        test    eax, eax
        jz      @no2
        pxor    xmm0, [eax + ecx]
        pxor    xmm1, [eax + ecx + 10H]
        pxor    xmm2, [eax + ecx + 20H]
        pxor    xmm3, [eax + ecx + 30H]
@no2:   movaps  xmm6, xmm0
        movaps  xmm7, xmm1
        movups  s2, xmm2
        movups  s3, xmm3
        mov     eax, 8
{$ifdef FPC} align 8 {$endif}
@s:     movaps  xmm4, xmm1
        paddd   xmm4, xmm0
        movaps  xmm5, xmm4
        pslld   xmm4, 7
        psrld   xmm5, 25
        pxor    xmm3, xmm4
        movaps  xmm4, xmm0
        pxor    xmm3, xmm5
        paddd   xmm4, xmm3
        movaps  xmm5, xmm4
        pslld   xmm4, 9
        psrld   xmm5, 23
        pxor    xmm2, xmm4
        movaps  xmm4, xmm3
        pxor    xmm2, xmm5
        pshufd  xmm3, xmm3, 93H
        paddd   xmm4, xmm2
        movaps  xmm5, xmm4
        pslld   xmm4, 13
        psrld   xmm5, 19
        pxor    xmm1, xmm4
        movaps  xmm4, xmm2
        pxor    xmm1, xmm5
        pshufd  xmm2, xmm2, 4EH
        paddd   xmm4, xmm1
        movaps  xmm5, xmm4
        pslld   xmm4, 18
        psrld   xmm5, 14
        pxor    xmm0, xmm4
        movaps  xmm4, xmm3
        pxor    xmm0, xmm5
        pshufd  xmm1, xmm1, 39H
        paddd   xmm4, xmm0
        movaps  xmm5, xmm4
        pslld   xmm4, 7
        psrld   xmm5, 25
        pxor    xmm1, xmm4
        movaps  xmm4, xmm0
        pxor    xmm1, xmm5
        paddd   xmm4, xmm1
        movaps  xmm5, xmm4
        pslld   xmm4, 9
        psrld   xmm5, 23
        pxor    xmm2, xmm4
        movaps  xmm4, xmm1
        pxor    xmm2, xmm5
        pshufd  xmm1, xmm1, 93H
        paddd   xmm4, xmm2
        movaps  xmm5, xmm4
        pslld   xmm4, 13
        psrld   xmm5, 19
        pxor    xmm3, xmm4
        movaps  xmm4, xmm2
        pxor    xmm3, xmm5
        pshufd  xmm2, xmm2, 4EH
        paddd   xmm4, xmm3
        sub     eax, 2
        movaps  xmm5, xmm4
        pslld   xmm4, 18
        psrld   xmm5, 14
        pxor    xmm0, xmm4
        pshufd  xmm3, xmm3, 39H
        pxor    xmm0, xmm5
        ja      @s
        movups  xmm4, s2
        movups  xmm5, s3
        paddd   xmm0, xmm6
        paddd   xmm1, xmm7
        paddd   xmm2, xmm4
        paddd   xmm3, xmm5
        lea     eax, [edx + ecx]
        xor     edx, ebx
        and     eax, -128
        add     ecx, 64
        shr     eax, 1
        add     eax, edi
        cmp     ecx, ebx
        movups  [eax], xmm0
        movups  [eax + 10H], xmm1
        movups  [eax + 20H], xmm2
        movups  [eax + 30H], xmm3
        jne     @loop
        pop     edi
        pop     esi
        pop     ebx
end;
{$endif CPUX64}

procedure SMix(R, N: PtrUInt; X, Y, V: PCardinalArray);
var
  i, j, R128: PtrUInt;
  b: PByteArray;
begin
  R128 := R * 128;
  SPrepareSse2(X, R * 2);
  b := pointer(V);
  MoveFast(X^, b^, R128);
  i := 0;
  repeat
    SBlockMix(@b[R128], b, nil, R);
    b := @b[R128];
    inc(i);
  until i = N - 1;
  SBlockMix(X, b, nil, R);
  i := 0;
  repeat
    j := (X[(R * 2 - 1) * 16] and (N - 1));
    SBlockMix(Y, X, @V[j * R * 32], R);
    j := (Y[(R * 2 - 1) * 16] and (N - 1));
    SBlockMix(X, Y, @V[j * R * 32], R);
    inc(i, 2);
  until i = N;
  SPrepareSse2(X, R * 2);
end;

{$else}

procedure SBlockMix(Input, Output: PByteArray; R: PtrUInt);
var
  i: PtrUInt;
  tmp: THash512;
begin
  Move512(@tmp, @Input[(R * 2 - 1) * 64]); // may use SSE2
  i := 0;
  repeat
    Xor512(@tmp, @Input[i * 128]); // may use SSE2
    Salsa20x8(@tmp);               // in mormot.crypt.core.pas
    Move512(@Output[i * 64], @tmp);
    Xor512(@tmp, @Input[i * 128 + 64]);
    Salsa20x8(@tmp);
    Move512(@Output[i * 64 + R * 64], @tmp);
    inc(i);
  until i >= R;
end;

procedure SMix(R, N: PtrUInt; X, Y, V: PCardinalArray);
var
  i, j, R32: PtrUInt;
begin
  R32 := R * 32;
  i := 0;
  repeat
    MoveFast(X^, V[i * R32], R32 * 4);
    SBlockMix(pointer(X), pointer(Y), R);
    inc(i);
    MoveFast(Y^, V[i * R32], R32 * 4);
    SBlockMix(pointer(Y), pointer(X), R);
    inc(i);
  until i >= N;
  i := 0;
  repeat
    j := (X[(R * 2 - 1) * 16] and (N - 1));
    XorMemory(pointer(X), @V[j * R32], R32 * 4);
    SBlockMix(pointer(X), pointer(Y), R);
    j := (Y[(R * 2 - 1) * 16] and (N - 1));
    XorMemory(pointer(Y), @V[j * R32], R32 * 4);
    SBlockMix(pointer(Y), pointer(X), R);
    inc(i, 2);
  until i >= N;
end;

{$endif CPUSSE2}

function SCryptMemoryUse(N, R, P: QWord): QWord;
begin
  result := ({data=}QWord(P * R) + {X=}R + {Y=}R + {V=}QWord(N * R)) * 128;
end;

function RawSCrypt(const Password: RawUtf8; const Salt: RawByteString;
  N, R, P, DestLen: PtrUInt): RawByteString;
var
  R128: PtrUInt;
  data: RawByteString;
  XY, V: PByteArray; // allocate X[R*128] Y[R*128] and V[N*R*128]
  d: pointer;
begin
  result := '';
  // validate parameters
  R128 := R * 128;
  if (DestLen < 16) or
     (N <= 1) or
     (N >= PtrUInt(1 shl 31)) or
     (not IsPowerOfTwo(N)) or               // must be > 1 and power of 2
     (R = 0) or                             // R = blocksize
     (P = 0) or                             // P = parallel
     (QWord(R128) * N > QWord(2) shl 30) or // allow up to 2GB of RAM for V
     (R * P >= 1 shl 30) or                 // must satisfy r * p < 2^30
     (R > (MaxInt shr 8)) or
     (N > ((MaxInt shr 7) div R)) then
    exit;
  // perform the SCrypt process
  data := Pbkdf2HmacSha256(Password, Salt, 1, P * R128);
  if data = '' then
    exit;
  XY := GetMemAligned(R128 * 2); // contiguous X,Y allocation (a few KB)
  V  := GetMemAligned(R128 * N); // keep huge V as a power of two for OS call
  try
    d := pointer(data);
    repeat // no parallel execution yet
      MoveFast(d^, XY[0], R128);
      SMix(R, N, @XY[0], @XY[R128], @V[0]);
      MoveFast(XY[0], d^, R128);
      inc(PByte(d), R128);
      dec(P);
    until P = 0;
  finally
    FreeMemAligned(XY, R128 * 2); // 16-byte aligned for SSE2 movaps access
    FreeMemAligned(V, R128 * N);
  end;
  result := Pbkdf2HmacSha256(Password, data, 1, DestLen);
end;


procedure InitializeUnit;
begin
  {$ifndef PUREMORMOT2}
  assert(SizeOf(TAesFullHeader) = SizeOf(TAesBlock));
  {$endif PUREMORMOT2}
  BCrypt := @BCryptHash; // to implement mcfBCrypt in mormot.crypt.secure
  {$ifndef CPUSSE2} // our SSE2 code above is faster than OpenSSL :)
  if not Assigned(SCrypt) then // if OpenSSL is not already set
  {$endif CPUSSE2}
    SCrypt := @RawSCrypt;
end;

initialization
  InitializeUnit;

end.
