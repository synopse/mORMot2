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
  // - a TAesFullHeader is encrypted at the begining, allowing fast Key validation,
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


implementation


{ **************** Deprecated MD4 and RC4 Support }

{$ifndef FPC} // this operation is an intrinsic with the FPC compiler
function RolDWord(value: cardinal; count: integer): cardinal;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := (value shl count) or (value shr (32 - count));
end;
{$endif FPC}

procedure MD4Transform(var buf: TBlock128; const in_: TMd5In);
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
            tab[$400 + OriginalLen and $FF] xor // = TE0[]
            tab[$500 + SourceLen and $FF] xor   // = TE1[]
            tab[SomeSalt and $7FF];             // = TD0[]
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
           (pi <> nil) then
        begin
          assert(false); // Head in po^ will overflow data in pi^
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


procedure InitializeUnit;
begin
  {$ifndef PUREMORMOT2}
  assert(SizeOf(TAesFullHeader) = SizeOf(TAesBlock));
  {$endif PUREMORMOT2}
end;

initialization
  InitializeUnit;
  
end.
