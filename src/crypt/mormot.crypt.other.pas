/// Framework Core Complementary Cryptographic Algorithms
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.other;

{
  *****************************************************************************

   Deprecated or Seldom Used Cryptographic Features
    - Deprecated MD4 and RC4 Support

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
procedure InitMd4(var Engine: TMd5);

/// direct MD4 hash calculation of some data
function Md4Buf(const Buffer; Len: cardinal): TMd5Digest;

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



implementation


{ **************** Deprecated MD4 and RC4 Support }

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

procedure InitMd4(var Engine: TMd5);
begin
  Engine.Init(@MD4Transform);
end;

function Md4Buf(const Buffer; Len: cardinal): THash128;
var
  md: TMd5;
begin
  InitMd4(md);
  md.Update(Buffer, Len);
  md.Final(result);
end;

function Md4(const s: RawByteString): RawUtf8;
var
  dig: TMd5Digest;
begin
  dig := Md4Buf(pointer(s), Length(s));
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



procedure InitializeUnit;
begin
end;

initialization
  InitializeUnit;
  
end.
