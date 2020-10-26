/// regression tests for mormot.core.crypto/secure/jwt units
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.crypto;

interface

{$I ..\src\mormot.defines.inc}

uses
  mormot.core.test,
  mormot.core.base,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.crypto,
  mormot.core.secure,
  mormot.core.jwt;

type
  /// regression tests for mormot.core.crypto and mormot.core.jwt features
  TTestCoreCrypto = class(TSynTestCase)
  published
    procedure _TSHA1;
    procedure _TSHA256;
    procedure _TRC4;
    procedure _TAES;
  end;

implementation



{ TTestCoreCrypto }

function SingleTest(const s: RawByteString; TDig: TSHA1Digest): boolean; overload;
var
  SHA: TSHA1;
  Digest: TSHA1Digest;
  i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s), length(s), Digest);
  result := IsEqual(Digest, TDig);
  if not result then
    exit;
  // 2. one update call for all chars
  for i := 1 to length(s) do
    SHA.Update(@s[i], 1);
  SHA.Final(Digest);
  result := IsEqual(Digest, TDig);
end;

procedure TTestCoreCrypto._TSHA1;
const
  Test1Out: TSHA1Digest = (
    $A9, $99, $3E, $36, $47, $06, $81, $6A, $BA, $3E,
    $25, $71, $78, $50, $C2, $6C, $9C, $D0, $D8, $9D);
  Test2Out: TSHA1Digest = (
    $84, $98, $3E, $44, $1C, $3B, $D2, $6E, $BA, $AE,
    $4A, $A1, $F9, $51, $29, $E5, $E5, $46, $70, $F1);
var
  s: RawByteString;
  SHA: TSHA1;
  Digest: TSHA1Digest;
begin
  //Check(false, 'expected');
  Check(SingleTest('abc', Test1Out));
  Check(SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', Test2Out));
  s := 'Wikipedia, l''encyclopedie libre et gratuite';
  SHA.Full(pointer(s), length(s), Digest);
  CheckEqual(SHA1DigestToString(Digest), 'c18cc65028bbdc147288a2d136313287782b9c73');
  HMAC_SHA1('', '', Digest);
  CheckEqual(SHA1DigestToString(Digest), 'fbdb1d1b18aa6c08324b7d64b71fb76370690e1d');
  HMAC_SHA1('key', 'The quick brown fox jumps over the lazy dog', Digest);
  CheckEqual(SHA1DigestToString(Digest), 'de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9');
  // from https://www.ietf.org/rfc/rfc6070.txt
  PBKDF2_HMAC_SHA1('password', 'salt', 1, Digest);
  s := SHA1DigestToString(Digest);
  CheckEqual(s, '0c60c80f961f0e71f3a9b524af6012062fe037a6');
  PBKDF2_HMAC_SHA1('password', 'salt', 2, Digest);
  s := SHA1DigestToString(Digest);
  CheckEqual(s, 'ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957');
  PBKDF2_HMAC_SHA1('password', 'salt', 4096, Digest);
  s := SHA1DigestToString(Digest);
  CheckEqual(s, '4b007901b765489abead49d926f721d065a429c1');
  // also test MD5
  CheckEqual(htdigest('agent007', 'download area', 'secret'),
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65');
  CheckEqual(MD5(''), 'd41d8cd98f00b204e9800998ecf8427e');
  CheckEqual(MD5('a') ,'0cc175b9c0f1b6a831c399e269772661');
  CheckEqual(MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'),
    'd174ab98d277d9f5a5611c2c9f419d9f');
end;

function SingleTest(const s: RawByteString; const TDig: TSHA256Digest): boolean; overload;
var
  SHA: TSHA256;
  Digest: TSHA256Digest;
  i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s), length(s), Digest);
  result := IsEqual(Digest, TDig);
  if not result then
    exit;
  // 2. one update call for all chars
  SHA.Init;
  for i := 1 to length(s) do
    SHA.Update(@s[i], 1);
  SHA.Final(Digest);
  result := IsEqual(Digest, TDig);
end;

procedure TTestCoreCrypto._TSHA256;
var
  Digest: TSHA256Digest;
const
  D1: TSHA256Digest = (
    $ba, $78, $16, $bf, $8f, $01, $cf, $ea, $41, $41, $40, $de, $5d, $ae, $22, $23,
    $b0, $03, $61, $a3, $96, $17, $7a, $9c, $b4, $10, $ff, $61, $f2, $00, $15, $ad);
  D2: TSHA256Digest = (
    $24, $8d, $6a, $61, $d2, $06, $38, $b8, $e5, $c0, $26, $93, $0c, $3e, $60, $39,
    $a3, $3c, $e4, $59, $64, $ff, $21, $67, $f6, $ec, $ed, $d4, $19, $db, $06, $c1);
  D3: TSHA256Digest = (
    $94, $E4, $A9, $D9, $05, $31, $23, $1D, $BE, $D8, $7E, $D2, $E4, $F3, $5E, $4A,
    $0B, $F4, $B3, $BC, $CE, $EB, $17, $16, $D5, $77, $B1, $E0, $8B, $A9, $BA, $A3);
begin
  Check(SingleTest('abc', D1));
  Check(SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2));
  {%H-}SHA256Weak('lagrangehommage', Digest); // test with len=256>64
  Check(IsEqual(Digest, D3));
  {$ifdef ASMX64}
  if cfSSE41 in CpuFeatures then
  begin
    Exclude(CpuFeatures, cfSSE41);
    Check(SingleTest('abc', D1));
    Check(SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2));
    Include(CpuFeatures, cfSSE41);
  end;
  {$endif ASMX64}
end;

procedure TTestCoreCrypto._TRC4;
const
  key:    array[0..4] of byte = ($61, $8A, $63, $D2, $FB);
  InDat:  array[0..4] of byte = ($DC, $EE, $4C, $F9, $2C);
  OutDat: array[0..4] of byte = ($F1, $38, $29, $C9, $DE);
  Test1:  array[0..7] of byte = ($01, $23, $45, $67, $89, $ab, $cd, $ef);
  Res1:   array[0..7] of byte = ($75, $b7, $87, $80, $99, $e0, $c5, $96);
  Key2:   array[0..3] of byte = ($ef, $01, $23, $45);
  Test2:  array[0..9] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Res2:   array[0..9] of byte = ($d6, $a1, $41, $a7, $ec, $3c, $38, $df, $bd, $61);
var
  RC4: TRC4;
  Dat: array[0..9] of byte;
  Backup: TRC4;
begin
  RC4.Init(Test1, 8);
  RC4.Encrypt(Test1, Dat, 8);
  Check(CompareMem(@Dat, @Res1, sizeof(Res1)));
  RC4.Init(Key2, 4);
  RC4.Encrypt(Test2, Dat, 10);
  Check(CompareMem(@Dat, @Res2, sizeof(Res2)));
  RC4.Init(key, sizeof(key));
  RC4.Encrypt(InDat, Dat, sizeof(InDat));
  Check(CompareMem(@Dat, @OutDat, sizeof(OutDat)));
  RC4.Init(key, sizeof(key));
  Backup := RC4;
  RC4.Encrypt(InDat, Dat, sizeof(InDat));
  Check(CompareMem(@Dat, @OutDat, sizeof(OutDat)));
  RC4 := Backup;
  RC4.Encrypt(InDat, Dat, sizeof(InDat));
  Check(CompareMem(@Dat, @OutDat, sizeof(OutDat)));
  RC4 := Backup;
  RC4.Encrypt(OutDat, Dat, sizeof(InDat));
  Check(CompareMem(@Dat, @InDat, sizeof(OutDat)));
end;

procedure TTestCoreCrypto._TAES;
var
  A: TAES;
  st: RawUTF8;
  Key: TSHA256Digest;
  s, b, p: TAESBlock;
  i, k, ks: integer;
begin
  st := '1234essai';
  PInteger(UniqueRawUTF8(st))^ := Random32;
  for k := 0 to 2 do
  begin
    ks := 128 + k * 64; // test keysize of 128, 192 and 256 bits
    for i := 1 to 100 do
    begin
      PBKDF2_HMAC_SHA256(st, 'salt', 10, Key);
      MoveFast(Key, s, 16);
      A.EncryptInit(Key, ks);
      A.Encrypt(s, b);
      A.Done;
      A.DecryptInit(Key, ks);
      A.Decrypt(b, p);
      A.Done;
      Check(IsEqual(p, s));
      AppendCharToRawUTF8(st, AnsiChar(Random32(255)));
    end;
  end;
end;

end.
