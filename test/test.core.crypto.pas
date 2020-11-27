/// regression tests for mormot.core.crypto/secure/jwt units
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.crypto;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.secure,
  mormot.core.perf,
  mormot.core.test,
  mormot.core.jwt,
  mormot.core.ecc;

type
  /// regression tests for mormot.core.crypto and mormot.core.jwt features
  TTestCoreCrypto = class(TSynTestCase)
  public
    procedure CryptData(dpapi: boolean);
  published
    /// Adler32 hashing functions
    procedure _Adler32;
    /// MD5 hashing functions
    procedure _MD5;
    /// SHA-1 hashing functions
    procedure _SHA1;
    /// SHA-256 hashing functions
    procedure _SHA256;
    /// SHA-512 hashing functions
    procedure _SHA512;
    /// SHA-3 / Keccak hashing functions
    procedure _SHA3;
    /// AES encryption/decryption functions
    procedure _AES256;
    /// AES-GCM encryption/decryption with authentication
    procedure _AES_GCM;
    /// RC4 encryption function
    procedure _RC4;
    /// Base-64 encoding/decoding functions
    procedure _Base64;
    {$ifndef PUREMORMOT2}
    /// CompressShaAes() using SHA-256 / AES-256-CTR algorithm over SynLZ
    procedure _CompressShaAes;
    {$endif PUREMORMOT2}
    /// AES-based pseudorandom number generator
    procedure _TAESPNRG;
    /// CryptDataForCurrentUser() function
    procedure _CryptDataForCurrentUser;
    {$ifdef MSWINDOWS}
    /// CryptDataForCurrentUserAPI() function
    procedure _CryptDataForCurrentUserAPI;
    {$endif MSWINDOWS}
    /// JWT classes
    procedure _JWT;
    /// compute some performance numbers, mostly against regression
    procedure Benchmark;
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

procedure TTestCoreCrypto._SHA1;
const
  Test1Out: TSHA1Digest = (
    $A9, $99, $3E, $36, $47, $06, $81, $6A, $BA, $3E, $25,
    $71, $78, $50, $C2, $6C, $9C, $D0, $D8, $9D);
  Test2Out: TSHA1Digest = (
    $84, $98, $3E, $44, $1C, $3B, $D2, $6E, $BA, $AE, $4A,
    $A1, $F9, $51, $29, $E5, $E5, $46, $70, $F1);
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
  CheckEqual(MD5('a'), '0cc175b9c0f1b6a831c399e269772661');
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

procedure TTestCoreCrypto._SHA256;

  procedure DoTest;
  const
    D1: TSHA256Digest = ($ba, $78, $16, $bf, $8f, $01, $cf, $ea, $41, $41, $40,
      $de, $5d, $ae, $22, $23, $b0, $03, $61, $a3, $96, $17, $7a, $9c, $b4, $10,
      $ff, $61, $f2, $00, $15, $ad);
    D2: TSHA256Digest = ($24, $8d, $6a, $61, $d2, $06, $38, $b8, $e5, $c0, $26,
      $93, $0c, $3e, $60, $39, $a3, $3c, $e4, $59, $64, $ff, $21, $67, $f6, $ec,
      $ed, $d4, $19, $db, $06, $c1);
    D3: TSHA256Digest = ($94, $E4, $A9, $D9, $05, $31, $23, $1D, $BE, $D8, $7E,
      $D2, $E4, $F3, $5E, $4A, $0B, $F4, $B3, $BC, $CE, $EB, $17, $16, $D5, $77,
      $B1, $E0, $8B, $A9, $BA, $A3);
    DIG4096 = 'c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a';
  var
    Digest: THash512Rec;
    Digests: THash256DynArray;
    sign: TSynSigner;
    c: AnsiChar;
    i: integer;
    sha: TSHA256;
  begin
    Check(SingleTest('abc', D1));
    Check(SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2));
    {%H-}SHA256Weak('lagrangehommage', Digest.Lo); // test with len=256>64
    Check(IsEqual(Digest.Lo, D3));
    PBKDF2_HMAC_SHA256('password', 'salt', 1, Digest.Lo);
    check(SHA256DigestToString(Digest.Lo) =
      '120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b');
    PBKDF2_HMAC_SHA256('password', 'salt', 2, Digest.Lo);
    check(SHA256DigestToString(Digest.Lo) =
      'ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43');
    SetLength(Digests, 2);
    check(IsZero(Digests[0]));
    check(IsZero(Digests[1]));
    PBKDF2_HMAC_SHA256('password', 'salt', 2, Digests);
    check(IsEqual(Digests[0], Digest.Lo));
    check(not IsEqual(Digests[1], Digest.Lo));
    check(SHA256DigestToString(Digests[1]) =
      '830651afcb5c862f0b249bd031f7a67520d136470f5ec271ece91c07773253d9');
    PBKDF2_HMAC_SHA256('password', 'salt', 4096, Digest.Lo);
    check(SHA256DigestToString(Digest.Lo) = DIG4096);
    FillZero(Digest.b);
    sign.PBKDF2(saSha256, 'password', 'salt', 4096, Digest);
    check(SHA256DigestToString(Digest.Lo) = DIG4096);
    c := 'a';
    sha.Init;
    for i := 1 to 1000000 do
      sha.Update(@c, 1);
    sha.Final(Digest.Lo);
    Check(SHA256DigestToString(Digest.Lo) =
      'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0');
  end;

begin
  DoTest;
  {$ifdef ASMX64}
  if cfSSE41 in CpuFeatures then
  begin
    Exclude(CpuFeatures, cfSSE41);
    DoTest;
    Include(CpuFeatures, cfSSE41);
  end;
  {$endif ASMX64}
end;

procedure TTestCoreCrypto._RC4;
const
  Key1: array[0..4] of byte = ($61, $8A, $63, $D2, $FB);
  InDat: array[0..4] of byte = ($DC, $EE, $4C, $F9, $2C);
  OutDat: array[0..4] of byte = ($F1, $38, $29, $C9, $DE);
  Test1: array[0..7] of byte = ($01, $23, $45, $67, $89, $ab, $cd, $ef);
  Res1: array[0..7] of byte = ($75, $b7, $87, $80, $99, $e0, $c5, $96);
  Key2: array[0..3] of byte = ($ef, $01, $23, $45);
  Test2: array[0..9] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Res2: array[0..9] of byte = ($d6, $a1, $41, $a7, $ec, $3c, $38, $df, $bd, $61);
var
  rc4, ref: TRC4;
  dat: array[0..9] of byte;
  bak: TRC4;
  key, s, d: RawByteString;
  ks, i, len: integer;
begin
  rc4.Init(Test1, 8);
  rc4.Encrypt(Test1, dat, 8);
  Check(CompareMem(@dat, @Res1, sizeof(Res1)));
  rc4.Init(Key2, 4);
  rc4.Encrypt(Test2, dat, 10);
  Check(CompareMem(@dat, @Res2, sizeof(Res2)));
  rc4.Init(Key1, sizeof(Key1));
  rc4.Encrypt(InDat, dat, sizeof(InDat));
  Check(CompareMem(@dat, @OutDat, sizeof(OutDat)));
  rc4.Init(Key1, sizeof(Key1));
  bak := rc4;
  rc4.Encrypt(InDat, dat, sizeof(InDat));
  Check(CompareMem(@dat, @OutDat, sizeof(OutDat)));
  rc4 := bak;
  rc4.Encrypt(InDat, dat, sizeof(InDat));
  Check(CompareMem(@dat, @OutDat, sizeof(OutDat)));
  rc4 := bak;
  rc4.Encrypt(OutDat, dat, sizeof(InDat));
  Check(CompareMem(@dat, @InDat, sizeof(OutDat)));
  key := RandomString(100);
  for ks := 1 to 10 do
  begin
    ref.InitSHA3(pointer(key)^, ks * 10);
    for i := 0 to 100 do
    begin
      len := i * 3;
      s := RandomAnsi7(len);
      SetString(d, nil, len);
      rc4 := ref;
      rc4.EncryptBuffer(pointer(s), pointer(d), len); // encrypt
      rc4 := ref;
      rc4.EncryptBuffer(pointer(d), pointer(d), len); // decrypt
      check(s = d);
    end;
  end;
end;

procedure TTestCoreCrypto._SHA512;

  procedure Test(const password, secret, expected: RawUTF8; rounds: integer = 0);
  var
    dig: THash512Rec;
    sign: TSynSigner;
  begin
    if rounds = 0 then
    begin
      HMAC_SHA512(password, secret, dig.b);
      Check(SHA512DigestToString(dig.b) = expected);
      sign.Init(saSha512, password);
      sign.Update(secret);
      Check(sign.final = expected);
    end
    else
    begin
      PBKDF2_HMAC_SHA512(password, secret, rounds, dig.b);
      Check(SHA512DigestToString(dig.b) = expected);
      FillZero(dig.b);
      sign.PBKDF2(saSha512, password, secret, rounds, dig);
      Check(SHA512DigestToString(dig.b) = expected);
    end;
  end;

const
  FOX: RawByteString = 'The quick brown fox jumps over the lazy dog';
var
  dig: TSHA512Digest;
  i: integer;
  sha: TSHA512;
  c: AnsiChar;
  temp: RawByteString;
begin // includes SHA-384, which is a truncated SHA-512
  Check(SHA384('') =
    '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63' +
    'f6e1da274edebfe76f65fbd51ad2f14898b95b');
  Check(SHA384('abc') =
    'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605' +
    'a43ff5bed8086072ba1e7cc2358baeca134c825a7');
  Check(SHA384('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn' +
    'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu') = '09330c33f711' +
    '47e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039');
  Check(SHA512('') = 'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d' +
    '36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e');
  Check(SHA512(FOX) = '07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785' +
    '436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
  Check(SHA512(FOX + '.') = '91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d128' +
    '90cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed');
  sha.Init;
  for i := 1 to length(FOX) do
    sha.Update(@FOX[i], 1);
  sha.Final(dig);
  Check(SHA512DigestToString(dig) = '07e547d9586f6a73f73fbac0435ed76951218fb7d0c' +
    '8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
  c := 'a';
  sha.Init;
  for i := 1 to 1000 do
    sha.Update(@c, 1);
  sha.Final(dig);
  Check(SHA512DigestToString(dig) =
    '67ba5535a46e3f86dbfbed8cbbaf0125c76ed549ff8' +
    'b0b9e03e0c88cf90fa634fa7b12b47d77b694de488ace8d9a65967dc96df599727d3292a8d9d447709c97');
  SetLength(temp, 1000);
  FillCharFast(pointer(temp)^, 1000, ord('a'));
  Check(SHA512(temp) = SHA512DigestToString(dig));
  for i := 1 to 1000000 do
    sha.Update(@c, 1);
  sha.Final(dig);
  Check(SHA512DigestToString(dig) =
    'e718483d0ce769644e2e42c7bc15b4638e1f98b13b2' +
    '044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b');
  Test('', '', 'b936cee86c9f87aa5d3c6f2e84cb5a4239a5fe50480a' +
    '6ec66b70ab5b1f4ac6730c6c515421b327ec1d69402e53dfb49ad7381eb067b338fd7b0cb22247225d47');
  Test('key', FOX, 'b42af09057bac1e2d41708e48a902e09b5ff7f12ab42' +
    '8a4fe86653c73dd248fb82f948a549f7b791a5b41915ee4d1ec3935357e4e2317250d0372afa2ebeeb3a');
  Test(FOX + FOX, FOX, '19e504ba787674baa63471436a4ec5a71ba359a0f2d375' +
    '12edd4db69dce1ec6a0e48f0ae460fc9342fbb453cf2942a0e3fa512dd361e30f0e8b8fc8c7a4ece96');
  Test('Jefe', 'what do ya want for nothing?',
    '164b7a7bfcf819e2e395fbe73b56e0a387bd64222e8' +
    '31fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737');
  Test('password', 'salt', '867f70cf1ade02cff3752599a3a53dc4af34c7a669815ae5' +
    'd513554e1c8cf252c02d470a285a0501bad999bfe943c08f050235d7d68b1da55e63f73b60a57fce', 1);
  Test('password', 'salt', 'd197b1b33db0143e018b12f3d1d1479e6cdebdcc97c5c0f87' +
    'f6902e072f457b5143f30602641b3d55cd335988cb36b84376060ecd532e039b742a239434af2d5', 4096);
  HMAC_SHA256('Jefe', 'what do ya want for nothing?', PHash256(@dig)^);
  Check(SHA256DigestToString(PHash256(@dig)^) = '5bdcc146bf60754e6a042426089575c' +
    '75a003f089d2739839dec58b964ec3843');
  HMAC_SHA384('Jefe', 'what do ya want for nothing?', PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^) = 'af45d2e376484031617f78d2b58a6b1' +
    'b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649');
  PBKDF2_HMAC_SHA384('password', 'salt', 4096, PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^) = '559726be38db125bc85ed7895f6e3cf574c7a01c' +
    '080c3447db1e8a76764deb3c307b94853fbe424f6488c5f4f1289626');
  PBKDF2_HMAC_SHA512('passDATAb00AB7YxDTT', 'saltKEYbcTcXHCBxtjD', 1, dig);
  Check(SHA512DigestToString(dig) = 'cbe6088ad4359af42e603c2a33760ef9d4017a7b2aad10af46' +
    'f992c660a0b461ecb0dc2a79c2570941bea6a08d15d6887e79f32b132e1c134e9525eeddd744fa');
  PBKDF2_HMAC_SHA384('passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqK',
    'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcG', 1, PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^) =
    '0644a3489b088ad85a0e42be3e7f82500ec189366' +
    '99151a2c90497151bac7bb69300386a5e798795be3cef0a3c803227');
  { // rounds=100000 is slow, so not tested by default
  PBKDF2_HMAC_SHA512('passDATAb00AB7YxDTT','saltKEYbcTcXHCBxtjD',100000,dig);
  Check(SHA512DigestToString(dig)='accdcd8798ae5cd85804739015ef2a11e32591b7b7d16f76819b30'+
    'b0d49d80e1abea6c9822b80a1fdfe421e26f5603eca8a47a64c9a004fb5af8229f762ff41f');
  PBKDF2_HMAC_SHA384('passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqK','saltKEYbcTcXHCBxtj'+
    'D2PnBh44AIQ6XUOCESOhXpEp3HrcG',100000,PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^)='bf625685b48fe6f187a1780c5cb8e1e4a7b0dbd'+
    '6f551827f7b2b598735eac158d77afd3602383d9a685d87f8b089af30');
  }
end;

procedure TTestCoreCrypto._SHA3;
const
  HASH1 = '79f38adec5c20307a98ef76e8324afbfd46cfd81b22e3973c65fa1bd9de31787';
  DK = '7bbdbe37ea70dd2ed640837ff8a926d381806ffa931695addd38ab950d35ad1880' +
    '1a8290e8d97fe14cdfd3cfdbcd0fe766d3e6e4636bd0a17d710a61678db363';
var
  instance: TSHA3;
  secret, data, encrypted: RawByteString;
  dig: THash256;
  h512: THash512Rec;
  s, i: integer;
  sign: TSynSigner;
begin
  // validate against official NIST vectors
  // taken from http://csrc.nist.gov/groups/ST/toolkit/examples.html#aHashing
  Check(instance.FullStr(SHA3_224, nil, 0) =
    '6B4E03423667DBB73B6E15454F0EB1ABD4597F9A1B078E3F5B5A6BC7');
  Check(instance.FullStr(SHA3_256, nil, 0) =
    'A7FFC6F8BF1ED76651C14756A061D662F580FF4DE43B49FA82D80A4B80F8434A');
  Check(instance.FullStr(SHA3_384, nil, 0) =
    '0C63A75B845E4F7D01107D852E4C2485C51A50AAAA94FC61995E71BBEE983A2AC3713831264ADB47FB6BD1E058D5F004');
  Check(instance.FullStr(SHA3_512, nil, 0) =
    'A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80A615B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26');
  Check(instance.FullStr(SHAKE_128, nil, 0) =
    '7F9C2BA4E88F827D616045507605853ED73B8093F6EFBC88EB1A6EACFA66EF26');
  Check(instance.FullStr(SHAKE_256, nil, 0) =
    '46B9DD2B0BA88D13233B3FEB743EEB243FCD52EA62B81B82B50C27646ED5762FD75DC4DDD8C0F200CB05019D67B592F6FC821C49479AB48640292EACB3B7C4BE');
  SetLength(data, 200);
  FillCharFast(pointer(data)^, 200, $A3);
  Check(instance.FullStr(SHA3_224, pointer(data), length(data)) =
    '9376816ABA503F72F96CE7EB65AC095DEEE3BE4BF9BBC2A1CB7E11E0');
  Check(instance.FullStr(SHA3_256, pointer(data), length(data)) =
    '79F38ADEC5C20307A98EF76E8324AFBFD46CFD81B22E3973C65FA1BD9DE31787');
  Check(instance.FullStr(SHA3_384, pointer(data), length(data)) =
    '1881DE2CA7E41EF95DC4732B8F5F002B189CC1E42B74168ED1732649CE1DBCDD76197A31FD55EE989F2D7050DD473E8F');
  Check(instance.FullStr(SHA3_512, pointer(data), length(data)) =
    'E76DFAD22084A8B1467FCF2FFA58361BEC7628EDF5F3FDC0E4805DC48CAEECA81B7C13C30ADF52A3659584739A2DF46BE589C51CA1A4A8416DF6545A1CE8BA00');
  instance.Init(SHA3_256);
  for i := 1 to length(data) do
    instance.Update(pointer(data), 1);
  instance.Final(dig);
  Check(SHA256DigestToString(dig) = HASH1);
  Check(sign.Full(saSha3256, data, nil, 0) = HASH1);
  instance.Init(SHA3_256);
  instance.Update(pointer(data), 100);
  instance.Update(pointer(data), 50);
  instance.Update(pointer(data), 20);
  instance.Update(pointer(data), 10);
  instance.Update(pointer(data), 10);
  instance.Update(pointer(data), 5);
  instance.Update(pointer(data), 5);
  instance.Final(dig, true); // NoInit=true to check Extendable-Output Function
  Check(SHA256DigestToString(dig) = HASH1);
  instance.Final(dig, true);
  Check(SHA256DigestToString(dig) =
    'f85500852a5b9bb4a35440e7e4b4dba9184477a4c97b97ab0b24b91a8b04d1c8');
  for i := 1 to 200 do
  begin
    FillZero(dig);
    instance.Final(dig, true);
    Check(not IsZero(dig), 'XOF mode');
  end;
  instance.Final(dig);
  Check(SHA256DigestToString(dig) =
    '75f8b0591e2baeae027d56c14ef3bc014d9dd29cce08b8b184528589147fc252', 'XOF vector');
  encrypted := instance.Cypher('secret', 'toto');
  Check(mormot.core.text.BinToHex(encrypted) = 'BF013A29');
  Check(BinToHexLower(encrypted) = 'bf013a29');
  for s := 0 to 3 do
  begin
    secret := RandomString(s * 3);
    Check(instance.Cypher(secret, '') = '');
    for i := 1 to 1000 do
    begin
      data := RandomString(i);
      encrypted := instance.Cypher(secret, data);
      Check((i < 16) or (encrypted <> data));
      instance.InitCypher(secret);
      Check(instance.Cypher(encrypted) = data);
    end;
  end;
  PBKDF2_SHA3(SHA3_512, 'pass', 'salt', 1000, @h512);
  check(SHA512DigestToString(h512.b) = DK);
  FillZero(h512.b);
  sign.PBKDF2(saSha3512, 'pass', 'salt', 1000, h512);
  check(SHA512DigestToString(h512.b) = DK);
  // taken from https://en.wikipedia.org/wiki/SHA-3
  Check(SHA3(SHAKE_128, 'The quick brown fox jumps over the lazy dog') =
    'F4202E3C5852F9182A0430FD8144F0A74B95E7417ECAE17DB0F8CFEED0E3E66E');
  Check(SHA3(SHAKE_128, 'The quick brown fox jumps over the lazy dof') =
    '853F4538BE0DB9621A6CEA659A06C1107B1F83F02B13D18297BD39D7411CF10C');
end;

procedure TTestCoreCrypto._TAESPNRG;
var
  p: TAESPRNG;
  b1, b2: TAESBlock;
  a1, a2: TAESPRNG;
  s1, s2, split, big: RawByteString;
  c: cardinal;
  d: double;
  e: TSynExtended;
  i, stripes: PtrInt;
  clo, chi, dlo, dhi, elo, ehi: integer;
  timer: TPrecisionTimer;
begin
  p := TAESPRNG.Main;
  p.FillRandom(b1);
  p.FillRandom(b2);
  Check(not IsEqual(b1, b2));
  Check(not CompareMem(@b1, @b2, sizeof(b1)));
  clo := 0;
  chi := 0;
  dlo := 0;
  dhi := 0;
  elo := 0;
  ehi := 0;
  a1 := TAESPRNG.Create;
  a2 := TAESPRNG.Create;
  try
    a1.FillRandom(b1);
    a2.FillRandom(b2);
    Check(not IsEqual(b1, b2));
    Check(not CompareMem(@b1, @b2, sizeof(b1)));
    Check(a1.FillRandom(0) = '');
    Check(a1.FillRandomHex(0) = '');
    for i := 1 to 2000 do
    begin
      s1 := a1.FillRandom(i);
      s2 := a2.FillRandom(i);
      check(length(s1) = i);
      check(length(s2) = i);
      if i > 4 then
        check(s1 <> s2);
      // compress the output to validate (somehow) its randomness
      check(length(AlgoSynLZ.Compress(s1)) > i, 'random should not compress');
      check(length(AlgoSynLZ.Compress(s2)) > i, 'random should not compress');
      s1 := a1.FillRandomHex(i);
      check(length(s1) = i * 2);
      check(mormot.core.text.HexToBin(pointer(s1), nil, i));
      c := a1.Random32;
      check(c <> a2.Random32, 'Random32 collision');
      if c < cardinal(maxint) then
        inc(clo)
      else
        inc(chi);
      check(a1.Random64 <> a2.Random64);
      check(a1.Random32(i) < cardinal(i));
      d := a1.RandomDouble;
      check((d >= 0) and (d < 1));
      if d < 0.5 then
        inc(dlo)
      else
        inc(dhi);
      d := a2.RandomDouble;
      check((d >= 0) and (d < 1));
      if d < 0.5 then
        inc(dlo)
      else
        inc(dhi);
      e := a1.Randomext;
      check((e >= 0) and (e < 1));
      if e < 0.5 then
        inc(elo)
      else
        inc(ehi);
      e := a2.Randomext;
      check((e >= 0) and (e < 1));
      if e < 0.5 then
        inc(elo)
      else
        inc(ehi);
    end;
  finally
    a1.Free;
    a2.Free;
  end;
  Check(clo + chi = 2000);
  Check(dlo + dhi = 4000);
  Check(elo + ehi = 4000);
  CheckUTF8((clo >= 900) and
            (clo <= 1100), 'Random32 distribution clo=%', [clo]);
  CheckUTF8((dlo >= 1800) and
            (dlo <= 2100), 'RandomDouble distribution dlo=%', [dlo]);
  CheckUTF8((elo >= 1800) and
            (elo <= 2100), 'RandomExt distribution elo=%', [elo]);
  s1 := p.FillRandom(100);
  for i := 1 to length(s1) do
    for stripes := 0 to 10 do
    begin
      split := p.AFSplit(pointer(s1)^, i, stripes);
      check(length(split) = i * (stripes + 1));
      check(TAESPRNG.AFUnsplit(split, pointer(s2)^, i));
      check(CompareMem(pointer(s1), pointer(s2), i));
    end;
  check(PosEx(s1, split) = 0);
  timer.Start;
  Check(p.Random32(0) = 0);
  for i := 1 to 100000 do
    Check(p.Random32(i) < cardinal(i));
  for i := 0 to 100000 do
    Check(p.Random32(maxInt - i) < cardinal(maxInt - i));
  NotifyTestSpeed('Random32', 100000 * 2, 100000 * 8, @timer);
  SetLength(big, 200000);
  timer.Start;
  p.FillRandom(pointer(big), length(big));
  NotifyTestSpeed('FillRandom', 1, length(big), @timer);
end;

procedure TTestCoreCrypto.CryptData(dpapi: boolean);
var
  i, size: integer;
  plain, enc, test: RawByteString;
  appsec: RawUTF8;
  func: function(const Data, AppSecret: RawByteString; Encrypt: boolean): RawByteString;
  tim: TPrecisionTimer;
const
  MAX = 1000;
begin
  {$ifdef MSWINDOWS}
  if dpapi then
    func := CryptDataForCurrentUserDPAPI
  else
  {$endif MSWINDOWS}
    func := CryptDataForCurrentUser;
  func('warmup', 'appsec', true);
  size := 0;
  tim.Start;
  for i := 0 to MAX - 1 do
  begin
    plain := TAESPRNG.Main.FillRandom(i);
    check(length(plain) = i);
    UInt32ToUtf8(i, appsec);
    enc := func(plain, appsec, true);
    if not ((plain = '') or (enc <> '')) then
      enc := func(plain, appsec, true);
    check((plain = '') or (enc <> ''));
    check(length(enc) >= length(plain));
    test := func(enc, appsec, false);
    check(length(test) = i);
    check(test = plain);
    inc(size, i + length(enc));
  end;
  if dpapi then
    NotifyTestSpeed('DPAPI', MAX * 2, size, @tim)
  else
    NotifyTestSpeed('AES-CFB', MAX * 2, size, @tim);
end;

procedure TTestCoreCrypto._CryptDataForCurrentUser;
begin
  CryptData(false);
end;

{$ifdef MSWINDOWS}
procedure TTestCoreCrypto._CryptDataForCurrentUserAPI;
begin
  CryptData(true);
end;
{$endif MSWINDOWS}

procedure TTestCoreCrypto._JWT;

  procedure test(one: TJWTAbstract);
  var
    t: RawUTF8;
    jwt: TJWTContent;
    i: integer;
    exp: TUnixTime;
  begin
    t := one.Compute(['http://example.com/is_root', true], 'joe');
    check(t <> '');
    check(TJWTAbstract.VerifyPayload(t, '', 'joe', '', @exp) = jwtValid);
    check(one.VerifyPayload(t, '', 'joe', '', @exp) = jwtValid);
    check(one.CacheTimeoutSeconds = 0);
    one.Options := one.Options + [joHeaderParse];
    one.Verify(t, jwt);
    check(jwt.result = jwtValid);
    check(jwt.reg[jrcIssuer] = 'joe');
    one.Options := one.Options - [joHeaderParse];
    one.CacheTimeoutSeconds := 60;
    check(one.CacheTimeoutSeconds = 60);
    one.Verify(t, jwt);
    check(exp = GetCardinal(pointer(jwt.reg[jrcExpirationTime])));
    check(jwt.result = jwtValid);
    check(jwt.reg[jrcExpirationTime] <> '');
    check(jwt.reg[jrcIssuer] = 'joe');
    check(jwt.data.B['http://example.com/is_root']);
    check((jwt.reg[jrcIssuedAt] <> '') = (jrcIssuedAt in one.Claims));
    check((jwt.reg[jrcJWTID] <> '') = (jrcJWTID in one.Claims));
    for i := 1 to 1000 do
    begin
      Finalize(jwt);
      FillCharFast(jwt, sizeof(jwt), 0);
      check(jwt.reg[jrcIssuer] = '');
      one.Verify(t, jwt);
      check(jwt.result = jwtValid, 'from cache');
      check(jwt.reg[jrcIssuer] = 'joe');
      check((jwt.reg[jrcJWTID] <> '') = (jrcJWTID in one.Claims));
    end;
    if (one.Algorithm <> 'none') and
       (t[length(t)] in ['1'..'9', 'B'..'Z', 'b'..'z']) then
    begin
      dec(t[length(t)]); // invalidate signature
      one.Verify(t, jwt);
      check(jwt.result <> jwtValid);
    end;
    one.Free;
  end;

  procedure Benchmark(algo: TSignAlgo);
  var
    i: integer;
    tok: RawUTF8;
    j: TJWTAbstract;
    jwt: TJWTContent;
    tim: TPrecisionTimer;
  begin
    j := JWT_CLASS[algo].Create('secret', 0, [jrcIssuer, jrcExpirationTime], []);
    try
      tok := j.Compute([], 'myself');
      tim.Start;
      for i := 1 to 1000 do
      begin
        jwt.result := jwtWrongFormat;
        j.Verify(tok, jwt);
        check(jwt.result = jwtValid);
        check(jwt.reg[jrcIssuer] = 'myself');
      end;
      NotifyTestSpeed('%', [JWT_TEXT[algo]], 1000, 0, @tim);
    finally
      j.Free;
    end;
  end;

var
  i: integer;
  j: TJWTAbstract;
  jwt: TJWTContent;
  secret: TECCCertificateSecret;
  tok: RawUTF8;
  tim: TPrecisionTimer;
  a: TSignAlgo;
begin
  test(TJWTNone.Create([jrcIssuer, jrcExpirationTime], [], 60));
  test(TJWTNone.Create([jrcIssuer, jrcExpirationTime, jrcIssuedAt], [], 60));
  test(TJWTNone.Create([jrcIssuer, jrcExpirationTime, jrcIssuedAt, jrcJWTID], [], 60));
  test(TJWTHS256.Create('sec', 100, [jrcIssuer, jrcExpirationTime], [], 60));
  test(TJWTHS256.Create('sec', 200, [jrcIssuer, jrcExpirationTime, jrcIssuedAt], [], 60));
  test(TJWTHS256.Create('sec', 10, [jrcIssuer, jrcExpirationTime, jrcIssuedAt,
    jrcJWTID], [], 60));
  j := TJWTHS256.Create('secret', 0, [jrcSubject], []);
  try
    jwt.result := jwtWrongFormat;
    j.Verify('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibm'
      + 'FtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeF'
      + 'ONFh7HgQ', jwt); // reference from jwt.io
    check(jwt.result = jwtValid);
    check(jwt.reg[jrcSubject] = '1234567890');
    check(jwt.data.U['name'] = 'John Doe');
    check(jwt.data.B['admin']);
    j.Verify('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibm'
      + 'FtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeF'
      + 'ONFh7hgQ', jwt); // altered one char in signature
    check(jwt.result = jwtInvalidSignature);
    tok := j.Compute(['uid', '{1CCA336D-A78F-4EB6-B701-1DB8E749BD1F}'], '', 'subject');
    j.Verify(tok, jwt);
    Check(jwt.result = jwtValid);
    check(jwt.reg[jrcSubject] = 'subject');
    check(jwt.data.U['uid'] = '{1CCA336D-A78F-4EB6-B701-1DB8E749BD1F}');
  finally
    j.Free;
  end;
  for i := 1 to 10 do
  begin
    secret := TECCCertificateSecret.CreateNew(nil); // self-signed certificate
    test(TJWTES256.Create(secret,
      [jrcIssuer, jrcExpirationTime], [], 60));
    test(TJWTES256.Create(secret,
      [jrcIssuer, jrcExpirationTime, jrcIssuedAt], [], 60));
    test(TJWTES256.Create(secret,
      [jrcIssuer, jrcExpirationTime, jrcIssuedAt, jrcJWTID], [], 60));
    secret.Free;
  end;
  for a := saSha256 to high(a) do
    Benchmark(a);
  secret := TECCCertificateSecret.CreateNew(nil);
  j := TJWTES256.Create(secret, [jrcIssuer, jrcExpirationTime], [], 60);
  try
    tok := j.Compute([], 'myself');
    tim.Start;
    for i := 1 to 100 do
    begin
      jwt.result := jwtWrongFormat;
      j.Verify(tok, jwt);
      check(jwt.result = jwtValid);
      check(jwt.reg[jrcIssuer] = 'myself');
    end;
    NotifyTestSpeed('ES256', 100, 0, @tim);
  finally
    j.Free;
    secret.Free;
  end;
end;

type
  TBenchmark = (
    // non cryptographic hashes
    bCRC32c, bXXHash32, bHash32,
    // cryptographic hashes
    bMD5, bSHA1, bHMACSHA1, bSHA256, bHMACSHA256, bSHA384, bHMACSHA384, bSHA512,
    bHMACSHA512, bSHA3_256, bSHA3_512,
    // encryption
    bRC4, bAES128CFB, bAES128OFB, bAES128CFBCRC, bAES128OFBCRC, bAES128GCM,
    bAES256CFB, bAES256OFB, bAES256CFBCRC, bAES256OFBCRC, bAES256GCM,
    bSHAKE128, bSHAKE256);

procedure TTestCoreCrypto.Benchmark;
const
  SIZ: array[0..4] of integer = (
    8, 50, 100, 1000, 10000);
  COUNT = 500;
  AESCLASS: array[bAES128CFB..bAES256GCM] of TAESAbstractClass = (
    TAESCFB, TAESOFB, TAESCFBCRC, TAESOFBCRC, TAESGCM,
    TAESCFB, TAESOFB, TAESCFBCRC, TAESOFBCRC, TAESGCM);
  AESBITS: array[bAES128CFB..bAES256GCM] of integer = (
    128, 128, 128, 128, 128,
    256, 256, 256, 256, 256);
var
  b: TBenchmark;
  s, i, size, n: integer;
  data, encrypted: RawByteString;
  dig: THash512Rec;
  MD5: TMD5;
  SHA1: TSHA1;
  SHA256: TSHA256;
  SHA384: TSHA384;
  SHA512: TSHA512;
  SHA3, SHAKE128, SHAKE256: TSHA3;
  RC4: TRC4;
  timer: TPrecisionTimer;
  time: array[TBenchmark] of Int64;
  AES: array[bAES128CFB..bAES256GCM] of TAESAbstract;
  TXT: array[TBenchmark] of RawUTF8;
begin
  GetEnumTrimmedNames(TypeInfo(TBenchmark), @TXT);
  for b := low(b) to high(b) do
    TXT[b] := LowerCase(TXT[b]);
  for b := low(AES) to high(AES) do
    AES[b] := AESCLASS[b].Create(dig{%H-}, AESBITS[b]);
  SHAKE128.InitCypher('secret', SHAKE_128);
  SHAKE256.InitCypher('secret', SHAKE_256);
  RC4.InitSHA3(dig, SizeOf(dig));
  FillCharFast(time, sizeof(time), 0);
  size := 0;
  n := 0;
  for s := 0 to high(SIZ) do
  begin
    data := RandomString(SIZ[s]);
    SetLength(encrypted, SIZ[s]);
    for b := low(b) to high(b) do
    begin
      timer.Start;
      for i := 1 to COUNT do
      begin
        dig.d0 := 0;
        dig.d1 := 0;
        case b of
          bXXHash32:
            dig.d0 := xxHash32(0, pointer(data), SIZ[s]);
          bHash32:
            dig.d0 := Hash32(pointer(data), SIZ[s]);
          bCRC32c:
            dig.d0 := crc32c(0, pointer(data), SIZ[s]);
          bMD5:
            MD5.Full(pointer(data), SIZ[s], dig.h0);
          bSHA1:
            SHA1.Full(pointer(data), SIZ[s], dig.b160);
          bHMACSHA1:
            HMAC_SHA1('secret', data, dig.b160);
          bSHA256:
            SHA256.Full(pointer(data), SIZ[s], dig.Lo);
          bHMACSHA256:
            HMAC_SHA256('secret', data, dig.Lo);
          bSHA384:
            SHA384.Full(pointer(data), SIZ[s], dig.b384);
          bHMACSHA384:
            HMAC_SHA384('secret', data, dig.b384);
          bSHA512:
            SHA512.Full(pointer(data), SIZ[s], dig.b);
          bHMACSHA512:
            HMAC_SHA512('secret', data, dig.b);
          bSHA3_256:
            SHA3.Full(pointer(data), SIZ[s], dig.Lo);
          bSHA3_512:
            SHA3.Full(pointer(data), SIZ[s], dig.b);
          bRC4:
            RC4.EncryptBuffer(pointer(data), pointer(encrypted), SIZ[s]);
          bAES128CFB, bAES128OFB, bAES256CFB, bAES256OFB:
            AES[b].EncryptPKCS7(data, {encrypt=}true);
          bAES128CFBCRC, bAES128OFBCRC, bAES256CFBCRC, bAES256OFBCRC, bAES128GCM,
            bAES256GCM:
            AES[b].MACAndCrypt(data, {encrypt=}true);
          bSHAKE128:
            SHAKE128.Cypher(pointer(data), pointer(encrypted), SIZ[s]);
          bSHAKE256:
            SHAKE256.Cypher(pointer(data), pointer(encrypted), SIZ[s]);
        end;
        Check((b >= bRC4) or (dig.d0 <> 0) or (dig.d1 <> 0));
      end;
      inc(time[b], NotifyTestSpeed('% %', [TXT[b], SIZ[s]], COUNT, SIZ[s] *
        COUNT, @timer, {onlylog=}true));
      //if b in [bSHA3_512,high(b)] then AddConsole('');
    end;
    inc(size, SIZ[s] * COUNT);
    inc(n, COUNT);
  end;
  for b := low(b) to high(b) do
    AddConsole(format('%d %s in %s i.e. %d/s or %s/s', [n, TXT[b],
      MicroSecToString(time[b]), (Int64(n) * 1000000) div time[b],
      KB((Int64(size) * 1000000) div time[b])]));
  for b := low(AES) to high(AES) do
    AES[b].Free;
end;

procedure TTestCoreCrypto._Adler32;
begin
  Check(Adler32SelfTest);
end;

procedure TTestCoreCrypto._Base64;
const
  Value64: RawUTF8 = 'SGVsbG8gL2Mn6XRhaXQg5+Ar';
var
  tmp: RawByteString;
  b64: RawUTF8;
  Value: WinAnsiString;
  i, L: Integer;
begin
  Value := 'Hello /c''0tait 67+';
  Value[10] := #$E9;
  Value[16] := #$E7;
  Value[17] := #$E0;
  Check(not IsBase64(Value));
  Check(BinToBase64(Value) = Value64);
  Check(IsBase64(Value64));
  tmp := StringFromFile(ExeVersion.ProgramFileName);
  b64 := BinToBase64(tmp);
  Check(IsBase64(b64));
  Check(Base64ToBin(b64) = tmp);
  tmp := '';
  for i := 1 to 1998 do
  begin
    b64 := BinToBase64(tmp);
    Check((tmp = '') or IsBase64(b64));
    Check(BinToBase64(tmp) = b64);
    Check(Base64ToBin(b64) = tmp);
    if tmp <> '' then
    begin
      L := length(b64);
      Check(not IsBase64(pointer(b64), L - 1));
      b64[Random(L) + 1] := '&';
      Check(not IsBase64(pointer(b64), L));
    end;
    b64 := BinToBase64uri(tmp);
    Check(Base64uriToBin(b64) = tmp);
    tmp := tmp + AnsiChar(Random(255));
  end;
end;

{$ifdef MSWINDOWS}
  // on Windows: enable Microsoft AES Cryptographic Provider (XP SP3 and up)
  {$define USE_PROV_RSA_AES}
{$endif MSWINDOWS}

const
  TEST_AES_REF: array[0..2, 0..4] of RawByteString =(
  // 128-bit
    ('aS24Jm0RHPz26P_RHqX-pGktuCZtERz89uj_0R6l_qRpLbgmbREc_Pbo_9Eepf6kB7pVFdRAcIoVhoTQPytzTQ',
    'aS24Jm0RHPz26P_RHqX-pCTLpnA2lH7fAWpovxWR8Voytqn9B_zTt6Zrt1Gjb4J5HUs6E7C9Uf4fV83SxyILCg',
    '0YRWak2ZiQj-cncKQ3atJtcclNgW9OiQPpY6mLvrfYQc_mORQygR9LFU2z2Prc8I5anMvOABB62Ei5AAWY8M0Q',
    '0YRWak2ZiQj-cncKQ3atJingGAyjpdvuFAvnZ4vDXweTPTJOFSBVUuqs9SW6vSkAyhtoFM9p-gO3IRZh227twA',
    '0YRWak2ZiQj-cncKQ3atJjjmhYzJAYmaqNOy9bCBqYa0YYLiSrlUwv9f4JqyVmPQg7w2zQjjdyHSCuYxA-coGQ'),
  // 192-bit
    ('3S2QhC78T0eesG3hiqtA2N0tkIQu_E9HnrBt4YqrQNjdLZCELvxPR56wbeGKq0DYJob7gbbvgBaFdm_Bwed4RQ',
    '3S2QhC78T0eesG3hiqtA2HNVuHHzMsrQOruEy1t6Q-AMQMszIPd_86pnqzIyzdSZut-CCacA9T5O8e8ZJKvZOQ',
    'a6wXR1K29yQvbGGkawiHN1RcFhrbtbne2w13ziEURY1Btg1oqiL-BqTGtEsu4LH5wLYcGNQJ21CR58LBtRysQg',
    'a6wXR1K29yQvbGGkawiHN4Cloz_9GlJhlEozeNI4MFjKwihToQP6_FDpDVHz21qUonhk6MZ9_-6vNvnGqbOTcg',
    'a6wXR1K29yQvbGGkawiHN7koCYngh0WS5R-rsGy5zSaC9txKnyHDavH1tkXlWZuxTjQCNHbiAIIRYK4giZDHzA'),
  // 256-bit
    ('Kw50ybT0hl8MXw1IcBFm5isOdMm09IZfDF8NSHARZuYrDnTJtPSGXwxfDUhwEWbmn9aUUA6_ZwXpKRiFMlXRiw',
    'Kw50ybT0hl8MXw1IcBFm5iV4ZAxvgHN-4j2F7ch7PWr6yHhbcp0Scqd2WDHZMRygi3thq9H3jKVo34_NPKdK1A',
    'vf-UrsBFA2NkziMn6szalnw24-wbPmG9lySgx0WLZZpfkTpw2euPIm6ZkFzjFa-lqr4yngOkvW99hPGzYEAjDw',
    'vf-UrsBFA2NkziMn6szalgQnKyYBxXxLhVI9s8D3cZkYsLsdfSUCTUY8moP2SenmHCWQWwaq_ibRCr4JngSkZQ',
    'vf-UrsBFA2NkziMn6szalimh8XYdFObdg_TwNyfX8Zy2Dk8YVPSDzzAvZ2Xx6WP_4owC6MIq7kZ2xPZ_d6vZmg'));

procedure TTestCoreCrypto._AES256;
var
  A: TAES;
  st, orig, crypted, s2, s3: RawByteString;
  Key: TSHA256Digest;
  s, b, p: TAESBlock;
  i, k, ks, m, len: integer;
  {$ifndef PUREMORMOT2}
  AES: TAESFull;
  {$endif PUREMORMOT2}
  PC: PAnsiChar;
  noaesni: boolean;
  Timer: array[boolean] of TPrecisionTimer;
  ValuesCrypted, ValuesOrig: array[0..1] of RawByteString;
  {$ifdef CPUINTEL}
  backup: TIntelCpuFeatures;
  {$endif CPUINTEL}
const
  MAX = 4096 * 1024;  // test 4 MB data, i.e. multi-threaded AES
  MODES: array[0..6 {$ifdef USE_PROV_RSA_AES} + 2{$endif}] of TAESAbstractClass =
    (TAESECB, TAESCBC, TAESCFB, TAESOFB, TAESCTR, TAESCFBCRC, TAESOFBCRC
     {$ifdef USE_PROV_RSA_AES}, TAESECB_API, TAESCBC_API{$endif});
      // TAESCFB_API and TAESOFB_API just do not work
begin
  {$ifdef CPUINTEL}
  backup := CpuFeatures;
  {$endif CPUINTEL}
  Check(AESTablesTest, 'Internal Tables');
  SetLength(orig, MAX);
  SetLength(crypted, MAX + 256);
  st := '1234essai';
  PInteger(UniqueRawUTF8(RawUTF8(st)))^ := Random(MaxInt);
  for noaesni := false to true do
  begin
    {%H-}Timer[noaesni].Init;
    for k := 0 to 2 do
    begin
      ks := 128 + k * 64; // test keysize of 128, 192 and 256 bits
      for m := 0 to high(MODES) do
      begin
        st := RawUTF8(StringOfChar('x', 50));
        with MODES[m].Create(pointer(st)^, ks) do
        try
          s2 := EncryptPKCS7(st, false);
          s3 := BinToBase64uri(s2);
          i := m;
          if i >= 7 then // e.g. TAESECB_API -> TAESECB
            dec(i, 7)
          else if i >= 5 then
            dec(i, 3);  // e.g. TAESCFBCRC -> TAESCFB
          CheckUTF8(TEST_AES_REF[k, i] = s3, 'test vector %-%', [MODES[m], ks]);
          check(DecryptPKCS7(s2, false) = st);
        finally
          Free;
        end;
      end;
      SHA256Weak(st, Key);
      for i := 1 to 100 do
      begin
        move(Key, s, 16);
        Timer[noaesni].Resume;
        A.EncryptInit(Key, ks);
        A.Encrypt(s, b);
        A.Done;
        A.DecryptInit(Key, ks);
        A.Decrypt(b, p);
        A.Done;
        Timer[noaesni].Pause;
        Check(CompareMem(@p, @s, sizeof(p)));
        Check(IsEqual(p, s));
        st := st + RandomString(4);
      end;
      PC := Pointer(orig);
      len := MAX;
      repeat // populate orig with random data
        if len > length(st) then
          i := length(st)
        else
          i := len;
        dec(len, i);
        move(pointer(st)^, PC^, i);
        inc(PC, i);
      until len = 0;
      {$ifndef PUREMORMOT2}
      len := AES.EncodeDecode(Key, ks, MAX, True, nil, nil, pointer(orig),
        pointer(crypted));
      Check(len < MAX + 256);
      Check(len >= MAX);
      len := AES.EncodeDecode(Key, ks, len, False, nil, nil, pointer(crypted), nil);
      try
        Check(len = MAX);
        Check(CompareMem(AES.outStreamCreated.Memory, pointer(orig), MAX));
      {$endif PUREMORMOT2}
        if not noaesni then
        begin
          for m := low(MODES) to high(MODES) do
            with MODES[m].Create(Key, ks) do
            try
              FillCharFast(pointer(@IV)^, sizeof(TAESBlock), 1);
              //Timer.Start;
              for i := 0 to 256 do
              begin
                if i < 64 then
                  len := i
                else if i < 128 then
                  len := i * 16
                else
                  len := i * 32;
                {$ifndef PUREMORMOT2}
                FillCharFast(pointer(crypted)^, len, 0);
                Encrypt(AES.outStreamCreated.Memory, pointer(crypted), len);
                FillCharFast(pointer(orig)^, len, 0);
                Decrypt(pointer(crypted), pointer(orig), len);
                Check((len = 0) or (not isZero(pointer(orig), len)) or isZero(AES.outStreamCreated.Memory,
                  len));
                Check(CompareMem(AES.outStreamCreated.Memory, pointer(orig), len));
                {$endif PUREMORMOT2}
                s2 := copy(orig, 1, len);
                Check(DecryptPKCS7(EncryptPKCS7(s2)) = s2, IntToStr(len));
              end;
//fRunConsole := Format('%s %s%d:%s'#10,[fRunConsole,Copy(MODES[m].ClassName,5,10),ks,Timer.Stop]);
              if m < length(ValuesCrypted) then
              begin
                ValuesCrypted[m] := Copy(crypted, 1, len);
                ValuesOrig[m] := s2;
              end
              else if m > 6 then
              begin
                Check(ValuesOrig[m - 7] = s2);
                Check(ValuesCrypted[m - 7] = Copy(crypted, 1, len), MODES[m].ClassName);
              end;
            finally
              Free;
            end;
        end;
      {$ifndef PUREMORMOT2}
      finally
        AES.outStreamCreated.Free;
      end;
      {$endif PUREMORMOT2}
    end;
    {$ifdef CPUINTEL}
    if noaesni then
    begin
      fRunConsole := format('%s cypher 1..%d bytes with AES-NI: %s, without: %s',
        [fRunConsole, length(st), Timer[false].Stop, Timer[true].Stop]);
      Include(CpuFeatures, cfAESNI); // revert Exclude() below from previous loop
    end;
    if A.UsesAESNI then
      Exclude(CpuFeatures, cfAESNI)
    else
    {$endif CPUINTEL}
      break;
  end;
  {$ifdef CPUINTEL}
  CpuFeatures := backup;
  {$endif CPUINTEL}
end;

procedure TTestCoreCrypto._AES_GCM;
const
  hex32: THash256 = ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b,
    $0c, $0d, $0e, $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a,
    $1b, $1c, $1d, $1e, $1f);
  buf32: THash256 = ($92, $4e, $17, $8a, $17, $fa, $1c, $a0, $e7, $48, $6f, $04,
    $04, $12, $3b, $91, $db, $f7, $97, $bb, $9d, $bd, $e9, $b1, $d4, $8d, $5c,
    $7f, $53, $16, $59, $12);
  tag32: array[0..15] of byte = ($10, $f9, $72, $b6, $f9, $e0, $a3, $c1, $cf,
    $9c, $cf, $56, $54, $3d, $ca, $79);
  K01: THash256 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  I01: array[0..11] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  P01: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  C01: array[0..15] of byte = ($ce, $a7, $40, $3d, $4d, $60, $6b, $6e, $07, $4e,
    $c5, $d3, $ba, $f3, $9d, $18);
  T01: array[0..15] of byte = ($d0, $d1, $c8, $a7, $99, $99, $6b, $f0, $26, $5b,
    $98, $b5, $d4, $8a, $b9, $19);
  K02: THash256 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  I02: array[0..11] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  H02: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  T02: array[0..15] of byte = ($2d, $45, $55, $2d, $85, $75, $92, $2b, $3c, $a3,
    $cc, $53, $84, $42, $fa, $26);
  K03: THash256 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  I03: array[0..11] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  H03: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  P03: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  C03: array[0..15] of byte = ($ce, $a7, $40, $3d, $4d, $60, $6b, $6e, $07, $4e,
    $c5, $d3, $ba, $f3, $9d, $18);
  T03: array[0..15] of byte = ($ae, $9b, $17, $71, $db, $a9, $cf, $62, $b3, $9b,
    $e0, $17, $94, $03, $30, $b4);
  K04: THash256 = ($fb, $76, $15, $b2, $3d, $80, $89, $1d, $d4, $70, $98, $0b,
    $c7, $95, $84, $c8, $b2, $fb, $64, $ce, $60, $97, $8f, $4d, $17, $fc, $e4,
    $5a, $49, $e8, $30, $b7);
  I04: array[0..11] of byte = ($db, $d1, $a3, $63, $60, $24, $b7, $b4, $02, $da,
    $7d, $6f);
  P04: array[0..15] of byte = ($a8, $45, $34, $8e, $c8, $c5, $b5, $f1, $26, $f5,
    $0e, $76, $fe, $fd, $1b, $1e);
  C04: array[0..15] of byte = ($5d, $f5, $d1, $fa, $bc, $bb, $dd, $05, $15, $38,
    $25, $24, $44, $17, $87, $04);
  T04: array[0..15] of byte = ($4c, $43, $cc, $e5, $a5, $74, $d8, $a8, $8b, $43,
    $d4, $35, $3b, $d6, $0f, $9f);
  K05: THash256 = ($40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b,
    $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a,
    $5b, $5c, $5d, $5e, $5f);
  I05: array[0..11] of byte = ($10, $11, $12, $13, $14, $15, $16, $17, $18, $19,
    $1a, $1b);
  H05: array[0..19] of byte = (0, $01, $02, $03, $04, $05, $06, $07, $08, $09,
    $0a, $0b, $0c, $0d, $0e, $0f, $10, $11, $12, $13);
  P05: array[0..23] of byte = ($20, $21, $22, $23, $24, $25, $26, $27, $28, $29,
    $2a, $2b, $2c, $2d, $2e, $2f, $30, $31, $32, $33, $34, $35, $36, $37);
  C05: array[0..23] of byte = ($59, $1b, $1f, $f2, $72, $b4, $32, $04, $86, $8f,
    $fc, $7b, $c7, $d5, $21, $99, $35, $26, $b6, $fa, $32, $24, $7c, $3c);
  T05: array[0..15] of byte = ($7d, $e1, $2a, $56, $70, $e5, $70, $d8, $ca, $e6,
    $24, $a1, $6d, $f0, $9c, $08);
  K07: THash256 = ($40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b,
    $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a,
    $5b, $5c, $5d, $5e, $5f);
  I07: array[0..11] of byte = ($10, $11, $12, $13, $14, $15, $16, $17, $18, $19,
    $1a, $1b);
  H07: THash256 = ($20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b,
    $2c, $2d, $2e, $2f, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3a,
    $3b, $3c, $3d, $3e, $3f);
  P07: array[0..255] of byte = (0, $01, $02, $03, $04, $05, $06, $07, $08, $09,
    $0a, $0b, $0c, $0d, $0e, $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18,
    $19, $1a, $1b, $1c, $1d, $1e, $1f, $20, $21, $22, $23, $24, $25, $26, $27,
    $28, $29, $2a, $2b, $2c, $2d, $2e, $2f, $30, $31, $32, $33, $34, $35, $36,
    $37, $38, $39, $3a, $3b, $3c, $3d, $3e, $3f, $40, $41, $42, $43, $44, $45,
    $46, $47, $48, $49, $4a, $4b, $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54,
    $55, $56, $57, $58, $59, $5a, $5b, $5c, $5d, $5e, $5f, $60, $61, $62, $63,
    $64, $65, $66, $67, $68, $69, $6a, $6b, $6c, $6d, $6e, $6f, $70, $71, $72,
    $73, $74, $75, $76, $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f, $80, $81,
    $82, $83, $84, $85, $86, $87, $88, $89, $8a, $8b, $8c, $8d, $8e, $8f, $90,
    $91, $92, $93, $94, $95, $96, $97, $98, $99, $9a, $9b, $9c, $9d, $9e, $9f,
    $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $aa, $ab, $ac, $ad, $ae,
    $af, $b0, $b1, $b2, $b3, $b4, $b5, $b6, $b7, $b8, $b9, $ba, $bb, $bc, $bd,
    $be, $bf, $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c7, $c8, $c9, $ca, $cb, $cc,
    $cd, $ce, $cf, $d0, $d1, $d2, $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da, $db,
    $dc, $dd, $de, $df, $e0, $e1, $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9, $ea,
    $eb, $ec, $ed, $ee, $ef, $f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8, $f9,
    $fa, $fb, $fc, $fd, $fe, $ff);
  C07: array[0..255] of byte = ($79, $3b, $3f, $d2, $52, $94, $12, $24, $a6, $af,
    $dc, $5b, $e7, $f5, $01, $b9, $15, $06, $96, $da, $12, $04, $5c, $1c, $60,
    $77, $d3, $ca, $c7, $74, $ac, $cf, $c3, $d5, $30, $d8, $48, $d6, $65, $d8,
    $1a, $49, $cb, $b5, 0, $b8, $8b, $bb, $62, $4a, $e6, $1d, $16, $67, $22, $9c,
    $30, $2d, $c6, $ff, $0b, $b4, $d7, $0b, $db, $bc, $85, $66, $d6, $f5, $b1,
    $58, $da, $99, $a2, $ff, $2e, $01, $dd, $a6, $29, $b8, $9c, $34, $ad, $1e,
    $5f, $eb, $a7, $0e, $7a, $ae, $43, $28, $28, $9c, $36, $29, $b0, $58, $83,
    $50, $58, $1c, $a8, $b9, $7c, $cf, $12, $58, $fa, $3b, $be, $2c, $50, $26,
    $04, $7b, $a7, $26, $48, $96, $9c, $ff, $8b, $a1, $0a, $e3, $0e, $05, $93,
    $5d, $f0, $c6, $93, $74, $18, $92, $b7, $6f, $af, $67, $13, $3a, $bd, $2c,
    $f2, $03, $11, $21, $bd, $8b, $b3, $81, $27, $a4, $d2, $ee, $de, $ea, $13,
    $27, $64, $94, $f4, $02, $cd, $7c, $10, $7f, $b3, $ec, $3b, $24, $78, $48,
    $34, $33, $8e, $55, $43, $62, $87, $09, $2a, $c4, $a2, $6f, $5e, $a7, $ea,
    $4a, $d6, $8d, $73, $15, $16, $39, $b0, $5b, $24, $e6, $8b, $98, $16, $d1,
    $39, $83, $76, $d8, $e4, $13, $85, $94, $75, $8d, $b9, $ad, $3b, $40, $92,
    $59, $b2, $6d, $cf, $c0, $6e, $72, $2b, $e9, $87, $b3, $76, $7f, $70, $a7,
    $b8, $56, $b7, $74, $b1, $ba, $26, $85, $b3, $68, $09, $14, $29, $fc, $cb,
    $8d, $cd, $de, $09, $e4);
  T07: array[0..15] of byte = ($87, $ec, $83, $7a, $bf, $53, $28, $55, $b2, $ce,
    $a1, $69, $d6, $94, $3f, $cd);
  K08: THash256 = ($fb, $76, $15, $b2, $3d, $80, $89, $1d, $d4, $70, $98, $0b,
    $c7, $95, $84, $c8, $b2, $fb, $64, $ce, $60, $97, $87, $8d, $17, $fc, $e4,
    $5a, $49, $e8, $30, $b7);
  I08: array[0..11] of byte = ($db, $d1, $a3, $63, $60, $24, $b7, $b4, $02, $da,
    $7d, $6f);
  H08: array[0..0] of byte = ($36);
  P08: array[0..0] of byte = ($a9);
  C08: array[0..0] of byte = ($0a);
  T08: array[0..15] of byte = ($be, $98, $7d, 0, $9a, $4b, $34, $9a, $a8, $0c,
    $b9, $c4, $eb, $c1, $e9, $f4);
  K09: THash256 = ($f8, $d4, $76, $cf, $d6, $46, $ea, $6c, $23, $84, $cb, $1c,
    $27, $d6, $19, $5d, $fe, $f1, $a9, $f3, $7b, $9c, $8d, $21, $a7, $9c, $21,
    $f8, $cb, $90, $d2, $89);
  I09: array[0..11] of byte = ($db, $d1, $a3, $63, $60, $24, $b7, $b4, $02, $da,
    $7d, $6f);
  H09: array[0..19] of byte = ($7b, $d8, $59, $a2, $47, $96, $1a, $21, $82, $3b,
    $38, $0e, $9f, $e8, $b6, $50, $82, $ba, $61, $d3);
  P09: array[0..19] of byte = ($90, $ae, $61, $cf, $7b, $ae, $bd, $4c, $ad, $e4,
    $94, $c5, $4a, $29, $ae, $70, $26, $9a, $ec, $71);
  C09: array[0..19] of byte = ($ce, $20, $27, $b4, $7a, $84, $32, $52, $01, $34,
    $65, $83, $4d, $75, $fd, $0f, $07, $29, $75, $2e);
  T09: array[0..15] of byte = ($ac, $d8, $83, $38, $37, $ab, $0e, $de, $84, $f4,
    $74, $8d, $a8, $89, $9c, $15);
  K10: THash256 = ($db, $bc, $85, $66, $d6, $f5, $b1, $58, $da, $99, $a2, $ff,
    $2e, $01, $dd, $a6, $29, $b8, $9c, $34, $ad, $1e, $5f, $eb, $a7, $0e, $7a,
    $ae, $43, $28, $28, $9c);
  I10: array[0..15] of byte = ($cf, $c0, $6e, $72, $2b, $e9, $87, $b3, $76, $7f,
    $70, $a7, $b8, $56, $b7, $74);
  P10: array[0..15] of byte = ($ce, $20, $27, $b4, $7a, $84, $32, $52, $01, $34,
    $65, $83, $4d, $75, $fd, $0f);
  C10: array[0..15] of byte = ($dc, $03, $e5, $24, $83, $0d, $30, $f8, $8e, $19,
    $7f, $3a, $ca, $ce, $66, $ef);
  T10: array[0..15] of byte = ($99, $84, $ef, $f6, $90, $57, $55, $d1, $83, $6f,
    $2d, $b0, $40, $89, $63, $4c);
  K11: THash256 = ($0e, $05, $93, $5d, $f0, $c6, $93, $74, $18, $92, $b7, $6f,
    $af, $67, $13, $3a, $bd, $2c, $f2, $03, $11, $21, $bd, $8b, $b3, $81, $27,
    $a4, $d2, $ee, $de, $ea);
  I11: array[0..16] of byte = ($74, $b1, $ba, $26, $85, $b3, $68, $09, $14, $29,
    $fc, $cb, $8d, $cd, $de, $09, $e4);
  H11: array[0..19] of byte = ($7b, $d8, $59, $a2, $47, $96, $1a, $21, $82, $3b,
    $38, $0e, $9f, $e8, $b6, $50, $82, $ba, $61, $d3);
  P11: array[0..19] of byte = ($90, $ae, $61, $cf, $7b, $ae, $bd, $4c, $ad, $e4,
    $94, $c5, $4a, $29, $ae, $70, $26, $9a, $ec, $71);
  C11: array[0..19] of byte = ($6b, $e6, $5e, $56, $06, $6c, $40, $56, $73, $8c,
    $03, $fe, $23, $20, $97, $4b, $a3, $f6, $5e, $09);
  T11: array[0..15] of byte = ($61, $08, $dc, $41, $7b, $f3, $2f, $7f, $b7, $55,
    $4a, $e5, $2f, $08, $8f, $87);

  procedure test(ptag: pointer; tlen: PtrInt; const key; kbits: PtrInt; pIV:
    pointer; IV_Len: PtrInt; pAAD: pointer; aLen: PtrInt; ctp: pointer; cLen:
    PtrInt; ptp: pointer; tn: integer);
  var
    tag: TAESBLock;
    ctxt: TAESGCMEngine;
    pt, ct: array[0..511] of byte;
  begin
    FillCharFast(pt, SizeOf(pt), 0);
    CheckUTF8(ctxt.FullDecryptAndVerify(key, kbits, pIV, IV_Len, pAAD, aLen, ctp,
      @pt, cLen, ptag, tlen), 'FullDecryptAndVerify #%', [tn]);
    CheckUTF8(CompareMem(@pt, ptp, cLen), 'Plain #%', [tn]);
    FillCharFast(ct, SizeOf(ct), 0);
    CheckUTF8(ctxt.FullEncryptAndAuthenticate(key, kbits, pIV, IV_Len, pAAD,
      aLen, ptp, @ct, cLen, tag), 'FullEncryptAndAuthenticate #%', [tn]);
    CheckUTF8(CompareMem(@tag, ptag, tlen), 'Tag #%', [tn]);
    CheckUTF8(CompareMem(@ct, ctp, cLen), 'Encoded #%', [tn]);
  end;

var
  ctxt: TAESGCMEngine;
  key, tag: TAESBlock;
  buf: THash512;
  n: integer;
begin
  key := PAESBlock(@hex32)^;
  for n := 1 to 32 do
  begin
    Check(ctxt.Init(key, 128));
    Check(ctxt.Reset(@hex32, n));
    Check(ctxt.Add_AAD(@hex32, n));
    Check(ctxt.Encrypt(@hex32, @buf, n));
    Check(ctxt.Final(tag));
    key := tag;
  end;
  Check(CompareMem(@buf32, @buf, SizeOf(buf32)));
  Check(CompareMem(@tag32, @tag, SizeOf(tag32)));
  test(@T01, 16, K01, 8 * sizeof(K01), @I01, sizeof(I01), nil, 0,
       @C01, sizeof(C01), @P01, 01);
  test(@T02, 16, K02, 8 * sizeof(K02), @I02, sizeof(I02), @H02, sizeof(H02),
       nil, 0, nil, 02);
  test(@T03, 16, K03, 8 * sizeof(K03), @I03, sizeof(I03), @H03, sizeof(H03),
       @C03, sizeof(C03), @P03, 03);
  test(@T04, 16, K04, 8 * sizeof(K04), @I04, sizeof(I04), nil, 0,
       @C04, sizeof(C04), @P04, 04);
  test(@T05, 16, K05, 8 * sizeof(K05), @I05, sizeof(I05), @H05, sizeof(H05),
       @C05, sizeof(C05), @P05, 05);
  test(@T07, 16, K07, 8 * sizeof(K07), @I07, sizeof(I07), @H07, sizeof(H07),
       @C07, sizeof(C07), @P07, 07);
  test(@T08, 16, K08, 8 * sizeof(K08), @I08, sizeof(I08), @H08, sizeof(H08),
       @C08, sizeof(C08), @P08, 08);
  test(@T09, 16, K09, 8 * sizeof(K09), @I09, sizeof(I09), @H09, sizeof(H09),
       @C09, sizeof(C09), @P09, 09);
  test(@T10, 16, K10, 8 * sizeof(K10), @I10, sizeof(I10), nil, 0, @C10,
       sizeof(C10), @P10, 10);
  test(@T11, 16, K11, 8 * sizeof(K11), @I11, sizeof(I11), @H11, sizeof(H11),
       @C11, sizeof(C11), @P11, 11);
end;

{$ifndef PUREMORMOT2}
procedure TTestCoreCrypto._CompressShaAes;
var
  s1, s2: RawByteString;
  keysize, i: integer;
begin
  for keysize := 0 to 10 do
  begin
    CompressShaAesSetKey(RandomString(keysize));
    for i := 0 to 50 do
    begin
      s1 := RandomString(i * 3);
      s2 := s1;
      Check(CompressShaAes(s1, true) = 'synshaaes');
      Check(CompressShaAes(s1, false) = 'synshaaes');
      Check(s1 = s2);
    end;
  end;
end;
{$endif PUREMORMOT2}

procedure TTestCoreCrypto._MD5;
var
  i, n: integer;
  md: TMD5;
  dig, dig2: TMD5Digest;
  tmp: TByteDynArray;
begin
  check(htdigest('agent007', 'download area', 'secret') =
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65');
  check(MD5('') = 'd41d8cd98f00b204e9800998ecf8427e');
  check(MD5('a') = '0cc175b9c0f1b6a831c399e269772661');
  check(MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789') =
    'd174ab98d277d9f5a5611c2c9f419d9f');
  SetLength(tmp, 256);
  for n := 256 - 80 to 256 do
  begin
    md.Init;
    for i := 1 to n do
      md.Update(tmp[0], 1);
    md.Final(dig);
    md.Full(pointer(tmp), n, dig2);
    check(IsEqual(dig, dig2));
    check(CompareMem(@dig, @dig2, sizeof(dig)));
  end;
end;

end.

