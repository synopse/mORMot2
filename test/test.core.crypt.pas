/// regression tests for mormot.crypto units
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.crypt;

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
  mormot.crypt.core,
  mormot.crypt.openssl,
  mormot.crypt.secure,
  mormot.core.perf,
  mormot.core.test,
  mormot.core.variants,
  mormot.crypt.jwt,
  mormot.crypt.ecc;

type
  /// regression tests for mormot.crypt.core and mormot.crypt.jwt features
  TTestCoreCrypto = class(TSynTestCase)
  public
    procedure CryptData(dpapi: boolean);
    procedure Prng(meta: TAesPrngClass; const name: RawUTF8);
  published
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
    /// AES encryption/decryption functions with proper test vectors and OpenSSL
    procedure _AES;
    /// AES-GCM encryption/decryption with authentication
    procedure _AES_GCM;
    /// RC4 encryption function
    procedure _RC4;
    /// 32-bit, 64-bit and 128-bit hashing functions including AesNiHash variants
    procedure Hashes;
    /// stream-oriented cryptography
    procedure Streams;
    /// Base64 encoding/decoding functions
    procedure _Base64;
    {$ifndef PUREMORMOT2}
    /// CompressShaAes() using SHA-256 / AES-256-CTR algorithm over SynLZ
    procedure _CompressShaAes;
    {$endif PUREMORMOT2}
    /// AES-based pseudorandom number generator
    procedure _TAesPNRG;
    /// CryptDataForCurrentUser() function
    procedure _CryptDataForCurrentUser;
    {$ifdef OSWINDOWS}
    /// CryptDataForCurrentUserApi() function
    procedure _CryptDataForCurrentUserApi;
    {$endif OSWINDOWS}
    /// JWT classes
    procedure _JWT;
    /// validate TBinaryCookieGenerator object
    procedure _TBinaryCookieGenerator;
    /// Cryptography Catalog
    procedure Catalog;
    /// compute some performance numbers, mostly against regression
    procedure Benchmark;
  end;



implementation



{ TTestCoreCrypto }

function SingleTest(const s: RawByteString; TDig: TSha1Digest): boolean; overload;
var
  SHA: TSha1;
  Digest: TSha1Digest;
  i: PtrInt;
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
  Test1Out: TSha1Digest = (
    $A9, $99, $3E, $36, $47, $06, $81, $6A, $BA, $3E, $25,
    $71, $78, $50, $C2, $6C, $9C, $D0, $D8, $9D);
  Test2Out: TSha1Digest = (
    $84, $98, $3E, $44, $1C, $3B, $D2, $6E, $BA, $AE, $4A,
    $A1, $F9, $51, $29, $E5, $E5, $46, $70, $F1);
var
  s: RawByteString;
  SHA: TSha1;
  Digest: TSha1Digest;
begin
  //Check(false, 'expected');
  Check(SingleTest('abc', Test1Out));
  Check(SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', Test2Out));
  s := 'Wikipedia, l''encyclopedie libre et gratuite';
  SHA.Full(pointer(s), length(s), Digest);
  CheckEqual(Sha1DigestToString(Digest), 'c18cc65028bbdc147288a2d136313287782b9c73');
  HmacSha1('', '', Digest);
  CheckEqual(Sha1DigestToString(Digest), 'fbdb1d1b18aa6c08324b7d64b71fb76370690e1d');
  HmacSha1('key', 'The quick brown fox jumps over the lazy dog', Digest);
  CheckEqual(Sha1DigestToString(Digest), 'de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9');
  // from https://www.ietf.org/rfc/rfc6070.txt
  Pbkdf2HmacSha1('password', 'salt', 1, Digest);
  s := Sha1DigestToString(Digest);
  CheckEqual(s, '0c60c80f961f0e71f3a9b524af6012062fe037a6');
  Pbkdf2HmacSha1('password', 'salt', 2, Digest);
  s := Sha1DigestToString(Digest);
  CheckEqual(s, 'ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957');
  Pbkdf2HmacSha1('password', 'salt', 4096, Digest);
  s := Sha1DigestToString(Digest);
  CheckEqual(s, '4b007901b765489abead49d926f721d065a429c1');
end;


procedure TTestCoreCrypto._SHA256;

  procedure SingleTest(const s: RawByteString; const TDig: TSha256Digest);
  var
    SHA: TSha256;
    Digest: TSha256Digest;
    i: PtrInt;
  begin
    // 1. Hash complete RawByteString
    SHA.Full(pointer(s), length(s), Digest);
    Check(IsEqual(Digest, TDig));
    // 2. one update call for all chars
    SHA.Init;
    for i := 1 to length(s) do
      SHA.Update(@s[i], 1);
    SHA.Final(Digest);
    Check(IsEqual(Digest, TDig));
    {$ifdef USE_OPENSSL}
    if TOpenSslHash.IsAvailable then
    begin
      CheckEqual(TOpenSslHash.Hash('sha256', s), Sha256DigestToString(TDig));
      CheckEqual(TOpenSslHash.Hash('', s), Sha256DigestToString(TDig));
    end;
    {$endif USE_OPENSSL}
  end;

  procedure DoTest;
  const
    D1: TSha256Digest = ($ba, $78, $16, $bf, $8f, $01, $cf, $ea, $41, $41, $40,
      $de, $5d, $ae, $22, $23, $b0, $03, $61, $a3, $96, $17, $7a, $9c, $b4, $10,
      $ff, $61, $f2, $00, $15, $ad);
    D2: TSha256Digest = ($24, $8d, $6a, $61, $d2, $06, $38, $b8, $e5, $c0, $26,
      $93, $0c, $3e, $60, $39, $a3, $3c, $e4, $59, $64, $ff, $21, $67, $f6, $ec,
      $ed, $d4, $19, $db, $06, $c1);
    D3: TSha256Digest = ($94, $E4, $A9, $D9, $05, $31, $23, $1D, $BE, $D8, $7E,
      $D2, $E4, $F3, $5E, $4A, $0B, $F4, $B3, $BC, $CE, $EB, $17, $16, $D5, $77,
      $B1, $E0, $8B, $A9, $BA, $A3);
    DIG4096 = 'c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a';
  var
    Digest: THash512Rec;
    Digests: THash256DynArray;
    sign: TSynSigner;
    c: AnsiChar;
    i: PtrInt;
    sha: TSha256;
  begin
    SingleTest('abc', D1);
    SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
    {%H-}Sha256Weak('lagrangehommage', Digest.Lo); // test with len=256>64
    Check(IsEqual(Digest.Lo, D3));
    {$ifdef USE_OPENSSL}
    if TOpenSslHmac.IsAvailable then
      CheckEqual(TOpenSslHmac.Hmac('', 'what do ya want for nothing?', 'Jefe'),
        '5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843');
    {$endif USE_OPENSSL}
    Pbkdf2HmacSha256('password', 'salt', 1, Digest.Lo);
    check(Sha256DigestToString(Digest.Lo) =
      '120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b');
    Pbkdf2HmacSha256('password', 'salt', 2, Digest.Lo);
    check(Sha256DigestToString(Digest.Lo) =
      'ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43');
    SetLength(Digests, 2);
    check(IsZero(Digests[0]));
    check(IsZero(Digests[1]));
    Pbkdf2HmacSha256('password', 'salt', 2, Digests);
    check(IsEqual(Digests[0], Digest.Lo));
    check(not IsEqual(Digests[1], Digest.Lo));
    check(Sha256DigestToString(Digests[1]) =
      '830651afcb5c862f0b249bd031f7a67520d136470f5ec271ece91c07773253d9');
    Pbkdf2HmacSha256('password', 'salt', 4096, Digest.Lo);
    check(Sha256DigestToString(Digest.Lo) = DIG4096);
    FillZero(Digest.b);
    sign.Pbkdf2(saSha256, 'password', 'salt', 4096, Digest);
    check(Sha256DigestToString(Digest.Lo) = DIG4096);
    c := 'a';
    sha.Init;
    for i := 1 to 1000000 do
      sha.Update(@c, 1);
    sha.Final(Digest.Lo);
    Check(Sha256DigestToString(Digest.Lo) =
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
  Key1:   array[0..4] of byte = ($61, $8A, $63, $D2, $FB);
  InDat:  array[0..4] of byte = ($DC, $EE, $4C, $F9, $2C);
  OutDat: array[0..4] of byte = ($F1, $38, $29, $C9, $DE);
  Test1:  array[0..7] of byte = ($01, $23, $45, $67, $89, $ab, $cd, $ef);
  Res1:   array[0..7] of byte = ($75, $b7, $87, $80, $99, $e0, $c5, $96);
  Key2:   array[0..3] of byte = ($ef, $01, $23, $45);
  Test2:  array[0..9] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Res2:   array[0..9] of byte = ($d6, $a1, $41, $a7, $ec, $3c, $38, $df, $bd, $61);
var
  rc4, ref: TRC4;
  dat: array[0..9] of byte;
  bak: TRC4;
  key, s, d: RawByteString;
  ks, i, len: integer;
begin
  rc4.Init(Test1, 8);
  rc4.Encrypt(Test1, dat, 8);
  Check(CompareMem(@dat, @Res1, SizeOf(Res1)));
  rc4.Init(Key2, 4);
  rc4.Encrypt(Test2, dat, 10);
  Check(CompareMem(@dat, @Res2, SizeOf(Res2)));
  rc4.Init(Key1, SizeOf(Key1));
  rc4.Encrypt(InDat, dat, SizeOf(InDat));
  Check(CompareMem(@dat, @OutDat, SizeOf(OutDat)));
  rc4.Init(Key1, SizeOf(Key1));
  bak := rc4;
  rc4.Encrypt(InDat, dat, SizeOf(InDat));
  Check(CompareMem(@dat, @OutDat, SizeOf(OutDat)));
  rc4 := bak;
  rc4.Encrypt(InDat, dat, SizeOf(InDat));
  Check(CompareMem(@dat, @OutDat, SizeOf(OutDat)));
  rc4 := bak;
  rc4.Encrypt(OutDat, dat, SizeOf(InDat));
  Check(CompareMem(@dat, @InDat, SizeOf(OutDat)));
  key := RandomString(100);
  for ks := 1 to 10 do
  begin
    ref.InitSha3(pointer(key)^, ks * 10);
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

  procedure Test(const password, secret, expected: RawUtf8; rounds: integer = 0);
  var
    dig: THash512Rec;
    sign: TSynSigner;
  begin
    if rounds = 0 then
    begin
      HmacSha512(password, secret, dig.b);
      Check(Sha512DigestToString(dig.b) = expected);
      sign.Init(saSha512, password);
      sign.Update(secret);
      Check(sign.final = expected);
      {$ifdef USE_OPENSSL}
      if TOpenSslHmac.IsAvailable and
         (password <> '') then
        CheckEqual(TOpenSslHmac.Hmac(
          'sha512', secret, pointer(password), length(password)), expected);
      {$endif USE_OPENSSL}
    end
    else
    begin
      Pbkdf2HmacSha512(password, secret, rounds, dig.b);
      Check(Sha512DigestToString(dig.b) = expected);
      FillZero(dig.b);
      sign.Pbkdf2(saSha512, password, secret, rounds, dig);
      Check(Sha512DigestToString(dig.b) = expected);
    end;
  end;

const
  FOX: RawByteString = 'The quick brown fox jumps over the lazy dog';
var
  dig: THash512Rec;
  i: PtrInt;
  sha: TSha512;
  c: AnsiChar;
  temp: RawByteString;
begin
  // includes SHA-384, which is a truncated SHA-512
  Check(SHA384('') =
    '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63' +
    'f6e1da274edebfe76f65fbd51ad2f14898b95b');
  Check(SHA384('abc') =
    'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605' +
    'a43ff5bed8086072ba1e7cc2358baeca134c825a7');
  Check(SHA384(
    'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn' +
    'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu') = '09330c33f711' +
    '47e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039');
  Check(Sha512('') =
    'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d' +
    '36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e');
  Check(Sha512(FOX) =
    '07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785' +
    '436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
  Check(Sha512(FOX + '.') =
    '91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d128' +
    '90cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed');
  sha.Init;
  for i := 1 to length(FOX) do
    sha.Update(@FOX[i], 1);
  sha.Final(dig.b);
  Check(Sha512DigestToString(dig.b) = '07e547d9586f6a73f73fbac0435ed76951218fb7d0c' +
    '8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
  {$ifdef USE_OPENSSL}
  if TOpenSslHash.IsAvailable then
    CheckEqual(TOpenSslHash.Hash('sha512', ''),
      'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d' +
      '36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e');
    CheckEqual(TOpenSslHash.Hash('sha512', FOX),
      '07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785' +
      '436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
  {$endif USE_OPENSSL}
  c := 'a';
  sha.Init;
  for i := 1 to 1000 do
    sha.Update(@c, 1);
  sha.Final(dig.b);
  Check(Sha512DigestToString(dig.b) =
    '67ba5535a46e3f86dbfbed8cbbaf0125c76ed549ff8' +
    'b0b9e03e0c88cf90fa634fa7b12b47d77b694de488ace8d9a65967dc96df599727d3292a8d9d447709c97');
  SetLength(temp, 1000);
  FillCharFast(pointer(temp)^, 1000, ord('a'));
  Check(Sha512(temp) = Sha512DigestToString(dig.b));
  for i := 1 to 1000000 do
    sha.Update(@c, 1);
  sha.Final(dig.b);
  Check(Sha512DigestToString(dig.b) =
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
  HmacSha256('Jefe', 'what do ya want for nothing?', dig.Lo);
  CheckEqual(Sha256DigestToString(dig.Lo),
    '5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843');
  HmacSha384('Jefe', 'what do ya want for nothing?', dig.b384);
  Check(Sha384DigestToString(dig.b384) = 'af45d2e376484031617f78d2b58a6b1' +
    'b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649');
  Pbkdf2HmacSha384('password', 'salt', 4096, dig.b384);
  Check(Sha384DigestToString(dig.b384) = '559726be38db125bc85ed7895f6e3cf574c7a01c' +
    '080c3447db1e8a76764deb3c307b94853fbe424f6488c5f4f1289626');
  Pbkdf2HmacSha512('passDATAb00AB7YxDTT', 'saltKEYbcTcXHCBxtjD', 1, dig.b);
  Check(Sha512DigestToString(dig.b) = 'cbe6088ad4359af42e603c2a33760ef9d4017a7b2aad10af46' +
    'f992c660a0b461ecb0dc2a79c2570941bea6a08d15d6887e79f32b132e1c134e9525eeddd744fa');
  Pbkdf2HmacSha384('passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqK',
    'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcG', 1, dig.b384);
  Check(Sha384DigestToString(dig.b384) =
    '0644a3489b088ad85a0e42be3e7f82500ec189366' +
    '99151a2c90497151bac7bb69300386a5e798795be3cef0a3c803227');
  { // rounds=100000 is slow, so not tested by default
  Pbkdf2HmacSha512('passDATAb00AB7YxDTT','saltKEYbcTcXHCBxtjD',100000,dig);
  Check(Sha512DigestToString(dig)='accdcd8798ae5cd85804739015ef2a11e32591b7b7d16f76819b30'+
    'b0d49d80e1abea6c9822b80a1fdfe421e26f5603eca8a47a64c9a004fb5af8229f762ff41f');
  Pbkdf2HmacSha384('passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqK','saltKEYbcTcXHCBxtj'+
    'D2PnBh44AIQ6XUOCESOhXpEp3HrcG',100000,PHash384(@dig)^);
  Check(Sha384DigestToString(PHash384(@dig)^)='bf625685b48fe6f187a1780c5cb8e1e4a7b0dbd'+
    '6f551827f7b2b598735eac158d77afd3602383d9a685d87f8b089af30');
  }
end;

procedure TTestCoreCrypto._SHA3;
const
  HASH1 = '79f38adec5c20307a98ef76e8324afbfd46cfd81b22e3973c65fa1bd9de31787';
  DK = '7bbdbe37ea70dd2ed640837ff8a926d381806ffa931695addd38ab950d35ad1880' +
    '1a8290e8d97fe14cdfd3cfdbcd0fe766d3e6e4636bd0a17d710a61678db363';
var
  instance: TSha3;
  secret, data, encrypted: RawByteString;
  dig: THash256;
  h512: THash512Rec;
  s, i: PtrInt;
  sign: TSynSigner;
begin
  // validate against official NIST vectors
  // taken from http://csrc.nist.gov/groups/ST/toolkit/examples.html#aHashing
  // see also https://www.di-mgt.com.au/sha_testvectors.html
  CheckEqual(instance.FullStr(SHA3_224, nil, 0),
    '6B4E03423667DBB73B6E15454F0EB1ABD4597F9A1B078E3F5B5A6BC7');
  CheckEqual(instance.FullStr(SHA3_256, nil, 0),
    'A7FFC6F8BF1ED76651C14756A061D662F580FF4DE43B49FA82D80A4B80F8434A');
  CheckEqual(instance.FullStr(SHA3_384, nil, 0),
    '0C63A75B845E4F7D01107D852E4C2485C51A50AAAA94FC61995E71BBEE983A2AC3713831264ADB47FB6BD1E058D5F004');
  CheckEqual(instance.FullStr(SHA3_512, nil, 0),
    'A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80A615B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26');
  CheckEqual(instance.FullStr(SHAKE_128, nil, 0),
    '7F9C2BA4E88F827D616045507605853ED73B8093F6EFBC88EB1A6EACFA66EF26');
  CheckEqual(instance.FullStr(SHAKE_256, nil, 0),
    '46B9DD2B0BA88D13233B3FEB743EEB243FCD52EA62B81B82B50C27646ED5762FD75DC4DDD8C0F200CB05019D67B592F6FC821C49479AB48640292EACB3B7C4BE');
  SetLength(data, 200);
  FillCharFast(pointer(data)^, 200, $A3);
  CheckEqual(instance.FullStr(SHA3_224, pointer(data), length(data)),
    '9376816ABA503F72F96CE7EB65AC095DEEE3BE4BF9BBC2A1CB7E11E0');
  CheckEqual(instance.FullStr(SHA3_256, pointer(data), length(data)),
    '79F38ADEC5C20307A98EF76E8324AFBFD46CFD81B22E3973C65FA1BD9DE31787');
  CheckEqual(instance.FullStr(SHA3_384, pointer(data), length(data)),
    '1881DE2CA7E41EF95DC4732B8F5F002B189CC1E42B74168ED1732649CE1DBCDD76197A31FD55EE989F2D7050DD473E8F');
  CheckEqual(instance.FullStr(SHA3_512, pointer(data), length(data)),
    'E76DFAD22084A8B1467FCF2FFA58361BEC7628EDF5F3FDC0E4805DC48CAEECA81B7C13C30ADF52A3659584739A2DF46BE589C51CA1A4A8416DF6545A1CE8BA00');
  {$ifdef ASMX64AVXNOCONST}
  if cpuAVX2 in X64CpuFeatures then
  begin
    exclude(X64CpuFeatures, cpuAVX2); // validate plain x86_64 asm version
    CheckEqual(instance.FullStr(SHA3_256, nil, 0),
      'A7FFC6F8BF1ED76651C14756A061D662F580FF4DE43B49FA82D80A4B80F8434A');
    CheckEqual(instance.FullStr(SHA3_256, pointer(data), length(data)),
      '79F38ADEC5C20307A98EF76E8324AFBFD46CFD81B22E3973C65FA1BD9DE31787');
    include(X64CpuFeatures, cpuAVX2);
  end;
  {$endif ASMX64AVXNOCONST}
  instance.Init(SHA3_256);
  for i := 1 to length(data) do
    instance.Update(pointer(data), 1);
  instance.Final(dig);
  Check(Sha256DigestToString(dig) = HASH1);
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
  CheckEqual(Sha256DigestToString(dig), HASH1);
  instance.Final(dig, true);
  CheckEqual(Sha256DigestToString(dig),
    'f85500852a5b9bb4a35440e7e4b4dba9184477a4c97b97ab0b24b91a8b04d1c8');
  for i := 1 to 200 do
  begin
    FillZero(dig);
    instance.Final(dig, true);
    Check(not IsZero(dig), 'Sha3 XOF mode');
  end;
  instance.Final(dig);
  CheckEqual(Sha256DigestToString(dig),
    '75f8b0591e2baeae027d56c14ef3bc014d9dd29cce08b8b184528589147fc252',
    'Sha3 XOF vector');
  encrypted := instance.Cypher('secret', 'toto');
  CheckEqual(mormot.core.text.BinToHex(encrypted), 'BF013A29');
  CheckEqual(BinToHexLower(encrypted), 'bf013a29');
  for s := 0 to 3 do
  begin
    secret := RandomString(s * 3);
    Check(instance.Cypher(secret, '') = '');
    for i := 1 to 1000 do
    begin
      data := RandomString(i);
      {$ifdef FPC}
      SetCodePage(data, CP_RAWBYTESTRING, {convert=}false);
      {$endif FPC}
      encrypted := instance.Cypher(secret, data);
      Check((i < 16) or
            (encrypted <> data));
      instance.InitCypher(secret);
      Check(instance.Cypher(encrypted) = data);
    end;
  end;
  Pbkdf2Sha3(SHA3_512, 'pass', 'salt', 1000, @h512);
  check(Sha512DigestToString(h512.b) = DK);
  FillZero(h512.b);
  sign.Pbkdf2(saSha3512, 'pass', 'salt', 1000, h512);
  check(Sha512DigestToString(h512.b) = DK);
  // taken from https://en.wikipedia.org/wiki/SHA-3
  CheckEqual(Sha3(SHAKE_128, 'The quick brown fox jumps over the lazy dog'),
    'F4202E3C5852F9182A0430FD8144F0A74B95E7417ECAE17DB0F8CFEED0E3E66E');
  CheckEqual(Sha3(SHAKE_128, 'The quick brown fox jumps over the lazy dof'),
    '853F4538BE0DB9621A6CEA659A06C1107B1F83F02B13D18297BD39D7411CF10C');
end;

procedure TTestCoreCrypto._TAesPNRG;
var
  timer: TPrecisionTimer;
  i: integer;
  big: RawByteString;
begin
  check(TAesPrng.IsAvailable);
  check(TSystemPrng.IsAvailable);
  Prng(TAesPrng, 'mORMot');
  {$ifdef USE_OPENSSL}
  Prng(TAesPrngOsl, 'OpenSSL');
  {$endif USE_OPENSSL}
  // same benchmarks as in Prng()
  timer.Start;
  Check(Random32(0) = 0);
  for i := 1 to 50000 do
    Check(Random32(i) < cardinal(i));
  for i := 0 to 50000 do
    Check(Random32(maxInt - i) < cardinal(maxInt - i));
  NotifyTestSpeed('Lecuyer Random32', [], 50000 * 2, 50000 * 8, @timer);
  SetLength(big, 100000);
  timer.Start;
  RandomBytes(pointer(big), length(big));
  NotifyTestSpeed('Lecuyer RandomBytes', [], 1, length(big), @timer);
end;

procedure TTestCoreCrypto.Prng(meta: TAesPrngClass; const name: RawUTF8);
var
  p: TAesPrngAbstract;
  b1, b2: TAesBlock;
  a1, a2: TAesPrngAbstract;
  s1, s2, split, big: RawByteString;
  c: cardinal;
  d: double;
  e: TSynExtended;
  i, stripes: PtrInt;
  clo, chi, dlo, dhi, elo, ehi: integer;
  timer: TPrecisionTimer;
begin
  if not meta.IsAvailable then
    exit;
  p := meta.Main;
  p.FillRandom(b1);
  p.FillRandom(b2);
  Check(not IsEqual(b1, b2));
  Check(not CompareMem(@b1, @b2, SizeOf(b1)));
  clo := 0;
  chi := 0;
  dlo := 0;
  dhi := 0;
  elo := 0;
  ehi := 0;
  a1 := meta.Create;
  a2 := meta.Create;
  try
    a1.FillRandom(b1);
    a2.FillRandom(b2);
    Check(not IsEqual(b1, b2));
    Check(not CompareMem(@b1, @b2, SizeOf(b1)));
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
      // verify Random32 / RandomDouble / RandomDouble distribution
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
      e := a1.RandomExt;
      check((e >= 0) and (e < 1));
      if e < 0.5 then
        inc(elo)
      else
        inc(ehi);
      e := a2.RandomExt;
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
  CheckUtf8((clo >= 900) and
            (clo <= 1100), 'Random32 distribution clo=%', [clo]);
  CheckUtf8((dlo >= 1800) and
            (dlo <= 2200), 'RandomDouble distribution dlo=%', [dlo]);
  CheckUtf8((elo >= 1800) and
            (elo <= 2200), 'RandomExt distribution elo=%', [elo]);
  s1 := p.FillRandom(100);
  for i := 1 to length(s1) do
    for stripes := 0 to 10 do
    begin
      split := p.AFSplit(pointer(s1)^, i, stripes);
      check(length(split) = i * (stripes + 1));
      check(TAesPrng.AFUnsplit(split, pointer(s2)^, i));
      check(CompareMem(pointer(s1), pointer(s2), i));
    end;
  check(PosEx(s1, split) = 0);
  timer.Start;
  Check(p.Random32(0) = 0);
  for i := 1 to 50000 do
    Check(p.Random32(i) < cardinal(i));
  for i := 0 to 50000 do
    Check(p.Random32(maxInt - i) < cardinal(maxInt - i));
  NotifyTestSpeed('% Random32', [name], 50000 * 2, 50000 * 8, @timer);
  SetLength(big, 100000);
  timer.Start;
  p.FillRandom(pointer(big), length(big));
  NotifyTestSpeed('% FillRandom', [name], 1, length(big), @timer);
end;

procedure TTestCoreCrypto.CryptData(dpapi: boolean);
var
  i, size: integer;
  plain, enc, test: RawByteString;
  appsec: RawUtf8;
  func: function(const Data, AppSecret: RawByteString; Encrypt: boolean): RawByteString;
  tim: TPrecisionTimer;
const
  MAX = 1000;
begin
  {$ifdef OSWINDOWS}
  if dpapi then
    func := CryptDataForCurrentUserDPAPI
  else
  {$endif OSWINDOWS}
    func := CryptDataForCurrentUser;
  func('warmup', 'appsec', true);
  size := 0;
  tim.Start;
  for i := 0 to MAX - 1 do
  begin
    plain := TAesPrng.Main.FillRandom(i);
    check(length(plain) = i);
    UInt32ToUtf8(i, appsec);
    enc := func(plain, appsec, true);
    if not ((plain = '') or
            (enc <> '')) then
      enc := func(plain, appsec, true);
    check((plain = '') or
          (enc <> ''));
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

{$ifdef OSWINDOWS}
procedure TTestCoreCrypto._CryptDataForCurrentUserApi;
begin
  CryptData(true);
end;
{$endif OSWINDOWS}

procedure TTestCoreCrypto._JWT;

  procedure test(one: TJwtAbstract);
  var
    t, sub, iss, hp: RawUtf8;
    jwt: TJwtContent;
    i: integer;
    exp: TUnixTime;
    exp64: Int64;
    bool: boolean;
    v: variant;
  begin
    t := one.Compute(['http://example.com/is_root', true], 'joe');
    check(t <> '');
    check(TJwtAbstract.VerifyPayload(
      t, '', '', 'joe', '', @exp, nil, @sub, @iss, @hp) = jwtValid);
    checkEqual(sub, '');
    checkEqual(iss, 'joe');
    if one.Algorithm = 'none' then
      checkEqual(hp + '.', t);
    check(TJwtAbstract.VerifyPayload(
      t, '', '', 'joe', '', nil, nil, nil, nil, nil, @v) = jwtValid);
    // {"http://example.com/is_root":true,"iss":"joe","exp":1658258457}
    check(_Safe(v)^.Count >= 3);
    with _Safe(v)^ do
    begin
      Check(GetAsInt64('exp', exp64));
      CheckEqual(exp, exp64);
      Check(GetAsRawUtf8('iss', iss));
      checkEqual(iss, 'joe');
      bool := false;
      Check(GetAsBoolean('http://example.com/is_root', bool));
      check(bool);
    end;
    check(one.VerifyPayload(
      t, one.Algorithm, '', 'joe', '', @exp, nil, @sub, @iss, nil) = jwtValid);
    checkEqual(one.ExtractAlgo(t), one.Algorithm);
    checkEqual(one.ExtractAlgo(copy(t, 2, 1000)), '');
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
    if jwt.result = jwtValid then
      for i := 1 to 1000 do
      begin
        Finalize(jwt);
        FillCharFast(jwt, SizeOf(jwt), 0);
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

  procedure Benchmark(J: TJwtAbstract; N: integer = 1000);
  var
    i: integer;
    tok: RawUtf8;
    jwt: TJwtContent;
    tim: TPrecisionTimer;
  begin
    try
      tok := J.Compute([], 'myself');
      tim.Start;
      for i := 2 to N do
      begin
        jwt.result := jwtWrongFormat;
        J.Verify(tok, jwt);
        check(jwt.result = jwtValid);
        check(jwt.reg[jrcIssuer] = 'myself');
      end;
      inc(tok[length(tok) - 5]);
      jwt.result := jwtWrongFormat;
      J.Verify(tok, jwt);
      check(jwt.result = jwtInvalidSignature, 'detection');
      NotifyTestSpeed('%', [J.Algorithm], N, 0, @tim);
    finally
      J.Free;
    end;
  end;

{$ifdef USE_OPENSSL}
const
  OSSL_JWT: array[0..10] of TJwtAbstractOslClass = (
    TJwtRS256Osl,
    TJwtRS384Osl,
    TJwtRS512Osl,
    TJwtPS256Osl,
    TJwtPS384Osl,
    TJwtPS512Osl,
    TJwtES256Osl,
    TJwtES384Osl,
    TJwtES512Osl,
    TJwtES256KOsl,
    TJwtEdDSAOsl);
var
  priv, pub: RawUtf8;
{$endif USE_OPENSSL}
var
  i: integer;
  j: TJwtAbstract;
  jwt: TJwtContent;
  secret: TEccCertificateSecret;
  tok: RawUtf8;
  a: TSignAlgo;
begin
  test(TJwtNone.Create(
    [jrcIssuer, jrcExpirationTime], [], 60));
  test(TJwtNone.Create(
    [jrcIssuer, jrcExpirationTime, jrcIssuedAt], [], 60));
  test(TJwtNone.Create(
    [jrcIssuer, jrcExpirationTime, jrcIssuedAt, jrcJWTID], [], 60));
  test(TJwtHS256.Create(
    'sec', 100, [jrcIssuer, jrcExpirationTime], [], 60));
  test(TJwtHS256.Create(
    'sec', 200, [jrcIssuer, jrcExpirationTime, jrcIssuedAt], [], 60));
  test(TJwtHS256.Create(
    'sec', 10, [jrcIssuer, jrcExpirationTime, jrcIssuedAt, jrcJWTID], [], 60));
  j := TJwtHS256.Create(
    'secret', 0, [jrcSubject], []);
  try
    jwt.result := jwtWrongFormat;
    j.Verify(
      'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibm' +
      'FtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeF' +
      'ONFh7HgQ', jwt); // reference from jwt.io
    check(jwt.result = jwtValid);
    check(jwt.reg[jrcSubject] = '1234567890');
    check(jwt.data.U['name'] = 'John Doe');
    check(jwt.data.B['admin']);
    j.Verify(
      'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibm' +
      'FtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeF' +
      'ONFh7hgQ', jwt); // altered one char in signature
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
    secret := TEccCertificateSecret.CreateNew(nil); // self-signed certificate
    test(TJwtES256.Create(secret,
      [jrcIssuer, jrcExpirationTime], [], 60));
    test(TJwtES256.Create(secret,
      [jrcIssuer, jrcExpirationTime, jrcIssuedAt], [], 60));
    test(TJwtES256.Create(secret,
      [jrcIssuer, jrcExpirationTime, jrcIssuedAt, jrcJWTID], [], 60));
    secret.Free;
  end;
  for a := saSha256 to high(a) do
    Benchmark(JWT_CLASS[a].Create(
      'secret', 0, [jrcIssuer, jrcExpirationTime], []));
  secret := TEccCertificateSecret.CreateNew(nil);
  try
    Benchmark(TJwtES256.Create(
      secret, [jrcIssuer, jrcExpirationTime], [], 60), 100);
  finally
    secret.Free;
  end;
  {$ifdef USE_OPENSSL}
  for i := 0 to high(OSSL_JWT) do
    if OSSL_JWT[i].IsAvailable then
    begin
      // RSA is very slow at key computing and signing, but fast to verify
      OSSL_JWT[i].GenerateKeys(priv, pub);
      Benchmark(OSSL_JWT[i].Create(
        priv, pub, '', '', [jrcIssuer, jrcExpirationTime], [], 60), 100);
    end;
  {$endif USE_OPENSSL}
end;

type
  TBenchmark = (
    // non cryptographic hashes
    bCRC32c, bXXHash32, bCRC32, bAdler32, bHash32, bAesniHash,
    // cryptographic hashes
    bMD5,
    bSHA1, bHMACSHA1, bSHA256, bHMACSHA256,
    bSHA384, bHMACSHA384, bSHA512, bHMACSHA512,
    bSHA3_256, bSHA3_512,
    // encryption
    bRC4,
    bAES128CFB, bAES128OFB, bAES128C64, bAES128CTR,
    bAES128CFC, bAES128OFC, bAES128CTC, bAES128GCM,
    bAES256CFB, bAES256OFB, bAES256C64, bAES256CTR,
    bAES256CFC, bAES256OFC, bAES256CTC, bAES256GCM,
  {$ifdef USE_OPENSSL}
    bAES128CFBO, bAES128OFBO, bAES128CTRO, bAES128GCMO,
    bAES256CFBO, bAES256OFBO, bAES256CTRO, bAES256GCMO,
  {$endif USE_OPENSSL}
    bSHAKE128, bSHAKE256);

procedure TTestCoreCrypto.Benchmark;
const
  bAESLAST = {$ifdef USE_OPENSSL} bAES256GCMO {$else} bAES256GCM {$endif};
  bAESOPENSSL = [ {$ifdef USE_OPENSSL} bAES128CFBO .. bAES256GCMO {$endif} ];
  SIZ: array[0..4] of integer = (
    8,
    50,
    100,
    1000,
    10000);
  COUNT = 500;
  AESCLASS: array[bAES128CFB.. bAESLAST] of TAesAbstractClass = (
    TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesCfc, TAesOfc, TAesCtc, TAesGcm,
    TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesCfc, TAesOfc, TAesCtc, TAesGcm
  {$ifdef USE_OPENSSL} ,
    TAesCfbOsl, TAesOfbOsl, TAesCtrOsl, TAesGcmOsl,
    TAesCfbOsl, TAesOfbOsl, TAesCtrOsl, TAesGcmOsl
  {$endif USE_OPENSSL});
  AESBITS: array[bAES128CFB..bAESLAST] of integer = (
    128, 128, 128, 128, 128, 128, 128, 128,
    256, 256, 256, 256, 256, 256, 256, 256
  {$ifdef USE_OPENSSL} ,
    128, 128, 128, 128, 256, 256, 256, 256
  {$endif USE_OPENSSL});
var
  b: TBenchmark;
  s, i, size, n: integer;
  data, encrypted: RawByteString;
  dig: THash512Rec;
  MD5: TMd5;
  SHA1: TSha1;
  SHA256: TSha256;
  SHA384: TSha384;
  SHA512: TSha512;
  SHA3, SHAKE128, SHAKE256: TSha3;
  RC4: TRC4;
  timer: TPrecisionTimer;
  time: array[TBenchmark] of Int64;
  AES: array[bAES128CFB..bAESLAST] of TAesAbstract;
  TXT: array[TBenchmark] of RawUtf8;
begin
  GetEnumTrimmedNames(TypeInfo(TBenchmark), @TXT);
  for b := low(b) to high(b) do
    TXT[b] := LowerCase(TXT[b]);
  for b := low(AES) to high(AES) do
    if AESCLASS[b].IsAvailable then
    begin
      AES[b] := AESCLASS[b].Create(dig{%H-}, AESBITS[b]);
      ShortStringToAnsi7String(AES[b].AlgoName, TXT[b]);
      {$ifdef USE_OPENSSL}
      if b in bAESOPENSSL then
        TXT[b] := 'openssl ' + TXT[b]
      else
      {$endif USE_OPENSSL}
        TXT[b] := 'mormot ' + TXT[b]
    end
    else
      AES[b] := nil;
  SHAKE128.InitCypher('secret', SHAKE_128);
  SHAKE256.InitCypher('secret', SHAKE_256);
  RC4.InitSha3(dig, SizeOf(dig));
  FillCharFast(time, SizeOf(time), 0);
  size := 0;
  n := 0;
  for s := 0 to high(SIZ) do
  begin
    data := RandomString(SIZ[s]);
    SetLength(encrypted, SIZ[s]);
    for b := low(b) to high(b) do
    if (b < low(AES)) or
       (b > high(AES)) or
       (AES[b] <> nil) then
    begin
      if (b = bAesniHash) and
         not Assigned(AesNiHash32) then
        continue;
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
          bAesniHash:
            dig.d0 := AesNiHash64(0, pointer(data), SIZ[s]);
          bCRC32c:
            dig.d0 := crc32c(0, pointer(data), SIZ[s]);
          bAdler32:
            dig.d0 := adler32(0, pointer(data), SIZ[s]);
          bCRC32:
            dig.d0 := crc32(0, pointer(data), SIZ[s]);
          bMD5:
            MD5.Full(pointer(data), SIZ[s], dig.h0);
          bSHA1:
            SHA1.Full(pointer(data), SIZ[s], dig.b160);
          bHMACSHA1:
            HmacSha1('secret', data, dig.b160);
          bSHA256:
            SHA256.Full(pointer(data), SIZ[s], dig.Lo);
          bHMACSHA256:
            HmacSha256('secret', data, dig.Lo);
          bSHA384:
            SHA384.Full(pointer(data), SIZ[s], dig.b384);
          bHMACSHA384:
            HmacSha384('secret', data, dig.b384);
          bSHA512:
            SHA512.Full(pointer(data), SIZ[s], dig.b);
          bHMACSHA512:
            HmacSha512('secret', data, dig.b);
          bSHA3_256:
            SHA3.Full(pointer(data), SIZ[s], dig.Lo);
          bSHA3_512:
            SHA3.Full(pointer(data), SIZ[s], dig.b);
          bRC4:
            RC4.EncryptBuffer(pointer(data), pointer(encrypted), SIZ[s]);
          {$ifdef USE_OPENSSL}
          bAES128CFBO, bAES128OFBO, bAES128CTRO,
          bAES256CFBO, bAES256OFBO, bAES256CTRO,
          {$endif USE_OPENSSL}
          bAES128CFB, bAES128OFB, bAES128C64, bAES128CTR,
          bAES256CFB, bAES256OFB, bAES256C64, bAES256CTR:
            AES[b].EncryptPkcs7(data, {encrypt=}true);
          {$ifdef USE_OPENSSL}
          bAES128GCMO, bAES256GCMO,
          {$endif USE_OPENSSL}
          bAES128CFC, bAES128OFC, bAES128CTC, bAES128GCM,
          bAES256CFC, bAES256OFC, bAES256CTC, bAES256GCM:
            AES[b].MacAndCrypt(data, {encrypt=}true, {ivatbeg=}true);
          bSHAKE128:
            SHAKE128.Cypher(pointer(data), pointer(encrypted), SIZ[s]);
          bSHAKE256:
            SHAKE256.Cypher(pointer(data), pointer(encrypted), SIZ[s]);
        else
          raise ESynCrypto.CreateUtf8('Unexpected %', [TXT[b]]);
        end;
        Check((b >= bRC4) or
              (dig.d0 <> 0) or
              (dig.d1 <> 0));
      end;
      inc(time[b], NotifyTestSpeed('% %', [TXT[b], SIZ[s]], COUNT, SIZ[s] *
        COUNT, @timer, {onlylog=}true));
      //if b in [bSHA3_512,high(b)] then AddConsole('');
    end;
    inc(size, SIZ[s] * COUNT);
    inc(n, COUNT);
  end;
  for b := low(b) to high(b) do
    if time[b] <> 0 then
      AddConsole(FormatString('% % in % i.e. %/s or %/s', [n, TXT[b],
        MicroSecToString(time[b]), K((Int64(n) * 1000000) div time[b]),
        KB((Int64(size) * 1000000) div time[b])]));
  for b := low(AES) to high(AES) do
    AES[b].Free;
end;

const
  HASHESMAX = 512;

function Hash32Test(buf: PAnsiChar; hash: THasher; var expected: cardinal): boolean;
var
  L, modif: PtrInt;
  c, c2 {, s}: cardinal;
begin
  result := false;
  if expected = 0 then
    expected := hash(0, buf, HASHESMAX) // use first call as aligned reference
  else if hash(0, buf, HASHESMAX) <> expected then
  begin
    //writeln('alignement problem');
    exit;
  end;
  for L := 0 to HASHESMAX do
  begin
    c := hash(0, buf, L);
    //s := 0;
    for modif := 0 to L - 1 do
    begin
      inc(buf[modif]);
      c2 := hash(0, buf, L);
      if c2 = c then
      begin
        //writeln('L=',L,' modif=',modif);
        exit; // should detect one modified bit at any position
      end;
      dec(buf[modif]);
      //inc(s, L);
    end;
    if hash(0, buf, L) <> c then
      exit; // should return the same value for the same data
    //inc(s, L);
    // timer.Stop; write(L, '=', GetExecutableLocation(@hash), ' ',
    // KBNoSpace(timer.PerSec(s)), '/s ');
  end; // at the end: s = 45133056 (45MB) for HASHESMAX=512
  {
  Some results for our 32-bit hashes (typical TSynDictionary use):

  On x86_64, for each value of L (0..256):
  4317d0 ../src/core/mormot.core.base.asmx64.inc crc32cfast (389)
    0=0B/s 4=436.5MB/s 8=555.2MB/s 12=499.5MB/s 16=827.8MB/s 20=675.3MB/s
    24=1GB/s 28=812.8MB/s 32=1.1GB/s 36=0.9GB/s 40=1.1GB/s 44=1GB/s 48=1.2GB/s
    52=1.1GB/s 56=1GB/s 60=0.9GB/s 64=1.3GB/s 68=1GB/s 72=1.3GB/s 76=1.2GB/s
    80=1.2GB/s 84=1.2GB/s 88=1.3GB/s 92=1.2GB/s 96=1.2GB/s 100=1.2GB/s
    104=1.2GB/s 108=1.2GB/s 112=1.3GB/s 116=1.2GB/s 120=1.2GB/s 124=1.2GB/s
    128=1.1GB/s 132=1.2GB/s 136=1.3GB/s 140=1.2GB/s 144=1.2GB/s 148=1.2GB/s
    152=1.3GB/s 156=1.3GB/s 160=1.3GB/s 164=1.2GB/s 168=1.3GB/s 172=1.2GB/s
    176=1.3GB/s 180=1.3GB/s 184=1.3GB/s 188=1.3GB/s 192=1.3GB/s 196=1.3GB/s
    200=1.3GB/s 204=1.3GB/s 208=1.2GB/s 212=1.3GB/s 216=1.3GB/s 220=1.3GB/s
    224=1.3GB/s 228=1.2GB/s 232=1.3GB/s 236=1.2GB/s 240=1.3GB/s 244=1.3GB/s
    248=1.3GB/s 252=1.3GB/s 256=1.3GB/s
  4d5610 ../src/core/mormot.crypt.core.asmx64.inc crc32c_sse42_aesni (5279)
    0=0B/s 4=381.9MB/s 8=763.4MB/s 12=1.1GB/s 16=1.4GB/s 20=1.7GB/s
    24=2GB/s 28=2.2GB/s 32=2.5GB/s 36=2.7GB/s 40=3GB/s 44=3.2GB/s 48=3.4GB/s
    52=3.6GB/s 56=3.7GB/s 60=3.9GB/s 64=4.2GB/s 68=3.1GB/s 72=3.3GB/s 76=3.7GB/s
    80=4.8GB/s 84=4.1GB/s 88=4.7GB/s 92=5GB/s 96=5.2GB/s 100=5.3GB/s
    104=5.6GB/s 108=5.5GB/s 112=5.8GB/s 116=5.4GB/s 120=6GB/s 124=5.7GB/s
    128=5.8GB/s 132=5.7GB/s 136=6.2GB/s 140=6.2GB/s 144=6.5GB/s 148=5.9GB/s
    152=6.6GB/s 156=6.2GB/s 160=6.7GB/s 164=6.6GB/s 168=6.5GB/s 172=6.7GB/s
    176=6.9GB/s 180=6.7GB/s 184=6.1GB/s 188=6.3GB/s 192=7.1GB/s 196=6.9GB/s
    200=9GB/s 204=8.3GB/s 208=8.8GB/s 212=8.6GB/s 216=7GB/s 220=8.4GB/s
    224=9.7GB/s 228=9.4GB/s 232=9.5GB/s 236=9.4GB/s 240=10.2GB/s 244=7.8GB/s
    248=5.8GB/s 252=1.9GB/s 256=10.1GB/s
  431ce0 ../src/core/mormot.core.base.asmx64.inc xxhash32 (860)
    0=0B/s 4=235MB/s 8=436.2MB/s 12=624.4MB/s 16=842.1MB/s 20=0.9GB/s
    24=1GB/s 28=1.1GB/s 32=1.5GB/s 36=1.6GB/s 40=1.6GB/s 44=1.7GB/s 48=2.1GB/s
    52=2.1GB/s 56=2GB/s 60=1.9GB/s 64=2.7GB/s 68=2.6GB/s 72=2.3GB/s 76=2.3GB/s
    80=2.9GB/s 84=2.9GB/s 88=2.8GB/s 92=2.8GB/s 96=2.8GB/s 100=3.1GB/s
    104=3.1GB/s 108=3GB/s 112=2.6GB/s 116=2.9GB/s 120=3.1GB/s 124=2.7GB/s
    128=3.1GB/s 132=3GB/s 136=2.9GB/s 140=2.5GB/s 144=3GB/s 148=3GB/s
    152=3.1GB/s 156=3.2GB/s 160=3.2GB/s 164=2.4GB/s 168=3.3GB/s 172=3.2GB/s
    176=3.4GB/s 180=3.3GB/s 184=3.1GB/s 188=3.3GB/s 192=3.4GB/s 196=2.8GB/s
    200=3.4GB/s 204=3.3GB/s 208=3.5GB/s 212=3.1GB/s 216=3.4GB/s 220=3.4GB/s
    224=3.4GB/s 228=3.4GB/s 232=3.5GB/s 236=3.5GB/s 240=3.6GB/s 244=3GB/s
    248=3.6GB/s 252=3.6GB/s 256=3.7GB/s
  4d4fe0 ../src/core/mormot.crypt.core.asmx64.inc _aesnihash32 (4930)
    0=0B/s 4=235MB/s 8=488.5MB/s 12=723MB/s 16=0.9GB/s 20=1.1GB/s
    24=1.3GB/s 28=1.6GB/s 32=1.8GB/ s 36=2GB/s 40=2.2GB/s 44=2.4GB/s 48=2.7GB/s
    52=2.9GB/s 56=3.1GB/s 60=3.4GB/s 64=3.7GB/s 68=3.3GB/s 72=3.5GB/s 76=4GB/s
    80=4.2GB/s 84=4.2GB/s 88=4.7GB/s 92=4.9GB/s 96=5.3GB/s 100=5.5GB/s
    104=5.7GB/s 108=5.7GB/s 112=6GB/s 116=6.4GB/s 120=6.2GB/s 124=6.4GB/s
    128=7GB/s 132=5.5GB/s 136=5.9GB/s 140=6.1GB/s 144=6.2GB/s 148=6.2GB/s
    152=6.5GB/s 156=6.7GB/s 160=6.6GB/s 164=7GB/s 168=5.2GB/s 172=6.6GB/s
    176=7.6GB/s 180=7.7GB/s 184=7.8GB/s 188=8GB/s 192=8GB/s 196=8.2GB/s
    200=8.5GB/s 204=8.7GB/s 208=8.6GB/s 212=7.2GB/s 216=9.4GB/s 220=9.3GB/s
    224=9.5GB/s 228=9.7GB/s 232=9.7GB/s 236=6.8GB/s 240=10.2GB/s 244=8.3GB/s
    248=10.5GB/s 252=10.6GB/s 256=10.2GB/s

  On i386 (Linux/FPC):
  080792b0 ../src/core/mormot.core.base.asmx86.inc crc32cfast (252)
    4=381.9MB/s 8=488.5MB/s 12=528.4MB/s 16=921.5MB/s 20=726.7MB/s 24=1.1GB/s
    28=906.4MB/s 32=1.3GB/s 36=1GB/s 40=1.4GB/s 44=1.1GB/s 48=1.5GB/s
    52=1.2GB/s 56=1.5GB/s 60=1.3GB/s 64=1.6GB/s 68=1.3GB/s 72=1.6GB/s
    76=1.4GB/s 80=1.4GB/s 84=1.3GB/s 88=1.6GB/s 92=1.4GB/s 96=1.6GB/s
    100=1.5GB/s 104=1.7GB/s 108=1.5GB/s 112=1.6GB/s 116=1.4GB/s 120=1.6GB/s
    124=1.5GB/s 128=1.5GB/s 132=1.5GB/s 136=1.6GB/s 140=1.5GB/s 144=1.6GB/s
    148=1.6GB/s 152=1.6GB/s 156=1.6GB/s 160=1.6GB/s 164=1.6GB/s 168=1.6GB/s
    172=1.4GB/s 176=1.6GB/s 180=1.5GB/s 184=1.6GB/s 188=1.6GB/s 192=1.6GB/s
    196=1.5GB/s 200=1.6GB/s 204=1.5GB/s 208=1.6GB/s 212=1.5GB/s 216=1.6GB/s
    220=1.5GB/s 224=1.6GB/s 228=1.5GB/s 232=1.6GB/s 236=1.5GB/s 240=1.6GB/s
    244=1.5GB/s 248=1.6GB/s 252=1.5GB/s 256=1.6GB/s
  08079d80 ../src/core/mormot.core.base.asmx86.inc crc32csse42 (1445)
    4=436.5MB/s 8=642.8MB/s 12=0.9GB/s 16=1.2GB/s 20=1.3GB/s 24=1.6GB/s
    28=1.9GB/s 32=2GB/s 36=2.3GB/s 40=2.3GB/s 44=2.5GB/s 48=2.5GB/s
    52=2.8GB/s 56=2.7GB/s 60=3GB/s 64=2.9GB/s 68=3.2GB/s 72=3.1GB/s
    76=3GB/s 80=3.2GB/s 84=3.4GB/s 88=3GB/s 92=3.4GB/s 96=3.4GB/s
    100=3.6GB/s 104=3.4GB/s 108=3.4GB/s 112=3.5GB/s 116=3.6GB/s 120=3.6GB/s
    124=3.7GB/s 128=3.6GB/s 132=3.7GB/s 136=3.7GB/s 140=3.8GB/s 144=3.8GB/s
    148=3.8GB/s 152=3.7GB/s 156=3.9GB/s 160=3.8GB/s 164=3.8GB/s 168=3.9GB/s
    172=4GB/s 176=3.8GB/s 180=3.9GB/s 184=3.8GB/s 188=3.9GB/s 192=3.8GB/s
    196=3.9GB/s 200=3.8GB/s 204=3.9GB/s 208=3.8GB/s 212=3.8GB/s 216=3.7GB/s
    220=3.9GB/s 224=3.8GB/s 228=1.9GB/s 232=3.8GB/s 236=3.9GB/s 240=3.8GB/s
    244=3.8GB/s 248=3.9GB/s 252=3.7GB/s 256=3.7GB/s
  08079830 ../src/core/mormot.core.base.asmx86.inc xxhash32 (806)
    4=235MB/s 8=436.2MB/s 12=610.6MB/s 16=842.1MB/s 20=0.9GB/s 24=1GB/s
    28=1.1GB/s 32=1.5GB/s 36=1.6GB/s 40=1.6GB/s 44=1.7GB/s 48=2.2GB/s
    52=2.1GB/s 56=2GB/s 60=1.6GB/s 64=2.6GB/s 68=2.5GB/s 72=2.4GB/s
    76=2.5GB/s 80=2.9GB/s 84=2.9GB/s 88=2.2GB/s 92=2.1GB/s 96=3.2GB/s
    100=2.9GB/s 104=2.7GB/s 108=2.9GB/s 112=3.2GB/s 116=3.2GB/s 120=2.9GB/s
    124=3GB/s 128=2.9GB/s 132=3.2GB/s 136=3GB/s 140=3.1GB/s 144=3.5GB/s
    148=3.3GB/s 152=3.2GB/s 156=3GB/s 160=3.4GB/s 164=3.5GB/s 168=3.3GB/s
    172=3.3GB/s 176=2.4GB/s 180=2.9GB/s 184=3.2GB/s 188=3.4GB/s 192=3.6GB/s
    196=3.5GB/s 200=3.4GB/s 204=3.5GB/s 208=3.6GB/s 212=3.6GB/s 216=3.4GB/s
    220=3.5GB/s 224=3.6GB/s 228=3.5GB/s 232=3.5GB/s 236=3.6GB/s 240=3.6GB/s
    244=3.4GB/s 248=3.1GB/s 252=3.6GB/s 256=3.7GB/s
  0810edd0 ../src/core/mormot.crypt.core.asmx86.inc _aesnihash32 (2638)
    4=235MB/s 8=508.9MB/s 12=785MB/s 16=1GB/s 20=1.2GB/s 24=1.4GB/s
    28=1.7GB/s 32=1.9GB/s 36=2GB/s 40=2.3GB/s 44=2.4GB/s 48=2.8GB/s
    52=3.1GB/s 56=3.3GB/s 60=3.6GB/s 64=3.8GB/s 68=3.3GB/s 72=3.5GB/s
    76=3.7GB/s 80=3.9GB/s 84=4.1GB/s 88=4.3GB/s 92=4.6GB/s 96=4.3GB/s
    100=5.1GB/s 104=5.3GB/s 108=5.4GB/s 112=5.7GB/s 116=5.8GB/s 120=6.1GB/s
    124=6.3GB/s 128=5.6GB/s 132=4.7GB/s 136=5.8GB/s 140=6.1GB/s 144=6.3GB/s
    148=6.1GB/s 152=6.5GB/s 156=7GB/s 160=7.2GB/s 164=7.2GB/s 168=7.6GB/s
    172=7.6GB/s 176=7.6GB/s 180=7.6GB/s 184=8.3GB/s 188=8.6GB/s 192=8.8GB/s
    196=7.7GB/s 200=7.9GB/s 204=7.5GB/s 208=8.3GB/s 212=8.4GB/s 216=8.7GB/s
    220=7.4GB/s 224=8GB/s 228=9.4GB/s 232=9.5GB/s 236=9.7GB/s 240=9.8GB/s
    244=10.1GB/s 248=8.2GB/s 252=10.3GB/s 256=10.5GB/s

  -> aesnihash32 is faster or as fast as very optimized Intel's crc32c+SSE4.2
     on x86_64, blow away everything on i386, and with much better output quality
     - see smhasher report about crc32c: insecure, 100% bias, collisions, distrib,
     BIC, and xxHash32: LongNeighbors, 4bit collisions, MomentChi2 220 - whereas
     https://github.com/tkaitchuck/aHash (same algorithm) passes all tests
  }
  result := true;
end;

function Hash64Test(buf: PAnsiChar; hash: THasher64; var expected: QWord): boolean;
var
  L, modif: PtrInt;
  c, c2: QWord;
begin
  result := false;
  if expected = 0 then
    expected := hash(0, buf, HASHESMAX) // use first call as aligned reference
  else if hash(0, buf, HASHESMAX) <> expected then
  begin
    //writeln('alignement problem');
    exit;
  end;
  for L := 0 to HASHESMAX do
  begin
    c := hash(0, buf, L);
    for modif := 0 to L - 1 do
    begin
      inc(buf[modif]);
      c2 := hash(0, buf, L);
      dec(buf[modif]);
      if c2 = c then
      begin
        //writeln('L=',L,' modif=',modif);
        exit; // should detect one modified bit at any position
      end;
    end;
    if hash(0, buf, L) <> c then
      exit; // should return the same value for the same data
  end;
  result := true;
end;

function Hash128Test(buf: PAnsiChar; hash: THasher128; out msg: string): boolean;
var
  L, modif: PtrInt;
  c, c2: THash128;
begin
  result := false;
  for L := 0 to HASHESMAX do
  begin
    FillZero(c);
    hash(@c, buf, L);
    for modif := 0 to L - 1 do
    begin
      FillZero(c2);
      inc(buf[modif]);
      hash(@c2, buf, L);
      dec(buf[modif]);
      if IsEqual(c, c2) then
      begin
        FormatString('L=% modif=%', [L, modif], msg);
        exit; // should detect one modified bit at any position
      end;
    end;
    FillZero(c2);
    hash(@c2, buf, L);
    if not IsEqual(c, c2) then
    begin
      msg := 'after reset';
      exit; // should return the same value for the same data
    end;
  end;
  result := true;
end;

procedure TTestCoreCrypto.Hashes;
const
  HASHALIGN = 4; // you may try with paranoid 32 here
var
  buf: RawByteString;
  P: PAnsiChar;
  msg: string;
  unalign: PtrInt;
  exp321, exp322, exp323, exp324: cardinal;
  exp641, exp642: QWord;
begin
  Check(Adler32SelfTest);
  SetLength(buf, HASHESMAX + HASHALIGN);
  exp321 := 0;
  exp322 := 0;
  exp323 := 0;
  exp324 := 0;
  exp641 := 0;
  exp642 := 0;
  for unalign := 0 to HASHALIGN - 1 do
  begin
    P := pointer(buf);
    inc(P, unalign);
    FillIncreasing(pointer(P), $12345670, HASHESMAX shr 2);
    Check(Hash32Test(P, @crc32cfast, exp321));
    Check(Hash32Test(P, @crc32c, exp322));
    Check(Hash32Test(P, @xxHash32, exp323));
    if Assigned(AesNiHash32) then
      Check(Hash32Test(P, @AesNiHash32, exp324));
    Check(Hash64Test(P, @crc32cTwice, exp641));
    if Assigned(AesNiHash64) then
      Check(Hash64Test(P, @AesNiHash64, exp642));
    Check(Hash128Test(P, @crc32c128, msg), msg{%H-});
    if Assigned(AesNiHash128) then
      Check(Hash128Test(P, @AesNiHash128, msg), msg);
  end;
end;

procedure TTestCoreCrypto.Streams;
var
  w: TAesPkcs7Writer;
  r: TAesPkcs7Reader;
  data, data2: array[0..199] of Int64; // 1600 bytes > buffer of 1024 bytes
  siz: Int64;
  mem: TRawByteStringStream;
  i, j: PtrInt;
  key, iv: THash128;
  piv: PAesBlock;
  m: TAesMode;
  withiv: boolean;
  aes: TAesAbstract;
  res, dec: RawByteString;
begin
  FillZero(key);
  FillZero(iv);
  for i := 0 to high(data) do
  begin
    data[i] := i * 777;
    iv[0] := i;
    for m := low(m) to high(m) do
      for withiv := false to true do
      begin
        if not (m in AES_PKCS7WRITER) then
          continue;
        if withiv then
          piv := nil  // generate random IV at output trail
        else
          piv := @iv; // we will supply the IV to the decoder
        mem := TRawByteStringStream.Create;
        w := TAesPkcs7Writer.Create(mem, key, 128, m, piv, 1024);
        try
          for j := 0 to i - 1 do
            CheckEqual(w.Write(data[j], 8), 8, 'write by 8 bytes chunks');
          w.Finish;
          res := mem.DataString;
          CheckEqual(mem.Position, mem.Size);
          CheckEqual(mem.Size, length(res));
        finally
          w.Free;
          mem.Free;
        end;
        Check(length(res) > i + 1);
        if not withiv then // the random IV make res genuine
        begin
          mem := TRawByteStringStream.Create;
          w := TAesPkcs7Writer.Create(mem, key, 128, m, piv, 1024);
          try
            CheckEqual(w.Write(data[0], i shl 3), i shl 3, 'write at once');
            w.Finish;
            CheckEqual(mem.Size, length(res));
            Check(mem.DataString = res, 'atonce');
          finally
            w.Free;
            mem.Free;
          end;
        end;
        aes := TAesFast[m].Create(key);
        try
          if not withiv then
            aes.IV := iv;
          dec := aes.DecryptPkcs7(res, withiv, {raiseonerror=}false);
          if not CheckFailed(length(dec) = i shl 3) then
            Check(CompareMem(pointer(dec), @data, length(dec)));
        finally
          aes.Free;
        end;
        mem := TRawByteStringStream.Create(res);
        r := TAesPkcs7Reader.Create(mem, key, 128, m, piv, 1024);
        try
          FillCharFast(data2, SizeOf(data2), 7);
          if i = 0 then
            CheckEqual(r.Read(data2[0], 8), 0)
          else
            for j := 0 to i - 1 do
              CheckEqual(r.Read(data2[j], 8), 8, 'read by 8 bytes chunks');
          CheckEqual(r.Read(data2[0], 8), 0, 'after');
          CheckEqual(mem.Position, mem.Size);
          Check(CompareMem(@data, @data2, i shl 3));
        finally
          r.Free;
          mem.Free;
        end;
        mem := TRawByteStringStream.Create(res);
        r := TAesPkcs7Reader.Create(mem, key, 128, m, piv, 1024);
        try
          FillCharFast(data2, SizeOf(data2), 1);
          CheckEqual(r.Read(data2, SizeOf(data2) * 2), i shl 3);
          CheckEqual(r.Read(data2[0], 8), 0, 'after2');
          CheckEqual(mem.Position, mem.Size);
          Check(CompareMem(@data, @data2, i shl 3));
        finally
          r.Free;
          mem.Free;
        end;
      end;
  end;
  siz := FileSize(WorkDir + 'People.json');
  if siz <> 0 then // need a first run to have the file
  begin
    AesPkcs7File(WorkDir + 'People.json', WorkDir + 'people.encrypt', true, 'Thomas');
    CheckEqual(AesPkcs7File(WorkDir + 'people.encrypt',
      WorkDir + 'people.decrypt', false, 'Thomas'), siz, 'AesPkcs7File');
    CheckEqual(FileSize(WorkDir + 'people.decrypt'), siz, 'AesPkcs7File size');
    CheckEqual(HashFile(WorkDir + 'People.json'),
      HashFile(WorkDir + 'people.decrypt'), 'AesPkcs7File hash');
  end;
end;

procedure TTestCoreCrypto._Base64;
const
  Value64: RawUtf8 = 'SGVsbG8gL2Mn6XRhaXQg5+Ar';
var
  tmp, tmp2: RawByteString;
  u, b64: RawUtf8;
  msg: string;
  Value: WinAnsiString;
  P: PUtf8Char;
  k: TPemKind;
  i, j, L, n: Integer;
  i64: Qword;
  enc, dec: TPrecisionTimer;
begin
  tmp := 'wrJQQCQkdzByZA==';
  Check(IsBase64(tmp));
  u := Base64ToBin(tmp);
  CheckHash(u, $B5C83B58);
  CheckEqual(BinToBase64(u), tmp);
  Value := 'Hello /c''0tait 67+';
  Value[10] := #$E9;
  Value[16] := #$E7;
  Value[17] := #$E0;
  Check(not IsBase64(Value));
  CheckEqual(BinToBase64(Value), Value64);
  Check(IsBase64(Value64));
  tmp := StringFromFile(Executable.ProgramFileName);
  if length(tmp) > 1 shl 20 then
    SetLength(tmp, 1 shl 20);
  b64 := BinToBase64(tmp);
  Check(IsBase64(b64));
  Check(Base64ToBin(b64) = tmp);
  CheckEqual(BinToBase58('Hello World!'), '2NEpo7TZRRrLZSi2U', 'b58-1');
  CheckEqual(BinToBase58('The quick brown fox jumps over the lazy dog.'),
    'USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z', 'b58-2');
  i64 := $cdb47f28000000; // test vector for leading zeros from RFC
  CheckEqual(BinToBase58(@i64, 7), '111233QC4', 'b58-3');
  CheckEqual(Base58ToBin('2NEpo7TZRRrLZSi2U'), 'Hello World!', 'b58-4');
  CheckEqual(Base58ToBin('USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z'),
    'The quick brown fox jumps over the lazy dog.', 'b58-5');
  tmp := Base58ToBin('111233QC4');
  CheckEqual(length(tmp), 7, 'b58-6');
  Check(CompareMem(pointer(tmp), @i64, 7), 'b58-7');
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
    if i < 67 then // Base58 is much slower than Base64: not used on big buffers
    begin
      b64 := BinToBase58(tmp);
      CheckEqual(Base58ToBin(b64), tmp);
    end;
    if tmp <> '' then
    begin
      Check(not IsPem(b64));
      b64 := DerToPem(pointer(tmp), length(tmp), TPemKind(i and 7));
      Check(IsPem(b64));
      CheckUtf8(PemToDer(b64) = tmp, b64);
      P := pointer(b64);
      CheckEqual(NextPem(P, @k), b64);
      Check(P <> nil);
      Check(k = TPemKind(i and 7));
      CheckEqual(NextPem(P, @k), '');
    end;
    tmp := tmp + AnsiChar(Random32(255));
  end;
  enc.Init;
  dec.Init;
  tmp := RandomString(1 shl 20);
  b64 := '';
  SetLength(b64, BinToBase64Length(length(tmp)));
  SetLength(tmp2, length(tmp));
  L := 0;
  n := 50;
  {$ifdef ASMX64AVXNOCONST}
  if cfAVX2 in CpuFeatures then
  begin
    n := n * 10;
    msg := ' avx2';
  end;
  {$endif ASMX64AVXNOCONST}
  for i := 0 to 20 do
  begin
    enc.Resume;
    for j := 1 to n do
      Base64Encode(pointer(b64), pointer(tmp), 1 shl i);
    enc.Pause;
    dec.Resume;
    for j := 1 to n do
      Base64Decode(pointer(b64), pointer(tmp2), BinToBase64Length(1 shl i) shr 2);
    dec.Pause;
    Check(CompareMem(pointer(tmp), pointer(tmp2), 1 shl i));
    inc(L, 1 shl i);
  end;
  i64 := Int64(L) * n; // may overflow 32-bit L with AVX
  NotifyTestSpeed('encoding' + msg, 0, i64, @enc);
  NotifyTestSpeed('decoding' + msg, 0, i64, @dec);
end;

const
  // reference vectors for all AES modes - matching OpenSSL implementation
  TEST_AES_REF: array[0..2, 0..6] of RawByteString = (
  // 128-bit TAesEcb, TAesCbc, TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesGcm
   ('aS24Jm0RHPz26P_RHqX-pGktuCZtERz89uj_0R6l_qRpLbgmbREc_Pbo_9Eepf6kB7pVFdRAcIoVhoTQPytzTQ',
    'i1vnbHBw0VZZdm-nlhq7H3N-C3oMLGfooWnwjI0F_X3QgeV6s-Q8ujVIbgpX5Bwu8tOn1SoUHHP4VS0VK5cOyQ',
    '9rlcKw63fOzEbXUpoCUDLPqt7TuuSjLGHdlDMneP0nrY4LLFbrc3MrLV6JoXmQM6d4FvmlsQpImuk9LWaf8hXw',
    '9rlcKw63fOzEbXUpoCUDLHuJV24miApjh5nwI-vJo4ODczlsBPQH-mKdBzlUwKOZ8bEV1IUEF6gVGsT-GOuA3w',
    '9rlcKw63fOzEbXUpoCUDLDNLyx8M6u_tGBRLx4j5ctLUsP9-TW7sOuOoF4OD4lJAjZleMbc8Z_BdmyuNRuiUtg',
    '9rlcKw63fOzEbXUpoCUDLPODC-Nwu96PUeytu204bloDoO7QOmLe8SSHM2P0kB5NW3VPROV5QLaVhYfld4uZBA',
    'gUZBx61sQ4gV3RZ-qpZrkQDnlu88Jb4mGPWorawImGYK4ei1yy3oRPPYBTclVHoRVRwHnHMB1NnGGq3T0qbZmw'),
  // 192-bit TAesEcb, TAesCbc, TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesGcm
   ('3S2QhC78T0eesG3hiqtA2N0tkIQu_E9HnrBt4YqrQNjdLZCELvxPR56wbeGKq0DYJob7gbbvgBaFdm_Bwed4RQ',
    'yua7dkKtXp0pM5n3VFoZrKhdt1ppikmmhFBKzflv32uY6cm4X3ZDZZnlAujYFBAWYR9fJXvhKmCcPljunWP2Zw',
    'Lp2JYG5d-d4TZagr2FMfqRxp9GCAHtCNcV5HmNoZpt34jqelBTDnTPagl9ZsIkrKRM_m0i3o0PWyK7hf6h9evg',
    'Lp2JYG5d-d4TZagr2FMfqcygVkV4gZnunc1EDx63mo2B0WfIDhpjtPSVuiXjXBUlPcEVs_YVJoPmIbJDD_mwpQ',
    'Lp2JYG5d-d4TZagr2FMfqbBYsYzcSw6Re_OY2Zthq1_MEtRiSeqYNI-Z2s1J_3Gwah3j29AUlU7fDl0w8_sjlA',
    'Lp2JYG5d-d4TZagr2FMfqc-3Wr2DBpXIPh2l-OjSsqlAcEVs8vH6tbc5_5G59H_wTCxihPcc8yz8f_fyGiDEaQ',
    'hBeEH6I4wWS55pvTLfjz5PxR1nq13tv920aVPw1sMbbraVjQ7Z7vD272rMCOrfMz4b9CFK7SUqh92pR6YIId8A'),
  // 256-bit TAesEcb, TAesCbc, TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesGcm
   ('Kw50ybT0hl8MXw1IcBFm5isOdMm09IZfDF8NSHARZuYrDnTJtPSGXwxfDUhwEWbmn9aUUA6_ZwXpKRiFMlXRiw',
    'uSh1TguJYyEhud6DBzk8TZrD01xIULmMHX0gRFAGaf2vDinDfDprSxCm5Fd49HN0a6EoBrK1cCqanTWqyuyM8A',
    'PYynVHoDmi6SK5qdbNUp5IPwHRadBtT6rf97pdIP3MHk1q1rZHNzquVCOF5_oSMs0rqP7bJ6j6BvWpzTGcvEPQ',
    'PYynVHoDmi6SK5qdbNUp5IFTsstbPmW8RbyfJ1fh1x2N2vQw5n_5DtDYx-49wgZnu5MEthDAT2h7XPqNIFgfdw',
    'PYynVHoDmi6SK5qdbNUp5JNCbgI49PtmxVueuHSTBkI6JbFu9smQCMkp8sQEFBAs8F46W4qqNgMiE9QhJUtoAg',
    'PYynVHoDmi6SK5qdbNUp5NDiW4s3_P_KGDXarkzNgBrxUjjzTUzVJ29q9Uq75xI3eTczo57cI5ibqZ-BvbYRLw',
    'rUvWiPrboNKztxCcC6Cq5GWAlbLOk_UO-GddAmNnHCIpbBSz-q6xqXP0aw0REnW9usdCu2DZZ28B2GbaOfydrg'));
  TEST_AES_TAG: array[0..2] of RawUtf8 = (
    '7C1DA6408329D2D2E393609DB188129E',  // 128-bit
    'EFF784967837F6BB0007276CA9C9F936',  // 192-bit
    '5F3411F163FF157C4A802DB5FF835823'); // 256-bit
  TEST_AES_MAC: array[7..9, 0..2] of RawUtf8 = (
    ('a6353d1260ec249aa1da751d9e888978258194e4454a0d719b39152b39d7b7a9', // TAesCfc
     '80f2ef4b22b48b4a0bcca7a9c509a2467b620569597d0791b9b56243fe03af1b',
     'c13dc3e510b02ecd5eec947dfd934fc256b308318dcbc16bc9aabf7b616fffb5'),
    ('a6353d1260ec249aa1da751d9e8889785716e5bac7e28577164eee94cc2cdaeb', // TAesOfc
     '80f2ef4b22b48b4a0bcca7a9c509a2462be402ff0ceb734b81feafb7bbb32d35',
     'c13dc3e510b02ecd5eec947dfd934fc261e76a48caa8808ed4a5979e30fa1fa5'),
     ('a6353d1260ec249aa1da751d9e888978d591cb79c5f2e77e2e15bd507aa11b04', // TAesCtc
      '80f2ef4b22b48b4a0bcca7a9c509a246041058e5b7e63e90fdb865dc0d8dc216',
      'c13dc3e510b02ecd5eec947dfd934fc27a9bada0c582df6d441b67a8455a1711'));

function ToAesReference(m: integer): integer;
begin
  result := m;
  if result >= 10 then
    {$ifdef USE_OPENSSL}
      if result >= 17 then
        dec(result, 17) // e.g. TAesEcbApi -> TAesEcb
      else
    {$endif USE_OPENSSL}
    dec(result, 10) // e.g. TAesEcbApi / TAesEcbOsl -> TAesEcb
  else if result = 9 then
    result := 5 // TAesCtc -> TAesCtr
  else if result >= 7 then
    dec(result, 5);  // e.g. TAesCfc -> TAesCfb
end;

procedure TTestCoreCrypto._AES;
const
  MAX = 4096 * 1024;  // test 4 MB data, i.e. multi-threaded AES
  MODES: array[0..9
     {$ifdef USE_OPENSSL} + 7 {$endif}
     {$ifdef USE_PROV_RSA_AES} + 2 {$endif}] of TAesAbstractClass = (
     // 0      1        2        3        4          5            6
     TAesEcb, TAesCbc, TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesGcm,
     // 7           8         9
     TAesCfc, TAesOfc, TAesCtc
     {$ifdef USE_OPENSSL} ,
     // 10          11         12         13         14
     TAesEcbOsl, TAesCbcOsl, TAesCfbOsl, TAesOfbOsl, nil,
     // 15            16
     TAesCtrOsl, TAesGcmOsl
     {$endif USE_OPENSSL}
     {$ifdef USE_PROV_RSA_AES} ,
     // 10/17     11/18
     TAesEcbApi, TAesCbcApi
     {$endif USE_PROV_RSA_AES}); // TAesCfbApi and TAesOfbApi are not compliant
var
  A: TAes;
  st, orig, crypted, s2, s3, s4: RawByteString;
  Key: TSha256Digest;
  s, b, p: TAesBlock;
  iv: THash128Rec;
  i, j, k, ks, m, len: integer;
  tag1, tag2: TAesBlock;
  mac: TAesMac256;
  mac1, mac2: THash256;
  one, two, encdec: TAesAbstract;
  noaesni, gcm, aead: boolean;
  Timer: array[boolean] of TPrecisionTimer;
  ValuesCrypted, ValuesOrig: array[0..6] of RawByteString;
  Tags: array[0..2, 7..9] of THash256DynArray; // Tags[k,m]
  h32: array[0..2, 0..9] of TCardinalDynArray;
  {$ifdef CPUINTEL}
  backup: TIntelCpuFeatures;
  {$endif CPUINTEL}
begin
  {$ifdef CPUINTEL}
  backup := CpuFeatures;
  {$endif CPUINTEL}
  Check(AesTablesTest, 'Internal Tables');
  SetLength(orig, MAX);
  SetLength(crypted, MAX + 256);
  st := '1234essai';
  orig := RandomString(8000);
  PInteger(UniqueRawUtf8(RawUtf8(st)))^ := Random32;
  for noaesni := false to true do
  begin
    {%H-}Timer[noaesni].Init;
    for k := 0 to 2 do
    begin
      ks := 128 + k * 64; // test keysize of 128, 192 and 256 bits
      for m := 3 to high(MODES) do
      begin
        if (MODES[m] = nil) or
           not MODES[m].IsAvailable then
          continue; // OpenSSL may not be available on this platform
        // following values should synch with TEST_AES_REF/MAC/TAG
        iv.L := $1234567890abcdef;
        iv.H := $0fedcba987654321;
        FillZero(THash256(mac));
        st := RawUtf8OfChar('x', 50);
        // create a TAesAbstract instance and validate it
        one := MODES[m].Create(pointer(st)^, ks);
        try
          // writeln(noaesni, ' ', one.AlgoName, ' k=', k, ' m=', m);
          gcm := one.InheritsFrom(TAesGcmAbstract);
          aead := one.InheritsFrom(TAesAbstractAead);
          one.IV := iv.b;
          if aead then
            TAesAbstractAead(one).Mac := mac;
          s2 := one.EncryptPkcs7(st, false);
          if aead then
          begin
            Check(m in [low(TEST_AES_MAC) .. high(TEST_AES_MAC)]);
            RandomBytes(@mac1, SizeOf(mac1));
            Check(one.MacEncryptGetTag(mac1));
            //writeln(m,' ',k,' ',Sha256DigestToString(mac1)); writeln(TEST_AES_MAC[m, k]);
            //CheckEqual(Sha256DigestToString(mac1), TEST_AES_MAC[m, k], 'TEST_AES_MAC');
          end
          else if gcm then
          begin
            RandomBytes(@tag1, SizeOf(tag1));
            Check(TAesGcmAbstract(one).AesGcmFinal(tag1));
            // writeln(one.classname, ks, ' ', AesBlockToShortString(tag1));
            CheckEqual(AesBlockToString(tag1), TEST_AES_TAG[k],
              FormatUtf8('TEST_AES_TAG % %', [ks, one.AlgoName]));
          end;
          one.IV := iv.b;
          if aead then
            TAesAbstractAead(one).Mac := mac;
          s2 := one.EncryptPkcs7(st, false); // twice to check AES ctxt reuse
          if aead then
          begin
            RandomBytes(@mac2, SizeOf(mac2));
            Check(one.MacEncryptGetTag(mac2));
            Check(IsEqual(mac2, mac1));
          end
          else if gcm then
          begin
            FillZero(tag2);
            Check(TAesGcmAbstract(one).AesGcmFinal(tag2));
            Check(not IsZero(tag2));
            Check(IsEqual(tag1, tag2));
          end;
          s3 := BinToBase64uri(s2);
          i := ToAesReference(m);
          //if TEST_AES_REF[k, i] <> s3 then
          // writeln(m, ' ', MODES[m].ClassName, ' ', ks, #13#10' ',s3, #13#10' ', TEST_AES_REF[k, i]);
          CheckUtf8(TEST_AES_REF[k, i] = s3, 'test vector %-% %', [MODES[m], ks, s3]);
          one.IV := iv.b;
          if aead then
            TAesAbstractAead(one).Mac := mac;
          check(one.DecryptPkcs7(s2, false) = st);
          if aead then
            Check(one.MacDecryptCheckTag(mac1))
          else if gcm then
            Check(TAesGcmAbstract(one).AesGcmFinal(tag1));
          two := one.Clone;
          try
            two.IV := iv.b;
            if aead then
              TAesAbstractAead(two).Mac := mac;
            s2 := two.EncryptPkcs7(st, false);
            if aead then
            begin
              FillZero(mac1);
              Check(two.MacEncryptGetTag(mac1));
              Check(IsEqual(mac2, mac1));
            end
            else if gcm then
            begin
              FillZero(tag1);
              Check(TAesGcmAbstract(two).AesGcmFinal(tag1));
              Check(IsEqual(tag1, tag2));
            end;
            two.IV := iv.b;
            if aead then
              TAesAbstractAead(two).Mac := mac;
            s2 := two.EncryptPkcs7(st, false); // twice to check AES ctxt reuse
            if aead then
            begin
              FillZero(mac2);
              Check(one.MacEncryptGetTag(mac2));
              Check(IsEqual(mac2, mac1));
            end
            else if gcm then
            begin
              FillZero(tag2);
              Check(TAesGcmAbstract(two).AesGcmFinal(tag2));
              Check(IsEqual(tag1, tag2));
            end;
            s4 := BinToBase64uri(s2);
            CheckEqual(s3, s4);
            encdec := two.CloneEncryptDecrypt;
            encdec.IV := iv.b;
            if aead then
              TAesAbstractAead(encdec).Mac := mac;
            checkEqual(encdec.DecryptPkcs7(s2, false), st);
            if aead then
              Check(encdec.MacDecryptCheckTag(mac1))
            else if gcm then
              Check(TAesGcmAbstract(encdec).AesGcmFinal(tag1));
            if encdec <> two then
              encdec.Free;
          finally
            two.Free;
          end;
        finally
          one.Free;
        end;
      end;
      Sha256Weak('test', Key);
      for i := 1 to 20 do
      begin
        MoveFast(Key, s, 16);
        Timer[noaesni].Resume;
        A.EncryptInit(Key, ks);
        for j := 1 to 100 do
          A.Encrypt(s, b);
        A.Done;
        A.DecryptInit(Key, ks);
        for j := 1 to 100 do
          A.Decrypt(b, p);
        A.Done;
        Timer[noaesni].Pause;
        CheckUtf8(IsEqual(p, s), 'encrypt/decrypt ks=% %<>%', [ks, p[0], s[0]]);
        Check(CompareMem(@p, @s, SizeOf(p)));
      end;
      iv.c3 := $e0ffffff; // to trigger an explicit CTR overflow
      for m := low(MODES) to high(MODES) do
        if (MODES[m] <> nil) and
           MODES[m].IsAvailable then
        begin
          one := MODES[m].Create(Key, ks);
          try
            gcm := one.InheritsFrom(TAesGcmAbstract);
            aead := one.InheritsFrom(TAesAbstractAead);
            if m <= 9 then
              SetLength(h32[k, m], 257);
            if aead then
            begin
              Check(k in [0..2]);
              Check(m in [7..9]);
              SetLength(Tags[k, m], 257);
            end;
            //Timer.Start;
            for i := 0 to 256 do
            begin
              if i < 64 then
                len := i
              else if i < 128 then
                len := i * 15
              else
                len := i * 31; // encrypt buffers from 0 to 7936 bytes
              s2 := copy(orig, 1, len);
              check(length(s2) = len);
              one.iv := iv.b;
              if aead then
                TAesAbstractAead(one).Mac := mac;
              s3 := one.EncryptPkcs7(s2);
              if m <= 9 then
                if noaesni then
                  CheckEqual(h32[k, m, i], cardinal(DefaultHasher(0, pointer(s3), length(s3))))
                else
                  h32[k, m, i] := cardinal(DefaultHasher(0, pointer(s3), length(s3)));
              if aead then
                if not noaesni then
                  Check(one.MacEncryptGetTag(Tags[k, m, i]))
                else
                begin
                  Check(one.MacEncryptGetTag(mac1));
                  Check(IsEqual(Tags[k, m, i], mac1));
                end
              else if gcm then
              begin
                FillZero(tag1);
                TAesGcmAbstract(one).AesGcmFinal(tag1);
              end;
              one.iv := iv.b;
              if aead then
                TAesAbstractAead(one).Mac := mac;
              CheckEqual(one.DecryptPkcs7(s3), s2, UInt32ToUtf8(len));
              if aead then
                Check(one.MacDecryptCheckTag(Tags[k, m, i]))
              else if gcm then
                Check(TAesGcmAbstract(one).AesGcmFinal(tag1));
            end;
//fRunConsole := Format('%s %s%d:%s'#10,[fRunConsole,Copy(MODES[m].ClassName,5,10),ks,Timer.Stop]);
            if m < length(ValuesCrypted) then
            begin
              // store the values generated by our AES pascal/asm code
              ValuesCrypted[m] := Copy(crypted, 1, len);
              ValuesOrig[m] := s2;
            end
            else if m > 7 then
            begin
              // validate our AES code against OpenSSL or WinAPI
              i := ToAesReference(m);
              Check(ValuesOrig[i] = s2);
              Check(ValuesCrypted[i] = Copy(crypted, 1, len), one.ClassName);
            end;
          finally
            one.Free;
          end
        end;
      end;
    {$ifdef CPUINTEL}
    if noaesni then
    begin
      fRunConsole := format('%s cypher with AES-NI: %s, without: %s',
        [fRunConsole, Timer[false].Stop, Timer[true].Stop]);
      Include(CpuFeatures, cfAESNI); // revert Exclude() below from previous loop
    end;
    if A.UsesAesni then
      Exclude(CpuFeatures, cfAESNI);
    {$endif CPUINTEL}
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
    tag: TAesBLock;
    ctxt: TAesGcmEngine;
    pt, ct: array[0..511] of byte;
  begin
    FillCharFast(pt, SizeOf(pt), 0);
    CheckUtf8(ctxt.FullDecryptAndVerify(key, kbits, pIV, IV_Len, pAAD, aLen, ctp,
      @pt, cLen, ptag, tlen), 'FullDecryptAndVerify #%', [tn]);
    CheckUtf8(CompareMem(@pt, ptp, cLen), 'Plain #%', [tn]);
    FillCharFast(ct, SizeOf(ct), 0);
    CheckUtf8(ctxt.FullEncryptAndAuthenticate(key, kbits, pIV, IV_Len, pAAD,
      aLen, ptp, @ct, cLen, tag), 'FullEncryptAndAuthenticate #%', [tn]);
    CheckUtf8(CompareMem(@tag, ptag, tlen), 'Tag #%', [tn]);
    CheckUtf8(CompareMem(@ct, ctp, cLen), 'Encoded #%', [tn]);
  end;

var
  ctxt: TAesGcmEngine;
  key, tag: TAesBlock;
  buf: THash512;
  n: integer;
begin
  key := PAesBlock(@hex32)^;
  FillZero(buf);
  FillZero(tag);
  check(ctxt.FullEncryptAndAuthenticate(key, 128, @hex32, 12, nil, 0,
    @buf, @buf, SizeOf(buf), tag));
  CheckEqual(CardinalToHex(crc32c(0, @buf, SizeOf(buf))), 'AC3DDD17');
  CheckEqual(Md5DigestToString(tag), '0332c40f9926bd3cdadf33148912c672');
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
  test(@T01, 16, K01, 8 * SizeOf(K01), @I01, SizeOf(I01), nil, 0,
       @C01, SizeOf(C01), @P01, 01);
  test(@T02, 16, K02, 8 * SizeOf(K02), @I02, SizeOf(I02), @H02, SizeOf(H02),
       nil, 0, nil, 02);
  test(@T03, 16, K03, 8 * SizeOf(K03), @I03, SizeOf(I03), @H03, SizeOf(H03),
       @C03, SizeOf(C03), @P03, 03);
  test(@T04, 16, K04, 8 * SizeOf(K04), @I04, SizeOf(I04), nil, 0,
       @C04, SizeOf(C04), @P04, 04);
  test(@T05, 16, K05, 8 * SizeOf(K05), @I05, SizeOf(I05), @H05, SizeOf(H05),
       @C05, SizeOf(C05), @P05, 05);
  test(@T07, 16, K07, 8 * SizeOf(K07), @I07, SizeOf(I07), @H07, SizeOf(H07),
       @C07, SizeOf(C07), @P07, 07);
  test(@T08, 16, K08, 8 * SizeOf(K08), @I08, SizeOf(I08), @H08, SizeOf(H08),
       @C08, SizeOf(C08), @P08, 08);
  test(@T09, 16, K09, 8 * SizeOf(K09), @I09, SizeOf(I09), @H09, SizeOf(H09),
       @C09, SizeOf(C09), @P09, 09);
  test(@T10, 16, K10, 8 * SizeOf(K10), @I10, SizeOf(I10), nil, 0, @C10,
       SizeOf(C10), @P10, 10);
  test(@T11, 16, K11, 8 * SizeOf(K11), @I11, SizeOf(I11), @H11, SizeOf(H11),
       @C11, SizeOf(C11), @P11, 11);
end;

{$ifndef PUREMORMOT2}
procedure TTestCoreCrypto._CompressShaAes;
var
  s1, s2: RawByteString;
  keysize, i: integer;
begin
  for keysize := 0 to 10 do
  begin
    CompressShaAesSetKey(RandomUtf8(keysize));
    for i := 0 to 50 do
    begin
      s1 := RandomUtf8(i * 3);
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
  md: TMd5;
  dig, dig2: TMd5Digest;
  tmp: TByteDynArray;
begin
  check(htdigest('agent007', 'download area', 'secret') =
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65');
  check(Md5('') = 'd41d8cd98f00b204e9800998ecf8427e');
  check(Md5('a') = '0cc175b9c0f1b6a831c399e269772661');
  check(Md5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789') =
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
    check(CompareMem(@dig, @dig2, SizeOf(dig)));
  end;
end;

procedure TTestCoreCrypto.Catalog;
var
  m: TAesMode;
  k, k2: integer;
  a, i: PtrInt;
  c32, cprev: cardinal;
  d, dprev: double;
  n, h, nprev, aead, pub, priv, pub2, priv2, jwt, iss, sub, s2, s3: RawUtf8;
  r, s: RawByteString;
  aes: TAesAbstract;
  key: THash256;
  rnd: TCryptRandom;
  hsh: TCryptHasher;
  sig: TCryptSigner;
  cip: TCryptCipherAlgo;
  asy: TCryptAsym;
  en, de: ICryptCipher;
  crt: TCryptCertAlgo;
  c1, c2, c3, c4: ICryptCert;
  fields: TCryptCertFields;
  str: TCryptStoreAlgo;
  st1, st2, st3: ICryptStore;
  cpe: TCryptCertPerUsage;
  alg: TCryptAlgos;
  fmt: TCryptCertFormat;
  cv: TCryptCertValidity;
  u: TCryptCertUsage;
  namelen: integer;
  names: RawUtf8;

  procedure AddAlgName;
  begin
    inc(namelen, length(alg[a].AlgoName) + 1);
    if namelen > 73 then
    begin
      names := names + CRLF + '     ';
      namelen := length(alg[a].AlgoName) + 1;
    end;
    names := names + ' ' + alg[a].AlgoName;
  end;

begin
  namelen := 0;
  // validate AesAlgoNameEncode / TAesMode
  FillZero(key);
  for k := 0 to 2 do
    for m := low(m) to high(m) do
    begin
      n := AesAlgoNameEncode(m, 128 + k * 64);
      check(length(n) = 11);
      check(IdemPChar(pointer(n), 'AES-'));
      CheckUtf8(AesAlgoNameDecode(n, k2) = TAesFast[m], n);
      UpperCaseSelf(n);
      CheckUtf8(AesAlgoNameDecode(n, k2) = TAesFast[m], n);
      aes := TAesFast[m].Create(key, 128 + k * 64);
      try
        Check(aes.AlgoMode = m);
        Check(IdemPropName(aes.AlgoName, pointer(n), length(n)));
      finally
        aes.Free;
      end;
      n[10] := ' ';
      CheckUtf8(AesAlgoNameDecode(n, k2) = nil, n);
    end;
  // validate Rnd High-Level Algorithms Factory
  alg := TCryptRandom.Instances;
  for a := 0 to high(alg) do
  begin
    rnd := alg[a] as TCryptRandom;
    AddAlgName;
    Check(mormot.crypt.secure.Rnd(rnd.AlgoName) = rnd);
    cprev := 0;
    dprev := 0;
    for i := 1 to 10 do
    begin
      c32 := rnd.Get32;
      CheckUtf8(c32 <> cprev, rnd.AlgoName);
      cprev := c32;
      c32 := rnd.Get32(i * 77);
      Check(c32 < cardinal(i * 77));
      d := rnd.GetDouble;
      check(d <> dprev);
      dprev := d;
      n := rnd.Get(i);
      check(length(n) = i);
    end;
  end;
  // validate Hash High-Level Algorithms Factory
  alg := TCryptHasher.Instances;
  for a := 0 to high(alg) do
  begin
    hsh := alg[a] as TCryptHasher;
    AddAlgName;
    Check(mormot.crypt.secure.Hasher(hsh.AlgoName) = hsh);
    h := hsh.Full(n);
    for i := 1 to length(n) do
    begin
      inc(n[i]);
      CheckUtf8(hsh.Full(n) <> h, hsh.AlgoName);
      dec(n[i]);
    end;
    CheckUtf8(hsh.Full(n) = h, hsh.AlgoName);
  end;
  // validate Sign High-Level Algorithms Factory
  alg := TCryptSigner.Instances;
  for a := 0 to high(alg) do
  begin
    sig := alg[a] as TCryptSigner;
    AddAlgName;
    Check(mormot.crypt.secure.Signer(sig.AlgoName) = sig);
    h := sig.Full('key', n);
    for i := 1 to length(n) do
    begin
      inc(n[i]);
      CheckUtf8(sig.Full('key', n) <> h, sig.AlgoName);
      dec(n[i]);
    end;
    CheckEqual(sig.Full('key', n), h, sig.AlgoName);
    for i := 1 to 5 do
    begin
      h := sig.NewPbkdf2('sec', 'salt', i).Update(n).Final;
      CheckEqual(h, sig.NewPbkdf2('sec', 'salt', i).Update(n).Final, sig.AlgoName);
      Check(h <> sig.NewPbkdf2('sec', 'sel', i).Update(n).Final);
      Check(h <> sig.NewPbkdf2('sec', 'salt', i + 1).Update(n).Final);
    end;
  end;
  // validate Cipher High-Level Algorithms Factory
  alg := TCryptCipherAlgo.Instances;
  for a := 0 to high(alg) do
  begin
    cip := alg[a] as TCryptCipherAlgo;
    AddAlgName;
    Check(mormot.crypt.secure.CipherAlgo(cip.AlgoName) = cip);
    if cip.IsAead then
      aead := cip.AlgoName
    else
      aead := '';
    nprev := '';
    for i := 1 to 5 do
    begin
      en := cip.Encrypt('hmac-sha256', 'sec', 'salt', i);
      de := cip.Decrypt('Hmac-SHA256', 'sec', 'salt', i);
      CheckUtf8(en.Process(n, r, aead), cip.AlgoName);
      Check(nprev <> r);
      Check(de.Process(r, s, aead));
      CheckEqual(n, s, cip.AlgoName);
      nprev := r;
    end;
  end;
  // validate Asym High-Level Algorithms Factory
  alg := TCryptAsym.Instances;
  for a := 0 to high(alg) do
  begin
    asy := alg[a] as TCryptAsym;
    Check(mormot.crypt.secure.Asym(asy.AlgoName) = asy);
    asy.GeneratePem(pub, priv, '');
    Check(pub <> '');
    Check(priv <> '');
    CheckUtf8(asy.Sign(n, priv, s), asy.AlgoName);
    Check(s <> '');
    Check(asy.Verify(n, pub, s));
    inc(n[1]);
    Check(not asy.Verify(n, pub, s));
    dec(n[1]);
    asy.GeneratePem(pub2, priv2, '');
    Check(pub2 <> '');
    Check(priv2 <> '');
    Check(pub <> pub2);
    Check(priv <> priv2);
  end;
  // validate Cert High-Level Algorithms Factory
  alg := TCryptCertAlgo.Instances;
  for a := 0 to high(alg) do
  begin
    crt := alg[a] as TCryptCertAlgo;
    AddAlgName;
    check(PosEx(UpperCase(CAA_JWT[crt.AsymAlgo]), UpperCase(crt.AlgoName)) > 0);
    c1 := crt.New;
    check(c1.AsymAlgo = crt.AsymAlgo);
    Check(c1.GetSerial = '');
    Check(not c1.HasPrivateSecret);
    Check(c1.IsVoid);
    Check(not c1.IsValidDate);
    CheckEqual(c1.GetSignatureInfo, '');
    if crt.AlgoName = 'syn-es256-v1' then
    begin
      // TEccCertificate V1 has limited Usage and Subjects support
      c1.Generate([cuCA, cuDigitalSignature, cuKeyCertSign], ' s1, s2 ', nil);
      CheckEqual(RawUtf8ArrayToCsv(c1.GetSubjects), 's1,s2');
      check(c1.GetUsage = CU_ALL);
      CheckEqual(c1.GetSubject, 's1');
    end
    else
    begin
      // X509 and TEccCertificate V2 have proper Usage and Subjects support
      c1.Generate([cuCA, cuDigitalSignature, cuKeyCertSign],
        ' synopse.info, www.synopse.info ', nil);
      CheckEqual(RawUtf8ArrayToCsv(c1.GetSubjects),
        'synopse.info,www.synopse.info');
      check(c1.GetUsage = [cuCA, cuDigitalSignature, cuKeyCertSign]);
      CheckEqual(c1.GetSubject, 'synopse.info');
    end;
    Check(not c1.IsVoid);
    Check(c1.GetSerial <> '');
    Check(c1.GetSubjectKey <> '');
    Check(c1.IsSelfSigned);
    if c1.GetAuthorityKey <> c1.GetSubjectKey then // equal on syn-ecc
      CheckEqual(c1.GetAuthorityKey, '', 'X509 self-sign has no auth');
    Check(c1.Verify(nil) = cvValidSelfSigned, 'cvValidSelfSigned1');
    Check(c1.Verify(c1) = cvValidSelfSigned, 'cvValidSelfSigned2');
    Check(c1.GetSignatureInfo <> '');
    Check(c1.HasPrivateSecret);
    jwt := c1.JwtCompute([], {iss=}'myself', {sub=}'me', '', 0, 10);
    check(jwt <> '');
    check(TJwtAbstract.VerifyPayload(jwt, crt.JwtName, 'me', 'myself',
      '', nil, nil, nil, nil, nil) = jwtValid);
    iss := '';
    sub := '';
    check(c1.JwtVerify(jwt, @iss, @sub, nil) = cvValidSelfSigned, 'jwtverify');
    CheckEqual(iss, 'myself');
    CheckEqual(sub, 'me');
    check(c1.Handle <> nil);
    check(c1.IsValidDate);
    check(c1.GetNotBefore <= NowUtc);
    check(c1.GetNotAfter > NowUtc);
    check(c1.SetPrivateKey(c1.GetPrivateKey), 'in-place pk replace');
    for fmt := ccfBinary to ccfPem do
    begin
      c2 := crt.New;
      Check(c2.IsVoid);
      Check(not c2.IsValidDate);
      Check(not c2.IsEqual(c1));
      Check(c2.GetDigest <> c1.GetDigest);
      // validate c2=cccCertOnly persistence in PEM/DER
      s := c1.Save(cccCertOnly, '', fmt);
      check(c2.Load(s));
      Check(not c2.IsVoid);
      Check(not c2.HasPrivateSecret, 'nopwd=pubonly');
      CheckEqual(c2.JwtCompute([], 'myself', 'me', '', 0, 10), '');
      Check(c2.IsEqual(c1));
      Check(c2.Verify(nil) = cvValidSelfSigned, 'cvValidSelfSigned3');
      Check(c2.Verify(c2) = cvValidSelfSigned, 'cvValidSelfSigned4');
      CheckEqual(c2.GetSerial, c1.GetSerial);
      CheckEqual(c2.GetSubject, c1.GetSubject);
      CheckEqual(c2.GetIssuerName, c1.GetIssuerName);
      CheckEqual(c2.GetSubjectKey, c1.GetSubjectKey);
      CheckEqual(c2.GetDigest, c1.GetDigest);
      CheckSame(c2.GetNotAfter, c1.GetNotAfter);
      CheckSame(c2.GetNotBefore, c1.GetNotBefore);
      Check(c2.IsValidDate);
      CheckEqual(word(c2.GetUsage), word(c1.GetUsage));
      CheckEqual(c2.GetPeerInfo, c1.GetPeerInfo);
      iss := '';
      sub := '';
      check(c2.JwtVerify(jwt, @iss, @sub, nil) = cvValidSelfSigned, 'jwtverify2');
      CheckEqual(iss, 'myself');
      CheckEqual(sub, 'me');
      Check(c2.Handle <> nil);
      // validate c3=cccCertWithPrivateKey persistence in PEM/DER
      c3 := crt.New;
      Check(not c3.IsEqual(c1));
      Check(not c3.IsEqual(c2));
      s := c1.Save(cccCertWithPrivateKey, 'pwd', fmt);
      check(c3.Load(s, cccCertWithPrivateKey, 'pwd'));
      Check(c3.HasPrivateSecret, 'pwd=priv');
      Check(c3.IsEqual(c1));
      Check(c3.IsEqual(c2));
      CheckEqual(c3.GetSerial, c1.GetSerial);
      CheckEqual(c3.GetSubject, c1.GetSubject);
      CheckEqual(c3.GetIssuerName, c1.GetIssuerName);
      CheckEqual(c3.GetSubjectKey, c1.GetSubjectKey);
      CheckSame(c3.GetNotAfter, c1.GetNotAfter);
      CheckSame(c3.GetNotBefore, c1.GetNotBefore);
      CheckEqual(c3.GetDigest, c1.GetDigest);
      CheckEqual(word(c3.GetUsage), word(c1.GetUsage));
      if fmt = ccfPem then // PKCS12 seems to add some information to X509 :(
        CheckEqual(c3.GetPeerInfo, c1.GetPeerInfo);
      s := c1.Save;
      check(c2.load(s));
      checkEqual(c2.GetPrivateKey, '');
      check(c2.Load(c1.Save(cccPrivateKeyOnly, '', fmt), cccPrivateKeyOnly, ''));
      check(c2.HasPrivateSecret);
      checkEqual(c2.GetPrivateKey, c1.GetPrivateKey);
      Check(c2.IsEqual(c1));
      c2.SetPrivateKey('');
      Check(c2.IsEqual(c1));
      checkEqual(c2.GetPrivateKey, '');
      check(c2.Load(c1.Save(cccPrivateKeyOnly, 'pass', fmt), cccPrivateKeyOnly, 'pass'));
      check(c2.HasPrivateSecret);
      checkEqual(c2.GetPrivateKey, c1.GetPrivateKey);
      Check(c2.IsEqual(c1));
      c3 := crt.New;
      check(c3.Load(c1.Save(cccPrivateKeyOnly, 'pass2', fmt), cccPrivateKeyOnly, 'pass2'));
      check(c3.HasPrivateSecret, 'privkey with no main cert');
      checkEqual(c3.GetPrivateKey, c1.GetPrivateKey);
      Check(not c3.IsEqual(c1));
    end;
    checkEqual(c1.SharedSecret(nil), '', 'shared(nil)');
    // validate signed certificate with c1 as CA
    s3 := GuidToRawUtf8(RandomGuid);
    Check(TrimGuid(s3));
    c3 := crt.New;
    c3.Generate([cuDataEncipherment, cuKeyAgreement], s3, c1);
    Check(not c3.IsEqual(c1));
    Check(not c3.IsEqual(c2));
    Check(not c3.IsSelfSigned);
    if crt.AlgoName <> 'syn-es256-v1' then
      CheckEqual(c3.GetSubject, s3);
    Check(c3.HasPrivateSecret);
    CheckEqual(c3.GetAuthorityKey, c1.GetSubjectKey);
    Check(c3.Verify(nil) = cvUnknownAuthority, 'Verify(nil)');
    Check(c3.Verify(c1) = cvValidSigned, 'cvValidSigned1');
    Check(c3.Verify(c2) = cvValidSigned, 'cvValidSigned2');
    Check(c3.Verify(c3) = cvUnknownAuthority, 'Verify(c3)');
    n := '0123456789012345012345678901234'; // not a 16-byte multiple length
    r := c3.Encrypt(n);
    if r <> '' then // not all algorithms support encryption (RSA+ES256 only)
    begin
      CheckEqual(c3.Decrypt(r), n, 'asym ctr');
      r := c3.Encrypt(n, 'aes-128-cbc');
      CheckEqual(c3.Decrypt(r, 'aes-128-cbc'), n, 'another padding');
    end;
    s2 := GuidToRawUtf8(RandomGuid);
    Check(TrimGuid(s2));
    c2 := crt.New;
    fields.CommonName := s2;
    c2.Generate([cuDigitalSignature, cuKeyAgreement], '', nil, 30, -1, @fields);
    Check(c2.IsSelfSigned);
    if crt.AlgoName <> 'syn-es256-v1' then
      CheckEqual(c2.GetSubject, s2);
    if c2.GetAuthorityKey <> c2.GetSubjectKey then
      CheckEqual(c2.GetAuthorityKey, '', 'X509 self-sign has no auth');
    if crt.AlgoName <> 'syn-es256-v1' then
      Check(c2.GetUsage = [cuDigitalSignature, cuKeyAgreement]);
    Check(c2.Verify(c1) = cvValidSelfSigned, 'self1');
    Check(c2.Verify(nil) = cvValidSelfSigned, 'self2');
    c2.Sign(c1); // change signature
    CheckEqual(c2.GetAuthorityKey, c1.GetSubjectKey);
    Check(c2.Verify(c1) = cvValidSigned, 'self3');
    Check(c2.Verify(nil) = cvUnknownAuthority, 'self4');
    if crt.AlgoName = 'syn-es256-v1' then
      check(c1.SharedSecret(c3) = c3.SharedSecret(c1), 'c1.GetUsage=CU_ALL')
    else
    begin
      checkEqual(c1.SharedSecret(c3), '', 'c1(c3) no cuKeyAgreement');
      checkEqual(c3.SharedSecret(c1), '', 'c3(c1) no cuKeyAgreement');
    end;
    s := c2.SharedSecret(c3);
    check(c3.SharedSecret(c2) = s, 'sharedsecret');
    check( (s <> '') = (crt.AsymAlgo = caaES256), 'caaES256=sharedsecret');
    // c1 has [cuCA, cuDigitalSignature, cuKeyCertSign]
    // c2 has [cuDigitalSignature, cuKeyAgreement]
    // c3 has [cuDataEncipherment, cuKeyAgreement]
    cpe.Clear;
    check(cpe.Usages = []);
    check(not cpe.GetUsage(cuCA, c4));
    check(c4 = nil);
    check(cpe.Add(nil) = []);
    check(cpe.Usages = []);
    for u := low(u) to high(u) do
    begin
      check(not cpe.GetUsage(u, c4));
      check(c4 = nil);
    end;
    check(cpe.Add(c1) = []);
    check(cpe.Usages = c1.GetUsage);
    for u := low(u) to high(u) do
      if u in cpe.Usages then
      begin
        check(cpe.GetUsage(u, c4));
        check(c4 = c1);
      end
      else
      begin
        check(not cpe.GetUsage(u, c4));
        check(c4 = nil);
      end;
    if cpe.Usages = CU_ALL then // 'syn-es256-v1'
    begin
      check(cpe.Add(c2) = CU_ALL);
      check(cpe.Usages = CU_ALL);
      for u := low(u) to high(u) do
      begin
        check(cpe.GetUsage(u, c4));
        check(c4 = c2);
      end;
      check(cpe.Add(c3) = CU_ALL);
      check(cpe.Usages = CU_ALL);
      for u := low(u) to high(u) do
      begin
        check(cpe.GetUsage(u, c4));
        check(c4 = c3);
      end;
    end
    else
    begin
      check(cpe.GetUsage(cuCA, c4));
      check(c4 = c1);
      check(not cpe.GetUsage(cuKeyAgreement, c4));
      check(c4 = nil);
      check(cpe.GetUsage(cuDigitalSignature, c4));
      check(c4 = c1);
      check(not cpe.GetUsage(cuDataEncipherment, c4));
      check(c4 = nil);
      check(cpe.Add(c2) = [cuDigitalSignature]);
      check(cpe.Usages = [cuCA, cuDigitalSignature, cuKeyCertSign,
        cuKeyAgreement]);
      for u := low(u) to high(u) do
        check(cpe.GetUsage(u, c4) = (u in cpe.Usages));
      check(cpe.GetUsage(cuCA, c4));
      check(c4 = c1);
      check(cpe.GetUsage(cuKeyAgreement, c4));
      check(c4 = c2);
      check(cpe.GetUsage(cuDigitalSignature, c4));
      check(c4 = c2);
      check(not cpe.GetUsage(cuDataEncipherment, c4));
      check(c4 = nil);
      check(cpe.Add(c3) = [cuKeyAgreement]);
      check(cpe.Usages = [cuCA, cuDigitalSignature, cuKeyCertSign,
        cuKeyAgreement, cuDataEncipherment]);
      for u := low(u) to high(u) do
        check(cpe.GetUsage(u, c4) = (u in cpe.Usages));
      check(cpe.GetUsage(cuCA, c4));
      check(c4 = c1);
      check(cpe.GetUsage(cuKeyAgreement, c4));
      check(c4 = c3);
      check(cpe.GetUsage(cuDigitalSignature, c4));
      check(c4 = c2);
      check(cpe.GetUsage(cuDataEncipherment, c4));
      check(c4 = c3);
    end;
    s := cpe.AsBinary;
    check(s <> '');
    cpe.Clear;
    check(cpe.Usages = []);
    check(cpe.AsBinary = '');
    if crt.AlgoName = 'syn-es256-v1' then
    begin
      check(cpe.FromBinary(crt, s) = CU_ALL);
      check(cpe.Usages = CU_ALL);
    end
    else
    begin
      check(cpe.FromBinary(crt, s) = [cuDigitalSignature, cuKeyAgreement]);
      check(cpe.Usages = [cuCA, cuDigitalSignature, cuKeyCertSign,
        cuKeyAgreement, cuDataEncipherment]);
    end;
    for u := low(u) to high(u) do
    begin
      check(cpe.GetUsage(u, c4) = (u in cpe.Usages));
      check((c4 <> nil) = (u in cpe.Usages));
    end;
  end;
  // validate Store High-Level Algorithms Factory
  r := RandomAnsi7(100);
  alg := TCryptStoreAlgo.Instances;
  for a := 0 to high(alg) do
  begin
    str := alg[a] as TCryptStoreAlgo;
    AddAlgName;
    //writeln(str.AlgoName);
    st1 := str.New;
    CheckEqual(st1.Count, 0);
    // set c1 as self-signed root certificate (in v1 format)
    c1 := st1.DefaultCertAlgo.Generate([cuCA, cuKeyCertSign], 'rootca');
    Check(c1.IsSelfSigned);
    Check(c1.GetUsage = [cuCA, cuKeyCertSign]);
    //writeln(C1.GetPeerInfo);
    CheckEqual(c1.GetSubject, 'rootca');
    Check(st1.IsValid(c1) = cvUnknownAuthority);
    Check(st1.Add(c1));
    CheckEqual(st1.Count, 1);
    Check(st1.IsValid(c1) = cvValidSelfSigned);
    Check(c1.HasPrivateSecret, 'priv1');
    Check(c1.Sign(pointer(r), length(r)) = '', 'no cuDigitalSignature 1');
    Check((c1.GetAuthorityKey = '') or
          (c1.GetAuthorityKey = c1.GetSubjectKey));
    // set c2 as intermediate CA, signed by c1 root CA
    c2 := nil;
    Check(not st1.Add(c2), 'no priv');
    CheckEqual(st1.Count, 1);
    c2 := st1.DefaultCertAlgo.New;
    Check(c2.Instance.ClassType = c1.Instance.ClassType);
    c2.Generate([cuCA, cuKeyCertSign], 'mainca', c1);
    Check(not c2.IsSelfSigned);
    //writeln(c2.GetPeerInfo);
    Check(c2.GetUsage = [cuCA, cuKeyCertSign]);
    Check(cuCA in c2.GetUsage);
    CheckEqual(c2.GetSubject, 'mainca');
    Check(not (cuDigitalSignature in c2.GetUsage));
    Check(c2.HasPrivateSecret, 'priv2');
    Check(st1.IsValid(c2) = cvValidSigned, 'c2');
    Check(st1.Add(c2));
    CheckEqual(st1.Count, 2);
    Check(st1.IsValid(c2) = cvValidSigned, 'c2');
    Check(c2.Sign(pointer(r), length(r)) = '', 'no cuDigitalSignature 2');
    CheckEqual(c2.GetAuthorityKey, c1.GetSubjectKey);
    Check(c2.GetAuthorityKey <> c2.GetSubjectKey);
    // set c3 as signing authority
    c3 := st1.DefaultCertAlgo.New;
    c3.Generate([cuDigitalSignature], 'testsigning', c2);
    Check(not c3.IsSelfSigned);
    Check(c3.GetUsage = [cuDigitalSignature]);
    //writeln(c3.GetPeerInfo);
    CheckEqual(c3.GetSubject, 'testsigning');
    Check(c3.Instance.ClassType = c1.Instance.ClassType);
    CheckEqual(c3.Instance.CryptAlgo.AlgoName, c2.Instance.CryptAlgo.AlgoName);
    Check(c3.HasPrivateSecret, 'priv3');
    Check(st1.IsValid(c3) = cvValidSigned, 'c3');
    Check(st1.Add(c3));
    CheckEqual(c3.GetAuthorityKey, c2.GetSubjectKey);
    // sign
    s := c3.Sign(pointer(r), length(r));
    Check(s <> '', 'sign');
    cv := st1.Verify(s, pointer(r), length(r));
    if cv <> cvNotSupported then
      // TCryptStoreOpenSsl.Verify has no way to know which cert signed it
      CheckUtf8(cv = cvValidSigned, 's1=%', [ToText(cv)^]);
    // persist the Store
    st2 := str.NewFrom(st1.Save);
    CheckEqual(st2.Count, 3);
    Check(st2 <> nil);
    Check(st2.IsValid(c1) = cvValidSelfSigned, '2c1');
    Check(st2.IsValid(c2) = cvValidSigned, '2c2');
    Check(st2.IsValid(c3) = cvValidSigned, '2c3');
    if cv <> cvNotSupported then
    begin
      Check(st2.Verify(s, pointer(r), length(r)) = cvValidSigned, 's2a');
      dec(r[1]);
      Check(st2.Verify(s, pointer(r), length(r)) = cvInvalidSignature, 's2b');
      inc(r[1]);
      Check(st2.Verify(s, pointer(r), length(r)) = cvValidSigned, 's2c');
      // validate CRL
      Check(st2.Revoke(c3, 0, crrWithdrawn));
      Check(st2.Verify(s, pointer(r), length(r)) = cvRevoked, 's2d');
      Check(st2.Revoke(c3, 0, crrNotRevoked));
      Check(st2.Verify(s, pointer(r), length(r)) = cvValidSigned, 's2e');
    end;
    // ensure new certs are not recognized by previous stores
    if st3 <> nil then
    begin
      CheckEqual(st3.Count, 3);
      Check(st3.IsValid(c1) = cvUnknownAuthority, '3c1');
      Check(st3.IsValid(c2) = cvUnknownAuthority, '3c2');
      Check(st3.IsValid(c3) = cvUnknownAuthority, '3c3');
      if cv <> cvNotSupported then
        Check(st3.Verify(s, pointer(r), length(r)) = cvUnknownAuthority, 's3');
    end;
    st3 := st2;
  end;
  AddConsole(Utf8ToString(names));
end;

procedure TTestCoreCrypto._TBinaryCookieGenerator;
var
  gen: TBinaryCookieGenerator;
  i: PtrInt;
  bak: RawUtf8;
  timer: TPrecisionTimer;
  cook: array of RawUtf8;
  cookid: array of TBinaryCookieGeneratorSessionID;
begin
  SetLength(cook, 16384);
  SetLength(cookid, length(cook));
  gen.Init;
  timer.Start;
  for i := 0 to high(cook) do
    cookid[i] := gen.Generate(cook[i]);
  NotifyTestSpeed('generate', length(cook), 0, @timer);
  for i := 0 to high(cook) - 1 do
    Check(cookid[i] <> cookid[i + 1]);
  for i := 0 to high(cook) do
    Check(cookid[i] <> 0);
  for i := 0 to high(cook) do
    CheckEqual(gen.Validate(cook[i]), cookid[i], 'gen1');
  for i := 0 to high(cook) shr 4 do
    CheckEqual(gen.Validate(ParseTrailingJwt(
      '/uri/' + cook[i] + '  ', {nodot=}true)), cookid[i], 'gen2');
  bak := gen.Save;
  gen.Init;
  for i := 0 to high(cook) do
    CheckEqual(gen.Validate(cook[i]), 0, 'void');
  Check(gen.Load(bak), 'load');
  timer.Start;
  for i := 0 to high(cook) do
    CheckEqual(gen.Validate(cook[i]), cookid[i], 'loaded');
  NotifyTestSpeed('validate', length(cook), 0, @timer);
end;

initialization
  {$ifdef USE_OPENSSL}
  // warning: OpenSSL on Windows requires to download the right libraries
  RegisterOpenSsl;
  {$endif USE_OPENSSL}

end.

