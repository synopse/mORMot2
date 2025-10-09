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
  mormot.core.os.security,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.log,
  mormot.crypt.core,
  mormot.crypt.other,
  mormot.crypt.openssl,
  mormot.crypt.secure,
  mormot.core.perf,
  mormot.core.test,
  mormot.core.variants,
  mormot.core.threads, // for TLoggedWorker inlining
  mormot.lib.pkcs11,
  mormot.lib.openssl11,
  mormot.net.sock,     // for NetBinToBase64()
  mormot.crypt.jwt,
  mormot.crypt.ecc,
  mormot.crypt.rsa,
  mormot.crypt.x509;

type
  /// regression tests for mormot.crypt.core and mormot.crypt.jwt features
  TTestCoreCrypto = class(TSynTestCase)
  public
    fDigestAlgo: TDigestAlgo;
    fCatalogAllGenerate: boolean;
    procedure CryptData(dpapi: integer; const name: string);
    procedure Prng(meta: TAesPrngClass; const name, big: RawUtf8);
    function DigestUser(const User, Realm: RawUtf8;
      out HA0: THash512Rec): TAuthServerResult;
    procedure CatalogRunAsym(Context: TObject);
    procedure CatalogRunCert(Context: TObject);
    procedure CatalogRunStore(Context: TObject);
    procedure RsaSlow(Context: TObject);
    procedure CrcSlow(Context: TObject); // 32-bit, 64-bit and 128-bit hash
    procedure Rfc(a: TSignAlgo; const P, S: RawUtf8; c, l: integer;
      const exp, msg: RawUtf8);
    procedure Kdf(a: TSignAlgo; const key, exp, msg: RawUtf8;
      const lab: RawUtf8 = 'kerberos'; const ctx: RawUtf8 = '');
    procedure TestSCript(api: TSCriptRaw; const name: RawUtf8);
    procedure OpenSslTest(Algo: THashAlgo; const msg, exp: RawUtf8); overload;
    procedure OpenSslTest(Algo: THashAlgo; const msg, key, exp: RawUtf8); overload;
  published
    /// 32-bit to 128-bit hashing functions: crc32c, AesNiHash, MD5, MD4...
    procedure Hashes;
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
    /// pure pascal RSA tests
    procedure _RSA;
    /// X509 Certificates
    procedure _X509;
    /// stream-oriented cryptography
    procedure Streams;
    /// Base64/Base58/Base32 encoding/decoding functions
    procedure BaseEncoding;
    {$ifndef PUREMORMOT2}
    /// CompressShaAes() using SHA-256 / AES-256-CTR algorithm over SynLZ
    procedure _CompressShaAes;
    {$endif PUREMORMOT2}
    /// AES-based (and OpenSSL) pseudorandom number generator
    procedure _PRNG;
    /// CryptDataForCurrentUser() function
    procedure _CryptDataForCurrentUser;
    {$ifdef OSWINDOWS}
    /// CryptDataForCurrentUserApi() function
    procedure _CryptDataForCurrentUserApi;
    {$endif OSWINDOWS}
    /// CryptDataWithSecret() function
    procedure _CryptDataWithSecret;
    /// JWT classes
    procedure _JWT;
    /// validate TBinaryCookieGenerator object
    procedure _TBinaryCookieGenerator;
    /// mormot.lib.pkcs11 unit validation
    procedure Pkcs11;
    /// validate client-server DIGEST access authentication
    procedure Digest;
    /// test the TKerberosKeyTab class
    procedure _TKerberosKeyTab;
    /// High-Level Cryptography Catalog
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

// https://github.com/brycx/Test-Vector-Generation/blob/master/PBKDF2/pbkdf2-hmac-sha2-test-vectors.md
procedure TTestCoreCrypto.Rfc(a: TSignAlgo; const P, S: RawUtf8; c, l: integer;
  const exp, msg: RawUtf8);
var
  sign: TSynSigner;
  res: RawByteString;
begin
  res := sign.Pbkdf2(a, P, S, c, l);
  CheckEqual(length(res), l);
  CheckEqualHex(res, exp, msg);
end;

// https://www.rfc-editor.org/rfc/rfc8009#page-13
procedure TTestCoreCrypto.Kdf(a: TSignAlgo; const key, exp, msg, lab, ctx: RawUtf8);
var
  sign: TSynSigner;
  bin, res: RawByteString;
begin
  Check(HexToBin(pointer(key), length(key), bin));
  res := sign.KdfSP800(a, length(exp) shr 1, bin, lab, ctx);
  CheckEqual(length(res), length(exp) shr 1);
  CheckEqualHex(res, exp, msg);
end;

procedure TTestCoreCrypto.TestSCript(api: TSCriptRaw; const name: RawUtf8);

  procedure One(const Expected, Password, Salt: RawByteString; N, R, P: PtrUInt);
  var
    h: RawByteString;
  begin
    h := api(Password, Salt, N, R, P, length(Expected) div 2);
    Check(h <> '');
    CheckEqual(BinToHexLower(h), Expected, name);
  end;

var
  timer: TPrecisionTimer;
begin
  if not Assigned(api) then
    exit;
  timer.Start;
  CheckEqual(api('', '', 3, 1, 1, 16), '', 'N=3');
  One('48b0d2a8a3272611984c50ebd630af52',
      'p', 's', 2, 1, 1);
  One('8756bc2e73774a06194e4042746fe3d1',
      'p', 'p', 2, 1, 1);
  One('482c858e229055e62f41e0ec819a5ee18bdb87251a534f75acd95ac5e50aa15f',
      'password', 'salt', 2, 10, 10);
  One('88bd5edb52d1dd00188772ad36171290224e74829525b18d7323a57f91963c37',
      'password', 'salt', 16, 100, 100);
  One('77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442f' +
      'cd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906',
      '', '', 16, 1, 1);
  One('fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b3731622' +
      'eaf30d92e22a3886ff109279d9830dac727afb94a83ee6d8360cbdfa2cc0640',
      'password', 'NaCl', 1024, 8, 16);
  One('7023bdcb3afd7348461c06cd81fd38ebfda8fbba904f8e3ea9b543f6545da1f2d' +
      '5432955613f0fcf62d49705242a9af9e61e85dc0d651e40dfcf017b45575887',
      'pleaseletmein', 'SodiumChloride', 16384, 8, 1);
  One('c3f182ee2dec846e70a6942fb529985a3a09765ef04c612923b17f18555a37076' +
      'deb2b9830d69de5492651e4506ae5776d96d40f67aaee37e1777b8ad5c3111432' +
      'bb3b6f7e1264401879e641ae', 'this is a long '#0' password',
      'and this is a long '#0' salt', 16384, 8, 1);
  NotifyTestSpeed('%', [name], 0, 0, @timer, fOwner.MultiThread);
end;

procedure TTestCoreCrypto.OpenSslTest(Algo: THashAlgo; const msg, exp: RawUtf8);
begin
  {$ifdef USE_OPENSSL}
  if TOpenSslHash.IsAvailable then
    CheckEqual(TOpenSslHash.Hash(Algo, msg), LowerCase(exp));
  {$endif USE_OPENSSL}
end;

procedure TTestCoreCrypto.OpenSslTest(Algo: THashAlgo; const msg, key, exp: RawUtf8);
begin
  {$ifdef USE_OPENSSL}
  if TOpenSslHmac.IsAvailable then
    CheckEqual(TOpenSslHmac.Hmac(Algo, msg, key), LowerCase(exp));
  {$endif USE_OPENSSL}
end;

procedure TTestCoreCrypto._SHA1;

  procedure DoTest;
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

begin
  DoTest;
  {$ifdef ASMX64}
  if cfSHA in CpuFeatures then
  begin
    Exclude(CpuFeatures, cfSHA); // validate regular code without SHA-NI
    DoTest;
    Include(CpuFeatures, cfSHA);
  end;
  {$endif ASMX64}
  // see https://datatracker.ietf.org/doc/html/rfc6070
  Rfc(saSha1, 'password', 'salt', 1, 20,
      '0c60c80f961f0e71f3a9b524af6012062fe037a6', '1 round');
  Rfc(saSha1, 'password', 'salt', 2, 20,
      'ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957', '2 rounds');
  Rfc(saSha1, 'passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt',
    4096, 25, '3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038', 'bigger');
  Rfc(saSha1, 'pass'#0'word', 'sa'#0'lt', 4096, 16,
      '56fa6aa75548099dcc37d7f03425e0c3', 'truncated');
  // do nothing if OpenSSL is not available
  OpenSslTest(hfSHA1, 'Wikipedia, l''encyclopedie libre et gratuite',
    'c18cc65028bbdc147288a2d136313287782b9c73');
  OpenSslTest(hfSHA1, '', '', 'fbdb1d1b18aa6c08324b7d64b71fb76370690e1d');
  OpenSslTest(hfSHA1, 'The quick brown fox jumps over the lazy dog', 'key',
    'de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9');
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
    OpenSslTest(hfSHA256, s, Sha256DigestToString(TDig));
  end;

  procedure DoTest;
  // validate against some well known (e.g. FIPS186-2) reference vectors
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
    { $ python
      >>> import hashlib
      >>> hashlib.pbkdf2_hmac('sha256', b'password', b'salt', 4096).hex() }
    DIG4096 = 'c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a';
  var
    Digest: THash512Rec;
    Digests: THash256DynArray;
    sign: TSynSigner;
    s: RawUtf8;
    c: AnsiChar;
    i: PtrInt;
    sha: TSha256;
  begin
    SingleTest('abc', D1);
    SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
    {%H-}Sha256Weak('lagrangehommage', Digest.Lo); // test with len=256>64
    Check(IsEqual(Digest.Lo, D3));
    Pbkdf2HmacSha256('password', 'salt', 1, Digest.Lo);
    check(Sha256DigestToString(Digest.Lo) =
      '120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b');
    Pbkdf2HmacSha256('password', 'salt', 2, Digest.Lo);
    check(Sha256DigestToString(Digest.Lo) =
      'ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43');
    SetLength(Digests, 2);
    check(IsZero(Digests[0]));
    check(IsZero(Digests[1]));
    Pbkdf2HmacSha256('password', 'salt', 4096, Digest.Lo);
    check(Sha256DigestToString(Digest.Lo) = DIG4096);
    FillZero(Digest.b);
    sign.Pbkdf2(saSha256, 'password', 'salt', 4096, @Digest);
    check(Sha256DigestToString(Digest.Lo) = DIG4096);
    s := BinToHexLower(sign.Pbkdf2(saSha256, 'password', 'salt', 1, 20));
    CheckEqual(s, '120fb6cffcf8b32c43e7225256c4f837a86548c9');
    s := BinToHexLower(Pbkdf2HmacSha256('password', 'salt', 1, 20));
    CheckEqual(s, '120fb6cffcf8b32c43e7225256c4f837a86548c9');
    c := 'a';
    sha.Init;
    for i := 1 to 1000000 do // one million 'a' chars, read one-by-one
      sha.Update(@c, 1);
    sha.Final(Digest.Lo);
    Check(Sha256DigestToString(Digest.Lo) =
      'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0');
    CheckEqual(Sha224(''),
      'd14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f');
    s := Sha256('');
    CheckEqual(s, 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
    OpenSslTest(hfSha256, '', s);
    s := Sha224('The quick brown fox jumps over the lazy dog');
    CheckEqual(s, '730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525');
    OpenSslTest(hfSha224, 'The quick brown fox jumps over the lazy dog', s);
    s := Sha224('The quick brown fox jumps over the lazy dog.');
    CheckEqual(s, '619cba8e8e05826e9b8c519c0a5c68f4fb653e8a3d8aa04bb2c8cd4c');
    OpenSslTest(hfSha224, 'The quick brown fox jumps over the lazy dog.', s);
  end;

begin
  DoTest;
  {$ifdef ASMX64}
  if cfSSE41 in CpuFeatures then // validate regular code without Sha256Sse4()
  begin
    Exclude(CpuFeatures, cfSSE41);
    DoTest;
    Include(CpuFeatures, cfSSE41);
  end;
  if cfSHA in CpuFeatures then // validate regular code without SHA-NI
  begin
    Exclude(CpuFeatures, cfSHA);
    DoTest;
    Include(CpuFeatures, cfSHA);
  end;
  {$endif ASMX64}
// https://github.com/brycx/Test-Vector-Generation/blob/master/PBKDF2/pbkdf2-hmac-sha2-test-vectors.md
  Rfc(saSha224, 'password', 'salt', 1, 20,
      '3c198cbdb9464b7857966bd05b7bc92bc1cc4e6e', '1 round');
  Rfc(saSha224, 'password', 'salt', 2, 20,
      '93200ffa96c5776d38fa10abdf8f5bfc0054b971', '2 rounds');
  Rfc(saSha224, 'passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt',
    4096, 25, '056c4ba438ded91fc14e0594e6f52b87e1f3690c0dc0fbc057', 'bigger');
  Rfc(saSha256, 'password', 'salt', 1, 20,
      '120fb6cffcf8b32c43e7225256c4f837a86548c9', '1 round');
  Rfc(saSha256, 'password', 'salt', 2, 20,
      'ae4d0c95af6b46d32d0adff928f06dd02a303f8e', '2 rounds');
  Rfc(saSha256, 'passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt',
    4096, 25, '348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c', 'bigger');
  // https://www.rfc-editor.org/rfc/rfc8009#page-13
  Kdf(saSha256, '3705D96080C17728A0E800EAB6E0D23C',
   'B31A018A48F54776F403E9A396325DC3',
   'Kc128', HexToBin('0000000299'));
  Kdf(saSha256, '3705D96080C17728A0E800EAB6E0D23C',
   '9B197DD1E8C5609D6E67C3E37C62C72E',
   'Ke128', HexToBin('00000002AA'));
  Kdf(saSha256, '3705D96080C17728A0E800EAB6E0D23C',
   '9FDA0E56AB2D85E1569A688696C26A6C',
   'Kc128', HexToBin('0000000255'));
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
  key := RandomWinAnsi(100);
  Check(length(key) = 100);
  for ks := 1 to 10 do
  begin
    ref.InitSha3(pointer(key)^, ks * 10);
    for i := 0 to 100 do
    begin
      len := i * 3;
      s := RandomAnsi7(len);
      FastNewRawByteString(d, len);
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
      if password <> '' then
        OpenSslTest(hfSHA512, secret, password, expected);
    end
    else
    begin
      Pbkdf2HmacSha512(password, secret, rounds, dig.b);
      Check(Sha512DigestToString(dig.b) = expected);
      FillZero(dig.b);
      sign.Pbkdf2(saSha512, password, secret, rounds, @dig);
      Check(Sha512DigestToString(dig.b) = expected);
    end;
  end;

  procedure DoTest;
  const
    FOX: RawByteString = 'The quick brown fox jumps over the lazy dog';
    ABCU: RawByteString = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijk' +
      'lmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu';
  var
    dig: THash512Rec;
    i: PtrInt;
    sha: TSha512;
    c: AnsiChar;
    temp: RawByteString;
  begin
    // includes SHA-384 and SHA-512/256, which are truncated SHA-512
    CheckEqual(SHA384(''),
      '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63' +
      'f6e1da274edebfe76f65fbd51ad2f14898b95b');
    CheckEqual(SHA384('abc'),
      'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605' +
      'a43ff5bed8086072ba1e7cc2358baeca134c825a7');
    CheckEqual(SHA384(ABCU), '09330c33f711' +
      '47e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039');
    CheckEqual(Sha512_256(''),
      'c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a');
    CheckEqual(Sha512_256('abc'),
      '53048e2681941ef99b2e29b76b4c7dabe4c2d0c634fc6d46e0e2f13107e7af23');
    CheckEqual(Sha512_256(ABCU),
      '3928e184fb8690f840da3988121d31be65cb9d3ef83ee6146feac861e19b563a');
    CheckEqual(Sha512(''),
      'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d' +
      '36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e');
    CheckEqual(Sha512(FOX),
      '07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785' +
      '436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
    CheckEqual(Sha512(FOX + '.'),
      '91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d128' +
      '90cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed');
    sha.Init;
    for i := 1 to length(FOX) do
      sha.Update(@FOX[i], 1);
    sha.Final(dig.b);
    Check(Sha512DigestToString(dig.b) = '07e547d9586f6a73f73fbac0435ed76951218fb7d0c' +
      '8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
    OpenSslTest(hfSHA512, '', 'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d' +
      '36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e');
    OpenSslTest(hfSHA512, FOX,
      '07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785' +
      '436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
    c := 'a';
    sha.Init;
    for i := 1 to 1000 do
      sha.Update(@c, 1);
    sha.Final(dig.b); // one thousand 'a' char
    Check(Sha512DigestToString(dig.b) =
      '67ba5535a46e3f86dbfbed8cbbaf0125c76ed549ff8' +
      'b0b9e03e0c88cf90fa634fa7b12b47d77b694de488ace8d9a65967dc96df599727d3292a8d9d447709c97');
    SetLength(temp, 1000);
    FillCharFast(pointer(temp)^, 1000, ord('a'));
    Check(Sha512(temp) = Sha512DigestToString(dig.b));
    for i := 1 to 1000000 do
      sha.Update(@c, 1);
    sha.Final(dig.b); // one million 'a' char
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
    CheckEqual(Sha384DigestToString(dig.b384), 'af45d2e376484031617f78d2b58a6b1' +
      'b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649');
    Pbkdf2HmacSha384('password', 'salt', 4096, dig.b384);
    CheckEqual(Sha384DigestToString(dig.b384), '559726be38db125bc85ed7895f6e3cf574c7a01c' +
      '080c3447db1e8a76764deb3c307b94853fbe424f6488c5f4f1289626');
    Pbkdf2HmacSha512('passDATAb00AB7YxDTT', 'saltKEYbcTcXHCBxtjD', 1, dig.b);
    CheckEqual(Sha512DigestToString(dig.b), 'cbe6088ad4359af42e603c2a33760ef9d4017a7b2aad10af46' +
      'f992c660a0b461ecb0dc2a79c2570941bea6a08d15d6887e79f32b132e1c134e9525eeddd744fa');
    Pbkdf2HmacSha384('passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqK',
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcG', 1, dig.b384);
    CheckEqual(Sha384DigestToString(dig.b384),
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

begin
  DoTest;
  {$ifdef ASMX86}
  if cfSSSE3 in CpuFeatures then // validate regular code without sha512_compress()
  begin
    Exclude(CpuFeatures, cfSSSE3);
    DoTest;
    Include(CpuFeatures, cfSSSE3);
  end;
  {$endif ASMX86}
  {$ifdef ASMX64}
  if cfSSE41 in CpuFeatures then // validate regular code without sha512_sse4()
  begin
    Exclude(CpuFeatures, cfSSE41);
    DoTest;
    Include(CpuFeatures, cfSSE41);
  end;
  {$endif ASMX64}
// https://github.com/brycx/Test-Vector-Generation/blob/master/PBKDF2/pbkdf2-hmac-sha2-test-vectors.md
  Rfc(saSha384, 'password', 'salt', 1, 20,
      'c0e14f06e49e32d73f9f52ddf1d0c5c719160923', '1 round');
  Rfc(saSha384, 'password', 'salt', 2, 20,
      '54f775c6d790f21930459162fc535dbf04a93918', '2 rounds');
  Rfc(saSha384, 'passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt',
    4096, 25, '819143ad66df9a552559b9e131c52ae6c5c1b0eed18f4d283b', 'bigger');
  Rfc(saSha512, 'password', 'salt', 1, 20,
      '867f70cf1ade02cff3752599a3a53dc4af34c7a6', '1 round');
  Rfc(saSha512, 'password', 'salt', 2, 20,
      'e1d9c16aa681708a45f5c7c4e215ceb66e011a2e', '2 rounds');
  Rfc(saSha512, 'passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt',
    4096, 25, '8c0511f4c6e597c6ac6315d8f0362e225f3c501495ba23b868', 'bigger');
  // https://www.rfc-editor.org/rfc/rfc8009#page-13
  Kdf(saSha384, '6D404D37FAF79F9DF0D33568D320669800EB4836472EA8A026D16B7182460C52',
   'EF5718BE86CC84963D8BBB5031E9F5C4BA41F28FAF69E73D',
   'Kc256', HexToBin('0000000299'));
  Kdf(saSha384, '6D404D37FAF79F9DF0D33568D320669800EB4836472EA8A026D16B7182460C52',
   '56AB22BEE63D82D7BC5227F6773F8EA7A5EB1C825160C38312980C442E5C7E49',
   'Ke256', HexToBin('00000002AA'));
  Kdf(saSha384, '6D404D37FAF79F9DF0D33568D320669800EB4836472EA8A026D16B7182460C52',
   '69B16514E3CD8E56B82010D5C73012B622C4D00FFC23ED1F',
   'Ki256', HexToBin('0000000255'));
end;

procedure TTestCoreCrypto._SHA3;

  procedure DoTest;
  const
    HASH1 = '79f38adec5c20307a98ef76e8324afbfd46cfd81b22e3973c65fa1bd9de31787';
    DK    = '7bbdbe37ea70dd2ed640837ff8a926d381806ffa931695addd38ab950d35ad18' +
            '801a8290e8d97fe14cdfd3cfdbcd0fe766d3e6e4636bd0a17d710a61678db363';
  var
    instance: TSha3;
    secret, data, encrypted, h: RawByteString;
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
      '0C63A75B845E4F7D01107D852E4C2485C51A50AAAA94FC61995E71BBEE983A2A' +
      'C3713831264ADB47FB6BD1E058D5F004');
    CheckEqual(instance.FullStr(SHA3_512, nil, 0),
      'A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80A6' +
      '15B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26');
    CheckEqual(instance.FullStr(SHAKE_128, nil, 0),
      '7F9C2BA4E88F827D616045507605853ED73B8093F6EFBC88EB1A6EACFA66EF26');
    CheckEqual(instance.FullStr(SHAKE_256, nil, 0),
      '46B9DD2B0BA88D13233B3FEB743EEB243FCD52EA62B81B82B50C27646ED5762F' +
      'D75DC4DDD8C0F200CB05019D67B592F6FC821C49479AB48640292EACB3B7C4BE');
    SetLength(data, 200);
    FillCharFast(pointer(data)^, 200, $A3);
    CheckEqual(instance.FullStr(SHA3_224, pointer(data), length(data)),
      '9376816ABA503F72F96CE7EB65AC095DEEE3BE4BF9BBC2A1CB7E11E0');
    CheckEqual(instance.FullStr(SHA3_256, pointer(data), length(data)),
      '79F38ADEC5C20307A98EF76E8324AFBFD46CFD81B22E3973C65FA1BD9DE31787');
    CheckEqual(instance.FullStr(SHA3_384, pointer(data), length(data)),
      '1881DE2CA7E41EF95DC4732B8F5F002B189CC1E42B74168ED1732649CE1DBCDD' +
      '76197A31FD55EE989F2D7050DD473E8F');
    CheckEqual(instance.FullStr(SHA3_512, pointer(data), length(data)),
      'E76DFAD22084A8B1467FCF2FFA58361BEC7628EDF5F3FDC0E4805DC48CAEECA8' +
      '1B7C13C30ADF52A3659584739A2DF46BE589C51CA1A4A8416DF6545A1CE8BA00');
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
    CheckEqualHex(encrypted, 'bf013a29');
    for s := 0 to 3 do
    begin
      secret := RandomWinAnsi(s * 3);
      Check(instance.Cypher(secret, '') = '');
      for i := 1 to 1000 do
      begin
        data := RandomWinAnsi(i);
        Check(length(data) = i);
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
    checkEqual(Sha512DigestToString(h512.b), DK);
    FillZero(h512.b);
    check(Sha512DigestToString(h512.b) <> DK);
    sign.Pbkdf2(saSha3512, 'pass', 'salt', 1000, @h512);
    checkEqual(Sha512DigestToString(h512.b), DK);
    FillZero(h512.b);
    check(Sha512DigestToString(h512.b) <> DK);
    sign.Pbkdf2('{algo:"sha-3/512",secret:"pass",salt:"salt",rounds:1000}', h512);
    checkEqual(Sha512DigestToString(h512.b), DK);
    sign.Pbkdf2('{algo:"sha-3/512",secret:"pass",salt:"salt",rounds:100}', h512);
    check(Sha512DigestToString(h512.b) <> DK);
    // taken from https://en.wikipedia.org/wiki/SHA-3
    h := 'F4202E3C5852F9182A0430FD8144F0A74B95E7417ECAE17DB0F8CFEED0E3E66E';
    CheckEqual(Sha3(SHAKE_128, 'The quick brown fox jumps over the lazy dog'), h);
    SetLength(h, length(h) div 2);
    OpenSslTest(hfShake128, 'The quick brown fox jumps over the lazy dog', h);
    h := '853F4538BE0DB9621A6CEA659A06C1107B1F83F02B13D18297BD39D7411CF10C';
    CheckEqual(Sha3(SHAKE_128, 'The quick brown fox jumps over the lazy dof'), h);
    SetLength(h, length(h) div 2);
    OpenSslTest(hfShake128, 'The quick brown fox jumps over the lazy dof', h);
  end;

begin
  DoTest;
  {$ifdef ASMX64AVXNOCONST}
  if cpuAVX2 in X64CpuFeatures then // validate without KeccakPermutationAvx2()
  begin
    Exclude(X64CpuFeatures, cpuAVX2);
    DoTest;
    Include(X64CpuFeatures, cpuAVX2);
  end;
  {$endif ASMX64AVXNOCONST}
end;

procedure TTestCoreCrypto._PRNG;
var
  timer: TPrecisionTimer;
  i: integer;
  big: RawByteString;
begin
  SetLength(big, 100000);
  // validate TAesPrgn (+ TAesPrngOsl) generators
  check(TAesPrng.IsAvailable);
  check(TSystemPrng.IsAvailable);
  Prng(TAesPrng, 'mORMot', big);
  {$ifdef USE_OPENSSL}
  Prng(TAesPrngOsl, 'OpenSSL', big);
  {$endif USE_OPENSSL}
  // include Lecuyer for comparison, with same benchmarks as in Prng()
  timer.Start;
  CheckEqual(Random32(0), 0);
  CheckEqual(Random32(1), 0);
  for i := 1 to 50000 do
    Check(Random32(i) < cardinal(i));
  for i := 0 to 50000 do
    Check(Random32(maxInt - i) < cardinal(maxInt - i));
  NotifyTestSpeed('Lecuyer Random32', [], 100003, 100003 * 4, @timer);
  timer.Start;
  for i := 1 to 100 do
    RandomBytes(pointer(big), length(big));
  NotifyTestSpeed('       Lecuyer RandomBytes', [], 1, length(big) * 10, @timer);
end;

procedure TTestCoreCrypto.Prng(meta: TAesPrngClass; const name, big: RawUtf8);
var
  p: TAesPrngAbstract;
  b1, b2: TAesBlock;
  a1, a2: TAesPrngAbstract;
  s1, s2, split: RawByteString;
  c: cardinal;
  d: double;
  e: TSynExtended;
  i, j, stripes: PtrInt;
  clo, chi, dlo, dhi, elo, ehi: integer;
  timer: TPrecisionTimer;
begin
  if not meta.IsAvailable then
    exit;
  // basic 16-bytes AES block validation
  FillZero(b1);
  FillZero(b2);
  Check(IsZero(b1));
  Check(IsZero(b2));
  Check(IsEqual(b1, b2));
  Check(CompareMem(@b1, @b2, SizeOf(b1)));
  p := meta.Main;
  p.FillRandom(b1);
  Check(not IsZero(b1));
  Check(IsZero(b2));
  Check(not IsEqual(b1, b2));
  p.FillRandom(b2);
  Check(not IsZero(b2));
  Check(not IsEqual(b1, b2));
  Check(not CompareMem(@b1, @b2, SizeOf(b1)));
  // validate this PRNG class
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
    CheckEqual(a1.FillRandom(0), '');
    CheckEqual(a1.FillRandomHex(0), '');
    Check(a1.Random32(0) = 0);
    Check(a1.Random32(1) = 0);
    for i := 1 to 2000 do
    begin
      s1 := '';
      s2 := '';
      s1 := a1.FillRandom(i);
      s2 := a2.FillRandom(i);
      CheckEqual(length(s1), i);
      CheckEqual(length(s2), i);
      if i > 4 then
        checkUtf8(s1 <> s2, 'prng len=%', [i]);
      // compress the output to validate (somehow) its randomness
      check(length(AlgoSynLZ.Compress(s1)) > i, 'random1 should not compress');
      check(length(AlgoSynLZ.Compress(s2)) > i, 'random2 should not compress');
      // validate other string generation methods
      s1 := a1.FillRandomHex(i);
      CheckEqual(length(s1), i * 2);
      check(mormot.core.text.HexToBin(pointer(s1), nil, i));
      s1 := a1.RandomPassword(i);
      CheckEqual(length(s1), i);
      for j := 1 to i do
        check(s1[j] in [#33 .. #126]);
      // verify Random32 / RandomDouble / RandomDouble distribution
      c := a1.Random32;
      if c < cardinal(maxint) then
        inc(clo)
      else
        inc(chi);
      check(c <> a2.Random32, 'Random32 collision');
      check(c <> a1.Random32, 'Random32 twice collision');
      check(a1.Random64 <> a2.Random64, 'Random64 collision');
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
  CheckEqual(clo + chi, 2000);
  CheckEqual(dlo + dhi, 4000);
  CheckEqual(elo + ehi, 4000);
  CheckUtf8((clo >= 900) and
            (clo <= 1100), 'Random32 distribution clo=%', [clo]);
  CheckUtf8((dlo >= 1800) and
            (dlo <= 2200), 'RandomDouble distribution dlo=%', [dlo]);
  CheckUtf8((elo >= 1800) and
            (elo <= 2200), 'RandomExt distribution elo=%', [elo]);
  // verify AFSplit/AFUnsplit anti-forensic secret distribution
  s1 := p.FillRandom(100);
  for i := 1 to length(s1) do
    for stripes := 0 to 10 do
    begin
      split := p.AFSplit(pointer(s1)^, i, stripes);
      CheckEqual(length(split), i * (stripes + 1));
      check(TAesPrng.AFUnsplit(split, pointer(s2)^, i));
      check(CompareMem(pointer(s1), pointer(s2), i));
    end;
  CheckEqual(PosEx(s1, split), 0);
  // some raw benchmark
  timer.Start;
  CheckEqual(p.Random32(0), 0);
  CheckEqual(p.Random32(1), 0);
  for i := 1 to 50000 do
    Check(p.Random32(i) < cardinal(i));
  for i := 0 to 50000 do
    Check(p.Random32(maxInt - i) < cardinal(maxInt - i));
  NotifyTestSpeed('% Random32', [name], 100003, 100003 * 4, @timer);
  timer.Start;
  for i := 1 to 100 do
    p.FillRandom(pointer(big), length(big));
  NotifyTestSpeed('       % FillRandom', [name], 1, length(big) * 100, @timer);
end;

function CryptDataSecretWrapper(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;
begin
  result := CryptDataWithSecret(Data, [AppSecret]);
end;

procedure TTestCoreCrypto.CryptData(dpapi: integer; const name: string);
var
  i, size, max: integer;
  plain, enc, test: RawByteString;
  appsec: RawUtf8;
  func: function(const Data, AppSecret: RawByteString; Encrypt: boolean): RawByteString;
  tim: TPrecisionTimer;
begin
  max := 1000;
  case dpapi of
    {$ifdef OSWINDOWS}
    0:
      func := CryptDataForCurrentUserDPAPI;
    {$endif OSWINDOWS}
    1:
      func := CryptDataForCurrentUser;
    2:
      begin
        func := CryptDataSecretWrapper;
        max := 50; // Pbkdf2Sha3() is slow
      end
  else
    exit;
  end;
  enc := func('warmup', 'appsec', true);
  Check(enc <> '');
  test := func(enc, 'appsec', false);
  Check(test <> '');
  CheckEqual(test, 'warmup');
  size := 0;
  tim.Start;
  for i := 0 to max - 1 do
  begin
    plain := RandomAnsi7(i);
    CheckEqual(length(plain), i);
    UInt32ToUtf8(i, appsec);
    enc := func(plain, appsec, true);
    if not ((plain = '') or
            (enc <> '')) then
      enc := func(plain, appsec, true);
    check((plain = '') or
          (enc <> ''));
    check(length(enc) >= length(plain));
    test := func(enc, appsec, false);
    CheckEqual(length(test), i);
    CheckEqual(test, plain);
    inc(size, i + length(enc));
  end;
  NotifyTestSpeed(name, max * 2, size, @tim);
end;

procedure TTestCoreCrypto._CryptDataForCurrentUser;
begin
  CryptData(1, 'AES-CFB');
end;

{$ifdef OSWINDOWS}
procedure TTestCoreCrypto._CryptDataForCurrentUserApi;
begin
  CryptData(0, 'DPAPI');
end;
{$endif OSWINDOWS}

procedure TTestCoreCrypto._CryptDataWithSecret;
begin
  CryptData(2, 'PBKDF2-SHAKE128');
end;

const
  _rsapriv = // from "openssl genrsa -out priv.pem 2048"
    '-----BEGIN PRIVATE KEY-----'#13#10 +
    'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC1Dj92HMReUOmP'#13#10 +
    'vV3AXdO3VLH8I693pjxHiTGBwtuWZ23cEKYYBiA1LbJ/Q5FwYqx77Bgsb4FXyshP'#13#10 +
    'bNE9ushd8QmaeOWexDrfmf1syl+EUjKv2kTjDnLllQDMlK3Bia53KQUB8Dv/UFao'#13#10 +
    'P64TfPk+p+eZkcRtg0d4Ea3S8hFJ9BBdfGP7CelpD78ZNhCvk4fKfIlLx6wP12kp'#13#10 +
    '+wTFzusHHhUfW0LHVIf3JUAwe2zTrvgeOefIXK96hc5qM3WDGfj+I3WV9ZnPOhar'#13#10 +
    'JW8pZnleK0IP8UR2FuMVtYqJuuS71z8/sc3PuiozHZCSEZr5qx59A2fAW/42ppz2'#13#10 +
    'tMI/uV17AgMBAAECggEACSI/ClbY2Ic4GSkXndjokYLuIwcBLEXlxu1sDXHxQPer'#13#10 +
    'yk0etpnh2GebQyH7UtJE2vrl8faYGFU3C2a1HD+cKb0QsSBwjl9eNvczmorlDFhX'#13#10 +
    'N+7eHcmV+0YBOdDgU8yofANvk2b1Uo57fgEPEkBHlKUaIRxXmUy6TMYw4NzsXrDM'#13#10 +
    'XyBDL3sGnjiKZ7IX6Wsx32SLyrFsHMYI5QWIb24nGi7/jeq4q8GZf/0gaL7WoSK6'#13#10 +
    'DDqGI9l2QiK6LThuOsNNaEC9fBndjsh3RW4yWLRsQZ6QDVXhlMxRxG8uuU4bsuYc'#13#10 +
    'XFoNCAYMIF+iWrAV/0g2GayAe5rzgB+z2DRyWLWlHQKBgQDoklA/vrBx8LxOkDVo'#13#10 +
    'IOl4Yskx6pRWNa8SIz6JMI387Hy7Uz3abiEBx0KjQPWv9I5fIySPTsHWsuNAdYKs'#13#10 +
    'v4b3DGS0QUlJRyGuVLx055E7uwcpk1I742t0lFgNZi7AOYT/cUO0jcDwkkRJZ+3k'#13#10 +
    'xqRhZv+GVJOUbm61byZRuyiApwKBgQDHS2noTFXlr9uHIHHn67ccTbCkjArDfgdu'#13#10 +
    'fxh2Fphst2ue+cPTc0L9lDND8EPAGZK3ffmcEbkjykF/Dey+3RXXCxELx7VjNPIJ'#13#10 +
    'STk0+7ysHX7/1ThuTa+vb/xdeHNglQXRTV9K5e1+3ucHuT/oeacyop8/Kzku8Qwr'#13#10 +
    '1LTx0MwjDQKBgDYjmTq9kSV0/ODtAQG0Z6T2mg9cpBtNc+us+KnG+8ac5oxU3Fk0'#13#10 +
    'ucpIMGMAhDDppRrQe3pAwy7PhcdDk5/TFf/8ipTLfdvpCxYh85zjKxPUfd5XxRTb'#13#10 +
    '4+/HeJfl6Ywl16f/HduyA+/8nJjZ8K8I7ssdxu3mUlSDQJJLxYfRIaSRAoGBAJmV'#13#10 +
    'Q2uykCuup2XuGfnZjEZylKNqDM107TM5HNe8OADoJTbhUgk89S5ILG251exPiOKB'#13#10 +
    'YX/lpKCxOGI6j+zSogcTzzId2Go4niGL3Vs4eMDHBl0PqypOEgsIKRq7PWb70Pzo'#13#10 +
    'PHySzsCL9Mzd9SMpxTDfZAuhOrMzLecFR+BmwTptAoGAN3g6MjW6nThBvrWtaQ6j'#13#10 +
    '2xE7gNaCgu0UVfTnMVmXD1Cbji3zW4+1lhQCGTnwrBhv54t1S1v6ilFCOtsZfLEC'#13#10 +
    'ZHhnT2RzGDGHrq115yC+T8SwTo7/h5p/2AuO4fXWP6MWXMJcXUGs6MshY5vgH4QY'#13#10 +
    'BPyNxBYuEhvuYUZ3nJXJZZ0='#13#10 +
    '-----END PRIVATE KEY-----'#13#10;
  _rsapub = // openssl rsa -pubout -in priv.pem -out pub.pem
    '-----BEGIN PUBLIC KEY-----'#13#10 +
    'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtQ4/dhzEXlDpj71dwF3T'#13#10 +
    't1Sx/COvd6Y8R4kxgcLblmdt3BCmGAYgNS2yf0ORcGKse+wYLG+BV8rIT2zRPbrI'#13#10 +
    'XfEJmnjlnsQ635n9bMpfhFIyr9pE4w5y5ZUAzJStwYmudykFAfA7/1BWqD+uE3z5'#13#10 +
    'PqfnmZHEbYNHeBGt0vIRSfQQXXxj+wnpaQ+/GTYQr5OHynyJS8esD9dpKfsExc7r'#13#10 +
    'Bx4VH1tCx1SH9yVAMHts0674HjnnyFyveoXOajN1gxn4/iN1lfWZzzoWqyVvKWZ5'#13#10 +
    'XitCD/FEdhbjFbWKibrku9c/P7HNz7oqMx2QkhGa+asefQNnwFv+Nqac9rTCP7ld'#13#10 +
    'ewIDAQAB'#13#10 +
    '-----END PUBLIC KEY-----'#13#10;

procedure TTestCoreCrypto._JWT;

  procedure test(one: TJwtAbstract; nofree: boolean = false);
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
    check(ParseJwt(t, jwt) = jwtValid);
    CheckEqual(jwt.reg[jrcIssuer], 'joe');
    check(TJwtAbstract.VerifyPayload(
      t, '', '', 'joe', '', nil, nil, nil, nil, nil, @v) = jwtValid);
    Finalize(jwt);
    CheckEqual(jwt.reg[jrcIssuer], '');
    check(ParseJwt(t, jwt) = jwtValid);
    CheckEqual(jwt.reg[jrcIssuer], 'joe');
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
    Finalize(jwt);
    CheckEqual(jwt.reg[jrcIssuer], '');
    check(ParseJwt(t, jwt) = jwtValid);
    check(jwt.data.B['http://example.com/is_root']);
    check((jwt.reg[jrcIssuedAt] <> '') = (jrcIssuedAt in one.Claims));
    check(jwt.result = jwtValid);
    CheckEqual(jwt.reg[jrcIssuer], 'joe');
    checkEqual(one.ExtractAlgo(t), one.Algorithm);
    checkEqual(one.ExtractAlgo(copy(t, 2, 1000)), '');
    checkEqual(one.CacheTimeoutSeconds, 0);
    one.Options := one.Options + [joHeaderParse];
    one.Verify(t, jwt);
    CheckUtf8(jwt.result = jwtValid, 'verify1=% %', [ToText(jwt.result)^, one]);
    checkEqual(jwt.reg[jrcIssuer], 'joe');
    one.Options := one.Options - [joHeaderParse];
    one.CacheTimeoutSeconds := 60;
    checkEqual(one.CacheTimeoutSeconds, 60);
    one.Verify(t, jwt);
    checkEqual(exp, GetCardinal(pointer(jwt.reg[jrcExpirationTime])));
    CheckUtf8(jwt.result = jwtValid, 'verify2=% %', [ToText(jwt.result)^, one]);
    check(jwt.reg[jrcExpirationTime] <> '');
    checkEqual(jwt.reg[jrcIssuer], 'joe');
    check(jwt.data.B['http://example.com/is_root']);
    check((jwt.reg[jrcIssuedAt] <> '') = (jrcIssuedAt in one.Claims));
    check((jwt.reg[jrcJWTID] <> '') = (jrcJWTID in one.Claims));
    if jwt.result = jwtValid then
      for i := 1 to 1000 do
      begin
        Finalize(jwt);
        FillCharFast(jwt, SizeOf(jwt), 0);
        checkEqual(jwt.reg[jrcIssuer], '');
        one.Verify(t, jwt);
        check(jwt.result = jwtValid, 'from cache');
        checkEqual(jwt.reg[jrcIssuer], 'joe');
        check((jwt.reg[jrcJWTID] <> '') = (jrcJWTID in one.Claims));
      end;
    if (one.Algorithm <> 'none') and
       (t[length(t)] in ['1'..'9', 'B'..'Z', 'b'..'z']) then
    begin
      dec(t[length(t)]); // invalidate signature
      one.Verify(t, jwt);
      check(jwt.result <> jwtValid, 'invalid sig');
    end;
    t := one.Compute([], 'john', '', '["one","two"]');
    check(t <> '');
    check(ParseJwt(t, jwt) = jwtValid);
    CheckEqual(jwt.reg[jrcIssuer], 'john');
    if jrcAudience in one.Claims then
      CheckEqual(jwt.reg[jrcAudience], '["one","two"]')
    else
      CheckEqual(jwt.reg[jrcAudience], '');
    if not nofree then
      one.Free;
  end;

  procedure Benchmark(J: TJwtAbstract; ctx: PUtf8Char; N: integer = 1000);
  var
    i: integer;
    tok: RawUtf8;
    jwt: TJwtContent;
    tim: TPrecisionTimer;
  begin
    J.CacheTimeoutSeconds := 0;
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
      inc(tok[length(tok) - 5]); // make signature either wrong or not base64uri
      jwt.result := jwtNoToken;
      J.Verify(tok, jwt);
      check(jwt.result in [jwtInvalidSignature, jwtWrongFormat], 'detection');
      NotifyTestSpeed('% %', [ctx, J.Algorithm], N, 0, @tim);
    finally
      J.Free;
    end;
  end;

const
  JWT_RSA: array[0..5] of TJwtRsaClass = (
    TJwtRs256,
    TJwtRs384,
    TJwtRs512,
    TJwtPs256,
    TJwtPs384,
    TJwtPs512);

{$ifdef USE_OPENSSL}
  OSSL_JWT: array[0..10] of TJwtAbstractOslClass = (
    TJwtEs256Osl,
    TJwtEs384Osl,
    TJwtEs512Osl,
    TJwtEs256KOsl,
    TJwtRs256Osl,
    TJwtRs384Osl,
    TJwtRs512Osl,
    TJwtPs256Osl,
    TJwtPs384Osl,
    TJwtPs512Osl,
    TJwtEddsaOsl);
{$endif USE_OPENSSL}

var
  i: integer;
  j: TJwtAbstract;
  jwt: TJwtContent;
  secret: TEccCertificateSecret;
  tok, priv, pub: RawUtf8;
  a: TSignAlgo;
  caa: TCryptAsymAlgo;
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
  j := TJwtCrypt.Create(caaES256, '-----BEGIN PUBLIC KEY-----'#13#10 +
    'MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEoIQ8m1iBHYoxrdLT1A6MH9naG+hk'#13#10 +
    '/ccw/Ij0p9Mk7JmNdzCUeEjzlU5/E683I9PZaz2/5RFj1HfKPTgDkxQFkA=='#13#10 +
    '-----END PUBLIC KEY-----'#13#10, [jrcAudience], ['aud'], 60);
  try
    CheckEqual(j.Algorithm, 'ES256');
  finally
    j.Free;
  end;
  j := TJwtCrypt.Create(caaES256, '-----BEGIN EC PRIVATE KEY-----'#13#10 +
    'MHcCAQEEIP00000000000000000000000000000000000000000roAoGCCqGSM49'#13#10 +
    'AwEHoUQDQgAEoIQ8m1iBHYoxrdLT1A6MH9naG+hk/ccw/Ij0p9Mk7JmNdzCUeEjz'#13#10 +
    'lU5/E683I9PZaz2/5RFj1HfKPTgDkxQFkA=='#13#10 +
    '-----END EC PRIVATE KEY-----'#13#10, [jrcAudience], ['aud'], 60);
  try
    CheckEqual(j.Algorithm, 'ES256');
  finally
    j.Free;
  end;
  for i := 1 to 10 do
  begin
    secret := TEccCertificateSecret.CreateNew(nil); // self-signed certificate
    test(TJwtEs256.Create(secret,
      [jrcIssuer, jrcExpirationTime], [], 60));
    test(TJwtEs256.Create(secret,
      [jrcIssuer, jrcExpirationTime, jrcIssuedAt], [], 60));
    test(TJwtEs256.Create(secret,
      [jrcIssuer, jrcExpirationTime, jrcIssuedAt, jrcJWTID], [], 60));
    secret.Free;
  end;
  for a := low(a) to high(a) do
    Benchmark(JWT_CLASS[a].Create(
      'secret', 0, [jrcIssuer, jrcExpirationTime], []), 'mORMot');
  secret := TEccCertificateSecret.CreateNew(nil);
  try
    Benchmark(TJwtEs256.Create(
      secret, [jrcIssuer, jrcExpirationTime], [], 60), 'mORMot', 100);
  finally
    secret.Free;
  end;
  for i := 0 to high(JWT_RSA) do
  begin
    j := JWT_RSA[i].Create(_rsapriv, [jrcIssuer, jrcExpirationTime], [], 60);
    test(j, {nofree=}true);
    Benchmark(j, 'mORMot', 100);
  end;
  for caa := low(caa) to high(caa) do
    if TJwtCrypt.Supports(caa) then
    begin
      // RSA is very slow at key computing and signing, but fast to verify
      if caa in CAA_RSA then
      begin
        priv := _rsapriv; // pre-computed RSA key pair
        pub  := _rsapub;
      end
      else
        pub := ''; // ECC algorithms are fast enough to generate a new key
      j := TJwtCrypt.Create(caa, pub, [jrcIssuer, jrcExpirationTime], [], 60);
      if pub <> '' then
        Check(TJwtCrypt(j).LoadPrivateKey(priv));
      test(j, {nofree=}true);
      Benchmark(j, 'TJwtCrypt', 100);
    end;
  {$ifdef USE_OPENSSL}
  for i := 0 to high(OSSL_JWT) do
    if OSSL_JWT[i].IsAvailable then
    begin
      if OSSL_JWT[i].GetAsymAlgo in CAA_RSA then
      begin
        priv := _rsapriv; // pre-computed RSA key pair
        pub  := _rsapub;
      end
      else
        OSSL_JWT[i].GenerateKeys(priv, pub);
      Benchmark(OSSL_JWT[i].Create(priv, pub, '', '',
        [jrcIssuer, jrcExpirationTime], [], 60), 'OpenSSL', 100);
    end;
  {$endif USE_OPENSSL}
end;

type
  TBenchmark = (
    // non cryptographic hashes
    bCRC32c, bXXHash32, bCRC32, bAdler32, bHash32, bAesniHash,
    // cryptographic hashes
    bMD4, bMD5,
    bSHA1, bHMACSHA1, bSHA256, bHMACSHA256,
    bSHA384, bHMACSHA384, bSHA512, bSHA512_256, bHMACSHA512,
    bSHA3_256, bSHA3_512,
    {$ifdef USE_OPENSSL}
    bSHA1O, bHMACSHA1O, bSHA256O, bHMACSHA256O,
    bSHA384O, bHMACSHA384O, bSHA512O, bSHA512_256O, bHMACSHA512O,
    bSHA3_256O, bSHA3_512O,
    {$endif USE_OPENSSL}
    // encryption
    bRC4,
    bAES128CBC, bAES128CFB, bAES128OFB, bAES128C64, bAES128CTR,
    bAES128CFC, bAES128OFC, bAES128CTC, bAES128GCM,
    bAES256CBC, bAES256CFB, bAES256OFB, bAES256C64, bAES256CTR,
    bAES256CFC, bAES256OFC, bAES256CTC, bAES256GCM,
    {$ifdef USE_OPENSSL}
    bAES128CBCO, bAES128CFBO, bAES128OFBO, bAES128CTRO, bAES128GCMO,
    bAES256CBCO, bAES256CFBO, bAES256OFBO, bAES256CTRO, bAES256GCMO,
    {$endif USE_OPENSSL}
    bSHAKE128, bSHAKE256, bBlowFish);

procedure TTestCoreCrypto.Benchmark;
const
  bAESFIRST = bAES128CBC;
  bAESLAST = {$ifdef USE_OPENSSL} bAES256GCMO {$else} bAES256GCM {$endif};
  bOPENSSL = [ {$ifdef USE_OPENSSL}
               bSHA1O .. bSHA3_512O, bAES128CBCO .. bAES256GCMO
               {$endif USE_OPENSSL} ];
  COUNT = 500;
  SIZ: array[0..4] of integer = (
    8,
    50,
    100,
    1000,
    10000);

  AESCLASS: array[bAESFIRST.. bAESLAST] of TAesAbstractClass = (
    TAesCbc, TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesCfc, TAesOfc, TAesCtc, TAesGcm,
    TAesCbc, TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesCfc, TAesOfc, TAesCtc, TAesGcm
  {$ifdef USE_OPENSSL} ,
    TAesCbcOsl, TAesCfbOsl, TAesOfbOsl, TAesCtrOsl, TAesGcmOsl,
    TAesCbcOsl, TAesCfbOsl, TAesOfbOsl, TAesCtrOsl, TAesGcmOsl
  {$endif USE_OPENSSL});

  AESBITS: array[bAESFIRST..bAESLAST] of integer = (
    128, 128, 128, 128, 128, 128, 128, 128, 128,
    256, 256, 256, 256, 256, 256, 256, 256, 256
  {$ifdef USE_OPENSSL} ,
    128, 128, 128, 128, 128, 256, 256, 256, 256, 256);

  OPENSSL_HASH: array[bSHA1O .. bSHA3_512O] of THashAlgo = (
    hfSHA1, hfSHA1, hfSHA256, hfSHA256, hfSHA384, hfSHA384, hfSHA512, hfSHA384,
    hfSHA512, hfSHA3_256, hfSHA3_512
  {$endif USE_OPENSSL});
var
  b: TBenchmark;
  s, i, size, n: integer;
  data, encrypted, s1, s2: RawByteString;
  dig: THash512Rec;
  MD: TMd5;
  SHA1: TSha1;
  SHA256: TSha256;
  SHA384: TSha384;
  SHA512: TSha512;
  SHA512_256: TSha512_256;
  SHA3, SHAKE128, SHAKE256: TSha3;
  bf: TBlowFishCtr;
  RC4: TRC4;
  timer: TPrecisionTimer;
  time: array[TBenchmark] of Int64;
  AES: array[bAESFIRST..bAESLAST] of TAesAbstract;
  TXT: array[TBenchmark] of RawUtf8;
begin
  GetEnumTrimmedNames(TypeInfo(TBenchmark), @TXT, false, {lower=}true);
  for b := low(AES) to high(AES) do
    if AESCLASS[b].IsAvailable then
    begin
      AES[b] := AESCLASS[b].Create(dig{%H-}, AESBITS[b]);
      ShortStringToAnsi7String(AES[b].AlgoName, TXT[b]);
    end
    else
      AES[b] := nil;
  {$ifdef USE_OPENSSL}
  if OpenSslIsAvailable then 
    for b := low(b) to high(b) do
      if b in bOPENSSL then
      begin
        if b < low(AES) then
          SetLength(TXT[b], length(TXT[b]) - 1);
        TXT[b] := 'openssl ' + TXT[b];
      end
      else
        TXT[b] := 'mormot ' + TXT[b];
  {$endif USE_OPENSSL}
  SHAKE128.InitCypher('secret', SHAKE_128);
  SHAKE256.InitCypher('secret', SHAKE_256);
  bf := TBlowFishCtr.Create('secret');
  RC4.InitSha3(dig, SizeOf(dig));
  FillCharFast(time, SizeOf(time), 0);
  size := 0;
  n := 0;
  for s := 0 to high(SIZ) do
  begin
    data := RandomWinAnsi(SIZ[s]);
    CheckEqual(length(data), SIZ[s]);
    SetLength(encrypted, SIZ[s]);
    for b := low(b) to high(b) do
    if (b < low(AES)) or
       (b > high(AES)) or
       (AES[b] <> nil) then
    begin
      if (b = bAesniHash) and
         not Assigned(AesNiHash32) then
        continue;
      if (b in bOPENSSL) and
         not OpenSslIsAvailable then
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
          bMD4:
            MD4Buf(pointer(data)^, SIZ[s], dig.h0);
          bMD5:
            MD.Full(pointer(data), SIZ[s], dig.h0);
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
          bSHA512_256:
            SHA512_256.Full(pointer(data), SIZ[s], dig.Lo);
          bHMACSHA512:
            HmacSha512('secret', data, dig.b);
          bSHA3_256:
            SHA3.Full(pointer(data), SIZ[s], dig.Lo);
          bSHA3_512:
            SHA3.Full(pointer(data), SIZ[s], dig.b);
          bRC4:
            RC4.EncryptBuffer(pointer(data), pointer(encrypted), SIZ[s]);
          {$ifdef USE_OPENSSL}
          bSHA1O,
          bSHA256O,
          bSHA384O,
          bSHA512O,
          bSHA512_256O,
          bHMACSHA512O,
          bSHA3_256O,
          bSHA3_512O:
            TOpenSslHash.Hash(OPENSSL_HASH[b], data);
          bHMACSHA1O,
          bHMACSHA256O,
          bHMACSHA384O:
            TOpenSslHmac.Hmac(OPENSSL_HASH[b], data, 'secret');
          bAES128CBCO,
          bAES128CFBO,
          bAES128OFBO,
          bAES128CTRO,
          bAES256CBCO,
          bAES256CFBO,
          bAES256OFBO,
          bAES256CTRO,
          {$endif USE_OPENSSL}
          bAES128CBC,
          bAES128CFB,
          bAES128OFB,
          bAES128C64,
          bAES128CTR,
          bAES256CBC,
          bAES256CFB,
          bAES256OFB,
          bAES256C64,
          bAES256CTR:
            AES[b].EncryptPkcs7(data, {encrypt=}true);
          {$ifdef USE_OPENSSL}
          bAES128GCMO,
          bAES256GCMO,
          {$endif USE_OPENSSL}
          bAES128CFC,
          bAES128OFC,
          bAES128CTC,
          bAES128GCM,
          bAES256CFC,
          bAES256OFC,
          bAES256CTC,
          bAES256GCM:
            AES[b].MacAndCrypt(data, {encrypt=}true, {ivatbeg=}true);
          bSHAKE128:
            SHAKE128.Cypher(pointer(data), pointer(encrypted), SIZ[s]);
          bSHAKE256:
            SHAKE256.Cypher(pointer(data), pointer(encrypted), SIZ[s]);
          bBlowFish:
            bf.EncryptBuffer(pointer(data), pointer(encrypted), SIZ[s]);
        else
          ESynCrypto.RaiseUtf8('Unexpected %', [TXT[b]]);
        end;
        Check((b >= bRC4) or
              (b in bOPENSSL) or
              (dig.d0 <> 0) or
              (dig.d1 <> 0));
      end;
      inc(time[b], NotifyTestSpeed('% %', [TXT[b], SIZ[s]], COUNT, SIZ[s] *
        COUNT, @timer, {onlylog=}true{(b in bOPENSSL) or (PosEx('gcm', TXT[b]) = 0)}));
      //if b in [bSHA3_512,high(b)] then AddConsole('');
    end;
    inc(size, SIZ[s] * COUNT);
    inc(n, COUNT);
    // we may add some small additionnal tests here (outside timers)
    CheckEqual(StrLen(pointer(data)), SIZ[s], 'datastrlen');
    bf.IV := 0;
    s1 := bf.Encrypt(data, {ivatbeg=}true);
    CheckEqual(length(s1), SIZ[s] + 8);
    CheckEqual(bf.IV, 0);
    s2 := bf.Decrypt(s1, {ivatbeg=}true);
    CheckEqual(length(s2), SIZ[s]);
    CheckEqual(s2, data);
    CheckEqual(bf.IV, 0);
  end;
  for b := low(b) to high(b) do
    if time[b] <> 0 then
      AddConsole(FormatString('% % in % i.e. %/s or %/s', [n, TXT[b],
        MicroSecToString(time[b]), K((Int64(n) * 1000000) div time[b]),
        KB((Int64(size) * 1000000) div time[b])]));
  for b := low(AES) to high(AES) do
    AES[b].Free;
  bf.Free;
end;

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

procedure TTestCoreCrypto.CrcSlow(Context: TObject);

const
  HASHESMAX = 512;
  HASHALIGN = 4; // you may try with paranoid 32 here

  procedure Hash32Test(buf: PAnsiChar; hash: THasher; var expected: cardinal);
  var
    L, modif: PtrInt;
    c, c2: cardinal;
  begin
    if expected = 0 then
      expected := hash(0, buf, HASHESMAX) // use first call as aligned reference
    else
      CheckEqual(hash(0, buf, HASHESMAX), expected, 'alignement problem');
    for L := 0 to HASHESMAX do
    begin
      c := hash(0, buf, L);
      for modif := 0 to L - 1 do
      begin
        inc(buf[modif]); // should detect one modified bit at any position
        c2 := hash(0, buf, L);
        dec(buf[modif]); // reset
        CheckUtf8(c <> c2, 'L=% modif=%', [L, modif]);
      end;
      CheckEqual(hash(0, buf, L), c, 'after reset');
    end;
  end;

  procedure Hash64Test(buf: PAnsiChar; hash: THasher64; var expected: QWord);
  var
    L, modif: PtrInt;
    c, c2: QWord;
  begin
    if expected = 0 then
      expected := hash(0, buf, HASHESMAX) // use first call as aligned reference
    else
      CheckEqual(hash(0, buf, HASHESMAX), expected, 'alignement problem');
    for L := 0 to HASHESMAX do
    begin
      c := hash(0, buf, L);
      for modif := 0 to L - 1 do
      begin
        inc(buf[modif]); // should detect one modified bit at any position
        c2 := hash(0, buf, L);
        dec(buf[modif]);
        CheckUtf8(c <> c2, 'L=% modif=%', [L, modif]);
      end;
      CheckEqual(hash(0, buf, L), c, 'after reset');
    end;
  end;

  procedure Hash128Test(buf: PAnsiChar; hash: THasher128);
  var
    L, modif: PtrInt;
    c, c2: THash128Rec;
  begin
    for L := 0 to HASHESMAX do
    begin
      FillZero(c.b);
      hash(@c, buf, L);
      if L > 16 then
      begin
        Check(c.c0 <> c.c1, 'c0c1'); // no 32-bit overlap is expected
        Check(c.c0 <> c.c2, 'c0c2');
        Check(c.c0 <> c.c3, 'c0c3');
        Check(c.c1 <> c.c2, 'c1c2');
        Check(c.c1 <> c.c3, 'c1c3');
        Check(c.c2 <> c.c3, 'c2c3');
      end;
      for modif := 0 to L - 1 do
      begin
        FillZero(c2.b);
        inc(buf[modif]); // should detect one modified bit at any position
        hash(@c2, buf, L);
        dec(buf[modif]);
        CheckUtf8(not IsEqual(c.b, c2.b), 'L=% modif=%', [L, modif]);
      end;
      FillZero(c2.b);
      hash(@c2, buf, L); // should return the same value for the same data
      CheckUtf8(IsEqual(c.b, c2.b), 'after reset');
    end;
  end;

var
  buf: RawByteString;
  u, pw, nfo, exp: RawUtf8;
  iv: Int64;
  P: PAnsiChar;
  unalign: PtrInt;
  n, rnd: integer;
  i64: Int64;
  logN, blocksize, parallel, r,
  exp321, exp322, exp323, exp324, exp325, exp326: cardinal;
  exp641, exp642: QWord;
  hasher: TSynHasher;
  h, h2: THashAlgo;
  s, s2: TSignAlgo;
  mcf, mcf2: TModularCryptFormat;
  timer: TPrecisionTimer;
begin
  // validate THashAlgo and TSignAlgo recognition
  for h := low(h) to high(h) do
  begin
    u := ToUtf8(h);
    Check(u <> '');
    Check(TextToHashAlgo(u, h2));
    Check(h = h2);
    Check(TextToHashAlgo(' ' + u, h2));
    Check(h = h2);
    Check(TextToHashAlgo(' ' + u + ' ', h2));
    Check(h = h2);
    Check(TextToHashAlgo(HASH_EXT[h], h2));
    Check(h = h2);
    u := ShortStringToUtf8(ToText(h)^);
    Check(u <> '');
    Check(TextToHashAlgo(u, h2));
    Check(h = h2);
  end;
  Check(TextToHashAlgo('SHA-512/256', h2));
  Check(h2 = hfSHA512_256);
  Check(TextToHashAlgo('SHA-3/256', h2));
  Check(h2 = hfSHA3_256);
  Check(TextToHashAlgo('SHA-3/512', h2));
  Check(h2 = hfSHA3_512);
  Check(not TextToHashAlgo('SHA5122', h));
  Check(not TextToHashAlgo('SHA512256', h));
  for s := low(s) to high(s) do
  begin
    u := ToUtf8(s);
    Check(u <> '');
    Check(TextToSignAlgo(u, s2));
    Check(s = s2);
    Check(TextToSignAlgo(' ' + u, s2));
    Check(s = s2);
    Check(TextToSignAlgo(' ' + u + ' ', s2));
    Check(s = s2);
    u := ShortStringToUtf8(ToText(s)^);
    Check(u <> '');
    Check(TextToSignAlgo(u, s2));
    Check(s = s2);
  end;
  Check(TextToSignAlgo('SHA-512', s2));
  Check(s2 = saSHA512);
  Check(TextToSignAlgo('SHA-3/256', s2));
  Check(s2 = saSHA3256);
  Check(TextToSignAlgo('SHA3-512', s2));
  Check(s2 = saSHA3512);
  Check(TextToSignAlgo('SHAKE-128', s2));
  Check(s2 = saSha3S128);
  Check(TextToSignAlgo('SHAKE/256', s2));
  Check(s2 = saSha3S256);
  Check(not TextToSignAlgo('SHA5122', s));
  Check(not TextToSignAlgo('SHA512256', s));
  // validate our 32-bit, 64-bit and 128-bit hash functions
  SetLength(buf, HASHESMAX + HASHALIGN);
  exp321 := 0;
  exp322 := 0;
  exp323 := 0;
  exp324 := 0;
  exp325 := 0;
  exp326 := 0;
  exp641 := 0;
  exp642 := 0;
  for unalign := 0 to HASHALIGN - 1 do // ensure alignment doesn't change result
  begin
    P := pointer(buf);
    inc(P, unalign);
    FillIncreasing(pointer(P), $12345670, HASHESMAX shr 2);
    Hash32Test(P, @crc32cfast,    exp321);
    Hash32Test(P, @crc32c,        exp322);
    Hash32Test(P, @xxHash32,      exp323);
    if Assigned(AesNiHash32) then
      Hash32Test(P, @AesNiHash32, exp324);
    Hash32Test(P, @crc32fast,     exp325);
    Hash32Test(P, @adler32,       exp326);
    Hash64Test(P, @crc32cTwice, exp641);
    if Assigned(AesNiHash64) then
      Hash64Test(P, @AesNiHash64, exp642);
    Hash128Test(P, @crc32c128);
    if Assigned(AesNiHash128) then
      Hash128Test(P, @AesNiHash128);
  end;
  CheckEqual(exp321, 4022360595);
  CheckEqual(exp321, exp322);
  CheckEqual(exp323, 1465265692);
  CheckEqual(exp325, 3408302637);
  CheckEqual(exp326, 4027950528);
  CheckEqual(adler32fast(0, P, HASHESMAX), exp326);
  CheckEqual(exp641, -1170836861443089901);
  // verify "Modular Crypt" hashing functions
  u := '$5$rounds=12345$q3hvJE5mn5jKRsW.$BbbYTFiaImz9rTy03GGi.Jf9YY5bmxN0LU3p3uI1iUB';
  Check(ModularCryptIdentify(u) = mcfSha256Crypt);
  Check(ModularCryptVerify('password', u) = mcfSha256Crypt);
  Check(ModularCryptVerify('p4ssword', u) = mcfInvalid);
  Check(ModularCryptVerify('password', u) = mcfSha256Crypt);
  delete(u, 5, 1);
  Check(ModularCryptIdentify(u) = mcfInvalid);
  Check(ModularCryptVerify('password', u) = mcfInvalid);
  delete(u, 2, 1);
  Check(ModularCryptIdentify(u) = mcfInvalid);
  u := '$1$3azHgidD$SrJPt7B.9rekpmwJwtON31';
  Check(ModularCryptIdentify(u) = mcfMd5Crypt);
  Check(ModularCryptVerify('password', u) = mcfMd5Crypt);
  Check(ModularCryptVerify('secret', u) = mcfInvalid);
  Check(ModularCryptVerify('the minimum number is still observed',
    '$5$rounds=10$roundstoolow$yfvwcWrQ8l/K0DAWyuPMDNHpIVlTQebY9l/gL972bIC') =
     mcfSha256Crypt);
  u := '$6$rounds=1400$anotherlongsalts$POfYwTEok97VWcjxIiSOjiykti.o/pQs.wP' +
       'vMxQ6Fm7I6IoYN3CmLs66x9t0oSwbtEW7o7UmJEiDwGqd8p4ur1';
  Check(ModularCryptVerify('a very much longer text to encrypt.  ' +
    'This one even stretches over morethan one line.', u) = mcfSha512Crypt);
  Check(ModularCryptVerify('a very much longer text to encrypt.  ' +
    'This one even stretches over more than one line.', u) = mcfInvalid);
  Check(ModularCryptIdentify(u) = mcfSha512Crypt);
  delete(u, 5, 1);
  Check(ModularCryptIdentify(u) = mcfInvalid);
  // official test vectors from test_handlers_pbkdf2.py
  u := '$pbkdf2$1212$OB.dtnSEXZK8U5cgxU/GYQ$y5LKPOplRmok7CZp/aqVDVg8zGI';
  Check(ModularCryptIdentify(u) = mcfPbkdf2Sha1);
  Check(ModularCryptVerify('password', u) = mcfPbkdf2Sha1);
  Check(ModularCryptVerify('p4ssword', u) = mcfInvalid);
  u := '$pbkdf2-sha256$1212$4vjV83LKPjQzk31VI4E0Vw$hsYF68OiOUPdDZ1Fg.fJPeq1h/gXXY7acBp9/6c.tmQ';
  Check(ModularCryptIdentify(u) = mcfPbkdf2Sha256);
  Check(ModularCryptVerify('password', u) = mcfPbkdf2Sha256);
  Check(ModularCryptVerify('p4ssword', u) = mcfInvalid);
  u := '$pbkdf2-sha256$6400$.6UI/S.nXIk8jcbdHx3Fhg$98jZicV16ODfEsEZeYPGHU3kbrUrvUEXOPimVSQDD44';
  Check(ModularCryptIdentify(u) = mcfPbkdf2Sha256);
  Check(ModularCryptVerify('password', u) = mcfPbkdf2Sha256);
  Check(ModularCryptVerify('p4ssword', u) = mcfInvalid);
  u := '$pbkdf2-sha512$1212$RHY0Fr3IDMSVO/RSZyb5ow$eNLfBK.eVozomMr.1gYa1' +
       '7k9B7KIK25NOEshvhrSX.esqY3s.FvWZViXz4KoLlQI.BzY/YTNJOiKc5gBYFYGww';
  Check(ModularCryptIdentify(u) = mcfPbkdf2Sha512);
  Check(ModularCryptVerify('password', u) = mcfPbkdf2Sha512);
  Check(ModularCryptVerify('p4ssword', u) = mcfInvalid);
  u := '$pbkdf2-sha3$1000$G85lPNdJLXoDVzhbCmsBCA$T6UjUUihUTmnYpwiRbhH8yi' +
       'BjOLTRzARcwK5gr7OEX.fRj9HD/ME7NivCFzgQ5W7BbBaAyoHKeirdX7cDPF59A';
  u := ModularCryptHash(mcfPbkdf2Sha3, 'password', 1000); // our own format
  Check(ModularCryptIdentify(u) = mcfPbkdf2Sha3);
  Check(ModularCryptVerify('password', u) = mcfPbkdf2Sha3);
  Check(ModularCryptVerify('p4ssword', u) = mcfInvalid);
  // BCrypt reference material
  for n := -100 to 100 do
  begin
    i64 := n;
    iv := BSwap64(i64);
    BlowFishCtrInc(@iv);
    CheckEqual(iv, BSwap64(i64 + 1), 'bfctr0');
    inc(i64, Int64(1) shl 32);
    iv := BSwap64(i64);
    BlowFishCtrInc(@iv);
    CheckEqual(iv, BSwap64(i64 + 1), 'bfctr1');
  end;
  i64 := cardinal(-1);
  iv := BSwap64(i64);
  BlowFishCtrInc(@iv);
  CheckEqual(iv, BSwap64(i64 + 1), 'bfctr2');
  Check(ModularCryptVerify('',
    '$2b$06$DCq7YPn5Rq63x1Lad4cll.TV4S6ytwfsfvkgY8jIucDrjc8deX1s.') = mcfBCrypt);
  Check(ModularCryptVerify('a',
    '$2a$06$m0CrhHm10qJ3lXRY.5zDGO3rS2KdeeWLuGmsfGlMfOxih58VYVfxe') = mcfBCrypt);
  Check(ModularCryptVerify('a',
    '$2y$06$m0CrhHm10qJ3lXRY.5zDGO3rS2KdeeWLuGmsfGlMfOxih58VYVfxe') = mcfBCrypt);
  Check(ModularCryptVerify('<.S.2K(Zq''',
    '$2b$04$VYAclAMpaXY/oqAo9yUpkuWmoYywaPzyhu56HxXpVltnBIfmO9tgu') = mcfBCrypt);
  Check(ModularCryptVerify('5.rApO%5jA',
    '$2a$05$kVNDrnYKvbNr5AIcxNzeIuRcyIF5cZk6UrwHGxENbxP5dVv.WQM/G') = mcfBCrypt);
  Check(ModularCryptVerify('oW++kSrQW^',
    '$2b$06$QLKkRMH9Am6irtPeSKN5sObJGr3j47cO6Pdf5JZ0AsJXuze0IbsNm') = mcfBCrypt);
  Check(ModularCryptVerify('ggJ\KbTnDG',
    '$2b$07$4H896R09bzjhapgCPS/LYuMzAQluVgR5iu/ALF8L8Aln6lzzYXwbq') = mcfBCrypt);
  Check(ModularCryptVerify('49b0:;VkH/',
    '$2b$08$hfvO2retKrSrx5f2RXikWeFWdtSesPlbj08t/uXxCeZoHRWDz/xFe') = mcfBCrypt);
  Check(ModularCryptVerify('>9N^5jc##''',
    '$2b$09$XZLvl7rMB3EvM0c1.JHivuIDPJWeNJPTVrpjZIEVRYYB/mF6cYgJK') = mcfBCrypt);
  Check(ModularCryptVerify('\$ch)s4WXp',
    '$2b$10$aIjpMOLK5qiS9zjhcHR5TOU7v2NFDmcsBmSFDt5EHOgp/jeTF3O/q') = mcfBCrypt);
  Check(ModularCryptVerify('RYoj\_>2P7',
    '$2b$12$esIAHiQAJNNBrsr5V13l7.RFWWJI2BZFtQlkFyiWXjou05GyuREZa') = mcfBCrypt);
  Check(ModularCryptVerify('password',
    '$2b$12$GhvMmNVjRW29ulnudl.LbuAnUtN/LRfe1JsBm1Xu6LE3059z5Tr8m') = mcfBCrypt);
  Check(ModularCryptVerify('a',
    '$2b$04$5DCebwootqWMCp59ISrMJ.l4WvgHIVg17ZawDIrDM2IjlE64GDNQS') = mcfBCrypt);
  Check(ModularCryptVerify('aa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.AyUxBk.ThHlsLvRTH7IqcG7yVHJ3SXq') = mcfBCrypt);
  Check(ModularCryptVerify('aaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.BxOVac5xPB6XFdRc/ZrzM9FgZkqmvbW') = mcfBCrypt);
  Check(ModularCryptVerify('aaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.Qbr209bpCtfl5hN7UQlG/L4xiD3AKau') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.oWszihPjDZI0ypReKsaDOW1jBl7oOii') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ./k.Xxn9YiqtV/sxh3EHbnOHd0Qsq27K') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.PYJqRFQbgRbIjMd5VNKmdKS4sBVOyDe') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ..VMYfzaw1wP/SGxowpLeGf13fxCCt.q') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.5B0p054nO5WgAD1n04XslDY/bqY9RJi') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.INBTgqm7sdlBJDg.J5mLMSRK25ri04y') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.s3y7CdFD0OR5p6rsZw/eZ.Dla40KLfm') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.Jx742Djra6Q7PqJWnTAS.85c28g.Siq') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.oKMXW3EZcPHcUV0ib5vDBnh9HojXnLu') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.w6nIjWpDPNSH5pZUvLjC1q25ONEQpeS') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.k1b2/r9A/hxdwKEKurg6OCn4MwMdiGq') = mcfBCrypt);
  Check(ModularCryptVerify('aaaaaaaaaaaaaaaa',
    '$2b$04$5DCebwootqWMCp59ISrMJ.3prCNHVX1Ws.7Hm2bJxFUnQOX9f7DFa') = mcfBCrypt);
  u := RawUtf8OfChar('a', 260);
  exp := '$2b$04$QqpSfI8JYX8HSxNwW5yx8Ohp12sNboonE6e5jfnGZ0fD4ZZwQkOOK';
  Check(ModularCryptVerify(u, exp) = mcfBCrypt);
  Check(ModularCryptVerify(u, exp, [mcfMd5Crypt, mcfBCrypt]) = mcfBCrypt);
  Check(ModularCryptVerify(u, exp, [mcfMd5Crypt, mcfSha512Crypt]) = mcfUnknown);
  Check(ModularCryptVerify(u, exp, [], {maxrounds=}3) = mcfInvalid);
  u[200] := 'b'; // BCrypt truncates the password at 72 bytes long
  Check(ModularCryptVerify(u, exp) = mcfBCrypt, 'truncate72');
  u[10] := 'b';
  Check(ModularCryptVerify(u, exp) = mcfInvalid, 'passwd');
  exp := '$bcrypt-sha256$v=2,t=2b,r=12$n79VH.0Q2TMWmt3Oqt9uku$Kq4Noyk3094Y2QlB8NdRT8SvGiI4ft2';
  Check(ModularCryptVerify('password', exp) = mcfBCryptSha256);
  Check(ModularCryptVerify('pAssword', exp) = mcfInvalid);
  // pure pascal and OpenSSL SCrypt implementation
  {$ifdef USE_OPENSSL}
  if OpenSslIsAvailable then
    if OpenSslVersion >= OPENSSL3_VERNUM then
      TestSCript(@OpenSslSCrypt, 'OpenSslSCrypt');
  {$endif USE_OPENSSL}
  TestSCript(@RawSCrypt, 'RawSCrypt');
  r := SCryptRounds; // default values
  SCryptRoundsDecode(r, logN, blocksize, parallel);
  Check(r = $8000e001);
  CheckEqual(logN, 16);
  CheckEqual(blocksize, 8);
  CheckEqual(parallel, 2);
  exp := '$scrypt$ln=4,r=8,p=1$QNx4N454ppMeKmDjxyrhsh7Q/PYBQw$zeGG+tsAueRzkvXfE1/F58KOKFEFfI0KpBYwE/3ZUWg';
  Check(ModularCryptVerify('password', exp) = mcfSCrypt);
  Check(ModularCryptVerify('pAssword', exp) = mcfInvalid);
  exp := '$scrypt$ln=8,r=8,p=1$WKs1xljLudd6z9kbY0wpJQ$yCR4iDZYDKv+iEJj6yHY0lv/epnfB6f/w1EbXrsJOuQ';
  Check(ModularCryptVerify('password', exp) = mcfSCrypt);
  Check(ModularCryptVerify('pAssword', exp) = mcfInvalid);
  // validate "Modular Crypt" formats
  for mcf := mcfMd5Crypt to high(mcf) do
  begin
    for n := 1 to 10 do
    begin
      RandomByteString(n * 7, pw); // should reach at least 64 bytes = 512-bit
      case mcf of
        mcfMd5Crypt:
          rnd := 1000; // fixed number
        mcfBCrypt, mcfBCryptSha256:
          rnd := 4 + n shr 2; // cost = 4..5 is enough here
        mcfSCrypt:
          begin
            rnd := SCryptRounds(4 + (n shr 2), 8, n);
            SCryptRoundsDecode(rnd, logN, blocksize, parallel);
            CheckEqual(logN, 4 + (n shr 2));
            CheckEqual(blocksize, 8);
            CheckEqual(parallel, n);
          end;
      else
        rnd := 1000 + n;
      end;
      u := ModularCryptHash(mcf, pw, rnd, {saltsize=}n);
      Check(u <> '');
      case mcf of
        mcfSha256Crypt .. mcfSha512Crypt:
          CheckEqual(PosEx(Make(['$rounds=', rnd, '$']), u), 3);
        mcfBCrypt:
          CheckEqual(PosEx(Make(['$', UInt2DigitsToShort(rnd), '$']), u), 4);
      end;
      nfo := '';
      Check(ModularCryptIdentify(u, @nfo) = mcf);
      Check(ModularCryptVerify(pw, u) = mcf);
      Check(nfo <> '');
      Check(StartWithExact(u, nfo));
      Check(ModularCryptIdentify(nfo) = mcf);
      CheckEqual(u, ModularCryptHash(nfo, pw), 'simulate client side re-hash');
      if u <> '' then // avoid GPF
      begin
        dec(PByteArray(u)[length(u) - 5]);
        Check(ModularCryptVerify(pw, u) = mcfInvalid);
      end;
      u := ModularCryptFakeInfo(pw, mcf);
      nfo := '';
      mcf2 := ModularCryptIdentify(u, @nfo);
      if mcf = mcfMd5Crypt then
        CheckUtf8(mcf2 in mcfValid, u) // random format
      else
        Check(mcf2 = mcf);
      CheckEqual(nfo, u);
      CheckEqual(ModularCryptFakeInfo(pw, mcf), u, 'consistent fake');
    end;
  end;
  for mcf := mcfMd5Crypt to high(mcf) do
  begin
    timer.Start; // output password hash with default values
    u := ModularCryptHash(mcf, 'password');
    if not fOwner.MultiThread then
      NotifyProgress([TrimLeftLowerCaseShort(ToText(mcf)), '=', timer.Stop]);
    Check(ModularCryptIdentify(u, @nfo) = mcf);
    Check(nfo <> '');
    Check(StartWithExact(u, nfo));
    Check(ModularCryptIdentify(nfo) = mcf);
    //ConsoleWrite([CRLF,'ModularCryptHash(''',nfo,''',''password'')=''',u,''');']);
  end;
  // reference vectors from https://en.wikipedia.org/wiki/Mask_generation_function
  buf := 'foo';
  CheckEqualHex(hasher.Mgf1(hfSHA1, pointer(buf), length(buf), 3), '1ac907');
  CheckEqualHex(hasher.Mgf1(hfSHA1, pointer(buf), length(buf), 5), '1ac9075cd4');
  buf := 'bar';
  CheckEqualHex(hasher.Mgf1(hfSHA1, pointer(buf), length(buf), 5), 'bc0c655e01');
  CheckEqualHex(hasher.Mgf1(hfSHA1, pointer(buf), length(buf), 50),
    'bc0c655e016bc2931d85a2e675181adcef7f581f76df2739da74' +
    'faac41627be2f7f415c89e983fd0ce80ced9878641cb4876');
  CheckEqualHex(hasher.Mgf1(hfSHA256, pointer(buf), length(buf), 50),
    '382576a7841021cc28fc4c0948753fb8312090cea942ea4c4e73' +
    '5d10dc724b155f9f6069f289d61daca0cb814502ef04eae1');
  {$ifdef USE_OPENSSL}
  if OpenSslIsAvailable then
  begin
    CheckEqual(BigNumHexFromDecimal('0'), '');
    CheckEqual(BigNumHexFromDecimal('1'), '01');
    CheckEqual(BigNumHexFromDecimal('15'), '0f');
    CheckEqual(BigNumHexFromDecimal('255'), 'ff');
    CheckEqual(BigNumHexFromDecimal('65534'), 'fffe');
    CheckEqual(BigNumHexFromDecimal('65535'), 'ffff');
    CheckEqual(BigNumHexFromDecimal('12345678901234567890'), 'ab54a98ceb1f0ad2');
  end;
  {$endif USE_OPENSSL}
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

procedure TTestCoreCrypto.BaseEncoding;
const
  Value64: RawUtf8 = 'SGVsbG8gL2Mn6XRhaXQg5+Ar';
var
  tmp, tmp2: RawByteString;
  u, b64, b58, b32: RawUtf8;
  msg: string;
  Value: WinAnsiString;
  P: PUtf8Char;
  k: TPemKind;
  i, j, L, n: Integer;
  i64: Qword;
  enc, dec: TPrecisionTimer;
  c: byte;
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
  b64 := NetBinToBase64(tmp);
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
  CheckEqual(BinToBase32(''), '');
  CheckEqual(BinToBase32('f'), 'MY======');
  CheckEqual(BinToBase32('fo'), 'MZXQ====');
  CheckEqual(BinToBase32('foo'), 'MZXW6===');
  CheckEqual(BinToBase32('foob'), 'MZXW6YQ=');
  CheckEqual(BinToBase32('fooba'), 'MZXW6YTB');
  CheckEqual(BinToBase32('foobar'), 'MZXW6YTBOI======');
  CheckEqual(Base32ToBin(''), '');
  CheckEqual(Base32ToBin('MY======'), 'f');
  CheckEqual(Base32ToBin('MZXQ===='), 'fo');
  CheckEqual(Base32ToBin('MZXW6==='), 'foo');
  CheckEqual(Base32ToBin('MZXW6YQ='), 'foob');
  CheckEqual(Base32ToBin('MZXW6YTB'), 'fooba');
  CheckEqual(Base32ToBin('MZXW6YTBOI======'), 'foobar');
  CheckEqual(Base32ToBin('MZXW6YTB1'), '');
  CheckEqual(Base32ToBin('MZXW6YTB========'), '');
  CheckEqual(Base32ToBin('my======'), 'f');
  CheckEqual(Base32ToBin('mzxq===='), 'fo');
  CheckEqual(Base32ToBin('mzxw6==='), 'foo');
  CheckEqual(Base32ToBin('mzxw6yq='), 'foob');
  CheckEqual(Base32ToBin('mzxw6ytb'), 'fooba');
  CheckEqual(Base32ToBin('mzxw6ytboi======'), 'foobar');
  CheckEqual(Base32ToBin('mzxw6ytb1'), '');
  CheckEqual(Base32ToBin('mzxw6ytb========'), '');
  tmp := '';
  for i := 1 to 1982 do
  begin
    b64 := BinToBase64(tmp);
    Check((tmp = '') or IsBase64(b64));
    Check(Base64ToBin(b64) = tmp);
    CheckEqual(NetBinToBase64(tmp), b64);
    if tmp <> '' then
    begin
      L := length(b64);
      Check(not IsBase64(pointer(b64), L - 1));
      b64[Random32(L) + 1] := '&';
      Check(not IsBase64(pointer(b64), L));
    end;
    b64 := BinToBase64uri(tmp);
    Check(Base64uriToBin(b64) = tmp);
    if i < 67 then // Base58 is much slower than Base64: not used on big buffers
    begin
      b58 := BinToBase58(tmp);
      Check(Base58ToBin(b58) = tmp);
    end;
    b32 := BinToBase32(tmp);
    Check(Base32ToBin(b32) = tmp);
    if tmp <> '' then
    begin
      Check(not IsPem(b64));
      Check(not IsPem(b32));
      Check(not NetIsPem(pointer(b64)));
      Check(not NetIsPem(pointer(b32)));
      b64 := DerToPem(pointer(tmp), length(tmp), TPemKind(i and 7));
      Check(IsPem(b64));
      Check(NetIsPem(pointer(b64)));
      CheckUtf8(PemToDer(b64) = tmp, b64);
      P := pointer(b64);
      CheckEqual(NextPem(P, @k), b64);
      Check(P <> nil);
      Check(k = TPemKind(i and 7));
      CheckEqual(NextPem(P, @k), '');
    end;
    b64 := UnZeroed(tmp);
    Check(StrLen(pointer(b64)) = length(b64), 'unz');
    Check(Zeroed(b64) = tmp, 'UnZeroed');
    c := Random32;
    Append(tmp, @c, 1);
  end;
  Check(Zeroed(UnZeroed(#0)) = #0, 'unz0');
  Check(Zeroed(UnZeroed(#0#0)) = #0#0, 'unz1');
  Check(Zeroed(UnZeroed(#0'~'#0)) = #0'~'#0, 'unz2');
  Check(Zeroed(UnZeroed(#0#0'~~')) = #0#0'~~', 'unz3');
  Check(Zeroed(UnZeroed('~'#0#0'~~')) = '~'#0#0'~~', 'unz4');
  enc.Init;
  dec.Init;
  tmp := RandomWinAnsi(1 shl 20);
  Check(length(tmp) = 1 shl 20);
  b32 := BinToBase32(tmp);
  tmp2 := Base32ToBin(b32);
  CheckEqual(length(tmp2), length(tmp));
  Check(EqualBuf(tmp, tmp2), 'tmp=tmp2'); // tmp = tmp2 fails on FPC :(
  tmp2 := Zeroed(UnZeroed(tmp));
  Check(CompareBuf(tmp2, tmp) = 0, 'unz1MB');
  b64 := '';
  tmp2 := '';
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
  NotifyTestSpeed('base64 encoding' + msg, 0, i64, @enc);
  NotifyTestSpeed('base64 decoding' + msg, 0, i64, @dec);
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
  TEST_AES_GCM: array[0..2] of RawUtf8 = (
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

  procedure NistVector(aes: TAesAbstractClass; const plain, iv: RawByteString;
    ks: integer; const key: RawUtf8; const expected: RawUtf8);
  var
    k, c, d, e: RawByteString;
    a: TAesAbstract;
  begin
    Check(plain <> '');
    Check((length(iv) = 16) or
          (iv = ''));
    CheckEqual(length(plain) and AesBlockMod, 0);
    k := mormot.core.text.HexToBin(Key);
    Check(length(k) in [16, 24, 32]);
    e := mormot.core.text.HexToBin(expected);
    CheckEqual(length(e) and AesBlockMod, 0);
    CheckEqual(length(k) shl 3, ks);
    SetLength(c, length(plain));
    SetLength(d, length(plain));
    a := aes.Create(pointer(k)^, ks);
    try
      if iv <> '' then
        a.IV := PAesBlock(iv)^;
      a.Encrypt(pointer(plain), pointer(c), length(plain));
    finally
      a.Free;
    end;
    CheckEqual(mormot.core.text.BinToHex(c), expected);
    a := aes.Create(pointer(k)^, ks);
    try
      if iv <> '' then
        a.IV := PAesBlock(iv)^;
      a.Decrypt(pointer(c), pointer(d), length(plain));
    finally
      a.Free;
    end;
    Check(d = plain);
    {writeln('plain=',BinToHex(plain));
    writeln('key=',BinToHex(k));
    writeln('ciphered=',BinToHex(c));
    writeln('deciph=',BinToHex(d));}
  end;

const
  MAX = 4096 * 1024;  // test 4 MB data, i.e. multi-threaded AES
  MODES: array[0..9
     {$ifdef USE_OPENSSL} + 7 {$endif}
     {$ifdef USE_PROV_RSA_AES} + 2 {$endif}] of TAesAbstractClass = (
     // 0      1         2        3        4         5        6
     TAesEcb, TAesCbc, TAesCfb, TAesOfb, TAesC64, TAesCtr, TAesGcm,
     // 7      8         9
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
     {$endif USE_PROV_RSA_AES}); // TAesCfbApi and TAesOfbApi are not compliant?
var
  A: TAes;
  st, orig, crypted, s2, s3, s4, nistplain, nistiv: RawByteString;
  Key: TSha256Digest;
  s, b, p: TAesBlock;
  iv: THash128Rec;
  i, j, k, ks, m, len: integer;
  c: cardinal;
  tag1, tag2: TAesBlock;
  mac: TAesMac256;
  mac1, mac2: THash256;
  one, two, encdec: TAesAbstract;
  cts: TAesCbc;
  noaesni, gcm, aead: boolean;
  Timer: array[boolean] of TPrecisionTimer;
  ValuesCrypted, ValuesOrig: array[0..6] of RawByteString;
  Tags: array[0..2, 7..9] of THash256DynArray; // Tags[k,m]
  h32: array[0..2, 0..9] of TCardinalDynArray;
  tab: PCardinalArray;
  {$ifdef CPUINTEL}
  backup: TIntelCpuFeatures;
  {$endif CPUINTEL}
begin
  {$ifdef CPUINTEL}
  backup := CpuFeatures;
  {$endif CPUINTEL}
  CheckEqual(SizeOf(TMd5Buf), SizeOf(TMd5Digest));
  CheckEqual(1 shl AesBlockShift, SizeOf(TAesBlock));
  CheckEqual(SizeOf(TAes), AES_CONTEXT_SIZE);
  Check(AES_CONTEXT_SIZE <= 300); // lib/static/libsqlite3/sqlite3mc.c KEYLENGTH
  {$ifndef PUREMORMOT2}
  CheckEqual(SizeOf(TAesFullHeader), SizeOf(TAesBlock));
  {$endif PUREMORMOT2}
  CheckEqual(SizeOf(TSha1), SHA_CONTEXT_SIZE);
  CheckEqual(SizeOf(TSha256), SHA_CONTEXT_SIZE);
  CheckEqual(SizeOf(TSha256), SizeOf(TSha1));
  CheckEqual(SizeOf(TSha3), SHA3_CONTEXT_SIZE);
  Check(SizeOf(TSha512) > SizeOf(TSha256));
  Check(SizeOf(TSha3) > SizeOf(TSha512));
  CheckEqual(SizeOf(TSha384), SizeOf(TSha384512));
  CheckEqual(SizeOf(TSha512), SizeOf(TSha384512));
  CheckEqual(SizeOf(TSha512_256), SizeOf(TSha384512));
  SetLength(orig, MAX);
  SetLength(crypted, MAX + 256);
  st := '1234essai';
  orig := RandomWinAnsi(8000);
  Check(length(orig) = 8000);
  PInteger(UniqueRawUtf8(RawUtf8(st)))^ := Random32;
  for noaesni := false to true do
  begin
    // FIPS 197 and SP 800-38A - Advanced Encryption Standard (AES) tests
    nistplain := mormot.core.text.HexToBin(
      '6BC1BEE22E409F96E93D7E117393172AAE2D8A571E03AC9C9EB76FAC45AF8E51' +
      '30C81C46A35CE411E5FBC1191A0A52EFF69F2445DF4F9B17AD2B417BE66C3710');
    nistiv := mormot.core.text.HexToBin(
      '000102030405060708090A0B0C0D0E0F');
    NistVector(TAesEcb, nistplain, '', 128,
      '2B7E151628AED2A6ABF7158809CF4F3C',
      '3AD77BB40D7A3660A89ECAF32466EF97F5D3D58503B9699DE785895A96FDBAAF' +
      '43B1CD7F598ECE23881B00E3ED0306887B0C785E27E8AD3F8223207104725DD4');
    NistVector(TAesEcb, nistplain, '', 192,
      '8E73B0F7DA0E6452C810F32B809079E562F8EAD2522C6B7B',
      'BD334F1D6E45F25FF712A214571FA5CC974104846D0AD3AD7734ECB3ECEE4EEF' +
      'EF7AFD2270E2E60ADCE0BA2FACE6444E9A4B41BA738D6C72FB16691603C18E0E');
    NistVector(TAesEcb, nistplain, '', 256,
      '603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4',
      'F3EED1BDB5D2A03C064B5A7E3DB181F8591CCB10D410ED26DC5BA74A31362870' +
      'B6ED21B99CA6F4F9F153E7B1BEAFED1D23304B7A39F9F3FF067D8D8F9E24ECC7');
    NistVector(TAesCbc, nistplain, nistiv, 128,
      '2B7E151628AED2A6ABF7158809CF4F3C',
      '7649ABAC8119B246CEE98E9B12E9197D5086CB9B507219EE95DB113A917678B2' +
      '73BED6B8E3C1743B7116E69E222295163FF1CAA1681FAC09120ECA307586E1A7');
    NistVector(TAesCbc, nistplain, nistiv, 192,
      '8E73B0F7DA0E6452C810F32B809079E562F8EAD2522C6B7B',
      '4F021DB243BC633D7178183A9FA071E8B4D9ADA9AD7DEDF4E5E738763F69145A' +
      '571B242012FB7AE07FA9BAAC3DF102E008B0E27988598881D920A9E64F5615CD');
    NistVector(TAesCbc, nistplain, nistiv, 256,
      '603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4',
      'F58C4C04D6E5F1BA779EABFB5F7BFBD69CFC4E967EDB808D679F777BC6702C7D' +
      '39F23369A9D9BACFA530E26304231461B2EB05E2C39BE9FCDA6C19078C6A9D1B');
    NistVector(TAesCfb, nistplain, nistiv, 128,
      '2B7E151628AED2A6ABF7158809CF4F3C',
      '3B3FD92EB72DAD20333449F8E83CFB4AC8A64537A0B3A93FCDE3CDAD9F1CE58B' +
      '26751F67A3CBB140B1808CF187A4F4DFC04B05357C5D1C0EEAC4C66F9FF7F2E6');
    NistVector(TAesCfb, nistplain, nistiv, 192,
      '8E73B0F7DA0E6452C810F32B809079E562F8EAD2522C6B7B',
      'CDC80D6FDDF18CAB34C25909C99A417467CE7F7F81173621961A2B70171D3D7A' +
      '2E1E8A1DD59B88B1C8E60FED1EFAC4C9C05F9F9CA9834FA042AE8FBA584B09FF');
    NistVector(TAesCfb, nistplain, nistiv, 256,
      '603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4',
      'DC7E84BFDA79164B7ECD8486985D386039FFED143B28B1C832113C6331E5407B' +
      'DF10132415E54B92A13ED0A8267AE2F975A385741AB9CEF82031623D55B1E471');
    NistVector(TAesOfb, nistplain, nistiv, 128,
      '2B7E151628AED2A6ABF7158809CF4F3C',
      '3B3FD92EB72DAD20333449F8E83CFB4A7789508D16918F03F53C52DAC54ED825' +
      '9740051E9C5FECF64344F7A82260EDCC304C6528F659C77866A510D9C1D6AE5E');
    NistVector(TAesOfb, nistplain, nistiv, 192,
      '8E73B0F7DA0E6452C810F32B809079E562F8EAD2522C6B7B',
      'CDC80D6FDDF18CAB34C25909C99A4174FCC28B8D4C63837C09E81700C1100401' +
      '8D9A9AEAC0F6596F559C6D4DAF59A5F26D9F200857CA6C3E9CAC524BD9ACC92A');
    NistVector(TAesOfb, nistplain, nistiv, 256,
      '603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4',
      'DC7E84BFDA79164B7ECD8486985D38604FEBDC6740D20B3AC88F6AD82A4FB08D' +
      '71AB47A086E86EEDF39D1C5BBA97C4080126141D67F37BE8538F5A8BE740E484');
    nistiv := mormot.core.text.HexToBin(
      'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF');
    NistVector(TAesCtr, nistplain, nistiv, 128,
      '2B7E151628AED2A6ABF7158809CF4F3C',
      '874D6191B620E3261BEF6864990DB6CE9806F66B7970FDFF8617187BB9FFFDFF' +
      '5AE4DF3EDBD5D35E5B4F09020DB03EAB1E031DDA2FBE03D1792170A0F3009CEE');
    NistVector(TAesCtr, nistplain, nistiv, 192,
      '8E73B0F7DA0E6452C810F32B809079E562F8EAD2522C6B7B',
      '1ABC932417521CA24F2B0459FE7E6E0B090339EC0AA6FAEFD5CCC2C6F4CE8E94' +
      '1E36B26BD1EBC670D1BD1D665620ABF74F78A7F6D29809585A97DAEC58C6B050');
    NistVector(TAesCtr, nistplain, nistiv, 256,
      '603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4',
      '601EC313775789A5B7A7F504BBF3D228F443E3CA4D62B59ACA84E990CACAF5C5' +
      '2B0930DAA23DE94CE87017BA2D84988DDFC9C58DB67AADA613C2DD08457941A6');
    // check AES internal tables access
    Check(AesTablesTest, 'Internal Tables');
    tab := AesTables;
    CheckEqual(tab[0],  $50a7f451);
    CheckEqual(tab[99],  0);
    CheckEqual(tab[255],  $4257b8d0);
    CheckEqual(tab[$300 + 0],  $5150a7f4);  // @tab[$300] = @TD3
    CheckEqual(tab[$300 + 255],  $d04257b8);
    CheckEqual(tab[$400 + 0],  $a56363c6);  // @tab[$400] = @TE0
    CheckEqual(tab[$400 + 255],  $3a16162c);
    CheckEqual(tab[$500 + 0],  $6363c6a5);  // @tab[$500] = @TE1
    CheckEqual(tab[$500 + 255],  $16162c3a);
    CheckEqual(tab[$700 + 0],  $c6a56363);  // @tab[$700] = @TE3
    CheckEqual(tab[$700 + 255],  $2c3a1616);
    // check both mORMot and OpenSSL against our reference vectors
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
            CheckEqual(Sha256DigestToString(mac1), TEST_AES_MAC[m, k], 'TEST_AES_MAC');
          end
          else if gcm then
          begin
            RandomBytes(@tag1, SizeOf(tag1));
            Check(TAesGcmAbstract(one).AesGcmFinal(tag1));
            //writeln(one.classname, ks, ' ', AesBlockToShortString(tag1));
            CheckEqual(AesBlockToString(tag1), TEST_AES_GCM[k],
              FormatUtf8('TEST_AES_GCM % %', [ks, one.AlgoName]));
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
            Check(k in [0..2]);
            if m <= 9 then
              SetLength(h32[k, m], 257);
            if aead then
            begin
              Check(m in [7..9]);
              SetLength(Tags[k, m], 257);
            end;
            //Timer.Start;
            for i := 0 to 256 do
            begin
              if i < 64 then
                len := i
              else if i < 128 then
                len := (i * 5) shr 2
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
                begin
                  c := DefaultHasher(0, pointer(s3), length(s3));
                  CheckUtf8(h32[k, m, i] = c, '%=% len=%',
                    [h32[k, m, i], c, length(s3)]);
                end
                else
                  h32[k, m, i] := DefaultHasher(0, pointer(s3), length(s3));
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
      AddConsole('cypher with AES-NI: %, without: %',
        [Timer[false].Stop, Timer[true].Stop]);
      Include(CpuFeatures, cfAESNI); // revert Exclude() below from previous loop
    end;
    if HasHWAes then
      Exclude(CpuFeatures, cfAESNI);
    {$endif CPUINTEL}
  end;
  {$ifdef CPUINTEL}
  CpuFeatures := backup;
  {$endif CPUINTEL}
  // see https://datatracker.ietf.org/doc/html/rfc3962#appendix-B
  st := mormot.core.text.HexToBin('636869636b656e207465726979616b69');
  CheckEqual(length(st), 16);
  FillZero(iv.b);
  cts := TAesCbc.Create(PHash128(st)^);
  try
    orig := mormot.core.text.HexToBin('4920776f756c64206c696b652074686520');
    crypted := cts.EncryptCts(orig);
    CheckEqual(BinToHex(crypted), 'C6353568F2BF8CB4D8A580362DA7FF7F97');
    cts.iv := iv.b; // reset IV
    s2 := cts.DecryptCts(crypted);
    CheckEqual(s2, orig);
    cts.iv := iv.b;
    orig := mormot.core.text.HexToBin(
      '4920776f756c64206c696b65207468652047656e6572616c20476175277320');
    crypted := cts.EncryptCts(orig);
    CheckEqual(BinToHex(crypted),
      'FC00783E0EFDB2C1D445D4C8EFF7ED2297687268D6ECCCC0C07B25E25ECFE5');
    cts.iv := iv.b;
    s2 := cts.DecryptCts(crypted);
    CheckEqual(s2, orig);
    for i := 16 to 100 do
    begin
      orig := RandomAnsi7(i);
      CheckEqual(length(orig), i);
      cts.iv := iv.b;
      crypted := cts.EncryptCts(orig);
      CheckEqual(length(crypted), i);
      cts.iv := iv.b;
      s2 := cts.DecryptCts(crypted);
      CheckEqual(s2, orig);
      s3 := cts.EncryptCts(orig, true);
      CheckEqual(cts.DecryptCts(s3, true), orig);
    end;
  finally
    cts.Free;
  end;
end;

procedure TTestCoreCrypto._AES_GCM;
const
  hex32: THash256 = ($00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b,
    $0c, $0d, $0e, $0f, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a,
    $1b, $1c, $1d, $1e, $1f);
  buf32: THash256 = ($92, $4e, $17, $8a, $17, $fa, $1c, $a0, $e7, $48, $6f, $04,
    $04, $12, $3b, $91, $db, $f7, $97, $bb, $9d, $bd, $e9, $b1, $d4, $8d, $5c,
    $7f, $53, $16, $59, $12);
  tag32: THash128 = ($10, $f9, $72, $b6, $f9, $e0, $a3, $c1, $cf,
    $9c, $cf, $56, $54, $3d, $ca, $79);

  K01: THash256 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  I01: array[0..11] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  P01: THash128 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  C01: THash128 = ($ce, $a7, $40, $3d, $4d, $60, $6b, $6e, $07, $4e,
    $c5, $d3, $ba, $f3, $9d, $18);
  T01: THash128 = ($d0, $d1, $c8, $a7, $99, $99, $6b, $f0, $26, $5b,
    $98, $b5, $d4, $8a, $b9, $19);

  K02: THash256 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  I02: array[0..11] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  H02: THash128 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  T02: THash128 = ($2d, $45, $55, $2d, $85, $75, $92, $2b, $3c, $a3,
    $cc, $53, $84, $42, $fa, $26);

  K03: THash256 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  I03: array[0..11] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  H03: THash128 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  P03: THash128 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  C03: THash128 = ($ce, $a7, $40, $3d, $4d, $60, $6b, $6e, $07, $4e,
    $c5, $d3, $ba, $f3, $9d, $18);
  T03: THash128 = ($ae, $9b, $17, $71, $db, $a9, $cf, $62, $b3, $9b,
    $e0, $17, $94, $03, $30, $b4);

  K04: THash256 = ($fb, $76, $15, $b2, $3d, $80, $89, $1d, $d4, $70, $98, $0b,
    $c7, $95, $84, $c8, $b2, $fb, $64, $ce, $60, $97, $8f, $4d, $17, $fc, $e4,
    $5a, $49, $e8, $30, $b7);
  I04: array[0..11] of byte = ($db, $d1, $a3, $63, $60, $24, $b7, $b4, $02, $da,
    $7d, $6f);
  P04: THash128 = ($a8, $45, $34, $8e, $c8, $c5, $b5, $f1, $26, $f5,
    $0e, $76, $fe, $fd, $1b, $1e);
  C04: THash128 = ($5d, $f5, $d1, $fa, $bc, $bb, $dd, $05, $15, $38,
    $25, $24, $44, $17, $87, $04);
  T04: THash128 = ($4c, $43, $cc, $e5, $a5, $74, $d8, $a8, $8b, $43,
    $d4, $35, $3b, $d6, $0f, $9f);

  K05: THash256 = ($40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b,
    $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a,
    $5b, $5c, $5d, $5e, $5f);
  I05: array[0..11] of byte = ($10, $11, $12, $13, $14, $15, $16, $17, $18, $19,
    $1a, $1b);
  H05: THash160 = (0, $01, $02, $03, $04, $05, $06, $07, $08, $09,
    $0a, $0b, $0c, $0d, $0e, $0f, $10, $11, $12, $13);
  P05: array[0..23] of byte = ($20, $21, $22, $23, $24, $25, $26, $27, $28, $29,
    $2a, $2b, $2c, $2d, $2e, $2f, $30, $31, $32, $33, $34, $35, $36, $37);
  C05: array[0..23] of byte = ($59, $1b, $1f, $f2, $72, $b4, $32, $04, $86, $8f,
    $fc, $7b, $c7, $d5, $21, $99, $35, $26, $b6, $fa, $32, $24, $7c, $3c);
  T05: THash128 = ($7d, $e1, $2a, $56, $70, $e5, $70, $d8, $ca, $e6,
    $24, $a1, $6d, $f0, $9c, $08);

  K07: THash256 = ($40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b,
    $4c, $4d, $4e, $4f, $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a,
    $5b, $5c, $5d, $5e, $5f);
  I07: array[0..11] of byte = ($10, $11, $12, $13, $14, $15, $16, $17, $18, $19,
    $1a, $1b);
  H07: THash256 = ($20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b,
    $2c, $2d, $2e, $2f, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3a,
    $3b, $3c, $3d, $3e, $3f);
  P07: TByteToByte = (0, $01, $02, $03, $04, $05, $06, $07, $08, $09,
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
  C07: TByteToByte = ($79, $3b, $3f, $d2, $52, $94, $12, $24, $a6, $af,
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
  T07: THash128 = ($87, $ec, $83, $7a, $bf, $53, $28, $55, $b2, $ce,
    $a1, $69, $d6, $94, $3f, $cd);

  K08: THash256 = ($fb, $76, $15, $b2, $3d, $80, $89, $1d, $d4, $70, $98, $0b,
    $c7, $95, $84, $c8, $b2, $fb, $64, $ce, $60, $97, $87, $8d, $17, $fc, $e4,
    $5a, $49, $e8, $30, $b7);
  I08: array[0..11] of byte = ($db, $d1, $a3, $63, $60, $24, $b7, $b4, $02, $da,
    $7d, $6f);
  H08: array[0..0] of byte = ($36);
  P08: array[0..0] of byte = ($a9);
  C08: array[0..0] of byte = ($0a);
  T08: THash128 = ($be, $98, $7d, 0, $9a, $4b, $34, $9a, $a8, $0c,
    $b9, $c4, $eb, $c1, $e9, $f4);

  K09: THash256 = ($f8, $d4, $76, $cf, $d6, $46, $ea, $6c, $23, $84, $cb, $1c,
    $27, $d6, $19, $5d, $fe, $f1, $a9, $f3, $7b, $9c, $8d, $21, $a7, $9c, $21,
    $f8, $cb, $90, $d2, $89);
  I09: array[0..11] of byte = ($db, $d1, $a3, $63, $60, $24, $b7, $b4, $02, $da,
    $7d, $6f);
  H09: THash160 = ($7b, $d8, $59, $a2, $47, $96, $1a, $21, $82, $3b,
    $38, $0e, $9f, $e8, $b6, $50, $82, $ba, $61, $d3);
  P09: THash160 = ($90, $ae, $61, $cf, $7b, $ae, $bd, $4c, $ad, $e4,
    $94, $c5, $4a, $29, $ae, $70, $26, $9a, $ec, $71);
  C09: THash160 = ($ce, $20, $27, $b4, $7a, $84, $32, $52, $01, $34,
    $65, $83, $4d, $75, $fd, $0f, $07, $29, $75, $2e);
  T09: THash128 = ($ac, $d8, $83, $38, $37, $ab, $0e, $de, $84, $f4,
    $74, $8d, $a8, $89, $9c, $15);

  K10: THash256 = ($db, $bc, $85, $66, $d6, $f5, $b1, $58, $da, $99, $a2, $ff,
    $2e, $01, $dd, $a6, $29, $b8, $9c, $34, $ad, $1e, $5f, $eb, $a7, $0e, $7a,
    $ae, $43, $28, $28, $9c);
  I10: THash128 = ($cf, $c0, $6e, $72, $2b, $e9, $87, $b3, $76, $7f,
    $70, $a7, $b8, $56, $b7, $74);
  P10: THash128 = ($ce, $20, $27, $b4, $7a, $84, $32, $52, $01, $34,
    $65, $83, $4d, $75, $fd, $0f);
  C10: THash128 = ($dc, $03, $e5, $24, $83, $0d, $30, $f8, $8e, $19,
    $7f, $3a, $ca, $ce, $66, $ef);
  T10: THash128 = ($99, $84, $ef, $f6, $90, $57, $55, $d1, $83, $6f,
    $2d, $b0, $40, $89, $63, $4c);

  K11: THash256 = ($0e, $05, $93, $5d, $f0, $c6, $93, $74, $18, $92, $b7, $6f,
    $af, $67, $13, $3a, $bd, $2c, $f2, $03, $11, $21, $bd, $8b, $b3, $81, $27,
    $a4, $d2, $ee, $de, $ea);
  I11: array[0..16] of byte = ($74, $b1, $ba, $26, $85, $b3, $68, $09, $14, $29,
    $fc, $cb, $8d, $cd, $de, $09, $e4);
  H11: THash160 = ($7b, $d8, $59, $a2, $47, $96, $1a, $21, $82, $3b,
    $38, $0e, $9f, $e8, $b6, $50, $82, $ba, $61, $d3);
  P11: THash160 = ($90, $ae, $61, $cf, $7b, $ae, $bd, $4c, $ad, $e4,
    $94, $c5, $4a, $29, $ae, $70, $26, $9a, $ec, $71);
  C11: THash160 = ($6b, $e6, $5e, $56, $06, $6c, $40, $56, $73, $8c,
    $03, $fe, $23, $20, $97, $4b, $a3, $f6, $5e, $09);
  T11: THash128 = ($61, $08, $dc, $41, $7b, $f3, $2f, $7f, $b7, $55,
    $4a, $e5, $2f, $08, $8f, $87);

  procedure test(ptag: pointer; tlen: PtrInt; const key; kbits: PtrInt;
    pIV: pointer; IV_Len: PtrInt; pAAD: pointer; aLen: PtrInt;
    ctp: pointer; cLen: PtrInt; ptp: pointer; tn: integer);
  var
    tag: TAesBLock;
    ctxt: TAesGcmEngine;
    avx: boolean;
    pt, ct: array[0..511] of byte;
  begin
    for avx := false to true do
    begin
      FillCharFast(pt, SizeOf(pt), 0);
      CheckUtf8(ctxt.FullDecryptAndVerify(key, kbits, pIV, pAAD, ctp, @pt, ptag,
        IV_Len, aLen, cLen, tlen, avx), 'FullDecryptAndVerify #%', [tn]);
      CheckUtf8(CompareMem(@pt, ptp, cLen), 'Plain #%', [tn]);
      FillCharFast(ct, SizeOf(ct), 0);
      CheckUtf8(ctxt.FullEncryptAndAuthenticate(key, kbits, pIV, pAAD, ptp, @ct,
        IV_Len, aLen, cLen, tag, avx), 'FullEncryptAndAuthenticate #%', [tn]);
      CheckUtf8(CompareMem(@tag, ptag, tlen), 'Tag #%', [tn]);
      CheckUtf8(CompareMem(@ct, ctp, cLen), 'Encoded #%', [tn]);
      {$ifndef CPUX64ASM}
      break;
      {$endif CPUX64ASM}
    end;
  end;

var
  ctxt: TAesGcmEngine;
  key, tag: TAesBlock;
  buf: THash512;
  n: integer;
  avx: boolean;
begin
  for avx := false to true do
  begin
    key := PAesBlock(@hex32)^;
    FillZero(buf);
    FillZero(tag);
    check(ctxt.FullEncryptAndAuthenticate(key, 128,
      @hex32, nil, @buf, @buf, 12, 0, SizeOf(buf), tag, avx));
    CheckEqual(CardinalToHex(crc32c(0, @buf, SizeOf(buf))), 'AC3DDD17');
    CheckEqual(Md5DigestToString(tag), '0332c40f9926bd3cdadf33148912c672');
  end;
  for n := 1 to 32 do
  begin
    Check(ctxt.Init(key, 128, false));
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
  test(@T10, 16, K10, 8 * SizeOf(K10), @I10, SizeOf(I10), nil, 0,
       @C10, SizeOf(C10), @P10, 10);
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

const
  // AesNiHash128() responses with fixed byte-increasing AesNiHashAntiFuzzTable
  // - to validate one identical algorithm on both i386 and x64 platforms
  AESNIHASH_REF128: PAnsiChar =     '00000000000000000000000000000000' +
    'ebcaa5d9d3111481ca62776f7cc716d5078f6490103c58d95b5e16001d40cf6b' +
    '9b0e47ebccb12f80270b0d17caafa8e1e64ef4b87ee4dd8743650db1ac9d59b0' +
    '345407a79f54f73c6a8120cd19ec289750bc08c882e7f30a72295534c6d5ad1c' +
    '2a4d337351355ef19da8ac689bc9abf5f4eea591f6845c428e70a22f16704d7c' +
    '7b6d2e6ba7fc17afe99577fdd4396a5400f82e31242f8f4837b128170def4d81' +
    '5ce88e10dd957cd5256cf17305668d7187e3a5f8364c2f11bf22cfab5c97e57c' +
    '9d62a4ea395e1a66c6da14059a5b046594887b40c2fd6beb93cb2c0ea3a6d826' +
    'e448eb7ff07caa6e91d9a2a8cdbf8ffb17e3839f9fd9b5db11fcc27ec73d90b6' +
    'ea5aa1a2c33f8507d4a4548ee3e951619e19c45be7fc245f2062b8a67200e933' +
    'e2bc6aee7eb66c9c626a5e24c294d64f9577d432b98b33e09b31b129956c9453' +
    '982c1746186eeeb2e725be97e486dc3343ab8f9474c99e6e38bd2d13a580f299' +
    'bba44ec2bfd7c869f719965d6b2232f0b7716a42122245c86fde9b2fcf8687cd' +
    'a9a2236e62424e427b7262ebc57694e5e3cf2be5086c55492ae1df51e642b16b' +
    'fa28fa2aa2b2b6470d03d915a77173d0222a5fdb927c149a2ed29ec6f39611b1' +
    'dfc2957b933caeac4206e5cd6b54505cf734b02d838621386f011a488a0aaa21' +
    'ca21a1cc16a0de4c7c1706a3d63a54196685816076f0bf423f62cc39c6e65cbe' +
    'eb71ecb63a7b6bff1f2501040a9cdd4d29d986eec0ba0eeb3cc81f107493fa26' +
    '8d16ede5b6c2311331043f392d75775205ea71c47441f88fb921091f36429447' +
    '5440c3a28156fcf83362c7de4563b9e5c74f2ae31c0892ba2416d9433fbec5e8' +
    'aa066e0839a2ff380eef8a50fdb8abf1a8e264d80d52547c19e8fc8c4cb0b6a9' +
    '228873ffd35acbd862051ba5972d1ea5c77da42d0932de9862d213d9c2a8e1be' +
    'ada5d458399c4aaa1047ae56ed803da266fc0be094eb5e1091e2f23842380209' +
    '75d184ec6b0f6cad9474dc5b335f7894aff4754b5ea5502a27ca73ff14328196' +
    '5539828fe6b11948b5be01053fbe212da322fa592d28db34898f824faddb1ebb' +
    '0f2071856be7a84df01aa15cc78b0e77959885fad4117d0d2f428db9a04557a3' +
    '097c7ffed3b8ad6e1a341610a40e852f70b117853a6de37263f5e726d67c3466';
  // AesNiHash32() responses with fixed byte-increasing AesNiHashAntiFuzzTable
  // - to validate one identical algorithm on both i386 and x64 platforms
  AESNIHASH_REF32: array[0 .. 52] of cardinal = (
    $00000000, $7ebbb034, $031c3f98, $84720a5e, $a838babc, $fbb8855b,
    $1c301a13, $611fff36, $9826aa67, $6e1939cf, $1a54fda0, $9dbffd04,
    $86308d64, $3593ea6e, $59276b61, $4341c163, $b88e7728, $11eca1e0,
    $1e1c27e1, $de24287e, $1af49bbc, $c0d1bf44, $e2606908, $bc8bc0f3,
    $932baa5c, $1211661d, $43fbca34, $67e4e4a0, $bb68c8c0, $1c17c3b0,
    $7dce27fa, $95a911c6, $04a499b3, $cda40a55, $d062c08f, $7f3a8f3f,
    $d4b99c7b, $1f110334, $ba207aaa, $f395c480, $19dbc289, $e50746b3,
    $bbc251fc, $ee5b2935, $70d8e8cf, $02a7706c, $f0ad90cf, $020670e0,
    $1322057e, $d9b7a878, $671232c8, $bbc2d9f4, $85676d1f);

procedure TTestCoreCrypto.Hashes;
var
  i, n: PtrInt;
  exp: cardinal;
  md: TMd5;
  dig, dig2: TMd5Digest;
  bytes: TByteToByte;
  ismd4: boolean;
  ref: PAnsiChar;
  ref32: PCardinal;
  h128, ref128: THash128;
  bak: THash512;
begin
  for i := 0 to high(bytes) do
    bytes[i] := i;
  // validate AesNiHash128() against reference vectors
  // - should be done FIRST with no process in the background
  if Assigned(AesNiHash128) and
     not CheckFailed(not fBackgroundRun.Waiting, 'no background run') then
  begin
    Move512(@bak, AesNiHashAntiFuzzTable);
    Move512(AesNiHashAntiFuzzTable, @bytes); // replace to get AESNIHASH_REF
    ref := AESNIHASH_REF128;
    ref32 := @AESNIHASH_REF32;
    n := 0;
    repeat
      exp := AesNiHash32(n, @bytes, n);
      CheckUtf8((n = 0) = (exp = 0), 'aesni32n1(%)', [n]);
      CheckUtf8(exp = ref32^, 'aesni32ref(%)', [n]);
      FillZero(h128);
      AesNiHash128(@h128, @bytes, n);
      Check(mormot.core.text.HexToBin(ref, @ref128, SizeOf(ref128)));
      inc(ref, SizeOf(ref128) * 2);
      CheckUtf8(IsEqual(h128, ref128), 'aesni128ref(%)', [n]);
      exp := PCardinal(@ref128)^;
      CheckUtf8(AesNiHash32(0, @bytes, n) = exp, 'aesni32trunc(%)', [n]);
      CheckUtf8(AesNiHash32(n, @bytes, n) = ref32^, 'aesni32n2(%)', [n]);
      inc(ref32);
      if n < 20 then
        inc(n) // specific verification of pshufb process for 1..16 bytes
      else
        inc(n, 7);
    until n > 250;
    CheckEqual(n, 251);
    Move512(AesNiHashAntiFuzzTable, @bak); // preserve existing hash tables
  end;
  // validate 32-bit, 64-bit and 128-bit crc functions in the background
  Run(CrcSlow, nil, 'crc', {threaded=}true, {notify=}false);
  // MD5 validation
  CheckEqual(htdigest('agent007', 'download area', 'secret'),
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65', 'htdigest');
  CheckEqual(Md5(''), 'd41d8cd98f00b204e9800998ecf8427e', 'MD5ref1');
  CheckEqual(Md5('a'), '0cc175b9c0f1b6a831c399e269772661', 'MD5ref2');
  CheckEqual(Md5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'),
    'd174ab98d277d9f5a5611c2c9f419d9f', 'MD5ref3');
  // MD4 validation
  CheckEqual(Md4(''), '31d6cfe0d16ae931b73c59d7e0c089c0', 'MD4ref1');
  CheckEqual(Md4('Wikipedia, l''encyclopedie libre et gratuite'),
    'b94e66e0817dd34dc7858a0c131d4079', 'MD4ref2');
  CheckEqual(Md4('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'),
    '043f8582f241db351ce627e153e7f0e4', 'MD4ref3');
  CheckEqual(Md4(HexToBin('839c7a4d7a92cb5678a5d5b9eea5a7573c8a74deb366c3dc20a08' +
    '3b69f5d2a3bb3719dc69891e9f95e809fd7e8b23ba6318edd45e51fe39708bf9427e9c3e8b9')),
    '4d7e6a1defa93d2dde05b45d864c429b', 'colllisionA');
  CheckEqual(Md4(HexToBin('839c7a4d7a92cbd678a5d529eea5a7573c8a74deb366c3dc20a08' +
    '3b69f5d2a3bb3719dc69891e9f95e809fd7e8b23ba6318edc45e51fe39708bf9427e9c3e8b9')),
    '4d7e6a1defa93d2dde05b45d864c429b', 'colllisionB');
  // MD Context Hashing validation
  for ismd4 := false to true do
    for n := 256 - 80 to 256 do
    begin
      // char-by-char update validation
      if ismd4 then
        Md4Init(md)
      else
        md.Init;
      for i := 0 to n - 1 do
        md.Update(bytes[i], 1);
      md.Final(dig);
      // full buffer single call validation
      if ismd4 then
        Md4Buf(bytes, n, dig2)
      else
        md.Full(@bytes, n, dig2);
      check(IsEqual(dig, dig2), 'MDrefA');
      check(CompareMem(@dig, @dig2, SizeOf(dig)), 'MDrefB');
    end;
end;

function TTestCoreCrypto.DigestUser(const User, Realm: RawUtf8;
  out HA0: THash512Rec): TAuthServerResult;
begin
  if DigestHA0(fDigestAlgo, User, Realm, User + '"pass', HA0) <> 0 then
    result := asrMatch
  else
    result := asrUnknownUser;
end;

procedure TTestCoreCrypto.Digest;
var
  n, u: integer;
  a: TDigestAlgo;
  h32: cardinal;
  realm, user, url, pwd, s, c, authuser, authurl, fpwd: RawUtf8;
  sec: SpiUtf8;
  opaque: Int64;
  fn: TFileName;
  dig: TDigestAuthServerFile;
  users, pwds, users2: TRawUtf8DynArray;
  bak: RawByteString;
begin
  // validate raw client-server Digest access authentication
  Check(DigestServerInit(daUndefined, '', '', '', 0) = '');
  Check(DigestClient(daUndefined, '', '', '', '', '') = '');
  for n := 1 to 10 do
  begin
    realm := RandomAnsi7(10) + '"';
    user := RandomUri(10);
    pwd := user + '"pass';
    url := '/' + RandomUri(15);
    opaque := Random64;
    // pwd and realm have a quote within
    for a := daMD5 to high(a) do
    begin
      Check(DigestServerInit(a, '', '', '', opaque) = '');
      s := DigestServerInit(a, QuotedStr(realm, '"'), '', '', opaque);
      Check(s <> '');
      CheckEqual(DigestRealm(s), realm, 'realm server');
      Check(DigestClient(daUndefined, s, 'GET', url, user, pwd) = '');
      c := DigestClient(a, s, 'GET', url, user, pwd);
      Check(c <> '');
      CheckEqual(DigestRealm(c), realm, 'realm client');
      fDigestAlgo := a;
      Check(DigestServerAuth(a, realm, 'GET', pointer(c), opaque, DigestUser,
        authuser, authurl, 100) = asrMatch, 'auth ok');
      dec(opaque);
      Check(DigestServerAuth(a, realm, 'GET', pointer(c), opaque, DigestUser,
        authuser, authurl, 100) = asrRejected, 'connection change detection');
      inc(opaque);
      fDigestAlgo := daUndefined;
      Check(DigestServerAuth(a, realm, 'GET', pointer(c), opaque, DigestUser,
        authuser, authurl, 100) = asrUnknownUser, 'wrong algo');
    end;
  end;
  Check(DigestServerInit(daUndefined, realm, '', '', opaque) = '');
  // validate TDigestAuthServerFile
  SetLength(users, 100);
  SetLength(pwds, length(users));
  for u := 0 to length(users) - 1 do
  begin
    FormatUtf8('user%', [u + 1], users[u]);
    FormatUtf8('%pwd%', [u + 1, Random32], pwds[u]);
  end;
  realm := RandomUri(10);
  fn := WorkDir + '.htdigest';
  for a := daMD5 to high(a) do
  begin
    // initialize a new .htdigest file for each algorithm
    fn := WorkDir + '.htdigest';
    DeleteFile(fn);
    fpwd := '';
    if a = daMD5_Sess then
      fpwd := 'encryptsecret';
    dig := TDigestAuthServerFile.Create(realm, fn, fpwd, a);
    try
      Check(dig.Encrypted = (fpwd <> ''));
      CheckEqual(dig.Count, 0);
      Check(not dig.Modified);
      for u := 0 to high(users) do
        dig.SetCredential(users[u], pwds[u]);
      Check(dig.Modified);
      CheckEqual(dig.Count, length(users));
      for u := 0 to high(users) do
        Check(dig.CheckCredential(users[u], pwds[u]) = asrMatch, 'check1');
    finally
      dig.Free;
    end;
    // reload the file from scratch
    dig := TDigestAuthServerFile.Create(realm, fn, fpwd, a);
    try
      // ensure everything was properly persisted
      Check(dig.Encrypted = (fpwd <> ''));
      CheckEqual(dig.Count, length(users), 'reload1');
      Check(not dig.Modified);
      for u := 0 to high(users) do
        Check(dig.CheckCredential(users[u], pwds[u]) = asrMatch, 'check2');
      Check(not dig.Modified);
      // remove some entries
      for u := 0 to length(users) shr 3 do
        dig.SetCredential(users[u * 8], '');
      Check(dig.Modified);
      for u := 0 to high(users) do
        Check((dig.CheckCredential(users[u], pwds[u]) = asrMatch) =
              ((u and 7) <> 0), 'check3');
      Check(dig.Modified);
      dig.SaveToFile;
      bak := StringFromFile(fn);
      // add missing entries
      Check(not dig.Modified);
      for u := length(users) shr 3 downto 0 do
        dig.SetCredential(users[u * 8], pwds[u * 8]);
      Check(dig.Modified);
      for u := 0 to high(users) do
        Check(dig.CheckCredential(users[u], pwds[u]) = asrMatch, 'check4');
      Check(dig.Modified);
      // update some entries
      for u := length(users) shr 3 downto 0 do
      begin
        pwds[u * 8] := pwds[u * 8] + 'new';
        dig.SetCredential(users[u * 8], pwds[u * 8]);
      end;
      Check(dig.Modified);
      for u := 0 to high(users) do
        Check(dig.CheckCredential(users[u], pwds[u]) = asrMatch, 'check6');
    finally
      dig.Free;
    end;
    // reload after modifications, and validate authentication and file refresh
    dig := TDigestAuthServerFile.Create(realm, fn, fpwd, a);
    try
      Check(dig.Encrypted = (fpwd <> ''));
      Check(not dig.Modified);
      CheckEqual(dig.Count, length(users), 'reload2');
      for u := 0 to high(users) do
        Check(dig.CheckCredential(users[u], pwds[u]) = asrMatch, 'check7');
      // test actual client/server authentication of all users
      for u := 0 to high(users) do
      begin
        opaque := Random64;
        s := dig.DigestInit(opaque, 0, '', '');
        Check(s <> '');
        c := DigestClient(a, s, 'GET', url, users[u], pwds[u]);
        Check(c <> '');
        Check(dig.DigestAlgoMatch(c), 'algo');
        Check(dig.DigestAuth(
          pointer(c), 'GET', opaque, 0, authuser, authurl) = asrMatch, 'auth1');
        CheckEqual(authuser, users[u]);
        CheckEqual(authurl, url);
        inc(opaque);
        Check(dig.DigestAuth(
          pointer(c), 'GET', opaque, 0, authuser, authurl) = asrRejected, 'auth2');
        dec(opaque);
        Check(dig.DigestAuth(
          pointer(c), 'GET', opaque, 0, authuser, authurl) = asrMatch, '3');
        CheckEqual(authuser, users[u]);
        CheckEqual(authurl, url);
        c := DigestClient(a, s, 'GET', url, users[u], pwds[u] + 'wrong');
        Check(c <> '');
        Check(dig.DigestAlgoMatch(c));
        Check(dig.DigestAuth(pointer(c), 'GET', opaque, 0,
          authuser, authurl) = asrIncorrectPassword, 'auth3');
        CheckEqual(authuser, '');
        CheckEqual(authurl, '');
        s := dig.BasicInit;
        Check(IdemPChar(pointer(s), 'WWW-AUTHENTICATE: BASIC '));
        CheckEqual(BasicRealm(copy(s, 25, 100)), dig.Realm);
        BasicClient(users[u], pwds[u], sec, '');
        Check(sec <> '');
        Check(dig.BasicAuth(pointer(sec), authuser));
        CheckEqual(authuser, users[u]);
        BasicClient(users[u], pwds[u] + 'wrong', sec, '');
        Check(sec <> '');
        Check(not dig.BasicAuth(pointer(sec), authuser));
        CheckEqual(authuser, '');
      end;
      // force file refresh (from previously bak state)
      Check(not dig.RefreshFile);
      FileFromString(bak, fn);
      FileSetDateFromUnixUtc(fn, UnixTimeUtc - SecsPerDay); // as previous day
      Check(dig.RefreshFile, 'RefreshFile');
      Check(not dig.Modified, 'not Modified');
      for u := 0 to high(users) do
        Check((dig.CheckCredential(users[u], pwds[u])  = asrMatch) =
              ((u and 7) <> 0), 'check8');
      Check(length(users) <> dig.Count, 'users<>count');
      // validate GetUsers method
      users2 := dig.GetUsers;
      CheckEqual(length(users2), dig.Count);
      h32 := 0;
      for u := 0 to high(users2) do
      begin
        Check(IdemPChar(pointer(users2[u]), 'USER'));
        h32 := crc32c(h32, pointer(users2[u]), length(users2[u]));
      end;
      CheckEqual(h32, 2570601015);
    finally
      dig.Free;
    end;
  end;
end;

const
  // $ klist -kt test.keytab
  // KVNO Timestamp           Principal      Password
  // ---- ------------------- ------------------------------------------------------
  //    1 26/06/2025 16:23:40 toto@MY.LAN    titi
  //    1 26/06/2025 16:23:40 toto2@MY.LAN   tutu
  KEYTAB_REF: array[0.. $8c] of byte = (
    $05, $02, $00, $00, $00, $41, $00, $01, $00, $06, $4d, $59, $2e, $4c, $41,
    $4e, $00, $04, $74, $6f, $74, $6f, $00, $00, $00, $01, $68, $5d, $57, $ec,
    $01, $00, $12, $00, $20, $c4, $f2, $ec, $2e, $9b, $04, $8c, $7f, $db, $82,
    $65, $e0, $15, $79, $f7, $fd, $4f, $33, $16, $4f, $b7, $29, $0a, $52, $86,
    $72, $98, $bf, $a2, $b7, $94, $ab, $00, $00, $00, $01, $00, $00, $00, $42,
    $00, $01, $00, $06, $4d, $59, $2e, $4c, $41, $4e, $00, $05, $74, $6f, $74,
    $6f, $32, $00, $00, $00, $01, $68, $5d, $57, $ec, $01, $00, $12, $00, $20,
    $18, $94, $1a, $0e, $92, $78, $d6, $d9, $78, $f3, $b5, $bb, $a7, $a1, $99,
    $50, $c6, $c1, $2c, $78, $6e, $26, $ba, $ec, $ac, $d9, $4d, $0b, $cb, $6f,
    $56, $87, $00, $00, $00, $01);

procedure TTestCoreCrypto._TKerberosKeyTab;
var
  bin, bin2, password: RawByteString;
  hex: RawUtf8;
  kt, kt2: TKerberosKeyTab;
  ktg: TKerberosKeyTabGenerator;
  a: TSignAlgo;
  p: PByteArray;
  i: PtrInt;
begin
  // validate low-level Kerberos cryptography
  // https://datatracker.ietf.org/doc/html/rfc3962#appendix-B
  a := saSha3S256;
  bin := MakeKerberosKeySeed('password', 'ATHENA.MIT.EDUraeburn',
    ENCTYPE_AES128_CTS_HMAC_SHA1_96, 1, @a);
  Check(a = saSha1);
  CheckEqualHex(bin, 'cdedb5281bb2f801565a1122b2563515');
  a := saSha3S256;
  bin := MakeKerberosKeySeed('password', 'ATHENA.MIT.EDUraeburn',
    ENCTYPE_AES256_CTS_HMAC_SHA1_96, 1, @a);
  Check(a = saSha1);
  CheckEqualHex(bin,
    'cdedb5281bb2f801565a1122b25635150ad1f7a04bb9f3a333ecc0e2e1f70837');
  a := saSha3S256;
  bin := MakeKerberosKeySeed('password', 'ATHENA.MIT.EDUraeburn',
    ENCTYPE_AES128_CTS_HMAC_SHA1_96, 2, @a);
  Check(a = saSha1);
  CheckEqualHex(bin, '01dbee7f4a9e243e988b62c73cda935d');
  a := saSha3S256;
  bin := MakeKerberosKeySeed('password', 'ATHENA.MIT.EDUraeburn',
    ENCTYPE_AES256_CTS_HMAC_SHA1_96, 2, @a);
  Check(a = saSha1);
  CheckEqualHex(bin,
    '01dbee7f4a9e243e988b62c73cda935da05378b93244ec8f48a99e61ad799d86');
  // https://datatracker.ietf.org/doc/html/rfc3961#appendix-A.1
  CheckEqualHex(Rfc3961Nfold('012345', 64 shr 3), 'be072631276b1955');
  CheckEqualHex(Rfc3961Nfold('password', 56 shr 3), '78a07b6caf85fa');
  CheckEqualHex(Rfc3961Nfold('password', 168 shr 3),
    '59e4a8ca7c0385c3c37b3f6d2000247cb6e6bd5b3e');
  CheckEqualHex(Rfc3961Nfold('kerberos', 64 shr 3), '6b65726265726f73');
  CheckEqualHex(Rfc3961Nfold('kerberos', 128 shr 3),
    '6b65726265726f737b9b5b2b93132b93');
  CheckEqualHex(Rfc3961Nfold('kerberos', 168 shr 3),
    '8372c236344e5f1550cd0747e15d62ca7a5a3bcea4');
  CheckEqualHex(Rfc3961Nfold('kerberos', 256 shr 3),
    '6b65726265726f737b9b5b2b93132b935c9bdcdad95c9899c4cae4dee6d6cae4');
  // https://datatracker.ietf.org/doc/html/rfc3962#appendix-B
  bin := MakeKerberosKey('password', 'ATHENA.MIT.EDUraeburn',
    ENCTYPE_AES128_CTS_HMAC_SHA1_96, 1);
  CheckEqualHex(bin, '42263c6e89f4fc28b8df68ee09799f15');
  bin := MakeKerberosKey('password', 'ATHENA.MIT.EDUraeburn',
    ENCTYPE_AES256_CTS_HMAC_SHA1_96, 1);
  CheckEqualHex(bin,
    'fe697b52bc0d3ce14432ba036a92e65bbb52280990a2fa27883998d72af30161');
  // [MS-KILE] "4.4 AES 128 Key Creation"
  p := FastNewRawByteString(password, 120 * 3);
  for i := 1 to 120 do
  begin
    p[0] := $ef;
    p[1] := $bf;
    p[2] := $bf;
    p := @p[3];
  end;
  Check(p = @PByteArray(password)[length(password)]);
  bin := MakeKerberosKeySeed(password, 'DOMAIN.COMhostclient.domain.com',
    ENCTYPE_AES128_CTS_HMAC_SHA1_96, 1000);
  CheckEqualHex(bin, 'c7730daa23521bc16ab83cbee3b37f41');
  bin := Rfc3962SeedtoKey(bin, ENCTYPE_AES128_CTS_HMAC_SHA1_96);
  CheckEqualHex(bin, 'b82ee122531c2d94821ac755bccb5879');
  // validate high-level TKerberosKeyTab wrapper
  FastSetRawByteString(bin, @KEYTAB_REF[0], length(KEYTAB_REF));
  CheckHash(bin, $1849920F);
  Check(BufferIsKeyTab(bin), 'bin1');
  kt := TKerberosKeyTab.Create;
  kt2 := TKerberosKeyTab.Create;
  try
    Check(kt.LoadFromBinary(bin), 'LoadFromString');
    if not CheckEqual(length(kt.Entry), 2, 'entry') then
      exit;
    with kt.Entry[0] do
    begin
      CheckEqual(TimeStamp, 1750947820);
      CheckEqual(KeyVersion, 1);
      CheckEqual(NameType, 1);
      CheckEqual(EncType, ENCTYPE_AES256_CTS_HMAC_SHA1_96);
      CheckEqual(Principal, 'toto@MY.LAN');
      CheckEqual(length(Key), SizeOf(THash256));
      hex := BinToHexLower(Key);
      CheckEqual(hex,
        'c4f2ec2e9b048c7fdb8265e01579f7fd4f33164fb7290a52867298bfa2b794ab');
      bin := MakeKerberosKey('titi', 'MY.LANtoto', EncType);
      CheckEqualHex(bin, hex);
    end;
    with kt.Entry[1] do
    begin
      CheckEqual(TimeStamp, 1750947820);
      CheckEqual(KeyVersion, 1);
      CheckEqual(NameType, 1);
      CheckEqual(EncType, ENCTYPE_AES256_CTS_HMAC_SHA1_96);
      CheckEqual(Principal, 'toto2@MY.LAN');
      CheckHash(Key, $D101D374);
      Check(MakeKerberosKey('tutu', 'MY.LANtoto2', EncType) = Key);
    end;
    Check(kt.Exists(kt.Entry[0]));
    Check(kt.Exists(kt.Entry[1]));
    CheckEqual(length(kt.Entry), 2, 'kt2');
    CheckEqual(length(kt2.Entry), 0, 'kt20');
    Check(not kt2.Exists(kt.Entry[0]));
    Check(not kt2.Exists(kt.Entry[1]));
    kt2.Add(kt.Entry[1]);
    CheckEqual(length(kt2.Entry), 1, 'kt21');
    Check(not kt2.Exists(kt.Entry[0]));
    Check(kt2.Exists(kt.Entry[1]));
    Check(kt2.Exists(kt2.Entry[0]));
    Check(CompareEntry(kt.Entry[1], kt2.Entry[0]));
    kt2.AddFrom(kt, ['toto']);
    CheckEqual(length(kt2.Entry), 1, 'kt21 filter');
    kt2.AddFrom(kt, []);
    CheckEqual(length(kt2.Entry), 2, 'kt22 no dup');
    Check(kt2.Exists(kt.Entry[0]));
    Check(kt2.Exists(kt.Entry[1]));
    Check(CompareEntry(kt.Entry[1], kt2.Entry[0]));
    Check(CompareEntry(kt.Entry[0], kt2.Entry[1]));
    bin2 := kt.SaveToBinary;
    CheckHash(bin2, $1849920F, 'same saved');
    Check(BufferIsKeyTab(bin2), 'bin2');
    bin2 := kt2.SaveToBinary;
    CheckHash(bin2, $67233E99, 'not the same order');
    Check(BufferIsKeyTab(bin2), 'bin3');
    Check(kt.LoadFromBinary(bin2), 'LoadFromString2');
    if CheckEqual(length(kt.Entry), 2, 'entry') then
    begin
      Check(CompareEntry(kt.Entry[0], kt2.Entry[0]));
      Check(CompareEntry(kt.Entry[1], kt2.Entry[1]));
      Check(not CompareEntry(kt.Entry[0], kt2.Entry[1]));
      Check(not CompareEntry(kt.Entry[1], kt2.Entry[0]));
      bin2 := kt.SaveToBinary;
      CheckHash(bin2, $67233E99);
      Check(BufferIsKeyTab(bin2), 'bin2');
      Check(not kt.Delete(10));
      Check(kt.Delete(0), 'deleted');
      Check(kt.Add(kt2.Entry[0]));
      bin2 := kt.SaveToBinary;
      CheckHash(bin2, $1849920F, 'delete saved');
      Check(BufferIsKeyTab(bin2), 'bin2');
      Check(kt.Delete(1), 'delete1');
      if CheckEqual(length(kt.Entry), 1, 'deleted1') then
        Check(CompareEntry(kt.Entry[0], kt2.Entry[1]));
      Check(kt.Delete(0), 'delete0');
      CheckEqual(length(kt.Entry), 0, 'flushed');
    end;
  finally
    kt2.Free;
    kt.Free;
  end;
  // TKerberosKeyTabGenerator should recreate the same exact KEYTAB_REF content
  ktg := TKerberosKeyTabGenerator.Create;
  try
    Check(ktg.AddNew('toto@MY.LAN',  'titi'), 'toto@MY.LAN');
    Check(ktg.AddNew('toto2@my.lan', 'tutu'), 'toto2@MY.LAN');
    if CheckEqual(length(ktg.Entry), 2) then
    begin
      CheckHash(ktg.Entry[1].Key, $D101D374);
      Check(ktg.Entry[1].Timestamp > 1750947820);
      Check(ktg.Entry[1].Timestamp > 1750947820);
      Check(UnixTimeUtc - ktg.Entry[0].Timestamp < 2, 'UnixTimeUtc');
      ktg.Entry[0].Timestamp := 1750947820; // as in KEYTAB_REF
      ktg.Entry[1].Timestamp := 1750947820;
      bin := ktg.SaveToBinary;
      Check(BufferIsKeyTab(bin), 'ktg');
      CheckHash(bin, $1849920F);
      Check(bin = bin2);
    end;
  finally
    ktg.Free;
  end;
end;

procedure TTestCoreCrypto.CatalogRunAsym(Context: TObject);
var
  asy: TCryptAsym absolute Context;
  pub, pub2, priv, priv2: RawUtf8;
  n, s: RawByteString;
  timer: TPrecisionTimer;
begin
  Check(mormot.crypt.secure.Asym(asy.AlgoName) = asy);
  if (asy.KeyAlgo in CKA_RSA) and
     not fCatalogAllGenerate then
  begin
    pub  := _rsapub; // don't validate the very slow RSA keypair generation
    priv := _rsapriv;
  end
  else
  begin
    timer.Start;
    asy.GeneratePem(pub, priv, '');
    Check(pub <> '');
    Check(priv <> '');
    asy.GeneratePem(pub2, priv2, '');
    NotifyTestSpeed('%.Generate %', [asy, asy.AlgoName], 2, 0, @timer, {onlylog=}true);
    Check(pub2 <> '');
    Check(priv2 <> '');
    Check(pub <> pub2);
    Check(priv <> priv2);
  end;
  n := RandomAnsi7(999);
  CheckUtf8(asy.Sign(n, priv, s), asy.AlgoName);
  Check(s <> '');
  Check(asy.Verify(n, pub, s));
  inc(n[1]);
  Check(not asy.Verify(n, pub, s));
  dec(n[1]);
end;

procedure TTestCoreCrypto.CatalogRunCert(Context: TObject);
var
  crt: TCryptCertAlgo absolute Context;
  timer: TPrecisionTimer;
  caa: TCryptAsymAlgo;
  c1, c2, c3, c4: ICryptCert;
  s, r, csr: RawByteString;
  jwt, iss, sub, s2, s3, n, priv: RawUtf8;
  fmt: TCryptCertFormat;
  cv: TCryptCertValidity;
  u: TCryptCertUsage;
  fields: TCryptCertFields;
  cpe: TCryptCertPerUsage;
begin
  timer.Start;
  check(PosEx(UpperCase(CAA_JWT[crt.AsymAlgo]), UpperCase(crt.AlgoName)) > 0);
  c1 := crt.New;
  caa := c1.AsymAlgo;
  check(caa = crt.AsymAlgo);
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
    Check(c1.AsymAlgo = caa, 'c1 caa');
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
  cv := c1.Verify(nil);
  CheckUtf8(cv = cvValidSelfSigned,
    '%:cvValidSelfSigned1=%', [crt.AlgoName, ToText(cv)^]);
  cv := c1.Verify(c1);
  CheckUtf8(cv = cvValidSelfSigned, 'cvValidSelfSigned2=%', [ToText(cv)^]);
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
  check(c1.IsValidDate, 'isvaliddate');
  check(c1.GetNotBefore <= NowUtc + CERT_DEPRECATION_THRESHOLD, 'nbef');
  check(c1.GetNotAfter > NowUtc - CERT_DEPRECATION_THRESHOLD, 'naft');
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
    CheckSameTime(c2.GetNotAfter, c1.GetNotAfter);
    CheckSameTime(c2.GetNotBefore, c1.GetNotBefore);
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
    CheckSameTime(c3.GetNotAfter, c1.GetNotAfter);
    CheckSameTime(c3.GetNotBefore, c1.GetNotBefore);
    CheckEqual(c3.GetDigest, c1.GetDigest);
    CheckEqual(word(c3.GetUsage), word(c1.GetUsage));
    if fmt = ccfPem then // PKCS12 seems to add some information to X509 :(
      CheckEqual(c3.GetPeerInfo, c1.GetPeerInfo);
    checkEqual(c3.GetPublicKey, c1.GetPublicKey);
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
  Check(c3.IsAuthorizedBy(c1), 'isauthby1');
  Check(not c3.IsAuthorizedBy(c3), 'isauthby2');
  cv := c3.Verify(nil);
  CheckUtf8(cv = cvUnknownAuthority, 'c3.Verify(nil)=%', [ToText(cv)^]);
  cv := c3.Verify(c1);
  CheckUtf8(cv = cvValidSigned, 'c3.Verify(c1)=%', [ToText(cv)^]);
  cv := c3.Verify(c2);
  CheckUtf8(cv = cvValidSigned, 'c3.Verify(c2)=%', [ToText(cv)^]);
  cv := c3.Verify(c3);
  CheckUtf8(cv = cvUnknownAuthority, 'c3.Verify(c3)=%', [ToText(cv)^]);
  n := '0123456789012345012345678901234'; // not a 16-byte multiple length
  r := c3.Encrypt(n);
  if r <> '' then // not all algorithms support encryption (RSA+ES256 only)
  begin
    CheckEqual(c3.Decrypt(r), n, 'asym ctr ' + crt.AlgoName);
    r := c3.Encrypt(n, 'aes-128-cbc');
    CheckEqual(c3.Decrypt(r, 'aes-128-cbc'), n, 'another padding ' + crt.AlgoName);
  end;
  s2 := GuidToRawUtf8(RandomGuid);
  Check(TrimGuid(s2));
  c2 := crt.New;
  fields.CommonName := s2;
  c2.Generate([cuDigitalSignature, cuKeyAgreement], '', nil, 30, -1, @fields);
  Check(c2.IsSelfSigned);
  Check(not c3.IsAuthorizedBy(c2), 'isauthby3');
  if crt.AlgoName <> 'syn-es256-v1' then
    CheckEqual(c2.GetSubject, s2);
  if c2.GetAuthorityKey <> c2.GetSubjectKey then
    CheckEqual(c2.GetAuthorityKey, '', 'X509 self-sign has no auth');
  if crt.AlgoName <> 'syn-es256-v1' then
    Check(c2.GetUsage = [cuDigitalSignature, cuKeyAgreement]);
  cv := c2.Verify(c1);
  CheckUtf8(cv = cvValidSelfSigned, '%:self1=%', [crt.AlgoName, ToText(cv)^]);
  if cv <> cvValidSelfSigned then
    ConsoleWriteRaw(c2.Save(cccCertWithPrivateKey, '', ccfPem)); // for debug
  cv := c2.Verify(nil);
  CheckUtf8(cv = cvValidSelfSigned, 'self2=%', [ToText(cv)^]);
  c2.Sign(c1); // change signature
  CheckEqual(c2.GetAuthorityKey, c1.GetSubjectKey);
  Check(not c2.IsSelfSigned);
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
  check( (s <> '') = (caa = caaES256), 'caaES256=sharedsecret');
  // c1 has [cuCA, cuDigitalSignature, cuKeyCertSign]
  // c2 has [cuDigitalSignature, cuKeyAgreement]
  // c3 has [cuDataEncipherment, cuKeyAgreement]
  cpe.Clear;
  check(cpe.Usages = [], 'cpeu1');
  check(not cpe.GetUsage(cuCA, c4), 'cpeu2');
  check(c4 = nil, 'c4');
  check(cpe.Add(nil) = [], 'cpeadd');
  check(cpe.Usages = [], 'cpeu3');
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
  priv := ''; // force generate a new private key
  csr := crt.CreateSelfSignedCsr('sub1,sub2', '', priv, [cuCA, cuDigitalSignature]);
  check(csr <> '', 'csr');
  check(priv <> '', 'priv');
  c2 := crt.GenerateFromCsr(csr);
  if not CheckFailed(c2 <> nil, 'gen csr1') then
  begin
    if crt.AlgoName <> 'syn-es256-v1' then
      check(c2.GetUsage = [cuCA, cuDigitalSignature], 'csr usage1');
    CheckEqual(c2.GetSubject, 'sub1', 'csr sub1');
    CheckEqual(RawUtf8ArrayToCsv(c2.GetSubjects), 'sub1,sub2', 'csr sub21');
    check(c2.IsSelfSigned, 'csr self1');
  end;
  c2 := crt.GenerateFromCsr(csr, c1);
  if not CheckFailed(c2 <> nil, 'gen csr2') then
  begin
    if crt.AlgoName <> 'syn-es256-v1' then
      check(c2.GetUsage = [cuCA, cuDigitalSignature], 'csr usage2');
    CheckEqual(c2.GetSubject, 'sub1', 'csr sub1');
    CheckEqual(RawUtf8ArrayToCsv(c2.GetSubjects), 'sub1,sub2', 'csr sub22');
    check(not c2.IsSelfSigned, 'csr self2');
    CheckEqual(c2.GetAuthorityKey, c1.GetSubjectKey, 'csr auth2');
  end;
  NotifyTestSpeed('% %', [c2.Instance, crt.AlgoName], 1, 0, @timer, {onlylog=}true);
end;

procedure TTestCoreCrypto.CatalogRunStore(Context: TObject);
var
  str: TCryptStoreAlgo absolute Context;
  timer: TPrecisionTimer;
  r, s: RawByteString;
  st1, st2, st3: ICryptStore;
  c1, c2, c3: ICryptCert;
  cv: TCryptCertValidity;
  crr: TCryptCertRevocationReason;
begin
  //writeln(str.AlgoName);
  timer.Start;
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
  r := RandomAnsi7(99);
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
  cv := st1.Verify(s, r);
  if cv <> cvNotSupported then
    // TCryptStoreOpenSsl.Verify has no way to know which cert signed it
    CheckUtf8(cv = cvValidSigned, 's1=%', [ToText(cv)^]);
  // persist the Store
  st2 := str.NewFrom(st1.Save);
  Check(st2 <> nil);
  CheckEqual(st2.Count, 3);
  Check(st2.IsValid(c1) = cvValidSelfSigned, '2c1');
  Check(st2.IsValid(c2) = cvValidSigned, '2c2');
  Check(st2.IsValid(c3) = cvValidSigned, '2c3');
  if cv <> cvNotSupported then
  begin
    Check(st2.Verify(s, r) = cvValidSigned, 's2a');
    dec(r[1]);
    Check(st2.Verify(s, r) = cvInvalidSignature, 's2b');
    inc(r[1]);
    Check(st2.Verify(s, r) = cvValidSigned, 's2c');
    // validate CRL on buffers (not OpenSSL)
    Check(st2.Revoke(c3, crrWithdrawn));
    Check(st2.Verify(s, r) = cvRevoked, 's2d');
    Check(st2.Revoke(c3, crrNotRevoked));
    Check(st2.Verify(s, r) = cvValidSigned, 's2e');
  end;
  // validate CRL on certificates
  Check(st2.Revoke(c3, crrWithdrawn), 'rev');
  crr := st2.IsRevoked(c3);
  CheckUtf8(crr = crrWithdrawn, 'wdw %', [ToText(crr)^]);
  // note: st2.Save fails with OpenSSL because the CRL is not signed
  // ensure new certs are not recognized by previous stores
  if st3 <> nil then
  begin
    CheckEqual(st3.Count, 3);
    Check(st3.IsValid(c1) = cvUnknownAuthority, '3c1');
    Check(st3.IsValid(c2) = cvUnknownAuthority, '3c2');
    Check(st3.IsValid(c3) = cvUnknownAuthority, '3c3');
    if cv <> cvNotSupported then
      Check(st3.Verify(s, r) = cvUnknownAuthority, 's3');
  end;
  st3 := st2;
  NotifyTestSpeed('%', [str.AlgoName], 1, 0, @timer, {onlylog=}true);
end;

procedure TTestCoreCrypto.Catalog;
var
  m: TAesMode;
  k, k2: integer;
  a, i, rounds, bytes: PtrInt;
  c32, cprev: cardinal;
  d, dprev: double;
  n, h, nprev, aead: RawUtf8;
  r, s: RawByteString;
  timer: TPrecisionTimer;
  aes: TAesAbstract;
  key: THash256;
  rnd: TCryptRandom;
  hsh: TCryptHasher;
  sig: TCryptSigner;
  cip: TCryptCipherAlgo;
  asy: TCryptAsym;
  en, de: ICryptCipher;
  crt: TCryptCertAlgo;
  str: TCryptStoreAlgo;
  alg: TCryptAlgos;
begin
  // validate AesAlgoNameEncode / TAesMode
  FillZero(key);
  for k := 0 to 2 do
    for m := low(m) to high(m) do
    begin
      n := AesAlgoNameEncode(m, 128 + k * 64);
      CheckEqual(length(n), 11);
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
  // validate Rnd() High-Level Algorithms Factory
  TAesPrng.Main.Random32; // warmup and entropy gathering outside timer
  alg := TCryptRandom.Instances;
  for a := 0 to high(alg) do
  begin
    rnd := alg[a] as TCryptRandom;
    NotifyProgress([rnd.AlgoName]);
    Check(mormot.crypt.secure.Rnd(rnd.AlgoName) = rnd);
    timer.Start;
    cprev := 0;
    dprev := 0;
    bytes := 0;
    rounds := 100;
    if PosEx('blocking', rnd.AlgoName) > 0 then
      rounds := 10; // some system random generators may be slow/blocking
    for i := 1 to rounds do
    begin
      c32 := rnd.Get32;
      CheckUtf8(c32 <> cprev, rnd.AlgoName);
      cprev := c32;
      c32 := rnd.Get32(i * 77);
      Check(c32 < cardinal(i * 77));
      d := rnd.GetDouble;
      check(d <> dprev);
      dprev := d;
      n := rnd.Get(i); // up to 10 bytes is fine on slow/blocking OS random API
      CheckEqual(length(n), i);
      inc(bytes, 12 + i);
    end;
    NotifyTestSpeed('%', [rnd.AlgoName], 0, bytes, @timer, {onlylog=}true);
  end;
  // validate Hash() High-Level Algorithms Factory
  alg := TCryptHasher.Instances;
  for a := 0 to high(alg) do
  begin
    hsh := alg[a] as TCryptHasher;
    NotifyProgress([hsh.AlgoName]);
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
  // validate Sign() High-Level Algorithms Factory
  alg := TCryptSigner.Instances;
  for a := 0 to high(alg) do
  begin
    sig := alg[a] as TCryptSigner;
    NotifyProgress([sig.AlgoName]);
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
  // validate Cipher() High-Level Algorithms Factory
  alg := TCryptCipherAlgo.Instances;
  for a := 0 to high(alg) do
  begin
    cip := alg[a] as TCryptCipherAlgo;
    NotifyProgress([cip.AlgoName]);
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
  // validate Asym() High-Level Algorithms Factory
  alg := TCryptAsym.Instances;
  //fCatalogAllGenerate := SystemInfo.dwNumberOfProcessors > 8; // not worth it
  for a := 0 to high(alg) do
  begin
    asy := alg[a] as TCryptAsym;
    Run(CatalogRunAsym, asy, asy.AlgoName,
      {threaded=} (asy.KeyAlgo in CKA_RSA) and fCatalogAllGenerate);
  end;
  // validate Cert() High-Level Algorithms Factory
  alg := TCryptCertAlgo.Instances;
  for a := 0 to high(alg) do
  begin
    crt := alg[a] as TCryptCertAlgo;
    Run(CatalogRunCert, crt, crt.AlgoName, {threaded=} crt.AsymAlgo in CAA_RSA);
  end;
  // validate Store() High-Level Algorithms Factory
  alg := TCryptStoreAlgo.Instances;
  for a := 0 to high(alg) do
  begin
    str := alg[a] as TCryptStoreAlgo;
    Run(CatalogRunStore, str, str.AlgoName, str.DefaultCertAlgo.AsymAlgo in CAA_RSA);
  end;
  // wait for all background thread process
  RunWait;
end;

procedure TTestCoreCrypto._TBinaryCookieGenerator;
var
  gen: TBinaryCookieGenerator;
  i: PtrInt;
  bak: RawUtf8;
  timer: TPrecisionTimer;
  r: TJwtContent;
  cook: array of RawUtf8;
  cookid: array of TBinaryCookieGeneratorSessionID;
begin
  // validate and benchmark a plain cookie with no record
  SetLength(cook, 16384);
  SetLength(cookid, length(cook));
  gen := TBinaryCookieGenerator.Create;
  try
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
  finally
    gen.Free;
  end;
  gen := TBinaryCookieGenerator.Create;
  try
    for i := 0 to high(cook) do
      CheckEqual(gen.Validate(cook[i]), 0, 'void');
    Check(gen.Load(bak), 'load');
    timer.Start;
    for i := 0 to high(cook) do
      CheckEqual(gen.Validate(cook[i]), cookid[i], 'loaded');
    NotifyTestSpeed('validate', length(cook), 0, @timer);
  finally
    gen.Free;
  end;
  // validate a cookie with its associated complex binary record
  SetLength(cook, 1024);
  gen := TBinaryCookieGenerator.Create;
  try
    FillCharFast(r, SizeOf(r), 0);
    for i := 0 to high(cook) do
    begin
      UInt32ToUtf8(i, r.reg[jrcIssuer]);
      r.data.InitObject([r.reg[jrcIssuer], i]);
      r.id.Value := i;
      cookid[i] := gen.Generate(cook[i], 0, @r, TypeInfo(TJwtContent));
      r.data.Clear; // to be reused in the loop
    end;
    for i := 0 to high(cook) - 1 do
      Check(cookid[i] <> cookid[i + 1]);
    for i := 0 to high(cook) do
      Check(cookid[i] <> 0);
    for i := 0 to high(cook) do
    begin
      // no Finalize(r); here to verify that RecordLoadBinary() does it
      r.id.Value := 0;
      CheckEqual(gen.Validate(cook[i], @r, TypeInfo(TJwtContent)),
        cookid[i], 'gen3');
      CheckEqual(r.id.Value, i);
      CheckEqual(GetInteger(pointer(r.reg[jrcIssuer])), i);
      Check(r.data.IsObject, 'obj');
      CheckEqual(r.data.Count, 1);
      CheckEqual(r.data.Names[0], r.reg[jrcIssuer]);
      CheckEqual(VariantToIntegerDef(r.data.Values[0], 0), i);
    end;
  finally
    gen.Free;
  end;
end;

procedure TTestCoreCrypto.Pkcs11;
var
  o: CK_OBJECT_CLASS;
  h: CK_HW_FEATURE_TYPE;
  k: CK_KEY_TYPE;
  c: CK_CERTIFICATE_TYPE;
  a: CK_ATTRIBUTE_TYPE;
  t: CK_MECHANISM_TYPE;
  r: CK_RV;
begin
  CheckEqual(1 shl ord(CKF_ERROR_STATE), $01000000);
  CheckEqual(ord(CKK_SHA512_T_HMAC), $00000045 + 1);
  CheckEqual(ToULONG(CKK_SHA512_T_HMAC), $00000045);
  CheckEqual(ToULONG(CKM_MD5), $0210);
  CheckEqual(cardinal(1 shl ord(CKF_EXTENSION)), $80000000);
  for o := low(o) to high(o) do
    Check(ToCKO(ToULONG(o)) = o);
  for h := low(h) to high(h) do
    Check(ToCKH(ToULONG(h)) = h);
  Check(ToCKK(ToULONG(CKK_none)) = CKK_VENDOR_DEFINED);
  for k := succ(low(k)) to high(k) do
    Check(ToCKK(ToULONG(k)) = k);
  for c := low(c) to high(c) do
    Check(ToCKC(ToULONG(c)) = c);
  for a := low(a) to high(a) do
    Check(ToCKA(ToULONG(a)) = a);
  for t := succ(low(t)) to pred(high(t)) do
    Check(ToULONG(succ(t)) > ToULONG(t));
  Check(ToCKM(ToULONG(CKM_none)) = CKM_VENDOR_DEFINED);
  for t := succ(low(t)) to high(t) do
    Check(ToCKM(ToULONG(t)) = t);
  for r := low(r) to pred(high(r)) do
    Check(ToULONG(succ(r)) > ToULONG(r));
  for r := low(r) to high(r) do
    Check(ToCKR(ToULONG(r)) = r);
end;

const
  // from FPC RTL
  _modulus =
    'bb32b4d0d89e9a9e8c79294c2ba8ef5c43d4933b9478ff3054c71bc8e52f1b99cd108d' +
    '67c8540c075ae4f4067fc7d684b42ccd2d4e4426011aea37beeff4c71507c3164c6b26' +
    '1909d2ff5910445b8a8981941dfee25f9a5f8a36d8b0e91f6f802254acac29435552d8' +
    '15be92687b94565118d0a7d5c35a47a8d83cc61d72dc04369daccf152c2e87d7f0fd49' +
    '7755aeec4aa9db8b291e3567fe9d9520dd798d600a7873dc2875a586df31fb130936a6' +
    'c3e02d46dc252b76f6adf4c77df868c23bb3335e542adf9baebfdc1019408d04ef6bca' +
    'eeb5853d2bd38d825fa91b6bbb06fe75e83c26372f31cfdc0e8d378ea5e87433d37f7b' +
    '0abc7206d1f3b2c56b18b5';
  _exponent = '010001';
  _hash = 'a135e3608e956e91743421e0677c03fbe2c7ce0890ff06423b66335e3428ef9a';
  _signature =
    '75bdcf54b21fd4f2891eec91d1e9f6d82adeb63bbb1db4e03a389b525e8f5b97669feb' +
    '2e9c87ef4e785124f5499918771e03e4ff83e31ce0cf4a8276809c35aafbf9b45b7918' +
    'f5d891d863ca441d5803dfd1c4190640a73ada10dc05c2ef480c449fdd157ab4cd1ade' +
    '0b067930e07607134ed425be5a0a1f78afd6045ba638e718bfb311b8377c0facded4cd' +
    '2b1e2692e480be260be355f050ebabf89e24f2833f56f0a74c185225db3b47b63612fb' +
    '9bdee1e1b8707807093e1551f24527a7631947d033ed7052c439e50b8a46e4d0c06dbc' +
    '38af1d64b49766a5cf9a82644650ffd733b61942db0bd8d47c8ef24a02dc9fd2ef557b' +
    '12ded804519f2b2b6c284d';

procedure TTestCoreCrypto.RsaSlow(Context: TObject);
var
  c: TRsa;
  bin, hash, signed, encrypted: RawByteString;
  timer: TPrecisionTimer;
begin
  // validate RSA key generation - in a background thread for this slow process
  hash := HexToBin(_hash);
  CheckEqual(length(hash), SizeOf(TSha256Digest));
  CheckHash(hash, $401CD1EB);
  timer.Start;
  c := TRsa.GenerateNew; // with RSA_DEFAULT_GENERATION_* values
  try
    NotifyTestSpeed('RS256 generate', -1, 0, @timer, fOwner.MultiThread);
    if CheckFailed(c <> nil, 'TimeOut') then
      exit;
    CheckEqual(c.ModulusBits, RSA_DEFAULT_GENERATION_BITS);
    CheckEqual(c.ModulusLen, 256);
    CheckEqual(c.E^.ToText, '65537');
    Check(c.HasPublicKey);
    Check(c.HasPrivateKey);
    Check(c.CheckPrivateKey);
    Check(c.MatchKey(c));
    Check(c.E^.IsPrime, 'genIsPrimeE');
    Check(c.P^.IsPrime, 'genIsPrimeP');
    Check(c.Q^.IsPrime, 'genIsPrimeQ');
    Check(not c.M^.IsPrime, 'genIsPrimeM');
    //c.P^.Debug('p', true); c.Q^.Debug('q', true);
    // have been pasted and verified with Wolfram Alpha "isprime" command and
    // http://www.javascripter.net/math/calculators/100digitbigintcalculator.htm
    signed := c.Sign(pointer(hash), hfSHA256);
    Check(c.Verify(pointer(hash), hfSHA256, signed), 'verif2');
    bin := c.SavePrivateKeyDer;
  finally
    c.Free;
  end;
  // ensure our generated RSA keys can be properly persisted and used
  c := TRsa.Create;
  try
    Check(c.LoadFromPrivateKeyDer(bin));
    CheckEqual(c.ModulusBits, RSA_DEFAULT_GENERATION_BITS);
    CheckEqual(c.ModulusLen, 256);
    Check(c.HasPublicKey);
    Check(c.HasPrivateKey);
    Check(c.CheckPrivateKey);
    Check(c.SavePrivateKeyDer = bin, 'gensaveload');
    CheckEqual(length(hash), SizeOf(TSha256Digest));
    CheckHash(hash, $401CD1EB);
    Check(c.Sign(pointer(hash), hfSHA256) = signed);
    Check(c.Verify(pointer(hash), hfSHA256, signed), 'verif3');
    encrypted := c.Seal(bin); // bin is typically > 1KB long
    Check(encrypted <> '', 'Seal');
    Check(c.Open(encrypted) = bin, 'Open');
  finally
    c.Free;
  end;
end;

const
  /// 2KB table of iterative differences of all known prime numbers < 18,000
  // - to validate generated BIGINT_PRIMES[] and TBigInt.MatchKnownPrime
  BIGINT_PRIMES_DELTA_BYTE: array[0 .. 258 * 8 - 1] of byte = (
    2, 1, 2, 2, 4, 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6,
    8, 4, 2, 4, 2, 4,14, 4, 6, 2,10, 2, 6, 6, 4, 6, 6, 2,10, 2, 4, 2,12,12,
    4, 2, 4, 6, 2,10, 6, 6, 6, 2, 6, 4, 2,10,14, 4, 2, 4,14, 6,10, 2, 4, 6,
    8, 6, 6, 4, 6, 8, 4, 8,10, 2,10, 2, 6, 4, 6, 8, 4, 2, 4,12, 8, 4, 8, 4,
    6,12, 2,18, 6,10, 6, 6, 2, 6,10, 6, 6, 2, 6, 6, 4, 2,12,10, 2, 4, 6, 6,
    2,12, 4, 6, 8,10, 8,10, 8, 6, 6, 4, 8, 6, 4, 8, 4,14,10,12, 2,10, 2, 4,
    2,10,14, 4, 2, 4,14, 4, 2, 4,20, 4, 8,10, 8, 4, 6, 6,14, 4, 6, 6, 8, 6,
   12, 4, 6, 2,10, 2, 6,10, 2,10, 2, 6,18, 4, 2, 4, 6, 6, 8, 6, 6,22, 2,10,
    8,10, 6, 6, 8,12, 4, 6, 6, 2, 6,12,10,18, 2, 4, 6, 2, 6, 4, 2, 4,12, 2,
    6,34, 6, 6, 8,18,10,14, 4, 2, 4, 6, 8, 4, 2, 6,12,10, 2, 4, 2, 4, 6,12,
   12, 8,12, 6, 4, 6, 8, 4, 8, 4,14, 4, 6, 2, 4, 6, 2, 6,10,20, 6, 4, 2,24,
    4, 2,10,12, 2,10, 8, 6, 6, 6,18, 6, 4, 2,12,10,12, 8,16,14, 6, 4, 2, 4,
    2,10,12, 6, 6,18, 2,16, 2,22, 6, 8, 6, 4, 2, 4, 8, 6,10, 2,10,14,10, 6,
   12, 2, 4, 2,10,12, 2,16, 2, 6, 4, 2,10, 8,18,24, 4, 6, 8,16, 2, 4, 8,16,
    2, 4, 8, 6, 6, 4,12, 2,22, 6, 2, 6, 4, 6,14, 6, 4, 2, 6, 4, 6,12, 6, 6,
   14, 4, 6,12, 8, 6, 4,26,18,10, 8, 4, 6, 2, 6,22,12, 2,16, 8, 4,12,14,10,
    2, 4, 8, 6, 6, 4, 2, 4, 6, 8, 4, 2, 6,10, 2,10, 8, 4,14,10,12, 2, 6, 4,
    2,16,14, 4, 6, 8, 6, 4,18, 8,10, 6, 6, 8,10,12,14, 4, 6, 6, 2,28, 2,10,
    8, 4,14, 4, 8,12, 6,12, 4, 6,20,10, 2,16,26, 4, 2,12, 6, 4,12, 6, 8, 4,
    8,22, 2, 4, 2,12,28, 2, 6, 6, 6, 4, 6, 2,12, 4,12, 2,10, 2,16, 2,16, 6,
   20,16, 8, 4, 2, 4, 2,22, 8,12, 6,10, 2, 4, 6, 2, 6,10, 2,12,10, 2,10,14,
    6, 4, 6, 8, 6, 6,16,12, 2, 4,14, 6, 4, 8,10, 8, 6, 6,22, 6, 2,10,14, 4,
    6,18, 2,10,14, 4, 2,10,14, 4, 8,18, 4, 6, 2, 4, 6, 2,12, 4,20,22,12, 2,
    4, 6, 6, 2, 6,22, 2, 6,16, 6,12, 2, 6,12,16, 2, 4, 6,14, 4, 2,18,24,10,
    6, 2,10, 2,10, 2,10, 6, 2,10, 2,10, 6, 8,30,10, 2,10, 8, 6,10,18, 6,12,
   12, 2,18, 6, 4, 6, 6,18, 2,10,14, 6, 4, 2, 4,24, 2,12, 6,16, 8, 6, 6,18,
   16, 2, 4, 6, 2, 6, 6,10, 6,12,12,18, 2, 6, 4,18, 8,24, 4, 2, 4, 6, 2,12,
    4,14,30,10, 6,12,14, 6,10,12, 2, 4, 6, 8, 6,10, 2, 4,14, 6, 6, 4, 6, 2,
   10, 2,16,12, 8,18, 4, 6,12, 2, 6, 6, 6,28, 6,14, 4, 8,10, 8,12,18, 4, 2,
    4,24,12, 6, 2,16, 6, 6,14,10,14, 4,30, 6, 6, 6, 8, 6, 4, 2,12, 6, 4, 2,
    6,22, 6, 2, 4,18, 2, 4,12, 2, 6, 4,26, 6, 6, 4, 8,10,32,16, 2, 6, 4, 2,
    4, 2,10,14, 6, 4, 8,10, 6,20, 4, 2, 6,30, 4, 8,10, 6, 6, 8, 6,12, 4, 6,
    2, 6, 4, 6, 2,10, 2,16, 6,20, 4,12,14,28, 6,20, 4,18, 8, 6, 4, 6,14, 6,
    6,10, 2,10,12, 8,10, 2,10, 8,12,10,24, 2, 4, 8, 6, 4, 8,18,10, 6, 6, 2,
    6,10,12, 2,10, 6, 6, 6, 8, 6,10, 6, 2, 6, 6, 6,10, 8,24, 6,22, 2,18, 4,
    8,10,30, 8,18, 4, 2,10, 6, 2, 6, 4,18, 8,12,18,16, 6, 2,12, 6,10, 2,10,
    2, 6,10,14, 4,24, 2,16, 2,10, 2,10,20, 4, 2, 4, 8,16, 6, 6, 2,12,16, 8,
    4, 6,30, 2,10, 2, 6, 4, 6, 6, 8, 6, 4,12, 6, 8,12, 4,14,12,10,24, 6,12,
    6, 2,22, 8,18,10, 6,14, 4, 2, 6,10, 8, 6, 4, 6,30,14,10, 2,12,10, 2,16,
    2,18,24,18, 6,16,18, 6, 2,18, 4, 6, 2,10, 8,10, 6, 6, 8, 4, 6, 2,10, 2,
   12, 4, 6, 6, 2,12, 4,14,18, 4, 6,20, 4, 8, 6, 4, 8, 4,14, 6, 4,14,12, 4,
    2,30, 4,24, 6, 6,12,12,14, 6, 4, 2, 4,18, 6,12, 8, 6, 4,12, 2,12,30,16,
    2, 6,22,14, 6,10,12, 6, 2, 4, 8,10, 6, 6,24,14, 6, 4, 8,12,18,10, 2,10,
    2, 4, 6,20, 6, 4,14, 4, 2, 4,14, 6,12,24,10, 6, 8,10, 2,30, 4, 6, 2,12,
    4,14, 6,34,12, 8, 6,10, 2, 4,20,10, 8,16, 2,10,14, 4, 2,12, 6,16, 6, 8,
    4, 8, 4, 6, 8, 6, 6,12, 6, 4, 6, 6, 8,18, 4,20, 4,12, 2,10, 6, 2,10,12,
    2, 4,20, 6,30, 6, 4, 8,10,12, 6, 2,28, 2, 6, 4, 2,16,12, 2, 6,10, 8,24,
   12, 6,18, 6, 4,14, 6, 4,12, 8, 6,12, 4, 6,12, 6,12, 2,16,20, 4, 2,10,18,
    8, 4,14, 4, 2, 6,22, 6,14, 6, 6,10, 6, 2,10, 2, 4, 2,22, 2, 4, 6, 6,12,
    6,14,10,12, 6, 8, 4,36,14,12, 6, 4, 6, 2,12, 6,12,16, 2,10, 8,22, 2,12,
    6, 4, 6,18, 2,12, 6, 4,12, 8, 6,12, 4, 6,12, 6, 2,12,12, 4,14, 6,16, 6,
    2,10, 8,18, 6,34, 2,28, 2,22, 6, 2,10,12, 2, 6, 4, 8,22, 6, 2,10, 8, 4,
    6, 8, 4,12,18,12,20, 4, 6, 6, 8, 4, 2,16,12, 2,10, 8,10, 2, 4, 6,14,12,
   22, 8,28, 2, 4,20, 4, 2, 4,14,10,12, 2,12,16, 2,28, 8,22, 8, 4, 6, 6,14,
    4, 8,12, 6, 6, 4,20, 4,18, 2,12, 6, 4, 6,14,18,10, 8,10,32, 6,10, 6, 6,
    2, 6,16, 6, 2,12, 6,28, 2,10, 8,16, 6, 8, 6,10,24,20,10, 2,10, 2,12, 4,
    6,20, 4, 2,12,18,10, 2,10, 2, 4,20,16,26, 4, 8, 6, 4,12, 6, 8,12,12, 6,
    4, 8,22, 2,16,14,10, 6,12,12,14, 6, 4,20, 4,12, 6, 2, 6, 6,16, 8,22, 2,
   28, 8, 6, 4,20, 4,12,24,20, 4, 8,10, 2,16, 2,12,12,34, 2, 4, 6,12, 6, 6,
    8, 6, 4, 2, 6,24, 4,20,10, 6, 6,14, 4, 6, 6, 2,12, 6,10, 2,10, 6,20, 4,
   26, 4, 2, 6,22, 2,24, 4, 6, 2, 4, 6,24, 6, 8, 4, 2,34, 6, 8,16,12, 2,10,
    2,10, 6, 8, 4, 8,12,22, 6,14, 4,26, 4, 2,12,10, 8, 4, 8,12, 4,14, 6,16,
    6, 8, 4, 6, 6, 8, 6,10,12, 2, 6, 6,16, 8, 6, 6,12,10, 2, 6,18, 4, 6, 6,
    6,12,18, 8, 6,10, 8,18, 4,14, 6,18,10, 8,10,12, 2, 6,12,12,36, 4, 6, 8,
    4, 6, 2, 4,18,12, 6, 8, 6, 6, 4,18, 2, 4, 2,24, 4, 6, 6,14,30, 6, 4, 6,
   12, 6,20, 4, 8, 4, 8, 6, 6, 4,30, 2,10,12, 8,10, 8,24, 6,12, 4,14, 4, 6,
    2,28,14,16, 2,12, 6, 4,20,10, 6, 6, 6, 8,10,12,14,10,14,16,14,10,14, 6,
   16, 6, 8, 6,16,20,10, 2, 6, 4, 2, 4,12, 2,10, 2, 6,22, 6, 2, 4,18, 8,10,
    8,22, 2,10,18,14, 4, 2, 4,18, 2, 4, 6, 8,10, 2,30, 4,30, 2,10, 2,18, 4,
   18, 6,14,10, 2, 4,20,36, 6, 4, 6,14, 4,20,10,14,22, 6, 2,30,12,10,18, 2,
    4,14, 6,22,18, 2,12, 6, 4, 8, 4, 8, 6,10, 2,12,18,10,14,16,14, 4, 6, 6,
    2, 6, 4, 2,28, 2,28, 6, 2, 4, 6,14, 4,12,14,16,14, 4, 6, 8, 6, 4, 6, 6,
    6, 8, 4, 8, 4,14,16, 8, 6, 4,12, 8,16, 2,10, 8, 4, 6,26, 6,10, 8, 4, 6,
   12,14,30, 4,14,22, 8,12, 4, 6, 8,10, 6,14,10, 6, 2,10,12,12,14, 6, 6,18,
   10, 6, 8,18, 4, 6, 2, 6,10, 2,10, 8, 6, 6,10, 2,18,10, 2,12, 4, 6, 8,10,
   12,14,12, 4, 8,10, 6, 6,20, 4,14,16,14,10, 8,10,12, 2,18, 6,12,10,12, 2,
    4, 2,12, 6, 4, 8, 4,44, 4, 2, 4, 2,10,12, 6, 6,14, 4, 6, 6, 6, 8, 6,36,
   18, 4, 6, 2,12, 6, 6, 6, 4,14,22,12, 2,18,10, 6,26,24, 4, 2, 4, 2, 4,14,
    4, 6, 6, 8,16,12, 2,42, 4, 2, 4,24, 6, 6, 2,18, 4,14, 6,28,18,14, 6,10,
   12, 2, 6,12,30, 6, 4, 6, 6,14, 4, 2,24, 4, 6, 6,26,10,18, 6, 8, 6, 6,30,
    4,12,12, 2,16, 2, 6, 4,12,18, 2, 6, 4,26,12, 6,12, 4,24,24,12, 6, 2,12,
   28, 8, 4, 6,12, 2,18, 6, 4, 6, 6,20,16, 2, 6, 6,18,10, 6, 2, 4, 8, 6, 6,
   24,16, 6, 8,10, 6,14,22, 8,16, 6, 2,12, 4, 2,22, 8,18,34, 2, 6,18, 4, 6,
    6, 8,10, 8,18, 6, 4, 2, 4, 8,16, 2,12,12, 6,18, 4, 6, 6, 6, 2, 6,12,10,
   20,12,18, 4, 6, 2,16, 2,10,14, 4,30, 2,10,12, 2,24, 6,16, 8,10, 2,12,22,
    6, 2,16,20,10, 2,12,12,18,10,12, 6, 2,10, 2, 6,10,18, 2,12, 6, 4, 6, 2);

procedure TTestCoreCrypto._RSA;
var
  c: TRsa;
  i, j, k: integer;
  rnd: HalfUInt; // truncated to 16-bit or 32-bit value
  a, b, s, d, e, m, v: PBigInt;
  cu, pem, txt: RawUtf8;
  bin, hash, signed, encrypted: RawByteString;
  pub1, pub2: TRsaPublicKey;
  pri1, pri2: TRsaPrivateKey;
  timer: TPrecisionTimer;
begin
  // validate RSA key generation - in a background thread for this slow process
  Run(RsaSlow, nil, 'rsaSlow', {threaded=}true, {notify=}false);
  // validate TBigInt/TRsaContext raw calculation
  c := TRsa.Create;
  try
    for i := 1 to 100 do
    begin
      CheckEqual(c.ActiveCount, 0, 'nomem');
      rnd := Random32;
      if rnd = 0 then
        continue; // avoid division per zero
      b := c.AllocateFrom(rnd);
      CheckEqual(b^.Size, 1);
      CheckEqual(b^.Value[0], rnd);
      v := c.AllocateFrom(rnd).IntMultiply(57777);
      d := b^.GreatestCommonDivisor(v);
      d.Trim;
      CheckEqual(d^.Size, 1);
      CheckEqual(d^.Value[0], gcd(rnd, PtrUInt(rnd) * 57777), 'gcd');
      v.Release;
      d.Release;
      cu := b^.ToHexa;
      txt := b^.ToText;
      CheckEqual(txt, UInt32ToUtf8(rnd));
      bin := b^.Save;
      s := c.LoadPermanent(bin);
      CheckEqual(txt, s.ToText);
      s.ResetPermanentAndRelease;
      CheckEqual(b^.IntMod(rnd), 0);
      for j := 1 to 10000 do
        CheckEqual(b^.IntMod(j), rnd mod HalfUInt(j));
      v := b^.Clone.Substract(c.AllocateFrom(1));
      Check(v.Compare(b) < 0);
      CheckEqual(v^.Size, 1);
      CheckEqual(v^.Value[0], rnd - 1);
      v := v^.Add(c.AllocateFrom(1));
      CheckEqual(v.Compare(b), 0);
      v.Release;
      CheckEqual(c.ActiveCount, 1);
      v := c.AllocateFrom(rnd).IntDivide(rnd);
      CheckEqual(v^.Size, 1);
      CheckEqual(v^.Value[0], 1);
      CheckEqual(v^.ToText, '1');
      CheckEqual(v.Compare(1), 0, 'b/rnd');
      CheckEqual(c.ActiveCount, 2);
      v.Release;
      CheckEqual(c.ActiveCount, 1);
      v := b^.Clone;
      CheckEqual(v^.ToText, txt);
      for j := 1 to 100 do
      begin
        v^.IntAdd(j * 3333);
        Check(v^.Compare(b) > 0);
      end;
      for j := 100 downto 1 do
      begin
        Check(v^.Compare(b) > 0);
        v^.IntSub(j * 3333);
      end;
      CheckEqual(v^.ToText, txt);
      Check(v^.Compare(b) = 0);
      CheckEqual(c.ActiveCount, 2);
      v.Release;
      CheckEqual(c.ActiveCount, 1);
      s := b.Clone;
      for k := 0 to 10 do
      begin
        if k < 9 then // limit up to s^.Size = 512 i.e. 4096 bits on CPU64
        begin
          // verify (s * s) / s = s
          v := s.Clone;
          CheckEqual(c.ActiveCount, 3);
          s := s.Multiply(v.Clone);
          d := s.Divide(v.Clone);
          CheckEqual(c.ActiveCount, 4);
          CheckEqual(d.ToHexa, v.ToHexa);
          CheckEqual(d.Compare(v), 0);
          d.Release;
          d := v.Clone;
          d.Add(c.AllocateFrom(0));
          CheckEqual(d.Compare(v), 0);
          d.Release;
          v.Release;
        end;
        for j := 0 to k do
        begin
          // b := b shl j to validate high number of bits
          b^.LeftShift(j);
          v := b.Clone;
          Check(v.Compare(b) = 0);
          CheckEqual(v.BitCount, b.BitCount);
          Check(v.IsZero = b.IsZero);
          CheckEqual(c.ActiveCount, 3);
          // verify (b * rnd) div rnd = s
          v := v.IntMultiply(rnd);
          CheckEqual(c.ActiveCount, 3);
          if rnd > 1 then
          begin
            Check(v.Compare(b) > 0);
            Check(v.BitCount > 0);
            Check(not v.IsZero);
          end;
          v := v.IntDivide(rnd);
          CheckEqual(v.Compare(b), 0);
          CheckEqual(v.ToHexa, b.ToHexa);
          CheckEqual(c.ActiveCount, 3);
          // verify b * 1 = b
          v := v.IntMultiply(1);
          CheckEqual(c.ActiveCount, 3);
          CheckEqual(v.Compare(b), 0);
          CheckEqual(v.Compare(0), CompareBI(rnd, 0), 'bi0');
          if rnd > 1 then
            CheckEqual(v.Trim.Compare(1), 1, 'bi1');
          // verify b * 0 = 0
          v := v.IntMultiply(0);
          CheckEqual(c.ActiveCount, 3);
          Check(v.Compare(b) <= 0);
          CheckEqual(v.Size, 1);
          CheckEqual(v.Value[0], 0);
          CheckEqual(v.BitCount, 0);
          Check(v.IsZero);
          CheckEqual(v.Compare(0), 0, '0');
          CheckEqual(v.ToText, '0');
          CheckEqual(v.ToHexa, '00');
          v.Trim;
          CheckEqual(v.Size, 1);
          v.Release;
          // verify b / b = 1
          v := b.Divide(b.Clone);
          CheckEqual(c.ActiveCount, 3);
          CheckEqual(v.Compare(1), 0, 'v/v');
          CheckEqual(v.ToText, '1');
          CheckEqual(v.ToHexa, '01');
          v.Release;
          CheckEqual(c.ActiveCount, 2);
          // verify b % b = 0
          v := b.Divide(b.Clone, bidMod);
          Check(v.IsZero);
          CheckEqual(v.Compare(0), 0, '0');
          CheckEqual(v.ToText, '0');
          CheckEqual(v.ToHexa, '00');
          v.Release;
          // verify persistence
          bin := b.Save;
          CheckEqual(length(bin), b.Size * HALF_BYTES);
          v := c.Load(pointer(bin), length(bin));
          Check(v <> nil);
          Check(not v.IsZero);
          CheckEqual(v.Compare(b), 0);
          v.Release;
        end;
        Check(b^.Size > 0); // from 1 to 56 = 448 bits
        // reverse b := b shr j to return to the initial value of b = rnd
        for j := 0 to k do
          b^.RightShift(j);
        CheckEqual(b^.Compare(rnd), 0);
        CheckEqual(b^.Size, 1);
        CheckEqual(b^.Value[0], rnd);
        CheckEqual(cu, b^.ToHexa);
        CheckEqual(c.ActiveCount, 2);
        // validate ShlBits/ShrBits
        for j := 0 to k do
        begin
          b^.ShlBits;
          CheckEqual(b^.Compare(rnd), 1);
        end;
        b^.ShrBits(k + 1);
        CheckEqual(b^.Compare(rnd), 0);
        CheckEqual(b^.Size, 1);
        CheckEqual(b^.Value[0], rnd);
      end;
      CheckEqual(b^.Size, 1);
      Check(not b^.IsZero);
      b.Release;
      CheckUtf8(s^.Size > 80, '%>80', [s^.Size]); // typical 90 .. 512 bytes
      Check(not s^.IsZero);
      s.Release;
      CheckEqual(c.ActiveCount, 0);
    end;
    // validate TBigInt.MatchKnownPrime
    rnd := 0;
    for i := 0 to high(BIGINT_PRIMES_DELTA_BYTE) do
    begin
      inc(rnd, BIGINT_PRIMES_DELTA_BYTE[i]); // 2 3 5 7 11 ..
      b := c.AllocateFrom(rnd);
      Check(b^.MatchKnownPrime(bspFast) = (rnd < 256));
      Check(b^.MatchKnownPrime(bspMost) = (rnd < 2000));
      Check(b^.MatchKnownPrime(bspAll));
      b := b.IntMultiply(high(HalfUInt));
      Check(b^.MatchKnownPrime(bspFast)); // high(HalfUInt) has stuffed primes
      Check(b^.MatchKnownPrime(bspAll));
      b.Release;
      CheckEqual(BIGINT_PRIMES[i], rnd); // computed in first MatchKnownPrime()
    end;
    CheckEqual(rnd, 17989, 'last prime');
    CheckEqual(BIGINT_PRIMES[high(BIGINT_PRIMES)], rnd, 'last table');
    // those values were reproducing an endless loop
    v := c.AllocateFromHex(
      '937577A81E8978AC5807A88554DDC172F14F20CEBF7B4BB519A2DCFF132AF1' +
      'DAA239F7443FDAEEEF49D3D78493E9ECCEE04B7FC095381C0DD9ABCB4D3A12505D');
    Check(v <> nil);
    b := c.AllocateFromHex('8B364C36D24EA139');
    d := v.GreatestCommonDivisor(b);
    Check(not d.IsZero);
    CheckEqual(d.Compare(1), 0, 'gcd1');
    d.Release;
    d := b.GreatestCommonDivisor(v);
    Check(not d.IsZero);
    CheckEqual(d.Compare(1), 0, 'gcd2');
    d.Release;
    b.Release;
    v.Release;
    // some reference vectors taken from Mbed TLS tests
    a := c.AllocateFromHex('EFE021C2645FD1DC586E69184AF4A31E' +
      'D5F53E93B5F123FA41680867BA110131944FE7952E2517337780CB0DB80E61AA' +
      'E7C8DDC6C5C6AADEB34EB38A2F40D5E6');
    Check(a <> nil);
    v := c.AllocateFromHex('0066A198186C18C10B2F5ED9B522752A' +
      '9830B69916E535C8F047518A889A43A594B6BED27A168D31D4A52F88925AA8F5');
    v^.SetPermanent;
    Check(v <> nil);
    e := c.AllocateFromHex('B2E7EFD37075B9F03FF989C7C5051C20' +
      '34D2A323810251127E7BF8625A4F49A5F3E27F4DA8BD59C47D6DAABA4C8127BD' +
      '5B5C25763222FEFCCFC38B832366C29E');
    Check(e <> nil);
    d := a.Copy.Multiply(v.Copy);
    Check(d <> nil);
    CheckEqual(d.ToHexa, '602AB7ECA597A3D6B56FF9829A5E8B85' +
      '9E857EA95A03512E2BAE7391688D264AA5663B0341DB9CCFD2C4C5F421FEC814' +
      '8001B72E848A38CAE1C65F78E56ABDEFE12D3C039B8A02D6BE593F0BBBDA56F1' +
      'ECF677152EF804370C1A305CAF3B5BF130879B56C61DE584A0F53A2447A51E', 'Multiply');
    d.Release;
    d := a.Divide(v.Copy);
    Check(d <> nil);
    CheckEqual(d.ToHexa, '0256567336059E52CAE22925474705F39A94', 'Divide');
    d.Release;
    d := a.Modulo(v);
    Check(d <> nil);
    CheckEqual(d.ToHexa, '6613F26162223DF488E9CD48CC132C7A' +
      '0AC93C701B001B092E4E5B9F73BCD27B9EE50D0657C77F374E903CDFA4C642', 'Modulo');
    d.Release;
    d := a.Divide(v.Copy, bidDivide, @m);
    Check(d <> nil);
    CheckEqual(d.ToHexa, '0256567336059E52CAE22925474705F39A94', 'Divide2');
    Check(m <> nil);
    CheckEqual(m.ToHexa, '6613F26162223DF488E9CD48CC132C7A' +
      '0AC93C701B001B092E4E5B9F73BCD27B9EE50D0657C77F374E903CDFA4C642', 'Modulo2');
    d.Release;
    m.Release;
    d := c.ModPower(a.Copy, e.Copy, v);
    Check(d <> nil);
    CheckEqual(d.ToHexa, '36E139AEA55215609D2816998ED020BB' +
      'BD96C37890F65171D948E9BC7CBAA4D9325D24D6A3C12710F10A09FA08AB87', 'ModPower');
    d.Release;
    d := a.ModInverse(v);
    Check(d <> nil);
    CheckEqual(d.ToHexa, '3A0AAEDD7E784FC07D8F9EC6E3BFD5' +
      'C3DBA76456363A10869622EAC2DD84ECC5B8A74DAC4D09E03B5E0BE779F2DF61', 'ModInverse');
    d.Release;
    v.ResetPermanentAndRelease;
    a.Release;
    e.Release;
  finally
    c.Free;
  end;
  // TRsaPublicKey / TRsaPrivateKey support
  Check(pub1.ToDer = '');
  pub1.Modulus := HexToBin(_modulus);
  pub1.Exponent := HexToBin(_exponent);
  Check(pub1.Modulus <> '');
  Check(pub1.Exponent <> '');
  bin := pub1.ToDer;
  Check(bin <> '');
  CheckHash(bin, $8B24F964);
  pem := DerToPem(bin, pemRsaPublicKey);
  CheckHash(pem, $70E26930);
  Check(not pub2.FromDer(''));
  Check(pub2.FromDer(bin));
  Check(pub1.Modulus = pub2.Modulus);
  Check(pub1.Exponent = pub2.Exponent);
  Check(pub2.ToDer = bin);
  Check(pri1.ToDer = '');
  pri1.Version := 0;
  Check(not pri1.Match(pub1));
  Check(not pri1.Match(pub2));
  pri1.Modulus := HexToBin(_modulus);
  pri1.PublicExponent := HexToBin(_exponent);
  pri1.Prime1 := _modulus + CardinalToHex(Random32);
  pri1.Prime2 := _modulus + CardinalToHex(Random32);
  pri1.Exponent1 := _modulus + CardinalToHex(Random32);
  pri1.Exponent2 := _modulus + CardinalToHex(Random32);
  pri1.Coefficient := _modulus + CardinalToHex(Random32);
  Check(pri1.Modulus <> '');
  Check(pri1.PublicExponent <> '');
  bin := pri1.ToDer;
  Check(bin <> '');
  Check(not pri2.FromDer(''));
  pri2.Version := 10;
  Check(not pri2.Match(pub1));
  Check(not pri2.Match(pub2));
  Check(pri2.FromDer(bin));
  CheckEqual(pri2.Version, 0);
  Check(pri1.Modulus = pri2.Modulus);
  Check(pri1.PublicExponent = pri2.PublicExponent);
  Check(pri1.Match(pub1));
  Check(pri1.Match(pub2));
  Check(pri1.Prime1 = pri2.Prime1);
  Check(pri1.Prime2 = pri2.Prime2);
  Check(pri1.Exponent1 = pri2.Exponent1);
  Check(pri1.Exponent2 = pri2.Exponent2);
  Check(pri1.Coefficient = pri2.Coefficient);
  Check(pri2.Match(pub1));
  Check(pri2.Match(pub2));
  Check(pri2.ToDer = bin);
  Finalize(pub1);
  Check(pub1.Modulus = '');
  bin := PemToDer(_rsapub);
  Check(bin <> '');
  Check(pub1.FromDer(bin));
  pem := DerToPem(pub1.ToDer, pemPublicKey);
  CheckEqual(pem, _rsapub);
  Check(pub1.ToDer = bin);
  Finalize(pri1);
  bin := PemToDer(_rsapriv);
  Check(bin <> '');
  Check(pri1.FromDer(bin));
  pem := DerToPem(pri1.ToDer, pemPrivateKey);
  CheckEqual(pem, _rsapriv);
  Check(pri1.ToDer = bin);
  // validate TRsa engine with pre-computed RSA-2048 signature from FPC RTL
  c := TRsa.Create;
  try
    CheckEqual(c.ModulusBits, 0);
    Check(not c.HasPublicKey);
    Check(not c.HasPrivateKey);
    c.LoadFromPublicKeyHexa(_exponent + _modulus);
    Check(not c.E^.MatchKnownPrime(bspAll), 'primeE');
    Check(not c.M^.MatchKnownPrime(bspAll), 'primeM');
    CheckEqual(c.ModulusBits, 2048);
    CheckEqual(c.ModulusLen, 256);
    Check(c.HasPublicKey);
    Check(not c.HasPrivateKey);
    bin := HexToBin(_signature);
    hash := HexToBin(_hash);
    Check(c.Verify(pointer(hash), hfSHA256, bin), 'verif1');
  finally
    c.Free;
  end;
  // validate TRsa engine with an OpenSSL pre-computed keys pair
  c := TRsa.Create;
  try
    CheckEqual(c.ModulusBits, 0);
    Check(c.LoadFromPrivateKeyPem(_rsapriv));
    Check(c.HasPublicKey);
    Check(c.HasPrivateKey);
    for i := 1 to 10 do
      Check(c.CheckPrivateKey);
    CheckEqual(c.ModulusBits, 2048);
    CheckEqual(c.ModulusLen, 256);
    CheckEqual(c.E.ToText, '65537');
    txt := c.M.ToText;
    CheckEqual(length(txt), 617);
    CheckEqual(txt, '228561590982339343339762178744837209677820304476338116' +
      '14935715679263051702047340052509459228454206920157433871429033573223' +
      '62981125454186528716701009665304636863304209373234056331129942716615' +
      '45311853167788090326090261663765906881682367319449271532979157661068' +
      '33097601596252744701416117296222530353033794689377683060616584000143' +
      '09506019675474689924856013287210146431598119237902897045673210110539' +
      '12099391648892163162390570968199309924800917567824837650962989877163' +
      '67835782945087481326459513205359587937546847860455498952838630860659' +
      '26318752735376362852801874608944209521817136626323103992972869397440' +
      '6778249727285222779');
    CheckHash(txt, $9137B7B8);
    Check(not c.E^.MatchKnownPrime(bspAll), 'primeE'); // known are < 18,000
    Check(not c.M^.MatchKnownPrime(bspAll), 'primeM');
    Check(not c.P^.MatchKnownPrime(bspAll), 'primeP');
    Check(not c.Q^.MatchKnownPrime(bspAll), 'primeQ');
    Check(c.E^.IsPrime, 'IsPrimeE');
    Check(c.P^.IsPrime, 'IsPrimeP');
    Check(c.Q^.IsPrime, 'IsPrimeQ');
    Check(not c.M^.IsPrime, 'IsPrimeM');
    Check(c.SavePublicKeyDer = PemToDer(_rsapub));
    Check(c.SavePrivateKeyDer = PemToDer(_rsapriv));
    if CheckFailed(length(hash) = SizeOf(TSha256Digest)) then
      exit;
    timer.Start;
    for i := 1 to 10 do
    begin
      bin := c.Sign(pointer(hash), hfSHA256);
      Check(bin <> '');
      CheckHash(bin, $FBA40C03);
    end;
    NotifyTestSpeed('RS256 sign', 10, 0, @timer);
  finally
    c.Free;
  end;
  c := TRsa.Create;
  try
    Check(c.LoadFromPublicKeyPem(_rsapub));
    Check(c.HasPublicKey);
    Check(not c.HasPrivateKey);
    CheckEqual(c.E.ToText, '65537');
    CheckEqual(BigIntToText(c.E.Save), '65537');
    CheckEqual(c.M.ToText, txt);
    Check(c.SavePublicKeyDer = PemToDer(_rsapub));
    Check(c.SavePrivateKeyDer = '');
    timer.Start;
    for i := 1 to 100 do
      Check(c.Verify(pointer(hash), hfSHA256, bin), 'verifloop');
    NotifyTestSpeed('RS256 verify', 100, 0, @timer);
    CheckEqualHex(hash, _hash);
  finally
    c.Free;
  end;
  // validate RSA-PSS padding
  bin := PemToDer(_rsapriv);
  c := TRsaPss.Create;
  try
    Check(c.LoadFromPrivateKeyDer(bin));
    CheckEqual(c.ModulusBits, RSA_DEFAULT_GENERATION_BITS);
    CheckEqual(c.ModulusLen, 256);
    Check(c.HasPublicKey);
    Check(c.HasPrivateKey);
    Check(c.CheckPrivateKey);
    CheckEqual(length(hash), SizeOf(TSha256Digest));
    CheckHash(hash, $401CD1EB);
    timer.Start;
    for i := 1 to 10 do
      signed := c.Sign(pointer(hash), hfSHA256);
    NotifyTestSpeed('PS256 sign', 10, 0, @timer);
    CheckEqual(length(signed), c.ModulusLen, 'signpss');
    timer.Start;
    for i := 1 to 100 do
      Check(c.Verify(pointer(hash), hfSHA256, signed), 'verifpps');
    NotifyTestSpeed('PS256 verify', 100, 0, @timer);
    encrypted := c.Seal(bin); // bin is typically > 1KB long
    Check(encrypted <> '', 'Seal');
    Check(c.Open(encrypted) = bin, 'Open');
  finally
    c.Free;
  end;
end;

const
  // synopse.info official website current certificate
  _synopseinfo_pem =
    '-----BEGIN CERTIFICATE-----'#13#10 +
    'MIIE+DCCA+CgAwIBAgISA8yDqq/5weIc+vqAr+ZnbidMMA0GCSqGSIb3DQEBCwUA'#13#10 +
    'MDIxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1MZXQncyBFbmNyeXB0MQswCQYDVQQD'#13#10 +
    'EwJSMzAeFw0yMzA4MTMwNTQ4MDJaFw0yMzExMTEwNTQ4MDFaMBcxFTATBgNVBAMT'#13#10 +
    'DHN5bm9wc2UuaW5mbzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMb8'#13#10 +
    'Tt6EqYOBvQQzNXtBaLQ6lYiVflpEKE1UEZvt15NuRGexkvwjipUOom789+W+wBCS'#13#10 +
    'WarxpI66VNw8EaZPwWITP56aNLQbsKSydmqwRA8gL4TpRubW4hVnhDWJ1CJ/tOO+'#13#10 +
    'L0EQNBtuRwqbUukap1XWozhi6UxOK96+GST1wbEHFAyjqeL9az4l5n4DbCvuU01O'#13#10 +
    'UVwN7VMwoSCo4OrQMvZGshljfhuXI1Te8qKtppo55ERCTI0d8/Yf3uardNQj7k5e'#13#10 +
    'eGcnM5ytrbItw7T1W5S5JwrC4QwvG5aLu2sgS5TZ1Phg0jj8pID3cYBHcQxl5gjs'#13#10 +
    'j/GKo1T0HBQiGplgnqECAwEAAaOCAiEwggIdMA4GA1UdDwEB/wQEAwIFoDAdBgNV'#13#10 +
    'HSUEFjAUBggrBgEFBQcDAQYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4E'#13#10 +
    'FgQUDZCbaWfVSYee6ppAquMz60i6QBIwHwYDVR0jBBgwFoAUFC6zF7dYVsuuUAlA'#13#10 +
    '5h+vnYsUwsYwVQYIKwYBBQUHAQEESTBHMCEGCCsGAQUFBzABhhVodHRwOi8vcjMu'#13#10 +
    'by5sZW5jci5vcmcwIgYIKwYBBQUHMAKGFmh0dHA6Ly9yMy5pLmxlbmNyLm9yZy8w'#13#10 +
    'KQYDVR0RBCIwIIIMc3lub3BzZS5pbmZvghB3d3cuc3lub3BzZS5pbmZvMBMGA1Ud'#13#10 +
    'IAQMMAowCAYGZ4EMAQIBMIIBBQYKKwYBBAHWeQIEAgSB9gSB8wDxAHcAtz77JN+c'#13#10 +
    'Tbp18jnFulj0bF38Qs96nzXEnh0JgSXttJkAAAGJ7abcUgAABAMASDBGAiEAu/Sb'#13#10 +
    'MZPG953HDXiG3wLK5e7vTwECkAxC3tkvVaYQTWgCIQDt6IAX6DuU/St+sv/J2U6G'#13#10 +
    'aYLcsKoq4V1aCy3heEAW9gB2AK33vvp8/xDIi509nB4+GGq0Zyldz7EMJMqFhjTr'#13#10 +
    '3IKKAAABie2m3HoAAAQDAEcwRQIhAPpnLANKEAnxr8buMf7hxj6VmxKZ6e6mV77w'#13#10 +
    'LVT/CeU7AiA550VskF0sgiln+guqIG1WuKnPRR2+e6BM19Ejn3d8mTANBgkqhkiG'#13#10 +
    '9w0BAQsFAAOCAQEAptkYhAh/dNuNamhW3M/iSbciTqdv4RlpT2TcNktF9IsZPw5C'#13#10 +
    'CIObmhvYEpIeABxUkAUb3cRYn0qAvNZo/JHY2BvWEtuEfHHG4B/lmRHPp9lzseVa'#13#10 +
    'epTwiCOc5LB2XD212/CPGJ5ktHdo7lajmD8EMCx3m1617gszYDc3l4pj0JBA5viJ'#13#10 +
    'ozuUeje7XRD0Es29EvXkXjiYkx+P3GT115mkH2SxdqfaDiaXc5uymibEClUEEPoR'#13#10 +
    'V0wo7BzlVzPR+M/5FYCoVRDtJS0jVLtiKDL1HKnk0vifiCa9VDtmetdI23obh10K'#13#10 +
    '7rF4458RkO9sisEdNgHz0LNeGSYcrPjsD9ySSw=='#13#10 +
    '-----END CERTIFICATE-----'#13#10;
  // the Let's Encrypt certificate used as authority to sign _synopseinfo_pem
  _synopseauth_pem =
    '-----BEGIN CERTIFICATE-----'#13#10 +
    'MIIFFjCCAv6gAwIBAgIRAJErCErPDBinU/bWLiWnX1owDQYJKoZIhvcNAQELBQAw'#13#10 +
    'TzELMAkGA1UEBhMCVVMxKTAnBgNVBAoTIEludGVybmV0IFNlY3VyaXR5IFJlc2Vh'#13#10 +
    'cmNoIEdyb3VwMRUwEwYDVQQDEwxJU1JHIFJvb3QgWDEwHhcNMjAwOTA0MDAwMDAw'#13#10 +
    'WhcNMjUwOTE1MTYwMDAwWjAyMQswCQYDVQQGEwJVUzEWMBQGA1UEChMNTGV0J3Mg'#13#10 +
    'RW5jcnlwdDELMAkGA1UEAxMCUjMwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK'#13#10 +
    'AoIBAQC7AhUozPaglNMPEuyNVZLD+ILxmaZ6QoinXSaqtSu5xUyxr45r+XXIo9cP'#13#10 +
    'R5QUVTVXjJ6oojkZ9YI8QqlObvU7wy7bjcCwXPNZOOftz2nwWgsbvsCUJCWH+jdx'#13#10 +
    'sxPnHKzhm+/b5DtFUkWWqcFTzjTIUu61ru2P3mBw4qVUq7ZtDpelQDRrK9O8Zutm'#13#10 +
    'NHz6a4uPVymZ+DAXXbpyb/uBxa3Shlg9F8fnCbvxK/eG3MHacV3URuPMrSXBiLxg'#13#10 +
    'Z3Vms/EY96Jc5lP/Ooi2R6X/ExjqmAl3P51T+c8B5fWmcBcUr2Ok/5mzk53cU6cG'#13#10 +
    '/kiFHaFpriV1uxPMUgP17VGhi9sVAgMBAAGjggEIMIIBBDAOBgNVHQ8BAf8EBAMC'#13#10 +
    'AYYwHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMBIGA1UdEwEB/wQIMAYB'#13#10 +
    'Af8CAQAwHQYDVR0OBBYEFBQusxe3WFbLrlAJQOYfr52LFMLGMB8GA1UdIwQYMBaA'#13#10 +
    'FHm0WeZ7tuXkAXOACIjIGlj26ZtuMDIGCCsGAQUFBwEBBCYwJDAiBggrBgEFBQcw'#13#10 +
    'AoYWaHR0cDovL3gxLmkubGVuY3Iub3JnLzAnBgNVHR8EIDAeMBygGqAYhhZodHRw'#13#10 +
    'Oi8veDEuYy5sZW5jci5vcmcvMCIGA1UdIAQbMBkwCAYGZ4EMAQIBMA0GCysGAQQB'#13#10 +
    'gt8TAQEBMA0GCSqGSIb3DQEBCwUAA4ICAQCFyk5HPqP3hUSFvNVneLKYY611TR6W'#13#10 +
    'PTNlclQtgaDqw+34IL9fzLdwALduO/ZelN7kIJ+m74uyA+eitRY8kc607TkC53wl'#13#10 +
    'ikfmZW4/RvTZ8M6UK+5UzhK8jCdLuMGYL6KvzXGRSgi3yLgjewQtCPkIVz6D2QQz'#13#10 +
    'CkcheAmCJ8MqyJu5zlzyZMjAvnnAT45tRAxekrsu94sQ4egdRCnbWSDtY7kh+BIm'#13#10 +
    'lJNXoB1lBMEKIq4QDUOXoRgffuDghje1WrG9ML+Hbisq/yFOGwXD9RiX8F6sw6W4'#13#10 +
    'avAuvDszue5L3sz85K+EC4Y/wFVDNvZo4TYXao6Z0f+lQKc0t8DQYzk1OXVu8rp2'#13#10 +
    'yJMC6alLbBfODALZvYH7n7do1AZls4I9d1P4jnkDrQoxB3UqQ9hVl3LEKQ73xF1O'#13#10 +
    'yK5GhDDX8oVfGKF5u+decIsH4YaTw7mP3GFxJSqv3+0lUFJoi5Lc5da149p90Ids'#13#10 +
    'hCExroL1+7mryIkXPeFM5TgO9r0rvZaBFOvV2z0gp35Z0+L4WPlbuEjN/lxPFin+'#13#10 +
    'HlUjr8gRsI3qfJOQFy/9rKIJR0Y/8Omwt/8oTWgy1mdeHmmjk7j1nYsvC9JSQ6Zv'#13#10 +
    'MldlTTKB3zhThV1+XWYp6rjd5JW1zbVWEkLNxE7GJThEUG3szgBVGP7pSWTUTsqX'#13#10 +
    'nLRbwHOoq7hHwg=='#13#10 +
    '-----END CERTIFICATE-----'#13#10;
  // so that tests below will continue to work after 2023/11/11
  _synopse_date = 45203;

  // openssl req -new -x509 -days 36524 -key "ecdsa.key" -sha256 -out ecdsa.crt
  _selfsigned_pem =
    '-----BEGIN CERTIFICATE-----'#13#10 +
    'MIICKTCCAc+gAwIBAgIUWXUw/wRkokMf9BvR/WUrUylq+Y4wCgYIKoZIzj0EAwIw'#13#10 +
    'aTELMAkGA1UEBhMCRlIxEzARBgNVBAgMClNvbWUtU3RhdGUxFTATBgNVBAoMDFN5'#13#10 +
    'bm9wc2UgSW5mbzEXMBUGA1UECwwOQWRtaW5pc3RyYXRpb24xFTATBgNVBAMMDHN5'#13#10 +
    'bm9wc2UuaW5mbzAgFw0yMzEwMDMxMzMzNTBaGA8yMTIzMTAwMzEzMzM1MFowaTEL'#13#10 +
    'MAkGA1UEBhMCRlIxEzARBgNVBAgMClNvbWUtU3RhdGUxFTATBgNVBAoMDFN5bm9w'#13#10 +
    'c2UgSW5mbzEXMBUGA1UECwwOQWRtaW5pc3RyYXRpb24xFTATBgNVBAMMDHN5bm9w'#13#10 +
    'c2UuaW5mbzBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABF8MFUarr+ZmjZX58Lu2'#13#10 +
    'EPcnii0AKAQbIifHG/oaSUpx+VjqQZqGNEDQi2Onv3P/i/ZUfV4oJKW2cusnLHhc'#13#10 +
    'IJSjUzBRMB0GA1UdDgQWBBRR47U5GlhtqxVg6O/fdQGxLlgaqTAfBgNVHSMEGDAW'#13#10 +
    'gBRR47U5GlhtqxVg6O/fdQGxLlgaqTAPBgNVHRMBAf8EBTADAQH/MAoGCCqGSM49'#13#10 +
    'BAMCA0gAMEUCIHJbTCZPNEvNhQ4vDmAJd5Vj8FDEjx/JBlRKvVxAr+yVAiEA4GaI'#13#10 +
    'siC94x9I5sQUdpYL9Py/IxiRxJzKSD2WlOsytKc='#13#10 +
    '-----END CERTIFICATE-----'#13#10;

  _crl_pem = // from http://crl3.digicert.com/CloudflareIncECCCA-3.crl
    '-----BEGIN X509 CRL-----'#13#10 +
    'MIIBTDCB8wIBATAKBggqhkjOPQQDAjBKMQswCQYDVQQGEwJVUzEZMBcGA1UEChMQ'#13#10 +
    'Q2xvdWRmbGFyZSwgSW5jLjEgMB4GA1UEAxMXQ2xvdWRmbGFyZSBJbmMgRUNDIENB'#13#10 +
    'LTMXDTIzMTAxNjA0MDAyMFoXDTIzMTAyMzA0MDAyMFowRjAhAhALvx7d/AXWY+kC'#13#10 +
    'OhO32r3mFw0yMzA2MjAyMjA4MjlaMCECEAjvt5OCw8Z/b6We0DwiL+wXDTIzMDYy'#13#10 +
    'MDIyMTcwM1qgMDAuMB8GA1UdIwQYMBaAFKXON+rrsHUOlGeItEX62SQQh5YfMAsG'#13#10 +
    'A1UdFAQEAgIFSDAKBggqhkjOPQQDAgNIADBFAiEA86qhZ80IYV7PqU79mz8T9Afp'#13#10 +
    'b+30K8FXr3J7GyoEQDMCIH+7vnTK09Ryqvdp+p0OqLjLGen3So7Wy981pObr8FRE'#13#10 +
    '-----END X509 CRL-----'#13#10;

procedure TTestCoreCrypto._X509;
var
  bin, der: RawByteString;
  pem, sav, sn: RawUtf8;
  x, a: TX509;
  i: integer;
  nfo: TX509Parsed;
  crl: TX509Crl;
  num: QWord;
  ca, cint, cc: ICryptCert;
  st: ICryptStore;
  c: array[cuKeyAgreement .. cuTlsClient] of ICryptCert;
  chain, chain2: ICryptCertChain;
  cu: TCryptCertUsage;
  cus: TCryptCertUsages;
  utc: TDateTime;
  timer: TPrecisionTimer;
begin
  {$ifdef OSWINDOWS}
  Check(WinX509Parse(_synopseinfo_pem, nfo)); // validate our SSPI parser
  {$else}
  Check(X509Parse(_synopseinfo_pem, nfo)); // likely to be the OpenSSL parser
  {$endif OSWINDOWS}
  // validate with the synopse.info RSA certificate from Let's Encrypt
  x := TX509.Create;
  try
    CheckEqual(x.SerialNumber, '');
    CheckEqual(x.Signed.SerialNumber, '');
    Check(x.LoadFromPem(_synopseinfo_pem), 'synopse.info pem');
    Check(x.SignatureAlgorithm = xsaSha256Rsa);
    Check(x.Signed.SerialNumber <> '');
    CheckEqual(x.SerialNumber,
      '03:cc:83:aa:af:f9:c1:e2:1c:fa:fa:80:af:e6:67:6e:27:4c');
    CheckEqual(x.SerialNumber, nfo.Serial, 'nfo sn');
    CheckEqual(x.Signed.SerialNumberText,
      '330929475774275458452528262248458246563660');
    CheckEqual(x.Subject[xaCN], 'synopse.info');
    CheckEqual(x.Issuer[xaCN],  'R3');
    CheckEqual(x.Issuer[xaO],   'Let''s Encrypt');
    CheckEqual(x.Issuer[xaC],   'US');
    CheckEqual(x.SubjectDN,     'CN=synopse.info');
    CheckEqual(x.SubjectDN, nfo.SubjectDN);
    CheckEqual(x.IssuerDN,      'CN=R3, C=US, O=Let''s Encrypt');
    Check(x.Usages =
      [cuDigitalSignature, cuKeyEncipherment, cuTlsServer, cuTlsClient]);
    Check(x.Usages = nfo.Usage, 'nfo u');
    CheckEqual(x.Extension[xeSubjectAlternativeName],
      'synopse.info,www.synopse.info');
    CheckEqual(RawUtf8ArrayToCsv(x.SubjectAlternativeNames),
      'synopse.info,www.synopse.info');
    CheckEqual(x.Extension[xeSubjectKeyIdentifier],
      '0d:90:9b:69:67:d5:49:87:9e:ea:9a:40:aa:e3:33:eb:48:ba:40:12');
    CheckEqual(x.Extension[xeAuthorityKeyIdentifier],
      '14:2e:b3:17:b7:58:56:cb:ae:50:09:40:e6:1f:af:9d:8b:14:c2:c6');
    CheckEqual(x.Extension[xeAuthorityInformationAccess],
      'ocsp=http://r3.o.lencr.org,caIssuers=http://r3.i.lencr.org/');
    CheckSameTime(nfo.NotBefore, x.NotBefore, 'nfo nb');
    CheckSameTime(nfo.NotAfter, x.NotAfter, 'nfo na');
    Check(FindOther(x.Signed.ExtensionOther, '1.3.6.1.5.5.7.1.1') = '');
    Check(x.Signed.ExtensionOther = nil);
    CheckEqual(x.Extension[xeCertificatePolicies], '2.23.140.1.2.1');
    CheckEqual(x.Extension[xeGoogleSignedCertificateTimestamp], '');
    Check(x.Signed.ExtensionRaw[xeGoogleSignedCertificateTimestamp] <> '');
    Check(not x.IsSelfSigned);
    Check(not (cuCa in x.Usages), 'ca1');
    Check(x.Signed.SubjectPublicKeyAlgorithm = xkaRsa);
    CheckEqual(x.SignatureSecurityBits, 112, '2048=112');
    CheckEqual(x.FingerPrint,
      'd71ab49d1d5eab337c6e570b81b60ec6224fb26e');
    CheckEqual(x.FingerPrint(hfSHA256),
      'ea0f4f07abb685f2aaf864a28d9f275ac1e9bb29e82d6a8dc9111cd9162da4e7');
    CheckEqual(x.SubjectPublicKeyAlgorithm, '2048-bit RSA encryption');
    //writeln(x.PeerInfo);
    CheckHash(x.PeerInfo, $DF9578B9, 'peerinfo1a');
    Check(TX509Parse(_synopseinfo_pem, nfo), 'TX509Parse');
    CheckHash(nfo.PeerInfo, $DF9578B9, 'peerinfo1b'); // very same parser
    a := TX509.Create;
    try
      // check synopse.info against Let's Encrypt authority certificate
      Check(a.LoadFromPem(_synopseauth_pem), 'synopse auth');
      CheckEqual(a.Signed.SerialNumberText,
        '192961496339968674994309121183282847578');
      CheckEqual(a.Subject[xaCN], 'R3');
      CheckEqual(a.SubjectDN, 'CN=R3, C=US, O=Let''s Encrypt');
      CheckEqual(a.SubjectDN, x.IssuerDN);
      CheckEqual(a.IssuerDN,
        'CN=ISRG Root X1, C=US, O=Internet Security Research Group');
      CheckEqual(a.Extension[xeSubjectKeyIdentifier],
                 x.Extension[xeAuthorityKeyIdentifier]);
      Check(cuCa in a.Usages, 'ca2');
      Check(a.Usages = [cuCA, cuDigitalSignature, cuKeyCertSign,
        cuCrlSign, cuTlsServer, cuTlsClient]);
      Check(a.Signed.ExtensionOther = nil);
      CheckEqual(a.Extension[xeAuthorityInformationAccess],
        'caIssuers=http://x1.i.lencr.org/');
      CheckEqual(a.Extension[xeCertificatePolicies],
        '2.23.140.1.2.1,1.3.6.1.4.1.44947.1.1.1');
      for i := 1 to 1000 do // will use TX509.fLastVerifyAuthPublicKey cache
        Check(x.Verify(a, [], _synopse_date) = cvValidSigned, 'verify 1000');
      bin := x.Signed.ToDer;
      Check(a.Verify(pointer(x.SignatureValue), pointer(bin),
        length(x.SignatureValue), length(bin), [], _synopse_date) =
          cvValidSigned, 'verbuf syn');
      CheckEqual(a.SubjectPublicKeyAlgorithm, '2048-bit RSA encryption');
      CheckHash(a.PeerInfo, $FFE7466C, 'peerinfo2');
      CheckHash(ObjectToJson(a), $F7A82903);
      CheckHash(ObjectToJson(x), $7C73C7E0);
      CheckEqual(x.SignatureSecurityBits, 112, '2048=112');
    finally
      a.Free;
    end;
  finally
    x.Free;
  end;
  // validate with an OpenSSL generated ECC256 self-signed certificate
  x := TX509.Create;
  try
    Check(x.LoadFromPem(_selfsigned_pem), 'selfsigned pem');
    Check(x.SignatureAlgorithm = xsaSha256Ecc256);
    Check(x.Signed.SerialNumber <> '');
    CheckEqual(x.SerialNumber,
      '59:75:30:ff:04:64:a2:43:1f:f4:1b:d1:fd:65:2b:53:29:6a:f9:8e');
    CheckEqual(x.Signed.SerialNumberText,
      '510713633959117522632981676132379983048564472206');
    CheckEqual(x.Subject[xaCN], 'synopse.info');
    CheckEqual(x.Subject[xaO],  'Synopse Info');
    CheckEqual(x.Subject[xaC],  'FR');
    CheckEqual(x.Issuer[xaCN],  'synopse.info');
    CheckEqual(x.Issuer[xaO],   'Synopse Info');
    CheckEqual(x.Issuer[xaC],   'FR');
    CheckEqual(x.IssuerDN,
      'CN=synopse.info, C=FR, ST=Some-State, O=Synopse Info, OU=Administration');
    CheckEqual(x.Extension[xeSubjectAlternativeName], '');
    Check(x.SubjectAlternativeNames = nil);
    CheckEqual(x.Extension[xeSubjectKeyIdentifier],
      '51:e3:b5:39:1a:58:6d:ab:15:60:e8:ef:df:75:01:b1:2e:58:1a:a9');
    CheckEqual(x.Extension[xeAuthorityKeyIdentifier],
      '51:e3:b5:39:1a:58:6d:ab:15:60:e8:ef:df:75:01:b1:2e:58:1a:a9');
    Check(x.Usages = [cuCA]);
    Check(x.IsSelfSigned);
    Check(cuCa in x.Usages, 'ca3');
    Check(x.Signed.SubjectPublicKeyAlgorithm = xkaEcc256);
    CheckEqual(x.SignatureSecurityBits, 128, '256=128');
    CheckEqual(x.FingerPrint,
      'd5ae8d642967b01f806cd5c7c1af8b47ff7337bc');
    CheckEqual(x.FingerPrint(hfSHA256),
      'b75b01ca2d59f3283a6843b76d777ebe5b5d752f11c686879cf45248564cffa4');
    for i := 1 to 1000 do // will use TX509.fLastVerifyAuthPublicKey cache
      Check(x.Verify = cvValidSelfSigned, 'verify self');
    bin := x.Signed.ToDer;
    Check(x.Verify(pointer(x.SignatureValue), pointer(bin),
      length(x.SignatureValue), length(bin), [cvWrongUsage]) =
        cvValidSelfSigned, 'verbuf self');
    CheckEqual(x.SubjectPublicKeyAlgorithm, '256-bit prime256v1 ECDSA');
    CheckHash(x.PeerInfo, $BCB82372, 'peerinfo3');
    CheckHash(ObjectToJson(x), $BBCBCFEB);
    Check(AsnDecChunk(x.SaveToDer), 'x.SaveToDer');
  finally
    x.Free;
  end;
  // validate our X.509 CRL class
  crl := TX509Crl.Create;
  try
    Check(crl.SignatureAlgorithm = xsaNone);
    Check(crl.IsRevoked('08efb79382c3c67f6fa59ed03c222fec') = crrNotRevoked);
    Check(crl.LoadFromPem(_crl_pem));
    Check(crl.SignatureAlgorithm = xsaSha256Ecc256);
    CheckEqual(DateTimeToIso8601Text(crl.ThisUpdate), '2023-10-16T04:00:20');
    CheckEqual(DateTimeToIso8601Text(crl.NextUpdate), '2023-10-23T04:00:20');
    CheckEqual(crl.CrlNumber, 1352);
    CheckEqual(crl.AuthorityKeyIdentifier,
      'a5:ce:37:ea:eb:b0:75:0e:94:67:88:b4:45:fa:d9:24:10:87:96:1f');
    CheckEqual(RawUtf8ArrayToCsv(crl.Revoked),
      '0b:bf:1e:dd:fc:05:d6:63:e9:02:3a:13:b7:da:bd:e6,' +
      '08:ef:b7:93:82:c3:c6:7f:6f:a5:9e:d0:3c:22:2f:ec');
    Check(crl.IsRevoked('08efb79382c3c67f6fa59ed03c222fec') = crrUnspecified);
    Check(crl.IsRevoked('08efb79382c3c67f6fa59ed03c222feb') = crrNotRevoked);
    CheckEqual(crl.IssuerDN,
      'CN=Cloudflare Inc ECC CA-3, C=US, O=Cloudflare, O=Inc.');
    der := crl.SaveToDer;
    Check(AsnDecChunk(der), 'crl.SaveToDer');
    pem := DerToPem(der, pemCrl);
    CheckEqual(pem, _crl_pem);
    crl.AfterModified; // force regenerate DER/PEM
    pem := DerToPem(crl.SaveToDer, pemCrl);
    CheckEqual(pem, _crl_pem);
  finally
    crl.Free;
  end;
  crl := TX509Crl.Create;
  try
    Check(crl.LoadFromPem(pem));
    Check(crl.SignatureAlgorithm = xsaSha256Ecc256);
    CheckEqual(DateTimeToIso8601Text(crl.ThisUpdate), '2023-10-16T04:00:20');
    CheckEqual(DateTimeToIso8601Text(crl.NextUpdate), '2023-10-23T04:00:20');
    CheckEqual(crl.CrlNumber, 1352);
    CheckEqual(crl.AuthorityKeyIdentifier,
      'a5:ce:37:ea:eb:b0:75:0e:94:67:88:b4:45:fa:d9:24:10:87:96:1f');
    CheckEqual(RawUtf8ArrayToCsv(crl.Revoked),
      '0b:bf:1e:dd:fc:05:d6:63:e9:02:3a:13:b7:da:bd:e6,' +
      '08:ef:b7:93:82:c3:c6:7f:6f:a5:9e:d0:3c:22:2f:ec');
    crl.Clear;
    Check(crl.SignatureAlgorithm = xsaNone);
    CheckEqual(crl.CrlNumber, 0);
    CheckEqual(crl.AuthorityKeyIdentifier, '');
  finally
    crl.Free;
  end;
  // create some prime256v1 certificates for PKI testing
  ca := CryptCertX509[caaES256].Generate(
    [cuCA, cuCrlSign, cuKeyCertSign], 'trust anchor');
  cint := ca.CertAlgo.Generate([cuCrlSign, cuKeyCertSign], 'intermediate', ca);
  Check(ca.Verify(nil) = cvValidSelfSigned);
  Check(cint.Verify(ca) = cvValidSigned);
  for cu := low(c) to high(c) do
  begin
    cus := [];
    include(cus, cu);
    c[cu] := ca.CertAlgo.Generate(cus, ShortStringToUtf8(ToText(cu)^), cint);
    Check(c[cu].Verify(ca) = cvUnknownAuthority);
    Check(c[cu].Verify(cint) = cvValidSigned);
  end;
  SetLength(chain, 3); // create an unordered chain - should be consolidated
  chain[1] := ca.CertAlgo.Generate([cuKeyCertSign], 'cint1', cint);
  chain[2] := ca.CertAlgo.Generate([cuKeyCertSign], 'cint2', chain[1]);
  chain[0] := ca.CertAlgo.Generate([cuTlsClient], 'www.toto.com', chain[2]);
  // validate a X.509 CRL generation and signature with a temporay authority
  crl := TX509Crl.Create;
  try
    CheckEqual(crl.CrlNumber, 0);
    Check(crl.IsRevoked('abcd') = crrNotRevoked);
    Check(crl.AddRevocation('ab:cd', crrCompromised));
    Check(crl.IsRevoked('abcd') = crrCompromised);
    Check(crl.IsRevoked('abce') = crrNotRevoked);
    Check(crl.AddRevocation('ef:01', crrReplaced));
    Check(crl.IsRevoked('EF:01') = crrReplaced);
    CheckEqual(RawUtf8ArrayToCsv(crl.Revoked), 'ab:cd,ef:01');
    Check(crl.SignatureAlgorithm = xsaNone);
    Check(crl.VerifyCryptCert(ca) = cvInvalidSignature);
    Check(crl.VerifyCryptCert(cint) = cvInvalidSignature);
    CheckEqual(crl.AuthorityKeyIdentifier, '');
    sn := c[cuNonRepudiation].GetSerial;
    Check(crl.AddRevocation(sn, crrCompromised));
    num := Random64 shr 1;
    crl.SignCryptCert(cint, num);
    CheckEqual(crl.Issuer[xaCN], 'intermediate');
    Check(crl.SignatureAlgorithm = xsaSha256Ecc256);
    Check(IdemPropNameU(crl.AuthorityKeyIdentifier, cint.GetSubjectKey));
    Check(crl.VerifyCryptCert(ca) = cvUnknownAuthority);
    Check(crl.VerifyCryptCert(cint) = cvValidSigned);
    bin := crl.SaveToDer;
    pem := crl.SaveToPem;
    Check(pem <> '');
    Check(PemToDer(pem) = bin);
    CheckEqual(crl.CrlNumber, num);
  finally
    crl.Free;
  end;
  crl := TX509Crl.Create;
  try
    CheckEqual(crl.Issuer[xaCN], '');
    Check(crl.LoadFromDer(bin));
    CheckEqual(crl.CrlNumber, num);
    Check(crl.SignatureAlgorithm = xsaSha256Ecc256);
    Check(IdemPropNameU(crl.AuthorityKeyIdentifier, cint.GetSubjectKey));
    CheckEqual(crl.Issuer[xaCN], 'intermediate');
    CheckEqual(RawUtf8ArrayToCsv(crl.Revoked), 'ab:cd,ef:01,' + sn);
    Check(crl.VerifyCryptCert(cint) = cvValidSigned);
    Check(crl.Verify(cint.Handle) = cvValidSigned);
    Check(crl.IsRevoked('abce') = crrNotRevoked);
    Check(crl.IsRevoked('ab:CD') = crrCompromised);
    Check(crl.IsRevoked('ef01') = crrReplaced);
    Check(crl.IsRevoked(sn) = crrCompromised);
    CheckEqual(crl.SaveToPem, pem);
    crl.AfterModified; // force regenerate DER/PEM
    CheckEqual(crl.SaveToPem, pem);
  finally
    crl.Free;
  end;
  // validate our PKI
  Check(cint.Verify(ca) = cvValidSigned);
  for cu := low(c) to high(c) do
    Check(c[cu].Verify(cint) = cvValidSigned);
  st := CryptStoreX509.New;
  CheckEqual(st.Count, 0);
  CheckEqual(st.CrlCount, 0);
  CheckEqual(length(st.Add([ca, cint])), 2);
  CheckEqual(st.Count, 2);
  CheckEqual(st.CrlCount, 0);
  Check(st.IsValid(ca) = cvValidSelfSigned);
  Check(st.IsValid(cint) = cvValidSigned);
  for cu := low(c) to high(c) do
    Check(st.IsValid(c[cu]) = cvValidSigned);
  Check(st.IsValidChain(chain) = cvValidSigned, 'chain consolidate');
  chain2 := chain;
  ChainAdd(chain2, c[cuNonRepudiation]);
  Check(st.IsValidChain(chain2) = cvValidSigned, 'chain ignore irrelevant');
  Check(st.FindOne('IntermediatE', ccmSubjectCN) = cint);
  Check(st.FindOne('Trust Anchor', ccmSubjectCN) = ca);
  Check(st.FindOne('TRUST Anchor', ccmIssuerCN) = ca);
  Check(st.Cache.FindOne('IntermediatE', ccmSubjectCN) = nil);
  Check(st.Cache.FindOne('Trust Anchor', ccmSubjectCN) = nil);
  Check(st.Cache.FindOne('TRUST Anchor', ccmIssuerCN) = nil);
  sav := st.Save;
  // try store certificate persistence as PEM
  st := CryptStoreX509.NewFrom(sav);
  CheckEqual(st.Count, 2);
  CheckEqual(st.CrlCount, 0);
  Check(st.IsValid(cint) = cvValidSigned);
  Check(st.IsValid(ca) = cvValidSelfSigned);
  Check(st.IsValidChain(chain) = cvValidSigned);
  CheckEqual(st.Save, sav);
  for cu := low(c) to high(c) do
  begin
    Check(st.IsRevoked(c[cu]) = crrNotRevoked);
    Check(st.IsValid(c[cu]) = cvValidSigned);
  end;
  // ensure dates are taken into account
  utc := NowUtc; // is likely to be cached on server side
  utc := utc - 100;
  for cu := low(c) to high(c) do
    Check(st.IsValid(c[cu], utc) = cvInvalidDate);
  utc := utc + 100;
  for cu := low(c) to high(c) do
    Check(st.IsValid(c[cu], utc) = cvValidSigned);
  utc := utc + 1000;
  for cu := low(c) to high(c) do
    Check(st.IsValid(c[cu], utc) = cvInvalidDate);
  utc := utc - 900;
  for cu := low(c) to high(c) do
    Check(st.IsValid(c[cu], utc) = cvValidSigned);
  // add a signed CRL generated by the CA
  Check(st.AddFromBuffer(pem) = nil, 'add crl');
  CheckEqual(st.Count, 2);
  CheckEqual(st.CrlCount, 1);
  for i := 1 to 5 do
    for cu := low(c) to high(c) do
      if cu <> cuNonRepudiation then
      begin
        Check(st.IsRevoked(c[cu]) = crrNotRevoked, 'r1');
        Check(st.IsValid(c[cu]) = cvValidSigned, 'r2');
      end
      else
      begin
        Check(st.IsRevoked(c[cu]) = crrCompromised, 'r3');
        Check(st.IsValid(c[cu]) = cvRevoked, 'r4');
      end;
  // should not affect the chain
  Check(st.IsValidChain(chain) = cvValidSigned);
  chain2 := chain;
  for cu := low(c) to high(c) do
    ChainAdd(chain2, c[cu]);
  Check(st.IsValidChain(chain2) = cvValidSigned, 'chain irrelevants');
  // benchmark a typical load DER + validate chain for a certificate
  bin := c[cuTlsClient].Save; // as retrieved e.g. from a TLS handshake
  timer.Start;
  for i := 1 to 10000 do
  begin
    cc := st.Cache.Load(bin);
    Check(cc <> nil, 'cc load');
    Check(st.IsValid(cc, utc) = cvValidSigned, 'cc valid');
  end;
  NotifyTestSpeed('x509-pki Load+IsValid', 10000, 0, @timer);
  Check(st.Cache.FindOne('Intermediate', ccmIssuerCN) = cc);
  // revoke a certificate (in the midddle of the chain)
  Check(st.IsValidChain(chain) = cvValidSigned);
  Check(st.IsValid(chain[0]) = cvUnknownAuthority);
  Check(st.IsValid(chain[2]) = cvUnknownAuthority);
  Check(st.IsValid(chain[1]) = cvValidSigned);
  st.Revoke(chain[2], crrServerCompromised);
  Check(st.IsValidChain(chain) = cvRevoked);
  Check(st.IsValid(chain[0]) = cvUnknownAuthority);
  Check(st.IsValid(chain[2]) = cvRevoked);
  Check(st.IsValid(chain[1]) = cvValidSigned);
  Check(st.IsRevoked(chain[0]) = crrNotRevoked);
  Check(st.IsRevoked(chain[1]) = crrNotRevoked);
  Check(st.IsRevoked(chain[2]) = crrServerCompromised);
  // ensure non-CA Revoke() as properly persistence as unsigned PEM CRL
  sav := st.Save;
  st := CryptStoreX509.NewFrom(sav);
  Check(PosEx('Signature: none', sav) <> 0, 'unsigned CRL');
  CheckEqual(st.Count, 2);
  CheckEqual(st.CrlCount, 2);
  Check(st.IsValid(cint) = cvValidSigned);
  Check(st.IsValid(ca) = cvValidSelfSigned);
  Check(st.IsValidChain(chain) = cvRevoked);
  Check(st.IsValid(chain[0]) = cvUnknownAuthority);
  Check(st.IsValid(chain[2]) = cvRevoked);
  Check(st.IsValid(chain[1]) = cvValidSigned);
  CheckEqual(st.Save, sav);
  // revoke the head of our chain
  st.Revoke(chain[0], crrCompromised);
  Check(st.IsValidChain(chain) = cvRevoked);
  Check(st.IsValid(chain[0]) = cvRevoked);
  Check(st.IsValid(chain[2]) = cvRevoked);
  Check(st.IsValid(chain[1]) = cvValidSigned);
  CheckEqual(st.Count, 2);
  CheckEqual(st.CrlCount, 3);
  Check(st.IsRevoked(chain[0]) = crrCompromised);
  Check(st.IsRevoked(chain[1]) = crrNotRevoked);
  Check(st.IsRevoked(chain[2]) = crrServerCompromised);
  sav := st.Save;
  // validate store persistence once again
  st := CryptStoreX509.NewFrom(sav);
  CheckEqual(st.Count, 2);
  CheckEqual(st.CrlCount, 3);
  CheckEqual(st.Save, sav);
  Check(st.IsValidChain(chain) = cvRevoked);
  Check(st.IsValid(chain[0]) = cvRevoked);
  Check(st.IsValid(chain[2]) = cvRevoked);
  Check(st.IsValid(chain[1]) = cvValidSigned);
  Check(st.IsRevoked(chain[0]) = crrCompromised);
  Check(st.IsRevoked(chain[1]) = crrNotRevoked);
  Check(st.IsRevoked(chain[2]) = crrServerCompromised);
  // revoke the CA, and observe the whole pyramid collapse
  Check(st.Revoke(ca, crrAuthorityCompromised));
  Check(st.IsValidChain(chain) = cvRevoked);
  Check(st.IsValid(chain[0]) = cvRevoked);
  Check(st.IsValid(chain[2]) = cvRevoked);
  Check(st.IsValid(chain[1]) = cvRevoked);
  for cu := low(c) to high(c) do
    Check(st.IsValid(c[cu]) = cvRevoked);
  // but individual certificates revocation state was not affected
  Check(st.IsRevoked(chain[0]) = crrCompromised);
  Check(st.IsRevoked(chain[1]) = crrNotRevoked);
  Check(st.IsRevoked(chain[2]) = crrServerCompromised);
end;
(*
var
  i, v, v2, n, c, d: PtrInt;
  p: PByte;

  procedure Add(v: PtrInt); // 4-bit encoding
  begin
    inc(n);
    if n and 1 = 1 then
      c := v
    else
      write(c + v shl 4, ',');
  end;

initialization // compress BIGINT_PRIMES[] from 8-bit deltas above
  n := 0;
  c := 0;
  for i := 2 to high(BIGINT_PRIMES_DELTA_BYTE) do
  begin
    v := BIGINT_PRIMES_DELTA_BYTE[i] shr 1; // all deltas are >= 2
    if v > 15 then // stored as (0, delta-15)
    begin
      Add(0);
      dec(v, 15);
    end;
    Add(v);
    if n and 63 = 0 then
      writeln;
  end;
  writeln(n);
*)
end.

