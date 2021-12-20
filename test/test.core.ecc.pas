/// regression tests for Elliptic-Curve Cryptography
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.ecc;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.test,
  mormot.crypt.secure,
  mormot.crypt.jwt,
  mormot.crypt.ecc256r1,
  mormot.crypt.ecc,
  mormot.core.perf,
  mormot.crypt.openssl,
  {$ifdef USE_OPENSSL}
  mormot.lib.openssl11,
  {$endif USE_OPENSSL}
  mormot.app.console,
  mormot.tools.ecc;

type
  /// this test case will test ECDH and ECDSA cryptography as implemented
  // in the mormot.crypt.ecc and mormot.crypt.ecc256r1 unit
  TTestCoreEcc = class(TSynTestCase)
  protected
    pub: array of TEccPublicKey;
    pubunc: array of TEccPublicKeyUncompressed;
    priv: array of TEccPrivateKey;
    sign: array of TEccSignature;
    hash: TEccHash;
  published
    /// avoid regression among platforms and compilers
    procedure ReferenceVectors;
    /// ECC private/public keys generation, signature, verifiaction and secret
    procedure ECC;
    /// ECDSA certificates chains and digital signatures
    procedure CertificatesAndSignatures;
    /// run most commands of the ECC tool
    procedure EccCommandLineTool;
    /// ECDHE stream protocol
    procedure ECDHEStreamProtocol;
    {$ifdef USE_OPENSSL}
    procedure _OpenSSL;
    {$endif USE_OPENSSL}
  end;



implementation

{ TTestCoreEcc }

const
  {$ifdef CPU64}
  ECC_COUNT = 301;
  {$else}
  ECC_COUNT = 51;
  {$endif CPU64}

procedure TTestCoreEcc.ReferenceVectors;
var
  pr1, pr2: TEccPrivateKey;
  pu1, pu2: TEccPublicKey;
  h1, h2: TEccHash;
  si1, si2: TEccSignature;
  s1, s2, s3: TEccSecretKey;
begin
  SetLength(pub, ECC_COUNT);
  SetLength(pubunc, ECC_COUNT);
  SetLength(priv, ECC_COUNT);
  SetLength(sign, ECC_COUNT);
  TAesPrng.Main.FillRandom(@hash, SizeOf(hash));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    'DC5B79BD481E536DD8075D06C18D42B25B557B4671017BA2A26102B69FD9B70A'),
    @pr1, SizeOf(pr1)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    '024698753E25650A3129320A7DDBA43D56051F4BEE3653897960A61FBC92AB24A5'),
    @pu1, SizeOf(pu1)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    'CFA96FAC873F522897000815BE96338DE8D355D5F495DD5C5A4FEF0AEDB66D5B'),
    @pr2, SizeOf(pr2)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    '0298D0D01FCE73146C10CD05E08BEA573BEE4EFC56D5EBAAC64B32672C8FAC1502'),
    @pu2, SizeOf(pu2)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    '9509D00BBBA2308445BC73311C3887E935183F65D361D4C39E2FA432B7168599'),
    @h1, SizeOf(h1)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    'F04CD0AA3D40433C51F35D07DBF4E11C91C922791A8BA7B930B5C30716D8B26E4B65EFBF' +
    'BDC0526A94ABDAA31130248F0413AC33D5BFA903E09847AAF42FD043'), @si1, SizeOf(si1)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    '3366C112F95B2F52836171CAD3F3441C4B3C75348859092B200DE5024CB0C91B'),
    @h2, SizeOf(h2)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    'EEEF6F1D0A590BFC72B9D7DC0DB4BF36A8928DA2B8078FEE567808BB082525438CF68546' +
    '26E17FBB28528450E50E43AB2598ED2CD3ACC7B43865BEB843452713'), @si2, SizeOf(si2)));
  Check(mormot.core.text.HexToBin(PAnsiChar(
    '51A0C8018EC725F9B9F821D826FEEC4CAE8843066685522F1961D25935EAA39E'),
    @s1, SizeOf(s1)));
  Check(Ecc256r1Verify(pu1, h1, si1));
  Check(Ecc256r1Verify(pu2, h2, si2));
  FillZero(s2);
  Check(Ecc256r1SharedSecret(pu1, pr2, s2));
  Check(IsEqual(s1, s2));
  Check(CompareMem(@s1, @s2, SizeOf(s1)));
  FillZero(s3);
  Check(Ecc256r1SharedSecret(pu2, pr1, s3));
  Check(IsEqual(s1, s3));
  Check(CompareMem(@s1, @s3, SizeOf(s1)));
  Check(ecdsa_verify_pas(pu1, h1, si1));
  Check(ecdsa_verify_pas(pu2, h2, si2));
  FillZero(s2);
  Check(ecdh_shared_secret_pas(pu1, pr2, s2));
  Check(IsEqual(s1, s2));
  FillZero(s3);
  Check(ecdh_shared_secret_pas(pu2, pr1, s3));
  Check(IsEqual(s1, s3));
end;

procedure TTestCoreEcc.ECC;
var
  i: integer;
  timer: TPrecisionTimer;
  sec1, sec2: TEccSecretKey;
begin
  Check(ecc_make_key_pas(pub[0], priv[0])); // also validate our pascal code
  timer.Start;
  for i := 1 to ECC_COUNT - 1 do
    Check(Ecc256r1MakeKey(pub[i], priv[i])); // may be OpenSSL
  NotifyTestSpeed('Ecc256r1MakeKey', ECC_COUNT - 1, 0, @timer);
  ecc_uncompress_key_pas(pub[0], pubunc[0]); // also validate our pascal code
  timer.Start;
  for i := 1 to ECC_COUNT - 1 do
    Ecc256r1Uncompress(pub[i], pubunc[i]); // may be OpenSSL
  NotifyTestSpeed('Ecc256r1Uncompress', ECC_COUNT - 1, 0, @timer);
  timer.Start;
  for i := 0 to ECC_COUNT - 2 do
    Check(Ecc256r1Sign(priv[i], hash, sign[i]));
  NotifyTestSpeed('Ecc256r1Sign', ECC_COUNT - 1, 0, @timer);
  Check(ecdsa_sign_pas(priv[ECC_COUNT - 1], hash, sign[ECC_COUNT - 1]), 's');
  Check(ecdsa_verify_pas(pub[7], hash, sign[7]));
  timer.Start;
  for i := 0 to ECC_COUNT - 1 do
    if i <> 7 then
      check(Ecc256r1Verify(pub[i], hash, sign[i]));
  NotifyTestSpeed('Ecc256r1Verify', ECC_COUNT - 1, 0, @timer);
  Check(ecdsa_verify_uncompressed_pas(pubunc[7], hash, sign[7]), 'vu');
  timer.Start;
  for i := 0 to ECC_COUNT - 1 do
    if i <> 7 then
      check(Ecc256r1VerifyUncomp(pubunc[i], hash, sign[i]));
  NotifyTestSpeed('Ecc256r1VerifyUncomp', ECC_COUNT - 1, 0, @timer);
  timer.Start;
  for i := 0 to ECC_COUNT - 2 do
  begin
    check(Ecc256r1SharedSecret(pub[i], priv[i + 1], sec1));
    check(Ecc256r1SharedSecret(pub[i + 1], priv[i], sec2));
    check(IsEqual(sec1, sec2));
  end;
  NotifyTestSpeed('Ecc256r1SharedSecret', (ECC_COUNT - 2) * 2, 0, @timer);
end;

procedure TTestCoreEcc.CertificatesAndSignatures;
const
  PUBPRIV64: RawUtf8 =
    'AQAKAAoAFAAp49cdwmwTSgk7ocIs+iWCLVmLFDvnzMbgAAAAAAAAACnj1x3CbBN' +
    'KCTuhwiz6JYItWYsUO+fMxuAAAAAAAAAAAgm92LeP/SogOQAmFAKppFHFPPn1vRERJ1dwk5y8' +
    'AloD66iKgas4FCX8yprik12Unvk3K45kS1tIkga7U273SBAoDj5WP1ENURn7znVgPm5UPrMZO' +
    'vaZNdUuDPlCy1uzNJeQTIkgAAAAnddux+slXpcupBr3m2g/2skZyPIT0Y2mk9As06J2mMY=';
  PUBPRIVJSON: RawUtf8 =
    '{"Version":1,"Serial":"29E3D71DC26C134A093BA1C22CFA2582",' +
     '"Issuer":"synopse.info","IssueDate":"2016-08-11","ValidityStart":' +
     '"2016-08-11","ValidityEnd":"2016-08-21","AuthoritySerial":' +
     '"29E3D71DC26C134A093BA1C22CFA2582","AuthorityIssuer":"synopse.info",' +
     '"IsSelfSigned":true,"Base64":"';
const
  // Generated by tests
  MYPRIVKEY: array[0..255] of byte = ($39, $EC, $C0, $0D, $D0, $ED, $47, $DC,
    $2A, $14, $72, $80, $D7, $E2, $48, $C1, $87, $6F, $11, $60, $5C, $77, $1C,
    $C6, $9B, $A8, $AD, $FD, $95, $17, $45, $A3, $2F, $A0, $4A, $B3, $AF, $B4,
    $27, $13, $85, $16, $E0, $6C, $F7, $75, $F1, $C5, $7C, $75, $6D, $34, $8C,
    $8F, $AB, $AD, $AA, $EA, $94, $5F, $A7, $B6, $F1, $E3, $D4, $0E, $3D, $FE,
    $96, $ED, $5C, $53, $90, $98, $60, $1A, $85, $9D, $BF, $70, $0F, $B2, $9D,
    $9B, $B2, $66, $36, $26, $F7, $FD, $3A, $5F, $DC, $AE, $67, $3B, $8E, $C4,
    $61, $71, $5D, $F6, $1F, $9A, $2A, $20, $A0, $C9, $F8, $0D, $FB, $EE, $3A,
    $17, $FA, $50, $FA, $AB, $EF, $72, $F8, $1D, $55, $CA, $1F, $6A, $86, $CB,
    $AA, $0E, $58, $01, $1F, $8E, $6F, $CC, $EA, $ED, $98, $1B, $4D, $1F, $85,
    $89, $74, $F6, $03, $FB, $9F, $1A, $50, $95, $F2, $8C, $79, $78, $9A, $94,
    $5C, $7F, $2E, $CA, $06, $3E, $E7, $93, $7F, $93, $8F, $64, $6D, $27, $A4,
    $B3, $81, $CE, $DB, $B1, $2A, $28, $79, $B6, $22, $87, $9F, $91, $01, $53,
    $6B, $B1, $AF, $91, $60, $87, $8F, $61, $87, $55, $D0, $FF, $33, $73, $05,
    $FD, $39, $DC, $A9, $B7, $EA, $D3, $72, $D6, $A6, $00, $98, $D2, $91, $96,
    $19, $A9, $1D, $7C, $6C, $9B, $F8, $D0, $50, $31, $52, $C3, $D8, $1D, $9B,
    $54, $1B, $09, $8C, $CE, $36, $1B, $4F, $2A, $EC, $98, $9B, $A2, $F7, $C4,
    $A8, $78, $AD, $DA, $B5, $56, $89, $67);
  MYPRIVKEY_LEN = SizeOf(MYPRIVKEY);
  MYPRIVKEY_ROUNDS = 100;
  MYPRIVKEY_PASS = '123456';
  MYPRIVKEY_CYPH = '4e/QgInP';
var
  selfsignedroot, secret: TEccCertificateSecret;
  cert: TEccCertificate;
  sav, json, serial: RawUtf8;
  bin: RawByteString;
  json1, json2, jsonchain: RawUtf8;
  chain: TEccCertificateChain;
  sign: TEccSignatureCertified;
  signcontent: TEccSignatureCertifiedContent;
begin
  chain := TEccCertificateChain.Create;
  try
    check(chain.Count = 0);
    selfsignedroot := TEccCertificateSecret.CreateNew(nil, 'synopse.info', 10);
    check(selfsignedroot.IsSelfSigned);
    check(selfsignedroot.HasSecret);
    check(chain.IsValid(nil) = ecvBadParameter);
    check(chain.IsValid(selfsignedroot) = ecvValidSelfSigned);
    check(chain.Add(nil) = -1);
    check(chain.Add(selfsignedroot) = -1);
    check(chain.Count = 0);
    check(chain.AddSelfSigned(selfsignedroot) = 0);
    check(chain.Count = 1);
    check(not chain.IsValidCached);
    chain.IsValidCached := true;
    selfsignedroot := TEccCertificateSecret.CreateNew(nil, 'mORMot.net', 0);
    serial := selfsignedroot.Serial;
    check(length(serial) = 32);
    check(selfsignedroot.IsSelfSigned);
    check(selfsignedroot.HasSecret);
    check(chain.IsValid(nil) = ecvBadParameter);
    check(chain.IsValid(selfsignedroot) = ecvValidSelfSigned);
    check(chain.Add(nil) = -1);
    check(chain.Add(selfsignedroot) = -1);
    check(chain.Count = 1);
    check(chain.AddSelfSigned(selfsignedroot) = 1);
    check(chain.Count = 2);
    secret := TEccCertificateSecret.CreateNew(selfsignedroot, 'google.fr');
    check(chain.Count = 2);
    check(secret.HasSecret);
    check(not secret.IsSelfSigned);
    check(chain.IsValid(secret) = ecvValidSigned);
    json1 := ObjectToJson(secret);
    sav := secret.PublicToBase64;
    cert := TEccCertificate.CreateFromBase64(sav);
    check(cert.Serial = secret.Serial);
    check(not cert.IsSelfSigned);
    check(chain.IsValid(cert) = ecvValidSigned);
    check(cert.Issuer = 'google.fr');
    check(cert.AuthorityIssuer = 'mormot.net');
    check(chain.Add(cert) = 2);
    check(chain.Count = 3);
    check(chain.GetBySerial(cert.Content.Head.Signed.Serial) = cert);
    json2 := ObjectToJson(cert);
    CheckEqual(json1, json2, 'serialization trim private key');
    secret.Free;
    inc(sav[10]); // corrupt
    cert := TEccCertificate.Create;
    check(not cert.FromBase64(sav));
    check(chain.IsValid(cert) = ecvCorrupted);
    secret := TEccCertificateSecret.CreateFromBase64(PUBPRIV64);
    check(secret.HasSecret);
    check(secret.IsSelfSigned);
    check(chain.IsValidRaw(secret.Content, true) = ecvValidSelfSigned);
    check(secret.Serial <> cert.Serial);
    check(secret.Serial = '29E3D71DC26C134A093BA1C22CFA2582');
    json1 := ObjectToJson(secret);
    check(json1 <> json2);
    json2 := PUBPRIVJSON + copy(PUBPRIV64, 1,
      posEx('y1uzNJeQTIk', PUBPRIV64) + 10) + 'AAAAA"}';
    check(json1 = json2, 'no private key');
    jsonchain := ObjectToJson(chain);
    check(length(jsonchain) = 1545);
    sav := secret.SaveToSource('MyPrivKey', 'Generated by tests', '123456');
//  FileFromString(sav,'privkey.pas');
    check(length(sav) = 1467);
    secret.Free;
    cert.Free;
    check(selfsignedroot.SaveToSecureFile('pass', '.', 64, 1000));
    secret := TEccCertificateSecret.CreateNew(selfsignedroot, 'toto.com');
    check(chain.Count = 3);
    check(chain.IsValid(secret) = ecvValidSigned);
    json := chain.SaveToJson;
    check(length(json) = 718, 'certificates have fixed len');
    chain.Free; // will release selfsignedroot
    chain := TEccCertificateChain.Create;
    check(chain.IsValid(secret) = ecvUnknownAuthority);
    check(chain.LoadFromJson(json));
    check(chain.SaveToJson = json);
    check(chain.Count = 3);
    check(chain.IsValid(secret) = ecvValidSigned);
    json := ObjectToJson(chain);
    check(length(json) = 1546);
    chain.SaveToFile('test');
    bin := secret.SaveToSecureBinary('toto', 64, 1000);
    check(length(bin) = 2320);
    secret.Free;
    secret := TEccCertificateSecret.CreateFromSecureBinary(@MYPRIVKEY,
      MYPRIVKEY_LEN, MYPRIVKEY_PASS, MYPRIVKEY_ROUNDS);
    check(secret.Serial = '29E3D71DC26C134A093BA1C22CFA2582');
    check(chain.IsValidRaw(secret.Content, true) = ecvValidSelfSigned);
    json2 := ObjectToJson(secret);
    check(json1 = json2);
    secret.Free;
    secret := TEccCertificateSecret.Create;
    check(chain.IsValid(secret) = ecvCorrupted);
    check(not secret.LoadFromSecureBinary(bin, 'titi', 1000));
    check(secret.LoadFromSecureBinary(bin, 'toto', 1000));
    check(chain.IsValid(secret) = ecvValidSigned);
    chain.Add(secret);
    check(chain.Count = 4);
    sign := TEccSignatureCertified.CreateNew(secret, pointer(json), length(json));
    check(sign.Check);
    check(sign.AuthoritySerial = secret.Serial);
    check(sign.AuthorityIssuer = secret.Issuer);
    sav := sign.ToBase64;
    bin := sign.SaveToDERBinary;
    check(length(bin) >= ECC_BYTES * 2 + 6);
    sign.Free;
    sign := TEccSignatureCertified.CreateFromBase64(sav);
    check(sign.Check);
    check(sign.Version = 1);
    check(sign.Date = EccText(NowEccDate));
    check(sign.AuthoritySerial = secret.Serial);
    check(sign.AuthorityIssuer = 'toto.com');
    check(sign.SaveToDERBinary = bin);
    check(chain.IsSigned(sign, pointer(json), length(json)) = ecvValidSigned);
    signcontent := sign.Certified;
    inc(signcontent.Signature[10]); // corrupt
    sign.Certified := signcontent;
    check(sign.Check, 'seems valid');
    check(chain.IsSigned(sign, pointer(json), length(json)) = ecvInvalidSignature);
    dec(signcontent.Signature[10]);
    sign.Certified := signcontent;
    check(chain.IsSigned(sign, pointer(json), length(json)) = ecvValidSigned);
    check(chain.IsSigned(sav, pointer(json), length(json)) = ecvValidSigned);
    dec(json[10]);
    check(chain.IsSigned(sign, pointer(json), length(json)) = ecvInvalidSignature);
    check(chain.IsSigned(sav, pointer(json), length(json)) = ecvInvalidSignature);
    chain.Clear;
    check(chain.Count = 0);
    check(chain.IsSigned(sign, pointer(json), length(json)) = ecvUnknownAuthority);
    sign.Free;
    selfsignedroot := TEccCertificateSecret.CreateFromSecureFile(
       '.', serial, 'pass', 1000);
    check(chain.LoadFromFile('test'));
    check(chain.Count = 3);
    check(chain.IsValid(selfsignedroot) = ecvValidSelfSigned);
    check(selfsignedroot.IssueDate = EccText(NowEccDate));
    check(selfsignedroot.Content.Head.Signed.IssueDate = NowEccDate);
    check(chain.GetBySerial(serial) <> nil);
    chain.IsValidCached := true;
    check(ObjectToJson(chain) = jsonchain);
    check(DeleteFile(selfsignedroot.SaveToSecureFileName));
    selfsignedroot.Free;
  finally
    chain.Free;
  end;
end;

procedure TTestCoreEcc.EccCommandLineTool;
var
  sw: ICommandLine;
  cl: TCommandLine;
  i: PtrInt;
  previd, prevpass: RawUtf8;
  plainfn, rawfn: TFileName;
  keys: array of record
    priv, pub, test, crypt: TFileName;
    id, issuer, pass, text: RawUtf8;
    rounds: integer;
  end;
  exectemp: variant;

  function Exec(const nv: array of const; cmd: TEccCommand): PDocVariantData;
  var
    sw: ICommandLine;
    cl: TCommandLine;
  begin
    cl := TCommandLine.Create(nv);
    sw := cl;
    {%H-}check(EccCommand(cmd, sw) = eccSuccess);
    if CheckFailed(cl.ConsoleLines <> nil) then
      result := @DocVariantDataFake
    else
    begin
      exectemp := _JsonFast(cl.ConsoleLines[0]);
      result := _Safe(exectemp);
    end;
  end;

begin
  if DirectoryExists('synecc') then
    check(DirectoryDelete('synecc', FILES_ALL, {filesnotdir=}true), 'rmdir')
  else
    CreateDir('synecc');
  SetCurrentDir('synecc');
  try
    SetLength(keys, ECC_COUNT shr 4);
    for i := 0 to high(keys) do
      with keys[i] do
      begin
        formatUtf8('name%', [i], issuer);
        formatUtf8('pass%', [i], pass);
        rounds := 1000 + i;
        cl := TCommandLine.Create([
          'auth',     {%H-}previd,
          'authpass', {%H-}prevpass,
          'authrounds',    rounds - 1,
          'issuer',        issuer,
          'days',          30 + i,
          'newpass',       pass,
          'newrounds',     rounds]);
        sw := cl;
        check(EccCommand(ecNew, sw) = eccSuccess);
        if CheckFailed(cl.ConsoleLines <> nil) then
          exit;
        id := TrimU(split(cl.ConsoleLines[high(cl.ConsoleLines)], '.'));
        priv := format('%s.private', [id]);
        pub := format('%s.public', [id]);
        previd := id;
        prevpass := pass;
        text := RandomTextParagraph(1000);
        test := format('test%d.txt', [i]);
        crypt := 'crypt-' + test;
        FileFromString(text, test);
        Exec(['file',       test,
              'out',        crypt,
              'auth',       pub,
              'saltrounds', i + 10], ecCrypt);
      end;
    sw := TCommandLine.Create([]);
    check(EccCommand(ecChainAll, sw) = eccSuccess);
    for i := 0 to high(keys) do
      with keys[i] do
      begin
        with Exec([
          'auth',   priv,
          'pass',   pass,
          'rounds', rounds], ecInfoPriv)^ do
        begin
          check(I['Version'] = 1);
          check(U['Serial'] = id);
          check(U['Issuer'] = issuer);
        end;
        with Exec(['file', crypt], ecInfoCrypt)^ do
        begin
          check(I['Size'] = length(text));
          check(U['recipient'] = issuer);
          check(U['Recipientserial'] = id);
          check(length(U['RandomPublicKey']) = SizeOf(TEccPublicKey) * 2);
          check(U['Algorithm'] = ShortStringToAnsi7String(
            ToText(ecaPBKDF2_HMAC_SHA256_AES256_CFB_SYNLZ)^));
          check(O['Signature']^.VarType = varNull, 'not signed');
          check(not B['Meta']);
        end;
        plainfn := 'plain-' + test;
        Exec([
          'file',       crypt,
          'out',        plainfn,
          'auth',       priv,
          'authpass',   pass,
          'authrounds', rounds,
          'saltrounds', i + 10], ecDecrypt);
        check(StringFromFile(plainfn) = text);
        Exec([
          'file',       test,
          'out',        crypt,
          'auth',       id,
          'pass',       pass,
          'rounds',     rounds], ecSign);
        Exec([
          'file',       test,
          'out',        crypt,
          'auth',       pub,
          'saltrounds', i + 10,
          'algo',       ord(ecaPBKDF2_HMAC_SHA256_AES128_CTR)], ecCrypt);
        rawfn := 'raw-' + test;
        with Exec([
          'file',    crypt,
          'rawfile', rawfn], ecInfoCrypt)^ do
        begin
          check(I['Size'] = length(text));
          check(U['recipient'] = issuer);
          check(U['Recipientserial'] = id);
          check(length(U['RandomPublicKey']) = SizeOf(TEccPublicKey) * 2);
          check(U['Algorithm'] = 'ecaPBKDF2_HMAC_SHA256_AES128_CTR');
          check(O['Signature']^.I['Version'] = 1, 'signed');
          check(O['Signature']^.U['AuthoritySerial'] = id, 'serial');
          check(B['Meta']);
        end;
        check(PosEx(StringFromFile(rawfn), StringFromFile(crypt)) =
                SizeOf(TEciesHeader) + 1);
        DeleteFile(plainfn);
        Exec([
          'file',       crypt,
          'out',        plainfn,
          'authpass',   pass,
          'authrounds', rounds,
          'saltrounds', i + 10], ecDecrypt);
        check(StringFromFile(plainfn) = text, 'guess .private from header');
      end;
  finally
    SetCurrentDir('..');
  end;
end;

procedure TTestCoreEcc.ECDHEStreamProtocol;
const
  MAX = 100;
var
  str: TRawByteStringDynArray;

  function Test(const prot: IProtocol; const name: string): integer;
  var
    i: integer;
    enc, after: RawByteString;
    ref: IProtocol; // to properly release memory
  begin
    ref := prot;
    result := 0;
    for i := 0 to MAX do
    begin
      prot.Encrypt(str[i], enc);
      inc(result, length(str[i]) + length(enc));
      check(length(enc) >= length(str[i]));
      check(prot.Decrypt(enc, after) = sprSuccess);
      check(after = str[i]);
    end;
  end;

var
  key: THash256;
  a: TEcdheAuth;
  ef: TEcdheEF;
  c: TEcdheProtocolClient;
  s: TEcdheProtocolServer;
  cs, ss: TEccCertificateSecret;
  i, siz: integer;
  enc, after: RawByteString;
  timer: TPrecisionTimer;

  procedure handshake;
  var
    cf: TEcdheFrameClient;
    sf: TEcdheFrameServer;
  begin
    c := TEcdheProtocolClient.Create(a, nil, cs, ef);
    s := TEcdheProtocolServer.Create(a, nil, ss, ef);
{    c.EF := efAesCfb128;
    c.MAC := macHmacCrc32c;
    s.EF := c.EF;
    s.MAC := c.MAC; }
    c.ComputeHandshake(cf);
    Check(s.ComputeHandshake(cf, sf) = sprSuccess);
    Check(c.ValidateHandshake(sf) = sprSuccess);
  end;

begin
  SetLength(str, MAX + 1);
  for i := 0 to MAX do
    str[i] := RandomString(i * 191 + 1);
  Test(TProtocolNone.Create, 'none');
  TAesPrng.Main.FillRandom(key);
  Test(TProtocolAes.Create(TAesCfb, key, 128), 'aes');
  Test(TProtocolAes.Create(TAesCtr, key, 128), 'aes');
  cs := TEccCertificateSecret.CreateNew(nil, 'client');
  ss := TEccCertificateSecret.CreateNew(nil, 'server');
  for ef := low(ef) to high(ef) do
    for a := low(a) to high(a) do
    begin
      handshake;
      for i := 0 to MAX do
      begin
        {%H-}c.Encrypt(str[i], enc);
        check({%H-}s.CheckError(enc) <> sprInvalidMAC);
        check(s.Decrypt(enc, after) = sprSuccess);
        check(after = str[i]);
        if i and 7 = 0 then
          continue; // check asymmetric communication
        s.Encrypt(str[i], enc);
        check(c.CheckError(enc) <> sprInvalidMAC);
        check(c.Decrypt(enc, after) = sprSuccess);
        check(after = str[i]);
        if i and 3 = 0 then
          continue;
        c.Encrypt(str[i], enc);
        check(s.CheckError(enc) <> sprInvalidMAC);
        check(s.Decrypt(enc, after) = sprSuccess);
        check(after = str[i]);
        c.Encrypt(str[i], enc);
        inc(enc[2]); // alter the communication
        if ef in [efAesCrc128, efAesCrc256]  then
          check(s.CheckError(enc) = sprInvalidMAC);
        check(s.Decrypt(enc, after) = sprInvalidMAC);
      end;
      c.Free;
      s.Free;
      handshake;
      Test(c, format('c%d', [ord(a)]));
      timer.Start;
      siz := Test(s, format('s%d', [ord(a)]));
      if a = low(a) then
        NotifyTestSpeed('%', [ToText(ef)^], MAX, siz, @timer);
    end;
  cs.Free;
  ss.Free;
end;


{$ifdef USE_OPENSSL}

procedure TTestCoreEcc._OpenSSL;

  procedure Test(EvpType, BitsOrCurve, Count: integer; const Digest, Name: RawUtf8);
  var
    timer: TPrecisionTimer;
    priv, pub: RawUtf8;
    msg, sig: RawByteString;
    i: integer;
  begin
    timer.Start;
    for i := 1 to Count do
      OpenSslGenerateKeys(EvpType, BitsOrCurve, priv, pub);
    Check({%H-}priv <> '');
    Check({%H-}pub <> '');
    NotifyTestSpeed('% Generation', [Name], Count, 0, @timer);
    msg := RandomString(100);
    if IdemPChar(pointer(Name), 'RSA') then
      Count := Count * 10;
    timer.Start;
    for i := 1 to Count do
      CheckUtf8(OpenSslSign(Digest, pointer(msg), pointer(priv),
        length(msg), length(priv), sig) <> 0, 'sign %', [Name]);
    NotifyTestSpeed('% Sign', [Name], Count, 0, @timer);
    timer.Start;
    for i := 1 to Count do
      CheckUtf8(OpenSslVerify(Digest, '', pointer(msg), pointer(pub), pointer({%H-}sig),
        length(msg), length(pub), length({%H-}sig)), 'verify %', [Name]);
    NotifyTestSpeed('% Verify', [Name], Count, 0, @timer);
    inc(msg[10]);
    CheckUtf8(not OpenSslVerify(Digest, '', pointer(msg), pointer(pub), pointer(sig),
      length(msg), length(pub), length(sig)), 'detect %', [Name]);
  end;

begin
  if not OpenSslIsAvailable then
    exit;
  Test(EVP_PKEY_RSA, 2048, 3, '', 'RSA 2048');
  Test(EVP_PKEY_RSA_PSS, 2048, 3, '', 'RSA-PSS 2048');
  Test(EVP_PKEY_EC, NID_X9_62_prime256v1, 100, '', 'prime256v1');
  Test(EVP_PKEY_ED25519, 0, 100, 'null', 'ed25519');
end;

{$endif USE_OPENSSL}

end.

