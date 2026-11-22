/// Framework Core ZipCrypto and WinZip AES Encryption for .zip Archives
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.zip;

{
  *****************************************************************************

   Password Encryption for mormot.core.zip TZipWrite/TZipRead
    - TZipCryptPkware Class Implementing the Traditional PKWARE ZipCrypto
    - TZipCryptAes Class Implementing the WinZip AE-1/AE-2 Format
    - TZipCryptGcm Class Implementing AES-GCM-256 WinZip AE-3 Format

   Just include this unit in your project to enable the zeAes256 (default)
   and zeZipCrypto or zeAes256Gcm algorithms of TZipWrite.Encryption, and the
   decryption of any encrypted TZipRead entry

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.zip,
  mormot.crypt.core,
  mormot.crypt.secure;


{ ************ TZipCryptPkware Class Implementing the Traditional ZipCrypto }

type
  /// traditional PKWARE encryption (ZipCrypto) - see PKware appnote 6.1
  TZipCryptPkware = class(TZipCrypt)
  protected
    fKey0, fKey1, fKey2: cardinal;
    fTab: PCrc32tab;
    procedure Init(const Password: RawByteString);
    procedure Crypt(Data: PByteArray; Len: PtrInt; Decrypting: boolean);
  public
    constructor Create(AesStrength: integer); override;
    procedure EncryptInit(const Password: RawByteString; Crc: cardinal;
      Header: PByteArray); override;
    procedure Encrypt(Data: PByteArray; Len: PtrInt); override;
    function DecryptInit(const Password: RawByteString; const Info: TFileInfo;
      Header: PByteArray): boolean; override;
    procedure Decrypt(Data: PByteArray; Len: PtrInt; AuthOnly: boolean); override;
  end;


{ ************ TZipCryptAes Class Implementing the WinZip AE-1/AE-2 Format }

const
  /// WinZip AES-CTR uses 64 * 128-bit = 1KB of pre-computed AES blocks
  ZIP_AESCTR = 64;

type
  /// WinZip AES encryption (AE-1/AE-2) of .zip entries
  // - registered by this unit into the mormot.core.zip ZipCryptClass[] variable
  // - key derivation is PBKDF2-HMAC-SHA1 with 1000 rounds over a random salt,
  // data is encrypted using AES-CTR with a little-endian counter, then
  // authenticated with the first 80-bit of its HMAC-SHA1
  // - on Intel, will use 4 interleaved AES-NI and SHA-NI opcodes if available
  // - see https://www.winzip.com/en/support/aes-encryption
  TZipCryptAes = class(TZipCrypt)
  protected
    fAes: TAes;
    fCtr: QWord;
    fKeyLen, fKeyStreamPos: PtrInt;
    fHmac: TSynSigner;
    fKeyStream: array[0 .. ZIP_AESCTR - 1] of TAesBlock; // pre-computed blocks
    function Setup(const Password: RawByteString; Salt: PByteArray): cardinal;
    procedure Crypt(Data: PByteArray; Len: PtrInt);
  public
    /// initialize the AES-128/AES-192/AES-256 engine (AesStrength=1/2/3)
    constructor Create(AesStrength: integer); override;
    /// finalize this instance, and its sensitive information
    destructor Destroy; override;
    /// generate a random salt and the password verification value
    procedure EncryptInit(const Password: RawByteString; Crc: cardinal;
      Header: PByteArray); override;
    /// in-place AES-CTR encryption with HMAC-SHA1 authentication
    procedure Encrypt(Data: PByteArray; Len: PtrInt); override;
    /// write the 80-bit HMAC-SHA1 authentication code
    procedure EncryptDone(Trailer: PByteArray); override;
    /// derivate the keys from the stored salt and check the password
    function DecryptInit(const Password: RawByteString; const Info: TFileInfo;
      Header: PByteArray): boolean; override;
    /// in-place AES-CTR decryption with HMAC-SHA1 authentication
    procedure Decrypt(Data: PByteArray; Len: PtrInt; AuthOnly: boolean); override;
    /// validate the 80-bit HMAC-SHA1 authentication code
    function DecryptDone(Trailer: PByteArray): boolean; override;
  end;


{ ************ TZipCryptGcm Class Implementing AES-GCM-256 WinZip AE-3 Format }

type
  /// WinZip AE-3 (2026) AES-256-GCM ZIP entry encryption.
  // - 16-byte salt + 4-byte key check + 4-byte counter as header
  // - 16-byte GCM tag + 4-byte FIN authentication as trailer
  // - 600,000 PBKDF2-HMAC-SHA256 iterations (HKDF-SHA256 subkeys)
  // - uses mormot.crypt.core TAesGcmEngine, including Intel AES-NI/CLMUL
  TZipCryptGcm = class(TZipCrypt)
  protected
    fGcm: TAesGcmEngine;
    fAut: THash256;
    fNonce: TAesBlock; // retain IV for the authenticated second pass
    fHeader: THash192; // salt + KCV + counter, must be unchanged
    fVerified: boolean;
    fProcessed: QWord;
    function Setup(const Password: RawByteString; Header: PByteArray): cardinal;
    procedure CheckSize(Len: PtrInt);
  public
    /// initialize the AES-256-GCM engine (AesStrength=4)
    constructor Create(AesStrength: integer); override;
    /// finalize this instance, and its sensitive information
    destructor Destroy; override;
    /// generate a random salt and the password verification value
    procedure EncryptInit(const Password: RawByteString; Crc: cardinal;
      Header: PByteArray); override;
    /// in-place AES-GCM encryption with authentication
    procedure Encrypt(Data: PByteArray; Len: PtrInt); override;
    /// write the AES-GCM authentication code
    procedure EncryptDone(Trailer: PByteArray); override;
    /// derivate the keys from the stored salt and check the password
    function DecryptInit(const Password: RawByteString; const Info: TFileInfo;
      Header: PByteArray): boolean; override;
    /// can be called instead of DecryptInit() twice
    function DecryptReset(const Password: RawByteString; const Info: TFileInfo;
      Header: PByteArray): boolean; override;
    /// in-place AES-GCM decryption with authentication
    procedure Decrypt(Data: PByteArray; Len: PtrInt; AuthOnly: boolean); override;
    /// validate the AES-GCM authentication code
    function DecryptDone(Trailer: PByteArray): boolean; override;
  end;


implementation


{ ************ TZipCryptPkware Class Implementing the Traditional ZipCrypto }

const
  // ZipCrypto 12-byte encryption header, prepended to the compressed data
  ZIP_CRYPTHEAD = 12;

constructor TZipCryptPkware.Create(AesStrength: integer);
begin
  inherited Create(AesStrength);
  fHeaderLen := ZIP_CRYPTHEAD;
  fTab := crc32tab; // reuse the zlib/IEEE-802 tables from mormot.core.base
end;

procedure TZipCryptPkware.Init(const Password: RawByteString);
var
  tmp: TSynTempBuffer;
begin
  fKey0 := 305419896;
  fKey1 := 591751049;
  fKey2 := 878082192;
  // appnote: update the keys with each password byte, as for plain data
  tmp.Init(pointer(Password), length(Password));
  Crypt(tmp.buf, tmp.len, {decrypting=}false);
  FillZero(tmp.buf^, tmp.len);
  tmp.Done;
end;

procedure TZipCryptPkware.Crypt(Data: PByteArray; Len: PtrInt;
  Decrypting: boolean);
var
  k0, k1, k2, t: cardinal;
  b, c: PtrUInt;
  tab: PCrc32tab;
begin
  // keys in local variables for register allocation, with no inline needed
  k0 := fKey0;
  k1 := fKey1;
  k2 := fKey2;
  tab := fTab;
  while Len > 0 do
  begin
    t := (k2 and $ffff) or 2;
    b := Data^[0];
    c := b xor (((t * (t xor 1)) shr 8) and $ff);
    Data^[0] := c;
    if Decrypting then
      b := c; // keys are always updated with the plain byte
    k0 := tab^[0, (k0 xor b) and $ff] xor (k0 shr 8);
    k1 := (k1 + (k0 and $ff)) * 134775813 + 1;
    k2 := tab^[0, (k2 xor (k1 shr 24)) and $ff] xor (k2 shr 8);
    Data := @Data^[1];
    dec(Len);
  end;
  fKey0 := k0;
  fKey1 := k1;
  fKey2 := k2;
end;

procedure TZipCryptPkware.EncryptInit(const Password: RawByteString;
  Crc: cardinal; Header: PByteArray);
begin
  Init(Password);
  // appnote: 11 random bytes, then the high byte of the crc32 as check byte
  Random128(PAesBlock(Header)); // Header is a THash512 of 64 bytes
  Header^[ZIP_CRYPTHEAD - 1] := Crc shr 24;
  Encrypt(Header, ZIP_CRYPTHEAD);
end;

procedure TZipCryptPkware.Encrypt(Data: PByteArray; Len: PtrInt);
begin
  Crypt(Data, Len, {decrypting=}false);
end;

function TZipCryptPkware.DecryptInit(const Password: RawByteString;
  const Info: TFileInfo; Header: PByteArray): boolean;
var
  h: array[0 .. ZIP_CRYPTHEAD - 1] of byte;
  check: PtrUInt;
begin
  Init(Password);
  MoveFast(Header^, h, ZIP_CRYPTHEAD);
  Decrypt(@h, ZIP_CRYPTHEAD, {AuthOnly=}false);
  // appnote: high byte of the crc32, or of the DOS time with a data descriptor
  if Info.flags and FLAG_DATADESCRIPTOR <> 0 then
    check := (cardinal(Info.zlastMod) shr 8) and $ff
  else
    check := Info.zcrc32 shr 24;
  result := h[ZIP_CRYPTHEAD - 1] = check;
end;

procedure TZipCryptPkware.Decrypt(Data: PByteArray; Len: PtrInt; AuthOnly: boolean);
begin
  Crypt(Data, Len, {decrypting=}true);
end;


{ ************ TZipCryptAes Class Implementing the WinZip AE-1/AE-2 Format }

const
  AES_ROUNDS = 1000; // PBKDF2 iterations, as defined by WinZip
  AES_PWV    = 2;    // password verification value size
  AES_MAC    = 10;   // truncated HMAC-SHA1 authentication code size

constructor TZipCryptAes.Create(AesStrength: integer);
begin
  inherited Create(AesStrength);
  fKeyLen := 8 + AesStrength shl 3;      // 16/24/32 bytes
  fHeaderLen := fKeyLen shr 1 + AES_PWV; // salt is 8/12/16 bytes
  fTrailerLen := AES_MAC;
end;

destructor TZipCryptAes.Destroy;
begin
  fAes.Done;
  fHmac.Done;
  FillZero(fKeyStream, SizeOf(fKeyStream));
  inherited Destroy;
end;

function TZipCryptAes.Setup(const Password: RawByteString;
  Salt: PByteArray): cardinal;
var
  klen: PtrInt;
  s, k: RawByteString;
begin
  // derivate AES key + HMAC key + password verification value from the salt
  FastSetRawByteString(s, Salt, fHeaderLen - AES_PWV);
  klen := fKeyLen * 2 + AES_PWV;
  k := fHmac.Pbkdf2(saSha1, Password, s, AES_ROUNDS, klen);
  FillZero(s);
  if length(k) <> klen then
    ESynZip.RaiseUtf8('%.Setup: PBKDF2 failed', [self]);
  fAes.EncryptInit(pointer(k)^, {bits=}fKeyLen shl 3);
  fHmac.Init(saSha1, @PByteArray(k)[fKeyLen], fKeyLen);
  result := PWord(@PByteArray(k)[fKeyLen * 2])^;
  FillZero(k);
  fCtr := 0;
  fKeyStreamPos := SizeOf(fKeyStream); // trigger the first DoBlocks() call
end;

procedure FillCtr(p: PHash128Rec; ctr: PtrUInt);
var
  n: integer;
begin
  // WinZip AES-CTR uses a 128-bit little-endian counter starting at 1
  n := ZIP_AESCTR;
  repeat
    inc(ctr);
    p^.L := ctr;
    p^.H := 0;
    inc(p);
    dec(n);
  until n = 0;
end;

procedure TZipCryptAes.Crypt(Data: PByteArray; Len: PtrInt);
var
  n: PtrInt;
begin
  while Len > 0 do
  begin
    if fKeyStreamPos = SizeOf(fKeyStream) then
    begin
      FillCtr(@fKeyStream, fCtr);
      inc(fCtr, ZIP_AESCTR);
      {$ifdef CPU32}
      if PtrUInt(fCtr) = 0 then // PtrUInt ctr would overflow after 64GB
        ESynZip.RaiseUtf8('%.Crypt: 32-bit overflow', [self]);
      {$endif CPU32}
      fAes.DoBlocks(@fKeyStream, @fKeyStream, length(fKeyStream), true); // AES-NI
      fKeyStreamPos := 0;
    end;
    n := SizeOf(fKeyStream) - fKeyStreamPos;
    if n > Len then
      n := Len;
    XorMemory(Data, @PByteArray(@fKeyStream)[fKeyStreamPos], n);
    inc(fKeyStreamPos, n);
    inc(PByte(Data), n);
    dec(Len, n);
  end;
end;

procedure TZipCryptAes.EncryptInit(const Password: RawByteString;
  Crc: cardinal; Header: PByteArray);
var
  saltlen: PtrInt;
begin
  saltlen := fHeaderLen - AES_PWV;
  if saltlen > SizeOf(TAesBlock) then
    ESynZip.RaiseUtf8('%.EncryptInit: saltlen=%', [self, saltlen]);
  Random128(PAesBlock(Header)); // Header is a THash512 of 64 bytes
  PWord(@Header[saltlen])^ := Setup(Password, Header);
end;

procedure TZipCryptAes.Encrypt(Data: PByteArray; Len: PtrInt);
begin
  Crypt(Data, Len);
  fHmac.Update(Data, Len); // authenticate the encrypted data - may use SHA-NI
end;

procedure TZipCryptAes.EncryptDone(Trailer: PByteArray);
var
  mac: THash512Rec;
begin
  fHmac.Final(@mac);
  MoveFast(mac, Trailer^, AES_MAC);
end;

function TZipCryptAes.DecryptInit(const Password: RawByteString;
  const Info: TFileInfo; Header: PByteArray): boolean;
begin
  result := Setup(Password, Header) = PWord(@Header[fHeaderLen - AES_PWV])^;
end;

procedure TZipCryptAes.Decrypt(Data: PByteArray; Len: PtrInt; AuthOnly: boolean);
begin
  fHmac.Update(Data, Len); // authenticate the encrypted data - may use SHA-NI
  if not AuthOnly then
    Crypt(Data, Len);
end;

function TZipCryptAes.DecryptDone(Trailer: PByteArray): boolean;
var
  mac: THash512Rec;
begin
  fHmac.Final(@mac);
  result := IsEqual(mac, Trailer^, AES_MAC); // constant-time comparison
end;


{ ************ TZipCryptGcm Class Implementing AES-GCM-256 WinZip AE-3 Format }

const
  // AE-3 units are 10,000 PBKDF2 iterations. WinZip 77.2 uses 60 units.
  ZIP_AE3_ITERATIONS = 600000;
  ZIP_AE3_HEADER = 24;  // salt16 + KCV4 + counter4
  ZIP_AE3_TRAILER = 20; // GCM tag16 + FIN4
  // NIST SP 800-38D: at most 2^39-256 ciphertext bits per key.
  ZIP_AE3_MAX_DATA = (QWord(1) shl 36) - 32;
  ZIP_AE3_AUT: array[0..3] of byte = (65, 85, 84, 0); // 'AUT'#0
  ZIP_AE3_DEK: array[0..3] of byte = (68, 69, 75, 0); // 'DEK'#0
  ZIP_AE3_NCE: array[0..3] of byte = (78, 67, 69, 0); // 'NCE'#0
  ZIP_AE3_KCV: array[0..3] of byte = (75, 67, 86, 0); // 'KCV'#0
  ZIP_AE3_FIN: array[0..3] of byte = (70, 73, 78, 0); // 'FIN'#0;

// HKDF-Expand block #1; PRK comes directly from PBKDF2-HMAC-SHA256
procedure ZipAe3Expand(const Prk: THash256; Info: pointer; InfoLen: integer;
  out Value: THash256);
var
  h: THmacSha256;
  one: byte;
begin
  one := 1;
  h.Init(@Prk, SizeOf(Prk));
  h.Update(Info, InfoLen);
  h.Update(@one, 1);
  h.Done(Value);
end;

// 4-byte password check value (key confirmation) = HMAC(AUT, 'KCV'#0)[0..3]
procedure ZipAe3Check(const Aut: THash256; out Digest: THash256);
var
  h: THmacSha256;
begin
  h.Init(@Aut, SizeOf(Aut));
  h.Update(@ZIP_AE3_KCV, SizeOf(ZIP_AE3_KCV));
  h.Done(Digest);
end;

procedure ZipAe3Fin(const Aut: THash256; const Tag: TAesBlock;
  out Digest: THash256);
var
  h: THmacSha256;
begin
  h.Init(@Aut, SizeOf(Aut));
  h.Update(@ZIP_AE3_FIN, SizeOf(ZIP_AE3_FIN));
  h.Update(@Tag, SizeOf(Tag));
  h.Done(Digest);
end;

constructor TZipCryptGcm.Create(AesStrength: integer);
begin
  inherited Create(4); // internal mode selector; wire strength stays 3
  fHeaderLen := ZIP_AE3_HEADER;
  fTrailerLen := ZIP_AE3_TRAILER;
  fAesIterations := ZIP_AE3_ITERATIONS;
end;

destructor TZipCryptGcm.Destroy;
begin
  fGcm.Done;
  FillZero(fAut);
  FillZero(fNonce);
  FillZero(fHeader, SizeOf(fHeader));
  inherited Destroy;
end;

function TZipCryptGcm.Setup(const Password: RawByteString;
  Header: PByteArray): cardinal;
var
  signer: TSynSigner;
  salt: RawByteString;
  prk: THash512Rec; // Pbkdf2() writes a 32-byte HMAC-SHA256 output
  dek, digest: THash256;
  info: array[0..7] of byte;
  nonce: TAesBlock;
  checkValue: cardinal;
begin
  fVerified := false;
  // Reject unbounded password KDF work when processing hostile archives
  if (fAesIterations < 320000) or
     (fAesIterations > 5000000) or
     (fAesIterations mod 10000 <> 0) then
    ESynZip.RaiseUtf8('%.Setup: invalid AE-3 work factor %',
      [self, fAesIterations]);
  SetString(salt, PAnsiChar(Header), 16); // 16 arbitrary binary salt bytes
  FillZero(prk.b);
  if signer.Pbkdf2(saSha256, Password, salt, fAesIterations, @prk) <> 32 then
    ESynZip.RaiseUtf8('%.Setup: AE-3 PBKDF2 failed', [self]);
  FillZero(salt);
  ZipAe3Expand(PHash256(@prk)^, @ZIP_AE3_AUT, 4, fAut);
  // KCV = HMAC-SHA256(AUT, 'KCV' + NUL)[0..3]
  ZipAe3Check(fAut, digest);
  checkValue := PCardinal(@digest)^;
  // prepare the data-encryption key independently
  MoveFast(ZIP_AE3_DEK, info, 4);
  MoveFast(Header^[20], info[4], 4);
  ZipAe3Expand(PHash256(@prk)^, @info, 8, dek);
  MoveFast(ZIP_AE3_NCE, info, 4);
  ZipAe3Expand(PHash256(@prk)^, @info, 8, digest);
  FillZero(nonce);
  MoveFast(digest, nonce, 12);
  fNonce := nonce;
  fGcm.Done;
  if not fGcm.Init(dek, 256) or
     not fGcm.Reset(PHash128Rec(@nonce), 12) then
    ESynZip.RaiseUtf8('%.Setup: AES-256-GCM initialization failed', [self]);
  fProcessed := 0;
  result := checkValue;
  FillZero(digest);
  FillZero(dek);
  FillZero(nonce);
  PInt64(@info)^ := 0;
  FillZero(prk.b);
end;

procedure TZipCryptGcm.CheckSize(Len: PtrInt);
begin
  if (Len < 0) or
     (QWord(Len) > ZIP_AE3_MAX_DATA - fProcessed) then
    ESynZip.RaiseUtf8('%.Crypt: AE-3 GCM data limit reached', [self]);
  fProcessed := fProcessed + QWord(Len);
end;

procedure TZipCryptGcm.EncryptInit(const Password: RawByteString;
  Crc: cardinal; Header: PByteArray);
var
  sha: TSha256;
  digest: THash256;
  kcv: cardinal;
begin
  Random128(PAesBlock(Header)); // independent fresh 16-byte salt per ZIP entry
  sha.Full(Header, 16, digest);
  MoveFast(digest, Header^[20], 4); // counter4 := SHA256(salt)[0..3]
  kcv := Setup(Password, Header);
  MoveFast(kcv, Header^[16], 4);
  FillZero(digest);
end;

procedure TZipCryptGcm.Encrypt(Data: PByteArray; Len: PtrInt);
begin
  CheckSize(Len);
  if (Len <> 0) and
     not fGcm.Encrypt(Data, Data, Len) then
    ESynZip.RaiseUtf8('%.Encrypt: AES-256-GCM failed', [self]);
end;

procedure TZipCryptGcm.EncryptDone(Trailer: PByteArray);
var
  tag: TAesBlock;
  fin: THash256;
begin
  if not fGcm.Final(tag) then
    ESynZip.RaiseUtf8('%.EncryptDone: AES-256-GCM failed', [self]);
  ZipAe3Fin(fAut, tag, fin);
  MoveFast(tag, Trailer^, 16);
  MoveFast(fin, Trailer^[16], 4);
  FillZero(fin);
  FillZero(tag);
end;

function TZipCryptGcm.DecryptInit(const Password: RawByteString;
  const Info: TFileInfo; Header: PByteArray): boolean;
var
  kcv: cardinal;
begin
  kcv := Setup(Password, Header);
  result := IsEqual(kcv, Header^[16], 4);
  if result then
    MoveFast(Header^, fHeader, ZIP_AE3_HEADER)
  else
    fGcm.Done;
end;

function TZipCryptGcm.DecryptReset(const Password: RawByteString;
  const Info: TFileInfo; Header: PByteArray): boolean;
begin
  // the first pass has already authenticated this exact entry
  result := fVerified and IsEqual(fHeader, Header^, ZIP_AE3_HEADER);
  fVerified := false;
  if not result then
    exit;
  result := fGcm.Reset(PHash128Rec(@fNonce), 12); // keep the AES key
  fProcessed := 0;
end;

procedure TZipCryptGcm.Decrypt(Data: PByteArray; Len: PtrInt;
  AuthOnly: boolean);
begin
  CheckSize(Len);
  if Len = 0 then
    exit;
  if AuthOnly then
  begin
    if fGcm.Auth(Data, Len) then // AES-CTR part not needed
      exit;
  end
  else if fGcm.Decrypt(Data, Data, Len) then
    exit;
  ESynZip.RaiseUtf8('%.Decrypt: AES-256-GCM failed', [self]);
end;

function TZipCryptGcm.DecryptDone(Trailer: PByteArray): boolean;
var
  tag: TAesBlock;
  fin: THash256;
begin
  if not fGcm.Final(tag, {andDone=}false) then
    ESynZip.RaiseUtf8('%.DecryptDone: AES-256-GCM failed', [self]);
  ZipAe3Fin(fAut, tag, fin);
  // constant-time checks of BOTH the GCM tag and the authenticated FIN field
  result := IsEqual(tag, Trailer^, 16);
  if not IsEqual(fin, Trailer^[16], 4) then
    result := false;
  fVerified := result; // permit one fast reset only after authenticated data
  FillZero(fin);
  FillZero(tag);
end;


initialization
  ZipCryptClass[zeAes256]    := TZipCryptAes;
  ZipCryptClass[zeZipCrypto] := TZipCryptPkware;
  ZipCryptClass[zeAes256Gcm] := TZipCryptGcm;

end.
