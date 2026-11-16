/// Framework Core ZipCrypto and WinZip AES Encryption for .zip Archives
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.zip;

{
  *****************************************************************************

   Password Encryption for mormot.core.zip TZipWrite/TZipRead
    - TZipCryptPkware Class Implementing the Traditional PKWARE ZipCrypto
    - TZipCryptAes Class Implementing the WinZip AE-1/AE-2 Format

   Just include this unit in your project to enable the zeAes256 (default)
   and zeZipCrypto algorithms of TZipWrite.Encryption, and the decryption of
   any encrypted TZipRead entry.

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
    procedure Decrypt(Data: PByteArray; Len: PtrInt); override;
  end;


{ ************ TZipCryptAes Class Implementing the WinZip AE-1/AE-2 Format }

type
  /// WinZip AES encryption (AE-1/AE-2) of .zip entries
  // - registered by this unit into the mormot.core.zip ZipCryptClass[] variable
  // - key derivation is PBKDF2-HMAC-SHA1 with 1000 rounds over a random salt,
  // data is encrypted using AES-CTR with a little-endian counter, then
  // authenticated with the first 80-bit of its HMAC-SHA1
  // - see https://www.winzip.com/en/support/aes-encryption
  TZipCryptAes = class(TZipCrypt)
  protected
    fAes: TAes;
    fHmac: TSynSigner;
    fCtr: QWord;
    fKeyLen, fKeyStreamPos: PtrInt;
    fKeyStream: array[0 .. 63] of TAesBlock; // pre-computed AES-CTR blocks
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
    procedure Decrypt(Data: PByteArray; Len: PtrInt); override;
    /// validate the 80-bit HMAC-SHA1 authentication code
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
  RandomBytes(Header, ZIP_CRYPTHEAD - 1);
  Header[ZIP_CRYPTHEAD - 1] := Crc shr 24;
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
  Decrypt(@h, ZIP_CRYPTHEAD);
  // appnote: high byte of the crc32, or of the DOS time with a data descriptor
  if Info.flags and FLAG_DATADESCRIPTOR <> 0 then
    check := (cardinal(Info.zlastMod) shr 8) and $ff
  else
    check := Info.zcrc32 shr 24;
  result := h[ZIP_CRYPTHEAD - 1] = check;
end;

procedure TZipCryptPkware.Decrypt(Data: PByteArray; Len: PtrInt);
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
  fKeyLen := 8 + AesStrength * 8;            // 16/24/32 bytes
  fHeaderLen := fKeyLen shr 1 + AES_PWV;     // salt is 8/12/16 bytes
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
  s: RawUtf8;
  k: RawByteString;
begin
  // derivate AES key + HMAC key + password verification value from the salt
  FastSetString(s, Salt, fHeaderLen - AES_PWV);
  k := fHmac.Pbkdf2(saSha1, RawUtf8(Password), s, AES_ROUNDS,
    fKeyLen * 2 + AES_PWV);
  if length(k) <> fKeyLen * 2 + AES_PWV then
    ESynZip.RaiseUtf8('%.Setup: PBKDF2 failed', [self]);
  fAes.EncryptInit(pointer(k)^, fKeyLen * 8);
  fHmac.Init(saSha1, @PByteArray(k)[fKeyLen], fKeyLen);
  result := PWord(@PByteArray(k)[fKeyLen * 2])^;
  FillZero(k);
  fCtr := 0;
  fKeyStreamPos := SizeOf(fKeyStream); // compute on first Crypt() call
end;

procedure TZipCryptAes.Crypt(Data: PByteArray; Len: PtrInt);
var
  i, n: PtrInt;
begin
  while Len > 0 do
  begin
    if fKeyStreamPos = SizeOf(fKeyStream) then
    begin
      // WinZip AES-CTR uses a 128-bit little-endian counter starting at 1
      FillCharFast(fKeyStream, SizeOf(fKeyStream), 0);
      for i := 0 to high(fKeyStream) do
      begin
        inc(fCtr);
        PQWord(@fKeyStream[i])^ := fCtr;
      end;
      fAes.DoBlocks(@fKeyStream, @fKeyStream, length(fKeyStream), true);
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
begin
  TAesPrng.Fill(Header, fHeaderLen - AES_PWV); // cryptographic random salt
  PWord(@Header[fHeaderLen - AES_PWV])^ := Setup(Password, Header);
end;

procedure TZipCryptAes.Encrypt(Data: PByteArray; Len: PtrInt);
begin
  Crypt(Data, Len);
  fHmac.Update(Data, Len); // authenticate the encrypted data
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

procedure TZipCryptAes.Decrypt(Data: PByteArray; Len: PtrInt);
begin
  fHmac.Update(Data, Len); // authenticate the encrypted data
  Crypt(Data, Len);
end;

function TZipCryptAes.DecryptDone(Trailer: PByteArray): boolean;
var
  mac: THash512Rec;
begin
  fHmac.Final(@mac);
  result := IsEqual(mac, Trailer^, AES_MAC); // constant-time comparison
end;


initialization
  ZipCryptClass[zeAes256] := TZipCryptAes;
  ZipCryptClass[zeZipCrypto] := TZipCryptPkware;

end.
