/// Framework Core Cryptographic Process using OpenSSL
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.crypto.openssl;

{
  *****************************************************************************

   High-Performance Cryptographic Features using OpenSSL 1.1.1
    - OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG)
    - AES Cypher/Uncypher in various Modes

  *****************************************************************************

  Note: on x86_64, our mormot.core.crypto.pas asm is stand-alone and faster
  than OpenSSL for most algorithms, but AES-GCM.

}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.crypto,
  mormot.core.ecc,
  mormot.lib.openssl11;


{ ************** OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG) }

type
  /// exception class raised by this unit
  EOpenSslCrypto = class(EOpenSsl);

  /// TAesPrng-compatible class using OpenSSL 1.1.1
  // - we abbreviate OpenSsl as Osl for class names for brevity
  // - may be used instead of TAesPrng if a "proven" generator is required -
  // you could override MainAesPrng global variable
  // - mormot.core.crypto TAesPrng is faster, especially for small output:
  // $ OpenSSL Random32 in 313.10ms i.e. 319,378/s, aver. 3us, 1.2 MB/s
  // $ OpenSSL FillRandom in 334us, 285.5 MB/s
  // $ mORMot Random32 in 4.76ms i.e. 21,003,990/s, aver. 0us, 80.1 MB/s
  // $ mORMot FillRandom in 212us, 449.8 MB/s
  TAesPrngOsl = class(TAesPrngAbstract)
  public
    /// initialize the CSPRNG using OpenSSL 1.1.1
    // - if the library is not available, will raise an Exception
    constructor Create; override;
    /// wrapper around function OpenSslIsAvailable
    class function IsAvailable: boolean; override;
    /// fill a binary buffer with some pseudorandom data
    // - this method is thread-safe
    // - is just a wrapper around RAND_bytes() API call
    procedure FillRandom(Buffer: pointer; Len: PtrInt); override;
    /// returns the single system-wide instance of TAesPrngOsl
    // - if you need to generate some random content, just call the
    // TAesPrngOsl.Main.FillRandom() overloaded methods, or directly
    // TAesPrngOsl.Fill() class methods
    class function Main: TAesPrngAbstract; override;
  end;


{ ************** AES Cypher/Uncypher in various Modes }

type
  /// reusable wrapper around OpenSSL Cipher process
  TAesOsl = object
  public
    Owner: TAesAbstract;
    Cipher: PEVP_CIPHER; // computed from OpenSslCipherName virtual method
    Ctx: array[boolean] of PEVP_CIPHER_CTX; // set and reused in CallEvp()
    procedure Init(aOwner: TAesAbstract; aCipherName: PUtf8Char);
    procedure Done;
    procedure SetEvp(DoEncrypt: boolean; const method: string);
    procedure UpdEvp(DoEncrypt: boolean; BufIn, BufOut: pointer; Count: cardinal);
    procedure Clone(ToOwner: TAesAbstract; out ToAesOsl: TAesOsl);
  end;

  /// handle AES cypher/uncypher with chaining with OpenSSL 1.1
  // - we abbreviate OpenSsl as Osl for class names for brevity
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAesEcbOsl, TAesCbcOsl, TAesCfbOsl, TAesOfbOsl and TAesCtrOsl
  // classes to handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  // - those classes are re-entrant, i.e. that you can call the Encrypt*
  // or Decrypt* methods on the same instance several times
  TAesAbstractOsl = class(TAesAbstract)
  protected
    fAes: TAesOsl;
    procedure AfterCreate; override; // circumvent Delphi bug about const aKey
  public
    /// creates a new instance with the very same values
    // - directly copy the existing OpenSSL context for efficiency
    function Clone: TAesAbstract; override;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - will return self to avoid creating two instances
    function CloneEncryptDecrypt: TAesAbstract; override;
    /// release the used instance memory and resources
    destructor Destroy; override;
    /// wrapper around function OpenSslIsAvailable
    // - actual cipher won't be checked until Create() since we need the keysize
    class function IsAvailable: boolean; override;
    /// perform the AES cypher in the corresponding mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the corresponding mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// return the OpenSSL EVP Cipher name of this class and key size
    function OpenSslCipherName: PUtf8Char; virtual; abstract;
  end;

  /// OpenSSL AES cypher/uncypher without chaining (ECB)
  // - this mode is known to be less secure than the others
  TAesEcbOsl = class(TAesAbstractOsl)
  public
    /// return the OpenSSL EVP Cipher name of this class and key size
    function OpenSslCipherName: PUtf8Char; override;
  end;

  /// OpenSSL AES cypher/uncypher with Cipher-block chaining (CBC)
  TAesCbcOsl = class(TAesAbstractOsl)
  public
    /// return the OpenSSL EVP Cipher name of this class and key size
    function OpenSslCipherName: PUtf8Char; override;
  end;

  /// OpenSSL AES cypher/uncypher with Cipher feedback (CFB)
  // - our TAesCfb class is slightly faster than OpenSSL:
  // $ 2500 aes128cfb in 10.80ms i.e. 231438/s or 492.5 MB/s
  // $ 2500 aes128cfbosl in 10.96ms i.e. 228039/s or 485.3 MB/s
  // $ 2500 aes256cfb in 13.36ms i.e. 187041/s or 398 MB/s
  // $ 2500 aes256cfbosl in 13.47ms i.e. 185473/s or 394.7 MB/s
  TAesCfbOsl = class(TAesAbstractOsl)
  public
    /// return the OpenSSL EVP Cipher name of this class and key size
    function OpenSslCipherName: PUtf8Char; override;
  end;

  /// OpenSSL AES cypher/uncypher with Output feedback (OFB)
  // - our TAesOfb class is faster than OpenSSL:
  // $ 2500 aes128ofb in 7.07ms i.e. 353207/s or 751.7 MB/s
  // $ 2500 aes128ofbosl in 8.20ms i.e. 304692/s or 648.4 MB/s
  // $ 2500 aes256ofb in 9.64ms i.e. 259201/s or 551.6 MB/s
  // $ 2500 aes256ofbosl in 10.71ms i.e. 233383/s or 496.6 MB/s
  TAesOfbOsl = class(TAesAbstractOsl)
  public
    /// return the OpenSSL EVP Cipher name of this class and key size
    function OpenSslCipherName: PUtf8Char; override;
  end;

  /// OpenSSL AES cypher/uncypher with 128-bit Counter mode (CTR)
  // - similar to TAesCtrNist, not our proprietary TAesCtr which has 64-bit CTR
  // - our TAesCtrNist class is faster than OpenSSL:
  // $ 2500 aes128ctr in 2.06ms i.e. 1209482/s or 2.5 GB/s
  // $ 2500 aes256ctr in 2.68ms i.e. 931792/s or 1.9 GB/s
  // $ 2500 aes128ctrosl in 2.37ms i.e. 1053518/s or 2.1 GB/s
  // $ 2500 aes256ctrosl in 3.22ms i.e. 775193/s or 1.6 GB/s
  TAesCtrNistOsl = class(TAesAbstractOsl)
  public
    /// return the OpenSSL EVP Cipher name of this class and key size
    function OpenSslCipherName: PUtf8Char; override;
  end;

  /// OpenSSL AES-GCM cypher/uncypher
  // - implements AEAD (authenticated-encryption with associated-data) process
  // via MacSetNonce/MacEncrypt or AesGcmAad/AesGcmFinal methods
  // - OpenSSL is faster than our TAesGcm class which is not interleaved:
  // $ 2500 aes128gcm in 14.41ms i.e. 173418/s or 369 MB/s
  // $ 2500 aes256gcm in 17.37ms i.e. 143918/s or 306.2 MB/s
  // $ 2500 aes128gcmosl in 3.03ms i.e. 824810/s or 1.7 GB/s
  // $ 2500 aes256gcmosl in 3.56ms i.e. 701065/s or 1.4 GB/s
  TAesGcmOsl = class(TAesGcmAbstract)
  protected
    fAes: TAesOsl;
    function AesGcmInit: boolean; override; // from fKey/fKeySize
    procedure AesGcmDone; override;
    procedure AesGcmReset; override; // from fIV/CTR_POS
    function AesGcmProcess(BufIn, BufOut: pointer; Count: cardinal): boolean; override;
  public
    /// creates a new instance with the very same values
    // - by design, our classes will use TAesGcmEngine stateless context, so
    // this method will just copy the current fields to a new instance,
    // by-passing the key creation step
    function Clone: TAesAbstract; override;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - will return self to avoid creating two instances
    function CloneEncryptDecrypt: TAesAbstract; override;
    /// AES-GCM pure alternative to MacSetNonce()
    // - set the IV as usual (only the first 12 bytes will be used for GCM),
    // then optionally append any AEAD data with this method before Encrypt()
    procedure AesGcmAad(Buf: pointer; Len: integer); override;
    /// AES-GCM pure alternative to MacEncryptGetTag/MacDecryptCheckTag
    // - after Encrypt, fill tag with the GCM value of the data and return true
    // - after Decrypt, return true only if the GCM value of the data match tag
    function AesGcmFinal(var tag: TAesBlock): boolean; override;
  end;


/// call once at program startup to use OpenSSL when its performance
// - mainly for AES-GCM and AES-CTR process
procedure RegisterOpenSsl;


implementation


{ TAesOsl }

procedure TAesOsl.Init(aOwner: TAesAbstract; aCipherName: PUtf8Char);
begin
  Owner := aOwner;
  EOpenSslCrypto.CheckAvailable(PClass(Owner)^, 'Create');
  Cipher := EVP_get_cipherbyname(aCipherName);
  if Cipher = nil then
    raise EOpenSslCrypto.CreateFmt('%s.Create: unknown ''%s'' cipher',
      [ClassNameShort(Owner)^, aCipherName]);
end;

procedure TAesOsl.Done;
begin
  if Ctx[false] <> nil then
    EVP_CIPHER_CTX_free(Ctx[false]);
  if Ctx[true] <> nil then
    EVP_CIPHER_CTX_free(Ctx[true]);
end;

procedure TAesOsl.SetEvp(DoEncrypt: boolean; const method: string);
var
  c: PEVP_CIPHER_CTX;
begin
  c := Ctx[DoEncrypt];
  if c = nil then
  begin
    // setup encrypt/decrypt context, with the proper key and no padding
    c := EVP_CIPHER_CTX_new;
    EOpenSslCrypto.Check(Owner, method,
      EVP_CipherInit_ex(c, Cipher, nil,
        @TAesAbstractOsl(Owner).fKey, nil, ord(DoEncrypt)));
    EOpenSslCrypto.Check(Owner, method,
      EVP_CIPHER_CTX_set_padding(c, 0));
    Ctx[DoEncrypt] := c;
  end;
  // OpenSSL allows to reuse the previous Ctxt[], just setting the (new) IV
  EOpenSslCrypto.Check(Owner, method,
    EVP_CipherInit_ex(c, nil, nil, nil, @Owner.IV, ord(DoEncrypt)));
end;

procedure TAesOsl.UpdEvp(DoEncrypt: boolean; BufIn, BufOut: pointer; Count: cardinal);
var
  outl: integer;
begin
  if (BufOut <> nil) and
     (Count and AesBlockMod <> 0) then
    raise ESynCrypto.CreateUtf8('%.%: Count=% is not a multiple of 16',
      [Owner, 'UpdEvp', Count]);
  EOpenSslCrypto.Check(Owner, 'UpdEvp',
    EVP_CipherUpdate(Ctx[DoEncrypt], BufOut, @outl, BufIn, Count));
  // no need to call EVP_CipherFinal_ex() since we expect no padding
end;

procedure TAesOsl.Clone(ToOwner: TAesAbstract; out ToAesOsl: TAesOsl);
var
  enc: boolean;
begin
  TAesAbstractOsl(ToOwner).fKeySize := TAesAbstractOsl(Owner).fKeySize;
  TAesAbstractOsl(ToOwner).fKeySizeBytes := TAesAbstractOsl(Owner).fKeySizeBytes;
  TAesAbstractOsl(ToOwner).fKey := TAesAbstractOsl(Owner).fKey;
  ToAesOsl.Owner := ToOwner;
  ToAesOsl.Cipher := Cipher;
  for enc := false to true do
    if Ctx[enc] <> nil then
    begin
      // efficient Ctx[] copy
      ToAesOsl.Ctx[enc] := EVP_CIPHER_CTX_new;
      EVP_CIPHER_CTX_copy(ToAesOsl.Ctx[enc], Ctx[enc]);
    end;
end;



{ ************** OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG) }

{ TAesPrngOsl }

class function TAesPrngOsl.IsAvailable: boolean;
begin
  result := OpenSslIsAvailable;
end;

constructor TAesPrngOsl.Create;
begin
  inherited Create;
  EOpenSslCrypto.CheckAvailable(PClass(self)^, 'Create');
end;

procedure TAesPrngOsl.FillRandom(Buffer: pointer; Len: PtrInt);
begin
  inc(fTotalBytes, Len);
  RAND_bytes(Buffer, Len);
end;

var
  MainAesPrngOsl: TAesPrngOsl;

class function TAesPrngOsl.Main: TAesPrngAbstract;
begin
  result := MainAesPrngOsl;
  if result = nil then
  begin
    EOpenSslCrypto.CheckAvailable(self, 'Main');
    GlobalLock;
    try
      if MainAesPrngOsl = nil then
        MainAesPrngOsl := TAesPrngOsl.Create;
    finally
      GlobalUnLock;
    end;
    result := MainAesPrngOsl;
  end;
end;


{ ************** AES Cypher/Uncypher in various Modes }

{ TAesAbstractOsl }

procedure TAesAbstractOsl.AfterCreate;
begin
  fAes.Init(self, OpenSslCipherName);
end;

destructor TAesAbstractOsl.Destroy;
begin
  fAes.Done;
  inherited Destroy;
end;

class function TAesAbstractOsl.IsAvailable: boolean;
begin
  result := OpenSslIsAvailable;
end;

procedure TAesAbstractOsl.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fAes.SetEvp({doencrypt=}true, 'Encrypt');
  fAes.UpdEvp({doencrypt=}true, BufIn, BufOut, Count);
end;

procedure TAesAbstractOsl.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fAes.SetEvp({doencrypt=}false, 'Decrypt');
  fAes.UpdEvp({doencrypt=}false, BufIn, BufOut, Count);
end;

function TAesAbstractOsl.Clone: TAesAbstract;
begin
  if fIVHistoryDec.Count <> 0 then
    result := inherited Clone
  else
  begin
    result := TAesAbstractOsl(NewInstance);
    fAes.Clone(result, TAesAbstractOsl(result).fAes); // efficient Ctx[] copy
  end;
end;

function TAesAbstractOsl.CloneEncryptDecrypt: TAesAbstract;
begin
  result := self; // there is one fCtx[] for each direction
end;


{ TAesEcbOsl }

function TAesEcbOsl.OpenSslCipherName: PUtf8Char;
begin
  case fKeySize of
    128:
      result := 'aes-128-ecb';
    192:
      result := 'aes-192-ecb';
  else
    result := 'aes-256-ecb';
  end;
end;

{ TAesCbcOsl }

function TAesCbcOsl.OpenSslCipherName: PUtf8Char;
begin
  case fKeySize of
    128:
      result := 'aes-128-cbc';
    192:
      result := 'aes-192-cbc';
  else
    result := 'aes-256-cbc';
  end;
end;

{ TAesCfbOsl }

function TAesCfbOsl.OpenSslCipherName: PUtf8Char;
begin
  case fKeySize of
    128:
      result := 'aes-128-cfb';
    192:
      result := 'aes-192-cfb';
  else
    result := 'aes-256-cfb';
  end;
end;


{ TAesOfbOsl }

function TAesOfbOsl.OpenSslCipherName: PUtf8Char;
begin
  case fKeySize of
    128:
      result := 'aes-128-ofb';
    192:
      result := 'aes-192-ofb';
  else
    result := 'aes-256-ofb';
  end;
end;

{ TAesCtrNistOsl }

function TAesCtrNistOsl.OpenSslCipherName: PUtf8Char;
begin
  case fKeySize of
    128:
      result := 'aes-128-ctr';
    192:
      result := 'aes-192-ctr';
  else
    result := 'aes-256-ctr';
  end;
end;

{ TAesGcmOsl }

function TAesGcmOsl.AesGcmInit: boolean;
var
  name: PUtf8Char;
begin
  case fKeySize of
    128:
      name := 'aes-128-gcm';
    192:
      name := 'aes-192-gcm';
  else
    name := 'aes-256-gcm';
  end;
  fAes.Init(self, name);
  result := true;
end;

procedure TAesGcmOsl.AesGcmDone;
begin
  fAes.Done;
end;

procedure TAesGcmOsl.AesGcmReset;
begin
  fAes.SetEvp(fStarted = stEnc, 'AesGcmProcess');
end;

function TAesGcmOsl.AesGcmProcess(BufIn, BufOut: pointer; Count: cardinal): boolean;
begin
  fAes.UpdEvp(fStarted = stEnc, BufIn, BufOut, Count);
  result := true;
end;

procedure TAesGcmOsl.AesGcmAad(Buf: pointer; Len: integer);
begin
  fAes.UpdEvp({encrypt=}true, Buf, nil, Len);
end;

function TAesGcmOsl.AesGcmFinal(var tag: TAesBlock): boolean;
var
  outl: integer;
  dummy: TAesBlock;
begin
  case fStarted of
    stEnc:
      begin
        EOpenSslCrypto.Check(self, 'AesGcmFinal enc',
          EVP_CipherFinal_ex(fAes.Ctx[true], @dummy, @outl));
        EOpenSslCrypto.Check(self, 'AesGcmFinal enctag',
          EVP_CIPHER_CTX_ctrl(fAes.Ctx[true], EVP_CTRL_GCM_GET_TAG, 16, @tag));
        result := true;
      end;
    stDec:
      begin
        EOpenSslCrypto.Check(self, 'AesGcmFinal dectag',
          EVP_CIPHER_CTX_ctrl(fAes.Ctx[false], EVP_CTRL_GCM_SET_TAG, 16, @tag));
        outl := 16;
        result := (EVP_CipherFinal_ex(fAes.Ctx[false], @dummy, @outl) > 0) and
                  (outl = 0);
      end
  else
    result := false;
  end;
  fStarted := stNone; // allow reuse of this fAes instance
end;

function TAesGcmOsl.Clone: TAesAbstract;
begin
  result := TAesGcmOsl(NewInstance);
  fAes.Clone(result, TAesGcmOsl(result).fAes); // efficient Ctx[] copy
end;

function TAesGcmOsl.CloneEncryptDecrypt: TAesAbstract;
begin
  result := self;
end;


procedure RegisterOpenSsl;
begin
  if not OpenSslIsAvailable then
    exit;
  // so that mormot.core.ecc.pas' TEcdheProtocol could benefit from OpenSSL
  {$ifndef HASAESNI64}
  ECDHEPROT_EF2AES[efAesCtr128] := TAesCtrNistOsl;
  ECDHEPROT_EF2AES[efAesCtr256] := TAesCtrNistOsl;
  {$endif HASAESNI64}
  ECDHEPROT_EF2AES[efAesGcm128] := TAesGcmOsl;
  ECDHEPROT_EF2AES[efAesGcm256] := TAesGcmOsl;
  // for TEccCertificate.Encrypt and TEccCertificateSecret.Decrypt ECIES
  {$ifndef HASAESNI64}
  ECIES_AES[ecaPBKDF2_HMAC_SHA256_AES256_CTR] := TAesCtrNistOsl;
  ECIES_AES[ecaPBKDF2_HMAC_SHA256_AES128_CTR] := TAesCtrNistOsl;
  ECIES_AES[ecaPBKDF2_HMAC_SHA256_AES256_CTR_SYNLZ] := TAesCtrNistOsl;
  ECIES_AES[ecaPBKDF2_HMAC_SHA256_AES128_CTR_SYNLZ] := TAesCtrNistOsl;
  {$endif HASAESNI64}
  ECIES_AES[ecaPBKDF2_AES128_GCM] := TAesGcmOsl;
  ECIES_AES[ecaPBKDF2_AES256_GCM] := TAesGcmOsl;
  ECIES_AES[ecaPBKDF2_AES128_GCM_SYNLZ] := TAesGcmOsl;
  ECIES_AES[ecaPBKDF2_AES256_GCM_SYNLZ] := TAesGcmOsl;
end;


initialization

finalization
  FreeAndNil(MainAesPrngOsl);

end.
