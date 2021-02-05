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

  Our mormot.core.crypto.pas unit is stand-alone and faster than OpenSSL for
  most algorithms, but AES-CTR and AES-GCM. For those two, you may try this unit.

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
  /// handle AES cypher/uncypher with chaining with OpenSSL 1.1
  // - we abbreviate OpenSsl as Osl for class names for brevity
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAesEcbOsl, TAesCbcOsl, TAesCfbOsl, TAesOfbOsl and TAesCtrOsl
  // classes to handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  // - those classes are re-entrant, i.e. that you can call the Encrypt*
  // or Decrypt* methods on the same instance several times
  TAesAbstractOsl = class(TAesAbstract)
  protected
    fCipher: PEVP_CIPHER; // computed from OpenSslCipherName virtual method
    fCtx: array[boolean] of PEVP_CIPHER_CTX; // set and reused in CallEvp()
    procedure AfterCreate; override; // circumvent Delphi bug about const aKey
    procedure CallEvp(BufIn, BufOut: pointer; Count: cardinal;
      DoEncrypt: boolean; const method: string);
  public
    /// creates a new instance with the very same values
    function Clone: TAesAbstract; override;
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
  // - OpenSSL parallelize its process, so is faster than our TAesCrtNist class:
  // $ 2500 aes128ctr in 9.39ms i.e. 266212/s or 566.5 MB/s
  // $ 2500 aes128ctrosl in 2.61ms i.e. 956754/s or 1.9 GB/s
  // $ 2500 aes256ctr in 11.94ms i.e. 209275/s or 445.3 MB/s
  // $ 2500 aes256ctrosl in 3.29ms i.e. 759878/s or 1.5 GB/s
  TAesCtrNistOsl = class(TAesAbstractOsl)
  public
    /// return the OpenSSL EVP Cipher name of this class and key size
    function OpenSslCipherName: PUtf8Char; override;
  end;


implementation



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
  EOpenSslCrypto.CheckAvailable(PClass(self)^, 'Create');
  fCipher := EVP_get_cipherbyname(OpenSslCipherName);
  if fCipher = nil then
    raise EOpenSslCrypto.CreateFmt(
      '%s.Create: ''%s'' cipher is unknown',
        [ClassNameShort(self)^, OpenSslCipherName]);
end;

destructor TAesAbstractOsl.Destroy;
begin
  if fCtx[false] <> nil then
    EVP_CIPHER_CTX_free(fCtx[false]);
  if fCtx[true] <> nil then
    EVP_CIPHER_CTX_free(fCtx[true]);
  inherited Destroy;
end;

class function TAesAbstractOsl.IsAvailable: boolean;
begin
  result := OpenSslIsAvailable;
end;

procedure TAesAbstractOsl.CallEvp(BufIn, BufOut: pointer; Count: cardinal;
  DoEncrypt: boolean; const method: string);
var
  ctx: PEVP_CIPHER_CTX;
  outl: integer;
begin
  if Count and AesBlockMod <> 0 then
    raise ESynCrypto.CreateUtf8('%.%: Count=% is not a multiple of 16',
      [self, method, Count]);
  ctx := fCtx[DoEncrypt];
  if ctx <> nil then
    EOpenSslCrypto.Check(self, method,
      EVP_CIPHER_CTX_reset(ctx)) // reuse the context
  else
  begin
    ctx := EVP_CIPHER_CTX_new;
    fCtx[DoEncrypt] := ctx;
  end;
  EOpenSslCrypto.Check(self, method,
    EVP_CipherInit_ex(ctx, fCipher, nil, @fKey, @fIV, ord(DoEncrypt)));
  EOpenSslCrypto.Check(self, method,
    EVP_CIPHER_CTX_set_padding(ctx, 0)); // we do the padding internally
  EOpenSslCrypto.Check(self, method,
    EVP_CipherUpdate(ctx, BufOut, @outl, BufIn, Count));
  EOpenSslCrypto.Check(self, method,
    EVP_CipherFinal_ex(ctx, BufOut, @outl));
end;

function TAesAbstractOsl.Clone: TAesAbstract;
begin
  if fIVHistoryDec.Count <> 0 then
    result := inherited Clone
  else
  begin
    // we can reuse the main parameters
    result := TAesAbstractOsl(NewInstance);
    TAesAbstractOsl(result).fKeySize := fKeySize;
    TAesAbstractOsl(result).fKeySizeBytes := fKeySizeBytes;
    MoveFast(fKey, TAesAbstractOsl(result).fKey, fKeySizeBytes);
    TAesAbstractOsl(result).fCipher := fCipher;
  end;
end;

procedure TAesAbstractOsl.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  CallEvp(BufIn, BufOut, Count, {doencrypt=}true, 'Encrypt');
end;

procedure TAesAbstractOsl.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  CallEvp(BufIn, BufOut, Count, {doencrypt=}false, 'Decrypt');
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



initialization

finalization
  FreeAndNil(MainAesPrngOsl);

end.
