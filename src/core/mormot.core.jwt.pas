/// Framework Core JSON Web Tokens (JWT) Support 
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.jwt;

{
  *****************************************************************************

   JSON Web Tokens (JWT) Implementation - see RFC 7797
    - High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers
    -

   Uses optimized mormot.core.crypto.pas for its process.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.json, // JSON + TDocVariant
  mormot.core.crypto;



{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ implemented in this unit and not in mormot.core.crypto, since TSynSignerParams
  expects JSON support, which requires mormot.core.json }

type
  /// the HMAC/SHA-3 algorithms known by TSynSigner
  TSignAlgo = (
    saSha1, saSha256, saSha384, saSha512,
    saSha3224, saSha3256, saSha3384, saSha3512, saSha3S128, saSha3S256);

  /// JSON-serialization ready object as used by TSynSigner.PBKDF2 overloaded methods
  // - default value for unspecified parameters will be SHAKE_128 with
  // rounds=1000 and a fixed salt
  TSynSignerParams = packed record
    algo: TSignAlgo;
    secret, salt: RawUTF8;
    rounds: integer;
  end;

  /// a generic wrapper object to handle digital HMAC-SHA-2/SHA-3 signatures
  // - used e.g. to implement TJWTSynSignerAbstract
  TSynSigner = object
  private
    ctxt: packed array[1..SHA3ContextSize] of byte; // enough space for all
    fSignatureSize: integer;
    fAlgo: TSignAlgo;
  public
    /// initialize the digital HMAC/SHA-3 signing context with some secret text
    procedure Init(aAlgo: TSignAlgo; const aSecret: RawUTF8); overload;
    /// initialize the digital HMAC/SHA-3 signing context with some secret binary
    procedure Init(aAlgo: TSignAlgo;
      aSecret: pointer; aSecretLen: integer); overload;
    /// initialize the digital HMAC/SHA-3 signing context with PBKDF2 safe
    // iterative key derivation of a secret salted text
    procedure Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; aPBKDF2Secret: PHash512Rec = nil); overload;
    /// process some message content supplied as memory buffer
    procedure Update(aBuffer: pointer; aLen: integer); overload;
    /// process some message content supplied as string
    procedure Update(const aBuffer: RawByteString); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// returns the computed digital signature as lowercase hexadecimal text
    function final: RawUTF8; overload;
    /// returns the raw computed digital signature
    // - SignatureSize bytes will be written: use Signature.Lo/h0/b3/b accessors
    procedure Final(out aSignature: THash512Rec;
      aNoInit: boolean = false); overload;
    /// one-step digital signature of a buffer as lowercase hexadecimal string
    function Full(aAlgo: TSignAlgo; const aSecret: RawUTF8;
      aBuffer: Pointer; aLen: integer): RawUTF8; overload;
    /// one-step digital signature of a buffer with PBKDF2 derivation
    function Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; aBuffer: Pointer; aLen: integer): RawUTF8; overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure PBKDF2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure PBKDF2(const aParams: TSynSignerParams;
      out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure PBKDF2(aParamsJSON: PUTF8Char; aParamsJSONLen: integer;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUTF8 = 'I6sWioAidNnhXO9BK';
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure PBKDF2(const aParamsJSON: RawUTF8;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUTF8 = 'I6sWioAidNnhXO9BK';
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// prepare a TAES object with the key derivated via a PBKDF2() call
    // - aDerivatedKey is defined as "var", since it will be zeroed after use
    procedure AssignTo(var aDerivatedKey: THash512Rec;
      out aAES: TAES; aEncrypt: boolean);
    /// fill the intenral context with zeros, for security
    procedure Done;
    /// the algorithm used for digitial signature
    property Algo: TSignAlgo read fAlgo;
    /// the size, in bytes, of the digital signature of this algorithm
    // - potential values are 20, 28, 32, 48 and 64
    property SignatureSize: integer read fSignatureSize;
  end;

  /// reference to a TSynSigner wrapper object
  PSynSigner = ^TSynSigner;


  /// hash algorithms available for HashFile/HashFull functions
  // and TSynHasher object
  THashAlgo = (
    hfMD5, hfSHA1, hfSHA256, hfSHA384, hfSHA512, hfSHA3_256, hfSHA3_512);

  /// set of algorithms available for HashFile/HashFull functions and TSynHasher object
  THashAlgos = set of THashAlgo;

  /// convenient multi-algorithm hashing wrapper
  // - as used e.g. by HashFile/HashFull functions
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  TSynHasher = object
  private
    fAlgo: THashAlgo;
    ctxt: array[1..SHA3ContextSize] of byte; // enough space for all algorithms
  public
    /// initialize the internal hashing structure for a specific algorithm
    // - returns false on unknown/unsupported algorithm
    function Init(aAlgo: THashAlgo): boolean;
    /// hash the supplied memory buffer
    procedure Update(aBuffer: Pointer; aLen: integer); overload;
    /// hash the supplied string content
    procedure Update(const aBuffer: RawByteString); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// returns the resulting hash as lowercase hexadecimal string
    function final: RawUTF8;
    /// one-step hash computation of a buffer as lowercase hexadecimal string
    function Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
    /// the hash algorithm used by this instance
    property Algo: THashAlgo read fAlgo;
  end;

function ToText(algo: TSignAlgo): PShortString; overload;
function ToText(algo: THashAlgo): PShortString; overload;

/// compute the hexadecimal hash of any (big) file
// - using a temporary buffer of 1MB for the sequential reading
function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUTF8; overload;

/// compute the hexadecimal hashe(s) of one file, as external .md5/.sha256/.. files
// - reading the file once in memory, then apply all algorithms on it and
// generate the text hash files in the very same folder
procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos); overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;



implementation



{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ TSynHasher }

function TSynHasher.Init(aAlgo: THashAlgo): boolean;
begin
  fAlgo := aAlgo;
  result := true;
  case aAlgo of
    hfMD5:
      PMD5(@ctxt)^.Init;
    hfSHA1:
      PSHA1(@ctxt)^.Init;
    hfSHA256:
      PSHA256(@ctxt)^.Init;
    hfSHA384:
      PSHA384(@ctxt)^.Init;
    hfSHA512:
      PSHA512(@ctxt)^.Init;
    hfSHA3_256:
      PSHA3(@ctxt)^.Init(SHA3_256);
    hfSHA3_512:
      PSHA3(@ctxt)^.Init(SHA3_512);
  else
    result := false;
  end;
end;

procedure TSynHasher.Update(aBuffer: Pointer; aLen: integer);
begin
  case fAlgo of
    hfMD5:
      PMD5(@ctxt)^.Update(aBuffer^, aLen);
    hfSHA1:
      PSHA1(@ctxt)^.Update(aBuffer, aLen);
    hfSHA256:
      PSHA256(@ctxt)^.Update(aBuffer, aLen);
    hfSHA384:
      PSHA384(@ctxt)^.Update(aBuffer, aLen);
    hfSHA512:
      PSHA512(@ctxt)^.Update(aBuffer, aLen);
    hfSHA3_256:
      PSHA3(@ctxt)^.Update(aBuffer, aLen);
    hfSHA3_512:
      PSHA3(@ctxt)^.Update(aBuffer, aLen);
  end;
end;

procedure TSynHasher.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer), length(aBuffer));
end;

function TSynHasher.final: RawUTF8;
begin
  case fAlgo of
    hfMD5:
      result := MD5DigestToString(PMD5(@ctxt)^.final);
    hfSHA1:
      result := SHA1DigestToString(PSHA1(@ctxt)^.final);
    hfSHA256:
      result := SHA256DigestToString(PSHA256(@ctxt)^.final);
    hfSHA384:
      result := SHA384DigestToString(PSHA384(@ctxt)^.final);
    hfSHA512:
      result := SHA512DigestToString(PSHA512(@ctxt)^.final);
    hfSHA3_256:
      result := SHA256DigestToString(PSHA3(@ctxt)^.Final256);
    hfSHA3_512:
      result := SHA512DigestToString(PSHA3(@ctxt)^.Final512);
  end;
end;

function TSynHasher.Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo);
  Update(aBuffer, aLen);
  result := final;
end;

function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
var
  hasher: TSynHasher;
begin
  result := hasher.Full(aAlgo, aBuffer, aLen);
end;

function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUTF8;
var
  hasher: TSynHasher;
  temp: RawByteString;
  F: THandle;
  size: Int64;
  read: cardinal;
begin
  result := '';
  if (aFileName = '') or not hasher.Init(aAlgo) then
    exit;
  F := FileOpenSequentialRead(aFileName);
  if PtrInt(F) >= 0 then
  try
    size := FileSize(F);
    SetLength(temp, 1 shl 20); // 1MB temporary buffer for reading
    while size > 0 do
    begin
      read := FileRead(F, pointer(temp)^, 1 shl 20);
      if read <= 0 then
        exit;
      hasher.Update(pointer(temp), read);
      dec(size, read);
    end;
    result := hasher.final;
  finally
    FileClose(F);
  end;
end;

procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos);
var
  data, hash: RawUTF8;
  efn, fn: string;
  a: THashAlgo;
begin
  if aAlgos = [] then
    exit;
  efn := ExtractFileName(aFileName);
  data := StringFromFile(aFileName);
  if data <> '' then
    for a := low(a) to high(a) do
      if a in aAlgos then
      begin
        FormatUTF8('% *%',
          [HashFull(a, pointer(data), length(data)), efn], hash);
        FormatString('%.%',
          [efn, LowerCase(TrimLeftLowerCaseShort(ToText(a)))], fn);
        FileFromString(hash, fn);
      end;
end;


{ TSynSigner }

procedure TSynSigner.Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer);
const
  SIGN_SIZE: array[TSignAlgo] of byte = (
    20, 32, 48, 64, 28, 32, 48, 64, 32, 64);
  SHA3_ALGO: array[saSha3224..saSha3S256] of TSHA3Algo = (
    SHA3_224, SHA3_256, SHA3_384, SHA3_512, SHAKE_128, SHAKE_256);
begin
  fAlgo := aAlgo;
  fSignatureSize := SIGN_SIZE[fAlgo];
  case fAlgo of
    saSha1:
      PHMAC_SHA1(@ctxt)^.Init(aSecret, aSecretLen);
    saSha256:
      PHMAC_SHA256(@ctxt)^.Init(aSecret, aSecretLen);
    saSha384:
      PHMAC_SHA384(@ctxt)^.Init(aSecret, aSecretLen);
    saSha512:
      PHMAC_SHA512(@ctxt)^.Init(aSecret, aSecretLen);
    saSha3224..saSha3S256:
      begin
        PSHA3(@ctxt)^.Init(SHA3_ALGO[fAlgo]);
        PSHA3(@ctxt)^.Update(aSecret, aSecretLen);
      end; // note: the HMAC pattern is included in SHA-3
  end;
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret: RawUTF8);
begin
  Init(aAlgo, pointer(aSecret), length(aSecret));
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; aPBKDF2Secret: PHash512Rec);
var
  temp: THash512Rec;
begin
  if aSecretPBKDF2Rounds > 1 then
  begin
    PBKDF2(aAlgo, aSecret, aSalt, aSecretPBKDF2Rounds, temp);
    Init(aAlgo, @temp, fSignatureSize);
    if aPBKDF2Secret <> nil then
      aPBKDF2Secret^ := temp;
    FillZero(temp.b);
  end
  else
    Init(aAlgo, aSecret);
end;

procedure TSynSigner.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer), length(aBuffer));
end;

procedure TSynSigner.Update(aBuffer: pointer; aLen: integer);
begin
  case fAlgo of
    saSha1:
      PHMAC_SHA1(@ctxt)^.Update(aBuffer, aLen);
    saSha256:
      PHMAC_SHA256(@ctxt)^.Update(aBuffer, aLen);
    saSha384:
      PHMAC_SHA384(@ctxt)^.Update(aBuffer, aLen);
    saSha512:
      PHMAC_SHA512(@ctxt)^.Update(aBuffer, aLen);
    saSha3224..saSha3S256:
      PSHA3(@ctxt)^.Update(aBuffer, aLen);
  end;
end;

procedure TSynSigner.Final(out aSignature: THash512Rec; aNoInit: boolean);
begin
  case fAlgo of
    saSha1:
      PHMAC_SHA1(@ctxt)^.Done(aSignature.b160, aNoInit);
    saSha256:
      PHMAC_SHA256(@ctxt)^.Done(aSignature.Lo, aNoInit);
    saSha384:
      PHMAC_SHA384(@ctxt)^.Done(aSignature.b384, aNoInit);
    saSha512:
      PHMAC_SHA512(@ctxt)^.Done(aSignature.b, aNoInit);
    saSha3224..saSha3S256:
      PSHA3(@ctxt)^.Final(@aSignature, fSignatureSize shl 3, aNoInit);
  end;
end;

function TSynSigner.final: RawUTF8;
var
  sig: THash512Rec;
begin
  Final(sig);
  result := BinToHexLower(@sig, fSignatureSize);
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret: RawUTF8;
  aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo, aSecret);
  Update(aBuffer, aLen);
  result := final;
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo, aSecret, aSalt, aSecretPBKDF2Rounds);
  Update(aBuffer, aLen);
  result := final;
end;

procedure TSynSigner.PBKDF2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; out aDerivatedKey: THash512Rec);
var
  iter: TSynSigner;
  temp: THash512Rec;
  i: integer;
begin
  Init(aAlgo, aSecret);
  iter := self;
  iter.Update(aSalt);
  if fAlgo < saSha3224 then
    iter.Update(#0#0#0#1); // padding and XoF mode already part of SHA-3 process
  iter.Final(aDerivatedKey, true);
  if aSecretPBKDF2Rounds < 2 then
    exit;
  temp := aDerivatedKey;
  for i := 2 to aSecretPBKDF2Rounds do
  begin
    iter := self;
    iter.Update(@temp, fSignatureSize);
    iter.Final(temp, true);
    XorMemory(@aDerivatedKey, @temp, fSignatureSize);
  end;
  FillZero(temp.b);
  FillCharFast(iter.ctxt, SizeOf(iter.ctxt), 0);
  FillCharFast(ctxt, SizeOf(ctxt), 0);
end;

procedure TSynSigner.PBKDF2(const aParams: TSynSignerParams;
  out aDerivatedKey: THash512Rec);
begin
  PBKDF2(aParams.algo, aParams.secret, aParams.salt, aParams.rounds, aDerivatedKey);
end;

procedure TSynSigner.PBKDF2(aParamsJSON: PUTF8Char; aParamsJSONLen: integer;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUTF8; aDefaultAlgo: TSignAlgo);
var
  tmp: TSynTempBuffer;
  k: TSynSignerParams;

  procedure SetDefault;
  begin
    k.algo := aDefaultAlgo;
    k.secret := '';
    k.salt := aDefaultSalt;
    k.rounds := 1000;
  end;

begin
  SetDefault;
  if (aParamsJSON = nil) or (aParamsJSONLen <= 0) then
    k.secret := aDefaultSalt
  else if aParamsJSON[1] <> '{' then
    FastSetString(k.secret, aParamsJSON, aParamsJSONLen)
  else
  begin
    tmp.Init(aParamsJSON, aParamsJSONLen);
    try
      if (RecordLoadJSON(k, tmp.buf, TypeInfo(TSynSignerParams)) = nil) or
         (k.secret = '') or (k.salt = '') then
      begin
        SetDefault;
        FastSetString(k.secret, aParamsJSON, aParamsJSONLen);
      end;
    finally
      FillCharFast(tmp.buf^, tmp.len, 0);
      tmp.Done;
    end;
  end;
  PBKDF2(k.algo, k.secret, k.salt, k.rounds, aDerivatedKey);
  FillZero(k.secret);
end;

procedure TSynSigner.PBKDF2(const aParamsJSON: RawUTF8;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUTF8; aDefaultAlgo: TSignAlgo);
begin
  PBKDF2(pointer(aParamsJSON), length(aParamsJSON),
    aDerivatedKey, aDefaultSalt, aDefaultAlgo);
end;

procedure TSynSigner.AssignTo(var aDerivatedKey: THash512Rec;
  out aAES: TAES; aEncrypt: boolean);
var
  ks: integer;
begin
  case algo of
    saSha3S128:
      ks := 128; // truncate to Keccak sponge precision
    saSha3S256:
      ks := 256;
  else
    case SignatureSize of
      20:
        begin
          ks := 128;
          aDerivatedKey.i0 := aDerivatedKey.i0 xor aDerivatedKey.i4;
        end;
      28:
        ks := 192;
      32:
        ks := 256;
      48:
        begin
          ks := 256;
          aDerivatedKey.d0 := aDerivatedKey.d0 xor aDerivatedKey.d4;
          aDerivatedKey.d1 := aDerivatedKey.d1 xor aDerivatedKey.d5;
        end;
      64:
        begin
          ks := 256;
          aDerivatedKey.d0 := aDerivatedKey.d0 xor aDerivatedKey.d4;
          aDerivatedKey.d1 := aDerivatedKey.d1 xor aDerivatedKey.d5;
          aDerivatedKey.d2 := aDerivatedKey.d0 xor aDerivatedKey.d6;
          aDerivatedKey.d3 := aDerivatedKey.d1 xor aDerivatedKey.d7;
        end;
    else
      exit;
    end;
  end;
  aAES.DoInit(aDerivatedKey, ks, aEncrypt);
  FillZero(aDerivatedKey.b);
end;

procedure TSynSigner.Done;
begin
  FillCharFast(self, SizeOf(self), 0);
end;

function ToText(algo: TSignAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(TSignAlgo), ord(algo));
end;

function ToText(algo: THashAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(THashAlgo), ord(algo));
end;



procedure InitializeUnit;
begin
  RttiCustom.RegisterType(TypeInfo(TSignAlgo));
  RttiCustom.RegisterFromText(TypeInfo(TSynSignerParams),
    'algo:TSignAlgo secret,salt:RawUTF8 rounds:integer');
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
end.
