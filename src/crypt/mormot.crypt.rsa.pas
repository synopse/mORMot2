/// Framework Core RSA Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.rsa;

{
  *****************************************************************************

   Rivest-Shamir-Adleman (RSA) Public-Key Cryptography
    - RSA Oriented Big-Integer Computation
    - RSA Low-Level Cryptography Functions

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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.crypt.core,
  mormot.crypt.secure;
  
{
  Implementation notes:
  - loosely based on fpTLSBigInt / fprsa units from the FPC RTL - but the whole
    design and core methods have been rewritten from scratch in modern OOP
  - we use half-registers (HalfUInt) for efficient computation on most systems
  - TODO: includes proper RSA keypair generation
}

{ **************** RSA Oriented Big-Integer Computation }

type
  /// exception class raised by this unit
  ERsaException = class(ESynException);

const
  /// number of bytes in a HalfUInt, i.e. 2 on CPU32 and 4 on CPU64
  HALF_BYTES = SizeOf(HalfUInt);
  /// number of bits in a HalfUInt, i.e. 16 on CPU32 and 32 on CPU64
  HALF_BITS = HALF_BYTES * 8;
  /// number of power of two bits in a HalfUInt, i.e. 4 on CPU32 and 5 on CPU64
  HALF_SHR = {$ifdef CPU32} 4 {$else} 5 {$endif};

  /// maximum HalfUInt value + 1
  RSA_RADIX = PtrUInt({$ifdef CPU32} $10000 {$else} $100000000 {$endif});
  /// maximum PtrUInt value - 1
  RSA_MAX = PtrUInt(-1);

type
  PBigInt = ^TBigInt;
  PPBigInt = ^PBigInt;

  TRsaContext = class;

  /// store one Big Integer value with proper COW support
  // - each value is owned as PBigInt by an associated TRsaContext instance
  // - you should call TBigInt.Release() once done with any instance
  {$ifdef USERECORDWITHMETHODS}
  TBigInt = record
  {$else}
  TBigInt = object
  {$endif USERECORDWITHMETHODS}
  private
    fNextFree: PBigInt; // next bigint in the Owner free instance cache
    procedure Resize(n: integer; nozero: boolean = false);
    function FindMaxExponentIndex: integer;
    function SetPermanent: PBigInt;
    procedure ResetPermanent;
    function TruncateMod(modulus: integer): PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// the associated Big Integer RSA context
    // - used to store modulo constants, and maintain an internal instance cache
    Owner: TRsaContext;
    /// number of HalfUInt in this Big Integer value
    Size: integer;
    /// number of HalfUInt allocated for this Big Integer value
    Capacity: integer;
    /// internal reference counter
    // - equals -1 for permanent/constant storage
    RefCnt: integer;
    /// raw access to the actual HalfUInt data
    Value: PHalfUIntArray;
    /// comparison with another Big Integer value
    // - values should have been Trim-med for the size to match
    function Compare(b: PBigInt): integer;
    /// make a COW instance, increasing RefCnt and returning self
    function Copy: PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// allocate a new Big Integer value with the same data as an existing one
    function Clone: PBigInt;
    /// decreases the value RefCnt, saving it in the internal FreeList once done
    procedure Release;
    /// a wrapper to ResetPermanent then Release
    procedure ResetPermanentAndRelease;
    /// export a Big Integer value into a binary buffer
    procedure Save(data: PByteArray; bytes: integer; andrelease: boolean); overload;
    /// export a Big Integer value into a binary RawByteString
    function Save(andrelease: boolean = false): RawByteString; overload;
    /// delete any meaningless leading zeros and return self
    function Trim: PBigInt;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// quickly search if contains 0
    function IsZero: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if a given bit is set to 1
    function BitIsSet(bit: PtrUInt): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// search the position of the first bit set
    function BitCount: integer;
    /// shift right the internal data HalfUInt by a number of slots
    function RightShift(n: integer): PBigInt;
    /// shift left the internal data HalfUInt by a number of slots
    function LeftShift(n: integer): PBigInt;
    /// compute the sum of two Big Integer values
    // - returns self := self + b as result
    // - will eventually release the b instance
    function Add(b: PBigInt): PBigInt;
    /// compute the difference of two Big Integer values
    // - returns self := abs(self - b) as result, and NegativeResult^ as its sign
    // - will eventually release the b instance
    function Substract(b: PBigInt; NegativeResult: PBoolean = nil): PBigInt;
    /// division or modulo computation
    // - self is the numerator
    // - if ComputeMod is false, v is the denominator; otherwise, is the modulus
    // - will eventually release the v instance
    function Divide(v: PBigInt; ComputeMod: boolean = false): PBigInt;
    /// standard multiplication between two Big Integer values
    // - will eventually release both self and b instances
    function Multiply(b: PBigInt; InnerPartial: PtrInt = 0;
      OuterPartial: PtrInt = 0): PBigInt;
    /// standard multiplication by itself
    // - will allocate a new Big Integer value and release self
    function Square: PBigint;
      {$ifdef HASINLINE} inline; {$endif}
    /// multiply by an unsigned integer value
    // - returns self := self * b
    // - will eventually release the self instance
    function IntMultiply(b: HalfUInt): PBigInt;
    /// divide by an unsigned integer value
    // - returns self := self div b
    // - optionally return self mod b
    function IntDivide(b: HalfUInt; modulo: PHalfUInt = nil): PBigInt;
    /// modulo by 10 computation
    // - computes self := self div 10 and return self mod 10
    function IntMod10: PtrUInt;
    /// return the Big Integer value as hexadecimal
    function ToHexa: RawUtf8;
    /// return the Big Integer value as text with base-10 digits
    // - self will remain untouched unless noclone is set
    function ToText(noclone: boolean = false): RawUtf8;
  end;

  /// define Normal, P and Q pre-computed modulos
  TRsaModulo = (
    rmM,
    rmP,
    rmQ);

  /// store Normal, P and Q pre-computed modulos as PBigInt
  TRsaModulos = array[TRsaModulo] of PBigInt;

  /// store one Big Integer computation context for RSA
  // - will maintain its own set of reference-counted Big Integer values
  TRsaContext = class(TObjectWithCustomCreate)
  private
    /// list of released PBigInt instance, ready to be re-used by Allocate()
    fFreeList: PBigInt;
    /// the radix used
    fRadix: PBigInt;
    /// contains Modulus
    fMod: TRsaModulos;
    /// contains mu
    fMu: TRsaModulos;
    /// contains b(k+1)
    fBk1: TRsaModulos;
    /// contains the normalized storage
    fNormMod: TRsaModulos;
    procedure ForceRelease(p: PPBigInt; n: integer = 1);
  public
    /// the size of the sliding window
    Window: integer;
    /// number of active PBigInt
    ActiveCount: integer;
    /// number of PBigInt instances stored in the internal instances cache
    FreeCount: integer;
    /// as set by SetModulo() and  used by Barret() and ModPower()
    CurrentModulo: TRsaModulo;
    /// initialize this Big Integer context
    constructor Create; override;
    /// finalize this Big Integer context memory
    destructor Destroy; override;
    /// allocate a new zeroed Big Integer value of the specified precision
    // - n is the number of TBitInt.Value[] items to initialize
    function Allocate(n: integer; nozero: boolean = false): PBigint;
    /// allocate a new Big Integer value from a 16/32-bit unsigned integer
    function AllocateFrom(v: HalfUInt): PBigInt;
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function Load(data: PByteArray; bytes: integer): PBigInt; overload;
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function Load(const data: RawByteString): PBigInt; overload;
    /// pre-compute some of the internal constant slots for a given modulo
    procedure SetModulo(b: PBigInt; modulo: TRsaModulo);
    /// release the internal constant slots for a given modulo
    procedure ResetModulo(modulo: TRsaModulo);
    /// compute the Barret reduction of a Big Integer value
    // - SetModulo() should have previously called
    function Barret(b: PBigint): PBigInt;
    /// compute a modular exponentiation
    // - SetModulo() should have previously be called
    function ModPower(b, exp: PBigInt): PBigInt;
    /// compute the Chinese Remainder Theorem as needed by quick RSA decrypts
    function ChineseRemainderTheorem(b, dp, dq, p, q, qinv: PBigInt): PBigInt;
  end;

const
  BIGINT_ZERO_VALUE: HalfUInt = 0;
  BIGINT_ONE_VALUE:  HalfUInt = 1;

  /// constant 0 as Big Integer value
  BIGINT_ZERO: TBigInt = (
    Size: {%H-}1;
    RefCnt: {%H-}-1;
    Value: {%H-}@BIGINT_ZERO_VALUE);

  /// constant 1 as Big Integer value
  BIGINT_ONE: TBigInt = (
    Size: {%H-}1;
    RefCnt: {%H-}-1;
    Value: {%H-}@BIGINT_ONE_VALUE);

/// branchless comparison of two Big Integer values
function CompareBI(A, B: HalfUInt): integer;
  {$ifdef HASINLINE} inline; {$endif}


{ **************** RSA Low-Level Cryptography Functions }

type
  /// store a RSA public key
  // - with DER (therefore PEM) serialization support
  // - e.g. as decoded from an X509 certificate
  {$ifdef USERECORDWITHMETHODS}
  TRsaPublicKey = record
  {$else}
  TRsaPublicKey = object
  {$endif USERECORDWITHMETHODS}
  public
    /// RSA key Modulus m or n
    Modulus: RawByteString;
    /// RSA key Public exponent e (typically 65537)
    Exponent: RawByteString;
    /// serialize this public key as binary PKCS#1 DER format
    function ToDer: TCertDer;
    /// unserialize a public key from binary PKCS#1 DER format
    function FromDer(const der: TCertDer): boolean;
  end;

  /// store a RSA private key
  // - with DER (therefore PEM) serialization support
  // - we don't support any PKCS encryption yet - ensure the private key
  // PEM file is safely stored with proper access restrictions
  {$ifdef USERECORDWITHMETHODS}
  TRsaPrivateKey = record
  {$else}
  TRsaPrivateKey = object
  {$endif USERECORDWITHMETHODS}
  public
    /// field layout is typically 0
    Version: integer;
    /// RSA key m or n
    Modulus: RawByteString;
    /// RSA key Public exponent e (typically 65537)
    PublicExponent: RawByteString;
    /// RSA key Private exponent d
    PrivateExponent: RawByteString;
    /// RSA key prime factor p of n
    Prime1: RawByteString;
    /// RSA key prime factor q of n
    Prime2: RawByteString;
    /// RSA key d mod (p - 1)
    Exponent1: RawByteString;
    /// RSA key d mod (q - 1)
    Exponent2: RawByteString;
    /// RSA key CRT coefficient q^(-1) mod p
    Coefficient: RawByteString;
    /// serialize this private key as binary DER format
    // - note that the layout follows PKCS#8 "openssl genrsa -out priv.pem 2048"
    // layout, but not "A.1.2. RSA Private Key Syntax" PKCS#1 as of RFC 8017
    function ToDer: TCertDer;
    /// unserialize a private key from binary DER format
    // - will recognize both PCKS#8 "openssl genrsa -out priv.pem 2048" ASN.1
    // layout and "A.1.2. RSA Private Key Syntax" PKCS#1 as of RFC 8017
    function FromDer(const der: TCertDer): boolean;
    /// check if this private key match a given public key
    function Match(const Pub: TRsaPublicKey): boolean;
  end;

  /// store the information of a RSA key
  // - holding all its PBigInt values in its parent TRsaContext
  TRsa = class(TRsaContext)
  protected
    fM, fE, fD, fP, fQ, fDP, fDQ, fQInv: PBigInt;
    fModulusLen, fModulusBits: integer;
  public
    /// finalize the internal memory
    destructor Destroy; override;
    /// load a public key from raw binary buffers
    // - fill M and E fields from the supplied binary buffers
    procedure LoadFromPublicKeyBinary(Modulus, Exponent: pointer;
      ModulusSize, ExponentSize: PtrInt);
    /// load a public key from PKCS#1 DER format
    function LoadFromPublicKeyDer(const Der: TCertDer): boolean;
    /// load a public key from PKCS#1 PEM format
    function LoadFromPublicKeyPem(const Pem: TCertPem): boolean;
    /// load a public key from an hexadecimal M and E fields concatenation
    procedure LoadFromPublicKeyHexa(const Hexa: RawUtf8);
    /// load a public key from a decoded TRsaPublicKey record
    procedure LoadFromPublicKey(const PublicKey: TRsaPublicKey);
    /// load a public key from a decoded TRsaPrivateKey record
    procedure LoadFromPrivateKey(const PrivateKey: TRsaPrivateKey);
    /// RSA key Modulus
    property M: PBigInt
      read fM;
    /// RSA key Public exponent (typically 65537)
    property E: PBigInt
      read fE;
    /// RSA key Private exponent
    property D: PBigInt
      read fD;
    /// RSA key as p in m = pq
    property P: PBigInt
      read fP;
    /// RSA key as q in m = pq
    property Q: PBigInt
      read fQ;
    /// RSA key as d mod (p-1)
    property DP: PBigInt
      read fDP;
    /// RSA key as d mod (q-1)
    property DQ: PBigInt
      read fDQ;
    /// RSA key as q^-1 mod p
    property QInv: PBigInt
      read fQInv;
    /// RSA modulus size in bytes
    property ModulusLen: integer
      read fModulusLen;
  published
    /// RSA modulus size in bits
    property ModulusBits: integer
      read fModulusBits;
  end;


implementation


{ **************** RSA Oriented Big-Integer Computation }

function Min(a, b: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if a < b then
    result := a
  else
    result := b;
end;

function Max(a, b: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if a > b then
    result := a
  else
    result := b;
end;

function CompareBI(A, B: HalfUInt): integer;
begin
  result := ord(A > B) - ord(A < B);
end;

function ValuesSize(bytes: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := (bytes + (HALF_BYTES - 1)) div HALF_BYTES;
end;

procedure ValuesLoad(value: PHalfUIntArray; data: PByteArray; bytes: integer);
var
  i, o: PtrInt;
  j: byte;
begin
  j := 0;
  o := 0;
  for i := bytes - 1 downto 0 do
  begin
    inc(Value[o], HalfUInt(data[i]) shl j);
    inc(j, 8);
    if j = HALF_BITS then
    begin
      j := 0;
      inc(o);
    end;
  end;
end;


{ TBigInt }

procedure TBigInt.Resize(n: integer; nozero: boolean);
begin
  if n > Capacity then
  begin
    Capacity := NextGrow(n); // reserve a bit more for faster size-up
    ReAllocMem(Value, Capacity * HALF_BYTES);
  end;
  if not nozero and
     (n > Size) then
    FillCharFast(Value[Size], (n - Size) * HALF_BYTES, 0);
  Size := n;
end;

function TBigInt.Trim: PBigInt;
var
  n: PtrInt;
begin
  n := Size;
  while (n > 1) and
        (Value[n - 1] = 0) do // delete any leading 0
    dec(n);
  Size := n;
  result := @self;
end;

function TBigInt.IsZero: boolean;
var
  i: PtrInt;
  p: PHalfUIntArray;
begin
  if @self <> nil then
  begin
    p := Value;
    if p <> nil then
    begin
      result := false;
      for i := 0 to Size - 1 do
        if p[i] <> 0 then
          exit;
    end;
  end;
  result := true;
end;

function TBigInt.BitIsSet(bit: PtrUInt): boolean;
begin
  result := Value[bit shr HALF_SHR] and
              (1 shl (bit and pred(HALF_BITS))) <> 0;
end;

function TBigInt.BitCount: integer;
var
  i: PtrInt;
  c: HalfUInt;
begin
  result := 0;
  i := Size - 1;
  while Value[i] = 0 do
  begin
    dec(i);
    if i < 0 then
      exit;
  end;
  result := i * HALF_BITS;
  c := Value[i];
  repeat
    inc(result);
    c := c shr 1;
  until c = 0;
end;

function TBigInt.Compare(b: PBigInt): integer;
var
  i: PtrInt;
begin
  result := CompareInteger(Size, b^.Size);
  if result <> 0 then
    exit;
  for i := Size - 1 downto 0 do
  begin
    result := CompareBI(Value[i], b^.Value[i]);
    if result <> 0 then
      exit;
  end;
end;

function TBigInt.SetPermanent: PBigInt;
begin
  if RefCnt <> 1 then
    raise ERsaException.CreateUtf8(
      'TBigInt.SetPermanent(%): RefCnt=%', [@self, RefCnt]);
  RefCnt := -1;
  result := @self;
end;

procedure TBigInt.ResetPermanent;
begin
  if RefCnt >= 0 then
    raise ERsaException.CreateUtf8(
      'TBigInt.ResetPermanent(%): RefCnt=%', [@self, RefCnt]);
  RefCnt := 1;
end;

function TBigInt.RightShift(n: integer): PBigInt;
begin
  if n > 0 then
  begin
    dec(Size, n);
    if Size <= 0 then
    begin
      Size := 1;
      Value[0] := 0;
    end
    else
      MoveFast(Value[n], Value[0], Size * HALF_BYTES);
  end;
  result := @self;
end;

function TBigInt.LeftShift(n: integer): PBigInt;
var
  s: integer;
begin
  if n > 0 then
  begin
    s := Size;
    Resize(s + n, {nozero=}true);
    MoveFast(Value[0], Value[n], s * HALF_BYTES);
    FillCharFast(Value[0], n * HALF_BYTES, 0);
  end;
  result := @self;
end;

function TBigInt.TruncateMod(modulus: integer): PBigInt;
begin
  if Size > modulus then
    Size := modulus;
  result := @self;
end;

function TBigInt.Copy: PBigInt;
begin
  if RefCnt >= 0 then
    inc(RefCnt);
  result := @self;
end;

function TBigInt.FindMaxExponentIndex: integer;
var
  mask, v: HalfUInt;
begin
  result := HALF_BITS - 1;
  mask := RSA_RADIX shr 1;
  v := Value[Size - 1];
  repeat
    if (v and mask) <> 0 then
      break;
    mask := mask shr 1;
    dec(result);
    if result < 0 then
      exit;
  until false;
  inc(result, (Size - 1) * HALF_BITS);
end;


procedure TBigInt.Release;
begin
  if (@self = nil) or
     (RefCnt < 0) then
    exit; // void or permanent
  dec(RefCnt);
  if RefCnt > 0 then
    exit;
  fNextFree := Owner.fFreeList; // store this value in the internal free list
  Owner.fFreeList := @self;
  inc(Owner.FreeCount);
  dec(Owner.ActiveCount);
end;

procedure TBigInt.ResetPermanentAndRelease;
begin
  if @self = nil then
    exit;
  ResetPermanent;
  Release;
end;

function TBigInt.Clone: PBigInt;
begin
  result := Owner.Allocate(Size, {nozero=}true);
  MoveFast(Value[0], result^.Value[0], Size * HALF_BYTES);
end;

procedure TBigInt.Save(data: PByteArray; bytes: integer; andrelease: boolean);
var
  i, k: PtrInt;
  c: cardinal;
  j: byte;
begin
  FillCharFast(data^, bytes, 0);
  k := bytes - 1;
  for i := 0 to Size - 1 do
  begin
    c := Value[i];
    if k >= 0 then
      for j := 0 to HALF_BYTES - 1 do
      begin
        data[k] := c shr (j * 8);
        dec(k);
        if k < 0 then
          break;
      end;
  end;
  if andrelease then
    Release;
end;

function TBigInt.Save(andrelease: boolean): RawByteString;
begin
  FastSetRawByteString(result, nil, Size * HALF_BYTES);
  Save(pointer(result), length(result), andrelease);
end;

function TBigInt.Add(b: PBigInt): PBigInt;
var
  n: integer;
  pa, pb: PHalfUInt;
  v: PtrUInt;
begin
  if not b^.IsZero then
  begin
    n := Max(Size, b^.Size);
    Resize(n + 1, {nozero=}true);
    b^.Resize(n);
    pa := pointer(Value);
    pb := pointer(b^.Value);
    v := 0;
    repeat
      inc(v, PtrUInt(pa^) + pb^);
      pa^ := v;
      v := v shr HALF_BITS; // branchless carry propagation
      inc(pa);
      inc(pb);
      dec(n);
    until n = 0;
    pa^ := v;
  end;
  b.Release;
  result := Trim;
end;

function TBigInt.Substract(b: PBigInt; NegativeResult: PBoolean): PBigInt;
var
  n: integer;
  pa, pb: PHalfUInt;
  v: PtrUInt;
begin
  n := Size;
  b^.Resize(n);
  pa := pointer(Value);
  pb := pointer(b^.Value);
  v := 0;
  repeat
    v := PtrUInt(pa^) - pb^ - v;
    pa^ := v;
    v := ord((v shr HALF_BITS) <> 0); // branchless carry
    inc(pa);
    inc(pb);
    dec(n);
  until n = 0;
  if NegativeResult <> nil then
    NegativeResult^ := v <> 0;
  b.Release;
  result := Trim;
end;

function TBigInt.IntMultiply(b: HalfUInt): PBigInt;
var
  r: PHalfUInt;
  v, m: PtrUInt;
  i: PtrInt;
begin
  result := Owner.Allocate(Size + 1, true);
  r := pointer(result^.Value);
  v := 0;
  m := b;
  for i := 0 to Size - 1 do
  begin
    inc(v, PtrUInt(Value[i]) * m);
    r^ := v;
    v := v shr HALF_BITS; // carry
    inc(r);
  end;
  r^ := v;
  Release;
  result^.Trim;
end;

function TBigInt.IntDivide(b: HalfUInt; modulo: PHalfUInt): PBigInt;
var
  r, d: PtrUInt;
  i: PtrInt;
begin
  r := 0;
  for i := Size - 1 downto 0 do
  begin
    r := (r shl HALF_BITS) + Value[i];
    d := r div b;
    Value[i] := d;
    dec(r, d * b); // fast r := r mod b
  end;
  if modulo <> nil then
    modulo^ := r;
  result := Trim;
end;

function TBigInt.IntMod10: PtrUInt;
var
  d: PtrUInt;
  i: PtrInt;
begin
  result := 0;
  for i := Size - 1 downto 0 do
  begin
    result := (result shl HALF_BITS) + Value[i];
    d := result div 10;  // fast multiplication by reciprocal on FPC
    Value[i] := d;
    dec(result, d * 10);
  end;
end;

function TBigInt.ToHexa: RawUtf8;
begin
  result := BinToHexDisplay(pointer(Value), Size * HALF_BYTES);
end;

function TBigInt.ToText(noclone: boolean): RawUtf8;
var
  v: PBigInt;
  tmp: TTextWriterStackBuffer;
  p: PByte;
begin
  case Size of
    0:
      result := '0';
    1:
      UInt32ToUtf8(Value[0], result);
  else
    begin
      if noclone then
        v := Copy // inc RefCnt
      else
        v := Clone;
      p := @tmp[high(tmp)];
      repeat
        dec(p);
        p^ := v.IntMod10 + ord('0'); // maybe faster if mod 10000
      until v.IsZero or
            (p = @tmp); // truncate after 8190 digits
      v.Release;
      FastSetString(result, p, PAnsiChar(@tmp[high(tmp)]) - pointer(p));
    end;
  end;
end;

function TBigInt.Divide(v: PBigInt; ComputeMod: boolean): PBigInt;
var
  d, inner, dash: HalfUInt;
  neg: boolean;
  j, m, n, orgsiz: integer;
  p: PHalfUInt;
  u, quo, tmp: PBigInt;
begin
  if ComputeMod and
     (Compare(v) < 0) then
  begin
    v.Release;
    result := @self; // just return u if u < v
    exit;
  end;
  m := Size - v^.Size;
  n := v^.Size + 1;
  orgsiz := Size;
  quo := Owner.Allocate(m + 1);
  tmp := Owner.Allocate(n);
  v.Trim;
  d := RSA_RADIX div (PtrUInt(v^.Value[v^.Size - 1]) + 1);
  u := Clone;
  if d > 1 then
  begin
    // Normalize
    u := u.IntMultiply(d);
    if ComputeMod and
       not Owner.fNormMod[Owner.CurrentModulo].IsZero then
      v := Owner.fNormMod[Owner.CurrentModulo]
    else
      v := v.IntMultiply(d);
  end;
  if orgsiz = u^.Size then
    u.Resize(orgsiz + 1); // allocate additional digit
  for j := 0 to m do
  begin
    // Get a temporary short version of u
    MoveFast(u^.Value[u^.Size - n - j], tmp^.Value[0], n * HALF_BYTES);
    // Calculate q'
    if tmp^.Value[tmp^.Size - 1] = v^.Value[v^.Size - 1] then
      dash := RSA_RADIX - 1
    else
    begin
      dash := (PtrUInt(tmp^.Value[tmp^.Size - 1]) * RSA_RADIX +
              tmp^.Value[tmp^.Size - 2]) div v^.Value[v^.Size - 1];
      if (v^.Size > 1) and
         (v^.Value[v^.Size - 2] > 0) then
      begin
        inner := (RSA_RADIX * tmp^.Value[tmp^.Size - 1] +
            tmp^.Value[tmp^.Size - 2] -
            PtrUInt(dash) * v^.Value[v^.Size - 1]) and $ffffffff;
        if (PtrUInt(v^.Value[v^.Size - 2]) * dash) >
            (PtrUInt(inner) * RSA_RADIX +
             tmp^.Value[tmp^.Size - 3]) then
          dec(dash);
      end;
    end;
    p := @quo^.Value[quo^.Size - j - 1];
    if dash > 0 then
    begin
      // Multiply and subtract
      tmp := tmp.Substract(v.Copy.IntMultiply(dash), @neg);
      tmp.Resize(n);
      p^ := dash;
      if neg then
      begin
        // Add back
        dec(p^);
        tmp := tmp.Add(v.Copy);
        // Lop off the carry
        dec(tmp^.Size);
        dec(v^.Size);
      end;
    end
    else
      p^ := 0;
    // Copy back to u
    MoveFast(tmp^.Value[0], u^.Value[u^.Size - n - j], n * HALF_BYTES);
  end;
  tmp.Release;
  v.Release;
  if ComputeMod then
  begin
    // return the remainder
    quo.Release;
    result := u.Trim.IntDivide(d);
  end
  else
  begin
    // return the quotient
    u.Release;
    result := quo.Trim;
  end
end;

function TBigInt.Multiply(b: PBigInt; InnerPartial: PtrInt; OuterPartial: PtrInt
  ): PBigInt;
var
  r: PBigInt;
  i, j, k, n: PtrInt;
  v: PtrUInt;
begin
  n := Size;
  r := Owner.Allocate(n + b^.Size);
  for i := 0 to b^.Size - 1 do
  begin
    v := 0; // initial carry value
    k := i;
    j := 0;
    if (OuterPartial <> 0) and
       (OuterPartial > i) and
       (OuterPartial < n) then
    begin
      k := OuterPartial - 1;
      j := k - 1;
    end;
    repeat
      if (InnerPartial > 0) and
         (k >= InnerPartial) then
        break;
      inc(v, PtrUInt(r^.Value[k]) +
             PtrUInt(Value[j]) * b^.Value[i]);
      r^.Value[k] := v;
      inc(k);
      v := v shr HALF_BITS; // carry
      inc(j);
    until j >= n;
    r^.Value[k] := v;
  end;
  Release;
  b.Release;
  result := r.Trim;
end;

function TBigInt.Square: PBigint;
begin
  result := Multiply(Copy);
end;


{ TRsaContext }

constructor TRsaContext.Create;
begin
  fRadix := Allocate(2, {nozero=}true);
  fRadix^.Value[0] := 0;
  fRadix^.Value[1] := 1;
  fRadix^.SetPermanent;
end;

procedure TRsaContext.ForceRelease(p: PPBigInt; n: integer);
begin
  while n > 0 do
  begin
    if p^ <> nil then
    begin
      p^^.RefCnt := 1;
      p^^.Release;
    end;
    inc(p);
    dec(n);
  end;
end;

destructor TRsaContext.Destroy;
var
  b, next : PBigInt;
begin
  fRadix.ResetPermanentAndRelease;
  b := fFreeList;
  while b <> nil do
  begin
    next := b^.fNextFree;
    if b^.Value<>nil then
      FreeMem(b^.Value);
    FreeMem(b);
    b := next;
  end;
  inherited Destroy;
end;

function TRsaContext.AllocateFrom(v: HalfUInt): PBigInt;
begin
  result := Allocate(1, {nozero=}true);
  result^.Value[0] := v;
end;

function TRsaContext.Allocate(n: integer; nozero: boolean): PBigint;
begin
  if self = nil then
    raise ERsaException.CreateUtf8('TBigInt.Allocate(%): Owner=nil', [n]);
  result := fFreeList;
  if result <> nil then
  begin
    // we can recycle a pre-allocated buffer
    if result^.RefCnt <> 0 then
      raise ERsaException.CreateUtf8(
        'TBigInt.Allocate(%): % RefCnt=%', [n, result, result^.RefCnt]);
    fFreeList := result^.fNextFree;
    dec(FreeCount);
    result.Resize(n, {nozero=}true);
  end
  else
  begin
    // we need to allocate a new buffer
    New(result);
    result^.Owner := self;
    result^.Size := n;
    result^.Capacity := NextGrow(n); // with some initial over-allocatation
    GetMem(result^.Value, result^.Capacity * HALF_BYTES);
  end;
  result^.RefCnt := 1;
  result^.fNextFree := nil;
  if not nozero then
    FillCharFast(result^.Value[0], n * HALF_BYTES, 0); // zeroed
  inc(ActiveCount);
end;

function TRsaContext.Load(data: PByteArray; bytes: integer): PBigInt;
begin
  result := Allocate(ValuesSize(bytes));
  ValuesLoad(result.Value, data, bytes);
end;

function TRsaContext.Load(const data: RawByteString): PBigInt;
begin
  result := Load(pointer(data), length(data));
end;

procedure TRsaContext.SetModulo(b: PBigInt; modulo: TRsaModulo);
var
  d: HalfUInt;
  k: integer;
begin
  k := b^.Size;
  fMod[modulo] := b.SetPermanent;
  d := RSA_RADIX div (PtrUInt(b^.Value[k - 1]) + 1);
  fNormMod[modulo] := b.IntMultiply(d).SetPermanent;
  b := fRadix.Clone.LeftShift(k * 2 - 1);
  fMu[modulo] := b.Divide(fMod[modulo]).SetPermanent;
  b.Release;
  fBk1[modulo] := AllocateFrom(1).LeftShift(k + 1).SetPermanent;
end;

procedure TRsaContext.ResetModulo(modulo: TRsaModulo);
begin
  fMod[modulo].ResetPermanentAndRelease;
  fNormMod[modulo].ResetPermanentAndRelease;
  fMu[modulo].ResetPermanentAndRelease;
  fBk1[modulo].ResetPermanentAndRelease;
end;

function TRsaContext.Barret(b: PBigint): PBigInt;
var
  q1, q2, q3, r1, r2, bim: PBigInt;
  k: integer;
begin
  bim := fMod[CurrentModulo];
  k := bim^.Size;
  if b^.Size > k * 2 then
  begin
    // use regular divide/modulo method instead  - Barrett cannot help
    result := b^.Divide(bim, {mod=}true);
    exit;
  end;
  // q1 = [x / b**(k-1)]
  q1 := b^.Clone.RightShift(k - 1);
  // Do outer partial multiply
  // q2 = q1 * mu
  q2 := q1.Multiply(fMu[CurrentModulo], 0, k - 1);
  // q3 = [q2 / b**(k+1)]
  q3 := q2.RightShift(k + 1);
  // r1 = x mod b**(k+1)
  r1 := b^.TruncateMod(k + 1);
  // Do inner partial multiply
  // r2 = q3 * m mod b**(k+1)
  r2 := q3.Multiply(bim, k + 1, 0).TruncateMod(k + 1);
  // if (r1 < r2) r1 = r1 + b**(k+1)
  if r1.Compare(r2) < 0 then
    r1 := r1.Add(fBk1[CurrentModulo]);
  // r = r1-r2
  result := r1.Substract(r2);
  // while (r >= m) do r = r-m
  while result.Compare(bim) >= 0 do
    result.Substract(bim);
end;

function TRsaContext.ModPower(b, exp: PBigInt): PBigInt;
var
  r, g2: PBigInt;
  i, j, k, l, partial, windowsize: integer;
  g: array of PBigInt;
begin
  i := exp.FindMaxExponentIndex;
  r := AllocateFrom(1);
  // compute optimum window size
  windowsize := 1;
  j := i;
  while j > HALF_BITS do
  begin
    inc(windowsize);
    j := j div HALF_SHR;
  end;
  // pre-compute all g[i] items
  k := 1 shl (windowsize - 1);
  SetLength(g, k);
  g[0] := b^.Clone.SetPermanent;
  g2 := Barret(g[0].Square); // g2 := residue of g^2
  for j := 1 to k - 1 do
    g[j] := Barret(g[j - 1].Multiply(g2.Copy)).SetPermanent;
  g2.Release;
  // reduce to left-to-right exponentiation, one exponent bit at a time
  repeat
    if exp.BitIsSet(i) then
    begin
      l := i - windowsize + 1;
      partial := 0;
      if l < 0 then // LSB of exponent will always be 1
        l := 0
      else
        while not exp.BitIsSet(l) do
          inc(l); // go back up
      // build up the section of the exponent
      j := i;
      while j >= l do
      begin
        r := Barret(r.Square);
        if exp.BitIsSet(j) then
          inc(partial);
        if j <> l then
          partial := partial shl 1;
        dec(j);
      end;
      partial := (partial - 1) shr 1; // Adjust for array
      r := Barret(r.Multiply(g[partial]));
      i := l - 1;
    end
    else
    begin
      // bit not set: just process the next bit
      r := Barret(r.Square);
      dec(i);
    end;
  until i < 0;
  // memory cleanup
  for i := 0 to k - 1 do
    g[i].ResetPermanentAndRelease;
  b.Release;
  exp.Release;
  result := r;
end;

function TRsaContext.ChineseRemainderTheorem(
  b, dp, dq, p, q, qinv : PBigInt): PBigInt;
var
  h, m1, m2: PBigInt;
begin
  CurrentModulo := rmP;
  m1 := ModPower(b.Copy, dp);
  CurrentModulo := rmQ;
  m2 := ModPower(b, dq);
  h := m1.Add(p).Substract(m2.Copy).Multiply(qinv);
  CurrentModulo := rmP;
  h := Barret(h);
  result := m2.Add(q.Multiply(h));
end;


{ **************** RSA Low-Level Cryptography Functions }

const
  ANS1_OID_RSAPUB = '1.2.840.113549.1.1.1';

function DerToRsa(const der: TCertDer; seqtype: integer; version: PInteger;
  const values: array of PRawByteString): boolean;
var
  pos: TIntegerDynArray;
  seq, oid, str: TAsnObject;
  vt, i: integer;
begin
  AsnNextInit(pos, 3);
  result := (der <> '') and
            (AsnNext(pos[0], der) = ASN1_SEQ);
  if not result then
    exit;
  if version <> nil then
  begin
    version^ := AsnNextInteger(pos[0], der, vt);
    result := vt = ASN1_INT;
  end;
  result := result and
            (AsnNextRaw(pos[0], der, seq) = ASN1_SEQ) and
              (AsnNextRaw(pos[1], seq, oid) = ASN1_OBJID) and
                (oid = AsnEncOid(ANS1_OID_RSAPUB)) and
            (AsnNextRaw(pos[0], der, str) = seqtype) and
              (AsnNext(pos[2], str) = ASN1_SEQ);
  if result and
     (version <> nil) then
    result := AsnNextInteger(pos[2], str, vt) = version^;
  for i := 0 to high(values) do
    if result then
      result := AsnNextBigInt(pos[2], str, values[i]^);
end;


{ TRsaPublicKey }

function TRsaPublicKey.ToDer: TCertDer;
begin
  if (Modulus = '') or
     (Exponent = '') then
    result := ''
  else
    // see "A.1.1. RSA Public Key Syntax" of RFC 8017
    result := Asn(ASN1_SEQ, [
                Asn(ASN1_SEQ, [
                  AsnOid(ANS1_OID_RSAPUB),
                  ASN1_NULL_VALUE // optional
                ]),
                Asn(ASN1_BITSTR, [
                  Asn(ASN1_SEQ, [
                    AsnBigInt(Modulus),
                    AsnBigInt(Exponent) // typically 65537
                  ])
                ])
              ]);
end;

function TRsaPublicKey.FromDer(const der: TCertDer): boolean;
begin
  if (Modulus <> '') or
     (Exponent <> '') then
    raise ERsaException.Create('TRsaPublicKey.FromDer over an existing key');
  result := DerToRsa(der, ASN1_BITSTR, nil, [
              @Modulus,
              @Exponent]);
end;


{ TRsaPrivateKey }

function TRsaPrivateKey.ToDer: TCertDer;
begin
  if (Modulus = '') or
     (PublicExponent = '') then
    result := ''
  else
    // PKCS#8 format (default as with openssl)
    result := Asn(ASN1_SEQ, [
                Asn(Version),
                Asn(ASN1_SEQ, [
                  AsnOid(ANS1_OID_RSAPUB),
                  ASN1_NULL_VALUE // optional
                ]),
                Asn(ASN1_OCTSTR, [
                  Asn(ASN1_SEQ, [
                    Asn(Version),
                    AsnBigInt(Modulus),
                    AsnBigInt(PublicExponent), // typically 65537
                    AsnBigInt(PrivateExponent),
                    AsnBigInt(Prime1),
                    AsnBigInt(Prime2),
                    AsnBigInt(Exponent1),
                    AsnBigInt(Exponent2),
                    AsnBigInt(Coefficient)
                  ])
                ])
              ]);
end;

function TRsaPrivateKey.FromDer(const der: TCertDer): boolean;
var
  n, vt: integer;
begin
  if (Modulus <> '') or
     (PublicExponent <> '') then
    raise ERsaException.Create('TRsaPrivateKey.FromDer over an existing key');
  // first try the openssl PKCS#8 layout
  result := DerToRsa(der, ASN1_OCTSTR, @Version, [
              @Modulus,
              @PublicExponent,
              @PrivateExponent,
              @Prime1,
              @Prime2,
              @Exponent1,
              @Exponent2,
              @Coefficient
            ]);
  if result then
    exit;
  // also try PKCS#1 from RFC 8017
  n := 1;
  if (der = '') or
     (AsnNext(n, der) <> ASN1_SEQ) then
    exit;
  Version := AsnNextInteger(n, der, vt);
  result := (vt = ASN1_INT) and
            AsnNextBigInt(n, der, Modulus) and
            AsnNextBigInt(n, der, PublicExponent) and
            AsnNextBigInt(n, der, PrivateExponent) and
            AsnNextBigInt(n, der, Prime1) and
            AsnNextBigInt(n, der, Prime2) and
            AsnNextBigInt(n, der, Exponent1) and
            AsnNextBigInt(n, der, Exponent2) and
            AsnNextBigInt(n, der, Coefficient);
end;

function TRsaPrivateKey.Match(const Pub: TRsaPublicKey): boolean;
begin
  result := (Modulus <> '') and
            (PublicExponent <> '') and
            (Modulus = Pub.Modulus) and
            (PublicExponent = Pub.Exponent);
end;


{ TRsa }

destructor TRsa.Destroy;
begin
  ResetModulo(rmM);
  ForceRelease(@fE, 7); // fM is already finalized
  inherited Destroy;
end;

procedure TRsa.LoadFromPublicKeyBinary(Modulus, Exponent: pointer;
  ModulusSize, ExponentSize: PtrInt);
begin
  if not fM.IsZero then
    raise ERsaException.CreateUtf8(
      '%.LoadFromPublicKey on existing data', [self]);
  if (ModulusSize < 10) or
     (ExponentSize < 3) then
    raise ERsaException.CreateUtf8(
      '%.LoadFromPublicKey: unexpected ModulusSize=% ExponentSize=%',
      [self, ModulusSize, ExponentSize]);
  fModulusLen := ModulusSize;
  fM := Load(Modulus, ModulusSize);
  fModulusBits := fM.BitCount;
  SetModulo(fM, rmM);
  fE := Load(Exponent, ExponentSize).SetPermanent;
end;

function TRsa.LoadFromPublicKeyDer(const Der: TCertDer): boolean;
var
  key: TRsaPublicKey;
begin
  result := key.FromDer(Der);
  if result then
    LoadFromPublicKey(key);
end;

function TRsa.LoadFromPublicKeyPem(const Pem: TCertPem): boolean;
begin
  result := LoadFromPublicKeyDer(PemToDer(Pem));
end;

procedure TRsa.LoadFromPublicKeyHexa(const Hexa: RawUtf8);
var
  bin: RawByteString;
  b: PAnsiChar absolute bin;
begin
  if not HexToBin(pointer(Hexa), length(Hexa), bin) or
     (length(bin) < 13) then
    raise ERsaException.CreateUtf8('Invalid %.LoadFromPublicKeyHexa', [self]);
  LoadFromPublicKeyBinary(b, b + 3, 3, length(bin) - 3);
end;

procedure TRsa.LoadFromPublicKey(const PublicKey: TRsaPublicKey);
begin
  LoadFromPublicKeyBinary(pointer(PublicKey.Modulus), pointer(PublicKey.Exponent),
    length(PublicKey.Modulus), length(PublicKey.Exponent));
end;

procedure TRsa.LoadFromPrivateKey(const PrivateKey: TRsaPrivateKey);
begin
  if not fM.IsZero then
    raise ERsaException.CreateUtf8('%.LoadFromPrivateKey on existing data', [self]);
  with PrivateKey do
    if (PrivateExponent = '') or
       (Prime1 = '') or
       (Prime2 = '') or
       (Exponent1 = '') or
       (Exponent2 = '') or
       (Coefficient = '') or
       (length(Modulus) < 3) or
       (length(PublicExponent) < 10) then
    raise ERsaException.CreateUtf8('Incorrect %.LoadFromPrivateKey call', [self]);
  LoadFromPublicKeyBinary(
    pointer(PrivateKey.Modulus), pointer(PrivateKey.PublicExponent),
    length(PrivateKey.Modulus),  length(PrivateKey.PublicExponent));
  fD := Load(PrivateKey.PrivateExponent).SetPermanent;
  fP := Load(PrivateKey.Prime1).SetPermanent;
  fQ := Load(PrivateKey.Prime2).SetPermanent;
  fDP := Load(PrivateKey.Exponent1).SetPermanent;
  fDQ := Load(PrivateKey.Exponent2).SetPermanent;
  fQInv := Load(PrivateKey.Coefficient).SetPermanent;
  SetModulo(fP, rmP);
  SetModulo(fQ, rmQ);
end;



end.
