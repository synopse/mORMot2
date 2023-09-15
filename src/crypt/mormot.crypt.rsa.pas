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
  mormot.core.buffers;
  

{ **************** RSA Oriented Big-Integer Computation }

type
  /// exception class raised by this unit
  ERsaException = class(ESynException);

  /// pointer to a single precision 32-bit component value
  PBIComponent = ^TBIComponent;
  /// a single precision 32-bit component value
  TBIComponent = cardinal;
  /// pointer to a single precision 32-bit component array
  PBIComponents = PCardinalArray;

  /// pointer to an unsigned double precision 64-bit component value
  PBILongComponent = ^TBILongComponent;
  /// an unsigned double precision 64-bit component value
  TBILongComponent = QWord;

  /// pointer to a signed double precision 64-bit component value
  PBISignedLongComponent = ^TBISignedLongComponent;
  /// a signed double precision 64-bit component value
  TBISignedLongComponent = Int64;

const
  /// maximum TBIComponent value + 1
  BIGINT_COMP_RADIX = TBILongComponent($0000000100000000);
  /// maximum TBILongComponent value - 1
  BIGINT_COMP_MAX = TBILongComponent($ffffffffffffffff);
  /// number of bytes in a TBIComponent, i.e. 4
  BIGINT_COMP_BYTE_SIZE = SizeOf(TBIComponent);
  /// number of power of two bits in a TBIComponent, since 1 shl 5 = 32
  BIGINT_COMP_BIT_SHR = 5;
  /// number of bits in a TBIComponent, i.e. 32
  BIGINT_COMP_BIT_SIZE = BIGINT_COMP_BYTE_SIZE * 8;

type
  PPBigInt = ^PBigInt;
  PBigInt = ^TBigInt;

  TBigIntContext = class;

  /// store one Big Integer value with proper COW support
  // - each value is owned by an associated TBigIntContext instance
  // - you should call TBigIntContext.Release() once done with this value
  TBigInt = record
  private
    /// next bigint in the Owner free instance cache
    NextFree: PBigInt;
    procedure ResizeComponents(n: integer; nozero: boolean = false);
    function FindMaxExponentIndex: integer;
    procedure SetPermanent;
    procedure ResetPermanent;
    function TruncateMod(modulus: integer): PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// the associated Big Integer context
    Owner: TBigIntContext;
    /// number of components in this Big Integer value
    Size: integer;
    /// number of components allocated for this bigint
    Capacity: integer;
    /// internal reference counter
    // - equals -1 for permanent/constant storage
    RefCnt: integer;
    /// raw access to the actual component data
    Components: PBIComponents;
    /// comparison with another Big Integer value
    function Compare(b: PBigInt): integer;
    /// make a COW instance, increasing RefCnt
    function Copy: PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// allocate a new Big Integer value with the same data as an existing one
    function Clone: PBigInt;
    /// decreases the value RefCnt, saving it in the internal FreeList once done
    procedure Release;
    /// a wrapper to b.ResetPermanent then Release(b)
    procedure ResetPermanentAndRelease;
    /// export a Big Integer value into a binary buffer
    procedure Save(data: PByteArray; bytes: integer; andrelease: boolean);
    /// delete any meaningless leading zeros and return self
    function Trim: PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// quickly search if contains 0
    function IsZero: boolean;
    /// check if a given bit is set to 1
    function BitIsSet(bit: PtrUInt): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// search the position of the first bit set
    function BitCount: integer;
    /// shift right the internal data components by a number of slots
    function RightShift(n: integer): PBigInt;
    /// shift left the internal data components by a number of slots
    function LeftShift(n: integer): PBigInt;
    /// compute the sum of two Big Integer values
    // - returns self := self + b as result
    // - will eventually release the b instance
    function Add(b: PBigInt): PBigInt;
    /// compute the difference of two Big Integer values
    // - returns self := abs(self - b) as result, and NegativeResult as its sign
    // - will eventually release the b instance
    function Substract(b: PBigInt; NegativeResult: PBoolean = nil): PBigInt;
    /// division or modulo computation
    // - self is the numerator
    // - if ComputeMod is false, v is the denominator; otherwise, is the modulus
    // - will eventually release the v instance
    function Divide(v: PBigInt; ComputeMod: boolean = false): PBigInt;
    /// standard multiplication between two Big Integer values
    // - will eventually release both self and b instances
    function Multiply(b: PBigInt; InnerPartial: integer = 0;
      OuterPartial: integer = 0): PBigInt;
    /// multiply by an unsigned integer value
    // - returns self := self * b
    // - will eventually release the self instance
    function IntMultiply(b: TBIComponent): PBigInt;
    /// divide by an unsigned integer value
    // - returns self := self div b
    function IntDivide(b: TBIComponent): PBigInt;
    /// return the Big integer value as hexadecimal
    function ToText: RawUtf8;
  end;

  /// define Normal, P and Q pre-computed modulos
  TBigIntModulo = (
    bivN,
    bivP,
    bivQ);
  /// store Normal, P and Q pre-computed modulos as PBigInt
  TBigIntModulos = array[TBigIntModulo] of PBigInt;

  /// store one Big Integer context for RSA
  // - will maintain its own set of reference-counted Big Integer values
  TBigIntContext = class
  private
    /// list of released PBigInt instance, ready to be re-used by Allocate()
    FreeList: PBigInt;
  public
    /// The radix used
    BIRadix: PBigInt;
    /// contains Modulus
    BIMod: TBigIntModulos;
    /// contains mu
    BImu: TBigIntModulos;
    /// contains b(k+1)
    BIbk1: TBigIntModulos;
    /// contains the normalized storage
    BINormalisedMod: TBigIntModulos;
    /// used by sliding-window
    G: PPBigInt;
    /// the size of the sliding window
    Window: Integer;
    /// number of active PBigInt
    ActiveCount: Integer;
    /// number of PBigInt instances stored in the internal FreeList cache
    FreeCount: Integer;
    /// the RSA modulo we are using
    ModOffset: TBigIntModulo;
    /// initialize this Big Integer context
    constructor Create(Size: integer); reintroduce;
    /// finalize this Big Integer context memory
    destructor Destroy; override;
    /// allocate a new zeroed Big Integer value of the specified precision
    // - n is the number of TBitInt.Components[] items to initialize
    function Allocate(n: integer; nozero: boolean = false): PBigint;
    /// allocate a new Big Integer vallue from a 32-bit unsigned integer
    function AllocateFrom(v: TBIComponent): PBigInt;
    /// allocate and import a Big Integer value from a binary buffer
    function Load(data: PByteArray; bytes: integer): PBigInt; overload;
    /// pre-compute some of the internal constant slots for a given modulo
    procedure SetMod(b: PBigInt; modulo: TBigIntModulo);
    /// release the internal constant slots for a given modulo
    procedure ResetMod(modulo: TBigIntModulo);
    /// compute the Barret reduction of a Big Integer value
    function Barret(b: PBigint): PBigInt;
  end;

const
  BIGINT_ZERO_VALUE: TBIComponent = 0;
  BIGINT_ONE_VALUE:  TBIComponent = 1;

  /// constant 0 as Big Integer value
  BIGINT_ZERO: TBigInt = (
    Size: {%H-}1;
    RefCnt: {%H-}-1;
    Components: {%H-}@BIGINT_ZERO_VALUE);

  /// constant 1 as Big Integer value
  BIGINT_ONE: TBigInt = (
    Size: {%H-}1;
    RefCnt: {%H-}-1;
    Components: {%H-}@BIGINT_ONE_VALUE);


{ **************** RSA Low-Level Cryptography Functions }


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

procedure TBigInt.ResizeComponents(n: integer; nozero: boolean);
begin
  if n > Capacity then
  begin
    Capacity := Max(Capacity * 2, n); // for faster size-up
    ReAllocMem(Components, Capacity * BIGINT_COMP_BYTE_SIZE);
  end;
  if not nozero and
     (n > Size) then
    FillCharFast(Components[Size], (n - Size) * BIGINT_COMP_BYTE_SIZE, 0);
  Size := n;
end;

function TBigInt.Trim: PBigInt;
var
  n: PtrInt;
begin
  n := Size;
  while (n > 1) and
        (Components[n - 1] = 0) do // delete any leading 0
    dec(n);
  Size := n;
  result := @self;
end;

function TBigInt.IsZero: boolean;
var
  i: PtrInt;
  p: PBIComponents;
begin
  if @self <> nil then
  begin
    p := Components;
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
  result := Components[bit shr BIGINT_COMP_BIT_SHR] and
             (1 shl (bit and pred(BIGINT_COMP_BIT_SIZE))) <> 0;
end;

function TBigInt.BitCount: integer;
var
  i: PtrInt;
  c: TBIComponent;
begin
  result := 0;
  i := Size - 1;
  while Components[i] = 0 do
  begin
    dec(i);
    if i < 0 then
      exit;
  end;
  result := i * BIGINT_COMP_BIT_SIZE;
  c := Components[i];
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
    result := CompareCardinal(Components[i], b^.Components[i]);
    if result <> 0 then
      exit;
  end;
end;

procedure TBigInt.SetPermanent;
begin
  if RefCnt <> 1 then
    raise ERsaException.CreateUtf8(
      'TBigInt.SetPermanent(%): RefCnt=%', [@self, RefCnt]);
  RefCnt := -1;
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
      Components[0] := 0;
    end
    else
      MoveFast(Components[n], Components[0], Size * BIGINT_COMP_BYTE_SIZE);
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
    ResizeComponents(s + n, {nozero=}true);
    MoveFast(Components[0], Components[n], s * BIGINT_COMP_BYTE_SIZE);
    FillCharFast(Components[0], n * BIGINT_COMP_BYTE_SIZE, 0);
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
  mask, v: TBIComponent;
begin
  result := BIGINT_COMP_BIT_SIZE - 1;
  mask := BIGINT_COMP_RADIX shr 1;
  v := Components[Size - 1];
  repeat
    if (v and mask) <> 0 then
      break;
    mask := mask shr 1;
    dec(result);
    if result < 0 then
      exit;
  until false;
  inc(result, (Size - 1) * BIGINT_COMP_BIT_SIZE);
end;


procedure TBigInt.Release;
begin
  if (@self = nil) or
     (RefCnt < 0) then
    exit;
  dec(RefCnt);
  if RefCnt > 0 then
    exit;
  NextFree := Owner.FreeList; // store this value in the internal free list
  Owner.FreeList := @self;
  inc(Owner.FreeCount);
  dec(Owner.ActiveCount);
end;

procedure TBigInt.ResetPermanentAndRelease;
begin
  ResetPermanent;
  Release;
end;

function TBigInt.Clone: PBigInt;
begin
  result := Owner.Allocate(Size, {nozero=}true);
  MoveFast(Components[0], result^.Components[0], Size * BIGINT_COMP_BYTE_SIZE);
end;

procedure TBigInt.Save(data: PByteArray; bytes: integer; andrelease: boolean);
var
  i, k: PtrInt;
  j: byte;
begin
  FillCharFast(Data^, bytes, 0);
  k := bytes - 1;
  for i := 0 to Size - 1 do
    if k >= 0 then
      for j := 0 to BIGINT_COMP_BYTE_SIZE - 1 do
      begin
        Data[k] := Components[i] shr (j * 8);
        dec(k);
        if k < 0 then
          break;
      end;
  if andrelease then
    Release;
end;


function TBigInt.Add(b: PBigInt): PBigInt;
var
  n: integer;
  pa, pb: PBIComponent;
  sum, tot: TBIComponent;
  carry: boolean;
begin
  if not b^.IsZero then
  begin
    n := Max(Size, b^.Size);
    ResizeComponents(n + 1);
    b^.ResizeComponents(n);
    pa := pointer(Components);
    pb := pointer(b^.Components);
    carry := false;
    repeat
      sum := pa^;
      inc(sum, pb^);
      tot := sum;
      inc(tot, ord(carry));
      carry := (sum < pa^) or (tot < sum);
      pa^ := tot;
      inc(pa);
      inc(pb);
      dec(n);
    until n = 0;
    pa^ := ord(carry);
  end;
  b.Release;
  result := Trim;
end;

function TBigInt.Substract(b: PBigInt; NegativeResult: PBoolean): PBigInt;
var
  n: integer;
  pa, pb: PBIComponent;
  carry, sub, tot: TBIComponent;
begin
  carry := 0;
  n := Size;
  b^.ResizeComponents(n);
  pa := pointer(Components);
  pb := pointer(b^.Components);
  repeat
    sub := pa^;
    dec(sub, pb^);
    inc(pb);
    tot := sub;
    dec(tot, carry);
    if (sub > pa^) or
       (tot > sub) then
      carry := 1
    else
      carry := 0;
    pa^ := tot;
    inc(pa);
    dec(n);
  until n = 0;
  if NegativeResult <> nil then
    NegativeResult^ := carry <> 0;
  b.Release;
  result := Trim;
end;

function TBigInt.IntMultiply(b: TBIComponent): PBigInt;
var
  r: PBIComponent;
  v: TBILongComponent;
  i: PtrInt;
begin
  result := Owner.Allocate(Size + 1);
  r := pointer(result^.Components);
  v := 0;
  for i := 0 to Size - 1 do
  begin
    inc(v, TBILongComponent(r^) + TBILongComponent(Components[i]) * B);
    r^ := v;
    v := v shr BIGINT_COMP_BIT_SIZE; // carry
    inc(r);
  end;
  r^ := v;
  Release;
  result^.Trim;
end;

function TBigInt.IntDivide(b: TBIComponent): PBigInt;
var
  r, d: TBILongComponent;
  i: PtrInt;
begin
  r := 0;
  for i := Size - 1 downto 0 do
  begin
    r := (r shl BIGINT_COMP_BIT_SIZE) + Components[i];
    d := r div b;
    Components[i] := d;
    dec(r, d * b); // fast r := r mod b
  end;
  result := Trim;
end;

function TBigInt.ToText: RawUtf8;
begin
  result := BinToHexDisplay(pointer(Components), Size * BIGINT_COMP_BYTE_SIZE);
end;

function TBigInt.Divide(v: PBigInt; ComputeMod: boolean): PBigInt;
var
  d, inner, dash: TBIComponent;
  neg: boolean;
  j, m, n, orgsiz: integer;
  p: PBIComponent;
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
  d := BIGINT_COMP_RADIX div (TBILongComponent(v^.Components[v^.Size - 1]) + 1);
  u := Clone;
  if d > 1 then
  begin
    // Normalize
    u := u.IntMultiply(d);
    if ComputeMod and
       not Owner.BINormalisedMod[Owner.ModOffset].IsZero then
      v := Owner.BINormalisedMod[Owner.ModOffset]
    else
      v := v.IntMultiply(d);
  end;
  if orgsiz = u^.Size then
    u.ResizeComponents(orgsiz + 1); // allocate additional digit
  for j := 0 to m do
  begin
    // Get a temporary short version of u
    MoveFast(u^.Components[u^.Size - n - j], tmp^.Components[0],
               n * BIGINT_COMP_BYTE_SIZE);
    // Calculate q'
    if tmp^.Components[tmp^.Size - 1] = v^.Components[v^.Size - 1] then
      dash := BIGINT_COMP_RADIX - 1
    else
    begin
      dash := (TBILongComponent(tmp^.Components[tmp^.Size - 1]) * BIGINT_COMP_RADIX +
              tmp^.Components[tmp^.Size - 2]) div v^.Components[v^.Size - 1];
      if (v^.Size > 1) and
         (v^.Components[v^.Size - 2] > 0) then
      begin
        inner := (BIGINT_COMP_RADIX * tmp^.Components[tmp^.Size - 1] +
            tmp^.Components[tmp^.Size - 2] -
            TBILongComponent(dash) * v^.Components[v^.Size - 1]) and $ffffffff;
        if (TBILongComponent(v^.Components[v^.Size - 2]) * dash) >
            (TBILongComponent(inner) * BIGINT_COMP_RADIX +
             tmp^.Components[tmp^.Size - 3]) then
          dec(dash);
      end;
    end;
    p := @quo^.Components[quo^.Size - j - 1];
    if dash > 0 then
    begin
      // Multiply and subtract
      tmp := tmp.Substract(v.Copy.IntMultiply(dash), @neg);
      tmp.ResizeComponents(n);
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
    MoveFast(tmp^.Components[0], u^.Components[u^.Size - n - j],
      n * BIGINT_COMP_BYTE_SIZE);
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

function TBigInt.Multiply(b: PBigInt; InnerPartial, OuterPartial: integer): PBigInt;
var
  r: PBigInt;
  i, j, n: integer;
  k: PtrInt;
  v: TBILongComponent;
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
      inc(v, TBILongComponent(r^.Components[k]) +
             TBILongComponent(Components[j]) * b^.Components[i]);
      r^.Components[k] := v;
      inc(k);
      v := v shr BIGINT_COMP_BIT_SIZE; // carry
      inc(j);
    until j >= n;
    r^.Components[k] := v;
  end;
  Release;
  b.Release;
  result := r.Trim;
end;


{ TBigIntContext }

constructor TBigIntContext.Create(Size: integer);
begin
  BIRadix := Allocate(2, {nozero=}true);
  BIRadix^.Components[0] := 0;
  BIRadix^.Components[1] := 1;
  BIRadix^.SetPermanent;
end;

destructor TBigIntContext.Destroy;
var
  b, next : PBigInt;
begin
  BIRadix.ResetPermanentAndRelease;
  b := FreeList;
  while b <> nil do
  begin
    next := b^.NextFree;
    if b^.Components<>nil then
      FreeMem(b^.Components);
    FreeMem(b);
    b := next;
  end;
  inherited Destroy;
end;

function TBigIntContext.AllocateFrom(v: TBIComponent): PBigInt;
begin
  result := Allocate(1, {nozero=}true);
  result^.Components[0] := v;
end;

function TBigIntContext.Allocate(n: integer; nozero: boolean): PBigint;
begin
  if self = nil then
    raise ERsaException.CreateUtf8('TBigInt.Allocate(%): Owner=nil', [n]);
  result := FreeList;
  if result <> nil then
  begin
    // we can recycle a pre-allocated buffer
    if result^.RefCnt <> 0 then
      raise ERsaException.CreateUtf8(
        'TBigInt.Allocate(%): % RefCnt=%', [n, result, result^.RefCnt]);
    FreeList := result^.NextFree;
    dec(FreeCount);
    result.ResizeComponents(n, nozero);
  end
  else
  begin
    New(result);
    result^.Owner := self;
    result^.Size := n;
    result^.Capacity := n * 2; // with some initial over-allocatation
    result^.Components := GetMem(result^.Capacity * BIGINT_COMP_BYTE_SIZE);
  end;
  result^.RefCnt := 1;
  result^.NextFree := nil;
  if not nozero then
    FillCharFast(result^.Components[0], n * BIGINT_COMP_BYTE_SIZE, 0); // zeroed
  inc(ActiveCount);
end;

function TBigIntContext.Load(data: PByteArray; bytes: integer): PBigInt;
var
  i, o: PtrInt;
  j: byte;
begin
  result := Allocate((bytes + BIGINT_COMP_BYTE_SIZE - 1) div BIGINT_COMP_BYTE_SIZE);
  j := 0;
  o := 0;
  for i := bytes - 1 downto 0 do
  begin
    inc(result^.Components[o], TBIComponent(data[i]) shl j);
    inc(j, 8);
    if j = BIGINT_COMP_BIT_SIZE then
    begin
      j := 0;
      inc(o);
    end;
  end;
end;

procedure TBigIntContext.SetMod(b: PBigInt; modulo: TBigIntModulo);
var
  d: TBIComponent;
  k: integer;
begin
  k := b^.Size;
  BIMod[modulo] := b;
  BIMod[modulo].SetPermanent;
  d := BIGINT_COMP_RADIX div (TBILongComponent(b^.Components[k - 1]) + 1);
  BINormalisedMod[modulo] := b.IntMultiply(d);
  BINormalisedMod[modulo].SetPermanent;
  BImu[modulo] := BIRadix.Clone.LeftShift(k * 2 - 1).Divide(BIMod[modulo]);
  BImu[modulo].SetPermanent;
  BIbk1[modulo] := AllocateFrom(1).LeftShift(k + 1);
  BIbk1[modulo].SetPermanent;
end;

procedure TBigIntContext.ResetMod(modulo: TBigIntModulo);
begin
  BIMod[modulo].ResetPermanentAndRelease;
  BINormalisedMod[modulo].ResetPermanentAndRelease;
  BImu[modulo].ResetPermanentAndRelease;
  BIbk1[modulo].ResetPermanentAndRelease;
end;

function TBigIntContext.Barret(b: PBigInt): PBigInt;
var
  q1, q2, q3, r1, r2, bim: PBigInt;
  k: integer;
begin
  bim := BIMod[ModOffset];
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
  q2 := q1.Multiply(BImu[ModOffset], 0, k - 1);
  // q3 = [q2 / b**(k+1)]
  q3 := q2.RightShift(k + 1);
  // r1 = x mod b**(k+1)
  r1 := b^.TruncateMod(k + 1);
  // Do inner partial multiply
  // r2 = q3 * m mod b**(k+1)
  r2 := q3.Multiply(bim, k + 1, 0).TruncateMod(k + 1);
  // if (r1 < r2) r1 = r1 + b**(k+1)
  if r1.Compare(r2) < 0 then
    r1 := r1.Add(BIbk1[ModOffset]);
  // r = r1-r2
  result := r1.Substract(r2);
  // while (r >= m) do r = r-m
  while result.Compare(bim) >= 0 do
    result.Substract(bim);
end;


{ **************** RSA Low-Level Cryptography Functions }

end.
