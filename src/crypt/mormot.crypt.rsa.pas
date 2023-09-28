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
    design and core methods have been rewritten from scratch in modern OOP and
    fixing memory leaks and performance bottlenecks
  - use half-registers (HalfUInt) for efficient computation on all CPUs
  - use dedicated x86_64 asm for core computation routines (2x speedup)
  - slower than OpenSSL, but likely the fastest FPC or Delphi native RSA library
  - TODO: includes proper RSA keypair generation
}

{.$define USEBARRET}
// could be defined to enable Barret reduction (not working yet)


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

  /// refine the extend of TBigInt.MatchKnownPrime() detection
  // - bspFast will search for known primes < 256 - e.g. RS256 at 250K/s rate
  // - bspMost will search for known primes < 2000 - e.g. RS256 at 45K/s rate
  // - bspAll will search for known primes < 18000 - e.g. RS256 at 6.5K/s rate
  TBigIntSimplePrime = (
    bspFast,
    bspMost,
    bspAll);

  /// define how TBigInt.Divide computes its result
  TBigIntDivide = (
    bidDivide,
    bidMod,
    bidModNorm);

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
    function UsedBytes: integer;
    procedure Resize(n: integer; nozero: boolean = false);
    {$ifdef USEBARRET}
    function TruncateMod(modulus: integer): PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
    {$endif USEBARRET}
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
    function Compare(b: PBigInt; andrelease: boolean = false): integer; overload;
    /// comparison with another Unsigned Integer value
    // - values should have been Trim-med for the size to match
    function Compare(u: HalfUInt; andrelease: boolean = false): integer; overload;
    /// make a COW instance, increasing RefCnt and returning self
    function Copy: PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// allocate a new Big Integer value with the same data as an existing one
    function Clone: PBigInt;
    /// mark the value with a RefCnt < 0
    function SetPermanent: PBigInt;
    /// mark the value with a RefCnt = 1
    function ResetPermanent: PBigInt;
    /// fill the internal memory buffer with zeros, for anti-forensic measure
    function Done: PBigInt;
    /// decreases the value RefCnt, saving it in the internal FreeList once done
    procedure Release;
    /// a wrapper to ResetPermanent then Release
    // - before release, fill the buffer with zeros to avoid forensic leaking
    procedure ResetPermanentAndRelease;
    /// export a Big Integer value into a binary buffer
    procedure Save(data: PByteArray; bytes: integer; andrelease: boolean); overload;
    /// export a Big Integer value into a binary RawByteString
    function Save(andrelease: boolean = false): RawByteString; overload;
    /// delete any meaningless leading zeros and return self
    function Trim: PBigInt;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// quickly check if contains 0
    function IsZero: boolean;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// quickly check if contains an even number, i.e. last bit is 0
    function IsEven: boolean;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// quickly check if contains an odd number, i.e. last bit is 1
    function IsOdd: boolean;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// check if a given bit is set to 1
    function BitIsSet(bit: PtrUInt): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// search the position of the first bit set
    function BitCount: integer;
    /// return the number of bits set in this value
    function BitSetCount: integer;
    /// return the index of the highest bit set
    function FindMaxBit: integer;
    /// return the index of the lowest bit set
    function FindMinBit: integer;
    /// shift right the internal data by some bits = div per 2/4/8...
    function ShrBits(bits: integer = 1): PBigInt;
    /// shift left the internal data by some bits = mul per 2/4/8...
    function ShlBits(bits: integer = 1): PBigInt;
    /// shift right the internal data HalfUInt by a number of slots
    function RightShift(n: integer): PBigInt;
    /// shift left the internal data HalfUInt by a number of slots
    function LeftShift(n: integer): PBigInt;
    /// compute the GCD of two numbers using Euclidean algorithm
    function GreatestCommonDivisor(b: PBigInt): PBigInt;
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
    // - if Compute is bidDivide, v is the denominator; otherwise, is the modulus
    // - will eventually release the v instance
    function Divide(v: PBigInt; Compute: TBigIntDivide = bidDivide;
      Remainder: PPBigInt = nil): PBigInt;
    /// modulo computation
    // - just redirect to Divide(v.Copy, bidMod)
    // - won't eventually release the v instance thanks to v.Copy
    function Modulo(v: PBigInt): PBigInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// standard multiplication between two Big Integer values
    // - will eventually release both self and b instances
    function Multiply(b: PBigInt): PBigInt;
    /// partial multiplication between two Big Integer values
    // - will eventually release both self and b instances
    function MultiplyPartial(b: PBigInt; InnerPartial: PtrInt;
      OuterPartial: PtrInt): PBigInt;
    /// standard multiplication by itself
    // - will allocate a new Big Integer value and release self
    function Square: PBigint;
      {$ifdef HASINLINE} inline; {$endif}
    /// add an unsigned integer value
    function IntAdd(b: HalfUInt): PBigInt;
    /// substract an unsigned integer value
    function IntSub(b: HalfUInt): PBigInt;
    /// multiply by an unsigned integer value
    // - returns self := self * b
    // - will eventually release the self instance
    function IntMultiply(b: HalfUInt): PBigInt;
    /// divide by an unsigned integer value
    // - returns self := self div b
    // - optionally return self mod b
    function IntDivide(b: HalfUInt; optmod: PHalfUInt = nil): PBigInt;
    /// compute the modulo by an unsigned integer value
    // - returns self mod b, keeping self untouched
    function IntMod(b: HalfUInt): PtrUInt;
    /// division and modulo by 10 computation
    // - computes self := self div 10 and return self mod 10
    function IntDivMod10: PtrUInt;
    /// check if this value is divisable by a small prime
    // - detection coverage can be customized from default primes < 2000
    function MatchKnownPrime(Extend: TBigIntSimplePrime = bspMost): boolean;
    /// check if the number is (likely to be) a prime
    // - can set a known simple primes Extend and Miller-Rabin tests Iterations
    function IsPrime(Extend: TBigIntSimplePrime = bspMost;
      Iterations: integer = 20): boolean;
    /// guess a random prime number of the exact current size
    // - loop over TAesPrng.Fill and IsPrime method within a timeout period
    function FillPrime(Extend: TBigIntSimplePrime = bspMost;
      Iterations: integer = 20; EndTix: Int64 = 0): boolean;
    /// return crc32c of the Big Integer value binary
    function ToHash: cardinal;
    /// return the Big Integer value as hexadecimal
    function ToHexa: RawUtf8;
    /// return the Big Integer value as text with base-10 digits
    // - self will remain untouched unless noclone is set
    function ToText(noclone: boolean = false): RawUtf8;
    /// could be used for low-level console debugging of a raw value
    procedure Debug(const name: shortstring; full: boolean = false);
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
    {$ifdef USEBARRET}
    /// contains mu
    fMu: TRsaModulos;
    /// contains b(k+1)
    fBk1: TRsaModulos;
    {$endif USEBARRET}
    /// contains the normalized storage
    fNormMod: TRsaModulos;
  public
    /// the size of the sliding window
    Window: integer;
    /// number of active PBigInt
    ActiveCount: integer;
    /// number of PBigInt instances stored in the internal instances cache
    FreeCount: integer;
    /// as set by SetModulo() and  used by Reduce() and ModPower()
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
    /// allocate a new Big Integer value from a ToHexa dump
    function AllocateFromHex(const hex: RawUtf8): PBigInt;
    /// call b^^.Release and set b^ := nil
    procedure Release(const b: array of PPBigInt);
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function Load(data: PByteArray; bytes: integer): PBigInt; overload;
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function Load(const data: RawByteString): PBigInt; overload;
    /// pre-compute some of the internal constant slots for a given modulo
    procedure SetModulo(b: PBigInt; modulo: TRsaModulo);
    /// release the internal constant slots for a given modulo
    procedure ResetModulo(modulo: TRsaModulo);
    /// compute the reduction of a Big Integer value in a given modulo
    // - if m is nil, SetModulo() should have previously be called
    // - redirect to Divide() or use the Barret algorithm
    // - will eventually release the b instance
    function Reduce(b, m: PBigint): PBigInt;
    /// compute a modular exponentiation, i.e. b^exp mod m
    // - if m is nil, SetModulo() should have previously be called
    // - will eventually release the b and exp instances
    function ModPower(b, exp, m: PBigInt): PBigInt;
  end;

const
  /// 2KB table of iterative difference of all known prime numbers < 18,000
  // - as used by TBigInt.MatchKnownPrime
  // - published in interface section for TTestCoreCrypto._RSA validation
  BIGINT_PRIMES_DELTA: array[0 .. 258 * 8 - 1] of byte = (
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
    /// you should better call this function to avoid forensic leaks
    procedure Done;
  end;

  /// store the information of a RSA key
  // - holding all its PBigInt values in its parent TRsaContext
  TRsa = class(TRsaContext)
  protected
    fM, fE, fD, fP, fQ, fDP, fDQ, fQInv: PBigInt;
    fModulusLen, fModulusBits: integer;
    /// compute the Chinese Remainder Theorem as needed by quick RSA decrypts
    function ChineseRemainderTheorem(b: PBigInt): PBigInt;
    // two virtual methods implementing default PKCS#1.5 RSA padding
    function DoUnPad(p: PByteArray; verify: boolean): RawByteString; virtual;
    function DoPad(p: pointer; n: integer; sign: boolean): RawByteString; virtual;
  public
    /// finalize the internal memory
    destructor Destroy; override;
    /// check if M and E fields are set
    function HasPublicKey: boolean;
    /// check if all fields are set, i.e. if a private key is stored
    function HasPrivateKey: boolean;
    /// load a public key from a decoded TRsaPublicKey record
    procedure LoadFromPublicKey(const PublicKey: TRsaPublicKey);
    /// load a public key from raw binary buffers
    // - fill M and E fields from the supplied binary buffers
    procedure LoadFromPublicKeyBinary(Modulus, Exponent: pointer;
      ModulusSize, ExponentSize: PtrInt);
    /// load a public key from PKCS#1 DER format
    function LoadFromPublicKeyDer(const Der: TCertDer): boolean;
    /// load a public key from PKCS#1 PEM format
    function LoadFromPublicKeyPem(const Pem: TCertPem): boolean;
    /// load a public key from an hexadecimal E and M fields concatenation
    procedure LoadFromPublicKeyHexa(const Hexa: RawUtf8);
    /// load a private key from a decoded TRsaPrivateKey record
    procedure LoadFromPrivateKey(const PrivateKey: TRsaPrivateKey);
    /// load a private key from PKCS#1 or PKCS#8 DER format
    function LoadFromPrivateKeyDer(const Der: TCertDer): boolean;
    /// load a private key from PKCS#1 or PKCS#8 PEM format
    function LoadFromPrivateKeyPem(const Pem: TCertPem): boolean;
    /// save the stored public key as a TRsaPublicKey record
    function SavePublicKey: TRsaPublicKey;
    /// save the stored public key in PKCS#1 DER format
    function SavePublicKeyDer: TCertDer;
    /// save the stored public key in PKCS#1 PEM format
    function SavePublicKeyPem: TCertPem;
    /// save the stored private key as a TRsaPrivateKey record
    // - caller should make Dest.Done once finished with the values
    procedure SavePrivateKey(out Dest: TRsaPrivateKey);
    /// save the stored private key in PKCS#1 DER format
    function SavePrivateKeyDer: TCertDer;
    /// save the stored private key in PKCS#1 PEM format
    function SavePrivateKeyPem: TCertPem;
    /// low-level PKCS#1.5 buffer Decryption or Verification
    // - Input should have ModulusLen bytes of data
    // - returns decrypted buffer without PKCS#1.5 padding, '' on error
    function BufferDecryptVerify(Input: pointer; Verify: boolean): RawByteString;
    /// low-level PKCS#1.5 buffer Encryption or Signature
    // - Input should have up to ModulusLen-11 bytes of data
    // - returns encrypted buffer with PKCS#1.5 padding, '' on error
    function BufferEncryptSign(Input: pointer; InputLen: integer;
      Sign: boolean): RawByteString;
    /// verification of a RSA binary signature
    // - returns the decoded binary OCTSTR Digest or '' if signature failed
    function Verify(const Signature: RawByteString;
      AlgorithmOid: PRawUtf8 = nil): RawByteString;
    /// compute a RSA binary signature of a given hash
    // - returns the encoded signature
    function Sign(Hash: PHash512; HashAlgo: THashAlgo): RawByteString;
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
    /// RSA key CRT exponent satisfying e * DP == 1 (mod (p-1))
    property DP: PBigInt
      read fDP;
    /// RSA key CRT exponent satisfying e * DQ == 1 (mod (q-1))
    property DQ: PBigInt
      read fDQ;
    /// RSA key coefficient satisfying q * qInv == 1 (mod p)
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

const
  /// the OID of a RSA encryption public key (PKCS#1)
  ASN1_OID_RSAPUB = '1.2.840.113549.1.1.1';

  /// the OID of the supported hash algorithms
  ASN1_OID_HASH: array[THashAlgo] of RawUtf8 = (
    '1.2.840.113549.2.5',       // hfMD5
    '1.3.14.3.2.26',            // hfSHA1
    '2.16.840.1.101.3.4.2.1',   // hfSHA256
    '2.16.840.1.101.3.4.2.2',   // hfSHA384
    '2.16.840.1.101.3.4.2.3',   // hfSHA512
    '2.16.840.1.101.3.4.2.6',   // hfSHA512_256
    '2.16.840.1.101.3.4.2.8',   // hfSHA3_256
    '2.16.840.1.101.3.4.2.10'); // hfSHA3_512


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

procedure ValuesSwap(value: PHalfUIntArray; data: PByteArray; bytes: integer);
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
  if n = Size then
    exit;
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

function TBigInt.IsEven: boolean;
begin
  result := (Value[0] and 1) = 0;
end;

function TBigInt.IsOdd: boolean;
begin
  result := (Value[0] and 1) <> 0;
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

function TBigInt.BitSetCount: integer;
var
  i: PtrInt;
begin
  result := 0;
  for i := 0 to Size - 1 do
    inc(result, GetBitsCountPtrInt(Value[i]));
end;

function TBigInt.Compare(b: PBigInt; andrelease: boolean): integer;
var
  i: PtrInt;
begin
  result := CompareInteger(Size, b^.Size);
  if result = 0 then
    for i := Size - 1 downto 0 do
    begin
      result := CompareBI(Value[i], b^.Value[i]);
      if result <> 0 then
        break;
    end;
  if andrelease then
    Release;
end;

function TBigInt.Compare(u: HalfUInt; andrelease: boolean): integer;
begin
  result := CompareInteger(Size, 1);
  if result = 0 then
    result := CompareBI(Value[0], u);
  if andrelease then
    Release;
end;

function TBigInt.SetPermanent: PBigInt;
begin
  if RefCnt <> 1 then
    raise ERsaException.CreateUtf8(
      'TBigInt.SetPermanent(%): RefCnt=%', [@self, RefCnt]);
  RefCnt := -1;
  result := @self;
end;

function TBigInt.ResetPermanent: PBigInt;
begin
  if RefCnt >= 0 then
    raise ERsaException.CreateUtf8(
      'TBigInt.ResetPermanent(%): RefCnt=%', [@self, RefCnt]);
  RefCnt := 1;
  result := @self;
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

{$ifdef USEBARRET}
function TBigInt.TruncateMod(modulus: integer): PBigInt;
begin
  if Size > modulus then
    Size := modulus;
  result := @self;
end;
{$endif USEBARRET}

function TBigInt.Copy: PBigInt;
begin
  if RefCnt >= 0 then
    inc(RefCnt);
  result := @self;
end;

function TBigInt.FindMaxBit: integer;
begin
  for result := Size * HALF_BITS - 1 downto 0 do
    if BitIsSet(result) then // fast enough
      exit;
  result := -1;
end;

function TBigInt.FindMinBit: integer;
begin
  for result := 0 to Size * HALF_BITS - 1 do
    if BitIsSet(result) then // fast enough
      exit;
  result := 0;
end;

function TBigInt.ShrBits(bits: integer): PBigInt;
var
  n: integer;
  a: PHalfUInt;
  v: PtrUInt;
begin
  result := @self;
  if bits <= 0 then
    exit;
  n := bits shr HALF_SHR;
  if n <> 0 then
    RightShift(n);
  bits := bits and pred(HALF_BITS);
  if bits = 0 then
    exit;
  n := Size;
  a := @Value[n];
  v := 0;
  repeat
    dec(a);
    v := (v shl HALF_BITS) + a^;
    a^ := v shr bits;
    dec(n);
  until n = 0;
  Trim;
end;

function TBigInt.ShlBits(bits: integer): PBigInt;
var
  n: integer;
  a: PHalfUInt;
  v: PtrUInt;
begin
  result := @self;
  if bits <= 0 then
    exit;
  n := bits shr HALF_SHR;
  if n <> 0 then
    LeftShift(n);
  bits := bits and pred(HALF_BITS);
  if bits = 0 then
    exit;
  a := pointer(Value);
  v := 0;
  n := Size;
  repeat
    inc(v, PtrUInt(a^) shl bits);
    a^ := v;
    v := v shr HALF_BITS;
    inc(a);
    dec(n);
  until n = 0;
  if v = 0 then
    exit;
  n := Size;
  Resize(n + 1, {nozero=}true);
  Value[n] := v;
end;

function TBigInt.GreatestCommonDivisor(b: PBigInt): PBigInt;
var
  ta, tb: PBigInt;
  z: integer;
begin
  // see https://www.di-mgt.com.au/euclidean.html#code-binarygcd
  if IsZero or
     b^.IsZero then
    raise ERsaException.Create('Unexpected TBigInt.GreatestCommonDivisor(0)');
  ta := Clone;
  tb := b.Clone;
  z := Min(ta.FindMinBit, tb.FindMinBit);
  while not ta.IsZero do
  begin
    // divisions by 2 preserve the invariant
    ta.ShrBits(ta.FindMinBit);
    tb.ShrBits(tb.FindMinBit);
    // set either ta or tb to abs(ta-tb)/2
    if ta.Compare(tb) >= 0 then
      ta.Substract(tb.Copy).ShrBits
    else
      tb.Substract(ta.Copy).ShrBits;
  end;
  ta.Release;
  result := tb.ShlBits(z);
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
  if @self <> nil then
    Done.ResetPermanent.Release;
end;

function TBigInt.Clone: PBigInt;
begin
  result := Owner.Allocate(Size, {nozero=}true);
  MoveFast(Value[0], result^.Value[0], Size * HALF_BYTES);
end;

function TBigInt.Done: PBigInt;
begin
  result := @self;
  if result <> nil then
    FillCharFast(Value^[0], Capacity * HALF_BYTES, 0); // anti-forensic
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
    Resize(n + 1);
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
  n, bs: integer;
  pa, pb: PHalfUInt;
  v: PtrUInt;
begin
  n := Size;
  bs := b^.Size;
  b^.Resize(n);
  pa := pointer(Value);
  pb := pointer(b^.Value);
  v := 0;
  {$ifdef CPUX64}
  while n >= _x64subn div HALF_BYTES do // substract 1024-bit per loop
  begin
    v := _x64sub(pa, pb, v);
    inc(PByte(pa), _x64subn);
    inc(PByte(pb), _x64subn);
    dec(n, _x64subn div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUX64}
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
  if b^.Size > bs then
    b^.Size := bs;
  b.Release;
  result := Trim;
end;

function TBigInt.IntMultiply(b: HalfUInt): PBigInt;
var
  a, r: PHalfUInt;
  v: PtrUInt;
  n: integer;
begin
  result := Owner.Allocate(Size + 1, {nozero=}true);
  a := pointer(Value);
  r := pointer(result^.Value);
  v := 0;
  n := Size;
  {$ifdef CPUX64}
  while n >= _x64muln div HALF_BYTES do // multiply 512-bit per loop
  begin
    v := _x64mul(a, r, b, v);
    inc(PByte(a), _x64muln);
    inc(PByte(r), _x64muln);
    dec(n, _x64muln div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUX64}
    repeat
      inc(v, PtrUInt(a^) * b);
      r^ := v;
      v := v shr HALF_BITS; // carry
      inc(a);
      inc(r);
      dec(n);
    until n = 0;
  r^ := v;
  Release;
  result^.Trim;
end;

function TBigInt.IntDivide(b: HalfUInt; optmod: PHalfUInt): PBigInt;
var
  n: integer;
  a: PHalfUInt;
  v, d: PtrUInt;
begin
  n := Size;
  a := @Value[n];
  v := 0;
  {$ifdef CPUX64}
  while n >= _x64divn div HALF_BYTES do // divide 1024-bit per loop
  begin
    dec(PByte(a), _x64divn);
    v := _x64div(a, b, v);
    dec(n, _x64divn div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUX64}
    repeat
      dec(a);
      v := (v shl HALF_BITS) + a^; // inject carry as high bits
      d := v div b;
      a^ := d;
      dec(v, d * b); // fast v := v mod b
      dec(n);
    until n = 0;
  if optmod <> nil then
    optmod^ := v;
  result := Trim;
end;

function TBigInt.IntMod(b: HalfUInt): PtrUInt;
var
  v: PHalfUInt;
  bb: PtrUInt;
  n: integer;
begin
  bb := b;
  n := Size;
  v := @Value[n];
  result := 0;
  {$ifdef CPUX64}
  while n >= _x64modn div HALF_BYTES do // mod 1024-bit per loop
  begin
    dec(PByte(v), _x64modn);
    result := _x64mod(v, bb, result);
    dec(n, _x64modn div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUX64}
    repeat
      dec(v);
      result := ((result shl HALF_BITS) + v^) mod bb;
      dec(n);
    until n = 0;
end;

function TBigInt.IntDivMod10: PtrUInt;
var
  d: PtrUInt;
  i: PtrInt;
begin
  i := Size - 1;
  result := Value[i];
  d := result div 10;
  if d = 0 then
    dec(Size); // auto trim
  Value[i] := d;
  dec(result, d * 10);
  while i <> 0 do
  begin
    dec(i);
    result := (result shl HALF_BITS) + Value[i];
    d := result div 10;  // fast multiplication by reciprocal on FPC
    Value[i] := d;
    dec(result, d * 10);
  end;
end;

const
  BIGINT_PRIMES_LAST: array[TBigIntSimplePrime] of integer = (
    53,                         // bspFast < 256
    302,                        // bspMost < 2000
    high(BIGINT_PRIMES_DELTA)); // bspAll  < 18000

function TBigInt.MatchKnownPrime(Extend: TBigIntSimplePrime): boolean;
var
  i: PtrInt;
  v: HalfUInt;
begin
  if not IsZero then
  begin
    result := true;
    if IsEven then // same as IntMod(2) = 0
      exit;
    v := 2; // start after 2, i.e. at 3
    for i := 1 to BIGINT_PRIMES_LAST[Extend] do
    begin
      inc(v, BIGINT_PRIMES_DELTA[i]);
      if IntMod(v) = 0 then
        exit;
    end;
  end;
  result := false;
end;

function TBigInt.IsPrime(Extend: TBigIntSimplePrime; Iterations: integer): boolean;
var
  r, a, w: PBigInt;
  s, n, attempt, bak: integer;
  v: PtrUInt;
  gen: PLecuyer;
begin
  result := false;
  // first check if not a factor of a well-known small prime
  if IsZero or
     (Iterations <= 0) or
     MatchKnownPrime(Extend) then
    exit;
  // validate is a prime number using Miller-Rabin tests
  bak := RefCnt;
  RefCnt := -1; // make permanent for use as modulo below
  w := Clone.IntSub(1); // w = value-1
  r := w.Clone;
  a := Owner.Allocate(Size, {nozero=}true);
  try
    // compute s = lsb(w) and r = w shr s
    s := r.FindMinBit;
    r.ShrBits(s);
    gen := Lecuyer;
    while Iterations > 0 do
    begin
      dec(Iterations);
      // generate random 1 < a < value - 1
      attempt := 0;
      repeat
        inc(attempt);
        if attempt = 30 then
          exit; // random generator seems pretty weak
        if Size > 2 then
        begin
          repeat
            n := gen.Next(Size);
          until n > 1;
          gen.Fill(@a^.Value[0], n * HALF_BYTES);
          a^.Value[0] := a^.Value[0] or 1; // odd
          a^.Size := n;
          a^.Trim;
        end
        else
        begin
          if Size = 1 then
            v := gen.Next(Value[0])
          else
            v := gen.Next;
          a^.Value[0] := v or 1; // odd
          a^.Size := 1;
        end;
      until (a.Compare(1) > 0) and
            (a.Compare(w) < 0);
      // search if a is composite
      a := Owner.ModPower(a, r.Copy, @self); // a = a^r mod value
      if (a.Compare(1) = 0) or
         (a.Compare(w) = 0) then
        continue; // this random is related: try another random
      for n := 1 to s - 1 do
      begin
        a := Owner.Reduce(a.Square, @self); // a = (a*a) mod value
        if (a.Compare(w) = 0) or
           (a.Compare(1) = 0) then
          break;
      end;
      if (a.Compare(w) <> 0) or
         (a.Compare(1) = 0) then
        exit; // not a prime
    end;
    result := true;
  finally
    a.Release;
    r.Release;
    w.Release;
    RefCnt := bak;
  end;
end;

const
  // ensure generated number is at least (nbits - 1) + 0.5 bits
  FIPS_MIN = $b504f334;

function TBigInt.FillPrime(Extend: TBigIntSimplePrime; Iterations: integer;
  EndTix: Int64): boolean;
var
  n, min: integer;
begin
  n := Size;
  result := n > 2;
  if not result then
    exit;
  if EndTix <= 0 then
    EndTix := GetTickCount64 + 60000; // never wait forever - 1 min seems enough
  // FIPS 4.48: 2^-100 error probability, number of rounds computed based on HAC
  if n >= 1450 shr HALF_SHR then
    min := 4
  else if n >= 1150 shr HALF_SHR then
    min := 5
  else if n >= 1000 shr HALF_SHR then
    min := 6
  else if n >= 850 shr HALF_SHR then
    min := 7
  else if n >= 750 shr HALF_SHR then
    min := 8
  else if n >= 500 shr HALF_SHR then
    min := 13
  else if n >= 250 shr HALF_SHR then
    min := 28
  else if n >= 150 shr HALF_SHR then
    min := 40
  else
    min := 51;
  if Iterations < min then
    Iterations := min;
  // compute a random number following FIPS 186-4 §B.3.3 steps 4.4, 5.5
  min := 1024;
  repeat
    TAesPrng.Fill(Value, n * HALF_BYTES); // our cryptographic PRNG
    Value[0] := Value[0] or 1; // set lower bit to ensure is an odd number
    Value[n - 1] := Value[n - 1] or (RSA_RADIX shr 1); // exact bit size
    dec(min);
    if min = 0 then
      exit; // too weak PNRG for sure
  until PCardinal(@Value[n - 1 {$ifdef CPU32} - 1 {$endif}])^ >= FIPS_MIN;
  // search for the next prime starting at this point
  repeat
    if IsPrime(Extend, Iterations) then
      exit; // we got lucky
    IntAdd(2);
  until GetTickCount64 > EndTix; // IsPrime() may be slow for sure
  result := false; // timed out
end;

procedure TBigInt.Debug(const name: shortstring; full: boolean);
var
  tmp: RawUtf8;
begin
  if full or
     (Size < 10) then
    tmp := ToText;
  ConsoleWrite('%: size=% used=% refcnt=% hash=%  %',
    [name, Size, UsedBytes, RefCnt, CardinalToHexShort(ToHash), tmp]);
end;

function TBigInt.UsedBytes: integer;
begin
  result := Size * HALF_BYTES;
  while (result > 1) and
        (PByteArray(Value)^[result - 1] = 0) do
    dec(result); // trim left 00
end;

function TBigInt.ToHash: cardinal;
begin
  if @self = nil then
    result := 0
  else
    result := crc32c(0, pointer(Value), UsedBytes);
end;

function TBigInt.ToHexa: RawUtf8;
begin
  if @self = nil then
    result := ''
  else
    result := BinToHexDisplay(pointer(Value), UsedBytes);
end;

function TBigInt.ToText(noclone: boolean): RawUtf8;
var
  v: PBigInt;
  tmp: TTextWriterStackBuffer;
  p: PByte;
begin
  if @self = nil then
    result := ''
  else
    case Size of
      0:
        result := SmallUInt32Utf8[0];
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
          p^ := v.IntDivMod10 + ord('0'); // fast enough (used for display only)
        until (v.Size = 0) or
              (p = @tmp); // truncate after 8190 digits (unlikely)
        v.Release;
        FastSetString(result, p, PAnsiChar(@tmp[high(tmp)]) - pointer(p));
      end;
    end;
end;

function TBigInt.Divide(v: PBigInt; Compute: TBigIntDivide;
  Remainder: PPBigInt): PBigInt;
var
  d, inner, dash, halfmod: HalfUInt;
  lastt, lastt2, lastv, lastv2: PtrUInt;
  neg: boolean;
  j, m, n, orgsiz: integer;
  p: PHalfUInt;
  u, quo, tmp: PBigInt;
begin
  if Compare(v) < 0 then
  begin
    if Compute = bidDivide then
    begin
      result := Owner.AllocateFrom(0);
      if Remainder <> nil then
        Remainder^ := Clone;
    end
    else
      result := Clone;
    v.Release;
    exit;
  end
  else if v.Size = 1 then
  begin
    quo := Clone.IntDivide(v.Value[0], @halfmod);
    if Compute = bidDivide then
    begin
      result := quo;
      if Remainder <> nil then
        Remainder^ := Owner.AllocateFrom(halfmod)
    end
    else
    begin
      quo.Release;
      result := Owner.AllocateFrom(halfmod);
    end;
    v.Release;
    exit;
  end;
  if Remainder <> nil then
    Remainder^ := nil;
  m := Size - v^.Size;
  n := v^.Size + 1;
  orgsiz := Size;
  quo := Owner.Allocate(m + 1);
  tmp := Owner.Allocate(n, {nozero=}true);
  v.Trim;
  d := RSA_RADIX div (PtrUInt(v^.Value[v^.Size - 1]) + 1);
  u := Clone;
  if d > 1 then
  begin
    // Normalize
    u := u.IntMultiply(d);
    if (Compute = bidModNorm) and
       (Owner.fNormMod[Owner.CurrentModulo] <> nil) then
      v := Owner.fNormMod[Owner.CurrentModulo]
    else
      v := v.IntMultiply(d);
  end;
  if orgsiz = u^.Size then
    u.Resize(orgsiz + 1); // allocate additional digit
  for j := 0 to m do
  begin
    // work on a temporary short version of u
    MoveFast(u^.Value[u^.Size - n - j], tmp^.Value[0], n * HALF_BYTES);
    // compute q'
    lastt := tmp^.Value[tmp^.Size - 1];
    lastv := v^.Value[v^.Size - 1];
    if lastt = lastv then
      dash := RSA_RADIX - 1
    else
    begin
      lastt2 := tmp^.Value[tmp^.Size - 2];
      dash := (PtrUInt(lastt) * RSA_RADIX + lastt2) div lastv;
      if v^.Size > 1 then
      begin
        lastv2 := v^.Value[v^.Size - 2];
        if lastv2 > 0 then
        begin
          inner := (RSA_RADIX * lastt + lastt2 - PtrUInt(dash) * lastv);
          if (PtrUInt(lastv2 * dash) >
               (PtrUInt(inner) * RSA_RADIX + tmp^.Value[tmp^.Size - 3])) then
            dec(dash);
        end;
      end;
    end;
    p := @quo^.Value[quo^.Size - j - 1];
    if dash > 0 then
    begin
      // multiply by dash and subtract
      tmp := tmp.Substract(v.Copy.IntMultiply(dash), @neg);
      tmp.Resize(n);
      p^ := dash;
      if neg then
      begin
        // add back
        dec(p^);
        tmp := tmp.Add(v.Copy);
        // trim the carry
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
  if Compute in [bidMod, bidModNorm] then
  begin
    // return the remainder
    quo.Release;
    result := u.Trim.IntDivide(d);
  end
  else
  begin
    // return the quotient and optionally the remainder
    result := quo.Trim;
    if Remainder <> nil then
      Remainder^ := u.Trim.IntDivide(d)
    else
      u.Release;
  end
end;

function TBigInt.Modulo(v: PBigInt): PBigInt;
begin
  result := Divide(v.Copy, bidMod);
end;

function TBigInt.Multiply(b: PBigInt): PBigInt;
var
  r: PBigInt;
  i, n: PtrInt;
  v, vi: PtrUInt;
  p, u: PHalfUInt;
begin
  r := Owner.Allocate(Size + b^.Size);
  for i := 0 to b^.Size - 1 do // O(n2) brute force algorithm
  begin
    v := 0; // initial carry value
    vi := b^.Value[i];
    p := @r^.Value[i];
    u := pointer(Value);
    n := Size;
    {$ifdef CPUX64}
    while n >= _x64multn div HALF_BYTES do // multiply 512-bit per loop
    begin
      v := _x64mult(u, p, vi, v);
      inc(PByte(p), _x64multn);
      inc(PByte(u), _x64multn);
      dec(n, _x64multn div HALF_BYTES);
    end;
    if n > 0 then
    {$endif CPUX64}
      repeat
        inc(v, PtrUInt(p^) + PtrUInt(u^) * vi);
        p^ := v;
        inc(p);
        inc(u);
        v := v shr HALF_BITS; // carry
        dec(n);
      until n = 0;
    p^ := v;
  end;
  Release;
  b.Release;
  result := r.Trim;
end;

function TBigInt.MultiplyPartial(b: PBigInt;
  InnerPartial, OuterPartial: PtrInt): PBigInt;
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
    if (OuterPartial > 0) and
       (OuterPartial > i) and
       (OuterPartial < n) then
    begin
      k := OuterPartial - 1;
      j := k - i;
    end;
    repeat
      if (InnerPartial > 0) and
         (k >= InnerPartial) then
        break;
      inc(v, PtrUInt(r^.Value[k]) + PtrUInt(Value[j]) * b^.Value[i]);
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

function TBigInt.IntAdd(b: HalfUInt): PBigInt;
var
  tmp: PBigInt;
begin
  if b <> 0 then
  begin
    tmp := Owner.Allocate(Size);
    tmp^.Value[0] := b;
    result := Add(tmp);
  end
  else
    result := @self;
end;

function TBigInt.IntSub(b: HalfUInt): PBigInt;
var
  tmp: PBigInt;
begin
  if b <> 0 then
  begin
    tmp := Owner.Allocate(Size);
    tmp^.Value[0] := b;
    result := Substract(tmp);
  end
  else
    result := @self;
end;


{ TRsaContext }

constructor TRsaContext.Create;
begin
  fRadix := Allocate(2, {nozero=}true);
  fRadix^.Value[0] := 0;
  fRadix^.Value[1] := 1;
  fRadix^.SetPermanent;
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
    if b^.Value <> nil then
      FreeMem(b^.Value);
    FreeMem(b);
    b := next;
  end;
  inherited Destroy;
end;

procedure TRsaContext.Release(const b: array of PPBigInt);
var
  i: PtrInt;
begin
  for i := 0 to high(b) do
  begin
    b[i]^^.Release;
    b[i]^ := nil;
  end;
end;

function TRsaContext.AllocateFrom(v: HalfUInt): PBigInt;
begin
  result := Allocate(1, {nozero=}true);
  result^.Value[0] := v;
end;

function TRsaContext.AllocateFromHex(const hex: RawUtf8): PBigInt;
var
  n: cardinal;
begin
  n := length(hex) shr 1;
  result := Allocate(n div HALF_BYTES, {nozero=}true);
  if not HexDisplayToBin(pointer(hex), pointer(result^.Value), n) then
    Release([@result]);
end;

const
  RSA_DEFAULT_ALLOCATE = 2048 shr HALF_SHR; // seems fair enough

function TRsaContext.Allocate(n: integer; nozero: boolean): PBigint;
begin
  if self = nil then
    raise ERsaException.CreateUtf8('TRsa.Allocate(%): Owner=nil', [n]);
  result := fFreeList;
  if result <> nil then
  begin
    // we can recycle a pre-allocated buffer
    if result^.RefCnt <> 0 then
      raise ERsaException.CreateUtf8(
        '%.Allocate(%): % RefCnt=%', [self, n, result, result^.RefCnt]);
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
    result^.Capacity := NextGrow(Max(RSA_DEFAULT_ALLOCATE, n)); // over-alloc
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
  ValuesSwap(result.Value, data, bytes);
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
  fMod[modulo] := b;
  d := RSA_RADIX div (PtrUInt(b^.Value[k - 1]) + 1);
  fNormMod[modulo] := b.IntMultiply(d).SetPermanent;
  {$ifdef USEBARRET}
  b := fRadix.Clone.LeftShift(k * 2 - 1);
  fMu[modulo] := b.Divide(fMod[modulo]).SetPermanent;
  b.Release;
  fBk1[modulo] := AllocateFrom(1).LeftShift(k + 1).SetPermanent; // = b(k+1)
  {$endif USEBARRET}
end;

procedure TRsaContext.ResetModulo(modulo: TRsaModulo);
begin
  fMod[modulo].ResetPermanentAndRelease; // also zeroed
  fNormMod[modulo].ResetPermanentAndRelease;
  {$ifdef USEBARRET}
  fMu[modulo].ResetPermanentAndRelease;
  fBk1[modulo].ResetPermanentAndRelease;
  {$endif USEBARRET}
end;

{$ifdef USEBARRET}
function TRsaContext.Reduce(b, m: PBigint): PBigInt;
var
  q1, q2, q3, r1, r2: PBigInt;
  k: integer;
begin
  if m <> nil then
  begin
    result := b^.Divide(m, bidMod); // custom modulo has no pre-computed const
    b^.Release;
    exit;
  end;
  m := fMod[CurrentModulo];
  if b^.Compare(m) < 0 then
  begin
    result := b; // just return b if b < m
    exit;
  end;
  k := m^.Size;
  if b^.Size > k * 2 then
  begin
    // use regular divide/modulo method - Barrett cannot help
    result := b^.Divide(m, bidModNorm);
    b^.Release;
    exit;
  end;
  // q1 = [x / b**(k-1)]
  q1 := b^.Clone.RightShift(k - 1);
  //writeln(#10'q1=',q1.ToHexa);
  //writeln(#10'mu=',fMu[CurrentModulo].ToHexa);
  // Do outer partial multiply
  // q2 = q1 * mu
  q2 := q1.MultiplyPartial(fMu[CurrentModulo], 0, k - 1);
  //writeln(#10'q2=',q2.tohexa);
  // q3 = [q2 / b**(k+1)]
  q3 := q2.RightShift(k + 1);
  //writeln(#10'q3=',q3.tohexa);
  // r1 = x mod b**(k+1)
  r1 := b^.TruncateMod(k + 1);
  //writeln(#10'r1=',r1.tohexa);
  // Do inner partial multiply
  // r2 = q3 * m mod b**(k+1)
  r2 := q3.MultiplyPartial(m, k + 1, 0).TruncateMod(k + 1);
  //writeln(#10'r2=',r2.tohexa);
  // if (r1 < r2) r1 = r1 + b**(k+1)
  if r1.Compare(r2) < 0 then
    r1 := r1.Add(fBk1[CurrentModulo]);
  // r = r1-r2
  result := r1.Substract(r2);
  //writeln(#10'r=',result.tohexa);
  // while (r >= m) do r = r-m
  while result.Compare(m) >= 0 do
    result.Substract(m);
end;
{$else}
function TRsaContext.Reduce(b, m: PBigint): PBigInt;
begin
  if m = nil then
    result := b^.Divide(fMod[CurrentModulo], bidModNorm)
  else
    result := b^.Divide(m, bidMod); // custom modulo has no pre-computed const
  b^.Release;
end;
{$endif USEBARRET}

function TRsaContext.ModPower(b, exp, m: PBigInt): PBigInt;
var
  g2: PBigInt;
  i, j, k, l, partial, windowsize: integer;
  g: array of PBigInt;
begin
  i := exp.FindMaxExponentIndex;
  if i < 0 then
    raise ERsaException.CreateUtf8('%.ModPower with exp=0', [self]);
  result := AllocateFrom(1);
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
  g2 := Reduce(g[0].Square, m); // g2 := residue of g^2
  for j := 1 to k - 1 do
    g[j] := Reduce(g[j - 1].Multiply(g2.Copy), m).SetPermanent;
  g2.Done.Release;
  // reduce to left-to-right exponentiation, one exponent bit at a time
  // e.g. 65537 = 2^16 + 2^1
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
        result := Reduce(result.Square, m);
        if exp.BitIsSet(j) then
          inc(partial);
        if j <> l then
          partial := partial shl 1;
        dec(j);
      end;
      partial := (partial - 1) shr 1; // Adjust for array
      result := Reduce(result.Multiply(g[partial]), m);
      i := l - 1;
    end
    else
    begin
      // bit not set: just process the next bit
      result := Reduce(result.Square, m);
      dec(i);
    end;
  until i < 0;
  // memory cleanup
  for i := 0 to k - 1 do
    g[i].ResetPermanentAndRelease;
  b.Release;
  exp.Release;
end;


{ **************** RSA Low-Level Cryptography Functions }

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
                (oid = AsnEncOid(ASN1_OID_RSAPUB)) and
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
                  AsnOid(ASN1_OID_RSAPUB),
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
                  AsnOid(ASN1_OID_RSAPUB),
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

procedure TRsaPrivateKey.Done;
begin
  FillZero(Modulus);
  FillZero(PublicExponent);
  FillZero(PrivateExponent);
  FillZero(Prime1);
  FillZero(Prime2);
  FillZero(Exponent1);
  FillZero(Exponent2);
  FillZero(Coefficient);
end;


{ TRsa }

destructor TRsa.Destroy;
begin
  // free key variables with anti-forensic buffer zeroing
  ResetModulo(rmM);
  ResetModulo(rmP);
  ResetModulo(rmQ);
  fE.ResetPermanentAndRelease;
  fD.ResetPermanentAndRelease;
  fDP.ResetPermanentAndRelease;
  fDQ.ResetPermanentAndRelease;
  fQInv.ResetPermanentAndRelease;
  // fM fP fQ are already finalized by ResetModulo()
  inherited Destroy;
end;

function TRsa.HasPublicKey: boolean;
begin
  result := (self <> nil) and
            not fM.IsZero and
            not fE.IsZero;
end;

function TRsa.HasPrivateKey: boolean;
begin
  result := HasPublicKey and
            not fD.IsZero and
            not fP.IsZero and
            not fQ.IsZero and
            not fDP.IsZero and
            not fDQ.IsZero and
            not fQInv.IsZero;
end;

procedure TRsa.LoadFromPublicKeyBinary(Modulus, Exponent: pointer;
  ModulusSize, ExponentSize: PtrInt);
begin
  if not fM.IsZero then
    raise ERsaException.CreateUtf8(
      '%.LoadFromPublicKey on existing data', [self]);
  if (ModulusSize < 10) or
     (ExponentSize < 2) then
    raise ERsaException.CreateUtf8(
      '%.LoadFromPublicKey: unexpected ModulusSize=% ExponentSize=%',
      [self, ModulusSize, ExponentSize]);
  fModulusLen := ModulusSize;
  fM := Load(Modulus, ModulusSize).SetPermanent;
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
  LoadFromPublicKeyBinary(b + 3, b, length(bin) - 3, 3);
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
       (length(Modulus) < 10) or
       (length(PublicExponent) < 2) then
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

function TRsa.LoadFromPrivateKeyDer(const Der: TCertDer): boolean;
var
  key: TRsaPrivateKey;
begin
  try
    result := key.FromDer(Der);
    if result then
      LoadFromPrivateKey(key);
  finally
    key.Done;
  end;
end;

function TRsa.LoadFromPrivateKeyPem(const Pem: TCertPem): boolean;
var
  der: TCertDer;
begin
  try
    der := PemToDer(Pem);
    result := LoadFromPrivateKeyDer(der);
  finally
    FillZero(RawByteString(der));
  end;
end;

function TRsa.SavePublicKey: TRsaPublicKey;
begin
  result.Modulus := fM.Save;
  result.Exponent := fE.Save;
end;

function TRsa.SavePublicKeyDer: TCertDer;
begin
  if HasPublicKey then
    result := SavePublicKey.ToDer
  else
    result := '';
end;

function TRsa.SavePublicKeyPem: TCertPem;
begin
  result := DerToPem(SavePublicKeyDer, pemRsaPublicKey);
end;

procedure TRsa.SavePrivateKey(out Dest: TRsaPrivateKey);
begin
  Dest.Version := 0;
  Dest.Modulus := fM.Save;
  Dest.PublicExponent := fE.Save;
  Dest.PrivateExponent := fD.Save;
  Dest.Prime1 := fP.Save;
  Dest.Prime2 := fQ.Save;
  Dest.Exponent1 := fDP.Save;
  Dest.Exponent2 := fDQ.Save;
  Dest.Coefficient := fQInv.Save;
end;

function TRsa.SavePrivateKeyDer: TCertDer;
var
  key: TRsaPrivateKey;
begin
  if HasPrivateKey then
  begin
    SavePrivateKey(key);
    try
      result := key.ToDer
    finally
      key.Done;
    end;
  end
  else
    result := '';
end;

function TRsa.SavePrivateKeyPem: TCertPem;
var
  der: TCertDer;
begin
  try
    der := SavePrivateKeyDer;
    result := DerToPem(der, pemRsaPrivateKey);
  finally
    FillZero(RawByteString(der));
  end;
end;

function TRsa.ChineseRemainderTheorem(b: PBigInt): PBigInt;
var
  h, m1, m2: PBigInt;
begin
  result := nil;
  if not HasPrivateKey then
    exit;
  CurrentModulo := rmP;
  m1 := ModPower(b.Copy, fDp, nil);
  CurrentModulo := rmQ;
  m2 := ModPower(b, fDq, nil);
  h := m1.Add(fP).Substract(m2.Copy).Multiply(fQInv);
  CurrentModulo := rmP;
  h := Reduce(h, nil);
  result := m2.Add(q.Multiply(h));
end;

function TRsa.DoUnPad(p: PByteArray; verify: boolean): RawByteString;
var
  count, padding: integer;
begin
  result := '';
  count := 0;
  if p[count] <> 0 then
    exit; // leading zero
  inc(count);
  padding := 0;
  if Verify then
  begin
    if p[count] <> 1 then
      exit; // block type 1
    inc(count);
    while (count < fModulusLen) and
          (p[count] = $ff) do
    begin
      inc(count);
      inc(padding); // ignore FF padding
    end;
  end
  else
  begin
    if p[count] <> 2 then
      exit; // block type 2
    inc(count);
    while (count < fModulusLen) and
          (p[count] <> 0) do
    begin
      inc(count);
      inc(padding); // ignore non-zero random padding
    end;
  end;
  if (count = fModulusLen) or
     (padding = 8) or
     (p[count] <> 0) then
    exit; // invalid padding with ending zero
  inc(count);
  FastSetRawByteString(result, @p[count], fModulusLen - count);
end;

function TRsa.DoPad(p: pointer; n: integer; sign: boolean): RawByteString;
var
  padding: integer;
  i: PtrInt;
  r: PByteArray absolute result;
begin
  result := '';
  padding := fModulusLen - n - 3;
  if (p = nil) or
     (padding < 8) or
     not HasPublicKey then
    exit;
  SetLength(result, fModulusLen);
  r[0] := 0; // leading zero
  if sign then
  begin
    r[1] := 1; // block type 1
    FillCharFast(r[2], padding, $ff);
    inc(padding, 2);
  end
  else
  begin
    r[1] := 2; // block type 2
    RandomBytes(@r[2], padding); // Lecuyer is enough
    inc(padding, 2);
    for i := 2 to padding - 1 do
      if r[i] = 0 then
        dec(r[i]); // should be non zero random padding
  end;
  r[padding] := 0; // padding ends with zero
  MoveFast(p^, r[padding + 1], n);
end;

function TRsa.BufferDecryptVerify(Input: pointer; Verify: boolean): RawByteString;
var
  enc, dec: PBigInt;
  exp: RawByteString;
begin
  result := '';
  if (Input = nil) or
     not HasPublicKey then
    exit;
  enc := Load(Input, fModulusLen);
  // perform the RSA calculation
  if Verify then
  begin
    // verify with Public Key
    CurrentModulo := rmM; // for ModPower()
    dec := ModPower(enc, fE, nil); // calls enc.Release
  end
  else
    // decrypt with Private Key
    dec := ChineseRemainderTheorem(enc);
  if dec = nil then
    exit;
  // decode result following proper padding (PKCS#1.5 with TRsa class)
  exp := dec.Save({andrelease=}true);
  result := DoUnPad(pointer(exp), Verify);
end;

function TRsa.BufferEncryptSign(Input: pointer; InputLen: integer;
  Sign: boolean): RawByteString;
var
  enc, dec: PBigInt;
  exp: RawByteString;
begin
  result := '';
  // encode input using proper padding (PKCS#1.5 with TRsa class)
  exp := DoPad(Input, InputLen, Sign);
  dec := Load(pointer(exp), fModulusLen);
  // perform the RSA calculation
  if Sign then
    // sign with private key
    enc := ChineseRemainderTheorem(dec)
  else
  begin
    // encrypt with public key
    CurrentModulo := rmM; // for ModPower()
    enc := ModPower(dec, fE, nil); // calls dec.Release
  end;
  if enc <> nil then
    result := enc.Save({andrelease=}true);
end;

function TRsa.Verify(const Signature: RawByteString;
  AlgorithmOid: PRawUtf8): RawByteString;
var
  verif, digest: RawByteString;
  p: integer;
begin
  result := '';
  if length(Signature) <> fModulusLen then
    exit; // the signature is a RSA BigInt by definition
  verif := BufferDecryptVerify(pointer(Signature), {verify=}true);
  if verif = '' then
    exit; // invalid signature
  p := 1;
  if (AsnNext(p, verif) = ASN1_SEQ) and  // DigestInfo
     (AsnNext(p, verif) = ASN1_SEQ) and  // AlgorithmIdentifier
     (AsnNext(p, verif, pointer(AlgorithmOid)) = ASN1_OBJID) then
    case AsnNextRaw(p, verif, digest) of
      ASN1_NULL: // optional Algorithm Parameters
        if AsnNextRaw(p, verif, digest) = ASN1_OCTSTR then
          result := digest;
      ASN1_OCTSTR:
        result := digest;
    end;
end;

function TRsa.Sign(Hash: PHash512; HashAlgo: THashAlgo): RawByteString;
var
  seq: TAsnObject;
  h: RawByteString;
begin
  FastSetRawByteString(h, Hash, HASH_SIZE[HashAlgo]);
  seq := Asn(ASN1_SEQ, [
           Asn(ASN1_SEQ, [
             AsnOid(pointer(ASN1_OID_HASH[HashAlgo])),
             ASN1_NULL_VALUE
           ]),
           Asn(h)
         ]);
  result := BufferEncryptSign(pointer(seq), length(seq), {sign=}true);
end;



initialization

finalization

end.
