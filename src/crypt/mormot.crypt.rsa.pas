/// Framework Core RSA Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.rsa;

{
  *****************************************************************************

   Rivest-Shamir-Adleman (RSA) Public-Key Cryptography
    - RSA Oriented Big-Integer Computation
    - RSA Low-Level Cryptography Functions
    - Registration of our RSA Engine to the TCryptAsym Factory

  *****************************************************************************

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
   to any restriction about the use of cryptographic software in your country.
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
  - new pure pascal OOP design of BigInt computation optimized for RSA process
  - garbage collection of BigInt instances, with proper anti-forensic wiping
  - use half-registers (HalfUInt) for efficient computation on all CPUs
  - dedicated x86_64/i386 asm for core computation routines (noticeable speedup)
  - slower than OpenSSL, but likely the fastest FPC or Delphi native RSA library
  - includes FIPS-level RSA keypair validation and generation
  - features both RSASSA-PKCS1-v1_5 and RSASSA-PSS signature schemes
  - started as a fcl-hash fork, but full rewrite inspired by Mbed TLS source
  - references: https://github.com/Mbed-TLS/mbedtls and the Handbook of Applied
    Cryptography (HAC) at https://cacr.uwaterloo.ca/hac/about/chap4.pdf
  - will register as Asym 'RS256','RS384','RS512' algorithms (if not overriden
    by mormot.crypt.openssl), keeping 'RS256-int' and 'PS256-int' for this unit
  - used by mormot.crypt.x509 to handle RSA signatures of its X.509 Certificates
}

{.$define USEBARRET}
// could be defined to enable Barret reduction (slower and with wrong results)


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
  // - bspFast will search for known primes < 256 - e.g. 2048-bit at 250K/s
  // - bspMost will search for known primes < 2000 - e.g. 2048-bit at 45K/s and
  // is in practice sufficient to detect most primes (Mbed TLS check < 1000)
  // - bspAll will search for known primes < 18000 - e.g. 2048-bit at 6.5K/s
  // - see RSA_DEFAULT_GENERATION_KNOWNPRIME = bspMost constant below
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
    /// partial multiplication between two Big Integer values
    // - will eventually release both self and b instances
    function MultiplyPartial(b: PBigInt; InnerPartial: PtrInt;
      OuterPartial: PtrInt): PBigInt;
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
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// allocate a new Big Integer value with the same data as an existing one
    function Clone: PBigInt;
    /// mark the value with a RefCnt < 0
    function SetPermanent: PBigInt;
    /// mark the value with a RefCnt = 1
    function ResetPermanent: PBigInt;
    /// decreases the value RefCnt, saving it in the internal FreeList once done
    procedure Release;
    /// a wrapper to ResetPermanent then Release
    // - before release, fill the buffer with zeros to avoid forensic leaking
    procedure ResetPermanentAndRelease;
      {$ifdef HASSAFEINLINE} inline; {$endif}
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
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// standard multiplication between two Big Integer values
    // - will eventually release both self and b instances
    function Multiply(b: PBigInt): PBigInt;
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
    /// compute the modular inverse, i.e. self^-1 mod m
    // - will eventually release the m instance
    function ModInverse(m: PBigInt): PBigInt;
    /// check if this value is divisable by a small prime
    // - detection coverage can be customized from default primes < 2000
    function MatchKnownPrime(Extend: TBigIntSimplePrime): boolean;
    /// check if the number is (likely to be) a prime following HAC 4.44
    // - can set a known simple primes Extend and Miller-Rabin tests Iterations
    function IsPrime(Extend: TBigIntSimplePrime = bspMost;
      Iterations: integer = 10): boolean;
    /// guess a random prime number of the exact current size
    // - loop over TAesPrng.Fill and IsPrime method within a timeout period
    // - if Iterations is too low, FIPS 4.48 recommendation will be forced
    function FillPrime(Extend: TBigIntSimplePrime; Iterations: integer;
      EndTix: Int64): boolean;
    /// return the crc32c hash of this Big Integer value binary
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

  /// how TRsaContext.Allocate should create the new allocated block
  // - memory block is filled with 0 as by default raZeroed is defined
  // - by default, a capacity overhead is allowed to the returned memory buffer,
  // to avoid heap reallocation during computation - you can set raExactSize if
  // you know the buffer size won't change (e.g. from TRsaContext.LoadPermanent)
  TRsaAllocate = set of (
    raZeroed,
    raExactSize);

  /// store one Big Integer computation context for RSA
  // - will maintain its own set of reference-counted Big Integer values,
  // for fast thread-local reuse and automated safe anti-forensic wipe
  TRsaContext = class(TSynPersistent)
  private
    /// list of released PBigInt instance, ready to be re-used by Allocate()
    fFreeList: PBigInt;
    /// contains Modulus
    fMod: TRsaModulos;
    /// contains the normalized storage
    fNormMod: TRsaModulos;
    {$ifdef USEBARRET}
    /// contains mu
    fMu: TRsaModulos;
    /// contains b(k+1)
    fBk1: TRsaModulos;
    {$endif USEBARRET}
  public
    /// the size of the sliding window
    Window: integer;
    /// number of active PBigInt
    ActiveCount: integer;
    /// number of PBigInt instances stored in the internal instances cache
    FreeCount: integer;
    /// as set by SetModulo() and  used by Reduce() and ModPower()
    CurrentModulo: TRsaModulo;
    /// finalize this Big Integer context memory
    destructor Destroy; override;
    /// allocate a new zeroed Big Integer value of the specified precision
    // - n is the number of TBitInt.Value[] items to initialize
    function Allocate(n: integer; opt: TRsaAllocate = [raZeroed]): PBigint;
    /// allocate a new Big Integer value from a 16/32-bit unsigned integer
    function AllocateFrom(v: HalfUInt): PBigInt;
    /// allocate a new Big Integer value from a ToHexa dump
    function AllocateFromHex(const hex: RawUtf8): PBigInt;
    /// call b^^.Release and set b^ := nil
    procedure Release(const b: array of PPBigInt);
    /// fill all released values with zero as anti-forensic safety measure
    procedure WipeReleased;
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function Load(data: PByteArray; bytes: integer;
      opt: TRsaAllocate = []): PBigInt; overload;
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function LoadPermanent(const data: RawByteString): PBigInt; overload;
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
  /// generates RSA keypairs checking all known primes < 2000
  // - bspMost seems the best compromise between performance and safety, since
  // even Mbed TLS only try for primes < 1000
  // - in practice bspFast is only slightly faster, and bspAll seems overkill
  // - when profiling, the Miller-Rabin test takes 150 more time than bspMost
  RSA_DEFAULT_GENERATION_KNOWNPRIME = bspMost;

  /// generates RSA keypairs using a proven 2^-112 error probability from
  // Miller-Rabin iterations
  // - TBigInt.FillPrime will ensure FIPS 186-5 minimum iteration is always used
  RSA_DEFAULT_GENERATION_ITERATIONS = 0;

  /// generates RSA keypairs in a time-coherent fashion
  {$ifdef CPUARM}
  // - we have seen some weak Raspberry PI timeout so 30 seconds seems fair
  RSA_DEFAULT_GENERATION_TIMEOUTMS = 30000;
  {$else}
  // - allow 10 seconds: typical time is around (or less) 1 second on Intel/AMD
  RSA_DEFAULT_GENERATION_TIMEOUTMS = 10000;
  {$endif CPUARM}

  /// 2KB table of iterative differences of all known prime numbers < 18,000
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

/// compute the base-10 decimal text from a Big Integer binary buffer
// - wrap PBigInt.ToText from LoadPermanent(der) in a temporary TRsaContext
function BigIntToText(const der: TCertDer): RawUtf8;

/// branchless comparison of two Big Integer internal buffer values
function CompareBI(A, B: HalfUInt): integer;
  {$ifdef HASINLINE} inline; {$endif}


{ **************** RSA Low-Level Cryptography Functions }

type
  /// the TRsa.Generate method result
  TRsaGenerateResult = (
    rgrSuccess,
    rgrIncorrectParams,
    rgrTimeout,
    rgrRandomGeneratorFailure,
    rgrWeakBitsMayRetry);

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
    /// serialize this public key as ASN1_SEQ, as stored in a X509 certificate
    function ToSubjectPublicKey: RawByteString;
    /// unserialize a public key from binary PKCS#1 DER format
    // - will try and fallback to a ASN1_SEQ, as stored in a X509 certificate
    function FromDer(const der: TCertDer): boolean;
  end;

  /// store a RSA private key
  // - with DER (therefore PEM) serialization support
  // - we don't support any PKCS encryption yet - ensure the private key
  // PEM file is safely stored with proper user access restrictions
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

  /// main RSA processing class for both public or private key
  // - supports PEM/DER persistence, and can Generate a new key pair
  // - holds all its PBigInt values in its parent TRsaContext
  // - uses regular RSASSA-PKCS1-v1_5 encoding - see TRsaPss for RSASSA-PSS
  // - note that only Verify() and Sign() methods are thread-safe
  // - this implementation follows RFC 8017 specifications
  TRsa = class(TRsaContext)
  protected
    fSafe: TOSLightLock; // for Verify() and Sign() - not reentrant lock
    fM, fE, fD, fP, fQ, fDP, fDQ, fQInv: PBigInt;
    fModulusLen, fModulusBits: integer;
    /// compute the Chinese Remainder Theorem (CRT) for RSA sign/decrypt
    function ChineseRemainderTheorem(b: PBigInt): PBigInt;
    function Pkcs1UnPad(p: PByteArray; verify: boolean): RawByteString;
    function Pkcs1Pad(p: pointer; n: integer; sign: boolean): RawByteString;
  public
    /// initialize the RSA key context
    constructor Create; override;
    /// initialize and generate a new RSA key context
    // - this is the main factory to generate a new RSA keypair
    // - will call Create and Generate() with proper retry on rgrWeakBitsMayRetry
    // - returns nil on generation error (with a silent exception)
    class function GenerateNew(Bits: integer = RSA_DEFAULT_GENERATION_BITS;
      Extend: TBigIntSimplePrime = RSA_DEFAULT_GENERATION_KNOWNPRIME;
      Iterations: integer = RSA_DEFAULT_GENERATION_ITERATIONS;
      TimeOutMS: integer = RSA_DEFAULT_GENERATION_TIMEOUTMS): TRsa;
    /// finalize the RSA key context
    destructor Destroy; override;
    /// check if M and E fields are set
    function HasPublicKey: boolean;
    /// check if all fields are set, i.e. if a private key is stored
    function HasPrivateKey: boolean;
    /// ensure that private key stored CRT constants are mathematically coherent
    // - i.e. that they are properly derived for Chinese Remainder Theorem (CRT)
    function CheckPrivateKey: boolean;
    /// check that the stored key match the public key stored in another TRsa
    function MatchKey(RsaPublicKey: TRsa): boolean;
    /// compute a genuine RSA public/private key pair of a given bit size
    // - valid bit sizes are 512, 1024, 2048 (default), 3072, 4096 and 7680;
    // today's minimal is 2048-bit, but you may consider 3072-bit for security
    // beyond 2030, and 4096-bit have a much higher computational cost and
    // 7680-bit is highly impractical (e.g. generation can be more than 30 secs)
    // - since our generator is not yet officially validated by any agency,
    // anything above default 2048 would not make much sense
    // - searching for proper random primes may take a lot of time on low-end
    // CPU so a timeout period can be supplied (default 10 secs)
    // - if Iterations value is too low, the FIPS recommendation will be forced
    // - on a slow CPU or with a huge number of Bits, you can increase TimeOutMS
    // - hint: consider using the TRsa.GenerateNew factory , which would properly
    // handle rgrWeakBitsMayRetry result
    function Generate(Bits: integer = RSA_DEFAULT_GENERATION_BITS;
      Extend: TBigIntSimplePrime = RSA_DEFAULT_GENERATION_KNOWNPRIME;
      Iterations: integer = RSA_DEFAULT_GENERATION_ITERATIONS;
      TimeOutMS: integer = RSA_DEFAULT_GENERATION_TIMEOUTMS): TRsaGenerateResult;
    /// load a public key from a decoded TRsaPublicKey record
    procedure LoadFromPublicKey(const PublicKey: TRsaPublicKey);
    /// load a public key from raw binary buffers
    // - fill M and E fields from the supplied binary buffers
    procedure LoadFromPublicKeyBinary(Modulus, Exponent: pointer;
      ModulusSize, ExponentSize: PtrInt);
    /// load a public key from PKCS#1 DER format
    // - will try and fallback to a ASN1_SEQ, as stored in a X509 certificate
    function LoadFromPublicKeyDer(const Der: TCertDer): boolean;
    /// load a public key from PKCS#1 PEM format
    // - will also accept and try to load from the DER format if PEM failed
    function LoadFromPublicKeyPem(const Pem: TCertPem): boolean;
    /// load a public key from an hexadecimal E and M fields concatenation
    procedure LoadFromPublicKeyHexa(const Hexa: RawUtf8);
    /// load a private key from a decoded TRsaPrivateKey record
    procedure LoadFromPrivateKey(const PrivateKey: TRsaPrivateKey);
    /// load a private key from PKCS#1 or PKCS#8 DER format
    function LoadFromPrivateKeyDer(const Der: TCertDer): boolean;
    /// load a private key from PKCS#1 or PKCS#8 PEM format
    // - will also accept and try to load from the DER format if PEM failed
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
    /// low-level thread-safe PKCS#1.5 buffer Decryption
    // - Input should have ModulusLen bytes of data
    // - returns decrypted buffer without PKCS#1.5 padding, '' on error
    function Pkcs1Decrypt(Input: pointer): RawByteString;
    /// low-level thread-safe PKCS#1.5 buffer Verification
    // - Input should have ModulusLen bytes of data
    // - returns decrypted signature without PKCS#1.5 padding, '' on error
    function Pkcs1Verify(Input: pointer): RawByteString;
    /// low-level thread-safe PKCS#1.5 buffer Encryption
    // - InputLen should be < to ModulusLen - 11 bytes for proper padding
    // - returns encrypted buffer with PKCS#1.5 padding, '' on error
    function Pkcs1Encrypt(Input: pointer; InputLen: integer): RawByteString;
    /// low-level thread-safe PKCS#1.5 buffer Signature
    // - InputLen should be < to ModulusLen - 11 bytes for proper padding
    // - returns the encrypted signature with PKCS#1.5 padding, '' on error
    function Pkcs1Sign(Input: pointer; InputLen: integer): RawByteString;
    /// verification of a RSA binary signature with the current Public Key
    // - this method is thread-safe but blocking from several threads
    function Verify(Hash: pointer; HashAlgo: THashAlgo;
      const Signature: RawByteString): boolean; overload;
    /// verification of a RSA binary signature with the current Public Key
    // - this method is thread-safe but blocking from several threads
    // - virtual method which may be overriden e.g. in TRsaPss inherited class
    function Verify(Hash, Sig: pointer; HashAlgo: THashAlgo;
      SigLen: integer): boolean; overload; virtual;
    /// compute a RSA binary signature with the current Private Key
    // - returns the encoded signature or '' on error
    // - this method is thread-safe but blocking from several threads
    // - virtual method which may be overriden e.g. in TRsaPss inherited class
    function Sign(Hash: PHash512; HashAlgo: THashAlgo): RawByteString; virtual;
    /// encrypt a message using the given Cipher and the stored public key
    // - follow the EVP_SealInit/EVP_SealFinal encoding from OpenSSL and its
    // EVP_PKEY.RsaSeal() wrapper from mormot.lib.openssl11
    function Seal(const Message: RawByteString;
      const Cipher: RawUtf8 = 'aes-128-ctr'): RawByteString; overload;
    /// encrypt a message using the given Cipher and the stored public key
    function Seal(Cipher: TAesAbstractClass; AesBits: integer;
      const Message: RawByteString): RawByteString; overload;
    /// decrypt a message using the given Cipher and the stored private key
    // - follow the EVP_OpenInit/EVP_OpenFinal encoding from OpenSSL and its
    // EVP_PKEY.RsaOpen() wrapper from mormot.lib.openssl11
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8 = 'aes-128-ctr'): RawByteString; overload;
    /// decrypt a message using the given Cipher and the stored private key
    function Open(Cipher: TAesAbstractClass; AesBits: integer;
      const Message: RawByteString): RawByteString; overload;
    /// RSA modulus size in bytes
    property ModulusLen: integer
      read fModulusLen;
    /// RSA Public key Modulus as m = p*q
    property M: PBigInt
      read fM;
    /// RSA Public key Exponent (typically 65537)
    property E: PBigInt
      read fE;
    /// RSA key Private Exponent
    property D: PBigInt
      read fD;
    /// RSA Private key first Prime as p in m = p*q
    property P: PBigInt
      read fP;
    /// RSA Private key second Prime as q in m = p*q
    property Q: PBigInt
      read fQ;
    /// RSA Private key CRT exponent satisfying e * DP == 1 (mod (p-1))
    property DP: PBigInt
      read fDP;
    /// RSA Private key CRT exponent satisfying e * DQ == 1 (mod (q-1))
    property DQ: PBigInt
      read fDQ;
    /// RSA Private key CRT coefficient satisfying q * qInv == 1 (mod p)
    property QInv: PBigInt
      read fQInv;
  published
    /// RSA modulus size in bits
    property ModulusBits: integer
      read fModulusBits;
  end;

  /// meta-class of the RSA processing classes, mainly TRsa or TRsaPss
  // - see e.g. CKA_TO_RSA[] global constant as a potential factory
  TRsaClass = class of TRsa;

  /// RSA processing class using Probabilistic Signature Scheme (PSS) signatures
  // - the RSASSA-PSS signature scheme is more secure than RSASSA-PKCS1-v1_5
  // - PSS encoding, originally invented by Bellare and Rogaway, is randomized
  // thereby producing a different value of signature each time
  // - this implementation follows RFC 8017 specifications
  // - note: Open/Seal won't use RSAES-OAEP but regular RSAES-PKCS1-v1_5
  TRsaPss = class(TRsa)
  public
    /// verification of a RSA binary signature with the current Public Key
    // - overriden method using the RSASSA-PSS signature scheme
    // - our implementation uses the same THashAlgo for its internal encoding,
    // e.g. its MGF1 function, as recommended by RFC 8017 8.1 to prevent
    // hash function substitution
    // - this method is thread-safe but blocking from several threads
    function Verify(Hash, Sig: pointer; HashAlgo: THashAlgo;
      SigLen: integer): boolean; override;
    /// compute a RSA binary signature with the current Private Key
    // - overriden method using the RSASSA-PSS signature scheme
    // - returns the encoded signature or '' on error
    // - our implementation uses the same THashAlgo for its internal encoding
    // - this method is thread-safe but blocking from several threads
    function Sign(Hash: PHash512; HashAlgo: THashAlgo): RawByteString; override;
  end;

/// low-level computation of the ASN.1 sequence of a hash signature
// - following RSASSA-PKCS1-v1_5 signature scheme RFC 8017 #9.2 steps 1 and 2
// - as used by TRsa.Sign() method and expected by CKM_RSA_PKCS signature
function RsaSignHashToDer(Hash: PHash512; HashAlgo: THashAlgo): TAsnObject;

function ToText(res: TRsaGenerateResult): PShortString; overload;


{ *********** Registration of our RSA Engine to the TCryptAsym Factory }

const
  /// lookup to be used as convenient CKA_TO_RSA[cka].Create factory
  CKA_TO_RSA: array[TCryptKeyAlgo] of TRsaClass = (
    nil,      // ckaNone
    TRsa,     // ckaRsa
    TRsaPss,  // ckaRsaPss
    nil,      // ckaEcc256
    nil,      // ckaEcc384
    nil,      // ckaEcc512
    nil,      // ckaEcc256k
    nil);     // ckaEdDSA

type
  /// store a RSA public key in ICryptPublicKey format
  // - using our pure pascal TRsa/TRsaPss engines of this unit
  TCryptPublicKeyRsa = class(TCryptPublicKey)
  protected
    fRsa: TRsa;
    // TCryptPublicKey.Verify overloads will call this overriden method
    function VerifyDigest(Sig: pointer; Dig: THash512Rec; SigLen, DigLen: integer;
      Hash: THashAlgo): boolean; override;
  public
    /// finalize this instance
    destructor Destroy; override;
    /// unserialized the public key from most known formats
    function Load(Algorithm: TCryptKeyAlgo;
      const PublicKeySaved: RawByteString): boolean; override;
    /// as used by ICryptCert.GetKeyParams
    function GetParams(out x, y: RawByteString): boolean; override;
    /// use RSA sealing, i.e. encryption with this public key
    function Seal(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
  end;

  /// store a RSA private key in ICryptPrivateKey format
  // - using our pure pascal TRsa/TRsaPss engines of this unit
  TCryptPrivateKeyRsa = class(TCryptPrivateKey)
  protected
    fRsa: TRsa;
    // decode the RSA private key ASN.1 and check for any associated public key
    function FromDer(algo: TCryptKeyAlgo; const der: RawByteString;
      pub: TCryptPublicKey): boolean; override;
    // TCryptPrivateKey.Sign overloads will call this overriden method
    function SignDigest(const Dig: THash512Rec; DigLen: integer;
      DigAlgo: TCryptAsymAlgo): RawByteString; override;
  public
    /// finalize this instance
    destructor Destroy; override;
    /// create a new private / public key pair
    // - returns the associated public key binary in SubjectPublicKey format
    function Generate(Algorithm: TCryptAsymAlgo): RawByteString; override;
    /// return the private key as raw binary
    // - follow PKCS#8 PrivateKeyInfo encoding for RSA
    function ToDer: RawByteString; override;
    /// return the associated public key as stored in a X509 certificate
    function ToSubjectPublicKey: RawByteString; override;
    /// use EciesSeal or RSA un-sealing, i.e. decryption with this private key
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
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

function BigIntToText(const der: TCertDer): RawUtf8;
var
  b: PBigInt;
begin
  with TRsaContext.Create do
    try
      b := LoadPermanent(der);
      result := b.ToText({noclone=}true);
      b.ResetPermanentAndRelease;
    finally
      Free;
    end;
end;

function ValuesSize(bytes: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := (bytes + (HALF_BYTES - 1)) div HALF_BYTES;
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
    ERsaException.RaiseUtf8(
      'TBigInt.SetPermanent(%): RefCnt=%', [@self, RefCnt]);
  RefCnt := -1;
  result := @self;
end;

function TBigInt.ResetPermanent: PBigInt;
begin
  if RefCnt >= 0 then
    ERsaException.RaiseUtf8(
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
     (RefCnt <= 0) then
    exit; // void, alreadly released (RefCnt=0) or permanent (RefCnt=-1)
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
    ResetPermanent.Release;
end;

function TBigInt.Clone: PBigInt;
begin
  result := Owner.Allocate(Size, []);
  MoveFast(Value[0], result^.Value[0], Size * HALF_BYTES);
end;

procedure TBigInt.Save(data: PByteArray; bytes: integer; andrelease: boolean);
begin
  MoveSwap(pointer(data), pointer(Value), bytes);
  if andrelease then
    Release;
end;

function TBigInt.Save(andrelease: boolean): RawByteString;
begin
  if @self = nil then
    result := ''
  else
  begin
    FastNewRawByteString(result, Size * HALF_BYTES);
    Save(pointer(result), length(result), andrelease);
  end;
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
    {$ifdef CPUINTEL}
    while n >= _xasmaddn div HALF_BYTES do // 512/1024-bit per iteration
    begin
      v := _xasmadd(pa, pb, v);
      inc(PByte(pa), _xasmaddn);
      inc(PByte(pb), _xasmaddn);
      dec(n, _xasmaddn div HALF_BYTES);
    end;
    if n > 0 then
    {$endif CPUINTEL}
      repeat
        inc(v, PtrUInt(pa^) + pb^); // 16/32-bit per iteration
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
  if not b^.IsZero then
  begin
    n := Size;
    bs := b^.Size;
    b^.Resize(n);
    pa := pointer(Value);
    pb := pointer(b^.Value);
    v := 0;
    {$ifdef CPUINTEL}
    while n >= _xasmsubn div HALF_BYTES do // 512/1024-bit per iteration
    begin
      v := _xasmsub(pa, pb, v);
      inc(PByte(pa), _xasmsubn);
      inc(PByte(pb), _xasmsubn);
      dec(n, _xasmsubn div HALF_BYTES);
    end;
    if n > 0 then
    {$endif CPUINTEL}
      repeat // 16/32-bit per iteration
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
  end;
  b.Release;
  result := Trim;
end;

function TBigInt.IntMultiply(b: HalfUInt): PBigInt;
var
  a, r: PHalfUInt;
  v: PtrUInt;
  n: integer;
begin
  result := Owner.Allocate(Size + 1, []);
  a := pointer(Value);
  r := pointer(result^.Value);
  v := 0;
  n := Size;
  {$ifdef CPUINTEL}
  while n >= _xasmmuln div HALF_BYTES do // 256/512-bit per iteration
  begin
    v := _xasmmul(a, r, b, v);
    inc(PByte(a), _xasmmuln);
    inc(PByte(r), _xasmmuln);
    dec(n, _xasmmuln div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUINTEL}
    repeat // 16/32-bit per iteration
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
  {$ifdef CPUINTEL}
  while n >= _xasmdivn div HALF_BYTES do // 512/1024-bit per iteration
  begin
    dec(PByte(a), _xasmdivn);
    v := _xasmdiv(a, b, v);
    dec(n, _xasmdivn div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUINTEL}
    repeat // 16/32-bit per iteration
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
  {$ifdef CPUINTEL}
  while n >= _xasmmodn div HALF_BYTES do // 512/1024-bit per iteration
  begin
    dec(PByte(v), _xasmmodn);
    result := _xasmmod(v, bb, result);
    dec(n, _xasmmodn div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUINTEL}
    repeat // 16/32-bit per iteration
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
  while i <> 0 do // 16-bit or 32-bit per iteration
  begin
    dec(i);
    result := (result shl HALF_BITS) + Value[i];
    d := result div 10;  // fast multiplication by reciprocal on FPC
    Value[i] := d;
    dec(result, d * 10);
  end;
end;

function TBigInt.ModInverse(m: PBigInt): PBigInt;
var
  u1, u3, v1, v3, t1, t3: PBigInt;
  iter: integer;
begin
  // see https://www.di-mgt.com.au/euclidean.html#code-modinv
  if m.Compare(1) <= 0 then
    raise ERsaException.Create('Unexpected TBigInt.ModInverse(0,1)');
  u1 := Owner.AllocateFrom(1);
  u3 := Clone;
  v1 := Owner.AllocateFrom(0);
  v3 := m.Clone;
  iter := 0;
  while not v3.IsZero do
  begin
    inc(iter);
    t1 := u1.Add(u3.Trim.Divide(v3.Copy.Trim, bidDivide, @t3).Multiply(v1.Copy));
    u3.Release;
    u1 := v1;
    v1 := t1;
    u3 := v3;
    v3 := t3;
  end;
  if u3.Compare(1) <> 0 then
    result := Owner.AllocateFrom(0)
  else if iter and 1 = 0 then
    result := u1.Copy
  else
    result := m.Clone.Substract(u1.Copy);
  u1.Release;
  u3.Release;
  v1.Release;
  v3.Release;
  m.Release;
end;

const
  BIGINT_PRIMES_LAST: array[TBigIntSimplePrime] of integer = (
    53,                         // bspFast < 256
    302,                        // bspMost < 2000
    high(BIGINT_PRIMES_DELTA)); // bspAll  < 18000

// profiling shows that Miller-Rabin takes 150 times more than bspMost

function TBigInt.MatchKnownPrime(Extend: TBigIntSimplePrime): boolean;
var
  i, v: PtrInt;
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
  gen: PLecuyer; // a generator with a period of 2^88 is strong enough
begin
  result := false;
  // first check if not a factor of a well-known small prime
  if IsZero or
     (Iterations <= 0) or
     MatchKnownPrime(Extend) then // detect most of the composite integers
    exit;
  // validate is a prime number using Miller-Rabin iterative tests (HAC 4.24)
  bak := RefCnt;
  RefCnt := -1; // make permanent for use as modulo below
  w := Clone.IntSub(1); // w = value-1
  r := w.Clone;
  a := Owner.Allocate(Size, []);
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
            n := gen^.Next(Size);
          until n > 1;
          gen^.Fill(@a^.Value[0], n * HALF_BYTES);
          a^.Value[0] := a^.Value[0] or 1; // odd
          a^.Size := n;
          a^.Trim;
        end
        else
        begin
          if Size = 1 then
            v := gen^.Next(Value[0]) // ensure a<w
          else
            v := gen^.Next; // only lower HalfUInt is enough for a<w
          a^.Value[0] := v or 1; // odd
          a^.Size := 1;
        end;
      until (a.Compare(1) > 0) and
            (a.Compare(w) < 0);
      // search if a is composite
      a := Owner.ModPower(a, r.Copy, @self); // a = a^r mod value
      if (a.Compare(1) = 0) or
         (a.Compare(w) = 0) then
        continue; // this random is related: try outside the family
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

function FipsMinIterations(bits: integer): integer;
begin
  // ensure 2^-112 error probability - see FIPS 186-5 appendix B.3 table B.1
  if bits >= 1536  then
    result := 4
  else if bits >= 1024 shr HALF_SHR then
    result := 5
  else if bits >= 512 shr HALF_SHR then
    result := 15  // not allowed by FIPS anyway
  else
    result := 51; // never used in practice for RSA
end;

function TBigInt.FillPrime(Extend: TBigIntSimplePrime; Iterations: integer;
  EndTix: Int64): boolean;
var
  n, min: integer;
  last32: PCardinal;
begin
  // ensure it is worth searching (paranoid)
  n := Size;
  if n <= 2 then
    raise ERsaException.Create('TBigInt.FillPrime: unsupported size');
  // never wait forever - 1 min seems enough even on slow Arm (tested on RaspPi)
  if EndTix <= 0 then
    EndTix := GetTickCount64 + MilliSecsPerMin; // time on Intel is around 1 sec
  // compute number of Miller-Rabin rounds for 2^-112 error probability
  min := FipsMinIterations(n shl HALF_SHR);
  if Iterations < min then // ensure at least FIPS recommendation
    Iterations := min;
  // compute a random number following FIPS 186-4 B.3.3 steps 4.4, 5.5
  min := 16;
  last32 := @Value[n - 1 {$ifdef CPU32} - 1 {$endif}];
  // since randomness may be a weak point, consolidate several trusted sources
  // see https://ieeexplore.ieee.org/document/9014350
  FillSystemRandom(pointer(Value), n * HALF_BYTES, false); // slow but approved
  {$ifdef CPUINTEL} // claimed to be NIST SP 800-90A and FIPS 140-2 compliant
  RdRand32(pointer(Value), (n * HALF_BYTES) shr 2); // xor with HW CPU prng
  {$endif CPUINTEL}
  repeat
    // xor the original trusted sources with our CSPRNG until we get enough
    TAesPrng.Main.XorRandom(Value, n * HALF_BYTES);
    if GetBitsCount(Value^, n * HALF_BITS) < n * (HALF_BITS div 3) then
    begin
      // one CSPRNG iteration is usually enough to reach 1/3 of the bits set
      // - with our TAesPrng, it never occurred after 1,000,000,000 trials
      dec(min);
      if min = 0 then // paranoid
        raise ERsaException.Create('TBigInt.FillPrime: weak CSPRNG');
      continue;
    end;
    // should be a big enough odd number
    Value[0] := Value[0] or 1; // set lower bit to ensure it is an odd number
    if last32^ < FIPS_MIN then
      last32^ := last32^ or $b5050000; // let's grow up
    if (Value[n - 1] or (RSA_RADIX shr 1) <> 0) and // absolute big enough
       (last32^ >= FIPS_MIN) then
      break;
    raise ERsaException.Create('TBigInt.FillPrime FIPS_MIN'); // paranoid
  until false;
  // brute force search for the next prime starting at this point
  result := true; 
  repeat
    if IsPrime(Extend, Iterations) then
      exit; // we got lucky
    IntAdd(2); // incremental search of odd number - see HAC 4.51
    while last32^ < FIPS_MIN do
    begin
      // handle IntAdd overflow - paranoid but safe
      TAesPrng.Main.XorRandom(Value, n * HALF_BYTES);
      Value[0] := Value[0] or 1;
    end;
    // note 1: HAC 4.53 advices for Gordon's algorithm to generate a "strong
    //      prime", but it seems not used by mbedtls nor OpenSSL
    // note 2: mbedtls can ensure (Value-1)/2 is also a prime for DH primes, but
    //      it seems not necessary for RSA because ECM algo negates its benefits
    // note 3: our version seems compliant anyway with FIPS 186-5 appendix A+B
    //      especially because having multiple rounds of Miller-Rabin is plenty
    //      with keysize >= 2048-bit (FIPS 186-4 appendix B.3.1 item A)
    // - see https://security.stackexchange.com/a/176396/155098
    //   and https://crypto.stackexchange.com/a/15761/40200
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
        UInt32ToUtf8(Value[0], result); // word or cardinal
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
    // simple case of value < divisor
    if Compute = bidDivide then
    begin
      result := Owner.AllocateFrom(0); // div = 0, mod = value
      if Remainder <> nil then
        Remainder^ := Clone;
    end
    else
      result := Clone; // mod = value
    v.Release;
    exit;
  end
  else if v.Size = 1 then
  begin
    // division by one HalfUInt
    quo := Clone.IntDivide(v.Value[0], @halfmod); // single call
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
  // regular division per another PBigInt
  if Remainder <> nil then
    Remainder^ := nil;
  m := Size - v^.Size;
  n := v^.Size + 1;
  orgsiz := Size;
  quo := Owner.Allocate(m + 1);
  tmp := Owner.Allocate(n, []);
  v.Trim;
  d := RSA_RADIX div (PtrUInt(v^.Value[v^.Size - 1]) + 1);
  u := Clone;
  if d > 1 then
  begin
    // normalize
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

// compute dst[] := dst[] + src[] * factor using O(n2) brute force algorithm
// - on modern CPU with mul opcode in a few cycles, Karatsuba is not faster
// - call a sub-function for better code generation on oldest Delphi
procedure RawMultiply(src, dst: PHalfUInt; n: integer; factor: PtrUInt);
var
  carry: PtrUInt;
begin
  carry := 0; // initial carry value
  {$ifdef CPUINTEL}
  while n >= _xasmmuladdn div HALF_BYTES do // 256/512-bit per loop
  begin
    carry := _xasmmuladd(src, dst, factor, carry);
    inc(PByte(dst), _xasmmuladdn);
    inc(PByte(src), _xasmmuladdn);
    dec(n, _xasmmuladdn div HALF_BYTES);
  end;
  if n > 0 then
  {$endif CPUINTEL}
    repeat // 16/32-bit per iteration
      inc(carry, PtrUInt(dst^) + PtrUInt(src^) * factor);
      dst^ := carry;
      inc(dst);
      inc(src);
      carry := carry shr HALF_BITS; // carry
      dec(n);
    until n = 0;
  dst^ := carry;
end;

function TBigInt.Multiply(b: PBigInt): PBigInt;
var
  r: PBigInt;
  i: PtrInt;
begin
  r := Owner.Allocate(Size + b^.Size);
  for i := 0 to b^.Size - 1 do
    RawMultiply(pointer(Value), @r^.Value[i], Size, b^.Value[i]);
  Release;
  b.Release;
  result := r.Trim;
end;

{$ifdef USEBARRET}
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
{$endif USEBARRET}

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
    result := Add(tmp); // seldom called
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
    result := Substract(tmp); // seldom called
  end
  else
    result := @self;
end;


{ TRsaContext }

destructor TRsaContext.Destroy;
var
  b, next : PBigInt;
begin
  // wipe and free all released blocks
  b := fFreeList;
  while b <> nil do
  begin
    next := b^.fNextFree;
    FillCharFast(b^.Value^, b^.Capacity * HALF_BYTES, 0); // = WipeReleased
    FreeMem(b^.Value);
    FreeMem(b);
    b := next;
  end;
  inherited Destroy;
  if ActiveCount <> 0 then
  try
    // warns for memory leaks after memory buffers are wiped and freed
    ERsaException.RaiseUtf8('%.Destroy: memory leak - ActiveCount=%',
      [self, ActiveCount]);
  except
    // just notify the debugger, console and mormot log that it was plain wrong
    on E: Exception do
      ConsoleShowFatalException(E, {waitforkey=}false);
    // but keep the program running and any other Destroy to be called
  end;
end;

procedure TRsaContext.WipeReleased;
var
  b : PBigInt;
begin
  b := fFreeList;
  while b <> nil do
  begin
    FillCharFast(b^.Value^, b^.Capacity * HALF_BYTES, 0); // anti-forensic
    b := b^.fNextFree;
  end;
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
  result := Allocate(1, []);
  result^.Value[0] := v;
end;

function TRsaContext.AllocateFromHex(const hex: RawUtf8): PBigInt;
var
  n: cardinal;
begin
  n := length(hex) shr 1;
  result := Allocate(n div HALF_BYTES, []);
  if not HexDisplayToBin(pointer(hex), pointer(result^.Value), n) then
    Release([@result]);
end;

const
  // fair enough overallocation
  RSA_DEFAULT_ALLOCATE = RSA_DEFAULT_GENERATION_BITS shr HALF_SHR;

function TRsaContext.Allocate(n: integer; opt: TRsaAllocate): PBigint;
begin
  if self = nil then
    ERsaException.RaiseUtf8('TRsa.Allocate(%): Owner=nil', [n]);
  result := fFreeList;
  if result <> nil then
  begin
    // we can recycle a pre-allocated instance
    if result^.RefCnt <> 0 then
      ERsaException.RaiseUtf8(
        '%.Allocate(%): % RefCnt=%', [self, n, result, result^.RefCnt]);
    fFreeList := result^.fNextFree;
    dec(FreeCount);
    if n > result^.Capacity then // need a bigger buffer (dedicated Resize)
    begin
      FreeMem(result^.Value); // Resize = ReallocMem = would move pointless data
      result^.Value := nil;
    end;
  end
  else
  begin
    // we need to allocate a new buffer
    New(result);
    result^.Owner := self;
    result^.Value := nil;
  end;
  result^.Size := n;
  if result^.Value = nil then
  begin
    if raExactSize in opt then
      result^.Capacity := n // e.g. from LoadPermanent()
    else
      result^.Capacity := NextGrow(Max(RSA_DEFAULT_ALLOCATE, n)); // over-alloc
    GetMem(result^.Value, result^.Capacity * HALF_BYTES);
  end;
  result^.RefCnt := 1;
  result^.fNextFree := nil;
  if raZeroed in opt then
    FillCharFast(result^.Value[0], n * HALF_BYTES, 0); // zeroed
  inc(ActiveCount);
end;

function TRsaContext.Load(data: PByteArray; bytes: integer;
  opt: TRsaAllocate): PBigInt;
begin
  result := Allocate(ValuesSize(bytes), opt + [raZeroed]); // raZeroed needed
  MoveSwap(pointer(result.Value), pointer(data), bytes);
end;

function TRsaContext.LoadPermanent(const data: RawByteString): PBigInt;
begin
  result := Load(pointer(data), length(data), [raExactSize]);
  result.RefCnt := -1;
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
  b := AllocateFrom(1).LeftShift(k * 2);
  fMu[modulo] := b.Divide(fMod[modulo]).SetPermanent;
  b.Release;
  fBk1[modulo] := AllocateFrom(1).LeftShift(k + 1).SetPermanent; // = b(k+1)
  {$endif USEBARRET}
end;

procedure TRsaContext.ResetModulo(modulo: TRsaModulo);
begin
  fMod[modulo].ResetPermanentAndRelease;
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
  base, exponent: PBigInt;
begin
  result := AllocateFrom(1);
  exponent := exp.Clone;
  exp.Release;
  base := Reduce(b, m);
  while not exponent.IsZero do
  begin
    if exponent.IsOdd then
      result := Reduce(result.Multiply(base.Copy), m);
    exponent.ShrBits;
    base := Reduce(base.Square, m);
  end;
  base.Release;
  exponent.Release;
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
  result := false;
  if (der = '') or
     (AsnNext(pos[0], der) <> ASN1_SEQ) or
     ((version <> nil) and
      (AsnNextInt32(pos[0], der, version^) <> ASN1_INT)) then
    exit;
  result := (AsnNextRaw(pos[0], der, seq) = ASN1_SEQ) and
            (AsnNext(pos[1], seq, @oid) = ASN1_OBJID) and
            (oid = CKA_OID[ckaRsa]) and
            (AsnNextRaw(pos[0], der, str) = seqtype) and
            (AsnNext(pos[2], str) = ASN1_SEQ);
  if result and
     (version <> nil) then
    result := AsnNextInteger(pos[2], str, vt) = version^;
  for i := 0 to high(values) do
    if result then
      result := AsnNextBigInt(pos[2], str, values[i]^);
end;

function RsaSignHashToDer(Hash: PHash512; HashAlgo: THashAlgo): TAsnObject;
var
  h: RawByteString;
begin
  FastSetRawByteString(h, Hash, HASH_SIZE[HashAlgo]);
  result := Asn(ASN1_SEQ, [
              Asn(ASN1_SEQ, [
                AsnOid(pointer(ASN1_OID_HASH[HashAlgo])),
                ASN1_NULL_VALUE
              ]),
              Asn(ASN1_OCTSTR, [h])
            ]);
end;

function ToText(res: TRsaGenerateResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TRsaGenerateResult), ord(res));
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
                CkaToSeq(ckaRsa),
                Asn(ASN1_BITSTR, [
                  Asn(ASN1_SEQ, [
                    AsnBigInt(Modulus),
                    AsnBigInt(Exponent) // typically 65537
                  ])
                ])
              ]);
end;

function TRsaPublicKey.ToSubjectPublicKey: RawByteString;
begin
  if (Modulus = '') or
     (Exponent = '') then
    result := ''
  else
    result := Asn(ASN1_SEQ, [
                AsnBigInt(Modulus),
                AsnBigInt(Exponent) // typically 65537
              ]);
end;

function TRsaPublicKey.FromDer(const der: TCertDer): boolean;
var
  pos: integer;
begin
  if (Modulus <> '') or
     (Exponent <> '') then
    raise ERsaException.Create('TRsaPublicKey.FromDer over an existing key');
  // first try PKCS#1 format
  result := DerToRsa(der, ASN1_BITSTR, nil, [
              @Modulus,
              @Exponent]);
  pos := 1;
  // try a simple ASN1_SEQ with two ASN1_INT, as stored in a X509 certificate
  if not result then
    result := (AsnNext(pos, der) = ASN1_SEQ) and
              AsnNextBigInt(pos, der, Modulus) and
              AsnNextBigInt(pos, der, Exponent);
end;


{ TRsaPrivateKey }

function TRsaPrivateKey.ToDer: TCertDer;
var
  oct: RawByteString;
begin
  result := '';
  if (Modulus = '') or
     (PublicExponent = '') then
    exit;
  // PKCS#8 format (default as with openssl)
  oct := AsnSafeOct([
           Asn(Version),
           AsnBigInt(Modulus),
           AsnBigInt(PublicExponent), // typically 65537
           AsnBigInt(PrivateExponent),
           AsnBigInt(Prime1),
           AsnBigInt(Prime2),
           AsnBigInt(Exponent1),
           AsnBigInt(Exponent2),
           AsnBigInt(Coefficient)
         ]);
  result := Asn(ASN1_SEQ, [
              Asn(Version),
              CkaToSeq(ckaRsa),
              oct
            ]);
  FillZero(oct);
end;

function TRsaPrivateKey.FromDer(const der: TCertDer): boolean;
var
  pos: integer;
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
  pos := 1;
  result := (der <> '') and
            (AsnNext(pos, der) = ASN1_SEQ) and
            (AsnNextInt32(pos, der, Version) = ASN1_INT) and
            AsnNextBigInt(pos, der, Modulus) and
            AsnNextBigInt(pos, der, PublicExponent) and
            AsnNextBigInt(pos, der, PrivateExponent) and
            AsnNextBigInt(pos, der, Prime1) and
            AsnNextBigInt(pos, der, Prime2) and
            AsnNextBigInt(pos, der, Exponent1) and
            AsnNextBigInt(pos, der, Exponent2) and
            AsnNextBigInt(pos, der, Coefficient);
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

constructor TRsa.Create;
begin
  inherited Create;
  fSafe.Init;
end;

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
  fSafe.Done;
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

function TRsa.CheckPrivateKey: boolean;
var
  p1, q1, h: PBigInt;
begin
  result := false;
  // ensure the privake key primes do match the public key
  if not HasPrivateKey or
     (fP.Multiply(fQ).Compare(fM, {andrelease=}true) <> 0) or
     not fE.IsPrime then
    exit;
  // ensure Chinese Remainder Theorem constants are consistent
  if (fQ.ModInverse(fP).Compare(fQInv, true) <> 0) then
    exit;
  p1 := fP.Clone.IntSub(1);
  q1 := fQ.Clone.IntSub(1);
  result := (fD.Modulo(p1).Compare(fDP, true) = 0) and
            (fD.Modulo(q1).Compare(fDQ, true) = 0);
  h := p1.Copy.Multiply(q1.Copy).SetPermanent; // h = (p-1)*(q-1)
  result := result and
    (fE.GreatestCommonDivisor(h).Compare(1, true) = 0) and
    (fE.ModInverse(h.Divide(p1.GreatestCommonDivisor(q1))).Compare(fD, true) = 0);
  // release and wipe memory
  h.ResetPermanentAndRelease;
  p1.Release;
  q1.Release;
  WipeReleased; // anti-forensic pass
end;

function TRsa.MatchKey(RsaPublicKey: TRsa): boolean;
begin
  result := RsaPublicKey.HasPublicKey and
            HasPublicKey and
            (RsaPublicKey.E.Compare(E) = 0) and
            (RsaPublicKey.M.Compare(M) = 0);
end;

class function TRsa.GenerateNew(Bits: integer; Extend: TBigIntSimplePrime;
  Iterations, TimeOutMS: integer): TRsa;
var
  res: TRsaGenerateResult;
begin
  result := Create;
  try
    res := result.Generate(Bits, Extend, Iterations, TimeOutMS);
    case res of
      rgrSuccess:
        exit;
      rgrWeakBitsMayRetry: // retry once with new instance
        begin
          result.Free;
          result := Create;
          res := result.Generate(Bits, Extend, Iterations, TimeOutMS);
          if res = rgrSuccess then
            exit;
        end;
    end;
    // silent but explicit exception on generation failure
    ERsaException.RaiseUtf8('%.GenerateNew failed as %', [self, ToText(res)^]);
  except
    FreeAndNil(result); // error occurred during keypair generation
  end;
end;

const
  // we force exponent = 65537 - FIPS 5.4 (e)
  BIGINT_65537_BIN: RawByteString = #$01#$00#$01; // Size=2 on CPU32

// see https://www.di-mgt.com.au/rsa_alg.html as reference

function TRsa.Generate(Bits: integer; Extend: TBigIntSimplePrime;
  Iterations, TimeOutMS: integer): TRsaGenerateResult;
var
  _e, _p, _q, _d, _h, _tmp: PBigInt;
  comp: integer;
  endtix: Int64;
begin
  // ensure we can actually generate such a RSA key
  result := rgrIncorrectParams;
  if HasPublicKey or
     HasPrivateKey or
     ((Bits <> 512) and    // broken with average CPU power
      (Bits <> 1024) and   // considered weak, and rejected by browsers and NIST
      (Bits <> 2048) and   // 112-bit of security = RSA_DEFAULT_GENERATION_BITS
      (Bits <> 3072) and   // 128-bit of security (as ECC256): secure until 2030
      (Bits <> 4096) and   // not worth it
      (Bits <> 7680)) then // REALLY slow for only 192-bit of security
    exit;                  // see https://stackoverflow.com/a/589850/458259
  // setup the timeout period
  if TimeOutMS <= 0 then
    TimeOutMS := MilliSecsPerMin; // blocking 1 minute seems fair enough
  endtix := GetTickCount64 + TimeOutMS;
  // setup local variables
  fModulusBits := Bits;
  fModulusLen := Bits shr 3;
  _e := LoadPermanent(BIGINT_65537_BIN); // most common exponent = 65537
  _p := Allocate(ValuesSize(ModulusLen shr 1));
  _q := Allocate(_p.Size);
  _d := nil;
  try
    // compute two p and q random primes
    repeat
      // FIPS 186-4 B.3.1: ensure x mod e<>1 i.e. gcd(x-1,e)=1
      result := rgrTimeout;
      repeat
        if not _p.FillPrime(Extend, Iterations, endtix) then
          exit; // timed out
      until _p.Modulo(_e).Compare(1, {andrelease=}true) <> 0;
      repeat
        if not _q.FillPrime(Extend, Iterations, endtix) then
          exit;
      until _q.Modulo(_e).Compare(1, {andrelease=}true) <> 0;
      result := rgrRandomGeneratorFailure;
      comp := _p.Compare(_q);
      if comp = 0 then
        exit // random generator is clearly wrong if p=q
      else if comp < 0 then
        ExchgPointer(@_p, @_q); // ensure p>q for ChineseRemainderTheorem
      // FIPS 186-4 B.3.3 step 5.4: ensure enough bits are set in difference
      _tmp := _p.Clone.Substract(_q.Copy);
      comp := _tmp.BitCount;
      _tmp.Release;
      if comp <= (Bits shr 1) - 99 then
        continue;
      // FIPS 186-4 B.3.1 criterion 2: ensure gcd( e, (p-1)*(q-1) ) = 1
      _p.IntSub(1);
      _q.IntSub(1);
      _h := _p.Copy.Multiply(_q.Copy); // h = (p-1)*(q-1)
      if _e.GreatestCommonDivisor(_h).Compare(1, {release=}true) <> 0 then
      begin
        _h.Release;
        continue;
      end;
      // compute smallest possible d = e^-1 mod LCM(p-1,q-1)
      _d := _e.ModInverse(_h.Divide(_p.GreatestCommonDivisor(_q)));
      _h.Release;
      // FIPS 186-4 B.3.1 criterion 3: ensure enough bits in d
      comp := _d.BitCount;
      if comp > (Bits + 1) shr 1 then
        break; // enough bits
      _d.Release;
    until false;
    // setup the RSA keys parameters with ChineseRemainderTheorem constants
    fD := _d.SetPermanent;
    fDP := fD.Modulo(_p).SetPermanent; // e * DP == 1 (mod (p-1))
    fDQ := fD.Modulo(_q).SetPermanent; // e * DQ == 1 (mod (q-1))
    fP := _p.IntAdd(1).SetPermanent;
    fQ := _q.IntAdd(1).SetPermanent;
    fE := _e;
    fM := _p.Multiply(_q).SetPermanent;
    fQInv := _q.ModInverse(_p).SetPermanent; // q * qInv == 1 (mod p)
    SetModulo(fM, rmM);
    SetModulo(fP, rmP);
    SetModulo(fQ, rmQ);
    dec(Bits, fM.BitCount);
    result := rgrWeakBitsMayRetry; // not final error - see TRsa.GenerateNew()
    if Bits in [0, 1] then // allow e.g. pq=1023 for 1024-bit
      result := rgrSuccess;
  finally
    // finalize local variables if were not assigned
    _q.Release;
    _p.Release;
    if fE = nil then
      _e.ResetPermanentAndRelease;
    _d.Release;
    WipeReleased; // eventual anti-forensic pass of all temp values
  end;
end;

procedure TRsa.LoadFromPublicKeyBinary(Modulus, Exponent: pointer;
  ModulusSize, ExponentSize: PtrInt);
begin
  if not fM.IsZero then
    ERsaException.RaiseUtf8(
      '%.LoadFromPublicKey on existing data', [self]);
  if (ModulusSize < 10) or
     (ExponentSize < 2) then
    ERsaException.RaiseUtf8(
      '%.LoadFromPublicKey: unexpected ModulusSize=% ExponentSize=%',
      [self, ModulusSize, ExponentSize]);
  fModulusLen := ModulusSize;
  fM := Load(Modulus, ModulusSize, [raExactSize]).SetPermanent;
  fModulusBits := fM.BitCount;
  SetModulo(fM, rmM);
  fE := Load(Exponent, ExponentSize, [raExactSize]).SetPermanent;
end;

function TRsa.LoadFromPublicKeyDer(const Der: TCertDer): boolean;
var
  key: TRsaPublicKey;
begin
  result := key.FromDer(Der);
  if result then
    try
      LoadFromPublicKey(key);
    except
      result := false;
    end;
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
    ERsaException.RaiseUtf8('Invalid %.LoadFromPublicKeyHexa', [self]);
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
    ERsaException.RaiseUtf8('%.LoadFromPrivateKey on existing data', [self]);
  with PrivateKey do
    if (PrivateExponent = '') or
       (Prime1 = '') or
       (Prime2 = '') or
       (Exponent1 = '') or
       (Exponent2 = '') or
       (Coefficient = '') or
       (length(Modulus) < 10) or
       (length(PublicExponent) < 2) then
    ERsaException.RaiseUtf8('Incorrect %.LoadFromPrivateKey call', [self]);
  LoadFromPublicKeyBinary(
    pointer(PrivateKey.Modulus), pointer(PrivateKey.PublicExponent),
    length(PrivateKey.Modulus),  length(PrivateKey.PublicExponent));
  fD := LoadPermanent(PrivateKey.PrivateExponent);
  fP := LoadPermanent(PrivateKey.Prime1);
  fQ := LoadPermanent(PrivateKey.Prime2);
  fDP := LoadPermanent(PrivateKey.Exponent1);
  fDQ := LoadPermanent(PrivateKey.Exponent2);
  fQInv := LoadPermanent(PrivateKey.Coefficient);
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
      try
        LoadFromPrivateKey(key);
      except
        result := false;
      end;
  finally
    key.Done;
  end;
end;

function TRsa.LoadFromPrivateKeyPem(const Pem: TCertPem): boolean;
var
  der: TCertDer;
begin
  try
    der := PemToDer(Pem); // returns input if was not PEM
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
  // https://en.wikipedia.org/wiki/RSA_(cryptosystem)#Using_the_Chinese_remainder_algorithm
  CurrentModulo := rmP;
  m1 := ModPower(b.Copy, fDp, nil);
  CurrentModulo := rmQ;
  m2 := ModPower(b, fDq, nil);
  h := m1.Add(fP).Substract(m2.Copy).Multiply(fQInv);
  CurrentModulo := rmP;
  h := Reduce(h, nil);
  result := m2.Add(q.Multiply(h));
  WipeReleased; // anti-forensic measure
end;

function TRsa.Pkcs1UnPad(p: PByteArray; verify: boolean): RawByteString;
var
  count, padding: integer;
begin
  // virtual method following RSASSA-PKCS1-v1_5 padding
  result := ''; // error
  if p[0] <> 0 then
    exit; // leading zero
  count := 2;
  padding := 0;
  if verify then
  begin
    if p[1] <> 1 then
      exit; // block type 1
    while (count < fModulusLen) and
          (p[count] = $ff) do
    begin
      inc(count);
      inc(padding); // ignore FF padding
    end;
  end
  else
  begin
    if p[1] <> 2 then
      exit; // block type 2
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

function TRsa.Pkcs1Pad(p: pointer; n: integer; sign: boolean): RawByteString;
var
  padding: integer;
  i: PtrInt;
  r: PByteArray absolute result;
begin
  // virtual method following RSASSA-PKCS1-v1_5 padding
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
    RandomBytes(@r[2], padding); // Lecuyer is enough for public padding
    inc(padding, 2);
    for i := 2 to padding - 1 do
      if r[i] = 0 then
        dec(r[i]); // non zero random padding
  end;
  r[padding] := 0; // padding ends with zero
  MoveFast(p^, r[padding + 1], n);
end;

function TRsa.Pkcs1Decrypt(Input: pointer): RawByteString;
var
  enc, dec: PBigInt;
  exp: RawByteString;
begin
  result := '';
  if (Input = nil) or
     not HasPrivateKey then
    exit;
  // decrypt with the RSA Private Key
  fSafe.Lock;
  try
    enc := Load(Input, fModulusLen);
    dec := ChineseRemainderTheorem(enc);
    if dec = nil then
      exit;
    exp := dec.Save({andrelease=}true);
  finally
    fSafe.UnLock;
  end;
  // decode the result following proper padding (PKCS#1.5 with TRsa class)
  result := Pkcs1UnPad(pointer(exp), {verify=}false);
end;

function TRsa.Pkcs1Verify(Input: pointer): RawByteString;
var
  enc, dec: PBigInt;
  exp: RawByteString;
begin
  result := '';
  if (Input = nil) or
     not HasPublicKey then
    exit;
  // verify by decrypting the signature with the RSA Public Key
  fSafe.Lock;
  try
    enc := Load(Input, fModulusLen);
    CurrentModulo := rmM; // for ModPower()
    dec := ModPower(enc, fE, nil); // calls enc.Release
    if dec = nil then
      exit;
    exp := dec.Save({andrelease=}true);
  finally
    fSafe.UnLock;
  end;
  // decode the result following proper padding (PKCS#1.5 with TRsa class)
  result := Pkcs1UnPad(pointer(exp), {verify=}true);
end;

function TRsa.Pkcs1Encrypt(Input: pointer; InputLen: integer): RawByteString;
var
  dec: PBigInt;
  exp: RawByteString;
begin
  result := '';
  if (Input = nil) or
     not HasPublicKey then
    exit;
  // encode input using proper padding (PKCS#1.5 with TRsa class)
  exp := Pkcs1Pad(Input, InputLen, {sign=}false);
  // encrypt with the RSA public key
  fSafe.Lock;
  try
    dec := Load(pointer(exp), fModulusLen);
    CurrentModulo := rmM; // for ModPower()
    result := ModPower(dec, fE, nil).Save({andrelease=}true);
  finally
    fSafe.UnLock;
  end;
end;

function TRsa.Pkcs1Sign(Input: pointer; InputLen: integer): RawByteString;
var
  dec: PBigInt;
  exp: RawByteString;
begin
  result := '';
  if (Input = nil) or
     not HasPrivateKey then
    exit;
  // encode input using proper padding (PKCS#1.5 with TRsa class)
  exp := Pkcs1Pad(Input, InputLen, {sign=}true);
  // encrypt the signature with the RSA Private Key
  fSafe.Lock;
  try
    dec := Load(pointer(exp), fModulusLen);
    result := ChineseRemainderTheorem(dec).Save({andrelease=}true);
  finally
    fSafe.UnLock;
  end;
end;

function TRsa.Verify(Hash: pointer; HashAlgo: THashAlgo;
  const Signature: RawByteString): boolean;
begin
  result := Verify(Hash, pointer(Signature), HashAlgo, length(Signature));
end;

function TRsa.Verify(Hash, Sig: pointer; HashAlgo: THashAlgo;
  SigLen: integer): boolean;
var
  verif, digest, oid: RawByteString;
  p: integer;
begin
  // this virtual method implements RSASSA-PKCS1-v1_5 signature scheme
  // 1. decode the supplied value using the stored public key
  result := false;
  if SigLen <> fModulusLen then
    exit; // the signature is a RSA BigInt by definition
  verif := Pkcs1Verify(Sig);
  if verif = '' then
    exit; // invalid decrypted signature or no public key
  // 2. parse the ASN.1 sequence to extract the stored hash and its algo oid
  p := 1;
  if (AsnNext(p, verif) <> ASN1_SEQ) or  // DigestInfo
     (AsnNext(p, verif) <> ASN1_SEQ) or  // AlgorithmIdentifier
     (AsnNext(p, verif, @oid) <> ASN1_OBJID) or
     (oid <> ASN1_OID_HASH[HashAlgo]) then
    exit;
  case AsnNextRaw(p, verif, digest) of
    ASN1_NULL: // optional Algorithm Parameters
      if AsnNextRaw(p, verif, digest) <> ASN1_OCTSTR then
        exit;
    ASN1_OCTSTR:
      ;
  else
    exit;
  end;
  result := CompareBuf(digest, Hash, HASH_SIZE[HashAlgo]) = 0;
end;

function TRsa.Sign(Hash: PHash512; HashAlgo: THashAlgo): RawByteString;
var
  seq: TAsnObject;
begin
  // this virtual method implements RSASSA-PKCS1-v1_5 signature scheme
  // 1. create the ASN.1 sequence of the hash to be encoded
  seq := RsaSignHashToDer(Hash, HashAlgo);
  // 2. sign it using the stored private key
  result := Pkcs1Sign(pointer(seq), length(seq));
end;

function TRsa.Seal(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
var
  mode: TAesMode;
  bits: integer;
begin
  if AesAlgoNameDecode(pointer(Cipher), mode, bits) then
    result := Seal(TAesFast[mode], bits, Message)
  else
    result := '';
end;

function TRsa.Open(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
var
  mode: TAesMode;
  bits: integer;
begin
  if AesAlgoNameDecode(pointer(Cipher), mode, bits) then
    result := Open(TAesFast[mode], bits, Message)
  else
    result := '';
end;

type
  // extra header for IV and plain text / key size storage
  // - should match the very same record definition in EVP_PKEY.RsaSeal/RsaOpen
  // from mormot.lib.openssl11.pas, which is fully compatible with this unit
  TRsaSealHeader = packed record
    iv: TAesBlock;
    plainlen: integer;
    encryptedkeylen: word; // typically 256 bytes for RSA-2048
    // followed by the encrypted key then the encrypted message
  end;
  PRsaSealHeader = ^TRsaSealHeader;

// this code follows OpenSSL EVP_SealInit/EVP_SealFinal from crypto/evp/p_seal.c
// algorithm, so that RSA Message encoding would stay compatible
// - see also the matching python code as comment in mormot.crypt.openssl

function TRsa.Seal(Cipher: TAesAbstractClass; AesBits: integer;
  const Message: RawByteString): RawByteString;
var
  msgpos: PtrInt;
  a: TAesAbstract;
  key: THash256;
  head: TRsaSealHeader;
  enckey, encmsg: RawByteString;
begin
  result := '';
  // validate input parameters
  head.plainlen := length(Message);
  if (head.plainlen = 0) or
     (head.plainlen > 128 shl 20) or // fair limitation for in-memory encryption
     (Cipher = nil) or
     not HasPublicKey then
    exit;
  // generate the ephemeral secret key and IV within the corresponding header
  RandomBytes(@head.iv, SizeOf(head.iv)); // use Lecuyer for public random
  try
    TAesPrng.Main.FillRandom(key); // use strong CSPRNG for the private secret
    // encrypt the ephemeral secret using the current RSA public key
    enckey := Pkcs1Encrypt(@key, AesBits shr 3);
    head.encryptedkeylen := length(enckey);
    // encrypt the message
    a := Cipher.Create(key, AesBits);
    try
      a.IV := head.iv;
      encmsg := a.EncryptPkcs7(Message, {ivatbeg=}false);
    finally
      a.Free;
    end;
    // concatenate the header, encrypted key and message
    msgpos := SizeOf(head) + length(enckey);
    FastNewRawByteString(result, msgpos + length(encmsg));
    PRsaSealHeader(result)^ := head;
    MoveFast(pointer(enckey)^, PByteArray(result)[SizeOf(head)], length(enckey));
    MoveFast(pointer(encmsg)^, PByteArray(result)[msgpos], length(encmsg));
  finally
    FillZero(key); // anti-forensic
  end;
end;

function TRsa.Open(Cipher: TAesAbstractClass; AesBits: integer;
  const Message: RawByteString): RawByteString;
var
  msgpos, msglen: PtrInt;
  a: TAesAbstract;
  key: RawByteString;
  head: PRsaSealHeader absolute Message;
  input: PByteArray absolute Message;
begin
  result := '';
  // decode and validate the header
  msglen := length(Message);
  if not HasPrivateKey or
     (Cipher = nil) or
     (msglen < SizeOf(head^)) or
     (head^.plainlen <= 0) or
     (head^.plainlen > 128 shl 20) or
     (head^.encryptedkeylen <> fModulusLen) then
    exit;
  msgpos := SizeOf(head^) + head^.encryptedkeylen;
  if msglen < msgpos + head^.plainlen then
    exit; // avoid buffer overflow on malformatted/forged input
  // decrypt the ephemeral key, then the message
  key := Pkcs1Decrypt(@input[SizeOf(head^)]);
  if key <> '' then
    try
      if length(key) <> AesBits shr 3 then
        exit;
      a := Cipher.Create(pointer(key)^, AesBits);
      try
        a.IV := head^.iv;
        result := a.DecryptPkcs7Buffer(@input[msgpos], msglen - msgpos,
          {ivatbeg=}false, {raiseerror=}false);
      finally
        a.Free;
      end;
    finally
      FillZero(key); // anti-forensic
    end;
end;


{ TRsaPss }

procedure RsaPssComputeSaltedHash(mHash, Salt: pointer; HashAlgo: THashAlgo;
  HashLen: integer; out Dest: THash512Rec);
var
  hasher: TSynHasher;
  zero: Int64;
begin
  // compute Hash( (0x)00 00 00 00 00 00 00 00 || mHash || salt )
  hasher.Init(HashAlgo);
  zero := 0;
  hasher.Update(@zero, SizeOf(zero));
  hasher.Update(mHash, HashLen);
  hasher.Update(Salt, HashLen);
  hasher.Final(Dest);
end;

procedure RsaPssComputeMask(Hash, Db: PByteArray; HashAlgo: THashAlgo;
  HashLen, DbLen, Bits: integer);
var
  dbmask: RawByteString;
  hasher: TSynHasher;
begin
  dbmask := hasher.Mgf1(HashAlgo, Hash, HashLen, DbLen);
  XorMemory(Db, pointer(dbmask), DbLen);
  Bits := Bits and 7;
  if Bits <> 0 then
    Db[0] := Db[0] and ($ff shr (8 - Bits)); // reset leftmost bit
end;

function TRsaPss.Verify(Hash, Sig: pointer; HashAlgo: THashAlgo; SigLen: integer): boolean;
var
  hlen, bits, len, dblen, padding: integer;
  encoded, decoded: PBigInt;
  exp: RawByteString;
  e, h: PByteArray;
  h2: THash512Rec;
begin
  // overriden method following RSASSA-PSS padding using HashAlgo
  result := false;
  hlen := HASH_SIZE[HashAlgo];
  if (Hash = nil) or
     (fModulusLen < hlen + 6) or
     (cardinal(fModulusLen - SigLen) > 1) or
     not HasPublicKey then
    exit;
  // decrypt the signature with the RSA Public Key
  fSafe.Lock;
  try
    encoded := Load(Sig, fModulusLen);
    CurrentModulo := rmM; // for ModPower()
    decoded := ModPower(encoded, fE, nil); // calls encoded.Release
    if decoded = nil then
      exit;
    exp := decoded.Save({andrelease=}true);
  finally
    fSafe.UnLock;
  end;
  if exp = '' then
    exit;
  // RFC 8017 9.1.2 verification operation
  e := pointer(exp);
  bits := fModulusBits - 1;
  if (bits and 7) = 0 then
    if e[0] = 0 then
      inc(PByte(e)) // just ignore leading 0
    else
      exit;
  len := (bits + 7) shr 3;
  if (len < hlen + 2) or
     (e[len - 1] <> $bc) or
     (e[0] and ($ff shl (bits and 7)) <> 0) then
    exit;
  dblen := len - hlen - 1;
  h := @e[dblen];
  RsaPssComputeMask(h, e, HashAlgo, hlen, dblen, bits);
  padding := 0;
  while e[padding] = 0 do
  begin
    inc(padding);
    if padding = dblen then
      exit;
  end;
  if e[padding] <> 1 then
    exit;
  RsaPssComputeSaltedHash(Hash, {salt=}@e[padding + 1], HashAlgo, hlen, h2);
  result := CompareMem(@h2, h, hlen);
end;

function TRsaPss.Sign(Hash: PHash512; HashAlgo: THashAlgo): RawByteString;
var
  bits, len, hlen, pslen, dblen: integer;
  salt: THash512;
  encoded: TBytes;
  dec: PBigInt;
  h: THash512Rec;
begin
  // overriden method following RSASSA-PSS padding using HashAlgo
  result := '';
  hlen := HASH_SIZE[HashAlgo];
  if (Hash = nil) or
     (fModulusLen < hlen + 6) or
     not HasPrivateKey then
    exit;
  if (ModulusBits + 7) shr 3 <> ModulusLen then
    ERsaException.RaiseUtf8('%.DoPad: m=p*q is weak', [self]);
  bits := ModulusBits - 1;
  len := (bits + 7) shr 3; // could be one less than ModulusLen
  // RFC 8017 9.1.1 encoding operation with saltlen = hashlen
  RandomBytes(@salt, hlen); // Lecuyer is good enough for public salt
  RsaPssComputeSaltedHash(Hash, @salt, HashAlgo, hlen, h);
  pslen := len - (hlen * 2 + 2);
  if pslen < 0 then
    exit;
  dblen := pslen + hlen + 1;
  SetLength(encoded, len); // fills with 0
  encoded[pslen] := 1;
  MoveFast(salt, encoded[pslen + 1], hlen);
  RsaPssComputeMask(@h, pointer(encoded), HashAlgo, hlen, dblen, bits);
  MoveFast(h, encoded[dblen], hlen);
  encoded[len - 1] := $bc;
  // encrypt the signature with the RSA Private Key
  fSafe.Lock;
  try
    dec := Load(pointer(encoded), len);
    result := ChineseRemainderTheorem(dec).Save({andrelease=}true);
  finally
    fSafe.UnLock;
  end;
end;


{ *********** Registration of our RSA Engine to the TCryptAsym Factory }

type
  TCryptAsymRsa = class(TCryptAsym)
  protected
    fDefaultHasher: TCryptHasher;
    fRsaClass: TRsaClass;
  public
    constructor Create(const name: RawUtf8); overload; override;
    constructor Create(const name, hasher: RawUtf8); reintroduce; overload;
    function KeyAlgo: TCryptKeyAlgo; override;
    procedure GenerateDer(out pub, priv: RawByteString; const privpwd: RawUtf8); override;
    function Sign(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const priv: RawByteString; out sig: RawByteString;
      const privpwd: RawUtf8 = ''): boolean; override;
    function Verify(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const pub, sig: RawByteString): boolean; override;
  end;

{ TCryptAsymRsa }

constructor TCryptAsymRsa.Create(const name: RawUtf8);
begin
  case PWord(name)^ of
    ord('R') + ord('S') shl 8:
      fRsaClass := TRsa;
    ord('P') + ord('S') shl 8:
      fRsaClass := TRsaPss;
  else
    ECrypt.RaiseUtf8('%.Create: unsupported name=%', [self, name]);
  end;
  inherited Create(name);
  if fDefaultHasher = nil then
    fDefaultHasher := Hasher('sha256');
  // RSASSA and RSASSA-PSS share the very same key file format
  fPemPrivate := ord(pemRsaPrivateKey);
  fPemPublic := ord(pemRsaPublicKey);
end;

constructor TCryptAsymRsa.Create(const name, hasher: RawUtf8);
begin
  fDefaultHasher := mormot.crypt.secure.Hasher(hasher); // set before Create()
  if fDefaultHasher = nil then
    ECrypt.RaiseUtf8('%.Create: unknown hasher=%', [self, hasher]);
  Create(name);
end;

function TCryptAsymRsa.KeyAlgo: TCryptKeyAlgo;
begin
  if fRsaClass = TRsa then
    result := ckaRsa
  else
    result := ckaRsaPss;
end;

procedure TCryptAsymRsa.GenerateDer(out pub, priv: RawByteString;
  const privpwd: RawUtf8);
var
  rsa: TRsa;
begin
  if privpwd <> '' then
    ECrypt.RaiseUtf8('%.GenerateDer: unsupported privpwd', [self]);
  rsa := fRsaClass.GenerateNew;
  if rsa <> nil then
  try
    pub := rsa.SavePublicKeyDer;
    priv := rsa.SavePrivateKeyDer;
  finally
    rsa.Free;
  end;
end;

function TCryptAsymRsa.Sign(hasher: TCryptHasher; msg: pointer;
  msglen: PtrInt; const priv: RawByteString; out sig: RawByteString;
  const privpwd: RawUtf8): boolean;
var
  digest: THash512Rec;
  algo: THashAlgo;
  rsa: TRsa;
begin
  result := false;
  if hasher = nil then
    hasher := fDefaultHasher;
  if (hasher = nil) or
     (priv = '') or
     (privpwd <> '') or
     not hasher.HashAlgo(algo) then
    exit; // invalid or unsupported
  rsa := fRsaClass.Create;
  try
    if not rsa.LoadFromPrivateKeyPem(priv) then // handle PEM or DER
      exit;
    FillZero(digest.b);
    hasher.Full(msg, msglen, digest);
    sig := rsa.Sign(@digest.b, algo);
    result := sig <> '';
  finally
    rsa.Free;
    FillZero(digest.b);
  end;
end;

function TCryptAsymRsa.Verify(hasher: TCryptHasher; msg: pointer;
  msglen: PtrInt; const pub, sig: RawByteString): boolean;
var
  digest: THash512Rec;
  algo: THashAlgo;
  rsa: TRsa;
begin
  result := false;
  if hasher = nil then
    hasher := fDefaultHasher;
  if (hasher = nil) or
     (pub = '') or
     not hasher.HashAlgo(algo) then
    exit; // invalid or unsupported
  rsa := fRsaClass.Create;
  try
    if not rsa.LoadFromPublicKeyPem(pub) then // handle PEM or DER
      exit;
    FillZero(digest.b);
    hasher.Full(msg, msglen, digest);
    result := rsa.Verify(@digest, algo, sig);
  finally
    rsa.Free;
    FillZero(digest.b);
  end;
end;


{ TCryptPublicKeyRsa }

destructor TCryptPublicKeyRsa.Destroy;
begin
  inherited Destroy;
  fRsa.Free;
end;

function TCryptPublicKeyRsa.Load(Algorithm: TCryptKeyAlgo;
  const PublicKeySaved: RawByteString): boolean;
begin
  result := false;
  if (fKeyAlgo <> ckaNone) or
     (PublicKeySaved = '') then
    exit;
  case Algorithm of
    ckaRsa,
    ckaRsaPss:
      begin
        fRsa := CKA_TO_RSA[Algorithm].Create;
        if fRsa.LoadFromPublicKeyPem(PublicKeySaved) then
        begin
          fKeyAlgo := Algorithm;
          result := true;
        end
        else
          FreeAndNil(fRsa);
      end;
  else
    ERsaException.RaiseUtf8('%.Create: unsupported %',
            [self, ToText(fKeyAlgo)^]);
  end;
end;

function TCryptPublicKeyRsa.VerifyDigest(Sig: pointer; Dig: THash512Rec;
  SigLen, DigLen: integer; Hash: THashAlgo): boolean;
begin
  result := false;
  if (self <> nil) and
     (DigLen <> 0) then
    case fKeyAlgo of
      ckaRsa,
      ckaRsaPss:
        // RSA digital signature verification (thread-safe but blocking)
        result := fRsa.Verify(@Dig, Sig, Hash, SigLen);
    end;
end;

function TCryptPublicKeyRsa.GetParams(out x, y: RawByteString): boolean;
begin
  result := false;
  if self <> nil then
    case fKeyAlgo of
      ckaRsa,
      ckaRsaPss:
        begin
          // for RSA, x is set to the Exponent (e), and y to the Modulus (n)
          x := fRsa.E^.Save;
          y := fRsa.M^.Save;
          result := (x <> '') and
                    (y <> '');
        end;
    end;
end;

function TCryptPublicKeyRsa.Seal(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  result  := '';
  if self <> nil then
    case fKeyAlgo of
      ckaRsa,
      ckaRsaPss:
        result := fRsa.Seal(Cipher, Message);
    end;
end;


{ TCryptPrivateKeyRsa }

function TCryptPrivateKeyRsa.FromDer(algo: TCryptKeyAlgo;
  const der: RawByteString; pub: TCryptPublicKey): boolean;
begin
  result := false;
  if fRsa = nil then
    case algo of
      ckaRsa,
      ckaRsaPss:
        begin
          fRsa := CKA_TO_RSA[algo].Create;
          if fRsa.LoadFromPrivateKeyPem(der) and
             fRsa.CheckPrivateKey and
             ((pub = nil) or
              fRsa.MatchKey((pub as TCryptPublicKeyRsa).fRsa)) then
            result := true
          else
            FreeAndNil(fRsa);
        end;
    end;
end;

function TCryptPrivateKeyRsa.Generate(Algorithm: TCryptAsymAlgo): RawByteString;
begin
  result := '';
  if (self = nil) or
     (fKeyAlgo <> ckaNone) then
    exit;
  if Algorithm in CAA_RSA then
    if fRsa = nil then
    begin
      fKeyAlgo := CAA_CKA[Algorithm];
      fRsa := CKA_TO_RSA[fKeyAlgo].GenerateNew(RSA_DEFAULT_GENERATION_BITS);
      if fRsa = nil then
        exit;
      result := fRsa.SavePublicKey.ToSubjectPublicKey;
    end;
end;

destructor TCryptPrivateKeyRsa.Destroy;
begin
  inherited Destroy;
  fRsa.Free;
end;

function TCryptPrivateKeyRsa.ToDer: RawByteString;
begin
  if (self = nil) or
     (fRsa = nil) then
    result := ''
  else
    result := fRsa.SavePrivateKeyDer;
end;

function TCryptPrivateKeyRsa.ToSubjectPublicKey: RawByteString;
begin
  if (self = nil) or
     (fRsa = nil) then
    result := ''
  else
    result := fRsa.SavePublicKey.ToSubjectPublicKey
end;

function TCryptPrivateKeyRsa.SignDigest(const Dig: THash512Rec; DigLen: integer;
  DigAlgo: TCryptAsymAlgo): RawByteString;
begin
  result := '';
  if (CAA_CKA[DigAlgo] = fKeyAlgo) and
     (HASH_SIZE[CAA_HF[DigAlgo]] = DigLen) then
    case fKeyAlgo of
      ckaRsa,
      ckaRsaPss:
        if fRsa <> nil then
          result := fRsa.Sign(@Dig.b, CAA_HF[DigAlgo]); // thread-safe
    end;
end;

function TCryptPrivateKeyRsa.Open(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  result := '';
  if self <> nil then
    case fKeyAlgo of
      ckaRsa,
      ckaRsaPss:
        if fRsa <> nil then
          result := fRsa.Open(Cipher, Message);
    end;
end;



procedure InitializeUnit;
begin
  // register this unit methods to our high-level cryptographic catalog
  CryptAsym[caaRS256] := TCryptAsymRsa.Implements(['RS256', 'RS256-int']);
  CryptAsym[caaRS384] := TCryptAsymRsa.Create('RS384', 'sha384');
  CryptAsym[caaRS512] := TCryptAsymRsa.Create('RS512', 'sha512');
  CryptAsym[caaPS256] := TCryptAsymRsa.Implements(['PS256', 'PS256-int']);
  CryptAsym[caaPS384] := TCryptAsymRsa.Create('PS384', 'sha384');
  CryptAsym[caaPS512] := TCryptAsymRsa.Create('PS512', 'sha512');
  CryptPublicKey[ckaRsa]     := TCryptPublicKeyRsa;
  CryptPublicKey[ckaRsaPss]  := TCryptPublicKeyRsa;
  CryptPrivateKey[ckaRsa]    := TCryptPrivateKeyRsa;
  CryptPrivateKey[ckaRsaPss] := TCryptPrivateKeyRsa;
  // RS256 RS384 RS512 may be overriden by faster mormot.crypt.openssl
  // but RS256-int PS256-int will stil be available to use this unit if needed
end;

initialization
  InitializeUnit;


end.
