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
  - we use half-registers (HalfUInt) for efficient computation on most systems
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
      {$ifdef HASSAFEINLINE} inline; {$endif}
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
    function Multiply(b: PBigInt): PBigInt;
    /// partial multiplication between two Big Integer values
    // - will eventually release both self and b instances
    function MultiplyPartial(b: PBigInt; InnerPartial: PtrInt;
      OuterPartial: PtrInt): PBigInt;
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
    /// compute the modulo by an unsigned integer value
    // - returns self mod b, keeping self untouched
    function IntMod(b: HalfUInt): PtrUInt;
    /// division and modulo by 10 computation
    // - computes self := self div 10 and return self mod 10
    function IntDivMod10: PtrUInt;
    /// check if this value is divisable by a small prime
    // - detection coverage can be customized from default primes < 2000
    function MatchKnownPrime(Extend: TBigIntSimplePrime = bspMost): boolean;
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
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function Load(data: PByteArray; bytes: integer): PBigInt; overload;
    /// allocate and import a Big Integer value from a big-endian binary buffer
    function Load(const data: RawByteString): PBigInt; overload;
    /// pre-compute some of the internal constant slots for a given modulo
    procedure SetModulo(b: PBigInt; modulo: TRsaModulo);
    /// release the internal constant slots for a given modulo
    procedure ResetModulo(modulo: TRsaModulo);
    /// compute the reduction of a Big Integer value in a given modulo
    // - SetModulo() should have previously called
    // - redirect to Divide() or use the Barret algorithm
    function Reduce(b: PBigint): PBigInt;
    /// compute a modular exponentiation
    // - SetModulo() should have previously be called
    function ModPower(b, exp: PBigInt): PBigInt;
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

const
  /// 4KB table of all known prime numbers < 18,000 for TBigInt.MatchKnownPrime
  BIGINT_PRIMES: array[0 .. 258 * 8 - 1] of word = (
    2    , 3    , 5    , 7    , 11   , 13   , 17   , 19   ,
    23   , 29   , 31   , 37   , 41   , 43   , 47   , 53   ,
    59   , 61   , 67   , 71   , 73   , 79   , 83   , 89   ,
    97   , 101  , 103  , 107  , 109  , 113  , 127  , 131  ,
    137  , 139  , 149  , 151  , 157  , 163  , 167  , 173  ,
    179  , 181  , 191  , 193  , 197  , 199  , 211  , 223  ,
    227  , 229  , 233  , 239  , 241  , 251  , 257  , 263  ,
    269  , 271  , 277  , 281  , 283  , 293  , 307  , 311  ,
    313  , 317  , 331  , 337  , 347  , 349  , 353  , 359  ,
    367  , 373  , 379  , 383  , 389  , 397  , 401  , 409  ,
    419  , 421  , 431  , 433  , 439  , 443  , 449  , 457  ,
    461  , 463  , 467  , 479  , 487  , 491  , 499  , 503  ,
    509  , 521  , 523  , 541  , 547  , 557  , 563  , 569  ,
    571  , 577  , 587  , 593  , 599  , 601  , 607  , 613  ,
    617  , 619  , 631  , 641  , 643  , 647  , 653  , 659  ,
    661  , 673  , 677  , 683  , 691  , 701  , 709  , 719  ,
    727  , 733  , 739  , 743  , 751  , 757  , 761  , 769  ,
    773  , 787  , 797  , 809  , 811  , 821  , 823  , 827  ,
    829  , 839  , 853  , 857  , 859  , 863  , 877  , 881  ,
    883  , 887  , 907  , 911  , 919  , 929  , 937  , 941  ,
    947  , 953  , 967  , 971  , 977  , 983  , 991  , 997  ,
    1009 , 1013 , 1019 , 1021 , 1031 , 1033 , 1039 , 1049 ,
    1051 , 1061 , 1063 , 1069 , 1087 , 1091 , 1093 , 1097 ,
    1103 , 1109 , 1117 , 1123 , 1129 , 1151 , 1153 , 1163 ,
    1171 , 1181 , 1187 , 1193 , 1201 , 1213 , 1217 , 1223 ,
    1229 , 1231 , 1237 , 1249 , 1259 , 1277 , 1279 , 1283 ,
    1289 , 1291 , 1297 , 1301 , 1303 , 1307 , 1319 , 1321 ,
    1327 , 1361 , 1367 , 1373 , 1381 , 1399 , 1409 , 1423 ,
    1427 , 1429 , 1433 , 1439 , 1447 , 1451 , 1453 , 1459 ,
    1471 , 1481 , 1483 , 1487 , 1489 , 1493 , 1499 , 1511 ,
    1523 , 1531 , 1543 , 1549 , 1553 , 1559 , 1567 , 1571 ,
    1579 , 1583 , 1597 , 1601 , 1607 , 1609 , 1613 , 1619 ,
    1621 , 1627 , 1637 , 1657 , 1663 , 1667 , 1669 , 1693 ,
    1697 , 1699 , 1709 , 1721 , 1723 , 1733 , 1741 , 1747 ,
    1753 , 1759 , 1777 , 1783 , 1787 , 1789 , 1801 , 1811 ,
    1823 , 1831 , 1847 , 1861 , 1867 , 1871 , 1873 , 1877 ,
    1879 , 1889 , 1901 , 1907 , 1913 , 1931 , 1933 , 1949 ,
    1951 , 1973 , 1979 , 1987 , 1993 , 1997 , 1999 , 2003 ,
    2011 , 2017 , 2027 , 2029 , 2039 , 2053 , 2063 , 2069 ,
    2081 , 2083 , 2087 , 2089 , 2099 , 2111 , 2113 , 2129 ,
    2131 , 2137 , 2141 , 2143 , 2153 , 2161 , 2179 , 2203 ,
    2207 , 2213 , 2221 , 2237 , 2239 , 2243 , 2251 , 2267 ,
    2269 , 2273 , 2281 , 2287 , 2293 , 2297 , 2309 , 2311 ,
    2333 , 2339 , 2341 , 2347 , 2351 , 2357 , 2371 , 2377 ,
    2381 , 2383 , 2389 , 2393 , 2399 , 2411 , 2417 , 2423 ,
    2437 , 2441 , 2447 , 2459 , 2467 , 2473 , 2477 , 2503 ,
    2521 , 2531 , 2539 , 2543 , 2549 , 2551 , 2557 , 2579 ,
    2591 , 2593 , 2609 , 2617 , 2621 , 2633 , 2647 , 2657 ,
    2659 , 2663 , 2671 , 2677 , 2683 , 2687 , 2689 , 2693 ,
    2699 , 2707 , 2711 , 2713 , 2719 , 2729 , 2731 , 2741 ,
    2749 , 2753 , 2767 , 2777 , 2789 , 2791 , 2797 , 2801 ,
    2803 , 2819 , 2833 , 2837 , 2843 , 2851 , 2857 , 2861 ,
    2879 , 2887 , 2897 , 2903 , 2909 , 2917 , 2927 , 2939 ,
    2953 , 2957 , 2963 , 2969 , 2971 , 2999 , 3001 , 3011 ,
    3019 , 3023 , 3037 , 3041 , 3049 , 3061 , 3067 , 3079 ,
    3083 , 3089 , 3109 , 3119 , 3121 , 3137 , 3163 , 3167 ,
    3169 , 3181 , 3187 , 3191 , 3203 , 3209 , 3217 , 3221 ,
    3229 , 3251 , 3253 , 3257 , 3259 , 3271 , 3299 , 3301 ,
    3307 , 3313 , 3319 , 3323 , 3329 , 3331 , 3343 , 3347 ,
    3359 , 3361 , 3371 , 3373 , 3389 , 3391 , 3407 , 3413 ,
    3433 , 3449 , 3457 , 3461 , 3463 , 3467 , 3469 , 3491 ,
    3499 , 3511 , 3517 , 3527 , 3529 , 3533 , 3539 , 3541 ,
    3547 , 3557 , 3559 , 3571 , 3581 , 3583 , 3593 , 3607 ,
    3613 , 3617 , 3623 , 3631 , 3637 , 3643 , 3659 , 3671 ,
    3673 , 3677 , 3691 , 3697 , 3701 , 3709 , 3719 , 3727 ,
    3733 , 3739 , 3761 , 3767 , 3769 , 3779 , 3793 , 3797 ,
    3803 , 3821 , 3823 , 3833 , 3847 , 3851 , 3853 , 3863 ,
    3877 , 3881 , 3889 , 3907 , 3911 , 3917 , 3919 , 3923 ,
    3929 , 3931 , 3943 , 3947 , 3967 , 3989 , 4001 , 4003 ,
    4007 , 4013 , 4019 , 4021 , 4027 , 4049 , 4051 , 4057 ,
    4073 , 4079 , 4091 , 4093 , 4099 , 4111 , 4127 , 4129 ,
    4133 , 4139 , 4153 , 4157 , 4159 , 4177 , 4201 , 4211 ,
    4217 , 4219 , 4229 , 4231 , 4241 , 4243 , 4253 , 4259 ,
    4261 , 4271 , 4273 , 4283 , 4289 , 4297 , 4327 , 4337 ,
    4339 , 4349 , 4357 , 4363 , 4373 , 4391 , 4397 , 4409 ,
    4421 , 4423 , 4441 , 4447 , 4451 , 4457 , 4463 , 4481 ,
    4483 , 4493 , 4507 , 4513 , 4517 , 4519 , 4523 , 4547 ,
    4549 , 4561 , 4567 , 4583 , 4591 , 4597 , 4603 , 4621 ,
    4637 , 4639 , 4643 , 4649 , 4651 , 4657 , 4663 , 4673 ,
    4679 , 4691 , 4703 , 4721 , 4723 , 4729 , 4733 , 4751 ,
    4759 , 4783 , 4787 , 4789 , 4793 , 4799 , 4801 , 4813 ,
    4817 , 4831 , 4861 , 4871 , 4877 , 4889 , 4903 , 4909 ,
    4919 , 4931 , 4933 , 4937 , 4943 , 4951 , 4957 , 4967 ,
    4969 , 4973 , 4987 , 4993 , 4999 , 5003 , 5009 , 5011 ,
    5021 , 5023 , 5039 , 5051 , 5059 , 5077 , 5081 , 5087 ,
    5099 , 5101 , 5107 , 5113 , 5119 , 5147 , 5153 , 5167 ,
    5171 , 5179 , 5189 , 5197 , 5209 , 5227 , 5231 , 5233 ,
    5237 , 5261 , 5273 , 5279 , 5281 , 5297 , 5303 , 5309 ,
    5323 , 5333 , 5347 , 5351 , 5381 , 5387 , 5393 , 5399 ,
    5407 , 5413 , 5417 , 5419 , 5431 , 5437 , 5441 , 5443 ,
    5449 , 5471 , 5477 , 5479 , 5483 , 5501 , 5503 , 5507 ,
    5519 , 5521 , 5527 , 5531 , 5557 , 5563 , 5569 , 5573 ,
    5581 , 5591 , 5623 , 5639 , 5641 , 5647 , 5651 , 5653 ,
    5657 , 5659 , 5669 , 5683 , 5689 , 5693 , 5701 , 5711 ,
    5717 , 5737 , 5741 , 5743 , 5749 , 5779 , 5783 , 5791 ,
    5801 , 5807 , 5813 , 5821 , 5827 , 5839 , 5843 , 5849 ,
    5851 , 5857 , 5861 , 5867 , 5869 , 5879 , 5881 , 5897 ,
    5903 , 5923 , 5927 , 5939 , 5953 , 5981 , 5987 , 6007 ,
    6011 , 6029 , 6037 , 6043 , 6047 , 6053 , 6067 , 6073 ,
    6079 , 6089 , 6091 , 6101 , 6113 , 6121 , 6131 , 6133 ,
    6143 , 6151 , 6163 , 6173 , 6197 , 6199 , 6203 , 6211 ,
    6217 , 6221 , 6229 , 6247 , 6257 , 6263 , 6269 , 6271 ,
    6277 , 6287 , 6299 , 6301 , 6311 , 6317 , 6323 , 6329 ,
    6337 , 6343 , 6353 , 6359 , 6361 , 6367 , 6373 , 6379 ,
    6389 , 6397 , 6421 , 6427 , 6449 , 6451 , 6469 , 6473 ,
    6481 , 6491 , 6521 , 6529 , 6547 , 6551 , 6553 , 6563 ,
    6569 , 6571 , 6577 , 6581 , 6599 , 6607 , 6619 , 6637 ,
    6653 , 6659 , 6661 , 6673 , 6679 , 6689 , 6691 , 6701 ,
    6703 , 6709 , 6719 , 6733 , 6737 , 6761 , 6763 , 6779 ,
    6781 , 6791 , 6793 , 6803 , 6823 , 6827 , 6829 , 6833 ,
    6841 , 6857 , 6863 , 6869 , 6871 , 6883 , 6899 , 6907 ,
    6911 , 6917 , 6947 , 6949 , 6959 , 6961 , 6967 , 6971 ,
    6977 , 6983 , 6991 , 6997 , 7001 , 7013 , 7019 , 7027 ,
    7039 , 7043 , 7057 , 7069 , 7079 , 7103 , 7109 , 7121 ,
    7127 , 7129 , 7151 , 7159 , 7177 , 7187 , 7193 , 7207 ,
    7211 , 7213 , 7219 , 7229 , 7237 , 7243 , 7247 , 7253 ,
    7283 , 7297 , 7307 , 7309 , 7321 , 7331 , 7333 , 7349 ,
    7351 , 7369 , 7393 , 7411 , 7417 , 7433 , 7451 , 7457 ,
    7459 , 7477 , 7481 , 7487 , 7489 , 7499 , 7507 , 7517 ,
    7523 , 7529 , 7537 , 7541 , 7547 , 7549 , 7559 , 7561 ,
    7573 , 7577 , 7583 , 7589 , 7591 , 7603 , 7607 , 7621 ,
    7639 , 7643 , 7649 , 7669 , 7673 , 7681 , 7687 , 7691 ,
    7699 , 7703 , 7717 , 7723 , 7727 , 7741 , 7753 , 7757 ,
    7759 , 7789 , 7793 , 7817 , 7823 , 7829 , 7841 , 7853 ,
    7867 , 7873 , 7877 , 7879 , 7883 , 7901 , 7907 , 7919 ,
    7927 , 7933 , 7937 , 7949 , 7951 , 7963 , 7993 , 8009 ,
    8011 , 8017 , 8039 , 8053 , 8059 , 8069 , 8081 , 8087 ,
    8089 , 8093 , 8101 , 8111 , 8117 , 8123 , 8147 , 8161 ,
    8167 , 8171 , 8179 , 8191 , 8209 , 8219 , 8221 , 8231 ,
    8233 , 8237 , 8243 , 8263 , 8269 , 8273 , 8287 , 8291 ,
    8293 , 8297 , 8311 , 8317 , 8329 , 8353 , 8363 , 8369 ,
    8377 , 8387 , 8389 , 8419 , 8423 , 8429 , 8431 , 8443 ,
    8447 , 8461 , 8467 , 8501 , 8513 , 8521 , 8527 , 8537 ,
    8539 , 8543 , 8563 , 8573 , 8581 , 8597 , 8599 , 8609 ,
    8623 , 8627 , 8629 , 8641 , 8647 , 8663 , 8669 , 8677 ,
    8681 , 8689 , 8693 , 8699 , 8707 , 8713 , 8719 , 8731 ,
    8737 , 8741 , 8747 , 8753 , 8761 , 8779 , 8783 , 8803 ,
    8807 , 8819 , 8821 , 8831 , 8837 , 8839 , 8849 , 8861 ,
    8863 , 8867 , 8887 , 8893 , 8923 , 8929 , 8933 , 8941 ,
    8951 , 8963 , 8969 , 8971 , 8999 , 9001 , 9007 , 9011 ,
    9013 , 9029 , 9041 , 9043 , 9049 , 9059 , 9067 , 9091 ,
    9103 , 9109 , 9127 , 9133 , 9137 , 9151 , 9157 , 9161 ,
    9173 , 9181 , 9187 , 9199 , 9203 , 9209 , 9221 , 9227 ,
    9239 , 9241 , 9257 , 9277 , 9281 , 9283 , 9293 , 9311 ,
    9319 , 9323 , 9337 , 9341 , 9343 , 9349 , 9371 , 9377 ,
    9391 , 9397 , 9403 , 9413 , 9419 , 9421 , 9431 , 9433 ,
    9437 , 9439 , 9461 , 9463 , 9467 , 9473 , 9479 , 9491 ,
    9497 , 9511 , 9521 , 9533 , 9539 , 9547 , 9551 , 9587 ,
    9601 , 9613 , 9619 , 9623 , 9629 , 9631 , 9643 , 9649 ,
    9661 , 9677 , 9679 , 9689 , 9697 , 9719 , 9721 , 9733 ,
    9739 , 9743 , 9749 , 9767 , 9769 , 9781 , 9787 , 9791 ,
    9803 , 9811 , 9817 , 9829 , 9833 , 9839 , 9851 , 9857 ,
    9859 , 9871 , 9883 , 9887 , 9901 , 9907 , 9923 , 9929 ,
    9931 , 9941 , 9949 , 9967 , 9973 , 10007, 10009, 10037,
    10039, 10061, 10067, 10069, 10079, 10091, 10093, 10099,
    10103, 10111, 10133, 10139, 10141, 10151, 10159, 10163,
    10169, 10177, 10181, 10193, 10211, 10223, 10243, 10247,
    10253, 10259, 10267, 10271, 10273, 10289, 10301, 10303,
    10313, 10321, 10331, 10333, 10337, 10343, 10357, 10369,
    10391, 10399, 10427, 10429, 10433, 10453, 10457, 10459,
    10463, 10477, 10487, 10499, 10501, 10513, 10529, 10531,
    10559, 10567, 10589, 10597, 10601, 10607, 10613, 10627,
    10631, 10639, 10651, 10657, 10663, 10667, 10687, 10691,
    10709, 10711, 10723, 10729, 10733, 10739, 10753, 10771,
    10781, 10789, 10799, 10831, 10837, 10847, 10853, 10859,
    10861, 10867, 10883, 10889, 10891, 10903, 10909, 10937,
    10939, 10949, 10957, 10973, 10979, 10987, 10993, 11003,
    11027, 11047, 11057, 11059, 11069, 11071, 11083, 11087,
    11093, 11113, 11117, 11119, 11131, 11149, 11159, 11161,
    11171, 11173, 11177, 11197, 11213, 11239, 11243, 11251,
    11257, 11261, 11273, 11279, 11287, 11299, 11311, 11317,
    11321, 11329, 11351, 11353, 11369, 11383, 11393, 11399,
    11411, 11423, 11437, 11443, 11447, 11467, 11471, 11483,
    11489, 11491, 11497, 11503, 11519, 11527, 11549, 11551,
    11579, 11587, 11593, 11597, 11617, 11621, 11633, 11657,
    11677, 11681, 11689, 11699, 11701, 11717, 11719, 11731,
    11743, 11777, 11779, 11783, 11789, 11801, 11807, 11813,
    11821, 11827, 11831, 11833, 11839, 11863, 11867, 11887,
    11897, 11903, 11909, 11923, 11927, 11933, 11939, 11941,
    11953, 11959, 11969, 11971, 11981, 11987, 12007, 12011,
    12037, 12041, 12043, 12049, 12071, 12073, 12097, 12101,
    12107, 12109, 12113, 12119, 12143, 12149, 12157, 12161,
    12163, 12197, 12203, 12211, 12227, 12239, 12241, 12251,
    12253, 12263, 12269, 12277, 12281, 12289, 12301, 12323,
    12329, 12343, 12347, 12373, 12377, 12379, 12391, 12401,
    12409, 12413, 12421, 12433, 12437, 12451, 12457, 12473,
    12479, 12487, 12491, 12497, 12503, 12511, 12517, 12527,
    12539, 12541, 12547, 12553, 12569, 12577, 12583, 12589,
    12601, 12611, 12613, 12619, 12637, 12641, 12647, 12653,
    12659, 12671, 12689, 12697, 12703, 12713, 12721, 12739,
    12743, 12757, 12763, 12781, 12791, 12799, 12809, 12821,
    12823, 12829, 12841, 12853, 12889, 12893, 12899, 12907,
    12911, 12917, 12919, 12923, 12941, 12953, 12959, 12967,
    12973, 12979, 12983, 13001, 13003, 13007, 13009, 13033,
    13037, 13043, 13049, 13063, 13093, 13099, 13103, 13109,
    13121, 13127, 13147, 13151, 13159, 13163, 13171, 13177,
    13183, 13187, 13217, 13219, 13229, 13241, 13249, 13259,
    13267, 13291, 13297, 13309, 13313, 13327, 13331, 13337,
    13339, 13367, 13381, 13397, 13399, 13411, 13417, 13421,
    13441, 13451, 13457, 13463, 13469, 13477, 13487, 13499,
    13513, 13523, 13537, 13553, 13567, 13577, 13591, 13597,
    13613, 13619, 13627, 13633, 13649, 13669, 13679, 13681,
    13687, 13691, 13693, 13697, 13709, 13711, 13721, 13723,
    13729, 13751, 13757, 13759, 13763, 13781, 13789, 13799,
    13807, 13829, 13831, 13841, 13859, 13873, 13877, 13879,
    13883, 13901, 13903, 13907, 13913, 13921, 13931, 13933,
    13963, 13967, 13997, 13999, 14009, 14011, 14029, 14033,
    14051, 14057, 14071, 14081, 14083, 14087, 14107, 14143,
    14149, 14153, 14159, 14173, 14177, 14197, 14207, 14221,
    14243, 14249, 14251, 14281, 14293, 14303, 14321, 14323,
    14327, 14341, 14347, 14369, 14387, 14389, 14401, 14407,
    14411, 14419, 14423, 14431, 14437, 14447, 14449, 14461,
    14479, 14489, 14503, 14519, 14533, 14537, 14543, 14549,
    14551, 14557, 14561, 14563, 14591, 14593, 14621, 14627,
    14629, 14633, 14639, 14653, 14657, 14669, 14683, 14699,
    14713, 14717, 14723, 14731, 14737, 14741, 14747, 14753,
    14759, 14767, 14771, 14779, 14783, 14797, 14813, 14821,
    14827, 14831, 14843, 14851, 14867, 14869, 14879, 14887,
    14891, 14897, 14923, 14929, 14939, 14947, 14951, 14957,
    14969, 14983, 15013, 15017, 15031, 15053, 15061, 15073,
    15077, 15083, 15091, 15101, 15107, 15121, 15131, 15137,
    15139, 15149, 15161, 15173, 15187, 15193, 15199, 15217,
    15227, 15233, 15241, 15259, 15263, 15269, 15271, 15277,
    15287, 15289, 15299, 15307, 15313, 15319, 15329, 15331,
    15349, 15359, 15361, 15373, 15377, 15383, 15391, 15401,
    15413, 15427, 15439, 15443, 15451, 15461, 15467, 15473,
    15493, 15497, 15511, 15527, 15541, 15551, 15559, 15569,
    15581, 15583, 15601, 15607, 15619, 15629, 15641, 15643,
    15647, 15649, 15661, 15667, 15671, 15679, 15683, 15727,
    15731, 15733, 15737, 15739, 15749, 15761, 15767, 15773,
    15787, 15791, 15797, 15803, 15809, 15817, 15823, 15859,
    15877, 15881, 15887, 15889, 15901, 15907, 15913, 15919,
    15923, 15937, 15959, 15971, 15973, 15991, 16001, 16007,
    16033, 16057, 16061, 16063, 16067, 16069, 16073, 16087,
    16091, 16097, 16103, 16111, 16127, 16139, 16141, 16183,
    16187, 16189, 16193, 16217, 16223, 16229, 16231, 16249,
    16253, 16267, 16273, 16301, 16319, 16333, 16339, 16349,
    16361, 16363, 16369, 16381, 16411, 16417, 16421, 16427,
    16433, 16447, 16451, 16453, 16477, 16481, 16487, 16493,
    16519, 16529, 16547, 16553, 16561, 16567, 16573, 16603,
    16607, 16619, 16631, 16633, 16649, 16651, 16657, 16661,
    16673, 16691, 16693, 16699, 16703, 16729, 16741, 16747,
    16759, 16763, 16787, 16811, 16823, 16829, 16831, 16843,
    16871, 16879, 16883, 16889, 16901, 16903, 16921, 16927,
    16931, 16937, 16943, 16963, 16979, 16981, 16987, 16993,
    17011, 17021, 17027, 17029, 17033, 17041, 17047, 17053,
    17077, 17093, 17099, 17107, 17117, 17123, 17137, 17159,
    17167, 17183, 17189, 17191, 17203, 17207, 17209, 17231,
    17239, 17257, 17291, 17293, 17299, 17317, 17321, 17327,
    17333, 17341, 17351, 17359, 17377, 17383, 17387, 17389,
    17393, 17401, 17417, 17419, 17431, 17443, 17449, 17467,
    17471, 17477, 17483, 17489, 17491, 17497, 17509, 17519,
    17539, 17551, 17569, 17573, 17579, 17581, 17597, 17599,
    17609, 17623, 17627, 17657, 17659, 17669, 17681, 17683,
    17707, 17713, 17729, 17737, 17747, 17749, 17761, 17783,
    17789, 17791, 17807, 17827, 17837, 17839, 17851, 17863,
    17881, 17891, 17903, 17909, 17911, 17921, 17923, 17929,
    17939, 17957, 17959, 17971, 17977, 17981, 17987, 17989);

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
    /// check if all fields are set, i.e. if a private key has been loaded
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
  if result = 0 then
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
  b.Release;
  result := Trim;
end;

function TBigInt.IntMultiply(b: HalfUInt): PBigInt;
var
  a, r: PHalfUInt;
  v: PtrUInt;
  n: integer;
begin
  result := Owner.Allocate(Size + 1, true);
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

function TBigInt.IntDivide(b: HalfUInt; modulo: PHalfUInt): PBigInt;
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
      v := (v shl HALF_BITS) + a^; // carry
      d := v div b;
      a^ := d;
      dec(v, d * b); // fast v := v mod b
      dec(n);
    until n = 0;
  if modulo <> nil then
    modulo^ := v;
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
    53,                   // bspFast < 256
    302,                  // bspMost < 2000
    high(BIGINT_PRIMES)); // bspAll  < 18000

function TBigInt.MatchKnownPrime(Extend: TBigIntSimplePrime): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to BIGINT_PRIMES_LAST[Extend] do
    if IntMod(BIGINT_PRIMES[i]) = 0 then
      exit;
  result := false;
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
        p^ := v.IntDivMod10 + ord('0'); // fast enough (used for display only)
      until (v.Size = 0) or
            (p = @tmp); // truncate after 8190 digits
      v.Release;
      FastSetString(result, p, PAnsiChar(@tmp[high(tmp)]) - pointer(p));
    end;
  end;
end;

function TBigInt.Divide(v: PBigInt; ComputeMod: boolean): PBigInt;
var
  d, inner, dash: HalfUInt;
  lastt, lastt2, lastv, lastv2: PtrUInt;
  neg: boolean;
  j, m, n, orgsiz: integer;
  p: PHalfUInt;
  u, quo, tmp: PBigInt;
begin
  if ComputeMod and
     (Compare(v) < 0) then
  begin
    v.Release;
    result := Copy; // just return self if self < v
    exit;
  end;
  m := Size - v^.Size;
  n := v^.Size + 1;
  orgsiz := Size;
  quo := Owner.Allocate(m + 1);
  tmp := Owner.Allocate(n, true);
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
          if (v^.Size > 2) and
             (PtrUInt(lastv2 * dash) >
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
  fMod[modulo].ResetPermanentAndRelease;
  fNormMod[modulo].ResetPermanentAndRelease;
  {$ifdef USEBARRET}
  fMu[modulo].ResetPermanentAndRelease;
  fBk1[modulo].ResetPermanentAndRelease;
  {$endif USEBARRET}
end;

{$ifdef USEBARRET}
function TRsaContext.Reduce(b: PBigint): PBigInt;
var
  q1, q2, q3, r1, r2, m: PBigInt;
  k: integer;
begin
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
    result := b^.Divide(m, {mod=}true);
    b^.Release;
    exit;
  end;
  // q1 = [x / b**(k-1)]
  q1 := b^.Clone.RightShift(k - 1);
  //writeln(#10'q1=',q1.ToHexa);
  //writeln(#10'mu=',fMu[CurrentModulo].ToHexa);
  // Do outer partial multiply
  // q2 = q1 * mu
  q2 := q1.Multiply(fMu[CurrentModulo], 0, k - 1);
  //writeln(#10'q2=',q2.tohexa);
  // q3 = [q2 / b**(k+1)]
  q3 := q2.RightShift(k + 1);
  //writeln(#10'q3=',q3.tohexa);
  // r1 = x mod b**(k+1)
  r1 := b^.TruncateMod(k + 1);
  //writeln(#10'r1=',r1.tohexa);
  // Do inner partial multiply
  // r2 = q3 * m mod b**(k+1)
  r2 := q3.Multiply(m, k + 1, 0).TruncateMod(k + 1);
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
function TRsaContext.Reduce(b: PBigint): PBigInt;
begin
  result := b^.Divide(fMod[CurrentModulo], {mod=}true);
  b^.Release;
end;
{$endif USEBARRET}

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
  g2 := Reduce(g[0].Square); // g2 := residue of g^2
  for j := 1 to k - 1 do
    g[j] := Reduce(g[j - 1].Multiply(g2.Copy)).SetPermanent;
  g2.Release;
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
        r := Reduce(r.Square);
        if exp.BitIsSet(j) then
          inc(partial);
        if j <> l then
          partial := partial shl 1;
        dec(j);
      end;
      partial := (partial - 1) shr 1; // Adjust for array
      r := Reduce(r.Multiply(g[partial]));
      i := l - 1;
    end
    else
    begin
      // bit not set: just process the next bit
      r := Reduce(r.Square);
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


{ TRsa }

destructor TRsa.Destroy;
begin
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
  result := key.FromDer(Der);
  if result then
    LoadFromPrivateKey(key);
end;

function TRsa.LoadFromPrivateKeyPem(const Pem: TCertPem): boolean;
begin
  result := LoadFromPrivateKeyDer(PemToDer(Pem));
end;

function TRsa.ChineseRemainderTheorem(b: PBigInt): PBigInt;
var
  h, m1, m2: PBigInt;
begin
  result := nil;
  if not HasPrivateKey then
    exit;
  CurrentModulo := rmP;
  m1 := ModPower(b.Copy, fDp);
  CurrentModulo := rmQ;
  m2 := ModPower(b, fDq);
  h := m1.Add(fP).Substract(m2.Copy).Multiply(fQInv);
  CurrentModulo := rmP;
  h := Reduce(h);
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
    dec := ModPower(enc, fE); // calls enc.Release
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
    enc := ModPower(dec, fE); // calls dec.Release
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
