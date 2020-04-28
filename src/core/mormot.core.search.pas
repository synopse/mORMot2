/// Framework Core Text Search Engines
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.search;

{
  *****************************************************************************

   Several Indexing and Search Engines, as used by other parts of the framework
    - GLOB and SOUNDEX Text Search
    - Versatile Expression Search Engine
    - Bloom Filter Probabilistic Index

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
  mormot.core.data;


{ ****************** GLOB and SOUNDEX Text Search }

type
  PMatch = ^TMatch;

  // used when inlining TMatch.Match
  TMatchSearchFunction = function(aMatch: PMatch;
    aText: PUTF8Char; aTextLen: PtrInt): boolean;

  /// low-level structure used by IsMatch() for actual GLOB search
  // - you can use this object to prepare a given pattern, e.g. in a loop
  // - implemented as a fast brute-force state-machine without any heap allocation
  // - some common patterns ('exactmatch', 'startwith*', '*endwith', '*contained*')
  // are handled with dedicated code, optionally with case-insensitive search
  // - consider using TMatchs (or SetMatchs/TMatchDynArray) if you expect to
  // search for several patterns, or even TExprParserMatch for expression search
  TMatch = object
  private
    Pattern, Text: PUTF8Char;
    P, T, PMax, TMax: PtrInt;
    Upper: PNormTable;
    State: (sNONE, sABORT, sEND, sLITERAL, sPATTERN, sRANGE, sVALID);
    procedure MatchAfterStar;
    procedure MatchMain;
  public
    /// published for proper inlining
    Search: TMatchSearchFunction;
    /// initialize the internal fields for a given glob search pattern
    // - note that the aPattern instance should remain in memory, since it will
    // be pointed to by the Pattern private field of this object
    procedure Prepare(const aPattern: RawUTF8;
      aCaseInsensitive, aReuse: boolean); overload;
    /// initialize the internal fields for a given glob search pattern
    // - note that the aPattern buffer should remain in memory, since it will
    // be pointed to by the Pattern private field of this object
    procedure Prepare(aPattern: PUTF8Char; aPatternLen: integer;
      aCaseInsensitive, aReuse: boolean); overload;
    /// initialize low-level internal fields for'*aPattern*' search
    // - this method is faster than a regular Prepare('*' + aPattern + '*'),
    // since it may use the SBNDMQ2 algorithm for patterns of length 2..31
    // - warning: the supplied aPattern variable may be modified in-place to be
    // filled with some lookup buffer, when SBNDMQ2 is triggered
    procedure PrepareContains(var aPattern: RawUTF8;
      aCaseInsensitive: boolean); overload;
    /// initialize low-level internal fields for a custom search algorithm
    procedure PrepareRaw(aPattern: PUTF8Char; aPatternLen: integer;
      aSearch: TMatchSearchFunction);
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(const aText: RawUTF8): boolean; overload;
      {$ifdef FPC} inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(aText: PUTF8Char; aTextLen: PtrInt): boolean; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method IS thread-safe, and won't lock
    function MatchThreadSafe(const aText: RawUTF8): boolean;
    /// returns TRUE if the supplied VCL/LCL content matches the prepared glob pattern
    // - this method IS thread-safe, will use on-stack UTF-8 temporary conversion
    // if possible, and won't lock
    function MatchString(const aText: string): boolean;
    /// returns TRUE if this search pattern matches another
    function Equals(const aAnother: TMatch): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern length as stored in PMax + 1
    function PatternLength: integer; {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern text as stored in Pattern
    function PatternText: PUTF8Char; {$ifdef HASINLINE}inline;{$endif}
  end;

  /// stores an array of GLOB search engines
  // - use SetMatchs() to initialize such an array from a CSV pattern text
  TMatchDynArray = array of TMatch;

  /// TMatch descendant owning a copy of the Pattern string to avoid GPF issues
  TMatchStore = record
    /// access to the research criteria
    // - defined as a nested record (and not an object) to circumvent Delphi bug
    Pattern: TMatch;
    /// Pattern.Pattern PUTF8Char will point to this instance
    PatternInstance: RawUTF8;
  end;

  TMatchStoreDynArray = array of TMatchStore;

  /// stores several TMatch instances, from a set of glob patterns
  TMatchs = class(TSynPersistent)
  protected
    fMatch: TMatchStoreDynArray;
    fMatchCount: integer;
  public
    /// add once some glob patterns to the internal TMach list
    // - aPatterns[] follows the IsMatch() syntax
    constructor Create(const aPatterns: TRawUTF8DynArray;
      CaseInsensitive: Boolean); reintroduce; overload;
    /// add once some glob patterns to the internal TMach list
    // - aPatterns[] follows the IsMatch() syntax
    procedure Subscribe(const aPatterns: TRawUTF8DynArray;
      CaseInsensitive: Boolean); overload; virtual;
    /// add once some glob patterns to the internal TMach list
    // - each CSV item in aPatterns follows the IsMatch() syntax
    procedure Subscribe(const aPatternsCSV: RawUTF8;
      CaseInsensitive: Boolean); overload;
    /// search patterns in the supplied UTF-8 text
    // - returns -1 if no filter has been subscribed
    // - returns -2 if there is no match on any previous pattern subscription
    // - returns fMatch[] index, i.e. >= 0 number on first matching pattern
    // - this method is thread-safe
    function Match(const aText: RawUTF8): integer; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// search patterns in the supplied UTF-8 text buffer
    function Match(aText: PUTF8Char; aLen: integer): integer; overload;
    /// search patterns in the supplied VCL/LCL text
    // - could be used on a TFileName for instance
    // - will avoid any memory allocation if aText is small enough
    function MatchString(const aText: string): integer;
  end;


/// fill the Match[] dynamic array with all glob patterns supplied as CSV
// - returns how many patterns have been set in Match[|]
// - note that the CSVPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  out Match: TMatchDynArray): integer; overload;

/// fill the Match[0..MatchMax] static array with all glob patterns supplied as CSV
// - note that the CSVPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(CSVPattern: PUTF8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer): integer; overload;

/// search if one TMach is already registered in the Several[] dynamic array
function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;

/// add one TMach if not already registered in the Several[] dynamic array
function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;

/// returns TRUE if Match=nil or if any Match[].Match(Text) is TRUE
function MatchAny(const Match: TMatchDynArray; const Text: RawUTF8): boolean;

/// apply the CSV-supplied glob patterns to an array of RawUTF8
// - any text not maching the pattern will be deleted from the array
procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TRawUTF8DynArray); overload;

/// apply the CSV-supplied glob patterns to an array of string
// - any text not maching the pattern will be deleted from the array
procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TStringDynArray); overload;

/// return TRUE if the supplied content matchs a glob pattern
// - ?  Matches any single characer
// - *	Matches any contiguous characters
// - [abc]  Matches a or b or c at that position
// - [^abc]	Matches anything but a or b or c at that position
// - [!abc]	Matches anything but a or b or c at that position
// - [a-e]  Matches a through e at that position
// - [abcx-z]  Matches a or b or c or x or y or or z, as does [a-cx-z]
// - 'ma?ch.*'	would match match.exe, mavch.dat, march.on, etc..
// - 'this [e-n]s a [!zy]est' would match 'this is a test', but would not
// match 'this as a test' nor 'this is a zest'
// - consider using TMatch or TMatchs if you expect to reuse the pattern
function IsMatch(const Pattern, Text: RawUTF8;
  CaseInsensitive: boolean = false): boolean;

/// return TRUE if the supplied content matchs a glob pattern, using VCL strings
// - is a wrapper around IsMatch() with fast UTF-8 conversion
function IsMatchString(const Pattern, Text: string;
  CaseInsensitive: boolean = false): boolean;


type
  /// available pronunciations for our fast Soundex implementation
  TSynSoundExPronunciation = (
    sndxEnglish, sndxFrench, sndxSpanish, sndxNone);

  TSoundExValues = array[0..ord('Z') - ord('B')] of byte;

  PSoundExValues = ^TSoundExValues;
  PSynSoundEx = ^TSynSoundEx;

  /// fast search of a text value, using the Soundex approximation mechanism
  // - Soundex is a phonetic algorithm for indexing names by sound,
  //  as pronounced in a given language. The goal is for homophones to be
  //  encoded to the same representation so that they can be matched despite
  //  minor differences in spelling
  // - this implementation is very fast and can be used e.g. to parse and search
  //  in a huge text buffer
  // - this version also handles french and spanish pronunciations on request,
  //  which differs from default Soundex, i.e. English
  TSynSoundEx = object
  protected
    Search, FirstChar: cardinal;
    fValues: PSoundExValues;
  public
    /// prepare for a Soundex search
    // - you can specify another language pronunciation than default english
    function Prepare(UpperValue: PAnsiChar;
      Lang: TSynSoundExPronunciation = sndxEnglish): boolean; overload;
    /// prepare for a custom Soundex search
    // - you can specify any language pronunciation from raw TSoundExValues array
    function Prepare(UpperValue: PAnsiChar; Lang: PSoundExValues): boolean; overload;
    /// return true if prepared value is contained in a text buffer
    // (UTF-8 encoded), by using the SoundEx comparison algorithm
    // - search prepared value at every word beginning in U^
    function UTF8(U: PUTF8Char): boolean;
    /// return true if prepared value is contained in a ANSI text buffer
    // by using the SoundEx comparison algorithm
    // - search prepared value at every word beginning in A^
    function Ansi(A: PAnsiChar): boolean;
  end;


/// Retrieve the Soundex value of a text word, from Ansi buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar = nil;
  Lang: TSynSoundExPronunciation = sndxEnglish): cardinal; overload;

/// Retrieve the Soundex value of a text word, from Ansi buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar;
  Lang: PSoundExValues): cardinal; overload;

/// Retrieve the Soundex value of a text word, from UTF-8 buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
// - very fast: all UTF-8 decoding is handled on the fly
function SoundExUTF8(U: PUTF8Char; next: PPUTF8Char = nil;
  Lang: TSynSoundExPronunciation = sndxEnglish): cardinal;

const
  /// number of bits to use for each interresting soundex char
  // - default is to use 8 bits, i.e. 4 soundex chars, which is the
  // standard approach
  // - for a more detailled soundex, use 4 bits resolution, which will
  // compute up to 7 soundex chars in a cardinal (that's our choice)
  SOUNDEX_BITS = 4;



{ ****************** Versatile Expression Search Engine }

type
  /// exception type used by TExprParser
  EExprParser = class(ESynException);

  /// identify an expression search engine node type, as used by TExprParser
  TExprNodeType = (entWord, entNot, entOr, entAnd);

  /// results returned by TExprParserAbstract.Parse method
  TExprParserResult = (
    eprSuccess, eprNoExpression,
    eprMissingParenthesis, eprTooManyParenthesis, eprMissingFinalWord,
    eprInvalidExpression, eprUnknownVariable, eprUnsupportedOperator,
    eprInvalidConstantOrVariable);

  TParserAbstract = class;

  /// stores an expression search engine node, as used by TExprParser
  TExprNode = class(TSynPersistent)
  protected
    fNext: TExprNode;
    fNodeType: TExprNodeType;
    function Append(node: TExprNode): boolean;
  public
    /// initialize a node for the search engine
    constructor Create(nodeType: TExprNodeType); reintroduce;
    /// recursively destroys the linked list of nodes (i.e. Next)
    destructor Destroy; override;
    /// browse all nodes until Next = nil
    function Last: TExprNode;
    /// points to the next node in the parsed tree
    property Next: TExprNode read fNext;
    /// what is actually stored in this node
    property NodeType: TExprNodeType read fNodeType;
  end;

  /// abstract class to handle word search, as used by TExprParser
  TExprNodeWordAbstract = class(TExprNode)
  protected
    fOwner: TParserAbstract;
    fWord: RawUTF8;
    /// should be set from actual data before TExprParser.Found is called
    fFound: boolean;
    function ParseWord: TExprParserResult; virtual; abstract;
  public
    /// you should override this virtual constructor for proper initialization
    constructor Create(aOwner: TParserAbstract; const aWord: RawUTF8); reintroduce; virtual;
  end;

  /// class-reference type (metaclass) for a TExprNode
  // - allow to customize the actual searching process for entWord
  TExprNodeWordClass = class of TExprNodeWordAbstract;

  /// parent class of TExprParserAbstract
  TParserAbstract = class(TSynPersistent)
  protected
    fExpression, fCurrentWord, fAndWord, fOrWord, fNotWord: RawUTF8;
    fCurrent: PUTF8Char;
    fCurrentError: TExprParserResult;
    fFirstNode: TExprNode;
    fWordClass: TExprNodeWordClass;
    fWords: array of TExprNodeWordAbstract;
    fWordCount: integer;
    fNoWordIsAnd: boolean;
    fFoundStack: array[byte] of boolean; // simple stack-based virtual machine
    procedure ParseNextCurrentWord; virtual; abstract;
    function ParseExpr: TExprNode;
    function ParseFactor: TExprNode;
    function ParseTerm: TExprNode;
    procedure Clear; virtual;
    // override this method to initialize fWordClass and fAnd/Or/NotWord
    procedure Initialize; virtual; abstract;
    /// perform the expression search over TExprNodeWord.fFound flags
    // - warning: caller should check that fFirstNode<>nil (e.g. WordCount>0)
    function Execute: boolean; {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize an expression parser
    constructor Create; override;
    /// finalize the expression parser
    destructor Destroy; override;
    /// initialize the parser from a given text expression
    function Parse(const aExpression: RawUTF8): TExprParserResult;
    /// try this parser class on a given text expression
    // - returns '' on success, or an explicit error message (e.g.
    // 'Missing parenthesis')
    class function ParseError(const aExpression: RawUTF8): RawUTF8;
    /// the associated text expression used to define the search
    property Expression: RawUTF8 read fExpression;
    /// how many words did appear in the search expression
    property WordCount: integer read fWordCount;
  end;

  /// abstract class to parse a text expression into nodes
  // - you should inherit this class to provide actual text search
  // - searched expressions can use parenthesis and &=AND -=WITHOUT +=OR operators,
  // e.g. '((w1 & w2) - w3) + w4' means ((w1 and w2) without w3) or w4
  // - no operator is handled like a AND, e.g. 'w1 w2' = 'w1 & w2'
  TExprParserAbstract = class(TParserAbstract)
  protected
    procedure ParseNextCurrentWord; override;
    // may be overriden to provide custom words escaping (e.g. handle quotes)
    procedure ParseNextWord; virtual;
    procedure Initialize; override;
  end;

  /// search expression engine using TMatch for the actual word searches
  TExprParserMatch = class(TExprParserAbstract)
  protected
    fCaseSensitive: boolean;
    fMatchedLastSet: integer;
    procedure Initialize; override;
  public
    /// initialize the search engine
    constructor Create(aCaseSensitive: boolean = true); reintroduce;
    /// returns TRUE if the expression is within the text buffer
    function Search(aText: PUTF8Char; aTextLen: PtrInt): boolean; overload;
    /// returns TRUE if the expression is within the text buffer
    function Search(const aText: RawUTF8): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
  end;


const
  /// may be used when overriding TExprParserAbstract.ParseNextWord method
  PARSER_STOPCHAR = ['&', '+', '-', '(', ')'];

function ToText(r: TExprParserResult): PShortString; overload;
function ToUTF8(r: TExprParserResult): RawUTF8; overload;



{ ****************** Bloom Filter Probabilistic Index }

type
  /// implements a thread-safe Bloom Filter storage
  // - a "Bloom Filter" is a space-efficient probabilistic data structure,
  // that is used to test whether an element is a member of a set. False positive
  // matches are possible, but false negatives are not. Elements can be added to
  // the set, but not removed. Typical use cases are to avoid unecessary
  // slow disk or network access if possible, when a lot of items are involved.
  // - memory use is very low, when compared to storage of all values: fewer
  // than 10 bits per element are required for a 1% false positive probability,
  // independent of the size or number of elements in the set - for instance,
  // storing 10,000,000 items presence with 1% of false positive ratio
  // would consume only 11.5 MB of memory, using 7 hash functions
  // - use Insert() methods to add an item to the internal bits array, and
  // Reset() to clear all bits array, if needed
  // - MayExist() function would check if the supplied item was probably set
  // - SaveTo() and LoadFrom() methods allow transmission of the bits array,
  // for a disk/database storage or transmission over a network
  // - internally, several (hardware-accelerated) crc32c hash functions will be
  // used, with some random seed values, to simulate several hashing functions
  // - Insert/MayExist/Reset methods are thread-safe
  TSynBloomFilter = class(TSynPersistentLock)
  private
    fSize: cardinal;
    fFalsePositivePercent: double;
    fBits: cardinal;
    fHashFunctions: cardinal;
    fInserted: cardinal;
    fStore: RawByteString;
    function GetInserted: cardinal;
  public
    /// initialize the internal bits storage for a given number of items
    // - by default, internal bits array size will be guess from a 1 % false
    // positive rate - but you may specify another value, to reduce memory use
    // - this constructor would compute and initialize Bits and HashFunctions
    // corresponding to the expected false positive ratio
    constructor Create(aSize: integer;
      aFalsePositivePercent: double = 1); reintroduce; overload;
    /// initialize the internal bits storage from a SaveTo() binary buffer
    // - this constructor will initialize the internal bits array calling LoadFrom()
    constructor Create(const aSaved: RawByteString;
      aMagic: cardinal = $B1003F11); reintroduce; overload;
    /// add an item in the internal bits array storage
    // - this method is thread-safe
    procedure Insert(const aValue: RawByteString); overload;
    /// add an item in the internal bits array storage
    // - this method is thread-safe
    procedure Insert(aValue: pointer; aValueLen: integer); overload; virtual;
    /// clear the internal bits array storage
    // - you may call this method after some time, if some items may have
    // been removed, to reduce false positives
    // - this method is thread-safe
    procedure Reset; virtual;
    /// returns TRUE if the supplied items was probably set via Insert()
    // - some false positive may occur, but not much than FalsePositivePercent
    // - this method is thread-safe
    function MayExist(const aValue: RawByteString): boolean; overload;
    /// returns TRUE if the supplied items was probably set via Insert()
    // - some false positive may occur, but not much than FalsePositivePercent
    // - this method is thread-safe
    function MayExist(aValue: pointer; aValueLen: integer): boolean; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    function SaveTo(aMagic: cardinal = $B1003F11): RawByteString; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    procedure SaveTo(aDest: TFileBufferWriter;
      aMagic: cardinal = $B1003F11); overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(const aSaved: RawByteString;
      aMagic: cardinal = $B1003F11): boolean; overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(P: PByte; PLen: integer;
      aMagic: cardinal = $B1003F11): boolean; overload; virtual;
  published
    /// maximum number of items which are expected to be inserted
    property Size: cardinal read fSize;
    /// expected percentage (1..100) of false positive results for MayExists()
    property FalsePositivePercent: double read fFalsePositivePercent;
    /// number of bits stored in the internal bits array
    property Bits: cardinal read fBits;
    /// how many hash functions would be applied for each Insert()
    property HashFunctions: cardinal read fHashFunctions;
    /// how many times the Insert() method has been called
    property Inserted: cardinal read GetInserted;
  end;

  /// implements a thread-safe differential Bloom Filter storage
  // - this inherited class is able to compute incremental serialization of
  // its internal bits array, to reduce network use
  // - an obfuscated revision counter is used to identify storage history
  TSynBloomFilterDiff = class(TSynBloomFilter)
  protected
    fRevision: Int64;
    fSnapShotAfterMinutes: cardinal;
    fSnapshotAfterInsertCount: cardinal;
    fSnapshotTimestamp: Int64;
    fSnapshotInsertCount: cardinal;
    fKnownRevision: Int64;
    fKnownStore: RawByteString;
  public
    /// add an item in the internal bits array storage
    // - this overloaded thread-safe method would compute fRevision
    procedure Insert(aValue: pointer; aValueLen: integer); override;
    /// clear the internal bits array storage
    // - this overloaded thread-safe method would reset fRevision
    procedure Reset; override;
    /// store the internal bits array into an incremental binary buffer
    // - here the difference from a previous SaveToDiff revision will be computed
    // - if aKnownRevision is outdated (e.g. if equals 0), the whole bits array
    // would be returned, and around 10 bits per item would be transmitted
    // (for 1% false positive ratio)
    // - incremental retrieval would then return around 10 bytes per newly added
    // item since the last snapshot reference state (with 1% ratio, i.e. 7 hash
    // functions)
    function SaveToDiff(const aKnownRevision: Int64): RawByteString;
    /// use the current internal bits array state as known revision
    // - is done the first time SaveToDiff() is called, then after 1/32th of
    // the filter size has been inserted (see SnapshotAfterInsertCount property),
    // or after SnapShotAfterMinutes property timeout period
    procedure DiffSnapshot;
    /// retrieve the revision number from an incremental binary buffer
    // - returns 0 if the supplied binary buffer does not match this bloom filter
    function DiffKnownRevision(const aDiff: RawByteString): Int64;
    /// read the internal bits array from an incremental binary buffer
    // - as previously serialized by the SaveToDiff() method
    // - may be used to transmit or store the state of a dataset
    // - returns false if the supplied content is incorrect, e.g. if the known
    // revision is deprecated
    function LoadFromDiff(const aDiff: RawByteString): boolean;
    /// the opaque revision number of this internal storage
    // - is in fact the Unix timestamp shifted by 31 bits, and an incremental
    // counter: this pattern will allow consistent IDs over several ServPanels
    property Revision: Int64 read fRevision;
    /// after how many Insert() the internal bits array storage should be
    // promoted as known revision
    // - equals Size div 32 by default
    property SnapshotAfterInsertCount: cardinal
      read fSnapshotAfterInsertCount write fSnapshotAfterInsertCount;
    /// after how many time the internal bits array storage should be
    // promoted as known revision
    // - equals 30 minutes by default
    property SnapShotAfterMinutes: cardinal
      read fSnapShotAfterMinutes write fSnapShotAfterMinutes;
  end;


/// RLE compression of a memory buffer containing mostly zeros
// - will store the number of consecutive zeros instead of plain zero bytes
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also compute the crc32c of the supplied content
// - use ZeroDecompress() to expand the compressed result
// - resulting content would be at most 14 bytes bigger than the input
// - you may use this function before SynLZ compression
procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TFileBufferWriter);

/// RLE uncompression of a memory buffer containing mostly zeros
// - returns Dest='' if P^ is not a valid ZeroCompress() function result
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also check the crc32c of the supplied content
procedure ZeroDecompress(P: PByte; Len: integer; var Dest: RawByteString);

/// RLE compression of XORed memory buffers resulting in mostly zeros
// - will perform ZeroCompress(Dest^ := New^ xor Old^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.SaveToDiff() in incremental mode
// - will also compute the crc32c of the supplied content
procedure ZeroCompressXor(New, Old: PAnsiChar; Len: cardinal;
  Dest: TFileBufferWriter);

/// RLE uncompression and ORing of a memory buffer containing mostly zeros
// - will perform Dest^ := Dest^ or ZeroDecompress(P^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.LoadFromDiff() in incremental mode
// - returns false if P^ is not a valid ZeroCompress/ZeroCompressXor() result
// - will also check the crc32c of the supplied content
function ZeroDecompressOr(P, Dest: PAnsiChar; Len, DestLen: integer): boolean;


const
  /// normal pattern search depth for DeltaCompress()
  // - gives good results on most content
  DELTA_LEVEL_FAST = 100;
  /// brutal pattern search depth for DeltaCompress()
  // - may become very slow, with minor benefit, on huge content
  DELTA_LEVEL_BEST = 500;
  /// 2MB as internal chunks/window default size for DeltaCompress()
  // - will use up to 9 MB of RAM during DeltaCompress() - none in DeltaExtract()
  DELTA_BUF_DEFAULT = 2 shl 20;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old + Delta
function DeltaCompress(const New, Old: RawByteString;
  Level: integer = DELTA_LEVEL_FAST;
  BufSize: integer = DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  Level: integer = DELTA_LEVEL_FAST;
  BufSize: integer = DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old + Delta
// - caller should call Freemem(Delta) once finished with the output buffer
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level: integer = DELTA_LEVEL_FAST;
  BufSize: integer = DELTA_BUF_DEFAULT): integer; overload;

type
  /// result of function DeltaExtract()
  TDeltaError = (
    dsSuccess, dsCrcCopy, dsCrcComp, dsCrcBegin, dsCrcEnd, dsCrcExtract,
    dsFlag, dsLen);

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(const Delta: RawByteString): integer; overload;

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(Delta: PAnsiChar): integer; overload;

/// apply the delta binary as computed by DeltaCompress()
// - decompression don't use any RAM, will perform crc32c check, and is very fast
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(const Delta, Old: RawByteString;
  out New: RawByteString): TDeltaError; overload;

/// low-level apply the delta binary as computed by DeltaCompress()
// - New should already be allocated with DeltaExtractSize(Delta) bytes
// - as such, expect Delta, Old and New to be <> nil, and Delta <> '='
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(Delta, Old, New: PAnsiChar): TDeltaError; overload;

function ToText(err: TDeltaError): PShortString; overload;




implementation


{ ****************** GLOB and SOUNDEX Text Search }

// inspired by ZMatchPattern.pas - http://www.zeoslib.sourceforge.net
procedure TMatch.MatchMain;
var
  RangeStart, RangeEnd: PtrInt;
  c: AnsiChar;
  flags: set of (Invert, MemberMatch);
begin
  while ((State = sNONE) and (P <= PMax)) do
  begin
    c := Upper[Pattern[P]];
    if T > TMax then
    begin
      if (c = '*') and (P + 1 > PMax) then
        State := sVALID
      else
        State := sABORT;
      exit;
    end
    else
      case c of
        '?':
          ;
        '*':
          MatchAfterStar;
        '[':
          begin
            inc(P);
            byte(flags) := 0;
            if Pattern[P] in ['!', '^'] then
            begin
              include(flags, Invert);
              inc(P);
            end;
            if (Pattern[P] = ']') then
            begin
              State := sPATTERN;
              exit;
            end;
            c := Upper[Text[T]];
            while Pattern[P] <> ']' do
            begin
              RangeStart := P;
              RangeEnd := P;
              inc(P);
              if P > PMax then
              begin
                State := sPATTERN;
                exit;
              end;
              if Pattern[P] = '-' then
              begin
                inc(P);
                RangeEnd := P;
                if (P > PMax) or (Pattern[RangeEnd] = ']') then
                begin
                  State := sPATTERN;
                  exit;
                end;
                inc(P);
              end;
              if P > PMax then
              begin
                State := sPATTERN;
                exit;
              end;
              if RangeStart < RangeEnd then
              begin
                if (c >= Upper[Pattern[RangeStart]]) and
                   (c <= Upper[Pattern[RangeEnd]]) then
                begin
                  include(flags, MemberMatch);
                  break;
                end;
              end
              else if (c >= Upper[Pattern[RangeEnd]]) and
                      (c <= Upper[Pattern[RangeStart]]) then
              begin
                include(flags, MemberMatch);
                break;
              end;
            end;
            if ((Invert in flags) and (MemberMatch in flags)) or
               not ((Invert in flags) or (MemberMatch in flags)) then
            begin
              State := sRANGE;
              exit;
            end;
            if MemberMatch in flags then
              while (P <= PMax) and (Pattern[P] <> ']') do
                inc(P);
            if P > PMax then
            begin
              State := sPATTERN;
              exit;
            end;
          end;
      else
        if c <> Upper[Text[T]] then
          State := sLITERAL;
      end;
    inc(P);
    inc(T);
  end;
  if State = sNONE then
    if T <= TMax then
      State := sEND
    else
      State := sVALID;
end;

procedure TMatch.MatchAfterStar;
var
  retryT, retryP: PtrInt;
begin
  if (TMax = 1) or (P = PMax) then
  begin
    State := sVALID;
    exit;
  end
  else if (PMax = 0) or (TMax = 0) then
  begin
    State := sABORT;
    exit;
  end;
  while ((T <= TMax) and (P < PMax)) and (Pattern[P] in ['?', '*']) do
  begin
    if Pattern[P] = '?' then
      inc(T);
    inc(P);
  end;
  if T >= TMax then
  begin
    State := sABORT;
    exit;
  end
  else if P >= PMax then
  begin
    State := sVALID;
    exit;
  end;
  repeat
    if (Upper[Pattern[P]] = Upper[Text[T]]) or (Pattern[P] = '[') then
    begin
      retryT := T;
      retryP := P;
      MatchMain;
      if State = sVALID then
        break;
      State := sNONE; // retry until end of Text, (check below) or State valid
      T := retryT;
      P := retryP;
    end;
    inc(T);
    if (T > TMax) or (P > PMax) then
    begin
      State := sABORT;
      exit;
    end;
  until State <> sNONE;
end;

function SearchAny(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  aMatch.State := sNONE;
  aMatch.P := 0;
  aMatch.T := 0;
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  aMatch.MatchMain;
  result := aMatch.State = sVALID;
end;

// faster alternative (without recursion) for only * ? (but not [...])

{$ifdef CPUX86} // less registers on this CPU

function SearchNoRange(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  c: AnsiChar;
  pat, txt: PtrInt; // use local registers
begin
  aMatch.T := 0; // aMatch.P/T are used for retry positions after *
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  pat := 0;
  txt := 0;
  repeat
    if pat <= aMatch.PMax then
    begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then
          begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*':
          begin
            aMatch.P := pat;
            aMatch.T := txt + 1;
            inc(pat);
            continue;
          end;
      else
        if (txt <= aMatch.TMax) and (c = aMatch.Text[txt]) then
        begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and (txt <= aMatch.TMax + 1) then
    begin
      inc(aMatch.T);
      pat := aMatch.P + 1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;

{$else} // optimized for x86_64/ARM with more registers

function SearchNoRange(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  c: AnsiChar;
  pat, patend, txtend, txtretry, patretry: PUTF8Char;
label
  fin;
begin
  pat := pointer(aMatch.Pattern);
  if pat = nil then
    goto fin;
  patend := pat + aMatch.PMax;
  patretry := nil;
  txtend := aText + aTextLen - 1;
  txtretry := nil;
  repeat
    if pat <= patend then
    begin
      c := pat^;
      if c <> '*' then
        if c <> '?' then
        begin
          if (aText <= txtend) and (c = aText^) then
          begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
        else
        begin // '?'
          if aText <= txtend then
          begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
      else
      begin // '*'
        inc(pat);
        txtretry := aText + 1;
        patretry := pat;
        continue;
      end;
    end
    else if aText > txtend then
      break;
    if (PtrInt(PtrUInt(txtretry)) > 0) and (txtretry <= txtend + 1) then
    begin
      aText := txtretry;
      inc(txtretry);
      pat := patretry;
      continue;
    end;
fin:
    result := false;
    exit;
  until false;
  result := true;
end;

{$endif CPUX86}

function SearchNoRangeU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  c: AnsiChar;
  pat, txt: PtrInt;
begin
  aMatch.T := 0;
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  pat := 0;
  txt := 0;
  repeat
    if pat <= aMatch.PMax then
    begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then
          begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*':
          begin
            aMatch.P := pat;
            aMatch.T := txt + 1;
            inc(pat);
            continue;
          end;
      else
        if (txt <= aMatch.TMax) and
           (aMatch.Upper[c] = aMatch.Upper[aMatch.Text[txt]]) then
        begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and (txt <= aMatch.TMax + 1) then
    begin
      inc(aMatch.T);
      pat := aMatch.P + 1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;

function SimpleContainsU(t, tend, p: PUTF8Char; pmax: PtrInt; up: PNormTable): boolean;
  {$ifdef HASINLINE} inline;{$endif}
// brute force case-insensitive search p[0..pmax] in t..tend-1
var
  first: AnsiChar;
  i: PtrInt;
label
  next;
begin
  first := up[p^];
  repeat
    if up[t^] <> first then
    begin
next:
      inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if up[t[i]] <> up[p[i]] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$ifdef CPU64} // naive but very efficient code generation on FPC x86-64

function SimpleContains8(t, tend, p: PUTF8Char; pmax: PtrInt): boolean; inline;
label
  next;
var
  i, first: PtrInt;
begin
  first := PPtrInt(p)^;
  repeat
    if PPtrInt(t)^ <> first then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 8 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$endif CPU64}


{$ifdef CPUX86}

function SimpleContains1(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE} inline;{$endif}
label
  next;
var
  i: PtrInt;
begin
  repeat
    if t^ <> p^ then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

function SimpleContains4(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE} inline;{$endif}
label
  next;
var
  i: PtrInt;
begin
  repeat
    if PCardinal(t)^ <> PCardinal(p)^ then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$else}

function SimpleContains1(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE} inline;{$endif}
label
  next;
var
  i: PtrInt;
  first: AnsiChar;
begin
  first := p^;
  repeat
    if t^ <> first then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

function SimpleContains4(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE} inline;{$endif}
label
  next;
var
  i: PtrInt;
  first: cardinal;
begin
  first := PCardinal(p)^;
  repeat
    if PCardinal(t)^ <> first then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$endif CPUX86}

function CompareMemU(P1, P2: PUTF8Char; len: PtrInt; U: PNormTable): Boolean;
  {$ifdef FPC} inline;{$endif}
begin // here we know that len>0
  result := false;
  repeat
    dec(len);
    if U[P1[len]] <> U[P2[len]] then
      exit;
  until len = 0;
  result := true;
end;

function SearchVoid(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := aTextLen = 0;
end;

function SearchNoPattern(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextLen) and
            CompareMem(aText, aMatch.Pattern, aTextLen);
end;

function SearchNoPatternU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextLen) and
            CompareMemU(aText, aMatch.Pattern, aTextLen, aMatch.Upper);
end;

function SearchContainsValid(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := true;
end;

function SearchContainsU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContainsU(aText, aText + aTextLen,
      aMatch.Pattern, aMatch.PMax, aMatch.Upper)
  else
    result := false;
end;

function SearchContains1(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains1(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

function SearchContains4(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains4(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

{$ifdef CPU64}
function SearchContains8(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin // optimized e.g. to search an IP address as '*12.34.56.78*' in logs
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains8(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;
{$endif CPU64}

function SearchStartWith(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextLen) and
            CompareMem(aText, aMatch.Pattern, aMatch.PMax + 1);
end;

function SearchStartWithU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextLen) and
            CompareMemU(aText, aMatch.Pattern, aMatch.PMax + 1, aMatch.Upper);
end;

function SearchEndWith(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextLen >= 0) and
            CompareMem(aText + aTextLen, aMatch.Pattern, aMatch.PMax);
end;

function SearchEndWithU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextLen >= 0) and
            CompareMemU(aText + aTextLen, aMatch.Pattern, aMatch.PMax, aMatch.Upper);
end;

procedure TMatch.Prepare(const aPattern: RawUTF8; aCaseInsensitive, aReuse: boolean);
begin
  Prepare(pointer(aPattern), length(aPattern), aCaseInsensitive, aReuse);
end;

procedure TMatch.Prepare(aPattern: PUTF8Char; aPatternLen: integer;
  aCaseInsensitive, aReuse: boolean);
const
  SPECIALS: PUTF8Char = '*?[';
begin
  Pattern := aPattern;
  pmax := aPatternLen - 1; // search in Pattern[0..PMax]
  if Pattern = nil then
  begin
    Search := SearchVoid;
    exit;
  end;
  if aCaseInsensitive and not IsCaseSensitive(aPattern, aPatternLen) then
    aCaseInsensitive := false; // don't slow down e.g. number or IP search
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7
  else
    Upper := @NormToNorm;
  Search := nil;
  if aReuse then
    if strcspn(Pattern, SPECIALS) > pmax then
      if aCaseInsensitive then
        Search := SearchNoPatternU
      else
        Search := SearchNoPattern
    else if pmax > 0 then
    begin
      if Pattern[pmax] = '*' then
      begin
        if strcspn(Pattern + 1, SPECIALS) = pmax - 1 then
          case Pattern[0] of
            '*':
              begin
                // *something*
                inc(Pattern);
                dec(pmax, 2); // trim trailing and ending *
                if pmax < 0 then
                  Search := SearchContainsValid
                else if aCaseInsensitive then
                  Search := SearchContainsU
              {$ifdef CPU64}
                else if pmax >= 7 then
                  Search := SearchContains8
              {$endif}
                else if pmax >= 3 then
                  Search := SearchContains4
                else
                  Search := SearchContains1;
              end;
            '?':
              // ?something*
              if aCaseInsensitive then
                Search := SearchNoRangeU
              else
                Search := SearchNoRange;
            '[':
              Search := SearchAny;
          else
            begin
              dec(pmax); // trim trailing *
              if aCaseInsensitive then
                Search := SearchStartWithU
              else
                Search := SearchStartWith;
            end;
          end;
      end
      else if (Pattern[0] = '*') and (strcspn(Pattern + 1, SPECIALS) >= pmax) then
      begin
        inc(Pattern); // jump leading *
        if aCaseInsensitive then
          Search := SearchEndWithU
        else
          Search := SearchEndWith;
      end;
    end
    else if Pattern[0] in ['*', '?'] then
      Search := SearchContainsValid;
  if not Assigned(Search) then
  begin
    aPattern := PosChar(Pattern, '[');
    if (aPattern = nil) or (aPattern - Pattern > pmax) then
      if aCaseInsensitive then
        Search := SearchNoRangeU
      else
        Search := SearchNoRange
    else
      Search := SearchAny;
  end;
end;

type
  // Holub and Durian (2005) SBNDM2 algorithm
  // see http://www.cri.haifa.ac.il/events/2005/string/presentations/Holub.pdf
  TSBNDMQ2Mask = array[AnsiChar] of cardinal;
  PSBNDMQ2Mask = ^TSBNDMQ2Mask;

function SearchSBNDMQ2ComputeMask(const Pattern: RawUTF8; u: PNormTable): RawByteString;
var
  i: PtrInt;
  p: PAnsiChar absolute Pattern;
  m: PSBNDMQ2Mask absolute result;
  c: PCardinal;
begin
  SetString(result, nil, SizeOf(m^));
  FillCharFast(m^, SizeOf(m^), 0);
  for i := 0 to length(Pattern) - 1 do
  begin
    c := @m^[u[p[i]]]; // for FPC code generation
    c^ := c^ or (1 shl i);
  end;
end;

function SearchSBNDMQ2(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern); // was filled by SearchSBNDMQ2ComputeMask()
  max := aMatch^.pmax;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then
  begin
    repeat
      state := mask[aText[i + 1]] shr 1; // in two steps for better FPC codegen
      state := state and mask[aText[i]];
      if state = 0 then
      begin
        inc(i, max); // fast skip
        if i >= aTextLen then
          break;
        continue;
      end;
      j := i - max;
      repeat
        dec(i);
        if i < 0 then
          break;
        state := (state shr 1) and mask[aText[i]];
      until state = 0;
      if i = j then
      begin
        result := true;
        exit;
      end;
      inc(i, max);
      if i >= aTextLen then
        break;
    until false;
  end;
  result := false;
end;

function SearchSBNDMQ2U(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  u: PNormTable;
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern);
  max := aMatch^.pmax;
  u := aMatch^.Upper;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then
  begin
    repeat
      state := mask[u[aText[i + 1]]] shr 1;
      state := state and mask[u[aText[i]]];
      if state = 0 then
      begin
        inc(i, max);
        if i >= aTextLen then
          break;
        continue;
      end;
      j := i - max;
      repeat
        dec(i);
        if i < 0 then
          break;
        state := (state shr 1) and mask[u[aText[i]]];
      until state = 0;
      if i = j then
      begin
        result := true;
        exit;
      end;
      inc(i, max);
      if i >= aTextLen then
        break;
    until false;
  end;
  result := false;
end;

procedure TMatch.PrepareContains(var aPattern: RawUTF8; aCaseInsensitive: boolean);
begin
  pmax := length(aPattern) - 1;
  if aCaseInsensitive and not IsCaseSensitive(pointer(aPattern), pmax + 1) then
    aCaseInsensitive := false;
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7
  else
    Upper := @NormToNorm;
  if pmax < 0 then
    Search := SearchContainsValid
  else if pmax > 30 then
    if aCaseInsensitive then
      Search := SearchContainsU
    else
      {$ifdef CPU64}
      Search := SearchContains8
      {$else}
      Search := SearchContains4
      {$endif CPU64}
  else if pmax >= 1 then
  begin
    // PMax in [1..30] = len in [2..31]
    aPattern := SearchSBNDMQ2ComputeMask(aPattern, Upper); // lookup table
    if aCaseInsensitive then
      Search := SearchSBNDMQ2U
    else
      Search := SearchSBNDMQ2;
  end
  else if aCaseInsensitive then
    Search := SearchContainsU
  else
    Search := SearchContains1; // todo: use IndexByte() on FPC?
  Pattern := pointer(aPattern);
end;

procedure TMatch.PrepareRaw(aPattern: PUTF8Char; aPatternLen: integer;
  aSearch: TMatchSearchFunction);
begin
  Pattern := aPattern;
  pmax := aPatternLen - 1; // search in Pattern[0..PMax]
  Search := aSearch;
end;

function TMatch.Match(const aText: RawUTF8): boolean;
begin
  if aText <> '' then
    result := Search(@self, pointer(aText), length(aText))
  else
    result := pmax < 0;
end;

function TMatch.Match(aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  if (aText <> nil) and (aTextLen > 0) then
    result := Search(@self, aText, aTextLen)
  else
    result := pmax < 0;
end;

function TMatch.MatchThreadSafe(const aText: RawUTF8): boolean;
var
  local: TMatch; // thread-safe with no lock!
begin
  local := self;
  if aText <> '' then
    result := local.Search(@local, pointer(aText), length(aText))
  else
    result := local.PMax < 0;
end;

function TMatch.MatchString(const aText: string): boolean;
var
  local: TMatch; // thread-safe with no lock!
  temp: TSynTempBuffer;
  len: integer;
begin
  if aText = '' then
  begin
    result := pmax < 0;
    exit;
  end;
  len := length(aText);
  temp.Init(len * 3);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(temp.buf, temp.len + 1, pointer(aText), len, [ccfNoTrailingZero]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUTF8(temp.buf, pointer(aText), len) - temp.buf;
  {$endif UNICODE}
  local := self;
  result := local.Search(@local, temp.buf, len);
  temp.Done;
end;

function TMatch.Equals(const aAnother: TMatch): boolean;
begin
  result := (pmax = TMatch(aAnother).pmax) and
            (Upper = TMatch(aAnother).Upper) and
            CompareMem(Pattern, TMatch(aAnother).Pattern, pmax + 1);
end;

function TMatch.PatternLength: integer;
begin
  result := pmax + 1;
end;

function TMatch.PatternText: PUTF8Char;
begin
  result := Pattern;
end;

function IsMatch(const Pattern, Text: RawUTF8; CaseInsensitive: boolean): boolean;
var
  match: TMatch;
begin
  match.Prepare(Pattern, CaseInsensitive, {reuse=}false);
  result := match.Match(Text);
end;

function IsMatchString(const Pattern, Text: string;
  CaseInsensitive: boolean): boolean;
var
  match: TMatch;
  pat, txt: RawUTF8;
begin
  StringToUTF8(Pattern, pat); // local variable is mandatory for FPC
  StringToUTF8(Text, txt);
  match.Prepare(pat, CaseInsensitive, {reuse=}false);
  result := match.Match(txt);
end;

function SetMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  out Match: TMatchDynArray): integer;
var
  P, S: PUTF8Char;
begin
  result := 0;
  P := pointer(CSVPattern);
  if P <> nil then
    repeat
      S := P;
      while not (P^ in [#0, ',']) do
        inc(P);
      if P <> S then
      begin
        SetLength(Match, result + 1);
        Match[result].Prepare(S, P - S, CaseInsensitive, {reuse=}true);
        inc(result);
      end;
      if P^ = #0 then
        break;
      inc(P);
    until false;
end;

function SetMatchs(CSVPattern: PUTF8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer): integer;
var
  S: PUTF8Char;
begin
  result := 0;
  if (CSVPattern <> nil) and (MatchMax >= 0) then
    repeat
      S := CSVPattern;
      while not (CSVPattern^ in [#0, ',']) do
        inc(CSVPattern);
      if CSVPattern <> S then
      begin
        Match^.Prepare(S, CSVPattern - S, CaseInsensitive, {reuse=}true);
        inc(result);
        if result > MatchMax then
          break;
        inc(Match);
      end;
      if CSVPattern^ = #0 then
        break;
      inc(CSVPattern);
    until false;
end;

function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to high(Several) do
    if Several[i].Equals(One) then
      exit;
  result := false;
end;

function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;
var
  n: integer;
begin
  result := not MatchExists(One, Several);
  if result then
  begin
    n := length(Several);
    SetLength(Several, n + 1);
    Several[n] := One;
  end;
end;

function MatchAny(const Match: TMatchDynArray; const Text: RawUTF8): boolean;
var
  m: PMatch;
  i: integer;
begin
  result := true;
  if Match = nil then
    exit;
  m := pointer(Match);
  for i := 1 to length(Match) do
    if m^.Match(Text) then
      exit
    else
      inc(m);
  result := false;
end;

procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TRawUTF8DynArray);
var
  match: TMatchDynArray;
  m, n, i: PtrInt;
begin
  if SetMatchs(CSVPattern, CaseInsensitive, match) = 0 then
    exit;
  n := 0;
  for i := 0 to high(Values) do
    for m := 0 to high(match) do
      if match[m].Match(Values[i]) then
      begin
        if i <> n then
          Values[n] := Values[i];
        inc(n);
        break;
      end;
  if n <> length(Values) then
    SetLength(Values, n);
end;

procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TStringDynArray);
var
  match: TMatchDynArray;
  m, n, i: PtrInt;
begin
  if SetMatchs(CSVPattern, CaseInsensitive, match) = 0 then
    exit;
  n := 0;
  for i := 0 to high(Values) do
    for m := 0 to high(match) do
      if match[m].MatchString(Values[i]) then
      begin
        if i <> n then
          Values[n] := Values[i];
        inc(n);
        break;
      end;
  if n <> length(Values) then
    SetLength(Values, n);
end;


{ TMatchs }

constructor TMatchs.Create(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean);
begin
  inherited Create;
  Subscribe(aPatterns, CaseInsensitive);
end;

function TMatchs.Match(const aText: RawUTF8): integer;
begin
  result := match(pointer(aText), length(aText));
end;

function TMatchs.Match(aText: PUTF8Char; aLen: integer): integer;
var
  one: ^TMatchStore;
  local: TMatch; // thread-safe with no lock!
begin
  if (self = nil) or (fMatch = nil) then
    result := -1 // no filter by name -> allow e.g. to process everything
  else
  begin
    one := pointer(fMatch);
    if aLen <> 0 then
    begin
      for result := 0 to fMatchCount - 1 do
      begin
        local := one^.Pattern;
        if local.Search(@local, aText, aLen) then
          exit;
        inc(one);
      end;
    end
    else
      for result := 0 to fMatchCount - 1 do
        if one^.Pattern.PMax < 0 then
          exit
        else
          inc(one);
    result := -2;
  end;
end;

function TMatchs.MatchString(const aText: string): integer;
var
  temp: TSynTempBuffer;
  len: integer;
begin
  len := length(aText);
  temp.Init(len * 3);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(temp.buf, temp.len + 1, pointer(aText), len, [ccfNoTrailingZero]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUTF8(temp.buf, pointer(aText), len, true) - temp.buf;
  {$endif UNICODE}
  result := match(temp.buf, len);
  temp.Done;
end;

procedure TMatchs.Subscribe(const aPatternsCSV: RawUTF8; CaseInsensitive: Boolean);
var
  patterns: TRawUTF8DynArray;
begin
  CSVToRawUTF8DynArray(pointer(aPatternsCSV), patterns);
  Subscribe(patterns, CaseInsensitive);
end;

procedure TMatchs.Subscribe(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean);
var
  i, j, m, n: integer;
  found: ^TMatchStore;
  pat: PRawUTF8;
begin
  m := length(aPatterns);
  if m = 0 then
    exit;
  n := fMatchCount;
  SetLength(fMatch, n + m);
  pat := pointer(aPatterns);
  for i := 1 to m do
  begin
    found := pointer(fMatch);
    for j := 1 to n do
      if StrComp(pointer(found^.PatternInstance), pointer(pat^)) = 0 then
      begin
        found := nil;
        break;
      end
      else
        inc(found);
    if found <> nil then
      with fMatch[n] do
      begin
        PatternInstance := pat^; // avoid GPF if aPatterns[] is released
        Pattern.Prepare(PatternInstance, CaseInsensitive, {reuse=}true);
        inc(n);
      end;
    inc(pat);
  end;
  fMatchCount := n;
  if n <> length(fMatch) then
    SetLength(fMatch, n);
end;



procedure SoundExComputeAnsi(var p: PAnsiChar; var result: cardinal;
  Values: PSoundExValues);
var
  n, v, old: PtrUInt;
begin
  n := 0;
  old := 0;
  if Values <> nil then
    repeat
      v := NormToUpperByte[ord(p^)]; // also handle 8 bit WinAnsi (1252 accents)
      if not (tcWord in TEXT_BYTES[v]) then
        break;
      inc(p);
      dec(v, ord('B'));
      if v > high(TSoundExValues) then
        continue;
      v := Values[v]; // get soundex value
      if (v = 0) or (v = old) then
        continue; // invalid or dopple value
      old := v;
      result := result shl SOUNDEX_BITS;
      inc(result, v);
      inc(n);
      if n = ((32 - 8) div SOUNDEX_BITS) then // first char use up to 8 bits
        break; // result up to a cardinal size
    until false;
end;

function SoundExComputeFirstCharAnsi(var p: PAnsiChar): cardinal;
label
  Err;
begin
  if p = nil then
  begin
Err:
    result := 0;
    exit;
  end;
  repeat
    result := NormToUpperByte[ord(p^)]; // also handle 8 bit WinAnsi (CP 1252)
    if result = 0 then
      goto Err; // end of input text, without a word
    inc(p);
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G', 'I'..'Z'];
end;

procedure SoundExComputeUTF8(var U: PUTF8Char; var result: cardinal; Values: PSoundExValues);
var
  n, v, old: cardinal;
begin
  n := 0;
  old := 0;
  if Values <> nil then
    repeat
      v := GetNextUTF8Upper(U);
      if not (tcWord in TEXT_BYTES[v]) then
        break;
      dec(v, ord('B'));
      if v > high(TSoundExValues) then
        continue;
      v := Values[v]; // get soundex value
      if (v = 0) or (v = old) then
        continue; // invalid or dopple value
      old := v;
      result := result shl SOUNDEX_BITS;
      inc(result, v);
      inc(n);
      if n = ((32 - 8) div SOUNDEX_BITS) then // first char use up to 8 bits
        break; // result up to a cardinal size
    until false;
end;

function SoundExComputeFirstCharUTF8(var U: PUTF8Char): cardinal;
label
  Err;
begin
  if U = nil then
  begin
Err:
    result := 0;
    exit;
  end;
  repeat
    result := GetNextUTF8Upper(U);
    if result = 0 then
      goto Err; // end of input text, without a word
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G', 'I'..'Z'];
end;


{ TSynSoundEx }

const
  /// english Soundex pronunciation scores
  // - defines the default values used for the SoundEx() function below
  // (used if Values parameter is nil)
  ValueEnglish: TSoundExValues =
  // B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
    (1, 2, 3, 0, 1, 2, 0, 0, 2, 2, 4, 5, 5, 0, 1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2);

  /// french Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueFrench: TSoundExValues =
  // B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
    (1, 2, 3, 0, 9, 7, 0, 0, 7, 2, 4, 5, 5, 0, 1, 2, 6, 8, 3, 0, 9, 0, 8, 0, 8);

  /// spanish Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueSpanish: TSoundExValues =
  // B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
    (1, 2, 3, 0, 1, 2, 0, 0, 0, 2, 0, 5, 5, 0, 1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2);

  SOUNDEXVALUES: array[TSynSoundExPronunciation] of PSoundExValues = (
    @ValueEnglish,
    @ValueFrench,
    @ValueSpanish,
    @ValueEnglish);


function TSynSoundEx.Ansi(A: PAnsiChar): boolean;
var
  Value, c: cardinal;
begin
  result := false;
  if A = nil then
    exit;
  repeat
    // test beginning of word
    c := SoundExComputeFirstCharAnsi(A);
    if c = 0 then
      exit
    else if c = FirstChar then
    begin
      // here we had the first char match -> check if word match UpperValue
      Value := c - (ord('A') - 1);
      SoundExComputeAnsi(A, Value, fValues);
      if Value = search then
      begin
        result := true; // UpperValue found!
        exit;
      end;
    end
    else
      repeat
        if A^ = #0 then
          exit
        else if not (tcWord in TEXT_CHARS[NormToUpper[A^]]) then
          break
        else
          inc(A);
      until false;
    // find beginning of next word
    repeat
      if A^ = #0 then
        exit
      else if tcWord in TEXT_CHARS[NormToUpper[A^]] then
        break
      else
        inc(A);
    until false;
  until false;
end;

function TSynSoundEx.UTF8(U: PUTF8Char): boolean;
var
  Value, c: cardinal;
  V: PUTF8Char;
begin
  result := false;
  if U = nil then
    exit;
  repeat
    // find beginning of word
    c := SoundExComputeFirstCharUTF8(U);
    if c = 0 then
      exit
    else if c = FirstChar then
    begin
      // here we had the first char match -> check if word match UpperValue
      Value := c - (ord('A') - 1);
      SoundExComputeUTF8(U, Value, fValues);
      if Value = search then
      begin
        result := true; // UpperValue found!
        exit;
      end;
    end
    else
      repeat
        c := GetNextUTF8Upper(U);
        if c = 0 then
          exit;
      until not (tcWord in TEXT_BYTES[c]);
    // find beginning of next word
    repeat
      if U = nil then
        exit;
      V := U;
      c := GetNextUTF8Upper(U);
      if c = 0 then
        exit;
    until tcWord in TEXT_BYTES[c];
    U := V;
  until U = nil;
end;

function TSynSoundEx.Prepare(UpperValue: PAnsiChar; Lang: PSoundExValues): boolean;
begin
  fValues := Lang;
  Search := SoundExAnsi(UpperValue, nil, Lang);
  if Search = 0 then
    result := false
  else
  begin
    FirstChar := SoundExComputeFirstCharAnsi(UpperValue);
    result := true;
  end;
end;

function TSynSoundEx.Prepare(UpperValue: PAnsiChar;
  Lang: TSynSoundExPronunciation): boolean;
begin
  result := Prepare(UpperValue, SOUNDEXVALUES[Lang]);
end;

function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar; Lang: PSoundExValues): cardinal;
begin
  result := SoundExComputeFirstCharAnsi(A);
  if result <> 0 then
  begin
    dec(result, ord('A') - 1);   // first Soundex char is first char
    SoundExComputeAnsi(A, result, Lang);
  end;
  if next <> nil then
  begin
    while tcWord in TEXT_CHARS[NormToUpper[A^]] do
      inc(A); // go to end of word
    next^ := A;
  end;
end;

function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExAnsi(A, next, SOUNDEXVALUES[Lang]);
end;

function SoundExUTF8(U: PUTF8Char; next: PPUTF8Char;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExComputeFirstCharUTF8(U);
  if result <> 0 then
  begin
    dec(result, ord('A') - 1);   // first Soundex char is first char
    SoundExComputeUTF8(U, result, SOUNDEXVALUES[Lang]);
  end;
  if next <> nil then
    next^ := FindNextUTF8WordBegin(U);
end;



{ ****************** Versatile Expression Search Engine }


function ToText(r: TExprParserResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TExprParserResult), ord(r));
end;

function ToUTF8(r: TExprParserResult): RawUTF8;
begin
  result := UnCamelCase(TrimLeftLowerCaseShort(ToText(r)));
end;


{ TExprNode }

function TExprNode.Append(node: TExprNode): boolean;
begin
  result := node <> nil;
  if result then
    Last.fNext := node;
end;

constructor TExprNode.Create(nodeType: TExprNodeType);
begin
  inherited Create;
  fNodeType := nodeType;
end;

destructor TExprNode.Destroy;
begin
  fNext.Free;
  inherited Destroy;
end;

function TExprNode.Last: TExprNode;
begin
  result := self;
  while result.Next <> nil do
    result := result.Next;
end;


{ TParserAbstract }

constructor TParserAbstract.Create;
begin
  inherited Create;
  Initialize;
end;

destructor TParserAbstract.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TParserAbstract.Clear;
begin
  fWordCount := 0;
  fWords := nil;
  fExpression := '';
  FreeAndNil(fFirstNode);
end;

function TParserAbstract.ParseExpr: TExprNode;
begin
  result := ParseFactor;
  ParseNextCurrentWord;
  if (fCurrentWord = '') or (fCurrentWord = ')') then
    exit;
  if IdemPropNameU(fCurrentWord, fAndWord) then
  begin
    // w1 & w2 = w1 AND w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entAnd));
    exit;
  end
  else if IdemPropNameU(fCurrentWord, fOrWord) then
  begin
    // w1 + w2 = w1 OR w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entOr));
    exit;
  end
  else if fNoWordIsAnd and result.Append(ParseExpr) then
    // 'w1 w2' = 'w1 & w2'
    result.Append(TExprNode.Create(entAnd));
end;

function TParserAbstract.ParseFactor: TExprNode;
begin
  if fCurrentError <> eprSuccess then
    result := nil
  else if IdemPropNameU(fCurrentWord, fNotWord) then
  begin
    ParseNextCurrentWord;
    result := ParseFactor;
    if fCurrentError <> eprSuccess then
      exit;
    result.Append(TExprNode.Create(entNot));
  end
  else
    result := ParseTerm;
end;

function TParserAbstract.ParseTerm: TExprNode;
begin
  result := nil;
  if fCurrentError <> eprSuccess then
    exit;
  if fCurrentWord = '(' then
  begin
    ParseNextCurrentWord;
    result := ParseExpr;
    if fCurrentError <> eprSuccess then
      exit;
    if fCurrentWord <> ')' then
    begin
      FreeAndNil(result);
      fCurrentError := eprMissingParenthesis;
    end;
  end
  else if fCurrentWord = '' then
  begin
    result := nil;
    fCurrentError := eprMissingFinalWord;
  end
  else
  try // calls meta-class overriden constructor
    result := fWordClass.Create(self, fCurrentWord);
    fCurrentError := TExprNodeWordAbstract(result).ParseWord;
    if fCurrentError <> eprSuccess then
    begin
      FreeAndNil(result);
      exit;
    end;
    SetLength(fWords, fWordCount + 1);
    fWords[fWordCount] := TExprNodeWordAbstract(result);
    inc(fWordCount);
  except
    FreeAndNil(result);
    fCurrentError := eprInvalidExpression;
  end;
end;

function TParserAbstract.Parse(const aExpression: RawUTF8): TExprParserResult;
var
  depth: integer;
  n: TExprNode;
begin
  Clear;
  fCurrentError := eprSuccess;
  fCurrent := pointer(aExpression);
  ParseNextCurrentWord;
  if fCurrentWord = '' then
  begin
    result := eprNoExpression;
    exit;
  end;
  fFirstNode := ParseExpr;
  result := fCurrentError;
  if result = eprSuccess then
  begin
    depth := 0;
    n := fFirstNode;
    while n <> nil do
    begin
      case n.NodeType of
        entWord:
          begin
            inc(depth);
            if depth > high(fFoundStack) then
            begin
              result := eprTooManyParenthesis;
              break;
            end;
          end;
        entOr, entAnd:
          dec(depth);
      end;
      n := n.Next;
    end;
  end;
  if result = eprSuccess then
    fExpression := aExpression
  else
    Clear;
  fCurrent := nil;
end;

class function TParserAbstract.ParseError(const aExpression: RawUTF8): RawUTF8;
var
  parser: TParserAbstract;
  res: TExprParserResult;
begin
  parser := Create;
  try
    res := parser.Parse(aExpression);
    if res = eprSuccess then
      result := ''
    else
      result := ToUTF8(res);
  finally
    parser.Free;
  end;
end;

function TParserAbstract.Execute: boolean;
var
  n: TExprNode;
  st: PBoolean;
begin // code below compiles very efficiently on FPC/x86-64
  st := @fFoundStack;
  n := fFirstNode;
  repeat
    case n.NodeType of
      entWord:
        begin
          st^ := TExprNodeWordAbstract(n).fFound;
          inc(st); // see eprTooManyParenthesis above to avoid buffer overflow
        end;
      entNot:
        PAnsiChar(st)[-1] := AnsiChar(ord(PAnsiChar(st)[-1]) xor 1);
      entOr:
        begin
          dec(st);
          PAnsiChar(st)[-1] := AnsiChar(st^ or boolean(PAnsiChar(st)[-1]));
        end; { TODO : optimize TExprParser OR when left member is already TRUE }
      entAnd:
        begin
          dec(st);
          PAnsiChar(st)[-1] := AnsiChar(st^ and boolean(PAnsiChar(st)[-1]));
        end;
    end;
    n := n.Next;
  until n = nil;
  result := boolean(PAnsiChar(st)[-1]);
end;


{ TExprParserAbstract }

procedure TExprParserAbstract.Initialize;
begin
  fAndWord := '&';
  fOrWord := '+';
  fNotWord := '-';
  fNoWordIsAnd := true;
end;

procedure TExprParserAbstract.ParseNextCurrentWord;
var
  P: PUTF8Char;
begin
  fCurrentWord := '';
  P := fCurrent;
  if P = nil then
    exit;
  while P^ in [#1..' '] do
    inc(P);
  if P^ = #0 then
    exit;
  if P^ in PARSER_STOPCHAR then
  begin
    FastSetString(fCurrentWord, P, 1);
    fCurrent := P + 1;
  end
  else
  begin
    fCurrent := P;
    ParseNextWord;
  end;
end;

procedure TExprParserAbstract.ParseNextWord;
const
  STOPCHAR = PARSER_STOPCHAR + [#0, ' '];
var
  P: PUTF8Char;
begin
  P := fCurrent;
  while not (P^ in STOPCHAR) do
    inc(P);
  FastSetString(fCurrentWord, fCurrent, P - fCurrent);
  fCurrent := P;
end;


{ TExprNodeWordAbstract }

constructor TExprNodeWordAbstract.Create(aOwner: TParserAbstract;
  const aWord: RawUTF8);
begin
  inherited Create(entWord);
  fWord := aWord;
  fOwner := aOwner;
end;


{ TExprParserMatchNode }

type
  TExprParserMatchNode = class(TExprNodeWordAbstract)
  protected
    fMatch: TMatch;
    function ParseWord: TExprParserResult; override;
  end;

  PExprParserMatchNode = ^TExprParserMatchNode;

function TExprParserMatchNode.ParseWord: TExprParserResult;
begin
  fMatch.Prepare(fWord, (fOwner as TExprParserMatch).fCaseSensitive, {reuse=}true);
  result := eprSuccess;
end;


{ TExprParserMatch }

constructor TExprParserMatch.Create(aCaseSensitive: boolean);
begin
  inherited Create;
  fCaseSensitive := aCaseSensitive;
end;

procedure TExprParserMatch.Initialize;
begin
  inherited Initialize;
  fWordClass := TExprParserMatchNode;
end;

function TExprParserMatch.Search(const aText: RawUTF8): boolean;
begin
  result := Search(pointer(aText), length(aText));
end;

var
  // equals 1 for ['0'..'9', 'A'..'Z', 'a'..'z', #$80..#$ff]
  ROUGH_UTF8: TAnsiCharToByte;

function TExprParserMatch.Search(aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  P, PEnd: PUTF8Char;
  n: PtrInt;
  tab: ^TAnsiCharToByte;
begin
  P := aText;
  if (P = nil) or (fWords = nil) then
  begin
    result := false;
    exit;
  end;
  if fMatchedLastSet > 0 then
  begin
    n := fWordCount;
    repeat
      dec(n);
      fWords[n].fFound := false;
    until n = 0;
    fMatchedLastSet := 0;
  end;
  PEnd := P + aTextLen;
  while (P < PEnd) and (fMatchedLastSet < fWordCount) do
  begin
    tab := @ROUGH_UTF8;
    while tab[P^] = 0 do
    begin
      inc(P);
      if P = PEnd then
        break;
    end;
    if P = PEnd then
      break;
    aText := P;
    repeat
      inc(P);
    until (P = PEnd) or (tab[P^] = 0);
    aTextLen := P - aText; // now aText/aTextLen point to a word
    n := fWordCount;
    repeat
      dec(n);
      with TExprParserMatchNode(fWords[n]) do
        if not fFound and fMatch.Match(aText, aTextLen) then
        begin
          fFound := true;
          inc(fMatchedLastSet);
        end;
    until n = 0;
  end;
  result := Execute;
end;



{ ****************** Bloom Filter Probabilistic Index }

{ TSynBloomFilter }

const
  BLOOM_VERSION = 0;
  BLOOM_MAXHASH = 32; // only 7 is needed for 1% false positive ratio

constructor TSynBloomFilter.Create(aSize: integer; aFalsePositivePercent: double);
const
  LN2 = 0.69314718056;
begin
  inherited Create;
  if aSize < 0 then
    fSize := 1000
  else
    fSize := aSize;
  if aFalsePositivePercent <= 0 then
    fFalsePositivePercent := 1
  else if aFalsePositivePercent > 100 then
    fFalsePositivePercent := 100
  else
    fFalsePositivePercent := aFalsePositivePercent;
  // see http://stackoverflow.com/a/22467497
  fBits := Round(-ln(fFalsePositivePercent / 100) * aSize / (LN2 * LN2));
  fHashFunctions := Round(fBits / fSize * LN2);
  if fHashFunctions = 0 then
    fHashFunctions := 1
  else if fHashFunctions > BLOOM_MAXHASH then
    fHashFunctions := BLOOM_MAXHASH;
  Reset;
end;

constructor TSynBloomFilter.Create(const aSaved: RawByteString; aMagic: cardinal);
begin
  inherited Create;
  if not LoadFrom(aSaved, aMagic) then
    raise ESynException.CreateUTF8('%.Create with invalid aSaved content', [self]);
end;

procedure TSynBloomFilter.Insert(const aValue: RawByteString);
begin
  Insert(pointer(aValue), length(aValue));
end;

procedure TSynBloomFilter.Insert(aValue: pointer; aValueLen: integer);
var
  h: integer;
  h1, h2: cardinal; // https://goo.gl/Pls5wi
begin
  if (self = nil) or (aValueLen <= 0) or (fBits = 0) then
    exit;
  h1 := crc32c(0, aValue, aValueLen);
  if fHashFunctions = 1 then
    h2 := 0
  else
    h2 := crc32c(h1, aValue, aValueLen);
  Safe.Lock;
  try
    for h := 0 to fHashFunctions - 1 do
    begin
      SetBitPtr(pointer(fStore), h1 mod fBits);
      inc(h1, h2);
    end;
    inc(fInserted);
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.GetInserted: cardinal;
begin
  Safe.Lock;
  try
    result := fInserted; // Delphi 5 does not support LockedInt64[]
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.MayExist(const aValue: RawByteString): boolean;
begin
  result := MayExist(pointer(aValue), length(aValue));
end;

function TSynBloomFilter.MayExist(aValue: pointer; aValueLen: integer): boolean;
var
  h: integer;
  h1, h2: cardinal; // https://goo.gl/Pls5wi
begin
  result := false;
  if (self = nil) or (aValueLen <= 0) or (fBits = 0) then
    exit;
  h1 := crc32c(0, aValue, aValueLen);
  if fHashFunctions = 1 then
    h2 := 0
  else
    h2 := crc32c(h1, aValue, aValueLen);
  Safe.Lock;
  try
    for h := 0 to fHashFunctions - 1 do
      if GetBitPtr(pointer(fStore), h1 mod fBits) then
        inc(h1, h2)
      else
        exit;
  finally
    Safe.UnLock;
  end;
  result := true;
end;

procedure TSynBloomFilter.Reset;
begin
  Safe.Lock;
  try
    if fStore = '' then
      SetLength(fStore, (fBits shr 3) + 1);
    FillcharFast(pointer(fStore)^, length(fStore), 0);
    fInserted := 0;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.SaveTo(aMagic: cardinal): RawByteString;
var
  W: TFileBufferWriter;
  BufLen: integer;
  temp: array[word] of byte;
begin
  BufLen := length(fStore) + 100;
  if BufLen <= SizeOf(temp) then
    W := TFileBufferWriter.Create(TRawByteStringStream, @temp, SizeOf(temp))
  else
    W := TFileBufferWriter.Create(TRawByteStringStream, BufLen);
  try
    SaveTo(W, aMagic);
    W.Flush;
    result := TRawByteStringStream(W.Stream).DataString;
  finally
    W.Free;
  end;
end;

procedure TSynBloomFilter.SaveTo(aDest: TFileBufferWriter; aMagic: cardinal);
begin
  aDest.Write4(aMagic);
  aDest.Write1(BLOOM_VERSION);
  Safe.Lock;
  try
    aDest.Write8(@fFalsePositivePercent);
    aDest.Write4(fSize);
    aDest.Write4(fBits);
    aDest.Write1(fHashFunctions);
    aDest.Write4(fInserted);
    ZeroCompress(pointer(fStore), Length(fStore), aDest);
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.LoadFrom(const aSaved: RawByteString; aMagic: cardinal): boolean;
begin
  result := LoadFrom(pointer(aSaved), length(aSaved));
end;

function TSynBloomFilter.LoadFrom(P: PByte; PLen: integer; aMagic: cardinal): boolean;
var
  start: PByte;
  version: integer;
begin
  result := false;
  start := P;
  if (P = nil) or (PLen < 32) or (PCardinal(P)^ <> aMagic) then
    exit;
  inc(P, 4);
  version := P^;
  inc(P);
  if version > BLOOM_VERSION then
    exit;
  Safe.Lock;
  try
    fFalsePositivePercent := unaligned(PDouble(P)^);
    inc(P, 8);
    if (fFalsePositivePercent <= 0) or (fFalsePositivePercent > 100) then
      exit;
    fSize := PCardinal(P)^;
    inc(P, 4);
    fBits := PCardinal(P)^;
    inc(P, 4);
    if fBits < fSize then
      exit;
    fHashFunctions := P^;
    inc(P);
    if fHashFunctions - 1 >= BLOOM_MAXHASH then
      exit;
    Reset;
    fInserted := PCardinal(P)^;
    inc(P, 4);
    ZeroDecompress(P, PLen - (PAnsiChar(P) - PAnsiChar(start)), fStore);
    result := length(fStore) = integer(fBits shr 3) + 1;
  finally
    Safe.UnLock;
  end;
end;


{ TSynBloomFilterDiff }

type
  TBloomDiffHeader = packed record
    kind: (bdDiff, bdFull, bdUpToDate);
    size: cardinal;
    inserted: cardinal;
    revision: Int64;
    crc: cardinal;
  end;

procedure TSynBloomFilterDiff.Insert(aValue: pointer; aValueLen: integer);
begin
  Safe.Lock;
  try
    inherited Insert(aValue, aValueLen);
    inc(fRevision);
    inc(fSnapshotInsertCount);
  finally
    Safe.UnLock;
  end;
end;

procedure TSynBloomFilterDiff.Reset;
begin
  Safe.Lock;
  try
    inherited Reset;
    fSnapshotAfterInsertCount := fSize shr 5;
    fSnapShotAfterMinutes := 30;
    fSnapshotTimestamp := 0;
    fSnapshotInsertCount := 0;
    fRevision := UnixTimeUTC shl 31;
    fKnownRevision := 0;
    fKnownStore := '';
  finally
    Safe.UnLock;
  end;
end;

procedure TSynBloomFilterDiff.DiffSnapshot;
begin
  Safe.Lock;
  try
    fKnownRevision := fRevision;
    fSnapshotInsertCount := 0;
    SetString(fKnownStore, PAnsiChar(pointer(fStore)), length(fStore));
    if fSnapShotAfterMinutes = 0 then
      fSnapshotTimestamp := 0
    else
      fSnapshotTimestamp := GetTickCount64 + fSnapShotAfterMinutes * 60000;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilterDiff.SaveToDiff(const aKnownRevision: Int64): RawByteString;
var
  head: TBloomDiffHeader;
  W: TFileBufferWriter;
  temp: array[word] of byte;
begin
  Safe.Lock;
  try
    if aKnownRevision = fRevision then
      head.kind := bdUpToDate
    else if (fKnownRevision = 0) or
            (fSnapshotInsertCount > fSnapshotAfterInsertCount) or
            ((fSnapshotInsertCount > 0) and (fSnapshotTimestamp <> 0) and
             (GetTickCount64 > fSnapshotTimestamp)) then
    begin
      DiffSnapshot;
      head.kind := bdFull;
    end
    else if (aKnownRevision < fKnownRevision) or (aKnownRevision > fRevision) then
      head.kind := bdFull
    else
      head.kind := bdDiff;
    head.size := length(fStore);
    head.inserted := fInserted;
    head.revision := fRevision;
    head.crc := crc32c(0, @head, SizeOf(head) - SizeOf(head.crc));
    if head.kind = bdUpToDate then
    begin
      SetString(result, PAnsiChar(@head), SizeOf(head));
      exit;
    end;
    if head.size + 100 <= SizeOf(temp) then
      W := TFileBufferWriter.Create(TRawByteStringStream, @temp, SizeOf(temp))
    else
      W := TFileBufferWriter.Create(TRawByteStringStream, head.size + 100);
    try
      W.Write(@head, SizeOf(head));
      case head.kind of
        bdFull:
          SaveTo(W);
        bdDiff:
          ZeroCompressXor(pointer(fStore), pointer(fKnownStore), head.size, W);
      end;
      W.Flush;
      result := TRawByteStringStream(W.Stream).DataString;
    finally
      W.Free;
    end;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilterDiff.DiffKnownRevision(const aDiff: RawByteString): Int64;
var
  head: ^TBloomDiffHeader absolute aDiff;
begin
  if (length(aDiff) < SizeOf(head^)) or (head.kind > high(head.kind)) or
     (head.size <> cardinal(length(fStore))) or
     (head.crc <> crc32c(0, pointer(head), SizeOf(head^) - SizeOf(head.crc))) then
    result := 0
  else
    result := head.Revision;
end;

function TSynBloomFilterDiff.LoadFromDiff(const aDiff: RawByteString): boolean;
var
  head: ^TBloomDiffHeader absolute aDiff;
  P: PByte;
  PLen: integer;
begin
  result := false;
  P := pointer(aDiff);
  PLen := length(aDiff);
  if (PLen < SizeOf(head^)) or (head.kind > high(head.kind)) or
     (head.crc <> crc32c(0, pointer(head), SizeOf(head^) - SizeOf(head.crc))) then
    exit;
  if (fStore <> '') and (head.size <> cardinal(length(fStore))) then
    exit;
  inc(P, SizeOf(head^));
  dec(PLen, SizeOf(head^));
  Safe.Lock;
  try
    case head.kind of
      bdFull:
        result := LoadFrom(P, PLen);
      bdDiff:
        if fStore <> '' then
          result := ZeroDecompressOr(pointer(P), Pointer(fStore), PLen, head.size);
      bdUpToDate:
        result := true;
    end;
    if result then
    begin
      fRevision := head.revision;
      fInserted := head.inserted;
    end;
  finally
    Safe.UnLock;
  end;
end;

procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TFileBufferWriter);
var
  PEnd, beg, zero: PAnsiChar;
  crc: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  PEnd := P + Len;
  beg := P;
  crc := 0;
  while P < PEnd do
  begin
    while (P^ <> #0) and (P < PEnd) do
      inc(P);
    zero := P;
    while (P^ = #0) and (P < PEnd) do
      inc(P);
    if P - zero > 3 then
    begin
      Len := zero - beg;
      crc := crc32c(crc, beg, Len);
      Dest.WriteVarUInt32(Len);
      Dest.Write(beg, Len);
      Len := P - zero;
      crc := crc32c(crc, @Len, SizeOf(Len));
      Dest.WriteVarUInt32(Len - 3);
      beg := P;
    end;
  end;
  Len := P - beg;
  if Len > 0 then
  begin
    crc := crc32c(crc, beg, Len);
    Dest.WriteVarUInt32(Len);
    Dest.Write(beg, Len);
  end;
  Dest.Write4(crc);
end;

procedure ZeroCompressXor(New, Old: PAnsiChar; Len: cardinal; Dest: TFileBufferWriter);
var
  beg, same, index, crc, L: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  beg := 0;
  index := 0;
  crc := 0;
  while index < Len do
  begin
    while (New[index] <> Old[index]) and (index < Len) do
      inc(index);
    same := index;
    while (New[index] = Old[index]) and (index < Len) do
      inc(index);
    L := index - same;
    if L > 3 then
    begin
      Dest.WriteVarUInt32(same - beg);
      Dest.WriteXor(New + beg, Old + beg, same - beg, @crc);
      crc := crc32c(crc, @L, SizeOf(L));
      Dest.WriteVarUInt32(L - 3);
      beg := index;
    end;
  end;
  L := index - beg;
  if L > 0 then
  begin
    Dest.WriteVarUInt32(L);
    Dest.WriteXor(New + beg, Old + beg, L, @crc);
  end;
  Dest.Write4(crc);
end;

procedure ZeroDecompress(P: PByte; Len: integer; var Dest: RawByteString);
var
  PEnd, D, DEnd: PAnsiChar;
  DestLen, crc: cardinal;
begin
  PEnd := PAnsiChar(P) + Len - 4;
  DestLen := FromVarUInt32(P);
  SetString(Dest, nil, DestLen); // FPC uses var
  D := pointer(Dest);
  DEnd := D + DestLen;
  crc := 0;
  while PAnsiChar(P) < PEnd do
  begin
    Len := FromVarUInt32(P);
    if D + Len > DEnd then
      break;
    MoveFast(P^, D^, Len);
    crc := crc32c(crc, D, Len);
    inc(P, Len);
    inc(D, Len);
    if PAnsiChar(P) >= PEnd then
      break;
    Len := FromVarUInt32(P) + 3;
    if D + Len > DEnd then
      break;
    FillCharFast(D^, Len, 0);
    crc := crc32c(crc, @Len, SizeOf(Len));
    inc(D, Len);
  end;
  if crc <> PCardinal(P)^ then
    Dest := '';
end;

function ZeroDecompressOr(P, Dest: PAnsiChar; Len, DestLen: integer): boolean;
var
  PEnd, DEnd: PAnsiChar;
  crc: cardinal;
begin
  PEnd := P + Len - 4;
  if cardinal(DestLen) <> FromVarUInt32(PByte(P)) then
  begin
    result := false;
    exit;
  end;
  DEnd := Dest + DestLen;
  crc := 0;
  while (P < PEnd) and (Dest < DEnd) do
  begin
    Len := FromVarUInt32(PByte(P));
    if Dest + Len > DEnd then
      break;
    crc := crc32c(crc, P, Len);
    OrMemory(pointer(Dest), pointer(P), Len);
    inc(P, Len);
    inc(Dest, Len);
    if P >= PEnd then
      break;
    Len := FromVarUInt32(PByte(P)) + 3;
    crc := crc32c(crc, @Len, SizeOf(Len));
    inc(Dest, Len);
  end;
  result := crc = PCardinal(P)^;
end;

function Max(a, b: PtrInt): PtrInt; {$ifdef HASINLINE} inline;{$endif}
begin
  if a > b then
    result := a
  else
    result := b;
end;

function Min(a, b: PtrInt): PtrInt; {$ifdef HASINLINE} inline;{$endif}
begin
  if a < b then
    result := a
  else
    result := b;
end;

{$ifdef HASINLINE}
function Comp(a, b: PAnsiChar; len: PtrInt): PtrInt; inline;
var
  lenptr: PtrInt;
begin
  result := 0;
  lenptr := len - SizeOf(PtrInt);
  if lenptr >= 0 then
    repeat
      if PPtrInt(a + result)^ <> PPtrInt(b + result)^ then
        break;
      inc(result, SizeOf(PtrInt));
    until result > lenptr;
  if result < len then
    repeat
      if a[result] <> b[result] then
        exit;
      inc(result);
    until result = len;
end;
{$else} // eax = a, edx = b, ecx = len
function Comp(a, b: PAnsiChar; len: PtrInt): PtrInt;
asm // the 'rep cmpsb' version is slower on Intel Core CPU (not AMD)
        OR      ecx, ecx
        push    ebx
        push    ecx
        jz      @ok

@1:     mov     bx, [eax]
        lea     eax, [eax + 2]
        cmp     bl, [edx]
        jne     @ok
        dec     ecx
        jz      @ok
        cmp     bh, [edx + 1]
        lea     edx, [edx + 2]
        jne     @ok
        dec     ecx
        jnz     @1

@ok:    pop     eax
        sub     eax, ecx
        pop     ebx
end;
{$endif HASINLINE}

function CompReverse(a, b: pointer; len: PtrInt): PtrInt;
begin
  result := 0;
  if len > 0 then
    repeat
      if PByteArray(a)[-result] <> PByteArray(b)[-result] then
        exit;
      inc(result);
    until result = len;
end;

function WriteCurOfs(curofs, curlen, curofssize: integer; sp: PAnsiChar): PAnsiChar;
begin
  if curlen = 0 then
  begin
    sp^ := #0;
    inc(sp);
  end
  else
  begin
    sp := Pointer(ToVarUInt32(curlen, PByte(sp)));
    PInteger(sp)^ := curofs;
    inc(sp, curofssize);
  end;
  result := sp;
end;

{$ifdef CPUINTEL} // crc32c SSE4.2 hardware accellerated dword hash
function crc32csse42(buf: pointer): cardinal;
{$ifdef CPUX86}
asm
        mov     edx, eax
        xor     eax, eax
        {$ifdef ISDELPHI2010}
        crc32   eax, dword ptr[edx]
        {$else}
        db $F2, $0F, $38, $F1, $02
        {$endif}
end;
{$else} {$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        xor     eax, eax
        crc32   eax, dword ptr[buf]
end;
{$endif CPUX86}
{$endif CPUINTEL}

function hash32prime(buf: pointer): cardinal;
begin // xxhash32-inspired - and won't pollute L1 cache with lookup tables
  result := PCardinal(buf)^;
  result := result xor (result shr 15);
  result := result * 2246822519;
  result := result xor (result shr 13);
  result := result * 3266489917;
  result := result xor (result shr 16);
end;

const
  HTabBits = 18; // fits well with DeltaCompress(..,BufSize=2MB)
  HTabMask = (1 shl HTabBits) - 1; // =$3ffff
  HListMask = $ffffff; // HTab[]=($ff,$ff,$ff)

type
  PHTab = ^THTab; // HTabBits=18 -> SizeOf=767KB
  THTab = packed array[0..HTabMask] of array[0..2] of byte;

function DeltaCompute(NewBuf, OldBuf, OutBuf, WorkBuf: PAnsiChar;
  NewBufSize, OldBufSize, MaxLevel: PtrInt; HList, HTab: PHTab): PAnsiChar;
var
  i, curofs, curlen, curlevel, match, curofssize, h, oldh: PtrInt;
  sp, pInBuf, pOut: PAnsiChar;
  ofs: cardinal;
  spb: PByte absolute sp;
  hash: function(buf: pointer): cardinal;
begin
  // 1. fill HTab[] with hashes for all old data
  {$ifdef CPUINTEL}
  if cfSSE42 in CpuFeatures then
    hash := @crc32csse42
  else
  {$endif CPUINTEL}
    hash := @hash32prime;
  FillCharFast(HTab^, SizeOf(HTab^), $ff); // HTab[]=HListMask by default
  pInBuf := OldBuf;
  oldh := -1; // force calculate first hash
  sp := pointer(HList);
  for i := 0 to OldBufSize - 3 do
  begin
    h := hash(pInBuf) and HTabMask;
    inc(pInBuf);
    if h = oldh then
      continue;
    oldh := h;
    h := PtrInt(@HTab^[h]); // fast 24-bit data process
    PCardinal(sp)^ := PCardinal(h)^;
    PCardinal(h)^ := cardinal(i) or (PCardinal(h)^ and $ff000000);
    inc(sp, 3);
  end;
  // 2. compression init
  if OldBufSize <= $ffff then
    curofssize := 2
  else
    curofssize := 3;
  curlen := -1;
  curofs := 0;
  pOut := OutBuf + 7;
  sp := WorkBuf;
  // 3. handle identical leading bytes
  match := Comp(OldBuf, NewBuf, Min(OldBufSize, NewBufSize));
  if match > 2 then
  begin
    sp := WriteCurOfs(0, match, curofssize, sp);
    sp^ := #0;
    inc(sp);
    inc(NewBuf, match);
    dec(NewBufSize, match);
  end;
  pInBuf := NewBuf;
  // 4. main loop: identify longest sequences using hash, and store reference
  if NewBufSize >= 8 then
    repeat
    // hash 4 next bytes from NewBuf, and find longest match in OldBuf
      ofs := PCardinal(@HTab^[hash(NewBuf) and HTabMask])^ and HListMask;
      if ofs <> HListMask then
      begin // brute force search loop of best hash match
        curlevel := MaxLevel;
        repeat
          with PHash128Rec(OldBuf + ofs)^ do
            // always test 8 bytes at once
            {$ifdef CPU64}
            if PHash128Rec(NewBuf)^.Lo=Lo then
            {$else}
            if (PHash128Rec(NewBuf)^.c0 = c0) and
               (PHash128Rec(NewBuf)^.c1 = c1) then
            {$endif CPU64}
            begin
              // test remaining bytes
              match := Comp(@PHash128Rec(NewBuf)^.c2, @c2,
                         Min(PtrUInt(OldBufSize) - ofs, NewBufSize) - 8);
              if match > curlen then
              begin // found a longer sequence
                curlen := match;
                curofs := ofs;
              end;
            end;
          dec(curlevel);
          ofs := PCardinal(@HList^[ofs])^ and HListMask;
        until (ofs = HListMask) or (curlevel = 0);
      end;
      // curlen = longest sequence length
      if curlen < 0 then
      begin
       // no sequence found -> copy one byte
        dec(NewBufSize);
        pOut^ := NewBuf^;
        inc(NewBuf);
        inc(pOut);
        if NewBufSize > 8 then // >=8 may overflow
          continue
        else
          break;
      end;
      inc(curlen, 8);
      sp := WriteCurOfs(curofs, curlen, curofssize, sp);
      spb := ToVarUInt32(NewBuf - pInBuf, spb);
      inc(NewBuf, curlen); // continue to search after the sequence
      dec(NewBufSize, curlen);
      curlen := -1;
      pInBuf := NewBuf;
      if NewBufSize > 8 then // >=8 may overflow
        continue
      else
        break;
    until false;
  // 5. write remaining bytes
  if NewBufSize > 0 then
  begin
    MoveFast(NewBuf^, pOut^, NewBufSize);
    inc(pOut, NewBufSize);
    inc(NewBuf, NewBufSize);
  end;
  sp^ := #0;
  inc(sp);
  spb := ToVarUInt32(NewBuf - pInBuf, spb);
  // 6. write header
  PInteger(OutBuf)^ := pOut - OutBuf - 7;
  h := sp - WorkBuf;
  PInteger(OutBuf + 3)^ := h;
  OutBuf[6] := AnsiChar(curofssize);
  // 7. copy commands
  MoveFast(WorkBuf^, pOut^, h);
  result := pOut + h;
end;

function ExtractBuf(GoodCRC: cardinal; p: PAnsiChar;
  var aUpd, Delta: PAnsiChar; Old: PAnsiChar): TDeltaError;
var
  pEnd, buf, upd, src: PAnsiChar;
  bufsize, datasize, leading, srclen: PtrUInt;
  curofssize: byte;
begin
  // 1. decompression init
  upd := aUpd;
  bufsize := PCardinal(p)^ and $00ffffff;
  inc(p, 3);
  datasize := PCardinal(p)^ and $00ffffff;
  inc(p, 3);
  curofssize := ord(p^);
  inc(p);
  buf := p;
  inc(p, bufsize);
  pEnd := p + datasize;
  src := nil;
  // 2. main loop
  while p < pEnd do
  begin
    // src/srclen = sequence to be copied
    srclen := FromVarUInt32(PByte(p));
    if srclen > 0 then
      if curofssize = 2 then
      begin
        src := Old + PWord(p)^;
        inc(p, 2);
      end
      else
      begin
        src := Old + PCardinal(p)^ and $00ffffff;
        inc(p, 3);
      end;
    // copy leading bytes
    leading := FromVarUInt32(PByte(p));
    if leading <> 0 then
    begin
      MoveFast(buf^, upd^, leading);
      inc(buf, leading);
      inc(upd, leading);
    end;
    // copy sequence
    if srclen <> 0 then
    begin
      if PtrUInt(upd - src) < srclen then
        MoveSmall(src, upd, srclen)
      else
        MoveFast(src^, upd^, srclen);
      inc(upd, srclen);
    end;
  end;
  // 3. result check
  Delta := p;
  if (p = pEnd) and (crc32c(0, aUpd, upd - aUpd) = GoodCRC) then
    // whole CRC is faster than incremental
    result := dsSuccess
  else
    result := dsCrcExtract;
  aUpd := upd;
end;

procedure WriteByte(var P: PAnsiChar; V: Byte);
  {$ifdef HASINLINE} inline;{$endif}
begin
  PByte(P)^ := V;
  inc(P);
end;

procedure WriteInt(var P: PAnsiChar; V: Cardinal);
  {$ifdef HASINLINE} inline;{$endif}
begin
  PCardinal(P)^ := V;
  inc(P, 4);
end;

const
  FLAG_COPIED = 0;
  FLAG_COMPRESS = 1;
  FLAG_BEGIN = 2;
  FLAG_END = 3;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level, BufSize: integer): integer;
var
  HTab, HList: PHTab;
  d, workbuf: PAnsiChar;
  db: PByte absolute d;
  BufRead, OldRead, Trailing, NewSizeSave: PtrInt;
  bigfile: boolean;

  procedure CreateCopied;
  begin
    Getmem(Delta, NewSizeSave + 17);  // 17 = 4*Integer + 1*Byte
    d := Delta;
    db := ToVarUInt32(0, ToVarUInt32(NewSizeSave, db));
    WriteByte(d, FLAG_COPIED); // block copied flag
    db := ToVarUInt32(NewSizeSave, db);
    WriteInt(d, crc32c(0, New, NewSizeSave));
    MoveFast(New^, d^, NewSizeSave);
    inc(d, NewSizeSave);
    result := d - Delta;
  end;

begin
  // 1. special cases
  if (NewSize = OldSize) and CompareMem(Old, New, NewSize) then
  begin
    Getmem(Delta, 1);
    Delta^ := '=';
    result := 1;
    exit;
  end;
  NewSizeSave := NewSize;
  if OldSize = 0 then
  begin // Delta from nothing -> direct copy of whole block
    CreateCopied;
    exit;
  end;
  // 2. compression init
  bigfile := OldSize > BufSize;
  if BufSize > NewSize then
    BufSize := NewSize;
  if BufSize > $ffffff then
    BufSize := $ffffff; // we store offsets with 2..3 bytes -> max 16MB chunk
  Trailing := 0;
  Getmem(workbuf, BufSize); // compression temporary buffers
  Getmem(HList, BufSize * SizeOf({%H-}HList[0]));
  Getmem(HTab, SizeOf({%H-}HTab^));
  Getmem(Delta, Max(NewSize, OldSize) + 4096); // Delta size max evalulation
  try
    d := Delta;
    db := ToVarUInt32(NewSize, db); // Destination Size
    // 3. handle leading and trailing identical bytes (for biggest files)
    if bigfile then
    begin
      // test initial same chars
      BufRead := Comp(New, Old, Min(NewSize, OldSize));
      if BufRead > 9 then
      begin
        // it happens very often: modification is usually in the middle/end
        db := ToVarUInt32(BufRead, db); // blockSize = Size BufIdem
        WriteByte(d, FLAG_BEGIN);
        WriteInt(d, crc32c(0, New, BufRead));
        inc(New, BufRead);
        dec(NewSize, BufRead);
        inc(Old, BufRead);
        dec(OldSize, BufRead);
      end;
      // test trailing same chars
      BufRead := CompReverse(New + NewSize - 1, Old + OldSize - 1,
        Min(NewSize, OldSize));
      if BufRead > 5 then
      begin
        if NewSize = BufRead then
          dec(BufRead); // avoid block overflow
        dec(OldSize, BufRead);
        dec(NewSize, BufRead);
        Trailing := BufRead;
      end;
    end;
    // 4. main loop
    repeat
      BufRead := Min(BufSize, NewSize);
      dec(NewSize, BufRead);
      if (BufRead = 0) and (Trailing > 0) then
      begin
        db := ToVarUInt32(Trailing, db);
        WriteByte(d, FLAG_END); // block idem end flag
        WriteInt(d, crc32c(0, New, Trailing));
        break;
      end;
      OldRead := Min(BufSize, OldSize);
      dec(OldSize, OldRead);
      db := ToVarUInt32(OldRead, db);
      if (BufRead < 4) or (OldRead < 4) or (BufRead div 4 > OldRead) then
      begin
        WriteByte(d, FLAG_COPIED); // block copied flag
        db := ToVarUInt32(BufRead, db);
        if BufRead = 0 then
          break;
        WriteInt(d, crc32c(0, New, BufRead));
        MoveFast(New^, d^, BufRead);
        inc(New, BufRead);
        inc(d, BufRead);
      end
      else
      begin
        WriteByte(d, FLAG_COMPRESS); // block compressed flag
        WriteInt(d, crc32c(0, New, BufRead));
        WriteInt(d, crc32c(0, Old, OldRead));
        d := DeltaCompute(New, Old, d, workbuf, BufRead, OldRead, Level, HList, HTab);
        inc(New, BufRead);
        inc(Old, OldRead);
      end;
    until false;
  // 5. release temp memory
  finally
    result := d - Delta;
    Freemem(HTab);
    Freemem(HList);
    Freemem(workbuf);
  end;
  if result >= NewSizeSave + 17 then
  begin
    // Delta didn't compress well -> store it (with up to 17 bytes overhead)
    Freemem(Delta);
    CreateCopied;
  end;
end;

function DeltaCompress(const New, Old: RawByteString;
  Level, BufSize: integer): RawByteString;
begin
  result := DeltaCompress(pointer(New), pointer(Old),
    length(New), length(Old), Level, BufSize);
end;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize,
  Level, BufSize: integer): RawByteString;
var
  Delta: PAnsiChar;
  DeltaLen: integer;
begin
  DeltaLen := DeltaCompress(New, Old, NewSize, OldSize, Delta, Level, BufSize);
  SetString(result, Delta, DeltaLen);
  Freemem(Delta);
end;

function DeltaExtract(Delta, Old, New: PAnsiChar): TDeltaError;
var
  BufCRC: Cardinal;
  Code: Byte;
  Len, BufRead, OldRead: PtrInt;
  db: PByte absolute Delta;
  Upd: PAnsiChar;
begin
  Result := dsSuccess;
  Len := FromVarUInt32(db);
  Upd := New;
  repeat
    OldRead := FromVarUInt32(db);
    Code := db^;
    inc(db);
    case Code of
      FLAG_COPIED:
        begin
          // block copied flag - copy new from Delta
          BufRead := FromVarUInt32(db);
          if BufRead = 0 then
            break;
          if crc32c(0, Delta + 4, BufRead) <> PCardinal(Delta)^ then
          begin
            result := dsCrcCopy;
            exit;
          end;
          inc(Delta, 4);
          MoveFast(Delta^, New^, BufRead);
          if BufRead >= Len then
            exit; // if Old=nil -> only copy new
          inc(Delta, BufRead);
          inc(New, BufRead);
        end;
      FLAG_COMPRESS:
        begin
          // block compressed flag - extract Delta from Old
          BufCRC := PCardinal(Delta)^;
          inc(Delta, 4);
          if crc32c(0, Old, OldRead) <> PCardinal(Delta)^ then
          begin
            result := dsCrcComp;
            exit;
          end;
          inc(Delta, 4);
          result := ExtractBuf(BufCRC, Delta, New, Delta, Old);
          if result <> dsSuccess then
            exit;
        end;
      FLAG_BEGIN:
        begin
          // block idem begin flag
          if crc32c(0, Old, OldRead) <> PCardinal(Delta)^ then
          begin
            result := dsCrcBegin;
            exit;
          end;
          inc(Delta, 4);
          MoveFast(Old^, New^, OldRead);
          inc(New, OldRead);
        end;
      FLAG_END:
        begin
          // block idem end flag
          if crc32c(0, Old, OldRead) <> PCardinal(Delta)^ then
            Result := dsCrcEnd;
          MoveFast(Old^, New^, OldRead);
          inc(New, OldRead);
          break;
        end;
    else
      begin
        result := dsFlag;
        exit;
      end;
    end; // Case Code of
    inc(Old, OldRead);
  until false;
  if New - Upd <> Len then
    result := dsLen;
end;

function DeltaExtract(const Delta, Old: RawByteString;
  out New: RawByteString): TDeltaError;
begin
  if (Delta = '') or (Delta = '=') then
  begin
    New := Old;
    result := dsSuccess;
  end
  else
  begin
    SetLength(New, DeltaExtractSize(pointer(Delta)));
    result := DeltaExtract(pointer(Delta), pointer(Old), pointer(New));
  end;
end;

function DeltaExtractSize(const Delta: RawByteString): integer;
begin
  result := DeltaExtractSize(pointer(Delta));
end;

function DeltaExtractSize(Delta: PAnsiChar): integer;
begin
  if Delta = nil then
    result := 0
  else
    result := FromVarUInt32(PByte(Delta));
end;

function ToText(err: TDeltaError): PShortString;
begin
  result := GetEnumName(TypeInfo(TDeltaError), ord(err));
end;



procedure InitializeUnit;
var
  c: AnsiChar;
begin
  for c := low(c) to high(c) do
    if c in ['0'..'9', 'A'..'Z', 'a'..'z', #$80..#$ff] then
      ROUGH_UTF8[c] := 1;
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
end.
