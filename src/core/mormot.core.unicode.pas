/// Framework Core Low-Level Unicode UTF-8 UTF-16 Ansi Conversion
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.unicode;

{
  *****************************************************************************

   Efficient Unicode Conversion Classes shared by all framework units
   - UTF-8 Efficient Encoding / Decoding
   - UTF-8 / UTF-16 / Ansi Conversion Classes
   - Low-Level String Conversion Functions
   - Text Case-(in)sensitive Conversion and Comparison
   - Operating-System Independent Unicode Process

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os;


{ *************** UTF-8 Efficient Encoding / Decoding }

// some constants used for UTF-8 conversion, including surrogates
type
  // see http://floodyberry.wordpress.com/2007/04/14/utf-8-conversion-tricks
  TUtf8Table = object
    Bytes: array[$80..$ff] of byte;
    Extra: array[0..6] of record
      offset, minimum: cardinal;
    end;
    FirstByte: array[2..6] of byte;
    /// UTF-8 encode one UCS4 CodePoint into Dest
    function Ucs4ToUtf8(ucs4: Ucs4CodePoint; Dest: PUtf8Char): PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a >127 UCS4 CodePoint from UTF-8
    function GetHighUtf8Ucs4(var U: PUtf8Char): Ucs4CodePoint;
      {$ifdef HASINLINE}inline;{$endif}
  end;
  PUtf8Table = ^TUtf8Table;

const
  UTF8_TABLE: TUtf8Table = (
    Bytes: (
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 0, 0);
    Extra: (
      (offset: $00000000;  minimum: $00010000),
      (offset: $00003080;  minimum: $00000080),
      (offset: $000e2080;  minimum: $00000800),
      (offset: $03c82080;  minimum: $00010000),
      (offset: $fa082080;  minimum: $00200000),
      (offset: $82082080;  minimum: $04000000),
      (offset: $00000000;  minimum: $04000000));
    FirstByte: (
      $c0, $e0, $f0, $f8, $fc));

  UTF8_EXTRA_SURROGATE = 3;
  UTF16_HISURROGATE_MIN = $d800;
  UTF16_HISURROGATE_MAX = $dbff;
  UTF16_LOSURROGATE_MIN = $dc00;
  UTF16_LOSURROGATE_MAX = $dfff;

  /// replace any incoming character whose value is unrepresentable in Unicode
  // - set e.g. by GetUtf8WideChar(), Utf8UpperReference() or
  // RawUnicodeToUtf8() when ccfReplacementCharacterForUnmatchedSurrogate is set
  // - encoded as $ef $bf $bd bytes in UTF-8
  UNICODE_REPLACEMENT_CHARACTER = $fffd;


/// internal function, used to retrieve a >127 US4 CodePoint from UTF-8
// - not to be called directly, but from inlined higher-level functions
// - here U^ shall be always >= #80
// - typical use is as such:
// !  ch := ord(P^);
// !  if ch and $80=0 then
// !    inc(P) else
// !    ch := GetHighUtf8Ucs4(P);
function GetHighUtf8Ucs4(var U: PUtf8Char): Ucs4CodePoint;

/// decode UTF-16 WideChar from UTF-8 input buffer
// - any surrogate (Ucs4>$ffff) is returned as UNICODE_REPLACEMENT_CHARACTER=$fffd
function GetUtf8WideChar(P: PUtf8Char): cardinal;
  {$ifdef HASINLINE}inline;{$endif}

/// get the UCS4 CodePoint stored in P^ (decode UTF-8 if necessary)
function NextUtf8Ucs4(var P: PUtf8Char): Ucs4CodePoint;
  {$ifdef HASINLINE}inline;{$endif}

/// UTF-8 encode one UTF-16 character into Dest
// - return the number of bytes written into Dest (i.e. 1,2 or 3)
// - this method does NOT handle UTF-16 surrogate pairs
function WideCharToUtf8(Dest: PUtf8Char; aWideChar: PtrUInt): integer;
  {$ifdef HASINLINE}inline;{$endif}

 /// UTF-8 encode one UTF-16 encoded UCS4 CodePoint into Dest
// - return the number of bytes written into Dest (i.e. from 1 up to 6)
// - Source will contain the next UTF-16 character
// - this method DOES handle UTF-16 surrogate pairs
function Utf16CharToUtf8(Dest: PUtf8Char; var Source: PWord): integer;

/// UTF-8 encode one UCS4 CodePoint into Dest
// - return the number of bytes written into Dest (i.e. from 1 up to 6)
// - this method DOES handle UTF-16 surrogate pairs
function Ucs4ToUtf8(ucs4: Ucs4CodePoint; Dest: PUtf8Char): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

type
  /// option set for RawUnicodeToUtf8() conversion
  TCharConversionFlags = set of (
    ccfNoTrailingZero,
    ccfReplacementCharacterForUnmatchedSurrogate);

/// convert a RawUnicode PWideChar into a UTF-8 string
procedure RawUnicodeToUtf8(WideChar: PWideChar; WideCharCount: integer;
  var result: RawUtf8; Flags: TCharConversionFlags = [ccfNoTrailingZero]); overload;

/// convert a RawUnicode PWideChar into a UTF-8 string
function RawUnicodeToUtf8(WideChar: PWideChar; WideCharCount: integer;
  Flags: TCharConversionFlags = [ccfNoTrailingZero]): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUnicode UTF-16 PWideChar into a UTF-8 buffer
// - replace system.UnicodeToUtf8 implementation, which is rather slow
// since Delphi 2009+
// - append a trailing #0 to the ending PUtf8Char, unless ccfNoTrailingZero is set
// - if ccfReplacementCharacterForUnmatchedSurrogate is set, this function will identify
// unmatched surrogate pairs and replace them with UNICODE_REPLACEMENT_CHARACTER -
// see https://en.wikipedia.org/wiki/Specials_(Unicode_block)
function RawUnicodeToUtf8(Dest: PUtf8Char; DestLen: PtrInt;
  Source: PWideChar; SourceLen: PtrInt; Flags: TCharConversionFlags): PtrInt; overload;

/// convert a RawUnicode PWideChar into a UTF-8 string
// - this version doesn't resize the resulting RawUtf8 string, but return
// the new resulting RawUtf8 byte count into Utf8Length
function RawUnicodeToUtf8(WideChar: PWideChar; WideCharCount: integer;
  out Utf8Length: integer): RawUtf8; overload;


/// convert an UTF-8 encoded text into a WideChar (UTF-16) buffer
// - faster than System.Utf8ToUnicode
// - sourceBytes can by 0, therefore length is computed from zero terminated source
// - enough place must be available in dest buffer (guess is sourceBytes*3+2)
// - a WideChar(#0) is added at the end (if something is written) unless
// NoTrailingZero is TRUE
// - returns the BYTE count written in dest, excluding the ending WideChar(#0)
function Utf8ToWideChar(dest: PWideChar; source: PUtf8Char; sourceBytes: PtrInt = 0;
  NoTrailingZero: boolean = false): PtrInt; overload;

/// convert an UTF-8 encoded text into a WideChar (UTF-16) buffer
// - faster than System.Utf8ToUnicode
// - this overloaded function expect a MaxDestChars parameter
// - sourceBytes can not be 0 for this function
// - enough place must be available in dest buffer (guess is sourceBytes*3+2)
// - a WideChar(#0) is added at the end (if something is written) unless
// NoTrailingZero is TRUE
// - returns the BYTE COUNT (not WideChar count) written in dest, excluding the
// ending WideChar(#0)
function Utf8ToWideChar(dest: PWideChar; source: PUtf8Char;
  MaxDestChars, sourceBytes: PtrInt; NoTrailingZero: boolean = false): PtrInt; overload;

/// direct conversion of a UTF-8 encoded buffer into a WinAnsi shortstring buffer
// - non WinAnsi chars are replaced by '?' placeholders
procedure Utf8ToShortString(var dest: shortstring; source: PUtf8Char);

/// calculate the UTF-16 Unicode characters count, UTF-8 encoded in source^
// - count may not match the UCS4 CodePoint, in case of UTF-16 surrogates
// - faster than System.Utf8ToUnicode with dest=nil
function Utf8ToUnicodeLength(source: PUtf8Char): PtrUInt;

/// returns TRUE if the supplied buffer has valid UTF-8 encoding
// - will stop when the buffer contains #0
function IsValidUtf8(source: PUtf8Char): boolean; overload;

/// returns TRUE if the supplied buffer has valid UTF-8 encoding
// - will also refuse #0 characters within the buffer
function IsValidUtf8(source: PUtf8Char; sourcelen: PtrInt): boolean; overload;

/// returns TRUE if the supplied buffer has valid UTF-8 encoding
// - will also refuse #0 characters within the buffer
function IsValidUtf8(const source: RawUtf8): boolean; overload;

/// returns TRUE if the supplied buffer has valid UTF-8 encoding with no #1..#31
// control characters
// - supplied input is a pointer to a #0 ended text buffer
function IsValidUtf8WithoutControlChars(source: PUtf8Char): boolean; overload;

/// returns TRUE if the supplied buffer has valid UTF-8 encoding with no #0..#31
// control characters
// - supplied input is a RawUtf8 variable
function IsValidUtf8WithoutControlChars(const source: RawUtf8): boolean; overload;

/// will truncate the supplied UTF-8 value if its length exceeds the specified
// UTF-16 Unicode characters count
// - count may not match the UCS4 CodePoint, in case of UTF-16 surrogates
// - returns FALSE if text was not truncated, TRUE otherwise
function Utf8TruncateToUnicodeLength(var text: RawUtf8; maxUtf16: integer): boolean;

/// will truncate the supplied UTF-8 value if its length exceeds the specified
// bytes count
// - this function will ensure that the returned content will contain only valid
// UTF-8 sequence, i.e. will trim the whole trailing UTF-8 sequence
// - returns FALSE if text was not truncated, TRUE otherwise
function Utf8TruncateToLength(var text: RawUtf8; maxBytes: PtrUInt): boolean;

/// compute the truncated length of the supplied UTF-8 value if it exceeds the
// specified bytes count
// - this function will ensure that the returned content will contain only valid
// UTF-8 sequence, i.e. will trim the whole trailing UTF-8 sequence
// - returns maxBytes if text was not truncated, or the number of fitting bytes
function Utf8TruncatedLength(const text: RawUtf8; maxBytes: PtrUInt): PtrInt; overload;

/// compute the truncated length of the supplied UTF-8 value if it exceeds the
// specified bytes count
// - this function will ensure that the returned content will contain only valid
// UTF-8 sequence, i.e. will trim the whole trailing UTF-8 sequence
// - returns maxBytes if text was not truncated, or the number of fitting bytes
function Utf8TruncatedLength(text: PAnsiChar;
  textlen, maxBytes: PtrUInt): PtrInt; overload;

/// calculate the UTF-16 Unicode characters count of the UTF-8 encoded first line
// - count may not match the UCS4 CodePoint, in case of UTF-16 surrogates
// - end the parsing at first #13 or #10 character
function Utf8FirstLineToUtf16Length(source: PUtf8Char): PtrInt;



{ **************** UTF-8 / Unicode / Ansi Conversion Classes }

type
  /// Exception raised by this unit in case of fatal conversion issue
  ESynUnicode = class(Exception);

  /// an abstract class to handle Ansi to/from Unicode translation
  // - implementations of this class will handle efficiently all Code Pages
  // - this default implementation will use the Operating System APIs
  // - you should not create your own class instance by yourself, but should
  // better retrieve an instance using TSynAnsiConvert.Engine(), which will
  // initialize either a TSynAnsiFixedWidth or a TSynAnsiConvert instance on need
  TSynAnsiConvert = class
  protected
    fCodePage: cardinal;
    fAnsiCharShift: byte;
  public
    /// initialize the internal conversion engine
    constructor Create(aCodePage: cardinal); reintroduce; virtual;
    /// returns the engine corresponding to a given code page
    // - a global list of TSynAnsiConvert instances is handled by the unit -
    // therefore, caller should not release the returned instance
    // - will return nil in case of unhandled code page
    // - is aCodePage is 0, will return CurrentAnsiConvert value
    class function Engine(aCodePage: cardinal): TSynAnsiConvert;
    /// direct conversion of a PAnsiChar buffer into an Unicode buffer
    // - Dest^ buffer must be reserved with at least SourceChars*2 bytes
    // - this default implementation will use the Operating System APIs
    // - will append a trailing #0 to the returned PWideChar, unless
    // NoTrailingZero is set
    function AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PWideChar; overload; virtual;
    /// direct conversion of a PAnsiChar buffer into a UTF-8 encoded buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - will append a trailing #0 to the returned PUtf8Char, unless
    // NoTrailingZero is set
    // - this default implementation will use the Operating System APIs
    function AnsiBufferToUtf8(Dest: PUtf8Char; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PUtf8Char; overload; virtual;
    /// convert any Ansi Text into an UTF-16 Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(const AnsiText: RawByteString): RawUnicode; overload;
    /// convert any Ansi buffer into an Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(
      Source: PAnsiChar; SourceChars: cardinal): RawUnicode; overload; virtual;
    /// convert any Ansi buffer into an Unicode String
    // - returns a SynUnicode, i.e. Delphi 2009+ UnicodeString or a WideString
    function AnsiToUnicodeString(
      Source: PAnsiChar; SourceChars: cardinal): SynUnicode; overload;
    /// convert any Ansi buffer into an Unicode String
    // - returns a SynUnicode, i.e. Delphi 2009+ UnicodeString or a WideString
    function AnsiToUnicodeString(const Source: RawByteString): SynUnicode; overload;
    /// convert any Ansi Text into an UTF-8 encoded String
    // - internally calls AnsiBufferToUtf8 virtual method
    function AnsiToUtf8(const AnsiText: RawByteString): RawUtf8; virtual;
    /// direct conversion of a PAnsiChar buffer into a UTF-8 encoded string
    // - will call AnsiBufferToUnicode() overloaded virtual method
    function AnsiBufferToRawUtf8(Source: PAnsiChar;
      SourceChars: cardinal): RawUtf8; overload; virtual;
    /// direct conversion of an Unicode buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - this default implementation will rely on the Operating System for
    // all non ASCII-7 chars
    function UnicodeBufferToAnsi(Dest: PAnsiChar; Source: PWideChar;
      SourceChars: cardinal): PAnsiChar; overload; virtual;
    /// direct conversion of an Unicode buffer into an Ansi Text
    function UnicodeBufferToAnsi(Source: PWideChar;
      SourceChars: cardinal): RawByteString; overload; virtual;
    /// convert any Unicode-encoded String into Ansi Text
    // - internally calls UnicodeBufferToAnsi virtual method
    function RawUnicodeToAnsi(const Source: RawUnicode): RawByteString;
    /// direct conversion of an UTF-8 encoded buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars bytes
    // - no trailing #0 is appended to the buffer
    function Utf8BufferToAnsi(Dest: PAnsiChar; Source: PUtf8Char;
      SourceChars: cardinal): PAnsiChar; overload; virtual;
    /// convert any UTF-8 encoded buffer into Ansi Text
    // - internally calls Utf8BufferToAnsi virtual method
    function Utf8BufferToAnsi(Source: PUtf8Char;
      SourceChars: cardinal): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert any UTF-8 encoded buffer into Ansi Text
    // - internally calls Utf8BufferToAnsi virtual method
    procedure Utf8BufferToAnsi(Source: PUtf8Char;
      SourceChars: cardinal; var result: RawByteString); overload; virtual;
    /// convert any UTF-8 encoded String into Ansi Text
    // - internally calls Utf8BufferToAnsi virtual method
    function Utf8ToAnsi(const u: RawUtf8): RawByteString; virtual;
    /// direct conversion of a UTF-8 encoded string into a WinAnsi buffer
    // - will truncate the destination string to DestSize bytes (including the
    // trailing #0), with a maximum handled size of 2048 bytes
    // - returns the number of bytes stored in Dest^ (i.e. the position of #0)
    function Utf8ToAnsiBuffer(const S: RawUtf8;
      Dest: PAnsiChar; DestSize: integer): integer;
    /// convert any Ansi Text (providing a From converted) into Ansi Text
    function AnsiToAnsi(From: TSynAnsiConvert;
      const Source: RawByteString): RawByteString; overload;
    /// convert any Ansi buffer (providing a From converted) into Ansi Text
    function AnsiToAnsi(From: TSynAnsiConvert; Source: PAnsiChar;
      SourceChars: cardinal): RawByteString; overload;
    /// corresponding code page
    property CodePage: cardinal
      read fCodePage;
    /// corresponding length binary shift used for worst conversion case
    property AnsiCharShift: byte
      read fAnsiCharShift;
  end;

  /// a class to handle Ansi to/from Unicode translation of fixed width encoding
  // (i.e. non MBCS)
  // - this class will handle efficiently all Code Page availables without MBCS
  // encoding - like WinAnsi (1252) or Russian (1251)
  // - it will use internal fast look-up tables for such encodings
  // - this class could take some time to generate, and will consume more than
  // 64 KB of memory: you should not create your own class instance by yourself,
  // but should better retrieve an instance using TSynAnsiConvert.Engine(), which
  // will initialize either a TSynAnsiFixedWidth or a TSynAnsiConvert instance
  // on need
  // - this class has some additional methods (e.g. IsValid*) which take
  // advantage of the internal lookup tables to provide some fast process
  TSynAnsiFixedWidth = class(TSynAnsiConvert)
  protected
    fAnsiToWide: TWordDynArray;
    fWideToAnsi: TByteDynArray;
  public
    /// initialize the internal conversion engine
    constructor Create(aCodePage: cardinal); override;
    /// direct conversion of a PAnsiChar buffer into an Unicode buffer
    // - Dest^ buffer must be reserved with at least SourceChars*2 bytes
    // - will append a trailing #0 to the returned PWideChar, unless
    // NoTrailingZero is set
    function AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PWideChar; override;
    /// direct conversion of a PAnsiChar buffer into a UTF-8 encoded buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - will append a trailing #0 to the returned PUtf8Char, unless
    // NoTrailingZero is set
    function AnsiBufferToUtf8(Dest: PUtf8Char; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PUtf8Char; override;
    /// convert any Ansi buffer into an Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(Source: PAnsiChar;
      SourceChars: cardinal): RawUnicode; override;
    /// direct conversion of an Unicode buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - this overridden version will use internal lookup tables for fast process
    function UnicodeBufferToAnsi(Dest: PAnsiChar;
      Source: PWideChar; SourceChars: cardinal): PAnsiChar; override;
    /// direct conversion of an UTF-8 encoded buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars bytes
    // - no trailing #0 is appended to the buffer
    // - non Ansi compatible characters are replaced as '?'
    function Utf8BufferToAnsi(Dest: PAnsiChar; Source: PUtf8Char;
      SourceChars: cardinal): PAnsiChar; override;
    /// conversion of a wide char into the corresponding Ansi character
    // - return -1 for an unknown WideChar in the current code page
    function WideCharToAnsiChar(wc: cardinal): integer;
    /// return TRUE if the supplied unicode buffer only contains characters of
    // the corresponding Ansi code page
    // - i.e. if the text can be displayed using this code page
    function IsValidAnsi(WideText: PWideChar; Length: PtrInt): boolean; overload;
    /// return TRUE if the supplied unicode buffer only contains characters of
    // the corresponding Ansi code page
    // - i.e. if the text can be displayed using this code page
    function IsValidAnsi(WideText: PWideChar): boolean; overload;
    /// return TRUE if the supplied UTF-8 buffer only contains characters of
    // the corresponding Ansi code page
    // - i.e. if the text can be displayed using this code page
    function IsValidAnsiU(Utf8Text: PUtf8Char): boolean;
    /// return TRUE if the supplied UTF-8 buffer only contains 8-bit characters
    // of the corresponding Ansi code page
    // - i.e. if the text can be displayed with only 8-bit unicode characters
    // (e.g. no "tm" or such) within this code page
    function IsValidAnsiU8Bit(Utf8Text: PUtf8Char): boolean;
    /// direct access to the Ansi-To-Unicode lookup table
    // - use this array like AnsiToWide: array[byte] of word
    property AnsiToWide: TWordDynArray
      read fAnsiToWide;
    /// direct access to the Unicode-To-Ansi lookup table
    // - use this array like WideToAnsi: array[word] of byte
    // - any unhandled WideChar will return ord('?')
    property WideToAnsi: TByteDynArray
      read fWideToAnsi;
  end;

  /// a class to handle UTF-8 to/from Unicode translation
  // - match the TSynAnsiConvert signature, for code page CP_UTF8
  // - this class is mostly a non-operation for conversion to/from UTF-8
  TSynAnsiUtf8 = class(TSynAnsiConvert)
  protected
    function UnicodeBufferToUtf8(Dest: PAnsiChar; DestChars: cardinal;
      Source: PWideChar; SourceChars: cardinal): PAnsiChar;
  public
    /// initialize the internal conversion engine
    constructor Create(aCodePage: cardinal); override;
    /// direct conversion of a PAnsiChar UTF-8 buffer into an Unicode buffer
    // - Dest^ buffer must be reserved with at least SourceChars*2 bytes
    // - will append a trailing #0 to the returned PWideChar, unless
    // NoTrailingZero is set
    function AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PWideChar; override;
    /// direct conversion of a PAnsiChar UTF-8 buffer into a UTF-8 encoded buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - will append a trailing #0 to the returned PUtf8Char, unless
    // NoTrailingZero is set
    function AnsiBufferToUtf8(Dest: PUtf8Char; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PUtf8Char; override;
    /// convert any UTF-8 Ansi buffer into an Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(Source: PAnsiChar;
      SourceChars: cardinal): RawUnicode; override;
    /// direct conversion of an Unicode buffer into a PAnsiChar UTF-8 buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    function UnicodeBufferToAnsi(Dest: PAnsiChar; Source: PWideChar;
      SourceChars: cardinal): PAnsiChar; override;
    /// direct conversion of an Unicode buffer into an Ansi Text
    function UnicodeBufferToAnsi(Source: PWideChar;
      SourceChars: cardinal): RawByteString; override;
    /// direct conversion of an UTF-8 encoded buffer into a PAnsiChar UTF-8 buffer
    // - Dest^ buffer must be reserved with at least SourceChars bytes
    // - no trailing #0 is appended to the buffer
    function Utf8BufferToAnsi(Dest: PAnsiChar; Source: PUtf8Char;
      SourceChars: cardinal): PAnsiChar; override;
    /// convert any UTF-8 encoded buffer into Ansi Text
    procedure Utf8BufferToAnsi(Source: PUtf8Char; SourceChars: cardinal;
      var result: RawByteString); override;
    /// convert any UTF-8 encoded String into Ansi Text
    // - directly assign the input as result, since no conversion is needed
    function Utf8ToAnsi(const u: RawUtf8): RawByteString; override;
    /// convert any Ansi Text into an UTF-8 encoded String
    // - directly assign the input as result, since no conversion is needed
    function AnsiToUtf8(const AnsiText: RawByteString): RawUtf8; override;
    /// direct conversion of a PAnsiChar buffer into a UTF-8 encoded string
    function AnsiBufferToRawUtf8(Source: PAnsiChar;
      SourceChars: cardinal): RawUtf8; override;
  end;

  /// a class to handle UTF-16 to/from Unicode translation
  // - match the TSynAnsiConvert signature, for code page CP_UTF16
  // - even if UTF-16 is not an Ansi format, code page CP_UTF16 may have been
  // used to store UTF-16 encoded binary content
  // - this class is mostly a non-operation for conversion to/from Unicode
  TSynAnsiUtf16 = class(TSynAnsiConvert)
  public
    /// initialize the internal conversion engine
    constructor Create(aCodePage: cardinal); override;
    /// direct conversion of a PAnsiChar UTF-16 buffer into an Unicode buffer
    // - Dest^ buffer must be reserved with at least SourceChars*2 bytes
    // - will append a trailing #0 to the returned PWideChar, unless
    // NoTrailingZero is set
    function AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PWideChar; override;
    /// direct conversion of a PAnsiChar UTF-16 buffer into a UTF-8 encoded buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - will append a trailing #0 to the returned PUtf8Char, unless
    // NoTrailingZero is set
    function AnsiBufferToUtf8(Dest: PUtf8Char; Source: PAnsiChar;
      SourceChars: cardinal; NoTrailingZero: boolean = false): PUtf8Char; override;
    /// convert any UTF-16 Ansi buffer into an Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(Source: PAnsiChar;
      SourceChars: cardinal): RawUnicode; override;
    /// direct conversion of an Unicode buffer into a PAnsiChar UTF-16 buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    function UnicodeBufferToAnsi(Dest: PAnsiChar; Source: PWideChar;
      SourceChars: cardinal): PAnsiChar; override;
    /// direct conversion of an UTF-8 encoded buffer into a PAnsiChar UTF-16 buffer
    // - Dest^ buffer must be reserved with at least SourceChars bytes
    // - no trailing #0 is appended to the buffer
    function Utf8BufferToAnsi(Dest: PAnsiChar; Source: PUtf8Char;
      SourceChars: cardinal): PAnsiChar; override;
  end;


var
  /// global TSynAnsiConvert instance to handle WinAnsi encoding (code page 1252)
  // - this instance is global and instantied during the whole program life time
  // - it will be created from hard-coded values, and not using the system API,
  // since it appeared that some systems (e.g. in Russia) did tweak the registry
  // so that 1252 code page maps 1251 code page
  WinAnsiConvert: TSynAnsiFixedWidth;

  /// global TSynAnsiConvert instance to handle current system encoding
  // - this is the encoding as used by the AnsiString type, so will be used
  // before Delphi 2009 to speed-up VCL string handling (especially for UTF-8)
  // - this instance is global and instantied during the whole program life time
  CurrentAnsiConvert: TSynAnsiConvert;

  /// global TSynAnsiConvert instance to handle UTF-8 encoding (code page CP_UTF8)
  // - this instance is global and instantied during the whole program life time
  Utf8AnsiConvert: TSynAnsiUtf8;



{ *************** Low-Level String Conversion Functions }

/// will fast replace all #0 chars as ~
// - could be used after UniqueRawUtf8() on a in-placed modified JSON buffer,
// in which all values have been ended with #0
// - you can optionally specify a maximum size, in bytes (this won't reallocate
// the string, but just add a #0 at some point in the UTF-8 buffer)
// - could allow logging of parsed input e.g. after an exception
procedure UniqueRawUtf8ZeroToTilde(var u: RawUtf8; MaxSize: integer = maxInt);

/// conversion of a wide char into a WinAnsi (CodePage 1252) char
// - return '?' for an unknown WideChar in code page 1252
function WideCharToWinAnsiChar(wc: cardinal): AnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion of a wide char into a WinAnsi (CodePage 1252) char index
// - return -1 for an unknown WideChar in code page 1252
function WideCharToWinAnsi(wc: cardinal): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied unicode buffer only contains WinAnsi characters
// - i.e. if the text can be displayed using ANSI_CHARSET
function IsWinAnsi(WideText: PWideChar): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied unicode buffer only contains WinAnsi characters
// - i.e. if the text can be displayed using ANSI_CHARSET
function IsWinAnsi(WideText: PWideChar; Length: integer): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied UTF-8 buffer only contains WinAnsi characters
// - i.e. if the text can be displayed using ANSI_CHARSET
function IsWinAnsiU(Utf8Text: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied UTF-8 buffer only contains WinAnsi 8-bit characters
// - i.e. if the text can be displayed using ANSI_CHARSET with only 8-bit unicode
// characters (e.g. no "tm" or such)
function IsWinAnsiU8Bit(Utf8Text: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of an AnsiString with an unknown code page into an
// UTF-8 encoded String
// - will assume CurrentAnsiConvert.CodePage prior to Delphi 2009
// - newer UNICODE versions of Delphi will retrieve the code page from string
procedure AnyAnsiToUtf8(const s: RawByteString; var result: RawUtf8); overload;

/// direct conversion of an AnsiString with an unknown code page into an
// UTF-8 encoded String
// - will assume CurrentAnsiConvert.CodePage prior to Delphi 2009
// - newer UNICODE versions of Delphi will retrieve the code page from string
function AnyAnsiToUtf8(const s: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi (CodePage 1252) string into a UTF-8 encoded String
// - faster than SysUtils: don't use Utf8Encode(WideString) -> no Windows.Global(),
// and use a fixed pre-calculated array for individual chars conversion
function WinAnsiToUtf8(const S: WinAnsiString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi (CodePage 1252) string into a UTF-8 encoded String
// - faster than SysUtils: don't use Utf8Encode(WideString) -> no Windows.Global(),
// and use a fixed pre-calculated array for individual chars conversion
function WinAnsiToUtf8(WinAnsi: PAnsiChar; WinAnsiLen: PtrInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi PAnsiChar buffer into a UTF-8 encoded buffer
// - Dest^ buffer must be reserved with at least SourceChars*3
// - call internally WinAnsiConvert fast conversion class
function WinAnsiBufferToUtf8(Dest: PUtf8Char;
  Source: PAnsiChar; SourceChars: cardinal): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi shortstring into a UTF-8 text
// - call internally WinAnsiConvert fast conversion class
function ShortStringToUtf8(const source: ShortString): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi (CodePage 1252) string into a Unicode encoded String
// - very fast, by using a fixed pre-calculated array for individual chars conversion
function WinAnsiToRawUnicode(const S: WinAnsiString): RawUnicode;

/// direct conversion of a WinAnsi (CodePage 1252) string into a Unicode buffer
// - very fast, by using a fixed pre-calculated array for individual chars conversion
// - text will be truncated if necessary to avoid buffer overflow in Dest[]
procedure WinAnsiToUnicodeBuffer(const S: WinAnsiString;
  Dest: PWordArray; DestLen: PtrInt);
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded string into a WinAnsi String
function Utf8ToWinAnsi(const S: RawUtf8): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded zero terminated buffer into a WinAnsi String
function Utf8ToWinAnsi(P: PUtf8Char): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded zero terminated buffer into a RawUtf8 String
procedure Utf8ToRawUtf8(P: PUtf8Char; var result: RawUtf8);
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded buffer into a WinAnsi PAnsiChar buffer
function Utf8ToWinPChar(dest: PAnsiChar; source: PUtf8Char; count: integer): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a UTF-8 encoded buffer into a RawUnicode string
// - if L is 0, L is computed from zero terminated P buffer
// - RawUnicode is ended by a WideChar(#0)
// - faster than System.Utf8Decode() which uses slow widestrings
function Utf8DecodeToRawUnicode(P: PUtf8Char; L: integer): RawUnicode; overload;

/// convert a UTF-8 string into a RawUnicode string
function Utf8DecodeToRawUnicode(const S: RawUtf8): RawUnicode; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a UTF-8 string into a RawUnicode string
// - this version doesn't resize the length of the result RawUnicode
// and is therefore useful before a Win32 Unicode API call (with nCount=-1)
// - if DestLen is not nil, the resulting length (in bytes) will be stored within
function Utf8DecodeToRawUnicodeUI(const S: RawUtf8;
  DestLen: PInteger = nil): RawUnicode; overload;

/// convert a UTF-8 string into a RawUnicode string
// - returns the resulting length (in bytes) will be stored within Dest
function Utf8DecodeToRawUnicodeUI(const S: RawUtf8;
  var Dest: RawUnicode): integer; overload;

/// convert a RawUnicode string into a UTF-8 string
function RawUnicodeToUtf8(const Unicode: RawUnicode): RawUtf8; overload;

/// convert a SynUnicode string into a UTF-8 string
function SynUnicodeToUtf8(const Unicode: SynUnicode): RawUtf8;

/// convert a WideString into a UTF-8 string
function WideStringToUtf8(const aText: WideString): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a Unicode encoded buffer into a WinAnsi PAnsiChar buffer
procedure RawUnicodeToWinPChar(dest: PAnsiChar;
  source: PWideChar; WideCharCount: integer);
  {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUnicode PWideChar into a WinAnsi (code page 1252) string
function RawUnicodeToWinAnsi(
  WideChar: PWideChar; WideCharCount: integer): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUnicode string into a WinAnsi (code page 1252) string
function RawUnicodeToWinAnsi(const Unicode: RawUnicode): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a WideString into a WinAnsi (code page 1252) string
function WideStringToWinAnsi(const Wide: WideString): WinAnsiString;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an AnsiChar buffer (of a given code page) into a UTF-8 string
procedure AnsiCharToUtf8(P: PAnsiChar; L: integer; var result: RawUtf8; ACP: integer);

/// convert any Raw Unicode encoded String into a generic SynUnicode Text
function RawUnicodeToSynUnicode(const Unicode: RawUnicode): SynUnicode; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Raw Unicode encoded String into a generic SynUnicode Text
function RawUnicodeToSynUnicode(
  WideChar: PWideChar; WideCharCount: integer): SynUnicode; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an Unicode buffer into a WinAnsi (code page 1252) string
procedure UnicodeBufferToWinAnsi(source: PWideChar; out Dest: WinAnsiString);

/// convert an Unicode buffer into a generic VCL string
function UnicodeBufferToString(source: PWideChar): string;

{$ifdef HASVARUSTRING}

/// convert a Delphi 2009+ or FPC Unicode string into our UTF-8 string
function UnicodeStringToUtf8(const S: UnicodeString): RawUtf8; inline;

// this function is the same as direct RawUtf8=AnsiString(CP_UTF8) assignment
// but is faster, since it uses no Win32 API call
function Utf8DecodeToUnicodeString(const S: RawUtf8): UnicodeString; overload; inline;

/// convert our UTF-8 encoded buffer into a Delphi 2009+ or FPC Unicode string
// - this function is the same as direct assignment, since RawUtf8=AnsiString(CP_UTF8),
// but is faster, since use no Win32 API call
procedure Utf8DecodeToUnicodeString(P: PUtf8Char; L: integer;
  var result: UnicodeString); overload;

/// convert a Delphi 2009+ or FPC Unicode string into a WinAnsi (code page 1252) string
function UnicodeStringToWinAnsi(const S: UnicodeString): WinAnsiString; inline;

/// convert our UTF-8 encoded buffer into a Delphi 2009+ or FPC Unicode string
// - this function is the same as direct assignment, since RawUtf8=AnsiString(CP_UTF8),
// but is faster, since use no Win32 API call
function Utf8DecodeToUnicodeString(P: PUtf8Char; L: integer): UnicodeString; overload; inline;

/// convert a Win-Ansi encoded buffer into a Delphi 2009+ or FPC Unicode string
// - this function is faster than default RTL, since use no Win32 API call
function WinAnsiToUnicodeString(WinAnsi: PAnsiChar; WinAnsiLen: PtrInt): UnicodeString; overload;

/// convert a Win-Ansi string into a Delphi 2009+ or FPC Unicode string
// - this function is faster than default RTL, since use no Win32 API call
function WinAnsiToUnicodeString(const WinAnsi: WinAnsiString): UnicodeString; inline; overload;

{$endif HASVARUSTRING}

/// convert any generic VCL Text into an UTF-8 encoded String
// - in the VCL context, it's prefered to use TLanguageFile.StringToUtf8()
//  method from mORMoti18n, which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToUtf8(const Text: string): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any generic VCL Text buffer into an UTF-8 encoded String
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
procedure StringToUtf8(Text: PChar; TextLen: PtrInt; var result: RawUtf8); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any generic VCL Text into an UTF-8 encoded String
// - this overloaded function use a faster by-reference parameter for the result
procedure StringToUtf8(const Text: string; var result: RawUtf8); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any generic VCL Text into an UTF-8 encoded String
function ToUtf8(const Text: string): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any generic VCL Text into an UTF-8 encoded TSynTempBuffer
// - returns the number of UTF-8 bytes available in Temp.buf
// - this overloaded function use a TSynTempBuffer for the result to avoid any
// memory allocation for the shorter content
// - caller should call Temp.Done to release any heap-allocated memory
function StringToUtf8(const Text: string; var Temp: TSynTempBuffer): integer; overload;

/// convert any Ansi memory buffer into UTF-8, using a TSynTempBuffer if needed
// - caller should release any memory by calling Temp.Done
// - returns a pointer to the UTF-8 converted buffer - which may be buf
function AnsiBufferToTempUtf8(var Temp: TSynTempBuffer;
  Buf: PAnsiChar; BufLen, CodePage: cardinal): PUtf8Char;

/// convert any UTF-8 encoded shortstring Text into an UTF-8 encoded String
// - expects the supplied content to be already ASCII-7 or UTF-8 encoded, e.g.
// a RTTI type or property name: it won't work with Ansi-encoded strings
function ToUtf8(const Ansi7Text: ShortString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any generic VCL Text buffer into an UTF-8 encoded buffer
// - Dest must be able to receive at least SourceChars*3 bytes
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringBufferToUtf8(Dest: PUtf8Char;
  Source: PChar; SourceChars: PtrInt): PUtf8Char; overload;

/// convert any generic VCL 0-terminated Text buffer into an UTF-8 string
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
procedure StringBufferToUtf8(Source: PChar; out result: RawUtf8); overload;

/// convert any generic VCL Text into a Raw Unicode encoded String
// - it's prefered to use TLanguageFile.StringToUtf8() method in mORMoti18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToRawUnicode(const S: string): RawUnicode; overload;

/// convert any generic VCL Text into a SynUnicode encoded String
// - it's prefered to use TLanguageFile.StringToUtf8() method in mORMoti18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToSynUnicode(const S: string): SynUnicode; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any generic VCL Text into a SynUnicode encoded String
// - overloaded to avoid a copy to a temporary result string of a function
procedure StringToSynUnicode(const S: string; var result: SynUnicode); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any generic VCL Text into a Raw Unicode encoded String
// - it's prefered to use TLanguageFile.StringToUtf8() method in mORMoti18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToRawUnicode(P: PChar; L: integer): RawUnicode; overload;

/// convert any Raw Unicode encoded string into a generic VCL Text
// - uses StrLenW() and not length(U) to handle case when was used as buffer
function RawUnicodeToString(const U: RawUnicode): string; overload;

/// convert any Raw Unicode encoded buffer into a generic VCL Text
function RawUnicodeToString(P: PWideChar; L: integer): string; overload;

/// convert any Raw Unicode encoded buffer into a generic VCL Text
procedure RawUnicodeToString(P: PWideChar; L: integer; var result: string); overload;

/// convert any SynUnicode encoded string into a generic VCL Text
function SynUnicodeToString(const U: SynUnicode): string;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any UTF-8 encoded String into a generic VCL Text
// - it's prefered to use TLanguageFile.Utf8ToString() in mORMoti18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function Utf8ToString(const Text: RawUtf8): string;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any UTF-8 encoded buffer into a generic VCL Text
// - it's prefered to use TLanguageFile.Utf8ToString() in mORMoti18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function Utf8DecodeToString(P: PUtf8Char; L: integer): string; overload;
  {$ifdef UNICODE}inline;{$endif}

/// convert any UTF-8 encoded buffer into a generic VCL Text
procedure Utf8DecodeToString(P: PUtf8Char; L: integer; var result: string); overload;

/// convert any UTF-8 encoded String into a generic WideString Text
function Utf8ToWideString(const Text: RawUtf8): WideString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any UTF-8 encoded String into a generic WideString Text
procedure Utf8ToWideString(const Text: RawUtf8; var result: WideString); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any UTF-8 encoded String into a generic WideString Text
procedure Utf8ToWideString(Text: PUtf8Char; Len: PtrInt; var result: WideString); overload;

/// convert any UTF-8 encoded String into a generic SynUnicode Text
function Utf8ToSynUnicode(const Text: RawUtf8): SynUnicode; overload;

/// convert any UTF-8 encoded String into a generic SynUnicode Text
procedure Utf8ToSynUnicode(const Text: RawUtf8; var result: SynUnicode); overload;

/// convert any UTF-8 encoded buffer into a generic SynUnicode Text
procedure Utf8ToSynUnicode(Text: PUtf8Char; Len: PtrInt; var result: SynUnicode); overload;

/// convert any Ansi 7-bit encoded String into a generic VCL Text
// - the Text content must contain only 7-bit pure ASCII characters
function Ansi7ToString(const Text: RawByteString): string; overload;
  {$ifndef UNICODE}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// convert any Ansi 7-bit encoded String into a generic VCL Text
// - the Text content must contain only 7-bit pure ASCII characters
function Ansi7ToString(Text: PWinAnsiChar; Len: PtrInt): string; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Ansi 7-bit encoded String into a generic VCL Text
// - the Text content must contain only 7-bit pure ASCII characters
procedure Ansi7ToString(Text: PWinAnsiChar; Len: PtrInt; var result: string); overload;

/// convert any generic VCL Text into Ansi 7-bit encoded String
// - the Text content must contain only 7-bit pure ASCII characters
function StringToAnsi7(const Text: string): RawByteString;
  {$ifndef UNICODE}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// convert any generic VCL Text into WinAnsi (Win-1252) 8-bit encoded String
function StringToWinAnsi(const Text: string): WinAnsiString;
  {$ifdef UNICODE}inline;{$endif}



{ **************** Text Case-(in)sensitive Conversion and Comparison }

type
  /// lookup table used for fast case conversion
  TNormTable = packed array[AnsiChar] of AnsiChar;
  /// pointer to a lookup table used for fast case conversion
  PNormTable = ^TNormTable;

  /// lookup table used for fast case conversion
  TNormTableByte = packed array[byte] of byte;

var
  /// lookup table used for fast case conversion to uppercase
  // - handle 8-bit upper chars as in WinAnsi / code page 1252 (e.g. accents)
  // - is defined globally, since may be used from an inlined function
  NormToUpper: TNormTable;
  NormToUpperByte: TNormTableByte absolute NormToUpper;

  /// lookup table used for fast case conversion to lowercase
  // - handle 8-bit upper chars as in WinAnsi / code page 1252 (e.g. accents)
  // - is defined globally, since may be used from an inlined function
  NormToLower: TNormTable;
  NormToLowerByte: TNormTableByte absolute NormToLower;

  /// this table will convert 'a'..'z' into 'A'..'Z'
  // - so it will work with UTF-8 without decoding, whereas NormToUpper[] expects
  // WinAnsi encoding
  NormToUpperAnsi7: TNormTable;
  NormToUpperAnsi7Byte: TNormTableByte absolute NormToUpperAnsi7;

  /// case sensitive NormToUpper[]/NormToLower[]-like table
  // - i.e. NormToNorm[c] = c
  NormToNorm: TNormTable;
  NormToNormByte: TNormTableByte absolute NormToNorm;

type
  /// character categories for text linefeed/word/identifier/uri parsing
  // - using such a set compiles into TEST [MEM], IMM so is more efficient
  // than a regular set of AnsiChar which generates much slower BT [MEM], IMM
  // - the same 256-byte memory will also be reused from L1 CPU cache
  // during the parsing of complex input
  TTextChar = set of (
    tcNot01013,
    tc1013,
    tcCtrlNotLF,
    tcCtrlNot0Comma,
    tcWord,
    tcIdentifierFirstChar,
    tcIdentifier,
    tcUriUnreserved);

  /// defines an AnsiChar lookup table used for branch-less text parsing
  TTextCharSet = array[AnsiChar] of TTextChar;
  /// points to an AnsiChar lookup table used for branch-less text parsing
  PTextCharSet = ^TTextCharSet;

  /// defines an Ordinal lookup table used for branch-less text parsing
  TTextByteSet = array[byte] of TTextChar;
  /// points to an Ordinal lookup table used for branch-less text parsing
  PTextByteSet = ^TTextByteSet;

var
  /// lookup table for text linefeed/word/identifier/uri branch-less parsing
  TEXT_CHARS: TTextCharSet;
  TEXT_BYTES: TTextByteSet absolute TEXT_CHARS;

/// returns TRUE if the given text buffer contains a..z,A..Z,0..9,_ characters
// - should match most usual property names values or other identifier names
// in the business logic source code
// - i.e. can be tested via IdemPropName*() functions, and the MongoDB-like
// extended JSON syntax as generated by dvoSerializeAsExtendedJson
// - following classic pascal naming convention, first char must be alphabetical
// or '_' (i.e. not a digit), following chars can be alphanumerical or '_'
function PropNameValid(P: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// returns TRUE if the given text buffers contains A..Z,0..9,_ characters
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - this function allows numbers as first char, so won't check the first char
// the same way than PropNameValid() which refuses digits as pascal convention
function PropNamesValid(const Values: array of RawUtf8): boolean;

/// case insensitive comparison of ASCII 7-bit identifiers
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - behavior is undefined with UTF-8 encoding (some false positive may occur)
function IdemPropName(const P1, P2: shortstring): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

  /// case insensitive comparison of ASCII 7-bit identifiers
  // - use it with property names values (i.e. only including A..Z,0..9,_ chars)
  // - behavior is undefined with UTF-8 encoding (some false positive may occur)
function IdemPropName(const P1: shortstring; P2: PUtf8Char; P2Len: PtrInt): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case insensitive comparison of ASCII 7-bit identifiers
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - behavior is undefined with UTF-8 encoding (some false positive may occur)
// - this version expects P1 and P2 to be a PAnsiChar with specified lengths
function IdemPropName(P1, P2: PUtf8Char; P1Len, P2Len: PtrInt): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case insensitive comparison of ASCII 7-bit identifiers
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - behavior is undefined with UTF-8 encoding (some false positive may occur)
// - this version expects P2 to be a PAnsiChar with specified length
function IdemPropNameU(const P1: RawUtf8; P2: PUtf8Char; P2Len: PtrInt): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case insensitive comparison of ASCII 7-bit identifiers of same length
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - behavior is undefined with UTF-8 encoding (some false positive may occur)
// - this version expects P1 and P2 to be a PAnsiChar with an already checked
// identical length, so may be used for a faster process, e.g. in a loop
// - if P1 and P2 are RawUtf8, you should better call overloaded function
// IdemPropNameU(const P1,P2: RawUtf8), which would be slightly faster by
// using the length stored before the actual text buffer of each RawUtf8
function IdemPropNameUSameLen(P1, P2: PUtf8Char; P1P2Len: PtrInt): boolean;
  {$ifndef OSANDROID}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// case insensitive comparison of ASCII 7-bit identifiers
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - behavior is undefined with UTF-8 encoding (some false positive may occur)
function IdemPropNameU(const P1, P2: RawUtf8): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// returns true if the beginning of p^ is the same as up^
// - ignore case - up^ must be already Upper
// - chars are compared as 7-bit Ansi only (no accentuated characters): but when
// you only need to search for field names e.g. IdemPChar() is prefered, because
// it'll be faster than IdemPCharU(), if UTF-8 decoding is not mandatory
// - if p is nil, will return FALSE
// - if up is nil, will return TRUE
function IdemPChar(p: PUtf8Char; up: PAnsiChar): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// returns true if the beginning of p^ is the same as up^
// - this overloaded function accept the uppercase lookup buffer as parameter
function IdemPChar(p: PUtf8Char; up: PAnsiChar; table: PNormTable): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// returns true if the beginning of p^ is the same as up^, ignoring white spaces
// - ignore case - up^ must be already Upper
// - any white space in the input p^ buffer is just ignored
// - chars are compared as 7-bit Ansi only (no accentuated characters): but when
// you only need to search for field names e.g. IdemPChar() is prefered, because
// it'll be faster than IdemPCharU(), if UTF-8 decoding is not mandatory
// - if p is nil, will return FALSE
// - if up is nil, will return TRUE
function IdemPCharWithoutWhiteSpace(p: PUtf8Char; up: PAnsiChar): boolean;

/// returns the index of a matching beginning of p^ in upArray[]
// - returns -1 if no item matched
// - ignore case - upArray^ must be already Upper
// - chars are compared as 7-bit Ansi only (no accentuated characters)
// - warning: this function expects upArray[] items to have AT LEAST TWO
// CHARS (it will use a fast comparison of initial 2 bytes)
function IdemPCharArray(p: PUtf8Char; const upArray: array of PAnsiChar): integer; overload;

/// returns the index of a matching beginning of p^ in upArray two characters
// - returns -1 if no item matched
// - ignore case - upArray^ must be already Upper
// - chars are compared as 7-bit Ansi only (no accentuated characters)
function IdemPCharArray(p: PUtf8Char; const upArrayBy2Chars: RawUtf8): PtrInt; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// returns true if the beginning of p^ is the same as up^
// - ignore case - up^ must be already Upper
// - this version will decode the UTF-8 content before using NormToUpper[], so
// it will be slower than the IdemPChar() function above, but will handle
// WinAnsi accentuated characters (e.g. 'e' acute will be matched as 'E')
function IdemPCharU(p, up: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// returns true if the beginning of p^ is same as up^
// - ignore case - up^ must be already Upper
// - this version expects p^ to point to an Unicode char array
function IdemPCharW(p: PWideChar; up: PUtf8Char): boolean;

/// check matching ending of p^ in upText
// - returns true if the item matched
// - ignore case - upText^ must be already Upper
// - chars are compared as 7-bit Ansi only (no accentuated characters)
function EndWith(const text, upText: RawUtf8): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// returns the index of a matching ending of p^ in upArray[]
// - returns -1 if no item matched
// - ignore case - upArray^ must be already Upper
// - chars are compared as 7-bit Ansi only (no accentuated characters)
function EndWithArray(const text: RawUtf8; const upArray: array of RawUtf8): integer;

/// returns true if the file name extension contained in p^ is the same same as extup^
// - ignore case - extup^ must be already Upper
// - chars are compared as WinAnsi (codepage 1252), not as UTF-8
// - could be used e.g. like IdemFileExt(aFileName,'.JP');
function IdemFileExt(p: PUtf8Char; extup: PAnsiChar; sepChar: AnsiChar = '.'): boolean;

/// returns matching file name extension index as extup^
// - ignore case - extup[] must be already Upper
// - chars are compared as WinAnsi (codepage 1252), not as UTF-8
// - could be used e.g. like IdemFileExts(aFileName,['.PAS','.INC']);
function IdemFileExts(p: PUtf8Char; const extup: array of PAnsiChar;
  sepChar: AnsiChar = '.'): integer;

/// fast retrieve the position of any value of a given set of characters
// - see also strspn() function which is likely to be faster
function PosCharAny(Str: PUtf8Char; Characters: PAnsiChar): PUtf8Char;

/// a non case-sensitive RawUtf8 version of Pos()
// - uppersubstr is expected to be already in upper case
// - this version handle only 7-bit ASCII (no accentuated characters)
// - see PosIU() if you want an UTF-8 version with accentuated chars support
function PosI(uppersubstr: PUtf8Char; const str: RawUtf8): PtrInt;

/// a non case-sensitive version of Pos()
// - uppersubstr is expected to be already in upper case
// - this version handle only 7-bit ASCII (no accentuated characters)
function StrPosI(uppersubstr, str: PUtf8Char): PUtf8Char;

/// a non case-sensitive RawUtf8 version of Pos()
// - substr is expected to be already in upper case
// - this version will decode the UTF-8 content before using NormToUpper[]
// - see PosI() for a non-accentuated, but faster version
function PosIU(substr: PUtf8Char; const str: RawUtf8): integer;

/// pure pascal version of strspn(), to be used with PUtf8Char/PAnsiChar
// - returns size of initial segment of s which appears in accept chars, e.g.
// ! strspn('abcdef','debca')=5
// - please note that this optimized version may read up to 3 bytes beyond
// accept but never after s end, so is safe e.g. over memory mapped files
function strspn(s, accept: pointer): integer;

/// pure pascal version of strcspn(), to be used with PUtf8Char/PAnsiChar
// - returns size of initial segment of s which doesn't appears in reject chars, e.g.
// ! strcspn('1234,6789',',')=4
// - please note that this optimized version may read up to 3 bytes beyond
// reject but never after s end, so is safe e.g. over memory mapped files
function strcspn(s, reject: pointer): integer;

/// our fast version of StrCompL(), to be used with PUtf8Char
// - i.e. make a binary comparison of two memory buffers, using supplied length
// - Default value is returned if both P1 and P2 buffers are equal
function StrCompL(P1, P2: pointer; L: PtrInt; Default: PtrInt = 0): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// our fast version of StrCompIL(), to be used with PUtf8Char
// - i.e. make a case-insensitive comparison of two memory buffers, using
// supplied length
// - Default value is returned if both P1 and P2 buffers are equal
function StrCompIL(P1, P2: pointer; L: PtrInt; Default: PtrInt = 0): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// use our fast version of StrIComp(), to be used with PUtf8Char/PAnsiChar
function StrIComp(Str1, Str2: pointer): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the next UCS4 CodePoint stored in U, then update the U pointer
// - this function will decode the UTF-8 content before using NormToUpper[]
// - will return '?' if the UCS4 CodePoint is higher than #255: so use this function
// only if you need to deal with ASCII characters (e.g. it's used for Soundex
// and for ContainsUtf8 function)
function GetNextUtf8Upper(var U: PUtf8Char): Ucs4CodePoint;
  {$ifdef HASINLINE}inline;{$endif}

/// points to the beginning of the next word stored in U
// - returns nil if reached the end of U (i.e. #0 char)
// - here a "word" is a Win-Ansi word, i.e. '0'..'9', 'A'..'Z'
function FindNextUtf8WordBegin(U: PUtf8Char): PUtf8Char;

/// return true if UpperValue (Ansi) is contained in A^ (Ansi)
// - find UpperValue starting at word beginning, not inside words
function FindAnsi(A, UpperValue: PAnsiChar): boolean;

/// return true if UpperValue (Ansi) is contained in U^ (UTF-8 encoded)
// - find UpperValue starting at word beginning, not inside words
// - UTF-8 decoding is done on the fly (no temporary decoding buffer is used)
function FindUtf8(U: PUtf8Char; UpperValue: PAnsiChar): boolean;

/// return true if Upper (Unicode encoded) is contained in U^ (UTF-8 encoded)
// - will use the slow but accurate Operating System API (Win32 or ICU)
// to perform the comparison at Unicode-level
// - consider using StrPosIReference() for our faster Unicode 10.0 version
function FindUnicode(PW: PWideChar; Upper: PWideChar; UpperLen: PtrInt): boolean;

/// return true if up^ is contained inside the UTF-8 buffer p^
// - search up^ at the beginning of every UTF-8 word (aka in Soundex)
// - here a "word" is a Win-Ansi word, i.e. '0'..'9', 'A'..'Z'
// - up^ must be already Upper
function ContainsUtf8(p, up: PUtf8Char): boolean;

/// returns TRUE if the supplied uppercased text is contained in the text buffer
function GetLineContains(p, pEnd, up: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// copy source into a 256 chars dest^ buffer with 7-bit upper case conversion
// - used internally for short keys match or case-insensitive hash
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be defined e.g. as
// array[byte] of AnsiChar on the caller stack)
function UpperCopy255(dest: PAnsiChar; const source: RawUtf8): PAnsiChar; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// copy source^ into a 256 chars dest^ buffer with 7-bit upper case conversion
// - used internally for short keys match or case-insensitive hash
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be defined e.g. as
// array[byte] of AnsiChar on the caller stack)
// - won't use SSE4.2 instructions on supported CPUs by default, which may read
// some bytes beyond the s string, so should be avoided e.g. over memory mapped
// files - call explicitely UpperCopy255BufSSE42() if you are confident on your input
function UpperCopy255Buf(dest: PAnsiChar; source: PUtf8Char; sourceLen: PtrInt): PAnsiChar;

/// copy source into dest^ with WinAnsi 8-bit upper case conversion
// - used internally for short keys match or case-insensitive hash
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar)
function UpperCopyWin255(dest: PWinAnsiChar; const source: RawUtf8): PWinAnsiChar;

/// copy UTF-16 source into dest^ with ASCII 7-bit upper case conversion
// - used internally for short keys match or case-insensitive hash
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar), replacing any non WinAnsi character by '?'
function UpperCopy255W(dest: PAnsiChar; const source: SynUnicode): PAnsiChar; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// copy WideChar source into dest^ with upper case conversion
// - used internally for short keys match or case-insensitive hash
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar), replacing any non WinAnsi character by '?'
function UpperCopy255W(dest: PAnsiChar; source: PWideChar; L: PtrInt): PAnsiChar; overload;

/// copy source into dest^ with ASCII 7-bit upper case conversion
// - returns final dest pointer
// - will copy up to the source buffer end: so Dest^ should be big enough -
// which will the case e.g. if Dest := pointer(source)
function UpperCopy(dest: PAnsiChar; const source: RawUtf8): PAnsiChar;

/// copy source into dest^ with ASCII 7-bit upper case conversion
// - returns final dest pointer
// - this special version expect source to be a shortstring
function UpperCopyShort(dest: PAnsiChar; const source: shortstring): PAnsiChar;

/// fast UTF-8 comparison handling WinAnsi CP-1252 case folding
// - this version expects u1 and u2 to be zero-terminated
// - decode the UTF-8 content before using NormToUpper[] lookup table
// - consider Utf8ICompReference() for Unicode 10.0 support
function Utf8IComp(u1, u2: PUtf8Char): PtrInt;

/// fast UTF-8 comparison handling WinAnsi CP-1252 case folding
// - this version expects u1 and u2 not to be necessary zero-terminated, but
// uses L1 and L2 as length for u1 and u2 respectively
// - decode the UTF-8 content before using NormToUpper[] lookup table
// - consider Utf8ILCompReference() for Unicode 10.0 support
function Utf8ILComp(u1, u2: PUtf8Char; L1, L2: cardinal): PtrInt;

/// copy UTF-8 buffer into dest^ handling WinAnsi CP-1252 NormToUpper[] folding
// - returns the final dest pointer
// - current implementation handles UTF-16 surrogates
function Utf8UpperCopy(Dest, Source: PUtf8Char; SourceChars: cardinal): PUtf8Char;

/// copy UTF-8 buffer into dest^ handling WinAnsi CP-1252 NormToUpper[] folding
// - returns the final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar), with UTF-8 encoding
function Utf8UpperCopy255(dest: PAnsiChar; const source: RawUtf8): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// fast case-insensitive Unicode comparison handling ASCII 7-bit chars
// - use the NormToUpperAnsi7Byte[] array, i.e. compare 'a'..'z' as 'A'..'Z'
// - this version expects u1 and u2 to be zero-terminated
function AnsiICompW(u1, u2: PWideChar): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// compare two "array of AnsiString" elements, with no case sensitivity
// - just a wrapper around StrIComp()
function SortDynArrayAnsiStringI(const A, B): integer;

/// compare two "array of PUtf8Char/PAnsiChar" elements, with no case sensitivity
// - just a wrapper around StrIComp()
function SortDynArrayPUtf8CharI(const A, B): integer;

/// compare two "array of generic string" elements, with no case sensitivity
// - the expected string type is the generic VCL string
// - just a wrapper around StrIComp() for AnsiString or AnsiICompW() for UNICODE
function SortDynArrayStringI(const A, B): integer;

/// compare two "array of WideString/UnicodeString" elements, with no case sensitivity
// - implemented here since would call AnsiICompW()
function SortDynArrayUnicodeStringI(const A, B): integer;

/// SameText() overloaded function with proper UTF-8 decoding
// - fast version using NormToUpper[] array for all WinAnsi characters
// - this version will decode each UTF-8 glyph before using NormToUpper[]
// - current implementation handles UTF-16 surrogates as Utf8IComp()
function SameTextU(const S1, S2: RawUtf8): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion of the supplied text into 8-bit uppercase
// - this will not only convert 'a'..'z' into 'A'..'Z', but also accentuated
// latin characters ('e' acute into 'E' e.g.), using NormToUpper[] array
// - it will therefore decode the supplied UTF-8 content to handle more than
// 7-bit of ascii characters (so this function is dedicated to WinAnsi code page
// 1252 characters set)
function UpperCaseU(const S: RawUtf8): RawUtf8;

/// fast conversion of the supplied text into 8-bit lowercase
// - this will not only convert 'A'..'Z' into 'a'..'z', but also accentuated
// latin characters ('E' acute into 'e' e.g.), using NormToLower[] array
// - it will therefore decode the supplied UTF-8 content to handle more than
// 7-bit of ascii characters
function LowerCaseU(const S: RawUtf8): RawUtf8;

/// fast conversion of the supplied text into 8-bit case sensitivity
// - convert the text in-place, returns the resulting length
// - it will decode the supplied UTF-8 content to handle more than 7-bit
// of ascii characters during the conversion (leaving not WinAnsi characters
// untouched)
// - will not set the last char to #0 (caller must do that if necessary)
function ConvertCaseUtf8(P: PUtf8Char; const Table: TNormTableByte): PtrInt;

/// check if the supplied text has some case-insentitive 'a'..'z','A'..'Z' chars
// - will therefore be correct with true UTF-8 content, but only for 7-bit
function IsCaseSensitive(const S: RawUtf8): boolean; overload;

/// check if the supplied text has some case-insentitive 'a'..'z','A'..'Z' chars
// - will therefore be correct with true UTF-8 content, but only for 7-bit
function IsCaseSensitive(P: PUtf8Char; PLen: PtrInt): boolean; overload;

/// fast conversion of the supplied text into uppercase
// - this will only convert 'a'..'z' into 'A'..'Z' (no NormToUpper use), and
// will therefore be correct with true UTF-8 content, but only for 7-bit
function UpperCase(const S: RawUtf8): RawUtf8;

/// fast conversion of the supplied text into uppercase
// - this will only convert 'a'..'z' into 'A'..'Z' (no NormToUpper use), and
// will therefore be correct with true UTF-8 content, but only for 7-bit
procedure UpperCaseCopy(Text: PUtf8Char; Len: PtrInt; var result: RawUtf8); overload;

/// fast conversion of the supplied text into uppercase
// - this will only convert 'a'..'z' into 'A'..'Z' (no NormToUpper use), and
// will therefore be correct with true UTF-8 content, but only for 7-bit
procedure UpperCaseCopy(const Source: RawUtf8; var Dest: RawUtf8); overload;

/// fast in-place conversion of the supplied variable text into uppercase
// - this will only convert 'a'..'z' into 'A'..'Z' (no NormToUpper use), and
// will therefore be correct with true UTF-8 content, but only for 7-bit
procedure UpperCaseSelf(var S: RawUtf8);

/// fast conversion of the supplied text into lowercase
// - this will only convert 'A'..'Z' into 'a'..'z' (no NormToLower use), and
// will therefore be correct with true UTF-8 content
function LowerCase(const S: RawUtf8): RawUtf8;

/// fast conversion of the supplied text into lowercase
// - this will only convert 'A'..'Z' into 'a'..'z' (no NormToLower use), and
// will therefore be correct with true UTF-8 content
procedure LowerCaseCopy(Text: PUtf8Char; Len: PtrInt; var result: RawUtf8);

/// fast in-place conversion of the supplied variable text into lowercase
// - this will only convert 'A'..'Z' into 'a'..'z' (no NormToLower use), and
// will therefore be correct with true UTF-8 content, but only for 7-bit
procedure LowerCaseSelf(var S: RawUtf8);

/// accurate conversion of the supplied UTF-8 content into the corresponding
// upper-case Unicode characters
// - will use the available API (e.g. Win32 or ICU), so may not be consistent on
// all systems - consider UpperCaseReference() to use our Unicode 10.0 tables
// - will temporary decode S into and from UTF-16 so is likely to be slower
function UpperCaseUnicode(const S: RawUtf8): RawUtf8;

/// accurate conversion of the supplied UTF-8 content into the corresponding
// lower-case Unicode characters
// - will use the available API (e.g. Win32 or ICU), so may not be consistent on
// all systems - and also slower than LowerCase/LowerCaseU versions
function LowerCaseUnicode(const S: RawUtf8): RawUtf8;

/// fast WinAnsi comparison using the NormToUpper[] array for all 8-bit values
function AnsiIComp(Str1, Str2: pointer): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}


{ ************** Operating-System Independent Unicode Process }

/// UpperCase conversion of a UTF-8 buffer using our Unicode 10.0 tables
// - won't call the Operating System, so is consistent on all platforms,
// whereas UpperCaseUnicode() may vary depending on each library implementation
// - some codepoints enhance in length, so D^ should be at least twice than S^
// - any invalid input is replaced by UNICODE_REPLACEMENT_CHARACTER=$fffd
// - won't use temporary UTF-16 decoding, and optimized for plain ASCII content
function Utf8UpperReference(S, D: PUtf8Char): PUtf8Char; overload;

/// UpperCase conversion of a UTF-8 buffer using our Unicode 10.0 tables
// - won't call the Operating System, so is consistent on all platforms,
// whereas UpperCaseUnicode() may vary depending on each library implementation
// - some codepoints enhance in length, so D^ should be at least twice than S^
// - any invalid input is replaced by UNICODE_REPLACEMENT_CHARACTER=$fffd
// - won't use temporary UTF-16 decoding, and optimized for plain ASCII content
// - knowing the Source length, this function will handle any ASCII 7-bit input
// by quad, for efficiency
function Utf8UpperReference(S, D: PUtf8Char; SLen: PtrUInt): PUtf8Char; overload;

/// UpperCase conversion of a UTF-8 string using our Unicode 10.0 tables
// - won't call the Operating System, so is consistent on all platforms,
// whereas UpperCaseUnicode() may vary depending on each library implementation
// - won't use temporary UTF-16 decoding, and optimized for plain ASCII content
function UpperCaseReference(const S: RawUtf8): RawUtf8;

/// UTF-8 comparison using our Unicode 10.0 tables
// - this version expects u1 and u2 to be zero-terminated
// - Utf8IComp() only handle WinAnsi CP-1252 latin accents
// - won't call the Operating System, so is consistent on all platforms, and
// don't require any temporary UTF-16 decoding
function Utf8ICompReference(u1, u2: PUtf8Char): PtrInt;

/// UTF-8 comparison using our Unicode 10.0 tables
// - this version expects u1 and u2 not to be necessary zero-terminated, but
// uses L1 and L2 as length for u1 and u2 respectively
// - Utf8ILComp() only handle WinAnsi CP-1252 latin accents
// - won't call the Operating System, so is consistent on all platforms, and
// don't require any temporary UTF-16 decoding
function Utf8ILCompReference(u1, u2: PUtf8Char; L1, L2: integer): PtrInt;

/// UpperCase conversion of UTF-8 into UCS4 using our Unicode 10.0 tables
// - won't call the Operating System, so is consistent on all platforms,
// whereas UpperCaseUnicode() may vary depending on each library implementation
function UpperCaseUcs4Reference(const S: RawUtf8): RawUcs4;

/// UTF-8 Unicode 10.0 case-insensitive Pattern search within UTF-8 buffer
// - returns nil if no match, or the Pattern position found inside U^
// - Up should have been already converted using UpperCaseUcs4Reference()
// - won't call the Operating System, so is consistent on all platforms, and
// don't require any temporary UTF-16 decoding
function StrPosIReference(U: PUtf8Char; const Up: RawUcs4): PUtf8Char;




implementation


{ *************** UTF-8 Efficient Encoding / Decoding }

{ TUtf8Table }

function TUtf8Table.Ucs4ToUtf8(ucs4: Ucs4CodePoint; Dest: PUtf8Char): PtrInt;
var
  j: PtrInt;
begin
  if ucs4 <= $7f then
  begin
    Dest^ := AnsiChar(ucs4);
    result := 1;
    exit;
  end
  else if ucs4 <= $7ff then
  begin
    Dest[0] := AnsiChar($C0 or (ucs4 shr 6));
    Dest[1] := AnsiChar($80 or (ucs4 and $3F));
    result := 2;
    exit;
  end
  else if ucs4 <= $ffff then
  begin
    Dest[0] := AnsiChar($E0 or (ucs4 shr 12));
    Dest[1] := AnsiChar($80 or ((ucs4 shr 6) and $3F));
    Dest[2] := AnsiChar($80 or (ucs4 and $3F));
    result := 3;
    exit;
  end
  else if ucs4 <= $1FFFFF then
    result := 4
  else if ucs4 <= $3FFFFFF then
    result := 5
  else
    result := 6;
  j := result - 1;
  repeat
    Dest[j] := AnsiChar((ucs4 and $3f) or $80);
    ucs4 := ucs4 shr 6;
    dec(j);
  until j = 0;
  Dest^ := AnsiChar(byte(ucs4) or FirstByte[result]);
end;

function TUtf8Table.GetHighUtf8Ucs4(var U: PUtf8Char): Ucs4CodePoint;
var
  xtra, i: PtrInt;
  v: byte;
  c: PtrUInt;
begin
  result := 0;
  c := byte(U^); // here c=U^>=#80
  inc(U);
  xtra := Bytes[c];
  if xtra = 0 then
    exit; // invalid leading byte
  i := 0;
  repeat
    v := byte(U[i]);
    if v and $c0 <> $80 then
      exit; // invalid input content
    c := (c shl 6) + v;
    inc(i);
  until i = xtra;
  inc(U, xtra);
  with Extra[xtra] do
  begin
    dec(c, offset);
    if c < minimum then
      exit; // invalid input content
  end;
  result := c;
end;


function GetHighUtf8Ucs4(var U: PUtf8Char): Ucs4CodePoint;
begin
  result := UTF8_TABLE.GetHighUtf8Ucs4(U);
end;

function GetUtf8WideChar(P: PUtf8Char): cardinal;
begin
  if P <> nil then
  begin
    result := byte(P^);
    if result and $80 <> 0 then
    begin
      result := UTF8_TABLE.GetHighUtf8Ucs4(P);
      if result > $ffff then
        // surrogates can't be stored in a single UTF-16 WideChar
        result := UNICODE_REPLACEMENT_CHARACTER;
    end;
  end
  else
    result := PtrUInt(P);
end;

function NextUtf8Ucs4(var P: PUtf8Char): Ucs4CodePoint;
begin
  if P <> nil then
  begin
    result := byte(P[0]);
    if result <= 127 then
      inc(P)
    else
      if result and $20 = 0 then
      begin
        result := (result shl 6) + byte(P[1]) - $3080; // fast process $0..$7ff
        inc(P, 2);
      end
      else
        result := UTF8_TABLE.GetHighUtf8Ucs4(P); // handle even UTF-16 surrogates
  end
  else
    result := 0;
end;

function WideCharToUtf8(Dest: PUtf8Char; aWideChar: PtrUInt): integer;
begin
  if aWideChar <= $7F then
  begin
    Dest^ := AnsiChar(aWideChar);
    result := 1;
  end
  else if aWideChar > $7ff then
  begin
    Dest[0] := AnsiChar($E0 or (aWideChar shr 12));
    Dest[1] := AnsiChar($80 or ((aWideChar shr 6) and $3F));
    Dest[2] := AnsiChar($80 or (aWideChar and $3F));
    result := 3;
  end
  else
  begin
    Dest[0] := AnsiChar($C0 or (aWideChar shr 6));
    Dest[1] := AnsiChar($80 or (aWideChar and $3F));
    result := 2;
  end;
end;

function Utf16CharToUtf8(Dest: PUtf8Char; var Source: PWord): integer;
var
  c: cardinal;
  j: integer;
begin
  c := Source^;
  inc(Source);
  case c of
    0..$7f:
      begin
        Dest^ := AnsiChar(c);
        result := 1;
        exit;
      end;
    UTF16_HISURROGATE_MIN..UTF16_HISURROGATE_MAX:
      begin
        c := ((c - $D7C0) shl 10) or
              (Source^ xor UTF16_LOSURROGATE_MIN);
        inc(Source);
      end;
    UTF16_LOSURROGATE_MIN..UTF16_LOSURROGATE_MAX:
      begin
        c := ((cardinal(Source^) - $D7C0) shl 10) or
              (c xor UTF16_LOSURROGATE_MIN);
        inc(Source);
      end;
  end; // now c is the UTF-32/UCS4 code point
  if c <= $7ff then
    result := 2
  else if c <= $ffff then
    result := 3
  else if c <= $1FFFFF then
    result := 4
  else if c <= $3FFFFFF then
    result := 5
  else
    result := 6;
  j := result - 1;
  repeat
    Dest[j] := AnsiChar((c and $3f) or $80);
    c := c shr 6;
    dec(j);
  until j = 0;
  Dest^ := AnsiChar(byte(c) or UTF8_TABLE.FirstByte[result]);
end;

function Ucs4ToUtf8(ucs4: Ucs4CodePoint; Dest: PUtf8Char): PtrInt;
var
  j: PtrInt;
begin
  if ucs4 <= $7f then
  begin
    Dest^ := AnsiChar(ucs4);
    result := 1;
    exit;
  end
  else if ucs4 <= $7ff then
  begin
    Dest[0] := AnsiChar($C0 or (ucs4 shr 6));
    Dest[1] := AnsiChar($80 or (ucs4 and $3F));
    result := 2;
    exit;
  end
  else if ucs4 <= $ffff then
  begin
    Dest[0] := AnsiChar($E0 or (ucs4 shr 12));
    Dest[1] := AnsiChar($80 or ((ucs4 shr 6) and $3F));
    Dest[2] := AnsiChar($80 or (ucs4 and $3F));
    result := 3;
    exit;
  end
  else if ucs4 <= $1FFFFF then
    result := 4
  else if ucs4 <= $3FFFFFF then
    result := 5
  else
    result := 6;
  j := result - 1;
  repeat
    Dest[j] := AnsiChar((ucs4 and $3f) or $80);
    ucs4 := ucs4 shr 6;
    dec(j);
  until j = 0;
  Dest^ := AnsiChar(byte(ucs4) or UTF8_TABLE.FirstByte[result]);
end;

function RawUnicodeToUtf8(Dest: PUtf8Char; DestLen: PtrInt; Source: PWideChar;
  SourceLen: PtrInt; Flags: TCharConversionFlags): PtrInt;
var
  c: cardinal;
  Tail: PWideChar;
  i, j: integer;
label
  unmatch;
begin
  result := PtrInt(Dest);
  inc(DestLen, PtrInt(Dest));
  if (Source <> nil) and
     (Dest <> nil) then
  begin
    // first handle 7-bit ASCII WideChars, by pairs (Sha optimization)
    SourceLen := SourceLen * 2 + PtrInt(PtrUInt(Source));
    Tail := PWideChar(SourceLen) - 2;
    if (PtrInt(PtrUInt(Dest)) < DestLen) and
       (Source <= Tail) then
      repeat
        c := PCardinal(Source)^;
        if c and $ff80ff80 <> 0 then
          break; // break on first non ASCII pair
        inc(Source, 2);
        c := c shr 8 or c;
        PWord(Dest)^ := c;
        inc(Dest, 2);
      until (Source > Tail) or
            (PtrInt(PtrUInt(Dest)) >= DestLen);
    // generic loop, handling one UCS4 CodePoint per iteration
    if (PtrInt(PtrUInt(Dest)) < DestLen) and
       (PtrInt(PtrUInt(Source)) < SourceLen) then
      repeat
      // inlined Utf16CharToUtf8() with bufferoverlow check and $FFFD on unmatch
        c := cardinal(Source^);
        inc(Source);
        case c of
          0..$7f:
            begin
              Dest^ := AnsiChar(c);
              inc(Dest);
              if (PtrInt(PtrUInt(Dest)) < DestLen) and
                 (PtrInt(PtrUInt(Source)) < SourceLen) then
                continue
              else
                break;
            end;
          UTF16_HISURROGATE_MIN..UTF16_HISURROGATE_MAX:
            if (PtrInt(PtrUInt(Source)) >= SourceLen) or
               ((cardinal(Source^) < UTF16_LOSURROGATE_MIN) or
                (cardinal(Source^) > UTF16_LOSURROGATE_MAX)) then
            begin
unmatch:      if (PtrInt(PtrUInt(@Dest[3])) > DestLen) or
                 not (ccfReplacementCharacterForUnmatchedSurrogate in Flags) then
                break;
              PWord(Dest)^ := $BFEF; // UTF-8 UNICODE_REPLACEMENT_CHARACTER
              Dest[2] := AnsiChar($BD);
              inc(Dest, 3);
              if (PtrInt(PtrUInt(Dest)) < DestLen) and
                 (PtrInt(PtrUInt(Source)) < SourceLen) then
                continue
              else
                break;
            end
            else
            begin
              c := ((c - $D7C0) shl 10) or
                   (cardinal(Source^) xor UTF16_LOSURROGATE_MIN);
              inc(Source);
            end;
          UTF16_LOSURROGATE_MIN..UTF16_LOSURROGATE_MAX:
            if (PtrInt(PtrUInt(Source)) >= SourceLen) or
               ((cardinal(Source^) < UTF16_HISURROGATE_MIN) or
                (cardinal(Source^) > UTF16_HISURROGATE_MAX)) then
              goto unmatch
            else
            begin
              c := ((cardinal(Source^) - $D7C0) shl 10) or
                   (c xor UTF16_LOSURROGATE_MIN);
              inc(Source);
            end;
        end; // now c is the UTF-32/UCS4 code point
        if c <= $7ff then
          i := 2
        else if c <= $ffff then
          i := 3
        else if c <= $1FFFFF then
          i := 4
        else if c <= $3FFFFFF then
          i := 5
        else
          i := 6;
        if PtrInt(PtrUInt(Dest)) + i > DestLen then
          break;
        j := i - 1;
        repeat
          Dest[j] := AnsiChar((c and $3f) or $80);
          c := c shr 6;
          dec(j);
        until j = 0;
        Dest^ := AnsiChar(byte(c) or UTF8_TABLE.FirstByte[i]);
        inc(Dest, i);
        if (PtrInt(PtrUInt(Dest)) < DestLen) and
           (PtrInt(PtrUInt(Source)) < SourceLen) then
          continue
        else
          break;
      until false;
    if not (ccfNoTrailingZero in Flags) then
      Dest^ := #0;
  end;
  result := PtrInt(PtrUInt(Dest)) - result;
end;

procedure RawUnicodeToUtf8(WideChar: PWideChar; WideCharCount: integer;
  var result: RawUtf8; Flags: TCharConversionFlags);
var
  tmp: TSynTempBuffer;
begin
  if (WideChar = nil) or
     (WideCharCount = 0) then
    result := ''
  else
  begin
    tmp.Init(WideCharCount * 3);
    FastSetString(result, tmp.buf, RawUnicodeToUtf8(tmp.buf, tmp.len + 1,
      WideChar, WideCharCount, Flags));
    tmp.Done;
  end;
end;

function RawUnicodeToUtf8(WideChar: PWideChar; WideCharCount: integer;
  Flags: TCharConversionFlags): RawUtf8;
begin
  RawUnicodeToUtf8(WideChar, WideCharCount, result, Flags);
end;

function RawUnicodeToUtf8(WideChar: PWideChar; WideCharCount: integer;
  out Utf8Length: integer): RawUtf8;
var
  LW: integer;
begin
  result := ''; // somewhat faster if result is freed before any SetLength()
  if WideCharCount = 0 then
    exit;
  LW := WideCharCount * 3; // maximum resulting length
  SetLength(result, LW);
  Utf8Length := RawUnicodeToUtf8(pointer(result), LW + 1,
    WideChar, WideCharCount, [ccfNoTrailingZero]);
  if Utf8Length <= 0 then
    result := '';
end;

procedure Utf8ToShortString(var dest: shortstring; source: PUtf8Char);
var
  c: cardinal;
  len, extra, i: integer;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  len := 0;
  if source <> nil then
    repeat
      c := byte(source^);
      inc(source);
      if c = 0 then
        break
      else if c <= 127 then
      begin
        inc(len);
        dest[len] := AnsiChar(c);
        if len < 253 then
          continue
        else
          break;
      end
      else
      begin
        extra := utf8.Bytes[c];
        if extra = 0 then
          break; // invalid leading byte
        i := extra;
        repeat
          if byte(source^) and $c0 <> $80 then
          begin
            dest[0] := AnsiChar(len);
            exit; // invalid UTF-8 content
          end;
          c := (c shl 6) + byte(source^);
          inc(source);
          dec(i);
        until i = 0;
        dec(c, utf8.Extra[extra].offset);
        // #256.. -> slower but accurate conversion
        inc(len);
        if c > $ffff then
          dest[len] := '?'
        else
          dest[len] := AnsiChar(WinAnsiConvert.fWideToAnsi[c]);
        if len < 253 then
          continue
        else
          break;
      end;
    until false;
  dest[0] := AnsiChar(len);
end;

function Utf8ToWideChar(dest: PWideChar; source: PUtf8Char;
  MaxDestChars, sourceBytes: PtrInt; NoTrailingZero: boolean): PtrInt;
// faster than System.Utf8ToUnicode()
var
  c: cardinal;
  begd: PWideChar;
  endSource: PUtf8Char;
  endDest: PWideChar;
  i, extra: integer;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  Quit, NoSource;
begin
  result := 0;
  if dest = nil then
    exit;
  if source = nil then
    goto NoSource;
  if sourceBytes = 0 then
  begin
    if source^ = #0 then
      goto NoSource;
    sourceBytes := StrLen(source);
  end;
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  endSource := source + sourceBytes;
  endDest := dest + MaxDestChars;
  begd := dest;
  repeat
    c := byte(source^);
    inc(source);
    if c <= 127 then
    begin
      PWord(dest)^ := c; // much faster than dest^ := WideChar(c) for FPC
      inc(dest);
      if (source < endSource) and
         (dest < endDest) then
        continue
      else
        break;
    end;
    extra := utf8.Bytes[c];
    if (extra = 0) or
       (source + extra > endSource) then
      break;
    i := 0;
    repeat
      if byte(source^) and $c0 <> $80 then
        goto Quit; // invalid input content
      c := (c shl 6) + byte(source[i]);
      inc(i);
    until i = extra;
    inc(source, extra);
    with utf8.Extra[extra] do
    begin
      dec(c, offset);
      if c < minimum then
        break; // invalid input content
    end;
    if c <= $ffff then
    begin
      PWord(dest)^ := c;
      inc(dest);
      if (source < endSource) and
         (dest < endDest) then
        continue
      else
        break;
    end;
    dec(c, $10000); // store as UTF-16 surrogates
    PWordArray(dest)[0] := (c shr 10) or UTF16_HISURROGATE_MIN;
    PWordArray(dest)[1] := (c and $3FF) or UTF16_LOSURROGATE_MIN;
    inc(dest, 2);
    if (source >= endSource) or
       (dest >= endDest) then
      break;
  until false;
Quit:
  result := PtrUInt(dest) - PtrUInt(begd); // dest-begd return byte length
NoSource:
  if not NoTrailingZero then
    dest^ := #0; // always append a WideChar(0) to the end of the buffer
end;

function Utf8ToWideChar(dest: PWideChar; source: PUtf8Char; sourceBytes: PtrInt;
  NoTrailingZero: boolean): PtrInt;
// faster than System.UTF8Decode()
var
  c: cardinal;
  begd: PWideChar;
  endSource, endSourceBy4: PUtf8Char;
  i, extra: PtrInt;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  quit, nosource, by1, by4;
begin
  result := 0;
  if dest = nil then
    exit;
  if source = nil then
    goto nosource;
  if sourceBytes = 0 then
  begin
    if source^ = #0 then
      goto nosource;
    sourceBytes := StrLen(source);
  end;
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  begd := dest;
  endSource := source + sourceBytes;
  endSourceBy4 := endSource - 4;
  if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(source) and 3 = 0) and{$endif}
     (source <= endSourceBy4) then
    repeat // handle 7-bit ASCII chars, by quad (Sha optimization)
by4:  c := PCardinal(source)^;
      if c and $80808080 <> 0 then
        goto by1; // break on first non ASCII quad
      inc(source, 4);
      PCardinal(dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
      c := c shr 16;
      PCardinal(dest + 2)^ := (c shl 8 or c) and $00ff00ff;
      inc(dest, 4);
    until source > endSourceBy4;
  if source < endSource then
    repeat
by1:  c := byte(source^);
      inc(source);
      if c <= 127 then
      begin
        PWord(dest)^ := c; // much faster than dest^ := WideChar(c) for FPC
        inc(dest);
        if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(source) and 3 = 0) and{$endif}
           (source <= endSourceBy4) then
          goto by4;
        if source < endSource then
          continue
        else
          break;
      end;
      extra := utf8.Bytes[c];
      if (extra = 0) or
         (source + extra > endSource) then
        break;
      i := 0;
      repeat
        if byte(source^) and $c0 <> $80 then
          goto quit; // invalid input content
        c := (c shl 6) + byte(source[i]);
        inc(i);
      until i = extra;
      inc(source, extra);
      with utf8.Extra[extra] do
      begin
        dec(c, offset);
        if c < minimum then
          break; // invalid input content
      end;
      if c <= $ffff then
      begin
        PWord(dest)^ := c;
        inc(dest);
        if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(source) and 3 = 0) and{$endif}
           (source <= endSourceBy4) then
          goto by4;
        if source < endSource then
          continue
        else
          break;
      end;
      dec(c, $10000); // store as UTF-16 surrogates
      PWordArray(dest)[0] := (c shr 10) or UTF16_HISURROGATE_MIN;
      PWordArray(dest)[1] := (c and $3FF) or UTF16_LOSURROGATE_MIN;
      inc(dest, 2);
      if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(source) and 3 = 0) and{$endif}
         (source <= endSourceBy4) then
        goto by4;
      if source >= endSource then
        break;
    until false;
quit:
  result := PtrUInt(dest) - PtrUInt(begd); // dest-begd returns bytes length
nosource:
  if not NoTrailingZero then
    dest^ := #0; // always append a WideChar(0) to the end of the buffer
end;

function IsValidUtf8(source: PUtf8Char): boolean;
var
  extra: integer;
  c: cardinal;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := false;
  if source <> nil then
    repeat
      c := byte(source^);
      inc(source);
      if c = 0 then
        break;
      if c and $80 <> 0 then
      begin
        extra := utf8.Bytes[c];
        if extra = 0 then
          // invalid leading byte
          exit;
        // check valid UTF-8 content
        repeat
          if byte(source^) and $c0 <> $80 then
            exit;
          inc(source);
          dec(extra);
        until extra = 0;
      end;
    until false;
  result := true;
end;

function IsValidUtf8(const source: RawUtf8): boolean;
begin
  result := IsValidUtf8(pointer(source), Length(source));
end;

function IsValidUtf8(source: PUtf8Char; sourcelen: PtrInt): boolean;
var
  extra: PtrInt;
  c: cardinal;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := false;
  inc(sourcelen, PtrInt(source));
  if source <> nil then
    while PtrInt(PtrUInt(source)) < sourcelen do
    begin
      c := byte(source^);
      inc(source);
      if c = 0 then
        exit;
      if c and $80 <> 0 then
      begin
        extra := utf8.Bytes[c];
        if (extra = 0) or
           (PtrInt(PtrUInt(source)) + extra > sourcelen) then
          // invalid leading byte
          exit;
        // check valid UTF-8 content
        repeat
          if byte(source^) and $c0 <> $80 then
            exit;
          inc(source);
          dec(extra)
        until extra = 0;
      end;
    end;
  result := true;
end;

function IsValidUtf8WithoutControlChars(source: PUtf8Char): boolean;
var
  extra: integer;
  c: cardinal;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := false;
  if source <> nil then
    repeat
      c := byte(source^);
      inc(source);
      if c = 0 then
        break;
      if c < 32 then
        exit; // disallow #1..#31 control char
      if c and $80 <> 0 then
      begin
        extra := utf8.Bytes[c];
        if extra = 0 then
          // invalid leading byte
          exit;
        // check valid UTF-8 content
        repeat
          if byte(source^) and $c0 <> $80 then
            exit;
          inc(source);
          dec(extra);
        until extra = 0;
      end;
    until false;
  result := true;
end;

function IsValidUtf8WithoutControlChars(const source: RawUtf8): boolean;
var
  s, extra, len: integer;
  c: cardinal;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := false;
  s := 1;
  len := Length(source);
  while s <= len do
  begin
    c := byte(source[s]);
    inc(s);
    if c < 32 then
      exit; // disallow #0..#31 control char
    if c and $80 <> 0 then
    begin
      extra := utf8.Bytes[c];
      if extra = 0 then
        // invalid leading byte
        exit;
      // check valid UTF-8 content
      repeat
        if byte(source[s]) and $c0 <> $80 then
          exit;
        inc(s);
        dec(extra);
      until extra = 0;
    end;
  end;
  result := true;
end;

function Utf8ToUnicodeLength(source: PUtf8Char): PtrUInt;
var
  c: PtrUInt;
  extra: integer;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := 0;
  if source <> nil then
    repeat
      c := byte(source^);
      inc(source);
      if c = 0 then
        break;
      if c <= 127 then
        inc(result)
      else
      begin
        extra := utf8.Bytes[c];
        if extra = 0 then
          // invalid leading byte
          exit;
        if extra >= UTF8_EXTRA_SURROGATE then
          inc(result, 2)
        else
          inc(result);
        // check valid UTF-8 content
        repeat
          if byte(source^) and $c0 <> $80 then
            exit;
          inc(source);
          dec(extra);
        until extra = 0;
      end;
    until false;
end;

function Utf8TruncateToUnicodeLength(var text: RawUtf8; maxUtf16: integer): boolean;
var
  c: PtrUInt;
  extra: integer;
  source: PUtf8Char;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  source := pointer(text);
  if (source <> nil) and
     (cardinal(maxUtf16) < cardinal(Length(text))) then
    repeat
      if maxUtf16 <= 0 then
      begin
        SetLength(text, source - pointer(text)); // truncate
        result := true;
        exit;
      end;
      c := byte(source^);
      inc(source);
      if c = 0 then
        break
      else if c <= 127 then
        dec(maxUtf16)
      else
      begin
        extra := utf8.Bytes[c];
        if extra = 0 then
          break; // invalid leading byte
        if extra >= UTF8_EXTRA_SURROGATE then
          dec(maxUtf16, 2)
        else
          dec(maxUtf16);
        // check valid UTF-8 content
        repeat
          if byte(source^) and $c0 <> $80 then
            break;
          inc(source);
          dec(extra);
        until extra = 0;
      end;
    until false;
  result := false;
end;

function Utf8TruncateToLength(var text: RawUtf8; maxBytes: PtrUInt): boolean;
begin
  if PtrUInt(Length(text)) < maxBytes then
  begin
    result := false;
    exit; // nothing to truncate
  end;
  while (maxBytes > 0) and
        (ord(text[maxBytes]) and $c0 = $80) do
    dec(maxBytes);
  if (maxBytes > 0) and
     (ord(text[maxBytes]) and $80 <> 0) then
    dec(maxBytes);
  SetLength(text, maxBytes);
  result := true;
end;

function Utf8TruncatedLength(const text: RawUtf8; maxBytes: PtrUInt): PtrInt;
begin
  result := Length(text);
  if PtrUInt(result) < maxBytes then
    exit;
  result := maxBytes;
  while (result > 0) and
        (ord(text[result]) and $c0 = $80) do
    dec(result);
  if (result > 0) and
     (ord(text[result]) and $80 <> 0) then
    dec(result);
end;

function Utf8TruncatedLength(text: PAnsiChar; textlen, maxBytes: PtrUInt): PtrInt;
begin
  if textlen < maxBytes then
  begin
    result := textlen;
    exit;
  end;
  result := maxBytes;
  while (result > 0) and
        (ord(text[result]) and $c0 = $80) do
    dec(result);
  if (result > 0) and
     (ord(text[result]) and $80 <> 0) then
    dec(result);
end;

function Utf8FirstLineToUtf16Length(source: PUtf8Char): PtrInt;
var
  c, extra: PtrUInt;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := 0;
  if source <> nil then
    repeat
      c := byte(source^);
      inc(source);
      if c <= 127 then
        if c in [0, 10, 13] then
          break // #0, #10 or #13 stop the count
        else
          inc(result)
      else
      begin
        extra := utf8.Bytes[c];
        if extra = 0 then
          exit; // invalid leading byte
        if extra >= UTF8_EXTRA_SURROGATE then
          inc(result, 2)
        else
          inc(result);
        inc(source, extra); // a bit less safe, but faster
      end;
    until false;
end;



{ **************** UTF-8 / Unicode / Ansi Conversion Classes }

var
  // internal list of TSynAnsiConvert instances
  SynAnsiConvertList: array of TSynAnsiConvert;

{ TSynAnsiConvert }

function TSynAnsiConvert.AnsiBufferToUnicode(Dest: PWideChar;
  Source: PAnsiChar; SourceChars: cardinal; NoTrailingZero: boolean): PWideChar;
var
  c: cardinal;
begin
  // first handle trailing 7-bit ASCII chars, by quad (Sha optimization)
  if SourceChars >= 4 then
    repeat
      c := PCardinal(Source)^;
      if c and $80808080 <> 0 then
        break; // break on first non ASCII quad
      dec(SourceChars, 4);
      inc(Source, 4);
      PCardinal(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
      c := c shr 16;
      PCardinal(Dest + 2)^ := (c shl 8 or c) and $00ff00ff;
      inc(Dest, 4);
    until SourceChars < 4;
  if (SourceChars > 0) and
     (ord(Source^) < 128) then
    repeat
      dec(SourceChars);
      PWord(Dest)^ := ord(Source^); // faster than dest^ := WideChar(c) on FPC
      inc(Source);
      inc(Dest);
    until (SourceChars = 0) or
          (ord(Source^) >= 128);
  if SourceChars > 0 then
    // rely on the Operating System for all remaining ASCII characters
    inc(Dest,
      Unicode_AnsiToWide(Source, Dest, SourceChars, SourceChars, fCodePage));
  if not NoTrailingZero then
    Dest^ := #0;
  result := Dest;
end;

function TSynAnsiConvert.AnsiBufferToUtf8(Dest: PUtf8Char; Source: PAnsiChar;
  SourceChars: cardinal; NoTrailingZero: boolean): PUtf8Char;
var
  tmp: TSynTempBuffer;
  c: cardinal;
  U: PWideChar;
begin
  // first handle trailing 7-bit ASCII chars, by quad (Sha optimization)
  if SourceChars >= 4 then
    repeat
      c := PCardinal(Source)^;
      if c and $80808080 <> 0 then
        break; // break on first non ASCII quad
      PCardinal(Dest)^ := c;
      dec(SourceChars, 4);
      inc(Source, 4);
      inc(Dest, 4);
    until SourceChars < 4;
  if (SourceChars > 0) and
     (ord(Source^) < 128) then
    repeat
      Dest^ := Source^;
      dec(SourceChars);
      inc(Source);
      inc(Dest);
    until (SourceChars = 0) or
          (ord(Source^) >= 128);
  // rely on the Operating System for all remaining ASCII characters
  if SourceChars = 0 then
    result := Dest
  else
  begin
    U := AnsiBufferToUnicode(tmp.Init(SourceChars * 3), Source, SourceChars);
    result := Dest + RawUnicodeToUtf8(Dest, SourceChars * 3, tmp.buf,
      (PtrUInt(U) - PtrUInt(tmp.buf)) shr 1, [ccfNoTrailingZero]);
    tmp.Done;
  end;
  if not NoTrailingZero then
    result^ := #0;
end;

// UTF-8 is AT MOST 50% bigger than UTF-16 in bytes in range U+0800..U+FFFF
// see http://stackoverflow.com/a/7008095 -> bytes=WideCharCount*3 below

function TSynAnsiConvert.AnsiToRawUnicode(const AnsiText: RawByteString): RawUnicode;
begin
  result := AnsiToRawUnicode(pointer(AnsiText), length(AnsiText));
end;

function TSynAnsiConvert.AnsiToRawUnicode(Source: PAnsiChar;
  SourceChars: cardinal): RawUnicode;
var
  U: PWideChar;
  tmp: TSynTempBuffer;
begin
  if SourceChars = 0 then
    result := ''
  else
  begin
    U := AnsiBufferToUnicode(tmp.Init(SourceChars * 2), Source, SourceChars);
    U^ := #0;
    SetString(result, PAnsiChar(tmp.buf), PtrUInt(U) - PtrUInt(tmp.buf) + 1);
    tmp.Done;
  end;
end;

function TSynAnsiConvert.AnsiToUnicodeString(Source: PAnsiChar;
  SourceChars: cardinal): SynUnicode;
var
  tmp: TSynTempBuffer;
  U: PWideChar;
begin
  if SourceChars = 0 then
    result := ''
  else
  begin
    U := AnsiBufferToUnicode(tmp.Init(SourceChars * 2), Source, SourceChars);
    SetString(result, PWideChar(tmp.buf), (PtrUInt(U) - PtrUInt(tmp.buf)) shr 1);
    tmp.Done;
  end;
end;

function TSynAnsiConvert.AnsiToUnicodeString(const Source: RawByteString): SynUnicode;
var
  tmp: TSynTempBuffer;
  U: PWideChar;
begin
  if Source = '' then
    result := ''
  else
  begin
    tmp.Init(length(Source) * 2); // max dest size in bytes
    U := AnsiBufferToUnicode(tmp.buf, pointer(Source), length(Source));
    SetString(result, PWideChar(tmp.buf), (PtrUInt(U) - PtrUInt(tmp.buf)) shr 1);
    tmp.Done;
  end;
end;

function TSynAnsiConvert.AnsiToUtf8(const AnsiText: RawByteString): RawUtf8;
begin
  result := AnsiBufferToRawUtf8(pointer(AnsiText), length(AnsiText));
end;

function TSynAnsiConvert.AnsiBufferToRawUtf8(Source: PAnsiChar;
  SourceChars: cardinal): RawUtf8;
var
  tmp: TSynTempBuffer;
  P: pointer;
begin
  if (Source = nil) or
     (SourceChars = 0) then
    result := ''
  else
  begin
    P := tmp.Init(SourceChars * 3);
    P := AnsiBufferToUtf8(P, Source, SourceChars);
    tmp.Done(P, result);
  end;
end;

constructor TSynAnsiConvert.Create(aCodePage: cardinal);
begin
  fCodePage := aCodePage;
  fAnsiCharShift := 1; // default is safe
end;

function IsFixedWidthCodePage(aCodePage: cardinal): boolean;
begin
  result := ((aCodePage >= 1250) and
             (aCodePage <= 1258)) or
            (aCodePage = CODEPAGE_LATIN1) or
            (aCodePage >= CP_RAWBLOB);
end;

function GetEngine(aCodePage: cardinal): TSynAnsiConvert;
  {$ifdef HASINLINE} inline; {$endif}
var
  n: integer;
  p: ^TSynAnsiConvert;
begin
  p := pointer(SynAnsiConvertList);
  if p <> nil then
  begin
    n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
    repeat
      result := p^;
      if result.CodePage = aCodePage then
        exit;
      inc(p);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;

function TSynAnsiConvertInitialize(aCodePage: cardinal): TSynAnsiConvert;
begin
  GlobalLock; // paranoid multi-threading
  try
    result := GetEngine(aCodePage);
    if result <> nil then
      exit;
    if aCodePage = CP_UTF8 then
      result := TSynAnsiUtf8.Create(CP_UTF8)
    else if aCodePage = CP_UTF16 then
      result := TSynAnsiUtf16.Create(CP_UTF16)
    else if IsFixedWidthCodePage(aCodePage) then
      result := TSynAnsiFixedWidth.Create(aCodePage)
    else
      result := TSynAnsiConvert.Create(aCodePage);
    ObjArrayAdd(SynAnsiConvertList, result);
  finally
    GlobalUnLock;
  end;
end;

class function TSynAnsiConvert.Engine(aCodePage: cardinal): TSynAnsiConvert;
begin
  if aCodePage > 0 then
  begin
    result := GetEngine(aCodePage);
    if result = nil then
      result := TSynAnsiConvertInitialize(aCodePage)
  end
  else
    result := CurrentAnsiConvert;
end;

function TSynAnsiConvert.UnicodeBufferToAnsi(Dest: PAnsiChar;
  Source: PWideChar; SourceChars: cardinal): PAnsiChar;
var
  c: cardinal;
begin
  // first handle trailing 7-bit ASCII chars, by pairs (Sha optimization)
  if SourceChars >= 2 then
    repeat
      c := PCardinal(Source)^;
      if c and $ff80ff80 <> 0 then
        break; // break on first non ASCII pair
      dec(SourceChars, 2);
      inc(Source, 2);
      c := c shr 8 or c;
      PWord(Dest)^ := c;
      inc(Dest, 2);
    until SourceChars < 2;
  if (SourceChars > 0) and
     (ord(Source^) < 128) then
    repeat
      Dest^ := AnsiChar(ord(Source^));
      dec(SourceChars);
      inc(Source);
      inc(Dest);
    until (SourceChars = 0) or
          (ord(Source^) >= 128);
  // rely on the Operating System for all remaining ASCII characters
  if SourceChars > 0 then
    inc(Dest,
      Unicode_WideToAnsi(Source, Dest, SourceChars, SourceChars, fCodePage));
  result := Dest;
end;

function TSynAnsiConvert.Utf8BufferToAnsi(Dest: PAnsiChar;
  Source: PUtf8Char; SourceChars: cardinal): PAnsiChar;
var
  tmp: TSynTempBuffer;
begin
  if (Source = nil) or
     (SourceChars = 0) then
    result := Dest
  else
  begin
    tmp.Init((SourceChars + 1) shl fAnsiCharShift);
    result := UnicodeBufferToAnsi(Dest, tmp.buf,
      Utf8ToWideChar(tmp.buf, Source, SourceChars) shr 1);
    tmp.Done;
  end;
end;

function TSynAnsiConvert.Utf8BufferToAnsi(
  Source: PUtf8Char; SourceChars: cardinal): RawByteString;
begin
  Utf8BufferToAnsi(Source, SourceChars, result);
end;

procedure TSynAnsiConvert.Utf8BufferToAnsi(Source: PUtf8Char; SourceChars: cardinal;
  var result: RawByteString);
var
  tmp: array[word] of AnsiChar;
  max: PtrInt;
  buf: PAnsiChar;
begin
  if (Source = nil) or
     (SourceChars = 0) then
    result := ''
  else
  begin
    max := (SourceChars + 1) shl fAnsiCharShift;
    if max < SizeOf(tmp) then
      // use a temporary stack buffer up to 64KB
      FastSetStringCP(result, @tmp,
        Utf8BufferToAnsi(@tmp, Source, SourceChars) - PAnsiChar(@tmp), fCodePage)
    else
    begin
      // huge strings will be allocated once and truncated, not resized
      FastSetStringCP(result, nil, max, fCodePage);
      buf := Utf8BufferToAnsi(pointer(result), Source, SourceChars);
      buf^ := #0; // mandatory to emulate a regular string
      PStrLen(PAnsiChar(pointer(result)) - _STRLEN)^ := buf - pointer(result);
    end;
  end;
end;

function TSynAnsiConvert.Utf8ToAnsi(const u: RawUtf8): RawByteString;
begin
  Utf8BufferToAnsi(pointer(u), length(u), result);
end;

function TSynAnsiConvert.Utf8ToAnsiBuffer(const S: RawUtf8;
  Dest: PAnsiChar; DestSize: integer): integer;
var
  tmp: array[0..2047] of AnsiChar; // truncated to 2KB as documented
begin
  if (DestSize <= 0) or
     (Dest = nil) then
  begin
    result := 0;
    exit;
  end;
  result := length(S);
  if result > 0 then
  begin
    if result > SizeOf(tmp) then
      result := SizeOf(tmp);
    result := Utf8BufferToAnsi(tmp{%H-}, pointer(S), result) - {%H-}tmp;
    if result >= DestSize then
      result := DestSize - 1;
    MoveFast(tmp, Dest^, result);
  end;
  Dest[result] := #0;
end;

function TSynAnsiConvert.UnicodeBufferToAnsi(Source: PWideChar;
  SourceChars: cardinal): RawByteString;
var
  tmp: TSynTempBuffer;
begin
  if (Source = nil) or
     (SourceChars = 0) then
    result := ''
  else
  begin
    tmp.Init((SourceChars + 1) shl fAnsiCharShift);
    FastSetStringCP(result, tmp.buf, UnicodeBufferToAnsi(
      tmp.buf, Source, SourceChars) - PAnsiChar(tmp.buf), fCodePage);
    tmp.Done;
  end;
end;

function TSynAnsiConvert.RawUnicodeToAnsi(const Source: RawUnicode): RawByteString;
begin
  result := UnicodeBufferToAnsi(pointer(Source), length(Source) shr 1);
end;

function TSynAnsiConvert.AnsiToAnsi(From: TSynAnsiConvert;
  const Source: RawByteString): RawByteString;
begin
  if From = self then
    result := Source
  else
    result := AnsiToAnsi(From, pointer(Source), length(Source));
end;

function TSynAnsiConvert.AnsiToAnsi(From: TSynAnsiConvert;
  Source: PAnsiChar; SourceChars: cardinal): RawByteString;
var
  tmpU: array[byte] of WideChar;
  U: PWideChar;
begin
  if From = self then
    FastSetStringCP(result, Source, SourceChars, fCodePage)
  else if (Source = nil) or
          (SourceChars = 0) then
    result := ''
  else if SourceChars < SizeOf(tmpU) shr 1 then
    result := UnicodeBufferToAnsi(tmpU{%H-}, (PtrUInt(From.AnsiBufferToUnicode(
      tmpU{%H-}, Source, SourceChars)) - PtrUInt(@tmpU)) shr 1)
  else
  begin
    GetMem(U, SourceChars * 2 + 2);
    result := UnicodeBufferToAnsi(U, From.AnsiBufferToUnicode(
      U, Source, SourceChars) - U);
    FreeMem(U);
  end;
end;


{ TSynAnsiFixedWidth }

function TSynAnsiFixedWidth.AnsiBufferToUnicode(Dest: PWideChar;
  Source: PAnsiChar; SourceChars: cardinal; NoTrailingZero: boolean): PWideChar;
var
  i: integer;
  tab: PWordArray;
begin
  // PWord*(Dest)[] is much faster than dest^ := WideChar(c) for FPC
  tab := pointer(fAnsiToWide);
  for i := 1 to SourceChars shr 2 do
  begin
    PWordArray(Dest)[0] := tab[Ord(Source[0])];
    PWordArray(Dest)[1] := tab[Ord(Source[1])];
    PWordArray(Dest)[2] := tab[Ord(Source[2])];
    PWordArray(Dest)[3] := tab[Ord(Source[3])];
    inc(Source, 4);
    inc(Dest, 4);
  end;
  for i := 1 to SourceChars and 3 do
  begin
    PWord(Dest)^ := tab[Ord(Source^)];
    inc(Dest);
    inc(Source);
  end;
  if not NoTrailingZero then
    Dest^ := #0;
  result := Dest;
end;

function TSynAnsiFixedWidth.AnsiBufferToUtf8(Dest: PUtf8Char;
  Source: PAnsiChar; SourceChars: cardinal; NoTrailingZero: boolean): PUtf8Char;
var
  EndSource, EndSourceBy4: PAnsiChar;
  c: cardinal;
label
  by4, by1; // ugly but faster
begin
  if (self = nil) or
     (Dest = nil) then
  begin
    result := nil;
    Exit;
  end
  else if (Source <> nil) and
          (SourceChars > 0) then
  begin
    // handle 7-bit ASCII WideChars, by quads (Sha optimization)
    EndSource := Source + SourceChars;
    EndSourceBy4 := EndSource - 4;
    if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
       (Source <= EndSourceBy4) then
      repeat
by4:    c := PCardinal(Source)^;
        if c and $80808080 <> 0 then
          goto by1; // break on first non ASCII quad
        inc(Source, 4);
        PCardinal(Dest)^ := c;
        inc(Dest, 4);
      until Source > EndSourceBy4;
    // generic loop, handling one WideChar per iteration
    if Source < EndSource then
      repeat
by1:    c := byte(Source^);
        inc(Source);
        if c <= $7F then
        begin
          Dest^ := AnsiChar(c); // 0..127 don't need any translation
          Inc(Dest);
          if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
             (Source <= EndSourceBy4) then
            goto by4;
          if Source < EndSource then
            continue
          else
            break;
        end
        else
        begin
          // no surrogate is expected in TSynAnsiFixedWidth charsets
          c := fAnsiToWide[c]; // convert FixedAnsi char into Unicode char
          if c > $7ff then
          begin
            Dest[0] := AnsiChar($E0 or (c shr 12));
            Dest[1] := AnsiChar($80 or ((c shr 6) and $3F));
            Dest[2] := AnsiChar($80 or (c and $3F));
            Inc(Dest, 3);
            if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
               (Source <= EndSourceBy4) then
              goto by4;
            if Source < EndSource then
              continue
            else
              break;
          end
          else
          begin
            Dest[0] := AnsiChar($C0 or (c shr 6));
            Dest[1] := AnsiChar($80 or (c and $3F));
            Inc(Dest, 2);
            if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
               (Source < EndSourceBy4) then
              goto by4;
            if Source < EndSource then
              continue
            else
              break;
          end;
        end;
      until false;
  end;
  if not NoTrailingZero then
    Dest^ := #0;
  {$ifdef ISDELPHI104}
  exit(Dest); // circumvent Delphi 10.4 optimizer bug
  {$else}
  result := Dest;
  {$endif ISDELPHI104}
end;

function TSynAnsiFixedWidth.AnsiToRawUnicode(Source: PAnsiChar;
  SourceChars: cardinal): RawUnicode;
begin
  if SourceChars = 0 then
    result := ''
  else
  begin
    SetString(result, nil, SourceChars * 2 + 1);
    AnsiBufferToUnicode(pointer(result), Source, SourceChars);
  end;
end;

const
  /// reference set for WinAnsi to Unicode conversion
  // - this table contains all the Unicode codepoints corresponding to
  // the Ansi Code Page 1252 (i.e. WinAnsi), which Unicode value are > 255
  // - values taken from MultiByteToWideChar(1252,0,@Tmp,256,@WinAnsiTable,256)
  // so these values are available outside the Windows platforms (e.g. Linux/BSD)
  // and even if registry has been tweaked as such:
  // http://www.fas.harvard.edu/~chgis/data/chgis/downloads/v4/howto/cyrillic.html
  WinAnsiUnicodeChars: packed array[128..159] of word = (
    8364, 129, 8218, 402, 8222, 8230, 8224, 8225, 710, 8240, 352, 8249, 338,
    141, 381, 143, 144, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 732, 8482,
    353, 8250, 339, 157, 382, 376);

constructor TSynAnsiFixedWidth.Create(aCodePage: cardinal);
var
  i, len: PtrInt;
  A256: array[0..255] of AnsiChar;
  U256: array[0..255] of WideChar;
begin
  inherited;
  if not IsFixedWidthCodePage(aCodePage) then
    // warning: CreateUtf8() uses Utf8ToString() -> call CreateFmt() now
    raise ESynUnicode.CreateFmt('%s.Create - Invalid code page %d',
      [ClassNameShort(self)^, fCodePage]);
  // create internal look-up tables
  SetLength(fAnsiToWide, 256);
  if (aCodePage = CODEPAGE_US) or
     (aCodePage = CODEPAGE_LATIN1) or
     (aCodePage >= CP_RAWBLOB) then
  begin
    // do not trust the Windows API for the 1252 code page :(
    for i := 0 to 255 do
      fAnsiToWide[i] := i;
    if aCodePage = CODEPAGE_US then
      for i := low(WinAnsiUnicodeChars) to high(WinAnsiUnicodeChars) do
        fAnsiToWide[i] := WinAnsiUnicodeChars[i];
  end
  else
  begin
    // initialize table from Operating System returned values
    for i := 0 to 255 do
      A256[i] := AnsiChar(i);
    FillcharFast(U256, SizeOf(U256), 0);
    len := PtrUInt(inherited AnsiBufferToUnicode(U256, A256, 256)) - PtrUInt(@U256);
    if (len < 500) or
       (len > 512) then
      // warning: CreateUtf8() uses Utf8ToString() -> call CreateFmt() now
      raise ESynUnicode.CreateFmt('OS error for %s.Create(%d)',
        [ClassNameShort(self)^, aCodePage]);
    MoveFast(U256[0], fAnsiToWide[0], 512);
  end;
  SetLength(fWideToAnsi, 65536);
  for i := 1 to 126 do
    fWideToAnsi[i] := i;
  FillcharFast(fWideToAnsi[127], 65536 - 127, ord('?')); // '?' for unknown char
  for i := 127 to 255 do
    if (fAnsiToWide[i] <> 0) and
       (fAnsiToWide[i] <> ord('?')) then
      fWideToAnsi[fAnsiToWide[i]] := i;
  // fixed width Ansi will never be bigger than UTF-8
  fAnsiCharShift := 0;
end;

function TSynAnsiFixedWidth.IsValidAnsi(WideText: PWideChar; Length: PtrInt): boolean;
var
  i: PtrInt;
  wc: PtrUInt;
begin
  result := false;
  if WideText <> nil then
    for i := 0 to Length - 1 do
    begin
      wc := PtrUInt(WideText[i]);
      if wc = 0 then
        break
      else if wc < 256 then
        if fAnsiToWide[wc] < 256 then
          continue
        else
          exit
      else if fWideToAnsi[wc] = ord('?') then
        exit
      else
        continue;
    end;
  result := true;
end;

function TSynAnsiFixedWidth.IsValidAnsi(WideText: PWideChar): boolean;
var
  wc: PtrUInt;
begin
  result := false;
  if WideText <> nil then
    repeat
      wc := PtrUInt(WideText^);
      inc(WideText);
      if wc = 0 then
        break
      else if wc < 256 then
        if fAnsiToWide[wc] < 256 then
          continue
        else
          exit
      else if fWideToAnsi[wc] = ord('?') then
        exit
      else
        continue;
    until false;
  result := true;
end;

function TSynAnsiFixedWidth.IsValidAnsiU(Utf8Text: PUtf8Char): boolean;
var
  c: PtrUInt;
  i, extra: PtrInt;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := false;
  if Utf8Text <> nil then
    repeat
      c := byte(Utf8Text^);
      inc(Utf8Text);
      if c = 0 then
        break
      else if c <= 127 then
        continue
      else
      begin
        extra := utf8.Bytes[c];
        if (extra = 0) or
           (utf8.Extra[extra].minimum > $ffff) then
          exit;
        i := extra;
        repeat
          if byte(Utf8Text^) and $c0 <> $80 then
            exit; // invalid UTF-8 content
          c := (c shl 6) + byte(Utf8Text^);
          inc(Utf8Text);
          dec(i)
        until i = 0;
        dec(c, utf8.Extra[extra].offset);
        if (c > $ffff) or
           (fWideToAnsi[c] = ord('?')) then
          exit; // invalid char in the WinAnsi code page
      end;
    until false;
  result := true;
end;

function TSynAnsiFixedWidth.IsValidAnsiU8Bit(Utf8Text: PUtf8Char): boolean;
var
  c: PtrUInt;
  i, extra: PtrInt;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  result := false;
  if Utf8Text <> nil then
    repeat
      c := byte(Utf8Text^);
      inc(Utf8Text);
      if c = 0 then
        break
      else if c <= 127 then
        continue
      else
      begin
        extra := utf8.Bytes[c];
        if (extra = 0) or
           (utf8.Extra[extra].minimum > $ffff) then
          exit;
        i := extra;
        repeat
          if byte(Utf8Text^) and $c0 <> $80 then
            exit; // invalid UTF-8 content
          c := (c shl 6) + byte(Utf8Text^);
          inc(Utf8Text);
          dec(i);
        until i = 0;
        dec(c, utf8.Extra[extra].offset);
        if (c > 255) or
           (fAnsiToWide[c] > 255) then
          exit; // not 8-bit char (like "tm" or such) is marked invalid
      end;
    until false;
  result := true;
end;

function TSynAnsiFixedWidth.UnicodeBufferToAnsi(Dest: PAnsiChar;
  Source: PWideChar; SourceChars: cardinal): PAnsiChar;
var
  c: cardinal;
  tab: PAnsiChar;
begin
  // first handle trailing 7-bit ASCII chars, by pairs (Sha optimization)
  if SourceChars >= 2 then
    repeat
      c := PCardinal(Source)^;
      if c and $ff80ff80 <> 0 then
        break; // break on first non ASCII pair
      dec(SourceChars, 2);
      inc(Source, 2);
      c := c shr 8 or c;
      PWord(Dest)^ := c;
      inc(Dest, 2);
    until SourceChars < 2;
  // use internal lookup tables for fast process of remaining chars
  tab := pointer(fWideToAnsi);
  for c := 1 to SourceChars shr 2 do
  begin
    Dest[0] := tab[Ord(Source[0])];
    Dest[1] := tab[Ord(Source[1])];
    Dest[2] := tab[Ord(Source[2])];
    Dest[3] := tab[Ord(Source[3])];
    inc(Source, 4);
    inc(Dest, 4);
  end;
  for c := 1 to SourceChars and 3 do
  begin
    Dest^ := tab[Ord(Source^)];
    inc(Dest);
    inc(Source);
  end;
  result := Dest;
end;

function TSynAnsiFixedWidth.Utf8BufferToAnsi(Dest: PAnsiChar;
  Source: PUtf8Char; SourceChars: cardinal): PAnsiChar;
var
  c: cardinal;
  endSource, endSourceBy4: PUtf8Char;
  i, extra: PtrInt;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  by1, by4, quit; // ugly but faster
begin
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  // first handle trailing 7-bit ASCII chars, by quad (Sha optimization)
  endSource := Source + SourceChars;
  endSourceBy4 := endSource - 4;
  if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
     (Source <= endSourceBy4) then
    repeat
by4:  c := PCardinal(Source)^;
      if c and $80808080 <> 0 then
        goto by1; // break on first non ASCII quad
      PCardinal(Dest)^ := c;
      inc(Source, 4);
      inc(Dest, 4);
    until Source > endSourceBy4;
  // generic loop, handling one UTF-8 code per iteration
  if Source < endSource then
  begin
    repeat
by1:  c := byte(Source^);
      inc(Source);
      if ord(c) <= 127 then
      begin
        Dest^ := AnsiChar(c);
        inc(Dest);
        if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
           (Source <= endSourceBy4) then
          goto by4;
        if Source < endSource then
          continue
        else
          break;
      end
      else
      begin
        extra := utf8.Bytes[c];
        if (extra = 0) or
           (Source + extra > endSource) then
          break;
        i := extra;
        repeat
          if byte(Source^) and $c0 <> $80 then
            goto quit; // invalid UTF-8 content
          c := (c shl 6) + byte(Source^);
          inc(Source);
          dec(i);
        until i = 0;
        dec(c, utf8.Extra[extra].offset);
        if c > $ffff then
          Dest^ := '?' // '?' as in unknown fWideToAnsi[] items
        else
          Dest^ := AnsiChar(fWideToAnsi[c]);
        inc(Dest);
        if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
           (Source <= endSourceBy4) then
          goto by4;
        if Source < endSource then
          continue
        else
          break;
      end;
    until false;
  end;
quit:
  result := Dest;
end;

function TSynAnsiFixedWidth.WideCharToAnsiChar(wc: cardinal): integer;
begin
  if wc < 256 then
    if fAnsiToWide[wc] < 256 then
      result := wc
    else
      result := -1
  else if wc <= 65535 then
  begin
    result := fWideToAnsi[wc];
    if result = ord('?') then
      result := -1;
  end
  else
    result := -1;
end;


{ TSynAnsiUtf8 }

function TSynAnsiUtf8.AnsiBufferToUnicode(Dest: PWideChar;
  Source: PAnsiChar; SourceChars: cardinal; NoTrailingZero: boolean): PWideChar;
begin
  result := Dest + (Utf8ToWideChar(Dest,
    PUtf8Char(Source), SourceChars, NoTrailingZero) shr 1);
end;

function TSynAnsiUtf8.AnsiBufferToUtf8(Dest: PUtf8Char;
  Source: PAnsiChar; SourceChars: cardinal; NoTrailingZero: boolean): PUtf8Char;
begin
  MoveFast(Source^, Dest^, SourceChars);
  if not NoTrailingZero then
    Dest[SourceChars] := #0;
  result := Dest + SourceChars;
end;

function TSynAnsiUtf8.AnsiToRawUnicode(Source: PAnsiChar;
  SourceChars: cardinal): RawUnicode;
begin
  result := Utf8DecodeToRawUniCode(PUtf8Char(Source), SourceChars);
end;

constructor TSynAnsiUtf8.Create(aCodePage: cardinal);
begin
  if aCodePage <> CP_UTF8 then
    raise ESynUnicode.CreateFmt('%s.Create(%d)', [ClassNameShort(self)^, aCodePage]);
  inherited Create(aCodePage);
end;

function TSynAnsiUtf8.UnicodeBufferToUtf8(Dest: PAnsiChar;
  DestChars: cardinal; Source: PWideChar; SourceChars: cardinal): PAnsiChar;
begin
  result := Dest + RawUnicodeToUtf8(PUtf8Char(Dest), DestChars,
    Source, SourceChars, [ccfNoTrailingZero]);
end;

function TSynAnsiUtf8.UnicodeBufferToAnsi(Dest: PAnsiChar;
  Source: PWideChar; SourceChars: cardinal): PAnsiChar;
begin
  result := UnicodeBufferToUtf8(Dest, SourceChars, Source, SourceChars);
end;

function TSynAnsiUtf8.UnicodeBufferToAnsi(Source: PWideChar;
  SourceChars: cardinal): RawByteString;
var
  tmp: TSynTempBuffer;
begin
  if (Source = nil) or
     (SourceChars = 0) then
    result := ''
  else
  begin
    tmp.Init(SourceChars * 3);
    FastSetStringCP(result, tmp.buf, UnicodeBufferToUtf8(tmp.buf,
      SourceChars * 3, Source, SourceChars) - PAnsiChar(tmp.buf), fCodePage);
    tmp.Done;
  end;
end;

function TSynAnsiUtf8.Utf8BufferToAnsi(Dest: PAnsiChar;
  Source: PUtf8Char; SourceChars: cardinal): PAnsiChar;
begin
  MoveFast(Source^, Dest^, SourceChars);
  result := Dest + SourceChars;
end;

procedure TSynAnsiUtf8.Utf8BufferToAnsi(Source: PUtf8Char; SourceChars: cardinal;
  var result: RawByteString);
begin
  FastSetString(RawUtf8(result), Source, SourceChars);
end;

function TSynAnsiUtf8.Utf8ToAnsi(const u: RawUtf8): RawByteString;
begin
  result := u;
  {$ifdef HASCODEPAGE}
  SetCodePage(result, CP_UTF8, false);
  {$endif HASCODEPAGE}
end;

function TSynAnsiUtf8.AnsiToUtf8(const AnsiText: RawByteString): RawUtf8;
begin
  result := AnsiText;
  {$ifdef HASCODEPAGE}
  SetCodePage(RawByteString(result), CP_UTF8, false);
  {$endif HASCODEPAGE}
end;

function TSynAnsiUtf8.AnsiBufferToRawUtf8(Source: PAnsiChar;
  SourceChars: cardinal): RawUtf8;
begin
  FastSetString(result, Source, SourceChars);
end;


{ TSynAnsiUtf16 }

function TSynAnsiUtf16.AnsiBufferToUnicode(Dest: PWideChar;
  Source: PAnsiChar; SourceChars: cardinal; NoTrailingZero: boolean): PWideChar;
begin
  MoveFast(Source^, Dest^, SourceChars);
  result := Pointer(PtrUInt(Dest) + SourceChars);
  if not NoTrailingZero then
    result^ := #0;
end;

const
  NOTRAILING: array[boolean] of TCharConversionFlags = (
    [], [ccfNoTrailingZero]);

function TSynAnsiUtf16.AnsiBufferToUtf8(Dest: PUtf8Char;
  Source: PAnsiChar; SourceChars: cardinal; NoTrailingZero: boolean): PUtf8Char;
begin
  SourceChars := SourceChars shr 1; // from byte count to WideChar count
  result := Dest + RawUnicodeToUtf8(Dest,
    SourceChars * 3, PWideChar(Source), SourceChars, NOTRAILING[NoTrailingZero]);
end;

function TSynAnsiUtf16.AnsiToRawUnicode(Source: PAnsiChar;
  SourceChars: cardinal): RawUnicode;
begin
  SetString(result, Source, SourceChars); // byte count
end;

constructor TSynAnsiUtf16.Create(aCodePage: cardinal);
begin
  if aCodePage <> CP_UTF16 then
    raise ESynUnicode.CreateFmt('%s.Create(%d)', [ClassNameShort(self)^, aCodePage]);
  inherited Create(aCodePage);
end;

function TSynAnsiUtf16.UnicodeBufferToAnsi(Dest: PAnsiChar;
  Source: PWideChar; SourceChars: cardinal): PAnsiChar;
begin
  SourceChars := SourceChars shl 1; // from WideChar count to byte count
  MoveFast(Source^, Dest^, SourceChars);
  result := Dest + SourceChars;
end;

function TSynAnsiUtf16.Utf8BufferToAnsi(Dest: PAnsiChar;
  Source: PUtf8Char; SourceChars: cardinal): PAnsiChar;
begin
  result := Dest + Utf8ToWideChar(PWideChar(Dest), Source, SourceChars, true);
end;

function AnsiBufferToTempUtf8(var temp: TSynTempBuffer;
  Buf: PAnsiChar; BufLen, CodePage: cardinal): PUtf8Char;
begin
  if (BufLen = 0) or
     (CodePage = CP_UTF8) or
     (CodePage >= CP_RAWBLOB) or
     IsAnsiCompatible(Buf, BufLen) then
  begin
    temp.Buf := nil;
    temp.len := BufLen;
    result := PUtf8Char(Buf);
  end
  else
  begin
    temp.Init(BufLen * 3);
    Buf := pointer(TSynAnsiConvert.Engine(CodePage).
      AnsiBufferToUtf8(temp.Buf, Buf, BufLen));
    temp.len := Buf - PAnsiChar(temp.Buf);
    result := temp.Buf;
  end;
end;


{ *************** Low-Level String Conversion Functions }

procedure AnyAnsiToUtf8(const s: RawByteString; var result: RawUtf8);
{$ifdef HASCODEPAGE}
var
  cp: cardinal;
{$endif HASCODEPAGE}
begin
  if s = '' then
    result := ''
  else
  begin
    {$ifdef HASCODEPAGE}
    cp := StringCodePage(s);
    if cp = CP_UTF8 then
      result := s
    else if cp >= CP_RAWBLOB then
    begin
      result := s;
      SetCodePage(RawByteString(result), CP_UTF8, false);
    end
    else
      result := TSynAnsiConvert.Engine(cp).
        AnsiBufferToRawUtf8(pointer(s), length(s));
    {$else}
    result := CurrentAnsiConvert.AnsiBufferToRawUtf8(pointer(s), length(s));
    {$endif HASCODEPAGE}
  end;
end;

function AnyAnsiToUtf8(const s: RawByteString): RawUtf8;
begin
  AnyAnsiToUtf8(s, result);
end;

function WinAnsiBufferToUtf8(Dest: PUtf8Char;
  Source: PAnsiChar; SourceChars: cardinal): PUtf8Char;
begin
  result := WinAnsiConvert.AnsiBufferToUtf8(Dest, Source, SourceChars);
end;

function ShortStringToUtf8(const source: ShortString): RawUtf8;
begin
  result := WinAnsiConvert.AnsiBufferToRawUtf8(@source[1], ord(source[0]));
end;

procedure WinAnsiToUnicodeBuffer(const S: WinAnsiString; Dest: PWordArray; DestLen: PtrInt);
var
  L: PtrInt;
begin
  L := length(S);
  if L <> 0 then
  begin
    if L >= DestLen then
      L := DestLen - 1; // truncate to avoid buffer overflow
    WinAnsiConvert.AnsiBufferToUnicode(PWideChar(Dest), pointer(S), L); // include last #0
  end
  else
    Dest^[0] := 0;
end;

function WinAnsiToRawUnicode(const S: WinAnsiString): RawUnicode;
begin
  result := WinAnsiConvert.AnsiToRawUnicode(S);
end;

function WinAnsiToUtf8(const S: WinAnsiString): RawUtf8;
begin
  result := WinAnsiConvert.AnsiBufferToRawUtf8(pointer(S), length(S));
end;

function WinAnsiToUtf8(WinAnsi: PAnsiChar; WinAnsiLen: PtrInt): RawUtf8;
begin
  result := WinAnsiConvert.AnsiBufferToRawUtf8(WinAnsi, WinAnsiLen);
end;

function WideCharToWinAnsiChar(wc: cardinal): AnsiChar;
begin
  wc := WinAnsiConvert.WideCharToAnsiChar(wc);
  if integer(wc) = -1 then
    result := '?'
  else
    result := AnsiChar(wc);
end;

function WideCharToWinAnsi(wc: cardinal): integer;
begin
  result := WinAnsiConvert.WideCharToAnsiChar(wc);
end;

function IsWinAnsi(WideText: PWideChar; Length: integer): boolean;
begin
  result := WinAnsiConvert.IsValidAnsi(WideText, Length);
end;

function IsWinAnsi(WideText: PWideChar): boolean;
begin
  result := WinAnsiConvert.IsValidAnsi(WideText);
end;

function IsWinAnsiU(Utf8Text: PUtf8Char): boolean;
begin
  result := WinAnsiConvert.IsValidAnsiU(Utf8Text);
end;

function IsWinAnsiU8Bit(Utf8Text: PUtf8Char): boolean;
begin
  result := WinAnsiConvert.IsValidAnsiU8Bit(Utf8Text);
end;

function Utf8ToWinPChar(dest: PAnsiChar; source: PUtf8Char; count: integer): integer;
begin
  result := WinAnsiConvert.Utf8BufferToAnsi(dest, source, count) - dest;
end;

function Utf8ToWinAnsi(const S: RawUtf8): WinAnsiString;
begin
  result := WinAnsiConvert.Utf8ToAnsi(S);
end;

function Utf8ToWinAnsi(P: PUtf8Char): WinAnsiString;
begin
  result := WinAnsiConvert.Utf8ToAnsi(P);
end;

procedure Utf8ToRawUtf8(P: PUtf8Char; var result: RawUtf8);
begin
  // fast and Delphi 2009+ ready
  FastSetString(result, P, StrLen(P));
end;

function Utf8DecodeToRawUnicode(P: PUtf8Char; L: integer): RawUnicode;
var
  tmp: TSynTempBuffer;
begin
  result := ''; // somewhat faster if result is freed before any SetLength()
  if L = 0 then
    L := StrLen(P);
  if L = 0 then
    exit;
  // +1 below is for #0 ending -> true WideChar(#0) ending
  tmp.Init(L * 3); // maximum posible unicode size (if all <#128)
  SetString(result, PAnsiChar(tmp.buf), Utf8ToWideChar(tmp.buf, P, L) + 1);
  tmp.Done;
end;

function Utf8DecodeToRawUnicode(const S: RawUtf8): RawUnicode;
begin
  if S = '' then
    result := ''
  else
    result := Utf8DecodeToRawUnicode(pointer(S), Length(S));
end;

function Utf8DecodeToRawUnicodeUI(const S: RawUtf8; DestLen: PInteger): RawUnicode;
var
  L: integer;
begin
  L := Utf8DecodeToRawUnicodeUI(S, result);
  if DestLen <> nil then
    DestLen^ := L;
end;

function Utf8DecodeToRawUnicodeUI(const S: RawUtf8; var Dest: RawUnicode): integer;
begin
  Dest := ''; // somewhat faster if Dest is freed before any SetLength()
  if S = '' then
  begin
    result := 0;
    exit;
  end;
  result := Length(S);
  SetLength(Dest, result * 2 + 2);
  result := Utf8ToWideChar(pointer(Dest), Pointer(S), result);
end;

/// convert a RawUnicode string into a UTF-8 string
function RawUnicodeToUtf8(const Unicode: RawUnicode): RawUtf8;
begin
  RawUnicodeToUtf8(pointer(Unicode), Length(Unicode) shr 1, result);
end;

function SynUnicodeToUtf8(const Unicode: SynUnicode): RawUtf8;
begin
  RawUnicodeToUtf8(pointer(Unicode), Length(Unicode), result);
end;

function RawUnicodeToSynUnicode(const Unicode: RawUnicode): SynUnicode;
begin
  SetString(result, PWideChar(pointer(Unicode)), Length(Unicode) shr 1);
end;

function RawUnicodeToSynUnicode(WideChar: PWideChar; WideCharCount: integer): SynUnicode;
begin
  SetString(result, WideChar, WideCharCount);
end;

procedure RawUnicodeToWinPChar(dest: PAnsiChar; source: PWideChar; WideCharCount: integer);
begin
  WinAnsiConvert.UnicodeBufferToAnsi(dest, source, WideCharCount);
end;

function RawUnicodeToWinAnsi(WideChar: PWideChar; WideCharCount: integer): WinAnsiString;
begin
  result := WinAnsiConvert.UnicodeBufferToAnsi(WideChar, WideCharCount);
end;

function RawUnicodeToWinAnsi(const Unicode: RawUnicode): WinAnsiString;
begin
  result := WinAnsiConvert.UnicodeBufferToAnsi(pointer(Unicode), Length(Unicode) shr 1);
end;

function WideStringToWinAnsi(const Wide: WideString): WinAnsiString;
begin
  result := WinAnsiConvert.UnicodeBufferToAnsi(pointer(Wide), Length(Wide));
end;

procedure UnicodeBufferToWinAnsi(source: PWideChar; out Dest: WinAnsiString);
var
  L: integer;
begin
  L := StrLenW(source);
  SetLength(Dest, L);
  WinAnsiConvert.UnicodeBufferToAnsi(pointer(Dest), source, L);
end;

function UnicodeBufferToString(source: PWideChar): string;
begin
  result := RawUnicodeToString(source, StrLenW(source));
end;

procedure AnsiCharToUtf8(P: PAnsiChar; L: integer; var result: RawUtf8; ACP: integer);
begin
  result := TSynAnsiConvert.Engine(ACP).AnsiBufferToRawUtf8(P, L);
end;

{$ifdef UNICODE}

function Ansi7ToString(const Text: RawByteString): string;
var
  i: PtrInt;
begin
  SetString(result, nil, Length(Text));
  for i := 0 to Length(Text) - 1 do
    PWordArray(result)[i] := PByteArray(Text)[i]; // no conversion for 7-bit Ansi
end;

function Ansi7ToString(Text: PWinAnsiChar; Len: PtrInt): string;
begin
  Ansi7ToString(Text, Len, result);
end;

procedure Ansi7ToString(Text: PWinAnsiChar; Len: PtrInt; var result: string);
var
  i: PtrInt;
begin
  SetString(result, nil, Len);
  for i := 0 to Len - 1 do
    PWordArray(result)[i] := PByteArray(Text)[i]; // no conversion for 7-bit Ansi
end;

function StringToAnsi7(const Text: string): RawByteString;
var
  i: PtrInt;
begin
  SetString(result, nil, Length(Text));
  for i := 0 to Length(Text) - 1 do
    PByteArray(result)[i] := PWordArray(Text)[i]; // no conversion for 7-bit Ansi
end;

function StringToWinAnsi(const Text: string): WinAnsiString;
begin
  result := RawUnicodeToWinAnsi(Pointer(Text), Length(Text));
end;

function StringBufferToUtf8(Dest: PUtf8Char; Source: PChar; SourceChars: PtrInt): PUtf8Char;
begin
  result := Dest + RawUnicodeToUtf8(Dest, SourceChars * 3, PWideChar(Source), SourceChars, []);
end;

procedure StringBufferToUtf8(Source: PChar; out result: RawUtf8);
begin
  RawUnicodeToUtf8(Source, StrLenW(Source), result);
end;

function StringToUtf8(const Text: string): RawUtf8;
begin
  RawUnicodeToUtf8(pointer(Text), Length(Text), result);
end;

procedure StringToUtf8(Text: PChar; TextLen: PtrInt; var result: RawUtf8);
begin
  RawUnicodeToUtf8(Text, TextLen, result);
end;

procedure StringToUtf8(const Text: string; var result: RawUtf8);
begin
  RawUnicodeToUtf8(pointer(Text), Length(Text), result);
end;

function StringToUtf8(const Text: string; var Temp: TSynTempBuffer): integer;
var
  len: integer;
begin
  len := length(Text);
  Temp.Init(len * 3);
  result := RawUnicodeToUtf8(Temp.buf, Temp.len + 1, pointer(Text), len, []);
end;

function ToUtf8(const Text: string): RawUtf8;
begin
  RawUnicodeToUtf8(pointer(Text), Length(Text), result);
end;

function StringToRawUnicode(const S: string): RawUnicode;
begin
  SetString(result, PAnsiChar(pointer(S)), length(S) * 2 + 1); // +1 for last wide #0
end;

function StringToSynUnicode(const S: string): SynUnicode;
begin
  result := S;
end;

procedure StringToSynUnicode(const S: string; var result: SynUnicode);
begin
  result := S;
end;

function StringToRawUnicode(P: PChar; L: integer): RawUnicode;
begin
  SetString(result, PAnsiChar(P), L * 2 + 1); // +1 for last wide #0
end;

function RawUnicodeToString(P: PWideChar; L: integer): string;
begin
  SetString(result, P, L);
end;

procedure RawUnicodeToString(P: PWideChar; L: integer; var result: string);
begin
  SetString(result, P, L);
end;

function RawUnicodeToString(const U: RawUnicode): string;
begin
  // uses StrLenW() and not length(U) to handle case when was used as buffer
  SetString(result, PWideChar(pointer(U)), StrLenW(Pointer(U)));
end;

function SynUnicodeToString(const U: SynUnicode): string;
begin
  result := U;
end;

function Utf8DecodeToString(P: PUtf8Char; L: integer): string;
begin
  Utf8DecodeToUnicodeString(P, L, result);
end;

procedure Utf8DecodeToString(P: PUtf8Char; L: integer; var result: string);
begin
  Utf8DecodeToUnicodeString(P, L, result);
end;

function Utf8ToString(const Text: RawUtf8): string;
begin
  Utf8DecodeToUnicodeString(pointer(Text), length(Text), result);
end;

{$else}

function Ansi7ToString(const Text: RawByteString): string;
begin
  result := Text; // if we are SURE this text is 7-bit Ansi -> direct assign
end;

function Ansi7ToString(Text: PWinAnsiChar; Len: PtrInt): string;
begin
  SetString(result, PAnsiChar(Text), Len);
end;

procedure Ansi7ToString(Text: PWinAnsiChar; Len: PtrInt; var result: string);
begin
  SetString(result, PAnsiChar(Text), Len);
end;

function StringToAnsi7(const Text: string): RawByteString;
begin
  result := Text; // if we are SURE this text is 7-bit Ansi -> direct assign
end;

function StringToWinAnsi(const Text: string): WinAnsiString;
begin
  result := WinAnsiConvert.AnsiToAnsi(CurrentAnsiConvert, Text);
end;

function StringBufferToUtf8(Dest: PUtf8Char; Source: PChar; SourceChars: PtrInt): PUtf8Char;
begin
  result := CurrentAnsiConvert.AnsiBufferToUtf8(Dest, Source, SourceChars);
end;

procedure StringBufferToUtf8(Source: PChar; out result: RawUtf8);
begin
  result := CurrentAnsiConvert.AnsiBufferToRawUtf8(Source, StrLen(Source));
end;

function StringToUtf8(const Text: string): RawUtf8;
begin
  result := CurrentAnsiConvert.AnsiToUtf8(Text);
end;

procedure StringToUtf8(Text: PChar; TextLen: PtrInt; var result: RawUtf8);
begin
  result := CurrentAnsiConvert.AnsiBufferToRawUtf8(Text, TextLen);
end;

procedure StringToUtf8(const Text: string; var result: RawUtf8);
begin
  result := CurrentAnsiConvert.AnsiToUtf8(Text);
end;

function StringToUtf8(const Text: string; var Temp: TSynTempBuffer): integer;
var
  len: integer;
begin
  len := length(Text);
  Temp.Init(len * 3);
  result := CurrentAnsiConvert.AnsiBufferToUtf8(Temp.buf, pointer(Text), len)
     - PUtf8Char(Temp.buf);
end;

function ToUtf8(const Text: string): RawUtf8;
begin
  result := CurrentAnsiConvert.AnsiToUtf8(Text);
end;

function StringToRawUnicode(const S: string): RawUnicode;
begin
  result := CurrentAnsiConvert.AnsiToRawUnicode(S);
end;

function StringToSynUnicode(const S: string): SynUnicode;
begin
  result := CurrentAnsiConvert.AnsiToUnicodeString(pointer(S), length(S));
end;

procedure StringToSynUnicode(const S: string; var result: SynUnicode);
begin
  result := CurrentAnsiConvert.AnsiToUnicodeString(pointer(S), length(S));
end;

function StringToRawUnicode(P: PChar; L: integer): RawUnicode;
begin
  result := CurrentAnsiConvert.AnsiToRawUnicode(P, L);
end;

function RawUnicodeToString(P: PWideChar; L: integer): string;
begin
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(P, L);
end;

procedure RawUnicodeToString(P: PWideChar; L: integer; var result: string);
begin
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(P, L);
end;

function RawUnicodeToString(const U: RawUnicode): string;
begin
  // uses StrLenW() and not length(U) to handle case when was used as buffer
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(Pointer(U), StrLenW(Pointer(U)));
end;

function SynUnicodeToString(const U: SynUnicode): string;
begin
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(Pointer(U), length(U));
end;

function Utf8DecodeToString(P: PUtf8Char; L: integer): string;
begin
  CurrentAnsiConvert.Utf8BufferToAnsi(P, L, RawByteString(result));
end;

procedure Utf8DecodeToString(P: PUtf8Char; L: integer; var result: string);
begin
  CurrentAnsiConvert.Utf8BufferToAnsi(P, L, RawByteString(result));
end;

function Utf8ToString(const Text: RawUtf8): string;
begin
  result := CurrentAnsiConvert.Utf8ToAnsi(Text);
end;

{$endif UNICODE}

function ToUtf8(const Ansi7Text: ShortString): RawUtf8;
begin
  FastSetString(result, @Ansi7Text[1], ord(Ansi7Text[0]));
end;

{$ifdef HASVARUSTRING} // some UnicodeString dedicated functions

function UnicodeStringToUtf8(const S: UnicodeString): RawUtf8;
begin
  RawUnicodeToUtf8(pointer(S), Length(S), result);
end;

function Utf8DecodeToUnicodeString(const S: RawUtf8): UnicodeString;
begin
  Utf8DecodeToUnicodeString(pointer(S), Length(S), result);
end;

procedure Utf8DecodeToUnicodeString(P: PUtf8Char; L: integer; var result: UnicodeString);
var
  tmp: TSynTempBuffer;
begin
  if (P = nil) or
     (L = 0) then
    result := ''
  else
  begin
    tmp.Init(L * 3); // maximum posible unicode size (if all <#128)
    SetString(result, PWideChar(tmp.buf), Utf8ToWideChar(tmp.buf, P, L) shr 1);
    tmp.Done;
  end;
end;

function UnicodeStringToWinAnsi(const S: UnicodeString): WinAnsiString;
begin
  result := WinAnsiConvert.UnicodeBufferToAnsi(pointer(S), Length(S));
end;

function Utf8DecodeToUnicodeString(P: PUtf8Char; L: integer): UnicodeString;
begin
  Utf8DecodeToUnicodeString(P, L, result);
end;

function WinAnsiToUnicodeString(WinAnsi: PAnsiChar; WinAnsiLen: PtrInt): UnicodeString;
begin
  SetString(result, nil, WinAnsiLen);
  WinAnsiConvert.AnsiBufferToUnicode(pointer(result), WinAnsi, WinAnsiLen);
end;

function WinAnsiToUnicodeString(const WinAnsi: WinAnsiString): UnicodeString;
begin
  result := WinAnsiToUnicodeString(pointer(WinAnsi), Length(WinAnsi));
end;

{$endif HASVARUSTRING}

procedure UniqueRawUtf8ZeroToTilde(var u: RawUtf8; MaxSize: integer);
var
  i: integer;
begin
  i := length(u);
  if i > MaxSize then
    PByteArray(u)[MaxSize] := 0
  else
    MaxSize := i;
  for i := 0 to MaxSize - 1 do
    if PByteArray(u)[i] = 0 then
      PByteArray(u)[i] := ord('~');
end;

procedure Utf8ToWideString(const Text: RawUtf8; var result: WideString);
begin
  Utf8ToWideString(pointer(Text), Length(Text), result);
end;

function Utf8ToWideString(const Text: RawUtf8): WideString;
begin
  {$ifdef FPC}
  Finalize(result);
  {$endif FPC}
  Utf8ToWideString(pointer(Text), Length(Text), result);
end;

procedure Utf8ToWideString(Text: PUtf8Char; Len: PtrInt; var result: WideString);
var
  tmp: TSynTempBuffer;
begin
  if (Text = nil) or
     (Len = 0) then
    result := ''
  else
  begin
    tmp.Init(Len * 3); // maximum posible unicode size (if all <#128)
    SetString(result, PWideChar(tmp.buf), Utf8ToWideChar(tmp.buf, Text, Len) shr 1);
    tmp.Done;
  end;
end;

function WideStringToUtf8(const aText: WideString): RawUtf8;
begin
  RawUnicodeToUtf8(pointer(aText), length(aText), result);
end;

function Utf8ToSynUnicode(const Text: RawUtf8): SynUnicode;
begin
  Utf8ToSynUnicode(pointer(Text), length(Text), result);
end;

procedure Utf8ToSynUnicode(const Text: RawUtf8; var result: SynUnicode);
begin
  Utf8ToSynUnicode(pointer(Text), length(Text), result);
end;

procedure Utf8ToSynUnicode(Text: PUtf8Char; Len: PtrInt; var result: SynUnicode);
var
  tmp: TSynTempBuffer;
begin
  if (Text = nil) or
     (Len <= 0) then
    result := ''
  else
  begin
    tmp.Init(Len * 3); // maximum posible unicode size (if all <#128)
    SetString(result, PWideChar(tmp.buf), Utf8ToWideChar(tmp.buf, Text, Len) shr 1);
    tmp.Done;
  end;
end;



{ **************** Text Case-(in)sensitive Conversion and Comparison }

function IdemPropNameUSameLen(P1, P2: PUtf8Char; P1P2Len: PtrInt): boolean;
label
  zero;
begin
  P1P2Len := PtrInt(@PAnsiChar(P1)[P1P2Len - SizeOf(cardinal)]);
  if P1P2Len >= PtrInt(PtrUInt(P1)) then
    repeat // case-insensitive compare 4 bytes per loop
      if (PCardinal(P1)^ xor PCardinal(P2)^) and $dfdfdfdf <> 0 then
        goto zero;
      inc(P1, SizeOf(cardinal));
      inc(P2, SizeOf(cardinal));
    until P1P2Len < PtrInt(PtrUInt(P1));
  inc(P1P2Len, SizeOf(cardinal));
  dec(PtrUInt(P2), PtrUInt(P1));
  if PtrInt(PtrUInt(P1)) < P1P2Len then
    repeat
      if (ord(P1^) xor ord(P2[PtrUInt(P1)])) and $df <> 0 then
        goto zero;
      inc(P1);
    until PtrInt(PtrUInt(P1)) >= P1P2Len;
  result := true;
  exit;
zero:
  result := false;
end;

function PropNameValid(P: PUtf8Char): boolean;
var
  tab: PTextCharSet;
{%H-}begin
  tab := @TEXT_CHARS;
  if (P <> nil) and
     (tcIdentifierFirstChar in tab[P^]) then
    // first char must be in ['_', 'a'..'z', 'A'..'Z']
    repeat
      inc(P); // following chars can be ['_', '0'..'9', 'a'..'z', 'A'..'Z']
      if tcIdentifier in tab[P^] then
        continue;
      result := P^ = #0;
      exit;
    until false
  else
    result := false;
end;

function PropNamesValid(const Values: array of RawUtf8): boolean;
var
  i, j: PtrInt;
  tab: PTextCharSet;
begin
  result := false;
  tab := @TEXT_CHARS;
  for i := 0 to high(Values) do
    for j := 1 to length(Values[i]) do
      if not (tcIdentifier in tab[Values[i][j]]) then
        exit; // not ['_', '0'..'9', 'a'..'z', 'A'..'Z']
  result := true;
end;

function IdemPropName(const P1, P2: shortstring): boolean;
begin
  if P1[0] = P2[0] then
    result := IdemPropNameUSameLen(@P1[1], @P2[1], ord(P2[0]))
  else
    result := false;
end;

function IdemPropName(const P1: shortstring; P2: PUtf8Char; P2Len: PtrInt): boolean;
begin
  if ord(P1[0]) = P2Len then
    result := IdemPropNameUSameLen(@P1[1], P2, P2Len)
  else
    result := false;
end;

function IdemPropName(P1, P2: PUtf8Char; P1Len, P2Len: PtrInt): boolean;
begin
  if P1Len = P2Len then
    result := IdemPropNameUSameLen(P1, P2, P2Len)
  else
    result := false;
end;

function IdemPropNameU(const P1: RawUtf8; P2: PUtf8Char; P2Len: PtrInt): boolean;
begin
  if length(P1) = P2Len then
    result := IdemPropNameUSameLen(pointer(P1), P2, P2Len)
  else
    result := false;
end;

function IdemPChar(p: PUtf8Char; up: PAnsiChar): boolean;
var
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7;
  {$else}
  table: PNormTable; // faster on PIC/ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  result := false;
  if p = nil then
    exit;
  if up <> nil then
  begin
    dec(PtrUInt(p), PtrUInt(up));
    {$ifndef CPUX86NOTPIC}
    table := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    repeat
      if up^ = #0 then
        break;
      if table[up[PtrUInt(p)]] <> up^ then
        exit;
      inc(up);
    until false;
  end;
  result := true;
end;

function IdemPChar(p: PUtf8Char; up: PAnsiChar; table: PNormTable): boolean;
begin
  result := false;
  if p = nil then
    exit;
  if up <> nil then
  begin
    dec(PtrUInt(p), PtrUInt(up));
    repeat
      if up^ = #0 then
        break;
      if table[up[PtrUInt(p)]] <> up^ then
        exit;
      inc(up);
    until false;
  end;
  result := true;
end;

function IdemPCharAnsi(
  {$ifdef CPUX86NOTPIC}
  const table: TNormTable;
  {$else}
  const table: PNormTable;
  {$endif CPUX86NOTPIC}
  p: PUtf8Char; up: PAnsiChar): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  // in this local IdemPChar() version, p and up are expected to be <> nil
  result := false;
  dec(PtrUInt(p), PtrUInt(up));
  repeat
    if up^ = #0 then
      break;
    if table[up[PtrUInt(p)]] <> up^ then
      exit;
    inc(up);
  until false;
  result := true;
end;

function IdemPCharByte(
  {$ifdef CPUX86NOTPIC}
  const table: TNormTableByte;
  {$else}
  const table: PByteArray;
  {$endif CPUX86NOTPIC}
  p: PUtf8Char; up: PAnsiChar): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  // in this local IdemPChar() version, p and up are expected to be <> nil
  result := false;
  dec(PtrUInt(p), PtrUInt(up));
  repeat
    if up^ = #0 then
      break;
    if table[PtrInt(up[PtrUInt(p)])] <> PByte(up)^ then
      exit;
    inc(up);
  until false;
  result := true;
end;

function IdemPCharWithoutWhiteSpace(p: PUtf8Char; up: PAnsiChar): boolean;
begin
  result := False;
  if p = nil then
    exit;
  if up <> nil then
    while up^ <> #0 do
    begin
      while p^ <= ' ' do // trim white space
        if p^ = #0 then
          exit
        else
          inc(p);
      if up^ <> NormToUpperAnsi7[p^] then
        exit;
      inc(up);
      inc(p);
    end;
  result := true;
end;

function IdemPCharArray(p: PUtf8Char; const upArray: array of PAnsiChar): integer;
var
  w: word;
  up: ^PAnsiChar;
  {$ifdef CPUX86NOTPIC}
  tab: TNormTableByte absolute NormToUpperAnsi7;
  {$else}
  tab: PByteArray; // faster on PIC/ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if p <> nil then
  begin
    {$ifndef CPUX86NOTPIC}
    tab := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    w := tab[ord(p[0])] + tab[ord(p[1])] shl 8;
    up := @upArray[0];
    for result := 0 to high(upArray) do
      if (PWord(up^)^ = w) and
         IdemPCharByte(tab, p + 2, up^ + 2) then
        exit
      else
        inc(up);
  end;
  result := -1;
end;

function IdemPCharArray(p: PUtf8Char; const upArrayBy2Chars: RawUtf8): PtrInt;
var
  w: word;
  u: PWordArray; // better code generation when inlined
  {$ifdef CPUX86NOTPIC}
  tab: TNormTableByte absolute NormToUpperAnsi7;
  {$else}
  tab: PByteArray; // faster on PIC/ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if p <> nil then
  begin
    {$ifndef CPUX86NOTPIC}
    tab := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    w := tab[ord(p[0])] + tab[ord(p[1])] shl 8;
    u := pointer(upArrayBy2Chars);
    for result := 0 to pred(length(upArrayBy2Chars) shr 1) do
      if u[result] = w then
        exit;
  end;
  result := -1;
end;

function IdemPCharU(p, up: PUtf8Char): boolean;
begin
  result := false;
  if (p = nil) or
     (up = nil) then
    exit;
  while up^ <> #0 do
  begin
    if GetNextUtf8Upper(p) <> ord(up^) then
      exit;
    inc(up);
  end;
  result := true;
end;

function IdemPCharW(p: PWideChar; up: PUtf8Char): boolean;
begin
  result := false;
  if (p = nil) or
     (up = nil) then
    exit;
  while up^ <> #0 do
  begin
    if (p^ > #255) or
       (up^ <> AnsiChar(NormToUpperByte[ord(p^)])) then
      exit;
    inc(up);
    inc(p);
  end;
  result := true;
end;

function EndWith(const text, upText: RawUtf8): boolean;
var
  o: PtrInt;
begin
  o := length(text) - length(upText);
  result := (o >= 0) and
            IdemPChar(PUtf8Char(pointer(text)) + o, pointer(upText));
end;

function EndWithArray(const text: RawUtf8; const upArray: array of RawUtf8): integer;
var
  t, o: PtrInt;
  {$ifdef CPUX86NOTPIC}
  tab: TNormTableByte absolute NormToUpperAnsi7;
  {$else}            
  tab: PByteArray; // faster on PIC/ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  t := length(text);
  if t > 0 then
  begin
    {$ifndef CPUX86NOTPIC}
    tab := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    for result := 0 to high(upArray) do
    begin
      o := t - length(upArray[result]);
      if (o >= 0) and
         ((upArray[result] = '') or
          IdemPCharByte(tab, PUtf8Char(pointer(text)) + o,
            pointer(upArray[result]))) then
        exit;
    end;
  end;
  result := -1;
end;

function IdemFileExt(p: PUtf8Char; extup: PAnsiChar; sepChar: AnsiChar): boolean;
var
  ext: PUtf8Char;
begin
  if (p <> nil) and
     (extup <> nil) then
  begin
    ext := nil;
    repeat
      if p^ = sepChar then
        ext := p; // get last '.' position from p into ext
      inc(p);
    until p^ = #0;
    result := IdemPChar(ext, extup);
  end
  else
    result := false;
end;

function IdemFileExts(p: PUtf8Char; const extup: array of PAnsiChar;
  sepChar: AnsiChar): integer;
var
  ext: PUtf8Char;
begin
  result := -1;
  if (p <> nil) and
     (high(extup) > 0) then
  begin
    ext := nil;
    repeat
      if p^ = sepChar then
        ext := p; // get last '.' position from p into ext
      inc(p);
    until p^ = #0;
    if ext <> nil then
      result := IdemPCharArray(ext, extup);
  end;
end;

function PosCharAny(Str: PUtf8Char; Characters: PAnsiChar): PUtf8Char;
var
  s: PAnsiChar;
  c: AnsiChar;
begin
  if (Str <> nil) and
     (Characters <> nil) and
     (Characters^ <> #0) then
    repeat
      c := Str^;
      if c = #0 then
        break;
      s := Characters;
      repeat
        if s^ = c then
        begin
          result := Str;
          exit;
        end;
        inc(s);
      until s^ = #0;
      inc(Str);
    until false;
  result := nil;
end;

function PosI(uppersubstr: PUtf8Char; const str: RawUtf8): PtrInt;
var
  u: AnsiChar;
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7;
  {$else}
  table: PNormTable;
  {$endif CPUX86NOTPIC}
begin
  if uppersubstr <> nil then
  begin
    {$ifndef CPUX86NOTPIC}
    table := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    u := uppersubstr^;
    for result := 1 to Length(str) do
      if table[str[result]] = u then
        if IdemPCharAnsi(table, @PUtf8Char(pointer(str))[result],
             PAnsiChar(uppersubstr) + 1) then
          exit;
  end;
  result := 0;
end;

function StrPosI(uppersubstr, str: PUtf8Char): PUtf8Char;
var
  u: AnsiChar;
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7;
  {$else}
  table: PNormTable;
  {$endif CPUX86NOTPIC}
begin
  if (uppersubstr <> nil) and
     (str <> nil) then
  begin
    {$ifndef CPUX86NOTPIC}
    table := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    u := uppersubstr^;
    inc(uppersubstr);
    result := str;
    while result^ <> #0 do
    begin
      if table[result^] = u then
        if IdemPCharAnsi(table, result + 1, PAnsiChar(uppersubstr)) then
          exit;
      inc(result);
    end;
  end;
  result := nil;
end;

function PosIU(substr: PUtf8Char; const str: RawUtf8): integer;
var
  p: PUtf8Char;
begin
  if (substr <> nil) and
     (str <> '') then
  begin
    p := pointer(str);
    repeat
      if GetNextUtf8Upper(p) = ord(substr^) then
        if IdemPCharU(p, substr + 1) then
        begin
          result := p - pointer(str);
          exit;
        end;
    until p^ = #0;
  end;
  result := 0;
end;

function strspn(s, accept: pointer): integer;
// FPC is efficient at compiling this code, but is SLOWER when inlined
var
  p: PCardinal;
  c: AnsiChar;
  d: cardinal;
begin
  // returns size of initial segment of s which are in accept
  result := 0;
  repeat
    c := PAnsiChar(s)[result];
    if c = #0 then
      break;
    p := accept;
    repeat // stop as soon as we find any character not from accept
      d := p^;
      inc(p);
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
      d := d shr 8;
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
      d := d shr 8;
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
      d := d shr 8;
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
    until false;
    inc(result);
  until false;
end;

function strcspn(s, reject: pointer): integer;
// FPC is efficient at compiling this code, but is SLOWER when inlined
var
  p: PCardinal;
  c: AnsiChar;
  d: cardinal;
begin
  // returns size of initial segment of s which are not in reject
  result := 0;
  repeat
    c := PAnsiChar(s)[result];
    if c = #0 then
      break;
    p := reject;
    repeat // stop as soon as we find any character from reject
      d := p^;
      inc(p);
      if AnsiChar(d) = c then
        exit
      else if AnsiChar(d) = #0 then
        break;
      d := d shr 8;
      if AnsiChar(d) = c then
        exit
      else if AnsiChar(d) = #0 then
        break;
      d := d shr 8;
      if AnsiChar(d) = c then
        exit
      else if AnsiChar(d) = #0 then
        break;
      d := d shr 8;
      if AnsiChar(d) = c then
        exit
      else if AnsiChar(d) = #0 then
        break;
    until false;
    inc(result);
  until false;
end;

function StrCompL(P1, P2: pointer; L, Default: PtrInt): PtrInt;
var
  i: PtrInt;
begin
  i := 0;
  repeat
    result := PByteArray(P1)[i] - PByteArray(P2)[i];
    if result = 0 then
    begin
      inc(i);
      if i < L then
        continue
      else
        break;
    end;
    exit;
  until false;
  result := Default;
end;

function StrCompIL(P1, P2: pointer; L, Default: PtrInt): PtrInt;
var
  i: PtrInt;
  {$ifdef CPUX86NOTPIC}
  tab: TNormTableByte absolute NormToUpperAnsi7Byte;
  {$else}
  tab: PByteArray; // faster on PIC/ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  i := 0;
  {$ifndef CPUX86NOTPIC}
  tab := @NormToUpperAnsi7Byte;
  {$endif CPUX86NOTPIC}
  repeat
    if tab[PByteArray(P1)[i]] = tab[PByteArray(P2)[i]] then
    begin
      inc(i);
      if i < L then
        continue
      else
        break;
    end;
    result := PByteArray(P1)[i] - PByteArray(P2)[i];
    exit;
  until false;
  result := Default;
end;

function StrIComp(Str1, Str2: pointer): PtrInt;
var
  C1, C2: byte; // integer/PtrInt are actually slower on FPC
  {$ifdef CPUX86NOTPIC}
  table: TNormTableByte absolute NormToUpperAnsi7Byte;
  {$else}
  table: PByteArray;
  {$endif CPUX86NOTPIC}
begin
  result := PtrInt(PtrUInt(Str2)) - PtrInt(PtrUInt(Str1));
  if result <> 0 then
    if Str1 <> nil then
      if Str2 <> nil then
      begin
        {$ifndef CPUX86NOTPIC}
        table := @NormToUpperAnsi7Byte;
        {$endif CPUX86NOTPIC}
        repeat
          C1 := table[PByteArray(Str1)[0]];
          C2 := table[PByteArray(Str1)[result]];
          inc(PByte(Str1));
        until (C1 = 0) or
              (C1 <> C2);
        result := C1 - C2;
      end
      else
        // Str2=''
        result := 1
    else
      // Str1=''
      result := -1;
end;

function GetLineContains(p, pEnd, up: PUtf8Char): boolean;
var
  i: PtrInt;
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7Byte;
  {$else}
  table: PNormTable;
  {$endif CPUX86NOTPIC}
label
  Fnd1, LF1, Fnd2, LF2, Ok; // ugly but fast
begin
  if (p <> nil) and
     (up <> nil) then
  begin
    {$ifndef CPUX86NOTPIC}
    table := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    if pEnd = nil then
      repeat
        if p^ <= #13 then // p^ into a temp var is slower
          goto LF1
        else if table[p^] = up^ then
          goto Fnd1;
        inc(p);
        continue;
LF1:    if (p^ = #0) or
           (p^ = #13) or
           (p^ = #10) then
          break;
        inc(p);
        continue;
Fnd1:   i := 0;
        repeat
          inc(i);
          if up[i] <> #0 then
            if up[i] = table[p[i]] then
              continue
            else
              break
          else
          begin
Ok:         result := true; // found
            exit;
          end;
        until false;
        inc(p);
      until false
    else
      repeat
        if p >= pEnd then
          break;
        if p^ <= #13 then
          goto LF2
        else if table[p^] = up^ then
          goto Fnd2;
        inc(p);
        continue;
LF2:    if (p^ = #13) or
           (p^ = #10) then
          break;
        inc(p);
        continue;
Fnd2:   i := 0;
        repeat
          inc(i);
          if up[i] = #0 then
            goto Ok;
          if p + i >= pEnd then
            break;
        until up[i] <> table[p[i]];
        inc(p);
      until false;
  end;
  result := false;
end;

function ContainsUtf8(p, up: PUtf8Char): boolean;
var
  u: PByte;
begin
  if (p <> nil) and
     (up <> nil) and
     (up^ <> #0) then
  begin
    result := true;
    repeat
      u := pointer(up);
      repeat
        if GetNextUtf8Upper(p) <> u^ then
          break
        else
          inc(u);
        if u^ = 0 then
          exit; // up^ was found inside p^
      until false;
      p := FindNextUtf8WordBegin(p);
    until p = nil;
  end;
  result := false;
end;

function GetNextUtf8Upper(var U: PUtf8Char): Ucs4CodePoint;
begin
  result := ord(U^);
  if result = 0 then
    exit;
  if result <= 127 then
  begin
    inc(U);
    result := NormToUpperByte[result];
    exit;
  end;
  result := UTF8_TABLE.GetHighUtf8Ucs4(U);
  if (result <= 255) and
     (WinAnsiConvert.AnsiToWide[result] <= 255) then
    result := NormToUpperByte[result];
end;

function FindNextUtf8WordBegin(U: PUtf8Char): PUtf8Char;
var
  c: cardinal;
  V: PUtf8Char;
begin
  result := nil;
  repeat
    c := GetNextUtf8Upper(U);
    if c = 0 then
      exit;
  until (c >= 127) or
        not (tcWord in TEXT_BYTES[c]); // not ['0'..'9', 'a'..'z', 'A'..'Z']
  repeat
    V := U;
    c := GetNextUtf8Upper(U);
    if c = 0 then
      exit;
  until (c < 127) and
        (tcWord in TEXT_BYTES[c]);
  result := V;
end;

function AnsiICompW(u1, u2: PWideChar): PtrInt;
var
  C1, C2: PtrInt;
  {$ifdef CPUX86NOTPIC}
  table: TNormTableByte absolute NormToUpperAnsi7Byte;
  {$else}
  table: PByteArray;
  {$endif CPUX86NOTPIC}
begin
  if u1 <> u2 then
    if u1 <> nil then
      if u2 <> nil then
      begin
        {$ifndef CPUX86NOTPIC}
        table := @NormToUpperAnsi7Byte;
        {$endif CPUX86NOTPIC}
        repeat
          C1 := PtrInt(u1^);
          C2 := PtrInt(u2^);
          result := C1 - C2;
          if result <> 0 then
          begin
            if (C1 > 255) or
               (C2 > 255) then
              exit;
            result := table[C1] - table[C2];
            if result <> 0 then
              exit;
          end;
          if (C1 = 0) or
             (C2 = 0) then
            break;
          inc(u1);
          inc(u2);
        until false;
      end
      else
        result := 1
    else  // u2=''
      result := -1
  else // u1=''
    result := 0;      // u1=u2
end;

function AnsiIComp(Str1, Str2: pointer): PtrInt;
var
  C1, C2: byte; // integer/PtrInt are actually slower on FPC
  lookupper: PByteArray; // better x86-64 / PIC asm generation
begin
  result := PtrInt(PtrUInt(Str2)) - PtrInt(PtrUInt(Str1));
  if result <> 0 then
    if Str1 <> nil then
      if Str2 <> nil then
      begin
        lookupper := @NormToUpperByte;
        repeat
          C1 := lookupper[PByteArray(Str1)[0]];
          C2 := lookupper[PByteArray(Str1)[result]];
          inc(PByte(Str1));
        until (C1 = 0) or
              (C1 <> C2);
        result := C1 - C2;
      end
      else
        result := 1
    else  // Str2=''
      result := -1;     // Str1=''
end;

function SortDynArrayAnsiStringI(const A, B): integer;
begin
  result := StrIComp(PUtf8Char(A), PUtf8Char(B));
end;

function SortDynArrayPUtf8CharI(const A, B): integer;
begin
  result := StrIComp(PUtf8Char(A), PUtf8Char(B));
end;

function SortDynArrayStringI(const A, B): integer;
begin
  {$ifdef UNICODE}
  result := AnsiICompW(PWideChar(A), PWideChar(B));
  {$else}
  result := StrIComp(PUtf8Char(A), PUtf8Char(B));
  {$endif UNICODE}
end;

function SortDynArrayUnicodeStringI(const A, B): integer;
begin
  result := AnsiICompW(PWideChar(A), PWideChar(B));
end;

function ConvertCaseUtf8(P: PUtf8Char; const Table: TNormTableByte): PtrInt;
var
  D, S: PUtf8Char;
  c: PtrUInt;
  extra, i: PtrInt;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
begin
  result := 0;
  if P = nil then
    exit;
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  D := P;
  repeat
    c := byte(P[0]);
    inc(P);
    if c = 0 then
      break;
    if c <= 127 then
    begin
      D[result] := AnsiChar(Table[c]);
      inc(result);
    end
    else
    begin
      extra := utf8.Bytes[c];
      if extra = 0 then
        exit; // invalid leading byte
      i := 0;
      repeat
        if byte(P[i]) and $c0 <> $80 then
          exit; // invalid input content
        c := (c shl 6) + byte(P[i]);
        inc(i);
      until i = extra;
      with utf8.Extra[extra] do
      begin
        dec(c, offset);
        if c < minimum then
          exit; // invalid input content
      end;
      if (c <= 255) and
         (Table[c] <= 127) then
      begin
        D[result] := AnsiChar(Table[c]);
        inc(result);
        inc(P, extra);
        continue;
      end;
      S := P - 1;
      inc(P, extra);
      inc(extra);
      MoveSmall(S, D + result, extra);
      inc(result, extra);
    end;
  until false;
end;

function UpperCaseU(const S: RawUtf8): RawUtf8;
var
  LS, LD: integer;
begin
  LS := length(S);
  FastSetString(result, pointer(S), LS);
  LD := ConvertCaseUtf8(pointer(result), NormToUpperByte);
  if LS <> LD then
    SetLength(result, LD);
end;

function LowerCaseU(const S: RawUtf8): RawUtf8;
var
  LS, LD: integer;
begin
  LS := length(S);
  FastSetString(result, pointer(S), LS);
  LD := ConvertCaseUtf8(pointer(result), NormToLowerByte);
  if LS <> LD then
    SetLength(result, LD);
end;

function Utf8IComp(u1, u2: PUtf8Char): PtrInt;
var
  c2: PtrInt;
  {$ifdef CPUX86NOTPIC}
  table: TNormTableByte absolute NormToUpperByte;
  {$else}
  table: PByteArray;
  {$endif CPUX86NOTPIC}
begin
  // fast UTF-8 comparison using the NormToUpper[] array for all 8-bit values
  {$ifndef CPUX86NOTPIC}
  table := @NormToUpperByte;
  {$endif CPUX86NOTPIC}
  if u1 <> u2 then
    if u1 <> nil then
      if u2 <> nil then
        repeat
          result := ord(u1^);
          c2 := ord(u2^);
          if result <= 127 then
            if result <> 0 then
            begin
              inc(u1);
              result := table[result];
              if c2 <= 127 then
              begin
                if c2 = 0 then
                  exit; // u1>u2 -> return u1^
                inc(u2);
                dec(result, table[c2]);
                if result <> 0 then
                  exit;
                continue;
              end;
            end
            else
            begin
              // u1^=#0 -> end of u1 reached
              if c2 <> 0 then    // end of u2 reached -> u1=u2 -> return 0
                result := -1;    // u1<u2
              exit;
            end
          else
          begin
            result := UTF8_TABLE.GetHighUtf8Ucs4(u1);
            if result <= 255 then
              result := table[result]; // 8-bit to upper, 32-bit as is
          end;
          if c2 <= 127 then
          begin
            if c2 = 0 then
              exit; // u1>u2 -> return u1^
            inc(u2);
            dec(result, table[c2]);
            if result <> 0 then
              exit;
            continue;
          end
          else
          begin
            c2 := UTF8_TABLE.GetHighUtf8Ucs4(u2);
            if c2 <= 255 then
              dec(result, table[c2])
            else // 8-bit to upper
              dec(result, c2); // 32-bit widechar returns diff
            if result <> 0 then
              exit;
          end;
        until false
      else
        result := 1 // u2=''
    else
      result := -1  // u1=''
  else
    result := 0;    // u1=u2
end;

function Utf8ILComp(u1, u2: PUtf8Char; L1, L2: cardinal): PtrInt;
var
  c2: PtrInt;
  extra, i: integer;
  {$ifdef CPUX86NOTPIC}
  table: TNormTableByte absolute NormToUpperByte;
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  table: PByteArray;
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  neg, pos;
begin
  // fast UTF-8 comparison using the NormToUpper[] array for all 8-bit values
  {$ifndef CPUX86NOTPIC}
  table := @NormToUpperByte;
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  if u1 <> u2 then
    if (u1 <> nil) and
       (L1 <> 0) then
      if (u2 <> nil) and
         (L2 <> 0) then
        repeat
          result := ord(u1^);
          c2 := ord(u2^);
          inc(u1);
          dec(L1);
          if result <= 127 then
          begin
            result := table[result];
            if c2 <= 127 then
            begin
              // 'a'..'z' / 'A'..'Z' case insensitive comparison
              dec(result, table[c2]);
              dec(L2);
              inc(u2);
              if result <> 0 then
                // found unmatching char
                exit
              else if L1 <> 0 then
                if L2 <> 0 then
                  // L1>0 and L2>0 -> next char
                  continue
                else
                  // L1>0 and L2=0 -> u1>u2
                  goto pos
              else
              if L2 <> 0 then
                // L1=0 and L2>0 -> u1<u2
                goto neg
              else
                // L1=0 and L2=0 -> u1=u2 -> returns 0
                exit;
            end;
          end
          else
          begin
            // Win-1252 case insensitive comparison
            extra := utf8.Bytes[result];
            if extra = 0 then
              // invalid leading byte
              goto neg;
            dec(L1, extra);
            if integer(L1) < 0 then
              goto neg;
            i := 0;
            repeat
              result := result shl 6;
              inc(result, ord(u1[i]));
              inc(i);
            until i = extra;
            inc(u1, extra);
            dec(result, utf8.Extra[extra].offset);
            if result and $ffffff00 = 0 then
              // 8-bit to upper conversion, 32-bit as is
              result := table[result];
          end;
          // here result=NormToUpper[u1^]
          inc(u2);
          dec(L2);
          if c2 <= 127 then
          begin
            dec(result, table[c2]);
            if result <> 0 then
              // found unmatching char
              exit;
          end
          else
          begin
            extra := utf8.Bytes[c2];
            if extra = 0 then
              goto pos;
            dec(L2, extra);
            if integer(L2) < 0 then
              goto pos;
            i := 0;
            repeat
              c2 := c2 shl 6;
              inc(c2, ord(u2[i]));
              inc(i);
            until i = extra;
            inc(u2, extra);
            dec(c2, utf8.Extra[extra].offset);
            if c2 and $ffffff00 = 0 then
              // 8-bit to upper
              dec(result, table[c2])
            else
              // returns 32-bit diff
              dec(result, c2);
            if result <> 0 then
              // found unmatching char
              exit;
          end;
          // here we have result=NormToUpper[u2^]-NormToUpper[u1^]=0
          if L1 = 0 then
            // test if we reached end of u1 or end of u2
            if L2 = 0 then
              // u1=u2
              exit
            else
              // u1<u2
              goto neg
          else
          if L2 = 0 then
            // u1>u2
            goto pos;
        until false
      else
pos:    // u2='' or u1>u2
        result := 1
    else
neg:  // u1='' or u1<u2
      result := -1
  else
    // u1=u2
    result := 0;
end;

function SameTextU(const S1, S2: RawUtf8): boolean;
// checking UTF-8 lengths is not accurate: surrogates may be confusing
begin
  result := Utf8IComp(pointer(S1), pointer(S2)) = 0;
end;

function FindAnsi(A, UpperValue: PAnsiChar): boolean;
var
  ValueStart: PAnsiChar;
begin
  result := false;
  if (A = nil) or
     (UpperValue = nil) then
    exit;
  ValueStart := UpperValue;
  repeat
    // test beginning of word
    repeat
      if A^ = #0 then
        exit
      else if tcWord in TEXT_CHARS[NormToUpper[A^]] then
        break
      else
        inc(A);
    until false;
    // check if this word is the UpperValue
    UpperValue := ValueStart;
    repeat
      if NormToUpper[A^] <> UpperValue^ then
        break;
      inc(UpperValue);
      if UpperValue^ = #0 then
      begin
        result := true; // UpperValue found!
        exit;
      end;
      inc(A);
      if A^ = #0 then
        exit;
    until false;
    // find beginning of next word
    repeat
      if A^ = #0 then
        exit
      else if not (tcWord in TEXT_CHARS[NormToUpper[A^]]) then
        break
      else
        inc(A);
    until false;
  until false;
end;

function FindUnicode(PW, Upper: PWideChar; UpperLen: PtrInt): boolean;
var
  Start: PWideChar;
  w: PtrUInt;
begin
  result := false;
  if (PW = nil) or
     (Upper = nil) then
    exit;
  repeat
    // go to beginning of next word
    repeat
      w := ord(PW^);
      if w = 0 then
        exit
      else if (w > 126) or
              (tcWord in TEXT_BYTES[w]) then
        Break;
      inc(PW);
    until false;
    Start := PW;
    // search end of word matching UpperLen characters
    repeat
      inc(PW);
      w := ord(PW^);
    until (PW - Start >= UpperLen) or
          (w = 0) or
          ((w < 126) and
           not (tcWord in TEXT_BYTES[w]));
    if PW - Start >= UpperLen then
      if Unicode_CompareString(Start, Upper, UpperLen, UpperLen,
           {ignorecase=}true) = 2 then
      begin
        result := true; // case-insensitive match found
        exit;
      end;
    // not found: go to end of current word
    repeat
      w := ord(PW^);
      if w = 0 then
        exit
      else if ((w < 126) and
              not (tcWord in TEXT_BYTES[w])) then
        Break;
      inc(PW);
    until false;
  until false;
end;

function FindUtf8(U: PUtf8Char; UpperValue: PAnsiChar): boolean;
var
  ValueStart: PAnsiChar;
  c: PtrUInt;
  FirstChar: AnsiChar;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  Next;
begin
  result := false;
  if (U = nil) or
     (UpperValue = nil) then
    exit;
  // handles 8-bits WinAnsi chars inside UTF-8 encoded data
  {$ifndef CPUX86NOTPIC}
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  FirstChar := UpperValue^;
  ValueStart := UpperValue + 1;
  repeat
    // test beginning of word
    repeat
      c := byte(U^);
      inc(U);
      if c = 0 then
        exit;
      if c <= 127 then
      begin
        if tcWord in TEXT_BYTES[c] then
          if PAnsiChar(@NormToUpper)[c] <> FirstChar then
            goto Next
          else
            break;
      end
      else if c and $20 = 0 then
      begin
        // fast direct process of $0..$7ff codepoints including accents
        c := ((c shl 6) + byte(U^)) - $3080;
        inc(U);
        if c <= 255 then
        begin
          c := NormToUpperByte[c];
          if tcWord in TEXT_BYTES[c] then
            if AnsiChar(c) <> FirstChar then
              goto Next
            else
              break;
        end;
      end
      else if utf8.Bytes[c] = 0 then
        // invalid leading byte
        exit
      else
        // just ignore surrogates for soundex
        inc(U, utf8.Bytes[c]);
    until false;
    // here we had the first char match -> check if this word match UpperValue
    UpperValue := ValueStart;
    repeat
      if UpperValue^ = #0 then
      begin
        result := true; // UpperValue found!
        exit;
      end;
      c := byte(U^);
      inc(U); // next chars
      if c = 0 then
        exit
      else if c <= 127 then
      begin
        if PAnsiChar(@NormToUpper)[c] <> UpperValue^ then
          break;
      end
      else if c and $20 = 0 then
      begin
        c := ((c shl 6) + byte(U^)) - $3080;
        inc(U);
        if (c > 255) or
           (PAnsiChar(@NormToUpper)[c] <> UpperValue^) then
          break;
      end
      else
      begin
        if utf8.Bytes[c] = 0 then
          // invalid leading byte
          exit
        else
          inc(U, utf8.Bytes[c]);
        break;
      end;
      inc(UpperValue);
    until false;
Next: // find beginning of next word
    U := FindNextUtf8WordBegin(U);
  until U = nil;
end;

function IdemPropNameU(const P1, P2: RawUtf8): boolean;
var
  L: PtrInt;
begin
  L := length(P1);
  if length(P2) = L then
    result := IdemPropNameUSameLen(pointer(P1), pointer(P2), L)
  else
    result := false;
end;

function UpperCopy255(dest: PAnsiChar; const source: RawUtf8): PAnsiChar;
begin
  if source <> '' then
    result := UpperCopy255Buf(dest, pointer(source),
      PStrLen(PAnsiChar(pointer(source)) - _STRLEN)^)
  else
    result := dest;
end;

function UpperCopy255Buf(dest: PAnsiChar; source: PUtf8Char; sourceLen: PtrInt): PAnsiChar;
var
  i, c, d {$ifdef CPU64}, _80, _61, _7b {$endif}: PtrUInt;
begin
  if sourceLen <> 0 then
  begin
    if sourceLen > 248 then
      sourceLen := 248; // avoid buffer overflow
    // we allow to copy up to 3/7 more chars in Dest^ since its size is 255
    {$ifdef CPU64}
    // unbranched uppercase conversion of 8 chars blocks
    _80 := PtrUInt($8080808080808080); // use registers for constants
    _61 := $6161616161616161;
    _7b := $7b7b7b7b7b7b7b7b;
    for i := 0 to sourceLen shr 3 do
    begin
      c := PPtrUIntArray(source)^[i];
      d := c or _80;
      PPtrUIntArray(dest)^[i] := c - ((d - PtrUInt(_61)) and
        not (d - _7b)) and ((not c) and _80) shr 2;
    end;
    {$else}
    // unbranched uppercase conversion of 4 chars blocks
    for i := 0 to sourceLen shr 2 do
    begin
      c := PPtrUIntArray(source)^[i];
      d := c or PtrUInt($80808080);
      PPtrUIntArray(dest)^[i] := c - ((d - PtrUInt($61616161)) and
        not (d - PtrUInt($7b7b7b7b))) and ((not c) and PtrUInt($80808080)) shr 2;
    end;
    {$endif CPU64}
  end;
  result := dest + sourceLen; // return the exact size
end;

function UpperCopyWin255(dest: PWinAnsiChar; const source: RawUtf8): PWinAnsiChar;
var
  i, L: PtrInt;
  {$ifdef CPUX86NOTPIC}
  tab: TNormTableByte absolute NormToUpperByte;
  {$else}
  tab: PByteArray; // faster on PIC/ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if source = '' then
    result := dest
  else
  begin
    L := PStrLen(PAnsiChar(pointer(source)) - _STRLEN)^;
    if L > 250 then
      L := 250; // avoid buffer overflow
    result := dest + L;
    {$ifndef CPUX86NOTPIC}
    tab := @NormToUpperByte;
    {$endif CPUX86NOTPIC}
    for i := 0 to L - 1 do
      dest[i] := AnsiChar(tab[PByteArray(source)[i]]);
  end;
end;

function Utf8UpperCopy(Dest, Source: PUtf8Char; SourceChars: cardinal): PUtf8Char;
var
  c: cardinal;
  endSource, endSourceBy4, up: PUtf8Char;
  extra, i: PtrInt;
  {$ifdef CPUX86NOTPIC}
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  by1, by4, set1; // ugly but faster
begin
  if (Source <> nil) and
     (Dest <> nil) then
  begin
    {$ifndef CPUX86NOTPIC}
    utf8 := @UTF8_TABLE;
    {$endif CPUX86NOTPIC}
    // first handle trailing 7-bit ASCII chars, by quad (Sha optimization)
    endSource := Source + SourceChars;
    endSourceBy4 := endSource - 4;
    up := @NormToUpper;
    if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
       (Source <= endSourceBy4) then
      repeat
by4:    c := PCardinal(Source)^;
        if c and $80808080 <> 0 then
          goto by1; // break on first non ASCII quad
        inc(Source, 4);
        Dest[0] := up[ToByte(c)];
        Dest[1] := up[ToByte(c shr 8)];
        Dest[2] := up[ToByte(c shr 16)];
        Dest[3] := up[ToByte(c shr 24)];
        inc(Dest, 4);
      until Source > endSourceBy4;
    // generic loop, handling one UCS4 CodePoint per iteration
    if Source < endSource then
      repeat
by1:    c := byte(Source^);
        inc(Source);
        if c <= 127 then
        begin
          Dest^ := up[c];
set1:     inc(Dest);
          if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(Source) and 3 = 0) and{$endif}
             (Source < endSourceBy4) then
            goto by4
          else if Source < endSource then
            continue
          else
            break;
        end
        else
        begin
          extra := utf8.Bytes[c];
          if (extra = 0) or
             (Source + extra > endSource) then
            break;
          i := 0;
          repeat
            c := (c shl 6) + byte(Source[i]);
            inc(i)
          until i = extra;
          with utf8.Extra[extra] do
          begin
            dec(c, offset);
            if c < minimum then
              break; // invalid input content
          end;
          if (c <= 255) and
             (up[c] <= #127) then
          begin
            Dest^ := up[c];
            inc(Source, extra);
            goto set1;
          end;
          Dest^ := Source[-1];
          repeat // here we now extra>0 - just copy UTF-8 input untouched
            inc(Dest);
            Dest^ := Source^;
            inc(Source);
            dec(extra);
            if extra = 0 then
              goto set1;
          until false;
        end;
      until false;
  end;
  result := Dest;
end;

function Utf8UpperCopy255(dest: PAnsiChar; const source: RawUtf8): PUtf8Char;
var
  L: integer;
begin
  L := length(source);
  if L > 0 then
  begin
    if L > 250 then
      L := 250; // avoid buffer overflow
    result := Utf8UpperCopy(pointer(dest), pointer(source), L);
  end
  else
    result := pointer(dest);
end;

function UpperCopy255W(dest: PAnsiChar; const source: SynUnicode): PAnsiChar;
begin
  result := UpperCopy255W(dest, pointer(source), length(source));
end;

function UpperCopy255W(dest: PAnsiChar; source: PWideChar; L: PtrInt): PAnsiChar;
var
  c: PtrUInt;
  d: byte;
  lookupper: PByteArray; // better x86-64 / PIC asm generation
begin
  if L > 0 then
  begin
    if L > 250 then
      L := 250; // avoid buffer overflow
    lookupper := @NormToUpperAnsi7Byte;
    repeat
      c := PWord(source)^;
      d := ord('?');
      if c < 255 then
        d := lookupper[c];
      dest^ := AnsiChar(d);
      inc(dest);
      inc(source);
      dec(L);
    until L = 0;
  end;
  result := dest;
end;

function UpperCopy(dest: PAnsiChar; const source: RawUtf8): PAnsiChar;
var
  s: PAnsiChar;
  c: byte;
  lookupper: PByteArray; // better x86-64 / PIC asm generation
begin
  s := pointer(source);
  if s <> nil then
  begin
    lookupper := @NormToUpperAnsi7Byte;
    repeat
      c := lookupper[ord(s^)];
      if c = 0 then
        break;
      dest^ := AnsiChar(c);
      inc(s);
      inc(dest);
    until false;
  end;
  result := dest;
end;

function UpperCopyShort(dest: PAnsiChar; const source: shortstring): PAnsiChar;
var
  s: PByteArray;
  i: PtrInt;
  lookupper: PByteArray; // better x86-64 / PIC asm generation
begin
  s := @source;
  lookupper := @NormToUpperAnsi7Byte;
  for i := 1 to s[0] do
  begin
    dest^ := AnsiChar(lookupper[s[i]]);
    inc(dest);
  end;
  result := dest;
end;

function UpperCaseUnicode(const S: RawUtf8): RawUtf8;
var
  tmp: TSynTempBuffer;
  len: integer;
begin
  if S = '' then
  begin
    result := '';
    exit;
  end;
  tmp.Init(length(s) * 2);
  len := Utf8ToWideChar(tmp.buf, pointer(S), length(S)) shr 1;
  RawUnicodeToUtf8(tmp.buf, Unicode_InPlaceUpper(tmp.buf, len),result);
  tmp.Done;
end;

function LowerCaseUnicode(const S: RawUtf8): RawUtf8;
var
  tmp: TSynTempBuffer;
  len: integer;
begin
  if S = '' then
  begin
    result := '';
    exit;
  end;
  tmp.Init(length(s) * 2);
  len := Utf8ToWideChar(tmp.buf, pointer(S), length(S)) shr 1;
  RawUnicodeToUtf8(tmp.buf, Unicode_InPlaceLower(tmp.buf, len),result);
  tmp.Done;
end;

function IsCaseSensitive(const S: RawUtf8): boolean;
begin
  result := IsCaseSensitive(pointer(S), length(S));
end;

function IsCaseSensitive(P: PUtf8Char; PLen: PtrInt): boolean;
begin
  result := true;
  if (P <> nil) and
     (PLen > 0) then
    repeat
      if ord(P^) in [ord('a')..ord('z'), ord('A')..ord('Z')] then
        exit;
      inc(P);
      dec(PLen);
    until PLen = 0;
  result := false;
end;

function UpperCase(const S: RawUtf8): RawUtf8;
var
  L, i: PtrInt;
begin
  L := length(S);
  FastSetString(Result, pointer(S), L);
  for i := 0 to L - 1 do
    if PByteArray(result)[i] in [ord('a')..ord('z')] then
      dec(PByteArray(result)[i], 32);
end;

procedure UpperCaseCopy(Text: PUtf8Char; Len: PtrInt; var result: RawUtf8);
var
  i: PtrInt;
begin
  FastSetString(result, Text, Len);
  for i := 0 to Len - 1 do
    if PByteArray(result)[i] in [ord('a')..ord('z')] then
      dec(PByteArray(result)[i], 32);
end;

procedure UpperCaseCopy(const Source: RawUtf8; var Dest: RawUtf8);
var
  L, i: PtrInt;
begin
  L := length(Source);
  FastSetString(Dest, pointer(Source), L);
  for i := 0 to L - 1 do
    if PByteArray(Dest)[i] in [ord('a')..ord('z')] then
      dec(PByteArray(Dest)[i], 32);
end;

procedure UpperCaseSelf(var S: RawUtf8);
var
  i: PtrInt;
  P: PByteArray;
begin
  P := UniqueRawUtf8(S);
  for i := 0 to length(S) - 1 do
    if P[i] in [ord('a')..ord('z')] then
      dec(P[i], 32);
end;

function LowerCase(const S: RawUtf8): RawUtf8;
var
  L, i: PtrInt;
begin
  L := length(S);
  FastSetString(result, pointer(S), L);
  for i := 0 to L - 1 do
    if PByteArray(result)[i] in [ord('A')..ord('Z')] then
      inc(PByteArray(result)[i], 32);
end;

procedure LowerCaseCopy(Text: PUtf8Char; Len: PtrInt; var result: RawUtf8);
var
  i: PtrInt;
begin
  FastSetString(result, Text, Len);
  for i := 0 to Len - 1 do
    if PByteArray(result)[i] in [ord('A')..ord('Z')] then
      inc(PByteArray(result)[i], 32);
end;

procedure LowerCaseSelf(var S: RawUtf8);
var
  i: PtrInt;
  P: PByteArray;
begin
  P := UniqueRawUtf8(S);
  for i := 0 to length(S) - 1 do
    if P[i] in [ord('A')..ord('Z')] then
      inc(P[i], 32);
end;


{ ************** Operating-System Independent Unicode Process }

// freely inspired by Bero's PUCU library, released under zlib license
//  https://github.com/BeRo1985/pucu  (C)2016-2020 Benjamin Rosseaux

type
  // 20016 bytes for full Unicode 10.0 case folding branchless conversion :)
  TUnicodeUpperTable = object
    Block: array[0..37, 0..127] of integer;
    IndexHi: array[0..271] of byte;
    IndexLo: array[0..8, 0..31] of byte;
    // branchless Unicode 10.0 uppercase folding using our internal tables
    function Ucs4Upper(c: Ucs4CodePoint): Ucs4CodePoint;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PUnicodeUpperTable = ^TUnicodeUpperTable;

const
  UU_BLOCK_HI = 7;
  UU_BLOCK_LO = 127;
  UU_INDEX_HI = 5;
  UU_INDEX_LO = 31;
  UU_MAX = $10ffff;

var
  UU: TUnicodeUpperTable = (
    Block: (
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 743, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -32, -32, -
      32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, -32, 0, -32, -32, -32, -32, -32, -32, -32, 121),
     (0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -232, 0, -1, 0, -1, 0, -1, 0, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, -1, 0, -1, 0, -1, -300),
     (195, 0, 0, -1, 0, -1, 0, 0, -1, 0, 0, 0, -1, 0, 0, 0, 0, 0, -1, 0,
      0, 97, 0, 0, 0, -1, 163, 0, 0, 0, 130, 0, 0, -1, 0, -1, 0, -1, 0, 0, -1, 0,
      0, 0, 0, -1, 0, 0, -1, 0, 0, 0, -1, 0, -1, 0, 0, -1, 0, 0, 0, -1, 0, 56, 0,
      0, 0, 0, 0, -1, -2, 0, -1, -2, 0, -1, -2, 0, -1, 0, -1, 0, -1, 0, -1, 0, -
      1, 0, -1, 0, -1, 0, -1, -79, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, 0, -1, -2, 0, -1, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1),
     (0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 10815,
      10815, 0, -1, 0, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 10783, 10780,
      10782, -210, -206, 0, -205, -205, 0, -202, 0, -203, 42319, 0, 0, 0, -205,
      42315, 0, -207, 0, 42280, 42308, 0, -209, -211, 42308, 10743, 42305, 0, 0,
      -211, 0, 10749, -213, 0, 0, -214, 0, 0, 0, 0, 0, 0, 0, 10727, 0, 0),
     (-218, 0, 0, -218, 0, 0, 0, 42282, -218, -69, -217, -217, -71, 0, 0, 0, 0, 0,
      -219, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42261, 42258, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, -1, 0, -1, 0, 0, 0, -1, 0, 0, 0, 130, 130, 130, 0, 0),
     (0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -38, -37, -37, -37, 0, -32,
      -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
      -32, -31, -32, -32, -32, -32, -32, -32, -32, -32, -32, -64, -63, -63, 0,
      -62, -57, 0, 0, 0, -47, -54, -8, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, -86, -80, 7, -116, 0, -96, 0, 0,
      -1, 0, 0, -1, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, -32, -80, -80, -80, -80, -80, -80, -80, -80, -80,
      -80, -80, -80, -80, -80, -80, -80, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1),
     (0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, -15, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1),
     (0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48,
      -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48,
      -48, -48, -48, -48, -48),
     (-48, -48, -48, -48, -48, -48, -48, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -8, -8, -8,
      -8, -8, -8, 0, 0),
     (-6254, -6253, -6244, -6242, -6242, -6243, -6236, -6181, 35266, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35332, 0, 0, 0, 3814, 0, 0),
     (0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1),
     (0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, 0, 0, 0, 0, -59, 0, 0, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1),
     (8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0,
      8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 8, 0, 8, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8,
      8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 74, 74, 86, 86, 86, 86, 100, 100,
      128, 128, 112, 112, 126, 126, 0, 0),
     (8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0,
      0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8,
      8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      -7205, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -28, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16,
      -16, -16, -16, -16, -16),
     (0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -26, -26, -26, -26,
      -26, -26, -26, -26, -26, -26, -26, -26, -26, -26, -26, -26, -26, -26, -26,
      -26, -26, -26, -26, -26, -26, -26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48,
      -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48,
      -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48, -48,
      -48, -48, -48, -48, -48, -48, -48, 0, 0, -1, 0, 0, 0, -10795, -10792, 0, -1,
      0, -1, 0, -1, 0, 0, 0, 0, 0, 0, -1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0,
      0, -1, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (-7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264,
      -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264,
      -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264, -7264,
      -7264, -7264, -7264, -7264, -7264, 0, -7264, 0, 0, 0, 0, 0, -7264, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, 0, 0, -1),
     (0, -1, 0, -1, 0,  -1, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, -1, 0, 0,
      0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -928, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864),
     (-38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864, -38864,
      -38864, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -40, -40, -40, -40,
      -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40,
      -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40,
      -40, -40, -40, -40, -40, -40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -40, -40,
      -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40,
      -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40, -40,
      -40, -40, -40, -40, 0, 0, 0, 0), (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64,
      -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64,
      -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64, -64,
      -64, -64, -64, -64, -64, -64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -32, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
      -32, -32, -32, -32, -32, -32, -32, -32, -32, -32, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34,
      -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34,
      -34, -34, -34, -34, -34, -34, -34, -34, -34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    );
    IndexHi: (0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3, 3, 3, 5, 6, 7, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 8, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3);
    IndexLo: ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 12, 12, 12, 12, 12, 12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12), (12, 12, 12, 12, 12,
      12, 12, 13, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
      12, 14, 15, 12, 16, 17, 18, 19), (12, 12, 20, 21, 12, 12, 12, 12, 12, 22,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 23, 24, 25, 12, 12,
      12, 12, 12), (12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12), (12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 26, 27, 28, 29, 12, 12, 12, 12,
      12, 12, 30, 31, 12, 12, 12, 12, 12, 12, 12, 12), (12, 12, 12, 12, 12, 12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
      12, 12, 12, 12, 12, 32, 12), (12, 12, 12, 12, 12, 12, 12, 12, 33, 34, 12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 35, 12, 12, 12, 12,
      12, 12), (12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
      12, 36, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12), (12, 12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 37, 12, 12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12));
  );

function TUnicodeUpperTable.Ucs4Upper(c: Ucs4CodePoint): Ucs4CodePoint;
var
  i: PtrUInt;
begin
  i := c shr UU_BLOCK_HI;
  result := Ucs4CodePoint(PtrInt(c) + Block[IndexLo[
    IndexHi[i shr UU_INDEX_HI], i and UU_INDEX_LO], c and UU_BLOCK_LO]);
end;

function Utf8UpperReference(S, D: PUtf8Char): PUtf8Char;
var
  c: Ucs4CodePoint;
  S2: PUtf8Char;
  {$ifdef CPUX86NOTPIC}
  tab: TUnicodeUpperTable absolute UU;
  {$else}
  tab: PUnicodeUpperTable;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @UU;
  {$endif CPUX86NOTPIC}
  if S <> nil then
    repeat
      c := ord(S^);
      if c <= 127 then
        if c = 0 then
          break
        else
        begin
          inc(c, tab.Block[0, c]); // branchless a..z -> A..Z
          D^ := AnsiChar(c);
          inc(S);
          inc(D);
          continue;
        end
      else if c and $20 = 0 then
      begin
        c := (c shl 6) + byte(S[1]) - $3080; // fast process $0..$7ff
        inc(S, 2);
      end
      else
      begin
        S2 := S;
        c := UTF8_TABLE.GetHighUtf8Ucs4(S2); // handle even surrogates
        S := S2;
        if c = 0 then
          c := UNICODE_REPLACEMENT_CHARACTER; // =$fffd for invalid input
      end;
      if c <= UU_MAX then
        c := tab.Ucs4Upper(c);
      inc(D, Ucs4ToUtf8(c, D));
    until false;
  D^ := #0;
  result := D;
end;

function Utf8UpperReference(S, D: PUtf8Char; SLen: PtrUInt): PUtf8Char;
var
  c: Ucs4CodePoint;
  endSBy4: PUtf8Char;
  extra, i: PtrInt;
  {$ifdef CPUX86NOTPIC}
  tab: TUnicodeUpperTable absolute UU;
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  tab: PUnicodeUpperTable;
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  by1, by4; // ugly but faster
begin
  if (S <> nil) and
     (D <> nil) then
  begin
    {$ifndef CPUX86NOTPIC}
    tab := @UU;
    utf8 := @UTF8_TABLE;
    {$endif CPUX86NOTPIC}
    // first handle trailing 7-bit ASCII chars, by quad
    inc(SLen, PtrUInt(S));
    endSBy4 := PUtf8Char(SLen) - 4;
    if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(S) and 3 = 0) and {$endif}
       (S <= endSBy4) then
      repeat
by4:    if PCardinal(S)^ and $80808080 <> 0 then
          goto by1; // break on first non ASCII quad
        i := byte(S[0]);
        inc(i, tab.Block[0, i]); // branchless a..z -> A..Z
        D[0] := AnsiChar(i);
        i := byte(S[1]);
        inc(i, tab.Block[0, i]);
        D[1] := AnsiChar(i);
        i := byte(S[2]);
        inc(i, tab.Block[0, i]);
        D[2] := AnsiChar(i);
        i := byte(S[3]);
        inc(i, tab.Block[0, i]);
        D[3] := AnsiChar(i);
        inc(S, 4);
        inc(D, 4);
      until S > endSBy4;
    // generic loop, handling one UCS4 CodePoint per iteration
    if S < PUtf8Char(SLen) then
      repeat
by1:    c := byte(S^);
        inc(S);
        if c <= 127 then
        begin
          inc(c, tab.Block[0, c]); // branchless a..z -> A..Z
          D^ := AnsiChar(c);
          inc(D);
          if {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}(PtrUInt(S) and 3 = 0) and{$endif}
             (S < endSBy4) then
            goto By4
          else if S < PUtf8Char(SLen) then
            continue
          else
            break;
        end
        else
        begin
          extra := utf8.Bytes[c];
          if (extra = 0) or
             (S + extra > PUtf8Char(SLen)) then
            break;
          i := 0;
          repeat
            c := (c shl 6) + byte(S[i]);
            inc(i)
          until i = extra;
          inc(S, extra);
          with utf8.Extra[extra] do
          begin
            dec(c, offset);
            if c < minimum then
              break; // invalid input content
          end;
          if c <= UU_MAX then
            c := tab.Ucs4Upper(c);
          inc(D, utf8.Ucs4ToUtf8(c, D));
          if S < PUtf8Char(SLen) then
            continue
          else
            break;
        end;
      until false;
    D^ := #0;
  end;
  result := D;
end;

function UpperCaseReference(const S: RawUtf8): RawUtf8;
var
  len: integer;
  tmp: TSynTempBuffer;
begin
  len := length(S);
  tmp.Init(len * 2); // some codepoints enhance in length
  tmp.Done(Utf8UpperReference(pointer(S), tmp.buf, len), result);
end;

function UpperCaseUcs4Reference(const S: RawUtf8): RawUcs4;
var
  n: PtrInt;
  c: Ucs4CodePoint;
  U: PUtf8Char;
begin
  if S = '' then
  begin
    result := nil;
    exit;
  end;
  SetLength(result, length(S) + 1);
  U := pointer(S);
  n := 0;
  repeat
    c := NextUtf8Ucs4(U);
    if c = 0 then
      break;
    if c <= UU_MAX then
      c := UU.Ucs4Upper(c);
    result[n] := c;
    inc(n);
  until false;
  if n = 0 then
    result := nil
  else
  begin
    result[n] := 0; // always end with a 0
    PDALen(PAnsiChar(result) - _DALEN)^ := n - _DAOFF; // fake length()
  end;
end;

function Utf8ICompReference(u1, u2: PUtf8Char): PtrInt;
var
  c2: PtrInt;
  {$ifdef CPUX86NOTPIC}
  tab: TUnicodeUpperTable absolute UU;
  {$else}
  tab: PUnicodeUpperTable;
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @UU;
  {$endif CPUX86NOTPIC}
  if u1 <> u2 then
    if u1 <> nil then
      if u2 <> nil then
        repeat
          result := ord(u1^);
          c2 := ord(u2^);
          if result <= 127 then
            if result <> 0 then
            begin
              inc(u1);
              inc(result, tab.Block[0, result]); // branchless a..z -> A..Z
              if c2 <= 127 then
              begin
                if c2 = 0 then
                  exit; // u1>u2 -> return u1^
                inc(u2);
                inc(c2, tab.Block[0, c2]);
                dec(result, c2);
                if result <> 0 then
                  exit;
                continue;
              end;
            end
            else
            begin
              // result=u1^=#0 -> end of u1 reached
              if c2 <> 0 then    // end of u2 reached -> u1=u2 -> return 0
                result := -1;    // u1<u2
              exit;
            end
          else
          begin
            // fast Unicode 10.0 uppercase conversion
            if result and $20 = 0 then
            begin
              result := (result shl 6) + byte(u1[1]) - $3080; // $0..$7ff
              inc(u1, 2);
            end
            else
              result := UTF8_TABLE.GetHighUtf8Ucs4(u1);
            if result <= UU_MAX then
              result := tab.Ucs4Upper(result);
          end;
          if c2 <= 127 then
          begin
            if c2 = 0 then
              exit; // u1>u2 -> return u1^
            inc(u2);
            inc(c2, tab.Block[0, c2]);
            dec(result, c2);
            if result <> 0 then
              exit;
            continue;
          end
          else
          begin
            if c2 and $20 = 0 then
            begin
              c2 := (c2 shl 6) + byte(u2[1]) - $3080; // process $0..$7ff
              inc(u2, 2);
            end
            else
              c2 := UTF8_TABLE.GetHighUtf8Ucs4(u2);
            if c2 <= UU_MAX then
              c2 := tab.Ucs4Upper(c2);
            dec(result, c2);
            if result <> 0 then
              exit;
          end;
        until false
      else
        result := 1 // u2=''
    else
      result := -1  // u1=''
  else
    result := 0;    // u1=u2
end;

function Utf8ILCompReference(u1, u2: PUtf8Char; L1, L2: integer): PtrInt;
var
  c2: PtrInt;
  extra, i: integer;
  {$ifdef CPUX86NOTPIC}
  tab: TUnicodeUpperTable absolute UU;
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  tab: PUnicodeUpperTable;
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  neg, pos;
begin
  {$ifndef CPUX86NOTPIC}
  tab := @UU;
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  if u1 <> u2 then
    if (u1 <> nil) and
       (L1 <> 0) then
      if (u2 <> nil) and
         (L2 <> 0) then
        repeat
          result := ord(u1^);
          c2 := ord(u2^);
          inc(u1);
          dec(L1);
          if result <= 127 then
          begin
            inc(result, tab.Block[0, result]); // branchless a..z -> A..Z
            if c2 <= 127 then
            begin
              inc(c2, tab.Block[0, c2]);
              dec(L2);
              inc(u2);
              dec(result, c2);
              if result <> 0 then
                // found unmatching char
                exit
              else if L1 <> 0 then
                if L2 <> 0 then
                  // L1>0 and L2>0 -> next char
                  continue
                else
                  // L1>0 and L2=0 -> u1>u2
                  goto pos
              else
              if L2 <> 0 then
                // L1=0 and L2>0 -> u1<u2
                goto neg
              else
                // L1=0 and L2=0 -> u1=u2 -> returns 0
                exit;
            end;
          end
          else
          begin
            // fast Unicode 10.0 uppercase conversion
            extra := utf8.Bytes[result];
            if extra = 0 then
              // invalid leading byte
              goto neg;
            dec(L1, extra);
            if L1 < 0 then
              goto neg;
            i := 0;
            repeat
              result := result shl 6;
              inc(result, ord(u1[i]));
              inc(i);
            until i = extra;
            inc(u1, extra);
            dec(result, utf8.Extra[extra].offset);
            if result <= UU_MAX then
              result := tab.Ucs4Upper(result);
          end;
          // here result=NormToUpper[u1^]
          inc(u2);
          dec(L2);
          if c2 <= 127 then
          begin
            inc(c2, tab.Block[0, c2]);
            dec(result, c2);
            if result <> 0 then
              // found unmatching codepoint
              exit;
          end
          else
          begin
            extra := utf8.Bytes[c2];
            if extra = 0 then
              goto pos;
            dec(L2, extra);
            if L2 < 0 then
              goto pos;
            i := 0;
            repeat
              c2 := c2 shl 6;
              inc(c2, ord(u2[i]));
              inc(i);
            until i = extra;
            inc(u2, extra);
            dec(c2, utf8.Extra[extra].offset);
            if c2 <= UU_MAX then
              c2 := tab.Ucs4Upper(c2);
            dec(result, c2);
            if result <> 0 then
              // found unmatching codepoint
              exit;
          end;
          // here we have result=0
          if L1 = 0 then
            // test if we reached end of u1 or end of u2
            if L2 = 0 then
              // u1=u2
              exit
            else
              // u1<u2
              goto neg
          else
          if L2 = 0 then
            // u1>u2
            goto pos;
        until false
      else
pos:    // u2='' or u1>u2
        result := 1
    else
neg:  // u1='' or u1<u2
      result := -1
  else
    // u1=u2
    result := 0;
end;

function StrPosIReference(U: PUtf8Char; const Up: RawUcs4): PUtf8Char;
var
  c, extra, i: PtrInt;
  u0, u2: PUtf8Char;
  up2: PInteger;
  {$ifdef CPUX86NOTPIC}
  tab: TUnicodeUpperTable absolute UU;
  utf8: TUtf8Table absolute UTF8_TABLE;
  {$else}
  tab: PUnicodeUpperTable;
  utf8: PUtf8Table;
  {$endif CPUX86NOTPIC}
label
  nxt;
begin
  result := nil;
  if (U = nil) or
     (Up = nil) then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @UU;
  utf8 := @UTF8_TABLE;
  {$endif CPUX86NOTPIC}
  repeat
    // fast search for the first character
nxt:u0 := U;
    c := byte(U^);
    inc(U);
    if c <= 127 then
    begin
      if c = 0 then
        exit; // not found -> return nil
      inc(c, tab.Block[0, c]); // branchless a..z -> A..Z
      if c <> Up[0] then
        continue;
    end
    else
    begin
      extra := utf8.Bytes[c];
      if extra = 0 then
        exit; // invalid input
      i := 0;
      repeat
        c := c shl 6;
        inc(c, ord(U[i]));
        inc(i);
      until i = extra;
      inc(U, extra);
      dec(c, utf8.Extra[extra].offset);
      if c <= UU_MAX then
        c := tab.Ucs4Upper(c);
      if c <> Up[0] then
        continue;
    end;
    // if we reached here, U^ and Up^ first UCS4 CodePoint do match
    u2 := U;
    up2 := @Up[1];
    repeat
      if up2^ = 0 then
      begin
        result := u0; // found -> return position in U
        exit;
      end;
      c := byte(u2^);
      inc(u2);
      if c <= 127 then
      begin
        if c = 0 then
          exit; // not found -> return nil
        inc(c, tab.Block[0, c]);
        if c <> up2^ then
          goto nxt;
        inc(up2);
      end
      else
      begin
        extra := utf8.Bytes[c];
        if extra = 0 then
          exit; // invalid input
        i := 0;
        repeat
          c := c shl 6;
          inc(c, ord(u2[i]));
          inc(i);
        until i = extra;
        inc(u2, extra);
        dec(c, utf8.Extra[extra].offset);
        if c <= UU_MAX then
          c := tab.Ucs4Upper(c);
        if c <> up2^ then
          goto nxt;
        inc(up2);
      end;
    until false;
  until false;
end;

procedure InitializeUnit;
var
  i: PtrInt;
  c: AnsiChar;
const
  n2u: array[138..255] of byte =
    // reference 8-bit upper chars as in WinAnsi / code page 1252
    (83, 139, 140, 141, 90, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152,
     153, 83, 155, 140, 157, 90, 89, 160, 161, 162, 163, 164, 165, 166, 167,
     168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181,
     182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 65, 65, 65, 65, 65,
     65, 198, 67, 69, 69, 69, 69, 73, 73, 73, 73, 68, 78, 79, 79, 79, 79,
     79, 215, 79, 85, 85, 85, 85, 89, 222, 223, 65, 65, 65, 65, 65, 65,
     198, 67, 69, 69, 69, 69, 73, 73, 73, 73, 68, 78, 79, 79, 79, 79,
     79, 247, 79, 85, 85, 85, 85, 89, 222, 89);
begin
  // initialize internal lookup tables for various text conversions
  for i := 0 to 255 do
    NormToNormByte[i] := i;
  NormToUpperAnsi7Byte := NormToNormByte;
  for i := ord('a') to ord('z') do
    dec(NormToUpperAnsi7Byte[i], 32);
  MoveFast(NormToUpperAnsi7, NormToUpper, 138);
  MoveFast(n2u, NormToUpperByte[138], SizeOf(n2u));
  for i := 0 to 255 do
  begin
    c := NormToUpper[AnsiChar(i)];
    if c in ['A'..'Z'] then
      inc(c, 32);
    NormToLower[AnsiChar(i)] := c;
  end;
  for c := low(c) to high(c) do
  begin
    if not (c in [#0, #10, #13]) then
      include(TEXT_CHARS[c], tcNot01013);
    if c in [#10, #13] then
      include(TEXT_CHARS[c], tc1013);
    if c in ['0'..'9', 'a'..'z', 'A'..'Z'] then
      include(TEXT_CHARS[c], tcWord);
    if c in ['_', 'a'..'z', 'A'..'Z'] then
      include(TEXT_CHARS[c], tcIdentifierFirstChar);
    if c in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] then
      include(TEXT_CHARS[c], tcIdentifier);
    if c in ['_', '-', '.', '0'..'9', 'a'..'z', 'A'..'Z'] then
      // '~' is part of the RFC 3986 but should be escaped in practice
      // see https://blog.synopse.info/?post/2020/08/11/The-RFC%2C-The-URI%2C-and-The-Tilde
      include(TEXT_CHARS[c], tcUriUnreserved);
    if c in [#1..#9, #11, #12, #14..' '] then
      include(TEXT_CHARS[c], tcCtrlNotLF);
    if c in [#1..' ', ';'] then
      include(TEXT_CHARS[c], tcCtrlNot0Comma);
  end;
  // setup basic Unicode conversion engines
  CurrentAnsiConvert := TSynAnsiConvert.Engine(Unicode_CodePage);
  WinAnsiConvert := TSynAnsiConvert.Engine(CODEPAGE_US) as TSynAnsiFixedWidth;
  Utf8AnsiConvert := TSynAnsiConvert.Engine(CP_UTF8) as TSynAnsiUtf8;
end;

procedure FinalizeUnit;
begin
  ObjArrayClear(SynAnsiConvertList);
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.
