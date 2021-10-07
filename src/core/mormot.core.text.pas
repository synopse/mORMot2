/// Framework Core Low-Level Text Processing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.text;

{
  *****************************************************************************

   Text Processing functions shared by all framework units
    - UTF-8 String Manipulation Functions
    - TRawUtf8DynArray Processing Functions
    - CSV-like Iterations over Text Buffers
    - TBaseWriter parent class for Text Generation
    - Numbers (integers or floats) and Variants to Text Conversion
    - Hexadecimal Text And Binary Conversion
    - Text Formatting functions and ESynException class
    - Resource and Time Functions

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  contnrs,
  types,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode;


{ ************ UTF-8 String Manipulation Functions }

type
  /// used to store a set of 8-bit encoded characters
  TSynAnsicharSet = set of AnsiChar;

  /// used to store a set of 8-bit unsigned integers
  TSynByteSet = set of byte;

  /// a generic callback, which can be used to translate some text on the fly
  // - maps procedure TLanguageFile.Translate(var English: string) signature
  // as defined in mORMoti18n.pas
  // - can be used e.g. for TSynMustache's {{"English text}} callback
  TOnStringTranslate = procedure(var English: string) of object;


/// extract a line from source array of chars
// - next will contain the beginning of next line, or nil if source if ended
function GetNextLine(source: PUtf8Char; out next: PUtf8Char;
  andtrim: boolean = false): RawUtf8;

/// trims leading whitespace characters from the string by removing
// new line, space, and tab characters
function TrimLeft(const S: RawUtf8): RawUtf8;

/// trims trailing whitespace characters from the string by removing trailing
// newline, space, and tab characters
function TrimRight(const S: RawUtf8): RawUtf8;

/// trims leading whitespaces of every lines of the UTF-8 text
// - also delete void lines
// - could be used e.g. before FindNameValue() call
// - modification is made in-place so S will be modified
procedure TrimLeftLines(var S: RawUtf8);

/// split a RawUtf8 string into two strings, according to SepStr separator
// - if SepStr is not found, LeftStr=Str and RightStr=''
// - if ToUpperCase is TRUE, then LeftStr and RightStr will be made uppercase
function Split(const Str, SepStr: RawUtf8; var LeftStr, RightStr: RawUtf8;
  ToUpperCase: boolean = false): boolean; overload;

/// split a RawUtf8 string into two strings, according to SepStr separator
// - this overloaded function returns the right string as function result
// - if SepStr is not found, LeftStr=Str and result=''
// - if ToUpperCase is TRUE, then LeftStr and result will be made uppercase
function Split(const Str, SepStr: RawUtf8; var LeftStr: RawUtf8;
  ToUpperCase: boolean = false): RawUtf8; overload;

/// split a RawUtf8 string into several strings, according to SepStr separator
// - this overloaded function will fill a DestPtr[] array of PRawUtf8
// - if any DestPtr[]=nil, the item will be skipped
// - if input Str end before al SepStr[] are found, DestPtr[] is set to ''
// - returns the number of values extracted into DestPtr[]
function Split(const Str: RawUtf8; const SepStr: array of RawUtf8;
  const DestPtr: array of PRawUtf8): PtrInt; overload;

/// returns the last occurence of the given SepChar separated context
// - e.g. SplitRight('01/2/34','/')='34'
// - if SepChar doesn't appear, will return Str, e.g. SplitRight('123','/')='123'
// - if LeftStr is supplied, the RawUtf8 it points to will be filled with
// the left part just before SepChar ('' if SepChar doesn't appear)
function SplitRight(const Str: RawUtf8; SepChar: AnsiChar; LeftStr: PRawUtf8 = nil): RawUtf8;

/// returns the last occurence of the given SepChar separated context
// - e.g. SplitRight('path/one\two/file.ext','/\')='file.ext', i.e.
// SepChars='/\' will be like ExtractFileName() over RawUtf8 string
// - if SepChar doesn't appear, will return Str, e.g. SplitRight('123','/')='123'
function SplitRights(const Str, SepChar: RawUtf8): RawUtf8;

/// check all character within text are spaces or control chars
// - i.e. a faster alternative to  if TrimU(text)='' then
function IsVoid(const text: RawUtf8): boolean;

/// returns the supplied text content, without any control char
// - a control char has an ASCII code #0 .. #32, i.e. text[]<=' '
// - you can specify a custom char set to be excluded, if needed
function TrimControlChars(const text: RawUtf8;
  const controls: TSynAnsicharSet = [#0 .. ' ']): RawUtf8;

/// fill all bytes of this memory buffer with zeros, i.e. 'toto' -> #0#0#0#0
// - will write the memory buffer directly, so if this string instance is shared
// (i.e. has refcount>1), all other variables will contains zeros
// - may be used to cleanup stack-allocated content
// ! ... finally FillZero(secret); end;
procedure FillZero(var secret: RawByteString); overload;

/// fill all bytes of this UTF-8 string with zeros, i.e. 'toto' -> #0#0#0#0
// - will write the memory buffer directly, so if this string instance is shared
// (i.e. has refcount>1), all other variables will contains zeros
// - may be used to cleanup stack-allocated content
// ! ... finally FillZero(secret); end;
procedure FillZero(var secret: RawUtf8); overload;

/// fill all bytes of this UTF-8 string with zeros, i.e. 'toto' -> #0#0#0#0
// - SpiUtf8 type has been defined explicitly to store Sensitive Personal
// Information
procedure FillZero(var secret: SpiUtf8); overload;

/// actual replacement function called by StringReplaceAll() on first match
// - not to be called as such, but defined globally for proper inlining
function StringReplaceAllProcess(const S, OldPattern, NewPattern: RawUtf8;
  found: integer): RawUtf8;

/// fast version of StringReplace(S, OldPattern, NewPattern,[rfReplaceAll]);
function StringReplaceAll(const S, OldPattern, NewPattern: RawUtf8): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast version of several cascaded StringReplaceAll()
function StringReplaceAll(const S: RawUtf8;
  const OldNewPatternPairs: array of RawUtf8): RawUtf8; overload;

/// fast replace of a specified char by a given string
function StringReplaceChars(const Source: RawUtf8; OldChar, NewChar: AnsiChar): RawUtf8;

/// fast replace of all #9 chars by a given string
function StringReplaceTabs(const Source, TabText: RawUtf8): RawUtf8;

/// UTF-8 dedicated (and faster) alternative to StringOfChar((Ch,Count))
function RawUtf8OfChar(Ch: AnsiChar; Count: integer): RawUtf8;

/// format a text content with SQL-like quotes
// - this function implements what is specified in the official SQLite3
// documentation: "A string constant is formed by enclosing the string in single
// quotes ('). A single quote within the string can be encoded by putting two
// single quotes in a row - as in Pascal."
function QuotedStr(const S: RawUtf8; Quote: AnsiChar = ''''): RawUtf8; overload;

/// format a text content with SQL-like quotes
procedure QuotedStr(const S: RawUtf8; Quote: AnsiChar; var result: RawUtf8); overload;

/// format a text buffer with SQL-like quotes
procedure QuotedStr(P: PUtf8Char; PLen: PtrInt; Quote: AnsiChar;
  var result: RawUtf8); overload;

/// unquote a SQL-compatible string
// - the first character in P^ must be either ' or " then internal double quotes
// are transformed into single quotes
// - 'text '' end'   -> text ' end
// - "text "" end"   -> text " end
// - returns nil if P doesn't contain a valid SQL string
// - returns a pointer just after the quoted text otherwise
function UnQuoteSqlStringVar(P: PUtf8Char; out Value: RawUtf8): PUtf8Char;

/// unquote a SQL-compatible string
function UnQuoteSqlString(const Value: RawUtf8): RawUtf8;

/// unquote a SQL-compatible symbol name
// - e.g. '[symbol]' -> 'symbol' or '"symbol"' -> 'symbol'
function UnQuotedSQLSymbolName(const ExternalDBSymbol: RawUtf8): RawUtf8;


/// get the next character after a quoted buffer
// - the first character in P^ must be either ', either "
// - it will return the latest quote position, ignoring double quotes within
function GotoEndOfQuotedString(P: PUtf8Char): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// get the next character not in [#1..' ']
function GotoNextNotSpace(P: PUtf8Char): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// get the next character not in [#9,' ']
function GotoNextNotSpaceSameLine(P: PUtf8Char): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// get the next character in [#0..' ']
function GotoNextSpace(P: PUtf8Char): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// check if the next character not in [#1..' '] matchs a given value
// - first ignore any non space character
// - then returns TRUE if P^=ch, setting P to the character after ch
// - or returns FALSE if P^<>ch, leaving P at the level of the unexpected char
function NextNotSpaceCharIs(var P: PUtf8Char; ch: AnsiChar): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the next SQL-like identifier within the UTF-8 buffer
// - will also trim any space (or line feeds) and trailing ';'
// - any comment like '/*nocache*/' will be ignored
// - returns true if something was set to Prop
function GetNextFieldProp(var P: PUtf8Char; var Prop: RawUtf8): boolean;

/// retrieve the next identifier within the UTF-8 buffer on the same line
// - GetNextFieldProp() will just handle line feeds (and ';') as spaces - which
// is fine e.g. for SQL, but not for regular config files with name/value pairs
// - returns true if something was set to Prop
function GetNextFieldPropSameLine(var P: PUtf8Char; var Prop: ShortString): boolean;

/// return true if IdemPChar(source,searchUp), and go to the next line of source
function IdemPCharAndGetNextLine(var source: PUtf8Char; searchUp: PAnsiChar): boolean;

/// search for a value from its uppercased named entry
// - i.e. iterate IdemPChar(source,UpperName) over every line of the source
// - returns the text just after UpperName if it has been found at line beginning
// - returns nil if UpperName was not found at any line beginning
// - could be used as alternative to FindIniNameValue() and FindIniNameValueInteger()
// if there is no section, i.e. if search should not stop at '[' but at source end
function FindNameValue(P: PUtf8Char; UpperName: PAnsiChar): PUtf8Char; overload;

/// search and returns a value from its uppercased named entry
// - i.e. iterate IdemPChar(source,UpperName) over every line of the source
// - returns true and the trimmed text just after UpperName into Value
// if it has been found at line beginning
// - returns false and set Value := '' if UpperName was not found (or leave
// Value untouched if KeepNotFoundValue is true)
// - could be used e.g. to efficently extract a value from HTTP headers, whereas
// FindIniNameValue() is tuned for [section]-oriented INI files
// - do TrimLeftLines(NameValuePairs) first if the lines start with spaces/tabs
function FindNameValue(const NameValuePairs: RawUtf8; UpperName: PAnsiChar;
  var Value: RawUtf8; KeepNotFoundValue: boolean = false;
  UpperNameSeparator: AnsiChar = #0): boolean; overload;

/// compute the line length from source array of chars
// - if PEnd = nil, end counting at either #0, #13 or #10
// - otherwise, end counting at either #13 or #10
// - just a wrapper around BufferLineLength() checking PEnd=nil case
function GetLineSize(P, PEnd: PUtf8Char): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// returns true if the line length from source array of chars is not less than
// the specified count
function GetLineSizeSmallerThan(P, PEnd: PUtf8Char; aMinimalCount: integer): boolean;

/// return next string delimited with #13#10 from P, nil if no more
// - this function returns a RawUnicode string type
function GetNextStringLineToRawUnicode(var P: PChar): RawUnicode;

/// trim first lowercase chars ('otDone' will return 'Done' e.g.)
// - return a PUtf8Char to avoid any memory allocation
function TrimLeftLowerCase(const V: RawUtf8): PUtf8Char;

/// trim first lowercase chars ('otDone' will return 'Done' e.g.)
// - return an RawUtf8 string: enumeration names are pure 7bit ANSI with Delphi 7
// to 2007, and UTF-8 encoded with Delphi 2009+
function TrimLeftLowerCaseShort(V: PShortString): RawUtf8;

/// trim first lowercase chars ('otDone' will return 'Done' e.g.)
// - return a shortstring: enumeration names are pure 7bit ANSI with Delphi 7
// to 2007, and UTF-8 encoded with Delphi 2009+
function TrimLeftLowerCaseToShort(V: PShortString): ShortString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// trim first lowercase chars ('otDone' will return 'Done' e.g.)
// - return a shortstring: enumeration names are pure 7bit ANSI with Delphi 7
// to 2007, and UTF-8 encoded with Delphi 2009+
procedure TrimLeftLowerCaseToShort(V: PShortString; out result: ShortString); overload;

/// fast append some UTF-8 text into a shortstring, with an ending ','
procedure AppendShortComma(text: PAnsiChar; len: PtrInt; var result: shortstring;
  trimlowercase: boolean);   {$ifdef FPC} inline; {$endif}

/// fast search of an exact case-insensitive match of a RTTI's PShortString array
function FindShortStringListExact(List: PShortString; MaxValue: integer;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;

/// fast case-insensitive search of a left-trimmed lowercase match
// of a RTTI's PShortString array
function FindShortStringListTrimLowerCase(List: PShortString; MaxValue: integer;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;

/// fast case-sensitive search of a left-trimmed lowercase match
// of a RTTI's PShortString array
function FindShortStringListTrimLowerCaseExact(List: PShortString; MaxValue: integer;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;

/// convert a CamelCase string into a space separated one
// - 'OnLine' will return 'On line' e.g., and 'OnMyLINE' will return 'On my LINE'
// - will handle capital words at the beginning, middle or end of the text, e.g.
// 'KLMFlightNumber' will return 'KLM flight number' and 'GoodBBCProgram' will
// return 'Good BBC program'
// - will handle a number at the beginning, middle or end of the text, e.g.
// 'Email12' will return 'Email 12'
// - '_' char is transformed into ' - '
// - '__' chars are transformed into ': '
// - return an RawUtf8 string: enumeration names are pure 7bit ANSI with Delphi 7
// to 2007, and UTF-8 encoded with Delphi 2009+
function UnCamelCase(const S: RawUtf8): RawUtf8; overload;

/// convert a CamelCase string into a space separated one
// - 'OnLine' will return 'On line' e.g., and 'OnMyLINE' will return 'On my LINE'
// - will handle capital words at the beginning, middle or end of the text, e.g.
// 'KLMFlightNumber' will return 'KLM flight number' and 'GoodBBCProgram' will
// return 'Good BBC program'
// - will handle a number at the beginning, middle or end of the text, e.g.
// 'Email12' will return 'Email 12'
// - return the char count written into D^
// - D^ and P^ are expected to be UTF-8 encoded: enumeration and property names
// are pure 7bit ANSI with Delphi 7 to 2007, and UTF-8 encoded with Delphi 2009+
// - '_' char is transformed into ' - '
// - '__' chars are transformed into ': '
function UnCamelCase(D, P: PUtf8Char): integer; overload;

/// convert a string into an human-friendly CamelCase identifier
// - replacing spaces or punctuations by an uppercase character
// - as such, it is not the reverse function to UnCamelCase()
procedure CamelCase(P: PAnsiChar; len: PtrInt; var s: RawUtf8;
  const isWord: TSynByteSet = [ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z')]); overload;

/// convert a string into an human-friendly CamelCase identifier
// - replacing spaces or punctuations by an uppercase character
// - as such, it is not the reverse function to UnCamelCase()
procedure CamelCase(const text: RawUtf8; var s: RawUtf8;
  const isWord: TSynByteSet = [ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z')]); overload;
  {$ifdef HASINLINE}inline;{$endif}

var
  /// these procedure type must be defined if a default system.pas is used
  // - expect generic "string" type, i.e. UnicodeString for Delphi 2009+
  LoadResStringTranslate: procedure(var Text: string) = nil;

/// UnCamelCase and translate a char buffer
// - P is expected to be #0 ended
// - return "string" type, i.e. UnicodeString for Delphi 2009+
procedure GetCaptionFromPCharLen(P: PUtf8Char; out result: string);



{ ************ CSV-like Iterations over Text Buffers }

/// return true if IdemPChar(source,searchUp) matches, and retrieve the value item
// - typical use may be:
// ! if IdemPCharAndGetNextItem(P,
// !   'CONTENT-DISPOSITION: FORM-DATA; NAME="',Name,'"') then ...
function IdemPCharAndGetNextItem(var source: PUtf8Char; const searchUp: RawUtf8;
  var Item: RawUtf8; Sep: AnsiChar = #13): boolean;

/// return next CSV string from P
// - P=nil after call when end of text is reached
function GetNextItem(var P: PUtf8Char; Sep: AnsiChar = ','): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return next CSV string from P
// - P=nil after call when end of text is reached
procedure GetNextItem(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8); overload;

/// return next CSV string (unquoted if needed) from P
// - P=nil after call when end of text is reached
procedure GetNextItem(var P: PUtf8Char; Sep, Quote: AnsiChar;
  var result: RawUtf8); overload;

/// return trimmed next CSV string from P
// - P=nil after call when end of text is reached
procedure GetNextItemTrimed(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8);

/// return next CRLF separated value string from P, ending #10 or #13#10 trimmed
// - any kind of line feed (CRLF or LF) will be handled, on all operating systems
// - as used e.g. by TSynNameValue.InitFromCsv and TDocVariantData.InitCsv
// - P=nil after call when end of text is reached
procedure GetNextItemTrimedCRLF(var P: PUtf8Char; var result: RawUtf8);

/// return next CSV string from P, nil if no more
// - this function returns the generic string type of the compiler, and
// therefore can be used with ready to be displayed text (e.g. for the VCL)
function GetNextItemString(var P: PChar; Sep: Char = ','): string;

/// extract a file extension from a file name, then compare with a comma
// separated list of extensions
// - e.g. GetFileNameExtIndex('test.log','exe,log,map')=1
// - will return -1 if no file extension match
// - will return any matching extension, starting count at 0
// - extension match is case-insensitive
function GetFileNameExtIndex(const FileName, CsvExt: TFileName): integer;

/// return next CSV string from P, nil if no more
// - output text would be trimmed from any left or right space
// - will always append a trailing #0 - excluded from Dest length (0..254)
procedure GetNextItemShortString(var P: PUtf8Char;
  out Dest: ShortString; Sep: AnsiChar = ',');

/// append some text lines with the supplied Values[]
// - if any Values[] item is '', no line is added
// - otherwise, appends 'Caption: Value', with Caption taken from CSV
procedure AppendCsvValues(const Csv: string; const Values: array of string;
  var result: string; const AppendBefore: string = #13#10);

/// return a CSV list of the iterated same value
// - e.g. CsvOfValue('?',3)='?,?,?'
function CsvOfValue(const Value: RawUtf8; Count: cardinal; const Sep: RawUtf8 = ','): RawUtf8;

 /// retrieve the next CSV separated bit index
// - each bit was stored as BitIndex+1, i.e. 0 to mark end of CSV chunk
// - several bits set to one can be regrouped via 'first-last,' syntax
procedure SetBitCsv(var Bits; BitsCount: integer; var P: PUtf8Char);

/// convert a set of bit into a CSV content
// - each bit is stored as BitIndex+1, and separated by a ','
// - several bits set to one can be regrouped via 'first-last,' syntax
// - ',0' is always appended at the end of the CSV chunk to mark its end
function GetBitCsv(const Bits; BitsCount: integer): RawUtf8;

/// decode next CSV hexadecimal string from P, nil if no more or not matching BinBytes
// - Bin is filled with 0 if the supplied CSV content is invalid
// - if Sep is #0, it will read the hexadecimal chars until a whitespace is reached
function GetNextItemHexDisplayToBin(var P: PUtf8Char; Bin: PByte; BinBytes: PtrInt;
  Sep: AnsiChar = ','): boolean;

type
  /// some stack-allocated zero-terminated character buffer
  // - as used by GetNextTChar64
  TChar64 = array[0..63] of AnsiChar;

/// return next CSV string from P as a #0-ended buffer, false if no more
// - if Sep is #0, will copy all characters until next whitespace char
// - returns the number of bytes stored into Buf[]
function GetNextTChar64(var P: PUtf8Char; Sep: AnsiChar; out Buf: TChar64): PtrInt;

/// return next CSV string as unsigned integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemCardinal(var P: PUtf8Char; Sep: AnsiChar = ','): PtrUInt;

/// return next CSV string as signed integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemInteger(var P: PUtf8Char; Sep: AnsiChar = ','): PtrInt;

/// return next CSV string as 64-bit signed integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemInt64(var P: PUtf8Char; Sep: AnsiChar = ','): Int64;

/// return next CSV string as 64-bit unsigned integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemQWord(var P: PUtf8Char; Sep: AnsiChar = ','): QWord;

/// return next CSV hexadecimal string as 64-bit unsigned integer from P
// - returns 0 if no valid hexadecimal text is available in P
// - if Sep is #0, it won't be searched for
// - will first fill the 64-bit value with 0, then decode each two hexadecimal
// characters available in P
// - could be used to decode TBaseWriter.AddBinToHexDisplayMinChars() output
function GetNextItemHexa(var P: PUtf8Char; Sep: AnsiChar = ','): QWord;

/// return next CSV string as unsigned integer from P, 0 if no more
// - P^ will point to the first non digit character (the item separator, e.g.
// ',' for CSV)
function GetNextItemCardinalStrict(var P: PUtf8Char): PtrUInt;

/// return next CSV string as unsigned integer from P, 0 if no more
// - this version expects P^ to point to an Unicode char array
function GetNextItemCardinalW(var P: PWideChar; Sep: WideChar = ','): PtrUInt;

/// return next CSV string as double from P, 0.0 if no more
// - if Sep is #0, will return all characters until next whitespace char
function GetNextItemDouble(var P: PUtf8Char; Sep: AnsiChar = ','): double;

/// return next CSV string as currency from P, 0.0 if no more
// - if Sep is #0, will return all characters until next whitespace char
function GetNextItemCurrency(var P: PUtf8Char; Sep: AnsiChar = ','): currency; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return next CSV string as currency from P, 0.0 if no more
// - if Sep is #0, will return all characters until next whitespace char
procedure GetNextItemCurrency(var P: PUtf8Char; out result: currency;
  Sep: AnsiChar = ','); overload;

/// return n-th indexed CSV string in P, starting at Index=0 for first one
function GetCsvItem(P: PUtf8Char; Index: PtrUInt; Sep: AnsiChar = ','): RawUtf8; overload;

/// return n-th indexed CSV string (unquoted if needed) in P, starting at Index=0 for first one
function GetUnQuoteCsvItem(P: PUtf8Char; Index: PtrUInt; Sep: AnsiChar = ',';
  Quote: AnsiChar = ''''): RawUtf8; overload;

/// return n-th indexed CSV string in P, starting at Index=0 for first one
// - this function return the generic string type of the compiler, and
// therefore can be used with ready to be displayed text (i.e. the VCL)
function GetCsvItemString(P: PChar; Index: PtrUInt; Sep: Char = ','): string;

/// return last CSV string in the supplied UTF-8 content
function GetLastCsvItem(const Csv: RawUtf8; Sep: AnsiChar = ','): RawUtf8;

/// return the index of a Value in a CSV string
// - start at Index=0 for first one
// - return -1 if specified Value was not found in CSV items
function FindCsvIndex(Csv: PUtf8Char; const Value: RawUtf8; Sep: AnsiChar = ',';
  CaseSensitive: boolean = true; TrimValue: boolean = false): integer;

/// add the strings in the specified CSV text into a dynamic array of UTF-8 strings
procedure CsvToRawUtf8DynArray(Csv: PUtf8Char; var List: TRawUtf8DynArray;
  Sep: AnsiChar = ','; TrimItems: boolean = false; AddVoidItems: boolean = false); overload;

/// add the strings in the specified CSV text into a dynamic array of UTF-8 strings
procedure CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8;
  var List: TRawUtf8DynArray); overload;

/// return the corresponding CSV text from a dynamic array of UTF-8 strings
function RawUtf8ArrayToCsv(const Values: array of RawUtf8;
  const Sep: RawUtf8 = ','; HighValues: integer = -1): RawUtf8;

/// return the corresponding CSV quoted text from a dynamic array of UTF-8 strings
// - apply QuoteStr() function to each Values[] item
function RawUtf8ArrayToQuotedCsv(const Values: array of RawUtf8;
  const Sep: RawUtf8 = ','; Quote: AnsiChar = ''''): RawUtf8;

/// append some prefix to all CSV values
// ! AddPrefixToCsv('One,Two,Three','Pre')='PreOne,PreTwo,PreThree'
function AddPrefixToCsv(Csv: PUtf8Char; const Prefix: RawUtf8;
  Sep: AnsiChar = ','): RawUtf8;

/// append a Value to a CSV string
procedure AddToCsv(const Value: RawUtf8; var Csv: RawUtf8; const Sep: RawUtf8 = ',');
  {$ifdef HASINLINE}inline;{$endif}

/// change a Value within a CSV string
function RenameInCsv(const OldValue, NewValue: RawUtf8; var Csv: RawUtf8;
  const Sep: RawUtf8 = ','): boolean;

/// append the strings in the specified CSV text into a dynamic array of integer
procedure CsvToIntegerDynArray(Csv: PUtf8Char; var List: TIntegerDynArray;
  Sep: AnsiChar = ',');

/// append the strings in the specified CSV text into a dynamic array of integer
procedure CsvToInt64DynArray(Csv: PUtf8Char; var List: TInt64DynArray;
  Sep: AnsiChar = ','); overload;

/// convert the strings in the specified CSV text into a dynamic array of integer
function CsvToInt64DynArray(Csv: PUtf8Char; Sep: AnsiChar = ','): TInt64DynArray; overload;

/// return the corresponding CSV text from a dynamic array of 32-bit integer
// - you can set some custom Prefix and Suffix text
function IntegerDynArrayToCsv(Values: PIntegerArray; ValuesCount: integer;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false): RawUtf8; overload;

/// return the corresponding CSV text from a dynamic array of 32-bit integer
// - you can set some custom Prefix and Suffix text
function IntegerDynArrayToCsv(const Values: TIntegerDynArray;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return the corresponding CSV text from a dynamic array of 64-bit integers
// - you can set some custom Prefix and Suffix text
function Int64DynArrayToCsv(Values: PInt64Array; ValuesCount: integer;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false): RawUtf8; overload;

/// return the corresponding CSV text from a dynamic array of 64-bit integers
// - you can set some custom Prefix and Suffix text
function Int64DynArrayToCsv(const Values: TInt64DynArray;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}


{ ************ TBaseWriter parent class for Text Generation }

type
  /// event signature for TBaseWriter.OnFlushToStream callback
  TOnTextWriterFlush = procedure(Text: PUtf8Char; Len: PtrInt) of object;

  /// defines how text is to be added into TBaseWriter / TTextWriter
  // - twNone will write the supplied text with no escaping
  // - twJsonEscape will properly escape " and \ as expected by JSON
  // - twOnSameLine will convert any line feeds or control chars into spaces
  TTextWriterKind = (
    twNone,
    twJsonEscape,
    twOnSameLine);

  /// available global options for a TBaseWriter / TBaseWriter instance
  // - TBaseWriter.WriteObject() method behavior would be set via their own
  // TTextWriterWriteObjectOptions, and work in conjunction with those settings
  // - twoStreamIsOwned would be set if the associated TStream is owned by
  // the TBaseWriter instance
  // - twoFlushToStreamNoAutoResize would forbid FlushToStream to resize the
  // internal memory buffer when it appears undersized - FlushFinal will set it
  // before calling a last FlushToStream
  // - by default, custom serializers defined via RegisterCustomJsonSerializer()
  // would let AddRecordJson() and AddDynArrayJson() write enumerates and sets
  // as integer numbers, unless twoEnumSetsAsTextInRecord or
  // twoEnumSetsAsBooleanInRecord (exclusively) are set - for Mustache data
  // context, twoEnumSetsAsBooleanInRecord will return a JSON object with
  // "setname":true/false fields
  // - variants and nested objects would be serialized with their default
  // JSON serialization options, unless twoForceJsonExtended or
  // twoForceJsonStandard is defined
  // - when enumerates and sets are serialized as text into JSON, you may force
  // the identifiers to be left-trimed for all their lowercase characters
  // (e.g. sllError -> 'Error') by setting twoTrimLeftEnumSets: this option
  // would default to the global TBaseWriter.SetDefaultEnumTrim setting
  // - twoEndOfLineCRLF would reflect the TEchoWriter.EndOfLineCRLF property
  // - twoBufferIsExternal would be set if the temporary buffer is not handled
  // by the instance, but specified at constructor, maybe from the stack
  // - twoIgnoreDefaultInRecord will force custom record serialization to avoid
  // writing the fields with default values, i.e. enable soWriteIgnoreDefault
  // when published properties are serialized
  TTextWriterOption = (
    twoStreamIsOwned,
    twoFlushToStreamNoAutoResize,
    twoEnumSetsAsTextInRecord,
    twoEnumSetsAsBooleanInRecord,
    twoFullSetsAsStar,
    twoTrimLeftEnumSets,
    twoForceJsonExtended,
    twoForceJsonStandard,
    twoEndOfLineCRLF,
    twoBufferIsExternal,
    twoIgnoreDefaultInRecord);
    
  /// options set for a TBaseWriter / TBaseWriter instance
  // - allows to override e.g. AddRecordJson() and AddDynArrayJson() behavior;
  // or set global process customization for a TBaseWriter
  TTextWriterOptions = set of TTextWriterOption;

  /// may be used to allocate on stack a 8KB work buffer for a TBaseWriter
  // - via the TBaseWriter.CreateOwnedStream overloaded constructor
  TTextWriterStackBuffer = array[0..8191] of AnsiChar;
  PTextWriterStackBuffer = ^TTextWriterStackBuffer;

  /// available options for TBaseWriter.WriteObject() method
  // - woHumanReadable will add some line feeds and indentation to the content,
  // to make it more friendly to the human eye
  // - woDontStoreDefault (which is set by default for WriteObject method) will
  // avoid serializing properties including a default value (JsonToObject function
  // will set the default values, so it may help saving some bandwidth or storage)
  // - woFullExpand will generate a debugger-friendly layout, including instance
  // class name, sets/enumerates as text, and reference pointer - as used by
  // TSynLog and ObjectToJsonFull()
  // - woStoreClassName will add a "ClassName":"TMyClass" field
  // - woStorePointer will add a "Address":"0431298A" field, and .map/.dbg/.mab
  // source code line number corresponding to ESynException.RaisedAt
  // - woStoreStoredFalse will write the 'stored false' properties, even
  // if they are marked as such (used e.g. to persist all settings on file,
  // but disallow the sensitive - password - fields be logged)
  // - woHumanReadableFullSetsAsStar will store an human-readable set with
  // all its enumerates items set to be stored as ["*"]
  // - woHumanReadableEnumSetAsComment will add a comment at the end of the
  // line, containing all available values of the enumaration or set, e.g:
  // $ "Enum": "Destroying", // Idle,Started,Finished,Destroying
  // - woEnumSetsAsText will store sets and enumerables as text (is also
  // included in woFullExpand or woHumanReadable)
  // - woDateTimeWithMagic will append the JSON_SQLDATE_MAGIC_C (i.e. U+FFF1)
  // before the ISO-8601 encoded TDateTime value
  // - woDateTimeWithZSuffix will append the Z suffix to the ISO-8601 encoded
  // TDateTime value, to identify the content as strict UTC value
  // - TTimeLog would be serialized as Int64, unless woTimeLogAsText is defined
  // - since TOrm.ID could be huge Int64 numbers, they may be truncated
  // on client side, e.g. to 53-bit range in JavaScript: you could define
  // woIDAsIDstr to append an additional "ID_str":"##########" field
  // - by default, RawBlob properties are serialized as null, unless
  // woRawBlobAsBase64 is defined or a custom serialization is used (e.g. TOrm)
  // - if woHideSensitivePersonalInformation is set, rcfSpi types (e.g. the
  // TSynPersistentWithPassword.Password field) will be serialized as "***"
  // to prevent security issues (e.g. in log)
  // - by default, TObjectList will set the woStoreClassName for its nested
  // objects, unless woObjectListWontStoreClassName is defined
  // - all inherited properties would be serialized, unless woDontStoreInherited
  // is defined, and only the topmost class level properties would be serialized
  // - woInt64AsHex will force Int64/QWord to be written as hexadecimal string -
  // see j2oAllowInt64Hex reverse option fot Json2Object
  // - woDontStoreVoid will avoid serializing numeric properties equal to 0 and
  // string properties equal to '' (replace both deprecated woDontStore0 and
  // woDontStoreEmptyString flags)
  // - woPersistentLock paranoid setting will call TSynPersistentLock.Lock/Unlock
  // during serialization
  TTextWriterWriteObjectOption = (
    woHumanReadable,
    woDontStoreDefault,
    woFullExpand,
    woStoreClassName,
    woStorePointer,
    woStoreStoredFalse,
    woHumanReadableFullSetsAsStar,
    woHumanReadableEnumSetAsComment,
    woEnumSetsAsText,
    woDateTimeWithMagic,
    woDateTimeWithZSuffix,
    woTimeLogAsText,
    woIDAsIDstr,
    woRawBlobAsBase64,
    woHideSensitivePersonalInformation,
    woObjectListWontStoreClassName,
    woDontStoreInherited,
    woInt64AsHex,
    woDontStoreVoid,
    woPersistentLock);

  /// options set for TBaseWriter.WriteObject() method
  TTextWriterWriteObjectOptions = set of TTextWriterWriteObjectOption;

  /// the potential places were TTextWriter.AddHtmlEscape should process
  // proper HTML string escaping, unless hfNone is used
  // $  < > & "  ->   &lt; &gt; &amp; &quote;
  // by default (hfAnyWhere)
  // $  < > &  ->   &lt; &gt; &amp;
  // outside HTML attributes (hfOutsideAttributes)
  // $  & "  ->   &amp; &quote;
  // within HTML attributes (hfWithinAttributes)
  TTextWriterHtmlFormat = (
    hfNone,
    hfAnyWhere,
    hfOutsideAttributes,
    hfWithinAttributes);

  /// the available JSON format, for TBaseWriter.AddJsonReformat() and its
  // JsonBufferReformat() and JsonReformat() wrappers
  // - jsonCompact is the default machine-friendly single-line layout
  // - jsonHumanReadable will add line feeds and indentation, for a more
  // human-friendly result
  // - jsonUnquotedPropName will emit the jsonHumanReadable layout, but
  // with all property names being quoted only if necessary: this format
  // could be used e.g. for configuration files - this format, similar to the
  // one used in the MongoDB extended syntax, is not JSON compatible: do not
  // use it e.g. with AJAX clients, but is would be handled as expected by all
  // our units as valid JSON input, without previous correction
  // - jsonUnquotedPropNameCompact will emit single-line layout with unquoted
  // property names
  // - by default we rely on UTF-8 encoding (which is mandatory in the RFC 8259)
  // but you can use jsonEscapeUnicode to produce compact 7-bit ASCII output,
  // with \u#### escape of all accents, e.g. as default python json.dumps
  // - jsonNoEscapeUnicode will process any \u#### pattern and ensure proper
  // UTF-8 is generated instead
  // - those features are not implemented in this unit, but in mormot.core.json
  TTextWriterJsonFormat = (
    jsonCompact,
    jsonHumanReadable,
    jsonUnquotedPropName,
    jsonUnquotedPropNameCompact,
    jsonEscapeUnicode,
    jsonNoEscapeUnicode);
    
  /// parent to T*Writer text processing classes, with the minimum set of methods
  // - use an internal buffer, so much faster than naive string+string
  // - see TTextWriter in mormot.core.json for proper JSON support
  // - see TJsonWriter in mormot.db.core for SQL resultset export
  // - see TJsonSerializer in mormot.orm.core for ORM oriented serialization
  TBaseWriter = class
  protected
    fStream: TStream;
    fInitialStreamPosition: PtrUInt;
    fTotalFileSize: PtrUInt;
    fHumanReadableLevel: integer;
    // internal temporary buffer
    fTempBufSize: integer;
    fTempBuf: PUtf8Char;
    fOnFlushToStream: TOnTextWriterFlush;
    fCustomOptions: TTextWriterOptions;
    procedure WriteToStream(data: pointer; len: PtrUInt); virtual;
    function GetTextLength: PtrUInt;
    procedure SetStream(aStream: TStream);
    procedure SetBuffer(aBuf: pointer; aBufSize: integer);
  public
    /// direct access to the low-level current position in the buffer
    // - you should not use this field directly
    B: PUtf8Char;
    /// direct access to the low-level last position in the buffer
    // - you should not use this field directly
    // - points in fact to 16 bytes before the buffer ending
    BEnd: PUtf8Char;
    /// the data will be written to the specified Stream
    // - aStream may be nil: in this case, it MUST be set before using any
    // Add*() method
    // - default internal buffer size if 8192
    constructor Create(aStream: TStream; aBufSize: integer = 8192); overload;
    /// the data will be written to the specified Stream
    // - aStream may be nil: in this case, it MUST be set before using any
    // Add*() method
    // - will use an external buffer (which may be allocated on stack)
    constructor Create(aStream: TStream; aBuf: pointer; aBufSize: integer); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - TRawByteStringStream.DataString method will be used by TBaseWriter.Text
    // to retrieve directly the content without any data move nor allocation
    // - default internal buffer size if 4096 (enough for most JSON objects)
    // - consider using a stack-allocated buffer and the overloaded method
    constructor CreateOwnedStream(aBufSize: integer = 4096); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - will use an external buffer (which may be allocated on stack)
    // - TRawByteStringStream.DataString method will be used by TBaseWriter.Text
    // to retrieve directly the content without any data move nor allocation
    constructor CreateOwnedStream(aBuf: pointer; aBufSize: integer); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - will use the stack-allocated TTextWriterStackBuffer if possible
    // - TRawByteStringStream.DataString method will be used by TBaseWriter.Text
    // to retrieve directly the content without any data move nor allocation
    constructor CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
      aBufSize: integer = SizeOf(TTextWriterStackBuffer)); overload;
    /// the data will be written to an external file
    // - you should call explicitly FlushFinal or FlushToStream to write
    // any pending data to the file
    constructor CreateOwnedFileStream(const aFileName: TFileName;
      aBufSize: integer = 8192);
    /// release all internal structures
    // - e.g. free fStream if the instance was owned by this class
    destructor Destroy; override;
    /// allow to override the default (JSON) serialization of enumerations and
    // sets as text, which would write the whole identifier (e.g. 'sllError')
    // - calling SetDefaultEnumTrim(true) would force the enumerations to
    // be trimmed for any lower case char, e.g. sllError -> 'Error'
    // - this is global to the current process, and should be use mainly for
    // compatibility purposes for the whole process
    // - you may change the default behavior by setting twoTrimLeftEnumSets
    // in the TBaseWriter.CustomOptions property of a given serializer
    // - note that unserialization process would recognize both formats
    class procedure SetDefaultEnumTrim(aShouldTrimEnumsAsText: boolean);

    /// write pending data, then retrieve the whole text as a UTF-8 string
    function Text: RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// write pending data, then retrieve the whole text as a UTF-8 string
    procedure SetText(out result: RawUtf8; reformat: TTextWriterJsonFormat = jsonCompact);
    /// set the internal stream content with the supplied UTF-8 text
    procedure ForceContent(const text: RawUtf8);
    /// write pending data to the Stream, with automatic buffer resizal
    // - you should not have to call FlushToStream in most cases, but FlushFinal
    // at the end of the process, just before using the resulting Stream
    // - FlushToStream may be used to force immediate writing of the internal
    // memory buffer to the destination Stream
    // - you can set FlushToStreamNoAutoResize=true or call FlushFinal if you
    // do not want the automatic memory buffer resizal to take place
    procedure FlushToStream; virtual;
    /// write pending data to the Stream, without automatic buffer resizal
    // - will append the internal memory buffer to the Stream
    // - in short, FlushToStream may be called during the adding process, and
    // FlushFinal at the end of the process, just before using the resulting Stream
    // - if you don't call FlushToStream or FlushFinal, some pending characters
    // may not be copied to the Stream: you should call it before using the Stream
    procedure FlushFinal;

    /// append one ASCII char to the buffer
    procedure Add(c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one comma (',') character
    procedure AddComma;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one ASCII char to the buffer, if not already there as LastChar
    procedure AddOnce(c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append two chars to the buffer
    procedure Add(c1, c2: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    {$ifdef CPU32} // already implemented by Add(Value: PtrInt) method on CPU64
    /// append a 64-bit signed integer Value as text
    procedure Add(Value: Int64); overload;
    {$endif CPU32}
    /// append a 32-bit signed integer Value as text
    procedure Add(Value: PtrInt); overload;
    /// append a boolean Value as text
    // - write either 'true' or 'false'
    procedure Add(Value: boolean); overload;
    /// append a Currency from its Int64 in-memory representation
    // - expects a PInt64 to avoid ambiguity with the AddCurr() method
    procedure AddCurr64(Value: PInt64);
    /// append a Currency value
    // - just an inlined wrapper around AddCurr64(PInt64(@Value))
    procedure AddCurr(const Value: currency); 
      {$ifdef HASINLINE}inline;{$endif}
    /// append an Unsigned 32-bit integer Value as a String
    procedure AddU(Value: cardinal);
    /// append an Unsigned 64-bit integer Value as a String
    procedure AddQ(Value: QWord);
    /// append an Unsigned 64-bit integer Value as a quoted hexadecimal String
    procedure AddQHex(Value: Qword);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a GUID value, encoded as text without any {}
    // - will store e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
    procedure Add(Value: PGUID; QuotedChar: AnsiChar = #0); overload;
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure AddDouble(Value: double; noexp: boolean = false);
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure AddSingle(Value: single; noexp: boolean = false);
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure Add(Value: Extended; precision: integer; noexp: boolean = false); overload;
    /// append a floating-point text buffer
    // - will correct on the fly '.5' -> '0.5' and '-.5' -> '-0.5'
    // - is used when the input comes from a third-party source with no regular
    // output, e.g. a database driver
    procedure AddFloatStr(P: PUtf8Char);
    /// append CR+LF (#13#10) chars
    // - this method won't call TEchoWriter.EchoAdd() registered events - use
    // TEchoWriter.AddEndOfLine() method instead
    // - TEchoWriter.AddEndOfLine() will append either CR+LF (#13#10) or
    // only LF (#10) depending on its internal options
    procedure AddCR;
      {$ifdef HASINLINE}inline;{$endif}
    /// append CR+LF (#13#10) chars and #9 indentation
    // - indentation depth is defined by the HumanReadableLevel value
    procedure AddCRAndIndent; virtual;
    /// write the same character multiple times
    procedure AddChars(aChar: AnsiChar; aCount: PtrInt);
    /// append an integer Value as a 2 digits text with comma
    procedure Add2(Value: PtrUInt);
    /// append an integer Value as a 3 digits text without any comma
    procedure Add3(Value: cardinal);
    /// append an integer Value as a 4 digits text with comma
    procedure Add4(Value: PtrUInt);
    /// append a time period, specified in micro seconds, in 00.000.000 TSynLog format
    procedure AddMicroSec(MS: cardinal);
    /// append some UTF-8 chars to the buffer
    // - input length is calculated from zero-ended char
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscape(P: Pointer); overload;
    /// append some UTF-8 chars to the buffer
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscape(P: Pointer; Len: PtrInt); overload;
    /// append some UTF-8 chars to the buffer
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscapeUtf8(const text: RawByteString);
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 encoded chars to the buffer, from a generic string type
    // - don't escapes chars according to the JSON RFC
    // - if s is a UnicodeString, will convert UTF-16 into UTF-8
    procedure AddNoJsonEscapeString(const s: string);
    /// append some unicode chars to the buffer
    // - WideCharCount is the unicode chars count, not the byte size
    // - don't escapes chars according to the JSON RFC
    // - will convert the Unicode chars into UTF-8
    procedure AddNoJsonEscapeW(WideChar: PWord; WideCharCount: integer);
    /// append some Ansi text as UTF-8 chars to the buffer
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscape(P: PAnsiChar; Len: PtrInt; CodePage: cardinal); overload;
    /// append some UTF-8 content to the buffer, with no JSON escape
    // - if supplied json is '', will write 'null'
    // - redirect to AddNoJsonEscape() otherwise
    procedure AddRawJson(const json: RawJson);
    /// append a line of text with CR+LF at the end
    procedure AddLine(const Text: shortstring);
    /// append some chars to the buffer in one line
    // - P should be ended with a #0
    // - will write #1..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLine(P: PUtf8Char); overload;
    /// append some chars to the buffer in one line
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLine(P: PUtf8Char; Len: PtrInt); overload;
    /// append some wide chars to the buffer in one line
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLineW(P: PWord; Len: PtrInt);
    /// append an UTF-8 String, with no JSON escaping
    procedure AddString(const Text: RawUtf8);
    /// append several UTF-8 strings
    procedure AddStrings(const Text: array of RawUtf8); overload;
    /// append an UTF-8 string several times
    procedure AddStrings(const Text: RawUtf8; count: PtrInt); overload;
    /// append a ShortString
    procedure AddShort(const Text: ShortString);
    /// append a TShort8 - Text should be not '', and up to 8 chars long
    // - this method is aggressively inlined, so may be preferred to AddShort()
    // for appending simple UTF-8 constant text
    procedure AddShorter(const Text: TShort8);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 'null' as text
    procedure AddNull;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a sub-part of an UTF-8 String
    // - emulates AddString(copy(Text,start,len))
    procedure AddStringCopy(const Text: RawUtf8; start,len: PtrInt);
    /// append after trim first lowercase chars ('otDone' will add 'Done' e.g.)
    procedure AddTrimLeftLowerCase(Text: PShortString);
    /// append a UTF-8 String excluding any space or control char
    // - this won't escape the text as expected by JSON
    procedure AddTrimSpaces(const Text: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a UTF-8 String excluding any space or control char
    // - this won't escape the text as expected by JSON
    procedure AddTrimSpaces(P: PUtf8Char); overload;
    /// append some chars, replacing a given character with another
    procedure AddReplace(Text: PUtf8Char; Orig, Replaced: AnsiChar);
    /// append some chars, quoting all " chars
    // - same algorithm than AddString(QuotedStr()) - without memory allocation,
    // and with an optional maximum text length (truncated with ending '...')
    // - this function implements what is specified in the official SQLite3
    // documentation: "A string constant is formed by enclosing the string in single
    // quotes ('). A single quote within the string can be encoded by putting two
    // single quotes in a row - as in Pascal."
    procedure AddQuotedStr(Text: PUtf8Char; TextLen: PtrUInt; Quote: AnsiChar;
      TextMaxLen: PtrInt = 0);
    /// append some chars, escaping all HTML special chars as expected
    procedure AddHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat = hfAnyWhere); overload;
    /// append some chars, escaping all HTML special chars as expected
    procedure AddHtmlEscape(Text: PUtf8Char; TextLen: PtrInt;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere); overload;
    /// append some VCL/LCL chars, escaping all HTML special chars as expected
    procedure AddHtmlEscapeString(const Text: string;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere);
    /// append some chars, escaping all HTML special chars as expected
    procedure AddHtmlEscapeUtf8(const Text: RawUtf8;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere);
    /// append some chars, escaping all XML special chars as expected
    // - i.e.   < > & " '  as   &lt; &gt; &amp; &quote; &apos;
    // - and all control chars (i.e. #1..#31) as &#..;
    // - see @http://www.w3.org/TR/xml/#syntax
    procedure AddXmlEscape(Text: PUtf8Char);
    /// append a property name, as '"PropName":'
    // - PropName content should not need to be JSON escaped (e.g. no " within,
    // and only ASCII 7-bit characters)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    procedure AddProp(PropName: PUtf8Char; PropNameLen: PtrInt);
    /// append a ShortString property name, as '"PropName":'
    // - PropName content should not need to be JSON escaped (e.g. no " within,
    // and only ASCII 7-bit characters)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    // - is a wrapper around AddProp()
    procedure AddPropName(const PropName: ShortString);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a RawUtf8 property name, as '"FieldName":'
    // - FieldName content should not need to be JSON escaped (e.g. no " within)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    // - is a wrapper around AddProp()
    procedure AddFieldName(const FieldName: RawUtf8);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the class name of an Object instance as text
    procedure AddClassName(aClass: TClass);
    /// append an Instance name and pointer, as '"TObjectList(00425E68)"'+SepChar
    // - append "void" if Instance = nil
    procedure AddInstanceName(Instance: TObject; SepChar: AnsiChar);
    /// append an Instance name and pointer, as 'TObjectList(00425E68)'+SepChar
    procedure AddInstancePointer(Instance: TObject; SepChar: AnsiChar;
      IncludeUnitName, IncludePointer: boolean);
    /// append some binary data as hexadecimal text conversion
    procedure AddBinToHex(Bin: Pointer; BinBytes: PtrInt);
    /// fast conversion from binary data into hexa chars, ready to be displayed
    // - using this function with Bin^ as an integer value will serialize it
    // in big-endian order (most-significant byte first), as used by humans
    // - up to the internal buffer bytes may be converted
    procedure AddBinToHexDisplay(Bin: pointer; BinBytes: PtrInt);
    /// fast conversion from binary data into MSB hexa chars
    // - up to the internal buffer bytes may be converted
    procedure AddBinToHexDisplayLower(Bin: pointer; BinBytes: PtrInt;
      QuotedChar: AnsiChar = #0);
    /// fast conversion from binary data into quoted MSB lowercase hexa chars
    // - up to the internal buffer bytes may be converted
    procedure AddBinToHexDisplayQuoted(Bin: pointer; BinBytes: PtrInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a Value as significant hexadecimal text
    // - expects BinBytes to be > 0
    // - append its minimal chars, i.e. excluding highest bytes containing 0
    // - use GetNextItemHexa() to decode such a text value
    procedure AddBinToHexDisplayMinChars(Bin: pointer; BinBytes: PtrInt;
      QuotedChar: AnsiChar = #0);
    /// add the pointer into significant hexa chars, ready to be displayed
    // - append its minimal chars i.e. excluding highest bytes containing 0
    procedure AddPointer(P: PtrUInt; QuotedChar: AnsiChar = #0);
    /// write a byte as two hexa chars
    procedure AddByteToHex(Value: PtrUInt);
    /// write a Int18 value (0..262143) as 3 chars
    // - this encoding is faster than Base64, and has spaces on the left side
    // - use function Chars3ToInt18() to decode the textual content
    procedure AddInt18ToChars3(Value: cardinal);

    /// append strings or integers with a specified format
    // - this class implementation will raise an exception for twJsonEscape,
    // and simply call FormatUtf8() over a temp RawUtf8 for twNone/twOnSameLine
    // - use faster and more complete overriden TTextWriter.Add instead!
    procedure Add(const Format: RawUtf8; const Values: array of const;
      Escape: TTextWriterKind = twNone;
      WriteObjectOptions: TTextWriterWriteObjectOptions = [woFullExpand]); overload; virtual;
    /// this class implementation will raise an exception
    // - use overriden TTextWriter version instead!
    function AddJsonReformat(Json: PUtf8Char; Format: TTextWriterJsonFormat;
      EndOfObject: PUtf8Char): PUtf8Char; virtual;
    /// this class implementation will raise an exception
    // - use overriden TTextWriter version instead!
    procedure AddVariant(const Value: variant; Escape: TTextWriterKind = twJsonEscape;
      WriteOptions: TTextWriterWriteObjectOptions = []); virtual;
    /// this class implementation will raise an exception
    // - use overriden TTextWriter version instead!
    // - TypeInfo is a PRttiInfo instance - but not available in this early unit
    procedure AddTypedJson(Value: pointer; TypeInfo: pointer;
      WriteOptions: TTextWriterWriteObjectOptions = []); virtual;
    /// write some #0 ended UTF-8 text, according to the specified format
    // - use overriden TTextWriter version instead!
    procedure Add(P: PUtf8Char; Escape: TTextWriterKind); overload; virtual;
    /// write some #0 ended UTF-8 text, according to the specified format
    // - use overriden TTextWriter version instead!
    procedure Add(P: PUtf8Char; Len: PtrInt; Escape: TTextWriterKind); overload; virtual;
    /// write some data Base64 encoded
    // - use overriden TTextWriter version instead!
    procedure WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean); virtual;

    /// serialize as JSON the given object
    // - use overriden TTextWriter version instead!
    procedure WriteObject(Value: TObject;
      WriteOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]); virtual;
    /// append a T*ObjArray dynamic array as a JSON array
    // - for proper serialization on Delphi 7-2009, use Rtti.RegisterObjArray()
    procedure AddObjArrayJson(const aObjArray;
      aOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]);
    /// return the last char appended
    // - returns #0 if no char has been written yet, or the buffer has been just
    // flushed: so this method is to be handled only in some particular usecases
    function LastChar: AnsiChar;
    /// how many bytes are currently in the internal buffer and not on disk/stream
    // - see TextLength for the total number of bytes, on both stream and memory
    function PendingBytes: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// how many bytes were currently written on disk/stream
    // - excluding the bytes in the internal buffer (see PendingBytes)
    // - see TextLength for the total number of bytes, on both stream and memory
    property WrittenBytes: PtrUInt
      read fTotalFileSize;
    /// low-level access to the current indentation level
    property HumanReadableLevel: integer
      read fHumanReadableLevel write fHumanReadableLevel;
    /// the last char appended is canceled
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure CancelLastChar; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// the last char appended is canceled, if match the supplied one
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure CancelLastChar(aCharToCancel: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// the last char appended is canceled if it was a ','
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure CancelLastComma;
      {$ifdef HASINLINE}inline;{$endif}
    /// rewind the Stream to the position when Create() was called
    // - note that this does not clear the Stream content itself, just
    // move back its writing position to its initial place
    procedure CancelAll;

    /// count of added bytes to the stream
    // - see PendingBytes for the number of bytes currently in the memory buffer
    // or WrittenBytes for the number of bytes already written to disk/stream
    property TextLength: PtrUInt
      read GetTextLength;
    /// the internal TStream used for storage
    // - you should call the FlushFinal (or FlushToStream) methods before using
    // this TStream content, to flush all pending characters
    // - if the TStream instance has not been specified when calling the
    // TBaseWriter constructor, it can be forced via this property, before
    // any writting
    property Stream: TStream
      read fStream write SetStream;
    /// global options to customize this TBaseWriter instance process
    // - allows to override e.g. AddRecordJson() and AddDynArrayJson() behavior
    property CustomOptions: TTextWriterOptions
      read fCustomOptions write fCustomOptions;
    /// optional event called before FlushToStream method process
    // - used e.g. by TEchoWriter to perform proper content echoing
    property OnFlushToStream: TOnTextWriterFlush
      read fOnFlushToStream write fOnFlushToStream;
  end;

  /// class of our simple TEXT format writer to a Stream
  TBaseWriterClass = class of TBaseWriter;

var
  /// contains the default JSON serialization class for the framework
  // - by default, TBaseWriter of this unit doesn't support WriteObject and
  // all fancy ways of JSON serialization
  // - end-user code should use this meta-class to initialize the best
  // available serializer class - e.g. TTextWriter from mormot.core.json
  DefaultTextWriterSerializer: TBaseWriterClass = TBaseWriter;

/// will serialize any TObject into its UTF-8 JSON representation
/// - serialize as JSON the published integer, Int64, floating point values,
// TDateTime (stored as ISO 8601 text), string, variant and enumerate
// (e.g. boolean) properties of the object (and its parents)
// - would set twoForceJsonStandard to force standard (non-extended) JSON
// - the enumerates properties are stored with their integer index value
// - will write also the properties published in the parent classes
// - nested properties are serialized as nested JSON objects
// - any TCollection property will also be serialized as JSON arrays
// - you can add some custom serializers for ANY class, via mormot.core.json.pas
// TRttiJson.RegisterCustomSerializer() class method
// - call internally TBaseWriter.WriteObject() method from DefaultTextWriterSerializer
function ObjectToJson(Value: TObject;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]): RawUtf8;

/// escape some UTF-8 text into HTML
// - just a wrapper around TBaseWriter.AddHtmlEscape() process,
// replacing < > & " chars depending on the HTML layer
function HtmlEscape(const text: RawUtf8;
  fmt: TTextWriterHtmlFormat = hfAnyWhere): RawUtf8;

/// escape some VCL/LCL text into UTF-8 HTML
// - just a wrapper around TBaseWriter.AddHtmlEscapeString() process,
// replacing < > & " chars depending on the HTML layer
function HtmlEscapeString(const text: string;
  fmt: TTextWriterHtmlFormat = hfAnyWhere): RawUtf8;


const
  /// JSON serialization options focusing of sets support
  // - as used e.g. by TTextWriter.AddRecordJson/AddDynArrayJson and
  // TDynArray.SaveJson methods, and SaveJson/RecordSaveJson functions
  // - to be used as TEXTWRITEROPTIONS_TEXTSET[EnumSetsAsText]
  TEXTWRITEROPTIONS_SETASTEXT: array[boolean] of TTextWriterOptions = (
    [twoFullSetsAsStar],
    [twoFullSetsAsStar, twoEnumSetsAsTextInRecord]);

type
  /// callback used to echo each line of TEchoWriter class
  // - should return TRUE on success, FALSE if the log was not echoed: but
  // TSynLog will continue logging, even if this event returned FALSE
  TOnTextWriterEcho = function(Sender: TBaseWriter; Level: TSynLogInfo;
    const Text: RawUtf8): boolean of object;

  /// add optional echoing of the lines to TBaseWriter
  // - as used e.g. by TSynLog writer for log optional redirection
  // - is defined as a nested class to reduce plain TBaseWriter scope, and
  // better follow the SOLID principles
  TEchoWriter = class
  protected
    fWriter: TBaseWriter;
    fEchoStart: PtrInt;
    fEchoBuf: RawUtf8;
    fEchos: array of TOnTextWriterEcho;
    function EchoFlush: PtrInt;
    function GetEndOfLineCRLF: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetEndOfLineCRLF(aEndOfLineCRLF: boolean);
  public
    /// prepare for the echoing process
    constructor Create(Owner: TBaseWriter); reintroduce;
    /// end the echoing process
    destructor Destroy; override;
    /// should be called from TBaseWriter.FlushToStream
    // - write pending data to the Stream, with automatic buffer resizal and echoing
    // - this overriden method will handle proper echoing
    procedure FlushToStream(Text: PUtf8Char; Len: PtrInt);
    /// mark an end of line, ready to be "echoed" to registered listeners
    // - append a LF (#10) char or CR+LF (#13#10) chars to the buffer, depending
    // on the EndOfLineCRLF property value (default is LF, to minimize storage)
    // - any callback registered via EchoAdd() will monitor this line
    // - used e.g. by TSynLog for console output, as stated by Level parameter
    procedure AddEndOfLine(aLevel: TSynLogInfo = sllNone);
    /// add a callback to echo each line written by this class
    // - this class expects AddEndOfLine to mark the end of each line
    procedure EchoAdd(const aEcho: TOnTextWriterEcho);
    /// remove a callback to echo each line written by this class
    // - event should have been previously registered by a EchoAdd() call
    procedure EchoRemove(const aEcho: TOnTextWriterEcho);
    /// reset the internal buffer used for echoing content
    procedure EchoReset;
    /// the associated TBaseWriter instance
    property Writer: TBaseWriter
      read fWriter;
    /// define how AddEndOfLine method stores its line feed characters
    // - by default (FALSE), it will append a LF (#10) char to the buffer
    // - you can set this property to TRUE, so that CR+LF (#13#10) chars will
    // be appended instead
    // - is just a wrapper around twoEndOfLineCRLF item in CustomOptions
    property EndOfLineCRLF: boolean
      read GetEndOfLineCRLF write SetEndOfLineCRLF;
  end;


{ ************ TRawUtf8DynArray Processing Functions }

/// returns TRUE if Value is nil or all supplied Values[] equal ''
function IsZero(const Values: TRawUtf8DynArray): boolean; overload;

/// quick helper to initialize a dynamic array of RawUtf8 from some constants
// - can be used e.g. as:
// ! MyArray := TRawUtf8DynArrayFrom(['a','b','c']);
function TRawUtf8DynArrayFrom(const Values: array of RawUtf8): TRawUtf8DynArray;

/// low-level efficient search of Value in Values[]
// - CaseSensitive=false will use StrICmp() for A..Z / a..z equivalence
function FindRawUtf8(Values: PRawUtf8; const Value: RawUtf8; ValuesCount: integer;
  CaseSensitive: boolean): integer; overload;

/// return the index of Value in Values[], -1 if not found
// - CaseSensitive=false will use StrICmp() for A..Z / a..z equivalence
function FindRawUtf8(const Values: TRawUtf8DynArray; const Value: RawUtf8;
  CaseSensitive: boolean = true): integer; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return the index of Value in Values[], -1 if not found
// - CaseSensitive=false will use StrICmp() for A..Z / a..z equivalence
function FindRawUtf8(const Values: array of RawUtf8; const Value: RawUtf8;
  CaseSensitive: boolean = true): integer; overload;

/// return the index of Value in Values[], -1 if not found
// - here name search would use fast IdemPropNameU() function
function FindPropName(const Names: array of RawUtf8; const Name: RawUtf8): integer; overload;

/// return the index of Value in Values[] using IdemPropNameU(), -1 if not found
// - typical use with a dynamic array is like:
// ! index := FindPropName(pointer(aDynArray),aValue,length(aDynArray));
function FindPropName(Values: PRawUtf8;
  const Value: RawUtf8; ValuesCount: integer): integer; overload;

/// true if Value was added successfully in Values[]
function AddRawUtf8(var Values: TRawUtf8DynArray; const Value: RawUtf8;
  NoDuplicates: boolean = false; CaseSensitive: boolean = true): boolean; overload;

/// add the Value to Values[], with an external count variable, for performance
procedure AddRawUtf8(var Values: TRawUtf8DynArray; var ValuesCount: integer;
  const Value: RawUtf8); overload;

/// true if both TRawUtf8DynArray are the same
// - comparison is case-sensitive
function RawUtf8DynArrayEquals(const A, B: TRawUtf8DynArray): boolean; overload;

/// true if both TRawUtf8DynArray are the same for a given number of items
// - A and B are expected to have at least Count items
// - comparison is case-sensitive
function RawUtf8DynArrayEquals(const A, B: TRawUtf8DynArray;
  Count: integer): boolean; overload;

/// add the Value to Values[] string array
function AddString(var Values: TStringDynArray; const Value: string): PtrInt;

/// convert the string dynamic array into a dynamic array of UTF-8 strings
procedure StringDynArrayToRawUtf8DynArray(const Source: TStringDynArray;
  var result: TRawUtf8DynArray);

/// convert the string list into a dynamic array of UTF-8 strings
procedure StringListToRawUtf8DynArray(Source: TStringList;
  var result: TRawUtf8DynArray);

/// retrieve the index where to insert a PUtf8Char in a sorted PUtf8Char array
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison is case-sensitive StrComp (so will work with any PAnsiChar)
// - returns -1 if the specified Value was found (i.e. adding will duplicate a value)
// - will use fast O(log(n)) binary search algorithm
function FastLocatePUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char): PtrInt; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the index where to insert a PUtf8Char in a sorted PUtf8Char array
// - this overloaded function accept a custom comparison function for sorting
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison is case-sensitive (so will work with any PAnsiChar)
// - returns -1 if the specified Value was found (i.e. adding will duplicate a value)
// - will use fast O(log(n)) binary search algorithm
function FastLocatePUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char; Compare: TUtf8Compare): PtrInt; overload;

/// retrieve the index where is located a PUtf8Char in a sorted PUtf8Char array
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison is case-sensitive StrComp (so will work with any PAnsiChar)
// - returns -1 if the specified Value was not found
// - will use inlined binary search algorithm with optimized x86_64 branchless asm
// - slightly faster than plain FastFindPUtf8CharSorted(P,R,Value,@StrComp)
function FastFindPUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char): PtrInt; overload;

/// retrieve the index where is located a PUtf8Char in a sorted uppercase PUtf8Char array
// - P[] array is expected to be already uppercased
// - searched Value is converted to uppercase before search via UpperCopy255Buf(),
// so is expected to be short, i.e. length < 250
// - R is the last index of available entries in P^ (i.e. Count-1)
// - returns -1 if the specified Value was not found
// - will use fast O(log(n)) binary search algorithm
// - slightly faster than plain FastFindPUtf8CharSorted(P,R,Value,@StrIComp)
function FastFindUpperPUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char; ValueLen: PtrInt): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the index where is located a PUtf8Char in a sorted PUtf8Char array
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison will use the specified Compare function
// - returns -1 if the specified Value was not found
// - will use fast O(log(n)) binary search algorithm
function FastFindPUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char; Compare: TUtf8Compare): PtrInt; overload;

/// retrieve the index of a PUtf8Char in a PUtf8Char array via a sort indexed
// - will use fast O(log(n)) binary search algorithm
function FastFindIndexedPUtf8Char(P: PPUtf8CharArray; R: PtrInt;
  var SortedIndexes: TCardinalDynArray; Value: PUtf8Char;
  ItemComp: TUtf8Compare): PtrInt;

/// add a RawUtf8 value in an alphaticaly sorted dynamic array of RawUtf8
// - returns the index where the Value was added successfully in Values[]
// - returns -1 if the specified Value was alredy present in Values[]
//  (we must avoid any duplicate for O(log(n)) binary search)
// - if CoValues is set, its content will be moved to allow inserting a new
// value at CoValues[result] position - a typical usage of CoValues is to store
// the corresponding ID to each RawUtf8 item
// - if FastLocatePUtf8CharSorted() has been already called, this index can
// be set to optional ForceIndex parameter
// - by default, exact (case-sensitive) match is used; you can specify a custom
// compare function if needed in Compare optional parameter
function AddSortedRawUtf8(var Values: TRawUtf8DynArray;
  var ValuesCount: integer; const Value: RawUtf8;
  CoValues: PIntegerDynArray = nil; ForcedIndex: PtrInt = -1;
  Compare: TUtf8Compare = nil): PtrInt;

/// delete a RawUtf8 item in a dynamic array of RawUtf8
// - if CoValues is set, the integer item at the same index is also deleted
function DeleteRawUtf8(var Values: TRawUtf8DynArray; var ValuesCount: integer;
  Index: integer; CoValues: PIntegerDynArray = nil): boolean; overload;

/// delete a RawUtf8 item in a dynamic array of RawUtf8;
function DeleteRawUtf8(var Values: TRawUtf8DynArray;
  Index: integer): boolean; overload;

/// sort a dynamic array of RawUtf8 items
// - if CoValues is set, the integer items are also synchronized
// - by default, exact (case-sensitive) match is used; you can specify a custom
// compare function if needed in Compare optional parameter
procedure QuickSortRawUtf8(var Values: TRawUtf8DynArray; ValuesCount: integer;
  CoValues: PIntegerDynArray = nil; Compare: TUtf8Compare = nil); overload;

/// sort a RawUtf8 array, low values first
procedure QuickSortRawUtf8(Values: PRawUtf8Array; L, R: PtrInt;
  caseInsensitive: boolean = false); overload;



{ ************ Numbers (integers or floats) and Variants to Text Conversion }

var
  /// naive but efficient cache to avoid string memory allocation for
  // 0..999 small numbers by Int32ToUtf8/UInt32ToUtf8
  // - use around 16KB of heap (since each item consumes 16 bytes), but increase
  // overall performance and reduce memory allocation (and fragmentation),
  // especially during multi-threaded execution
  // - noticeable when strings are used as array indexes (e.g.
  // in mormot.db.nosql.bson)
  // - is defined globally, since may be used from an inlined function
  SmallUInt32Utf8: array[0..999] of RawUtf8;

/// fast RawUtf8 version of 32-bit IntToStr()
function Int32ToUtf8(Value: PtrInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast RawUtf8 version of 32-bit IntToStr()
// - result as var parameter saves a local assignment and a try..finally
procedure Int32ToUtf8(Value: PtrInt; var result: RawUtf8); overload;

/// fast RawUtf8 version of 64-bit IntToStr()
function Int64ToUtf8(Value: Int64): RawUtf8; overload;
  {$ifdef HASSAFEINLINE}inline;{$endif} // Delphi 2007 has trouble inlining this

/// fast RawUtf8 version of 64-bit IntToStr()
// - result as var parameter saves a local assignment and a try..finally
procedure Int64ToUtf8(Value: Int64; var result: RawUtf8); overload;

/// fast RawUtf8 version of 32-bit IntToStr()
function ToUtf8(Value: PtrInt): RawUtf8; overload;

{$ifdef CPU32}
/// fast RawUtf8 version of 64-bit IntToStr()
function ToUtf8(Value: Int64): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}
{$endif CPU32}

/// optimized conversion of a cardinal into RawUtf8
function UInt32ToUtf8(Value: PtrUInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// optimized conversion of a cardinal into RawUtf8
procedure UInt32ToUtf8(Value: PtrUInt; var result: RawUtf8); overload;

/// fast RawUtf8 version of 64-bit IntToStr(), with proper QWord support
procedure UInt64ToUtf8(Value: QWord; var result: RawUtf8);

/// convert a string into its INTEGER Curr64 (value*10000) representation
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - if NoDecimal is defined, will be set to TRUE if there is no decimal, AND
// the returned value will be an Int64 (not a PInt64(@Curr)^)
function StrToCurr64(P: PUtf8Char; NoDecimal: PBoolean = nil): Int64;

/// convert a string into its currency representation
// - will call StrToCurr64()
function StrToCurrency(P: PUtf8Char): currency;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a currency value into a string
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
function CurrencyToStr(const Value: currency): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
function Curr64ToStr(const Value: Int64): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
procedure Curr64ToStr(const Value: Int64; var result: RawUtf8); overload;

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
// - return the number of chars written to Dest^
function Curr64ToPChar(const Value: Int64; Dest: PUtf8Char): PtrInt;

/// internal fast INTEGER Curr64 (value*10000) value to text conversion
// - expect the last available temporary char position in P
// - return the last written char position (write in reverse order in P^)
// - will return 0 for Value=0, or a string representation with always 4 decimals
//   (e.g. 1->'0.0001' 500->'0.0500' 25000->'2.5000' 30000->'3.0000')
// - is called by Curr64ToPChar() and Curr64ToStr() functions
function StrCurr64(P: PAnsiChar; const Value: Int64): PAnsiChar;

/// faster than default SysUtils.IntToStr implementation
function IntToString(Value: integer): string; overload;

/// faster than default SysUtils.IntToStr implementation
function IntToString(Value: cardinal): string; overload;

/// faster than default SysUtils.IntToStr implementation
function IntToString(Value: Int64): string; overload;

/// convert a floating-point value to its numerical text equivalency
function DoubleToString(Value: Double): string;

/// convert a currency value from its Int64 binary representation into
// its numerical text equivalency
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
function Curr64ToString(Value: Int64): string;

/// convert a floating-point value to its numerical text equivalency
// - on Delphi Win32, calls FloatToText() in ffGeneral mode; on FPC uses str()
// - DOUBLE_PRECISION will redirect to DoubleToShort() and its faster Fabian
// Loitsch's Grisu algorithm if available
// - returns the count of chars stored into S, i.e. length(S)
function ExtendedToShort(var S: ShortString;
  Value: TSynExtended; Precision: integer): integer;

/// convert a floating-point value to its numerical text equivalency without
// scientification notation
// - DOUBLE_PRECISION will redirect to DoubleToShortNoExp() and its faster Fabian
// Loitsch's Grisu algorithm if available - or calls str(Value:0:precision,S)
// - returns the count of chars stored into S, i.e. length(S)
function ExtendedToShortNoExp(var S: ShortString; Value: TSynExtended;
  Precision: integer): integer;

/// check if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - as returned by ExtendedToShort/DoubleToShort textual conversion
// - such values do appear as IEEE floating points, but are not defined in JSON
function FloatToShortNan(const s: shortstring): TFloatNan;
  {$ifdef HASINLINE}inline;{$endif}

/// check if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - as returned e.g. by ExtendedToStr/DoubleToStr textual conversion
// - such values do appear as IEEE floating points, but are not defined in JSON
function FloatToStrNan(const s: RawUtf8): TFloatNan;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a floating-point value to its numerical text equivalency
function ExtendedToStr(Value: TSynExtended; Precision: integer): RawUtf8; overload;

/// convert a floating-point value to its numerical text equivalency
procedure ExtendedToStr(Value: TSynExtended; Precision: integer;
  var result: RawUtf8); overload;

/// recognize if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function FloatToJsonNan(const s: ShortString): PShortString;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a floating-point value to its JSON text equivalency
// - depending on the platform, it may either call str() or FloatToText()
// in ffGeneral mode (the shortest possible decimal string using fixed or
// scientific format)
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function ExtendedToJson(var tmp: ShortString; Value: TSynExtended;
  Precision: integer; NoExp: boolean): PShortString;

/// convert a 64-bit floating-point value to its numerical text equivalency
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm implementation
// - returns the count of chars stored into S, i.e. length(S)
function DoubleToShort(var S: ShortString; const Value: double): integer;
  {$ifdef FPC}inline;{$endif}

/// convert a 64-bit floating-point value to its numerical text equivalency
// without scientific notation
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm implementation
// - returns the count of chars stored into S, i.e. length(S)
function DoubleToShortNoExp(var S: ShortString; const Value: double): integer;
  {$ifdef FPC}inline;{$endif}

{$ifdef DOUBLETOSHORT_USEGRISU}
const
  // special text returned if the double is not a number
  C_STR_INF: string[3] = 'Inf';
  C_STR_QNAN: string[3] = 'Nan';

  // min_width parameter special value, as used internally by FPC for str(d,s)
  // - DoubleToAscii() only accept C_NO_MIN_WIDTH or 0 for min_width: space
  // trailing has been removed in this cut-down version
  C_NO_MIN_WIDTH = -32767;

/// raw function to convert a 64-bit double into a shortstring, stored in str
// - implements Fabian Loitsch's Grisu algorithm dedicated to double values
// - currently, this unit only set min_width=0 (for DoubleToShortNoExp to avoid
// any scientific notation ) or min_width=C_NO_MIN_WIDTH (for DoubleToShort to
// force the scientific notation when the double cannot be represented as
// a simple fractinal number)
procedure DoubleToAscii(min_width, frac_digits: integer;
  const v: double; str: PAnsiChar);
{$endif DOUBLETOSHORT_USEGRISU}

/// convert a 64-bit floating-point value to its JSON text equivalency
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function DoubleToJson(var tmp: ShortString; Value: double;
  NoExp: boolean): PShortString;

/// convert a 64-bit floating-point value to its numerical text equivalency
function DoubleToStr(Value: Double): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit floating-point value to its numerical text equivalency
procedure DoubleToStr(Value: Double; var result: RawUtf8); overload;

/// copy a floating-point text buffer with proper correction and validation
// - will correct on the fly '.5' -> '0.5' and '-.5' -> '-0.5'
// - will end not only on #0 but on any char not matching 1[.2[e[-]3]] pattern
// - is used when the input comes from a third-party source with no regular
// output, e.g. a database driver, via TBaseWriter.AddFloatStr
function FloatStrCopy(s, d: PUtf8Char): PUtf8Char;

/// fast conversion of 2 digit characters into a 0..99 value
// - returns FALSE on success, TRUE if P^ is not correct
function Char2ToByte(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion of 3 digit characters into a 0..9999 value
// - returns FALSE on success, TRUE if P^ is not correct
function Char3ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion of 4 digit characters into a 0..9999 value
// - returns FALSE on success, TRUE if P^ is not correct
function Char4ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}


/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - note: null will be returned as 'null'
function VariantToUtf8(const V: Variant): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - note: null will be returned as 'null'
function ToUtf8(const V: Variant): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant/TVarData into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - note: null will be returned as 'null'
function ToUtf8(const V: TVarData): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - wasString is set if the V value was a text
// - empty and null variants will be stored as 'null' text - as expected by JSON
// - custom variant types (e.g. TDocVariant) will be stored as JSON
procedure VariantToUtf8(const V: Variant; var result: RawUtf8;
   var wasString: boolean); overload;

/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - returns TRUE if the V value was a text, FALSE if was not (e.g. a number)
// - empty and null variants will be stored as 'null' text - as expected by JSON
// - custom variant types (e.g. TDocVariant) will be stored as JSON
function VariantToUtf8(const V: Variant; var Text: RawUtf8): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a variant value into a JSON content
// - is properly implemented by mormot.core.json.pas: if this unit is not
// included in the project, this function will raise an exception
// - follows the TBaseWriter.AddVariant() and VariantLoadJson() format
// - is able to handle simple and custom variant types, for instance:
// !  VariantSaveJson(1.5)='1.5'
// !  VariantSaveJson('test')='"test"'
// !  o := _Json('{ BSON: [ "test", 5.05, 1986 ] }');
// !  VariantSaveJson(o)='{"BSON":["test",5.05,1986]}'
// !  o := _Obj(['name','John','doc',_Obj(['one',1,'two',_Arr(['one',2])])]);
// !  VariantSaveJson(o)='{"name":"John","doc":{"one":1,"two":["one",2]}}'
// - note that before Delphi 2009, any varString value is expected to be
// a RawUtf8 instance - which does make sense in the mORMot area
procedure VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8); overload;

/// save a variant value into a JSON content
// - just a wrapper around the overloaded procedure
function VariantSaveJson(const Value: variant;
  Escape: TTextWriterKind = twJsonEscape): RawUtf8; overload;

var
  /// unserialize a JSON content into a variant
  // - is properly implemented by mormot.core.json.pas: if this unit is not
  // included in the project, this function is nil
  // - used by mormot.core.data.pas RTTI_BINARYLOAD[tkVariant]() for complex types
  BinaryVariantLoadAsJson: procedure(var Value: variant; Json: PUtf8Char;
    TryCustomVariant: pointer);

  /// write a TDateTime into strict ISO-8601 date and/or time text
  // - is implemented by DateTimeToIso8601TextVar from mormot.core.datetime.pas:
  // if this unit is not included in the project, this function is nil
  // - used by VariantToUtf8() for TDateTime conversion
  _VariantToUtf8DateTimeToIso8601: procedure(DT: TDateTime; FirstChar: AnsiChar;
    var result: RawUtf8; WithMS: boolean);

  /// Date/Time conversion from ISO-8601 text
  // - is implemented by Iso8601ToDateTime() from mormot.core.datetime.pas:
  // if this unit is not included in the project, this function is nil
  // - used by TRttiProp.SetValue() for TDateTime properties with a getter
  _Iso8601ToDateTime: function(const iso: RawByteString): TDateTime;


type
  /// used e.g. by UInt4DigitsToShort/UInt3DigitsToShort/UInt2DigitsToShort
  // - such result type would avoid a string allocation on heap
  TShort4 = string[4];


{ ************ Text Formatting functions }

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
procedure VarRecToUtf8(const V: TVarRec; var result: RawUtf8;
  wasString: PBoolean = nil);

type
  /// a memory structure which avoids a temporary RawUtf8 allocation
  // - used by VarRecToTempUtf8() and FormatUtf8()/FormatShort()
  TTempUtf8 = record
    Len: PtrInt;
    Text: PUtf8Char;
    TempRawUtf8: pointer;
    Temp: array[0..23] of AnsiChar;
  end;
  PTempUtf8 = ^TTempUtf8;

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text, using a specified temporary buffer
// - this function would allocate a RawUtf8 in TempRawUtf8 only if needed,
// but use the supplied Res.Temp[] buffer for numbers to text conversion -
// caller should ensure to make RawUtf8(TempRawUtf8) := '' on the entry
// - it would return the number of UTF-8 bytes, i.e. Res.Len
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
function VarRecToTempUtf8(const V: TVarRec; var Res: TTempUtf8): integer;

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text, returning FALSE if the argument was not a string value
function VarRecToUtf8IsString(const V: TVarRec; var value: RawUtf8): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an open array (const Args: array of const) argument to an Int64
// - returns TRUE and set Value if the supplied argument is a vtInteger, vtInt64
// or vtBoolean
// - returns FALSE if the argument is not an integer
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
function VarRecToInt64(const V: TVarRec; out value: Int64): boolean;

/// convert an open array (const Args: array of const) argument to a floating
// point value
// - returns TRUE and set Value if the supplied argument is a number (e.g.
// vtInteger, vtInt64, vtCurrency or vtExtended)
// - returns FALSE if the argument is not a number
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
function VarRecToDouble(const V: TVarRec; out value: double): boolean;

/// convert an open array (const Args: array of const) argument to a value
// encoded as with :(...): inlined parameters in FormatUtf8(Format,Args,Params)
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
procedure VarRecToInlineValue(const V: TVarRec; var result: RawUtf8);

/// get an open array (const Args: array of const) character argument
// - only handle varChar and varWideChar kind of arguments
function VarRecAsChar(const V: TVarRec): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// fast Format() function replacement, optimized for RawUtf8
// - only supported token is %, which will be written in the resulting string
// according to each Args[] supplied items - so you will never get any exception
// as with the SysUtils.Format() when a specifier is incorrect
// - resulting string has no length limit and uses fast concatenation
// - there is no escape char, so to output a '%' character, you need to use '%'
// as place-holder, and specify '%' as value in the Args array
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
function FormatUtf8(const Format: RawUtf8; const Args: array of const): RawUtf8; overload;

/// fast Format() function replacement, optimized for RawUtf8
// - overloaded function, which avoid a temporary RawUtf8 instance on stack
procedure FormatUtf8(const Format: RawUtf8; const Args: array of const;
  out result: RawUtf8); overload;

/// fast Format() function replacement, tuned for direct memory buffer write
// - use the same single token % (and implementation) than FormatUtf8()
// - returns the number of UTF-8 bytes appended to Dest^
function FormatBuffer(const Format: RawUtf8; const Args: array of const;
  Dest: pointer; DestLen: PtrInt): PtrInt;

/// fast Format() function replacement, for UTF-8 content stored in shortstring
// - use the same single token % (and implementation) than FormatUtf8()
// - shortstring allows fast stack allocation, so is perfect for small content
// - truncate result if the text size exceeds 255 bytes
procedure FormatShort(const Format: RawUtf8; const Args: array of const;
  var result: shortstring);

/// fast Format() function replacement, for UTF-8 content stored in shortstring
function FormatToShort(const Format: RawUtf8; const Args: array of const): shortstring;

/// fast Format() function replacement, tuned for small content
// - use the same single token % (and implementation) than FormatUtf8()
procedure FormatString(const Format: RawUtf8; const Args: array of const;
  out result: string); overload;

/// fast Format() function replacement, tuned for small content
// - use the same single token % (and implementation) than FormatUtf8()
function FormatString(const Format: RawUtf8; const Args: array of const): string; overload;
  {$ifdef FPC}inline;{$endif}

/// fast Format() function replacement, for UTF-8 content stored in TShort16
// - truncate result if the text size exceeds 16 bytes
procedure FormatShort16(const Format: RawUtf8; const Args: array of const;
  var result: TShort16);

/// direct conversion of a VCL string into a console OEM-encoded String
// - under Windows, will use the CP_OEMCP encoding
// - under Linux, will expect the console to be defined with UTF-8 encoding
function StringToConsole(const S: string): RawByteString;

/// write some text to the console using a given color
procedure ConsoleWrite(const Fmt: RawUtf8; const Args: array of const;
  Color: TConsoleColor = ccLightGray; NoLineFeed: boolean = false); overload;

/// could be used in the main program block of a console application to
// handle unexpected fatal exceptions
// - WaitForEnterKey=true won't do anything on POSIX (to avoid locking a daemon)
// - typical use may be:
// !begin
// !  try
// !    ... // main console process
// !  except
// !    on E: Exception do
// !      ConsoleShowFatalException(E);
// !  end;
// !end.
procedure ConsoleShowFatalException(E: Exception; WaitForEnterKey: boolean = true);


{ ************ Resource and Time Functions }

/// convert a size to a human readable value power-of-two metric value
// - append EB, PB, TB, GB, MB, KB or B symbol with or without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
procedure KB(bytes: Int64; out result: TShort16; nospace: boolean); overload;

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol with preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(bytes: Int64): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KBNoSpace(bytes: Int64): TShort16;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol with or without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(bytes: Int64; nospace: boolean): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a string size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(const buffer: RawByteString): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif}

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
procedure KBU(bytes: Int64; var result: RawUtf8);

/// convert a count to a human readable value power-of-two metric value
// - append E, P, T, G, M, K symbol, with one fractional digit
procedure K(value: Int64; out result: TShort16); overload;

  /// convert a count to a human readable value power-of-two metric value
  // - append E, P, T, G, M, K symbol, with one fractional digit
function K(value: Int64): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a micro seconds elapsed time into a human readable value
// - append 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
function MicroSecToString(Micro: QWord): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a micro seconds elapsed time into a human readable value
// - append 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
procedure MicroSecToString(Micro: QWord; out result: TShort16); overload;

/// convert a nano seconds elapsed time into a human readable value
// - append 'ns', 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value
// range, with two fractional digits
procedure NanoSecToString(Nano: QWord; out result: TShort16);

/// convert "valueunit" values into x or x.xx text with up to 2 digits
// - supplied value should be the actual unit value * 100
procedure By100ToTwoDigitString(value: cardinal; const valueunit: shortstring;
  var result: TShort16);

/// convert an integer value into its textual representation with thousands marked
// - ThousandSep is the character used to separate thousands in numbers with
// more than three digits to the left of the decimal separator
function IntToThousandString(Value: integer;
  const ThousandSep: TShort4 = ','): shortstring;


{ ************ ESynException class }

{$ifndef NOEXCEPTIONINTERCEPT}

type
  /// global hook callback to customize exceptions logged by TSynLog
  // - should return TRUE if all needed information has been logged by the
  // event handler
  // - should return FALSE if Context.EAddr and Stack trace is to be appended
  TSynLogExceptionToStr = function(WR: TBaseWriter;
    const Context: TSynLogExceptionContext): boolean;

var
  /// allow to customize the ESynException logging message
  TSynLogExceptionToStrCustom: TSynLogExceptionToStr = nil;

/// the default Exception handler for logging
// - defined here to be called e.g. by ESynException.CustomLog() as default
function DefaultSynLogExceptionToStr(WR: TBaseWriter;
  const Context: TSynLogExceptionContext): boolean;

{$endif NOEXCEPTIONINTERCEPT}


type
  {$M+}
  /// generic parent class of all custom Exception types of this unit
  // - all our classes inheriting from ESynException are serializable,
  // so you could use ObjectToJsonDebug(anyESynException) to retrieve some
  // extended information
  ESynException = class(Exception)
  protected
    fRaisedAt: pointer;
  public
    /// constructor which will use FormatUtf8() instead of Format()
    // - expect % as delimiter, so is less error prone than %s %d %g
    // - will handle vtPointer/vtClass/vtObject/vtVariant kind of arguments,
    // appending class name for any class or object, the hexa value for a
    // pointer, or the JSON representation of any supplied TDocVariant
    constructor CreateUtf8(const Format: RawUtf8; const Args: array of const);
    /// constructor appending some FormatUtf8() content to the GetLastError
    // - message will contain GetLastError value followed by the formatted text
    // - expect % as delimiter, so is less error prone than %s %d %g
    // - will handle vtPointer/vtClass/vtObject/vtVariant kind of arguments,
    // appending class name for any class or object, the hexa value for a
    // pointer, or the JSON representation of any supplied TDocVariant
    constructor CreateLastOSError(const Format: RawUtf8; const Args: array of const);
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// can be used to customize how the exception is logged
    // - this default implementation will call the TSynLogExceptionToStrCustom
    // global callback, if defined, or a default handler internal to this unit
    // - override this method to provide a custom logging content
    // - should return TRUE if Context.EAddr and Stack trace is not to be
    // written (i.e. as for any TSynLogExceptionToStr callback)
    function CustomLog(WR: TBaseWriter;
      const Context: TSynLogExceptionContext): boolean; virtual;
    {$endif NOEXCEPTIONINTERCEPT}
    /// the code location when this exception was triggered
    // - populated by mormot.core.log unit, during interception - so may be nil
    // - you can use TDebugFile.FindLocation(ESynException) class function to
    // guess the corresponding source code line
    // - will be serialized as "Address": hexadecimal and source code location,
    // using TDebugFile .map/.dbg/.mab information, by JSON WriteObject
    // when woStorePointer option is defined - e.g. with ObjectToJsonDebug()
    property RaisedAt: pointer
      read fRaisedAt write fRaisedAt;
  published
    /// the Exception Message string, as defined in parent Exception class
    property Message;
  end;
  {$M-}

  /// meta-class of the ESynException hierarchy
  ESynExceptionClass = class of ESynException;

/// convert any HTTP_* constant to an integer error code and its English text
// - see @http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
// - calls StatusCodeToReason() to retrieve the text message
function StatusCodeToErrorMsg(Code: integer): shortstring;


{ **************** Hexadecimal Text And Binary Conversion }


type
  /// lookup table used for fast hexadecimal conversion
  THexToDualByte = packed array[0..511] of byte;
  TAnsiCharToByte = array[AnsiChar] of byte;
  TAnsiCharToWord = array[AnsiChar] of word;
  TByteToWord = array[byte] of word;
  PByteToWord = ^TByteToWord;

var
  /// a conversion table from hexa chars into binary data
  // - [0..255] range maps the 0..15 binary, [256..511] maps 0..15 binary shl 4
  // - returns 255 for any character out of 0..9,A..Z,a..z range
  // - used e.g. by HexToBin() function
  // - is defined globally, since may be used from an inlined function
  ConvertHexToBin: THexToDualByte;

  /// fast lookup table for converting hexadecimal numbers from 0 to 15
  // into their ASCII equivalence
  // - is local for better code generation
  TwoDigitsHex: array[byte] of array[1..2] of AnsiChar;
  TwoDigitsHexW: TAnsiCharToWord absolute TwoDigitsHex;
  TwoDigitsHexWB: TByteToWord absolute TwoDigitsHex;
  /// lowercase hexadecimal lookup table
  TwoDigitsHexLower: array[byte] of array[1..2] of AnsiChar;
  TwoDigitsHexWLower: TAnsiCharToWord absolute TwoDigitsHexLower;
  TwoDigitsHexWBLower: TByteToWord absolute TwoDigitsHexLower;

/// fast conversion from hexa chars into binary data
// - BinBytes contain the bytes count to be converted: Hex^ must contain
//  at least BinBytes*2 chars to be converted, and Bin^ enough space
// - if Bin=nil, no output data is written, but the Hex^ format is checked
// - return false if any invalid (non hexa) char is found in Hex^
// - using this function with Bin^ as an integer value will decode in big-endian
// order (most-signignifican byte first)
function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean; overload;

/// fast conversion with no validity check from hexa chars into binary data
procedure HexToBinFast(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt);

/// fast conversion from one hexa char pair into a 8-bit AnsiChar
// - return false if any invalid (non hexa) char is found in Hex^
// - similar to HexToBin(Hex,nil,1)
function HexToCharValid(Hex: PAnsiChar): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast check if the supplied Hex buffer is an hexadecimal representation
// of a binary buffer of a given number of bytes
function IsHex(const Hex: RawByteString; BinBytes: PtrInt): boolean;

/// fast conversion from one hexa char pair into a 8-bit AnsiChar
// - return false if any invalid (non hexa) char is found in Hex^
// - similar to HexToBin(Hex,Bin,1) but with Bin<>nil
// - use HexToCharValid if you want to check a hexadecimal char content
function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from two hexa bytes into a 16-bit UTF-16 WideChar
// - as used by JsonUnicodeEscapeToUtf8() for \u#### chars unescape
// - similar to HexDisplayToBin(Hex,@wordvar,2)
// - returns 0 on malformated input
function HexToWideChar(Hex: PUtf8Char): cardinal;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with BinBytes^ as an integer value will encode it
// in low-endian order (less-signignifican byte first): don't use it for display
procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from hexa chars into binary data
function HexToBin(const Hex: RawUtf8): RawByteString; overload;

/// fast conversion from binary data into hexa chars
function BinToHex(const Bin: RawByteString): RawUtf8; overload;

/// fast conversion from binary data into hexa chars
function BinToHex(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from binary data into hexa chars, ready to be displayed
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with Bin^ as an integer value will encode it
// in big-endian order (most-signignifican byte first): use it for display
procedure BinToHexDisplay(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from binary data into hexa chars, ready to be displayed
function BinToHexDisplay(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from binary data into lowercase hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with BinBytes^ as an integer value will encode it
// in low-endian order (less-signignifican byte first): don't use it for display
procedure BinToHexLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from binary data into lowercase hexa chars
function BinToHexLower(const Bin: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
function BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
procedure BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt; var result: RawUtf8); overload;

/// fast conversion from binary data into lowercase hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with Bin^ as an integer value will encode it
// in big-endian order (most-signignifican byte first): use it for display
procedure BinToHexDisplayLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from binary data into lowercase hexa chars
function BinToHexDisplayLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from up to 127 bytes of binary data into lowercase hexa chars
function BinToHexDisplayLowerShort(Bin: PAnsiChar; BinBytes: PtrInt): shortstring;

/// fast conversion from up to 64-bit of binary data into lowercase hexa chars
function BinToHexDisplayLowerShort16(Bin: Int64; BinBytes: PtrInt): TShort16;

/// fast conversion from binary data into hexa lowercase chars, ready to be
// used as a convenient TFileName prefix
function BinToHexDisplayFile(Bin: PAnsiChar; BinBytes: PtrInt): TFileName;

/// append one byte as hexadecimal char pairs, into a text buffer
function ByteToHex(P: PAnsiChar; Value: byte): PAnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
function PointerToHex(aPointer: Pointer): RawUtf8; overload;

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
procedure PointerToHex(aPointer: Pointer; var result: RawUtf8); overload;

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - such result type would avoid a string allocation on heap
function PointerToHexShort(aPointer: Pointer): TShort16; overload;

/// fast conversion from a cardinal value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToCardinal()
function CardinalToHex(aCardinal: cardinal): RawUtf8;

/// fast conversion from a cardinal value into hexa chars, ready to be displayed
// - use internally BinToHexDisplayLower()
// - reverse function of HexDisplayToCardinal()
function CardinalToHexLower(aCardinal: cardinal): RawUtf8;

/// fast conversion from a cardinal value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - such result type would avoid a string allocation on heap
function CardinalToHexShort(aCardinal: cardinal): TShort16;

/// compute the hexadecimal representation of the crc32 checkum of a given text
// - wrapper around CardinalToHex(crc32c(...))
function crc32cUtf8ToHex(const str: RawUtf8): RawUtf8;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToInt64()
function Int64ToHex(aInt64: Int64): RawUtf8; overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToInt64()
procedure Int64ToHex(aInt64: Int64; var result: RawUtf8); overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - such result type would avoid a string allocation on heap
procedure Int64ToHexShort(aInt64: Int64; out result: TShort16); overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - such result type would avoid a string allocation on heap
function Int64ToHexShort(aInt64: Int64): TShort16; overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToInt64()
function Int64ToHexString(aInt64: Int64): string;

/// fast conversion from hexa chars in reverse order into a binary buffer
function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;

/// fast conversion from hexa chars in reverse order into a cardinal
// - reverse function of CardinalToHex()
// - returns false and set aValue=0 if Hex is not a valid hexadecimal 32-bit
// unsigned integer
// - returns true and set aValue with the decoded number, on success
function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
  {$ifndef FPC}{$ifdef HASINLINE}inline;{$endif}{$endif}
  // inline gives an error under release conditions with (old?) FPC

/// fast conversion from hexa chars in reverse order into a cardinal
// - reverse function of Int64ToHex()
// - returns false and set aValue=0 if Hex is not a valid hexadecimal 64-bit
// signed integer
// - returns true and set aValue with the decoded number, on success
function HexDisplayToInt64(Hex: PAnsiChar; out aValue: Int64): boolean; overload;
    {$ifndef FPC}{$ifdef HASINLINE}inline;{$endif}{$endif}
    { inline gives an error under release conditions with FPC }

/// fast conversion from hexa chars in reverse order into a cardinal
// - reverse function of Int64ToHex()
// - returns 0 if the supplied text buffer is not a valid hexadecimal 64-bit
// signed integer
function HexDisplayToInt64(const Hex: RawByteString): Int64; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion from octal C-like escape into binary data
// - \xxx is converted into a single xxx byte from octal, and \\ into \
// - will stop the conversion when Oct^=#0 or when invalid \xxx is reached
// - returns the number of bytes written to Bin^
function OctToBin(Oct: PAnsiChar; Bin: PByte): PtrInt; overload;

/// conversion from octal C-like escape into binary data
// - \xxx is converted into a single xxx byte from octal, and \\ into \
function OctToBin(const Oct: RawUtf8): RawByteString; overload;

/// revert the value as encoded by TBaseWriter.AddInt18ToChars3() or Int18ToChars3()
// - no range check is performed: you should ensure that the incoming text
// follows the expected 3-chars layout
function Chars3ToInt18(P: pointer): cardinal;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the value as encoded by TBaseWriter.AddInt18ToChars3() method
function Int18ToChars3(Value: cardinal): RawUtf8; overload;

/// compute the value as encoded by TBaseWriter.AddInt18ToChars3() method
procedure Int18ToChars3(Value: cardinal; var result: RawUtf8); overload;

/// creates a 3 digits string from a 0..999 value as '000'..'999'
// - consider using UInt3DigitsToShort() to avoid temporary memory allocation,
// e.g. when used as FormatUtf8() parameter
function UInt3DigitsToUtf8(Value: cardinal): RawUtf8;

/// creates a 4 digits string from a 0..9999 value as '0000'..'9999'
// - consider using UInt4DigitsToShort() to avoid temporary memory allocation,
// e.g. when used as FormatUtf8() parameter
function UInt4DigitsToUtf8(Value: cardinal): RawUtf8;

  /// creates a 4 digits short string from a 0..9999 value
// - using TShort4 as returned string would avoid a string allocation on heap
// - could be used e.g. as parameter to FormatUtf8()
function UInt4DigitsToShort(Value: cardinal): TShort4;

/// creates a 3 digits short string from a 0..999 value
// - using TShort4 as returned string would avoid a string allocation on heap
// - could be used e.g. as parameter to FormatUtf8()
function UInt3DigitsToShort(Value: cardinal): TShort4;

/// creates a 2 digits short string from a 0..99 value
// - using TShort4 as returned string would avoid a string allocation on heap
// - could be used e.g. as parameter to FormatUtf8()
function UInt2DigitsToShort(Value: byte): TShort4;
  {$ifdef HASINLINE}inline;{$endif}

/// creates a 2 digits short string from a 0..99 value
// - won't test Value>99 as UInt2DigitsToShort()
function UInt2DigitsToShortFast(Value: byte): TShort4;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit integer (storing a IP4 address) into its full notation
// - returns e.g. '1.2.3.4' for any valid address, or '' if ip4=0
function IP4Text(ip4: cardinal): shortstring; overload;

/// convert a 128-bit buffer (storing an IP6 address) into its full notation
// - returns e.g. '2001:0db8:0a0b:12f0:0000:0000:0000:0001'
function IP6Text(ip6: PHash128): shortstring; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 128-bit buffer (storing an IP6 address) into its full notation
// - returns e.g. '2001:0db8:0a0b:12f0:0000:0000:0000:0001'
procedure IP6Text(ip6: PHash128; result: PShortString); overload;

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(aIP: PUtf8Char; out aValue: cardinal): boolean; overload;

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(const aIP: RawUtf8; out aValue: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an IPv4 'x.x.x.x' text into its 32-bit value, 0 or localhost
// - returns <> 0 value if the text was a valid IPv4 text, 0 on parsing error
// - '' or '127.0.0.1' will also return 0
function IPToCardinal(const aIP: RawUtf8): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// append a TGUID binary content as text
// - will store e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without any {})
// - this will be the format used for JSON encoding, e.g.
// $ { "UID": "C9A646D3-9C61-4CB7-BFCD-EE2522C8F633" }
function GuidToText(P: PUtf8Char; guid: PByteArray): PUtf8Char;

/// convert a TGUID into UTF-8 encoded { text }
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - if you do not need the embracing { }, use ToUtf8() overloaded function
function GuidToRawUtf8(const guid: TGUID): RawUtf8;

/// convert a TGUID into UTF-8 encoded text
// - will return e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without the {})
// - if you need the embracing { }, use GuidToRawUtf8() function instead
function ToUtf8(const guid: TGUID): RawUtf8; overload;

/// convert a TGUID into text
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - this version is faster than the one supplied by SysUtils
function GuidToString(const guid: TGUID): string;

type
  /// stack-allocated ASCII string, used by GuidToShort() function
  TGuidShortString = string[38];
  PGuidShortString = ^TGuidShortString;

/// convert a TGUID into text
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - using a shortstring will allow fast allocation on the stack, so is
// preferred e.g. when providing a GUID to a ESynException.CreateUtf8()
function GuidToShort(const guid: TGUID): TGuidShortString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TGUID into text
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - using a shortstring will allow fast allocation on the stack, so is
// preferred e.g. when providing a GUID to a ESynException.CreateUtf8()
procedure GuidToShort(const
  guid: TGUID; out dest: TGuidShortString); overload;

/// convert some text into its TGUID binary value
// - expect e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without any {})
// - return nil if the supplied text buffer is not a valid TGUID
// - this will be the format used for JSON encoding, e.g.
// $ { "UID": "C9A646D3-9C61-4CB7-BFCD-EE2522C8F633" }
function TextToGuid(P: PUtf8Char; guid: PByteArray): PUtf8Char;

/// convert some VCL text into a TGUID
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - return {00000000-0000-0000-0000-000000000000} if the supplied text buffer
// is not a valid TGUID
function StringToGuid(const text: string): TGUID;

/// convert some UTF-8 encoded text into a TGUID
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - return {00000000-0000-0000-0000-000000000000} if the supplied text buffer
// is not a valid TGUID
function RawUtf8ToGuid(const text: RawByteString): TGUID;

/// read a TStream content into a String
// - it will read binary or text content from the current position until the
// end (using TStream.Size)
// - uses RawByteString for byte storage, whatever the codepage is
function StreamToRawByteString(aStream: TStream): RawByteString;

/// create a TStream from a string content
// - uses RawByteString for byte storage, whatever the codepage is
// - in fact, the returned TStream is a TRawByteString instance, since this
// function is just a wrapper around:
// ! result := TRawByteStringStream.Create(aString);
function RawByteStringToStream(const aString: RawByteString): TStream;
  {$ifdef HASINLINE}inline;{$endif}

/// read an UTF-8 text from a TStream
// - format is Length(integer):Text, i.e. the one used by WriteStringToStream
// - will return '' if there is no such text in the stream
// - you can set a MaxAllowedSize value, if you know how long the size should be
// - it will read from the current position in S: so if you just write into S,
// it could be a good idea to rewind it before call, e.g.:
// !  WriteStringToStream(Stream,aUtf8Text);
// !  Stream.Seek(0,soBeginning);
// !  str := ReadStringFromStream(Stream);
function ReadStringFromStream(S: TStream;
  MaxAllowedSize: integer = 255): RawUtf8;

/// write an UTF-8 text into a TStream
// - format is Length(integer):Text, i.e. the one used by ReadStringFromStream
function WriteStringToStream(S: TStream; const Text: RawUtf8): boolean;


implementation

{$ifdef FPC}
  // globally disable some FPC paranoid warnings - rely on x86_64 as reference
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable }
{$endif FPC}

 
{ ************ UTF-8 String Manipulation Functions }

function GetNextLine(source: PUtf8Char; out next: PUtf8Char; andtrim: boolean): RawUtf8;
var
  beg: PUtf8Char;
begin
  if source = nil then
  begin
    {$ifdef FPC}
    FastAssignNew(result);
    {$else}
    result := '';
    {$endif FPC}
    next := source;
    exit;
  end;
  if andtrim then // optional trim left
    while source^ in [#9, ' '] do
      inc(source);
  beg := source;
  repeat // just here to avoid a goto
    if source[0] > #13 then
      if source[1] > #13 then
        if source[2] > #13 then
          if source[3] > #13 then
          begin
            inc(source, 4); // fast process 4 chars per loop
            continue;
          end
          else
            inc(source, 3)
        else
          inc(source, 2)
      else
        inc(source);
    case source^ of
      #0:
        next := nil;
      #10:
        next := source + 1;
      #13:
        if source[1] = #10 then
          next := source + 2
        else
          next := source + 1;
    else
      begin
        inc(source);
        continue;
      end;
    end;
    if andtrim then // optional trim right
      while (source > beg) and
            (source[-1] in [#9, ' ']) do
        dec(source);
    FastSetString(result, beg, source - beg);
    exit;
  until false;
end;

function TrimLeft(const S: RawUtf8): RawUtf8;
var
  i, l: PtrInt;
begin
  l := Length(S);
  i := 1;
  while (i <= l) and
        (S[i] <= ' ') do
    Inc(i);
  result := Copy(S, i, Maxint);
end;

function TrimRight(const S: RawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  i := Length(S);
  while (i > 0) and
        (S[i] <= ' ') do
    Dec(i);
  FastSetString(result, pointer(S), i);
end;

procedure TrimLeftLines(var S: RawUtf8);
var
  P, D: PUtf8Char;
begin
  if S = '' then
    exit;
  P := UniqueRawUtf8(S);
  D := P; // in-place process
  repeat
    P := GotoNextNotSpace(P);
    while not (P^ in [#0, #10, #13]) do
    begin
      D^ := P^;
      inc(P);
      inc(D);
    end;
    if P^ = #0 then
      break;
    D^ := #10;
    inc(D);
  until false;
  if D = pointer(S) then
    S := ''
  else
    PStrLen(PtrUInt(S) - _STRLEN)^ := D - pointer(S); // no SetLength needed
end;

function SplitRight(const Str: RawUtf8; SepChar: AnsiChar; LeftStr: PRawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  for i := length(Str) downto 1 do
    if Str[i] = SepChar then
    begin
      result := copy(Str, i + 1, maxInt);
      if LeftStr <> nil then
        LeftStr^ := copy(Str, 1, i - 1);
      exit;
    end;
  result := Str;
  if LeftStr <> nil then
    LeftStr^ := '';
end;

function SplitRights(const Str, SepChar: RawUtf8): RawUtf8;
var
  i, j, sep: PtrInt;
  c: AnsiChar;
begin
  sep := length(SepChar);
  if sep > 0 then
    if sep = 1 then
      result := SplitRight(Str, SepChar[1])
    else
    begin
      for i := length(Str) downto 1 do
      begin
        c := Str[i];
        for j := 1 to sep do
          if c = SepChar[j] then
          begin
            result := copy(Str, i + 1, maxInt);
            exit;
          end;
      end;
    end;
  result := Str;
end;

function Split(const Str, SepStr: RawUtf8; var LeftStr, RightStr: RawUtf8;
  ToUpperCase: boolean): boolean;
var
  i: integer;
  tmp: RawUtf8; // may be called as Split(Str,SepStr,Str,RightStr)
begin
  {$ifdef FPC} // to use fast FPC SSE version
  if length(SepStr) = 1 then
    i := PosExChar(SepStr[1], Str)
  else
  {$endif FPC}
    i := PosEx(SepStr, Str);
  if i = 0 then
  begin
    LeftStr := Str;
    RightStr := '';
    result := false;
  end
  else
  begin
    tmp := copy(Str, 1, i - 1);
    RightStr := copy(Str, i + length(SepStr), maxInt);
    LeftStr := tmp;
    result := true;
  end;
  if ToUpperCase then
  begin
    UpperCaseSelf(LeftStr);
    UpperCaseSelf(RightStr);
  end;
end;

function Split(const Str, SepStr: RawUtf8; var LeftStr: RawUtf8;
  ToUpperCase: boolean): RawUtf8;
begin
  Split(Str, SepStr, LeftStr, result, ToUpperCase);
end;

function Split(const Str: RawUtf8; const SepStr: array of RawUtf8;
  const DestPtr: array of PRawUtf8): PtrInt;
var
  s, i, j: PtrInt;
begin
  j := 1;
  result := 0;
  s := 0;
  if high(SepStr) >= 0 then
    while result <= high(DestPtr) do
    begin
      i := PosEx(SepStr[s], Str, j);
      if i = 0 then
      begin
        if DestPtr[result] <> nil then
          DestPtr[result]^ := copy(Str, j, MaxInt);
        inc(result);
        break;
      end;
      if DestPtr[result] <> nil then
        DestPtr[result]^ := copy(Str, j, i - j);
      inc(result);
      if s < high(SepStr) then
        inc(s);
      j := i + 1;
    end;
  for i := result to high(DestPtr) do
    if DestPtr[i] <> nil then
      DestPtr[i]^ := '';
end;

function IsVoid(const text: RawUtf8): boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 1 to length(text) do
    if text[i] > ' ' then
      exit;
  result := true;
end;

function TrimControlChars(const text: RawUtf8;
  const controls: TSynAnsicharSet): RawUtf8;
var
  len, i, j, n: PtrInt;
  P: PAnsiChar;
begin
  len := length(text);
  for i := 1 to len do
    if text[i] in controls then
    begin
      n := i - 1;
      FastSetString(result, nil, len);
      P := pointer(result);
      if n > 0 then
        MoveFast(pointer(text)^, P^, n);
      for j := i + 1 to len do
        if not (text[j] in controls) then
        begin
          P[n] := text[j];
          inc(n);
        end;
      SetLength(result, n); // truncate
      exit;
    end;
  result := text; // no control char found
end;

procedure FillZero(var secret: RawByteString);
begin
  if secret<>'' then
    with PStrRec(Pointer(PtrInt(secret) - _STRRECSIZE))^ do
      if refCnt=1 then // avoid GPF if const
        FillCharFast(pointer(secret)^, length, 0);
end;

procedure FillZero(var secret: RawUtf8);
begin
  FillZero(RawByteString(secret));
end;

procedure FillZero(var secret: SpiUtf8);
begin
  FillZero(RawByteString(secret));
end;

function StringReplaceAllProcess(const S, OldPattern, NewPattern: RawUtf8;
  found: integer): RawUtf8;
var
  i, last, oldlen, newlen, sharedlen: PtrInt;
  posCount: integer;
  pos: TIntegerDynArray;
  src, dst: PAnsiChar;
begin
  oldlen := length(OldPattern);
  newlen := length(NewPattern);
  SetLength(pos, 64);
  pos[0] := found;
  posCount := 1;
  repeat
    found := PosEx(OldPattern, S, found + oldlen);
    if found = 0 then
      break;
    AddInteger(pos, posCount, found);
  until false;
  FastSetString(result, nil, Length(S) + (newlen - oldlen) * posCount);
  last := 1;
  src := pointer(S);
  dst := pointer(result);
  for i := 0 to posCount - 1 do
  begin
    sharedlen := pos[i] - last;
    MoveFast(src^, dst^, sharedlen);
    inc(src, sharedlen + oldlen);
    inc(dst, sharedlen);
    if newlen > 0 then
    begin
      MoveSmall(pointer(NewPattern), dst, newlen);
      inc(dst, newlen);
    end;
    last := pos[i] + oldlen;
  end;
  MoveFast(src^, dst^, length(S) - last + 1);
end;

function StringReplaceAll(const S, OldPattern, NewPattern: RawUtf8): RawUtf8;
var
  found: integer;
begin
  if (S = '') or
     (OldPattern = '') or
     (OldPattern = NewPattern) then
    result := S
  else
  begin
    {$ifdef FPC} // will use fast FPC SSE version
    if length(OldPattern) = 1 then
      found := IndexByte(pointer(S)^, PStrLen(PtrUInt(S) - _STRLEN)^,
        byte(OldPattern[1])) + 1
    else
    {$endif FPC}
      found := PosEx(OldPattern, S, 1); // our PosEx() is faster than RTL Pos()
    if found = 0 then
      result := S
    else
      result := StringReplaceAllProcess(S, OldPattern, NewPattern, found);
  end;
end;

function StringReplaceAll(const S: RawUtf8;
  const OldNewPatternPairs: array of RawUtf8): RawUtf8;
var
  n, i: PtrInt;
begin
  result := S;
  n := high(OldNewPatternPairs);
  if (n > 0) and
     (n and 1 = 1) then
    for i := 0 to n shr 1 do
      result := StringReplaceAll(result,
        OldNewPatternPairs[i * 2], OldNewPatternPairs[i * 2 + 1]);
end;

function StringReplaceChars(const Source: RawUtf8; OldChar, NewChar: AnsiChar): RawUtf8;
var
  i, j, n: PtrInt;
begin
  if (OldChar <> NewChar) and
     (Source <> '') then
  begin
    n := length(Source);
    for i := 0 to n - 1 do
      if PAnsiChar(pointer(Source))[i] = OldChar then
      begin
        FastSetString(result, PAnsiChar(pointer(Source)), n);
        for j := i to n - 1 do
          if PAnsiChar(pointer(result))[j] = OldChar then
            PAnsiChar(pointer(result))[j] := NewChar;
        exit;
      end;
  end;
  result := Source;
end;

function StringReplaceTabs(const Source, TabText: RawUtf8): RawUtf8;

  procedure Process(S, D, T: PAnsiChar; TLen: integer);
  begin
    repeat
      if S^ = #0 then
        break
      else if S^ <> #9 then
      begin
        D^ := S^;
        inc(D);
        inc(S);
      end
      else
      begin
        if TLen > 0 then
        begin
          MoveSmall(T, D, TLen);
          inc(D, TLen);
        end;
        inc(S);
      end;
    until false;
  end;

var
  L, i, n, ttl: PtrInt;
begin
  ttl := length(TabText);
  L := Length(Source);
  n := 0;
  if ttl <> 0 then
    for i := 1 to L do
      if Source[i] = #9 then
        inc(n);
  if n = 0 then
  begin
    result := Source;
    exit;
  end;
  FastSetString(result, nil, L + n * pred(ttl));
  Process(pointer(Source), pointer(result), pointer(TabText), ttl);
end;

function RawUtf8OfChar(Ch: AnsiChar; Count: integer): RawUtf8;
begin
  if Count <= 0 then
    FastAssignNew(result)
  else
  begin
    FastSetString(result, nil, Count);
    FillCharFast(pointer(result)^, Count, byte(Ch));
  end;
end;

function QuotedStr(const S: RawUtf8; Quote: AnsiChar): RawUtf8;
begin
  QuotedStr(pointer(S), length(S), Quote, result);
end;

procedure QuotedStr(const S: RawUtf8; Quote: AnsiChar; var result: RawUtf8);
var
  P: PUtf8Char;
  tmp: pointer; // will hold a RawUtf8 with no try..finally exception block
begin
  tmp := nil;
  P := pointer(S);
  if (P <> nil) and
     (P = pointer(result)) then
  begin
    RawUtf8(tmp) := S; // make private ref-counted copy for QuotedStr(U,'"',U)
    P := pointer(tmp);
  end;
  QuotedStr(P, length(S), Quote, result);
  if tmp <> nil then
    {$ifdef FPC}
    FastAssignNew(tmp);
    {$else}
    RawUtf8(tmp) := '';
    {$endif FPC}
end;

procedure QuotedStr(P: PUtf8Char; PLen: PtrInt; Quote: AnsiChar;
  var result: RawUtf8);
var
  i, quote1, nquote: PtrInt;
  R: PUtf8Char;
  c: AnsiChar;
begin
  nquote := 0;
  {$ifdef FPC}
  quote1 := IndexByte(P^, PLen, byte(Quote)); // to use fast FPC RTL SSE2 asm
  if quote1 >= 0 then
    for i := quote1 to PLen - 1 do
      if P[i] = Quote then
        inc(nquote);
  {$else}
  quote1 := 0;
  for i := 0 to PLen - 1 do
    if P[i] = Quote then
    begin
      if nquote = 0 then
        quote1 := i;
      inc(nquote);
    end;
  {$endif FPC}
  FastSetString(result, nil, PLen + nquote + 2);
  R := pointer(result);
  R^ := Quote;
  inc(R);
  if nquote = 0 then
  begin
    MoveFast(P^, R^, PLen);
    R[PLen] := Quote;
  end
  else
  begin
    MoveFast(P^, R^, quote1);
    inc(R, quote1);
    inc(PLen, PtrInt(PtrUInt(P))); // efficient use of registers on FPC
    inc(quote1, PtrInt(PtrUInt(P)));
    repeat
      if quote1 = PLen then
        break;
      c := PAnsiChar(quote1)^;
      inc(quote1);
      R^ := c;
      inc(R);
      if c <> Quote then
        continue;
      R^ := c;
      inc(R);
    until false;
    R^ := Quote;
  end;
end;

function GotoEndOfQuotedString(P: PUtf8Char): PUtf8Char;
var
  quote: AnsiChar;
begin
  // P^=" or P^=' at function call
  quote := P^;
  inc(P);
  repeat
    if P^ = #0 then
      break
    else if P^ <> quote then
      inc(P)
    else if P[1] = quote then // allow double quotes inside string
      inc(P, 2)
    else
      break; // end quote
  until false;
  result := P;
end; // P^='"' at function return

function GotoNextNotSpace(P: PUtf8Char): PUtf8Char;
begin
  {$ifdef FPC}
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  {$else}
  if P^ in [#1..' '] then
    repeat
      inc(P)
    until not (P^ in [#1..' ']);
  {$endif FPC}
  result := P;
end;

function GotoNextNotSpaceSameLine(P: PUtf8Char): PUtf8Char;
begin
  while P^ in [#9, ' '] do
    inc(P);
  result := P;
end;

function GotoNextSpace(P: PUtf8Char): PUtf8Char;
begin
  if P^ > ' ' then
    repeat
      inc(P)
    until P^ <= ' ';
  result := P;
end;

function NextNotSpaceCharIs(var P: PUtf8Char; ch: AnsiChar): boolean;
begin
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if P^ = ch then
  begin
    inc(P);
    result := true;
  end
  else
    result := false;
end;

function GotoNextSqlIdentifier(P: PUtf8Char; tab: PTextCharSet): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif}
begin
  while tcCtrlNot0Comma in tab[P^] do // in [#1..' ', ';']
    inc(P);
  if PWord(P)^ = ord('/') + ord('*') shl 8 then
  begin
    // detect and ignore e.g. '/*nocache*/'
    repeat
      inc(P);
      if PWord(P)^ = ord('*') + ord('/') shl 8 then
      begin
        inc(P, 2);
        break;
      end;
    until P^ = #0;
    while tcCtrlNot0Comma in tab[P^] do
      inc(P);
  end;
  result := P;
end;

function GetNextFieldProp(var P: PUtf8Char; var Prop: RawUtf8): boolean;
var
  B: PUtf8Char;
  tab: PTextCharSet;
begin
  tab := @TEXT_CHARS;
  P := GotoNextSqlIdentifier(P, tab);
  B := P;
  while tcIdentifier in tab[P^] do
    inc(P); // go to end of ['_', '0'..'9', 'a'..'z', 'A'..'Z'] chars
  FastSetString(Prop, B, P - B);
  P := GotoNextSqlIdentifier(P, tab);
  result := Prop <> '';
end;

function GetNextFieldPropSameLine(var P: PUtf8Char; var Prop: ShortString): boolean;
var
  B: PUtf8Char;
  tab: PTextCharSet;
begin
  tab := @TEXT_CHARS;
  while tcCtrlNotLF in tab[P^] do
    inc(P); // ignore [#1..#9, #11, #12, #14..' ']
  B := P;
  while tcIdentifier in tab[P^] do
    inc(P); // go to end of field name
  SetString(Prop, PAnsiChar(B), P - B);
  while tcCtrlNotLF in TEXT_CHARS[P^] do
    inc(P);
  result := Prop <> '';
end;

function UnQuoteSqlStringVar(P: PUtf8Char; out Value: RawUtf8): PUtf8Char;
var
  quote: AnsiChar;
  PBeg, PS: PUtf8Char;
  internalquote: PtrInt;
begin
  result := nil;
  if P = nil then
    exit;
  quote := P^; // " or '
  inc(P);
  // compute unquoted string length
  PBeg := P;
  internalquote := 0;
  P := PosChar(P, quote); // fast SSE2 search on x86_64
  if P = nil then
    exit; // we need at least an ending quote
  while true do
    if P^ = #0 then
      exit // where is my quote?
    else if P^ <> quote then
      inc(P)
    else if P[1] = quote then
    begin
      inc(P, 2); // allow double quotes inside string
      inc(internalquote);
    end
    else
      break; // end quote
  // create unquoted string
  if internalquote = 0 then
    // no quote within
    FastSetString(Value, PBeg, P - PBeg)
  else
  begin
    // unescape internal quotes
    SetLength(Value, P - PBeg - internalquote);
    P := PBeg;
    PS := Pointer(Value);
    repeat
      if P[0] = quote then
        if P[1] = quote then
          // allow double quotes inside string
          inc(P)
        else
          // end quote
          break;
      PS^ := P[0];
      inc(PS);
      inc(P);
    until false;
  end;
  result := P + 1;
end;

function UnQuoteSqlString(const Value: RawUtf8): RawUtf8;
begin
  UnQuoteSqlStringVar(pointer(Value), result);
end;

function UnQuotedSQLSymbolName(const ExternalDBSymbol: RawUtf8): RawUtf8;
begin
  if (ExternalDBSymbol <> '') and
     (ExternalDBSymbol[1] in ['[', '"', '''', '(']) then
    // e.g. for ZDBC's GetFields()
    result := copy(ExternalDBSymbol, 2, length(ExternalDBSymbol) - 2)
  else
    result := ExternalDBSymbol;
end;

function IdemPCharAndGetNextLine(var source: PUtf8Char; searchUp: PAnsiChar): boolean;
begin
  if source = nil then
    result := false
  else
  begin
    result := IdemPChar(source, searchUp);
    source := GotoNextLine(source);
  end;
end;

function FindNameValue(P: PUtf8Char; UpperName: PAnsiChar): PUtf8Char;
var
  table: PNormTable; // faster even on i386
  u: PAnsiChar;
label
  eof, eol;
begin
  if (P = nil) or
     (UpperName = nil) then
    goto eof;
  table := @NormToUpperAnsi7;
  repeat
    if table[P^] <> UpperName^ then // first character is likely not to match
      repeat // quickly go to end of current line
        repeat
eol:      if P^ <= #13 then
            break;
          inc(P);
        until false;
        if (P^ = #13) or
           (P^ = #10) then
        begin
          repeat
            inc(P);
          until (P^ <> #10) and
                (P^ <> #13);
          if P^ = #0 then
            goto eof;
          break; // handle next line
        end
        else if P^ <> #0 then
          continue; // e.g. #9
eof:    result := nil; // reached P^=#0 -> not found
        exit;
      until false
    else
    begin
      // first char did match -> try other chars
      inc(P);
      u := UpperName + 1;
      repeat
        if u^ = #0 then
          break
        else if u^ <> table[P^] then
          goto eol;
        inc(P);
        inc(u);
      until false;
      result := P; // if found, points just after UpperName
      exit;
    end;
  until false;
end;

function FindNameValue(const NameValuePairs: RawUtf8; UpperName: PAnsiChar;
  var Value: RawUtf8; KeepNotFoundValue: boolean; UpperNameSeparator: AnsiChar): boolean;
var
  P: PUtf8Char;
  L: PtrInt;
begin
  P := FindNameValue(pointer(NameValuePairs), UpperName);
  if P <> nil then
    repeat
      if UpperNameSeparator <> #0 then
        if P^ = UpperNameSeparator then
          inc(P) // e.g. THttpSocket.HeaderGetValue uses UpperNameSeparator=':'
        else
          break;
      while P^ in [#9, ' '] do // trim left
        inc(P);
      L := 0;
      while P[L] > #13 do      // end of line/value
        inc(L);
      while P[L - 1] = ' ' do  // trim right
        dec(L);
      FastSetString(Value, P, L);
      result := true;
      exit;
    until false;
  if not KeepNotFoundValue then
    {$ifdef FPC}
    FastAssignNew(Value);
    {$else}
    Value := '';
    {$endif FPC}
  result := false;
end;

function GetLineSize(P, PEnd: PUtf8Char): PtrUInt;
var
  c: byte;
begin
  {$ifdef CPUX64}
  if PEnd <> nil then
  begin
    result := BufferLineLength(P, PEnd); // use branchless SSE2 on x86_64
    exit;
  end;
  result := PtrUInt(P) - 1;
  {$else}
  result := PtrUInt(P) - 1;
  if PEnd <> nil then
    repeat // inlined BufferLineLength()
      inc(result);
      if PtrUInt(result) < PtrUInt(PEnd) then
      begin
        c := PByte(result)^;
        if (c > 13) or
           ((c <> 10) and
            (c <> 13)) then
          continue;
      end;
      break;
    until false
  else
  {$endif CPUX64}
    repeat // inlined BufferLineLength() ending at #0 for PEnd=nil
      inc(result);
      c := PByte(result)^;
      if (c > 13) or
         ((c <> 0) and (c <> 10) and (c <> 13)) then
        continue;
      break;
    until false;
  dec(result, PtrUInt(P)); // returns length
end;

function GetLineSizeSmallerThan(P, PEnd: PUtf8Char; aMinimalCount: integer): boolean;
begin
  result := false;
  if P <> nil then
    while (P < PEnd) and
          (P^ <> #10) and
          (P^ <> #13) do
      if aMinimalCount = 0 then
        exit
      else
      begin
        dec(aMinimalCount);
        inc(P);
      end;
  result := true;
end;

function GetNextStringLineToRawUnicode(var P: PChar): RawUnicode;
var
  S: PChar;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while S^ >= ' ' do
      inc(S);
    result := StringToRawUnicode(P, S - P);
    while (S^ <> #0) and
          (S^ < ' ') do
      inc(S); // ignore e.g. #13 or #10
    if S^ <> #0 then
      P := S
    else
      P := nil;
  end;
end;

function TrimLeftLowerCase(const V: RawUtf8): PUtf8Char;
begin
  result := Pointer(V);
  if result <> nil then
  begin
    while result^ in ['a'..'z'] do
      inc(result);
    if result^ = #0 then
      result := Pointer(V);
  end;
end;

function TrimLeftLowerCaseToShort(V: PShortString): ShortString;
begin
  TrimLeftLowerCaseToShort(V, result);
end;

procedure TrimLeftLowerCaseToShort(V: PShortString; out result: ShortString);
var
  P: PAnsiChar;
  L: integer;
begin
  L := length(V^);
  P := @V^[1];
  while (L > 0) and
        (P^ in ['a'..'z']) do
  begin
    inc(P);
    dec(L);
  end;
  if L = 0 then
    result := V^
  else
    SetString(result, P, L);
end;

function TrimLeftLowerCaseShort(V: PShortString): RawUtf8;
var
  P: PAnsiChar;
  L: integer;
begin
  L := length(V^);
  P := @V^[1];
  while (L > 0) and
        (P^ in ['a'..'z']) do
  begin
    inc(P);
    dec(L);
  end;
  if L = 0 then
    FastSetString(result, @V^[1], length(V^))
  else
    FastSetString(result, P, L);
end;

procedure AppendShortComma(text: PAnsiChar; len: PtrInt; var result: shortstring;
  trimlowercase: boolean);
begin
  if trimlowercase then
    while text^ in ['a'..'z'] do
      if len = 1 then
        exit
      else
      begin
        inc(text);
        dec(len);
      end;
  if integer(ord(result[0])) + len >= 255 then
    exit;
  if len > 0 then
    MoveSmall(text, @result[ord(result[0]) + 1], len);
  inc(result[0], len + 1);
  result[ord(result[0])] := ',';
end;

function IdemPropNameUSmallNotVoid(P1, P2, P1P2Len: PtrInt): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  inc(P1P2Len, P1);
  dec(P2, P1);
  repeat
    result := (PByte(P1)^ xor ord(PAnsiChar(P1)[P2])) and $df = 0;
    if not result then
      exit;
    inc(P1);
  until P1 >= P1P2Len;
end;

function FindShortStringListExact(List: PShortString; MaxValue: integer;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;
var
  PLen: PtrInt;
begin
  if aValueLen <> 0 then
    for result := 0 to MaxValue do
    begin
      PLen := PByte(List)^;
      if (PLen = aValueLen) and
         IdemPropNameUSmallNotVoid(PtrInt(@List^[1]), PtrInt(aValue), PLen) then
        exit;
      List := pointer(@PAnsiChar(PLen)[PtrUInt(List) + 1]); // next
    end;
  result := -1;
end;

function FindShortStringListTrimLowerCase(List: PShortString; MaxValue: integer;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;
var
  PLen: PtrInt;
begin
  if aValueLen <> 0 then
    for result := 0 to MaxValue do
    begin
      PLen := ord(List^[0]);
      inc(PUtf8Char(List));
      repeat // trim lower case
        if not (PUtf8Char(List)^ in ['a'..'z']) then
          break;
        inc(PUtf8Char(List));
        dec(PLen);
      until PLen = 0;
      if (PLen = aValueLen) and
         IdemPropNameUSmallNotVoid(PtrInt(aValue), PtrInt(List), PLen) then
        exit;
      inc(PUtf8Char(List), PLen); // next
    end;
  result := -1;
end;

function FindShortStringListTrimLowerCaseExact(List: PShortString; MaxValue: integer;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;
var
  PLen: PtrInt;
begin
  if aValueLen <> 0 then
    for result := 0 to MaxValue do
    begin
      PLen := ord(List^[0]);
      inc(PUtf8Char(List));
      repeat
        if not (PUtf8Char(List)^ in ['a'..'z']) then
          break;
        inc(PUtf8Char(List));
        dec(PLen);
      until PLen = 0;
      if (PLen = aValueLen) and
         CompareMemFixed(aValue, List, PLen) then
        exit;
      inc(PUtf8Char(List), PLen);
    end;
  result := -1;
end;

function UnCamelCase(const S: RawUtf8): RawUtf8;
var
  tmp: TSynTempBuffer;
  destlen: PtrInt;
begin
  if S = '' then
    result := ''
  else
  begin
    destlen := UnCamelCase(tmp.Init(length(S) * 2), pointer(S));
    tmp.Done(PAnsiChar(tmp.buf) + destlen, result);
  end;
end;

function UnCamelCase(D, P: PUtf8Char): integer;
var
  Space, SpaceBeg, DBeg: PUtf8Char;
  CapitalCount: integer;
  Number: boolean;
label
  Next;
begin
  DBeg := D;
  if (D <> nil) and
     (P <> nil) then
  begin
    // avoid GPF
    Space := D;
    SpaceBeg := D;
    repeat
      CapitalCount := 0;
      Number := P^ in ['0'..'9'];
      if Number then
        repeat
          inc(CapitalCount);
          D^ := P^;
          inc(P);
          inc(D);
        until not (P^ in ['0'..'9'])
      else
        repeat
          inc(CapitalCount);
          D^ := P^;
          inc(P);
          inc(D);
        until not (P^ in ['A'..'Z']);
      if P^ = #0 then
        break; // no lowercase conversion of last fully uppercased word
      if (CapitalCount > 1) and
         not Number then
      begin
        dec(P);
        dec(D);
      end;
      while P^ in ['a'..'z'] do
      begin
        D^ := P^;
        inc(D);
        inc(P);
      end;
      if P^ = '_' then
        if P[1] = '_' then
        begin
          D^ := ':';
          inc(P);
          inc(D);
          goto Next;
        end
        else
        begin
          PWord(D)^ := ord(' ') + ord('-') shl 8;
          inc(D, 2);
Next:     if Space = SpaceBeg then
            SpaceBeg := D + 1;
          inc(P);
          Space := D + 1;
        end
      else
        Space := D;
      if P^ = #0 then
        break;
      D^ := ' ';
      inc(D);
    until false;
    if Space > DBeg then
      dec(Space);
    while Space > SpaceBeg do
    begin
      if Space^ in ['A'..'Z'] then
        if not (Space[1] in ['A'..'Z', ' ']) then
          inc(Space^, 32); // lowercase conversion of not last fully uppercased word
      dec(Space);
    end;
  end;
  result := D - DBeg;
end;

procedure CamelCase(P: PAnsiChar; len: PtrInt; var s: RawUtf8; const isWord: TSynByteSet);
var
  i: PtrInt;
  d: PAnsiChar;
  tmp: array[byte] of AnsiChar;
begin
  if len > SizeOf(tmp) then
    len := SizeOf(tmp);
  for i := 0 to len - 1 do
    if not (ord(P[i]) in isWord) then
    begin
      if i > 0 then
      begin
        MoveSmall(P, @tmp, i);
        inc(P, i);
        dec(len, i);
      end;
      d := @tmp[i];
      while len > 0 do
      begin
        while (len > 0) and
              not (ord(P^) in isWord) do
        begin
          inc(P);
          dec(len);
        end;
        if len = 0 then
          break;
        d^ := NormToUpperAnsi7[P^];
        inc(d);
        repeat
          inc(P);
          dec(len);
          if not (ord(P^) in isWord) then
            break;
          d^ := P^;
          inc(d);
        until len = 0;
      end;
      P := @tmp;
      len := d - tmp;
      break;
    end;
  FastSetString(s, P, len);
end;

procedure CamelCase(const text: RawUtf8; var s: RawUtf8; const isWord: TSynByteSet);
begin
  CamelCase(pointer(text), length(text), s, isWord);
end;

procedure GetCaptionFromPCharLen(P: PUtf8Char; out result: string);
var
  Temp: array[byte] of AnsiChar;
begin
  if P = nil then
    exit;
  {$ifdef UNICODE}
  Utf8DecodeToUnicodeString(Temp, UnCamelCase(@Temp, P), result);
  {$else}
  SetString(result, PAnsiChar(@Temp), UnCamelCase(@Temp, P));
  {$endif UNICODE}
  if Assigned(LoadResStringTranslate) then
    LoadResStringTranslate(result);
end;


{ ************ CSV-like Iterations over Text Buffers }

function IdemPCharAndGetNextItem(var source: PUtf8Char; const searchUp: RawUtf8;
  var Item: RawUtf8; Sep: AnsiChar): boolean;
begin
  if source <> nil then
    if IdemPChar(source, Pointer(searchUp)) then
    begin
      inc(source, Length(searchUp));
      GetNextItem(source, Sep, Item);
      result := true;
      exit;
    end;
  result := false;
end;

function GetNextItem(var P: PUtf8Char; Sep: AnsiChar): RawUtf8;
begin
  GetNextItem(P, Sep, result);
end;

procedure GetNextItem(var P: PUtf8Char; Sep: AnsiChar; var result: RawUtf8);
var
  S: PUtf8Char;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while (S^ <> #0) and
          (S^ <> Sep) do
      inc(S);
    FastSetString(result, P, S - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

procedure GetNextItem(var P: PUtf8Char; Sep, Quote: AnsiChar; var result: RawUtf8);
begin
  if P = nil then
    result := ''
  else if P^ = Quote then
  begin
    P := UnQuoteSqlStringVar(P, result);
    if P = nil then
      result := ''
    else if P^ <> #0 then
      inc(P);
  end
  else
    GetNextItem(P, Sep, result);
end;

procedure GetNextItemTrimed(var P: PUtf8Char; Sep: AnsiChar; var result: RawUtf8);
var
  S, E: PUtf8Char;
begin
  if (P = nil) or
     (Sep <= ' ') then
    result := ''
  else
  begin
    while (P^ <= ' ') and
          (P^ <> #0) do
      inc(P); // trim left
    S := P;
    while (S^ <> #0) and
          (S^ <> Sep) do
      inc(S);
    E := S;
    while (E > P) and (E[-1] in [#1..' ']) do
      dec(E); // trim right
    FastSetString(result, P, E - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

procedure GetNextItemTrimedCRLF(var P: PUtf8Char; var result: RawUtf8);
var
  S, E: PUtf8Char;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while (S^ <> #0) and
          (S^ <> #10) do
      inc(S);
    E := S;
    if (E > P) and
       (E[-1] = #13) then
      dec(E);
    FastSetString(result, P, E - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function GetNextItemString(var P: PChar; Sep: Char): string;
var
  S: PChar;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while (S^ <> #0) and
          (S^ <> Sep) do
      inc(S);
    SetString(result, P, S - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function GetFileNameExtIndex(const FileName, CsvExt: TFileName): integer;
var
  Ext: TFileName;
  P: PChar;
begin
  result := -1;
  P := pointer(CsvExt);
  Ext := ExtractFileExt(FileName);
  if (P = nil) or
     (Ext = '') or
     (Ext[1] <> '.') then
    exit;
  delete(Ext, 1, 1);
  repeat
    inc(result);
    if SameText(GetNextItemString(P), Ext) then
      exit;
  until P = nil;
  result := -1;
end;

procedure AppendCsvValues(const Csv: string; const Values: array of string;
  var Result: string; const AppendBefore: string);
var
  s: string;
  i, bool: integer;
  P: PChar;
  first: boolean;
begin
  P := pointer(Csv);
  if P = nil then
    exit;
  first := True;
  for i := 0 to high(Values) do
  begin
    s := GetNextItemString(P);
    if Values[i] <> '' then
    begin
      if first then
      begin
        Result := Result + #13#10;
        first := false;
      end
      else
        Result := Result + AppendBefore;
      bool := FindCsvIndex('0,-1', RawUtf8(Values[i]));
      Result := Result + s + ': ';
      if bool < 0 then
        Result := Result + Values[i]
      else
        Result := Result + GetCsvItemString(pointer(GetNextItemString(P)), bool, '/');
    end;
  end;
end;

procedure GetNextItemShortString(var P: PUtf8Char; out Dest: ShortString; Sep: AnsiChar);
var
  S: PUtf8Char;
  len: PtrInt;
begin
  S := P;
  if S <> nil then
  begin
    while (S^ <= ' ') and (S^ <> #0) do
      inc(S);
    P := S;
    while (S^ <> Sep) and (S^ <> #0) do
      inc(S);
    len := S - P;
    repeat
      dec(len);
    until (len < 0) or
          not (P[len] in [#1..' ']); // trim right spaces
    if len >= 254 then
      len := 254 // leave space for trailing #0
    else
      inc(len);
    Dest[0] := AnsiChar(len);
    Dest[len + 1] := #0; // trailing #0
    if len > 0 then
      MoveSmall(P, @Dest[1], len);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end
  else
    PCardinal(@Dest)^ := 0; // trailing #0
end;

function GetNextItemHexDisplayToBin(var P: PUtf8Char;
  Bin: PByte; BinBytes: PtrInt; Sep: AnsiChar): boolean;
var
  S: PUtf8Char;
  len: integer;
begin
  result := false;
  FillCharFast(Bin^, BinBytes, 0);
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  S := P;
  if Sep = #0 then
    while S^ > ' ' do
      inc(S)
  else
    while (S^ <> #0) and (S^ <> Sep) do
      inc(S);
  len := S - P;
  while (P[len - 1] in [#1..' ']) and (len > 0) do
    dec(len); // trim right spaces
  if len <> BinBytes * 2 then
    exit;
  if not HexDisplayToBin(PAnsiChar(P), Bin, BinBytes) then
    FillCharFast(Bin^, BinBytes, 0)
  else
  begin
    if S^ = #0 then
      P := nil
    else if Sep <> #0 then
      P := S + 1
    else
      P := S;
    result := true;
  end;
end;

function GetNextItemCardinal(var P: PUtf8Char; Sep: AnsiChar): PtrUInt;
var
  c: PtrUInt;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  if P^ = ' ' then
    repeat
      inc(P)
    until P^ <> ' ';
  c := byte(P^) - 48;
  if c > 9 then
    result := 0
  else
  begin
    result := c;
    inc(P);
    repeat
      c := byte(P^) - 48;
      if c > 9 then
        break
      else
        result := result * 10 + c;
      inc(P);
    until false;
  end;
  if Sep <> #0 then
    while (P^ <> #0) and (P^ <> Sep) do
      inc(P); // go to end of CSV item (ignore any decimal)
  if P^ = #0 then
    P := nil
  else if Sep <> #0 then
    inc(P);
end;

function GetNextItemCardinalStrict(var P: PUtf8Char): PtrUInt;
var
  c: PtrUInt;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  c := byte(P^) - 48;
  if c > 9 then
    result := 0
  else
  begin
    result := c;
    inc(P);
    repeat
      c := byte(P^) - 48;
      if c > 9 then
        break
      else
        result := result * 10 + c;
      inc(P);
    until false;
  end;
  if P^ = #0 then
    P := nil;
end;

function CsvOfValue(const Value: RawUtf8; Count: cardinal; const Sep: RawUtf8): RawUtf8;
var
  ValueLen, SepLen: PtrUInt;
  i: cardinal;
  P: PAnsiChar;
begin
  // CsvOfValue('?',3)='?,?,?'
  result := '';
  if Count = 0 then
    exit;
  ValueLen := length(Value);
  SepLen := Length(Sep);
  FastSetString(result, nil, ValueLen * Count + SepLen * pred(Count));
  P := pointer(result);
  i := 1;
  repeat
    if ValueLen = 1 then
    begin
      P^ := Value[1]; // optimized for the Value='?' common case
      inc(P);
    end
    else
    begin
      MoveFast(Pointer(Value)^, P^, ValueLen);
      inc(P, ValueLen);
    end;
    if i = Count then
      break;
    if SepLen = 1 then
    begin
      P^ := Sep[1]; // optimized for the Sep=',' most common case
      inc(P);
      inc(i);
    end
    else if SepLen > 0 then
    begin
      MoveFast(Pointer(Sep)^, P^, SepLen);
      inc(P, SepLen);
      inc(i);
    end;
  until false;
  // assert(P-pointer(result)=length(result));
end;

procedure SetBitCsv(var Bits; BitsCount: integer; var P: PUtf8Char);
var
  bit, last: cardinal;
begin
  while P <> nil do
  begin
    bit := GetNextItemCardinalStrict(P) - 1; // '0' marks end of list
    if bit >= cardinal(BitsCount) then
      break; // avoid GPF
    if (P = nil) or
       (P^ = ',') then
      SetBitPtr(@Bits, bit)
    else if P^ = '-' then
    begin
      inc(P);
      last := GetNextItemCardinalStrict(P) - 1; // '0' marks end of list
      if last >= cardinal(BitsCount) then
        exit;
      while bit <= last do
      begin
        SetBitPtr(@Bits, bit);
        inc(bit);
      end;
    end;
    if (P <> nil) and
       (P^ = ',') then
      inc(P);
  end;
  if (P <> nil) and
     (P^ = ',') then
    inc(P);
end;

function GetBitCsv(const Bits; BitsCount: integer): RawUtf8;
var
  i, j: integer;
begin
  result := '';
  i := 0;
  while i < BitsCount do
    if GetBitPtr(@Bits, i) then
    begin
      j := i;
      while (j + 1 < BitsCount) and GetBitPtr(@Bits, j + 1) do
        inc(j);
      result := result + UInt32ToUtf8(i + 1);
      if j = i then
        result := result + ','
      else if j = i + 1 then
        result := result + ',' + UInt32ToUtf8(j + 1) + ','
      else
        result := result + '-' + UInt32ToUtf8(j + 1) + ',';
      i := j + 1;
    end
    else
      inc(i);
  result := result + '0'; // '0' marks end of list
end;

function GetNextItemCardinalW(var P: PWideChar; Sep: WideChar): PtrUInt;
var
  c: PtrUInt;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  c := word(P^) - 48;
  if c > 9 then
    result := 0
  else
  begin
    result := c;
    inc(P);
    repeat
      c := word(P^) - 48;
      if c > 9 then
        break
      else
        result := result * 10 + c;
      inc(P);
    until false;
  end;
  while (P^ <> #0) and (P^ <> Sep) do // go to end of CSV item (ignore any decimal)
    inc(P);
  if P^ = #0 then
    P := nil
  else
    inc(P);
end;

function GetNextItemInteger(var P: PUtf8Char; Sep: AnsiChar): PtrInt;
var
  minus: boolean;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  if P^ = ' ' then
    repeat
      inc(P)
    until P^ <> ' ';
  if P^ in ['+', '-'] then
  begin
    minus := P^ = '-';
    inc(P);
  end
  else
    minus := false;
  result := PtrInt(GetNextItemCardinal(P, Sep));
  if minus then
    result := -result;
end;

function GetNextTChar64(var P: PUtf8Char; Sep: AnsiChar; out Buf: TChar64): PtrInt;
var
  S: PUtf8Char;
  c: AnsiChar;
begin
  result := 0;
  S := P;
  if S = nil then
    exit;
  if Sep = #0 then
    repeat // store up to next whitespace
      c := S[result];
      if c <= ' ' then
        break;
      Buf[result] := c;
      inc(result);
      if result >= SizeOf(Buf) then
        exit; // avoid buffer overflow
    until false
  else
    repeat // store up to Sep or end of string
      c := S[result];
      if (c = #0) or
         (c = Sep) then
        break;
      Buf[result] := c;
      inc(result);
      if result >= SizeOf(Buf) then
        exit; // avoid buffer overflow
    until false;
  Buf[result] := #0; // make asciiz
  inc(S, result); // S[result]=Sep or #0
  if S^ = #0 then
    P := nil
  else if Sep = #0 then
    P := S
  else
    P := S + 1;
end;

{$ifdef CPU64}

function GetNextItemInt64(var P: PUtf8Char; Sep: AnsiChar): Int64;
begin
  result := GetNextItemInteger(P, Sep); // PtrInt=Int64
end;

function GetNextItemQWord(var P: PUtf8Char; Sep: AnsiChar): QWord;
begin
  result := GetNextItemCardinal(P, Sep); // PtrUInt=QWord
end;

{$else}

function GetNextItemInt64(var P: PUtf8Char; Sep: AnsiChar): Int64;
var
  tmp: TChar64;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
    SetInt64(tmp, result)
  else
    result := 0;
end;

function GetNextItemQWord(var P: PUtf8Char; Sep: AnsiChar): QWord;
var
  tmp: TChar64;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
    SetQWord(tmp, result)
  else
    result := 0;
end;

{$endif CPU64}

function GetNextItemHexa(var P: PUtf8Char; Sep: AnsiChar): QWord;
var
  tmp: TChar64;
  L: integer;
begin
  result := 0;
  L := GetNextTChar64(P, Sep, tmp);
  if (L > 0) and
     (L and 1 = 0) then
    if not HexDisplayToBin(@tmp, @result, L shr 1) then
      result := 0;
end;

function GetNextItemDouble(var P: PUtf8Char; Sep: AnsiChar): double;
var
  tmp: TChar64;
  err: integer;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
  begin
    result := GetExtended(tmp, err);
    if err <> 0 then
      result := 0;
  end
  else
    result := 0;
end;

function GetNextItemCurrency(var P: PUtf8Char; Sep: AnsiChar): currency;
begin
  GetNextItemCurrency(P, result, Sep);
end;

procedure GetNextItemCurrency(var P: PUtf8Char; out result: currency; Sep: AnsiChar);
var
  tmp: TChar64;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
    PInt64(@result)^ := StrToCurr64(tmp)
  else
    result := 0;
end;

function GetCsvItem(P: PUtf8Char; Index: PtrUInt; Sep: AnsiChar): RawUtf8;
var
  i: PtrUInt;
begin
  if P = nil then
    result := ''
  else
    for i := 0 to Index do
      GetNextItem(P, Sep, result);
end;

function GetUnQuoteCsvItem(P: PUtf8Char; Index: PtrUInt; Sep, Quote: AnsiChar): RawUtf8;
var
  i: PtrUInt;
begin
  if P = nil then
    result := ''
  else
    for i := 0 to Index do
      GetNextItem(P, Sep, Quote, result);
end;

function GetLastCsvItem(const Csv: RawUtf8; Sep: AnsiChar): RawUtf8;
var
  i: integer;
begin
  for i := length(Csv) downto 1 do
    if Csv[i] = Sep then
    begin
      result := copy(Csv, i + 1, maxInt);
      exit;
    end;
  result := Csv;
end;

function GetCsvItemString(P: PChar; Index: PtrUInt; Sep: Char): string;
var
  i: PtrUInt;
begin
  if P = nil then
    result := ''
  else
    for i := 0 to Index do
      result := GetNextItemString(P, Sep);
end;

function FindCsvIndex(Csv: PUtf8Char; const Value: RawUtf8; Sep: AnsiChar;
  CaseSensitive, TrimValue: boolean): integer;
var
  s: RawUtf8;
begin
  result := 0;
  while Csv <> nil do
  begin
    GetNextItem(Csv, Sep, s);
    if TrimValue then
      s := TrimU(s);
    if CaseSensitive then
    begin
      if s = Value then
        exit;
    end
    else if SameTextU(s, Value) then
      exit;
    inc(result);
  end;
  result := -1; // not found
end;

procedure CsvToRawUtf8DynArray(Csv: PUtf8Char; var List: TRawUtf8DynArray;
  Sep: AnsiChar; TrimItems, AddVoidItems: boolean);
var
  s: RawUtf8;
  n: integer;
begin
  n := length(List);
  while Csv <> nil do
  begin
    if TrimItems then
      GetNextItemTrimed(Csv, Sep, s)
    else
      GetNextItem(Csv, Sep, s);
    if (s <> '') or
       AddVoidItems then
      AddRawUtf8(List, n, s);
  end;
  if n <> length(List) then
    SetLength(List, n);
end;

procedure CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8;
  var List: TRawUtf8DynArray);
var
  offs, i, n: integer;
begin
  n := length(List);
  offs := 1;
  while offs <= length(Csv) do
  begin
    i := PosEx(Sep, Csv, offs);
    if i = 0 then
    begin
      i := PosEx(SepEnd, Csv, offs);
      if i = 0 then
        i := MaxInt
      else
        dec(i, offs);
      AddRawUtf8(List, n, Copy(Csv, offs, i));
      break;
    end;
    AddRawUtf8(List, n, Copy(Csv, offs, i - offs));
    offs := i + length(Sep);
  end;
  SetLength(List, n);
end;

function AddPrefixToCsv(Csv: PUtf8Char; const Prefix: RawUtf8; Sep: AnsiChar): RawUtf8;
var
  s: RawUtf8;
begin
  GetNextItem(Csv, Sep, result);
  if result = '' then
    exit;
  result := Prefix + result;
  while Csv <> nil do
  begin
    GetNextItem(Csv, Sep, s);
    if s <> '' then
      result := result + ',' + Prefix + s;
  end;
end;

procedure AddToCsv(const Value: RawUtf8; var Csv: RawUtf8; const Sep: RawUtf8);
begin
  if Csv = '' then
    Csv := Value
  else
    Csv := Csv + Sep + Value;
end;

function RenameInCsv(const OldValue, NewValue: RawUtf8; var Csv: RawUtf8;
  const Sep: RawUtf8): boolean;
var
  pattern: RawUtf8;
  i, j: integer;
begin
  result := OldValue = NewValue;
  i := length(OldValue);
  if result or
     (length(Sep) <> 1) or
     (length(Csv) < i) or
     (PosEx(Sep, OldValue) > 0) or
     (PosEx(Sep, NewValue) > 0) then
    exit;
  if CompareMem(pointer(OldValue), pointer(Csv), i) and // first (or unique) item
    ((Csv[i + 1] = Sep[1]) or
     (Csv[i + 1] = #0)) then
    i := 1
  else
  begin
    j := 1;
    pattern := Sep + OldValue;
    repeat
      i := PosEx(pattern, Csv, j);
      if i = 0 then
        exit;
      j := i + length(pattern);
    until (Csv[j] = Sep[1]) or
          (Csv[j] = #0);
    inc(i);
  end;
  delete(Csv, i, length(OldValue));
  insert(NewValue, Csv, i);
  result := true;
end;

function RawUtf8ArrayToCsv(const Values: array of RawUtf8; const Sep: RawUtf8;
  HighValues: integer): RawUtf8;
var
  i, len, seplen, L: integer;
  P: PAnsiChar;
begin
  result := '';
  if HighValues < 0 then
    HighValues := high(Values);
  if HighValues < 0 then
    exit;
  seplen := length(Sep);
  len := seplen * HighValues;
  for i := 0 to HighValues do
    inc(len, length(Values[i]));
  FastSetString(result, nil, len);
  P := pointer(result);
  i := 0;
  repeat
    L := length(Values[i]);
    if L > 0 then
    begin
      MoveFast(pointer(Values[i])^, P^, L);
      inc(P, L);
    end;
    if i = HighValues then
      Break;
    if seplen > 0 then
    begin
      MoveSmall(pointer(Sep), P, seplen);
      inc(P, seplen);
    end;
    inc(i);
  until false;
end;

function RawUtf8ArrayToQuotedCsv(const Values: array of RawUtf8;
  const Sep: RawUtf8; Quote: AnsiChar): RawUtf8;
var
  i: integer;
  tmp: TRawUtf8DynArray;
begin
  SetLength(tmp, length(Values));
  for i := 0 to High(Values) do
    QuotedStr(Values[i], Quote, tmp[i]);
  result := RawUtf8ArrayToCsv(tmp, Sep);
end;

procedure CsvToIntegerDynArray(Csv: PUtf8Char; var List: TIntegerDynArray;
  Sep: AnsiChar);
var
  n: integer;
begin
  n := length(List);
  while Csv <> nil do
    AddInteger(List, n, GetNextItemInteger(Csv, Sep));
  SetLength(List, n);
end;

procedure CsvToInt64DynArray(Csv: PUtf8Char; var List: TInt64DynArray;
  Sep: AnsiChar);
var
  n: integer;
begin
  n := length(List);
  while Csv <> nil do
    AddInt64(List, n, GetNextItemInt64(Csv, Sep));
  SetLength(List, n);
end;

function CsvToInt64DynArray(Csv: PUtf8Char; Sep: AnsiChar): TInt64DynArray;
var
  n: integer;
begin
  n := 0;
  while Csv <> nil do
    AddInt64(result, n, GetNextItemInt64(Csv, Sep));
  SetLength(result, n);
end;

function IntegerDynArrayToCsv(Values: PIntegerArray; ValuesCount: integer;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean): RawUtf8;
type
  TInts16 = packed array[word] of string[15]; // shortstring are faster (no heap allocation)
var
  i, L, Len: PtrInt;
  tmp: array[0..15] of AnsiChar;
  ints: ^TInts16;
  P: PAnsiChar;
  tmpbuf: TSynTempBuffer; // faster than a dynamic array
begin
  result := '';
  if ValuesCount = 0 then
    exit;
  if InlinedValue then
    Len := 4 * ValuesCount
  else
    Len := 0;
  tmpbuf.Init(ValuesCount * SizeOf({%H-}ints[0]) + Len);
  try
    ints := tmpbuf.buf;
     // compute whole result length at once
    dec(ValuesCount);
    inc(Len, length(Prefix) + length(Suffix));
    tmp[15] := ',';
    for i := 0 to ValuesCount do
    begin
      P := StrInt32(@tmp[15], Values[i]);
      L := @tmp[15] - P;
      if i < ValuesCount then
        inc(L); // append tmp[15]=','
      inc(Len, L);
      SetString(ints[i], P, L);
    end;
    // create result
    FastSetString(result, nil, Len);
    P := pointer(result);
    if Prefix <> '' then
    begin
      L := length(Prefix);
      MoveSmall(pointer(Prefix), P, L);
      inc(P, L);
    end;
    for i := 0 to ValuesCount do
    begin
      if InlinedValue then
      begin
        PWord(P)^ := ord(':') + ord('(') shl 8;
        inc(P, 2);
      end;
      L := ord(ints[i][0]);
      MoveSmall(@ints[i][1], P, L);
      inc(P, L);
      if InlinedValue then
      begin
        PWord(P)^ := ord(')') + ord(':') shl 8;
        inc(P, 2);
      end;
    end;
    if Suffix <> '' then
      MoveSmall(pointer(Suffix), P, length(Suffix));
  finally
    tmpbuf.Done;
  end;
end;

function Int64DynArrayToCsv(Values: PInt64Array; ValuesCount: integer;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean): RawUtf8;
type
  TInt = packed record
    Len: byte;
    Val: array[0..19] of AnsiChar; // Int64: 19 digits, then - sign
  end;
var
  i, L, Len: PtrInt;
  int: ^TInt;
  P: PAnsiChar;
  tmp: TSynTempBuffer; // faster than a dynamic array
begin
  result := '';
  if ValuesCount = 0 then
    exit;
  if InlinedValue then
    Len := 4 * ValuesCount
  else
    Len := 0;
  int := tmp.Init(ValuesCount * SizeOf(TInt) + Len);
  try
     // compute whole result length at once
    dec(ValuesCount);
    inc(Len, length(Prefix) + length(Suffix));
    for i := 0 to ValuesCount do
    begin
      P := StrInt64(PAnsiChar(int) + 21, Values[i]);
      L := PAnsiChar(int) + 21 - P;
      int^.Len := L;
      if i < ValuesCount then
        inc(L); // for ,
      inc(Len, L);
      inc(int);
    end;
    // create result
    FastSetString(result, nil, Len);
    P := pointer(result);
    if Prefix <> '' then
    begin
      L := length(Prefix);
      MoveSmall(pointer(Prefix), P, L);
      inc(P, L);
    end;
    int := tmp.buf;
    repeat
      if InlinedValue then
      begin
        PWord(P)^ := ord(':') + ord('(') shl 8;
        inc(P, 2);
      end;
      L := int^.Len;
      MoveSmall(PAnsiChar(int) + 21 - L, P, L);
      inc(P, L);
      if InlinedValue then
      begin
        PWord(P)^ := ord(')') + ord(':') shl 8;
        inc(P, 2);
      end;
      if ValuesCount = 0 then
        break;
      inc(int);
      P^ := ',';
      inc(P);
      dec(ValuesCount);
    until false;
    if Suffix <> '' then
      MoveSmall(pointer(Suffix), P, length(Suffix));
  finally
    tmp.Done;
  end;
end;

function IntegerDynArrayToCsv(const Values: TIntegerDynArray;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean): RawUtf8;
begin
  result := IntegerDynArrayToCsv(pointer(Values), length(Values),
    Prefix, Suffix, InlinedValue);
end;

function Int64DynArrayToCsv(const Values: TInt64DynArray;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean): RawUtf8;
begin
  result := Int64DynArrayToCsv(pointer(Values), length(Values),
    Prefix, Suffix, InlinedValue);
end;


{ ************ TBaseWriter parent class for Text Generation }

{ TBaseWriter }

constructor TBaseWriter.Create(aStream: TStream; aBufSize: integer);
begin
  SetStream(aStream);
  if aBufSize < 256 then
    aBufSize := 256;
  SetBuffer(nil, aBufSize);
end;

constructor TBaseWriter.Create(aStream: TStream; aBuf: pointer; aBufSize: integer);
begin
  SetStream(aStream);
  SetBuffer(aBuf, aBufSize);
end;

constructor TBaseWriter.CreateOwnedFileStream(
  const aFileName: TFileName; aBufSize: integer);
begin
  DeleteFile(aFileName);
  Create(TFileStream.Create(aFileName, fmCreate or fmShareDenyWrite), aBufSize);
  Include(fCustomOptions, twoStreamIsOwned);
end;

constructor TBaseWriter.CreateOwnedStream(aBuf: pointer; aBufSize: integer);
begin
  SetStream(TRawByteStringStream.Create);
  SetBuffer(aBuf, aBufSize);
  Include(fCustomOptions, twoStreamIsOwned);
end;

constructor TBaseWriter.CreateOwnedStream(aBufSize: integer);
begin
  Create(TRawByteStringStream.Create, aBufSize);
  Include(fCustomOptions, twoStreamIsOwned);
end;

constructor TBaseWriter.CreateOwnedStream(
  var aStackBuf: TTextWriterStackBuffer; aBufSize: integer);
begin
  if aBufSize > SizeOf(aStackBuf) then // too small -> allocate on heap
    CreateOwnedStream(aBufSize)
  else
  begin
    SetStream(TRawByteStringStream.Create);
    SetBuffer(@aStackBuf, SizeOf(aStackBuf));
    Include(fCustomOptions, twoStreamIsOwned);
  end;
end;

destructor TBaseWriter.Destroy;
begin
  if twoStreamIsOwned in fCustomOptions then
    fStream.Free;
  if not (twoBufferIsExternal in fCustomOptions) then
    FreeMem(fTempBuf);
  inherited;
end;

function TBaseWriter.PendingBytes: PtrUInt;
begin
  result := B - fTempBuf + 1;
end;

procedure TBaseWriter.Add(c: AnsiChar);
begin
  if B >= BEnd then
    FlushToStream; // may rewind B -> not worth any local PUtf8Char variable
  B[1] := c;
  inc(B);
end;

procedure TBaseWriter.AddComma;
begin
  if B >= BEnd then
    FlushToStream;
  B[1] := ',';
  inc(B);
end;

procedure TBaseWriter.Add(c1, c2: AnsiChar);
begin
  if B >= BEnd then
    FlushToStream;
  B[1] := c1;
  B[2] := c2;
  inc(B, 2);
end;

procedure TBaseWriter.Add(const Format: RawUtf8; const Values: array of const;
  Escape: TTextWriterKind; WriteObjectOptions: TTextWriterWriteObjectOptions);
var
  tmp: RawUtf8;
begin
  // basic implementation: see faster and more complete version in TTextWriter
  FormatUtf8(Format, Values, tmp);
  case Escape of
    twNone:
      AddString(tmp);
    twOnSameLine:
      AddOnSameLine(pointer(tmp)); // minimalistic version for TSynLog
    twJsonEscape:
      raise ESynException.CreateUtf8(
        '%.Add(twJsonEscape) unimplemented: use TTextWriter', [self]);
  end;
end;

procedure TBaseWriter.AddVariant(const Value: variant; Escape: TTextWriterKind;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  raise ESynException.CreateUtf8(
    '%.AddVariant unimplemented: use TTextWriter', [self]);
end;

procedure TBaseWriter.AddTypedJson(Value, TypeInfo: pointer;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  raise ESynException.CreateUtf8(
    '%.AddTypedJson unimplemented: use TTextWriter', [self]);
end;

function TBaseWriter.{%H-}AddJsonReformat(Json: PUtf8Char;
  Format: TTextWriterJsonFormat; EndOfObject: PUtf8Char): PUtf8Char;
begin
  raise ESynException.CreateUtf8(
    '%.AddJsonReformat unimplemented: use TTextWriter', [self]);
end;

procedure TBaseWriter.Add(P: PUtf8Char; Escape: TTextWriterKind);
begin
  raise ESynException.CreateUtf8(
    '%.Add(..,Escape: TTextWriterKind) unimplemented: use TTextWriter', [self]);
end;

procedure TBaseWriter.Add(P: PUtf8Char; Len: PtrInt; Escape: TTextWriterKind);
begin
  raise ESynException.CreateUtf8(
    '%.Add(..,Escape: TTextWriterKind) unimplemented: use TTextWriter', [self]);
end;

procedure TBaseWriter.WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean);
begin
  raise ESynException.CreateUtf8(
    '%.WrBase64() unimplemented: use TTextWriter', [self]);
end;

procedure TBaseWriter.AddShorter(const Text: TShort8);
var
  L: PtrInt;
begin
  L := ord(Text[0]);
  if L > 0 then
  begin
    {$ifdef DEBUG} assert(L <= 8); {$endif}
    if BEnd - B <= L then
      FlushToStream;
    PInt64(B + 1)^ := PInt64(@Text[1])^;
    inc(B, L);
  end;
end;

procedure TBaseWriter.AddNull;
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := NULL_LOW;
  inc(B, 4);
end;

procedure TBaseWriter.WriteObject(Value: TObject;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  raise ESynException.CreateUtf8(
    '%.WriteObject unimplemented: use TTextWriter', [self]);
end;

procedure TBaseWriter.AddObjArrayJson(const aObjArray;
  aOptions: TTextWriterWriteObjectOptions);
var
  i: PtrInt;
  a: TObjectDynArray absolute aObjArray;
begin
  Add('[');
  for i := 0 to length(a) - 1 do
  begin
    WriteObject(a[i], aOptions);
    AddComma;
  end;
  CancelLastComma;
  Add(']');
end;

procedure TBaseWriter.WriteToStream(data: pointer; len: PtrUInt);
begin
  if Assigned(fOnFlushToStream) then
    fOnFlushToStream(data, len);
  fStream.WriteBuffer(data^, len);
  inc(fTotalFileSize, len);
end;

function TBaseWriter.GetTextLength: PtrUInt;
begin
  if self = nil then
    result := 0
  else
    result := PtrUInt(B - fTempBuf + 1) + fTotalFileSize - fInitialStreamPosition;
end;

var
  DefaultTextWriterTrimEnum: boolean;
  
class procedure TBaseWriter.SetDefaultEnumTrim(aShouldTrimEnumsAsText: boolean);
begin
  DefaultTextWriterTrimEnum := aShouldTrimEnumsAsText;
end;

procedure TBaseWriter.SetBuffer(aBuf: pointer; aBufSize: integer);
begin
  if aBufSize <= 16 then
    raise ESynException.CreateUtf8('%.SetBuffer(size=%)', [self, aBufSize]);
  if aBuf = nil then
    GetMem(fTempBuf, aBufSize)
  else
  begin
    fTempBuf := aBuf;
    Include(fCustomOptions, twoBufferIsExternal);
  end;
  fTempBufSize := aBufSize;
  B := fTempBuf - 1; // Add() methods will append at B+1
  BEnd := fTempBuf + (fTempBufSize - 16); // -16 to avoid buffer overwrite/overread
  if DefaultTextWriterTrimEnum then
    Include(fCustomOptions, twoTrimLeftEnumSets);
end;

procedure TBaseWriter.SetStream(aStream: TStream);
begin
  if fStream <> nil then
    if twoStreamIsOwned in fCustomOptions then
    begin
      FreeAndNilSafe(fStream);
      Exclude(fCustomOptions, twoStreamIsOwned);
    end;
  if aStream <> nil then
  begin
    fStream := aStream;
    fInitialStreamPosition := fStream.Position;
    fTotalFileSize := fInitialStreamPosition;
  end;
end;

procedure TBaseWriter.FlushFinal;
var
  len: PtrInt;
begin // don't mess with twoFlushToStreamNoAutoResize: it may not be final
  len := B - fTempBuf + 1;
  if len > 0 then
    WriteToStream(fTempBuf, len);
  B := fTempBuf - 1;
end;

procedure TBaseWriter.FlushToStream;
var
  tmp, written: PtrUInt;
begin
  FlushFinal;
  if twoFlushToStreamNoAutoResize in fCustomOptions then
    exit;
  written := fTotalFileSize - fInitialStreamPosition;
  tmp := fTempBufSize;
  if (tmp < 49152) and
     (written > PtrUInt(tmp) * 4) then
    // tune small (stack-allocated?) buffer to grow by twice its size
    fTempBufSize := fTempBufSize * 2
  else if (written > 40 shl 20) and
          (tmp < 1 shl 20) then
    // total > 40MB -> grow internal buffer to 1MB
    fTempBufSize := 1 shl 20
  else
    // nothing to change about internal buffer size
    exit;
  if twoBufferIsExternal in fCustomOptions then
    // use heap, not stack from now on
    exclude(fCustomOptions, twoBufferIsExternal)
  else
    // from big content comes bigger buffer - but no need to realloc/move
    FreeMem(fTempBuf);
  GetMem(fTempBuf, fTempBufSize);
  BEnd := fTempBuf + (fTempBufSize - 16); // as in SetBuffer()
  B := fTempBuf - 1;
end;

procedure TBaseWriter.ForceContent(const text: RawUtf8);
begin
  CancelAll;
  if (fInitialStreamPosition = 0) and
     fStream.InheritsFrom(TRawByteStringStream) then
    TRawByteStringStream(fStream).DataString := text
  else
    fStream.WriteBuffer(pointer(text)^, length(text));
  fTotalFileSize := fInitialStreamPosition + PtrUInt(length(text));
end;

procedure TBaseWriter.SetText(out result: RawUtf8; reformat: TTextWriterJsonFormat);
var
  Len: cardinal;
  temp: TBaseWriter;
begin
  FlushFinal;
  Len := fTotalFileSize - fInitialStreamPosition;
  if Len = 0 then
    exit
  else if fStream.InheritsFrom(TRawByteStringStream) then
    TRawByteStringStream(fStream).GetAsText(fInitialStreamPosition, Len, result)
  else if fStream.InheritsFrom(TCustomMemoryStream) then
    with TCustomMemoryStream(fStream) do
      FastSetString(result, PAnsiChar(Memory) + fInitialStreamPosition, Len)
  else
  begin
    FastSetString(result, nil, Len);
    fStream.Seek(fInitialStreamPosition, soBeginning);
    fStream.Read(pointer(result)^, Len);
  end;
  if reformat <> jsonCompact then
  begin
    // reformat using the very same temp buffer but not the same RawUtf8
    temp := DefaultTextWriterSerializer.CreateOwnedStream(fTempBuf, fTempBufSize);
    try
      temp.AddJsonReformat(pointer(result), reformat, nil);
      temp.SetText(result);
    finally
      temp.Free;
    end;
  end;
end;

function TBaseWriter.Text: RawUtf8;
begin
  SetText(result);
end;

procedure TBaseWriter.CancelAll;
begin
  if self = nil then
    exit; // avoid GPF
  if fTotalFileSize <> 0 then
    fTotalFileSize := fStream.Seek(fInitialStreamPosition, soBeginning);
  B := fTempBuf - 1;
end;

procedure TBaseWriter.CancelLastChar(aCharToCancel: AnsiChar);
var
  P: PUtf8Char;
begin
  P := B;
  if (P >= fTempBuf) and
     (P^ = aCharToCancel) then
    dec(B);
end;

procedure TBaseWriter.CancelLastChar;
begin
  if B >= fTempBuf then // Add() methods append at B+1
    dec(B);
end;

procedure TBaseWriter.CancelLastComma;
var
  P: PUtf8Char;
begin
  P := B;
  if (P >= fTempBuf) and
     (P^ = ',') then
    dec(B);
end;

function TBaseWriter.LastChar: AnsiChar;
begin
  if B >= fTempBuf then
    result := B^
  else
    result := #0;
end;

procedure TBaseWriter.AddOnce(c: AnsiChar);
begin
  if (B >= fTempBuf) and
     (B^ = c) then
    exit; // no duplicate
  if B >= BEnd then
    FlushToStream;
  B[1] := c;
  inc(B);
end;

procedure TBaseWriter.Add(Value: PtrInt);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 23 then
    FlushToStream;
  if PtrUInt(Value) <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  else
  begin
    P := StrInt32(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveSmall(P, B + 1, Len);
  inc(B, Len);
end;

{$ifdef CPU32} // Add(Value: PtrInt) already implemented it for CPU64
procedure TBaseWriter.Add(Value: Int64);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: integer;
begin
  if BEnd - B <= 24 then
    FlushToStream;
  if Value < 0 then
  begin
    P := StrUInt64(@tmp[23], -Value) - 1;
    P^ := '-';
    Len := @tmp[23] - P;
  end
  else if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  else
  begin
    P := StrUInt64(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveSmall(P, B + 1, Len);
  inc(B, Len);
end;
{$endif CPU32}

procedure TBaseWriter.AddCurr64(Value: PInt64);
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 31 then
    FlushToStream;
  P := StrCurr64(@tmp[31], Value^);
  Len := @tmp[31] - P;
  if Len > 4 then
    if P[Len - 1] = '0' then
      if P[Len - 2] = '0' then
        if P[Len - 3] = '0' then
          if P[Len - 4] = '0' then
            dec(Len, 5) // 'xxx.0000' -> 'xxx'
          else
            dec(Len, 3) // 'xxx.1000' -> 'xxx.1'
        else
          dec(Len, 2) // 'xxx.1200' -> 'xxx.12'
      else
        dec(Len); // 'xxx.1220' -> 'xxx.123'
  MoveSmall(P, B + 1, Len);
  inc(B, Len);
end;

procedure TBaseWriter.AddCurr(const Value: currency);
begin
  AddCurr64(PInt64(@Value));
end;

procedure TBaseWriter.AddU(Value: cardinal);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 24 then
    FlushToStream;
  if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveSmall(P, B + 1, Len);
  inc(B, Len);
end;

procedure TBaseWriter.AddQ(Value: QWord);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 32 then
    FlushToStream;
  if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  else
  begin
    P := StrUInt64(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveSmall(P, B + 1, Len);
  inc(B, Len);
end;

procedure TBaseWriter.AddQHex(Value: Qword);
begin
  AddBinToHexDisplayLower(@Value, SizeOf(Value), '"');
end;

procedure TBaseWriter.Add(Value: Extended; precision: integer; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(ExtendedToJson(tmp, Value, precision, noexp)^);
end;

procedure TBaseWriter.AddDouble(Value: double; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(DoubleToJson(tmp, Value, noexp)^);
end;

procedure TBaseWriter.AddSingle(Value: single; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(ExtendedToJson(tmp, Value, SINGLE_PRECISION, noexp)^);
end;

procedure TBaseWriter.Add(Value: boolean);
var
  PS: PShortString;
begin
  if Value then // normalize: boolean may not be in the expected [0,1] range
    PS := @BOOL_STR[true]
  else
    PS := @BOOL_STR[false];
  AddShorter(PS^);
end;

procedure TBaseWriter.AddFloatStr(P: PUtf8Char);
begin
  if StrLen(P) > 127 then
    exit; // clearly invalid input
  if BEnd - B <= 127 then
    FlushToStream;
  inc(B);
  if P <> nil then
    B := FloatStrCopy(P, B) - 1
  else
    B^ := '0';
end;

procedure TBaseWriter.Add(Value: PGUID; QuotedChar: AnsiChar);
begin
  if BEnd - B <= 38 then
    FlushToStream;
  inc(B);
  if QuotedChar <> #0 then
  begin
    B^ := QuotedChar;
    inc(B);
  end;
  GuidToText(B, pointer(Value));
  inc(B, 36);
  if QuotedChar <> #0 then
    B^ := QuotedChar
  else
    dec(B);
end;

procedure TBaseWriter.AddCR;
begin
  if B >= BEnd then
    FlushToStream;
  PWord(B + 1)^ := 13 + 10 shl 8; // CR + LF
  inc(B, 2);
end;

procedure TBaseWriter.AddCRAndIndent;
var
  ntabs: cardinal;
begin
  if B^ = #9 then
    // we just already added an indentation level - do it once
    exit;
  ntabs := fHumanReadableLevel;
  if ntabs >= cardinal(fTempBufSize) then
    ntabs := 0; // fHumanReadableLevel=-1 after the last level of a document
  if BEnd - B <= PtrInt(ntabs) then
    FlushToStream;
  PWord(B + 1)^ := 13 + 10 shl 8; // CR + LF
  if ntabs > 0 then
    FillCharFast(B[3], ntabs, 9); // #9=tab
  inc(B, ntabs + 2);
end;

procedure TBaseWriter.AddChars(aChar: AnsiChar; aCount: PtrInt);
var
  n: PtrInt;
begin
  repeat
    n := BEnd - B;
    if n <= aCount then
    begin
      FlushToStream;
      n := BEnd - B;
    end;
    if aCount < n then
      n := aCount;
    FillCharFast(B[1], n, ord(aChar));
    inc(B, n);
    dec(aCount, n);
  until aCount <= 0;
end;

procedure TBaseWriter.Add2(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 99 then
    PCardinal(B + 1)^ := $3030 + ord(',') shl 16
  else     // '00,' if overflow
    PCardinal(B + 1)^ := TwoDigitLookupW[Value] + ord(',') shl 16;
  inc(B, 3);
end;

procedure TBaseWriter.Add3(Value: cardinal);
var
  V: cardinal;
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 999 then
    PCardinal(B + 1)^ := $303030 // '000,' if overflow
  else
  begin
    V := Value div 10;
    PCardinal(B + 1)^ := TwoDigitLookupW[V] + (Value - V * 10 + 48) shl 16;
  end;
  inc(B, 4);
  B^ := ',';
end;

procedure TBaseWriter.Add4(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 9999 then
    PCardinal(B + 1)^ := $30303030 // '0000,' if overflow
  else
    YearToPChar(Value, B + 1);
  inc(B, 5);
  B^ := ',';
end;

function Value3Digits(V: cardinal; P: PUtf8Char; W: PWordArray): cardinal;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := V div 100;
  PWord(P + 1)^ := W[V - result * 100];
  V := result;
  result := result div 10;
  P^ := AnsiChar(V - result * 10 + 48);
end;

procedure TBaseWriter.AddMicroSec(MS: cardinal);
var
  W: PWordArray;
begin
  // in 00.000.000 TSynLog format
  if B >= BEnd then
    FlushToStream;
  B[3] := '.';
  B[7] := '.';
  inc(B);
  W := @TwoDigitLookupW;
  MS := Value3Digits(Value3Digits(MS, B + 7, W), B + 3, W);
  if MS > 99 then
    MS := $3939
  else
    MS := W[MS];
  PWord(B)^ := MS;
  inc(B, 9);
end;

procedure TBaseWriter.AddNoJsonEscape(P: Pointer);
begin
  AddNoJsonEscape(P, StrLen(PUtf8Char(P)));
end;

procedure TBaseWriter.AddNoJsonEscape(P: Pointer; Len: PtrInt);
var
  direct: PtrInt;
  lastcommainmem: boolean;
  D: PUtf8Char;
begin
  if (P <> nil) and
     (Len > 0) then
    if Len < fTempBufSize * 2 then
    begin
      repeat
        D := B + 1;
        direct := BEnd - D; // guess biggest size available in fTempBuf at once
        if direct > 0 then // 0..-15 may happen because Add up to BEnd + 16
        begin
          if Len < direct then
            direct := Len;
          // append UTF-8 bytes to fTempBuf
          if direct > 0 then
          begin
            MoveFast(P^, D^, direct);
            inc(B, direct);
          end;
          dec(Len, direct);
          if Len = 0 then
            break;
          inc(PByte(P), direct);
        end;
        FlushToStream;
      until false;
    end
    else
    begin
      FlushFinal; // no auto-resize if content is really huge
      lastcommainmem := PAnsiChar(P)[Len - 1]= ',';
      if lastcommainmem then
        dec(Len);
      WriteToStream(P, Len); // no need to transit huge content into fTempBuf
      if lastcommainmem then
        Add(','); // but we need the last comma to be cancelable
    end;
end;

procedure EngineAppendUtf8(W: TBaseWriter; Engine: TSynAnsiConvert;
  P: PAnsiChar; Len: PtrInt);
var
  tmp: TSynTempBuffer;
begin
  // explicit conversion using a temporary buffer on stack
  Len := Engine.AnsiBufferToUtf8(tmp.Init(Len * 3), P, Len) - PUtf8Char({%H-}tmp.buf);
  W.AddNoJsonEscape(tmp.buf, Len);
  tmp.Done;
end;

procedure TBaseWriter.AddNoJsonEscape(P: PAnsiChar; Len: PtrInt; CodePage: cardinal);
var
  B: PAnsiChar;
begin
  if Len > 0 then
    case CodePage of
      CP_UTF8, CP_RAWBYTESTRING, CP_RAWBLOB:
        AddNoJsonEscape(P, Len);
      CP_UTF16:
        AddNoJsonEscapeW(PWord(P), 0);
    else
      begin
        // first handle trailing 7-bit ASCII chars, by quad
        B := P;
        if Len >= 4 then
          repeat
            if PCardinal(P)^ and $80808080 <> 0 then
              break; // break on first non ASCII quad
            inc(P, 4);
            dec(Len, 4);
          until Len < 4;
        if (Len > 0) and
           (P^ <= #127) then
          repeat
            inc(P);
            dec(Len);
          until (Len = 0) or
                (P^ > #127);
        if P <> B then
          AddNoJsonEscape(B, P - B);
        if Len > 0 then
          // rely on explicit conversion for all remaining ASCII characters
          EngineAppendUtf8(self, TSynAnsiConvert.Engine(CodePage), P, Len);
      end;
    end;
end;

procedure TBaseWriter.AddNoJsonEscapeUtf8(const text: RawByteString);
begin
  AddNoJsonEscape(pointer(text), length(text));
end;

procedure TBaseWriter.AddRawJson(const json: RawJson);
begin
  if json = '' then
    AddNull
  else
    AddNoJsonEscape(pointer(json), length(json));
end;

procedure TBaseWriter.AddNoJsonEscapeString(const s: string);
begin
  if s <> '' then
    {$ifdef UNICODE}
    AddNoJsonEscapeW(pointer(s), 0);
    {$else}
    AddNoJsonEscape(pointer(s), length(s), CurrentAnsiConvert.CodePage);
    {$endif UNICODE}
end;

procedure TBaseWriter.AddNoJsonEscapeW(WideChar: PWord; WideCharCount: integer);
var
  PEnd: PtrUInt;
begin
  if WideChar = nil then
    exit;
  if WideCharCount = 0 then
    repeat
      if B >= BEnd then
        FlushToStream;
      if WideChar^ = 0 then
        break;
      if WideChar^ <= 127 then
      begin
        B[1] := AnsiChar(ord(WideChar^));
        inc(WideChar);
        inc(B);
      end
      else
        inc(B, Utf16CharToUtf8(B + 1, WideChar));
    until false
  else
  begin
    PEnd := PtrUInt(WideChar) + PtrUInt(WideCharCount) * SizeOf(WideChar^);
    repeat
      if B >= BEnd then
        FlushToStream;
      if WideChar^ = 0 then
        break;
      if WideChar^ <= 127 then
      begin
        B[1] := AnsiChar(ord(WideChar^));
        inc(WideChar);
        inc(B);
        if PtrUInt(WideChar) < PEnd then
          continue
        else
          break;
      end;
      inc(B, Utf16CharToUtf8(B + 1, WideChar));
      if PtrUInt(WideChar) < PEnd then
        continue
      else
        break;
    until false;
  end;
end;

procedure TBaseWriter.AddProp(PropName: PUtf8Char; PropNameLen: PtrInt);
begin
  if PropNameLen <= 0 then
    exit; // paranoid check
  if BEnd - B <= PropNameLen then
    FlushToStream;
  if twoForceJsonExtended in fCustomOptions then
  begin
    MoveSmall(PropName, B + 1, PropNameLen);
    inc(B, PropNameLen + 1);
    B^ := ':';
  end
  else
  begin
    B[1] := '"';
    MoveSmall(PropName, B + 2, PropNameLen);
    inc(B, PropNameLen + 2);
    PWord(B)^ := ord('"') + ord(':') shl 8;
    inc(B);
  end;
end;

procedure TBaseWriter.AddPropName(const PropName: ShortString);
begin
  AddProp(@PropName[1], ord(PropName[0]));
end;

procedure TBaseWriter.AddFieldName(const FieldName: RawUtf8);
begin
  AddProp(Pointer(FieldName), length(FieldName));
end;

procedure TBaseWriter.AddClassName(aClass: TClass);
begin
  if aClass <> nil then
    AddShort(ClassNameShort(aClass)^);
end;

procedure TBaseWriter.AddInstanceName(Instance: TObject; SepChar: AnsiChar);
begin
  Add('"');
  if Instance = nil then
    AddShorter('void')
  else
    AddShort(ClassNameShort(Instance)^);
  Add('(');
  AddPointer(PtrUInt(Instance));
  Add(')', '"');
  if SepChar <> #0 then
    Add(SepChar);
end;

procedure TBaseWriter.AddInstancePointer(Instance: TObject; SepChar: AnsiChar;
  IncludeUnitName, IncludePointer: boolean);
begin
  if IncludeUnitName and
     Assigned(ClassUnit) then
  begin
    AddShort(ClassUnit(PClass(Instance)^)^);
    Add('.');
  end;
  AddShort(PPShortString(PPAnsiChar(Instance)^ + vmtClassName)^^);
  if IncludePointer then
  begin
    Add('(');
    AddPointer(PtrUInt(Instance));
    Add(')');
  end;
  if SepChar<>#0 then
    Add(SepChar);
end;

procedure TBaseWriter.AddShort(const Text: ShortString);
var
  L: PtrInt;
begin
  L := ord(Text[0]);
  if L = 0 then
    exit;
  if BEnd - B <= L then
    FlushToStream;
  MoveFast(Text[1], B[1], L);
  inc(B, L);
end;

procedure TBaseWriter.AddLine(const Text: shortstring);
var
  L: PtrInt;
begin
  L := ord(Text[0]);
  if BEnd - B <= L then
    FlushToStream;
  inc(B);
  if L > 0 then
  begin
    MoveFast(Text[1], B^, L);
    inc(B, L);
  end;
  PWord(B)^ := 13 + 10 shl 8; // CR + LF
  inc(B);
end;

procedure TBaseWriter.AddOnSameLine(P: PUtf8Char);
var
  D: PUtf8Char;
  c: AnsiChar;
begin
  if P <> nil then
  begin
    D := B + 1;
    if P^ <> #0 then
      repeat
        if D >= BEnd then
        begin
          B := D - 1;
          FlushToStream;
          D := B + 1;
        end;
        c := P^;
        if c < ' ' then
          if c = #0 then
            break
          else
            c := ' ';
        D^ := c;
        inc(P);
        inc(D);
      until false;
    B := D - 1;
  end;
end;

procedure TBaseWriter.AddOnSameLine(P: PUtf8Char; Len: PtrInt);
var
  D: PUtf8Char;
  c: AnsiChar;
begin
  if (P <> nil) and
     (Len > 0) then
  begin
    D := B + 1;
    repeat
      if D >= BEnd then
      begin
        B := D - 1;
        FlushToStream;
        D := B + 1;
      end;
      c := P^;
      if c < ' ' then
        c := ' ';
      D^ := c;
      inc(D);
      inc(P);
      dec(Len);
    until Len = 0;
    B := D - 1;
  end;
end;

procedure TBaseWriter.AddOnSameLineW(P: PWord; Len: PtrInt);
var
  PEnd: PtrUInt;
  c: cardinal;
begin
  if P = nil then
    exit;
  if Len = 0 then
    PEnd := 0
  else
    PEnd := PtrUInt(P) + PtrUInt(Len) * SizeOf(WideChar);
  while (Len = 0) or
        (PtrUInt(P) < PEnd) do
  begin
    if B >= BEnd then
      FlushToStream;
    // escape chars, so that all content will stay on the same text line
    c := P^;
    case c of
      0:
        break;
      1..32:
        begin
          B[1] := ' ';
          inc(B);
          inc(P);
        end;
      33..127:
        begin
          B[1] := AnsiChar(c); // direct store 7-bit ASCII
          inc(B);
          inc(P);
        end;
    else // characters higher than #127 -> UTF-8 encode
      inc(B, Utf16CharToUtf8(B + 1, P));
    end;
  end;
end;

procedure TBaseWriter.AddTrimLeftLowerCase(Text: PShortString);
var
  P: PAnsiChar;
  L: integer;
begin
  L := length(Text^);
  P := @Text^[1];
  while (L > 0) and (P^ in ['a'..'z']) do
  begin
    inc(P);
    dec(L);
  end;
  if L = 0 then
    AddShort(Text^)
  else
    AddNoJsonEscape(P, L);
end;

procedure TBaseWriter.AddTrimSpaces(const Text: RawUtf8);
begin
  AddTrimSpaces(pointer(Text));
end;

procedure TBaseWriter.AddTrimSpaces(P: PUtf8Char);
var
  c: AnsiChar;
begin
  if P <> nil then
    repeat
      c := P^;
      inc(P);
      if c > ' ' then
        Add(c);
    until c = #0;
end;

procedure TBaseWriter.AddReplace(Text: PUtf8Char; Orig, Replaced: AnsiChar);
begin
  if Text <> nil then
    while Text^ <> #0 do
    begin
      if Text^ = Orig then
        Add(Replaced)
      else
        Add(Text^);
      inc(Text);
    end;
end;

procedure TBaseWriter.AddByteToHex(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  PWord(B + 1)^ := TwoDigitsHexWB[Value];
  inc(B, 2);
end;

procedure TBaseWriter.AddInt18ToChars3(Value: cardinal);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := ((Value shr 12) and $3f) or
                       ((Value shr 6) and $3f) shl 8 or
                       (Value and $3f) shl 16 + $202020;
  inc(B, 3);
end;

procedure TBaseWriter.AddString(const Text: RawUtf8);
var
  L: PtrInt;
begin
  L := PtrInt(Text);
  if L = 0 then
    exit;
  L := PStrLen(L - _STRLEN)^;
  if L < fTempBufSize then
  begin
    if BEnd - B <= L then
      FlushToStream;
    MoveFast(pointer(Text)^, B[1], L);
    inc(B, L);
  end
  else
    AddNoJsonEscape(pointer(Text), L);
end;

procedure TBaseWriter.AddStringCopy(const Text: RawUtf8; start, len: PtrInt);
var
  L: PtrInt;
begin
  L := PtrInt(Text);
  if (len <= 0) or
     (L = 0) then
    exit;
  if start < 0 then
    start := 0
  else
    dec(start);
  L := PStrLen(L - _STRLEN)^;
  dec(L, start);
  if L > 0 then
  begin
    if len < L then
      L := len;
    AddNoJsonEscape(@PByteArray(Text)[start], L);
  end;
end;

procedure TBaseWriter.AddStrings(const Text: array of RawUtf8);
var
  i: PtrInt;
begin
  for i := 0 to high(Text) do
    AddString(Text[i]);
end;

procedure TBaseWriter.AddStrings(const Text: RawUtf8; count: PtrInt);
var
  i, L, siz: PtrInt;
begin
  L := length(Text);
  siz := L * count;
  if siz > 0 then
    if siz > fTempBufSize then
      for i := 1 to count do
        AddString(Text) // would overfill our buffer -> manual append
    else
    begin
      if BEnd - B <= siz then
        FlushToStream;
      for i := 1 to count do
      begin
        MoveFast(pointer(Text)^, B[1], L); // direct in-memory append
        inc(B, L);
      end;
    end;
end;

procedure TBaseWriter.AddBinToHexDisplay(Bin: pointer; BinBytes: PtrInt);
begin
  if cardinal(BinBytes * 2 - 1) >= cardinal(fTempBufSize) then
    exit;
  if BEnd - B <= BinBytes * 2 then
    FlushToStream;
  BinToHexDisplay(Bin, PAnsiChar(B + 1), BinBytes);
  inc(B, BinBytes * 2);
end;

procedure TBaseWriter.AddBinToHexDisplayLower(Bin: pointer; BinBytes: PtrInt;
  QuotedChar: AnsiChar);
begin
  if cardinal(BinBytes * 2 + 1) >= cardinal(fTempBufSize) then
    exit;
  if BEnd - B <= BinBytes * 2 then
    FlushToStream;
  inc(B);
  if QuotedChar <> #0 then
  begin
    B^ := QuotedChar;
    inc(B);
  end;
  BinToHexDisplayLower(Bin, pointer(B), BinBytes);
  inc(B, BinBytes * 2);
  if QuotedChar <> #0 then
    B^ := QuotedChar
  else
    dec(B);
end;

procedure TBaseWriter.AddBinToHexDisplayQuoted(Bin: pointer; BinBytes: PtrInt);
begin
  AddBinToHexDisplayLower(Bin, BinBytes, '"');
end;

function DisplayMinChars(Bin: PByteArray; BinBytes: PtrInt): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := BinBytes;
  repeat // append hexa chars up to the last non zero byte
    dec(result);
  until (result = 0) or
        (Bin[result] <> 0);
  inc(result);
end;

procedure TBaseWriter.AddBinToHexDisplayMinChars(Bin: pointer; BinBytes: PtrInt;
  QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(Bin, DisplayMinChars(Bin, BinBytes), QuotedChar);
end;

procedure TBaseWriter.AddPointer(P: PtrUInt; QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(@P, DisplayMinChars(@P, SizeOf(P)), QuotedChar);
end;

procedure TBaseWriter.AddBinToHex(Bin: Pointer; BinBytes: PtrInt);
var
  chunk: PtrInt;
begin
  if BinBytes <= 0 then
    exit;
  if B >= BEnd then
    FlushToStream;
  inc(B);
  repeat
    // guess biggest size to be added into buf^ at once
    chunk := (BEnd - B) shr 1; // div 2 -> two hexa chars per byte
    if BinBytes < chunk then
      chunk := BinBytes;
    // add hexa characters
    mormot.core.text.BinToHex(PAnsiChar(Bin), PAnsiChar(B), chunk);
    inc(B, chunk * 2);
    inc(PByte(Bin), chunk);
    dec(BinBytes, chunk);
    if BinBytes = 0 then
      break;
    // FlushToStream writes B-fTempBuf+1 -> special one below:
    WriteToStream(fTempBuf, B - fTempBuf);
    B := fTempBuf;
  until false;
  dec(B); // allow CancelLastChar
end;

procedure TBaseWriter.AddQuotedStr(Text: PUtf8Char; TextLen: PtrUInt;
  Quote: AnsiChar; TextMaxLen: PtrInt);
var
  Q: PUtf8Char;
begin
  Add(Quote);
  if (TextMaxLen > 5) and
     (TextLen > PtrUInt(TextMaxLen)) then
    TextLen := TextMaxLen - 5
  else
    TextMaxLen := 0;
  inc(TextLen, PtrUInt(Text)); // PUtf8Char(TextLen)=TextEnd
  if Text <> nil then
  begin
    repeat
      Q := PosChar(Text, Quote); // fast SSE2 asm on x86_64
      if Q = nil then
      begin
        AddNoJsonEscape(Text, PUtf8Char(TextLen) - Text); // no double quote
        break;
      end;
      inc(Q); // include first Quote
      AddNoJsonEscape(Text, Q - Text);
      Add(Quote); // double Quote
      Text := Q;  // continue
    until false;
    if TextMaxLen <> 0 then
      AddShorter('...');
  end;
  Add(Quote);
end;

var
  HTML_ESC: array[hfAnyWhere..hfWithinAttributes] of TAnsiCharToByte;

procedure TBaseWriter.AddHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat);
var
  B: PUtf8Char;
  esc: ^TAnsiCharToByte;
begin
  if Text = nil then
    exit;
  if Fmt = hfNone then
  begin
    AddNoJsonEscape(Text);
    exit;
  end;
  esc := @HTML_ESC[Fmt];
  repeat
    B := Text;
    while esc[Text^] = 0 do
      inc(Text);
    AddNoJsonEscape(B, Text - B);
    case Text^ of
      #0:
        exit;
      '<':
        AddShorter('&lt;');
      '>':
        AddShorter('&gt;');
      '&':
        AddShorter('&amp;');
      '"':
        AddShorter('&quot;');
    end;
    inc(Text);
  until Text^ = #0;
end;

function HtmlEscape(const text: RawUtf8; fmt: TTextWriterHtmlFormat): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TBaseWriter;
begin
  W := TBaseWriter.CreateOwnedStream(temp);
  try
    W.AddHtmlEscape(pointer(text), length(text), fmt);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlEscapeString(const text: string; fmt: TTextWriterHtmlFormat): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TBaseWriter;
begin
  W := TBaseWriter.CreateOwnedStream(temp);
  try
    W.AddHtmlEscapeString(text, fmt);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TBaseWriter.AddHtmlEscape(Text: PUtf8Char; TextLen: PtrInt;
  Fmt: TTextWriterHtmlFormat);
var
  B: PUtf8Char;
  esc: ^TAnsiCharToByte;
begin
  if (Text = nil) or
     (TextLen <= 0) then
    exit;
  if Fmt = hfNone then
  begin
    AddNoJsonEscape(Text, TextLen);
    exit;
  end;
  inc(TextLen, PtrInt(Text)); // TextLen = final PtrInt(Text)
  esc := @HTML_ESC[Fmt];
  repeat
    B := Text;
    while (PtrUInt(Text) < PtrUInt(TextLen)) and
          (esc[Text^] = 0) do
      inc(Text);
    AddNoJsonEscape(B, Text - B);
    if PtrUInt(Text) = PtrUInt(TextLen) then
      exit;
    case Text^ of
      #0:
        exit;
      '<':
        AddShorter('&lt;');
      '>':
        AddShorter('&gt;');
      '&':
        AddShorter('&amp;');
      '"':
        AddShorter('&quot;');
    end;
    inc(Text);
  until false;
end;

procedure TBaseWriter.AddHtmlEscapeString(const Text: string; Fmt: TTextWriterHtmlFormat);
var
  tmp: TSynTempBuffer;
  len: integer;
begin
  len := StringToUtf8(Text, tmp);
  AddHtmlEscape(tmp.buf, len, Fmt);
  tmp.Done;
end;

procedure TBaseWriter.AddHtmlEscapeUtf8(const Text: RawUtf8; Fmt: TTextWriterHtmlFormat);
begin
  AddHtmlEscape(pointer(Text), length(Text), Fmt);
end;

var
  XML_ESC: TAnsiCharToByte;

procedure TBaseWriter.AddXmlEscape(Text: PUtf8Char);
var
  i, beg: PtrInt;
  esc: ^TAnsiCharToByte;
begin
  if Text = nil then
    exit;
  esc := @XML_ESC;
  i := 0;
  repeat
    beg := i;
    if esc[Text[i]] = 0 then
    begin
      repeat // it is faster to handle all not-escaped chars at once
        inc(i);
      until esc[Text[i]] <> 0;
      AddNoJsonEscape(Text + beg, i - beg);
    end;
    repeat
      case Text[i] of
        #0:
          exit;
        #1..#8, #11, #12, #14..#31:
          ; // ignore invalid character - see http://www.w3.org/TR/xml/#NT-Char
        #9, #10, #13:
          begin
            // characters below ' ', #9 e.g. -> // '&#x09;'
            AddShorter('&#x');
            AddByteToHex(ord(Text[i]));
            Add(';');
          end;
        '<':
          AddShorter('&lt;');
        '>':
          AddShorter('&gt;');
        '&':
          AddShorter('&amp;');
        '"':
          AddShorter('&quot;');
        '''':
          AddShorter('&apos;');
      else
        break; // should match XML_ESC[] lookup table
      end;
      inc(i);
    until false;
  until false;
end;


{ TEchoWriter }

constructor TEchoWriter.Create(Owner: TBaseWriter);
begin
  fWriter := Owner;
  if Assigned(fWriter.OnFlushToStream) then
    raise ESynException.CreateUtf8('Unexpected %.Create', [self]);
  fWriter.OnFlushToStream := FlushToStream;
end;

destructor TEchoWriter.Destroy;
begin
  if (fWriter <> nil) and
     (TMethod(fWriter.OnFlushToStream).Data = self) then
    fWriter.OnFlushToStream := nil;
  inherited Destroy;
end;

procedure TEchoWriter.AddEndOfLine(aLevel: TSynLogInfo);
var
  i: PtrInt;
begin
  if twoEndOfLineCRLF in fWriter.CustomOptions then
    fWriter.AddCR
  else
    fWriter.Add(#10);
  if fEchos <> nil then
  begin
    fEchoStart := EchoFlush;
    for i := length(fEchos) - 1 downto 0 do // for MultiEventRemove() below
    try
      fEchos[i](fWriter, aLevel, fEchoBuf);
    except // remove callback in case of exception during echoing in user code
      MultiEventRemove(fEchos, i);
    end;
    fEchoBuf := '';
  end;
end;

procedure TEchoWriter.FlushToStream(Text: PUtf8Char; Len: PtrInt);
begin
  if fEchos <> nil then
  begin
    EchoFlush;
    fEchoStart := 0;
  end;
end;

procedure TEchoWriter.EchoAdd(const aEcho: TOnTextWriterEcho);
begin
  if self <> nil then
    if MultiEventAdd(fEchos, TMethod(aEcho)) then
      if fEchos <> nil then
        fEchoStart := fWriter.B - fWriter.fTempBuf + 1; // ignore any previous buffer
end;

procedure TEchoWriter.EchoRemove(const aEcho: TOnTextWriterEcho);
begin
  if self <> nil then
    MultiEventRemove(fEchos, TMethod(aEcho));
end;

function TEchoWriter.EchoFlush: PtrInt;
var
  L, LI: PtrInt;
  P: PUtf8Char;
begin
  P := fWriter.fTempBuf;
  result := fWriter.B - P + 1;
  L := result - fEchoStart;
  inc(P, fEchoStart);
  while (L > 0) and (P[L - 1] in [#10, #13]) do // trim right CR/LF chars
    dec(L);
  LI := length(fEchoBuf); // fast append to fEchoBuf
  SetLength(fEchoBuf, LI + L);
  MoveFast(P^, PByteArray(fEchoBuf)[LI], L);
end;

procedure TEchoWriter.EchoReset;
begin
  fEchoBuf := '';
end;

function TEchoWriter.GetEndOfLineCRLF: boolean;
begin
  result := twoEndOfLineCRLF in fWriter.CustomOptions;
end;

procedure TEchoWriter.SetEndOfLineCRLF(aEndOfLineCRLF: boolean);
begin
  if aEndOfLineCRLF then
    fWriter.CustomOptions := fWriter.CustomOptions + [twoEndOfLineCRLF]
  else
    fWriter.CustomOptions := fWriter.CustomOptions - [twoEndOfLineCRLF];
end;


function ObjectToJson(Value: TObject; Options: TTextWriterWriteObjectOptions): RawUtf8;
var
  temp: TTextWriterStackBuffer;
begin
  if Value = nil then
    result := NULL_STR_VAR
  else
    with DefaultTextWriterSerializer.CreateOwnedStream(temp) do
    try
      include(fCustomOptions, twoForceJsonStandard);
      WriteObject(Value, Options);
      SetText(result);
    finally
      Free;
    end;
end;


{ ************ TRawUtf8DynArray Processing Functions }

function IsZero(const Values: TRawUtf8DynArray): boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 0 to length(Values) - 1 do
    if Values[i] <> '' then
      exit;
  result := true;
end;

function TRawUtf8DynArrayFrom(const Values: array of RawUtf8): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  Finalize(result);
  SetLength(result, length(Values));
  for i := 0 to high(Values) do
    result[i] := Values[i];
end;

function FindRawUtf8(Values: PRawUtf8; const Value: RawUtf8; ValuesCount: integer;
  CaseSensitive: boolean): integer;
var
  ValueLen: TStrLen;
begin
  dec(ValuesCount);
  ValueLen := length(Value);
  if ValueLen = 0 then
    for result := 0 to ValuesCount do
      if Values^ = '' then
        exit
      else
        inc(Values)
  else if CaseSensitive then
    for result := 0 to ValuesCount do
      if (PtrUInt(Values^) <> 0) and
         (PStrLen(PtrUInt(Values^) - _STRLEN)^ = ValueLen) and
         CompareMemFixed(pointer(PtrInt(Values^)), pointer(Value), ValueLen) then
        exit
      else
        inc(Values)
  else
    for result := 0 to ValuesCount do
      if (PtrUInt(Values^) <> 0) and // StrIComp() won't change length
         (PStrLen(PtrUInt(Values^) - _STRLEN)^ = ValueLen) and
         (StrIComp(pointer(Values^), pointer(Value)) = 0) then
        exit
      else
        inc(Values);
  result := -1;
end;

function FindPropName(Values: PRawUtf8; const Value: RawUtf8; ValuesCount: integer): integer;
var
  ValueLen: TStrLen;
begin
  dec(ValuesCount);
  ValueLen := length(Value);
  if ValueLen = 0 then
    for result := 0 to ValuesCount do
      if Values^ = '' then
        exit
      else
        inc(Values)
  else
    for result := 0 to ValuesCount do
      if (PtrUInt(Values^) <> 0) and
         (PStrLen(PtrUInt(Values^) - _STRLEN)^ = ValueLen) and
         IdemPropNameUSameLenNotNull(pointer(Values^), pointer(Value), ValueLen) then
        exit
      else
        inc(Values);
  result := -1;
end;

function FindRawUtf8(const Values: TRawUtf8DynArray; const Value: RawUtf8;
  CaseSensitive: boolean): integer;
begin
  result := FindRawUtf8(pointer(Values), Value, length(Values), CaseSensitive);
end;

function FindRawUtf8(const Values: array of RawUtf8; const Value: RawUtf8;
  CaseSensitive: boolean): integer;
begin
  result := high(Values);
  if result >= 0 then
    result := FindRawUtf8(@Values[0], Value, result + 1, CaseSensitive);
end;

function FindPropName(const Names: array of RawUtf8; const Name: RawUtf8): integer;
begin
  result := high(Names);
  if result >= 0 then
    result := FindPropName(@Names[0], Name, result + 1);
end;

function AddRawUtf8(var Values: TRawUtf8DynArray; const Value: RawUtf8;
  NoDuplicates, CaseSensitive: boolean): boolean;
var
  i: PtrInt;
begin
  if NoDuplicates then
  begin
    i := FindRawUtf8(Values, Value, CaseSensitive);
    if i >= 0 then
    begin
      result := false;
      exit;
    end;
  end;
  i := length(Values);
  SetLength(Values, i + 1);
  Values[i] := Value;
  result := true;
end;

procedure AddRawUtf8(var Values: TRawUtf8DynArray; var ValuesCount: integer;
  const Value: RawUtf8);
var
  capacity: integer;
begin
  capacity := Length(Values);
  if ValuesCount = capacity then
    SetLength(Values, NextGrow(capacity));
  Values[ValuesCount] := Value;
  inc(ValuesCount);
end;

function RawUtf8DynArrayEquals(const A, B: TRawUtf8DynArray): boolean;
var
  n, i: PtrInt;
begin
  result := false;
  n := length(A);
  if n <> length(B) then
    exit;
  for i := 0 to n - 1 do
    if A[i] <> B[i] then
      exit;
  result := true;
end;

function RawUtf8DynArrayEquals(const A, B: TRawUtf8DynArray; Count: integer): boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 0 to Count - 1 do
    if A[i] <> B[i] then
      exit;
  result := true;
end;

function AddString(var Values: TStringDynArray; const Value: string): PtrInt;
begin
  result := length(Values);
  SetLength(Values, result + 1);
  Values[result] := Value;
end;

procedure StringDynArrayToRawUtf8DynArray(const Source: TStringDynArray;
  var Result: TRawUtf8DynArray);
var
  i: PtrInt;
begin
  Finalize(Result);
  SetLength(Result, length(Source));
  for i := 0 to length(Source) - 1 do
    StringToUtf8(Source[i], Result[i]);
end;

procedure StringListToRawUtf8DynArray(Source: TStringList; var Result: TRawUtf8DynArray);
var
  i: PtrInt;
begin
  Finalize(Result);
  SetLength(Result, Source.Count);
  for i := 0 to Source.Count - 1 do
    StringToUtf8(Source[i], Result[i]);
end;

function FastLocatePUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt; Value: PUtf8Char): PtrInt;
begin
  result := FastLocatePUtf8CharSorted(P, R, Value, TUtf8Compare(@StrComp));
end;

function FastLocatePUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char; Compare: TUtf8Compare): PtrInt;
var
  L, i, cmp: PtrInt;
begin
  // fast O(log(n)) binary search
  if not Assigned(Compare) or
     (R < 0) then
    result := 0
  else if Compare(P^[R], Value) < 0 then // quick return if already sorted
    result := R + 1
  else
  begin
    L := 0;
    result := -1; // return -1 if found
    repeat
      {$ifdef CPUX64}
      i := L + R;
      i := i shr 1;
      {$else}
      i := (L + R) shr 1;
      {$endif CPUX64}
      cmp := Compare(P^[i], Value);
      if cmp = 0 then
        exit;
      if cmp < 0 then
        L := i + 1
      else
        R := i - 1;
    until (L > R);
    while (i >= 0) and (Compare(P^[i], Value) >= 0) do
      dec(i);
    result := i + 1; // return the index where to insert
  end;
end;

function FastFindPUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char; Compare: TUtf8Compare): PtrInt;
var
  L, cmp: PtrInt;
begin
  // fast O(log(n)) binary search
  L := 0;
  if Assigned(Compare) and (R >= 0) then
    repeat
      {$ifdef CPUX64}
      result := L + R;
      result := result shr 1;
      {$else}
      result := (L + R) shr 1;
      {$endif CPUX64}
      cmp := Compare(P^[result], Value);
      if cmp = 0 then
        exit;
      if cmp < 0 then
      begin
        L := result + 1;
        if L <= R then
          continue;
        break;
      end;
      R := result - 1;
      if L <= R then
        continue;
      break;
    until false;
  result := -1;
end;

{$ifdef CPUX64}

function FastFindPUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt; Value: PUtf8Char): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        {$ifdef win64}  // P=rcx/rdi R=rdx/rsi Value=r8/rdx
        push    rdi
        mov     rdi, P  // P=rdi
        {$endif win64}
        push    r12
        push    r13
        xor     r9, r9  // L=r9
        test    R, R
        jl      @err
        test    Value, Value
        jz      @void
        mov     cl, byte ptr [Value]  // to check first char (likely diverse)
@s:     lea     rax, qword ptr [r9 + R]
        shr     rax, 1
        lea     r12, qword ptr [rax - 1]  // branchless main loop
        lea     r13, qword ptr [rax + 1]
        mov     r10, qword ptr [rdi + rax * 8]
        test    r10, r10
        jz      @lt
        cmp     cl, byte ptr [r10]
        je      @eq
        cmovc   R, r12
        cmovnc  r9, r13
@nxt:   cmp     r9, R
        jle     @s
@err:   mov     rax, -1
@found: pop     r13
        pop     r12
        {$ifdef win64}
        pop     rdi
        {$endif win64}
        ret
@lt:    mov     r9, r13 // very unlikely P[rax]=nil
        jmp     @nxt
@eq:    mov     r11, Value // first char equal -> check others
@sub:   mov     cl, byte ptr [r10]
        add     r10, 1
        add     r11, 1
        test    cl, cl
        jz      @found
        mov     cl, byte ptr [r11]
        cmp     cl, byte ptr [r10]
        je      @sub
        mov     cl, byte ptr [Value]  // reset first char
        cmovc   R, r12
        cmovnc  r9, r13
        cmp     r9, R
        jle     @s
        jmp     @err
@void:  mov     rax, -1
        cmp     qword ptr [P], 0
        cmove   rax, Value
        jmp     @found
end;

{$else}

function FastFindPUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt; Value: PUtf8Char): PtrInt;
var
  L: PtrInt;
  c: byte;
  piv, val: PByte;
begin
  // fast O(log(n)) binary search using inlined StrCompFast()
  if R >= 0 then
    if Value <> nil then
    begin
      L := 0;
      repeat
        result := (L + R) shr 1;
        piv := pointer(P^[result]);
        if piv <> nil then
        begin
          val := pointer(Value);
          c := piv^;
          if c = val^ then
            repeat
              if c = 0 then
                exit;  // StrComp(P^[result],Value)=0
              inc(piv);
              inc(val);
              c := piv^;
            until c <> val^;
          if c > val^ then
          begin
            R := result - 1;  // StrComp(P^[result],Value)>0
            if L <= R then
              continue;
            break;
          end;
        end;
        L := result + 1;  // StrComp(P^[result],Value)<0
        if L <= R then
          continue;
        break;
      until false;
    end
    else if P^[0] = nil then
    begin
      // '' should be in lowest P[] slot
      result := 0;
      exit;
    end;
  result := -1;
end;

{$endif CPUX64}

function FastFindUpperPUtf8CharSorted(P: PPUtf8CharArray; R: PtrInt;
  Value: PUtf8Char; ValueLen: PtrInt): PtrInt;
var
  tmp: array[byte] of AnsiChar;
begin
  UpperCopy255Buf(@tmp, Value, ValueLen)^ := #0;
  result := FastFindPUtf8CharSorted(P, R, @tmp);
end;

function FastFindIndexedPUtf8Char(P: PPUtf8CharArray; R: PtrInt;
  var SortedIndexes: TCardinalDynArray; Value: PUtf8Char;
  ItemComp: TUtf8Compare): PtrInt;
var
  L, cmp: PtrInt;
begin
  // fast O(log(n)) binary search
  L := 0;
  if 0 <= R then
    repeat
      {$ifdef CPUX64}
      result := L + R;
      result := result shr 1;
      {$else}
      result := (L + R) shr 1;
      {$endif CPUX64}
      cmp := ItemComp(P^[SortedIndexes[result]], Value);
      if cmp = 0 then
      begin
        result := SortedIndexes[result];
        exit;
      end;
      if cmp < 0 then
      begin
        L := result + 1;
        if L <= R then
          continue;
        break;
      end;
      R := result - 1;
      if L <= R then
        continue;
      break;
    until false;
  result := -1;
end;

function AddSortedRawUtf8(var Values: TRawUtf8DynArray; var ValuesCount: integer;
  const Value: RawUtf8; CoValues: PIntegerDynArray; ForcedIndex: PtrInt;
  Compare: TUtf8Compare): PtrInt;
var
  n: PtrInt;
begin
  if ForcedIndex >= 0 then
    result := ForcedIndex
  else
  begin
    if not Assigned(Compare) then
      Compare := @StrComp;
    result := FastLocatePUtf8CharSorted(pointer(Values), ValuesCount - 1,
      pointer(Value), Compare);
    if result < 0 then
      exit; // Value exists -> fails
  end;
  n := Length(Values);
  if ValuesCount = n then
  begin
    n := NextGrow(n);
    SetLength(Values, n);
    if CoValues <> nil then
      SetLength(CoValues^, n);
  end;
  n := ValuesCount;
  if result < n then
  begin
    n := (n - result) * SizeOf(pointer);
    MoveFast(Pointer(Values[result]), Pointer(Values[result + 1]), n);
    PtrInt(Values[result]) := 0; // avoid GPF
    if CoValues <> nil then
    begin
      {$ifdef CPU64} n := n shr 1; {$endif} // 64-bit pointer to 32-bit integer
      MoveFast(CoValues^[result], CoValues^[result + 1], n);
    end;
  end
  else
    result := n;
  Values[result] := Value;
  inc(ValuesCount);
end;

type
  /// used internally for faster quick sort
  TQuickSortRawUtf8 = object
    Compare: TUtf8Compare;
    CoValues: PIntegerArray;
    pivot: pointer;
    procedure Sort(Values: PPointerArray; L, R: PtrInt);
  end;

procedure TQuickSortRawUtf8.Sort(Values: PPointerArray; L, R: PtrInt);
var
  I, J, P: PtrInt;
  tmp: Pointer;
  int: integer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := Values^[P];
        while Compare(Values^[I], pivot) < 0 do
          inc(I);
        while Compare(Values^[J], pivot) > 0 do
          dec(J);
        if I <= J then
        begin
          tmp := Values^[J];
          Values^[J] := Values^[I];
          Values^[I] := tmp;
          if CoValues <> nil then
          begin
            int := CoValues^[J];
            CoValues^[J] := CoValues^[I];
            CoValues^[I] := int;
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          Sort(Values, L, J);
        L := I;
      end
      else
      begin
        if I < R then
          Sort(Values, I, R);
        R := J;
      end;
    until L >= R;
end;

procedure QuickSortRawUtf8(var Values: TRawUtf8DynArray; ValuesCount: integer;
  CoValues: PIntegerDynArray; Compare: TUtf8Compare);
var
  QS: TQuickSortRawUtf8;
begin
  if Assigned(Compare) then
    QS.Compare := Compare
  else
    QS.Compare := @StrComp;
  if CoValues = nil then
    QS.CoValues := nil
  else
    QS.CoValues := pointer(CoValues^);
  QS.Sort(pointer(Values), 0, ValuesCount - 1);
end;

procedure QuickSortRawUtf8(Values: PRawUtf8Array; L, R: PtrInt;
  caseInsensitive: boolean);
var
  QS: TQuickSortRawUtf8;
begin
  QS.Compare := StrCompByCase[caseInsensitive];
  QS.CoValues := nil;
  QS.Sort(pointer(Values), L, R);
end;

function DeleteRawUtf8(var Values: TRawUtf8DynArray; Index: integer): boolean;
var
  n: integer;
begin
  n := length(Values);
  if cardinal(Index) >= cardinal(n) then
    result := false
  else
  begin
    dec(n);
    if PRefCnt(PtrUInt(Values) - _DAREFCNT)^ > 1 then
      Values := copy(Values); // make unique
    Values[Index] := ''; // avoid GPF
    if n > Index then
    begin
      MoveFast(pointer(Values[Index + 1]), pointer(Values[Index]),
        (n - Index) * SizeOf(pointer));
      PtrUInt(Values[n]) := 0; // avoid GPF
    end;
    SetLength(Values, n);
    result := true;
  end;
end;

function DeleteRawUtf8(var Values: TRawUtf8DynArray; var ValuesCount: integer;
  Index: integer; CoValues: PIntegerDynArray): boolean;
var
  n: integer;
begin
  n := ValuesCount;
  if cardinal(Index) >= cardinal(n) then
    result := false
  else
  begin
    dec(n);
    ValuesCount := n;
    if PRefCnt(PtrUInt(Values) - _DAREFCNT)^ > 1 then
      Values := copy(Values); // make unique
    Values[Index] := ''; // avoid GPF
    dec(n, Index);
    if n > 0 then
    begin
      if CoValues <> nil then
        MoveFast(CoValues^[Index + 1], CoValues^[Index], n * SizeOf(integer));
      MoveFast(pointer(Values[Index + 1]), pointer(Values[Index]), n * SizeOf(pointer));
      PtrUInt(Values[ValuesCount]) := 0; // avoid GPF
    end;
    result := true;
  end;
end;


{ ************ Numbers (integers or floats) to Text Conversion }

procedure Int32ToUtf8(Value: PtrInt; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if PtrUInt(Value) <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrInt32(@tmp[23], Value);
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

function Int32ToUtf8(Value: PtrInt): RawUtf8;
begin
  Int32ToUtf8(Value, result);
end;

procedure Int64ToUtf8(Value: Int64; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  {$ifdef CPU64}
  if PtrUInt(Value) <= high(SmallUInt32Utf8) then
  {$else} // Int64Rec gives compiler internal error C4963
  if (PCardinalArray(@Value)^[0] <= high(SmallUInt32Utf8)) and
     (PCardinalArray(@Value)^[1] = 0) then
  {$endif CPU64}
    result := SmallUInt32Utf8[Value]
  else
  begin
    {$ifdef CPU64}
    P := StrInt32(@tmp[23], Value);
    {$else}
    P := StrInt64(@tmp[23], Value);
    {$endif CPU64}
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

procedure UInt64ToUtf8(Value: QWord; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  {$ifdef CPU64}
  if Value <= high(SmallUInt32Utf8) then
  {$else} // Int64Rec gives compiler internal error C4963
  if (PCardinalArray(@Value)^[0] <= high(SmallUInt32Utf8)) and
     (PCardinalArray(@Value)^[1] = 0) then
  {$endif CPU64}
    result := SmallUInt32Utf8[Value]
  else
  begin
    {$ifdef CPU64}
    P := StrUInt32(@tmp[23], Value);
    {$else}
    P := StrUInt64(@tmp[23], Value);
    {$endif CPU64}
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

function Int64ToUtf8(Value: Int64): RawUtf8; // faster than SysUtils.IntToStr
begin
  Int64ToUtf8(Value, result);
end;

{$ifdef CPU32} // already implemented by ToUtf8(Value: PtrInt) below for CPU64
function ToUtf8(Value: Int64): RawUtf8;
begin
  Int64ToUtf8(Value, result);
end;
{$endif CPU32}

function ToUtf8(Value: PtrInt): RawUtf8;
begin
  Int32ToUtf8(Value, result);
end;

procedure UInt32ToUtf8(Value: PtrUInt; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if Value <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

function UInt32ToUtf8(Value: PtrUInt): RawUtf8;
begin
  UInt32ToUtf8(Value, result);
end;

function StrCurr64(P: PAnsiChar; const Value: Int64): PAnsiChar;
var
  c: QWord;
  d: cardinal;
begin
  if Value = 0 then
  begin
    result := P - 1;
    result^ := '0';
    exit;
  end;
  if Value < 0 then
    c := -Value
  else
    c := Value;
  if c < 10000 then
  begin
    result := P - 6; // only decimals -> append '0.xxxx'
    PWord(result)^ := ord('0') + ord('.') shl 8;
    YearToPChar(c, PUtf8Char(P) - 4);
  end
  else
  begin
    result := StrUInt64(P - 1, c);
    d := PCardinal(P - 5)^; // in two explit steps for CPUARM (alf)
    PCardinal(P - 4)^ := d;
    P[-5] := '.'; // insert '.' just before last 4 decimals
  end;
  if Value < 0 then
  begin
    dec(result);
    result^ := '-';
  end;
end;

procedure Curr64ToStr(const Value: Int64; var result: RawUtf8);
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
  Decim, L: cardinal;
begin
  if Value = 0 then
    result := SmallUInt32Utf8[0]
  else
  begin
    P := StrCurr64(@tmp[31], Value);
    L := @tmp[31] - P;
    if L > 4 then
    begin
      Decim := PCardinal(P + L - SizeOf(cardinal))^; // 4 last digits = 4 decimals
      if Decim = ord('0') + ord('0') shl 8 + ord('0') shl 16 + ord('0') shl 24 then
        dec(L, 5)
      else // no decimal
      if Decim and $ffff0000 = ord('0') shl 16 + ord('0') shl 24 then
        dec(L, 2); // 2 decimals
    end;
    FastSetString(result, P, L);
  end;
end;

function Curr64ToStr(const Value: Int64): RawUtf8;
begin
  Curr64ToStr(Value, result);
end;

function CurrencyToStr(const Value: currency): RawUtf8;
begin
  result := Curr64ToStr(PInt64(@Value)^);
end;

function Curr64ToPChar(const Value: Int64; Dest: PUtf8Char): PtrInt;
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
  Decim: cardinal;
begin
  P := StrCurr64(@tmp[31], Value);
  result := @tmp[31] - P;
  if result > 4 then
  begin
    // Decim = 4 last digits = 4 decimals
    Decim := PCardinal(P + result - SizeOf(cardinal))^;
    if Decim = ord('0') + ord('0') shl 8 + ord('0') shl 16 + ord('0') shl 24 then
      // no decimal -> trunc trailing *.0000 chars
      dec(result, 5)
    else if Decim and $ffff0000 = ord('0') shl 16 + ord('0') shl 24 then
      // 2 decimals -> trunc trailing *.??00 chars
      dec(result, 2);
  end;
  MoveSmall(P, Dest, result);
end;

function StrToCurr64(P: PUtf8Char; NoDecimal: PBoolean): Int64;
var
  c: cardinal;
  minus: boolean;
  Dec: cardinal;
begin
  result := 0;
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  if P^ = '-' then
  begin
    minus := true;
    repeat
      inc(P)
    until P^ <> ' ';
  end
  else
  begin
    minus := false;
    if P^ = '+' then
      repeat
        inc(P)
      until P^ <> ' ';
  end;
  if P^ = '.' then
  begin
    // '.5' -> 500
    Dec := 2;
    inc(P);
  end
  else
    Dec := 0;
  c := byte(P^) - 48;
  if c > 9 then
    exit;
  PCardinal(@result)^ := c;
  inc(P);
  repeat
    if P^ <> '.' then
    begin
      c := byte(P^) - 48;
      if c > 9 then
        break;
      {$ifdef CPU32DELPHI}
      result := result shl 3 + result + result;
      {$else}
      result := result * 10;
      {$endif CPU32DELPHI}
      inc(result, c);
      inc(P);
      if Dec <> 0 then
      begin
        inc(Dec);
        if Dec < 5 then
          continue
        else
          break;
      end;
    end
    else
    begin
      inc(Dec);
      inc(P);
    end;
  until false;
  if NoDecimal <> nil then
    if Dec = 0 then
    begin
      NoDecimal^ := true;
      if minus then
        result := -result;
      exit;
    end
    else
      NoDecimal^ := false;
  if Dec <> 5 then
    // Dec=5 most of the time
    case Dec of
      0, 1:
        result := result * 10000;
      {$ifdef CPU32DELPHI}
      2:
        result := result shl 10 - result shl 4 - result shl 3;
      3:
        result := result shl 6 + result shl 5 + result shl 2;
      4:
        result := result shl 3 + result + result;
      {$else}
      2:
        result := result * 1000;
      3:
        result := result * 100;
      4:
        result := result * 10;
      {$endif CPU32DELPHI}
    end;
  if minus then
    result := -result;
end;

function StrToCurrency(P: PUtf8Char): currency;
begin
  PInt64(@result)^ := StrToCurr64(P, nil);
end;

{$ifdef UNICODE}

function IntToString(Value: integer): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  P := StrInt32(@tmp[23], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[23] - P, result);
end;

function IntToString(Value: cardinal): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  P := StrUInt32(@tmp[23], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[23] - P, result);
end;

function IntToString(Value: Int64): string;
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
begin
  P := StrInt64(@tmp[31], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[31] - P, result);
end;

function DoubleToString(Value: Double): string;
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := '0'
  else
    Ansi7ToString(PWinAnsiChar(@tmp[1]), DoubleToShort(tmp, Value), result);
end;

function Curr64ToString(Value: Int64): string;
var
  tmp: array[0..31] of AnsiChar;
begin
  Ansi7ToString(tmp, Curr64ToPChar(Value, tmp), result);
end;

{$else UNICODE}

function IntToString(Value: integer): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if cardinal(Value) <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrInt32(@tmp[23], Value);
    SetString(result, P, @tmp[23] - P);
  end;
end;

function IntToString(Value: cardinal): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if Value <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    SetString(result, P, @tmp[23] - P);
  end;
end;

function IntToString(Value: Int64): string;
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
begin
  if (Value >= 0) and
     (Value <= high(SmallUInt32Utf8)) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrInt64(@tmp[31], Value);
    SetString(result, P, @tmp[31] - P);
  end;
end;

function DoubleToString(Value: Double): string;
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := '0'
  else
    SetString(result, PAnsiChar(@tmp[1]), DoubleToShort(tmp, Value));
end;

function Curr64ToString(Value: Int64): string;
begin
  result := Curr64ToStr(Value);
end;

{$endif UNICODE}

{$ifndef EXTENDEDTOSHORT_USESTR}
var // standard FormatSettings (US)
  SettingsUS: TFormatSettings;
{$endif EXTENDEDTOSHORT_USESTR}

// used ExtendedToShortNoExp / DoubleToShortNoExp from str/DoubleToAscii output
function FloatStringNoExp(S: PAnsiChar; Precision: PtrInt): PtrInt;
var
  i, prec: PtrInt;
  c: AnsiChar;
begin
  result := ord(S[0]);
  prec := result; // if no decimal
  if S[1] = '-' then
    dec(prec);
  // test if scientific format -> return as this
  for i := 2 to result do
  begin
    c := S[i];
    if c = 'E' then // should not appear
      exit
    else if c = '.' then
      if i >= Precision then
      begin
        // return huge decimal number as is
        result := i - 1;
        exit;
      end
      else
        dec(prec);
  end;
  if (prec >= Precision) and
     (prec <> result) then
  begin
    dec(result, prec - Precision);
    if S[result + 1] > '5' then
    begin
      // manual rounding
      prec := result;
      repeat
        c := S[prec];
        if c <> '.' then
          if c = '9' then
          begin
            S[prec] := '0';
            if ((prec = 2) and (S[1] = '-')) or
               (prec = 1) then
            begin
              i := result;
              inc(S, prec);
              repeat
                // inlined MoveFast(S[prec],S[prec+1],result);
                S[i] := S[i - 1];
                dec(i);
              until i = 0;
              S^ := '1';
              dec(S, prec);
              break;
            end;
          end
          else if (c >= '0') and (c <= '8') then
          begin
            inc(S[prec]);
            break;
          end
          else
            break;
        dec(prec);
      until prec = 0;
    end; // note: this fixes http://stackoverflow.com/questions/2335162
  end;
  if S[result] = '0' then
    repeat
      // trunc any trailing 0
      dec(result);
      c := S[result];
      if c <> '.' then
        if c <> '0' then
          break
        else
          continue
      else
      begin
        dec(result);
        if (result = 2) and
           (S[1] = '-') and
           (S[2] = '0') then
        begin
          result := 1;
          S[1] := '0'; // '-0.000' -> '0'
        end;
        break; // if decimal are all '0' -> return only integer part
      end;
    until false;
end;

function ExtendedToShortNoExp(var S: ShortString; Value: TSynExtended;
  Precision: integer): integer;
begin
  {$ifdef DOUBLETOSHORT_USEGRISU}
  if Precision = DOUBLE_PRECISION then
    DoubleToAscii(0, DOUBLE_PRECISION, Value, @S)
  else
  {$endif DOUBLETOSHORT_USEGRISU}
    str(Value: 0: Precision, S); // not str(Value:0,S) -> '  0.0E+0000'
  result := FloatStringNoExp(@S, Precision);
  S[0] := AnsiChar(result);
end;

const // range when to switch into scientific notation - minimal 6 digits
  SINGLE_HI = 1E3;
  SINGLE_LO = 1E-3;
  DOUBLE_HI = 1E9;
  DOUBLE_LO = 1E-9;
  {$ifdef TSYNEXTENDED80}
  EXT_HI = 1E12;
  EXT_LO = 1E-12;
  {$endif TSYNEXTENDED80}

{$ifdef EXTENDEDTOSHORT_USESTR}
function ExtendedToShort(var S: ShortString; Value: TSynExtended; Precision: integer): integer;
var
  scientificneeded: boolean;
  valueabs: TSynExtended;
begin
  {$ifdef DOUBLETOSHORT_USEGRISU}
  if Precision = DOUBLE_PRECISION then
  begin
    result := DoubleToShort(S, Value);
    exit;
  end;
  {$endif DOUBLETOSHORT_USEGRISU}
  if Value = 0 then
  begin
    PWord(@S)^ := 1 + ord('0') shl 8;
    result := 1;
    exit;
  end;
  scientificneeded := false;
  valueabs := abs(Value);
  if Precision <= SINGLE_PRECISION then
  begin
    if (valueabs > SINGLE_HI) or
       (valueabs < SINGLE_LO) then
      scientificneeded := true;
  end
  else
  {$ifdef TSYNEXTENDED80}
  if Precision > DOUBLE_PRECISION then
  begin
    if (valueabs > EXT_HI) or
       (valueabs < EXT_LO) then
      scientificneeded := true;
  end
  else
  {$endif TSYNEXTENDED80}
  if (valueabs > DOUBLE_HI) or
     (valueabs < DOUBLE_LO) then
    scientificneeded := true;
  if scientificneeded then
  begin
    str(Value, S);
    if S[1] = ' ' then
    begin
      dec(S[0]);
      MoveSmall(@S[2], @S[1], ord(S[0]));
    end;
    result := ord(S[0]);
  end
  else
  begin
    str(Value: 0:Precision, S); // not str(Value:0,S) -> '  0.0E+0000'
    result := FloatStringNoExp(@S, Precision);
    S[0] := AnsiChar(result);
  end;
end;

{$else not EXTENDEDTOSHORT_USESTR}

function ExtendedToShort(var S: ShortString; Value: TSynExtended; Precision: integer): integer;
{$ifdef UNICODE}
var
  i: PtrInt;
{$endif UNICODE}
begin
  // use ffGeneral: see https://synopse.info/forum/viewtopic.php?pid=442#p442
  result := FloatToText(PChar(@S[1]), Value, fvExtended, ffGeneral, Precision, 0, SettingsUS);
  {$ifdef UNICODE} // FloatToText(PWideChar) is faster than FloatToText(PAnsiChar)
  for i := 1 to result do
    PByteArray(@S)[i] := PWordArray(PtrInt(@S) - 1)[i];
  {$endif UNICODE}
  S[0] := AnsiChar(result);
end;

{$endif EXTENDEDTOSHORT_USESTR}

function FloatToShortNan(const s: shortstring): TFloatNan;
begin
  case PInteger(@s)^ and $ffdfdfdf of
    3 + ord('N') shl 8 + ord('A') shl 16 + ord('N') shl 24:
      result := fnNan;
    3 + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24,
    4 + ord('+') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := fnInf;
    4 + ord('-') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := fnNegInf;
  else
    result := fnNumber;
  end;
end;

function FloatToStrNan(const s: RawUtf8): TFloatNan;
begin
  case length(s) of
    3:
      case PInteger(s)^ and $dfdfdf of
        ord('N') + ord('A') shl 8 + ord('N') shl 16:
          result := fnNan;
        ord('I') + ord('N') shl 8 + ord('F') shl 16:
          result := fnInf;
      else
        result := fnNumber;
      end;
    4:
      case PInteger(s)^ and $dfdfdfdf of
        ord('+') + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24:
          result := fnInf;
        ord('-') + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24:
          result := fnNegInf;
      else
        result := fnNumber;
      end;
  else
    result := fnNumber;
  end;
end;

function ExtendedToStr(Value: TSynExtended; Precision: integer): RawUtf8;
begin
  ExtendedToStr(Value, Precision, result);
end;

procedure ExtendedToStr(Value: TSynExtended; Precision: integer; var result: RawUtf8);
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := SmallUInt32Utf8[0]
  else
    FastSetString(result, @tmp[1], ExtendedToShort(tmp, Value, Precision));
end;

function FloatToJsonNan(const s: ShortString): PShortString;
begin
  case PInteger(@s)^ and $ffdfdfdf of
    3 + ord('N') shl 8 + ord('A') shl 16 + ord('N') shl 24:
      result := @JSON_NAN[fnNan];
    3 + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24,
    4 + ord('+') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := @JSON_NAN[fnInf];
    4 + ord('-') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := @JSON_NAN[fnNegInf];
  else
    result := @s;
  end;
end;

function ExtendedToJson(var tmp: ShortString; Value: TSynExtended;
  Precision: integer; NoExp: boolean): PShortString;
begin
  if Value = 0 then
    result := @JSON_NAN[fnNumber]
  else
  begin
    if NoExp then
      ExtendedToShortNoExp(tmp, Value, Precision)
    else
      ExtendedToShort(tmp, Value, Precision);
    result := FloatToJsonNan(tmp);
  end;
end;

{$ifdef DOUBLETOSHORT_USEGRISU}

{
    Implement 64-bit floating point (double) to ASCII conversion using the
    GRISU-1 efficient algorithm.

    Original Code in flt_core.inc flt_conv.inc flt_pack.inc from FPC RTL.
    Copyright (C) 2013 by Max Nazhalov
    Licenced with LGPL 2 with the linking exception.
    If you don't agree with these License terms, disable this feature
    by undefining DOUBLETOSHORT_USEGRISU in Synopse.inc

    GRISU Original Algorithm
    Copyright (c) 2009 Florian Loitsch

    We extracted a double-to-ascii only cut-down version of those files,
    and made a huge refactoring to reach the best performance, especially
    tuning the Intel target with some dedicated asm and code rewrite.

  With Delphi 10.3 on Win32:
   100000 FloatToText    in 38.11ms i.e. 2,623,570/s, aver. 0us, 47.5 MB/s
   100000 str            in 43.19ms i.e. 2,315,082/s, aver. 0us, 50.7 MB/s
   100000 DoubleToShort  in 45.50ms i.e. 2,197,367/s, aver. 0us, 43.8 MB/s
   100000 DoubleToAscii  in 42.44ms i.e. 2,356,045/s, aver. 0us, 47.8 MB/s

  With Delphi 10.3 on Win64:
   100000 FloatToText    in 61.83ms i.e. 1,617,233/s, aver. 0us, 29.3 MB/s
   100000 str            in 53.20ms i.e. 1,879,663/s, aver. 0us, 41.2 MB/s
   100000 DoubleToShort  in 18.45ms i.e. 5,417,998/s, aver. 0us, 108 MB/s
   100000 DoubleToAscii  in 18.19ms i.e. 5,496,921/s, aver. 0us, 111.5 MB/s

  With FPC on Win32:
   100000 FloatToText    in 115.62ms i.e.  864,842/s, aver. 1us, 15.6 MB/s
   100000 str            in 57.30ms i.e. 1,745,109/s, aver. 0us, 39.9 MB/s
   100000 DoubleToShort  in 23.88ms i.e. 4,187,078/s, aver. 0us, 83.5 MB/s
   100000 DoubleToAscii  in 23.34ms i.e. 4,284,490/s, aver. 0us, 86.9 MB/s

  With FPC on Win64:
   100000 FloatToText    in 76.92ms i.e. 1,300,052/s, aver. 0us, 23.5 MB/s
   100000 str            in 27.70ms i.e. 3,609,456/s, aver. 0us, 82.6 MB/s
   100000 DoubleToShort  in 14.73ms i.e. 6,787,944/s, aver. 0us, 135.4 MB/s
   100000 DoubleToAscii  in 13.78ms i.e. 7,253,735/s, aver. 0us, 147.2 MB/s

  With FPC on Linux x86_64:
   100000 FloatToText    in 81.48ms i.e. 1,227,249/s, aver. 0us, 22.2 MB/s
   100000 str            in 36.98ms i.e. 2,703,871/s, aver. 0us, 61.8 MB/s
   100000 DoubleToShort  in 13.11ms i.e. 7,626,601/s, aver. 0us, 152.1 MB/s
   100000 DoubleToAscii  in 12.59ms i.e. 7,942,180/s, aver. 0us, 161.2 MB/s

  - Our rewrite is twice faster than original flt_conv.inc from FPC RTL (str)
  - Delphi Win32 has trouble making 64-bit computation - no benefit since it
    has good optimized i87 asm (but slower than our code with FPC/Win32)
  - FPC is more efficient when compiling integer arithmetic; we avoided slow
    division by calling our Div100(), but Delphi Win64 is still far behind
  - Delphi Win64 has very slow FloatToText and str()

}

// Controls printing of NaN-sign.
// Undefine to print NaN sign during float->ASCII conversion.
// IEEE does not interpret the sign of a NaN, so leave it defined.
{$define GRISU1_F2A_NAN_SIGNLESS}

// Controls rounding of generated digits when formatting with narrowed
// width (either fixed or exponential notation).
// Traditionally, FPC and BP7/Delphi use "roundTiesToAway" mode.
// Undefine to use "roundTiesToEven" approach.
{$define GRISU1_F2A_HALF_ROUNDUP}

// This one is a hack against Grusu sub-optimality.
// It may be used only strictly together with GRISU1_F2A_HALF_ROUNDUP.
// It does not violate most general rules due to the fact that it is
// applicable only when formatting with narrowed width, where the fine
// view is more desirable, and the precision is already lost, so it can
// be used in general-purpose applications.
// Refer to its implementation.
{$define GRISU1_F2A_AGRESSIVE_ROUNDUP} // Defining this fixes several tests.

// Undefine to enable SNaN support.
// Note: IEEE [754-2008, page 31] requires (1) to recognize "SNaN" during
// ASCII->float, and (2) to generate the "invalid FP operation" exception
// either when SNaN is printed as "NaN", or "SNaN" is evaluated to QNaN,
// so it would be preferable to undefine these settings,
// but the FPC RTL is not ready for this right now..
{$define GRISU1_F2A_NO_SNAN}

/// If Value=0 would just store '0', whatever frac_digits is supplied.
{$define GRISU1_F2A_ZERONOFRACT}

var
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their byte digits (00..99) equivalence
  // - used e.g. by DoubleToAscii() implementing Grisu algorithm
  TwoDigitByteLookupW: packed array[0..99] of word;

const
  // TFloatFormatProfile for double
  nDig_mantissa = 17;
  nDig_exp10 = 3;

type
  // "Do-It-Yourself Floating-Point" structures
  TDIY_FP = record
    f: qword;
    e: integer;
  end;

  TDIY_FP_Power_of_10 = record
    c: TDIY_FP;
    e10: integer;
  end;
  PDIY_FP_Power_of_10 = ^TDIY_FP_Power_of_10;

const
  ROUNDER = $80000000;

{$ifdef CPUINTEL} // our faster version using 128-bit x86_64 multiplication

procedure d2a_diy_fp_multiply(var x, y: TDIY_FP; normalize: boolean;
  out result: TDIY_FP); {$ifdef HASINLINE}inline;{$endif}
var
  p: THash128Rec;
begin
  mul64x64(x.f, y.f, p); // fast x86_64 / i386 asm
  if (p.c1 and ROUNDER) <>  0 then
    inc(p.h);
  result.f := p.h;
  result.e := PtrInt(x.e) + PtrInt(y.e) + 64;
  if normalize then
    if (PQWordRec(@result.f)^.h and ROUNDER) = 0 then
    begin
      result.f := result.f * 2;
      dec(result.e);
    end;
end;

{$else} // regular Grisu method - optimized for 32-bit CPUs

procedure d2a_diy_fp_multiply(var x, y: TDIY_FP; normalize: boolean; out result: TDIY_FP);
var
  _x: TQWordRec absolute x;
  _y: TQWordRec absolute y;
  r: TQWordRec absolute result;
  ac, bc, ad, bd, t1: TQWordRec;
begin
  ac.v := qword(_x.h) * _y.h;
  bc.v := qword(_x.l) * _y.h;
  ad.v := qword(_x.h) * _y.l;
  bd.v := qword(_x.l) * _y.l;
  t1.v := qword(ROUNDER) + bd.h + bc.l + ad.l;
  result.f := ac.v + ad.h + bc.h + t1.h;
  result.e := x.e + y.e + 64;
  if normalize then
    if (r.h and ROUNDER) = 0 then
    begin
      inc(result.f, result.f);
      dec(result.e);
    end;
end;

{$endif CPUINTEL}

const
  // alpha =-61; gamma = 0
  // full cache: 1E-450 .. 1E+432, step = 1E+18
  // sparse = 1/10
  C_PWR10_DELTA = 18;
  C_PWR10_COUNT = 50;

type
  TDIY_FP_Cached_Power10 = record
    base:         array [ 0 .. 9 ] of TDIY_FP_Power_of_10;
    factor_plus:  array [ 0 .. 1 ] of TDIY_FP_Power_of_10;
    factor_minus: array [ 0 .. 1 ] of TDIY_FP_Power_of_10;
    // extra mantissa correction [ulp; signed]
    corrector:    array [ 0 .. C_PWR10_COUNT - 1 ] of shortint;
  end;

const
  CACHED_POWER10: TDIY_FP_Cached_Power10 = (
    base: (
        ( c: ( f: qword($825ECC24C8737830); e: -362 ); e10:  -90 ),
        ( c: ( f: qword($E2280B6C20DD5232); e: -303 ); e10:  -72 ),
        ( c: ( f: qword($C428D05AA4751E4D); e: -243 ); e10:  -54 ),
        ( c: ( f: qword($AA242499697392D3); e: -183 ); e10:  -36 ),
        ( c: ( f: qword($9392EE8E921D5D07); e: -123 ); e10:  -18 ),
        ( c: ( f: qword($8000000000000000); e:  -63 ); e10:    0 ),
        ( c: ( f: qword($DE0B6B3A76400000); e:   -4 ); e10:   18 ),
        ( c: ( f: qword($C097CE7BC90715B3); e:   56 ); e10:   36 ),
        ( c: ( f: qword($A70C3C40A64E6C52); e:  116 ); e10:   54 ),
        ( c: ( f: qword($90E40FBEEA1D3A4B); e:  176 ); e10:   72 )
    );
    factor_plus: (
        ( c: ( f: qword($F6C69A72A3989F5C); e:   534 ); e10:  180 ),
        ( c: ( f: qword($EDE24AE798EC8284); e:  1132 ); e10:  360 )
    );
    factor_minus: (
        ( c: ( f: qword($84C8D4DFD2C63F3B); e:  -661 ); e10: -180 ),
        ( c: ( f: qword($89BF722840327F82); e: -1259 ); e10: -360 )
    );
    corrector: (
        0,  0,  0,  0,  1,  0,  0,  0,  1, -1,
        0,  1,  1,  1, -1,  0,  0,  1,  0, -1,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
       -1,  0,  0, -1,  0,  0,  0,  0,  0, -1,
        0,  0,  0,  0,  1,  0,  0,  0, -1,  0
    ));
  CACHED_POWER10_MIN10 = -90 -360;
  // = ref.base[low(ref.base)].e10 + ref.factor_minus[high(ref.factor_minus)].e10

// return normalized correctly rounded approximation of the power of 10
// scaling factor, intended to shift a binary exponent of the original number
// into selected [ alpha .. gamma ] range
procedure d2a_diy_fp_cached_power10(exp10: integer; out factor: TDIY_FP_Power_of_10);
var
  i, xmul: integer;
  A, B: PDIY_FP_Power_of_10;
  cx: PtrInt;
  ref: ^TDIY_FP_Cached_Power10;
begin
  ref := @CACHED_POWER10; // much better code generation on PIC/x86_64
  // find non-sparse index
  if exp10 <= CACHED_POWER10_MIN10 then
    i := 0
  else
  begin
    i := (exp10 - CACHED_POWER10_MIN10) div C_PWR10_DELTA;
    if i * C_PWR10_DELTA + CACHED_POWER10_MIN10 <> exp10 then
      inc(i); // round-up
    if i > C_PWR10_COUNT - 1 then
      i := C_PWR10_COUNT - 1;
  end;
  // generate result
  xmul := i div length(ref.base);
  A := @ref.base[i - (xmul * length(ref.base))]; // fast mod
  dec(xmul, length(ref.factor_minus));
  if xmul = 0 then
  begin
    // base
    factor := A^;
    exit;
  end;
  // surrogate
  if xmul > 0 then
  begin
    dec(xmul);
    B := @ref.factor_plus[xmul];
  end
  else
  begin
    xmul := -(xmul + 1);
    B := @ref.factor_minus[xmul];
  end;
  factor.e10 := A.e10 + B.e10;
  if A.e10 <> 0 then
  begin
    d2a_diy_fp_multiply(A.c, B.c, true, factor.c);
    // adjust mantissa
    cx := ref.corrector[i];
    if cx <> 0 then
      inc(int64(factor.c.f), int64(cx));
  end
  else
    // exact
    factor.c := B^.c;
end;

procedure d2a_unpack_float(const f: double; out minus: boolean;
  out result: TDIY_FP);   {$ifdef HASINLINE}inline;{$endif}
type
  TSplitFloat = packed record
    case byte of
      0: (f: double);
      1: (b: array[0..7] of byte);
      2: (w: array[0..3] of word);
      3: (d: array[0..1] of cardinal);
      4: (l: qword);
  end;
var
  doublebits: TSplitFloat;
begin
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
  // high and low cardinal are swapped when using the arm fpa
  doublebits.d[0] := TSplitFloat(f).d[1];
  doublebits.d[1] := TSplitFloat(f).d[0];
{$else not FPC_DOUBLE_HILO_SWAPPED}
  doublebits.f := f;
{$endif FPC_DOUBLE_HILO_SWAPPED}
{$ifdef endian_big}
  minus := (doublebits.b[0] and $80 <> 0);
  result.e := (doublebits.w[0] shr 4) and $7FF;
{$else endian_little}
  minus := (doublebits.b[7] and $80 <> 0);
  result.e := (doublebits.w[3] shr 4) and $7FF;
{$endif endian}
  result.f := doublebits.l and $000FFFFFFFFFFFFF;
end;

const
  C_FRAC2_BITS = 52;
  C_EXP2_BIAS = 1023;
  C_DIY_FP_Q = 64;
  C_GRISU_ALPHA = -61;
  C_GRISU_GAMMA = 0;

  C_EXP2_SPECIAL = C_EXP2_BIAS * 2 + 1;
  C_MANT2_INTEGER = qword(1) shl C_FRAC2_BITS;

type
  TAsciiDigits = array[0..39] of byte;
  PAsciiDigits = ^TAsciiDigits;

// convert unsigned integers into decimal digits

{$ifdef FPC_64} // leverage efficient FPC 64-bit division as mul reciprocal

function d2a_gen_digits_64(buf: PAsciiDigits; x: qword): PtrInt;
var
  tab: PWordArray;
  P: PAnsiChar;
  c100: qword;
begin
  tab := @TwoDigitByteLookupW; // 0..99 value -> two byte digits (00..99)
  P := PAnsiChar(@buf[24]); // append backwards
  repeat
    if x >= 100 then
    begin
      dec(P, 2);
      c100 := x div 100;
      dec(x, c100 * 100);
      PWord(P)^ := tab[x]; // 2 digits per loop
      if c100 = 0 then
        break;
      x := c100;
      continue;
    end;
    if x < 10 then
    begin
      dec(P);
      P^ := AnsiChar(x); // 0..9
      break;
    end;
    dec(P, 2);
    PWord(P)^ := tab[x]; // 10..99
    break;
  until false;
  PQWordArray(buf)[0] := PQWordArray(P)[0]; // faster than MoveSmall(P,buf,result)
  PQWordArray(buf)[1] := PQWordArray(P)[1];
  PQWordArray(buf)[2] := PQWordArray(P)[2];
  result := PAnsiChar(@buf[24]) - P;
end;

{$else not FPC_64} // use three 32-bit groups of digit

function d2a_gen_digits_32(buf: PAsciiDigits; x: dword; pad_9zero: boolean): PtrInt;
const
  digits: array[0..9] of cardinal = (
    0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
var
  n: PtrInt;
  m: cardinal;
  {$ifdef FPC}
  z: cardinal;
  {$else}
  d100: TDiv100Rec;
  {$endif FPC}
  tab: PWordArray;
begin
  // Calculate amount of digits
  if x = 0 then
    n := 0  // emit nothing if padding is not required
  else
  begin
    n := integer((BSRdword(x) + 1) * 1233) shr 12;
    if x >= digits[n] then
      inc(n);
  end;
  if pad_9zero and (n < 9) then
    n := 9;
  result := n;
  if n = 0 then
    exit;
  // Emit digits
  dec(PByte(buf));
  tab := @TwoDigitByteLookupW;
  m := x;
  while (n >= 2) and (m <> 0) do
  begin
    dec(n);
    {$ifdef FPC} // FPC will use fast mul reciprocal
    z := m div 100; // compute two 0..9 digits
    PWord(@buf[n])^ := tab^[m - z * 100];
    m := z;
    {$else}
    Div100(m, d100); // our asm is faster than Delphi div operation
    PWord(@buf[n])^ := tab^[d100.M];
    m := d100.D;
    {$endif FPC}
    dec(n);
  end;
  if n = 0 then
    exit;
  if m <> 0 then
  begin
    if m > 9 then
      m := m mod 10; // compute last 0..9 digit
    buf[n] := m;
    dec(n);
    if n = 0 then
      exit;
  end;
  repeat
    buf[n] := 0; // padding with 0
    dec(n);
  until n = 0;
end;

function d2a_gen_digits_64(buf: PAsciiDigits; const x: qword): PtrInt;
var
  n_digits: PtrInt;
  temp: qword;
  splitl, splitm, splith: cardinal;
begin
  // Split X into 3 unsigned 32-bit integers; lower two should be < 10 digits long
  n_digits := 0;
  if x < 1000000000 then
    splitl := x
  else
  begin
    temp := x div 1000000000;
    splitl := x - temp * 1000000000;
    if temp < 1000000000 then
      splitm := temp
    else
    begin
      splith := temp div 1000000000;
      splitm := cardinal(temp) - splith * 1000000000;
      n_digits := d2a_gen_digits_32(buf, splith, false); // Generate hi digits
    end;
    inc(n_digits, d2a_gen_digits_32(@buf[n_digits], splitm, n_digits <> 0));
  end;
  // Generate digits
  inc(n_digits, d2a_gen_digits_32(@buf[n_digits], splitl, n_digits <> 0));
  result := n_digits;
end;

{$endif FPC_64}

// Performs digit sequence rounding, returns decimal point correction
function d2a_round_digits(var buf: TAsciiDigits; var n_current: integer;
  n_max: PtrInt; half_round_to_even: boolean = true): PtrInt;
var
  n: PtrInt;
  dig_round, dig_sticky: byte;
  {$ifdef GRISU1_F2A_AGRESSIVE_ROUNDUP}
  i: PtrInt;
  {$endif GRISU1_F2A_AGRESSIVE_ROUNDUP}
begin
  result := 0;
  n := n_current;
  n_current := n_max;
  // Get round digit
  dig_round := buf[n_max];
{$ifdef GRISU1_F2A_AGRESSIVE_ROUNDUP}
  // Detect if rounding-up the second last digit turns the "dig_round"
  // into "5"; also make sure we have at least 1 digit between "dig_round"
  // and the second last.
  if not half_round_to_even then
    if (dig_round = 4) and
       (n_max < n - 3) then
      if buf[n - 2] >= 8 then // somewhat arbitrary...
      begin
        // check for only "9" are in between
        i := n - 2;
        repeat
          dec(i);
        until (i = n_max) or
              (buf[i] <> 9);
        if i = n_max then
          // force round-up
          dig_round := 9; // any value ">=5"
      end;
{$endif GRISU1_F2A_AGRESSIVE_ROUNDUP}
  if dig_round < 5 then
    exit;
  // Handle "round half to even" case
  if (dig_round = 5) and
     half_round_to_even and
     ((n_max = 0) or
      (buf[n_max - 1] and 1 = 0)) then
  begin
    // even and a half: check if exactly the half
    dig_sticky := 0;
    while (n > n_max + 1) and (dig_sticky = 0) do
    begin
      dec(n);
      dig_sticky := buf[n];
    end;
    if dig_sticky = 0 then
      exit; // exactly a half -> no rounding is required
  end;
  // Round-up
  while n_max > 0 do
  begin
    dec(n_max);
    inc(buf[n_max]);
    if buf[n_max] < 10 then
    begin
      // no more overflow: stop now
      n_current := n_max + 1;
      exit;
    end;
    // continue rounding
  end;
  // Overflow out of the 1st digit, all n_max digits became 0
  buf[0] := 1;
  n_current := 1;
  result := 1;
end;

// format the number in the fixed-point representation
procedure d2a_return_fixed(str: PAnsiChar; minus: boolean;
  var digits: TAsciiDigits; n_digits_have, fixed_dot_pos, frac_digits: integer);
var
  p: PAnsiChar;
  d: PByte;
  cut_digits_at, n_before_dot, n_before_dot_pad0, n_after_dot_pad0,
  n_after_dot, n_tail_pad0: integer;
begin
  // Round digits if necessary
  cut_digits_at := fixed_dot_pos + frac_digits;
  if cut_digits_at < 0 then
    // zero
    n_digits_have := 0
  else if cut_digits_at < n_digits_have then
    // round digits
    inc(fixed_dot_pos, d2a_round_digits(digits, n_digits_have, cut_digits_at
      {$ifdef GRISU1_F2A_HALF_ROUNDUP}, false {$endif} ));
  // Before dot: digits, pad0
  if (fixed_dot_pos <= 0) or
     (n_digits_have = 0) then
  begin
    n_before_dot := 0;
    n_before_dot_pad0 := 1;
  end
  else if fixed_dot_pos > n_digits_have then
  begin
    n_before_dot := n_digits_have;
    n_before_dot_pad0 := fixed_dot_pos - n_digits_have;
  end
  else
  begin
    n_before_dot := fixed_dot_pos;
    n_before_dot_pad0 := 0;
  end;
  // After dot: pad0, digits, pad0
  if fixed_dot_pos < 0 then
    n_after_dot_pad0 := -fixed_dot_pos
  else
    n_after_dot_pad0 := 0;
  if n_after_dot_pad0 > frac_digits then
    n_after_dot_pad0 := frac_digits;
  n_after_dot := n_digits_have - n_before_dot;
  n_tail_pad0 := frac_digits - n_after_dot - n_after_dot_pad0;
  p := str + 1;
  // Sign
  if minus then
  begin
    p^ := '-';
    inc(p);
  end;
  // integer significant digits
  d := @digits;
  if n_before_dot > 0 then
    repeat
      p^ := AnsiChar(d^ + ord('0'));
      inc(p);
      inc(d);
      dec(n_before_dot);
    until n_before_dot = 0;
  // integer 0-padding
  if n_before_dot_pad0 > 0 then
    repeat
      p^ := '0';
      inc(p);
      dec(n_before_dot_pad0);
    until n_before_dot_pad0 = 0;
  // Fractional part
  if frac_digits <> 0 then
  begin
    // Dot
    p^ := '.';
    inc(p);
    // Pre-fraction 0-padding
    if n_after_dot_pad0 > 0 then
      repeat
        p^ := '0';
        inc(p);
        dec(n_after_dot_pad0);
      until n_after_dot_pad0 = 0;
    // Fraction significant digits
    if n_after_dot > 0 then
      repeat
        p^ := AnsiChar(d^ + ord('0'));
        inc(p);
        inc(d);
        dec(n_after_dot);
      until n_after_dot = 0;
    // Tail 0-padding
    if n_tail_pad0 > 0 then
      repeat
        p^ := '0';
        inc(p);
        dec(n_tail_pad0);
      until n_tail_pad0 = 0;
  end;
  // Store length
  str[0] := AnsiChar(p - str - 1);
end;

// formats the number as exponential representation
procedure d2a_return_exponential(str: PAnsiChar; minus: boolean;
  digits: PByte; n_digits_have, n_digits_req, d_exp: PtrInt);
var
  p, exp: PAnsiChar;
begin
  p := str + 1;
  // Sign
  if minus then
  begin
    p^ := '-';
    inc(p);
  end;
  // integer part
  if n_digits_have > 0 then
  begin
    p^ := AnsiChar(digits^ + ord('0'));
    dec(n_digits_have);
  end
  else
    p^ := '0';
  inc(p);
  // Dot
  if n_digits_req > 1 then
  begin
    p^ := '.';
    inc(p);
  end;
  // Fraction significant digits
  if n_digits_req < n_digits_have then
    n_digits_have := n_digits_req;
  if n_digits_have > 0 then
  begin
    repeat
      inc(digits);
      p^ := AnsiChar(digits^ + ord('0'));
      inc(p);
      dec(n_digits_have);
    until n_digits_have = 0;
    while p[-1] = '0' do
      dec(p); // trim #.###00000 -> #.###
    if p[-1] = '.' then
      dec(p); // #.0 -> #
  end;
  // Exponent designator
  p^ := 'E';
  inc(p);
  // Exponent sign (+ is not stored, as in Delphi)
  if d_exp < 0 then
  begin
    p^ := '-';
    d_exp := -d_exp;
    inc(p);
  end;
  // Exponent digits
  exp := pointer(SmallUInt32Utf8[d_exp]); // 0..999 range is fine
  PCardinal(p)^ := PCardinal(exp)^;
  inc(p, PStrLen(exp - _STRLEN)^);
  // Store length
  str[0] := AnsiChar(p - str - 1);
end;

/// set one of special results with proper sign
procedure d2a_return_special(str: PAnsiChar; sign: integer;
  const spec: shortstring);
begin
  // Compute length
  str[0] := spec[0];
  if sign <> 0 then
    inc(str[0]);
  inc(str);
  // Sign
  if sign <> 0 then
  begin
    if sign > 0 then
      str^ := '+'
    else
      str^ := '-';
    inc(str);
  end;
  // Special text (3 chars)
  PCardinal(str)^ := PCardinal(@spec[1])^;
end;


// Calculates the exp10 of a factor required to bring the binary exponent
// of the original number into selected [ alpha .. gamma ] range:
// result := ceiling[ ( alpha - e ) * log10(2) ]
function d2a_k_comp(e, alpha{, gamma}: integer): integer;
var
  dexp: double;
const
  D_LOG10_2: double = 0.301029995663981195213738894724493027; // log10(2)
var
  x, n: integer;
begin
  x := alpha - e;
  dexp := x * D_LOG10_2;
  // ceil( dexp )
  n := trunc(dexp);
  if x > 0 then
    if dexp <> n then
      inc(n); // round-up
  result := n;
end;

procedure DoubleToAscii(min_width, frac_digits: integer; const v: double;
  str: PAnsiChar);
var
  w, D: TDIY_FP;
  c_mk: TDIY_FP_Power_of_10;
  n, mk, dot_pos, n_digits_need, n_digits_have: integer;
  n_digits_req, n_digits_sci: integer;
  minus: boolean;
  fl, one_maskl: qword;
  one_e: integer;
  {$ifdef CPU32}
  one_mask, f: cardinal; // run a 2nd loop with 32-bit range
  {$endif CPU32}
  buf: TAsciiDigits;
begin
  // Limit parameters
  if frac_digits > 216 then
    frac_digits := 216; // Delphi compatible
  if min_width <= C_NO_MIN_WIDTH then
    min_width := -1 // no minimal width
  else if min_width < 0 then
    min_width := 0; // minimal width is as short as possible
  // Format profile: select "n_digits_need" (and "n_digits_exp")
  n_digits_req := nDig_mantissa;
  // number of digits to be calculated by Grisu
  n_digits_need := nDig_mantissa;
  if n_digits_req < n_digits_need then
    n_digits_need := n_digits_req;
  // number of mantissa digits to be printed in exponential notation
  if min_width < 0 then
    n_digits_sci := n_digits_req
  else
  begin
    n_digits_sci := min_width -1 {sign} -1 {dot} -1 {E} -1 {E-sign} - nDig_exp10;
    if n_digits_sci < 2 then
      n_digits_sci := 2; // at least 2 digits
    if n_digits_sci > n_digits_req then
      n_digits_sci := n_digits_req; // at most requested by real_type
  end;
  // Float -> DIY_FP
  d2a_unpack_float(v, minus, w);
  // Handle Zero
  if (w.e = 0) and
     (w.f = 0) then
  begin
    {$ifdef GRISU1_F2A_ZERONOFRACT}
    PWord(str)^ := 1 + ord('0') shl 8; // just return '0'
    {$else}
    if frac_digits >= 0 then
      d2a_return_fixed(str, minus, buf, 0, 1, frac_digits)
    else
      d2a_return_exponential(str, minus, @buf, 0, n_digits_sci, 0);
    {$endif GRISU1_F2A_ZERONOFRACT}
    exit;
  end;
  // Handle specials
  if w.e = C_EXP2_SPECIAL then
  begin
    n := 1 - ord(minus) * 2; // default special sign [-1|+1]
    if w.f = 0 then
      d2a_return_special(str, n, C_STR_INF)
    else
    begin
      // NaN [also pseudo-NaN, pseudo-Inf, non-normal for floatx80]
      {$ifdef GRISU1_F2A_NAN_SIGNLESS}
      n := 0;
      {$endif GRISU1_F2A_NAN_SIGNLESS}
      {$ifndef GRISU1_F2A_NO_SNAN}
      if (w.f and (C_MANT2_INTEGER shr 1)) = 0 then
        return_special(str, n, C_STR_SNAN)
      else
      {$endif GRISU1_F2A_NO_SNAN}
        d2a_return_special(str, n, C_STR_QNAN);
    end;
    exit;
  end;
  // Handle denormals
  if w.e <> 0 then
  begin
    // normal
    w.f := w.f or C_MANT2_INTEGER;
    n := C_DIY_FP_Q - C_FRAC2_BITS - 1;
  end
  else
  begin
    // denormal
    n := 63 - BSRqword(w.f);
    inc(w.e);
  end;
  // Final normalization
  w.f := w.f shl n;
  dec(w.e, C_EXP2_BIAS + n + C_FRAC2_BITS);
  // 1. Find the normalized "c_mk = f_c * 2^e_c" such that
  //    "alpha <= e_c + e_w + q <= gamma"
  // 2. Define "V = D * 10^k": multiply the input number by "c_mk", do not
  //    normalize to land into [ alpha .. gamma ]
  // 3. Generate digits ( n_digits_need + "round" )
  if (C_GRISU_ALPHA <= w.e) and
     (w.e <= C_GRISU_GAMMA) then
  begin
    // no scaling required
    D := w;
    c_mk.e10 := 0;
  end
  else
  begin
    mk := d2a_k_comp(w.e, C_GRISU_ALPHA{, C_GRISU_GAMMA} );
    d2a_diy_fp_cached_power10(mk, c_mk);
    // Let "D = f_D * 2^e_D := w (*) c_mk"
    if c_mk.e10 = 0 then
      D := w
    else
      d2a_diy_fp_multiply(w, c_mk.c, false, D);
  end;
  // Generate digits: integer part
  n_digits_have := d2a_gen_digits_64(@buf, D.f shr (-D.e));
  dot_pos := n_digits_have;
  // Generate digits: fractional part
  {$ifdef CPU32}
  f := 0; // "sticky" digit
  {$endif CPU32}
  if D.e < 0 then
    repeat
      // MOD by ONE
      one_e := D.e;
      one_maskl := qword(1) shl (-D.e) - 1;
      fl := D.f and one_maskl;
      // 64-bit loop (very efficient on x86_64, slower on i386)
      while {$ifdef CPU32} (one_e < -29) and {$endif}
            (n_digits_have < n_digits_need + 1) and (fl <> 0) do
      begin
        // f := f * 5;
        inc(fl, fl shl 2);
        // one := one / 2
        one_maskl := one_maskl shr 1;
        inc(one_e);
        // DIV by one
        buf[n_digits_have] := fl shr (-one_e);
        // MOD by one
        fl := fl and one_maskl;
        // next
        inc(n_digits_have);
      end;
      {$ifdef CPU32}
      if n_digits_have >= n_digits_need + 1 then
      begin
        // only "sticky" digit remains
        f := ord(fl <> 0);
        break;
      end;
      one_mask := cardinal(one_maskl);
      f := cardinal(fl);
      // 32-bit loop
      while (n_digits_have < n_digits_need + 1) and (f <> 0) do
      begin
        // f := f * 5;
        inc(f, f shl 2);
        // one := one / 2
        one_mask := one_mask shr 1;
        inc(one_e);
        // DIV by one
        buf[n_digits_have] := f shr (-one_e);
        // MOD by one
        f := f and one_mask;
        // next
        inc(n_digits_have);
      end;
      {$endif CPU32}
    until true;
  {$ifdef CPU32}
  // Append "sticky" digit if any
  if (f <> 0) and
     (n_digits_have >= n_digits_need + 1) then
  begin
    // single "<>0" digit is enough
    n_digits_have := n_digits_need + 2;
    buf[n_digits_need + 1] := 1;
  end;
  {$endif CPU32}
  // Round to n_digits_need using "roundTiesToEven"
  if n_digits_have > n_digits_need then
    inc(dot_pos, d2a_round_digits(buf, n_digits_have, n_digits_need));
  // Generate output
  if frac_digits >= 0 then
  begin
    d2a_return_fixed(str, minus, buf, n_digits_have, dot_pos - c_mk.e10,
      frac_digits);
    exit;
  end;
  if n_digits_have > n_digits_sci then
    inc(dot_pos, d2a_round_digits(buf, n_digits_have, n_digits_sci
      {$ifdef GRISU1_F2A_HALF_ROUNDUP}, false {$endif} ));
  d2a_return_exponential(str, minus, @buf, n_digits_have, n_digits_sci,
    dot_pos - c_mk.e10 - 1);
end;

function DoubleToShort(var S: ShortString; const Value: double): integer;
var
  valueabs: double;
begin
  valueabs := abs(Value);
  if (valueabs > DOUBLE_HI) or
     (valueabs < DOUBLE_LO) then
  begin
    // = str(Value,S) for scientific notation
    DoubleToAscii(C_NO_MIN_WIDTH, -1, Value, @S);
    result := ord(S[0]);
  end
  else
    result := DoubleToShortNoExp(S, Value);
end;

function DoubleToShortNoExp(var S: ShortString; const Value: double): integer;
begin
  DoubleToAscii(0, DOUBLE_PRECISION, Value, @S); // = str(Value:0:DOUBLE_PRECISION,S)
  result := FloatStringNoExp(@S, DOUBLE_PRECISION);
  S[0] := AnsiChar(result);
end;

{$else} // use regular Extended version

function DoubleToShort(var S: ShortString; const Value: double): integer;
begin
  result := ExtendedToShort(S, Value, DOUBLE_PRECISION);
end;

function DoubleToShortNoExp(var S: ShortString; const Value: double): integer;
begin
  result := ExtendedToShortNoExp(S, Value, DOUBLE_PRECISION);
end;

{$endif DOUBLETOSHORT_USEGRISU}

function DoubleToJson(var tmp: ShortString; Value: double;
  NoExp: boolean): PShortString;
begin
  if Value = 0 then
    result := @JSON_NAN[fnNumber]
  else
  begin
    if NoExp then
      DoubleToShortNoExp(tmp, Value)
    else
      DoubleToShort(tmp, Value);
    result := FloatToJsonNan(tmp);
  end;
end;

function DoubleToStr(Value: Double): RawUtf8;
begin
  DoubleToStr(Value, result);
end;

procedure DoubleToStr(Value: Double; var result: RawUtf8);
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := SmallUInt32Utf8[0]
  else
    FastSetString(result, @tmp[1], DoubleToShort(tmp{%H-}, Value));
end;

function FloatStrCopy(s, d: PUtf8Char): PUtf8Char;
var
  c: AnsiChar;
begin
  while s^=' ' do
    inc(s);
  c := s^;
  if (c='+') or
     (c='-') then
  begin
    inc(s);
    d^ := c;
    inc(d);
    c := s^;
  end;
  if c='.' then
  begin
    PCardinal(d)^ := ord('0')+ord('.')shl 8; // '.5' -> '0.5'
    inc(d,2);
    inc(s);
    c := s^;
  end;
  if (c >= '0') and
     (c <= '9') then
    repeat
      inc(s);
      d^ := c;
      inc(d);
      c := s^;
      if ((c >= '0') and
          (c <= '9')) or
         (c = '.') then
        continue;
      if (c <> 'e') and
         (c <> 'E') then
        break;
      inc(s);
      d^ := c; // 1.23e120 or 1.23e-45
      inc(d);
      c := s^;
      if c = '-' then
      begin
        inc(s);
        d^ := c;
        inc(d);
        c := s^;
      end;
      while (c >= '0') and
            (c <= '9') do
      begin
        inc(s);
        d^ := c;
        inc(d);
        c := s^;
      end;
      break;
    until false;
  result := d;
end;


function Char2ToByte(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
var
  B: PtrUInt;
begin
  B := ConvertHexToBinTab[ord(P[0])];
  if B <= 9 then
  begin
    Value := B;
    B := ConvertHexToBinTab[ord(P[1])];
    if B <= 9 then
    begin
      Value := Value * 10 + B;
      result := false;
      exit;
    end;
  end;
  result := true; // error
end;

function Char3ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
var
  B: PtrUInt;
begin
  B := ConvertHexToBinTab[ord(P[0])];
  if B <= 9 then
  begin
    Value := B;
    B := ConvertHexToBinTab[ord(P[1])];
    if B <= 9 then
    begin
      Value := Value * 10 + B;
      B := ConvertHexToBinTab[ord(P[2])];
      if B <= 9 then
      begin
        Value := Value * 10 + B;
        result := false;
        exit;
      end;
    end;
  end;
  result := true; // error
end;

function Char4ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
var
  B: PtrUInt;
begin
  B := ConvertHexToBinTab[ord(P[0])];
  if B <= 9 then
  begin
    Value := B;
    B := ConvertHexToBinTab[ord(P[1])];
    if B <= 9 then
    begin
      Value := Value * 10 + B;
      B := ConvertHexToBinTab[ord(P[2])];
      if B <= 9 then
      begin
        Value := Value * 10 + B;
        B := ConvertHexToBinTab[ord(P[3])];
        if B <= 9 then
        begin
          Value := Value * 10 + B;
          result := false;
          exit;
        end;
      end;
    end;
  end;
  result := true; // error
end;


procedure VariantToUtf8(const V: Variant; var result: RawUtf8;
  var wasString: boolean);
var
  tmp: TVarData;
  vt: cardinal;
begin
  wasString := false;
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty, varNull:
        result := NULL_STR_VAR;
      varSmallint:
        Int32ToUtf8(VSmallInt, result);
      varShortInt:
        Int32ToUtf8(VShortInt, result);
      varWord:
        UInt32ToUtf8(VWord, result);
      varLongWord:
        UInt32ToUtf8(VLongWord, result);
      varByte:
        result := SmallUInt32Utf8[VByte];
      varBoolean:
        if VBoolean then
          result := SmallUInt32Utf8[1]
        else
          result := SmallUInt32Utf8[0];
      varInteger:
        Int32ToUtf8(VInteger, result);
      varInt64:
        Int64ToUtf8(VInt64, result);
      varWord64:
        UInt64ToUtf8(VInt64, result);
      varSingle:
        ExtendedToStr(VSingle, SINGLE_PRECISION, result);
      varDouble:
        DoubleToStr(VDouble, result);
      varCurrency:
        Curr64ToStr(VInt64, result);
      varDate:
        begin
          if not Assigned(_VariantToUtf8DateTimeToIso8601) then
            raise ESynException.Create('VariantToUtf8(varDate) unsupported:' +
              ' please include mormot.core.json to your uses clause');
          _VariantToUtf8DateTimeToIso8601(VDate, 'T', result, {withms=}false);
          wasString := true;
        end;
      varString:
        begin
          wasString := true;
          {$ifdef HASCODEPAGE}
          AnyAnsiToUtf8(RawByteString(VString), result);
          {$else}
          result := RawUtf8(VString);
          {$endif HASCODEPAGE}
        end;
      {$ifdef HASVARUSTRING}
      varUString:
        begin
          wasString := true;
          RawUnicodeToUtf8(VAny, length(UnicodeString(VAny)), result);
        end;
      {$endif HASVARUSTRING}
      varOleStr:
        begin
          wasString := true;
          RawUnicodeToUtf8(VAny, length(WideString(VAny)), result);
        end;
    else
      if SetVariantUnRefSimpleValue(V, tmp{%H-}) then
        // simple varByRef
        VariantToUtf8(Variant(tmp), result, wasString)
      else if vt = varVariantByRef then
        // complex varByRef
        VariantToUtf8(PVariant(VPointer)^, result, wasString)
      else if vt = varStringByRef then
      begin
        wasString := true;
        {$ifdef HASCODEPAGE}
        AnyAnsiToUtf8(PRawByteString(VString)^, result);
        {$else}
        result := PRawUtf8(VString)^;
        {$endif HASCODEPAGE}
      end
      else if vt = varOleStrByRef then
      begin
        wasString := true;
        RawUnicodeToUtf8(pointer(PWideString(VAny)^),
          length(PWideString(VAny)^), result);
      end
      else
      {$ifdef HASVARUSTRING}
      if vt = varUStringByRef then
      begin
        wasString := true;
        RawUnicodeToUtf8(pointer(PUnicodeString(VAny)^),
          length(PUnicodeString(VAny)^), result);
      end
      else
      {$endif HASVARUSTRING}
        // not recognizable vt -> seralize as JSON to handle also custom types
        VariantSaveJson(V, twJsonEscape, result);
    end;
end;

function VariantToUtf8(const V: Variant): RawUtf8;
var
  wasString: boolean;
begin
  VariantToUtf8(V, result, wasString);
end;

function ToUtf8(const V: Variant): RawUtf8;
var
  wasString: boolean;
begin
  VariantToUtf8(V, result, wasString);
end;

function ToUtf8(const V: TVarData): RawUtf8; overload;
var
  wasString: boolean;
begin
  VariantToUtf8(PVariant(@V)^, result, wasString);
end;

function VariantToUtf8(const V: Variant; var Text: RawUtf8): boolean;
begin
  VariantToUtf8(V, Text, result);
end;

procedure VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8);
var
  temp: TTextWriterStackBuffer;
begin
  // not very fast, but creates valid JSON
  with DefaultTextWriterSerializer.CreateOwnedStream(temp) do
  try
    AddVariant(Value, Escape); // may encounter TObjectVariant -> WriteObject
    SetText(result);
  finally
    Free;
  end;
end;

function VariantSaveJson(const Value: variant; Escape: TTextWriterKind): RawUtf8;
begin
  VariantSaveJson(Value, Escape, result);
end;



{ ************ Text Formatting functions }

function VarRecAsChar(const V: TVarRec): integer;
begin
  case V.VType of
    vtChar:
      result := ord(V.VChar);
    vtWideChar:
      result := ord(V.VWideChar);
  else
    result := 0;
  end;
end;

function VarRecToInt64(const V: TVarRec; out value: Int64): boolean;
begin
  case V.VType of
    vtInteger:
      value := V.VInteger;
    vtInt64 {$ifdef FPC}, vtQWord{$endif}:
      value := V.VInt64^;
    vtBoolean:
      if V.VBoolean then
        value := 1
      else
        value := 0; // normalize
    vtVariant:
      value := V.VVariant^;
  else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function VarRecToDouble(const V: TVarRec; out value: double): boolean;
begin
  case V.VType of
    vtInteger:
      value := V.VInteger;
    vtInt64:
      value := V.VInt64^;
    {$ifdef FPC}
    vtQWord:
      value := V.VQWord^;
    {$endif FPC}
    vtBoolean:
      if V.VBoolean then
        value := 1
      else
        value := 0; // normalize
    vtExtended:
      value := V.VExtended^;
    vtCurrency:
      value := V.VCurrency^;
    vtVariant:
      value := V.VVariant^;
  else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function VarRecToTempUtf8(const V: TVarRec; var Res: TTempUtf8): integer;
var
  i: PtrInt;
  v64: Int64;
  isString: boolean;
label
  smlu32, none, done;
begin
  Res.TempRawUtf8 := nil; // no allocation by default - and avoid GPF
  case V.VType of
    vtString:
      if V.VString = nil then
        goto none
      else
      begin
        Res.Text := @V.VString^[1];
        Res.Len := ord(V.VString^[0]);
        goto done;
      end;
    vtAnsiString:
      begin
        // expect UTF-8 content
        Res.Text := pointer(V.VAnsiString);
        Res.Len := length(RawUtf8(V.VAnsiString));
        goto done;
      end;
    {$ifdef HASVARUSTRING}
    vtUnicodeString:
      RawUnicodeToUtf8(V.VPWideChar, length(UnicodeString(V.VUnicodeString)),
        RawUtf8(Res.TempRawUtf8));
    {$endif HASVARUSTRING}
    vtWideString:
      RawUnicodeToUtf8(V.VPWideChar, length(WideString(V.VWideString)),
        RawUtf8(Res.TempRawUtf8));
    vtPChar:
      begin
        // expect UTF-8 content
        Res.Text := V.VPointer;
        Res.Len := StrLen(V.VPointer);
        goto done;
      end;
    vtChar:
      begin
        Res.Temp[0] := V.VChar; // V may be on transient stack (alf: FPC)
        Res.Text := @Res.Temp;
        Res.Len := 1;
        goto done;
      end;
    vtPWideChar:
      RawUnicodeToUtf8(V.VPWideChar, StrLenW(V.VPWideChar),
        RawUtf8(Res.TempRawUtf8));
    vtWideChar:
      RawUnicodeToUtf8(@V.VWideChar, 1, RawUtf8(Res.TempRawUtf8));
    vtBoolean:
      begin
        if V.VBoolean then // normalize
          Res.Text := pointer(SmallUInt32Utf8[1])
        else
          Res.Text := pointer(SmallUInt32Utf8[0]);
        Res.Len := 1;
        goto done;
      end;
    vtInteger:
      begin
        i := V.VInteger;
        if PtrUInt(i) <= high(SmallUInt32Utf8) then
        begin
smlu32:   Res.Text := pointer(SmallUInt32Utf8[i]);
          Res.Len := PStrLen(Res.Text - _STRLEN)^;
        end
        else
        begin
          Res.Text := PUtf8Char(StrInt32(@Res.Temp[23], i));
          Res.Len := @Res.Temp[23] - Res.Text;
        end;
        goto done;
      end;
    vtInt64:
      {$ifdef CPU64}
      if PQWord(V.VInt64)^ <= high(SmallUInt32Utf8) then
      {$else}
      if (PCardinalArray(V.VInt64)^[0] <= high(SmallUInt32Utf8)) and
         (PCardinalArray(V.VInt64)^[1] = 0) then
      {$endif CPU64}
      begin
        i := V.VInt64^;
        goto smlu32;
      end
      else
      begin
        Res.Text := PUtf8Char(StrInt64(@Res.Temp[23], V.VInt64^));
        Res.Len := @Res.Temp[23] - Res.Text;
        goto done;
      end;
    {$ifdef FPC}
    vtQWord:
      if V.VQWord^ <= high(SmallUInt32Utf8) then
      begin
        i := V.VQWord^;
        goto smlu32;
      end
      else
      begin
        Res.Text := PUtf8Char(StrUInt64(@Res.Temp[23], V.VQWord^));
        Res.Len := @Res.Temp[23] - Res.Text;
        goto done;
      end;
    {$endif FPC}
    vtCurrency:
      begin
        Res.Text := @Res.Temp;
        Res.Len := Curr64ToPChar(V.VInt64^, Res.Temp);
        goto done;
      end;
    vtExtended:
      DoubleToStr(V.VExtended^, RawUtf8(Res.TempRawUtf8));
    vtPointer, vtInterface:
      begin
        Res.Text := @Res.Temp;
        Res.Len := DisplayMinChars(@V.VPointer, SizeOf(pointer)) * 2;
        BinToHexDisplayLower(@V.VPointer, @Res.Temp, Res.Len shr 1);
        goto done;
      end;
    vtClass:
      begin
        if V.VClass = nil then
          goto none;
        Res.Text := PPUtf8Char(PtrInt(PtrUInt(V.VClass)) + vmtClassName)^ + 1;
        Res.Len := ord(Res.Text[-1]);
        goto done;
      end;
    vtObject:
      begin
        if V.VObject = nil then
          goto none;
        Res.Text := PPUtf8Char(PPtrInt(V.VObject)^ + vmtClassName)^ + 1;
        Res.Len := ord(Res.Text[-1]);
        goto done;
      end;
    vtVariant:
      if VariantToInt64(V.VVariant^, v64) then
        if (PCardinalArray(@v64)^[0] <= high(SmallUInt32Utf8)) and
           (PCardinalArray(@v64)^[1] = 0) then
        begin
          i := v64;
          goto smlu32;
        end
        else
        begin
          Res.Text := PUtf8Char(StrInt64(@Res.Temp[23], v64));
          Res.Len := @Res.Temp[23] - Res.Text;
          goto done;
        end
      else
        VariantToUtf8(V.VVariant^, RawUtf8(Res.TempRawUtf8), isString);
  else
    begin
none: Res.Len := 0;
      goto done;
    end;
  end;
  Res.Text := Res.TempRawUtf8;
  Res.Len := length(RawUtf8(Res.TempRawUtf8));
done:
  result := Res.Len;
end;

procedure VarRecToUtf8(const V: TVarRec; var result: RawUtf8; wasString: PBoolean);
var
  isString: boolean;
label
  none;
begin
  isString := false;
  with V do
    case V.VType of
      vtString:
        begin
          isString := true;
          if VString = nil then
            goto none;
          FastSetString(result, @VString^[1], ord(VString^[0]));
        end;
      vtAnsiString:
        begin
          isString := true;
          result := RawUtf8(VAnsiString); // expect UTF-8 content
        end;
      {$ifdef HASVARUSTRING}
      vtUnicodeString:
        begin
          isString := true;
          RawUnicodeToUtf8(VUnicodeString,
            length(UnicodeString(VUnicodeString)), result);
        end;
      {$endif HASVARUSTRING}
      vtWideString:
        begin
          isString := true;
          RawUnicodeToUtf8(VWideString, length(WideString(VWideString)), result);
        end;
      vtPChar:
        begin
          isString := true;
          FastSetString(result, VPChar, StrLen(VPChar));
        end;
      vtChar:
        begin
          isString := true;
          FastSetString(result, PAnsiChar(@VChar), 1);
        end;
      vtPWideChar:
        begin
          isString := true;
          RawUnicodeToUtf8(VPWideChar, StrLenW(VPWideChar), result);
        end;
      vtWideChar:
        begin
          isString := true;
          RawUnicodeToUtf8(@VWideChar, 1, result);
        end;
      vtBoolean:
        if VBoolean then // normalize
          result := SmallUInt32Utf8[1]
        else
          result := SmallUInt32Utf8[0];
      vtInteger:
        Int32ToUtf8(VInteger, result);
      vtInt64:
        Int64ToUtf8(VInt64^, result);
      {$ifdef FPC}
      vtQWord:
        UInt64ToUtf8(VQWord^, result);
      {$endif FPC}
      vtCurrency:
        Curr64ToStr(VInt64^, result);
      vtExtended:
        DoubleToStr(VExtended^,result);
      vtPointer:
        begin
          isString := true;
          PointerToHex(VPointer, result);
        end;
      vtClass:
        begin
          isString := true;
          if VClass <> nil then
            ClassToText(VClass, result)
          else
none:       result := '';
        end;
      vtObject:
        if VObject <> nil then
          ClassToText(PClass(VObject)^, result)
        else
          goto none;
      vtInterface:
      {$ifdef HASINTERFACEASTOBJECT}
        if VInterface <> nil then
          ClassToText((IInterface(VInterface) as TObject).ClassType, result)
        else
          goto none;
      {$else}
        PointerToHex(VInterface,result);
      {$endif HASINTERFACEASTOBJECT}
      vtVariant:
        VariantToUtf8(VVariant^, result, isString);
    else
      goto none;
    end;
  if wasString <> nil then
    wasString^ := isString;
end;

function VarRecToUtf8IsString(const V: TVarRec; var value: RawUtf8): boolean;
begin
  VarRecToUtf8(V, value, @result);
end;

procedure VarRecToInlineValue(const V: TVarRec; var result: RawUtf8);
var
  wasString: boolean;
  tmp: RawUtf8;
begin
  VarRecToUtf8(V, tmp, @wasString);
  if wasString then
    QuotedStr(tmp, '"', result)
  else
    result := tmp;
end;

function FormatUtf8(const Format: RawUtf8; const Args: array of const): RawUtf8;
begin
  FormatUtf8(Format, Args, result);
end;

type
  // only supported token is %, with any const arguments
  TFormatUtf8 = object
    b: PTempUtf8;
    L, argN: PtrInt;
    blocks: array[0..63] of TTempUtf8; // to avoid most heap allocations
    procedure Parse(const Format: RawUtf8; const Args: array of const);
    procedure Write(Dest: PUtf8Char);
    function WriteMax(Dest: PUtf8Char; Max: PtrUInt): PUtf8Char;
  end;

procedure TFormatUtf8.Parse(const Format: RawUtf8; const Args: array of const);
var
  F, FDeb: PUtf8Char;
  c: PTempUtf8;
begin
  if length(Args) * 2 >= high(blocks) then
    raise ESynException.Create('FormatUtf8: too many args (max=32)');
  L := 0;
  argN := 0;
  c := @blocks;
  F := pointer(Format);
  repeat
    if F^ = #0 then
      break;
    if F^ <> '%' then
    begin
      FDeb := F;
      repeat
        inc(F);
      until (F^ = '%') or
            (F^ = #0);
      c^.Text := FDeb;
      c^.Len := F - FDeb;
      inc(L, c^.Len);
      c^.TempRawUtf8 := nil;
      inc(c);
      if F^ = #0 then
        break;
    end;
    inc(F); // jump '%'
    if argN <= high(Args) then
    begin
      inc(L, VarRecToTempUtf8(Args[argN], c^));
      if c^.Len > 0 then
        inc(c);
      inc(argN);
      if F^ = #0 then
        break;
    end
    else // no more available Args -> add all remaining text
    if F^ = #0 then
      break
    else
    begin
      c^.Text := F;
      c^.Len := length(Format) - (F - pointer(Format));
      inc(L, c^.Len);
      c^.TempRawUtf8 := nil;
      inc(c);
      break;
    end;
  until false;
  b := c;
end;

procedure TFormatUtf8.Write(Dest: PUtf8Char);
var
  d: PTempUtf8;
begin
  d := @blocks;
  repeat
    {$ifdef HASINLINE}
    MoveSmall(d^.Text, Dest, d^.Len);
    {$else}
    MoveFast(d^.Text^, Dest^, d^.Len);
    {$endif HASINLINE}
    inc(Dest, d^.Len);
    if d^.TempRawUtf8 <> nil then
      {$ifdef FPC}
      FastAssignNew(d^.TempRawUtf8);
      {$else}
      RawUtf8(d^.TempRawUtf8) := '';
      {$endif FPC}
    inc(d);
  until d = b;
end;

function TFormatUtf8.WriteMax(Dest: PUtf8Char; Max: PtrUInt): PUtf8Char;
var
  d: PTempUtf8;
begin
  if Max > 0 then
  begin
    inc(Max, PtrUInt(Dest));
    d := @blocks;
    if Dest <> nil then
      repeat
        if PtrUInt(Dest) + PtrUInt(d^.Len) > Max then
        begin
          // avoid buffer overflow
          {$ifdef HASINLINE}
          MoveSmall(d^.Text, Dest, Max - PtrUInt(Dest));
          {$else}
          MoveFast(d^.Text^, Dest^, Max - PtrUInt(Dest));
          {$endif HASINLINE}
          repeat
            if d^.TempRawUtf8 <> nil then
              {$ifdef FPC}
              FastAssignNew(d^.TempRawUtf8);
              {$else}
              RawUtf8(d^.TempRawUtf8) := '';
              {$endif FPC}
            inc(d);
          until d = b; // avoid memory leak
          result := PUtf8Char(Max);
          exit;
        end;
        {$ifdef HASINLINE}
        MoveSmall(d^.Text, Dest, d^.Len);
        {$else}
        MoveFast(d^.Text^, Dest^, d^.Len);
        {$endif HASINLINE}
        inc(Dest, d^.Len);
        if d^.TempRawUtf8 <> nil then
          {$ifdef FPC}
          FastAssignNew(d^.TempRawUtf8);
          {$else}
          RawUtf8(d^.TempRawUtf8) := '';
          {$endif FPC}
        inc(d);
      until d = b;
  end;
  result := Dest;
end;

procedure FormatUtf8(const Format: RawUtf8; const Args: array of const;
  out result: RawUtf8);
var
  process: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    result := Format
  else if PWord(Format)^ = ord('%') then    // optimize raw conversion
    VarRecToUtf8(Args[0], result)
  else
  begin
    process.Parse(Format, Args);
    if process.L <> 0 then
    begin
      FastSetString(result, nil, process.L);
      process.Write(pointer(result));
    end;
  end;
end;

procedure FormatShort(const Format: RawUtf8; const Args: array of const;
  var result: shortstring);
var
  process: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    SetString(result, PAnsiChar(pointer(Format)), length(Format))
  else
  begin
    process.Parse(Format, Args);
    result[0] := AnsiChar(process.WriteMax(@result[1], 255) - @result[1]);
  end;
end;

function FormatBuffer(const Format: RawUtf8; const Args: array of const;
  Dest: pointer; DestLen: PtrInt): PtrInt;
var
  process: TFormatUtf8;
begin
  if (Dest = nil) or
     (DestLen <= 0) then
  begin
    result := 0;
    exit; // avoid buffer overflow
  end;
  process.Parse(Format, Args);
  result := PtrUInt(process.WriteMax(Dest, DestLen)) - PtrUInt(Dest);
end;

function FormatToShort(const Format: RawUtf8;
  const Args: array of const): shortstring;
var
  process: TFormatUtf8;
begin
  process.Parse(Format, Args);
  result[0] := AnsiChar(process.WriteMax(@result[1], 255) - @result[1]);
end;

procedure FormatShort16(const Format: RawUtf8; const Args: array of const;
  var result: TShort16);
var
  process: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    SetString(result, PAnsiChar(pointer(Format)), length(Format))
  else
  begin
    process.Parse(Format, Args);
    result[0] := AnsiChar(process.WriteMax(@result[1], 16) - @result[1]);
  end;
end;

procedure FormatString(const Format: RawUtf8; const Args: array of const;
  out result: string);
var
  process: TFormatUtf8;
  temp: TSynTempBuffer; // will avoid most memory allocations
begin
  if (Format = '') or
     (high(Args) < 0) then
  begin
    // no formatting needed
    Utf8DecodeToString(pointer(Format), length(Format), result);
    exit;
  end;
  process.Parse(Format, Args);
  temp.Init(process.L);
  process.Write(temp.buf);
  Utf8DecodeToString(temp.buf, process.L, result);
  temp.Done;
end;

function FormatString(const Format: RawUtf8; const Args: array of const): string;
begin
  FormatString(Format, Args, result);
end;


function StringToConsole(const S: string): RawByteString;
begin
  result := Utf8ToConsole(StringToUtf8(S));
end;

procedure ConsoleWrite(const Fmt: RawUtf8; const Args: array of const;
  Color: TConsoleColor; NoLineFeed: boolean);
var
  tmp: RawUtf8;
begin
  FormatUtf8(Fmt, Args, tmp);
  ConsoleWrite(tmp, Color, NoLineFeed);
end;

{$I-}

procedure ConsoleShowFatalException(E: Exception; WaitForEnterKey: boolean);
begin
  ConsoleWrite(#13#10'Fatal exception ', cclightRed, true);
  ConsoleWrite('%', [E.ClassType], ccWhite, true);
  ConsoleWrite(' raised with message ', ccLightRed, true);
  ConsoleWrite('%', [E.Message], ccLightMagenta);
  TextColor(ccLightGray);
  if WaitForEnterKey then
  begin
    writeln(#13#10'Program will now abort');
    {$ifndef OSPOSIX}
    writeln('Press [Enter] to quit');
    ConsoleWaitForEnterKey;
    {$endif OSPOSIX}
  end;
  ioresult;
end;

{$I+}


{ ************ Resource and Time Functions }

procedure KB(bytes: Int64; out result: TShort16; nospace: boolean);
type
  TUnits = (kb, mb, gb, tb, pb, eb, b);
const
  TXT: array[{nospace:}boolean, TUnits] of RawUtf8 = (
    (' KB', ' MB', ' GB', ' TB', ' PB', ' EB', '% B'),
    ('KB',  'MB',  'GB',  'TB',  'PB',  'EB', '%B'));
var
  hi, rem: cardinal;
  u: TUnits;
begin
  if bytes < 1 shl 10 - (1 shl 10) div 10 then
  begin
    FormatShort16(TXT[nospace, b], [integer(bytes)], result);
    exit;
  end;
  if bytes < 1 shl 20 - (1 shl 20) div 10 then
  begin
    u := kb;
    rem := bytes;
    hi := bytes shr 10;
  end
  else if bytes < 1 shl 30 - (1 shl 30) div 10 then
  begin
    u := mb;
    rem := bytes shr 10;
    hi := bytes shr 20;
  end
  else if bytes < Int64(1) shl 40 - (Int64(1) shl 40) div 10 then
  begin
    u := gb;
    rem := bytes shr 20;
    hi := bytes shr 30;
  end
  else if bytes < Int64(1) shl 50 - (Int64(1) shl 50) div 10 then
  begin
    u := tb;
    rem := bytes shr 30;
    hi := bytes shr 40;
  end
  else if bytes < Int64(1) shl 60 - (Int64(1) shl 60) div 10 then
  begin
    u := pb;
    rem := bytes shr 40;
    hi := bytes shr 50;
  end
  else
  begin
    u := eb;
    rem := bytes shr 50;
    hi := bytes shr 60;
  end;
  rem := rem and 1023;
  if rem <> 0 then
    rem := rem div 102;
  if rem = 10 then
  begin
    rem := 0;
    inc(hi); // round up as expected by (most) human beings
  end;
  if rem <> 0 then
    FormatShort16('%.%%', [hi, rem, TXT[nospace, u]], result)
  else
    FormatShort16('%%', [hi, TXT[nospace, u]], result);
end;

function KB(bytes: Int64): TShort16;
begin
  KB(bytes, result, {nospace=}false);
end;

function KBNoSpace(bytes: Int64): TShort16;
begin
  KB(bytes, result, {nospace=}true);
end;

function KB(bytes: Int64; nospace: boolean): TShort16;
begin
  KB(bytes, result, nospace);
end;

function KB(const buffer: RawByteString): TShort16;
begin
  KB(length(buffer), result, {nospace=}false);
end;

procedure KBU(bytes: Int64; var result: RawUtf8);
var
  tmp: TShort16;
begin
  KB(bytes, tmp, {nospace=}false);
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

procedure K(value: Int64; out result: TShort16);
begin
  KB(Value, result, {nospace=}true);
  if result[0] <> #0 then
    dec(result[0]); // just trim last 'B' ;)
end;

function K(value: Int64): TShort16;
begin
  K(Value, result);
end;

function IntToThousandString(Value: integer;
  const ThousandSep: TShort4): shortstring;
var
  i, L, Len: cardinal;
begin
  str(Value, result);
  L := length(result);
  Len := L + 1;
  if Value < 0 then
    // ignore '-' sign
    dec(L, 2)
  else
    dec(L);
  for i := 1 to L div 3 do
    insert(ThousandSep, result, Len - i * 3);
end;

function MicroSecToString(Micro: QWord): TShort16;
begin
  MicroSecToString(Micro, result);
end;

procedure By100ToTwoDigitString(value: cardinal; const valueunit: shortstring;
  var result: TShort16);
var
  d100: TDiv100Rec;
begin
  if value < 100 then
    FormatShort16('0.%%', [UInt2DigitsToShortFast(value), valueunit], result)
  else
  begin
    Div100(value, d100{%H-});
    if d100.m = 0 then
      FormatShort16('%%', [d100.d, valueunit], result)
    else
      FormatShort16('%.%%', [d100.d, UInt2DigitsToShortFast(d100.m), valueunit], result);
  end;
end;

procedure _TimeToString(value: cardinal; const u: shortstring;
  var result: TShort16);
var
  d: cardinal;
begin
  d := value div 60;
  FormatShort16('%%%',
    [d, u, UInt2DigitsToShortFast(value - (d * 60))], result);
end;

procedure MicroSecToString(Micro: QWord; out result: TShort16);
begin
  if Int64(Micro) <= 0 then
    PCardinal(@result)^ := 3 + ord('0') shl 8 + ord('u') shl 16 + ord('s') shl 24
  else if Micro < 1000 then
    FormatShort16('%us', [Micro], result)
  else if Micro < 1000000 then
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 10, 'ms', result)
  else if Micro < 60000000 then
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 10000, 's', result)
  else if Micro < QWord(3600000000) then
    _TimeToString(
      {$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 1000000, 'm', result)
  else if Micro < QWord(86400000000 * 2) then
    _TimeToString(Micro div 60000000, 'h', result)
  else
    FormatShort16('%d', [Micro div QWord(86400000000)], result)
end;

procedure NanoSecToString(Nano: QWord; out result: TShort16);
begin
  if Int64(Nano) <= 0 then
    PCardinal(@result)^ := 3 + ord('0') shl 8 + ord('n') shl 16 + ord('s') shl 24
  else if Nano > 9900 then
    MicroSecToString(Nano div 1000, result)
  else if Nano >= 1000 then
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Nano)^ {$else} Nano {$endif} div 10, 'us', result)
  else
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Nano)^ {$else} Nano {$endif} * 100, 'ns', result);
end;


{ ************ ESynException class }

{ ESynException }

constructor ESynException.CreateUtf8(const Format: RawUtf8;
  const Args: array of const);
var
  msg: string;
begin
  FormatString(Format, Args, msg);
  inherited Create(msg);
end;

constructor ESynException.CreateLastOSError(const Format: RawUtf8;
  const Args: array of const);
var
  error: integer;
begin
  error := GetLastError;
  CreateUtf8(FormatUtf8('OSError 0x% [%] %', [CardinalToHexShort(error),
    StringToUtf8(SysErrorMessage(error)), Format]), Args);
end;

{$ifndef NOEXCEPTIONINTERCEPT}

function DefaultSynLogExceptionToStr(WR: TBaseWriter;
  const Context: TSynLogExceptionContext): boolean;
var
  extcode: cardinal;
  extnames: TPUtf8CharDynArray;
  i: PtrInt;
begin
  WR.AddClassName(Context.EClass);
  if (Context.ELevel = sllException) and
     (Context.EInstance <> nil) and
     (Context.EClass <> EExternalException) then
  begin
    extcode := Context.AdditionalInfo(extnames);
    if extcode <> 0 then
    begin
      WR.AddShorter(' 0x');
      WR.AddBinToHexDisplayLower(@extcode, SizeOf(extcode));
      for i := 0 to high(extnames) do
      begin
        {$ifdef OSWINDOWS}
        WR.AddShort(' [.NET/CLR unhandled ');
        {$else}
        WR.AddShort(' [unhandled ');
        {$endif OSWINDOWS}
        WR.AddNoJSONEScape(extnames[i]);
        WR.AddShort('Exception]');
      end;
    end;
    WR.Add(' ');
    if WR.ClassType = TBaseWriter then
      {$ifdef UNICODE}
      WR.AddOnSameLineW(pointer(Context.EInstance.Message), 0)
      {$else}
      WR.AddOnSameLine(pointer(Context.EInstance.Message))
      {$endif UNICODE}
    else
      WR.WriteObject(Context.EInstance);
  end
  else if Context.ECode <> 0 then
  begin
    WR.Add(' ', '(');
    WR.AddPointer(Context.ECode);
    WR.Add(')');
  end;
  result := false; // caller should append "at EAddr" and the stack trace
end;

function ESynException.CustomLog(WR: TBaseWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  if Assigned(TSynLogExceptionToStrCustom) then
    result := TSynLogExceptionToStrCustom(WR, Context)
  else
    result := DefaultSynLogExceptionToStr(WR, Context);
end;

{$endif NOEXCEPTIONINTERCEPT}


function StatusCodeToErrorMsg(Code: integer): shortstring;
var
  msg: RawUtf8;
begin
  StatusCodeToReason(Code, msg);
  FormatShort('HTTP Error % - %', [Code, msg], result);
end;


{ **************** Hexadecimal Text And Binary Conversion }

procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var {$ifdef CPUX86NOTPIC}
    tab: TAnsiCharToWord absolute TwoDigitsHexW;
    {$else}
    tab: ^TAnsiCharToWord; // faster on PIC, ARM and x86_64
    {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexW;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    repeat
      PWord(Hex)^ := tab[Bin^];
      inc(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
end;

function BinToHex(const Bin: RawByteString): RawUtf8;
var
  L: integer;
begin
  L := length(Bin);
  FastSetString(result, nil, L * 2);
  mormot.core.text.BinToHex(pointer(Bin), pointer(result), L);
end;

function BinToHex(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  FastSetString(result, nil, BinBytes * 2);
  mormot.core.text.BinToHex(Bin, pointer(result), BinBytes);
end;

function HexToBin(const Hex: RawUtf8): RawByteString;
var
  L: integer;
begin
  result := '';
  L := length(Hex);
  if L and 1 <> 0 then
    L := 0
  else // hexadecimal should be in char pairs
    L := L shr 1;
  SetLength(result, L);
  if not mormot.core.text.HexToBin(pointer(Hex), pointer(result), L) then
    result := '';
end;

function ByteToHex(P: PAnsiChar; Value: byte): PAnsiChar;
begin
  PWord(P)^ := TwoDigitsHexWB[Value];
  result := P + 2;
end;

procedure BinToHexDisplay(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var {$ifdef CPUX86NOTPIC}
    tab: TAnsiCharToWord absolute TwoDigitsHexW;
    {$else}
    tab: ^TAnsiCharToWord; // faster on PIC, ARM and x86_64
    {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexW;
  {$endif CPUX86NOTPIC}
  inc(Hex, BinBytes * 2);
  if BinBytes > 0 then
    repeat
      dec(Hex, 2);
      PWord(Hex)^ := tab[Bin^];
      inc(Bin);
      dec(BinBytes);
    until BinBytes = 0;
end;

function BinToHexDisplay(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  FastSetString(result, nil, BinBytes * 2);
  BinToHexDisplay(Bin, pointer(result), BinBytes);
end;

procedure BinToHexLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var {$ifdef CPUX86NOTPIC}
    tab: TAnsiCharToWord absolute TwoDigitsHexWLower;
    {$else}
    tab: ^TAnsiCharToWord; // faster on PIC, ARM and x86_64
    {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexWLower;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    repeat
      PWord(Hex)^ := tab[Bin^];
      inc(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
end;

function BinToHexLower(const Bin: RawByteString): RawUtf8;
begin
  BinToHexLower(pointer(Bin), length(Bin), result);
end;

procedure BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt; var result: RawUtf8);
begin
  FastSetString(result, nil, BinBytes * 2);
  BinToHexLower(Bin, pointer(result), BinBytes);
end;

function BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  BinToHexLower(Bin, BinBytes, result);
end;

procedure BinToHexDisplayLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var {$ifdef CPUX86NOTPIC}
     tab: TAnsiCharToWord absolute TwoDigitsHexWLower;
    {$else}
     tab: ^TAnsiCharToWord; // faster on PIC, ARM and x86_64
    {$endif CPUX86NOTPIC}
begin
  if (Bin = nil) or
     (Hex = nil) or
     (BinBytes <= 0) then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexWLower;
  {$endif CPUX86NOTPIC}
  inc(Hex, BinBytes * 2);
  repeat
    dec(Hex, 2);
    PWord(Hex)^ := tab[Bin^];
    inc(Bin);
    dec(BinBytes);
  until BinBytes = 0;
end;

function BinToHexDisplayLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  FastSetString(result, nil, BinBytes * 2);
  BinToHexDisplayLower(Bin, pointer(result), BinBytes);
end;

function BinToHexDisplayLowerShort(Bin: PAnsiChar; BinBytes: PtrInt): shortstring;
begin
  if BinBytes > 127 then
    BinBytes := 127;
  result[0] := AnsiChar(BinBytes * 2);
  BinToHexDisplayLower(Bin, @result[1], BinBytes);
end;

function BinToHexDisplayLowerShort16(Bin: Int64; BinBytes: PtrInt): TShort16;
begin
  if BinBytes > 8 then
    BinBytes := 8;
  result[0] := AnsiChar(BinBytes * 2);
  BinToHexDisplayLower(@Bin, @result[1], BinBytes);
end;

{$ifdef UNICODE}
function BinToHexDisplayFile(Bin: PAnsiChar; BinBytes: PtrInt): TFileName;
var
  temp: TSynTempBuffer;
begin
  temp.Init(BinBytes * 2);
  BinToHexDisplayLower(Bin, temp.Buf, BinBytes);
  Ansi7ToString(PWinAnsiChar(temp.buf), BinBytes * 2, string(result));
  temp.Done;
end;
{$else}
function BinToHexDisplayFile(Bin: PAnsiChar; BinBytes: PtrInt): TFileName;
begin
  SetString(result, nil, BinBytes * 2);
  BinToHexDisplayLower(Bin, pointer(result), BinBytes);
end;
{$endif UNICODE}

procedure PointerToHex(aPointer: Pointer; var result: RawUtf8);
begin
  FastSetString(result, nil, SizeOf(Pointer) * 2);
  BinToHexDisplay(@aPointer, pointer(result), SizeOf(Pointer));
end;

function PointerToHex(aPointer: Pointer): RawUtf8;
begin
  FastSetString(result, nil, SizeOf(aPointer) * 2);
  BinToHexDisplay(@aPointer, pointer(result), SizeOf(aPointer));
end;

function CardinalToHex(aCardinal: cardinal): RawUtf8;
begin
  FastSetString(result, nil, SizeOf(aCardinal) * 2);
  BinToHexDisplay(@aCardinal, pointer(result), SizeOf(aCardinal));
end;

function CardinalToHexLower(aCardinal: cardinal): RawUtf8;
begin
  FastSetString(result, nil, SizeOf(aCardinal) * 2);
  BinToHexDisplayLower(@aCardinal, pointer(result), SizeOf(aCardinal));
end;

function Int64ToHex(aInt64: Int64): RawUtf8;
begin
  FastSetString(result, nil, SizeOf(Int64) * 2);
  BinToHexDisplay(@aInt64, pointer(result), SizeOf(Int64));
end;

procedure Int64ToHex(aInt64: Int64; var result: RawUtf8);
begin
  FastSetString(result, nil, SizeOf(Int64) * 2);
  BinToHexDisplay(@aInt64, pointer(result), SizeOf(Int64));
end;

function PointerToHexShort(aPointer: Pointer): TShort16;
begin
  result[0] := AnsiChar(DisplayMinChars(@aPointer, SizeOf(aPointer)) * 2);
  BinToHexDisplayLower(@aPointer, @result[1], ord(result[0]) shr 1);
end;

function CardinalToHexShort(aCardinal: cardinal): TShort16;
begin
  result[0] := AnsiChar(SizeOf(aCardinal) * 2);
  BinToHexDisplay(@aCardinal, @result[1], SizeOf(aCardinal));
end;

function crc32cUtf8ToHex(const str: RawUtf8): RawUtf8;
begin
  result := CardinalToHex(crc32c(0, pointer(str), length(str)));
end;

function Int64ToHexShort(aInt64: Int64): TShort16;
begin
  result[0] := AnsiChar(SizeOf(aInt64) * 2);
  BinToHexDisplay(@aInt64, @result[1], SizeOf(aInt64));
end;

procedure Int64ToHexShort(aInt64: Int64; out result: TShort16);
begin
  result[0] := AnsiChar(SizeOf(aInt64) * 2);
  BinToHexDisplay(@aInt64, @result[1], SizeOf(aInt64));
end;

function Int64ToHexString(aInt64: Int64): string;
var
  temp: TShort16;
begin
  Int64ToHexShort(aInt64, temp);
  Ansi7ToString(@temp[1], ord(temp[0]), result);
end;

function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;
var
  b, c: byte;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  result := false; // return false if any invalid char
  if (Hex = nil) or
     (Bin = nil) then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
  begin
    inc(Bin, BinBytes - 1);
    repeat
      b := tab[Ord(Hex[0]) + 256]; // + 256 for shl 4
      c := tab[Ord(Hex[1])];
      if (b = 255) or
         (c = 255) then
        exit;
      Bin^ := b or c;
      dec(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
  end;
  result := true; // correct content in Hex
end;

function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
begin
  result := HexDisplayToBin(Hex, @aValue, SizeOf(aValue));
  if not result then
    aValue := 0;
end;

function HexDisplayToInt64(Hex: PAnsiChar; out aValue: Int64): boolean;
begin
  result := HexDisplayToBin(Hex, @aValue, SizeOf(aValue));
  if not result then
    aValue := 0;
end;

function HexDisplayToInt64(const Hex: RawByteString): Int64;
begin
  if not HexDisplayToBin(pointer(Hex), @result, SizeOf(result)) then
    result := 0;
end;

function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;
var
  b, c: byte;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  result := false; // return false if any invalid char
  if Hex = nil then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    if Bin <> nil then
      repeat
        b := tab[Ord(Hex[0]) + 256]; // + 256 for shl 4
        c := tab[Ord(Hex[1])];
        if (b = 255) or
           (c = 255) then
          exit;
        inc(Hex, 2);
        Bin^ := b or c;
        inc(Bin);
        dec(BinBytes);
      until BinBytes = 0
    else
      repeat // Bin=nil -> validate Hex^ input
        if (tab[Ord(Hex[0])] > 15) or
           (tab[Ord(Hex[1])] > 15) then
          exit;
        inc(Hex, 2);
        dec(BinBytes);
      until BinBytes = 0;
  result := true; // conversion OK
end;

procedure HexToBinFast(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
  c: byte;
begin
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    repeat
      c := tab[ord(Hex[0]) + 256]; // + 256 for shl 4
      c := tab[ord(Hex[1])] or c;
      Bin^ := c;
      inc(Hex, 2);
      inc(Bin);
      dec(BinBytes);
    until BinBytes = 0;
end;

function IsHex(const Hex: RawByteString; BinBytes: PtrInt): boolean;
begin
  result := (length(Hex) = BinBytes * 2) and
    mormot.core.text.HexToBin(pointer(Hex), nil, BinBytes);
end;

function HexToCharValid(Hex: PAnsiChar): boolean;
begin
  result := (ConvertHexToBin[Ord(Hex[0])] <= 15) and
            (ConvertHexToBin[Ord(Hex[1])] <= 15);
end;

function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char): boolean;
var
  b, c: byte;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if Hex <> nil then
  begin
    {$ifndef CPUX86NOTPIC}
    tab := @ConvertHexToBin;
    {$endif CPUX86NOTPIC}
    b := tab[ord(Hex[0]) + 256]; // + 256 for shl 4
    c := tab[ord(Hex[1])];
    if (b <> 255) and
       (c <> 255) then
    begin
      if Bin <> nil then
        Bin^ := AnsiChar(b + c);
      result := true;
      exit;
    end;
  end;
  result := false; // return false if any invalid char
end;

function HexToWideChar(Hex: PUtf8Char): cardinal;
var
  B: cardinal;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  result := tab[ord(Hex[0])];
  if result <= 15 then
  begin
    result := result shl 12;
    B := tab[ord(Hex[1])];
    if B <= 15 then
    begin
      B := B shl 8;
      inc(result, B);
      B := tab[ord(Hex[2])];
      if B <= 15 then
      begin
        B := B shl 4;
        inc(result, B);
        B := tab[ord(Hex[3])];
        if B <= 15 then
        begin
          inc(result, B);
          exit;
        end;
      end;
    end;
  end;
  result := 0;
end;

function OctToBin(Oct: PAnsiChar; Bin: PByte): PtrInt;
var
  c, v: byte;
label
  _nxt;
begin
  result := PtrInt(Bin);
  if Oct <> nil then
    repeat
      c := ord(Oct^);
      inc(Oct);
      if c <> ord('\') then
      begin
        if c = 0 then
          break;
_nxt:   Bin^ := c;
        inc(Bin);
        continue;
      end;
      c := ord(Oct^);
      inc(Oct);
      if c = ord('\') then
        goto _nxt;
      dec(c, ord('0'));
      if c > 3 then
        // stop at malformated input (includes #0)
        break;
      c := c shl 6;
      v := c;
      c := ord(Oct[0]);
      dec(c, ord('0'));
      if c > 7 then
        break;
      c := c shl 3;
      v := v or c;
      c := ord(Oct[1]);
      dec(c, ord('0'));
      if c > 7 then
        break;
      c := c or v;
      Bin^ := c;
      inc(Bin);
      inc(Oct, 2);
    until false;
  result := PAnsiChar(Bin) - PAnsiChar(result);
end;

function OctToBin(const Oct: RawUtf8): RawByteString;
var
  tmp: TSynTempBuffer;
  L: integer;
begin
  tmp.Init(length(Oct));
  try
    L := OctToBin(pointer(Oct), tmp.buf);
    SetString(result, PAnsiChar(tmp.buf), L);
  finally
    tmp.Done;
  end;
end;

function Int18ToChars3(Value: cardinal): RawUtf8;
begin
  FastSetString(result, nil, 3);
  PCardinal(result)^ := ((Value shr 12) and $3f) or
                        ((Value shr 6) and $3f) shl 8 or
                        (Value and $3f) shl 16 + $202020;
end;

procedure Int18ToChars3(Value: cardinal; var result: RawUtf8);
begin
  FastSetString(result, nil, 3);
  PCardinal(result)^ := ((Value shr 12) and $3f) or
                        ((Value shr 6) and $3f) shl 8 or
                        (Value and $3f) shl 16 + $202020;
end;

function Chars3ToInt18(P: pointer): cardinal;
begin
  result := PCardinal(P)^ - $202020;
  result := ((result shr 16) and $3f) or
            ((result shr 8) and $3f) shl 6 or
            (result and $3f) shl 12;
end;

function UInt3DigitsToUtf8(Value: cardinal): RawUtf8;
begin
  FastSetString(result, nil, 3);
  PWordArray(result)[0] := TwoDigitLookupW[Value div 10];
  PByteArray(result)[2] := (Value mod 10) + 48;
end;

function UInt4DigitsToUtf8(Value: cardinal): RawUtf8;
begin
  FastSetString(result, nil, 4);
  if Value > 9999 then
    Value := 9999;
  YearToPChar(Value, pointer(result));
end;

function UInt4DigitsToShort(Value: cardinal): TShort4;
begin
  result[0] := #4;
  if Value > 9999 then
    Value := 9999;
  YearToPChar(Value, @result[1]);
end;

function UInt3DigitsToShort(Value: cardinal): TShort4;
begin
  if Value > 999 then
    Value := 999;
  YearToPChar(Value, @result[0]);
  result[0] := #3; // override first digit
end;

function UInt2DigitsToShort(Value: byte): TShort4;
begin
  result[0] := #2;
  if Value > 99 then
    Value := 99;
  PWord(@result[1])^ := TwoDigitLookupW[Value];
end;

function UInt2DigitsToShortFast(Value: byte): TShort4;
begin
  result[0] := #2;
  PWord(@result[1])^ := TwoDigitLookupW[Value];
end;

function IP4Text(ip4: cardinal): shortstring;
var
  b: array[0..3] of byte absolute ip4;
begin
  if ip4 = 0 then
    result := ''
  else
    FormatShort('%.%.%.%', [b[0], b[1], b[2], b[3]], result);
end;

procedure IP6Text(ip6: PHash128; result: PShortString);
var
  i: integer;
  p: PByte;
  tab: ^TByteToWord;
begin
  if IsZero(ip6^) then
    result^ := ''
  else
  begin
    result^[0] := AnsiChar(39);
    p := @result^[1];
    tab := @TwoDigitsHexWBLower;
    for i := 0 to 7 do
    begin
      PWord(p)^ := tab[ip6^[0]];
      inc(p, 2);
      PWord(p)^ := tab[ip6^[1]];
      inc(p, 2);
      inc(PWord(ip6));
      p^ := ord(':');
      inc(p);
    end;
  end;
end;

function IP6Text(ip6: PHash128): shortstring;
begin
  IP6Text(ip6, @result);
end;

function IPToCardinal(aIP: PUtf8Char; out aValue: cardinal): boolean;
var
  i, c: cardinal;
  b: array[0..3] of byte;
begin
  aValue := 0;
  result := false;
  if (aIP = nil) or
     (IdemPChar(aIP, '127.0.0.1') and (aIP[9] = #0)) then
    exit;
  for i := 0 to 3 do
  begin
    c := GetNextItemCardinal(aIP, '.');
    if (c > 255) or
       ((aIP = nil) and (i < 3)) then
      exit;
    b[i] := c;
  end;
  if PCardinal(@b)^ <> $0100007f then
  begin
    aValue := PCardinal(@b)^;
    result := true;
  end;
end;

function IPToCardinal(const aIP: RawUtf8; out aValue: cardinal): boolean;
begin
  result := IPToCardinal(pointer(aIP), aValue);
end;

function IPToCardinal(const aIP: RawUtf8): cardinal;
begin
  IPToCardinal(pointer(aIP), result);
end;

function GuidToText(P: PUtf8Char; guid: PByteArray): PUtf8Char;
var
  i: PtrInt;
begin
  // encode as '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
  for i := 3 downto 0 do
  begin
    PWord(P)^ := TwoDigitsHexWB[guid[i]];
    inc(P, 2);
  end;
  inc(PByte(guid), 4);
  for i := 1 to 2 do
  begin
    P[0] := '-';
    PWord(P + 1)^ := TwoDigitsHexWB[guid[1]];
    PWord(P + 3)^ := TwoDigitsHexWB[guid[0]];
    inc(PByte(guid), 2);
    inc(P, 5);
  end;
  P[0] := '-';
  PWord(P + 1)^ := TwoDigitsHexWB[guid[0]];
  PWord(P + 3)^ := TwoDigitsHexWB[guid[1]];
  P[5] := '-';
  inc(PByte(guid), 2);
  inc(P, 6);
  for i := 0 to 5 do
  begin
    PWord(P)^ := TwoDigitsHexWB[guid[i]];
    inc(P, 2);
  end;
  result := P;
end;

function GuidToRawUtf8(const guid: TGUID): RawUtf8;
var
  P: PUtf8Char;
begin
  FastSetString(result, nil, 38);
  P := pointer(result);
  P^ := '{';
  GuidToText(P + 1, @guid)^ := '}';
end;

function ToUtf8(const guid: TGUID): RawUtf8;
begin
  FastSetString(result, nil, 36);
  GuidToText(pointer(result), @guid);
end;

function GuidToShort(const guid: TGUID): TGuidShortString;
begin
  GuidToShort(guid, result);
end;

procedure GuidToShort(const guid: TGUID; out dest: TGuidShortString);
begin
  dest[0] := #38;
  dest[1] := '{';
  dest[38] := '}';
  GuidToText(@dest[2], @guid);
end;

{$ifdef UNICODE}
function GuidToString(const guid: TGUID): string;
var
  tmp: array[0..35] of AnsiChar;
  i: integer;
begin
  GuidToText(tmp, @guid);
  SetString(result, nil, 38);
  PWordArray(result)[0] := ord('{');
  for i := 1 to 36 do
    PWordArray(result)[i] := ord(tmp[i - 1]); // no conversion for 7-bit Ansi
  PWordArray(result)[37] := ord('}');
end;
{$else}
function GuidToString(const guid: TGUID): string;
begin
  result := GuidToRawUtf8(guid);
end;
{$endif UNICODE}

function HexaToByte(P: PUtf8Char; var Dest: byte): boolean;
  {$ifdef HASINLINE}inline;{$endif}
var
  b, c: byte;
begin
  b := ConvertHexToBin[Ord(P[0]) + 256]; // + 256 for shl 4
  if b <> 255 then
  begin
    c := ConvertHexToBin[Ord(P[1])];
    if c <> 255 then
    begin
      Dest := b + c;
      result := true;
      exit;
    end;
  end;
  result := false; // mark error
end;

function TextToGuid(P: PUtf8Char; guid: PByteArray): PUtf8Char;
var
  i: PtrInt;
begin
  // decode from '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
  result := nil;
  for i := 3 downto 0 do
  begin
    if not HexaToByte(P, guid[i]) then
      exit;
    inc(P, 2);
  end;
  inc(PByte(guid), 4);
  for i := 1 to 2 do
  begin
    if (P^ <> '-') or
       not HexaToByte(P + 1, guid[1]) or
       not HexaToByte(P + 3, guid[0]) then
      exit;
    inc(P, 5);
    inc(PByte(guid), 2);
  end;
  if (P[0] <> '-') or
     (P[5] <> '-') or
     not HexaToByte(P + 1, guid[0]) or
     not HexaToByte(P + 3, guid[1]) then
    exit;
  inc(PByte(guid), 2);
  inc(P, 6);
  for i := 0 to 5 do
    if HexaToByte(P, guid[i]) then
      inc(P, 2)
    else
      exit;
  result := P;
end;

function StringToGuid(const text: string): TGUID;
{$ifdef UNICODE}
var
  tmp: array[0..35] of byte;
  i: integer;
{$endif UNICODE}
begin
  if (length(text) = 38) and
     (text[1] = '{') and
     (text[38] = '}') then
  begin
    {$ifdef UNICODE}
    for i := 0 to 35 do
      tmp[i] := PWordArray(text)[i + 1];
    if TextToGuid(@tmp, @result) <> nil then
    {$else}
    if TextToGuid(@text[2], @result) <> nil then
    {$endif UNICODE}
      exit; // conversion OK
  end;
  FillZero(PHash128(@result)^);
end;

function RawUtf8ToGuid(const text: RawByteString): TGUID;
begin
  // decode from '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}'
  if (length(text) <> 38) or
     (text[1] <> '{') or
     (text[38] <> '}') or
     (TextToGuid(@text[2], @result) = nil) then
   FillZero(PHash128(@result)^);
end;

function StreamToRawByteString(aStream: TStream): RawByteString;
var
  current, size: Int64;
begin
  result := '';
  if aStream = nil then
    exit;
  current := aStream.Position;
  if (current = 0) and
     aStream.InheritsFrom(TRawByteStringStream) then
  begin
    result := TRawByteStringStream(aStream).DataString; // fast COW
    exit;
  end;
  size := aStream.Size - current;
  if (size = 0) or
     (size > maxInt) then
    exit;
  SetLength(result, size);
  aStream.Read(pointer(result)^, size);
  aStream.Position := current;
end;

function RawByteStringToStream(const aString: RawByteString): TStream;
begin
  result := TRawByteStringStream.Create(aString);
end;

function ReadStringFromStream(S: TStream; MaxAllowedSize: integer): RawUtf8;
var
  L: integer;
begin
  result := '';
  L := 0;
  if (S.Read(L, 4) <> 4) or
     (L <= 0) or
     (L > MaxAllowedSize) then
    exit;
  FastSetString(result, nil, L);
  if S.Read(pointer(result)^, L) <> L then
    result := '';
end;

function WriteStringToStream(S: TStream; const Text: RawUtf8): boolean;
var
  L: integer;
begin
  L := length(Text);
  if L = 0 then
    result := S.Write(L, 4) = 4
  else
    {$ifdef FPC}
    result := (S.Write(L, 4) = 4) and
              (S.Write(pointer(Text)^, L) = L);
    {$else}
    result := S.Write(pointer(PtrInt(Text) - SizeOf(integer))^, L + 4) = L + 4;
    {$endif FPC}
end;



procedure InitializeUnit;
var
  i: PtrInt;
  v: byte;
  c: AnsiChar;
  P: PAnsiChar;
  B: PByteArray;
  tmp: array[0..15] of AnsiChar;
const
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';
  HexCharsLower: array[0..15] of AnsiChar = '0123456789abcdef';
begin
  // initialize internal lookup tables for various text conversions
  for i := 0 to 255 do
  begin
    TwoDigitsHex[i][1] := HexChars[i shr 4];
    TwoDigitsHex[i][2] := HexChars[i and $f];
    TwoDigitsHexLower[i][1] := HexCharsLower[i shr 4];
    TwoDigitsHexLower[i][2] := HexCharsLower[i and $f];
  end;
  {$ifndef EXTENDEDTOSHORT_USESTR}
  {$ifdef ISDELPHIXE}
  SettingsUS := TFormatSettings.Create(ENGLISH_LANGID);
  {$else}
  GetLocaleFormatSettings(ENGLISH_LANGID, SettingsUS);
  {$endif ISDELPHIXE}
  SettingsUS.DecimalSeparator := '.'; // value may have been overriden :(
  {$endif EXTENDEDTOSHORT_USESTR}
  {$ifdef DOUBLETOSHORT_USEGRISU}
  MoveFast(TwoDigitLookup[0], TwoDigitByteLookupW[0], SizeOf(TwoDigitLookup));
  for i := 0 to 199 do
    dec(PByteArray(@TwoDigitByteLookupW)[i], ord('0')); // '0'..'9' -> 0..9
  {$endif DOUBLETOSHORT_USEGRISU}
  FillcharFast(ConvertHexToBin[0], SizeOf(ConvertHexToBin), 255); // all to 255
  B := @ConvertHexToBin;
  v := 0;
  for i := ord('0') to ord('9') do
  begin
    B[i] := v;
    B[i + 256] := v shl 4;
    inc(v);
  end;
  for i := ord('A') to ord('F') do
  begin
    B[i] := v;
    B[i + 256] := v shl 4;
    B[i + (ord('a') - ord('A'))] := v;
    B[i + (ord('a') - ord('A') + 256)] := v shl 4;
    inc(v);
  end;
  for i := 0 to high(SmallUInt32Utf8) do
  begin
    P := StrUInt32(@tmp[15], i);
    FastSetString(SmallUInt32Utf8[i], P, @tmp[15] - P);
  end;
  for c := #0 to #127 do
  begin
    XML_ESC[c] := ord(c in [#0..#31, '<', '>', '&', '"', '''']);
    HTML_ESC[hfAnyWhere, c] := ord(c in [#0, '&', '"', '<', '>']);
    HTML_ESC[hfOutsideAttributes, c] := ord(c in [#0, '&', '<', '>']);
    HTML_ESC[hfWithinAttributes, c] := ord(c in [#0, '&', '"']);
  end;
end;


initialization
  InitializeUnit;

end.

