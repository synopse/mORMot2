/// Framework Core Low-Level Text Processing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.text;

{
  *****************************************************************************

   Text Processing functions shared by all framework units
    - CSV-like Iterations over Text Buffers
    - TTextWriter parent class for Text Generation
    - Numbers (integers or floats) and Variants to Text Conversion
    - Text Formatting Functions
    - ESynException class
    - HTTP/REST Common Headers Parsing (e.g. cookies)
    - Hexadecimal Text And Binary Conversion

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  types,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode;


{ ************ CSV-like Iterations over Text Buffers }

/// return true if IdemPChar(source,searchUp) matches, and retrieve the value item
// - typical use may be:
// ! if IdemPCharAndGetNextItem(P,
// !   'CONTENT-DISPOSITION: FORM-DATA; NAME="',Name,'"') then ...
function IdemPCharAndGetNextItem(var source: PUtf8Char; const searchUp: RawUtf8;
  var Item: RawUtf8; Sep: AnsiChar = #13): boolean;

/// return next CSV string from P until P = nil
function GetNextItem(var P: PUtf8Char; Sep: AnsiChar = ','): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return next CSV string from P until P = nil
procedure GetNextItem(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8); overload;

/// return next CSV string (unquoted if needed) from P until P = nil
procedure GetNextItem(var P: PUtf8Char; Sep, Quote: AnsiChar;
  var result: RawUtf8); overload;

/// return next CSV string from P until P = nil from several separator characters
// - returns the character which ended the result string, i.e. #0 or one of Sep
function GetNextItemMultiple(var P: PUtf8Char; const Sep: RawUtf8;
  var Next: RawUtf8): AnsiChar; overload;

/// return trimmed next CSV string from P until P = nil
procedure GetNextItemTrimed(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8);

/// return trimmed next CSV string buffer and length from P until P = nil
function GetNextItemTrimedBuffer(var P: PUtf8Char; Sep: AnsiChar;
  out Item: PUtf8Char): PtrInt;

/// return next CSV string buffer and length from P until P = nil
function GetNextItemBuffer(var P: PUtf8Char; Sep: AnsiChar; out Item: PUtf8Char): PtrInt;
  {$ifdef ASMX64}inline;{$endif}

/// return next CSV string buffer and length from P until P = nil
function GetNextItemBufferLen(var P: PUtf8Char; var PL: PtrInt; Sep: AnsiChar;
  out Item: PUtf8Char; TrimValue: boolean): PtrInt;

/// return trimmed next CSV string from P, ending value at #0 .. #13
// - typically usage is to parse HTTP headers
// - P=nil after call when P^ = #0 end of text is reached, or return P^ = #10
procedure GetNextItemTrimedLine(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8);

/// return trimmed next CSV string from P, ending value at #0 .. #13
// - as used internally by GetNextItemTrimedLine()
function GetNextItemTrimedLineBuffer(var P: PUtf8Char; Sep: AnsiChar;
  out Item: PUtf8Char): PtrInt;

/// return trimmed next CSV string from P until P = nil, ignoring any Escaped char
procedure GetNextItemTrimedEscaped(var P: PUtf8Char; Sep, Esc: AnsiChar;
  var result: RawUtf8);

/// return next CRLF separated value string from P, ending #10 or #13#10 trimmed
// - any kind of line feed (CRLF or LF) will be handled, on all operating systems
// - as used e.g. by TSynNameValue.InitFromCsv and TDocVariantData.InitFromPairs
// - P=nil after call when end of text is reached
procedure GetNextItemTrimedCRLF(var P: PUtf8Char; var result: RawUtf8);

/// return next CSV string from P until P = nil
// - this function returns the RTL string type of the compiler, and
// therefore can be used with ready to be displayed text (e.g. for the UI)
function GetNextItemString(var P: PChar; Sep: Char = ','): string;

/// extract a file extension from a file name, then compare with a comma
// separated list of extensions
// - e.g. GetFileNameExtIndex('test.log','exe,log,map')=1
// - will return -1 if no file extension match
// - will return any matching extension, starting count at 0
// - extension match is case-insensitive
// - see also SameExt() from mormot.core.os.pas
function GetFileNameExtIndex(const FileName, CsvExt: TFileName): integer;

/// return next CSV string from P until P = nil
// - output text would be trimmed from any left or right space
// - will always append a #0 terminator - excluded from Dest length (0..254)
procedure GetNextItemShortString(var P: PUtf8Char; Dest: PShortString;
  Sep: AnsiChar = ',');

/// fast version of several cascaded StringReplaceAll() as old=new,... parameters
function StringReplaceCsv(const S: RawUtf8; OldNewPatternPairs: PUtf8Char;
  CaseInsensitive: boolean = false): RawUtf8;

/// append some text lines with the supplied Values[]
// - if any Values[] item is '', no line is added
// - otherwise, appends 'Caption: Value', with Caption taken from CSV
procedure AppendCsvValues(const Csv: string; const Values: array of string;
  var result: string; const AppendBefore: string = EOL);

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
function GetNextItemHexDisplayToBin(var P: PUtf8Char; Bin: PByte;
  BinBytes: PtrInt; Sep: AnsiChar = ','): boolean;

type
  /// some stack-allocated zero-terminated character buffer
  // - as used by GetNextTChar64 or ConvertToBase64 lookup tables
  TChar64 = TTemp64;
  PChar64 = ^TChar64;

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
// - could be used to decode TTextWriter.AddBinToHexDisplayMinChars() output
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
// - this function return the RTL string type of the compiler, and
// therefore can be used with ready to be displayed text
function GetCsvItemString(P: PChar; Index: PtrUInt; Sep: Char = ','): string;

/// return first CSV string in the supplied UTF-8 content
function GetFirstCsvItem(const Csv: RawUtf8; Sep: AnsiChar = ','): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// return last CSV string in the supplied UTF-8 content
function GetLastCsvItem(const Csv: RawUtf8; Sep: AnsiChar = ','): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// quickly check if Value is in Csv with no temporary memory allocation
function CsvContains(const Csv, Value: RawUtf8; Sep: AnsiChar = ',';
  CaseSensitive: boolean = true): boolean; overload;

/// quickly check if Value is in Csv with no temporary memory allocation
function CsvContains(Csv, Value: PUtf8Char; ValueLen: PtrInt;
  Sep: AnsiChar; CaseSensitive, TrimValue: boolean): boolean; overload;

/// quickly check if Value is in Csv with no temporary memory allocation
function CsvContains(Csv, Value: PUtf8Char; CsvLen, ValueLen: PtrInt;
  Sep: AnsiChar; CaseSensitive, TrimValue: boolean): boolean; overload;

/// return the index of a Value in a CSV string
// - start at Index=0 for first one
// - return -1 if specified Value was not found in CSV items
function FindCsvIndex(Csv: PUtf8Char; const Value: RawUtf8; Sep: AnsiChar = ',';
  CaseSensitive: boolean = true; TrimValue: boolean = false): integer;

/// add the strings in the specified CSV text into a dynamic array of UTF-8 strings
// - warning: will add the strings, so List := nil may be needed before call
procedure CsvToRawUtf8DynArray(Csv: PUtf8Char; var List: TRawUtf8DynArray;
  Sep: AnsiChar = ','; TrimItems: boolean = false; AddVoidItems: boolean = false;
  Quote: AnsiChar = #0); overload;

/// add the strings in the specified CSV text into a dynamic array of UTF-8 strings
// - warning: will add the strings, so List := nil may be needed before call
procedure CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8;
  var List: TRawUtf8DynArray); overload;

/// convert the strings in the specified CSV text into a dynamic array of UTF-8 strings
function CsvToRawUtf8DynArray(const Csv: RawUtf8; const Sep: RawUtf8 = ',';
  const SepEnd: RawUtf8 = ''): TRawUtf8DynArray; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return the corresponding CSV text from a dynamic array of UTF-8 strings
function RawUtf8ArrayToCsv(const Values: TRawUtf8DynArray;
  const Sep: RawUtf8 = ','; Reverse: boolean = false): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// return the corresponding CSV text from a dynamic array of UTF-8 strings
procedure RawUtf8ArrayToCsvVar(const Values: TRawUtf8DynArray; var Csv: RawUtf8;
  const Sep: RawUtf8 = ','; Reverse: boolean = false);
  {$ifdef HASINLINE}inline;{$endif}

/// return the corresponding CSV text from an array of UTF-8 strings
// - using a Python-like friendly syntax
// - we could not use plain Join() overload due to a Delphi compiler limitation
function JoinCsv(const Sep: RawUtf8; const Values: array of RawUtf8;
  Reverse: boolean = false): RawUtf8;

/// low-level CSV generator e.g. for Join(), RawUtf8ArrayToCsv() and TRawUtf8List.GetText
procedure PRawUtf8ToCsv(v: PPUtf8Char; n: integer; const sep: RawUtf8;
  Reverse: boolean; var result: RawUtf8);

type
  TVariantToTempUtf8Flags = set of (
    vfNoAlloc, vfNoComplex, vfNullAsVoid, vfBooleanAsInt, vfDateAsFloat);

/// return the corresponding CSV text from an array of variants using TTempUtf8
procedure PVariantToCsv(v: PVariant; n: integer; const sep: RawUtf8;
  Reverse: boolean; var result: RawUtf8; flags: TVariantToTempUtf8Flags = []);

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

/// recognize #9 ';' or ',' as separator in a CSV text
// - to implement a separator-tolerant CSV parser
function CsvGuessSeparator(const Csv: RawUtf8): AnsiChar;

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
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;

/// return the corresponding CSV text from a dynamic array of 32-bit integer
// - you can set some custom Prefix and Suffix text
function IntegerDynArrayToCsv(const Values: TIntegerDynArray;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return the corresponding CSV text from a dynamic array of 64-bit integers
// - you can set some custom Prefix and Suffix text
function Int64DynArrayToCsv(Values: PInt64Array; ValuesCount: integer;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;

/// return the corresponding CSV text from a dynamic array of 64-bit integers
// - you can set some custom Prefix and Suffix text
function Int64DynArrayToCsv(const Values: TInt64DynArray;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}


{ ************ TTextWriter parent class for Text Generation }

type
  /// event signature for TTextWriter.OnFlushToStream callback
  TOnTextWriterFlush = procedure(Text: PUtf8Char; Len: PtrInt) of object;

  /// defines how text is to be added into TTextWriter / TJsonWriter
  // - twNone will write the supplied text with no escaping
  // - twJsonEscape will properly escape " and \ as expected by JSON
  // - twOnSameLine will convert any line feeds or control chars into spaces
  TTextWriterKind = (
    twNone,
    twJsonEscape,
    twOnSameLine);

  /// available options for TTextWriter / TJsonWriter output rendering format
  // - TTextWriter.WriteObject() method behavior would be set via their own
  // TTextWriterWriteObjectOptions, and work in conjunction with those settings
  // - by default, custom serializers set via TRttiJson.RegisterCustomSerializer()
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
  // may default to the deprecated global TTextWriter.SetDefaultEnumTrim setting
  // - twoEndOfLineCRLF would reflect the TEchoWriter.EndOfLineCRLF property
  // - twoIgnoreDefaultInRecord will force custom record serialization to avoid
  // writing the fields with default values, i.e. enable soWriteIgnoreDefault
  // when published properties are serialized
  // - twoDateTimeWithZ appends an ending 'Z' to TDateTime/TDateTimeMS values
  // - twoNonExpandedArrays will force the 'non expanded' optimized JSON layout
  // for array of records or classes, ignoring other formatting options:
  // $ {"fieldCount":2,"values":["f1","f2","1v1",1v2,"2v1",2v2...],"rowCount":20}
  // - twoIndentSpaces will indent with two spaces instead of two tabs
  TTextWriterOption = (
    twoEnumSetsAsTextInRecord,
    twoEnumSetsAsBooleanInRecord,
    twoFullSetsAsStar,
    twoTrimLeftEnumSets,
    twoForceJsonExtended,
    twoForceJsonStandard,
    twoEndOfLineCRLF,
    twoIgnoreDefaultInRecord,
    twoDateTimeWithZ,
    twoNonExpandedArrays,
    twoIndentSpaces);

  /// options set for a TTextWriter / TJsonWriter instance
  // - allows to override e.g. AddRecordJson() and AddDynArrayJson() behavior;
  // or set global process customization for a TTextWriter
  TTextWriterOptions = set of TTextWriterOption;

  /// available internal flags defining TTextWriter / TJsonWriter process
  // - twfDestIsOwnedStream is set if the associated TStream is owned by the
  // TTextWriter instance - as a TRawByteStringStream with twfRawByteStringStream
  // - twfDestIsShortString is set by CreateOwnedShort or with a TLocalWriter
  // - twfDestIsRawUtf8 is set by CreateOwnedStream(TTextWriterStackBuffer)
  // - twfFlushNoAutoResize would forbid FlushToStream to adjust the internal
  // memory buffer size - see TTextWriter.FlushToStreamNoAutoResize property
  // - twfNoWriteToStreamException let WriteToStream silently fail - use
  // TTextWriter.NoWriteToStreamException property to specify this option
  // - twfBufferIsOnStack would be set if the temporary buffer is external to
  // this instance, but specified at constructor, maybe from the stack
  TTextWriterFlag = (
    twfDestIsOwnedStream,
    twfRawByteStringStream,
    twfDestIsShortString,
    twfDestIsRawUtf8,
    twfFlushNoAutoResize,
    twfNoWriteToStreamException,
    twfBufferIsOnStack);

  /// internal flags used by a TTextWriter / TJsonWriter instance
  TTextWriterFlags = set of TTextWriterFlag;

  /// may be used to allocate on stack a 8KB work buffer for a TTextWriter
  // - via the TTextWriter.CreateOwnedStream overloaded constructor
  TTextWriterStackBuffer = TBuffer8K;
  PTextWriterStackBuffer = ^TTextWriterStackBuffer;

  /// available options for TTextWriter.WriteObject() method
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
  // - woDateTimeNullAsVoidString will store TDateTime = 0 as legacy "" content
  // - TTimeLog would be serialized as Int64, unless woTimeLogAsText is defined
  // - since TOrm.ID could be huge Int64 numbers, they may be truncated
  // on client side, e.g. to 53-bit range in JavaScript: you could define
  // woIDAsIDstr to append an additional "ID_str":"##########" field
  // - by default, RawBlob properties are serialized as null, unless
  // woRawBlobAsBase64/woRawByteStringAsBase64Magic are defined or a custom
  // serialization is used (e.g. TOrm)
  // - if woHideSensitivePersonalInformation is set, rcfSpi types (e.g. the
  // TObjectWithPassword.Password field) will be serialized as "***"
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
  // - woRttiMethodsLock paranoid setting will call TSynLockedWithRttiMethods
  // Lock/Unlock during serialization
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
    woDateTimeNullAsVoidString,
    woTimeLogAsText,
    woIDAsIDstr,
    woRawBlobAsBase64,
    woRawByteStringAsBase64Magic,
    woHideSensitivePersonalInformation,
    woObjectListWontStoreClassName,
    woDontStoreInherited,
    woInt64AsHex,
    woDontStoreVoid,
    woRttiMethodsLock);

  /// options set for TTextWriter.WriteObject() method
  TTextWriterWriteObjectOptions = set of TTextWriterWriteObjectOption;
  /// two sets of TTextWriter.WriteObject() options
  TTextWriterWriteObjectOptionsBoolean = array[boolean] of TTextWriterWriteObjectOptions;

  /// the potential places were TJsonWriter.AddHtmlEscape should process
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

  /// the JSON/JSON-like known formats supported by JsonReformat()
  // - all those formats are inter-operable within the JSON data model
  // - jsonCompact is the default standard machine-friendly single-line JSON
  // - jsonHumanReadable will add line feeds and #9 (tab) indentation, for a
  // more human-friendly result of a standard JSON content
  // - jsonUnquotedPropName will emit the jsonHumanReadable layout, but
  // with all property names being quoted only if necessary: this format
  // could be used e.g. for configuration files - this format, similar to the
  // one used in the MongoDB extended syntax, is NOT JSON compatible: do not
  // use it e.g. with AJAX clients, but is would be handled as expected by all
  // our units as valid JSON input, without previous correction
  // - jsonUnquotedPropNameCompact will emit single-line layout with unquoted
  // property names, which is the smallest data output within mORMot instances
  // - jsonC will keep and normalize comments whereas jsonHumanReadable won't
  // - json5 will emit unquoted names, but with a trailing , before } or ]
  // - jsonH for indented unquoted names and values, using LF as delimiter -
  // i.e. the .hjson "Human JSON" format, very suitable for config files
  // - jsonMorml is as unquoted, unindented and small as possible - resulting
  // "less is more Markup Language" - aka .morml - is still UTF-8 human readable
  // - by default we rely on UTF-8 encoding (which is mandatory in the RFC 8259)
  // but you can use jsonEscapeUnicode to produce pure 7-bit ASCII output,
  // with \u#### escape of non-ASCII chars, e.g. as default python json.dumps
  // - jsonNoEscapeUnicode replaces any \u#### pattern by pure UTF-8 output
  // - those features are not implemented in this unit, but in mormot.core.json
  TTextWriterJsonFormat = (
    jsonCompact,
    jsonHumanReadable,
    jsonUnquotedPropName,
    jsonUnquotedPropNameCompact,
    jsonC,
    json5,
    jsonH,
    jsonMorml,
    jsonEscapeUnicode,
    jsonNoEscapeUnicode);

  /// parent to T*Writer text processing classes, with the minimum set of methods
  // - use an internal buffer to be faster than naive string concatenation
  // - see TTextDateWriter in mormot.core.datetime for date/time methods
  // - see TJsonWriter in mormot.core.json for proper JSON support
  // - see TResultsWriter in mormot.db.core for SQL resultset export
  // - see TOrmWriter in mormot.orm.core for ORM oriented serialization
  // - note: mORMot 1.18 TTextWriter.RegisterCustomJSONSerializerFromText()
  // are moved into Rtti.RegisterFromText() as other RTTI-related methods
  TTextWriter = class
  protected // check TLocalWriter if you add some new fields to this base class
    fDest: pointer; // may be a TStream, a PShortString or a RawUtf8
    fOnFlushToStream: TOnTextWriterFlush;
    fTempBuf: PUtf8Char;
    fTempBufSize: integer;
    fHumanReadableLevel: integer;
    fWrittenBytes: Int64;
    fInitialStreamPosition: Int64;
    fCustomOptions: TTextWriterOptions; // 16-bit
    fFlags: TTextWriterFlags;     // 8-bit
    fShortStringMax: byte;        // 8-bit = high(Dest) for twfDestIsShortString
    function GetTextLength: Int64;
    function GetStream: TStream;
      {$ifdef HASINLINE} inline; {$endif}
    procedure SetStream(aStream: TStream);
    procedure SetBuffer(aBuf: pointer; aBufSize: PtrUInt);
    procedure SetOwnedStream(aBuf: pointer; aBufSize: PtrUInt);
    procedure SetOwnedRawUtf8(var aStackBuf: TTextWriterStackBuffer);
    procedure WriteToStream(data: pointer; len: PtrUInt); virtual;
    procedure InternalSetBuffer(aBuf: PUtf8Char; const aBufSize: PtrUInt);
      {$ifdef FPC} inline; {$endif}
    class procedure RaiseUnimplemented(const Method: ShortString);
    function GetFlag(one: TTextWriterFlag): boolean;
    procedure SetFlag(one: TTextWriterFlag; value: boolean);
    procedure StrRefConst(s: PStrRecConst); {$ifdef HASINLINE}inline;{$endif}
  public
    /// direct access to the low-level current position in the buffer
    // - you should not use this field directly
    // - B^ points in fact to the last written character to allow CancelLastChar,
    // i.e. the next output position is B[1]
    B: PUtf8Char;
    /// direct access to the low-level last position in the buffer
    // - you should not use this field directly
    // - points in fact 16 bytes before the actual buffer ending for AddDirect()
    BEnd: PUtf8Char;
    /// the data will be written to the specified Stream
    // - aStream may be nil: in this case, it MUST be set before using any
    // Add*() method
    // - default internal buffer size if 8192
    constructor Create(aStream: TStream; aBufSize: PtrUInt = 8192); overload;
    /// the data will be written to the specified Stream
    // - aStream may be nil: in this case, it MUST be set before using any
    // Add*() method
    // - will use an external buffer (which may be allocated on stack)
    constructor Create(aStream: TStream; aBuf: pointer; aBufSize: PtrUInt); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - default internal buffer size if 4096 (enough for most JSON objects)
    // - consider using a stack-allocated buffer and the overloaded method
    constructor CreateOwnedStream(aBufSize: PtrUInt = 4096); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - will use an external buffer (which may be allocated on stack)
    constructor CreateOwnedStream(aBuf: pointer; aBufSize: PtrUInt); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - will use the stack-allocated TTextWriterStackBuffer if possible
    constructor CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
      aBufSize: PtrUInt); overload;
    /// the data will be written to an internal RawUtf8 using the 8KB stack buffer
    // - fDest will be pointer(RawUtf8), not a true TRawByteStringStream
    constructor CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer); overload;
    /// the data will be appended to an existing RawUtf8 using the 8KB stack buffer
    // - fDest will be pointer(RawUtf8), not a true TRawByteStringStream
    constructor CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
      var aAppendTo: RawUtf8); overload;
    /// the data will be written to an external file
    // - you should call explicitly FlushFinal or FlushToStream to write
    // any pending data to the file
    constructor CreateOwnedFileStream(const aFileName: TFileName;
      aBufSize: PtrUInt = 16384);
    /// the data will be written to a ShortString - another is used as temp buffer
    // - don't forget to call FlushFinal before Free to actually fill aDest
    // - but NEVER try to reuse this instance by calling e.g. SetText or CancelAll
    // - you may prefer TLocalWriter from mormot.core.datetime for NO heap alloc
    constructor CreateOwnedShort(var aDest, aTemp: ShortString);
    /// release all internal structures
    // - e.g. free associated TStream if owned by this class
    destructor Destroy; override;

    /// write pending data, then retrieve the whole text as a UTF-8 string
    // - call CancelAll to reuse this instance after this method (or FlushFinal)
    function Text: RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// write pending data, then retrieve the whole text as a UTF-8 string
    // - call CancelAll to reuse this instance after this method (or FlushFinal)
    // - not available after CreateOwnedShort() constructor or with TLocalWriter
    procedure SetText(var result: RawUtf8; reformat: TTextWriterJsonFormat = jsonCompact);
    /// write pending data, then return a UTF-8 #0 ended buffer of the whole text
    // - may return the internal buffer directly if nothing was written to Stream
    // - flush the TRawByteStringStream/TCustomMemoryStream, and return its memory
    // - but return nil if there is no memory involved, e.g. with a TFileStream
    // - by definition, the returned buffer <> nil has a size of TextLength bytes
    // - you may use immediately the returned buffer <> nil, then call CancelAll
    // to reuse this instance after this method - but don't access result any more
    // - not available after CreateOwnedShort() constructor or with TLocalWriter
    function GetTextAsBuffer: PUtf8Char;
    /// set the internal stream content with the supplied UTF-8 text
    procedure ForceContent(const text: RawUtf8);
    /// write pending data to the destination TStream, with proper buffer adjust
    // - you should not have to call FlushToStream in most cases, but FlushFinal
    // at the end of the process, just before using the resulting Stream
    // - FlushToStream may be used to force immediate writing of the internal
    // memory buffer to the destination Stream
    // - you can set FlushToStreamNoAutoResize=true or call FlushFinal if you
    // do not want the automatic memory buffer size adjustment to take place
    procedure FlushToStream; virtual;
    /// to be called as P := FlushToStream(P) when P is the current B + 1
    function FlushToStreamUsing(P: PUtf8Char): PUtf8Char;
      {$ifdef HASINLINE}inline;{$endif}
    /// write pending data to the Stream, without automatic buffer resize
    // - will append the internal memory buffer to the Stream
    // - in short, FlushToStream may be called during the adding process, and
    // FlushFinal at the end of the process, just before using the resulting Stream
    // - if you don't call FlushToStream or FlushFinal, some pending characters
    // may not be copied to the Stream: you should call it before using the Stream
    // - call CancelAll to reuse this instance after this method - or SetText()
    procedure FlushFinal;
      {$ifdef HASINLINE}inline;{$endif}

    /// append one ASCII char
    procedure Add(const c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one ASCII char with no buffer check
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddDirect(const c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append two ASCII chars with no buffer check
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddDirect(const c1, c2: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append three ASCII chars with no buffer check
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddDirect(const c1, c2, c3: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append four ASCII chars with no buffer check
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddDirect(const c1, c2, c3, c4: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append the CRLF constant, i.e. #13#10 on Windows and #10 on POSIX
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddDirectNewLine;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one comma (',') character
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddComma;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one ASCII char, if not already there as LastChar
    procedure AddOnce(const c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append two chars
    procedure Add(const c1, c2: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    {$ifdef CPU32}
    /// append a 64-bit signed integer Value as text
    // - already implemented by Add(Value: PtrInt) method on CPU64
    procedure Add(const Value: Int64); overload;
    {$endif CPU32}
    /// append a PtrInt signed integer Value as text
    procedure Add(const Value: PtrInt); overload;
    /// append a boolean Value as 'true' or 'false' text
    procedure Add(Value: boolean); overload;
    /// append a boolean Value as 1 or 0 number
    procedure AddU(Value: boolean); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a Currency from its Int64 in-memory representation
    // - expects a PInt64 to avoid ambiguity with the AddCurr() method
    procedure AddCurr64(Value: PInt64);
    /// append a Currency value
    // - just an inlined wrapper around AddCurr64(PInt64(@Value))
    procedure AddCurr(const Value: currency); 
      {$ifdef HASINLINE}inline;{$endif}
    /// append an Unsigned 32-bit integer Value as a String
    procedure AddU(const Value: PtrUInt); overload;
    /// append an Unsigned integer <= 255 < 999 Value as a String
    procedure AddB(const Value: PtrUInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// append an Unsigned 32-bit integer Value as a quoted hexadecimal String
    procedure AddUHex(Value: cardinal; QuotedChar: AnsiChar = '"');
      {$ifdef HASINLINE}inline;{$endif}
    /// append an Unsigned 64-bit integer Value as a String
    procedure AddQ(const Value: QWord; Reserve: PtrInt = 32);
    /// append an Unsigned 64-bit integer Value as a quoted hexadecimal String
    procedure AddQHex(Value: Qword; QuotedChar: AnsiChar = '"');
      {$ifdef HASINLINE}inline;{$endif}
    /// append a GUID value, encoded as text without any {}
    // - will store e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
    // - you can set tab = @TwoDigitsHexLower to force a lowercase output
    procedure Add(Value: PGuid; QuotedChar: AnsiChar = #0; tab: PWordArray = nil); overload;
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure AddDouble(const Value: double; noexp: boolean = false);
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure AddSingle(const Value: single; noexp: boolean = false);
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure Add(const Value: Extended; precision: integer; noexp: boolean = false); overload;
    /// append a floating-point text buffer
    // - will correct on the fly '.5' -> '0.5' and '-.5' -> '-0.5'
    // - is used when the input comes from a third-party source with no regular
    // output, e.g. a database driver
    procedure AddFloatStr(P: PUtf8Char; Len: PtrInt = -1);
    /// append CR+LF (#13#10) chars
    // - this method won't call TEchoWriter.EchoAdd() registered events - use
    // TEchoWriter.AddEndOfLine() method instead
    // - TEchoWriter.AddEndOfLine() will append either CR+LF (#13#10) or
    // only LF (#10) depending on its internal options
    procedure AddCR;
      {$ifdef HASINLINE}inline;{$endif}
    /// append CR+LF (#13#10) chars and #9/#32 indentation
    // - indentation depth is defined by the HumanReadableLevel value
    procedure AddCRAndIndent; virtual;
    /// write the same character multiple times (up to the internal buffer size)
    procedure AddChars(aChar: AnsiChar; aCount: PtrInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// append an integer Value as fixed-length 2 digits text with comma
    procedure Add2(Value: cardinal);
    /// append an integer Value as fixed-length 3 digits text without any comma
    procedure Add3(Value: cardinal);
    /// append an integer Value as fixed-length 4 digits text with comma
    procedure Add4(Value: PtrUInt);
    /// append an array of RawUtf8 as CSV
    procedure AddCsvStrings(const Values: array of RawUtf8; const Sep: RawUtf8 = ',';
      HighValues: PtrInt = -1; Reverse: boolean = false); overload;
    /// append a memory array of RawUtf8 as CSV
    procedure AddCsvStrings(Values: PRawUtf8Array; HighValues: PtrInt;
      const Sep: RawUtf8 = ','; Reverse: boolean = false); overload;
    /// append an array of integers as CSV
    procedure AddCsvInteger(const Integers: array of integer);
    /// append an array of doubles as CSV
    procedure AddCsvDouble(const Doubles: array of double);
    /// append some #0-ended UTF-8 buffer
    // - input length is calculated from zero-ended char
    // - does not escape chars according to the JSON RFC
    procedure AddNoJsonEscape(P: pointer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append an UTF-8 buffer of any size
    // - does not escape chars according to the JSON RFC
    // - called by inlined AddNoJsonEscape() if Len >= AvailableBytes
    procedure AddNoJsonEscapeBig(P: pointer; Len: PtrInt);
    /// append some UTF-8 buffer with its length - inlined for small content
    // - does not escape chars according to the JSON RFC
    procedure AddNoJsonEscape(P: pointer; Len: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a RTL string as UTF-8
    // - does not escape chars according to the JSON RFC
    // - if s is a UnicodeString, will convert UTF-16 into UTF-8
    procedure AddNoJsonEscapeString(const s: string);
    /// append one UTF-16 encoded UCS-4 CodePoint as UTF-8
    // - will increase PW after the CodePoint, properly handling UTF-16 surrogates
    procedure AddWideCharNext(var PW: PWord);
    /// append one UTF-16 WideChar as UTF-8 - won't handle UTF-16 surrogates
    procedure AddWideChar(W: WideChar);
    /// append one UCS-4 CodePoint as UTF-8 - up to U+7FFFFFFF (2^32-1)
    procedure AddUcs4(ucs4: Ucs4CodePoint);
    /// append a UTF-16 encoded buffer as UTF-8
    // - WideCharCount is the UTF-16 chars count, not the byte size; if it is
    // 0, then it will convert until an ending #0 (fastest way)
    // - does not escape chars according to the JSON RFC
    procedure AddNoJsonEscapeW(PW: PWord; WideCharCount: integer); overload;
    /// append a #0-ended UTF-16 encoded buffer as UTF-8
    // - does not escape chars according to the JSON RFC
    procedure AddNoJsonEscapeW(PW: PWord); overload;
    /// append Ansi encoded buffer as UTF-8, with a specific CodePage
    // - does not escape chars according to the JSON RFC
    procedure AddNoJsonEscapeCP(P: PAnsiChar; Len: PtrInt; CodePage: cardinal);
    /// append some raw UTF-8 buffer, with no JSON escape
    // - if supplied json is '', will write 'null' so that valid JSON is written
    // - redirect to AddNoJsonEscape() otherwise
    procedure AddRawJson(const json: RawJson);
    /// append a line of text with CR+LF at the end
    procedure AddLine(const Text: ShortString);
    /// append a #0-ended UTF-8 buffer in one line
    // - will write #1..#31 chars as spaces (so content will stay on the same line)
    // - this method is slightly faster than its overload with explicit Len param
    procedure AddOnSameLine(P: PUtf8Char); overload;
    /// append a UTF-8 buffer in one line up to the supplied Len bytes
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLine(P: PUtf8Char; Len: PtrInt); overload;
    /// append a #0-ended UTF-16 buffer as UTF-8, in one line
    // - will write #1..#31 chars as spaces
    procedure AddOnSameLineW(P: PWord);
    /// append some RTL string as UTF-8, in one line
    // - will write #0..#31 chars as spaces
    procedure AddOnSameLineString(const Text: string);
      {$ifdef HASINLINE}inline;{$endif}
    /// append an UTF-8 String, with no JSON escaping
    procedure AddString(const Text: RawUtf8);
    /// append several UTF-8 strings
    procedure AddStrings(const Values: array of RawUtf8); overload;
    /// append an UTF-8 string several times
    procedure AddStrings(const Text: RawUtf8; count: PtrInt); overload;
    /// append a ShortString - and ensure has space for 255 chars (including Text)
    procedure AddShort(const Text: ShortString); overload;
    /// append a ShortString - or at least a small buffer typically < 256 chars
    procedure AddShort(Text: PUtf8Char; TextLen: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a TShort8 - Text should be not '', and up to 8 chars long
    // - this method is aggressively inlined, so may be preferred to AddShort()
    // for appending simple UTF-8 constant text
    procedure AddShorter(const Short8: TShort8);
      {$ifdef HASINLINE}inline;{$endif}
    /// append up to 4 chars, encoded as 32-bit constant
    // - called e.g. as (JSON_BASE64_MAGIC_C, 3) / (JSON_BASE64_MAGIC_QUOTE_C, 4)
    // or (JSON_SQLDATE_MAGIC_C, 3) / (JSON_SQLDATE_MAGIC_QUOTE_C, 4)
    procedure AddShort4(Text4Chars: cardinal; const TextLen: PtrInt = 4);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 'null' as text
    procedure AddNull;
    /// append a sub-part of an UTF-8 String
    // - emulates AddString(copy(Text,start,len))
    procedure AddStringCopy(const Text: RawUtf8; start, len: PtrInt);
    /// append after trim first lowercase chars ('otDone' will add 'Done' e.g.)
    procedure AddTrimLeftLowerCase(Text: PShortString);
    /// append a UTF-8 String excluding any space or control char
    // - this won't escape the text as expected by JSON
    procedure AddTrimSpaces(const Text: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a #0-terminated UTF-8 buffer excluding any space or control char
    // - this won't escape the text as expected by JSON
    procedure AddTrimSpaces(P: PUtf8Char); overload;
    /// append some text with left-filled spaces up to Width characters count
    procedure AddSpaced(const Text: RawUtf8; Width: PtrInt;
      SepChar: AnsiChar = #0); overload;
    /// append some text with left-filled spaces up to Width characters count
    // - if the value too big to fit, will truncate up to the first Width chars
    procedure AddSpaced(Text: PUtf8Char; TextLen, Width: PtrInt); overload;
    /// append some UTF-8 chars, replacing a given character with another
    procedure AddReplace(Text: PUtf8Char; Orig, Replaced: AnsiChar);
    /// append some UTF-8 chars, quoting all " chars
    // - same algorithm than AddString(QuotedStr()) - without memory allocation,
    // and with an optional maximum text length (truncated with ending '...')
    // - this function implements what is specified in the official SQLite3
    // documentation: "A string constant is formed by enclosing the string in
    // single quotes ('). A single quote within the string can be encoded by
    // putting two single quotes in a row - as in Pascal."
    procedure AddQuotedStr(Text: PUtf8Char; TextLen: PtrUInt; Quote: AnsiChar;
      TextMaxLen: PtrInt = 0);
    /// append some UTF-16 chars, quoting all " chars
    procedure AddQuotedStrW(Text: PWideChar; TextLen: PtrUInt; Quote: AnsiChar;
      TextMaxLen: PtrInt = 0);
    /// append an URI-decoded domain name, also normalizing dual // into /
    // - only parameters - i.e. after '?' - may have ' ' replaced by '+'
    // - will also ensure start with a '/' as requested in HTTP common log format
    procedure AddUrlNameNormalize(U: PUtf8Char; L: PtrInt);
    /// append some UTF-8 chars, escaping all HTML special chars as expected
    procedure AddHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat = hfAnyWhere); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 chars, escaping all HTML special chars as expected
    // - implemented by mormot.core.fmt.pas which would inject the HTML logic
    procedure AddHtmlEscape(Text: PUtf8Char; TextLen: PtrInt;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-16 chars, escaping all HTML special chars as expected
    procedure AddHtmlEscapeW(Text: PWideChar; Fmt: TTextWriterHtmlFormat = hfAnyWhere);
    /// append some RTL string chars, escaping all HTML special chars as expected
    procedure AddHtmlEscapeString(const Text: string; Fmt: TTextWriterHtmlFormat = hfAnyWhere);
    /// append a property name, as '"PropName":'
    // - PropName content should not need any JSON escape (e.g. no " within,
    // and only ASCII 7-bit characters)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    procedure AddProp(PropName: PUtf8Char; PropNameLen: PtrInt); overload;
    /// append a property name, as '"PropName":'
    // - just a wrapper around AddProp(PropName, StrLen(PropName))
    procedure AddProp(PropName: PUtf8Char); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a ShortString property name, as '"PropName":'
    // - PropName content should not need any JSON escape (e.g. no " within,
    // and only ASCII 7-bit characters)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    // - is a wrapper around AddProp() - see AddFieldName() for RawUtf8 name
    procedure AddPropName(const PropName: ShortString); overload;
    /// append an usigned integer as property name, as '"123":'
    procedure AddPropName(PropName: PtrUInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a JSON field name, followed by a number value and a comma (',')
    procedure AddPropInt64(const PropName: ShortString; Value: Int64;
      WithQuote: AnsiChar = #0);
    /// append a RawUtf8 property name, as '"FieldName":'
    // - FieldName content should not need any JSON escape (e.g. no " within)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    // - is a wrapper around AddProp() for RawUtf8
    procedure AddFieldName(const FieldName: RawUtf8);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a RawUtf8 property name, as '"FieldName"
    // - FieldName content should not need any JSON escape (e.g. no " within)
    procedure AddQuotedFieldName(const FieldName: RawUtf8;
      const VoidPlaceHolder: RawUtf8 = ''); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a RawUtf8 property name, as '"FieldName"
    // - FieldName content should not need any JSON escape (e.g. no " within)
    procedure AddQuotedFieldName(FieldName: PUtf8Char; FieldNameLen: PtrInt;
      const VoidPlaceHolder: RawUtf8 = ''); overload;
    /// append the class name of an Object instance as text
    procedure AddClassName(aClass: TClass);
    /// append a quoted Instance name and pointer, as "TObjectList(00425E68)"
    // - append "void" if Instance = nil
    // - could be used to append an instance reference as JSON object property
    procedure AddInstanceName(Instance: TObject; SepChar: AnsiChar = ':');
    /// append an Instance name and pointer, as 'TObjectList(00425E68)'+SepChar
    // - caller should have ensure that Instance <> nil
    procedure AddInstancePointer(Instance: TObject; SepChar: AnsiChar;
      IncludeUnitName, IncludePointer: boolean);
    /// append some binary data as hexadecimal text conversion
    procedure AddBinToHex(Bin: pointer; BinBytes: PtrInt; LowerHex: boolean = false;
      QuotedChar: AnsiChar = #0);
    /// append some binary data as hexadecimal text conversion
    // - append its minimal chars, i.e. excluding last bytes containing 0
    procedure AddBinToHexMinChars(Bin: pointer; BinBytes: PtrInt;
      LowerHex: boolean = false; QuotedChar: AnsiChar = #0);
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
    /// append a short Value as '12:50:b6:1e:c6:aa' hexadecimal text
    procedure AddBinToHumanHex(Bin: pointer; BinBytes: PtrInt;
      QuotedChar: AnsiChar = #0; Reverse: boolean = false);
    /// add the pointer into significant hexa chars, ready to be displayed
    // - append its minimal chars i.e. excluding highest bytes containing 0
    procedure AddPointer(P: PtrUInt; QuotedChar: AnsiChar = #0);
    /// write a byte as two hexa chars
    procedure AddByteToHex(Value: PtrUInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// write a byte as two hexa chars
    procedure AddByteToHexLower(Value: PtrUInt);
      {$ifdef HASINLINE}inline;{$endif}

    /// append strings or integers with a specified format
    // - this class implementation will raise an exception for twJsonEscape,
    // and simply call FormatUtf8() over a temp RawUtf8 for twNone/twOnSameLine
    // - raise an ESynException for twJsonEscape: use inherited TJsonWriter instead
    procedure Add(const Format: RawUtf8; const Values: array of const;
      Escape: TTextWriterKind = twNone;
      WriteObjectOptions: TTextWriterWriteObjectOptions = [woFullExpand]); overload; virtual;
    /// append a JSON value, array or document, in a specified format
    // - this class will raise an ESynException: use inherited TJsonWriter instead
    function AddJsonReformat(Json: PUtf8Char; Format: TTextWriterJsonFormat;
      Preproc: TObject = nil): boolean; virtual;
    /// this class implementation will raise an exception
    // - this class will raise an ESynException: use inherited TJsonWriter instead
    procedure AddVariant(const Value: variant; Escape: TTextWriterKind = twJsonEscape;
      WriteOptions: TTextWriterWriteObjectOptions = []); virtual;
    /// append a variant content as UTF-8 text
    // - with optional HTML escape (via a TTempUtf8) but no JSON serialization
    procedure AddVarData(Value: PVarData; HtmlEscape: boolean);
    /// append some JSON value using RTTI
    // - this class will raise an ESynException: use inherited TJsonWriter instead
    // - TypeInfo is a PRttiInfo instance - but not available in this early unit
    function AddTypedJson(Value: pointer; TypeInfo: pointer;
      WriteOptions: TTextWriterWriteObjectOptions = []): pointer; virtual;
    /// write some #0 ended UTF-8 text, according to the specified format
    // - this class will raise an ESynException: use inherited TJsonWriter instead
    procedure Add(P: PUtf8Char; Escape: TTextWriterKind); overload; virtual;
    /// write some #0 ended UTF-8 text, according to the specified format
    // - this class will raise an ESynException: use inherited TJsonWriter instead
    procedure Add(P: PUtf8Char; Len: PtrInt; Escape: TTextWriterKind); overload; virtual;
    /// append an open array constant value as UTF-8 text
    // - this class may raise an ESynException e.g. on vtVariant: use TJsonWriter
    procedure AddVarRec(V: PVarRec); overload;
    /// prepare direct access to the internal output buffer
    // - return nil if Len is too big to fit in the current buffer size
    // - return the position to write text
    // - but WON'T increase the instance position: caller should do inc(B, ...)
    function AddPrepare(Len: PtrInt): pointer;
    /// prepare direct access to the internal output buffer
    // - return the position to write text
    // - but WON'T increase the instance position: caller should do inc(B, ...)
    function AddPrepareShort(Len: PtrInt): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// write some data Base64 encoded
    // - this class will raise an ESynException: use inherited TJsonWriter instead
    procedure WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean); virtual;
    /// serialize as JSON the given object
    // - this class will raise an ESynException: use inherited TJsonWriter instead
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
      {$ifdef HASINLINE}inline;{$endif}
    /// how many bytes are currently in the internal buffer and not on disk/stream
    // - see TextLength for the total number of bytes, on both stream and memory
    function PendingBytes: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// how many bytes are currently available in the internal memory buffer
    function AvailableBytes: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// how many bytes were currently written on disk/stream
    // - excluding the bytes in the internal buffer (see PendingBytes)
    // - see TextLength for the total number of bytes, on both stream and memory
    property WrittenBytes: Int64
      read fWrittenBytes;
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
    procedure CancelLastComma; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// replace the last ',' appended, or just append it
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure ReplaceLastComma(aReplaceChar: AnsiChar);
      {$ifdef HASINLINE}inline;{$endif}
    // deprecated method with confusing name - use ReplaceLastComma() from now on
    procedure CancelLastComma(aReplaceChar: AnsiChar); overload;
    /// rewind the Stream to the position when Create() was called to reuse it
    // - note that this does not clear the Stream content itself, just
    // move back its writing position to its initial place
    // - mandatory call after FlushFinal or Text/SetText() to reuse this instance
    // - not available after CreateOwnedShort() constructor or with TLocalWriter
    procedure CancelAll;
    /// same as CancelAll, and also reset the CustomOptions before reusing it
    procedure CancelAllAsNew;
      {$ifdef HASINLINE}inline;{$endif}
    /// same as CancelAll, and also set a new local TTextWriterStackBuffer
    // to reuse this instance constructed via CreateOwnedStream(temp)
    procedure CancelAllWith(var temp: TTextWriterStackBuffer);

    /// count of added bytes to the stream
    // - see PendingBytes for the number of bytes currently in the memory buffer
    // or WrittenBytes for the number of bytes already written to disk/stream
    property TextLength: Int64
      read GetTextLength;
    /// the internal TStream used for storage
    // - you should call the FlushFinal (or FlushToStream) methods before using
    // this TStream content, to flush all pending characters
    // - if the TStream instance has not been specified in the constructor,
    // it can be forced via this property, before any writing
    // - warning: may return nil, e.g. after CreateOwnedShort() or
    // CreateOwnedStream(TTextWriterStackBuffer) since they maintain no TStream
    property Stream: TStream
      read GetStream write SetStream;
    /// global options to customize this TTextWriter instance process
    // - allows to override e.g. AddRecordJson() and AddDynArrayJson() behavior
    property CustomOptions: TTextWriterOptions
      read fCustomOptions write fCustomOptions;
    /// the internal flags used by this TTextWriter instance
    // - should not be modified by the end-user code directly
    // - use the FlushToStreamNoAutoResize or NoWriteToStreamException or
    // StreamIsOwned properties to set the corresponding flags just after Create
    property Flags: TTextWriterFlags
      read fFlags;
    /// optional event called before FlushToStream method process
    // - used e.g. by TEchoWriter to perform proper content echoing
    property OnFlushToStream: TOnTextWriterFlush
      read fOnFlushToStream write fOnFlushToStream;
    /// set twfFlushNoAutoResize in the internal Flags of this instance
    property FlushToStreamNoAutoResize: boolean
      index twfFlushNoAutoResize read GetFlag write SetFlag;
    /// set twfNoWriteToStreamException in the internal flags of this instance
    property NoWriteToStreamException: boolean
      index twfNoWriteToStreamException read GetFlag write SetFlag;
    /// set twfDestIsOwnedStream in the internal flags of this instance
    property StreamIsOwned: boolean
      index twfDestIsOwnedStream read GetFlag write SetFlag;
  end;

  /// class of our simple TEXT format writer to a Stream
  TBaseWriterClass = class of TTextWriter;

const
  /// the file extensions suitable for each JsonReformat() function
  JSON_FMT_EXT: array[TTextWriterJsonFormat] of TFileName = (
    '.json',  // jsonCompact
    '.json',  // jsonHumanReadable
    '.json5', // jsonUnquotedPropName
    '.json5', // jsonUnquotedPropNameCompact
    '.jsonc', // jsonC
    '.json5', // json5
    '.hjson', // jsonH
    '.morml', // jsonMorml
    '.json',  // jsonEscapeUnicode
    '.json'); // jsonNoEscapeUnicode

var
  /// contains the default JSON serialization class for the framework
  // - used internally by ObjectToJson/VariantSaveJson to avoid circular references
  // - will be set to TJsonWriter by mormot.core.json; default TTextWriter
  // would raise an exception on any JSON processing attempt
  DefaultJsonWriter: TBaseWriterClass = TTextWriter;

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
// - call internally TTextWriter.WriteObject() method from DefaultJsonWriter
function ObjectToJson(Value: TObject;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]): RawUtf8; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// will serialize any TObject into its UTF-8 JSON representation
procedure ObjectToJson(Value: TObject; var result: RawUtf8;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]); overload;

/// will serialize any TObject into its expanded UTF-8 JSON representation
// - includes TEXTWRITEROPTIONS_DEBUG debugger-friendly information, similar to
// TSynLog, i.e. class name and sets/enumerates as text
// - redirect to ObjectToJson() with the proper TTextWriterWriteObjectOptions,
// since our JSON serialization detects and serialize Exception.Message
function ObjectToJsonDebug(Value: TObject): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// a wrapper around ConsoleWrite(ObjectToJson(Value))
procedure ConsoleObject(Value: TObject;
  Options: TTextWriterWriteObjectOptions = [woHumanReadable]);

var
  /// mormot.core.fmt.pas will inject here proper TTextWriter.AddHtmlEscape
  // - if called with TextLen = 0, will use StrLen(Text)
  _AddHtmlEscape: procedure(W: TTextWriter; Text: PUtf8Char; TextLen: PtrInt;
    Fmt: TTextWriterHtmlFormat = hfAnyWhere);

const
  /// convenient parameter to EscapeHex() / UnescapeHex() binary to ASCII 7-bit
  ESC_ASCII: TSynAnsicharSet = [#0 .. #31, '$', #127 .. #255];

/// quickly identify if any character appears in an UTF-8 string
function NeedsEscape(text: PUtf8Char; const toescape: TSynAnsicharSet): boolean;

/// escape as \xx hexadecimal some chars from a set into a pre-allocated buffer
// - dest^ should have at least srclen * 3 bytes, for \## trios
function EscapeHexBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): PUtf8Char; overload;

/// escape as \xx hexadecimal one char into a pre-allocated buffer
// - dest^ should have at least srclen * 3 bytes, for \## trios
function EscapeHexBuffer(src, dest: PUtf8Char; srclen: integer;
  toescape, escape: AnsiChar): PUtf8Char; overload;

/// escape as \xx hexadecimal some chars from a set into a new RawUtf8 string
// - as used e.g. by LdapEscape()
function EscapeHex(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): RawUtf8; overload;

/// escape as \xx hexadecimal one char into a new RawUtf8 string
function EscapeHex(const src: RawUtf8;
  toescape: AnsiChar; escape: AnsiChar = '\'): RawUtf8; overload;

/// un-escape \xx or \c encoded chars from a pre-allocated buffer
// - any CR/LF after \ will also be ignored
// - dest^ should have at least the same length than src^
function UnescapeHexBuffer(src, dest: PUtf8Char; escape: AnsiChar = '\'): PUtf8Char;

/// un-escape \xx or \c encoded chars into a new RawUtf8 string
// - any CR/LF after \ will also be ignored
function UnescapeHex(const src: RawUtf8; escape: AnsiChar = '\'): RawUtf8; overload;

/// un-escape \xx or \c encoded chars into a new RawUtf8 string (escape='\')
// - any CR/LF after \ will also be ignored
procedure UnescapeHex(var dst: RawUtf8; src: PUtf8Char; srclen: PtrInt; escape: AnsiChar); overload;

/// escape as \char pair some chars from a set into a pre-allocated buffer
// - dest^ should have at least srclen * 2 bytes, for \char pairs
// - by definition, escape should be part of the toescape set
function EscapeCharBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): PUtf8Char;

/// escape as \char pair some chars from a set into a new RawUtf8 string
// - by definition, escape should be part of the toescape set
function EscapeChar(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): RawUtf8;

const
  /// TTextWriter JSON serialization options focusing of sets support
  // - as used e.g. by TJsonWriter.AddRecordJson/AddDynArrayJson and
  // TDynArray.SaveJson methods, and SaveJson/RecordSaveJson functions
  // - to be used as TEXTWRITEROPTIONS_TEXTSET[EnumSetsAsText]
  TEXTWRITEROPTIONS_SETASTEXT: array[boolean] of TTextWriterOptions = (
    [twoFullSetsAsStar],
    [twoFullSetsAsStar, twoEnumSetsAsTextInRecord]);

  /// TTextWriter JSON serialization options including twoEnumSetsAsTextInRecord
  TEXTWRITEROPTIONS_ENUMASTEXT: array[boolean] of TTextWriterOptions = (
    [],
    [twoEnumSetsAsTextInRecord]);

  /// TTextWriter JSON serialization options including woEnumSetsAsText
  TEXTWRITEROBJECTOPTIONS_ENUMASTEXT: TTextWriterWriteObjectOptionsBoolean = (
    [],
    [woEnumSetsAsText]);

  /// TTextWriter JSON serialization options with debugging/logging information
  TEXTWRITEROPTIONS_DEBUG =
    [woDontStoreDefault, woHumanReadable, woStoreClassName, woStorePointer,
     woHideSensitivePersonalInformation];

type
  TEchoWriter = class;

  /// callback used to echo each line of TEchoWriter class
  // - should return TRUE on success, FALSE if the log was not echoed: but
  // TSynLog will continue logging, even if this event returned FALSE
  TOnTextWriterEcho = function(Sender: TEchoWriter; Level: TSynLogLevel;
    const Text: RawUtf8): boolean of object;

  TEchoWriterBack = record
    Level: TSynLogLevelDynArray;
    Text: TRawUtf8DynArray;
    Count: PtrInt;
  end;

  /// add optional echoing of the lines to TTextWriter
  // - as used e.g. by TSynLog writer for log optional redirection
  // - is defined as a nested class to reduce plain TTextWriter scope, and
  // better follow the SOLID principles
  TEchoWriter = class
  protected
    fBackSafe: TLightLock; // protect fBack.Level/Text
    fWriter: TTextWriter;
    fEchoStart: PtrInt;
    fEchoBuf: RawUtf8;
    fEchos: array of TOnTextWriterEcho;
    fWriteLineFeed: cardinal;
    fWriteLineFeedLen: byte;
    fEchoPendingExecuteBackground: boolean;
    fBack: TEchoWriterBack;
    function EchoFlush: PtrInt;
    procedure EchoPendingToBackground(aLevel: TSynLogLevel);
    procedure EchoAddEndOfLine(aLevel: TSynLogLevel);
    function GetEndOfLineCRLF: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetEndOfLineCRLF(aEndOfLineCRLF: boolean);
  public
    /// prepare for the echoing process
    constructor Create(Owner: TTextWriter); reintroduce;
    /// end the echoing process
    destructor Destroy; override;
    /// should be called from TTextWriter.FlushToStream
    // - write pending data to the Stream, with automatic buffer resize and echoing
    // - this overriden method will handle proper echoing
    procedure FlushToStream(Text: PUtf8Char; Len: PtrInt);
    /// mark an end of line, ready to be "echoed" to registered listeners
    // - append a LF (#10) char or CR+LF (#13#10) chars to the buffer, depending
    // on the EndOfLineCRLF property value (default is LF, to minimize storage)
    // - any callback registered via EchoAdd() will monitor this line in the
    // current thread, or calling EchoPendingExecute from a background thread
    // - used e.g. by TSynLog for console output, as stated by Level parameter
    procedure AddEndOfLine(aLevel: TSynLogLevel = sllNone);
      {$ifdef HASINLINE}inline;{$endif}
    /// add a callback to echo each line written by this class
    // - this class expects AddEndOfLine to mark the end of each line
    procedure EchoAdd(const aEcho: TOnTextWriterEcho);
    /// remove a callback to echo each line written by this class
    // - event should have been previously registered by a EchoAdd() call
    procedure EchoRemove(const aEcho: TOnTextWriterEcho);
    /// reset the internal buffer used for echoing content
    procedure EchoReset;
    /// run all pending EchoPendingExecuteBackground notifications
    // - should be executed from a background thread
    procedure EchoPendingExecute;
    /// the associated TTextWriter instance
    property Writer: TTextWriter
      read fWriter;
    /// define how AddEndOfLine method stores its line feed characters
    // - by default (FALSE), it will append a LF (#10) char to the buffer
    // - you can set this property to TRUE, so that CR+LF (#13#10) chars will
    // be appended instead
    // - is just a wrapper around twoEndOfLineCRLF item in Writer.CustomOptions
    property EndOfLineCRLF: boolean
      read GetEndOfLineCRLF write SetEndOfLineCRLF;
    /// if EchoPendingExecute is about to be executed in the background
    property EchoPendingExecuteBackground: boolean
      read fEchoPendingExecuteBackground write fEchoPendingExecuteBackground;
  end;



{ ************ Numbers (integers or floats) and Variants to Text Conversion }

var
  /// naive but efficient cache to avoid string memory allocation for 0 .. 999
  // - filled with statically allocated UINT_999[] constant values at startup
  // - is defined globally, since may be used from an inlined function
  SmallUInt32Utf8: array[0 .. 999] of RawUtf8;
  /// raw pre-allocated SmallUInt32Utf8[] values as L1-friendly constants
  UINT_999: array[0 .. 999] of TStrRecConst;

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

/// optimized conversion of a cardinal into RawUtf8 for a given number of Digits
// - will always return the last Digits chars, or prepend '0' if necessary
procedure UInt32DigitsToUtf8(Value, Digits: PtrUInt; var result: RawUtf8);

/// fast RawUtf8 version of 64-bit IntToStr(), with proper QWord support
procedure UInt64ToUtf8(Value: QWord; var result: RawUtf8);

{$ifndef WIN32DELPHI} // Delphi has its own x86/x87 asm version
/// internal conversion of the retained integer mantissa times 10^exponent
// - honors the current rounding mode; nearest uses fixed-width integer arithmetic
// - scanners own the grammar and the significant-digit capacity
function DecimalToDouble(Mantissa: UInt64; Exponent: PtrInt; Negative: boolean): double;

{$ifdef CPUX64}
var
  DecimalUseFma: boolean; // initialized once from CPU and OS capabilities
const
  // RN(10^e - RN(10^e)); paired with the existing POW10[e] high part.
  DecimalReciprocalLow: array[-22..-1] of UInt64 = (
    UInt64($B7FA7566D9CBA769), // 10^-22
    UInt64($383F769FB7E0B75E), // 10^-21
    UInt64($38675447A5D8E536), // 10^-20
    UInt64($388A52B31E9E3D07), // 10^-19
    UInt64($B8D7C628066E8CEE), // 10^-18
    UInt64($B90DB7B2080A3029), // 10^-17
    UInt64($3925B4C2EBE68799), // 10^-16
    UInt64($B97937831647F5A0), // 10^-15
    UInt64($394EA70909833DE7), // 10^-14
    UInt64($B9CECD79A5A0DF95), // 10^-13
    UInt64($39F97F27F0F6E886), // 10^-12
    UInt64($3A47F7BC7B4D28AA), // 10^-11
    UInt64($BA720A5465DF8D2C), // 10^-10
    UInt64($BAB34674BFABB83B), // 10^-9
    UInt64($BAD03023DF2D4C94), // 10^-8
    UInt64($3B15E1E99483B023), // 10^-7
    UInt64($3B4B5A63F9A49C2C), // 10^-6
    UInt64($BB8EE78183F91E64), // 10^-5
    UInt64($BBB6A161E4F765FE), // 10^-4
    UInt64($BBD89374BC6A7EFA), // 10^-3
    UInt64($BC0EB851EB851EB8), // 10^-2
    UInt64($BC5999999999999A)); // 10^-1
{$endif CPUX64}

/// get the extended floating point value stored in P^
// - set the err content to the index of any faulty character, 0 if conversion
// was successful (same as the standard val function)
// - this optimized function is consistent on all platforms/compilers and return
// the decoded value even if err is not 0 (e.g. if P^ is not #0 ended)
function GetExtended(P: PUtf8Char; out err: integer): TSynExtended; overload;
{$endif WIN32DELPHI}

/// get the extended floating point value stored in P^
// - this overloaded version returns 0 as a result if the content of P is invalid
function GetExtended(P: PUtf8Char): TSynExtended; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// get a 64-bit floating-point value stored in a RawUtf8 string
// - returns TRUE if the supplied text was successfully converted into a double
function ToDouble(const text: RawUtf8; out value: double): boolean;
  {$ifdef HASINLINE}inline;{$endif}

type
  /// the non-number values potentially stored in an IEEE floating point
  TFloatNan = (
    fnNumber, fnNan, fnInf, fnNegInf);

  TPow10 = array[-31..55] of TSynExtended;
  PPow10 = ^TPow10;

const
  // some constants also available in the Math unit - see ShortToFloatNan()
  NaN         =  0.0 / 0.0;
  Infinity    =  1.0 / 0.0;
  NegInfinity = -1.0 / 0.0;

  /// the JavaScript-like values of non-number IEEE constants
  // - as recognized by ShortToFloatNan, and used by TTextWriter.Add()
  // when serializing such single/double/extended floating-point values
  // - GetExtended() should also detect those values
  JSON_NAN: array[TFloatNan] of TShort15 = (
    '0', '"NaN"', '"Infinity"', '"-Infinity"');

  /// most common 10 ^ exponent constants, ending with values for HugePower10*()
  POW10: TPow10 = (
    1E-31, 1E-30, 1E-29, 1E-28, 1E-27, 1E-26, 1E-25, 1E-24, 1E-23, 1E-22,
    1E-21, 1E-20, 1E-19, 1E-18, 1E-17, 1E-16, 1E-15, 1E-14, 1E-13, 1E-12,
    1E-11, 1E-10, 1E-9,  1E-8,  1E-7,  1E-6,  1E-5,  1E-4,  1E-3,  1E-2,
    1E-1,  1E0,   1E1,   1E2,   1E3,   1E4,   1E5,   1E6,   1E7,   1E8,
    1E9,   1E10,  1E11,  1E12,  1E13,  1E14,  1E15,  1E16,  1E17,  1E18,
    1E19,  1E20,  1E21,  1E22,  1E23,  1E24,  1E25,  1E26,  1E27,  1E28,
    1E29,  1E30,  1E31,  0,{32} -1,{33} 1E0,{34} 1E32, 1E64, 1E96, 1E128,
    1E160, 1E192, 1E224, 1E256, 1E288, 1E320, 1E-0,{45} 1E-32, 1E-64,
    1E-96, 1E-128, 1E-160, 1E-192, 1E-224, 1E-256, 1E-288, 1E-320);

var
  /// best possible precision when rendering a "single" kind of float
  // - can be used as parameter for ExtendedToShort/ExtendedToStr
  // - is defined as a var, so that you may be able to override the default
  // settings, for the whole process
  SINGLE_PRECISION: integer = 8;
  /// best possible precision when rendering a "double" kind of float
  // - can be used as parameter for ExtendedToShort/ExtendedToStr
  // - is defined as a var, so that you may be able to override the default
  // settings, for the whole process
  DOUBLE_PRECISION: integer = 15;
  /// best possible precision when rendering a "extended" kind of float
  // - can be used as parameter for ExtendedToShort/ExtendedToStr
  // - is defined as a var, so that you may be able to override the default
  // settings, for the whole process
  EXTENDED_PRECISION: integer = 18;

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
function ExtendedToShort(S: PShortString;
  Value: TSynExtended; Precision: integer): integer;

/// convert a floating-point value to its numerical text equivalency without
// scientification notation
// - DOUBLE_PRECISION will redirect to DoubleToShortNoExp() and its faster Fabian
// Loitsch's Grisu algorithm if available - or calls str(Value:0:precision,S)
// - returns the count of chars stored into S, i.e. length(S)
function ExtendedToShortNoExp(S: PShortString; Value: TSynExtended;
  Precision: integer): integer;

/// raw check if the supplied text buffer is NAN/INF/+INF/-INF, i.e. not a number
function Utf8ToFloatNan(s: PUtf8Char; len: PtrInt): TFloatNan;

/// check if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - as returned by ExtendedToShort/DoubleToShort textual conversion
// - such values do appear as IEEE floating points, but are not defined in JSON
function ShortToFloatNan(const s: ShortString): TFloatNan;
  {$ifdef HASINLINE}inline;{$endif}

/// check if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - as returned e.g. by ExtendedToStr/DoubleToStr textual conversion
// - such values do appear as IEEE floating points, but are not defined in JSON
function RawUtf8ToFloatNan(const s: RawUtf8): TFloatNan;

/// convert a floating-point value to its numerical text equivalency
function ExtendedToStr(Value: TSynExtended; Precision: integer): RawUtf8; overload;

/// convert a floating-point value to its numerical text equivalency
procedure ExtendedToStr(Value: TSynExtended; Precision: integer;
  var result: RawUtf8); overload;

/// recognize if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function FloatToJsonNan(s: PShortString): PShortString;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a floating-point value to its JSON text equivalency
// - depending on the platform, it may either call str() or FloatToText()
// in ffGeneral mode (the shortest possible decimal string using fixed or
// scientific format)
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function ExtendedToJson(tmp: PShortString; Value: TSynExtended;
  Precision: integer; NoExp: boolean): PShortString;

/// convert a 64-bit floating-point value to its numerical text equivalency
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm implementation
// - returns the count of chars stored into S, i.e. length(S)
// - S should be a true ShortString with enough chars, not e.g. TShort7
function DoubleToShort(S: PShortString; const Value: double): integer;

/// convert a 64-bit floating-point value to its numerical text equivalency
// without scientific notation
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm implementation
// - returns the count of chars stored into S, i.e. length(S)
function DoubleToShortNoExp(S: PShortString; const Value: double): integer;

{$ifdef DOUBLETOSHORT_USEGRISU}
const
  // special text returned if the double is not a number
  C_STR_INF:  TShort3 = 'Inf';
  C_STR_QNAN: TShort3 = 'Nan';

  // min_width parameter special value, as used internally by FPC for str(d,s)
  // - DoubleToAscii() only accept C_NO_MIN_WIDTH or 0 for min_width: space
  // trailing has been removed in this cut-down version
  C_NO_MIN_WIDTH = -32767;

/// raw function to convert a 64-bit double into a ShortString, stored in str
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
function DoubleToJson(tmp: PShortString; const Value: double;
  NoExp: boolean): PShortString;

/// convert a 64-bit floating-point value to its numerical text equivalency
function DoubleToStr(const Value: Double): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit floating-point value to its numerical text equivalency
procedure DoubleToStr(const Value: Double; var result: RawUtf8); overload;

/// copy a floating-point text buffer with proper correction and validation
// - will correct on the fly '.5' -> '0.5' and '-.5' -> '-0.5'
// - will end not only on #0 but on any char not matching 1[.2[e[-]3]] pattern
// - is used when the input comes from a third-party source with no regular
// output, e.g. a database driver, via TTextWriter.AddFloatStr
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

/// combine VarIsString() and VariantToUtf8() functions
function VarIsUtf8(const V: Variant; var Text: RawUtf8): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any non-null Variant into UTF-8 encoded String
// - empty and null variants will return false (usable e.g. for mustache data)
function VariantToText(const V: Variant; var Text: RawUtf8): boolean; overload;

/// save a variant value into a JSON content
// - just a wrapper around the _VariantSaveJson procedure redirection
function VariantSaveJson(const Value: variant;
  Escape: TTextWriterKind = twJsonEscape): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a variant value into a JSON content
// - just a wrapper around the _VariantSaveJson procedure redirection
procedure VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// internal low-level function to compare two variants with RawUt8 conversion
// - as used e.g. by FastVarDataComp() for complex VTypes
function VariantCompAsText(A, B: PVarData; caseInsensitive: boolean): integer;

/// internal low-level function to compare two variants with TTempUtf8 conversion
// - as used e.g. by FastVarDataComp() for diverse non-complex VType
function VariantCompAsTempUtf8(A, B: PVarData; caseInsensitive: boolean;
  flags: TVariantToTempUtf8Flags = []): integer;

var
  /// serialize a variant value into a JSON content
  // - is implemented by mormot.core.json.pas and mormot.core.variants.pas:
  // will raise an exception if none of these units is included in the project
  // - follows the TTextWriter.AddVariant() and VariantLoadJson() format
  // - is able to handle simple and custom variant types, for instance:
  // !  VariantSaveJson(1.5)='1.5'
  // !  VariantSaveJson('test')='"test"'
  // !  o := _Json('{ BSON: [ "test", 5.05, 1986 ] }');
  // !  VariantSaveJson(o)='{"BSON":["test",5.05,1986]}'
  // !  o := _Obj(['name','John','doc',_Obj(['one',1,'two',_Arr(['one',2])])]);
  // !  VariantSaveJson(o)='{"name":"John","doc":{"one":1,"two":["one",2]}}'
  // - note that before Delphi 2009, any varString value is expected to be
  // a RawUtf8 instance - which does make sense in the mORMot area
  _VariantSaveJson: procedure(const Value: variant; Escape: TTextWriterKind;
    var result: RawUtf8);

  /// unserialize a JSON content into a variant
  // - properly implemented by JsonToAnyVariant() in mormot.core.variants.pas :
  // if this unit is not included in the project, this function is nil
  // - used by mormot.core.data.pas RTTI_BINARYLOAD[tkVariant]() for complex types
  _VariantLoadJson: procedure(var Value: variant; Json: PUtf8Char;
    TryCustomVariant: pointer);

  /// write a TDateTime into strict ISO-8601 date and/or time text
  // - is implemented by DateTimeToIso8601TextVar from mormot.core.datetime.pas:
  // if this unit is not included in the project, an ESynException is raised
  // - used by VariantToUtf8() for TDateTime conversion
  _VariantToUtf8DateTimeIso8601: procedure(DT: TDateTime; FirstChar: AnsiChar;
    var result: RawUtf8; WithMS: boolean);

  /// Date/Time conversion from ISO-8601 text
  // - is implemented by Iso8601ToDateTime() from mormot.core.datetime.pas:
  // if this unit is not included in the project, this function is nil
  // - used by TRttiProp.SetValue() for TDateTime properties with a getter
  _Iso8601ToDateTime: function(const iso: RawByteString): TDateTime;

/// wrap ToDouble(Text, V) and _Iso8601ToDateTime(Text)
function AnyTextToDouble(const Text: RawUtf8; out V: double): boolean;

/// wrap VariantToDouble(Value, V) and AnyTextToDouble(VariantToText(Value))
// - V=null or any not number-shaped value will return false
function AnyVariantToDouble(const Value: Variant; out V: double): boolean;

/// convert any numerical or text Variant into a 64-bit integer
// - call first VariantToInt64() then GetInt64Bool() via VariantToTempUtf8()
// - V=null will return true/0, but any not integer-shaped value will return false
function AnyVariantToInteger(const Value: Variant; out V: Int64): boolean;

/// convert any numerical or text Variant into a 64-bit integer or a given default
// - call first VariantToInt64() then GetInt64Bool() via VariantToTempUtf8()
// - V=null or any not integer-shaped value will return the supplied Default
function AnyVariantToIntegerDef(const V: Variant; Default: Int64 = 0): Int64;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a text buffer from a 18-bit integer value (0..262143) as 3 chars
// - this encoding is faster than Base64, and has spaces on the left side
// - use function Chars3ToInt18() to decode the textual content
// - used e.g. to efficiently encode the TSynLog ThreadNumber as text
procedure Int18ToText(Value: cardinal; Text: PUtf8Char);
  {$ifdef HASINLINE}inline;{$endif}

/// decode the 18-bit integer value as encoded by Int18ToChars3()
// - no range check is performed: you should ensure that the incoming text
// follows the expected 3-chars layout
// - used e.g. to efficiently decode the TSynLog ThreadNumber text
function Chars3ToInt18(P: pointer): cardinal;
  {$ifdef HASINLINE}inline;{$endif}

/// encode a 18-bit integer following Int18ToText/Chars3ToInt18 3 chars format
function Int18ToChars3(Value: cardinal): RawUtf8; overload;

/// encode a 18-bit integer following Int18ToText/Chars3ToInt18 3 chars format
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
// - could be used e.g. as parameter to FormatUtf8() with no memory allocation
function UInt4DigitsToShort(Value: cardinal): TShort7;

/// creates a 3 digits short string from a 0..999 value
// - could be used e.g. as parameter to FormatUtf8() with no memory allocation
function UInt3DigitsToShort(Value: cardinal): TShort3;

/// creates a 2 digits short string from a 0..99 value
// - could be used e.g. as parameter to FormatUtf8() with no memory allocation
function UInt2DigitsToShort(Value: byte): TShort3;
  {$ifdef HASINLINE}inline;{$endif}

/// creates a 2 digits short string from a 00..99 value
// - won't test Value>99 as UInt2DigitsToShort()
function UInt2DigitsToShortFast(Value: byte): TShort3;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - result is in little endian order, not network order: 1.2.3.4 becomes $04030201
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(aIP: PUtf8Char; out aValue: cardinal): boolean; overload;

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - result is in little endian order, not network order: 1.2.3.4 becomes $04030201
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(const aIP: RawUtf8; out aValue: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an IPv4 'x.x.x.x' text into its 32-bit value, 0 or localhost
// - result is in little endian order, not network order: 1.2.3.4 becomes $04030201
// - returns <> 0 value if the text was a valid IPv4 text, 0 on parsing error
// - '' or '127.0.0.1' will also return 0
function IPToCardinal(const aIP: RawUtf8): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}


{ ************ Text Formatting functions }

const
  /// which TVarRec.VType are numbers, e.g. don't need to be quoted as JSON
  // - vtVariant may be a string or a complex type
  vtNotString = [vtBoolean, vtInteger, vtInt64, {$ifdef FPC} vtQWord, {$endif}
                 vtCurrency, vtExtended, vtPointer, vtInterface];

type
  /// a memory structure which avoids smallest temporary RawUtf8 allocations
  // - used by VarRecToTempUtf8/VariantToTempUtf8 and FormatUtf8/FormatShort
  // - would allocate a RawUtf8 in TempRawUtf8 only if needed, but use the
  // Temp[0..23] buffer for numbers or small text conversion
  // - you MUST eventually release any TempRawUtf8 by calling TempUtf8Done()
  TTempUtf8 = record
    Len: PtrInt;
    Text: PUtf8Char;
    TempRawUtf8: pointer;
    Temp: TTemp24;
  end;
  PTempUtf8 = ^TTempUtf8;

/// release Res.TempRawUtf8 after VariantToTempUtf8/VarRecToTempUtf8
// - is faster than FastAssignNew() since we know that its RefCnt = 1
procedure TempUtf8Done(var Res: TTempUtf8);
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant into a TTempUtf8 transient instance
// - return wasString boolean, i.e. true if the V value was a text
// - empty and null will be stored as 'null' text - unless vfNullAsVoid is set
// - booleans will be stored as 'true' or 'false' - unless vfBooleanAsInt is set
// - custom variant types (e.g. TDocVariant) as JSON - unless vfNoComplex is set
// - Res.Text will always be #0 terminated
// - you MUST eventually call TempUtf8Done(Res) unless vfNoAlloc has been set
function VariantToTempUtf8(const V: variant; var Res: TTempUtf8;
  Flags: TVariantToTempUtf8Flags = []): boolean;


var /// used by VariantToTempUtf8() for TDateTime conversion
  _VariantToTempUtf8DateTimeIso8601: procedure(DT: TDateTime;
    FirstChar: AnsiChar; var result: TTempUtf8; WithMS: boolean);

/// append any Variant to a TSynTempAdder using TTempUtf8
procedure VariantToAdder(var Adder: TSynTempAdder; const V: variant;
  Flags: TVariantToTempUtf8Flags = []);

/// convert an open array (const Args: array of const) argument into a TTempUtf8
// - it would return true if Res.Len > 0, so Res could be added or processed
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
// - Res.Text may NOT be #0 terminated if the TVarRec is a ShortString
// - you MUST eventually release any TempRawUtf8 by calling TempUtf8Done(Res)
function VarRecToTempUtf8(V: PVarRec; var Res: TTempUtf8;
  wasString: PBoolean = nil): boolean;

/// convert an open array (const Args: array of const) argument to an UTF-8 string
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
procedure VarRecToUtf8(V: PVarRec; var result: RawUtf8;
  wasString: PBoolean = nil);

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text, returning FALSE if the argument was not a string value
function VarRecToUtf8IsString(const V: TVarRec; var value: RawUtf8): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// append an open array (const Args: array of const) argument to a TSynTempAdder
procedure VarRecToAdder(var Adder: TSynTempAdder; V: PVarRec);

/// convert an open array (const Args: array of const) argument to an Int64
// - returns TRUE and set Value if the supplied argument is a vtInteger, vtInt64
// or vtBoolean
// - returns FALSE if the argument is not an integer
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
function VarRecToInt64(V: PVarRec; out value: Int64): boolean;

/// convert an open array (const Args: array of const) argument to a floating
// point value
// - returns TRUE and set Value if the supplied argument is a number (e.g.
// vtInteger, vtInt64, vtCurrency or vtExtended)
// - returns FALSE if the argument is not a number
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
function VarRecToDouble(V: PVarRec; out value: double): boolean;

/// convert an open array (const Args: array of const) argument to a value
// encoded as with :(...): inlined parameters in FormatUtf8(Format,Args,Params)
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
procedure VarRecToInlineValue(const V: TVarRec; var result: RawUtf8);

/// get an open array (const Args: array of const) character argument
// - only handle vtChar and vtWideChar kind of arguments
function VarRecAsChar(V: PVarRec): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a supplied "array of const" argument is an instance of a given class
function VarRecAs(V: PVarRec; aClass: TClass): pointer;

/// check if a supplied "array of const" argument is a default value
// - e.g. '', 0, nil or false values would return true
function VarRecIsDefault(V: PVarRec): boolean;

/// check if a supplied "array of const" argument is a void value
// - same as VarRecIsDefault() but vtBoolean always returns false
function VarRecIsVoid(V: PVarRec): boolean;
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
// - see FormatSql() and FormatJson() from mormot.core.json for ? placeholders
function FormatUtf8(const Format: RawUtf8; const Args: array of const): RawUtf8; overload;

/// fast Format() function replacement, optimized for RawUtf8
// - overloaded function, which avoid a temporary RawUtf8 instance on stack
procedure FormatUtf8(const Format: RawUtf8; const Args: array of const;
  var Result: RawUtf8); overload;

/// raw FormatUtf8() function process, using an existing TTextWriterStackBuffer
procedure FormatUtf8Raw(const Format: RawUtf8; Args: PVarRec; ArgsCount: PtrInt;
  var Result: RawUtf8; var Temp: TTextWriterStackBuffer);

/// fast Format() function replacement, tuned for direct memory buffer write
// - use the same single token % (and implementation) than FormatUtf8()
// - returns the number of UTF-8 bytes appended to Dest^
function FormatBuffer(const Format: RawUtf8; const Args: array of const;
  Dest: pointer; DestLen: PtrInt): PtrInt;

/// raw Format() function replacement, tuned for direct memory buffer write
function FormatBufferRaw(const Format: RawUtf8; Args: PVarRec; ArgsCount: PtrInt;
  Dest: pointer; DestLen: PtrInt): PUtf8Char;

/// fast Format() function replacement, for UTF-8 content stored in ShortString
// - use the same single token % (and implementation) than FormatUtf8()
// - ShortString allows fast stack allocation, so is perfect for small content
// - truncate result if the text size exceeds high(result) e.g. 255 bytes
procedure FormatShort(const Format: RawUtf8; const Args: array of const;
  var result: ShortString);

/// fast Format() function replacement, for UTF-8 content stored in ShortString
function FormatToShort(const Format: RawUtf8; const Args: array of const): ShortString;
  {$ifdef FPC}inline;{$endif} // Delphi has trouble with this

/// fast Format() function replacement, tuned for small content
// - use the same single token % (and implementation) than FormatUtf8()
procedure FormatString(const Format: RawUtf8; const Args: array of const;
  var result: string); overload;

/// fast Format() function replacement, tuned for small content
// - use the same single token % (and implementation) than FormatUtf8()
function FormatString(const Format: RawUtf8; const Args: array of const): string; overload;
  {$ifdef FPC}inline;{$endif} // Delphi don't inline "array of const" parameters

/// fast Format() function replacement, for UTF-8 content stored in variant
function FormatVariant(const Format: RawUtf8; const Args: array of const): variant;

/// fast Format() function replacement in to a TSynTempAdder
procedure FormatAdder(var Dest: TSynTempAdder; const Format: RawUtf8; const Args: array of const);

/// concatenate several arguments into an UTF-8 string
function Make(const Args: array of const): RawUtf8; overload;

/// concatenate several arguments into an UTF-8 string
procedure Make(const Args: array of const; var Result: RawUtf8;
  const IncludeLast: RawUtf8 = ''); overload;

/// concatenate several arguments into a RTL string
function MakeString(const Args: array of const): string;

/// append some text items to a RawUtf8 variable
// - see also AppendLine() below if you need a separator
procedure Append(var Text: RawUtf8; const Args: array of const); overload;

/// append one text item to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; const Added: RawByteString); overload;

/// append two text items to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; const Added1, Added2: RawByteString); overload;

/// append one char to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; Added: AnsiChar); overload;

/// append one text buffer to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; Added: pointer; AddedLen: PtrInt); overload;

/// append one short string to a RawUtf8 variable with no code page conversion
procedure AppendStr(var Text: RawUtf8; const Added: ShortString);

/// append one full range UCS-4 CodePoint into Dest with proper UTF-8 encoding
procedure AppendUcs4(var Text: RawUtf8; ucs4: Ucs4CodePoint);

/// append some text items to a RawByteString variable
procedure Append(var Text: RawByteString; const Args: array of const); overload;

/// append one text item to a RawByteString variable with no code page conversion
procedure Append(var Text: RawByteString; const Added: RawByteString); overload;

/// append one text buffer to a RawByteString variable with no code page conversion
procedure Append(var Text: RawByteString; Added: pointer; AddedLen: PtrInt); overload;

/// append two text buffers to a RawByteString variable with no code page conversion
procedure Append(var Text: RawByteString; const Added1, Added2: RawByteString); overload;

/// prepend some text to a RawByteString variable with no code page conversion
procedure Prepend(var Text: RawByteString; const Added: RawByteString); overload;

/// prepend one char to a RawByteString variable with no code page conversion
procedure Prepend(var Text: RawByteString; Added: AnsiChar); overload;

/// prepend some text items at the beginning of a RawUtf8 variable
procedure Prepend(var Text: RawUtf8; const Args: array of const); overload;

/// prepend some text items at the beginning of a RawByteString variable
procedure Prepend(var Text: RawByteString; const Args: array of const); overload;

/// append one char to a RawUtf8 variable if it not already ends with it
procedure AppendIfNone(var Text: RawUtf8; EndWith: AnsiChar);

/// prepend one char to a RawUtf8 variable if it not already starts with it
procedure PrependIfNone(var Text: RawUtf8; EndWith: AnsiChar);

/// append some text to a RawUtf8, ensuring previous text is separated with CRLF
// - could be used e.g. to update HTTP headers since here EOL = #13#10
procedure AppendLine(var Text: RawUtf8; const Args: array of const;
  const Separator: RawUtf8 = EOL);

/// append some path parts into a single file name with proper path delimiters
// - set EndWithDelim=true if you want to create e.g. a full folder name
// - similar to os.path.join() in the Python RTL
// - e.g. on Windows: MakePath(['abc', 1, 'toto.json']) = 'abc\1\toto.json'
function MakePath(const Part: array of const; EndWithDelim: boolean = false;
  Delim: AnsiChar = PathDelim): TFileName; overload;

/// append some path parts into a single file name with proper path delimiters
procedure MakePath(const Part: array of const; var Dest: TFileName;
  EndWithDelim: boolean = false; Delim: AnsiChar = PathDelim); overload;

/// a wrapper around ExpandFileName(MakePath(Part))
function MakeExpandedPath(const Part: array of const;
  EndWithDelim: boolean = false): TFileName;

/// a wrapper around EnsureDirectoryExists(MakePath(Part))
function EnsureDirectoryExists(const Part: array of const;
  RaiseExceptionOnCreationFailure: ExceptionClass = nil;
  NoExpand: boolean = false): TFileName; overload;

/// a wrapper around EnsureDirectoryExists(NormalizeFileName(MakePath(Part)))
function NormalizeDirectoryExists(const Part: array of const;
  RaiseExceptionOnCreationFailure: ExceptionClass = nil): TFileName; overload;

/// ensure all \ / path delimiters are normalized into the current OS expectation
// - if SafeFileNameU() succeeded, convert from UTF-8 into FileName
function NormalizeUriToFileName(const Uri: RawUtf8; var FileName: TFileName;
  const FolderName: TFileName = ''): boolean;

/// ensure all \ path delimiters are normalized into / URI on Windows
procedure NormalizeUriVar(const FileName: RawUtf8; var Uri: RawUtf8);
  {$ifdef OSLINUX} inline; {$endif}

/// ensure all \ path delimiters are normalized into / URI on Windows
function NormalizeUriU(const FileName: RawUtf8): RawUtf8;
  {$ifdef OSWINDOWS}{$ifdef HASINLINE} inline; {$endif}{$endif}

/// ensure all \ path delimiters are normalized into / URI on Windows
procedure NormalizeUri(const FileName: TFileName; var Uri: RawUtf8);
  {$ifndef UNICODE}{$ifdef OSWINDOWS}{$ifdef HASINLINE} inline; {$endif}{$endif}{$endif}

/// a wrapper around FileExists(MakePath(Part))
// - can optionally set the file name into a variable if it did exist
function FileExistsMake(const Part: array of const;
  SetIfFound: PFileName = nil): boolean;

/// a wrapper around DirectoryExists(MakePath(Part))
// - can optionally set the file name into a variable if it did exist
function DirectoryExistsMake(const Part: array of const;
  SetIfFound: PFileName = nil): boolean;

/// MakePath() variant which can handle the file extension specifically
function MakeFileName(const Part: array of const; LastIsExt: boolean = true): TFileName;

/// create a CSV text from some values
function MakeCsv(const Value: array of const; EndWithComma: boolean = false;
  Comma: AnsiChar = ','): RawUtf8;

/// direct conversion of a RTL string into a console OEM-encoded String
// - under Windows, will use GetConsoleOutputCP() codepage, following CP_OEM
// - under Linux, will expect the console to be defined with UTF-8 encoding
function StringToConsole(const S: string): RawByteString;

/// write some text to the console using a given color
// - redirect to mormot.core.os ConsoleWrite() with proper thread safety
procedure ConsoleWrite(const Fmt: RawUtf8; const Args: array of const;
  Color: TConsoleColor = ccDefault; NoLineFeed: boolean = false); overload;

/// write some text to the console using a given color
// - redirect to mormot.core.os ConsoleWrite() with proper thread safety
procedure ConsoleWrite(const Args: array of const;
  Color: TConsoleColor = ccDefault; NoLineFeed: boolean = false); overload;

/// write some text to the console using the current color
// - similar to writeln() but redirect to ConsoleWrite() with proper thread safety
procedure ConsoleWriteRaw(const Args: array of const; NoLineFeed: boolean = false); overload;

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


{ ************ ESynException class }

{$ifndef NOEXCEPTIONINTERCEPT}

type
  /// global hook callback to customize exceptions logged by TSynLog
  // - should return TRUE if all needed information has been logged by the
  // event handler
  // - should return FALSE if Context.EAddr and Stack trace is to be appended
  TSynLogExceptionToStr = function(WR: TTextWriter;
    const Context: TSynLogExceptionContext): boolean;

var
  /// allow to customize the ESynException logging message
  TSynLogExceptionToStrCustom: TSynLogExceptionToStr = nil;

/// the default Exception handler for logging
// - defined here to be called e.g. by ESynException.CustomLog() as default
function DefaultSynLogExceptionToStr(WR: TTextWriter;
  const Context: TSynLogExceptionContext; WithAdditionalInfo: boolean): boolean;

{$endif NOEXCEPTIONINTERCEPT}


type
  /// generic parent class of all custom Exception types of this unit
  // - all our classes inheriting from ESynException are serializable,
  // so you could use ObjectToJsonDebug(any ESynException) to retrieve some
  // extended information
  ESynException = class(ExceptionWithProps)
  protected
    fRaisedAt: pointer;
    fMessageUtf8: RawUtf8;
    // internal method called by the constructor when fMessageUtf8 was just set
    // - this virtual method will redirect to Create(Utf8ToString(fMessageUtf8))
    procedure CreateAfterSetMessageUtf8; virtual;
  public
    /// constructor which will use FormatUtf8() instead of Format()
    // - expect % as delimiter, so is less error prone than %s %d %g
    // - will handle vtPointer/vtClass/vtObject/vtVariant kind of arguments,
    // appending class name for any class or object, the hexa value for a
    // pointer, or the JSON representation of any supplied TDocVariant
    constructor CreateUtf8(const Format: RawUtf8; const Args: array of const); virtual;
    /// constructor will accept RawUtf8 instead of string as message text
    constructor CreateU(const Msg: RawUtf8);
    /// constructor appending some FormatUtf8() content to the GetLastError
    // - message will contain GetLastError value followed by the formatted text
    // - expect % as delimiter, so is less error prone than %s %d %g
    // - will handle vtPointer/vtClass/vtObject/vtVariant kind of arguments,
    // appending class name for any class or object, the hexa value for a
    // pointer, or the JSON representation of any supplied TDocVariant
    // - the exception will be raised at the caller address (as expected)
    class procedure RaiseLastOSError(const Format: RawUtf8;
      const Args: array of const; const Trailer: ShortString = 'OSError');
    /// a wrapper function around raise CreateUtf8()
    // - generated executable code could be slightly shorter
    // - the exception will be raised at the caller address (as expected)
    class procedure RaiseUtf8(const Format: RawUtf8; const Args: array of const);
    /// a wrapper function around raise CreateU()
    // - the exception will be raised at the caller address (as expected)
    class procedure RaiseU(const Msg: RawUtf8);
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// can be used to customize how the exception is logged
    // - this default implementation will call the TSynLogExceptionToStrCustom
    // global callback, if defined, or a default handler internal to this unit
    // - override this method to provide a custom logging content
    // - should return TRUE if Context.EAddr and Stack trace is not to be
    // written (i.e. as for any TSynLogExceptionToStr callback)
    function CustomLog(WR: TTextWriter;
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
    /// the Exception Message UTF-8 text, as generated by CreateUtf8()
    property MessageUtf8: RawUtf8
      read fMessageUtf8;
  published
    /// the Exception Message string, as defined in parent Exception class
    property Message;
  end;

  /// meta-class of the ESynException hierarchy
  ESynExceptionClass = class of ESynException;

/// retrieve Exception.Message as UTF-8 - handling ESynException.MessageUtf8
procedure ExceptionUtf8(E: Exception; var Message: RawUtf8);


{ **************** HTTP/REST Common Headers Parsing (e.g. cookies) }

type
  /// the available HTTP methods transmitted between client and server
  // - remote ORM supports non-standard mLOCK/mUNLOCK/mABORT/mSTATE verbs
  // - not all IANA verbs are available, because our TRestRouter will only
  // support mGET .. mOPTIONS verbs anyway
  // - for basic CRUD operations, we consider Create=mPOST, Read=mGET,
  // Update=mPUT and Delete=mDELETE - even if it is not fully RESTful
  TUriMethod = (
    mNone,
    mGET,
    mPOST,
    mPUT,
    mDELETE,
    mHEAD,
    mBEGIN,
    mEND,
    mABORT,
    mLOCK,
    mUNLOCK,
    mSTATE,
    mPATCH,
    mOPTIONS);

  /// set of available HTTP methods transmitted between client and server
  TUriMethods = set of TUriMethod;

/// convert a string HTTP verb into its TUriMethod enumerate
// - conversion is case-insensitive
function ToMethod(const method: RawUtf8): TUriMethod;

/// convert a TUriMethod enumerate to its #0 terminated uppercase text
function ToText(m: TUriMethod): PUtf8Char; overload;

type
  /// store one HTTP input cookie name/value pair
  /// - cookies are still stored untouched in the headers raw buffer
  // - e.g. NameStart='sessionId' and ValueStart='e8bb43229de9' for
  // $ Set-Cookie: sessionId=e8bb43229de9; Domain=foo.example.com
  THttpCookie = TTextBufferPair;
  /// refers to one HTTP input cookie
  PHttpCookie = ^THttpCookie;

  /// parse and store HTTP cookies received on server side
  // - shared by framework server classes, both at HTTP or REST levels
  // - Cookie[] items do point to the ParseServer() headers buffer
  {$ifdef USERECORDWITHMETHODS}
  THttpCookies = record
  {$else}
  THttpCookies = object
  {$endif USERECORDWITHMETHODS}
  private
    fCookies: TTextBufferPairDynArray; // only if InCookie[] is used
  public
    /// reset the internal list
    procedure Clear;
    /// parse the cookies from request HTTP headers on server side
    // - e.g. 'Cookie: name=value; name2=value2; name3=value3'
    // - first clear all existing cookies, then decode from the supplied headers
    // - note that the Head buffer should remain available and untouched during
    // all the process of this instance, since Cookies[] points to this memory
    procedure ParseServer(Head: PUtf8Char);
    /// retrieve a cookie name/value pair in the internal storage
    function FindCookie(const CookieName: RawUtf8): PHttpCookie;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve a cookie value from its name
    // - should always previously check "if not ###Parsed then Parse()"
    function GetCookie(const CookieName: RawUtf8): RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve a cookie value from its name
    // - should always previously check "if not ###Parsed then Parse()"
    // - consider FindCookie() if you don't really require a transient RawUtf8
    procedure RetrieveCookie(const CookieName: RawUtf8; var DestValue: RawUtf8);
      {$ifdef HASINLINE} inline; {$endif}
    {$ifdef HASINLINE} { Delphi 7 should use GetCookie() or RetrieveCookie() }
    /// retrieve an incoming HTTP cookie value
    // - cookie name are case-sensitive
    // - should always previously check "if not ###Parsed then Parse()"
    property Cookie[const CookieName: RawUtf8]: RawUtf8
      read GetCookie; default;
    {$endif HASINLINE}
    /// direct access to the internal name/value pairs list
    // - you may use NameTextBufferPair/ValueTextBufferPair() to have RawUtf8
    property Cookies: TTextBufferPairDynArray
      read fCookies;
  end;
  PHttpCookies = ^THttpCookies;

/// quickly parse a 'Cookie: Name=Value' from within HTTP headers
// - returns the length of the found Value, or 0 if Name did not match
// - could be directly applied e.g. to TBinaryCookieGenerator.Validate()
function CookieFromHeaders(Headers: PUtf8Char; const Name: RawUtf8;
  out Value: PUtf8Char): PtrInt; overload;

/// quickly return Value from 'Cookie: Name=Value' within HTTP headers
function CookieFromHeaders(Headers: PUtf8Char; const Name: RawUtf8): RawUtf8; overload;

const
  /// server can use this cookie value to delete a cookie on the browser side
  COOKIE_EXPIRED = '; Expires=Sat, 01 Jan 2010 00:00:01 GMT';
var
  /// maximum number of cookies allowed by THttpCookies.ParseServer
  // - used as Deny-Of-Service (DOS) Attack detection threshold
  // - typically no more than 50 cookies per domain
  COOKIE_MAXCOUNT_DOSATTACK: integer = 128;
  /// maximum total size of cookies allowed by THttpCookies.ParseServer
  // - used asDeny-Of-Service (DOS) Attack detection threshold
  // - usually no more than 4KB per cookie, but seems to be 4KB in total for IE
  COOKIE_MAXSIZE_DOSATTACK: integer = 4096;

/// retrieve the HTTP reason text from its integer code as PRawUtf8
// - e.g. StatusCodeToText(200)^='OK'
// - as defined in http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
// - returns the generic 'Invalid Request' for any unknown Code
function StatusCodeToText(Code: cardinal): PRawUtf8;

/// retrieve the HTTP reason text from its integer code
// - as defined in http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
procedure StatusCodeToReason(Code: cardinal; var Reason: RawUtf8);

/// convert any HTTP_* constant to an integer status code and its English text
// - returns e.g. '200 OK' or '404 Not Found', calling StatusCodeToText()
function StatusCodeToShort(Code: cardinal): TShort47;

/// convert any HTTP_* constant to an integer error code and its English text
// - returns e.g. 'HTTP Error 404 - Not Found', calling StatusCodeToText()
function StatusCodeToErrorMsg(Code: integer): RawUtf8;

/// returns true for successful HTTP status codes, i.e. in 200..399 range
// - will map mainly SUCCESS (200), CREATED (201), NOCONTENT (204),
// PARTIALCONTENT (206), NOTMODIFIED (304) or TEMPORARYREDIRECT (307) codes
// - any HTTP status not part of this range will be identified as erronous
// request e.g. in the web server statistics
function StatusCodeIsSuccess(Code: integer): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check the supplied HTTP header to contain only #13#10 EOL
// - to avoid unexpected HTTP body injection, e.g. from unsafe business code
function IsInvalidHttpHeader(const Headers: RawUtf8): boolean;

/// check if the supplied text start with 'http://' or 'https://'
function IsHttp(const text: RawUtf8): boolean;

/// check if the supplied text start with 'ldap://' or 'ldaps://'
function IsLdap(const text: RawUtf8): boolean;


{ **************** Hexadecimal Text And Binary Conversion }

var
  /// conversion table from hexa chars into 0..15 binary data
  // - returns 255 for any character out of 0..9,A..Z,a..z range
  // - used e.g. by HexToBin() function
  // - is defined globally, since may be used from an inlined function
  ConvertHexToBin: TAnsiCharToByte;
  /// conversion table from hexa chars into "shl 4" binary data
  ConvertHexToShl: TAnsiCharToByte;

  /// fast lookup table for converting hexadecimal numbers from 0 to 15
  // into their ASCII equivalence
  // - is local for better code generation
  TwoDigitsHex: TByteToWord;
  TwoDigitsHexW: TAnsiCharToWord absolute TwoDigitsHex;
  /// lowercase hexadecimal lookup table
  TwoDigitsHexLower: TByteToWord;
  TwoDigitsHexWLower: TAnsiCharToWord absolute TwoDigitsHexLower;

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
function HexToCharValid(Hex: PAnsiChar): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// internal conversion from hexa pair into a AnsiChar for PIC, ARM and x86_64
function HexToCharValid(Hex: PAnsiChar; HexToBin: PByteArray): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast check if the supplied Hex buffer is an hexadecimal representation
// of a binary buffer of a given number of bytes
function IsHex(const Hex: RawByteString; BinBytes: PtrInt): boolean;

/// fast conversion from one hexa char pair into a 8-bit AnsiChar
// - return false if any invalid (non hexa) char is found in Hex^
// - similar to HexToBin(Hex,Bin,1) but with Bin<>nil
// - use HexToCharValid if you want to check a hexadecimal char content
function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// internal conversion from hexa pair into a AnsiChar for PIC, ARM and x86_64
function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char; HexToBin: PByteArray): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from two hexa bytes into a 16-bit UTF-16 WideChar
// - as used e.g. for \u#### JSON content unescape
// - similar to HexDisplayToBin(Hex,@wordvar,2)
// - returns 0 on malformed input
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
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from hexa chars into binary data
function HexToBin(Hex: PAnsiChar; HexLen: PtrInt;
  var Bin: RawByteString): boolean; overload;

/// fast conversion from ToHumanHex() hexa chars into binary data
function HumanHexToBin(const hex: RawUtf8; var Bin: RawByteString;
  CP: cardinal = CP_RAWBYTESTRING): boolean; overload;

/// fast conversion from ToHumanHex() hexa chars into binary data
function HumanHexToBin(const hex: RawUtf8): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from ToHumanHex() hexa chars into an UTF-8 string
// - may be a convenient way to generate some UTF-8 constant from ASCII-7 source
function HexToUtf8(const hex: RawUtf8): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison between two ToHumanHex() hexa values
function HumanHexCompare(const a, b: RawUtf8): integer; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison between two ToHumanHex() hexa values
function HumanHexCompare(a, b: PUtf8Char): integer; overload;

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
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into hexa chars, ready to be displayed
function BinToHexDisplay(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from binary data into lowercase hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with BinBytes^ as an integer value will encode it
// in low-endian order (less-signignifican byte first): don't use it for display
procedure BinToHexLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
function BinToHexLower(const Bin: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
function BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
procedure BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt; var result: RawUtf8); overload;

/// fast in-place conversion from binary data into lowercase hexa chars
// - a dedicated procedure avoid any temporary stack RawUtf8 string allocation
procedure BinToHexLowerSelf(var Bin: RawByteString);

/// fast conversion from binary data into lowercase hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with Bin^ as an integer value will encode it
// in big-endian order (most-signignifican byte first): use it for display
procedure BinToHexDisplayLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
function BinToHexDisplayLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from up to 127 bytes of binary data into lowercase hexa chars
function BinToHexDisplayLowerShort(Bin: PAnsiChar; BinBytes: PtrInt): ShortString;

/// fast conversion from up to 64-bit of binary data into lowercase hexa chars
function BinToHexDisplayLowerShort16(Bin: Int64; BinBytes: PtrInt): TShort16;

/// fast conversion from up to 64-bit of binary data into lowercase hexa chars
// - warning: here binary size is in bits (typically 1..64), not bytes
procedure BinBitsToHexDisplayLowerShort16(Bin: Int64; BinBits: PtrInt;
  var Result: TShort16);

/// trim right '0' chars in a text buffer - e.g. from BinToHexLower()
function TrimMinDisplayHex(Text: PUtf8Char; TextLen: PtrInt): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into hexa lowercase chars, ready to be
// used as a convenient TFileName prefix
function BinToHexDisplayFile(Bin: PAnsiChar; BinBytes: PtrInt): TFileName;

/// append one byte as hexadecimal char pairs, into a text buffer
function ByteToHex(P: PAnsiChar; Value: byte): PAnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
function PointerToHex(aPointer: pointer): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
procedure PointerToHex(aPointer: pointer; var result: RawUtf8); overload;

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally DisplayMinChars() and BinToHexDisplay()
// - such result type would avoid a string allocation on heap
function PointerToHexShort(aPointer: pointer): TShort16; overload;

/// append an Instance name and pointer, as 'unit.name.TObjectList(00425E68)'
// - used e.g. by TTextWriter.AddInstancePointer
function PointerToText(Instance: TObject; Dest: PUtf8Char;
  IncludeUnitName, IncludePointer: boolean): PUtf8Char;

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
function CardinalToHexShort(aCardinal: cardinal): TShort15;

/// compute the hexadecimal representation of the crc32 checkum of a given text
// - wrapper around CardinalToHex(crc32c(...))
function crc32cUtf8ToHex(const str: RawUtf8): RawUtf8;

/// compute the crc32c of the UTF-8 representation of a string/TFileName
function crc32cString(const str: string): cardinal;

/// compute the hexadecimal crc32c of the UTF-8 of a string/TFileName
function crc32cStringToHexShort(const str: string): TShort15;
  {$ifdef HASINLINE} inline; {$endif}

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
  {$ifdef HASINLINE} inline; {$endif}

/// fast conversion for up to 256-bit of little-endian input into non-zero hexa
// - Len should be <= 32 bytes, to fit in a TShort64 result
// - use internally DisplayMinChars() and BinToHexDisplay()
function ToHexShort(P: pointer; Len: PtrInt): TShort64;

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally DisplayMinChars() and BinToHexDisplayLower()
function Int64ToHexLower(aInt64: Int64): RawUtf8; overload;

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
  {$ifdef CPUX86NOTPIC}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// fast conversion from hexa chars in reverse order into a cardinal
// - reverse function of Int64ToHex()
// - returns false and set aValue=0 if Hex is not a valid hexadecimal 64-bit
// signed integer
// - returns true and set aValue with the decoded number, on success
function HexDisplayToInt64(Hex: PAnsiChar; out aValue: Int64): boolean; overload;
    {$ifdef ISDELPHI}{$ifdef HASINLINE}inline;{$endif}{$endif}
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

/// append a TGuid binary content as 36 chars text
// - will store e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without any {})
// - this will be the format used for JSON encoding, e.g.
// $ { "UID": "C9A646D3-9C61-4CB7-BFCD-EE2522C8F633" }
// - you can set tab = @TwoDigitsHexLower to force a lowercase output
function GuidToText(P: PUtf8Char; guid: PByteArray; tab: PWordArray = nil): PUtf8Char;

/// convert a TGuid into 38 chars encoded { text } as RawUtf8
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with braces)
// - if you do not need the embracing { }, use ToUtf8() overloaded function
function GuidToRawUtf8(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} guid: TGuid): RawUtf8;

/// convert a TGuid into 36 chars encoded text as RawUtf8
// - will return e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without braces)
// - if you need the embracing { }, use GuidToRawUtf8() function instead
function ToUtf8(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}guid: TGuid): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TGuid into 36 chars encoded text as RawUtf8, unless it is GUID_NULL
function NotNullGuidToUtf8(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} guid: TGuid): RawUtf8;

/// convert a TGuid into 36 chars encoded text as RawUtf8
// - will return e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without braces)
// - you can set tab = @TwoDigitsHexLower to force a lowercase output
procedure ToUtf8({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} guid: TGuid;
  var text: RawUtf8; tab: PWordArray = nil); overload;

/// convert one or several TGuid into 36 chars encoded CSV text
// - will return e.g.
// ! '3F2504E0-4F89-11D3-9A0C-0305E82C3301,C595476E-73D1-4B9C-9725-308C4A72DEC8'
// - you can set tab = @TwoDigitsHexLower to force a lowercase output
function GuidArrayToCsv(const guid: array of TGuid; SepChar: AnsiChar = ',';
  tab: PWordArray = nil): RawUtf8;

/// convert a TGuid into into 38 chars encoded { text } as RTL string
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with braces)
// - this version is faster than the one supplied by SysUtils
function GuidToString(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} guid: TGuid): string;

/// convert a TGuid into its standard uppercase text representation with braces
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}'
// - using a ShortString will allow fast allocation on the stack, so is
// preferred e.g. when providing a Guid to a ESynException.CreateUtf8()
function GuidToShort({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): TShortGuid; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TGuid into its standard uppercase text representation with braces
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}'
// - using a ShortString will allow fast allocation on the stack, so is
// preferred e.g. when providing a Guid to a ESynException.CreateUtf8()
procedure GuidToShort({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid; out dest: TShortGuid); overload;

/// convert a TGuid into lowercase '3f2504e0-4f89-11d3-9a0c-0305e82c3301' text
function UuidToShort({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): TShortGuid;
  {$ifdef HASINLINE}inline;{$endif}

/// convert some text into its TGuid binary value
// - expect e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without any {}) but
// will ignore internal '-' so '3F2504E04F8911D39A0C0305E82C3301' is also fine
// - note: TGuid binary order does not follow plain HexToBin or HexDisplayToBin
// - warning: P should be not nil, and point to the first hexadecimal character
// - return nil if the supplied text buffer is not a valid TGuid
// - this will be the format used for JSON encoding, e.g.
// $ { "Uid": "C9A646D3-9C61-4CB7-BFCD-EE2522C8F633" }
function TextToGuid(P: PUtf8Char; Guid: PByteArray): PUtf8Char;

/// convert some GUID or UUID RTL string text into a TGuid binary variable
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with braces)
// - return {00000000-0000-0000-0000-000000000000} if the supplied text buffer
// is not a valid TGuid
function StringToGuid(const text: string): TGuid;

/// convert some GUID or UUID UTF-8 encoded text into a TGuid binary variable
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with braces)
// or '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without braces) or even
// '3F2504E04F8911D39A0C0305E82C3301' following TGuid order (not HexToBin)
// - return {00000000-0000-0000-0000-000000000000} if the supplied text buffer
// is not a valid TGuid
function RawUtf8ToGuid(const text: RawByteString): TGuid; overload;

/// convert some GUID or UUID UTF-8 encoded text into a TGuid binary variable
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with braces)
// or '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without braces) or even
// '3F2504E04F8911D39A0C0305E82C3301' following TGuid order (not HexToBin)
function RawUtf8ToGuid(const text: RawByteString; out guid: TGuid): boolean; overload;

/// convert some GUID or UUID UTF-8 encoded text into a TGuid binary variable
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with braces)
// or '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without braces) or even
// '3F2504E04F8911D39A0C0305E82C3301' following TGuid order (not HexToBin)
function RawUtf8ToGuid(text: PUtf8Char; textlen: PtrInt; out guid: TGuid): boolean; overload;

/// trim any space and '{' '-' '}' chars from input to get a 32-char TGuid hexa
// - change in-place the text into lowercase hexadecimal
// - returns true if resulting text is a 128-bit cleaned hexa, false otherwise
function TrimGuid(var text: RawUtf8): boolean;


implementation

uses
  Math;

{$ifdef FPC}
  // globally disable some FPC paranoid warnings - rely on x86_64 as reference
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable }
{$endif FPC}


{ ************ CSV-like Iterations over Text Buffers }

function IdemPCharAndGetNextItem(var source: PUtf8Char; const searchUp: RawUtf8;
  var Item: RawUtf8; Sep: AnsiChar): boolean;
begin
  if source <> nil then
    if IdemPChar(source, pointer(searchUp)) then
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
    FastAssignNew(result)
  else
  begin
    S := PosChar0(P, Sep); // SSE2 asm on i386 and x86_64
    FastSetString(result, P, S);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function StringReplaceCsv(const S: RawUtf8; OldNewPatternPairs: PUtf8Char;
  CaseInsensitive: boolean): RawUtf8;
var
  old, new: RawUtf8;
begin
  result := S;
  while OldNewPatternPairs <> nil do
  begin
    GetNextItem(OldNewPatternPairs, '=', old);
    GetNextItem(OldNewPatternPairs, ',', new);
    result := StringReplaceAll(result, old, new, CaseInsensitive);
  end;
end;

function GetNextItemMultiple(var P: PUtf8Char; const Sep: RawUtf8;
  var Next: RawUtf8): AnsiChar;
var
  len: PtrInt;
begin
  if P = nil then
  begin
    Next := '';
    result := #0;
  end
  else
  begin
    len := strcspn(P, pointer(Sep)); // search size of P which are not in Sep
    FastSetString(Next, P, len);
    inc(P, len);
    result := P^;
    if result <> #0 then
      inc(P)
    else
      P := nil;
  end;
end;

procedure GetNextItem(var P: PUtf8Char; Sep, Quote: AnsiChar; var result: RawUtf8);
begin
  if P = nil then
    FastAssignNew(result)
  else if P^ = Quote then
  begin
    P := UnQuoteSqlStringVar(P, result);
    if P = nil then
      FastAssignNew(result)
    else if P^ = #0 then
      P := nil
    else
      inc(P);
  end
  else
    GetNextItem(P, Sep, result);
end;

function GetNextItemTrimedBuffer(var P: PUtf8Char; Sep: AnsiChar;
  out Item: PUtf8Char): PtrInt;
var
  S: PUtf8Char;
begin
  result := 0;
  S := P;
  if (S = nil) or
     (Sep <= ' ') then
    exit;
  while S^ in [#1 .. ' '] do
    inc(S); // trim left
  Item := S;
  S := PosChar0(S, Sep); // use fast SSE2 asm on x86_64
  if S^ = #0 then
    P := nil
  else
    P := S + 1;
  result := S - Item;
  S := Item;
  while (result <> 0) and
        (S[result - 1] <= ' ') do
    dec(result); // trim right
end;

function GetNextItemBuffer(var P: PUtf8Char; Sep: AnsiChar; out Item: PUtf8Char): PtrInt;
var
  S: PUtf8Char;
begin
  result := 0;
  S := P;
  if (S = nil) or
     (Sep <= ' ') then
    exit;
  Item := S;
  result := PosChar0(S, Sep) - S; // use fast SSE2 asm on x86_64
  inc(S, result);
  if S^ = #0 then
    P := nil
  else
    P := S + 1; // skip Sep
end;

function GetNextItemBufferLen(var P: PUtf8Char; var PL: PtrInt; Sep: AnsiChar;
  out Item: PUtf8Char; TrimValue: boolean): PtrInt;
var
  S, E: PUtf8Char;
begin
  result := 0;
  S := P;
  if (S = nil) or
     (PL <= 0) or
     (Sep <= ' ') then
    exit;
  if TrimValue and
     (S^ <= ' ') then
  begin
    E := S + PL;
    repeat
      inc(S) // trim left
    until (S >= E) or
          (S^ > ' ');
    PL := E - S;
  end;
  Item := S;
  result := ByteScanIndex(pointer(S), PL, ord(Sep)); // SSE2 asm on x86_64
  if result < 0 then
  begin
    P := nil;
    result := PL;
  end
  else
  begin
    inc(result); // let P/PL point after Sep
    dec(PL, result);
    P := S + result;
    dec(result);
  end;
  if TrimValue then
    while (result <> 0) and
          (S[result - 1] <= ' ') do
      dec(result);
end;

procedure GetNextItemTrimed(var P: PUtf8Char; Sep: AnsiChar; var result: RawUtf8);
var
  S: PUtf8Char;
  len: PtrInt;
begin
  len := GetNextItemTrimedBuffer(P, Sep, S);
  FastSetString(result, S, len);
end;

procedure GetNextItemTrimedLine(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8);
var
  item: PUtf8Char;
  len: PtrInt;
begin
  if (P <> nil) and
     (Sep > ' ') then
  begin
    len := GetNextItemTrimedLineBuffer(P, Sep, item);
    FastSetString(result, item, len);
  end
  else
    FastAssignNew(result);
end;

function GetNextItemTrimedLineBuffer(var P: PUtf8Char; Sep: AnsiChar;
  out Item: PUtf8Char): PtrInt;
var
  S, E: PUtf8Char;
begin // caller should ensure that (P <> nil) and (Sep > ' ')
  while P^ in [#14 .. ' '] do
    inc(P); // trim left
  S := P;
  while (S^ > #13) and
        (S^ <> Sep) do
    inc(S); // go to end of value
  E := S;
  while (E > P) and
        (E[-1] in [#14 .. ' ']) do
    dec(E); // trim right
  Item := P;
  result := E - P;
  if (cardinal(PWord(S)^) = EOLW) or
     (S^ = Sep) then
    P := S + 1
  else if S^ = #10 then
    P := S
  else
    P := nil; // end of text or malformatted
end;

procedure GetNextItemTrimedEscaped(var P: PUtf8Char; Sep, Esc: AnsiChar;
  var result: RawUtf8);
var
  S, E: PUtf8Char;
begin
  if (P = nil) or
     (Sep <= ' ') or
     (Esc = #0) then
    FastAssignNew(result)
  else
  begin
    P := GotoNextNotSpace(P);  // trim left
    S := P;
    while (S^ <> #0) and
          ((S^ <> Sep) or
           ((S > P) and
            (S[-1] = Esc))) do // ignore e.g. \. if Sep='.' and Esc='\'
      inc(S);
    E := S;
    while (E > P) and
          (E[-1] in [#1 .. ' ']) do
      dec(E); // trim right
    FastSetString(result, P, E);
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
    FastAssignNew(result)
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
    FastSetString(result, P, E);
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
  ext: TFileName;
  P: PChar;
begin
  ext := ExtractExt(FileName, {withoutdot=}true);
  result := 0;
  P := pointer(CsvExt); // allow void extension e.g. for POSIX exe as 'exe,'
  while P <> nil do
    if SameTextS(GetNextItemString(P), ext) then
      exit
    else
      inc(result);
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
  first := true;
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

procedure GetNextItemShortString(var P: PUtf8Char; Dest: PShortString; Sep: AnsiChar);
var
  S: PUtf8Char;
  len: PtrInt;
begin
  if P <> nil then
  begin
    len := GetNextItemTrimedBuffer(P, Sep, S);
    if (len <> 0) and
       (len <= 254) then
    begin
      PByte(Dest)^ := len;
      PByteArray(Dest)^[len + 1] := 0; // #0 terminator
      MoveFast(S^, Dest^[1], len);
      exit;
    end;
  end;
  PCardinal(Dest)^ := 0 // Dest='' with #0 terminator
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
  P := GotoNextNotSpace(P);
  S := P;
  if Sep = #0 then
    while S^ > ' ' do
      inc(S)
  else
    S := PosChar0(S, Sep);
  len := S - P;
  while (P[len - 1] in [#1 .. ' ']) and
        (len > 0) do
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
    while (P^ <> #0) and
          (P^ <> Sep) do
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
  FastAssignNew(result);
  if Count = 0 then
    exit;
  ValueLen := length(Value);
  SepLen := Length(Sep);
  FastSetString(result, ValueLen * Count + SepLen * pred(Count));
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
      MoveFast(pointer(Value)^, P^, ValueLen);
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
      MoveFast(pointer(Sep)^, P^, SepLen);
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
  FastAssignNew(result);
  i := 0;
  while i < BitsCount do
    if GetBitPtr(@Bits, i) then
    begin
      j := i;
      while (j + 1 < BitsCount) and
            GetBitPtr(@Bits, j + 1) do
        inc(j);
      Append(result, UInt32ToUtf8(i + 1));
      if j = i then
        AppendShortToUtf8(',', result)
      else if j = i + 1 then
        Append(result, [',', j + 1, ','])
      else
        Append(result, ['-', j + 1, ',']);
      i := j + 1;
    end
    else
      inc(i);
  AppendShortToUtf8('0', result); // '0' marks end of list
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
  while (P^ <> #0) and
        (P^ <> Sep) do // go to end of CSV item (ignore any decimal)
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
  q: QWord; // safer with a transient variable
begin
  q := 0;
  L := GetNextTChar64(P, Sep, tmp);
  if (L > 0) and
     (L and 1 = 0) then
    if not HexDisplayToBin(@tmp, @q, L shr 1) then
      q := 0;
  result := q;
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
    FastAssignNew(result)
  else
    for i := 0 to Index do
      GetNextItem(P, Sep, result);
end;

function GetUnQuoteCsvItem(P: PUtf8Char; Index: PtrUInt; Sep, Quote: AnsiChar): RawUtf8;
var
  i: PtrUInt;
begin
  if P = nil then
    FastAssignNew(result)
  else
    for i := 0 to Index do
      GetNextItem(P, Sep, Quote, result);
end;

function GetFirstCsvItem(const Csv: RawUtf8; Sep: AnsiChar): RawUtf8;
var
  i: PtrInt;
begin
  i := PosExChar(Sep, Csv);
  if i = 0 then
    result := Csv
  else
    FastSetString(result, pointer(Csv), i - 1);
end;

function GetLastCsvItem(const Csv: RawUtf8; Sep: AnsiChar): RawUtf8;
begin
  result := SplitRight(Csv, Sep, nil);
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

function CsvContains(Csv, Value: PUtf8Char; ValueLen: PtrInt;
  Sep: AnsiChar; CaseSensitive, TrimValue: boolean): boolean;
var
  o: PUtf8Char; // no temporary memory allocation
  l: PtrInt;
begin
  result := (Csv <> nil) and
            (ValueLen > 0);
  if result then
    repeat // use fast SSE2 asm on x86_64
      if TrimValue then
        l := GetNextItemTrimedBuffer(Csv, Sep, o)
      else
        l := GetNextItemBuffer(Csv, Sep, o);
      if l = ValueLen then
        if CaseSensitive then
        begin
          if CompareMem(o, Value, l) then
            exit;
        end else if IdemPropNameUSameLenNotNull(o, Value, l) then
          exit;
    until Csv = nil;
  result := false;
end;

function CsvContains(const Csv, Value: RawUtf8; Sep: AnsiChar; CaseSensitive: boolean): boolean;
begin
  result := CsvContains(pointer(Csv), pointer(Value), length(Value), Sep, CaseSensitive, false);
end;

function CsvContains(Csv, Value: PUtf8Char; CsvLen, ValueLen: PtrInt;
  Sep: AnsiChar; CaseSensitive, TrimValue: boolean): boolean;
var
  o: PUtf8Char; // no temporary memory allocation
  l: PtrInt;
begin
  result := (Csv <> nil) and
            (ValueLen > 0);
  if result then
    repeat // use fast SSE2 asm on x86_64
      l := GetNextItemBufferLen(Csv, CsvLen, Sep, o, TrimValue);
      if l = ValueLen then
        if CaseSensitive then
        begin
          if CompareMem(o, Value, l) then
            exit;
        end else if IdemPropNameUSameLenNotNull(o, Value, l) then
          exit;
    until Csv = nil;
  result := false;
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
      TrimSelf(s);
    if CaseSensitive then
    begin
      if SortDynArrayRawByteString(s, Value) = 0 then
        exit;
    end
    else if SameTextU(s, Value) then
      exit;
    inc(result);
  end;
  result := -1; // not found
end;

procedure CsvToRawUtf8DynArray(Csv: PUtf8Char; var List: TRawUtf8DynArray;
  Sep: AnsiChar; TrimItems, AddVoidItems: boolean; Quote: AnsiChar);
var
  s: RawUtf8;
  n: integer;
begin
  n := length(List);
  while (Csv <> nil) and
        (Csv^ <> #0) do
  begin
    if Quote <> #0 then
    begin
      GetNextItem(Csv, Sep, Quote, s);
      if TrimItems then
        TrimSelf(s);
    end
    else if TrimItems and
            (Sep > ' ') then
      GetNextItemTrimed(Csv, Sep, s)
    else
      GetNextItem(Csv, Sep, s);
    if (s <> '') or
       AddVoidItems then
      AddRawUtf8(List, n, s);
  end;
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

procedure CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8;
  var List: TRawUtf8DynArray);
var
  offs, i, n: integer;
  s: RawUtf8;
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
        i := length(csv) + 1;
      FastSetString(s, @PByteArray(Csv)[offs - 1], i - offs);
      AddRawUtf8(List, n, s);
      break;
    end;
    FastSetString(s, @PByteArray(Csv)[offs - 1], i - offs);
    AddRawUtf8(List, n, s);
    offs := i + length(Sep);
  end;
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

function CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8): TRawUtf8DynArray;
begin
  result := nil;
  CsvToRawUtf8DynArray(Csv, Sep, SepEnd, result);
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
      Append(result, [',', Prefix, s]);
  end;
end;

procedure AddToCsv(const Value: RawUtf8; var Csv: RawUtf8; const Sep: RawUtf8);
begin
  if Csv = '' then
    Csv := Value
  else
    Append(Csv, Sep, Value);
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

function CsvGuessSeparator(const Csv: RawUtf8): AnsiChar;
begin
  if PosExChar(#9, Csv) <> 0 then
    result := #9
  else if PosExChar(';', Csv) <> 0 then
    result := ';'
  else if PosExChar(',', Csv) <> 0 then
    result := ','
  else
    result := #0;
end;

procedure PRawUtf8ToCsv(v: PPUtf8Char; n: integer; const sep: RawUtf8;
  Reverse: boolean; var result: RawUtf8);
var
  len, seplen: PtrInt;
  p: PAnsiChar;
  s: PUtf8Char;
begin
  FastAssignNew(result);
  if (v = nil) or
     (n <= 0) then
    exit;
  if n = 1 then
  begin
    result := PRawUtf8(v)^;
    exit;
  end;
  seplen := length(sep);
  p := FastNewString(seplen * (n - 1) + SumRawUtf8Length(pointer(v), n), CP_UTF8);
  pointer(result) := p;
  if Reverse then
    v := @PPointerArray(v)[n - 1];
  repeat
    s := v^;
    if s <> nil then
    begin
      len := PStrLen(s - _STRLEN)^;
      MoveFast(s^, p^, len);
      inc(p, len);
    end;
    dec(n);
    if n = 0 then
      break;
    if Reverse then
      dec(v)
    else
      inc(v);
    if seplen = 0 then
      continue;
    MoveFast(pointer(sep)^, p^, seplen);
    inc(p, seplen);
  until false;
end;

procedure PVariantToCsv(v: PVariant; n: integer; const sep: RawUtf8;
  Reverse: boolean; var result: RawUtf8; flags: TVariantToTempUtf8Flags);
var
  tmp: TSynTempAdder;
begin
  tmp.Init;
  if Reverse then
    v := @PVariantArray(v)[n - 1];
  if n > 0 then
    repeat
      VariantToAdder(tmp, v^, flags); // use TTempUtf8
      dec(n);
      if n = 0 then
        break;
      if Reverse then
        dec(v)
      else
        inc(v);
      tmp.Add(sep);
    until false;
  tmp.Done(result);
end;

function RawUtf8ArrayToCsv(const Values: TRawUtf8DynArray; const Sep: RawUtf8;
  Reverse: boolean): RawUtf8;
begin
  PRawUtf8ToCsv(pointer(Values), length(Values), Sep, Reverse, result);
end;

procedure RawUtf8ArrayToCsvVar(const Values: TRawUtf8DynArray; var Csv: RawUtf8;
  const Sep: RawUtf8; Reverse: boolean);
begin
  PRawUtf8ToCsv(@Values[0], length(Values), Sep, Reverse, Csv);
end;

function JoinCsv(const Sep: RawUtf8; const Values: array of RawUtf8;
  Reverse: boolean): RawUtf8;
begin
  PRawUtf8ToCsv(@Values[0], length(Values), Sep, Reverse, result);
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
  RawUtf8ArrayToCsvVar(tmp, result, Sep);
end;

procedure CsvToIntegerDynArray(Csv: PUtf8Char; var List: TIntegerDynArray;
  Sep: AnsiChar);
var
  n: integer;
begin
  n := length(List);
  while (Csv <> nil) and
        (Csv^ <> #0) do
    AddInteger(List, n, GetNextItemInteger(Csv, Sep));
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

procedure CsvToInt64DynArray(Csv: PUtf8Char; var List: TInt64DynArray;
  Sep: AnsiChar);
var
  n: integer;
begin
  n := length(List);
  while (Csv <> nil) and
        (Csv^ <> #0) do
    AddInt64(List, n, GetNextItemInt64(Csv, Sep));
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

function CsvToInt64DynArray(Csv: PUtf8Char; Sep: AnsiChar): TInt64DynArray;
var
  n: integer;
begin
  result := nil;
  n := 0;
  while (Csv <> nil) and
        (Csv^ <> #0) do
    AddInt64(result, n, GetNextItemInt64(Csv, Sep));
  if result <> nil then
    DynArrayFakeLength(result, n);
end;

const // first byte is the len, then 20 bytes buffer for the 64-bit integer text
  I2T_SIZE = 21; // as TSynTempBuffer = up to 194 integers on stack

procedure IntToText(int: PAnsiChar; len, n: PtrInt; const pref, suf: RawUtf8;
  inlin: boolean; sep: AnsiChar; var result: RawUtf8);
var
  L: PtrUInt;
  P: PAnsiChar;
begin
  inc(len, (n - 1) + length(pref) + length(suf));
  if inlin then
    inc(len, n * 4); // :( ): markers
  P := FastSetString(result, len);
  if pref <> '' then
  begin
    L := length(pref);
    MoveFast(pointer(pref)^, P^, L);
    inc(P, L);
  end;
  if inlin then
    repeat
      PCardinal(P)^ := ord(':') + ord('(') shl 8;
      inc(P, 2);
      MoveFast(int[I2T_SIZE - ord(int^)], P^, ord(int^));
      inc(P, ord(int^));
      PCardinal(P)^ := ord(')') + ord(':') shl 8;
      inc(P, 2);
      dec(n);
      if n = 0 then
        break;
      inc(int, I2T_SIZE);
      P^ := sep;
      inc(P);
    until false
  else
    repeat
      L := ord(int^);
      MoveFast(PAnsiChar(int)[I2T_SIZE - L], P^, L);
      inc(P, L);
      dec(n);
      if n = 0 then
        break;
      inc(int, I2T_SIZE);
      P^ := sep;
      inc(P);
    until false;
  if suf <> '' then
    MoveFast(pointer(suf)^, P^, length(suf));
end;

function IntegerDynArrayToCsv(Values: PIntegerArray; ValuesCount: integer;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
var
  i, L, Len: PtrInt;
  int, P: PAnsiChar;
  temp: TSynTempBuffer; // faster than a dynamic array
begin
  FastAssignNew(result);
  if ValuesCount = 0 then
    exit;
  int := temp.Init(ValuesCount * I2T_SIZE);
  try
    Len := 0;
    for i := 0 to ValuesCount - 1 do
    begin
      P := StrInt32(int + I2T_SIZE, Values[i]);
      L := int + I2T_SIZE - P;
      int^ := AnsiChar(L);
      inc(Len, L);
      inc(int, I2T_SIZE);
    end;
    IntToText(temp.buf, Len, ValuesCount, Prefix, Suffix, InlinedValue, SepChar, result);
  finally
    temp.Done;
  end;
end;

function Int64DynArrayToCsv(Values: PInt64Array; ValuesCount: integer;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
var
  i, L, Len: PtrInt;
  int, P: PAnsiChar;
  temp: TSynTempBuffer; // faster than a dynamic array
begin
  FastAssignNew(result);
  if ValuesCount = 0 then
    exit;
  int := temp.Init(ValuesCount * I2T_SIZE);
  try
    Len := 0;
    for i := 0 to ValuesCount - 1 do
    begin
      P := StrInt64(int + I2T_SIZE, Values[i]);
      L := int + I2T_SIZE - P;
      int^ := AnsiChar(L);
      inc(Len, L);
      inc(int, I2T_SIZE);
    end;
    IntToText(temp.buf, Len, ValuesCount, Prefix, Suffix, InlinedValue, SepChar, result);
  finally
    temp.Done;
  end;
end;

function IntegerDynArrayToCsv(const Values: TIntegerDynArray;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
begin
  result := IntegerDynArrayToCsv(pointer(Values), length(Values),
    Prefix, Suffix, InlinedValue, SepChar);
end;

function Int64DynArrayToCsv(const Values: TInt64DynArray;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
begin
  result := Int64DynArrayToCsv(pointer(Values), length(Values),
    Prefix, Suffix, InlinedValue, SepChar);
end;


{ ************ TTextWriter parent class for Text Generation }

function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char): boolean; // for inlining
var
  b, c: byte;
begin
  if Hex <> nil then
  begin
    b := ConvertHexToShl[Hex[0]];
    c := ConvertHexToBin[Hex[1]];
    if (b <> 255) and
       (c <> 255) then
    begin
      if Bin <> nil then
      begin
        inc(c, b);
        Bin^ := AnsiChar(c);
      end;
      result := true;
      exit;
    end;
  end;
  result := false; // return false if any invalid char
end;

procedure BinToHexDisplay(Bin, Hex: PAnsiChar; BinBytes: PtrInt); // for inlining
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexW;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if BinBytes <= 0 then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexW;
  {$endif CPUX86NOTPIC}
  inc(Hex, BinBytes * 2);
  repeat
    dec(Hex, 2);
    PWord(Hex)^ := tab[Bin^];
    inc(Bin);
    dec(BinBytes);
  until BinBytes = 0;
end;

procedure BinToHexDisplayLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexWLower;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if BinBytes <= 0 then
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

procedure Int18ToText(Value: cardinal; Text: PUtf8Char);
begin
  PCardinal(Text)^ := PtrUInt(((Value shr 12) and $3f) or // 6-bit per char
                              (((Value shr 6) and $3f) shl 8) or
                              ((Value and $3f) shl 16)) + $202020;
end;

procedure TempUtf8Done(var Res: TTempUtf8);
var
  sr: PStrRec;
begin
  sr := Res.TempRawUtf8;
  if sr = nil then
    exit; // no temporary memory allocation to release
  dec(sr);
  FreeMem(sr); // we know that sr^.refCnt = 1
end;


{ TTextWriter }

const
  TRAIL_BYTES = 16; // TTextWriter.BEnd before actual buffer ending

procedure TTextWriter.InternalSetBuffer(aBuf: PUtf8Char; const aBufSize: PtrUInt);
begin
  fTempBufSize := aBufSize;
  fTempBuf := aBuf;
  dec(aBuf);
  B := aBuf;   // Add() methods will append at B+1
  BEnd := @aBuf[aBufSize - (TRAIL_BYTES - 1)]; // to avoid overwrite/overread
end;

procedure TTextWriter.SetBuffer(aBuf: pointer; aBufSize: PtrUInt);
begin
  if aBufSize <= TRAIL_BYTES then
    ESynException.RaiseUtf8('%.SetBuffer(size=%)', [self, aBufSize]);
  if aBuf = nil then
    GetMem(aBuf, aBufSize)
  else
    Include(fFlags, twfBufferIsOnStack);
  InternalSetBuffer(aBuf, aBufSize);
end;

procedure TTextWriter.SetOwnedStream(aBuf: pointer; aBufSize: PtrUInt);
begin
  fDest := TRawByteStringStream.Create; // inlined SetStream()
  fFlags := [twfDestIsOwnedStream, twfRawByteStringStream];
  SetBuffer(aBuf, aBufSize); // aBuf may be nil
end;

procedure TTextWriter.SetOwnedRawUtf8(var aStackBuf: TTextWriterStackBuffer);
begin
  fFlags := [twfDestIsRawUtf8, twfBufferIsOnStack]; // now fDest = RawUtf8
  InternalSetBuffer(@aStackBuf, SizeOf(aStackBuf));
end;

constructor TTextWriter.Create(aStream: TStream; aBufSize: PtrUInt);
begin
  SetStream(aStream);
  if aBufSize < 256 then
    aBufSize := 256;
  SetBuffer(nil, aBufSize);
end;

constructor TTextWriter.Create(aStream: TStream; aBuf: pointer; aBufSize: PtrUInt);
begin
  SetStream(aStream);
  SetBuffer(aBuf, aBufSize);
end;

constructor TTextWriter.CreateOwnedStream(aBuf: pointer; aBufSize: PtrUInt);
begin
  SetOwnedStream(aBuf, aBufSize);
end;

constructor TTextWriter.CreateOwnedStream(aBufSize: PtrUInt);
begin
  SetOwnedStream(nil, aBufSize);
end;

constructor TTextWriter.CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
  aBufSize: PtrUInt);
begin
  if aBufSize > SizeOf(aStackBuf) then // temp too small -> allocate on heap
    SetOwnedStream(nil, aBufSize)
  else
    SetOwnedRawUtf8(aStackBuf);
end;

constructor TTextWriter.CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer);
begin
  SetOwnedRawUtf8(aStackBuf);
end;

constructor TTextWriter.CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
  var aAppendTo: RawUtf8);
begin
  SetOwnedRawUtf8(aStackBuf);
  pointer(fDest) := pointer(aAppendTo); // will now own this instance
  pointer(aAppendTo) := nil;
end;

constructor TTextWriter.CreateOwnedFileStream(
  const aFileName: TFileName; aBufSize: PtrUInt);
begin
  DeleteFile(aFileName);
  fDest := TFileStreamEx.Create(aFileName, fmCreate or fmShareRead);
  fFlags := [twfDestIsOwnedStream];
  SetBuffer(nil, aBufSize);
end;

constructor TTextWriter.CreateOwnedShort(var aDest, aTemp: ShortString);
begin // should match exactly TLocalWriter.Init from mormot.core.fmt
  if high(aTemp) < TRAIL_BYTES then
    ESynException.RaiseUtf8('%.CreateOwnedShort(temp[%])', [self, high(aTemp)]);
  fFlags := [twfBufferIsOnStack, twfDestIsShortString, twfFlushNoAutoResize];
  InternalSetBuffer(@aTemp, high(aTemp) + 1);
  aDest[0] := #0;
  fDest := @aDest; // not a true TStream
  fShortStringMax := high(aDest);
end;

destructor TTextWriter.Destroy;
begin
  if twfDestIsRawUtf8 in fFlags then // fDest is a RawUtf8 not a TStream
    FastAssignNew(RawUtf8(fDest))
  else if twfDestIsOwnedStream in fFlags then
    FreeAndNil(fDest);
  if not (twfBufferIsOnStack in fFlags) then
    FreeMem(fTempBuf);
  inherited Destroy;
end;

function TTextWriter.PendingBytes: PtrUInt;
begin
  result := B - fTempBuf + 1;
end;

function TTextWriter.AvailableBytes: PtrUInt;
begin
  result := BEnd - B;
  if PtrInt(result) < 0 then
    result := 0; // may happen with AddDirect/AddComma
end;

procedure TTextWriter.Add(const c: AnsiChar);
begin
  if B >= BEnd then
    FlushToStream; // may rewind B -> not worth any local PUtf8Char variable
  B[1] := c;
  inc(B);
end;

procedure TTextWriter.AddDirect(const c: AnsiChar);
begin
  B[1] := c;
  inc(B);
end;

procedure TTextWriter.AddDirect(const c1, c2: AnsiChar);
begin
  PCardinal(B + 1)^ := byte(c1) + PtrUInt(byte(c2)) shl 8;
  inc(B, 2); // with proper constant propagation above when inlined
end;

procedure TTextWriter.AddDirect(const c1, c2, c3: AnsiChar);
begin
  PCardinal(B + 1)^ := byte(c1) + PtrUInt(byte(c2)) shl 8 + PtrUInt(byte(c3)) shl 16;
  inc(B, 3); // with proper constant propagation above when inlined
end;

procedure TTextWriter.AddDirect(const c1, c2, c3, c4: AnsiChar);
begin
  PCardinal(B + 1)^ := byte(c1) + PtrUInt(byte(c2)) shl 8 +
                       PtrUInt(byte(c3)) shl 16 + PtrUInt(byte(c4)) shl 24;
  inc(B, 4); // with proper constant propagation above when inlined
end;

procedure TTextWriter.AddDirectNewLine;
begin
  {$ifdef OSPOSIX} // mimics CRLF = #10 on POSIX
  B[1] := #10;
  inc(B);
  {$else}          // mimics CRLF = #13#10 on Windows
  PWord(B + 1)^ := EOLW;
  inc(B, 2);
  {$endif OSPOSIX}
end;

procedure TTextWriter.AddComma;
begin
  B[1] := ',';
  inc(B);
end;

procedure TTextWriter.Add(const c1, c2: AnsiChar);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := byte(c1) + PtrUInt(byte(c2)) shl 8;
  inc(B, 2); // with proper constant propagation above when inlined
end;

procedure TTextWriter.AddShorter(const Short8: TShort8);
begin
  if B >= BEnd then
    FlushToStream;
  PInt64(B + 1)^ := PInt64(@Short8[1])^;
  inc(B, ord(Short8[0]));
end;

procedure TTextWriter.AddShort4(Text4Chars: cardinal; const TextLen: PtrInt);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := Text4Chars;
  inc(B, TextLen);
end;

class procedure TTextWriter.RaiseUnimplemented(const Method: ShortString);
begin
  raise ESynException.CreateUtf8(
    '%.% unimplemented: use TJsonWriter', [self, Method])
    {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame)
    {$else} at ReturnAddress {$endif}
end;

procedure TTextWriter.Add(const Format: RawUtf8; const Values: array of const;
  Escape: TTextWriterKind; WriteObjectOptions: TTextWriterWriteObjectOptions);
var
  tmp: RawUtf8;
begin
  // basic implementation: see faster and more complete version in TJsonWriter
  FormatUtf8(Format, Values, tmp);
  case Escape of
    twNone:
      AddString(tmp);
    twOnSameLine:
      AddOnSameLine(pointer(tmp)); // minimalistic version for TSynLog
    twJsonEscape:
      RaiseUnimplemented('Add(twJsonEscape)');
  end;
end;

procedure TTextWriter.AddVariant(const Value: variant; Escape: TTextWriterKind;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  RaiseUnimplemented('AddVariant');
end;

procedure TTextWriter.AddVarData(Value: PVarData; HtmlEscape: boolean);
var
  tmp: TTempUtf8;
begin
  if cardinal(Value^.VType) = varVariantByRef then
    Value := Value^.VPointer;
  if HtmlEscape and
     not (cardinal(Value^.VType) in VTYPE_NUMERIC) then
  begin
    VariantToTempUtf8(PVariant(Value)^, tmp);
    if tmp.Len <> 0 then
      _AddHtmlEscape(self, tmp.Text, tmp.Len); // in mormot.core.fmt.pas
    TempUtf8Done(tmp);
  end
  else // avoid UTF-8 conversion for plain numbers or if no HTML escaping
    AddVariant(PVariant(Value)^, twNone); // fast TJsonWriter.AddVariant
end;

function TTextWriter.AddTypedJson(Value, TypeInfo: pointer;
  WriteOptions: TTextWriterWriteObjectOptions): pointer;
begin
  RaiseUnimplemented('AddTypedJson');
  result := nil;
end;

function TTextWriter.GetFlag(one: TTextWriterFlag): boolean;
begin
  result := (self <> nil) and
            (one in fFlags);
end;
procedure TTextWriter.SetFlag(one: TTextWriterFlag; value: boolean);
begin
  if self <> nil then
    if value then
      include(fFlags, one)
    else
      exclude(fFlags, one);
end;

function TTextWriter.AddJsonReformat(Json: PUtf8Char;
  Format: TTextWriterJsonFormat; Preproc: TObject): boolean;
begin
  RaiseUnimplemented('AddJsonReformat');
  result := false; // make compiler happy
end;

procedure TTextWriter.Add(P: PUtf8Char; Escape: TTextWriterKind);
begin
  RaiseUnimplemented('Add(Escape)');
end;

procedure TTextWriter.Add(P: PUtf8Char; Len: PtrInt; Escape: TTextWriterKind);
begin
  RaiseUnimplemented('Add(Escape)');
end;

procedure TTextWriter.AddVarRec(V: PVarRec);
begin
  case V^.VType of // use efficient jmp table
    vtInteger:
      Add(V^.VInteger);
    vtBoolean:
      AddU(V^.VBoolean); // normalize as 0 or 1
    vtChar:
      Add(V^.VChar);
    vtWideChar:
      AddWideChar(V^.VWideChar);
    vtExtended:
      AddDouble(V^.VExtended^);
    vtCurrency:
      AddCurr64(V^.VInt64);
    vtInt64:
      Add(V^.VInt64^);
    {$ifdef FPC}
    vtQWord:
      AddQ(V^.VQWord^);
    {$endif FPC}
    vtVariant:
      AddVariant(V^.VVariant^, twNone); // implemented in TJsonWriter
    vtString:
      if V^.VString^[0] <> #0 then
        AddShort(V^.VString^);
    vtPointer,
    vtInterface:
      if V^.VPointer = nil then
        AddShort4(NULL_LOW)
      else
        Add(PtrInt(V^.VPointer)); // as VarRecToVariant()
    vtPChar:
      AddNoJsonEscape(V^.VPChar, mormot.core.base.StrLen(V^.VPChar));
    vtObject:
      if V^.VObject <> nil then
        AddClassName(PClass(V^.VObject)^); // no WriteObject() here
    vtClass:
      AddClassName(V^.VClass);
    vtAnsiString:
      if V^.VAnsiString <> nil then // expect RawUtf8
        AddNoJsonEscape(V^.VAnsiString, PStrLen(V^.VPChar - _STRLEN)^);
    vtPWideChar,
    {$ifdef HASVARUSTRING}
    vtUnicodeString,
    {$endif HASVARUSTRING}
    vtWideString:
      if V^.VWideString <> nil then
        AddNoJsonEscapeW(V^.VWideString);
  end;
end;

procedure TTextWriter.WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean);
begin
  RaiseUnimplemented('WrBase64');
end;

procedure TTextWriter.AddNull;
begin
  AddShort4(NULL_LOW);
end;

function TTextWriter.AddPrepare(Len: PtrInt): pointer;
begin
  result := nil;
  if Len >= fTempBufSize - TRAIL_BYTES then
    exit;
  if BEnd - B <= Len then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  result := B + 1;
end;

function TTextWriter.AddPrepareShort(Len: PtrInt): pointer;
begin
  if BEnd - B <= Len then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  result := B + 1;
end;

procedure TTextWriter.WriteObject(Value: TObject;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  RaiseUnimplemented('WriteObject');
end;

procedure TTextWriter.AddObjArrayJson(const aObjArray;
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
  ReplaceLastComma(']');
end;

procedure TTextWriter.WriteToStream(data: pointer; len: PtrUInt);
var
  written: PtrInt;
begin
  if Assigned(fOnFlushToStream) then
    fOnFlushToStream(data, len);
  if len = 0 then
    exit;
  if twfDestIsRawUtf8 in fFlags then // fDest is a RawUtf8 not a TStream
  begin
    inc(fWrittenBytes, len);
    Append(RawUtf8(fDest), data, len); // fDest may be nil = ''
  end
  else if Assigned(fDest) then
    if twfDestIsShortString in fFlags then
    begin // here fDest is a PShortString not a TStream
      inc(fWrittenBytes, len);
      AppendShortBuffer(data, len, fShortStringMax, fDest);
      if PShortString(fDest)^[0] = #255 then
        fDest := nil; // don't write anything anymore
    end
    else
    repeat
      written := TStream(fDest).Write(data^, len);
      if written <= 0 then
        if twfNoWriteToStreamException in fFlags then
          break // silent failure
        else
          ESynException.RaiseUtf8('%.WriteToStream failed on %',
            [self, TStream(fDest)]);
      inc(fWrittenBytes, written);
      dec(len, written);
      if len = 0 then
        break;
      inc(PByte(data), written); // several calls to Write() may be needed
    until false;
end;

function TTextWriter.GetTextLength: Int64;
begin
  result := PtrUInt(self);
  if self <> nil then
    result := PtrInt(B - fTempBuf + 1) + fWrittenBytes - fInitialStreamPosition;
end;

function TTextWriter.GetStream: TStream;
begin
  if (self = nil) or
     (fFlags * [twfDestIsRawUtf8, twfDestIsShortString] <> []) then
    result := nil    // fDest is a PShortString or a RawUtf8
  else
    result := fDest; // here fDest is expected to be a TStream
end;

procedure TTextWriter.SetStream(aStream: TStream);
begin
  if fFlags * [twfDestIsRawUtf8, twfDestIsShortString] <> [] then
    exit; // invalid call
  exclude(fFlags, twfRawByteStringStream);
  if fDest <> nil then
    if twfDestIsOwnedStream in fFlags then
    begin
      FreeAndNilSafe(fDest);
      exclude(fFlags, twfDestIsOwnedStream);
    end;
  if aStream = nil then
    exit;
  fDest := aStream;
  fInitialStreamPosition := aStream.Position;
  fWrittenBytes := fInitialStreamPosition;
  if aStream.InheritsFrom(TRawByteStringStream) then
    include(fFlags, twfRawByteStringStream);
end;

procedure TTextWriter.FlushFinal;
var
  len: PtrInt;
begin
  len := B - fTempBuf + 1;
  if len > 0 then
    WriteToStream(fTempBuf, len);
  B := fTempBuf - 1;
  {$ifdef HASCODEPAGE}
  if twfRawByteStringStream in fFlags then
    TRawByteStringStream(fDest).EnsureDataStringIsUtf8;
  {$endif HASCODEPAGE}
end;

procedure TTextWriter.FlushToStream;
var
  tmp, written: Int64;
begin
  FlushFinal;
  if twfFlushNoAutoResize in fFlags then
    exit;
  written := fWrittenBytes - fInitialStreamPosition;
  tmp := fTempBufSize;
  if (tmp < 49152) and
     (written > tmp * 4) then
    // tune small (stack-allocated?) buffer to grow by twice its size
    fTempBufSize := fTempBufSize * 2
  else if (written > 40 shl 20) and
          (tmp < 1 shl 20) then
    fTempBufSize := 1 shl 20 // total > 40MB -> grow internal buffer once to 1MB
  else
    exit; // nothing to change about internal buffer size
  if twfBufferIsOnStack in fFlags then
    exclude(fFlags, twfBufferIsOnStack) // use heap, not stack from now on
  else
    FreeMem(fTempBuf); // no need to realloc/move the previous buffer content
  GetMem(fTempBuf, fTempBufSize);
  BEnd := fTempBuf + (fTempBufSize - TRAIL_BYTES); // as in SetBuffer()
  B := fTempBuf - 1;
end;

function TTextWriter.FlushToStreamUsing(P: PUtf8Char): PUtf8Char;
begin
  B := P - 1;
  FlushToStream;
  result := fTempBuf;
end;

procedure TTextWriter.ForceContent(const text: RawUtf8);
begin
  CancelAll;
  if (fInitialStreamPosition = 0) and
     (twfRawByteStringStream in fFlags) then
    TRawByteStringStream(fDest).DataString := text
  else
    TStream(fDest).WriteBuffer(pointer(text)^, length(text));
  fWrittenBytes := fInitialStreamPosition + length(text);
end;

procedure TTextWriter.SetText(var result: RawUtf8; reformat: TTextWriterJsonFormat);
var
  Len: PtrInt;
  temp: TTextWriter;
begin
  FlushFinal;
  Len := fWrittenBytes - fInitialStreamPosition;
  if (Len = 0) or
     (twfDestIsShortString in fFlags) then
  begin
    FastAssignNew(result);
    exit;
  end;
  if twfDestIsRawUtf8 in fFlags then // fDest is a RawUtf8 not a TStream
    FastAssignUtf8(result, RawByteString(fDest)) // direct assign
  else if twfRawByteStringStream in fFlags then
    TRawByteStringStream(fDest).GetAsText(fInitialStreamPosition, Len, result)
  else if TStream(fDest).InheritsFrom(TCustomMemoryStream) then
    FastSetString(result, PAnsiChar(TCustomMemoryStream(fDest).Memory) +
                            fInitialStreamPosition, Len)
  else
  begin
    FastSetString(result, Len);
    TStream(fDest).Seek(fInitialStreamPosition, soBeginning);
    if not StreamReadAll(TStream(fDest), pointer(result), Len) then
      FastAssignNew(result);
  end;
  if reformat <> jsonCompact then
  begin
    // reformat using the very same temp buffer but not the same RawUtf8
    temp := DefaultJsonWriter.CreateOwnedStream(fTempBuf, fTempBufSize);
    try
      temp.AddJsonReformat(pointer(result), reformat);
      temp.SetText(result);
    finally
      temp.Free;
    end;
  end;
end;

function TTextWriter.GetTextAsBuffer: PUtf8Char;
begin
  if fWrittenBytes = 0 then // just return the internal buffer
  begin
    B[1] := #0; // include an ending #0 for proper PUtf8Char support
    result := fTempBuf;
    exit;
  end;
  result := nil; // if the TStream has no proper memory buffer to return
  if (fInitialStreamPosition = 0) and
     not (twfDestIsShortString in fFlags) then
    if twfDestIsRawUtf8 in fFlags then // fDest is a RawUtf8 not a TStream
    begin
      FlushFinal;
      result := fDest;
    end
    else if twfRawByteStringStream in fFlags then
    begin
      FlushFinal;
      result := pointer(TRawByteStringStream(fDest).DataString);
    end
    else if TStream(fDest).InheritsFrom(TCustomMemoryStream) then
    begin
      AddDirect(#0); // TCustomMemoryStream needs this ending #0
      FlushFinal;
      result := TCustomMemoryStream(fDest).Memory;
    end;
end;

function TTextWriter.Text: RawUtf8;
begin
  SetText(result);
end;

procedure TTextWriter.CancelAll;
begin
  if self = nil then
    exit; // avoid GPF
  if fWrittenBytes <> 0 then
    if twfDestIsRawUtf8 in fFlags then // fDest is a RawUtf8 not a TStream
    begin
      fWrittenBytes := 0;
      if fDest <> nil then
        FastAssignNew(RawUtf8(fDest)); // seldom called (SetText did reset to nil='')
    end
    else if not (twfDestIsShortString in fFlags) then
      fWrittenBytes := TStream(fDest).Seek(fInitialStreamPosition, soBeginning);
  B := fTempBuf - 1;
end;

procedure TTextWriter.CancelAllAsNew;
begin
  CancelAll;
  fCustomOptions := [];
end;

procedure TTextWriter.CancelAllWith(var temp: TTextWriterStackBuffer);
begin
  CancelAll;
  if twfBufferIsOnStack in fFlags then
    InternalSetBuffer(@temp, SizeOf(temp)); // just refresh the stack buffer
end;

procedure TTextWriter.CancelLastChar(aCharToCancel: AnsiChar);
var
  P: PUtf8Char;
begin
  P := B;
  if (P >= fTempBuf) and
     (P^ = aCharToCancel) then
    dec(B);
end;

procedure TTextWriter.CancelLastChar;
begin
  if B >= fTempBuf then // Add() methods append at B+1
    dec(B);
end;

procedure TTextWriter.CancelLastComma;
var
  P: PUtf8Char;
begin
  P := B;
  if (P >= fTempBuf) and
     (P^ = ',') then
    dec(B);
end;

procedure TTextWriter.ReplaceLastComma(aReplaceChar: AnsiChar);
var
  P: PUtf8Char;
begin
  P := B;
  if (P < fTempBuf) or
     (P^ <> ',') then
  begin
    inc(P);
    B := P;
  end;
  P^ := aReplaceChar;
end;

procedure TTextWriter.CancelLastComma(aReplaceChar: AnsiChar);
begin
  ReplaceLastComma(aReplaceChar);
end;

function TTextWriter.LastChar: AnsiChar;
begin
  if B >= fTempBuf then
    result := B^
  else
    result := #0;
end;

procedure TTextWriter.AddOnce(const c: AnsiChar);
begin
  if (B >= fTempBuf) and
     (B^ = c) then
    exit; // no duplicate
  if B >= BEnd then
    FlushToStream;
  B[1] := c;
  inc(B);
end;

procedure TTextWriter.StrRefConst(s: PStrRecConst);
begin
  PCardinal(B + 1)^ := s^.TextLo; // append up to 4 chars - e.g. UINT_999[]
  inc(B, s^.Header.length);
end;

procedure TTextWriter.Add(const Value: PtrInt);
var
  tmp: TTemp24;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 24 then
    FlushToStream;
  if PtrUInt(Value) <= high(UINT_999) then
    StrRefConst(@UINT_999[Value])
  else
  begin
    P := StrInt32(@tmp[23], Value);
    Len := @tmp[23] - P;
    MoveFast(P^, B[1], Len);
    inc(B, Len);
  end;
end;

{$ifdef CPU32} // Add(Value: PtrInt) already implements it for CPU64
procedure TTextWriter.Add(const Value: Int64);
var
  tmp: TTemp24;
  P: PAnsiChar;
  Len: integer;
begin
  if BEnd - B <= 24 then
    FlushToStream;
  if Value >= 0 then
    if Value <= high(UINT_999) then
    begin
      StrRefConst(@UINT_999[Value]);
      exit;
    end
    else
    begin
      P := StrUInt64(@tmp[23], Value);
      Len := @tmp[23] - P;
    end
  else
  begin
    P := StrUInt64(@tmp[23], -Value) - 1;
    P^ := '-';
    Len := @tmp[23] - P;
  end;
  MoveByOne(P, B + 1, Len);
  inc(B, Len);
end;
{$endif CPU32}

procedure TTextWriter.AddCurr64(Value: PInt64);
var
  tmp: TTemp32;
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
            dec(Len, 5)   // 'xxx.0000' -> 'xxx'
          else
            dec(Len, 3) // 'xxx.1000' -> 'xxx.1'
        else
          dec(Len, 2) // 'xxx.1200' -> 'xxx.12'
      else
        dec(Len);  // 'xxx.1220' -> 'xxx.123'
  MoveFast(P^, B[1], Len);
  inc(B, Len);
end;

procedure TTextWriter.AddCurr(const Value: currency);
begin
  AddCurr64(PInt64(@Value));
end;

procedure TTextWriter.AddU(const Value: PtrUInt);
var
  tmp: TTemp24;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 24 then
    FlushToStream;
  if Value <= high(UINT_999) then
    StrRefConst(@UINT_999[Value])
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    Len := @tmp[23] - P;
    MoveFast(P^, B[1], Len);
    inc(B, Len);
  end;
end;

procedure TTextWriter.AddB(const Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  StrRefConst(@UINT_999[Value]); // caller ensured Value <= 255 < 999
end;

procedure TTextWriter.AddUHex(Value: cardinal; QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(@Value, SizeOf(Value), QuotedChar);
end;

procedure TTextWriter.AddQ(const Value: QWord; Reserve: PtrInt);
var
  tmp: TTemp24;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= Reserve then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  if {$ifndef HASQWORD} (Value >= 0) and {$endif}
     (Value <= high(UINT_999)) then
    StrRefConst(@UINT_999[Value])
  else
  begin
    P := StrUInt64(@tmp[23], Value);
    Len := @tmp[23] - P;
    MoveFast(P^, B[1], Len);
    inc(B, Len);
  end;
end;

procedure TTextWriter.AddQHex(Value: Qword; QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(@Value, SizeOf(Value), QuotedChar);
end;

procedure TTextWriter.AddShort(Text: PUtf8Char; TextLen: PtrInt);
begin
  if TextLen <= 0 then
    exit;
  if BEnd - B <= TextLen then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  MoveFast(Text^, B[1], TextLen);
  inc(B, TextLen);
end;

procedure TTextWriter.AddShort(const Text: ShortString);
begin
  if BEnd - B <= 255 then
    FlushToStream;
  MoveFast(Text[1], B[1], ord(Text[0]));
  inc(B, ord(Text[0]));
end;

procedure TTextWriter.Add(const Value: Extended; precision: integer; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(ExtendedToJson(@tmp, Value, precision, noexp)^);
end;

procedure TTextWriter.AddDouble(const Value: double; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(DoubleToJson(@tmp, Value, noexp)^);
end;

procedure TTextWriter.AddSingle(const Value: single; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(ExtendedToJson(@tmp, Value, SINGLE_PRECISION, noexp)^);
end;

procedure TTextWriter.Add(Value: boolean);
var
  PS: PShortString;
begin
  PS := @BOOL_STR[false];
  if Value then // normalize: boolean may not be in the expected [0,1] range
    inc(PByte(PS), SizeOf(BOOL_STR[false])); // string[7]
  AddShorter(PS^);
end;

procedure TTextWriter.AddU(Value: boolean);
var
  c: AnsiChar;
begin
  if B >= BEnd then
    FlushToStream; // may rewind B -> not worth any local PUtf8Char variable
  inc(B);
  c := '0';
  if Value then
    inc(c);
  B^ := c;
end;

procedure TTextWriter.AddFloatStr(P: PUtf8Char; Len: PtrInt);
begin
  if BEnd - B <= 127 then
    FlushToStream;
  inc(B);
  if Len < 0 then
    Len := mormot.core.base.StrLen(P);
  if (P <> nil) and
     (Len >= 0) and
     (Len < 127) then
    B := FloatStrCopy(P, B) - 1
  else
    B^ := '0';
end;

procedure TTextWriter.Add(Value: PGuid; QuotedChar: AnsiChar; tab: PWordArray);
begin
  if BEnd - B <= 38 then
    FlushToStream;
  inc(B);
  if QuotedChar <> #0 then
  begin
    B^ := QuotedChar;
    inc(B);
  end;
  B := GuidToText(B, pointer(Value), tab);
  if QuotedChar <> #0 then
    B^ := QuotedChar
  else
    dec(B);
end;

procedure TTextWriter.AddCR;
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := EOLW; // CR + LF
  inc(B, 2);
end;

procedure TTextWriter.AddCRAndIndent;
var
  ntabs: PtrUInt;
  p: PUtf8Char;
  c32: cardinal;
begin
  ntabs := fHumanReadableLevel;
  c32 := $09090909;
  if twoIndentSpaces in fCustomOptions then
  begin
    c32 := $20202020;
    ntabs := ntabs * 2; // indent by two spaces instead of a single #9 tab
  end;
  p := B;
  if (p >= fTempBuf) and
     (ord(p^) = ToByte(c32)) then
    exit; // we just already added an indentation level - do it once
  if ntabs >= PtrUInt(fTempBufSize) then
    ntabs := 0; // fHumanReadableLevel=-1 after the last level of a document
  if PtrInt(BEnd - p) <= PtrInt(ntabs) then // note: PtrInt(BEnd - B) could be < 0
  begin
    FlushToStream;
    p := B;
  end;
  inc(p);
  if twoEndOfLineCRLF in fCustomOptions then
  begin
    PCardinal(p)^ := EOLW;
    inc(p);
  end
  else
    p^ := #10;
  PCardinal(p + 1)^ := c32; // #9#9#9#9 or #32#32#32#32
  if ntabs > 4 then
    FillCharFast(p[5], ntabs - 4, ToByte(c32)); // #9 or #32
  B := @p[ntabs];
end;

procedure TTextWriter.AddChars(aChar: AnsiChar; aCount: PtrInt);
begin
  if aCount <= 0 then
    exit;
  if PtrInt(BEnd - B) < aCount then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  FillCharFast(B[1], MinPtrInt(aCount, fTempBufSize), ord(aChar));
  inc(B, aCount);
end;

procedure TTextWriter.Add2(Value: cardinal);
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 99 then
    Value := $3030 + ord(',') shl 16
  else     // '00,' if overflow
    Value := TwoDigitLookupW[Value] + ord(',') shl 16;
  PCardinal(B + 1)^ := Value;
  inc(B, 3);
end;

procedure TTextWriter.Add3(Value: cardinal);
var
  V: cardinal;
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 999 then
    Value := $303030 // '000,' if overflow
  else
  begin
    V := Value div 10;
    Value := TwoDigitLookupW[V] + (Value - V * 10 + 48) shl 16;
  end;
  PCardinal(B + 1)^ := Value;
  inc(B, 4);
  B^ := ',';
end;

procedure TTextWriter.Add4(Value: PtrUInt);
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

procedure TTextWriter.AddCsvStrings(const Values: array of RawUtf8;
  const Sep: RawUtf8; HighValues: PtrInt; Reverse: boolean);
begin
  if HighValues < 0 then
    HighValues := high(Values);
  if HighValues >= 0 then
    AddCsvStrings(@Values[0], HighValues, Sep, Reverse);
end;

procedure TTextWriter.AddCsvStrings(Values: PRawUtf8Array; HighValues: PtrInt;
  const Sep: RawUtf8; Reverse: boolean);
var
  i: PtrInt;
begin
  if HighValues < 0 then
    exit;
  i := 0;
  if Reverse then
  begin
    i := HighValues;
    HighValues := 0;
  end;
  repeat
    AddString(Values^[i]); // fast enough
    if i = HighValues then
      break;
    if Sep <> '' then
      AddShort(pointer(Sep), PStrLen(PtruInt(Sep) - _STRLEN)^);
    if Reverse then
      dec(i)
    else
      inc(i);
  until false;
end;

procedure TTextWriter.AddCsvInteger(const Integers: array of integer);
var
  i: PtrInt;
begin
  if length(Integers) = 0 then
    exit;
  for i := 0 to high(Integers) do
  begin
    Add(Integers[i]);
    AddComma;
  end;
  CancelLastComma;
end;

procedure TTextWriter.AddCsvDouble(const Doubles: array of double);
var
  i: PtrInt;
begin
  if length(Doubles) = 0 then
    exit;
  for i := 0 to high(Doubles) do
  begin
    AddDouble(Doubles[i]);
    AddComma;
  end;
  CancelLastComma;
end;

procedure TTextWriter.AddNoJsonEscapeBig(P: pointer; Len: PtrInt);
var
  direct: PtrInt;
  D: PUtf8Char;
  comma: boolean;
begin
  if (P <> nil) and
     (Len > 0) then
    if Len < fTempBufSize * 2 then // also happen when FlushToStream is needed
      repeat
        D := B + 1;
        direct := BEnd - D; // guess biggest size available in fTempBuf at once
        if direct > 0 then  // 0..-15 may happen because Add up to TRAIL_BYTES
        begin
          if Len < direct then
            direct := Len;
          MoveFast(P^, D^, direct); // fill fTempBuf as much as possible
          inc(B, direct);
          dec(Len, direct);
          if Len = 0 then
            break;
          inc(PByte(P), direct);
        end;
        FlushToStream;
      until false
    else
    begin
      FlushFinal; // no auto-resize if content is really huge
      comma := PAnsiChar(P)[Len - 1] = ',';
      if comma then
        dec(Len);
      WriteToStream(P, Len); // no need to transit huge content into fTempBuf
      if comma then
        AddDirect(','); // but we need the last comma to be cancelable
    end;
end;

procedure TTextWriter.AddNoJsonEscape(P: pointer; Len: PtrInt);
begin
  if (P <> nil) and
     (Len > 0) then
    if BEnd - B >= Len then // note: PtrInt(BEnd - B) could be < 0
    begin
      MoveFast(P^, B[1], Len); // efficient inlining for small chunks
      inc(B, Len);
    end
    else
      AddNoJsonEscapeBig(P, Len); // big chunks or need flush (hardly the case)
end;

procedure TTextWriter.AddNoJsonEscape(P: pointer);
begin
  if P <> nil then
    AddNoJsonEscape(P, mormot.core.base.StrLen(PUtf8Char(P)));
end;

procedure TTextWriter.AddNoJsonEscapeCP(P: PAnsiChar; Len: PtrInt; CodePage: cardinal);
var
  engine: TSynAnsiConvert;
  max: PtrInt;
  tmp: pointer; // temporary RawUtf8 buffer
begin
  if (P = nil) or
     (Len <= 0) then
    exit;
  if CodePage = CP_ACP then // CP_UTF8 is very likely on POSIX or LCL
    CodePage := Unicode_CodePage; // = CurrentAnsiConvert.CodePage
  case CodePage of
    CP_UTF8, CP_RAWBYTESTRING, CP_RAWBLOB:
      AddNoJsonEscape(P, Len);
    CP_UTF16:
      AddNoJsonEscapeW(PWord(P));
  else
    begin
      engine := TSynAnsiConvert.Engine(CodePage);
      max := Len * 3;
      if max < fTempBufSize then // write directly into the output buffer
      begin
        if BEnd - B <= max then  // note: PtrInt(BEnd - B) could be < 0
          FlushToStream;
        B := engine.AnsiBufferToUtf8(B + 1, P, Len, {notrail0=}true) - 1;
      end
      else
      begin
        tmp := FastNewString(max); // allocate temporary (big) buffer
        P := pointer(engine.AnsiBufferToUtf8(tmp, P, Len, {notrail0=}true));
        AddNoJsonEscape(tmp, P - tmp);
        FastAssignNewNotVoid(tmp);
      end;
    end;
  end;
end;

procedure TTextWriter.AddRawJson(const json: RawJson);
begin
  if json = '' then
    AddShort4(NULL_LOW)
  else
    AddString(json);
end;

procedure TTextWriter.AddNoJsonEscapeString(const s: string);
begin
  if pointer(s) <> nil then
    {$ifdef UNICODE}
    AddNoJsonEscapeW(pointer(s));
    {$else}
    AddNoJsonEscapeCP(pointer(s), PStrLen(PAnsiChar(pointer(s)) - _STRLEN)^,
      Unicode_CodePage);
    {$endif UNICODE}
end;

procedure TTextWriter.AddWideCharNext(var PW: PWord);
var
  c: cardinal;
begin
  if B >= BEnd then
    FlushToStream;
  c := PW^;
  inc(PW);
  if c <= $7f then
    AddDirect(AnsiChar(c)) // most obvious case
  else
    inc(B, Utf16HiCharToUtf8(B + 1, c, PW)); // handle UTF-16 surrogates
end;

procedure TTextWriter.AddWideChar(W: WideChar);
begin
  if B >= BEnd then
    FlushToStream;
  inc(B, IsoUcsToUtf8(ord(W), B + 1));
end;

procedure TTextWriter.AddUcs4(ucs4: Ucs4CodePoint);
begin
  if B >= BEnd then
    FlushToStream;
  inc(B, Ucs4ToUtf8(ucs4, B + 1)); // ucs4 is the UTF-32/UCS-4 code point
end;

procedure TTextWriter.AddNoJsonEscapeW(PW: PWord; WideCharCount: integer);
var
  PEnd: PtrUInt;
  c: cardinal;
begin // only called from AddQuotedStrW()
  if (PW = nil) or
     (WideCharCount <= 0) then
    exit;
  PEnd := PtrUInt(PW) + PtrUInt(WideCharCount) * SizeOf(PW^);
  repeat
    if B >= BEnd then
      FlushToStream;
    c := PW^;
    inc(PW);
    if c <= $7f then
    begin
      if c = 0 then
        exit;
      B[1] := AnsiChar(c);
      inc(B);
    end
    else
      inc(B, Utf16HiCharToUtf8(B + 1, c, PW)); // handle UTF-16 surrogates
  until PtrUInt(PW) >= PEnd;
end;

procedure TTextWriter.AddNoJsonEscapeW(PW: PWord);
var
  dst: PUtf8Char;
  c: cardinal;
begin
  if PW = nil then
    exit;
  dst := B + 1;
  if dst > BEnd then
    dst := FlushToStreamUsing(dst);
  repeat
    c := PW^;
    inc(PW);
    if c <= $7f then
    begin
      if c = 0 then
        break;
      dst^ := AnsiChar(c);
      inc(dst);
      if dst <= BEnd then
        continue;
    end
    else
    begin
      inc(dst, Utf16HiCharToUtf8(dst, c, PW));
      if dst <= BEnd then
        continue;
    end;
    dst := FlushToStreamUsing(dst);
  until false;
  B := dst - 1;
end;

procedure TTextWriter.AddProp(PropName: PUtf8Char);
begin
  AddProp(PropName, mormot.core.base.StrLen(PropName));
end;

procedure TTextWriter.AddProp(PropName: PUtf8Char; PropNameLen: PtrInt);
begin // not faster with a local P: PUtf8Char temp pointer instead of B
  if PropNameLen <= 0 then
    exit; // paranoid check
  if BEnd - B <= PropNameLen then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  if twoForceJsonExtended in fCustomOptions then
  begin
    MoveFast(PropName^, B[1], PropNameLen);
    inc(B, PropNameLen + 1);
    B^ := ':';
  end
  else
  begin
    B[1] := '"';
    MoveFast(PropName^, B[2], PropNameLen);
    inc(B, PropNameLen + 2);
    PCardinal(B)^ := ord('"') + ord(':') shl 8;
    inc(B);
  end;
end;

procedure TTextWriter.AddPropName(const PropName: ShortString);
begin
  AddProp(@PropName[1], ord(PropName[0]));
end;

procedure TTextWriter.AddPropName(PropName: PtrUInt);
var
  tmp: TTemp24;
  P: PAnsiChar;
begin
  P := StrUInt32(@tmp[23], PropName);
  AddProp(PUtf8Char(P), @tmp[23] - P);
end;

procedure TTextWriter.AddPropInt64(const PropName: ShortString;
  Value: Int64; WithQuote: AnsiChar);
begin
  AddProp(@PropName[1], ord(PropName[0]));
  if WithQuote <> #0 then
  begin
    B[1] := WithQuote;
    inc(B);
  end;
  Add(Value);
  inc(B);
  if WithQuote <> #0 then
  begin
    B^ := WithQuote;
    inc(B);
  end;
  B^ := ',';
end;

procedure TTextWriter.AddFieldName(const FieldName: RawUtf8);
begin
  AddProp(pointer(FieldName), length(FieldName));
end;

procedure TTextWriter.AddQuotedFieldName(const FieldName, VoidPlaceHolder: RawUtf8);
begin
  AddQuotedFieldName(pointer(FieldName), length(FieldName), VoidPlaceHolder);
end;

procedure TTextWriter.AddQuotedFieldName(
  FieldName: PUtf8Char; FieldNameLen: PtrInt; const VoidPlaceHolder: RawUtf8);
begin
  if FieldNameLen = 0 then
  begin
    FieldName := pointer(VoidPlaceHolder);
    FieldNameLen := length(VoidPlaceHolder);
  end;
  if BEnd - B <= FieldNameLen then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  B[1] := '"';
  MoveFast(FieldName^, B[2], FieldNameLen);
  inc(B, FieldNameLen + 2);
  B^ := '"';
end;

procedure TTextWriter.AddClassName(aClass: TClass);
begin
  if aClass <> nil then
    AddShort(ClassNameShort(aClass)^);
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

function PointerToText(Instance: TObject; Dest: PUtf8Char;
  IncludeUnitName, IncludePointer: boolean): PUtf8Char;
var
  s: PShortString;
  l: PtrInt;
begin
  if IncludeUnitName then
  begin
    s := ClassUnit(PClass(Instance)^); // we know Instance <> nil
    if s^[0] <> #0 then
    begin
      MoveFast(s^[1], Dest^, ord(s^[0]));
      inc(Dest, ord(s^[0]));
      Dest^ := '.';
      inc(Dest);
    end;
  end;
  s := PPShortString(PPAnsiChar(Instance)^ + vmtClassName)^;
  MoveFast(s^[1], Dest^, ord(s^[0]));
  inc(Dest, ord(s^[0]));
  if IncludePointer then
  begin
    Dest^ := '(';
    inc(Dest);
    l := DisplayMinChars(@Instance, SizeOf(Instance));
    BinToHexDisplayLower(@Instance, pointer(Dest), l);
    inc(Dest, l * 2);
    Dest^ := ')';
    inc(Dest);
  end;
  result := Dest;
end;

procedure TTextWriter.AddInstancePointer(Instance: TObject; SepChar: AnsiChar;
  IncludeUnitName, IncludePointer: boolean);
var
  P: PUtf8Char;
begin
  if BEnd - B <= 255 then
    FlushToStream;
  P := PointerToText(Instance, B + 1, IncludeUnitName, IncludePointer);
  P^ := SepChar;
  if SepChar = #0 then
    dec(P);
  B := P;
end;

procedure TTextWriter.AddInstanceName(Instance: TObject; SepChar: AnsiChar);
begin // inlined AddInstancePointer() with optional quotes
  if BEnd - B <= 255 then
    FlushToStream;
  if not (twoForceJsonExtended in fCustomOptions) then
    AddDirect('"');
  if Instance = nil then
    AddDirect('v', 'o', 'i', 'd')
  else
    B := PointerToText(Instance, B + 1, {unitname=}false, {pointer=}true) - 1;
  if not (twoForceJsonExtended in fCustomOptions) then
    AddDirect('"');
  if SepChar <> #0 then
    AddDirect(SepChar);
end;

procedure TTextWriter.AddLine(const Text: ShortString);
var
  L: PtrInt;
begin
  L := ord(Text[0]);
  if BEnd - B <= L then // note: PtrInt(BEnd - B) could be < 0
    FlushToStream;
  inc(B);
  if L > 0 then
  begin
    MoveFast(Text[1], B^, L);
    inc(B, L);
  end;
  PCardinal(B)^ := EOLW; // CR + LF
  inc(B);
end;

procedure TTextWriter.AddOnSameLine(P: PUtf8Char);
var
  l: PtrInt;
begin // mostly used for TSynLog RawUtf8 append
  if (P <> nil) and
     (P^ <> #0) then
    repeat
      if P^ >= ' ' then
      begin
        l := 0;
        repeat
          inc(l);
        until P[l] < ' ';
        AddNoJsonEscape(P, l); // efficient MoveFast()
        inc(P, l);
        if P^ = #0 then
          exit; // most common case
      end;
      repeat
        inc(P);
        if P^ = #0 then
          exit;
      until P^ > ' ';
      AddOnce(' ');
    until false;
end;

procedure TTextWriter.AddOnSameLine(P: PUtf8Char; Len: PtrInt);
var
  i, s: PtrInt;
begin // mostly used for TSynLog ShortString append or Reformat() comments
  i := 0;
  if (P <> nil) and
     (i < Len) then
    repeat
      if P[i] >= ' ' then
      begin
        s := i;
        repeat
          inc(i);
        until (i = Len) or
              (P[i] < ' ');
        AddNoJsonEscape(P + s, i - s); // efficient MoveFast()
        if i = Len then
          exit; // most common case
      end;
      repeat
        inc(i);
        if i = Len then
          exit;
      until P[i] > ' ';
      AddOnce(' ');
    until false;
end;

procedure TTextWriter.AddOnSameLineW(P: PWord);
var
  src: PWord;
  dst: PUtf8Char;
  c: cardinal;
begin
  src := P;
  if src = nil then
    exit;
  dst := B + 1;
  repeat
    if dst > BEnd then
      dst := FlushToStreamUsing(dst);
    c := src^;
    inc(src);
    if c <= $7f then
    begin
      if c < 32 then
        if c = 0 then
          break
        else
          dst^ := ' ' // ensure stay on the same line
      else
        dst^ := AnsiChar(c); // direct store 7-bit ASCII
      inc(dst);
    end
    else
    begin
      P := src; // need a local pointer to handle surrogates
      inc(dst, Utf16HiCharToUtf8(dst, c, P)); // convert UTF-16 to UTF-8
      src := P;
    end;
  until false;
  B := dst - 1;
end;

procedure TTextWriter.AddOnSameLineString(const Text: string);
begin
  {$ifdef UNICODE}AddOnSameLineW{$else}AddOnSameLine{$endif}(pointer(Text));
end;

procedure TTextWriter.AddTrimLeftLowerCase(Text: PShortString);
var
  P: PAnsiChar;
  L: PtrInt;
begin
  L := TrimLeftLowerCaseP(Text, P);
  AddShort(pointer(P), L);
end;

procedure TTextWriter.AddTrimSpaces(const Text: RawUtf8);
begin
  AddTrimSpaces(pointer(Text));
end;

procedure TTextWriter.AddTrimSpaces(P: PUtf8Char);
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

procedure TTextWriter.AddReplace(Text: PUtf8Char; Orig, Replaced: AnsiChar);
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

procedure TTextWriter.AddByteToHex(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := TwoDigitsHex[Value];
  inc(B, 2);
end;

procedure TTextWriter.AddByteToHexLower(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := TwoDigitsHexLower[Value];
  inc(B, 2);
end;

procedure TTextWriter.AddString(const Text: RawUtf8);
var
  l: PtrInt;
begin // inlined AddNoJsonEscape(pointer(text), length(text))
  if pointer(Text) = nil then
    exit;
  l := PStrLen(PAnsiChar(pointer(Text)) - _STRLEN)^;
  if BEnd - B >= l then // note: PtrInt(BEnd - B) could be < 0
  begin
    MoveFast(pointer(Text)^, B[1], l); // efficient inlining for small chunks
    inc(B, l);
  end
  else
    AddNoJsonEscapeBig(pointer(Text), l);
end;

procedure TTextWriter.AddSpaced(Text: PUtf8Char; TextLen, Width: PtrInt);
begin
  if Width <= TextLen then
    TextLen := Width // truncate text right
  else
    AddChars(' ', Width - TextLen);
  AddNoJsonEscape(Text, TextLen);
end;

procedure TTextWriter.AddSpaced(const Text: RawUtf8; Width: PtrInt;
  SepChar: AnsiChar);
begin
  AddSpaced(pointer(Text), length(Text), Width);
  if SepChar <> #0 then
    Add(SepChar);
end;

procedure TTextWriter.AddStringCopy(const Text: RawUtf8; start, len: PtrInt);
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

procedure TTextWriter.AddStrings(const Values: array of RawUtf8);
var
  i: PtrInt;
  p: PPUtf8Char;
begin
  p := @Values[0];
  for i := 0 to high(Values) do
  begin
    if p^ <> nil then
      AddNoJsonEscape(p^, PStrLen(p^ - _STRLEN)^);
    inc(p);
  end;
end;

procedure TTextWriter.AddStrings(const Text: RawUtf8; count: PtrInt);
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
      if BEnd - B <= siz then // note: PtrInt(BEnd - B) could be < 0
        FlushToStream;
      for i := 1 to count do
      begin
        MoveFast(pointer(Text)^, B[1], L); // direct in-memory append
        inc(B, L);
      end;
    end;
end;

procedure TTextWriter.AddBinToHexDisplay(Bin: pointer; BinBytes: PtrInt);
var
  max: PtrInt;
begin
  max := BinBytes * 2 + 1;
  if BEnd - B <= max then // note: PtrInt(BEnd - B) could be < 0
    if PtrUInt(max) >= PtrUInt(fTempBufSize) then
      exit // too big for a single call
    else
      FlushToStream;
  BinToHexDisplay(Bin, PAnsiChar(B + 1), BinBytes);
  inc(B, BinBytes * 2);
end;

procedure TTextWriter.AddBinToHexDisplayLower(Bin: pointer; BinBytes: PtrInt;
  QuotedChar: AnsiChar);
var
  max: PtrInt;
  P: PUtf8Char;
begin
  if Bin = nil then
    exit;
  max := BinBytes * 2 + 1;
  if BEnd - B <= max then // note: PtrInt(BEnd - B) could be < 0
    if PtrUInt(max) >= PtrUInt(fTempBufSize) then
      exit // too big for a single call
    else
      FlushToStream;
  P := B + 1;
  if QuotedChar <> #0 then
  begin
    P^ := QuotedChar;
    inc(P);
  end;
  BinToHexDisplayLower(Bin, pointer(P), BinBytes);
  inc(P, BinBytes * 2);
  P^ := QuotedChar;
  if QuotedChar = #0 then
    dec(P);
  B := P;
end;

procedure TTextWriter.AddBinToHexDisplayQuoted(Bin: pointer; BinBytes: PtrInt);
begin
  AddBinToHexDisplayLower(Bin, BinBytes, '"');
end;

procedure TTextWriter.AddBinToHexDisplayMinChars(Bin: pointer; BinBytes: PtrInt;
  QuotedChar: AnsiChar);
begin
  if BinBytes > 0 then
    AddBinToHexDisplayLower(Bin, DisplayMinChars(Bin, BinBytes), QuotedChar);
end;

procedure TTextWriter.AddPointer(P: PtrUInt; QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(@P, DisplayMinChars(@P, SizeOf(P)), QuotedChar);
end;

procedure TTextWriter.AddBinToHumanHex(Bin: pointer; BinBytes: PtrInt;
  QuotedChar: AnsiChar; Reverse: boolean);
var
  P: PAnsiChar;
begin
  P := AddPrepare(BinBytes * 3);
  if P = nil then
    exit; // too big
  P^ := QuotedChar;
  if QuotedChar <> #0 then
    inc(P);
  ToHumanHexP(P, Bin, BinBytes, Reverse);
  inc(P, BinBytes * 3 - 1);
  P^ := QuotedChar;
  if QuotedChar = #0 then
    dec(P);
  B := pointer(P);
end;

procedure TTextWriter.AddBinToHex(Bin: pointer; BinBytes: PtrInt;
  LowerHex: boolean; QuotedChar: AnsiChar);
var
  chunk: PtrInt;
begin
  if BinBytes <= 0 then
    exit;
  if B >= BEnd then
    FlushToStream;
  inc(B);
  if QuotedChar <> #0 then
  begin
    B^ := QuotedChar;
    inc(B);
  end;
  repeat
    // guess biggest size to be added into buf^ at once
    chunk := (BEnd - B) shr 1; // div 2 -> two hexa chars per byte
    if BinBytes < chunk then
      chunk := BinBytes;
    // add hexa characters
    if LowerHex then
      mormot.core.text.BinToHexLower(PAnsiChar(Bin), PAnsiChar(B), chunk)
    else
      mormot.core.text.BinToHex(PAnsiChar(Bin), PAnsiChar(B), chunk);
    inc(B, chunk * 2);
    dec(BinBytes, chunk);
    if BinBytes = 0 then
      break;
    inc(PByte(Bin), chunk);
    // FlushToStream writes B-fTempBuf+1 -> need custom code here
    WriteToStream(fTempBuf, B - fTempBuf);
    B := fTempBuf;
  until false;
  if QuotedChar <> #0 then
    B^ := QuotedChar
  else
    dec(B); // allow CancelLastChar
end;

procedure TTextWriter.AddBinToHexMinChars(Bin: pointer; BinBytes: PtrInt;
  LowerHex: boolean; QuotedChar: AnsiChar);
begin
  if BinBytes > 0 then
    AddBinToHex(Bin, DisplayMinChars(Bin, BinBytes), LowerHex, QuotedChar);
end;

procedure TTextWriter.AddQuotedStr(Text: PUtf8Char; TextLen: PtrUInt;
  Quote: AnsiChar; TextMaxLen: PtrInt);
var
  q: PtrInt;
begin
  Add(Quote);
  if (TextMaxLen > 5) and
     (TextLen > PtrUInt(TextMaxLen)) then
    TextLen := TextMaxLen - 5
  else
    TextMaxLen := 0;
  if Text <> nil then
  begin
    repeat
      q := ByteScanIndex(pointer(Text), TextLen, byte(Quote)); // may use SSE2
      if q < 0 then
      begin
        AddNoJsonEscape(Text, TextLen); // no double quote
        break;
      end;
      inc(q); // include first Quote
      AddNoJsonEscape(Text, q);
      AddDirect(Quote); // double Quote
      inc(Text, q); // continue
      dec(TextLen, q);
    until TextLen = 0;
    if TextMaxLen <> 0 then
      AddDirect('.', '.', '.');
  end;
  AddDirect(Quote);
end;

procedure TTextWriter.AddQuotedStrW(Text: PWideChar; TextLen: PtrUInt;
  Quote: AnsiChar; TextMaxLen: PtrInt);
var
  q: PtrInt;
begin
  Add(Quote);
  if (TextMaxLen > 5) and
     (TextLen > PtrUInt(TextMaxLen)) then
    TextLen := TextMaxLen - 5
  else
    TextMaxLen := 0;
  if Text <> nil then
  begin
    repeat
      q := WordScanIndex(pointer(Text), TextLen, byte(Quote));
      if q < 0 then
      begin
        AddNoJsonEscapeW(pointer(Text), TextLen); // no quote
        break;
      end;
      inc(q); // include first Quote
      AddNoJsonEscapeW(pointer(Text), q);
      AddDirect(Quote); // double Quote
      inc(Text, q);
      dec(TextLen, q);
    until TextLen = 0;
    if TextMaxLen <> 0 then
      AddDirect('.', '.', '.');
  end;
  AddDirect(Quote);
end;

procedure TTextWriter.AddUrlNameNormalize(U: PUtf8Char; L: PtrInt);
begin
  if (L <= 0) or
     (U^ <> '/') then
  begin
    inc(B);
    B^ := '/'; // a normalized URI should start with '/'
    if L <= 0 then
      exit;
  end;
  repeat
    if B >= BEnd then
      FlushToStream; // inlined Add() in the loop
    inc(B);
    case U^ of
      #0:
        begin
          dec(B); // reached end of URI (should not happen if L is accurate)
          break;
        end;
      '%':
        if (L <= 2) or
           not HexToChar(PAnsiChar(U + 1), B) then
          B^ := '%'  // browsers may not follow the RFC (e.g. encode % as % !)
        else
        begin
          inc(U, 2); // jump %xx
          dec(L, 2);
        end;
      '/':
         if (L = 1) or
            (U[1] <> '/') then
           B^ := '/'
         else
           dec(B); // normalize URI by ignoring this first /
    else
      B^ := U^;
    end;
    inc(U);
    dec(L);
  until L = 0;
end;

procedure __AddHtmlEscape(W: TTextWriter; Text: PUtf8Char; TextLen: PtrInt;
  Fmt: TTextWriterHtmlFormat);
begin
  ESynException.RaiseUtf8('%.AddHtmlEscape requires mormot.core.fmt', [W]);
end;

procedure TTextWriter.AddHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat);
begin
  _AddHtmlEscape(self, Text, {TextLen=}0, Fmt); // in mormot.core.fmt.pas
end;

procedure TTextWriter.AddHtmlEscape(Text: PUtf8Char; TextLen: PtrInt;
  Fmt: TTextWriterHtmlFormat);
begin
  if TextLen > 0 then
    _AddHtmlEscape(self, Text, TextLen, Fmt); // in mormot.core.fmt.pas
end;

procedure TTextWriter.AddHtmlEscapeW(Text: PWideChar; Fmt: TTextWriterHtmlFormat);
var
  tmp: TSynTempBuffer;
begin
  if Text <> nil then
    if Fmt <> hfNone then
    begin
      RawUnicodeToUtf8(Text, mormot.core.base.StrLenW(Text), tmp, []);
      if tmp.len <> 0 then
      begin
        _AddHtmlEscape(self, tmp.buf, {TextLen=}0, Fmt); // faster TextLen=0
        tmp.Done;
      end;
    end
    else
      AddNoJsonEscapeW(pointer(Text)); // seldom called
end;

{$ifdef UNICODE}
procedure TTextWriter.AddHtmlEscapeString(const Text: string; Fmt: TTextWriterHtmlFormat);
begin
  AddHtmlEscapeW(pointer(Text), Fmt);
end;
{$else}
procedure TTextWriter.AddHtmlEscapeString(const Text: string; Fmt: TTextWriterHtmlFormat);
var
  tmp: TSynTempBuffer;
  p: PUtf8Char;
begin
  p := StringToUtf8Temp(Text, tmp);
  if tmp.Len <> 0 then
    _AddHtmlEscape(self, p, tmp.len, Fmt); // in mormot.core.fmt.pas
  tmp.Done;
end;
{$endif UNICODE}


function ObjectToJson(Value: TObject; Options: TTextWriterWriteObjectOptions): RawUtf8;
begin
  ObjectToJson(Value, result, Options);
end;

procedure ObjectToJson(Value: TObject; var Result: RawUtf8;
  Options: TTextWriterWriteObjectOptions);
var
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
begin
  if Value = nil then
    Result := NULL_STR_VAR
  else
    with DefaultJsonWriter.CreateOwnedStream(temp) do
    try
      fCustomOptions := [twoForceJsonStandard];
      WriteObject(Value, Options);
      SetText(Result);
    finally
      Free;
    end;
end;

function ObjectToJsonDebug(Value: TObject): RawUtf8;
begin
  // our JSON serialization properly detects and serializes Exception.Message
  ObjectToJson(Value, result, TEXTWRITEROPTIONS_DEBUG);
end;

procedure ConsoleObject(Value: TObject; Options: TTextWriterWriteObjectOptions);
begin
  ConsoleWrite(ObjectToJson(Value, Options));
end;

function NeedsEscape(text: PUtf8Char; const toescape: TSynAnsicharSet): boolean;
var
  c: AnsiChar;
begin
  result := true;
  if text <> nil then
    repeat
      c := text^;
      if c = #0 then
        break
      else if c in toescape then
        exit
      else
        inc(text);
    until false;
  result := false;
end;

function EscapeHexBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar): PUtf8Char;
var
  c: AnsiChar;
  hex: PByteToWord; // better code generation on x86_64 and arm
begin
  hex := @TwoDigitsHex;
  result := dest;
  if srclen > 0 then
    repeat
      c := src^;
      if c in toescape then
      begin
        result^ := escape;
        PWord(result + 1)^ := hex[ord(c)];
        inc(result, 3);
      end
      else
      begin
        result^ := c;
        inc(result);
      end;
      inc(src);
      dec(srclen);
    until srclen = 0;
end;

function EscapeHex(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar): RawUtf8;
var
  l: PtrInt;
begin
  if not NeedsEscape(pointer(src), toescape) then
  begin
    result := src; // obvious
    exit;
  end;
  l := length(src);
  if l <> 0 then
  begin
    FastSetString(result, l * 3); // allocate maximum size
    l := EscapeHexBuffer(pointer(src), pointer(result), l,
      toescape, escape) - pointer(result);
  end;
  FakeSetLength(result, l); // return in-place with no realloc
end;

function EscapeHexBuffer(src, dest: PUtf8Char; srclen: integer;
  toescape, escape: AnsiChar): PUtf8Char;
var
  c: AnsiChar;
  hex: PByteToWord; // better code generation on x86_64 and arm
begin
  hex := @TwoDigitsHex;
  result := dest;
  if srclen > 0 then
    repeat
      c := src^;
      if c = toescape then
      begin
        result^ := escape;
        PWord(result + 1)^ := hex[ord(c)];
        inc(result, 3);
      end
      else
      begin
        result^ := c;
        inc(result);
      end;
      inc(src);
      dec(srclen);
    until srclen = 0;
end;

function EscapeHex(const src: RawUtf8; toescape, escape: AnsiChar): RawUtf8;
var
  l: PtrInt;
begin
  if PosExChar(toescape, src) = 0 then
  begin
    result := src; // obvious
    exit;
  end;
  l := length(src);
  FastSetString(result, l * 3); // allocate maximum size
  FakeSetLength(result, EscapeHexBuffer(pointer(src), pointer(result), l,
    toescape, escape) - pointer(result));
end;

function UnescapeHexBuffer(src, dest: PUtf8Char; escape: AnsiChar): PUtf8Char;
var
  c: AnsiChar;
begin
  result := dest;
  if src <> nil then
    while src^ <> #0 do
    begin
      if src^ = escape then
      begin
        inc(src);
        if src^ in [#10, #13] then // \CRLF or \LF
        begin
          repeat
            inc(src);
          until not (src^ in [#10, #13]);
          continue;
        end
        else if HexToChar(PAnsiChar(src), @c) then // \xx
        begin
          result^ := c;
          inc(src, 2);
          inc(result);
          continue;
        end;
        if src^ = #0 then // unexpected \c into c (drop the escape)
          break;
      end;
      result^ := src^;
      inc(src);
      inc(result);
    end;
end;

procedure UnescapeHex(var dst: RawUtf8; src: PUtf8Char; srclen: PtrInt; escape: AnsiChar);
begin
  FastSetString(dst, srclen); // allocate maximum size
  FakeSetLength(dst, UnescapeHexBuffer(src, pointer(dst), escape) - pointer(dst));
end;

function UnescapeHex(const src: RawUtf8; escape: AnsiChar): RawUtf8;
begin
  if PosExChar(escape, src) = 0 then
    result := src // no unescape needed
  else
    UnescapeHex(result, pointer(src), length(src), escape);
end;

function EscapeCharBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar): PUtf8Char;
begin
  result := dest;
  if srclen > 0 then
    repeat
      if src^ in toescape then
      begin
        result^ := escape;
        inc(result);
      end;
      result^ := src^;
      inc(result);
      inc(src);
      dec(srclen);
    until srclen = 0;
end;

function EscapeChar(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar): RawUtf8;
var
  l: PtrInt;
begin
  l := length(src);
  if l <> 0 then
  begin
    FastSetString(result, l * 2); // allocate maximum size
    l := EscapeCharBuffer(pointer(src), pointer(result), l,
      toescape, escape) - pointer(result);
  end;
  FakeSetLength(result, l); // return in-place with no realloc
end;


{ TEchoWriter }

constructor TEchoWriter.Create(Owner: TTextWriter);
begin
  fWriter := Owner;
  if Assigned(fWriter.OnFlushToStream) then
    ESynException.RaiseUtf8('Unexpected %.Create', [self]);
  fWriter.OnFlushToStream := FlushToStream; // register
  if twoEndOfLineCRLF in fWriter.CustomOptions then
  begin
    fWriteLineFeed := EOLW; // #13#10 - typical on Windows
    fWriteLineFeedLen := 2;
  end
  else
  begin
    fWriteLineFeed := $0a;  // #10 - as on POSIX
    fWriteLineFeedLen := 1;
  end
end;

destructor TEchoWriter.Destroy;
begin
  if (fWriter <> nil) and
     (TMethod(fWriter.OnFlushToStream).Data = self) then
    fWriter.OnFlushToStream := nil; // unregister
  inherited Destroy;
end;

procedure TEchoWriter.EchoPendingToBackground(aLevel: TSynLogLevel);
var
  n, cap: PtrInt;
begin
  fBackSafe.Lock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    n := fBack.Count;
    if length(fBack.Level) = n then
    begin
      cap := NextGrow(n);
      SetLength(fBack.Level, cap);
      SetLength(fBack.Text, cap);
    end;
    fBack.Level[n] := aLevel;
    fBack.Text[n] := fEchoBuf;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fBackSafe.UnLock;
  end;
end;

procedure TEchoWriter.EchoAddEndOfLine(aLevel: TSynLogLevel);
var
  e: PtrInt;
begin
  fEchoStart := EchoFlush; // fill fEchoBuf with current line
  if fEchoPendingExecuteBackground then
    EchoPendingToBackground(aLevel)
  else
    for e := length(fEchos) - 1 downto 0 do // for MultiEventRemove() below
      try
        fEchos[e](self, aLevel, fEchoBuf);
      except // remove callback in case of exception during echoing
        MultiEventRemove(fEchos, e);
      end;
  fEchoBuf := '';
end;

procedure TEchoWriter.AddEndOfLine(aLevel: TSynLogLevel);
begin
  PCardinal(fWriter.B + 1)^ := fWriteLineFeed; // fast append #13 or #13#10
  inc(fWriter.B, fWriteLineFeedLen);
  if fEchos <> nil then
    EchoAddEndOfLine(aLevel); // redirection to fEchos[] callbacks
end;

procedure TEchoWriter.EchoPendingExecute;
var
  todo: TEchoWriterBack; // thread-safe per reference copy
  i, e: PtrInt;
begin
  if fBack.Count = 0 then
    exit;
  fBackSafe.Lock;
  MoveFast(fBack, todo, SizeOf(fBack)); // fast copy without refcount
  FillCharFast(fBack, SizeOf(fBack), 0);
  fBackSafe.UnLock;
  for e := length(fEchos) - 1 downto 0 do // for MultiEventRemove() below
    try
      for i := 0 to todo.Count - 1 do
        fEchos[e](self, todo.Level[i], todo.Text[i]);
    except // remove callback in case of exception during echoing in user code
      MultiEventRemove(fEchos, e);
    end;
end;

procedure TEchoWriter.FlushToStream(Text: PUtf8Char; Len: PtrInt);
begin
  if fEchos = nil then
    exit;
  EchoFlush; // fill fEchoBuf with current TTextWriter buffer content
  fEchoStart := 0;
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
  L: PtrInt;
  P: PUtf8Char;
begin
  P := fWriter.fTempBuf;
  result := fWriter.B - P + 1; // returns the new fEchoStart position
  L := result - fEchoStart;
  if L = 0 then
    exit;
  inc(P, fEchoStart);
  while (L > 0) and
        (P[L - 1] in [#10, #13]) do // trim right CR/LF chars
    dec(L);
  Append(fEchoBuf, P, L); // very efficient
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


{ ************ Numbers (integers or floats) to Text Conversion }

procedure Int32ToUtf8(Value: PtrInt; var result: RawUtf8);
var
  tmp: TTemp24;
  P: PAnsiChar;
begin
  if PtrUInt(Value) <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrInt32(@tmp[23], Value);
    FastSetString(result, P, @tmp[23]);
  end;
end;

function Int32ToUtf8(Value: PtrInt): RawUtf8;
begin
  Int32ToUtf8(Value, result);
end;

procedure Int64ToUtf8(Value: Int64; var result: RawUtf8);
var
  tmp: TTemp24;
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
    FastSetString(result, P, @tmp[23]);
  end;
end;

procedure UInt64ToUtf8(Value: QWord; var result: RawUtf8);
var
  tmp: TTemp24;
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
    FastSetString(result, P, @tmp[23]);
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
  tmp: TTemp24;
  P: PAnsiChar;
begin
  if Value <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    FastSetString(result, P, @tmp[23]);
  end;
end;

procedure UInt32DigitsToUtf8(Value, Digits: PtrUInt; var result: RawUtf8);
var
  tmp: TTemp24;
  p: PUtf8Char;
  prepend: PtrInt;
begin
  Digits := MinPtrUInt(23, Digits); // support up to 23 digits
  p := @tmp[23 - Digits];
  prepend := StrUInt32(@tmp[23], Value) - p;
  if prepend > 0 then
    FillCharFast(p^, prepend, ord('0'));
  FastSetString(result, p, Digits);
end;

function UInt32ToUtf8(Value: PtrUInt): RawUtf8;
begin
  UInt32ToUtf8(Value, result);
end;

// Only the retained integer mantissa is converted: no tail rescan or big integers.
function DecimalToDoubleDirected(Mantissa: UInt64; Exponent: PtrInt; Negative: boolean): double;
const
  Scale: double = 1.3407807929942597e154; // 2^512
  InvScale: double = 7.458340731200207e-155;
  MaxScaled: double = 1.3407807929942596e154;
var
  d: double;
  q: UInt64;
begin
  if Mantissa = 0 then
  begin
    result := 0;
    if Negative then
      result := -result;
    exit;
  end;
  // Recover the short path for equivalent inputs such as 228518839.20000000.
  while (Mantissa > 9007199254740991) or (Exponent < -22) do
  begin
    q := Mantissa div 10;
    if q * 10 <> Mantissa then
      break;
    Mantissa := q;
    inc(Exponent);
  end;
  // Apply the sign before rounding the integer conversion.
  if Mantissa <= UInt64(High(Int64)) then
    if Negative then
      d := -Int64(Mantissa)
    else
      d := Int64(Mantissa)
  else
  begin
    // Both 32-bit halves and the power-of-two scaling are exact doubles.
    d := Int64(Mantissa shr 32) * 4294967296.0;
    if Negative then
      d := -d - cardinal(Mantissa)
    else
      d := d + cardinal(Mantissa);
  end;
  if Exponent = 0 then
    result := d
  else if (Exponent >= -22) and (Exponent < 0) then
    // Exact mantissa / exact power: the short-number rule from #583.
    result := d / POW10[-Exponent]
  else if (Exponent >= -31) and (Exponent <= 31) then
    result := d * POW10[Exponent]
  else if Exponent < -342 then
    result := d * 0.0
  else if Exponent < -307 then
  begin
    // Delay underflow until after the significant digits have been applied.
    inc(Exponent, 308);
    if Exponent < -31 then
      d := d * (POW10[(-Exponent and not 31) shr 5 + 45] / POW10[-Exponent and 31])
    else
      d := d * POW10[Exponent];
    result := d * 1E-308;
  end
  else if Exponent < 0 then
  begin
    Exponent := -Exponent;
    result := d * (POW10[(Exponent and not 31) shr 5 + 45] / POW10[Exponent and 31]);
  end
  else if Exponent > 308 then
  begin
    q := $7ff0000000000000;
    if Negative then
      q := q or $8000000000000000;
    result := PDouble(@q)^;
  end
  else if Exponent >= 290 then
  begin
    d := d * (POW10[(Exponent and not 31) shr 5 + 34] * POW10[Exponent and 31] * InvScale);
    if abs(d) > MaxScaled then
    begin
      q := $7ff0000000000000;
      if Negative then
        q := q or $8000000000000000;
      result := PDouble(@q)^;
    end
    else
      result := d * Scale;
  end
  else
    result := d * (POW10[(Exponent and not 31) shr 5 + 34] * POW10[Exponent and 31]);
end;

{ Fixed-width conversion of an integer mantissa and decimal exponent.
  Integer scaling follows fast_float (decimal_to_binary.h).
  No input string, discarded-tail analysis, or variable-precision arithmetic.

  https://github.com/fastfloat/fast_float
MIT License

Copyright (c) 2021 The fast_float authors

Permission is hereby granted, free of charge, to any
person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the
Software without restriction, including without
limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software
is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice
shall be included in all copies or substantial portions
of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

}

const
  DecimalPowers: array[-342..308, 0..1] of UInt64 = (
    (UInt64($EEF453D6923BD65A), UInt64($113FAA2906A13B3F)),
    (UInt64($9558B4661B6565F8), UInt64($4AC7CA59A424C507)),
    (UInt64($BAAEE17FA23EBF76), UInt64($5D79BCF00D2DF649)),
    (UInt64($E95A99DF8ACE6F53), UInt64($F4D82C2C107973DC)),
    (UInt64($91D8A02BB6C10594), UInt64($79071B9B8A4BE869)),
    (UInt64($B64EC836A47146F9), UInt64($9748E2826CDEE284)),
    (UInt64($E3E27A444D8D98B7), UInt64($FD1B1B2308169B25)),
    (UInt64($8E6D8C6AB0787F72), UInt64($FE30F0F5E50E20F7)),
    (UInt64($B208EF855C969F4F), UInt64($BDBD2D335E51A935)),
    (UInt64($DE8B2B66B3BC4723), UInt64($AD2C788035E61382)),
    (UInt64($8B16FB203055AC76), UInt64($4C3BCB5021AFCC31)),
    (UInt64($ADDCB9E83C6B1793), UInt64($DF4ABE242A1BBF3D)),
    (UInt64($D953E8624B85DD78), UInt64($D71D6DAD34A2AF0D)),
    (UInt64($87D4713D6F33AA6B), UInt64($8672648C40E5AD68)),
    (UInt64($A9C98D8CCB009506), UInt64($680EFDAF511F18C2)),
    (UInt64($D43BF0EFFDC0BA48), UInt64($212BD1B2566DEF2)),
    (UInt64($84A57695FE98746D), UInt64($14BB630F7604B57)),
    (UInt64($A5CED43B7E3E9188), UInt64($419EA3BD35385E2D)),
    (UInt64($CF42894A5DCE35EA), UInt64($52064CAC828675B9)),
    (UInt64($818995CE7AA0E1B2), UInt64($7343EFEBD1940993)),
    (UInt64($A1EBFB4219491A1F), UInt64($1014EBE6C5F90BF8)),
    (UInt64($CA66FA129F9B60A6), UInt64($D41A26E077774EF6)),
    (UInt64($FD00B897478238D0), UInt64($8920B098955522B4)),
    (UInt64($9E20735E8CB16382), UInt64($55B46E5F5D5535B0)),
    (UInt64($C5A890362FDDBC62), UInt64($EB2189F734AA831D)),
    (UInt64($F712B443BBD52B7B), UInt64($A5E9EC7501D523E4)),
    (UInt64($9A6BB0AA55653B2D), UInt64($47B233C92125366E)),
    (UInt64($C1069CD4EABE89F8), UInt64($999EC0BB696E840A)),
    (UInt64($F148440A256E2C76), UInt64($C00670EA43CA250D)),
    (UInt64($96CD2A865764DBCA), UInt64($380406926A5E5728)),
    (UInt64($BC807527ED3E12BC), UInt64($C605083704F5ECF2)),
    (UInt64($EBA09271E88D976B), UInt64($F7864A44C633682E)),
    (UInt64($93445B8731587EA3), UInt64($7AB3EE6AFBE0211D)),
    (UInt64($B8157268FDAE9E4C), UInt64($5960EA05BAD82964)),
    (UInt64($E61ACF033D1A45DF), UInt64($6FB92487298E33BD)),
    (UInt64($8FD0C16206306BAB), UInt64($A5D3B6D479F8E056)),
    (UInt64($B3C4F1BA87BC8696), UInt64($8F48A4899877186C)),
    (UInt64($E0B62E2929ABA83C), UInt64($331ACDABFE94DE87)),
    (UInt64($8C71DCD9BA0B4925), UInt64($9FF0C08B7F1D0B14)),
    (UInt64($AF8E5410288E1B6F), UInt64($7ECF0AE5EE44DD9)),
    (UInt64($DB71E91432B1A24A), UInt64($C9E82CD9F69D6150)),
    (UInt64($892731AC9FAF056E), UInt64($BE311C083A225CD2)),
    (UInt64($AB70FE17C79AC6CA), UInt64($6DBD630A48AAF406)),
    (UInt64($D64D3D9DB981787D), UInt64($92CBBCCDAD5B108)),
    (UInt64($85F0468293F0EB4E), UInt64($25BBF56008C58EA5)),
    (UInt64($A76C582338ED2621), UInt64($AF2AF2B80AF6F24E)),
    (UInt64($D1476E2C07286FAA), UInt64($1AF5AF660DB4AEE1)),
    (UInt64($82CCA4DB847945CA), UInt64($50D98D9FC890ED4D)),
    (UInt64($A37FCE126597973C), UInt64($E50FF107BAB528A0)),
    (UInt64($CC5FC196FEFD7D0C), UInt64($1E53ED49A96272C8)),
    (UInt64($FF77B1FCBEBCDC4F), UInt64($25E8E89C13BB0F7A)),
    (UInt64($9FAACF3DF73609B1), UInt64($77B191618C54E9AC)),
    (UInt64($C795830D75038C1D), UInt64($D59DF5B9EF6A2417)),
    (UInt64($F97AE3D0D2446F25), UInt64($4B0573286B44AD1D)),
    (UInt64($9BECCE62836AC577), UInt64($4EE367F9430AEC32)),
    (UInt64($C2E801FB244576D5), UInt64($229C41F793CDA73F)),
    (UInt64($F3A20279ED56D48A), UInt64($6B43527578C1110F)),
    (UInt64($9845418C345644D6), UInt64($830A13896B78AAA9)),
    (UInt64($BE5691EF416BD60C), UInt64($23CC986BC656D553)),
    (UInt64($EDEC366B11C6CB8F), UInt64($2CBFBE86B7EC8AA8)),
    (UInt64($94B3A202EB1C3F39), UInt64($7BF7D71432F3D6A9)),
    (UInt64($B9E08A83A5E34F07), UInt64($DAF5CCD93FB0CC53)),
    (UInt64($E858AD248F5C22C9), UInt64($D1B3400F8F9CFF68)),
    (UInt64($91376C36D99995BE), UInt64($23100809B9C21FA1)),
    (UInt64($B58547448FFFFB2D), UInt64($ABD40A0C2832A78A)),
    (UInt64($E2E69915B3FFF9F9), UInt64($16C90C8F323F516C)),
    (UInt64($8DD01FAD907FFC3B), UInt64($AE3DA7D97F6792E3)),
    (UInt64($B1442798F49FFB4A), UInt64($99CD11CFDF41779C)),
    (UInt64($DD95317F31C7FA1D), UInt64($40405643D711D583)),
    (UInt64($8A7D3EEF7F1CFC52), UInt64($482835EA666B2572)),
    (UInt64($AD1C8EAB5EE43B66), UInt64($DA3243650005EECF)),
    (UInt64($D863B256369D4A40), UInt64($90BED43E40076A82)),
    (UInt64($873E4F75E2224E68), UInt64($5A7744A6E804A291)),
    (UInt64($A90DE3535AAAE202), UInt64($711515D0A205CB36)),
    (UInt64($D3515C2831559A83), UInt64($D5A5B44CA873E03)),
    (UInt64($8412D9991ED58091), UInt64($E858790AFE9486C2)),
    (UInt64($A5178FFF668AE0B6), UInt64($626E974DBE39A872)),
    (UInt64($CE5D73FF402D98E3), UInt64($FB0A3D212DC8128F)),
    (UInt64($80FA687F881C7F8E), UInt64($7CE66634BC9D0B99)),
    (UInt64($A139029F6A239F72), UInt64($1C1FFFC1EBC44E80)),
    (UInt64($C987434744AC874E), UInt64($A327FFB266B56220)),
    (UInt64($FBE9141915D7A922), UInt64($4BF1FF9F0062BAA8)),
    (UInt64($9D71AC8FADA6C9B5), UInt64($6F773FC3603DB4A9)),
    (UInt64($C4CE17B399107C22), UInt64($CB550FB4384D21D3)),
    (UInt64($F6019DA07F549B2B), UInt64($7E2A53A146606A48)),
    (UInt64($99C102844F94E0FB), UInt64($2EDA7444CBFC426D)),
    (UInt64($C0314325637A1939), UInt64($FA911155FEFB5308)),
    (UInt64($F03D93EEBC589F88), UInt64($793555AB7EBA27CA)),
    (UInt64($96267C7535B763B5), UInt64($4BC1558B2F3458DE)),
    (UInt64($BBB01B9283253CA2), UInt64($9EB1AAEDFB016F16)),
    (UInt64($EA9C227723EE8BCB), UInt64($465E15A979C1CADC)),
    (UInt64($92A1958A7675175F), UInt64($BFACD89EC191EC9)),
    (UInt64($B749FAED14125D36), UInt64($CEF980EC671F667B)),
    (UInt64($E51C79A85916F484), UInt64($82B7E12780E7401A)),
    (UInt64($8F31CC0937AE58D2), UInt64($D1B2ECB8B0908810)),
    (UInt64($B2FE3F0B8599EF07), UInt64($861FA7E6DCB4AA15)),
    (UInt64($DFBDCECE67006AC9), UInt64($67A791E093E1D49A)),
    (UInt64($8BD6A141006042BD), UInt64($E0C8BB2C5C6D24E0)),
    (UInt64($AECC49914078536D), UInt64($58FAE9F773886E18)),
    (UInt64($DA7F5BF590966848), UInt64($AF39A475506A899E)),
    (UInt64($888F99797A5E012D), UInt64($6D8406C952429603)),
    (UInt64($AAB37FD7D8F58178), UInt64($C8E5087BA6D33B83)),
    (UInt64($D5605FCDCF32E1D6), UInt64($FB1E4A9A90880A64)),
    (UInt64($855C3BE0A17FCD26), UInt64($5CF2EEA09A55067F)),
    (UInt64($A6B34AD8C9DFC06F), UInt64($F42FAA48C0EA481E)),
    (UInt64($D0601D8EFC57B08B), UInt64($F13B94DAF124DA26)),
    (UInt64($823C12795DB6CE57), UInt64($76C53D08D6B70858)),
    (UInt64($A2CB1717B52481ED), UInt64($54768C4B0C64CA6E)),
    (UInt64($CB7DDCDDA26DA268), UInt64($A9942F5DCF7DFD09)),
    (UInt64($FE5D54150B090B02), UInt64($D3F93B35435D7C4C)),
    (UInt64($9EFA548D26E5A6E1), UInt64($C47BC5014A1A6DAF)),
    (UInt64($C6B8E9B0709F109A), UInt64($359AB6419CA1091B)),
    (UInt64($F867241C8CC6D4C0), UInt64($C30163D203C94B62)),
    (UInt64($9B407691D7FC44F8), UInt64($79E0DE63425DCF1D)),
    (UInt64($C21094364DFB5636), UInt64($985915FC12F542E4)),
    (UInt64($F294B943E17A2BC4), UInt64($3E6F5B7B17B2939D)),
    (UInt64($979CF3CA6CEC5B5A), UInt64($A705992CEECF9C42)),
    (UInt64($BD8430BD08277231), UInt64($50C6FF782A838353)),
    (UInt64($ECE53CEC4A314EBD), UInt64($A4F8BF5635246428)),
    (UInt64($940F4613AE5ED136), UInt64($871B7795E136BE99)),
    (UInt64($B913179899F68584), UInt64($28E2557B59846E3F)),
    (UInt64($E757DD7EC07426E5), UInt64($331AEADA2FE589CF)),
    (UInt64($9096EA6F3848984F), UInt64($3FF0D2C85DEF7621)),
    (UInt64($B4BCA50B065ABE63), UInt64($FED077A756B53A9)),
    (UInt64($E1EBCE4DC7F16DFB), UInt64($D3E8495912C62894)),
    (UInt64($8D3360F09CF6E4BD), UInt64($64712DD7ABBBD95C)),
    (UInt64($B080392CC4349DEC), UInt64($BD8D794D96AACFB3)),
    (UInt64($DCA04777F541C567), UInt64($ECF0D7A0FC5583A0)),
    (UInt64($89E42CAAF9491B60), UInt64($F41686C49DB57244)),
    (UInt64($AC5D37D5B79B6239), UInt64($311C2875C522CED5)),
    (UInt64($D77485CB25823AC7), UInt64($7D633293366B828B)),
    (UInt64($86A8D39EF77164BC), UInt64($AE5DFF9C02033197)),
    (UInt64($A8530886B54DBDEB), UInt64($D9F57F830283FDFC)),
    (UInt64($D267CAA862A12D66), UInt64($D072DF63C324FD7B)),
    (UInt64($8380DEA93DA4BC60), UInt64($4247CB9E59F71E6D)),
    (UInt64($A46116538D0DEB78), UInt64($52D9BE85F074E608)),
    (UInt64($CD795BE870516656), UInt64($67902E276C921F8B)),
    (UInt64($806BD9714632DFF6), UInt64($BA1CD8A3DB53B6)),
    (UInt64($A086CFCD97BF97F3), UInt64($80E8A40ECCD228A4)),
    (UInt64($C8A883C0FDAF7DF0), UInt64($6122CD128006B2CD)),
    (UInt64($FAD2A4B13D1B5D6C), UInt64($796B805720085F81)),
    (UInt64($9CC3A6EEC6311A63), UInt64($CBE3303674053BB0)),
    (UInt64($C3F490AA77BD60FC), UInt64($BEDBFC4411068A9C)),
    (UInt64($F4F1B4D515ACB93B), UInt64($EE92FB5515482D44)),
    (UInt64($991711052D8BF3C5), UInt64($751BDD152D4D1C4A)),
    (UInt64($BF5CD54678EEF0B6), UInt64($D262D45A78A0635D)),
    (UInt64($EF340A98172AACE4), UInt64($86FB897116C87C34)),
    (UInt64($9580869F0E7AAC0E), UInt64($D45D35E6AE3D4DA0)),
    (UInt64($BAE0A846D2195712), UInt64($8974836059CCA109)),
    (UInt64($E998D258869FACD7), UInt64($2BD1A438703FC94B)),
    (UInt64($91FF83775423CC06), UInt64($7B6306A34627DDCF)),
    (UInt64($B67F6455292CBF08), UInt64($1A3BC84C17B1D542)),
    (UInt64($E41F3D6A7377EECA), UInt64($20CABA5F1D9E4A93)),
    (UInt64($8E938662882AF53E), UInt64($547EB47B7282EE9C)),
    (UInt64($B23867FB2A35B28D), UInt64($E99E619A4F23AA43)),
    (UInt64($DEC681F9F4C31F31), UInt64($6405FA00E2EC94D4)),
    (UInt64($8B3C113C38F9F37E), UInt64($DE83BC408DD3DD04)),
    (UInt64($AE0B158B4738705E), UInt64($9624AB50B148D445)),
    (UInt64($D98DDAEE19068C76), UInt64($3BADD624DD9B0957)),
    (UInt64($87F8A8D4CFA417C9), UInt64($E54CA5D70A80E5D6)),
    (UInt64($A9F6D30A038D1DBC), UInt64($5E9FCF4CCD211F4C)),
    (UInt64($D47487CC8470652B), UInt64($7647C3200069671F)),
    (UInt64($84C8D4DFD2C63F3B), UInt64($29ECD9F40041E073)),
    (UInt64($A5FB0A17C777CF09), UInt64($F468107100525890)),
    (UInt64($CF79CC9DB955C2CC), UInt64($7182148D4066EEB4)),
    (UInt64($81AC1FE293D599BF), UInt64($C6F14CD848405530)),
    (UInt64($A21727DB38CB002F), UInt64($B8ADA00E5A506A7C)),
    (UInt64($CA9CF1D206FDC03B), UInt64($A6D90811F0E4851C)),
    (UInt64($FD442E4688BD304A), UInt64($908F4A166D1DA663)),
    (UInt64($9E4A9CEC15763E2E), UInt64($9A598E4E043287FE)),
    (UInt64($C5DD44271AD3CDBA), UInt64($40EFF1E1853F29FD)),
    (UInt64($F7549530E188C128), UInt64($D12BEE59E68EF47C)),
    (UInt64($9A94DD3E8CF578B9), UInt64($82BB74F8301958CE)),
    (UInt64($C13A148E3032D6E7), UInt64($E36A52363C1FAF01)),
    (UInt64($F18899B1BC3F8CA1), UInt64($DC44E6C3CB279AC1)),
    (UInt64($96F5600F15A7B7E5), UInt64($29AB103A5EF8C0B9)),
    (UInt64($BCB2B812DB11A5DE), UInt64($7415D448F6B6F0E7)),
    (UInt64($EBDF661791D60F56), UInt64($111B495B3464AD21)),
    (UInt64($936B9FCEBB25C995), UInt64($CAB10DD900BEEC34)),
    (UInt64($B84687C269EF3BFB), UInt64($3D5D514F40EEA742)),
    (UInt64($E65829B3046B0AFA), UInt64($CB4A5A3112A5112)),
    (UInt64($8FF71A0FE2C2E6DC), UInt64($47F0E785EABA72AB)),
    (UInt64($B3F4E093DB73A093), UInt64($59ED216765690F56)),
    (UInt64($E0F218B8D25088B8), UInt64($306869C13EC3532C)),
    (UInt64($8C974F7383725573), UInt64($1E414218C73A13FB)),
    (UInt64($AFBD2350644EEACF), UInt64($E5D1929EF90898FA)),
    (UInt64($DBAC6C247D62A583), UInt64($DF45F746B74ABF39)),
    (UInt64($894BC396CE5DA772), UInt64($6B8BBA8C328EB783)),
    (UInt64($AB9EB47C81F5114F), UInt64($66EA92F3F326564)),
    (UInt64($D686619BA27255A2), UInt64($C80A537B0EFEFEBD)),
    (UInt64($8613FD0145877585), UInt64($BD06742CE95F5F36)),
    (UInt64($A798FC4196E952E7), UInt64($2C48113823B73704)),
    (UInt64($D17F3B51FCA3A7A0), UInt64($F75A15862CA504C5)),
    (UInt64($82EF85133DE648C4), UInt64($9A984D73DBE722FB)),
    (UInt64($A3AB66580D5FDAF5), UInt64($C13E60D0D2E0EBBA)),
    (UInt64($CC963FEE10B7D1B3), UInt64($318DF905079926A8)),
    (UInt64($FFBBCFE994E5C61F), UInt64($FDF17746497F7052)),
    (UInt64($9FD561F1FD0F9BD3), UInt64($FEB6EA8BEDEFA633)),
    (UInt64($C7CABA6E7C5382C8), UInt64($FE64A52EE96B8FC0)),
    (UInt64($F9BD690A1B68637B), UInt64($3DFDCE7AA3C673B0)),
    (UInt64($9C1661A651213E2D), UInt64($6BEA10CA65C084E)),
    (UInt64($C31BFA0FE5698DB8), UInt64($486E494FCFF30A62)),
    (UInt64($F3E2F893DEC3F126), UInt64($5A89DBA3C3EFCCFA)),
    (UInt64($986DDB5C6B3A76B7), UInt64($F89629465A75E01C)),
    (UInt64($BE89523386091465), UInt64($F6BBB397F1135823)),
    (UInt64($EE2BA6C0678B597F), UInt64($746AA07DED582E2C)),
    (UInt64($94DB483840B717EF), UInt64($A8C2A44EB4571CDC)),
    (UInt64($BA121A4650E4DDEB), UInt64($92F34D62616CE413)),
    (UInt64($E896A0D7E51E1566), UInt64($77B020BAF9C81D17)),
    (UInt64($915E2486EF32CD60), UInt64($ACE1474DC1D122E)),
    (UInt64($B5B5ADA8AAFF80B8), UInt64($D819992132456BA)),
    (UInt64($E3231912D5BF60E6), UInt64($10E1FFF697ED6C69)),
    (UInt64($8DF5EFABC5979C8F), UInt64($CA8D3FFA1EF463C1)),
    (UInt64($B1736B96B6FD83B3), UInt64($BD308FF8A6B17CB2)),
    (UInt64($DDD0467C64BCE4A0), UInt64($AC7CB3F6D05DDBDE)),
    (UInt64($8AA22C0DBEF60EE4), UInt64($6BCDF07A423AA96B)),
    (UInt64($AD4AB7112EB3929D), UInt64($86C16C98D2C953C6)),
    (UInt64($D89D64D57A607744), UInt64($E871C7BF077BA8B7)),
    (UInt64($87625F056C7C4A8B), UInt64($11471CD764AD4972)),
    (UInt64($A93AF6C6C79B5D2D), UInt64($D598E40D3DD89BCF)),
    (UInt64($D389B47879823479), UInt64($4AFF1D108D4EC2C3)),
    (UInt64($843610CB4BF160CB), UInt64($CEDF722A585139BA)),
    (UInt64($A54394FE1EEDB8FE), UInt64($C2974EB4EE658828)),
    (UInt64($CE947A3DA6A9273E), UInt64($733D226229FEEA32)),
    (UInt64($811CCC668829B887), UInt64($806357D5A3F525F)),
    (UInt64($A163FF802A3426A8), UInt64($CA07C2DCB0CF26F7)),
    (UInt64($C9BCFF6034C13052), UInt64($FC89B393DD02F0B5)),
    (UInt64($FC2C3F3841F17C67), UInt64($BBAC2078D443ACE2)),
    (UInt64($9D9BA7832936EDC0), UInt64($D54B944B84AA4C0D)),
    (UInt64($C5029163F384A931), UInt64($A9E795E65D4DF11)),
    (UInt64($F64335BCF065D37D), UInt64($4D4617B5FF4A16D5)),
    (UInt64($99EA0196163FA42E), UInt64($504BCED1BF8E4E45)),
    (UInt64($C06481FB9BCF8D39), UInt64($E45EC2862F71E1D6)),
    (UInt64($F07DA27A82C37088), UInt64($5D767327BB4E5A4C)),
    (UInt64($964E858C91BA2655), UInt64($3A6A07F8D510F86F)),
    (UInt64($BBE226EFB628AFEA), UInt64($890489F70A55368B)),
    (UInt64($EADAB0ABA3B2DBE5), UInt64($2B45AC74CCEA842E)),
    (UInt64($92C8AE6B464FC96F), UInt64($3B0B8BC90012929D)),
    (UInt64($B77ADA0617E3BBCB), UInt64($9CE6EBB40173744)),
    (UInt64($E55990879DDCAABD), UInt64($CC420A6A101D0515)),
    (UInt64($8F57FA54C2A9EAB6), UInt64($9FA946824A12232D)),
    (UInt64($B32DF8E9F3546564), UInt64($47939822DC96ABF9)),
    (UInt64($DFF9772470297EBD), UInt64($59787E2B93BC56F7)),
    (UInt64($8BFBEA76C619EF36), UInt64($57EB4EDB3C55B65A)),
    (UInt64($AEFAE51477A06B03), UInt64($EDE622920B6B23F1)),
    (UInt64($DAB99E59958885C4), UInt64($E95FAB368E45ECED)),
    (UInt64($88B402F7FD75539B), UInt64($11DBCB0218EBB414)),
    (UInt64($AAE103B5FCD2A881), UInt64($D652BDC29F26A119)),
    (UInt64($D59944A37C0752A2), UInt64($4BE76D3346F0495F)),
    (UInt64($857FCAE62D8493A5), UInt64($6F70A4400C562DDB)),
    (UInt64($A6DFBD9FB8E5B88E), UInt64($CB4CCD500F6BB952)),
    (UInt64($D097AD07A71F26B2), UInt64($7E2000A41346A7A7)),
    (UInt64($825ECC24C873782F), UInt64($8ED400668C0C28C8)),
    (UInt64($A2F67F2DFA90563B), UInt64($728900802F0F32FA)),
    (UInt64($CBB41EF979346BCA), UInt64($4F2B40A03AD2FFB9)),
    (UInt64($FEA126B7D78186BC), UInt64($E2F610C84987BFA8)),
    (UInt64($9F24B832E6B0F436), UInt64($DD9CA7D2DF4D7C9)),
    (UInt64($C6EDE63FA05D3143), UInt64($91503D1C79720DBB)),
    (UInt64($F8A95FCF88747D94), UInt64($75A44C6397CE912A)),
    (UInt64($9B69DBE1B548CE7C), UInt64($C986AFBE3EE11ABA)),
    (UInt64($C24452DA229B021B), UInt64($FBE85BADCE996168)),
    (UInt64($F2D56790AB41C2A2), UInt64($FAE27299423FB9C3)),
    (UInt64($97C560BA6B0919A5), UInt64($DCCD879FC967D41A)),
    (UInt64($BDB6B8E905CB600F), UInt64($5400E987BBC1C920)),
    (UInt64($ED246723473E3813), UInt64($290123E9AAB23B68)),
    (UInt64($9436C0760C86E30B), UInt64($F9A0B6720AAF6521)),
    (UInt64($B94470938FA89BCE), UInt64($F808E40E8D5B3E69)),
    (UInt64($E7958CB87392C2C2), UInt64($B60B1D1230B20E04)),
    (UInt64($90BD77F3483BB9B9), UInt64($B1C6F22B5E6F48C2)),
    (UInt64($B4ECD5F01A4AA828), UInt64($1E38AEB6360B1AF3)),
    (UInt64($E2280B6C20DD5232), UInt64($25C6DA63C38DE1B0)),
    (UInt64($8D590723948A535F), UInt64($579C487E5A38AD0E)),
    (UInt64($B0AF48EC79ACE837), UInt64($2D835A9DF0C6D851)),
    (UInt64($DCDB1B2798182244), UInt64($F8E431456CF88E65)),
    (UInt64($8A08F0F8BF0F156B), UInt64($1B8E9ECB641B58FF)),
    (UInt64($AC8B2D36EED2DAC5), UInt64($E272467E3D222F3F)),
    (UInt64($D7ADF884AA879177), UInt64($5B0ED81DCC6ABB0F)),
    (UInt64($86CCBB52EA94BAEA), UInt64($98E947129FC2B4E9)),
    (UInt64($A87FEA27A539E9A5), UInt64($3F2398D747B36224)),
    (UInt64($D29FE4B18E88640E), UInt64($8EEC7F0D19A03AAD)),
    (UInt64($83A3EEEEF9153E89), UInt64($1953CF68300424AC)),
    (UInt64($A48CEAAAB75A8E2B), UInt64($5FA8C3423C052DD7)),
    (UInt64($CDB02555653131B6), UInt64($3792F412CB06794D)),
    (UInt64($808E17555F3EBF11), UInt64($E2BBD88BBEE40BD0)),
    (UInt64($A0B19D2AB70E6ED6), UInt64($5B6ACEAEAE9D0EC4)),
    (UInt64($C8DE047564D20A8B), UInt64($F245825A5A445275)),
    (UInt64($FB158592BE068D2E), UInt64($EED6E2F0F0D56712)),
    (UInt64($9CED737BB6C4183D), UInt64($55464DD69685606B)),
    (UInt64($C428D05AA4751E4C), UInt64($AA97E14C3C26B886)),
    (UInt64($F53304714D9265DF), UInt64($D53DD99F4B3066A8)),
    (UInt64($993FE2C6D07B7FAB), UInt64($E546A8038EFE4029)),
    (UInt64($BF8FDB78849A5F96), UInt64($DE98520472BDD033)),
    (UInt64($EF73D256A5C0F77C), UInt64($963E66858F6D4440)),
    (UInt64($95A8637627989AAD), UInt64($DDE7001379A44AA8)),
    (UInt64($BB127C53B17EC159), UInt64($5560C018580D5D52)),
    (UInt64($E9D71B689DDE71AF), UInt64($AAB8F01E6E10B4A6)),
    (UInt64($9226712162AB070D), UInt64($CAB3961304CA70E8)),
    (UInt64($B6B00D69BB55C8D1), UInt64($3D607B97C5FD0D22)),
    (UInt64($E45C10C42A2B3B05), UInt64($8CB89A7DB77C506A)),
    (UInt64($8EB98A7A9A5B04E3), UInt64($77F3608E92ADB242)),
    (UInt64($B267ED1940F1C61C), UInt64($55F038B237591ED3)),
    (UInt64($DF01E85F912E37A3), UInt64($6B6C46DEC52F6688)),
    (UInt64($8B61313BBABCE2C6), UInt64($2323AC4B3B3DA015)),
    (UInt64($AE397D8AA96C1B77), UInt64($ABEC975E0A0D081A)),
    (UInt64($D9C7DCED53C72255), UInt64($96E7BD358C904A21)),
    (UInt64($881CEA14545C7575), UInt64($7E50D64177DA2E54)),
    (UInt64($AA242499697392D2), UInt64($DDE50BD1D5D0B9E9)),
    (UInt64($D4AD2DBFC3D07787), UInt64($955E4EC64B44E864)),
    (UInt64($84EC3C97DA624AB4), UInt64($BD5AF13BEF0B113E)),
    (UInt64($A6274BBDD0FADD61), UInt64($ECB1AD8AEACDD58E)),
    (UInt64($CFB11EAD453994BA), UInt64($67DE18EDA5814AF2)),
    (UInt64($81CEB32C4B43FCF4), UInt64($80EACF948770CED7)),
    (UInt64($A2425FF75E14FC31), UInt64($A1258379A94D028D)),
    (UInt64($CAD2F7F5359A3B3E), UInt64($96EE45813A04330)),
    (UInt64($FD87B5F28300CA0D), UInt64($8BCA9D6E188853FC)),
    (UInt64($9E74D1B791E07E48), UInt64($775EA264CF55347E)),
    (UInt64($C612062576589DDA), UInt64($95364AFE032A819E)),
    (UInt64($F79687AED3EEC551), UInt64($3A83DDBD83F52205)),
    (UInt64($9ABE14CD44753B52), UInt64($C4926A9672793543)),
    (UInt64($C16D9A0095928A27), UInt64($75B7053C0F178294)),
    (UInt64($F1C90080BAF72CB1), UInt64($5324C68B12DD6339)),
    (UInt64($971DA05074DA7BEE), UInt64($D3F6FC16EBCA5E04)),
    (UInt64($BCE5086492111AEA), UInt64($88F4BB1CA6BCF585)),
    (UInt64($EC1E4A7DB69561A5), UInt64($2B31E9E3D06C32E6)),
    (UInt64($9392EE8E921D5D07), UInt64($3AFF322E62439FD0)),
    (UInt64($B877AA3236A4B449), UInt64($9BEFEB9FAD487C3)),
    (UInt64($E69594BEC44DE15B), UInt64($4C2EBE687989A9B4)),
    (UInt64($901D7CF73AB0ACD9), UInt64($F9D37014BF60A11)),
    (UInt64($B424DC35095CD80F), UInt64($538484C19EF38C95)),
    (UInt64($E12E13424BB40E13), UInt64($2865A5F206B06FBA)),
    (UInt64($8CBCCC096F5088CB), UInt64($F93F87B7442E45D4)),
    (UInt64($AFEBFF0BCB24AAFE), UInt64($F78F69A51539D749)),
    (UInt64($DBE6FECEBDEDD5BE), UInt64($B573440E5A884D1C)),
    (UInt64($89705F4136B4A597), UInt64($31680A88F8953031)),
    (UInt64($ABCC77118461CEFC), UInt64($FDC20D2B36BA7C3E)),
    (UInt64($D6BF94D5E57A42BC), UInt64($3D32907604691B4D)),
    (UInt64($8637BD05AF6C69B5), UInt64($A63F9A49C2C1B110)),
    (UInt64($A7C5AC471B478423), UInt64($FCF80DC33721D54)),
    (UInt64($D1B71758E219652B), UInt64($D3C36113404EA4A9)),
    (UInt64($83126E978D4FDF3B), UInt64($645A1CAC083126EA)),
    (UInt64($A3D70A3D70A3D70A), UInt64($3D70A3D70A3D70A4)),
    (UInt64($CCCCCCCCCCCCCCCC), UInt64($CCCCCCCCCCCCCCCD)),
    (UInt64($8000000000000000), UInt64($0)),
    (UInt64($A000000000000000), UInt64($0)),
    (UInt64($C800000000000000), UInt64($0)),
    (UInt64($FA00000000000000), UInt64($0)),
    (UInt64($9C40000000000000), UInt64($0)),
    (UInt64($C350000000000000), UInt64($0)),
    (UInt64($F424000000000000), UInt64($0)),
    (UInt64($9896800000000000), UInt64($0)),
    (UInt64($BEBC200000000000), UInt64($0)),
    (UInt64($EE6B280000000000), UInt64($0)),
    (UInt64($9502F90000000000), UInt64($0)),
    (UInt64($BA43B74000000000), UInt64($0)),
    (UInt64($E8D4A51000000000), UInt64($0)),
    (UInt64($9184E72A00000000), UInt64($0)),
    (UInt64($B5E620F480000000), UInt64($0)),
    (UInt64($E35FA931A0000000), UInt64($0)),
    (UInt64($8E1BC9BF04000000), UInt64($0)),
    (UInt64($B1A2BC2EC5000000), UInt64($0)),
    (UInt64($DE0B6B3A76400000), UInt64($0)),
    (UInt64($8AC7230489E80000), UInt64($0)),
    (UInt64($AD78EBC5AC620000), UInt64($0)),
    (UInt64($D8D726B7177A8000), UInt64($0)),
    (UInt64($878678326EAC9000), UInt64($0)),
    (UInt64($A968163F0A57B400), UInt64($0)),
    (UInt64($D3C21BCECCEDA100), UInt64($0)),
    (UInt64($84595161401484A0), UInt64($0)),
    (UInt64($A56FA5B99019A5C8), UInt64($0)),
    (UInt64($CECB8F27F4200F3A), UInt64($0)),
    (UInt64($813F3978F8940984), UInt64($4000000000000000)),
    (UInt64($A18F07D736B90BE5), UInt64($5000000000000000)),
    (UInt64($C9F2C9CD04674EDE), UInt64($A400000000000000)),
    (UInt64($FC6F7C4045812296), UInt64($4D00000000000000)),
    (UInt64($9DC5ADA82B70B59D), UInt64($F020000000000000)),
    (UInt64($C5371912364CE305), UInt64($6C28000000000000)),
    (UInt64($F684DF56C3E01BC6), UInt64($C732000000000000)),
    (UInt64($9A130B963A6C115C), UInt64($3C7F400000000000)),
    (UInt64($C097CE7BC90715B3), UInt64($4B9F100000000000)),
    (UInt64($F0BDC21ABB48DB20), UInt64($1E86D40000000000)),
    (UInt64($96769950B50D88F4), UInt64($1314448000000000)),
    (UInt64($BC143FA4E250EB31), UInt64($17D955A000000000)),
    (UInt64($EB194F8E1AE525FD), UInt64($5DCFAB0800000000)),
    (UInt64($92EFD1B8D0CF37BE), UInt64($5AA1CAE500000000)),
    (UInt64($B7ABC627050305AD), UInt64($F14A3D9E40000000)),
    (UInt64($E596B7B0C643C719), UInt64($6D9CCD05D0000000)),
    (UInt64($8F7E32CE7BEA5C6F), UInt64($E4820023A2000000)),
    (UInt64($B35DBF821AE4F38B), UInt64($DDA2802C8A800000)),
    (UInt64($E0352F62A19E306E), UInt64($D50B2037AD200000)),
    (UInt64($8C213D9DA502DE45), UInt64($4526F422CC340000)),
    (UInt64($AF298D050E4395D6), UInt64($9670B12B7F410000)),
    (UInt64($DAF3F04651D47B4C), UInt64($3C0CDD765F114000)),
    (UInt64($88D8762BF324CD0F), UInt64($A5880A69FB6AC800)),
    (UInt64($AB0E93B6EFEE0053), UInt64($8EEA0D047A457A00)),
    (UInt64($D5D238A4ABE98068), UInt64($72A4904598D6D880)),
    (UInt64($85A36366EB71F041), UInt64($47A6DA2B7F864750)),
    (UInt64($A70C3C40A64E6C51), UInt64($999090B65F67D924)),
    (UInt64($D0CF4B50CFE20765), UInt64($FFF4B4E3F741CF6D)),
    (UInt64($82818F1281ED449F), UInt64($BFF8F10E7A8921A4)),
    (UInt64($A321F2D7226895C7), UInt64($AFF72D52192B6A0D)),
    (UInt64($CBEA6F8CEB02BB39), UInt64($9BF4F8A69F764490)),
    (UInt64($FEE50B7025C36A08), UInt64($2F236D04753D5B4)),
    (UInt64($9F4F2726179A2245), UInt64($1D762422C946590)),
    (UInt64($C722F0EF9D80AAD6), UInt64($424D3AD2B7B97EF5)),
    (UInt64($F8EBAD2B84E0D58B), UInt64($D2E0898765A7DEB2)),
    (UInt64($9B934C3B330C8577), UInt64($63CC55F49F88EB2F)),
    (UInt64($C2781F49FFCFA6D5), UInt64($3CBF6B71C76B25FB)),
    (UInt64($F316271C7FC3908A), UInt64($8BEF464E3945EF7A)),
    (UInt64($97EDD871CFDA3A56), UInt64($97758BF0E3CBB5AC)),
    (UInt64($BDE94E8E43D0C8EC), UInt64($3D52EEED1CBEA317)),
    (UInt64($ED63A231D4C4FB27), UInt64($4CA7AAA863EE4BDD)),
    (UInt64($945E455F24FB1CF8), UInt64($8FE8CAA93E74EF6A)),
    (UInt64($B975D6B6EE39E436), UInt64($B3E2FD538E122B44)),
    (UInt64($E7D34C64A9C85D44), UInt64($60DBBCA87196B616)),
    (UInt64($90E40FBEEA1D3A4A), UInt64($BC8955E946FE31CD)),
    (UInt64($B51D13AEA4A488DD), UInt64($6BABAB6398BDBE41)),
    (UInt64($E264589A4DCDAB14), UInt64($C696963C7EED2DD1)),
    (UInt64($8D7EB76070A08AEC), UInt64($FC1E1DE5CF543CA2)),
    (UInt64($B0DE65388CC8ADA8), UInt64($3B25A55F43294BCB)),
    (UInt64($DD15FE86AFFAD912), UInt64($49EF0EB713F39EBE)),
    (UInt64($8A2DBF142DFCC7AB), UInt64($6E3569326C784337)),
    (UInt64($ACB92ED9397BF996), UInt64($49C2C37F07965404)),
    (UInt64($D7E77A8F87DAF7FB), UInt64($DC33745EC97BE906)),
    (UInt64($86F0AC99B4E8DAFD), UInt64($69A028BB3DED71A3)),
    (UInt64($A8ACD7C0222311BC), UInt64($C40832EA0D68CE0C)),
    (UInt64($D2D80DB02AABD62B), UInt64($F50A3FA490C30190)),
    (UInt64($83C7088E1AAB65DB), UInt64($792667C6DA79E0FA)),
    (UInt64($A4B8CAB1A1563F52), UInt64($577001B891185938)),
    (UInt64($CDE6FD5E09ABCF26), UInt64($ED4C0226B55E6F86)),
    (UInt64($80B05E5AC60B6178), UInt64($544F8158315B05B4)),
    (UInt64($A0DC75F1778E39D6), UInt64($696361AE3DB1C721)),
    (UInt64($C913936DD571C84C), UInt64($3BC3A19CD1E38E9)),
    (UInt64($FB5878494ACE3A5F), UInt64($4AB48A04065C723)),
    (UInt64($9D174B2DCEC0E47B), UInt64($62EB0D64283F9C76)),
    (UInt64($C45D1DF942711D9A), UInt64($3BA5D0BD324F8394)),
    (UInt64($F5746577930D6500), UInt64($CA8F44EC7EE36479)),
    (UInt64($9968BF6ABBE85F20), UInt64($7E998B13CF4E1ECB)),
    (UInt64($BFC2EF456AE276E8), UInt64($9E3FEDD8C321A67E)),
    (UInt64($EFB3AB16C59B14A2), UInt64($C5CFE94EF3EA101E)),
    (UInt64($95D04AEE3B80ECE5), UInt64($BBA1F1D158724A12)),
    (UInt64($BB445DA9CA61281F), UInt64($2A8A6E45AE8EDC97)),
    (UInt64($EA1575143CF97226), UInt64($F52D09D71A3293BD)),
    (UInt64($924D692CA61BE758), UInt64($593C2626705F9C56)),
    (UInt64($B6E0C377CFA2E12E), UInt64($6F8B2FB00C77836C)),
    (UInt64($E498F455C38B997A), UInt64($B6DFB9C0F956447)),
    (UInt64($8EDF98B59A373FEC), UInt64($4724BD4189BD5EAC)),
    (UInt64($B2977EE300C50FE7), UInt64($58EDEC91EC2CB657)),
    (UInt64($DF3D5E9BC0F653E1), UInt64($2F2967B66737E3ED)),
    (UInt64($8B865B215899F46C), UInt64($BD79E0D20082EE74)),
    (UInt64($AE67F1E9AEC07187), UInt64($ECD8590680A3AA11)),
    (UInt64($DA01EE641A708DE9), UInt64($E80E6F4820CC9495)),
    (UInt64($884134FE908658B2), UInt64($3109058D147FDCDD)),
    (UInt64($AA51823E34A7EEDE), UInt64($BD4B46F0599FD415)),
    (UInt64($D4E5E2CDC1D1EA96), UInt64($6C9E18AC7007C91A)),
    (UInt64($850FADC09923329E), UInt64($3E2CF6BC604DDB0)),
    (UInt64($A6539930BF6BFF45), UInt64($84DB8346B786151C)),
    (UInt64($CFE87F7CEF46FF16), UInt64($E612641865679A63)),
    (UInt64($81F14FAE158C5F6E), UInt64($4FCB7E8F3F60C07E)),
    (UInt64($A26DA3999AEF7749), UInt64($E3BE5E330F38F09D)),
    (UInt64($CB090C8001AB551C), UInt64($5CADF5BFD3072CC5)),
    (UInt64($FDCB4FA002162A63), UInt64($73D9732FC7C8F7F6)),
    (UInt64($9E9F11C4014DDA7E), UInt64($2867E7FDDCDD9AFA)),
    (UInt64($C646D63501A1511D), UInt64($B281E1FD541501B8)),
    (UInt64($F7D88BC24209A565), UInt64($1F225A7CA91A4226)),
    (UInt64($9AE757596946075F), UInt64($3375788DE9B06958)),
    (UInt64($C1A12D2FC3978937), UInt64($52D6B1641C83AE)),
    (UInt64($F209787BB47D6B84), UInt64($C0678C5DBD23A49A)),
    (UInt64($9745EB4D50CE6332), UInt64($F840B7BA963646E0)),
    (UInt64($BD176620A501FBFF), UInt64($B650E5A93BC3D898)),
    (UInt64($EC5D3FA8CE427AFF), UInt64($A3E51F138AB4CEBE)),
    (UInt64($93BA47C980E98CDF), UInt64($C66F336C36B10137)),
    (UInt64($B8A8D9BBE123F017), UInt64($B80B0047445D4184)),
    (UInt64($E6D3102AD96CEC1D), UInt64($A60DC059157491E5)),
    (UInt64($9043EA1AC7E41392), UInt64($87C89837AD68DB2F)),
    (UInt64($B454E4A179DD1877), UInt64($29BABE4598C311FB)),
    (UInt64($E16A1DC9D8545E94), UInt64($F4296DD6FEF3D67A)),
    (UInt64($8CE2529E2734BB1D), UInt64($1899E4A65F58660C)),
    (UInt64($B01AE745B101E9E4), UInt64($5EC05DCFF72E7F8F)),
    (UInt64($DC21A1171D42645D), UInt64($76707543F4FA1F73)),
    (UInt64($899504AE72497EBA), UInt64($6A06494A791C53A8)),
    (UInt64($ABFA45DA0EDBDE69), UInt64($487DB9D17636892)),
    (UInt64($D6F8D7509292D603), UInt64($45A9D2845D3C42B6)),
    (UInt64($865B86925B9BC5C2), UInt64($B8A2392BA45A9B2)),
    (UInt64($A7F26836F282B732), UInt64($8E6CAC7768D7141E)),
    (UInt64($D1EF0244AF2364FF), UInt64($3207D795430CD926)),
    (UInt64($8335616AED761F1F), UInt64($7F44E6BD49E807B8)),
    (UInt64($A402B9C5A8D3A6E7), UInt64($5F16206C9C6209A6)),
    (UInt64($CD036837130890A1), UInt64($36DBA887C37A8C0F)),
    (UInt64($802221226BE55A64), UInt64($C2494954DA2C9789)),
    (UInt64($A02AA96B06DEB0FD), UInt64($F2DB9BAA10B7BD6C)),
    (UInt64($C83553C5C8965D3D), UInt64($6F92829494E5ACC7)),
    (UInt64($FA42A8B73ABBF48C), UInt64($CB772339BA1F17F9)),
    (UInt64($9C69A97284B578D7), UInt64($FF2A760414536EFB)),
    (UInt64($C38413CF25E2D70D), UInt64($FEF5138519684ABA)),
    (UInt64($F46518C2EF5B8CD1), UInt64($7EB258665FC25D69)),
    (UInt64($98BF2F79D5993802), UInt64($EF2F773FFBD97A61)),
    (UInt64($BEEEFB584AFF8603), UInt64($AAFB550FFACFD8FA)),
    (UInt64($EEAABA2E5DBF6784), UInt64($95BA2A53F983CF38)),
    (UInt64($952AB45CFA97A0B2), UInt64($DD945A747BF26183)),
    (UInt64($BA756174393D88DF), UInt64($94F971119AEEF9E4)),
    (UInt64($E912B9D1478CEB17), UInt64($7A37CD5601AAB85D)),
    (UInt64($91ABB422CCB812EE), UInt64($AC62E055C10AB33A)),
    (UInt64($B616A12B7FE617AA), UInt64($577B986B314D6009)),
    (UInt64($E39C49765FDF9D94), UInt64($ED5A7E85FDA0B80B)),
    (UInt64($8E41ADE9FBEBC27D), UInt64($14588F13BE847307)),
    (UInt64($B1D219647AE6B31C), UInt64($596EB2D8AE258FC8)),
    (UInt64($DE469FBD99A05FE3), UInt64($6FCA5F8ED9AEF3BB)),
    (UInt64($8AEC23D680043BEE), UInt64($25DE7BB9480D5854)),
    (UInt64($ADA72CCC20054AE9), UInt64($AF561AA79A10AE6A)),
    (UInt64($D910F7FF28069DA4), UInt64($1B2BA1518094DA04)),
    (UInt64($87AA9AFF79042286), UInt64($90FB44D2F05D0842)),
    (UInt64($A99541BF57452B28), UInt64($353A1607AC744A53)),
    (UInt64($D3FA922F2D1675F2), UInt64($42889B8997915CE8)),
    (UInt64($847C9B5D7C2E09B7), UInt64($69956135FEBADA11)),
    (UInt64($A59BC234DB398C25), UInt64($43FAB9837E699095)),
    (UInt64($CF02B2C21207EF2E), UInt64($94F967E45E03F4BB)),
    (UInt64($8161AFB94B44F57D), UInt64($1D1BE0EEBAC278F5)),
    (UInt64($A1BA1BA79E1632DC), UInt64($6462D92A69731732)),
    (UInt64($CA28A291859BBF93), UInt64($7D7B8F7503CFDCFE)),
    (UInt64($FCB2CB35E702AF78), UInt64($5CDA735244C3D43E)),
    (UInt64($9DEFBF01B061ADAB), UInt64($3A0888136AFA64A7)),
    (UInt64($C56BAEC21C7A1916), UInt64($88AAA1845B8FDD0)),
    (UInt64($F6C69A72A3989F5B), UInt64($8AAD549E57273D45)),
    (UInt64($9A3C2087A63F6399), UInt64($36AC54E2F678864B)),
    (UInt64($C0CB28A98FCF3C7F), UInt64($84576A1BB416A7DD)),
    (UInt64($F0FDF2D3F3C30B9F), UInt64($656D44A2A11C51D5)),
    (UInt64($969EB7C47859E743), UInt64($9F644AE5A4B1B325)),
    (UInt64($BC4665B596706114), UInt64($873D5D9F0DDE1FEE)),
    (UInt64($EB57FF22FC0C7959), UInt64($A90CB506D155A7EA)),
    (UInt64($9316FF75DD87CBD8), UInt64($9A7F12442D588F2)),
    (UInt64($B7DCBF5354E9BECE), UInt64($C11ED6D538AEB2F)),
    (UInt64($E5D3EF282A242E81), UInt64($8F1668C8A86DA5FA)),
    (UInt64($8FA475791A569D10), UInt64($F96E017D694487BC)),
    (UInt64($B38D92D760EC4455), UInt64($37C981DCC395A9AC)),
    (UInt64($E070F78D3927556A), UInt64($85BBE253F47B1417)),
    (UInt64($8C469AB843B89562), UInt64($93956D7478CCEC8E)),
    (UInt64($AF58416654A6BABB), UInt64($387AC8D1970027B2)),
    (UInt64($DB2E51BFE9D0696A), UInt64($6997B05FCC0319E)),
    (UInt64($88FCF317F22241E2), UInt64($441FECE3BDF81F03)),
    (UInt64($AB3C2FDDEEAAD25A), UInt64($D527E81CAD7626C3)),
    (UInt64($D60B3BD56A5586F1), UInt64($8A71E223D8D3B074)),
    (UInt64($85C7056562757456), UInt64($F6872D5667844E49)),
    (UInt64($A738C6BEBB12D16C), UInt64($B428F8AC016561DB)),
    (UInt64($D106F86E69D785C7), UInt64($E13336D701BEBA52)),
    (UInt64($82A45B450226B39C), UInt64($ECC0024661173473)),
    (UInt64($A34D721642B06084), UInt64($27F002D7F95D0190)),
    (UInt64($CC20CE9BD35C78A5), UInt64($31EC038DF7B441F4)),
    (UInt64($FF290242C83396CE), UInt64($7E67047175A15271)),
    (UInt64($9F79A169BD203E41), UInt64($F0062C6E984D386)),
    (UInt64($C75809C42C684DD1), UInt64($52C07B78A3E60868)),
    (UInt64($F92E0C3537826145), UInt64($A7709A56CCDF8A82)),
    (UInt64($9BBCC7A142B17CCB), UInt64($88A66076400BB691)),
    (UInt64($C2ABF989935DDBFE), UInt64($6ACFF893D00EA435)),
    (UInt64($F356F7EBF83552FE), UInt64($583F6B8C4124D43)),
    (UInt64($98165AF37B2153DE), UInt64($C3727A337A8B704A)),
    (UInt64($BE1BF1B059E9A8D6), UInt64($744F18C0592E4C5C)),
    (UInt64($EDA2EE1C7064130C), UInt64($1162DEF06F79DF73)),
    (UInt64($9485D4D1C63E8BE7), UInt64($8ADDCB5645AC2BA8)),
    (UInt64($B9A74A0637CE2EE1), UInt64($6D953E2BD7173692)),
    (UInt64($E8111C87C5C1BA99), UInt64($C8FA8DB6CCDD0437)),
    (UInt64($910AB1D4DB9914A0), UInt64($1D9C9892400A22A2)),
    (UInt64($B54D5E4A127F59C8), UInt64($2503BEB6D00CAB4B)),
    (UInt64($E2A0B5DC971F303A), UInt64($2E44AE64840FD61D)),
    (UInt64($8DA471A9DE737E24), UInt64($5CEAECFED289E5D2)),
    (UInt64($B10D8E1456105DAD), UInt64($7425A83E872C5F47)),
    (UInt64($DD50F1996B947518), UInt64($D12F124E28F77719)),
    (UInt64($8A5296FFE33CC92F), UInt64($82BD6B70D99AAA6F)),
    (UInt64($ACE73CBFDC0BFB7B), UInt64($636CC64D1001550B)),
    (UInt64($D8210BEFD30EFA5A), UInt64($3C47F7E05401AA4E)),
    (UInt64($8714A775E3E95C78), UInt64($65ACFAEC34810A71)),
    (UInt64($A8D9D1535CE3B396), UInt64($7F1839A741A14D0D)),
    (UInt64($D31045A8341CA07C), UInt64($1EDE48111209A050)),
    (UInt64($83EA2B892091E44D), UInt64($934AED0AAB460432)),
    (UInt64($A4E4B66B68B65D60), UInt64($F81DA84D5617853F)),
    (UInt64($CE1DE40642E3F4B9), UInt64($36251260AB9D668E)),
    (UInt64($80D2AE83E9CE78F3), UInt64($C1D72B7C6B426019)),
    (UInt64($A1075A24E4421730), UInt64($B24CF65B8612F81F)),
    (UInt64($C94930AE1D529CFC), UInt64($DEE033F26797B627)),
    (UInt64($FB9B7CD9A4A7443C), UInt64($169840EF017DA3B1)),
    (UInt64($9D412E0806E88AA5), UInt64($8E1F289560EE864E)),
    (UInt64($C491798A08A2AD4E), UInt64($F1A6F2BAB92A27E2)),
    (UInt64($F5B5D7EC8ACB58A2), UInt64($AE10AF696774B1DB)),
    (UInt64($9991A6F3D6BF1765), UInt64($ACCA6DA1E0A8EF29)),
    (UInt64($BFF610B0CC6EDD3F), UInt64($17FD090A58D32AF3)),
    (UInt64($EFF394DCFF8A948E), UInt64($DDFC4B4CEF07F5B0)),
    (UInt64($95F83D0A1FB69CD9), UInt64($4ABDAF101564F98E)),
    (UInt64($BB764C4CA7A4440F), UInt64($9D6D1AD41ABE37F1)),
    (UInt64($EA53DF5FD18D5513), UInt64($84C86189216DC5ED)),
    (UInt64($92746B9BE2F8552C), UInt64($32FD3CF5B4E49BB4)),
    (UInt64($B7118682DBB66A77), UInt64($3FBC8C33221DC2A1)),
    (UInt64($E4D5E82392A40515), UInt64($FABAF3FEAA5334A)),
    (UInt64($8F05B1163BA6832D), UInt64($29CB4D87F2A7400E)),
    (UInt64($B2C71D5BCA9023F8), UInt64($743E20E9EF511012)),
    (UInt64($DF78E4B2BD342CF6), UInt64($914DA9246B255416)),
    (UInt64($8BAB8EEFB6409C1A), UInt64($1AD089B6C2F7548E)),
    (UInt64($AE9672ABA3D0C320), UInt64($A184AC2473B529B1)),
    (UInt64($DA3C0F568CC4F3E8), UInt64($C9E5D72D90A2741E)),
    (UInt64($8865899617FB1871), UInt64($7E2FA67C7A658892)),
    (UInt64($AA7EEBFB9DF9DE8D), UInt64($DDBB901B98FEEAB7)),
    (UInt64($D51EA6FA85785631), UInt64($552A74227F3EA565)),
    (UInt64($8533285C936B35DE), UInt64($D53A88958F87275F)),
    (UInt64($A67FF273B8460356), UInt64($8A892ABAF368F137)),
    (UInt64($D01FEF10A657842C), UInt64($2D2B7569B0432D85)),
    (UInt64($8213F56A67F6B29B), UInt64($9C3B29620E29FC73)),
    (UInt64($A298F2C501F45F42), UInt64($8349F3BA91B47B8F)),
    (UInt64($CB3F2F7642717713), UInt64($241C70A936219A73)),
    (UInt64($FE0EFB53D30DD4D7), UInt64($ED238CD383AA0110)),
    (UInt64($9EC95D1463E8A506), UInt64($F4363804324A40AA)),
    (UInt64($C67BB4597CE2CE48), UInt64($B143C6053EDCD0D5)),
    (UInt64($F81AA16FDC1B81DA), UInt64($DD94B7868E94050A)),
    (UInt64($9B10A4E5E9913128), UInt64($CA7CF2B4191C8326)),
    (UInt64($C1D4CE1F63F57D72), UInt64($FD1C2F611F63A3F0)),
    (UInt64($F24A01A73CF2DCCF), UInt64($BC633B39673C8CEC)),
    (UInt64($976E41088617CA01), UInt64($D5BE0503E085D813)),
    (UInt64($BD49D14AA79DBC82), UInt64($4B2D8644D8A74E18)),
    (UInt64($EC9C459D51852BA2), UInt64($DDF8E7D60ED1219E)),
    (UInt64($93E1AB8252F33B45), UInt64($CABB90E5C942B503)),
    (UInt64($B8DA1662E7B00A17), UInt64($3D6A751F3B936243)),
    (UInt64($E7109BFBA19C0C9D), UInt64($CC512670A783AD4)),
    (UInt64($906A617D450187E2), UInt64($27FB2B80668B24C5)),
    (UInt64($B484F9DC9641E9DA), UInt64($B1F9F660802DEDF6)),
    (UInt64($E1A63853BBD26451), UInt64($5E7873F8A0396973)),
    (UInt64($8D07E33455637EB2), UInt64($DB0B487B6423E1E8)),
    (UInt64($B049DC016ABC5E5F), UInt64($91CE1A9A3D2CDA62)),
    (UInt64($DC5C5301C56B75F7), UInt64($7641A140CC7810FB)),
    (UInt64($89B9B3E11B6329BA), UInt64($A9E904C87FCB0A9D)),
    (UInt64($AC2820D9623BF429), UInt64($546345FA9FBDCD44)),
    (UInt64($D732290FBACAF133), UInt64($A97C177947AD4095)),
    (UInt64($867F59A9D4BED6C0), UInt64($49ED8EABCCCC485D)),
    (UInt64($A81F301449EE8C70), UInt64($5C68F256BFFF5A74)),
    (UInt64($D226FC195C6A2F8C), UInt64($73832EEC6FFF3111)),
    (UInt64($83585D8FD9C25DB7), UInt64($C831FD53C5FF7EAB)),
    (UInt64($A42E74F3D032F525), UInt64($BA3E7CA8B77F5E55)),
    (UInt64($CD3A1230C43FB26F), UInt64($28CE1BD2E55F35EB)),
    (UInt64($80444B5E7AA7CF85), UInt64($7980D163CF5B81B3)),
    (UInt64($A0555E361951C366), UInt64($D7E105BCC332621F)),
    (UInt64($C86AB5C39FA63440), UInt64($8DD9472BF3FEFAA7)),
    (UInt64($FA856334878FC150), UInt64($B14F98F6F0FEB951)),
    (UInt64($9C935E00D4B9D8D2), UInt64($6ED1BF9A569F33D3)),
    (UInt64($C3B8358109E84F07), UInt64($A862F80EC4700C8)),
    (UInt64($F4A642E14C6262C8), UInt64($CD27BB612758C0FA)),
    (UInt64($98E7E9CCCFBD7DBD), UInt64($8038D51CB897789C)),
    (UInt64($BF21E44003ACDD2C), UInt64($E0470A63E6BD56C3)),
    (UInt64($EEEA5D5004981478), UInt64($1858CCFCE06CAC74)),
    (UInt64($95527A5202DF0CCB), UInt64($F37801E0C43EBC8)),
    (UInt64($BAA718E68396CFFD), UInt64($D30560258F54E6BA)),
    (UInt64($E950DF20247C83FD), UInt64($47C6B82EF32A2069)),
    (UInt64($91D28B7416CDD27E), UInt64($4CDC331D57FA5441)),
    (UInt64($B6472E511C81471D), UInt64($E0133FE4ADF8E952)),
    (UInt64($E3D8F9E563A198E5), UInt64($58180FDDD97723A6)),
    (UInt64($8E679C2F5E44FF8F), UInt64($570F09EAA7EA7648))
  );


function DecimalMul128(A, B: UInt64; out Hi: UInt64): UInt64;
{$ifdef ASMX64}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef ABISYSVX64}
        mov     r8, rdx
        mov     rcx, rdi
        mov     rdx, rsi
        {$endif}
        mov     rax, rcx
        mul     rdx
        mov     [r8], rdx
end;
{$else ASMX64}
var
  T, LowProduct, Cross: UInt64;
begin
  LowProduct := UInt64(cardinal(A)) * cardinal(B);
  T := (A shr 32) * cardinal(B) + (LowProduct shr 32);
  Cross := UInt64(cardinal(T)) + UInt64(cardinal(A)) * (B shr 32);
  Hi := (A shr 32) * (B shr 32) + (T shr 32) + (Cross shr 32);
  result := (Cross shl 32) or cardinal(LowProduct);
end;
{$endif ASMX64}


function DecimalLeadingZeros(A: UInt64): Integer;
{$ifdef ASMX64}
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef ABISYSVX64}
        bsr     rax, rdi
        {$else}
        bsr     rax, rcx
        {$endif}
        xor     eax, 63
end;
{$else ASMX64}
begin
  result := 0;
  while A and $8000000000000000 = 0 do
  begin
    inc(result);
    A := A shl 1;
  end;
end;
{$endif ASMX64}


function DecimalToDoubleBitsPortable(Mantissa: UInt64; Exponent: PtrInt): UInt64;
var
  Hi, Lo, Extra, W, M: UInt64;
  Leading, Upper, Shift, Power: Integer;
begin
  if (Mantissa = 0) or (Exponent < -342) then
  begin
    result := 0;
    exit;
  end;
  if Exponent > 308 then
  begin
    result := $7ff0000000000000;
    exit;
  end;
  Leading := DecimalLeadingZeros(Mantissa);
  W := Mantissa shl Leading;
  Lo := DecimalMul128(W, DecimalPowers[Exponent, 0], Hi);
  if Hi and $1ff = $1ff then
  begin
    DecimalMul128(W, DecimalPowers[Exponent, 1], Extra);
    inc(Lo, Extra);
    inc(Hi, ord(Extra > Lo));
  end;
  Upper := Hi shr 63;
  Shift := Upper + 9;
  M := Hi shr Shift;
  // Delphi's shr is logical even for signed operands: use div with floor.
  Power := 217706 * Exponent;
  if Power < 0 then
    dec(Power, 65535);
  Power := Power div 65536 + 63 + Upper - Leading + 1023;
  if Power <= 0 then
  begin
    if Power <= -63 then
      result := 0
    else
    begin
      M := M shr (1 - Power);
      result := (M + (M and 1)) shr 1;
    end;
    exit;
  end;
  if (Lo <= 1) and (Exponent >= -4) and (Exponent <= 23) and
     (M and 3 = 1) and (M shl Shift = Hi) then
    M := M and not UInt64(1);
  M := (M + (M and 1)) shr 1;
  if M >= UInt64(1) shl 53 then
  begin
    M := UInt64(1) shl 52;
    inc(Power);
  end;
  if Power >= 2047 then
    result := $7ff0000000000000
  else
    result := (UInt64(Power) shl 52) or (M and $000fffffffffffff);
end;

{$ifdef ASMX64}
function DecimalToDouble(Mantissa: UInt64; Exponent: PtrInt; Negative: boolean): double;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif}
        {$ifdef ABISYSVX64}
        movzx   r8d, dl
        mov     rcx, rdi
        mov     rdx, rsi
        stmxcsr dword ptr [rsp - 4]
        test    dword ptr [rsp - 4], $6000
        {$else}
        stmxcsr dword ptr [rsp + 8]
        test    dword ptr [rsp + 8], $6000
        {$endif}
        jnz     @directed
        movzx   eax, r8b
        shl     rax, 63
        movq    xmm5, rax
        test    rcx, rcx
        jz      @zero
        lea     rax, [rdx + 342]
        cmp     rax, 650
        ja      @range
        lea     r8, [rip + DecimalPowers]
        shl     eax, 4
        add     r8, rax
        mov     r10d, edx
        mov     rax, rcx
        bsr     rcx, rcx
        xor     ecx, 63
        shl     rax, cl
        mov     r9, rax
        imul    edx, edx, 217706
        sar     edx, 16
        add     edx, 1086
        sub     edx, ecx
        mov     r11d, edx
        mul     qword ptr [r8]
        mov     ecx, edx
        and     ecx, $1ff
        cmp     ecx, $1ff
        je      @wider
@shift:
        mov     r9, rdx
        mov     rcx, rdx
        shr     rcx, 63
        add     r11d, ecx
        add     ecx, 9
        shr     rdx, cl
        test    r11d, r11d
        jle     @subnormal
        cmp     rax, 1
        ja      @round
        add     r10d, 4
        cmp     r10d, 27
        ja      @round
        mov     eax, edx
        and     eax, 3
        cmp     eax, 1
        jne     @round
        mov     rax, rdx
        shl     rax, cl
        cmp     rax, r9
        jne     @round
        and     rdx, -2
@round:
        mov     eax, edx
        and     eax, 1
        add     rax, rdx
        shr     rax, 1
        bt      rax, 53
        jc      @carry
@pack:
        cmp     r11d, 2047
        jge     @infinity
        btr     rax, 52
        shl     r11, 52
        or      rax, r11
        jmp     @packedResult
@carry:
        shr     rax, 1
        inc     r11d
        jmp     @pack
@wider:
        movq    xmm0, rax
        movq    xmm1, rdx
        mov     rax, r9
        mul     qword ptr [r8 + 8]
        movq    rax, xmm0
        add     rax, rdx
        movq    rdx, xmm1
        adc     rdx, 0
        jmp     @shift
@subnormal:
        cmp     r11d, -63
        jle     @zero
        mov     ecx, 1
        sub     ecx, r11d
        shr     rdx, cl
        mov     eax, edx
        and     eax, 1
        add     rax, rdx
        shr     rax, 1
        jmp     @packedResult
@range:
        test    rdx, rdx
        jns     @infinity
@zero:
        xor     eax, eax
        jmp     @packedResult
@infinity:
        mov     rax, $7ff0000000000000
@packedResult:
        movq    xmm0, rax
        xorpd   xmm0, xmm5
        ret
@directed:
        {$ifdef ABISYSVX64}
        mov     edx, r8d
        {$endif}
        jmp     DecimalToDoubleDirected
end;

{$else ASMX64}
function DecimalToDouble(Mantissa: UInt64; Exponent: PtrInt; Negative: boolean): double;
var
  bits: UInt64;
begin
  if GetRoundMode <> rmNearest then
    result := DecimalToDoubleDirected(Mantissa, Exponent, Negative)
  else
  begin
    bits := DecimalToDoubleBitsPortable(Mantissa, Exponent);
    if Negative then
      bits := bits or $8000000000000000;
    result := PDouble(@bits)^;
  end;
end;
{$endif ASMX64}

function GetExtended(P: PUtf8Char): TSynExtended;
var
  err: integer;
begin
  result := GetExtended(P, err);
  if err <> 0 then
    result := 0;
end;

{$ifndef WIN32DELPHI} // Delphi has its own x86/x87 asm version

{$ifdef ASMX64}
function GetExtendedPascal(P: PUtf8Char; out err: integer): TSynExtended;
{$else}
function GetExtended(P: PUtf8Char; out err: integer): TSynExtended;
{$ifend}
const
  Scale: double = 1.3407807929942597e154; // 2^512
  InvScale: double = 7.458340731200207e-155; // 2^-512
  MaxScaled: double = 1.3407807929942596e154; // MaxDouble * 2^-512
var
  remdigit, frac, exp: PtrInt;
  bits: UInt64;
  flags: set of (fNeg, fNegExp, fValid, fDot);
  v64: Int64; // allows 64-bit resolution for the digits (match 80-bit extended)
  d64: TSynExtended;
label
  z, e, x, o;
begin
  byte(flags) := 0;
  v64 := 0;
  frac := 0;
  if P = nil then
    goto z; // will return 0 but err=1
  err := frac; // =0 for success
  if P^ = ' ' then
    repeat
      inc(P);
    until P^ <> ' '; // trailing spaces
  if P^ = '+' then
    inc(P)
  else if P^ = '-' then
  begin
    inc(P);
    include(flags, fNeg);
  end;
  if P^ > '9' then
  begin
    if (P[1] = #0) or
       (P[2] = #0) then
      goto z;
    case PCardinal(P)^ and $00dfdfdf of
      ord('N') + ord('A') shl 8 + ord('N') shl 16:
        result := NaN;
      ord('I') + ord('N') shl 8 + ord('F') shl 16:
        if fNeg in flags then
          result := NegInfinity
        else
          result := Infinity;
    else
      begin
z:      err := 1; // fast error path for non-number input
        result := 0;
      end;
    end;
    exit;
  end;
  remdigit := 18; // v64=-9,223,372,036,854,775,808..+9,223,372,036,854,775,807
  repeat
    exp := ord(P^) - ord('0');
    if PtrUInt(exp) <= 9 then
    begin
      if (remdigit <> 0) or  // validate the 19th significant digit
         (v64 > MAX_INT64_DIV10 - ord(exp > 7)) then
        dec(remdigit);
      if remdigit >= 0 then // over-required digits are just ignored
      begin
        v64 := v64 {$ifdef HASSLOWMUL64} shl 3 + v64 + v64 {$else} * 10 {$endif} + exp;
        include(flags, fValid);
        dec(frac, ord(frac <> 0)); // digits after '.' (branchless)
        inc(P);
        continue;
      end;
      inc(frac, ord(frac >= 0)); // handle #############00000
      inc(P);
      continue;
    end;
    if P^ <> '.' then
      break;
    inc(P);
    if fDot in flags then
      goto e; // only one dot allowed
    include(flags, fDot);
    if frac > 0 then
    begin // the integer mantissa is full: fractional digits belong to its tail
      while P^ in ['0'..'9'] do
        inc(P);
      continue;
    end;
    dec(frac);
    if v64 = 0 then // properly handle 0.00000000000000000123
      while P^ = '0' do
      begin
        dec(frac);
        inc(P);
      end;
  until false;
  inc(frac, ord(frac < 0)); // adjust digits after '.'
  if P^ in ['E', 'e'] then
  begin
    if not (fValid in flags) then
      goto e;
    exp := 0;
    exclude(flags, fValid);
    inc(P);
    if P^ = '+' then
      inc(P)
    else if P^ = '-' then
    begin
      inc(P);
      include(flags, fNegExp);
    end;
    repeat
      remdigit := PtrInt(P^) - ord('0');
      if PtrUInt(remdigit) > 9 then
        break;
      exp := (exp * 10) + remdigit;
      include(flags, fValid);
      inc(P);
      if exp >= $fff000 then // huge constant, but still aarch64 friendly
        goto e;
    until false;
    if fNegExp in flags then
      dec(frac, exp)
    else
      inc(frac, exp);
  end;
  if (P^ <> #0) or
     not (fValid in flags) then
e:  err := 1; // return the (partial) value even if not ended with #0
  {$ifndef TSYNEXTENDED80}
  if (err = 0) and ((v64 = 0) or ((frac >= -342) and (frac <= 308))) then
  begin
    result := DecimalToDouble(v64, frac, fNeg in flags);
    bits := PUInt64(@result)^;
    if (bits and $7fffffffffffffff <> $7ff0000000000000) then
      exit;
    err := 1;
  end;
  {$endif TSYNEXTENDED80}
  if v64 = 0 then
  begin
    result := 0; // fast path for e.g. '0' or '0E400'
    goto x;
  end;
  d64 := v64;
  if (frac < 0) and
     (frac >= -22) and
     (v64 <= MAX_SAFE_JS_INTEGER) then
  begin
    // Clinger's fast path: d64 and 10^-frac are both exact doubles, so a single
    // IEEE division is correctly rounded - whereas POW10[frac] * d64 is not,
    // since 1E-1..1E-22 are inexact (e.g. '1.2' returned 1.2000000000000002)
    result := d64 / POW10[-frac];
    goto x;
  end;
  if PtrUInt(frac) + 31 <= 62 then // -31 .. +31: overwhelmingly common
    result := POW10[frac]
  else if frac < -31 then
  begin
    if frac <= -324 then
    begin
      if frac < -342 then
        goto o;
      // avoid creating a subnormal 10^frac before applying d64
      frac := -(frac + 160);
      result := d64 * POW10[50] * // 1E-160
                (POW10[(frac and not 31) shr 5 + 45] / POW10[frac and 31]);
      goto x;
    end;
    frac := -frac;
    result := POW10[(frac and not 31) shr 5 + 45] / POW10[frac and 31];
  end
  else
  begin // frac >= 32
    if frac > 308 then
    begin
o:    result := d64;
      err := 1;
      goto x;
    end;
    if frac >= 290 then // avoid overflow, even with unmasked FPU
    begin
      result := (POW10[(frac and not 31) shr 5 + 34] *
                 POW10[frac and 31] * InvScale) * d64;
      if result > MaxScaled then
        goto o;
      result := result * Scale;
      goto x;
    end;
    result := POW10[(frac and not 31) shr 5 + 34] * POW10[frac and 31];
  end;
  result := result * d64;
x:if fNeg in flags then
    result := -result;
end;

{$ifdef ASMX64}
// A tail adapter keeps the scanner a leaf with the normal platform unwind ABI.
function FinishExtendedNumber(Mantissa: UInt64; Exponent: PtrInt;
  Negative: cardinal; out err: integer): TSynExtended;
var
  bits: UInt64;
begin
  err := 0;
  result := DecimalToDouble(Mantissa, Exponent, Negative and 1 <> 0);
  bits := PUInt64(@result)^;
  if (Mantissa <> 0) and ((Exponent < -342) or (Exponent > 308) or
     (bits and $7fffffffffffffff = $7ff0000000000000)) then
  begin
    err := 1;
    result := Mantissa;
    if Negative and 1 <> 0 then
      result := -result;
  end;
end;

// Scanners retain a bounded integer mantissa and skip the remaining digits.
const
  NumberSignMask: UInt64 = $8000000000000000;
  NumberNaN: double = NaN;
  NumberInfinity: double = Infinity;

function GetExtended(P: PUtf8Char; out err: integer): TSynExtended;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifdef ABISYSVX64}
        mov     rcx, rdi
        mov     rdx, rsi
        {$endif ABISYSVX64}
        movq    xmm2, rcx // original input for the general path
        test    rcx, rcx
        jz      @slow
        xor     r11d, r11d
        cmp     byte ptr [rcx], '-'
        jne     @positive
        inc     rcx
        inc     r11d
@positive:
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @special
        lea     r8, [rcx + 18] // up to 18 digits need no overflow check
        mov     r9d, eax
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @integerEnd
@integer:
        cmp     rcx, r8
        jae     @moreInteger
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @integerEnd
@integerOne:
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @integer
@integerEnd:
        cmp     eax, -48
        je      @integerValue
        cmp     eax, -2 // '.' minus '0'
        jne     @integerExponent
@fractionStart:
        inc     rcx
        mov     r10, rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @finish
        sub     r8, 2
@fractionFour:
        cmp     rcx, r8
        jae     @fractionTwo
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @finish
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @finish
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @finish
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @fractionFour
        jmp     @finish
@fractionTwo:
        add     r8, 2
@fraction:
        cmp     rcx, r8 // last permitted byte; two digits fit while rcx < r8
        jae     @lastFraction
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        ja      @finish
@fractionOne:
        lea     r9, [r9 + r9 * 4]
        lea     r9, [rax + r9 * 2]
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @fraction
@finish:
        sub     r10, rcx
        cmp     eax, -48
        jne     @exponent
@value:
        mov     rax, r9
        shr     rax, 53
        jnz     @largeMantissa
        pxor    xmm0, xmm0
        cvtsi2sd xmm0, r9
        test    r11b, 1
        jz      @scaleExact
        movq    xmm3, qword ptr [rip + NumberSignMask]
        xorpd   xmm0, xmm3
@scaleExact:
        lea     r8, [rip + POW10]
        lea     rax, [r10 + 22]
        cmp     rax, 21
        ja      @nonFraction
        neg     r10
        divsd   xmm0, qword ptr [r8 + r10 * 8 + 31 * 8]
@store:
        mov     dword ptr [rdx], 0
        ret
@applySign:
        test    r11b, 1
        jz      @store
        movq    xmm3, qword ptr [rip + NumberSignMask]
        xorpd   xmm0, xmm3
        jmp     @store
@integerValue:
        test    r9, r9
        js      @unsignedInteger
        test    r11b, 1
        jnz     @negativeInteger
@positiveInteger:
        pxor    xmm0, xmm0
        cvtsi2sd xmm0, r9
        mov     dword ptr [rdx], 0
        ret
@negativeInteger:
        neg     r9
        jnz     @positiveInteger
        movq    xmm0, qword ptr [rip + NumberSignMask]
        mov     dword ptr [rdx], 0
        ret
@unsignedInteger:
        xor     r10d, r10d
        jmp     @convertFull
@integerExponent:
        xor     r10d, r10d
        jmp     @exponent
@lastFraction:
        ja      @moreFraction
        jmp     @fractionOne
@moreInteger:
        movq    xmm4, rax
        mov     rax, 1000000000000000000
        cmp     r9, rax
        movq    rax, xmm4
        jae     @discardInteger
@retainInteger:
        inc     r8
        jmp     @integerOne
@discardInteger:
        xor     r10d, r10d
@discardIntegerLoop:
        inc     r10
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @discardIntegerLoop
        cmp     eax, -2
        jne     @discardIntegerEnd
@discardIntegerFraction:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @discardIntegerFraction
@discardIntegerEnd:
        cmp     eax, -48
        je      @value
        jmp     @exponent
@moreFraction:
        movq    xmm4, rax
        mov     rax, 1000000000000000000
        cmp     r9, rax
        movq    rax, xmm4
        jae     @discardFraction
@retainFraction:
        inc     r8
        jmp     @fractionOne
@discardFraction:
        sub     r10, rcx
@discardFractionLoop:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @discardFractionLoop
        cmp     eax, -48
        je      @value
        jmp     @exponent
@exponent:
        or      eax, 32
        cmp     eax, 53 // 'e' or 'E', after subtracting '0' and setting bit 5
        jne     @slow
        inc     rcx
        movzx   eax, byte ptr [rcx]
        cmp     eax, '+'
        je      @exponentSign
        cmp     eax, '-'
        jne     @exponentFirst
        or      r11d, 2
@exponentSign:
        inc     rcx
        movzx   eax, byte ptr [rcx]
@exponentFirst:
        sub     eax, 48
        cmp     eax, 9
        ja      @slow
        mov     r8d, eax
        jmp     @exponentNext
@exponentLoop:
        cmp     r8d, 1000000 // the portable path validates huge exponents
        ja      @slow
        lea     r8, [r8 + r8 * 4]
        lea     r8, [rax + r8 * 2]
@exponentNext:
        inc     rcx
        movzx   eax, byte ptr [rcx]
        sub     eax, 48
        cmp     eax, 9
        jbe     @exponentLoop
        cmp     eax, -48
        jne     @slow
        test    r11b, 2
        jz      @exponentPositive
        sub     r10, r8
        jmp     @checkScale
@exponentPositive:
        add     r10, r8
@checkScale:
        jmp     @value
@nonFraction:
        test    r9, r9
        jz      @store
        cmp     r10, 22
        ja      @convertFull
        mulsd   xmm0, qword ptr [r8 + r10 * 8 + 31 * 8]
        jmp     @store
@largeMantissa:
        // Remove only exact trailing decimal zeroes; this can recover the Clinger path.
        mov     r8, $cccccccccccccccd
        mov     rcx, $1999999999999999
@trimMantissa:
        mov     rax, r9
        imul    rax, r8
        ror     rax, 1
        cmp     rax, rcx // quotient fits iff the original mantissa is divisible by ten
        ja      @convertFull
        mov     r9, rax
        inc     r10
        shr     rax, 53
        jnz     @trimMantissa
        jmp     @value
@convertFull:
        cmp     r10, -323 // retain the portable underflow/error contract
        jl      @slow
        mov     rax, rdx
        mov     rcx, r9
        mov     rdx, r10
        mov     r8d, r11d
        mov     r9, rax
        {$ifdef ABISYSVX64}
        mov     rdi, rcx
        mov     rsi, rdx
        mov     edx, r8d
        mov     rcx, r9
        {$endif ABISYSVX64}
        jmp     FinishExtendedNumber
@special:
        movq    rcx, xmm2
        xor     r11d, r11d
        cmp     byte ptr [rcx], ' '
        jne     @specialSign
@specialSpace:
        inc     rcx
        cmp     byte ptr [rcx], ' '
        je      @specialSpace
@specialSign:
        movzx   eax, byte ptr [rcx]
        cmp     eax, '+'
        je      @specialSigned
        cmp     eax, '-'
        jne     @specialFirst
        inc     r11d
@specialSigned:
        inc     rcx
@specialFirst:
        movzx   eax, byte ptr [rcx]
        or      eax, 32
        cmp     eax, 'n'
        je      @nan
        cmp     eax, 'i'
        jne     @slow
        movzx   eax, byte ptr [rcx + 1]
        or      eax, 32
        cmp     eax, 'n'
        jne     @slow
        movzx   eax, byte ptr [rcx + 2]
        or      eax, 32
        cmp     eax, 'f'
        jne     @slow
        movsd   xmm0, qword ptr [rip + NumberInfinity]
        jmp     @applySign
@nan:
        movzx   eax, byte ptr [rcx + 1]
        or      eax, 32
        cmp     eax, 'a'
        jne     @slow
        movzx   eax, byte ptr [rcx + 2]
        or      eax, 32
        cmp     eax, 'n'
        jne     @slow
        movsd   xmm0, qword ptr [rip + NumberNaN] // Pascal ignores the NaN sign
        jmp     @store
@slow:
        movq    rcx, xmm2
        {$ifdef ABISYSVX64}
        mov     rdi, rcx
        mov     rsi, rdx
        {$endif ABISYSVX64}
        jmp     GetExtendedPascal
end;
{$ifend}

{$endif WIN32DELPHI}

function ToDouble(const text: RawUtf8; out value: double): boolean;
var
  err: integer;
  v: double;
begin
  v := GetExtended(pointer(text), err);
  result := err = 0;
  if result then
    value := v;
end;

procedure Curr64ToStr(const Value: Int64; var result: RawUtf8);
var
  tmp: TTemp32;
  P: PAnsiChar;
  decim, L: cardinal;
begin
  if Value = 0 then
    result := SmallUInt32Utf8[0]
  else
  begin
    P := StrCurr64(@tmp[31], Value);
    L := @tmp[31] - P;
    if L > 4 then
    begin
      decim := PCardinal(P + L - SizeOf(cardinal))^; // 4 last digits = 4 decimals
      if decim = $30303030 then
        dec(L, 5)  // no decimal
      else if decim and $ffff0000 = $30300000 then
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
  tmp: TTemp32;
  P: PAnsiChar;
  decim: cardinal; // = 4 last digits to check if 0/2 decimals
begin
  P := StrCurr64(@tmp[31], Value);
  result := @tmp[31] - P;
  if result > 4 then
  begin
    decim := PCardinal(P + result - SizeOf(cardinal))^;
    if decim = $30303030 then // no decimal -> trunc trailing *.0000 chars
      dec(result, 5)
    else if decim and $ffff0000 = $30300000 then // 2 decimals -> trunc *.??00
      dec(result, 2);
  end;
  MoveFast(P^, Dest^, result);
end;

function StrToCurr64(P: PUtf8Char; NoDecimal: PBoolean): Int64;
var
  c: cardinal;
  minus: boolean;
  decim: cardinal;
begin
  result := 0;
  if P = nil then
    exit;
  while P^ in [#1 .. ' '] do
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
    decim := 2;
    inc(P);
  end
  else
    decim := 0;
  c := byte(P^) - 48;
  if c > 9 then
    exit;
  result := c;
  inc(P);
  repeat
    if P^ <> '.' then
    begin
      c := byte(P^) - 48;
      if c > 9 then
        break;
      result := result {$ifdef HASSLOWMUL64} shl 3 + result + result {$else} * 10 {$endif};
      inc(result, c);
      inc(P);
      if decim <> 0 then
      begin
        inc(decim);
        if decim < 5 then
          continue
        else
          break;
      end;
    end
    else
    begin
      inc(decim);
      inc(P);
    end;
  until false;
  if NoDecimal <> nil then
    if decim = 0 then
    begin
      NoDecimal^ := true;
      if minus then
        result := -result;
      exit;
    end
    else
      NoDecimal^ := false;
  if decim <> 5 then
    // decim=5 most of the time
    case decim of
      0, 1:
        result := result * 10000;
      {$ifdef HASSLOWMUL64}
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
      {$endif HASSLOWMUL64}
    end;
  if minus then
    result := -result;
end;

function StrToCurrency(P: PUtf8Char): currency;
var
  curr: currency; // safer with a transient local value
begin
  PInt64(@curr)^ := StrToCurr64(P, nil);
  result := curr;
end;

{$ifdef UNICODE}

function IntToString(Value: integer): string;
var
  tmp: TTemp24;
  P: PAnsiChar;
begin
  P := StrInt32(@tmp[23], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[23] - P, result);
end;

function IntToString(Value: cardinal): string;
var
  tmp: TTemp24;
  P: PAnsiChar;
begin
  P := StrUInt32(@tmp[23], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[23] - P, result);
end;

function IntToString(Value: Int64): string;
var
  tmp: TTemp32;
  P: PAnsiChar;
begin
  P := StrInt64(@tmp[31], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[31] - P, result);
end;

function DoubleToString(Value: Double): string;
var
  tmp: ShortString;
begin
  if PInt64(@Value)^ = 0 then
    result := '0'
  else
    Ansi7ToString(PWinAnsiChar(@tmp[1]), DoubleToShort(@tmp, Value), result);
end;

function Curr64ToString(Value: Int64): string;
var
  tmp: TTemp32;
begin
  Ansi7ToString(tmp, Curr64ToPChar(Value, tmp), result);
end;

{$else UNICODE}

function IntToString(Value: integer): string;
var
  tmp: TTemp24;
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
  tmp: TTemp24;
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
  tmp: TTemp32;
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
  if PInt64(@Value)^ = 0 then
    result := '0'
  else
    SetString(result, PAnsiChar(@tmp[1]), DoubleToShort(@tmp, Value));
end;

function Curr64ToString(Value: Int64): string;
begin
  result := Curr64ToStr(Value);
end;

{$endif UNICODE}

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
            if ((prec = 2) and
                (S[1] = '-')) or
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
          else if (c >= '0') and
                  (c <= '8') then
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

function ExtendedToShortNoExp(S: PShortString; Value: TSynExtended;
  Precision: integer): integer;
begin
  {$ifdef DOUBLETOSHORT_USEGRISU}
  if Precision = DOUBLE_PRECISION then
    DoubleToAscii(0, DOUBLE_PRECISION, Value, pointer(S))
  else
  {$endif DOUBLETOSHORT_USEGRISU}
    str(Value: 0: Precision, S^); // not str(Value:0,S) -> '  0.0E+0000'
  result := FloatStringNoExp(pointer(S), Precision);
  S^[0] := AnsiChar(result);
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
function ExtendedToShort(S: PShortString; Value: TSynExtended; Precision: integer): integer;
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
    PCardinal(S)^ := 1 + ord('0') shl 8;
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
    str(Value, S^);
    if S^[1] = ' ' then
    begin
      dec(S^[0]);
      MoveFast(S^[2], S^[1], ord(S^[0]));
    end;
    result := ord(S^[0]);
  end
  else
  begin
    str(Value: 0:Precision, S^); // not str(Value:0,S) -> '  0.0E+0000'
    result := FloatStringNoExp(pointer(S), Precision);
    S^[0] := AnsiChar(result);
  end;
end;

{$else not EXTENDEDTOSHORT_USESTR}

const
  /// RTL TFormatSettings closest to the JSON expectations
  // - used only as fallback for ExtendedToShort() without EXTENDEDTOSHORT_USESTR
  JsonFormatSettings: TFormatSettings = (
    ThousandSeparator: #0;
    DecimalSeparator: '.';
  {%H-});

function ExtendedToShort(S: PShortString; Value: TSynExtended; Precision: integer): integer;
{$ifdef UNICODE}
var
  i: PtrInt;
{$endif UNICODE}
begin
  // use ffGeneral: see https://synopse.info/forum/viewtopic.php?pid=442#p442
  result := FloatToText(PChar(@S^[1]), Value, fvExtended, ffGeneral, Precision, 0, JsonFormatSettings);
  {$ifdef UNICODE} // FloatToText(PWideChar) is faster than FloatToText(PAnsiChar)
  for i := 1 to result do
    PByteArray(S)[i] := PWordArray(PtrInt(S) - 1)[i];
  {$endif UNICODE}
  S^[0] := AnsiChar(result);
end;

{$endif EXTENDEDTOSHORT_USESTR}

function Utf8ToFloatNan(s: PUtf8Char; len: PtrInt): TFloatNan;
begin
  result := fnNumber;
  case len of
    3:
      case PInteger(s)^ and $dfdfdf of
        ord('N') + ord('A') shl 8 + ord('N') shl 16:
          result := fnNan;
        ord('I') + ord('N') shl 8 + ord('F') shl 16:
          result := fnInf;
      end;
    4:
      case PInteger(s)^ and $dfdfdfff of
        ord('+') + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24:
          result := fnInf;
        ord('-') + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24:
          result := fnNegInf;
      end;
  end;
end;

function ShortToFloatNan(const s: ShortString): TFloatNan;
begin
  result := Utf8ToFloatNan(@s[1], ord(s[0]));
end;

function RawUtf8ToFloatNan(const s: RawUtf8): TFloatNan;
begin
  result := Utf8ToFloatNan(pointer(s), length(s));
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
    FastSetString(result, @tmp[1], ExtendedToShort(@tmp, Value, Precision));
end;

function FloatToJsonNan(s: PShortString): PShortString;
var
  fn: TFloatNan;
begin
  result := s;
  case PInteger(s)^ and $dfdfdfff of
    3 + ord('N') shl 8 + ord('A') shl 16 + ord('N') shl 24:
      fn := fnNan;
    3 + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24,
    4 + (ord('+') and $df) shl 8 + ord('I') shl 16 + ord('N') shl 24:
      fn := fnInf;
    4 + (ord('-') and $df) shl 8 + ord('I') shl 16 + ord('N') shl 24:
      fn := fnNegInf;
  else
    exit;
  end;
  result := @JSON_NAN[fn];
end;

function ExtendedToJson(tmp: PShortString; Value: TSynExtended;
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

{$ifdef ASMINTEL} // our faster version using 128-bit x86_64 multiplication

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

{$endif ASMINTEL}

const
  // alpha =-61; gamma = 0
  // full cache: 1E-450 .. 1E+432, step = 1E+18
  // sparse = 1/10
  C_PWR10_DELTA = 18;
  C_PWR10_COUNT = 50;

type
  TDIY_FP_Cached_Power10 = record
    base:         array[0 .. 9] of TDIY_FP_Power_of_10;
    factor_plus:  array[0 .. 1] of TDIY_FP_Power_of_10;
    factor_minus: array[0 .. 1] of TDIY_FP_Power_of_10;
    // extra mantissa correction [ulp; signed]
    corrector:    array[0 .. C_PWR10_COUNT - 1] of shortint;
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

type
  TSplitFloat = packed record
    case byte of
      0: (f: double);
      1: (b: array[0..7] of byte);
      2: (w: array[0..3] of word);
      3: (d: array[0..1] of cardinal);
      4: (l: qword);
  end;

procedure d2a_unpack_float(const f: double; out minus: boolean;
  out result: TDIY_FP);   {$ifdef HASINLINE}inline;{$endif}
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

  C_EXP2_SPECIAL = C_EXP2_BIAS * 2 + 1; // $7ff
  C_MANT2_INTEGER = qword(1) shl C_FRAC2_BITS;
  C_EXP_POS = {$ifdef FPC_DOUBLE_HILO_SWAPPED} 0 {$else} 1 {$endif};

function doubleIsSpecial(const f: double): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := ((TSplitFloat(f).d[C_EXP_POS] shr 20) and C_EXP2_SPECIAL) = C_EXP2_SPECIAL;
end;

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
  PHash192(buf)^ := PHash192(P)^; // faster than MoveByOne(P,buf,result)
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
  p: PAnsiChar;
  exp: PStrRecConst; // 0..999 range is fine
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
  // Fraction significant digits
  if n_digits_req < n_digits_have then
    n_digits_have := n_digits_req;
  if n_digits_have > 0 then
  begin
    p^ := '.';
    inc(p);
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
  exp := @UINT_999[d_exp];
  PCardinal(p)^ := exp^.TextLo;
  inc(p, exp^.Header.length);
  // Store length
  str[0] := AnsiChar(p - str - 1);
end;

/// set one of special results with proper sign
procedure d2a_return_special(str: PAnsiChar; sign: integer;
  const spec: ShortString);
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
// result := ceiling[ ( alpha - e ) * log10(2) ] with fixed-point integer arithmetic
function d2a_k_comp(e, alpha: integer): integer; {$ifdef HASINLINE}inline;{$endif}
var
  x: integer;
begin
  x := alpha - e;
  if x > 0 then
    result := ((x * 78913) shr 18) + 1
  else if x < 0 then
    result := -(((-x) * 78913) shr 18)
  else
    result := 0;
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
    PCardinal(str)^ := 1 + ord('0') shl 8; // just return '0'
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
  // Append "sticky" digit if any
  if ({$ifdef CPU32} f {$else} fl {$endif} <> 0) and
     (n_digits_have >= n_digits_need + 1) then
  begin
    // single "<>0" digit is enough
    n_digits_have := n_digits_need + 2;
    buf[n_digits_need + 1] := 1;
  end;
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

function DoubleToShort(S: PShortString; const Value: double): integer;
var
  valueabs: double;
begin
  valueabs := abs(Value);
  if doubleIsSpecial(valueabs) or
     (valueabs > {$ifdef FPC}double{$endif}(DOUBLE_HI)) or
     (valueabs < {$ifdef FPC}double{$endif}(DOUBLE_LO)) then
    // = str(Value,S) for scientific notation outside of 1E-9<Value<1E9 range
    DoubleToAscii(C_NO_MIN_WIDTH, -1, Value, pointer(S))
  else
  begin
    // inlined DoubleToShortNoExp() = str(Value:0:15,S^)
    DoubleToAscii(0, DOUBLE_PRECISION, Value, pointer(S));
    S^[0] := AnsiChar(FloatStringNoExp(pointer(S), DOUBLE_PRECISION));
  end;
  result := ord(S^[0]);
end;

function DoubleToShortNoExp(S: PShortString; const Value: double): integer;
begin
  DoubleToAscii(0, DOUBLE_PRECISION, Value, pointer(S)); // = str(Value:0:15,S^)
  result := FloatStringNoExp(pointer(S), DOUBLE_PRECISION);
  S^[0] := AnsiChar(result);
end;

{$else} // use regular Extended version

function DoubleToShort(S: PShortString; const Value: double): integer;
begin
  result := ExtendedToShort(S, Value, DOUBLE_PRECISION);
end;

function DoubleToShortNoExp(S: PShortString; const Value: double): integer;
begin
  result := ExtendedToShortNoExp(S, Value, DOUBLE_PRECISION);
end;

{$endif DOUBLETOSHORT_USEGRISU}

function DoubleToJson(tmp: PShortString; const Value: double;
  NoExp: boolean): PShortString;
begin
  if PInt64(@Value)^ = 0 then
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

function DoubleToStr(const Value: Double): RawUtf8;
begin
  DoubleToStr(Value, result);
end;

procedure DoubleToStr(const Value: Double; var result: RawUtf8);
var
  tmp: ShortString;
begin
  if PInt64(@Value)^ = 0 then
    result := SmallUInt32Utf8[0]
  else
    FastSetString(result, @tmp[1], DoubleToShort(@tmp, Value));
end;

function FloatStrCopy(s, d: PUtf8Char): PUtf8Char;
var
  c: AnsiChar;
begin
  while s^ = ' ' do
    inc(s);
  c := s^;
  if (c = '+') or
     (c = '-') then
  begin
    inc(s);
    d^ := c;
    inc(d);
    c := s^;
  end;
  if c = '.' then
  begin
    PCardinal(d)^ := ord('0')+ord('.')shl 8; // '.5' -> '0.5'
    inc(d,2);
    inc(s);
    c := s^;
  end;
  if c in ['0' .. '9'] then
    repeat
      inc(s);
      d^ := c;
      inc(d);
      c := s^;
      if (c in ['0' .. '9']) or
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
      while c in ['0' .. '9'] do
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
  vd: PVarData;
  vt: cardinal;
  tmp: TVarData;
begin
  wasString := false;
  vd := VarDataFromVariant(V); // handle varVariantByRef
  vt := vd^.VType;
  case vt of // most simple types with a O(1) case jmp
    varEmpty,
    varNull:
      result := NULL_STR_VAR;
    varBoolean:
      if vd^.VBoolean then
        result := SmallUInt32Utf8[1] // normalize as '0' or '1'
      else
        result := SmallUInt32Utf8[0];
    varByte:
      result := SmallUInt32Utf8[vd^.VByte];
    varSmallint:
      Int32ToUtf8(vd^.VSmallInt, result);
    varShortInt:
      Int32ToUtf8(vd^.VShortInt, result);
    varWord:
      UInt32ToUtf8(vd^.VWord, result);
    varInteger,
    varOleInt:
      Int32ToUtf8(vd^.VInteger, result);
    varLongWord,
    varOleUInt:
      UInt32ToUtf8(vd^.VLongWord, result);
    varInt64:
      Int64ToUtf8(vd^.VInt64, result);
    varWord64:
      UInt64ToUtf8(vd^.VInt64, result);
    varSingle:
      ExtendedToStr(vd^.VSingle, SINGLE_PRECISION, result);
    varDouble:
      DoubleToStr(vd^.VDouble, result);
    varCurrency:
      Curr64ToStr(vd^.VInt64, result);
    varDate:
      begin
        wasString := true;
        _VariantToUtf8DateTimeIso8601(vd^.VDate, 'T', result, {withms=}false);
      end;
    varOleStr:
      begin
        wasString := true;
        RawUnicodeToUtf8(vd^.VAny, length(WideString(vd^.VAny)), result);
      end;
    varOlePAnsiChar: // = VT_LPSTR
      begin
        wasString := true;
        CurrentAnsiConvert.AnsiBufferToRawUtf8(vd^.VString, StrLen(vd^.VString), result);
      end;
    varOlePWideChar: // = VT_LPWSTR
      begin
        wasString := true;
        RawUnicodeToUtf8(vd^.VAny, StrLenW(vd^.VAny), result);
      end;
  else
    if vt = varString then
    begin
      wasString := true;
      {$ifdef HASCODEPAGE}
      AnyAnsiToUtf8Var(RawByteString(vd^.VString), result);
      {$else}
      result := RawUtf8(vd^.VString);
      {$endif HASCODEPAGE}
    end
    {$ifdef HASVARUSTRING}
    else if vt = varUString then
    begin
      wasString := true;
      RawUnicodeToUtf8(vd^.VAny, length(UnicodeString(vd^.VAny)), result);
    end
    {$endif HASVARUSTRING}
    else if vt and varByRef = 0 then
      // not recognizable vt -> seralize as JSON to handle also custom types
      _VariantSaveJson(V, twJsonEscape, result) // = mormot.core.variants.pas
    else // varByRef values appear with Automation/COM or DispInvoke()
      VariantToUtf8(SetVarDataUnRef(vt, vd, tmp)^, result, wasString);
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

function ToUtf8(const V: TVarData): RawUtf8;
var
  wasString: boolean;
begin
  VariantToUtf8(PVariant(@V)^, result, wasString);
end;

function VariantToUtf8(const V: Variant; var Text: RawUtf8): boolean;
begin
  VariantToUtf8(V, Text, result);
end;

function VarIsUtf8(const V: Variant; var Text: RawUtf8): boolean;
begin
  result := false;
  if VarIsString(V) then
    VariantToUtf8(V, Text, result);
end;

function VariantToText(const V: Variant; var Text: RawUtf8): boolean;
begin
  if VarIsEmptyOrNull(V) then
    FastAssignNew(Text)
  else
    VariantToUtf8(V, Text, result);
  result := Text <> '';
end;

function VariantSaveJson(const Value: variant; Escape: TTextWriterKind): RawUtf8;
begin
  _VariantSaveJson(Value, Escape, result);
end;

procedure VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8);
begin
  _VariantSaveJson(Value, Escape, result);
end;

procedure __VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8);
begin
  ESynException.RaiseU('VariantSaveJson() unsupported:' +
    ' please include mormot.core.variants to your uses clause');
end;

procedure __VariantToUtf8DateTimeIso8601(DT: TDateTime; FirstChar: AnsiChar;
  var result: RawUtf8; WithMS: boolean);
begin
  ESynException.RaiseU('VariantToUtf8(varDate) unsupported:' +
    ' please include mormot.core.datetime to your uses clause');
end;

function VariantCompAsText(A, B: PVarData; caseInsensitive: boolean): integer;
var
  au, bu: pointer;
  wasString: boolean;
begin // used e.g. by FastVarDataComp() for complex VTypes
  au := nil; // no try..finally for local RawUtf8 variables
  bu := nil;
  VariantToUtf8(PVariant(A)^, RawUtf8(au), wasString);
  VariantToUtf8(PVariant(B)^, RawUtf8(bu), wasString);
  result := SortDynArrayAnsiStringByCase[caseInsensitive](au, bu);
  FastAssignNew(au);
  FastAssignNew(bu);
end;

function VariantCompAsTempUtf8(A, B: PVarData; caseInsensitive: boolean;
  flags: TVariantToTempUtf8Flags): integer;
var
  at, bt: TTempUtf8;
begin // used e.g. by FastVarDataComp() for diverse non-complex VType
  VariantToTempUtf8(PVariant(A)^, at, flags);
  VariantToTempUtf8(PVariant(B)^, bt, flags);
  if caseInsensitive then
    result := StrIComp(at.Text, bt.Text)
  else
    result := StrComp(at.Text, bt.Text);
  TempUtf8Done(at);
  TempUtf8Done(bt);
end;

function AnyTextToDouble(const Text: RawUtf8; out V: double): boolean;
begin
  result := true;
  if Text = '' then
    PInt64(@V)^ := 0
  else if not ToDouble(Text, V) then
    if Assigned(_Iso8601ToDateTime) then
    begin
      V := _Iso8601ToDateTime(Text);
      result := V <> 0;
    end
    else
      result := false;
end;

function AnyVariantToDouble(const Value: Variant; out V: double): boolean;
var
  u: pointer;
begin
  result := false;
  if VarIsEmptyOrNull(Value) then // null means no value, so not a valid double
    exit;
  u := nil;
  result := VariantToDouble(Value, V);
  if result or
     not VarIsString(Value) or
     not VariantToText(Value, RawUtf8(u)) then
    exit;
  result := AnyTextToDouble(RawUtf8(u), V); // TDateTime or float text
  if u <> nil then
    FastAssignNew(u);
end;

function AnyVariantToInteger(const Value: Variant; out V: Int64): boolean;
var
  tmp: TTempUtf8;
  d: double;
begin
  result := true;
  if VariantToInt64(Value, V) then
    exit; // direct conversion from an integer value - null would return 0
  if VariantToDouble(Value, d) then
  begin
    V := trunc(d); // better truncate than convert to TTempUtf8
    exit;
  end;
  VariantToTempUtf8(Value, tmp, [vfNoAlloc, vfNullAsVoid]);
  result := GetInt64Bool(tmp.Text, V); // try from text e.g. '123'
end;

function AnyVariantToIntegerDef(const V: Variant; Default: Int64): Int64;
begin
  if VarIsEmptyOrNull(V) or
     not AnyVariantToInteger(V, result) then
    result := Default;
end;

function Int18ToChars3(Value: cardinal): RawUtf8;
begin
  Int18ToText(Value, FastSetString(result, 3));
end;

procedure Int18ToChars3(Value: cardinal; var result: RawUtf8);
begin
  Int18ToText(Value, FastSetString(result, 3));
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
  PWord(FastSetString(result, 3))^ := TwoDigitLookupW[Value div 10];
  PByteArray(result)[2] := (Value mod 10) + 48;
end;

function UInt4DigitsToUtf8(Value: cardinal): RawUtf8;
begin
  if Value > 9999 then
    Value := 9999;
  YearToPChar(Value, FastSetString(result, 4));
end;

function UInt4DigitsToShort(Value: cardinal): TShort7;
begin
  result[0] := #4;
  if Value > 9999 then
    Value := 9999;
  YearToPChar(Value, @result[1]);
end;

function UInt3DigitsToShort(Value: cardinal): TShort3;
begin
  if Value > 999 then
    Value := 999;
  YearToPChar(Value, @result[0]);
  result[0] := #3; // override first digit
end;

function UInt2DigitsToShort(Value: byte): TShort3;
begin
  result[0] := #2;
  if Value > 99 then
    Value := 99;
  PWord(@result[1])^ := TwoDigitLookupW[Value];
end;

function UInt2DigitsToShortFast(Value: byte): TShort3;
begin
  result[0] := #2;
  PWord(@result[1])^ := TwoDigitLookupW[Value];
end;

function IPToCardinal(aIP: PUtf8Char; out aValue: cardinal): boolean;
var
  i, c: cardinal;
  b: array[0..3] of byte;
begin
  aValue := 0;
  result := false;
  if (aIP = nil) or
     ((PCardinalArray(aIP)[0] = HOST_127) and    // 127.
      (PCardinalArray(aIP)[1] = HOST_127_4) and  // 0.0.
      (PWordArray(aIP)[4] = ord('1'))) then      // 1
    exit;
  for i := 0 to 3 do
  begin
    c := GetNextItemCardinal(aIP, '.');
    if (c > 255) or
       ((aIP = nil) and
        (i < 3)) then
      exit;
    b[i] := c;
  end;
  if PCardinal(@b)^ = $0100007f then // may be e.g. '127.000.000.001'
    exit;
  aValue := PCardinal(@b)^;
  result := true;
end;

function IPToCardinal(const aIP: RawUtf8; out aValue: cardinal): boolean;
begin
  result := IPToCardinal(pointer(aIP), aValue);
end;

function IPToCardinal(const aIP: RawUtf8): cardinal;
begin
  IPToCardinal(pointer(aIP), result);
end;


{ ************ Text Formatting functions }

function VarRecAsChar(V: PVarRec): integer;
begin
  case V^.VType of
    vtChar:
      result := ord(V^.VChar);
    vtWideChar:
      result := ord(V^.VWideChar);
  else
    result := 0;
  end;
end;

function VarRecAs(V: PVarRec; aClass: TClass): pointer;
begin
  if (V^.VType = vtObject) and
     (V^.VObject <> nil) and
     V^.VObject.InheritsFrom(aClass) then
    result := V^.VObject
  else
    result := nil;
end;

function VarRecIsVoid(V: PVarRec): boolean;
begin // we consider a boolean to be never void by design
  result := (V^.VType <> vtBoolean) and VarRecIsDefault(V);
end;

function VarRecIsDefault(V: PVarRec): boolean;
begin
  case V^.VType of
    vtString:
      result := V^.VString^[0] = #0;
    vtAnsiString,
    {$ifdef HASVARUSTRING}
    vtUnicodeString,
    {$endif HASVARUSTRING}
    vtWideString,
    vtPChar,
    vtPWideChar,
    vtPointer,
    vtObject,
    vtClass,
    vtInterface:
      result := V^.VPointer = nil; // void pointer/string value
    vtChar:
      result := V^.VChar = #0;
    vtWideChar:
      result := V^.VWideChar = #0;
    vtBoolean:
      result := not V^.VBoolean; // false means default
    vtInteger:
      result := V^.VInteger = 0;
    {$ifdef FPC}
    vtQWord,
    {$endif FPC}
    vtCurrency,
    vtInt64:
      result := V^.VInt64^ = 0;
    vtExtended:
      result := V^.VExtended^ = 0;
    vtVariant:
      result := VarIsEmptyOrNull(V^.VVariant^);
  else
    result := false;
  end;
end;

function VarRecToInt64(V: PVarRec; out value: Int64): boolean;
begin
  case V^.VType of
    vtInteger:
      value := V^.VInteger;
    {$ifdef FPC} vtQWord, {$endif}
    vtInt64:
      value := V^.VInt64^;
    vtBoolean:
      if V^.VBoolean then // normalize
        value := 1
      else
        value := 0;
    vtVariant:
      value := V^.VVariant^;
  else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function VarRecToDouble(V: PVarRec; out value: double): boolean;
begin
  case V^.VType of
    vtInteger:
      value := V^.VInteger;
    vtInt64:
      value := V^.VInt64^;
    {$ifdef FPC}
    vtQWord:
      value := V^.VQWord^;
    {$endif FPC}
    vtBoolean:
      if V^.VBoolean then // normalize
        value := 1
      else
        value := 0;
    vtExtended:
      value := V^.VExtended^;
    vtCurrency:
      value := V^.VCurrency^;
    vtVariant:
      value := V^.VVariant^;
  else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function PrepareTempUtf8(var Res: TTempUtf8; Len: PtrInt; NoTempAlloc: boolean): boolean;
begin
  result := false;
  if Len >= SizeOf(Res.Temp) then // memory allocation needed (with ending #0)
  begin
    if NoTempAlloc then
      exit; // e.g. when try to extract a float or an iso8601 date
    Res.TempRawUtf8 := FastNewString(Len, CP_UTF8); // new RawUtf8
    Res.Text := Res.TempRawUtf8;
  end
  else
    Res.Text := @Res.Temp; // we can use the 24 bytes stack buffer (very common)
  Res.Len := Len;
  result := true;
end;

procedure WideToTempUtf8(WideChar: PWideChar; WideCharCount: PtrUInt;
  var Res: TTempUtf8; NoTempAlloc: boolean);
begin
  Res.Text := nil;
  Res.Len := 0;
  if (WideChar <> nil) and
     (WideCharCount <> 0) then
    if IsAnsiCompatibleW(WideChar, WideCharCount) then // most common case
    begin
      if not PrepareTempUtf8(Res, WideCharCount, NoTempAlloc) then
        exit;
      Res.Text[WideCharCount] := #0; // ensure is #0 terminated
      repeat
        dec(WideCharCount);
        Res.Text[WideCharCount] := AnsiChar(ord(WideChar[WideCharCount]));
      until WideCharCount = 0;
    end
    else if PrepareTempUtf8(Res, WideCharCount * 3, NoTempAlloc) then
      Res.Len := RawUnicodeToUtf8(Res.Text, Res.Len + 1, WideChar, WideCharCount, []);
end;

function BStrToTempUtf8(bstr: pointer; var Res: TTempUtf8; NoTemp: boolean): boolean;
begin
  WideToTempUtf8(bstr, length(WideString(bstr)), Res, NoTemp);
  result := true;
end;

{$ifdef HASVARUSTRING}
function UStrToTempUtf8(ustr: pointer; var Res: TTempUtf8; NoTemp: boolean): boolean;
begin
  WideToTempUtf8(ustr, length(UnicodeString(ustr)), Res, NoTemp);
  result := true;
end;
{$endif HASVARUSTRING}

function DoubleToTempUtf8(V: double; var Res: TTempUtf8): boolean;
var
  tmp: ShortString;
begin
  Res.Len := MinPtrInt(High(Res.Temp), DoubleToShort(@tmp, V)); // truncate
  Res.Text := @Res.Temp;
  MoveFast(tmp[1], Res.Temp, Res.Len);
  Res.Temp[Res.Len] := #0; // ensure #0 terminated
  result := false;
end;

function Curr64ToTempUtf8(V: Int64; var Res: TTempUtf8): boolean;
begin
  Res.Len := Curr64ToPChar(V, @Res.Temp);
  Res.Text := @Res.Temp;
  Res.Temp[Res.Len] := #0; // #0 terminated
  result := false;
end;

function PtrIntToTempUtf8(V: PtrInt; var Res: TTempUtf8): boolean;
begin
  if PtrUInt(V) <= high(UINT_999) then
    with UINT_999[V] do
    begin
      Res.Text := @TextLo;
      Res.Len := Header.length;
    end
  else
  begin
    Res.Text := PUtf8Char(StrInt32(@Res.Temp[23], V));
    Res.Len := @Res.Temp[23] - Res.Text;
    Res.Temp[23] := #0; // make #0 terminated
  end;
  result := false;
end;

{$ifdef CPU32}
function Int64ToTempUtf8(V: PInt64; var Res: TTempUtf8): boolean;
begin
  Res.Text := PUtf8Char(StrInt64(@Res.Temp[23], V^));
  Res.Len := @Res.Temp[23] - Res.Text;
  Res.Temp[23] := #0; // make #0 terminated
  result := false;
end;
{$endif CPU32}

function QWordToTempUtf8(const V: QWord; var Res: TTempUtf8): boolean;
begin
  Res.Text := PUtf8Char(StrUInt64(@Res.Temp[23], V)); // also cardinal
  Res.Len := @Res.Temp[23] - Res.Text;
  Res.Temp[23] := #0; // make #0 terminated
  result := false;
end;

function VariantToTempUtf8(const V: variant; var Res: TTempUtf8;
  Flags: TVariantToTempUtf8Flags): boolean;
var
  tmp: TVarData;
  vd: PVarData;
  vt: cardinal;
label
  n, dt;
begin
  result := false;             // wasString=false by default (assume numbers)
  Res.TempRawUtf8 := nil;      // no allocation by default - and avoid GPF
  vd := VarDataFromVariant(V); // handle varVariantByRef
  vt := vd^.VType;
  case vt of // most simple types with a O(1) case jmp
    varEmpty,
    varNull:
n:    if vfNullAsVoid in Flags then
      begin
        result := true;
        Res.Text := nil;
        Res.Len := 0;
      end
      else
      begin
        Res.Text := pointer(NULL_STR_VAR); // 'null' + wasString=false
        Res.Len := 4;
      end;
    varByte:
      PtrIntToTempUtf8(vd^.VByte, Res);
    varSmallint:
      PtrIntToTempUtf8(vd^.VSmallInt, Res);
    varShortInt:
      PtrIntToTempUtf8(vd^.VShortInt, Res);
    varWord:
      PtrIntToTempUtf8(vd^.VWord, Res);
    varBoolean:
      if vfBooleanAsInt in Flags then
      begin
        Res.Temp[0] := '0';
        if vd^.VBoolean then // normalize
          inc(Res.Temp[0]);
        Res.Text := @Res.Temp;
        Res.Len := 1;
      end
      else if vd^.VBoolean then
      begin
        Res.Text := @BOOL_STR[true][1]; // 'false' + wasString=false
        Res.Len := 4;
      end
      else
      begin
        Res.Text := @BOOL_STR[false][1]; // 'true' + wasString=false
        Res.Len := 5;
      end;
    varInteger,
    varOleInt:
      PtrIntToTempUtf8(vd^.VInteger, Res);
    varLongWord,
    varOleUInt:
      QWordToTempUtf8(vd^.VInt64, Res);  // seldom called
    varInt64:
      {$ifdef CPU64}
      PtrIntToTempUtf8(vd^.VInt64, Res);
      {$else}
      Int64ToTempUtf8(@vd^.VInt64, Res);
      {$endif CPU64}
    varWord64:
      QWordToTempUtf8(vd^.VInt64, Res);
    varSingle:
      DoubleToTempUtf8(vd^.VSingle, Res);
    varDouble:
dt:   DoubleToTempUtf8(vd^.VDouble, Res);
    varCurrency:
      Curr64ToTempUtf8(vd^.VInt64, Res);
    varDate:
      begin
        if vfDateAsFloat in Flags then
          goto dt;
        result := true;
        _VariantToTempUtf8DateTimeIso8601(vd^.VDate, 'T', Res, {withMS=}false);
      end;
    varOleStr:
      result := BStrToTempUtf8(vd^.VAny, Res, vfNoAlloc in Flags);
  else
    case vt of
      varString: // most common non-simple type
        begin
          result := true;
          Res.Text := vd^.VString; // assume RawUtf8
          Res.Len := length(RawUtf8(vd^.VString));
        end;
      {$ifdef HASVARUSTRING}
      varUString:
        result := UStrToTempUtf8(vd^.VAny, Res, vfNoAlloc in Flags);
      {$endif HASVARUSTRING}
    else
      if vt and varByRef = 0 then
      begin
        // not recognizable vt -> serialize as JSON to handle also custom types
        if Flags * [vfNoAlloc, vfNoComplex] <> [] then
          goto n;  // 'null' + result=false
        result := true;
        _VariantSaveJson(V, twJsonEscape, RawUtf8(Res.TempRawUtf8));
        Res.Text := pointer(Res.TempRawUtf8);
        Res.Len := length(RawUtf8(Res.TempRawUtf8));
      end
      else // varByRef values appear with Automation/COM or DispInvoke()
        VariantToTempUtf8(SetVarDataUnRef(vt, vd, tmp)^, Res, Flags);
    end;
  end;
end;

procedure VariantToAdder(var Adder: TSynTempAdder; const V: variant;
  Flags: TVariantToTempUtf8Flags);
var
  u: TTempUtf8;
begin
  VariantToTempUtf8(V, u, Flags);
  if u.Len <= 0 then
    exit;
  Adder.Add(u.Text, u.Len);
  TempUtf8Done(u);
end;

function VarRecToTempUtf8(V: PVarRec; var Res: TTempUtf8; wasString: PBoolean): boolean;
var
  isString: boolean;
label
  n;
begin
  isString := true;
  Res.TempRawUtf8 := nil; // no allocation by default - and avoid GPF
  case V^.VType of
    vtString:
      begin
        Res.Text := @V^.VString^[1]; // may NOT be #0 terminated
        Res.Len := ord(V^.VString^[0]);
      end;
    vtAnsiString: // expect UTF-8 content
      begin
        Res.Text := V^.VPointer;
        Res.Len := length(RawUtf8(V^.VPointer));
      end;
    {$ifdef HASVARUSTRING}
    vtUnicodeString:
      UStrToTempUtf8(V^.VPointer, Res, false);
    {$endif HASVARUSTRING}
    vtWideString:
      BStrToTempUtf8(V^.VPointer, Res, false);
    vtPChar: // expect UTF-8 content
      begin
        Res.Text := V^.VPointer;
        Res.Len := mormot.core.base.StrLen(V^.VPointer);
      end;
    vtChar:
      begin
        Res.Temp[0] := V^.VChar; // V may be on transient stack (alf: FPC)
        Res.Text := @Res.Temp;
        Res.Len := 1;
      end;
    vtPWideChar:
      WideToTempUtf8(V^.VPWideChar, StrLenW(V^.VPWideChar), Res, false);
    vtWideChar:
      WideToTempUtf8(@V^.VWideChar, 1, Res, false);
    vtBoolean:
      begin
        isString := false;
        if V^.VBoolean then // normalize as '0' or '1'
          Res.Text := @UINT_999[1].TextLo
        else
          Res.Text := @UINT_999[0].TextLo;
        Res.Len := 1;
      end;
    vtInteger:
      isString := PtrIntToTempUtf8(V^.VInteger, Res);
    vtInt64:
      {$ifdef CPU64}
      isString := PtrIntToTempUtf8(V^.VInt64^, Res);
      {$else}
      isString := Int64ToTempUtf8(V^.VInt64, Res);
      {$endif CPU64}
    {$ifdef FPC}
    vtQWord:
      isString := QwordToTempUtf8(V^.VQWord^, Res);
    {$endif FPC}
    vtCurrency:
      isString := Curr64ToTempUtf8(V^.VInt64^, Res);
    vtExtended:
      isString := DoubleToTempUtf8(V^.VExtended^, Res);
    vtPointer, vtInterface:
      PtrIntToTempUtf8(PtrInt(V^.VPointer), Res); // keep isString=true
    vtClass:
      begin
        if V^.VClass = nil then
          goto n;
        Res.Text := PPUtf8Char(PtrInt(PtrUInt(V^.VClass)) + vmtClassName)^ + 1;
        Res.Len := ord(Res.Text[-1]);
      end;
    vtObject:
      begin
        if V^.VObject = nil then
          goto n;
        Res.Text := PPUtf8Char(PPtrInt(V^.VObject)^ + vmtClassName)^ + 1;
        Res.Len := ord(Res.Text[-1]);
      end;
    vtVariant:
      isString := VariantToTempUtf8(V^.VVariant^, Res);
  else
n:  Res.Len := 0;
  end;
  if wasString <> nil then
    wasString^ := isString;
  result := Res.Len <> 0;
end;

procedure VarRecToUtf8(V: PVarRec; var result: RawUtf8; wasString: PBoolean);
var
  isString: boolean;
label
  none;
begin
  isString := false;
  case V^.VType of
    vtString: // assume UTF-8
      begin
        isString := true;
        FastSetString(result, @V^.VString^[1], ord(V^.VString^[0]));
      end;
    vtAnsiString:
      begin
        isString := true;
        AnyAnsiToUtf8Var(RawByteString(V^.VPointer), result); // use codepage
      end;
    {$ifdef HASVARUSTRING}
    vtUnicodeString:
      begin
        isString := true;
        RawUnicodeToUtf8(V^.VPointer, length(UnicodeString(V^.VPointer)), result);
      end;
    {$endif HASVARUSTRING}
    vtWideString:
      begin
        isString := true;
        RawUnicodeToUtf8(V^.VPointer, length(WideString(V^.VPointer)), result);
      end;
    vtPChar:
      begin
        isString := true;
        FastSetString(result, V^.VPChar, mormot.core.base.StrLen(V^.VPChar));
      end;
    vtChar:
      begin
        isString := true;
        FastSetString(result, PAnsiChar(@V^.VChar), 1);
      end;
    vtPWideChar:
      begin
        isString := true;
        RawUnicodeToUtf8(V^.VPWideChar, StrLenW(V^.VPWideChar), result);
      end;
    vtWideChar:
      begin
        isString := true;
        RawUnicodeToUtf8(@V^.VWideChar, 1, result);
      end;
    vtBoolean:
      if V^.VBoolean then // normalize  as '0' or '1'
        result := SmallUInt32Utf8[1]
      else
        result := SmallUInt32Utf8[0];
    vtInteger:
      Int32ToUtf8(V^.VInteger, result);
    vtInt64:
      Int64ToUtf8(V^.VInt64^, result);
    {$ifdef FPC}
    vtQWord:
      UInt64ToUtf8(V^.VQWord^, result);
    {$endif FPC}
    vtCurrency:
      Curr64ToStr(V^.VInt64^, result);
    vtExtended:
      DoubleToStr(V^.VExtended^,result);
    vtPointer:
      UInt32ToUtf8(PtrUInt(V^.VPointer), result); // isString=false
    vtClass:
      begin
        isString := true;
        if V^.VClass <> nil then
          ClassToText(V^.VClass, result)
        else
none:     FastAssignNew(result);
      end;
    vtObject:
      if V^.VObject <> nil then
        ClassToText(PClass(V^.VObject)^, result)
      else
        goto none;
    vtInterface:
    {$ifdef HASINTERFACEASTOBJECT}
      if V^.VInterface <> nil then
        ClassToText((IInterface(V^.VInterface) as TObject).ClassType, result)
      else
        goto none;
    {$else}
      PointerToHex(V^.VInterface, result);
    {$endif HASINTERFACEASTOBJECT}
    vtVariant:
      VariantToUtf8(V^.VVariant^, result, isString);
  else
    goto none;
  end;
  if wasString <> nil then
    wasString^ := isString;
end;

function VarRecToUtf8IsString(const V: TVarRec; var value: RawUtf8): boolean;
begin
  VarRecToUtf8(@V, value, @result);
end;

procedure VarRecToAdder(var Adder: TSynTempAdder; V: PVarRec);
var
  tmp: TTempUtf8;
begin
  VarRecToTempUtf8(V, tmp, nil);
  Adder.Add(tmp.Text, tmp.Len);
  TempUtf8Done(tmp);
end;

procedure VarRecToInlineValue(const V: TVarRec; var result: RawUtf8);
var
  wasString: boolean;
  tmp: RawUtf8;
begin
  VarRecToUtf8(@V, tmp, @wasString);
  if wasString then
    QuotedStr(tmp, '"', result)
  else
    result := tmp;
end;

function FormatUtf8(const Format: RawUtf8; const Args: array of const): RawUtf8;
begin
  FormatUtf8(Format, Args, result);
end;

function FormatVariant(const Format: RawUtf8; const Args: array of const): variant;
begin
  ClearVariantForString(result);
  FormatUtf8(Format, Args, RawUtf8(TVarData(result).VString));
end;

type
  // 4KB info on stack - only supported token is %, with any const arguments
  {$ifdef USERECORDWITHMETHODS}
  TFormatUtf8 = record
  {$else}
  TFormatUtf8 = object
  {$endif USERECORDWITHMETHODS}
  public
    max, last: PTempUtf8;
    size: PtrInt;
    blocks: array[0..80] of TTempUtf8; // 4KB to avoid most heap allocations
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    procedure InitParse(const Format: RawUtf8; Arg: PVarRec; ArgCount: PtrInt);
    procedure InitDelim(Arg: PVarRec; ArgCount: integer; EndWithDelim: boolean;
      Delim: AnsiChar);
    procedure AddText(const SomeText: RawUtf8);
    procedure AddVarRec(Arg: PVarRec; ArgCount: PtrUInt);
    procedure DoAppend(var Text: RawUtf8; Arg: PVarRec; ArgCount: PtrInt);
    procedure DoPrepend(var Text: RawUtf8; Arg: PVarRec; ArgCount, CodePage: PtrInt);
    procedure WriteAll(Dest: PUtf8Char; d: PTempUtf8);
      {$ifdef HASINLINE} inline; {$endif}
    procedure WriteString(var result: string);
    function WriteMax(Dest: PUtf8Char; MaxSize: PtrUInt): PUtf8Char;
  end;

procedure TFormatUtf8.Init;
begin
  last := @blocks;
  max := @PByteArray(last)[SizeOf(blocks)];
  size := 0;
end;

procedure TooManyArgs;
begin // blocks[] allows up to 40 arguments
  ESynException.RaiseU('TFormatUtf8: too many arguments');
end;

procedure TFormatUtf8.AddVarRec(Arg: PVarRec; ArgCount: PtrUInt);
var
  c: PTempUtf8;
begin
  if ArgCount = 0 then
    exit;
  c := last;
  repeat
    if PtrUInt(c) >= PtrUInt(max) then
      TooManyArgs;
    if VarRecToTempUtf8(Arg, c^) then
    begin
      inc(size, c^.Len);
      inc(c);
    end;
    inc(Arg);
    dec(ArgCount)
  until ArgCount = 0;
  last := c;
end;

procedure TFormatUtf8.WriteAll(Dest: PUtf8Char; d: PTempUtf8);
begin
  repeat
    MoveFast(d^.Text^, Dest^, d^.Len); // no MoveByOne() - may be huge result
    inc(Dest, d^.Len);
    TempUtf8Done(d^);
    inc(d);
  until d = last;
end;

procedure TFormatUtf8.InitParse(const Format: RawUtf8; Arg: PVarRec; ArgCount: PtrInt);
var
  F, FDeb: PUtf8Char;
  c: PTempUtf8;
begin
  c := @blocks;
  max := @PByteArray(c)[SizeOf(blocks)];
  size := 0;
  F := pointer(Format);
  if F <> nil then
    repeat
      if F^ = #0 then
        break;
      if PtrUInt(c) >= PtrUInt(max) then
        TooManyArgs;
      if F^ <> '%' then
      begin
        FDeb := F;
        repeat
          inc(F);
        until (F^ = '%') or
              (F^ = #0);
        c^.Len := F - FDeb;
        if c^.Len <> 0 then // %%% does not need any blocks[] slot
        begin
          c^.Text := FDeb;
          inc(size, c^.Len);
          c^.TempRawUtf8 := nil;
          inc(c);
        end;
        continue;
      end;
      inc(F); // jump '%'
      if ArgCount <> 0 then
      begin
        if VarRecToTempUtf8(Arg, c^) then
        begin
          inc(size, c^.Len);
          inc(c);
        end;
        inc(Arg);
        dec(ArgCount);
        continue;
      end
      else // ArgCount = 0 -> no more available Args -> add all remaining text
      begin
        if F^ <> #0 then
        begin
          if PtrUInt(c) >= PtrUInt(max) then
            TooManyArgs;
          c^.Text := F;
          c^.Len := length(Format) - (F - pointer(Format));
          inc(size, c^.Len);
          c^.TempRawUtf8 := nil;
          inc(c);
        end;
        break;
      end;
    until false;
  last := c;
end;

procedure TFormatUtf8.InitDelim(Arg: PVarRec; ArgCount: integer;
  EndWithDelim: boolean; Delim: AnsiChar);
var
  c: PTempUtf8;
begin
  c := @blocks;
  max := @PByteArray(c)[SizeOf(blocks)];
  size := 0;
  if ArgCount <= 0 then
   exit;
  repeat
    if PtrUInt(c) >= PtrUInt(max) then
      TooManyArgs;
    if VarRecToTempUtf8(Arg, c^) then
    begin
      inc(size, c^.Len);
      if (c^.Text[c^.Len - 1] <> Delim) and
         (EndWithDelim or
          (ArgCount <> 1)) then // append delimiter
      begin
        inc(c);
        if PtrUInt(c) >= PtrUInt(max) then
          TooManyArgs;
        c^.Len := 1;
        c^.Text := @c^.Temp;
        c^.Temp[0] := Delim;
        c^.TempRawUtf8 := nil;
        inc(size);
      end;
      inc(c);
    end;
    inc(Arg);
    dec(ArgCount);
  until ArgCount = 0;
  last := c;
end;

procedure TFormatUtf8.AddText(const SomeText: RawUtf8);
var
  c: PTempUtf8;
begin // in our internal usage, we know that SomeText is <> ''
  c := last;
  if PtrUInt(c) >= PtrUInt(max) then
    TooManyArgs;
  c^.Len := length(SomeText);
  inc(size, c^.Len);
  c^.Text := pointer(SomeText);
  c^.TempRawUtf8 := nil;
  inc(last);
end;

procedure TFormatUtf8.DoAppend(var Text: RawUtf8; Arg: PVarRec; ArgCount: PtrInt);
begin
  AddVarRec(Arg, ArgCount);
  if size = 0 then
    exit; // nothing to add
  ArgCount := length(Text);
  SetLength(Text, ArgCount + size);
  WriteAll(PUtf8Char(@PByteArray(Text)[ArgCount]), @blocks); // append Arg[] text
end;

procedure TFormatUtf8.DoPrepend(var Text: RawUtf8; Arg: PVarRec;
  ArgCount, CodePage: PtrInt);
var
  new: PUtf8Char;
begin
  if ArgCount <= 0 then
    exit;
  Init;
  AddVarRec(Arg, ArgCount);
  if size = 0 then
    exit; // nothing to add
  ArgCount := length(Text);
  new := FastNewString(size + ArgCount, CodePage);
  MoveFast(pointer(Text)^, new[size], ArgCount);
  FastAssignNew(Text, new);
  WriteAll(new, @blocks);
end;

function TFormatUtf8.WriteMax(Dest: PUtf8Char; MaxSize: PtrUInt): PUtf8Char;
var
  d: PTempUtf8;
  avail: PtrUInt;
begin
  if (MaxSize > 0) and
     (size <> 0) and
     (Dest <> nil) then
  begin
    inc(MaxSize, PtrUInt(Dest));
    d := @blocks;
    repeat
      avail := MaxSize - PtrUInt(Dest);
      if PtrUInt(d^.Len) > avail then // avoid buffer overflow
      begin
        MoveFast(d^.Text^, Dest^, avail);
        repeat
          TempUtf8Done(d^);
          inc(d);
        until d = last; // avoid memory leak
        result := PUtf8Char(MaxSize);
        exit;
      end;
      MoveFast(d^.Text^, Dest^, d^.Len);
      inc(Dest, d^.Len);
      TempUtf8Done(d^);
      inc(d);
    until d = last;
  end;
  result := Dest;
end;

procedure TFormatUtf8.WriteString(var result: string);
var
  temp: TSynTempBuffer; // will avoid most memory allocations
begin
  result := '';
  if size = 0 then
    exit;
  {$ifndef UNICODE}
  if Unicode_CodePage = CP_UTF8 then // e.g. on POSIX or Windows + Lazarus
  begin
    WriteAll(FastSetString(RawUtf8(result), size), @blocks);
    exit; // here string=UTF8String=RawUtf8
  end;
  {$endif UNICODE}
  temp.Init(size);
  WriteAll(temp.buf, @blocks);
  Utf8DecodeToString(temp.buf, size, result);
  temp.Done;
end;

procedure FormatUtf8(const Format: RawUtf8; const Args: array of const;
  var Result: RawUtf8);
var
  f: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    Result := Format
  else if cardinal(PWord(Format)^) = ord('%') then // optimize raw conversion
    VarRecToUtf8(@Args[0], Result)
  else
  begin
    f.InitParse(Format, @Args[0], length(Args)); // handle all supplied Args[]
    if f.size <> 0 then
      f.WriteAll(FastSetString(Result, f.size), @f.blocks)
    else
      FastAssignNew(Result);
  end;
end;

procedure FormatUtf8Raw(const Format: RawUtf8; Args: PVarRec; ArgsCount: PtrInt;
  var Result: RawUtf8; var Temp: TTextWriterStackBuffer);
var
  f: TFormatUtf8 absolute Temp;
begin
  f.InitParse(Format, Args, ArgsCount); // handle all supplied Args[]
  if f.size <> 0 then
    f.WriteAll(FastSetString(Result, f.size), @f.blocks)
  else
    FastAssignNew(Result);
end;

function FormatBufferRaw(const Format: RawUtf8; Args: PVarRec; ArgsCount: PtrInt;
  Dest: pointer; DestLen: PtrInt): PUtf8Char;
var
  f: TFormatUtf8;
begin
  f.InitParse(Format, Args, ArgsCount);
  result := f.WriteMax(Dest, DestLen);
end;

function FormatBuffer(const Format: RawUtf8; const Args: array of const;
  Dest: pointer; DestLen: PtrInt): PtrInt;
begin
  if (Dest = nil) or
     (DestLen <= 0) then
    result := 0 // avoid buffer overflow
  else
    result := FormatBufferRaw(Format, @Args[0], length(Args),
                Dest, DestLen) - PUtf8Char(Dest);
end;

procedure FormatShort(const Format: RawUtf8; const Args: array of const;
  var result: ShortString);
var
  f: TFormatUtf8;
begin
  f.InitParse(Format, @Args[0], length(Args));
  result[0] := AnsiChar(f.WriteMax(@result[1], high(result)) - @result[1]);
end;

function FormatToShort(const Format: RawUtf8;
  const Args: array of const): ShortString;
begin
  result[0] := AnsiChar(FormatBufferRaw(
    Format, @Args[0], length(Args), @result[1], high(result)) - @result[1]);
end;

procedure FormatAdder(var Dest: TSynTempAdder; const Format: RawUtf8; const Args: array of const);
var
  f: TFormatUtf8;
begin
  f.InitParse(Format, @Args[0], length(Args));
  f.WriteAll(Dest.Add(f.size), @f.blocks);
end;

procedure FormatString(const Format: RawUtf8; const Args: array of const;
  var result: string);
var
  f: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    Utf8ToStringVar(Format, result)
  else
  begin
    f.InitParse(Format, @Args[0], length(Args));
    f.WriteString(result);
  end;
end;

function FormatString(const Format: RawUtf8; const Args: array of const): string;
begin
  FormatString(Format, Args, result);
end;

procedure AppendLine(var Text: RawUtf8; const Args: array of const;
  const Separator: RawUtf8);
var
  seplen, textlen: PtrUInt;
  f: TFormatUtf8;
begin
  {%H-}f.Init;
  textlen := PtrUInt(Text);
  if textlen <> 0 then
  begin
    textlen := PStrLen(textlen - _STRLEN)^;
    seplen := PtrUInt(Separator);
    if seplen <> 0 then
    begin
      seplen := PStrLen(seplen - _STRLEN)^;
      if (seplen <= textlen) and
         (text[textlen] <> Separator[seplen]) then
       begin // not already ending with last Separator chars
         f.blocks[0].Len := seplen;
         f.blocks[0].Text := pointer(Separator);
         f.blocks[0].TempRawUtf8 := nil;
         f.size := seplen;
         inc(f.last);
       end;
    end;
  end;
  f.DoAppend(Text, @Args[0], length(Args));
end;

procedure Append(var Text: RawUtf8; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.Init;
  f.DoAppend(Text, @Args[0], length(Args));
end;

procedure Append(var Text: RawByteString; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.Init;
  f.DoAppend(RawUtf8(Text), @Args[0], length(Args));
  if Text <> '' then
    FakeCodePage(Text, CP_RAWBYTESTRING);
end;

procedure _App1(res: PPtrUInt; add: pointer; len: PtrUInt; const cp: integer);
  {$ifdef HASINLINE} inline; {$endif}
var
  t, r: PtrUInt;
begin
  t := res^;
  if t = 0 then
  begin
    t := PtrUInt(FastNewString(len, cp)); // first time
    res^ := t;
  end
  else
  begin
    t := PStrLen(t - _STRLEN)^;
    SetLength(PRawUtf8(res)^, t + len); // realloc
    r := res^;
    inc(t, r);
    {$ifdef HASCODEPAGE}
    PStrRec(r - _STRRECSIZE)^.CodePage := cp; // force the code page (for FPC)
    {$endif HASCODEPAGE}
  end;
  MoveFast(add^, pointer(t)^, len);
end;

procedure _App2(var res: RawUtf8; const add1, add2: RawByteString; const cp: integer);
  {$ifdef HASINLINE} inline; {$endif}
var
  l, a, a1, a2: PtrInt;
  r: PAnsiChar;
begin
  a1 := length(add1); // no automatic UTF-8 conversion involved
  a2 := length(add2);
  a := a1 + a2;
  if a = 0 then
    exit;
  l := length(res);
  SetLength(res, l + a);
  r := pointer(res);
  {$ifdef HASCODEPAGE}
  PStrRec(r - _STRRECSIZE)^.CodePage := cp;
  {$endif HASCODEPAGE}
  MoveFast(pointer(add1)^, r[l], a1);
  MoveFast(pointer(add2)^, r[l + a1], a2);
end;

procedure Append(var Text: RawUtf8; const Added: RawByteString);
begin
  if Added <> '' then
    _App1(@Text, pointer(Added), PStrLen(PtrUInt(Added) - _STRLEN)^, CP_UTF8);
end;

procedure Append(var Text: RawUtf8; const Added1, Added2: RawByteString);
begin
  _App2(Text, Added1, Added2, CP_UTF8);
end;

procedure Append(var Text: RawUtf8; Added: AnsiChar);
var
  L: PtrInt;
begin
  L := length(Text);
  SetLength(Text, L + 1);
  PByteArray(Text)[L] := ord(Added);
end;

procedure AppendIfNone(var Text: RawUtf8; EndWith: AnsiChar);
var
  L: PtrInt;
begin
  L := length(Text);
  if (L <> 0) and
     (Text[L] = EndWith) then
    exit;
  SetLength(Text, L + 1);
  PByteArray(Text)[L] := ord(EndWith);
end;

procedure Append(var Text: RawUtf8; Added: pointer; AddedLen: PtrInt);
begin
  if (Added <> nil) and (AddedLen > 0) then
    _App1(@Text, Added, AddedLen, CP_UTF8);
end;

procedure AppendStr(var Text: RawUtf8; const Added: ShortString);
begin
  if Added[0] <> #0 then
    _App1(@Text, @Added[1], ord(Added[0]), CP_UTF8);
end;

procedure AppendUcs4(var Text: RawUtf8; ucs4: Ucs4CodePoint);
var
  tmp: array[0 .. 15] of AnsiChar;
begin
  _App1(@Text, @tmp, Ucs4ToUtf8(ucs4, @tmp), CP_UTF8);
end;

procedure Append(var Text: RawByteString; const Added: RawByteString);
begin
  if Added <> '' then
    _App1(@Text, pointer(Added), PStrLen(PtrUInt(Added) - _STRLEN)^, CP_RAWBYTESTRING);
end;

procedure Append(var Text: RawByteString; const Added1, Added2: RawByteString);
begin
  _App2(RawUtf8(Text), Added1, Added2, CP_RAWBYTESTRING);
end;

procedure Append(var Text: RawByteString; Added: pointer; AddedLen: PtrInt);
begin
  if (Added <> nil) and (AddedLen > 0) then
    _App1(@Text, Added, AddedLen, CP_RAWBYTESTRING);
end;

procedure Prepend(var Text: RawUtf8; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoPrepend(Text, @Args[0], length(Args), CP_UTF8);
end;

procedure Prepend(var Text: RawByteString; const Added: RawByteString);
var
  t, a: PtrInt;
  new: PAnsiChar;
begin
  t := length(Text);
  a := length(Added);
  if a <> 0 then
    if t = 0 then
      Text := Added
    else
    begin
      new := FastNewString(t + a);
      MoveFast(PByteArray(Text)[0], new[a], t);
      MoveFast(PByteArray(Added)[0], new[0], a);
      FastAssignNew(Text, new);
    end;
end;

procedure Prepend(var Text: RawByteString; Added: AnsiChar);
var
  L: PtrInt;
begin
  L := length(Text);
  SetLength(Text, L + 1); // is likely to avoid any ReallocMem
  MoveFast(PByteArray(Text)[0], PByteArray(Text)[1], L);
  PByteArray(Text)[0] := ord(Added);
end;

procedure PrependIfNone(var Text: RawUtf8; EndWith: AnsiChar);
var
  L: PtrInt;
begin
  L := length(Text);
  if (L <> 0) and
     (Text[1] = EndWith) then
    exit;
  SetLength(Text, L + 1); // is likely to avoid any ReallocMem
  MoveFast(PByteArray(Text)[0], PByteArray(Text)[1], L);
  PByteArray(Text)[0] := ord(EndWith);
end;

procedure Prepend(var Text: RawByteString; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoPrepend(RawUtf8(Text), @Args[0], length(Args), CP_RAWBYTESTRING);
end;

function Make(const Args: array of const): RawUtf8;
var
  f: TFormatUtf8;
  new: PUtf8Char;
begin
  if high(Args) = 0 then
  begin
    VarRecToUtf8(@Args[0], result); // could be returned e.g. by reference
    exit;
  end;
  {%H-}f.Init;
  f.AddVarRec(@Args[0], length(Args));
  if f.size <> 0 then
  begin
    new := FastNewString(f.size, CP_UTF8); // inlined FastSetString()
    f.WriteAll(new, @f.blocks);
  end
  else
    new := nil;
  FastAssignNew(result, new);
end;

procedure Make(const Args: array of const; var Result: RawUtf8;
  const IncludeLast: RawUtf8);
var
  f: TFormatUtf8;
  new: PUtf8Char;
begin
  {%H-}f.Init;
  f.AddVarRec(@Args[0], length(Args));
  if IncludeLast <> '' then
    f.AddText(IncludeLast);
  if f.size <> 0 then
  begin
    new := FastNewString(f.size, CP_UTF8); // inlined FastSetString()
    f.WriteAll(new, @f.blocks);
  end
  else
    new := nil;
  FastAssignNew(Result, new);
end;

function MakeString(const Args: array of const): string;
var
  f: TFormatUtf8;
begin
  {%H-}f.Init;
  f.AddVarRec(@Args[0], length(Args));
  f.WriteString(result);
end;

function MakePath(const Part: array of const; EndWithDelim: boolean;
  Delim: AnsiChar): TFileName;
var
  f: TFormatUtf8;
begin
  {%H-}f.InitDelim(@Part[0], length(Part), EndWithDelim, Delim);
  f.WriteString(string(result));
end;

procedure MakePath(const Part: array of const; var Dest: TFileName;
  EndWithDelim: boolean; Delim: AnsiChar);
var
  f: TFormatUtf8;
begin
  {%H-}f.InitDelim(@Part[0], length(Part), EndWithDelim, Delim);
  f.WriteString(string(Dest));
end;

function MakeExpandedPath(const Part: array of const; EndWithDelim: boolean): TFileName;
begin
  result := ExpandFileName(MakePath(Part, EndWithDelim));
end;

function EnsureDirectoryExists(const Part: array of const;
  RaiseExceptionOnCreationFailure: ExceptionClass; NoExpand: boolean): TFileName;
begin
  result := EnsureDirectoryExists(MakePath(Part),
    RaiseExceptionOnCreationFailure, NoExpand);
end;

function NormalizeDirectoryExists(const Part: array of const;
  RaiseExceptionOnCreationFailure: ExceptionClass): TFileName;
begin
  result := EnsureDirectoryExists(NormalizeFileName(MakePath(Part)),
    RaiseExceptionOnCreationFailure);
end;

function NormalizeUriToFileName(const Uri: RawUtf8; var FileName: TFileName;
  const FolderName: TFileName): boolean;
var
  fn: RawUtf8;
begin
  fn := StringReplaceChars(Uri, '/', PathDelim);
  result := SafeFileNameU(fn);
  if result then
    if FolderName = '' then
      Utf8ToFileName(fn, FileName)
    else
      MakePath([FolderName, fn], FileName);
end;

procedure NormalizeUriVar(const FileName: RawUtf8; var Uri: RawUtf8);
begin
  {$ifdef OSLINUX}
  Uri := FileName;
  {$else}
  Uri := StringReplaceChars(FileName, '\', '/');
  {$endif OSLINUX}
end;

function NormalizeUriU(const FileName: RawUtf8): RawUtf8;
begin
  NormalizeUriVar(FileName, result);
end;

procedure NormalizeUri(const FileName: TFileName; var Uri: RawUtf8);
begin
  {$ifdef UNICODE}
  {$ifdef OSWINDOWS}
  NormalizeUriVar(StringToUtf8(FileName), Uri);
  {$else}
  StringToUtf8(FileName, Uri);
  {$endif OSWINDOWS}
  {$else}
  NormalizeUriVar(FileName, Uri);
  {$endif UNICODE}
end;

function FileExistsMake(const Part: array of const;
  SetIfFound: PFileName): boolean;
var
  filename: TFileName;
begin
  MakePath(Part, filename);
  result := FileExists(filename);
  if result and
     (SetIfFound <> nil) then
    SetIfFound^ := filename;
end;

function DirectoryExistsMake(const Part: array of const;
  SetIfFound: PFileName): boolean;
var
  folder: TFileName;
begin
  MakePath(Part, folder);
  result := DirectoryExists(folder);
  if result and
     (SetIfFound <> nil) then
    SetIfFound^ := folder;
end;

function MakeFileName(const Part: array of const; LastIsExt: boolean): TFileName;
var
  f: TFormatUtf8;
  ext: RawUtf8;
  hipart: integer;
begin
  hipart := High(Part);
  if LastIsExt then
    if (hipart > 0) and
       VarRecToUtf8IsString(Part[hipart], ext) then
      dec(hipart)
    else
      ext := '';
  f.InitDelim(@Part[0], hipart + 1, false, PathDelim);
  if ext <> '' then
  begin
    if ext[1] <> '.' then
      f.AddText('.');
    f.AddText(ext);
  end;
  f.WriteString(string(result));
end;

function MakeCsv(const Value: array of const; EndWithComma: boolean;
  Comma: AnsiChar): RawUtf8;
var
  f: TFormatUtf8;
begin
  f.InitDelim(@Value[0], length(Value), EndWithComma, Comma);
  if f.size <> 0 then
    f.WriteAll(FastSetString(result, f.size), @f.blocks)
  else
    FastAssignNew(result);
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
  if not HasConsole then
    exit;
  FormatUtf8(Fmt, Args, tmp);
  ConsoleWrite(tmp, Color, NoLineFeed);
end;

procedure ConsoleWrite(const Args: array of const;
  Color: TConsoleColor; NoLineFeed: boolean);
var
  tmp: RawUtf8;
begin
  if not HasConsole then
    exit;
  Make(Args, tmp);
  ConsoleWrite(tmp, Color, NoLineFeed);
end;

procedure ConsoleWriteRaw(const Args: array of const; NoLineFeed: boolean);
var
  tmp: RawUtf8;
begin
  if not HasConsole then
    exit;
  Make(Args, tmp);
  ConsoleWrite(tmp, ccDefault, NoLineFeed, {nocolor=}true);
end;

procedure ConsoleShowFatalException(E: Exception; WaitForEnterKey: boolean);
begin
  if not HasConsole then
    exit;
  ConsoleWrite(CRLF + 'Fatal exception ', ccLightRed, {nolinefeed=}true);
  ConsoleWrite('%', [E], ccWhite, true);
  ConsoleWrite(' raised with message ', ccLightRed);
  ConsoleWrite('  %', [E.Message], ccLightMagenta);
  if not WaitForEnterKey then
    exit;
  ConsoleWriteRaw(CRLF + 'Program will now abort');
  {$ifndef OSPOSIX}
  ConsoleWriteRaw('Press [Enter] to quit');
  ConsoleWaitForEnterKey;
  {$endif OSPOSIX}
end;


{ ************ ESynException class }

{ ESynException }

procedure ESynException.CreateAfterSetMessageUtf8;
begin
  inherited Create(Utf8ToString(fMessageUtf8));
end;

constructor ESynException.CreateUtf8(const Format: RawUtf8;
  const Args: array of const);
begin
  FormatUtf8(Format, Args, fMessageUtf8);
  CreateAfterSetMessageUtf8;
end;

constructor ESynException.CreateU(const Msg: RawUtf8);
begin
  fMessageUtf8 := Msg;
  CreateAfterSetMessageUtf8;
end;

class procedure ESynException.RaiseLastOSError(const Format: RawUtf8;
  const Args: array of const; const Trailer: ShortString);
var
  error: integer;
  fmt: RawUtf8;
begin
  error := GetLastError;
  FormatUtf8('% 0x% [%] %', [Trailer, CardinalToHexShort(error),
    StringReplaceAll(GetErrorText(error), '%', '#'), Format], fmt);
  raise CreateUtf8(fmt, Args)
  {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame)
  {$else} at ReturnAddress {$endif}
end;

class procedure ESynException.RaiseUtf8(const Format: RawUtf8;
  const Args: array of const);
begin
  raise CreateUtf8(Format, Args)
  {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame)
  {$else} at ReturnAddress {$endif}
end;

class procedure ESynException.RaiseU(const Msg: RawUtf8);
begin
  raise CreateU(Msg)
  {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame)
  {$else} at ReturnAddress {$endif}
end;

{$ifndef NOEXCEPTIONINTERCEPT}

function DefaultSynLogExceptionToStr(WR: TTextWriter;
  const Context: TSynLogExceptionContext; WithAdditionalInfo: boolean): boolean;
{$ifdef OSWINDOWS} // no TSynLogExceptionContext.AdditionalInfo() on POSIX
var
  s: ShortString;
{$endif OSWINDOWS}
begin
  WR.AddClassName(Context.EClass);
  if (Context.ELevel = sllException) and
     (Context.EInstance <> nil) and
     (Context.EClass <> EExternalException) then
  begin
    {$ifdef OSWINDOWS}
    if WithAdditionalInfo and
       Context.AdditionalInfo(s) then
      WR.AddShort(s); // e.g. ' [.NET/CLR unhandled StackOverflowException]'
    {$endif OSWINDOWS}
    WR.AddDirect(' ');
    if PClass(WR)^ = TTextWriter then // no WriteObject() yet
      WR.AddOnSameLineString(Context.EInstance.Message)
    else
      WR.WriteObject(Context.EInstance); // use RTTI for JSON serialization
  end
  else if Context.ECode <> 0 then
  begin
    WR.AddDirect(' ', '(');
    {$ifdef OSWINDOWS}
    WinErrorShortVar(PtrUInt(Context.ECode), s); // decode most known error codes
    WR.AddShort(s);
    {$else}
    WR.AddPointer(Context.ECode);
    {$endif OSWINDOWS}
    WR.AddDirect(')');
  end;
  result := false; // caller should append "at EAddr" and the stack trace
end;

function ESynException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  if Assigned(TSynLogExceptionToStrCustom) then
    result := TSynLogExceptionToStrCustom(WR, Context)
  else
    result := DefaultSynLogExceptionToStr(WR, Context, {addinfo=}true);
end;

{$endif NOEXCEPTIONINTERCEPT}

procedure ExceptionUtf8(E: Exception; var Message: RawUtf8);
begin
  if E.InheritsFrom(ESynException) and
     (ESynException(E).MessageUtf8 <> '') then
    Message := ESynException(E).MessageUtf8 // no conversion needed
  else
    StringToUtf8(E.Message, Message);
end;


{ **************** HTTP/REST Common Headers Parsing (e.g. cookies) }

const
  // sorted by occurrence for in-order O(n) search via IntegerScanIndex()
  METHODNAME: array[TUriMethod] of PUtf8Char = (
    'GET',     // no mNone entry for efficient IntegerScanIndex()
    'POST',
    'PUT',
    'DELETE',
    'HEAD',
    'BEGIN',
    'END',
    'ABORT',
    'LOCK',
    'UNLOCK',
    'STATE',
    'PATCH',
    'OPTIONS',
    '');
var
  // quick O(n) search of the first 4 characters within L1 cache (56 bytes)
  METHODNAME32: array[TUriMethod] of cardinal;

function ToMethod(const method: RawUtf8): TUriMethod;
begin
  case length(method) of
    3 .. 7:
      result := TUriMethod(IntegerScanIndex(@METHODNAME32, // may use SSE2
        length(METHODNAME32) - 1, (PCardinal(method)^) and $dfdfdfdf) + 1);
  else
    result := mNone;
  end;
end;

function ToText(m: TUriMethod): PUtf8Char;
begin
  dec(m); // METHODNAME[] has no mNone entry
  if cardinal(m) < cardinal(ord(high(METHODNAME))) then
    result := METHODNAME[m]
  else
    result := nil;
end;

const
  // last item is fake HTTP status 513 = 'Invalid Request'
  INDEX_HTTP_INVALID = 47;
  // sorted by actual usage order for WordScanIndex() in matching HTTP_CODE[]
  HTTP_REASON: array[0 .. INDEX_HTTP_INVALID] of RawUtf8 = (
   'OK',                                // HTTP_SUCCESS - should be first
   'No Content',                        // HTTP_NOCONTENT
   'Temporary Redirect',                // HTTP_TEMPORARYREDIRECT
   'Permanent Redirect',                // HTTP_PERMANENTREDIRECT
   'Moved Permanently',                 // HTTP_MOVEDPERMANENTLY
   'Bad Request',                       // HTTP_BADREQUEST
   'Unauthorized',                      // HTTP_UNAUTHORIZED
   'Forbidden',                         // HTTP_FORBIDDEN
   'Not Found',                         // HTTP_NOTFOUND
   'Method Not Allowed',                // HTTP_NOTALLOWED
   'Not Modified',                      // HTTP_NOTMODIFIED
   'Not Acceptable',                    // HTTP_NOTACCEPTABLE
   'Partial Content',                   // HTTP_PARTIALCONTENT
   'Payload Too Large',                 // HTTP_PAYLOADTOOLARGE
   'Created',                           // HTTP_CREATED
   'See Other',                         // HTTP_SEEOTHER
   'Continue',                          // HTTP_CONTINUE
   'Switching Protocols',               // HTTP_SWITCHINGPROTOCOLS
   'Accepted',                          // HTTP_ACCEPTED
   'Non-Authoritative Information',     // HTTP_NONAUTHORIZEDINFO
   'Reset Content',                     // HTTP_RESETCONTENT
   'Multi-Status',                      // 207
   'Multiple Choices',                  // HTTP_MULTIPLECHOICES
   'Found',                             // HTTP_FOUND
   'Use Proxy',                         // HTTP_USEPROXY
   'Proxy Authentication Required',     // HTTP_PROXYAUTHREQUIRED
   'Request Timeout',                   // HTTP_TIMEOUT
   'Conflict',                          // HTTP_CONFLICT
   'Gone',                              // HTTP_GONE
   'Length Required',                   // HTTP_LENGTHREQUIRED
   'Precondition Failed',               // 412
   'URI Too Long',                      // 414
   'Unsupported Media Type',            // HTTP_UNSUPPORTEDMEDIATYPE
   'Requested Range Not Satisfiable',   // HTTP_RANGENOTSATISFIABLE
   'I''m a teapot',                     // HTTP_TEAPOT
   'Unprocessable Content',             // HTTP_UNPROCESSABLE_CONTENT
   'Upgrade Required',                  // HTTP_UPGRADE_REQUIRED
   'Too Many Requests',                 // HTTP_TOO_MANY_REQUESTS
   'Internal Server Error',             // HTTP_SERVERERROR
   'Not Implemented',                   // HTTP_NOTIMPLEMENTED
   'Bad Gateway',                       // HTTP_BADGATEWAY
   'Service Unavailable',               // HTTP_UNAVAILABLE
   'Gateway Timeout',                   // HTTP_GATEWAYTIMEOUT
   'HTTP Version Not Supported',        // HTTP_HTTPVERSIONNONSUPPORTED
   'Insufficient Storage',              // HTTP_INSUFFICIENTSTORAGE
   'Network Authentication Required',   // 511
   'Client Side Connection Error',      // HTTP_CLIENTERROR = 666
   'Invalid Request');                  // last INDEX_HTTP_INVALID = 513
  HTTP_CODE: array[0 .. INDEX_HTTP_INVALID] of word = ( // match HTTP_REASON[]
    HTTP_SUCCESS,
    HTTP_NOCONTENT,
    HTTP_TEMPORARYREDIRECT,
    HTTP_PERMANENTREDIRECT,
    HTTP_MOVEDPERMANENTLY,
    HTTP_BADREQUEST,
    HTTP_UNAUTHORIZED,
    HTTP_FORBIDDEN,
    HTTP_NOTFOUND,
    HTTP_NOTALLOWED,
    HTTP_NOTMODIFIED,
    HTTP_NOTACCEPTABLE,
    HTTP_PARTIALCONTENT,
    HTTP_PAYLOADTOOLARGE,
    HTTP_CREATED,
    HTTP_SEEOTHER,
    HTTP_CONTINUE,
    HTTP_SWITCHINGPROTOCOLS,
    HTTP_ACCEPTED,
    HTTP_NONAUTHORIZEDINFO,
    HTTP_RESETCONTENT,
    207,
    HTTP_MULTIPLECHOICES,
    HTTP_FOUND,
    HTTP_USEPROXY,
    HTTP_PROXYAUTHREQUIRED,
    HTTP_TIMEOUT,
    HTTP_CONFLICT,
    HTTP_GONE,
    HTTP_LENGTHREQUIRED,
    412,
    414,
    HTTP_UNSUPPORTEDMEDIATYPE,
    HTTP_RANGENOTSATISFIABLE,
    HTTP_TEAPOT,
    HTTP_UNPROCESSABLE_CONTENT,
    HTTP_UPGRADE_REQUIRED,
    HTTP_TOO_MANY_REQUESTS,
    HTTP_SERVERERROR,
    HTTP_NOTIMPLEMENTED,
    HTTP_BADGATEWAY,
    HTTP_UNAVAILABLE,
    HTTP_GATEWAYTIMEOUT,
    HTTP_HTTPVERSIONNONSUPPORTED,
    HTTP_INSUFFICIENTSTORAGE,
    511,
    HTTP_CLIENTERROR,
    513); // last INDEX_HTTP_INVALID = fake 'Invalid Request' fallback code

function StatusCodeToText(Code: cardinal): PRawUtf8;
var
  i: PtrInt;
begin
  if Code <> 200 then // optimistic approach :)
    if (Code <= HTTP_CLIENTERROR) and  // 100..666
       (Code >= 100) then
    begin
      i := WordScanIndex(@HTTP_CODE, length(HTTP_CODE), Code); // may use SSE2
      if i < 0 then
        i := INDEX_HTTP_INVALID; // returns cached 513 'Invalid Request'
    end
    else
      i := INDEX_HTTP_INVALID
  else
    i := 0;
  result := @HTTP_REASON[i];
end;

procedure StatusCodeToReason(Code: cardinal; var Reason: RawUtf8);
begin
  Reason := StatusCodeToText(Code)^;
end;

function StatusCodeToShort(Code: cardinal): TShort47;
begin
  result[0] := #0;
  if Code <> HTTP_CLIENTERROR then // hide the number of the beast
  begin
    if Code > 999 then
      Code := 999; // ensure stay in TShort47 within standard HTTP 3-digits range
    AppendShortCardinal(Code, result);
    AppendShortChar(' ', @result);
  end;
  AppendShortAnsi7String(StatusCodeToText(Code)^, result);
end;

function StatusCodeToErrorMsg(Code: integer): RawUtf8;
begin
  FormatUtf8('HTTP Error % - %', [Code, StatusCodeToText(Code)^], result);
end;

function StatusCodeIsSuccess(Code: integer): boolean;
begin
  result := (Code >= HTTP_SUCCESS) and
            (Code < HTTP_BADREQUEST); // 200..399
end;

function IsInvalidHttpHeader(const Headers: RawUtf8): boolean;
var
  p, pend: PUtf8Char;
  l: PtrInt;
begin
  result := false;
  p := pointer(Headers);
  if p = nil then
    exit;
  pend := p + PStrLen(p - _STRLEN)^;
  repeat
    if p^ = #0 then
      exit; // clean ending
    l := BufferLineLength(p, pend); // use SSE2 on x86_64
    if l = 0 then
      break; // void line is only for the end of headers
    inc(p, l);
    if cardinal(PWord(p)^) = EOLW then
      inc(p, 2)
    else if p^ = #0 then
      exit // allow ending without any CRLF
    else
      break;
  until false;
  result := true;
end;

function IsHttp(const text: RawUtf8): boolean;
begin
  result := (length(text) > 5) and
            (PCardinal(text)^ and $dfdfdfdf = HTTP_32) and
            ((text[5] = ':') or
             ((text[5] in ['s', 'S']) and
              (text[6] = ':')));
end;

function IsLdap(const text: RawUtf8): boolean;
begin
  result := (length(text) > 5) and
            (PCardinal(text)^ and $dfdfdfdf = LDAP_32) and
            ((text[5] = ':') or
             ((text[5] in ['s', 'S']) and
              (text[6] = ':')));
end;


{ THttpCookies }

procedure THttpCookies.Clear;
begin
  fCookies := nil;
end;

procedure THttpCookies.ParseServer(Head: PUtf8Char);
var // https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Cookie
  count, plen, total: PtrInt;
  p: PUtf8Char;
  new: THttpCookie;
begin
  fCookies := nil; // first Clear any previous cookie
  count := 0;
  total := 0;
  while Head <> nil do
  begin
    // find all 'Cookie: name=value; name2=value2; name3=value3' lines
    p := FindNameValuePointer(Head, 'COOKIE:', plen);
    if p = nil then
      break;
    inc(total, plen);
    if total > COOKIE_MAXSIZE_DOSATTACK then
      ESynException.RaiseUtf8('RetrieveCookies got % cookies (>%)',
        [KB(total), KB(COOKIE_MAXSIZE_DOSATTACK)]);
    Head := GotoNextLine(p + plen);
    // parse each line pairs
    repeat
      if IdemPChar(p, '__SECURE-') then
        inc(p, 9); // e.g. if rsoCookieSecure is in Server.Options
      new.NameLen := GetNextItemTrimedLineBuffer(p, '=', new.NameStart);
      new.ValueLen := GetNextItemTrimedLineBuffer(p, ';', new.ValueStart);
      if (new.NameLen = 0) or
         (new.ValueLen = 0) then
        continue;
      if count >= COOKIE_MAXCOUNT_DOSATTACK then
        ESynException.RaiseU('RetrieveCookies overflow: DOS attempt?');
      if count = length(fCookies) then
        SetLength(fCookies, NextGrow(count));
      fCookies[count] := new;
      inc(count);
    until (p = nil) or
          (p^ < ' '); // next 'name2=value2; ...' pair
  end;
  if count <> 0 then
    DynArrayFakeLength(fCookies, count);
end;

function THttpCookies.FindCookie(const CookieName: RawUtf8): PHttpCookie;
begin
  result := nil;
  if @self <> nil then
    result := pointer(FindTextBufferPair(
                pointer(CookieName), length(CookieName), pointer(fCookies)));
end;

function THttpCookies.GetCookie(const CookieName: RawUtf8): RawUtf8;
begin
  RetrieveCookie(CookieName, result);
end;

procedure THttpCookies.RetrieveCookie(const CookieName: RawUtf8;
  var DestValue: RawUtf8);
var
  c: PHttpCookie;
begin
  c := FindCookie(CookieName);
  if c <> nil then
    FastSetString(DestValue, c^.ValueStart, c^.ValueLen)
  else
    FastAssignNew(DestValue);
end;

function CookieFromHeaders(Headers: PUtf8Char; const Name: RawUtf8;
  out Value: PUtf8Char): PtrInt;
var
  p, n: PUtf8Char;
  plen, l: PtrInt;
begin // same logic than THttpCookies.ParseServer above
  if Name <> '' then
    while Headers <> nil do
    begin
      p := FindNameValuePointer(Headers, 'COOKIE:', plen);
      if p = nil then
        break;
      Headers := GotoNextLine(p + plen);
      repeat
        if IdemPChar(p, '__SECURE-') then
          inc(p, 9);
        l := GetNextItemTrimedLineBuffer(p, '=', n);
        result := GetNextItemTrimedLineBuffer(p, ';', Value);
        if (l = length(Name)) and
           (result <> 0) and
           mormot.core.base.CompareMem(n, pointer(Name), l) then
          exit; // found the cookie and return its value
      until (p = nil) or
            (p^ < ' ');
    end;
  result := 0;
end;

function CookieFromHeaders(Headers: PUtf8Char; const Name: RawUtf8): RawUtf8;
var
  l: integer;
  v: PUtf8Char;
begin
  l := CookieFromHeaders(Headers, Name, v);
  FastSetString(result, v, l);
end;


{ **************** Hexadecimal Text And Binary Conversion }

procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexW;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
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
  mormot.core.text.BinToHex(pointer(Bin), FastSetString(result, L * 2), L);
end;

function BinToHex(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  mormot.core.text.BinToHex(Bin, FastSetString(result, BinBytes * 2), BinBytes);
end;

function HexToBin(Hex: PAnsiChar; HexLen: PtrInt;
  var Bin: RawByteString): boolean;
begin
  Bin := '';
  if (Hex = nil) or
     (HexLen and 1 <> 0) then
  begin
    result := false;
    exit; // hexadecimal should be not void, and in char pairs
  end;
  HexLen := HexLen shr 1;
  pointer(Bin) := FastNewString(HexLen);
  result := mormot.core.text.HexToBin(Hex, pointer(Bin), HexLen);
  if not result then
    Bin := '';
end;

function HexToBin(const Hex: RawUtf8): RawByteString;
begin
  HexToBin(pointer(Hex), length(Hex), result);
end;

function HexaToByte(P: PUtf8Char; var Dest: byte): boolean;
  {$ifdef HASINLINE}inline;{$endif}
var
  b, c: byte;
begin
  b := ConvertHexToShl[P[0]];
  if b <> 255 then
  begin
    c := ConvertHexToBin[P[1]];
    if c <> 255 then
    begin
      inc(b, c);
      Dest := b;
      result := true;
      exit;
    end;
  end;
  result := false; // mark error
end;

function HumanHexToBin(const hex: RawUtf8; var Bin: RawByteString; CP: cardinal): boolean;
var
  len: PtrInt;
  h, p: PAnsiChar;
begin
  Bin := '';
  result := false;
  len := length(hex);
  if len = 0 then
    exit;
  p := FastNewString(len shr 1, CP); // shr 1 = maximum length
  pointer(Bin) := p;
  h := pointer(hex);
  repeat
    while h^ = ' ' do
      inc(h);
    if not HexaToByte(pointer(h), PByte(p)^) then
      break; // invalid 'xx' pair - may be len < 2
    inc(p);
    inc(h, 2);
    dec(len, 2);
    if len = 0 then
    begin
      result := true; // properly ended with 'xx' last hexa byte
      break;
    end;
    while h^ = ' ' do
      inc(h);
    if h^ <> ':' then
      continue;
    dec(len);
    if len = 0 then
      break; // should not end with ':'
    inc(h);
  until false;
  if result then
    FakeLength(Bin, p - pointer(Bin))
  else
    Bin := '';
end;

function HumanHexCompare(a, b: PUtf8Char): integer;
var
  ca, cb: byte;
begin
  result := 0;
  if a <> b then
    if a <> nil then
      if b <> nil then
      begin
        repeat
          while a^ = ' ' do
            inc(a);
          while b^ = ' ' do
            inc(b);
          if not HexaToByte(pointer(a), ca{%H-}) or
             not HexaToByte(pointer(b), cb{%H-}) then
          begin
            result := ComparePointer(a, b); // consistent but not zero
            break;
          end;
          result := ca - cb;
          if result <> 0 then
            break;
          inc(a, 2);
          inc(b, 2);
          while a^ = ' ' do
            inc(a);
          while b^ = ' ' do
            inc(b);
          case a^ of
            #0:
              begin
                if b^ <> #0 then
                  dec(result);
                break;
              end;
            ':':
              inc(a);
          end;
          case b^ of
            #0:
              begin
                inc(result); // we know a^<>#0
                break;
              end;
            ':':
              inc(b);
          end;
        until false;
      end
      else
        inc(result)
    else
      dec(result);
end;

function HumanHexCompare(const a, b: RawUtf8): integer;
begin
  result := HumanHexCompare(pointer(a), pointer(b));
end;

function HumanHexToBin(const hex: RawUtf8): RawByteString;
begin
  HumanHexToBin(hex, result);
end;

function HexToUtf8(const hex: RawUtf8): RawUtf8;
begin
  HumanHexToBin(hex, RawByteString(result), CP_UTF8);
end;

function ByteToHex(P: PAnsiChar; Value: byte): PAnsiChar;
begin
  PWord(P)^ := TwoDigitsHex[Value];
  result := P + 2;
end;

function BinToHexDisplay(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  BinToHexDisplay(Bin, FastSetString(result, BinBytes * 2), BinBytes);
end;

procedure BinToHexLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexWLower;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
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
  BinToHexLower(Bin, FastSetString(result, BinBytes * 2), BinBytes);
end;

function BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  BinToHexLower(Bin, BinBytes, result);
end;

procedure BinToHexLowerSelf(var Bin: RawByteString);
var
  hexa: RawUtf8;
begin
  FastSetString(hexa, length(Bin) * 2);
  BinToHexLower(pointer(Bin), length(Bin), hexa);
  Bin := hexa;
end;

function BinToHexDisplayLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  BinToHexDisplayLower(Bin, FastSetString(result, BinBytes * 2), BinBytes);
end;

function BinToHexDisplayLowerShort(Bin: PAnsiChar; BinBytes: PtrInt): ShortString;
begin
  if BinBytes > 127 then
    BinBytes := 127;
  result[0] := AnsiChar(BinBytes * 2);
  BinToHexDisplayLower(Bin, @result[1], BinBytes);
end;

function {%H-}BinToHexDisplayLowerShort16(Bin: Int64; BinBytes: PtrInt): TShort16;
begin
  if BinBytes > 8 then
    BinBytes := 8;
  result[0] := AnsiChar(BinBytes * 2);
  BinToHexDisplayLower(@Bin, @result[1], BinBytes);
end;

procedure BinBitsToHexDisplayLowerShort16(Bin: Int64; BinBits: PtrInt;
  var Result: TShort16);
begin
  Result[0] := AnsiChar(BitsToBytes(BinBits) * 2);
  if Result[0] > #16 then
    Result[0] := #16;
  BinToHexDisplayLower(@Bin, @Result[1], ord(Result[0]) shr 1);
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

function TrimMinDisplayHex(Text: PUtf8Char; TextLen: PtrInt): PtrInt;
begin
  while (TextLen <> 0) and
        (Text[TextLen - 1] = '0') do
    dec(TextLen);
  result := TextLen;
end;

procedure PointerToHex(aPointer: pointer; var result: RawUtf8);
begin
  BinToHexDisplay(@aPointer,
    FastSetString(result, SizeOf(pointer) * 2), SizeOf(pointer));
end;

function PointerToHex(aPointer: pointer): RawUtf8;
begin
  PointerToHex(aPointer, result);
end;

function CardinalToHex(aCardinal: cardinal): RawUtf8;
begin
  BinToHexDisplay(@aCardinal,
    FastSetString(result, SizeOf(aCardinal) * 2), SizeOf(aCardinal));
end;

function CardinalToHexLower(aCardinal: cardinal): RawUtf8;
begin
  BinToHexDisplayLower(@aCardinal,
    FastSetString(result, SizeOf(aCardinal) * 2), SizeOf(aCardinal));
end;

function Int64ToHex(aInt64: Int64): RawUtf8;
begin
  BinToHexDisplay(@aInt64,
    FastSetString(result, SizeOf(Int64) * 2), SizeOf(Int64));
end;

procedure Int64ToHex(aInt64: Int64; var result: RawUtf8);
begin
  BinToHexDisplay(@aInt64,
    FastSetString(result, SizeOf(Int64) * 2), SizeOf(Int64));
end;

function PointerToHexShort(aPointer: pointer): TShort16;
begin
  result[0] := AnsiChar(DisplayMinChars(@aPointer, SizeOf(aPointer)) * 2);
  BinToHexDisplayLower(@aPointer, @result[1], ord(result[0]) shr 1);
end;

function CardinalToHexShort(aCardinal: cardinal): TShort15;
begin
  result[0] := AnsiChar(SizeOf(aCardinal) * 2);
  BinToHexDisplay(@aCardinal, @result[1], SizeOf(aCardinal));
end;

function crc32cUtf8ToHex(const str: RawUtf8): RawUtf8;
begin
  result := CardinalToHex(crc32c(0, pointer(str), length(str)));
end;

function crc32cString(const str: string): cardinal;
var
  temp: TSynTempBuffer;
  p: pointer; // in explicit two steps for safety
begin
  p := StringToUtf8Temp(str, temp); // returns str if already ASCII-7 or UTF-8
  result := crc32c(0, p, temp.len);
  temp.Done;
end;

function crc32cStringToHexShort(const str: string): TShort15;
begin
  result := CardinalToHexShort(crc32cString(str));
end;

function ToHexShort(P: pointer; Len: PtrInt): TShort64;
begin
  if Len = 0 then
  begin
    result[0] := AnsiChar(Len);
    exit;
  end;
  if Len > 32 then
    Len := 32;
  Len := DisplayMinChars(P, Len);
  result[0] := AnsiChar(Len * 2);
  BinToHexDisplay(P, @result[1], Len);
end;

function Int64ToHexLower(aInt64: Int64): RawUtf8;
var
  L: PtrInt;
begin
  L := DisplayMinChars(@aInt64, SizeOf(Int64));
  BinToHexDisplayLower(@aInt64, FastSetString(result, L * 2), L);
end;

procedure Int64ToHexShort(aInt64: Int64; out result: TShort16);
begin
  result[0] := AnsiChar(SizeOf(aInt64) * 2);
  BinToHexDisplay(@aInt64, @result[1], SizeOf(aInt64));
end;

function Int64ToHexShort(aInt64: Int64): TShort16;
begin
  Int64ToHexShort(aInt64, result);
end;

function Int64ToHexString(aInt64: Int64): string;
var
  temp: TShort16;
begin
  Int64ToHexShort(aInt64, temp);
  Ansi7ToString(@temp[1], ord(temp[0]), result);
end;

{$ifdef CPUX86NOTPIC}
function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;
var
  b, c: byte;
begin
  result := false; // return false if any invalid char
  if (Hex = nil) or
     (Bin = nil) then
    exit;
  inc(Bin, BinBytes - 1); // display = reverse order
  if BinBytes > 0 then
    repeat
      c := ConvertHexToShl[Hex[0]];
      if c = 255 then
        exit;
      b := ConvertHexToBin[Hex[1]];
      if b = 255 then
        exit;
      Bin^ := b + c;
      dec(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
  result := true;
end;

function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
begin
  result := HexDisplayToBin(Hex, @aValue, 4);
  if not result then
    aValue := 0;
end;
{$else}
function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;
var
  b, c: byte;
  tab: PAnsiCharToByte; // higher number of registers x86_64 and arm/aarch64
begin
  result := false;
  if (Hex = nil) or
     (Bin = nil) then
    exit;
  tab := @ConvertHexToBin;
  inc(Bin, BinBytes - 1); // display = reverse order
  if BinBytes > 0 then
    repeat
      c := tab[Hex[0]];
      if c = 255 then
        exit;
      c := c shl 4;
      b := tab[Hex[1]];
      if b = 255 then
        exit;
      inc(b, c);
      Bin^ := b;
      dec(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
  result := true;
end;

function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
var
  v, b, err: cardinal;
  tab: PAnsiCharToByte;
begin // unrolled version for x86_64 and arm/aarch64 - used mainly in REST auth
  aValue := 0;
  result := false;
  tab := @ConvertHexToBin;
  err := 255;
  v := tab[Hex[0]];
  if v = err then
    exit;
  b := tab[Hex[1]];
  v := v shl 4;
  if b = err then
    exit;
  inc(v, b);
  b := tab[Hex[2]];
  v := v shl 4;
  if b = err then
    exit;
  inc(v, b);
  b := tab[Hex[3]];
  v := v shl 4;
  if b = err then
    exit;
  inc(v, b);
  b := tab[Hex[4]];
  v := v shl 4;
  if b = err then
    exit;
  inc(v, b);
  b := tab[Hex[5]];
  v := v shl 4;
  if b = err then
    exit;
  inc(v, b);
  b := tab[Hex[6]];
  v := v shl 4;
  if b = err then
    exit;
  inc(v, b);
  b := tab[Hex[7]];
  v := v shl 4;
  if b = err then
    exit;
  inc(v, b);
  aValue := v;
  result := true;
end;
{$endif CPUX86NOTPIC}

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
  tab: PByteArray;
begin
  result := false; // return false if any invalid char
  if Hex = nil then
    exit;
  if BinBytes > 0 then
    if Bin <> nil then
      repeat
        b := ConvertHexToShl[Hex[0]];
        if b = 255 then
          exit;
        c := ConvertHexToBin[Hex[1]];
        if c = 255 then
          exit;
        inc(Hex, 2);
        Bin^ := b or c;
        inc(Bin);
        dec(BinBytes);
      until BinBytes = 0
    else // Bin=nil -> validate Hex^ input
    begin
      tab := @ConvertHexToBin;
      repeat
        if (tab[Ord(Hex[0])] > 15) or
           (tab[Ord(Hex[1])] > 15) then
          exit;
        inc(Hex, 2);
        dec(BinBytes);
      until BinBytes = 0;
    end;
  result := true; // conversion OK
end;

procedure HexToBinFast(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt);
var
  c: byte;
begin
  if BinBytes > 0 then
    repeat
      c := ConvertHexToShl[Hex[0]];
      c := ConvertHexToBin[Hex[1]] or c;
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
  result := (ConvertHexToBin[Hex[0]] <= 15) and
            (ConvertHexToBin[Hex[1]] <= 15);
end;

function HexToCharValid(Hex: PAnsiChar; HexToBin: PByteArray): boolean;
begin
  result := (HexToBin[Ord(Hex[0])] <= 15) and
            (HexToBin[Ord(Hex[1])] <= 15);
end;

function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char; HexToBin: PByteArray): boolean;
var
  b, c: byte;
begin
  if Hex <> nil then
  begin
    b := HexToBin[ord(Hex[0]) + 256]; // + 256 for shl 4
    c := HexToBin[ord(Hex[1])];
    if (b <> 255) and
       (c <> 255) then
    begin
      if Bin <> nil then
      begin
        inc(c, b);
        Bin^ := AnsiChar(c);
      end;
      result := true;
      exit;
    end;
  end;
  result := false; // return false if any invalid char
end;

function HexToWideChar(Hex: PUtf8Char): cardinal;
var
  B: cardinal;
  tab: PAnsiCharToByte;
begin
  tab := @ConvertHexToBin;
  result := tab[Hex[0]];
  if result <= 15 then
  begin
    result := result shl 12;
    B := tab[Hex[1]];
    if B <= 15 then
    begin
      B := B shl 8;
      inc(result, B);
      B := tab[Hex[2]];
      if B <= 15 then
      begin
        B := B shl 4;
        inc(result, B);
        B := tab[Hex[3]];
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
        // stop at malformed input (includes #0)
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
  L: PtrInt;
begin
  tmp.Init(length(Oct));
  try
    L := OctToBin(pointer(Oct), tmp.buf);
    FastSetRawByteString(result, tmp.buf, L);
  finally
    tmp.Done;
  end;
end;

function GuidToText(P: PUtf8Char; guid: PByteArray; tab: PWordArray): PUtf8Char;
var
  i: PtrInt;
begin
  // encode as '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
  if tab = nil then
    tab := @TwoDigitsHex; // uppercased hexa by default (as for GUID)
  for i := 3 downto 0 do
  begin
    PWord(P)^ := tab[guid[i]];
    inc(P, 2);
  end;
  inc(PByte(guid), 4);
  for i := 1 to 2 do
  begin
    P[0] := '-';
    PWord(P + 1)^ := tab[guid[1]];
    PWord(P + 3)^ := tab[guid[0]];
    inc(PByte(guid), 2);
    inc(P, 5);
  end;
  P[0] := '-';
  PWord(P + 1)^ := tab[guid[0]];
  PWord(P + 3)^ := tab[guid[1]];
  P[5] := '-';
  inc(PByte(guid), 2);
  inc(P, 6);
  for i := 0 to 5 do
  begin
    PWord(P)^ := tab[guid[i]];
    inc(P, 2);
  end;
  result := P;
end;

function GuidToRawUtf8({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): RawUtf8;
var
  P: PUtf8Char;
begin
  P := FastSetString(result, 38);
  P^ := '{';
  GuidToText(P + 1, @guid)^ := '}';
end;

function ToUtf8({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): RawUtf8;
begin
  ToUtf8(guid, result);
end;

function NotNullGuidToUtf8({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): RawUtf8;
begin
  FastAssignNew(result);
  if not IsNullGuid(guid) then
    ToUtf8(guid, result);
end;

procedure ToUtf8({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} guid: TGuid;
  var text: RawUtf8; tab: PWordArray);
begin
  GuidToText(FastSetString(text, 36), @guid, tab);
end;

function GuidArrayToCsv(const guid: array of TGuid; SepChar: AnsiChar;
  tab: PWordArray): RawUtf8;
var
  n: integer;
  g: PGuid;
  P: PUtf8Char;
begin
  FastAssignNew(result);
  n := length(guid);
  if n = 0 then
    exit;
  p := FastSetString(result, (37 * n) - 1);
  g := @guid[0];
  repeat
    GuidToText(p, pointer(g), tab);
    dec(n);
    if n = 0 then
      exit;
    inc(p, 36);
    p^ := SepChar;
    inc(p);
    inc(g);
  until false;
end;

function GuidToShort({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): TShortGuid;
begin
  GuidToShort(Guid, result);
end;

procedure GuidToShort({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid; out dest: TShortGuid);
begin
  dest[0] := #38;
  dest[1] := '{';
  GuidToText(@dest[2], @guid)^ := '}';
end;

function UuidToShort({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): TShortGuid;
begin
  result[0] := #36;
  GuidToText(@result[1], @guid, @TwoDigitsHexLower);
end;

{$ifdef UNICODE}
function GuidToString({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  guid: TGuid): string;
var
  tmp: TShortGuid;
begin
  GuidToShort(guid, tmp);
  Ansi7ToString(@tmp[1], 38, result);
end;
{$else}
function GuidToString(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} guid: TGuid): string;
begin
  result := GuidToRawUtf8(guid);
end;
{$endif UNICODE}

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
    if P^ = '-' then // '-' separators are optional
      inc(P);
    if not HexaToByte(P, guid[1]) or
       not HexaToByte(P + 2, guid[0]) then
      exit;
    inc(P, 4);
    inc(PByte(guid), 2);
  end;
  if P^ = '-' then
    inc(P);
  if not HexaToByte(P, guid[0]) or // in reverse order than the previous loop
     not HexaToByte(P + 2, guid[1]) then
    exit;
  inc(P, 4);
  inc(PByte(guid), 2);
  if P^ = '-' then
    inc(P);
  for i := 0 to 5 do
    if HexaToByte(P, guid[i]) then
      inc(P, 2)
    else
      exit;
  result := P;
end;

function StringToGuid(const text: string): TGuid;
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

function RawUtf8ToGuid(const text: RawByteString): TGuid;
begin
  if not RawUtf8ToGuid(text, result) then
    FillZero(PHash128(@result)^);
end;

function RawUtf8ToGuid(const text: RawByteString; out guid: TGuid): boolean;
begin
  result := RawUtf8ToGuid(pointer(text), length(text), guid);
end;

function RawUtf8ToGuid(text: PUtf8Char; textlen: PtrInt; out guid: TGuid): boolean;
begin
  result := true;
  case textlen of
    32, // '3F2504E04F8911D39A0C0305E82C3301' TextToGuid() order, not HexToBin()
    36: // '3F2504E0-4F89-11D3-9A0C-0305E82C3301' JSON compatible layout
      if TextToGuid(text, @guid) <> nil then
        exit;
    38: // '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' regular layout
      if (text[0] = '{') and
         (text[37] = '}') and
         (TextToGuid(@text[1], @guid) <> nil) then
        exit;
  end;
  result := false;
end;

function TrimGuid(var text: RawUtf8): boolean;
var
  s, d: PUtf8Char;
  L: PtrInt;
  c: AnsiChar;
begin
  s := UniqueRawUtf8(text);
  if s = nil then
  begin
    result := false;
    exit;
  end;
  result := true;
  d := s;
  repeat
    c := s^;
    inc(s);
    case c of
      #0:
        break;
      #1..' ', '-', '{', '}': // trim spaces and GUID/UUID separators
        continue;
      'A'..'F':
        inc(c, 32);    // convert to lower-case
      'a'..'f', '0'..'9':
        ;              // valid hexadecimal char
    else
      result := false; // not a true hexadecimal content
    end;
    d^ := c;
    inc(d);
  until false;
  L := d - pointer(text);
  if L = 0 then
  begin
    FastAssignNew(text);
    result := false;
  end
  else
  begin
    FakeLength(text, L);
    result := result and (L = 32);
  end;
end;

function _ShortToUuid(const text: ShortString; out uuid: TGuid): boolean;
begin // much more efficient than default TryStringToGUID() in mormot.core.os
  result := (text[0] = #36) and
            (TextToGuid(@text[1], @uuid) <> nil);
end;

procedure _AppendShortUuid(const u: TGuid; var s: ShortString);
begin // much more efficient than default GUIDToString() in mormot.core.os
  if ord(s[0]) > high(s) - 36 then
    exit;
  GuidToText(@s[ord(s[0]) + 1], @u, @TwoDigitsHexLower);
  inc(s[0], 36);
end;

procedure HexLookup(lookup, hex: PAnsiChar);
var
  h, l: PtrInt;
begin
  for h := 0 to 15 do
    for l := 0 to 15 do
    begin
      lookup[0] := hex[h];
      lookup[1] := hex[l];
      inc(lookup, 2);
    end;
end;


procedure InitializeUnit;
var
  i: PtrInt;
  v: byte;
  P: PAnsiChar;
  B, B4: PByteArray;
  pc: PCardinalArray;
  tmp: TTemp16;
begin
  {$ifdef CPUX64}
  DecimalUseFma := CpuFeatures * [cfAVX, cfFMA] = [cfAVX, cfFMA];
  {$endif CPUX64}
  // initialize internal lookup tables for various text conversions
  HexLookup(@TwoDigitsHex,      '0123456789ABCDEF');
  HexLookup(@TwoDigitsHexLower, '0123456789abcdef');
  {$ifdef DOUBLETOSHORT_USEGRISU}
  MoveFast(TwoDigitLookup[0], TwoDigitByteLookupW[0], SizeOf(TwoDigitLookup));
  B := @TwoDigitByteLookupW;
  for i := 0 to 199 do
    dec(B[i], ord('0')); // '0'..'9' -> 0..9
  {$endif DOUBLETOSHORT_USEGRISU}
  FillcharFast(ConvertHexToBin, SizeOf(ConvertHexToBin), 255); // all to 255
  FillcharFast(ConvertHexToShl, SizeOf(ConvertHexToShl), 255);
  B  := @ConvertHexToBin;
  B4 := @ConvertHexToShl;
  v := 0;
  for i := ord('0') to ord('9') do
  begin
    B[i] := v;
    B4[i] := v shl 4;
    inc(v);
  end;
  for i := ord('A') to ord('F') do
  begin
    B[i] := v;
    B4[i] := v shl 4;
    B[i + (ord('a') - ord('A'))] := v;
    B4[i + (ord('a') - ord('A'))] := v shl 4;
    inc(v);
  end;
  PInt64(@tmp[8])^ := 0; // FastSetConst() copy 8 bytes - up to 7 may be 0
  for i := 0 to high(SmallUInt32Utf8) do // 0..999 into '0'..'999' RawUtf8
  begin
    P := StrUInt32(@tmp[8], i);
    FastSetConst(SmallUInt32Utf8[i], UINT_999[i], P, @tmp[8] - P);
  end;
  pc := @METHODNAME32;
  i := length(METHODNAME32);
  repeat
    dec(i);
    pc[i] := PCardinal(METHODNAME[TUriMethod(i)])^;
  until i = 0;
  ShortToUuid                   := _ShortToUuid;
  AppendShortUuid               := _AppendShortUuid;
  _AddHtmlEscape                := __AddHtmlEscape;
  _VariantToUtf8DateTimeIso8601 := __VariantToUtf8DateTimeIso8601;
  _VariantToTempUtf8DateTimeIso8601 := @__VariantToUtf8DateTimeIso8601;
  _VariantSaveJson              := __VariantSaveJson;
end;


initialization
  Assert(SizeOf(TFormatUtf8) <= SizeOf(TTextWriterStackBuffer)); // 4KB<=8KB
  InitializeUnit;

end.

