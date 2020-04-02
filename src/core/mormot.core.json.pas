/// Framework Core Low-Level JSON Processing
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.json;

{
  *****************************************************************************

   JSON functions shared by all framework units
    - Low-Level JSON Processing Functions
    - TTextWriter class with proper JSON escaping and WriteObject() support

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text;


{ ********** Low-Level JSON Processing Functions }

type
  /// kind of character used from JSON_CHARS[] for efficient JSON parsing
  // - using such a set compiles into TEST [MEM], IMM so is more efficient
  // than a regular set of AnsiChar which generates much slower BT [MEM], IMM
  // - the same 256-byte memory will also be reused from L1 CPU cache
  // during the parsing of complex JSON input
  TJsonChar = set of (jcJsonIdentifierFirstChar, jcJsonIdentifier,
    jcEndOfJSONField, jcEndOfJSONFieldOr0, jcEndOfJSONValueField,
    jcJSONStringMarker, jcDigitFirstChar, jcDigitFloatChar);

  /// defines a lookup table used for branch-less JSON parsing
  TJsonCharSet = array[AnsiChar] of TJsonChar;
  /// points to a lookup table used for branch-less JSON parsing
  PJsonCharSet = ^TJsonCharSet;

var
  /// 256-byte lookup table for fast branchless JSON text escaping
  // - 0 = JSON_ESCAPE_NONE indicates no escape needed
  // - 1 = JSON_ESCAPE_ENDINGZERO indicates #0 (end of string)
  // - 2 = JSON_ESCAPE_UNICODEHEX should be escaped as \u00xx
  // - b,t,n,f,r,\," as escaped character for #8,#9,#10,#12,#13,\,"
  JSON_ESCAPE: TNormTableByte;

const
  /// JSON_ESCAPE[] lookup value: indicates no escape needed
  JSON_ESCAPE_NONE = 0;
  /// JSON_ESCAPE[] lookup value: indicates #0 (end of string)
  JSON_ESCAPE_ENDINGZERO = 1;
  /// JSON_ESCAPE[] lookup value: should be escaped as \u00xx
  JSON_ESCAPE_UNICODEHEX = 2;

var
  /// 256-byte lookup table for fast branchless JSON parsing
  // - to be used e.g. as:
  // ! if jvJsonIdentifier in JSON_CHARS[P^] then ...
  JSON_CHARS: TJsonCharSet;


/// returns TRUE if the given text buffers would be escaped when written as JSON
// - e.g. if contains " or \ characters, as defined by
// http://www.ietf.org/rfc/rfc4627.txt
function NeedsJsonEscape(const Text: RawUTF8): boolean; overload;
  {$ifdef HASINLINE} inline;{$endif}

/// returns TRUE if the given text buffers would be escaped when written as JSON
// - e.g. if contains " or \ characters, as defined by
// http://www.ietf.org/rfc/rfc4627.txt

function NeedsJsonEscape(P: PUTF8Char): boolean; overload;

/// returns TRUE if the given text buffers would be escaped when written as JSON
// - e.g. if contains " or \ characters, as defined by
// http://www.ietf.org/rfc/rfc4627.txt
function NeedsJsonEscape(P: PUTF8Char; PLen: integer): boolean; overload;

/// UTF-8 encode one or two \u#### JSON escaped codepoints into Dest
// - P^ should point at 'u1234' just after \u1234
// - return ending P position, maybe after another \u#### UTF-16 surrogate char
function JsonEscapeToUtf8(var D: PUTF8Char; P: PUTF8Char): PUTF8Char;
  {$ifdef HASINLINE} inline; {$endif}

/// returns TRUE if the given text buffer contains simple characters as
// recognized by JSON extended syntax
// - follow GetJSONPropName and GotoNextJSONObjectOrArray expectations
function JsonPropNameValid(P: PUTF8Char): boolean;
  {$ifdef HASINLINE} inline;{$endif}

/// decode a JSON field in-place from an UTF-8 encoded text buffer
// - this function decodes in the P^ buffer memory itself (no memory allocation
// or copy), for faster process - so take care that P^ is not shared
// - PDest points to the next field to be decoded, or nil when end is reached
// - EndOfObject (if not nil) is set to the JSON value char (',' ':' or '}' e.g.)
// - optional wasString is set to true if the JSON value was a JSON "string"
// - '"strings"' are decoded as 'strings', with wasString=true, properly JSON
// unescaped (e.g. any \u0123 pattern would be converted into UTF-8 content)
// - null is decoded as nil, with wasString=false
// - true/false boolean values are returned as 'true'/'false', with wasString=false
// - any number value is returned as its ascii representation, with wasString=false
// - works for both field names or values (e.g. '"FieldName":' or 'Value,')
function GetJSONField(P: PUTF8Char; out PDest: PUTF8Char; wasString: PBoolean = nil;
  EndOfObject: PUTF8Char = nil; Len: PInteger = nil): PUTF8Char;

/// decode a JSON field name in an UTF-8 encoded buffer
// - this function decodes in the P^ buffer memory itself (no memory allocation
// or copy), for faster process - so take care that P^ is not shared
// - it will return the property name (with an ending #0) or nil on error
// - this function will handle strict JSON property name (i.e. a "string"), but
// also MongoDB extended syntax, e.g. {age:{$gt:18}} or {'people.age':{$gt:18}}
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
function GetJSONPropName(var P: PUTF8Char; Len: PInteger = nil): PUTF8Char; overload;

/// decode a JSON field name in an UTF-8 encoded shortstring variable
// - this function would left the P^ buffer memory untouched, so may be safer
// than the overloaded GetJSONPropName() function in some cases
// - it will return the property name as a local UTF-8 encoded shortstring,
// or PropName='' on error
// - this function won't unescape the property name, as strict JSON (i.e. a "st\"ring")
// - but it will handle MongoDB syntax, e.g. {age:{$gt:18}} or {'people.age':{$gt:18}}
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
procedure GetJSONPropName(var P: PUTF8Char; out PropName: shortstring); overload;

/// decode a JSON content in an UTF-8 encoded buffer
// - GetJSONField() will only handle JSON "strings" or numbers - if
// HandleValuesAsObjectOrArray is TRUE, this function will process JSON {
// objects } or [ arrays ] and add a #0 at the end of it
// - this function decodes in the P^ buffer memory itself (no memory allocation
// or copy), for faster process - so take care that it is an unique string
// - returns a pointer to the value start, and moved P to the next field to
// be decoded, or P=nil in case of any unexpected input
// - wasString is set to true if the JSON value was a "string"
// - EndOfObject (if not nil) is set to the JSON value end char (',' ':' or '}')
// - if Len is set, it will contain the length of the returned pointer value
function GetJSONFieldOrObjectOrArray(var P: PUTF8Char; wasString: PBoolean = nil;
  EndOfObject: PUTF8Char = nil; HandleValuesAsObjectOrArray: Boolean = false;
  NormalizeBoolean: Boolean = true; Len: PInteger = nil): PUTF8Char;

/// retrieve the next JSON item as a RawJSON variable
// - buffer can be either any JSON item, i.e. a string, a number or even a
// JSON array (ending with ]) or a JSON object (ending with })
// - EndOfObject (if not nil) is set to the JSON value end char (',' ':' or '}')
procedure GetJSONItemAsRawJSON(var P: PUTF8Char; var result: RawJSON;
  EndOfObject: PAnsiChar = nil);

/// retrieve the next JSON item as a RawUTF8 decoded buffer
// - buffer can be either any JSON item, i.e. a string, a number or even a
// JSON array (ending with ]) or a JSON object (ending with })
// - EndOfObject (if not nil) is set to the JSON value end char (',' ':' or '}')
// - just call GetJSONField(), and create a new RawUTF8 from the returned value,
// after proper unescape if wasString^=true
function GetJSONItemAsRawUTF8(var P: PUTF8Char; var output: RawUTF8;
  wasString: PBoolean = nil; EndOfObject: PUTF8Char = nil): boolean;

/// read the position of the JSON value just after a property identifier
// - this function will handle strict JSON property name (i.e. a "string"), but
// also MongoDB extended syntax, e.g. {age:{$gt:18}} or {'people.age':{$gt:18}}
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
function GotoNextJSONPropName(P: PUTF8Char): PUTF8Char;

/// get the next character after a quoted buffer
// - the first character in P^ must be "
// - it will return the latest " position, ignoring \" within
// - caller should check that return PUTF8Char is indeed a "
function GotoEndOfJSONString(P: PUTF8Char): PUTF8Char; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// get the next character after a quoted buffer
// - the first character in P^ must be "
// - it will return the latest " position, ignoring \" within
// - overloaded version when JSON_CHARS table is already available locally
function GotoEndOfJSONString(P: PUTF8Char; tab: PJsonCharSet): PUTF8Char; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// reach positon just after the current JSON item in the supplied UTF-8 buffer
// - buffer can be either any JSON item, i.e. a string, a number or even a
// JSON array (ending with ]) or a JSON object (ending with })
// - returns nil if the specified buffer is not valid JSON content
// - returns the position in buffer just after the item excluding the separator
// character - i.e. result^ may be ',','}',']'
// - for speed, numbers and true/false/null constant won't be exactly checked,
// and MongoDB extended syntax like {age:{$gt:18}} will be allowed - so you
// may consider GotoEndJSONItemAndValidate() if you expect strict JSON parsing
function GotoEndJSONItem(P: PUTF8Char): PUTF8Char;

/// reach positon just after the current JSON item in the supplied UTF-8 buffer
// - in respect to GotoEndJSONItem(), this function will validate for strict
// JSON simple values, i.e. real numbers or only true/false/null constants,
// and refuse MongoDB extended syntax like {age:{$gt:18}}
function GotoEndJSONItemStrict(P: PUTF8Char): PUTF8Char;

/// reach the positon of the next JSON item in the supplied UTF-8 buffer
// - buffer can be either any JSON item, i.e. a string, a number or even a
// JSON array (ending with ]) or a JSON object (ending with })
// - returns nil if the specified number of items is not available in buffer
// - returns the position in buffer after the item including the separator
// character (optionally in EndOfObject) - i.e. result will be at the start of
// the next object, and EndOfObject may be ',','}',']'
function GotoNextJSONItem(P: PUTF8Char; NumberOfItemsToJump: cardinal = 1;
  EndOfObject: PAnsiChar = nil): PUTF8Char;

/// reach the position of the next JSON object of JSON array
// - first char is expected to be either '[' or '{'
// - will return nil in case of parsing error or unexpected end (#0)
// - will return the next character after ending ] or } - i.e. may be , } ]
function GotoNextJSONObjectOrArray(P: PUTF8Char): PUTF8Char; overload;
  {$ifdef FPC}inline;{$endif}

/// reach the position of the next JSON object of JSON array
// - first char is expected to be just after the initial '[' or '{'
// - specify ']' or '}' as the expected EndChar
// - will return nil in case of parsing error or unexpected end (#0)
// - will return the next character after ending ] or } - i.e. may be , } ]
function GotoNextJSONObjectOrArray(P: PUTF8Char; EndChar: AnsiChar): PUTF8Char; overload;
  {$ifdef FPC}inline;{$endif}

/// reach the position of the next JSON object of JSON array
// - first char is expected to be either '[' or '{'
// - this version expects a maximum position in PMax: it may be handy to break
// the parsing for HUGE content - used e.g. by JSONArrayCount(P,PMax)
// - will return nil in case of parsing error or if P reached PMax limit
// - will return the next character after ending ] or { - i.e. may be , } ]
function GotoNextJSONObjectOrArrayMax(P,PMax: PUTF8Char): PUTF8Char;

/// compute the number of elements of a JSON array
// - this will handle any kind of arrays, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first char AFTER the initial '[' (which
// may be a closing ']')
// - returns -1 if the supplied input is invalid, or the number of identified
// items in the JSON array buffer
function JSONArrayCount(P: PUTF8Char): integer; overload;

/// compute the number of elements of a JSON array
// - this will handle any kind of arrays, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first char after the initial '[' (which
// may be a closing ']')
// - this overloaded method will abort if P reaches a certain position: for
// really HUGE arrays, it is faster to allocate the content within the loop,
// not ahead of time
function JSONArrayCount(P,PMax: PUTF8Char): integer; overload;

/// go to the #nth item of a JSON array
// - implemented via a fast SAX-like approach: the input buffer is not changed,
// nor no memory buffer allocated neither content copied
// - returns nil if the supplied index is out of range
// - returns a pointer to the index-nth item in the JSON array (first index=0)
// - this will handle any kind of arrays, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first initial '[' char
function JSONArrayItem(P: PUTF8Char; Index: integer): PUTF8Char;

/// retrieve the positions of all elements of a JSON array
// - this will handle any kind of arrays, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first char AFTER the initial '[' (which
// may be a closing ']')
// - returns false if the supplied input is invalid
// - returns true on success, with Values[] pointing to each unescaped value,
// may be a JSON string, object, array of constant
function JSONArrayDecode(P: PUTF8Char; out Values: TPUTF8CharDynArray): boolean;

/// compute the number of fields in a JSON object
// - this will handle any kind of objects, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first char after the initial '{' (which
// may be a closing '}')
function JSONObjectPropCount(P: PUTF8Char): integer;

/// go to a named property of a JSON object
// - implemented via a fast SAX-like approach: the input buffer is not changed,
// nor no memory buffer allocated neither content copied
// - returns nil if the supplied property name does not exist
// - returns a pointer to the matching item in the JSON object
// - this will handle any kind of objects, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first initial '{' char
function JsonObjectItem(P: PUTF8Char; const PropName: RawUTF8;
  PropNameFound: PRawUTF8 = nil): PUTF8Char;

/// go to a property of a JSON object, by its full path, e.g. 'parent.child'
// - implemented via a fast SAX-like approach: the input buffer is not changed,
// nor no memory buffer allocated neither content copied
// - returns nil if the supplied property path does not exist
// - returns a pointer to the matching item in the JSON object
// - this will handle any kind of objects, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first initial '{' char
function JsonObjectByPath(JsonObject,PropPath: PUTF8Char): PUTF8Char;

/// return all matching properties of a JSON object
// - here the PropPath could be a comma-separated list of full paths,
// e.g. 'Prop1,Prop2' or 'Obj1.Obj2.Prop1,Obj1.Prop2'
// - returns '' if no property did match
// - returns a JSON object of all matching properties
// - this will handle any kind of objects, including those with nested
// JSON objects or arrays
// - incoming P^ should point to the first initial '{' char
function JsonObjectsByPath(JsonObject,PropPath: PUTF8Char): RawUTF8;

/// convert one JSON object into two JSON arrays of keys and values
// - i.e. makes the following transformation:
// $ {key1:value1,key2,value2...} -> [key1,key2...] + [value1,value2...]
// - this function won't allocate any memory during its process, nor
// modify the JSON input buffer
// - is the reverse of the TTextWriter.AddJSONArraysAsJSONObject() method
function JSONObjectAsJSONArrays(JSON: PUTF8Char; out keys,values: RawUTF8): boolean;

/// remove comments from a text buffer before passing it to JSON parser
// - handle two types of comments: starting from // till end of line
// or /* ..... */ blocks anywhere in the text content
// - may be used to prepare configuration files before loading;
// for example we store server configuration in file config.json and
// put some comments in this file then code for loading is:
// !var cfg: RawUTF8;
// !  cfg := StringFromFile(ExtractFilePath(paramstr(0))+'Config.json');
// !  RemoveCommentsFromJSON(@cfg[1]);
// !  pLastChar := JSONToObject(sc,pointer(cfg),configValid);
procedure RemoveCommentsFromJSON(P: PUTF8Char); overload;

/// remove comments from a text buffer before passing it to JSON parser
// - won't remove the comments in-place, but allocate a new string
function RemoveCommentsFromJSON(const s: RawUTF8): RawUTF8; overload;

type
  /// points to one value of raw UTF-8 content, decoded from a JSON buffer
  // - used e.g. by JSONDecode() overloaded function to returns names/values
  TValuePUTF8Char = object
  public
    /// a pointer to the actual UTF-8 text
    Value: PUTF8Char;
    /// how many UTF-8 bytes are stored in Value
    ValueLen: PtrInt;
    /// convert the value into a UTF-8 string
    procedure ToUTF8(var Text: RawUTF8); overload; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into a UTF-8 string
    function ToUTF8: RawUTF8; overload; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into a VCL/generic string
    function ToString: string;
    /// convert the value into a signed integer
    function ToInteger: PtrInt; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into an unsigned integer
    function ToCardinal: PtrUInt; {$ifdef HASINLINE}inline;{$endif}
    /// will call IdemPropNameU() over the stored text Value
    function Idem(const Text: RawUTF8): boolean; {$ifdef HASINLINE}inline;{$endif}
  end;
  /// used e.g. by JSONDecode() overloaded function to returns values
  TValuePUTF8CharArray = array[0..maxInt div SizeOf(TValuePUTF8Char)-1] of TValuePUTF8Char;
  PValuePUTF8CharArray = ^TValuePUTF8CharArray;

  /// store one name/value pair of raw UTF-8 content, from a JSON buffer
  // - used e.g. by JSONDecode() overloaded function or UrlEncodeJsonObject()
  // to returns names/values
  TNameValuePUTF8Char = record
    /// a pointer to the actual UTF-8 name text
    Name: PUTF8Char;
    /// a pointer to the actual UTF-8 value text
    Value: PUTF8Char;
    /// how many UTF-8 bytes are stored in Name (should be integer, not PtrInt)
    NameLen: integer;
    /// how many UTF-8 bytes are stored in Value
    ValueLen: integer;
  end;

  /// used e.g. by JSONDecode() overloaded function to returns name/value pairs
  TNameValuePUTF8CharDynArray = array of TNameValuePUTF8Char;

/// decode the supplied UTF-8 JSON content for the supplied names
// - data will be set in Values, according to the Names supplied e.g.
// ! JSONDecode(JSON,['name','year'],@Values) -> Values[0].Value='John'; Values[1].Value='1972';
// - if any supplied name wasn't found its corresponding Values[] will be nil
// - this procedure will decode the JSON content in-memory, i.e. the PUtf8Char
// array is created inside JSON, which is therefore modified: make a private
// copy first if you want to reuse the JSON content
// - if HandleValuesAsObjectOrArray is TRUE, then this procedure will handle
// JSON arrays or objects
// - support enhanced JSON syntax, e.g. '{name:'"John",year:1972}' is decoded
// just like '{"name":'"John","year":1972}'
procedure JSONDecode(var JSON: RawUTF8; const Names: array of RawUTF8;
  Values: PValuePUTF8CharArray; HandleValuesAsObjectOrArray: Boolean = false); overload;

/// decode the supplied UTF-8 JSON content for the supplied names
// - an overloaded function when the JSON is supplied as a RawJSON variable
procedure JSONDecode(var JSON: RawJSON; const Names: array of RawUTF8;
  Values: PValuePUTF8CharArray; HandleValuesAsObjectOrArray: Boolean = false); overload;

/// decode the supplied UTF-8 JSON content for the supplied names
// - data will be set in Values, according to the Names supplied e.g.
// ! JSONDecode(P,['name','year'],Values) -> Values[0]^='John'; Values[1]^='1972';
// - if any supplied name wasn't found its corresponding Values[] will be nil
// - this procedure will decode the JSON content in-memory, i.e. the PUtf8Char
// array is created inside P, which is therefore modified: make a private
// copy first if you want to reuse the JSON content
// - if HandleValuesAsObjectOrArray is TRUE, then this procedure will handle
// JSON arrays or objects
// - if ValuesLen is set, ValuesLen[] will contain the length of each Values[]
// - returns a pointer to the next content item in the JSON buffer
function JSONDecode(P: PUTF8Char; const Names: array of RawUTF8;
  Values: PValuePUTF8CharArray; HandleValuesAsObjectOrArray: Boolean = false): PUTF8Char; overload;

/// decode the supplied UTF-8 JSON content into an array of name/value pairs
// - this procedure will decode the JSON content in-memory, i.e. the PUtf8Char
// array is created inside JSON, which is therefore modified: make a private
// copy first if you want to reuse the JSON content
// - the supplied JSON buffer should stay available until Name/Value pointers
// from returned Values[] are accessed
// - if HandleValuesAsObjectOrArray is TRUE, then this procedure will handle
// JSON arrays or objects
// - support enhanced JSON syntax, e.g. '{name:'"John",year:1972}' is decoded
// just like '{"name":'"John","year":1972}'
function JSONDecode(P: PUTF8Char; out Values: TNameValuePUTF8CharDynArray;
  HandleValuesAsObjectOrArray: Boolean = false): PUTF8Char; overload;

/// decode the supplied UTF-8 JSON content for the one supplied name
// - this function will decode the JSON content in-memory, so will unescape it
// in-place: it must be called only once with the same JSON data
function JSONDecode(var JSON: RawUTF8; const aName: RawUTF8 = 'result';
  wasString: PBoolean = nil; HandleValuesAsObjectOrArray: Boolean = false): RawUTF8; overload;

/// retrieve a pointer to JSON string field content, without unescaping it
// - returns either ':' for name field, either '}',',' for value field
// - returns nil on JSON content error
// - this function won't touch the JSON buffer, so you can call it before
// using in-place escape process via JSONDecode() or GetJSONField()
function JSONRetrieveStringField(P: PUTF8Char; out Field: PUTF8Char;
  out FieldLen: integer; ExpectNameField: boolean): PUTF8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// encode a JSON object UTF-8 buffer into URI parameters
// - you can specify property names to ignore during the object decoding
// - you can omit the leading query delimiter ('?') by setting IncludeQueryDelimiter=false
// - warning: the ParametersJSON input buffer will be modified in-place
function UrlEncodeJsonObject(const URIName: RawUTF8; ParametersJSON: PUTF8Char;
  const PropNamesToIgnore: array of RawUTF8; IncludeQueryDelimiter: Boolean = true): RawUTF8; overload;

/// encode a JSON object UTF-8 buffer into URI parameters
// - you can specify property names to ignore during the object decoding
// - you can omit the leading query delimiter ('?') by setting IncludeQueryDelimiter=false
// - overloaded function which will make a copy of the input JSON before parsing
function UrlEncodeJsonObject(const URIName, ParametersJSON: RawUTF8;
  const PropNamesToIgnore: array of RawUTF8; IncludeQueryDelimiter: Boolean = true): RawUTF8; overload;


/// convert UTF-8 content into a JSON string
// - with proper escaping of the content, and surounding " characters
procedure QuotedStrJSON(const aText: RawUTF8; var result: RawUTF8;
  const aPrefix: RawUTF8 = ''; const aSuffix: RawUTF8 = ''); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert UTF-8 buffer into a JSON string
// - with proper escaping of the content, and surounding " characters
procedure QuotedStrJSON(P: PUTF8Char; PLen: PtrInt; var result: RawUTF8;
  const aPrefix: RawUTF8 = ''; const aSuffix: RawUTF8 = ''); overload;

/// convert UTF-8 content into a JSON string
// - with proper escaping of the content, and surounding " characters
function QuotedStrJSON(const aText: RawUTF8): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast Format() function replacement, handling % and ? parameters
// - will include Args[] for every % in Format
// - will inline Params[] for every ? in Format, handling special "inlined"
// parameters, as exected by mORMot.pas unit, i.e. :(1234): for numerical
// values, and :('quoted '' string'): for textual values
// - if optional JSONFormat parameter is TRUE, ? parameters will be written
// as JSON quoted strings, without :(...): tokens, e.g. "quoted "" string"
// - resulting string has no length limit and uses fast concatenation
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
function FormatUTF8(const Format: RawUTF8; const Args, Params: array of const;
  JSONFormat: boolean = false): RawUTF8; overload;



{ ********** TTextWriter class with proper JSON escaping and WriteObject() support }

type
  TTextWriter = class(TBaseWriter)
  protected
    procedure InternalAddFixedAnsi(Source: PAnsiChar; SourceChars: Cardinal;
      AnsiToWide: PWordArray; Escape: TTextWriterKind);
  public
    /// write some #0 ended UTF-8 text, according to the specified format
    // - if Escape is a constant, consider calling directly AddNoJSONEscape,
    // AddJSONEscape or AddOnSameLine methods
    procedure Add(P: PUTF8Char; Escape: TTextWriterKind); overload;
    /// write some #0 ended UTF-8 text, according to the specified format
    // - if Escape is a constant, consider calling directly AddNoJSONEscape,
    // AddJSONEscape or AddOnSameLine methods
    procedure Add(P: PUTF8Char; Len: PtrInt; Escape: TTextWriterKind); overload;
    /// write some #0 ended Unicode text as UTF-8, according to the specified format
    // - if Escape is a constant, consider calling directly AddNoJSONEscapeW,
    // AddJSONEscapeW or AddOnSameLineW methods
    procedure AddW(P: PWord; Len: PtrInt; Escape: TTextWriterKind);
    /// append some UTF-8 encoded chars to the buffer, from the main AnsiString type
    // - use the current system code page for AnsiString parameter
    procedure AddAnsiString(const s: AnsiString; Escape: TTextWriterKind); overload;
    /// append some UTF-8 encoded chars to the buffer, from any AnsiString value
    // - if CodePage is left to its default value of -1, it will assume
    // CurrentAnsiConvert.CodePage prior to Delphi 2009, but newer UNICODE
    // versions of Delphi will retrieve the code page from string
    // - if CodePage is defined to a >= 0 value, the encoding will take place
    procedure AddAnyAnsiString(const s: RawByteString; Escape: TTextWriterKind;
      CodePage: Integer = -1);
    /// append some UTF-8 encoded chars to the buffer, from any Ansi buffer
    // - the codepage should be specified, e.g. CP_UTF8, CP_RAWBYTESTRING,
    // CODEPAGE_US, or any version supported by the Operating System
    // - if codepage is 0, the current CurrentAnsiConvert.CodePage would be used
    // - will use TSynAnsiConvert to perform the conversion to UTF-8
    procedure AddAnyAnsiBuffer(P: PAnsiChar; Len: PtrInt;
      Escape: TTextWriterKind; CodePage: Integer);
    /// write some data Base64 encoded
    // - if withMagic is TRUE, will write as '"\uFFF0base64encodedbinary"'
    procedure WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean);
    /// append strings or integers with a specified format
    // - % = #37 marks a string, integer, floating-point, or class parameter
    // to be appended as text (e.g. class name)
    // - note that due to a limitation of the "array of const" format, cardinal
    // values should be type-casted to Int64() - otherwise the integer mapped
    // value will be transmitted, therefore wrongly
    procedure Add(const Format: RawUTF8; const Values: array of const;
      Escape: TTextWriterKind = twNone;
      WriteObjectOptions: TTextWriterWriteObjectOptions = [woFullExpand]); overload;
    /// append some values at once
    // - text values (e.g. RawUTF8) will be escaped as JSON
    procedure Add(const Values: array of const); overload;
    /// append a TTimeLog value, expanded as Iso-8601 encoded text
    procedure AddTimeLog(Value: PInt64);
    /// append a TUnixTime value, expanded as Iso-8601 encoded text
    procedure AddUnixTime(Value: PInt64);
    /// append a TUnixMSTime value, expanded as Iso-8601 encoded text
    procedure AddUnixMSTime(Value: PInt64; WithMS: boolean = false);
    /// append a TDateTime value, expanded as Iso-8601 encoded text
    // - use 'YYYY-MM-DDThh:mm:ss' format (with FirstChar='T')
    // - if WithMS is TRUE, will append '.sss' for milliseconds resolution
    // - if QuoteChar is not #0, it will be written before and after the date
    procedure AddDateTime(Value: PDateTime; FirstChar: AnsiChar = 'T';
      QuoteChar: AnsiChar = #0; WithMS: boolean = false); overload;
    /// append a TDateTime value, expanded as Iso-8601 encoded text
    // - use 'YYYY-MM-DDThh:mm:ss' format
    // - append nothing if Value=0
    // - if WithMS is TRUE, will append '.sss' for milliseconds resolution
    procedure AddDateTime(const Value: TDateTime; WithMS: boolean = false); overload;
    /// append a TDateTime value, expanded as Iso-8601 text with milliseconds
    // and Time Zone designator
    // - i.e. 'YYYY-MM-DDThh:mm:ss.sssZ' format
    // - TZD is the ending time zone designator ('', 'Z' or '+hh:mm' or '-hh:mm')
    procedure AddDateTimeMS(const Value: TDateTime; Expanded: boolean = true;
      FirstTimeChar: AnsiChar = 'T'; const TZD: RawUTF8 = 'Z');

    /// append a variant content as number or string
    // - this overriden version will properly handle JSON escape
    procedure AddVariant(const Value: variant; Escape: TTextWriterKind = twJSONEscape); override;
    /// append a JSON value, array or document, in a specified format
    // - this overriden version will properly handle JSON escape
    function AddJSONReformat(JSON: PUTF8Char; Format: TTextWriterJSONFormat;
      EndOfObject: PUTF8Char): PUTF8Char; override;
    /// serialize as JSON the given object
    // - this overriden version will properly handle JSON escape
    procedure WriteObject(Value: TObject;
      Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]); override;

    /// append some UTF-8 encoded chars to the buffer
    // - escapes chars according to the JSON RFC
    // - if Len is 0, writing will stop at #0 (default Len = 0 is slightly faster
    // than specifying Len>0 if you are sure P is zero-ended - e.g. from RawUTF8)
    procedure AddJSONEscape(P: Pointer; Len: PtrInt = 0); overload;
    /// append some UTF-8 encoded chars to the buffer, from a generic string type
    // - faster than AddJSONEscape(pointer(StringToUTF8(string))
    // - escapes chars according to the JSON RFC
    procedure AddJSONEscapeString(const s: string);  {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 encoded chars to the buffer, from the main AnsiString type
    // - escapes chars according to the JSON RFC
    procedure AddJSONEscapeAnsiString(const s: AnsiString);
    /// append some UTF-8 encoded chars to the buffer, from a generic string type
    // - faster than AddNoJSONEscape(pointer(StringToUTF8(string))
    // - don't escapes chars according to the JSON RFC
    // - will convert the Unicode chars into UTF-8
    procedure AddNoJSONEscapeString(const s: string);  {$ifdef HASINLINE}inline;{$endif}
    /// append some Unicode encoded chars to the buffer
    // - if Len is 0, Len is calculated from zero-ended widechar
    // - escapes chars according to the JSON RFC
    procedure AddJSONEscapeW(P: PWord; Len: PtrInt = 0);
    /// append an open array constant value to the buffer
    // - "" will be added if necessary
    // - escapes chars according to the JSON RFC
    // - very fast (avoid most temporary storage)
    procedure AddJSONEscape(const V: TVarRec); overload;
    /// append a UTF-8 JSON String, between double quotes and with JSON escaping
    procedure AddJSONString(const Text: RawUTF8);
    /// flush a supplied TTextWriter, and write pending data as JSON escaped text
    // - may be used with InternalJSONWriter, as a faster alternative to
    // ! AddJSONEscape(Pointer(fInternalJSONWriter.Text),0);
    procedure AddJSONEscape(Source: TTextWriter); overload;
    /// flush a supplied TTextWriter, and write pending data as JSON escaped text
    // - may be used with InternalJSONWriter, as a faster alternative to
    // ! AddNoJSONEscapeUTF8(Source.Text);
    procedure AddNoJSONEscape(Source: TTextWriter); overload;
    /// append an open array constant value to the buffer
    // - "" won't be added for string values
    // - string values may be escaped, depending on the supplied parameter
    // - very fast (avoid most temporary storage)
    procedure Add(const V: TVarRec; Escape: TTextWriterKind = twNone;
      WriteObjectOptions: TTextWriterWriteObjectOptions = [woFullExpand]); overload;
    /// encode the supplied data as an UTF-8 valid JSON object content
    // - data must be supplied two by two, as Name,Value pairs, e.g.
    // ! aWriter.AddJSONEscape(['name','John','year',1972]);
    // will append to the buffer:
    // ! '{"name":"John","year":1972}'
    // - or you can specify nested arrays or objects with '['..']' or '{'..'}':
    // ! aWriter.AddJSONEscape(['doc','{','name','John','ab','[','a','b']','}','id',123]);
    // will append to the buffer:
    // ! '{"doc":{"name":"John","abc":["a","b"]},"id":123}'
    // - note that, due to a Delphi compiler limitation, cardinal values should be
    // type-casted to Int64() (otherwise the integer mapped value will be converted)
    // - you can pass nil as parameter for a null JSON value
    procedure AddJSONEscape(const NameValuePairs: array of const); overload;
    /// encode the supplied (extended) JSON content, with parameters,
    // as an UTF-8 valid JSON object content
    // - in addition to the JSON RFC specification strict mode, this method will
    // handle some BSON-like extensions, e.g. unquoted field names:
    // ! aWriter.AddJSON('{id:?,%:{name:?,birthyear:?}}',['doc'],[10,'John',1982]);
    // - you can use nested _Obj() / _Arr() instances
    // ! aWriter.AddJSON('{%:{$in:[?,?]}}',['type'],['food','snack']);
    // ! aWriter.AddJSON('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
    // ! // which are the same as:
    // ! aWriter.AddShort('{"type":{"$in":["food","snack"]}}');
    // - if the SynMongoDB unit is used in the application, the MongoDB Shell
    // syntax will also be recognized to create TBSONVariant, like
    // ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
    // see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
    // !  aWriter.AddJSON('{name:?,field:/%/i}',['acme.*corp'],['John']))
    // ! // will write
    // ! '{"name":"John","field":{"$regex":"acme.*corp","$options":"i"}}'
    // - will call internally _JSONFastFmt() to create a temporary TDocVariant
    // with all its features - so is slightly slower than other AddJSON* methods
    procedure AddJSON(const Format: RawUTF8; const Args, Params: array of const);
    /// append two JSON arrays of keys and values as one JSON object
    // - i.e. makes the following transformation:
    // $ [key1,key2...] + [value1,value2...] -> {key1:value1,key2,value2...}
    // - this method won't allocate any memory during its process, nor
    // modify the keys and values input buffers
    // - is the reverse of the JSONObjectAsJSONArrays() function
    procedure AddJSONArraysAsJSONObject(keys, values: PUTF8Char);
  end;

implementation

uses
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.variants;


{ ********** Low-Level JSON Processing Functions }


function NeedsJsonEscape(P: PUTF8Char; PLen: integer): boolean;
var
  tab: PNormTableByte;
begin
  result := true;
  tab := @JSON_ESCAPE;
  if PLen > 0 then
    repeat
      if tab[ord(P^)] <> JSON_ESCAPE_NONE then
        exit;
      inc(P);
      dec(PLen);
    until PLen = 0;
  result := false;
end;

function NeedsJsonEscape(const Text: RawUTF8): boolean;
begin
  result := NeedsJsonEscape(pointer(Text), length(Text));
end;

function NeedsJsonEscape(P: PUTF8Char): boolean;
var
  tab: PNormTableByte;
  esc: byte;
begin
  result := false;
  if P = nil then
    exit;
  tab := @JSON_ESCAPE;
  repeat
    esc := tab[ord(P^)];
    if esc = JSON_ESCAPE_NONE then
      inc(P)
    else if esc = JSON_ESCAPE_ENDINGZERO then
      exit
    else
      break;
  until false;
  result := true;
end;

function JsonEscapeToUtf8(var D: PUTF8Char;  P: PUTF8Char): PUTF8Char;
var
  c, s: cardinal;
begin // P^ points at 'u1234' just after \u0123
  c := (ConvertHexToBin[ord(P[1])] shl 12) or
       (ConvertHexToBin[ord(P[2])] shl 8) or
       (ConvertHexToBin[ord(P[3])] shl 4) or
        ConvertHexToBin[ord(P[4])];
  if c = 0 then
    D^ := '?' // \u0000 is an invalid value
  else if c <= $7f then
    D^ := AnsiChar(c)
  else if c < $7ff then
  begin
    D[0] := AnsiChar($C0 or (c shr 6));
    D[1] := AnsiChar($80 or (c and $3F));
    inc(D);
  end
  else if (c >= UTF16_HISURROGATE_MIN) and (c <= UTF16_LOSURROGATE_MAX) then
    if PWord(P + 5)^ = ord('\') + ord('u') shl 8 then
    begin
      s := (ConvertHexToBin[ord(P[7])] shl 12)+
           (ConvertHexToBin[ord(P[8])] shl 8)+
           (ConvertHexToBin[ord(P[9])] shl 4)+
            ConvertHexToBin[ord(P[10])];
      case c of // inlined UTF16CharToUtf8()
        UTF16_HISURROGATE_MIN..UTF16_HISURROGATE_MAX:
          c := ((c - $D7C0) shl 10) or (s xor UTF16_LOSURROGATE_MIN);
        UTF16_LOSURROGATE_MIN..UTF16_LOSURROGATE_MAX:
          c := ((s - $D7C0)shl 10) or (c xor UTF16_LOSURROGATE_MIN);
      end;
      inc(D, UCS4ToUTF8(c, D));
      result := P + 11;
      exit;
    end
    else
      D^ := '?'
  else
  begin
    D[0] := AnsiChar($E0 or (c shr 12));
    D[1] := AnsiChar($80 or ((c shr 6) and $3F));
    D[2] := AnsiChar($80 or (c and $3F));
    inc(D,2);
  end;
  inc(D);
  result := P + 5;
end;

function JsonPropNameValid(P: PUTF8Char): boolean;
var
  tab: PJsonCharSet;
begin
  tab := @JSON_CHARS;
  if (P <> nil) and (jcJsonIdentifierFirstChar in tab[P^]) then
  begin
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    result := P^ = #0;
  end
  else
    result := false;
end;

function GetJSONField(P: PUTF8Char; out PDest: PUTF8Char; wasString: PBoolean;
  EndOfObject: PUTF8Char; Len: PInteger): PUTF8Char;
var
  D: PUTF8Char;
  c4, surrogate, extra: PtrUInt;
  c: AnsiChar;
  jsonset: PJsonCharSet;
label
  slash, num, lit;
begin // see http://www.ietf.org/rfc/rfc4627.txt
  if wasString <> nil then
    wasString^ := false; // not a string by default
  PDest := nil; // PDest=nil indicates error or unexpected end (#0)
  result := nil;
  if P = nil then
    exit;
  if P^ <= ' ' then
    repeat
      inc(P);
      if P^ = #0 then
        exit;
    until P^ > ' ';
  case P^ of
    '"':
      begin // " -> unescape P^ into D^
        if wasString <> nil then
          wasString^ := true;
        inc(P);
        result := P;
        D := P;
        repeat
          c := P^;
          if c = #0 then
            exit
          else if c = '"' then
            break
          else if c = '\' then
            goto slash;
          inc(P);
          D^ := c;
          inc(D);
          continue;
slash:    inc(P); // unescape JSON string
          c := P^;
          if (c = '"') or (c = '\') then
          begin
lit:        inc(P);
            D^ := c; // most common case
            inc(D);
            continue;
          end
          else if c = #0 then
            exit // to avoid potential buffer overflow issue on \#0
          else if c = 'b' then
            c := #8
          else if c = 't' then
            c := #9
          else if c = 'n' then
            c := #10
          else if c = 'f' then
            c := #12
          else if c = 'r' then
            c := #13
          else if c = 'u' then  // decode '\u0123' UTF-16 into UTF-8
          begin
            // note: JsonEscapeToUtf8() inlined here to optimize GetJSONField
            c4 := (ConvertHexToBin[ord(P[1])] shl 12) or
                  (ConvertHexToBin[ord(P[2])] shl 8) or
                  (ConvertHexToBin[ord(P[3])] shl 4) or
                   ConvertHexToBin[ord(P[4])];
            inc(P, 5); // optimistic conversion (no check)
            case c4 of
              0:
                begin
                  D^ := '?'; // \u0000 is an invalid value
                  inc(D);
                end;
              1..$7f:
                begin
                  D^ := AnsiChar(c4);
                  inc(D);
                end;
              $80..$7ff:
                begin
                  D[0] := AnsiChar($C0 or (c4 shr 6));
                  D[1] := AnsiChar($80 or (c4 and $3F));
                  inc(D, 2);
                end;
              UTF16_HISURROGATE_MIN..UTF16_LOSURROGATE_MAX:
                if PWord(P)^ = ord('\') + ord('u') shl 8 then
                begin
                  inc(P);
                  surrogate := (ConvertHexToBin[ord(P[1])] shl 12) or
                               (ConvertHexToBin[ord(P[2])] shl 8) or
                               (ConvertHexToBin[ord(P[3])] shl 4) or
                                ConvertHexToBin[ord(P[4])];
                  case c4 of // inlined UTF16CharToUtf8()
                    UTF16_HISURROGATE_MIN..UTF16_HISURROGATE_MAX:
                      c4 := ((c4 - $D7C0) shl 10) or (surrogate xor UTF16_LOSURROGATE_MIN);
                    UTF16_LOSURROGATE_MIN..UTF16_LOSURROGATE_MAX:
                      c4 := ((surrogate - $D7C0) shl 10) or (c4 xor UTF16_LOSURROGATE_MIN);
                  end;
                  if c4 <= $7ff then
                    c := #2
                  else if c4 <= $ffff then
                    c := #3
                  else if c4 <= $1FFFFF then
                    c := #4
                  else if c4 <= $3FFFFFF then
                    c := #5
                  else
                    c := #6;
                  extra := ord(c) - 1;
                  repeat
                    D[extra] := AnsiChar((c4 and $3f) or $80);
                    c4 := c4 shr 6;
                    dec(extra);
                  until extra = 0;
                  D^ := AnsiChar(Byte(c4) or UTF8_FIRSTBYTE[ord(c)]);
                  inc(D, ord(c));
                  inc(P, 5);
                end
                else
                begin
                  D^ := '?'; // unexpected surrogate without its pair
                  inc(D);
                end;
            else
              begin
                D[0] := AnsiChar($E0 or (c4 shr 12));
                D[1] := AnsiChar($80 or ((c4 shr 6) and $3F));
                D[2] := AnsiChar($80 or (c4 and $3F));
                inc(D, 3);
              end;
            end;
            continue;
          end;
          goto lit;
        until false;
        // here P^='"'
        D^ := #0; // make zero-terminated
        if Len <> nil then
          Len^ := D - result;
        inc(P);
        if P^ = #0 then
          exit;
      end;
    '0':
      if P[1] in ['0'..'9'] then // 0123 excluded by JSON!
        exit
      else // leave PDest=nil for unexpected end
        goto num; // may be 0.123
    '-', '1'..'9':
      begin // numerical field: all chars before end of field
num:    result := P;
        jsonset := @JSON_CHARS;
        repeat
          if not (jcDigitFloatChar in jsonset[P^]) then
            break;
          inc(P);
        until false;
        if P^ = #0 then
          exit;
        if Len <> nil then
          Len^ := P - result;
        if P^ <= ' ' then
        begin
          P^ := #0; // force numerical field with no trailing ' '
          inc(P);
        end;
      end;
    'n':
      if (PInteger(P)^ = NULL_LOW) and (jcEndOfJSONValueField in JSON_CHARS[P[4]]) then
      begin
        result := nil; // null -> returns nil and wasString=false
        if Len <> nil then
          Len^ := 0; // when result is converted to string
        inc(P, 4);
      end
      else
        exit;
    'f':
      if (PInteger(P + 1)^ = ord('a') + ord('l') shl 8 + ord('s') shl 16 +
         ord('e') shl 24) and (jcEndOfJSONValueField in JSON_CHARS[P[5]]) then
      begin
        result := P; // false -> returns 'false' and wasString=false
        if Len <> nil then
          Len^ := 5;
        inc(P, 5);
      end
      else
        exit;
    't':
      if (PInteger(P)^ = TRUE_LOW) and (jcEndOfJSONValueField in JSON_CHARS[P[4]]) then
      begin
        result := P; // true -> returns 'true' and wasString=false
        if Len <> nil then
          Len^ := 4;
        inc(P, 4);
      end
      else
        exit;
  else
    exit; // PDest=nil to indicate error
  end;
  jsonset := @JSON_CHARS;
  while not (jcEndOfJSONField in jsonset[P^]) do
  begin
    if P^ = #0 then
      exit; // leave PDest=nil for unexpected end
    inc(P);
  end;
  if EndOfObject <> nil then
    EndOfObject^ := P^;
  P^ := #0; // make zero-terminated
  PDest := @P[1];
  if P[1] = #0 then
    PDest := nil;
end;

function GotoEndOfJSONString(P: PUTF8Char; tab: PJsonCharSet): PUTF8Char;
begin // P^='"' at function call
  inc(P);
  repeat
    if not (jcJSONStringMarker in tab[P^]) then begin
      inc(P);
      continue; // very fast parsing of most UTF-8 chars
    end;
    // now c in [#0, '"', '\']
    if (P^ = '"') or (P^ = #0) or (P[1] = #0) then
      break; // end of string/buffer, or buffer overflow detected as \#0
    inc(P, 2); // c was '\' -> ignore \#
  until false;
  result := P;
end; // P^='"' at function return (if input was correct)

function GotoEndOfJSONString(P: PUTF8Char): PUTF8Char;
begin
  result := GotoEndOfJSONString(P, @JSON_CHARS);
end;

function GetJSONPropName(var P: PUTF8Char; Len: PInteger): PUTF8Char;
var
  Name: PUTF8Char;
  wasString: boolean;
  c, EndOfObject: AnsiChar;
  tab: PJsonCharSet;
begin // should match GotoNextJSONObjectOrArray() and JsonPropNameValid()
  result := nil;
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  Name := P; // put here to please some versions of Delphi compiler
  c := P^;
  if c = '"' then
  begin
    Name := GetJSONField(P, P, @wasString, @EndOfObject, Len);
    if (Name = nil) or not wasString or (EndOfObject <> ':') then
      exit;
  end
  else if c = '''' then
  begin // single quotes won't handle nested quote character
    inc(P);
    Name := P;
    while P^ <> '''' do
      if P^ < ' ' then
        exit
      else
        inc(P);
    if Len <> nil then
      Len^ := P - Name;
    P^ := #0;
    repeat
      inc(P)
    until (P^ > ' ') or (P^ = #0);
    if P^ <> ':' then
      exit;
    inc(P);
  end
  else
  begin // e.g. '{age:{$gt:18}}'
    tab := @JSON_CHARS;
    if not (jcJsonIdentifierFirstChar in tab[c]) then
      exit;
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    if Len <> nil then
      Len^ := P - Name;
    if (P^ <= ' ') and (P^ <> #0) then
    begin
      P^ := #0;
      inc(P);
    end;
    while (P^ <= ' ') and (P^ <> #0) do
      inc(P);
    if not (P^ in [':', '=']) then // allow both age:18 and age=18 pairs
      exit;
    P^ := #0;
    inc(P);
  end;
  result := Name;
end;

procedure GetJSONPropName(var P: PUTF8Char; out PropName: shortstring);
var
  Name: PAnsiChar;
  c: AnsiChar;
  tab: PJsonCharSet;
begin // match GotoNextJSONObjectOrArray() and overloaded GetJSONPropName()
  PropName[0] := #0;
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  Name := pointer(P);
  c := P^;
  if c = '"' then
  begin
    inc(Name);
    P := GotoEndOfJSONString(P, @JSON_CHARS);
    if P^ <> '"' then
      exit;
    SetString(PropName, Name, P - Name); // note: won't unescape JSON strings
    repeat
      inc(P)
    until (P^ > ' ') or (P^ = #0);
    if P^ <> ':' then
    begin
      PropName[0] := #0;
      exit;
    end;
    inc(P);
  end
  else if c = '''' then
  begin // single quotes won't handle nested quote character
    inc(P);
    inc(Name);
    while P^ <> '''' do
      if P^ < ' ' then
        exit
      else
        inc(P);
    SetString(PropName, Name, P - Name);
    repeat
      inc(P)
    until (P^ > ' ') or (P^ = #0);
    if P^ <> ':' then
    begin
      PropName[0] := #0;
      exit;
    end;
    inc(P);
  end
  else
  begin // e.g. '{age:{$gt:18}}'
    tab := @JSON_CHARS;
    if not (jcJsonIdentifierFirstChar in tab[c]) then
      exit;
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    SetString(PropName, Name, P - Name);
    while (P^ <= ' ') and (P^ <> #0) do
      inc(P);
    if not (P^ in [':', '=']) then
    begin // allow both age:18 and age=18 pairs
      PropName[0] := #0;
      exit;
    end;
    inc(P);
  end;
end;

function GotoNextJSONPropName(P: PUTF8Char): PUTF8Char;
var
  c: AnsiChar;
  tab: PJsonCharSet;
label
  s;
begin  // should match GotoNextJSONObjectOrArray()
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  result := nil;
  if P = nil then
    exit;
  c := P^;
  if c = '"' then
  begin
    P := GotoEndOfJSONString(P, @JSON_CHARS);
    if P^ <> '"' then
      exit;
s:  repeat
      inc(P)
    until (P^ > ' ') or (P^ = #0);
    if P^ <> ':' then
      exit;
  end
  else if c = '''' then
  begin // single quotes won't handle nested quote character
    inc(P);
    while P^ <> '''' do
      if P^ < ' ' then
        exit
      else
        inc(P);
    goto s;
  end
  else
  begin // e.g. '{age:{$gt:18}}'
    tab := @JSON_CHARS;
    if not (jcJsonIdentifierFirstChar in tab[c]) then
      exit;
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    if (P^ <= ' ') and (P^ <> #0) then
      inc(P);
    while (P^ <= ' ') and (P^ <> #0) do
      inc(P);
    if not (P^ in [':', '=']) then // allow both age:18 and age=18 pairs
      exit;
  end;
  repeat
    inc(P)
  until (P^ > ' ') or (P^ = #0);
  result := P;
end;


{ TValuePUTF8Char }

procedure TValuePUTF8Char.ToUTF8(var Text: RawUTF8);
begin
  FastSetString(Text, Value, ValueLen);
end;

function TValuePUTF8Char.ToUTF8: RawUTF8;
begin
  FastSetString(result, Value, ValueLen);
end;

function TValuePUTF8Char.ToString: string;
begin
  UTF8DecodeToString(Value, ValueLen, result);
end;

function TValuePUTF8Char.ToInteger: PtrInt;
begin
  result := GetInteger(Value);
end;

function TValuePUTF8Char.ToCardinal: PtrUInt;
begin
  result := GetCardinal(Value);
end;

function TValuePUTF8Char.Idem(const Text: RawUTF8): boolean;
begin
  if length(Text) = ValueLen then
    result := IdemPropNameUSameLen(pointer(Text), Value, ValueLen)
  else
    result := false;
end;


procedure JSONDecode(var JSON: RawUTF8; const Names: array of RawUTF8;
  Values: PValuePUTF8CharArray; HandleValuesAsObjectOrArray: Boolean);
begin
  JSONDecode(UniqueRawUTF8(JSON), Names, Values, HandleValuesAsObjectOrArray);
end;

procedure JSONDecode(var JSON: RawJSON; const Names: array of RawUTF8;
  Values: PValuePUTF8CharArray; HandleValuesAsObjectOrArray: Boolean);
begin
  JSONDecode(UniqueRawUTF8(RawUTF8(JSON)), Names, Values, HandleValuesAsObjectOrArray);
end;

function JSONDecode(P: PUTF8Char; const Names: array of RawUTF8;
  Values: PValuePUTF8CharArray; HandleValuesAsObjectOrArray: Boolean): PUTF8Char;
var
  n, i: PtrInt;
  namelen, valuelen: integer;
  name, value: PUTF8Char;
  EndOfObject: AnsiChar;
begin
  result := nil;
  if Values = nil then
    exit; // avoid GPF
  n := length(Names);
  FillCharFast(Values[0], n * SizeOf(Values[0]), 0);
  dec(n);
  if P = nil then
    exit;
  while P^ <> '{' do
    if P^ = #0 then
      exit
    else
      inc(P);
  inc(P); // jump {
  repeat
    name := GetJSONPropName(P, @namelen);
    if name = nil then
      exit;  // invalid JSON content
    value := GetJSONFieldOrObjectOrArray(P, nil, @EndOfObject,
      HandleValuesAsObjectOrArray, true, @valuelen);
    if not (EndOfObject in [',', '}']) then
      exit; // invalid item separator
    for i := 0 to n do
      if (Values[i].value = nil) and IdemPropNameU(Names[i], name, namelen) then
      begin
        Values[i].value := value;
        Values[i].valuelen := valuelen;
        break;
      end;
  until (P = nil) or (EndOfObject = '}');
  if P = nil then // result=nil indicates failure -> points to #0 for end of text
    result := @NULCHAR
  else
    result := P;
end;

function JSONDecode(var JSON: RawUTF8; const aName: RawUTF8; wasString: PBoolean;
  HandleValuesAsObjectOrArray: Boolean): RawUTF8;
var
  P, Name, Value: PUTF8Char;
  NameLen, ValueLen: integer;
  EndOfObject: AnsiChar;
begin
  result := '';
  P := pointer(JSON);
  if P = nil then
    exit;
  while P^ <> '{' do
    if P^ = #0 then
      exit
    else
      inc(P);
  inc(P); // jump {
  repeat
    Name := GetJSONPropName(P, @NameLen);
    if Name = nil then
      exit;  // invalid JSON content
    Value := GetJSONFieldOrObjectOrArray(P, wasString, @EndOfObject,
      HandleValuesAsObjectOrArray, true, @ValueLen);
    if not (EndOfObject in [',', '}']) then
      exit; // invalid item separator
    if IdemPropNameU(aName, Name, NameLen) then
    begin
      FastSetString(result, Value, ValueLen);
      exit;
    end;
  until (P = nil) or (EndOfObject = '}');
end;

function JSONDecode(P: PUTF8Char; out Values: TNameValuePUTF8CharDynArray;
  HandleValuesAsObjectOrArray: Boolean): PUTF8Char;
var
  n: PtrInt;
  field: TNameValuePUTF8Char;
  EndOfObject: AnsiChar;
begin
  {$ifdef FPC}
  Values := nil;
  {$endif}
  result := nil;
  n := 0;
  if P <> nil then
  begin
    while P^ <> '{' do
      if P^ = #0 then
        exit
      else
        inc(P);
    inc(P); // jump {
    repeat
      field.Name := GetJSONPropName(P, @field.NameLen);
      if field.Name = nil then
        exit;  // invalid JSON content
      field.Value := GetJSONFieldOrObjectOrArray(P, nil, @EndOfObject,
        HandleValuesAsObjectOrArray, true, @field.ValueLen);
      if not (EndOfObject in [',', '}']) then
        exit; // invalid item separator
      if n = length(Values) then
        SetLength(Values, n + 32);
      Values[n] := field;
      inc(n);
    until (P = nil) or (EndOfObject = '}');
  end;
  SetLength(Values, n);
  if P = nil then // result=nil indicates failure -> points to #0 for end of text
    result := @NULCHAR
  else
    result := P;
end;

function JSONRetrieveStringField(P: PUTF8Char; out Field: PUTF8Char;
  out FieldLen: integer; ExpectNameField: boolean): PUTF8Char;
begin
  result := nil;
  // retrieve string field
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  if P^ <> '"' then
    exit;
  Field := P + 1;
  P := GotoEndOfJSONString(P, @JSON_CHARS);
  if P^ <> '"' then
    exit; // here P^ should be '"'
  FieldLen := P - Field;
  // check valid JSON delimiter
  repeat
    inc(P)
  until (P^ > ' ') or (P^ = #0);
  if ExpectNameField then
  begin
    if P^ <> ':' then
      exit; // invalid name field
  end
  else if not (P^ in ['}', ',']) then
    exit; // invalid value field
  result := P; // return either ':' for name field, either '}',',' for value
end;

function GetJSONFieldOrObjectOrArray(var P: PUTF8Char; wasString: PBoolean;
  EndOfObject: PUTF8Char; HandleValuesAsObjectOrArray: Boolean;
  NormalizeBoolean: Boolean; Len: PInteger): PUTF8Char;
var
  Value: PUTF8Char;
  wStr: boolean;
begin
  result := nil;
  if P = nil then
    exit;
  while ord(P^) in [1..32] do
    inc(P);
  if HandleValuesAsObjectOrArray and (P^ in ['{', '[']) then
  begin
    Value := P;
    P := GotoNextJSONObjectOrArray(P);
    if P = nil then
      exit; // invalid content
    if Len <> nil then
      Len^ := P - Value;
    if wasString <> nil then
      wasString^ := false; // was object or array
    while ord(P^) in [1..32] do
      inc(P);
    if EndOfObject <> nil then
      EndOfObject^ := P^;
    P^ := #0; // make zero-terminated
    if P[1] = #0 then
      P := nil
    else
      inc(P);
    result := Value;
  end
  else
  begin
    result := GetJSONField(P, P, @wStr, EndOfObject, Len);
    if wasString <> nil then
      wasString^ := wStr;
    if not wStr and NormalizeBoolean and (result <> nil) then
    begin
      if PInteger(result)^ = TRUE_LOW then
        result := pointer(SmallUInt32UTF8[1])
      else // normalize true -> 1
      if PInteger(result)^ = FALSE_LOW then
        result := pointer(SmallUInt32UTF8[0])
      else  // normalize false -> 0
        exit;
      if Len <> nil then
        Len^ := 1;
    end;
  end;
end;

procedure GetJSONItemAsRawJSON(var P: PUTF8Char; var result: RawJSON;
  EndOfObject: PAnsiChar);
var
  B: PUTF8Char;
begin
  result := '';
  if P = nil then
    exit;
  B := GotoNextNotSpace(P);
  P := GotoEndJSONItem(B);
  if P = nil then
    exit;
  FastSetString(RawUTF8(result), B, P - B);
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  if EndOfObject <> nil then
    EndOfObject^ := P^;
  if P^ <> #0 then //if P^=',' then
    repeat
      inc(P)
    until (P^ > ' ') or (P^ = #0);
end;

function GetJSONItemAsRawUTF8(var P: PUTF8Char; var output: RawUTF8;
  wasString: PBoolean; EndOfObject: PUTF8Char): boolean;
var
  V: PUTF8Char;
  VLen: integer;
begin
  V := GetJSONFieldOrObjectOrArray(P, wasString, EndOfObject, true, true, @VLen);
  if V = nil then // parsing error
    result := false
  else
  begin
    FastSetString(output, V, VLen);
    result := true;
  end;
end;

function GotoNextJSONObjectOrArrayInternal(P, PMax: PUTF8Char; EndChar: AnsiChar): PUTF8Char;
var
  tab: PJsonCharSet;
label
  Prop;
begin // should match GetJSONPropName()
  result := nil;
  repeat
    case P^ of // quick parsing without full validation
      '{', '[':
        begin
          if PMax = nil then
            P := GotoNextJSONObjectOrArray(P)
          else
            P := GotoNextJSONObjectOrArrayMax(P, PMax);
          if P = nil then
            exit;
        end;
      ':':
        if EndChar <> '}' then
          exit
        else
          inc(P); // syntax for JSON object only
      ',':
        inc(P); // comma appears in both JSON objects and arrays
      '}':
        if EndChar = '}' then
          break
        else
          exit;
      ']':
        if EndChar = ']' then
          break
        else
          exit;
      '"':
        begin
          P := GotoEndOfJSONString(P, @JSON_CHARS);
          if P^ <> '"' then
            exit;
          inc(P);
        end;
      '-', '+', '0'..'9':
        begin // '0123' excluded by JSON, but not here
          tab := @JSON_CHARS;
          repeat
            inc(P);
          until not (jcDigitFloatChar in tab[P^]);
        end;
      't':
        if PInteger(P)^ = TRUE_LOW then
          inc(P, 4)
        else
          goto Prop;
      'f':
        if PInteger(P + 1)^ = FALSE_LOW2 then
          inc(P, 5)
        else
          goto Prop;
      'n':
        if PInteger(P)^ = NULL_LOW then
          inc(P, 4)
        else
          goto Prop;
      '''':
        begin // single-quoted identifier
          repeat
            inc(P);
            if P^ <= ' ' then
              exit;
          until P^ = '''';
          repeat
            inc(P)
          until (P^ > ' ') or (P^ = #0);
          if P^ <> ':' then
            exit;
        end;
      '/':
        begin
          repeat // allow extended /regex/ syntax
            inc(P);
            if P^ = #0 then
              exit;
          until P^ = '/';
          repeat
            inc(P)
          until (P^ > ' ') or (P^ = #0);
        end;
    else
      begin
Prop:   tab := @JSON_CHARS;
        if not (jcJsonIdentifierFirstChar in tab[P^]) then
          exit;
        repeat
          inc(P);
        until not (jcJsonIdentifier in tab[P^]);
        while (P^ <= ' ') and (P^ <> #0) do
          inc(P);
        if P^ = '(' then
        begin // handle e.g. "born":isodate("1969-12-31")
          inc(P);
          while (P^ <= ' ') and (P^ <> #0) do
            inc(P);
          if P^ = '"' then
          begin
            P := GotoEndOfJSONString(P, tab);
            if P^ <> '"' then
              exit;
          end;
          inc(P);
          while (P^ <= ' ') and (P^ <> #0) do
            inc(P);
          if P^ <> ')' then
            exit;
          inc(P);
        end
        else if P^ <> ':' then
          exit;
      end;
    end;
    while (P^ <= ' ') and (P^ <> #0) do
      inc(P);
    if (PMax <> nil) and (P >= PMax) then
      exit;
  until P^ = EndChar;
  result := P + 1;
end;

function GotoEndJSONItemStrict(P: PUTF8Char): PUTF8Char;
var
  tab: PJsonCharSet;
label
  pok, ok;
begin
  result := nil; // to notify unexpected end
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  case P^ of
    // complex JSON string, object or array
    '"':
      begin
        P := GotoEndOfJSONString(P, @JSON_CHARS);
        if P^ <> '"' then
          exit;
        inc(P);
        goto ok;
      end;
    '[':
      begin
        repeat
          inc(P)
        until (P^ > ' ') or (P^ = #0);
        P := GotoNextJSONObjectOrArrayInternal(P, nil, ']');
        goto pok;
      end;
    '{':
      begin
        repeat
          inc(P)
        until (P^ > ' ') or (P^ = #0);
        P := GotoNextJSONObjectOrArrayInternal(P, nil, '}');
pok:    if P = nil then
          exit;
ok:     while (P^ <= ' ') and (P^ <> #0) do
          inc(P);
        result := P;
        exit;
      end;
    // strict JSON numbers and constants validation
    't':
      if PInteger(P)^ = TRUE_LOW then
      begin
        inc(P, 4);
        goto ok;
      end;
    'f':
      if PInteger(P + 1)^ = FALSE_LOW2 then
      begin
        inc(P, 5);
        goto ok;
      end;
    'n':
      if PInteger(P)^ = NULL_LOW then
      begin
        inc(P, 4);
        goto ok;
      end;
    '-', '+', '0'..'9':
      begin
        tab := @JSON_CHARS;
        repeat
          inc(P)
        until not (jcDigitFloatChar in tab[P^]);
        goto ok;
      end;
  end;
end;

function GotoEndJSONItem(P: PUTF8Char): PUTF8Char;
var
  tab: PJsonCharSet;
label
  pok, ok;
begin
  result := nil; // to notify unexpected end
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  // handle complex JSON string, object or array
  case P^ of
    '"':
      begin
        P := GotoEndOfJSONString(P, @JSON_CHARS);
        if P^ <> '"' then
          exit;
        inc(P);
        goto ok;
      end;
    '[':
      begin
        repeat
          inc(P)
        until (P^ > ' ') or (P^ = #0);
        P := GotoNextJSONObjectOrArrayInternal(P, nil, ']');
        goto pok;
      end;
    '{':
      begin
        repeat
          inc(P)
        until (P^ > ' ') or (P^ = #0);
        P := GotoNextJSONObjectOrArrayInternal(P, nil, '}');
pok:    if P = nil then
          exit;
ok:     while (P^ <= ' ') and (P^ <> #0) do
          inc(P);
        result := P;
        exit;
      end;
  end;
  // quick ignore numeric or true/false/null or MongoDB extended {age:{$gt:18}}
  tab := @JSON_CHARS;
  if jcEndOfJSONFieldOr0 in tab[P^] then
    exit; // no value
  repeat
    inc(P);
  until jcEndOfJSONFieldOr0 in tab[P^];
  if P^ = #0 then
    exit; // unexpected end
  result := P;
end;

function GotoNextJSONItem(P: PUTF8Char; NumberOfItemsToJump: cardinal;
  EndOfObject: PAnsiChar): PUTF8Char;
begin
  result := nil; // to notify unexpected end
  while NumberOfItemsToJump <> 0 do
  begin
    P := GotoEndJSONItem(P);
    if P = nil then
      exit;
    inc(P); // ignore jcEndOfJSONField
    dec(NumberOfItemsToJump);
  end;
  if EndOfObject <> nil then
    EndOfObject^ := P[-1];
  result := P;
end;

function GotoNextJSONObjectOrArray(P: PUTF8Char): PUTF8Char;
var
  EndChar: AnsiChar;
begin // should match GetJSONPropName()
  result := nil; // mark error or unexpected end (#0)
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  case P^ of
    '[':
      EndChar := ']';
    '{':
      EndChar := '}';
  else
    exit;
  end;
  repeat
    inc(P)
  until (P^ > ' ') or (P^ = #0);
  result := GotoNextJSONObjectOrArrayInternal(P, nil, EndChar);
end;

function GotoNextJSONObjectOrArray(P: PUTF8Char; EndChar: AnsiChar): PUTF8Char;
begin // should match GetJSONPropName()
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  result := GotoNextJSONObjectOrArrayInternal(P, nil, EndChar);
end;

function GotoNextJSONObjectOrArrayMax(P, PMax: PUTF8Char): PUTF8Char;
var
  EndChar: AnsiChar;
begin // should match GetJSONPropName()
  result := nil; // mark error or unexpected end (#0)
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  case P^ of
    '[':
      EndChar := ']';
    '{':
      EndChar := '}';
  else
    exit;
  end;
  repeat
    inc(P)
  until (P^ > ' ') or (P^ = #0);
  result := GotoNextJSONObjectOrArrayInternal(P, PMax, EndChar);
end;

function JSONArrayCount(P: PUTF8Char): integer;
var
  n: integer;
begin
  result := -1;
  n := 0;
  P := GotoNextNotSpace(P);
  if P^ <> ']' then
    repeat
      P := GotoEndJSONItem(P);
      if P = nil then
        exit; // invalid content, or #0 reached
      if P^ <> ',' then
        break;
      inc(P);
    until false;
  if P^ = ']' then
    result := n;
end;

function JSONArrayDecode(P: PUTF8Char; out Values: TPUTF8CharDynArray): boolean;
var
  n, max: integer;
begin
  result := false;
  max := 0;
  n := 0;
  P := GotoNextNotSpace(P);
  if P^ <> ']' then
    repeat
      if max = n then
      begin
        max := NextGrow(max);
        SetLength(Values, max);
      end;
      Values[n] := P;
      P := GotoEndJSONItem(P);
      if P = nil then
        exit; // invalid content, or #0 reached
      if P^ <> ',' then
        break;
      inc(P);
    until false;
  if P^ = ']' then
  begin
    SetLength(Values, n);
    result := true;
  end
  else
    Values := nil;
end;

function JSONArrayItem(P: PUTF8Char; Index: integer): PUTF8Char;
begin
  if P <> nil then
  begin
    P := GotoNextNotSpace(P);
    if P^ = '[' then
    begin
      P := GotoNextNotSpace(P + 1);
      if P^ <> ']' then
        repeat
          if Index <= 0 then
          begin
            result := P;
            exit;
          end;
          P := GotoEndJSONItem(P);
          if (P = nil) or (P^ <> ',') then
            break; // invalid content or #0 reached
          inc(P);
          dec(Index);
        until false;
    end;
  end;
  result := nil;
end;

function JSONArrayCount(P, PMax: PUTF8Char): integer;
var
  n: integer;
begin
  result := -1;
  n := 0;
  P := GotoNextNotSpace(P);
  if P^ <> ']' then
    while P < PMax do
    begin
      case P^ of
        '"':
          begin // no PMax check within string (expect somewhat small)
            P := GotoEndOfJSONString(P, @JSON_CHARS);
            if P^ <> '"' then
              exit;
            inc(P);
          end;
        '{', '[':
          begin
            P := GotoNextJSONObjectOrArrayMax(P, PMax);
            if P = nil then
              exit; // invalid content or PMax reached
          end;
      end;
      while not (P^ in [#0, ',', ']']) do
        inc(P);
      inc(n);
      if P^ <> ',' then
        break;
      repeat
        inc(P)
      until (P^ > ' ') or (P^ = #0);
    end;
  if P^ = ']' then
    result := n;
end;

function JSONObjectPropCount(P: PUTF8Char): integer;
var
  n: integer;
begin
  result := -1;
  n := 0;
  P := GotoNextNotSpace(P);
  if P^ <> '}' then
    repeat
      P := GotoNextJSONPropName(P);
      if P = nil then
        exit; // invalid field name
      P := GotoEndJSONItem(P);
      if P = nil then
        exit; // invalid content, or #0 reached
      inc(n);
      if P^ <> ',' then
        break;
      inc(P);
    until false;
  if P^ = '}' then
    result := n;
end;

function JsonObjectItem(P: PUTF8Char; const PropName: RawUTF8;
  PropNameFound: PRawUTF8): PUTF8Char;
var
  name: shortstring; // no memory allocation nor P^ modification
  PropNameLen: integer;
  PropNameUpper: array[byte] of AnsiChar;
begin
  if P <> nil then
  begin
    P := GotoNextNotSpace(P);
    PropNameLen := length(PropName);
    if PropNameLen <> 0 then
    begin
      if PropName[PropNameLen] = '*' then
      begin
        UpperCopy255Buf(PropNameUpper{%H-},
          pointer(PropName), PropNameLen - 1)^ := #0;
        PropNameLen := 0;
      end;
      if P^ = '{' then
        P := GotoNextNotSpace(P + 1);
      if P^ <> '}' then
        repeat
          GetJSONPropName(P, name);
          if (name[0] = #0) or (name[0] > #200) then
            break;
          while (P^ <= ' ') and (P^ <> #0) do
            inc(P);
          if PropNameLen = 0 then // 'PropName*'
          begin
            name[ord(name[0]) + 1] := #0; // make ASCIIZ
            if IdemPChar(@name[1], PropNameUpper) then
            begin
              if PropNameFound <> nil then
                FastSetString(PropNameFound^, @name[1], ord(name[0]));
              result := P;
              exit;
            end;
          end
          else if IdemPropName(name, pointer(PropName), PropNameLen) then
          begin
            result := P;
            exit;
          end;
          P := GotoEndJSONItem(P);
          if (P = nil) or (P^ <> ',') then
            break; // invalid content, or #0 reached
          inc(P);
        until false;
    end;
  end;
  result := nil;
end;

function JsonObjectByPath(JsonObject, PropPath: PUTF8Char): PUTF8Char;
var
  objName: RawUTF8;
begin
  result := nil;
  if (JsonObject = nil) or (PropPath = nil) then
    exit;
  repeat
    GetNextItem(PropPath, '.', objName);
    if objName = '' then
      exit;
    JsonObject := JsonObjectItem(JsonObject, objName);
    if JsonObject = nil then
      exit;
  until PropPath = nil; // found full name scope
  result := JsonObject;
end;

function JsonObjectsByPath(JsonObject, PropPath: PUTF8Char): RawUTF8;
var
  itemName, objName, propNameFound, objPath: RawUTF8;
  start, ending, obj: PUTF8Char;
  WR: TBaseWriter;
  temp: TTextWriterStackBuffer;

  procedure AddFromStart(const name: RaWUTF8);
  begin
    start := GotoNextNotSpace(start);
    ending := GotoEndJSONItem(start);
    if ending = nil then
      exit;
    if WR = nil then
    begin
      WR := TBaseWriter.CreateOwnedStream(temp);
      WR.Add('{');
    end
    else
      WR.Add(',');
    WR.AddFieldName(name);
    while (ending > start) and (ending[-1] <= ' ') do
      dec(ending); // trim right
    WR.AddNoJSONEscape(start, ending - start);
  end;

begin
  result := '';
  if (JsonObject = nil) or (PropPath = nil) then
    exit;
  WR := nil;
  try
    repeat
      GetNextItem(PropPath, ',', itemName);
      if itemName = '' then
        break;
      if itemName[length(itemName)] <> '*' then
      begin
        start := JsonObjectByPath(JsonObject, pointer(itemName));
        if start <> nil then
          AddFromStart(itemName);
      end
      else
      begin
        objPath := '';
        obj := pointer(itemName);
        repeat
          GetNextItem(obj, '.', objName);
          if objName = '' then
            exit;
          propNameFound := '';
          JsonObject := JsonObjectItem(JsonObject, objName, @propNameFound);
          if JsonObject = nil then
            exit;
          if obj = nil then
          begin // found full name scope
            start := JsonObject;
            repeat
              AddFromStart(objPath + propNameFound);
              ending := GotoNextNotSpace(ending);
              if ending^ <> ',' then
                break;
              propNameFound := '';
              start := JsonObjectItem(GotoNextNotSpace(ending + 1), objName, @propNameFound);
            until start = nil;
            break;
          end
          else
            objPath := objPath + objName + '.';
        until false;
      end;
    until PropPath = nil;
    if WR <> nil then
    begin
      WR.Add('}');
      WR.SetText(result);
    end;
  finally
    WR.Free;
  end;
end;

function JSONObjectAsJSONArrays(JSON: PUTF8Char; out keys, values: RawUTF8): boolean;
var
  wk, wv: TBaseWriter;
  kb, ke, vb, ve: PUTF8Char;
  temp1, temp2: TTextWriterStackBuffer;
begin
  result := false;
  if (JSON = nil) or (JSON^ <> '{') then
    exit;
  wk := TBaseWriter.CreateOwnedStream(temp1);
  wv := TBaseWriter.CreateOwnedStream(temp2);
  try
    wk.Add('[');
    wv.Add('[');
    kb := JSON + 1;
    repeat
      ke := GotoEndJSONItem(kb);
      if (ke = nil) or (ke^ <> ':') then
        exit; // invalid input content
      vb := ke + 1;
      ve := GotoEndJSONItem(vb);
      if (ve = nil) or not (ve^ in [',', '}']) then
        exit;
      wk.AddNoJSONEscape(kb, ke - kb);
      wk.Add(',');
      wv.AddNoJSONEscape(vb, ve - vb);
      wv.Add(',');
      kb := ve + 1;
    until ve^ = '}';
    wk.CancelLastComma;
    wk.Add(']');
    wk.SetText(keys);
    wv.CancelLastComma;
    wv.Add(']');
    wv.SetText(values);
    result := true;
  finally
    wv.Free;
    wk.Free;
  end;
end;

procedure RemoveCommentsFromJSON(P: PUTF8Char);
begin // replace comments by ' ' characters which will be ignored by parser
  if P <> nil then
    while P^ <> #0 do
    begin
      case P^ of
        '"':
          begin
            P := GotoEndOfJSONString(P, @JSON_CHARS);
            if P^ <> '"' then
              exit;
          end;
        '/':
          begin
            inc(P);
            case P^ of
              '/':
                begin // this is // comment - replace by ' '
                  dec(P);
                  repeat
                    P^ := ' ';
                    inc(P)
                  until (P^ = #0) or (P^ = #10) or (P^ = #13);
                end;
              '*':
                begin // this is /* comment - replace by ' ' but keep CRLF
                  P[-1] := ' ';
                  repeat
                    if (P^ <> #10) and (P^ <> #13) then
                      // keep CRLF for correct line numbering (e.g. for error)
                      P^ := ' ';
                    inc(P);
                    if PWord(P)^ = ord('*') + ord('/') shl 8 then
                    begin
                      PWord(P)^ := $2020;
                      inc(P, 2);
                      break;
                    end;
                  until P^ = #0;
                end;
            end;
          end;
      end;
      inc(P);
    end;
end;

function RemoveCommentsFromJSON(const s: RawUTF8): RawUTF8;
begin
  if PosExChar('/', s) = 0 then
    result := s
  else
  begin
    FastSetString(result, pointer(s), length(s));
    RemoveCommentsFromJSON(pointer(s)); // remove in-place
  end;
end;


function UrlEncodeJsonObject(const URIName: RawUTF8; ParametersJSON: PUTF8Char;
  const PropNamesToIgnore: array of RawUTF8; IncludeQueryDelimiter: Boolean): RawUTF8;
var
  i, j: integer;
  sep: AnsiChar;
  Params: TNameValuePUTF8CharDynArray;
  temp: TTextWriterStackBuffer;
begin
  if ParametersJSON = nil then
    result := URIName
  else
    with TBaseWriter.CreateOwnedStream(temp) do
    try
      AddString(URIName);
      if (JSONDecode(ParametersJSON, Params, true) <> nil) and (Params <> nil) then
      begin
        sep := '?';
        for i := 0 to length(Params) - 1 do
          with Params[i] do
          begin
            for j := 0 to high(PropNamesToIgnore) do
              if IdemPropNameU(PropNamesToIgnore[j], name, NameLen) then
              begin
                NameLen := 0;
                break;
              end;
            if NameLen = 0 then
              continue;
            if IncludeQueryDelimiter then
              Add(sep);
            AddNoJSONEscape(name, NameLen);
            Add('=');
            AddString(UrlEncode(Value));
            sep := '&';
            IncludeQueryDelimiter := true;
          end;
      end;
      SetText(result);
    finally
      Free;
    end;
end;

function UrlEncodeJsonObject(const URIName, ParametersJSON: RawUTF8;
  const PropNamesToIgnore: array of RawUTF8; IncludeQueryDelimiter: Boolean): RawUTF8;
var
  temp: TSynTempBuffer;
begin
  temp.Init(ParametersJSON);
  try
    result := UrlEncodeJsonObject(URIName, temp.buf, PropNamesToIgnore, IncludeQueryDelimiter);
  finally
    temp.Done;
  end;
end;

procedure QuotedStrJSON(P: PUTF8Char; PLen: PtrInt; var result: RawUTF8;
  const aPrefix, aSuffix: RawUTF8);
var
  temp: TTextWriterStackBuffer;
  Lp, Ls: PtrInt;
  D: PUTF8Char;
begin
  if (P = nil) or (PLen <= 0) then
    result := '""'
  else if (pointer(result) = pointer(P)) or NeedsJsonEscape(P, PLen) then
    with TTextWriter.CreateOwnedStream(temp) do
    try
      AddString(aPrefix);
      Add('"');
      AddJSONEscape(P, PLen);
      Add('"');
      AddString(aSuffix);
      SetText(result);
      exit;
    finally
      Free;
    end
  else
  begin
    Lp := length(aPrefix);
    Ls := length(aSuffix);
    FastSetString(result, nil, PLen + Lp + Ls + 2);
    D := pointer(result); // we checked dest result <> source P above
    if Lp > 0 then
    begin
      MoveFast(pointer(aPrefix)^, D^, Lp);
      inc(D, Lp);
    end;
    D^ := '"';
    MoveFast(P^, D[1], PLen);
    inc(D, PLen);
    D[1] := '"';
    if Ls > 0 then
      MoveFast(pointer(aSuffix)^, D[2], Ls);
  end;
end;

procedure QuotedStrJSON(const aText: RawUTF8; var result: RawUTF8;
  const aPrefix, aSuffix: RawUTF8);
begin
  QuotedStrJSON(pointer(aText), Length(aText), result, aPrefix, aSuffix);
end;

function QuotedStrJSON(const aText: RawUTF8): RawUTF8;
begin
  QuotedStrJSON(pointer(aText), Length(aText), result, '', '');
end;

function FormatUTF8(const Format: RawUTF8; const Args, Params: array of const;
  JSONFormat: boolean): RawUTF8;
var
  i, tmpN, L, A, P, len: PtrInt;
  isParam: AnsiChar;
  tmp: TRawUTF8DynArray;
  inlin: set of 0..255;
  F, FDeb: PUTF8Char;
  wasString: Boolean;
const
  NOTTOQUOTE: array[boolean] of set of 0..31 = (
    [vtBoolean, vtInteger, vtInt64 {$ifdef FPC} , vtQWord {$endif},
     vtCurrency, vtExtended],
    [vtBoolean, vtInteger, vtInt64 {$ifdef FPC} , vtQWord {$endif},
     vtCurrency,vtExtended,vtVariant]);
label
  Txt;
begin
  result := '';
  if Format = '' then
    exit;
  if (high(Args) < 0) and (high(Params) < 0) then
  begin // no formatting to process, but may be a const -> make unique
    FastSetString(result, pointer(Format), length(Format));
    exit; // e.g. _JsonFmt() will parse it in-place
  end;
  if high(Params) < 0 then
  begin
    FormatUTF8(Format, Args, result); // faster function with no ?
    exit;
  end;
  if Format = '%' then
  begin
    VarRecToUTF8(Args[0], result); // optimize raw conversion
    exit;
  end;
  tmpN := 0;
  FillCharFast(inlin, SizeOf(inlin), 0);
  L := 0;
  A := 0;
  P := 0;
  F := pointer(Format);
  while F^ <> #0 do
  begin
    if F^ <> '%' then
    begin
      FDeb := F;
      while not (F^ in [#0, '%', '?']) do
        inc(F);
Txt:  len := F - FDeb;
      if len > 0 then
      begin
        inc(L, len);
        if tmpN = length({%H-}tmp) then
          SetLength(tmp, tmpN + 8);
        FastSetString(tmp[tmpN], FDeb, len); // add inbetween text
        inc(tmpN);
      end;
    end;
    if F^ = #0 then
      break;
    isParam := F^;
    inc(F); // jump '%' or '?'
    if (isParam = '%') and (A <= high(Args)) then
    begin // handle % substitution
      if tmpN = length(tmp) then
        SetLength(tmp, tmpN + 8);
      VarRecToUTF8(Args[A], tmp[tmpN]);
      inc(A);
      if tmp[tmpN] <> '' then
      begin
        inc(L, length(tmp[tmpN]));
        inc(tmpN);
      end;
    end
    else if (isParam = '?') and (P <= high(Params)) then
    begin // handle ? substitution
      if tmpN = length(tmp) then
        SetLength(tmp, tmpN + 8);
      if JSONFormat and (Params[P].VType = vtVariant) then
        VariantSaveJSON(Params[P].VVariant^, twJSONEscape, tmp[tmpN])
      else
      begin
        VarRecToUTF8(Params[P], tmp[tmpN]);
        wasString := not (Params[P].VType in NOTTOQUOTE[JSONFormat]);
        if wasString then
          if JSONFormat then
            QuotedStrJSON(tmp[tmpN], tmp[tmpN])
          else
            tmp[tmpN] := QuotedStr(tmp[tmpN], '''');
        if not JSONFormat then
        begin
          inc(L, 4); // space for :():
          include(inlin, tmpN);
        end;
      end;
      inc(P);
      inc(L, length(tmp[tmpN]));
      inc(tmpN);
    end
    else if F^ <> #0 then
    begin // no more available Args -> add all remaining text
      FDeb := F;
      repeat
        inc(F)
      until (F^ = #0);
      goto Txt;
    end;
  end;
  if L = 0 then
    exit;
  if not JSONFormat and (tmpN > SizeOf(inlin) shl 3) then
    raise ESynException.CreateUTF8('Too many parameters for FormatUTF8(): %>%',
      [tmpN, SizeOf(inlin) shl 3]);
  FastSetString(result, nil, L);
  F := pointer(result);
  for i := 0 to tmpN - 1 do
    if tmp[i] <> '' then
    begin
      if byte(i) in inlin then
      begin
        PWord(F)^ := ord(':') + ord('(') shl 8;
        inc(F, 2);
      end;
      L := PStrLen(PAnsiChar(pointer(tmp[i])) - _STRLEN)^;
      MoveFast(pointer(tmp[i])^, F^, L);
      inc(F, L);
      if byte(i) in inlin then
      begin
        PWord(F)^ := ord(')') + ord(':') shl 8;
        inc(F, 2);
      end;
    end;
end;



{ ********** TTextWriter class with proper JSON escaping and WriteObject() support }

procedure TTextWriter.InternalAddFixedAnsi(Source: PAnsiChar; SourceChars: Cardinal;
  AnsiToWide: PWordArray; Escape: TTextWriterKind);
var
  c: cardinal;
  esc: byte;
begin
  while SourceChars > 0 do
  begin
    c := byte(Source^);
    if c <= $7F then
    begin
      if c = 0 then
        exit;
      if B >= BEnd then
        FlushToStream;
      case Escape of // twJSONEscape or twOnSameLine only occur on c <= $7f
        twNone:
          begin
            inc(B);
            B^ := AnsiChar(c);
          end;
        twJSONEscape:
          begin
            esc := JSON_ESCAPE[c];
            if esc = JSON_ESCAPE_NONE then
            begin // no escape needed
              inc(B);
              B^ := AnsiChar(c);
            end
            else if esc = JSON_ESCAPE_ENDINGZERO then // #0
              exit
            else if esc = JSON_ESCAPE_UNICODEHEX then
            begin // characters below ' ', #7 e.g. -> \u0007
              AddShort('\u00');
              AddByteToHex(c);
            end
            else
              Add('\', AnsiChar(esc)); // escaped as \ + b,t,n,f,r,\,"
          end;
        twOnSameLine:
          begin
            inc(B);
            if c < 32 then
              B^ := ' '
            else
              B^ := AnsiChar(c);
          end;
      end
    end
    else
    begin // no surrogate is expected in TSynAnsiFixedWidth charsets
      if BEnd - B <= 3 then
        FlushToStream;
      c := AnsiToWide[c]; // convert FixedAnsi char into Unicode char
      if c > $7ff then
      begin
        B[1] := AnsiChar($E0 or (c shr 12));
        B[2] := AnsiChar($80 or ((c shr 6) and $3F));
        B[3] := AnsiChar($80 or (c and $3F));
        inc(B, 3);
      end
      else
      begin
        B[1] := AnsiChar($C0 or (c shr 6));
        B[2] := AnsiChar($80 or (c and $3F));
        inc(B, 2);
      end;
    end;
    dec(SourceChars);
    inc(Source);
  end;
end;

procedure TTextWriter.Add(P: PUTF8Char; Escape: TTextWriterKind);
begin
  if P <> nil then
    case Escape of
      twNone:
        AddNoJSONEscape(P, StrLen(P));
      twJSONEscape:
        AddJSONEscape(P);
      twOnSameLine:
        AddOnSameLine(P);
    end;
end;

procedure TTextWriter.Add(P: PUTF8Char; Len: PtrInt; Escape: TTextWriterKind);
begin
  if P <> nil then
    case Escape of
      twNone:
        AddNoJSONEscape(P, Len);
      twJSONEscape:
        AddJSONEscape(P, Len);
      twOnSameLine:
        AddOnSameLine(P, Len);
    end;
end;

procedure TTextWriter.AddW(P: PWord; Len: PtrInt; Escape: TTextWriterKind);
begin
  if P <> nil then
    case Escape of
      twNone:
        AddNoJSONEscapeW(P, Len);
      twJSONEscape:
        AddJSONEScapeW(P, Len);
      twOnSameLine:
        AddOnSameLineW(P, Len);
    end;
end;

procedure TTextWriter.AddAnsiString(const s: AnsiString; Escape: TTextWriterKind);
begin
  AddAnyAnsiBuffer(pointer(s), length(s), Escape, 0);
end;

procedure TTextWriter.AddAnyAnsiString(const s: RawByteString;
  Escape: TTextWriterKind; CodePage: Integer);
var
  L: integer;
begin
  L := length(s);
  if L = 0 then
    exit;
  if (L > 2) and (PInteger(s)^ and $ffffff = JSON_BASE64_MAGIC) then
  begin
    AddNoJSONEscape(pointer(s), L); // was marked as a BLOB content
    exit;
  end;
  if CodePage < 0 then
    {$ifdef HASCODEPAGE}
    CodePage := StringCodePage(s);
    {$else}
    CodePage := 0; // TSynAnsiConvert.Engine(0)=CurrentAnsiConvert
    {$endif}
  AddAnyAnsiBuffer(pointer(s), L, Escape, CodePage);
end;

procedure EngineAppendUTF8(W: TTextWriter; Engine: TSynAnsiConvert;
  P: PAnsiChar; Len: PtrInt; Escape: TTextWriterKind);
var
  tmp: TSynTempBuffer;
begin // explicit conversion using a temporary buffer on stack
  Len := Engine.AnsiBufferToUTF8(tmp.Init(Len*3), P, Len) - PUTF8Char({%H-}tmp.buf);
  W.Add(tmp.buf,Len,Escape);
  tmp.Done;
end;

procedure TTextWriter.AddAnyAnsiBuffer(P: PAnsiChar; Len: PtrInt;
  Escape: TTextWriterKind; CodePage: Integer);
var
  B: PUTF8Char;
  engine: TSynAnsiConvert;
begin
  if Len > 0 then
    case CodePage of
      CP_UTF8, CP_RAWBYTESTRING: // direct write of RawUTF8/RawByteString
        Add(PUTF8Char(P), Len, Escape);
      CP_UTF16:                  // direct write of UTF-16 content
        AddW(PWord(P), 0, Escape);
      CP_SQLRAWBLOB:             // TSQLRawBlob written with Base-64 encoding
        begin
          AddNoJSONEscape(@PByteArray(@JSON_BASE64_MAGIC_QUOTE_VAR)[1], 3);
          WrBase64(P, Len, {withMagic=}false);
        end;
    else
      begin
        // first handle trailing 7 bit ASCII chars, by quad
        B := pointer(P);
        if Len >= 4 then
          repeat
            if PCardinal(P)^ and $80808080 <> 0 then
              break; // break on first non ASCII quad
            inc(P, 4);
            dec(Len, 4);
          until Len < 4;
        if (Len > 0) and (P^ < #128) then
          repeat
            inc(P);
            dec(Len);
          until (Len = 0) or (P^ >= #127);
        if P <> pointer(B) then
          Add(B, P - B, Escape);
        if Len <= 0 then
          exit;
        // rely on explicit conversion for all remaining ASCII characters
        engine := TSynAnsiConvert.Engine(CodePage);
        if engine.InheritsFrom(TSynAnsiFixedWidth) then
          InternalAddFixedAnsi(P, Len,
            pointer(TSynAnsiFixedWidth(engine).AnsiToWide), Escape)
        else
          EngineAppendUTF8(self, engine, P, Len, Escape);
      end;
    end;
end;


procedure TTextWriter.WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean);
var
  trailing, main, n: PtrUInt;
begin
  if withMagic then
    if Len <= 0 then
    begin
      AddShort('null'); // JSON null is better than "" for BLOBs
      exit;
    end
    else
      AddNoJSONEscape(@JSON_BASE64_MAGIC_QUOTE_VAR, 4);
  if Len > 0 then
  begin
    n := Len div 3;
    trailing := Len - n * 3;
    dec(Len, trailing);
    if BEnd - B > integer(n + 1) shl 2 then
    begin
      // will fit in available space in Buf -> fast in-buffer Base64 encoding
      n := Base64EncodeMain(@B[1], P, Len);
      inc(B, n * 4);
      inc(P, n * 3);
    end
    else
    begin
      // bigger than available space in Buf -> do it per chunk
      FlushToStream;
      while Len > 0 do
      begin // length(buf) const -> so is ((length(buf)-4)shr2 )*3
        n := ((fTempBufSize - 4) shr 2) * 3;
        if Len < n then
          n := Len;
        main := Base64EncodeMain(PAnsiChar(fTempBuf), P, n);
        n := main * 4;
        if n < cardinal(fTempBufSize) - 4 then
          inc(B, n)
        else
        begin
          fStream.WriteBuffer(fTempBuf^, n);
          inc(fTotalFileSize, n);
        end;
        n := main * 3;
        inc(P, n);
        dec(Len, n);
      end;
    end;
    if trailing > 0 then
    begin
      Base64EncodeTrailing(@B[1], P, trailing);
      inc(B, 4);
    end;
  end;
  if withMagic then
    Add('"');
end;

procedure TTextWriter.Add(const Format: RawUTF8; const Values: array of const;
  Escape: TTextWriterKind; WriteObjectOptions: TTextWriterWriteObjectOptions);
var
  ValuesIndex: integer;
  S, F: PUTF8Char;
begin
  if Format = '' then
    exit;
  if (Format = '%') and (high(Values) >= 0) then
  begin
    Add(Values[0], Escape);
    exit;
  end;
  ValuesIndex := 0;
  F := pointer(Format);
  repeat
    S := F;
    repeat
      if (F^ = #0) or (F^ = '%') then
        break;
      inc(F);
    until false;
    AddNoJSONEscape(S, F - S);
    if F^ = #0 then
      exit;
    // add next value as text instead of F^='%' placeholder
    if ValuesIndex <= high(Values) then // missing value will display nothing
      Add(Values[ValuesIndex], Escape, WriteObjectOptions);
    inc(F);
    inc(ValuesIndex);
  until false;
end;

procedure TTextWriter.Add(const Values: array of const);
var
  i: PtrInt;
begin
  for i := 0 to high(Values) do
    AddJSONEscape(Values[i]);
end;

procedure TTextWriter.AddTimeLog(Value: PInt64);
begin
  if BEnd - B <= 31 then
    FlushToStream;
  inc(B, PTimeLogBits(Value)^.Text(B + 1, true, 'T'));
end;

procedure TTextWriter.AddUnixTime(Value: PInt64);
begin // inlined UnixTimeToDateTime()
  AddDateTime(Value^ / SecsPerDay + UnixDateDelta);
end;

procedure TTextWriter.AddUnixMSTime(Value: PInt64; WithMS: boolean);
begin // inlined UnixMSTimeToDateTime()
  AddDateTime(Value^ / MSecsPerDay + UnixDateDelta, WithMS);
end;

procedure TTextWriter.AddDateTime(Value: PDateTime; FirstChar: AnsiChar;
  QuoteChar: AnsiChar; WithMS: boolean);
begin
  if (Value^ = 0) and (QuoteChar = #0) then
    exit;
  if BEnd - B <= 25 then
    FlushToStream;
  inc(B);
  if QuoteChar <> #0 then
    B^ := QuoteChar
  else
    dec(B);
  if Value^ <> 0 then
  begin
    inc(B);
    if trunc(Value^) <> 0 then
    begin
      DateToIso8601PChar(Value^, B, true);
      inc(B, 10);
    end;
    if frac(Value^) <> 0 then
    begin
      TimeToIso8601PChar(Value^, B, true, FirstChar, WithMS);
      if WithMS then
        inc(B, 13)
      else
        inc(B, 9);
    end;
    dec(B);
  end;
  if QuoteChar <> #0 then
  begin
    inc(B);
    B^ := QuoteChar;
  end;
end;

procedure TTextWriter.AddDateTime(const Value: TDateTime; WithMS: boolean);
begin
  if Value = 0 then
    exit;
  if BEnd - B <= 23 then
    FlushToStream;
  inc(B);
  if trunc(Value) <> 0 then
  begin
    DateToIso8601PChar(Value, B, true);
    inc(B, 10);
  end;
  if frac(Value) <> 0 then
  begin
    TimeToIso8601PChar(Value, B, true, 'T', WithMS);
    if WithMS then
      inc(B, 13)
    else
      inc(B, 9);
  end;
  dec(B);
end;

procedure TTextWriter.AddDateTimeMS(const Value: TDateTime; Expanded: boolean;
  FirstTimeChar: AnsiChar; const TZD: RawUTF8);
var
  T: TSynSystemTime;
begin
  if Value = 0 then
    exit;
  T.FromDateTime(Value);
  Add(DTMS_FMT[Expanded], [UInt4DigitsToShort(T.Year), UInt2DigitsToShortFast(T.Month),
    UInt2DigitsToShortFast(T.Day), FirstTimeChar, UInt2DigitsToShortFast(T.Hour),
    UInt2DigitsToShortFast(T.Minute), UInt2DigitsToShortFast(T.Second),
    UInt3DigitsToShort(T.MilliSecond), TZD]);
end;

procedure TTextWriter.AddVariant(const Value: variant; Escape: TTextWriterKind);
var
  vt: cardinal;
begin
  vt := TVarData(Value).VType;
  with TVarData(Value) do
    case vt of
      varEmpty, varNull:
        AddShort('null');
      varSmallint:
        Add(VSmallint);
      varShortInt:
        Add(VShortInt);
      varByte:
        AddU(VByte);
      varWord:
        AddU(VWord);
      varLongWord:
        AddU(VLongWord);
      varInteger:
        Add(VInteger);
      varInt64:
        Add(VInt64);
      varWord64:
        AddQ(VInt64);
      varSingle:
        AddSingle(VSingle);
      varDouble:
        AddDouble(VDouble);
      varDate:
        AddDateTime(@VDate, 'T', '"');
      varCurrency:
        AddCurr64(VInt64);
      varBoolean:
        Add(VBoolean); // 'true'/'false'
      varVariant:
        AddVariant(PVariant(VPointer)^, Escape);
      varString:
        begin
          if Escape = twJSONEscape then
            Add('"');
          {$ifdef HASCODEPAGE}
          AddAnyAnsiString(RawByteString(VString), Escape);
          {$else}  // VString is expected to be a RawUTF8
          Add(VString, length(RawUTF8(VString)), Escape);
          {$endif}
          if Escape = twJSONEscape then
            Add('"');
        end;
      varOleStr {$ifdef HASVARUSTRING}, varUString{$endif}:
        begin
          if Escape = twJSONEscape then
            Add('"');
          AddW(VAny, 0, Escape);
          if Escape = twJSONEscape then
            Add('"');
        end;
    else
      if vt = varVariant or varByRef then
        AddVariant(PVariant(VPointer)^, Escape)
      else if vt = varByRef or varString then
      begin
        if Escape = twJSONEscape then
          Add('"');
        {$ifdef HASCODEPAGE}
        AddAnyAnsiString(PRawByteString(VAny)^, Escape);
        {$else}  // VString is expected to be a RawUTF8
        Add(PPointer(VAny)^, length(PRawUTF8(VAny)^), Escape);
        {$endif}
        if Escape = twJSONEscape then
          Add('"');
      end
      else if {$ifdef HASVARUSTRING} (vt = varByRef or varUString) or {$endif}
        (vt = varByRef or varOleStr) then
      begin
        if Escape = twJSONEscape then
          Add('"');
        AddW(PPointer(VAny)^, 0, Escape);
        if Escape = twJSONEscape then
          Add('"');
      end
      else if not CustomVariantToJSON(self, Value, Escape) then
        raise ESynException.CreateUTF8('%.AddVariant VType=%', [self, vt]);
    end;
end;

function TTextWriter.AddJSONReformat(JSON: PUTF8Char; Format: TTextWriterJSONFormat;
 EndOfObject: PUTF8Char): PUTF8Char;
var
  objEnd: AnsiChar;
  Name, Value: PUTF8Char;
  NameLen: integer;
  ValueLen: PtrInt;
  tab: PJsonCharSet;
begin
  result := nil;
  if JSON = nil then
    exit;
  while (JSON^ <= ' ') and (JSON^ <> #0) do
    inc(JSON);
  case JSON^ of
    '[':
      begin // array
        repeat
          inc(JSON)
        until (JSON^ = #0) or (JSON^ > ' ');
        if JSON^ = ']' then
        begin
          Add('[');
          inc(JSON);
        end
        else
        begin
          if not (Format in [jsonCompact, jsonUnquotedPropNameCompact]) then
            AddCRAndIndent;
          inc(fHumanReadableLevel);
          Add('[');
          repeat
            if JSON = nil then
              exit;
            if not (Format in [jsonCompact, jsonUnquotedPropNameCompact]) then
              AddCRAndIndent;
            JSON := AddJSONReformat(JSON, Format, @objEnd);
            if objEnd = ']' then
              break;
            Add(objEnd);
          until false;
          dec(fHumanReadableLevel);
          if not (Format in [jsonCompact, jsonUnquotedPropNameCompact]) then
            AddCRAndIndent;
        end;
        Add(']');
      end;
    '{':
      begin // object
        repeat
          inc(JSON)
        until (JSON^ = #0) or (JSON^ > ' ');
        Add('{');
        inc(fHumanReadableLevel);
        if not (Format in [jsonCompact, jsonUnquotedPropNameCompact]) then
          AddCRAndIndent;
        if JSON^ = '}' then
          repeat
            inc(JSON)
          until (JSON^ = #0) or (JSON^ > ' ')
        else
          repeat
            Name := GetJSONPropName(JSON, @NameLen);
            if Name = nil then
              exit;
            if (Format in [jsonUnquotedPropName, jsonUnquotedPropNameCompact]) and
               JsonPropNameValid(Name) then
              AddNoJSONEscape(Name, NameLen)
            else
            begin
              Add('"');
              AddJSONEscape(Name);
              Add('"');
            end;
            if Format in [jsonCompact, jsonUnquotedPropNameCompact] then
              Add(':')
            else
              Add(':', ' ');
            while (JSON^ <= ' ') and (JSON^ <> #0) do
              inc(JSON);
            JSON := AddJSONReformat(JSON, Format, @objEnd);
            if objEnd = '}' then
              break;
            Add(objEnd);
            if not (Format in [jsonCompact, jsonUnquotedPropNameCompact]) then
              AddCRAndIndent;
          until false;
        dec(fHumanReadableLevel);
        if not (Format in [jsonCompact, jsonUnquotedPropNameCompact]) then
          AddCRAndIndent;
        Add('}');
      end;
    '"':
      begin // string
        Value := JSON;
        JSON := GotoEndOfJSONString(JSON, @JSON_CHARS);
        if JSON^ <> '"' then
          exit;
        inc(JSON);
        AddNoJSONEscape(Value, JSON - Value);
      end;
  else
    begin // numeric value or true/false/null constant or MongoDB extended
      tab := @JSON_CHARS;
      if jcEndOfJSONFieldOr0 in tab[JSON^] then
        exit; // no value
      Value := JSON;
      ValueLen := 0;
      repeat
        inc(ValueLen);
      until jcEndOfJSONFieldOr0 in tab[JSON[ValueLen]];
      inc(JSON, ValueLen);
      while (ValueLen > 0) and (Value[ValueLen - 1] <= ' ') do
        dec(ValueLen);
      AddNoJSONEscape(Value, ValueLen);
    end;
  end;
  if JSON = nil then
    exit;
  while (JSON^ <= ' ') and (JSON^ <> #0) do
    inc(JSON);
  if EndOfObject <> nil then
    EndOfObject^ := JSON^;
  if JSON^ <> #0 then
    repeat
      inc(JSON)
    until (JSON^ = #0) or (JSON^ > ' ');
  result := JSON;
end;

procedure TTextWriter.AddJSONEscape(P: Pointer; Len: PtrInt);
var
  i, start: PtrInt;
  {$ifdef CPUX86NOTPIC}
  tab: TNormTableByte absolute JSON_ESCAPE;
  {$else}
  tab: PNormTableByte;
  {$endif}
label
  noesc;
begin
  if P = nil then
    exit;
  if Len = 0 then
    dec(Len); // -1 = no end
  i := 0;
  {$ifndef CPUX86NOTPIC}
  tab := @JSON_ESCAPE;
  {$endif}
  if tab[PByteArray(P)[i]] = 0 then
  begin
noesc:
    start := i;
    if Len < 0 then
      repeat // fastest loop is for AddJSONEscape(P,nil)
        inc(i);
      until tab[PByteArray(P)[i]] <> JSON_ESCAPE_NONE
    else
      repeat
        inc(i);
      until (i >= Len) or (tab[PByteArray(P)[i]] <> JSON_ESCAPE_NONE);
    inc(PByte(P), start);
    dec(i, start);
    if Len >= 0 then
      dec(Len, start);
    if BEnd - B <= i then
      AddNoJSONEscape(P, i)
    else
    begin
      MoveFast(P^, B[1], i);
      inc(B, i);
    end;
    if (Len >= 0) and (i >= Len) then
      exit;
  end;
  repeat
    if BEnd - B <= 10 then
      FlushToStream;
    case tab[PByteArray(P)[i]] of // better codegen with no temp var
      JSON_ESCAPE_NONE:
        goto noesc;
      JSON_ESCAPE_ENDINGZERO:
        exit; // #0
      JSON_ESCAPE_UNICODEHEX:
      begin // characters below ' ', #7 e.g. -> // 'u0007'
        PCardinal(B + 1)^ := ord('\') + ord('u') shl 8 + ord('0') shl 16 + ord('0') shl 24;
        inc(B, 4);
        PWord(B + 1)^ := TwoDigitsHexWB[PByteArray(P)[i]];
      end;
    else // escaped as \ + b,t,n,f,r,\,"
      PWord(B + 1)^ := (integer(tab[PByteArray(P)[i]]) shl 8) or ord('\');
    end;
    inc(i);
    inc(B, 2);
  until (Len >= 0) and (i >= Len);
end;

procedure TTextWriter.AddNoJSONEscapeString(const s: string);
begin
  if s <> '' then
    {$ifdef UNICODE}
    AddNoJSONEscapeW(pointer(s), 0);
    {$else}
    AddAnsiString(s, twNone);
    {$endif}
end;

procedure TTextWriter.AddJSONEscapeString(const s: string);
begin
  if s <> '' then
    {$ifdef UNICODE}
    AddJSONEscapeW(pointer(s), Length(s));
    {$else}
    AddAnyAnsiString(s, twJSONEscape, 0);
    {$endif}
end;

procedure TTextWriter.AddJSONEscapeAnsiString(const s: AnsiString);
begin
  AddAnyAnsiString(s, twJSONEscape, 0);
end;

procedure TTextWriter.AddJSONEscapeW(P: PWord; Len: PtrInt);
var
  i, c, s: PtrInt;
  esc: byte;
begin
  if P = nil then
    exit;
  if Len = 0 then
    Len := MaxInt;
  i := 0;
  while i < Len do
  begin
    s := i;
    repeat
      c := PWordArray(P)[i];
      if (c <= 127) and (JSON_ESCAPE[c] <> JSON_ESCAPE_NONE) then
        break;
      inc(i);
    until i >= Len;
    if i <> s then
      AddNoJSONEscapeW(@PWordArray(P)[s], i - s);
    if i >= Len then
      exit;
    c := PWordArray(P)[i];
    if c = 0 then
      exit;
    esc := JSON_ESCAPE[c];
    if esc = JSON_ESCAPE_ENDINGZERO then // #0
      exit
    else if esc = JSON_ESCAPE_UNICODEHEX then
    begin // characters below ' ', #7 e.g. -> \u0007
      AddShort('\u00');
      AddByteToHex(c);
    end
    else
      Add('\', AnsiChar(esc)); // escaped as \ + b,t,n,f,r,\,"
    inc(i);
  end;
end;

procedure TTextWriter.AddJSONEscape(const V: TVarRec);
begin
  with V do
    case VType of
      vtPointer:
        AddShort('null');
      vtString, vtAnsiString, {$ifdef HASVARUSTRING}vtUnicodeString, {$endif}
      vtPChar, vtChar, vtWideChar, vtWideString, vtClass:
        begin
          Add('"');
          case VType of
            vtString:
              if VString^[0] <> #0 then
                AddJSONEscape(@VString^[1], ord(VString^[0]));
            vtAnsiString:
              AddJSONEscape(VAnsiString);
            {$ifdef HASVARUSTRING}
            vtUnicodeString:
              AddJSONEscapeW(pointer(UnicodeString(VUnicodeString)),
                length(UnicodeString(VUnicodeString)));
            {$endif}
            vtPChar:
              AddJSONEscape(VPChar);
            vtChar:
              AddJSONEscape(@VChar, 1);
            vtWideChar:
              AddJSONEscapeW(@VWideChar, 1);
            vtWideString:
              AddJSONEscapeW(VWideString);
            vtClass:
              AddClassName(VClass);
          end;
          Add('"');
        end;
      vtBoolean:
        Add(VBoolean); // 'true'/'false'
      vtInteger:
        Add(VInteger);
      vtInt64:
        Add(VInt64^);
      {$ifdef FPC}
      vtQWord:
        AddQ(V.VQWord^);
      {$endif}
      vtExtended:
        AddDouble(VExtended^);
      vtCurrency:
        AddCurr64(VInt64^);
      vtObject:
        WriteObject(VObject);
      vtVariant:
        AddVariant(VVariant^, twJSONEscape);
    end;
end;

procedure TTextWriter.AddJSONEscape(Source: TTextWriter);
begin
  if Source.fTotalFileSize = 0 then
    AddJSONEscape(Source.fTempBuf, Source.B - Source.fTempBuf + 1)
  else
    AddJSONEscape(Pointer(Source.Text));
end;

procedure TTextWriter.AddNoJSONEscape(Source: TTextWriter);
begin
  if Source.fTotalFileSize = 0 then
    AddNoJSONEscape(Source.fTempBuf, Source.B - Source.fTempBuf + 1)
  else
    AddNoJSONEscapeUTF8(Source.Text);
end;

procedure TTextWriter.AddJSONString(const Text: RawUTF8);
begin
  Add('"');
  AddJSONEscape(pointer(Text));
  Add('"');
end;

procedure TTextWriter.Add(const V: TVarRec; Escape: TTextWriterKind;
  WriteObjectOptions: TTextWriterWriteObjectOptions);
begin
  with V do
    case VType of
      vtInteger:
        Add(VInteger);
      vtBoolean:
        if VBoolean then
          Add('1')
        else
          Add('0'); // normalize
      vtChar:
        Add(@VChar, 1, Escape);
      vtExtended:
        AddDouble(VExtended^);
      vtCurrency:
        AddCurr64(VInt64^);
      vtInt64:
        Add(VInt64^);
      {$ifdef FPC}
      vtQWord:
        AddQ(VQWord^);
      {$endif}
      vtVariant:
        AddVariant(VVariant^, Escape);
      vtString:
        if VString^[0] <> #0 then
          Add(@VString^[1], ord(VString^[0]), Escape);
      vtInterface, vtPointer:
        AddBinToHexDisplayMinChars(@VPointer, SizeOf(VPointer));
      vtPChar:
        Add(PUTF8Char(VPChar), Escape);
      vtObject:
        WriteObject(VObject, WriteObjectOptions);
      vtClass:
        AddClassName(VClass);
      vtWideChar:
        AddW(@VWideChar, 1, Escape);
      vtPWideChar:
        AddW(pointer(VPWideChar), StrLenW(VPWideChar), Escape);
      vtAnsiString:
        if VAnsiString <> nil then
          Add(VAnsiString, length(RawUTF8(VAnsiString)), Escape); // expect RawUTF8
      vtWideString:
        if VWideString <> nil then
          AddW(VWideString, length(WideString(VWideString)), Escape);
      {$ifdef HASVARUSTRING}
      vtUnicodeString:
        if VUnicodeString <> nil then // convert to UTF-8
          AddW(VUnicodeString, length(UnicodeString(VUnicodeString)), Escape);
      {$endif}
    end;
end;

procedure TTextWriter.AddJSON(const Format: RawUTF8; const Args, Params: array of const);
var
  temp: variant;
begin
  _JsonFmt(Format, Args, Params, JSON_OPTIONS_FAST, temp);
  AddVariant(temp, twJSONEscape);
end;

procedure TTextWriter.AddJSONArraysAsJSONObject(keys, values: PUTF8Char);
var
  k, v: PUTF8Char;
begin
  if (keys = nil) or (keys^ <> '[') or (values = nil) or (values^ <> '[') then
  begin
    AddShort('null');
    exit;
  end;
  inc(keys); // jump initial [
  inc(values);
  Add('{');
  repeat
    k := GotoEndJSONItem(keys);
    v := GotoEndJSONItem(values);
    if (k = nil) or (v = nil) then
      break; // invalid JSON input
    AddNoJSONEscape(keys, k - keys);
    Add(':');
    AddNoJSONEscape(values, v - values);
    Add(',');
    if (k^ <> ',') or (v^ <> ',') then
      break; // reached the end of the input JSON arrays
    keys := k + 1;
    values := v + 1;
  until false;
  CancelLastComma;
  Add('}');
end;

procedure TTextWriter.AddJSONEscape(const NameValuePairs: array of const);
var
  a: integer;

  procedure WriteValue;
  begin
    case VarRecAsChar(NameValuePairs[a]) of
      ord('['):
        begin
          Add('[');
          while a < high(NameValuePairs) do
          begin
            inc(a);
            if VarRecAsChar(NameValuePairs[a]) = ord(']') then
              break;
            WriteValue;
          end;
          CancelLastComma;
          Add(']');
        end;
      ord('{'):
        begin
          Add('{');
          while a < high(NameValuePairs) do
          begin
            inc(a);
            if VarRecAsChar(NameValuePairs[a]) = ord('}') then
              break;
            AddJSONEscape(NameValuePairs[a]);
            Add(':');
            inc(a);
            WriteValue;
          end;
          CancelLastComma;
          Add('}');
        end
    else
      AddJSONEscape(NameValuePairs[a]);
    end;
    Add(',');
  end;

begin
  Add('{');
  a := 0;
  while a < high(NameValuePairs) do
  begin
    AddJSONEscape(NameValuePairs[a]);
    inc(a);
    Add(':');
    WriteValue;
    inc(a);
  end;
  CancelLastComma;
  Add('}');
end;

procedure TTextWriter.WriteObject(Value: TObject;
  Options: TTextWriterWriteObjectOptions);
begin




  {
    TODODODODODO !!!!!!!!!
   *************************
  }




end;


procedure InitializeConstants;
var
  i: PtrInt;
  c: AnsiChar;
begin
  // branchless JSON escaping - JSON_ESCAPE_NONE=0 if no JSON escape needed
  JSON_ESCAPE[0] := JSON_ESCAPE_ENDINGZERO;   // 1 for #0 end of input
  for i := 1 to 31 do
    JSON_ESCAPE[i] := JSON_ESCAPE_UNICODEHEX; // 2 should be escaped as \u00xx
  JSON_ESCAPE[8] := ord('b');  // others contain the escaped character
  JSON_ESCAPE[9] := ord('t');
  JSON_ESCAPE[10] := ord('n');
  JSON_ESCAPE[12] := ord('f');
  JSON_ESCAPE[13] := ord('r');
  JSON_ESCAPE[ord('\')] := ord('\');
  JSON_ESCAPE[ord('"')] := ord('"');
  // branchless JSON parsing
  include(JSON_CHARS[#0], jcEndOfJSONFieldOr0);
  for c := low(c) to high(c) do begin
    if c in [',', ']', '}', ':'] then
    begin
      include(JSON_CHARS[c], jcEndOfJSONField);
      include(JSON_CHARS[c], jcEndOfJSONFieldOr0);
    end;
    if c in [#0, #9, #10, #13, ' ',  ',', '}', ']'] then
      include(JSON_CHARS[c], jcEndOfJSONValueField);
    if c in [#0, '"', '\'] then
      include(JSON_CHARS[c], jcJSONStringMarker);
    if c in ['-', '0'..'9'] then
      include(JSON_CHARS[c], jcDigitFirstChar);
    if c in ['-', '+', '0'..'9', '.', 'E', 'e'] then
      include(JSON_CHARS[c], jcDigitFloatChar);
    if c in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$'] then
      include(JSON_CHARS[c], jcJsonIdentifierFirstChar);
    if c in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']'] then
      include(JSON_CHARS[c], jcJsonIdentifier);
  end;
end;

initialization
  InitializeConstants;
  DefaultTextWriterSerializer := TTextWriter;
end.

