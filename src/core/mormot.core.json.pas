/// Framework Core Low-Level JSON Processing
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.json;

{
  *****************************************************************************

   JSON functions shared by all framework units
    - Low-Level JSON Processing Functions
    - TTextWriter class with proper JSON escaping and WriteObject() support
    - JSON-aware TSynNameValue Name/Value RawUTF8 Storage
    - JSON-aware TSynDictionary Storage
    - Custom JSON Serialization Registration
    - JSON Serialization Wrapper Functions

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  {$ifdef ISDELPHI}
  typinfo,  // circumvent Delphi inlining problem of mormot.core.rtti methods
  {$endif ISDELPHI}
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.data;


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
// - used e.g. by TSynDictionary.LoadFromJSON
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

  { TTextWriter }

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
    procedure AddTimeLog(Value: PInt64; QuoteChar: AnsiChar = #0);
    /// append a TUnixTime value, expanded as Iso-8601 encoded text
    procedure AddUnixTime(Value: PInt64; QuoteChar: AnsiChar = #0);
    /// append a TUnixMSTime value, expanded as Iso-8601 encoded text
    procedure AddUnixMSTime(Value: PInt64; WithMS: boolean = false;
      QuoteChar: AnsiChar = #0);
    /// append a TDateTime value, expanded as Iso-8601 encoded text
    // - use 'YYYY-MM-DDThh:mm:ss' format (with FirstChar='T')
    // - if WithMS is TRUE, will append '.sss' for milliseconds resolution
    // - if QuoteChar is not #0, it will be written before and after the date
    procedure AddDateTime(Value: PDateTime; FirstChar: AnsiChar = 'T';
      QuoteChar: AnsiChar = #0; WithMS: boolean = false;
      AlwaysDateAndTime: boolean = false); overload;
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
    // - properly handle Value as a TRttiVarData  from TRttiProp.GetValue
    procedure AddVariant(const Value: variant; Escape: TTextWriterKind = twJSONEscape;
      WriteOptions: TTextWriterWriteObjectOptions = [woFullExpand]); override;
    /// append complex types as JSON content
    // - handle rkClass as WriteObject, rkEnumeration/rkSet with proper options,
    // rkRecord, rkDynArray or rkVariant using proper JSON serialization
    // - other types will append 'null'
    // - returns the size of the Value, in bytes
    function AddTypedJSON(Value, TypeInfo: pointer;
      WriteOptions: TTextWriterWriteObjectOptions = [woFullExpand]): PtrInt; override;
    /// append a JSON value, array or document, in a specified format
    // - this overriden version will properly handle JSON escape
    function AddJSONReformat(JSON: PUTF8Char; Format: TTextWriterJSONFormat;
      EndOfObject: PUTF8Char): PUTF8Char; override;

    /// append a record content as UTF-8 encoded JSON or custom serialization
    // - default serialization will use Base64 encoded binary stream, or
    // a custom serialization, in case of a previous registration via
    // RegisterCustomJSONSerializer() class method - from a dynamic array
    // handling this kind of records, or directly from TypeInfo() of the record
    // - by default, custom serializers defined via RegisterCustomJSONSerializer()
    // would write enumerates and sets as integer numbers, unless
    // twoEnumSetsAsTextInRecord or twoEnumSetsAsBooleanInRecord is set in
    // the instance CustomOptions
    // - returns the element size
    function AddRecordJSON(Value: pointer; Info: PRttiInfo;
      WriteOptions: TTextWriterWriteObjectOptions = []): PtrInt;
    /// append a void record content as UTF-8 encoded JSON or custom serialization
    // - this method will first create a void record (i.e. filled with #0 bytes)
    // then save its content with default or custom serialization
    procedure AddVoidRecordJSON(Info: PRttiInfo;
      WriteOptions: TTextWriterWriteObjectOptions = []);
    /// append a dynamic array content as UTF-8 encoded JSON array
    // - typical content could be
    // ! '[1,2,3,4]' or '["\uFFF0base64encodedbinary"]'
    procedure AddDynArrayJSON(var DynArray: TDynArray;
      WriteOptions: TTextWriterWriteObjectOptions = []); overload;
    /// append a dynamic array content as UTF-8 encoded JSON array
    // - expect a dynamic array TDynArrayHashed wrapper as incoming parameter
    procedure AddDynArrayJSON(var DynArray: TDynArrayHashed;
      WriteOptions: TTextWriterWriteObjectOptions = []); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a dynamic array content as UTF-8 encoded JSON array
    // - returns the array element size
    function AddDynArrayJSON(Value: pointer; Info: PRttiInfo;
      WriteOptions: TTextWriterWriteObjectOptions = []): PtrInt; overload;
    /// append UTF-8 content as text
    // - Text CodePage will be used (if possible) - assume RawUTF8 otherwise
    // - will properly handle JSON escape between two " double quotes
    procedure AddText(const Text: RawByteString; Escape: TTextWriterKind = twJSONEscape);
    /// append UTF-16 content as text
    // - P should be a #0 terminated PWideChar buffer
    // - will properly handle JSON escape between two " double quotes
    procedure AddTextW(P: PWord; Escape: TTextWriterKind = twJSONEscape);
    /// append some UTF-8 encoded chars to the buffer
    // - escapes chars according to the JSON RFC
    // - if Len is 0, writing will stop at #0 (default Len = 0 is slightly faster
    // than specifying Len>0 if you are sure P is zero-ended - e.g. from RawUTF8)
    procedure AddJSONEscape(P: Pointer; Len: PtrInt = 0); overload;
    /// append some Unicode encoded chars to the buffer
    // - if Len is 0, Len is calculated from zero-ended widechar
    // - escapes chars according to the JSON RFC
    procedure AddJSONEscapeW(P: PWord; Len: PtrInt = 0);
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
    // - used e.g. by TSynDictionary.SaveToJSON
    procedure AddJSONArraysAsJSONObject(keys, values: PUTF8Char);
  end;


{ ************ JSON-aware TSynNameValue Name/Value RawUTF8 Storage }

type
  /// store one Name/Value pair, as used by TSynNameValue class
  TSynNameValueItem = record
    /// the name of the Name/Value pair
    // - this property is hashed by TSynNameValue for fast retrieval
    Name: RawUTF8;
    /// the value of the Name/Value pair
    Value: RawUTF8;
    /// any associated Pointer or numerical value
    Tag: PtrInt;
  end;

  /// Name/Value pairs storage, as used by TSynNameValue class
  TSynNameValueItemDynArray = array of TSynNameValueItem;

  /// event handler used to convert on the fly some UTF-8 text content
  TOnSynNameValueConvertRawUTF8 = function(const text: RawUTF8): RawUTF8 of object;

  /// callback event used by TSynNameValue
  TOnSynNameValueNotify = procedure(const Item: TSynNameValueItem; Index: PtrInt) of object;

  /// pseudo-class used to store Name/Value RawUTF8 pairs
  // - use internaly a TDynArrayHashed instance for fast retrieval
  // - is therefore faster than TRawUTF8List
  // - is defined as an object, not as a class: you can use this in any
  // class, without the need to destroy the content
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}TSynNameValue = record private
  {$else}TSynNameValue = object protected{$endif}
    fOnAdd: TOnSynNameValueNotify;
    function GetBlobData: RawByteString;
    procedure SetBlobData(const aValue: RawByteString);
    function GetStr(const aName: RawUTF8): RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    function GetInt(const aName: RawUTF8): Int64; {$ifdef HASINLINE}inline;{$endif}
    function GetBool(const aName: RawUTF8): Boolean; {$ifdef HASINLINE}inline;{$endif}
  public
    /// the internal Name/Value storage
    List: TSynNameValueItemDynArray;
    /// the number of Name/Value pairs
    Count: integer;
    /// low-level access to the internal storage hasher
    DynArray: TDynArrayHashed;
    /// initialize the storage
    // - will also reset the internal List[] and the internal hash array
    procedure Init(aCaseSensitive: boolean);
    /// add an element to the array
    // - if aName already exists, its associated Value will be updated
    procedure Add(const aName, aValue: RawUTF8; aTag: PtrInt = 0);
    /// reset content, then add all name=value pairs from a supplied .ini file
    // section content
    // - will first call Init(false) to initialize the internal array
    // - Section can be retrieved e.g. via FindSectionFirstLine()
    procedure InitFromIniSection(Section: PUTF8Char;
      OnTheFlyConvert: TOnSynNameValueConvertRawUTF8 = nil;
      OnAdd: TOnSynNameValueNotify = nil);
    /// reset content, then add all name=value; CSV pairs
    // - will first call Init(false) to initialize the internal array
    // - if ItemSep=#10, then any kind of line feed (CRLF or LF) will be handled
    procedure InitFromCSV(CSV: PUTF8Char; NameValueSep: AnsiChar = '=';
      ItemSep: AnsiChar = #10);
    /// reset content, then add all fields from an JSON object
    // - will first call Init() to initialize the internal array
    // - then parse the incoming JSON object, storing all its field values
    // as RawUTF8, and returning TRUE if the supplied content is correct
    // - warning: the supplied JSON buffer will be decoded and modified in-place
    function InitFromJSON(JSON: PUTF8Char; aCaseSensitive: boolean = false): boolean;
    /// reset content, then add all name, value pairs
    // - will first call Init(false) to initialize the internal array
    procedure InitFromNamesValues(const Names, Values: array of RawUTF8);
    /// search for a Name, return the index in List
    // - using fast O(1) hash algoritm
    function Find(const aName: RawUTF8): integer;
    /// search for the first chars of a Name, return the index in List
    // - using O(n) calls of IdemPChar() function
    // - here aUpperName should be already uppercase, as expected by IdemPChar()
    function FindStart(const aUpperName: RawUTF8): integer;
    /// search for a Value, return the index in List
    // - using O(n) brute force algoritm with case-sensitive aValue search
    function FindByValue(const aValue: RawUTF8): integer;
    /// search for a Name, and delete its entry in the List if it exists
    function Delete(const aName: RawUTF8): boolean;
    /// search for a Value, and delete its entry in the List if it exists
    // - returns the number of deleted entries
    // - you may search for more than one match, by setting a >1 Limit value
    function DeleteByValue(const aValue: RawUTF8; Limit: integer = 1): integer;
    /// search for a Name, return the associated Value as a UTF-8 string
    function Value(const aName: RawUTF8; const aDefaultValue: RawUTF8 = ''): RawUTF8;
    /// search for a Name, return the associated Value as integer
    function ValueInt(const aName: RawUTF8; const aDefaultValue: Int64 = 0): Int64;
    /// search for a Name, return the associated Value as boolean
    // - returns true only if the value is exactly '1'
    function ValueBool(const aName: RawUTF8): Boolean;
    /// search for a Name, return the associated Value as an enumerate
    // - returns true and set aEnum if aName was found, and associated value
    // matched an aEnumTypeInfo item
    // - returns false if no match was found
    function ValueEnum(const aName: RawUTF8; aEnumTypeInfo: pointer; out aEnum;
      aEnumDefault: byte = 0): boolean; overload;
    /// returns all values, as CSV or INI content
    function AsCSV(const KeySeparator: RawUTF8 = '=';
      const ValueSeparator: RawUTF8 = #13#10; const IgnoreKey: RawUTF8 = ''): RawUTF8;
    /// returns all values as a JSON object of string fields
    function AsJSON: RawUTF8;
    /// fill the supplied two arrays of RawUTF8 with the stored values
    procedure AsNameValues(out Names,Values: TRawUTF8DynArray);
    /// search for a Name, return the associated Value as variant
    // - returns null if the name was not found
    function ValueVariantOrNull(const aName: RawUTF8): variant;
    /// compute a TDocVariant document from the stored values
    // - output variant will be reset and filled as a TDocVariant instance,
    // ready to be serialized as a JSON object
    // - if there is no value stored (i.e. Count=0), set null
    procedure AsDocVariant(out DocVariant: variant;
      ExtendedJson: boolean = false; ValueAsString: boolean = true;
      AllowVarDouble: boolean = false); overload;
    /// compute a TDocVariant document from the stored values
    function AsDocVariant(ExtendedJson: boolean = false;
      ValueAsString: boolean = true): variant; overload; {$ifdef HASINLINE}inline;{$endif}
    /// merge the stored values into a TDocVariant document
    // - existing properties would be updated, then new values will be added to
    // the supplied TDocVariant instance, ready to be serialized as a JSON object
    // - if ValueAsString is TRUE, values would be stored as string
    // - if ValueAsString is FALSE, numerical values would be identified by
    // IsString() and stored as such in the resulting TDocVariant
    // - if you let ChangedProps point to a TDocVariantData, it would contain
    // an object with the stored values, just like AsDocVariant
    // - returns the number of updated values in the TDocVariant, 0 if
    // no value was changed
    function MergeDocVariant(var DocVariant: variant; ValueAsString: boolean;
      ChangedProps: PVariant = nil; ExtendedJson: boolean = false;
      AllowVarDouble: boolean = false): integer;
    /// returns true if the Init() method has been called
    function Initialized: boolean;
    /// can be used to set all data from one BLOB memory buffer
    procedure SetBlobDataPtr(aValue: pointer);
    /// can be used to set or retrieve all stored data as one BLOB content
    property BlobData: RawByteString read GetBlobData write SetBlobData;
    /// event triggerred after an item has just been added to the list
    property OnAfterAdd: TOnSynNameValueNotify read fOnAdd write fOnAdd;
    /// search for a Name, return the associated Value as a UTF-8 string
    // - returns '' if aName is not found in the stored keys
    property Str[const aName: RawUTF8]: RawUTF8 read GetStr; default;
    /// search for a Name, return the associated Value as integer
    // - returns 0 if aName is not found, or not a valid Int64 in the stored keys
    property Int[const aName: RawUTF8]: Int64 read GetInt;
    /// search for a Name, return the associated Value as boolean
    // - returns true if aName stores '1' as associated value
    property Bool[const aName: RawUTF8]: Boolean read GetBool;
  end;

  /// a reference pointer to a Name/Value RawUTF8 pairs storage
  PSynNameValue = ^TSynNameValue;


{ *********** JSON-aware TSynDictionary Storage }

type
  // internal flag, used only by TSynDictionary.InArray protected method
  TSynDictionaryInArray = (
    iaFind, iaFindAndDelete, iaFindAndUpdate, iaFindAndAddIfNotExisting, iaAdd);

  /// event called by TSynDictionary.ForEach methods to iterate over stored items
  // - if the implementation method returns TRUE, will continue the loop
  // - if the implementation method returns FALSE, will stop values browsing
  // - aOpaque is a custom value specified at ForEach() method call
  TSynDictionaryEvent = function(const aKey; var aValue;
    aIndex, aCount: integer; aOpaque: pointer): boolean of object;

  /// event called by TSynDictionary.DeleteDeprecated
  // - called just before deletion: return false to by-pass this item
  TSynDictionaryCanDeleteEvent = function(const aKey, aValue;
    aIndex: integer): boolean of object;

  /// thread-safe dictionary to store some values from associated keys
  // - will maintain a dynamic array of values, associated with a hash table
  // for the keys, so that setting or retrieving values would be O(1)
  // - all process is protected by a TSynLocker, so will be thread-safe
  // - TDynArray is a wrapper which do not store anything, whereas this class
  // is able to store both keys and values, and provide convenient methods to
  // access the stored data, including JSON serialization and binary storage
  TSynDictionary = class(TSynPersistentLock)
  protected
    fKeys: TDynArrayHashed;
    fValues: TDynArray;
    fTimeOut: TCardinalDynArray;
    fTimeOuts: TDynArray;
    fCompressAlgo: TAlgoCompress;
    fOnCanDelete: TSynDictionaryCanDeleteEvent;
    function InArray(const aKey, aArrayValue; aAction: TSynDictionaryInArray;
      aCompare: TDynArraySortCompare): boolean;
    procedure SetTimeouts;
    function ComputeNextTimeOut: cardinal;
    function KeyFullHash(const Elem): cardinal;
    function KeyFullCompare(const A, B): integer;
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
    function GetTimeOutSeconds: cardinal;
  public
    /// initialize the dictionary storage, specifyng dynamic array keys/values
    // - aKeyTypeInfo should be a dynamic array TypeInfo() RTTI pointer, which
    // would store the keys within this TSynDictionary instance
    // - aValueTypeInfo should be a dynamic array TypeInfo() RTTI pointer, which
    // would store the values within this TSynDictionary instance
    // - by default, string keys would be searched following exact case, unless
    // aKeyCaseInsensitive is TRUE
    // - you can set an optional timeout period, in seconds - you should call
    // DeleteDeprecated periodically to search for deprecated items
    constructor Create(aKeyTypeInfo, aValueTypeInfo: pointer;
      aKeyCaseInsensitive: boolean = false; aTimeoutSeconds: cardinal = 0;
      aCompressAlgo: TAlgoCompress = nil); reintroduce; virtual;
    /// finalize the storage
    // - would release all internal stored values
    destructor Destroy; override;
    /// try to add a value associated with a primary key
    // - returns the index of the inserted item, -1 if aKey is already existing
    // - this method is thread-safe, since it will lock the instance
    function Add(const aKey, aValue): integer;
    /// store a value associated with a primary key
    // - returns the index of the matching item
    // - if aKey does not exist, a new entry is added
    // - if aKey does exist, the existing entry is overriden with aValue
    // - this method is thread-safe, since it will lock the instance
    function AddOrUpdate(const aKey, aValue): integer;
    /// clear the value associated via aKey
    // - does not delete the entry, but reset its value
    // - returns the index of the matching item, -1 if aKey was not found
    // - this method is thread-safe, since it will lock the instance
    function Clear(const aKey): integer;
    /// delete all key/value stored in the current instance
    procedure DeleteAll;
    /// delete a key/value association from its supplied aKey
    // - this would delete the entry, i.e. matching key and value pair
    // - returns the index of the deleted item, -1 if aKey was not found
    // - this method is thread-safe, since it will lock the instance
    function Delete(const aKey): integer;
    /// delete a key/value association from its internal index
    // - this method is not thread-safe: you should use fSafe.Lock/Unlock
    // e.g. then Find/FindValue to retrieve the index value
    function DeleteAt(aIndex: integer): boolean;
    /// search and delete all deprecated items according to TimeoutSeconds
    // - returns how many items have been deleted
    // - you can call this method very often: it will ensure that the
    // search process will take place at most once every second
    // - this method is thread-safe, but blocking during the process
    function DeleteDeprecated: integer;
    /// search of a primary key within the internal hashed dictionary
    // - returns the index of the matching item, -1 if aKey was not found
    // - if you want to access the value, you should use fSafe.Lock/Unlock:
    // consider using Exists or FindAndCopy thread-safe methods instead
    // - aUpdateTimeOut will update the associated timeout value of the entry
    function Find(const aKey; aUpdateTimeOut: boolean = false): integer;
    /// search of a primary key within the internal hashed dictionary
    // - returns a pointer to the matching item, nil if aKey was not found
    // - if you want to access the value, you should use fSafe.Lock/Unlock:
    // consider using Exists or FindAndCopy thread-safe methods instead
    // - aUpdateTimeOut will update the associated timeout value of the entry
    function FindValue(const aKey; aUpdateTimeOut: boolean = false;
      aIndex: PInteger = nil): pointer;
    /// search of a primary key within the internal hashed dictionary
    // - returns a pointer to the matching or already existing item
    // - if you want to access the value, you should use fSafe.Lock/Unlock:
    // consider using Exists or FindAndCopy thread-safe methods instead
    // - will update the associated timeout value of the entry, if applying
    function FindValueOrAdd(const aKey; var added: boolean;
      aIndex: PInteger = nil): pointer;
    /// search of a stored value by its primary key, and return a local copy
    // - so this method is thread-safe
    // - returns TRUE if aKey was found, FALSE if no match exists
    // - will update the associated timeout value of the entry, unless
    // aUpdateTimeOut is set to false
    function FindAndCopy(const aKey; out aValue; aUpdateTimeOut: boolean = true): boolean;
    /// search of a stored value by its primary key, then delete and return it
    // - returns TRUE if aKey was found, fill aValue with its content,
    // and delete the entry in the internal storage
    // - so this method is thread-safe
    // - returns FALSE if no match exists
    function FindAndExtract(const aKey; out aValue): boolean;
    /// search for a primary key presence
    // - returns TRUE if aKey was found, FALSE if no match exists
    // - this method is thread-safe
    function Exists(const aKey): boolean;
    /// apply a specified event over all items stored in this dictionnary
    // - would browse the list in the adding order
    // - returns the number of times OnEach has been called
    // - this method is thread-safe, since it will lock the instance
    function ForEach(const OnEach: TSynDictionaryEvent;
      Opaque: pointer = nil): integer; overload;
    /// apply a specified event over matching items stored in this dictionnary
    // - would browse the list in the adding order, comparing each key and/or
    // value item with the supplied comparison functions and aKey/aValue content
    // - returns the number of times OnMatch has been called, i.e. how many times
    // KeyCompare(aKey,Keys[#])=0 or ValueCompare(aValue,Values[#])=0
    // - this method is thread-safe, since it will lock the instance
    function ForEach(const OnMatch: TSynDictionaryEvent;
      KeyCompare, ValueCompare: TDynArraySortCompare; const aKey, aValue;
      Opaque: pointer = nil): integer; overload;
    /// touch the entry timeout field so that it won't be deprecated sooner
    // - this method is not thread-safe, and is expected to be execute e.g.
    // from a ForEach() TSynDictionaryEvent callback
    procedure SetTimeoutAtIndex(aIndex: integer);
    /// search aArrayValue item in a dynamic-array value associated via aKey
    // - expect the stored value to be a dynamic array itself
    // - would search for aKey as primary key, then use TDynArray.Find
    // to delete any aArrayValue match in the associated dynamic array
    // - returns FALSE if Values is not a tkDynArray, or if aKey or aArrayValue
    // were not found
    // - this method is thread-safe, since it will lock the instance
    function FindInArray(const aKey, aArrayValue;
      aCompare: TDynArraySortCompare): boolean;
    /// search of a stored key by its associated key, and return a key local copy
    // - won't use any hashed index but RTTI TDynArray.IndexOf search over
    // over fValues() so is much slower than FindAndCopy() for huge arrays
    // - will update the associated timeout value of the entry, unless
    // aUpdateTimeOut is set to false
    // - this method is thread-safe
    // - returns TRUE if aValue was found, FALSE if no match exists
    function FindKeyFromValue(const aValue; out aKey;
      aUpdateTimeOut: boolean = true): boolean;
    /// add aArrayValue item within a dynamic-array value associated via aKey
    // - expect the stored value to be a dynamic array itself
    // - would search for aKey as primary key, then use TDynArray.Add
    // to add aArrayValue to the associated dynamic array
    // - returns FALSE if Values is not a tkDynArray, or if aKey was not found
    // - this method is thread-safe, since it will lock the instance
    function AddInArray(const aKey, aArrayValue;
      aCompare: TDynArraySortCompare): boolean;
    /// add once aArrayValue within a dynamic-array value associated via aKey
    // - expect the stored value to be a dynamic array itself
    // - would search for aKey as primary key, then use
    // TDynArray.FindAndAddIfNotExisting to add once aArrayValue to the
    // associated dynamic array
    // - returns FALSE if Values is not a tkDynArray, or if aKey was not found
    // - this method is thread-safe, since it will lock the instance
    function AddOnceInArray(const aKey, aArrayValue;
      aCompare: TDynArraySortCompare): boolean;
    /// clear aArrayValue item of a dynamic-array value associated via aKey
    // - expect the stored value to be a dynamic array itself
    // - would search for aKey as primary key, then use TDynArray.FindAndDelete
    // to delete any aArrayValue match in the associated dynamic array
    // - returns FALSE if Values is not a tkDynArray, or if aKey or aArrayValue were
    // not found
    // - this method is thread-safe, since it will lock the instance
    function DeleteInArray(const aKey, aArrayValue;
      aCompare: TDynArraySortCompare): boolean;
    /// replace aArrayValue item of a dynamic-array value associated via aKey
    // - expect the stored value to be a dynamic array itself
    // - would search for aKey as primary key, then use TDynArray.FindAndUpdate
    // to delete any aArrayValue match in the associated dynamic array
    // - returns FALSE if Values is not a tkDynArray, or if aKey or aArrayValue were
    // not found
    // - this method is thread-safe, since it will lock the instance
    function UpdateInArray(const aKey, aArrayValue;
      aCompare: TDynArraySortCompare): boolean;
    /// make a copy of the stored values
    // - this method is thread-safe, since it will lock the instance during copy
    // - resulting length(Dest) will match the exact values count
    // - T*ObjArray will be reallocated and copied by content (using a temporary
    // JSON serialization), unless ObjArrayByRef is true and pointers are copied
    procedure CopyValues(out Dest; ObjArrayByRef: boolean = false);
    /// serialize the content as a "key":value JSON object
    procedure SaveToJSON(W: TTextWriter; EnumSetsAsText: boolean = false); overload;
    /// serialize the content as a "key":value JSON object
    function SaveToJSON(EnumSetsAsText: boolean = false): RawUTF8; overload;
    /// serialize the Values[] as a JSON array
    function SaveValuesToJSON(EnumSetsAsText: boolean = false): RawUTF8;
    /// unserialize the content from "key":value JSON object
    // - if the JSON input may not be correct (i.e. if not coming from SaveToJSON),
    // you may set EnsureNoKeyCollision=TRUE for a slow but safe keys validation
    function LoadFromJSON(const JSON: RawUTF8;
      CustomVariantOptions: PDocVariantOptions = nil): boolean; overload;
    /// unserialize the content from "key":value JSON object
    // - note that input JSON buffer is not modified in place: no need to create
    // a temporary copy if the buffer is about to be re-used
    function LoadFromJSON(JSON: PUTF8Char;
      CustomVariantOptions: PDocVariantOptions = nil): boolean; overload;
    /// save the content as SynLZ-compressed raw binary data
    // - warning: this format is tied to the values low-level RTTI, so if you
    // change the value/key type definitions, LoadFromBinary() would fail
    function SaveToBinary(NoCompression: boolean = false;
      Algo: TAlgoCompress = nil): RawByteString;
    /// load the content from SynLZ-compressed raw binary data
    // - as previously saved by SaveToBinary method
    function LoadFromBinary(const binary: RawByteString): boolean;
    /// can be assigned to OnCanDeleteDeprecated to check TSynPersistentLock(aValue).Safe.IsLocked
    class function OnCanDeleteSynPersistentLock(const aKey, aValue; aIndex: integer): boolean;
    /// can be assigned to OnCanDeleteDeprecated to check TSynPersistentLock(aValue).Safe.IsLocked
    class function OnCanDeleteSynPersistentLocked(const aKey, aValue; aIndex: integer): boolean;
    /// returns how many items are currently stored in this dictionary
    // - this method is thread-safe
    function Count: integer;
    /// fast returns how many items are currently stored in this dictionary
    // - this method is NOT thread-safe so should be protected by fSafe.Lock/UnLock
    function RawCount: integer; {$ifdef HASINLINE}inline;{$endif}
    /// direct access to the primary key identifiers
    // - if you want to access the keys, you should use fSafe.Lock/Unlock
    property Keys: TDynArrayHashed read fKeys;
    /// direct access to the associated stored values
    // - if you want to access the values, you should use fSafe.Lock/Unlock
    property Values: TDynArray read fValues;
    /// defines how many items are currently stored in Keys/Values internal arrays
    property Capacity: integer read GetCapacity write SetCapacity;
    /// direct low-level access to the internal access tick (GetTickCount64 shr 10)
    // - may be nil if TimeOutSeconds=0
    property TimeOut: TCardinalDynArray read fTimeOut;
    /// returns the aTimeOutSeconds parameter value, as specified to Create()
    property TimeOutSeconds: cardinal read GetTimeOutSeconds;
    /// the compression algorithm used for binary serialization
    property CompressAlgo: TAlgoCompress read fCompressAlgo write fCompressAlgo;
    /// callback to by-pass DeleteDeprecated deletion by returning false
    // - can be assigned e.g. to OnCanDeleteSynPersistentLock if Value is a
    // TSynPersistentLock instance, to avoid any potential access violation
    property OnCanDeleteDeprecated: TSynDictionaryCanDeleteEvent
      read fOnCanDelete write fOnCanDelete;
  end;



{ ********** Custom JSON Serialization Registration }

type
  TRttiCustomJsonWriter = procedure(W: TBaseWriter; const aValue) of object;


{ ********** JSON Serialization Wrapper Functions }

/// serialize most kind of content as JSON, using its RTTI
// - is just a wrapper around TTextWriter.AddTypedJSON()
// - so would handle tkClass, tkEnumeration, tkSet, tkRecord, tkDynArray,
// tkVariant kind of content - other kinds would return 'null'
// - you can override serialization options if needed
procedure SaveJSON(const Value; TypeInfo: PRttiInfo;
  Options: TTextWriterOptions; var result: RawUTF8); overload;

/// serialize most kind of content as JSON, using its RTTI
// - is just a wrapper around TTextWriter.AddTypedJSON()
// - so would handle tkClass, tkEnumeration, tkSet, tkRecord, tkDynArray,
// tkVariant kind of content - other kinds would return 'null'
function SaveJSON(const Value; TypeInfo: PRttiInfo;
  EnumSetsAsText: boolean = false): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save record into its JSON serialization as saved by TTextWriter.AddRecordJSON
// - will use default Base64 encoding over RecordSave() binary - or custom true
// JSON format (as set by TTextWriter.RegisterCustomJSONSerializer or via
// enhanced RTTI), if available (following EnumSetsAsText optional parameter
// for nested enumerates and sets)
function RecordSaveJSON(const Rec; TypeInfo: PRttiInfo;
  EnumSetsAsText: boolean = false): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// serialize a dynamic array content as JSON
// - Value shall be set to the source dynamic array field
// - is just a wrapper around TTextWriter.AddDynArrayJSON(), creating
// a temporary TDynArray wrapper on the stack
// - to be used e.g. for custom record JSON serialization, within a
// TDynArrayJSONCustomWriter callback or RegisterCustomJSONSerializerFromText()
// (following EnumSetsAsText optional parameter for nested enumerates and sets)
function DynArraySaveJSON(const Value; TypeInfo: PRttiInfo;
  EnumSetsAsText: boolean = false): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// will serialize any TObject into its UTF-8 JSON representation
/// - serialize as JSON the published integer, Int64, floating point values,
// TDateTime (stored as ISO 8601 text), string, variant and enumerate
// (e.g. boolean) properties of the object (and its parents)
// - would set twoForceJSONStandard to force standard (non-extended) JSON
// - the enumerates properties are stored with their integer index value
// - will write also the properties published in the parent classes
// - nested properties are serialized as nested JSON objects
// - any TCollection property will also be serialized as JSON arrays
// - you can add some custom serializers for ANY Delphi class, via mORMot.pas'
// TJSONSerializer.RegisterCustomSerializer() class method
// - call internaly TJSONSerializer.WriteObject() method (or fallback to
// TJSONWriter if mORMot.pas is not linked to the executable)
function ObjectToJSON(Value: TObject;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]): RawUTF8;

/// will serialize set of TObject into its UTF-8 JSON representation
// - follows ObjectToJSON()/TTextWriter.WriterObject() functions output
// - if Names is not supplied, the corresponding class names would be used
function ObjectsToJSON(const Names: array of RawUTF8; const Values: array of TObject;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]): RawUTF8;


implementation

uses
  mormot.core.datetime,
  mormot.core.os,
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
  begin // ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$']
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']']
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
            break; // not ['-', '+', '0'..'9', '.', 'E', 'e']
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
      if (PInteger(P)^ = NULL_LOW) and
         (jcEndOfJSONValueField in JSON_CHARS[P[4]]) then
         // [#0, #9, #10, #13, ' ',  ',', '}', ']']
      begin
        result := nil; // null -> returns nil and wasString=false
        if Len <> nil then
          Len^ := 0; // when result is converted to string
        inc(P, 4);
      end
      else
        exit;
    'f':
      if (PInteger(P + 1)^ =
          ord('a') + ord('l') shl 8 + ord('s') shl 16 + ord('e') shl 24) and
         (jcEndOfJSONValueField in JSON_CHARS[P[5]]) then
         // [#0, #9, #10, #13, ' ',  ',', '}', ']']
      begin
        result := P; // false -> returns 'false' and wasString=false
        if Len <> nil then
          Len^ := 5;
        inc(P, 5);
      end
      else
        exit;
    't':
      if (PInteger(P)^ = TRUE_LOW) and
         (jcEndOfJSONValueField in JSON_CHARS[P[4]]) then
         // [#0, #9, #10, #13, ' ',  ',', '}', ']']
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
  while not (jcEndOfJSONField in jsonset[P^]) do // not [',', ']', '}', ':']
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
    if not (jcJSONStringMarker in tab[P^]) then begin // not [#0, '"', '\']
      inc(P);
      continue; // very fast parsing of most UTF-8 chars
    end;
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
      exit; // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$']
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']']
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
      exit; // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$']
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']']
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
      exit; // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$']
    repeat
      inc(P);
    until not (jcJsonIdentifier in tab[P^]);
    // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']']
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
          // not ['-', '+', '0'..'9', '.', 'E', 'e']
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
          exit; // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$']
        repeat
          inc(P);
        until not (jcJsonIdentifier in tab[P^]);
        // not ['_', '0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']']
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
        // not ['-', '+', '0'..'9', '.', 'E', 'e']
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
  if jcEndOfJSONFieldOr0 in tab[P^] then // not [#0, ',', ']', '}', ':']
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



{ ********** Low-Level JSON Serialization for all TRTTIParserType }

type
  TRttiJsonSaveContext = object
    W: TTextWriter;
    Options: TTextWriterWriteObjectOptions;
    Prop: PRttiProp;
    Rtti: TRttiCache;
    procedure Init(WR: TTextWriter; WriteOptions: TTextWriterWriteObjectOptions;
      TypeInfo: PRttiInfo; PropInfo: PRttiProp); {$ifdef HASINLINE} inline; {$endif}
    procedure Add64(Value: PInt64; UnSigned: boolean);
    procedure AddShort(PS: PShortString);
    procedure AddShortBoolean(PS: PShortString; Value: boolean);
    procedure AddDateTime(Value: PDateTime; WithMS: boolean);
  end;

  /// internal function handler for JSON persistence of any TRTTIParserType value
  // - i.e. the kind of functions called via PT_JSONSAVE[] lookup table
  TRttiJsonSave = function(Data: pointer; const Ctxt: TRttiJsonSaveContext): PtrInt;


function _JS_Boolean(Data: PBoolean; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.Add(Data^);
  result := SizeOf(Data^);
end;

function _JS_Byte(Data: PByte; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddU(Data^);
  result := SizeOf(Data^);
end;

function _JS_Cardinal(Data: PCardinal; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddU(Data^);
  result := SizeOf(Data^);
end;

function _JS_Currency(Data: PInt64; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddCurr64(Data);
  result := SizeOf(Data^);
end;

function _JS_Double(Data: PDouble; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddDouble(unaligned(Data^));
  result := SizeOf(Data^);
end;

function _JS_Extended(Data: PSynExtended; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddDouble(Data^);
  result := SizeOf(Data^);
end;

function _JS_Int64(Data: PInt64; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.Add64(Data, {unsigned=}false);
  result := SizeOf(Data^);
end;

function _JS_Integer(Data: PInteger; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.Add(Data^);
  result := SizeOf(Data^);
end;

function _JS_QWord(Data: PInt64; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.Add64(Data, {unsigned=}true);
  result := SizeOf(Data^);
end;

function _JS_RawByteString(Data: PRawByteString; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.WrBase64(pointer(Data^), length(Data^), {withmagic=}true);
  result := SizeOf(Data^);
end;

function _JS_RawJSON(Data: PRawJSON; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddRawJSON(Data^);
  result := SizeOf(Data^);
end;

function _JS_RawUTF8(Data: PPointer; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.Add('"');
  Ctxt.W.AddJSONEscape(Data^);
  Ctxt.W.Add('"');
  result := SizeOf(Data^);
end;

function _JS_Single(Data: PSingle; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddSingle(Data^);
  result := SizeOf(Data^);
end;

function _JS_Unicode(Data: PPWord; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.Add('"');
  Ctxt.W.AddJSONEscapeW(Data^);
  Ctxt.W.Add('"');
  result := SizeOf(Data^);
end;

function _JS_DateTime(Data: PDateTime; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.AddDateTime(Data, {withms=}false);
  result := SizeOf(Data^);
end;

function _JS_DateTimeMS(Data: PDateTime; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.AddDateTime(Data, {withms=}true);
  result := SizeOf(Data^);
end;

function _JS_GUID(Data: PGUID; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.Add(Data, '"');
  result := SizeOf(Data^);
end;

function _JS_Hash128(Data: PHash128; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddBinToHexDisplayLower(Data, SizeOf(Data^), '"');
  result := SizeOf(Data^);
end;

function _JS_Hash256(Data: PHash256; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddBinToHexDisplayLower(Data, SizeOf(Data^), '"');
  result := SizeOf(Data^);
end;

function _JS_Hash512(Data: PHash128; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddBinToHexDisplayLower(Data, SizeOf(Data^), '"');
  result := SizeOf(Data^);
end;

function _JS_TimeLog(Data: PInt64; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  if woTimeLogAsText in Ctxt.Options then
    Ctxt.W.AddTimeLog(Data, '"')
  else
    Ctxt.Add64(Data, true);
  result := SizeOf(Data^);
end;

function _JS_UnixTime(Data: PInt64; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  if woTimeLogAsText in Ctxt.Options then
    Ctxt.W.AddUnixTime(Data, '"')
  else
    Ctxt.Add64(Data, true);
  result := SizeOf(Data^);
end;

function _JS_UnixMSTime(Data: PInt64; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  if woTimeLogAsText in Ctxt.Options then
    Ctxt.W.AddUnixMSTime(Data, {withms=}true, '"')
  else
    Ctxt.Add64(Data, true);
  result := SizeOf(Data^);
end;

function _JS_Variant(Data: PVariant; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddVariant(Data^);
  result := SizeOf(Data^);
end;

function _JS_WinAnsi(Data: PWinAnsiString; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.Add('"');
  Ctxt.W.AddAnyAnsiBuffer(pointer(Data^), length(Data^), twJSONEscape, CODEPAGE_US);
  Ctxt.W.Add('"');
  result := SizeOf(Data^);
end;

function _JS_Word(Data: PWord; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddU(Data^);
  result := SizeOf(Data^);
end;

function _JS_Array(Data: pointer; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin

  //result := Ctxt.Rtti.Size;
end;

function _JS_DynArray(Data: pointer; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  result := Ctxt.W.AddDynArrayJSON(Data, Ctxt.Rtti.Info);
end;

function _JS_Interface(Data: PInterface; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.AddNull;
  result := SizeOf(Data^);
end;

function _JS_ID(Data: PInt64; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
  Ctxt.W.Add(Data^);
  if woIDAsIDstr in Ctxt.Options then
  begin
    if Ctxt.Prop <> nil then
    begin
      Ctxt.W.Add(',', '"');
      Ctxt.W.AddShort(Ctxt.Prop^.Name^);
      Ctxt.W.AddShorter('_str":');
    end
    else
    begin
      Ctxt.W.Add(',');
      Ctxt.W.AddPropName('ID_str');
    end;
    Ctxt.W.Add('"');
    Ctxt.W.Add(Data^);
    Ctxt.W.Add('"');
  end;
  result := SizeOf(Data^);
end;

function _JS_Enumeration(Data: PByte; const Ctxt: TRttiJsonSaveContext): PtrInt;
var
  o: TTextWriterOptions;
  PS: PShortString;
begin
  o := Ctxt.W.CustomOptions;
  if (twoEnumSetsAsBooleanInRecord in o) or (twoEnumSetsAsTextInRecord in o) then
  begin
    PS := Ctxt.Rtti.EnumInfo^.GetEnumNameOrd(Data^);
    if twoEnumSetsAsBooleanInRecord in o then
      Ctxt.AddShortBoolean(PS, true)
    else
      Ctxt.AddShort(PS);
  end
  else
    Ctxt.W.AddU(Data^);
  result := Ctxt.Rtti.Size;
end;

function _JS_Set(Data: PCardinal; const Ctxt: TRttiJsonSaveContext): PtrInt;
var
  PS: PShortString;
  i: integer;
  o: TTextWriterOptions;
begin
  o := Ctxt.W.CustomOptions;
  if twoEnumSetsAsBooleanInRecord in o then
  begin
    PS := Ctxt.Rtti.EnumInfo^.NameList;
    Ctxt.W.Add('{');
    for i := 0 to Ctxt.Rtti.EnumMax do
    begin
      Ctxt.AddShortBoolean(PS, GetBitPtr(Data, i));
      Ctxt.W.Add(',');
      inc(PByte(PS), PByte(PS)^ + 1); // next
    end;
    Ctxt.W.CancelLastComma;
    Ctxt.W.Add('}');
  end
  else if twoEnumSetsAsTextInRecord in o then
  begin
    Ctxt.W.Add('[');
    if (twoFullSetsAsStar in o) and
       GetAllBits(Data^, Ctxt.Rtti.EnumMax + 1) then
      Ctxt.W.AddShorter('"*"')
    else
    begin
      PS := Ctxt.Rtti.EnumInfo^.NameList;
      for i := 0 to Ctxt.Rtti.EnumMax do
      begin
        if GetBitPtr(Data, i) then
        begin
          Ctxt.W.AddShort(PS^);
          Ctxt.W.Add(',');
        end;
        inc(PByte(PS), PByte(PS)^ + 1); // next
      end;
      Ctxt.W.CancelLastComma;
    end;
    Ctxt.W.Add(']');
  end
  else
    Ctxt.W.AddU(Data^ and Ctxt.Rtti.EnumMask);
  result := Ctxt.Rtti.Size;
end;

function _JS_Record(Data: pointer; const Ctxt: TRttiJsonSaveContext): PtrInt;
begin
end;

function _JS_Class(Data: PObject; const Ctxt: TRttiJsonSaveContext): PtrInt;
var
  todo: integer;
begin


  result := SizeOf(Data^);
end;

const
  // use pointer to allow any kind of Data^ type in above functions
  // - typecast to TRttiJsonSave before call
  PT_JSONSAVE: array[TRTTIParserType] of pointer = (
    nil, @_JS_Array, @_JS_Boolean, @_JS_Byte, @_JS_Cardinal, @_JS_Currency,
    @_JS_Double, @_JS_Extended, @_JS_Int64, @_JS_Integer, @_JS_QWord,
    @_JS_RawByteString, @_JS_RawJSON, @_JS_RawUTF8, @_JS_Record, @_JS_Single,
    {$ifdef UNICODE} @_JS_Unicode {$else} @_JS_RawByteString {$endif},
    @_JS_Unicode, @_JS_DateTime, @_JS_DateTimeMS, @_JS_GUID, @_JS_Hash128,
    @_JS_Hash256, @_JS_Hash512, nil, @_JS_TimeLog, @_JS_Unicode, @_JS_UnixTime,
    @_JS_UnixMSTime, @_JS_Variant, @_JS_Unicode, @_JS_WinAnsi, @_JS_Word,
    @_JS_Enumeration, @_JS_Set, @_JS_Class, @_JS_DynArray, @_JS_Interface, nil);

  // use pointer to allow any complex kind of Data^ type in above functions
  // - typecast to TRttiJsonSave before call
  PTC_JSONSAVE: array[TRTTIParserComplexType] of pointer = (
    nil, nil, nil, nil, @_JS_ID, @_JS_ID, @_JS_QWord, @_JS_QWord, @_JS_QWord);

var
  // JSON serialization of most complex types for TTextWriter.AddTypedJSON
  RTTI_JSONSAVE: array[TRttiKind] of TRttiJsonSave;


{ TRttiJsonSaveContext }

procedure TRttiJsonSaveContext.Init(WR: TTextWriter;
  WriteOptions: TTextWriterWriteObjectOptions; TypeInfo: PRttiInfo;
  PropInfo: PRttiProp);
begin
  W := WR;
  Options := WriteOptions;
  Prop := PropInfo;
  TypeInfo^.ComputeCache(Rtti);
end;

procedure TRttiJsonSaveContext.Add64(Value: PInt64; UnSigned: boolean);
begin
  if woInt64AsHex in Options then
    if Value^ = 0 then
      W.Add('"', '"')
    else
      W.AddBinToHexDisplayLower(Value, SizeOf(Value^), '"')
  else if UnSigned then
    W.AddQ(PQWord(Value)^)
  else
    W.Add(Value^);
end;

procedure TRttiJsonSaveContext.AddShort(PS: PShortString);
begin
  W.Add('"');
  if twoTrimLeftEnumSets in W.CustomOptions then
    W.AddTrimLeftLowerCase(PS)
  else
    W.AddShort(PS^);
  W.Add('"');
end;

procedure TRttiJsonSaveContext.AddShortBoolean(PS: PShortString; Value: boolean);
begin
  AddShort(PS);
  W.Add(':');
  W.Add(Value);
end;

procedure TRttiJsonSaveContext.AddDateTime(Value: PDateTime; WithMS: boolean);
begin
  if woDateTimeWithMagic in Options then
    W.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR, 4)
  else
    W.Add('"');
  W.AddDateTime(Value^, WithMS);
  if woDateTimeWithZSuffix in Options then
    if frac(Value^) = 0 then // FireFox can't decode short form "2017-01-01Z"
      W.AddShorter('T00:00Z')
    else
      W.Add('Z');
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
              AddShorter('\u00');
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
      CP_UTF8:          // direct write of RawUTF8 content
        if Escape = twJSONEscape then
          Add(PUTF8Char(P), 0, Escape) // faster with no Len
        else
          Add(PUTF8Char(P), Len, Escape); // expects a supplied Len
      CP_RAWBYTESTRING: // direct write of RawByteString content
        Add(PUTF8Char(P), Len, Escape);
      CP_UTF16:         // direct write of UTF-16 content
        AddW(PWord(P), 0, Escape);
      CP_SQLRAWBLOB:    // TSQLRawBlob written with Base-64 encoding
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
      AddNull; // JSON null is better than "" for BLOBs
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

procedure TTextWriter.AddTimeLog(Value: PInt64; QuoteChar: AnsiChar);
begin
  if BEnd - B <= 31 then
    FlushToStream;
  B := PTimeLogBits(Value)^.Text(B + 1, true, 'T', QuoteChar) - 1;
end;

procedure TTextWriter.AddUnixTime(Value: PInt64; QuoteChar: AnsiChar);
var
  DT: TDateTime;
begin // inlined UnixTimeToDateTime()
  DT := Value^ / SecsPerDay + UnixDateDelta;
  AddDateTime(@DT, 'T', QuoteChar, {withms=}false, {dateandtime=}true);
end;

procedure TTextWriter.AddUnixMSTime(Value: PInt64; WithMS: boolean;
  QuoteChar: AnsiChar);
var
  DT: TDateTime;
begin // inlined UnixMSTimeToDateTime()
  DT := Value^ / MSecsPerDay + UnixDateDelta;
  AddDateTime(@DT, 'T', QuoteChar, WithMS, {dateandtime=}true);
end;

procedure TTextWriter.AddDateTime(Value: PDateTime; FirstChar: AnsiChar;
  QuoteChar: AnsiChar; WithMS: boolean; AlwaysDateAndTime: boolean);
var
  T: TSynSystemTime;
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
    if AlwaysDateAndTime or (trunc(Value^) <> 0) then
    begin
      T.FromDate(Date);
      B := DateToIso8601PChar(B, true, T.Year, T.Month, T.Day);
    end;
    if AlwaysDateAndTime or (frac(Value^) <> 0) then
    begin
      T.FromTime(Value^);
      B := TimeToIso8601PChar(B, true, T.Hour, T.Minute, T.Second, T.MilliSecond,
        FirstChar, WithMS);
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
    B := DateToIso8601PChar(Value, B, true);
  if frac(Value) <> 0 then
    B := TimeToIso8601PChar(Value, B, true, 'T', WithMS);
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

procedure TTextWriter.AddVariant(const Value: variant; Escape: TTextWriterKind;
  WriteOptions: TTextWriterWriteObjectOptions);
var
  cv: TSynInvokeableVariantType;
  v: TVarData absolute Value;
  vt: cardinal;
begin
  vt := v.VType;
  case vt of
    varEmpty, varNull:
      AddNull;
    varSmallint:
      Add(v.VSmallint);
    varShortInt:
      Add(v.VShortInt);
    varByte:
      AddU(v.VByte);
    varWord:
      AddU(v.VWord);
    varLongWord:
      AddU(v.VLongWord);
    varInteger:
      Add(v.VInteger);
    varInt64:
      Add(v.VInt64);
    varWord64:
      AddQ(v.VInt64);
    varSingle:
      AddSingle(v.VSingle);
    varDouble:
      AddDouble(v.VDouble);
    varDate:
      AddDateTime(@v.VDate, 'T', '"');
    varCurrency:
      AddCurr64(v.VInt64);
    varBoolean:
      Add(v.VBoolean); // 'true'/'false'
    varVariant:
      AddVariant(PVariant(v.VPointer)^, Escape);
    varString:
      AddText(RawByteString(v.VString), Escape);
    varOleStr {$ifdef HASVARUSTRING}, varUString{$endif}:
      AddTextW(v.VAny, Escape);
    varAny:
      // Value is a TRttiVarData from TRttiProp.GetValue: V.VAny = GetFieldAddr
      AddTypedJSON(TRttiVarData(V).Info, V.VAny, WriteOptions);
  else
    if vt = varVariant or varByRef then
      AddVariant(PVariant(v.VPointer)^, Escape)
    else if vt = varByRef or varString then
      AddText(PRawByteString(v.VAny)^, Escape)
    else if (vt = varByRef or varOleStr)
      {$ifdef HASVARUSTRING} or (vt = varByRef or varUString) {$endif} then
      AddTextW(PPointer(v.VAny)^, Escape)
    else if vt >= varArray then // complex types are always < varArray
      AddNull
    else if DocVariantType.FindSynVariantType(vt, cv) then
      cv.ToJSON(self, Value, Escape)
    else if not CustomVariantToJSON(self, Value, Escape) then
      raise ESynException.CreateUTF8('%.AddVariant VType=%', [self, vt]);
  end;
end;

function TTextWriter.AddTypedJSON(Value, TypeInfo: pointer;
  WriteOptions: TTextWriterWriteObjectOptions): PtrInt;
var
  ctxt: TRttiJsonSaveContext;
  save: TRttiJsonSave;
begin
  {%H-}ctxt.Init(self, WriteOptions, TypeInfo, nil);
  save := RTTI_JSONSAVE[PRttiInfo(TypeInfo)^.Kind];
  if Assigned(save) then
    result := save(Value, ctxt)
  else
  begin
    AddNull;
    result := PRttiInfo(TypeInfo)^.RttiSize;
  end;
end;


procedure TTextWriter.AddText(const Text: RawByteString; Escape: TTextWriterKind);
begin
  if Escape = twJSONEscape then
    Add('"');
  {$ifdef HASCODEPAGE}
  AddAnyAnsiString(Text, Escape);
  {$else}
  Add(pointer(Text), length(Text), Escape);
  {$endif}
  if Escape = twJSONEscape then
    Add('"');
end;

procedure TTextWriter.AddTextW(P: PWord; Escape: TTextWriterKind);
begin
  if Escape = twJSONEscape then
    Add('"');
  AddW(P, 0, Escape);
  if Escape = twJSONEscape then
    Add('"');
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
        exit; // [#0, ',', ']', '}', ':']
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
    dec(Len); // -1 = no end = AddJSONEscape(P, 0)
  i := 0;
  {$ifndef CPUX86NOTPIC}
  tab := @JSON_ESCAPE;
  {$endif}
  if tab[PByteArray(P)[i]] = 0 then
  begin
noesc:
    start := i;
    if Len < 0 then  // fastest loop is with AddJSONEscape(P, 0)
      repeat
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
    if B >= BEnd then
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
      AddShorter('\u00');
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
        AddNull;
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
  if (keys = nil) or (keys[0] <> '[') or (values = nil) or (values[0] <> '[') or
     (keys[1] = ']') or (values[1] = ']') then
  begin
    AddNull;
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


type
  TJSONObject =
    (oNone, oObject, oException, oSynException, oList, oObjectList,
     oCollection, oUtfs, oStrings, oPersistent,
     oCustomReaderWriter, oCustomPropName);

  (*
  type
    /// use a dedicated structure for easy maintenance of this recurcive code
    TWriteAsJSONContext = record
      W: TBaseWriter;
      Options: TTextWriterWriteObjectOptions;
      Instance: TObject;
      Added: boolean;
      PropName: PShortString;
      CustomComment: RawUTF8;
      tmpWS: WideString;
      {$ifdef HASVARUSTRING}
      tmpUS: UnicodeString;
      {$endif HASVARUSTRING}
      tmpS: RawByteString;
      tmpV: variant;
    end;

  procedure TRttiProp.WriteAsJSON(var ctxt: TWriteAsJSONContext);
  var
    info: PRttiInfo;
  begin
    ctxt.W.WriteObjectPropName(ctxt.PropName^, ctxt.Options);
    info := TypeInfo;

  end;
  *)

function TTextWriter.AddRecordJSON(Value: pointer; Info: PRttiInfo;
  WriteOptions: TTextWriterWriteObjectOptions): PtrInt;
begin
  result := AddTypedJSON(Value, Info, WriteOptions);
end;

procedure TTextWriter.AddVoidRecordJSON(Info: PRttiInfo;
  WriteOptions: TTextWriterWriteObjectOptions);
var
  tmp: TSynTempBuffer;
begin
  tmp.InitZero(Info^.RecordSize);
  AddRecordJSON(tmp.buf, Info, WriteOptions);
  tmp.Done;
end;

procedure TTextWriter.AddDynArrayJSON(var DynArray: TDynArray;
  WriteOptions: TTextWriterWriteObjectOptions);
var
  n: PtrInt;
  k: TRTTIParserType;
  P: PByte;
  jsonsave: TRttiJsonSave;
  ctxt: TRttiJsonSaveContext;
  tmp: RawByteString;
begin
  Add('[');
  n := DynArray.Count;
  if n > 0 then
  begin
    {%H-}ctxt.Init(self, WriteOptions, DynArray.ElemType, nil);
    // GlobalJSONCustomParsers.fParser[]...
    k := DynArray.GuessKnownType({exacttype=}true);
    P := DynArray.Value^;
    case k of
      ptNone:
        // fallback to binary serialization with Base64 encoding
        begin
          tmp := DynArray.SaveTo;
          WrBase64(pointer(tmp), length(tmp), {withMagic=}true);
        end;
      ptCustom:
        ;
    else
      begin
        // efficient JSON serialization from recognized TRTTIParserType
        if k = ptORM then
          jsonsave := PTC_JSONSAVE[DynArray.KnownComplexType]
        else
          jsonsave := PT_JSONSAVE[k];
        if Assigned(jsonsave) then
          repeat
            inc(P, jsonsave(P, ctxt));
            Add(',');
            dec(n);
          until n = 0
        else
          raise ESynException.CreateUTF8('%.AddDynArrayJSON(%): %?',
            [self, DynArray.ArrayType^.Name^, ToText(k)^]);
      end;
    end;
    CancelLastComma;
  end;
  Add(']');
end;

procedure TTextWriter.AddDynArrayJSON(var DynArray: TDynArrayHashed;
  WriteOptions: TTextWriterWriteObjectOptions);
begin // needed if UNDIRECTDYNARRAY is set (Delphi 2009+)
  AddDynArrayJSON(PDynArray(@DynArray)^, WriteOptions);
end;

function TTextWriter.AddDynArrayJSON(Value: pointer; Info: PRttiInfo;
  WriteOptions: TTextWriterWriteObjectOptions): PtrInt;
var
  temp: TDynArray;
begin
  temp.Init(Info, Value^);
  AddDynArrayJSON(temp, WriteOptions);
  result := temp.ElemSize;
end;



{ ************ JSON-aware TSynNameValue Name/Value RawUTF8 Storage }

{ TSynNameValue }

procedure TSynNameValue.Add(const aName, aValue: RawUTF8; aTag: PtrInt);
var
  added: boolean;
  i: Integer;
begin
  i := DynArray.FindHashedForAdding(aName, added);
  with List[i] do
  begin
    if added then
      Name := aName;
    Value := aValue;
    Tag := aTag;
  end;
  if Assigned(fOnAdd) then
    fOnAdd(List[i], i);
end;

procedure TSynNameValue.InitFromIniSection(Section: PUTF8Char;
  OnTheFlyConvert: TOnSynNameValueConvertRawUTF8; OnAdd: TOnSynNameValueNotify);
var
  s: RawUTF8;
  i: integer;
begin
  Init(false);
  fOnAdd := OnAdd;
  while (Section <> nil) and (Section^ <> '[') do
  begin
    s := GetNextLine(Section, Section);
    i := PosExChar('=', s);
    if (i > 1) and not (s[1] in [';', '[']) then
      if Assigned(OnTheFlyConvert) then
        Add(copy(s, 1, i - 1), OnTheFlyConvert(copy(s, i + 1, 1000)))
      else
        Add(copy(s, 1, i - 1), copy(s, i + 1, 1000));
  end;
end;

procedure TSynNameValue.InitFromCSV(CSV: PUTF8Char; NameValueSep, ItemSep: AnsiChar);
var
  n, v: RawUTF8;
begin
  Init(false);
  while CSV <> nil do
  begin
    GetNextItem(CSV, NameValueSep, n);
    if ItemSep = #10 then
      GetNextItemTrimedCRLF(CSV, v)
    else
      GetNextItem(CSV, ItemSep, v);
    if n = '' then
      break;
    Add(n, v);
  end;
end;

procedure TSynNameValue.InitFromNamesValues(const Names, Values: array of RawUTF8);
var
  i: integer;
begin
  Init(false);
  if high(Names) <> high(Values) then
    exit;
  DynArray.Capacity := length(Names);
  for i := 0 to high(Names) do
    Add(Names[i], Values[i]);
end;

function TSynNameValue.InitFromJSON(JSON: PUTF8Char; aCaseSensitive: boolean): boolean;
var
  N, V: PUTF8Char;
  nam, val: RawUTF8;
  Nlen, Vlen, c: integer;
  EndOfObject: AnsiChar;
begin
  result := false;
  Init(aCaseSensitive);
  if JSON = nil then
    exit;
  while (JSON^ <= ' ') and (JSON^ <> #0) do
    inc(JSON);
  if JSON^ <> '{' then
    exit;
  repeat
    inc(JSON)
  until (JSON^ = #0) or (JSON^ > ' ');
  c := JSONObjectPropCount(JSON);
  if c <= 0 then
    exit;
  DynArray.Capacity := c;
  repeat
    N := GetJSONPropName(JSON, @Nlen);
    if N = nil then
      exit;
    V := GetJSONFieldOrObjectOrArray(JSON, nil, @EndOfObject, true, true, @Vlen);
    if V = nil then
      exit;
    FastSetString(nam, N, Nlen);
    FastSetString(val, V, Vlen);
    Add(nam, val);
  until EndOfObject = '}';
  result := true;
end;

procedure TSynNameValue.Init(aCaseSensitive: boolean);
begin
  // release dynamic arrays memory before FillcharFast()
  List := nil;
  DynArray.Hasher.Clear;
  // initialize hashed storage
  FillCharFast(self, SizeOf(self), 0);
  DynArray.InitSpecific(TypeInfo(TSynNameValueItemDynArray), List,
    djRawUTF8, @Count, not aCaseSensitive);
end;

function TSynNameValue.Find(const aName: RawUTF8): integer;
begin
  result := DynArray.FindHashed(aName);
end;

function TSynNameValue.FindStart(const aUpperName: RawUTF8): integer;
begin
  for result := 0 to Count - 1 do
    if IdemPChar(pointer(List[result].Name), pointer(aUpperName)) then
      exit;
  result := -1;
end;

function TSynNameValue.FindByValue(const aValue: RawUTF8): integer;
begin
  for result := 0 to Count - 1 do
    if List[result].Value = aValue then
      exit;
  result := -1;
end;

function TSynNameValue.Delete(const aName: RawUTF8): boolean;
begin
  result := DynArray.FindHashedAndDelete(aName) >= 0;
end;

function TSynNameValue.DeleteByValue(const aValue: RawUTF8; Limit: integer): integer;
var
  ndx: integer;
begin
  result := 0;
  if Limit < 1 then
    exit;
  for ndx := Count - 1 downto 0 do
    if List[ndx].Value = aValue then
    begin
      DynArray.Delete(ndx);
      inc(result);
      if result >= Limit then
        break;
    end;
  if result > 0 then
    DynArray.ReHash;
end;

function TSynNameValue.Value(const aName: RawUTF8; const aDefaultValue: RawUTF8): RawUTF8;
var
  i: integer;
begin
  if @self = nil then
    i := -1
  else
    i := DynArray.FindHashed(aName);
  if i < 0 then
    result := aDefaultValue
  else
    result := List[i].Value;
end;

function TSynNameValue.ValueInt(const aName: RawUTF8; const aDefaultValue: Int64): Int64;
var
  i, err: integer;
begin
  i := DynArray.FindHashed(aName);
  if i < 0 then
    result := aDefaultValue
  else
  begin
    result := GetInt64(pointer(List[i].Value), err);
    if err <> 0 then
      result := aDefaultValue;
  end;
end;

function TSynNameValue.ValueBool(const aName: RawUTF8): Boolean;
begin
  result := Value(aName) = '1';
end;

function TSynNameValue.ValueEnum(const aName: RawUTF8; aEnumTypeInfo: pointer;
  out aEnum; aEnumDefault: byte): boolean;
var
  v: RawUTF8;
  err, i: integer;
begin
  result := false;
  byte(aEnum) := aEnumDefault;
  v := trim(Value(aName, ''));
  if v = '' then
    exit;
  i := GetInteger(pointer(v), err);
  if (err <> 0) or (i < 0) then
    i := GetEnumNameValue(aEnumTypeInfo, v, true);
  if i >= 0 then
  begin
    byte(aEnum) := i;
    result := true;
  end;
end;

function TSynNameValue.Initialized: boolean;
begin
  result := DynArray.Value = @List;
end;

function TSynNameValue.GetBlobData: RawByteString;
begin
  result := DynArray.SaveTo;
end;

procedure TSynNameValue.SetBlobDataPtr(aValue: pointer);
begin
  DynArray.LoadFrom(aValue);
  DynArray.ReHash;
end;

procedure TSynNameValue.SetBlobData(const aValue: RawByteString);
begin
  DynArray.LoadFromBinary(aValue);
  DynArray.ReHash;
end;

function TSynNameValue.GetStr(const aName: RawUTF8): RawUTF8;
begin
  result := Value(aName, '');
end;

function TSynNameValue.GetInt(const aName: RawUTF8): Int64;
begin
  result := ValueInt(aName, 0);
end;

function TSynNameValue.GetBool(const aName: RawUTF8): Boolean;
begin
  result := Value(aName) = '1';
end;

function TSynNameValue.AsCSV(const KeySeparator, ValueSeparator, IgnoreKey: RawUTF8): RawUTF8;
var
  i: integer;
  temp: TTextWriterStackBuffer;
begin
  with TBaseWriter.CreateOwnedStream(temp) do
  try
    for i := 0 to Count - 1 do
      if (IgnoreKey = '') or (List[i].Name <> IgnoreKey) then
      begin
        AddNoJSONEscapeUTF8(List[i].Name);
        AddNoJSONEscapeUTF8(KeySeparator);
        AddNoJSONEscapeUTF8(List[i].Value);
        AddNoJSONEscapeUTF8(ValueSeparator);
      end;
    SetText(result);
  finally
    Free;
  end;
end;

function TSynNameValue.AsJSON: RawUTF8;
var
  i: integer;
  temp: TTextWriterStackBuffer;
begin
  with TTextWriter.CreateOwnedStream(temp) do
  try
    Add('{');
    for i := 0 to Count - 1 do
      with List[i] do
      begin
        AddProp(pointer(Name), length(Name));
        Add('"');
        AddJSONEscape(pointer(Value));
        Add('"', ',');
      end;
    CancelLastComma;
    Add('}');
    SetText(result);
  finally
    Free;
  end;
end;

procedure TSynNameValue.AsNameValues(out Names, Values: TRawUTF8DynArray);
var
  i: integer;
begin
  SetLength(Names, Count);
  SetLength(Values, Count);
  for i := 0 to Count - 1 do
  begin
    Names[i] := List[i].Name;
    Values[i] := List[i].Value;
  end;
end;

function TSynNameValue.ValueVariantOrNull(const aName: RawUTF8): variant;
var
  i: integer;
begin
  i := Find(aName);
  if i < 0 then
    SetVariantNull(result{%H-})
  else
    RawUTF8ToVariant(List[i].Value, result);
end;

procedure TSynNameValue.AsDocVariant(out DocVariant: variant;
  ExtendedJson, ValueAsString, AllowVarDouble: boolean);
var
  ndx: PtrInt;
  dv: TDocVariantData absolute DocVariant;
begin
  if Count > 0 then
    begin
      dv.Init(JSON_OPTIONS_NAMEVALUE[ExtendedJson], dvObject);
      dv.SetCount(Count);
      dv.Capacity := Count;
      for ndx := 0 to Count - 1 do
      begin
        dv.Names[ndx] := List[ndx].Name;
        if ValueAsString or
           not GetNumericVariantFromJSON(pointer(List[ndx].Value),
             TVarData(dv.Values[ndx]), AllowVarDouble) then
          RawUTF8ToVariant(List[ndx].Value, dv.Values[ndx]);
      end;
    end
  else
    TVarData(DocVariant).VType := varNull;
end;

function TSynNameValue.AsDocVariant(ExtendedJson, ValueAsString: boolean): variant;
begin
  AsDocVariant(result, ExtendedJson, ValueAsString);
end;

function TSynNameValue.MergeDocVariant(var DocVariant: variant;
  ValueAsString: boolean; ChangedProps: PVariant; ExtendedJson,
  AllowVarDouble: Boolean): integer;
var
  dv: TDocVariantData absolute DocVariant;
  i, ndx: PtrInt;
  v: variant;
  intvalues: TRawUTF8Interning;
begin
  if dv.VarType <> DocVariantVType then
    TDocVariant.New(DocVariant, JSON_OPTIONS_NAMEVALUE[ExtendedJson]);
  if ChangedProps <> nil then
    TDocVariant.New(ChangedProps^, dv.Options);
  if dvoInternValues in dv.Options then
    intvalues := DocVariantType.InternValues
  else
    intvalues := nil;
  result := 0; // returns number of changed values
  for i := 0 to Count - 1 do
    if List[i].Name <> '' then
    begin
      VarClear(v);
      if ValueAsString or
         not GetNumericVariantFromJSON(pointer(List[i].Value),
           TVarData(v), AllowVarDouble) then
        RawUTF8ToVariant(List[i].Value, v);
      ndx := dv.GetValueIndex(List[i].Name);
      if ndx < 0 then
        ndx := dv.InternalAdd(List[i].Name)
      else if SortDynArrayVariantComp(TVarData(v), TVarData(dv.Values[ndx]), false) = 0 then
        continue; // value not changed -> skip
      if ChangedProps <> nil then
        PDocVariantData(ChangedProps)^.AddValue(List[i].Name, v);
      SetVariantByValue(v, dv.Values[ndx]);
      if intvalues <> nil then
        intvalues.UniqueVariant(dv.Values[ndx]);
      inc(result);
    end;
end;


{ *********** JSON-aware TSynDictionary Storage }

{ TSynDictionary }

const
  DIC_KEYCOUNT = 0;
  DIC_KEY = 1;
  DIC_VALUECOUNT = 2;
  DIC_VALUE = 3;
  DIC_TIMECOUNT = 4;
  DIC_TIMESEC = 5;
  DIC_TIMETIX = 6;

function TSynDictionary.KeyFullHash(const Elem): cardinal;
begin
  result := fKeys.Hasher.Hasher(0, @Elem, fKeys.ElemSize);
end;

function TSynDictionary.KeyFullCompare(const A, B): integer;
var
  i: PtrInt;
begin

  for i := 0 to fKeys.ElemSize - 1 do
  begin
    result := TByteArray(A)[i] - TByteArray(B)[i];
    if result <> 0 then
      exit;
  end;
  result := 0;
end;

constructor TSynDictionary.Create(aKeyTypeInfo, aValueTypeInfo: pointer;
  aKeyCaseInsensitive: boolean; aTimeoutSeconds: cardinal; aCompressAlgo: TAlgoCompress);
begin
  inherited Create;
  fSafe.Padding[DIC_KEYCOUNT].VType := varInteger;
  fSafe.Padding[DIC_KEY].VType := varUnknown;
  fSafe.Padding[DIC_VALUECOUNT].VType := varInteger;
  fSafe.Padding[DIC_VALUE].VType := varUnknown;
  fSafe.Padding[DIC_TIMECOUNT].VType := varInteger;
  fSafe.Padding[DIC_TIMESEC].VType := varInteger;
  fSafe.Padding[DIC_TIMETIX].VType := varInteger;
  fSafe.PaddingUsedCount := DIC_TIMETIX + 1;
  fKeys.Init(aKeyTypeInfo, fSafe.Padding[DIC_KEY].VAny, nil, nil, nil,
    @fSafe.Padding[DIC_KEYCOUNT].VInteger, aKeyCaseInsensitive);
  if not Assigned(fKeys.HashItem) then
    fKeys.EventHash := KeyFullHash;
  if not Assigned(fKeys.{$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$endif}Compare) then
    fKeys.EventCompare := KeyFullCompare;
  fValues.Init(aValueTypeInfo, fSafe.Padding[DIC_VALUE].VAny,
    @fSafe.Padding[DIC_VALUECOUNT].VInteger);
  fTimeouts.Init(TypeInfo(TIntegerDynArray), fTimeOut,
    @fSafe.Padding[DIC_TIMECOUNT].VInteger);
  if aCompressAlgo = nil then
    aCompressAlgo := AlgoSynLZ;
  fCompressAlgo := aCompressAlgo;
  fSafe.Padding[DIC_TIMESEC].VInteger := aTimeoutSeconds;
end;

function TSynDictionary.ComputeNextTimeOut: cardinal;
begin
  result := fSafe.Padding[DIC_TIMESEC].VInteger;
  if result <> 0 then
    result := cardinal(GetTickCount64 shr 10) + result;
end;

function TSynDictionary.GetCapacity: integer;
begin
  fSafe.Lock;
  result := fKeys.Capacity;
  fSafe.UnLock;
end;

procedure TSynDictionary.SetCapacity(const Value: integer);
begin
  fSafe.Lock;
  fKeys.Capacity := Value;
  fValues.Capacity := Value;
  if fSafe.Padding[DIC_TIMESEC].VInteger > 0 then
    fTimeOuts.Capacity := Value;
  fSafe.UnLock;
end;

function TSynDictionary.GetTimeOutSeconds: cardinal;
begin
  result := fSafe.Padding[DIC_TIMESEC].VInteger;
end;

procedure TSynDictionary.SetTimeouts;
var
  i: PtrInt;
  timeout: cardinal;
begin
  if fSafe.Padding[DIC_TIMESEC].VInteger = 0 then
    exit;
  fTimeOuts.Count := fSafe.Padding[DIC_KEYCOUNT].VInteger;
  timeout := ComputeNextTimeOut;
  for i := 0 to fSafe.Padding[DIC_TIMECOUNT].VInteger - 1 do
    fTimeOut[i] := timeout;
end;

function TSynDictionary.DeleteDeprecated: integer;
var
  i: PtrInt;
  now: cardinal;
begin
  result := 0;
  if (self = nil) or (fSafe.Padding[DIC_TIMECOUNT].VInteger = 0) or // no entry
    (fSafe.Padding[DIC_TIMESEC].VInteger = 0) then // nothing in fTimeOut[]
    exit;
  now := GetTickCount64 shr 10;
  if fSafe.Padding[DIC_TIMETIX].VInteger = integer(now) then
    exit; // no need to search more often than every second
  fSafe.Lock;
  try
    fSafe.Padding[DIC_TIMETIX].VInteger := now;
    for i := fSafe.Padding[DIC_TIMECOUNT].VInteger - 1 downto 0 do
      if (now > fTimeOut[i]) and (fTimeOut[i] <> 0) and
         (not Assigned(fOnCanDelete) or
          fOnCanDelete(fKeys.ItemPtr(i)^, fValues.ItemPtr(i)^, i)) then
      begin
        fKeys.Delete(i);
        fValues.Delete(i);
        fTimeOuts.Delete(i);
        inc(result);
      end;
    if result > 0 then
      fKeys.Rehash; // mandatory after fKeys.Delete(i)
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynDictionary.DeleteAll;
begin
  if self = nil then
    exit;
  fSafe.Lock;
  try
    fKeys.Clear;
    fKeys.Hasher.Clear; // mandatory to avoid GPF
    fValues.Clear;
    if fSafe.Padding[DIC_TIMESEC].VInteger > 0 then
      fTimeOuts.Clear;
  finally
    fSafe.UnLock;
  end;
end;

destructor TSynDictionary.Destroy;
begin
  fKeys.Clear;
  fValues.Clear;
  inherited Destroy;
end;

function TSynDictionary.Add(const aKey, aValue): integer;
var
  added: boolean;
  tim: cardinal;
begin
  fSafe.Lock;
  try
    result := fKeys.FindHashedForAdding(aKey, added);
    if added then
    begin
      with fKeys{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif} do
        ItemCopyFrom(@aKey, result); // fKey[result] := aKey;
      if fValues.Add(aValue) <> result then
        raise ESynException.CreateUTF8('%.Add fValues.Add', [self]);
      tim := ComputeNextTimeOut;
      if tim > 0 then
        fTimeOuts.Add(tim);
    end
    else
      result := -1;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.AddOrUpdate(const aKey, aValue): integer;
var
  added: boolean;
  tim: cardinal;
begin
  fSafe.Lock;
  try
    tim := ComputeNextTimeOut;
    result := fKeys.FindHashedForAdding(aKey, added);
    if added then
    begin
      with fKeys{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif} do
        ItemCopyFrom(@aKey, result); // fKey[result] := aKey
      if fValues.Add(aValue) <> result then
        raise ESynException.CreateUTF8('%.AddOrUpdate fValues.Add', [self]);
      if tim <> 0 then
        fTimeOuts.Add(tim);
    end
    else
    begin
      fValues.ItemCopyFrom(@aValue, result, {ClearBeforeCopy=}true);
      if tim <> 0 then
        fTimeOut[result] := tim;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.Clear(const aKey): integer;
begin
  fSafe.Lock;
  try
    result := fKeys.FindHashed(aKey);
    if result >= 0 then
    begin
      fValues.ItemClear(fValues.ItemPtr(result));
      if fSafe.Padding[DIC_TIMESEC].VInteger > 0 then
        fTimeOut[result] := 0;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.Delete(const aKey): integer;
begin
  fSafe.Lock;
  try
    result := fKeys.FindHashedAndDelete(aKey);
    if result >= 0 then
    begin
      fValues.Delete(result);
      if fSafe.Padding[DIC_TIMESEC].VInteger > 0 then
        fTimeOuts.Delete(result);
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.DeleteAt(aIndex: integer): boolean;
begin
  if cardinal(aIndex) < cardinal(fSafe.Padding[DIC_KEYCOUNT].VInteger) then
    // use Delete(aKey) to have efficient hash table update
    result := Delete(fKeys.ItemPtr(aIndex)^) = aIndex
  else
    result := false;
end;

function TSynDictionary.InArray(const aKey, aArrayValue;
  aAction: TSynDictionaryInArray; aCompare: TDynArraySortCompare): boolean;
var
  nested: TDynArray;
  ndx: integer;
begin
  result := false;
  if (fValues.ElemType = nil) or (fValues.ElemType^.Kind <> rkDynArray) then
    raise ESynException.CreateUTF8('%.Values: % items are not dynamic arrays',
      [self, fValues.ArrayTypeShort^]);
  fSafe.Lock;
  try
    ndx := fKeys.FindHashed(aKey);
    if ndx < 0 then
      exit;
    nested.Init(fValues.ElemType, fValues.ItemPtr(ndx)^);
    nested.Compare := aCompare;
    case aAction of
      iaFind:
        result := nested.Find(aArrayValue) >= 0;
      iaFindAndDelete:
        result := nested.FindAndDelete(aArrayValue) >= 0;
      iaFindAndUpdate:
        result := nested.FindAndUpdate(aArrayValue) >= 0;
      iaFindAndAddIfNotExisting:
        result := nested.FindAndAddIfNotExisting(aArrayValue) >= 0;
      iaAdd:
        result := nested.Add(aArrayValue) >= 0;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.FindInArray(const aKey, aArrayValue;
  aCompare: TDynArraySortCompare): boolean;
begin
  result := InArray(aKey, aArrayValue, iaFind, aCompare);
end;

function TSynDictionary.FindKeyFromValue(const aValue;
  out aKey; aUpdateTimeOut: boolean): boolean;
var
  ndx: integer;
begin
  fSafe.Lock;
  try
    ndx := fValues.IndexOf(aValue); // use fast RTTI for value search
    result := ndx >= 0;
    if result then
    begin
      fKeys.ItemCopyAt(ndx, @aKey);
      if aUpdateTimeOut then
        SetTimeoutAtIndex(ndx);
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.DeleteInArray(const aKey, aArrayValue;
  aCompare: TDynArraySortCompare): boolean;
begin
  result := InArray(aKey, aArrayValue, iaFindAndDelete, aCompare);
end;

function TSynDictionary.UpdateInArray(const aKey, aArrayValue;
  aCompare: TDynArraySortCompare): boolean;
begin
  result := InArray(aKey, aArrayValue, iaFindAndUpdate, aCompare);
end;

function TSynDictionary.AddInArray(const aKey, aArrayValue;
  aCompare: TDynArraySortCompare): boolean;
begin
  result := InArray(aKey, aArrayValue, iaAdd, aCompare);
end;

function TSynDictionary.AddOnceInArray(const aKey, aArrayValue;
  aCompare: TDynArraySortCompare): boolean;
begin
  result := InArray(aKey, aArrayValue, iaFindAndAddIfNotExisting, aCompare);
end;

function TSynDictionary.Find(const aKey; aUpdateTimeOut: boolean): integer;
var
  tim: cardinal;
begin // caller is expected to call fSafe.Lock/Unlock
  if self = nil then
    result := -1
  else
    result := fKeys.FindHashed(aKey);
  if aUpdateTimeOut and (result >= 0) then
  begin
    tim := fSafe.Padding[DIC_TIMESEC].VInteger;
    if tim > 0 then // inlined fTimeout[result] := GetTimeout
      fTimeout[result] := cardinal(GetTickCount64 shr 10) + tim;
  end;
end;

function TSynDictionary.FindValue(const aKey; aUpdateTimeOut: boolean;
  aIndex: PInteger): pointer;
var
  ndx: PtrInt;
begin
  ndx := Find(aKey, aUpdateTimeOut);
  if aIndex <> nil then
    aIndex^ := ndx;
  if ndx < 0 then
    result := nil
  else
    result := PAnsiChar(fValues.Value^) + PtrUInt(ndx) * fValues.ElemSize;
end;

function TSynDictionary.FindValueOrAdd(const aKey; var added: boolean;
  aIndex: PInteger): pointer;
var
  ndx: integer;
  tim: cardinal;
begin
  tim := fSafe.Padding[DIC_TIMESEC].VInteger; // inlined tim := GetTimeout
  if tim <> 0 then
    tim := cardinal(GetTickCount64 shr 10) + tim;
  ndx := fKeys.FindHashedForAdding(aKey, added);
  if added then
  begin
    with fKeys{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif} do
      ItemCopyFrom(@aKey, ndx); // fKey[i] := aKey
    fValues.Count := ndx + 1; // reserve new place for associated value
    if tim > 0 then
      fTimeOuts.Add(tim);
  end
  else if tim > 0 then
    fTimeOut[ndx] := tim;
  if aIndex <> nil then
    aIndex^ := ndx;
  result := fValues.ItemPtr(ndx);
end;

function TSynDictionary.FindAndCopy(const aKey;
  out aValue; aUpdateTimeOut: boolean): boolean;
var
  ndx: integer;
begin
  fSafe.Lock;
  try
    ndx := Find(aKey, aUpdateTimeOut);
    if ndx >= 0 then
    begin
      fValues.ItemCopyAt(ndx, @aValue);
      result := true;
    end
    else
      result := false;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.FindAndExtract(const aKey; out aValue): boolean;
var
  ndx: integer;
begin
  fSafe.Lock;
  try
    ndx := fKeys.FindHashedAndDelete(aKey);
    if ndx >= 0 then
    begin
      fValues.ItemCopyAt(ndx, @aValue);
      fValues.Delete(ndx);
      if fSafe.Padding[DIC_TIMESEC].VInteger > 0 then
        fTimeOuts.Delete(ndx);
      result := true;
    end
    else
      result := false;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.Exists(const aKey): boolean;
begin
  fSafe.Lock;
  try
    result := fKeys.FindHashed(aKey) >= 0;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynDictionary.CopyValues(out Dest; ObjArrayByRef: boolean);
begin
  fSafe.Lock;
  try
    fValues.CopyTo(Dest, ObjArrayByRef);
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.ForEach(const OnEach: TSynDictionaryEvent;
  Opaque: pointer): integer;
var
  k, v: PAnsiChar;
  i, n, ks, vs: integer;
begin
  result := 0;
  fSafe.Lock;
  try
    n := fSafe.Padding[DIC_KEYCOUNT].VInteger;
    if (n = 0) or not Assigned(OnEach) then
      exit;
    k := fKeys.Value^;
    ks := fKeys.ElemSize;
    v := fValues.Value^;
    vs := fValues.ElemSize;
    for i := 0 to n - 1 do
    begin
      inc(result);
      if not OnEach(k^, v^, i, n, Opaque) then
        break;
      inc(k, ks);
      inc(v, vs);
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.ForEach(const OnMatch: TSynDictionaryEvent;
  KeyCompare, ValueCompare: TDynArraySortCompare; const aKey, aValue;
  Opaque: pointer): integer;
var
  k, v: PAnsiChar;
  i, n, ks, vs: integer;
begin
  fSafe.Lock;
  try
    result := 0;
    if not Assigned(OnMatch) or
       (not Assigned(KeyCompare) and not Assigned(ValueCompare)) then
      exit;
    n := fSafe.Padding[DIC_KEYCOUNT].VInteger;
    k := fKeys.Value^;
    ks := fKeys.ElemSize;
    v := fValues.Value^;
    vs := fValues.ElemSize;
    for i := 0 to n - 1 do
    begin
      if (Assigned(KeyCompare) and (KeyCompare(k^, aKey) = 0)) or
         (Assigned(ValueCompare) and (ValueCompare(v^, aValue) = 0)) then
      begin
        inc(result);
        if not OnMatch(k^, v^, i, n, Opaque) then
          break;
      end;
      inc(k, ks);
      inc(v, vs);
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynDictionary.SetTimeoutAtIndex(aIndex: integer);
var
  tim: cardinal;
begin
  if cardinal(aIndex) >= cardinal(fSafe.Padding[DIC_KEYCOUNT].VInteger) then
    exit;
  tim := fSafe.Padding[DIC_TIMESEC].VInteger;
  if tim > 0 then
    fTimeOut[aIndex] := cardinal(GetTickCount64 shr 10) + tim;
end;

function TSynDictionary.Count: integer;
begin
  result := fSafe.LockedInt64[DIC_KEYCOUNT];
end;

function TSynDictionary.RawCount: integer;
begin
  result := fSafe.Padding[DIC_KEYCOUNT].VInteger;
end;

procedure TSynDictionary.SaveToJSON(W: TTextWriter; EnumSetsAsText: boolean);
var
  k, v: RawUTF8;
begin
  fSafe.Lock;
  try
    if fSafe.Padding[DIC_KEYCOUNT].VInteger > 0 then
    begin
      fKeys.{$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$endif}
        SaveToJSON(k, EnumSetsAsText);
      fValues.SaveToJSON(v, EnumSetsAsText);
    end;
  finally
    fSafe.UnLock;
  end;
  W.AddJSONArraysAsJSONObject(pointer(k), pointer(v));
end;

function TSynDictionary.SaveToJSON(EnumSetsAsText: boolean): RawUTF8;
var
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  W := DefaultTextWriterSerializer.CreateOwnedStream(temp) as TTextWriter;
  try
    SaveToJSON(W, EnumSetsAsText);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TSynDictionary.SaveValuesToJSON(EnumSetsAsText: boolean): RawUTF8;
begin
  fSafe.Lock;
  try
    fValues.SaveToJSON(result, EnumSetsAsText);
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.LoadFromJSON(const JSON: RawUTF8;
  CustomVariantOptions: PDocVariantOptions): boolean;
begin // pointer(JSON) is not modified in-place thanks to JSONObjectAsJSONArrays()
  result := LoadFromJSON(pointer(JSON), CustomVariantOptions);
end;

function TSynDictionary.LoadFromJSON(JSON: PUTF8Char;
  CustomVariantOptions: PDocVariantOptions): boolean;
var
  k, v: RawUTF8; // private copy of the JSON input, expanded as Keys/Values arrays
begin
  result := false;
  if not JSONObjectAsJSONArrays(JSON, k, v) then
    exit;
  fSafe.Lock;
  try
    if fKeys.LoadFromJSON(pointer(k), nil, CustomVariantOptions) <> nil then
      if fValues.LoadFromJSON(pointer(v), nil, CustomVariantOptions) <> nil then
        if fKeys.Count = fValues.Count then
        begin
          SetTimeouts;
          fKeys.Rehash; // warning: duplicated keys won't be identified
          result := true;
        end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynDictionary.LoadFromBinary(const binary: RawByteString): boolean;
var
  plain: RawByteString;
  reader: TFastReader;
begin
  result := false;
  plain := fCompressAlgo.Decompress(binary);
  if plain = '' then
    exit;
  reader.Init(plain);
  fSafe.Lock;
  try
    try
      RTTI_BINARYLOAD[rkDynArray](fKeys.Value, reader, fKeys.ArrayType);
      RTTI_BINARYLOAD[rkDynArray](fValues.Value, reader, fValues.ArrayType);
      if fKeys.Count = fValues.Count then
      begin
        SetTimeouts;  // set ComputeNextTimeOut for all items
        fKeys.ReHash; // optimistic: input from safe TSynDictionary.SaveToBinary
        result := true;
      end;
    except
      result := false;
    end;
  finally
    fSafe.UnLock;
  end;
end;

class function TSynDictionary.OnCanDeleteSynPersistentLock(const aKey, aValue;
  aIndex: integer): boolean;
begin
  result := not TSynPersistentLock(aValue).Safe^.IsLocked;
end;

class function TSynDictionary.OnCanDeleteSynPersistentLocked(const aKey, aValue;
  aIndex: integer): boolean;
begin
  result := not TSynPersistentLock(aValue).Safe.IsLocked;
end;

function TSynDictionary.SaveToBinary(NoCompression: boolean;
  Algo: TAlgoCompress): RawByteString;
var
  tmp: TTextWriterStackBuffer;
  W: TBufferWriter;
begin
  fSafe.Lock;
  try
    result := '';
    if fSafe.Padding[DIC_KEYCOUNT].VInteger = 0 then
      exit;
    W := TBufferWriter.Create(tmp);
    try
      RTTI_BINARYSAVE[rkDynArray](fKeys.Value, W, fKeys.ArrayType);
      RTTI_BINARYSAVE[rkDynArray](fValues.Value, W, fValues.ArrayType);
      result := W.FlushAndCompress(NoCompression, Algo);
    finally
      W.Free;
    end;
  finally
    fSafe.UnLock;
  end;
end;


{ ********** JSON Serialization Wrapper Functions }

procedure SaveJSON(const Value; TypeInfo: PRttiInfo; Options: TTextWriterOptions;
  var result: RawUTF8);
var
  temp: TTextWriterStackBuffer;
begin
  with DefaultTextWriterSerializer.CreateOwnedStream(temp) do
  try
    CustomOptions := CustomOptions + Options;
    AddTypedJSON(TypeInfo, @Value);
    SetText(result);
  finally
    Free;
  end;
end;

function SaveJSON(const Value; TypeInfo: PRttiInfo; EnumSetsAsText: boolean): RawUTF8;
var
  options: TTextWriterOptions;
begin
  if EnumSetsAsText then
    options := [twoEnumSetsAsTextInRecord, twoFullSetsAsStar]
  else
    options := [twoFullSetsAsStar];
  SaveJSON(Value, TypeInfo, options, result);
end;

function RecordSaveJSON(const Rec; TypeInfo: PRttiInfo;
  EnumSetsAsText: boolean): RawUTF8;
begin
  if TypeInfo^.Kind in rkRecordTypes then
    result := SaveJSON(Rec, TypeInfo, EnumSetsAsText)
  else
    result := NULL_STR_VAR;
end;

function DynArraySaveJSON(const Value; TypeInfo: PRttiInfo;
  EnumSetsAsText: boolean): RawUTF8;
begin
  if (PPointer(Value)^ = nil) or (TypeInfo^.Kind <> rkDynArray) then
    result := NULL_STR_VAR
  else
  result := SaveJSON(Value, TypeInfo, EnumSetsAsText);
end;

function ObjectToJSON(Value: TObject;
  Options: TTextWriterWriteObjectOptions): RawUTF8;
var
  temp: TTextWriterStackBuffer;
begin
  if Value = nil then
    result := NULL_STR_VAR
  else
    with DefaultTextWriterSerializer.CreateOwnedStream(temp) do
    try
      CustomOptions := CustomOptions + [twoForceJSONStandard];
      WriteObject(Value, Options);
      SetText(result);
    finally
      Free;
    end;
end;

function ObjectsToJSON(const Names: array of RawUTF8; const Values: array of TObject;
  Options: TTextWriterWriteObjectOptions): RawUTF8;
var
  i, n: PtrInt;
  temp: TTextWriterStackBuffer;
begin
  with DefaultTextWriterSerializer.CreateOwnedStream(temp) do
  try
    n := length(Names);
    Add('{');
    for i := 0 to high(Values) do
      if Values[i] <> nil then
      begin
        if i < n then
          AddFieldName(Names[i])
        else
          AddPropName(ClassNameShort(Values[i])^);
        WriteObject(Values[i], Options);
        Add(',');
      end;
    CancelLastComma;
    Add('}');
    SetText(result);
  finally
    Free;
  end;
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
  // JSON serialization
  RTTI_JSONSAVE[rkEnumeration] := @_JS_Enumeration;
  RTTI_JSONSAVE[rkSet]         := @_JS_Set;
  RTTI_JSONSAVE[rkClass]       := @_JS_Class;
  RTTI_JSONSAVE[rkVariant]     := @_JS_Variant;
  RTTI_JSONSAVE[rkDynArray]    := @_JS_Array;
  RTTI_JSONSAVE[rkRecord]      := @_JS_Record;
  {$ifdef FPC}
  RTTI_JSONSAVE[rkObject]      := @_JS_Record;
  {$endif FPC}
end;

initialization
  InitializeConstants;
  DefaultTextWriterSerializer := TTextWriter;
end.

