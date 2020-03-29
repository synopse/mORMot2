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

type
  /// kind of character used from JSON_CHARS[] for efficient JSON parsing
  // - using such a set compiles into TEST [MEM], IMM so is more efficient
  // than a regular set of AnsiChar which generates much slower BT [MEM], IMM
  // - the same 256-byte memory will also be reused from L1 CPU cache
  // during the parsing of complex JSON input
  TJsonChar = set of (jcJsonIdentifierFirstChar, jcJsonIdentifier,
    jcEndOfJSONField, jcEndOfJSONFieldOr0, jcEndOfJSONValueField,
    jcDigitChars, jcDigitFirstChars, jcDigitFloatChars);

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



{ ********** TTextWriter class with proper JSON escaping and WriteObject() support }

type
  TTextWriter = class(TBaseWriter)
  public
{
    procedure AddVariant(const Value: variant; Escape: TTextWriterKind = twJSONEscape); virtual;
    function AddJSONReformat(JSON: PUTF8Char; Format: TTextWriterJSONFormat;
      EndOfObject: PUTF8Char): PUTF8Char; virtual;
    procedure WriteObject(Value: TObject;
      Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]); virtual;
}
  end;

implementation

uses
  mormot.core.rtti;


const
  NULL_LOW = ord('n') + ord('u') shl 8 + ord('l') shl 16 + ord('l') shl 24;
  FALSE_LOW = ord('f') + ord('a') shl 8 + ord('l') shl 16 + ord('s') shl 24;
  FALSE_LOW2 = ord('a') + ord('l') shl 8 + ord('s') shl 16 + ord('e') shl 24;
  TRUE_LOW = ord('t') + ord('r') shl 8 + ord('u') shl 16 + ord('e') shl 24;
  NULL_UPP = ord('N') + ord('U') shl 8 + ord('L') shl 16 + ord('L') shl 24;


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

// note: JsonEscapeToUtf8() is inlined below otherwise the code is slower

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
          else if c = 'u' then
          begin // inlined decoding of '\u0123' UTF-16 codepoint(s) into UTF-8
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
          if not (jcDigitFloatChars in jsonset[P^]) then
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


{ ********** TTextWriter class with proper JSON escaping and WriteObject() support }



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
    if c in [',',']','}',':'] then begin
      include(JSON_CHARS[c], jcEndOfJSONField);
      include(JSON_CHARS[c], jcEndOfJSONFieldOr0);
    end;
    if c in [#0,#9,#10,#13,' ',',','}',']'] then
      include(JSON_CHARS[c], jcEndOfJSONValueField);
    if c in ['-','+','0'..'9'] then
      include(JSON_CHARS[c], jcDigitChars);
    if c in ['-','1'..'9'] then // 0### excluded by JSON
      include(JSON_CHARS[c], jcDigitFirstChars);
    if c in ['-','+','0'..'9','.','E','e'] then
      include(JSON_CHARS[c], jcDigitFloatChars);
    if c in ['_','0'..'9','a'..'z','A'..'Z','$'] then
      include(JSON_CHARS[c], jcJsonIdentifierFirstChar);
    if c in ['_','0'..'9','a'..'z','A'..'Z','.','[',']'] then
      include(JSON_CHARS[c], jcJsonIdentifier);
  end;
end;

initialization
  InitializeConstants;

end.





