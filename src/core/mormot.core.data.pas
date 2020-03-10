/// Framework Core Low-Level Data Processing Functions
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.data;

{
  *****************************************************************************

   Low-Level Data Processing Functions shared by all framework units
    - Variable Length Integer Encoding / Decoding
    - Base64 and Base64URI Encoding / Decoding
    - INI Files and In-memory Access

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  Classes,
  Contnrs,
  Types,
  SysUtils,
  mormot.core.base;


{ ************ Variable Length Integer Encoding / Decoding }

/// convert a cardinal into a 32-bit variable-length integer buffer
function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;

/// return the number of bytes necessary to store a 32-bit variable-length integer
// - i.e. the ToVarUInt32() buffer size
function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// return the number of bytes necessary to store some data with a its
// 32-bit variable-length integer legnth
function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an integer into a 32-bit variable-length integer buffer
// - store negative values as cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - fast inlined process for any number < 128
// - use overloaded FromVarUInt32() or FromVarUInt32Safe() with a SourceMax
// pointer to avoid any potential buffer overflow
function FromVarUInt32(var Source: PByte): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// safely convert a 32-bit variable-length integer buffer into a cardinal
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - will call FromVarUInt32() if SourceMax=nil, or FromVarUInt32Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt32(var Source: PByte; SourceMax: PByte; out Value: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version could be called if number is likely to be > $7f, so it
// inlining the first byte won't make any benefit
function FromVarUInt32Big(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - used e.g. when inlining FromVarUInt32()
// - this version must be called if Source^ has already been checked to be > $7f
// ! result := Source^;
// ! inc(Source);
// ! if result>$7f then
// !   result := (result and $7F) or FromVarUInt32Up128(Source);
function FromVarUInt32Up128(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version must be called if Source^ has already been checked to be > $7f
function FromVarUInt32High(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into an integer
// - decode negative values from cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function FromVarInt32(var Source: PByte): integer;

/// convert a UInt64 into a 64-bit variable-length integer buffer
function ToVarUInt64(Value: QWord; Dest: PByte): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
function FromVarUInt64(var Source: PByte): QWord; overload;

/// safely convert a 64-bit variable-length integer buffer into a UInt64
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
// - will call FromVarUInt64() if SourceMax=nil, or FromVarUInt64Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt64(var Source: PByte; SourceMax: PByte; out Value: Qword): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a Int64 into a 64-bit variable-length integer buffer
function ToVarInt64(Value: Int64; Dest: PByte): PByte; {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit variable-length integer buffer into a Int64
function FromVarInt64(var Source: PByte): Int64;

/// convert a 64-bit variable-length integer buffer into a Int64
// - this version won't update the Source pointer
function FromVarInt64Value(Source: PByte): Int64;

/// jump a value in the 32-bit or 64-bit variable-length integer buffer
function GotoNextVarInt(Source: PByte): pointer; {$ifdef HASINLINE}inline;{$endif}


{ ************ Base64 and Base64URI Encoding / Decoding }

const
  /// UTF-8 encoded \uFFF0 special code to mark Base64 binary content in JSON
  // - Unicode special char U+FFF0 is UTF-8 encoded as EF BF B0 bytes
  // - as generated by BinToBase64WithMagic() functions, and expected by
  // SQLParamContent() and ExtractInlineParameters() functions
  // - used e.g. when transmitting TDynArray.SaveTo() content
  JSON_BASE64_MAGIC = $b0bfef;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  JSON_BASE64_MAGIC_QUOTE = ord('"')+cardinal(JSON_BASE64_MAGIC) shl 8;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  // - defined as a cardinal variable to be used as:
  // ! AddNoJSONEscape(@JSON_BASE64_MAGIC_QUOTE_VAR,4);
  JSON_BASE64_MAGIC_QUOTE_VAR: cardinal = JSON_BASE64_MAGIC_QUOTE;

/// just a wrapper around Base64ToBin() for in-place decode of JSON_BASE64_MAGIC
// '\uFFF0base64encodedbinary' content into binary
// - input ParamValue shall have been checked to match the expected pattern
procedure Base64MagicDecode(var ParamValue: RawUTF8);

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: RawByteString): boolean; overload;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUTF8Char; ValueLen: Integer;
  var Blob: RawByteString): boolean; overload;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: TSynTempBuffer): boolean; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(const s: RawByteString): RawUTF8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUTF8; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(const s: RawByteString): shortstring; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): shortstring; overload;

/// fast conversion from binary data into prefixed/suffixed Base64 encoded UTF-8 text
// - with optional JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUTF8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(const data: RawByteString): RawUTF8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUTF8; overload;

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if s was not a valid Base64-encoded input
function Base64ToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if sp/len buffer was not a valid Base64-encoded input
function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns false and data='' if sp/len buffer was invalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if sp/len buffer was invvalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var Blob: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt;
  nofullcheck: boolean=true): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt;
  nofullcheck: boolean=true): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(const s: RawByteString): boolean; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(sp: PAnsiChar; len: PtrInt): boolean; overload;

/// retrieve the expected encoded length after Base64 process
function BinToBase64Length(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;

/// direct low-level decoding of a Base64 encoded buffer
// - here len is the number of 4 chars chunks in sp input
// - deprecated low-level function: use Base64ToBin/Base64ToBinSafe instead
function Base64Decode(sp,rp: PAnsiChar; len: PtrInt): boolean;

/// fast conversion from binary data into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(const s: RawByteString): RawUTF8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUTF8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded shortstring
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - returns '' if BinBytes void or too big for the resulting shortstring
function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): shortstring;

/// conversion from any Base64 encoded value into URI-compatible encoded text
// - warning: will modify the supplied base64 string in-place
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
procedure Base64ToURI(var base64: RawUTF8);

/// low-level conversion from a binary buffer into Base64-like URI-compatible encoded text
// - you should rather use the overloaded BinToBase64uri() functions
procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);

/// retrieve the expected encoded length after Base64-URI process
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uriLength(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64-URI encoded buffer
// - here len is the number of bytes in sp
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBinLength(len: PtrInt): PtrInt;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
procedure Base64uriToBin(sp: PAnsiChar; len: PtrInt; var result: RawByteString); overload;

/// fast conversion from Base64-URI encoded text into binary data
// - caller should always execute temp.Done when finished with the data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt; var temp: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct low-level decoding of a Base64-URI encoded buffer
// - the buffer is expected to be at least Base64uriToBinLength() bytes long
// - returns true if the supplied sp[] buffer has been successfully decoded
// into rp[] - will break at any invalid character, so is always safe to use
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - you should better not use this, but Base64uriToBin() overloaded functions
function Base64uriDecode(sp,rp: PAnsiChar; len: PtrInt): boolean;



{ ************ INI Files and In-memory Access }

/// find a Name= Value in a [Section] of a INI RawUTF8 Content
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;

/// find a Name= Value in a [Section] of a INI WinAnsi Content
// - same as FindIniEntry(), but the value is converted from WinAnsi into UTF-8
function FindWinAnsiIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;

/// find a Name= numeric Value in a [Section] of a INI RawUTF8 Content and
// return it as an integer, or 0 if not found
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntryInteger(const Content, Section, Name: RawUTF8): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// find a Name= Value in a [Section] of a .INI file
// - if Section equals '', find the Name= value before any [Section]
// - use internaly fast FindIniEntry() function above
function FindIniEntryFile(const FileName: TFileName; const Section, Name: RawUTF8): RawUTF8;

/// update a Name= Value in a [Section] of a INI RawUTF8 Content
// - this function scans and update the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', update the Name= value before any [Section]
procedure UpdateIniEntry(var Content: RawUTF8; const Section,Name,Value: RawUTF8);

/// update a Name= Value in a [Section] of a .INI file
// - if Section equals '', update the Name= value before any [Section]
// - use internaly fast UpdateIniEntry() function above
procedure UpdateIniEntryFile(const FileName: TFileName; const Section,Name,Value: RawUTF8);

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
function FindSectionFirstLine(var source: PUTF8Char; search: PAnsiChar): boolean;

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
// - this version expects source^ to point to an Unicode char array
function FindSectionFirstLineW(var source: PWideChar; search: PUTF8Char): boolean;

/// retrieve the whole content of a section as a string
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function GetSectionContent(SectionFirstLine: PUTF8Char): RawUTF8; overload;

/// retrieve the whole content of a section as a string
// - use SectionFirstLine() then previous GetSectionContent()
function GetSectionContent(const Content, SectionName: RawUTF8): RawUTF8; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
function DeleteSection(var Content: RawUTF8; const SectionName: RawUTF8;
  EraseSectionHeader: boolean=true): boolean; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function DeleteSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  EraseSectionHeader: boolean=true): boolean; overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
procedure ReplaceSection(var Content: RawUTF8; const SectionName,
  NewSectionContent: RawUTF8); overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
procedure ReplaceSection(SectionFirstLine: PUTF8Char;
  var Content: RawUTF8; const NewSectionContent: RawUTF8); overload;

/// return TRUE if Value of UpperName does exist in P, till end of current section
// - expect UpperName as 'NAME='
function ExistsIniName(P: PUTF8Char; UpperName: PAnsiChar): boolean;

/// find the Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
function FindIniNameValue(P: PUTF8Char; UpperName: PAnsiChar): RawUTF8;

/// return TRUE if one of the Value of UpperName exists in P, till end of
// current section
// - expect UpperName e.g. as 'CONTENT-TYPE: '
// - expect UpperValues to be any upper value with left side matching, e.g. as
// used by IsHTMLContentTypeTextual() function:
// ! result := ExistsIniNameValue(htmlHeaders,HEADER_CONTENT_TYPE_UPPER,
// !  ['TEXT/','APPLICATION/JSON','APPLICATION/XML']);
// - warning: this function calls IdemPCharArray(), so expects UpperValues[]
/// items to have AT LEAST TWO CHARS (it will use fast initial 2 bytes compare)
function ExistsIniNameValue(P: PUTF8Char; const UpperName: RawUTF8;
  const UpperValues: array of PAnsiChar): boolean;

/// find the integer Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
// - return 0 if no NAME= entry was found
function FindIniNameValueInteger(P: PUTF8Char; UpperName: PAnsiChar): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// replace a value from a given set of name=value lines
// - expect UpperName as 'UPPERNAME=', otherwise returns false
// - if no UPPERNAME= entry was found, then Name+NewValue is added to Content
// - a typical use may be:
// ! UpdateIniNameValue(headers,HEADER_CONTENT_TYPE,HEADER_CONTENT_TYPE_UPPER,contenttype);
function UpdateIniNameValue(var Content: RawUTF8; const Name, UpperName, NewValue: RawUTF8): boolean;



implementation

uses
  mormot.core.text,
  mormot.core.os;


{$ifdef CPUARM} // circumvent FPC issue on ARM
function ToByte(value: cardinal): cardinal; inline;
begin
  result := value and $ff;
end;
{$else}
type ToByte = byte;
{$endif}

{ ************ Variable Length Integer Encoding / Decoding }

function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
begin // 0=0,1=1,2=-1,3=2,4=-2...
  if Value < 0 then
    // -1->2, -2->4..
    Value := (-Value) shl 1
  else if Value > 0 then
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
    // 0->0
  result := ToVarUInt32(Value, Dest);
end;

function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;
label
  _1, _2, _3; // ugly but fast
begin
  if Value > $7f then
  begin
    if Value < $80 shl 7 then
      goto _1
    else if Value < $80 shl 14 then
      goto _2
    else if Value < $80 shl 21 then
      goto _3;
    Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_3: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_2: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_1: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
  end;
  Dest^ := Value;
  inc(Dest);
  result := Dest;
end;

function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := 1
  else if Value < $80 shl 7 then
    result := 2
  else if Value < $80 shl 14 then
    result := 3
  else if Value < $80 shl 21 then
    result := 4
  else
    result := 5;
end;

function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := Value + 1
  else if Value < $80 shl 7 then
    result := Value + 2
  else if Value < $80 shl 14 then
    result := Value + 3
  else if Value < $80 shl 21 then
    result := Value + 4
  else
    result := Value + 5;
end;

function FromVarUInt32(var Source: PByte): cardinal;
begin
  result := Source^;
  inc(Source);
  if result > $7f then
    result := (result and $7F) or FromVarUInt32Up128(Source);
end;

function FromVarUInt32Big(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin // Values between 128 and 16256
    c := p^;
    c := c shl 7;
    result := result and $7F or c;
    inc(p);
    if c > $7f shl 7 then
    begin // Values between 16257 and 2080768
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or c;
      if c > $7f shl 14 then
      begin // Values between 2080769 and 266338304
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32Up128(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin // Values above 128
  p := Source;
  result := p^ shl 7;
  inc(p);
  if result > $7f shl 7 then
  begin // Values above 16257
    c := p^;
    c := c shl 14;
    inc(p);
    result := result and $3FFF or c;
    if c > $7f shl 14 then
    begin
      c := p^;
      c := c shl 21;
      inc(p);
      result := result and $1FFFFF or c;
      if c > $7f shl 21 then
      begin
        c := p^;
        c := c shl 28;
        inc(p);
        result := result and $FFFFFFF or c;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32(var Source: PByte; SourceMax: PByte; out Value: cardinal): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt32(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;
var
  c: cardinal;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  Value := c;
  if c > $7f then
  begin // Values between 128 and 16256
    if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
      exit;
    c := Source^;
    c := c shl 7;
    Value := Value and $7F or c;
    inc(Source);
    if c > $7f shl 7 then
    begin // Values between 16257 and 2080768
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      c := c shl 14;
      inc(Source);
      Value := Value and $3FFF or c;
      if c > $7f shl 14 then
      begin // Values between 2080769 and 266338304
        if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
          exit;
        c := Source^;
        c := c shl 21;
        inc(Source);
        Value := Value and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
            exit;
          c := Source^;
          c := c shl 28;
          inc(Source);
          Value := Value and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  result := Source; // safely decoded
end;

function FromVarInt32(var Source: PByte): integer;
var
  c: cardinal;
  p: PByte;
begin // fast stand-alone function with no FromVarUInt32 call
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin
    c := p^;
    c := c shl 7;
    result := result and $7F or integer(c);
    inc(p);
    if c > $7f shl 7 then
    begin
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or integer(c);
      if c > $7f shl 14 then
      begin
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or integer(c);
        if c > $7f shl 21 then
        begin
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or integer(c);
        end;
      end;
    end;
  end;
  Source := p;
  // 0=0,1=1,2=-1,3=2,4=-2...
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function FromVarUInt32High(var Source: PByte): cardinal;
var
  c: cardinal;
begin
  result := Source^;
  inc(Source);
  c := Source^ shl 7;
  inc(Source);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    exit;
  c := Source^ shl 14;
  inc(Source);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    exit;
  c := Source^ shl 21;
  inc(Source);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    exit;
  c := Source^ shl 28;
  inc(Source);
  result := result and $FFFFFFF or c;
end;

function ToVarInt64(Value: Int64; Dest: PByte): PByte;
begin // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU32}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    result := ToVarUInt64((-Value) shl 1, Dest)
  else
     // 1->1, 2->3..
    result := ToVarUInt64((Value shl 1) - 1, Dest);
{$else}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1
  else
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
  result := ToVarUInt64(Value, Dest);
{$endif CPU32}
end;

function ToVarUInt64(Value: QWord; Dest: PByte): PByte;
var
  c: cardinal;
label
  _1, _2, _4; // ugly but fast
begin
  repeat
    c := Value;
    {$ifdef CPU32}
    if PInt64Rec(@Value)^.Hi = 0 then
    {$else}
      if Value shr 32 = 0 then
    {$endif CPU32}
      begin
        if c > $7f then
        begin
          if c < $80 shl 7 then
            goto _1
          else if c < $80 shl 14 then
            goto _2
          else if c >= $80 shl 21 then
            goto _4;
          Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_2:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_1:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
        end;
        Dest^ := c;
        inc(Dest);
        result := Dest;
        exit;
      end;
_4: PCardinal(Dest)^ := (c and $7F) or (((c shr 7) and $7F) shl 8) or
      (((c shr 14) and $7F) shl 16) or (((c shr 21) and $7F) shl 24) or $80808080;
    inc(Dest, 4);
    Value := Value shr 28;
  until false;
end;

function FromVarUInt64(var Source: PByte): QWord;
var
  c, n: PtrUInt;
  p: PByte;
begin
  p := Source;
  {$ifdef CPU64}
  result := p^;
  if result > $7f then
  begin
    result := result and $7F;
  {$else}
  if p^ > $7f then
  begin
    result := PtrUInt(p^) and $7F;
  {$endif}
    n := 0;
    inc(p);
    repeat
      c := p^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (QWord(c and $7f) shl n);
      inc(p);
    until false;
    result := result or (QWord(c) shl n);
  end
  {$ifndef CPU64}
  else
    result := p^
  {$endif};
  inc(p);
  Source := p;
end;

function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;
var
  c, n: PtrUInt;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  if c > $7f then
  begin
    Value := c and $7F;
    n := 7;
    repeat
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      inc(Source);
      if c <= $7f then
        break;
      c := c and $7f;
      Value := Value or (QWord(c) shl n);
      inc(n, 7);
    until false;
    Value := Value or (QWord(c) shl n);
  end
  else
    Value := c;
  result := Source; // safely decoded
end;

function FromVarUInt64(var Source: PByte; SourceMax: PByte; out Value: QWord): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt64(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt64Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarInt64(var Source: PByte): Int64;
var
  c, n: PtrUInt;
begin // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU64}
  result := Source^;
  if result > $7f then
  begin
    result := result and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
  end;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
{$else}
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    if PCardinal(@result)^ and 1 <> 0 then
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -(result shr 1);
  end
  else
  begin
    if c = 0 then
      result := 0
    else if c and 1 = 0 then
      // 0->0, 2->-1, 4->-2..
      result := -Int64(c shr 1)
    else
      // 1->1, 3->2..
      result := (c shr 1) + 1;
  end;
{$endif CPU64}
  inc(Source);
end;

function FromVarInt64Value(Source: PByte): Int64;
var
  c, n: PtrUInt;
begin // 0=0,1=1,2=-1,3=2,4=-2...
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    {$ifdef CPU64}
    if result and 1 <> 0 then
    {$else}
    if PCardinal(@result)^ and 1 <> 0 then
    {$endif}
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -Int64(result shr 1);
  end
  else if c = 0 then
    result := 0
  else if c and 1 = 0 then
    // 0->0, 2->-1, 4->-2..
    result := -Int64(c shr 1)
  else
    // 1->1, 3->2..
    result := (c shr 1) + 1;
end;

function GotoNextVarInt(Source: PByte): pointer;
begin
  if Source <> nil then
  begin
    if Source^ > $7f then
      repeat
        inc(Source)
      until Source^ <= $7f;
    inc(Source);
  end;
  result := Source;
end;



{ ************ Base64 and Base64URI Encoding / Decoding }

type
  TBase64Enc = array[0..63] of AnsiChar;
  PBase64Enc = ^TBase64Enc;
  TBase64Dec = array[AnsiChar] of shortint;
  PBase64Dec = ^TBase64Dec;

const
  b64enc:    TBase64Enc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  b64URIenc: TBase64Enc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';

var
  /// a conversion table from Base64 text into binary data
  // - used by Base64ToBin/IsBase64 functions
  // - contains -1 for invalid char, -2 for '=', 0..63 for b64enc[] chars
  ConvertBase64ToBin, ConvertBase64URIToBin: TBase64Dec;

  
{ --------- Base64 encoding/decoding }

function Base64AnyDecode(const decode: TBase64Dec; sp, rp: PAnsiChar; len: PtrInt): boolean;
var
  c, ch: PtrInt;
begin
  result := false;
  while len >= 4 do
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[2]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[3]];
    if ch < 0 then
      exit;
    c := c or ch;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    dec(len, 4);
    inc(rp, 3);
    inc(sp, 4);
  end;
  if len >= 2 then
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    if len = 2 then
      rp[0] := AnsiChar((c or ch) shr 4)
    else
    begin
      c := (c or ch) shl 6;
      ch := decode[sp[2]];
      if ch < 0 then
        exit;
      c := (c or ch) shr 2;
      rp[1] := AnsiChar(c);
      rp[0] := AnsiChar(c shr 8);
    end;
  end;
  result := true;
end;

function Base64Decode(sp, rp: PAnsiChar; len: PtrInt): boolean; {$ifdef FPC} inline;{$endif}
var
  tab: PBase64Dec; // use local register
begin
  tab := @ConvertBase64ToBin;
  len := len shl 2; // len was the number of 4 chars chunks in sp
  if (len > 0) and (tab[sp[len - 2]] >= 0) then
    if tab[sp[len - 1]] >= 0 then

    else
      dec(len)
  else
    dec(len, 2); // Base64AnyDecode() algorithm ignores the trailing '='
  result := Base64AnyDecode(tab^, sp, rp, len);
end;

{$ifdef ASMX86}

function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
asm // eax=rp edx=sp ecx=len - pipeline optimized version by AB
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebx, edx
        mov     esi, eax
        mov     eax, ecx
        mov     edx, 1431655766 // faster eax=len div 3 using reciprocal
        sar     ecx, 31
        imul    edx
        mov     eax, edx
        sub     eax, ecx
        mov     edi, offset b64enc
        mov     ebp, eax
        push    eax
        jz      @z
        // edi=b64enc[] ebx=sp esi=rp ebp=len div 3
        xor     eax, eax
        @1:     // read 3 bytes from sp
        movzx   edx, byte ptr[ebx]
        shl     edx, 16
        mov     al, [ebx + 2]
        mov     ah, [ebx + 1]
        add     ebx, 3
        or      eax, edx
        // encode as Base64
        mov     ecx, eax
        mov     edx, eax
        shr     ecx, 6
        and     edx, $3f
        and     ecx, $3f
        mov     dh, [edi + edx]
        mov     dl, [edi + ecx]
        mov     ecx, eax
        shr     eax, 12
        shr     ecx, 18
        shl     edx, 16
        and     ecx, $3f
        and     eax, $3f
        mov     cl, [edi + ecx]
        mov     ch, [edi + eax]
        or      ecx, edx
        // write the 4 encoded bytes into rp
        mov     [esi], ecx
        add     esi, 4
        dec     ebp
        jnz     @1
@z:     pop     eax // result := len div 3
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

{$else}

function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
var
  i: integer;
  c: cardinal;
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  result := len div 3;
  for i := 1 to result do
  begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := enc[(c shr 18) and $3f];
    rp[1] := enc[(c shr 12) and $3f];
    rp[2] := enc[(c shr 6) and $3f];
    rp[3] := enc[c and $3f];
    inc(rp, 4);
    inc(sp, 3);
  end;
end;

{$endif ASMX86}

procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef HASINLINE} inline; {$endif}
var
  c: cardinal;
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
        PWord(rp + 2)^ := ord('=') + ord('=') shl 8;
      end;
    2:
      begin
        c := ord(sp[0]) shl 10 + ord(sp[1]) shl 2;
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
        rp[3] := '=';
      end;
  end;
end;

procedure Base64Encode(rp, sp: PAnsiChar; len: cardinal);
var
  main: cardinal;
begin
  main := Base64EncodeMain(rp, sp, len);
  Base64EncodeTrailing(rp + main * 4, sp + main * 3, len - main * 3);
end;

function BinToBase64Length(len: PtrUInt): PtrUInt;
begin
  result := ((len + 2) div 3) * 4;
end;

function BinToBase64(const s: RawByteString): RawUTF8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, nil, BinToBase64Length(len));
  Base64Encode(pointer(result), pointer(s), len);
end;

function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): shortstring;
var
  destlen: integer;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  destlen := BinToBase64Length(BinBytes);
  if destlen > 255 then
    exit; // avoid buffer overflow
  result[0] := AnsiChar(destlen);
  Base64Encode(@result[1], Bin, BinBytes);
end;

function BinToBase64Short(const s: RawByteString): shortstring;
begin
  result := BinToBase64Short(pointer(s), length(s));
end;

function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUTF8;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  FastSetString(result, nil, BinToBase64Length(BinBytes));
  Base64Encode(pointer(result), Bin, BinBytes);
end;

function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUTF8;
var
  lendata, lenprefix, lensuffix, len: integer;
  res: PByteArray absolute result;
begin
  result := '';
  lendata := length(data);
  lenprefix := length(Prefix);
  lensuffix := length(Suffix);
  if lendata + lenprefix + lensuffix = 0 then
    exit;
  len := ((lendata + 2) div 3) * 4 + lenprefix + lensuffix;
  if WithMagic then
    inc(len, 3);
  FastSetString(result, nil, len);
  if lenprefix > 0 then
    MoveSmall(pointer(Prefix), res, lenprefix);
  if WithMagic then
  begin
    PInteger(@res[lenprefix])^ := JSON_BASE64_MAGIC;
    inc(lenprefix, 3);
  end;
  Base64Encode(@res[lenprefix], pointer(data), lendata);
  if lensuffix > 0 then
    MoveSmall(pointer(Suffix), @res[len - lensuffix], lensuffix);
end;

function BinToBase64WithMagic(const data: RawByteString): RawUTF8;
var
  len: integer;
begin
  result := '';
  len := length(data);
  if len = 0 then
    exit;
  FastSetString(result, nil, ((len + 2) div 3) * 4 + 3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC;
  Base64Encode(PAnsiChar(pointer(result)) + 3, pointer(data), len);
end;

function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUTF8;
begin
  result := '';
  if DataLen <= 0 then
    exit;
  FastSetString(result, nil, ((DataLen + 2) div 3) * 4 + 3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC;
  Base64Encode(PAnsiChar(pointer(result)) + 3, Data, DataLen);
end;

function IsBase64Internal(sp: PAnsiChar; len: PtrInt; dec: PBase64Dec): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (len = 0) or (len and 3 <> 0) then
    exit;
  for i := 0 to len - 5 do
    if dec[sp[i]] < 0 then
      exit;
  inc(sp, len - 4);
  if (dec[sp[0]] = -1) or (dec[sp[1]] = -1) or (dec[sp[2]] = -1) or (dec[sp[3]] = -1) then
    exit;
  result := true; // layout seems correct
end;

function IsBase64(sp: PAnsiChar; len: PtrInt): boolean;
begin
  result := IsBase64Internal(sp, len, @ConvertBase64ToBin);
end;

function IsBase64(const s: RawByteString): boolean;
begin
  result := IsBase64Internal(pointer(s), length(s), @ConvertBase64ToBin);
end;

function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;
var
  dec: PBase64Dec;
begin
  dec := @ConvertBase64ToBin;
  if IsBase64Internal(sp, len, dec) then
  begin
    if dec[sp[len - 2]] >= 0 then
      if dec[sp[len - 1]] >= 0 then
        result := 0
      else
        result := 1
    else
      result := 2;
    result := (len shr 2) * 3 - result;
  end
  else
    result := 0;
end;

function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;
var
  dec: PBase64Dec;
begin
  result := 0;
  if (len = 0) or (len and 3 <> 0) then
    exit;
  dec := @ConvertBase64ToBin;
  if dec[sp[len - 2]] >= 0 then
    if dec[sp[len - 1]] >= 0 then
      result := 0
    else
      result := 1
  else
    result := 2;
  result := (len shr 2) * 3 - result;
end;

function Base64ToBin(const s: RawByteString): RawByteString;
begin
  Base64ToBinSafe(pointer(s), length(s), result);
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result);
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
begin
  result := Base64ToBinSafe(sp, len, data);
end;

function Base64ToBinSafe(const s: RawByteString): RawByteString;
begin
  Base64ToBinSafe(pointer(s), length(s), result);
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result);
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
var
  resultLen: PtrInt;
begin
  resultLen := Base64ToBinLength(sp, len);
  if resultLen <> 0 then
  begin
    SetString(data, nil, resultLen);
    if ConvertBase64ToBin[sp[len - 2]] >= 0 then
      if ConvertBase64ToBin[sp[len - 1]] >= 0 then
        // keep len as it is
      else
        dec(len)
    else
      dec(len, 2); // adjust for Base64AnyDecode() algorithm
    result := Base64AnyDecode(ConvertBase64ToBin, sp, pointer(data), len);
    if not result then
      data := '';
  end
  else
  begin
    result := false;
    data := '';
  end;
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var blob: TSynTempBuffer): boolean;
begin
  blob.Init(Base64ToBinLength(sp, len));
  result := (blob.len > 0) and Base64Decode(sp, blob.buf, len shr 2);
end;

function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt; nofullcheck: boolean): boolean;
begin // nofullcheck is just ignored and deprecated
  result := (bin <> nil) and (Base64ToBinLength(base64, base64len) = binlen) and Base64Decode(base64, bin, base64len shr 2);
end;

function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt; nofullcheck: boolean): boolean;
begin
  result := Base64ToBin(pointer(base64), bin, length(base64), binlen, nofullcheck);
end;

{ --------- Base64 URI encoding/decoding }

{$ifdef ASMX86}

function Base64uriEncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
asm // eax=rp edx=sp ecx=len - pipeline optimized version by AB
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebx, edx
        mov     esi, eax
        mov     eax, ecx
        mov     edx, 1431655766 // faster eax=len div 3 using reciprocal
        sar     ecx, 31
        imul    edx
        mov     eax, edx
        sub     eax, ecx
        mov     edi, offset b64urienc
        mov     ebp, eax
        push    eax
        jz      @z
        // edi=b64urienc[] ebx=sp esi=rp ebp=len div 3
        xor     eax, eax
@1:    // read 3 bytes from sp
        movzx   edx, byte ptr[ebx]
        shl     edx, 16
        mov     al, [ebx + 2]
        mov     ah, [ebx + 1]
        add     ebx, 3
        or      eax, edx
        // encode as Base64uri
        mov     ecx, eax
        mov     edx, eax
        shr     ecx, 6
        and     edx, $3f
        and     ecx, $3f
        mov     dh, [edi + edx]
        mov     dl, [edi + ecx]
        mov     ecx, eax
        shr     eax, 12
        shr     ecx, 18
        shl     edx, 16
        and     ecx, $3f
        and     eax, $3f
        mov     cl, [edi + ecx]
        mov     ch, [edi + eax]
        or      ecx, edx
        // write the 4 encoded bytes into rp
        mov     [esi], ecx
        add     esi, 4
        dec     ebp
        jnz     @1
@z:     pop     eax // result := len div 3
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

procedure Base64uriEncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef HASINLINE} inline;{$endif}
var
  c: cardinal;
begin
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := b64urienc[(c shr 6) and $3f];
        rp[1] := b64urienc[c and $3f];
      end;
    2:
      begin
        c := ord(sp[0]) shl 10 + ord(sp[1]) shl 2;
        rp[0] := b64urienc[(c shr 12) and $3f];
        rp[1] := b64urienc[(c shr 6) and $3f];
        rp[2] := b64urienc[c and $3f];
      end;
  end;
end;

procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);
var
  main: cardinal;
begin
  main := Base64uriEncodeMain(rp, sp, len);
  Base64uriEncodeTrailing(rp + main * 4, sp + main * 3, len - main * 3);
end;

{$else}

procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);
var
  i, main, c: cardinal;
  enc: PBase64Enc; // slightly faster
begin
  enc := @b64URIenc;
  main := len div 3;
  for i := 1 to main do
  begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := enc[(c shr 18) and $3f];
    rp[1] := enc[(c shr 12) and $3f];
    rp[2] := enc[(c shr 6) and $3f];
    rp[3] := enc[c and $3f];
    inc(rp, 4);
    inc(sp, 3);
  end;
  case len - main * 3 of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
      end;
    2:
      begin
        c := ord(sp[0]) shl 10 + ord(sp[1]) shl 2;
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
      end;
  end;
end;

{$endif ASMX86}

function BinToBase64uriLength(len: PtrUInt): PtrUInt;
begin
  result := (len div 3) * 4;
  case len - (result shr 2) * 3 of // fast len mod 3
    1:
      inc(result, 2);
    2:
      inc(result, 3);
  end;
end;

function BinToBase64uri(const s: RawByteString): RawUTF8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, nil, BinToBase64uriLength(len));
  Base64uriEncode(pointer(result), pointer(s), len);
end;

function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUTF8;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  FastSetString(result, nil, BinToBase64uriLength(BinBytes));
  Base64uriEncode(pointer(result), Bin, BinBytes);
end;

function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): shortstring;
var
  len: integer;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  len := BinToBase64uriLength(BinBytes);
  if len > 255 then
    exit;
  byte(result[0]) := len;
  Base64uriEncode(@result[1], Bin, BinBytes);
end;

function Base64uriToBinLength(len: PtrInt): PtrInt;
begin
  if len = 0 then
    result := 0
  else
  begin
    result := (len shr 2) * 3;
    case len and 3 of
      1:
        result := 0;
      2:
        inc(result, 1);
      3:
        inc(result, 2);
    end;
  end;
end;

function Base64uriDecode(sp, rp: PAnsiChar; len: PtrInt): boolean;
begin
  result := Base64AnyDecode(ConvertBase64URIToBin, sp, rp, len);
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64uriToBin(sp, len, result);
end;

function Base64uriToBin(const s: RawByteString): RawByteString;
begin
  Base64uriToBin(pointer(s), length(s), result);
end;

procedure Base64uriToBin(sp: PAnsiChar; len: PtrInt; var result: RawByteString);
var
  resultLen: PtrInt;
begin
  resultLen := Base64uriToBinLength(len);
  if resultLen <> 0 then
  begin
    SetString(result, nil, resultLen);
    if Base64AnyDecode(ConvertBase64URIToBin, sp, pointer(result), len) then
      exit;
  end;
  result := '';
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt; var temp: TSynTempBuffer): boolean;
begin
  temp.Init(Base64uriToBinLength(len));
  result := (temp.len > 0) and
    Base64AnyDecode(ConvertBase64URIToBin, sp, temp.buf, len);
end;

function Base64uriToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt): boolean;
begin
  result := Base64uriToBin(pointer(base64), bin, length(base64), binlen);
end;

function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean;
var
  resultLen: PtrInt;
begin
  resultLen := Base64uriToBinLength(base64len);
  result := (resultLen = binlen) and Base64AnyDecode(ConvertBase64URIToBin, base64, bin, base64len);
end;

procedure Base64ToURI(var base64: RawUTF8);
var
  P: PUTF8Char;
begin
  P := UniqueRawUTF8(base64);
  if P <> nil then
    repeat
      case P^ of
        #0:
          break;
        '+':
          P^ := '-';
        '/':
          P^ := '_';
        '=':
          begin // trim unsignificant trailing '=' characters
            SetLength(base64, P - pointer(base64));
            break;
          end;
      end;
      inc(P);
    until false;
end;

procedure Base64MagicDecode(var ParamValue: RawUTF8);
var
  tmp: RawUTF8;
begin // '\uFFF0base64encodedbinary' decode into binary (input shall have been checked)
  tmp := ParamValue;
  if not Base64ToBinSafe(PAnsiChar(pointer(tmp)) + 3, length(tmp) - 3,
          RawByteString(ParamValue)) then
    ParamValue := '';
end;

function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: RawByteString): boolean;
var
  ValueLen: integer;
begin // '\uFFF0base64encodedbinary' checked and decode into binary
  if (Value = nil) or (Value[0] = #0) or (Value[1] = #0) or (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC) then
    result := false
  else
  begin
    ValueLen := StrLen(Value) - 3;
    if ValueLen > 0 then
      result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: TSynTempBuffer): boolean;
var
  ValueLen: integer;
begin // '\uFFF0base64encodedbinary' checked and decode into binary
  if (Value = nil) or (Value[0] = #0) or (Value[1] = #0) or (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC) then
    result := false
  else
  begin
    ValueLen := StrLen(Value) - 3;
    if ValueLen > 0 then
      result := Base64ToBin(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicCheckAndDecode(Value: PUTF8Char; ValueLen: integer; var Blob: RawByteString): boolean;
begin // '\uFFF0base64encodedbinary' checked and decode into binary
  if (ValueLen < 4) or (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC) then
    result := false
  else
    result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen - 3, Blob);
end;



{ ************ INI Files and In-memory Access }

function FindSectionFirstLine(var source: PUTF8Char; search: PAnsiChar): boolean;
begin
  result := false;
  if source = nil then
    exit;
  repeat
    if source^ = '[' then
    begin
      inc(source);
      result := IdemPChar(source, search);
    end;
    while source^ in ANSICHARNOT01310 do
      inc(source);
    while source^ in [#10, #13] do
      inc(source);
    if result then
      exit; // found
  until source^ = #0;
  source := nil;
end;

function FindSectionFirstLineW(var source: PWideChar; search: PUTF8Char): boolean;
begin
  result := false;
  if source = nil then
    exit;
  repeat
    if source^ = '[' then
    begin
      inc(source);
      result := IdemPCharW(source, search);
    end;
    while not (cardinal(source^) in [0, 10, 13]) do
      inc(source);
    while cardinal(source^) in [10, 13] do
      inc(source);
    if result then
      exit; // found
  until source^ = #0;
  source := nil;
end;

function FindIniNameValue(P: PUTF8Char; UpperName: PAnsiChar): RawUTF8;
var
  u, PBeg: PUTF8Char;
  by4: cardinal;
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7;
  {$else}
  table: PNormTable;
  {$endif}
begin // expect UpperName as 'NAME='
  if (P <> nil) and (P^ <> '[') and (UpperName <> nil) then
  begin
    {$ifndef CPUX86NOTPIC} table := @NormToUpperAnsi7; {$endif}
    PBeg := nil;
    u := P;
    repeat
      while u^ = ' ' do
        inc(u); // trim left ' '
      if u^ = #0 then
        break;
      if table[u^] = UpperName[0] then
        PBeg := u;
      repeat
        by4 := PCardinal(u)^;
        if ToByte(by4) > 13 then
          if ToByte(by4 shr 8) > 13 then
            if ToByte(by4 shr 16) > 13 then
              if ToByte(by4 shr 24) > 13 then
              begin
                inc(u, 4);
                continue;
              end
              else
                inc(u, 3)
            else
              inc(u, 2)
          else
            inc(u);
        if u^ in [#0, #10, #13] then
          break
        else
          inc(u);
      until false;
      if PBeg <> nil then
      begin
        inc(PBeg);
        P := u;
        u := pointer(UpperName + 1);
        repeat
          if u^ <> #0 then
            if table[PBeg^] <> u^ then
              break
            else
            begin
              inc(u);
              inc(PBeg);
            end
          else
          begin
            FastSetString(result, PBeg, P - PBeg);
            exit;
          end;
        until false;
        PBeg := nil;
        u := P;
      end;
      if u^ = #13 then
        inc(u);
      if u^ = #10 then
        inc(u);
    until u^ in [#0, '['];
  end;
  result := '';
end;

function IdemPChar2(table: PNormTable; p: PUTF8Char; up: PAnsiChar): boolean;
  {$ifdef HASINLINE} inline;{$endif}
var
  u: AnsiChar;
begin // here p and up are expected to be <> nil
  result := false;
  dec(PtrUInt(p), PtrUInt(up));
  repeat
    u := up^;
    if u = #0 then
      break;
    if table^[up[PtrUInt(p)]] <> u then
      exit;
    inc(up);
  until false;
  result := true;
end;

function ExistsIniName(P: PUTF8Char; UpperName: PAnsiChar): boolean;
var
  table: PNormTable;
begin
  result := false;
  table := @NormToUpperAnsi7;
  if (P <> nil) and (P^ <> '[') then
    repeat
      if P^ = ' ' then
      begin
        repeat
          inc(P)
        until P^ <> ' '; // trim left ' '
        if P^ = #0 then
          break;
      end;
      if IdemPChar2(table, P, UpperName) then
      begin
        result := true;
        exit;
      end;
      repeat
        if P[0] > #13 then
          if P[1] > #13 then
            if P[2] > #13 then
              if P[3] > #13 then
              begin
                inc(P, 4);
                continue;
              end
              else
                inc(P, 3)
            else
              inc(P, 2)
          else
            inc(P);
        case P^ of
          #0:
            exit;
          #10:
            begin
              inc(P);
              break;
            end;
          #13:
            begin
              if P[1] = #10 then
                inc(P, 2)
              else
                inc(P);
              break;
            end;
        else
          inc(P);
        end;
      until false;
    until P^ = '[';
end;

function ExistsIniNameValue(P: PUTF8Char; const UpperName: RawUTF8;
  const UpperValues: array of PAnsiChar): boolean;
var
  PBeg: PUTF8Char;
begin
  result := true;
  if high(UpperValues) >= 0 then
    while (P <> nil) and (P^ <> '[') do
    begin
      if P^ = ' ' then
        repeat
          inc(P)
        until P^ <> ' '; // trim left ' '
      PBeg := P;
      if IdemPChar(PBeg, pointer(UpperName)) then
      begin
        inc(PBeg, length(UpperName));
        if IdemPCharArray(PBeg, UpperValues) >= 0 then
          exit; // found one value
        break;
      end;
      P := GotoNextLine(P);
    end;
  result := false;
end;

function GetSectionContent(SectionFirstLine: PUTF8Char): RawUTF8;
var
  PBeg: PUTF8Char;
begin
  PBeg := SectionFirstLine;
  while (SectionFirstLine <> nil) and (SectionFirstLine^ <> '[') do
    SectionFirstLine := GotoNextLine(SectionFirstLine);
  if SectionFirstLine = nil then
    result := PBeg
  else
    FastSetString(result, PBeg, SectionFirstLine - PBeg);
end;

function GetSectionContent(const Content, SectionName: RawUTF8): RawUTF8;
var
  P: PUTF8Char;
  UpperSection: array[byte] of AnsiChar;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    result := GetSectionContent(P)
  else
    result := '';
end;

function DeleteSection(var Content: RawUTF8; const SectionName: RawUTF8;
  EraseSectionHeader: boolean): boolean;
var
  P: PUTF8Char;
  UpperSection: array[byte] of AnsiChar;
begin
  result := false; // no modification
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    result := DeleteSection(P, Content, EraseSectionHeader);
end;

function DeleteSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  EraseSectionHeader: boolean): boolean;
var
  PEnd: PUTF8Char;
  IndexBegin: PtrInt;
begin
  result := false;
  PEnd := SectionFirstLine;
  if EraseSectionHeader then // erase [Section] header line
    while (PtrUInt(SectionFirstLine) > PtrUInt(Content)) and (SectionFirstLine^ <> '[') do
      dec(SectionFirstLine);
  while (PEnd <> nil) and (PEnd^ <> '[') do
    PEnd := GotoNextLine(PEnd);
  IndexBegin := SectionFirstLine - pointer(Content);
  if IndexBegin = 0 then
    exit; // no modification
  if PEnd = nil then
    SetLength(Content, IndexBegin)
  else
    delete(Content, IndexBegin + 1, PEnd - SectionFirstLine);
  result := true; // Content was modified
end;

procedure ReplaceSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  const NewSectionContent: RawUTF8);
var
  PEnd: PUTF8Char;
  IndexBegin: PtrInt;
begin
  if SectionFirstLine = nil then
    exit;
  // delete existing [Section] content
  PEnd := SectionFirstLine;
  while (PEnd <> nil) and (PEnd^ <> '[') do
    PEnd := GotoNextLine(PEnd);
  IndexBegin := SectionFirstLine - pointer(Content);
  if PEnd = nil then
    SetLength(Content, IndexBegin)
  else
    delete(Content, IndexBegin + 1, PEnd - SectionFirstLine);
  // insert section content
  insert(NewSectionContent, Content, IndexBegin + 1);
end;

procedure ReplaceSection(var Content: RawUTF8; const SectionName, NewSectionContent: RawUTF8);
var
  UpperSection: array[byte] of AnsiChar;
  P: PUTF8Char;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    ReplaceSection(P, Content, NewSectionContent)
  else
    Content := Content + '[' + SectionName + ']'#13#10 + NewSectionContent;
end;

function FindIniNameValueInteger(P: PUTF8Char; UpperName: PAnsiChar): PtrInt;
begin
  result := GetInteger(pointer(FindIniNameValue(P, UpperName)));
end;

function FindIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;
var
  P: PUTF8Char;
  UpperSection, UpperName: array[byte] of AnsiChar;
begin
  result := '';
  P := pointer(Content);
  if P = nil then
    exit;
  // UpperName := UpperCase(Name)+'=';
  PWord(UpperCopy255(UpperName, Name))^ := ord('=');
  if Section = '' then
    // find the Name= entry before any [Section]
    result := FindIniNameValue(P, UpperName)
  else
  begin
    // find the Name= entry in the specified [Section]
    PWord(UpperCopy255(UpperSection, Section))^ := ord(']');
    if FindSectionFirstLine(P, UpperSection) then
      result := FindIniNameValue(P, UpperName);
  end;
end;

function FindWinAnsiIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;
begin
  result := WinAnsiToUtf8(WinAnsiString(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryInteger(const Content, Section, Name: RawUTF8): integer;
begin
  result := GetInteger(pointer(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryFile(const FileName: TFileName; const Section, Name: RawUTF8): RawUTF8;
var
  Content: RawUTF8;
begin
  Content := StringFromFile(FileName);
  if Content = '' then
    result := ''
  else
    result := FindIniEntry(Content, Section, Name);
end;

function UpdateIniNameValueInternal(var Content: RawUTF8; const NewValue, NewValueCRLF: RawUTF8;
  var P: PUTF8Char; UpperName: PAnsiChar; UpperNameLength: integer): boolean;
var
  PBeg: PUTF8Char;
  i: integer;
begin
  while (P <> nil) and (P^ <> '[') do
  begin
    while P^ = ' ' do
      inc(P);   // trim left ' '
    PBeg := P;
    P := GotoNextLine(P);
    if IdemPChar(PBeg, UpperName) then
    begin
     // update Name=Value entry
      result := true;
      inc(PBeg, UpperNameLength);
      i := (PBeg - pointer(Content)) + 1;
      if (i = length(NewValue)) and CompareMem(PBeg, pointer(NewValue), i) then
        exit; // new Value is identical to the old one -> no change
      if P = nil then // avoid last line (P-PBeg) calculation error
        SetLength(Content, i - 1)
      else
        delete(Content, i, P - PBeg); // delete old Value
      insert(NewValueCRLF, Content, i); // set new value
      exit;
    end;
  end;
  result := false;
end;

function UpdateIniNameValue(var Content: RawUTF8; const Name, UpperName, NewValue: RawUTF8): boolean;
var
  P: PUTF8Char;
begin
  if UpperName = '' then
    result := false
  else
  begin
    P := pointer(Content);
    result := UpdateIniNameValueInternal(Content, NewValue, NewValue + #13#10,
      P, pointer(UpperName), length(UpperName));
    if result or (Name = '') then
      exit;
    if Content <> '' then
      Content := Content + #13#10;
    Content := Content + Name + NewValue;
    result := true;
  end;
end;

procedure UpdateIniEntry(var Content: RawUTF8; const Section, Name, Value: RawUTF8);
const
  CRLF = #13#10;
var
  P: PUTF8Char;
  SectionFound: boolean;
  i, UpperNameLength: PtrInt;
  V: RawUTF8;
  UpperSection, UpperName: array[byte] of AnsiChar;
label
  Sec;
begin
  UpperNameLength := length(Name);
  PWord(UpperCopy255Buf(UpperName, pointer(Name), UpperNameLength))^ := ord('=');
  inc(UpperNameLength);
  V := Value + CRLF;
  P := pointer(Content);
  // 1. find Section, and try update within it
  if Section = '' then
    goto Sec; // find the Name= entry before any [Section]
  SectionFound := false;
  PWord(UpperCopy255(UpperSection, Section))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
  begin
Sec:SectionFound := true;
    if UpdateIniNameValueInternal(Content, Value, V, P, @UpperName, UpperNameLength) then
      exit;
    // we reached next [Section] without having found Name=
  end;
  // 2. section or Name= entry not found: add Name=Value
  V := Name + '=' + V;
  if not SectionFound then
    // create not existing [Section]
    V := '[' + Section + (']' + CRLF) + V;
  // insert Name=Value at P^ (end of file or end of [Section])
  if P = nil then
    // insert at end of file
    Content := Content + V
  else
  begin
    // insert at end of [Section]
    i := (P - pointer(Content)) + 1;
    insert(V, Content, i);
  end;
end;

procedure UpdateIniEntryFile(const FileName: TFileName; const Section, Name, Value: RawUTF8);
var
  Content: RawUTF8;
begin
  Content := StringFromFile(FileName);
  UpdateIniEntry(Content, Section, Name, Value);
  FileFromString(Content, FileName);
end;


initialization

end.

