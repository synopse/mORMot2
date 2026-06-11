/// Framework Core High-Level Content Processing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.fmt;

{
  *****************************************************************************

   Binary, JSON and Text Advanced Formatting Functions
    - HTML Text Conversions
    - Basic XML Conversions
    - YAML 1.2 core-schema to JSON or TDocVariant Support
    - Markup (e.g. Markdown or Emoji) Process
    - INI Files In-memory Access
    - TSynJsonFileSettings parent class
    - JSON and Text Preprocessor
    - Source Code Generation Functions

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  contnrs,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.json,
  mormot.core.variants;


{ ************* HTML Text Conversions }

/// append some UTF-8 chars, escaping all HTML special chars as expected
procedure AddHtmlEscapeUtf8(W: TTextWriter; const Text: RawUtf8;
  Fmt: TTextWriterHtmlFormat = hfAnyWhere);

/// low-level function removing all &lt; &gt; &amp; &quot; HTML entities
procedure AddHtmlUnescape(W: TTextWriter; p, amp: PUtf8Char; plen: PtrUInt);

/// low-level function removing all HTML <tag> and &entities;
procedure AddHtmlAsText(W: TTextWriter; p, tag: PUtf8Char; plen: PtrUInt);

/// check if some UTF-8 text would need HTML escaping
function NeedsHtmlEscape(text: PUtf8Char; fmt: TTextWriterHtmlFormat): boolean;

/// low-level conversion of an &amp; HTML entity into 32-bit Unicode Code Point
function EntityToUcs4(entity: PUtf8Char; len: byte): Ucs4CodePoint;

/// escape some UTF-8 text into HTML
// - just a wrapper around TTextWriter.AddHtmlEscape() process,
// replacing < > & " chars depending on the HTML layer
function HtmlEscape(const text: RawUtf8;
  fmt: TTextWriterHtmlFormat = hfAnyWhere): RawUtf8;

/// escape some RTL string text into UTF-8 HTML
// - just a wrapper around TTextWriter.AddHtmlEscapeString() process,
// replacing < > & " chars depending on the HTML layer
function HtmlEscapeString(const text: string;
  fmt: TTextWriterHtmlFormat = hfAnyWhere): RawUtf8; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// escape some UTF-8 text into a HTML ShortString
// - just a wrapper around TTextWriter.AddHtmlEscape() process,
// replacing < > & " chars depending on the HTML layer
function HtmlEscapeShort(const text: RawUtf8;
  fmt: TTextWriterHtmlFormat = hfAnyWhere): ShortString;

/// escape some RTL string text into UTF-8 HTML
// - just a wrapper around TTextWriter.AddHtmlEscapeString() process,
// replacing < > & " chars depending on the HTML layer
procedure HtmlEscapeString(const text: string; var result: RawUtf8;
  fmt: TTextWriterHtmlFormat); overload;

/// convert all &lt; &gt; &amp; &quot; HTML entities into their UTF-8 equivalency
function HtmlUnescape(const text: RawUtf8): RawUtf8;

/// minimal HTML-to-text conversion function
// - trim all HTML <tag></tag> and &entities; - with minimal CRLF formatting
function HtmlToText(const text: RawUtf8): RawUtf8;

/// quickly detect <p> <div> <br> <li> <h#> HTML tags
function HtmlTagNeedsCRLF(tag: PUtf8Char): boolean;


{ ************* Basic XML Conversions }

const
  /// standard header for an UTF-8 encoded XML file
  XMLUTF8_HEADER = '<?xml version="1.0" encoding="UTF-8"?>'#13#10;

  /// standard namespace for a generic XML File
  XMLUTF8_NAMESPACE = '<contents xmlns="http://www.w3.org/2001/XMLSchema-instance">';

/// check if some UTF-8 text would need XML escaping
function NeedsXmlEscape(text: PUtf8Char): boolean;

/// escape some UTF-8 text into XML
// - just a wrapper around the AddXmlEscape() function
function XmlEscape(const text: RawUtf8): RawUtf8;

/// convert a JSON array or document into a simple XML content
// - just a wrapper around AddJsonToXml() function, with an optional
// header before the XML converted data (e.g. XMLUTF8_HEADER), and an optional
// name space content node which will nest the generated XML data (e.g.
// '<contents xmlns="http://www.w3.org/2001/XMLSchema-instance">') - the
// corresponding ending token will be appended after (e.g. '</contents>')
// - WARNING: the JSON buffer is decoded in-place, so P^ WILL BE modified
procedure JsonBufferToXml(P: PUtf8Char; const Header, NameSpace: RawUtf8;
  out result: RawUtf8);

/// convert a JSON array or document into a simple XML content
// - just a wrapper around AddJsonToXml() function, making a private copy
// of the supplied JSON buffer using TSynTempBuffer (so that JSON content
// would stay untouched)
// - the optional header is added at the beginning of the resulting string
// - an optional name space content node could be added around the generated XML,
// e.g. '<content>'
function JsonToXml(const Json: RawUtf8; const Header: RawUtf8 = XMLUTF8_HEADER;
  const NameSpace: RawUtf8 = ''): RawUtf8;

/// append some chars, escaping all XML special chars as expected
// - i.e.   < > & " '  as   &lt; &gt; &amp; &quote; &apos;
// - and all control chars (i.e. #1..#31) as &#..;
// - see @http://www.w3.org/TR/xml/#syntax
procedure AddXmlEscape(W: TTextWriter; Text: PUtf8Char);

/// append a JSON value, array or document as simple XML content
// - as called by JsonBufferToXml() and JsonToXml() wrappers
// - this method is called recursively to handle all kind of JSON values
// - WARNING: the JSON buffer is decoded in-place, so will be changed
// - returns the end of the current JSON converted level, or nil if the
// supplied content was not correct JSON
function AddJsonToXml(W: TTextWriter; Json: PUtf8Char; ArrayName: PUtf8Char = nil;
  EndOfObject: PUtf8Char = nil): PUtf8Char;


{ ************* YAML 1.2 core-schema to JSON or TDocVariant Support }

{ main high-level YAML conversion functions }

type
  /// exception raised by YAML parser on unsupported or invalid input
  EYamlException = class(ESynException);

  /// customize YAML serialization output - NOT IMPLEMENTED YET
  // - ywoFlowCompact: emit flow style ([] / {}) for short / leaf containers
  // - ywoNoComments: placeholder; this parser does not preserve comments
  TYamlWriterOption = (
    ywoFlowCompact,
    ywoNoComments);
  TYamlWriterOptions = set of TYamlWriterOption;

const
  /// TDocVariant flavor for YAML, enabling floating numbers
  // - we don't enable name interning by default because it is actually slower
  // for typical YAML small content
  JSON_YAML = JSON_FAST_FLOAT;

/// parse YAML UTF-8 text into a TDocVariantData
// - accepts YAML 1.2 core-schema subset (see unit header)
// - raises EYamlException on unsupported constructs or syntactic errors
// - Options defaults to mormot.net.openapi-compatible values when empty
procedure YamlToVariant(const Yaml: RawUtf8; out Doc: TDocVariantData;
  Options: TDocVariantOptions = JSON_YAML);

/// convenient wrapper called e.g. by TOpenApiParser.ParseYaml()
// - will catch internally any EYamlException and return false on failure
function TryYamlToVariant(const Yaml: RawUtf8;
  out Doc: TDocVariantData; Options: TDocVariantOptions = JSON_YAML): boolean;

/// parse a YAML file into a TDocVariantData
// - file is expected to be UTF-8 (BOM tolerated); see YamlToVariant
// - will catch internally any EYamlException and return false on failure
function TryYamlFileToVariant(const FileName: TFileName; out Doc: TDocVariantData;
  Options: TDocVariantOptions = JSON_YAML): boolean;

/// serialize a TDocVariant as YAML 1.2 UTF-8 text
function VariantToYaml(const Doc: variant;
  Options: TYamlWriterOptions = []): RawUtf8;

/// save a TDocVariant as a YAML file (UTF-8, no BOM, LF line endings)
procedure SaveVariantToYamlFile(const Doc: variant; const FileName: TFileName;
  Options: TYamlWriterOptions = []);

/// recognize *.yaml and *.yml file patterns
function IsYamlFileName(const FileName: TFileName): boolean;

/// convert YAML 1.2 UTF-8 text directly into a JSON RawUtf8
// - convenience wrapper around the internal parser's JSON buffer, useful for
// pipelines that feed further JSON-based processing (e.g. RTTI settings,
// JsonToObject, LoadJson) without going through TDocVariantData
// - returns '' on parse failure (inspect EYamlException for details)
function YamlToJson(const Yaml: RawUtf8): RawUtf8;

/// convert YAML 1.2 UTF-8 text directly into a JSON RawUtf8
// - will catch internally any EYamlException and return false on failure
function TryYamlToJson(const Yaml: RawUtf8; out Json: RawUtf8): boolean;

/// convert JSON UTF-8 text into YAML 1.2 UTF-8 text
// - pipes the JSON through TDocVariantData then VariantToYaml
// - useful for converting existing JSON settings files or API payloads to YAML
function JsonToYaml(const Json: RawUtf8; Options: TYamlWriterOptions = [];
  DocOptions: TDocVariantOptions = JSON_YAML): RawUtf8;

var
  /// maximum YAML nesting depth before the parser raises EYamlException
  // - default 512 is ample for real-world OpenAPI specs; raise for
  // pathologically deep inputs
  // - converts would-be EStackOverflow into a clean EYamlException with
  // line info; guards both block and flow recursive descent
  YamlMaxDepth: integer = 512;


{ low-level functions which may be handy in some scenarios }

/// YAML 1.2 core-schema decimal/hexa/octal integer with safe overflow detection
// - return false on signed 64-bit overflow so caller falls back to string
function TryYamlInt(p: PUtf8Char; out V: Int64): boolean;

/// strip an unquoted trailing "# ..." YAML comment from a scalar fragment
// - accounts for single/double quoted spans where # is literal
procedure YamlStripLineComment(var S: RawUtf8);

/// emit some text as YAML string, with JSON-escaped double-quotes if needed
procedure WriteYamlString(W: TJsonWriter; P: PUtf8Char; L: PtrInt);

/// emit some variant as YAML string, with JSON-escaped double-quotes if needed
procedure WriteYamlVariant(W: TJsonWriter; const V: variant);


{ ************* Markup (e.g. Markdown or Emoji) process }

type
  /// tune AddHtmlEscapeWiki/AddHtmlEscapeMarkdown wrapper functions process
  // - heHtmlEscape will escape any HTML special chars, e.g. & into &amp;
  // - heEmojiToUtf8 will convert any Emoji text into UTF-8 Unicode character,
  // recognizing e.g. :joy: or :) in the text
  TTextWriterHtmlEscape = set of (
    heHtmlEscape,
    heEmojiToUtf8);

/// convert some wiki-like text into proper HTML
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, +..+ into
// <strong>..</strong>, `..` into <code>..</code>, and http://... as
// <a href=http://...>
// - escape any HTML special chars, and Emoji tags as specified with esc
procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]);

/// convert minimal Markdown text into proper HTML
// - see https://enterprise.github.com/downloads/en/markdown-cheatsheet.pdf
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, **..** into
// <strong>..</strong>, `...` into <code>...</code>, backslash espaces \\
// \* \_ and so on, [title](http://...) and detect plain http:// as
// <a href=...>
// - create unordered lists from trailing * + - chars, blockquotes from
// trailing > char, and code line from 4 initial spaces
// - as with default Markdown, won't escape HTML special chars (i.e. you can
// write plain HTML in the supplied text) unless esc is set otherwise
// - only inline-style links and images are supported yet (not reference-style);
// tables aren't supported either
procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]);

/// escape some wiki-marked text into HTML
// - just a wrapper around AddHtmlEscapeWiki() process
function HtmlEscapeWiki(const wiki: RawUtf8;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]): RawUtf8;

/// escape some Markdown-marked text into HTML
// - just a wrapper around AddHtmlEscapeMarkdown() process
function HtmlEscapeMarkdown(const md: RawUtf8;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]): RawUtf8;

type
  /// map the first Unicode page of Emojis, from U+1F600 to U+1F64F
  // - naming comes from github/Markdown :identifiers: and copied in EMOJI_RTTI[]
  TEmoji = (
    eNone,
    eGrinning,
    eGrin,
    eJoy,
    eSmiley,
    eSmile,
    eSweat_smile,
    eLaughing,
    eInnocent,
    eSmiling_imp,
    eWink,
    eBlush,
    eYum,
    eRelieved,
    eHeart_eyes,
    eSunglasses,
    eSmirk,
    eNeutral_face,
    eExpressionless,
    eUnamused,
    eSweat,
    ePensive,
    eConfused,
    eConfounded,
    eKissing,
    eKissing_heart,
    eKissing_smiling_eyes,
    eKissing_closed_eyes,
    eStuck_out_tongue,
    eStuck_out_tongue_winking_eye,
    eStuck_out_tongue_closed_eyes,
    eDisappointed,
    eWorried,
    eAngry,
    ePout,
    eCry,
    ePersevere,
    eTriumph,
    eDisappointed_relieved,
    eFrowning,
    eAnguished,
    eFearful,
    eWeary,
    eSleepy,
    eTired_face,
    eGrimacing,
    eSob,
    eOpen_mouth,
    eHushed,
    eCold_sweat,
    eScream,
    eAstonished,
    eFlushed,
    eSleeping,
    eDizzy_face,
    eNo_mouth,
    eMask,
    eSmile_cat,
    eJoy_cat,
    eSmiley_cat,
    eHeart_eyes_cat,
    eSmirk_cat,
    eKissing_cat,
    ePouting_cat,
    eCrying_cat_face,
    eScream_cat,
    eSlightly_frowning_face,
    eSlightly_smiling_face,
    eUpside_down_face,
    eRoll_eyes,
    eNo_good,
    oOk_woman,
    eBow,
    eSee_no_evil,
    eHear_no_evil,
    eSpeak_no_evil,
    eRaising_hand,
    eRaised_hands,
    eFrowning_woman,
    ePerson_with_pouting_face,
    ePray);

var
  /// github/Markdown compatible text of Emojis
  // - e.g. 'grinning' or 'person_with_pouting_face'
  EMOJI_TEXT: array[TEmoji] of RawUtf8;

  /// github/Markdown compatible tag of Emojis, including trailing and ending :
  // - e.g. ':grinning:' or ':person_with_pouting_face:'
  EMOJI_TAG: array[TEmoji] of RawUtf8;

  /// the Unicode character matching a given Emoji, after UTF-8 encoding
  EMOJI_UTF8: array[TEmoji] of RawUtf8;

  /// low-level access to TEmoji RTTI - used when inlining EmojiFromText()
  EMOJI_RTTI: PShortString;

  /// to recognize simple :) :( :| :/ :D :o :p :s characters as smilleys
  EMOJI_AFTERDOTS: array['('..'|'] of TEmoji;

/// setup the Emoji internal structures, e.g. all EMOJI_*[] global variables
// - delayed to reduce process startup time for this seldom-used feature
procedure EmojiInit;

/// recognize github/Markdown compatible text of Emojis
// - for instance 'sunglasses' text buffer will return eSunglasses
// - returns eNone if no case-insensitive match was found
function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
  {$ifdef HASINLINE}inline;{$endif}

/// low-level parser of github/Markdown compatible text of Emojis
// - supplied P^ should point to ':'
// - will append the recognized UTF-8 Emoji if P contains e.g. :joy: or :)
// - will append ':' if no Emoji text is recognized, and return eNone
// - will try both EMOJI_AFTERDOTS[] and EMOJI_RTTI[] reference set
// - if W is nil, won't append anything, but just return the recognized TEmoji
function EmojiParseDots(var P: PUtf8Char; W: TTextWriter = nil): TEmoji;

/// low-level conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
procedure EmojiToDots(P: PUtf8Char; W: TTextWriter); overload;

/// conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
function EmojiToDots(const text: RawUtf8): RawUtf8; overload;

/// low-level conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
procedure EmojiFromDots(P: PUtf8Char; W: TTextWriter); overload;

/// conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
function EmojiFromDots(const text: RawUtf8): RawUtf8; overload;


{ ************ INI Files In-memory Access }

/// find a Name= Value in a [Section] of a INI RawUtf8 Content
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
// - will follow INI relaxed expectations, i.e. ignore spaces/tabs around
// the '=' or ':' sign, and at the end of each input text line
function FindIniEntry(const Content, Section, Name: RawUtf8;
  const DefaultValue: RawUtf8 = ''): RawUtf8;

/// find a Name= Value in a [Section] of a INI WinAnsi Content
// - same as FindIniEntry(), but the value is converted from WinAnsi into UTF-8
function FindWinAnsiIniEntry(const Content, Section, Name: RawUtf8): RawUtf8;

/// find a Name= numeric Value in a [Section] of a INI RawUtf8 Content and
// return it as an integer, or 0 if not found
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntryInteger(const Content, Section, Name: RawUtf8): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// find a Name= Value in a [Section] of a .INI file
// - if Section equals '', find the Name= value before any [Section]
// - use internally fast FindIniEntry() function above
function FindIniEntryFile(const FileName: TFileName;
  const Section, Name: RawUtf8; const DefaultValue: RawUtf8 = ''): RawUtf8;

/// update a Name= Value in a [Section] of a INI RawUtf8 Content
// - this function scans and update the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', update the Name= value before any [Section]
procedure UpdateIniEntry(var Content: RawUtf8; const Section, Name, Value: RawUtf8);

/// update a Name= Value in a [Section] of a .INI file
// - if Section equals '', update the Name= value before any [Section]
// - use internally fast UpdateIniEntry() function above
procedure UpdateIniEntryFile(const FileName: TFileName; const Section, Name, Value: RawUtf8);

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
function FindSectionFirstLine(var source: PUtf8Char; search: PAnsiChar;
  sourceend: PPUtf8Char = nil): boolean;

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
// - this version expects source^ to point to an Unicode char array
function FindSectionFirstLineW(var source: PWideChar; search: PUtf8Char): boolean;

/// retrieve the whole content of a section as a string
function GetSectionContent(const Content, SectionName: RawUtf8): RawUtf8;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
function DeleteSection(var Content: RawUtf8; const SectionName: RawUtf8;
  EraseSectionHeader: boolean = true): boolean; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function DeleteSection(SectionFirstLine: PUtf8Char; var Content: RawUtf8;
  EraseSectionHeader: boolean = true): boolean; overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
procedure ReplaceSection(var Content: RawUtf8; const SectionName,
  NewSectionContent: RawUtf8); overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
procedure ReplaceSection(SectionFirstLine: PUtf8Char;
  var Content: RawUtf8; const NewSectionContent: RawUtf8); overload;

/// return TRUE if Value of UpperName does exist in P, till end of current section
// - expects UpperName as 'NAME=' or 'HTTPHEADERNAME:'
// - will follow INI relaxed expectations, i.e. ignore spaces/tabs around '='/':'
function ExistsIniName(P: PUtf8Char; UpperName: PAnsiChar): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// find the Value of UpperName in P, till end of current INI section
// - expect UpperName already as 'NAME=' for efficient INI key=value lookup
// - will follow INI relaxed expectations, i.e. ignore spaces/tabs around
// the '=' or ':' sign, and at the end of each input text line
function FindIniNameValue(P: PUtf8Char; UpperName: PAnsiChar;
  const DefaultValue: RawUtf8 = ''; PEnd: PUtf8Char = nil): RawUtf8;

/// raw internal function used by FindIniNameValue() with no result allocation
// - Value=nil if not found, or Value<>nil and return length (may be 0 for '')
function FindIniNameValueP(P, PEnd: PUtf8Char; UpperName: PAnsiChar;
  out Value: PUtf8Char): PtrInt;

/// find the Value of UpperName in Content, wrapping FindIniNameValue()
// - will follow INI relaxed expectations, i.e. ignore spaces/tabs around
// the '=' or ':' sign, and at the end of each input text line
function FindIniNameValueU(const Content: RawUtf8; UpperName: PAnsiChar): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// return TRUE if one of the leftmost Value of UpperName exists in P
// - expect UpperName e.g. as 'CONTENT-TYPE: ' (i.e. HEADER_CONTENT_TYPE_UPPER)
// - expect UpperValues to be an array of upper values with left side matching,
// and ending with nil - as expected by IdemPPChar(), i.e. with at least 2 chars
// - note: won't ignore spaces/tabs around the '=' sign
function ExistsIniNameValue(P: PUtf8Char; const UpperName: RawUtf8;
  UpperValues: PPAnsiChar): boolean;

/// find the integer Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME=' or 'CONTENT-LENGTH:'
// - return 0 if no NAME= entry was found
// - will follow INI relaxed expectations, i.e. ignore spaces/tabs around '='/':'
function FindIniNameValueInteger(P: PUtf8Char; const UpperName: RawUtf8): PtrInt;

/// replace a value from a given set of name=value lines
// - expect UpperName as 'UPPERNAME=', otherwise returns false
// - if no UPPERNAME= entry was found, then Name+NewValue is added to Content
// - a typical use may be:
// ! UpdateNameValue(headers,HEADER_CONTENT_TYPE,HEADER_CONTENT_TYPE_UPPER,contenttype);
// - note: won't ignore spaces/tabs around the '=' sign
function UpdateNameValue(var Content: RawUtf8;
  const Name, UpperName, NewValue: RawUtf8): boolean;

/// returns TRUE if the supplied HTML Headers contains 'Content-Type: text/...',
// 'Content-Type: application/json' or 'Content-Type: application/xml'
function IsHttpHeadersContentTypeText(Headers: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// search if the WebSocketUpgrade() header is present
// - consider checking the hsrConnectionUpgrade flag instead
function IsHttpHeadersTextWebSocketUpgrade(headers: PUtf8Char): boolean;

type
  /// define IniToObject() and ObjectToIni() extended features
  // - nested objects and multi-line text (if ifMultiLineSections is set) are
  // stored in their own section, named from their section level and property
  // (e.g. [mainprop.nested]) with ifClassSection feature, and/or as
  // mainprop.nested.field = value with ifClassValue
  // - nested arrays (with ifArraySection) are stored as inlined JSON or within
  // prefixed sections like [nested-xxx] with any not azAZ_09 as xxx separator
  // - ifMultiLineJsonArray would parse multi-line field=[val1,... JSON arrays
  // - ifClearValues will let IniToObject() call TRttiCustomProp.ClearValue()
  // on each property
  TIniFeatures = set of (
    ifClassSection, ifClassValue, ifMultiLineSections, ifArraySection,
    ifMultiLineJsonArray, ifClearValues);

/// fill a class Instance properties from an .ini content
// - the class property fields are searched in the supplied main SectionName
// (if SectionName='' then nested objects or arrays will be parsed from their
// own section)
// - returns true if at least one property has been identified
function IniToObject(const Ini: RawUtf8; Instance: TObject;
  const SectionName: RawUtf8 = 'Main'; DocVariantOptions: PDocVariantOptions = nil;
  Level: integer = 0; Features: TIniFeatures =
    [ifClassSection, ifClassValue, ifMultiLineSections, ifArraySection]): boolean;

/// serialize a class Instance properties into an .ini content
// - the class property fields are written in the supplied main SectionName
// - nested objects and multi-line text values are written in their own section,
// named from their section level and property (e.g. [mainprop.nested1.nested2])
function ObjectToIni(const Instance: TObject; const SectionName: RawUtf8 = 'Main';
  Options: TTextWriterWriteObjectOptions =
    [woEnumSetsAsText, woRawBlobAsBase64, woHumanReadableEnumSetAsComment];
    Level: integer = 0; Features: TIniFeatures =
      [ifClassSection, ifMultiLineSections, ifArraySection]): RawUtf8;


{ ************* TSynJsonFileSettings parent class }

type
  /// customize TSynJsonFileSettings process
  // - fsoDisableSaveIfNeeded will disable SaveIfNeeded method process
  // - fsoReadIni will disable JSON loading, and expect INI file format
  // - fsoWriteIni/fsoWriteHjson will force SaveIfNeeded to use INI/HJson format
  // - fsoWriteYaml will force SaveIfNeeded to use YAML encoding; there is no
  // fsoReadYaml since our YAML parser raise exceptions so explicit LoadFromYaml
  // LoadFromFile methods are cleaner for this new feature
  // - fsoNoEnumsComment will customize SaveIfNeeded output
  TSynJsonFileSettingsOption = (
    fsoDisableSaveIfNeeded,
    fsoReadIni,
    fsoWriteIni,
    fsoWriteYaml,
    fsoWriteHjson,
    fsoNoEnumsComment);
  TSynJsonFileSettingsOptions = set of TSynJsonFileSettingsOption;

  /// abstract parent class able to store settings as JSON / INI / YAML file
  // - supports standard JSON, but also JSON5, JSONC or HJson variations
  // - fallback and try to read as INI or YAML file if no valid JSON is found
  TSynJsonFileSettings = class(TSynAutoCreateFields)
  protected
    fCurrentContent, fSectionName: RawUtf8;
    fFileName: TFileName;
    fLoadedAsIni: boolean;
    fSettingsOptions: TSynJsonFileSettingsOptions;
    fIniOptions: TIniFeatures;
    fCurrentHash: cardinal;
    // could be overriden to validate the content coherency and/or clean fields
    function AfterLoad(const aText: RawUtf8): boolean; virtual;
  public
    /// initialize this instance and all its published fields
    constructor Create; override;
    /// read existing settings from a JSON or INI (not YAML) content
    // - if the input is no JSON object, then .INI structure is tried
    // - if fsoReadIni options are set, only .INI format is tried, and no JSON
    // - since our YAML parser raise exceptions, we don't try YAML by default:
    // call explicitly the new LoadFromYaml() method or use .yaml/.yml explicit
    // extension for LoadFromFile()
    function LoadFromText(const aContent: RawUtf8;
      const aSectionName: RawUtf8 = 'Main'): boolean;
    {$ifndef PUREMORMOT2}
    /// read existing settings from a JSON or INI (not YAML) content
    // - backward compatibility method - do not use any more but LoadFromText()
    // or the explicit LoadFromJsonText/LoadFromIniText/LoadFromYamlText methods
    function LoadFromJson(const aContent: RawUtf8;
      const aSectionName: RawUtf8 = 'Main'): boolean;
    {$endif PUREMORMOT2}
    /// read existing settings from a JSON content
    // - misleading LoadFromJson() is kept for backward compatibility purposes
    function LoadFromJsonText(const aJson: RawUtf8): boolean;
    /// read existing settings from a INI content
    function LoadFromIniText(const aIni, aSectionName: RawUtf8): boolean;
    /// read existing settings from a YAML content
    // - since YAML
    function LoadFromYamlText(const aYaml: RawUtf8): boolean;
    /// read existing settings from a JSON or INI or YAML file
    // - will guess the format for the .json .ini .yaml file extension, or
    // fallback to LoadFromText() to recognize JSON variants and INI format
    function LoadFromFile(const aFileName: TFileName;
      const aSectionName: RawUtf8 = 'Main'): boolean; virtual;
    /// just a wrapper around ExtractFilePath(FileName);
    function FolderName: TFileName;
    /// persist the settings as a JSON file, named from LoadFromFile() parameter
    // - will use the INI format if it was used at loading, or fsoWriteIni is set
    // - will use the YAML format if it was used at loading, or fsoWriteYaml is set
    // - return TRUE if file has been modified, FALSE if was not needed or failed
    function SaveIfNeeded: boolean; virtual;
    /// optional persistence file name, as set by LoadFromFile()
    property FileName: TFileName
      read fFileName write fFileName;
    /// allow to customize the storing process
    property SettingsOptions: TSynJsonFileSettingsOptions
      read fSettingsOptions write fSettingsOptions;
    /// allow to customize fsoReadIni/fsoWriteIni storing process
    property IniOptions: TIniFeatures
      read fIniOptions write fIniOptions;
    /// can be used to compare two instances original file content
    // - will use DefaultHashTrim() so value could change after process restart
    property InitialFileHash: cardinal
      read fCurrentHash write fCurrentHash;
  end;
  /// meta-class definition of TSynJsonFileSettings
  TSynJsonFileSettingsClass = class of TSynJsonFileSettings;


{ ********** JSON and Text Preprocessor }

type
  /// tune JsonPreprocess() pre-processor features
  // - jppIncludeAbsolute enables unsafe "include <file>" outside the root folder
  // - jppDebugComment will emit verbose debugging comments during pre-processing
  TPreprocFlags = set of (
    jppIncludeAbsolute,
    jppDebugComment);

/// pre-process and format JSON with $".." and $(ident) expansion
function JsonPreprocess(const Json: RawUtf8;
  Format: TTextWriterJsonFormat = jsonHumanReadable;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): RawUtf8;

/// pre-process and format JSON with $".." and $(ident) expansion into a file
function JsonPreprocessToFile(const Json: RawUtf8; const Dest: TFileName;
  Format: TTextWriterJsonFormat = jsonHumanReadable;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): boolean;

/// pre-process and format JSON with $".." and $(ident) expansion into a stream
function JsonPreprocessToStream(const Json: RawUtf8; Dest: TStream;
  Format: TTextWriterJsonFormat = jsonHumanReadable;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): boolean;

/// pre-process any text with $".." and $(ident) expansion
function TextPreprocess(const Text: RawUtf8;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): RawUtf8;

/// pre-process any text with $".." and $(ident) expansion into a file
function TextPreprocessToFile(const Text: RawUtf8; const Dest: TFileName;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): boolean;

/// pre-process any text with $".." and $(ident) expansion into a stream
function TextPreprocessToStream(const Text: RawUtf8; Dest: TStream;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): boolean;


{ ********** Source Code Generation Functions }

const
  // published for unit testing in TNetworkProtocols.OpenAPI (e.g. if sorted)
  RESERVED_KEYWORDS: array[0..91] of RawUtf8 = (
    'ABSOLUTE', 'ABSTRACT', 'ALIAS', 'AND', 'ARRAY', 'AS', 'ASM', 'ASSEMBLER',
    'BEGIN', 'CASE', 'CLASS', 'CONST', 'CONSTREF', 'CONSTRUCTOR', 'DESTRUCTOR',
    'DIV', 'DO', 'DOWNTO', 'ELSE', 'END', 'EXCEPT', 'EXPORT', 'EXTERNAL',
    'FALSE', 'FAR', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR', 'FORWARD',
    'FUNCTION', 'GENERIC', 'GOTO', 'IF', 'IMPLEMENTATION', 'IN', 'INHERITED',
    'INITIALIZATION', 'INLINE', 'INTERFACE', 'IS', 'LABEL', 'LIBRARY', 'MOD',
    'NEAR', 'NEW', 'NIL', 'NOT', 'OBJECT', 'OF', 'ON', 'OPERATOR', 'OR', 'OUT',
    'OVERRIDE', 'PACKED', 'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PROPERTY',
    'PROTECTED', 'PUBLIC', 'PUBLISHED', 'RAISE', 'READ', 'RECORD',
    'REINTRODUCE', 'REPEAT', 'RESOURCESTRING', 'SELF', 'SET', 'SHL', 'SHR',
    'STATIC', 'STRING', 'THEN', 'THREADVAR', 'TO', 'TRUE', 'TRY', 'TYPE',
    'UNIT', 'UNTIL', 'USES', 'VAR', 'VARIANT', 'VIRTUAL', 'WHILE', 'WITH',
    'WRITE', 'WRITELN', 'XOR');

/// quickly check if a text is a case-insensitive pascal code keyword
function IsReservedKeyWord(const aName: RawUtf8): boolean;

/// wrap CamelCase() and IsReservedKeyWord() to generate a valid pascal identifier
// - if aName is void after camel-casing, will raise an ESynUnicode
function SanitizePascalName(const aName: RawUtf8; KeyWordCheck: boolean): RawUtf8;

/// generate some pascal source code holding some data binary as constant
// - can store sensitive information (e.g. certificates) within the executable
// - generates a source code snippet of the following format:
// ! const
// !   // Comment
// !   ConstName: array[0..2] of byte = (
// !     $01, $02, $03);
function BinToSource(const ConstName, Comment: RawUtf8; Data: pointer;
  Len: PtrInt; PerLine: PtrInt = 16; const Suffix: RawUtf8 = '';
  LF: TLineFeed = lfSystem): RawUtf8; overload;

/// generate some pascal source code holding some data binary as constant
function BinToSource(const ConstName, Comment: RawUtf8; const Data: RawByteString;
  PerLine: PtrInt = 16; const Suffix: RawUtf8 = '';
  LF: TLineFeed = lfSystem): RawUtf8; overload;

/// generate some pascal source code holding some data binary as constant
procedure BinToSource(Dest: TTextWriter; const ConstName, Comment: RawUtf8;
  Data: pointer; Len, PerLine: PtrInt; const LF: ShortString); overload;

/// generate some pascal source code string constant from UTF-8 text buffer
procedure TextToSource(Dest: TTextWriter; P: PUtf8Char; const LF: ShortString); overload;

/// generate some pascal source code string constant from UTF-8 text
function TextToSource(const Text: RawUtf8; LF: TLineFeed = lfSystem): RawUtf8; overload;

/// generate some pascal source code string constant from UTF-8 text
function TextToSourceShort(const Text: RawUtf8; LF: TLineFeed = lfSystem): ShortString; overload;


implementation


{ ************* HTML Text Conversions }

var
  HTML_ESC: array[hfAnyWhere..hfWithinAttributes] of TAnsiCharToByte;
const
  HTML_ESCAPED: array[1 .. 4] of TShort7 = (
    '&lt;', '&gt;', '&amp;', '&quot;');

procedure __AddHtmlEscape(W: TTextWriter; Text: PUtf8Char; TextLen: PtrInt;
  Fmt: TTextWriterHtmlFormat);
var
  beg: PUtf8Char;
  esc: PAnsiCharToByte;
begin
  if Text <> nil then
    if Fmt <> hfNone then
    begin
      if TextLen > 0 then
        inc(TextLen, PtrUInt(Text)) // PtrUInt(TextLen) = PtrUInt(TextEnd)
      else
        TextLen := 0;
      esc := @HTML_ESC[Fmt];
      beg := Text;
      repeat
        if TextLen = 0 then // slightly faster without TextLen check
          while true do
            if esc[Text^] = 0 then
              inc(Text)
            else
              break
        else
          while (PtrUInt(Text) < PtrUInt(TextLen)) and
                (esc[Text^] = 0) do
            inc(Text);
        W.AddNoJsonEscape(beg, Text - beg);
        if (Text^ = #0) or
           ((TextLen <> 0) and
            (PtrUInt(Text) >= PtrUInt(TextLen))) then
          exit;
        W.AddShorter(HTML_ESCAPED[esc[Text^]]);
        inc(Text);
        beg := Text;
      until (Text^ = #0) or
            ((TextLen <> 0) and
             (PtrUInt(Text) >= PtrUInt(TextLen)));
    end
    else
    begin
      if TextLen <= 0 then
        TextLen := StrLen(Text);
      W.AddNoJsonEscape(Text, TextLen); // hfNone
    end;
end;

procedure AddHtmlEscapeUtf8(W: TTextWriter; const Text: RawUtf8;
  Fmt: TTextWriterHtmlFormat);
var
  p: PUtf8Char;
begin
  p := pointer(Text);
  if p <> nil then
    if Fmt <> hfNone then
      __AddHtmlEscape(W, p, {TextLen=}0, Fmt) // faster with no TextLen
    else
      W.AddNoJsonEscapeBig(p, PStrLen(p - _STRLEN)^) // seldom called
end;

procedure AddHtmlUnescape(W: TTextWriter; p, amp: PUtf8Char; plen: PtrUInt);
var
  l: PtrUInt;
  c: Ucs4CodePoint;
begin
  repeat
    if amp = nil then
    begin
      amp := PosChar(p, plen, '&'); // use fast SSE2 asm on x86_64
      if amp = nil then
      begin
        W.AddNoJsonEscape(p, plen); // no more content to escape
        exit;
      end;
    end;
    l := amp - p;
    if l <> 0 then
    begin
      W.AddNoJsonEscape(p, l);
      dec(plen, l);
      if plen = 0 then
        exit;
      p := amp;
    end;
    amp := nil; // call PosChar() on next iteration
    inc(p); // ignore '&'
    dec(plen);
    l := 0;
    while (l < plen) and
          (p[l] in ['a'..'z', 'A'..'Z', '1'..'4']) do
      inc(l);
    if p[l] = ';' then
    begin
      c := EntityToUcs4(p, l); // &lt; -> ord('<')
      if c <> 0 then
      begin
        if c = $00a0 then             // &nbsp;
          c := ord(' ');
        if c <= $7f then              // &amp;
          W.Add(AnsiChar(c))
        else if c = $2026 then
          W.AddShort4(DOT_24, 3)      // &hellip;
        else
          W.AddWideChar(WideChar(c)); // &Eacute;
        inc(l); // consume ending ;
        inc(p, l);
        dec(plen, l);
        continue;
      end;
    end;
    W.AddDirect('&');
  until plen = 0;
end;

function NeedsHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat): boolean;
var
  esc: PAnsiCharToByte;
begin
  if (Text <> nil) and
     (Fmt <> hfNone) then
  begin
    result := true;
    esc := @HTML_ESC[Fmt];
    while true do
      if esc[Text^] = 0 then
        inc(Text) // fast process of unescaped plain text
      else if Text^ = #0 then
        break     // no escape needed
      else
        exit;     // needs XML escape
  end;
  result := false;
end;

function HtmlEscape(const text: RawUtf8; fmt: TTextWriterHtmlFormat): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  if NeedsHtmlEscape(pointer(text), fmt) then
  begin
    W := TTextWriter.CreateOwnedStream(temp);
    try
      __AddHtmlEscape(W, pointer(text), {TextLen=}0, fmt);
      W.SetText(result);
    finally
      W.Free;
    end;
  end
  else
    result := text;
end;

function HtmlEscapeShort(const text: RawUtf8; fmt: TTextWriterHtmlFormat): ShortString;
var
  temp: TLocalWriter;
begin
  if NeedsHtmlEscape(pointer(text), fmt) then
  begin
    __AddHtmlEscape(temp.Init(result), pointer(text), {TextLen=}0, fmt);
    temp.Done;
  end
  else
    Ansi7StringToShortString(text, result);
end;

function HtmlEscapeString(const text: string; fmt: TTextWriterHtmlFormat): RawUtf8;
begin
  HtmlEscapeString(text, result, fmt);
end;

procedure HtmlEscapeString(const text: string; var result: RawUtf8; fmt: TTextWriterHtmlFormat);
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  {$ifdef UNICODE}
  if fmt = hfNone then
  begin
    StringToUtf8(text, result);
    exit;
  end;
  {$else}
  if not NeedsHtmlEscape(pointer(text), fmt) then // work for any AnsiString
  begin
    if IsAnsiCompatible(text) then
      result := text
    else
      StringToUtf8(text, result);
    exit;
  end;
  {$endif UNICODE}
  W := TTextWriter.CreateOwnedStream(temp);
  try
    W.AddHtmlEscapeString(text, fmt);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

const // rough but efficient storage of all &xxx; entities for fast SSE2 search
  HTML_UNESCAPE: array[1 .. 100] of array[0 .. 3] of AnsiChar = (
    'amp',  'quot', 'rsqu', {4=}'ndas', {5=}'trad', {6=}'hell',
    'nbsp', 'iexc', 'cent', 'poun', 'curr', 'yen',  'brvb', 'sect', 'uml',
    'copy', 'ordf', 'laqu', 'not',  'shy',  'reg',  'macr', 'deg',  'plus',
    'sup2', 'sup3', 'acut', 'micr', 'para', 'midd', 'cedi', 'sup1', 'ordm',
    'raqu',  {35=}'frac',   'ique', 'Agra', 'Aacu', 'Acir', 'Atil',
    'Auml', 'Arin', 'AEli', 'Cced', 'Egra', 'Eacu', 'Ecir', 'Euml', 'Igra',
    'Iacu', 'Icir', 'Iuml', 'ETH',  'Ntil', 'Ogra', 'Oacu', 'Ocir', 'Otil',
    'Ouml', 'time', 'Osla', 'Ugra', 'Uacu', 'Ucir', 'Uuml', 'Yacu', 'THOR',
    'szli', 'agra', 'aacu', 'acir', 'atil', 'auml', 'arin', 'aeli', 'cced',
    'egra', 'eacu', 'ecir', 'euml', 'igra', 'iacu', 'icir', 'iuml', 'eth',
    'ntil', 'ogra', 'oacu', 'ocir', 'otil', 'ouml', 'divi', 'osla', 'ugra',
    'uacu', 'ucir', 'uuml', 'yacu', 'thor', 'yuml');
  HTML_UNESCAPED: array[1 .. 6] of word = (
    ord('&'), ord('"'), ord(''''), ord('-'), 153, $2026);

function EntityToUcs4(entity: PUtf8Char; len: byte): Ucs4CodePoint;
var
  by4: cardinal;
  ndx: PtrInt;
label
  ok;
begin
  case len of
    2:
      begin
        case cardinal(PWord(entity)^) of
          ord('l') + ord('t') shl 8:
            ndx := ord('<');
          ord('g') + ord('t') shl 8:
            ndx := ord('>');
        else
          ndx := 0;
        end;
        goto ok;
      end;
    3:
      by4 := PCardinal(entity)^ and $ffffff;
    4 .. 6:
      by4 := PCardinal(entity)^;
  else
    begin
      ndx := 0;
      goto ok;
    end;
  end;
  ndx := IntegerScanIndex(@HTML_UNESCAPE, length(HTML_UNESCAPE), by4) + 1;
  if ndx >= 35 then // adjust 'frac' as frac14', 'frac12' or 'frac34'
    if ndx > 35 then
      inc(ndx, 2)
    else
      case cardinal(PWord(entity + 4)^) of
        ord('1') + ord('4') shl 8:
          ;
        ord('1') + ord('2') shl 8:
          inc(ndx);
        ord('3') + ord('4') shl 8:
          inc(ndx, 2);
      else
        ndx := 0;
      end;
  if ndx <> 0 then
    if ndx <= high(HTML_UNESCAPED) then
      ndx := HTML_UNESCAPED[ndx] // non linear entities
     else
       inc(ndx, $00a0 - 7); // &nbsp; = U+00A0, &iexcl; = U+00A1, ...
ok:result := ndx;
end;

function HtmlUnescape(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  amp: PUtf8CHar;
  temp: TTextWriterStackBuffer;
begin
  amp := PosCharU(text, '&'); // use fast SSE2 asm on x86_64
  if amp = nil then
  begin
    result := text; // nothing to change
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlUnescape(W, pointer(text), amp, length(text));
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlToText(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  tag: PUtf8CHar;
  temp: TTextWriterStackBuffer;
begin
  tag := PosCharU(text, '<'); // use fast SSE2 asm on x86_64
  if tag = nil then
  begin
    result := HtmlUnescape(text); // no tag, but there may be some &entity;
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlAsText(W, pointer(text), tag, length(text));
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlTagNeedsCRLF(tag: PUtf8Char): boolean;
var
  taglen: PtrUInt;
begin
  result := false;
  if tag^ = '/' then
    inc(tag); // identify </p> just like <p>
  taglen := 0;
  if tag[taglen] in ['a'..'z', 'A'..'Z'] then
    repeat
      inc(taglen);
    until (taglen > 3) or
          not (tag[taglen] in ['a'..'z', 'A'..'Z', '1'..'9']);
  case taglen of
    1:
      result := tag^ in ['p', 'P'];
    2:
      case cardinal(PWord(tag)^) and $dfdf of
        ord('B') + ord('R') shl 8,
        ord('L') + ord('I') shl 8,
        ord('H') + (ord('1') and $df) shl 8,
        ord('H') + (ord('2') and $df) shl 8,
        ord('H') + (ord('3') and $df) shl 8,
        ord('H') + (ord('4') and $df) shl 8,
        ord('H') + (ord('5') and $df) shl 8,
        ord('H') + (ord('6') and $df) shl 8:
          result := true;
      end;
    3:
      result := PCardinal(tag)^ and $00dfdfdf =
                  ord('D') + ord('I') shl 8 + ord('V') shl 16;
  end;
end;

procedure AddHtmlAsText(W: TTextWriter; p, tag: PUtf8Char; plen: PtrUInt);
var
  l: PtrInt;
begin
  repeat
    if tag = nil then
    begin
      tag := PosChar(p, plen, '<'); // use fast SSE2 asm on x86_64
      if tag = nil then
      begin
        AddHtmlUnescape(W, p, nil, plen);
        exit;
      end;
    end;
    l := tag - p;
    if l <> 0 then
    begin
      AddHtmlUnescape(W, p, nil, l);
      dec(plen, l);
      if plen = 0 then
        exit;
      p := tag;
    end;
    inc(p); // ignore '<'
    dec(plen);
    tag := PosChar(p, plen, '>');
    if tag = nil then
      W.Add('<') // not a real tag
    else
    begin
      if HtmlTagNeedsCRLF(p) then
        if W.LastChar >= ' ' then // <p> <h1> append once a line feed
          W.AddDirect(#13, #10);
      l := tag - p + 1;
      inc(p, l);
      dec(plen, l);
    end;
    tag := nil; // call PosChar() on next iteration
  until plen = 0;
end;


{ ************* Basic XML Conversions }

var
  XML_ESC: TAnsiCharToByte;
const
  XML_ESCAPED: array[1..9] of TShort7 = (
    '&#x09;', '&#x0a;', '&#x0d;', '&lt;', '&gt;', '&amp;', '&quot;', '&apos;', '');

procedure AddXmlEscape(W: TTextWriter; Text: PUtf8Char);
var
  beg: PUtf8Char;
  esc: PAnsiCharToByte;
begin
  if (W = nil) or
     (Text = nil) or
     (Text^ = #0) then
    exit;
  esc := @XML_ESC;
  repeat
    beg := Text;
    while esc[Text^] = 0 do
      inc(Text);
    W.AddNoJsonEscape(beg, Text - beg);
    if Text^ = #0 then
      exit;
    W.AddShorter(XML_ESCAPED[esc[Text^]]);
    inc(Text);
  until Text^ = #0;
end;

function AddJsonToXml(W: TTextWriter; Json: PUtf8Char;
  ArrayName, EndOfObject: PUtf8Char): PUtf8Char;
var
  info: TGetJsonField;
  Name: PUtf8Char;
  n, c: integer;
begin
  result := nil;
  if Json = nil then
    exit;
  while (Json^ <= ' ') and
        (Json^ <> #0) do
    inc(Json);
  if Json^ = '/' then
    Json := GotoEndOfSlashComment(Json);
  case Json^ of
  '[':
    begin
      repeat
        inc(Json);
      until (Json^ = #0) or
            (Json^ > ' ');
      if Json^ = ']' then
        Json := GotoNextNotSpace(Json + 1)
      else
      begin
        n := 0;
        repeat
          if Json = nil then
            exit;
          W.Add('<');
          if ArrayName = nil then
            W.AddU(n)
          else
            AddXmlEscape(W, ArrayName);
          W.AddDirect('>');
          Json := AddJsonToXml(W, Json, nil, @info.EndOfObject);
          W.AddDirect('<', '/');
          if ArrayName = nil then
            W.AddU(n)
          else
            AddXmlEscape(W, ArrayName);
          W.AddDirect('>');
          inc(n);
        until info.EndOfObject = ']';
      end;
    end;
  '{':
    begin
      repeat
        inc(Json);
      until (Json^ = #0) or
            (Json^ > ' ');
      if Json^ = '}' then
        Json := GotoNextNotSpace(Json + 1)
      else
      begin
        repeat
          Name := GetJsonPropName(Json);
          if Name = nil then
            exit;
          while (Json^ <= ' ') and
                (Json^ <> #0) do
            inc(Json);
          if Json^ = '[' then // arrays are written as list of items, without root
            Json := AddJsonToXml(W, Json, Name, @info.EndOfObject)
          else
          begin
            W.Add('<');
            AddXmlEscape(W, Name);
            W.AddDirect('>');
            Json := AddJsonToXml(W, Json, Name, @info.EndOfObject);
            W.AddDirect('<', '/');
            AddXmlEscape(W, Name);
            W.AddDirect('>');
          end;
        until info.EndOfObject = '}';
      end;
    end;
  else
    begin // unescape the JSON content and write as UTF-8 escaped XML
      info.Json := Json;
      info.GetJsonField;
      if info.Value <> nil then // null or "" would store a void entry
      begin
        c := PInteger(info.Value)^ and $ffffff;
        if (c = JSON_BASE64_MAGIC_C) or
           (c = JSON_SQLDATE_MAGIC_C) then
          inc(info.Value, 3); // ignore the Magic codepoint encoded as UTF-8
        AddXmlEscape(W, info.Value);
      end;
      if EndOfObject <> nil then
        EndOfObject^ := info.EndOfObject;
      result := info.Json;
      exit;
    end;
  end;
  if Json <> nil then
  begin
    while (Json^ <= ' ') and
          (Json^ <> #0) do
      inc(Json);
    if EndOfObject <> nil then
      EndOfObject^ := Json^;
    if Json^ <> #0 then
      repeat
        inc(Json);
      until (Json^ = #0) or
            (Json^ > ' ');
  end;
  result := Json;
end;

function XmlEscape(const text: RawUtf8): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  if NeedsXmlEscape(pointer(text)) then
  begin
    W := TTextWriter.CreateOwnedStream(temp);
    try
      AddXmlEscape(W, pointer(text));
      W.SetText(result);
    finally
      W.Free;
    end;
  end
  else
    result := text;
end;

function NeedsXmlEscape(text: PUtf8Char): boolean;
var
  esc: PAnsiCharToByte;
begin
  result := true;
  esc := @XML_ESC;
  if Text <> nil then
    while true do
      if esc[Text^] = 0 then
        inc(Text) // fast process of unescaped plain text
      else if Text^ = #0 then
        break     // no escape needed
      else
        exit;     // needs XML escape
  result := false;
end;

procedure JsonBufferToXml(P: PUtf8Char; const Header, NameSpace: RawUtf8;
  out result: RawUtf8);
var
  i, j, namespaceLen: PtrInt;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  if P = nil then
    result := Header
  else
  begin
    W := TTextWriter.CreateOwnedStream(temp);
    try
      W.AddString(Header);
      namespaceLen := length(NameSpace);
      if namespaceLen <> 0 then
        W.AddString(NameSpace);
      AddJsonToXml(W, P);
      if namespaceLen <> 0 then
        for i := 1 to namespaceLen do
          if NameSpace[i] = '<' then
          begin
            for j := i + 1 to namespaceLen do
              if NameSpace[j] in [' ', '>'] then
              begin
                W.AddDirect('<', '/');
                W.AddStringCopy(NameSpace, i + 1, j - i - 1);
                W.AddDirect('>');
                break;
              end;
            break;
          end;
      W.SetText(result);
    finally
      W.Free;
    end;
  end;
end;

function JsonToXml(const Json, Header, NameSpace: RawUtf8): RawUtf8;
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Json);
  try
    JsonBufferToXml(tmp.buf, Header, NameSpace, result);
  finally
    tmp.Done;
  end;
end;


{ ************* YAML 1.2 core-schema to JSON or TDocVariant Support

  - plain / double-quoted / single-quoted scalars with core-schema type
    inference (null / bool / int / float / string)
  - block and flow mappings and sequences
  - literal (|) and folded (>) block scalars with strip/clip/keep chomping
  - comments (# to end of line) outside quoted scalars
  - raises EYamlException on unsupported constructs (&anchor, *alias,
    !!tag, multi-document ---) or on syntactic errors, with 1-based line
    information attached

   Parser strategy: YAML tokens are re-encoded into a JSON buffer, then passed
   to TDocVariantData.InitJson. This sidesteps building a second in-memory DOM
   and reuses the existing JSON scalar handling.

   Writer strategy: walk the TDocVariantData via TTextWriter and emit
   block-style YAML (flow-style when ywoFlowCompact is set).

   This code was initialy proposed by zen010101, using Claude AI, with manual
   refactoring for integration into mORMot: best performance is not the goal
   here, even if we reach more than 100MB/s which seems fast enough for YAML.
}

type
  TYamlLine = record
    Indent: PtrInt;   // count of leading space characters
    Content: RawUtf8; // trimmed of leading spaces and trailing \r
  end;
  TYamlLines = array of TYamlLine;
  PYamlLine = ^TYamlLine;

procedure YamlStripLineComment(var S: RawUtf8);
var
  p: PUtf8Char;
  cut: PtrInt;
begin
  if S = '' then
    exit;
  p := pointer(S);
  cut := -1;
  repeat
    case p^ of
      #0:
        break;
      '''':
        begin
          p := GotoEndOfQuotedString(p);
          if p^ <> '''' then
            exit;
        end;
      '"':
        begin
          p := GotoEndOfJsonString(p);
          if p^ <> '"' then
            exit;
        end;
      '#':
        if (p = pointer(S)) or
           ((p - 1)^ in [' ', #9]) then
        begin
          cut := p - PUtf8Char(pointer(S));
          break;
        end;
    end;
    inc(p);
  until false;
  if cut < 0 then
    exit;
  while (cut > 0) and
        (p[cut - 1] <= ' ') do
    dec(cut);
  SetLength(S, cut);
end;

function IsYamlNull(p: PUtf8Char): boolean;
  {$ifdef HASINLINE} inline; {$endif}
begin
  // YAML 1.2 core schema: null has three case-insensitive spellings,
  // the '~' shortcut, or the empty string
  result := (PWord(p)^ = ord('~')) or
            ((PInteger(p)^ and $DFDFDFDF = NULL_HI) and
             (p[4] = #0));
end;

function TryYamlInt(p: PUtf8Char; out V: Int64): boolean;
var
  neg: boolean;
  b: byte;
  u, prev: QWord;
begin
  result := false;
  if p = nil then
    exit;
  neg := false;
  if p^ in ['-', '+'] then
  begin
    neg := p^ = '-';
    inc(p);
  end;
  u := 0;
  if (p[0] = '0') and
     (p[1] in ['x', 'X']) then
  begin
    // hex (YAML 1.2 core schema: 0x[0-9a-fA-F]+)
    inc(p, 2);
    if p^ = #0 then
      exit;
    repeat
      b := ConvertHexToBin[p^];
      if b = 255 then
        if p^ = #0 then
          break
        else
          exit;
      prev := u;
      u := (u shl 4) or b;
      if Int64(u) < Int64(prev) then
        exit; // Delphi 7 compatible 63-bit overflow
      inc(p);
    until false;
  end
  else if (p[0] = '0') and
          (p[1] = 'o') then
  begin
    // octal 0o... (YAML 1.2)
    inc(p, 2);
    if p^ = #0 then
      exit;
    repeat
      if not (p^ in ['0'..'7']) then
        if p^ = #0 then
          break
        else
          exit;
      prev := u;
      u := (u shl 3) or QWord(ord(p^) - ord('0'));
      if Int64(u) < Int64(prev) then
        exit; // 63-bit overflow
      inc(p);
    until false;
  end
  else
  begin
    // decimal - 0123 is valid YAML even if not in JSON standard
    if p^ = #0 then
      exit;
    repeat
      if not (p^ in ['0'..'9']) then
        if p^ = #0 then
          break
        else
          exit;
      prev := u;
      u := u * 10 + QWord(ord(p^) - ord('0'));
      if Int64(u) < Int64(prev) then
        exit; // 63-bit overflow
      inc(p);
    until false;
  end;
  if neg then
    u := -u;
  V := u;
  result := true;
end;

function YamlLineKeyEnd(const S: RawUtf8): PtrInt;
// returns the 0-based index of the ':' that ends the key (followed by space or
// EOL), or -1 if not a mapping line; the key itself may be quoted
var
  p: PUtf8Char;
begin
  p := pointer(S);
  if p <> nil then
    repeat
      case p^ of
        #0:
          break;
        '''':
          begin
            p := GotoEndOfQuotedString(p);
            if p^ = #0 then
              break;
          end;
        '"':
          begin
            p := GotoEndOfJsonString(p);
            if p^ = #0 then
              break;
          end;
        '#':
          if (p = pointer(S)) or
             ((p - 1)^ in [' ', #9]) then
            break; // comment - no colon before it
        ':':
          if p[1] <= ' ' then
          begin
            result := p - PUtf8Char(pointer(S));
            exit;
          end;
      end;
      inc(p);
    until false;
  result := -1;
end;

function YamlLineIsDashItem(const S: RawUtf8; out afterDash: RawUtf8): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (S = '') or
     (S[1] <> '-') then
    exit;
  if length(S) <> 1 then
  begin
    i := 2;
    if S[i] > ' ' then // YAML requires '- '
      exit;
    repeat
      inc(i);
    until not (S[i] in [' ', #9]);
    afterDash := copy(S, i, MaxInt);
  end;
  result := true;
end;

function YamlDashLine(p: PUtf8Char): boolean;
  {$ifdef HASINLINE} inline; {$endif}
begin // fast check, no afterDash extraction
  result := (p <> nil) and
            (p[0] = '-') and
            (p[1] <= ' ');
end;

function YamlSpecialChars(p: PUtf8Char): boolean;
begin
  result := true;
  repeat
    case p^ of
      #0:
        break;
      #1 .. #31, // but ' ' in texts are allowed
      '"',
      '\':
        exit; // special char requires quotes
      ':':
        if p[1] in [' ' , #9] then
          exit; // avoid YAML "key: value" confusion
      '#':
        if p[-1] in [' ' , #9] then
          exit; // avoid YAML "# comment" confusion
    end;
    inc(p);
  until false;
  result := false;
end;

function YamlMultiDocument(p: PUtf8Char): boolean;
  {$ifdef HASINLINE} inline; {$endif}
var
  c: cardinal;
begin // caller should have ensured length=3
  c := PCardinal(p)^ and $ffffff;
  result := (c = ord('-') + ord('-') shl 8 + ord('-') shl 16) or
            (c = ord('.') + ord('.') shl 8 + ord('.') shl 16);
end;



{ ----- YAML -> JSON converter --------------------------------------------- }

type
  TYamlToJson = class
  protected
    fLines: TYamlLines;
    fCount: integer;
    fIdx: integer;
    fDepth, fMaxDepth: integer;
    fOut: TJsonWriter;
    procedure Error(LineIdx: integer; const Msg: ShortString);
    procedure ErrorFmt(LineIdx: integer; const Fmt: RawUtf8;
      const Args: array of const);
    procedure FillLines(const Yaml: RawUtf8);
    function SkipBlankLines: PYamlLine;
    function SkipQuoted(LineIdx: integer; p: PUtf8Char): PUtf8Char;
    function AtEnd: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    procedure CheckUnsupportedScalar(LineIdx: integer; p: PUtf8Char);
    procedure MergeMultilineQuoted(var rest: RawUtf8; firstLineIdx: integer);
    procedure FoldPlainScalar(var rest: RawUtf8; MapIndent: integer);
    procedure ParseValue(MinIndent: integer);
    procedure ParseBlockMap(Indent: integer);
    procedure ParseBlockSeq(Indent: integer);
    procedure ParseImplicitMapFromDash(MapIndent: integer;
      const firstEntry: RawUtf8; firstLineIdx: integer);
    procedure ParseFlow(var p: PUtf8Char; LineIdx: integer);
    procedure ParseFlowMap(var p: PUtf8Char; LineIdx: integer);
    procedure ParseFlowSeq(var p: PUtf8Char; LineIdx: integer);
    function ParseFlowExtract(p: PUtf8Char; EndChar: AnsiChar;
      LineIdx: integer; var Text: RawUtf8): PUtf8Char;
    procedure EmitFlowScalar(const Frag: RawUtf8; LineIdx: integer);
    procedure EmitScalarFragment(const Frag: RawUtf8; LineIdx: integer);
    procedure EmitKey(const K: RawUtf8);
    procedure EmitCollectBlockScalar(MinIndent, ExplicitIndent: integer;
      Folded, Chomp: AnsiChar);
    procedure EmitBlockScalar(const rest: RawUtf8; BaseIndent: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Run(const Yaml: RawUtf8): RawUtf8;
  end;


constructor TYamlToJson.Create;
begin
  inherited Create;
  fOut := TJsonWriter.CreateOwnedStream;
  fMaxDepth := YamlMaxDepth;
  if fMaxDepth < 4 then
    fMaxDepth := 4; // sanity floor: allow at least a few nested structures
end;

destructor TYamlToJson.Destroy;
begin
  fOut.Free;
  inherited Destroy;
end;

procedure TYamlToJson.Error(LineIdx: integer; const Msg: ShortString);
begin
  EYamlException.RaiseUtf8('YAML line %: %', [LineIdx + 1, Msg]);
end;

procedure TYamlToJson.ErrorFmt(LineIdx: integer; const Fmt: RawUtf8;
  const Args: array of const);
var
  msg: ShortString;
begin
  FormatShort(Fmt, Args, msg);
  Error(LineIdx, msg);
end;

function TYamlToJson.AtEnd: boolean;
begin
  result := fIdx >= fCount;
end;

function TYamlToJson.SkipBlankLines: PYamlLine;
begin
  result := @fLines[fIdx];
  while fIdx < fCount do
    if (result^.Content = '') or
       (result^.Content[1] = '#') then
    begin
      inc(fIdx);
      inc(result);
    end
    else
      break;
end;

function TYamlToJson.SkipQuoted(LineIdx: integer; p: PUtf8Char): PUtf8Char;
begin
  if p^ = '''' then // p^ = " or ' at calling
    result := GotoEndOfQuotedString(p)  // escaped as ''
  else
    result := GotoEndOfJsonString(p);   // escaped as \"
  if result^ <> p^ then
    ErrorFmt(LineIdx, 'missing ending % quote', [p^]);
  inc(result); // return the position after ending quote
end;

procedure TYamlToJson.CheckUnsupportedScalar(LineIdx: integer; p: PUtf8Char);
begin
  if p = nil then
    exit;
  // per YAML 1.2 §5.3, anchor/alias/tag indicators are only meaningful
  // at the start of a NODE fragment (which is what CheckUnsupportedScalar
  // is called with - a key's rest or a dash item's afterDash).
  // Mid-fragment occurrences are literal text in plain scalars. This avoids
  // firing on real-world OpenAPI descriptions: URL query "?a=1&b=2"
  // (27 hits in GitHub REST), markdown "**bold**" (182 hits) and
  // markdown inline images "![img](url)" (215 hits)
  case p^ of
    '&':
      Error(LineIdx, 'YAML anchors (&name) are not supported');
    '*':
      Error(LineIdx, 'YAML aliases (*name) are not supported');
    '!':
      Error(LineIdx, 'explicit YAML tags (!...) are not supported');
  end;
  if (PStrLen(p - _STRLEN)^ = 3) and
     YamlMultiDocument(p) then
    Error(LineIdx, 'multi-document streams are not supported');
end;

procedure TYamlToJson.EmitKey(const K: RawUtf8);
begin
  if K <> '' then
    case K[1] of
      '"':
        fOut.AddString(K);              // already JSON-escaped
      '''':
        fOut.AddQuotedStringAsJson(K);  // unquote and JSON-escape
    else
      fOut.AddJsonString(K);            // JSON-escape
    end
  else
    fOut.AddShort4(ord('"') + ord('"') shl 8, 2);
end;

procedure TYamlToJson.EmitScalarFragment(const Frag: RawUtf8; LineIdx: integer);
var
  s: RawUtf8;
  ival: Int64;
begin
  s := Frag;
  YamlStripLineComment(s);
  if (s <> '') and
     (s[1] = ' ') then
    s := TrimLeft(s);
  if s = '' then
  begin
    fOut.AddNull;
    exit;
  end;
  if s[1] in ['"', ''''] then
  begin
    EmitKey(s);
    exit;
  end;
  CheckUnsupportedScalar(LineIdx, pointer(s));
  if IsYamlNull(pointer(s)) then
    fOut.AddNull
  else if TryYamlInt(pointer(s), ival) then
    fOut.Add(ival)                          // normalized 0x 0o integers
  else if IsBooleanJson(pointer(s)) or
          IsNumberJson(pointer(s)) then
    fOut.AddString(s)                       // valid JSON number or boolean
  else
    fOut.AddJsonString(s);                  // as JSON "string"
end;

procedure TYamlToJson.EmitFlowScalar(const Frag: RawUtf8; LineIdx: integer);
var
  s: RawUtf8;
begin
  // inside flow, no trailing "# comment" stripping (flow uses , and close-brace
  // as terminators; comments after flow close belong to the line)
  s := TrimU(Frag);
  if s = '' then
    Error(LineIdx, 'empty value in flow collection');
  EmitScalarFragment(s, LineIdx);
end;

procedure TYamlToJson.EmitCollectBlockScalar(MinIndent, ExplicitIndent: integer;
  Folded, Chomp: AnsiChar);
var
  blockIndent, blankRun: integer;
  prevWasContent: boolean;
  c: PYamlLine;
  buf: TSynTempAdder;
begin
  // skip leading blank lines; stop at the first non-blank line.
  c := SkipBlankLines;
  if ExplicitIndent > 0 then
  begin
    // YAML 1.2 §8.1.1.1 explicit indent indicator: the content indent is
    // fixed at parent_indent + ExplicitIndent, NOT auto-detected. MinIndent
    // equals parent_indent + 1 at this point, so parent_indent = MinIndent-1
    blockIndent := MinIndent - 1 + ExplicitIndent;
    // if the first non-blank line is shallower than blockIndent, the block is
    // empty (blockIndent reset to -1 so the existing "empty block" path below
    // takes over)
    if (fIdx >= fCount) or
       (c^.Indent < blockIndent) then
      blockIndent := -1;
  end
  else
  begin
    // detect block indent from the first non-blank line whose indent >= MinIndent
    blockIndent := -1;
    if c^.Indent >= MinIndent then
      blockIndent := c^.Indent;
  end;
  if blockIndent < 0 then
    // empty block no chomping needed
    exit;
  buf.Init;
  try
    blankRun := 0;
    prevWasContent := false;
    while fIdx < fCount do
    begin
      if c^.Content = '' then
      begin
        inc(blankRun);
        inc(fIdx);
        inc(c);
        continue;
      end;
      if c^.Indent < blockIndent then
        break;
      // block content line: strip blockIndent prefix
      if prevWasContent then
        if blankRun = 0 then
          buf.AddDirect(folded)
        else
          // preserve blank-line runs literally as \n * blankRun
          buf.AddChars(#10, blankRun)
      else
        // leading blanks before first content: keep them
        buf.AddChars(#10, blankRun);
      buf.AddChars(' ', c^.Indent - blockIndent);
      buf.Add(c^.Content);
      prevWasContent := true;
      blankRun := 0;
      inc(fIdx);
      inc(c);
    end;
    // apply chomping to the trailing content
    case Chomp of
      '-':
        // strip all trailing newlines
        while (buf.Store.added > 0) and
              (PUtf8Char(buf.Buffer)[buf.Store.added - 1] in [#13, #10]) do
          dec(buf.Store.added);
      '+':
        // keep trailing newlines exactly; ensure at least one
        if (buf.Store.added = 0) or
           not (PUtf8Char(buf.Buffer)[buf.Store.added - 1] in [#13, #10]) then
        begin
          PUtf8Char(buf.Buffer)[buf.Store.added] := #10; // append
          inc(buf.Store.added);
        end;
      else
        // clip: collapse trailing newlines to a single one
        while (buf.Store.added >= 2) and
              (PUtf8Char(buf.Buffer)[buf.Store.added - 1] = #10) and
              (PUtf8Char(buf.Buffer)[buf.Store.added - 2] = #10) do
          dec(buf.Store.added);
        if (buf.Store.added <= 0) or
           (PUtf8Char(buf.Buffer)[buf.Store.added - 1] <> #10) then
         begin
           PUtf8Char(buf.Buffer)[buf.Store.added] := #10; // append
           inc(buf.Store.added);
         end;
    end;
    fOut.AddJsonStringBuffer(buf.Buffer, buf.Store.added);
  finally
    buf.Store.Done; // almost never any allocation
  end;
end;

procedure TYamlToJson.EmitBlockScalar(const rest: RawUtf8; BaseIndent: integer);
var
  folded, chomp: AnsiChar;
  explicitIndent: integer;
  i: PtrInt;
begin
  // rest starts with '|' or '>' optionally followed by chomp ('-' or '+')
  // and/or an explicit indent indicator (single digit 1..9, YAML 1.2 §8.1.1.1)
  if rest[1] = '>' then
    folded := ' '
  else
    folded := #10;
  chomp := ' '; // 'clip' default (no chomp indicator)
  explicitIndent := 0;
  for i := 2 to length(rest) do
    case rest[i] of
      '-',
      '+':
        chomp := rest[i];
      '0':
        // YAML 1.2 §8.1.1.1 forbids 0 as an explicit indent indicator
        Error(fIdx, 'block-scalar indent indicator must be 1..9');
      '1' .. '9':
        // first digit captured; a second digit is malformed (spec caps at 1..9)
        if explicitIndent = 0 then
          explicitIndent := ord(rest[i]) - ord('0')
        else
          Error(fIdx, 'malformed block-scalar header (double indent indicator)');
    else
      break; // #32 #9 '#' or unexpected
    end;
  inc(fIdx); // advance past the key-line marker
  EmitCollectBlockScalar(BaseIndent + 1, explicitIndent, folded, chomp);
end;

procedure TYamlToJson.MergeMultilineQuoted(var rest: RawUtf8;
  firstLineIdx: integer);
// YAML 1.2 §7.5: a " or ' scalar may span multiple physical lines.
// - double-quoted: a trailing '\' absorbs the line break and leading ws of the
//   next line (escaped line break); otherwise the line break folds to a space
// - single-quoted: '' is the only escape; line break folds to a space
// When invoked, rest must start with the opening quote and fIdx must point at
// the first line of the scalar. On return, rest contains the full quoted span
// (still wrapped in quotes, ready for EmitKey), and fIdx has been
// advanced past any consumed continuation lines. The first (key) line is NOT
// consumed here - callers are responsible for their own fIdx advance on the
// first line, just as before the merge was introduced
var
  quote: AnsiChar;
  trailingEscape: boolean;

  function ScanQuoteClosed(p: PUtf8Char; skipOpeningQuote: boolean): boolean;
  var
    esc: boolean;
  begin
    trailingEscape := false;
    result := p <> nil;
    if not result then
      exit;
    if skipOpeningQuote and
       (p^ = quote) then
      inc(p);
    esc := false;
    while p^ <> #0 do
    begin
      if quote = '"' then
      begin
        if esc then
          esc := false
        else if p^ = '\' then
          esc := true
        else if p^ = '"' then
          exit;
      end
      else
      begin
        // single-quoted: '' is an escape; otherwise '' closes
        if p^ = '''' then
          if p[1] = '''' then
            inc(p) // consume the first of the '' escape
          else
            exit;
      end;
      inc(p);
    end;
    // unterminated: remember if double-quoted ended with a trailing '\'
    trailingEscape := (quote = '"') and esc;
    result := false;
  end;

var
  cur: PRawUtf8; // we need a temp value since = rest first
  buf: TSynTempAdder;
begin
  if rest = '' then
    exit;
  quote := rest[1];
  if not (quote in ['"', '''']) then
    exit;
  if ScanQuoteClosed(pointer(rest), {skipOpeningQuote=}true) then
    exit; // single-line, already closed
  // multi-line merge
  buf.Init;
  try
    cur := @rest;
    repeat
      // absorbed line break: drop the trailing '\' and emit no separator
      buf.Add(pointer(cur^), length(cur^) - ord(trailingEscape));
      if not trailingEscape then
        // folded line break -> single space (per YAML 1.2 §7.5)
        buf.AddDirect(' ');
      inc(fIdx);
      if AtEnd then
        Error(firstLineIdx, 'unterminated multi-line quoted scalar');
      // Content is already left-trimmed by SplitYamlLines via Indent metadata,
      // which is exactly the §7.5 requirement that leading ws is ignored
      cur := @fLines[fIdx].Content;
      if cur^ = '' then
        continue;
      if ScanQuoteClosed(pointer(cur^), {skipOpeningQuote=}false) then
      begin
        // last line closes the scalar; include everything up to and past the
        // closing quote (any trailing content is ignored - quoted scalars end
        // at the closing quote)
        buf.Add(cur^);
        // leave fIdx pointing at this closing line; callers advance it
        break;
      end;
    until false;
  finally
    buf.Done(rest);
  end;
end;

procedure TYamlToJson.FoldPlainScalar(var rest: RawUtf8; MapIndent: integer);
// YAML 1.2 §6.5 plain scalar folding: a plain scalar value may continue
// across lines that are indented strictly deeper than the map key. Each
// line break folds to a single space. When invoked, fIdx must already point
// at the first potential continuation line (the caller consumed the key
// line). Advances fIdx past each folded line. A line break is NOT consumed
// when the next line opens a nested structure (dash, flow, block-scalar
// indicator, quoted scalar, or another "key: value" entry)
var
  c: PYamlLine;
begin
  c := @fLines[fIdx];
  while fIdx < fCount do
  begin
    // blank line terminates folding (paragraph break in plain scalars)
    if (c^.Content = '') or
    // sibling or outer-scope line is not part of the folded scalar
       (c^.Indent <= MapIndent) or
    // a real "- " dash item at the continuation indent starts a new seq entry
       YamlDashLine(pointer(c^.Content)) or
    // a new "key:" line (quoted or plain) is a nested map, not continuation;
    // LineKeyEnd handles both "foo:" and '"foo":' forms
       (YamlLineKeyEnd(c^.Content) >= 0) then
      exit;
    // otherwise it's plain-scalar text; fold with a single space per §6.5.
    // once we are continuing a plain scalar, indicator chars at line start
    // (", ', {, [, |, >) are just literal text - the GitHub REST spec embeds
    // markdown ("[List selected ...](https://...)") and quoted phrases (e.g.
    // "My TEam Name") mid-description
    Append(rest, ' ', c^.Content);
    inc(fIdx);
    inc(c);
  end;
end;

procedure TYamlToJson.ParseBlockMap(Indent: integer);
var
  first: boolean;
  lineIdx: integer;
  keyEnd: PtrInt;
  keyText, rest: RawUtf8;
  c: PYamlLine;
begin
  fOut.Add('{');
  first := true;
  repeat
    c := SkipBlankLines;
    if AtEnd then
      break;
    if c^.Indent <> Indent then
      break;
    keyEnd := YamlLineKeyEnd(c^.Content);
    if keyEnd < 0 then
      break;
    TrimRightCopy(c^.Content, 1, keyEnd, keyText);
    rest := '';
    if keyEnd + 1 < length(c^.Content) then
      TrimLeftCopy(c^.Content, keyEnd + 2, MaxInt, rest);
    if not first then
      fOut.AddDirect(',');
    EmitKey(keyText);
    fOut.AddDirect(':');
    lineIdx := fIdx;
    if (rest = '') or
       (rest[1] = '#') then
    begin
      inc(fIdx);
      c := SkipBlankLines;
      if (not AtEnd) and
         (c^.Indent > Indent) then
        ParseValue(c^.Indent)
      else if (not AtEnd) and
              (c^.Indent = Indent) and
              YamlDashLine(pointer(c^.Content)) then
        // YAML 1.2 compact block seq: "key:" followed by "- item" at the
        // same indent as the key (common in real-world OpenAPI specs)
        ParseBlockSeq(c^.Indent)
      else
        fOut.AddNull;
    end
    else if rest[1] in ['|', '>'] then
      EmitBlockScalar(rest, Indent)
    else
    begin
      // YAML 1.2 §7.5: a quoted scalar may span multiple lines
      if rest[1] in ['"', ''''] then
        MergeMultilineQuoted(rest, lineIdx);
      CheckUnsupportedScalar(lineIdx, pointer(rest));
      if rest[1] in ['{', '['] then
      begin
        // inline flow collection as the value - rewrite Content so ParseValue
        // sees the flow fragment alone, not the full "key: {..}"/"key: [..]"
        // line (otherwise ParseValue would dispatch back to ParseBlockMap).
        // Mirrors the same pattern in ParseImplicitMapFromDash
        c^.Content := rest;
        ParseValue(Indent);
      end
      else
      begin
        inc(fIdx); // consume the key line (or the quoted-scalar close line)
        // YAML 1.2 §6.5: plain scalar may fold across indented lines
        if not (rest[1] in ['"', '''']) then
          FoldPlainScalar(rest, Indent);
        EmitScalarFragment(rest, lineIdx);
      end;
    end;
    first := false;
  until false;
  fOut.AddDirect('}');
end;

procedure TYamlToJson.ParseBlockSeq(Indent: integer);
var
  first: boolean;
  afterDash: RawUtf8;
  lineIdx, implicitIndent: integer;
  c: PYamlLine;
begin
  fOut.Add('[');
  first := true;
  repeat
    c := SkipBlankLines;
    if AtEnd then
      break;
    if c^.Indent <> Indent then
      break;
    if not YamlLineIsDashItem(c^.Content, afterDash) then
      break;
    lineIdx := fIdx;
    if not first then
      fOut.AddDirect(',');
    first := false;
    implicitIndent := Indent + 2;
    if afterDash = '' then
    begin
      inc(fIdx);
      c := SkipBlankLines;
      if (not AtEnd) and
         (c^.Indent > Indent) then
        ParseValue(c^.Indent)
      else
        fOut.AddNull;
    end
    else if afterDash[1] in ['|', '>'] then
      EmitBlockScalar(afterDash, Indent)
    else if YamlDashLine(pointer(afterDash)) then
    begin
      // YAML 1.2 compact nested block-seq: "- - X" on one physical line.
      // The inner "- X" starts a nested seq at (outer Indent + 2); rewrite
      // the current line to look like it starts at that indent and recurse
      c^.Content := afterDash;
      c^.Indent := implicitIndent;
      ParseBlockSeq(implicitIndent);
    end
    else if afterDash[1] in ['{', '['] then
    begin
      // flow collection inline
      CheckUnsupportedScalar(lineIdx, pointer(afterDash));
      // use flow parser directly on afterDash content
      // simplest: treat the rest of the line as a flow value
      // ParseValue at current indent will see the `- ` already consumed;
      // instead, parse the flow value now by replacing the line content
      // temporarily
      c^.Content := afterDash;
      c^.Indent := implicitIndent;
      ParseValue(implicitIndent);
    end
    else if YamlLineKeyEnd(afterDash) >= 0 then
      // implicit mapping item: "- key: value" [, "   key2: v2"]
      ParseImplicitMapFromDash(implicitIndent, afterDash, lineIdx)
    else
    begin
      if afterDash[1] in ['"', ''''] then
        MergeMultilineQuoted(afterDash, lineIdx);
      CheckUnsupportedScalar(lineIdx, pointer(afterDash));
      inc(fIdx);
      if not (afterDash[1] in ['"', '''']) then
        FoldPlainScalar(afterDash, Indent);
      EmitScalarFragment(afterDash, lineIdx);
    end;
  until false;
  fOut.AddDirect(']');
end;

procedure TYamlToJson.ParseImplicitMapFromDash(MapIndent: integer;
  const firstEntry: RawUtf8; firstLineIdx: integer);
var
  keyEnd: PtrInt;
  lineContent, keyText, rest: RawUtf8;
  curIdx: integer;
  c: PYamlLine;
begin
  fOut.Add('{');
  // emit the first entry from afterDash
  keyEnd := YamlLineKeyEnd(firstEntry);
  if keyEnd < 0 then
    Error(firstLineIdx, 'missing mapping entry after "- "');
  TrimRightCopy(firstEntry, 1, keyEnd, keyText);
  rest := '';
  if keyEnd + 1 < length(firstEntry) then
    TrimLeftCopy(firstEntry, keyEnd + 2, MaxInt, rest);
  EmitKey(keyText);
  fOut.AddDirect(':');
  if (rest = '') or
     (rest[1] = '#') then
  begin
    inc(fIdx);
    c := SkipBlankLines;
    if (not AtEnd) and
       (c^.Indent > MapIndent) then
      ParseValue(c^.Indent)
    else if (not AtEnd) and
            (c^.Indent = MapIndent) and
            YamlDashLine(pointer(c^.Content)) then
      ParseBlockSeq(c^.Indent)
    else
      fOut.AddNull;
  end
  else if rest[1] in ['|', '>'] then
    // block-scalar min-indent must exceed the parent-key indent (MapIndent), so
    // the EmitBlockScalar base = MapIndent matches ParseBlockMap/continuation
    EmitBlockScalar(rest, MapIndent)
  else
  begin
    if rest[1] in ['"', ''''] then
      MergeMultilineQuoted(rest, firstLineIdx);
    CheckUnsupportedScalar(firstLineIdx, pointer(rest));
    if rest[1] in ['{', '['] then
    begin
      c := @fLines[fIdx];
      c^.Content := rest;
      c^.Indent := MapIndent;
      ParseValue(MapIndent);
    end
    else
    begin
      inc(fIdx);
      if not (rest[1] in ['"', '''']) then
        FoldPlainScalar(rest, MapIndent);
      EmitScalarFragment(rest, firstLineIdx);
    end;
  end;
  // continuation: subsequent lines at MapIndent with key-colon
  repeat
    c := SkipBlankLines;
    if AtEnd or
       (c^.Indent <> MapIndent) then
      break;
    lineContent := c^.Content;
    keyEnd := YamlLineKeyEnd(lineContent);
    if keyEnd < 0 then
      break;
    TrimRightCopy(lineContent, 1, keyEnd, keyText);
    rest := '';
    if keyEnd + 1 < length(lineContent) then
      TrimLeftCopy(lineContent, keyEnd + 2, MaxInt, rest);
    fOut.AddDirect(',');
    EmitKey(keyText);
    fOut.AddDirect(':');
    curIdx := fIdx;
    if (rest = '') or
       (rest[1] = '#') then
    begin
      inc(fIdx);
      c := SkipBlankLines;
      if (not AtEnd) and
         (c^.Indent > MapIndent) then
        ParseValue(c^.Indent)
      else if (not AtEnd) and
              (c^.Indent = MapIndent) and
              YamlDashLine(pointer(c^.Content)) then
        ParseBlockSeq(c^.Indent)
      else
        fOut.AddNull;
    end
    else if rest[1] in ['|', '>'] then
      EmitBlockScalar(rest, MapIndent)
    else
    begin
      if rest[1] in ['"', ''''] then
        MergeMultilineQuoted(rest, curIdx);
      CheckUnsupportedScalar(curIdx, pointer(rest));
      if rest[1] in ['{', '['] then
      begin
        fLines[fIdx].Content := rest;
        ParseValue(MapIndent);
      end
      else
      begin
        inc(fIdx);
        if not (rest[1] in ['"', '''']) then
          FoldPlainScalar(rest, MapIndent);
        EmitScalarFragment(rest, curIdx);
      end;
    end;
  until false;
  fOut.AddDirect('}');
end;

procedure TYamlToJson.ParseFlow(var p: PUtf8Char; LineIdx: integer);
begin
  inc(fDepth);
  try
    if fDepth > fMaxDepth then
      ErrorFmt(LineIdx,
        'YAML nesting depth exceeds YamlMaxDepth (%)', [fMaxDepth]);
    // p points at '{' or '['
    if p^ = '{' then
      ParseFlowMap(p, LineIdx)
    else if p^ = '[' then
      ParseFlowSeq(p, LineIdx)
    else
      Error(LineIdx, 'expected "{" or "[" in flow value');
  finally
    dec(fDepth);
  end;
end;

function TYamlToJson.ParseFlowExtract(p: PUtf8Char; EndChar: AnsiChar;
  LineIdx: integer; var Text: RawUtf8): PUtf8Char;
var
  depth: integer;
begin
  result := p;
  depth := 0;
  repeat
    case result^ of
      #0:
        break;
      '''',
      '"':
        result := SkipQuoted(LineIdx, result);
      '{',
      '[':
        inc(depth);
      '}':
        if depth = 0 then
          if EndChar = '}' then
            Error(LineIdx, 'unbalanced "}" in flow-seq value')
          else
            break
        else
          dec(depth);
      ']':
        if depth = 0 then
          if EndChar = ']' then
            Error(LineIdx, 'unbalanced "]" in flow-seq value')
          else
            break
        else
          dec(depth);
      ',':
        if depth = 0 then
          break;
    end;
    inc(result);
  until false;
  FastSetString(Text, p, result);
end;

procedure TYamlToJson.ParseFlowMap(var p: PUtf8Char; LineIdx: integer);
var
  first: boolean;
  keyStart, keyEnd: PUtf8Char;
  keyText, valText: RawUtf8;
begin
  if p^ <> '{' then
    Error(LineIdx, 'expected "{"');
  inc(p);
  fOut.Add('{');
  first := true;
  while p^ <> #0 do
  begin
    // skip whitespace
    while p^ in [' ', #9] do
      inc(p);
    if p^ = #0 then
      Error(LineIdx, 'unterminated flow mapping');
    if p^ = '}' then
    begin
      inc(p);
      fOut.AddDirect('}');
      exit;
    end;
    if (p^ = ',') and
       not first then
    begin
      inc(p);
      continue;
    end;
    // read key: unquoted, quoted, or flow-collection key (rare)
    keyStart := p;
    if p^ in ['"', ''''] then
    begin
      p := SkipQuoted(LineIdx, p);
      keyEnd := p;
      while p^ in [' ', #9] do
        inc(p);
      if p^ <> ':' then
        Error(LineIdx, 'expected ":" after flow key');
      inc(p);
    end
    else
    begin
      while not (p^ in [':', ',', '}', #0, #10]) do
        inc(p);
      if p^ <> ':' then
        Error(LineIdx, 'expected ":" after flow key');
      keyEnd := p;
      inc(p);
    end;
    FastSetString(keyText, keyStart, keyEnd);
    TrimSelf(keyText);
    CheckUnsupportedScalar(LineIdx, pointer(keyText));
    if not first then
      fOut.Add(',');
    EmitKey(keyText);
    fOut.AddDirect(':');
    first := false;
    // skip whitespace before value
    while p^ in [' ', #9] do
      inc(p);
    if p^ = #0 then
      Error(LineIdx, 'unterminated flow mapping (expected value)');
    if p^ in ['{', '['] then
      ParseFlow(p, LineIdx)
    else
    begin
      p := ParseFlowExtract(p, ']', LineIdx, valText);
      EmitFlowScalar(valText, LineIdx);
    end;
  end;
  Error(LineIdx, 'unterminated flow mapping');
end;

procedure TYamlToJson.ParseFlowSeq(var p: PUtf8Char; LineIdx: integer);
var
  first: boolean;
  valText: RawUtf8;
begin
  if p^ <> '[' then
    Error(LineIdx, 'expected "["');
  inc(p);
  fOut.Add('[');
  first := true;
  while p^ <> #0 do
  begin
    while p^ in [' ', #9] do
      inc(p);
    if p^ = #0 then
      Error(LineIdx, 'unterminated flow sequence');
    if p^ = ']' then
    begin
      inc(p);
      fOut.AddDirect(']');
      exit;
    end;
    if (p^ = ',') and
       not first then
    begin
      inc(p);
      continue;
    end;
    if p^ in ['{', '['] then
    begin
      if not first then
        fOut.AddDirect(',');
      ParseFlow(p, LineIdx);
      first := false;
      continue;
    end;
    p := ParseFlowExtract(p, '}', LineIdx, valText);
    if not first then
      fOut.AddDirect(',');
    EmitFlowScalar(valText, LineIdx);
    first := false;
  end;
  Error(LineIdx, 'unterminated flow sequence');
end;

procedure TYamlToJson.ParseValue(MinIndent: integer);
var
  p: PUtf8Char;
  lineIdx: integer;
  c: PYamlLine;
begin
  inc(fDepth);
  try
    if fDepth > fMaxDepth then
      ErrorFmt(fIdx,
        'YAML nesting depth exceeds YamlMaxDepth (%)', [fMaxDepth]);
    c := SkipBlankLines;
    if AtEnd then
    begin
      fOut.AddNull;
      exit;
    end;
    if c^.Indent < MinIndent then
    begin
      fOut.AddNull;
      exit;
    end;
    lineIdx := fIdx;
    CheckUnsupportedScalar(lineIdx, pointer(c^.Content));
    if c^.Content[1] in ['{', '['] then
    begin
      // single-line flow at this indent
      p := pointer(c^.Content);
      ParseFlow(p, lineIdx);
      inc(fIdx);
    end
    else if YamlDashLine(pointer(c^.Content)) then
      ParseBlockSeq(c^.Indent)
    else if YamlLineKeyEnd(c^.Content) >= 0 then
      ParseBlockMap(c^.Indent)
    else
    begin
      // plain top-level scalar line
      if c^.Content[1] in ['|', '>'] then
        EmitBlockScalar(c^.Content, fLines[fIdx].Indent)
      else if c^.Content[1] in ['"', ''''] then
      begin
        inc(fIdx);
        MergeMultilineQuoted(c^.Content, lineIdx); // merge in-place
        EmitScalarFragment(c^.Content, lineIdx);
      end
      else
      begin
        EmitScalarFragment(c^.Content, lineIdx);
        inc(fIdx);
      end;
    end;
  finally
    dec(fDepth);
  end;
end;

// split Yaml into fLines[] with indent metadata
// - accepts both LF and CRLF terminators; trailing \r is stripped
procedure TYamlToJson.FillLines(const Yaml: RawUtf8);
var
  lineStart, lineEnd, lineNext: PUtf8Char;
  len, indent: PtrInt;
  c: PYamlLine;
begin
  fCount := 0;
  fLines := nil;
  c := nil; // for Delphi compiler
  lineStart := pointer(Yaml);
  if lineStart = nil then
    exit;
  lineEnd := lineStart + length(Yaml);
  repeat
    // detect line ending (using SSE2 asm if available)
    len := BufferLineLength(lineStart, lineEnd); // both CRLF or LF
    lineNext := lineStart + len;
    // prepare a new entry in fLines[]
    if fCount >= length(fLines) then
    begin
      SetLength(fLines, NextGrow(fCount));
      c := @fLines[fCount];
    end
    else
      inc(c);
    // compute indentation count
    indent := 0; // direct c^.Indent use trigger invalid code on FPC arm32
    while lineStart[indent] = ' ' do
      inc(indent);
    inc(lineStart, indent); // trim left
    dec(len, indent);
    c^.Indent := indent;
    // upfront scan: reject multi-doc separators and YAML directives
    if lineStart^ = #9 then
      Error(fCount, 'tab characters are not allowed for indentation')
    else if c^.Indent = 0 then
      // per YAML 1.2 spec, --- and ... markers are only structural when at
      // col 0; inside indented content (block scalars, etc.) they're literal
      if lineStart^ = '%' then
        Error(fCount, 'YAML directives (%YAML / %TAG ...) are not supported')
      else if len = 3 then
        if YamlMultiDocument(lineStart) then
           if fCount = 0 then
             len := 0 // tolerate a single leading "---" marker
           else
             Error(fCount, 'multi-document streams are not supported');
    // store trimmed content
    while (len > 0) and
          (lineStart[len - 1] = ' ') do
      dec(len); // trim right
    FastSetString(c^.Content, lineStart, len);
    inc(fCount);
    // ignore a single CRLF/LF and go to next line
    lineStart := lineNext;
    if lineStart^ = #13 then
      inc(lineStart);
    if lineStart^ = #10 then
      inc(lineStart);
  until lineStart^ = #0;
  if length(fLines) <> fCount then
    DynArrayFakeLength(fLines, fCount);
end;

function TYamlToJson.Run(const Yaml: RawUtf8): RawUtf8;
var
  c: PYamlLine;
begin
  FillLines(Yaml);
  fIdx := 0;
  c := SkipBlankLines;
  if AtEnd then
  begin
    result := '{}';
    exit;
  end;
  ParseValue(c^.Indent);
  // after parsing the top-level value, any non-blank line with indent <=
  // topIndent that was not consumed indicates an inconsistent-indent error
  SkipBlankLines;
  if not AtEnd then
    ErrorFmt(fIdx, 'unexpected line at this indentation (idx=% count=%)',
      [fIdx, fCount]);
  fOut.SetText(result);
end;

function IsYamlFileName(const FileName: TFileName): boolean;
begin
  result := SameExt(FileName, ['yaml', 'yml'], {withoutdot=}true) >= 0;
end;

function YamlToJson(const Yaml: RawUtf8): RawUtf8;
var
  conv: TYamlToJson;
begin
  conv := TYamlToJson.Create;
  try
    result := conv.Run(Yaml);
  finally
    conv.Free;
  end;
end;

function TryYamlToJson(const Yaml: RawUtf8; out Json: RawUtf8): boolean;
begin
  try
    Json := YamlToJson(Yaml); // may raise EYamlException
    result := true;
  except
    result := false;
  end;
end;

function JsonToYaml(const Json: RawUtf8; Options: TYamlWriterOptions;
  DocOptions: TDocVariantOptions): RawUtf8;
var
  doc: TDocVariantData;
begin
  if doc.InitJson(Json, DocOptions) then
    result := VariantToYaml(variant(doc), Options)
  else
    FastAssignNew(result);
end;

procedure YamlToVariant(const Yaml: RawUtf8; out Doc: TDocVariantData;
  Options: TDocVariantOptions);
var
  json: RawUtf8; // in two steps for FPC arm32
begin
  json := YamlToJson(Yaml);
  if Doc.InitJsonInPlace(pointer(json), Options) = nil then
    EYamlException.RaiseU('YamlToVariant: JSON output error');
end;

function TryYamlToVariant(const Yaml: RawUtf8;
  out Doc: TDocVariantData; Options: TDocVariantOptions): boolean;
// convenience alias preserving mormot.net.openapi's exact options set
begin
  try
    YamlToVariant(Yaml, Doc, Options);
    result := true;
  except
    result := false;
  end;
end;

function TryYamlFileToVariant(const FileName: TFileName; out Doc: TDocVariantData;
  Options: TDocVariantOptions): boolean;
var
  yaml: RawUtf8;
begin
  yaml := RawUtf8FromFile(FileName); // BOM stripping
  if yaml = '' then
    EYamlException.RaiseUtf8('TryYamlFileToVariant: file not found: %',
      [FileName]);
  result := TryYamlToVariant(yaml, Doc, Options);
end;


{ ----- TDocVariant -> YAML writer ----------------------------------------- }

type
  TVariantToYaml = class
  private
    fOut: TJsonWriter;
    fOptions: TYamlWriterOptions; // not used yet
    procedure WriteVarData(vd: PVarData; Indent: PtrInt);
    procedure WriteBlockMap(const dv: TDocVariantData; Indent: PtrInt);
    procedure WriteBlockSeq(const dv: TDocVariantData; Indent: PtrInt);
    procedure WriteKey(const K: RawUtf8);
      {$ifdef HASINLINE} inline; {$endif}
    procedure WriteIndent(N: PtrInt);
      {$ifdef HASINLINE} inline; {$endif}
  public
    constructor Create(Options: TYamlWriterOptions);
    destructor Destroy; override;
    function Run(const Doc: variant): RawUtf8;
  end;

constructor TVariantToYaml.Create(Options: TYamlWriterOptions);
begin
  inherited Create;
  fOut := TJsonWriter.CreateOwnedStream;
  fOptions := Options;
end;

destructor TVariantToYaml.Destroy;
begin
  fOut.Free;
  inherited Destroy;
end;

procedure TVariantToYaml.WriteIndent(N: PtrInt);
begin
  fOut.AddChars(' ', N);
end;

procedure TVariantToYaml.WriteKey(const K: RawUtf8);
begin
  WriteYamlString(fOut, pointer(K), length(K));
end;

procedure TVariantToYaml.WriteBlockMap(const dv: TDocVariantData; Indent: PtrInt);
var
  i: PtrInt;
  v: PVariant;
  cd: PDocVariantData;
begin
  if dv.Count = 0 then
  begin
    fOut.AddShort4(ord('{') + ord('}') shl 8, 2);
    exit;
  end;
  i := 0;
  v := pointer(dv.Values);
  repeat
    WriteKey(dv.Names[i]);
    fOut.AddDirect(':');
    if _Safe(v^, cd) and
       (cd^.Count > 0) then
    begin
      fOut.Add(#10);
      WriteIndent(Indent + 2);
      WriteVarData(PVarData(cd), Indent + 2);
    end
    else
    begin
      fOut.Add(' ');
      WriteVarData(PVarData(v), Indent + 2);
    end;
    inc(i);
    if i = dv.Count then
      break;
    inc(v);
    fOut.AddDirect(#10);
    WriteIndent(Indent);
  until false;
end;

procedure TVariantToYaml.WriteBlockSeq(const dv: TDocVariantData; Indent: PtrInt);
var
  i: PtrInt;
  v: PVariant;
  cd: PDocVariantData;
begin
  if dv.Count = 0 then
  begin
    fOut.AddShort4(ord('[') + ord(']') shl 8, 2);
    exit;
  end;
  i := 0;
  v := pointer(dv.Values);
  repeat
    fOut.AddShort4(ord('-') + ord(' ') shl 8, 2);
    if _Safe(v^, cd) and
       (cd^.Count > 0) then
    begin
      // put child map/seq inline after the dash
      if cd^.Kind = dvObject then
        WriteBlockMap(cd^, Indent + 2)
      else
      begin
        fOut.AddDirect(#10);
        WriteIndent(Indent + 2);
        WriteBlockSeq(cd^, Indent + 2);
      end;
    end
    else
      WriteVarData(PVarData(v), Indent + 2);
    inc(i);
    if i = dv.Count then
      break;
    inc(v);
    fOut.AddDirect(#10);
    WriteIndent(Indent);
  until false;
end;

procedure TVariantToYaml.WriteVarData(vd: PVarData; Indent: PtrInt);
var
  vt: cardinal;
begin
  repeat
    vt := vd^.VType;
    if vt <> varVariantByRef then
      break;
    vd := vd^.VPointer;
  until false;
  if (vt <= varOleUInt) and
     (vt <> varOleStr) then
    // simple and numeric types share the same text form in JSON and YAML, so
    // let TJsonWriter.AddVariant emit them directly without any escaping
    fOut.AddVariant(PVariant(vd)^, twNone)
  else if vt = varString then
    // in a TDocVariant, varString are usually normalized as RawUtf8
    WriteYamlString(fOut, vd^.VAny, length(RawUtf8(vd^.VAny)))
  else if vt = DocVariantVType then
    // nested object or array
    if PDocVariantData(vd)^.IsObject then
      WriteBlockMap(PDocVariantData(vd)^, Indent)
    else if PDocVariantData(vd)^.IsArray then
      WriteBlockSeq(PDocVariantData(vd)^, Indent)
    else
      fOut.AddNull
  else
    // string-like or unknown: coerce to UTF-8 and apply YAML quoting rules
    WriteYamlVariant(fOut, PVariant(vd)^);
end;

function TVariantToYaml.Run(const Doc: variant): RawUtf8;
begin
  WriteVarData(@Doc, {indent=}0);
  fOut.AddDirect(#10);
  fOut.SetText(result);
end;


function VariantToYaml(const Doc: variant; Options: TYamlWriterOptions): RawUtf8;
var
  conv: TVariantToYaml;
begin
  conv := TVariantToYaml.Create(Options);
  try
    result := conv.Run(Doc);
  finally
    conv.Free;
  end;
end;

procedure SaveVariantToYamlFile(const Doc: variant; const FileName: TFileName;
  Options: TYamlWriterOptions);
begin
  FileFromString(VariantToYaml(Doc, Options), FileName);
end;

procedure WriteYamlString(W: TJsonWriter; P: PUtf8Char; L: PtrInt);
begin
  if L <= 0 then
    W.AddShort4(ord('"') + ord('"') shl 8, 2)
  else if
      // leading chars that need quoting
      (P^ in [' ', #9, '!', '&', '*', '>', '|', '%', '@', '`', '"', '''',
              '#', '-', '?', ':', '{', '[', '}', ']', ',']) or
      // trailing whitespace triggers quotes
      (P[L - 1] in [' ', #9]) or
      // reserved plain-scalar forms: null, bool, numbers - must be quoted
      // to preserve string type on round-trip
      IsYamlNull(P) or
      IsBooleanJson(P) or
      IsNumberJson(P) or
      // quick scan for chars that require escape
      YamlSpecialChars(P) then
    // emit as JSON-escaped double-quoted string - valid YAML 1.2 §5.7
    // since the JSON escape set is a subset of YAML's flow-scalar escapes
    W.AddJsonStringBuffer(P, L)
  else
    // we can directly output this text without quotes
    W.AddNoJsonEscape(P, L);
end;

procedure WriteYamlVariant(W: TJsonWriter; const V: variant);
var
  tmp: TTempUtf8;
begin
  if VariantToTempUtf8(V, tmp) then
    // wasString: apply YAML quoting rules
    WriteYamlString(W, tmp.Text, tmp.Len)
  else
    // direct output of e.g. BSON content as JSON
    W.AddNoJsonEscape(tmp.Text, tmp.Len);
  TempUtf8Done(tmp);
end;


{ ************* Markup (e.g. Markdown or Emoji) process }

{ internal TTextWriterEscape class }

type
  TTextWriterEscapeStyle = (
    tweBold,
    tweItalic,
    tweCode);

  TTextWriterEscapeLineStyle = (
    twlNone,
    twlParagraph,
    twlOrderedList,
    twlUnorderedList,
    twlBlockquote,
    twlCode4,
    twlCode3);

  {$ifdef USERECORDWITHMETHODS}
  TTextWriterEscape = record
  {$else}
  TTextWriterEscape = object
  {$endif USERECORDWITHMETHODS}
  public
    P, B, P2, B2: PUtf8Char;
    W: TTextWriter;
    st: set of TTextWriterEscapeStyle;
    fmt: TTextWriterHtmlFormat;
    esc: TTextWriterHtmlEscape;
    lst: TTextWriterEscapeLineStyle;
    procedure Start(dest: TTextWriter; src: PUtf8Char; escape: TTextWriterHtmlEscape);
    function ProcessText(const stopchars: TSynByteSet): AnsiChar;
    procedure ProcessHRef;
    function ProcessLink: boolean;
    procedure ProcessEmoji;
      {$ifdef HASINLINE}inline;{$endif}
    procedure Toggle(style: TTextWriterEscapeStyle);
    procedure SetLine(style: TTextWriterEscapeLineStyle);
    procedure EndOfParagraph;
    procedure NewMarkdownLine;
    procedure AddHtmlEscapeWiki(dest: TTextWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
    procedure AddHtmlEscapeMarkdown(dest: TTextWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
  end;

procedure TTextWriterEscape.Start(dest: TTextWriter; src: PUtf8Char;
  escape: TTextWriterHtmlEscape);
begin
  P := src;
  W := dest;
  st := [];
  if heHtmlEscape in escape then
    fmt := hfOutsideAttributes
  else
    fmt := hfNone;
  esc := escape;
  lst := twlNone;
end;

function IsHttpOrHttps(P: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := (PCardinal(P)^ = HTTP__32) and
            ((PCardinal(P + 4)^ and $ffffff = HTTP__24) or
             (PCardinal(P + 4)^ =
             ord('s') + ord(':') shl 8 + ord('/') shl 16 + ord('/') shl 24));
end;

function TTextWriterEscape.ProcessText(const stopchars: TSynByteSet): AnsiChar;
begin
  if P = nil then
  begin
    result := #0;
    exit;
  end;
  B := P;
  while not (ord(P^) in stopchars) and
        not IsHttpOrHttps(P) do
    inc(P);
  W.AddHtmlEscape(B, P - B, fmt);
  result := P^;
end;

procedure TTextWriterEscape.ProcessHRef;
begin
  B := P;
  while P^ > ' ' do
    inc(P);
  W.AddShort('<a href="');
  W.AddHtmlEscape(B, P - B, hfWithinAttributes);
  W.AddShort('" rel="nofollow">');
  W.AddHtmlEscape(B, P - B);
  W.AddDirect('<', '/', 'a', '>');
end;

function TTextWriterEscape.ProcessLink: boolean;
begin
  inc(P);
  B2 := P;
  while not (P^ in [#0, ']']) do
    inc(P);
  P2 := P;
  if PWord(P)^ = ord(']') + ord('(') shl 8 then
  begin
    inc(P, 2);
    B := P;
    while not (P^ in [#0, ')']) do
      inc(P);
    if P^ = ')' then
    begin
      // [GitHub](https://github.com)
      result := true;
      exit;
    end;
  end;
  P := B2; // rollback
  result := false;
end;

procedure TTextWriterEscape.ProcessEmoji;
begin
  if heEmojiToUtf8 in esc then
    EmojiParseDots(P, W)
  else
  begin
    W.Add(':');
    inc(P);
  end;
end;

procedure TTextWriterEscape.Toggle(style: TTextWriterEscapeStyle);
const
  HTML: array[tweBold..tweCode] of TShort7 = (
    'strong>', 'em>', 'code>');
begin
  W.Add('<');
  if style in st then
  begin
    W.Add('/');
    exclude(st, style);
  end
  else
    include(st, style);
  W.AddShorter(HTML[style]);
end;

procedure TTextWriterEscape.EndOfParagraph;
begin
  if tweBold in st then
    Toggle(tweBold);
  if tweItalic in st then
    Toggle(tweItalic);
  if P <> nil then
    if PWord(P)^ = EOLW then
      inc(P, 2)
    else
      inc(P);
end;

procedure TTextWriterEscape.SetLine(style: TTextWriterEscapeLineStyle);
const
  HTML: array[twlParagraph..twlCode3] of TShort7 = (
    'p>', 'li>', 'li>', 'p>', 'code>', 'code>');
  HTML2: array[twlOrderedList..twlCode3] of TShort15 = (
    'ol>', 'ul>', 'blockquote>', 'pre>', 'pre>');
begin
  if lst >= low(HTML) then
  begin
    if (lst < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShorter(HTML[lst]);
    end;
    if (lst >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShort(HTML2[lst]);
    end;
  end;
  if style >= low(HTML) then
  begin
    if (style >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShort(HTML2[style]);
    end;
    if (style < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShorter(HTML[style]);
    end;
  end;
  lst := style;
end;

procedure TTextWriterEscape.NewMarkdownLine;
label
  none;
var
  c: cardinal;
begin
  if P = nil then
    exit;
  c := PCardinal(P)^;
  if c and $ffffff = ord('`') + ord('`') shl 8 + ord('`') shl 16 then
  begin
    inc(P, 3);
    if lst = twlCode3 then
    begin
      lst := twlCode4; // to close </code></pre>
      NewMarkdownLine;
      exit;
    end;
    SetLine(twlCode3);
  end;
  if lst = twlCode3 then
    exit; // no prefix process within ``` code blocks
  if c = $20202020 then
  begin
    SetLine(twlCode4);
    inc(P, 4);
    exit;
  end;
  P := GotoNextNotSpaceSameLine(P); // don't implement nested levels yet
  case P^ of
    '*',
    '+',
    '-':
      if P[1] = ' ' then
        SetLine(twlUnorderedList)
      else
        goto none;
    '1'..'9':
      begin
        // first should be 1. then any ##. number to continue
        B := P;
        repeat
          inc(P)
        until not (P^ in ['0'..'9']);
        if (P^ = '.') and
           ((lst = twlOrderedList) or
            (PWord(B)^ = ord('1') + ord('.') shl 8)) then
          SetLine(twlOrderedList)
        else
        begin
          P := B;
none:     if lst = twlParagraph then
          begin
            c := PWord(P)^; // detect blank line to separate paragraphs
            if c = EOLW then
              inc(P, 2)
            else if c and $ff = $0a then
              inc(P)
            else
            begin
              W.AddOnce(' ');
              exit;
            end;
          end;
          SetLine(twlParagraph);
          exit;
        end;
      end;
    '>':
      if P[1] = ' ' then
        SetLine(twlBlockquote)
      else
        goto none;
  else
    goto none;
  end;
  P := GotoNextNotSpaceSameLine(P + 1);
end;

procedure TTextWriterEscape.AddHtmlEscapeWiki(dest: TTextWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  SetLine(twlParagraph);
  repeat
    case ProcessText([0, 10, 13,
                      ord('*'), ord('+'), ord('`'), ord('\'), ord(':')]) of
      #0:
        break;
      #10,
      #13:
        begin
          EndOfParagraph;
          SetLine(twlParagraph);
          continue;
        end;
      '\':
        if P[1] in ['\', '`', '*', '+'] then
        begin
          inc(P);
          W.Add(P^);
        end
        else
          W.Add('\');
      '*':
        Toggle(tweItalic);
      '+':
        Toggle(tweBold);
      '`':
        Toggle(tweCode);
      'h':
        begin
          ProcessHRef;
          continue;
        end;
      ':':
        begin
          ProcessEmoji;
          continue;
        end;
    end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

procedure TTextWriterEscape.AddHtmlEscapeMarkdown(dest: TTextWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  NewMarkDownLine;
  repeat
    if lst >= twlCode4 then // no Markdown tags within code blocks
      if ProcessText([0, 10, 13]) = #0 then
        break
      else
      begin
        if PWord(P)^ = EOLW then
          inc(P, 2)
        else
          inc(P);
        W.AddCR; // keep LF within <pre>
        NewMarkdownLine;
        continue;
      end
    else
      case ProcessText([0, 10, 13, ord('*'), ord('_'), ord('`'),
                        ord('\'), ord('['), ord('!'), ord(':')]) of
        #0:
          break;
        #10,
        #13:
          begin
            EndOfParagraph;
            NewMarkdownLine;
            continue;
          end;
        '\':
          if P[1] in ['\', '`', '*', '_', '[', ']', '{', '}',
                      '(', ')', '#', '+', '-', '.', '!'] then
          begin
            // backslash escape
            inc(P);
            W.Add(P^);
          end
          else
            W.Add('\');
        '*',
        '_':
          if P[1] = P[0] then
          begin
            // **This text will be bold** or __This text will be bold__
            inc(P);
            Toggle(tweBold);
          end
          else
            // *This text will be italic* or _This text will be italic_
            Toggle(tweItalic);
        '`':
          // `This text will be code`
          Toggle(tweCode);
        '[':
          if ProcessLink then
          begin
            // [GitHub](https://github.com)
            W.AddShort('<a href="');
            W.AddHtmlEscape(B, P - B, hfWithinAttributes);
            if IsHttpOrHttps(B) then
              W.AddShort('" rel="nofollow">')
            else
              W.Add('"', '>');
            W.AddHtmlEscape(B2, P2 - B2, fmt);
            W.AddDirect('<', '/', 'a', '>'); // no inc(P) needed here
          end
          else
            // not a true link -> just append
            W.Add('[');
        '!':
          begin
            if P[1] = '[' then
            begin
              inc(P);
              if ProcessLink then
              begin
                W.AddShort('<img alt="');
                W.AddHtmlEscape(B2, P2 - B2, hfWithinAttributes);
                W.AddShorter('" src="');
                W.AddNoJsonEscape(B, P - B);
                W.AddDirect('"', '>');
                inc(P);
                continue;
              end;
              dec(P);
            end;
            W.Add('!'); // not a true image
          end;
        'h':
          begin
            ProcessHRef;
            continue;
          end;
        ':':
          begin
            ProcessEmoji;
            continue;
          end;
      end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

function HtmlEscapeWiki(const wiki: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
  W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeWiki(W, pointer(wiki), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlEscapeMarkdown(const md: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeMarkdown(W, pointer(md), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeWiki(W, P, esc);
end;

procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeMarkdown(W, P, esc);
end;

var
  _EMOJISET: boolean;
  _EMOJI_UTF8: array[TEmoji] of TStrRecConst;

procedure EmojiInit;
var
  e: TEmoji;
begin
  if _EMOJISET then
    exit;
  GlobalLock;
  if not _EMOJISET then
  begin
    // Emoji Efficient Parsing
    Assert(ord(high(TEmoji)) = $4f + 1);
    EMOJI_RTTI := GetEnumName(TypeInfo(TEmoji), 1); // ignore eNone=0
    GetEnumTrimmedNames(TypeInfo(TEmoji), @EMOJI_TEXT, scLowerCase);
    FastAssignNew(EMOJI_TEXT[eNone]);
    for e := succ(low(e)) to high(e) do
    begin
      Join([':', EMOJI_TEXT[e], ':'], EMOJI_TAG[e]);
      // order matches U+1F600 to U+1F64F codepoints
      Ucs4ToUtf8(ord(e) + $1f5ff, FastSetConst(EMOJI_UTF8[e], _EMOJI_UTF8[e], nil, 4));
    end;
    EMOJI_AFTERDOTS[')'] := eSmiley;
    EMOJI_AFTERDOTS['('] := eFrowning;
    EMOJI_AFTERDOTS['|'] := eExpressionless;
    EMOJI_AFTERDOTS['/'] := eConfused;
    EMOJI_AFTERDOTS['D'] := eLaughing;
    EMOJI_AFTERDOTS['o'] := eOpen_mouth;
    EMOJI_AFTERDOTS['O'] := eOpen_mouth;
    EMOJI_AFTERDOTS['p'] := eYum;
    EMOJI_AFTERDOTS['P'] := eYum;
    EMOJI_AFTERDOTS['s'] := eScream;
    EMOJI_AFTERDOTS['S'] := eScream;
    _EMOJISET := true;
  end;
  GlobalUnLock;
end;

function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
begin
  // RTTI has shortstrings in adjacent L1 cache lines -> faster than EMOJI_TEXT[]
  result := TEmoji(FindShortStringListTrimLowerCase(
                     EMOJI_RTTI, ord(high(TEmoji)) - 1, P, len) + 1);
  // note: we may enhance performance by using FastFindPUtf8CharSorted()
end;

function EmojiParseDots(var P: PUtf8Char; W: TTextWriter): TEmoji;
var
  c: PUtf8Char;
begin
  if not _EMOJISET then
    EmojiInit;
  result := eNone;
  inc(P); // ignore trailing ':'
  c := P;
  if c[-2] <= ' ' then
  begin
    if (c[1] <= ' ') and
       (c^ in ['('..'|']) then
      result := EMOJI_AFTERDOTS[c^]; // e.g. :)
    if result = eNone then
    begin
      while c^ in ['a'..'z', 'A'..'Z', '_'] do
        inc(c);
      if (c^ = ':') and
         (c[1] <= ' ') then // try e.g. :joy_cat:
        result := EmojiFromText(P, c - P);
    end;
    if result <> eNone then
    begin
      P := c + 1; // continue parsing after the Emoji text
      if W <> nil then
        W.AddShort4(_EMOJI_UTF8[result].TextLo);
      exit;
    end;
  end;
  if W <> nil then
    W.Add(':');
end;

procedure EmojiToDots(P: PUtf8Char; W: TTextWriter);
var
  B: PUtf8Char;
  c: cardinal;
begin
  if not _EMOJISET then
    EmojiInit;
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while (P^ <> #0) and
            (PWord(P)^ <> $9ff0) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      B := P;
      c := NextUtf8Ucs4(P) - $1f5ff;
      if c <= cardinal(high(TEmoji)) then
        W.AddString(EMOJI_TAG[TEmoji(c)])
      else
        W.AddNoJsonEscape(B, P - B);
    until P^ = #0;
end;

function EmojiToDots(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  if PosExChar(#$f0, text) = 0 then
  begin
    result := text; // no UTF-8 smiley for sure
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiToDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure EmojiFromDots(P: PUtf8Char; W: TTextWriter);
var
  B: PUtf8Char;
begin
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while not (P^ in [#0, ':']) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      EmojiParseDots(P, W);
    until P^ = #0;
end;

function EmojiFromDots(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiFromDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ ************ INI Files In-memory Access }

function IdemPChar2(table: PNormTable; p: PUtf8Char; up: PAnsiChar): boolean;
  {$ifdef HASINLINE}inline;{$endif}
var
  u: AnsiChar;
begin
  // here p and up are expected to be <> nil
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

function FindSectionFirstLine(var source: PUtf8Char; search: PAnsiChar;
  sourceend: PPUtf8Char): boolean;
var
  p: PUtf8Char;
  table: PNormTable;
  charset: PTextCharSet;
begin
  result := false;
  p := source;
  if (p = nil) or
     (search = nil) then
    exit;
  table := @NormToUpperAnsi7;
  charset := @TEXT_CHARS;
  repeat
    if p^ = '[' then
    begin
      inc(p);
      result := IdemPChar2(table, p, search);
    end;
    while tcNot01013 in charset[p^] do
      inc(p);
    while tc1013 in charset[p^] do
      inc(p);
    if result then
    begin
      source := p;
      if sourceend <> nil then
      begin
        repeat
          while tcNot01013 in charset[p^] do
            inc(p);
          while tc1013 in charset[p^] do
            inc(p);
        until p^ in [#0, '['];
        sourceend^ := p;
      end;
      exit;
    end;
  until p^ = #0;
  source := nil;
end;

function FindSectionFirstLineW(var source: PWideChar; search: PUtf8Char): boolean;
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

function FindIniNameValueU(const Content: RawUtf8; UpperName: PAnsiChar): RawUtf8;
var
  p: PUtf8Char absolute Content;
begin
  result := FindIniNameValue(p, UpperName, '', p + length(Content));
end;

function FindIniNameValueP(P, PEnd: PUtf8Char; UpperName: PAnsiChar;
  out Value: PUtf8Char): PtrInt;
var // note: won't use "out Len: PtrInt" which is confusing with integer vars
  u, PBeg: PUtf8Char;
  first: AnsiChar;
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7;
  {$else}
  table: PNormTable;
  {$endif CPUX86NOTPIC}
label
  fnd;
begin // expects UpperName as 'NAME=' and P^ at the beginning of section content
  u := P;
  if (u <> nil) and
     (u^ <> '[') and
     (UpperName <> nil) then
  begin
    first := UpperName[0];
    {$ifndef CPUX86NOTPIC}
    table := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    PBeg := nil;
    repeat
      while u^ in [#9, ' '] do
        inc(u); // trim left ' '
      if u^ = #0 then
        break;
      if table[u^] = first then
        PBeg := u; // check for UpperName=... line below - ignore ; comment
      {$ifdef ASMX64}
      if PEnd <> nil then
        inc(u, BufferLineLength(u, PEnd)) // we can use SSE2
      else
      {$endif ASMX64}
        while true do
          if u^ > #13 then
            inc(u)
          else if u^ in [#0, #10, #13] then
            break
          else
            inc(u);
      if PBeg <> nil then
      begin
        inc(PBeg);
        P := u;
        u := pointer(UpperName + 1);
        repeat
          if u^ <> #0 then
            if table[PBeg^] = u^ then
            begin
              inc(u);
              inc(PBeg);
            end
            else
            begin
              if u^ <> '=' then
                break;
              if PBeg^ <> ' ' then
                if PBeg^ = ':' then // allow ':' within INI (as WinAPI)
                  goto fnd
                else
                  break;
              repeat // ignore spaces/tabs around the '=' or ':' separator
                inc(PBeg);
                case PBeg^ of
                  #9, ' ':
                    continue;
                  '=', ':':
                    goto fnd;
                else
                  break;
                end;
              until false;
              break;
            end
          else
          begin
            if PBeg^ in [#9, ' '] then
              repeat
fnd:            inc(PBeg); // should ignore spaces/tabs after the '=' sign
              until not (PBeg^ in [#9, ' ']);
            result := P - PBeg;
            while (result > 0) and
                  (PBeg[result - 1] in [#9, ' ']) do
              dec(result); // should trim spaces/tabs at the end of the line
            Value := PBeg; // <> nil but maybe with result=len=0
            exit;
          end;
        until false;
        PBeg := nil;
        u := P;
      end;
      while u^ in [#10, #13] do
        inc(u);
    until u^ in [#0, '['];
  end;
  Value := nil; // not found
  result := 0;  // value length
end;

function FindIniNameValue(P: PUtf8Char; UpperName: PAnsiChar;
  const DefaultValue: RawUtf8; PEnd: PUtf8Char): RawUtf8;
var
  v: PUtf8Char;
  l: PtrInt;
begin // expects UpperName as 'NAME=' and P^ at the beginning of section content
  l := FindIniNameValueP(P, PEnd, UpperName, v);
  if v = nil then
    result := DefaultValue
  else
    FastSetString(result, v, l); // return '' for l=0 (existing void value)
end;

function ExistsIniName(P: PUtf8Char; UpperName: PAnsiChar): boolean;
var
  v: PUtf8Char;
begin
  FindIniNameValueP(P, {PEnd=}nil, pointer(UpperName), v);
  result := v <> nil; // found, but maybe with result length = 0
end;

function ExistsIniNameValue(P: PUtf8Char; const UpperName: RawUtf8;
  UpperValues: PPAnsiChar): boolean;
var
  table: PNormTable;
begin
  if (UpperValues <> nil) and
     (UpperValues^ <> nil) and
     (UpperName <> '') then
  begin
    result := true;
    table := @NormToUpperAnsi7;
    while (P <> nil) and
          (P^ <> '[') do
    begin
      if P^ = ' ' then
        repeat
          inc(P)
        until P^ <> ' '; // trim left ' '
      if IdemPChar2(table, P, pointer(UpperName)) then
        if IdemPPChar(GotoNextNotSpace(P + length(UpperName)), UpperValues) >= 0 then
          exit // found one value
        else
          break;
      P := GotoNextLine(P);
    end;
  end;
  result := false;
end;

function GetSectionContent(const Content, SectionName: RawUtf8): RawUtf8;
var
  P, PEnd: PUtf8Char;
  up: TByteToAnsiChar;
begin
  P := pointer(Content);
  PWord(UpperCopy255(@up, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, @up, @PEnd) then
    FastSetString(result, P, PEnd)
  else
    FastAssignNew(result);
end;

function DeleteSection(var Content: RawUtf8; const SectionName: RawUtf8;
  EraseSectionHeader: boolean): boolean;
var
  P: PUtf8Char;
  up: TByteToAnsiChar;
begin
  result := false; // no modification
  P := pointer(Content);
  PWord(UpperCopy255(@up, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, @up) then
    result := DeleteSection(P, Content, EraseSectionHeader);
end;

function DeleteSection(SectionFirstLine: PUtf8Char; var Content: RawUtf8;
  EraseSectionHeader: boolean): boolean;
var
  PEnd: PUtf8Char;
  IndexBegin: PtrInt;
begin
  result := false;
  PEnd := SectionFirstLine;
  if EraseSectionHeader then // erase [Section] header line
    while (PtrUInt(SectionFirstLine) > PtrUInt(Content)) and
          (SectionFirstLine^ <> '[') do
      dec(SectionFirstLine);
  while (PEnd <> nil) and
        (PEnd^ <> '[') do
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

procedure ReplaceSection(SectionFirstLine: PUtf8Char; var Content: RawUtf8;
  const NewSectionContent: RawUtf8);
var
  PEnd: PUtf8Char;
  IndexBegin: PtrInt;
begin
  if SectionFirstLine = nil then
    exit;
  // delete existing [Section] content
  PEnd := SectionFirstLine;
  while (PEnd <> nil) and
        (PEnd^ <> '[') do
    PEnd := GotoNextLine(PEnd);
  IndexBegin := SectionFirstLine - pointer(Content);
  if PEnd = nil then
    SetLength(Content, IndexBegin)
  else
    delete(Content, IndexBegin + 1, PEnd - SectionFirstLine);
  // insert section content
  insert(NewSectionContent, Content, IndexBegin + 1);
end;

procedure ReplaceSection(var Content: RawUtf8; const SectionName, NewSectionContent: RawUtf8);
var
  up: TByteToAnsiChar;
  P: PUtf8Char;
begin
  P := pointer(Content);
  PWord(UpperCopy255(@up, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, @up) then
    ReplaceSection(P, Content, NewSectionContent)
  else
    Append(Content, ['[', SectionName, ']'#13#10, NewSectionContent]);
end;

function FindIniNameValueInteger(P: PUtf8Char; const UpperName: RawUtf8): PtrInt;
var
  v: PUtf8Char;
begin
  FindIniNameValueP(P, {PEnd=}nil, pointer(UpperName), v);
  result := GetInteger(v);
end;

function FindIniEntry(const Content, Section, Name, DefaultValue: RawUtf8): RawUtf8;
var
  P, PEnd: PUtf8Char;
  n, s: TByteToAnsiChar;
begin
  result := DefaultValue;
  P := pointer(Content);
  if P = nil then
    exit;
  // fast n := UpperCase(Name)+'='
  PWord(UpperCopy255(@n, Name))^ := ord('=');
  if Section = '' then
    // find the Name= entry before any [Section]
    result := FindIniNameValue(P, @n, DefaultValue, P + length(Content))
  else
  begin
    // find the Name= entry in the specified [Section]
    PWord(UpperCopy255(@s, Section))^ := ord(']');
    if FindSectionFirstLine(P, @s, @PEnd) then
      result := FindIniNameValue(P, @n, DefaultValue, PEnd);
  end;
end;

function FindWinAnsiIniEntry(const Content, Section, Name: RawUtf8): RawUtf8;
begin
  result := WinAnsiToUtf8(WinAnsiString(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryInteger(const Content, Section, Name: RawUtf8): integer;
begin
  result := GetInteger(pointer(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryFile(const FileName: TFileName;
  const Section, Name, DefaultValue: RawUtf8): RawUtf8;
var
  Content: RawUtf8;
begin
  Content := StringFromFile(FileName);
  if Content = '' then
    result := DefaultValue
  else
    result := FindIniEntry(Content, Section, Name, DefaultValue);
end;

function UpdateNameValueInternal(var Content: RawUtf8;
  const NewValue, NewValueCRLF: RawUtf8;
  var P: PUtf8Char; UpperName: PAnsiChar; UpperNameLength: integer): boolean;
var
  next: PUtf8Char;
  i: PtrInt;
begin
  if UpperName <> nil then
    while (P <> nil) and
          (P^ <> '[') do
    begin
      while P^ = ' ' do
        inc(P);   // trim left ' '
      next := GotoNextLine(P);
      if IdemPChar2(@NormToUpperAnsi7, P, UpperName) then
      begin // update Name=Value entry
        result := true;
        inc(P, UpperNameLength);
        i := (P - pointer(Content)) + 1;
        if (i = length(NewValue)) and
           mormot.core.base.CompareMem(P, pointer(NewValue), i) then
          exit; // new Value is identical to the old one -> no change
        if next = nil then // avoid last line (P-PBeg) calculation error
          SetLength(Content, i - 1)
        else
          delete(Content, i, next - P);   // delete old Value
        insert(NewValueCRLF, Content, i); // set new value
        exit;
      end;
      P := next;
    end;
  result := false;
end;

function UpdateNameValue(var Content: RawUtf8;
  const Name, UpperName, NewValue: RawUtf8): boolean;
var
  P: PUtf8Char;
begin
  result := false;
  if UpperName = '' then
    exit;
  P := pointer(Content);
  result := UpdateNameValueInternal(Content, NewValue, NewValue + #13#10,
    P, pointer(UpperName), length(UpperName));
  if result or
     (Name = '') then
    exit;
  AppendLine(Content, [Name, NewValue]);
  result := true;
end;

procedure UpdateIniEntry(var Content: RawUtf8; const Section, Name, Value: RawUtf8);
var
  P: PUtf8Char;
  SectionFound: boolean;
  i, len: PtrInt;
  V: RawUtf8;
  up: TByteToAnsiChar;
begin
  Join([Value, EOL], V);
  P := pointer(Content);
  // 1. find Section, and try update within it
  if Section = '' then
    SectionFound := true // find the Name= entry before any [Section]
  else
  begin
    PWord(UpperCopy255(@up, Section))^ := ord(']');
    SectionFound := FindSectionFirstLine(P, @up);
  end;
  len := length(Name);
  PWord(UpperCopy255Buf(@up, pointer(Name), len))^ := ord('=');
  inc(len);
  if SectionFound and
     UpdateNameValueInternal(Content, Value, V, P, @up, len) then
      exit;
  // 2. section or Name= entry not found: add Name=Value
  V := Join([Name, '=', V]);
  if not SectionFound then
    // create not existing [Section]
    V := Join(['[', Section, (']' + EOL), V]);
  // insert Name=Value at P^ (end of file or end of [Section])
  if P = nil then
    // insert at end of file
    Append(Content, V)
  else
  begin
    // insert at end of [Section]
    i := (P - pointer(Content)) + 1;
    insert(V, Content, i);
  end;
end;

procedure UpdateIniEntryFile(const FileName: TFileName; const Section, Name, Value: RawUtf8);
var
  Content: RawUtf8;
begin
  Content := StringFromFile(FileName);
  UpdateIniEntry(Content, Section, Name, Value);
  FileFromString(Content, FileName);
end;

function IsHttpHeadersContentTypeText(Headers: PUtf8Char): boolean;
var
  ct: PUtf8Char;
  len: PtrInt;
begin
  ct := FindNameValuePointer(Headers, HEADER_CONTENT_TYPE_UPPER, len);
  result := (ct <> nil) and
            IsContentTypeCompressible(ct, len, {onlytext=}true);
end;

const
  WS_UPGRADE: array[0..2] of PAnsiChar = (
    'UPGRADE',
    'KEEP-ALIVE, UPGRADE',
    nil);

function IsHttpHeadersTextWebSocketUpgrade(headers: PUtf8Char): boolean;
begin
  result := ExistsIniNameValue(pointer(headers), 'CONNECTION: ', @WS_UPGRADE);
end;

function IniToObject(const Ini: RawUtf8; Instance: TObject;
  const SectionName: RawUtf8; DocVariantOptions: PDocVariantOptions;
  Level: integer; Features: TIniFeatures): boolean;
var
  r: TRttiCustom;
  i, uplen: PtrInt;
  p: PRttiCustomProp;
  section, sectionend, nested, nestedend: PUtf8Char;
  obj: TObject;
  n: RawUtf8;
  up: TByteToAnsiChar;

  function FillUp(p: PRttiCustomProp): PAnsiChar;
  begin
    result := @up;
    if Level <> 0 then
    begin
      result := UpperCopy255(result, SectionName); // recursive name
      result^ := '.';
      inc(result);
    end;
    result := UpperCopy255(result, p^.Name);
    uplen := result - PAnsiChar(@up);
    nested := pointer(Ini);
  end;

  function FillProp(obj: TObject; p: PRttiCustomProp): boolean;
  var
    v: RawUtf8;
    json: PUtf8Char;
    item: TObject;
  begin
    result := false;
    v := FindIniNameValue(section, @up, #0, sectionend);
    if (ifMultiLineSections in Features) and
       (v = #0) and
       (rcfMultiLineStrings in p^.Value.Flags) then // strings or array of RawUtf8
    begin
      PWord(FillUp(p))^ := ord(']');
      if FindSectionFirstLine(nested, @up, @nestedend) then
      begin
        // multi-line text value has been stored in its own section
        FastSetString(v, nested, nestedend);
        if p^.Prop^.SetValueText(obj, v) then
          result := true;
      end;
    end
    else if v <> #0 then // we found this propname=value in section..sectionend
      if (p^.OffsetSet <= 0) or // setter does not support JSON
         (rcfBoolean in p^.Value.Cache.Flags) or // simple value from text
         (p^.Value.Kind in (rkIntegerPropTypes + rkStringTypes +
                            [rkEnumeration, rkSet, rkFloat])) then
      begin
        if p^.Prop^.SetValueText(obj, v) then // RTTI conversion from JSON/CSV
          result := true;
      end
      else // e.g. rkVariant, rkDynArray complex values from JSON array/object
      begin
        json := pointer(v);
        GetDataFromJson(@PByteArray(obj)[p^.OffsetSet], json,
          nil, p^.Value, DocVariantOptions, true, nil);
        if (json = nil) and
           (v <> '') and
           (ifMultiLineJsonArray in Features) and // try multi line JSON
           (v[1] = '[') and
           (PosExChar(']', v) = 0) and
           (FindIniNameValueP(section, sectionend, @up, json) <> 0) then
          GetDataFromJson(@PByteArray(obj)[p^.OffsetSet], json,
            nil, p^.Value, DocVariantOptions, true, nil);
        if json <> nil then
          result := true;
      end
    else // v=#0 i.e. no propname=value in this section
    if (rcfObjArray in p^.Value.Flags) and
       (p^.OffsetSet >= 0) and
       (ifArraySection in Features) then // recognize e.g. [name-xxx] or [name xxx]
    begin
      FillUp(p)^ := #0;
      repeat
        if nested^ = '[' then
        begin
          inc(nested);
          if IdemPChar2(@NormToUpperAnsi7, nested, @up) and
             not (tcIdentifier in TEXT_CHARS[nested[uplen]]) then // not azAZ_09
          begin
            nestedend := PosChar(nested, ']');
            if nestedend <> nil then
            begin
              FastSetString(n, nested, nestedend);
              item := p^.Value.ArrayRtti.ClassNewInstance;
              if item <> nil then
                if IniToObject(Ini, item, n, DocVariantOptions, Level + 1,
                     Features - [ifClassSection] + [ifClassValue]) then
                begin
                  PtrArrayAdd(PPointer(@PByteArray(obj)[p^.OffsetSet])^, item);
                  result := true;
                end
                else
                  item.Free;
            end;
          end;
        end;
        nested := GotoNextLine(nested);
      until nested = nil;
    end;
  end;

  function FillObj: boolean;
  var
    i: integer;
    pp: PRttiCustomProp;
    r: TRttiCustom;
    u: PAnsiChar;
  begin
    result := false;
    r := Rtti.RegisterClass(obj);
    pp := pointer(r.Props.List);
    for i := 1 to r.Props.Count do
    begin
      if pp^.Prop <> nil then
      begin
        u := UpperCopy255(@up, p^.Name);
        u^ := '.';
        inc(u);
        PWord(UpperCopy255(u, pp^.Name))^ := ord('='); // [p.Name].[pp.Field]=
        if FillProp(obj, pp) then
          result := true;
      end;
      inc(pp);
    end;
  end;

begin
  result := false; // true when at least one property has been read
  if (Ini = '') or
     (Instance = nil) then
    exit;
  section := nil; // SectionName='' if only nested rkClass/rkDynArray
  if SectionName <> '' then
  begin
    PWord(UpperCopy255(@up, SectionName))^ := ord(']');
    section := pointer(Ini);
    if not FindSectionFirstLine(section, @up, @sectionend) then
      exit; // section not found
  end;
  r := Rtti.RegisterClass(Instance);
  p := pointer(r.Props.List);
  for i := 1 to r.Props.Count do
  begin
    if p^.Prop <> nil then
    begin
      if ifClearValues in Features then
        p^.ClearValue(Instance, {freeandnil=}false);
      if p^.Value.Kind = rkClass then
      begin
        obj := p^.Prop^.GetObjProp(Instance);
        if obj <> nil then
          if (ifClassValue in Features) and // check PropName.Field= entries
             FillObj then
            result := true
          else if ifClassSection in Features then
          begin // recursive load from another per-property section
            if Level = 0 then
              n := p^.Name
            else
              Join([SectionName, '.', p^.Name], n);
            if IniToObject(Ini, obj, n, DocVariantOptions, Level + 1, Features) then
              result := true;
          end;
      end
      else
      begin
        PWord(UpperCopy255(@up, p^.Name))^ := ord('=');
        if FillProp(Instance, p) then
          result := true;
      end;
    end;
    inc(p);
  end;
end;

function TrimAndIsMultiLine(var U: RawUtf8): boolean;
var
  L: PtrInt;
  P: PUtf8Char absolute U;
begin
  result := false;
  L := length(U);
  if L = 0 then
    exit;
  while P[L - 1] in [#13, #10] do
  begin
    dec(L);
    if L = 0 then
    begin
      FastAssignNew(U); // no meaningful text
      exit;
    end;
  end;
  if L <> length(U) then
    SetLength(U, L); // trim right
  if BufferLineLength(P, P + L) = L then // may use x86_64 SSE2 asm
    exit; // no line feed
  result := true; // there are line feeds within this text
  U := TrimChar(U, [#13]); // normalize #13#10 into #10 as ObjectToIni()
end;

function ObjectToIni(const Instance: TObject; const SectionName: RawUtf8;
  Options: TTextWriterWriteObjectOptions; Level: integer; Features: TIniFeatures): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
  nested: TRawUtf8DynArray;
  nestedcount, i: integer;

  procedure WriteClassInMainSection(obj: TObject; feat: TIniFeatures;
    const prefix: RawUtf8);
  var
    i, a: PtrInt;
    v64: Int64;
    arr: PPointer;
    o: TObject;
    r: TRttiCustom;
    p: PRttiCustomProp;
    s: RawUtf8;

    function FullSectionName: RawUtf8;
    begin
      if Level = 0 then
        result := p^.Name
      else
        Join([SectionName, '.', p^.Name], result);
    end;

    procedure WriteNameEqual;
    begin
      if prefix <> '' then
        W.AddString(prefix);
      W.AddString(p^.Name);
      W.Add('=');
    end;

  begin
    r := Rtti.RegisterClass(obj);
    p := pointer(r.Props.List);
    for i := 1 to r.Props.Count do
    begin
      if p^.Prop <> nil then
        case p^.Value.Kind of
          rkClass:
            begin
              o := p^.Prop^.GetObjProp(obj);
              if o <> nil then
                if ((Level = 0) and
                    (prefix = '')) or
                   (ifClassSection in feat) then // priority over ifClassValue
                begin
                  s := ObjectToIni(o, FullSectionName, Options, Level + 1, feat);
                  if s <> '' then
                    AddRawUtf8(nested, nestedcount, s);
                end
                else
                  WriteClassInMainSection(o, feat, Join([prefix, p^.Name, '.']));
            end;
          rkEnumeration, rkSet:
            begin
              if woHumanReadableEnumSetAsComment in Options then
              begin
                p^.Value.Cache.EnumInfo^.GetEnumNameAll(
                  s, '; values=', #10, {quoted=}false, {trimmed=}true, {sep=}'/');
                W.AddString(s);
              end;
              // AddValueJson() would have written "quotes" or ["a","b"]
              WriteNameEqual;
              v64 := p^.Prop^.GetOrdProp(obj);
              if p^.Value.Kind = rkEnumeration then
                W.AddTrimLeftLowerCase(p^.Value.Cache.EnumInfo^.GetEnumNameOrd(v64))
              else
                p^.Value.Cache.EnumInfo^.GetSetNameJsonArray(
                  W, v64, ',', {quote=}#0, {star=}true, {trim=}true);
              W.Add(#10);
            end;
          else
            if (ifMultiLineSections in feat) and
               (rcfMultiLineStrings in p^.Value.Flags) then
            begin
              p^.Prop^.GetAsString(obj, s); // strings or array of RawUtf8
              if TrimAndIsMultiLine(s) then
                // store multi-line text values in their own section
                AddRawUtf8(nested, nestedcount,
                  FormatUtf8('[%]'#10'%'#10#10, [FullSectionName, s]))
              else
              begin
                WriteNameEqual;
                W.AddString(s); // single line text
                W.Add(#10);
              end;
            end
            else if (rcfObjArray in p^.Value.Flags) and
                    (ifArraySection in feat) and
                    (p^.OffsetGet >= 0) then
            begin
              arr := PPointer(PAnsiChar(obj) + p^.OffsetGet)^;
              if arr <> nil then
                for a := 0 to PDALen(PAnsiChar(arr) - _DALEN)^ + (_DAOFF - 1) do
                begin
                  s := SectionName;  // e.g. [section.propnam#0]
                  if s <> '' then
                    Append(s, '.');
                  s := ObjectToIni(arr^, Make([s, p.Name, '#', a]),
                    Options - [woHumanReadableEnumSetAsComment],
                    Level + 1, feat - [ifClassSection] + [ifClassValue]);
                  if s <> '' then
                    AddRawUtf8(nested, nestedcount, s);
                  inc(arr);
                end;
            end
            else
            begin
              WriteNameEqual;
              p^.AddValueJson(W, obj, // simple and complex types
                Options - [woHumanReadableEnumSetAsComment, woInt64AsHex],
                twOnSameLine);
              W.Add(#10);
            end;
        end;
      inc(p);
    end;
  end;

begin
  FastAssignNew(result);
  if Instance = nil then
    exit;
  nestedcount := 0;
  W := DefaultJsonWriter.CreateOwnedStream(tmp);
  try
    W.CustomOptions := [twoTrimLeftEnumSets];
    if SectionName <> '' then
      W.Add('[%]'#10, [SectionName]);
    WriteClassInMainSection(Instance, Features, '');
    W.Add(#10);
    for i := 0 to nestedcount - 1 do
      W.AddString(nested[i]); // eventually write other sections
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ ************* TSynJsonFileSettings parent class }

{ TSynJsonFileSettings }

constructor TSynJsonFileSettings.Create;
begin
  inherited Create;
  fIniOptions := [ifClassSection, ifClassValue, ifMultiLineSections, ifArraySection];
end;

function TSynJsonFileSettings.AfterLoad(const aText: RawUtf8): boolean;
begin
  fCurrentContent := aText;
  fCurrentHash := DefaultHashTrim(aText);
  result := true; // success
end;

function TSynJsonFileSettings.LoadFromJsonText(const aJson: RawUtf8): boolean;
begin
  result := JsonSettingsToObject(aJson, self) and // recognize HJson/JSON5
            AfterLoad(aJson);
end;

function TSynJsonFileSettings.LoadFromIniText(const aIni, aSectionName: RawUtf8): boolean;
begin
  fSectionName := aSectionName; // to be used when writing
  result := IniToObject(aIni, self, aSectionName, @JSON_[mFastFloat], 0, fIniOptions);
  if not result then
    exit;
  include(fSettingsOptions, fsoWriteIni); // save back as INI
  result := AfterLoad(aIni);
end;

function TSynJsonFileSettings.LoadFromYamlText(const aYaml: RawUtf8): boolean;
var
  json: RawUtf8;
begin
  result := TryYamlToJson(aYaml, json) and
            JsonSettingsToObject(json, self);
  if not result then
    exit;
  include(fSettingsOptions, fsoWriteYaml); // save back as YAML
  result := AfterLoad(aYaml);
end;

function TSynJsonFileSettings.LoadFromText(const aContent: RawUtf8;
  const aSectionName: RawUtf8): boolean;
begin
  if fsoReadIni in fSettingsOptions then
  begin
    result := LoadFromIniText(aContent, aSectionName); // only INI
    include(fSettingsOptions, fsoWriteIni); // save back as INI
  end
  else
    result := LoadFromJsonText(aContent) or
              LoadFromIniText(aContent, aSectionName); // fallback to INI
end;

{$ifndef PUREMORMOT2}
function TSynJsonFileSettings.LoadFromJson(const aContent: RawUtf8;
  const aSectionName: RawUtf8): boolean;
begin
  result := LoadFromText(aContent, aSectionName);
end;
{$endif PUREMORMOT2}

function TSynJsonFileSettings.LoadFromFile(const aFileName: TFileName;
  const aSectionName: RawUtf8): boolean;
var
  text: RawUtf8;
begin
  fCurrentContent := ''; // ignore file neither valid JSON nor INI/YAML
  fCurrentHash := 0;
  fFileName := aFileName;
  text := RawUtf8FromFile(aFileName); // may detect BOM
  if text = '' then
    result := false
  else
    case SameExt(aFileName,
        ['yaml', 'yml', 'ini', 'json', 'jsonc', 'json5', 'hjson'], true) of
      0, 1:
        result := LoadFromYamlText(text);
      2:
        result := LoadFromIniText(text, aSectionName);
      3 .. 6:
        result := LoadFromJsonText(text); // JsonSettingsToObject supports those
    else
      result := LoadFromText(text, aSectionName); // detect JSON or INI
    end;
end;

function TSynJsonFileSettings.FolderName: TFileName;
begin
  if self = nil then
    result := ''
  else
    result := ExtractFilePath(fFileName);
end;

function TSynJsonFileSettings.SaveIfNeeded: boolean;
var
  saved: RawUtf8;
  opt: TTextWriterWriteObjectOptions;
begin
  result := false;
  if (self = nil) or
     (fFileName = '') or
     (fsoDisableSaveIfNeeded in fSettingsOptions) then
    exit;
  opt := SETTINGS_WRITEOPTIONS;
  if fSettingsOptions * [fsoNoEnumsComment, fsoWriteYaml, fsoWriteIni] <> [] then
    exclude(opt, woHumanReadableEnumSetAsComment);
  if fsoWriteIni in fSettingsOptions then
    saved := ObjectToIni(self, fSectionName, opt, 0, fIniOptions)
  else
  begin
    saved := ObjectToJson(self, opt);
    if fsoWriteYaml in fSettingsOptions then
      saved := JsonToYaml(saved)
    else if fsoWriteHjson in fSettingsOptions then
      saved := JsonReformat(saved, jsonH); // very human friendly
  end;
  if saved = fCurrentContent then
    exit; // don't rewrite the same content on disk
  result := FileFromString(saved, fFileName);
  if not result then
    exit;
  fCurrentContent := saved;
  fCurrentHash := DefaultHashTrim(saved);
end;


{ ********** JSON and Text Preprocessor }

const
  DSL_INCLUDE_DEPTH = 4; // avoid infinite include <filename>

{ TPreProc }

type
  TPreprocIfLevel = 0 .. 31; // 0 = outer level, 1..31 = nested levels
  TPreprocIf = (
    piNone,
    piIf,
    piIfDef,
    piElse,
    piEnd);

  /// implement pre-processing to JSON or text input
  TPreproc = class(TPreprocAbstract)
  protected
    Vars: TBinDictionary;
    IfLevel: TPreprocIfLevel;       // 8-bit
    IncludeDepth: byte;             // 8-bit
    Options: TPreprocFlags;         // 8-bit
    IfSkip: set of TPreprocIfLevel; // 32-bit
    IncludeFolder: TFileName;
    procedure DoSkip(var P: PUtf8Char);
    function DoFind(Key: pointer; KeyLen: PtrInt; var ValueLen: PtrInt): pointer;
    function DoIf(P: PUtf8Char): PUtf8Char;
    procedure DoEndif(var P: PUtf8Char);
      {$ifdef HASINLINE} inline; {$endif}
    procedure DoRegister(m: TPreprocMarker; k, v, ve: PUtf8Char; kl: PtrInt);
    procedure DoInclude(P: PUtf8Char; Verbatim: boolean);
  public
    /// initialize this pre-processor engine
    constructor Create(flags: TPreprocFlags; const folder: TFileName); reintroduce;
    /// finalize this engine and all transient variables
    destructor Destroy; override;
    /// called with P^=$$ to integrate DSL sections
    function ParseSection(P: PUtf8Char): PUtf8Char; override;
    /// called with P^=$ for conditional process
    function WasIf(var P: PUtf8Char): boolean; override;
    /// called with P^=$ to recognize $(ident) into Value/Len
    function Expand(P: PUtf8Char; var Value: PUtf8Char; var Len: PtrInt;
      KeepMarker: boolean): PUtf8Char; override;
    /// called with P^=$ to recognize $(ident) and append as plain unescaped text
    function ExpandTo(P: PUtf8Char; W: TTextWriter): PUtf8Char; override;
  end;

constructor TPreproc.Create(flags: TPreprocFlags; const folder: TFileName);
begin
  inherited Create;
  Options := flags;
  if DirectoryExists(folder) then
    IncludeFolder := IncludeTrailingPathDelimiter(folder);
  Vars := TBinDictionary.Create;
end;

destructor TPreproc.Destroy;
begin
  inherited Destroy;
  Vars.Free;
end;

function TPreproc.DoFind(Key: pointer; KeyLen: PtrInt; var ValueLen: PtrInt): pointer;
begin
  result := Vars.Find(Key, KeyLen, @ValueLen); // from known variables/templates
  if result = nil then
    result := GlobalInfoFind(Key, KeyLen, ValueLen); // global macros
end;

function TPreproc.Expand(P: PUtf8Char; var Value: PUtf8Char; var Len: PtrInt;
  KeepMarker: boolean): PUtf8Char;
var
  key: PUtf8Char;
  keylen: PtrInt;
  ending: AnsiChar;
begin
  result := P;
  inc(result); // called with P^ = '$'
  ending := '$';
  if result^ in ['(', '{'] then // $(ident) ${ident} format, not $ident$
  begin
    if result^ = '(' then
      ending := ')'
    else
      ending := '}';
    inc(result);
  end;
  key := result;
  while (result^ <> ending) and
        not (result^ in [#0 .. ' ', '|']) do
    inc(result);
  Value := nil;
  if result^ <= ' ' then
    exit;
  Value := DoFind(key, result - key, Len); // resolve
  if result^ = '|' then   // $(ident|default) or $ident|default$
  begin
    inc(result);
    if result^ = '$' then // $ident|$default$$ or $(ident|$(def1)|$(def2))}
    begin
      result := Expand(result, key, keylen, KeepMarker); // cascaded defaults
      if Value = nil then // fallback to the nested $default$
      begin
        Value := key;
        Len := keylen;
      end;
      while (result^ <> ending) and
            (result^ in [#0 .. #31]) do
        inc(result);
      inc(result); // skip trailing $ or }
      exit;
    end;
    key := result;
    while (result^ <> ending) and
          not (result^ in [#0 .. #31, '|']) do
      inc(result);
    if result^ < ' ' then
      exit;
    if Value = nil then // fallback to the specified default
    begin
      Value := key;
      Len := result - key;
    end;
  end;
  inc(result); // skip trailing $ } )
  if (Value = nil) or
     KeepMarker or
     (TPreprocMarker(Value^) > high(TPreprocMarker)) then
    exit;
  inc(Value); // trim marker
  dec(Len);
end;

function TPreproc.ExpandTo(P: PUtf8Char; W: TTextWriter): PUtf8Char;
var
  v: PUtf8Char;
  l: PtrInt;
  m: TPreprocMarker;
begin
  result := Expand(P, v, l, {KeepMarker=}true);
  if v = nil then // this $(ident) was not found
    exit;
  m := TPreprocMarker(v^);
  if m = pmTemplate then
    exit;// { } [] not expanded in "string"
  if m <= high(m) then // skip marker
  begin
    inc(v);
    dec(l);
  end;
  W.AddShort(v, l);
end;

procedure TPreproc.DoEndif(var P: PUtf8Char);
begin
  if IfLevel <> 0 then
  begin
    exclude(IfSkip, IfLevel); // cleanup for debugging
    dec(IfLevel);
    if (IfLevel <> 0) and
       (IfLevel in IfSkip) then
      DoSkip(P); // continue skip as in DoIf()
  end
  else if Assigned(OnAddDebugComment) then
    OnAddDebugComment('$endif$ with no prior $if')
end;

function ParseIfDollar(var P: PUtf8Char): TPreprocIf; // caller ensured P^='$'
begin
  result := piNone;
  case PCardinal(P)^ of
    ord('$') + ord('i') shl 8 + ord('f') shl 16 + ord(' ') shl 24:  // '$if '
      begin
        result := piIf;
        inc(P, 4);
      end;
    ord('$') + ord('i') shl 8 + ord('f') shl 16 + ord('d') shl 24:
      if PCardinal(P + 4)^ and $ffffff =
           ord('e') + ord('f') shl 8 + ord(' ') shl 16 then         // '$ifdef '
      begin
        inc(P, 7);
        result := piIfDef;
      end;
    ord('$') + ord('e') shl 8 + ord('l') shl 16 + ord('s') shl 24:
      if cardinal(PWord(P + 4)^) = ord('e') + ord('$') shl 8 then   // '$else$'
      begin
        inc(P, 6);
        result := piElse;
      end;
    ord('$') + ord('e') shl 8 + ord('n') shl 16 + ord('d') shl 24:
      if PCardinal(P + 4)^ and $ffffff =
           ord('i') + ord('f') shl 8 + ord('$') shl 16 then         // '$endif$'
      begin
        inc(P, 7);
        result := piEnd;
      end;
  end;
end;

procedure TPreproc.DoSkip(var P: PUtf8Char);
begin
  repeat
    P := PosChar(P, '$'); // use SSE2 on x86_64
    case ParseIfDollar(P) of
      piNone:
        inc(P);
      piIf:
        begin
          dec(P, 4); // caller should detect and execute DslIf()
          exit;
        end;
      piIfDef:
        begin
          dec(P, 7);
          exit;
        end;
      piEnd:
        begin
          DoEndif(P);
          exit;
        end;
      piElse:
        if IfLevel = 0 then // paranoid
          exit
        else if IfLevel in IfSkip then
        begin
          exclude(IfSkip, IfLevel); // include until $end$
          exit;
        end
        else
          include(IfSkip, IfLevel);  // and continue skip loop
    end;
  until false;
end;

function TPreproc.DoIf(P: PUtf8Char): PUtf8Char;
var
  exp: TTextExpression;
  len, level: PtrInt;
  match: boolean;
begin // P^ = 'id$' or 'id = val$' from '$ifdef id$' or '$if id = val$'
  result := ParseTextExpression(P, exp, {altstopchar=}'$');
  if result = nil then
  begin
    result := @NULCHAR; // force <> nil but #0
    exit;
  end;
  while (exp.ValueLen > 0) and
        (exp.ValueStart[exp.ValueLen - 1] = ' ') do
    dec(exp.ValueLen);
  if exp.ValueLen = 0 then
    if result^ = '$' then // resolve $val$
    begin
      result := Expand(result, exp.ValueStart, len, {keepmarker=}false);
      if exp.ValueStart <> nil then
        exp.ValueLen := len;
      result := GotoNextNotSpace(result);
      if result^ <> '$' then
        exit;
    end
    else
      exit; // '$if id =$' is invalid syntax (ValueLen=-1 for $ifdef id$)
  if IfLevel < high(IfLevel) then
    if (IfLevel > 0) and
       (IfLevel in IfSkip) then // quickly skip all nested $if$ $endif$
    begin
      level := 0;
      while true do
        if result^ = #0 then
          exit
        else if result^ <> '$' then
          inc(result)
        else
          case ParseIfDollar(result) of
            piNone:
              inc(result);
            piIf,
            piIfDef:
              inc(level);
            piEnd:
              if level = 0 then
                break
              else
                dec(level);
          end;
      DoSkip(result);
      exit; // no inc(result) after DslSkip()
    end
    else
    begin
      len := 0; // need a PtrInt, not an integer
      exp.NameStart := DoFind(exp.NameStart, exp.NameLen, len);
      exp.NameLen := len;
      if (exp.NameStart <> nil) and
         (TPreprocMarker(exp.NameStart^) <= high(TPreprocMarker)) then
      begin
       inc(exp.NameStart); // trim marker after raw DoFind()
       dec(exp.NameLen);
      end;
      if exp.ValueLen < 0 then
        match := exp.NameStart <> nil // $ifdef id$ just need DoFind() <> nil
      else if (exp.ValueStart = nil) or
              (exp.ValueLen = 0) then
        match := (exp.NameStart = nil) or (exp.NameLen = 0) // both void
      else
        match := EvaluateTextExpression(exp); // non-void = < > <= >= ~ ~~ * **
      inc(IfLevel);
      if match then // $if$ include [$else$ skip] $endif$
        exclude(IfSkip, IfLevel)
      else
      begin
        include(IfSkip, IfLevel);
        DoSkip(result);
        exit; // no inc(result) after DslSkip()
      end;
    end
  else if Assigned(OnAddDebugComment) then
    OnAddDebugComment('too many nested $if$ (15 allowed)');
  if result^ = '$' then
    inc(result);
end;

function TPreproc.WasIf(var P: PUtf8Char): boolean;
begin
  result := true;
  case ParseIfDollar(P) of // called with P^ = '$'
    piIf,
    piIfDef: // complex $if $ifdef evaluation
      P := DoIf(P);
    piElse:        // $else$ just toggles skip flag
      if IfLevel > 0 then
        if IfLevel in IfSkip then
          exclude(IfSkip, IfLevel) // include until $end$
        else
        begin
          include(IfSkip, IfLevel); // skip until $end$
          DoSkip(P);
        end
      else if Assigned(OnAddDebugComment) then
        OnAddDebugComment('$else$ with no prior $if');
    piEnd:         // $endif$
      DoEndif(P);
  else
    result := false;
  end;
end;

function GotoTemplateEnding(P: PUtf8Char): PUtf8Char;
var
  level: integer;
begin
  result := P + 1; // skip { [
  level := 0;
  repeat
    case result^ of
      #0:
        exit;
      '{',
      '[':
        inc(level);
      '}',
      ']':
        if level = 0 then
          break
        else
          dec(level);
      '"':
        result := GotoEndOfJsonString(result);
    end;
    inc(result);
  until false;
end;

procedure TPreproc.DoRegister(m: TPreprocMarker; k, v, ve: PUtf8Char; kl: PtrInt);
var
  beg, val: PUtf8Char;
  l, vallen: PtrInt; // @vallen = PPtrInt
  comment: PShortString;
  tmp: TSynTempAdder;
begin
  tmp.Init;
  tmp.AddDirect(AnsiChar(m)); // type: #0=template #1="string" #2=const/num
  repeat
    beg := v;
    while (v < ve) and
          (cardinal(PWord(v)^) <> SLASH_16) and
          not (v^ in ['$', '#']) do
      inc(v);
    tmp.Add(beg, v - beg);
    if v >= ve then
      break;
    if v^ = '$' then
      if (m = pmTemplate) and
         WasIf(v) then // $if$ $endif$
        continue
      else
      begin
        // $ident$ ${ident} $env:NAME$ ${env:NAME}
        v := Expand(v, val, vallen, {KeepMarker=}false);
        if val <> nil then
          tmp.Add(val, vallen); // early evaluation of $(ident) within templates
      end
    else
    begin
      // # comment or // comment
      l := v - beg;
      while (l <> 0) and
            (v[l - 1] = ' ') do
        dec(l); // trim right
      if l > 0 then
      begin
        tmp.Add(beg, l);
        tmp.AddDirect(#10);
      end;
      v := GotoNextLineSmall(v);
    end;
  until v >= ve;
  Vars.Update(k, tmp.Buffer, kl, tmp.Size);
  if Assigned(OnAddDebugComment) then
  begin
    comment := tmp.Buffer;
    if tmp.Size > 100 then
    begin
      comment^[0] := #100; // truncate to 100 chars seems enough for a comment
      AppendShort('... size=', comment^);
      AppendShortCardinal(tmp.Size, comment^);
    end
    else
      comment^[0] := AnsiChar(tmp.Size - 1); // we can append a small value
    AppendShort(' as $(', comment^);
    AppendShortBuffer(pointer(k), kl, high(comment^), pointer(comment));
    AppendShortCharSafe(')', comment^);
    OnAddDebugComment(comment^);
  end;
  tmp.Store.Done; // free memory - unlikely from heap
end;

function TPreproc.ParseSection(P: PUtf8Char): PUtf8Char;
var
  key, value: PUtf8Char;
  keylen: PtrInt;
  marker: TPreprocMarker;
label
  ok;
begin // called with P^ = '$$'
  result := P + 2; // allow '$$' or '$$ some text' markers
  if result^ = '$' then
  begin
    result := GotoNextLineSmall(result + 1);
    P := result; // P^ = line just after '$$$' = verbatim start
    repeat
      result := GotoNextNotSpace(GotoNextLineSmall(result));
    until (result^ = '$') and
          (cardinal(PWord(result + 1)^) = DOLLAR_16);
    if Assigned(OnVerbatim) then
      OnVerbatim(P, result - P);
    result := GotoNextLineSmall(result); // ignore trailing '$$$' line
    exit;
  end;
  repeat
    result := GotoNextLineSmall(result);
ok: result := GotoNextNotSpace(result);
    case result^ of
      #0:
        exit;
      '$':
        begin
          if result[1] = '$' then
            break;    // end of DSL section
          P := result;
          if not WasIf(P) then
            continue; // $ is not allowed in identifiers anyway
          result := P;
          goto ok;    // $if$ $else$ $endif$ conditional logic
        end;
      '#',
      '/':
        continue;     // comment line
      'i',
      'I':
        if (IncludeFolder <> '') and
           (result[1] in ['n', 'N']) then
          case IdemPCharSep(result + 2, 'CLUDE |SERT |') of
            0:
              begin
                DoInclude(GotoNextNotSpace(result + 8), {verbatim=}false);
                continue; // this is a reserved keyword, never an identifier
              end;
            1:
              begin
                DoInclude(GotoNextNotSpace(result + 8), {verbatim=}true);
                continue; // this is a reserved keyword, never an identifier
              end;
          end;
    end;
    key := result;
    repeat
      inc(result);
    until result^ in [#0 .. ' ', '=', ':', '$', '|'];
    keylen := result - key;
    result := GotoNextNotSpace(result);
    if not (result^ in ['=', ':']) then
      continue;       // $ and | are not allowed in identifiers
    while result^ in ['=', ':', ' '] do
      inc(result);    // allow := or == syntax
    marker := pmEscapedString;
    value := result + 1; // early to make Delphi happy - exclude starting { [ "
    case result^ of
      #0:
        exit;
      '{',
      '[': // value = whole {..}/[..] text block
        begin
          result := GotoTemplateEnding(result);
          DoRegister(pmTemplate, key, value, result - 1, keylen);
          continue; // has been expanded and processed for any nested $if$
        end;
      '"',
      '''':
        result := GotoEndOfQuotedString(result); // ignore double quotes within
    else
      begin
        value := result;
        while not (result^ in [#0 .. #31, '#', '/']) do
        begin
          // unquoted value in $$ DSL section = till end of line or comment
          if result^ in ['\', '"'] then
            marker := pmPlainString; // would need W.AddJsonEscape()
          inc(result);
        end;
        while result[-1] = ' ' do
          dec(result); // trim right
        if IsConstantOrNumberJson(value, result - value) then
          marker := pmConstNum;
      end;
    end;
    DoRegister(marker, key, value, result, keylen);
  until false;
  result := GotoNextLineSmall(result); // ignore ending '$$...' marker line
end;

procedure TPreproc.DoInclude(P: PUtf8Char; Verbatim: boolean);
var
  tmp: ShortString; // to compute the expanded filename
  v: PUtf8Char;
  vlen: PtrInt;     // @vlen = PPtrInt
  c: AnsiChar;
  txt: RawUtf8;     // UTF-8 file name
  fn: TFileName;    // RTL file name
begin
  tmp[0] := #0;
  if P^ in ['"', ''''] then
    inc(P); // allow quoted "<filename>", but with no quotes support
  while not (P^ in [#0, '"', '''', #13, #10]) do
  begin
    if P^ = '$' then
    begin
      P := Expand(P, v, vlen, {KeepMarker=}false);
      if v <> nil then // include "config/general-$mode$.json"
        AppendShortBuffer(pointer(v), vlen, high(tmp), @tmp);
    end
    else
    begin
      c := P^;
      if c = InvertedPathDelim then
        c := PathDelim; // normalize for direct cross-platform support
      AppendShortCharSafe(c, tmp);
    end;
  end;
  if IncludeDepth >= DSL_INCLUDE_DEPTH then
  begin
    if Assigned(OnAddDebugComment) then
      OnAddDebugComment('include too deep: rejected');
    exit;
  end;
  fn := MakeString([tmp]);
  if IsExpandedPath(fn) then
  begin
    if not (jppIncludeAbsolute in Options) then
      exit;
  end else if SafeFileName(fn) then
    fn := IncludeFolder + fn
  else
    exit;
  txt := RawUtf8FromFile(fn);
  if Assigned(OnAddDebugComment) then
  begin
    if txt = '' then
      AppendShort(' not found: skipped', tmp)
    else
    begin
      AppendShort(' included: size=', tmp);
      AppendShortCardinal(length(txt), tmp);
    end;
    OnAddDebugComment(tmp);
  end;
  if txt = '' then
    exit;
  inc(IncludeDepth);
  if Verbatim then
    OnVerbatim(pointer(txt), length(txt)) // insert <filename>
  else
    OnAppend(pointer(txt));               // include <filename>
  dec(IncludeDepth);
end;


function JsonPreprocess(const Json: RawUtf8; Format: TTextWriterJsonFormat;
  Flags: TPreprocFlags; const IncludeRoot: TFileName): RawUtf8;
var
  preproc: TPreproc;
begin
  preproc := TPreProc.Create(Flags, IncludeRoot);
  try
    JsonBufferReformat(pointer(Json), result, Format, preproc);
  finally
    preproc.Free;
  end;
end;

function JsonPreprocessToFile(const Json: RawUtf8; const Dest: TFileName;
  Format: TTextWriterJsonFormat; Flags: TPreprocFlags; const IncludeRoot: TFileName): boolean;
var
  preproc: TPreproc;
begin
  preproc := TPreProc.Create(Flags, IncludeRoot);
  try
    result := JsonBufferReformatToFile(pointer(Json), Dest, Format, preproc);
  finally
    preproc.Free;
  end;
end;

function JsonPreprocessToStream(const Json: RawUtf8; Dest: TStream;
  Format: TTextWriterJsonFormat; Flags: TPreprocFlags; const IncludeRoot: TFileName): boolean;
var
  preproc: TPreproc;
begin
  preproc := TPreProc.Create(Flags, IncludeRoot);
  try
    result := JsonBufferReformatToStream(pointer(Json), Dest, Format, preproc);
  finally
    preproc.Free;
  end;
end;


{ TPreprocText }

type
  /// called from TextPreprocess/TextPreprocessToFile
  TPreprocText = class(TPreproc)
  protected
    W: TTextWriter;
    LineFeed: cardinal;    // e.g. #13#10
    LineFeedLen: cardinal; // e.g. 2
    // TPreprocAbstract OnAppend/OnVerbatim/OnAddDebugComment callbacks
    function TextAppend(P: PUtf8Char): boolean;
    procedure TextVerbatim(P: PUtf8Char; Len: PtrInt);
    procedure TextComment(const info: ShortString);
  public
    function TextPreprocessToWriter(P: PUtf8Char; Dest: TTextWriter): boolean;
  end;

function TPreprocText.TextAppend(P: PUtf8Char): boolean;
begin
  W.AddNoJsonEscape(P, StrLen(P));
  result := true; // not used anyway
end;

procedure TPreprocText.TextVerbatim(P: PUtf8Char; Len: PtrInt);
begin
  W.AddNoJsonEscape(P, Len);
end;

procedure TPreprocText.TextComment(const info: ShortString);
begin
  PCardinal(W.B + 1)^ := LineFeed;
  inc(W.B, LineFeedLen);
  W.AddShort('# debug: ');
  W.AddShort(info);
  PCardinal(W.B + 1)^ := LineFeed;
  inc(W.B, LineFeedLen);
end;

function TPreprocText.TextPreprocessToWriter(P: PUtf8Char; Dest: TTextWriter): boolean;
var
  B, v: PUtf8Char;
  l: PtrInt;
begin
  result := false;
  if (P = nil) or
     (Dest = nil) then
    exit;
  // setup the callbacks
  W := Dest;
  OnAppend := TextAppend;
  OnVerbatim := TextVerbatim;
  if jppDebugComment in Options then
  begin
    OnAddDebugComment := TextComment;
    // detect the first source line feed for proper TextComment() mimics
    B := P;
    while not (P ^ in [#0, #10, #13]) do
      inc(P);
    if P^ = #13 then
    begin
      LineFeed := EOLW;
      LineFeedLen := 2;
    end
    else
    begin
      LineFeed := 10;
      LineFeedLen := 1;
    end;
    P := B;
  end;
  // main loop
  repeat
    while (P^ <> #0) and
          (P^ <> '$') do
      inc(P);
    W.AddNoJsonEscape(B, P - B);
    if P^ = #0 then
      break;
    if P[1] = '$' then
    begin
      B := ParseSection(P);    // $$ ... $$ DSL section: include + vars
      continue;
    end;
    B := P;
    if WasIf(B) then           // $if$ $else$ $endif$
      continue;
    if P[1] in ['(', '{'] then // only $(ident) ${ident} - not $ident$
    begin
      P := Expand(P, v, l, {keepmarker=}false);
      if v <> nil then
        W.AddShort(v, l);
    end
    else
      inc(P); // this was not $(ident) -> write verbatim and continue
  until false;
  result := true;
end;


function TextPreprocessToStream(const Text: RawUtf8; Dest: TStream;
  Flags: TPreprocFlags; const IncludeRoot: TFileName): boolean;
var
  preproc: TPreprocText;
  W: TTextWriter;
  temp: TBuffer128K;
begin
  result := false;
  if (Text = '') or
     (Dest = nil) then
    exit;
  preproc := TPreprocText.Create(Flags, IncludeRoot);
  try
    W := TTextWriter.Create(Dest, @temp, SizeOf(temp));
    try
      result := preproc.TextPreprocessToWriter(pointer(Text), W);
      W.FlushFinal;
    finally
      W.Free;
    end;
  finally
    preproc.Free;
  end;
end;

function TextPreprocess(const Text: RawUtf8; Flags: TPreprocFlags;
  const IncludeRoot: TFileName): RawUtf8;
var
  S: TRawByteStringStream;
begin
  S := TRawByteStringStream.Create;
  try
    TextPreprocessToStream(Text, S, Flags, IncludeRoot);
    result := S.DataString;
  finally
    S.Free;
  end;
end;

function TextPreprocessToFile(const Text: RawUtf8; const Dest: TFileName;
  Flags: TPreprocFlags; const IncludeRoot: TFileName): boolean;
var
  F: TStream;
begin
  try
    F := TFileStreamEx.Create(Dest, fmCreate);
    try
      TextPreprocessToStream(Text, F, Flags, IncludeRoot);
      result := true;
    finally
      F.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;


{ ********** Source Code Generation Functions }

function IsReservedKeyWord(const aName: RawUtf8): boolean;
var
  up: TByteToAnsiChar;
begin
  UpperCopy255Buf(@up, pointer(aName), length(aName))^ := #0;
  result := FastFindPUtf8CharSorted(
    @RESERVED_KEYWORDS, high(RESERVED_KEYWORDS), @up) >= 0; // O(log(n)) search
end;

function SanitizePascalName(const aName: RawUtf8; KeyWordCheck: boolean): RawUtf8;
begin
  CamelCase(aName, result);
  if result = '' then
    ESynUnicode.RaiseFmt(nil, 'Unexpected SanitizePascalName(%s)', [aName]);
  result[1] := NormToUpperAnsi7[result[1]]; // ensure PascalCase
  if KeyWordCheck and
     IsReservedKeyWord(result) then
    result := '_' + result; // avoid identifier name collision
end;

function BinToSource(const ConstName, Comment: RawUtf8; Data: pointer;
  Len, PerLine: PtrInt; const Suffix: RawUtf8; LF: TLineFeed): RawUtf8;
var
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  FastAssignNew(result);
  if (Data = nil) or
     (Len <= 0) or
     (PerLine <= 0) then
    exit;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    BinToSource(W, ConstName, Comment, Data, Len, PerLine, LINE_FEED[LF]);
    if Suffix <> '' then
    begin
      W.AddString(Suffix);
      W.AddShort(LINE_FEED[LF]);
    end;
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function BinToSource(const ConstName, Comment: RawUtf8;
  const Data: RawByteString; PerLine: PtrInt; const Suffix: RawUtf8;
  LF: TLineFeed): RawUtf8;
begin
  result := BinToSource(ConstName, Comment, pointer(Data), length(Data),
    PerLine, Suffix, LF);
end;

procedure BinToSource(Dest: TTextWriter; const ConstName, Comment: RawUtf8;
  Data: pointer; Len, PerLine: PtrInt; const LF: ShortString);
var
  line, i: PtrInt;
  P: PByte;
begin
  if (Dest = nil) or
     (Data = nil) or
     (Len <= 0) or
     (PerLine <= 0) then
    exit;
  Dest.AddShorter('const');
  if Comment <> '' then
    Dest.Add('%  // %', [LF, Comment]);
  Dest.Add('%  %: array[0..%] of byte = (', [LF, ConstName, Len - 1]);
  P := pointer(Data);
  repeat
    Dest.AddShorter(LF);
    Dest.AddDirect(' ', ' ', ' ');
    line := MinPtrInt(Len, PerLine);
    for i := 1 to line do
    begin
      Dest.AddDirect(' ', '$');
      Dest.AddByteToHexLower(P^);
      inc(P);
      Dest.AddComma;
    end;
    dec(Len,line);
  until Len = 0;
  Dest.CancelLastComma;
  Dest.Add(');%  %_LEN = SizeOf(%);%', [LF, ConstName, ConstName, LF]);
end;

procedure TextToSource(Dest: TTextWriter; P: PUtf8Char; const LF: ShortString);
var
  line: string[87];
  inquote: boolean;
begin
  if P = nil then
    exit;
  inquote := false;
  line[0] := #0;
  while P^ <> #0 do
  begin
    if line[0] = #0 then
      PCardinal(@line)^ := $202002
    else if line[0] > #75 then
    begin
      if inquote then
        AppendShortChar('''', @line);
      inquote := false;
      AppendShortTwoChars(ord(' ') + ord('+') shl 8, @line);
      AppendShort(LF, line);
      Dest.AddShort(line);
      PCardinal(@line)^ := $202002;
    end;
    if (P^ < ' ') = inquote then
    begin
      AppendShortChar('''', @line);
      inquote := not inquote;
    end;
    if P^ in [#32 .. #126] then
      if P^ = '''' then
        AppendShortTwoChars(ord('''') + ord('''') shl 8, @line)
      else
        AppendShortChar(P^, @line)
    else
    begin // control chars or 8-bit high values (may not be true UTF-8)
      AppendShortChar('#', @line);
      AppendShortByte(ord(P^), @line);
    end;
    inc(P);
  end;
  if line[0] = #0 then
    exit;
  if inquote then
    AppendShortChar('''', @line);
  AppendShort(LF, line);
  Dest.AddShort(line);
end;

function TextToSource(const Text: RawUtf8; LF: TLineFeed): RawUtf8;
var
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  FastAssignNew(result);
  if Text = '' then
    exit;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    TextToSource(W, pointer(Text), LINE_FEED[LF]);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TextToSourceShort(const Text: RawUtf8; LF: TLineFeed): ShortString;
var
  temp: TLocalWriter;
begin
  TextToSource(temp.Init(result), pointer(Text), LINE_FEED[LF]);
  temp.Done;
end;


procedure InitializeUnit;
var
  esc: PAnsiCharToByte;
begin
  // HTML Efficient Parsing
  esc := @HTML_ESC[hfAnyWhere]; // HTML_ESCAPED[1..4] = &lt; &gt; &amp; &quot;
  esc[#0]  := 1;
  esc['<'] := 1;
  esc['>'] := 2;
  esc['&'] := 3;
  esc['"'] := 4;
  esc := @HTML_ESC[hfOutsideAttributes];
  esc[#0]  := 1;
  esc['<'] := 1;
  esc['>'] := 2;
  esc['&'] := 3;
  esc := @HTML_ESC[hfWithinAttributes];
  esc[#0]  := 1;
  esc['&'] := 3;
  esc['"'] := 4;
  _AddHtmlEscape := __AddHtmlEscape;
  // XML Efficient Parsing
  FillCharFast(XML_ESC, 31, 9); // ignore invalid #1 .. #31 control char
  esc := @XML_ESC; // XML_ESCAPED[] = &#x09 &#x0a &#x0d &lt &gt &amp &quot &apos
  esc[#0]   := 1;   // go out of loop to abort
  esc[#9]   := 1;
  esc[#10]  := 2;
  esc[#13]  := 3;
  esc['<']  := 4;
  esc['>']  := 5;
  esc['&']  := 6;
  esc['"']  := 7;
  esc[''''] := 8;
end; // EMOJI_*[] constants are delayed via explicit EmojiInit

initialization
  InitializeUnit;

end.
