/// Framework Core Basic Internationalization Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.i18n;

{
  *****************************************************************************

   Basic Internationalization (i18n) Support
    - TSynLanguage per-language translation table
    - TSynLanguages registry with per-thread language selection
    - global wiring of the framework translation hooks

   Translation tables map the original English text to its translation, and
   are loaded from .po (GNU gettext) as the main format - .ini, .yaml and
   .json, with its relaxed JSON5 / JSONC / HJson variants, are also supported.

   Once loaded, three wiring channels are available: the TSynMustache
   translate tag views channel, the LoadResStringTranslate slot consumed by
   the GetCaptionFrom* captions, and - on FPC only - the whole executable
   resourcestring table via TSynLanguages.TranslateResourceStrings.

   Connects the translation slots kept since mORMot 1 mORMoti18n.pas:
   TOnStringTranslate / TOnUtf8Translate callbacks, LoadResStringTranslate,
   i18nDateText / i18nDateTimeText, and the TSynMustache translate tag
   channel - see https://synopse.info/forum/viewtopic.php?id=7592

   Note: the translate tag literal syntax appears in the TSynLanguage
   documentation below, but never within this block comment - on Delphi, a
   curly brace would close the comment right there (no nested comments).

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.fmt;


{ ************* TSynLanguage per-language Translation Table }

type
  /// exception raised on invalid i18n definition or process
  EI18nException = class(ESynException);

  /// implement one translation table for a given language
  // - table keys are expected to be the original English text, following the
  // mORMot 1 mORMoti18n.pas semantic and the Mustache {{"English text}}
  // convention - so a missing key just fallbacks to the original English
  // - thread-safe access via an internal TSynDictionary
  TSynLanguage = class(TSynPersistent)
  protected
    fLanguage: TLanguage;
    fIso: RawUtf8;
    fTexts: TSynDictionary; // RawUtf8 (english/key) -> RawUtf8 (translated)
    fDateFormat, fDateTimeFormat: string;
  public
    /// initialize the table for a given language
    constructor Create(aLanguage: TLanguage); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// merge translations from a TDocVariantData object document
    // - this is the shared implementation of AddFromJson and AddFromYaml: each
    // document field name is the English key, and its value the translation
    // - returns the number of added or replaced pairs, or -1 if Doc is not an
    // object document (e.g. is void or is an array)
    function AddFromVariant(const Doc: TDocVariantData): integer;
    /// merge translations from a JSON object, e.g. {"Hello":"Bonjour"}
    // - the relaxed JSON5 / JSONC / HJson variants are also supported, i.e.
    // comments and unquoted property names, as our settings units do
    // - returns the number of added or replaced pairs, or -1 on invalid JSON
    function AddFromJson(const Json: RawUtf8): integer;
    /// merge translations from an UTF-8 JSON object file (BOM tolerated)
    function AddFromJsonFile(const FileName: TFileName): integer;
    /// merge translations from a YAML mapping content, e.g. as
    // ! Hello: Bonjour
    // ! World: Monde
    // - use the YAML 1.2 core-schema subset parser of mormot.core.fmt
    // - returns the number of added or replaced pairs, or -1 on invalid YAML
    function AddFromYaml(const Yaml: RawUtf8): integer;
    /// merge translations from a GNU gettext .po file content
    // - the .po msgid is the original English text and msgstr its translation,
    // which maps 1:1 on our "key = English text" convention
    // - fuzzy entries, the empty-msgid header and untranslated (void msgstr)
    // entries are just ignored
    // - both CRLF and LF line endings are supported, and the C-like \n \t \r
    // \b \f \\ \" escapes are decoded - any unsupported escape sequence (e.g.
    // \u or \x) just keeps the escaped character itself
    // - non-goals of this basic parser: msgctxt disambiguation, and the
    // msgid_plural / msgstr[] plural forms - such entries are just ignored
    // - returns the number of added or replaced translations
    function AddFromPo(const Po: RawUtf8): integer;
    /// merge translations from an UTF-8 GNU gettext .po file (BOM tolerated)
    function AddFromPoFile(const FileName: TFileName): integer;
    /// merge translations from INI-like content, e.g. as
    // ! Hello=Bonjour
    // ! World=Monde
    // - if aSection is set, only this [aSection] content would be parsed,
    // otherwise the whole content is, ignoring any section header
    // - blank lines, ';' or '#' comment lines, and lines with a void key or a
    // void value are just ignored; blanks around '=' and any trailing CR are
    // trimmed, and both CRLF and LF line endings are supported
    // - note that the INI format has no escaping at all: as a consequence, a
    // translation containing a line feed can't be expressed as INI - use the
    // .po or JSON formats for such texts
    // - returns the number of added or replaced translations
    function AddFromIni(const Ini: RawUtf8; const Section: RawUtf8 = ''): integer;
    /// merge translations from an INI file (UTF-8, BOM tolerated)
    function AddFromIniFile(const FileName: TFileName;
      const Section: RawUtf8 = ''): integer;
    /// merge translations from a file, recognized by its extension
    // - .po (gettext), .ini or .msg, .yaml or .yml, .json / .jsonc / .json5
    // / .hjson (the relaxed JSON variants are read by our JSON parser)
    // - returns -1 if the file does not exist or its extension is unknown
    function AddFromFile(const FileName: TFileName): integer;
    /// translate the supplied text in-place
    // - returns true if the key was found and Text was replaced
    // - returns false and leaves Text untouched (fallback to original text)
    function Translate(var Text: RawUtf8): boolean;
    /// TOnStringTranslate-compatible callback, e.g. to be assigned to
    // TMvcViewsMustache.OnTranslate or supplied to TSynMustache.Render()
    procedure TranslateString(var English: string);
    /// TOnUtf8Translate-compatible callback
    // - Translated is left '' if the key was not found (i.e. caller fallback)
    procedure TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
      var Translated: RawUtf8);
    /// the associated language of this table
    property Language: TLanguage
      read fLanguage;
    /// the ISO 639-1 text of this language, e.g. 'en' or 'zh'
    property Iso: RawUtf8
      read fIso;
    /// optional FormatDateTime() pattern used by the i18nDateText hook
    // - default '' will use the plain RTL DateToStr() rendering
    // - this pattern is independent from the process locale: '/' and ':' are
    // rendered as such, and are not rewritten into the RTL DateSeparator /
    // TimeSeparator locale globals - e.g. 'yyyy/mm/dd' does render slashes,
    // even on a POSIX/C locale system where DateSeparator is '-'
    property DateFormat: string
      read fDateFormat write fDateFormat;
    /// optional FormatDateTime() pattern used by the i18nDateTimeText hook
    // - default '' will use the plain RTL DateTimeToStr() rendering
    // - this pattern is independent from the process locale: '/' and ':' are
    // rendered as such, and are not rewritten into the RTL DateSeparator /
    // TimeSeparator locale globals - e.g. 'yyyy/mm/dd hh:nn' does render
    // slashes and colons, whatever the system locale is
    property DateTimeFormat: string
      read fDateTimeFormat write fDateTimeFormat;
    /// how many translation pairs are stored in this table
    function Count: integer;
  end;


{ ************* TSynLanguages Registry with per-thread Language }

  /// a dynamic array of TLanguage values
  // - as returned e.g. by TSynLanguages.LoadedLanguages
  TLanguageDynArray = array of TLanguage;

  /// registry of TSynLanguage tables with per-thread language selection
  // - typical web usage: load the tables once at startup, then call
  // TSynLanguages.SetThreadLanguage() at each request start (e.g. from an
  // URI parameter or a cookie), and assign TranslateString to the Mustache
  // views engine (e.g. TMvcViewsMustache.OnTranslate)
  TSynLanguages = class(TSynPersistent)
  protected
    fLang: array[TLanguage] of TSynLanguage; // owned instances
    fDefaultLanguage: TLanguage;
    function GetLanguage(aLanguage: TLanguage): TSynLanguage;
  public
    /// finalize the registry and all its owned tables
    destructor Destroy; override;
    /// get or create the translation table of a given language
    property Language[aLanguage: TLanguage]: TSynLanguage
      read GetLanguage;
    /// return the table of a language, nil if none was loaded
    function Find(aLanguage: TLanguage): TSynLanguage;
    /// return the table matching an ISO 639-1 text, e.g. 'fr' - nil if none
    function FindIso(const Iso: RawUtf8): TSynLanguage;
    /// load all <iso>.<ext> files from a folder, e.g. en.json or zh.po
    // - the file name (without its extension) is the ISO 639-1 language text,
    // and any extension supported by TSynLanguage.AddFromFile is recognized
    // - returns the number of recognized language files
    function LoadFromFolder(const Folder: TFileName): integer;
    /// return the languages currently loaded in this registry
    // - i.e. those for which a TSynLanguage table does exist, in TLanguage
    // enumerate order - void if nothing was loaded yet
    // - e.g. to fill a language selection list in the User Interface
    function LoadedLanguages: TLanguageDynArray;
    /// language used when no per-thread language was set
    // - equals lngUndefined by default, i.e. no translation at all
    property DefaultLanguage: TLanguage
      read fDefaultLanguage write fDefaultLanguage;
    /// set the language of the current thread, e.g. at HTTP request start
    // - setting lngUndefined would fallback to DefaultLanguage
    class procedure SetThreadLanguage(aLanguage: TLanguage);
    /// the language of the current thread, as set by SetThreadLanguage
    class function ThreadLanguage: TLanguage;
    /// the table effective for the current thread
    // - i.e. the thread language table, or the DefaultLanguage table, or nil
    function Current: TSynLanguage;
    /// translate the supplied text using the current thread language
    function Translate(var Text: RawUtf8): boolean;
    /// TOnStringTranslate-compatible callback using the thread language
    // - to be assigned e.g. to TMvcViewsMustache.OnTranslate
    procedure TranslateString(var English: string);
    /// TOnUtf8Translate-compatible callback using the thread language
    procedure TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
      var Translated: RawUtf8);
    /// wire this instance to the global framework translation hooks
    // - set self as the main I18n instance, and assign the global
    // LoadResStringTranslate slot - which translates the GetCaptionFrom*
    // family of this framework, not the RTL resourcestring loading - and the
    // i18nDateText / i18nDateTimeText slots, using each language optional
    // DateFormat / DateTimeFormat patterns
    // - those two date/time slots are rendering-only: the supplied value is
    // formatted as such, with no time zone conversion at all
    procedure SetGlobal;
    /// translate all resourcestring of this executable using the current
    // thread language
    // - on FPC, the resourcestring values are stored in a per-unit writable
    // table, which this method rewrites via the objpas.SetResourceStrings()
    // official API, using the original English text as translation key
    // - the previous values are restored first, so switching the language at
    // runtime is safe - and calling it with no language set would just reset
    // the executable to its original English text
    // - warning: the effect is process-wide and this method is NOT thread-safe
    // - only the language to apply is read from the calling thread: the tables
    // it rewrites are global to the process, and are patched with no lock at
    // all, so any concurrent thread reading a resourcestring could race - call
    // it at startup, or when no other thread is using those texts
    // - the translations are stored with an explicit CP_UTF8 header, so are
    // consumed losslessly by Format() or any string concatenation, as long as
    // mormot.core.os did set DefaultSystemCodePage := CP_UTF8 on FPC
    // - do nothing on Delphi, which stores its resourcestring within the
    // executable resources, with no such writable runtime table: use the
    // Mustache {{"text}} channel or the caption hook instead
    procedure TranslateResourceStrings;
  end;

/// the main TSynLanguages instance, as set by TSynLanguages.SetGlobal
// - nil if no SetGlobal call was made
function I18n: TSynLanguages;


implementation


{ ************* TSynLanguage per-language Translation Table }

function LoadUtf8File(const FileName: TFileName): RawUtf8;
begin
  result := RawUtf8(StringFromFile(FileName));
  if (length(result) >= 3) and
     (PCardinal(result)^ and $00ffffff = BOM_UTF8) then
    delete(result, 1, 3); // ignore any UTF-8 BOM
end;

const
  /// the file extensions recognized by TSynLanguage.AddFromFile()
  LANGUAGE_EXT: array[0 .. 8] of TFileName = (
    'po',                                 // 0     = GNU gettext
    'ini', 'msg',                         // 1, 2  = INI-like
    'yaml', 'yml',                        // 3, 4  = YAML
    'json', 'jsonc', 'json5', 'hjson');   // 5..8  = JSON and relaxed variants

/// recognize the translation file format from its extension, -1 if unsupported
function LanguageFileFormat(const FileName: TFileName): PtrInt;
begin
  result := SameExt(FileName, LANGUAGE_EXT, {withoutdot=}true);
end;

type
  /// which .po text slot is currently filled by TSynLanguage.AddFromPo()
  TPoSlot = (
    poNone,
    poId,
    poStr);

/// decode one .po "quoted text" and append its content to Text
// - P is expected to point to the initial '"' - any other input is ignored
// - reuse the JSON_UNESCAPE[] lookup table for the \n \t \r \b \f \\ \" C-like
// escapes, since JSON and .po do share the very same syntax for those - and
// any other escape (e.g. \u which is JSON-specific, or \x) is left as its
// escaped character, as expected by the .po format
procedure PoUnescapeAppend(P: PUtf8Char; var Text: RawUtf8);
var
  c: AnsiChar;
  n: PtrInt;
  d: PUtf8Char;
  tmp: TSynTempBuffer;
begin
  if (P = nil) or
     (P^ <> '"') then
    exit; // no quoted text on this line
  inc(P);
  n := GetLineSize(P, nil); // unescaping can only shrink: this is the maximum
  if n = 0 then
    exit;
  d := tmp.Init(n);
  repeat
    c := P^;
    if (c = '"') or  // normal ending of this quoted text
       (c = #0) or
       (c = #10) or
       (c = #13) then // tolerate any unterminated line
      break;
    if c = '\' then
    begin
      inc(P);
      c := P^;
      if c = #0 then
        break; // pending backslash at the very end of the input
      if JSON_UNESCAPE[c] > JSON_UNESCAPE_UTF16 then
        c := JSON_UNESCAPE[c]; // #0 = non-ASCII and #1 = \u keep c as it is
    end;
    d^ := c;
    inc(d);
    inc(P);
  until false;
  Append(Text, tmp.buf, d - PUtf8Char(tmp.buf));
  tmp.Done;
end;


{ TSynLanguage }

constructor TSynLanguage.Create(aLanguage: TLanguage);
begin
  inherited Create;
  if aLanguage = lngUndefined then
    EI18nException.RaiseUtf8('%.Create(lngUndefined)', [self]);
  fLanguage := aLanguage;
  fIso := LANG_ISO[aLanguage];
  fTexts := TSynDictionary.Create(
    TypeInfo(TRawUtf8DynArray), TypeInfo(TRawUtf8DynArray));
end;

destructor TSynLanguage.Destroy;
begin
  fTexts.Free;
  inherited Destroy;
end;

function TSynLanguage.AddFromVariant(const Doc: TDocVariantData): integer;
var
  i: PtrInt;
  v: RawUtf8;
begin
  result := -1;
  if not Doc.IsObject then
    exit;
  result := 0;
  for i := 0 to Doc.Count - 1 do
  begin
    VariantToUtf8(Doc.Values[i], v);
    fTexts.AddOrUpdate(Doc.Names[i], v);
    inc(result);
  end;
end;

function TSynLanguage.AddFromJson(const Json: RawUtf8): integer;
var
  doc: TDocVariantData;
  normalized: RawUtf8;
begin
  result := -1;
  if Json = '' then
    exit;
  if doc.InitJson(Json, JSON_FAST) then
    result := AddFromVariant(doc)
  else
  begin
    doc.Clear; // release any partially parsed content before trying again
    // JsonBufferReformat() recognizes the JSON5/JSONC/HJson relaxed variants,
    // as JsonSettingsToObject() does for our settings classes
    if JsonBufferReformat(pointer(Json), normalized,
         jsonUnquotedPropNameCompact) and
       doc.InitJson(normalized, JSON_FAST) then
      result := AddFromVariant(doc);
  end;
end;

function TSynLanguage.AddFromJsonFile(const FileName: TFileName): integer;
begin
  result := AddFromJson(LoadUtf8File(FileName));
end;

function TSynLanguage.AddFromYaml(const Yaml: RawUtf8): integer;
var
  doc: TDocVariantData;
begin
  if TryYamlToVariant(Yaml, doc, JSON_FAST) then
    result := AddFromVariant(doc)
  else
    result := -1;
end;

function TSynLanguage.AddFromPo(const Po: RawUtf8): integer;
var
  P, L: PUtf8Char;
  id, str: RawUtf8;
  slot: TPoSlot;
  skip: boolean;

  procedure Flush; // store any pending entry, then reset the parsing state
  begin
    if not skip and
       (id <> '') and    // ignore the void msgid header entry
       (str <> '') then  // ignore any untranslated entry
    begin
      fTexts.AddOrUpdate(id, str);
      inc(result);
    end;
    FastAssignNew(id);
    FastAssignNew(str);
    slot := poNone;
    skip := false;
  end;

begin
  result := 0;
  slot := poNone;
  skip := false;
  P := pointer(Po);
  if P = nil then
    exit;
  repeat
    L := GotoNextNotSpaceSameLine(P); // tolerate any indentation
    case L^ of
      '#': // a comment always introduces the next entry
        begin
          if (id <> '') or
             (str <> '') then
            Flush;
          if (L[1] = ',') and
             GetLineContains(L + 2, nil, 'FUZZY') then
            skip := true; // fuzzy = pending human review, so unusable as such
        end;
      '"': // continuation of the current multi-line msgid/msgstr text
        case slot of
          poId:
            PoUnescapeAppend(L, id);
          poStr:
            PoUnescapeAppend(L, str);
        end;
      'm',
      'M':
        case IdemPCharSep(L, 'MSGID_PLURAL|MSGSTR[|MSGCTXT|MSGID|MSGSTR|') of
          0,   // msgid_plural "..."
          1:   // msgstr[0] "..."
            begin
              skip := true; // plural forms are not supported yet
              slot := poNone;
            end;
          2:   // msgctxt "..." introduces the next entry
            begin
              if (id <> '') or
                 (str <> '') then
                Flush;
              skip := true; // context disambiguation is not supported yet
              slot := poNone;
            end;
          3:   // msgid "..."
            begin
              if (id <> '') or
                 (str <> '') then
                Flush; // previous entry is done: store it before starting anew
              slot := poId;
              PoUnescapeAppend(GotoNextNotSpaceSameLine(L + 5), id);
            end;
          4:   // msgstr "..."
            begin
              slot := poStr;
              PoUnescapeAppend(GotoNextNotSpaceSameLine(L + 6), str);
            end;
        end;
    end;
    P := GotoNextLine(P);
  until P = nil;
  Flush; // store the last pending entry
end;

function TSynLanguage.AddFromPoFile(const FileName: TFileName): integer;
begin
  result := AddFromPo(LoadUtf8File(FileName));
end;

function TSynLanguage.AddFromIni(const Ini, Section: RawUtf8): integer;
var
  P, L, V, E: PUtf8Char;
  key, value: RawUtf8;
  up: TByteToAnsiChar;
begin
  result := 0;
  P := pointer(Ini);
  if P = nil then
    exit;
  if Section <> '' then
  begin
    PWord(UpperCopy255(@up, Section))^ := ord(']'); // e.g. 'FR]'#0
    if not FindSectionFirstLine(P, @up) then
      exit; // no such [Section] in this content
  end;
  repeat
    L := GotoNextNotSpaceSameLine(P); // tolerate any indentation
    if L^ = '[' then
    begin
      if Section <> '' then
        break; // end of the requested [Section]
      // no Section: just ignore this header line and continue
    end
    else if not (L^ in [#0, #10, #13, ';', '#']) then
    begin
      // this line is not void nor a comment: search for its 'key=value' pair
      V := L;
      while not (V^ in [#0, #10, #13, '=']) do
        inc(V);
      if V^ = '=' then
      begin
        E := V; // trim any blank before '='
        while (E > L) and
              (E[-1] in [#9, ' ']) do
          dec(E);
        FastSetString(key, L, E - L);
        V := GotoNextNotSpaceSameLine(V + 1); // trim any blank after '='
        E := V;
        while not (E^ in [#0, #10, #13]) do // stop before any trailing CRLF
          inc(E);
        while (E > V) and
              (E[-1] in [#9, ' ']) do
          dec(E); // trim any blank at the end of this line
        FastSetString(value, V, E - V);
        if (key <> '') and
           (value <> '') then
        begin
          fTexts.AddOrUpdate(key, value);
          inc(result);
        end;
      end;
    end;
    P := GotoNextLine(P);
  until P = nil;
end;

function TSynLanguage.AddFromIniFile(const FileName: TFileName;
  const Section: RawUtf8): integer;
begin
  result := AddFromIni(LoadUtf8File(FileName), Section);
end;

function TSynLanguage.AddFromFile(const FileName: TFileName): integer;
begin
  result := -1;
  if not FileExists(FileName) then
    exit;
  case LanguageFileFormat(FileName) of
    0:      // .po
      result := AddFromPoFile(FileName);
    1, 2:   // .ini .msg
      result := AddFromIniFile(FileName);
    3, 4:   // .yaml .yml
      result := AddFromYaml(LoadUtf8File(FileName));
    5 .. 8: // .json .jsonc .json5 .hjson
      result := AddFromJson(LoadUtf8File(FileName));
  end;
end;

function TSynLanguage.Translate(var Text: RawUtf8): boolean;
var
  tr: RawUtf8;
begin
  result := (Text <> '') and
            fTexts.FindAndCopy(Text, tr, {updatetimeout=}false);
  if result then
    Text := tr;
end;

procedure TSynLanguage.TranslateString(var English: string);
var
  u: RawUtf8;
begin
  StringToUtf8(English, u);
  if Translate(u) then
    English := Utf8ToString(u);
end;

procedure TSynLanguage.TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
  var Translated: RawUtf8);
var
  key: RawUtf8;
begin
  FastSetString(key, English, EnglishLen);
  if not fTexts.FindAndCopy(key, Translated, {updatetimeout=}false) then
    Translated := ''; // caller would fallback to the English text
end;

function TSynLanguage.Count: integer;
begin
  if self = nil then
    result := 0
  else
    result := fTexts.Count;
end;


{ ************* TSynLanguages Registry with per-thread Language }

threadvar
  _ThreadLanguage: TLanguage;

var
  _MainI18n: TSynLanguages;

function I18n: TSynLanguages;
begin
  result := _MainI18n;
end;

procedure _LoadResStringTranslate(var Text: string);
begin
  if _MainI18n <> nil then
    _MainI18n.TranslateString(Text);
end;

// both hooks below are rendering-only slots: the supplied value is already the
// wall clock the caller wants to be displayed - e.g. TTimeLogBits.i18nText
// gives its own TTimeLog bits, just as its unhooked fallback would render them
// - so no time zone math is done here, and the mormot.core.os UtcToLocal /
// LocalToUtc functions are deliberately not called: converting an UTC value
// into local time is the caller responsibility, not this unit business

var
  // the TFormatSettings used by the two hooks below, filled at initialization
  // - the RTL FormatDateTime() does not render '/' and ':' as such: it rewrites
  // them into the DateSeparator / TimeSeparator fields of the supplied settings,
  // which default to process-wide locale globals - e.g. a POSIX/C locale gives
  // DateSeparator = '-', so 'yyyy/mm/dd' would render as '2026-07-31'
  // - we do promise a per-language date layout, so the pattern can't be silently
  // rewritten by whatever locale the process happens to run with: mapping both
  // separators to themselves makes '/' and ':' literal, as '-' or '.' already are
  // - all other fields (month/day names, AM/PM, TwoDigitYearCenturyWindow...) are
  // copied from the RTL defaults, so e.g. 'mmm' or 'ampm' still behave as usual
  _I18nFormat: TFormatSettings;

function _I18nDateTimeText(const DateTime: TDateTime): string;
var
  lang: TSynLanguage;
begin
  lang := nil;
  if _MainI18n <> nil then
    lang := _MainI18n.Current;
  if (lang <> nil) and
     (lang.DateTimeFormat <> '') then
    result := FormatDateTime(lang.DateTimeFormat, DateTime, _I18nFormat)
  else
    result := DateTimeToStr(DateTime);
end;

function _I18nDateText(const Iso: TTimeLog): string;
var
  lang: TSynLanguage;
  dt: TDateTime;
begin
  dt := PTimeLogBits(@Iso)^.ToDateTime;
  lang := nil;
  if _MainI18n <> nil then
    lang := _MainI18n.Current;
  if (lang <> nil) and
     (lang.DateFormat <> '') then
    result := FormatDateTime(lang.DateFormat, dt, _I18nFormat)
  else
    result := DateToStr(dt);
end;

{$ifdef FPC}

// objpas.TResourceIterator callback, with arg = the TSynLanguage table to apply
// - arg may be nil, i.e. no language: every entry keeps its DefaultValue
// - note that objpas is part of the units implicitly available in objfpc/delphi
// modes, so needs no explicit uses clause entry
function _TranslateResourceString(Name, Value: AnsiString; Hash: LongInt;
  arg: pointer): AnsiString;
var
  u: RawUtf8;
begin
  result := ''; // a void result keeps the current value untouched
  if arg = nil then
    exit; // no language: keep the English text restored by ResetResourceTables
  // Value is the original English DefaultValue, as supplied by the RTL: get our
  // RawUtf8 key via the framework unknown code page AnsiString conversion
  AnyAnsiToUtf8Var(Value, u);
  if not TSynLanguage(arg).Translate(u) then
    exit; // unknown text: fallback to the English text
  // return those UTF-8 bytes as such, with an explicit CP_UTF8 header: the RTL
  // just stores this AnsiString into the table CurrentValue field, as-is
  FastSetStringCP(result, pointer(u), length(u), CP_UTF8);
end;

{$endif FPC}

{ TSynLanguages }

destructor TSynLanguages.Destroy;
var
  l: TLanguage;
begin
  if _MainI18n = self then
  begin
    // unhook the global slots pointing to this instance
    _MainI18n := nil;
    LoadResStringTranslate := nil;
    i18nDateText := nil;
    i18nDateTimeText := nil;
  end;
  for l := low(fLang) to high(fLang) do
    fLang[l].Free;
  inherited Destroy;
end;

function TSynLanguages.GetLanguage(aLanguage: TLanguage): TSynLanguage;
begin
  if aLanguage = lngUndefined then
    EI18nException.RaiseUtf8('%.Language[lngUndefined]', [self]);
  result := fLang[aLanguage];
  if result = nil then
  begin
    result := TSynLanguage.Create(aLanguage);
    fLang[aLanguage] := result;
  end;
end;

function TSynLanguages.Find(aLanguage: TLanguage): TSynLanguage;
begin
  if self = nil then
    result := nil
  else
    result := fLang[aLanguage];
end;

function TSynLanguages.FindIso(const Iso: RawUtf8): TSynLanguage;
begin
  result := Find(IsoTextToLanguage(Iso));
end;

function TSynLanguages.LoadFromFolder(const Folder: TFileName): integer;
var
  sr: TSearchRec;
  lng: TLanguage;
  dir, fn: TFileName;
begin
  result := 0;
  dir := IncludeTrailingPathDelimiter(Folder);
  if FindFirst(dir + FILES_ALL, faAnyFile, sr) = 0 then
  begin
    repeat
      if SearchRecValidFile(sr) and
         (LanguageFileFormat(sr.Name) >= 0) then // e.g. ignore any .txt file
      begin
        fn := GetFileNameWithoutExt(sr.Name);
        lng := IsoTextToLanguage(StringToUtf8(fn));
        if lng <> lngUndefined then
        begin
          Language[lng].AddFromFile(dir + sr.Name);
          inc(result);
        end;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

function TSynLanguages.LoadedLanguages: TLanguageDynArray;
var
  l: TLanguage;
  n: PtrInt;
begin
  SetLength(result, length(fLang)); // single allocation, then truncate below
  n := 0;
  for l := low(fLang) to high(fLang) do
    if fLang[l] <> nil then
    begin
      result[n] := l;
      inc(n);
    end;
  SetLength(result, n);
end;

class procedure TSynLanguages.SetThreadLanguage(aLanguage: TLanguage);
begin
  _ThreadLanguage := aLanguage;
end;

class function TSynLanguages.ThreadLanguage: TLanguage;
begin
  result := _ThreadLanguage;
end;

function TSynLanguages.Current: TSynLanguage;
var
  lng: TLanguage;
begin
  lng := _ThreadLanguage;
  if lng = lngUndefined then
    lng := fDefaultLanguage;
  if lng = lngUndefined then
    result := nil
  else
    result := fLang[lng];
end;

function TSynLanguages.Translate(var Text: RawUtf8): boolean;
var
  lang: TSynLanguage;
begin
  lang := Current;
  result := (lang <> nil) and
            lang.Translate(Text);
end;

procedure TSynLanguages.TranslateString(var English: string);
var
  lang: TSynLanguage;
begin
  lang := Current;
  if lang <> nil then
    lang.TranslateString(English);
end;

procedure TSynLanguages.TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
  var Translated: RawUtf8);
var
  lang: TSynLanguage;
begin
  lang := Current;
  if lang = nil then
    Translated := ''
  else
    lang.TranslateUtf8(English, EnglishLen, Translated);
end;

procedure TSynLanguages.SetGlobal;
begin
  _MainI18n := self;
  LoadResStringTranslate := _LoadResStringTranslate;
  i18nDateText := _I18nDateText;
  i18nDateTimeText := _I18nDateTimeText;
end;

procedure TSynLanguages.TranslateResourceStrings;
begin
  {$ifdef FPC}
  // restore the English DefaultValue of every entry first: any text which the
  // new language does not translate would otherwise keep its previous value
  ResetResourceTables;
  // SetResourceStrings() is called even with no language (i.e. a nil arg, which
  // the callback maps to "keep the English text"), because it is the only RTL
  // entry point ending with UpdateResourceStringRefs: ResetResourceTables alone
  // would leave any "var s: string = SomeResourceString" global out of sync
  SetResourceStrings(@_TranslateResourceString, Current); // thread language
  {$endif FPC}
end;


initialization
  // start from the RTL locale settings, to keep its month/day names and AM/PM
  {$ifdef FPC}
  _I18nFormat := DefaultFormatSettings; // FPC RTL global
  {$else}
  _I18nFormat := SysUtils.FormatSettings; // Delphi RTL global
  {$endif FPC}
  // then make the '/' and ':' pattern characters render as themselves
  _I18nFormat.DateSeparator := '/';
  _I18nFormat.TimeSeparator := ':';

end.
