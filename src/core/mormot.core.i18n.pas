/// Framework Core Basic Internationalization (i18n) Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.i18n;

{
  *****************************************************************************

   Basic Internationalization (i18n) Support
    - TLanguageFile per-language translation table
    - TLanguageFiles .m18n multi-tables with per-thread language selection
    - Global wiring of the framework translation hooks

   Translation tables map the original English text to its translation, and
   are loaded from .po and its compiled .mo binary (GNU gettext) as the main
   formats - .ini, .yaml and .json, with its relaxed JSON5 / JSONC / HJson
   variants, are also supported.

   All TLanguageFiles tables could be persisted (as compressed binary) into
   a .m18n file or as executable resource using TObjectStore methods.

   Once loaded, three wiring channels are available: the TSynMustache
   translate tag views channel, the LoadResStringTranslate slot consumed by
   the GetCaptionFrom* captions, and the whole executable resourcestring table
   via TLanguageFiles.TranslateResourceStrings.

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
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.fmt;


{ ************* TLanguageFile per-language Translation Table }

type
  /// exception raised on invalid i18n definition or process
  EI18nException = class(ESynException);

  /// implement one translation table for a given language
  // - table keys are expected to be the original English text, following the
  // mORMot 1 mORMoti18n.pas semantic and the Mustache {{"English text}}
  // convention - so a missing key just fallbacks to the original English
  // - thread-safe access via an internal TSynDictionary
  TLanguageFile = class(TSynPersistent)
  protected
    fIso: RawUtf8;
    fTexts: TSynDictionary; // RawUtf8 (english/key) -> RawUtf8 (translated)
    fLanguage: TLanguage;
    fDateFormat, fDateTimeFormat: string;
    fDateTimeSettings: TFormatSettings;
    procedure DoTranslateString(var English: string);
    function GetCount: integer;
  public
    /// initialize the table for a given language
    constructor Create(aLanguage: TLanguage); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// merge translations as UTF-8 text pairs
    function Add(const EnglishTranslatedPairs: array of RawUtf8): integer;
    /// merge translations from a file, recognized by its extension
    // - .po or .mo (gettext), .ini or .msg, .yaml or .yml, .json / .jsonc /
    // .json5 / .hjson (the relaxed JSON variants are read by our JSON parser)
    // - returns -1 if the file does not exist or its extension is unknown
    function AddFromFile(const FileName: TFileName): integer;
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
    /// merge translations from GNU gettext .mo binary content
    // - .mo is the binary format compiled from a .po source by msgfmt: both
    // hold the very same msgid/msgstr pairs, so this method is just the
    // binary counterpart of AddFromPo()
    // - .mo files generated on a reverse endianness machine are supported
    // - the void msgid header entry and untranslated (void msgstr) entries
    // are just ignored, as AddFromPo() does - and msgfmt does not include
    // any fuzzy entry in its .mo output, unless --use-fuzzy is specified
    // - non-goals, as for AddFromPo(): msgctxt disambiguation, and the
    // msgid_plural / msgstr[] plural forms - such entries are just ignored
    // - the strings are expected to be UTF-8 encoded, as declared by the
    // header of any modern .mo file, and as AddFromPo() does for its source
    // - the whole input is validated before any merge, so that an invalid
    // .mo content is rejected as a whole, and never merges anything
    // - returns the number of added or replaced translations, or -1 if the
    // supplied content is no valid .mo binary
    function AddFromMo(const Mo: RawByteString): integer;
    /// merge translations from a GNU gettext .mo binary file
    // - returns the number of added or replaced translations, or -1 if this
    // file does not exist or is no valid .mo binary
    function AddFromMoFile(const FileName: TFileName): integer;
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
    /// translate the supplied text in-place
    // - returns true if the key was found and Text was replaced
    // - returns false and leaves Text untouched (fallback to original text)
    function Translate(var Text: RawUtf8): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// TOnStringTranslate-compatible callback, e.g. to be assigned to
    // TMvcViewsMustache.OnTranslate or supplied to TSynMustache.Render()
    procedure TranslateString(var English: string);
      {$ifdef HASINLINE} inline; {$endif}
    /// TOnUtf8Translate-compatible callback
    // - Translated is left '' if the key was not found (i.e. caller fallback)
    procedure TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
      var Translated: RawUtf8);
    /// persist all translations as a compressed binary blob
    function SaveToBinary: RawByteString;
    /// retrieve all translations from SaveToBinary() compressed binary blob
    function LoadFromBinary(const Binary: RawByteString): boolean;
    /// low-level access to the actual raw TSynDictionary storage
    // - mapped as RawUtf8 English keys into RawUtf8 translated values
    // - call e.g. Texts.DeleteAll to reset all translations
    property Texts: TSynDictionary
      read fTexts;
    /// optional settings used with DateFormat/DateTimeFormat patterns
    // - default value is independent from the process locale: '/' and ':' are
    // rendered as such, and are not rewritten into the RTL DateSeparator /
    // TimeSeparator locale globals - e.g. 'yyyy/mm/dd hh:nn' does render
    // slashes and colons, whatever the system locale is
    property DateTimeSettings: TFormatSettings
      read fDateTimeSettings write fDateTimeSettings;
  published
    /// the associated language of this table
    property Language: TLanguage
      read fLanguage;
    /// the ISO 639-1 text of this language, e.g. 'en' or 'zh'
    property Iso: RawUtf8
      read fIso;
    /// optional FormatDateTime() pattern used by the i18nDateText hook
    // - default '' will use the plain RTL DateToStr() rendering
    property DateFormat: string
      read fDateFormat write fDateFormat;
    /// optional FormatDateTime() pattern used by the i18nDateTimeText hook
    // - default '' will use the plain RTL DateTimeToStr() rendering
    property DateTimeFormat: string
      read fDateTimeFormat write fDateTimeFormat;
    /// how many translation pairs are stored in this table
    property Count: integer
      read GetCount;
  end;


{ ************* TLanguageFiles .m18n multi-tables with per-thread Language }

type
  /// a dynamic array of TLanguage values
  // - as returned e.g. by TLanguageFiles.LoadedLanguages
  TLanguageDynArray = array of TLanguage;

  /// a O(1) storage of per-TLanguage TLanguageFile instances
  TLanguageFilePerLang = array[TLanguage] of TLanguageFile;

  /// "m18n" set of TLanguageFile tables with per-thread language selection
  // - typical web usage: load the tables once at startup, then call
  // TLanguageFiles.SetThreadLanguage() at each request start (e.g. from an
  // URI parameter or a cookie), and assign TranslateString to the Mustache
  // views engine (e.g. TMvcViewsMustache.OnTranslate)
  // - inherits from TObjectStore so could be persisted as file or executable
  // resource - our canonical binary file extension is .m18n
  TLanguageFiles = class(TObjectStore)
  protected
    fDefaultLanguage: TLanguage;
    fLangCount: integer;
    fLoadedLanguages: TLanguageDynArray; // cache
    fLang: TLanguageFilePerLang;         // main O(1) lookup array
    /// low-level virtual methods implementing the .m18n binary persistence
    procedure LoadFromReader; override;
    procedure SaveToWriter(aWriter: TBufferWriter); override;
  public
    /// finalize the instance and all its owned tables
    destructor Destroy; override;

    /// return the table matching an ISO 639-1 text, e.g. 'fr' - nil if none
    function FindIso(const Iso: RawUtf8): TLanguageFile;
    /// get or create the translation table of a given language
    // - mostly for internal use, e.g. AddFrom*() methods
    function FindOrNew(aLanguage: TLanguage): TLanguageFile;

    /// merge translations of a given language as UTF-8 text pairs
    function Add(aLanguage: TLanguage;
      const EnglishTranslatedPairs: array of RawUtf8): integer;
    /// load all <iso>.<ext> files from a folder, e.g. en.json or zh.po
    // - the file name (without its extension) is the ISO 639-1 language text,
    // and any extension supported by TLanguageFile.AddFromFile is recognized
    // - several files of the same language are merged into its single table,
    // in a deterministic order - by name, then by ascending format index as
    // defined by the LANGUAGE_EXT[] order - so that if they do define the
    // same key, the winner does not depend on the OS enumeration order: in
    // particular, a compiled fr.mo wins over the fr.po source it came from
    // - returns the number of recognized language files
    function AddFromFolder(const Folder: TFileName): integer;
    /// merge translations of a given language from a file, recognized by its extension
    function AddFromFile(aLanguage: TLanguage; const FileName: TFileName): integer;
    /// merge translations of a given language from a TDocVariantData object document
    function AddFromVariant(aLanguage: TLanguage; const Doc: TDocVariantData): integer;
    /// merge translations of a given language from a JSON object
    function AddFromJson(aLanguage: TLanguage; const Json: RawUtf8): integer;
    /// return the languages currently loaded in this instance
    // - i.e. those for which a TLanguageFile table does exist, in TLanguage
    // enumerate order - void if nothing was loaded yet
    // - e.g. to fill a language selection list in the User Interface
    function LoadedLanguages: TLanguageDynArray;

    /// translate the supplied text using aLanguage enumerate
    function Translate(aLanguage: TLanguage; var Text: RawUtf8): boolean; overload;
    /// TOnStringTranslate callback using aLanguage enumerate
    // - to be assigned e.g. to TMvcViewsMustache.OnTranslate
    procedure TranslateString(aLanguage: TLanguage; var English: string); overload;
    /// TOnUtf8Translate  callback using aLanguage enumerate
    // - Translated is left '' if the key was not found (i.e. caller fallback)
    procedure TranslateUtf8(aLanguage: TLanguage; English: PUtf8Char;
      EnglishLen: integer; var Translated: RawUtf8); overload;

    /// set the language of the current thread, e.g. at HTTP request start
    // - setting lngUndefined would fallback to DefaultLanguage
    class procedure SetThreadLanguage(aLanguage: TLanguage);
    /// the language of the current thread, as set by SetThreadLanguage()
    class function ThreadLanguage: TLanguage;
    /// the translation table effective for the current thread
    // - i.e. from the current SetThreadLanguage() or DefaultLanguage, or nil
    function Current: TLanguageFile;
    /// translate the supplied text using SetThreadLanguage() or DefaultLanguage
    function Translate(var Text: RawUtf8): boolean; overload;
    /// TOnStringTranslate callback using SetThreadLanguage() or DefaultLanguage
    // - to be assigned e.g. to TMvcViewsMustache.OnTranslate
    procedure TranslateString(var English: string); overload;
    /// TOnUtf8Translate  callback using SetThreadLanguage() or DefaultLanguage
    procedure TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
      var Translated: RawUtf8); overload;

    /// wire this instance to the global framework translation hooks
    // - set itself as the main I18n instance, and assign the global
    // LoadResStringTranslate slot - which translates the GetCaptionFrom*
    // family of this framework, not the RTL resourcestring loading - and the
    // i18nDateText / i18nDateTimeText slots, using each language optional
    // DateFormat / DateTimeFormat / DateTimeSettings patterns
    // - warning: effect is process-wide and WILL USE ThreadLanguage value
    // in each and every of those i18n shared functions
    procedure SetGlobal;
    /// translate all resourcestring of this executable to the given language
    // - expect original resourcestrings to be English text translation keys
    // - on Delphi, set global LoadResStringFunc or redirect raw LoadResString()
    // maintaining its own efficient cache of translated strings
    // - on FPC, the resourcestring values are stored in a per-unit writable
    // table, which this method rewrites via the objpas.SetResourceStrings()
    // - switching the language at runtime is safe
    // - setting unknown/undefined language would reset to original English
    // - warning: effect is process-wide and WILL NOT USE ThreadLanguage value
    procedure TranslateResourceStrings(aLanguage: TLanguage);

    /// get the current translation table of a given language
    // - may return nil if none - use FindOrNew() or AddFrom*() methods to
    // setup translations on a new language
    property Language: TLanguageFilePerLang
      read fLang;
  published
    /// one optional text identifier, e.g. defining the program and version
    property Name;
    /// language used when no per-thread language was set
    // - equals lngUndefined by default, i.e. no translation at all
    // - this property value is ignored for all Translate*(aLanguage) methods
    property DefaultLanguage: TLanguage
      read fDefaultLanguage write fDefaultLanguage;
    /// how many translation languages are stored in this instance
    property Count: integer
      read fLangCount;
  end;


{ ************* Global wiring of the framework translation hooks }

/// the main TLanguageFiles instance, as set by TLanguageFiles.SetGlobal
// - nil if no SetGlobal call was made
function I18n: TLanguageFiles;


implementation


{ ************* TLanguageFile per-language Translation Table }

const
  /// the file extensions recognized by TLanguageFile.AddFromFile()
  // - this order is also the TLanguageFiles.AddFromFolder() loading order,
  // i.e. the priority of each format: the last one does win, so that a
  // compiled .mo takes precedence over the .po source it was generated from
  LANGUAGE_EXT: array[0 .. 9] of TFileName = (
    'po',                                 // 0     = GNU gettext source
    'ini', 'msg',                         // 1, 2  = INI-like
    'yaml', 'yml',                        // 3, 4  = YAML
    'json', 'jsonc', 'json5', 'hjson',    // 5..8  = JSON and relaxed variants
    'mo');                                // 9     = GNU gettext binary

/// recognize the translation file format from its extension, -1 if unsupported
function LanguageFileFormat(const FileName: TFileName): PtrInt;
begin
  result := SameExt(FileName, LANGUAGE_EXT, {withoutdot=}true);
end;

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

const
  /// the magic number stored at the beginning of any GNU gettext .mo file
  // - is stored in the endianness of the machine which did generate the file
  MO_MAGIC = $950412de;

type
  /// map the fixed-size header of a GNU gettext .mo binary file
  TMoHeader = packed record
    Magic: cardinal;
    Revision: cardinal;  // major shl 16 + minor
    Count: cardinal;     // number of strings
    OrigTab: cardinal;   // offset of the original strings table
    TransTab: cardinal;  // offset of the translated strings table
    HashSize: cardinal;  // the hash table is unused by TLanguageFile.AddFromMo
    HashTab: cardinal;
  end;
  PMoHeader = ^TMoHeader;

  /// map one (length, offset) pair of a GNU gettext .mo string table
  // - the string itself is #0 terminated, and Len does not include it
  TMoEntry = packed record
    Len: cardinal;
    Offset: cardinal;
  end;
  TMoEntryArray = array[0 .. (MaxInt div SizeOf(TMoEntry)) - 1] of TMoEntry;
  PMoEntryArray = ^TMoEntryArray;

{$ifdef FPC}

// objpas.TResourceIterator callback, with arg = the TLanguageFile table to apply
// - arg may be nil, i.e. no language: every entry keeps its DefaultValue
// - note that objpas is part of the units implicitly available in objfpc/delphi
// modes, so needs no explicit uses clause entry
function _TranslateResourceString(const Name, Value: AnsiString; Hash: LongInt;
  arg: pointer): AnsiString;
begin
  result := ''; // a void result keeps the current value untouched (no language)
  if (arg <> nil) and
     (PClass(arg)^ = TLanguageFile) then
    if Unicode_CodePage = CP_UTF8 then // always the case with Lazarus
      TLanguageFile(arg).fTexts.FindAndCopy(Value, result, {updtimeout=}false)
    else
    begin
      result := Value;
      TLanguageFile(arg).DoTranslateString(result); // AnsiString <> RawUtf8
    end;
end;

{$else}

var
  _LoadResFile: TLanguageFile;   // not SetGlobal/_MainI18n.Current
  _LoadResCache: TSynDictionary; // efficient thread-safe cache

function _LoadResString(ResStringRec: PResStringRec): string;
begin
  if _LoadResCache <> nil then
    if _LoadResCache.FindAndCopy(ResStringRec, result, false) then
      exit;
  OsLoadResString(ResStringRec, result);
  _LoadResFile.TranslateString(result);
  if _LoadResCache <> nil then
    _LoadResCache.Add(ResStringRec, result);
end;

{$endif FPC}

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
  _I18nDefaultFormatSettings: TFormatSettings;


{ TLanguageFile }

constructor TLanguageFile.Create(aLanguage: TLanguage);
begin
  inherited Create;
  if aLanguage = lngUndefined then
    EI18nException.RaiseUtf8('%.Create(lngUndefined)', [self]);
  fLanguage := aLanguage;
  fIso := LANG_ISO[aLanguage];
  fTexts := TSynDictionary.Create(
    TypeInfo(TRawUtf8DynArray), TypeInfo(TRawUtf8DynArray));
  fTexts.ThreadUse := uRWLock; // non-blocking thread-safe Translate()
  fDateTimeSettings := _I18nDefaultFormatSettings;
end;

destructor TLanguageFile.Destroy;
begin
  {$ifdef ISDELPHI}
  if _LoadResFile = self then
    _LoadResFile := nil;
  {$endif ISDELPHI}
  fTexts.Free;
  inherited Destroy;
end;

function TLanguageFile.Add(const EnglishTranslatedPairs: array of RawUtf8): integer;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to high(EnglishTranslatedPairs) shr 1 do
      if fTexts.AddOrUpdate(EnglishTranslatedPairs[i * 2],
                            EnglishTranslatedPairs[i * 2 + 1]) >= 0 then
        inc(result);
end;

function TLanguageFile.AddFromVariant(const Doc: TDocVariantData): integer;
var
  i: PtrInt;
  v: RawUtf8;
begin
  result := -1;
  if (self = nil) or
     not Doc.IsObject then
    exit;
  result := 0;
  for i := 0 to Doc.Count - 1 do
  begin
    VariantToUtf8(Doc.Values[i], v);
    fTexts.AddOrUpdate(Doc.Names[i], v);
    inc(result);
  end;
end;

function TLanguageFile.AddFromJson(const Json: RawUtf8): integer;
var
  doc: TDocVariantData;
  normalized: RawUtf8; // local normalized copy is parsed in-place
begin
  result := -1;
  if (self <> nil) and
     JsonBufferReformat(pointer(Json), normalized, jsonUnquotedPropNameCompact) and
     (doc.InitJsonInPlace(pointer(normalized), JSON_FAST) <> nil) then
    result := AddFromVariant(doc);
end;

function TLanguageFile.AddFromJsonFile(const FileName: TFileName): integer;
begin
  result := AddFromJson(RawUtf8FromFile(FileName));
end;

function TLanguageFile.AddFromYaml(const Yaml: RawUtf8): integer;
var
  doc: TDocVariantData;
begin
  if (self <> nil) and
     TryYamlToVariant(Yaml, doc, JSON_FAST) then
    result := AddFromVariant(doc)
  else
    result := -1;
end;

function TLanguageFile.AddFromPo(const Po: RawUtf8): integer;
var
  P, L: PUtf8Char;
  id, str: RawUtf8;
  slot: (poNone, poId, poStr); // which .po text slot is currently filled
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
  if (self = nil) or
     (P = nil) then
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

function TLanguageFile.AddFromPoFile(const FileName: TFileName): integer;
begin
  result := AddFromPo(RawUtf8FromFile(FileName));
end;

function TLanguageFile.AddFromMo(const Mo: RawByteString): integer;
var
  h: PMoHeader;
  o, t: PMoEntryArray;
  len, n, ot, tt, rev, tab: PtrUInt;
  i, cnt: PtrInt;
  swap: boolean;
  id, str: RawUtf8;

  function Get(const e: TMoEntry; Text: PRawUtf8): boolean;
  var
    l, ofs: PtrUInt;
  begin
    l := e.Len;
    ofs := e.Offset;
    if swap then
    begin
      l := bswap32(l);
      ofs := bswap32(ofs);
    end;
    result := (l < len) and      // the #0 terminator is part of the content
              (ofs < len - l);   // unsigned arithmetic: no overflow possible
    if result and
       (Text <> nil) then
      FastSetString(Text^, @PByteArray(Mo)[ofs], PtrInt(l));
  end;

begin
  result := -1;
  len := PtrUInt(length(Mo));
  if (self = nil) or
     (len < SizeOf(TMoHeader)) then
    exit; // clearly not a .mo binary file
  h := pointer(Mo);
  swap := h^.Magic <> MO_MAGIC;
  if swap and
     (bswap32(h^.Magic) <> MO_MAGIC) then
    exit; // invalid magic number
  n := h^.Count;
  ot := h^.OrigTab;
  tt := h^.TransTab;
  rev := h^.Revision;
  if swap then // this .mo was generated on a reverse endianness machine
  begin
    n := bswap32(n);
    ot := bswap32(ot);
    tt := bswap32(tt);
    rev := bswap32(rev);
  end;
  if rev shr 16 > 1 then
    exit; // unsupported major .mo format revision
  if n > len div SizeOf(TMoEntry) then
    exit; // more entries than this content could ever store
  tab := n * SizeOf(TMoEntry); // size of each strings table, in bytes
  if (ot > len - tab) or
     (tt > len - tab) then
    exit; // any strings table is out of range
  o := @PByteArray(Mo)[ot];
  t := @PByteArray(Mo)[tt];
  cnt := n; // n was cropped above, so it does fit in a PtrInt
  for i := 0 to cnt - 1 do // validate first: an invalid .mo merges nothing
    if not Get(o^[i], nil) or
       not Get(t^[i], nil) then
      exit;
  result := 0;
  for i := 0 to cnt - 1 do
  begin
    Get(o^[i], @id);
    if (id = '') or                  // ignore the void msgid header entry
       (PosExChar(#0, id) <> 0) or   // msgid + #0 + msgid_plural
       (PosExChar(#4, id) <> 0) then // msgctxt + #4 + msgid
      continue;
    Get(t^[i], @str);
    if str = '' then // ignore any untranslated entry
      continue;
    fTexts.AddOrUpdate(id, str);
    inc(result);
  end;
end;

function TLanguageFile.AddFromMoFile(const FileName: TFileName): integer;
begin
  result := AddFromMo(StringFromFile(FileName)); // binary: no LoadUtf8File()
end;

function TLanguageFile.AddFromIni(const Ini, Section: RawUtf8): integer;
var
  P, L, V, E: PUtf8Char;
  key, value: RawUtf8;
  up: TByteToAnsiChar;
begin
  result := 0;
  P := pointer(Ini);
  if (self = nil) or
     (P = nil) then
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

function TLanguageFile.AddFromIniFile(const FileName: TFileName;
  const Section: RawUtf8): integer;
begin
  result := AddFromIni(RawUtf8FromFile(FileName), Section);
end;

function TLanguageFile.AddFromFile(const FileName: TFileName): integer;
begin
  result := -1;
  if (self <> nil) and
     FileExists(FileName) then
    case LanguageFileFormat(FileName) of
      0:      // .po
        result := AddFromPoFile(FileName);
      1, 2:   // .ini .msg
        result := AddFromIniFile(FileName);
      3, 4:   // .yaml .yml
        result := AddFromYaml(RawUtf8FromFile(FileName));
      5 .. 8: // .json .jsonc .json5 .hjson
        result := AddFromJson(RawUtf8FromFile(FileName));
      9:      // .mo
        result := AddFromMoFile(FileName);
    end;
end;

function TLanguageFile.Translate(var Text: RawUtf8): boolean;
begin
  if (self <> nil) and
     (Text <> '') then
    result := fTexts.FindAndCopy(Text, Text, {updtimeout=}false)
  else
    result := false;
end;

procedure TLanguageFile.TranslateString(var English: string);
begin
  if (self <> nil) and
     (English <> '') then
    {$ifdef FPC}
    if Unicode_CodePage = CP_UTF8 then // most common case with Lazarus
      fTexts.FindAndCopy(English, English, {updtimeout=}false)
    else
    {$endif FPC}
      DoTranslateString(English);
end;

procedure TLanguageFile.DoTranslateString(var English: string);
var
  u: RawUtf8; // needed mostly on Delphi with AnsiString/UnicodeString
begin
  StringToUtf8(English, u);
  if fTexts.FindAndCopy(u, u, {updtimeout=}false) then
    Utf8ToStringVar(u, English);
end;

procedure TLanguageFile.TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
  var Translated: RawUtf8);
var
  key: RawUtf8;
begin
  if (self <> nil) and
     (EnglishLen > 0) then
  begin
    FastSetString(key, English, EnglishLen);
    if fTexts.FindAndCopy(key, Translated, {updtimeout=}false) then
      exit;
  end;
  Translated := ''; // caller would fallback to the English text
end;

function TLanguageFile.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fTexts.Count;
end;

function TLanguageFile.SaveToBinary: RawByteString;
begin
  result := fTexts.SaveToBinary; // TSynDictionary layout
end;

function TLanguageFile.LoadFromBinary(const Binary: RawByteString): boolean;
begin
  result := fTexts.LoadFromBinary(Binary);
end;


{ ************* TLanguageFiles .m18n multi-tables  with per-thread Language }

threadvar
  _ThreadLanguage: TLanguage;

var
  _MainI18n: TLanguageFiles;


{ TLanguageFiles }

class procedure TLanguageFiles.SetThreadLanguage(aLanguage: TLanguage);
begin
  _ThreadLanguage := aLanguage;
end;

class function TLanguageFiles.ThreadLanguage: TLanguage;
begin
  result := _ThreadLanguage;
end;

destructor TLanguageFiles.Destroy;
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

function TLanguageFiles.FindOrNew(aLanguage: TLanguage): TLanguageFile;
begin
  if (self = nil) or
     (aLanguage = lngUndefined) then
    EI18nException.RaiseUtf8('%.FindOrNew(lngUndefined)', [self]);
  fSafe.WriteLock; // fLang[] read is atomic but this method needs protection
  try
    result := fLang[aLanguage];
    if result <> nil then
      exit;
    // initialize the TLanguageFile instance for this TLanguage
    result := TLanguageFile.Create(aLanguage);
    fLang[aLanguage] := result;
    fLoadedLanguages := nil; // re-computed when needed
    inc(fLangCount);
  finally
    fSafe.WriteUnLock;
  end;
end;

function TLanguageFiles.FindIso(const Iso: RawUtf8): TLanguageFile;
begin
  if self = nil then
    result := nil
  else
    result := fLang[IsoTextToLanguage(Iso)];
end;

function TLanguageFiles.Add(aLanguage: TLanguage;
  const EnglishTranslatedPairs: array of RawUtf8): integer;
begin
  result := FindOrNew(aLanguage).Add(EnglishTranslatedPairs);
end;

function TLanguageFiles.AddFromFolder(const Folder: TFileName): integer;
var
  sr: TSearchRec;
  lng: TLanguage;
  dir: TFileName;
  files: TFileNameDynArray;
  formats: TByteDynArray;
  da: TDynArray;
  f, i: PtrInt;
  n: integer; // not PtrInt
begin
  result := 0;
  if self = nil then
    exit;
  dir := IncludeTrailingPathDelimiter(Folder);
  // first retrieve all the translation file names available in this folder
  n := 0;
  da.Init(TypeInfo(TFileNameDynArray), files, @n);
  if FindFirst(dir + FILES_ALL, faAnyFile, sr) = 0 then
  begin
    repeat
      if SearchRecValidFile(sr) and
         (LanguageFileFormat(sr.Name) >= 0) then // e.g. ignore any .txt file
        da.Add(sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  if n = 0 then
    exit;
  // merge them by ascending LANGUAGE_EXT[] format index, then by name, so that
  // the result does not depend on the OS folder enumeration order
  da.Sort(SortDynArrayFileName); // sorted by file name, grouped by extension
  SetLength(formats, n);
  for i := 0 to n - 1 do
    formats[i] := LanguageFileFormat(files[i]);
  for f := 0 to high(LANGUAGE_EXT) do
    for i := 0 to n - 1 do
      if formats[i] = f then
      begin
        lng := IsoTextToLanguage(StringToUtf8(GetFileNameWithoutExt(files[i])));
        if lng = lngUndefined then
          continue;
        FindOrNew(lng).AddFromFile(dir + files[i]);
        inc(result);
      end;
end;

function TLanguageFiles.AddFromFile(aLanguage: TLanguage; const FileName: TFileName): integer;
begin
  if self = nil then
    result := -1
  else
    result := FindOrNew(aLanguage).AddFromFile(FileName);
end;

function TLanguageFiles.AddFromVariant(aLanguage: TLanguage; const Doc: TDocVariantData): integer;
begin
  if self = nil then
    result := -1
  else
    result := FindOrNew(aLanguage).AddFromVariant(Doc);
end;

function TLanguageFiles.AddFromJson(aLanguage: TLanguage; const Json: RawUtf8): integer;
begin
  if self = nil then
    result := -1
  else
    result := FindOrNew(aLanguage).AddFromJson(Json);
end;

function TLanguageFiles.LoadedLanguages: TLanguageDynArray;
var
  l: TLanguage;
  n: PtrInt;
begin
  result := fLoadedLanguages;
  if (result <> nil) or
     (fLangCount = 0) then
    exit;
  SetLength(result, fLangCount);
  n := 0;
  for l := low(fLang) to high(fLang) do
    if fLang[l] <> nil then
    begin
      result[n] := l;
      inc(n);
    end;
  if n <> fLangCount then
    EI18nException.RaiseU('LangCount?'); // paranoid
  fLoadedLanguages := result; // set eventually (as atomic pointer)
end;

function TLanguageFiles.Current: TLanguageFile;
begin
  result := pointer(self);
  if self = nil then
    exit;
  result := fLang[_ThreadLanguage];
  if result = nil then
    result := fLang[fDefaultLanguage];
end;

function TLanguageFiles.Translate(var Text: RawUtf8): boolean;
var
  lang: TLanguageFile;
begin
  if self <> nil then
  begin
    lang := fLang[_ThreadLanguage]; // inlined TLanguageFiles.Current
    if lang = nil then
      lang := fLang[fDefaultLanguage];
    result := lang.Translate(Text);
  end
  else
    result := false;
end;

procedure TLanguageFiles.TranslateString(var English: string);
var
  lang: TLanguageFile;
begin
  if self = nil then
    exit;
  lang := fLang[_ThreadLanguage]; // inlined TLanguageFiles.Current
  if lang = nil then
    lang := fLang[fDefaultLanguage];
  lang.TranslateString(English);
end;

procedure TLanguageFiles.TranslateUtf8(English: PUtf8Char; EnglishLen: integer;
  var Translated: RawUtf8);
var
  lang: TLanguageFile;
begin
  lang := nil;
  if self <> nil then
  begin
    lang := fLang[_ThreadLanguage]; // inlined TLanguageFiles.Current
    if lang = nil then
      lang := fLang[fDefaultLanguage];
  end;
  lang.TranslateUtf8(English, EnglishLen, Translated);
end;

function TLanguageFiles.Translate(aLanguage: TLanguage; var Text: RawUtf8): boolean;
begin
  if self <> nil then
    result := fLang[aLanguage].Translate(Text)
  else
    result := false;
end;

procedure TLanguageFiles.TranslateString(aLanguage: TLanguage; var English: string);
begin
  if self <> nil then
    fLang[aLanguage].TranslateString(English);
end;

procedure TLanguageFiles.TranslateUtf8(aLanguage: TLanguage; English: PUtf8Char;
  EnglishLen: integer; var Translated: RawUtf8);
var
  lang: TLanguageFile;
begin
  lang := nil;
  if self <> nil then
    lang := fLang[aLanguage];
  lang.TranslateUtf8(English, EnglishLen, Translated);
end;

procedure TLanguageFiles.TranslateResourceStrings(aLanguage: TLanguage);
begin
  {$ifdef FPC}
  // restore the English DefaultValue of every entry first: any text which the
  // new language does not translate would otherwise keep its previous value
  ResetResourceTables;
  // SetResourceStrings() is called even with no language (i.e. a nil arg, which
  // the callback maps to "keep the English text"), because it is the only RTL
  // entry point ending with UpdateResourceStringRefs: ResetResourceTables alone
  // would leave any "var s: string = SomeResourceString" global out of sync
  SetResourceStrings(@_TranslateResourceString, fLang[aLanguage]);
  {$else}
  {$ifdef HASCACHEDRESSTRING}
  // Delphi 10.4+ sysutils has cache + global LoadResStringFunc hook
  ResStringCleanupCache;                // not mandatory but cleaner
  LoadResStringFunc := @_LoadResString; // replace global helper callback
  {$else}
  // patch once at Intel CPU level to redirect to our function
  if PByte(@System.LoadResString)^ <> $e9 then
    RedirectCode(@System.LoadResString, @_LoadResString);
  {$endif HASCACHEDRESSTRING}
  // initialize or clear resourcestring values cache
  if _LoadResCache = nil then
  begin
    _LoadResCache := TSynDictionary.Create(TypeInfo(TPointerDynArray),
      TypeInfo(TStringDynArray));
    _LoadResCache.ThreadUse := uRWLock; // non-blocking _LoadResString()
  end
  else
     _LoadResCache.DeleteAll;
  // use the supplied TLanguageFile instance
  _LoadResFile := fLang[aLanguage];
  {$endif FPC}
end;

const
  M18N_MAGIC = $4E38316D; // 'm18n' in little endian

procedure TLanguageFiles.LoadFromReader;

  procedure ReadError(msg: PUtf8Char);
  begin
    fReader.ErrorData('%.LoadFromReader failed as %', [self, msg], EI18nException);
  end;

var
  version, n: integer;
  l: TLanguage;
begin
  fSafe.WriteLock;
  try
    if fLangCount <> 0 then
      ReadError('existing data');
    if fReader.Next4 <> M18N_MAGIC then
      ReadError('missing m18n magic');
    version := fReader.NextByte;
    if version <> 0 then
      ReadError('unsupported m18n version');
    inherited LoadFromReader; // fName persistence
    n := fReader.NextByte;
    // first read all English original keys at once
    repeat
      l := TLanguage(fReader.NextByte);
      if l = lngUndefined then
        break;
      if ord(l) > ord(high(l)) then
        ReadError('invalid lang1');
      if fLang[l] <> nil then
        ReadError('duplicated lang');
      FindOrNew(l).Texts.Keys.DynArray^.LoadFromReader(fReader^);
      fLang[l].Texts.Keys.ForceReHash;
    until false;
    if fLangCount <> n then
      ReadError('LangCount mismatch');
    // then read all translations
    repeat
      l := TLanguage(fReader.NextByte);
      if l = lngUndefined then
        break;
      if ord(l) > ord(high(l)) then
        ReadError('invalid lang2');
      if fLang[l] = nil then
        ReadError('missing lang');
      fLang[l].Texts.Values.LoadFromReader(fReader^);
    until false;
    if fReader.Next4 <> M18N_MAGIC then
      ReadError('missing m18n trailer');
  finally
    fSafe.WriteUnlock;
  end;
end;

procedure TLanguageFiles.SaveToWriter(aWriter: TBufferWriter);
var
  l: TLanguage;
  lang: TLanguageFile;
begin
  fSafe.ReadOnlyLock;
  try
    aWriter.Write4(M18N_MAGIC);
    aWriter.Write1(0);               // version 0 of the format
    inherited SaveToWriter(aWriter); // fName persistence
    aWriter.Write1(fLangCount);
    // first append all English original text at once (for better compression)
    for l := low(fLang) to high(fLang) do
    begin
      lang := fLang[l];
      if lang = nil then
        continue;
      aWriter.Write1(ord(lang.Language));
      lang.Texts.Keys.DynArray^.SaveTo(aWriter);
    end;
    aWriter.Write1(ord(lngUndefined)); // end loop
    // then append all translations
    for l := low(fLang) to high(fLang) do
    begin
      lang := fLang[l];
      if lang = nil then
        continue;
      aWriter.Write1(ord(lang.Language));
      lang.Texts.Values.SaveTo(aWriter);
    end;
    aWriter.Write1(ord(lngUndefined)); // end loop
    aWriter.Write4(M18N_MAGIC);
  finally
    fSafe.ReadOnlyUnLock;
  end;
end;


{ ************* Global wiring of the framework translation hooks }

function I18n: TLanguageFiles;
begin
  result := _MainI18n;
end;

procedure _LoadResStringTranslate(var Text: string);
begin
  _MainI18n.TranslateString(Text);
end;

// both hooks below are rendering-only slots: the supplied value is already the
// wall clock the caller wants to be displayed - e.g. TTimeLogBits.i18nText
// gives its own TTimeLog bits, just as its unhooked fallback would render them

function _I18nDateTimeText(const DateTime: TDateTime): string;
var
  lang: TLanguageFile;
begin
  lang := _MainI18n.Current;
  if (lang <> nil) and
     (lang.fDateTimeFormat <> '') then
    result := FormatDateTime(lang.fDateTimeFormat, DateTime, lang.fDateTimeSettings)
  else
    result := DateTimeToStr(DateTime);
end;

function _I18nDateText(const Iso: TTimeLog): string;
var
  lang: TLanguageFile;
  dt: TDateTime;
begin
  dt := PTimeLogBits(@Iso)^.ToDateTime;
  lang := _MainI18n.Current;
  if (lang <> nil) and
     (lang.fDateFormat <> '') then
    result := FormatDateTime(lang.fDateFormat, dt, lang.fDateTimeSettings)
  else
    result := DateToStr(dt);
end;

procedure TLanguageFiles.SetGlobal;
begin
  _MainI18n := self;
  LoadResStringTranslate := _LoadResStringTranslate;
  i18nDateText := _I18nDateText;
  i18nDateTimeText := _I18nDateTimeText;
end;


initialization
  // start from the RTL locale settings, to keep its month/day names and AM/PM
  {$ifdef FPC}
  _I18nDefaultFormatSettings := DefaultFormatSettings;   // FPC RTL global
  {$else}
  {$ifdef ISDELPHIXE}
  _I18nDefaultFormatSettings := SysUtils.FormatSettings; // new Delphi global
  {$else} // old Delphi 7-2010
  GetLocaleFormatSettings(LANG_USER_DEFAULT, _I18nDefaultFormatSettings);
  {$endif ISDELPHIXE}
  {$endif FPC}
  // then make the '/' and ':' pattern characters render as themselves
  _I18nDefaultFormatSettings.DateSeparator := '/';
  _I18nDefaultFormatSettings.TimeSeparator := ':';

finalization
  {$ifdef ISDELPHI}
  // release and disable _LoadResString() cache and translation
  _LoadResFile := nil;
  FreeAndNil(_LoadResCache);
  {$endif ISDELPHI}

end.
