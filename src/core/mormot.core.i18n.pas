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

   Connects the translation slots kept since mORMot 1 mORMoti18n.pas:
   TOnStringTranslate / TOnUtf8Translate callbacks, LoadResStringTranslate,
   i18nDateText / i18nDateTimeText, and the TSynMustache {{"text}} channel -
   see https://synopse.info/forum/viewtopic.php?id=7592

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
  mormot.core.json;


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
    /// merge translations from a JSON object, e.g. {"Hello":"Bonjour"}
    // - returns the number of added or replaced pairs, or -1 on invalid JSON
    function AddFromJson(const Json: RawUtf8): integer;
    /// merge translations from an UTF-8 JSON object file (BOM tolerated)
    function AddFromJsonFile(const FileName: TFileName): integer;
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
    property DateFormat: string
      read fDateFormat write fDateFormat;
    /// optional FormatDateTime() pattern used by the i18nDateTimeText hook
    // - default '' will use the plain RTL DateTimeToStr() rendering
    property DateTimeFormat: string
      read fDateTimeFormat write fDateTimeFormat;
    /// how many translation pairs are stored in this table
    function Count: integer;
  end;


{ ************* TSynLanguages Registry with per-thread Language }

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
    /// load all <iso>.json files from a folder, e.g. en.json or zh.json
    // - returns the number of recognized language files
    function LoadFromFolder(const Folder: TFileName): integer;
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
    procedure SetGlobal;
  end;

/// the main TSynLanguages instance, as set by TSynLanguages.SetGlobal
// - nil if no SetGlobal call was made
function I18n: TSynLanguages;


implementation


{ ************* TSynLanguage per-language Translation Table }

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

function TSynLanguage.AddFromJson(const Json: RawUtf8): integer;
var
  doc: TDocVariantData;
  i: PtrInt;
  v: RawUtf8;
begin
  result := -1;
  if not doc.InitJson(Json, JSON_FAST) or
     not doc.IsObject then
    exit;
  result := 0;
  for i := 0 to doc.Count - 1 do
  begin
    VariantToUtf8(doc.Values[i], v);
    fTexts.AddOrUpdate(doc.Names[i], v);
    inc(result);
  end;
end;

function TSynLanguage.AddFromJsonFile(const FileName: TFileName): integer;
var
  json: RawUtf8;
begin
  json := RawUtf8(StringFromFile(FileName));
  if (length(json) >= 3) and
     (PCardinal(json)^ and $00ffffff = $00bfbbef) then
    delete(json, 1, 3); // ignore any UTF-8 BOM
  result := AddFromJson(json);
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

function _I18nDateTimeText(const DateTime: TDateTime): string;
var
  lang: TSynLanguage;
begin
  lang := nil;
  if _MainI18n <> nil then
    lang := _MainI18n.Current;
  if (lang <> nil) and
     (lang.DateTimeFormat <> '') then
    result := FormatDateTime(lang.DateTimeFormat, DateTime)
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
    result := FormatDateTime(lang.DateFormat, dt)
  else
    result := DateToStr(dt);
end;

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
  fn: TFileName;
begin
  result := 0;
  if FindFirst(IncludeTrailingPathDelimiter(Folder) + '*.json',
       faAnyFile, sr) = 0 then
  begin
    repeat
      fn := GetFileNameWithoutExt(sr.Name);
      lng := IsoTextToLanguage(StringToUtf8(fn));
      if lng <> lngUndefined then
      begin
        Language[lng].AddFromJsonFile(
          IncludeTrailingPathDelimiter(Folder) + sr.Name);
        inc(result);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
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


end.
