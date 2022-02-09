/// Framework Core {{mustache}} Templates Renderer
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.mustache;

{
  *****************************************************************************

   Logic-Less Mustache Templates Rendering
   - Mustache Execution Data Context Types
   - TSynMustache Template Processing

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants;


{ ************ Mustache Execution Data Context Types }

type
  /// exception raised during process of a {{mustache}} template
  ESynMustache = class(ESynException);

  /// identify the {{mustache}} tag kind
  // - mtVariable if the tag is a variable - e.g. {{myValue}} - or an Expression
  // Helper - e.g. {{helperName valueName}}
  // - mtVariableUnescape, mtVariableUnescapeAmp to unescape the variable HTML - e.g.
  // {{{myRawValue}}} or {{& name}}
  // - mtSection and mtInvertedSection for sections beginning - e.g.
  // {{#person}} or {{^person}}
  // - mtSectionEnd for sections ending - e.g. {{/person}}
  // - mtComment for comments - e.g. {{! ignore me}}
  // - mtPartial for partials - e.g. {{> next_more}}
  // - mtSetPartial for setting an internal partial - e.g.
  // {{<foo}}This is the foo partial {{myValue}} template{{/foo}}
  // - mtSetDelimiter for setting custom delimeter symbols - e.g. {{=<% %>=}} -
  // Warning: current implementation only supports two character delimiters
  // - mtTranslate for content i18n via a callback - e.g. {{"English text}}
  // - mtText for all text that appears outside a symbol
  TSynMustacheTagKind = (
    mtVariable,
    mtVariableUnescape,
    mtVariableUnescapeAmp,
    mtSection,
    mtInvertedSection,
    mtSectionEnd,
    mtComment,
    mtPartial,
    mtSetPartial,
    mtSetDelimiter,
    mtTranslate,
    mtText);

  /// store a {{mustache}} tag
  TSynMustacheTag = record
    /// points to the mtText buffer start
    // - main template's text is not allocated as a separate string during
    // parsing, but will rather be copied directly from the template memory
    TextStart: PUtf8Char;
    /// stores the mtText buffer length
    TextLen: integer;
    /// the index in Tags[] of the other end of this section (16-bit)
    // - either the index of mtSectionEnd for mtSection/mtInvertedSection
    // - or the index of mtSection/mtInvertedSection for mtSectionEnd
    SectionOppositeIndex: SmallInt;
    /// the kind of the tag
    Kind: TSynMustacheTagKind;
    /// the tag content, excluding trailing {{ }} and corresponding symbol
    // - is not set for mtText nor mtSetDelimiter
    Value: RawUtf8;
  end;

  /// store all {{mustache}} tags of a given template
  TSynMustacheTagDynArray = array of TSynMustacheTag;

  /// states the section content according to a given value
  // - msNothing for false values or empty lists
  // - msSingle for non-false values but not a list
  // - msSinglePseudo is for *-first *-last *-odd and helper values
  // - msList for non-empty lists
  TSynMustacheSectionType = (
    msNothing,
    msSingle,
    msSinglePseudo,
    msList);

  TSynMustache = class;

  /// callback signature used to process an Expression Helper variable
  // - i.e. {{helperName value}} tags
  // - returned value will be used to process as replacement of a single {{tag}}
  TSynMustacheHelperEvent =
    procedure(const Value: variant; out Result: variant) of object;

  /// used to store a registered Expression Helper implementation
  TSynMustacheHelper = record
    /// the Expression Helper name
    Name: RawUtf8;
    /// the corresponding callback to process the tag
    Event: TSynMustacheHelperEvent;
  end;

  /// used to store all registered Expression Helpers
  // - i.e. {{helperName value}} tags
  // - use TSynMustache.HelperAdd/HelperDelete class methods to manage the list
  // or retrieve standard helpers via TSynMustache.HelpersGetStandardList
  TSynMustacheHelpers = array of TSynMustacheHelper;

  /// handle {{mustache}} template rendering context, i.e. all values
  // - this abstract class should not be used directly, but rather any
  // other overridden class
  TSynMustacheContext = class
  protected
    fContextCount: integer;
    fWriter: TJsonWriter;
    fOwner: TSynMustache;
    fEscapeInvert: boolean;
    fOwnWriter: boolean;
    fHelpers: TSynMustacheHelpers;
    fOnStringTranslate: TOnStringTranslate;
    procedure TranslateBlock(Text: PUtf8Char; TextLen: Integer); virtual;
    procedure PopContext; virtual; abstract;
    procedure AppendValue(const ValueName: RawUtf8; UnEscape: boolean);
      virtual; abstract;
    function AppendSection(const ValueName: RawUtf8): TSynMustacheSectionType;
      virtual; abstract;
    function GotoNextListItem: boolean;
      virtual; abstract;
  public
    /// initialize the rendering context for the given text writer
    constructor Create(Owner: TSynMustache; WR: TJsonWriter);
    /// release this rendering context instance
    destructor Destroy; override;
    /// the registered Expression Helpers, to handle {{helperName value}} tags
    // - use TSynMustache.HelperAdd/HelperDelete class methods to manage the list
    // or retrieve standard helpers via TSynMustache.HelpersGetStandardList
    property Helpers: TSynMustacheHelpers
      read fHelpers write fHelpers;
    /// access to the {{"English text}} translation callback
    property OnStringTranslate: TOnStringTranslate
      read fOnStringTranslate write fOnStringTranslate;
    /// read-only access to the associated text writer instance
    property Writer: TJsonWriter
      read fWriter;
    /// invert the HTML characters escaping process
    // - by default, {{value}} will escape value chars, and {{{value}} won't
    // - set this property to true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    property EscapeInvert: boolean
      read fEscapeInvert write fEscapeInvert;
  end;

  /// handle {{mustache}} template rendering context from a custom variant
  // - the context is given via a custom variant type implementing
  // TSynInvokeableVariantType.Lookup, e.g. TDocVariant or TSMVariant
  TSynMustacheContextVariant = class(TSynMustacheContext)
  protected
    fContext: array of record
      Document: TVarData;
      DocumentType: TSynInvokeableVariantType;
      ListCount: integer;
      ListCurrent: integer;
      ListCurrentDocument: TVarData;
      ListCurrentDocumentType: TSynInvokeableVariantType;
    end;
    fTempGetValueFromContextHelper: TVariantDynArray;
    fReuse: TLightLock;
    procedure PushContext(aDoc: TVarData);
    procedure PopContext; override;
    procedure AppendValue(const ValueName: RawUtf8; UnEscape: boolean);
      override;
    function AppendSection(const ValueName: RawUtf8): TSynMustacheSectionType;
      override;
    function GotoNextListItem: boolean; override;
    function GetValueFromContext(const ValueName: RawUtf8;
      var Value: TVarData): TSynMustacheSectionType;
    function GetValueCopyFromContext(const ValueName: RawUtf8): variant;
    procedure AppendVariant(const Value: variant; UnEscape: boolean);
  public
    /// initialize the context from a custom variant document
    // - note that the aDocument instance shall be available during all
    // lifetime of this TSynMustacheContextVariant instance
    // - you should not use this constructor directly, but the
    // corresponding TSynMustache.Render*() methods
    constructor Create(Owner: TSynMustache; WR: TJsonWriter;
      SectionMaxCount: integer; const aDocument: variant; OwnWriter: boolean);
    /// allow to reuse this Mustache template rendering context
    procedure CancelAll;
  end;

  /// maintain a list of {{mustache}} partials
  // - this list of partials template could be supplied to TSynMustache.Render()
  // method, to render {{>partials}} as expected
  // - using a dedicated class allows to share the partials between execution
  // context, without recurring to non SOLID global variables
  // - you may also define "internal" partials, e.g. {{<foo}}This is foo{{/foo}}
  TSynMustachePartials = class
  protected
    fList: TRawUtf8List;
    fOwned: boolean;
    function GetPartial(const PartialName: RawUtf8): TSynMustache;
  public
    /// initialize the template partials storage
    // - after creation, the partials should be registered via the Add() method
    // - you shall manage this instance life time with a try..finally Free block
    constructor Create; overload;
    /// initialize a template partials storage with the supplied templates
    // - partials list is expected to be supplied in Name / Template pairs
    // - this instance can be supplied as parameter to the TSynMustache.Render()
    // method, which will free the instances as soon as it finishes
    constructor CreateOwned(
      const NameTemplatePairs: array of RawUtf8); overload;
    /// initialize a template partials storage with the supplied templates
    // - partials list is expected to be supplied as a dvObject TDocVariant,
    // each member being the name/template string pairs
    // - if the supplied variant is not a matching TDocVariant, will return nil
    // - this instance can be supplied as parameter to the TSynMustache.Render()
    // method, which will free the instances as soon as it finishes
    class function CreateOwned(
      const Partials: variant): TSynMustachePartials; overload;
    /// register a {{>partialName}} template
    // - returns the parsed template
    function Add(const aName,aTemplate: RawUtf8): TSynMustache; overload;
    /// register a {{>partialName}} template
    // - returns the parsed template
    function Add(const aName: RawUtf8;
      aTemplateStart, aTemplateEnd: PUtf8Char): TSynMustache; overload;
    /// search some text withing the {{mustache}} partial
    function FoundInTemplate(const text: RawUtf8): PtrInt;
    /// delete the partials
    destructor Destroy; override;
    /// low-level access to the internal partials list
    property List: TRawUtf8List
      read fList;
  end;



{ ************ TSynMustache Template Processing }

  /// handles one {{mustache}} pre-rendered template
  // - once parsed, a template will be stored in this class instance, to be
  // rendered lated via the Render() method
  // - you can use the Parse() class function to maintain a shared cache of
  // parsed templates
  // - implements all official mustache specifications, and some extensions
  // - handles {{.}} pseudo-variable for the current context object (very
  // handy when looping through a simple list, for instance)
  // - handles {{-index}} pseudo-variable for the current context array index
  // (1-based value) so that e.g.
  // "My favorite things:\n{{#things}}{{-index}}. {{.}}\n{{/things}}"
  // over {things:["Peanut butter", "Pen spinning", "Handstands"]} renders as
  // "My favorite things:\n1. Peanut butter\n2. Pen spinning\n3. Handstands\n"
  // - you could use {{-index0}} for 0-based index value
  // - handles -first  -last  and  -odd  pseudo-section keys, e.g.
  // "{{#things}}{{^-first}}, {{/-first}}{{.}}{{/things}}"
  // over {things:["one", "two", "three"]} renders as 'one, two, three'
  // - allows inlined partial templates , to be defined e.g. as
  // {{<foo}}This is the foo partial {{myValue}} template{{/foo}}
  // - features {{"English text}} translation, via a custom callback
  // - this implementation is thread-safe and re-entrant (i.e. the same
  // TSynMustache instance can be used by several threads at once)
  TSynMustache = class
  protected
    fTemplate: RawUtf8;
    fTags: TSynMustacheTagDynArray;
    fInternalPartials: TSynMustachePartials;
    fSectionMaxCount: integer;
    fCachedContext: TSynMustacheContextVariant;
    // standard helpers implementation
    class procedure DateTimeToText(const Value: variant; out Result: variant);
    class procedure DateToText(const Value: variant; out Result: variant);
    class procedure DateFmt(const Value: variant; out Result: variant);
    class procedure TimeLogToText(const Value: variant; out Result: variant);
    class procedure BlobToBase64(const Value: variant; out Result: variant);
    class procedure ToJson(const Value: variant; out Result: variant);
    class procedure JsonQuote(const Value: variant; out Result: variant);
    class procedure JsonQuoteUri(const Value: variant; out Result: variant);
    class procedure WikiToHtml(const Value: variant; out Result: variant);
    class procedure MarkdownToHtml(const Value: variant; out Result: variant);
    class procedure SimpleToHtml(const Value: variant; out Result: variant);
    class procedure Lower(const Value: variant; out Result: variant);
    class procedure Upper(const Value: variant; out Result: variant);
    class procedure EnumTrim(const Value: variant; out Result: variant);
    class procedure EnumTrimRight(const Value: variant; out Result: variant);
    class procedure PowerOfTwo(const Value: variant; out Result: variant);
    class procedure Equals_(const Value: variant; out Result: variant);
    class procedure If_(const Value: variant; out Result: variant);
    class procedure NewGuid(const Value: variant; out Result: variant);
    class procedure ExtractFileName(const Value: variant; out Result: variant);
  public
    /// parse a {{mustache}} template, and returns the corresponding
    // TSynMustache instance
    // - an internal cache is maintained by this class function
    // - this implementation is thread-safe and re-entrant: i.e. the same
    // TSynMustache returned instance can be used by several threads at once
    // - will raise an ESynMustache exception on error
    class function Parse(const aTemplate: RawUtf8): TSynMustache;
    /// remove the specified {{mustache}} template from the internal cache
    // - returns TRUE on success, or FALSE if the template was not cached
    // by a previous call to Parse() class function
    class function UnParse(const aTemplate: RawUtf8): boolean;
    /// parse and render a {{mustache}} template over the supplied JSON
    // - an internal templates cache is maintained by this class function
    // - returns TRUE and set aContent the rendered content on success
    // - returns FALSE if the template is not correct
    class function TryRenderJson(const aTemplate, aJson: RawUtf8;
      out aContent: RawUtf8): boolean;
  public
    /// initialize and parse a pre-rendered {{mustache}} template
    // - you should better use the Parse() class function instead, which
    // features an internal thread-safe cache
    constructor Create(const aTemplate: RawUtf8); overload;
    /// initialize and parse a pre-rendered {{mustache}} template
    // - you should better use the Parse() class function instead, which
    // features an internal thread-safe cache
    constructor Create(
      aTemplate: PUtf8Char; aTemplateLen: integer); overload; virtual;
    /// finalize internal memory
    destructor Destroy; override;
    /// register one Expression Helper callback for a given list of helpers
    // - i.e. to let aEvent process {{aName value}} tags
    // - the supplied name will be checked against the current list, and replace
    // any existing entry
    class procedure HelperAdd(var Helpers: TSynMustacheHelpers;
      const aName: RawUtf8; aEvent: TSynMustacheHelperEvent); overload;
    /// register several Expression Helper callbacks for a given list of helpers
    // - the supplied names will be checked against the current list, and replace
    // any existing entry
    class procedure HelperAdd(var Helpers: TSynMustacheHelpers;
      const aNames: array of RawUtf8;
      const aEvents: array of TSynMustacheHelperEvent); overload;
    /// unregister one Expression Helper callback for a given list of helpers
    class procedure HelperDelete(var Helpers: TSynMustacheHelpers;
      const aName: RawUtf8);
    /// search for one Expression Helper event by name
    class function HelperFind(const Helpers: TSynMustacheHelpers;
      aName: PUtf8Char; aNameLen: TStrLen): PtrInt;
    /// returns a list of most used static Expression Helpers
    // - registered helpers are DateTimeToText, DateToText, DateFmt, TimeLogToText,
    // BlobToBase64, JsonQuote, JsonQuoteUri, ToJson, EnumTrim, EnumTrimRight,
    // Lower, Upper, PowerOfTwo, Equals (expecting two parameters), MarkdownToHtml,
    // SimpleToHtml (Markdown with no HTML pass-through) and WikiToHtml
    // (following TJsonWriter.AddHtmlEscapeWiki syntax)
    // - an additional #if helper is also registered, which would allow runtime
    // view logic, via = < > <= >= <> operators over two values:
    // $ {{#if .,"=",123}}  {{#if Total,">",1000}}  {{#if info,"<>",""}}
    // which may be shortened as such:
    // $ {{#if .=123}}  {{#if Total>1000}}  {{#if info<>""}}
    class function HelpersGetStandardList: TSynMustacheHelpers; overload;
    /// returns a list of most used static Expression Helpers, adding some
    // custom callbacks
    // - is just a wrapper around HelpersGetStandardList and HelperAdd()
    class function HelpersGetStandardList(const aNames: array of RawUtf8;
      const aEvents: array of TSynMustacheHelperEvent): TSynMustacheHelpers; overload;

    /// renders the {{mustache}} template into a destination text buffer
    // - the context is given via our abstract TSynMustacheContext wrapper
    // - the rendering extended in fTags[] is supplied as parameters
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned
    procedure RenderContext(Context: TSynMustacheContext;
      TagStart, TagEnd: integer; Partials: TSynMustachePartials;
      NeverFreePartials: boolean);
    /// renders the {{mustache}} template from a variant defined context
    // - the context is given via a custom variant type implementing
    // TSynInvokeableVariantType.Lookup, e.g. TDocVariant or TSMVariant
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned,
    // a list of Expression Helpers, or a custom {{"English text}} callback
    // - can be used e.g. via a TDocVariant:
    // !var mustache := TSynMustache;
    // !    doc: variant;
    // !    html: RawUtf8;
    // !begin
    // !  mustache := TSynMustache.Parse(
    // !    'Hello {{name}}'#13#10'You have just won {{value}} dollars!');
    // !  TDocVariant.New(doc);
    // !  doc.name := 'Chris';
    // !  doc.value := 10000;
    // !  html := mustache.Render(doc);
    // !  // here html='Hello Chris'#13#10'You have just won 10000 dollars!'
    // - you can also retrieve the context from an ORM query:
    // ! dummy := TSynMustache.Parse(
    // !   '{{#items}}'#13#10'{{Int}}={{Test}}'#13#10'{{/items}}').Render(
    // !   aClient.RetrieveDocVariantArray(TOrmTest, 'items', 'Int,Test'));
    // - set EscapeInvert = true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    function Render(const Context: variant;
      Partials: TSynMustachePartials = nil;
      Helpers: TSynMustacheHelpers = nil;
      const OnTranslate: TOnStringTranslate = nil;
      EscapeInvert: boolean = false): RawUtf8;
    /// renders the {{mustache}} template from JSON defined context
    // - the context is given via a JSON object, defined from UTF-8 buffer
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned,
    // a list of Expression Helpers, or a custom {{"English text}} callback
    // - is just a wrapper around Render(_JsonFast())
    // - you can write e.g. with the extended JSON syntax:
    // ! html := mustache.RenderJson('{things:["one", "two", "three"]}');
    // - set EscapeInvert = true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    function RenderJson(const Json: RawUtf8;
      Partials: TSynMustachePartials = nil;
      Helpers: TSynMustacheHelpers = nil;
      const OnTranslate: TOnStringTranslate = nil;
      EscapeInvert: boolean = false): RawUtf8; overload;
    /// renders the {{mustache}} template from JSON defined context
    // - the context is given via a JSON object, defined with parameters
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned,
    // a list of Expression Helpers, or a custom {{"English text}} callback
    // - is just a wrapper around Render(_JsonFastFmt())
    // - you can write e.g. with the extended JSON syntax:
    // !   html := mustache.RenderJson('{name:?,value:?}',[],['Chris',10000]);
    // - set EscapeInvert = true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    function RenderJson(const Json: RawUtf8;
      const Args, Params: array of const;
      Partials: TSynMustachePartials = nil;
      Helpers: TSynMustacheHelpers = nil;
      const OnTranslate: TOnStringTranslate = nil;
      EscapeInvert: boolean = false): RawUtf8; overload;
    /// search some text within the {{mustache}} template text
    function FoundInTemplate(const text: RawUtf8): boolean;

    /// read-only access to the raw {{mustache}} template content
    property Template: RawUtf8
      read fTemplate;
    /// the maximum possible number of nested contexts
    property SectionMaxCount: Integer
      read fSectionMaxCount;
  end;


const
  /// Mustache-friendly JSON Serialization Options
  // - as used e.g. from mormot.rest.mvc Data Context from Cookies
  TEXTWRITEROPTIONS_MUSTACHE =
     [twoForceJsonExtended,
      twoEnumSetsAsBooleanInRecord,
      twoTrimLeftEnumSets];

  /// this constant can be used to define as JSON a tag value
  NULL_OR_TRUE: array[boolean] of RawUtf8 = (
    'null', 'true');

  /// this constant can be used to define as JSON a tag value as separator
  NULL_OR_COMMA: array[boolean] of RawUtf8 = (
    'null', '","');



implementation


{ ************ Mustache Execution Data Context Types }

{ TSynMustacheContext }

constructor TSynMustacheContext.Create(Owner: TSynMustache; WR: TJsonWriter);
begin
  fOwner := Owner;
  fWriter := WR;
end;

destructor TSynMustacheContext.Destroy;
begin
  inherited Destroy;
  if fOwnWriter then
    fWriter.Free;
end;

procedure TSynMustacheContext.TranslateBlock(Text: PUtf8Char; TextLen: Integer);
var
  s: string;
begin
  if Assigned(OnStringTranslate) then
  begin
    Utf8DecodeToString(Text, TextLen, s);
    OnStringTranslate(s);
    fWriter.AddNoJsonEscapeString(s);
  end
  else
    fWriter.AddNoJsonEscape(Text, TextLen);
end;


{ TSynMustacheContextVariant }

constructor TSynMustacheContextVariant.Create(Owner: TSynMustache;
  WR: TJsonWriter; SectionMaxCount: integer; const aDocument: variant;
  OwnWriter: boolean);
begin
  fOwnWriter := OwnWriter;
  inherited Create(Owner, WR);
  SetLength(fContext, SectionMaxCount + 4);
  PushContext(TVarData(aDocument)); // weak copy
end;

procedure TSynMustacheContextVariant.CancelAll;
begin
  fContextCount := 0;
  fEscapeInvert := false;
  fWriter.CancelAll;
  fReuse.UnLock;
end;

procedure TSynMustacheContextVariant.PushContext(aDoc: TVarData);
begin
  if fContextCount >= length(fContext) then
    // was roughtly set by SectionMaxCount
    SetLength(fContext, fContextCount + 32);
  with fContext[fContextCount] do
  begin
    Document := aDoc;
    DocumentType := FindSynVariantType(aDoc.VType);
    ListCurrent := -1;
    if DocumentType = nil then
      ListCount := -1
    else
      ListCount := DocumentType.IterateCount(aDoc);
  end;
  inc(fContextCount);
end;

procedure TSynMustacheContextVariant.PopContext;
begin
  if fContextCount > 1 then
    dec(fContextCount);
end;

function TSynMustacheContextVariant.GetValueCopyFromContext(
  const ValueName: RawUtf8): variant;
var
  tmp: TVarData;
begin
  if (ValueName = '') or
     (ValueName[1] in ['0'..'9', '"', '{', '[']) or
     (ValueName = 'true') or
     (ValueName = 'false') or
     (ValueName = 'null') then
    VariantLoadJson(result, ValueName, @JSON_[mFast])
  else
  begin
    GetValueFromContext(ValueName, tmp);
    SetVariantByValue(variant(tmp), result); // copy value
  end;
end;

function TSynMustacheContextVariant.GetValueFromContext(
  const ValueName: RawUtf8; var Value: TVarData): TSynMustacheSectionType;
var
  i, space, helper: PtrInt;

  procedure ProcessHelper;
  var
    valnam: RawUtf8;
    val: TVarData;
    valArr: TDocVariantData absolute val;
    valFree: boolean;
    names: TRawUtf8DynArray;
    res: PVarData;
    j, k, n: integer;
  begin
    valnam := Copy(ValueName, space + 1, maxInt);
    valFree := false;
    if valnam <> '' then
    begin
      if valnam = '.' then
        GetValueFromContext(valnam, val)
      else if ((valnam <> '') and
               (valnam[1] in ['1'..'9', '"', '{', '['])) or
              (valnam = 'true') or
              (valnam = 'false') or
              (valnam = 'null') then
      begin
        // {{helper 123}} or {{helper "constant"}} or {{helper [1,2,3]}}
        val.VType := varEmpty;
        JsonToVariantInPlace(variant(val), pointer(valnam), JSON_FAST_FLOAT);
        valFree := true;
      end
      else
      begin
        for j := 1 to length(valnam) do
          case valnam[j] of
            ' ':
              // allows {{helper1 helper2 value}} recursive calls
              break;
            ',':
              begin
                // {{helper value,123,"constant"}}
                CsvToRawUtf8DynArray(Pointer(valnam), names, ',', true);
                // TODO: handle 123,"a,b,c"
                valArr.InitFast;
                for k := 0 to High(names) do
                  valArr.AddItem(GetValueCopyFromContext(names[k]));
                valFree := true;
                break;
              end;
            '<',
            '>',
            '=':
              begin
                // {{#if .=123}} -> {{#if .,"=",123}}
                k := j + 1;
                if valnam[k] in ['=', '>'] then
                  inc(k);
                valArr.InitArray([
                   GetValueCopyFromContext(Copy(valnam, 1, j - 1)),
                   Copy(valnam, j, k - j),
                   GetValueCopyFromContext(Copy(valnam, k, maxInt))],
                  JSON_FAST_FLOAT);
                valFree := true;
                break;
              end;
          end;
        if not valFree then
          GetValueFromContext(valnam, val);
      end;
    end;
    n := fContextCount + 4;
    if length(fTempGetValueFromContextHelper) < n then
      SetLength(fTempGetValueFromContextHelper, n);
    res := @fTempGetValueFromContextHelper[fContextCount - 1];
    Helpers[helper].Event(variant(val), variant(res^));
    Value := res^;
    if valFree then
      VarClear(variant(val));
    result := msSinglePseudo;
  end;

begin
  result := msNothing;
  if ValueName = '.' then
    with fContext[fContextCount - 1] do
    begin
      if ListCount > 0 then
        Value := ListCurrentDocument
      else
        Value := Document;
      exit;
    end;
  space := PosExChar(' ', ValueName);
  if space > 1 then
  begin
    // {{helper value}}
    helper := TSynMustache.HelperFind(Helpers, pointer(ValueName), space - 1);
    if helper >= 0 then
    begin
      ProcessHelper;
      exit;
    end;
    // if helper not found, will return the unprocessed value
  end;
  // recursive search of {{value}}
  for i := fContextCount - 1 downto 0 do
    with fContext[i] do
      if DocumentType <> nil then
        if ListCount < 0 then
        begin
          // single item context
          DocumentType.Lookup(Value, Document, pointer(ValueName));
          if Value.VType >= varNull then
            exit;
        end
        else if IdemPChar(pointer(ValueName), '-INDEX') then
        begin
          // {{-index}}
          Value.VType := varInteger;
          if ValueName[7] = '0' then
            Value.VInteger := ListCurrent
          else
            Value.VInteger := ListCurrent + 1;
          exit;
        end
        else if (ListCurrent < ListCount) and
                (ListCurrentDocumentType <> nil) then
        begin
          ListCurrentDocumentType.Lookup(
            Value, ListCurrentDocument, pointer(ValueName));
          if Value.VType >= varNull then
            exit;
        end;
  // no matching {{value}} -> try if was not {{helper}}
  if space = 0 then
  begin
    space := length(ValueName);
    helper := TSynMustache.HelperFind(Helpers, pointer(ValueName), space);
    if helper >= 0 then
      ProcessHelper;
  end;
end;

procedure TSynMustacheContextVariant.AppendValue(const ValueName: RawUtf8;
  UnEscape: boolean);
var
  Value: TVarData;
begin
  GetValueFromContext(ValueName, Value);
  AppendVariant(variant(Value), UnEscape);
end;

procedure TSynMustacheContextVariant.AppendVariant(const Value: variant;
  UnEscape: boolean);
var
  ValueText: RawUtf8;
  wasString: boolean;
begin
  if TVarData(Value).VType > varNull then
    if VarIsNumeric(Value) then
      // avoid RawUtf8 conversion for plain numbers
      fWriter.AddVariant(Value, twNone)
    else
    begin
      if fEscapeInvert then
        UnEscape := not UnEscape;
      VariantToUtf8(Value, ValueText, wasString);
      if UnEscape then
        fWriter.AddNoJsonEscape(pointer(ValueText), length(ValueText))
      else
        fWriter.AddHtmlEscape(pointer(ValueText));
    end;
end;

function TSynMustacheContextVariant.AppendSection(
  const ValueName: RawUtf8): TSynMustacheSectionType;
var
  Value: TVarData;
begin
  result := msNothing;
  if (fContextCount > 0) and
     (ValueName[1] = '-') then
    with fContext[fContextCount - 1] do
      if ListCount >= 0 then
      begin
        if ((ValueName = '-first') and
            (ListCurrent = 0)) or
           ((ValueName = '-last') and
            (ListCurrent = ListCount - 1)) or
           ((ValueName = '-odd') and
            (ListCurrent and 1 = 0)) then
          result := msSinglePseudo;
        exit;
      end;
  result := GetValueFromContext(ValueName, Value);
  if result <> msNothing then
  begin
    if (Value.VType <= varNull) or
       ((Value.VType = varBoolean) and
        not Value.VBoolean)
      then
      result := msNothing;
    exit;
  end;
  PushContext(Value);
  if (Value.VType <= varNull) or
     ((Value.VType = varBoolean) and
      not Value.VBoolean) then
    // null or false value will not display the section
    exit;
  with fContext[fContextCount - 1] do
    if ListCount < 0 then
      // single item
      result := msSingle
    else if ListCount = 0 then
      // empty list will not display the section
      exit
    else
      // non-empty list
      result := msList;
end;

function TSynMustacheContextVariant.GotoNextListItem: boolean;
begin
  result := false;
  if fContextCount > 0 then
    with fContext[fContextCount - 1] do
    begin
      ListCurrentDocument.VType := varEmpty;
      ListCurrentDocumentType := nil;
      inc(ListCurrent);
      if ListCurrent >= ListCount then
        exit;
      DocumentType.Iterate(ListCurrentDocument, Document, ListCurrent);
      ListCurrentDocumentType := FindSynVariantType(ListCurrentDocument.VType);
      result := true;
    end;
end;


{ TSynMustachePartials }

constructor TSynMustachePartials.Create;
begin
  fList := TRawUtf8List.CreateEx([fNoDuplicate, fCaseSensitive]);
end;

constructor TSynMustachePartials.CreateOwned(
  const NameTemplatePairs: array of RawUtf8);
var
  A: PtrInt;
begin
  Create;
  fOwned := true;
  for A := 0 to high(NameTemplatePairs) div 2 do
    Add(NameTemplatePairs[A * 2], NameTemplatePairs[A * 2 + 1]);
end;

function TSynMustachePartials.Add(const aName, aTemplate: RawUtf8): TSynMustache;
begin
  result := TSynMustache.Parse(aTemplate);
  if (result <> nil) and
     (fList.AddObject(aName, result) < 0) then
    raise ESynMustache.CreateUtf8('%.Add(%) duplicated name', [self, aName]);
end;

function TSynMustachePartials.Add(const aName: RawUtf8;
  aTemplateStart, aTemplateEnd: PUtf8Char): TSynMustache;
var
  aTemplate: RawUtf8;
begin
  FastSetString(aTemplate, aTemplateStart, aTemplateEnd - aTemplateStart);
  result := Add(aName, aTemplate);
end;

function TSynMustachePartials.FoundInTemplate(const text: RawUtf8): PtrInt;
begin
  if self <> nil then
    result := fList.Contains(text)
  else
    result := -1;
end;

class function TSynMustachePartials.CreateOwned(
  const Partials: variant): TSynMustachePartials;
var
  p: PtrInt;
begin
  result := nil;
  with _Safe(Partials)^ do
    if IsObject and
       (Count > 0) then
    begin
      result := TSynMustachePartials.Create;
      result.fOwned := true;
      for p := 0 to Count - 1 do
        result.Add(names[p], VariantToUtf8(Values[p]));
    end;
end;

destructor TSynMustachePartials.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

function TSynMustachePartials.GetPartial(
  const PartialName: RawUtf8): TSynMustache;
var
  i: PtrInt;
begin
  if self = nil then
  begin
    result := nil;
    exit;
  end;
  i := fList.IndexOf(PartialName); // using O(1) hashing
  if i < 0 then
    result := nil
  else
    result := TSynMustache(fList.Objects[i]);
end;

function KindToText(Kind: TSynMustacheTagKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TSynMustacheTagKind), ord(Kind));
end;



{ ************ TSynMustache Template Processing }

type
  TSynMustacheParser = class
  protected
    fTagStart, fTagStop: word;
    fPos, fPosMin, fPosMax, fPosTagStart: PUtf8Char;
    fTagCount: integer;
    fTemplate: TSynMustache;
    fScanStart, fScanEnd: PUtf8Char;
    function Scan(ExpectedTag: Word): boolean;
    procedure AddTag(aKind: TSynMustacheTagKind;
      aStart: PUtf8Char = nil; aEnd: PUtf8Char = nil);
  public
    constructor Create(Template: TSynMustache;
      const DelimiterStart, DelimiterStop: RawUtf8);
    procedure Parse(p, PEnd: PUtf8Char);
  end;

  TSynMustacheCache = class(TRawUtf8List)
  public
    function Parse(const aTemplate: RawUtf8): TSynMustache;
    function UnParse(const aTemplate: RawUtf8): boolean;
  end;

var
  SynMustacheCache: TSynMustacheCache = nil;


{ TSynMustacheParser }

procedure TSynMustacheParser.AddTag(aKind: TSynMustacheTagKind;
  aStart, aEnd: PUtf8Char);
var
  P: PUtf8Char;
begin
  if (aStart = nil) or
     (aEnd = nil) then
  begin
    aStart := fScanStart;
    aEnd := fScanEnd;
    case aKind of
      mtComment,
      mtSection,
      mtSectionEnd,
      mtInvertedSection,
      mtSetDelimiter,
      mtPartial:
        begin
          // (indented) standalone lines should be removed from the template
          if aKind <> mtPartial then
            while (fPosTagStart > fPosMin) and
                  (fPosTagStart[-1] in [' ', #9]) do
              // ignore any indentation chars
              dec(fPosTagStart);
          if (fPosTagStart = fPosMin) or
             (fPosTagStart[-1] = #$0A) then
            // tag starts on a new line -> check if ends on the same line
            if (fPos > fPosMax) or
               (fPos^ = #$0A) or
               (PWord(fPos)^ = $0A0D) then
            begin
              if fPos <= fPosMax then
                if fPos^ = #$0A then
                  inc(fPos)
                else if PWord(fPos)^ = $0A0D then
                  inc(fPos, 2);
              if fTagCount > 0 then
                // remove any indentation chars from previous text
                with fTemplate.fTags[fTagCount - 1] do
                  if Kind = mtText then
                    while (TextLen > 0) and
                          (TextStart[TextLen - 1] in [' ', #9]) do
                      dec(TextLen);
            end;
        end;
      mtVariable,
      mtVariableUnescape,
      mtVariableUnescapeAmp:
        begin
          // handle JSON object/array with nested }
          // e.g. as {{helper [{a:{a:1,b:2}}]}}
          P := PosChar(aStart, ' ');
          if (P <> nil) and
             (P < aEnd) then
          begin
            P := GotoNextNotSpaceSameLine(P + 1);
            if P^ in ['{', '['] then
            begin
              P := GotoEndJsonItem(P);
              if P <> nil then
              begin
                aEnd := P;
                fPos := P;
                if not Scan(fTagStop) then
                  raise ESynMustache.CreateUtf8('Unfinished {{%', [aStart]);
                if (aKind = mtVariableUnescape) and
                   (fTagStop = $7d7d) and
                   (PWord(fPos - 1)^ = $7d7d) then
                  // {{{name}}} -> point after }}}
                  inc(fPos);
              end;
            end;
          end;
        end;
    end;
  end;
  if aEnd <= aStart then
    exit;
  if fTagCount >= length(fTemplate.fTags) then
    SetLength(fTemplate.fTags, NextGrow(fTagCount));
  with fTemplate.fTags[fTagCount] do
  begin
    Kind := aKind;
    SectionOppositeIndex := -1;
    case aKind of
      mtText,
      mtComment,
      mtTranslate:
        begin
          TextStart := aStart;
          TextLen := aEnd - aStart;
        end;
    else
      begin
        TextStart := fPosTagStart;
        TextLen := aEnd - fPosTagStart;
        // superfluous in-tag whitespace should be ignored
        while (aStart < aEnd) and
              (aStart^ <= ' ') do
          inc(aStart);
        while (aEnd > aStart) and
              (aEnd[-1] <= ' ') do
          dec(aEnd);
        if aEnd = aStart then
          raise ESynMustache.CreateUtf8(
            'Void % identifier', [KindToText(aKind)^]);
        FastSetString(Value, aStart, aEnd - aStart);
      end;
    end;
  end;
  inc(fTagCount);
end;

constructor TSynMustacheParser.Create(Template: TSynMustache;
  const DelimiterStart, DelimiterStop: RawUtf8);
begin
  fTemplate := Template;
  if length(DelimiterStart) <> 2 then
    raise ESynMustache.CreateUtf8('DelimiterStart="%"', [DelimiterStart]);
  if length(DelimiterStop) <> 2 then
    raise ESynMustache.CreateUtf8('DelimiterStop="%"', [DelimiterStop]);
  fTagStart := PWord(DelimiterStart)^;
  fTagStop := PWord(DelimiterStop)^;
end;

function GotoNextTag(P, PMax: PUtf8Char; ExpectedTag: Word): PUtf8Char;
begin
  if P < PMax then
    repeat
      if PWord(P)^ <> ExpectedTag then
      begin
        inc(P);
        if P < PMax then
          continue;
        break;
      end;
      result := P;
      exit;
    until false;
  result := nil;
end;

function TSynMustacheParser.Scan(ExpectedTag: Word): boolean;
var
  P: PUtf8Char;
begin
  P := GotoNextTag(fPos, fPosMax, ExpectedTag);
  if P = nil then
    result := false
  else
  begin
    fScanEnd := P;
    fScanStart := fPos;
    fPos := P + 2;
    result := true;
  end;
end;

function SectionNameMatch(const start, finish: RawUtf8): boolean;
var
  i: integer;
begin
  if start = finish then
    result := true
  else
  begin
    i := PosExChar(' ', start);
    result := (i > 0) and
              IdemPropNameU(finish, pointer(start), i - 1);
  end;
end;

procedure TSynMustacheParser.Parse(P, PEnd: PUtf8Char);
var
  Kind: TSynMustacheTagKind;
  Symbol: AnsiChar;
  i, j, secCount, secLevel: integer;
begin
  secCount := 0;
  if P = nil then
    exit;
  fPos := P;
  fPosMin := P;
  fPosMax := PEnd - 1;
  repeat
    if not Scan(fTagStart) then
      break;
    fPosTagStart := fScanEnd;
    AddTag(mtText);
    if fPos >= fPosMax then
      break;
    Symbol := fPos^;
    case Symbol of
      '=':
        Kind := mtSetDelimiter;
      '{':
        Kind := mtVariableUnescape;
      '&':
        Kind := mtVariableUnescapeAmp;
      '#':
        Kind := mtSection;
      '^':
        Kind := mtInvertedSection;
      '/':
        Kind := mtSectionEnd;
      '!':
        Kind := mtComment;
      '>':
        Kind := mtPartial;
      '<':
        Kind := mtSetPartial;
      '"':
        Kind := mtTranslate;
    else
      Kind := mtVariable;
    end;
    if Kind <> mtVariable then
      inc(fPos);
    if not Scan(fTagStop) then
      raise ESynMustache.CreateUtf8('Unfinished {{tag [%]', [fPos]);
    case Kind of
      mtSetDelimiter:
        begin
          if (fScanEnd - fScanStart <> 6) or
             (fScanEnd[-1] <> '=') then
            raise ESynMustache.Create(
              'mtSetDelimiter syntax is e.g. {{=<% %>=}}');
          fTagStart := PWord(fScanStart)^;
          fTagStop := PWord(fScanStart + 3)^;
          continue; // do not call AddTag(Kind=mtSetDelimiter)
        end;
      mtVariableUnescape:
        if (Symbol = '{') and
           (fTagStop = $7d7d) and
           (PWord(fPos - 1)^ = $7d7d) then
          // {{{name}}} -> point after }}}
          inc(fPos);
    end;
    AddTag(Kind);
  until false;
  AddTag(mtText, fPos, fPosMax + 1);
  for i := 0 to fTagCount - 1 do
    with fTemplate.fTags[i] do
      case Kind of
        mtSection,
        mtInvertedSection,
        mtSetPartial:
          begin
            inc(secCount);
            if secCount > fTemplate.fSectionMaxCount then
              fTemplate.fSectionMaxCount := secCount;
            secLevel := 1;
            for j := i + 1 to fTagCount - 1 do
              case fTemplate.fTags[j].Kind of
                mtSection,
                mtInvertedSection,
                mtSetPartial:
                  inc(secLevel);
                mtSectionEnd:
                  begin
                    dec(secLevel);
                    if secLevel = 0 then
                      if SectionNameMatch(Value, fTemplate.fTags[j].Value) then
                      begin
                        fTemplate.fTags[j].SectionOppositeIndex := i;
                        SectionOppositeIndex := j;
                        if Kind = mtSetPartial then
                        begin
                          if fTemplate.fInternalPartials = nil then
                            fTemplate.fInternalPartials :=
                              TSynMustachePartials.Create;
                          fTemplate.fInternalPartials.Add(Value,
                            TextStart + TextLen + 2,
                            fTemplate.fTags[j].TextStart);
                        end;
                        break;
                      end
                      else
                        raise ESynMustache.CreateUtf8(
                          'Got {{/%}}, expected {{/%}}',
                          [Value, fTemplate.fTags[j].Value]);
                  end;
              end;
            if SectionOppositeIndex < 0 then
              raise ESynMustache.CreateUtf8(
                'Missing section end {{/%}}', [Value]);
          end;
        mtSectionEnd:
          begin
            dec(secCount);
            if SectionOppositeIndex < 0 then
              raise ESynMustache.CreateUtf8(
                'Unexpected section end {{/%}}', [Value]);
          end;
      end;
  SetLength(fTemplate.fTags, fTagCount);
end;


{ TSynMustacheCache }

function TSynMustacheCache.Parse(const aTemplate: RawUtf8): TSynMustache;
begin
  result := GetObjectFrom(aTemplate);
  if result = nil then
  begin
    result := TSynMustache.Create(aTemplate);
    AddObjectUnique(aTemplate, @result);
  end;
end;

function TSynMustacheCache.UnParse(const aTemplate: RawUtf8): boolean;
begin
  result := Delete(aTemplate) >= 0;
end;


{ TSynMustache }

class function TSynMustache.Parse(const aTemplate: RawUtf8): TSynMustache;
begin
  if SynMustacheCache = nil then
  begin
    GlobalLock; // RegisterGlobalShutdownRelease() will use it anyway
    try
      if SynMustacheCache = nil then
        SynMustacheCache := RegisterGlobalShutdownRelease(
          TSynMustacheCache.CreateEx([fObjectsOwned, fNoDuplicate, fCaseSensitive]));
    finally
      GlobalUnLock;
    end;
  end;
  result := SynMustacheCache.Parse(aTemplate);
end;

class function TSynMustache.UnParse(const aTemplate: RawUtf8): boolean;
begin
  result := SynMustacheCache.UnParse(aTemplate);
end;

class function TSynMustache.TryRenderJson(const aTemplate, aJson: RawUtf8;
  out aContent: RawUtf8): boolean;
var
  mus: TSynMustache;
begin
  if aTemplate <> '' then
  try
    mus := Parse(aTemplate);
    aContent := mus.RenderJson(aJson);
    result := true;
  except
    result := false;
  end
  else
    result := false;
end;

constructor TSynMustache.Create(const aTemplate: RawUtf8);
begin
  Create(pointer(aTemplate), length(aTemplate));
end;

constructor TSynMustache.Create(aTemplate: PUtf8Char; aTemplateLen: integer);
begin
  inherited Create;
  fTemplate := aTemplate;
  with TSynMustacheParser.Create(self, '{{', '}}') do
  try
    Parse(aTemplate, aTemplate + aTemplateLen);
  finally
    Free;
  end;
  fCachedContext := TSynMustacheContextVariant.Create(self,
    TJsonWriter.CreateOwnedStream(8192), SectionMaxCount + 4, Null, true);
end;

procedure TSynMustache.RenderContext(Context: TSynMustacheContext;
  TagStart, TagEnd: integer; Partials: TSynMustachePartials;
  NeverFreePartials: boolean);
var
  partial: TSynMustache;
begin
  try
    while TagStart <= TagEnd do
    begin
      with fTags[TagStart] do
        case Kind of
          mtText:
            if TextLen <> 0 then
              // may be 0 e.g. for standalone without previous Line
              Context.fWriter.AddNoJsonEscape(TextStart, TextLen);
          mtVariable:
            Context.AppendValue(Value, false);
          mtVariableUnescape,
          mtVariableUnescapeAmp:
            Context.AppendValue(Value, true);
          mtSection:
            case Context.AppendSection(Value) of
              msNothing:
                begin
                  // e.g. for no key, false value, or empty list
                  TagStart := SectionOppositeIndex;
                  continue; // ignore whole section
                end;
              msList:
                begin
                  while Context.GotoNextListItem do
                    RenderContext(Context, TagStart + 1,
                      SectionOppositeIndex - 1, Partials, true);
                  TagStart := SectionOppositeIndex;
                  // ignore whole section since we just rendered it as a list
                  continue;
                end;
            // msSingle,msSinglePseudo:
            //   process the section once with current context
            end;
          mtInvertedSection:
            // display section for no key, false value, or empty list
            if Context.AppendSection(Value) <> msNothing then
            begin
              TagStart := SectionOppositeIndex;
              continue; // ignore whole section
            end;
          mtSectionEnd:
            if (fTags[SectionOppositeIndex].Kind in
                 [mtSection, mtInvertedSection]) and
               (Value[1] <> '-') and
               (PosExChar(' ', fTags[SectionOppositeIndex].Value) = 0) then
              Context.PopContext;
          mtComment:
            ; // just ignored
          mtPartial:
            begin
              partial := fInternalPartials.GetPartial(Value);
              if (partial = nil) and
                 (Context.fOwner <> self) then
                 // recursive call
                partial := Context.fOwner.fInternalPartials.GetPartial(Value);
              if (partial = nil) and
                 (Partials <> nil) then
                partial := Partials.GetPartial(Value);
              if partial <> nil then
                partial.RenderContext(
                  Context, 0, high(partial.fTags), Partials, true);
            end;
          mtSetPartial:
            // ignore whole internal {{<partial}}
            TagStart := SectionOppositeIndex;
          mtTranslate:
            if TextLen <> 0 then
              Context.TranslateBlock(TextStart, TextLen);
        else
          raise ESynMustache.CreateUtf8('Kind=% not implemented yet',
            [KindToText(fTags[TagStart].Kind)^]);
        end;
      inc(TagStart);
    end;
  finally
    if (Partials <> nil) and
       (Partials.fOwned) and
       not NeverFreePartials then
      Partials.Free;
  end;
end;

function TSynMustache.Render(const Context: variant;
  Partials: TSynMustachePartials; Helpers: TSynMustacheHelpers;
  const OnTranslate: TOnStringTranslate; EscapeInvert: boolean): RawUtf8;
var
  Ctxt: TSynMustacheContextVariant;
  tmp: TTextWriterStackBuffer;
begin
  Ctxt := fCachedContext; // thread-safe reuse of a shared rendering context
  if Ctxt.fReuse.TryLock then
    Ctxt.PushContext(TVarData(Context)) // weak copy
  else
    Ctxt := TSynMustacheContextVariant.Create(
      self, TJsonWriter.CreateOwnedStream(tmp), SectionMaxCount, Context, true);
  try
    Ctxt.Helpers := Helpers;
    Ctxt.OnStringTranslate := OnTranslate;
    Ctxt.EscapeInvert := EscapeInvert;
    RenderContext(Ctxt, 0, high(fTags), Partials, false);
    Ctxt.Writer.SetText(result);
  finally
    if Ctxt = fCachedContext then
      Ctxt.CancelAll
    else
      Ctxt.Free;
  end;
end;

function TSynMustache.RenderJson(const Json: RawUtf8;
  Partials: TSynMustachePartials; Helpers: TSynMustacheHelpers;
  const OnTranslate: TOnStringTranslate; EscapeInvert: boolean): RawUtf8;
var
  context: variant;
begin
  _Json(Json, context{%H-}, JSON_FAST_FLOAT);
  result := Render(context, Partials, Helpers, OnTranslate, EscapeInvert);
end;

function TSynMustache.RenderJson(const Json: RawUtf8;
  const Args, Params: array of const; Partials: TSynMustachePartials;
  Helpers: TSynMustacheHelpers; const OnTranslate: TOnStringTranslate;
  EscapeInvert: boolean): RawUtf8;
var
  context: variant;
begin
  _Json(FormatUtf8(Json, Args, Params, true), context{%H-}, JSON_FAST_FLOAT);
  result := Render(context, Partials, Helpers, OnTranslate, EscapeInvert);
end;

destructor TSynMustache.Destroy;
begin
  FreeAndNil(fInternalPartials);
  inherited;
  fCachedContext.Free;
end;

function TSynMustache.FoundInTemplate(const text: RawUtf8): boolean;
begin
  // internal partials are part of fTemplate
  result := (self <> nil) and
            (text <> '') and
            (PosEx(text, fTemplate) > 0);
end;

class procedure TSynMustache.HelperAdd(var Helpers: TSynMustacheHelpers;
  const aName: RawUtf8; aEvent: TSynMustacheHelperEvent);
var
  n, i: PtrInt;
begin
  n := length(Helpers);
  for i := 0 to n - 1 do
    if IdemPropNameU(Helpers[i].Name, aName) then
    begin
      Helpers[i].Event := aEvent;
      exit;
    end;
  SetLength(Helpers, n + 1);
  Helpers[n].Name := aName;
  Helpers[n].Event := aEvent;
end;

class procedure TSynMustache.HelperAdd(var Helpers: TSynMustacheHelpers;
  const aNames: array of RawUtf8;
  const aEvents: array of TSynMustacheHelperEvent);
var
  n, i: PtrInt;
begin
  n := length(aNames);
  if n = length(aEvents) then
    for i := 0 to n - 1 do
      HelperAdd(Helpers, aNames[i], aEvents[i]);
end;

class procedure TSynMustache.HelperDelete(var Helpers: TSynMustacheHelpers;
  const aName: RawUtf8);
var
  n, i, j: PtrInt;
begin
  n := length(Helpers);
  for i := 0 to n - 1 do
    if IdemPropNameU(Helpers[i].Name, aName) then
    begin
      for j := i to n - 2 do
        Helpers[j] := Helpers[j + 1];
      SetLength(Helpers, n - 1);
      exit;
    end;
end;

class function TSynMustache.HelperFind(const Helpers: TSynMustacheHelpers;
  aName: PUtf8Char; aNameLen: TStrLen): PtrInt;
var
  h: ^TSynMustacheHelper;
  p: PUtf8Char;
  n: integer;
begin
  h := pointer(Helpers);
  if (h <> nil) and
     (aNameLen > 0) then
  begin
    result := 0;
    n := PDALen(PAnsiChar(h) - _DALEN)^ + _DAOFF;
    repeat
      P := pointer(h^.Name);
      if (PStrLen(P - _STRLEN)^ = aNameLen) and
         IdemPropNameUSameLenNotNull(P, aName, aNameLen) then
        exit;
      inc(h);
      inc(result);
      dec(n);
    until n = 0;
  end;
  result := -1;
end;

var
  HelpersStandardList: TSynMustacheHelpers;

class function TSynMustache.HelpersGetStandardList: TSynMustacheHelpers;
begin
  if HelpersStandardList = nil then
    HelperAdd(HelpersStandardList,
     ['DateTimeToText',
      'DateToText',
      'DateFmt',
      'TimeLogToText',
      'JsonQuote',
      'JsonQuoteUri',
      'ToJson',
      'MarkdownToHtml',
      'SimpleToHtml',
      'WikiToHtml',
      'BlobToBase64',
      'EnumTrim',
      'EnumTrimRight',
      'PowerOfTwo',
      'Equals',
      'If',
      'NewGuid',
      'ExtractFileName',
      'Lower',
      'Upper'],
     [DateTimeToText,
      DateToText,
      DateFmt,
      TimeLogToText,
      JsonQuote,
      JsonQuoteUri,
      ToJson,
      MarkdownToHtml,
      SimpleToHtml,
      WikiToHtml,
      BlobToBase64,
      EnumTrim,
      EnumTrimRight,
      PowerOfTwo,
      Equals_,
      If_,
      NewGuid,
      ExtractFileName,
      Lower,
      Upper]);
  result := HelpersStandardList;
end;

class function TSynMustache.HelpersGetStandardList(
  const aNames: array of RawUtf8;
  const aEvents: array of TSynMustacheHelperEvent): TSynMustacheHelpers;
begin
  // make first a copy to not change/affect global HelpersStandardList
  result := copy(HelpersGetStandardList);
  HelperAdd(result, aNames, aEvents);
end;

class procedure TSynMustache.DateTimeToText(
  const Value: variant; out Result: variant);
var
  Time: TTimeLogBits;
  dt: TDateTime;
begin
  if VariantToDateTime(Value, dt) then
  begin
    Time.From(dt, false);
    Result := Time.i18nText;
  end
  else
    SetVariantNull(Result{%H-});
end;

class procedure TSynMustache.DateToText(const Value: variant;
  out Result: variant);
var
  Time: TTimeLogBits;
  dt: TDateTime;
begin
  if VariantToDateTime(Value, dt) then
  begin
    Time.From(dt, true);
    Result := Time.i18nText;
  end
  else
    SetVariantNull(Result{%H-});
end;

class procedure TSynMustache.DateFmt(const Value: variant;
  out Result: variant);
var
  dt: TDateTime;
begin
  // {{DateFmt DateValue,"dd/mm/yyy"}}
  with _Safe(Value)^ do
    if IsArray and
       (Count = 2) and
       VariantToDateTime(Values[0], dt) then
      Result := FormatDateTime(Values[1], dt)
    else
      SetVariantNull(Result{%H-});
end;

class procedure TSynMustache.TimeLogToText(const Value: variant;
  out Result: variant);
var
  Time: TTimeLogBits;
begin
  if VariantToInt64(Value, Time.Value) then
    Result := Time.i18nText
  else
    SetVariantNull(Result{%H-});
end;

class procedure TSynMustache.ToJson(const Value: variant;
  out Result: variant);
begin
  if not VarIsEmptyOrNull(Value) then
    RawUtf8ToVariant(JsonReformat(VariantToUtf8(Value)), Result);
end;

class procedure TSynMustache.JsonQuote(const Value: variant;
  out Result: variant);
var
  json: RawUtf8;
begin
  if not VarIsEmptyOrNull(Value) then
    // avoid to return "null"
    VariantToUtf8(Value, json);
  RawUtf8ToVariant(QuotedStrJson(json), Result);
end;

class procedure TSynMustache.JsonQuoteUri(const Value: variant;
  out Result: variant);
var
  json: RawUtf8;
begin
  if not VarIsEmptyOrNull(Value) then
    // avoid to return "null"
    VariantToUtf8(Value, json);
  RawUtf8ToVariant(UrlEncode(QuotedStrJson(json)), Result);
end;

procedure ToHtml(const Value: variant; var Result: variant;
  fmt: TTextWriterHtmlEscape; wiki: boolean = false);
var
  txt: RawUtf8;
  d: PDocVariantData;
begin
  // {{{SimpleToHtml content,browserhasnoemoji,nohtmlescape}}}
  d := _Safe(Value);
  if d^.IsArray and
     (d^.Count >= 2) then
  begin
    if VarIsEmptyOrNull(d^.Values[0]) then
      exit; // don't append 'null' text
    VariantToUtf8(d^.Values[0], txt);
    if not VarIsVoid(d^.Values[1]) then
      exclude(fmt, heEmojiToUtf8);
    if (d^.Count >= 3) and
       not VarIsVoid(d^.Values[2]) then
      exclude(fmt, heHtmlEscape);
  end
  else
    // {{{MarkdownToHtml content}}}
    if VarIsEmptyOrNull(Value) then
      exit
    else
      VariantToUtf8(Value, txt);
  if txt <> '' then
    if wiki then
      txt := HtmlEscapeWiki(txt, fmt)
    else
      txt := HtmlEscapeMarkdown(txt, fmt);
  RawUtf8ToVariant(txt, Result);
end;

class procedure TSynMustache.WikiToHtml(const Value: variant;
  out Result: variant);
begin
  ToHtml(Value, Result, [heHtmlEscape, heEmojiToUtf8], {wiki=}true);
end;

class procedure TSynMustache.MarkdownToHtml(const Value: variant;
  out Result: variant);
begin
  // default Markdown is to allow HTML tags
  ToHtml(Value, Result, [heEmojiToUtf8]);
end;

class procedure TSynMustache.SimpleToHtml(const Value: variant;
  out Result: variant);
begin
  ToHtml(Value, Result, [heHtmlEscape, heEmojiToUtf8]);
end;

class procedure TSynMustache.BlobToBase64(const Value: variant;
  out Result: variant);
var
  tmp: RawUtf8;
  wasString: boolean;
begin
  VariantToUtf8(Value, tmp, wasString);
  if wasString and
     (pointer(tmp) <> nil) then
  begin
    if PInteger(tmp)^ and $00ffffff = JSON_BASE64_MAGIC_C then
      delete(tmp, 1, 3);
    RawUtf8ToVariant(tmp, Result);
  end
  else
    Result := Value;
end;

class procedure TSynMustache.EnumTrim(const Value: variant;
  out Result: variant);
var
  tmp: RawUtf8;
  wasString: boolean;
  short: PUtf8Char;
begin
  VariantToUtf8(Value, tmp, wasString);
  if not wasString then
    exit;
  short := TrimLeftLowerCase(tmp);
  RawUtf8ToVariant(short, StrLen(short), Result);
end;

class procedure TSynMustache.EnumTrimRight(const Value: variant;
  out Result: variant);
var
  tmp: RawUtf8;
  wasString: boolean;
  i, L: integer;
begin
  VariantToUtf8(Value, tmp, wasString);
  if not wasString then
    exit;
  L := length(tmp);
  for i := 1 to L do
    if not (tmp[i] in ['a'..'z']) then
    begin
      L := i - 1;
      break;
    end;
  RawUtf8ToVariant(Pointer(tmp), L, Result);
end;

class procedure TSynMustache.PowerOfTwo(const Value: variant;
  out Result: variant);
var
  V: Int64;
begin
  if TVarData(Value).VType > varNull then
    if VariantToInt64(Value, V) then
      Result := Int64(1) shl V;
end;

class procedure TSynMustache.Equals_(const Value: variant;
  out Result: variant);
begin
  // {{#Equals .,12}}
  with _Safe(Value)^ do
    if IsArray and
       (Count = 2) and
       (FastVarDataComp(@Values[0], @Values[1], false) = 0) then
      Result := true
    else
      SetVariantNull(Result{%H-});
end;

class procedure TSynMustache.If_(const Value: variant; out Result: variant);
var
  cmp: integer;
  oper: RawUtf8;
  wasString: boolean;
begin
  // {{#if .<>""}} or {{#if .,"=",123}}
  SetVariantNull(result{%H-});
  with _Safe(Value)^ do
    if IsArray and
       (Count = 3) then
    begin
      VariantToUtf8(Values[1], oper, wasString);
      if wasString and
         (oper <> '') then
      begin
        cmp := FastVarDataComp(@Values[0], @Values[2], false);
        case PWord(oper)^ of
          ord('='):
            if cmp = 0 then
              result := True;
          ord('>'):
            if cmp > 0 then
              result := True;
          ord('<'):
            if cmp < 0 then
              result := True;
          ord('>') + ord('=') shl 8:
            if cmp >= 0 then
              result := True;
          ord('<') + ord('=') shl 8:
            if cmp <= 0 then
              result := True;
          ord('<') + ord('>') shl 8:
            if cmp <> 0 then
              result := True;
        end;
      end;
    end;
end;

class procedure TSynMustache.NewGuid(const Value: variant;
  out Result: variant);
var
  g: TGUID;
begin
  RandomGuid(g);
  RawUtf8ToVariant(GuidToRawUtf8(g), Result);
end;

class procedure TSynMustache.ExtractFileName(const Value: variant;
  out Result: variant);
begin
  Result := SysUtils.ExtractFileName(Value);
end;

class procedure TSynMustache.Lower(const Value: variant;
  out Result: variant);
begin
  Result := SysUtils.LowerCase(Value);
end;

class procedure TSynMustache.Upper(const Value: variant;
  out Result: variant);
begin
  Result := SysUtils.UpperCase(Value);
end;



end.
