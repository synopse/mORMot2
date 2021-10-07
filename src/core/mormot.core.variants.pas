/// Framework Core Low-Level Variants / TDocVariant process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.variants;

{
  *****************************************************************************

  Variant / TDocVariant feature shared by all framework units
  - Low-Level Variant Wrappers
  - Custom Variant Types with JSON support
  - TDocVariant Object/Array Document Holder with JSON support
  - JSON Parsing into Variant
  - Variant Binary Serialization

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data, // already included in mormot.core.json
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.json;

  
{ ************** Low-Level Variant Wrappers }

type
  /// exception class raised by this unit during raw Variant process
  ESynVariant = class(ESynException);

const
  {$ifdef HASVARUSTRING}
  varFirstCustom = varUString + 1;
  {$else}
  varFirstCustom = varAny + 1;
  {$endif HASVARUSTRING}

/// fastcheck if a variant hold a value
// - varEmpty, varNull or a '' string would be considered as void
// - varBoolean=false or varDate=0 would be considered as void
// - a TDocVariantData with Count=0 would be considered as void
// - any other value (e.g. integer) would be considered as not void
function VarIsVoid(const V: Variant): boolean;

/// returns a supplied string as variant, or null if v is void ('')
function VarStringOrNull(const v: RawUtf8): variant;

type
  /// a set of simple TVarData.VType, as specified to VarIs()
  TVarDataTypes = set of 0..255;

/// allow to check for a specific set of TVarData.VType
function VarIs(const V: Variant; const VTypes: TVarDataTypes): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// same as Dest := Source, but copying by reference
// - i.e. VType is defined as varVariant or varByRef / varVariantByRef
// - for instance, it will be used for late binding of TDocVariant properties,
// to let following statements work as expected:
// ! V := _Json('{arr:[1,2]}');
// ! V.arr.Add(3);   // will work, since V.arr will be returned by reference
// ! writeln(V);     // will write '{"arr":[1,2,3]}'
procedure SetVariantByRef(const Source: Variant; var Dest: Variant);

/// same as Dest := Source, but copying by value
// - will unreference any varByRef content
// - will convert any string value into RawUtf8 (varString) for consistency
procedure SetVariantByValue(const Source: Variant; var Dest: Variant);

/// same as FillChar(Value^,SizeOf(TVarData),0)
// - so can be used for TVarData or Variant
// - it will set V.VType := varEmpty, so Value will be Unassigned
// - it won't call VarClear(variant(Value)): it should have been cleaned before
procedure ZeroFill(Value: PVarData);
  {$ifdef HASINLINE}inline;{$endif}

/// fill all bytes of the value's memory buffer with zeros, i.e. 'toto' -> #0#0#0#0
// - may be used to cleanup stack-allocated content
procedure FillZero(var value: variant); overload;

/// convert a FormatUtf8() UTF-8 encoded string into a variant RawUtf8 varString
procedure FormatUtf8ToVariant(const Fmt: RawUtf8; const Args: array of const;
  var Value: variant);

/// convert an UTF-8 encoded text buffer into a variant RawUtf8 varString
// - this overloaded version expects a destination variant type (e.g. varString
// varOleStr / varUString) - if the type is not handled, will raise an
// EVariantTypeCastError
procedure RawUtf8ToVariant(const Txt: RawUtf8; var Value: TVarData;
  ExpectedValueType: cardinal); overload;

/// convert an open array (const Args: array of const) argument to a variant
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - vt*String or vtVariant arguments are returned as varByRef
procedure VarRecToVariant(const V: TVarRec; var result: variant); overload;

/// convert an open array (const Args: array of const) argument to a variant
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - vt*String or vtVariant arguments are returned as varByRef
function VarRecToVariant(const V: TVarRec): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a variant to an open array (const Args: array of const) argument
// - will always map to a vtVariant kind of argument
procedure VariantToVarRec(const V: variant; var result: TVarRec);
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant into a VCL string type
// - expects any varString value to be stored as a RawUtf8
// - prior to Delphi 2009, use VariantToString(aVariant) instead of
// string(aVariant) to safely retrieve a string=AnsiString value from a variant
// generated by our framework units - otherwise, you may loose encoded characters
// - for Unicode versions of Delphi, there won't be any potential data loss,
// but this version may be slightly faster than a string(aVariant)
function VariantToString(const V: Variant): string;

/// convert a dynamic array of variants into its JSON serialization
// - will use a TDocVariantData temporary storage
function VariantDynArrayToJson(const V: TVariantDynArray): RawUtf8;

/// convert a JSON array into a dynamic array of variants
// - will use a TDocVariantData temporary storage
function JsonToVariantDynArray(const Json: RawUtf8): TVariantDynArray;

/// convert an open array list into a dynamic array of variants
// - will use a TDocVariantData temporary storage
function ValuesToVariantDynArray(const items: array of const): TVariantDynArray;

type
  /// function prototype used internally for variant comparison
  // - as used e.g. by TDocVariantData.SortByValue
  TVariantCompare = function(const V1, V2: variant): PtrInt;
  /// function prototype used internally for extended variant comparison
  // - as used by TDocVariantData.SortByRow
  TVariantComparer = function(const V1, V2: variant): PtrInt of object;
  /// function prototype used internally for extended variant comparison
  // - as used by TDocVariantData.SortArrayByFields
  TVariantCompareField = function(const FieldName: RawUtf8;
    const V1, V2: variant): PtrInt of object;

/// internal function as called by inlined VariantCompare/VariantCompareI and
// the SortDynArrayVariantComp() function overriden by this unit
function FastVarDataComp(A, B: PVarData; caseInsensitive: boolean): integer;

/// TVariantCompare-compatible case-sensitive comparison function
// - just a wrapper around FastVarDataComp(caseInsensitive=false)
function VariantCompare(const V1, V2: variant): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// TVariantCompare-compatible case-insensitive comparison function
// - just a wrapper around FastVarDataComp(caseInsensitive=true)
function VariantCompareI(const V1, V2: variant): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison of a Variant and UTF-8 encoded String (or number)
// - slightly faster than plain V=Str, which computes a temporary variant
// - here Str='' equals unassigned, null or false
// - if CaseSensitive is false, will use IdemPropNameU() for comparison
function VariantEquals(const V: Variant; const Str: RawUtf8;
  CaseSensitive: boolean = true): boolean; overload;


{ ************** Custom Variant Types with JSON support }

type
  /// define how our custom variant types behave, i.e. its methods featureset
  TSynInvokeableVariantTypeOptions = set of (
    sioHasTryJsonToVariant,
    sioHasToJson,
    sioCanIterate);

  /// custom variant handler with easier/faster access of variant properties,
  // and JSON serialization support
  // - default GetProperty/SetProperty methods are called via some protected
  // virtual IntGet/IntSet methods, with less overhead (to be overriden)
  // - these kind of custom variants will be faster than the default
  // TInvokeableVariantType for properties getter/setter, but you should
  // manually register each type by calling SynRegisterCustomVariantType()
  // - also feature custom JSON parsing, via TryJsonToVariant() protected method
  TSynInvokeableVariantType = class(TInvokeableVariantType)
  protected
    fOptions: TSynInvokeableVariantTypeOptions;
    {$ifdef ISDELPHI}
    /// our custom call backs do not want the function names to be uppercased
    function FixupIdent(const AText: string): string; override;
    {$endif ISDELPHI}
    // intercept for a faster direct IntGet/IntSet calls
    // - note: SetProperty/GetProperty are never called by this class/method
    // - also circumvent FPC 3.2+ inverted parameters order
    {$ifdef FPC_VARIANTSETVAR}
    procedure DispInvoke(Dest: PVarData; var Source: TVarData;
    {$else} // see http://mantis.freepascal.org/view.php?id=26773
    {$ifdef ISDELPHIXE7}
    procedure DispInvoke(Dest: PVarData; [ref] const Source: TVarData;
    {$else}
    procedure DispInvoke(Dest: PVarData; const Source: TVarData;
    {$endif ISDELPHIXE7}
    {$endif FPC_VARIANTSETVAR}
      CallDesc: PCallDesc; Params: Pointer); override;
    /// override those abstract methods for getter/setter implementation
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean; virtual;
    function IntSet(const Instance, Value: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; virtual;
  public
    /// virtual constructor which should set the custom type Options
    constructor Create; virtual;
    /// search of a registered custom variant type from its low-level VarType
    // - will first compare with its own VarType for efficiency
    function FindSynVariantType(aVarType: Word;
      out CustomType: TSynInvokeableVariantType): boolean;
    /// customization of JSON parsing into variants
    // - is enabled only if the sioHasTryJsonToVariant option is set
    // - will be called by e.g. by VariantLoadJson() or GetVariantFromJson()
    // with Options: PDocVariantOptions parameter not nil
    // - this default implementation will always returns FALSE,
    // meaning that the supplied JSON is not to be handled by this custom
    // (abstract) variant type
    // - this method could be overridden to identify any custom JSON content
    // and convert it into a dedicated variant instance, then return TRUE
    // - warning: should NOT modify JSON buffer in-place, unless it returns true
    function TryJsonToVariant(var Json: PUtf8Char; var Value: variant;
      EndOfObject: PUtf8Char): boolean; virtual;
    /// customization of variant into JSON serialization
    procedure ToJson(W: TTextWriter; const Value: variant); virtual;
    /// clear the content
    // - this default implementation will set VType := varEmpty
    // - override it if your custom type needs to manage its internal memory
    procedure Clear(var V: TVarData); override;
    /// copy two variant content
    // - this default implementation will copy the TVarData memory
    // - override it if your custom type needs to manage its internal structure
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: boolean); override;
    /// copy two variant content by value
    // - this default implementation will call the Copy() method
    // - override it if your custom types may use a by reference copy pattern
    procedure CopyByValue(var Dest: TVarData;
      const Source: TVarData); virtual;
    /// this method will allow to look for dotted name spaces, e.g. 'parent.child'
    // - should return Unassigned if the FullName does not match any value
    // - will identify TDocVariant storage, or resolve and call the generic
    // TSynInvokeableVariantType.IntGet() method until nested value match
    procedure Lookup(var Dest: TVarData; const Instance: TVarData;
      FullName: PUtf8Char);
    /// will check if the value is an array, and return the number of items
    // - if the document is an array, will return the items count (0 meaning
    // void array) - used e.g. by TSynMustacheContextVariant
    // - this default implementation will return -1 (meaning this is not an array)
    // - overridden method could implement it, e.g. for TDocVariant of kind dvArray
    function IterateCount(const V: TVarData): integer; virtual;
    /// allow to loop over an array document
    // - Index should be in 0..IterateCount-1 range
    // - this default implementation will do nothing
    procedure Iterate(var Dest: TVarData; const V: TVarData;
      Index: integer); virtual;
    /// returns TRUE if the supplied variant is of the exact custom type
    function IsOfType(const V: variant): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// identify how this custom type behave
    // - as set by the class constructor, to avoid calling any virtual method
    property Options: TSynInvokeableVariantTypeOptions
      read fOptions;
  end;

  /// class-reference type (metaclass) of custom variant type definition
  // - used by SynRegisterCustomVariantType() function
  TSynInvokeableVariantTypeClass = class of TSynInvokeableVariantType;

/// register a custom variant type to handle properties
// - the registration process is thread-safe
// - this will implement an internal mechanism used to bypass the default
// _DispInvoke() implementation in Variant.pas, to use a faster version
// - is called in case of TDocVariant, TBsonVariant or TSqlDBRowVariant
function SynRegisterCustomVariantType(
  aClass: TSynInvokeableVariantTypeClass): TSynInvokeableVariantType;

/// try to serialize a custom variant value into JSON
// - as used e.g. by TTextWriter.AddVariant
function CustomVariantToJson(W: TTextWriter; const Value: variant;
  Escape: TTextWriterKind): boolean;


{ ************** TDocVariant Object/Array Document Holder with JSON support }

type
  /// JSON_[] constant convenient TDocVariant options
  // - mVoid defines a safe (and slow) full-copy behavior with [] (no option)
  // - mDefault defines a safe (and slow) full-copy behavior, returning null
  // for unknown fields, as defined e.g. by _Json() and _JsonFmt() functions
  // or JSON_OPTIONS[false]
  // - mFast will copy-by-reference any TDocVariantData content, as defined
  // e.g. by _JsonFast() and _JsonFastFmt() functions or JSON_OPTIONS[true]
  // - mFastFloat will copy-by-reference and can parse floating points as double
  // - mFastStrict will copy-by-reference and only parse strict (quoted) JSON,
  // as defined by JSON_FAST_STRICT global variable
  // - mFastExtended will copy-by-reference and write extended (unquoted) JSON,
  // as defined by JSON_FAST_EXTENDED global variable
  // - mFastExtendedIntern will copy-by-reference, write extended JSON and
  // intern names and values, as defined by JSON_FAST_EXTENDEDINTERN variable
  // - mNameValue will copy-by-reference and check field names case-sensitively,
  // as defined by JSON_NAMEVALUE[false] global variable
  // - mNameValueExtended will copy-by-reference, check field names
  // case-sensitively and write extended (unquoted) JSON,
  // as defined by JSON_NAMEVALUE[true] global variable
  // - mNameValueIntern will copy-by-reference, check field names
  // case-sensitively and intern names and values,
  // as defined by JSON_NAMEVALUEINTERN[false] global variable
  // - mNameValueInternExtended will copy-by-reference, check field names
  // case-sensitively, write extended JSON and intern names and values,
  // as defined by JSON_NAMEVALUEINTERN[true] global variable
  TDocVariantModel = (
    mVoid,
    mDefault,
    mFast,
    mFastFloat,
    mFastStrict,
    mFastExtended,
    mFastExtendedIntern,
    mNameValue,
    mNameValueExtended,
    mNameValueIntern,
    mNameValueInternExtended);

var
  /// some convenient TDocVariant options, e.g. as JSON_[fDefault]
  JSON_: array[TDocVariantModel] of TDocVariantOptions = (
  // mVoid
    [],
  // mDefault
    [dvoReturnNullForUnknownProperty],
  // mFast
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference],
  // mFastFloat
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoAllowDoubleValue],
  // mFastStrict
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoJsonParseDoNotTryCustomVariants],
  // mFastExtended
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoSerializeAsExtendedJson],
  // mFastExtendedIntern
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoSerializeAsExtendedJson,
     dvoJsonParseDoNotTryCustomVariants,
     dvoInternNames,
     dvoInternValues],
  // mNameValue
     [dvoReturnNullForUnknownProperty,
      dvoValueCopiedByReference,
      dvoNameCaseSensitive],
  // mNameValueExtended
     [dvoReturnNullForUnknownProperty,
      dvoValueCopiedByReference,
      dvoNameCaseSensitive,
      dvoSerializeAsExtendedJson],
  // mNameValueIntern
     [dvoReturnNullForUnknownProperty,
      dvoValueCopiedByReference,
      dvoNameCaseSensitive,
      dvoInternNames,
      dvoInternValues],
  // mNameValueInternExtended
     [dvoReturnNullForUnknownProperty,
      dvoValueCopiedByReference,
      dvoNameCaseSensitive,
      dvoInternNames,
      dvoInternValues,
      dvoSerializeAsExtendedJson]
    );

const
  /// same as JSON_[mFast], but can not be used as PDocVariantOptions
  // - handle only currency for floating point values: use JSON_FAST_FLOAT
  // if you want to support double values, with potential precision loss
  JSON_FAST =
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference];

  /// same as JSON_FAST, but including dvoAllowDoubleValue for floating
  // point values parsing into double, with potential precision loss
  JSON_FAST_FLOAT =
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoAllowDoubleValue];

var
  /// TDocVariant options which may be used for plain JSON parsing
  // - this won't recognize any extended syntax
  JSON_FAST_STRICT: TDocVariantOptions;

  /// TDocVariant options to be used so that JSON serialization would
  // use the unquoted JSON syntax for field names
  // - you could use it e.g. on a TOrm variant published field to
  // reduce the JSON escape process during storage in the database, by
  // customizing your TOrmModel instance:
  // !  (aModel.Props[TOrmMyRecord]['VariantProp'] as TOrmPropInfoRttiVariant).
  // !    DocVariantOptions := JSON_FAST_EXTENDED;
  // or - in a cleaner way - by overriding TOrm.InternalDefineModel():
  // ! class procedure TOrmMyRecord.InternalDefineModel(Props: TOrmProperties);
  // ! begin
  // !   (Props.Fields.ByName('VariantProp') as TOrmPropInfoRttiVariant).
  // !     DocVariantOptions := JSON_FAST_EXTENDED;
  // ! end;
  // or to set all variant fields at once:
  // ! class procedure TOrmMyRecord.InternalDefineModel(Props: TOrmProperties);
  // ! begin
  // !   Props.SetVariantFieldsDocVariantOptions(JSON_FAST_EXTENDED);
  // ! end;
  // - consider using JSON_NAMEVALUE[true] for case-sensitive
  // TSynNameValue-like storage, or JSON_FAST_EXTENDEDINTERN if you
  // expect RawUtf8 names and values interning
  JSON_FAST_EXTENDED: TDocVariantOptions;

  /// TDocVariant options for JSON serialization with efficient storage
  // - i.e. unquoted JSON syntax for field names and RawUtf8 interning
  // - may be used e.g. for efficient persistence of similar data
  // - consider using JSON_FAST_EXTENDED if you don't expect
  // RawUtf8 names and values interning, or need BSON variants parsing
  JSON_FAST_EXTENDEDINTERN: TDocVariantOptions;

  /// TDocVariant options to be used for case-sensitive TSynNameValue-like
  // storage, with optional extended JSON syntax serialization
  // - consider using JSON_FAST_EXTENDED for case-insensitive objects
  JSON_NAMEVALUE: TDocVariantOptionsBool;

  /// TDocVariant options to be used for case-sensitive TSynNameValue-like
  // storage, RawUtf8 interning and optional extended JSON syntax serialization
  // - consider using JSON_FAST_EXTENDED for case-insensitive objects,
  // or JSON_NAMEVALUE[] if you don't expect names and values interning
  JSON_NAMEVALUEINTERN: TDocVariantOptionsBool;

  // - JSON_OPTIONS[false] is e.g. _Json() and _JsonFmt() functions default
  // - JSON_OPTIONS[true] are used e.g. by _JsonFast() and _JsonFastFmt() functions
  // - handle only currency for floating point values: use JSON_FAST_FLOAT/JSON_[mFastFloat]
  // if you want to support double values, with potential precision loss
  JSON_OPTIONS: TDocVariantOptionsBool;

// some slightly more verbose backward compatible options
{$ifndef PUREMORMOT2}
  JSON_OPTIONS_FAST_STRICT: TDocVariantOptions
    absolute JSON_FAST_STRICT;
  JSON_OPTIONS_NAMEVALUE: TDocVariantOptionsBool
    absolute JSON_NAMEVALUE;
  JSON_OPTIONS_NAMEVALUEINTERN: TDocVariantOptionsBool
    absolute JSON_NAMEVALUEINTERN;
  JSON_OPTIONS_FAST_EXTENDED: TDocVariantOptions
    absolute JSON_FAST_EXTENDED;
  JSON_OPTIONS_FAST_EXTENDEDINTERN: TDocVariantOptions
    absolute JSON_FAST_EXTENDEDINTERN;

const
  JSON_OPTIONS_FAST = JSON_FAST;
  JSON_OPTIONS_FAST_FLOAT = JSON_FAST_FLOAT;
{$endif PUREMORMOT2}


type
  /// pointer to a TDocVariant storage
  // - since variants may be stored by reference (i.e. as varByRef), it may
  // be a good idea to use such a pointer via DocVariantData(aVariant)^ or
  // _Safe(aVariant)^ instead of TDocVariantData(aVariant),
  // if you are not sure how aVariant was allocated (may be not _Obj/_Json)
  // - note: due to a local variable lifetime change in Delphi 11, don't use
  // this function with a temporary variant (e.g. from TList<variant>.GetItem) -
  // call _DV() and a local TDocVariantData instead of a PDocVariantData
  PDocVariantData = ^TDocVariantData;

  /// define the TDocVariant storage layout
  // - if it has one or more named properties, it is a dvObject
  // - if it has no name property, it is a dvArray
  TDocVariantKind = (
    dvUndefined,
    dvObject,
    dvArray);

  /// exception class associated to TDocVariant JSON/BSON document
  EDocVariant = class(ESynException)
  protected
    class procedure RaiseSafe(Kind: TDocVariantKind);
  end;

  /// a custom variant type used to store any JSON/BSON document-based content
  // - i.e. name/value pairs for objects, or an array of values (including
  // nested documents), stored in a TDocVariantData memory structure
  // - you can use _Obj()/_ObjFast() _Arr()/_ArrFast() _Json()/_JsonFast() or
  // _JsonFmt()/_JsonFastFmt() functions to create instances of such variants
  // - property access may be done via late-binding - with some restrictions
  // for older versions of FPC, e.g. allowing to write:
  // ! TDocVariant.NewFast(aVariant);
  // ! aVariant.Name := 'John';
  // ! aVariant.Age := 35;
  // ! writeln(aVariant.Name,' is ',aVariant.Age,' years old');
  // - it also supports a small set of pseudo-properties or pseudo-methods:
  // ! aVariant._Count = DocVariantData(aVariant).Count
  // ! aVariant._Kind = ord(DocVariantData(aVariant).Kind)
  // ! aVariant._JSON = DocVariantData(aVariant).JSON
  // ! aVariant._(i) = DocVariantData(aVariant).Value[i]
  // ! aVariant.Value(i) = DocVariantData(aVariant).Value[i]
  // ! aVariant.Value(aName) = DocVariantData(aVariant).Value[aName]
  // ! aVariant.Name(i) = DocVariantData(aVariant).Name[i]
  // ! aVariant.Add(aItem) = DocVariantData(aVariant).AddItem(aItem)
  // ! aVariant._ := aItem = DocVariantData(aVariant).AddItem(aItem)
  // ! aVariant.Add(aName,aValue) = DocVariantData(aVariant).AddValue(aName,aValue)
  // ! aVariant.Exists(aName) = DocVariantData(aVariant).GetValueIndex(aName)>=0
  // ! aVariant.Delete(i) = DocVariantData(aVariant).Delete(i)
  // ! aVariant.Delete(aName) = DocVariantData(aVariant).Delete(aName)
  // ! aVariant.NameIndex(aName) = DocVariantData(aVariant).GetValueIndex(aName)
  // - it features direct JSON serialization/unserialization, e.g.:
  // ! assert(_Json('["one",2,3]')._JSON='["one",2,3]');
  // - it features direct trans-typing into a string encoded as JSON, e.g.:
  // ! assert(_Json('["one",2,3]')='["one",2,3]');
  TDocVariant = class(TSynInvokeableVariantType)
  protected
    /// name and values interning are shared among all TDocVariantData instances
    fInternNames, fInternValues: TRawUtf8Interning;
    function CreateInternNames: TRawUtf8Interning;
    function CreateInternValues: TRawUtf8Interning;
    /// fast getter implementation
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean; override;
    /// fast setter implementation
    function IntSet(const Instance, Value: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; override;
  public
    /// initialize a variant instance to store some document-based content
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set aOptions=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    class procedure New(out aValue: variant;
      aOptions: TDocVariantOptions = []); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store per-reference document-based content
    // - same as New(aValue, JSON_FAST);
    // - to be used e.g. as
    // !var v: variant;
    // !begin
    // !  TDocVariant.NewFast(v);
    // !  ...
    class procedure NewFast(out aValue: variant;
      aKind: TDocVariantKind = dvUndefined); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// ensure a variant is a TDocVariant instance
    // - if aValue is not a TDocVariant, will create a new JSON_FAST
    class procedure IsOfTypeOrNewFast(var aValue: variant);
    /// initialize several variant instances to store document-based content
    // - replace several calls to TDocVariantData.InitFast
    // - to be used e.g. as
    // !var v1,v2,v3: TDocVariantData;
    // !begin
    // !  TDocVariant.NewFast([@v1,@v2,@v3]);
    // !  ...
    class procedure NewFast(const aValues: array of PDocVariantData;
      aKind: TDocVariantKind = dvUndefined); overload;
    /// initialize a variant instance to store some document-based content
    // - you can use this function to create a variant, which can be nested into
    // another document, e.g.:
    // ! aVariant := TDocVariant.New;
    // ! aVariant.id := 10;
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set Options=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    // - in practice, you should better use _Obj()/_ObjFast() _Arr()/_ArrFast()
    // functions or TDocVariant.NewFast()
    class function New(Options: TDocVariantOptions = []): variant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store some document-based object content
    // - object will be initialized with data supplied two by two, as Name,Value
    // pairs, e.g.
    // ! aVariant := TDocVariant.NewObject(['name','John','year',1972]);
    // which is the same as:
    // ! TDocVariant.New(aVariant);
    // ! TDocVariantData(aVariant).AddValue('name','John');
    // ! TDocVariantData(aVariant).AddValue('year',1972);
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set Options=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    // - in practice, you should better use the function _Obj() which is a
    // wrapper around this class method
    class function NewObject(const NameValuePairs: array of const;
      Options: TDocVariantOptions = []): variant;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied as parameters, e.g.
    // ! aVariant := TDocVariant.NewArray(['one',2,3.0]);
    // which is the same as:
    // ! TDocVariant.New(aVariant);
    // ! TDocVariantData(aVariant).AddItem('one');
    // ! TDocVariantData(aVariant).AddItem(2);
    // ! TDocVariantData(aVariant).AddItem(3.0);
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set aOptions=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    // - in practice, you should better use the function _Arr() which is a
    // wrapper around this class method
    class function NewArray(const Items: array of const;
      Options: TDocVariantOptions = []): variant; overload;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied dynamic array of variants
    class function NewArray(const Items: TVariantDynArray;
      Options: TDocVariantOptions = []): variant; overload;
    /// initialize a variant instance to store some document-based object content
    // from a supplied (extended) JSON content
    // - in addition to the JSON RFC specification strict mode, this method will
    // handle some BSON-like extensions, e.g. unquoted field names
    // - a private copy of the incoming JSON buffer will be used, then
    // it will call the TDocVariantData.InitJsonInPlace() method
    // - to be used e.g. as:
    // ! var V: variant;
    // ! begin
    // !   V := TDocVariant.NewJson('{"id":10,"doc":{"name":"John","birthyear":1972}}');
    // !   assert(V.id=10);
    // !   assert(V.doc.name='John');
    // !   assert(V.doc.birthYear=1972);
    // !   // and also some pseudo-properties:
    // !   assert(V._count=2);
    // !   assert(V.doc._kind=ord(dvObject));
    // - or with a JSON array:
    // !   V := TDocVariant.NewJson('["one",2,3]');
    // !   assert(V._kind=ord(dvArray));
    // !   for i := 0 to V._count-1 do
    // !     writeln(V._(i));
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, add dvoValueCopiedByReference in Options
    // will increase the process speed a lot
    // - in practice, you should better use the function _Json()/_JsonFast()
    // which are handy wrappers around this class method
    class function NewJson(const Json: RawUtf8;
      Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store some document-based object content
    // from a supplied existing TDocVariant instance
    // - use it on a value returned as varByRef (e.g. by _() pseudo-method),
    // to ensure the returned variant will behave as a stand-alone value
    // - for instance, the following:
    // !  oSeasons := TDocVariant.NewUnique(o.Seasons);
    // is the same as:
    // ! 	oSeasons := o.Seasons;
    // !  _Unique(oSeasons);
    // or even:
    // !  oSeasons := _Copy(o.Seasons);
    class function NewUnique(const SourceDocVariant: variant;
      Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant;
      {$ifdef HASINLINE}inline;{$endif}
    /// will return the unique element of a TDocVariant array or a default
    // - if the value is a dvArray with one single item, it will this value
    // - if the value is not a TDocVariant nor a dvArray with one single item,
    // it wil return the default value
    class procedure GetSingleOrDefault(const docVariantArray, default: variant;
      var result: variant);

    /// finalize the stored information
    destructor Destroy; override;
    /// used by dvoInternNames for string interning of all Names[] values
    function InternNames: TRawUtf8Interning;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by dvoInternValues for string interning of all RawUtf8 Values[]
    function InternValues: TRawUtf8Interning;
      {$ifdef HASINLINE}inline;{$endif}
    // this implementation will write the content as JSON object or array
    procedure ToJson(W: TTextWriter; const Value: variant); override;
    /// will check if the value is an array, and return the number of items
    // - if the document is an array, will return the items count (0 meaning
    // void array) - used e.g. by TSynMustacheContextVariant
    // - this overridden method will implement it for dvArray instance kind
    function IterateCount(const V: TVarData): integer; override;
    /// allow to loop over an array document
    // - Index should be in 0..IterateCount-1 range
    // - this default implementation will do handle dvArray instance kind
    procedure Iterate(var Dest: TVarData; const V: TVarData;
      Index: integer); override;
    /// low-level callback to access internal pseudo-methods
    // - mainly the _(Index: integer): variant method to retrieve an item
    // if the document is an array
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): boolean; override;
    /// low-level callback to access internal pseudo-methods
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): boolean; override;
    /// low-level callback to clear the content
    procedure Clear(var V: TVarData); override;
    /// low-level callback to copy two variant content
    // - such copy will by default be done by-value, for safety
    // - if you are sure you will use the variants as read-only, you can set
    // the dvoValueCopiedByReference Option to use faster by-reference copy
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: boolean); override;
    /// copy two variant content by value
    // - overridden method since instance may use a by-reference copy pattern
    procedure CopyByValue(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    /// compare two variant values
    // - redirect to case-sensitive FastVarDataComp() comparison
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
  end;

  /// method used by TDocVariantData.ReduceAsArray to filter each object
  // - should return TRUE if the item match the expectations
  TOnReducePerItem = function(Item: PDocVariantData): boolean of object;

  /// method used by TDocVariantData.ReduceAsArray to filter each object
  // - should return TRUE if the item match the expectations
  TOnReducePerValue = function(const Value: variant): boolean of object;

  {$ifdef HASITERATORS}
  /// internal state engine used by TDocVariant enumerators records
  TDocVariantEnumeratorState = record
  private
    Curr, After: PVariant;
  public
    procedure Init(Values: PVariantArray; Count: PtrUInt); inline;
    procedure Void; inline;
    function MoveNext: Boolean; inline;
  end;

  /// local iterated name/value pair as returned by TDocVariantData.GetEnumerator
  // and TDocVariantData.Fields
  // - we use pointers for best performance - but warning: Name may be nil for
  // TDocVariantData.GetEnumerator over an array
  TDocVariantFields = record
    /// points to current Name[] - nil if the TDocVariantData is an array
    Name: PRawUtf8;
    /// points to the current Value[] - never nil
    Value: PVariant;
  end;

  /// low-level Enumerator as returned by TDocVariantData.GetEnumerator
  // (default "for .. in dv do") and TDocVariantData.Fields
  TDocVariantFieldsEnumerator = record
  private
    State: TDocVariantEnumeratorState;
    Name: PRawUtf8;
    function GetCurrent: TDocVariantFields; inline;
  public
    function MoveNext: Boolean; inline;
    function GetEnumerator: TDocVariantFieldsEnumerator; inline;
    /// returns the current Name/Value or Value as pointers in TDocVariantFields
    property Current: TDocVariantFields
      read GetCurrent;
  end;

  /// low-level Enumerator as returned by TDocVariantData.FieldNames
  TDocVariantFieldNamesEnumerator = record
  private
    Curr, After: PRawUtf8;
  public
    function MoveNext: Boolean; inline;
    function GetEnumerator: TDocVariantFieldNamesEnumerator; inline;
    /// returns the current Name/Value or Value as pointers in TDocVariantFields
    property Current: PRawUtf8
      read Curr;
  end;

  /// low-level Enumerator as returned by TDocVariantData.Items and FieldValues
  TDocVariantItemsEnumerator = record
  private
    State: TDocVariantEnumeratorState;
  public
    function MoveNext: Boolean; inline;
    function GetEnumerator: TDocVariantItemsEnumerator; inline;
    /// returns the current Value as pointer
    property Current: PVariant
      read State.Curr;
  end;

  /// low-level Enumerator as returned by TDocVariantData.Objects
  TDocVariantObjectsEnumerator = record
  private
    State: TDocVariantEnumeratorState;
    Value: PDocVariantData;
  public
    function MoveNext: Boolean; inline;
    function GetEnumerator: TDocVariantObjectsEnumerator; inline;
    /// returns the current Value as pointer to each TDocVariantData object
    property Current: PDocVariantData
      read Value;
  end;
  {$endif HASITERATORS}

  {$A-} { packet object not allowed since Delphi 2009 :( }
  /// memory structure used for TDocVariant storage of any JSON/BSON
  // document-based content as variant
  // - i.e. name/value pairs for objects, or an array of values (including
  // nested documents)
  // - you can use _Obj()/_ObjFast() _Arr()/_ArrFast() _Json()/_JsonFast() or
  // _JsonFmt()/_JsonFastFmt() functions to create instances of such variants
  // - you can transtype such an allocated variant into TDocVariantData
  // to access directly its internals (like Count or Values[]/Names[]):
  // ! aVariantObject := TDocVariant.NewObject(['name','John','year',1972]);
  // ! aVariantObject := _ObjFast(['name','John','year',1972]);
  // ! with _Safe(aVariantObject)^ do
  // !   for i := 0 to Count-1 do
  // !     writeln(Names[i],'=',Values[i]); // for an object
  // ! aVariantArray := TDocVariant.NewArray(['one',2,3.0]);
  // ! aVariantArray := _JsonFast('["one",2,3.0]');
  // ! with _Safe(aVariantArray)^ do
  // !   for i := 0 to Count-1 do
  // !     writeln(Values[i]); // for an array
  // - use "with _Safe(...)^ do"  and not "with TDocVariantData(...) do" as the
  // former will handle internal variant redirection (varByRef), e.g. from late
  // binding or assigned another TDocVariant
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}
  TDocVariantData = record
  {$else}
  TDocVariantData = object
  {$endif USERECORDWITHMETHODS}
  private
    // note: this structure uses all TVarData available space: no filler needed!
    VType: TVarType;              // 16-bit
    VOptions: TDocVariantOptions; // 16-bit
    VName: TRawUtf8DynArray;      // pointer
    VValue: TVariantDynArray;     // pointer
    VCount: integer;              // 32-bit
    // retrieve the value as varByRef
    function GetValueOrItem(const aNameOrIndex: variant): variant;
    procedure SetValueOrItem(const aNameOrIndex, aValue: variant);
    // kind is stored as dvoIsArray/dvoIsObject within VOptions
    function GetKind: TDocVariantKind;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetOptions(const opt: TDocVariantOptions); // keep dvoIsObject/Array
      {$ifdef HASINLINE}inline;{$endif}
    // capacity is Length(VValue) and Length(VName)
    procedure SetCapacity(aValue: integer);
    function GetCapacity: integer;
      {$ifdef HASINLINE}inline;{$endif}
    // implement U[] I[] B[] D[] O[] O_[] A[] A_[] _[] properties
    function GetOrAddIndexByName(const aName: RawUtf8): integer;
      {$ifdef HASINLINE}inline;{$endif}
    function GetOrAddPVariantByName(const aName: RawUtf8): PVariant;
      {$ifdef HASINLINE}inline;{$endif}
    function GetPVariantByName(const aName: RawUtf8): PVariant;
    function GetRawUtf8ByName(const aName: RawUtf8): RawUtf8;
    procedure SetRawUtf8ByName(const aName, aValue: RawUtf8);
    function GetStringByName(const aName: RawUtf8): string;
    procedure SetStringByName(const aName: RawUtf8; const aValue: string);
    function GetInt64ByName(const aName: RawUtf8): Int64;
    procedure SetInt64ByName(const aName: RawUtf8; const aValue: Int64);
    function GetBooleanByName(const aName: RawUtf8): boolean;
    procedure SetBooleanByName(const aName: RawUtf8; aValue: boolean);
    function GetDoubleByName(const aName: RawUtf8): Double;
    procedure SetDoubleByName(const aName: RawUtf8; const aValue: Double);
    function GetDocVariantExistingByName(const aName: RawUtf8;
      aNotMatchingKind: TDocVariantKind): PDocVariantData;
    function GetObjectExistingByName(const aName: RawUtf8): PDocVariantData;
    function GetDocVariantOrAddByName(const aName: RawUtf8;
      aKind: TDocVariantKind): PDocVariantData;
    function GetObjectOrAddByName(const aName: RawUtf8): PDocVariantData;
    function GetArrayExistingByName(const aName: RawUtf8): PDocVariantData;
    function GetArrayOrAddByName(const aName: RawUtf8): PDocVariantData;
    function GetAsDocVariantByIndex(aIndex: integer): PDocVariantData;
    procedure ClearFast;
  public
    /// initialize a TDocVariantData to store some document-based content
    // - can be used with a stack-allocated TDocVariantData variable:
    // !var Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.Init;
    // !  Doc.AddValue('name','John');
    // !  assert(Doc.Value['name']='John');
    // !  assert(variant(Doc).name='John');
    // !end;
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure Init(aOptions: TDocVariantOptions = [];
      aKind: TDocVariantKind = dvUndefined); overload;
    /// initialize a TDocVariantData to store some document-based content
    // - use the options corresponding to the supplied TDocVariantModel
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure Init(aModel: TDocVariantModel;
      aKind: TDocVariantKind = dvUndefined); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a TDocVariantData to store per-reference document-based content
    // - same as Doc.Init(JSON_FAST);
    // - can be used with a stack-allocated TDocVariantData variable:
    // !var Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitFast;
    // !  Doc.AddValue('name','John');
    // !  assert(Doc.Value['name']='John');
    // !  assert(variant(Doc).name='John');
    // !end;
    // - see also TDocVariant.NewFast() if you want to initialize several
    // TDocVariantData variable instances at once
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitFast(aKind: TDocVariantKind = dvUndefined); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a TDocVariantData to store per-reference document-based content
    // - this overloaded method allows to specify an estimation of how many
    // properties or items this aKind document would contain
    procedure InitFast(InitialCapacity: integer; aKind: TDocVariantKind); overload;
    /// initialize a TDocVariantData to store document-based object content
    // - object will be initialized with data supplied two by two, as Name,Value
    // pairs, e.g.
    // !var Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitObject(['name','John','year',1972]);
    // which is the same as:
    // ! var Doc: TDocVariantData;
    // !begin
    // !  Doc.Init;
    // !  Doc.AddValue('name','John');
    // !  Doc.AddValue('year',1972);
    // - this method is called e.g. by _Obj() and _ObjFast() global functions
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObject(const NameValuePairs: array of const;
      aOptions: TDocVariantOptions = []); overload;
    /// initialize a TDocVariantData to store document-based object content
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObject(const NameValuePairs: array of const;
      Model: TDocVariantModel); overload;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied as parameters, e.g.
    // !var Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitArray(['one',2,3.0]);
    // !  assert(Doc.Count=3);
    // !end;
    // which is the same as:
    // ! var Doc: TDocVariantData;
    // !     i: integer;
    // !begin
    // !  Doc.Init;
    // !  Doc.AddItem('one');
    // !  Doc.AddItem(2);
    // !  Doc.AddItem(3.0);
    // !  assert(Doc.Count=3);
    // !  for i := 0 to Doc.Count-1 do
    // !    writeln(Doc.Value[i]);
    // !end;
    // - this method is called e.g. by _Arr() and _ArrFast() global functions
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitArray(const aItems: array of const;
      aOptions: TDocVariantOptions = []); overload;
    /// initialize a variant instance to store some document-based array content
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitArray(const aItems: array of const;
      aModel: TDocVariantModel); overload;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied as variant dynamic array
    // - if Items is [], the variant will be set as null
    // - will be almost immediate, since TVariantDynArray is reference-counted,
    // unless ItemsCopiedByReference is set to FALSE
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitArrayFromVariants(const aItems: TVariantDynArray;
      aOptions: TDocVariantOptions = [];
      aItemsCopiedByReference: boolean = true);
    /// initialize a variant instance to store some RawUtf8 array content
    procedure InitArrayFrom(const aItems: TRawUtf8DynArray;
      aOptions: TDocVariantOptions); overload;
    /// initialize a variant instance to store some 32-bit integer array content
    procedure InitArrayFrom(const aItems: TIntegerDynArray;
      aOptions: TDocVariantOptions); overload;
    /// initialize a variant instance to store some 64-bit integer array content
    procedure InitArrayFrom(const aItems: TInt64DynArray;
      aOptions: TDocVariantOptions); overload;
    /// initialize a variant instance to store some double array content
    procedure InitArrayFrom(const aItems: TDoubleDynArray;
      aOptions: TDocVariantOptions); overload;
    /// initialize a variant instance to store some dynamic array content
    procedure InitArrayFrom(var aItems; ArrayInfo: PRttiInfo;
      aOptions: TDocVariantOptions; ItemsCount: PInteger = nil); overload;
    /// initialize a variant instance to store some TDynArray content
    procedure InitArrayFrom(const aItems: TDynArray;
      aOptions: TDocVariantOptions); overload;
    /// initialize a variant instance to store a T*ObjArray content
    // - will call internally ObjectToVariant() to make the conversion
    procedure InitArrayFromObjArray(const ObjArray; aOptions: TDocVariantOptions;
      aWriterOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]);
    /// initialize a variant instance to store some document-based object content
    // - object will be initialized with names and values supplied as dynamic arrays
    // - if aNames and aValues are [] or do have matching sizes, the variant
    // will be set as null
    // - will be almost immediate, since Names and Values are reference-counted
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObjectFromVariants(const aNames: TRawUtf8DynArray;
       const aValues: TVariantDynArray; aOptions: TDocVariantOptions = []);
    /// initialize a variant instance to store a document-based object with a
    // single property
    // - the supplied path could be 'Main.Second.Third', to create nested
    // objects, e.g. {"Main":{"Second":{"Third":value}}}
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObjectFromPath(const aPath: RawUtf8; const aValue: variant;
      aOptions: TDocVariantOptions = []);
    /// initialize a variant instance to store some document-based object content
    // from a supplied JSON array or JSON object content
    // - warning: the incoming JSON buffer will be modified in-place: so you should
    // make a private copy before running this method, e.g. using TSynTempBuffer
    // - this method is called e.g. by _JsonFmt() _JsonFastFmt() global functions
    // with a temporary JSON buffer content created from a set of parameters
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    function InitJsonInPlace(Json: PUtf8Char;
      aOptions: TDocVariantOptions = [];
      aEndOfObject: PUtf8Char = nil): PUtf8Char;
    /// initialize a variant instance to store some document-based object content
    // from a supplied JSON array or JSON object content
    // - a private copy of the incoming JSON buffer will be used, then
    // it will call the other overloaded InitJsonInPlace() method
    // - this method is called e.g. by _Json() and _JsonFast() global functions
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    // - handle only currency for floating point values: set JSON_FAST_FLOAT
    // or dvoAllowDoubleValue option to support double, with potential precision loss
    function InitJson(const Json: RawUtf8;
      aOptions: TDocVariantOptions = []): boolean; overload;
    /// initialize a variant instance to store some document-based object content
    // from a supplied JSON array or JSON object content
    // - use the options corresponding to the supplied TDocVariantModel
    // - a private copy of the incoming JSON buffer will be made
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    function InitJson(const Json: RawUtf8; aModel: TDocVariantModel): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// ensure a document-based variant instance will have one unique options set
    // - this will create a copy of the supplied TDocVariant instance, forcing
    // all nested events to have the same set of Options
    // - you can use this function to ensure that all internal properties of this
    // variant will be copied e.g. per-reference (if you set JSON_[mDefault])
    // or per-value (if you set JSON_[mDefault]) whatever options the nested
    // objects or arrays were created with
    // - will raise an EDocVariant if the supplied variant is not a TDocVariant
    // - you may rather use _Unique() or _UniqueFast() wrappers if you want to
    // ensure that a TDocVariant instance is unique
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitCopy(const SourceDocVariant: variant;
      aOptions: TDocVariantOptions);
    /// initialize a variant instance to store some document-based object content
    // from a supplied CSV UTF-8 encoded text
    // - the supplied content may have been generated by ToTextPairs() method
    // - if ItemSep=#10, then any kind of line feed (CRLF or LF) will be handled
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitCsv(aCsv: PUtf8Char; aOptions: TDocVariantOptions;
      NameValueSep: AnsiChar = '='; ItemSep: AnsiChar = #10;
      DoTrim: boolean = true); overload;
    /// initialize a variant instance to store some document-based object content
    // from a supplied CSV UTF-8 encoded text
    // - the supplied content may have been generated by ToTextPairs() method
    // - if ItemSep = #10, then any kind of line feed (CRLF or LF) will be handled
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitCsv(const aCsv: RawUtf8; aOptions: TDocVariantOptions;
      NameValueSep: AnsiChar = '='; ItemSep: AnsiChar = #10;
      DoTrim: boolean = true); overload;
       {$ifdef HASINLINE}inline;{$endif}

    /// to be called before any Init*() method call, when a previous Init*()
    // has already be performed on the same instance, to avoid memory leaks
    // - for instance:
    // !var Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitArray(['one',2,3.0]); // no need of any Doc.Clear here
    // !  assert(Doc.Count=3);
    // !  Doc.Clear; // to release memory before following InitObject()
    // !  Doc.InitObject(['name','John','year',1972]);
    // !end;
    // - will check the VType, and call ClearFast private method
    procedure Clear;
    /// delete all internal stored values
    // - like Clear + Init() with the same options
    // - will reset Kind to dvUndefined
    procedure Reset;
    /// fill all Values[] with #0, then delete all values
    // - could be used to specifically remove sensitive information from memory
    procedure FillZero;
    /// check if the Document is an object - i.e. Kind = dvObject
    function IsObject: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if the Document is an array - i.e. Kind = dvArray
    function IsArray: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// guess the TDocVariantModel corresponding to the current document Options
    // - returns true if model has been found and set
    // - returns false if no JSON_[] matches the current options
    function GetModel(out model: TDocVariantModel): boolean;
    /// low-level method to force a number of items
    // - could be used to fast add items to the internal Values[]/Names[] arrays
    // - just set protected VCount field, do not resize the arrays: caller
    // should ensure that Capacity is big enough
    procedure SetCount(aCount: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// efficient comparison of two TDocVariantData content
    // - will return the same result than JSON comparison, but more efficiently
    function Compare(const Another: TDocVariantData;
      CaseInsensitive: boolean = false): integer;
    /// efficient comparison of two TDocVariantData objects
    // - will always ensure that both this instance and Another are Objects
    // - will compare all values following the supplied Fields order
    // - if no Fields is specified, will fallback to regular Compare()
    function CompareObject(const ObjFields: array of RawUtf8;
      const Another: TDocVariantData; CaseInsensitive: boolean = false): integer;
    /// efficient equality comparison of two TDocVariantData content
    // - just a wrapper around Compare(Another)=0
    function Equals(const Another: TDocVariantData;
      CaseInsensitive: boolean = false): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level method called internally to reserve place for new values
    // - returns the index of the newly created item in Values[]/Names[] arrays
    // - you should not have to use it, unless you want to add some items
    // directly within the Values[]/Names[] arrays, using e.g.
    // InitFast(InitialCapacity) to initialize the document
    // - if aName='', append a dvArray item, otherwise append a dvObject field
    // - you can specify an optional aIndex value to Insert instead of Add
    // - warning: FPC optimizer is confused by Values[InternalAdd(name)] so
    // you should call InternalAdd() in an explicit previous step
    function InternalAdd(const aName: RawUtf8; aIndex: integer = -1): integer;
    {$ifdef HASITERATORS}
    /// an enumerator able to compile "for .. in dv do" statements
    // - returns pointers over all Names[] and Values[]
    // - warning: if the document is an array, returned Name is nil:
    // ! var e: TDocVariantFields;
    // ! ...
    // !    dv.InitArray([1, 3, 3, 4]);
    // !    for e in dv do
    // !      // here e^.Name = nil
    // !      writeln(e^.Value^);
    // ! // output  1  2  3  4
    function GetEnumerator: TDocVariantFieldsEnumerator;
    /// an enumerator able to compile "for .. in dv.Fields do" for objects
    // - returns pointers over all Names[] and Values[]
    // - don't iterate if the document is an array - so Name is never nil:
    // ! var e: TDocVariantFields;
    // ! ...
    // !   dv.InitJson('{a:1,b:2,c:3}');
    // !   for e in dv.Fields do
    // !     writeln(e.Name^, ':', e.Value^);
    // ! // output  a:1  b:2  c:3
    function Fields: TDocVariantFieldsEnumerator;
    /// an enumerator able to compile "for .. in dv.FieldNames do" for objects
    // - returns pointers over all Names[]
    // - don't iterate if the document is an array - so n is never nil:
    // ! var n: PRawUtf8;
    // ! ...
    // !   dv.InitJson('{a:1,b:2,c:3}');
    // !   for n in dv.FieldNames do
    // !     writeln(n^);
    // ! // output  a  b  c
    function FieldNames: TDocVariantFieldNamesEnumerator;
    /// an enumerator able to compile "for .. in dv.FieldValues do" for objects
    // - returns pointers over all Values[]
    // - don't iterate if the document is an array:
    // ! var v: PVariant;
    // ! ...
    // !   dv.InitJson('{a:1,b:2,c:3}');
    // !   for v in dv.FieldValues do
    // !     writeln(v^);
    // ! // output  1  2  3
    function FieldValues: TDocVariantItemsEnumerator;
    /// an enumerator able to compile "for .. in dv.Items do" for arrays
    // - returns a PVariant over all Values[] of a document array
    // - don't iterate if the document is an object
    // - for instance:
    // ! var v: PVariant;
    // ! ...
    // !    dv.InitArray([1, 3, 3, 4]);
    // !    for v in dv.Items do
    // !      writeln(v^);
    // ! // output  1  2  3  4
    function Items: TDocVariantItemsEnumerator;
    /// an enumerator able to compile "for .. dv.Objects do" for array of objects
    // - returns all Values[] of a document array which are a TDocVariantData
    // - don't iterate if the document is an object, or if an item is not a
    // TDocVariantData:
    // ! var d: PDocVariantData;
    // ! ...
    // !    dv.InitJson('[{a:1,b:1},1,"no object",{a:2,b:2}]');
    // !    for d in dv.Objects do
    // !      writeln(d^.ToJson);
    // ! // output {"a":1,"b":1} and {"a":2,"b":2} only
    // ! // (ignoring 1 and "no object" items)
    function Objects: TDocVariantObjectsEnumerator;
    {$endif HASITERATORS}

    /// save a document as UTF-8 encoded JSON
    // - will write either a JSON object or array, depending of the internal
    // layout of this instance (i.e. Kind property value)
    // - will write  'null'  if Kind is dvUndefined
    // - implemented as just a wrapper around VariantSaveJson()
    function ToJson(const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
      Format: TTextWriterJsonFormat = jsonCompact): RawUtf8;
    /// save a document as UTF-8 encoded JSON file
    procedure SaveToJsonFile(const FileName: TFileName);
    /// save an array of objects as UTF-8 encoded non expanded layout JSON
    // - returned content would be a JSON object in mORMot's TOrmTable non
    // expanded format, with reduced JSON size, i.e.
    // $ {"fieldCount":3,"values":["ID","FirstName","LastName",...']}
    // - will write '' if Kind is dvUndefined or dvObject
    // - will raise an exception if the array document is not an array of
    // objects with identical field names
    function ToNonExpandedJson: RawUtf8;
    /// save a document as an array of UTF-8 encoded JSON
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - will use VariantToUtf8() to populate the result array: as a consequence,
    // any nested custom variant types (e.g. TDocVariant) will be stored as JSON
    procedure ToRawUtf8DynArray(out Result: TRawUtf8DynArray); overload;
    /// save a document as an array of UTF-8 encoded JSON
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - will use VariantToUtf8() to populate the result array: as a consequence,
    // any nested custom variant types (e.g. TDocVariant) will be stored as JSON
    function ToRawUtf8DynArray: TRawUtf8DynArray; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// save a document as an CSV of UTF-8 encoded JSON
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - will use VariantToUtf8() to populate the result array: as a consequence,
    // any nested custom variant types (e.g. TDocVariant) will be stored as JSON
    function ToCsv(const Separator: RawUtf8 = ','): RawUtf8;
    /// save a document as UTF-8 encoded Name=Value pairs
    // - will follow by default the .INI format, but you can specify your
    // own expected layout
    procedure ToTextPairsVar(out result: RawUtf8;
      const NameValueSep: RawUtf8 = '='; const ItemSep: RawUtf8 = #13#10;
      Escape: TTextWriterKind = twJsonEscape);
    /// save a document as UTF-8 encoded Name=Value pairs
    // - will follow by default the .INI format, but you can specify your
    // own expected layout
    function ToTextPairs(const NameValueSep: RawUtf8 = '=';
      const ItemSep: RawUtf8 = #13#10;
      Escape: TTextWriterKind = twJsonEscape): RawUtf8;
       {$ifdef HASINLINE}inline;{$endif}
    /// save an array document as an array of TVarRec, i.e. an array of const
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - values will be passed by referenced as vtVariant to @VValue[ndx]
    // - would allow to write code as such:
    // !  Doc.InitArray(['one',2,3]);
    // !  Doc.ToArrayOfConst(vr);
    // !  s := FormatUtf8('[%,%,%]',vr,[],true);
    // !  // here s='[one,2,3]') since % would be replaced by Args[] parameters
    // !  s := FormatUtf8('[?,?,?]',[],vr,true);
    // !  // here s='["one",2,3]') since ? would be escaped by Params[] parameters
    procedure ToArrayOfConst(out Result: TTVarRecDynArray); overload;
    /// save an array document as an array of TVarRec, i.e. an array of const
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - values will be passed by referenced as vtVariant to @VValue[ndx]
    // - would allow to write code as such:
    // !  Doc.InitArray(['one',2,3]);
    // !  s := FormatUtf8('[%,%,%]',Doc.ToArrayOfConst,[],true);
    // !  // here s='[one,2,3]') since % would be replaced by Args[] parameters
    // !  s := FormatUtf8('[?,?,?]',[],Doc.ToArrayOfConst,true);
    // !  // here s='["one",2,3]') since ? would be escaped by Params[] parameters
    function ToArrayOfConst: TTVarRecDynArray; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// save an object document as an URI-encoded list of parameters
    // - object field names should be plain ASCII-7 RFC compatible identifiers
    // (0..9a..zA..Z_.~), otherwise their values are skipped
    function ToUrlEncode(const UriRoot: RawUtf8): RawUtf8;

    /// returns true if this is not a true TDocVariant, or Count equals 0
    function IsVoid: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item index in this document from its name
    // - search will follow dvoNameCaseSensitive option of this document
    // - lookup the value by name for an object document, or accept an integer
    // text as index for an array document
    // - returns -1 if not found
    function GetValueIndex(const aName: RawUtf8): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item index in this document from its name
    // - lookup the value by name for an object document, or accept an integer
    // text as index for an array document
    // - returns -1 if not found
    function GetValueIndex(aName: PUtf8Char; aNameLen: PtrInt;
      aCaseSensitive: boolean): integer; overload;
    /// find an item in this document, and returns its value
    // - raise an EDocVariant if not found and dvoReturnNullForUnknownProperty
    // is not set in Options (in this case, it will return Null)
    function GetValueOrRaiseException(const aName: RawUtf8): variant;
    /// find an item in this document, and returns its value
    // - return the supplied default if aName is not found, or if the instance
    // is not a TDocVariant
    function GetValueOrDefault(const aName: RawUtf8;
      const aDefault: variant): variant;
    /// find an item in this document, and returns its value
    // - return null if aName is not found, or if the instance is not a TDocVariant
    function GetValueOrNull(const aName: RawUtf8): variant;
    /// find an item in this document, and returns its value
    // - return a cleared variant if aName is not found, or if the instance is
    // not a TDocVariant
    function GetValueOrEmpty(const aName: RawUtf8): variant;
    /// find an item in this document, and returns its value as enumerate
    // - return false if aName is not found, if the instance is not a TDocVariant,
    // or if the value is not a string corresponding to the supplied enumerate
    // - return true if the name has been found, and aValue stores the value
    // - will call Delete() on the found entry, if aDeleteFoundEntry is true
    function GetValueEnumerate(const aName: RawUtf8; aTypeInfo: PRttiInfo;
      out aValue; aDeleteFoundEntry: boolean = false): boolean;
    /// returns a TDocVariant object containing all properties matching the
    // first characters of the supplied property name
    // - returns null if the document is not a dvObject
    // - will use IdemPChar(), so search would be case-insensitive
    function GetValuesByStartName(const aStartName: RawUtf8;
      TrimLeftStartName: boolean = false): variant;
    /// returns a JSON object containing all properties matching the
    // first characters of the supplied property name
    // - returns null if the document is not a dvObject
    // - will use IdemPChar(), so search would be case-insensitive
    function GetJsonByStartName(const aStartName: RawUtf8): RawUtf8;
    /// find an item in this document, and returns its value as TVarData
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true and set aValue if the name has been found
    // - will use simple loop lookup to identify the name, unless aSortedCompare is
    // set, and would let use a faster O(log(n)) binary search after a SortByName()
    function GetVarData(const aName: RawUtf8; var aValue: TVarData;
      aSortedCompare: TUtf8Compare = nil): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in this document, and returns its value as TVarData pointer
    // - return nil if aName is not found, or if the instance is not a TDocVariant
    // - return a pointer to the value if the name has been found, and optionally
    // fill aFoundIndex^ with its index in Values[]
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetVarData(const aName: RawUtf8; aSortedCompare: TUtf8Compare = nil;
      aFoundIndex: PInteger = nil): PVarData; overload;
    /// find an item in this document, and returns its value as boolean
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using B[] property if you want simple read/write typed access
    function GetAsBoolean(const aName: RawUtf8; out aValue: boolean;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as integer
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using I[] property if you want simple read/write typed access
    function GetAsInteger(const aName: RawUtf8; out aValue: integer;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as integer
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using I[] property if you want simple read/write typed access
    function GetAsInt64(const aName: RawUtf8; out aValue: Int64;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as floating point
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using D[] property if you want simple read/write typed access
    function GetAsDouble(const aName: RawUtf8; out aValue: double;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as RawUtf8
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using U[] property if you want simple read/write typed access
    function GetAsRawUtf8(const aName: RawUtf8; out aValue: RawUtf8;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as a TDocVariantData
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found and points to a TDocVariant:
    // then aValue stores a pointer to the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsDocVariant(const aName: RawUtf8; out aValue: PDocVariantData;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find a non-void array item in this document, and returns its value
    // - return false if aName is not found, or if not a TDocVariant array
    // - return true if the name was found as non-void array and set to aArray
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsArray(const aName: RawUtf8; out aArray: PDocVariantData;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find a non-void object item in this document, and returns its value
    // - return false if aName is not found, or if not a TDocVariant object
    // - return true if the name was found as non-void object and set to aObject
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsObject(const aName: RawUtf8; out aObject: PDocVariantData;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as a TDocVariantData
    // - returns a void TDocVariant if aName is not a document
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using O[] or A[] properties if you want simple read-only
    // access, or O_[] or A_[] properties if you want the ability to add
    // a missing object or array in the document
    function GetAsDocVariantSafe(const aName: RawUtf8;
      aSortedCompare: TUtf8Compare = nil): PDocVariantData;
    /// find an item in this document, and returns pointer to its value
    // - return false if aName is not found
    // - return true if the name has been found: then aValue stores a pointer
    // to the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsPVariant(const aName: RawUtf8; out aValue: PVariant;
      aSortedCompare: TUtf8Compare = nil): boolean; overload;
       {$ifdef HASINLINE}inline;{$endif}
    /// find an item in this document, and returns pointer to its value
    // - lookup the value by aName/aNameLen for an object document, or accept
    // an integer text as index for an array document
    // - return nil if aName is not found, or if the instance is not a TDocVariant
    // - return a pointer to the stored variant, if the name has been found
    function GetAsPVariant(aName: PUtf8Char; aNameLen: PtrInt): PVariant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a value, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - it will return Unassigned if the path does match the supplied aPath
    function GetValueByPath(const aPath: RawUtf8): variant; overload;
    /// retrieve a value, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - it will return FALSE if the path does not match the supplied aPath
    // - returns TRUE and set the found value in aValue
    function GetValueByPath(const aPath: RawUtf8;
      out aValue: variant): boolean; overload;
    /// retrieve a value, given its path
    // - path is defined as a list of names, e.g. ['doc','glossary','title']
    // - it will return Unassigned if the path does not match the data
    // - this method will only handle nested TDocVariant values: use the
    // slightly slower GetValueByPath() overloaded method, if any nested object
    // may be of another type (e.g. a TBsonVariant)
    function GetValueByPath(
      const aDocVariantPath: array of RawUtf8): variant; overload;
    /// retrieve a reference to a value, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - if the supplied aPath does not match any object, it will return nil
    // - if aPath is found, returns a pointer to the corresponding value
    function GetPVariantByPath(const aPath: RawUtf8): PVariant;
    /// retrieve a reference to a TDocVariant, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - if the supplied aPath does not match any object, it will return false
    // - if aPath stores a valid TDocVariant, returns true and a pointer to it
    function GetDocVariantByPath(const aPath: RawUtf8;
      out aValue: PDocVariantData): boolean;
    /// retrieve a dvObject in the dvArray, from a property value
    // - {aPropName:aPropValue} will be searched within the stored array,
    // and the corresponding item will be copied into Dest, on match
    // - returns FALSE if no match is found, TRUE if found and copied
    // - create a copy of the variant by default, unless DestByRef is TRUE
    // - will call VariantEquals() for value comparison
    function GetItemByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean; var Dest: variant;
      DestByRef: boolean = false): boolean;
    /// retrieve a reference to a dvObject in the dvArray, from a property value
    // - {aPropName:aPropValue} will be searched within the stored array,
    // and the corresponding item will be copied into Dest, on match
    // - returns FALSE if no match is found, TRUE if found and copied by reference
    function GetDocVariantByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean; out Dest: PDocVariantData): boolean;
    /// find an item in this document, and returns its value
    // - raise an EDocVariant if not found and dvoReturnNullForUnknownProperty
    // is not set in Options (in this case, it will return Null)
    // - create a copy of the variant by default, unless DestByRef is TRUE
    function RetrieveValueOrRaiseException(aName: PUtf8Char; aNameLen: integer;
      aCaseSensitive: boolean; var Dest: variant; DestByRef: boolean): boolean; overload;
    /// retrieve an item in this document from its index, and returns its value
    // - raise an EDocVariant if the supplied Index is not in the 0..Count-1
    // range and dvoReturnNullForUnknownProperty is set in Options
    // - create a copy of the variant by default, unless DestByRef is TRUE
    procedure RetrieveValueOrRaiseException(Index: integer;
     var Dest: variant; DestByRef: boolean); overload;
    /// retrieve an item in this document from its index, and returns its Name
    // - raise an EDocVariant if the supplied Index is not in the 0..Count-1
    // range and dvoReturnNullForUnknownProperty is set in Options
    procedure RetrieveNameOrRaiseException(Index: integer; var Dest: RawUtf8);
    /// set an item in this document from its index
    // - raise an EDocVariant if the supplied Index is not in 0..Count-1 range
    procedure SetValueOrRaiseException(Index: integer; const NewValue: variant);

    /// add a value in this document
    // - if aName is set, if dvoCheckForDuplicatedNames option is set, any
    // existing duplicated aName will raise an EDocVariant; if instance's
    // kind is dvArray and aName is defined, it will raise an EDocVariant
    // - aName may be '' e.g. if you want to store an array: in this case,
    // dvoCheckForDuplicatedNames option should not be set; if instance's Kind
    // is dvObject, it will raise an EDocVariant exception
    // - if aValueOwned is true, then the supplied aValue will be assigned to
    // the internal values - by default, it will use SetVariantByValue()
    // - you can therefore write e.g.:
    // ! TDocVariant.New(aVariant);
    // ! Assert(TDocVariantData(aVariant).Kind=dvUndefined);
    // ! TDocVariantData(aVariant).AddValue('name','John');
    // ! Assert(TDocVariantData(aVariant).Kind=dvObject);
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added value
    function AddValue(const aName: RawUtf8; const aValue: variant;
      aValueOwned: boolean = false; aIndex: integer = -1): integer; overload;
    /// add a value in this document
    // - overloaded function accepting a UTF-8 encoded buffer for the name
    function AddValue(aName: PUtf8Char; aNameLen: integer; const aValue: variant;
      aValueOwned: boolean = false; aIndex: integer = -1): integer; overload;
    /// add a value in this document, or update an existing entry
    // - if instance's Kind is dvArray, it will raise an EDocVariant exception
    // - any existing Name would be updated with the new Value, unless
    // OnlyAddMissing is set to TRUE, in which case existing values would remain
    // - returns the index of the corresponding value, which may be just added
    function AddOrUpdateValue(const aName: RawUtf8; const aValue: variant;
      wasAdded: PBoolean = nil; OnlyAddMissing: boolean = false): integer;
    /// add a value in this document, from its text representation
    // - this function expects a UTF-8 text for the value, which would be
    // converted to a variant number, if possible (as varInt/varInt64/varCurrency
    // and/or as varDouble is AllowVarDouble is set)
    // - if Update=TRUE, will set the property, even if it is existing
    function AddValueFromText(const aName, aValue: RawUtf8;
      DoUpdate: boolean = false; AllowVarDouble: boolean = false): integer;
    /// add some properties to a TDocVariantData dvObject
    // - data is supplied two by two, as Name,Value pairs
    // - caller should ensure that Kind=dvObject, otherwise it won't do anything
    // - any existing Name would be duplicated - use Update() if you want to
    // replace any existing value
    procedure AddNameValuesToObject(const NameValuePairs: array of const);
    /// merge some properties to a TDocVariantData dvObject
    // - data is supplied two by two, as Name,Value pairs
    // - caller should ensure that Kind=dvObject, otherwise it won't do anything
    // - any existing Name would be updated with the new Value
    procedure Update(const NameValuePairs: array of const);
    {$ifndef PUREMORMOT2}
    /// deprecated method which redirects to Update()
    procedure AddOrUpdateNameValuesToObject(const NameValuePairs: array of const);
    {$endif PUREMORMOT2}
    /// merge some TDocVariantData dvObject properties to a TDocVariantData dvObject
    // - data is supplied two by two, as Name,Value pairs
    // - caller should ensure that both variants have Kind=dvObject, otherwise
    // it won't do anything
    // - any existing Name would be updated with the new Value, unless
    // OnlyAddMissing is set to TRUE, in which case existing values would remain
    procedure AddOrUpdateObject(const NewValues: variant;
      OnlyAddMissing: boolean = false; RecursiveUpdate: boolean = false);
    /// add a value to this document, handled as array
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    // - you can therefore write e.g.:
    // ! TDocVariant.New(aVariant);
    // ! Assert(TDocVariantData(aVariant).Kind=dvUndefined);
    // ! TDocVariantData(aVariant).AddItem('one');
    // ! Assert(TDocVariantData(aVariant).Kind=dvArray);
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added item
    function AddItem(const aValue: variant; aIndex: integer = -1): integer;
    /// add a value to this document, handled as array, from its text representation
    // - this function expects a UTF-8 text for the value, which would be
    // converted to a variant number, if possible (as varInt/varInt64/varCurrency
    // unless AllowVarDouble is set)
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added item
    function AddItemFromText(const aValue: RawUtf8;
      AllowVarDouble: boolean = false; aIndex: integer = -1): integer;
    /// add a RawUtf8 value to this document, handled as array
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added item
    function AddItemText(const aValue: RawUtf8; aIndex: integer = -1): integer;
    /// add one or several values to this document, handled as array
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    procedure AddItems(const aValue: array of const);
    /// add one or several values from another document
    // - supplied document should be of the same kind than the current one,
    // otherwise nothing is added
    procedure AddFrom(const aDocVariant: Variant);
    /// add or update or on several valeus from another object
    // - current document should be an object
    procedure AddOrUpdateFrom(const aDocVariant: Variant;
      aOnlyAddMissing: boolean = false);
    /// add one or several properties, specified by path, from another object
    // - path are defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - matching values would be added as root values, with the path as name
    // - instance and supplied aSource should be a dvObject
    procedure AddByPath(const aSource: TDocVariantData;
      const aPaths: array of RawUtf8);
    /// delete a value/item in this document, from its index
    // - return TRUE on success, FALSE if the supplied index is not correct
    function Delete(Index: PtrInt): boolean; overload;
    /// delete a value/item in this document, from its name
    // - return TRUE on success, FALSE if the supplied name does not exist
    function Delete(const aName: RawUtf8): boolean; overload;
    /// delete/filter some values/items in this document, from their name
    // - return the number of deleted items
    function Delete(const aNames: array of RawUtf8): integer; overload;
    /// delete a value in this document, by property name match
    // - {aPropName:aPropValue} will be searched within the stored array or
    // object, and the corresponding item will be deleted, on match
    // - returns FALSE if no match is found, TRUE if found and deleted
    // - will call VariantEquals() for value comparison
    function DeleteByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean): boolean;
    /// delete one or several value/item in this document, from its value
    // - returns the number of deleted items
    // - returns 0 if the document is not a dvObject, or if no match was found
    // - if the value exists several times, all occurences would be removed
    // - is optimized for DeleteByValue(null) call
    function DeleteByValue(const aValue: Variant;
      CaseInsensitive: boolean = false): integer;
    /// delete all values matching the first characters of a property name
    // - returns the number of deleted items
    // - returns 0 if the document is not a dvObject, or if no match was found
    // - will use IdemPChar(), so search would be case-insensitive
    function DeleteByStartName(aStartName: PUtf8Char;
      aStartNameLen: integer): integer;
    /// search a property match in this document, handled as array or object
    // - {aPropName:aPropValue} will be searched within the stored array or
    // object, and the corresponding item index will be returned, on match
    // - returns -1 if no match is found
    // - will call VariantEquals() for value comparison
    function SearchItemByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean): integer; overload;
    /// search a property match in this document, handled as array or object
    // - {aPropName:aPropValue} will be searched within the stored array or
    // object, and the corresponding item index will be returned, on match
    // - returns -1 if no match is found
    // - will call VariantEquals() for value comparison
    function SearchItemByProp(const aPropNameFmt: RawUtf8;
      const aPropNameArgs: array of const; const aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean): integer; overload;
    /// search a value in this document, handled as array
    // - aValue will be searched within the stored array
    // and the corresponding item index will be returned, on match
    // - returns -1 if no match is found
    // - you could make several searches, using the StartIndex optional parameter
    function SearchItemByValue(const aValue: Variant;
      CaseInsensitive: boolean = false; StartIndex: PtrInt = 0): PtrInt;
    /// sort the document object values by name
    // - do nothing if the document is not a dvObject
    // - will follow case-insensitive order (@StrIComp) by default, but you
    // can specify @StrComp as comparer function for case-sensitive ordering
    // - once sorted, you can use GetVarData(..,Compare) or GetAs*(..,Compare)
    // methods for much faster O(log(n)) binary search
    procedure SortByName(SortCompare: TUtf8Compare = nil;
      SortCompareReversed: boolean = false);
    /// sort the document object values by value using a comparison function
    // - work for both dvObject and dvArray documents
    // - will sort by UTF-8 text (VariantCompare) if no custom aCompare is supplied
    procedure SortByValue(SortCompare: TVariantCompare = nil;
      SortCompareReversed: boolean = false);
    /// sort the document object values by value using a comparison method
    // - work for both dvObject and dvArray documents
    // - you should supply a TVariantComparer callback method
    procedure SortByRow(const SortComparer: TVariantComparer;
      SortComparerReversed: boolean = false);
    /// sort the document array values by a field of some stored objet values
    // - do nothing if the document is not a dvArray, or if the items are no dvObject
    // - aValueCompare will be called with the aItemPropName values, not row
    // - will sort by UTF-8 text (VariantCompare) if no custom aValueCompare is supplied
    // - this method is faster than SortByValue/SortByRow
    procedure SortArrayByField(const aItemPropName: RawUtf8;
      aValueCompare: TVariantCompare = nil;
      aValueCompareReverse: boolean = false;
      aNameSortedCompare: TUtf8Compare = nil);
    /// sort the document array values by field(s) of some stored objet values
    // - allow up to 4 fields (aItemPropNames[0]..aItemPropNames[3])
    // - do nothing if the document is not a dvArray, or if the items are no dvObject
    // - will sort by UTF-8 text (VariantCompare) if no aValueCompareField is supplied
    procedure SortArrayByFields(const aItemPropNames: array of RawUtf8;
      aValueCompare: TVariantCompare = nil;
      const aValueCompareField: TVariantCompareField = nil;
      aValueCompareReverse: boolean = false; aNameSortedCompare: TUtf8Compare = nil);
    /// inverse the order of Names and Values of this document
    // - could be applied after a content sort if needed
    procedure Reverse;
    /// create a TDocVariant object, from a selection of properties of this
    // document, by property name
    // - if the document is a dvObject, to reduction will be applied to all
    // its properties
    // - if the document is a dvArray, the reduction will be applied to each
    // stored item, if it is a document
    procedure Reduce(const aPropNames: array of RawUtf8; aCaseSensitive: boolean;
      out result: TDocVariantData; aDoNotAddVoidProp: boolean = false); overload;
    /// create a TDocVariant object, from a selection of properties of this
    // document, by property name
    // - always returns a TDocVariantData, even if no property name did match
    // (in this case, it is dvUndefined)
    function Reduce(const aPropNames: array of RawUtf8; aCaseSensitive: boolean;
      aDoNotAddVoidProp: boolean = false): variant; overload;
    /// create a TDocVariant array, from the values of a single properties of
    // this document, specified by name
    // - you can optionally apply an additional filter to each reduced item
    procedure ReduceAsArray(const aPropName: RawUtf8;
      out result: TDocVariantData;
      const OnReduce: TOnReducePerItem = nil); overload;
    /// create a TDocVariant array, from the values of a single properties of
    // this document, specified by name
    // - always returns a TDocVariantData, even if no property name did match
    // (in this case, it is dvUndefined)
    // - you can optionally apply an additional filter to each reduced item
    function ReduceAsArray(const aPropName: RawUtf8;
      const OnReduce: TOnReducePerItem = nil): variant; overload;
    /// create a TDocVariant array, from the values of a single properties of
    // this document, specified by name
    // - this overloaded method accepts an additional filter to each reduced item
    procedure ReduceAsArray(const aPropName: RawUtf8;
      out result: TDocVariantData;
      const OnReduce: TOnReducePerValue); overload;
    /// create a TDocVariant array, from the values of a single properties of
    // this document, specified by name
    // - always returns a TDocVariantData, even if no property name did match
    // (in this case, it is dvUndefined)
    // - this overloaded method accepts an additional filter to each reduced item
    function ReduceAsArray(const aPropName: RawUtf8;
      const OnReduce: TOnReducePerValue): variant; overload;
    /// rename some properties of a TDocVariant object
    // - returns the number of property names modified
    function Rename(const aFromPropName, aToPropName: TRawUtf8DynArray): integer;
    /// map {"obj.prop1"..,"obj.prop2":..} into {"obj":{"prop1":..,"prop2":...}}
    // - the supplied aObjectPropName should match the incoming dotted value
    // of all properties (e.g. 'obj' for "obj.prop1")
    // - if any of the incoming property is not of "obj.prop#" form, the
    // whole process would be ignored
    // - return FALSE if the TDocVariant did not change
    // - return TRUE if the TDocVariant has been flattened
    function FlattenAsNestedObject(const aObjectPropName: RawUtf8): boolean;

    /// how this document will behave
    // - those options are set when creating the instance
    // - dvoArray and dvoObject are not options, but define the document Kind,
    // so those items are ignored when assigned to this property
    property Options: TDocVariantOptions
      read VOptions write SetOptions;
    /// returns the document internal layout
    // - just after initialization, it will return dvUndefined
    // - most of the time, you will add named values with AddValue() or by
    // setting the variant properties: it will return dvObject
    // - but is you use AddItem(), values will have no associated names: the
    // document will be a dvArray
    // - value computed from the dvoArray and dvoObject presence in Options
    property Kind: TDocVariantKind
      read GetKind;
    /// return the custom variant type identifier, i.e. DocVariantType.VarType
    property VarType: word
      read VType;
    /// number of items stored in this document
    // - is 0 if Kind=dvUndefined
    // - is the number of name/value pairs for Kind=dvObject
    // - is the number of items for Kind=dvArray
    property Count: integer
      read VCount;
    /// the current capacity of this document
    // - allow direct access to VValue[] length
    property Capacity: integer
      read GetCapacity write SetCapacity;
    /// direct acces to the low-level internal array of values
    // - note that length(Values)=Capacity and not Count, so copy(Values, 0, Count)
    // or use FieldValues iterator if you want the exact count
    // - transtyping a variant and direct access to TDocVariantData is the
    // fastest way of accessing all properties of a given dvObject:
    // ! with _Safe(aVariantObject)^ do
    // !   for i := 0 to Count-1 do
    // !     writeln(Names[i],'=',Values[i]);
    // - or to access a dvArray items (e.g. a MongoDB collection):
    // ! with TDocVariantData(aVariantArray) do
    // !   for i := 0 to Count-1 do
    // !     writeln(Values[i]);
    property Values: TVariantDynArray
      read VValue;
    /// direct acces to the low-level internal array of names
    // - is void (nil) if Kind is not dvObject
    // - note that length(Names)=Capacity and not Count, so copy(Names, 0, Count)
    // or use FieldNames iterator if you want the exact count
    // - transtyping a variant and direct access to TDocVariantData is the
    // fastest way of accessing all properties of a given dvObject:
    // ! with _Safe(aVariantObject)^ do
    // !   for i := 0 to Count-1 do
    // !     writeln(Names[i],'=',Values[i]);
    property Names: TRawUtf8DynArray
      read VName;
    /// find an item in this document, and returns its value
    // - raise an EDocVariant if aNameOrIndex is neither an integer nor a string
    // - raise an EDocVariant if Kind is dvArray and aNameOrIndex is a string
    // or if Kind is dvObject and aNameOrIndex is an integer
    // - raise an EDocVariant if Kind is dvObject and if aNameOrIndex is a
    // string, which is not found within the object property names and
    // dvoReturnNullForUnknownProperty is set in Options
    // - raise an EDocVariant if Kind is dvArray and if aNameOrIndex is a
    // integer, which is not within 0..Count-1 and dvoReturnNullForUnknownProperty
    // is set in Options
    // - so you can use directly:
    // ! // for an array document:
    // ! aVariant := TDocVariant.NewArray(['one',2,3.0]);
    // ! for i := 0 to TDocVariantData(aVariant).Count-1 do
    // !   aValue := TDocVariantData(aVariant).Value[i];
    // ! // for an object document:
    // ! aVariant := TDocVariant.NewObject(['name','John','year',1972]);
    // ! assert(aVariant.Name=TDocVariantData(aVariant)['name']);
    // ! assert(aVariant.year=TDocVariantData(aVariant)['year']);
    // - due to the internal implementation of variant execution (somewhat
    // slow _DispInvoke() function), it is a bit faster to execute:
    // ! aValue := TDocVariantData(aVariant).Value['name'];
    // or
    // ! aValue := _Safe(aVariant).Value['name'];
    // instead of
    // ! aValue := aVariant.name;
    // but of course, if want to want to access the content by index (typically
    // for a dvArray), using Values[] - and Names[] - properties is much faster
    // than this variant-indexed pseudo-property:
    // ! with TDocVariantData(aVariant) do
    // !   for i := 0 to Count-1 do
    // !     Writeln(Values[i]);
    // is faster than:
    // ! with TDocVariantData(aVariant) do
    // !   for i := 0 to Count-1 do
    // !     Writeln(Value[i]);
    // which is faster than:
    // ! for i := 0 to aVariant.Count-1 do
    // !   Writeln(aVariant._(i));
    // - this property will return the value as varByRef (just like with
    // variant late binding of any TDocVariant instance), so you can write:
    // !var Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitJson('{arr:[1,2]}');
    // !  assert(Doc.Count=2);
    // !  Doc.Value['arr'].Add(3);  // works since Doc.Value['arr'] is varByRef
    // !  writeln(Doc.ToJson);      // will write '{"arr":[1,2,3]}'
    // !end;
    // - if you want to access a property as a copy, i.e. to assign it to a
    // variant variable which will stay alive after this TDocVariant instance
    // is release, you should not use Value[] but rather
    // GetValueOrRaiseException or GetValueOrNull/GetValueOrEmpty
    // - see U[] I[] B[] D[] O[] O_[] A[] A_[] _[] properties for direct access
    // of strong typed values
    property Value[const aNameOrIndex: Variant]: Variant
      read GetValueOrItem write SetValueOrItem; default;

    /// direct access to a dvObject UTF-8 stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsRawUtf8() if you want to check the availability of the field
    // - U['prop'] := 'value' would add a new property, or overwrite an existing
    property U[const aName: RawUtf8]: RawUtf8
      read GetRawUtf8ByName write SetRawUtf8ByName;
    /// direct string access to a dvObject UTF-8 stored property value from its name
    // - just a wrapper around U[] property, to avoid a compilation warning when
    // using plain string variables (internally, RawUtf8 will be used for storage)
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsRawUtf8() if you want to check the availability of the field
    // - S['prop'] := 'value' would add a new property, or overwrite an existing
    property S[const aName: RawUtf8]: string
      read GetStringByName write SetStringByName;
    /// direct access to a dvObject integer stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsInt/GetAsInt64 if you want to check the availability of the field
    // - I['prop'] := 123 would add a new property, or overwrite an existing
    property I[const aName: RawUtf8]: Int64
      read GetInt64ByName write SetInt64ByName;
    /// direct access to a dvObject boolean stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsBoolean if you want to check the availability of the field
    // - B['prop'] := true would add a new property, or overwrite an existing
    property B[const aName: RawUtf8]: boolean
      read GetBooleanByName write SetBooleanByName;
    /// direct access to a dvObject floating-point stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsDouble if you want to check the availability of the field
    // - D['prop'] := 1.23 would add a new property, or overwrite an existing
    property D[const aName: RawUtf8]: Double
      read GetDoubleByName write SetDoubleByName;
    /// direct access to a dvObject existing dvObject property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - O['prop'] would return a fake void TDocVariant if the property is not
    // existing or not a dvObject, just like GetAsDocVariantSafe()
    // - use O_['prop'] to force adding any missing property
    property O[const aName: RawUtf8]: PDocVariantData
      read GetObjectExistingByName;
    /// direct access or add a dvObject's dvObject property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - O_['prop'] would add a new property if there is none existing, or
    // overwrite an existing property which is not a dvObject
    property O_[const aName: RawUtf8]: PDocVariantData
      read GetObjectOrAddByName;
    /// direct access to a dvObject existing dvArray property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - A['prop'] would return a fake void TDocVariant if the property is not
    // existing or not a dvArray, just like GetAsDocVariantSafe()
    // - use A_['prop'] to force adding any missing property
    property A[const aName: RawUtf8]: PDocVariantData
      read GetArrayExistingByName;
    /// direct access or add a dvObject's dvArray property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - A_['prop'] would add a new property if there is none existing, or
    // overwrite an existing property which is not a dvArray
    property A_[const aName: RawUtf8]: PDocVariantData
      read GetArrayOrAddByName;
    /// direct access to a dvArray's TDocVariant property from its index
    // - simple values may directly use Values[] dynamic array, but to access
    // a TDocVariantData members, this property is safer
    // - follows dvoReturnNullForUnknownProperty option to raise an exception
    // - _[ndx] would return a fake void TDocVariant if aIndex is out of range,
    // if the property is not existing or not a TDocVariantData (just like
    // GetAsDocVariantSafe)
    property _[aIndex: integer]: PDocVariantData
      read GetAsDocVariantByIndex;
  end;
  {$A+} { packet object not allowed since Delphi 2009 :( }

var
  /// the internal custom variant type used to register TDocVariant
  DocVariantType: TDocVariant;

  /// copy of DocVariantType.VarType
  // - as used by inlined functions of TDocVariantData
  DocVariantVType: cardinal;

  // defined here for inlining - properly filled in initialization section below
  DV_FAST: array[TDocVariantKind] of TVarData;


/// retrieve the text representation of a TDocVairnatKind
function ToText(kind: TDocVariantKind): PShortString; overload;

/// direct access to a TDocVariantData from a given variant instance
// - return a pointer to the TDocVariantData corresponding to the variant
// instance, which may be of kind varByRef (e.g. when retrieved by late binding)
// - raise an EDocVariant exception if the instance is not a TDocVariant
// - the following direct trans-typing may fail, e.g. for varByRef value:
// ! TDocVariantData(aVarDoc.ArrayProp).Add('new item');
// - so you can write the following:
// ! DocVariantData(aVarDoc.ArrayProp).AddItem('new item');
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function DocVariantData(const DocVariant: variant): PDocVariantData;

const
  /// constant used e.g. by _Safe() and _DV() overloaded functions
  // - will be in code section of the exe, so will be read-only by design
  // - would have Kind=dvUndefined and Count=0, so _Safe() would return
  // a valid, but void document
  // - its VType is varNull, so would be viewed as a null variant
  // - dvoReturnNullForUnknownProperty is defined, so that U[]/I[]... methods
  // won't raise any exception about unexpected field name
  DocVariantDataFake: TDocVariantData = (
    VType: varNull;
    VOptions: [dvoReturnNullForUnknownProperty]{%H-});

/// direct access to a TDocVariantData from a given variant instance
// - return a pointer to the TDocVariantData corresponding to the variant
// instance, which may be of kind varByRef (e.g. when retrieved by late binding)
// - will return a read-only fake TDocVariantData with Kind=dvUndefined if the
// supplied variant is not a TDocVariant instance, so could be safely used
// in a with block (use "with" moderation, of course):
// ! with _Safe(aDocVariant)^ do
// !   for ndx := 0 to Count-1 do // here Count=0 for the "fake" result
// !     writeln(Names[ndx]);
// or excluding the "with" statement, as more readable code:
// ! var dv: PDocVariantData;
// !     ndx: PtrInt;
// ! begin
// !   dv := _Safe(aDocVariant);
// !   for ndx := 0 to dv.Count-1 do // here Count=0 for the "fake" result
// !     writeln(dv.Names[ndx]);
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _Safe(const DocVariant: variant): PDocVariantData; overload;
  {$ifdef FPC}inline;{$endif} // Delphi has problems inlining this :(

/// direct access to a TDocVariantData from a given variant instance
// - return a pointer to the TDocVariantData corresponding to the variant
// instance, which may be of kind varByRef (e.g. when retrieved by late binding)
// - will check the supplied document kind, i.e. either dvObject or dvArray and
// raise a EDocVariant exception if it does not match
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _Safe(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): PDocVariantData; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct access to a TDocVariantData from a given variant instance
// - return true and set DocVariant with a pointer to the TDocVariantData
// corresponding to the variant instance, which may be of kind varByRef
// (e.g. when retrieved by late binding)
// - return false if the supplied Value is not a TDocVariant, but e.g. a string,
// a number or another type of custom variant
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _Safe(const DocVariant: variant; out DV: PDocVariantData): boolean; overload;
  {$ifdef FPC}inline;{$endif} // Delphi has problems inlining this :(

/// direct access to a TDocVariantData array from a given variant instance
// - return true and set DV with a pointer to the TDocVariantData
// corresponding to the variant instance, if it is a dvArray
// - return false if the supplied Value is not an array TDocVariant
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _SafeArray(const Value: variant; out DV: PDocVariantData): boolean; overload;

/// direct access to a TDocVariantData object from a given variant instance
// - return true and set DV with a pointer to the TDocVariantData
// corresponding to the variant instance, if it is a dvObject
// - return false if the supplied Value is not an object TDocVariant
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _SafeObject(const Value: variant; out DV: PDocVariantData): boolean; overload;

/// direct copy of a TDocVariantData from a given variant instance
// - slower, but maybe used instead of _Safe() e.g. on Delphi 11
function _DV(const DocVariant: variant): TDocVariantData; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct copy of a TDocVariantData from a given variant instance
// - slower, but maybe used instead of _Safe() e.g. on Delphi 11
function _DV(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): TDocVariantData; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct copy of a TDocVariantData from a given variant instance
// - slower, but maybe used instead of _Safe() e.g. on Delphi 11
function _DV(const DocVariant: variant;
  var DV: TDocVariantData): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// initialize a variant instance to store some document-based object content
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, e.g.
// ! aVariant := _Obj(['name','John','year',1972]);
// or even with nested objects:
// ! aVariant := _Obj(['name','John','doc',_Obj(['one',1,'two',2.0])]);
// - this global function is an alias to TDocVariant.NewObject()
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, set Options=[dvoValueCopiedByReference]
// or using _ObjFast() will increase the process speed a lot
function _Obj(const NameValuePairs: array of const;
  Options: TDocVariantOptions = []): variant;

/// add a property value to a document-based object content
// - if Obj is a TDocVariant object, will add the Name/Value pair
// - if Obj is not a TDocVariant, will create a new fast document,
// initialized with supplied the Name/Value pairs
// - this function will also ensure that ensure Obj is not stored by reference,
// but as a true TDocVariantData
procedure _ObjAddProp(const Name: RawUtf8; const Value: variant;
  var Obj: variant);

/// add some property values to a document-based object content
// - if Obj is a TDocVariant object, will add the Name/Value pairs
// - if Obj is not a TDocVariant, will create a new fast document,
// initialized with supplied the Name/Value pairs
// - this function will also ensure that ensure Obj is not stored by reference,
// but as a true TDocVariantData
procedure _ObjAddProps(const NameValuePairs: array of const;
  var Obj: variant); overload;

/// add the property values of a document to a document-based object content
// - if Document is not a TDocVariant object, will do nothing
// - if Obj is a TDocVariant object, will add Document fields to its content
// - if Obj is not a TDocVariant object, Document will be copied to Obj
procedure _ObjAddProps(const Document: variant;
  var Obj: variant); overload;

/// initialize a variant instance to store some document-based array content
// - array will be initialized with data supplied as parameters, e.g.
// ! aVariant := _Arr(['one',2,3.0]);
// - this global function is an alias to TDocVariant.NewArray()
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, set Options = [dvoValueCopiedByReference]
// or using _ArrFast() will increase the process speed a lot
function _Arr(const Items: array of const;
  Options: TDocVariantOptions = []): variant;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content
// - this global function is an alias to TDocVariant.NewJson(), and
// will return an Unassigned variant if JSON content was not correctly converted
// - object or array will be initialized from the supplied JSON content, e.g.
// ! aVariant := _Json('{"id":10,"doc":{"name":"John","birthyear":1972}}');
// ! // now you can access to the properties via late binding
// ! assert(aVariant.id=10);
// ! assert(aVariant.doc.name='John');
// ! assert(aVariant.doc.birthYear=1972);
// ! // and also some pseudo-properties:
// ! assert(aVariant._count=2);
// ! assert(aVariant.doc._kind=ord(dvObject));
// ! // or with a JSON array:
// ! aVariant := _Json('["one",2,3]');
// ! assert(aVariant._kind=ord(dvArray));
// ! for i := 0 to aVariant._count-1 do
// !   writeln(aVariant._(i));
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names:
// ! aVariant := _Json('{id:10,doc:{name:"John",birthyear:1972}}');
// - if the mormot.db.nosql.bson unit is used in the application, the MongoDB
// Shell syntax will also be recognized to create TBsonVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, add dvoValueCopiedByReference in Options
// will increase the process speed a lot, or use _JsonFast()
// - handle only currency for floating point values: call _JsonFastFloat or set
// dvoAllowDoubleValue option to support double, with potential precision loss
function _Json(const Json: RawUtf8;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with parameters formating
// - wrapper around the _Json(FormatUtf8(...,JsonFormat=true)) function,
// i.e. every Args[] will be inserted for each % and Params[] for each ?,
// with proper JSON escaping of string values, and writing nested _Obj() /
// _Arr() instances as expected JSON objects / arrays
// - typical use (in the context of mormot.db.nosql.bson unit) could be:
// ! aVariant := _JsonFmt('{%:{$in:[?,?]}}',['type'],['food','snack']);
// ! aVariant := _JsonFmt('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
// ! // which are the same as:
// ! aVariant := _JsonFmt('{type:{$in:["food","snack"]}}');
// ! // in this context:
// ! u := VariantSaveJson(aVariant);
// ! assert(u='{"type":{"$in":["food","snack"]}}');
// ! u := VariantSaveMongoJson(aVariant,modMongoShell);
// ! assert(u='{type:{$in:["food","snack"]}}');
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, add dvoValueCopiedByReference in Options
// will increase the process speed a lot, or use _JsonFast()
function _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant; overload;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with parameters formating
// - this overload function will set directly a local variant variable,
// and would be used by inlined _JsonFmt/_JsonFastFmt functions
procedure _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions; out Result: variant); overload;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content
// - this global function is an alias to TDocVariant.NewJson(), and
// will return TRUE if JSON content was correctly converted into a variant
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names or ObjectID()
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, add dvoValueCopiedByReference in Options
// will increase the process speed a lot, or use _JsonFast()
function _Json(const Json: RawUtf8; var Value: variant;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): boolean; overload;

/// initialize a variant instance to store some document-based object content
// - this global function is an handy alias to:
// ! Obj(NameValuePairs, JSON_FAST);
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
function _ObjFast(const NameValuePairs: array of const): variant; overload;

/// initialize a variant instance to store any object as a TDocVariant
// - is a wrapper around ObjectToVariant(aObject, result, aOptions)
function _ObjFast(aObject: TObject;
   aOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// initialize a variant instance to store some document-based array content
// - this global function is an handy alias to:
// ! _Array(Items, JSON_FAST);
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
function _ArrFast(const Items: array of const): variant; overload;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content
// - this global function is an handy alias to:
// ! _Json(JSON, JSON_FAST);
// so it will return an Unassigned variant if JSON content was not correct
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names or ObjectID()
// - will handle only currency for floating point values to avoid precision
// loss: use _JsonFastFloat() instead if you want to support double values
function _JsonFast(const Json: RawUtf8): variant;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with double conversion
// - _JsonFast() will support only currency floats: use this method instead
// if your JSON input is likely to require double values - with potential
// precision loss
function _JsonFastFloat(const Json: RawUtf8): variant;

/// initialize a variant instance to store some extended document-based content
// - this global function is an handy alias to:
// ! _Json(JSON,JSON_FAST_EXTENDED);
function _JsonFastExt(const Json: RawUtf8): variant;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with parameters formating
// - this global function is an handy alias e.g. to:
// ! aVariant := _JsonFmt('{%:{$in:[?,?]}}',['type'],['food','snack'], JSON_FAST);
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names or ObjectID():
function _JsonFastFmt(const Format: RawUtf8;
   const Args, Params: array of const): variant;

/// ensure a document-based variant instance will have only per-value nested
// objects or array documents
// - is just a wrapper around:
// ! TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_[mDefault])
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-value whatever options the nested objects or
// arrays were created with
// - for huge document with a big depth of nested objects or arrays, a full
// per-value copy may be time and resource consuming, but will be also safe
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
procedure _Unique(var DocVariant: variant);

/// ensure a document-based variant instance will have only per-value nested
// objects or array documents
// - is just a wrapper around:
// ! TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_FAST)
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-reference whatever options the nested objects or
// arrays were created with
// - for huge document with a big depth of nested objects or arrays, it will
// first create a whole copy of the document nodes, but further assignments
// of the resulting value will be per-reference, so will be almost instant
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
procedure _UniqueFast(var DocVariant: variant);

/// return a full nested copy of a document-based variant instance
// - is just a wrapper around:
// ! TDocVariant.NewUnique(DocVariant,JSON_[mDefault])
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-value whatever options the nested objects or
// arrays were created with: to be used on a value returned as varByRef
// (e.g. by _() pseudo-method)
// - for huge document with a big depth of nested objects or arrays, a full
// per-value copy may be time and resource consuming, but will be also safe -
// consider using _ByRef() instead if a fast copy-by-reference is enough
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
function _Copy(const DocVariant: variant): variant;

/// return a full nested copy of a document-based variant instance
// - is just a wrapper around:
// ! TDocVariant.NewUnique(DocVariant, JSON_FAST)
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-value whatever options the nested objects or
// arrays were created with: to be used on a value returned as varByRef
// (e.g. by _() pseudo-method)
// - for huge document with a big depth of nested objects or arrays, a full
// per-value copy may be time and resource consuming, but will be also safe -
// consider using _ByRef() instead if a fast copy-by-reference is enough
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
function _CopyFast(const DocVariant: variant): variant;

/// copy a TDocVariant to another variable, changing the options on the fly
// - note that the content (items or properties) is copied by reference,
// so consider using _Copy() instead if you expect to safely modify its content
// - will return null if the supplied variant is not a TDocVariant
function _ByRef(const DocVariant: variant;
   Options: TDocVariantOptions): variant; overload;

/// copy a TDocVariant to another variable, changing the options on the fly
// - note that the content (items or properties) is copied by reference,
// so consider using _Copy() instead if you expect to safely modify its content
// - will return null if the supplied variant is not a TDocVariant
procedure _ByRef(const DocVariant: variant; out Dest: variant;
  Options: TDocVariantOptions); overload;

/// convert a TDocVariantData array or a string value into a CSV
// - will call either TDocVariantData.ToCsv, or return the string
// - returns '' if the supplied value is neither a TDocVariant or a string
// - could be used e.g. to store either a JSON CSV string or a JSON array of
// strings in a settings property
function _Csv(const DocVariantOrString: variant): RawUtf8;

/// will convert any TObject into a TDocVariant document instance
// - fast processing function as used by _ObjFast(Value)
// - note that the result variable should already be cleared: no VarClear()
// is done by this function
// - would be used e.g. by VarRecToVariant() function
// - if you expect lazy-loading of a TObject, see TObjectVariant.New()
procedure ObjectToVariant(Value: TObject; var result: variant;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]); overload;

/// will convert any TObject into a TDocVariant document instance
// - convenient overloaded function to include woEnumSetsAsText option
function ObjectToVariant(Value: TObject; EnumSetsAsText: boolean): variant; overload;

/// will serialize any TObject into a TDocVariant debugging document
// - just a wrapper around _JsonFast(ObjectToJsonDebug()) with an optional
// "Context":"..." text message
// - if the supplied context format matches '{....}' then it will be added
// as a corresponding TDocVariant JSON object
function ObjectToVariantDebug(Value: TObject;
  const ContextFormat: RawUtf8; const ContextArgs: array of const;
  const ContextName: RawUtf8 = 'context'): variant; overload;

/// get the enumeration names corresponding to a set value, as a JSON array
function SetNameToVariant(Value: cardinal; Info: TRttiCustom;
  FullSetsAsStar: boolean = false): variant;

/// fill a class instance from a TDocVariant object document properties
// - returns FALSE if the variant is not a dvObject, TRUE otherwise
function DocVariantToObject(var doc: TDocVariantData; obj: TObject;
  objRtti: TRttiCustom = nil): boolean;

/// fill a T*ObjArray variable from a TDocVariant array document values
// - will always erase the T*ObjArray instance, and fill it from arr values
procedure DocVariantToObjArray(var arr: TDocVariantData; var objArray;
  objClass: TClass);

/// will convert a blank TObject into a TDocVariant document instance
function ObjectDefaultToVariant(aClass: TClass;
  aOptions: TDocVariantOptions): variant; overload;


type
  /// ref-counted interface for thread-safe access to a TDocVariant document
  // - is implemented e.g. by TLockedDocVariant, for IoC/DI resolution
  // - fast and safe storage of any JSON-like object, as property/value pairs,
  // or a JSON-like array, as values
  ILockedDocVariant = interface
    ['{CADC2C20-3F5D-4539-9D23-275E833A86F3}']
    function GetValue(const Name: RawUtf8): Variant;
    procedure SetValue(const Name: RawUtf8; const Value: Variant);
    /// check and return a given property by name
    // - returns TRUE and fill Value with the value associated with the supplied
    // Name, using an internal lock for thread-safety
    // - returns FALSE if the Name was not found, releasing the internal lock:
    // use ExistsOrLock() if you want to add the missing value
    function Exists(const Name: RawUtf8; out Value: Variant): boolean;
    /// check and return a given property by name
    // - returns TRUE and fill Value with the value associated with the supplied
    // Name, using an internal lock for thread-safety
    // - returns FALSE and set the internal lock if Name does not exist:
    // caller should then release the lock via ReplaceAndUnlock()
    function ExistsOrLock(const Name: RawUtf8; out Value: Variant): boolean;
    /// set a value by property name, and set a local copy
    // - could be used as such, for implementing a thread-safe cache:
    // ! if not cache.ExistsOrLock('prop',local) then
    // !   cache.ReplaceAndUnlock('prop',newValue,local);
    // - call of this method should have been precedeed by ExistsOrLock()
    // returning false, i.e. be executed on a locked instance
    procedure ReplaceAndUnlock(const Name: RawUtf8; const Value: Variant;
      out LocalValue: Variant);
    /// add an existing property value to the given TDocVariant document object
    // - returns TRUE and add the Name/Value pair to Obj if Name is existing,
    // using an internal lock for thread-safety
    // - returns FALSE if Name is not existing in the stored document, and
    // lock the internal storage: caller should eventually release the lock
    // via AddNewPropAndUnlock()
    // - could be used as such, for implementing a thread-safe cache:
    // ! if not cache.AddExistingPropOrLock('Articles',Scope) then
    // !   cache.AddNewPropAndUnlock('Articles',GetArticlesFromDB,Scope);
    // here GetArticlesFromDB would occur inside the main lock
    function AddExistingPropOrLock(const Name: RawUtf8;
      var Obj: variant): boolean;
    /// add a property value to the given TDocVariant document object and
    // to the internal stored document, then release a previous lock
    // - call of this method should have been precedeed by AddExistingPropOrLock()
    // returning false, i.e. be executed on a locked instance
    procedure AddNewPropAndUnlock(const Name: RawUtf8; const Value: variant;
      var Obj: variant);
    /// add an existing property value to the given TDocVariant document object
    // - returns TRUE and add the Name/Value pair to Obj if Name is existing
    // - returns FALSE if Name is not existing in the stored document
    // - this method would use a lock during the Name lookup, but would always
    // release the lock, even if returning FALSE (see AddExistingPropOrLock)
    function AddExistingProp(const Name: RawUtf8; var Obj: variant): boolean;
    /// add a property value to the given TDocVariant document object
    // - this method would not expect the resource to be locked when called,
    // as with AddNewPropAndUnlock
    // - will use the internal lock for thread-safety
    // - if the Name is already existing, would update/change the existing value
    // - could be used as such, for implementing a thread-safe cache:
    // ! if not cache.AddExistingProp('Articles',Scope) then
    // !   cache.AddNewProp('Articles',GetArticlesFromDB,Scope);
    // here GetArticlesFromDB would occur outside the main lock
    procedure AddNewProp(const Name: RawUtf8; const Value: variant;
      var Obj: variant);
    /// append a value to the internal TDocVariant document array
    // - you should not use this method in conjunction with other document-based
    // alternatives, like Exists/AddExistingPropOrLock or AddExistingProp
    procedure AddItem(const Value: variant);
    /// makes a thread-safe copy of the internal TDocVariant document object or array
    function Copy: variant;
    /// delete all stored properties
    procedure Clear;
    /// save the stored values as UTF-8 encoded JSON Object
    function ToJson(HumanReadable: boolean = false): RawUtf8;
    /// low-level access to the associated thread-safe mutex
    function Lock: TAutoLocker;
    /// the document fields would be safely accessed via this property
    // - this is the main entry point of this storage
    // - will raise an EDocVariant exception if Name does not exist at reading
    // - implementation class would make a thread-safe copy of the variant value
    property Value[const Name: RawUtf8]: Variant
      read GetValue write SetValue; default;
  end;

  /// allows thread-safe access to a TDocVariant document
  // - this class inherits from TInterfacedObjectWithCustomCreate so you
  // could define one published property of a mormot.core.interfaces.pas
  // TInjectableObject as ILockedDocVariant so that this class may be
  // automatically injected
  TLockedDocVariant = class(TInterfacedObjectWithCustomCreate, ILockedDocVariant)
  protected
    fValue: TDocVariantData;
    fLock: TAutoLocker;
    function GetValue(const Name: RawUtf8): Variant;
    procedure SetValue(const Name: RawUtf8; const Value: Variant);
  public
    /// initialize the thread-safe document with a fast TDocVariant
    // - i.e. call Create(true) aka Create(JSON_FAST)
    // - will be the TInterfacedObjectWithCustomCreate default constructor,
    // called e.g. during IoC/DI resolution
    constructor Create; overload; override;
    /// initialize the thread-safe document storage from a given template
    constructor Create(options: TDocVariantModel); reintroduce; overload;
    /// initialize the thread-safe document storage with the corresponding options
    constructor Create(options: TDocVariantOptions); reintroduce; overload;
    /// finalize the storage
    destructor Destroy; override;
    /// check and return a given property by name
    function Exists(const Name: RawUtf8;
      out Value: Variant): boolean;
    /// check and return a given property by name
    // - this version
    function ExistsOrLock(const Name: RawUtf8;
      out Value: Variant): boolean;
    /// set a value by property name, and set a local copy
    procedure ReplaceAndUnlock(const Name: RawUtf8; const Value: Variant;
      out LocalValue: Variant);
    /// add an existing property value to the given TDocVariant document object
    // - returns TRUE and add the Name/Value pair to Obj if Name is existing
    // - returns FALSE if Name is not existing in the stored document
    function AddExistingPropOrLock(const Name: RawUtf8;
      var Obj: variant): boolean;
    /// add a property value to the given TDocVariant document object and
    // to the internal stored document
    procedure AddNewPropAndUnlock(const Name: RawUtf8; const Value: variant;
      var Obj: variant);
    /// add an existing property value to the given TDocVariant document object
    // - returns TRUE and add the Name/Value pair to Obj if Name is existing
    // - returns FALSE if Name is not existing in the stored document
    // - this method would use a lock during the Name lookup, but would always
    // release the lock, even if returning FALSE (see AddExistingPropOrLock)
    function AddExistingProp(const Name: RawUtf8;
      var Obj: variant): boolean;
    /// add a property value to the given TDocVariant document object
    // - this method would not expect the resource to be locked when called,
    // as with AddNewPropAndUnlock
    // - will use the internal lock for thread-safety
    // - if the Name is already existing, would update/change the existing value
    procedure AddNewProp(const Name: RawUtf8; const Value: variant;
      var Obj: variant);
    /// append a value to the internal TDocVariant document array
    procedure AddItem(const Value: variant);
    /// makes a thread-safe copy of the internal TDocVariant document object or array
    function Copy: variant;
    /// delete all stored properties
    procedure Clear;
    /// save the stored value as UTF-8 encoded JSON Object
    // - implemented as just a wrapper around VariantSaveJson()
    function ToJson(HumanReadable: boolean = false): RawUtf8;
    /// low-level access to the associated thread-safe mutex
    function Lock: TAutoLocker;
    /// the document fields would be safely accessed via this property
    // - will raise an EDocVariant exception if Name does not exist
    // - result variant is returned as a copy, not as varByRef, since a copy
    // will definitively be more thread safe
    property Value[const Name: RawUtf8]: variant
      read GetValue write SetValue; default;
  end;



{ ************** JSON Parsing into Variant }

/// low-level function to set a variant from an unescaped JSON number or string
// - expect the JSON input buffer to be already unescaped, e.g. by GetJsonField()
// - is called e.g. by function VariantLoadJson()
// - will instantiate either a null, boolean, integer, Int64, currency, double
// (if AllowDouble is true or dvoAllowDoubleValue is in TryCustomVariants^) or
// string value (as RawUtf8), guessing the best numeric type according to the textual content,
// and string in all other cases, except if TryCustomVariants points to some
// options (e.g. @JSON_[mFast] for fast instance) and input is a known
// object or array, either encoded as strict-JSON (i.e. {..} or [..]),
// or with some extended (e.g. BSON) syntax
procedure GetVariantFromJson(Json: PUtf8Char; wasString: boolean;
  var Value: variant; TryCustomVariants: PDocVariantOptions = nil;
  AllowDouble: boolean = false; JsonLen: integer = 0);

/// low-level function to set a variant from an unescaped JSON non string
// - expect the JSON input buffer to be already unescaped, e.g. by GetJsonField(),
// and having returned wasString=TRUE (i.e. not surrounded by double quotes)
// - is called e.g. by function GetVariantFromJson()
// - will recognize null, boolean, integer, Int64, currency, double
// (if AllowDouble is true) input, then set Value and return TRUE
// - returns FALSE if the supplied input has no expected JSON format
function GetVariantFromNotStringJson(Json: PUtf8Char;
  var Value: TVarData; AllowDouble: boolean): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// identify either varInt64, varDouble, varCurrency types following JSON format
// - any non valid number is returned as varString
// - is used e.g. by GetVariantFromJson() to guess the destination variant type
// - warning: supplied JSON is expected to be not nil
function TextToVariantNumberType(Json: PUtf8Char): cardinal;

/// identify either varInt64 or varCurrency types following JSON format
// - this version won't return varDouble, i.e. won't handle more than 4 exact
// decimals (as varCurrency), nor scientific notation with exponent (1.314e10)
// - this will ensure that any incoming JSON will converted back with its exact
// textual representation, without digit truncation due to limited precision
// - any non valid number is returned as varString
// - is used e.g. by GetVariantFromJson() to guess the destination variant type
// - warning: supplied JSON is expected to be not nil
function TextToVariantNumberTypeNoDouble(Json: PUtf8Char): cardinal;

/// low-level function to set a numerical variant from an unescaped JSON number
// - returns TRUE if TextToVariantNumberType/TextToVariantNumberTypeNoDouble(JSON)
// identified it as a number and set Value to the corresponding content
// - returns FALSE if JSON is a string, or null/true/false
function GetNumericVariantFromJson(Json: PUtf8Char;
  var Value: TVarData; AllowVarDouble: boolean): boolean;

 /// return a variant, may be containing a RawUtf8 stored within this class
// - similar to TextToVariant(), but with string interning
// - first try with GetNumericVariantFromJson(), then fallback to
// RawUtf8ToVariant() with string variable interning
procedure UniqueVariant(Interning: TRawUtf8Interning;
  var aResult: variant; aText: PUtf8Char; aTextLen: PtrInt;
  aAllowVarDouble: boolean = false); overload;

/// convert the next CSV item from an UTF-8 encoded text buffer
// into a variant number or RawUtf8 varString
// - first try with GetNumericVariantFromJson(), then fallback to RawUtf8ToVariant
// - is a wrapper around GetNextItem() + TextToVariant()
function GetNextItemToVariant(var P: PUtf8Char;
  out Value: Variant; Sep: AnsiChar = ',';
  AllowDouble: boolean = true): boolean;

/// retrieve a variant value from a JSON buffer as per RFC 8259, RFC 7159, RFC 7158
// - follows TTextWriter.AddVariant() format (calls GetVariantFromJson)
// - will instantiate either an integer, Int64, currency, double or string value
// (as RawUtf8), guessing the best numeric type according to the textual content,
// and string in all other cases, except TryCustomVariants points to some options
// (e.g. @JSON_[mFast] for fast instance) and input is a known object or
// array, either encoded as strict-JSON (i.e. {..} or [..]), or with some
// extended (e.g. BSON) syntax
// - warning: the JSON buffer will be modified in-place during process - use
// a temporary copy or the overloaded functions with RawUtf8 parameter
// if you need to access it later
procedure JsonToVariantInPlace(var Value: Variant; Json: PUtf8Char;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty];
  AllowDouble: boolean = false);

/// retrieve a variant value from a JSON UTF-8 text as per RFC 8259, RFC 7159, RFC 7158
// - follows TTextWriter.AddVariant() format (calls GetVariantFromJson)
// - will instantiate either an integer, Int64, currency, double or string value
// (as RawUtf8), guessing the best numeric type according to the textual content,
// and string in all other cases, except TryCustomVariants points to some options
// (e.g. @JSON_[mFast] for fast instance) and input is a known object or
// array, either encoded as strict-JSON (i.e. {..} or [..]), or with some
// extended (e.g. BSON) syntax
// - this overloaded procedure will make a temporary copy before JSON parsing
// and return the variant as result
function JsonToVariant(const Json: RawUtf8;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty];
  AllowDouble: boolean = false): variant;

/// retrieve a variant value from a JSON number or string
// - follows TTextWriter.AddVariant() format (calls GetVariantFromJson)
// - will instantiate either an integer, Int64, currency, double or string value
// (as RawUtf8), guessing the best numeric type according to the textual content,
// and string in all other cases, except TryCustomVariants points to some options
// (e.g. @JSON_[mFast] for fast instance) and input is a known object or
// array, either encoded as strict-JSON (i.e. {..} or [..]), or with some
// extended (e.g. BSON) syntax
// - warning: the JSON buffer will be modified in-place during process - use
// a temporary copy or the overloaded functions with RawUtf8 parameter
// if you need to access it later
function VariantLoadJson(var Value: variant; Json: PUtf8Char;
  EndOfObject: PUtf8Char = nil; TryCustomVariants: PDocVariantOptions = nil;
  AllowDouble: boolean = false): PUtf8Char; overload;

/// retrieve a variant value from a JSON number or string
// - follows TTextWriter.AddVariant() format (calls GetVariantFromJson)
// - will instantiate either an integer, Int64, currency, double or string value
// (as RawUtf8), guessing the best numeric type according to the textual content,
// and string in all other cases, except TryCustomVariants points to some options
// (e.g. @JSON_[mFast] for fast instance) and input is a known object or
// array, either encoded as strict-JSON (i.e. {..} or [..]), or with some
// extended (e.g. BSON) syntax
// - this overloaded procedure will make a temporary copy before JSON parsing
// and return the variant as result
procedure VariantLoadJson(var Value: Variant; const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions = nil;
  AllowDouble: boolean = false); overload;

/// retrieve a variant value from a JSON number or string
// - follows TTextWriter.AddVariant() format (calls GetVariantFromJson)
// - will instantiate either an integer, Int64, currency, double or string value
// (as RawUtf8), guessing the best numeric type according to the textual content,
// and string in all other cases, except TryCustomVariants points to some options
// (e.g. @JSON_[mFast] for fast instance) and input is a known object or
// array, either encoded as strict-JSON (i.e. {..} or [..]), or with some
// extended (e.g. BSON) syntax
// - this overloaded procedure will make a temporary copy before JSON parsing
// and return the variant as result
function VariantLoadJson(const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions = nil;
  AllowDouble: boolean = false): variant; overload;


{ ************** Variant Binary Serialization }

{$ifndef PUREMORMOT2}

/// compute the number of bytes needed to save a Variant content
// using the VariantSave() function
// - will return 0 in case of an invalid (not handled) Variant type
// - deprecated function - use overloaded BinarySave() functions instead
function VariantSaveLength(const Value: variant): integer; deprecated;
  {$ifdef HASINLINE}inline;{$endif}


/// save a Variant content into a destination memory buffer
// - Dest must be at least VariantSaveLength() bytes long
// - will handle standard Variant types and custom types (serialized as JSON)
// - will return nil in case of an invalid (not handled) Variant type
// - will use a proprietary binary format, with some variable-length encoding
// of the string length
// - warning: will encode generic string fields as within the variant type
// itself: using this function between UNICODE and NOT UNICODE
// versions of Delphi, will propably fail - you have been warned!
// - deprecated function - use overloaded BinarySave() functions instead
function VariantSave(const Value: variant; Dest: PAnsiChar): PAnsiChar;
  overload; deprecated;   {$ifdef HASINLINE}inline;{$endif}

{$endif PUREMORMOT2}

/// save a Variant content into a binary buffer
// - will handle standard Variant types and custom types (serialized as JSON)
// - will return '' in case of an invalid (not handled) Variant type
// - just a wrapper around VariantSaveLength()+VariantSave()
// - warning: will encode generic string fields as within the variant type
// itself: using this function between UNICODE and NOT UNICODE
// versions of Delphi, will propably fail - you have been warned!
// - is a wrapper around BinarySave(rkVariant)
function VariantSave(const Value: variant): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variant value from our optimized binary serialization format
// - follow the data layout as used by RecordLoad() or VariantSave() function
// - return nil if the Source buffer is incorrect
// - in case of success, return the memory buffer pointer just after the
// read content
// - how custom type variants are created can be defined via CustomVariantOptions
// - is a wrapper around BinaryLoad(rkVariant)
function VariantLoad(var Value: variant; Source: PAnsiChar;
  CustomVariantOptions: PDocVariantOptions;
  SourceMax: PAnsiChar = nil): PAnsiChar; overload;

/// retrieve a variant value from our optimized binary serialization format
// - follow the data layout as used by RecordLoad() or VariantSave() function
// - return varEmpty if the Source buffer is incorrect
// - just a wrapper around VariantLoad()
// - how custom type variants are created can be defined via CustomVariantOptions
// - is a wrapper around BinaryLoad(rkVariant)
function VariantLoad(const Bin: RawByteString;
  CustomVariantOptions: PDocVariantOptions): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variant value from variable-length buffer
// - matches TFileBufferWriter.Write()
// - how custom type variants are created can be defined via CustomVariantOptions
// - is just a wrapper around VariantLoad/BinaryLoad
procedure FromVarVariant(var Source: PByte; var Value: variant;
  CustomVariantOptions: PDocVariantOptions = nil);
  {$ifdef HASINLINE}inline;{$endif}


implementation


{ ************** Low-Level Variant Wrappers }

function VarIs(const V: Variant; const VTypes: TVarDataTypes): boolean;
var
  vd: PVarData;
  vt: cardinal;
begin
  vd := @V;
  repeat
    vt := vd^.VType;
    if vt <> varVariantByRef then
      break;
    vd := vd^.VPointer;
    if vd = nil then
    begin
      result := false;
      exit;
    end;
  until false;
  result := vt in VTypes;
end;

function VarIsVoid(const V: Variant): boolean;
var
  vt: cardinal;
begin
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty, varNull:
        result := true;
      varBoolean:
        result := not VBoolean;
      varString, varOleStr {$ifdef HASVARUSTRING}, varUString{$endif}:
        result := VAny = nil;
      varDate:
        result := VInt64 = 0;
    else
      if vt = varVariantByRef then
        result := VarIsVoid(PVariant(VPointer)^)
      else if (vt = varStringByRef) or
              (vt = varOleStrByRef)
              {$ifdef HASVARUSTRING} or
              (vt = varUStringByRef)
              {$endif HASVARUSTRING} then
        result := PPointer(VAny)^ = nil
      else if vt = DocVariantVType then
        result := TDocVariantData(V).Count = 0
      else
        result := false;
    end;
end;

function VarStringOrNull(const v: RawUtf8): variant;
begin
  if v = '' then
    SetVariantNull(result{%H-})
  else
    RawUtf8ToVariant(v, result);
end;

procedure SetVariantByRef(const Source: Variant; var Dest: Variant);
var
  vt: cardinal;
begin
  VarClear(Dest);
  vt := TVarData(Source).VType;
  if ((vt and varByRef) <> 0) or
     (vt in VTYPE_SIMPLE) then
    TVarData(Dest) := TVarData(Source)
  else if not SetVariantUnRefSimpleValue(Source, TVarData(Dest)) then
  begin
    TRttiVarData(Dest).VType := varVariantByRef;
    TVarData(Dest).VPointer := @Source;
  end;
end;

procedure SetVariantByValue(const Source: Variant; var Dest: Variant);
var
  s: PVarData;
  d: TVarData absolute Dest;
  dt: cardinal absolute Dest;
  vt: cardinal;
begin
  s := @Source;
  VarClear(Dest);
  vt := s^.VType;
  if vt = varVariantByRef then
  begin
    s := s^.VPointer;
    vt := s^.VType;
  end;
  case vt of
    varEmpty..varDate, varBoolean, varShortInt..varWord64:
      begin
        dt := vt;
        d.VInt64 := s^.VInt64;
      end;
    varString:
      begin
        dt := varString;
        d.VAny := nil;
        RawByteString(d.VAny) := RawByteString(s^.VAny);
      end;
    varStringByRef:
      begin
        dt := varString;
        d.VAny := nil;
        RawByteString(d.VAny) := PRawByteString(s^.VAny)^;
      end;
    {$ifdef HASVARUSTRING} varUString, varUStringByRef, {$endif}
    varOleStr, varOleStrByRef:
      begin
        dt := varString;
        d.VAny := nil;
        VariantToUtf8(PVariant(s)^, RawUtf8(d.VAny)); // store a RawUtf8 instance
      end;
  else
    if not SetVariantUnRefSimpleValue(PVariant(s)^, d) then
      if vt = DocVariantVType then
        DocVariantType.CopyByValue(d, s^)
      else
        Dest := PVariant(s)^;
  end;
end;

procedure ZeroFill(Value: PVarData);
begin
  // slightly faster than FillChar(Value,SizeOf(Value),0);
  PInt64Array(Value)^[0] := 0;
  PInt64Array(Value)^[1] := 0;
  {$ifdef CPU64}
  PInt64Array(Value)^[2] := 0;
  {$endif CPU64}
end;

procedure FillZero(var value: variant);
begin
  if TVarData(value).VType = varString then
    FillZero(RawByteString(TVarData(value).VAny));
  VarClear(value);
end;

procedure _VariantClearSeveral(V: PVarData; n: integer);
var
  vt, docv: cardinal;
  handler: TCustomVariantType;
  clearproc: procedure(V: PVarData);
label
  clr, hdr;
begin
  handler := nil;
  docv := DocVariantVType;
  clearproc := @VarClearProc;
  repeat
    vt := V^.VType;
    if vt <= varWord64 then
    begin
      if (vt >= varOleStr) and
         (vt <= varError) then
        if vt = varOleStr then
          WideString(V^.VAny) := ''
        else
          goto clr; // varError/varDispatch
    end // note: varVariant/varUnknown are not handled because should not appear
    else if vt = varString then
      {$ifdef FPC}
      FastAssignNew(V^.VAny)
      {$else}
      RawUtf8(V^.VAny) := ''
      {$endif FPC}
    else if vt < varByRef then // varByRef has no refcount -> nothing to clear
      if vt = docv then
        PDocVariantData(V)^.ClearFast
      {$ifdef HASVARUSTRING}
      else if vt = varUString then
        UnicodeString(V^.VAny) := ''
      {$endif HASVARUSTRING}
      else if vt >= varArray then // custom types are below varArray
clr:    clearproc(V)
      else if handler = nil then
        if FindCustomVariantType(vt, handler) then
hdr:      handler.Clear(V^)
        else
          goto clr
      else if vt = handler.VarType then
        goto hdr
      else
        goto clr;
    PInteger(V)^ := varEmpty; // reset VType
    inc(V);
    dec(n);
  until n = 0;
end;

procedure FormatUtf8ToVariant(const Fmt: RawUtf8; const Args: array of const;
  var Value: variant);
begin
  RawUtf8ToVariant(FormatUtf8(Fmt, Args), Value);
end;

procedure RawUtf8ToVariant(const Txt: RawUtf8; var Value: TVarData;
  ExpectedValueType: cardinal);
begin
  if ExpectedValueType = varString then
  begin
    RawUtf8ToVariant(Txt,variant(Value));
    exit;
  end;
  VarClearAndSetType(variant(Value), ExpectedValueType);
  Value.VAny := nil; // avoid GPF below
  if Txt <> '' then
    case ExpectedValueType of
      varOleStr:
        Utf8ToWideString(Txt, WideString(Value.VAny));
      {$ifdef HASVARUSTRING}
      varUString:
        Utf8DecodeToUnicodeString(
          pointer(Txt), length(Txt), UnicodeString(Value.VAny));
      {$endif HASVARUSTRING}
    else
      raise ESynVariant.CreateUtf8('RawUtf8ToVariant(%)?', [ExpectedValueType]);
    end;
end;

function VariantToString(const V: Variant): string;
var
  wasString: boolean;
  tmp: RawUtf8;
  vt: cardinal;
begin
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty, varNull:
        result := ''; // default VariantToUtf8(null)='null'
      {$ifdef UNICODE} // not HASVARUSTRING: here we handle string=UnicodeString
      varUString:
        result := UnicodeString(VAny);
      else
        if vt = varUStringByRef then
          result := PUnicodeString(VAny)^
      {$endif UNICODE}
      else
      begin
        VariantToUtf8(V, tmp, wasString);
        if tmp = '' then
          result := ''
        else
          Utf8ToStringVar(tmp, result);
      end;
    end;
end;

procedure VariantToVarRec(const V: variant; var result: TVarRec);
begin
  result.VType := vtVariant;
  if TVarData(V).VType = varVariantByRef then
    result.VVariant := TVarData(V).VPointer
  else
    result.VVariant := @V;
end;

function VarRecToVariant(const V: TVarRec): variant;
begin
  VarRecToVariant(V, result);
end;

procedure VarRecToVariant(const V: TVarRec; var result: variant);
begin
  VarClear(result{%H-});
  with TVarData(result) do
    case V.VType of
      vtPointer:
        VType := varNull;
      vtBoolean:
        begin
          VType := varBoolean;
          VBoolean := V.VBoolean;
        end;
      vtInteger:
        begin
          VType := varInteger;
          VInteger := V.VInteger;
        end;
      vtInt64:
        begin
          VType := varInt64;
          VInt64 := V.VInt64^;
        end;
      {$ifdef FPC}
      vtQWord:
        begin
          VType := varWord64;
          VQWord := V.VQWord^;
        end;
      {$endif FPC}
      vtCurrency:
        begin
          VType := varCurrency;
          VInt64 := PInt64(V.VCurrency)^;
        end;
      vtExtended:
        begin
          VType := varDouble;
          VDouble := V.VExtended^;
        end;
      vtVariant:
        result := V.VVariant^;
      // warning: use varStringByRef makes GPF -> safe and fast refcount
      vtAnsiString:
        begin
          VType := varString;
          VAny := nil;
          RawByteString(VAny) := RawByteString(V.VAnsiString);
        end;
      {$ifdef HASVARUSTRING}
      vtUnicodeString,
      {$endif HASVARUSTRING}
      vtWideString, vtString, vtPChar, vtChar, vtWideChar, vtClass:
        begin
          VType := varString;
          VString := nil; // avoid GPF on next line
          VarRecToUtf8(V, RawUtf8(VString)); // return as new RawUtf8 instance
        end;
      vtObject:
        // class instance will be serialized as a TDocVariant
        ObjectToVariant(V.VObject, result, [woDontStoreDefault]);
    else
      raise ESynVariant.CreateUtf8('Unhandled TVarRec.VType=%', [V.VType]);
    end;
end;

function VariantDynArrayToJson(const V: TVariantDynArray): RawUtf8;
var
  tmp: TDocVariantData;
begin
  tmp.InitArrayFromVariants(V);
  result := tmp.ToJson;
end;

function JsonToVariantDynArray(const Json: RawUtf8): TVariantDynArray;
var
  tmp: TDocVariantData;
begin
  tmp.InitJson(Json, JSON_FAST);
  result := tmp.VValue;
end;

function ValuesToVariantDynArray(const items: array of const): TVariantDynArray;
var
  tmp: TDocVariantData;
begin
  tmp.InitArray(items, JSON_FAST);
  result := tmp.VValue;
end;


var
  // FastVarDataComp() efficient lookup for per-VType comparison function
  _VARDATACMP: array[boolean, 0 .. $102 {varUString}] of TDynArraySortCompare;

function VariantCompAsUtf8(const A, B: variant; caseInsensitive: boolean): integer;
// need to serialize both as UTF-8 text/JSON
var
  au, bu: RawUtf8;
  wasString: boolean;
begin
  VariantToUtf8(A, au, wasString);
  VariantToUtf8(B, bu, wasString);
  result := StrCompByCase[caseInsensitive](pointer(au), pointer(bu));
end;

function FastVarDataComp(A, B: PVarData; caseInsensitive: boolean): integer;
var
  at, bt: cardinal;
  sametypecomp: TDynArraySortCompare;
label
  rtl, utf;
begin
  if A <> nil then
    repeat
      at := cardinal(A^.VType);
      if at <> varVariantByRef then
        break;
      A := A^.VPointer;
    until false
  else
    at := varNull;
  if B <> nil then
    repeat
      bt := cardinal(B^.VType);
      if bt <> varVariantByRef then
        break;
      B := B^.VPointer;
    until false
  else
    bt := varNull;
  if at = bt then
    // optimized comparison if A and B share the same type (most common case)
    if at <= high(_VARDATACMP[false]) then
    begin
      sametypecomp := _VARDATACMP[caseInsensitive, at];
      if Assigned(sametypecomp) then
        result := sametypecomp(A^.VAny, B^.VAny)
      else
rtl:    result := VariantCompSimple(PVariant(A)^, PVariant(B)^)
    end
    else if at = varStringByRef then
      // e.g. from TRttiVarData / TRttiCustomProp.CompareValue
      result := _VARDATACMP[caseInsensitive, varString](
        PPointer(A^.VAny)^, PPointer(B^.VAny)^)
    else if at = varSynUnicode or varByRef then
      result := _VARDATACMP[caseInsensitive, varSynUnicode](
         PPointer(A^.VAny)^, PPointer(B^.VAny)^)
    else if at < varFirstCustom then
      goto rtl
    else if at = DocVariantVType then
      // direct TDocVariantDat.VName/VValue comparison with no serialization
      result := PDocVariantData(A)^.Compare(PDocVariantData(B)^, caseInsensitive)
    else
      // compare from custom types UTF-8 text representation/serialization
utf:  result := VariantCompAsUtf8(PVariant(A)^, PVariant(B)^, caseInsensitive)
  // A and B do not share the same type
  else if (at <= varNull) or
          (bt <= varNull) then
    result := ord(at > varNull) - ord(bt > varNull)
  else if (at < varString) and
          (at <> varOleStr) and
          (bt < varString) and
          (bt <> varOleStr) then
    goto rtl
  else
    goto utf;
end;

function VariantCompare(const V1, V2: variant): PtrInt;
begin
  result := FastVarDataComp(@V1, @V2, {caseins=}false);
end;

function VariantCompareI(const V1, V2: variant): PtrInt;
begin
  result := FastVarDataComp(@V1, @V2, {caseins=}true);
end;

function VariantEquals(const V: Variant; const Str: RawUtf8;
  CaseSensitive: boolean): boolean;

  function Complex: boolean;
  var
    wasString: boolean;
    tmp: RawUtf8;
  begin
    VariantToUtf8(V, tmp, wasString);
    if CaseSensitive then
      result := (tmp = Str)
    else
      result := IdemPropNameU(tmp, Str);
  end;

var
  v1, v2: Int64;
  vt: cardinal;
begin
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty, varNull:
        result := Str = '';
      varBoolean:
        result := VBoolean = (Str <> '');
      varString:
        if CaseSensitive then
          result := RawUtf8(VString) = Str
        else
          result := IdemPropNameU(RawUtf8(VString), Str);
    else
      if VariantToInt64(V, v1) then
      begin
        SetInt64(pointer(Str), v2);
        result := v1 = v2;
      end
      else
        result := Complex;
    end;
end;


{ ************** Custom Variant Types with JSON support }

procedure GetJsonToAnyVariant(var Value: variant; var Json: PUtf8Char;
  EndOfObject: PUtf8Char; Options: PDocVariantOptions; AllowDouble: boolean); forward;


var
  /// internal list of our TSynInvokeableVariantType instances
  // - SynVariantTypes[0] is always DocVariantVType
  // - SynVariantTypes[1] is typically BsonVariantType from mormot.db.nosql.bson
  // - instances are owned by Variants.pas as TInvokeableVariantType /
  // TCustomVariantType
  SynVariantTypes: array of TSynInvokeableVariantType;

  /// list of custom types (but not DocVariantVType) supporting TryJsonToVariant
  SynVariantTryJsonTypes: array of TSynInvokeableVariantType;

function FindSynVariantTypeFromVType(aVarType: word): TSynInvokeableVariantType;
  {$ifdef HASINLINE}inline;{$endif}
var
  n: integer;
  t: ^TSynInvokeableVariantType;
begin
  if (cardinal(aVarType) >= varFirstCustom) and
     (cardinal(aVarType) < varArray) then
  begin
    t := pointer(SynVariantTypes);
    if t <> nil then
    begin
      n := PDALen(PAnsiChar(t) - _DALEN)^ + _DAOFF;
      repeat
        result := t^;
        if result.VarType = aVarType then
          exit;
        inc(t);
        dec(n);
      until n = 0;
    end;
  end;
  result := nil;
end;

function SynRegisterCustomVariantType(
  aClass: TSynInvokeableVariantTypeClass): TSynInvokeableVariantType;
var
  i: PtrInt;
begin
  GlobalLock;
  try
    for i := 0 to length(SynVariantTypes) - 1 do
    begin
      result := SynVariantTypes[i];
      if PPointer(result)^ = pointer(aClass) then
        // returns already registered instance
        exit;
    end;
    result := aClass.Create; // register variant type
    ObjArrayAdd(SynVariantTypes, result);
    if sioHasTryJsonToVariant in result.Options then
      ObjArrayAdd(SynVariantTryJsonTypes, result);
  finally
    GlobalUnLock;
  end;
end;


{ TSynInvokeableVariantType }

constructor TSynInvokeableVariantType.Create;
begin
  inherited Create; // call RegisterCustomVariantType(self)
end;

function TSynInvokeableVariantType.IterateCount(const V: TVarData): integer;
begin
  result := -1; // this is not an array
end;

procedure TSynInvokeableVariantType.Iterate(var Dest: TVarData;
  const V: TVarData; Index: integer);
begin
  // do nothing
end;

{$ifdef ISDELPHI}
function TSynInvokeableVariantType.FixupIdent(const AText: string): string;
begin
  result := AText; // NO uppercased identifier for our custom types!
end;
{$endif ISDELPHI}

function TSynInvokeableVariantType.{%H-}IntGet(var Dest: TVarData;
  const Instance: TVarData; Name: PAnsiChar; NameLen: PtrInt;
  NoException: boolean): boolean;
begin
  raise ESynVariant.CreateUtf8('Unexpected %.IntGet(%): this kind of ' +
    'custom variant does not support sub-fields', [self, Name]);
end;

function TSynInvokeableVariantType.{%H-}IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
begin
  raise ESynVariant.CreateUtf8('Unexpected %.IntSet(%): this kind of ' +
    'custom variant is read-only', [self, Name]);
end;

const
  DISPATCH_METHOD = 1;
  DISPATCH_PROPERTYGET = 2; // in practice, never generated by the FPC compiler
  DISPATCH_PROPERTYPUT = 4;
  ARGTYPE_MASK = $7f;
  ARGREF_MASK = $80;
  VAR_PARAMNOTFOUND = HRESULT($80020004);

{$ifdef FPC}
var
  DispInvokeArgOrderInverted: boolean; // circumvent FPC 3.2+ breaking change
{$endif FPC}

procedure TSynInvokeableVariantType.DispInvoke(
{$ifdef FPC_VARIANTSETVAR}
  Dest: PVarData; var Source: TVarData;
{$else} // see http://mantis.freepascal.org/view.php?id=26773
  {$ifdef ISDELPHIXE7}
  Dest: PVarData; [ref] const Source: TVarData; // why not just "var" ????
  {$else}
  Dest: PVarData; const Source: TVarData;
  {$endif ISDELPHIXE7}
{$endif FPC_VARIANTSETVAR}
  CallDesc: PCallDesc; Params: Pointer);
var
  name: string;
  res: TVarData;
  namelen, i, t, n: PtrInt;
  nameptr, a: PAnsiChar;
  asize: PtrInt;
  v: PVarData;
  args: TVarDataArray; // DoProcedure/DoFunction require a dynamic array
  {$ifdef FPC}
  inverted: boolean;
  {$endif FPC}

  procedure RaiseInvalid;
  begin
    raise ESynVariant.CreateUtf8('%.DispInvoke: invalid %(%) call',
      [self, name, CallDesc^.ArgCount]);
  end;

  procedure TryFunction;
  begin
    if not DoFunction(Dest^, Source, name, args) then
      RaiseInvalid;
  end;

begin
  // circumvent https://bugs.freepascal.org/view.php?id=38653 and
  // inverted args order FPC bugs, avoid unneeded conversion to varOleString
  // for Delphi, and implement direct IntGet/IntSet calls for all
  n := CallDesc^.ArgCount;
  nameptr := @CallDesc^.ArgTypes[n];
  namelen := StrLen(nameptr);
  // faster direct property getter
  if (Dest <> nil) and
     (n = 0) and
     (CallDesc^.CallType in [DISPATCH_METHOD, DISPATCH_PROPERTYGET]) and
     IntGet(Dest^, Source, nameptr, namelen, {noexception=}false) then
    exit;
  Ansi7ToString(pointer(nameptr), namelen, name);
  if n > 0 then
  begin
    // convert varargs Params buffer into an array of TVarData
    SetLength(args, n);
    {$ifdef FPC} // circumvent FPC 3.2+ inverted order
    inverted := (n > 1) and
                DispInvokeArgOrderInverted;
    if inverted then
      v := @args[n - 1]
    else
    {$endif FPC}
      v := pointer(args);
    a := Params;
    for i := 0 to n - 1 do
    begin
      asize := SizeOf(pointer);
      t := CallDesc^.ArgTypes[i] and ARGTYPE_MASK;
      case t of
        {$ifdef HASVARUSTRARG}
        varUStrArg:
          t := varUString;
        {$endif HASVARUSTRARG}
        varStrArg:
          t := varString;
      end;
      if CallDesc^.ArgTypes[i] and ARGREF_MASK <> 0 then
      begin
        TRttiVarData(v^).VType := t or varByRef;
        v^.VPointer := PPointer(a)^;
      end
      else
      begin
        TRttiVarData(v^).VType := t;
        case t of
          varError:
            begin
              v^.VError := VAR_PARAMNOTFOUND;
              asize := 0;
            end;
          varVariant:
            {$ifdef CPU32DELPHI}
            begin
              v^ := PVarData(a)^;
              asize := SizeOf(TVarData); // pushed by value
            end;
            {$else}
            v^ := PPVarData(a)^^; // pushed by reference (as other parameters)
            {$endif CPU32DELPHI}
          varDouble,
          varCurrency,
          varDate,
          varInt64,
          varWord64:
            begin
              v^.VInt64 := PInt64(a)^;
              asize := SizeOf(Int64);
            end;
          // small values are stored as pointers on stack but pushed as 32-bit
          varSingle,
          varSmallint,
          varInteger,
          varLongWord,
          varBoolean,
          varShortInt,
          varByte,
          varWord:
            v^.VInteger := PInteger(a)^; // we assume little endian
        else
          v^.VAny := PPointer(a)^; // e.g. varString or varOleStr
        end;
      end;
      inc(a, asize);
      {$ifdef FPC}
      if inverted then
        dec(v)
      else
      {$endif FPC}
        inc(v);
    end;
  end;
  case CallDesc^.CallType of
    // note: IntGet was already tried in function trailer
    DISPATCH_METHOD:
      if Dest <> nil then
        TryFunction
      else if not DoProcedure(Source, name, args) then
      begin
        PCardinal(@res)^ := varEmpty;
        try
          TryFunction;
        finally
          VarClearProc(res);
        end;
      end;
    DISPATCH_PROPERTYGET:
      if Dest <> nil then
        TryFunction
      else
        RaiseInvalid;
    DISPATCH_PROPERTYPUT:
      if (Dest <> nil) or
         (n <> 1) or
         not IntSet(Source, args[0], nameptr, namelen) then
        RaiseInvalid;
  else
    RaiseInvalid;
  end;
end;

procedure TSynInvokeableVariantType.Clear(var V: TVarData);
begin
  ZeroFill(@V); // will set V.VType := varEmpty
end;

procedure TSynInvokeableVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: boolean);
begin
  if Indirect then
    SetVariantByRef(variant(Source), variant(Dest))
  else
  begin
    VarClear(variant(Dest)); // Dest may be a complex type
    Dest := Source;
  end;
end;

procedure TSynInvokeableVariantType.CopyByValue(
  var Dest: TVarData; const Source: TVarData);
begin
  Copy(Dest, Source, false);
end;

function TSynInvokeableVariantType.TryJsonToVariant(var Json: PUtf8Char;
  var Value: variant; EndOfObject: PUtf8Char): boolean;
begin
  result := false;
end;

procedure TSynInvokeableVariantType.ToJson(W: TTextWriter; const Value: variant);
begin
  raise ESynVariant.CreateUtf8('%.ToJson is not implemented', [self]);
end;

function TSynInvokeableVariantType.IsOfType(const V: variant): boolean;
var
  vt: cardinal;
  vd: PVarData;
{%H-}begin
  if self <> nil then
  begin
    vd := @V;
    repeat
      vt := vd^.VType;
      if vt <> varVariantByRef then
        break;
      vd := vd^.VPointer;
    until false;
    result := vt = VarType;
  end
  else
    result := false;
end;

function TSynInvokeableVariantType.FindSynVariantType(aVarType: Word;
  out CustomType: TSynInvokeableVariantType): boolean;
begin
  if (self <> nil) and
     (aVarType = VarType) then
    CustomType := self
  else
    CustomType := FindSynVariantTypeFromVType(aVarType);
  result := CustomType <> nil;
end;

procedure TSynInvokeableVariantType.Lookup(var Dest: TVarData;
  const Instance: TVarData; FullName: PUtf8Char);
var
  handler: TSynInvokeableVariantType;
  v, tmp: TVarData; // PVarData wouldn't store e.g. RowID/count
  vt: cardinal;
  itemName: ShortString;
begin
  TRttiVarData(Dest).VType := varEmpty; // left to Unassigned if not found
  v := Instance;
  repeat
    vt := v.VType;
    if vt <> varVariantByRef then
      break;
    v := PVarData(v.VPointer)^;
  until false;
  repeat
    if vt < varFirstCustom then
      exit; // we need a complex type to lookup
    GetNextItemShortString(FullName, itemName, '.'); // itemName ends with #0
    if itemName[0] in [#0, #254] then
      exit;
    if vt = VarType then
      handler := self
    else
    begin
      handler := FindSynVariantTypeFromVType(vt);
      if handler = nil then
        exit;
    end;
    tmp := v; // v will be modified in-place
    TRttiVarData(v).VType := varEmpty; // IntGet() would clear it otherwise!
    if not handler.IntGet(v, tmp, @itemName[1], ord(itemName[0]), {noexc=}true) then
      exit; // property not found (no exception should be raised in Lookup)
    repeat
      vt := v.VType;
      if vt <> varVariantByRef then
        break;
      v := PVarData(v.VPointer)^;
    until false;
    if (vt = DocVariantVType) and
       (TDocVariantData(v).VCount = 0) then
      // recognize void TDocVariant as null
      v.VType := varNull; // do not use PCardinal/TRttiVarData(v).VType here
  until FullName = nil;
  Dest := v;
end;

function CustomVariantToJson(W: TTextWriter; const Value: variant;
  Escape: TTextWriterKind): boolean;
var
  v: TCustomVariantType;
  tmp: variant;
begin
  result := true;
  if FindCustomVariantType(TVarData(Value).VType, v) then
    if v.InheritsFrom(TSynInvokeableVariantType) then
      TSynInvokeableVariantType(v).ToJson(W, Value)
    else
      try
        v.CastTo(TVarData(tmp), TVarData(Value), varNativeString);
        W.AddVariant(tmp, Escape);
      except
        result := false;
      end
  else
    result := false;
end;


function ToText(kind: TDocVariantKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TDocVariantKind), ord(kind));
end;


{ EDocVariant }

class procedure EDocVariant.RaiseSafe(Kind: TDocVariantKind);
begin
  raise CreateUtf8('_Safe(%)?', [ToText(Kind)^]);
end;


// defined here for proper inlining
// PInteger() is faster than (dvoIsObject in VOptions) especially on Intel CPUs

function TDocVariantData.GetKind: TDocVariantKind;
var
  c: cardinal;
begin
  c := PInteger(@self)^;
  if (c and (1 shl (ord(dvoIsObject) + 16))) <> 0 then
    result := dvObject
  else if (c and (1 shl (ord(dvoIsArray) + 16))) <> 0 then
    result := dvArray
  else
    result := dvUndefined;
end;

function TDocVariantData.IsObject: boolean;
begin
  result := (PInteger(@self)^ and (1 shl (ord(dvoIsObject) + 16))) <> 0;
end;

function TDocVariantData.IsArray: boolean;
begin
  result := (PInteger(@self)^ and (1 shl (ord(dvoIsArray) + 16))) <> 0;
end;


{ TDocVariant }

destructor TDocVariant.Destroy;
begin
  inherited Destroy;
  fInternNames.Free;
  fInternValues.Free;
end;

function IntGetPseudoProp(ndx: integer; const source: TDocVariantData;
  var Dest: variant): boolean;
begin
  // sub-function to avoid temporary RawUtf8
  result := true;
  case ndx of
    0:
      Dest := source.Count;
    1:
      Dest := ord(source.GetKind);
    2:
      RawUtf8ToVariant(source.ToJson, Dest);
  else
    result := false;
  end;
end;

function TDocVariant.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean;
var
  dv: TDocVariantData absolute Instance;
  ndx: integer;
begin
  if Name = nil then
    result := false
  else if (NameLen > 4) and
          (Name[0] = '_') and
          IntGetPseudoProp(IdemPCharArray(@Name[1],
            ['COUNT',
             'KIND',
             'JSON']), dv, variant(Dest)) then
    result := true
  else
  begin
    ndx := dv.GetValueIndex(pointer(Name), NameLen,
      dvoNameCaseSensitive in dv.VOptions);
    if ndx < 0 then
      if NoException or
         (dvoReturnNullForUnknownProperty in dv.VOptions) then
      begin
        SetVariantNull(PVariant(@Dest)^);
        result := false;
      end
      else
        raise EDocVariant.CreateUtf8('[%] property not found', [Name])
    else
    begin
      SetVariantByRef(dv.VValue[ndx], PVariant(@Dest)^);
      result := true;
    end;
  end;
end;

function TDocVariant.IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var
  ndx: PtrInt;
  aName: RawUtf8;
  dv: TDocVariantData absolute Instance;
begin
  result := true;
  if dv.IsArray and
     (PWord(Name)^ = ord('_')) then
  begin
    dv.AddItem(variant(Value));
    exit;
  end;
  ndx := dv.GetValueIndex(
    pointer(Name), NameLen, dvoNameCaseSensitive in dv.VOptions);
  if ndx < 0 then
  begin
    FastSetString(aName, Name, NameLen);
    ndx := dv.InternalAdd(aName);
  end;
  SetVariantByValue(variant(Value), dv.VValue[ndx]);
  if dvoInternValues in dv.VOptions then
    DocVariantType.InternValues.UniqueVariant(dv.VValue[ndx]);
end;

function TDocVariant.IterateCount(const V: TVarData): integer;
var
  Data: TDocVariantData absolute V;
begin
  if Data.IsArray then
    result := Data.VCount
  else
    result := -1;
end;

procedure TDocVariant.Iterate(var Dest: TVarData;
  const V: TVarData; Index: integer);
var
  Data: TDocVariantData absolute V;
begin
  if Data.IsArray and
     (cardinal(Index) < cardinal(Data.VCount)) then
    Dest := TVarData(Data.VValue[Index])
  else
    TRttiVarData(Dest).VType := varEmpty;
end;

function TDocVariant.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): boolean;
var
  Data: PDocVariantData;
begin
  result := false;
  Data := @V; // allow to modify a const argument
  case length(Arguments) of
    0:
      if SameText(Name, 'Clear') then
      begin
        Data^.VCount := 0;
        Data^.VOptions := Data^.VOptions - [dvoIsObject, dvoIsArray];
        result := true;
      end;
    1:
      if SameText(Name, 'Add') then
      begin
        Data^.AddItem(variant(Arguments[0]));
        result := true;
      end
      else if SameText(Name, 'Delete') then
      begin
        Data^.Delete(Data^.GetValueIndex(ToUtf8(Arguments[0])));
        result := true;
      end;
    2:
      if SameText(Name, 'Add') then
      begin
        Data^.AddValue(ToUtf8(Arguments[0]), variant(Arguments[1]));
        result := true;
      end;
  end;
end;

function TDocVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): boolean;
var
  ndx: integer;
  Data: PDocVariantData;
  temp: RawUtf8;
begin
  result := true;
  Data := @V; // allow to modify a const argument
  case length(Arguments) of
    1:
      if SameText(Name, 'Exists') then
      begin
        variant(Dest) := Data.GetValueIndex(ToUtf8(Arguments[0])) >= 0;
        exit;
      end
      else if SameText(Name, 'NameIndex') then
      begin
        variant(Dest) := Data.GetValueIndex(ToUtf8(Arguments[0]));
        exit;
      end
      else if VariantToInteger(variant(Arguments[0]), ndx) then
      begin
        if (Name = '_') or
           SameText(Name, 'Value') then
        begin
          Data.RetrieveValueOrRaiseException(ndx, variant(Dest), true);
          exit;
        end
        else if SameText(Name, 'Name') then
        begin
          Data.RetrieveNameOrRaiseException(ndx, temp);
          RawUtf8ToVariant(temp, variant(Dest));
          exit;
        end;
      end
      else if (Name = '_') or
              SameText(Name, 'Value') then
      begin
        temp := ToUtf8(Arguments[0]);
        Data.RetrieveValueOrRaiseException(pointer(temp), length(temp),
          dvoNameCaseSensitive in Data.VOptions, variant(Dest), true);
        exit;
      end;
  end;
  result := dvoReturnNullForUnknownProperty in Data.VOptions; // to avoid error
end;

procedure TDocVariant.ToJson(W: TTextWriter; const Value: variant);
var
  ndx: PtrInt;
  vt: cardinal;
  forced: TTextWriterOptions;
  checkExtendedPropName: boolean;
begin
  vt := TDocVariantData(Value).VType;
  if vt > varNull then
    if vt = DocVariantVType then
      with TDocVariantData(Value) do
        if [dvoIsArray, dvoIsObject] * VOptions = [] then
          W.AddNull
        else
        begin
          forced := [];
          if [twoForceJsonExtended, twoForceJsonStandard] * W.CustomOptions = [] then
          begin
            if dvoSerializeAsExtendedJson in VOptions then
              forced := [twoForceJsonExtended]
            else
              forced := [twoForceJsonStandard];
            W.CustomOptions := W.CustomOptions + forced;
          end;
          if IsObject then
          begin
            checkExtendedPropName := twoForceJsonExtended in W.CustomOptions;
            W.Add('{');
            for ndx := 0 to VCount - 1 do
            begin
              if checkExtendedPropName and
                 JsonPropNameValid(pointer(VName[ndx])) then
                W.AddNoJsonEscape(pointer(VName[ndx]), Length(VName[ndx]))
              else
              begin
                W.Add('"');
                W.AddJsonEscape(pointer(VName[ndx]));
                W.Add('"');
              end;
              W.Add(':');
              W.AddVariant(VValue[ndx], twJsonEscape);
              W.AddComma;
            end;
            W.CancelLastComma;
            W.Add('}');
          end
          else
          begin
            W.Add('[');
            for ndx := 0 to VCount - 1 do
            begin
              W.AddVariant(VValue[ndx], twJsonEscape);
              W.AddComma;
            end;
            W.CancelLastComma;
            W.Add(']');
          end;
          if forced <> [] then
            W.CustomOptions := W.CustomOptions - forced;
        end
    else
      raise EDocVariant.CreateUtf8('Unexpected variant type %', [vt])
  else
    W.AddNull;
end;

procedure TDocVariant.Clear(var V: TVarData);
begin
  //Assert(V.VType=DocVariantVType);
  TDocVariantData(V).ClearFast;
end;

procedure TDocVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: boolean);
var
  S: TDocVariantData absolute Source;
  D: TDocVariantData absolute Dest;
  i: PtrInt;
begin
  //Assert(Source.VType=DocVariantVType);
  if Indirect then
  begin
    SetVariantByRef(variant(Source), variant(Dest));
    exit;
  end;
  VarClearAndSetType(variant(Dest), PCardinal(@S)^); // VType + VOptions
  pointer(D.VName) := nil; // avoid GPF
  pointer(D.VValue) := nil;
  D.VCount := S.VCount;
  if S.VCount = 0 then
    exit; // no data to copy
  D.VName := S.VName;
  if dvoValueCopiedByReference in S.VOptions then
    D.VValue := S.VValue
  else
  begin
    SetLength(D.VValue, S.VCount);
    for i := 0 to S.VCount - 1 do
      D.VValue[i] := S.VValue[i];
  end;
end;

procedure TDocVariant.CopyByValue(var Dest: TVarData; const Source: TVarData);
begin
  //Assert(Source.VType=DocVariantVType);
  Copy(Dest, Source, {indirect=}false);
end;

procedure TDocVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest, Source, VarType);
end;

procedure TDocVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  json: RawUtf8;
  wasString: boolean;
begin
  if AVarType = VarType then
  begin
    VariantToUtf8(Variant(Source), json, wasString);
    if wasString then
    begin
      VarClear(variant(Dest));
      variant(Dest) := _JsonFast(json); // convert from JSON text
      exit;
    end;
    RaiseCastError;
  end
  else
  begin
    if Source.VType <> VarType then
      RaiseCastError;
    VariantSaveJson(variant(Source), twJsonEscape, json);
    RawUtf8ToVariant(json, Dest, AVarType); // convert to JSON text
  end;
end;

procedure TDocVariant.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
var
  res: integer;
begin
  res := FastVarDataComp(@Left, @Right, {caseins=}false);
  if res < 0 then
    Relationship := crLessThan
  else if res > 0 then
    Relationship := crGreaterThan
  else
    Relationship := crEqual;
end;

class procedure TDocVariant.New(out aValue: variant;
  aOptions: TDocVariantOptions);
begin
  TDocVariantData(aValue).Init(aOptions);
end;

class procedure TDocVariant.NewFast(out aValue: variant;
  aKind: TDocVariantKind);
begin
  TVarData(aValue) := DV_FAST[aKind];
end;

class procedure TDocVariant.IsOfTypeOrNewFast(var aValue: variant);
begin
  if DocVariantType.IsOfType(aValue) then
    exit;
  VarClear(aValue);
  TVarData(aValue) := DV_FAST[dvUndefined];
end;

class procedure TDocVariant.NewFast(const aValues: array of PDocVariantData;
  aKind: TDocVariantKind);
var
  i: PtrInt;
  def: PDocVariantData;
begin
  def := @DV_FAST[aKind];
  for i := 0 to high(aValues) do
    aValues[i]^ := def^;
end;

class function TDocVariant.New(Options: TDocVariantOptions): Variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).Init(Options);
end;

class function TDocVariant.NewObject(const NameValuePairs: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitObject(NameValuePairs, Options);
end;

class function TDocVariant.NewArray(const Items: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArray(Items, Options);
end;

class function TDocVariant.NewArray(const Items: TVariantDynArray;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArrayFromVariants(Items, Options);
end;

class function TDocVariant.NewJson(const Json: RawUtf8;
  Options: TDocVariantOptions): variant;
begin
  _Json(Json, result, Options);
end;

class function TDocVariant.NewUnique(const SourceDocVariant: variant;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitCopy(SourceDocVariant, Options);
end;

class procedure TDocVariant.GetSingleOrDefault(
  const docVariantArray, default: variant; var result: variant);
var
  vt: cardinal;
begin
  vt := TVarData(docVariantArray).VType;
  if vt = varVariantByRef then
    GetSingleOrDefault(
      PVariant(TVarData(docVariantArray).VPointer)^, default, result)
  else if (vt <> DocVariantVType) or
          (TDocVariantData(docVariantArray).Count <> 1) or
          not TDocVariantData(docVariantArray).IsArray then
    result := default
  else
    result := TDocVariantData(docVariantArray).Values[0];
end;

function DocVariantData(const DocVariant: variant): PDocVariantData;
var
  docv, vt: cardinal;
begin
  result := @DocVariant;
  docv := DocVariantVType;
  vt := result^.VType;
  if vt = docv then
    exit
  else if vt = varVariantByRef then
  begin
    result := PVarData(result)^.VPointer;
    if cardinal(result^.VType) = docv then
      exit;
  end;
  raise EDocVariant.CreateUtf8('DocVariantType.Data(%<>TDocVariant)',
    [ord(result^.VType)]);
end;

{$ifdef FPC_OR_UNICODE} // Delphi has problems inlining this :(
function _Safe(const DocVariant: variant): PDocVariantData;
var
  docv, vt: cardinal;
begin
  result := @DocVariant;
  docv := DocVariantVType;
  vt := result^.VType;
  if vt = docv then
    exit
  else if vt = varVariantByRef then
  begin
    result := PVarData(result)^.VPointer;
    if cardinal(result^.VType) = docv then
      exit;
  end;
  result := @DocVariantDataFake;
end;
{$else} // fallback for Delphi 7/2007
function _Safe(const DocVariant: variant): PDocVariantData;
asm
        mov     ecx, DocVariantVType
        movzx   edx, word ptr [eax].TVarData.VType
        cmp     edx, ecx
        jne     @by
        ret
@ptr:   mov     eax, [eax].TVarData.VPointer
        movzx   edx, word ptr [eax].TVarData.VType
        cmp     edx, ecx
        je      @ok
@by:    cmp     edx, varVariantByRef
        je      @ptr
        lea     eax, [DocVariantDataFake]
@ok:
end;
{$endif FPC_OR_UNICODE}

function _Safe(const DocVariant: variant; out DV: PDocVariantData): boolean;
var
  docv, vt: cardinal;
  v: PDocVariantData;
label
  no;
begin
  docv := DocVariantVType;
  v := @DocVariant;
  vt := v^.VType;
  if vt <> docv then
    if vt <> varVariantByRef then
    begin
no:   result := false;
      exit;
    end
    else
    begin
      v := PVarData(v)^.VPointer;
      if cardinal(v^.VType) <> docv then
        goto no;
    end;
  DV := v;
  result := true;
end;

function _SafeArray(const Value: variant; out DV: PDocVariantData): boolean;
begin
  result := _Safe(Value, DV) and
            not {%H-}DV^.IsObject;
end;

function _SafeObject(const Value: variant; out DV: PDocVariantData): boolean;
begin
  result := _Safe(Value, DV) and
            not {%H-}DV^.IsArray;
end;

function _Safe(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): PDocVariantData;
begin
  if ExpectedKind = dvArray then
  begin
    if _SafeArray(DocVariant, result) then
      exit;
  end else if (ExpectedKind = dvObject) and
              _SafeObject(DocVariant, result) then
    exit;
  EDocVariant.RaiseSafe(ExpectedKind);
end;

function _DV(const DocVariant: variant): TDocVariantData;
begin
  result := _Safe(DocVariant)^;
end;

function _DV(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): TDocVariantData;
begin
  result := _Safe(DocVariant, ExpectedKind)^;
end;

function _DV(const DocVariant: variant; var DV: TDocVariantData): boolean;
var
  docv, vt: cardinal;
  v: PDocVariantData;
label
  no;
begin
  docv := DocVariantVType;
  v := @DocVariant;
  vt := v^.VType;
  if vt <> docv then
    if vt <> varVariantByRef then
    begin
no:   result := false;
      exit;
    end
    else
    begin
      v := PVarData(v)^.VPointer;
      if cardinal(v^.VType) <> docv then
        goto no;
    end;
  DV := v^;
  result := true;
end;

function _Csv(const DocVariantOrString: variant): RawUtf8;
begin
  with _Safe(DocVariantOrString)^ do
    if IsArray then
      result := ToCsv
    else if IsObject or
            (TDocVariantData(DocVariantOrString).VType <= varNull) or
            not VariantToUtf8(DocVariantOrString, result) then
      result := ''; // VariantToUtf8() returns 'null' for empty/null
end;

function ObjectToVariant(Value: TObject; EnumSetsAsText: boolean): variant;
const
  OPTIONS: array[boolean] of TTextWriterWriteObjectOptions = (
     [woDontStoreDefault], [woDontStoreDefault, woEnumSetsAsText]);
begin
  ObjectToVariant(Value, result, OPTIONS[EnumSetsAsText]);
end;

function ObjectToVariantDebug(Value: TObject;
  const ContextFormat: RawUtf8; const ContextArgs: array of const;
  const ContextName: RawUtf8): variant;
begin
  ObjectToVariant(Value, result, [woDontStoreDefault, woEnumSetsAsText]);
  if ContextFormat <> '' then
    if ContextFormat[1] = '{' then
      _ObjAddProps([ContextName,
        _JsonFastFmt(ContextFormat, [], ContextArgs)], result)
    else
      _ObjAddProps([ContextName,
        FormatUtf8(ContextFormat, ContextArgs)], result);
end;

procedure ObjectToVariant(Value: TObject; var result: variant;
  Options: TTextWriterWriteObjectOptions);
var
  json: RawUtf8;
begin
  VarClear(result{%H-});
  json := ObjectToJson(Value, Options);
  if PDocVariantData(@result)^.InitJsonInPlace(
      pointer(json), JSON_FAST) = nil then
    VarClear(result);
end;

function SetNameToVariant(Value: cardinal; Info: TRttiCustom;
  FullSetsAsStar: boolean): variant;
var
  bit: PtrInt;
  PS: PShortString;
  arr: TDocVariantData;
begin
  TVarData(arr) := DV_FAST[dvArray];
  if FullSetsAsStar and
     GetAllBits(Value, Info.Cache.EnumMax + 1) then
    arr.AddItem('*')
  else
    with Info.Cache do
    begin
      PS := @EnumList;
      for bit := EnumInfo.MinValue to EnumMax do
      begin
        if GetBitPtr(@Value, bit) then
          arr.AddItem(PS^);
        inc(PByte(PS), ord(PS^[0]) + 1); // next item
      end;
    end;
  result := variant(arr);
end;

function DocVariantToObject(var doc: TDocVariantData; obj: TObject;
  objRtti: TRttiCustom): boolean;
var
  p: PtrInt;
  prop: PRttiCustomProp;
begin
  if doc.IsObject and
     (doc.Count > 0) and
     (obj <> nil) then
  begin
    if objRtti = nil then
      objRtti := Rtti.RegisterClass(PClass(obj)^);
    for p := 0 to doc.Count - 1 do
    begin
      prop := objRtti.Props.Find(doc.Names[p]);
      if prop <> nil then
        prop^.Prop.SetValue(obj, doc.Values[p]);
    end;
    result := true;
  end
  else
    result := false;
end;

procedure DocVariantToObjArray(var arr: TDocVariantData; var objArray;
  objClass: TClass);
var
  info: TRttiCustom;
  i: PtrInt;
  obj: TObjectDynArray absolute objArray;
begin
  if objClass = nil then
    exit;
  ObjArrayClear(obj);
  if (not arr.IsArray) or
     (arr.Count = 0) then
    exit;
  info := Rtti.RegisterClass(objClass);
  SetLength(obj, arr.Count);
  for i := 0 to arr.Count - 1 do
  begin
    obj[i] := info.ClassNewInstance;
    DocVariantToObject(_Safe(arr.Values[i])^, obj[i], info);
  end;
end;

function ObjectDefaultToVariant(aClass: TClass;
  aOptions: TDocVariantOptions): variant;
var
  tempvoid: TObject;
  json: RawUtf8;
begin
  VarClear(result);
  tempvoid := Rtti.RegisterClass(aClass).ClassNewInstance;
  try
    json := ObjectToJson(tempvoid, [woDontStoreDefault]);
    PDocVariantData(@result)^.InitJsonInPlace(pointer(json), aOptions);
  finally
    tempvoid.Free;
  end;
end;


{$ifdef HASITERATORS}

{ TDocVariantEnumeratorState }

procedure TDocVariantEnumeratorState.Void;
begin
  After := nil;
  Curr := nil;
end;

procedure TDocVariantEnumeratorState.Init(Values: PVariantArray; Count: PtrUInt);
begin
  if Count = 0 then
    Void
  else
  begin
    Curr := pointer(Values);
    After := @Values[Count];
    dec(Curr);
  end;
end;

function TDocVariantEnumeratorState.MoveNext: Boolean;
begin
   inc(Curr);
   result := PtrUInt(Curr) < PtrUInt(After); // Void = nil+1<nil = false
end;

{ TDocVariantFieldsEnumerator }

function TDocVariantFieldsEnumerator.GetCurrent: TDocVariantFields;
begin
  result.Name := Name;
  result.Value := State.Curr;
end;

function TDocVariantFieldsEnumerator.MoveNext: Boolean;
begin
  result := State.MoveNext;
  if result and
     Assigned(Name) then
    inc(Name);
end;

function TDocVariantFieldsEnumerator.GetEnumerator: TDocVariantFieldsEnumerator;
begin
  result := self;
end;

{ TDocVariantFieldNamesEnumerator }

function TDocVariantFieldNamesEnumerator.MoveNext: Boolean;
begin
  inc(Curr);
  result := PtrUInt(Curr) < PtrUInt(After);
end;

function TDocVariantFieldNamesEnumerator.GetEnumerator: TDocVariantFieldNamesEnumerator;
begin
  result := self;
end;

{ TDocVariantItemsEnumerator }

function TDocVariantItemsEnumerator.MoveNext: Boolean;
begin
   result := State.MoveNext;
end;

function TDocVariantItemsEnumerator.GetEnumerator: TDocVariantItemsEnumerator;
begin
  result := self;
end;

{ TDocVariantObjectsEnumerator }

function TDocVariantObjectsEnumerator.MoveNext: Boolean;
var
  vt: cardinal;
  vd: PVarData; // inlined while not DocVariant.IsOfType() + Value := _Safe()
begin
  repeat
    inc(State.Curr);
    vd := pointer(State.Curr);
    if PtrUInt(vd) >= PtrUInt(State.After) then
      break;
    repeat
      vt := vd^.VType;
      if vt = DocVariantVType then
      begin
        Value := pointer(vd);
        result := true;
        exit;
      end;
      if vt <> varVariantByRef then
        break;
      vd := vd^.VPointer;
    until false;
  until false;
  result := false;
end;

function TDocVariantObjectsEnumerator.GetEnumerator: TDocVariantObjectsEnumerator;
begin
  result := self;
end;

{$endif HASITERATORS}


{ TDocVariantData }

function TDocVariantData.GetValueIndex(const aName: RawUtf8): integer;
begin
  result := GetValueIndex(Pointer(aName), Length(aName),
    dvoNameCaseSensitive in VOptions);
end;

function TDocVariantData.GetCapacity: integer;
begin
  result := length(VValue);
end;

function TDocVariant.InternNames: TRawUtf8Interning;
begin
  result := fInternNames;
  if result = nil then
    result := CreateInternNames;
end;

function TDocVariant.CreateInternNames: TRawUtf8Interning;
begin
  GlobalLock;
  try
    if fInternNames = nil then
      fInternNames := TRawUtf8Interning.Create;
  finally
    GlobalUnLock;
  end;
  result := fInternNames;
end;

function TDocVariant.InternValues: TRawUtf8Interning;
begin
  result := fInternValues;
  if fInternValues = nil then
    result := CreateInternValues;
end;

function TDocVariant.CreateInternValues: TRawUtf8Interning;
begin
  GlobalLock;
  try
    if fInternValues = nil then
      fInternValues := TRawUtf8Interning.Create;
  finally
    GlobalUnLock;
  end;
  result := fInternValues;
end;

procedure TDocVariantData.SetOptions(const opt: TDocVariantOptions);
begin
  VOptions := (opt - [dvoIsArray, dvoIsObject]) +
              (VOptions * [dvoIsArray, dvoIsObject]);
end;

procedure TDocVariantData.Init(aOptions: TDocVariantOptions;
  aKind: TDocVariantKind);
var
  opt: cardinal; // Intel has latency on word-level memory access
begin
  aOptions := aOptions - [dvoIsArray, dvoIsObject];
  if aKind <> dvUndefined then
    if aKind = dvArray then
      include(aOptions, dvoIsArray)
    else
      include(aOptions, dvoIsObject);
  opt := word(aOptions);
  TRttiVarData(self).VType := DocVariantVType + opt shl 16;
  pointer(VName) := nil; // reset garbage content -> explicit Clear call needed
  pointer(VValue) := nil;
  VCount := 0;
end;

procedure TDocVariantData.Init(aModel: TDocVariantModel; aKind: TDocVariantKind);
begin
  Init(JSON_[aModel], aKind);
end;

procedure TDocVariantData.InitFast(aKind: TDocVariantKind);
begin
  TVarData(self) := DV_FAST[aKind];
end;

procedure TDocVariantData.InitFast(InitialCapacity: integer;
  aKind: TDocVariantKind);
begin
  TVarData(self) := DV_FAST[aKind];
  if aKind = dvObject then
    SetLength(VName, InitialCapacity);
  SetLength(VValue, InitialCapacity);
end;

procedure TDocVariantData.InitObject(const NameValuePairs: array of const;
  aOptions: TDocVariantOptions);
begin
  Init(aOptions, dvObject);
  AddNameValuesToObject(NameValuePairs);
end;

procedure TDocVariantData.InitObject(const NameValuePairs: array of const;
  Model: TDocVariantModel);
begin
  Init(Model, dvObject);
  AddNameValuesToObject(NameValuePairs);
end;

procedure TDocVariantData.AddNameValuesToObject(
  const NameValuePairs: array of const);
var
  n, arg: PtrInt;
  tmp: variant;
begin
  n := length(NameValuePairs);
  if (n = 0) or
     (n and 1 = 1) or
     IsArray then
    exit; // nothing to add
  include(VOptions, dvoIsObject);
  n := n shr 1;
  if length(VValue) < VCount + n then
  begin
    SetLength(VValue, VCount + n);
    SetLength(VName, VCount + n);
  end;
  for arg := 0 to n - 1 do
  begin
    VarRecToUtf8(NameValuePairs[arg * 2], VName[arg + VCount]);
    if dvoInternNames in VOptions then
      DocVariantType.InternNames.UniqueText(VName[arg + VCount]);
    if dvoValueCopiedByReference in VOptions then
      VarRecToVariant(NameValuePairs[arg * 2 + 1], VValue[arg + VCount])
    else
    begin
      VarRecToVariant(NameValuePairs[arg * 2 + 1], tmp);
      SetVariantByValue(tmp, VValue[arg + VCount]);
    end;
    if dvoInternValues in VOptions then
      DocVariantType.InternValues.UniqueVariant(VValue[arg + VCount]);
  end;
  inc(VCount, n);
end;

{$ifndef PUREMORMOT2}
procedure TDocVariantData.AddOrUpdateNameValuesToObject(
  const NameValuePairs: array of const);
begin
  Update(NameValuePairs);
end;
{$endif PUREMORMOT2}

procedure TDocVariantData.Update(const NameValuePairs: array of const);
var
  n, arg: PtrInt;
  nam: RawUtf8;
  val: Variant;
begin
  n := length(NameValuePairs);
  if (n = 0) or
     (n and 1 = 1) or
     IsArray then
    exit; // nothing to add
  for arg := 0 to (n shr 1) - 1 do
  begin
    VarRecToUtf8(NameValuePairs[arg * 2], nam);
    VarRecToVariant(NameValuePairs[arg * 2 + 1], val);
    AddOrUpdateValue(nam, val)
  end;
end;

procedure TDocVariantData.AddOrUpdateObject(const NewValues: variant;
  OnlyAddMissing: boolean; RecursiveUpdate: boolean);
var
  n, idx: PtrInt;
  new: PDocVariantData;
  wasAdded: boolean;
begin
  new := _Safe(NewValues);
  if not IsArray and
     not new^.IsArray then
    for n := 0 to new^.Count - 1 do
    begin
      idx := AddOrUpdateValue(
        new^.names[n], new^.Values[n], @wasAdded, OnlyAddMissing);
      if RecursiveUpdate and
         not wasAdded then
        TDocVariantData(Values[idx]).AddOrUpdateObject(
          new^.Values[n], OnlyAddMissing, true);
    end;
end;

procedure TDocVariantData.InitArray(const aItems: array of const;
  aOptions: TDocVariantOptions);
var
  arg: PtrInt;
  tmp: variant;
begin
  Init(aOptions, dvArray);
  if high(aItems) >= 0 then
  begin
    VCount := length(aItems);
    SetLength(VValue, VCount);
    if dvoValueCopiedByReference in VOptions then
      for arg := 0 to high(aItems) do
        VarRecToVariant(aItems[arg], VValue[arg])
    else
      for arg := 0 to high(aItems) do
      begin
        VarRecToVariant(aItems[arg], tmp);
        SetVariantByValue(tmp, VValue[arg]);
      end;
  end;
end;

procedure TDocVariantData.InitArray(const aItems: array of const;
  aModel: TDocVariantModel);
begin
  InitArray(aItems, JSON_[aModel]);
end;

procedure TDocVariantData.InitArrayFromVariants(const aItems: TVariantDynArray;
  aOptions: TDocVariantOptions; aItemsCopiedByReference: boolean);
begin
  if aItems = nil then
    TRttiVarData(self).VType := varNull
  else
  begin
    Init(aOptions, dvArray);
    VCount := length(aItems);
    VValue := aItems; // fast by-reference copy of VValue[]
    if not aItemsCopiedByReference then
      InitCopy(variant(self), aOptions);
  end;
end;

procedure TDocVariantData.InitArrayFromObjArray(const ObjArray;
  aOptions: TDocVariantOptions; aWriterOptions: TTextWriterWriteObjectOptions);
var
  ndx: PtrInt;
  aItems: TObjectDynArray absolute ObjArray;
begin
  if aItems = nil then
    TRttiVarData(self).VType := varNull
  else
  begin
    Init(aOptions, dvArray);
    VCount := length(aItems);
    SetLength(VValue, VCount);
    for ndx := 0 to VCount - 1 do
      ObjectToVariant(aItems[ndx], VValue[ndx], aWriterOptions);
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TRawUtf8DynArray;
  aOptions: TDocVariantOptions);
var
  ndx: PtrInt;
  v: PRttiVarData;
begin
  if aItems = nil then
    TRttiVarData(self).VType := varNull
  else
  begin
    Init(aOptions, dvArray);
    VCount := length(aItems);
    SetLength(VValue, VCount);
    v := pointer(VValue);
    for ndx := 0 to VCount - 1 do
    begin
      v^.VType := varString;
      RawUtf8(v^.Data.VAny) := aItems[ndx];
      inc(v);
    end;
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TIntegerDynArray;
  aOptions: TDocVariantOptions);
var
  ndx: PtrInt;
  v: PRttiVarData;
begin
  if aItems = nil then
    VType := varNull
  else
  begin
    Init(aOptions, dvArray);
    VCount := length(aItems);
    SetLength(VValue, VCount);
    v := pointer(VValue);
    for ndx := 0 to VCount - 1 do
    begin
      v^.VType := varInteger;
      v^.Data.VInteger := aItems[ndx];
      inc(v);
    end;
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TInt64DynArray;
  aOptions: TDocVariantOptions);
var
  ndx: PtrInt;
  v: PRttiVarData;
begin
  if aItems = nil then
    VType := varNull
  else
  begin
    Init(aOptions, dvArray);
    VCount := length(aItems);
    SetLength(VValue, VCount);
    v := pointer(VValue);
    for ndx := 0 to VCount - 1 do
    begin
      v^.VType := varInt64;
      v^.Data.VInt64 := aItems[ndx];
      inc(v);
    end;
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TDoubleDynArray;
  aOptions: TDocVariantOptions);
var
  ndx: PtrInt;
begin
  if aItems = nil then
    VType := varNull
  else
  begin
    Init(aOptions, dvArray);
    VCount := length(aItems);
    SetLength(VValue, VCount);
    for ndx := 0 to VCount - 1 do
      VValue[ndx] := aItems[ndx];
  end;
end;

procedure TDocVariantData.InitArrayFrom(var aItems; ArrayInfo: PRttiInfo;
  aOptions: TDocVariantOptions; ItemsCount: PInteger);
var
  da: TDynArray;
begin
  da.Init(ArrayInfo, aItems, ItemsCount);
  InitArrayFrom(da, aOptions);
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TDynArray;
  aOptions: TDocVariantOptions);
var
  n: integer;
  P: PByte;
  v: PVarData;
  item: TRttiCustom;
  json: RawUtf8;
begin
  Init(aOptions, dvArray);
  n := aItems.Count;
  item := aItems.Info.ArrayRtti;
  if (n = 0) or
     (item = nil) then
    exit;
  if item.Kind in rkRecordOrDynArrayTypes then
  begin
    // use temporary JSON conversion for complex nested content
    aItems.SaveToJson(json);
    if (json <> '') and
       (json[1] = '[') and
       (PCardinal(@PByteArray(json)[1])^ <> JSON_BASE64_MAGIC_QUOTE_C) then
      // should be serialized as a true array
      InitJsonInPlace(pointer(json), aOptions);
  end
  else
  begin
    // handle array of simple types
    VCount := n;
    SetLength(VValue, n);
    P := aItems.Value^;
    v := pointer(VValue);
    repeat
      inc(P, item.ValueToVariant(P, v^));
      inc(v);
      dec(n);
    until n = 0;
  end;
end;

procedure TDocVariantData.InitObjectFromVariants(const aNames: TRawUtf8DynArray;
  const aValues: TVariantDynArray; aOptions: TDocVariantOptions);
begin
  if (aNames = nil) or
     (length(aNames) <> length(aValues)) then
    VType := varNull
  else
  begin
    Init(aOptions, dvObject);
    VCount := length(aNames);
    VName := aNames; // fast by-reference copy of VName[] and VValue[]
    VValue := aValues;
  end;
end;

procedure TDocVariantData.InitObjectFromPath(const aPath: RawUtf8;
  const aValue: variant; aOptions: TDocVariantOptions);
var
  right: RawUtf8;
begin
  if aPath = '' then
    VType := varNull
  else
  begin
    Init(aOptions, dvObject);
    VCount := 1;
    SetLength(VName, 1);
    SetLength(VValue, 1);
    Split(aPath, '.', VName[0], right);
    if right = '' then
      VValue[0] := aValue
    else
      PDocVariantData(@VValue[0])^.InitObjectFromPath(right, aValue, aOptions);
  end;
end;

function TDocVariantData.InitJsonInPlace(Json: PUtf8Char;
  aOptions: TDocVariantOptions; aEndOfObject: PUtf8Char): PUtf8Char;
var
  EndOfObject: AnsiChar;
  Name: PUtf8Char;
  NameLen, cap: integer;
  intnames, intvalues: TRawUtf8Interning;
begin
  Init(aOptions);
  result := nil;
  if Json = nil then
    exit;
  if dvoInternValues in VOptions then
    intvalues := DocVariantType.InternValues
  else
    intvalues := nil;
  while (Json^ <= ' ') and
        (Json^ <> #0) do
    inc(Json);
  case Json^ of
    '[':
      begin
        repeat
          inc(Json);
          if Json^ = #0 then
            exit;
        until Json^ > ' ';
        include(VOptions, dvoIsArray);
        if Json^ = ']' then
          // void but valid input array
          Json := GotoNextNotSpace(Json + 1)
        else
        begin
          if dvoJsonParseDoNotGuessCount in VOptions then
            cap := 8 // with a lot of nested objects -> best to ignore
          else
          begin
            // guess of the Json array items count - prefetch up to 64KB of input
            cap := abs(JsonArrayCount(Json, Json + JSON_PREFETCH));
            if cap = 0 then
              exit; // invalid content
          end;
          SetLength(VValue, cap);
          repeat
            if VCount = cap then
            begin
              // grow if our initial guess was aborted due to huge input
              cap := NextGrow(cap);
              SetLength(VValue, cap);
            end;
            // unserialize the next item
            GetJsonToAnyVariant(VValue[VCount], Json, @EndOfObject, @VOptions,
              {double=}false{is set from VOptions});
            if Json = nil then
            begin
              VCount := 0;
              exit; // invalid input
            end;
            if intvalues <> nil then
              intvalues.UniqueVariant(VValue[VCount]);
            inc(VCount);
          until EndOfObject = ']';
          // no SetLength(VValue,VCount) if NextGrow() was used on huge input
        end;
      end;
    '{':
      begin
        repeat
          inc(Json);
          if Json^ = #0 then
            exit;
        until Json^ > ' ';
        include(VOptions, dvoIsObject);
        if Json^ = '}' then
          // void but valid input object
          Json := GotoNextNotSpace(Json + 1)
        else
        begin
          if dvoJsonParseDoNotGuessCount in VOptions then
            cap := 4 // with a lot of nested documents -> best to ignore
          else
          begin
            // guess of the Json object properties count - prefetch up to 64KB
            cap := JsonObjectPropCount(Json, Json + JSON_PREFETCH);
            if cap = 0 then
              exit // invalid content (was <0 if early abort)
            else if cap < 0 then
            begin // nested or huge objects are evil -> no more guess
              cap := -cap;
              include(VOptions, dvoJsonParseDoNotGuessCount);
            end;
          end;
          if dvoInternNames in VOptions then
            intnames := DocVariantType.InternNames
          else
            intnames := nil;
          SetLength(VValue, cap);
          SetLength(VName, cap);
          repeat
            // see http://docs.mongodb.org/manual/reference/mongodb-extended-Json
            Name := GetJsonPropName(Json, @NameLen);
            if Name = nil then
              exit;
            if VCount = cap then
            begin
              // grow if our initial guess was aborted due to huge input
              cap := NextGrow(cap);
              SetLength(VValue, cap);
              SetLength(VName, cap);
            end;
            if intnames <> nil then
              intnames.Unique(VName[VCount], Name, NameLen)
            else
              FastSetString(VName[VCount], Name, NameLen);
            GetJsonToAnyVariant(VValue[VCount], Json, @EndOfObject, @VOptions,
              {double=}false{is set from VOptions});
            if Json = nil then
              if EndOfObject = '}' then // valid object end
                Json := @NULCHAR
              else
                exit; // invalid input
            if intvalues <> nil then
              intvalues.UniqueVariant(VValue[VCount]);
            inc(VCount);
          until EndOfObject = '}';
          // no SetLength(VValue/VNAme,VCount) if NextGrow() on huge input
        end;
      end;
    'n', 'N':
      begin
        if IdemPChar(Json + 1, 'ULL') then
        begin
          include(VOptions, dvoIsObject);
          result := GotoNextNotSpace(Json + 4);
        end;
        exit;
      end;
  else
    exit;
  end;
  while (Json^ <= ' ') and
        (Json^ <> #0) do
    inc(Json);
  if aEndOfObject <> nil then
    aEndOfObject^ := Json^;
  if Json^ <> #0 then
    repeat
      inc(Json)
    until (Json^ = #0) or
          (Json^ > ' ');
  result := Json; // indicates successfully parsed
end;

function TDocVariantData.InitJson(const Json: RawUtf8;
  aOptions: TDocVariantOptions): boolean;
var
  tmp: TSynTempBuffer;
begin
  if Json = '' then
    result := false
  else
  begin
    tmp.Init(Json);
    try
      result := InitJsonInPlace(tmp.buf, aOptions) <> nil;
    finally
      tmp.Done;
    end;
  end;
end;

function TDocVariantData.InitJson(const Json: RawUtf8; aModel: TDocVariantModel): boolean;
begin
  result := InitJson(Json, JSON_[aModel]);
end;

procedure TDocVariantData.InitCsv(aCsv: PUtf8Char; aOptions: TDocVariantOptions;
  NameValueSep, ItemSep: AnsiChar; DoTrim: boolean);
var
  n, v: RawUtf8;
  val: variant;
begin
  Init(aOptions, dvObject);
  while aCsv <> nil do
  begin
    GetNextItem(aCsv, NameValueSep, n);
    if ItemSep = #10 then
      GetNextItemTrimedCRLF(aCsv, v)
    else
      GetNextItem(aCsv, ItemSep, v);
    if DoTrim then
      v := TrimU(v);
    if n = '' then
      break;
    RawUtf8ToVariant(v, val);
    AddValue(n, val);
  end;
end;

procedure TDocVariantData.InitCsv(const aCsv: RawUtf8; aOptions: TDocVariantOptions;
  NameValueSep, ItemSep: AnsiChar; DoTrim: boolean);
begin
  InitCsv(pointer(aCsv), aOptions, NameValueSep, ItemSep, DoTrim);
end;

procedure TDocVariantData.InitCopy(const SourceDocVariant: variant;
  aOptions: TDocVariantOptions);
var
  ndx: PtrInt;
  vt: cardinal;
  Source: PDocVariantData;
  SourceVValue: TVariantDynArray;
  Handler: TCustomVariantType;
  v: PVarData;
begin
  with TVarData(SourceDocVariant) do
    if cardinal(VType) = varVariantByRef then
      Source := VPointer
    else
      Source := @SourceDocVariant;
  if cardinal(Source^.VType) <> DocVariantVType then
    raise EDocVariant.CreateUtf8(
      'No TDocVariant for InitCopy(%)', [ord(Source.VType)]);
  SourceVValue := Source^.VValue; // local fast per-reference copy
  if Source <> @self then
  begin
    VType := Source^.VType;
    VCount := Source^.VCount;
    pointer(VName) := nil;  // avoid GPF
    pointer(VValue) := nil;
    aOptions := aOptions - [dvoIsArray, dvoIsObject]; // may not be same as Source
    if Source^.IsArray then
      include(aOptions, dvoIsArray)
    else if Source^.IsObject then
    begin
      include(aOptions, dvoIsObject);
      SetLength(VName, VCount);
      for ndx := 0 to VCount - 1 do
        VName[ndx] := Source^.VName[ndx]; // manual copy is needed
      if (dvoInternNames in aOptions) and
         not (dvoInternNames in Source^.Options) then
        with DocVariantType.InternNames do
          for ndx := 0 to VCount - 1 do
            UniqueText(VName[ndx]);
    end;
    VOptions := aOptions;
  end
  else
  begin
    SetOptions(aOptions);
    VariantDynArrayClear(VValue); // full copy of all values
  end;
  if VCount > 0 then
  begin
    SetLength(VValue, VCount);
    for ndx := 0 to VCount - 1 do
    begin
      v := @SourceVValue[ndx];
      repeat
        vt := v^.VType;
        if vt <> varVariantByRef then
          break;
        v := v^.VPointer;
      until false;
      if vt < varFirstCustom then
        // simple string/number types copy
        VValue[ndx] := variant(v^)
      else if vt = DocVariantVType then
        // direct recursive copy for TDocVariant
        TDocVariantData(VValue[ndx]).InitCopy(variant(v^), VOptions)
      else if FindCustomVariantType(vt, Handler) then
        if Handler.InheritsFrom(TSynInvokeableVariantType) then
          TSynInvokeableVariantType(Handler).CopyByValue(
            TVarData(VValue[ndx]), v^)
        else
          Handler.Copy(
            TVarData(VValue[ndx]), v^, false)
      else
        VValue[ndx] := variant(v^); // default copy
    end;
    if dvoInternValues in VOptions then
      with DocVariantType.InternValues do
        for ndx := 0 to VCount - 1 do
          UniqueVariant(VValue[ndx]);
  end;
  VariantDynArrayClear(SourceVValue);
end;

procedure TDocVariantData.ClearFast;
begin
  PInteger(@VType)^ := 0; // clear VType and VOptions
  RawUtf8DynArrayClear(VName);
  VariantDynArrayClear(VValue);
  VCount := 0;
end;

procedure TDocVariantData.Clear;
begin
  if cardinal(VType) = DocVariantVType then
    ClearFast
  else
    VarClear(variant(self));
end;

procedure TDocVariantData.Reset;
var
  backup: TDocVariantOptions;
begin
  if VCount = 0 then
    exit;
  backup := VOptions - [dvoIsArray, dvoIsObject];
  ClearFast;
  VType := DocVariantVType;
  VOptions := backup;
end;

procedure TDocVariantData.FillZero;
var
  ndx: PtrInt;
begin
  for ndx := 0 to VCount - 1 do
    mormot.core.variants.FillZero(VValue[ndx]);
  Reset;
end;

function TDocVariantData.GetModel(out model: TDocVariantModel): boolean;
var
  opt: TDocVariantOptions;
  ndx: PtrInt;
begin
  opt := VOptions - [dvoIsArray, dvoIsObject, dvoJsonParseDoNotGuessCount];
  ndx := WordScanIndex(@JSON_, ord(high(TDocVariantModel)) + 1, word(opt));
  if ndx < 0 then
    result := false
  else
  begin
    model := TDocVariantModel(ndx);
    result := true;
  end;
end;

procedure TDocVariantData.SetCount(aCount: integer);
begin
  VCount := aCount;
end;

function TDocVariantData.Compare(const Another: TDocVariantData;
  CaseInsensitive: boolean): integer;
var
  j, n: PtrInt;
  nameCmp: TUtf8Compare;
begin
  // first validate the type: as { or [ in JSON
  nameCmp := nil;
  if IsArray then
  begin
    if not Another.IsArray then
    begin
      result := -1;
      exit;
    end;
  end
  else if IsObject then
    if not Another.IsObject then
    begin
      result := 1;
      exit;
    end
    else
      nameCmp := StrCompByCase[not (dvoNameCaseSensitive in VOptions)];
  // compare as many in-order content as possible
  n := Another.VCount;
  if VCount < n then
    n := VCount;
  for j := 0 to n - 1 do
  begin
    if Assigned(nameCmp) then
    begin // each name should match
      result := nameCmp(pointer(VName[j]), pointer(Another.VName[j]));
      if result <> 0 then
        exit;
    end;
    result := FastVarDataComp(@VValue[j], @Another.VValue[j], CaseInsensitive);
    if result <> 0 then // each value should match
      exit;
  end;
  // all content did match -> difference is now about the document count
  result := VCount - Another.VCount;
end;

function TDocVariantData.CompareObject(const ObjFields: array of RawUtf8;
  const Another: TDocVariantData; CaseInsensitive: boolean): integer;
var
  f: PtrInt;
  ndx: integer;
  v1, v2: PVarData;
begin
  if IsObject then
    if Another.IsObject then
    begin
      // compare Object, Object by specified fields
      if high(ObjFields) < 0 then
      begin
        result := Compare(Another, CaseInsensitive);
        exit;
      end;
      for f := 0 to high(ObjFields) do
      begin
        v1 := GetVarData(ObjFields[f], nil, @ndx);
        if (cardinal(ndx) < cardinal(Another.VCount)) and
           (StrCompByCase[not (dvoNameCaseSensitive in VOptions)](
              pointer(ObjFields[f]), pointer(Another.VName[ndx])) = 0) then
          v2 := @Another.VValue[ndx] // ObjFields are likely at the same position
        else
          v2 := Another.GetVarData(ObjFields[f]); // full safe field name lookup
        result := FastVarDataComp(v1, v2, CaseInsensitive);
        if result <> 0 then // each value should match
          exit;
      end;
      // all fields did match -> difference is now about the document size
      result := VCount - Another.VCount;
    end
    else
      result := 1   // Object, not Object
  else if Another.IsObject then
    result := -1  // not Object, Object
  else
    result := 0;  // not Object, not Object
end;

function TDocVariantData.Equals(const Another: TDocVariantData;
  CaseInsensitive: boolean): boolean;
begin
  result := Compare(Another, CaseInsensitive) = 0;
end;

function TDocVariantData.InternalAdd(
  const aName: RawUtf8; aIndex: integer): integer;
var
  len: integer;
begin
  // validate consistent add/insert
  if aName <> '' then
  begin
    if IsArray then
      raise EDocVariant.CreateUtf8(
        'Add: Unexpected [%] object property in an array', [aName]);
    if not IsObject then
    begin
      VType := DocVariantVType; // may not be set yet
      include(VOptions, dvoIsObject);
    end;
  end
  else
  begin
    if IsObject then
      raise EDocVariant.Create('Add: Unexpected array item in an object');
    if not IsArray then
    begin
      VType := DocVariantVType; // may not be set yet
      include(VOptions, dvoIsArray);
    end;
  end;
  // grow up memory if needed
  len := length(VValue);
  if VCount >= len then
  begin
    len := NextGrow(VCount);
    SetLength(VValue, len);
  end;
  result := VCount;
  inc(VCount);
  if cardinal(aIndex) < cardinal(result) then
  begin
    // reserve space for the inserted new item
    dec(result, aIndex);
    MoveFast(VValue[aIndex], VValue[aIndex + 1], result * SizeOf(variant));
    PInteger(@VValue[aIndex])^ := varEmpty; // avoid GPF
    if aName <> '' then
    begin
      if Length(VName) <> len then
        SetLength(VName, len);
      MoveFast(VName[aIndex], VName[aIndex + 1], result * SizeOf(pointer));
      PPointer(@VName[aIndex])^ := nil;
    end;
    result := aIndex;
  end;
  if aName <> '' then
  begin
    // store the object field name
    if Length(VName) <> len then
      SetLength(VName, len);
    if dvoInternNames in VOptions then
      DocVariantType.InternNames.Unique(VName[result], aName)
    else
      VName[result] := aName;
  end;
end;

{$ifdef HASITERATORS}

function TDocVariantData.GetEnumerator: TDocVariantFieldsEnumerator;
begin
  result.State.Init(pointer(Values), VCount);
  if IsObject then
  begin
    result.Name := pointer(Names);
    dec(result.Name);
  end
  else
    result.Name := nil;
end;

function TDocVariantData.Items: TDocVariantItemsEnumerator;
begin
  if IsObject then
    result{%H-}.State.Void
  else
    result.State.Init(pointer(Values), VCount);
end;

function TDocVariantData.Objects: TDocVariantObjectsEnumerator;
begin
  if IsObject then
    result{%H-}.State.Void
  else
    result.State.Init(pointer(Values), VCount);
end;

function TDocVariantData.Fields: TDocVariantFieldsEnumerator;
begin
  if IsArray then
    result{%H-}.State.Void
  else
    result := GetEnumerator;
end;

function TDocVariantData.FieldNames: TDocVariantFieldNamesEnumerator;
begin
  if IsArray or
     (VCount = 0) then
  begin
    result.Curr := nil;
    result.After := nil;
  end
  else
  begin
    result.Curr := pointer(Names);
    result.After := @Names[VCount];
    dec(result.Curr);
  end;
end;

function TDocVariantData.FieldValues: TDocVariantItemsEnumerator;
begin
  if IsArray then
    result{%H-}.State.Void
  else
    result.State.Init(pointer(Values), VCount);
end;

{$endif HASITERATORS}

procedure TDocVariantData.SetCapacity(aValue: integer);
begin
  if IsObject then
    SetLength(VName, aValue);
  SetLength(VValue, aValue);
end;

function TDocVariantData.AddValue(const aName: RawUtf8; const aValue: variant;
  aValueOwned: boolean; aIndex: integer): integer;
begin
  if dvoCheckForDuplicatedNames in VOptions then
  begin
    result := GetValueIndex(aName);
    if result >= 0 then
      raise EDocVariant.CreateUtf8('AddValue: Duplicated [%] name', [aName]);
  end;
  result := InternalAdd(aName, aIndex);
  if aValueOwned then
    VValue[result] := aValue
  else
    SetVariantByValue(aValue, VValue[result]);
  if dvoInternValues in VOptions then
    DocVariantType.InternValues.UniqueVariant(VValue[result]);
end;

function TDocVariantData.AddValue(aName: PUtf8Char; aNameLen: integer;
  const aValue: variant; aValueOwned: boolean; aIndex: integer): integer;
var
  tmp: RawUtf8;
begin
  FastSetString(tmp, aName, aNameLen);
  result := AddValue(tmp, aValue, aValueOwned, aIndex);
end;

function TDocVariantData.AddValueFromText(const aName, aValue: RawUtf8;
  DoUpdate, AllowVarDouble: boolean): integer;
begin
  if aName = '' then
  begin
    result := -1;
    exit;
  end;
  result := GetValueIndex(aName);
  if not DoUpdate and
     (dvoCheckForDuplicatedNames in VOptions) and
     (result >= 0) then
    raise EDocVariant.CreateUtf8(
      'AddValueFromText: Duplicated [%] name', [aName]);
  if result < 0 then
    result := InternalAdd(aName);
  VarClear(VValue[result]);
  if not GetNumericVariantFromJson(pointer(aValue),
          TVarData(VValue[result]), AllowVarDouble) then
    if dvoInternValues in VOptions then
      DocVariantType.InternValues.UniqueVariant(VValue[result], aValue)
    else
      RawUtf8ToVariant(aValue, VValue[result]);
end;

procedure TDocVariantData.AddByPath(const aSource: TDocVariantData;
  const aPaths: array of RawUtf8);
var
  p, added: PtrInt;
  v: TVarData;
begin
  if (aSource.Count = 0) or
     (not aSource.IsObject) or
     IsArray then
    exit;
  for p := 0 to High(aPaths) do
  begin
    DocVariantType.Lookup(v, TVarData(aSource), pointer(aPaths[p]));
    if cardinal(v.VType) < varNull then
      continue; // path not found
    added := InternalAdd(aPaths[p]);
    PVarData(@VValue[added])^ := v;
    if dvoInternValues in VOptions then
      DocVariantType.InternValues.UniqueVariant(VValue[added]);
  end;
end;

procedure TDocVariantData.AddFrom(const aDocVariant: Variant);
var
  src: PDocVariantData;
  ndx: PtrInt;
begin
  src := _Safe(aDocVariant);
  if src^.Count = 0 then
    exit; // nothing to add
  if src^.IsArray then
    // add array items
    if IsObject then
      // types should match
      exit
    else
      for ndx := 0 to src^.Count - 1 do
        AddItem(src^.VValue[ndx])
  else
    // add object items
    if IsArray then
      // types should match
      exit
    else
      for ndx := 0 to src^.Count - 1 do
        AddValue(src^.VName[ndx], src^.VValue[ndx]);
end;

procedure TDocVariantData.AddOrUpdateFrom(const aDocVariant: Variant;
  aOnlyAddMissing: boolean);
var
  src: PDocVariantData;
  ndx: PtrInt;
begin
  src := _Safe(aDocVariant, dvObject);
  for ndx := 0 to src^.Count - 1 do
    AddOrUpdateValue(
      src^.VName[ndx], src^.VValue[ndx], nil, aOnlyAddMissing);
end;

function TDocVariantData.AddItem(const aValue: variant; aIndex: integer): integer;
begin
  result := InternalAdd('', aIndex);
  SetVariantByValue(aValue, VValue[result]);
  if dvoInternValues in VOptions then
    DocVariantType.InternValues.UniqueVariant(VValue[result]);
end;

function TDocVariantData.AddItemFromText(const aValue: RawUtf8;
  AllowVarDouble: boolean; aIndex: integer): integer;
begin
  result := InternalAdd('', aIndex);
  if not GetNumericVariantFromJson(pointer(aValue),
           TVarData(VValue[result]), AllowVarDouble) then
    if dvoInternValues in VOptions then
      DocVariantType.InternValues.UniqueVariant(VValue[result], aValue)
    else
      RawUtf8ToVariant(aValue, VValue[result]);
end;

function TDocVariantData.AddItemText(
  const aValue: RawUtf8; aIndex: integer): integer;
begin
  result := InternalAdd('', aIndex);
  if dvoInternValues in VOptions then
    DocVariantType.InternValues.UniqueVariant(VValue[result], aValue)
  else
    RawUtf8ToVariant(aValue, VValue[result]);
end;

procedure TDocVariantData.AddItems(const aValue: array of const);
var
  ndx, added: PtrInt;
begin
  for ndx := 0 to high(aValue) do
  begin
    added := InternalAdd('');
    VarRecToVariant(aValue[ndx], VValue[added]);
    if dvoInternValues in VOptions then
      DocVariantType.InternValues.UniqueVariant(VValue[added]);
  end;
end;

function TDocVariantData.SearchItemByProp(const aPropName, aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean): integer;
var
  ndx: PtrInt;
begin
  if IsObject then
  begin
    result := GetValueIndex(aPropName);
    if (result >= 0) and
       VariantEquals(VValue[result], aPropValue, aPropValueCaseSensitive) then
      exit;
  end
  else if IsArray then
    for result := 0 to VCount - 1 do
      with _Safe(VValue[result])^ do
        if IsObject then
        begin
          ndx := GetValueIndex(aPropName);
          if (ndx >= 0) and
             VariantEquals(VValue[ndx], aPropValue, aPropValueCaseSensitive) then
            exit;
        end;
  result := -1;
end;

function TDocVariantData.SearchItemByProp(const aPropNameFmt: RawUtf8;
  const aPropNameArgs: array of const; const aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean): integer;
var
  name: RawUtf8;
begin
  FormatUtf8(aPropNameFmt, aPropNameArgs, name);
  result := SearchItemByProp(name, aPropValue, aPropValueCaseSensitive);
end;

function TDocVariantData.SearchItemByValue(const aValue: Variant;
  CaseInsensitive: boolean; StartIndex: PtrInt): PtrInt;
begin
  for result := StartIndex to VCount - 1 do
    if FastVarDataComp(@VValue[result], @aValue, CaseInsensitive) = 0 then
      exit;
  result := -1;
end;

type
  TQuickSortDocVariant = object
    names: PPointerArray;
    values: PVariantArray;
    nameCompare: TUtf8Compare;
    valueCompare: TVariantCompare;
    valueComparer: TVariantComparer;
    reversed: PtrInt;
    procedure SortByName(L, R: PtrInt);
    procedure SortByValue(L, R: PtrInt);
  end;

procedure TQuickSortDocVariant.SortByName(L, R: PtrInt);
var
  I, J, P: PtrInt;
  pivot: pointer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := names[P];
        while nameCompare(names[I], pivot) * reversed < 0 do
          inc(I);
        while nameCompare(names[J], pivot) * reversed > 0 do
          dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            ExchgPointer(@names[I], @names[J]);
            ExchgVariant(@values[I], @values[J]);
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          SortByName(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          SortByName(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TQuickSortDocVariant.SortByValue(L, R: PtrInt);
var
  I, J, P: PtrInt;
  pivot: PVariant;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := @values[P];
        if Assigned(valueCompare) then
        begin // called from SortByValue
          while valueCompare(values[I], pivot^) * reversed < 0 do
            inc(I);
          while valueCompare(values[J], pivot^) * reversed > 0 do
            dec(J);
        end
        else
        begin // called from SortByRow
          while valueComparer(values[I], pivot^) * reversed < 0 do
            inc(I);
          while valueComparer(values[J], pivot^) * reversed > 0 do
            dec(J);
        end;
        if I <= J then
        begin
          if I <> J then
          begin
            if names <> nil then
              ExchgPointer(@names[I], @names[J]);
            ExchgVariant(@values[I], @values[J]);
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          SortByValue(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          SortByValue(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDocVariantData.SortByName(
  SortCompare: TUtf8Compare; SortCompareReversed: boolean);
var
  qs: TQuickSortDocVariant;
begin
  if (not IsObject) or
     (VCount <= 0) then
    exit;
  if Assigned(SortCompare) then
    qs.nameCompare := SortCompare
  else
    qs.nameCompare := @StrIComp;
  qs.names := pointer(VName);
  qs.values := pointer(VValue);
  if SortCompareReversed then
    qs.reversed := -1
  else
    qs.reversed := 1;
  qs.SortByName(0, VCount - 1);
end;

procedure TDocVariantData.SortByValue(SortCompare: TVariantCompare;
  SortCompareReversed: boolean);
var
  qs: TQuickSortDocVariant;
begin
  if VCount <= 0 then
    exit;
  if Assigned(SortCompare) then
    qs.valueCompare := SortCompare
  else
    qs.valueCompare := @VariantCompare;
  qs.valueComparer := nil;
  qs.names := pointer(VName);
  qs.values := pointer(VValue);
  if SortCompareReversed then
    qs.reversed := -1
  else
    qs.reversed := 1;
  qs.SortByValue(0, VCount - 1);
end;

procedure TDocVariantData.SortByRow(const SortComparer: TVariantComparer;
  SortComparerReversed: boolean);
var
  qs: TQuickSortDocVariant;
begin
  if (VCount <= 0) or
     not Assigned(SortComparer) then
    exit;
  qs.valueCompare := nil;
  qs.valueComparer := SortComparer;
  qs.names := pointer(VName);
  qs.values := pointer(VValue);
  if SortComparerReversed then
    qs.reversed := -1
  else
    qs.reversed := 1;
  qs.SortByValue(0, VCount - 1);
end;

type
  TQuickSortByFieldLookup = array[0..3] of PVariant;
  PQuickSortByFieldLookup = ^TQuickSortByFieldLookup;
  {$ifdef USERECORDWITHMETHODS}
  TQuickSortDocVariantValuesByField = record
  {$else}
  TQuickSortDocVariantValuesByField = object
  {$endif USERECORDWITHMETHODS}
    Lookup: array of TQuickSortByFieldLookup;
    Compare: TVariantCompare;
    CompareField: TVariantCompareField;
    Fields: PRawUtf8Array;
    P: PtrInt;
    Pivot: PQuickSortByFieldLookup;
    Doc: PDocVariantData;
    TempExch: TQuickSortByFieldLookup;
    Reverse: boolean;
    Depth: integer; // = high(Lookup)
    procedure Init(const aPropNames: array of RawUtf8;
      aNameSortedCompare: TUtf8Compare);
    function DoComp(Value: PQuickSortByFieldLookup): PtrInt;
      {$ifndef CPUX86} inline; {$endif}
    procedure Sort(L, R: PtrInt);
  end;

procedure TQuickSortDocVariantValuesByField.Init(
  const aPropNames: array of RawUtf8; aNameSortedCompare: TUtf8Compare);
var
  namecomp: TUtf8Compare;
  v: pointer;
  row, f: PtrInt;
  rowdata: PDocVariantData;
  ndx: integer;
begin
  Depth := high(aPropNames);
  if (Depth < 0) or
     (Depth > high(TQuickSortByFieldLookup)) then
    raise EDocVariant.CreateUtf8('TDocVariantData.SortByFields(%)', [Depth]);
  // resolve GetPVariantByName(aPropNames) once into Lookup[]
  SetLength(Lookup, Doc^.VCount);
  if Assigned(aNameSortedCompare) then // just like GetVarData() searches names
    namecomp := aNameSortedCompare
  else
    namecomp := StrCompByCase[not (dvoNameCaseSensitive in Doc^.VOptions)];
  for f := 0 to Depth do
  begin
    if aPropNames[f] = '' then
      raise EDocVariant.CreateUtf8('TDocVariantData.SortByFields(%=void)', [f]);
    ndx := -1;
    for row := 0 to Doc^.VCount - 1 do
    begin
      rowdata := _Safe(Doc^.VValue[row]);
      if (cardinal(ndx) < cardinal(rowdata^.VCount)) and
         (namecomp(pointer(rowdata^.VName[ndx]), pointer(aPropNames[f])) = 0) then
        v := @rowdata^.VValue[ndx] // get the value at the (likely) same position
      else
      begin
        v := rowdata^.GetVarData(aPropNames[f], aNameSortedCompare, @ndx);
        if v = nil then
          v := @NullVarData;
      end;
      Lookup[row, f] := v;
    end;
  end;
end;

function TQuickSortDocVariantValuesByField.DoComp(
  Value: PQuickSortByFieldLookup): PtrInt;
begin
  if Assigned(Compare) then
  begin
    result := Compare(Value[0]^, Pivot[0]^);
    if (result = 0) and
       (depth > 0) then
    begin
      result := Compare(Value[1]^, Pivot[1]^);
      if (result = 0) and
         (depth > 1) then
      begin
        result := Compare(Value[2]^, Pivot[2]^);
        if (result = 0) and
           (depth > 2) then
         result := Compare(Value[3]^, Pivot[3]^);
      end;
    end;
  end
  else
  begin
    result := CompareField(Fields[0], Value[0]^, Pivot[0]^);
    if (result = 0) and
       (depth > 0) then
    begin
      result := CompareField(Fields[1], Value[1]^, Pivot[1]^);
      if (result = 0) and
         (depth > 1) then
      begin
        result := CompareField(Fields[2], Value[2]^, Pivot[2]^);
        if (result = 0) and
           (depth > 2) then
         result := CompareField(Fields[3], Value[3]^, Pivot[3]^);
      end;
    end;
  end;
  if Reverse then
    result := -result;
end;

procedure TQuickSortDocVariantValuesByField.Sort(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        Pivot := @Lookup[P];
        while DoComp(@Lookup[I]) < 0 do
          inc(I);
        while DoComp(@Lookup[J]) > 0 do
          dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            if Doc.VName <> nil then
              ExchgPointer(@Doc.VName[I], @Doc.VName[J]);
            ExchgVariant(@Doc.VValue[I], @Doc.VValue[J]);
            ExchgPointers(@Lookup[I], @Lookup[J], Depth + 1);
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          Sort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          Sort(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDocVariantData.SortArrayByField(const aItemPropName: RawUtf8;
  aValueCompare: TVariantCompare; aValueCompareReverse: boolean;
  aNameSortedCompare: TUtf8Compare);
var
  QS: TQuickSortDocVariantValuesByField;
begin
  if (VCount <= 0) or
     (aItemPropName = '') or
     not IsArray then
    exit;
  if not Assigned(aValueCompare) then
    aValueCompare := VariantCompare;
  QS.Compare := aValueCompare;
  QS.Doc := @self;
  QS.Init([aItemPropName], aNameSortedCompare);
  QS.Reverse := aValueCompareReverse;
  QS.Sort(0, VCount - 1);
end;

procedure TDocVariantData.SortArrayByFields(
  const aItemPropNames: array of RawUtf8; aValueCompare: TVariantCompare;
  const aValueCompareField: TVariantCompareField;
  aValueCompareReverse: boolean; aNameSortedCompare: TUtf8Compare);
var
  QS: TQuickSortDocVariantValuesByField;
begin
  if (VCount <= 0) or
     not IsArray then
    exit;
  if Assigned(aValueCompareField) then
  begin
    QS.Compare := nil;
    QS.Fields := @aItemPropNames[0];
    QS.CompareField := aValueCompareField;
  end
  else if Assigned(aValueCompare) then
      QS.Compare := aValueCompare
    else
      QS.Compare := VariantCompare;
  QS.Doc := @self;
  QS.Init(aItemPropNames, aNameSortedCompare);
  QS.Reverse := aValueCompareReverse;
  QS.Sort(0, VCount - 1);
end;

procedure TDocVariantData.Reverse;
begin
  if VCount <= 0 then
    exit;
  if VName <> nil then
    DynArray(TypeInfo(TRawUtf8DynArray), VName, @VCount).Reverse;
  DynArray(TypeInfo(TVariantDynArray), VValue, @VCount).Reverse;
end;

function TDocVariantData.Reduce(const aPropNames: array of RawUtf8;
  aCaseSensitive, aDoNotAddVoidProp: boolean): variant;
begin
  VarClear(result{%H-});
  Reduce(
    aPropNames, aCaseSensitive, PDocVariantData(@result)^, aDoNotAddVoidProp);
end;

procedure TDocVariantData.Reduce(const aPropNames: array of RawUtf8;
  aCaseSensitive: boolean; out result: TDocVariantData;
  aDoNotAddVoidProp: boolean);
var
  ndx, j: PtrInt;
  reduced: TDocVariantData;
begin
  TVarData(result) := DV_FAST[dvUndefined];
  if (VCount = 0) or
     (high(aPropNames) < 0) then
    exit;
  if IsObject then
  begin
    if aCaseSensitive then
    begin
      for j := 0 to high(aPropNames) do
        for ndx := 0 to VCount - 1 do
          if VName[ndx] = aPropNames[j] then
          begin
            if not aDoNotAddVoidProp or
               not VarIsVoid(VValue[ndx]) then
              result.AddValue(VName[ndx], VValue[ndx]);
            break;
          end;
    end
    else
      for j := 0 to high(aPropNames) do
        for ndx := 0 to VCount - 1 do
          if IdemPropNameU(VName[ndx], aPropNames[j]) then
          begin
            if not aDoNotAddVoidProp or
               not VarIsVoid(VValue[ndx]) then
              result.AddValue(VName[ndx], VValue[ndx]);
            break;
          end;
  end
  else if IsArray then
    for ndx := 0 to VCount - 1 do
    begin
      _Safe(VValue[ndx])^.Reduce(
        aPropNames, aCaseSensitive, reduced, aDoNotAddVoidProp);
      if reduced.IsObject then
        result.AddItem(variant(reduced));
    end;
end;

function TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  const OnReduce: TOnReducePerItem): variant;
begin
  VarClear(result{%H-});
  ReduceAsArray(aPropName, PDocVariantData(@result)^, OnReduce);
end;

procedure TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  out result: TDocVariantData; const OnReduce: TOnReducePerItem);
var
  ndx, j: PtrInt;
  item: PDocVariantData;
begin
  TVarData(result) := DV_FAST[dvArray];
  if (VCount = 0) or
     (aPropName = '') or
     not IsArray then
    exit;
  for ndx := 0 to VCount - 1 do
    if _SafeObject(VValue[ndx], item) then
    begin
      j := item^.GetValueIndex(aPropName);
      if j >= 0 then
        if not Assigned(OnReduce) or
           OnReduce(item) then
          result.AddItem(item^.VValue[j]);
    end;
end;

function TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  const OnReduce: TOnReducePerValue): variant;
begin
  VarClear(result{%H-});
  ReduceAsArray(aPropName, PDocVariantData(@result)^, OnReduce);
end;

procedure TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  out result: TDocVariantData; const OnReduce: TOnReducePerValue);
var
  ndx, j: PtrInt;
  item: PDocVariantData;
  v: PVariant;
begin
  TVarData(result) := DV_FAST[dvArray];
  if (VCount = 0) or
     (aPropName = '') or
     not IsArray then
    exit;
  for ndx := 0 to VCount - 1 do
    if _SafeObject(VValue[ndx], item) then
    begin
      j := item^.GetValueIndex(aPropName);
      if j >= 0 then
      begin
        v := @item^.VValue[j];
        if not Assigned(OnReduce) or
           OnReduce(v^) then
          result.AddItem(v^);
      end;
    end;
end;

function TDocVariantData.Rename(
  const aFromPropName, aToPropName: TRawUtf8DynArray): integer;
var
  n, p, ndx: PtrInt;
begin
  result := 0;
  n := length(aFromPropName);
  if length(aToPropName) = n then
    for p := 0 to n - 1 do
    begin
      ndx := GetValueIndex(aFromPropName[p]);
      if ndx >= 0 then
      begin
        VName[ndx] := aToPropName[p];
        inc(result);
      end;
    end;
end;

function TDocVariantData.FlattenAsNestedObject(
  const aObjectPropName: RawUtf8): boolean;
var
  ndx, len: PtrInt;
  Up: array[byte] of AnsiChar;
  nested: TDocVariantData;
begin
  // {"p.a1":5,"p.a2":"dfasdfa"} -> {"p":{"a1":5,"a2":"dfasdfa"}}
  result := false;
  if (VCount = 0) or
     (aObjectPropName = '') or
     (not IsObject) then
    exit;
  PWord(UpperCopy255(Up{%H-}, aObjectPropName))^ := ord('.'); // e.g. 'P.'
  for ndx := 0 to Count - 1 do
    if not IdemPChar(pointer(VName[ndx]), Up) then
      exit; // all fields should match "p.####"
  len := length(aObjectPropName) + 1;
  for ndx := 0 to Count - 1 do
    system.delete(VName[ndx], 1, len);
  nested := self;
  ClearFast;
  InitObject([aObjectPropName, variant(nested)]);
  result := true;
end;

function TDocVariantData.Delete(Index: PtrInt): boolean;
var
  n: PtrInt;
begin
  if cardinal(Index) >= cardinal(VCount) then
    result := false
  else
  begin
    dec(VCount);
    if VName <> nil then
    begin
      if PRefCnt(PAnsiChar(pointer(VName)) - _DAREFCNT)^ > 1 then
        VName := copy(VName); // make unique
      VName[Index] := '';
    end;
    if PRefCnt(PAnsiChar(pointer(VValue)) - _DAREFCNT)^ > 1 then
      VValue := copy(VValue); // make unique
    VarClear(VValue[Index]);
    n := VCount - Index;
    if n <> 0 then
    begin
      if VName <> nil then
      begin
        MoveFast(VName[Index + 1], VName[Index], n * SizeOf(pointer));
        PtrUInt(VName[VCount]) := 0; // avoid GPF
      end;
      MoveFast(VValue[Index + 1], VValue[Index], n * SizeOf(variant));
      TVarData(VValue[VCount]).VType := varEmpty; // avoid GPF
    end;
    result := true;
  end;
end;

function TDocVariantData.Delete(const aName: RawUtf8): boolean;
begin
  result := Delete(GetValueIndex(aName));
end;

function TDocVariantData.Delete(const aNames: array of RawUtf8): integer;
var
  n: PtrInt;
begin
  result := 0;
  for n := 0 to high(aNames) do
    inc(result, ord(Delete(aNames[n])));
end;

function TDocVariantData.DeleteByProp(const aPropName, aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean): boolean;
begin
  result := Delete(SearchItemByProp(aPropName, aPropValue, aPropValueCaseSensitive));
end;

function TDocVariantData.DeleteByValue(const aValue: Variant;
  CaseInsensitive: boolean): integer;
var
  ndx: PtrInt;
begin
  result := 0;
  if VarIsEmptyOrNull(aValue) then
  begin
    for ndx := VCount - 1 downto 0 do
      if VarDataIsEmptyOrNull(@VValue[ndx]) then
      begin
        Delete(ndx);
        inc(result);
      end;
  end
  else
    for ndx := VCount - 1 downto 0 do
      if FastVarDataComp(@VValue[ndx], @aValue, CaseInsensitive) = 0 then
      begin
        Delete(ndx);
        inc(result);
      end;
end;

function TDocVariantData.DeleteByStartName(
  aStartName: PUtf8Char; aStartNameLen: integer): integer;
var
  ndx: PtrInt;
  upname: array[byte] of AnsiChar;
begin
  result := 0;
  if aStartNameLen = 0 then
    aStartNameLen := StrLen(aStartName);
  if (VCount = 0) or
     (not IsObject) or
     (aStartNameLen = 0) then
    exit;
  UpperCopy255Buf(upname{%H-}, aStartName, aStartNameLen)^ := #0;
  for ndx := Count - 1 downto 0 do
    if IdemPChar(pointer(names[ndx]), upname) then
    begin
      Delete(ndx);
      inc(result);
    end;
end;

function TDocVariantData.IsVoid: boolean;
begin
  result := (cardinal(VType) <> DocVariantVType) or
            (VCount = 0);
end;

function FindNonVoidRawUtf8(n: PPUtf8Char; name: PUtf8Char; len: TStrLen;
  count: PtrInt): PtrInt;
var
  p: PUtf8Char;
begin
  // FPC does proper inlining in this loop
  for result := 0 to count - 1 do
  begin
    p := n^; // all VName[]<>'' so p=n^<>nil
    if (PStrLen(p - _STRLEN)^ = len) and
       CompareMemFixed(p, name, len) then
      exit
    else
      inc(n);
  end;
  result := -1;
end;

function FindNonVoidRawUtf8I(n: PPUtf8Char; name: PUtf8Char; len: TStrLen;
  count: PtrInt): PtrInt;
var
  p: PUtf8Char;
begin
  for result := 0 to count - 1 do
  begin
    p := n^; // all VName[]<>'' so p=n^<>nil
    if (PStrLen(p - _STRLEN)^ = len) and
       IdemPropNameUSameLenNotNull(p, name, len) then
      exit
    else
      inc(n);
  end;
  result := -1;
end;

function TDocVariantData.GetValueIndex(aName: PUtf8Char; aNameLen: PtrInt;
  aCaseSensitive: boolean): integer;
var
  err: integer;
begin
  if (cardinal(VType) = DocVariantVType) and
     (VCount > 0) then
    if IsArray then
    begin
      // try index integer as text, for lookup in array document
      result := GetInteger(aName, err);
      if (err <> 0) or
         (cardinal(result) >= cardinal(VCount)) then
        result := -1;
    end
    else
    // O(n) lookup for name -> efficient brute force sub-functions
    if aCaseSensitive then
      result := FindNonVoidRawUtf8(pointer(VName), aName, aNameLen, VCount)
    else
      result := FindNonVoidRawUtf8I(pointer(VName), aName, aNameLen, VCount)
  else
    result := -1;
end;

function TDocVariantData.GetValueOrRaiseException(
  const aName: RawUtf8): variant;
begin
  RetrieveValueOrRaiseException(pointer(aName), length(aName),
    dvoNameCaseSensitive in VOptions, result, false);
end;

function TDocVariantData.GetValueOrDefault(const aName: RawUtf8;
  const aDefault: variant): variant;
var
  ndx: PtrInt;
begin
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) then
    result := aDefault
  else
  begin
    ndx := GetValueIndex(aName);
    if ndx >= 0 then
      result := VValue[ndx]
    else
      result := aDefault;
  end;
end;

function TDocVariantData.GetValueOrNull(const aName: RawUtf8): variant;
var
  ndx: PtrInt;
begin
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) then
    SetVariantNull(result{%H-})
  else
  begin
    ndx := GetValueIndex(aName);
    if ndx >= 0 then
      result := VValue[ndx]
    else
      SetVariantNull(result);
  end;
end;

function TDocVariantData.GetValueOrEmpty(const aName: RawUtf8): variant;
var
  ndx: PtrInt;
begin
  VarClear(result{%H-});
  if (cardinal(VType) = DocVariantVType) and
     IsObject then
  begin
    ndx := GetValueIndex(aName);
    if ndx >= 0 then
      result := VValue[ndx];
  end;
end;

function TDocVariantData.GetAsBoolean(const aName: RawUtf8; out aValue: boolean;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToBoolean(PVariant(found)^, aValue)
end;

function TDocVariantData.GetAsInteger(const aName: RawUtf8; out aValue: integer;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToInteger(PVariant(found)^, aValue);
end;

function TDocVariantData.GetAsInt64(const aName: RawUtf8; out aValue: Int64;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToInt64(PVariant(found)^, aValue)
end;

function TDocVariantData.GetAsDouble(const aName: RawUtf8; out aValue: double;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToDouble(PVariant(found)^, aValue);
end;

function TDocVariantData.GetAsRawUtf8(const aName: RawUtf8; out aValue: RawUtf8;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
  wasString: boolean;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
  begin
    if cardinal(found^.VType) > varNull then
      // avoid default VariantToUtf8(null)='null'
      VariantToUtf8(PVariant(found)^, aValue, wasString);
    result := true;
  end;
end;

function TDocVariantData.GetValueEnumerate(const aName: RawUtf8;
  aTypeInfo: PRttiInfo; out aValue; aDeleteFoundEntry: boolean): boolean;
var
  text: RawUtf8;
  ndx, ord: integer;
begin
  result := false;
  ndx := GetValueIndex(aName);
  if ndx < 0 then
    exit;
  VariantToUtf8(Values[ndx], text);
  ord := GetEnumNameValue(aTypeInfo, text, true);
  if ord < 0 then
    exit;
  byte(aValue) := ord;
  if aDeleteFoundEntry then
    Delete(ndx);
  result := true;
end;

function TDocVariantData.GetAsDocVariant(const aName: RawUtf8;
  out aValue: PDocVariantData; aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  result := (found <> nil) and
            _Safe(PVariant(found)^, aValue);
end;

function TDocVariantData.GetAsArray(const aName: RawUtf8;
  out aArray: PDocVariantData; aSortedCompare: TUtf8Compare): boolean;
begin
  result := GetAsDocVariant(aName, aArray, aSortedCompare) and
            aArray^.IsArray and
            (aArray^.Count > 0);
end;

function TDocVariantData.GetAsObject(const aName: RawUtf8;
  out aObject: PDocVariantData; aSortedCompare: TUtf8Compare): boolean;
begin
  result := GetAsDocVariant(aName, aObject, aSortedCompare) and
            aObject^.IsObject and
            (aObject^.Count > 0);
end;

function TDocVariantData.GetAsDocVariantSafe(const aName: RawUtf8;
  aSortedCompare: TUtf8Compare): PDocVariantData;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := @DocVariantDataFake
  else
    result := _Safe(PVariant(found)^);
end;

function TDocVariantData.GetAsPVariant(const aName: RawUtf8;
  out aValue: PVariant; aSortedCompare: TUtf8Compare): boolean;
begin
  aValue := pointer(GetVarData(aName, aSortedCompare));
  result := aValue <> nil;
end;

function TDocVariantData.GetAsPVariant(
  aName: PUtf8Char; aNameLen: PtrInt): PVariant;
var
  ndx: PtrInt;
begin
  ndx := GetValueIndex(aName, aNameLen, dvoNameCaseSensitive in VOptions);
  if ndx >= 0 then
    result := @VValue[ndx]
  else
    result := nil;
end;

function TDocVariantData.GetVarData(const aName: RawUtf8;
  aSortedCompare: TUtf8Compare; aFoundIndex: PInteger): PVarData;
var
  ndx: PtrInt;
begin
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) or
     (VCount = 0) or
     (aName = '') then
  begin
    result := nil;
    if aFoundIndex <> nil then
      aFoundIndex^ := -1;
  end
  else
  begin
    if Assigned(aSortedCompare) then
      if @aSortedCompare = @StrComp then
        // use our branchless asm for StrComp()
        ndx := FastFindPUtf8CharSorted(
          pointer(VName), VCount - 1, pointer(aName))
      else
        ndx := FastFindPUtf8CharSorted(
          pointer(VName), VCount - 1, pointer(aName), aSortedCompare)
    else if dvoNameCaseSensitive in VOptions then
      ndx := FindNonVoidRawUtf8(
        pointer(VName), pointer(aName), length(aName), VCount)
    else
      ndx := FindNonVoidRawUtf8I(
        pointer(VName), pointer(aName), length(aName), VCount);
    if aFoundIndex <> nil then
      aFoundIndex^ := ndx;
    if ndx >= 0 then
      result := @VValue[ndx]
    else
      result := nil;
  end;
end;

function TDocVariantData.GetVarData(const aName: RawUtf8; var aValue: TVarData;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
  begin
    aValue := found^;
    result := true;
  end;
end;

function TDocVariantData.GetValueByPath(const aPath: RawUtf8): variant;
var
  Dest: TVarData;
begin
  VarClear(result{%H-});
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) then
    exit;
  DocVariantType.Lookup(Dest, TVarData(self), pointer(aPath));
  if cardinal(Dest.VType) >= varNull then
    result := variant(Dest); // copy
end;

function TDocVariantData.GetValueByPath(const aPath: RawUtf8;
  out aValue: variant): boolean;
var
  Dest: TVarData;
begin
  result := false;
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) then
    exit;
  DocVariantType.Lookup(Dest, TVarData(self), pointer(aPath));
  if Dest.VType = varEmpty then
    exit;
  aValue := variant(Dest); // copy
  result := true;
end;

function TDocVariantData.GetPVariantByPath(const aPath: RawUtf8): PVariant;
var
  p: PUtf8Char;
  item: RawUtf8;
  par: PVariant;
begin
  result := nil;
  if (cardinal(VType) <> DocVariantVType) or
     (aPath = '') or
     (not IsObject) or
     (count = 0) then
    exit;
  par := @self;
  p := pointer(aPath);
  repeat
    GetNextItem(p, '.', item);
    if _Safe(par^).GetAsPVariant(item, result) then
      par := result
    else
    begin
      result := nil;
      exit;
    end;
  until p = nil;
  // if we reached here, we have par=result=found item
end;

function TDocVariantData.GetDocVariantByPath(const aPath: RawUtf8;
  out aValue: PDocVariantData): boolean;
var
  v: PVariant;
begin
  v := GetPVariantByPath(aPath);
  result := (v <> nil) and
            _Safe(v^, aValue);
end;

function TDocVariantData.GetValueByPath(
  const aDocVariantPath: array of RawUtf8): variant;
var
  found, res: PVarData;
  vt: cardinal;
  P: integer;
begin
  VarClear(result{%H-});
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) or
     (high(aDocVariantPath) < 0) then
    exit;
  found := @self;
  P := 0;
  repeat
    found := PDocVariantData(found).GetVarData(aDocVariantPath[P]);
    if found = nil then
      exit;
    if P = high(aDocVariantPath) then
      break; // we found the item!
    inc(P);
    // if we reached here, we should try for the next scope within Dest
    repeat
      vt := found^.VType;
      if vt <> varVariantByRef then
        break;
      found := found^.VPointer;
    until false;
    if vt = VType then
      continue;
    exit;
  until false;
  res := found;
  while cardinal(res^.VType) = varVariantByRef do
    res := res^.VPointer;
  if (cardinal(res^.VType) = VType) and
     (PDocVariantData(res)^.VCount = 0) then
    // return void TDocVariant as null
    TVarData(result).VType := varNull
  else
    // copy found value
    result := PVariant(found)^;
end;

function TDocVariantData.GetItemByProp(const aPropName, aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean; var Dest: variant; DestByRef: boolean): boolean;
var
  ndx: integer;
begin
  result := false;
  if not IsArray then
    exit;
  ndx := SearchItemByProp(aPropName, aPropValue, aPropValueCaseSensitive);
  if ndx < 0 then
    exit;
  RetrieveValueOrRaiseException(ndx, Dest, DestByRef);
  result := true;
end;

function TDocVariantData.GetDocVariantByProp(
  const aPropName, aPropValue: RawUtf8; aPropValueCaseSensitive: boolean;
  out Dest: PDocVariantData): boolean;
var
  ndx: PtrInt;
begin
  result := false;
  if not IsArray then
    exit;
  ndx := SearchItemByProp(aPropName, aPropValue, aPropValueCaseSensitive);
  if ndx >= 0 then
    result := _Safe(VValue[ndx], Dest);
end;

function TDocVariantData.GetJsonByStartName(const aStartName: RawUtf8): RawUtf8;
var
  Up: array[byte] of AnsiChar;
  temp: TTextWriterStackBuffer;
  ndx: PtrInt;
  W: TTextWriter;
begin
  if (not IsObject) or
     (VCount = 0) then
  begin
    result := NULL_STR_VAR;
    exit;
  end;
  UpperCopy255(Up, aStartName)^ := #0;
  W := DefaultTextWriterSerializer.CreateOwnedStream(temp) as TTextWriter;
  try
    W.Add('{');
    for ndx := 0 to VCount - 1 do
      if IdemPChar(Pointer(VName[ndx]), Up) then
      begin
        if (dvoSerializeAsExtendedJson in VOptions) and
           JsonPropNameValid(pointer(VName[ndx])) then
        begin
          W.AddNoJsonEscape(pointer(VName[ndx]), Length(VName[ndx]));
        end
        else
        begin
          W.Add('"');
          W.AddJsonEscape(pointer(VName[ndx]));
          W.Add('"');
        end;
        W.Add(':');
        W.AddVariant(VValue[ndx], twJsonEscape);
        W.AddComma;
      end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TDocVariantData.GetValuesByStartName(const aStartName: RawUtf8;
  TrimLeftStartName: boolean): variant;
var
  Up: array[byte] of AnsiChar;
  ndx: PtrInt;
  name: RawUtf8;
begin
  if aStartName = '' then
  begin
    result := Variant(self);
    exit;
  end;
  if (not IsObject) or
     (VCount = 0) then
  begin
    SetVariantNull(result{%H-});
    exit;
  end;
  TDocVariant.NewFast(result);
  UpperCopy255(Up{%H-}, aStartName)^ := #0;
  for ndx := 0 to VCount - 1 do
    if IdemPChar(Pointer(VName[ndx]), Up) then
    begin
      name := VName[ndx];
      if TrimLeftStartName then
        system.delete(name, 1, length(aStartName));
      TDocVariantData(result).AddValue(name, VValue[ndx]);
    end;
end;

procedure TDocVariantData.SetValueOrRaiseException(Index: integer;
  const NewValue: variant);
begin
  if cardinal(Index) >= cardinal(VCount) then
    raise EDocVariant.CreateUtf8(
      'Out of range Values[%] (count=%)', [Index, VCount])
  else
    VValue[Index] := NewValue;
end;

procedure TDocVariantData.RetrieveNameOrRaiseException(
  Index: integer; var Dest: RawUtf8);
begin
  if (cardinal(Index) >= cardinal(VCount)) or
     (VName = nil) then
    if dvoReturnNullForUnknownProperty in VOptions then
      Dest := ''
    else
      raise EDocVariant.CreateUtf8(
        'Out of range Names[%] (count=%)', [Index, VCount])
  else
    Dest := VName[Index];
end;

procedure TDocVariantData.RetrieveValueOrRaiseException(Index: integer;
  var Dest: variant; DestByRef: boolean);
var
  Source: PVariant;
begin
  if cardinal(Index) >= cardinal(VCount) then
    if dvoReturnNullForUnknownProperty in VOptions then
      SetVariantNull(Dest)
    else
      raise EDocVariant.CreateUtf8(
        'Out of range Values[%] (count=%)', [Index, VCount])
  else if DestByRef then
    SetVariantByRef(VValue[Index], Dest)
  else
  begin
    Source := @VValue[Index];
    while PVarData(Source)^.VType = varVariantByRef do
      Source := PVarData(Source)^.VPointer;
    Dest := Source^;
  end;
end;

function TDocVariantData.RetrieveValueOrRaiseException(
  aName: PUtf8Char; aNameLen: integer; aCaseSensitive: boolean;
  var Dest: variant; DestByRef: boolean): boolean;
var
  ndx: integer;
begin
  ndx := GetValueIndex(aName, aNameLen, aCaseSensitive);
  if ndx < 0 then
    if dvoReturnNullForUnknownProperty in VOptions then
      SetVariantNull(Dest)
    else
      raise EDocVariant.CreateUtf8('[%] property not found', [aName])
  else
    RetrieveValueOrRaiseException(ndx, Dest, DestByRef);
  result := ndx >= 0;
end;

function TDocVariantData.GetValueOrItem(const aNameOrIndex: variant): variant;
var
  wasString: boolean;
  Name: RawUtf8;
begin
  if IsArray then
    // fast index lookup e.g. for Value[1]
    RetrieveValueOrRaiseException(
      VariantToIntegerDef(aNameOrIndex, -1), result, true)
  else
  begin
    // by name lookup e.g. for Value['abc']
    VariantToUtf8(aNameOrIndex, Name, wasString);
    if wasString then
      RetrieveValueOrRaiseException(pointer(Name), length(Name),
        dvoNameCaseSensitive in VOptions, result, true)
    else
      RetrieveValueOrRaiseException(
        GetIntegerDef(pointer(Name), -1), result, true);
  end;
end;

procedure TDocVariantData.SetValueOrItem(const aNameOrIndex, aValue: variant);
var
  wasString: boolean;
  ndx: integer;
  Name: RawUtf8;
begin
  if IsArray then
    // fast index lookup e.g. for Value[1]
    SetValueOrRaiseException(VariantToIntegerDef(aNameOrIndex, -1), aValue)
  else
  begin
    // by name lookup e.g. for Value['abc']
    VariantToUtf8(aNameOrIndex, Name, wasString);
    if wasString then
    begin
      ndx := GetValueIndex(Name);
      if ndx < 0 then
        ndx := InternalAdd(Name);
      SetVariantByValue(aValue, VValue[ndx]);
      if dvoInternValues in VOptions then
        DocVariantType.InternValues.UniqueVariant(VValue[ndx]);
    end
    else
      SetValueOrRaiseException(
        VariantToIntegerDef(aNameOrIndex, -1), aValue);
  end;
end;

function TDocVariantData.AddOrUpdateValue(const aName: RawUtf8;
  const aValue: variant; wasAdded: PBoolean; OnlyAddMissing: boolean): integer;
begin
  if IsArray then
    raise EDocVariant.CreateUtf8(
      'AddOrUpdateValue("%") on an array', [aName]);
  result := GetValueIndex(aName);
  if result < 0 then
  begin
    result := InternalAdd(aName);
    if wasAdded <> nil then
      wasAdded^ := true;
  end
  else
  begin
    if wasAdded <> nil then
      wasAdded^ := false;
    if OnlyAddMissing then
      exit;
  end;
  SetVariantByValue(aValue, VValue[result]);
  if dvoInternValues in VOptions then
    DocVariantType.InternValues.UniqueVariant(VValue[result]);
end;

function TDocVariantData.ToJson(const Prefix, Suffix: RawUtf8;
  Format: TTextWriterJsonFormat): RawUtf8;
var
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  if (cardinal(VType) <> DocVariantVType) and
     (VType > varNull) then
  begin
    result := ''; // null -> 'null'
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    W.AddString(Prefix);
    DocVariantType.ToJson(W, variant(self));
    W.AddString(Suffix);
    W.SetText(result, Format);
  finally
    W.Free;
  end;
end;

procedure TDocVariantData.SaveToJsonFile(const FileName: TFileName);
var
  F: TFileStream;
  W: TTextWriter;
begin
  if cardinal(VType) <> DocVariantVType then
    exit;
  F := TFileStream.Create(FileName, fmCreate);
  try
    W := TTextWriter.Create(F, 65536);
    try
      DocVariantType.ToJson(W, variant(self));
      W.FlushFinal;
    finally
      W.Free;
    end;
  finally
    F.Free;
  end;
end;

function TDocVariantData.ToNonExpandedJson: RawUtf8;
var
  field: TRawUtf8DynArray;
  fieldCount, r, f: PtrInt;
  W: TTextWriter;
  row: PDocVariantData;
  temp: TTextWriterStackBuffer;
begin
  if not IsArray then
  begin
    result := '';
    exit;
  end;
  if VCount = 0 then
  begin
    result := '[]';
    exit;
  end;
  fieldCount := 0;
  with _Safe(VValue[0])^ do
    if IsObject then
    begin
      field := VName;
      fieldCount := VCount;
    end;
  if fieldCount = 0 then
    raise EDocVariant.Create('ToNonExpandedJson: Value[0] is not an object');
  W := DefaultTextWriterSerializer.CreateOwnedStream(temp) as TTextWriter;
  try
    W.Add('{"fieldCount":%,"rowCount":%,"values":[', [fieldCount, VCount]);
    for f := 0 to fieldCount - 1 do
    begin
      W.Add('"');
      W.AddJsonEscape(pointer(field[f]));
      W.Add('"', ',');
    end;
    for r := 0 to VCount - 1 do
    begin
      row := _Safe(VValue[r]);
      if (r > 0) and
         ((not row^.IsObject) or
          (row^.VCount <> fieldCount)) then
        raise EDocVariant.CreateUtf8(
          'ToNonExpandedJson: Value[%] not expected object', [r]);
      for f := 0 to fieldCount - 1 do
        if (r > 0) and
           not IdemPropNameU(row^.VName[f], field[f]) then
          raise EDocVariant.CreateUtf8(
            'ToNonExpandedJson: Value[%] field=% expected=%',
            [r, row^.VName[f], field[f]])
        else
        begin
          W.AddVariant(row^.VValue[f], twJsonEscape);
          W.AddComma;
        end;
    end;
    W.CancelLastComma;
    W.Add(']', '}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TDocVariantData.ToRawUtf8DynArray(out Result: TRawUtf8DynArray);
var
  ndx: PtrInt;
  wasString: boolean;
begin
  if IsObject then
    raise EDocVariant.Create('ToRawUtf8DynArray expects a dvArray');
  if IsArray then
  begin
    SetLength(Result, VCount);
    for ndx := 0 to VCount - 1 do
      VariantToUtf8(VValue[ndx], Result[ndx], wasString);
  end;
end;

function TDocVariantData.ToRawUtf8DynArray: TRawUtf8DynArray;
begin
  ToRawUtf8DynArray(result);
end;

function TDocVariantData.ToCsv(const Separator: RawUtf8): RawUtf8;
var
  tmp: TRawUtf8DynArray; // fast enough in practice
begin
  ToRawUtf8DynArray(tmp);
  result := RawUtf8ArrayToCsv(tmp, Separator);
end;

procedure TDocVariantData.ToTextPairsVar(out result: RawUtf8;
  const NameValueSep, ItemSep: RawUtf8; escape: TTextWriterKind);
var
  ndx: PtrInt;
  temp: TTextWriterStackBuffer;
begin
  if IsArray then
    raise EDocVariant.Create('ToTextPairs expects a dvObject');
  if (VCount > 0) and
     IsObject then
    with DefaultTextWriterSerializer.CreateOwnedStream(temp) do
    try
      ndx := 0;
      repeat
        AddString(VName[ndx]);
        AddString(NameValueSep);
        AddVariant(VValue[ndx], escape);
        inc(ndx);
        if ndx = VCount then
          break;
        AddString(ItemSep);
      until false;
      SetText(result);
    finally
      Free;
    end;
end;

function TDocVariantData.ToTextPairs(const NameValueSep: RawUtf8;
  const ItemSep: RawUtf8; Escape: TTextWriterKind): RawUtf8;
begin
  ToTextPairsVar(result, NameValueSep, ItemSep, Escape);
end;

procedure TDocVariantData.ToArrayOfConst(out Result: TTVarRecDynArray);
var
  ndx: PtrInt;
begin
  if IsObject then
    raise EDocVariant.Create('ToArrayOfConst expects a dvArray');
  if IsArray then
  begin
    SetLength(Result, VCount);
    for ndx := 0 to VCount - 1 do
    begin
      Result[ndx].VType := vtVariant;
      Result[ndx].VVariant := @VValue[ndx];
    end;
  end;
end;

function TDocVariantData.ToArrayOfConst: TTVarRecDynArray;
begin
  ToArrayOfConst(result);
end;

function TDocVariantData.ToUrlEncode(const UriRoot: RawUtf8): RawUtf8;
var
  json: RawUtf8; // temporary in-place modified buffer
begin
  VariantSaveJson(variant(self), twJsonEscape, json);
  result := UrlEncodeJsonObject(UriRoot, Pointer(json), []);
end;

function TDocVariantData.GetOrAddIndexByName(const aName: RawUtf8): integer;
begin
  result := GetValueIndex(aName);
  if result < 0 then
    result := InternalAdd(aName);
end;

function TDocVariantData.GetOrAddPVariantByName(const aName: RawUtf8): PVariant;
var
  ndx: PtrInt;
begin
  ndx := GetValueIndex(aName);
  if ndx < 0 then
    ndx := InternalAdd(aName);
  result := @VValue[ndx];
end;

function TDocVariantData.GetPVariantByName(const aName: RawUtf8): PVariant;
var
  ndx: PtrInt;
begin
  ndx := GetValueIndex(aName);
  if ndx < 0 then
    if dvoReturnNullForUnknownProperty in VOptions then
      result := @DocVariantDataFake
    else
      raise EDocVariant.CreateUtf8('[%] property not found', [aName])
  else
    result := @VValue[ndx];
end;

function TDocVariantData.GetInt64ByName(const aName: RawUtf8): Int64;
begin
  if not VariantToInt64(GetPVariantByName(aName)^, result) then
    result := 0;
end;

function TDocVariantData.GetRawUtf8ByName(const aName: RawUtf8): RawUtf8;
var
  wasString: boolean;
  v: PVariant;
begin
  v := GetPVariantByName(aName);
  if PVarData(v)^.VType <= varNull then // default VariantToUtf8(null)='null'
    result := ''
  else
    VariantToUtf8(v^, result, wasString);
end;

function TDocVariantData.GetStringByName(const aName: RawUtf8): string;
begin
  result := VariantToString(GetPVariantByName(aName)^);
end;

procedure TDocVariantData.SetInt64ByName(const aName: RawUtf8;
  const aValue: Int64);
begin
  GetOrAddPVariantByName(aName)^ := aValue;
end;

procedure TDocVariantData.SetRawUtf8ByName(const aName, aValue: RawUtf8);
begin
  RawUtf8ToVariant(aValue, GetOrAddPVariantByName(aName)^);
end;

procedure TDocVariantData.SetStringByName(const aName: RawUtf8;
  const aValue: string);
begin
  RawUtf8ToVariant(StringToUtf8(aValue), GetOrAddPVariantByName(aName)^);
end;

function TDocVariantData.GetBooleanByName(const aName: RawUtf8): boolean;
begin
  if not VariantToBoolean(GetPVariantByName(aName)^, result) then
    result := false;
end;

procedure TDocVariantData.SetBooleanByName(const aName: RawUtf8;
  aValue: boolean);
begin
  GetOrAddPVariantByName(aName)^ := aValue;
end;

function TDocVariantData.GetDoubleByName(const aName: RawUtf8): Double;
begin
  if not VariantToDouble(GetPVariantByName(aName)^, result) then
    result := 0;
end;

procedure TDocVariantData.SetDoubleByName(const aName: RawUtf8;
  const aValue: Double);
begin
  GetOrAddPVariantByName(aName)^ := aValue;
end;

function TDocVariantData.GetDocVariantExistingByName(const aName: RawUtf8;
  aNotMatchingKind: TDocVariantKind): PDocVariantData;
begin
  result := GetAsDocVariantSafe(aName);
  if result^.GetKind = aNotMatchingKind then
    result := @DocVariantDataFake;
end;

function TDocVariantData.GetDocVariantOrAddByName(const aName: RawUtf8;
  aKind: TDocVariantKind): PDocVariantData;
var
  ndx: PtrInt;
begin
  ndx := GetOrAddIndexByName(aName);
  result := _Safe(VValue[ndx]);
  if result^.Kind <> aKind then
  begin
    result := @VValue[ndx];
    VarClear(PVariant(result)^);
    result^.InitFast(aKind);
  end;
end;

function TDocVariantData.GetObjectExistingByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantExistingByName(aName, dvArray);
end;

function TDocVariantData.GetObjectOrAddByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantOrAddByName(aName, dvObject);
end;

function TDocVariantData.GetArrayExistingByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantExistingByName(aName, dvObject);
end;

function TDocVariantData.GetArrayOrAddByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantOrAddByName(aName, dvArray);
end;

function TDocVariantData.GetAsDocVariantByIndex(
  aIndex: integer): PDocVariantData;
begin
  if cardinal(aIndex) < cardinal(VCount) then
    result := _Safe(VValue[aIndex])
  else if dvoReturnNullForUnknownProperty in VOptions then
    result := @DocVariantDataFake
  else
    raise EDocVariant.CreateUtf8(
      'Out of range _[%] (count=%)', [aIndex, VCount]);
end;

function _Obj(const NameValuePairs: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitObject(NameValuePairs, Options);
end;

function _Arr(const Items: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArray(Items, Options);
end;

procedure _ObjAddProp(const Name: RawUtf8; const Value: variant;
  var Obj: variant);
var
  o: PDocVariantData;
begin
  if _SafeObject(Obj, o) then
  begin
    // append new names/values to existing object
    if o <> @Obj then
      // ensure not stored by reference
      TVarData(Obj) := PVarData(o)^;
    o^.AddOrUpdateValue(Name, Value);
  end
  else
  begin
    // create new object
    VarClear(Obj);
    TDocVariantData(Obj).InitObject([Name, Value], JSON_FAST);
  end
end;

procedure _ObjAddProps(const NameValuePairs: array of const;
  var Obj: variant);
var
  o: PDocVariantData;
begin
  if _SafeObject(Obj, o) then
  begin
    // append new names/values to existing object
    if o <> @Obj then
      // ensure not stored by reference
      TVarData(Obj) := PVarData(o)^;
    o^.AddNameValuesToObject(NameValuePairs);
  end
  else
  begin
    // create new object
    VarClear(Obj);
    TDocVariantData(Obj).InitObject(NameValuePairs, JSON_FAST);
  end
end;

procedure _ObjAddProps(const Document: variant; var Obj: variant);
var
  ndx: PtrInt;
  d, o: PDocVariantData;
begin
  o := _Safe(Obj);
  if _SafeObject(Document, d) then
    if not o.IsObject then
      Obj := Document
    else
      for ndx := 0 to d^.VCount - 1 do
        o^.AddOrUpdateValue(d^.VName[ndx], d^.VValue[ndx]);
end;

function _ObjFast(const NameValuePairs: array of const): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitObject(NameValuePairs, JSON_FAST);
end;

function _ObjFast(aObject: TObject;
  aOptions: TTextWriterWriteObjectOptions): variant;
begin
  ObjectToVariant(aObject, result, aOptions);
end;

function _ArrFast(const Items: array of const): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArray(Items, JSON_FAST);
end;

function _Json(const Json: RawUtf8; Options: TDocVariantOptions): variant;
begin
  _Json(Json, result, Options);
end;

function _JsonFast(const Json: RawUtf8): variant;
begin
  _Json(Json, result, JSON_FAST);
end;

function _JsonFastFloat(const Json: RawUtf8): variant;
begin
  _Json(Json, result, JSON_FAST_FLOAT);
end;

function _JsonFastExt(const Json: RawUtf8): variant;
begin
  _Json(Json, result, JSON_FAST_EXTENDED);
end;

function _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions): variant;
begin
  _JsonFmt(Format, Args, Params, Options, result);
end;

procedure _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions; out Result: variant);
var
  temp: RawUtf8;
begin
  temp := FormatUtf8(Format, Args, Params, true);
  if TDocVariantData(Result).InitJsonInPlace(pointer(temp), Options) = nil then
    TDocVariantData(Result).ClearFast;
end;

function _JsonFastFmt(const Format: RawUtf8;
  const Args, Params: array of const): variant;
begin
  _JsonFmt(Format, Args, Params, JSON_FAST, result);
end;

function _Json(const Json: RawUtf8; var Value: variant;
  Options: TDocVariantOptions): boolean;
begin
  VarClear(Value);
  if not TDocVariantData(Value).InitJson(Json, Options) then
  begin
    TDocVariantData(Value).ClearFast;
    result := false;
  end
  else
    result := true;
end;

procedure _Unique(var DocVariant: variant);
begin
  // TDocVariantData(DocVariant): InitCopy() will check the DocVariant type
  TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_[mDefault]);
end;

procedure _UniqueFast(var DocVariant: variant);
begin
  // TDocVariantData(DocVariant): InitCopy() will check the DocVariant type
  TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_[mFast]);
end;

function _Copy(const DocVariant: variant): variant;
begin
  result := TDocVariant.NewUnique(DocVariant, JSON_[mDefault]);
end;

function _CopyFast(const DocVariant: variant): variant;
begin
  result := TDocVariant.NewUnique(DocVariant, JSON_[mFast]);
end;

function _ByRef(const DocVariant: variant; Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result) := _Safe(DocVariant)^; // fast byref copy
  TDocVariantData(result).SetOptions(Options);
end;

procedure _ByRef(const DocVariant: variant; out Dest: variant;
  Options: TDocVariantOptions);
begin
  TDocVariantData(Dest) := _Safe(DocVariant)^; // fast byref copy
  TDocVariantData(Dest).SetOptions(Options);
end;



{ TLockedDocVariant }

constructor TLockedDocVariant.Create;
begin
  Create(JSON_FAST);
end;

constructor TLockedDocVariant.Create(options: TDocVariantModel);
begin
  Create(JSON_[options]);
end;

constructor TLockedDocVariant.Create(options: TDocVariantOptions);
begin
  fLock := TAutoLocker.Create;
  fValue.Init(options);
end;

destructor TLockedDocVariant.Destroy;
begin
  inherited;
  fLock.Free;
end;

function TLockedDocVariant.Exists(const Name: RawUtf8;
  out Value: Variant): boolean;
var
  i: PtrInt;
begin
  fLock.Enter;
  try
    i := fValue.GetValueIndex(Name);
    if i < 0 then
      result := false
    else
    begin
      Value := fValue.Values[i];
      result := true;
    end;
  finally
    fLock.Leave;
  end;
end;

function TLockedDocVariant.ExistsOrLock(const Name: RawUtf8;
  out Value: Variant): boolean;
var
  i: PtrInt;
begin
  result := true;
  fLock.Enter;
  try
    i := fValue.GetValueIndex(Name);
    if i < 0 then
      result := false
    else
      Value := fValue.Values[i];
  finally
    if result then
      fLock.Leave;
  end;
end;

procedure TLockedDocVariant.ReplaceAndUnlock(
  const Name: RawUtf8; const Value: Variant; out LocalValue: Variant);
begin
  // caller made fLock.Enter
  try
    SetValue(Name, Value);
    LocalValue := Value;
  finally
    fLock.Leave;
  end;
end;

function TLockedDocVariant.AddExistingPropOrLock(const Name: RawUtf8;
  var Obj: variant): boolean;
var
  i: PtrInt;
begin
  result := true;
  fLock.Enter;
  try
    i := fValue.GetValueIndex(Name);
    if i < 0 then
      result := false
    else
      _ObjAddProps([Name, fValue.Values[i]], Obj);
  finally
    if result then
      fLock.Leave;
  end;
end;

procedure TLockedDocVariant.AddNewPropAndUnlock(const Name: RawUtf8;
  const Value: variant; var Obj: variant);
begin
  // caller made fLock.Enter
  try
    SetValue(Name, Value);
    _ObjAddProps([Name, Value], Obj);
  finally
    fLock.Leave;
  end;
end;

function TLockedDocVariant.AddExistingProp(const Name: RawUtf8;
  var Obj: variant): boolean;
var
  i: PtrInt;
begin
  result := true;
  fLock.Enter;
  try
    i := fValue.GetValueIndex(Name);
    if i < 0 then
      result := false
    else
      _ObjAddProps([Name,
      fValue.Values[i]], Obj);
  finally
    fLock.Leave;
  end;
end;

procedure TLockedDocVariant.AddNewProp(const Name: RawUtf8;
  const Value: variant; var Obj: variant);
begin
  fLock.Enter;
  try
    SetValue(Name, Value);
    _ObjAddProps([Name, Value], Obj);
  finally
    fLock.Leave;
  end;
end;

function TLockedDocVariant.GetValue(const Name: RawUtf8): Variant;
begin
  fLock.Enter;
  try
    fValue.RetrieveValueOrRaiseException(pointer(Name), length(Name),
      dvoNameCaseSensitive in fValue.Options, result, false);
  finally
    fLock.Leave;
  end;
end;

procedure TLockedDocVariant.SetValue(const Name: RawUtf8;
  const Value: Variant);
begin
  fLock.Enter;
  try
    fValue.AddOrUpdateValue(Name, Value);
  finally
    fLock.Leave;
  end;
end;

procedure TLockedDocVariant.AddItem(const Value: variant);
begin
  fLock.Enter;
  try
    fValue.AddItem(Value);
  finally
    fLock.Leave;
  end;
end;

function TLockedDocVariant.Copy: variant;
begin
  VarClear(result);
  fLock.Enter;
  try
    TDocVariantData(result).InitCopy(variant(fValue), JSON_FAST);
  finally
    fLock.Leave;
  end;
end;

procedure TLockedDocVariant.Clear;
var
  opt: TDocVariantOptions;
begin
  fLock.Enter;
  try
    opt := fValue.Options;
    fValue.ClearFast;
    fValue.Init(opt);
  finally
    fLock.Leave;
  end;
end;

function TLockedDocVariant.ToJson(HumanReadable: boolean): RawUtf8;
var
  tmp: RawUtf8;
begin
  fLock.Enter;
  try
    VariantSaveJson(variant(fValue), twJsonEscape, tmp);
  finally
    fLock.Leave;
  end;
  if HumanReadable then
    JsonBufferReformat(pointer(tmp), result)
  else
    result := tmp;
end;

function TLockedDocVariant.Lock: TAutoLocker;
begin
  result := fLock;
end;


{ ************** JSON Parsing into Variant }

function GetVariantFromNotStringJson(Json: PUtf8Char; var Value: TVarData;
  AllowDouble: boolean): boolean;
begin
  if Json <> nil then
    Json := GotoNextNotSpace(Json);
  if (Json = nil) or
     ((PInteger(Json)^ = NULL_LOW) and
      (jcEndOfJsonValueField in JSON_CHARS[Json[4]])) then
    TRttiVarData(Value).VType := varNull
  else if (PInteger(Json)^ = FALSE_LOW) and
          (Json[4] = 'e') and
          (jcEndOfJsonValueField in JSON_CHARS[Json[5]]) then
  begin
    TRttiVarData(Value).VType := varBoolean;
    Value.VInteger := ord(false);
  end
  else if (PInteger(Json)^ = TRUE_LOW) and
          (jcEndOfJsonValueField in JSON_CHARS[Json[4]]) then
  begin
    TRttiVarData(Value).VType := varBoolean;
    Value.VInteger := ord(true);
  end
  else if not GetNumericVariantFromJson(Json, Value, AllowDouble) then
  begin
    result := false;
    exit;
  end;
  result := true;
end;

function GotoEndOfJsonNumber(P: PUtf8Char; var PEndNum: PUtf8Char): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif} // inlined for better code generation
var
  tab: PJsonCharSet;
begin
  result := P;
  tab := @JSON_CHARS;
  repeat
    inc(result);
  until not (jcDigitFloatChar in tab[result^]);
  PEndNum := result;
  while not (jcEndOfJsonFieldNotName in tab[result^]) do
    inc(result); // #0, ',', ']', '}'
end;

// internal method used by VariantLoadJson(), GetVariantFromJson() and
// TDocVariantData.InitJson()
procedure GetJsonToAnyVariant(var Value: variant; var Json: PUtf8Char;
  EndOfObject: PUtf8Char; Options: PDocVariantOptions; AllowDouble: boolean);
var
  V: TVarData absolute Value;
  n, Plen: integer;
  t: ^TSynInvokeableVariantType;
  P, P2, P3: PUtf8Char;
  EndOfObject2: AnsiChar;
  wasParsedWithinString: boolean;
  wasString: boolean;
label
  parse, parsed, astext;
begin
  VarClear(Value);
  if EndOfObject <> nil then
    EndOfObject^ := ' ';
  P := Json;
  if Options = nil then
    goto parse; // no complex variant -> direct process
  if dvoAllowDoubleValue in Options^ then
    AllowDouble := true; // for GetVariantFromNotStringJson() below
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  wasParsedWithinString := false;
  case JSON_TOKENS[P^] of
    jtFirstDigit:  // '-', '0'..'9': numbers are directly processed
      begin
        P2 := P;
        P := GotoEndOfJsonNumber(P, P3{%H-});
        if EndOfObject <> nil then
          EndOfObject^ := P^;
        if P3^ <> #0 then
          P3^ := #0; // make asciiz
        if not GetNumericVariantFromJson(P2, V, AllowDouble) then
        begin
          P := P2;
          PLen := P3 - P;
          goto astext; // too big number is stored as text
        end;
        if (P = P3) or
           (P^ <> #0) then
          inc(P);
        Json := P;
        exit;
      end;
    jtDoubleQuote:
      if dvoJsonObjectParseWithinString in Options^ then
      begin
        P := GetJsonField(P, Json, @wasString, EndOfObject, @Plen);
        EndOfObject := nil;
        wasParsedWithinString := true;
      end
      else
      begin
        // parse string/numerical values or true/false/null constants
parse:  P := GetJsonField(P, Json, @wasString, EndOfObject, @Plen);
parsed: if {%H-}wasString or
           not GetVariantFromNotStringJson(P, V, AllowDouble) then
        begin
astext:   TRttiVarData(V).VType := varString;
          V.VAny := nil; // avoid GPF below
          FastSetString(RawUtf8(V.VAny), P, Plen{%H-});
        end;
        exit;
      end;
    jtNullFirstChar:
      if PInteger(P)^ = NULL_LOW then
        goto parse;
    jtFalseFirstChar:
      if PInteger(P)^ = FALSE_LOW then
        goto parse;
    jtTrueFirstChar:
      if PInteger(P)^ = TRUE_LOW then
        goto parse;
  end;
  // if we reach here, input Json may be some complex value
  t := pointer(SynVariantTryJsonTypes);
  if (t <> nil) and
     not (dvoJsonParseDoNotTryCustomVariants in Options^) then
  begin
    n := PDALen(PAnsiChar(t) - _DALEN)^ + _DAOFF; // call all TryJsonToVariant()
    repeat
      P2 := P;
      // currently, only implemented by mormot.db.nosql.bson BsonVariantType
      if t^.TryJsonToVariant(P2, Value, @EndOfObject2) then
      begin
        if not wasParsedWithinString then
        begin
          if EndOfObject <> nil then
            EndOfObject^ := EndOfObject2;
          Json := P2;
        end;
        exit;
      end;
      inc(t);
      dec(n);
    until n = 0;
  end;
  if P^ in ['{', '['] then
  begin
    // default Json parsing and conversion to TDocVariant instance
    P := TDocVariantData(Value).InitJsonInPlace(P, Options^, EndOfObject);
    if P = nil then
    begin
      TDocVariantData(Value).ClearFast;
      Json := nil;
      exit; // error parsing
    end;
    if not wasParsedWithinString then
      Json := P;
  end
  else // back to simple variant types
    if wasParsedWithinString then
      goto parsed
    else
      goto parse;
end;

function TextToVariantNumberTypeNoDouble(Json: PUtf8Char): cardinal;
var
  start: PUtf8Char;
  c: AnsiChar;
begin
  result := varString;
  c := Json[0];
  if (jcDigitFirstChar in JSON_CHARS[c]) and // ['-', '0'..'9']
     (((c >= '1') and
       (c <= '9')) or      // is first char numeric?
     ((c = '0') and
      ((Json[1] = '.') or
       (Json[1] = #0))) or // '012' is not Json, but '0.xx' and '0' are
     ((c = '-') and
      (Json[1] >= '0') and
      (Json[1] <= '9'))) then  // negative number
  begin
    start := Json;
    repeat
      inc(Json)
    until (Json^ < '0') or
          (Json^ > '9'); // check digits
    case Json^ of
      #0:
        if Json - start <= 19 then
          // no decimal, and matcthing signed Int64 precision
          result := varInt64;
      '.':
        if (Json[1] >= '0') and
           (Json[1] <= '9') and
           (Json[2] in [#0, '0'..'9']) then
          if (Json[2] = #0) or
             (Json[3] = #0) or
             ((Json[3] >= '0') and
              (Json[3] <= '9') and
              (Json[4] = #0) or
             ((Json[4] >= '0') and
              (Json[4] <= '9') and
              (Json[5] = #0))) then
            result := varCurrency; // currency ###.1234 number
    end;
  end;
end;

function TextToVariantNumberType(Json: PUtf8Char): cardinal;
var
  start: PUtf8Char;
  exp: PtrInt;
  c: AnsiChar;
label
  exponent;
begin
  result := varString;
  c := Json[0];
  if (jcDigitFirstChar in JSON_CHARS[c]) and // ['-', '0'..'9']
     (((c >= '1') and
       (c <= '9')) or      // is first char numeric?
     ((c = '0') and
      ((Json[1] = '.') or
       (Json[1] = #0))) or // '012' is not Json, but '0.xx' and '0' are
     ((c = '-') and
      (Json[1] >= '0') and
      (Json[1] <= '9'))) then  // negative number
  begin
    start := Json;
    repeat
      inc(Json)
    until (Json^ < '0') or
          (Json^ > '9'); // check digits
    case Json^ of
      #0:
        if Json - start <= 19 then // signed Int64 precision
          result := varInt64
        else
          result := varDouble; // we may loose precision, but still a number
      '.':
        if (Json[1] >= '0') and
           (Json[1] <= '9') and
           (Json[2] in [#0, '0'..'9']) then
          if (Json[2] = #0) or
             (Json[3] = #0) or
             ((Json[3] >= '0') and
              (Json[3] <= '9') and
              (Json[4] = #0) or
             ((Json[4] >= '0') and
              (Json[4] <= '9') and
              (Json[5] = #0))) then
            result := varCurrency // currency ###.1234 number
          else
          begin
            repeat // more than 4 decimals
              inc(Json)
            until (Json^ < '0') or
                  (Json^ > '9');
            case Json^ of
              #0:
                result := varDouble;
              'e', 'E':
                begin
exponent:         inc(Json); // inlined custom GetInteger()
                  start := Json;
                  c := Json^;
                  if (c = '-') or
                     (c = '+') then
                  begin
                    inc(Json);
                    c := Json^;
                  end;
                  inc(Json);
                  dec(c, 48);
                  if c > #9 then
                    exit;
                  exp := ord(c);
                  c := Json^;
                  dec(c, 48);
                  if c <= #9 then
                  begin
                    inc(Json);
                    exp := exp * 10 + ord(c);
                    c := Json^;
                    dec(c, 48);
                    if c <= #9 then
                    begin
                      inc(Json);
                      exp := exp * 10 + ord(c);
                    end;
                  end;
                  if Json^ <> #0 then
                    exit;
                  if start^ = '-' then
                    exp := -exp;
                  if (exp > -324) and
                     (exp < 308) then
                    result := varDouble; // 5.0 x 10^-324 .. 1.7 x 10^308
                end;
            end;
          end;
      'e', 'E':
        goto exponent;
    end;
  end;
end;

function GetNumericVariantFromJson(Json: PUtf8Char; var Value: TVarData;
  AllowVarDouble: boolean): boolean;
var
  // logic below is extracted from mormot.core.base.pas' GetExtended()
  digit: integer;
  frac, exp: PtrInt;
  c: AnsiChar;
  flags: set of (fNeg, fNegExp, fValid);
  v64: Int64; // allows 64-bit resolution for the digits (match 80-bit extended)
  d: double;
begin
  // parse input text as number into v64, frac, digit, exp
  result := false;
  byte(flags) := 0;
  v64 := 0;
  frac := 0;
  if Json = nil then
    exit;
  c := Json^;
  if c = '-' then // note: '+xxx' is not valid Json so is not handled here
  begin
    inc(Json);
    c := Json^;
    include(flags, fNeg);
  end;
  if (c = '0') and
     ((Json[1] <> '.') and
      (Json[1] <> #0)) then // '012' is not Json, but '0.xx' and '0' are
    exit;
  digit := 19; // max Int64 resolution
  repeat
    inc(Json);
    if (c >= '0') and
       (c <= '9') then
    begin
      dec(digit); // over-required digits are just ignored
      if digit >= 0 then
      begin
        dec(c, ord('0'));
        {$ifdef CPU64}
        v64 := v64 * 10;
        {$else}
        v64 := v64 shl 3 + v64 + v64;
        {$endif CPU64}
        inc(v64, integer(c));
        include(flags, fValid);
        if frac <> 0 then
          dec(frac); // frac<0 for digits after '.'
        c := Json^;
        continue;
      end;
      if frac >= 0 then
        inc(frac); // frac>0 to handle #############00000
      c := Json^;
      continue;
    end;
    if c <> '.' then
      break;
    if frac > 0 then
      exit;
    dec(frac);
    c := Json^;
    if c = #0 then
      exit; // invalid '123.'
  until false;
  if frac < 0 then
    inc(frac); // adjust digits after '.'
  if (c = 'E') or
     (c = 'e') then
  begin
    exp := 0;
    exclude(flags, fValid);
    c := Json^;
    if c = '+' then
      inc(Json)
    else if c = '-' then
    begin
      inc(Json);
      include(flags, fNegExp);
    end;
    repeat
      c := Json^;
      inc(Json);
      if (c < '0') or
         (c > '9') then
        break;
      dec(c, ord('0'));
      exp := (exp * 10) + byte(c);
      include(flags, fValid);
    until false;
    if fNegExp in flags then
      dec(frac, exp)
    else
      inc(frac, exp);
  end;
  if (c <> #0) or
     (not (fValid in flags) and
      (c = #0)) then
    exit;
  if fNeg in flags then
    v64 := -v64;
  // now v64, frac, digit, exp contain the parsed number
  if (frac = 0) and
     (digit >= 0) then
  begin
    // return an integer or Int64 value
    Value.VInt64 := v64;
    if digit <= 9 then
      TRttiVarData(Value).VType := varInt64
    else
      TRttiVarData(Value).VType := varInteger;
  end
  else if (frac < 0) and
          (frac >= -4) then
  begin
    // currency as ###.0123
    TRttiVarData(Value).VType := varCurrency;
    inc(frac, 4);
    if frac <> 0 then // stored as round(CurrValue*10000)
      repeat
        {$ifdef CPU64}
        v64 := v64 * 10;
        {$else}
        v64 := v64 shl 3 + v64 + v64;
        {$endif CPU64}
        dec(frac);
      until frac = 0;
    Value.VInt64 := v64;
  end
  else if not AllowVarDouble then
    exit
  else
  begin
    // any double value
    TRttiVarData(Value).VType := varDouble;
    if (frac >= -31) and
       (frac <= 31) then
      d := POW10[frac]
    else
      d := HugePower10(frac);
    Value.VDouble := d * v64;
  end;
  result := true;
end;

procedure UniqueVariant(Interning: TRawUtf8Interning; var aResult: variant;
  aText: PUtf8Char; aTextLen: PtrInt; aAllowVarDouble: boolean);
var
  tmp: RawUtf8;
begin
  if not GetNumericVariantFromJson(
    aText, TVarData(aResult), aAllowVarDouble) then
  begin
    FastSetString(tmp, aText, aTextLen);
    if Interning = nil then
      RawUtf8ToVariant(tmp, aResult)
    else
      Interning.UniqueVariant(aResult, tmp);
  end;
end;

procedure JsonToVariantInPlace(var Value: variant; Json: PUtf8Char;
  Options: TDocVariantOptions; AllowDouble: boolean);
begin
  if (Json <> nil) and
     (Json^ <> #0) then
    GetJsonToAnyVariant(Value, Json, nil, @Options, AllowDouble)
  else
    VarClear(Value);
end;

function JsonToVariant(const Json: RawUtf8; Options: TDocVariantOptions;
  AllowDouble: boolean): variant;
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Json); // temp copy before in-place decoding
  try
    JsonToVariantInPlace(result, tmp.buf, Options, AllowDouble);
  finally
    tmp.Done;
  end;
end;

procedure TextToVariant(const aValue: RawUtf8; AllowVarDouble: boolean;
  out aDest: variant);
begin
  if not GetNumericVariantFromJson(
     pointer(aValue), TVarData(aDest), AllowVarDouble) then
    RawUtf8ToVariant(aValue, aDest);
end;

function GetNextItemToVariant(var P: PUtf8Char; out Value: Variant;
  Sep: AnsiChar; AllowDouble: boolean): boolean;
var
  temp: RawUtf8;
begin
  if P = nil then
    result := false
  else
  begin
    GetNextItem(P, Sep, temp);
    if not GetNumericVariantFromJson(
        pointer(temp), TVarData(Value), AllowDouble) then
      RawUtf8ToVariant(temp, Value);
    result := true;
  end;
end;

procedure GetVariantFromJson(Json: PUtf8Char; wasString: boolean;
  var Value: variant; TryCustomVariants: PDocVariantOptions;
  AllowDouble: boolean; JsonLen: integer);
var
  V: TVarData absolute Value;
begin
  // first handle any strict-Json syntax objects or arrays into custom variants
  // (e.g. when called directly from TOrmPropInfoRttiVariant.SetValue)
  if (TryCustomVariants <> nil) and
     (Json <> nil) then
    if (GotoNextNotSpace(Json)^ in ['{', '[']) and
       not wasString then
    begin
      GetJsonToAnyVariant(Value, Json, nil, TryCustomVariants, AllowDouble);
      exit;
    end
    else
      AllowDouble := dvoAllowDoubleValue in TryCustomVariants^;
  // handle simple text or numerical values
  VarClear(Value);
  // try any numerical value
  if wasString or
     not GetVariantFromNotStringJson(Json, V, AllowDouble) then
  begin
    // found no numerical value -> return a string in the expected format
    TRttiVarData(Value).VType := varString;
    V.VString := nil; // avoid GPF below
    if JsonLen = 0 then
      JsonLen := StrLen(Json);
    FastSetString(RawUtf8(V.VString), Json, JsonLen);
  end;
end;

procedure _BinaryVariantLoadAsJson(var Value: variant; Json: PUtf8Char;
  TryCustomVariant: pointer);
begin
  if TryCustomVariant = nil then
    TryCustomVariant := @JSON_[mFast];
  GetJsonToAnyVariant(Value, Json, nil, TryCustomVariant, {double=}true);
end;

function VariantLoadJson(var Value: variant; Json, EndOfObject: PUtf8Char;
  TryCustomVariants: PDocVariantOptions; AllowDouble: boolean): PUtf8Char;
var
  wasString: boolean;
  Val: PUtf8Char;
  ValLen: integer;
begin
  result := Json;
  if Json = nil then
    exit;
  if TryCustomVariants <> nil then
  begin
    if dvoAllowDoubleValue in TryCustomVariants^ then
      AllowDouble := true;
    if dvoJsonObjectParseWithinString in TryCustomVariants^ then
    begin
      Json := GotoNextNotSpace(Json);
      if Json^ = '"' then
      begin
        Val := GetJsonField(result, result, @wasString, EndOfObject);
        GetJsonToAnyVariant(
          Value, Val, EndOfObject, TryCustomVariants, AllowDouble);
      end
      else
        GetJsonToAnyVariant(
          Value, result, EndOfObject, TryCustomVariants, AllowDouble);
    end
    else
      GetJsonToAnyVariant(
        Value, result, EndOfObject, TryCustomVariants, AllowDouble);
  end
  else
  begin
    Val := GetJsonField(result, result, @wasString, EndOfObject, @ValLen);
    GetVariantFromJson(Val, wasString, Value, nil, AllowDouble, ValLen);
  end;
  if result = nil then
    result := @NULCHAR; // reached end, but not invalid input
end;

procedure VariantLoadJson(var Value: Variant; const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions; AllowDouble: boolean);
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Json); // temp copy before in-place decoding
  try
    VariantLoadJson(Value, tmp.buf, nil, TryCustomVariants, AllowDouble);
  finally
    tmp.Done;
  end;
end;

function VariantLoadJson(const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions; AllowDouble: boolean): variant;
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Json);
  try
    VariantLoadJson(result, tmp.buf, nil, TryCustomVariants, AllowDouble);
  finally
    tmp.Done;
  end;
end;


{ ************** Variant Binary Serialization }

{$ifndef PUREMORMOT2}

function VariantSaveLength(const Value: variant): integer;
begin
  result := {%H-}BinarySaveLength(@Value, TypeInfo(Variant), nil, [rkVariant]);
end;

function VariantSave(const Value: variant; Dest: PAnsiChar): PAnsiChar;
var
  dummy: integer;
begin
  result := {%H-}BinarySave(@Value, Dest, TypeInfo(Variant), dummy, [rkVariant]);
end;

{$endif PUREMORMOT2}

function VariantSave(const Value: variant): RawByteString;
begin
  result := BinarySave(@Value, TypeInfo(Variant), [rkVariant]);
end;

function VariantLoad(var Value: variant; Source: PAnsiChar;
  CustomVariantOptions: PDocVariantOptions; SourceMax: PAnsiChar): PAnsiChar;
begin
  if SourceMax = nil then
    // backward compatible: assume fake 100MB Source input buffer
    SourceMax := Source + 100 shl 20;
  result := BinaryLoad(@Value, Source, TypeInfo(Variant), nil, SourceMax,
    [rkVariant], CustomVariantOptions);
end;

function VariantLoad(const Bin: RawByteString;
  CustomVariantOptions: PDocVariantOptions): variant;
begin
  BinaryLoad(@result, Bin, TypeInfo(Variant),
    [rkVariant], CustomVariantOptions);
end;

procedure FromVarVariant(var Source: PByte; var Value: variant;
  CustomVariantOptions: PDocVariantOptions);
begin
  Source := PByte(VariantLoad(Value, PAnsiChar(Source), CustomVariantOptions));
end;

var
  // naive but efficient type cache - e.g. for TBsonVariant or TQuickJsVariant
  LastDispInvoke: TSynInvokeableVariantType;

// sysdispinvoke() replacement to meet TSynInvokeableVariantType expectations
procedure NewDispInvoke(Dest: PVarData;
{$ifdef FPC_VARIANTSETVAR}
  var Source: TVarData;
{$else} // see http://mantis.freepascal.org/view.php?id=26773
  const Source: TVarData; // "[ref] const" on modern Delphi
{$endif FPC_VARIANTSETVAR}
  CallDesc: PCallDesc; Params: pointer); cdecl;
// warning: Delphi OSX64 LINUX ANDROID64 expects Params := @VAList
var
  v: TVarData;
  vp: PVariant;
  t: cardinal;
  ct: TSynInvokeableVariantType;
label
  direct;
begin
  t := Source.vType;
  if t = varVariantByRef then
    NewDispInvoke(Dest, PVarData(Source.VPointer)^, calldesc, params)
  else
  begin
    TRttiVarData(v).VType := varEmpty;
    vp := @v;
    if Dest = nil then
      vp := nil;
    ct := nil;
    try
      case t of
        varDispatch,
        varAny,
        varUnknown,
        varDispatch or varByRef,
        varAny or varByRef,
        varUnknown or varByRef:
          if Assigned(VarDispProc) and
             Assigned(VarCopyProc) then
            // standard Windows ComObj unit call
            VarDispProc(vp, variant(Source), CallDesc, Params)
          else
            VarInvalidOp;
        CFirstUserType .. varTypeMask:
          begin
            ct := DocVariantType; // recognize our TDocVariant
            if t = ct.VarType then
              goto direct;
            ct := LastDispInvoke; // atomic load
            if (ct <> nil) and
               (ct.VarType = t) then
              // most calls are grouped within the same custom variant type
              goto direct;
            // FindCustomVariantType() is O(1) but has a global lock
            if FindCustomVariantType(t, TCustomVariantType(ct)) then
              if ct.InheritsFrom(TSynInvokeableVariantType) then
              begin
                // direct access of our custom variants without any temp copy
                LastDispInvoke := ct;
direct:         if Dest <> nil then
                  VarClear(PVariant(Dest)^); // no temp copy, but Dest cleanup
                ct.DispInvoke(Dest, Source, CallDesc, Params);
                Dest := nil;
              end
              else if ct.InheritsFrom(TInvokeableVariantType) then
                // use standard RTL behavior for non-mORMot custom variants
                ct.DispInvoke(pointer(vp), Source, CallDesc, Params)
              else
               VarInvalidOp
            else
             VarInvalidOp;
          end;
      else
        VarInvalidOp;
      end;
    finally
      if Dest <> nil then
      begin
        if (ct <> nil) and
           (v.VType = ct.VarType) then // don't search twice if we got it
          ct.Copy(Dest^, v, {indirect=}false)
        else
          VarCopyProc(Dest^, v);
        VarClear(vp^);
      end;
    end;
  end;
end;

function SortDynArrayEmptyNull(const A, B): integer;
begin
  result := 0; // VType=varEmpty/varNull are always equal
end;

function SortDynArrayWordBoolean(const A, B): integer;
begin
  if WordBool(A) then // normalize
    if WordBool(B) then
      result := 0
    else
      result := 1
  else if WordBool(B) then
    result := -1
  else
    result := 0;
end;

const
  // comparison of simple types - copied at startup to _VARDATACMP[]
  _NUM1: array[varEmpty..varDate] of TDynArraySortCompare = (
    SortDynArrayEmptyNull,
    SortDynArrayEmptyNull,
    SortDynArraySmallInt,
    SortDynArrayInteger,
    SortDynArraySingle,
    SortDynArrayDouble,
    SortDynArrayInt64,
    SortDynArrayDouble);
  _NUM2: array[varShortInt..varWord64] of TDynArraySortCompare = (
    SortDynArrayShortInt,
    SortDynArrayByte,
    SortDynArrayWord,
    SortDynArrayCardinal,
    SortDynArrayInt64,
    SortDynArrayQWord);

procedure InitializeUnit;
var
  vm: TVariantManager; // available since Delphi 7
  vt: cardinal;
  {$ifdef FPC}
  test: variant;
  {$endif FPC}
begin
  // register the TDocVariant custom type
  DocVariantType := TDocVariant(SynRegisterCustomVariantType(TDocVariant));
  vt := DocVariantType.VarType;
  DocVariantVType := vt;
  PCardinal(@DV_FAST[dvUndefined])^ := vt;
  PCardinal(@DV_FAST[dvArray])^ := vt;
  PCardinal(@DV_FAST[dvObject])^ := vt;
  assert({%H-}SynVariantTypes[0].VarType = vt);
  PDocVariantData(@DV_FAST[dvUndefined])^.VOptions := JSON_FAST;
  PDocVariantData(@DV_FAST[dvArray])^.VOptions := JSON_FAST + [dvoIsArray];
  PDocVariantData(@DV_FAST[dvObject])^.VOptions := JSON_FAST + [dvoIsObject];
  // FPC allows to define variables with absolute JSON_[...] but Delphi doesn't
  JSON_FAST_STRICT := JSON_[mFastStrict];
  JSON_FAST_EXTENDED := JSON_[mFastExtended];
  JSON_FAST_EXTENDEDINTERN := JSON_[mFastExtendedIntern];
  JSON_NAMEVALUE := PDocVariantOptionsBool(@JSON_[mNameValue])^;
  JSON_NAMEVALUEINTERN := PDocVariantOptionsBool(@JSON_[mNameValueIntern])^;
  JSON_OPTIONS:= PDocVariantOptionsBool(@JSON_[mDefault])^;
  // redirect to the feature complete variant wrapper functions
  BinaryVariantLoadAsJson := _BinaryVariantLoadAsJson;
  VariantClearSeveral := _VariantClearSeveral;
  SortDynArrayVariantComp := pointer(@FastVarDataComp);
  // setup FastVarDataComp() efficient lookup comparison functions
  MoveFast(_NUM1, _VARDATACMP[false, varEmpty], SizeOf(_NUM1));
  MoveFast(_NUM2, _VARDATACMP[false, varShortInt], SizeOf(_NUM2));
  _VARDATACMP[false, varBoolean] := @SortDynArrayWordBoolean;
  _VARDATACMP[true] := _VARDATACMP[false]; // =caseinsensitive
  {$ifdef CPUINTEL}
  _VARDATACMP[false, varString] := @SortDynArrayAnsiString;
  {$else}
  _VARDATACMP[false, varString] := @SortDynArrayRawByteString;
  {$endif CPUINTEL}
  _VARDATACMP[true,  varString] := @SortDynArrayAnsiStringI;
  _VARDATACMP[false, varOleStr] := @SortDynArrayUnicodeString;
  _VARDATACMP[true,  varOleStr] := @SortDynArrayUnicodeStringI;
  {$ifdef HASVARUSTRING}
  _VARDATACMP[false, varUString] := @SortDynArrayUnicodeString;
  _VARDATACMP[true,  varUString] := @SortDynArrayUnicodeStringI;
  {$endif HASVARUSTRING}
  // patch DispInvoke for performance and to circumvent RTL inconsistencies
  GetVariantManager(vm);
  vm.DispInvoke := NewDispInvoke;
  SetVariantManager(vm);
  {$ifdef FPC}
  // circumvent FPC 3.2+ inverted parameters order - may be fixed in later FPC
  test := _ObjFast([]);
  test.Add('nam', 'val');
  DispInvokeArgOrderInverted := (_Safe(test)^.Names[0] = 'val');
  {$endif FPC}
end;

initialization
  InitializeUnit;

end.

