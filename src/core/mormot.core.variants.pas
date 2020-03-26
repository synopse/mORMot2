/// Framework Core Low-Level Variants / TDocVariant process
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.variants;

{
  *****************************************************************************

  Variant / TDocVariant feature shared by all framework units
  - Low-Level Variant Wrappers
  - Custom Variant Types with JSON support

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  contnrs,
  types,
  sysutils,
  variants,
  mormot.core.base,
  mormot.core.text,
  mormot.core.json;

  
{ ************** Low-Level Variant Wrappers }

/// fastcheck if a variant hold a value
// - varEmpty, varNull or a '' string would be considered as void
// - varBoolean=false or varDate=0 would be considered as void
// - a TDocVariantData with Count=0 would be considered as void
// - any other value (e.g. integer) would be considered as not void
function VarIsVoid(const V: Variant): boolean;

/// returns a supplied string as variant, or null if v is void ('')
function VarStringOrNull(const v: RawUTF8): variant;

type
  /// a set of simple TVarData.VType, as specified to VarIs()
  TVarDataTypes = set of 0..255;

/// allow to check for a specific set of TVarData.VType
function VarIs(const V: Variant; const VTypes: TVarDataTypes): Boolean;
  {$ifdef HASINLINE} inline;{$endif}

/// same as Dest := Source, but copying by reference
// - i.e. VType is defined as varVariant or varByRef
// - for instance, it will be used for late binding of TDocVariant properties,
// to let following statements work as expected:
// ! V := _Json('{arr:[1,2]}');
// ! V.arr.Add(3);   // will work, since V.arr will be returned by reference
// ! writeln(V);     // will write '{"arr":[1,2,3]}'
procedure SetVariantByRef(const Source: Variant; var Dest: Variant);

/// same as Dest := Source, but copying by value
// - will unreference any varByRef content
// - will convert any string value into RawUTF8 (varString) for consistency
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

/// convert a FormatUTF8() UTF-8 encoded string into a variant RawUTF8 varString
procedure FormatUTF8ToVariant(const Fmt: RawUTF8; const Args: array of const; var Value: variant);

/// convert an UTF-8 encoded text buffer into a variant RawUTF8 varString
// - this overloaded version expects a destination variant type (e.g. varString
// varOleStr / varUString) - if the type is not handled, will raise an
// EVariantTypeCastError
procedure RawUTF8ToVariant(const Txt: RawUTF8; var Value: TVarData;
  ExpectedValueType: cardinal); overload;

  
{ ************** Custom Variant Types with JSON support }

type
  /// custom variant handler with easier/faster access of variant properties,
  // and JSON serialization support
  // - default GetProperty/SetProperty methods are called via some protected
  // virtual IntGet/IntSet methods, with less overhead (to be overriden)
  // - these kind of custom variants will be faster than the default
  // TInvokeableVariantType for properties getter/setter, but you should
  // manually register each type by calling SynRegisterCustomVariantType()
  // - also feature custom JSON parsing, via TryJSONToVariant() protected method
  TSynInvokeableVariantType = class(TInvokeableVariantType)
  protected
    {$ifdef ISDELPHI}
    /// our custom call backs do not want the function names to be uppercased
    function FixupIdent(const AText: string): string; override;
    {$endif ISDELPHI}
    /// override those two abstract methods for fast getter/setter implementation
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; virtual;
    function IntSet(const Instance, Value: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; virtual;
  public
    /// search of a registered custom variant type from its low-level VarType
    // - will first compare with its own VarType for efficiency
    function FindSynVariantType(aVarType: Word; out CustomType: TSynInvokeableVariantType): boolean;
    /// customization of JSON parsing into variants
    // - will be called by e.g. by VariantLoadJSON() or GetVariantFromJSON()
    // with Options: PDocVariantOptions parameter not nil
    // - this default implementation will always returns FALSE,
    // meaning that the supplied JSON is not to be handled by this custom
    // (abstract) variant type
    // - this method could be overridden to identify any custom JSON content
    // and convert it into a dedicated variant instance, then return TRUE
    // - warning: should NOT modify JSON buffer in-place, unless it returns true
    function TryJSONToVariant(var JSON: PUTF8Char; var Value: variant;
      EndOfObject: PUTF8Char): boolean; virtual;
    /// customization of variant into JSON serialization
    procedure ToJSON(W: TBaseWriter; const Value: variant; Escape: TTextWriterKind); overload; virtual;
    /// retrieve the field/column value
    // - this method will call protected IntGet abstract method
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: String): Boolean; override;
    /// set the field/column value
    // - this method will call protected IntSet abstract method
    {$ifdef FPC_VARIANTSETVAR} // see http://mantis.freepascal.org/view.php?id=26773
    function SetProperty(var V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
    {$else}
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
    {$endif}
    /// clear the content
    // - this default implementation will set VType := varEmpty
    // - override it if your custom type needs to manage its internal memory
    procedure Clear(var V: TVarData); override;
    /// copy two variant content
    // - this default implementation will copy the TVarData memory
    // - override it if your custom type needs to manage its internal structure
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    /// copy two variant content by value
    // - this default implementation will call the Copy() method
    // - override it if your custom types may use a by reference copy pattern
    procedure CopyByValue(var Dest: TVarData; const Source: TVarData); virtual;
    /// this method will allow to look for dotted name spaces, e.g. 'parent.child'
    // - should return Unassigned if the FullName does not match any value
    // - will identify TDocVariant storage, or resolve and call the generic
    // TSynInvokeableVariantType.IntGet() method until nested value match
    procedure Lookup(var Dest: TVarData; const Instance: TVarData; FullName: PUTF8Char);
    /// will check if the value is an array, and return the number of items
    // - if the document is an array, will return the items count (0 meaning
    // void array) - used e.g. by TSynMustacheContextVariant
    // - this default implementation will return -1 (meaning this is not an array)
    // - overridden method could implement it, e.g. for TDocVariant of kind dvArray
    function IterateCount(const V: TVarData): integer; virtual;
    /// allow to loop over an array document
    // - Index should be in 0..IterateCount-1 range
    // - this default implementation will do nothing
    procedure Iterate(var Dest: TVarData; const V: TVarData; Index: integer); virtual;
    /// returns TRUE if the supplied variant is of the exact custom type
    function IsOfType(const V: variant): boolean;
  end;

  /// class-reference type (metaclass) of custom variant type definition
  // - used by SynRegisterCustomVariantType() function
  TSynInvokeableVariantTypeClass = class of TSynInvokeableVariantType;

/// register a custom variant type to handle properties
// - this will implement an internal mechanism used to bypass the default
// _DispInvoke() implementation in Variant.pas, to use a faster version
// - is called in case of TSynTableVariant, TDocVariant, TBSONVariant or
// TSQLDBRowVariant
function SynRegisterCustomVariantType(aClass: TSynInvokeableVariantTypeClass): TSynInvokeableVariantType;


implementation

{ ************** Low-Level Variant Wrappers }

function VarIs(const V: Variant; const VTypes: TVarDataTypes): Boolean;
var
  vd: PVarData;
  vt: cardinal;
begin
  vd := @V;
  repeat
    vt := vd^.VType;
    if vt <> varVariant or varByRef then
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
      if vt = varVariant or varByRef then
        result := VarIsVoid(PVariant(VPointer)^)
      else if (vt = varByRef or varString) or (vt = varByRef or varOleStr)
      {$ifdef HASVARUSTRING} or (vt = varByRef or varUString) {$endif} then
        result := PPointer(VAny)^ = nil
      else if vt = cardinal(DocVariantVType) then
        //result := TDocVariantData(V).Count = 0
      else
        result := false;
    end;
end;

function VarStringOrNull(const v: RawUTF8): variant;
begin
  if v = '' then
    SetVariantNull(result)
  else
    RawUTF8ToVariant(v, result);
end;

procedure SetVariantByRef(const Source: Variant; var Dest: Variant);
var
  vt: cardinal;
begin
  VarClear(Dest);
  vt := TVarData(Source).VType;
  if (vt = varVariant or varByRef) or (vt in VTYPE_SIMPLE) then
    TVarData(Dest) := TVarData(Source)
  else if not SetVariantUnRefSimpleValue(Source, TVarData(Dest)) then
  begin
    TVarData(Dest).VType := varVariant or varByRef;
    TVarData(Dest).VPointer := @Source;
  end;
end;

procedure SetVariantByValue(const Source: Variant; var Dest: Variant);
var
  s: PVarData;
  d: TVarData absolute Dest;
  vt: cardinal;
begin
  s := @Source;
  VarClear(Dest);
  vt := s^.VType;
  if vt = varVariant or varByRef then
  begin
    s := s^.VPointer;
    vt := s^.VType;
  end;
  case vt of
    varEmpty..varDate, varBoolean, varShortInt..varWord64:
      begin
        d.VType := vt;
        d.VInt64 := s^.VInt64;
      end;
    varString:
      begin
        d.VType := varString;
        d.VAny := nil;
        RawByteString(d.VAny) := RawByteString(s^.VAny);
      end;
    varByRef or varString:
      begin
        d.VType := varString;
        d.VAny := nil;
        RawByteString(d.VAny) := PRawByteString(s^.VAny)^;
      end;
  {$ifdef HASVARUSTRING} varUString, varByRef or varUString, {$endif}
  varOleStr, varByRef or varOleStr:
      begin
        d.VType := varString;
        d.VAny := nil;
        VariantToUTF8(PVariant(s)^, RawUTF8(d.VAny)); // store a RawUTF8 instance
      end;
  else
    if not SetVariantUnRefSimpleValue(PVariant(s)^, d) then
      if vt = cardinal(DocVariantVType) then
        //DocVariantType.CopyByValue(d, s^)
      else
        Dest := PVariant(s)^;
  end;
end;

procedure ZeroFill(Value: PVarData);
begin // slightly faster than FillChar(Value,SizeOf(Value),0);
  PInt64Array(Value)^[0] := 0;
  PInt64Array(Value)^[1] := 0;
  {$ifdef CPU64}
  //assert(SizeOf(TVarData)=24);
  PInt64Array(Value)^[2] := 0;
  {$endif}
end;

procedure FillZero(var value: variant);
begin
  with TVarData(value) do
    case cardinal(VType) of
      varString:
        FillZero(RawByteString(VAny));
    end;
  VarClear(value);
end;

procedure FormatUTF8ToVariant(const Fmt: RawUTF8; const Args: array of const; var Value: variant);
begin
  RawUTF8ToVariant(FormatUTF8(Fmt, Args), Value);
end;

procedure RawUTF8ToVariant(const Txt: RawUTF8; var Value: TVarData; ExpectedValueType: cardinal);
begin
  if ExpectedValueType = varString then
  begin
    RawUTF8ToVariant(Txt,variant(Value));
    exit;
  end;
  VarClear(variant(Value));
  Value.VType := ExpectedValueType;
  Value.VAny := nil; // avoid GPF below
  if Txt <> '' then
    case ExpectedValueType of
      varOleStr:
        UTF8ToWideString(Txt, WideString(Value.VAny));
      {$ifdef HASVARUSTRING}
      varUString:
        UTF8DecodeToUnicodeString(pointer(Txt), length(Txt), UnicodeString(Value.VAny));
      {$endif}
    else
      raise ESynException.CreateUTF8('RawUTF8ToVariant(%)?', [ExpectedValueType]);
    end;
end;


{ ************** Custom Variant Types with JSON support }

var // owned by Variants.pas as TInvokeableVariantType/TCustomVariantType
  SynVariantTypes: array of TSynInvokeableVariantType;

function FindSynVariantTypeFromVType(aVarType: cardinal): TSynInvokeableVariantType;
  {$ifdef HASINLINE} inline;{$endif}
var
  i: integer;
  t: ^TSynInvokeableVariantType;
begin
  t := pointer(SynVariantTypes);
  for i := 1 to length(TObjectDynArray(t)) do
  begin
    result := t^;
    if result.VarType = aVarType then
      exit;
    inc(t);
  end;
  result := nil;
end;

function SynRegisterCustomVariantType(aClass: TSynInvokeableVariantTypeClass): TSynInvokeableVariantType;
var
  i: PtrInt;
begin
  for i := 0 to length(SynVariantTypes) - 1 do
  begin
    result := SynVariantTypes[i]; // returns already registered instance
    if PPointer(result)^ = pointer(aClass) then
      exit;
  end;
  result := aClass.Create; // register variant type
  ObjArrayAdd(SynVariantTypes, result);
end;


{ TSynInvokeableVariantType }

function TSynInvokeableVariantType.IterateCount(const V: TVarData): integer;
begin
  result := -1; // this is not an array
end;

procedure TSynInvokeableVariantType.Iterate(var Dest: TVarData; const V: TVarData; Index: integer);
begin // do nothing
end;

{$ifdef ISDELPHI}
function TSynInvokeableVariantType.FixupIdent(const AText: string): string;
begin
  result := AText; // NO uppercased identifier for our custom types!
end;
{$endif ISDELPHI}

function TSynInvokeableVariantType.IntGet(var Dest: TVarData; const Instance: TVarData; Name: PAnsiChar; NameLen: PtrInt): boolean;
begin
  raise ESynException.CreateUTF8('Unexpected %.IntGet(%): this kind of ' +
    'custom variant does not support sub-fields', [self, Name]);
end;

function TSynInvokeableVariantType.IntSet(const Instance, Value: TVarData; Name: PAnsiChar; NameLen: PtrInt): boolean;
begin
  raise ESynException.CreateUTF8('Unexpected %.IntSet(%): this kind of ' +
    'custom variant is read-only', [self, Name]);
end;

function TSynInvokeableVariantType.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
{$ifdef UNICODE}
var
  Buf: array[byte] of AnsiChar;
{$endif}
begin
  {$ifdef UNICODE}
  IntGet(Dest, V, Buf, RawUnicodeToUtf8(Buf, SizeOf(Buf), pointer(Name), length(Name), []));
  {$else}
  IntGet(Dest, V, pointer(Name), length(Name));
  {$endif UNICODE}
  result := true; // IntGet=false+Dest=null e.g. if dvoReturnNullForUnknownProperty
end;

{$ifdef FPC_VARIANTSETVAR} // see http://mantis.freepascal.org/view.php?id=26773
function TSynInvokeableVariantType.SetProperty(var V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
{$else}
function TSynInvokeableVariantType.SetProperty(const V: TVarData;
  const Name: string; const Value: TVarData): Boolean;
{$endif FPC_VARIANTSETVAR}
var
  ValueSet: TVarData;
  PropName: PAnsiChar;
  Unicode: pointer;
  PropNameLen, UnicodeLen: PtrInt;
  vt: cardinal;
{$ifdef UNICODE}
  Buf: array[byte] of AnsiChar; // to avoid heap allocation
{$endif}
begin
{$ifdef UNICODE}
  PropNameLen := RawUnicodeToUtf8(Buf, SizeOf(Buf), pointer(Name), length(Name), []);
  PropName := @Buf[0];
{$else}
  PropName := pointer(Name);
  PropNameLen := length(Name);
{$endif UNICODE}
  vt := Value.VType;
  if vt = varByRef or varOleStr then
  begin
    Unicode := PPointer(Value.VAny)^;
    UnicodeLen := length(WideString(Unicode));
  end
  else if vt = varOleStr then
  begin
    Unicode := Value.VAny;
    UnicodeLen := length(WideString(Unicode));
  end
  else
  {$ifdef HASVARUSTRING}
  if vt = varByRef or varUString then
  begin
    Unicode := PPointer(Value.VAny)^;
    UnicodeLen := length(UnicodeString(Unicode));
  end
  else if vt = varUString then
  begin
    Unicode := Value.VAny;
    UnicodeLen := length(UnicodeString(Unicode));
  end
  else
  {$endif HASVARUSTRING}
  if SetVariantUnRefSimpleValue(variant(Value), ValueSet) then
  begin
    result := IntSet(V, ValueSet, PropName, PropNameLen);
    exit;
  end
  else
  begin
    result := IntSet(V, Value, PropName, PropNameLen);
    exit;
  end;
  try // unpatched RTL does not like Unicode values :( -> use a temp RawUTF8
    ValueSet.VType := varString;
    ValueSet.VString := nil; // to avoid GPF in next line
    RawUnicodeToUtf8(Unicode, UnicodeLen, RawUTF8(ValueSet.VString));
    result := IntSet(V, ValueSet, PropName, PropNameLen);
  finally
    RawUTF8(ValueSet.VString) := ''; // avoid memory leak
  end;
end;

procedure TSynInvokeableVariantType.Clear(var V: TVarData);
begin
  ZeroFill(@V); // will set V.VType := varEmpty
end;

procedure TSynInvokeableVariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect then
    SimplisticCopy(Dest, Source, true)
  else
  begin
    VarClear(variant(Dest)); // Dest may be a complex type
    Dest := Source;
  end;
end;

procedure TSynInvokeableVariantType.CopyByValue(var Dest: TVarData; const Source: TVarData);
begin
  Copy(Dest, Source, false);
end;

function TSynInvokeableVariantType.TryJSONToVariant(var JSON: PUTF8Char;
  var Value: variant; EndOfObject: PUTF8Char): boolean;
begin
  result := false;
end;

procedure TSynInvokeableVariantType.ToJSON(W: TBaseWriter; const Value: variant;
  Escape: TTextWriterKind);
begin
  raise ESynException.CreateUTF8('%.ToJSON is not implemented', [self]);
end;

function TSynInvokeableVariantType.IsOfType(const V: variant): boolean;
var
  vt: cardinal;
  vd: PVarData;
begin
  if self <> nil then
  begin
    vd := @V;
    repeat
      vt := vd^.VType;
      if vt <> varByRef or varVariant then
        break;
      vd := vd^.VPointer;
    until false;
    result := vt = VarType;
  end
  else
    result := false;
end;

function TSynInvokeableVariantType.FindSynVariantType(aVarType: Word; out CustomType: TSynInvokeableVariantType): boolean;
begin
  if aVarType = VarType then
    CustomType := self
  else
    CustomType := FindSynVariantTypeFromVType(VarType);
  result := CustomType <> nil;
end;

procedure TSynInvokeableVariantType.Lookup(var Dest: TVarData;
  const Instance: TVarData; FullName: PUTF8Char);
var
  handler: TSynInvokeableVariantType;
  v, tmp: TVarData; // PVarData wouldn't store e.g. RowID/count
  vt: cardinal;
  itemName: ShortString;
begin
  PInteger(@Dest)^ := varEmpty; // left to Unassigned if not found
  v := Instance;
  repeat
    vt := v.VType;
    if vt <> varByRef or varVariant then
      break;
    v := PVarData(v.VPointer)^;
  until false;
  repeat
    if vt <= varString then
      exit; // we need a complex type to lookup
    GetNextItemShortString(FullName, itemName, '.');
    if itemName[0] in [#0, #255] then
      exit;
    itemName[ord(itemName[0]) + 1] := #0; // ensure is ASCIIZ
    if vt = VarType then
      handler := self
    else
    begin
      handler := FindSynVariantTypeFromVType(vt);
      if handler = nil then
        exit;
    end;
    tmp := v; // v will be modified in-place
    PInteger(@v)^ := varEmpty; // IntGet() would clear it otherwise!
    if not handler.IntGet(v, tmp, @itemName[1], ord(itemName[0])) then
      exit; // property not found
    repeat
      vt := v.VType;
      if vt <> varByRef or varVariant then
        break;
      v := PVarData(v.VPointer)^;
    until false;
    if (vt = cardinal(DocVariantVType)) {and (TDocVariantData(v).VCount=0)} then
      v.VType := varNull; // recognize void TDocVariant as null
  until FullName = nil;
  Dest := v;
end;



initialization


end.

