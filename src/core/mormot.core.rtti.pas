/// Framework Core Low-Level Cross-Compiler RTTI Definitions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.rtti;

{
  *****************************************************************************

   Cross-Compiler RTTI Definitions shared by all framework units
    - Low-Level Cross-Compiler RTTI Definitions
    - Enumerations RTTI
    - Published Class Properties and Methods RTTI
    - IInvokable Interface RTTI
    - Efficient Dynamic Arrays and Records Process
    - Managed Types Finalization, Random or Copy
    - RTTI Value Types used for JSON Parsing
    - RTTI-based Registration for Custom JSON Parsing
    - Redirect Most Used FPC RTL Functions to Optimized x86_64 Assembly

     Purpose of this unit is to avoid any direct use of TypInfo.pas RTL unit,
    which is not exactly compatible between compilers, and lack of direct
    RTTI access with no memory allocation. We define pointers to RTTI
    record/object to access TypeInfo() via a set of explicit methods.
     Here fake record/objects are just wrappers around pointers defined in
    Delphi/FPC RTL's TypInfo.pas with the magic of inlining.
     We redefined all RTTI definitions as TRtti* types to avoid confusion
    with type names as published by the TypInfo unit.
     TRttiCustom class is the main cached entry of our customizable RTTI,
    accessible from the global Rtti.* methods.

    See mormot.core.rtti.fpc.inc and mormot.core.rtti.delphi.inc for
    compiler-specific code.

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  contnrs,
  typinfo,  // use official RTL for accurate layouts (especially FPC unaligned)
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text; // ESynException, and text process (e.g. for enums)


{ ************* Low-Level Cross-Compiler RTTI Definitions }

type
  /// the kind of Exception raised by this unit
  ERttiException = class(ESynException);

  /// map TOrdType, to specify ordinal (rkInteger and rkEnumeration) storage size and sign
  // - note: on FPC, Int64 is stored as its own TRttiKind, not as rkInteger
  TRttiOrd = (
    roSByte,
    roUByte,
    roSWord,
    roUWord,
    roSLong,
    roULong
    {$ifdef FPC_NEWRTTI} ,
    roSQWord,
    roUQWord
    {$endif FPC_NEWRTTI});

  /// map TFloatType, to specify floating point (ftFloat) storage size and precision
  TRttiFloat = (
    rfSingle,
    rfDouble,
    rfExtended,
    rfComp,
    rfCurr);

{$ifdef FPC}

  /// map TTypeKind, to specify available type families for FPC RTTI values
  // - FPC types differs from Delphi, and are taken from FPC typinfo.pp unit
  // - here below,  we defined rkLString instead of rkAString to match Delphi -
  // see https://lists.freepascal.org/pipermail/fpc-devel/2013-June/032360.html
  // "Compiler uses internally some LongStrings which is not possible to use
  // for variable declarations" so rkLStringOld seems never used in practice
  TRttiKind = (
    rkUnknown,
    rkInteger,
    rkChar,
    rkEnumeration,
    rkFloat,
    rkSet,
    rkMethod,
    rkSString,
    rkLStringOld {=rkLString},
    rkLString {=rkAString},
    rkWString,
    rkVariant,
    rkArray,
    rkRecord,
    rkInterface,
    rkClass,
    rkObject,
    rkWChar,
    rkBool,
    rkInt64,
    rkQWord,
    rkDynArray,
    rkInterfaceRaw,
    rkProcVar,
    rkUString,
    rkUChar,
    rkHelper,
    rkFile,
    rkClassRef,
    rkPointer);

const
  /// potentially managed types in TRttiKind enumerates
  rkManagedTypes = [rkLStringOld, rkLString, rkWstring, rkUstring, rkArray,
                    rkObject, rkRecord, rkDynArray, rkInterface, rkVariant];

  /// maps record or object in TRttiKind enumerates
  rkRecordTypes = [rkObject, rkRecord];

type
  ///  TTypeKind enumerate as defined in Delphi 6 and up
  // - dkUString and following appear only since Delphi 2009
  TDelphiType = (
    dkUnknown,
    dkInteger,
    dkChar,
    dkEnumeration,
    dkFloat,
    dkString,
    dkSet,
    dkClass,
    dkMethod,
    dkWChar,
    dkLString,
    dkWString,
    dkVariant,
    dkArray,
    dkRecord,
    dkInterface,
    dkInt64,
    dkDynArray,
    dkUString,
    dkClassRef,
    dkPointer,
    dkProcedure);

const
  /// convert our TRttiKind to Delphi's TTypeKind enumerate
  // - used internally for cross-compiler TDynArray binary serialization
  FPCTODELPHI: array[TRttiKind] of TDelphiType = (
    dkUnknown, dkInteger, dkChar, dkEnumeration, dkFloat,
    dkSet, dkMethod, dkString, dkLString, dkLString,
    dkWString, dkVariant, dkArray, dkRecord, dkInterface,
    dkClass, dkRecord, dkWChar, dkEnumeration, dkInt64, dkInt64,
    dkDynArray, dkInterface, dkProcedure, dkUString, dkWChar,
    dkPointer, dkPointer, dkClassRef, dkPointer);

  /// convert Delphi's TTypeKind to our TRttiKind enumerate
  DELPHITOFPC: array[TDelphiType] of TRttiKind = (
    rkUnknown, rkInteger, rkChar, rkEnumeration, rkFloat,
    rkSString, rkSet, rkClass, rkMethod, rkWChar, rkLString, rkWString,
    rkVariant, rkArray, rkRecord, rkInterface, rkInt64, rkDynArray,
    rkUString, rkClassRef, rkPointer, rkProcVar);

{$else}

  /// available type families for Delphi 6 and up, similar to typinfo.pas
  // - redefined here to leverage FPC and Delphi compatibility as much as possible
  TRttiKind = (
    rkUnknown,
    rkInteger,
    rkChar,
    rkEnumeration,
    rkFloat,
    rkSString,
    rkSet,
    rkClass,
    rkMethod,
    rkWChar,
    rkLString,
    rkWString,
    rkVariant,
    rkArray,
    rkRecord,
    rkInterface,
    rkInt64,
    rkDynArray
    {$ifdef UNICODE},
    rkUString,
    rkClassRef,
    rkPointer,
    rkProcedure
    {$endif UNICODE});

const
  /// potentially managed types in TRttiKind enumerates
  rkManagedTypes = [rkLString, rkWstring, {$ifdef UNICODE} rkUstring, {$endif}
                    rkArray, rkRecord, rkDynArray, rkInterface, rkVariant];
  /// maps record or object in TTypeKind RTTI enumerates
  rkRecordTypes = [rkRecord];

{$endif FPC}

  /// maps long string in TRttiKind RTTI enumerates
  rkStringTypes =
    [rkLString, {$ifdef FPC} rkLStringOld, {$endif} rkWString
     {$ifdef HASVARUSTRING} , rkUString {$endif} ];

  /// maps types with proper TRttiProp.RttiOrd field
  // - i.e. rkOrdinalTypes excluding the 64-bit values
  rkHasRttiOrdTypes =
    [rkInteger, rkChar, rkWChar, rkEnumeration, rkSet
     {$ifdef FPC} , rkBool, rkUChar {$endif} ];

  /// maps 1, 8, 16, 32 and 64-bit ordinal in TRttiKind RTTI enumerates
  rkOrdinalTypes =
    rkHasRttiOrdTypes + [{$ifdef FPC} rkQWord, {$endif} rkInt64];

  /// maps values which expect TRttiProp.GetOrdProp/SetOrdProp
  // - includes 32-bit ordinals and pointers
  rkGetOrdPropTypes =
     rkHasRttiOrdTypes + [rkClass, rkDynArray, rkInterface];

  /// maps ordinal values which expect TRttiProp.GetInt64Prop/SetInt64Prop
  // - includes 64-bit ordinals
  rkGetInt64PropTypes =
     [rkInt64 {$ifdef FPC} , rkQWord {$endif} ];

  /// maps records or dynamic arrays
  rkRecordOrDynArrayTypes = rkRecordTypes + [rkDynArray];

  /// maps records or static arrays
  rkRecordOrArrayTypes = rkRecordTypes + [rkArray];

  /// all recognized TRttiKind enumerates, i.e. all but rkUnknown
  rkAllTypes = [succ(low(TRttiKind))..high(TRttiKind)];

  /// quick retrieve how many bytes an ordinal consist in
  ORDTYPE_SIZE: array[TRttiOrd] of byte = (
    1, 1, 2, 2, 4, 4 {$ifdef FPC_NEWRTTI} , 8, 8 {$endif} );

  /// quick retrieve how many bytes a floating-point consist in
  FLOATTYPE_SIZE: array[TRttiFloat] of byte = (
    4, 8, {$ifdef TSYNEXTENDED80} 10 {$else} 8 {$endif}, 8, 8 );


type
  PRttiKind = ^TRttiKind;
  TRttiKinds = set of TRttiKind;
  PRttiOrd = ^TRttiOrd;
  PRttiFloat = ^TRttiFloat;

type
  /// pointer to low-level RTTI of a type definition, as returned by TypeInfo()
  // system function
  // - equivalency to PTypeInfo as defined in TypInfo RTL unit and old mORMot.pas
  // - this is the main entry point of all the information exposed by this unit
  PRttiInfo = ^TRttiInfo;

  /// double-reference to RTTI type definition
  // - Delphi and newer FPC do store all nested TTypeInfo as pointer to pointer,
  // to ease linking of the executable
  PPRttiInfo = ^PRttiInfo;

  /// dynamic array of low-level RTTI type definitions
  PRttiInfoDynArray = array of PRttiInfo;

  /// pointer to a RTTI class property definition as stored in PRttiProps.PropList
  // - equivalency to PPropInfo as defined in TypInfo RTL unit and old mORMot.pas
  PRttiProp = ^TRttiProp;

  /// used to store a chain of properties RTTI
  // - could be used e.g. by TOrmPropInfo to handled flattened properties
  PRttiPropDynArray = array of PRttiProp;

  /// pointer to all RTTI class properties definitions
  // - as returned by PRttiInfo.RttiProps() or GetRttiProps()
  PRttiProps = ^TRttiProps;

  /// a wrapper to published properties of a class, as defined by compiler RTTI
  // - access properties for only a given class level, not inherited properties
  // - start enumeration by getting a PRttiProps with PRttiInfo.RttiProps(), then
  // use P := PropList to get the first PRttiProp, and iterate with P^.Next
  // - this enumeration is very fast and doesn't require any temporary memory,
  //  as in the TypInfo.GetPropInfos() PPropList usage
  // - for TOrm, you should better use the Properties.Fields[] array,
  // which is faster and contains the properties published in parent classes
  TRttiProps = object
  public
    /// number of published properties in this object
    function PropCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// point to a TPropInfo packed array
    // - layout is as such, with variable TPropInfo storage size:
    // ! PropList: array[1..PropCount] of TPropInfo
    // - use TPropInfo.Next to get the next one:
    // ! P := PropList;
    // ! for i := 1 to PropCount do
    // ! begin
    // !   // ... do something with P
    // !   P := P^.Next;
    // ! end;
    function PropList: PRttiProp;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a Field property RTTI information from a Property Name
    function FieldProp(const PropName: shortstring): PRttiProp;
  end;

  /// pointer to TClassType, as returned by PRttiInfo.RttiClass()
  // - as returned by PRttiInfo.RttiClass() or GetRttiClass()
  // - equivalency to PClassData/PClassType as defined in old mORMot.pas
  PRttiClass = ^TRttiClass;

  /// a wrapper to class type information, as defined by the compiler RTTI
  // - get a PRttiClass with PRttiInfo.RttiClass() or GetRttiClass()
  TRttiClass = object
  public
    /// the class type
    function RttiClass: TClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// the parent class type information
    function ParentInfo: PRttiInfo;
      {$ifdef HASINLINE}inline;{$endif}
    /// the number of published properties of this class and all parents
    // - use RttiProps if you want to properties only published in this class 
    function PropCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// the name (without .pas extension) of the unit were the class was defined
    // - then the PRttiProps information follows: use the method
    // RttiProps to retrieve its address
    function UnitName: PShortString;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the information about the published properties of this class
    // - stored after UnitName memory
    function RttiProps: PRttiProps;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast and easy find if this class inherits from a specific class type
    // - you should rather consider using TRttiInfo.InheritsFrom directly
    function InheritsFrom(AClass: TClass): boolean;
  end;

  /// pointer to TEnumType, as returned by PRttiInfo.EnumBaseType/SetEnumType
  // - equivalency to PEnumType as defined in old mORMot.pas
  PRttiEnumType = ^TRttiEnumType;

  /// a wrapper to enumeration type information, as defined by the compiler RTTI
  // and returned by PRttiInfo.EnumBaseType/SetEnumType
  // - we use this to store the enumeration values as integer, but easily provide
  // a text equivalent, translated if necessary, from the enumeration type
  // definition itself
  TRttiEnumType = object
  private
    // as used by TRttiInfo.EnumBaseType/SetBaseType
    function EnumBaseType: PRttiEnumType;
      {$ifdef HASINLINE}inline;{$endif}
    function SetBaseType: PRttiEnumType;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// specify ordinal storage size and sign
    // - is prefered to MaxValue to identify the number of stored bytes
    function RttiOrd: TRttiOrd;
      {$ifdef HASINLINE}inline;{$endif}
    /// first value of enumeration type, typicaly 0
    // - may be < 0 e.g. for boolean
    function MinValue: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// same as ord(high(type)): not the enumeration count, but the highest index
    function MaxValue: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// a concatenation of shortstrings, containing the enumeration names
    // - those shortstrings are not aligned whatsoever (even if
    // FPC_REQUIRES_PROPER_ALIGNMENT is set)
    function NameList: PShortString;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the corresponding enumeration name
    // - return a void '' shortstring if Value is invalid (>MaxValue)
    function GetEnumNameOrd(Value: cardinal): PShortString;
      {$ifdef FPC} inline; {$endif}
    /// get the corresponding enumeration name
    // - return the first one if Value is invalid (>MaxValue)
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetEnumName(const Value): PShortString;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the caption text corresponding to a enumeration name
    // - return the first one if Value is invalid (>MaxValue)
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetCaption(const Value): string;
    /// get all caption names, ready to be display, as lines separated by #13#10
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    // - if UsedValuesBits is not nil, only the corresponding bits set are added
    function GetCaptionStrings(UsedValuesBits: pointer = nil): string;
    /// add caption names, ready to be display, to a TStrings class
    // - add pointer(ord(element)) as Objects[] value
    // - if UsedValuesBits is not nil, only the corresponding bits set are added
    // - can be used e.g. to populate a combo box as such:
    // ! PTypeInfo(TypeInfo(TMyEnum))^.EnumBaseType^.AddCaptionStrings(ComboBox.Items);
    procedure AddCaptionStrings(Strings: TStrings;
      UsedValuesBits: pointer = nil);
    /// retrieve all element names as a dynamic array of RawUtf8
    // - names could be optionally trimmed left from their initial lower chars
    procedure GetEnumNameAll(var result: TRawUtf8DynArray;
      TrimLeftLowerCase: boolean); overload;
    /// retrieve all element names as CSV, with optional quotes
    procedure GetEnumNameAll(out result: RawUtf8; const Prefix: RawUtf8 = '';
      quotedValues: boolean = false; const Suffix: RawUtf8 = '';
      trimedValues: boolean = false; unCamelCased: boolean = false); overload;
    /// retrieve all trimed element names as CSV
    procedure GetEnumNameTrimedAll(var result: RawUtf8; const Prefix: RawUtf8 = '';
      quotedValues: boolean = false; const Suffix: RawUtf8 = '');
    /// get all enumeration names as a JSON array of strings
    function GetEnumNameAllAsJsonArray(TrimLeftLowerCase: boolean;
      UnCamelCased: boolean = false): RawUtf8;
    /// get the corresponding enumeration ordinal value, from its name
    // - if EnumName does start with lowercases 'a'..'z', they will be searched:
    // e.g. GetEnumNameValue('sllWarning') will find sllWarning item
    // - if Value does not start with lowercases 'a'..'z', they will be ignored:
    // e.g. GetEnumNameValue('Warning') will find sllWarning item
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameValue(const EnumName: ShortString): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the corresponding enumeration ordinal value, from its name
    // - if Value does start with lowercases 'a'..'z', they will be searched:
    // e.g. GetEnumNameValue('sllWarning') will find sllWarning item
    // - if Value does not start with lowercases 'a'..'z', they will be ignored:
    // e.g. GetEnumNameValue('Warning') will find sllWarning item
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameValue(Value: PUtf8Char): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the corresponding enumeration ordinal value, from its name
    // - if Value does start with lowercases 'a'..'z', they will be searched:
    // e.g. GetEnumNameValue('sllWarning') will find sllWarning item
    // - if AlsoTrimLowerCase is TRUE, and EnumName does not start with
    // lowercases 'a'..'z', they will be ignored: e.g. GetEnumNameValue('Warning')
    // will find sllWarning item
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameValue(Value: PUtf8Char; ValueLen: integer;
      AlsoTrimLowerCase: boolean = true): integer; overload;
    /// get the corresponding enumeration ordinal value, from its trimmed name
    function GetEnumNameValueTrimmed(Value: PUtf8Char; ValueLen: integer;
      ExactCase: boolean): integer;
    /// get the corresponding enumeration name, without the first lowercase chars
    // (otDone -> 'Done')
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetEnumNameTrimed(const Value): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the enumeration names corresponding to a set value
    function GetSetNameCsv(Value: cardinal; SepChar: AnsiChar = ',';
      FullSetsAsStar: boolean = false): RawUtf8; overload;
    /// get the enumeration names corresponding to a set value
    procedure GetSetNameCsv(W: TBaseWriter; Value: cardinal; SepChar: AnsiChar = ',';
      FullSetsAsStar: boolean = false); overload;
    /// get the corresponding enumeration ordinal value, from its name without
    // its first lowercase chars ('Done' will find otDone e.g.)
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameTrimedValue(const EnumName: ShortString): integer; overload;
    /// get the corresponding enumeration ordinal value, from its name without
    // its first lowercase chars ('Done' will find otDone e.g.)
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameTrimedValue(Value: PUtf8Char; ValueLen: integer = 0): integer; overload;
    /// compute how many bytes this type will use to be stored as a enumerate
    function SizeInStorageAsEnum: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// compute how many bytes (1, 2, 4) this type will use to be stored as a set
    // - consider using TRttiInfo.SetEnumSize if ISFPC32 conditional is defined
    function SizeInStorageAsSet: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// store an enumeration value from its ordinal representation
    procedure SetEnumFromOrdinal(out Value; Ordinal: PtrUInt);
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// RTTI of a record/object type definition (managed) field
  // - defined here since this structure is not available in oldest
  // Delphi's TypInfo.pas
  // - maps TRecordElement in FPC rtti.inc or TManagedField in TypInfo
  TRttiRecordField = record
    /// the RTTI of this managed field
    {$ifdef HASDIRECTTYPEINFO}
    TypeInfo: PRttiInfo;
    {$else}
    TypeInfoRef: PPRttiInfo;
    {$endif HASDIRECTTYPEINFO}
    /// where this managed field starts in the record memory layout
    Offset: PtrUInt;
  end;
  /// pointer to the RTTI of a record/object type definition (managed) field
  PRttiRecordField = ^TRttiRecordField;

  /// define the interface abilities
  TRttiIntfFlag = (
    ifHasGuid,
    ifDispInterface,
    ifDispatch
    {$ifdef FPC} ,
    ifHasStrGUID {$endif});

  /// define the set of interface abilities
  TRttiIntfFlags = set of TRttiIntfFlag;

  /// a wrapper to interface type information, as defined by the the compiler RTTI
  TRttiInterfaceTypeData = object
    /// ancestor interface type
    function IntfParent: PRttiInfo;
      {$ifdef HASINLINE}inline;{$endif}
    /// interface abilities - not inlined to avoid random trouble on FPC trunk
    function IntfFlags: TRttiIntfFlags;
    /// interface 128-bit GUID
    function IntfGuid: PGUID;
      {$ifdef HASINLINE}inline;{$endif}
    /// where the interface has been defined
    function IntfUnit: PShortString;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// pointer to a wrapper to interface type information
  PRttiInterfaceTypeData = ^TRttiInterfaceTypeData;

  /// record RTTI as returned by TRttiInfo.RecordManagedFields
  TRttiRecordManagedFields = record
    /// the record size in bytes
    Size: PtrInt;
    /// how many managed Fields[] are defined in this record
    Count: PtrInt;
    /// points to the first field RTTI
    // - use inc(Fields) to go to the next one
    Fields: PRttiRecordField;
  end;

  /// enhanced RTTI of a record/object type definition
  // - as returned by TRttiInfo.RecordAllFields on Delphi 2010+
  TRttiRecordAllField = record
    /// the field RTTI definition
    TypeInfo: PRttiInfo;
    /// the field offset in the record
    Offset: PtrUInt;
    /// the field property name
    Name: PShortString;
  end;
  PRttiRecordAllField = ^TRttiRecordAllField;

  /// as returned by TRttiInfo.RecordAllFields
  TRttiRecordAllFields = array of TRttiRecordAllField;

  /// quick identification of a RTTI value types
  TRttiCacheFlag = (
    rcfQWord,
    rcfBoolean,
    rcfHasRttiOrd,
    rcfGetOrdProp,
    rcfGetInt64Prop,
    rcfIsRawBlob,
    rcfIsCurrency);

  /// as used by TRttiCache.Flags
  // - rcfQWord/rcfBoolean map Info^.IsQWord/IsBoolean
  // - rcfIsRawBlob/rcfIsCurrency map Info^.IsRawBlob/IsCurrency
  // - set rcfHasRttiOrd/rcfGetOrdProp/rcfGetInt64Prop to access the value
  TRttiCacheFlags = set of TRttiCacheFlag;

  /// convenient wrapper about PRttiInfo content and its more precise information
  // - may be cached between use for more efficient process
  TRttiCache = record
    /// the associated RTTI TypeInfo()
    Info: PRttiInfo;
    /// the size in bytes of a value of this type - equals Info^.RttiSize
    Size: integer;
    /// equals Info^.Kind
    Kind: TRttiKind;
    /// quick identification of specific types, e.g. rkOrdinalTypes
    Flags: TRttiCacheFlags;
    /// for rkHasRttiOrdTypes/rcfHasRttiOrd, equals Info^.RttiOrd
    RttiOrd: TRttiOrd;
    /// corresponding TRttiVarData.VType
    // - rkEnumeration,rkSet,rkDynArray,rkClass,rkInterface,rkRecord,rkArray are
    // identified as varAny with TVarData.VAny pointing to the actual value, and
    // will be handled as expected by TTextWriter.AddRttiVarData
    RttiVarDataVType: cardinal;
    /// type-specific information
    case TRttiKind of
      rkFloat: (
        RttiFloat: TRttiFloat);
      rkLString: ( // from TypeInfo() on older Delphi with no CP RTTI
        CodePage: cardinal; // RawBlob=CP_RAWBYTESTRING not CP_RAWBLOB
        Engine: TSynAnsiConvert);
      rkEnumeration, rkSet: (
        EnumMax:  cardinal;
        EnumInfo: PRttiEnumType;
        EnumList: PShortString);
      rkDynArray, rkArray: (
        ItemInfo: PRttiInfo;
        ItemSize: integer;
        ItemCount: integer; // rkArray only
      );
  end;

  /// map extended PRttiInfo content
  PRttiCache = ^TRttiCache;

  {$A-}

  /// main entry-point wrapper to access RTTI for a given pascal type
  // - as returned by the TypeInfo() low-level compiler function
  // - other RTTI objects can be computed from a pointer to this structure
  // - user types defined as an alias don't have this type information:
  // ! type
  // !   TNewType = TOldType;
  // here TypeInfo(TNewType) = TypeInfo(TOldType)
  // - user types defined as new types have this type information:
  // ! type
  // !   TNewType = type TOldType;
  // here TypeInfo(TNewType) <> TypeInfo(TOldType)
  TRttiInfo = object
  public
    /// the value type family
    // - not defined as an inlined function, since first field is always aligned
    Kind: TRttiKind;
    /// the declared name of the type ('String','Word','RawUnicode'...)
    // - won't adjust internal/cardinal names on FPC as with Name method
    RawName: ShortString;
    /// the declared name of the type ('String','Word','RawUnicode'...)
    // - will return '' if @self is nil
    // - on FPC, will adjust 'integer'/'cardinal' from 'longint'/'longword' RTTI
    function Name: PShortString;
      {$ifdef ISDELPHI2006ANDUP}inline;{$endif}
    /// efficiently finalize any (managed) type value
    // - do nothing for unmanaged types (e.g. integer)
    // - if you are sure that your type is managed, you may call directly
    // $ RTTI_FINALIZE[Info^.Kind](Data, Info);
    procedure Clear(Data: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// efficiently copy any (managed) type value
    // - do nothing for unmanaged types (e.g. integer)
    // - if you are sure that your type is managed, you may call directly
    // $ RTTI_COPY[Info^.Kind](Dest, Source, Info);
    procedure Copy(Dest, Source: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// compute extended information about this RTTI type
    procedure ComputeCache(out Cache: TRttiCache);
    /// for ordinal types, get the storage size and sign
    function RttiOrd: TRttiOrd;
      {$ifdef HASINLINE}inline;{$endif}
    /// return TRUE if the property is an unsigned 64-bit field (QWord/UInt64)
    function IsQWord: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// return TRUE if the property is a boolean field
    function IsBoolean: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// return TRUE if the property is a currency field
    function IsCurrency: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// return true if this property is a BLOB (RawBlob)
    function IsRawBlob: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkFloat: get the storage size and precision
    // - will also properly detect our currency internal type as rfCurr
    function RttiFloat: TRttiFloat;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkEnumeration: get the enumeration type information
    function EnumBaseType: PRttiEnumType; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkEnumeration: get the enumeration values information
    function EnumBaseType(out NameList: PShortString;
      out Max: integer): PRttiEnumType; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkSet: get the type information of its associated enumeration
    function SetEnumType: PRttiEnumType; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkSet: get the associated enumeration values information
    function SetEnumType(out NameList: PShortString;
      out Max: integer): PRttiEnumType; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkSet: in how many bytes this type is stored
    // - is very efficient on latest FPC only - i.e. ifdef ISFPC32
    function SetEnumSize: PtrInt; {$ifdef ISFPC32} inline; {$endif}
    /// compute in how many bytes this type is stored
    // - will use Kind (and RttiOrd/RttiFloat) to return the exact value
    function RttiSize: PtrInt;
    /// check if this type is a managed type, or has any managed field
    // - will also check for the nested fields e.g. for rkRecordTypes
    function IsManaged: boolean;
    /// for rkRecordTypes: get the record size
    // - returns 0 if the type is not a record/object
    function RecordSize: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkRecordTypes: retrieve RTTI information about all managed fields
    // of this record
    // - non managed fields (e.g. integers, double...) are not listed here
    // - also includes the total record size in bytes
    // - caller should ensure the type is indeed a record/object
    // - note: if FPC_OLDRTTI is defined, unmanaged fields are included
    procedure RecordManagedFields(out Fields: TRttiRecordManagedFields);
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkRecordTypes: check if this record as any managed fields
    function RecordManagedFieldsCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkRecordTypes: retrieve enhanced RTTI information about all fields
    // of this record, for JSON serialization without text definition
    // - this information is currently only available since Delphi 2010
    // - if any field has no RTTI (e.g. a static array of unmanaged type), then
    // it will ignore this uncomplete, therefore non-useful RTTI
    // - in practice, it may be a good habit to always define the records used
    // within the SOA (e.g. as DTOs) calling RegisterFromText, and don't rely on
    // this RTTI, since it will be more cross-platform, and more customizable
    function RecordAllFields(out RecSize: PtrInt): TRttiRecordAllFields;
    /// for rkDynArray: get the dynamic array standard RTTI of the stored item
    // - returns nil if the item has no managed field
    // - caller should ensure the type is indeed a dynamic array
    function DynArrayItemType: PRttiInfo; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkDynArray: get the dynamic array deep RTTI of the stored item
    // - works for both managed and unmanaged types, on FPC and Delphi 2010+
    // - caller should ensure the type is indeed a dynamic array
    function DynArrayItemTypeExtended: PRttiInfo;
    /// for rkDynArray: get the dynamic array type information of the stored item
    // - this overloaded method will also return the item size in bytes
    // - caller should ensure the type is indeed a dynamic array
    function DynArrayItemType(out aDataSize: PtrInt): PRttiInfo; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkDynArray: get the dynamic array size (in bytes) of the stored item
    function DynArrayItemSize: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkArray: get the static array type information of the stored item
    // - returns nil if the array type is unmanaged (i.e. behave like Delphi)
    // - aDataSize is the size in bytes of all aDataCount static items (not
    // the size of each item)
    // - caller should ensure the type is indeed a static array
    function ArrayItemType(out aDataCount, aDataSize: PtrInt): PRttiInfo;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkArray: get the size in bytes of all the static array items
    // - caller should ensure the type is indeed a static array
    function ArraySize: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// recognize most used string types, returning their code page
    // - will return the exact code page on FPC and since Delphi 2009, from RTTI
    // - for non Unicode versions of Delphi, will recognize WinAnsiString as
    // CODEPAGE_US, RawUnicode as CP_UTF16, RawByteString as CP_RAWBYTESTRING,
    // AnsiString as 0, and any other type as RawUtf8
    // - it will also recognize RawBlob as the fake CP_RAWBLOB codepage
    function AnsiStringCodePage: integer;
      {$ifdef HASCODEPAGE}inline;{$endif}
    {$ifdef HASCODEPAGE}
    /// returning the code page stored in the RTTI
    // - without recognizing e.g. RawBlob
    // - caller should ensure the type is indeed a rkLString
    function AnsiStringCodePageStored: integer; inline;
    {$endif HASCODEPAGE}
    /// retrieve rkLString, rkSString, rkUString, rkWString, rkChar, rkWChar
    // values as RawUtf8, from a pointer to its memory storage
    // - makes heap allocations and encoding conversion, so may be slow
    procedure StringToUtf8(Data: pointer; var Value: RawUtf8);
    /// for rkClass: get the class type information
    function RttiClass: PRttiClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkClass: get the class type information
    function RttiNonVoidClass: PRttiClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkClass: return the number of published properties in this class
    // - you can count the plain fields without any getter function, if you
    // do need only the published properties corresponding to some value
    // actually stored, and ignore e.g. any textual conversion
    function ClassFieldCount(onlyWithoutGetter: boolean): integer;
    /// for rkClass: fast and easy check if a class inherits from this RTTI
    function InheritsFrom(AClass: TClass): boolean;
    /// for rkInterface: get the interface type information
    function InterfaceType: PRttiInterfaceTypeData;
      {$ifdef HASINLINE}inline;{$endif}
    /// for rkInterface: get the TGUID of a given interface type information
    // - returns nil if this type is not an interface
    function InterfaceGuid: PGUID;
    /// for rkInterface: get the unit name of a given interface type information
    // - returns '' if this type is not an interface
    function InterfaceUnitName: PShortString;
    /// for rkInterface: get the ancestor/parent of a given interface type information
    // - returns nil if this type has no parent
    function InterfaceAncestor: PRttiInfo;
    /// for rkInterface: get all ancestors/parents of a given interface type information
    // - only ancestors with an associated TGUID will be added
    // - if OnlyImplementedBy is not nil, only the interface explicitly
    // implemented by this class will be added, and AncestorsImplementedEntry[]
    // will contain the corresponding PInterfaceEntry values
    procedure InterfaceAncestors(out Ancestors: PRttiInfoDynArray;
      OnlyImplementedBy: TInterfacedObjectClass;
      out AncestorsImplementedEntry: TPointerDynArray);
  end;

  {$A+}

  /// how a RTTI property definition access its value
  // - as returned by TPropInfo.Getter/Setter/GetterIs/SetterIs methods
  TRttiPropCall = (
    rpcNone,
    rpcField,
    rpcMethod,
    rpcIndexed);

  /// a wrapper containing a RTTI class property definition
  // - used for direct Delphi / UTF-8 SQL type mapping/conversion
  // - doesn't depend on RTL's TypInfo unit, to enhance cross-compiler support
  TRttiProp = object
  public
    /// raw retrieval of the property read access definition
    // - note: 'var Call' generated incorrect code on Delphi XE4 -> use PMethod
    function Getter(Instance: TObject; Call: PMethod): TRttiPropCall;
      {$ifdef HASINLINE}inline;{$endif}
    /// raw retrieval of the property access definition
    function Setter(Instance: TObject; Call: PMethod): TRttiPropCall;
      {$ifdef HASINLINE}inline;{$endif}
    /// raw retrieval of rkInteger,rkEnumeration,rkSet,rkChar,rkWChar,rkBool
    // - rather call GetOrdValue/GetInt64Value
    // - returns an Int64 to properly support cardinal values
    function GetOrdProp(Instance: TObject): Int64;
    /// raw assignment of rkInteger,rkEnumeration,rkSet,rkChar,rkWChar,rkBool
    // - rather call SetOrdValue/SetInt64Value
    procedure SetOrdProp(Instance: TObject; Value: PtrInt);
    /// raw retrieval of rkClass
    function GetObjProp(Instance: TObject): TObject;
    /// raw retrieval of rkInt64, rkQWord
    // - rather call GetInt64Value
    function GetInt64Prop(Instance: TObject): Int64;
    /// raw assignment of rkInt64, rkQWord
    // - rather call SetInt64Value
    procedure SetInt64Prop(Instance: TObject; const Value: Int64);
    /// raw retrieval of rkLString
    procedure GetLongStrProp(Instance: TObject; var Value: RawByteString);
    /// raw assignment of rkLString
    procedure SetLongStrProp(Instance: TObject; const Value: RawByteString);
    /// raw copy of rkLString
    procedure CopyLongStrProp(Source,Dest: TObject);
    /// raw retrieval of rkString into an Ansi7String
    procedure GetShortStrProp(Instance: TObject; var Value: RawUtf8);
    /// raw retrieval of rkWString
    procedure GetWideStrProp(Instance: TObject; var Value: WideString);
    /// raw assignment of rkWString
    procedure SetWideStrProp(Instance: TObject; const Value: WideString);
    {$ifdef HASVARUSTRING}
    /// raw retrieval of rkUString
    procedure GetUnicodeStrProp(Instance: TObject; var Value: UnicodeString);
    /// raw assignment of rkUString
    procedure SetUnicodeStrProp(Instance: TObject; const Value: UnicodeString);
    {$endif HASVARUSTRING}
    /// raw retrieval of rkFloat/currency
    // - use instead GetCurrencyValue
    procedure GetCurrencyProp(Instance: TObject; var Value: currency);
    /// raw assignment of rkFloat/currency
    procedure SetCurrencyProp(Instance: TObject; const Value: currency);
    /// raw retrieval of rkFloat/double
    function GetDoubleProp(Instance: TObject): double;
    /// raw assignment of rkFloat/double
    procedure SetDoubleProp(Instance: TObject; Value: Double);
    /// raw retrieval of rkFloat - with conversion to 64-bit double
    // - use instead GetDoubleValue
    function GetFloatProp(Instance: TObject): double;
    /// raw assignment of rkFloat
    // - use instead SetDoubleValue
    procedure SetFloatProp(Instance: TObject; Value: TSynExtended);
    /// raw retrieval of rkVariant
    // - will use varByRef from the field address if SetByRef is true
    procedure GetVariantProp(Instance: TObject; var Result: Variant; SetByRef: boolean);
    /// raw assignment of rkVariant
    procedure SetVariantProp(Instance: TObject; const Value: Variant);
    /// raw retrieval of the 'stored' flag using getter
    /// - called by IsStored when inlined
    function GetIsStored(Instance: TObject): boolean;
  public
    /// contains the index value of an indexed class data property
    // - outside SQLite3, this can be used to define a VARCHAR() length value
    // for the textual field definition (sftUtf8Text/sftAnsiText); e.g.
    // the following will create a NAME VARCHAR(40) field:
    // ! Name: RawUtf8 index 40 read fName write fName;
    // - is used by a dynamic array property for fast usage of the
    // TOrm.DynArray(DynArrayFieldIndex) method
    function Index: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// contains the default value for an ordinal or set property
    // - NO_DEFAULT=$80000000 indicates none was defined in source code
    // - see also TPropInfo.DefaultOr0
    function Default: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the Default RTTI value defined for this property, or 0 if not set
    function DefaultOr0: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// index of the property in the current inherited class definition
    // - first name index at a given class level is 0
    // - index is reset to 0 at every inherited class level
    function NameIndex: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// the property Name, directly returned from RTTI
    function Name: PShortString;
      {$ifdef HASINLINE}inline;{$endif}
    /// the property Name, converted as a RawUtf8
    function NameUtf8: RawUtf8;
    /// the type information of this property
    // - will de-reference the PropType pointer on Delphi and newer FPC compilers
    function TypeInfo: PRttiInfo;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the next property information
    // - no range check: use RttiProps()^.PropCount to determine the properties count
    // - get the first PRttiProp with RttiProps()^.PropList
    function Next: PRttiProp;
      {$ifdef HASINLINE}inline;{$endif}
    /// return FALSE (AS_UNIQUE) if was marked as "stored AS_UNIQUE"
    //  (i.e. "stored false"), or TRUE by default
    // - if Instance=nil, will work only at RTTI level, not with field or method
    // (and will return TRUE if nothing is defined in the RTTI)
    function IsStored(Instance: TObject): boolean;
      {$ifdef FPC} inline; {$endif}
    /// return true if this property is a BLOB (RawBlob)
    function IsRawBlob: boolean;
      {$ifdef FPC} inline; {$endif}
    /// compute in how many bytes this property is stored
    function FieldSize: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// return TRUE if the property has no getter but direct field read
    // - returns FALSE if no "read" attribute was specified: use GetterCall
    // if you want to mimic how Get*() methods could use the "write" field
    function GetterIsField: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// return TRUE if the property has no setter but direct field write
    // - returns FALSE if no "write" attribute is specified: use SetterCall
    // if you want to mimic how Set*() methods could use the "read" field
    function SetterIsField: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns how a property should be retrieved
    // - no "read" attribute specified will return rpcField if "write" is a
    // direct field access - just like any Get*() method would do
    function GetterCall: TRttiPropCall;
    /// returns how a property should be set
    // - no "write" attribute specified will return rpcField if "read" is a
    // direct field access - just like any Set*() method would do
    function SetterCall: TRttiPropCall;
    /// return TRUE if the property has a write setter or direct field
    function WriteIsDefined: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the low-level field read address, if GetterIsField is TRUE
    function GetterAddr(Instance: pointer): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the low-level field write address, if SetterIsField is TRUE
    function SetterAddr(Instance: pointer): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level getter of the field value memory pointer
    // - return NIL if both getter and setter are methods
    function GetFieldAddr(Instance: TObject): pointer;
      {$ifdef HASINLINE}inline;{$endif}

    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - returns an Int64 to properly support cardinal values
    // - return -1 on any error
    function GetOrdValue(Instance: TObject): Int64;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - ordinal properties smaller than rkInt64 will return an Int64-converted
    // value (e.g. rkInteger)
    // - return 0 on any error
    function GetInt64Value(Instance: TObject): Int64;
    /// low-level getter of the currency property value of a given instance
    // - this method will check if the corresponding property is exactly currency
    // - return 0 on any error
    procedure GetCurrencyValue(Instance: TObject; var Value: currency);
    /// low-level getter of the floating-point property value of a given instance
    // - this method will check if the corresponding property is floating-point
    // - return 0 on any error
    function GetDoubleValue(Instance: TObject): double;
    /// low-level setter of the floating-point property value of a given instance
    // - this method will check if the corresponding property is floating-point
    procedure SetDoubleValue(Instance: TObject; const Value: double);
    /// low-level getter of the long string property content of a given instance
    // - just a wrapper around low-level GetLongStrProp() function
    // - call GetLongStrValue() method if you want a conversion into RawUtf8
    // - will work only for Kind=rkLString
    procedure GetRawByteStringValue(Instance: TObject; var Value: RawByteString);
    /// low-level setter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    procedure SetOrdValue(Instance: TObject; Value: PtrInt);
    /// low-level setter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    procedure SetInt64Value(Instance: TObject; Value: Int64);
    {$ifdef HASVARUSTRING}
    /// low-level setter of the Unicode string property value of a given instance
    // - this method will check if the corresponding property is a Unicode String
    procedure SetUnicodeStrValue(Instance: TObject; const Value: UnicodeString);
    /// low-level getter of the Unicode string property value of a given instance
    // - this method will check if the corresponding property is a Unicode String
    function GetUnicodeStrValue(Instance: TObject): UnicodeString;
    {$endif HASVARUSTRING}
    /// retrieve rkLString, rkSString, rkUString, rkWString, rkChar, rkWChar as RawUtf8
    // - this would make heap allocations and encoding conversion, so may be slow
    procedure GetAsString(Instance: TObject; var Value: RawUtf8);
    /// set rkLString, rkSString, rkUString, rkWString, rkChar, rkWChar from
    // a RawUtf8 value
    // - this would make heap allocations and encoding conversion, so may be slow
    function SetAsString(Instance: TObject; const Value: RawUtf8): boolean;
    /// set a property value from a variant value
    // - to be called when a setter is involved - not very fast, but safe
    function SetValue(Instance: TObject; const Value: variant): boolean;
  end;

const
  NO_DEFAULT = integer($80000000);

/// retrieve the text name of one TRttiKind enumerate
function ToText(k: TRttiKind): PShortString; overload;

/// convert an ordinal value from its (signed) pointer-sized integer representation
function FromRttiOrd(o: TRttiOrd; P: pointer): Int64;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an ordinal value into its (signed) pointer-sized integer representation
procedure ToRttiOrd(o: TRttiOrd; P: pointer; Value: PtrInt);
  {$ifdef HASINLINE}inline;{$endif}


{$ifdef HASINLINE}
// some functions which should be defined here for proper inlining

{$ifdef FPC}

{$ifndef HASDIRECTTYPEINFO}
function Deref(Info: pointer): pointer; inline;
{$endif HASDIRECTTYPEINFO}

{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
function AlignToPtr(p: pointer): pointer; inline;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

{$endif FPC}

type
  // redefined here for proper Delphi inlining
  PTypeData = type typinfo.PTypeData;
  TPropInfo = type typinfo.TPropInfo;
  PPropInfo = type typinfo.PPropInfo;

/// efficiently inlined low-level function to retrieve raw RTTI structure
function GetTypeData(TypeInfo: pointer): PTypeData; inline;

{$endif HASINLINE}

{$ifdef ISDELPHI}// Delphi requires those definitions for proper inlining

const
  NO_INDEX = integer($80000000);

  ptField = $ff;
  ptVirtual = $fe;

type
  /// used to map a TPropInfo.GetProc/SetProc and retrieve its kind
  // - defined here for proper Delphi inlining
  PropWrap = packed record
    FillBytes: array [0 .. SizeOf(Pointer) - 2] of byte;
    /// =$ff for a ptField address, or =$fe for a ptVirtual method
    Kind: byte;
  end;

  /// PPropData not defined in Delphi 7/2007 TypInfo
  // - defined here for proper Delphi inlining
  TPropData = packed record
    PropCount: word;
    PropList: record end;
  end;
  PPropData = ^TPropData;

  /// rkRecord RTTI is not defined in Delphi 7/2007 TTypeData
  // - defined here for proper Delphi inlining
  TRecordInfo = packed record
    RecSize: integer;
    ManagedFldCount: integer;
  end;
  PRecordInfo = ^TRecordInfo;

  /// rkArray RTTI not defined in Delphi 7/2007 TTypeData
  // - defined here for proper Delphi inlining
  TArrayInfo = packed record
    ArraySize: integer;
    ElCount: integer;
    ArrayType: PPRttiInfo;
    DimCount: byte;
    Dims: array[0..255 {DimCount-1}] of PPRttiInfo;
  end;
  PArrayInfo = ^TArrayInfo;

{$endif ISDELPHI}


{ **************** Published Class Properties and Methods RTTI }

/// retrieve the class RTTI information for a specific class
function GetRttiClass(RttiClass: TClass): PRttiClass;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the class property RTTI information for a specific class
function GetRttiProps(RttiClass: TClass): PRttiProps;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the class property RTTI information for a specific class
// - will return the number of published properties
// - and set the PropInfo variable to point to the first property
// - typical use to enumerate all published properties could be:
//  !  var i: integer;
//  !      CT: TClass;
//  !      P: PRttiProp;
//  !  begin
//  !    CT := ..;
//  !    repeat
//  !      for i := 1 to GetRttiProp(CT,P) do
// !       begin
//  !        // use P^
//  !        P := P^.Next;
//  !      end;
//  !      CT := GetClassParent(CT);
//  !    until CT=nil;
//  !  end;
// such a loop is much faster than using the RTL's TypeInfo or RTTI units
function GetRttiProp(C: TClass; out PropInfo: PRttiProp): integer;

/// retrieve a Field property RTTI information from a Property Name
function ClassFieldProp(ClassType: TClass; const PropName: shortstring): PRttiProp;

/// retrieve a Field property RTTI information from a Property Name
// - this special version also searches into parent properties
// (TRttiProp search scope is only inside the current class level)
function ClassFieldPropWithParents(aClassType: TClass; const aPropName: shortstring;
  aCaseSensitive: boolean = false): PRttiProp;

/// retrieve an integer/Int64 Field propery value from a Property Name
// - this special version also searches into parent properties
// (TRttiProp search scope is only inside the current class level)
// - returns TRUE and set PropValue if a matching property was found
function ClassFieldInt64(Instance: TObject; const PropName: ShortString;
  out PropValue: Int64): boolean;

/// retrieve a class Field property instance from a Property Name
// - this special version also searches into parent properties
// (TRttiProp search scope is only inside the current class level)
// - returns TRUE and set PropInstance if a matching property was found
function ClassFieldInstance(Instance: TObject; const PropName: shortstring;
  PropClassType: TClass; out PropInstance): boolean; overload;

/// retrieve a Field property RTTI information from a Property Name
// - this special version also searches into parent properties
// (TRttiProp search scope is only inside the current class level)
function ClassFieldPropWithParentsFromUtf8(aClassType: TClass; PropName: PUtf8Char;
  PropNameLen: integer; aCaseSensitive: boolean = false): PRttiProp;

/// retrieve a Field property RTTI information searching for an exact
// Property class type
// - this special version also searches into parent properties
function ClassFieldPropWithParentsFromClassType(aClassType,
  aSearchedClassType: TClass): PRttiProp;

/// retrieve a Field property RTTI information searching for an inherited
// Property class type
// - this special version also searches into parent properties
function ClassFieldPropWithParentsInheritsFromClassType(aClassType,
  aSearchedClassType: TClass): PRttiProp;

/// retrieve a Field property RTTI information searching for an exact
// Property offset address
// - this special version also searches into parent properties
function ClassFieldPropWithParentsFromClassOffset(aClassType: TClass;
  aSearchedOffset: pointer): PRttiProp;

/// retrieve a class Field property instance from a Property class type
// - this version also searches into parent properties
// - returns TRUE and set PropInstance if a matching property was found
function ClassFieldInstance(Instance: TObject; PropClassType: TClass;
  out PropInstance): boolean; overload;

/// retrieve all class Field property instances from a Property class type
// - this version also searches into parent properties
// - returns all matching property instances found
function ClassFieldInstances(Instance: TObject;
  PropClassType: TClass): TObjectDynArray;

/// retrieve a class instance property value matching a class type
// - if aSearchedInstance is aSearchedClassType, will return aSearchedInstance
// - if aSearchedInstance is not aSearchedClassType, it will try all nested
// properties of aSearchedInstance for a matching aSearchedClassType: if no
// exact match is found, will return aSearchedInstance
function ClassFieldPropInstanceMatchingClass(aSearchedInstance: TObject;
  aSearchedClassType: TClass): TObject;

/// retrieve the total number of properties for a class, including its parents
function ClassFieldCountWithParents(ClassType: TClass;
  onlyWithoutGetter: boolean = false): integer;

/// returns TRUE if the class has some published fields, including its parents
function ClassHasPublishedFields(ClassType: TClass): boolean;

/// retrieve all class hierachy types which have some published properties
function ClassHierarchyWithField(ClassType: TClass): TClassDynArray;

/// retrieve the PRttiProp values of all published properties of a class
// - you could select which property types should be included in the list
function ClassFieldAllProps(ClassType: TClass;
  Types: TRttiKinds = [low(TRttiKind)..high(TRttiKind)]): PRttiPropDynArray;

/// retrieve the field names of all published properties of a class
// - will optionally append the property type to the name, e.g 'Age: integer'
// - you could select which property types should be included in the list
function ClassFieldNamesAllProps(
  ClassType: TClass; IncludePropType: boolean = false;
  Types: TRttiKinds = [low(TRttiKind)..high(TRttiKind)]): TRawUtf8DynArray;

/// retrieve the field names of all published properties of a class
// - will optionally append the property type to the name, e.g 'Age: integer'
// - you could select which property types should be included in the list
function ClassFieldNamesAllPropsAsText(
  ClassType: TClass; IncludePropType: boolean = false;
  Types: TRttiKinds = [low(TRttiKind)..high(TRttiKind)]): RawUtf8;


type
  /// information about one method, as returned by GetPublishedMethods
  TPublishedMethodInfo = record
    /// the method name
    Name: RawUtf8;
    /// a callback to the method, for the given class instance
    Method: TMethod;
  end;
  /// information about all methods, as returned by GetPublishedMethods
  TPublishedMethodInfoDynArray = array of TPublishedMethodInfo;

/// retrieve published methods information about any class instance
// - will optionaly accept a Class, in this case Instance is ignored
// - will work with FPC and Delphi RTTI
function GetPublishedMethods(Instance: TObject;
  out Methods: TPublishedMethodInfoDynArray; aClass: TClass = nil): integer;


/// copy object properties
// - copy integer, Int64, enumerates (including boolean), variant, records,
// dynamic arrays, classes and any string properties (excluding shortstring)
// - TCollection items can be copied also, if they are of the same exact class
// - object properties instances are created in aTo if the objects are not
// TOrm children (in this case, these are not class instances, but
// INTEGER reference to records, so only the integer value is copied), that is
// for regular classes
procedure CopyObject(aFrom, aTo: TObject); overload;

/// create a new object instance, from an existing one
// - will create a new instance of the same class, then call the overloaded
// CopyObject() procedure to copy its values
function CopyObject(aFrom: TObject): TObject; overload;

/// copy two TStrings instances
// - will just call Dest.Assign(Source) in practice
procedure CopyStrings(Source, Dest: TStrings);

/// copy two TCollection instances
// - will call CopyObject() in loop to repopulate the Dest collection,
// which will work even if Assign() method was not overriden
procedure CopyCollection(Source, Dest: TCollection);

/// set any default integer or enumerates (including boolean) published
// properties values for a TPersistent/TSynPersistent
// - set only the values set as "property ... default ..." at class type level
// - will also reset the published properties of the nested classes
procedure SetDefaultValuesObject(Value: TObject);

/// returns TRUE on a nil instance or if all its published properties are default/0
// - calls internally TPropInfo.IsDefaultOrVoid()
function IsObjectDefaultOrVoid(Value: TObject): boolean;

/// will reset all the object properties to their default
// - strings will be set to '', numbers to 0
// - if FreeAndNilNestedObjects is the default FALSE, will recursively reset
// all nested class properties values
// - if FreeAndNilNestedObjects is TRUE, will FreeAndNil() all the nested
// class properties
// - for a TOrm, use its ClearProperties method instead, which will
// handle the ID property, and any nested JOINed instances
procedure ClearObject(Value: TObject; FreeAndNilNestedObjects: boolean = false);



{ *************** Enumerations RTTI }

/// helper to retrieve low-level RTTI information of an enumeration type
// - just a wrapper around
// $ aTypeInfo^.EnumBaseType(List, result);
function GetEnumType(aTypeInfo: PRttiInfo; out List: PShortString): integer;

/// helper to retrieve the text of an enumerate item
// - just a wrapper around
// $ aTypeInfo^.EnumBaseType.GetEnumNameOrd(aIndex)
function GetEnumName(aTypeInfo: PRttiInfo; aIndex: integer): PShortString;

/// get the corresponding enumeration name, without the first lowercase chars
// (otDone -> 'Done')
// - this will return the code-based English text; use GetEnumCaption() to
// retrieve the enumeration display text
function GetEnumNameTrimed(aTypeInfo: PRttiInfo; aIndex: integer): RawUtf8;

/// helper to retrieve all texts of an enumerate
// - may be used as cache for overloaded ToText() content
procedure GetEnumNames(aTypeInfo: PRttiInfo; aDest: PPShortString);

/// helper to retrieve all trimmed texts of an enumerate
// - may be used as cache to retrieve UTF-8 text without lowercase 'a'..'z' chars
procedure GetEnumTrimmedNames(aTypeInfo: PRttiInfo; aDest: PRawUtf8); overload;

/// helper to retrieve all trimmed texts of an enumerate as UTF-8 strings
function GetEnumTrimmedNames(aTypeInfo: PRttiInfo): TRawUtf8DynArray; overload;

/// helper to retrieve the index of an enumerate item from its text
// - returns -1 if aValue was not found
// - will search for the exact text and also trim the lowercase 'a'..'z' chars on
// left side of the text if no exact match is found and AlsoTrimLowerCase is TRUE
function GetEnumNameValue(aTypeInfo: PRttiInfo; aValue: PUtf8Char; aValueLen: PtrInt;
  AlsoTrimLowerCase: boolean = false): integer; overload;

/// retrieve the index of an enumerate item from its left-trimmed text
// - text comparison is case-insensitive for A-Z characters
// - will trim the lowercase 'a'..'z' chars on left side of the supplied aValue text
// - returns -1 if aValue was not found
function GetEnumNameValueTrimmed(aTypeInfo: PRttiInfo;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;

/// retrieve the index of an enumerate item from its left-trimmed text
// - text comparison is case-sensitive for A-Z characters
// - will trim the lowercase 'a'..'z' chars on left side of the supplied aValue text
// - returns -1 if aValue was not found
function GetEnumNameValueTrimmedExact(aTypeInfo: PRttiInfo;
  aValue: PUtf8Char; aValueLen: PtrInt): integer;

/// helper to retrieve the index of an enumerate item from its text
function GetEnumNameValue(aTypeInfo: PRttiInfo; const aValue: RawUtf8;
  AlsoTrimLowerCase: boolean = false): integer; overload;

/// store an enumeration value from its ordinal representation
procedure SetEnumFromOrdinal(aTypeInfo: PRttiInfo; out Value; Ordinal: PtrUInt);

/// helper to retrieve the CSV text of all enumerate items defined in a set
function GetSetName(aTypeInfo: PRttiInfo; const value): RawUtf8;

/// helper to retrieve the CSV text of all enumerate items defined in a set
procedure GetSetNameShort(aTypeInfo: PRttiInfo; const value; out result: ShortString;
  trimlowercase: boolean = false);

/// helper to retrieve all (translated) caption texts of an enumerate
// - may be used as cache for overloaded ToCaption() content
procedure GetEnumCaptions(aTypeInfo: PRttiInfo; aDest: PString);

/// UnCamelCase and translate the enumeration item
function GetCaptionFromEnum(aTypeInfo: PRttiInfo; aIndex: integer): string;

/// low-level helper to retrieve a (translated) caption from a PShortString
// - as used e.g. by GetEnumCaptions or GetCaptionFromEnum
procedure GetCaptionFromTrimmed(PS: PShortString; var result: string);

/// will get a class name as UTF-8
// - will trim 'T', 'TSyn', 'TSql' or 'TOrm' left side of the class name
// - will encode the class name as UTF-8 (for Unicode Delphi versions)
// - is used e.g. to extract the SQL table name for a TOrm class
function GetDisplayNameFromClass(C: TClass): RawUtf8;

///  UnCamelCase and translate the class name, triming any left 'T', 'TSyn',
// 'TSql' or 'TOrm'
// - return generic VCL string type, i.e. UnicodeString for Delphi 2009+
function GetCaptionFromClass(C: TClass): string;

/// defined here to avoid circular dependency in mormot.core.os.pas
function ToText(cmd: TParseCommands): shortstring; overload;


{ ***************** IInvokable Interface RTTI }

type
  /// handled kind of parameters direction for an interface method
  // - IN, IN/OUT, OUT directions can be applied to arguments, e.g. to be
  // available through our JSON-serialized remote access: rmdVar and rmdOut
  // kind of parameters will be returned within the "result": JSON array
  // - rmdResult is used for a function method, to handle the returned value
  TRttiMethodArgDirection = (
    rmdConst,
    rmdVar,
    rmdOut,
    rmdResult);

  /// set of parameter directions e.g. for an interface-based service method
  TRttiMethodArgDirections = set of TRttiMethodArgDirection;

  TRttiMethodArg = record
    /// the argument name, as declared in pascal code
    ParamName: PShortString;
    /// the type name, as declared in pascal code
    TypeName: PShortString;
    /// the low-level RTTI information of this argument
    TypeInfo: PRttiInfo;
    /// how the parameter has been defined (const/var/out/result)
    Direction: TRttiMethodArgDirection;
  end;
  PRttiMethodArg = ^TRttiMethodArg;

  /// store IInvokable method information
  TRttiMethod = record
    /// the method name, e.g. 'Add' for ICalculator.Add
    Name: RawUtf8;
    /// 0 for the root interface, >0 for inherited interfaces
    HierarchyLevel: integer;
    /// the method arguments
    Args: array of TRttiMethodArg;
    /// if this method is a function, i.e. expects a result
    IsFunction: boolean;
  end;
  PRttiMethod = ^TRttiMethod;

  /// store IInvokable methods information
  TRttiInterface = record
    /// the interface name, e.g. 'ICalculator'
    Name: RawUtf8;
    /// the unit where the interface was defined
    UnitName: RawUtf8;
    /// the associated GUID of this interface
    Guid: TGUID;
    /// the interface methods
    Methods: array of TRttiMethod;
  end;
  PRttiInterface = ^TRttiInterface;

/// retrieve methods information of a given IInvokable
// - all methods will be added, also from inherited interface definitions
// - returns the number of methods detected
function GetRttiInterface(aTypeInfo: PRttiInfo;
  out aDefinition: TRttiInterface): integer;

/// execute an instance method from its RTTI per-interface information
// - calling this function with a pre-computed PInterfaceEntry value is faster
// than calling the TObject.GetInterface() method, especially when the class
// implements several interfaces, since it avoid a slow GUID lookup
// - if the interface is retrieved using a getter, will fallback to
// the regular TObject.GetInterface RTL method
function GetInterfaceFromEntry(Instance: TObject; Entry: PInterfaceEntry;
  out Obj): boolean;

/// returns all TGUID implemented by a given class
// - TObject.GetInterfaceTable is not consistent on Delphi and FPC
function GetRttiClassGuid(aClass: TClass): PGuidDynArray;

const
  PSEUDO_RESULT_NAME: string[6] = 'Result';
  PSEUDO_SELF_NAME:   string[4] = 'Self';



{ ************* Efficient Dynamic Arrays and Records Process }

/// faster alternative to Finalize(aVariantDynArray)
// - this function will take account and optimize the release of a dynamic
// array of custom variant types values
// - for instance, an array of TDocVariant will be optimized for speed
procedure VariantDynArrayClear(var Value: TVariantDynArray);
  {$ifdef HASINLINE}inline;{$endif}

/// low-level finalization of a dynamic array of any kind
// - faster than RTL Finalize() or setting nil, when you know ElemInfo
// - see also TRttiInfo.Clear if you want to finalize any type
procedure FastDynArrayClear(Value: PPointer; ElemInfo: PRttiInfo);

/// low-level finalization of all dynamic array items of any kind
// - as called by FastDynArrayClear(), after dec(RefCnt) reached 0
procedure FastFinalizeArray(Value: PPointer; ElemTypeInfo: PRttiInfo;
  Count: integer);

/// clear the managed fields of a record content
// - won't reset all values to zero, only managed fields - see RecordZero()
// - caller should ensure the type is indeed a record/object
// - see also TRttiInfo.Clear if you want to finalize any type
// - same as RTTI_FINALIZE[rkRecord]()
function FastRecordClear(Value: pointer; Info: PRttiInfo): PtrInt;

/// low-level finalization of a dynamic array of RawUtf8
// - faster than RTL Finalize() or setting nil
procedure RawUtf8DynArrayClear(var Value: TRawUtf8DynArray);
  {$ifdef HASINLINE}inline;{$endif}

/// check if the TypeInfo() points to an "array of RawUtf8"
// - e.g. returns true for TypeInfo(TRawUtf8DynArray) or other sub-types
// defined as "type aNewType = type TRawUtf8DynArray"
function IsRawUtf8DynArray(Info: PRttiInfo): boolean;

/// initialize a record content
// - calls FastRecordClear() and FillCharFast() with 0
// - do nothing if the TypeInfo is not from a record/object
procedure RecordZero(Dest: pointer; Info: PRttiInfo);

/// copy a record content from source to Dest
procedure RecordCopy(var Dest; const Source; Info: PRttiInfo);
  {$ifdef FPC}inline;{$endif}

/// efficiently copy several (dynamic) array items
// - faster than the RTL CopyArray() function
procedure CopySeveral(Dest, Source: PByte; SourceCount: PtrInt;
  ItemInfo: PRttiInfo; ItemSize: PtrInt);

/// low-level initialization of a dynamic array
// - faster than System.DynArraySetLength() function on a void dynamic array,
// when the RTTI is known
// - caller should ensure that Dest is not nil, but Dest^ = nil (i.e. a
// clear/void dynamic array)
function DynArrayNew(Dest: PPointer; Count, ItemSize: PtrInt): pointer;

/// low-level size up of a dynamic array
// - faster than System.DynArraySetLength() function dynamic array with RefCnt=1
// - caller should ensure that Dest is not nil
// - DataBytes is expected to be Count * ItemSize
function DynArrayGrow(Dest: PPointer; Count, ItemSize: PtrInt): PAnsiChar;

/// create a dynamic array from another one
// - same as RTTI_COPY[rkDynArray] but with an optional external source count
procedure DynArrayCopy(Dest, Source: PPointer; Info: PRttiInfo;
  SourceExtCount: PInteger);


{ ************* Managed Types Finalization, Random or Copy }

type
  /// internal function handler for finalizing a managed type value
  // - i.e. the kind of functions called via RTTI_FINALIZE[] lookup table
  // - as used by TRttiInfo.Clear() inlined method
  TRttiFinalizer = function(Data: pointer; Info: PRttiInfo): PtrInt;

  /// the type of RTTI_FINALIZE[] efficient lookup table
  TRttiFinalizers = array[TRttiKind] of TRttiFinalizer;
  PRttiFinalizers = ^TRttiFinalizers;

  /// internal function handler for copying a managed type value
  // - i.e. the kind of functions called via RTTI_COPY[] lookup table
  TRttiCopier = function(Dest, Source: pointer; Info: PRttiInfo): PtrInt;

  /// the type of RTTI_COPY[] efficient lookup table
  TRttiCopiers = array[TRttiKind] of TRttiCopier;
  PRttiCopiers = ^TRttiCopiers;


var
  /// lookup table of finalization functions for managed types
  // - as used by TRttiInfo.Clear() inlined method
  // - RTTI_FINALIZE[...]=nil for unmanaged types (e.g. rkOrdinalTypes)
  RTTI_FINALIZE: TRttiFinalizers;

  /// lookup table of copy function for managed types
  // - as used by TRttiInfo.Copy() inlined method
  // - RTTI_COPY[...]=nil for unmanaged types (e.g. rkOrdinalTypes)
  RTTI_COPY: TRttiCopiers;


{ ************** RTTI Value Types used for JSON Parsing }

type
  /// the kind of variables handled by our RTTI/JSON parser
  // - the last item should be ptCustom, for non simple types
  // - ptOrm is recognized from TID, T*ID, TRecordReference,
  // TRecordReferenceToBeDeleted and TRecordVersion type names
  // - ptTimeLog is recognized from TTimeLog, TCreateTime and TModTime
  // - other types (not ptComplexTypes) are recognized by their genuine type name
  // - ptUnicodeString is defined even if not available prior to Delphi 2009
  // - replace deprecated TJsonCustomParserRTTIType type from old mORMot 1.18
  // - TDynArrayKind is now an alias to this genuine enumerate
  TRttiParserType = (
    ptNone,
    ptArray,
    ptBoolean,
    ptByte,
    ptCardinal,
    ptCurrency,
    ptDouble,
    ptExtended,
    ptInt64,
    ptInteger,
    ptQWord,
    ptRawByteString,
    ptRawJson,
    ptRawUtf8,
    ptRecord,
    ptSingle,
    ptString,
    ptSynUnicode,
    ptDateTime,
    ptDateTimeMS,
    ptGuid,
    ptHash128,
    ptHash256,
    ptHash512,
    ptOrm,
    ptTimeLog,
    ptUnicodeString,
    ptUnixTime,
    ptUnixMSTime,
    ptVariant,
    ptWideString,
    ptWinAnsi,
    ptWord,
    ptEnumeration,
    ptSet,
    ptClass,
    ptDynArray,
    ptInterface,
    ptCustom);

  /// the complex kind of variables for ptTimeLog and ptOrm TRttiParserType
  // - as recognized by TypeNameToStandardParserType/TypeInfoToStandardParserType
  TRttiParserComplexType = (
    pctNone,
    pctTimeLog,
    pctCreateTime,
    pctModTime,
    pctID,
    pctSpecificClassID,
    pctRecordReference,
    pctRecordReferenceToBeDeleted,
    pctRecordVersion);

  PRttiParserType = ^TRttiParserType;
  TRttiParserTypes = set of TRttiParserType;
  PRttiParserComplexType = ^TRttiParserComplexType;
  TRttiParserComplexTypes = set of TRttiParserComplexType;

const
  /// map a PtrInt type to the TRttiParserType set
  ptPtrInt  = {$ifdef CPU64} ptInt64 {$else} ptInteger {$endif};

  /// map a PtrUInt type to the TRttiParserType set
  ptPtrUInt = {$ifdef CPU64} ptQWord {$else} ptCardinal {$endif};

  /// which TRttiParserType are not simple types
  // - ptTimeLog and ptOrm are complex, since more than one TypeInfo() may
  // map to their TRttiParserType - see also TRttiParserComplexType
  ptComplexTypes =
    [ptArray, ptRecord, ptCustom, ptTimeLog, ptOrm,
     ptDynArray, ptEnumeration, ptSet, ptClass, ptInterface];

  /// which TRttiParserType types don't need memory management
  ptUnmanagedTypes =
    [ptBoolean..ptQWord, ptSingle, ptDateTime..ptTimeLog,
     ptUnixTime, ptUnixMSTime, ptWord..ptClass];

  /// which TRttiParserType types are (usually) serialized as JSON "text"
  // - actual serialization may depend e.g. on TTextWriterWriteObjectOptions
  ptStringTypes =
    [ptRawByteString .. ptRawUtf8, ptString .. ptHash512, ptTimeLog,
     ptUnicodeString, ptWideString, ptWinAnsi];

var
  /// simple lookup to the plain RTTI type of most simple managed types
  // - nil for unmanaged types (e.g. rkOrdinals) or for more complex types
  // requering additional PRttiInfo (rkRecord, rkDynArray, rkArray...)
  // - you can use PT_INFO[] for types with no RTTI before Delphi 2010, for
  // instance PT_INFO[ptGuid], PT_INFO[ptHash128], PT_INFO[ptHash256] and
  // PT_INFO[ptHash512] since oldest compilers refuse to compile TypeInfo(TGUID),
  // TypeInfo(THash128), TypeInfo(THash256) and TypeInfo(THash512)
  PT_INFO: array[TRttiParserType] of PRttiInfo;

  /// simple lookup to the plain RTTI type of most simple managed types
  // - nil if the complex type is not known
  // - mormot.orm.base may set the exact TypeInfo(TRecordReference) value - this
  // unit set plain TypeInfo(QWord) which is enough for JSON Serialization
  PTC_INFO: array[TRttiParserComplexType] of PRttiInfo;

  /// simple lookup to the size in bytes of TRttiParserType values
  PT_SIZE: array[TRttiParserType] of byte = (
    0, 0, 1, 1, 4, 8, 8, 8,
    8, 4, 8, SizeOf(pointer), SizeOf(pointer), SizeOf(pointer),
    0, 4, SizeOf(pointer), SizeOf(pointer), 8, 8,
    16, 16, 32, 64, 8, 8, SizeOf(pointer), 8, 8,
    SizeOf(variant), SizeOf(pointer), SizeOf(pointer), 2,
    0, 0, SizeOf(pointer), SizeOf(pointer), SizeOf(pointer), 0);

const
  /// type definition name lookup to the TRttiParserType values
  // - ptComplexTypes types should see PTC_NAME[] constant
  PT_NAME: array[TRttiParserType] of RawUtf8 = (
    '', '', 'boolean', 'byte', 'cardinal', 'currency', 'double', 'extended',
    'Int64', 'integer', 'QWord', 'RawByteString', 'RawJson', 'RawUtf8',
    '', 'single', 'string', 'SynUnicode', 'TDateTime', 'TDateTimeMS',
    'TGUID', 'THash128', 'THash256', 'THash512', '', '', 'UnicodeString',
    'TUnixTime', 'TUnixMSTime', 'variant', 'WideString', 'WinAnsi', 'word',
    '', '', '', '', '', '');

  /// type definition name lookup to the TRttiParserComplexType values
  // - for ptComplexTypes types, with PT_NAME[]=''
  // - ptcSpecificClassID returns '' since T....ID types are variable
  PTC_NAME: array[TRttiParserComplexType] of RawUtf8 = (
    '', 'TTimeLog', 'TCreateTime', 'TModTime', 'TID', '',
    'TRecordReference', 'TRecordReferenceToBeDeleted', 'TRecordVersion');

/// retrieve the text name of one TRttiParserType enumerate
function ToText(t: TRttiParserType): PShortString; overload;

/// retrieve the TypeInfo() from PT_INFO[] PTC_INFO[] constant arrays
function ParserTypeToTypeInfo(pt: TRttiParserType;
  pct: TRttiParserComplexType): PRttiInfo;

/// recognize a simple value type from a supplied type name
// - from known ('byte', 'string', 'RawUtf8', 'TGUID'...) type names
// - will return ptNone for any unknown type
// - for ptOrm and ptTimeLog, optional Complex will contain the specific type found
function TypeNameToStandardParserType(Name: PUtf8Char; NameLen: integer;
  Complex: PRttiParserComplexType = nil): TRttiParserType; overload;

/// recognize a simple value type from a supplied type name
// - from known ('byte', 'string', 'RawUtf8', 'TGUID'...) type names
// - will return ptNone for any unknown type
function TypeNameToStandardParserType(Name: PShortString;
  Complex: PRttiParserComplexType = nil): TRttiParserType; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// recognize a simple value type from a supplied type name
// - from known ('byte', 'string', 'RawUtf8', 'TGUID'...) type names, then
// calling Rtti.Find() if CheckRttiCustomTypes=true
// - will return ptNone for any unknown type
function TypeNameToStandardParserType(const Name: RawUtf8;
  Complex: PRttiParserComplexType = nil): TRttiParserType; overload;

/// recognize a simple value type from a supplied type information
// - if FirstSearchByName=true, call TypeNameToStandardParserType(Info^.Name^)
// - will return ptNone for any unknown type
function TypeInfoToStandardParserType(Info: PRttiInfo;
  FirstSearchByName: boolean = true;
  Complex: PRttiParserComplexType = nil): TRttiParserType; overload;

/// recognize most simple types and return their known dynamic array RTTI
// - returns nil if we don't know any dynamic array for this type
// - ExpectExactElemInfo=true ensure that result's ArrayRtti.Info = ElemInfo
function TypeInfoToDynArrayTypeInfo(ElemInfo: PRttiInfo;
  ExpectExactElemInfo: boolean; ParserType: PRttiParserType = nil): PRttiInfo;

/// recognize a simple value type from a dynamic array RTTI
// - if ExactType=false, will approximate the first field
function DynArrayTypeInfoToStandardParserType(
  DynArrayInfo, ElemInfo: PRttiInfo; ElemSize: integer; ExactType: boolean;
  out FieldSize: integer; Complex: PRttiParserComplexType = nil): TRttiParserType;

/// trim ending 'DynArray' or 's' chars from a dynamic array type name
// - used internally to guess the associated item type name
function DynArrayItemTypeLen(const DynArrayTypeName: RawUtf8): PtrInt;



{ ************** RTTI-based Registration for Custom JSON Parsing }

const
  /// TRttiCustomList stores its TypeInfo() by PRttiInfo.Kind + Name[0..1]
  // - optimized "hash table of the poor" (tm) for Find(TypeInfo) and Find(Name)
  // - should be a bit mask (i.e. power of two minus 1)
  // - the fact that we use modulo 32 makes Find(Name) case insensitive :)
  RTTICUSTOMTYPEINFOHASH = 31;

type
  TRttiCustom = class;

  PRttiCustomProp = ^TRttiCustomProp;

  /// variant-like value as returned by TRttiCustomProp.GetValueDirect and
  // GetValueGetter methods
  // - simple values (integers, floats, strings or variant) are set into Data
  // - rkEnumeration, rkSet, rkDynArray, rkClass, rkInterface, rkRecord and
  // rkObject are stored as varAny/PropValue pointer to the field value (for
  // GetValueDirect) or Instance (for GetValueGetter if PropValueIsInstance=true),
  // and Prop to the corresponding property RTTI
  // - will be properly handled by TTextWriter.AddVariant/AddRttiVarData
  // - can be casted as a variant value, but contains RTTI and clear flag:
  // ! if rvd.NeedsClear then VarClearProc(rvd.Data);
  TRttiVarData = packed record
    case integer of
    varUnknown: (
      VType: cardinal);    // maps DataType + NeedsClear + PropValueIsInstance
    varVariant: (
      Data: TVarData);
    varAny: (
      DataType: word;      // matches TVarData.VType
      NeedsClear: boolean;
      PropValueIsInstance: boolean;
      // Assert(@PropValue=@VAny) is done in initialization section below
      {$ifdef CPU32}
      Prop: PRttiCustomProp;
      PropValue: pointer; // TObject if PropValueIsInstance=true, or field addr
      {$else}
      Padding4: cardinal;
      PropValue: pointer; // TObject if PropValueIsInstance=true, or field addr
      Prop: PRttiCustomProp;
      {$endif CPU32});
  end;
  PRttiVarData = ^TRttiVarData;

  /// define specific behavior for a given TypeInfo/PRttIinfo
  // - rcfIsManaged is set if a value of this type expects finalization
  // - rcfObjArray is for T*ObjArray dynamic arrays
  // - rcfBinary is for hexadecimal serialization of integers
  // - rcfWithoutRtti is set if was created purely by text, and uses fake RTTI
  // - rcfSpi identifies types containing Sensitive Personal Information
  // (e.g. a bank card number or a plain password) which should be hidden
  // - rcfHookWrite, rcfHookWriteProperty, rcfHookRead, rcfHookReadProperty for
  // TObjectWithCustomCreate kind of class, to customize JSON serialization
  // calling the set of TObjectWithCustomCreate protected virtual methods -
  // disabled by default not to slow down the serialization process
  // - rcfHasNestedProperties is set e.g. for rkClass or rcfWithoutRtti records,
  // rcfHasNestedManagedProperties if any of the property/field is rcfIsManaged
  // - rcfHasOffsetSetJsonLoadProperties is set if all nested properties can be
  // directly written, i.e. have OffsetSet >= 0 and Assigned(JsonLoad)
  // - rcfArrayItemManaged maps rcfIsManaged flag in ArrayRtti.Flags
  // - rcfReadIgnoreUnknownFields will let JSON unserialization ignore unknown
  // fields for this class/record
  // - rcfAutoCreateFields is defined when AutoCreateFields() has been called
  // - rcfDisableStored is set for TOrm, where "stored AS_UNIQUE" does not mean
  // "not stored" for serialization but "UNIQUE SQL"
  // - rcfClassMayBeID is set e.g. for TOrm classes, which may be storing
  // not instances but IDs in published properties PtrInt
  TRttiCustomFlag = (
    rcfIsManaged,
    rcfObjArray,
    rcfBinary,
    rcfWithoutRtti,
    rcfSpi,
    rcfHookWrite,
    rcfHookWriteProperty,
    rcfHookRead,
    rcfHookReadProperty,
    rcfHasNestedProperties,
    rcfHasNestedManagedProperties,
    rcfHasOffsetSetJsonLoadProperties,
    rcfArrayItemManaged,
    rcfReadIgnoreUnknownFields,
    rcfAutoCreateFields,
    rcfDisableStored,
    rcfClassMayBeID);

  /// define specific behaviors for a given TypeInfo/PRttIinfo
  // - as stored in TRttiCustom.Flags
  TRttiCustomFlags = set of TRttiCustomFlag;

  /// store information about one property/field of a given TypeInfo/PRttIinfo
  // - used by both rkClass for published properties, and rkRecord/rkObject
  // for nested fields
  TRttiCustomProp = object
  protected
    function InitFrom(RttiProp: PRttiProp): PtrInt;
    function ValueIsVoidGetter(Data: pointer): boolean;
    procedure GetValueDirect(Data: PByte; out RVD: TRttiVarData);
    procedure GetValueGetter(Instance: TObject; out RVD: TRttiVarData);
  public
    /// contains standard TypeInfo/PRttiInfo of this field/property
    // - for instance, Value.Size contains its memory size in bytes
    Value: TRttiCustom;
    /// read field/property offset in the record/class instance memory
    // - equals -1 if Prop has a getter
    OffsetGet: PtrInt;
    /// write field/property offset in the record/class instance memory
    // - equals -1 if Prop has a setter
    OffsetSet: PtrInt;
    /// contains Prop^.Name or a customized field/property name
    // - e.g. 'SubProp'
    // - equals '' if Props.NameChange() was set to New='', meaning this field
    // should not be part of the serialized JSON object
    Name: RawUtf8;
    /// store standard RTTI of this published property
    // - equals nil for rkRecord/rkObject nested field
    Prop: PRttiProp;
    /// equals NO_DEFAULT or the default value
    OrdinalDefault: integer;
    /// case-insensitive compare the supplied name/len with the Name property
    function NameMatch(P: PUtf8Char; Len: PtrInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// very fast retrieval of any field value into a TVarData-like
    // - works if Prop is defined or not, calling any getter method if needed
    // - complex TRttiVarData with varAny pointer will be properly handled by
    // TTextWriter.AddVariant/AddRttiVarData (e.g. rkEnumeration or rkDynArray)
    // - rvd can be casted to a variant, but contains RTTI Info and clear flag:
    // ! if rvd.NeedsClear then VarClearProc(rvd.Data);
    procedure GetValue(Data: pointer; out RVD: TRttiVarData);
      {$ifdef HASINLINE}inline;{$endif}
    /// set a field value to a given TVarData-like content
    // - optionally check and apply RVD.NeedsClear flag
    // - not implemented for Prop = nil (i.e. rkRecord/rkObject nested field)
    procedure SetValue(Data: pointer; var RVD: TRttiVarData; andclear: boolean);
    /// check if the Value equals the default property set in source code
    // - caller should have checked that PropDefault <> NO_DEFAULT
    function ValueIsDefault(Data: pointer): boolean;
    /// check if the Value is void (0 / '' / null)
    function ValueIsVoid(Data: pointer): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// compare two properties values with proper getter method call
    function CompareValue(Data, Other: pointer; const OtherRtti: TRttiCustomProp;
      CaseInsensitive: boolean): integer;
    /// append the field value as JSON with proper getter method call
    // - wrap GetValue() + AddVariant() over a temp TRttiVarData
    procedure AddValueJson(W: TBaseWriter; Data: pointer;
      Options: TTextWriterWriteObjectOptions);
  end;

  /// store information about the properties/fields of a given TypeInfo/PRttiInfo
  TRttiCustomPropDynArray = array of TRttiCustomProp;

  PRttiCustomPropDynArray = array of PRttiCustomProp;

  /// store information about all properties/fields of a given TypeInfo/PRttIinfo
  // - includes parent properties when filled by AddFromClass(IncludeParents=true)
  TRttiCustomProps = object
  public
    /// one List[] item per property/field
    List: TRttiCustomPropDynArray;
    /// how many properties/fields are in List[]
    Count: integer;
    /// total size, in bytes, of all properties/fields
    // - equals the sum of List[].Value.Size
    Size: integer;
    /// List[NotInheritedIndex]..List[Count-1] store the last level of properties
    NotInheritedIndex: integer;
    /// locate a property/field by name
    function Find(PropName: PUtf8Char; PropNameLen: PtrInt): PRttiCustomProp; overload;
    /// locate a property/field by name
    function Find(const PropName: RawUtf8): PRttiCustomProp; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// customize a property/field name
    // - New is expected to be only plain pascal identifier, i.e.
    // A-Z a-z 0-9 and _ characters, up to 63 in length
    // - if New equals '', this published property will be excluded from
    // the JSON serialized object
    // - the New shortstring should be a local constant, to avoid random GPF
    function NameChange(const Old, New: RawUtf8): PRttiCustomProp;
    /// customize property/field name, specified as old/new pairs
    // - each Old[] field name will be replaced by the corresponding New[] name
    // - New[] is expected to be only plain pascal identifier, i.e.
    // A-Z a-z 0-9 and _ characters, up to 63 in length
    // - if any New[] equals '', this published property will be excluded from
    // the JSON serialized object
    // - setting both Old=New=[] will return back to the default names from RTTI
    // - the New shortstring should be a local constant, to avoid random GPF
    // - Rtti.ByClass[TMyClass].Props.NameChanges() replaces deprecated
    // TJsonSerializer.RegisterCustomSerializerFieldNames(TMyClass, ...)
    procedure NameChanges(const Old, New: array of RawUtf8);
    /// manuall adding of a property/field definition
    // - append as last field, unless AddFirst is set to true
    procedure Add(Info: PRttiInfo; Offset: PtrInt; const PropName: RawUtf8;
      AddFirst: boolean = false);
    /// register the published properties of a given class
    // - is called recursively if IncludeParents is true
    procedure AddFromClass(ClassInfo: PRttiInfo; IncludeParents: boolean);
    /// prepare List[result].Name from TRttiCustom.SetPropsFromText
    function FromTextPrepare(const PropName: RawUtf8): integer;
    /// register the properties specified from extended RTTI (Delphi 2010+ only)
    // - do nothing on FPC or Delphi 2009 and older
    procedure SetFromRecordExtendedRtti(RecordInfo: PRttiInfo);
    /// called once List[] and Size have been defined
    // - compute the fManaged[] internal list and return the matching flags
    function AdjustAfterAdded: TRttiCustomFlags;
    /// retrieve all List[] items as text
    procedure AsText(out Result: RawUtf8; IncludePropType: boolean;
      const Prefix, Suffix: RawUtf8);
    /// finalize and fill with zero all properties of this class instance
    // - it will individually fill the properties, not the whole memory
    // as TRttiCustom.FinalizeAndClear would on a record
    procedure FinalizeAndClearPublishedProperties(Instance: TObject);
  protected
    /// points to List[] items which are managed
    fManaged: PRttiCustomPropDynArray;
    /// reset all properties
    procedure InternalClear;
    /// finalize the managed properties of this instance
    // - called e.g. when no RTTI is available, i.e. text serialization
    procedure FinalizeManaged(Data: PAnsiChar);
    /// copy the fields of a rkRecordTypes instance
    // - called e.g. when no RTTI is available, i.e. text serialization
    // - will move() all bytes between managed fields
    procedure CopyRecord(Dest, Source: PAnsiChar);
    /// copy the properties of a rkClass instance
    // - called e.g. when no RTTI is available, i.e. text serialization
    // - will copy all published properties one-by-one
    procedure CopyProperties(Dest, Source: PAnsiChar);
  end;

  PRttiCustomProps = ^TRttiCustomProps;

  /// used internally for fast allocation of a rkClass instance
  // - member is properly initialized by TRttiJson from mormot.core.json.pas
  TRttiCustomNewInstance = function(Rtti: TRttiCustom): pointer;

  /// internal function handler for filling a value with some randomness
  TRttiCustomRandom = procedure(Data: pointer; Rtti: TRttiCustom);

  TRttiCustomFromTextExpectedEnd = (
    eeNothing,
    eeSquare,
    eeCurly,
    eeEndKeyWord);

  /// the recognized raw RTL classes as identified in TRttiCustom.ValueRtlClass
  TRttiValueClass = (
    vcNone,
    vcCollection,
    vcStrings,
    vcObjectList,
    vcList,
    vcESynException,
    vcException);


  /// allow to customize the process of a given TypeInfo/PRttiInfo
  // - a global list of TRttiCustom instances mapping TypeInfo() is maintained
  // in Rtti: TRttiCustomList
  // - never instantiate this class directly, but call RttiCustom methods
  TRttiCustom = class
  protected
    fCache: TRttiCache;
    fParser: TRttiParserType;
    fParserComplex: TRttiParserComplexType;
    fFlags: TRttiCustomFlags;
    fPrivateSlot: pointer;
    fPrivateSlots: TObjectDynArray;
    fArrayRtti: TRttiCustom;
    fFinalize: TRttiFinalizer;
    fSetRandom: TRttiCustomRandom;
    fCopy: TRttiCopier;
    fName: RawUtf8;
    fProps: TRttiCustomProps;
    fOwnedRtti: array of TRttiCustom;
    fValueRtlClass: TRttiValueClass;
    fArrayFirstField: TRttiParserType;
    // used by mormot.core.json.pas
    fBinarySize: integer;
    fJsonLoad: pointer; // contains a TRttiJsonLoad - used if fJsonReader=nil
    fJsonSave: pointer; // contains a TRttiJsonSave - used if fJsonWriter=nil
    fJsonReader, fJsonWriter: TMethod; // TOnRttiJsonRead/TOnRttiJsonWrite
    fClassNewInstance: TRttiCustomNewInstance; // mormot.core.json implemented
    fAutoCreateClasses, // three lists made by RegisterAutoCreateFieldsClass
    fAutoCreateObjArrays,
    fAutoCreateInterfaces: PRttiCustomPropDynArray;
    // used by NoRttiSetAndRegister()
    fNoRttiInfo: TByteDynArray;
    // customize class process
    fValueClass: TClass;
    fObjArrayClass: TClass;
    fCollectionItem: TCollectionItemClass;
    fCollectionItemRtti: TRttiCustom;
    procedure SetValueClass(aClass: TClass; aInfo: PRttiInfo);
    // for TRttiCustomList.RegisterObjArray/RegisterBinaryType/RegisterFromText
    function SetObjArray(Item: TClass): TRttiCustom;
    function SetBinaryType(BinarySize: integer): TRttiCustom;
    procedure SetPropsFromText(var P: PUtf8Char;
      ExpectedEnd: TRttiCustomFromTextExpectedEnd; NoRegister: boolean);
    // initialize from fProps, with no associated RTTI - and calls DoRegister()
    // - will create a "fake" rkRecord/rkDynArray PRttiInfo (TypeName may be '')
    procedure NoRttiSetAndRegister(ParserType: TRttiParserType;
      const TypeName: RawUtf8; DynArrayElemType: TRttiCustom;
      NoRegister: boolean);
    // called by ValueFinalize() for dynamic array defined from text
    procedure NoRttiArrayFinalize(Data: PAnsiChar);
    /// initialize this Value process for Parser and Parser Complex kinds
    // - this default method will set Name and Flags according to Props[]
    // - overriden in mormot.core.json for proper JSON process setup
    // - returns self to allow cascaded calls as a fluent interface
    function SetParserType(aParser: TRttiParserType;
      aParserComplex: TRttiParserComplexType): TRttiCustom; virtual;
  public
    /// initialize the customizer class from known RTTI
    constructor Create(aInfo: PRttiInfo); virtual;
    /// initialize abstract custom serialization for a given record
    // - not registered in the main TRttiCustomList: caller should free it
    // - in practice, is used only by test.core.data.pas regression tests
    constructor CreateFromText(const RttiDefinition: RawUtf8);
    /// finalize this instance
    destructor Destroy; override;
    /// efficiently finalize a stored value of this type
    // - if rcfObjArray is defined in Flags, will release all nested TObject
    procedure ValueFinalize(Data: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// efficiently finalize a stored value of this type, and fill it with zeros
    // - if rcfObjArray is defined in Flags, will release all nested TObject
    procedure ValueFinalizeAndClear(Data: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// efficiently copy a stored value of this type
    function ValueCopy(Dest, Source: pointer): PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// return TRUE if the Value is 0 / nil / '' / null
    function ValueIsVoid(Data: PAnsiChar): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// compare two stored values of this type
    // - not implemented in this class (raise an ERttiException)
    // but in TRttiJson, so that it will use mormot.core.data comparison
    function ValueCompare(Data, Other: pointer;
      CaseInsensitive: boolean): integer; virtual;
    /// fill a variant with a stored value of this type
    // - not implemented in this class (raise an ERttiException)
    // but in TRttiJson, so that it will use mormot.core.variants process
    function ValueToVariant(Data: pointer; out Dest: TVarData): PtrInt; virtual;
    /// fill a value from random - including strings and nested types
    // - this method is thread-safe using Rtti.DoLock/DoUnLock
    procedure ValueRandom(Data: pointer);
    /// create a new TObject instance of this rkClass
    // - not implemented here (raise an ERttiException) but in TRttiJson,
    // so that mormot.core.rtti has no dependency to TSynPersistent and such
    function ClassNewInstance: pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// reset all stored Props[] and associated flags
    procedure PropsClear;
    /// register once an instance of a given class per RTTI
    // - thread-safe returns aObject, or an existing object (freeing aObject)
    // - just like PrivateSlot property, but for as many class as needed
    function SetPrivateSlot(aObject: TObject): pointer;
    /// retrieve an instance of a given class per RTTI
    // - previously registered by SetPrivateSlot
    function GetPrivateSlot(aClass: TClass): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level RTTI kind, taken from Rtti property
    property Kind: TRttiKind
      read fCache.Kind;
    /// direct access to the low-level RTTI TypeInfo() pointer, from Rtti property
    property Info: PRttiInfo
      read fCache.Info;
    /// the known type name
    // - may be an hexadecimal value of self, if rcfWithoutRtti is in Flags
    property Name: RawUtf8
      read fName;
    /// direct access to the low-level size in bytes used to store a value
    // of this type, as taken from Rtti property
    // - warning: for rkArray/rkDynArray, equals SizeOf(pointer), not the item
    // size, which is hold in Cache.ItemSize
    property Size: integer
      read fCache.Size;
    /// direct access to the ready-to-use RTTI
    property Cache: TRttiCache
      read fCache;
    /// define specific behavior for this type
    property Flags: TRttiCustomFlags
      read fFlags write fFlags;
    /// high-level Parser kind
    property Parser: TRttiParserType
      read fParser;
    /// high-level Parser Complex kind
    property ParserComplex: TRttiParserComplexType
      read fParserComplex;
    /// store information about the properties/fields of this type
    // - only set for rkClass and rkRecord/rkObject
    property Props: TRttiCustomProps
      read fProps;
    /// shortcut to the TRttiCustom of the item of a (dynamic) array
    // - only set for rkArray and rkDynArray
    // - may be set also for unmanaged types - use Cache.ItemInfo if you want
    // the raw PRttiInfo TypeInfo() pointer for rkManagedTypes only
    property ArrayRtti: TRttiCustom
      read fArrayRtti;
    /// best guess of first field type for a rkDynArray
    // - equals ArrayRtti.Parser if ArrayRtti.Kind is not rkRecordTypes
    property ArrayFirstField: TRttiParserType
      read fArrayFirstField;
    /// store the number of bytes for hexadecimal serialization for rcfBinary
    // - used when rcfBinary is defined in Flags; equals 0 if disabled (default)
    property BinarySize: integer
      read fBinarySize;
    /// store the class of this type, i.e. contains Cache.Info.RttiClass.RttiClass
    property ValueClass: TClass
      read fValueClass;
    /// identify most common RTL inherited classes for special handling
    // - recognize TCollection TStrings TObjectList TList parents
    // - TRttiValueClass enumerate is faster than InheritsFrom() call
    property ValueRtlClass: TRttiValueClass
      read fValueRtlClass;
    /// store the class of a T*ObjArray dynamic array
    // - shortcut to ArrayRtti.Info.RttiClass.RttiClass
    // - used when rcfObjArray is defined in Flags
    property ObjArrayClass: TClass
      read fObjArrayClass;
    /// store the Item class for a given TCollection
    // - as previously registered by Rtti.RegisterCollection()
    property CollectionItem: TCollectionItemClass
      read fCollectionItem;
    /// opaque private instance used by mormot.orm.base.pas or mormot.core.log.pas
    // - stores e.g. the TOrmProperties ORM information of a TOrm,
    // or the TSynLogFamily of a TSynLog instance
    // - is owned, as TObject, by this TRttiCustom
    property PrivateSlot: pointer
      read fPrivateSlot write fPrivateSlot;
    /// opaque TRttiJsonLoad callback used by mormot.core.json.pas
    property JsonLoad: pointer
      read fJsonLoad write fJsonLoad;
    /// opaque TRttiJsonSave callback used by mormot.core.json.pas
    property JsonSave: pointer
      read fJsonSave write fJsonSave;
    /// opaque TOnRttiJsonRead callback used by mormot.core.json.pas
    property JsonReader: TMethod
      read fJsonReader write fJsonReader;
    /// opaque TOnRttiJsonWrite callback used by mormot.core.json.pas
    property JsonWriter: TMethod
      read fJsonWriter write fJsonWriter;
  end;

  PRttiCustom = ^TRttiCustom;

  /// meta-class of TRttiCustom
  // - is usually a TRttiCustom or a TRttiJson class type
  TRttiCustomClass = class of TRttiCustom;

  /// store PRttiInfo/TRttiCustom pairs for TRttiCustomList hash table
  TRttiCustomListPair = record
    RttiInfoRttiCustom: TPointerDynArray;
    CurrentEnd: pointer;
  end;
  PRttiCustomListPair = ^TRttiCustomListPair;

  /// internal structure for TRttiCustomList "hash table of the poor" (tm)
  // - consume e.g. around 50KB of memory on for all mormot2tests types
  TRttiCustomListHashTable = record
    /// for DoRegister thread-safety - no need of TSynLocker padding
    Lock: TRTLCriticalSection;
    /// speedup search by name e.g. from a loop
    LastPair: array[succ(low(TRttiKind)) .. high(TRttiKind)] of TRttiCustom;
    /// store PRttiInfo/TRttiCustom pairs by TRttiKind.Kind+Name[0..1]
    Pairs: array[succ(low(TRttiKind)) .. high(TRttiKind)] of
           array[0..RTTICUSTOMTYPEINFOHASH] of TRttiCustomListPair;
  end;

  /// maintain a thread-safe list of PRttiInfo/TRttiCustom/TRttiJson registration
  TRttiCustomList = object
  private
    // store PRttiInfo/TRttiCustom pairs by TRttiKind.Kind+Name[0..1]
    Table: ^TRttiCustomListHashTable;
    // used to release memory used by registered customizations
    Instances: array of TRttiCustom;
    fGlobalClass: TRttiCustomClass;
    function GetByClass(ObjectClass: TClass): TRttiCustom;
      {$ifdef HASINLINE}inline;{$endif}
    // called by FindOrRegister() for proper inlining
    function DoRegister(Info: PRttiInfo): TRttiCustom; overload;
    function DoRegister(ObjectClass: TClass): TRttiCustom; overload;
    function DoRegister(ObjectClass: TClass; ToDo: TRttiCustomFlags): TRttiCustom; overload;
    procedure Add(Instance: TRttiCustom);
    procedure SetGlobalClass(RttiClass: TRttiCustomClass); // ensure Count=0
    procedure Init;
    procedure Done;
  public
    /// how many TRttiCustom instances have been registered
    Count: integer;
    /// how many TRttiCustom instances have been registered for a given type
    Counts: array[succ(low(TRttiKind)) .. high(TRttiKind)] of integer;
    /// main Lecuyer Random generator as used by TRttiCustom.ValueRandom
    // - this is not thread-safe, but it is much faster and documented as such
    SharedRandom: TLecuyer;
    /// efficient search of TRttiCustom from a given RTTI TypeInfo()
    // - returns nil if Info is not known
    // - call RegisterType() if you want to initialize the type via its RTTI
    // - not inlined since less efficient code is generated
    function Find(Info: PRttiInfo): TRttiCustom; overload;
    /// efficient search of TRttiCustom from a given TObject class
    // - returns nil if Info is not known
    // - will use the ObjectClass vmtAutoTable slot for very fast O(1) lookup
    function Find(ObjectClass: TClass): TRttiCustom; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// efficient search of TRttiCustom from a given type name
    // - internally use our "hash table of the poor" (tm) for quick search
    function Find(Name: PUtf8Char; NameLen: PtrInt;
      Kind: TRttiKind): TRttiCustom; overload;
    /// efficient search of TRttiCustom from a given type name
    // - internally use our "hash table of the poor" (tm) for quick search
    function Find(Name: PUtf8Char; NameLen: PtrInt;
      Kinds: TRttiKinds = []): TRttiCustom; overload;
    /// efficient search of TRttiCustom from a given type name
    function Find(const Name: shortstring; Kinds: TRttiKinds = []): TRttiCustom;
       overload; {$ifdef HASINLINE}inline;{$endif}
    /// manual search of any matching TRttiCustom.ArrayRtti type
    function FindByArrayRtti(ElemInfo: PRttiInfo): TRttiCustom;
    /// register a given RTTI TypeInfo()
    // - returns a new (or existing if it was already registered) TRttiCustom
    // - if Info.Kind is rkDynArray, it will also register the nested rkRecord
    function RegisterType(Info: PRttiInfo): TRttiCustom;
      {$ifdef HASINLINE}inline;{$endif}
    /// register one or several RTTI TypeInfo()
    // - to ensure that those types will be recognized by text definition
    // - will just call RegisterType() for each Info[]
    procedure RegisterTypes(const Info: array of PRttiInfo);
    /// recognize (and register if needed) a standard simple type
    // - will call TypeNameToStandardParserType() to check for known type names
    // - returns a new (or existing if it was already registered) TRttiCustom if
    // the supplied name matches a known type - returns nil if nothing was found
    function RegisterTypeFromName(Name: PUtf8Char; NameLen: PtrInt;
      ParserType: PRttiParserType = nil): TRttiCustom; overload;
    /// recognize (and register if needed) a standard simple type
    // - calls TypeNameToStandardParserType() to check for known type names
    // - returns a new (or existing if it was already registered) TRttiCustom if
    // the supplied name matches a known type - returns nil if nothing was found
    function RegisterTypeFromName(const Name: RawUtf8;
      ParserType: PRttiParserType = nil): TRttiCustom; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// register a given class type, using its RTTI
    // - returns existing or new TRttiCustom
    // - please call RegisterCollection for TCollection
    // - will use the ObjectClass vmtAutoTable slot for very fast O(1) lookup
    function RegisterClass(ObjectClass: TClass): TRttiCustom; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// register a given class type, using its RTTI
    // - returns existing or new TRttiCustom
    // - please call RegisterCollection for TCollection
    // - will use the ObjectClass vmtAutoTable slot for very fast O(1) lookup
    function RegisterClass(aObject: TObject): TRttiCustom; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// register a given class type, using its RTTI, to auto-create/free its
    // class and dynamic array published fields
    function RegisterAutoCreateFieldsClass(ObjectClass: TClass): TRttiCustom;
      {$ifdef HASINLINE}inline;{$endif}
    /// register one or several RTTI TypeInfo()
    // - to ensure that those classes will be recognized by text definition
    // - will just call RegisterClass() for each ObjectClass[]
    procedure RegisterClasses(const ObjectClass: array of TClass);
    /// define how a given TCollectionClass should instantiate its items
    // - we need to know the CollectionItem to propertly initialize a TCollection
    // - not thread-safe: should be called once from the main thread, at startup,
    // e.g. in the initialization section of your collection definition unit
    function RegisterCollection(Collection: TCollectionClass;
      CollectionItem: TCollectionItemClass): TRttiCustom;
    /// register some TypeInfo() containing unsafe parameter values
    // - i.e. any RTTI type containing Sensitive Personal Information, e.g.
    // a bank card number or a plain password
    // - such values will force associated values to be ignored during loging,
    // as a more tuned alternative to optNoLogInput or optNoLogOutput
    // - not thread-safe: should be called once from the main thread, at startup,
    // e.g. in the initialization section of your types definition unit
    procedure RegisterUnsafeSpiType(const Types: array of PRttiInfo);
    /// register one RTTI TypeInfo() to be serialized as hexadecimal
    // - data will be serialized as BinToHexDisplayLower() JSON hexadecimal
    // string, using BinarySize bytes of the value, i.e. BinarySize*2 hexa chars
    // - you can truncate the original data size (e.g. if all bits of an integer
    // are not used) by specifying the aFieldSize optional parameter
    // - will also ensure that those types will be recognized by text definition
    // - leave BinarySize=0 to write all bytes as hexadecimal
    // - set BinarySize=-1 to unregister the binary serialization for the type
    // - not thread-safe: should be called once from the main thread, at startup,
    // e.g. in the initialization section of your types definition unit
    function RegisterBinaryType(Info: PRttiInfo; BinarySize: integer = 0): TRttiCustom;
    /// register one or several RTTI TypeInfo() to be serialized as hexadecimal
    // - TypeInfo() and associated size information will here be defined by pairs:
    // ([TypeInfo(TType1),TYPE1_BYTES,TypeInfo(TType2),TYPE2_BYTES])
    // - a wrapper around the RegisterBinaryType() method
    // - not thread-safe: should be called once from the main thread, at startup,
    // e.g. in the initialization section of your types definition unit
    procedure RegisterBinaryTypes(const InfoBinarySize: array of const);
    /// register one dynamic array RTTI TypeInfo() to be serialized as T*ObjArray
    // - not needed on FPC and Delphi 2010+ since "array of TSomeClass" will be
    // recognized directly - see HASDYNARRAYTYPE conditional
    // - allow JSON serialization and unserialization of the registered dynamic
    // array property defined in any TPersistent or TOrm for oldest Delphi
    // - could be used as such (note the T*ObjArray type naming convention):
    // ! TUserObjArray = array of TUser;
    // ! ...
    // ! Rtti.RegisterObjArray(TypeInfo(TUserObjArray), TUser);
    // - then you can use ObjArrayAdd/ObjArrayFind/ObjArrayDelete to manage
    // the stored items, and never forget to call ObjArrayClear to release
    // the memory
    // - set Item=nil to unregister the type as a T*ObjArray - may be needed
    // to bypass the FPC and Delphi 2010+ automatic recognition
    // - may return nil if DynArray is not a rkDynArray
    // - replace deprecated TJsonSerializer.RegisterObjArrayForJson() method
    // - not thread-safe: should be called once from the main thread, at startup,
    // e.g. in the initialization section of your T*ObjArray definition unit
    function RegisterObjArray(DynArray: PRttiInfo; Item: TClass): TRttiCustom;
    /// register one or several dynamic array RTTI TypeInfo() to be serialized
    // as T*ObjArray
    // - not needed on FPC and Delphi 2010+ since "array of TSomeClass" will be
    // recognized directly - see HASDYNARRAYTYPE conditional
    // - will call the RegisterObjArray() class method by pair:
    // ! Rtti.RegisterObjArrays([
    // !   TypeInfo(TAddressObjArray), TAddress,
    // !   TypeInfo(TUserObjArray), TUser]);
    // - not thread-safe: should be called once from the main thread, at startup,
    // e.g. in the initialization section of your T*ObjArray definition unit
    procedure RegisterObjArrays(const DynArrayItem: array of const);
    /// register TypeInfo() custom serialization for a given dynamic array or record
    // - DynArrayOrRecord should be valid TypeInfo() - use overloaded
    // RegisterFromText(TypeName) if the record has no TypeInfo()
    // - the RTTI information will here be defined as plain text
    // - since Delphi 2010, you can call directly RegisterType()
    // - the record where the data will be stored should be defined as PACKED:
    // ! type TMyRecord = packed record
    // !   A,B,C: integer;
    // !   D: RawUtf8;
    // !   E: record; // or array of record/integer/string/...
    // !     E1,E2: double;
    // !   end;
    // ! end;
    // - call this method with RttiDefinition='' to return back to the default
    // serialization, i.e. binary + Base64 or Delphi 2010+ extended RTTI
    // - RTTI textual information shall be supplied as text, with the
    // same format as any pascal record:
    // ! 'A,B,C: integer; D: RawUtf8; E: record E1,E2: double;'
    // ! 'A,B,C: integer; D: RawUtf8; E: array of record E1,E2: double;'
    // ! 'A,B,C: integer; D: RawUtf8; E: array of SynUnicode; F: array of TGUID'
    // or a shorter alternative syntax for records and arrays:
    // ! 'A,B,C: integer; D: RawUtf8; E: {E1,E2: double}'
    // ! 'A,B,C: integer; D: RawUtf8; E: [E1,E2: double]'
    // in fact ; could be ignored:
    // ! 'A,B,C:integer D:RawUtf8 E:{E1,E2:double}'
    // ! 'A,B,C:integer D:RawUtf8 E:[E1,E2:double]'
    // or even : could be ignored:
    // ! 'A,B,C integer D RawUtf8 E{E1,E2 double}'
    // ! 'A,B,C integer D RawUtf8 E[E1,E2 double]'
    // - it will return the cached TRttiCustom instance corresponding to the
    // supplied RTTI text definition - i.e. the rkRecord if TypeInfo(SomeArray)
    function RegisterFromText(DynArrayOrRecord: PRttiInfo;
      const RttiDefinition: RawUtf8): TRttiCustom; overload;
    /// define a custom serialization for several dynamic arrays or records
    // - the TypeInfo() and textual RTTI information will here be defined as
    // ([TypeInfo(TType1),_TType1, TypeInfo(TType2),_TType2]) pairs
    // - a wrapper around the overloaded RegisterFromText() method
    procedure RegisterFromText(
      const TypeInfoTextDefinitionPairs: array of const); overload;
    /// register by name a custom serialization for a given dynamic array or record
    // - use overloaded RegisterFromText(TypeName) if the record has TypeInfo()
    // - the RTTI information will here be defined as plain text
    function RegisterFromText(const TypeName: RawUtf8;
      const RttiDefinition: RawUtf8): TRttiCustom; overload;
    /// low-level access to the global mutex associated with this RTTI list
    procedure DoLock;
    /// low-level access to the global mutex associated with this RTTI list
    procedure DoUnLock;
    /// default property to access a given RTTI TypeInfo() customization
    // - you can access or register one type by using this default property:
    // ! Rtti[TypeInfo(TMyClass)].Props.NameChange('old', 'new')
    property ByTypeInfo[P: PRttiInfo]: TRttiCustom
      read RegisterType; default;
    /// default property to access a given RTTI customization of a class
    // - you can access or register one type by using this default property:
    // ! Rtti.ByClass[TMyClass].Props.NameChanges(['old', 'new'])
    property ByClass[C: TClass]: TRttiCustom
      read GetByClass;
    /// which kind of TRttiCustom class is to be used for registration
    // - properly set e.g. by mormot.core.json.pas to TRttiJson for JSON support
    property GlobalClass: TRttiCustomClass
      read fGlobalClass write SetGlobalClass;
  end;


var
  /// low-level access to the list of registered PRttiInfo/TRttiCustom/TRttiJson
  Rtti: TRttiCustomList;


implementation

{$ifdef FPC_X64MM}
{$ifdef CPUX64}
uses
  mormot.core.fpcx64mm; // for direct call of _getmem/_freemem x86_64 asm
{$else}
  {$undef FPC_X64MM}
{$endif CPUX64}
{$endif FPC_X64MM}


{ some inlined definitions which should be declared before $include code }

function FromRttiOrd(o: TRttiOrd; P: pointer): Int64;
begin
  case o of
    roSByte:
      result := PShortInt(P)^;
    roSWord:
      result := PSmallInt(P)^;
    roSLong:
      result := PInteger(P)^;
    roUByte:
      result := PByte(P)^;
    roUWord:
      result := PWord(P)^;
    roULong:
      result := PCardinal(P)^;
    {$ifdef FPC_NEWRTTI}
    roSQWord, roUQWord:
      result := PInt64(P)^;
    {$endif FPC_NEWRTTI}
  else
    result := 0; // should never happen
  end;
end;

procedure ToRttiOrd(o: TRttiOrd; P: pointer; Value: PtrInt);
begin
  case o of
    roUByte, roSByte:
      PByte(P)^ := Value;
    roUWord, roSWord:
      PWord(P)^ := Value;
    roULong, roSLong:
      PCardinal(P)^ := Value;
    {$ifdef FPC_NEWRTTI}
    roSQWord, roUQWord:
      PInt64(P)^ := Value;
    {$endif FPC_NEWRTTI}
  end;
end;

type
  // local wrapper to retrieve IInvokable Interface RTTI via GetRttiInterface()
  TGetRttiInterface = class
  public
    Level: integer;
    MethodCount, ArgCount: integer;
    CurrentMethod: PRttiMethod;
    Definition: TRttiInterface;
    procedure AddMethod(const aMethodName: ShortString; aParamCount: integer;
      aKind: TMethodKind);
    procedure AddArgument(aParamName, aTypeName: PShortString; aInfo: PRttiInfo;
      aFlags: TParamFlags);
    procedure RaiseError(const Format: RawUtf8; const Args: array of const);
    // this method will be implemented in mormot.core.rtti.fpc/delphi.inc
    procedure AddMethodsFromTypeInfo(aInterface: PTypeInfo);
  end;

{$ifdef FPC}
  {$include mormot.core.rtti.fpc.inc}      // FPC specific RTTI access
{$else}
  {$include mormot.core.rtti.delphi.inc}   // Delphi specific RTTI access
{$endif FPC}


{ ************* Low-Level Cross-Compiler RTTI Definitions }

{ TRttiClass }

function TRttiClass.RttiClass: TClass;
begin
  result := PTypeData(@self)^.ClassType;
end;

function TRttiClass.UnitName: PShortString;
begin
  result := @PTypeData(@self)^.UnitName;
end;

function _ClassUnit(C: TClass): PShortString;
var
  P: PRttiInfo;
begin
  P := PPointer(PAnsiChar(C) + vmtTypeInfo)^;
  if P <> nil then
    result := P^.RttiNonVoidClass^.UnitName
  else
    result := @NULCHAR;
end;

function TRttiClass.InheritsFrom(AClass: TClass): boolean;
var
  P: PRttiInfo;
begin
  result := true;
  if RttiClass = AClass then
    exit;
  P := ParentInfo;
  while P <> nil do
    with P^.RttiNonVoidClass^ do
      if RttiClass = AClass then
        exit
      else
        P := ParentInfo;
  result := false;
end;


function TRttiProp.Name: PShortString;
begin
  result := @PPropInfo(@self)^.Name;
end;

function TRttiProp.NameUtf8: RawUtf8;
begin
  ShortStringToAnsi7String(PPropInfo(@self)^.Name, result);
end;

function TRttiProp.Next: PRttiProp;
begin
  // this abstract code compiles into 2 asm lines under FPC :)
  with PPropInfo(@self)^ do
    result := AlignToPtr(@PByteArray(@self)[
      (PtrUInt(@PPropInfo(nil).Name) + SizeOf(Name[0])) + Length(Name)]);
end;


{ TRttiProps = TPropData in TypInfo }

function TRttiProps.FieldProp(const PropName: shortstring): PRttiProp;
var
  i: integer;
begin
  if @self<>nil then
  begin
    result := PropList;
    for i := 1 to PropCount do
      if PropNameEquals(result^.Name, @PropName) then
        exit
      else
        result := result^.Next;
  end;
  result := nil;
end;


{ TRttiEnumType }

function TRttiEnumType.RttiOrd: TRttiOrd;
begin
  result := TRttiOrd(PTypeData(@self)^.OrdType);
end;

function TRttiEnumType.MinValue: PtrInt;
begin
  result := PTypeData(@self).MinValue;
end;

function TRttiEnumType.MaxValue: PtrInt;
begin
  result := PTypeData(@self).MaxValue;
end;

function TRttiEnumType.NameList: PShortString;
begin
  result := @PTypeData(@self).NameList;
end;

function TRttiEnumType.SizeInStorageAsEnum: integer;
begin
  if @self = nil then
    result := 0
  else
    result := ORDTYPE_SIZE[RttiOrd]; // MaxValue does not work e.g. with WordBool
end;

function TRttiEnumType.SizeInStorageAsSet: integer;
begin
  if @self <> nil then
  begin
    result := MaxValue;
    if result < 8 then
      result := sizeof(byte)
    else if result < 16 then
      result := sizeof(word)
    else if result < 32 then
      result := sizeof(cardinal)
    else if result < 64 then
      result := sizeof(QWord)
    else
      result := 0;
  end
  else
    result := 0;
end;

function TRttiEnumType.GetEnumName(const Value): PShortString;
begin
  result := GetEnumNameOrd(FromRttiOrd(RttiOrd, @Value));
end;

function TRttiEnumType.GetCaption(const Value): string;
begin
  GetCaptionFromTrimmed(GetEnumNameOrd(FromRttiOrd(RttiOrd, @Value)), result);
end;

procedure TRttiEnumType.AddCaptionStrings(Strings: TStrings;
  UsedValuesBits: Pointer);
var
  i, L: PtrInt;
  Line: array[byte] of AnsiChar;
  P: PAnsiChar;
  V: PShortString;
  s: string;
begin
  if @self = nil then
    exit;
  Strings.BeginUpdate;
  try
    V := NameList;
    for i := MinValue to MaxValue do
    begin
      if (UsedValuesBits = nil) or
         GetBitPtr(UsedValuesBits, i) then
      begin
        L := ord(V^[0]);
        P := @V^[1];
        while (L > 0) and
              (P^ in ['a'..'z']) do
        begin
          // ignore left lowercase chars
          inc(P);
          dec(L);
        end;
        if L = 0 then
        begin
          L := ord(V^[0]);
          P := @V^[1];
        end;
        Line[L] := #0; // GetCaptionFromPCharLen() expect it as ASCIIZ
        MoveFast(P^, Line, L);
        GetCaptionFromPCharLen(Line, s);
        Strings.AddObject(s, pointer(i));
      end;
      inc(PByte(V), length(V^)+1);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function TRttiEnumType.GetCaptionStrings(UsedValuesBits: pointer): string;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    AddCaptionStrings(List, UsedValuesBits);
    result := List.Text;
  finally
    List.Free;
  end;
end;

procedure TRttiEnumType.GetEnumNameAll(var result: TRawUtf8DynArray;
  TrimLeftLowerCase: boolean);
var
  max, i: PtrInt;
  V: PShortString;
begin
  Finalize(result);
  max := MaxValue - MinValue;
  SetLength(result, max + 1);
  V := NameList;
  for i := 0 to max do
  begin
    if TrimLeftLowerCase then
      result[i] := TrimLeftLowerCaseShort(V)
    else
      ShortStringToAnsi7String(V^, result[i]);
    inc(PByte(V), length(V^) + 1);
  end;
end;

procedure TRttiEnumType.GetEnumNameAll(out result: RawUtf8; const Prefix: RawUtf8;
  quotedValues: boolean; const Suffix: RawUtf8; trimedValues, unCamelCased: boolean);
var
  i: integer;
  V: PShortString;
  uncamel: shortstring;
  temp: TTextWriterStackBuffer;
begin
  with TBaseWriter.CreateOwnedStream(temp) do
  try
    AddString(Prefix);
    V := NameList;
    for i := MinValue to MaxValue do
    begin
      if quotedValues then
        Add('"');
      if unCamelCased then
      begin
        TrimLeftLowerCaseToShort(V, uncamel);
        AddShort(uncamel);
      end
      else if trimedValues then
        AddTrimLeftLowerCase(V)
      else
        AddShort(V^);
      if quotedValues then
        Add('"');
      AddComma;
      inc(PByte(V), length(V^) + 1);
    end;
    CancelLastComma;
    AddString(Suffix);
    SetText(result);
  finally
    Free;
  end;
end;

procedure TRttiEnumType.GetEnumNameTrimedAll(var result: RawUtf8; const Prefix: RawUtf8;
  quotedValues: boolean; const Suffix: RawUtf8);
begin
  GetEnumNameAll(result, Prefix, quotedValues, Suffix, {trimed=}true);
end;

function TRttiEnumType.GetEnumNameAllAsJsonArray(TrimLeftLowerCase: boolean;
  UnCamelCased: boolean): RawUtf8;
begin
  GetEnumNameAll(result, '[', {quoted=}true, ']', TrimLeftLowerCase, UnCamelCased);
end;

function TRttiEnumType.GetEnumNameValue(const EnumName: ShortString): integer;
begin
  result := GetEnumNameValue(@EnumName[1], ord(EnumName[0]));
end;

function TRttiEnumType.GetEnumNameValue(Value: PUtf8Char): integer;
begin
  result := GetEnumNameValue(Value, StrLen(Value));
end;

function TRttiEnumType.GetEnumNameValue(Value: PUtf8Char; ValueLen: integer;
  AlsoTrimLowerCase: boolean): integer;
begin
  if (Value <> nil) and
     (ValueLen > 0) then
  begin
    result := FindShortStringListExact(NameList, MaxValue, Value, ValueLen);
    if (result < 0) and
       AlsoTrimLowerCase then
      result := FindShortStringListTrimLowerCase(NameList, MaxValue, Value, ValueLen);
  end
  else
    result := -1;
end;

function TRttiEnumType.GetEnumNameValueTrimmed(Value: PUtf8Char; ValueLen: integer;
  ExactCase: boolean): integer;
begin
  if (Value <> nil) and
     (ValueLen > 0) then
    if ExactCase then
      result := FindShortStringListTrimLowerCaseExact(NameList, MaxValue, Value, ValueLen)
    else
      result := FindShortStringListTrimLowerCase(NameList, MaxValue, Value, ValueLen)
  else
    result := -1;
end;

function TRttiEnumType.GetEnumNameTrimed(const Value): RawUtf8;
begin
  result := TrimLeftLowerCaseShort(GetEnumName(Value));
end;

procedure TRttiEnumType.GetSetNameCsv(W: TBaseWriter; Value: cardinal;
  SepChar: AnsiChar; FullSetsAsStar: boolean);
var
  j: integer;
  PS: PShortString;
begin
  W.Add('[');
  if FullSetsAsStar and GetAllBits(Value, MaxValue + 1) then
    W.AddShorter('"*"')
  else
  begin
    PS := NameList;
    for j := MinValue to MaxValue do
    begin
      if GetBitPtr(@Value, j) then
      begin
        W.Add('"');
        if twoTrimLeftEnumSets in W.CustomOptions then
          W.AddTrimLeftLowerCase(PS)
        else
          W.AddShort(PS^);
        W.Add('"', SepChar);
      end;
      inc(PByte(PS), ord(PS^[0]) + 1); // next item
    end;
  end;
  W.CancelLastComma;
  W.Add(']');
end;

function TRttiEnumType.GetSetNameCsv(Value: cardinal; SepChar: AnsiChar;
  FullSetsAsStar: boolean): RawUtf8;
var
  W: TBaseWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TBaseWriter.CreateOwnedStream(temp);
  try
    GetSetNameCsv(W, Value, SepChar, FullSetsAsStar);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TRttiEnumType.GetEnumNameTrimedValue(const EnumName: ShortString): integer;
begin
  result := GetEnumNameTrimedValue(@EnumName[1], ord(EnumName[0]));
end;

function TRttiEnumType.GetEnumNameTrimedValue(Value: PUtf8Char; ValueLen: integer): integer;
begin
  if Value = nil then
    result := -1
  else
  begin
    if ValueLen = 0 then
      ValueLen := StrLen(Value);
    result := FindShortStringListTrimLowerCase(NameList, MaxValue, Value, ValueLen);
    if result < 0 then
      result := FindShortStringListExact(NameList, MaxValue, Value, ValueLen);
  end;
end;

procedure TRttiEnumType.SetEnumFromOrdinal(out Value; Ordinal: PtrUInt);
begin
  ToRttiOrd(RttiOrd, @Value, Ordinal);
end;



{ TRttiInterfaceTypeData }

function TRttiInterfaceTypeData.IntfFlags: TRttiIntfFlags;
begin
  result := TRttiIntfFlags(PTypeData(@self)^.IntfFlags);
end;

function TRttiInterfaceTypeData.IntfUnit: PShortString;
begin
  result := @PTypeData(@self)^.IntfUnit;
end;


{ TRttiInfo }

procedure TRttiInfo.Clear(Data: pointer);
var
  fin: TRttiFinalizer;
begin
  fin := RTTI_FINALIZE[Kind];
  if Assigned(fin) then
    fin(Data, @self);
end;

procedure TRttiInfo.Copy(Dest, Source: pointer);
var
  cop: TRttiCopier;
begin
  cop := RTTI_COPY[Kind];
  if Assigned(cop) then
    cop(Dest, Source, @self);
end;

function TRttiInfo.RttiOrd: TRttiOrd;
begin
  result := TRttiOrd(GetTypeData(@self)^.OrdType);
end;

function TRttiInfo.IsCurrency: boolean;
begin
  result := TRttiFloat(GetTypeData(@self)^.FloatType) = rfCurr;
end;

function TRttiInfo.IsRawBlob: boolean;
begin
  result := @self = TypeInfo(RawBlob);
end;

function TRttiInfo.RttiFloat: TRttiFloat;
begin
  result := TRttiFloat(GetTypeData(@self)^.FloatType);
end;

{$ifndef ISFPC32}
function TRttiInfo.SetEnumSize: PtrInt;
begin
  result := SetEnumType^.SizeInStorageAsSet;
end;
{$endif ISFPC32}

function TRttiInfo.DynArrayItemSize: PtrInt;
begin
  if DynArrayItemType(result) = nil then
    result := 0;
end;

function TRttiInfo.RttiSize: PtrInt;
begin
  case Kind of
    rkInteger, rkEnumeration, rkChar, rkWChar
    {$ifdef FPC}, rkBool, rkUChar{$endif}:
      result := ORDTYPE_SIZE[TRttiOrd(GetTypeData(@self)^.OrdType)];
    rkSet:
      result := SetEnumSize;
    rkFloat:
      result := FLOATTYPE_SIZE[TRttiFloat(GetTypeData(@self)^.FloatType)];
    rkLString, {$ifdef FPC} rkLStringOld, rkInterfaceRaw, {$endif}
    {$ifdef HASVARUSTRING} rkUString, {$endif}
    rkWString, rkClass, rkInterface, rkDynArray
    {$ifdef FPC_OR_UNICODE} , rkClassRef, rkPointer {$endif}:
      result := SizeOf(pointer);
    rkInt64 {$ifdef FPC}, rkQWord{$endif}:
      result := 8;
    rkVariant:
      result := SizeOf(variant);
    rkArray:
      result := ArraySize;
    rkRecord {$ifdef FPC}, rkObject {$endif}:
      result := RecordSize;
    rkSString:
      result := GetTypeData(@self)^.MaxLength + 1;
  else
    result := 0;
  end;
end;

function TRttiInfo.IsManaged: boolean;
begin
  if Kind in rkRecordTypes then
    result := RecordManagedFieldsCount > 0
  else
    result := Kind in rkManagedTypes;
  // note: rkArray should be handled specificically: we return true here by now
end;

function TRttiInfo.ClassFieldCount(onlyWithoutGetter: boolean): integer;
begin
  result := ClassFieldCountWithParents(RttiClass^.RttiClass, onlyWithoutGetter);
end;

function TRttiInfo.InheritsFrom(AClass: TClass): boolean;
begin
  result := RttiNonVoidClass^.InheritsFrom(AClass);
end;

function TRttiInfo.EnumBaseType(out NameList: PShortString;
  out Max: integer): PRttiEnumType;
begin
  result := EnumBaseType;
  NameList := result^.NameList;
  Max := result^.MaxValue;
end;

function TRttiInfo.SetEnumType: PRttiEnumType;
begin
  if (@self = nil) or
     (Kind <> rkSet) then
    result := nil
  else
    result := PRttiEnumType(GetTypeData(@self))^.SetBaseType;
end;

function TRttiInfo.SetEnumType(out NameList: PShortString;
  out Max: integer): PRttiEnumType;
begin
  result := SetEnumType;
  if result <> nil then
  begin
    NameList := result^.NameList;
    Max := result^.MaxValue;
  end;
end;


var
  /// conversion table from TRttiKind to TRttiVarData.VType
  // - rkEnumeration,rkSet,rkDynArray,rkClass,rkInterface,rkRecord,rkArray are
  // identified as varAny with TVarData.VAny pointing to the actual value
  RTTI_TO_VARTYPE: array[TRttiKind] of cardinal;

procedure TRttiInfo.ComputeCache(out Cache: TRttiCache);
var
  enum: PRttiEnumType;
  siz, cnt: PtrInt;
begin
  Cache.Info := @self;
  Cache.Size := RttiSize;
  Cache.Kind := Kind;
  Cache.RttiVarDataVType := RTTI_TO_VARTYPE[Kind];
  Cache.Flags := [];
  if Kind in rkOrdinalTypes then
  begin
    if Kind in rkHasRttiOrdTypes then
    begin
      include(Cache.Flags, rcfHasRttiOrd);
      Cache.RttiOrd := RttiOrd;
    end;
    if IsQWord then
      include(Cache.Flags, rcfQword);
    if IsBoolean then
    begin
      Cache.RttiVarDataVType := varBoolean; // no rkBool on Delphi
      include(Cache.Flags, rcfBoolean);
    end;
  end;
  if Kind in rkGetOrdPropTypes then
    include(Cache.Flags, rcfGetOrdProp)
  else if Kind in rkGetInt64PropTypes then
    include(Cache.Flags, rcfGetInt64Prop);
  case Kind of
    rkFloat:
      begin
        Cache.RttiFloat := RttiFloat;
        if Cache.RttiFloat = rfCurr then
        begin
          Cache.RttiVarDataVType := varCurrency;
          include(Cache.Flags, rcfIsCurrency);
        end;
      end;
    rkEnumeration:
      begin
        enum := Cache.Info.EnumBaseType;
        Cache.EnumInfo := enum;
        Cache.EnumMax := enum.MaxValue;
        Cache.EnumList := enum.NameList;
      end;
    rkSet:
      begin
        enum := Cache.Info.SetEnumType;
        Cache.EnumInfo := enum;
        Cache.EnumMax := enum.MaxValue;
        Cache.EnumList := enum.NameList;
      end;
    rkDynArray:
      begin
        Cache.ItemInfo := DynArrayItemType(siz);
        Cache.ItemSize := siz;
      end;
    rkArray:
      begin
        Cache.ItemInfo := ArrayItemType(cnt, siz);
        if (cnt = 0) or
           (siz mod cnt <> 0) then
          raise ERttiException.CreateUtf8('ComputeCache(%): array siz=% cnt=%',
            [RawName, siz, cnt]);
        Cache.ItemSize := siz div cnt;
        Cache.ItemCount := cnt;
      end;
    rkLString:
      if IsRawBlob then
      begin
        include(Cache.Flags, rcfIsRawBlob);
        Cache.CodePage := CP_RAWBYTESTRING; // CP_RAWBLOB is internal
        Cache.Engine := TSynAnsiConvert.Engine(CP_RAWBYTESTRING);
      end
      else
      begin
        Cache.CodePage := AnsiStringCodePage; // use TypeInfo() on old Delphi
        Cache.Engine := TSynAnsiConvert.Engine(Cache.CodePage);
      end;
   end;
end;

function TRttiInfo.InterfaceType: PRttiInterfaceTypeData;
begin
  result := pointer(GetTypeData(@self));
end;

function TRttiInfo.AnsiStringCodePage: integer;
begin
  if @self = TypeInfo(RawBlob) then
    result := CP_RAWBLOB
  else
  {$ifdef HASCODEPAGE}
  if Kind = rkLString then
    // has rkLStringOld any codepage? don't think so -> UTF-8
    result := GetTypeData(@self)^.CodePage
  else
    result := CP_UTF8; // default is UTF-8
  {$else}
  if @self = TypeInfo(RawUtf8) then
    result := CP_UTF8
  else if @self = TypeInfo(WinAnsiString) then
    result := CODEPAGE_US
  else if @self = TypeInfo(RawUnicode) then
    result := CP_UTF16
  else if @self = TypeInfo(RawByteString) then
    result := CP_RAWBYTESTRING // RawBlob has same internal code page
  else if @self = TypeInfo(AnsiString) then
    result := CP_ACP
  else
    result := CP_UTF8; // default is UTF-8
  {$endif HASCODEPAGE}
end;

{$ifdef HASCODEPAGE}

function TRttiInfo.AnsiStringCodePageStored: integer;
begin
  result := GetTypeData(@self)^.CodePage;
end;

{$endif HASCODEPAGE}

procedure TRttiInfo.StringToUtf8(Data: pointer; var Value: RawUtf8);
begin
  case Kind of
    rkChar:
      FastSetString(Value, Data, {ansicharcount=}1);
    rkWChar:
      RawUnicodeToUtf8(Data, {widecharcount=}1, Value);
    rkSString:
      ShortStringToAnsi7String(PShortString(Data)^, Value);
    rkLString:
      Value := PRawUtf8(Data)^;
    rkWString:
      RawUnicodeToUtf8(Data, length(PWideString(Data)^), Value);
    {$ifdef HASVARUSTRING}
    rkUString:
      RawUnicodeToUtf8(Data, length(PUnicodeString(Data)^), Value);
    {$endif HASVARUSTRING}
  else
    Value := '';
  end;
end;

function TRttiInfo.InterfaceGuid: PGUID;
begin
  if (@self = nil) or
     (Kind <> rkInterface) then
    result := nil
  else
    result := InterfaceType^.IntfGuid;
end;

function TRttiInfo.InterfaceUnitName: PShortString;
begin
  if (@self = nil) or
     (Kind <> rkInterface) then
    result := @NULCHAR
  else
    result := InterfaceType^.IntfUnit;
end;

function TRttiInfo.InterfaceAncestor: PRttiInfo;
begin
  if (@self = nil) or
     (Kind <> rkInterface) then
    result := nil
  else
    result := InterfaceType^.IntfParent;
end;

procedure TRttiInfo.InterfaceAncestors(out Ancestors: PRttiInfoDynArray;
  OnlyImplementedBy: TInterfacedObjectClass;
  out AncestorsImplementedEntry: TPointerDynArray);
var
  n: integer;
  nfo: PRttiInfo;
  typ: PRttiInterfaceTypeData;
  entry: pointer;
begin
  if (@self = nil) or
     (Kind <> rkInterface) then
    exit;
  n := 0;
  typ := InterfaceType;
  repeat
    nfo := typ^.IntfParent;
    if nfo = nil then
      exit;
    if nfo = TypeInfo(IInterface) then
      exit;
    typ := nfo^.InterfaceType;
    if ifHasGuid in typ^.IntfFlags then
    begin
      if OnlyImplementedBy <> nil then
      begin
        entry := OnlyImplementedBy.GetInterfaceEntry(typ^.IntfGuid^);
        if entry = nil then
          continue;
        SetLength(AncestorsImplementedEntry, n + 1);
        AncestorsImplementedEntry[n] := entry;
      end;
      SetLength(Ancestors, n + 1);
      Ancestors[n] := nfo;
      inc(n);
    end;
  until false;
end;


{ TRttiProp }

function TRttiProp.Index: integer;
begin
  result := PPropInfo(@self)^.Index;
end;

function TRttiProp.Default: integer;
begin
  result := PPropInfo(@self)^.Default;
end;

function TRttiProp.NameIndex: integer;
begin
  result := PPropInfo(@self)^.NameIndex;
end;

function TRttiProp.FieldSize: PtrInt;
begin
  result := TypeInfo^.RttiSize;
end;

function TRttiProp.GetterAddr(Instance: pointer): pointer;
begin
  result := Pointer(PtrUInt(Instance) +
    PtrUInt(PPropInfo(@self)^.GetProc) {$ifdef ISDELPHI} and $00ffffff {$endif} );
end;

function TRttiProp.SetterAddr(Instance: pointer): pointer;
begin
  result := Pointer(PtrUInt(Instance) +
    PtrUInt(PPropInfo(@self)^.SetProc) {$ifdef ISDELPHI} and $00ffffff {$endif} );
end;

function TRttiProp.GetFieldAddr(Instance: TObject): pointer;
begin
  if not GetterIsField then
    if not SetterIsField then
      // both are methods -> returns nil
      result := nil
    else
      // field - Setter is the field offset in the instance data
      result := SetterAddr(Instance)
  else
    // field - Getter is the field offset in the instance data
    result := GetterAddr(Instance);
end;

function TRttiProp.GetterCall: TRttiPropCall;
var
  call: TMethod;
begin
  result := Getter(nil, @call);
end;

function TRttiProp.SetterCall: TRttiPropCall;
var
  call: TMethod;
begin
  result := Setter(nil, @call);
end;

function TRttiProp.DefaultOr0: integer;
begin
  result := PPropInfo(@self)^.Default;
  if result = NO_DEFAULT then
    result := 0;
end;

function TRttiProp.IsRawBlob: boolean;
begin
  result := TypeInfo = system.TypeInfo(RawBlob);
end;

function TRttiProp.SetValue(Instance: TObject; const Value: variant): boolean;
var
  k: TRttiKind;
  v64: Int64;
  f64: double;
  u: RawUtf8;
begin
  result := false; // invalid or unsupported type
  k := TypeInfo.Kind;
  if k in rkOrdinalTypes then
    if VariantToInt64(Value, v64) then
      SetInt64Value(Instance, v64)
    else
      exit
  else if k in rkStringTypes then
    if VarIsEmptyOrNull(Value) then // otherwise would set 'null' text
      SetAsString(Instance, '')
    else if VariantToUtf8(Value, u) then
      SetAsString(Instance, u)
    else
      exit
  else if k = rkFloat then
    if VariantToDouble(Value, f64) then
      SetFloatProp(Instance, f64)
    else if Assigned(_Iso8601ToDateTime) and
            VariantToUtf8(Value, u) then
    begin
      f64 := _Iso8601ToDateTime(u);
      if f64 = 0 then
        exit;
      SetFloatProp(Instance, f64);
    end
    else
      exit
  else if k = rkVariant then
    SetVariantProp(Instance, Value)
  else
    exit;
  result := true;
end;

function TRttiProp.GetOrdProp(Instance: TObject): Int64;
type
  TGetProc = function: Pointer of object; // pointer result is a PtrInt register
  TGetIndexed = function(Index: integer): Pointer of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      call.Code := PPointer({%H-}call.Data)^;
    rpcMethod:
      call.Code := TGetProc(call);
    rpcIndexed:
      call.Code := TGetIndexed(call)(Index);
  else
    call.Code := nil; // call.Code is used to store the raw value
  end;
  with TypeInfo^ do
    if (Kind = rkClass) or
       (Kind = rkDynArray) or
       (Kind = rkInterface) then
      result := PtrInt(call.Code)
    else
      result := FromRttiOrd(RttiOrd, @call.Code);
end;

procedure TRttiProp.SetOrdProp(Instance: TObject; Value: PtrInt);
type
  // on all targets, Value is a register for any RttiOrd size
  TSetProc = procedure(Value: PtrInt) of object;
  TSetIndexed = procedure(Index: integer; Value: PtrInt) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      with TypeInfo^ do
        if (Kind = rkClass) or
           (Kind = rkDynArray) or
           (Kind = rkInterface) then
          PPtrInt({%H-}call.Data)^ := Value
        else
          ToRttiOrd(RttiOrd, call.Data, Value);
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

function TRttiProp.GetObjProp(Instance: TObject): TObject;
type
  TGetProc = function: TObject of object;
  TGetIndexed = function(Index: integer): TObject of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      result := PObject({%H-}call.Data)^;
    rpcMethod:
      result := TGetProc(call);
    rpcIndexed:
      result := TGetIndexed(call)(Index);
  else
    result := nil;
  end;
end;

function TRttiProp.GetInt64Prop(Instance: TObject): Int64;
type
  TGetProc = function: Int64 of object;
  TGetIndexed = function(Index: integer): Int64 of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      result := PInt64({%H-}call.Data)^;
    rpcMethod:
      result := TGetProc(call);
    rpcIndexed:
      result := TGetIndexed(call)(Index);
  else
    result := 0;
  end;
end;

procedure TRttiProp.SetInt64Prop(Instance: TObject; const Value: Int64);
type
  TSetProc = procedure(Value: Int64) of object;
  TSetIndexed = procedure(Index: integer; Value: Int64) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PInt64({%H-}call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

procedure TRttiProp.GetLongStrProp(Instance: TObject; var Value: RawByteString);
var
  rpc: TRttiPropCall;
  call: TMethod;

    procedure SubProc(rpc: TRttiPropCall); // avoid try..finally
    type
      TGetProc = function: RawByteString of object;
      TGetIndexed = function(Index: integer): RawByteString of object;
    begin
      case rpc of
        rpcMethod:
          Value := TGetProc(call);
        rpcIndexed:
          Value := TGetIndexed(call)(Index);
      else
        Value := '';
      end;
    end;

begin
  rpc := Getter(Instance, @call);
  if rpc = rpcField then
    Value := PRawByteString(call.Data)^
  else
    SubProc(rpc);
end;

procedure TRttiProp.SetLongStrProp(Instance: TObject; const Value: RawByteString);
type
  TSetProc = procedure(const Value: RawByteString) of object;
  TSetIndexed = procedure(Index: integer; const Value: RawByteString) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PRawByteString({%H-}call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

procedure TRttiProp.CopyLongStrProp(Source, Dest: TObject);
var
  tmp: RawByteString;
begin
  GetLongStrProp(Source, tmp);
  SetLongStrProp(Dest, tmp);
end;

procedure TRttiProp.GetShortStrProp(Instance: TObject; var Value: RawUtf8);
type
  TGetProc = function: ShortString of object;
  TGetIndexed = function(Index: integer): ShortString of object;
var
  call: TMethod;
  tmp: ShortString;
begin
  case Getter(Instance, @call) of
    rpcField:
      tmp := PShortString({%H-}call.Data)^;
    rpcMethod:
      tmp := TGetProc(call);
    rpcIndexed:
      tmp := TGetIndexed(call)(Index);
  else
    tmp := '';
  end;
  ShortStringToAnsi7String(tmp, Value);
end; // no SetShortStrProp() by now

procedure TRttiProp.GetWideStrProp(Instance: TObject; var Value: WideString);
type
  TGetProc = function: WideString of object;
  TGetIndexed = function(Index: integer): WideString of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      Value := PWideString({%H-}call.Data)^;
    rpcMethod:
      Value := TGetProc(call);
    rpcIndexed:
      Value := TGetIndexed(call)(Index);
  else
    Value := '';
  end;
end;

procedure TRttiProp.SetWideStrProp(Instance: TObject; const Value: WideString);
type
  TSetProc = procedure(const Value: WideString) of object;
  TSetIndexed = procedure(Index: integer; const Value: WideString) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PWideString({%H-}call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

{$ifdef HASVARUSTRING}

procedure TRttiProp.GetUnicodeStrProp(Instance: TObject; var Value: UnicodeString);
var
  rpc: TRttiPropCall;
  call: TMethod;

    procedure SubProc(rpc: TRttiPropCall); // avoid try..finally
    type
      TGetProc = function: UnicodeString of object;
      TGetIndexed = function(Index: integer): UnicodeString of object;
    begin
      case rpc of
        rpcMethod:
          Value := TGetProc(call);
        rpcIndexed:
          Value := TGetIndexed(call)(Index);
      else
        Value := '';
      end;
    end;

begin
  rpc := Getter(Instance, @call);
  if rpc = rpcField then
    Value := PUnicodeString(call.Data)^
  else
    SubProc(rpc);
end;

procedure TRttiProp.SetUnicodeStrProp(Instance: TObject; const Value: UnicodeString);
type
  TSetProc = procedure(const Value: UnicodeString) of object;
  TSetIndexed = procedure(Index: integer; const Value: UnicodeString) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PUnicodeString({%H-}call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

{$endif HASVARUSTRING}

procedure TRttiProp.GetCurrencyProp(Instance: TObject; var Value: currency);
type
  TGetProc = function: currency of object;
  TGetIndexed = function(Index: integer): currency of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      Value := PCurrency({%H-}call.Data)^;
    rpcMethod:
      Value := TGetProc(call);
    rpcIndexed:
      Value := TGetIndexed(call)(Index);
  else
    PInt64(@Value)^ := 0;
  end;
end;

procedure TRttiProp.SetCurrencyProp(Instance: TObject; const Value: currency);
type
  TSetProc = procedure(const Value: currency) of object;
  TSetIndexed = procedure(Index: integer; const Value: currency) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PCurrency({%H-}call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

function TRttiProp.GetDoubleProp(Instance: TObject): double;
type
  TGetProc = function: double of object;
  TGetIndexed = function(Index: integer): double of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      result := unaligned(PDouble({%H-}call.Data)^);
    rpcMethod:
      result := TGetProc(call);
    rpcIndexed:
      result := TGetIndexed(call)(Index);
  else
    result := 0;
  end;
end;

procedure TRttiProp.SetDoubleProp(Instance: TObject; Value: Double);
type
  TSetProc = procedure(const Value: double) of object;
  TSetIndexed = procedure(Index: integer; const Value: double) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      unaligned(PDouble({%H-}call.Data)^) := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

function TRttiProp.GetFloatProp(Instance: TObject): double;
type
  TSingleProc = function: Single of object;
  TSingleIndexed = function(Index: integer): Single of object;
  TDoubleProc = function: Double of object;
  TDoubleIndexed = function(Index: integer): Double of object;
  TExtendedProc = function: Extended of object;
  TExtendedIndexed = function(Index: integer): Extended of object;
  TCurrencyProc = function: currency of object;
  TCurrencyIndexed = function(Index: integer): currency of object;
var
  call: TMethod;
  rf: TRttiFloat;
begin
  result := 0;
  rf := TypeInfo^.RttiFloat;
  case Getter(Instance, @call) of
    rpcField:
      case rf of
        rfSingle:
          result := PSingle({%H-}call.Data)^;
        rfDouble:
          result := unaligned(PDouble(call.Data)^);
        rfExtended:
          result := PExtended(call.Data)^;
        rfCurr:
          CurrencyToDouble(PCurrency(call.Data), result);
      end;
    rpcMethod:
      case rf of
        rfSingle:
          result := TSingleProc(call);
        rfDouble:
          result := TDoubleProc(call);
        rfExtended:
          result := TExtendedProc(call);
        rfCurr:
          CurrencyToDouble(TCurrencyProc(call), result);
      end;
    rpcIndexed:
      case rf of
        rfSingle:
          result := TSingleIndexed(call)(Index);
        rfDouble:
          result := TDoubleIndexed(call)(Index);
        rfExtended:
          result := TExtendedIndexed(call)(Index);
        rfCurr:
          CurrencyToDouble(TCurrencyIndexed(call)(Index), result);
      end;
  end;
end;

procedure TRttiProp.SetFloatProp(Instance: TObject; Value: TSynExtended);
type
  TSingleProc = procedure(const Value: Single) of object;
  TSingleIndexed = procedure(Index: integer; const Value: Single) of object;
  TDoubleProc = procedure(const Value: double) of object;
  TDoubleIndexed = procedure(Index: integer; const Value: double) of object;
  TExtendedProc = procedure(const Value: Extended) of object;
  TExtendedIndexed = procedure(Index: integer; const Value: Extended) of object;
  TCurrencyProc = procedure(const Value: currency) of object;
  TCurrencyIndexed = procedure(Index: integer; const Value: currency) of object;
var
  call: TMethod;
  rf: TRttiFloat;
begin
  rf := TypeInfo^.RttiFloat;
  case Setter(Instance, @call) of
    rpcField:
      case rf of
        rfSingle:
          PSingle({%H-}call.Data)^ := Value;
        rfDouble:
          unaligned(PDouble(call.Data)^) := Value;
        rfExtended:
          PExtended(call.Data)^ := Value;
        rfCurr:
          DoubleToCurrency(Value, PCurrency(call.Data));
      end;
    rpcMethod:
      case rf of
        rfSingle:
          TSingleProc(call)(Value);
        rfDouble:
          TDoubleProc(call)(Value);
        rfExtended:
          TExtendedProc(call)(Value);
        rfCurr:
          TCurrencyProc(call)(DoubleToCurrency(Value));
      end;
    rpcIndexed:
      case rf of
        rfSingle:
          TSingleIndexed(call)(Index, Value);
        rfDouble:
          TDoubleIndexed(call)(Index, Value);
        rfExtended:
          TExtendedIndexed(call)(Index, Value);
        rfCurr:
          TCurrencyIndexed(call)(Index, DoubleToCurrency(Value));
      end;
  end;
end;

procedure TRttiProp.GetVariantProp(Instance: TObject; var Result: Variant;
  SetByRef: boolean);
var
  rpc: TRttiPropCall;
  call: TMethod;

  procedure SubProc(rpc: TRttiPropCall); // avoid try..finally
  type
    TGetProc = function: variant of object;
    TGetIndexed = function(Index: integer): variant of object;
  begin
    case rpc of
      rpcMethod:
        Result := TGetProc(call);
      rpcIndexed:
        Result := TGetIndexed(call)(Index);
    else
      SetVariantNull(result);
    end;
  end;

begin
  rpc := Getter(Instance, @call);
  if rpc <> rpcField then
    SubProc(rpc)
  else if not SetVariantUnRefSimpleValue(PVariant(call.Data)^, PVarData(@Result)^) then
    if SetByRef then
    begin
      VarClear(Result);
      TVarData(Result).VType := varVariantByRef;
      TVarData(Result).VPointer := call.Data;
    end
    else
      result := PVariant(call.Data)^;
end;

procedure TRttiProp.SetVariantProp(Instance: TObject; const Value: Variant);
type
  TSetProc = procedure(const Value: variant) of object;
  TSetIndexed = procedure(Index: integer; const Value: variant) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PVariant({%H-}call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

function TRttiProp.GetOrdValue(Instance: TObject): Int64;
begin
  if (Instance <> nil) and
     (@self <> nil) and
     (TypeInfo^.Kind in [rkInteger, rkEnumeration, rkSet,
       {$ifdef FPC}rkBool, {$endif} rkClass]) then
    result := GetOrdProp(Instance)
  else
    result := -1;
end;

function TRttiProp.GetInt64Value(Instance: TObject): Int64;
begin
  if (Instance <> nil) and
     (@self <> nil) then
    case TypeInfo^.Kind of
      rkInteger, rkEnumeration, rkSet, rkChar, rkWChar,
      rkClass {$ifdef FPC}, rkBool{$endif}:
        result := GetOrdProp(Instance);
      rkInt64 {$ifdef FPC}, rkQWord{$endif}:
        result := GetInt64Prop(Instance);
    else
      result := 0;
    end
  else
    result := 0;
end;

procedure TRttiProp.GetCurrencyValue(Instance: TObject; var Value: currency);
begin
  if (Instance <> nil) and
     (@self <> nil) then
    with TypeInfo^ do
      if Kind = rkFloat then
        if RttiFloat = rfCurr then
          GetCurrencyProp(Instance, Value)
        else
          DoubleToCurrency(GetFloatProp(Instance), Value)
      else
        PInt64(@Value)^ := 0
  else
    PInt64(@Value)^ := 0;
end;

function TRttiProp.GetDoubleValue(Instance: TObject): double;
begin
  if (Instance <> nil) and
     (@self <> nil) and
     (TypeInfo^.Kind = rkFloat) then
    result := GetFloatProp(Instance)
  else
    result := 0;
end;

procedure TRttiProp.SetDoubleValue(Instance: TObject; const Value: double);
begin
  if (Instance <> nil) and
     (@self <> nil) and
     (TypeInfo^.Kind = rkFloat) then
    SetFloatProp(Instance, Value);
end;

procedure TRttiProp.GetRawByteStringValue(Instance: TObject; var Value: RawByteString);
begin
  if (Instance <> nil) and
     (@self <> nil) and
     (TypeInfo^.Kind in [{$ifdef FPC}rkLStringOld, {$endif} rkLString]) then
    GetLongStrProp(Instance, Value)
  else
    FastAssignNew(Value, nil);
end;

procedure TRttiProp.SetOrdValue(Instance: TObject; Value: PtrInt);
begin
  if (Instance <> nil) and
     (@self <> nil) and
     (TypeInfo^.Kind in [rkInteger, rkEnumeration, rkSet,
       {$ifdef FPC}rkBool, {$endif}rkClass]) then
    SetOrdProp(Instance, Value);
end;

procedure TRttiProp.SetInt64Value(Instance: TObject; Value: Int64);
begin
  if (Instance <> nil) and
     (@self <> nil) then
    case TypeInfo^.Kind of
      rkInteger, rkEnumeration, rkSet, rkChar, rkWChar,
      rkClass {$ifdef FPC}, rkBool{$endif}:
        SetOrdProp(Instance, Value);
      rkInt64{$ifdef FPC}, rkQWord{$endif}:
        SetInt64Prop(Instance, Value);
    end;
end;

{$ifdef HASVARUSTRING}

function TRttiProp.GetUnicodeStrValue(Instance: TObject): UnicodeString;
begin
  if (Instance <> nil) and
     (@self <> nil) and
     (TypeInfo^.Kind = rkUString) then
    GetUnicodeStrProp(Instance, result{%H-})
  else
    result := '';
end;

procedure TRttiProp.SetUnicodeStrValue(Instance: TObject; const Value: UnicodeString);
begin
  if (Instance <> nil) and
     (@self <> nil) and
     (TypeInfo^.Kind = rkUString) then
    SetUnicodeStrProp(Instance, Value);
end;

{$endif HASVARUSTRING}

procedure TRttiProp.GetAsString(Instance: TObject; var Value: RawUtf8);
var
  v: PtrInt;
  WS: WideString;
  {$ifdef HASVARUSTRING}
  US: UnicodeString;
  {$endif HASVARUSTRING}
begin
  case TypeInfo^.Kind of
    rkChar, rkWChar:
    begin
      v := GetOrdProp(Instance);
      if TypeInfo^.Kind = rkChar then
        FastSetString(Value, @v, {ansicharcount=}1)
      else
        RawUnicodeToUtf8(@v, {widecharcount=}1, Value);
    end;
    rkSString:
      GetShortStrProp(Instance, Value);
    rkLString:
      GetLongStrProp(Instance, RawByteString(Value));
    rkWString:
    begin
      GetWideStrProp(Instance, WS);
      RawUnicodeToUtf8(pointer(WS), length(WS), Value);
    end;
    {$ifdef HASVARUSTRING}
    rkUString:
    begin
      GetUnicodeStrProp(Instance, US);
      RawUnicodeToUtf8(pointer(US), length(US), Value);
    end;
    {$endif HASVARUSTRING}
  else
    Value := '';
  end;
end;

function TRttiProp.SetAsString(Instance: TObject; const Value: RawUtf8): boolean;
var
  v: PtrInt;
  P: PUtf8Char;
begin
  result := true;
  case TypeInfo^.Kind of
    rkChar, rkWChar:
    begin
      if Value = '' then
        v := 0
      else if TypeInfo^.Kind = rkChar then
        v := ord(Value[1])
      else
      begin
        P := pointer(Value);
        v := NextUtf8Ucs4(P);
      end;
      SetOrdProp(Instance, v);
    end;
    rkLString:
      SetLongStrProp(Instance, Value);
    rkWString:
      SetWideStrProp(Instance, Utf8ToWideString(Value));
    {$ifdef HASVARUSTRING}
    rkUString:
      SetUnicodeStrProp(Instance, Utf8DecodeToUnicodeString(Value));
    {$endif HASVARUSTRING}
  else
    result := false; // unsupported type
  end;
end;

function ToText(k: TRttiKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TRttiKind), ord(k));
end;

function ToText(t: TRttiParserType): PShortString;
begin
  result := GetEnumName(TypeInfo(TRttiParserType), ord(t));
end;


{ **************** Published Class Properties and Methods RTTI }

function GetRttiClass(RttiClass: TClass): PRttiClass;
begin
  result := PRttiInfo(PPointer(PAnsiChar(RttiClass) + vmtTypeInfo)^)^.RttiClass;
end;

function ClassHasPublishedFields(ClassType: TClass): boolean;
var
  cp: PRttiProps;
begin
  result := true;
  while ClassType <> nil do
  begin
    cp := GetRttiProps(ClassType);
    if cp = nil then
      break; // no RTTI information (e.g. reached TObject level)
    if cp^.PropCount > 0 then
      exit;
    ClassType := GetClassParent(ClassType);
  end;
  result := false;
end;

function ClassHierarchyWithField(ClassType: TClass): TClassDynArray;

  procedure InternalAdd(C: TClass; var list: TClassDynArray);
  var
    P: PRttiProps;
  begin
    if C = nil then
      exit;
    InternalAdd(GetClassParent(C), list);
    P := GetRttiProps(C);
    if (P <> nil) and
       (P^.PropCount > 0) then
      PtrArrayAdd(list, pointer(C));
  end;

begin
  result := nil;
  InternalAdd(ClassType, result);
end;

function ClassFieldAllProps(ClassType: TClass; Types: TRttiKinds): PRttiPropDynArray;
var
  CP: PRttiProps;
  P: PRttiProp;
  i, n: integer;
begin
  n := 0;
  result := nil;
  while ClassType <> nil do
  begin
    CP := GetRttiProps(ClassType);
    if CP = nil then
      break; // no RTTI information (e.g. reached TObject level)
    if CP^.PropCount > 0 then
    begin
      SetLength(result, n + CP^.PropCount);
      P := CP^.PropList;
      for i := 1 to CP^.PropCount do
      begin
        if P^.TypeInfo^.Kind in Types then
        begin
          result[n] := P;
          inc(n);
        end;
        P := P^.Next
      end;
    end;
    ClassType := GetClassParent(ClassType);
  end;
  SetLength(result,n);
end;

function ClassFieldNamesAllProps(ClassType: TClass; IncludePropType: boolean;
  Types: TRttiKinds): TRawUtf8DynArray;
var
  props: PRttiPropDynArray;
  n, i: PtrInt;
begin
  result := nil;
  props := ClassFieldAllProps(ClassType, Types); // recursive in-order list
  n := length(props);
  SetLength(result, n);
  for i := 0 to n - 1 do
    with props[i]^ do
      if IncludePropType then
        FormatUtf8('%: %', [Name^, TypeInfo^.Name^], result[i])
      else
        ShortStringToAnsi7String(Name^, result[i]);
end;

function ClassFieldNamesAllPropsAsText(ClassType: TClass; IncludePropType: boolean;
  Types: TRttiKinds): RawUtf8;
begin
  result := RawUtf8ArrayToCsv(
    ClassFieldNamesAllProps(ClassType, IncludePropType, Types), ', ');
end;

function ClassFieldProp(ClassType: TClass; const PropName: shortstring): PRttiProp;
begin
  if ClassType <> nil then
    result := GetRttiProps(ClassType)^.FieldProp(PropName)
  else
    result := nil;
end;

function ClassFieldPropWithParents(aClassType: TClass; const aPropName: shortstring;
  aCaseSensitive: boolean): PRttiProp;
var
  n, i: integer;
begin
  while aClassType <> nil do
  begin
    n := GetRttiProp(aClassType, result);
    if n <> 0 then
      if aCaseSensitive then
        for i := 1 to n do
          if result^.Name^ = aPropName then
            exit
          else
            result := result^.Next
      else
        for i := 1 to n do
          if IdemPropName(result^.Name^, @aPropName[1], ord(aPropName[0])) then
            exit
          else
            result := result^.Next;
    aClassType := GetClassParent(aClassType);
  end;
  result := nil;
end;

function ClassFieldPropWithParentsFromUtf8(aClassType: TClass; PropName: PUtf8Char;
  PropNameLen: integer; aCaseSensitive: boolean): PRttiProp;
var
  n, i: integer;
begin
  if PropNameLen <> 0 then
    while aClassType <> nil do
    begin
      n := GetRttiProp(aClassType, result);
      if n <> 0 then
        if aCaseSensitive then
          for i := 1 to n do
            if (result^.Name^[0] = AnsiChar(PropNameLen)) and
               CompareMemFixed(@result^.Name^[1], PropName, PropNameLen) then
              exit
            else
              result := result^.Next
        else
          for i := 1 to n do
            if IdemPropName(result^.Name^, PropName, PropNameLen) then
              exit
            else
              result := result^.Next;
      aClassType := GetClassParent(aClassType);
    end;
  result := nil;
end;

function ClassFieldPropWithParentsFromClassType(aClassType,
  aSearchedClassType: TClass): PRttiProp;
var
  i: integer;
begin
  if aSearchedClassType <> nil then
    while aClassType <> nil do
    begin
      for i := 1 to GetRttiProp(aClassType, result) do
        with result^.TypeInfo^ do
          if (Kind = rkClass) and
             (RttiNonVoidClass^.RttiClass = aSearchedClassType) then
            exit
          else
            result := result^.Next;
      aClassType := GetClassParent(aClassType);
    end;
  result := nil;
end;

function ClassFieldPropWithParentsInheritsFromClassType(aClassType,
  aSearchedClassType: TClass): PRttiProp;
var
  i: integer;
begin
  if aSearchedClassType <> nil then
    while aClassType <> nil do
    begin
      for i := 1 to GetRttiProp(aClassType, result) do
        with result^.TypeInfo^ do
          if (Kind = rkClass) and
             InheritsFrom(aSearchedClassType) then
            exit
          else
            result := result^.Next;
      aClassType := GetClassParent(aClassType);
    end;
  result := nil;
end;

function ClassFieldPropWithParentsFromClassOffset(aClassType: TClass;
  aSearchedOffset: pointer): PRttiProp;
var
  i: integer;
begin
  if aSearchedOffset <> nil then
    while aClassType <> nil do
    begin
      for i := 1 to GetRttiProp(aClassType, result) do
        if result^.GetFieldAddr(nil) = aSearchedOffset then
          exit
        else
          result := result^.Next;
      aClassType := GetClassParent(aClassType);
    end;
  result := nil;
end;

function ClassFieldInstance(Instance: TObject; const PropName: shortstring;
  PropClassType: TClass; out PropInstance): boolean;
var
  P: PRttiProp;
begin
  result := false;
  if Instance = nil then
    exit;
  P := ClassFieldPropWithParents(PPointer(Instance)^, PropName);
  if P = nil then
    exit;
  with P^.TypeInfo^ do
    if (Kind <> rkClass) or
       not InheritsFrom(PropClassType) then
      exit;
  TObject(PropInstance) := P^.GetObjProp(Instance);
  result := true;
end;

function ClassFieldInstance(Instance: TObject; PropClassType: TClass;
  out PropInstance): boolean;
var
  P: PRttiProp;
begin
  result := false;
  if (Instance = nil) or
     (PropClassType = nil) then
    exit;
  P := ClassFieldPropWithParentsFromClassType(PPointer(Instance)^, PropClassType);
  if P = nil then
    exit;
  TObject(PropInstance) := P^.GetObjProp(Instance);
  result := true;
end;

function ClassFieldInt64(Instance: TObject; const PropName: ShortString;
  out PropValue: Int64): boolean;
var
  P: PRttiProp;
begin
  result := false;
  if Instance = nil then
    exit;
  P := ClassFieldPropWithParents(PPointer(Instance)^, PropName);
  if P = nil then
    exit;
  PropValue := P^.GetInt64Value(Instance);
  result := true;
end;

function ClassFieldInstances(Instance: TObject; PropClassType: TClass): TObjectDynArray;
var
  nested: PRttiPropDynArray;
  i: PtrInt;
begin
  result := nil;
  if (Instance = nil) or
     (PropClassType = nil) then
    exit;
  nested := ClassFieldAllProps(PPointer(Instance)^, [rkClass]);
  for i := 0 to high(nested) do
    with nested[i]^ do
      if TypeInfo^.InheritsFrom(PropClassType) then
        ObjArrayAdd(result, GetObjProp(Instance));
end;

function ClassFieldPropInstanceMatchingClass(
  aSearchedInstance: TObject; aSearchedClassType: TClass): TObject;
var
  P: PRttiProp;
begin
  result := aSearchedInstance;
  if (aSearchedInstance = nil) or
     aSearchedInstance.InheritsFrom(aSearchedClassType) then
    exit;
  P := ClassFieldPropWithParentsFromClassType(
    PPointer(aSearchedInstance)^, aSearchedClassType);
  if P <> nil then
  begin
    result := P^.GetObjProp(aSearchedInstance);
    if result = nil then
      result := aSearchedInstance;
  end;
end;

function ClassFieldCountWithParents(ClassType: TClass; onlyWithoutGetter: boolean): integer;
var
  cp: PRttiProps;
  p: PRttiProp;
  i: integer;
begin
  result := 0;
  while ClassType <> nil do
  begin
    cp := GetRttiProps(ClassType);
    if cp = nil then
      break; // no RTTI information (e.g. reached TObject level)
    p := cp^.PropList;
    for i := 1 to cp^.PropCount do
    begin
      if (not onlyWithoutGetter) or
         p^.GetterIsField then
        inc(result);
      p := p^.Next;
    end;
    ClassType := GetClassParent(ClassType);
  end;
end;


{ *************** Enumerations RTTI }

function GetEnumType(aTypeInfo: PRttiInfo; out List: PShortString): integer;
begin
  with aTypeInfo^.EnumBaseType^ do
  begin
    List := NameList;
    result := MaxValue;
  end;
end;

function GetEnumNameTrimed(aTypeInfo: PRttiInfo; aIndex: integer): RawUtf8;
begin
  result := TrimLeftLowerCaseShort(GetEnumName(aTypeInfo, aIndex));
end;

procedure GetEnumNames(aTypeInfo: PRttiInfo; aDest: PPShortString);
var
  info: PRttiEnumType;
  p: PShortString;
  i: PtrInt;
begin
  info := aTypeInfo^.EnumBaseType;
  if info <> nil then
  begin
    p := info^.NameList;
    for i := 0 to info^.MaxValue do
    begin
      aDest^ := p;
      p := @PByteArray(p)^[ord(p^[0]) + 1];
      inc(aDest);
    end;
  end;
end;

procedure GetEnumTrimmedNames(aTypeInfo: PRttiInfo; aDest: PRawUtf8);
var
  info: PRttiEnumType;
  p: PShortString;
  i: PtrInt;
begin
  info := aTypeInfo^.EnumBaseType;
  if info <> nil then
  begin
    p := info^.NameList;
    for i := 0 to info^.MaxValue do
    begin
      aDest^ := TrimLeftLowerCaseShort(p);
      p := @PByteArray(p)^[ord(p^[0]) + 1];
      inc(aDest);
    end;
  end;
end;

function GetEnumTrimmedNames(aTypeInfo: PRttiInfo): TRawUtf8DynArray;
begin
  aTypeInfo^.EnumBaseType^.GetEnumNameAll(result{%H-}, {trim=}true);
end;

function GetEnumNameValue(aTypeInfo: PRttiInfo; aValue: PUtf8Char;
  aValueLen: PtrInt; AlsoTrimLowerCase: boolean): integer;
begin
  result := aTypeInfo^.EnumBaseType^.
    GetEnumNameValue(aValue, aValueLen, AlsoTrimLowerCase);
end;

function GetEnumNameValueTrimmed(aTypeInfo: PRttiInfo; aValue: PUtf8Char;
  aValueLen: PtrInt): integer;
begin
  result := aTypeInfo^.EnumBaseType^.
    GetEnumNameValueTrimmed(aValue, aValueLen, {exact=}false);
end;

function GetEnumNameValueTrimmedExact(aTypeInfo: PRttiInfo; aValue: PUtf8Char;
  aValueLen: PtrInt): integer;
begin
  result := aTypeInfo^.EnumBaseType^.
    GetEnumNameValueTrimmed(aValue, aValueLen, {exact=}true);
end;

function GetEnumNameValue(aTypeInfo: PRttiInfo; const aValue: RawUtf8;
  AlsoTrimLowerCase: boolean): integer;
begin
  result := aTypeInfo^.EnumBaseType^.
    GetEnumNameValue(pointer(aValue), length(aValue), AlsoTrimLowerCase);
end;

procedure SetEnumFromOrdinal(aTypeInfo: PRttiInfo; out Value; Ordinal: PtrUInt);
begin
  aTypeInfo^.EnumBaseType^.SetEnumFromOrdinal(Value, Ordinal);
end;

function GetSetName(aTypeInfo: PRttiInfo; const value): RawUtf8;
var
  info: PRttiEnumType;
  PS: PShortString;
  i: PtrInt;
begin
  result := '';
  info := aTypeInfo^.SetEnumType;
  if info <> nil then
  begin
    PS := info^.NameList;
    for i := 0 to info^.MaxValue do
    begin
      if GetBitPtr(@value, i) then
        result := FormatUtf8('%%,', [result, PS^]);
      inc(PByte(PS), PByte(PS)^ + 1); // next
    end;
    if result <> '' then
      SetLength(result, length(result) - 1); // trim last comma
  end;
end;

procedure GetSetNameShort(aTypeInfo: PRttiInfo; const value;
  out result: ShortString; trimlowercase: boolean);
var
  info: PRttiEnumType;
  PS: PShortString;
  i: PtrInt;
begin
  result := '';
  info := aTypeInfo^.SetEnumType;
  if info <> nil then
  begin
    PS := info^.NameList;
    for i := 0 to info^.MaxValue do
    begin
      if GetBitPtr(@value, i) then
        AppendShortComma(@PS^[1], PByte(PS)^, result, trimlowercase);
      inc(PByte(PS), PByte(PS)^ + 1); // next
    end;
    if result[ord(result[0])] = ',' then
      dec(result[0]);
  end;
end;

procedure GetCaptionFromTrimmed(PS: PShortString; var result: string);
var
  tmp: array[byte] of AnsiChar;
  L: integer;
begin
  L := ord(PS^[0]);
  inc(PByte(PS));
  while (L > 0) and
        (PS^[0] in ['a'..'z']) do
  begin
    inc(PByte(PS));
    dec(L);
  end;
  tmp[L] := #0; // as expected by GetCaptionFromPCharLen/UnCamelCase
  if L > 0 then
    MoveSmall(PS, @tmp, L);
  GetCaptionFromPCharLen(tmp, result);
end;

procedure GetEnumCaptions(aTypeInfo: PRttiInfo; aDest: PString);
var
  MaxValue, i: integer;
  res: PShortString;
begin
  aTypeInfo^.EnumBaseType(res, MaxValue);
  if res <> nil then
    for i := 0 to MaxValue do
    begin
      GetCaptionFromTrimmed(res, aDest^);
      inc(PByte(res), PByte(res)^ + 1); // next
      inc(aDest);
    end;
end;

function GetCaptionFromEnum(aTypeInfo: PRttiInfo; aIndex: integer): string;
begin
  GetCaptionFromTrimmed(GetEnumName(aTypeInfo, aIndex), result{%H-});
end;

function GetDisplayNameFromClass(C: TClass): RawUtf8;
var
  name: PShortString;
  totrim: integer;
begin
  if C = nil then
  begin
    result := '';
    exit;
  end;
  name := ClassNameShort(C);
  totrim := 0;
  if name^[0] > #4 then
    // fast case-insensitive compare
    case PInteger(@name^[1])^ and $DFDFDFDF of
      // backward compatibility trim of left-sided TSql* or TSqlRecord*
      ord('T') + ord('S') shl 8 + ord('Q') shl 16 + ord('L') shl 24:
        if (name^[0] <= #10) or
           (PInteger(@name^[5])^ and $DFDFDFDF <>
            ord('R') + ord('E') shl 8 + ord('C') shl 16 + ord('O') shl 24) or
           (PWord(@name^[9])^ and $DFDF <> ord('R') + ord('D')shl 8) then
          totrim := 4
        else
          totrim := 10;
      // trim left-sided TOrm* and TSyn* naming conventions
      ord('T') + ord('O') shl 8 + ord('R') shl 16 + ord('M') shl 24,
      ord('T') + ord('S') shl 8 + ord('Y') shl 16 + ord('N') shl 24:
        totrim := 4;
    end;
  if (totrim = 0) and
     (name^[1] = 'T') then
    // trim left-sided T* from regular Delphi/FPC type
    totrim := 1;
  FastSetString(result, @name^[totrim + 1], ord(name^[0]) - totrim);
end;

function GetCaptionFromClass(C: TClass): string;
var
  tmp: RawUtf8;
  P: PUtf8Char;
begin
  if C = nil then
    result := ''
  else
  begin
    tmp := ToText(C);
    P := pointer(tmp);
    if IdemPChar(P, 'TSQL') or
       IdemPChar(P, 'TORM') or
       IdemPChar(P, 'TSYN') then
      inc(P, 4)
    else if P^ = 'T' then
       inc(P);
    GetCaptionFromPCharLen(P, result);
  end;
end;

function ToText(cmd: TParseCommands): shortstring;
begin
  if cmd = [] then
    result[0] := #0
  else
    GetSetNameShort(TypeInfo(TParseCommands), cmd, result, {trim=}true);
end;


{ ***************** IInvokable Interface RTTI }

procedure TGetRttiInterface.AddMethod(const aMethodName: ShortString;
  aParamCount: integer; aKind: TMethodKind);
var
  i: PtrInt;
begin
  CurrentMethod := @Definition.Methods[MethodCount];
  ShortStringToAnsi7String(aMethodName, CurrentMethod^.Name);
  for i := 0 to MethodCount - 1 do
    if IdemPropNameU(Definition.Methods[i].Name, CurrentMethod^.Name) then
      RaiseError('duplicated method name', []);
  CurrentMethod^.HierarchyLevel := Level;
  if aKind = mkFunction then
    inc(aParamCount);
  SetLength(CurrentMethod^.Args, aParamCount);
  CurrentMethod^.IsFunction := aKind = mkFunction;
  inc(MethodCount);
  ArgCount := 0;
end;

procedure TGetRttiInterface.AddArgument(aParamName, aTypeName: PShortString;
  aInfo: PRttiInfo; aFlags: TParamFlags);
var
  a: PRttiMethodArg;
begin
  a := @CurrentMethod^.Args[ArgCount];
  inc(ArgCount);
  if {$ifdef FPC} pfSelf in aFlags {$else} ArgCount = 1 {$endif} then
    a^.ParamName := @PSEUDO_SELF_NAME
  else if aParamName = nil then
  begin
    a^.ParamName := @PSEUDO_RESULT_NAME;
    include(aFlags, pfOut); // result is an "out"
  end
  else
    a^.ParamName := aParamName;
  a^.TypeInfo := aInfo;
  if aTypeName = nil then
    aTypeName := aInfo^.Name;
  a^.TypeName := aTypeName;
  if ArgCount > 1 then
    if aInfo^.Kind in rkRecordOrDynArrayTypes  then
    begin
      if aFlags * [pfConst, pfVar, pfOut] = [] then
        RaiseError('%: % parameter should be declared as const, var or out',
          [a^.ParamName^, aTypeName^]);
    end
    else if aInfo^.Kind = rkInterface then
      if not (pfConst in aFlags) then
        RaiseError('%: % parameter should be declared as const',
          [a^.ParamName^, aTypeName^]);
  if aParamName = nil then
    a^.Direction := rmdResult
  else if pfVar in aFlags then
    a^.Direction := rmdVar
  else if pfOut in aFlags then
    a^.Direction := rmdOut;
end;

procedure TGetRttiInterface.RaiseError(const Format: RawUtf8;
  const Args: array of const);
var
  m: RawUtf8;
begin
  if CurrentMethod <> nil then
    m := '.' + CurrentMethod^.Name;
  raise ERttiException.CreateUtf8('GetRttiInterface(%%) failed - %',
    [Definition.Name, {%H-}m, FormatUtf8(Format, Args)]);
end;

function GetRttiInterface(aTypeInfo: PRttiInfo;
  out aDefinition: TRttiInterface): integer;
var
  getter: TGetRttiInterface;
begin
  getter := TGetRttiInterface.Create;
  try
    getter.AddMethodsFromTypeInfo(pointer(aTypeInfo));
    aDefinition := getter.Definition;
  finally
    getter.Free;
  end;
  result := length(aDefinition.Methods);
end;

function GetInterfaceFromEntry(Instance: TObject; Entry: PInterfaceEntry;
  out Obj): boolean;
begin
  result := false;
  pointer(Obj) := nil;
  if Entry <> nil then
    {$ifdef FPC}
    if Entry^.IType = etStandard then
    {$else}
    if Entry^.IOffset <> 0 then
    {$endif FPC}
    begin
      // fast interface retrieval from the interface field instance
      Pointer(Obj) := Pointer(PAnsiChar(Instance) + Entry^.IOffset);
      if Pointer(Obj) <> nil then
      begin
        IInterface(Obj)._AddRef;
        result := true;
        exit;
      end;
    end
    else
      // there is a getter method -> use slower but safe RTL method
      result := Instance.GetInterface(Entry^.IID{$ifdef FPC}^{$endif}, Obj);
end;

function GetRttiClassGuid(aClass: TClass): PGuidDynArray;
var
  T: PInterfaceTable;
  n, i: PtrInt;
begin
  result := nil;
  n := 0;
  while aClass <> nil do
  begin
    T := aClass.GetInterfaceTable;
    if (T <> nil) and
       (T^.EntryCount > 0) then
    begin
      SetLength(result, length(result) + PtrInt(T^.EntryCount));
      for i := 0 to T^.EntryCount - 1 do
      begin
        result[n] := {$ifndef FPC}@{$endif}T^.Entries[i].IID;
        inc(n);
      end;
    end;
    aClass := GetClassParent(aClass);
  end;
end;


{ ************* Efficient Dynamic Arrays and Records Process }

procedure VariantDynArrayClear(var Value: TVariantDynArray);
begin
  FastDynArrayClear(@Value, TypeInfo(variant));
end;

procedure RawUtf8DynArrayClear(var Value: TRawUtf8DynArray);
begin
  FastDynArrayClear(@Value, TypeInfo(RawUtf8));
end;

function IsRawUtf8DynArray(Info: PRttiInfo): boolean;
var
  r: TRttiCustom;
begin
  r := Rtti.RegisterType(Info);
  if r <> nil then
    r := r.ArrayRtti;
  result := (r <> nil) and
            (r.Parser = ptRawUtf8) and
            (r.Cache.CodePage = CP_UTF8);
end;

procedure _RecordClearSeveral(v: PAnsiChar; info: PRttiInfo; n: integer);
var
  fields: TRttiRecordManagedFields;
  f: PRttiRecordField;
  p: PRttiInfo;
  i: PtrInt;
  fin: PRttiFinalizers;
begin
  info.RecordManagedFields(fields); // retrieve RTTI once for n items
  if fields.Count > 0 then
  begin
    fin := @RTTI_FINALIZE;
    repeat
      f := fields.Fields;
      i := fields.Count;
      repeat
        p := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
        {$ifdef FPC_OLDRTTI}
        if Assigned(fin[p^.Kind]) then
        {$endif FPC_OLDRTTI}
          fin[p^.Kind](v + f^.Offset, p);
        inc(f);
        dec(i);
      until i = 0;
      inc(v, fields.Size);
      dec(n);
    until n = 0;
  end;
end;

procedure _StringClearSeveral(v: PPointer; n: PtrInt);
var
  p: PStrRec;
begin
  repeat
    p := v^;
    if p <> nil then
    begin
      v^ := nil;
      dec(p);
      if (p^.refCnt >= 0) and
         RefCntDecFree(p^.refCnt) then
        {$ifdef FPC_X64MM}
        _Freemem(p); // works for both rkLString + rkUString
        {$else}
        Freemem(p);
        {$endif FPC_X64MM}
    end;
    inc(v);
    dec(n);
  until n = 0;
end;

procedure FastFinalizeArray(Value: PPointer; ElemTypeInfo: PRttiInfo;
  Count: integer);
var
  fin: TRttiFinalizer;
begin
  //  caller ensured ElemTypeInfo<>nil and Count>0
  case ElemTypeInfo^.Kind of
    {$ifdef FPC}
    rkObject,
    {$endif FPC}
    rkRecord:
      // retrieve ElemTypeInfo.RecordManagedFields once
      _RecordClearSeveral(pointer(Value), ElemTypeInfo, Count);
    {$ifdef FPC}
    rkLStringOld,
    {$endif FPC}
    {$ifdef HASVARUSTRING}
    rkUString,
    {$endif HASVARUSTRING}
    rkLString:
      // optimized loop for AnsiString / UnicodeString (PStrRec header)
      _StringClearSeveral(pointer(Value), Count);
    rkVariant:
      // from mormot.core.variants - supporting custom variants
      // or at least from mormot.core.base
      VariantClearSeveral(pointer(Value), Count);
    else
      begin
        // regular finalization
        fin := RTTI_FINALIZE[ElemTypeInfo^.Kind];
        if Assigned(fin) then  // e.g. rkWString, rkArray, rkDynArray
          repeat
            inc(PByte(Value), fin(PByte(Value), ElemTypeInfo));
            dec(Count);
          until Count = 0;
      end;
  end;
end;

procedure FastDynArrayClear(Value: PPointer; ElemInfo: PRttiInfo);
var
  p: PDynArrayRec;
begin
  if Value <> nil then
  begin
    p := Value^;
    if p <> nil then
    begin
      dec(p);
      if (p^.refCnt >= 0) and
         RefCntDecFree(p^.refCnt) then
      begin
        if ElemInfo <> nil then
          FastFinalizeArray(Value^, ElemInfo, p^.length);
        Freemem(p);
      end;
      Value^ := nil;
    end;
  end;
end;

function FastRecordClear(Value: pointer; Info: PRttiInfo): PtrInt;
var
  fields: TRttiRecordManagedFields;
  f: PRttiRecordField;
  p: PRttiInfo;
  n: PtrInt;
  fin: PRttiFinalizers;
begin
  // caller ensured Info is indeed a record/object
  Info.RecordManagedFields(fields);
  n := fields.Count;
  if n > 0 then
  begin
    fin := @RTTI_FINALIZE;
    f := fields.Fields;
    repeat
      p := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
      {$ifdef FPC_OLDRTTI}
      if Assigned(fin[p^.Kind]) then
      {$endif FPC_OLDRTTI}
        fin[p^.Kind](PAnsiChar(Value) + f^.Offset, p);
      inc(f);
      dec(n);
    until n = 0;
  end;
  result := fields.Size;
end;

procedure RecordZero(Dest: pointer; Info: PRttiInfo);
begin
  if Info^.Kind in rkRecordTypes then
    FillCharFast(Dest^, FastRecordClear(Dest, Info), 0);
end;

procedure RecordCopy(var Dest; const Source; Info: PRttiInfo);
begin
  if Info^.Kind in rkRecordTypes then
    RTTI_COPY[rkRecord](@Dest, @Source, Info);
end;

procedure _RecordCopySeveral(Dest, Source: PAnsiChar; n: PtrInt; Info: PRttiInfo);
var
  fields: TRttiRecordManagedFields;
  f: PRttiRecordField;
  p: PRttiInfo;
  i, offset: PtrUInt;
begin
  Info^.RecordManagedFields(fields); // retrieve RTTI once for all items
  repeat
    i := fields.Count;
    offset := 0;
    if i > 0 then
    begin
      f := fields.Fields;
      repeat
        p := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
        {$ifdef FPC_OLDRTTI}
        if Info^.Kind in rkManagedTypes then
        {$endif FPC_OLDRTTI}
        begin
          offset := f^.Offset - offset;
          if offset <> 0 then
          begin
            MoveFast(Source^, Dest^, offset);
            inc(Source, offset);
            inc(Dest, offset);
          end;
          offset := RTTI_COPY[p^.Kind](Dest, Source, p);
          inc(Source, offset);
          inc(Dest, offset);
          inc(offset, f^.Offset);
        end;
        inc(f);
        dec(i);
      until i = 0;
    end;
    offset := PtrUInt(fields.Size) - offset;
    if offset <> 0 then
      MoveFast(Source^, Dest^, offset);
    dec(n);
  until n = 0;
end;

procedure CopySeveral(Dest, Source: PByte; SourceCount: PtrInt;
  ItemInfo: PRttiInfo; ItemSize: PtrInt);
var
  cop: TRttiCopier;
  elemsize: PtrInt;
label
  raw;
begin
  if SourceCount > 0 then
    if ItemInfo = nil then
raw:  MoveFast(Source^, Dest^, ItemSize * SourceCount)
    else if ItemInfo^.Kind in rkRecordTypes then
      // retrieve record/object RTTI once for all items
      _RecordCopySeveral(pointer(Dest), pointer(Source), SourceCount, ItemInfo)
    else
    begin
      // loop the TRttiCopier function over all items
      cop := RTTI_COPY[ItemInfo^.Kind];
      if Assigned(cop) then
        repeat
          elemsize := cop(Dest, Source, ItemInfo);
          inc(Source, elemsize);
          inc(Dest, elemsize);
          dec(SourceCount);
        until SourceCount = 0
      else
        goto raw;
    end;
end;

function DynArrayNew(Dest: PPointer; Count, ItemSize: PtrInt): pointer;
begin
  result := AllocMem(Count * ItemSize +  SizeOf(TDynArrayRec));
  PDynArrayRec(result)^.refCnt := 1;
  PDynArrayRec(result)^.length := Count;
  inc(PDynArrayRec(result));
  Dest^ := result;
end;

function DynArrayGrow(Dest: PPointer; Count, ItemSize: PtrInt): PAnsiChar;
var
  old: PtrInt;
begin
  result := Dest^;
  dec(PDynArrayRec(result));
  ReallocMem(result, (Count * ItemSize) + SizeOf(TDynArrayRec));
  old := PDynArrayRec(result)^.length;
  PDynArrayRec(result)^.length := Count;
  inc(PDynArrayRec(result));
  FillCharFast(result[old * ItemSize], (Count - old) * ItemSize, 0);
  Dest^ := result;
end;

procedure DynArrayCopy(Dest, Source: PPointer; Info: PRttiInfo;
  SourceExtCount: PInteger);
var
  n, itemsize: PtrInt;
  iteminfo: PRttiInfo;
begin
  iteminfo := Info^.DynArrayItemType(itemsize);
  if Dest^ <> nil then
    FastDynArrayClear(Dest, iteminfo);
  Source := Source^;
  if Source <> nil then
  begin
    if SourceExtCount <> nil then
      n := SourceExtCount^
    else
      n := PDALen(PAnsiChar(Source) - _DALEN)^ + _DAOFF;
    DynArrayNew(Dest, n, itemsize); // allocate zeroed memory
    CopySeveral(Dest^, pointer(Source), n, iteminfo, itemsize);
  end;
end;

procedure DynArrayEnsureUnique(Value: PPointer; Info: PRttiInfo);
var
  p: PDynArrayRec;
  n, elemsize: PtrInt;
begin
  p := Value^;
  Value^ := nil;
  dec(p);
  if (p^.refCnt < 0) or
     ((p^.refCnt > 1) and
      not RefCntDecFree(p^.refCnt)) then
  begin
    n := p^.length;
    Info := Info^.DynArrayItemType(elemsize);
    DynArrayNew(Value, n, elemsize); // allocate zeroed memory
    inc(p);
    CopySeveral(pointer(p), Value^, n, Info, elemsize);
  end;
end;


{ ************* Managed Types Finalization, Random or Copy }

{ RTTI_FINALIZE[] implementation functions }

function _StringClear(V: PPointer; Info: PRttiInfo): PtrInt;
var
  p: PStrRec;
begin
  p := V^;
  if p <> nil then // works for both rkLString + rkUString
  begin
    V^ := nil;
    dec(p);
    if (p^.refCnt >= 0) and
       RefCntDecFree(p^.refCnt) then
      Freemem(p);
  end;
  result := SizeOf(V^);
end;

function _WStringClear(V: PWideString; Info: PRttiInfo): PtrInt;
begin
  if V^ <> '' then
    {$ifdef FPC}
    Finalize(V^);
    {$else}
    V^ := '';
    {$endif FPC}
  result := SizeOf(V^);
end;

function _VariantClear(V: PVarData; Info: PRttiInfo): PtrInt;
begin
  {$ifdef CPUINTEL} // see VarClear: problems reported on ARM + BSD
  if PCardinal(V)^ and $BFE8 <> 0 then
  {$else}
  if V^.VType >= varOleStr then // bypass for most obvious types
  {$endif CPUINTEL}
    VarClearProc(V^);
  PCardinal(V)^ := 0;
  result := SizeOf(V^);
end;

function _InterfaceClear(V: PInterface; Info: PRttiInfo): PtrInt;
begin
  if V^ <> nil then
    {$ifdef FPC}
    Finalize(V^);
    {$else}
    V^ := nil;
    {$endif FPC}
  result := SizeOf(V^);
end;

function _DynArrayClear(V: PPointer; Info: PRttiInfo): PtrInt;
var
  p: PDynArrayRec;
begin
  p := V^;
  if p <> nil then
  begin
    dec(p);
    if (p^.refCnt >= 0) and
       RefCntDecFree(p^.refCnt) then
    begin
      Info := Info^.DynArrayItemType;
      if Info <> nil then
        FastFinalizeArray(V^, Info, p^.length);
      Freemem(p);
    end;
    V^ := nil;
  end;
  result := SizeOf(V^);
end;

function _ArrayClear(V: PByte; Info: PRttiInfo): PtrInt;
var
  n: PtrInt;
  fin: TRttiFinalizer;
begin
  Info := Info^.ArrayItemType(n, result);
  if Info = nil then
    FillCharFast(V^, result, 0)
  else
  begin
    fin := RTTI_FINALIZE[Info^.Kind];
    if Assigned(fin) then
      repeat
        inc(V, fin(V, Info));
        dec(n);
      until n = 0;
  end;
end;

function _ObjClear(V: PObject; Info: PRttiInfo): PtrInt;
begin
  if V^ <> nil then
  begin
    V^.Destroy;
    V^ := nil;
  end;
  result := SizeOf(V^);
end;

function _ObjArrayClear(V: PPointer; Info: PRttiInfo): PtrInt;
begin
  if V^ <> nil then
  begin
    RawObjectsClear(pointer(V), PDALen(PAnsiChar(V^) - _DALEN)^ + _DAOFF);
    _DynArrayClear(V, Info);
  end;
  result := SizeOf(V^);
end;


{ PT_RANDOM[] implementation functions }

procedure _NoRandom(V: PPointer; RC: TRttiCustom);
begin
end;

procedure _FillRandom(V: PByte; RC: TRttiCustom);
begin
  Rtti.SharedRandom.Fill(V, RC.Cache.Size);
end;

procedure _StringRandom(V: PPointer; RC: TRttiCustom);
var
  tmp: TShort31;
begin
  Rtti.SharedRandom.FillShort31(tmp);
  FastSetStringCP(V^, @tmp[1], ord(tmp[0]), RC.Cache.CodePage);
end;

procedure _WStringRandom(V: PWideString; RC: TRttiCustom);
var
  tmp: TShort31;
  i: PtrInt;
  W: PWordArray;
begin
  Rtti.SharedRandom.FillShort31(tmp);
  SetString(V^, PWideChar(nil), ord(tmp[0]));
  W := pointer(V^);
  for i := 1 to ord(tmp[0]) do
    W[i - 1] := cardinal(PByteArray(@tmp)[i]);
end;

{$ifdef HASVARUSTRING}
procedure _UStringRandom(V: PUnicodeString; RC: TRttiCustom);
var
  tmp: TShort31;
  i: PtrInt;
  W: PWordArray;
begin
  Rtti.SharedRandom.FillShort31(tmp);
  SetString(V^, PWideChar(nil), ord(tmp[0]));
  W := pointer(V^);
  for i := 1 to ord(tmp[0]) do
    W[i - 1] := cardinal(PByteArray(@tmp)[i]);
end;
{$endif HASVARUSTRING}

procedure _VariantRandom(V: PRttiVarData; RC: TRttiCustom);
begin
  {$ifdef CPUINTEL} // see VarClear: problems reported on ARM + BSD
  if V^.VType and $BFE8 <> 0 then
  {$else}
  if V^.DataType >= varOleStr then // bypass for most obvious types
  {$endif CPUINTEL}
    VarClearProc(V^.Data);
  V^.VType := varInteger;
  V^.Data.VInt64 := Rtti.SharedRandom.Next;
  // generate some 8-bit 32-bit 64-bit integers or a RawUtf8 varString
  case V^.Data.VInteger and 3 of
    0:
      V^.VType := varInteger;
    1:
      V^.VType := varInt64;
    2:
      V^.VType := varByte;
    3:
      begin
        V^.VType := varString;
        V^.Data.VAny := nil;
        _StringRandom(@V^.Data.VAny, RC);
      end;
  end;
end;

procedure _DoubleRandom(V: PDouble; RC: TRttiCustom);
begin
  V^ := Rtti.SharedRandom.NextDouble;
end;

procedure _DateTimeRandom(V: PDouble; RC: TRttiCustom);
begin
  V^ := 38000 + Int64(Rtti.SharedRandom.Next) / (maxInt shr 12);
end;

procedure _SingleRandom(V: PSingle; RC: TRttiCustom);
begin
  V^ := Rtti.SharedRandom.NextDouble;
end;

var
  PT_RANDOM: array[TRttiParserType] of pointer = (
    // ptNone, ptArray, ptBoolean, ptByte, ptCardinal, ptCurrency,
    @_NoRandom, @_NoRandom, @_FillRandom, @_FillRandom, @_FillRandom, @_FillRandom,
    // ptDouble, ptExtended, ptInt64, ptInteger, ptQWord,
    @_DoubleRandom, @_NoRandom, @_FillRandom, @_FillRandom, @_FillRandom,
    // ptRawByteString, ptRawJson, ptRawUtf8, ptRecord, ptSingle,
    @_StringRandom, @_NoRandom, @_StringRandom, @_NoRandom, @_SingleRandom,
    // ptString,
    {$ifdef UNICODE} @_UStringRandom, {$else} @_StringRandom, {$endif}
    // ptSynUnicode,
    {$ifdef HASVARUSTRING} @_UStringRandom {$else} @_WStringRandom {$endif},
    // ptDateTime, ptDateTimeMS, ptGuid, ptHash128,
    @_DateTimeRandom, @_DateTimeRandom, @_FillRandom, @_FillRandom,
    // ptHash256, ptHash512, ptOrm, ptTimeLog,
    @_FillRandom, @_FillRandom, @_NoRandom, @_FillRandom,
    // ptUnicodeString,
    {$ifdef HASVARUSTRING} @_UStringRandom {$else} @_NoRandom {$endif},
    // ptUnixTime, ptUnixMSTime, ptVariant, ptWideString,
    @_FillRandom, @_FillRandom, @_VariantRandom, @_WStringRandom,
    // ptWinAnsi, ptWord, ptEnumeration, ptSet,
    @_StringRandom, @_FillRandom, @_FillRandom, @_FillRandom,
    // ptClass, ptDynArray, ptInterface, ptCustom
    @_NoRandom, @_NoRandom, @_NoRandom, @_NoRandom);


{ RTTI_COPY[] implementation functions }

function _LStringCopy(Dest, Source: PRawByteString; Info: PRttiInfo): PtrInt;
begin
  if (Dest^ <> '') or
     (Source^ <> '') then
    Dest^ := Source^;
  result := SizeOf(Source^);
end;

{$ifdef HASVARUSTRING}
function _UStringCopy(Dest, Source: PUnicodeString; Info: PRttiInfo): PtrInt;
begin
  if (Dest^ <> '') or
     (Source^ <> '') then
    Dest^ := Source^;
  result := SizeOf(Source^);
end;
{$endif HASVARUSTRING}

function _WStringCopy(Dest, Source: PWideString; Info: PRttiInfo): PtrInt;
begin
  if (Dest^ <> '') or
     (Source^ <> '') then
    Dest^ := Source^;
  result := SizeOf(Source^);
end;

function _VariantCopy(Dest, Source: PVarData; Info: PRttiInfo): PtrInt;
var
  vt: cardinal;
label
  rtl, raw;
begin
  {$ifdef CPUINTEL} // see VarClear: problems reported on ARM + BSD
  if PCardinal(Dest)^ and $BFE8 <> 0 then
  {$else}
  if Dest^.VType >= varOleStr then
  {$endif CPUINTEL}
    VarClearProc(Dest^);
  vt := Source^.VType;
  PCardinal(Dest)^ := vt;
  if vt > varNull then
    // varEmpty,varNull need no copy
    if vt <= varWord64 then
      // most used types
      if (vt < varOleStr) or
         (vt > varError) then
raw:    // copy any simple value (e.g. ordinal, varByRef)
        Dest^.VInt64 := Source^.VInt64
      else if vt = varOleStr then
      begin
        // copy WideString with reference counting
        Dest^.VAny := nil;
        WideString(Dest^.VAny) := WideString(Source^.VAny)
      end
      else
        // varError, varDispatch
        goto rtl
    else if vt = varString then
    begin
      // copy AnsiString with reference counting
      Dest^.VAny := nil;
      RawByteString(Dest^.VAny) := RawByteString(Source^.VAny)
    end
    else if vt >= varByRef then
      // varByRef has no refcount -> copy VPointer
      goto raw
    {$ifdef HASVARUSTRING}
    else if vt = varUString then
    begin
      // copy UnicodeString with reference counting
      Dest^.VAny := nil;
      UnicodeString(Dest^.VAny) := UnicodeString(Source^.VAny)
    end
    {$endif HASVARUSTRING}
    else
rtl:  // copy any complex type via the RTL function of the variants unit
      VarCopyProc(Dest^, Source^);
  result := SizeOf(Source^);
end;

function _InterfaceCopy(Dest, Source: PInterface; Info: PRttiInfo): PtrInt;
begin
  Dest^ := Source^;
  result := SizeOf(Source^);
end;

function _RecordCopy(Dest, Source: PByte; Info: PRttiInfo): PtrInt;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset: PtrUInt;
  f: PRttiRecordField;
  cop: PRttiCopiers;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  cop := @RTTI_COPY; 
  offset := 0;
  while fields.Count <> 0 do
  begin
    dec(fields.Count);
    Info := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
    {$ifdef FPC_OLDRTTI}
    if Info^.Kind in rkManagedTypes then
    {$endif FPC_OLDRTTI}
    begin
      offset := f^.Offset - offset;
      if offset <> 0 then
      begin
        MoveFast(Source^, Dest^, offset);
        inc(Source, offset);
        inc(Dest, offset);
      end;
      offset := cop[Info^.Kind](Dest, Source, Info);
      inc(Source, offset);
      inc(Dest, offset);
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset > 0 then
    MoveFast(Source^, Dest^, offset);
  result := fields.Size;
end;

function _DynArrayCopy(Dest, Source: PPointer; Info: PRttiInfo): PtrInt;
begin
  DynArrayCopy(Dest, Source, Info, {extcount=}nil);
  result := SizeOf(Source^);
end;

function _ArrayCopy(Dest, Source: PByte; Info: PRttiInfo): PtrInt;
var
  n, itemsize: PtrInt;
  cop: TRttiCopier;
label
  raw;
begin
  Info := Info^.ArrayItemType(n, result);
  if Info = nil then
raw:MoveFast(Source^, Dest^, result)
  else
  begin
    cop := RTTI_COPY[Info^.Kind];
    if Assigned(cop) then
      repeat
        itemsize := cop(Dest ,Source, Info);
        inc(Source, itemsize);
        inc(Dest, itemsize);
        dec(n);
      until n = 0
    else
      goto raw;
  end;
end;


{ ************** RTTI Value Types used for JSON Parsing }

function ParserTypeToTypeInfo(pt: TRttiParserType;
  pct: TRttiParserComplexType): PRttiInfo;
begin
  result := PTC_INFO[pct];
  if result = nil then
    result := PT_INFO[pt];
end;

function TypeNameToStandardParserType(Name: PUtf8Char; NameLen: integer;
  Complex: PRttiParserComplexType): TRttiParserType;
const
  SORTEDMAX = 43;
  // fast branchless O(log(N)) binary search on x86_64
  SORTEDNAMES: array[0..SORTEDMAX] of PUtf8Char = (
    'ARRAY', 'BOOLEAN', 'BYTE', 'CARDINAL', 'CURRENCY', 'DOUBLE', 'EXTENDED',
    'INT64', 'INTEGER', 'INTERFACE', 'LONGINT', 'LONGWORD', 'PTRINT', 'PTRUINT', 'QWORD',
    'RAWBLOB', 'RAWBYTESTRING', 'RAWJSON', 'RAWUTF8', 'RECORD', 'SINGLE',
    'SPIUTF8', 'STRING', 'SYNUNICODE',
    'TCREATETIME', 'TDATETIME', 'TDATETIMEMS', 'TGUID', 'THASH128', 'THASH256',
    'THASH512', 'TID', 'TMODTIME', 'TRECORDREFERENCE', 'TRECORDREFERENCETOBEDELETED',
    'TRECORDVERSION', 'TTIMELOG', 'TUNIXMSTIME', 'TUNIXTIME',
    'UNICODESTRING', 'UTF8STRING', 'VARIANT', 'WIDESTRING', 'WORD');
  // warning: recognized types should match at binary storage level!
  SORTEDTYPES: array[0..SORTEDMAX] of TRttiParserType = (
    ptArray, ptBoolean, ptByte, ptCardinal, ptCurrency, ptDouble, ptExtended,
    ptInt64, ptInteger, ptInterface, ptInteger, ptCardinal, ptPtrInt, ptPtrUInt, ptQWord,
    ptRawByteString, ptRawByteString, ptRawJson, ptRawUtf8, ptRecord, ptSingle,
    ptRawUtf8, ptString, ptSynUnicode,
    ptTimeLog, ptDateTime, ptDateTimeMS, ptGuid, ptHash128, ptHash256,
    ptHash512, ptOrm, ptTimeLog, ptOrm, ptOrm, ptOrm, ptUnixMSTime,
    ptUnixTime, ptTimeLog, ptUnicodeString,
    ptRawUtf8, ptVariant, ptWideString, ptWord);
  SORTEDCOMPLEX: array[0..SORTEDMAX] of TRttiParserComplexType = (
    pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctCreateTime, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctNone, pctID, pctModTime, pctRecordReference, pctRecordReferenceToBeDeleted,
    pctRecordVersion, pctNone, pctTimeLog, pctNone, 
    pctNone, pctNone, pctNone, pctNone, pctNone);
var
  ndx: PtrInt;
  up: PUtf8Char;
  tmp: array[byte] of AnsiChar; // avoid unneeded memory allocation
  c: TRttiParserComplexType;
begin
  UpperCopy255Buf(@tmp, Name, NameLen)^ := #0;
  up := @tmp;
  //for ndx := 1 to SORTEDMAX do if StrComp(SORTEDNAMES[ndx], SORTEDNAMES[ndx-1])<=0 then
  //writeln(SORTEDNAMES[ndx]);
  ndx := FastFindPUtf8CharSorted(@SORTEDNAMES, SORTEDMAX, up);
  if ndx >= 0 then
  begin
    result := SORTEDTYPES[ndx];
    c := SORTEDCOMPLEX[ndx];
  end
  else if (NameLen < 200) and
          (tmp[0] = 'T') and // T...ID pattern in name?
          (PWord(@tmp[NameLen - 2])^ = ord('I') + ord('D') shl 8) then
  begin
    result := ptOrm;
    c := pctSpecificClassID;
  end
  else
  begin
    result := ptNone;
    c := pctNone;
  end;
  if Complex <> nil then
    Complex^ := c;
end;

function TypeNameToStandardParserType(Name: PShortString;
  Complex: PRttiParserComplexType): TRttiParserType;
begin
  result := TypeNameToStandardParserType(@Name^[1], ord(Name^[0]), Complex);
end;

function TypeNameToStandardParserType(const Name: RawUtf8;
  Complex: PRttiParserComplexType): TRttiParserType;
begin
  result := TypeNameToStandardParserType(pointer(Name), length(Name), Complex);
end;

function TypeInfoToStandardParserType(Info: PRttiInfo; FirstSearchByName: boolean;
  Complex: PRttiParserComplexType): TRttiParserType;
var
  cp: integer;
begin
  result := ptNone;
  if Complex <> nil then
    Complex^ := pctNone;
  if Info = nil then
    exit;
  if FirstSearchByName then
  begin
    result := TypeNameToStandardParserType(
      @Info^.RawName[1], ord(Info^.RawName[0]), Complex);
    if result <> ptNone then
      if (result = ptOrm) and
         (Info^.Kind <> rkInt64) then
        // paranoid check e.g. for T...ID false positives
        result := ptNone
      else
        exit;
  end;
  case Info^.Kind of
    // FPC and Delphi will use a fast jmp table
    {$ifdef FPC} rkLStringOld, {$endif} rkLString:
      if (not FirstSearchByName) and
         (Info = TypeInfo(RawJson)) then
        result := ptRawJson
      else
      begin
        cp := Info^.AnsiStringCodePage;
        if cp = CP_UTF8 then
          result := ptRawUtf8
        else if cp = CODEPAGE_US then
          result := ptWinAnsi
        else if cp >= CP_RAWBLOB then
          result := ptRawByteString
        {$ifndef UNICODE}
        else if (cp = CP_ACP) or
                (cp = Unicode_CodePage) then
          result := ptString
        {$endif UNICODE}
        else
          result := ptRawUtf8; // fallback to UTF-8 string
      end;
    rkWString:
      result := ptWideString;
  {$ifdef HASVARUSTRING}
    rkUString:
      result := ptUnicodeString;
  {$endif HASVARUSTRING}
  {$ifdef FPC_OR_UNICODE}
    {$ifdef UNICODE} rkProcedure, {$endif} rkClassRef, rkPointer:
      result := ptPtrInt;
  {$endif FPC_OR_UNICODE}
    rkVariant:
      result := ptVariant;
    rkArray:
      result := ptArray;
    rkDynArray:
      result := ptDynArray;
    rkRecord {$ifdef FPC}, rkObject {$endif}:
      {$ifndef HASNOSTATICRTTI}
      if (not FirstSearchByName) and
         (Info = TypeInfo(TGuid)) then
        result := ptGuid
      else
      {$endif HASNOSTATICRTTI}
        result := ptRecord;
    rkChar:
      result := ptByte;
    rkWChar:
      result := ptWord;
    rkMethod:
      result := ptPtrInt;
    rkInterface:
      result := ptInterface;
    rkInteger:
      case Info^.RttiOrd of
        roSByte, roUByte:
          result := ptByte;
        roSWord, roUWord:
          result := ptWord;
        roSLong:
          result := ptInteger;
        roULong:
          result := ptCardinal;
      {$ifdef FPC_NEWRTTI}
        roSQWord:
          result := ptInt64;
        roUQWord:
          result := ptQWord;
      {$endif FPC_NEWRTTI}
      end;
    rkInt64:
    {$ifndef FPC}
      if Info^.IsQWord then
        result := ptQWord
      else
    {$endif FPC}
      if FirstSearchByName then
        result := ptInt64
      else if Info = TypeInfo(TID) then
      begin
        result := ptOrm;
        if Complex <> nil then
          Complex^ := pctID;
      end
      else if Info = TypeInfo(TTimeLog) then
      begin
        result := ptTimeLog;
        if Complex <> nil then
          Complex^ := pctTimeLog;
      end
      else if Info = TypeInfo(TUnixTime) then
        result := ptUnixTime
      else if Info = TypeInfo(TUnixMSTime) then
        result := ptUnixMSTime
      else
        result := ptInt64;
  {$ifdef FPC}
    rkQWord:
      result := ptQWord;
    rkBool:
      result := ptBoolean;
  {$endif FPC}
    rkEnumeration:
    {$ifndef FPC}
      if Info^.IsBoolean then
        result := ptBoolean
      else
    {$endif FPC}
        result := ptEnumeration;
    rkSet:
      result := ptSet;
    rkClass:
      result := ptClass;
    rkFloat:
      case Info^.RttiFloat of
        rfSingle:
          result := ptSingle;
        rfDouble:
          if FirstSearchByName then
            result := ptDouble
          else if Info = TypeInfo(TDateTime) then
            result := ptDateTime
          else if Info = TypeInfo(TDateTimeMS) then
            result := ptDateTimeMS
          else
            result := ptDouble;
        rfCurr:
          result := ptCurrency;
        rfExtended:
          result := ptExtended;
        // rfComp: not implemented yet
      end;
  end;
end;

function SizeToDynArrayKind(size: integer): TRttiParserType;
  {$ifdef HASINLINE}inline;{$endif}
begin  // rough estimation
  case size of
    1:
      result := ptByte;
    2:
      result := ptWord;
    4:
      result := ptInteger;
    8:
      result := ptInt64;
    16:
      result := ptHash128;
    32:
      result := ptHash256;
    64:
      result := ptHash512;
  else
    result := ptNone;
  end;
end;

var
  PT_DYNARRAY: array[TRttiParserType] of pointer; // most simple dynamic arrays

function TypeInfoToDynArrayTypeInfo(ElemInfo: PRttiInfo;
  ExpectExactElemInfo: boolean; ParserType: PRttiParserType): PRttiInfo;
var
  parser: TRttiParserType;
  rc: TRttiCustom;
begin
  parser := TypeInfoToStandardParserType(ElemInfo, {byname=}false);
  if parser = ptArray then
    parser := SizeToDynArrayKind(ElemInfo^.ArraySize);
  result := PT_DYNARRAY[parser];
  if result <> nil then
  begin
    if ParserType <> nil then
      ParserType^ := Parser;
    if (not ExpectExactElemInfo) or
       (PT_INFO[parser] = ElemInfo) then
      exit;
    rc := Rtti.RegisterType(result);
    if (rc.ArrayRtti <> nil) and
       (rc.ArrayRtti.Info = ElemInfo) then
      exit;
  end;
  rc := Rtti.FindByArrayRtti(ElemInfo); // search in registered rkDynArray
  if rc <> nil then
  begin
    if ParserType <> nil then
      ParserType^ := rc.ArrayRtti.Parser;
    result := rc.Info;
  end;
end;

function DynArrayTypeInfoToStandardParserType(DynArrayInfo, ElemInfo: PRttiInfo;
  ElemSize: integer; ExactType: boolean; out FieldSize: integer;
  Complex: PRttiParserComplexType): TRttiParserType;
// warning: we can't use TRttiInfo.RecordAllFields since it would break
// backward compatibility and code expectations
var
  fields: TRttiRecordManagedFields;
  offset: integer;
  pt: TRttiParserType;
begin
  result := ptNone;
  if Complex <> nil then
    Complex^ := pctNone;
  FieldSize := 0;
  // fast guess of most known ArrayType
  if (DynArrayInfo <> nil) and
     ((ElemInfo = nil) or
      not(ElemInfo^.Kind in [rkEnumeration, rkSet, rkDynArray, rkClass])) then
    for pt := ptBoolean to ptWord do
      if PT_DYNARRAY[pt] = DynArrayInfo then
      begin
        result := pt;
        break;
      end;
  if result = ptNone then
    repeat
      // guess from RTTI of nested record(s)
      if ElemInfo = nil then
      begin
        result := SizeToDynArrayKind(ElemSize);
        if result = ptNone then
          FieldSize := ElemSize;
      end
      else
      // try to guess from 1st record/object field
      if not exactType and
         (ElemInfo^.Kind in rkRecordTypes) then
      begin
        ElemInfo.RecordManagedFields(fields);
        if fields.Count = 0 then
        begin
          ElemInfo := nil;
          continue;
        end;
        offset := fields.Fields^.Offset;
        if offset <> 0 then
        begin
          result := SizeToDynArrayKind(offset);
          if result = ptNone then
            FieldSize := offset;
        end
        else
        begin
          ElemInfo := fields.Fields^.
            {$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
          if (ElemInfo = nil) or
             (ElemInfo^.Kind in rkRecordTypes) then
            continue; // nested records
          result := TypeInfoToStandardParserType(
            ElemInfo, {fromname=}true, Complex);
          if result = ptNone then
          begin
            ElemInfo := nil;
            continue;
          end;
        end;
      end;
      break;
    until false;
  if result = ptNone then
    // will recognize simple arrays from TypeName and ElemType
    result := TypeInfoToStandardParserType(ElemInfo, true, Complex);
  if PT_SIZE[result] <> 0 then
    FieldSize := PT_SIZE[result];
end;

function DynArrayItemTypeLen(const DynArrayTypeName: RawUtf8): PtrInt;
begin
  result := length(DynArrayTypeName);
  if (result > 12) and
     IdemPropName('DynArray', @PByteArray(DynArrayTypeName)[result - 8], 8) then
    dec(result, 8)
  else if (result > 3) and
          (DynArrayTypeName[result] in ['s', 'S']) then
    dec(result)
  else
    result := 0;
end;


{ ************** RTTI-based Registration for Custom JSON Parsing }

{ TRttiCustomProp }

function TRttiCustomProp.InitFrom(RttiProp: PRttiProp): PtrInt;
var
  addr: PtrInt;
begin
  Value := Rtti.RegisterType(RttiProp^.TypeInfo);
  if Value = nil then
    raise ERttiException.CreateUtf8('TRttiCustom: % property has no RTTI',
      [RttiProp^.Name^]);
  addr := PtrInt(RttiProp^.GetFieldAddr(nil));
  // GetterCall/SetterCall will handle void "read"/"write" attributes
  if RttiProp^.GetterCall = rpcField then
    OffsetGet := addr
  else
    OffsetGet := -1;
  if RttiProp^.SetterCall = rpcField then
    OffsetSet := addr
  else
    OffsetSet := -1;
  Name := ToUtf8(RttiProp^.Name^);
  Prop := RttiProp;
  if rcfHasRttiOrd in Value.Cache.Flags then
    OrdinalDefault := RttiProp.Default
  else
    OrdinalDefault := NO_DEFAULT;
  result := Value.Size;
end;

function TRttiCustomProp.NameMatch(P: PUtf8Char; Len: PtrInt): boolean;
var
  n: PUtf8Char;
begin
  result := false;
  n := pointer(Name);
  if (n = nil) or
     (PStrLen(n - _STRLEN)^ <> Len) then
    exit;
  {$ifdef FPC} // Delphi is not efficient at inlining this
  pointer(Len) := @PUtf8Char(n)[Len - SizeOf(cardinal)];
  dec(PtrUInt(P), PtrUInt(n));
  if Len >= PtrInt(PtrUInt(n)) then
    repeat // compare 4 Bytes per loop
      if (PCardinal(n)^ xor PCardinal(P + PtrUInt(n))^) and $dfdfdfdf <> 0 then
        exit;
      inc(PCardinal(n));
    until Len < PtrInt(PtrUInt(n));
  inc(Len, SizeOf(cardinal));
  if PtrInt(PtrUInt(n)) < Len then
    repeat
      if (ord(n^) xor ord(P[PtrUInt(n)])) and $df <> 0 then
        exit;
      inc(PByte(n));
    until PtrInt(PtrUInt(n)) >= Len;
  result := true;
  {$else}
  result := IdemPropNameUSameLenNotNull(n, P, Len)
  {$endif FPC}
end;

procedure TRttiCustomProp.GetValue(Data: pointer; out RVD: TRttiVarData);
begin
  if (Prop = nil) or
     (OffsetGet >= 0 ) then
    // direct memory access of the value
    GetValueDirect(Data, RVD)
  else
    // we need to call a getter method
    GetValueGetter(Data, RVD);
end;

procedure TRttiCustomProp.SetValue(Data: pointer; var RVD: TRttiVarData;
  andclear: boolean);
begin
  if Prop <> nil then
    Prop.SetValue(TObject(Data), variant(RVD));
  if andclear and
     RVD.NeedsClear then
    VarClearProc(RVD.Data);
  if Prop = nil then // raise exception after NeedsClear to avoid memory leak
    raise ERttiException.Create('TRttiCustomProp.SetValue: with Prop=nil');
end;

procedure TRttiCustomProp.AddValueJson(W: TBaseWriter; Data: pointer;
  Options: TTextWriterWriteObjectOptions);
var
  rvd: TRttiVarData;
  tw: TTextWriterKind;
begin
  GetValue(Data, rvd);
  if Value.Parser = ptRawJson then
    tw := twNone
  else
    tw := twJsonEscape;
  W.AddVariant(variant(rvd), tw, Options);
  if rvd.NeedsClear then
    VarClearProc(rvd.Data);
end;

function TRttiCustomProp.ValueIsDefault(Data: pointer): boolean;
begin
  if rcfHasRttiOrd in Value.Cache.Flags then
    if OffsetGet >= 0 then
      result := FromRttiOrd(
        Value.Cache.RttiOrd, PAnsiChar(Data) + OffsetGet) = OrdinalDefault
    else
      result := Prop.GetOrdProp(Data) = OrdinalDefault
  else if rcfGetInt64Prop in Value.Cache.Flags then
    if OffsetGet >= 0 then
      result := PInt64(PAnsiChar(Data) + OffsetGet)^ = OrdinalDefault
    else
      result := Prop.GetInt64Prop(Data) = OrdinalDefault
  else
    // only ordinals have default values
    result := false;
end;

function TRttiCustomProp.ValueIsVoid(Data: pointer): boolean;
begin
  // we assume the caller ensured Data<>nil
  if OffsetGet >= 0 then
    // direct check value from field in memory
    result := Value.ValueIsVoid(PAnsiChar(Data) + OffsetGet)
  else
    // slightly slower method using a getter
    result := ValueIsVoidGetter(Data);
end;

function TRttiCustomProp.ValueIsVoidGetter(Data: pointer): boolean;
var
  rvd: TRttiVarData;
begin
  if Prop = nil then
    result := true
  else if Value.Kind = rkClass then
    result := IsObjectDefaultOrVoid(Prop.GetObjProp(Data))
  else
  begin
    GetValueGetter(Data, rvd);
    case rvd.DataType of
      varEmpty, varNull:
        result := true;
      varAny, varUnknown, varString, varOleStr
      {$ifdef HASVARUSTRING}, varUString {$endif}:
        result := rvd.Data.VAny = nil;
      varInt64, varWord64, varDouble, varCurrency, varBoolean:
        result := rvd.Data.VInt64 = 0;
    else
      result := false;
    end;
    if rvd.NeedsClear then
      VarClearProc(rvd.Data);
  end;
end;

procedure TRttiCustomProp.GetValueDirect(Data: PByte; out RVD: TRttiVarData);
begin
  inc(Data, OffsetGet);
  RVD.VType := Value.Cache.RttiVarDataVType; // reset NeedsClear/ValueIsInstance
  case RVD.VType of
  varEmpty:
    // void Data or unsupported TRttiKind
    exit;
  varInt64, varBoolean:
    // rkInteger, rkBool using VInt64 for proper cardinal support
    RVD.Data.VInt64 := FromRttiOrd(Value.Cache.RttiOrd, Data);
  varWord64:
    // rkInt64, rkQWord
    begin
      if not (rcfQWord in Value.Cache.Flags) then
        RVD.VType := varInt64;
      RVD.Data.VInt64 := PInt64(Data)^;
    end;
  varDouble, varCurrency:
    // copy those 64-bit types at binary level
    RVD.Data.VInt64 := PInt64(Data)^;
  varAny:
    begin
      // rkEnumeration,rkSet,rkDynArray,rkClass,rkInterface,rkRecord,rkObject
      RVD.PropValue := Data; // keeping RVD.PropValueIsInstance=false
      RVD.Prop := @self;
      // varAny/Value handled by TTextWriter.AddVariant/AddRttiVarData
    end;
  varUnknown:
    // rkChar, rkWChar, rkSString converted into temporary RawUtf8
    begin
      RVD.VType := varString;
      RVD.NeedsClear := true;
      RVD.Data.VAny := nil; // avoid GPF
      Value.Info.StringToUtf8(Data, RawUtf8(RVD.Data.VAny));
    end;
  else
    // varString or varVariant which could be returned by reference
    begin
      RVD.Data.VAny := Data;
      if (Value.Kind = rkVariant) or
         (PPointer(Data)^ <> nil) then
        RVD.VType := RVD.VType or varByRef
    end;
  end;
end;

procedure TRttiCustomProp.GetValueGetter(Instance: TObject;
  out RVD: TRttiVarData);
begin
  RVD.VType := Value.Cache.RttiVarDataVType; // reset NeedsClear/ValueIsInstance
  case RVD.VType of
  varEmpty:
    // unsupported TRttiKind
    exit;
  varInt64, varBoolean:
    // rkInteger, rkBool
    RVD.Data.VInt64 := Prop.GetOrdProp(Instance); // VInt64 for cardinal
  varWord64:
    // rkInt64, rkQWord
    begin
      if not (rcfQWord in Value.Cache.Flags) then
        RVD.VType := varInt64;
      RVD.Data.VInt64 := Prop.GetInt64Prop(Instance);
    end;
  varCurrency:
    Prop.GetCurrencyProp(Instance, RVD.Data.VCurrency);
  varDouble:
    RVD.Data.VDouble := Prop.GetFloatProp(Instance);
  varAny:
    begin
      // rkEnumeration,rkSet,rkDynArray,rkClass,rkInterface,rkRecord,rkObject
      RVD.PropValueIsInstance := true;
      RVD.PropValue := Instance;
      RVD.Prop := @self;
      // varAny/Value/Prop handled by TTextWriter.AddVariant/AddRttiVarData
    end;
  varUnknown:
    // rkChar, rkWChar, rkSString converted into temporary RawUtf8
    begin
      RVD.VType := varString;
      RVD.Data.VAny := nil; // avoid GPF
      Prop.GetAsString(Instance, RawUtf8(RVD.Data.VAny));
      RVD.NeedsClear := RVD.Data.VAny <> nil; // if a RawUtf8 was allocated
    end
  else
    // varString/varOleStr/varUString or varVariant
    begin
      RVD.Data.VAny := nil; // avoid GPF below
      case Value.Kind of
        rkLString:
          Prop.GetLongStrProp(Instance, RawByteString(RVD.Data.VAny));
        rkWString:
          Prop.GetWideStrProp(Instance, WideString(RVD.Data.VAny));
        {$ifdef HASVARUSTRING}
        rkUString:
          Prop.GetUnicodeStrProp(Instance, UnicodeString(RVD.Data.VAny));
        {$endif HASVARUSTRING}
        rkVariant:
          begin
            RVD.VType := varEmpty; // to fill as variant
            Prop.GetVariantProp(Instance, variant(RVD), {byref=}false);
            RVD.NeedsClear := true; // we allocated a RVD for the getter result
            exit;
          end;
      end;
      RVD.NeedsClear := RVD.Data.VAny <> nil;
    end;
  end;
end;

function TRttiCustomProp.CompareValue(Data, Other: pointer;
  const OtherRtti: TRttiCustomProp; CaseInsensitive: boolean): integer;
var
  v1, v2: TRttiVarData;
begin
  // direct comparison of ordinal values (rkClass is handled below)
  if (rcfHasRttiOrd in Value.Cache.Flags) and
     (rcfHasRttiOrd in OtherRtti.Value.Cache.Flags) then
    if OffsetGet >= 0 then
      result := CompareInt64(
        FromRttiOrd(Value.Cache.RttiOrd, PAnsiChar(Data) + OffsetGet),
        FromRttiOrd(OtherRtti.Value.Cache.RttiOrd, PAnsiChar(Other) + OtherRtti.OffsetGet))
    else
      result := CompareInt64(Prop.GetOrdProp(Data), OtherRtti.Prop.GetOrdProp(Other))
  else if (rcfGetInt64Prop in Value.Cache.Flags) and
          (rcfGetInt64Prop in OtherRtti.Value.Cache.Flags) then
    if OffsetGet >= 0 then
      result := CompareInt64(PInt64(PAnsiChar(Data) + OffsetGet)^,
                             PInt64(PAnsiChar(Other) + OtherRtti.OffsetGet)^)
    else
      result := CompareInt64(Prop.GetInt64Prop(Data),
                             OtherRtti.Prop.GetInt64Prop(Other))
  else
  // comparison using temporary TRttiVarData (using varByRef if possible)
  begin
    GetValue(Data, v1);
    OtherRtti.GetValue(Other, v2);
    if (v1.Data.VType <> varAny) and
       (v2.Data.VType <> varAny) then
      // standard variant comparison function (from mormot.core.variants)
      result := SortDynArrayVariantComp(v1.Data, v2.Data, CaseInsensitive)
    else if (v1.Data.VType = v2.Data.VType) and
            (v1.Prop = v2.Prop) then
      // v1 and v2 are both varAny, with the very same RTTI type -> use
      // mormot.core.json efficient comparison (also handle rkClass/TObject)
      result := Value.ValueCompare(v1.PropValue, v2.PropValue, CaseInsensitive)
    else
      // we don't know much about those fields: just compare the pointers
      result := ComparePtrInt(PtrInt(v1.PropValue), PtrInt(v2.PropValue));
    if v1.NeedsClear then
      VarClearProc(v1.Data);
    if v2.NeedsClear then
      VarClearProc(v2.Data);
  end;
end;


{ TRttiCustomProps }

function TRttiCustomProps.Find(
  PropName: PUtf8Char; PropNameLen: PtrInt): PRttiCustomProp;
var
  n: integer;
begin
  if PropNameLen <> 0 then
  begin
    result := pointer(List);
    if result <> nil then
    begin
      n := Count;
      repeat
        if result^.NameMatch(PropName, PropNameLen) then
          exit;
        inc(result);
        dec(n);
      until n = 0;
    end;
  end;
  result := nil;
end;

function TRttiCustomProps.Find(const PropName: RawUtf8): PRttiCustomProp;
begin
  result := Find(pointer(PropName), length(PropName));
end;

function TRttiCustomProps.NameChange(const Old, New: RawUtf8): PRttiCustomProp;
begin
  result := Find(Old);
  if result <> nil then
    result^.Name := New;
end;

procedure TRttiCustomProps.NameChanges(const Old, New: array of RawUtf8);
var
  i: PtrInt;
  p: PRttiCustomProp;
begin
  if high(Old) <> high(New) then
    raise ERttiException.CreateUtf8(
      'NameChanges(%,%) fields count', [high(Old), high(New)]);
  // first reset the names from RTTI (if available)
  p := pointer(List);
  for i := 1 to Count do
  begin
    if p^.Prop <> nil then
      p^.Name := ToUtf8(p^.Prop^.Name^);
    inc(p);
  end;
  // customize field names
  for i := 0 to high(Old) do
  begin
    p := Find(Old[i]);
    if p = nil then
      raise ERttiException.CreateUtf8('NameChanges(%) unknown', [Old[i]]);
    p^.Name := New[i];
  end;
end;

procedure TRttiCustomProps.Add(Info: PRttiInfo; Offset: PtrInt;
  const PropName: RawUtf8; AddFirst: boolean);
var
  n: PtrInt;
begin
  if (Info = nil) or
     (Offset < 0) or
     (PropName = '') or
     (Find(PropName) <> nil) then // don't register if already existing
    exit;
  SetLength(List, Count + 1);
  if AddFirst then
  begin
    if Count > 0 then
    begin
      MoveFast(List[0], List[1], SizeOf(List[0]) * Count);
      pointer(List[0].Name) := nil; // avoid GPF below
    end;
    n := 0;
  end
  else
    n := Count;
  inc(Count);
  with List[n] do
  begin
    Value := Rtti.RegisterType(Info);
    OffsetGet := Offset;
    OffsetSet := Offset;
    Name := PropName;
    Prop := nil;
    OrdinalDefault := NO_DEFAULT;
    inc(Size, Value.Size);
  end;
end;

function TRttiCustomProps.FromTextPrepare(const PropName: RawUtf8): integer;
begin
  if PropName = '' then
    raise ERttiException.Create('FromTextPrepare: Void property name');
  if Find(PropName) <> nil then
    raise ERttiException.CreateUtf8('Duplicated % property name', [PropName]);
  result := Count;
  inc(Count);
  SetLength(List, Count);
  List[result].Name := PropName;
end;

function TRttiCustomProps.AdjustAfterAdded: TRttiCustomFlags;
var
  i, n: PtrInt;
  p: PRttiCustomProp;
begin
  if Count = 0 then
  begin
    result := [];
    exit;
  end;
  result := [rcfHasNestedProperties, rcfHasOffsetSetJsonLoadProperties];
  SetLength(fManaged, Count);
  n := 0;
  p := pointer(List);
  for i := 1 to Count do
  begin
    if (rcfIsManaged in p^.Value.Flags) and
       (p^.OffsetGet >= 0) then
    begin
      include(result, rcfHasNestedManagedProperties);
      fManaged[n] := p;
      inc(n);
    end;
    if (p^.OffsetSet < 0) or
       not Assigned(p^.Value.fJsonLoad) then
      exclude(result, rcfHasOffsetSetJsonLoadProperties);
    inc(p);
  end;
  SetLength(fManaged, n);
end;

procedure TRttiCustomProps.AsText(out Result: RawUtf8; IncludePropType: boolean;
  const Prefix, Suffix: RawUtf8);
var
  tmp: TTextWriterStackBuffer;
  i: PtrInt;
begin
  if Count > 0 then
    with TBaseWriter.CreateOwnedStream(tmp) do
    try
      AddString(Prefix);
      for i := 0 to Count - 1 do
        with List[i] do
        begin
          if i > 0 then
            Add(',', ' ');
          AddNoJsonEscapeUtf8(Name);
          if IncludePropType then
          begin
            Add(':', ' ');
            AddString(Value.Name);
          end;
        end;
      AddString(Suffix);
      SetText(Result);
    finally
      Free;
    end;
end;

procedure TRttiCustomProps.InternalClear;
begin
  List := nil;
  Count := 0;
  Size := 0;
  NotInheritedIndex := 0;
  fManaged := nil;
end;

procedure TRttiCustomProps.AddFromClass(ClassInfo: PRttiInfo;
  IncludeParents: boolean);
var
  rc: PRttiClass;
  rp: PRttiProp;
  rs: PRttiProps;
  n, p: PtrInt;
begin
  if (ClassInfo = nil) or
     (ClassInfo^.Kind <> rkClass) then
    exit;
  rc := ClassInfo^.RttiNonVoidClass;
  if IncludeParents then
    // put parent properties first
    AddFromClass(rc^.ParentInfo, true);
  rs := rc^.RttiProps;
  p := rs^.PropCount;
  if p = 0 then
    exit;
  n := Count;
  NotInheritedIndex := n;
  inc(Count, p);
  SetLength(List, Count);
  rp := rs^.PropList;
  repeat
    inc(Size, List[n].InitFrom(rp));
    rp := rp^.Next;
    inc(n)
  until n = Count;
end;

procedure TRttiCustomProps.SetFromRecordExtendedRtti(RecordInfo: PRttiInfo);
var
  dummy: PtrInt;
  all: TRttiRecordAllFields;
  f: PRttiRecordAllField;
  i: PtrInt;
begin
  if (RecordInfo = nil) or
     not (RecordInfo^.Kind in rkRecordTypes) then
    exit;
  all := RecordInfo^.RecordAllFields(dummy);
  InternalClear;
  if all = nil then
    // enhanced RTTI is available since Delphi 2010
    exit;
  Count := length(all);
  SetLength(List, Count);
  f := pointer(all);
  for i := 0 to Count - 1 do
    with List[i] do
    begin
      Value := Rtti.RegisterType(f^.TypeInfo);
      inc(Size, Value.Size);
      OffsetGet := f^.Offset;
      OffsetSet := f^.Offset;
      Name := ToUtf8(f^.Name^);
      OrdinalDefault := NO_DEFAULT;
      inc(f);
    end;
end;

// TRttiCustom method defined here for proper inlining
procedure TRttiCustom.ValueFinalize(Data: pointer);
begin
  if Assigned(fFinalize) then
    // handle any kind of value from RTTI, including T*ObjArray
    fFinalize(Data, fCache.Info)
  else if rcfWithoutRtti in fFlags then
    // was defined from text
    if ArrayRtti <> nil then
      // static or dynamic array (not T*ObjArray)
      NoRttiArrayFinalize(Data)
    else if rcfHasNestedManagedProperties in fFlags then
      // rcfWithoutRtti records
      fProps.FinalizeManaged(Data);
end;

procedure TRttiCustomProps.FinalizeManaged(Data: PAnsiChar);
var
  pp: ^PRttiCustomProp;
  p: PRttiCustomProp;
  n: integer;
begin
  pp := pointer(fManaged);
  if pp <> nil then
  begin
    n := PDALen(PAnsiChar(pp) - _DALEN)^ + _DAOFF;
    repeat
      p := pp^;
      p.Value.ValueFinalize(Data + p.OffsetSet);
      inc(pp);
      dec(n);
    until n = 0;
  end;
end;

procedure TRttiCustomProps.FinalizeAndClearPublishedProperties(Instance: TObject);
var
  pp: PRttiCustomProp;
  p: PtrInt;
  n: integer;
  rtti: TRttiCustom;
  empty: TVarData;
begin
  PInteger(@empty)^ := 0;
  n := Count;
  pp := pointer(List);
  if pp <> nil then
    repeat
      p := pp^.OffsetSet;
      if p >= 0 then
      begin
        inc(p, PtrInt(Instance));
        rtti := pp^.Value;
        rtti.ValueFinalize(pointer(p));
        if pp^.OrdinalDefault <> NO_DEFAULT then
          MoveSmall(@pp^.OrdinalDefault, pointer(p), rtti.Size)
        else
          FillZeroSmall(pointer(p), rtti.Size);
      end
      else
        pp^.Prop^.SetValue(Instance, PVariant(@empty)^);
      inc(pp);
      dec(n);
    until n = 0;
end;

// TRttiCustom method defined here for proper inlining
function TRttiCustom.ValueCopy(Dest, Source: pointer): PtrInt;
begin
  if not Assigned(fCopy) then
  begin
    if rcfHasNestedProperties in fFlags then
      if fCache.Kind = rkClass then
        fProps.CopyProperties(Dest, Source)
      else
        fProps.CopyRecord(Dest, Source)
    else
      MoveFast(Source^, Dest^, fCache.Size);
    result := fCache.Size;
  end
  else
    result := fCopy(Dest, Source, fCache.Info);
end;

procedure TRttiCustomProps.CopyRecord(Dest, Source: PAnsiChar);
var
  pp: ^PRttiCustomProp;
  n: integer;
  offset: PtrInt;
begin
  offset := 0;
  pp := pointer(fManaged);
  if pp <> nil then
  begin
    n := PDALen(PAnsiChar(pp) - _DALEN)^ + _DAOFF;
    repeat
      offset := pp^.OffsetGet - offset;
      if offset <> 0 then
      begin
        MoveFast(Source^, Dest^, offset); // fast copy unmanaged field
        inc(Source, offset);
        inc(Dest, offset);
      end;
      offset := pp^.Value.ValueCopy(Dest, Source); // copy managed field
      inc(Source, offset);
      inc(Dest, offset);
      inc(offset, pp^.OffsetGet);
      inc(pp);
      dec(n);
    until n = 0;
  end;
  offset := Size - offset;
  if offset > 0 then
    MoveFast(Source^, Dest^, offset);
end;

procedure TRttiCustomProps.CopyProperties(Dest, Source: PAnsiChar);
var
  p: PRttiCustomProp;
  n: integer;
  v: TRttiVarData;
begin
  p := pointer(List); // all published properties, not only Managed[]
  if p <> nil then
  begin
    n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
    repeat
      if (p^.OffsetGet < 0) or
         (p^.OffsetSet < 0) then
      begin
        // there is a getter or a setter -> use local temporary value
        p^.GetValue(Source, v);
        p^.SetValue(Dest, v, {andclear=}true);
      end
      else
        // direct content copy from the fields memory buffers
        p^.Value.ValueCopy(Dest + p^.OffsetSet, Source + p^.OffsetGet);
      inc(p);
      dec(n);
    until n = 0;
  end;
end;


{ TRttiCustom }

type
  EHook = class(Exception) // to access @Message private field offset
  public
    function MessageOffset: PtrInt; // for Delphi
  end;

function EHook.MessageOffset: PtrInt;
begin
  result := PtrInt(@Message);
end;

procedure TRttiCustom.SetValueClass(aClass: TClass; aInfo: PRttiInfo);
var
  vmt: TObject;
begin
  fValueClass := aClass;
  // set vmtAutoTable slot for efficient Find(TClass) - to be done asap
  vmt := ClassPropertiesAdd(aClass, self, {freexist=}false);
  if vmt <> self then
    raise ERttiException.CreateUtf8(
      '%.SetValueClass(%): vmtAutoTable already set to %', [self, aClass, vmt]);
  // identify the most known class types
  if aClass.InheritsFrom(TCollection) then
    fValueRtlClass := vcCollection
  else if aClass.InheritsFrom(TStrings) then
    fValueRtlClass := vcStrings
  else if aClass.InheritsFrom(TObjectList) then
    fValueRtlClass := vcObjectList
  else if aClass.InheritsFrom(TList) then
    fValueRtlClass := vcList
  else if aClass.InheritsFrom(ESynException) then
    fValueRtlClass := vcESynException
  else if aClass.InheritsFrom(Exception) then
    fValueRtlClass := vcException;
  // register the published properties of this class
  fProps.AddFromClass(aInfo, {includeparents=}true);
  if fValueRtlClass = vcException then
    // manual registration of the Exception.Message property
    fProps.Add(TypeInfo(string), EHook(nil).MessageOffset, 'Message');
end;

constructor TRttiCustom.Create(aInfo: PRttiInfo);
var
  dummy: integer;
  pt: TRttiParserType;
  pct: TRttiParserComplexType;
  item: PRttiInfo;
begin
  if aInfo = nil then
  begin
    include(fFlags, rcfWithoutRtti);
    exit; // will call NoRttiSetAndRegister() later on
  end;
  // retrieve RTTI into ready-to-be-consummed cache
  aInfo^.ComputeCache(fCache);
  if aInfo^.IsManaged then
    // also check nested record fields
    include(fFlags, rcfIsManaged);
  case fCache.Kind of
    rkClass:
      SetValueClass(aInfo.RttiClass.RttiClass, aInfo);
    rkRecord:
      fProps.SetFromRecordExtendedRtti(aInfo); // only for Delphi 2010+
    rkLString:
      if aInfo = TypeInfo(SpiUtf8) then
        include(fFlags, rcfSpi);
    rkDynArray:
      begin
        item := fCache.ItemInfo;
        if item = nil then // =nil for unmanaged types
        begin
          // try to guess the actual type, e.g. a TGUID or an integer
          item := aInfo^.DynArrayItemTypeExtended; // FPC or Delphi 2010+
          if item = nil then
          begin
            // on Delphi 7-2009, recognize at least the most common types
            pt := DynArrayTypeInfoToStandardParserType(aInfo, nil,
              fCache.ItemSize, {exacttype=}true, dummy, @pct);
            item := ParserTypeToTypeInfo(pt, pct);
          end
          else if item.Kind = rkClass then
          begin
            // no need to call RegisterObjArray() on FPC and Delphi 2010+ :)
            include(fFlags, rcfObjArray);
            fObjArrayClass := item.RttiClass^.RttiClass;
          end;
        end;
        fArrayRtti := Rtti.RegisterType(item);
        if (fArrayRtti <> nil) and
           (fArrayFirstField = ptNone) then
          if fArrayRtti.Kind in rkRecordOrDynArrayTypes then
            // guess first field (using fProps[0] would break compatibility)
            fArrayFirstField := DynArrayTypeInfoToStandardParserType(
              aInfo, fCache.ItemInfo, fCache.ItemSize, {exacttype=}false, dummy)
          else
            fArrayFirstField := fArrayRtti.Parser;
      end;
    rkArray:
      begin
        fArrayRtti := Rtti.RegisterType(fCache.ItemInfo);
        if (fArrayRtti = nil) or
           not (rcfIsManaged in fArrayRtti.Flags) then
          // a static array is as managed as its nested items
          exclude(fFlags, rcfIsManaged);
      end;
  end;
  // initialize processing callbacks
  fFinalize := RTTI_FINALIZE[fCache.Kind];
  fCopy := RTTI_COPY[fCache.Kind];
  pt := TypeInfoToStandardParserType(aInfo, {byname=}true, @pct);
  SetParserType(pt, pct);
end;

destructor TRttiCustom.Destroy;
begin
  inherited Destroy;
  ObjArrayClear(fOwnedRtti);
  TObject(fPrivateSlot).Free;
  ObjArrayClear(fPrivateSlots);
end;

constructor TRttiCustom.CreateFromText(const RttiDefinition: RawUtf8);
var
  P: PUtf8Char;
begin
  Create(nil); // no associated RTTI
  P := pointer(RttiDefinition);
  SetPropsFromText(P, eeNothing, {NoRegister=}true);
end;

procedure TRttiCustom.NoRttiSetAndRegister(ParserType: TRttiParserType;
  const TypeName: RawUtf8; DynArrayElemType: TRttiCustom; NoRegister: boolean);
begin
  if (fNoRttiInfo <> nil) or
     not (rcfWithoutRtti in fFlags) then
    raise ERttiException.CreateUtf8('Unexpected %.NoRttiSetAndRegister(%)',
      [self, TypeName]);
  // validate record/dynarray only supported types
  case ParserType of
    ptRecord:
      begin
        fCache.Kind := rkRecord;
        fCache.Size := Props.Size; // as computed by caller
      end;
    ptDynArray:
      begin
        fCache.Kind := rkDynArray;
        fCache.Size := SizeOf(pointer);
        fArrayRtti := DynArrayElemType;
        if (DynArrayElemType.Info <> nil) and
           DynArrayElemType.Info.IsManaged then
          fCache.ItemInfo := DynArrayElemType.Info; // as regular dynarray RTTI
        fCache.ItemSize := DynArrayElemType.Size;
      end;
    ptClass:
      begin
        fCache.Kind := rkClass;
        fCache.Size := SizeOf(pointer);
      end;
  else
    raise ERttiException.CreateUtf8('Unexpected %.CreateWithoutRtti(%)',
      [self, ToText(ParserType)^]);
  end;
  if NoRegister then
  begin
    // initialize the instance, but don't register to TRttiCustomList
    SetParserType(ParserType, pctNone);
    exit;
  end;
  // create fake RTTI which should be enough for our purpose
  SetLength(fNoRttiInfo, length(TypeName) + 64); // all filled with zeros
  fCache.Info := pointer(fNoRttiInfo);
  fCache.Info.Kind := fCache.Kind;
  if TypeName = '' then
    fCache.Info.RawName := BinToHexDisplayLowerShort(@self, SizeOf(pointer))
  else
    fCache.Info.RawName := TypeName;
  case ParserType of
    ptRecord:
      PRecordInfo(GetTypeData(fCache.Info))^.RecSize := fCache.Size;
    ptDynArray:
      GetTypeData(fCache.Info)^.elSize := fCache.ItemSize;
  end;
  // initialize process
  SetParserType(ParserType, pctNone);
  // register to the internal list
  Rtti.Add(self);
end;

function {%H-}_New_NotImplemented(Rtti: TRttiCustom): pointer;
begin
  raise ERttiException.CreateUtf8('%.ClassNewInstance(%:%) not implemented -> ' +
    'please include mormot.core.json unit to register TRttiJson',
    [Rtti, Rtti.Name, ToText(Rtti.Kind)^]);
end;

function TRttiCustom.SetParserType(aParser: TRttiParserType;
  aParserComplex: TRttiParserComplexType): TRttiCustom;
begin
  fParser := aParser;
  fParserComplex := aParserComplex;
  fSetRandom := PT_RANDOM[aParser];
  if fCache.Info <> nil then
    ShortStringToAnsi7String(fCache.Info.Name^, fName);
  fFlags := fFlags + fProps.AdjustAfterAdded;
  if (fArrayRtti <> nil) and
     (rcfIsManaged in fArrayRtti.Flags) then
    include(fFlags, rcfArrayItemManaged);
  fClassNewInstance := @_New_NotImplemented;
  result := self;
end;

procedure TRttiCustom.NoRttiArrayFinalize(Data: PAnsiChar);
var
  n: integer;
  mem: PDynArrayRec;
begin
  if Kind = rkArray then
  begin
    // static array has fixed number of items
    n := fCache.ItemCount;
    mem := nil;
  end
  else
  begin
    // dereference rkDynArray pointer and retrieve length
    mem := PPointer(Data)^;
    if mem = nil then
      exit;
    PPointer(Data)^ := nil;
    Data := pointer(mem);
    dec(mem);
    if mem.refCnt > 1 then
      raise ERttiException.CreateUtf8('%.ArrayFinalize: % has refcnt=%',
        [self, ArrayRtti.Name, mem.refCnt]);
    n := mem.length;
  end;
  // release memory (T*ObjArray would never occur here)
  repeat
    ArrayRtti.ValueFinalize(Data);
    inc(Data, ArrayRtti.Size);
    dec(n);
  until n = 0;
  if mem <> nil then
    FreeMem(mem);
end;

procedure TRttiCustom.ValueFinalizeAndClear(Data: pointer);
begin
  ValueFinalize(Data);
  FillCharFast(Data^, fCache.Size, 0);
end;

function TRttiCustom.ValueIsVoid(Data: PAnsiChar): boolean;
var
  s: PtrInt;
begin
  case Kind of
    rkVariant:
      result := cardinal(PVarData(Data).VType) <= varNull;
    rkClass:
      result := IsObjectDefaultOrVoid(PObject(Data)^);
    else
      begin
        result := false;
        s := fCache.Size;
        repeat
          dec(s);
          if Data[s] <> #0 then
            exit;
        until s = 0;
        result := true;
      end;
  end;
end;

function TRttiCustom.{%H-}ValueCompare(Data, Other: pointer; CaseInsensitive: boolean): integer;
begin
  raise ERttiException.CreateUtf8('%.ValueCompare not implemented -> please ' +
    'include mormot.core.json unit to register TRttiJson', [self]);
end;

function TRttiCustom.{%H-}ValueToVariant(Data: pointer;
  out Dest: TVarData): PtrInt;
begin
  raise ERttiException.CreateUtf8('%.ValueToVariant not implemented -> please ' +
    'include mormot.core.json unit to register TRttiJson', [self]);
end;

procedure TRttiCustom.ValueRandom(Data: pointer);
begin
  mormot.core.os.EnterCriticalSection(Rtti.Table^.Lock);
  fSetRandom(Data, self); // handle most simple kind of values from RTTI
  mormot.core.os.LeaveCriticalSection(Rtti.Table^.Lock);
end;

function TRttiCustom.ClassNewInstance: pointer;
begin
  result := fClassNewInstance(self);
end;

procedure TRttiCustom.PropsClear;
begin
  Props.InternalClear;
  fFlags := fFlags - [rcfHasNestedProperties, rcfHasNestedManagedProperties];
end;

function TRttiCustom.SetObjArray(Item: TClass): TRttiCustom;
begin
  if (self <> nil) and
     (Kind = rkDynArray) and
     (fCache.ItemSize = SizeOf(pointer)) and
     (fCache.ItemInfo = nil) then
  begin
    fObjArrayClass := Item;
    if Item = nil then
    begin
      // unregister
      exclude(fFlags, rcfObjArray);
      fArrayRtti := nil;
      fFinalize := @_DynArrayClear;
    end
    else
    begin
      // register
      include(fFlags, rcfObjArray);
      fArrayRtti := Rtti.RegisterClass(Item); // will call _ObjClear()
      fFinalize := @_ObjArrayClear; // calls RawObjectsClear()
    end;
  end;
  SetParserType(Parser, ParserComplex); // notify format change
  result := self;
end;

var
  RttiArrayCount: integer;

function TRttiCustom.SetBinaryType(BinarySize: integer): TRttiCustom;
begin
  if self <> nil then
  begin
    if BinarySize < 0 then
    begin
      BinarySize := 0;
      exclude(fFlags, rcfBinary);
    end
    else
    begin
      if BinarySize = 0 then
        BinarySize := fCache.Size;
      include(fFlags, rcfBinary);
    end;
    fBinarySize := BinarySize;
    SetParserType(Parser, ParserComplex); // notify format change (e.g. for json)
  end;
  result := self;
end;

procedure TRttiCustom.SetPropsFromText(var P: PUtf8Char;
  ExpectedEnd: TRttiCustomFromTextExpectedEnd; NoRegister: boolean);
var
  prop: TIntegerDynArray;
  propcount: integer;
  propname, typname, atypname: RawUtf8;
  ee: TRttiCustomFromTextExpectedEnd;
  alen, i: PtrInt;
  pt, apt: TRttiParserType;
  c, ac, nested: TRttiCustom;
  cp: PRttiCustomProp;
begin
  PropsClear;
  fCache.Size := 0;
  propcount := 0;
  while (P <> nil) and
        (P^ <> #0) do
  begin
    // fill prop[] from new properties, and set associated type
    if P^ = ',' then
      inc(P);
    if P^ in ['''', '"'] then
    begin
      // parse identifier as SQL string (e.g. "@field0")
      P := UnQuoteSqlStringVar(P, propname);
      if P = nil then
        break;
    end
    else if not GetNextFieldProp(P, propname) then
      // expect regular object pascal identifier (i.e. 0..9,a..z,A..Z,_)
      break;
    if P^ = ',' then
    begin
      // a,'b,b',c: integer
      inc(P);
      AddInteger(prop{%H-}, propcount, Props.FromTextPrepare(propname));
      continue; // several properties defined with the same type
    end;
    AddInteger(prop, propcount, Props.FromTextPrepare(propname));
    if P^ = ':' then
      P := GotoNextNotSpace(P + 1);
    // identify type for prop[]
    typname := '';
    atypname := '';
    c := nil;
    ac := nil;
    pt := ptNone;
    ee := eeNothing;
    if P^ = '{' then
    begin
      // rec: { a,b: integer }
      pt := ptRecord;
      ee := eeCurly;
      repeat
        inc(P)
      until (P^ > ' ') or
            (P^ = #0);
    end
    else if P^ = '[' then
    begin
      // arr: [ a,b:integer ]
      pt := ptDynArray;
      ee := eeSquare;
      repeat
        inc(P)
      until (P^ > ' ') or
            (P^ = #0);
    end
    else
    begin
      if not GetNextFieldProp(P, typname) then
        ERttiException.CreateUtf8('Missing field type for %', [propname]);
      c := Rtti.RegisterTypeFromName(typname, @pt);
      if c = nil then
      case pt of
        ptArray:
          // array of ...
          begin
            if IdemPChar(P, 'OF') then
            begin
              // array of ....   or   array of record ... end
              P := GotoNextNotSpace(P + 2);
              if not GetNextFieldProp(P, atypname) or
                 (P = nil) then
                ERttiException.Create('Missing array field type');
              FormatUtf8('[%%]', [atypname, RttiArrayCount], typname);
              LockedInc32(@RttiArrayCount); // ensure genuine type name
              ac := Rtti.RegisterTypeFromName(atypname, @apt);
              if ac = nil then
                if apt = ptRecord then
                  // array of record ... end
                  ee := eeEndKeyWord
                else
                  P := nil;
            end
            else
              P := nil;
            if P = nil then
              raise ERttiException.CreateUtf8('Expected text definition syntax is ' +
                '"array of record" or "array of KnownType" for %', [propname]);
            pt := ptDynArray;
          end;
        ptRecord:
          // record ... end
          ee := eeEndKeyWord;
        ptNone:
          // unknown type name -> try from T*DynArray/T*s pattern
          begin
            alen := DynArrayItemTypeLen(typname);
            if alen > 0 then
            begin
              // try TIntegerDynArray/TIntegers -> integer
              ac := Rtti.RegisterTypeFromName(@PByteArray(typname)[1], alen - 1);
              if ac = nil then
                // try TMyTypeDynArray/TMyTypes -> TMyType
                ac := Rtti.RegisterTypeFromName(pointer(typname), alen);
            end;
            if ac = nil then
              raise ERttiException.CreateUtf8(
                'Unknown type %: %', [propname, typname]);
            pt := ptDynArray;
          end;
      end;
    end;
    // retrieve nested type information
    if ee <> eeNothing then
    begin
      if (c <> nil) or
         (ac <> nil) or
         not (pt in [ptRecord, ptDynArray]) then
        raise ERttiException.CreateUtf8(
          'Unexpected nested % %', [c, ToText(pt)^]);
      nested := Rtti.GlobalClass.Create(nil);
      nested.SetPropsFromText(P, ee, NoRegister);
      nested.NoRttiSetAndRegister(ptRecord, '', nil, NoRegister);
      if NoRegister then
        ObjArrayAdd(fOwnedRtti, nested);
      if pt = ptRecord then
        // rec: record .. end  or  rec: { ... }
        c := nested
      else
        // arr: [ ... ]   or  arr: array of record .. end
        ac := nested;
    end;
    if ac <> nil then
    begin
      if (c <> nil) or
         (pt <> ptDynArray) then // paranoid
        raise ERttiException.CreateUtf8(
          'Unexpected array % %', [c, ToText(pt)^]);
      c := Rtti.GlobalClass.Create(nil);
      c.NoRttiSetAndRegister(ptDynArray, typname, ac, NoRegister);
      if NoRegister then
        ObjArrayAdd(fOwnedRtti, c);
    end;
    // set type for all prop[]
    for i := 0 to propcount - 1 do
    begin
      cp := @Props.List[prop[i]];
      cp^.Value := c;
      cp^.OffsetGet := fCache.Size;
      cp^.OffsetSet := fCache.Size;
      cp^.OrdinalDefault := NO_DEFAULT;
      inc(fCache.Size, c.fCache.Size);
    end;
    // continue until we reach end of buffer or ExpectedEnd
    while P^ in [#1..' ', ';'] do
      inc(P);
    case ExpectedEnd of
      eeEndKeyWord:
        if IdemPChar(P, 'END') then
        begin
          inc(P, 3);
          while P^ in [#1..' ', ';'] do
            inc(P);
          break;
        end;
      eeSquare:
        if P^ = ']' then
        begin
          inc(P);
          break;
        end;
      eeCurly:
        if P^ = '}' then
        begin
          inc(P);
          break;
        end;
    end;
    propcount := 0;
  end;
  // set whole size and managed fields/properties
  fProps.Size := fCache.Size;
  fFlags := fFlags + Props.AdjustAfterAdded;
end;

function TRttiCustom.GetPrivateSlot(aClass: TClass): pointer;
var
  n: integer;
  slot: PPointer;
begin
  slot := pointer(fPrivateSlots);
  if slot <> nil then
  begin
    result := slot^;
    if PClass(result)^ = aClass then
      exit;
    n := PDALen(PAnsiChar(slot) - _DALEN)^ + (_DAOFF - 1);
    if n <> 0 then
      repeat
        inc(slot);
        result := slot^;
        if PClass(result)^ = aClass then
          exit;
        dec(n);
      until n = 0;
  end;
  result := nil;
end;

function TRttiCustom.SetPrivateSlot(aObject: TObject): pointer;
begin
  Rtti.DoLock;
  try
    result := GetPrivateSlot(PClass(aObject)^);
    if result = nil then
    begin
      ObjArrayAdd(fPrivateSlots, aObject);
      result := aObject;
    end
    else
      aObject.Free;
  finally
    Rtti.DoUnLock;
  end;
end;


{ TRttiCustomList }

procedure TRttiCustomList.Init;
begin
  Table := AllocMem(SizeOf(Table^));
  InitializeCriticalSection(Table^.Lock);
  fGlobalClass := TRttiCustom;
end;

procedure TRttiCustomList.Done;
var
  i: PtrInt;
begin
  for i := Count - 1 downto 0 do
    Instances[i].Free;
  DeleteCriticalSection(Table^.Lock);
  Dispose(Table);
end;

function TRttiCustomList.Find(Info: PRttiInfo): TRttiCustom;
var
  P: PRttiCustomListPair;
begin
  if Info^.Kind <> rkClass then
  begin
    // our optimized "hash table of the poor" (tm) lookup
    P := @Table^.Pairs[Info^.Kind,
      (PtrUInt(Info.RawName[0]) xor PtrUInt(Info.RawName[1])) and
      RTTICUSTOMTYPEINFOHASH];
    // note: we tried to include RawName[2] and $df, but with no gain
    result := pointer(P^.RttiInfoRttiCustom);
    if result = nil then
      exit;
    P := P^.CurrentEnd;
    repeat
      // efficient brute force search within L1 cache
      if PPointer(result)^ <> Info then
      begin
        inc(PByte(result), 2 * SizeOf(pointer)); // PRttiInfo/TRttiCustom pairs
        if PAnsiChar(result) < PAnsiChar(P) then
          continue;
        result := nil; // not found
        exit;
      end;
      result := PPointerArray(result)[1]; // found
      exit;
    until false;
  end
  else
    // it is (slightly) faster to use the vmtAutoTable slot for classes
    result := PPointer(PAnsiChar(Info.RttiNonVoidClass.RttiClass) + vmtAutoTable)^;
end;

function TRttiCustomList.Find(ObjectClass: TClass): TRttiCustom;
begin
  result := PPointer(PAnsiChar(ObjectClass) + vmtAutoTable)^;
end;

function FindNameInPairs(Pairs, PEnd: PPointerArray;
  Name: PUtf8Char; NameLen: PtrInt): TRttiCustom;
var
  s: PRttiInfo;
begin
  repeat
    s := Pairs[0];
    if ord(s^.RawName[0]) <> NameLen then
    begin
      Pairs := @Pairs[2]; // PRttiInfo/TRttiCustom pairs
      if PAnsiChar(Pairs) >= PAnsiChar(PEnd) then
        break;
    end
    else if not IdemPropNameUSameLenNotNull(Name, @s^.RawName[1], NameLen) then
    begin
      Pairs := @Pairs[2];
      if PAnsiChar(Pairs) >= PAnsiChar(PEnd) then
        break;
    end
    else
    begin
      result := Pairs[1];  // found
      exit;
    end;
  until false;
  result := nil; // not found
end;

function TRttiCustomList.Find(Name: PUtf8Char; NameLen: PtrInt;
  Kind: TRttiKind): TRttiCustom;
var
  P: PRttiCustomListPair;
begin
  if (Kind <> rkUnknown) and
     (Name <> nil) and
     (NameLen > 0) then
  begin
    // try latest found value e.g. calling from JsonRetrieveObjectRttiCustom()
    result := Table^.LastPair[Kind];
    if (result <> nil) and
       IdemPropNameU(result.Name, Name, NameLen) then
      exit;
    // our optimized "hash table of the poor" (tm) lookup
    P := @Table^.Pairs[Kind,
      (PtrUInt(NameLen) xor PtrUInt(Name[0])) and RTTICUSTOMTYPEINFOHASH];
    result := pointer(P^.RttiInfoRttiCustom);
    if result <> nil then
    begin
      result := FindNameInPairs(pointer(result), P^.CurrentEnd, Name, NameLen);
      if result <> nil then
        Table^.LastPair[Kind] := result;
    end;
  end
  else
    result := nil;
end;

function TRttiCustomList.Find(Name: PUtf8Char; NameLen: PtrInt;
  Kinds: TRttiKinds): TRttiCustom;
var
  k: TRttiKind;
begin
  // not very optimized, but called only at startup from Rtti.RegisterFromText()
  if (Name <> nil) and
     (NameLen > 0) then
  begin
    if Kinds = [] then
      Kinds := rkAllTypes;
    for k := low(Table^.Pairs) to high(Table^.Pairs) do
      if k in Kinds then
      begin
        result := Find(Name, NameLen, k);
        if result <> nil then
          exit;
      end;
  end;
  result := nil;
end;

function TRttiCustomList.Find(const Name: shortstring; Kinds: TRttiKinds): TRttiCustom;
begin
  result := Find(@Name[1], ord(Name[0]), Kinds);
end;

function TRttiCustomList.FindByArrayRtti(ElemInfo: PRttiInfo): TRttiCustom;
var
  i: PtrInt;
  p: PRttiCustomListPair;
  pp: PPointerArray;
begin
  mormot.core.os.EnterCriticalSection(Table^.Lock);
  try
    if ElemInfo <> nil then
      for i := 0 to high(Table^.Pairs[rkDynArray]) do
      begin
        p := @Table^.Pairs[rkDynArray, i];
        pp := pointer(p^.RttiInfoRttiCustom);
        p := p^.CurrentEnd;
        if pp <> nil then
          repeat
            result := pp[1]; // PRttiInfo/TRttiCustom pairs
            if (result.ArrayRtti <> nil) and
               (result.ArrayRtti.Info = ElemInfo) then
              exit;
            pp := @pp[2];
          until PAnsiChar(pp) = PAnsiChar(p);
      end;
    result := nil;
  finally
    mormot.core.os.LeaveCriticalSection(Table^.Lock);
  end;
end;

function TRttiCustomList.RegisterType(Info: PRttiInfo): TRttiCustom;
begin
  if Info <> nil then
  begin
    result := Find(Info);
    if result = nil then
      result := DoRegister(Info);
  end
  else
    result := nil;
end;

procedure TRttiCustomList.DoLock;
begin
  mormot.core.os.EnterCriticalSection(Table^.Lock);
end;

procedure TRttiCustomList.DoUnLock;
begin
  mormot.core.os.LeaveCriticalSection(Table^.Lock);
end;

function TRttiCustomList.DoRegister(Info: PRttiInfo): TRttiCustom;
begin
  if Info = nil then
  begin
    result := nil;
    exit;
  end;
  mormot.core.os.EnterCriticalSection(Table^.Lock);
  try
    result := Find(Info); // search again (for thread safety)
    if result <> nil then
      exit; // already registered in the background
    result := GlobalClass.Create(Info);
    Add(result);
  finally
    mormot.core.os.LeaveCriticalSection(Table^.Lock);
  end;
  assert(Find(Info) = result); // paranoid check
end;

function TRttiCustomList.DoRegister(ObjectClass: TClass): TRttiCustom;
var
  info: PRttiInfo;
begin
  info := PPointer(PAnsiChar(ObjectClass) + vmtTypeInfo)^;
  if info <> nil then
    result := DoRegister(info)
  else
  begin
    // generate fake RTTI for classes without {$M+}, e.g. TObject or Exception
    mormot.core.os.EnterCriticalSection(Table^.Lock);
    try
      result := Find(ObjectClass); // search again (for thread safety)
      if result <> nil then
        exit; // already registered in the background
      result := GlobalClass.Create(nil);
      result.SetValueClass(ObjectClass, nil);
      result.NoRttiSetAndRegister(ptClass, ToText(ObjectClass), nil, {noreg=}false);
      GetTypeData(result.fCache.Info)^.ClassType := ObjectClass;
    finally
      mormot.core.os.LeaveCriticalSection(Table^.Lock);
    end;
  end;
end;

function TRttiCustomList.DoRegister(ObjectClass: TClass; ToDo: TRttiCustomFlags): TRttiCustom;
var
  i: integer;
  p: PRttiCustomProp;
begin
  mormot.core.os.EnterCriticalSection(Table^.Lock);
  try
    result := DoRegister(ObjectClass);
    if (rcfAutoCreateFields in ToDo) and
       not (rcfAutoCreateFields in result.fFlags) then
    begin
      // detect T*AutoCreate fields
      p := pointer(result.Props.List);
      for i := 1 to result.Props.Count do
      begin
        case p^.Value.Kind of
          rkClass:
            if (p^.OffsetGet >= 0) and
               (p^.OffsetSet >= 0) then
              PtrArrayAdd(result.fAutoCreateClasses, p);
          rkDynArray:
            if (rcfObjArray in p^.Value.Flags) and
               (p^.OffsetGet >= 0) then
              PtrArrayAdd(result.fAutoCreateObjArrays, p);
          rkInterface:
            if (p^.OffsetGet >= 0) and
               (p^.OffsetSet >= 0) then
              PtrArrayAdd(result.fAutoCreateInterfaces, p);
        end;
        inc(p);
      end;
      include(result.fFlags, rcfAutoCreateFields); // should be set once defined
    end;
  finally
    mormot.core.os.LeaveCriticalSection(Table^.Lock);
  end;
end;

procedure TRttiCustomList.Add(Instance: TRttiCustom);
var
  hash, n: PtrInt;
  P: PRttiCustomListPair;
begin // call is made within Lock..UnLock
  hash := (PtrUInt(Instance.Info.RawName[0]) xor
           PtrUInt(Instance.Info.RawName[1])) and RTTICUSTOMTYPEINFOHASH;
  P := @Table^.Pairs[Instance.Kind, hash];
  n := length(P^.RttiInfoRttiCustom);
  if (n = 0) or
     (@P^.RttiInfoRttiCustom[n] = P^.CurrentEnd) then
  begin
    SetLength(P^.RttiInfoRttiCustom, n + 32); // seldom resize for thread safety
    P^.CurrentEnd := @P^.RttiInfoRttiCustom[n];
  end;
  PPointerArray(P^.CurrentEnd)[0] := Instance.Info;
  PPointerArray(P^.CurrentEnd)[1] := Instance;
  P^.CurrentEnd := @PPointerArray(P^.CurrentEnd)[2];
  ObjArrayAddCount(Instances, Instance, Count); // to release memory
  inc(Counts[Instance.Kind]);
end;

procedure TRttiCustomList.SetGlobalClass(RttiClass: TRttiCustomClass);
var
  i: PtrInt;
  regtypes: RawUtf8;
  newunit: PShortString;
begin
  if Count <> 0 then
  begin
    for i := 0 to Count - 1 do
      regtypes := {%H-}regtypes + Instances[i].Name + ' ';
    newunit := _ClassUnit(RttiClass);
    raise ERttiException.CreateUtf8('Rtti.Count=% at Rtti.GlobalClass := % : ' +
      'some types have been registered as % before % has been loaded and ' +
      'initialized - please put % in the uses clause where you register '+
      'your [ %] types, in addition to mormot.core.rtti',
      [Count, RttiClass, fGlobalClass, newunit, newunit, regtypes]);
  end;
  fGlobalClass := RttiClass;
end;

procedure TRttiCustomList.RegisterTypes(const Info: array of PRttiInfo);
var
  i: PtrInt;
begin
  for i := 0 to high(Info) do
    RegisterType(Info[i]);
end;

function TRttiCustomList.RegisterTypeFromName(Name: PUtf8Char;
  NameLen: PtrInt; ParserType: PRttiParserType): TRttiCustom;
var
  pt: TRttiParserType;
  pct: TRttiParserComplexType;
begin
  if ParserType <> nil then
    ParserType^ := ptNone;
  result := Find(Name, NameLen);
  if result = nil then
  begin
    pt := TypeNameToStandardParserType(Name, NameLen, @pct);
    if ParserType <> nil then
      ParserType^ := pt;
    if (pt <> ptNone) and
       (pt <> ptArray) then
      result := RegisterType(ParserTypeToTypeInfo(pt, pct));
  end
  else if ParserType <> nil then
    ParserType^ := result.Parser;
end;

function TRttiCustomList.RegisterTypeFromName(const Name: RawUtf8;
  ParserType: PRttiParserType): TRttiCustom;
begin
  result := RegisterTypeFromName(pointer(Name), length(Name), ParserType);
end;

function TRttiCustomList.RegisterClass(ObjectClass: TClass): TRttiCustom;
begin
  result := PPointer(PAnsiChar(ObjectClass) + vmtAutoTable)^;
  if result = nil then
    result := DoRegister(ObjectClass);
end;

function TRttiCustomList.GetByClass(ObjectClass: TClass): TRttiCustom;
begin
  result := RegisterClass(ObjectClass);
end;

function TRttiCustomList.RegisterClass(aObject: TObject): TRttiCustom;
begin
  result := PPointer(PPAnsiChar(aObject)^ + vmtAutoTable)^;
  if result = nil then
    result := DoRegister(PClass(aObject)^);
end;

function TRttiCustomList.RegisterAutoCreateFieldsClass(ObjectClass: TClass): TRttiCustom;
begin
  result := PPointer(PAnsiChar(ObjectClass) + vmtAutoTable)^;
  if (result = nil) or // caller is likely to have checked it - paranoiac we are
     not (rcfAutoCreateFields in result.Flags) then
    result := DoRegister(ObjectClass, [rcfAutoCreateFields]);
end;

procedure TRttiCustomList.RegisterClasses(const ObjectClass: array of TClass);
var
  i: PtrInt;
begin
  for i := 0 to high(ObjectClass) do
  begin
    if ObjectClass[i].InheritsFrom(TCollection) then
      raise ERttiException.CreateUtf8(
        'RegisterClasses(%): please call RegisterCollection() instead',
        [ObjectClass[i]]);
    RegisterClass(ObjectClass[i]);
  end;
end;

function TRttiCustomList.RegisterCollection(Collection: TCollectionClass;
  CollectionItem: TCollectionItemClass): TRttiCustom;
begin
  result := RegisterClass(Collection);
  if result <> nil then
  begin
    result.fCollectionItem := CollectionItem;
    result.fCollectionItemRtti := RegisterClass(CollectionItem);
  end;
end;

procedure TRttiCustomList.RegisterUnsafeSpiType(const Types: array of PRttiInfo);
var
  i: PtrInt;
begin
  for i := 0 to high(Types) do
    include(RegisterType(Types[i]).fFlags, rcfSpi);
end;

function TRttiCustomList.RegisterBinaryType(Info: PRttiInfo;
  BinarySize: integer): TRttiCustom;
begin
  result := RegisterType(Info).SetBinaryType(BinarySize);
end;

procedure TRttiCustomList.RegisterBinaryTypes(const InfoBinarySize: array of const);
var
  i, n: PtrInt;
begin
  n := length(InfoBinarySize);
  if (n <> 0) and
     (n and 1 = 0) then
    for i := 0 to (n shr 1) - 1 do
      if (InfoBinarySize[i * 2].VType <> vtPointer) or
         not(InfoBinarySize[i * 2 + 1].VType {%H-}in [vtInteger, vtInt64]) then
        raise ERttiException.Create('Rtti.RegisterBinaryTypes(?)')
      else if RegisterType(InfoBinarySize[i * 2].VPointer).
         SetBinaryType(InfoBinarySize[i * 2 + 1].VInteger) = nil then
        raise ERttiException.CreateUtf8('Rtti.RegisterBinaryTypes: %?',
           [PRttiInfo(InfoBinarySize[i * 2].VPointer)^.Name]);
end;

function TRttiCustomList.RegisterObjArray(DynArray: PRttiInfo;
  Item: TClass): TRttiCustom;
begin
  if DynArray^.Kind = rkDynArray then
    result := RegisterType(DynArray).SetObjArray(Item)
  else
    result := nil;
end;

procedure TRttiCustomList.RegisterObjArrays(const DynArrayItem: array of const);
var
  i, n: PtrInt;
begin
  n := length(DynArrayItem);
  if (n <> 0) and
     (n and 1 = 0) then
    for i := 0 to (n shr 1) - 1 do
      if (DynArrayItem[i * 2].VType <> vtPointer) or
         (DynArrayItem[i * 2 + 1].VType <> vtClass) then
        raise ERttiException.Create('Rtti.RegisterObjArrays([?])')
      else
        RegisterObjArray(DynArrayItem[i * 2].VPointer,
          DynArrayItem[i * 2 + 1].VClass);
end;

function TRttiCustomList.RegisterFromText(DynArrayOrRecord: PRttiInfo;
  const RttiDefinition: RawUtf8): TRttiCustom;
var
  P: PUtf8Char;
  rttisize: integer;
begin
  if (DynArrayOrRecord = nil) or
     not (DynArrayOrRecord^.Kind in rkRecordOrDynArrayTypes) then
    raise ERttiException.Create('Rtti.RegisterFromText(DynArrayOrRecord?)');
  result := RegisterType(DynArrayOrRecord);
  mormot.core.os.EnterCriticalSection(Table^.Lock);
  try
    if result.Kind = rkDynArray then
      if result.ArrayRtti = nil then
      begin
        result.fArrayRtti := RegisterFromText('', RttiDefinition);
        result := result.fArrayRtti;
        exit;
      end
      else
        result := result.ArrayRtti;
    result.PropsClear; // reset to the Base64 serialization if RttiDefinition=''
    P := pointer(RttiDefinition);
    if P <> nil then
    begin
      rttisize := result.Size; // was taken from RTTI
      result.SetPropsFromText(P, eeNothing, {NoRegister=}false);
      if result.Props.Size <> rttisize then
        raise ERttiException.CreateUtf8('Rtti.RegisterFromText(%): text ' +
          'definition  covers % bytes, but RTTI defined %',
          [DynArrayOrRecord^.RawName, result.Props.Size, rttisize]);
    end
    else if result.Kind in rkRecordTypes then
      result.Props.SetFromRecordExtendedRtti(result.Info); // only for Delphi 2010+
    result.SetParserType(result.Parser, result.ParserComplex);
  finally
    mormot.core.os.LeaveCriticalSection(Table^.Lock);
  end;
end;

function TRttiCustomList.RegisterFromText(const TypeName: RawUtf8;
  const RttiDefinition: RawUtf8): TRttiCustom;
var
  P: PUtf8Char;
  new: boolean;
begin
  mormot.core.os.EnterCriticalSection(Table^.Lock);
  try
    result := Find(pointer(TypeName), length(TypeName));
    new := result = nil;
    if new then
      result := GlobalClass.Create(nil)
    else if not (result.Kind in rkRecordTypes) then
      raise ERttiException.CreateUtf8('Rtti.RegisterFromText: existing % is a %',
        [TypeName, ToText(result.Kind)^]);
    result.PropsClear;
    P := pointer(RttiDefinition);
    result.SetPropsFromText(P, eeNothing, {NoRegister=}false);
    if new then
      result.NoRttiSetAndRegister(ptRecord, TypeName, nil, {NoRegister=}false);
  finally
    mormot.core.os.LeaveCriticalSection(Table^.Lock);
  end;
end;

procedure TRttiCustomList.RegisterFromText(
  const TypeInfoTextDefinitionPairs: array of const);
var
  i, n: PtrInt;
  d: RawUtf8;
begin
  n := length(TypeInfoTextDefinitionPairs);
  if (n <> 0) and
     (n and 1 = 0) then
    for i := 0 to (n shr 1) - 1 do
      if (TypeInfoTextDefinitionPairs[i * 2].VType <> vtPointer) or
         not VarRecToUtf8IsString(TypeInfoTextDefinitionPairs[i * 2 + 1], d) then
        raise ERttiException.Create('Rtti.RegisterFromText[?]')
      else
         RegisterFromText(TypeInfoTextDefinitionPairs[i * 2].VPointer, d);
end;


procedure CopyCollection(Source, Dest: TCollection);
var
  i: integer; // Items[] uses an integer
begin
  if (Source = nil) or
     (Dest = nil) or
     (Source.ClassType <> Dest.ClassType) then
    exit;
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for i := 0 to Source.Count - 1 do
      CopyObject(Source.Items[i], Dest.Add); // Assign() fails for most objects
  finally
    Dest.EndUpdate;
  end;
end;

procedure CopyStrings(Source, Dest: TStrings);
begin
  if (Source <> nil) and
     (Dest <> nil) then
    Dest.Assign(Source); // will do the copy VCL-style
end;

procedure CopyObject(aFrom, aTo: TObject);
var
  cf: TRttiCustom;
  rf, rt: PRttiCustomProps;
  pf, pt: PRttiCustomProp;
  i: integer;
  rvd: TRttiVarData;
begin
  if (aFrom <> nil) and
     (aTo <> nil) then
  begin
    cf := Rtti.RegisterClass(PClass(aFrom)^);
    if (cf.ValueRtlClass = vcCollection) and
       (PClass(aFrom)^ = PClass(aTo)^)  then
      // specific process of TCollection items
      CopyCollection(TCollection(aFrom), TCollection(aTo))
    else if (cf.ValueRtlClass = vcStrings) and
            PClass(aTo)^.InheritsFrom(TStrings) then
      // specific process of TStrings items using VCL-style copy
      TStrings(aTo).Assign(TStrings(aFrom))
    else if PClass(aTo)^.InheritsFrom(PClass(aFrom)^) then
      // fast copy from RTTI properties of the common (or same) hierarchy
      cf.Props.CopyProperties(pointer(aTo), pointer(aFrom))
    else
    begin
      // no common inheritance -> slower lookup by property name
      rf := @cf.Props;
      rt := @Rtti.RegisterClass(PClass(aTo)^).Props;
      pf := pointer(rf.List);
      for i := 1 to rf.Count do
      begin
        if pf^.Name <> '' then
        begin
          pt := rt.Find(pf^.Name);
          if pt <> nil then
          begin
            pf^.GetValue(pointer(aFrom), rvd);
            pt^.SetValue(pointer(aTo), rvd, {andclear=}true);
          end;
        end;
        inc(pf);
      end;
    end;
  end;
end;

function CopyObject(aFrom: TObject): TObject;
begin
  if aFrom = nil then
    result := nil
  else
  begin
    result := Rtti.RegisterClass(aFrom.ClassType).ClassNewInstance;
    CopyObject(aFrom, result);
  end;
end;

procedure SetDefaultValuesObject(Value: TObject);
var
  rtticustom: TRttiCustom;
  p: PRttiCustomProp;
  i: integer;
begin
  if Value = nil then
    exit;
  rtticustom := Rtti.RegisterClass(Value.ClassType);
  p := pointer(rtticustom.Props.List);
  for i := 1 to rtticustom.Props.Count do
  begin
    if p^.Value.Kind = rkClass then
      SetDefaultValuesObject(p^.Prop.GetObjProp(Value))
    else if p^.OrdinalDefault <> NO_DEFAULT then
      p^.Prop.SetInt64Value(Value, p^.OrdinalDefault);
    inc(p);
  end;
end;

procedure ClearObject(Value: TObject; FreeAndNilNestedObjects: boolean);
var
  rtticustom: TRttiCustom;
  p: PRttiCustomProp;
  i: integer;
begin
  if Value = nil then
    exit;
  rtticustom := Rtti.RegisterClass(Value.ClassType);
  p := pointer(rtticustom.Props.List);
  for i := 1 to rtticustom.Props.Count do
  begin
    if not FreeAndNilNestedObjects and
       (p^.Value.Kind = rkClass) then
      ClearObject(p^.Prop.GetObjProp(Value), false)
    else if p^.OffsetSet >= 0 then
      // for rkClass, _ObjClear() mimics FreeAndNil()
      p^.Value.ValueFinalizeAndClear(PAnsiChar(Value) + p^.OffsetSet)
    else
      p^.SetValue(pointer(Value), PRttiVarData(@NullVarData)^, {andclear=}false);
    inc(p);
  end;
end;

function IsObjectDefaultOrVoid(Value: TObject): boolean;
var
  rtticustom: TRttiCustom;
  p: PRttiCustomProp;
  i: integer;
begin
  if Value <> nil then
  begin
    result := false;
    rtticustom := Rtti.RegisterClass(Value.ClassType);
    p := pointer(rtticustom.Props.List);
    for i := 1 to rtticustom.Props.Count do
      if p^.ValueIsVoid(Value) then
        inc(p)
      else
        exit;
  end;
  result := true;
end;



procedure InitializeUnit;
var
  k: TRttiKind;
  t: TRttiParserType;
begin
  RTTI_FINALIZE[rkLString]   := @_StringClear;
  RTTI_FINALIZE[rkWString]   := @_WStringClear;
  RTTI_FINALIZE[rkVariant]   := @_VariantClear;
  RTTI_FINALIZE[rkArray]     := @_ArrayClear;
  RTTI_FINALIZE[rkRecord]    := @FastRecordClear;
  RTTI_FINALIZE[rkInterface] := @_InterfaceClear;
  RTTI_FINALIZE[rkDynArray]  := @_DynArrayClear;
  RTTI_COPY[rkLString]   := @_LStringCopy;
  RTTI_COPY[rkWString]   := @_WStringCopy;
  RTTI_COPY[rkVariant]   := @_VariantCopy;
  RTTI_COPY[rkArray]     := @_ArrayCopy;
  RTTI_COPY[rkRecord]    := @_RecordCopy;
  RTTI_COPY[rkInterface] := @_InterfaceCopy;
  RTTI_COPY[rkDynArray]  := @_DynArrayCopy;
  RTTI_TO_VARTYPE[rkInteger] := varInt64;
  RTTI_TO_VARTYPE[rkInt64]   := varWord64;
  RTTI_TO_VARTYPE[rkFloat]   := varDouble;
  RTTI_TO_VARTYPE[rkLString] := varString;
  RTTI_TO_VARTYPE[rkWString] := varOleStr;
  RTTI_TO_VARTYPE[rkVariant] := varVariant;
  RTTI_TO_VARTYPE[rkChar]    := varUnknown; // allocate temp RawUtf8 -> varString
  RTTI_TO_VARTYPE[rkWChar]   := varUnknown;
  RTTI_TO_VARTYPE[rkSString] := varUnknown;
  {$ifdef HASVARUSTRING}
  RTTI_FINALIZE[rkUString]   := @_StringClear; // share same PStrRec layout
  RTTI_COPY[rkUString]       := @_UStringCopy;
  RTTI_TO_VARTYPE[rkUString] := varUString;
  {$endif HASVARUSTRING}
  {$ifdef FPC}
  RTTI_FINALIZE[rkLStringOld] := @_StringClear;
  RTTI_FINALIZE[rkObject]     := @FastRecordClear;
  RTTI_COPY[rkLStringOld]     := @_LStringCopy;
  RTTI_COPY[rkObject]         := @_RecordCopy;
  RTTI_TO_VARTYPE[rkBool]       := varBoolean;
  RTTI_TO_VARTYPE[rkQWord]      := varWord64;
  RTTI_TO_VARTYPE[rkLStringOld] := varString;
  RTTI_TO_VARTYPE[rkObject]     := varAny;
  {$endif FPC}
  for k := low(k) to high(k) do
  begin
    // paranoid checks
    if Assigned(RTTI_FINALIZE[k]) <> (k in rkManagedTypes) then
      raise ERttiException.CreateUtf8('Unexpected RTTI_FINALIZE[%]', [ToText(k)^]);
    if Assigned(RTTI_COPY[k]) <> (k in rkManagedTypes) then
      raise ERttiException.CreateUtf8('Unexpected RTTI_COPY[%]', [ToText(k)^]);
    // TTextWriter.AddRttiVarData for TRttiCustomProp.GetValueDirect/GetValueGetter
    case k of
      rkEnumeration, rkSet, rkDynArray, rkClass, rkInterface, rkRecord, rkArray:
        RTTI_TO_VARTYPE[k] := varAny;
    end;
  end;
  RTTI_FINALIZE[rkClass] := @_ObjClear;
  PT_INFO[ptBoolean] := TypeInfo(boolean);
  PT_INFO[ptByte] := TypeInfo(byte);
  PT_INFO[ptCardinal] := TypeInfo(cardinal);
  PT_INFO[ptCurrency] := TypeInfo(Currency);
  PT_INFO[ptDouble] := TypeInfo(Double);
  PT_INFO[ptExtended] := TypeInfo(Extended);
  PT_INFO[ptInt64] := TypeInfo(Int64);
  PT_INFO[ptInteger] := TypeInfo(integer);
  PT_INFO[ptQWord] := TypeInfo(QWord);
  PT_INFO[ptRawByteString] := TypeInfo(RawByteString);
  PT_INFO[ptRawJson] := TypeInfo(RawJson);
  PT_INFO[ptRawUtf8] := TypeInfo(RawUtf8);
  PT_INFO[ptSingle] := TypeInfo(Single);
  PT_INFO[ptString] := TypeInfo(String);
  PT_INFO[ptSynUnicode] := TypeInfo(SynUnicode);
  PT_INFO[ptDateTime] := TypeInfo(TDateTime);
  PT_INFO[ptDateTimeMS] := TypeInfo(TDateTimeMS);
  {$ifdef HASNOSTATICRTTI} // for Delphi 7/2007: use fake TypeInfo()
  PT_INFO[ptGuid] := @_TGUID;
  PT_INFO[ptHash128] := @_THASH128;
  PT_INFO[ptHash256] := @_THASH256;
  PT_INFO[ptHash512] := @_THASH512;
  {$else}
  PT_INFO[ptGuid] := TypeInfo(TGUID);
  PT_INFO[ptHash128] := TypeInfo(THash128);
  PT_INFO[ptHash256] := TypeInfo(THash256);
  PT_INFO[ptHash512] := TypeInfo(THash512);
  {$endif HASNOSTATICRTTI}
  {$ifdef HASVARUSTRING}
  PT_INFO[ptUnicodeString] := TypeInfo(UnicodeString);
  PT_DYNARRAY[ptUnicodeString] := TypeInfo(TUnicodeStringDynArray);
  {$else}
  PT_INFO[ptUnicodeString] := TypeInfo(SynUnicode);
  PT_DYNARRAY[ptUnicodeString] := TypeInfo(TSynUnicodeDynArray);
  {$endif HASVARUSTRING}
  PT_INFO[ptUnixTime] := TypeInfo(TUnixTime);
  PT_INFO[ptUnixMSTime] := TypeInfo(TUnixMSTime);
  PT_INFO[ptVariant] := TypeInfo(Variant);
  PT_INFO[ptWideString] := TypeInfo(WideString);
  PT_INFO[ptWinAnsi] := TypeInfo(WinAnsiString);
  PT_INFO[ptWord] := TypeInfo(Word);
  // ptComplexTypes may have several matching TypeInfo() -> put generic
  PT_INFO[ptOrm] := TypeInfo(TID);
  PT_INFO[ptTimeLog] := TypeInfo(TTimeLog);
  PTC_INFO[pctTimeLog] := TypeInfo(TTimeLog);
  PTC_INFO[pctID] := TypeInfo(TID);
  PTC_INFO[pctCreateTime] := TypeInfo(TTimeLog);
  PTC_INFO[pctModTime] := TypeInfo(TTimeLog);
  // may be overriden to the exact TRecordReference/TRecordVersion TypeInfo()
  PTC_INFO[pctSpecificClassID] := TypeInfo(QWord);
  PTC_INFO[pctRecordReference] := TypeInfo(QWord);
  PTC_INFO[pctRecordReferenceToBeDeleted] := TypeInfo(QWord);
  PTC_INFO[pctRecordVersion] := TypeInfo(QWord);
  for t := succ(low(t)) to high(t) do
    if Assigned(PT_INFO[t]) = (t in (ptComplexTypes - [ptOrm, ptTimeLog])) then
      raise ERttiException.CreateUtf8('Unexpected PT_INFO[%]', [ToText(t)^]);
  PT_DYNARRAY[ptBoolean] := TypeInfo(TBooleanDynArray);
  PT_DYNARRAY[ptByte] := TypeInfo(TByteDynArray);
  PT_DYNARRAY[ptCardinal] := TypeInfo(TCardinalDynArray);
  PT_DYNARRAY[ptCurrency] := TypeInfo(TCurrencyDynArray);
  PT_DYNARRAY[ptDouble] := TypeInfo(TDoubleDynArray);
  PT_DYNARRAY[ptExtended] := TypeInfo(TExtendedDynArray);
  PT_DYNARRAY[ptInt64] := TypeInfo(TInt64DynArray);
  PT_DYNARRAY[ptInteger] := TypeInfo(TIntegerDynArray);
  PT_DYNARRAY[ptQWord] := TypeInfo(TQWordDynArray);
  PT_DYNARRAY[ptRawByteString] := TypeInfo(TRawByteStringDynArray);
  PT_DYNARRAY[ptRawJson] := TypeInfo(TRawJsonDynArray);
  PT_DYNARRAY[ptRawUtf8] := TypeInfo(TRawUtf8DynArray);
  PT_DYNARRAY[ptSingle] := TypeInfo(TSingleDynArray);
  PT_DYNARRAY[ptString] := TypeInfo(TStringDynArray);
  PT_DYNARRAY[ptSynUnicode] := TypeInfo(TSynUnicodeDynArray);
  PT_DYNARRAY[ptDateTime] := TypeInfo(TDateTimeDynArray);
  PT_DYNARRAY[ptDateTimeMS] := TypeInfo(TDateTimeMSDynArray);
  PT_DYNARRAY[ptGuid] := TypeInfo(TGuidDynArray);
  PT_DYNARRAY[ptHash128] := TypeInfo(THash128DynArray);
  PT_DYNARRAY[ptHash256] := TypeInfo(THash256DynArray);
  PT_DYNARRAY[ptHash512] := TypeInfo(THash512DynArray);
  PT_DYNARRAY[ptOrm] := TypeInfo(TIDDynArray);
  PT_DYNARRAY[ptTimeLog] := TypeInfo(TTimeLogDynArray);
  PT_DYNARRAY[ptUnixTime] := TypeInfo(TUnixTimeDynArray);
  PT_DYNARRAY[ptUnixMSTime] := TypeInfo(TUnixMSTimeDynArray);
  PT_DYNARRAY[ptVariant] := TypeInfo(TVariantDynArray);
  PT_DYNARRAY[ptWideString] := TypeInfo(TWideStringDynArray);
  PT_DYNARRAY[ptWinAnsi] := TypeInfo(TWinAnsiDynArray);
  PT_DYNARRAY[ptWord] := TypeInfo(TWordDynArray);
  // prepare global thread-safe TRttiCustomList
  Rtti.Init;
  ClassUnit := _ClassUnit;
  // redirect most used FPC RTL functions to optimized x86_64 assembly
  {$ifdef FPC_CPUX64}
  {$ifndef NOPATCHRTL}
  RedirectCode(@system.Move, @MoveFast);
  RedirectCode(@system.FillChar, @FillCharFast);
  PatchCode(@fpc_ansistr_incr_ref, @_ansistr_incr_ref, $17); // fpclen=$2f
  PatchJmp(@fpc_ansistr_decr_ref, @_ansistr_decr_ref, $27); // fpclen=$3f
  PatchJmp(@fpc_ansistr_assign, @_ansistr_assign, $3f);    // fpclen=$3f
  PatchCode(@fpc_ansistr_compare, @_ansistr_compare,$77); // fpclen=$12f
  PatchCode(@fpc_ansistr_compare_equal, @_ansistr_compare_equal,$57); // fpc=$cf
  PatchCode(@fpc_unicodestr_incr_ref, @_ansistr_incr_ref, $17);      // fpc=$2f
  PatchJmp(@fpc_unicodestr_decr_ref, @_ansistr_decr_ref, $27);      // fpc=$3f
  PatchJmp(@fpc_unicodestr_assign, @_ansistr_assign, $3f);         // fpc=$3f
  PatchCode(@fpc_dynarray_incr_ref, @_dynarray_incr_ref, $17);    // fpc=$2f
  PatchJmp(@fpc_dynarray_clear, @_dynarray_decr_ref, $2f,
    PtrUInt(@_dynarray_decr_ref_free));
  RedirectCode(@fpc_dynarray_decr_ref, @fpc_dynarray_clear);
  {$ifdef FPC_HAS_CPSTRING}
  // Delphi/Windows is never natively UTF-8, but FPC+Lazarus may be :)
  if DefaultSystemCodePage = CP_UTF8 then
  begin
    // dedicated UTF-8 concatenation RTL function replacements
    RedirectRtl(@_fpc_ansistr_concat, @_ansistr_concat_utf8);
    RedirectRtl(@_fpc_ansistr_concat_multi, @_ansistr_concat_multi_utf8);
  end;
  {$ifdef FPC_X64MM}
  RedirectCode(@fpc_ansistr_setlength, @_ansistr_setlength);
  {$endif FPC_X64MM}
  {$endif FPC_HAS_CPSTRING}
  {$ifdef FPC_X64MM}
  RedirectCode(@fpc_getmem, @_Getmem);
  RedirectCode(@fpc_freemem, @_Freemem);
  {$endif FPC_X64MM}
  {$endif NOPATCHRTL}
  {$endif FPC_CPUX64}
  // validate some redefined RTTI structures with compiler definitions
  assert(SizeOf(TRttiVarData) = SizeOf(TVarData));
  assert(@PRttiVarData(nil)^.PropValue = @PVarData(nil)^.VAny);
  {$ifdef FPC_OR_UNICODE}
  assert(SizeOf(TRttiRecordField) = SizeOf(TManagedField));
  {$endif FPC_OR_UNICODE}
end;


initialization
  InitializeUnit;

finalization
  Rtti.Done;
  
end.

