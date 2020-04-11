/// Framework Core Low-Level Cross-Compiler RTTI Definitions
// - this unit is a part of the freeware Synopse mORMot framework 2,
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
    - Managed Types Finalization or Copy
    - RTTI Value Types used for JSON Parsing
    - RTTI-based Registration for Custom JSON Parsing

    Purpose of this unit is to avoid any direct use of TypInfo.pas RTL unit,
    which is not exactly compatible between compilers, and lack of direct
    RTTI access with no memory allocation. We define pointers to RTTI
    record/object to access TypeInfo() via a set of explicit methods.
    Here fake record/objects are just wrappers around pointers defined in
    Delphi/FPC RTL's TypInfo.pas with the magic of inlining.

    We redefined all RTTI definitions as TRtti* types to avoid confusion
    with TypInfo unit names.

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  TypInfo,  // use official RTL for accurate layouts (especially FPC)
  mormot.core.base,
  mormot.core.os,
  mormot.core.text; // ESynException, and enumerations text process


{ ************* Low-Level Cross-Compiler RTTI Definitions }

type
  /// the kind of Exception raised by this unit
  ERttiException = class(ESynException);

  /// map TOrdType, to specify ordinal (rkInteger and rkEnumeration) storage size and sign
  // - note: on FPC, Int64 is stored as its own TRttiKind, not as rkInteger
  TRttiOrd = (roSByte, roUByte, roSWord, roUWord, roSLong, roULong
    {$ifdef FPC_NEWRTTI} ,roSQWord, roUQWord {$endif});

  /// map TFloatType, to specify floating point (ftFloat) storage size and precision
  TRttiFloat = (rfSingle, rfDouble, rfExtended, rfComp, rfCurr);

{$ifdef FPC}

  /// map TTypeKind, to specify available type families for FPC RTTI values
  // - FPC types differs from Delphi, and are taken from FPC typinfo.pp unit
  // - here below,  we defined rkLString instead of rkAString to match Delphi -
  // see https://lists.freepascal.org/pipermail/fpc-devel/2013-June/032360.html
  // "Compiler uses internally some LongStrings which is not possible to use
  // for variable declarations" so rkLStringOld seems never used in practice
  TRttiKind = (rkUnknown, rkInteger, rkChar, rkEnumeration, rkFloat, rkSet,
    rkMethod, rkSString, rkLStringOld {=rkLString}, rkLString {=rkAString},
    rkWString, rkVariant, rkArray, rkRecord, rkInterface,
    rkClass, rkObject, rkWChar, rkBool, rkInt64, rkQWord,
    rkDynArray, rkInterfaceRaw, rkProcVar, rkUString, rkUChar,
    rkHelper, rkFile, rkClassRef, rkPointer);

const
  /// potentially managed types in TRttiKind enumerates
  // - should match ManagedType*() functions
  rkManagedTypes = [rkLStringOld, rkLString, rkWstring, rkUstring, rkArray,
                    rkObject, rkRecord, rkDynArray, rkInterface, rkVariant];

  /// maps record or object in TRttiKind enumerates
  rkRecordTypes = [rkObject, rkRecord];

type
  ///  TTypeKind enumerate as defined in Delphi 6 and up
  // - dkUString and following appear only since Delphi 2009
  TDelphiType = (dkUnknown, dkInteger, dkChar, dkEnumeration, dkFloat,
    dkString, dkSet, dkClass, dkMethod, dkWChar, dkLString, dkWString,
    dkVariant, dkArray, dkRecord, dkInterface, dkInt64, dkDynArray,
    dkUString, dkClassRef, dkPointer, dkProcedure);

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
  TRttiKind = (rkUnknown, rkInteger, rkChar, rkEnumeration, rkFloat,
    rkSString, rkSet, rkClass, rkMethod, rkWChar, rkLString, rkWString,
    rkVariant, rkArray, rkRecord, rkInterface, rkInt64, rkDynArray
    {$ifdef UNICODE}, rkUString, rkClassRef, rkPointer, rkProcedure {$endif});

const
  /// potentially managed types in TRttiKind enumerates
  // - should match ManagedType*() functions
  rkManagedTypes = [rkLString, rkWstring, {$ifdef UNICODE} rkUstring, {$endif}
                    rkArray, rkRecord, rkDynArray, rkInterface, rkVariant];
  /// maps record or object in TTypeKind RTTI enumerates
  rkRecordTypes = [rkRecord];

{$endif FPC}

  /// maps long string in TRttiKind RTTI enumerates
  rkStringTypes =
    [rkLString, {$ifdef FPC} rkLStringOld, {$endif} rkWString
     {$ifdef HASVARUSTRING} , rkUString {$endif} ];

  /// maps 1, 8, 16, 32 and 64-bit ordinal in TRttiKind RTTI enumerates
  rkOrdinalTypes =
    [rkInteger, rkChar, rkWChar, rkEnumeration, rkSet, rkInt64
     {$ifdef FPC} , rkBool, rkQWord {$endif} ];

  /// all recognized TRTTIKind enumerates, i.e. all but rkUnknown
  rkAllTypes = [succ(low(TRTTIKind))..high(TRTTIKind)];

  /// quick retrieve how many bytes an ordinal consist in
  ORDTYPE_SIZE: array[TRttiOrd] of byte =
    (1, 1, 2, 2, 4, 4 {$ifdef FPC_NEWRTTI} , 8, 8 {$endif} );

  /// quick retrieve how many bytes a floating-point consist in
  FLOATTYPE_SIZE: array[TRttiFloat] of byte =
    (4, 8, {$ifdef TSYNEXTENDED80} 10 {$else} 8 {$endif}, 8, 8 );


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
  // - could be used e.g. by TSQLPropInfo to handled flattened properties
  PRttiPropDynArray = array of PRttiProp;

  /// pointer to all RTTI class properties definitions
  // - as returned by PRttiInfo.RttiProps()
  PRttiProps = ^TRttiProps;

  /// a wrapper to published properties of a class, as defined by compiler RTTI
  // - access properties for only a given class level, not inherited properties
  // - start enumeration by getting a PRttiProps with PRttiInfo.RttiProps(), then
  // use P := PropList to get the first PRttiProp, and iterate with P^.Next
  // - this enumeration is very fast and doesn't require any temporary memory,
  //  as in the TypInfo.GetPropInfos() PPropList usage
  // - for TSQLRecord, you should better use the RecordProps.Fields[] array,
  // which is faster and contains the properties published in parent classes
  TRttiProps = object
  public
    /// number of published properties in this object
    function PropCount: integer; {$ifdef HASINLINE} inline; {$endif}
    /// point to a TPropInfo packed array
    // - layout is as such, with variable TPropInfo storage size:
    // ! PropList: array[1..PropCount] of TPropInfo
    // - use TPropInfo.Next to get the next one:
    // ! P := PropList;
    // ! for i := 1 to PropCount do begin
    // !   // ... do something with P
    // !   P := P^.Next;
    // ! end;
    function PropList: PRttiProp; {$ifdef HASINLINE} inline; {$endif}
    /// retrieve a Field property RTTI information from a Property Name
    function FieldProp(const PropName: shortstring): PRttiProp;
  end;

  /// pointer to TClassType, as returned by PRttiInfo.RttiClass()
  // - equivalency to PClassData/PClassType as defined in old mORMot.pas
  PRttiClass = ^TRttiClass;

  /// a wrapper to class type information, as defined by the compiler RTTI
  // - get a PRttiClass with PRttiInfo.RttiClass()
  TRttiClass = object
  public
    /// the class type
    // - not defined as an inlined function, since first field is always aligned
    RttiClass: TClass;
    /// the parent class type information
    function ParentInfo: PRttiInfo; {$ifdef HASINLINE} inline; {$endif}
    /// the number of published properties
    function PropCount: integer; {$ifdef HASINLINE} inline; {$endif}
    /// the name (without .pas extension) of the unit were the class was defined
    // - then the PRttiProps information follows: use the method
    // RttiProps to retrieve its address
    function UnitName: ShortString; {$ifdef HASINLINE} inline; {$endif}
    /// get the information about the published properties of this class
    // - stored after UnitName memory
    function RttiProps: PRttiProps; {$ifdef HASINLINE} inline; {$endif}
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
    function EnumBaseType: PRttiEnumType; {$ifdef HASINLINE} inline; {$endif}
    function SetBaseType: PRttiEnumType;  {$ifdef HASINLINE} inline; {$endif}
  public
    /// specify ordinal storage size and sign
    // - is prefered to MaxValue to identify the number of stored bytes
    // - not defined as an inlined function, since first field is always aligned
    RttiOrd: TRttiOrd;
    /// first value of enumeration type, typicaly 0
    // - may be < 0 e.g. for boolean
    function MinValue: PtrInt;       {$ifdef HASINLINE} inline; {$endif}
    /// same as ord(high(type)): not the enumeration count, but the highest index
    function MaxValue: PtrInt;       {$ifdef HASINLINE} inline; {$endif}
    /// a concatenation of shortstrings, containing the enumeration names
    // - those shortstrings are not aligned whatsoever (even if
    // FPC_REQUIRES_PROPER_ALIGNMENT is set)
    function NameList: PShortString; {$ifdef HASINLINE} inline; {$endif}
    /// get the corresponding enumeration name
    // - return the first one if Value is invalid (>MaxValue)
    function GetEnumNameOrd(Value: cardinal): PShortString;
      {$ifdef HASINLINE} inline; {$endif}
    /// get the corresponding enumeration name
    // - return the first one if Value is invalid (>MaxValue)
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetEnumName(const Value): PShortString;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve all element names as a dynamic array of RawUTF8
    // - names could be optionally trimmed left from their initial lower chars
    procedure GetEnumNameAll(var result: TRawUTF8DynArray;
      TrimLeftLowerCase: boolean); overload;
    /// retrieve all element names as CSV, with optional quotes
    procedure GetEnumNameAll(var result: RawUTF8; const Prefix: RawUTF8 = '';
      quotedValues: boolean = false; const Suffix: RawUTF8 = '';
      trimedValues: boolean = false; unCamelCased: boolean = false); overload;
    /// retrieve all trimed element names as CSV
    procedure GetEnumNameTrimedAll(var result: RawUTF8; const Prefix: RawUTF8 = '';
      quotedValues: boolean = false; const Suffix: RawUTF8 = '');
    /// get all enumeration names as a JSON array of strings
    function GetEnumNameAllAsJSONArray(TrimLeftLowerCase: boolean;
      UnCamelCased: boolean = false): RawUTF8;
    /// get the corresponding enumeration ordinal value, from its name
    // - if EnumName does start with lowercases 'a'..'z', they will be searched:
    // e.g. GetEnumNameValue('sllWarning') will find sllWarning item
    // - if Value does not start with lowercases 'a'..'z', they will be ignored:
    // e.g. GetEnumNameValue('Warning') will find sllWarning item
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameValue(const EnumName: ShortString): Integer; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// get the corresponding enumeration ordinal value, from its name
    // - if Value does start with lowercases 'a'..'z', they will be searched:
    // e.g. GetEnumNameValue('sllWarning') will find sllWarning item
    // - if Value does not start with lowercases 'a'..'z', they will be ignored:
    // e.g. GetEnumNameValue('Warning') will find sllWarning item
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameValue(Value: PUTF8Char): Integer; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// get the corresponding enumeration ordinal value, from its name
    // - if Value does start with lowercases 'a'..'z', they will be searched:
    // e.g. GetEnumNameValue('sllWarning') will find sllWarning item
    // - if AlsoTrimLowerCase is TRUE, and EnumName does not start with
    // lowercases 'a'..'z', they will be ignored: e.g. GetEnumNameValue('Warning')
    // will find sllWarning item
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameValue(Value: PUTF8Char; ValueLen: integer;
      AlsoTrimLowerCase: boolean = true): Integer; overload;
    /// get the corresponding enumeration ordinal value, from its trimmed name
    function GetEnumNameValueTrimmed(Value: PUTF8Char; ValueLen: integer;
      ExactCase: boolean): Integer;
    /// get the corresponding enumeration name, without the first lowercase chars
    // (otDone -> 'Done')
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetEnumNameTrimed(const Value): RawUTF8;
      {$ifdef HASINLINE} inline; {$endif}
    /// get the enumeration names corresponding to a set value
    function GetSetNameCSV(Value: cardinal; SepChar: AnsiChar = ',';
      FullSetsAsStar: boolean = false): RawUTF8; overload;
    /// get the enumeration names corresponding to a set value
    procedure GetSetNameCSV(W: TBaseWriter; Value: cardinal; SepChar: AnsiChar = ',';
      FullSetsAsStar: boolean = false); overload;
    /// get the corresponding enumeration ordinal value, from its name without
    // its first lowercase chars ('Done' will find otDone e.g.)
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameTrimedValue(const EnumName: ShortString): Integer; overload;
    /// get the corresponding enumeration ordinal value, from its name without
    // its first lowercase chars ('Done' will find otDone e.g.)
    // - return -1 if not found (don't use directly this value to avoid any GPF)
    function GetEnumNameTrimedValue(Value: PUTF8Char; ValueLen: integer = 0): Integer; overload;
    /// compute how many bytes this type will use to be stored as a enumerate
    function SizeInStorageAsEnum: Integer;
      {$ifdef HASINLINE} inline; {$endif}
    /// compute how many bytes (1, 2, 4) this type will use to be stored as a set
    // - consider using TRttiInfo.SetEnumSize if ISFPC32 conditional is defined
    function SizeInStorageAsSet: Integer;
      {$ifdef HASINLINE} inline; {$endif}
    /// store an enumeration value from its ordinal representation
    procedure SetEnumFromOrdinal(out Value; Ordinal: PtrUInt);
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve the ordinal representation of an enumeration Value pointer
    function GetOrdinalFromEnum(Value: pointer): PtrUInt;
      {$ifdef HASINLINE} inline; {$endif}
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
  TRttiIntfFlag = (ifHasGuid, ifDispInterface, ifDispatch
    {$ifdef FPC} , ifHasStrGUID {$endif});
  /// define the set of interface abilities
  TRttiIntfFlags = set of TRttiIntfFlag;

  /// a wrapper to interface type information, as defined by the the compiler RTTI
  TRttiInterfaceTypeData = object
    /// ancestor interface type
    function IntfParent: PRttiInfo; {$ifdef HASINLINE} inline; {$endif}
    /// interface abilities
    function IntfFlags: TRttiIntfFlags; {$ifdef HASINLINE} inline; {$endif}
    /// interface 128-bit GUID
    function IntfGuid: PGUID; {$ifdef HASINLINE} inline; {$endif}
    /// where the interface has been defined
    function IntfUnit: PShortString; {$ifdef HASINLINE} inline; {$endif}
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

  /// convenient wrapper about PRttiInfo content and its more precise information
  // - may be cached between use for more efficient process
  TRttiCache = record
    /// the associated RTTI TypeInfo()
    Info: PRttiInfo;
    /// the size in bytes of a value of this type - equals Info.Rttisize
    Size: PtrInt;
    /// type-specific information
    case TRttiKind of
      rkEnumeration, rkSet: (
        EnumInfo: PRttiEnumType;
        EnumMask: cardinal;
        EnumMax:  cardinal);
      rkDynArray, rkArray: (
        ItemInfo: PRttiInfo;
        ItemSize: PtrInt;
        ItemCount: PtrInt; // rkArray only
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
    // - on FPC, will adjust 'integer'/'cardinal' from 'longint'/'longword' RTTI
    function Name: PShortString;          {$ifdef HASINLINE} inline; {$endif}
    /// efficiently finalize any (managed) type value
    // - do nothing for unmanaged types (e.g. integer)
    // - if you are sure that your type is managed, you may call directly
    // $ RTTI_FINALIZE[Info^.Kind](Data, Info);
    procedure Clear(Data: pointer);       {$ifdef HASINLINE} inline; {$endif}
    /// efficiently copy any (managed) type value
    // - do nothing for unmanaged types (e.g. integer)
    // - if you are sure that your type is managed, you may call directly
    // $ RTTI_COPY[Info^.Kind](Dest, Source, Info);
    procedure Copy(Dest, Source: pointer); {$ifdef HASINLINE} inline; {$endif}
    /// compute extended information about this RTTI type
    procedure ComputeCache(out Cache: TRttiCache);
    /// for ordinal types, get the storage size and sign
    function RttiOrd: TRttiOrd;           {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property is an unsigned 64-bit field (QWord/UInt64)
    function IsQWord: boolean;            {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property is a boolean field
    function IsBoolean: boolean;          {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property is a TSynCurrency field
    function IsSynCurrency: boolean;      {$ifdef HASINLINE} inline; {$endif}
    /// for rkFloat: get the storage size and precision
    // - will also properly detect our TSynCurrency internal type as rfCurr
    function RttiFloat: TRttiFloat;       {$ifdef HASINLINE} inline; {$endif}
    /// for rkEnumeration: get the enumeration type information
    function EnumBaseType: PRttiEnumType; overload; {$ifdef HASINLINE} inline; {$endif}
    /// for rkEnumeration: get the enumeration values information
    function EnumBaseType(out NameList: PShortString;
      out Max: integer): PRttiEnumType;   overload; {$ifdef HASINLINE} inline; {$endif}
    /// for rkSet: get the type information of its associated enumeration
    function SetEnumType: PRttiEnumType;  overload; {$ifdef HASINLINE} inline; {$endif}
    /// for rkSet: get the associated enumeration values information
    function SetEnumType(out NameList: PShortString;
      out Max: integer): PRttiEnumType;   overload; {$ifdef HASINLINE} inline; {$endif}
    /// for rkSet: in how many bytes this type is stored
    // - is very efficient on latest FPC only - i.e. ifdef ISFPC32
    function SetEnumSize: PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// compute in how many bytes this type is stored
    // - will use Kind (and RttiOrd/RttiFloat) to return the exact value
    function RttiSize: PtrInt;
    /// for rkRecordTypes: get the record size
    // - returns 0 if the type is not a record/object
    function RecordSize: PtrInt;  {$ifdef HASINLINE} inline; {$endif}
    /// for rkRecordTypes: retrieve RTTI information about all managed fields
    // of this record
    // - non managed fields (e.g. integers, double...) are not listed here
    // - also includes the total record size in bytes
    // - caller should ensure the type is indeed a record/object
    // - note: if FPC_OLDRTTI is defined, unmanaged fields are included
    procedure RecordManagedFields(out Fields: TRttiRecordManagedFields);
      {$ifdef HASINLINE} inline; {$endif}
    /// for rkRecordTypes: retrieve enhanced RTTI information about all fields
    // of this record
    // - this information is currently only available since Delphi 2010
    function RecordAllFields(out RecSize: PtrInt): TRttiRecordAllFields;
    /// for rkDynArray: get the dynamic array type information of the stored item
    // - caller should ensure the type is indeed a dynamic array
    function DynArrayItemType: PRttiInfo; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// for rkDynArray: get the dynamic array type information of the stored item
    // - this overloaded method will also return the item size in bytes
    // - caller should ensure the type is indeed a dynamic array
    function DynArrayItemType(out aDataSize: PtrInt): PRttiInfo; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// for rkDynArray: get the dynamic array size (in bytes) of the stored item
    function DynArrayItemSize: PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// for rkArray: get the static array type information of the stored item
    // - returns nil if the array type is unmanaged (i.e. behave like Delphi)
    // - aDataSize is the size in bytes of all aDataCount static items (not
    // the size of each item)
    // - caller should ensure the type is indeed a static array
    function ArrayItemType(out aDataCount, aDataSize: PtrInt): PRttiInfo;
      {$ifdef HASINLINE} inline; {$endif}
    /// for rkArray: get the size in bytes of all the static array items
    // - caller should ensure the type is indeed a static array
    function ArrayItemSize: PtrInt;       {$ifdef HASINLINE} inline; {$endif}
    /// recognize most used string types, returning their code page
    // - will return the exact code page on FPC and since Delphi 2009, from RTTI
    // - for non Unicode versions of Delphi, will recognize WinAnsiString as
    // CODEPAGE_US, RawUnicode as CP_UTF16, RawByteString as CP_RAWBYTESTRING,
    // AnsiString as 0, and any other type as RawUTF8
    // - it will also recognize TSQLRawBlob as the fake CP_SQLRAWBLOB codepage
    function AnsiStringCodePage: integer; {$ifdef HASCODEPAGE}inline;{$endif}
    {$ifdef HASCODEPAGE}
    /// returning the code page stored in the RTTI
    // - without recognizing e.g. TSQLRawBlob
    // - caller should ensure the type is indeed a rkLString
    function AnsiStringCodePageStored: integer; inline;
    {$endif HASCODEPAGE}
    /// for rkClass: get the class type information
    function RttiClass: PRttiClass;       {$ifdef HASINLINE} inline; {$endif}
    /// for rkClass: return the number of published properties in this class
    // - you can count the plain fields without any getter function, if you
    // do need only the published properties corresponding to some value
    // actually stored, and ignore e.g. any textual conversion
    function ClassFieldCount(onlyWithoutGetter: boolean): integer;
    /// for rkClass: fast and easy check if a class inherits from this RTTI
    function InheritsFrom(AClass: TClass): boolean;
    /// for rkInterface: get the interface type information
    function InterfaceType: PRttiInterfaceTypeData; {$ifdef HASINLINE} inline; {$endif}
    /// for rkInterface: get the TGUID of a given interface type information
    // - returns nil if this type is not an interface
    function InterfaceGUID: PGUID;
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
  // - as returned by TPropInfo.Getter/Setter methods
  TRttiPropCall = (
    rpcNone, rpcField, rpcMethod, rpcIndexed);

  /// variant-like value as returned by TRttiProp.GetValue()
  // - simple values (integers or floats) are set into Value.Data
  // - strings or variants are retrieved as varByref - getters with NeedsClear=true
  // - rkEnumeration, rkSet, rkDynArray, rkClass, rkInterface, rkRecord and
  // rkObject are stored as varAny pointer to the field value (varNull if no address)
  // - will be properly handled by TTextWriter.AddVariant/AddTypedJSON
  // - can be casted as a variant value, but contains RTTI Info and clear flag:
  // ! if Value.NeedsClear then VarClearProc(Value.Data);
  TRttiVarData = packed record
    case integer of
    varVariant: (
      Data: TVarData);
    varAny: (
      DataType: word;
      NeedsClear: boolean;
      Padding1: byte;
      {$ifdef CPU32}
      Info: PRttiInfo;
      VInt64: Int64;
      {$else}
      Padding4: cardinal;
      VInt64: Int64;
      Info: PRttiInfo;
      {$endif CPU32});
    varUnknown: (
      VType: cardinal); // maps DataType + NeedsClear + Padding1
  end;
  PRttiVarData = ^TRttiVarData;

  { TRttiProp }

  /// a wrapper containing a RTTI class property definition
  // - used for direct Delphi / UTF-8 SQL type mapping/conversion
  // - doesn't depend on RTL's TypInfo unit, to enhance cross-compiler support
  TRttiProp = object
  public
    /// raw retrieval of the property read access definition
    // - note: 'var Call' generated incorrect code on Delphi XE4 -> use PMethod
    function Getter(Instance: TObject; Call: PMethod): TRttiPropCall; {$ifdef HASINLINE} inline; {$endif}
    /// raw retrieval of the property access definition
    function Setter(Instance: TObject; Call: PMethod): TRttiPropCall; {$ifdef HASINLINE} inline; {$endif}
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
    procedure GetShortStrProp(Instance: TObject; var Value: RawUTF8);
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
    procedure GetCurrencyProp(Instance: TObject; var Value: TSynCurrency);
    /// raw assignment of rkFloat/currency
    procedure SetCurrencyProp(Instance: TObject; const Value: TSynCurrency);
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
    procedure GetVariantProp(Instance: TObject; var result: Variant);
    /// raw assignment of rkVariant
    procedure SetVariantProp(Instance: TObject; const Value: Variant);
  public
    /// contains the index value of an indexed class data property
    // - outside SQLite3, this can be used to define a VARCHAR() length value
    // for the textual field definition (sftUTF8Text/sftAnsiText); e.g.
    // the following will create a NAME VARCHAR(40) field:
    // ! Name: RawUTF8 index 40 read fName write fName;
    // - is used by a dynamic array property for fast usage of the
    // TSQLRecord.DynArray(DynArrayFieldIndex) method
    function Index: Integer; {$ifdef HASINLINE} inline; {$endif}
    /// contains the default value (NO_DEFAULT=$80000000 indicates none set)
    // when an ordinal or set property is saved as TPersistent
    // - see TPropInfo.DefaultOr0/DefaultOrVoid for easy use
    function Default: Longint; {$ifdef HASINLINE} inline; {$endif}
    /// index of the property in the current inherited class definition
    // - first name index at a given class level is 0
    // - index is reset to 0 at every inherited class level
    function NameIndex: integer; {$ifdef HASINLINE} inline; {$endif}
    /// the property Name
    function Name: PShortString; {$ifdef HASINLINE} inline; {$endif}
    /// the type information of this property
    // - will de-reference the PropType pointer on Delphi and newer FPC compilers
    function TypeInfo: PRttiInfo; {$ifdef HASINLINE} inline; {$endif}
    /// get the next property information
    // - no range check: use RttiProps()^.PropCount to determine the properties count
    // - get the first PRttiProp with RttiProps()^.PropList
    function Next: PRttiProp; {$ifdef HASINLINE} inline; {$endif}
    /// return FALSE (AS_UNIQUE) if was marked as "stored AS_UNIQUE"
    //  (i.e. "stored false"), or TRUE by default
    // - if Instance=nil, will work only at RTTI level, not with field or method
    // (and will return TRUE if nothing is defined in the RTTI)
    function IsStored(Instance: TObject): boolean;
    /// return the Default RTTI value defined for this property, or 0 if not set
    function DefaultOr0: integer; {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property has its Default RTTI value, or is 0/""/nil
    function IsDefaultOrVoid(Instance: TObject): boolean;
    /// compute in how many bytes this property is stored
    function FieldSize: PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property has no getter but direct field read
    function GetterIsField: boolean;    {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property has no setter but direct field write
    function SetterIsField: boolean;    {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property has a write setter or direct field
    function WriteIsDefined: boolean;   {$ifdef HASINLINE} inline; {$endif}
    /// returns the low-level field read address, if GetterIsField is TRUE
    function GetterAddr(Instance: pointer): pointer; {$ifdef HASINLINE} inline; {$endif}
    /// returns the low-level field write address, if SetterIsField is TRUE
    function SetterAddr(Instance: pointer): pointer; {$ifdef HASINLINE} inline; {$endif}
    /// low-level getter of the field value memory pointer
    // - return NIL if both getter and setter are methods
    function GetFieldAddr(Instance: TObject): pointer; {$ifdef HASINLINE} inline; {$endif}

    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - returns an Int64 to properly support cardinal values
    // - return -1 on any error
    function GetOrdValue(Instance: TObject): Int64; {$ifdef HASINLINE} inline; {$endif}
    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - ordinal properties smaller than rkInt64 will return an Int64-converted
    // value (e.g. rkInteger)
    // - return 0 on any error
    function GetInt64Value(Instance: TObject): Int64;
    /// low-level getter of the currency property value of a given instance
    // - this method will check if the corresponding property is exactly currency
    // - return 0 on any error
    procedure GetCurrencyValue(Instance: TObject; var Value: TSynCurrency);
    /// low-level getter of the floating-point property value of a given instance
    // - this method will check if the corresponding property is floating-point
    // - return 0 on any error
    function GetDoubleValue(Instance: TObject): double;
    /// low-level setter of the floating-point property value of a given instance
    // - this method will check if the corresponding property is floating-point
    procedure SetDoubleValue(Instance: TObject; const Value: double);
    /// low-level getter of the long string property content of a given instance
    // - just a wrapper around low-level GetLongStrProp() function
    // - call GetLongStrValue() method if you want a conversion into RawUTF8
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
    /// retrieve rkLString, rkSString, rkUString, rkWString, rkChar, rkWChar as RawUTF8
    // - this would make heap allocations and encoding conversion, so may be slow
    procedure GetAsString(Instance: TObject; var Value: RawUTF8);
    /// very fast retrieval of any property value into a TVarData-like
    // - property RTTI is stored in Value.Info
    // - minimize memory allocation returning strings and variants as varByRef
    // - complex TRttiVarData with varAny pointer will be properly handled by
    // TTextWriter.AddVariant/AddTypedJSON (e.g. rkEnumeration or rkDynArray)
    procedure GetValue(Instance: TObject; out Value: TRttiVarData);
  end;

const
  NO_DEFAULT = longint($80000000);

/// retrieve the text name of one TRttiKind enumerate
function ToText(k: TRttiKind): PShortString; overload;


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

function GetTypeData(TypeInfo: pointer): PTypeData; inline;

{$endif HASINLINE}

{$ifdef ISDELPHI }// Delphi requires those definitions for proper inlining

const
  NO_INDEX = longint($80000000);

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
//  !      for i := 1 to GetRttiProps(CT,P) do begin
//  !        // use P^
//  !        P := P^.Next;
//  !      end;
//  !      CT := GetClassParent(CT);
//  !    until CT=nil;
//  !  end;
// such a loop is much faster than using the RTL's TypeInfo or RTTI units
function GetRttiProp(C: TClass; out PropInfo: PRttiProp): integer;

/// retrieve the total number of properties for a class, including its parents
function ClassFieldCountWithParents(C: TClass;  onlyWithoutGetter: boolean = false): integer;

type
  /// information about one method, as returned by GetPublishedMethods
  TPublishedMethodInfo = record
    /// the method name
    Name: RawUTF8;
    /// a callback to the method, for the given class instance
    Method: TMethod;
  end;
  /// information about all methods, as returned by GetPublishedMethods
  TPublishedMethodInfoDynArray = array of TPublishedMethodInfo;

/// retrieve published methods information about any class instance
// - will optionaly accept a Class, in this case Instance is ignored
// - will work with FPC and Delphi RTTI
function GetPublishedMethods(Instance: TObject; out Methods: TPublishedMethodInfoDynArray;
  aClass: TClass = nil): integer;


{ *************** Enumerations RTTI }

/// helper to retrieve the text of an enumerate item
// - just a wrapper around
// $ PRttiInfo(aTypeInfo)^.EnumBaseType.GetEnumNameOrd(aIndex)
function GetEnumName(aTypeInfo: pointer; aIndex: integer): PShortString;

/// helper to retrieve all texts of an enumerate
// - may be used as cache for overloaded ToText() content
procedure GetEnumNames(aTypeInfo: pointer; aDest: PPShortString);

/// helper to retrieve all trimmed texts of an enumerate
// - may be used as cache to retrieve UTF-8 text without lowercase 'a'..'z' chars
procedure GetEnumTrimmedNames(aTypeInfo: pointer; aDest: PRawUTF8); overload;

/// helper to retrieve all trimmed texts of an enumerate as UTF-8 strings
function GetEnumTrimmedNames(aTypeInfo: pointer): TRawUTF8DynArray; overload;

/// helper to retrieve the index of an enumerate item from its text
// - returns -1 if aValue was not found
// - will search for the exact text and also trim the lowercase 'a'..'z' chars on
// left side of the text if no exact match is found and AlsoTrimLowerCase is TRUE
function GetEnumNameValue(aTypeInfo: pointer; aValue: PUTF8Char; aValueLen: PtrInt;
  AlsoTrimLowerCase: boolean = false): Integer; overload;

/// retrieve the index of an enumerate item from its left-trimmed text
// - text comparison is case-insensitive for A-Z characters
// - will trim the lowercase 'a'..'z' chars on left side of the supplied aValue text
// - returns -1 if aValue was not found
function GetEnumNameValueTrimmed(aTypeInfo: pointer;
  aValue: PUTF8Char; aValueLen: PtrInt): integer;

/// retrieve the index of an enumerate item from its left-trimmed text
// - text comparison is case-sensitive for A-Z characters
// - will trim the lowercase 'a'..'z' chars on left side of the supplied aValue text
// - returns -1 if aValue was not found
function GetEnumNameValueTrimmedExact(aTypeInfo: pointer;
  aValue: PUTF8Char; aValueLen: PtrInt): integer;

/// helper to retrieve the index of an enumerate item from its text
function GetEnumNameValue(aTypeInfo: pointer; const aValue: RawUTF8;
  AlsoTrimLowerCase: boolean = false): Integer; overload;

/// helper to retrieve the CSV text of all enumerate items defined in a set
function GetSetName(aTypeInfo: pointer; const value): RawUTF8;

/// helper to retrieve the CSV text of all enumerate items defined in a set
procedure GetSetNameShort(aTypeInfo: pointer; const value; out result: ShortString;
  trimlowercase: boolean = false);


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
    Name: RawUTF8;
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
    Name: RawUTF8;
    /// the unit where the interface was defined
    UnitName: RawUTF8;
    /// the associated GUID of this interface
    GUID: TGUID;
    /// the interface methods
    Methods: array of TRttiMethod;
  end;
  PRttiInterface = ^TRttiInterface;

/// retrieve methods information of a given IInvokable
// - all methods will be added, also from inherited interface definitions
// - returns the number of methods detected
function GetRttiInterface(aTypeInfo: pointer; out aDefinition: TRttiInterface): integer;


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

/// clear a record content
// - caller should ensure the type is indeed a record/object
// - see also TRttiInfo.Clear if you want to finalize any type
// - same as RTTI_FINALIZE[rkRecord]()
function FastRecordClear(Value: pointer; Info: PRttiInfo): PtrInt;

/// low-level finalization of a dynamic array of RawUTF8
// - faster than RTL Finalize() or setting nil
procedure RawUTF8DynArrayClear(var Value: TRawUTF8DynArray);
  {$ifdef HASINLINE}inline;{$endif}

/// initialize a record content
// - calls FastRecordClear() and FillCharFast() with 0
// - do nothing if the TypeInfo is not from a record/object
procedure RecordZero(Dest: pointer; Info: PRttiInfo);

/// efficiently copy several (dynamic) array items
// - faster than the RTL CopyArray() function
procedure CopySeveral(Dest, Source: PByte; SourceCount: PtrInt;
  ItemInfo: PRttiInfo; ItemSize: PtrInt);

/// create a dynamic array from another one
// - same as RTTI_COPY[rkDynArray] but with an optional external source count
procedure DynArrayCopy(Dest, Source: PByte; Info: PRttiInfo;
  SourceExtCount: PInteger);


{ ************* Managed Types Finalization or Copy }

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
  /// the kind of variables handled by TJSONCustomParser
  // - the last item should be ptCustom, for non simple types
  // - ptORM is recognized from TID, T*ID, TRecordReference,
  // TRecordReferenceToBeDeleted and TRecordVersion type names
  // - ptTimeLog is recognized from TTimeLog, TCreateTime and TModTime
  // - other types (not ptComplexTypes) are recognized by their genuine type name
  // - ptUnicodeString is defined even if not available prior to Delphi 2009
  // - replace deprecated TJSONCustomParserRTTIType type from old mORMot 1.18
  // - TDynArrayKind is now an alias to this genuine enumerate
  TRTTIParserType = (
    ptNone,
    ptArray, ptBoolean, ptByte, ptCardinal, ptCurrency, ptDouble, ptExtended,
    ptInt64, ptInteger, ptQWord, ptRawByteString, ptRawJSON, ptRawUTF8,
    ptRecord, ptSingle, ptString, ptSynUnicode, ptDateTime, ptDateTimeMS,
    ptGUID, ptHash128, ptHash256, ptHash512, ptORM, ptTimeLog, ptUnicodeString,
    ptUnixTime, ptUnixMSTime, ptVariant, ptWideString, ptWinAnsi, ptWord,
    ptEnumeration, ptSet, ptClass, ptDynArray, ptInterface, ptCustom);

  /// the complex kind of variables for ptTimeLog and ptORM TRTTIParserType
  // - as recognized by TypeNameToParserType/TypeInfoToParserType
  TRTTIParserComplexType = (
    pctNone,
    pctTimeLog,
    pctCreateTime,
    pctModTime,
    pctID,
    pctSpecificClassID,
    pctRecordReference,
    pctRecordReferenceToBeDeleted,
    pctRecordVersion);

  PTRTTIParserType = ^TRTTIParserType;
  TRTTIParserTypes = set of TRTTIParserType;
  PRTTIParserComplexType = ^TRTTIParserComplexType;

const
  /// map a PtrInt type to the TRTTIParserType set
  ptPtrInt  = {$ifdef CPU64} ptInt64 {$else} ptInteger {$endif};

  /// map a PtrUInt type to the TRTTIParserType set
  ptPtrUInt = {$ifdef CPU64} ptQWord {$else} ptCardinal {$endif};

  /// which TRTTIParserType are not simple types
  // - ptTimeLog and ptORM are complex, since more than one TypeInfo() may
  // map to their TRTTIParserType - see also TRTTIParserComplexType
  ptComplexTypes =
    [ptArray, ptRecord, ptCustom, ptTimeLog, ptORM,
     ptDynArray, ptEnumeration, ptSet, ptClass, ptInterface];

  /// which TRTTIParserType types don't need memory management
  ptUnmanagedTypes =
    [ptBoolean..ptQWord, ptSingle, ptDateTime..ptTimeLog,
     ptUnixTime, ptUnixMSTime, ptWord..ptClass];

  /// which TRTTIParserType types are (usually) serialized as JSON "text"
  // - actual serialization may depend e.g. on TTextWriterWriteObjectOptions
  ptStringTypes =
    [ptRawByteString .. ptRawUTF8, ptString .. ptHash512, ptTimeLog,
     ptUnicodeString, ptWideString, ptWinAnsi];

var
  /// simple lookup to the plain RTTI type of most simple managed types
  // - nil for unmanaged types (e.g. rkOrdinals) or for more complex types
  // requering additional PRttiInfo (rkRecord, rkDynArray, rkArray...)
  PT_INFO: array[TRTTIParserType] of PRttiInfo;

  /// simple lookup to the size in bytes of TRTTIParserType values
  PT_SIZE: array[TRTTIParserType] of byte = (
    0, 0, 1, 1, 4, 8, 8, 8,
    8, 4, 8, SizeOf(pointer), SizeOf(pointer), SizeOf(pointer),
    0, 4, SizeOf(pointer), SizeOf(pointer), 8, 8,
    16, 16, 32, 64, 8, 8, SizeOf(pointer), 8, 8,
    SizeOf(variant), SizeOf(pointer), SizeOf(pointer), 2,
    0, 0, SizeOf(pointer), SizeOf(pointer), SizeOf(pointer), 0);

const
  /// type definition name lookup to the TRTTIParserType values
  // - ptComplexTypes types should see PTC_NAME[] constant
  PT_NAME: array[TRTTIParserType] of RawUTF8 = (
    '', '', 'boolean', 'byte', 'cardinal', 'currency', 'double', 'extended',
    'Int64', 'integer', 'QWord', 'RawByteString', 'RawJSON', 'RawUTF8',
    '', 'single', 'string', 'SynUnicode', 'TDateTime', 'TDateTimeMS',
    'TGUID', 'THash128', 'THash256', 'THash512', '', '', 'UnicodeString',
    'TUnixTime', 'TUnixMSTime', 'variant', 'WideString', 'WinAnsi', 'word',
    '', '', '', '', '', '');

  /// type definition name lookup to the TRTTIParserComplexType values
  // - for ptComplexTypes types, with PT_NAME[]=''
  // - ptcSpecificClassID returns '' since T....ID types are variable
  PTC_NAME: array[TRTTIParserComplexType] of RawUTF8 = (
    '', 'TTimeLog', 'TCreateTime', 'TModTime', 'TID', '',
    'TRecordReference', 'TRecordReferenceToBeDeleted', 'TRecordVersion');

/// retrieve the text name of one TRTTIParserType enumerate
function ToText(t: TRTTIParserType): PShortString; overload;

/// recognize a simple value type from a supplied type name
// - from the standard type names ('byte', 'string', 'RawUTF8', 'TGUID'...)
// - will return ptNone for any unknown type
// - if ItemTypeName is set, it will contain the uppercased Name/NameLen text
// - for ptORM and ptTimeLog, optional Complex will contain the specific type found
function TypeNameToParserType(Name: PUTF8Char; NameLen: integer;
  ItemTypeName: PRawUTF8; Complex: PRTTIParserComplexType = nil): TRTTIParserType; overload;

/// recognize a simple value type from a supplied type name
// - from the registered custom serialization type names
// - will return ptNone for any unknown type
function TypeNameToParserType(Name: PShortString;
  Complex: PRTTIParserComplexType = nil): TRTTIParserType; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// recognize a simple value type from a supplied type name
// - from the registered custom serialization type names
// - will return ptNone for any unknown type
function TypeNameToParserType(const Name: RawUTF8;
  Complex: PRTTIParserComplexType = nil): TRTTIParserType; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// recognize a simple value type from a supplied type information
// - if FirstSearchByName=true, will first call TypeNameToParserType(Info^.Name^)
// - will return ptNone for any unknown type
function TypeInfoToParserType(Info: PRttiInfo; FirstSearchByName: boolean = true;
  Complex: PRTTIParserComplexType = nil): TRTTIParserType; overload;

/// recognize a simple value type from a dynamic array RTTI
// - if ExactType=false, will approximate the first field
function DynArrayTypeInfoToParserType(DynArrayInfo, ElemInfo: PRttiInfo;
  ElemSize: integer; ExactType: boolean; out FieldSize: integer;
  Complex: PRTTIParserComplexType = nil): TRTTIParserType;


{ ************** RTTI-based Registration for Custom JSON Parsing }

const
  /// TRttiCustomList stores its TypeInfo() by PRttiInfo.Kind + NameLen and 7
  // - optimized "hash table of the poor" (tm) for Find(TypeInfo) and Find(Name)
  // - should be a bit mask (i.e. power of two minus 1)
  RTTICUSTOMTYPEINFOHASH = 7;

type
  TRttiCustom = class;

  /// define specific behavior for a given TypeInfo/PRttIinfo
  // - rcfObjArray is for T*ObjArray dynamic arrays
  // - rcfBinary is for hexadecimal serialization of integers
  // - rcfHasNestedProperties is set e.g. for rkDynArray or rkClass
  // - rcReadIgnoreUnknownFields will let JSON unserialization ignore unknown
  // fields for this class/record
  TRttiCustomFlag = (
    rcfObjArray, rcfBinary, rcfHasNestedProperties, rcReadIgnoreUnknownFields);
  /// define specific behaviors for a given TypeInfo/PRttIinfo
  TRttiCustomFlags = set of TRttiCustomFlag;

  /// store information about one property/field of a given TypeInfo/PRttIinfo
  // - used by both rkClass for published properties, and rkRecord/rkObject
  // for nested fields
  TRttiCustomProp = object
  protected
    function InitFrom(RttiProp: PRttiProp): PtrInt;
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
    /// map to Prop^.Name or a customized field/property name
    Name: PShortString;
    /// store standard RTTI of this published property
    // - equals nil for rkRecord/rkObject nested field
    Prop: PRttiProp;
  end;
  PRttiCustomProp = ^TRttiCustomProp;

  /// store information about the properties/fields of a given TypeInfo/PRttiInfo
  TRttiCustomPropDynArray = array of TRttiCustomProp;

  /// store information about all properties/fields of a given TypeInfo/PRttIinfo
  // - includes parent properties
  TRttiCustomProps = object
  protected
    procedure AddFrom(ClassInfo: PRttiInfo; IncludeParents: boolean);
  public
    /// one List[] item per property/field
    List: TRttiCustomPropDynArray;
    /// how many properties/fields are in List[]
    Count: PtrInt;
    /// total size, in bytes, of all properties/fields
    // - equals the sum of List[].Value.Size
    Size: PtrInt;
    /// locate a property/field by name
    function Find(const PropName: shortstring): PRttiCustomProp; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// locate a property/field by name
    function Find(PropName: PUTF8Char; PropNameLen: PtrInt): PRttiCustomProp; overload;
    /// customize a property/field name
    function NameChange(const Old, New: shortstring): PRttiCustomProp;
  end;

  /// allow to customize the process of a given TypeInfo/PRttiInfo
  // - a global list of TRttiCustom instances mapping TypeInfo() is maintained
  // - never instantiate this class directly, but call RttiCustom.FindOrRegister
  TRttiCustom = class
  protected
    fKind: TRttiKind;
    fFlags:TRttiCustomFlags;
    fParser: TRTTIParserType;
    fParserComplex: TRTTIParserComplexType;
    fRtti: TRttiCache;
    fBinarySize: integer;
    fProps: TRttiCustomProps;
    fArrayRtti: TRttiCustom;
    fObjArrayClass: TClass;
    fFinalize: TRttiFinalizer;
    fCopy: TRttiCopier;
    // callbacks used by mormot.core.json.pas
    fJsonSave: pointer; // contains TRttiJsonSave - used if fJsonWriter=nil
    fJsonReader, fJsonWriter: TMethod;
    // slot used by mormot.orm.base.pas
    fORM: TObject;
    // called by TRttiCustomList.RegisterObjArray/RegisterBinaryType
    function SetObjArray(Item: TClass): TRttiCustom;
    function SetBinaryType(BinarySize: integer): TRttiCustom;
  public
    /// initialize the customizer class from known RTTI
    constructor Create(aInfo: PRttiInfo); virtual;
    /// customize the Parser and Parser Complex kinds
    // - overriden in mormot.core.json for proper JSON process
    // - returns self to allow cascaded calls as a fluent interface
    function SetParserType(aParser: TRTTIParserType;
      aParserComplex: TRTTIParserComplexType): TRttiCustom; virtual;
    /// efficiently finalize a stored value of this type
    // - if rcfObjArray is defined in Flags, will release all nested TObject
    function ValueFinalize(Data: pointer): PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// efficiently copy a stored value of this type
    function ValueCopy(Dest, Source: pointer): PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the Value is 0 / nil / '' / null
    function ValueIsVoid(Data: pointer): boolean; virtual;
    /// low-level RTTI kind
    // - equals Info.Kind
    property Kind: TRttiKind read fKind;
    /// define specific behavior for this type
    property Flags: TRttiCustomFlags read fFlags write fFlags;
    /// direct access to the low-level RTTI TypeInfo() pointer
    property Info: PRttiInfo read fRtti.Info;
    /// direct access to the low-level value size in bytes of this type, from RTTI
    // - for rkDynArray,
    property Size: PtrInt read fRtti.Size;
    /// direct access to the ready-to-use RTTI
    property Rtti: TRttiCache read fRtti;
    /// high-level Parser kind
    property Parser: TRTTIParserType read fParser;
    /// high-level Parser Complex kind
    property ParserComplex: TRTTIParserComplexType read fParserComplex;
    /// store information about the properties/fields of this type
    // - only set for rkClass and rkRecord/rkObject
    property Props: TRttiCustomProps read fProps;
    /// shortcut to the TRttiCustom of the item of a (dynamic) array
    // - only set for rkArray and rkDynArray
    property ArrayRtti: TRttiCustom read fArrayRtti;
    /// store the number of bytes for hexadecimal serialization for rcfBinary
    // - used when rcfBinary is defined in Flags; equals 0 if disabled (default)
    property BinarySize: integer read fBinarySize;
    /// store the class of a T*ObjArray dynamic array
    // - shortcut to ArrayRtti.RttiClass.RttiClass
    // - used when rcfObjArray is defined in Flags
    property ObjArrayClass: TClass read fObjArrayClass;
  end;

  /// meta-class of TRttiCustom
  TRttiCustomClass = class of TRttiCustom;

  /// maintain a thread-safe list of PRttiInfo/TRttiCustom registration
  TRttiCustomList = object
  private
    // for DoRegister thread-safety
    Lock: TRTLCriticalSection;
    // used to release memory used by registered customizations
    Instances: array of TRttiCustom;
    // store PRttiInfo/TRttiCustom pairs by TRttiKind.Kind+Name[0] for efficiency
    Pairs: array[succ(low(TRttiKind)) .. high(TRttiKind)] of
           array[0..RTTICUSTOMTYPEINFOHASH] of TPointerDynArray;
    /// called by FindOrRegister() for proper inlining
    function DoRegister(Info: PRttiInfo): TRttiCustom;
  public
    /// which kind of TRttiCustom is to be used for registration
    // - properly set e.g. by mormot.core.json.pas for JSON support
    GlobalClass: TRttiCustomClass;
    /// how many TRttiCustom instances have been registered
    Count: integer;
    /// efficient search of TRttiCustom from a given RTTI TypeInfo()
    // - returns nil if Info is not known
    // - call RegisterType() if you want to initialize the type via its RTTI
    // - not inlined since less efficient code is generated
    function Find(Info: PRttiInfo): TRttiCustom; overload;
    /// efficient search of TRttiCustom from a given TObject class
    // - returns nil if Info is not known
    function Find(ObjectClass: TClass): TRttiCustom; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// efficient search of TRttiCustom from a given type name
    function Find(Name: PUTF8Char; NameLen: PtrInt;
      Kind: TRttiKind): TRttiCustom; overload;
    /// efficient search of TRttiCustom from a given type name
    function Find(Name: PUTF8Char; NameLen: PtrInt;
      Kinds: TRttiKinds = []): TRttiCustom; overload;
    /// efficient search of TRttiCustom from a given type name
    function Find(const Name: shortstring; Kinds: TRttiKinds = []): TRttiCustom; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// register a given RTTI TypeInfo()
    // - returns a new (or existing if it was already registered) TRttiCustom
    // - will use GlobalClass to instantiate a new TRttiCustom
    // - if Info.Kind is rkDynArray, it will also register the nested rkRecord
    function RegisterType(Info: PRttiInfo): TRttiCustom;
    /// register one or several RTTI TypeInfo()
    // - to ensure that those types will be recognized by text definition
    // - will just call RegisterType() for each Info[]
    procedure RegisterTypes(const Info: array of PRttiInfo);
    /// register a given class type, using its RTTI
    // - returns a new (or existing if it was already registered) TRttiCustom
    function RegisterClass(ObjectClass: TClass): TRttiCustom;
    /// register one or several RTTI TypeInfo()
    // - to ensure that those types will be recognized by text definition
    // - will just call RegisterType() for each Info[]
    procedure RegisterClasses(const ObjectClass: array of TClass);
    /// register one RTTI TypeInfo() to be serialized as hexadecimal
    // - data will be serialized as BinToHexDisplayLower() JSON hexadecimal
    // string, using BinarySize bytes of the value, i.e. BinarySize*2 hexa chars
    // - you can truncate the original data size (e.g. if all bits of an integer
    // are not used) by specifying the aFieldSize optional parameter
    // - will also ensure that those types will be recognized by text definition
    // - leave BinarySize=0 to write all bytes as hexadecimal
    // - set BinarySize=-1 to unregister the binary serialization for the type
    function RegisterBinaryType(Info: PRttiInfo; BinarySize: integer = 0): TRttiCustom;
    /// register one or several RTTI TypeInfo() to be serialized as hexadecimal
    // - TypeInfo() and associated size information will here be defined by pairs:
    // ([TypeInfo(TType1),TYPE1_BYTES,TypeInfo(TType2),TYPE2_BYTES])
    // - a wrapper around the RegisterBinaryType() method
    procedure RegisterBinaryTypes(const InfoBinarySize: array of const);
    /// register one dynamic array RTTI TypeInfo() to be serialized as T*ObjArray
    // - will allow JSON serialization and unserialization of the registered
    // dynamic array property defined in any TPersistent or TSQLRecord
    // - could be used as such (note the T*ObjArray type naming convention):
    // ! TUserObjArray = array of TUser;
    // ! ...
    // ! RttiCustom.RegisterObjArray(TypeInfo(TUserObjArray), TUser);
    // - then you can use ObjArrayAdd/ObjArrayFind/ObjArrayDelete to manage
    // the stored items, and never forget to call ObjArrayClear to release
    // the memory
    // - set Item=nil to unregister the type as a T*ObjArray
    // - may return nil if DynArray is not a rkDynArray
    function RegisterObjArray(DynArray: PRttiInfo; Item: TClass): TRttiCustom;
    /// register one or several dynamic array RTTI TypeInfo() to be serialized
    // as T*ObjArray
    // - will call the RegisterObjArray() class method by pair:
    // ! RttiCustom.RegisterObjArray([
    // !  TypeInfo(TAddressObjArray),TAddress, TypeInfo(TUserObjArray),TUser]);
    procedure RegisterObjArrays(const DynArrayItem: array of const);
    /// default property to access a given RTTI TypeInfo() customization
    // - you can access or register one type by using this default property:
    // ! RttiCustom[TypeInfo(TMyClass)].Props.NameChange('old', 'new')
    property ByTypeInfo[P: PRttiInfo]: TRttiCustom read RegisterType; default;
  end;


var
  /// low-level access to the list of registered PRttiInfo/TRttiCustom
  RttiCustom: TRttiCustomList;


implementation


{ some inlined definitions which should be declared before $include code }

/// convert an ordinal value into its (signed) pointer-sized integer representation
function FromRttiOrd(o: TRttiOrd; P: pointer): Int64;
  {$ifdef HASINLINE}inline;{$endif}
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
  {$ifdef HASINLINE}inline;{$endif}
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
  // wrapper to retrieve IInvokable Interface RTTI via GetRttiInterface()
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
    procedure RaiseError(const Format: RawUTF8; const Args: array of const);
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

function TRttiClass.UnitName: ShortString;
begin
  result := PTypeData(@self)^.UnitName;
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
    with P^.RttiClass^ do
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

function TRttiProp.Next: PRttiProp;
begin // this abtract code compiles into 2 asm lines under FPC :)
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

function TRttiEnumType.SizeInStorageAsEnum: Integer;
begin
  if @self = nil then
    result := 0
  else
    result := ORDTYPE_SIZE[RttiOrd]; // MaxValue does not work e.g. with WordBool
end;

function TRttiEnumType.SizeInStorageAsSet: Integer;
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

procedure TRttiEnumType.GetEnumNameAll(var result: TRawUTF8DynArray;
  TrimLeftLowerCase: boolean);
var
  max, i: integer;
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
      result[i] := RawUTF8(V^);
    inc(PByte(V), length(V^) + 1);
  end;
end;

procedure TRttiEnumType.GetEnumNameAll(var result: RawUTF8; const Prefix: RawUTF8;
  quotedValues: boolean; const Suffix: RawUTF8; trimedValues, unCamelCased: boolean);
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
      Add(',');
      inc(PByte(V), length(V^) + 1);
    end;
    CancelLastComma;
    AddString(Suffix);
    SetText(result);
  finally
    Free;
  end;
end;

procedure TRttiEnumType.GetEnumNameTrimedAll(var result: RawUTF8; const Prefix: RawUTF8;
  quotedValues: boolean; const Suffix: RawUTF8);
begin
  GetEnumNameAll(result, Prefix, quotedValues, Suffix, {trimed=}true);
end;

function TRttiEnumType.GetEnumNameAllAsJSONArray(TrimLeftLowerCase: boolean;
  UnCamelCased: boolean): RawUTF8;
begin
  GetEnumNameAll(result, '[', {quoted=}true, ']', TrimLeftLowerCase, UnCamelCased);
end;

function TRttiEnumType.GetEnumNameValue(const EnumName: ShortString): Integer;
begin
  result := GetEnumNameValue(@EnumName[1], ord(EnumName[0]));
end;

function TRttiEnumType.GetEnumNameValue(Value: PUTF8Char): Integer;
begin
  result := GetEnumNameValue(Value, StrLen(Value));
end;

function TRttiEnumType.GetEnumNameValue(Value: PUTF8Char; ValueLen: integer;
  AlsoTrimLowerCase: boolean): Integer;
begin
  if (Value <> nil) and (ValueLen > 0) then
  begin
    result := FindShortStringListExact(NameList, MaxValue, Value, ValueLen);
    if (result < 0) and AlsoTrimLowerCase then
      result := FindShortStringListTrimLowerCase(NameList, MaxValue, Value, ValueLen);
  end
  else
    result := -1;
end;

function TRttiEnumType.GetEnumNameValueTrimmed(Value: PUTF8Char; ValueLen: integer;
  ExactCase: boolean): Integer;
begin
  if (Value <> nil) and (ValueLen > 0) then
    if ExactCase then
      result := FindShortStringListTrimLowerCaseExact(NameList, MaxValue, Value, ValueLen)
    else
      result := FindShortStringListTrimLowerCase(NameList, MaxValue, Value, ValueLen)
  else
    result := -1;
end;

function TRttiEnumType.GetEnumNameTrimed(const Value): RawUTF8;
begin
  result := TrimLeftLowerCaseShort(GetEnumName(Value));
end;

procedure TRttiEnumType.GetSetNameCSV(W: TBaseWriter; Value: cardinal;
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

function TRttiEnumType.GetSetNameCSV(Value: cardinal; SepChar: AnsiChar;
  FullSetsAsStar: boolean): RawUTF8;
var
  W: TBaseWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TBaseWriter.CreateOwnedStream(temp);
  try
    GetSetNameCSV(W, Value, SepChar, FullSetsAsStar);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TRttiEnumType.GetEnumNameTrimedValue(const EnumName: ShortString): Integer;
begin
  result := GetEnumNameTrimedValue(@EnumName[1], ord(EnumName[0]));
end;

function TRttiEnumType.GetEnumNameTrimedValue(Value: PUTF8Char; ValueLen: integer): Integer;
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

function TRttiEnumType.GetOrdinalFromEnum(Value: pointer): PtrUInt;
begin
  result := 0;
  ToRttiOrd(RttiOrd, @result, PPtrInt(Value)^); // no need to handle sign
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

function TRttiInfo.IsSynCurrency: boolean;
begin
  {$ifndef CPUX86}
  result := @self = TypeInfo(TSynCurrency);
  {$else}
  result := TRttiFloat(GetTypeData(@self)^.FloatType) = rfCurr;
  {$endif CPUX86}
end;

function TRttiInfo.RttiFloat: TRttiFloat;
begin
  {$ifndef CPUX86}
  if @self = TypeInfo(TSynCurrency) then
    result := rfCurr // RTTI identifies TSynCurrency as rfDouble
  else
  {$endif CPUX86}
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
      result := ArrayItemSize;
    rkRecord {$ifdef FPC}, rkObject {$endif} :
      result := RecordSize;
    rkSString:
      result := GetTypeData(@self)^.MaxLength + 1;
  else
    result := 0;
  end;
end;

function TRttiInfo.ClassFieldCount(onlyWithoutGetter: boolean): integer;
begin
  result := ClassFieldCountWithParents(RttiClass^.RttiClass, onlyWithoutGetter);
end;

function TRttiInfo.InheritsFrom(AClass: TClass): boolean;
begin
  result := RttiClass^.InheritsFrom(AClass);
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
  if (@self = nil) or (Kind <> rkSet) then
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


const
  BYTETOBITS: array[1..4] of cardinal = (
    $ff, $ffff, $ffffff, $ffffffff);

procedure TRttiInfo.ComputeCache(out Cache: TRttiCache);
var
  enum: PRttiEnumType;
begin
  Cache.Info := @self;
  Cache.Size := RttiSize;
  case Kind of
    rkEnumeration:
      Cache.EnumInfo := Cache.Info.EnumBaseType;
    rkSet:
    begin
      enum := Cache.Info.SetEnumType;
      Cache.EnumInfo := enum;
      Cache.EnumMax := enum.MaxValue;
      Cache.EnumMask := BYTETOBITS[Cache.Size];
    end;
    rkDynArray:
      Cache.ItemInfo := DynArrayItemType(Cache.ItemSize);
    rkArray:
      Cache.ItemInfo := ArrayItemType(Cache.ItemCount, Cache.ItemSize);
  end;
end;

function TRttiInfo.InterfaceType: PRttiInterfaceTypeData;
begin
  result := pointer(GetTypeData(@self));
end;

function TRttiInfo.AnsiStringCodePage: integer;
begin
  if @self = TypeInfo(TSQLRawBlob) then
    result := CP_SQLRAWBLOB
  else
  {$ifdef HASCODEPAGE}
  if Kind = rkLString then // has rkLStringOld any codepage? -> UTF-8
    result := GetTypeData(@self)^.CodePage
  else
    result := CP_UTF8; // default is UTF-8
  {$else}
  if @self = TypeInfo(RawUTF8) then
    result := CP_UTF8
  else if @self = TypeInfo(WinAnsiString) then
    result := CODEPAGE_US
  else if @self = TypeInfo(RawUnicode) then
    result := CP_UTF16
  else if @self = TypeInfo(RawByteString) then
    result := CP_RAWBYTESTRING
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

function TRttiInfo.InterfaceGUID: PGUID;
begin
  if (@self = nil) or (Kind <> rkInterface) then
    result := nil
  else
    result := InterfaceType^.IntfGuid;
end;

function TRttiInfo.InterfaceUnitName: PShortString;
begin
  if (@self = nil) or (Kind <> rkInterface) then
    result := @NULCHAR
  else
    result := InterfaceType^.IntfUnit;
end;

function TRttiInfo.InterfaceAncestor: PRttiInfo;
begin
  if (@self = nil) or (Kind <> rkInterface) then
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
  if (@self = nil) or (Kind <> rkInterface) then
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

function TRttiProp.Index: Integer;
begin
  result := PPropInfo(@self)^.Index;
end;

function TRttiProp.Default: Longint;
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
    else  // field - Setter is the field offset in the instance data
      result := SetterAddr(Instance)
  else    // field - Getter is the field offset in the instance data
    result := GetterAddr(Instance);
end;

function TRttiProp.DefaultOr0: integer;
begin
  result := PPropInfo(@self)^.Default;
  if result = NO_DEFAULT then
    result := 0;
end;

function TRttiProp.IsDefaultOrVoid(Instance: TObject): boolean;
var
  p: PPointer;
begin
  case TypeInfo^.Kind of
    rkInteger, rkEnumeration, rkSet, rkChar, rkWChar
    {$ifdef FPC}, rkBool {$endif}:
      result := GetOrdProp(Instance) = DefaultOr0;
    rkFloat:
      result := GetFloatProp(Instance) = 0;
    rkInt64 {$ifdef FPC}, rkQWord {$endif}:
      result := GetInt64Prop(Instance) = 0;
    rkLString, {$ifdef FPC} rkLStringOld, {$endif}
    {$ifdef HASVARUSTRING} rkUString, {$endif}
    rkWString, rkDynArray, rkClass, rkInterface:
      begin
        p := GetFieldAddr(Instance);
        result := (p <> nil) and (p^ = nil);
      end;
    rkVariant:
      begin
        p := GetFieldAddr(Instance);
        result := (p <> nil) and VarDataIsEmptyOrNull(p^);
      end;
  else
    result := false;
  end;
end;

var
  // conversion from TRttiKind to TRttiVarData.VType
  RTTI_TO_VARTYPE: array[TRttiKind] of cardinal;

procedure TRttiProp.GetValue(Instance: TObject; out Value: TRttiVarData);
label
  clr;
begin
  if Instance = nil then
    goto clr;
  Value.Info := TypeInfo;
  Value.VType := RTTI_TO_VARTYPE[Value.Info^.Kind];
  case Value.VType of
  varEmpty:
    // unsupported TRttiKind
    exit;
  varInt64 {$ifdef FPC} , varBoolean {$endif}:
    // rkInteger, rkBool
    Value.VInt64 := GetOrdProp(Instance); // VInt64 for cardinal
  varWord64:
    // rkInt64, rkQWord
    begin
      if not Value.Info^.IsQWord then
        Value.VType := varInt64;
      Value.VInt64 := GetInt64Prop(Instance);
    end;
  varDouble:
    // rkFloat
    if Value.Info^.IsSynCurrency then
    begin
      Value.VType := varCurrency;
      GetCurrencyProp(Instance, PSynCurrency(@Value.VInt64)^);
    end
    else
      Value.Data.VDouble := GetFloatProp(Instance);
  varAny:
    // rkEnumeration, rkSet, rkDynArray, rkClass, rkInterface, rkRecord, rkObject
    {$ifndef FPC}
    if Value.Info^.IsBoolean then
    begin
      Value.VType := varBoolean;
      Value.VInt64 := GetOrdProp(Instance);
    end
    else
    {$endif FPC}
    begin
      // VAny will be properly handled by TTextWriter.AddVariant/AddTypedJSON
      Value.Data.VAny := GetFieldAddr(Instance);
      if Value.Data.VAny = nil then
clr:    Value.VType := varNull;
    end;
  varUnknown:
    // rkChar, rkWChar, rkSString converted into temporary RawUTF8
    begin
      Value.VType := varString;
      Value.Data.VAny := nil; // avoid GPF
      GetAsString(Instance, RawUTF8(Value.Data.VAny));
      Value.NeedsClear := true; // we allocated a value
    end
  else
    // varString or varVariant which could be returned by reference
    begin
      Value.Data.VAny := GetFieldAddr(Instance);
      if Value.Data.VAny <> nil then
        // return field address as varByRef - TTextWriter.AddVariant compatible
        Value.VType := Value.VType or varByRef
      else
      begin
        // we need to call a getter method - VAny already nil
        case Value.Info^.Kind of
          rkLString:
            GetLongStrProp(Instance, RawByteString(Value.Data.VAny));
          rkWString:
            GetWideStrProp(Instance, WideString(Value.Data.VAny));
          {$ifdef HASVARUSTRING}
          rkUString:
            GetUnicodeStrProp(Instance, UnicodeString(Value.Data.VAny));
          {$endif HASVARUSTRING}
          rkVariant:
          begin
            Value.VType := varEmpty; // to fill as PVariant()
            GetVariantProp(Instance, PVariant(@Value)^)
          end;
        end;
        Value.NeedsClear := true; // we allocated a value
      end;
    end;
  end;
end;

function TRttiProp.GetOrdProp(Instance: TObject): Int64;
type
  TGetProc = function: Pointer of object; // pointer result is a PtrInt register
  TGetIndexed = function(Index: Integer): Pointer of object;
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
    if Kind in [rkClass, rkDynArray, rkInterface] then
      result := PtrInt(call.Code)
    else
      result := FromRttiOrd(RttiOrd, @call.Code);
end;

procedure TRttiProp.SetOrdProp(Instance: TObject; Value: PtrInt);
type
  TSetProc = procedure(Value: PtrInt) of object;
  TSetIndexed = procedure(Index: integer; Value: PtrInt) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      with TypeInfo^ do
        if Kind = rkClass then
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
  TGetIndexed = function(Index: Integer): TObject of object;
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
  TGetIndexed = function(Index: Integer): Int64 of object;
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
      TGetIndexed = function(Index: Integer): RawByteString of object;
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

procedure TRttiProp.GetShortStrProp(Instance: TObject; var Value: RawUTF8);
type
  TGetProc = function: ShortString of object;
  TGetIndexed = function(Index: Integer): ShortString of object;
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
  TGetIndexed = function(Index: Integer): WideString of object;
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
      TGetIndexed = function(Index: Integer): UnicodeString of object;
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

procedure TRttiProp.GetCurrencyProp(Instance: TObject; var Value: TSynCurrency);
type
  TGetProc = function: TSynCurrency of object;
  TGetIndexed = function(Index: Integer): TSynCurrency of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      Value := PSynCurrency({%H-}call.Data)^;
    rpcMethod:
      Value := TGetProc(call);
    rpcIndexed:
      Value := TGetIndexed(call)(Index);
  else
    PInt64(@Value)^ := 0;
  end;
end;

procedure TRttiProp.SetCurrencyProp(Instance: TObject; const Value: TSynCurrency);
type
  TSetProc = procedure(const Value: TSynCurrency) of object;
  TSetIndexed = procedure(Index: integer; const Value: TSynCurrency) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PSynCurrency({%H-}call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

function TRttiProp.GetDoubleProp(Instance: TObject): double;
type
  TGetProc = function: double of object;
  TGetIndexed = function(Index: Integer): double of object;
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
  TSingleIndexed = function(Index: Integer): Single of object;
  TDoubleProc = function: Double of object;
  TDoubleIndexed = function(Index: Integer): Double of object;
  TExtendedProc = function: Extended of object;
  TExtendedIndexed = function(Index: Integer): Extended of object;
  // warning: TSynCurrency getters are very likely to fail
  TCurrencyProc = function: TSynCurrency of object;
  TCurrencyIndexed = function(Index: Integer): TSynCurrency of object;
var
  call: TMethod;
  rf: TRttiFloat;
begin
  result := 0;
  rf := TypeInfo^.RttiFloat; // detect TSynCurrency as rfCurr
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
          CurrencyToDouble(PSynCurrency(call.Data), result);
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
  // warning: TSynCurrency setters are very likely to fail
  TCurrencyProc = procedure(const Value: TSynCurrency) of object;
  TCurrencyIndexed = procedure(Index: integer; const Value: TSynCurrency) of object;
var
  call: TMethod;
  rf: TRttiFloat;
begin
  Value := 0;
  rf := TypeInfo^.RttiFloat; // detect TSynCurrency as rfCurr
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
          DoubleToCurrency(Value, PSynCurrency(call.Data));
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

procedure TRttiProp.GetVariantProp(Instance: TObject; var result: Variant);
var
  rpc: TRttiPropCall;
  call: TMethod;

  procedure SubProc(rpc: TRttiPropCall); // avoid try..finally
  type
    TGetProc = function: variant of object;
    TGetIndexed = function(Index: Integer): variant of object;
  begin
    case rpc of
      rpcMethod:
        result := TGetProc(call);
      rpcIndexed:
        result := TGetIndexed(call)(Index);
    else
      SetVariantNull(result);
    end;
  end;

begin
  rpc := Getter(Instance, @call);
  if rpc <> rpcField then
    SubProc(rpc)
  else if not SetVariantUnRefSimpleValue(PVariant(call.Data)^, PVarData(@result)^) then
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
  if (Instance <> nil) and (@self <> nil) and (TypeInfo^.Kind in
      [rkInteger, rkEnumeration, rkSet, {$ifdef FPC}rkBool, {$endif} rkClass]) then
    result := GetOrdProp(Instance)
  else
    result := -1;
end;

function TRttiProp.GetInt64Value(Instance: TObject): Int64;
begin
  if (Instance <> nil) and (@self <> nil) then
    case TypeInfo^.Kind of
      rkInteger, rkEnumeration, rkSet, rkChar, rkWChar,
      rkClass {$ifdef FPC}, rkBool{$endif}:
        result := GetOrdProp(Instance);
      rkInt64{$ifdef FPC}, rkQWord{$endif}:
        result := GetInt64Prop(Instance);
    else
      result := 0;
    end
  else
    result := 0;
end;

procedure TRttiProp.GetCurrencyValue(Instance: TObject; var Value: TSynCurrency);
begin
  if (Instance <> nil) and (@self <> nil) then
    with TypeInfo^ do
      if Kind = rkFloat then
        if RttiFloat = rfCurr then // RttiFloat detect TSynCurrency as rfCurr
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
  if (Instance <> nil) and (@self <> nil) and (TypeInfo^.Kind = rkFloat) then
    result := GetFloatProp(Instance)
  else
    result := 0;
end;

procedure TRttiProp.SetDoubleValue(Instance: TObject; const Value: double);
begin
  if (Instance <> nil) and (@self <> nil) and (TypeInfo^.Kind = rkFloat) then
    SetFloatProp(Instance, Value);
end;

procedure TRttiProp.GetRawByteStringValue(Instance: TObject; var Value: RawByteString);
begin
  if (Instance <> nil) and (@self <> nil) and
     (TypeInfo^.Kind in [{$ifdef FPC}rkLStringOld, {$endif} rkLString]) then
    GetLongStrProp(Instance, Value)
  else
    Finalize(Value);
end;

procedure TRttiProp.SetOrdValue(Instance: TObject; Value: PtrInt);
begin
  if (Instance <> nil) and (@self <> nil) and (TypeInfo^.Kind in
      [rkInteger, rkEnumeration, rkSet, {$ifdef FPC}rkBool, {$endif}rkClass]) then
    SetOrdProp(Instance, Value);
end;

procedure TRttiProp.SetInt64Value(Instance: TObject; Value: Int64);
begin
  if (Instance <> nil) and (@self <> nil) then
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
  if (Instance <> nil) and (@self <> nil) and (TypeInfo^.Kind = rkUString) then
    GetUnicodeStrProp(Instance, result)
  else
    result := '';
end;

procedure TRttiProp.SetUnicodeStrValue(Instance: TObject; const Value: UnicodeString);
begin
  if (Instance <> nil) and (@self <> nil) and (TypeInfo^.Kind = rkUString) then
    SetUnicodeStrProp(Instance, Value);
end;

{$endif HASVARUSTRING}

procedure TRttiProp.GetAsString(Instance: TObject; var Value: RawUTF8);
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

function ToText(k: TRttiKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TRttiKind), ord(k));
end;

function ToText(t: TRTTIParserType): PShortString;
begin
  result := GetEnumName(TypeInfo(TRTTIParserType), ord(t));
end;


{ **************** Published Class Properties and Methods RTTI }

function ClassFieldCountWithParents(C: TClass; onlyWithoutGetter: boolean): integer;
var CP: PRttiProps;
    P: PRttiProp;
    i: integer;
begin
  result := 0;
  while C <> nil do
  begin
    CP := GetRttiProps(C);
    if CP = nil then
      break; // no RTTI information (e.g. reached TObject level)
    if onlyWithoutGetter then
    begin
      P := CP^.PropList;
      for i := 1 to CP^.PropCount do
      begin
        if P^.GetterIsField then
          inc(result);
        P := P^.Next;
      end;
    end
    else
      inc(result,CP^.PropCount);
    C := GetClassParent(C);
  end;
end;



{ *************** Enumerations RTTI }

procedure GetEnumNames(aTypeInfo: pointer; aDest: PPShortString);
var
  info: PRttiEnumType;
  p: PShortString;
  i: PtrInt;
begin
  info := PRttiInfo(aTypeInfo)^.EnumBaseType;
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

procedure GetEnumTrimmedNames(aTypeInfo: pointer; aDest: PRawUTF8);
var
  info: PRttiEnumType;
  p: PShortString;
  i: PtrInt;
begin
  info := PRttiInfo(aTypeInfo)^.EnumBaseType;
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

function GetEnumTrimmedNames(aTypeInfo: pointer): TRawUTF8DynArray;
begin
  PRttiInfo(aTypeInfo)^.EnumBaseType^.GetEnumNameAll(result, {trim=}true);
end;

function GetEnumNameValue(aTypeInfo: pointer; aValue: PUTF8Char;
  aValueLen: PtrInt; AlsoTrimLowerCase: boolean): Integer;
begin
  result := PRttiInfo(aTypeInfo)^.EnumBaseType^.
    GetEnumNameValue(aValue, aValueLen, AlsoTrimLowerCase);
end;

function GetEnumNameValueTrimmed(aTypeInfo: pointer; aValue: PUTF8Char;
  aValueLen: PtrInt): integer;
begin
  result := PRttiInfo(aTypeInfo)^.EnumBaseType^.
    GetEnumNameValueTrimmed(aValue, aValueLen, {exact=}false);
end;

function GetEnumNameValueTrimmedExact(aTypeInfo: pointer; aValue: PUTF8Char;
  aValueLen: PtrInt): integer;
begin
  result := PRttiInfo(aTypeInfo)^.EnumBaseType^.
    GetEnumNameValueTrimmed(aValue, aValueLen, {exact=}true);
end;

function GetEnumNameValue(aTypeInfo: pointer; const aValue: RawUTF8;
  AlsoTrimLowerCase: boolean): Integer;
begin
  result := PRttiInfo(aTypeInfo)^.EnumBaseType^.
    GetEnumNameValue(pointer(aValue), length(aValue), AlsoTrimLowerCase);
end;

function GetSetName(aTypeInfo: pointer; const value): RawUTF8;
var
  info: PRttiEnumType;
  PS: PShortString;
  i: PtrInt;
begin
  result := '';
  info := PRttiInfo(aTypeInfo)^.SetEnumType;
  if info <> nil then
  begin
    PS := info^.NameList;
    for i := 0 to info^.MaxValue do
    begin
      if GetBitPtr(@value, i) then
        result := FormatUTF8('%%,', [result, PS^]);
      inc(PByte(PS), PByte(PS)^ + 1); // next
    end;
    if result <> '' then
      SetLength(result, length(result) - 1); // trim last comma
  end;
end;

procedure GetSetNameShort(aTypeInfo: pointer; const value; out result: ShortString;
  trimlowercase: boolean);
var
  info: PRttiEnumType;
  PS: PShortString;
  i: PtrInt;
begin
  result := '';
  info := PRttiInfo(aTypeInfo)^.SetEnumType;
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

const
  PSEUDO_RESULT_NAME: string[6] = 'Result';
  PSEUDO_SELF_NAME:   string[4] = 'Self';

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
    a^.ParamName := @PSEUDO_RESULT_NAME
  else
    a^.ParamName := aParamName;
  a^.TypeInfo := aInfo;
  if aTypeName = nil then
    aTypeName := aInfo^.Name;
  a^.TypeName := aTypeName;
  if ArgCount > 1 then
    if aInfo^.Kind in (rkRecordTypes + [rkDynArray])  then
    begin
      if aFlags * [pfConst, pfVar, pfOut] = [] then
        RaiseError('%: % parameter should be declared as const, var or out',
          [aParamName^, aTypeName^]);
    end else if aInfo^.Kind = rkInterface then
      if not (pfConst in aFlags) then
        RaiseError('%: % parameter should be declared as const',
          [aParamName^, aTypeName^]);
  if aParamName = nil then
    a^.Direction := rmdResult
  else if pfVar in aFlags then
    a^.Direction := rmdVar
  else if pfOut in aFlags then
    a^.Direction := rmdOut;
end;

procedure TGetRttiInterface.RaiseError(const Format: RawUTF8; const Args: array of const);
var
  m: RawUTF8;
begin
  if CurrentMethod <> nil then
    m := '.' + CurrentMethod^.Name;
  raise ERttiException.CreateUTF8('GetRttiInterface(%%) failed - %',
    [Definition.Name, {%H-}m, FormatUTF8(Format, Args)]);
end;

function GetRttiInterface(aTypeInfo: pointer; out aDefinition: TRttiInterface): integer;
var
  getter: TGetRttiInterface;
begin
  getter := TGetRttiInterface.Create;
  try
    getter.AddMethodsFromTypeInfo(aTypeInfo);
    aDefinition := getter.Definition;
  finally
    getter.Free;
  end;
  result := length(aDefinition.Methods);
end;


{ ************* Efficient Dynamic Arrays and Records Process }

procedure VariantDynArrayClear(var Value: TVariantDynArray);
begin
  FastDynArrayClear(@Value, TypeInfo(variant));
end;

procedure RawUTF8DynArrayClear(var Value: TRawUTF8DynArray);
begin
  FastDynArrayClear(@Value, TypeInfo(RawUTF8));
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
      if (p^.refCnt >= 0) and RefCntDecFree(p^.refCnt) then
        Freemem(p); // works for both rkLString + rkUString
    end;
    inc(v);
    dec(n);
  until n = 0;
end;

procedure FastFinalizeArray(Value: PPointer; ElemTypeInfo: PRttiInfo;
  Count: integer);
var
  fin: TRttiFinalizer;
begin //  caller ensured ElemTypeInfo<>nil and Count>0
  case ElemTypeInfo^.Kind of
    rkRecord {$ifdef FPC} , rkObject {$endif}:
      // retrieve ElemTypeInfo.RecordManagedFields once
      _RecordClearSeveral(pointer(Value), ElemTypeInfo, Count);
    {$ifdef HASVARUSTRING} rkUString, {$endif} {$ifdef FPC} rkLStringOld, {$endif}
    rkLString:
      // optimized loop for AnsiString / UnicodeString (PStrRec header)
      _StringClearSeveral(pointer(Value), Count);
    rkVariant:
      // from mormot.core.variants - supporting custom variants
      // or at least from mormot.core.base
      VariantClearSeveral(pointer(Value), Count);
    else
      // regular finalization
      begin
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
      if (p^.refCnt >= 0) and RefCntDecFree(p^.refCnt) then
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
begin // caller ensured Info is indeed a record/object
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

procedure _RecordCopySeveral(Dest, Source: PAnsiChar; n: PtrInt; Info: PRttiInfo);
var
  fields: TRttiRecordManagedFields;
  f: PRttiRecordField;
  p: PRttiInfo;
  i: PtrInt;
  cop: PRttiCopiers;
begin
  Info^.RecordManagedFields(fields); // retrieve RTTI once for all items
  if fields.Count > 0 then
  begin
    cop := @RTTI_COPY;
    repeat
      f := fields.Fields;
      i := fields.Count;
      repeat
        p := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
        {$ifdef FPC_OLDRTTI}
        if Assigned(cop[p^.Kind]) then
        {$endif FPC_OLDRTTI}
          cop[p^.Kind](Dest + f^.Offset, Source + f^.Offset, p);
        inc(f);
        dec(i);
      until i = 0;
      inc(Source, fields.Size);
      inc(Dest, fields.Size);
      dec(n);
    until n = 0;
  end;
end;

procedure CopySeveral(Dest, Source: PByte; SourceCount: PtrInt;
  ItemInfo: PRttiInfo; ItemSize: PtrInt);
var
  cop: TRttiCopier;
  elemsize: PtrInt;
begin
  if SourceCount > 0 then
    if ItemInfo = nil then
      MoveFast(Source^, Dest^, ItemSize * SourceCount)
    else
    if ItemInfo^.Kind in rkRecordTypes then
      _RecordCopySeveral(pointer(Dest), pointer(Source), SourceCount, ItemInfo)
    else
    begin
      cop := RTTI_COPY[ItemInfo^.Kind];
      if Assigned(cop) then
        repeat
          elemsize := cop(Dest, Source, ItemInfo);
          inc(Source, elemsize);
          inc(Dest, elemsize);
          dec(SourceCount);
        until SourceCount = 0;
    end;
end;

procedure DynArrayCopy(Dest, Source: PByte; Info: PRttiInfo;
  SourceExtCount: PInteger);
var
  n, itemsize: PtrInt;
  iteminfo: PRttiInfo;
begin
  iteminfo := Info^.DynArrayItemType(itemsize);
  if PPointer(Dest)^ <> nil then
    FastDynArrayClear(pointer(Dest), iteminfo);
  if PPointer(Source)^ <> nil then
  begin
    if SourceExtCount <> nil then
      n := SourceExtCount^
    else
      n := PDynArrayRec(PAnsiChar(Source) - SizeOf(TDynArrayRec))^.length;
    DynArraySetLength(pointer(Dest), Info, 1, @n); // allocate memory
    CopySeveral(PPointer(Dest)^, PPointer(Source)^, n, iteminfo, itemsize);
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
     ((p^.refCnt > 1) and not RefCntDecFree(p^.refCnt)) then
  begin
    n := p^.length;
    DynArraySetLength(pointer(Value), Info, 1, @n);
    Info := Info^.DynArrayItemType(elemsize);
    inc(p);
    CopySeveral(pointer(p), Value^, n, Info, elemsize);
  end;
end;


{ ************* Managed Types Finalization or Copy }


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
    if (p^.refCnt >= 0) and RefCntDecFree(p^.refCnt) then
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
    {$endif}
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
    {$endif}
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
    if (p^.refCnt >= 0) and RefCntDecFree(p^.refCnt) then
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
  Info^.ArrayItemType(n, result);
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


{ RTTI_COPY[] implementation functions }

function _LStringCopy(Dest, Source: PRawByteString; Info: PRttiInfo): PtrInt;
begin
  if (Dest^ <> '') or (Source^ <> '') then
    Dest^ := Source^;
  result := SizeOf(Source^);
end;

{$ifdef HASVARUSTRING}
function _UStringCopy(Dest, Source: PUnicodeString; Info: PRttiInfo): PtrInt;
begin
  if (Dest^ <> '') or (Source^ <> '') then
    Dest^ := Source^;
  result := SizeOf(Source^);
end;
{$endif HASVARUSTRING}

function _WStringCopy(Dest, Source: PWideString; Info: PRttiInfo): PtrInt;
begin
  if (Dest^ <> '') or (Source^ <> '') then
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
    if vt <= varWord64 then
      if (vt < varOleStr) or (vt > varError) then
raw:    Dest^.VInt64 := Source^.VInt64 // will copy any simple value
      else if vt = varOleStr then
      begin
        Dest^.VAny := nil;
        WideString(Dest^.VAny) := WideString(Source^.VAny)
      end
      else
        goto rtl // varError, varDispatch
    else if vt = varString then
    begin
      Dest^.VAny := nil;
      RawByteString(Dest^.VAny) := RawByteString(Source^.VAny)
    end
    else if vt >= varByRef then
      goto raw // varByRef has no refcount
    {$ifdef HASVARUSTRING}
    else if vt = varUString then
    begin
      Dest^.VAny := nil;
      UnicodeString(Dest^.VAny) := UnicodeString(Source^.VAny)
    end
    {$endif HASVARUSTRING}
    else
rtl:  VarCopyProc(Dest^, Source^); // will handle any complex type
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
  offset, itemsize: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  fields.Fields := @RTTI_COPY; // reuse pointer slot on stack
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
      itemsize := PRttiCopiers(fields.Fields)[Info^.Kind](Dest, Source, Info);
      inc(Source, itemsize);
      inc(Dest, itemsize);
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset > 0 then
    MoveFast(Source^, Dest^, offset);
  result := fields.Size;
end;

function _DynArrayCopy(Dest, Source: PByte; Info: PRttiInfo): PtrInt;
begin
  DynArrayCopy(Dest, Source, Info, {extcount=}nil);
  result := SizeOf(pointer);
end;

function _ArrayCopy(Dest, Source: PByte; Info: PRttiInfo): PtrInt;
var
  n, itemsize: PtrInt;
  cop: TRttiCopier;
begin
  Info^.ArrayItemType(n, result);
  if Info = nil then
    MoveFast(Source^, Dest^, result)
  else
  begin
    cop := RTTI_COPY[Info^.Kind];
    if Assigned(cop) then
      repeat
        itemsize := cop(Dest ,Source, Info);
        inc(Source, itemsize);
        inc(Dest, itemsize);
        dec(n);
      until n = 0;
  end;
end;


{ ************** RTTI Value Types used for JSON Parsing }

function TypeNameToParserType(Name: PUTF8Char; NameLen: integer;
  ItemTypeName: PRawUTF8; Complex: PRTTIParserComplexType): TRTTIParserType;
const
  SORTEDMAX = 42;
  // fast branchless O(log(N)) binary search on x86_64
  SORTEDNAMES: array[0..SORTEDMAX] of PUTF8Char = (
    'ARRAY', 'BOOLEAN', 'BYTE', 'CARDINAL', 'CURRENCY', 'DOUBLE', 'EXTENDED',
    'INT64', 'INTEGER', 'INTERFACE', 'LONGINT', 'LONGWORD', 'PTRINT', 'PTRUINT', 'QWORD',
    'RAWBYTESTRING', 'RAWJSON', 'RAWUTF8', 'RECORD', 'SINGLE', 'STRING', 'SYNUNICODE',
    'TCREATETIME', 'TDATETIME', 'TDATETIMEMS', 'TGUID', 'THASH128', 'THASH256',
    'THASH512', 'TID', 'TMODTIME', 'TRECORDREFERENCE', 'TRECORDREFERENCETOBEDELETED',
    'TRECORDVERSION', 'TSQLRAWBLOB', 'TTIMELOG', 'TUNIXMSTIME', 'TUNIXTIME',
    'UNICODESTRING', 'UTF8STRING', 'VARIANT', 'WIDESTRING', 'WORD');
  // warning: recognized types should match at binary storage level!
  SORTEDTYPES: array[0..SORTEDMAX] of TRTTIParserType = (
    ptArray, ptBoolean, ptByte, ptCardinal, ptCurrency, ptDouble, ptExtended,
    ptInt64, ptInteger, ptInterface, ptInteger, ptCardinal, ptPtrInt, ptPtrUInt, ptQWord,
    ptRawByteString, ptRawJSON, ptRawUTF8, ptRecord, ptSingle, ptString, ptSynUnicode,
    ptTimeLog, ptDateTime, ptDateTimeMS, ptGUID, ptHash128, ptHash256, ptHash512,
    ptORM, ptTimeLog, ptORM, ptORM, ptORM, ptRawByteString, ptUnixMSTime,
    ptUnixTime, ptTimeLog, ptUnicodeString,
    ptRawUTF8, ptVariant, ptWideString, ptWord);
  SORTEDCOMPLEX: array[0..SORTEDMAX] of TRTTIParserComplexType = (
    pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctNone, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctCreateTime, pctNone, pctNone, pctNone, pctNone, pctNone,
    pctNone, pctID, pctModTime, pctRecordReference, pctRecordReferenceToBeDeleted,
    pctRecordVersion, pctNone, pctTimeLog, pctNone, pctNone,
    pctNone, pctNone, pctNone, pctNone, pctNone);
var
  ndx: PtrInt;
  up: PUTF8Char;
  tmp: array[byte] of AnsiChar; // avoid unneeded memory allocation
begin
  if ItemTypeName <> nil then
  begin
    UpperCaseCopy(Name, NameLen, ItemTypeName^);
    up := pointer(ItemTypeName^);
  end
  else
  begin
    UpperCopy255Buf(@tmp, Name, NameLen);
    up := @tmp;
  end;
{  for ndx := 1 to SORTEDMAX do if StrComp(SORTEDNAMES[ndx],SORTEDNAMES[ndx-1])<=0
    then writeln(SORTEDNAMES[ndx]); }
  ndx := FastFindPUTF8CharSorted(@SORTEDNAMES, SORTEDMAX, up);
  if ndx >= 0 then
  begin
    if Complex <> nil then
      Complex^ := SORTEDCOMPLEX[ndx];
    result := SORTEDTYPES[ndx];
  end
  else if (Name[0] = 'T') and // T...ID pattern in name?
          (PWord(@Name[NameLen - 2])^ = ord('I') + ord('D') shl 8) then
  begin
    result := ptORM;
    if Complex <> nil then
      Complex^ := pctSpecificClassID;
  end
  else
    result := ptNone;
end;


function TypeNameToParserType(Name: PShortString; Complex: PRTTIParserComplexType): TRTTIParserType;
begin
  result := TypeNameToParserType(@Name^[1], ord(Name^[0]), nil, Complex);
end;

function TypeNameToParserType(const Name: RawUTF8;
  Complex: PRTTIParserComplexType): TRTTIParserType;
begin
  result := TypeNameToParserType(pointer(Name), length(Name), nil, Complex);
end;

function TypeInfoToParserType(Info: PRttiInfo; FirstSearchByName: boolean;
  Complex: PRTTIParserComplexType): TRTTIParserType;
begin
  result := ptNone;
  if Info = nil then
    exit;
  if FirstSearchByName then
  begin
    result := TypeNameToParserType(Info^.RawName, Complex);
    if result <> ptNone then
      if (result = ptORM) and (Info^.Kind <> rkInt64) then
        result := ptNone // paranoid check e.g. for T...ID false positives
      else
        exit;
  end;
  case Info^.Kind of // FPC and Delphi will use a fast jmp table
    {$ifdef FPC} rkLStringOld, {$endif} rkLString:
      result := ptRawUTF8;
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
      result := ptRecord;
    rkChar:
      result := ptByte;
    rkWChar:
      result := ptWord;
    rkMethod, rkInterface:
      result := ptPtrInt;
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
  {$ifdef HASINLINE} inline; {$endif}
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

function DynArrayTypeInfoToParserType(DynArrayInfo, ElemInfo: PRttiInfo;
  ElemSize: integer; ExactType: boolean; out FieldSize: integer;
  Complex: PRTTIParserComplexType): TRTTIParserType;
// warning: we can't use TRttiInfo.RecordAllFields since it would break
// backward compatibility and code expectations
var
  fields: TRttiRecordManagedFields;
  offset: integer;
begin
  result := ptNone;
  FieldSize := 0;
  case ElemSize of // very fast guess of most known ArrayType
    1:
      if DynArrayInfo = TypeInfo(TBooleanDynArray) then
        result := ptBoolean;
    4:
      if DynArrayInfo = TypeInfo(TCardinalDynArray) then
        result := ptCardinal
      else if DynArrayInfo = TypeInfo(TSingleDynArray) then
        result := ptSingle
    {$ifdef CPU64} ;
    8: {$else} else {$endif}
      if DynArrayInfo = TypeInfo(TRawUTF8DynArray) then
        result := ptRawUTF8
      else if DynArrayInfo = TypeInfo(TStringDynArray) then
        result := ptString
      else if DynArrayInfo = TypeInfo(TWinAnsiDynArray) then
        result := ptWinAnsi
      else if DynArrayInfo = TypeInfo(TRawByteStringDynArray) then
        result := ptRawByteString
      else if DynArrayInfo = TypeInfo(TSynUnicodeDynArray) then
        result := ptSynUnicode
      else if (DynArrayInfo = TypeInfo(TClassDynArray)) or
              (DynArrayInfo = TypeInfo(TPointerDynArray)) then
        result := ptPtrInt
      else
      {$ifdef CPU64}
      else {$else} ;
    8: {$endif}
      case ElemInfo^.Kind of
        rkFloat:
          if DynArrayInfo = TypeInfo(TDoubleDynArray) then
            result := ptDouble
          else if DynArrayInfo = TypeInfo(TCurrencyDynArray) then
            result := ptCurrency
          else if DynArrayInfo = TypeInfo(TDateTimeDynArray) then
            result := ptDateTime
          else if DynArrayInfo = TypeInfo(TDateTimeMSDynArray) then
            result := ptDateTimeMS;
        rkInt64:
          if DynArrayInfo = TypeInfo(TTimeLogDynArray) then
            result := ptTimeLog
          else if DynArrayInfo = TypeInfo(TUnixTimeDynArray) then
            result := ptUnixTime
          else if DynArrayInfo = TypeInfo(TUnixMSTimeDynArray) then
            result := ptUnixMSTime;
      end;
    {$ifdef TSYNEXTENDED80}
    10:
      if DynArrayInfo = TypeInfo(TSynExtendedDynArray) then
        result := ptExtended
    {$endif TSYNEXTENDED80}
  end;
  if result = ptNone then
    repeat // guess from RTTI of nested record(s)
      if ElemInfo = nil then
      begin
        result := SizeToDynArrayKind(ElemSize);
        if result = ptNone then
          FieldSize := ElemSize;
      end
      else // try to guess from 1st record/object field
      if not exactType and (ElemInfo^.Kind in rkRecordTypes) then
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
          if (ElemInfo = nil) or (ElemInfo^.Kind in rkRecordTypes) then
            continue; // nested records
          result := TypeInfoToParserType(ElemInfo, {fromname=}true, Complex);
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
    result := TypeInfoToParserType(ElemInfo, true, Complex);
  if PT_SIZE[result] <> 0 then
    FieldSize := PT_SIZE[result];
end;


{ ************** RTTI-based Registration for Custom JSON Parsing }

{ TRttiCustomProp }

function TRttiCustomProp.InitFrom(RttiProp: PRttiProp): PtrInt;
var
  addr: PtrInt;
begin
  Value := RttiCustom.RegisterType(RttiProp^.TypeInfo);
  if Value = nil then
    raise ERttiException.CreateUTF8('TRttiCustom: % property has no RTTI',
      [RttiProp^.Name^]);
  addr := PtrInt(RttiProp^.GetFieldAddr(nil));
  if RttiProp^.GetterIsField then
    OffsetGet := addr
  else
    OffsetGet := -1;
  if RttiProp^.SetterIsField then
    OffsetSet := addr
  else
    OffsetSet := -1;
  Name := RttiProp^.Name;
  Prop := RttiProp;
  result := Value.Size;
end;


{ TRttiCustomProps }

procedure TRttiCustomProps.AddFrom(ClassInfo: PRttiInfo; IncludeParents: boolean);
var
  rc: PRttiClass;
  rp: PRttiProp;
  n: PtrInt;
begin
  if ClassInfo = nil then
    exit;
  rc := ClassInfo^.RttiClass;
  if IncludeParents then
    AddFrom(rc^.ParentInfo, true);
  if rc^.PropCount = 0 then
    exit;
  n := Count;
  inc(Count, rc^.PropCount);
  SetLength(List, Count);
  rp := rc^.RttiProps.PropList;
  repeat
    inc(Size, List[n].InitFrom(rp));
    rp := rp^.Next;
    inc(n)
  until n = Count;
end;

function TRttiCustomProps.Find(const PropName: shortstring): PRttiCustomProp;
begin
  result := Find(@PropName[1], ord(PropName[0]));
end;

function TRttiCustomProps.Find(PropName: PUTF8Char;
  PropNameLen: PtrInt): PRttiCustomProp;
var
  n: integer;
  p: ^PRttiCustomProp;
  s: PUTF8Char;
begin
  p := pointer(List);
  if p <> nil then
  begin
    n := Count;
    repeat
      s := pointer(p^.Name);
      if (ord(s[0]) = PropNameLen) and
         IdemPropNameUSameLen(s + 1, PropName, PropNameLen) then
      begin
        result := p^;
        exit;
      end;
      inc(p);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;

function TRttiCustomProps.NameChange(const Old, New: shortstring): PRttiCustomProp;
begin
  result := Find(Old);
  if result <> nil then
    result^.Name := @New;
end;


{ TRttiCustom }

constructor TRttiCustom.Create(aInfo: PRttiInfo);
var
  dummy: integer;
  p: TRTTIParserType;
  c: TRTTIParserComplexType;
begin
  aInfo^.ComputeCache(fRtti);
  fKind := aInfo^.Kind;
  fFinalize := RTTI_FINALIZE[fKind];
  fCopy := RTTI_COPY[fKind];
  case fKind of
    rkClass:
    begin
      fProps.AddFrom(aInfo, {includeparents=}true);
      if fProps.Count > 0 then
        include(fFlags, rcfHasNestedProperties);
    end;
  end;
  if fKind = rkDynArray then
    p := DynArrayTypeInfoToParserType(aInfo, Rtti.ItemInfo,
      Rtti.ItemSize, {exacttype=}true, dummy, @c)
  else
    p := TypeInfoToParserType(aInfo, {byname=}true, @c);
  SetParserType(p, c);
end;

function TRttiCustom.SetParserType(aParser: TRTTIParserType;
  aParserComplex: TRTTIParserComplexType): TRttiCustom;
begin
  fParser := aParser;
  fParserComplex := aParserComplex;
  result := self;
end;

function TRttiCustom.ValueFinalize(Data: pointer): PtrInt;
begin
  if Assigned(fFinalize) then
    result := fFinalize(Data, fRtti.Info)  // =_ObjArrayClear for rcfObjArray
  else
    result := fRtti.Size;
end;

function TRttiCustom.ValueCopy(Dest, Source: pointer): PtrInt;
begin
  if Assigned(fCopy) then
    result := fCopy(Dest, Source, fRtti.Info)
  else
  begin
    MoveFast(Source^, Dest^, fRtti.Size);
    result := fRtti.Size;
  end;
end;

function TRttiCustom.ValueIsVoid(Data: pointer): boolean;
begin
  if Kind <> rkVariant then
    result := IsZeroSmall(Data, fRtti.Size)
  else
    result := integer(PVarData(Data)^.VType) <= varNull; // empty or null
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

function TRttiCustom.SetObjArray(Item: TClass): TRttiCustom;
begin
  if (self <> nil) and (Kind = rkDynArray) and
     (Rtti.ItemSize = SizeOf(pointer)) and (Rtti.ItemInfo = nil) then
  begin
    fObjArrayClass := Item;
    if Item = nil then
    begin  // unregister
      exclude(fFlags, rcfObjArray);
      fArrayRtti := nil;
      fFinalize := @_DynArrayClear;
    end
    else
    begin  // register
      include(fFlags, rcfObjArray);
      fArrayRtti := RttiCustom.RegisterType(Item.ClassInfo);
      fFinalize := @_ObjArrayClear; // calls RawObjectsClear()
    end;
  end;
  result := self;
end;

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
        BinarySize := Rtti.Size;
      include(fFlags, rcfBinary);
    end;
    fBinarySize := BinarySize;
  end;
  result := self;
end;


{ TRttiCustomList }

function TRttiCustomList.Find(Info: PRttiInfo): TRttiCustom;
var
  PEnd: PAnsiChar;
  k: TRttiKind;
  // paranoid use of a local TPointerDynArray for refcnt? slower...
begin
  k := Info.Kind;
  if k <> rkClass then
  begin
    result := pointer(Pairs[k, ord(Info.RawName[0]) and RTTICUSTOMTYPEINFOHASH]);
    if result = nil then
      exit;
    PEnd := @PPointerArray(result)[PDALen(PAnsiChar(result) - _DALEN)^ + _DAOFF];
    repeat // fast brute force search within L1 cache
      if PPointer(result)^ <> Info then
      begin
        inc(PByte(result), 2 * SizeOf(pointer)); // PRttiInfo/TRttiCustom pairs
        if PAnsiChar(result) < PEnd then
          continue;
        result := nil; // not found
        exit;
      end;
      inc(PPointer(result)); // found
      exit;
    until false;
  end
  else
    // use vmtAutoTable slot
    result := ClassPropertiesGet(Info.RttiClass.RttiClass, TRttiCustom);
end;

function TRttiCustomList.Find(ObjectClass: TClass): TRttiCustom;
begin
  result := ClassPropertiesGet(ObjectClass, TRttiCustom);
end;

function TRttiCustomList.Find(Name: PUTF8Char; NameLen: PtrInt;
  Kind: TRttiKind): TRttiCustom;
var
  PEnd: pointer;
  s: PRttiInfo;
begin
  if (Kind = rkUnknown) or (Name = nil) or (NameLen < 0) then
  begin
    result := nil;
    exit;
  end;
  result := pointer(Pairs[Kind, NameLen and RTTICUSTOMTYPEINFOHASH]);
  if result = nil then
    exit;
  PEnd := @PPointerArray(result)[PDALen(PAnsiChar(result) - _DALEN)^ + _DAOFF];
  repeat
    s := PPointer(result)^;
    if (ord(s^.RawName[0]) <> NameLen) or
       not IdemPropNameUSameLen(Name, @s^.RawName[1], NameLen) then
    begin
      inc(PByte(result), 2 * SizeOf(pointer)); // PRttiInfo/TRttiCustom pairs
      if pointer(result) <> PEnd then
        continue;
      result := nil; // not found
      exit;
    end;
    inc(PPointer(result)); // found
    exit;
  until false;
end;

function TRttiCustomList.Find(Name: PUTF8Char; NameLen: PtrInt;
  Kinds: TRttiKinds): TRttiCustom;
var
  k: TRttiKind;
begin
  if Kinds = [] then
    Kinds := rkAllTypes;
  for k := low(Pairs) to high(Pairs) do
    if (k in Kinds) and
       (Pairs[k, NameLen and RTTICUSTOMTYPEINFOHASH] <> nil) then
      begin
        result := Find(Name, NameLen, k);
        if result <> nil then
          exit;
      end;
  result := nil;
end;

function TRttiCustomList.Find(const Name: shortstring; Kinds: TRttiKinds): TRttiCustom;
begin
  result := Find(@Name[1], ord(Name[0]), Kinds);
end;

function TRttiCustomList.RegisterType(Info: PRttiInfo): TRttiCustom;
begin
  result := Find(Info);
  if result = nil then
    result := DoRegister(Info);
end;

function TRttiCustomList.DoRegister(Info: PRttiInfo): TRttiCustom;
var
  hash, n: PtrInt;
  newlist: TPointerDynArray; // don't touch List during background Find()
begin
  EnterCriticalSection(Lock);
  try
    result := Find(Info); // search again (for thread safety)
    if result <> nil then
      exit; // already registered in the background
    result := GlobalClass.Create(Info);
    ObjArrayAddCount(Instances, result, Count); // to release memory
    hash := ord(Info.RawName[0]) and RTTICUSTOMTYPEINFOHASH;
    newlist := copy(Pairs[Info.Kind, hash]);
    n := length(newlist);
    SetLength(newlist, n + 2); // PRttiInfo/TRttiCustom pairs
    newlist[n] := Info;
    newlist[n + 1] := result;
    case result.Kind of
     rkDynArray, rkArray:
       // also register associated record
       if (result.Rtti.ItemInfo <> nil) and
          (result.Rtti.ItemInfo.Kind in rkRecordTypes) then
          RegisterType(result.Rtti.ItemInfo);
     rkClass:
       // set vmtAutoTable slot for Find(TClass)
       ClassPropertiesAdd(Info.RttiClass.RttiClass, result);
    end;
    Pairs[Info.Kind, hash] := newlist; // atomic set for almost-thread-safe Find()
  finally
    LeaveCriticalSection(Lock);
  end;
  assert(Find(Info) = result); // paranoid check
end;

procedure TRttiCustomList.RegisterTypes(const Info: array of PRttiInfo);
var
  i: PtrInt;
begin
  for i := 0 to high(Info) do
    RegisterType(Info[i]);
end;

function TRttiCustomList.RegisterClass(ObjectClass: TClass): TRttiCustom;
begin
  result := RegisterType(ObjectClass.ClassInfo);
end;

procedure TRttiCustomList.RegisterClasses(const ObjectClass: array of TClass);
var
  i: PtrInt;
begin
  for i := 0 to high(ObjectClass) do
    RegisterType(ObjectClass[i].ClassInfo);
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
  if (n <> 0) and (n and 1 = 0) then
    for i := 0 to (n shr 1) - 1 do
      if (InfoBinarySize[i * 2].VType <> vtPointer) or
         not(InfoBinarySize[i * 2 + 1].VType in [vtInteger, vtInt64]) then
        raise ERttiException.Create('RttiCustom.RegisterBinaryTypes[?]')
      else if RegisterType(InfoBinarySize[i * 2].VPointer).
         SetBinaryType(InfoBinarySize[i * 2 + 1].VInteger) = nil then
        raise ERttiException.CreateUTF8('RttiCustom.RegisterBinaryTypes: %?',
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
  if (n <> 0) and (n and 1 = 0) then
    for i := 0 to (n shr 1) - 1 do
      if (DynArrayItem[i * 2].VType <> vtPointer) or
         (DynArrayItem[i * 2 + 1].VType <> vtClass) then
        raise ERttiException.Create('RttiCustom.RegisterObjArrays[?]')
      else
        RegisterObjArray(DynArrayItem[i * 2].VPointer,
          DynArrayItem[i * 2 + 1].VClass);
end;



procedure InitializeUnit;
var
  k: TRttiKind;
  t: TRTTIParserType;
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
  RTTI_TO_VARTYPE[rkChar]    := varUnknown; // allocate temp RawUTF8 -> varString
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
  {$else}
  {$endif FPC}
  for k := low(k) to high(k) do // paranoid checks
  begin
    if Assigned(RTTI_FINALIZE[k]) <> (k in rkManagedTypes) then
      raise ERttiException.CreateUTF8('Unexpected RTTI_FINALIZE[%]', [ToText(k)^]);
    if Assigned(RTTI_COPY[k]) <> (k in rkManagedTypes) then
      raise ERttiException.CreateUTF8('Unexpected RTTI_COPY[%]', [ToText(k)^]);
    case k of
      rkEnumeration, rkSet, rkDynArray, rkClass, rkInterface, rkRecord, rkArray:
        RTTI_TO_VARTYPE[k] := varAny; // VAny = Field address
    end;
  end;
  PT_INFO[ptBoolean] := TypeInfo(Boolean);
  PT_INFO[ptByte] := TypeInfo(Byte);
  PT_INFO[ptCardinal] := TypeInfo(Cardinal);
  PT_INFO[ptCurrency] := TypeInfo(Currency);
  PT_INFO[ptDouble] := TypeInfo(Double);
  PT_INFO[ptExtended] := TypeInfo(Extended);
  PT_INFO[ptInt64] := TypeInfo(Int64);
  PT_INFO[ptInteger] := TypeInfo(Integer);
  PT_INFO[ptQWord] := TypeInfo(QWord);
  PT_INFO[ptRawByteString] := TypeInfo(RawByteString);
  PT_INFO[ptRawJSON] := TypeInfo(RawJSON);
  PT_INFO[ptRawUTF8] := TypeInfo(RawUTF8);
  PT_INFO[ptSingle] := TypeInfo(Single);
  PT_INFO[ptString] := TypeInfo(String);
  PT_INFO[ptSynUnicode] := TypeInfo(SynUnicode);
  PT_INFO[ptDateTime] := TypeInfo(TDateTime);
  PT_INFO[ptDateTimeMS] := TypeInfo(TDateTimeMS);
  {$ifdef HASNOSTATICRTTI} // for Delphi 7/2007: use fake TypeInfo()
  PT_INFO[ptGUID] := @_TGUID;
  PT_INFO[ptHash128] := @_THASH128;
  PT_INFO[ptHash256] := @_THASH256;
  PT_INFO[ptHash512] := @_THASH512;
  {$else}
  PT_INFO[ptGUID] := TypeInfo(TGUID);
  PT_INFO[ptHash128] := TypeInfo(THash128);
  PT_INFO[ptHash256] := TypeInfo(THash256);
  PT_INFO[ptHash512] := TypeInfo(THash512);
  {$endif HASNOSTATICRTTI}
  {$ifdef HASVARUSTRING}
  PT_INFO[ptUnicodeString] := TypeInfo(UnicodeString);
  {$else}
  PT_INFO[ptUnicodeString] := TypeInfo(SynUnicode);
  {$endif HASVARUSTRING}
  PT_INFO[ptUnixTime] := TypeInfo(TUnixTime);
  PT_INFO[ptUnixMSTime] := TypeInfo(TUnixMSTime);
  PT_INFO[ptVariant] := TypeInfo(Variant);
  PT_INFO[ptWideString] := TypeInfo(WideString);
  PT_INFO[ptWinAnsi] := TypeInfo(WinAnsiString);
  PT_INFO[ptWord] := TypeInfo(Word);
  // ptComplexTypes may have several matching TypeInfo() -> put generic
  PT_INFO[ptORM] := TypeInfo(TID);
  PT_INFO[ptTimeLog] := TypeInfo(TTimeLog);
  for t := succ(low(t)) to high(t) do
    if Assigned(PT_INFO[t]) = (t in (ptComplexTypes - [ptORM, ptTimeLog])) then
      raise ERttiException.CreateUTF8('Unexpected PT_INFO[%]', [ToText(t)^]);
  // prepare global thread-safe TRttiCustomList
  InitializeCriticalSection(RttiCustom.Lock);
  RttiCustom.GlobalClass := TRttiCustom;
end;

procedure FinalizeUnit;
var
  i: PtrInt;
begin
  for i := 0 to RttiCustom.Count - 1 do
    RttiCustom.Instances[i].Free;
end;

initialization
  InitializeUnit;
  assert(SizeOf(TRttiVarData) = SizeOf(TVarData));
  assert(@PRttiVarData(nil)^.VInt64 = @PVarData(nil)^.VInt64);
  {$ifdef FPC_OR_UNICODE}
  assert(SizeOf(TRttiRecordField) = SizeOf(TManagedField));
  {$endif FPC_OR_UNICODE}

finalization
  FinalizeUnit;
end.

