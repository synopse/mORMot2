/// Framework Core Low-Level Cross-Compiler RTTI Definitions
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.rtti;

{
  *****************************************************************************

   Cross-Compiler RTTI Definitions shared by all framework units
    - Low-Level Cross-Compiler RTTI Definitions

    - Enumerations RTTI
    - Record And Dynamic Array RTTI
    - Published Class Properties RTTI
    - Published Methods RTTI
    - IInvokable Interface RTTI

    Purpose of this unit is to avoid any direct use of TypInfo.pas RTL unit,
    which is not exactly compatible between compilers, and lack of direct
    RTTI access with no memory allocation. We define pointers to RTTI
    record/object to access TypeInfo() via a set of explicit methods.
    Here fake record/objects are just wrappers around pointers defined in
    Delphi/FPC RTL's TypInfo.pas with the magic of inlining.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  typinfo, // Delphi requires those definitions up here for proper inlining
  mormot.core.base;


{ ************* Low-Level Cross-Compiler RTTI Definitions }

type
  /// specify ordinal (tkInteger and tkEnumeration) storage size and sign
  // - note: on FPC, Int64 is stored as its own TTypeKind, not as tkInteger
  TOrdType = (otSByte, otUByte, otSWord, otUWord, otSLong, otULong
    {$ifdef FPC_NEWRTTI} , otSQWord, otUQWord {$endif});

  /// specify floating point (ftFloat) storage size and precision
  // - here ftDouble is renamed ftDoub to avoid confusion with TSQLDBFieldType
  TFloatType = (ftSingle, ftDoub, ftExtended, ftComp, ftCurr);

{$ifdef FPC}

  /// available type families for FPC RTTI values
  // - values differs from Delphi,  and are taken from FPC typinfo.pp unit
  // - here below,  we defined tkLString instead of tkAString to match Delphi -
  // see https://lists.freepascal.org/pipermail/fpc-devel/2013-June/032360.html
  // "Compiler uses internally some LongStrings which is not possible to use
  // for variable declarations" so tkLStringOld seems never used in practice
  TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet,
    tkMethod, tkSString, tkLStringOld {=tkLString}, tkLString {=tkAString},
    tkWString, tkVariant, tkArray, tkRecord, tkInterface,
    tkClass, tkObject, tkWChar, tkBool, tkInt64, tkQWord,
    tkDynArray, tkInterfaceRaw, tkProcVar, tkUString, tkUChar,
    tkHelper, tkFile, tkClassRef, tkPointer);

const
  /// potentially managed types in TTypeKind RTTI enumerate
  // - should match ManagedType*() functions
  tkManagedTypes = [tkLStringOld, tkLString, tkWstring, tkUstring, tkArray,
                    tkObject, tkRecord, tkDynArray, tkInterface, tkVariant];
  /// maps record or object in TTypeKind RTTI enumerate
  tkRecordTypes = [tkObject, tkRecord];
  /// maps record or object in TTypeKind RTTI enumerate
  tkRecordKinds = [tkObject, tkRecord];

type
  ///  TTypeKind RTTI enumerate as defined in Delphi 6 and up
  TDelphiTypeKind = (dkUnknown, dkInteger, dkChar, dkEnumeration, dkFloat,
    dkString, dkSet, dkClass, dkMethod, dkWChar, dkLString, dkWString,
    dkVariant, dkArray, dkRecord, dkInterface, dkInt64, dkDynArray,
    dkUString, dkClassRef, dkPointer, dkProcedure);

const
  /// convert FPC's TTypeKind to Delphi's RTTI enumerate
  // - used internally for cross-compiler TDynArray binary serialization
  FPCTODELPHI: array[TTypeKind] of TDelphiTypeKind = (
    dkUnknown, dkInteger, dkChar, dkEnumeration, dkFloat,
    dkSet, dkMethod, dkString, dkLString, dkLString,
    dkWString, dkVariant, dkArray, dkRecord, dkInterface,
    dkClass, dkRecord, dkWChar, dkEnumeration, dkInt64, dkInt64,
    dkDynArray, dkInterface, dkProcedure, dkUString, dkWChar,
    dkPointer, dkPointer, dkClassRef, dkPointer);

  /// convert Delphi's TTypeKind to FPC's RTTI enumerate
  DELPHITOFPC: array[TDelphiTypeKind] of TTypeKind = (
    tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkSString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray,
    tkUString, tkClassRef, tkPointer, tkProcVar);

{$else}

  /// available type families for Delphi 6 and up, similar to typinfo.pas
  // - redefined here to be shared between SynCommons.pas and mORMot.pas,
  // also leveraging FPC compatibility as much as possible (FPC's typinfo.pp
  // is not convenient to share code with Delphi - see e.g. its tkLString)
  TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
    {$ifdef UNICODE} , tkUString, tkClassRef, tkPointer, tkProcedure {$endif} );

const
  /// maps record or object in TTypeKind RTTI enumerate
  tkRecordTypes = [tkRecord];
  /// maps record or object in TTypeKind RTTI enumerate
  tkRecordKinds = tkRecord;

{$endif FPC}

  /// maps long string in TTypeKind RTTI enumerate
  tkStringTypes =
    [tkLString, {$ifdef FPC} tkLStringOld, {$endif} tkWString
     {$ifdef HASVARUSTRING} , tkUString {$endif} ];

  /// maps 1, 8, 16, 32 and 64-bit ordinal in TTypeKind RTTI enumerate
  tkOrdinalTypes =
    [tkInteger, tkChar, tkWChar, tkEnumeration, tkSet, tkInt64
     {$ifdef FPC} , tkBool, tkQWord {$endif} ];

  /// quick retrieve how many bytes an ordinal consist in
  ORDTYPE_SIZE: array[TOrdType] of byte =
    (1, 1, 2, 2, 4, 4 {$ifdef FPC_NEWRTTI} , 8, 8 {$endif} );

  NULL_SHORTSTRING: string[1] = '';

  
type
  PTypeKind = ^TTypeKind;
  TTypeKinds = set of TTypeKind;
  POrdType = ^TOrdType;
  PFloatType = ^TFloatType;

type
  {$ifndef FPC}
  PMethod = ^TMethod; // not defined on older Delphi revisions
  {$endif FPC}

  /// pointer to low-level RTTI of a type definition, as returned by TypeInfo()
  // system function
  PTypeInfo = ^TTypeInfo;

  /// double-reference to RTTI type definition
  // - Delphi and newer FPC do store all nested TTypeInfo as pointer to pointer,
  // to ease linking of the executable
  PPTypeInfo = ^PTypeInfo;

  /// dynamic array of low-level RTTI type definitions
  PTypeInfoDynArray = array of PTypeInfo;

  /// pointer to a RTTI class property definition as stored in PClassProp.PropList
  PPropInfo = ^TPropInfo;

  /// used to store a chain of properties RTTI
  // - could be used e.g. by TSQLPropInfo to handled flattened properties
  PPropInfoDynArray = array of PPropInfo;

  /// pointer to all RTTI class properties definitions
  // - as returned by PTypeInfo.ClassProp()
  PClassProp = ^TClassProp;

  /// a wrapper to published properties of a class, as defined by compiler RTTI
  // - access properties for only a given class level, not inherited properties
  // - start enumeration by getting a PClassProp with PTypeInfo.ClassProp(), then
  // use P := PropList to get the first PPropInfo, and iterate with P^.Next
  // - this enumeration is very fast and doesn't require any temporary memory,
  //  as in the TypInfo.GetPropInfos() PPropList usage
  // - for TSQLRecord, you should better use the RecordProps.Fields[] array,
  // which is faster and contains the properties published in parent classes
  TClassProp = object
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
    function PropList: PPropInfo; {$ifdef HASINLINE} inline; {$endif}
    /// retrieve a Field property RTTI information from a Property Name
    function FieldProp(const PropName: shortstring): PPropInfo;
  end;

  /// pointer to TClassType, as returned by PTypeInfo.ClassType()
  PClassType = ^TClassType;

  /// a wrapper to class type information, as defined by the compiler RTTI
  // - get a PClassType with PTypeInfo.ClassType()
  TClassType = object
  public
    /// the class type
    // - not defined as an inlined function, since first field is always aligned
    ClassType: TClass;
    /// the parent class type information
    function ParentInfo: PTypeInfo; {$ifdef HASINLINE} inline; {$endif}
    /// the number of published properties
    function PropCount: integer; {$ifdef HASINLINE} inline; {$endif}
    /// the name (without .pas extension) of the unit were the class was defined
    // - then the PClassProp follows: use the method ClassProp to retrieve its
    // address
    function UnitName: ShortString; {$ifdef HASINLINE} inline; {$endif}
    /// get the information about the published properties of this class
    // - stored after UnitName memory
    function ClassProp: PClassProp; {$ifdef HASINLINE} inline; {$endif}
    /// fast and easy find if this class inherits from a specific class type
    // - you should rather consider using TTypeInfo.InheritsFrom directly
    function InheritsFrom(AClass: TClass): boolean;
  end;

  /// pointer to TEnumType, as returned by PTypeInfo.EnumBaseType/SetEnumType
  PEnumType = ^TEnumType;

  /// a wrapper to enumeration type information, as defined by the compiler RTTI
  // and returned by PTypeInfo.EnumBaseType/SetEnumType
  // - we use this to store the enumeration values as integer, but easily provide
  // a text equivalent, translated if necessary, from the enumeration type
  // definition itself
  TEnumType = object
  private
    // as used by TTypeInfo.EnumBaseType/SetBaseType
    function EnumBaseType: PEnumType; {$ifdef HASINLINE} inline; {$endif}
    function SetBaseType: PEnumType; {$ifdef HASINLINE} inline; {$endif}
  public
    /// specify ordinal storage size and sign
    // - is prefered to MaxValue to identify the number of stored bytes
    // - not defined as an inlined function, since first field is always aligned
    OrdType: TOrdType;
    /// first value of enumeration type, typicaly 0
    // - may be < 0 e.g. for boolean
    function MinValue: PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// same as ord(high(type)): not the enumeration count, but the highest index
    function MaxValue: PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// a concatenation of shortstrings, containing the enumeration names
    // - those shortstrings are not aligned whatsoever (even if
    // FPC_REQUIRES_PROPER_ALIGNMENT is set)
    function NameList: PShortString; {$ifdef HASINLINE} inline; {$endif}
    /// get the corresponding enumeration name
    // - return the first one if Value is invalid (>MaxValue)
    function GetEnumNameOrd(Value: Integer): PShortString;
    /// get the corresponding enumeration name
    // - return the first one if Value is invalid (>MaxValue)
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetEnumName(const Value): PShortString; {$ifdef HASINLINE} inline; {$endif}
    /// retrieve all element names as a dynamic array of RawUTF8
    // - names could be optionally trimmed left from their initial lower chars
    procedure GetEnumNameAll(var result: TRawUTF8DynArray; TrimLeftLowerCase: boolean); overload;
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
    /// get the corresponding enumeration name, without the first lowercase chars
    // (otDone -> 'Done')
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetEnumNameTrimed(const Value): RawUTF8; {$ifdef HASINLINE} inline; {$endif}
    /// get the enumeration names corresponding to a set value
    function GetSetNameCSV(Value: integer; SepChar: AnsiChar = ',';
      FullSetsAsStar: boolean = false): RawUTF8; overload;
    ///  get the corresponding caption name, without the first lowercase chars
    // (otDone -> 'Done')
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    // - internally call UnCamelCase() then System.LoadResStringTranslate() if available
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetCaption(const Value): string;
    /// get all caption names, ready to be display, as lines separated by #13#10
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    // - if UsedValuesBits is not nil, only the corresponding bits set are added
    function GetCaptionStrings(UsedValuesBits: Pointer = nil): string;
    /// add caption names, ready to be display, to a TStrings class
    // - add pointer(ord(element)) as Objects[] value
    // - if UsedValuesBits is not nil, only the corresponding bits set are added
    // - can be used e.g. to populate a combo box as such:
    // ! PTypeInfo(TypeInfo(TMyEnum))^.EnumBaseType^.AddCaptionStrings(ComboBox.Items);
    procedure AddCaptionStrings(Strings: TStrings; UsedValuesBits: Pointer = nil);
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
    /// compute how many bytes this type will use to be stored as a set
    function SizeInStorageAsSet: Integer;
    /// store an enumeration value from its ordinal representation
    // - copy SizeInStorageAsEnum bytes from Ordinal to Value pointer
    procedure SetEnumFromOrdinal(out Value; Ordinal: Integer);
  end;

  /// RTTI of a record/object type definition (managed) field
  // - defined as a record since it is the same for FPC and Delphi, and
  // is not available in oldest Delphi's TypInfo.pas
  // - see e.g. TRecordElement in FPC rtti.inc
  TRecordField = record
    /// the RTTI of this managed field
    {$ifdef HASDIRECTTYPEINFO}
    TypeInfo: PTypeInfo;
    {$else}
    TypeInfoRef: PPTypeInfo;
    {$endif}
    /// where this managed field starts in the record memory layout
    Offset: PtrUInt;
  end;
  /// pointer to the RTTI of a record/object type definition (managed) field
  PRecordField = ^TRecordField;

  /// define the interface abilities
  TIntfFlag = (ifHasGuid, ifDispInterface, ifDispatch
    {$ifdef FPC} , ifHasStrGUID {$endif});
  /// define the set of interface abilities
  TIntfFlags = set of TIntfFlag;

  /// a wrapper to interface type information, as defined by the the compiler RTTI
  TInterfaceTypeData = object
    /// ancestor interface type
    function IntfParent: PTypeInfo; {$ifdef HASINLINE} inline; {$endif}
    /// interface abilities
    function IntfFlags: TIntfFlags; {$ifdef HASINLINE} inline; {$endif}
    /// interface 128-bit GUID
    function IntfGuid: PGUID; {$ifdef HASINLINE} inline; {$endif}
    /// where the interface has been defined
    function IntfUnit: PShortString; {$ifdef HASINLINE} inline; {$endif}
  end;

  /// pointer to a wrapper to interface type information
  PInterfaceTypeData = ^TInterfaceTypeData;

  /// information returned by TTypeInfo.RecordManagedFields
  TRecordManagedFields = record
    /// the record size in bytes
    Size: integer;
    /// how many managed Fields[] are defintion in this record
    Count: integer;
    /// points to the first field RTTI
    // - use inc(Fields) to go to the next one
    Fields: PRecordField;
  end;

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
  TTypeInfo = object
    /// the value type family
    // - not defined as an inlined function, since first field is always aligned
    Kind: TTypeKind;
    /// the declared name of the type ('String','Word','RawUnicode'...)
    // - all subsequent properties should be accessed via TTypeInfo methods
    function Name: ShortString; {$ifdef HASINLINE} inline; {$endif}
    /// for ordinal types, get the storage size and sign
    function OrdType: TOrdType; {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property is an unsigned 64-bit field
    function IsQWord: boolean; {$ifdef HASINLINE} inline; {$endif}
    /// for tkFloat: get the storage size and precision
    function FloatType: TFloatType; {$ifdef HASINLINE} inline; {$endif}
    /// for tkEnumeration: get the enumeration type information
    function EnumBaseType: PEnumType; {$ifdef HASINLINE} inline; {$endif}
    /// for tkSet: get the type information of its associated enumeration
    function SetEnumType: PEnumType;
    /// for tkRecordTypes: get the record size
    function RecordSize: integer; {$ifdef HASINLINE} inline; {$endif}
    /// retrieve RTTI information about all managed fields of this record
    // - non managed fields (e.g. integers, double...) are not listed here
    // - optionally returns the total record size in bytes
    procedure RecordManagedFields(out Fields: TRecordManagedFields);
      {$ifdef HASINLINE} inline; {$endif}
    /// for tkDynArray: get the dynamic array type information of the stored item
    function DynArrayItemType(aDataSize: PInteger = nil): PTypeInfo;
      {$ifdef HASINLINE} inline; {$endif}
    /// for tkDynArray: get the dynamic array size (in bytes) of the stored item
    function DynArrayItemSize: integer; {$ifdef HASINLINE} inline; {$endif}
    /// recognize most used string types, returning their code page
    // - will recognize TSQLRawBlob as the fake CP_SQLRAWBLOB code page
    // - will return the exact code page since Delphi 2009, from RTTI
    // - for non Unicode versions of Delphi, will recognize WinAnsiString as
    // CODEPAGE_US, RawUnicode as CP_UTF16, RawByteString as CP_RAWBYTESTRING,
    // AnsiString as 0, and any other type as RawUTF8
    function AnsiStringCodePage: integer; {$ifdef HASCODEPAGE}inline;{$endif}
    /// for tkClass: get the class type information
    function ClassType: PClassType; {$ifdef HASINLINE} inline; {$endif}
    /// for tkClass: return the number of published properties in this class
    // - you can count the plain fields without any getter function, if you
    // do need only the published properties corresponding to some value
    // actually stored, and ignore e.g. any textual conversion
    function ClassFieldCount(onlyWithoutGetter: boolean): integer;
    /// for tkClass: fast and easy check if a class inherits from this RTTI
    function InheritsFrom(AClass: TClass): boolean;
    /// for tkInterface: get the interface type information
    function InterfaceType: PInterfaceTypeData; {$ifdef HASINLINE} inline; {$endif}
    /// for tkInterface: get the TGUID of a given interface type information
    // - returns nil if this type is not an interface
    function InterfaceGUID: PGUID;
    /// for tkInterface: get the unit name of a given interface type information
    // - returns '' if this type is not an interface
    function InterfaceUnitName: PShortString;
    /// for tkInterface: get the ancestor/parent of a given interface type information
    // - returns nil if this type has no parent
    function InterfaceAncestor: PTypeInfo;
    /// for tkInterface: get all ancestors/parents of a given interface type information
    // - only ancestors with an associated TGUID will be added
    // - if OnlyImplementedBy is not nil, only the interface explicitly
    // implemented by this class will be added, and AncestorsImplementedEntry[]
    // will contain the corresponding PInterfaceEntry values
    procedure InterfaceAncestors(out Ancestors: PTypeInfoDynArray;
      OnlyImplementedBy: TInterfacedObjectClass;
      out AncestorsImplementedEntry: TPointerDynArray);
  end;

  /// how a RTTI property definition access its value
  // - as returned by TPropInfo.Getter/Setter methods
  TPropInfoCall = (
    picNone, picField, picMethod, picIndexed);

  /// a wrapper containing a RTTI class property definition
  // - used for direct Delphi / UTF-8 SQL type mapping/conversion
  // - doesn't depend on RTL's TypInfo unit, to enhance cross-compiler support
  TPropInfo = object
  public
    /// raw retrieval of the property read access definition
    // - note: 'var Call' generated incorrect code on Delphi XE4 -> use PMethod
    function Getter(Instance: TObject; Call: PMethod): TPropInfoCall; {$ifdef HASINLINE} inline; {$endif}
    /// raw retrieval of the property access definition
    function Setter(Instance: TObject; Call: PMethod): TPropInfoCall; {$ifdef HASINLINE} inline; {$endif}
    /// raw retrieval of tkInteger,tkEnumeration,tkSet,tkChar,tkWChar,tkBool
    // - rather call GetOrdValue/GetInt64Value
    function GetOrdProp(Instance: TObject): PtrInt;
    /// raw assignment of tkInteger,tkEnumeration,tkSet,tkChar,tkWChar,tkBool
    // - rather call SetOrdValue/SetInt64Value
    procedure SetOrdProp(Instance: TObject; Value: PtrInt);
    /// raw retrieval of tkClass
    function GetObjProp(Instance: TObject): TObject;
    /// raw retrieval of tkInt64,tkQWord
    // - rather call GetInt64Value
    function GetInt64Prop(Instance: TObject): Int64;
    /// raw assignment of tkInt64,tkQWord
    // - rather call SetInt64Value
    procedure SetInt64Prop(Instance: TObject; const Value: Int64);
    /// raw retrieval of tkLString
    procedure GetLongStrProp(Instance: TObject; var Value: RawByteString);
    /// raw assignment of tkLString
    procedure SetLongStrProp(Instance: TObject; const Value: RawByteString);
    /// raw copy of tkLString
    procedure CopyLongStrProp(Source,Dest: TObject);
    /// raw retrieval of tkString into an Ansi7String
    procedure GetShortStrProp(Instance: TObject; var Value: RawByteString);
    /// raw retrieval of tkWString
    procedure GetWideStrProp(Instance: TObject; var Value: WideString);
    /// raw assignment of tkWString
    procedure SetWideStrProp(Instance: TObject; const Value: WideString);
    {$ifdef HASVARUSTRING}
    /// raw retrieval of tkUString
    procedure GetUnicodeStrProp(Instance: TObject; var Value: UnicodeString);
    /// raw assignment of tkUString
    procedure SetUnicodeStrProp(Instance: TObject; const Value: UnicodeString);
    {$endif HASVARUSTRING}
    /// raw retrieval of tkFloat/currency
    // - use instead GetCurrencyValue
    function GetCurrencyProp(Instance: TObject): currency;
    /// raw assignment of tkFloat/currency
    procedure SetCurrencyProp(Instance: TObject; const Value: Currency);
    /// raw retrieval of tkFloat/double
    function GetDoubleProp(Instance: TObject): double;
    /// raw assignment of tkFloat/double
    procedure SetDoubleProp(Instance: TObject; Value: Double);
    /// raw retrieval of tkFloat - with conversion to 64-bit double
    // - use instead GetDoubleValue
    function GetFloatProp(Instance: TObject): double;
    /// raw assignment of tkFloat
    // - use instead SetDoubleValue
    procedure SetFloatProp(Instance: TObject; Value: TSynExtended);
    /// raw retrieval of tkVariant
    procedure GetVariantProp(Instance: TObject; var result: Variant);
    /// raw assignment of tkVariant
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
    function TypeInfo: PTypeInfo; {$ifdef HASINLINE} inline; {$endif}
    /// get the next property information
    // - no range check: use ClassProp()^.PropCount to determine the properties count
    // - get the first PPropInfo with ClassProp()^.PropList
    function Next: PPropInfo; {$ifdef HASINLINE} inline; {$endif}
    /// return FALSE (AS_UNIQUE) if was marked as "stored AS_UNIQUE"
    //  (i.e. "stored false"), or TRUE by default
    // - if Instance=nil, will work only at RTTI level, not with field or method
    // (and will return TRUE if nothing is defined in the RTTI)
    function IsStored(Instance: TObject): boolean;
    /// copy a published property value from one instance to another
    // - this method use direct copy of the low-level binary content, and is
    // therefore faster than a SetValue(Dest,GetValue(Source)) call
    // - if DestInfo is nil, it will assume DestInfo=@self
    procedure CopyValue(Source, Dest: TObject; DestInfo: PPropInfo = nil);
    /// create a new instance of a published property
    // - copying its properties values from a given instance of another class
    // - if the destination property is not of the aFrom class, it will first
    // search for any extact mach in the destination nested properties
    function CopyToNewObject(aFrom: TObject): TObject;
    /// compare two published properties
    function SameValue(Source: TObject; DestInfo: PPropInfo; Dest: TObject): boolean;
    /// return the Default RTTI value defined for this property, or 0 if not set
    function DefaultOr0: integer; {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property has its Default RTTI value, or is 0/""/nil
    function IsDefaultOrVoid(Instance: TObject): boolean;
    /// compute in how many bytes this property is stored
    function RetrieveFieldSize: integer;
    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - return -1 on any error
    function GetOrdValue(Instance: TObject): PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - ordinal properties smaller than tkInt64 will return an Int64-converted
    // value (e.g. tkInteger)
    // - return 0 on any error
    function GetInt64Value(Instance: TObject): Int64;
    /// low-level getter of the currency property value of a given instance
    // - this method will check if the corresponding property is exactly currency
    // - return 0 on any error
    function GetCurrencyValue(Instance: TObject): Currency;
    /// low-level getter of the floating-point property value of a given instance
    // - this method will check if the corresponding property is floating-point
    // - return 0 on any error
    function GetDoubleValue(Instance: TObject): double;
    /// low-level setter of the floating-point property value of a given instance
    // - this method will check if the corresponding property is floating-point
    procedure SetDoubleValue(Instance: TObject; const Value: double);
    /// low-level getter of the long string property value of a given instance
    // - this method will check if the corresponding property is a Long String,
    // and will return '' if it's not the case
    // - it will convert the property content into RawUTF8, for RawUnicode,
    // WinAnsiString, TSQLRawBlob and generic Delphi 6-2007 string property
    // - WideString and UnicodeString properties will also be UTF-8 converted
    procedure GetLongStrValue(Instance: TObject; var result: RawUTF8);
    /// low-level getter of the long string property content of a given instance
    // - just a wrapper around low-level GetLongStrProp() function
    // - call GetLongStrValue() method if you want a conversion into RawUTF8
    // - will work only for Kind=tkLString
    procedure GetRawByteStringValue(Instance: TObject; var Value: RawByteString);
    /// low-level setter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    procedure SetOrdValue(Instance: TObject; Value: PtrInt);
    /// low-level setter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    procedure SetInt64Value(Instance: TObject; Value: Int64);
    /// low-level setter of the long string property value of a given instance
    // - this method will check if the corresponding property is a Long String
    // - it will convert the property content into RawUTF8, for RawUnicode,
    // WinAnsiString, TSQLRawBlob and generic Delphi 6-2007 string property
    // - will set WideString and UnicodeString properties from UTF-8 content
    procedure SetLongStrValue(Instance: TObject; const Value: RawUTF8);
    /// low-level setter of the string property value of a given instance
    // - uses the generic string type: to be used within the VCL
    // - this method will check if the corresponding property is a Long String
    // or an UnicodeString (for Delphi 2009+), and will call the corresponding
    // SetLongStrValue() or SetUnicodeStrValue() method
    procedure SetGenericStringValue(Instance: TObject; const Value: string);
    /// low-level getter of the long string property value of a given instance
    // - uses the generic string type: to be used within the VCL
    // - this method will check if the corresponding property is a Long String,
    // or an UnicodeString (for Delphi 2009+),and will return '' if it's
    // not the case
    function GetGenericStringValue(Instance: TObject): string;
    {$ifdef HASVARUSTRING}
    /// low-level setter of the Unicode string property value of a given instance
    // - this method will check if the corresponding property is a Unicode String
    procedure SetUnicodeStrValue(Instance: TObject; const Value: UnicodeString);
    /// low-level getter of the Unicode string property value of a given instance
    // - this method will check if the corresponding property is a Unicode String
    function GetUnicodeStrValue(Instance: TObject): UnicodeString;
    {$endif HASVARUSTRING}
    /// return TRUE if the property has no getter but direct field read
    function GetterIsField: boolean;  {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property has no setter but direct field write
    function SetterIsField: boolean;  {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property has a write setter or direct field
    function WriteIsDefined: boolean; {$ifdef HASINLINE} inline; {$endif}
    /// returns the low-level field read address, if GetterIsField is TRUE
    function GetterAddr(Instance: pointer): pointer; {$ifdef HASINLINE} inline; {$endif}
    /// returns the low-level field write address, if SetterIsField is TRUE
    function SetterAddr(Instance: pointer): pointer; {$ifdef HASINLINE} inline; {$endif}
    /// low-level getter of the field value memory pointer
    // - return NIL if both getter and setter are methods
    function GetFieldAddr(Instance: TObject): pointer; {$ifdef HASINLINE} inline; {$endif}
    /// low-level setter of the property value as its default
    // - this method will check the property type, e.g. setting '' for strings,
    // and 0 for numbers, or running FreeAndNil() on any nested object (unless
    // FreeAndNilNestedObjects is false so that ClearObject() is used
    procedure SetDefaultValue(Instance: TObject; FreeAndNilNestedObjects: boolean = true);
    /// low-level setter of the property value from a supplied variant
    // - will optionally make some conversion if the property type doesn't
    // match the variant type, e.g. a text variant could be converted to integer
    // when setting a tkInteger kind of property
    // - a tkDynArray property is expected to be a T*ObjArray and will be
    // converted from a TDocVariant using a newly allocated T*ObjArray
    procedure SetFromVariant(Instance: TObject; const Value: variant);
    /// low-level getter of the property value into a variant value
    // - a tkDynArray property is expected to be a T*ObjArray and will be
    // converted into a TDocVariant using a temporary JSON serialization
    procedure GetVariant(Instance: TObject; var Dest: variant);
  end;

const
  NO_DEFAULT = longint($80000000);

{$ifndef FPC} // Delphi requires those definitions for proper inlining

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

  {$ifdef UNICODE}
  TRecordInfo = TTypeData;
  PRecordInfo = PTypeData;
  {$else}
  TRecordInfo = packed record // tkRecord not defined in Delphi 7/2007 TTypeData
    RecSize: integer;
    ManagedFldCount: integer;
  end;
  PRecordInfo = ^TRecordInfo;
  PPropData = ^TPropData; // not defined e.g. in Delphi 7/2007
  {$endif UNICODE}

{$endif FPC}

/// retrieve the class property RTTI information for a specific class
function InternalClassProp(ClassType: TClass): PClassProp;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the class property RTTI information for a specific class
// - will return the number of published properties
// - and set the PropInfo variable to point to the first property
// - typical use to enumerate all published properties could be:
//  !  var i: integer;
//  !      CT: TClass;
//  !      P: PPropInfo;
//  !  begin
//  !    CT := ..;
//  !    repeat
//  !      for i := 1 to InternalClassPropInfo(CT,P) do begin
//  !        // use P^
//  !        P := P^.Next;
//  !      end;
//  !      CT := GetClassParent(CT);
//  !    until CT=nil;
//  !  end;
// such a loop is much faster than using the RTL's TypeInfo or RTTI units
function InternalClassPropInfo(ClassType: TClass; out PropInfo: PPropInfo): integer;

/// retrieve the total number of properties for a class, including its parents
function ClassFieldCountWithParents(ClassType: TClass;
  onlyWithoutGetter: boolean = false): integer;


implementation

{$ifdef FPC}
  {$include mormot.core.rtti.fpc.inc}
{$else}
  {$include mormot.core.rtti.delphi.inc}
{$endif FPC}


{ ************* Low-Level Cross-Compiler RTTI Definitions }

{ TClassType }

function TClassType.UnitName: ShortString;
begin
  result := PTypeData(@self)^.UnitName;
end;

function TClassType.InheritsFrom(AClass: TClass): boolean;
var
  P: PTypeInfo;
begin
  result := true;
  if ClassType = AClass then
    exit;
  P := ParentInfo;
  while P <> nil do
    with P^.ClassType^ do
      if ClassType = AClass then
        exit
      else
        P := ParentInfo;
  result := false;
end;


{ TClassProp = TPropData in TypInfo }

function TPropInfo.Name: PShortString;
begin
  result := @TypInfo.PPropInfo(@self)^.Name;
end;

function TPropInfo.Next: PPropInfo;
begin // this abtract code compiles into 2 asm lines under FPC :)
  with TypInfo.PPropInfo(@self)^ do
    result := AlignToPtr(@PByteArray(@self)[
      (PtrUInt(@TypInfo.PPropInfo(nil).Name) + SizeOf(Name[0])) + Length(Name)]);
end;

function TClassProp.FieldProp(const PropName: shortstring): PPropInfo;
var i: integer;
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




{ TEnumType }

function TEnumType.MinValue: PtrInt;
begin
  result := PTypeData(@self).MinValue;
end;

function TEnumType.MaxValue: PtrInt;
begin
  result := PTypeData(@self).MaxValue;
end;

function TEnumType.NameList: PShortString;
begin
  result := @PTypeData(@self).NameList;
end;

function TEnumType.SizeInStorageAsEnum: Integer;
begin
  result := ORDTYPE_SIZE[OrdType]; // MaxValue does not work e.g. with WordBool
end;

function TEnumType.SizeInStorageAsSet: Integer;
begin
  case MaxValue of
    0..7:
      result := sizeof(byte);
    8..15:
      result := sizeof(word);
    16..31:
      result := sizeof(cardinal);
    else
      result := 0;
  end;
end;

function TEnumType.GetEnumNameOrd(Value: Integer): PShortString;
begin

end;

function TEnumType.GetEnumName(const Value): PShortString;
begin

end;

procedure TEnumType.GetEnumNameAll(var result: TRawUTF8DynArray;
  TrimLeftLowerCase: boolean);
begin

end;

procedure TEnumType.GetEnumNameAll(var result: RawUTF8; const Prefix: RawUTF8;
  quotedValues: boolean; const Suffix: RawUTF8; trimedValues, unCamelCased: boolean);
begin

end;

procedure TEnumType.GetEnumNameTrimedAll(var result: RawUTF8;
  const Prefix: RawUTF8; quotedValues: boolean; const Suffix: RawUTF8);
begin

end;

function TEnumType.GetEnumNameAllAsJSONArray(TrimLeftLowerCase: boolean;
  UnCamelCased: boolean): RawUTF8;
begin

end;

function TEnumType.GetEnumNameValue(const EnumName: ShortString): Integer;
begin

end;

function TEnumType.GetEnumNameValue(Value: PUTF8Char): Integer;
begin

end;

function TEnumType.GetEnumNameValue(Value: PUTF8Char; ValueLen: integer;
  AlsoTrimLowerCase: boolean): Integer;
begin

end;

function TEnumType.GetEnumNameTrimed(const Value): RawUTF8;
begin

end;

function TEnumType.GetSetNameCSV(Value: integer; SepChar: AnsiChar;
  FullSetsAsStar: boolean): RawUTF8;
begin

end;

function TEnumType.GetCaption(const Value): string;
begin

end;

function TEnumType.GetCaptionStrings(UsedValuesBits: Pointer): string;
begin

end;

procedure TEnumType.AddCaptionStrings(Strings: TStrings; UsedValuesBits: Pointer);
begin

end;

function TEnumType.GetEnumNameTrimedValue(const EnumName: ShortString): Integer;
begin

end;

function TEnumType.GetEnumNameTrimedValue(Value: PUTF8Char; ValueLen: integer): Integer;
begin

end;

procedure TEnumType.SetEnumFromOrdinal(out Value; Ordinal: Integer);
begin

end;


{ TInterfaceTypeData }

function TInterfaceTypeData.IntfFlags: TIntfFlags;
begin
  byte(result) := byte(PTypeData(@self)^.IntfFlags);
end;

function TInterfaceTypeData.IntfGuid: PGUID;
begin
  result := @PTypeData(@self)^.Guid;
end;

function TInterfaceTypeData.IntfUnit: PShortString;
begin
  result := @PTypeData(@self)^.IntfUnit;
end;


{ TTypeInfo }

function TTypeInfo.Name: ShortString;
begin
  result := PTypeInfo(@self)^.Name;
end;

function TTypeInfo.OrdType: TOrdType;
begin
  byte(result) := byte(GetTypeData(@self)^.OrdType);
end;

function TTypeInfo.FloatType: TFloatType;
begin
  byte(result) := byte(GetTypeData(@self)^.FloatType);
end;

function TTypeInfo.ClassFieldCount(onlyWithoutGetter: boolean): integer;
begin
  result := ClassFieldCountWithParents(ClassType^.ClassType, onlyWithoutGetter);
end;

function TTypeInfo.InheritsFrom(AClass: TClass): boolean;
begin
  result := ClassType^.InheritsFrom(AClass);
end;

function TTypeInfo.SetEnumType: PEnumType;
begin
  if (@self = nil) or (Kind <> tkSet) then
    result := nil
  else
    result := PEnumType(GetTypeData(@self))^.SetBaseType;
end;

function TTypeInfo.InterfaceType: PInterfaceTypeData;
begin
  result := pointer(GetTypeData(@self));
end;

function TTypeInfo.DynArrayItemType(aDataSize: PInteger): PTypeInfo;
begin

end;

function TTypeInfo.DynArrayItemSize: integer;
begin

end;

function TTypeInfo.AnsiStringCodePage: integer;
begin

end;

function TTypeInfo.InterfaceGUID: PGUID;
begin
  if (@self = nil) or (Kind <> tkInterface) then
    result := nil
  else
    result := InterfaceType^.IntfGuid;
end;

function TTypeInfo.InterfaceUnitName: PShortString;
begin
  if (@self = nil) or (Kind <> tkInterface) then
    result := @NULL_SHORTSTRING
  else
    result := InterfaceType^.IntfUnit;
end;

function TTypeInfo.InterfaceAncestor: PTypeInfo;
begin
  if (@self = nil) or (Kind <> tkInterface) then
    result := nil
  else
    result := InterfaceType^.IntfParent;
end;

procedure TTypeInfo.InterfaceAncestors(out Ancestors: PTypeInfoDynArray;
  OnlyImplementedBy: TInterfacedObjectClass;
  out AncestorsImplementedEntry: TPointerDynArray);
var
  n: integer;
  nfo: PTypeInfo;
  typ: PInterfaceTypeData;
  entry: pointer;
begin
  if (@self = nil) or (Kind <> tkInterface) then
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


{ TPropInfo }

function TPropInfo.Index: Integer;
begin
  result := TypInfo.PPropInfo(@self)^.Index;
end;

function TPropInfo.Default: Longint;
begin
  result := TypInfo.PPropInfo(@self)^.Default;
end;

function TPropInfo.NameIndex: integer;
begin
  result := TypInfo.PPropInfo(@self)^.NameIndex;
end;

function TPropInfo.IsStored(Instance: TObject): boolean;
type
  TGetProc = function: boolean of object;
  TGetIndexed = function(Index: integer): boolean of object;
var
  pt: byte;
  call: TMethod;
begin
  with TypInfo.PPropInfo(@self)^ do
  begin
    pt := {$ifdef FPC} (PropProcs shr 4) and 3 {$else} PropWrap(StoredProc).Kind {$endif};
    if {$ifdef FPC} pt = ptConst {$else} (PtrUInt(StoredProc) and (not PtrUInt($ff))) = 0 {$endif} then
      result := boolean(PtrUInt(StoredProc))
    else
    begin
      case pt of
        ptField:
          begin
            result := PBoolean(PtrUInt(Instance) +
              PtrUInt(StoredProc) {$ifndef FPC} and $00ffffff {$endif})^;
            exit;
          end;
        ptVirtual:
          call.Code := PPointer(PPtrUInt(Instance)^ +
            {$ifdef FPC} PtrUInt {$else} word {$endif}(StoredProc))^;
        else
          call.Code := pointer(StoredProc);
      end;
      call.Data := Instance;
      if {$ifdef FPC} (PropProcs shr 6) and 1 {$else} Index {$endif} <> NO_INDEX then
        result := TGetIndexed(call)(Index)
      else
        result := TGetProc(call);
    end;
  end;
end;

function TPropInfo.DefaultOr0: integer;
begin
  result := TypInfo.PPropInfo(@self)^.Default;
  if result = NO_DEFAULT then
    result := 0;
end;

function TPropInfo.IsDefaultOrVoid(Instance: TObject): boolean;
begin

end;

function TPropInfo.RetrieveFieldSize: integer;
begin

end;

function TPropInfo.WriteIsDefined: boolean;
begin

end;

function TPropInfo.GetterAddr(Instance: pointer): pointer;
begin
  result := Pointer(PtrUInt(Instance) +
     PtrUInt(TypInfo.PPropInfo(@self)^.GetProc) {$ifndef FPC} and $00ffffff {$endif} );
end;

function TPropInfo.SetterAddr(Instance: pointer): pointer;
begin
  result := Pointer(PtrUInt(Instance) +
     PtrUInt(TypInfo.PPropInfo(@self)^.SetProc) {$ifndef FPC} and $00ffffff {$endif} );
end;

function TPropInfo.GetFieldAddr(Instance: TObject): pointer;
begin

end;

function TPropInfo.Getter(Instance: TObject; Call: PMethod): TPropInfoCall;
begin

end;

function TPropInfo.Setter(Instance: TObject; Call: PMethod): TPropInfoCall;
begin

end;

function TPropInfo.GetOrdProp(Instance: TObject): PtrInt;
begin

end;

procedure TPropInfo.SetOrdProp(Instance: TObject; Value: PtrInt);
begin

end;

function TPropInfo.GetObjProp(Instance: TObject): TObject;
begin

end;

function TPropInfo.GetInt64Prop(Instance: TObject): Int64;
begin

end;

procedure TPropInfo.SetInt64Prop(Instance: TObject; const Value: Int64);
begin

end;

procedure TPropInfo.GetLongStrProp(Instance: TObject; var Value: RawByteString);
begin

end;

procedure TPropInfo.SetLongStrProp(Instance: TObject; const Value: RawByteString);
begin

end;

procedure TPropInfo.CopyLongStrProp(Source, Dest: TObject);
begin

end;

procedure TPropInfo.GetShortStrProp(Instance: TObject; var Value: RawByteString);
begin

end;

procedure TPropInfo.GetWideStrProp(Instance: TObject; var Value: WideString);
begin

end;

procedure TPropInfo.SetWideStrProp(Instance: TObject; const Value: WideString);
begin

end;

{$ifdef HASVARUSTRING}

procedure TPropInfo.GetUnicodeStrProp(Instance: TObject; var Value: UnicodeString);
begin

end;

procedure TPropInfo.SetUnicodeStrProp(Instance: TObject;
  const Value: UnicodeString);
begin

end;

{$endif HASVARUSTRING}

function TPropInfo.GetCurrencyProp(Instance: TObject): currency;
begin

end;

procedure TPropInfo.SetCurrencyProp(Instance: TObject; const Value: Currency);
begin

end;

function TPropInfo.GetDoubleProp(Instance: TObject): double;
begin

end;

procedure TPropInfo.SetDoubleProp(Instance: TObject; Value: Double);
begin

end;

function TPropInfo.GetFloatProp(Instance: TObject): double;
begin

end;

procedure TPropInfo.SetFloatProp(Instance: TObject; Value: TSynExtended);
begin

end;

procedure TPropInfo.GetVariantProp(Instance: TObject; var result: Variant);
begin

end;

procedure TPropInfo.SetVariantProp(Instance: TObject; const Value: Variant);
begin

end;

procedure TPropInfo.CopyValue(Source, Dest: TObject; DestInfo: PPropInfo);
begin

end;

function TPropInfo.CopyToNewObject(aFrom: TObject): TObject;
begin

end;

function TPropInfo.SameValue(Source: TObject; DestInfo: PPropInfo; Dest: TObject): boolean;
begin

end;

function TPropInfo.GetOrdValue(Instance: TObject): PtrInt;
begin

end;

function TPropInfo.GetInt64Value(Instance: TObject): Int64;
begin

end;

function TPropInfo.GetCurrencyValue(Instance: TObject): Currency;
begin

end;

function TPropInfo.GetDoubleValue(Instance: TObject): double;
begin

end;

procedure TPropInfo.SetDoubleValue(Instance: TObject; const Value: double);
begin

end;

procedure TPropInfo.GetLongStrValue(Instance: TObject; var result: RawUTF8);
begin

end;

procedure TPropInfo.GetRawByteStringValue(Instance: TObject;
  var Value: RawByteString);
begin

end;

procedure TPropInfo.SetOrdValue(Instance: TObject; Value: PtrInt);
begin

end;

procedure TPropInfo.SetInt64Value(Instance: TObject; Value: Int64);
begin

end;

procedure TPropInfo.SetLongStrValue(Instance: TObject; const Value: RawUTF8);
begin

end;

procedure TPropInfo.SetGenericStringValue(Instance: TObject; const Value: string);
begin

end;

function TPropInfo.GetGenericStringValue(Instance: TObject): string;
begin

end;

{$ifdef HASVARUSTRING}

procedure TPropInfo.SetUnicodeStrValue(Instance: TObject;
  const Value: UnicodeString);
begin

end;

function TPropInfo.GetUnicodeStrValue(Instance: TObject): UnicodeString;
begin

end;

{$endif HASVARUSTRING}

procedure TPropInfo.SetDefaultValue(Instance: TObject;
  FreeAndNilNestedObjects: boolean);
begin

end;

procedure TPropInfo.SetFromVariant(Instance: TObject; const Value: variant);
begin

end;

procedure TPropInfo.GetVariant(Instance: TObject; var Dest: variant);
begin

end;



function ClassFieldCountWithParents(ClassType: TClass;
  onlyWithoutGetter: boolean): integer;
var CP: PClassProp;
    P: PPropInfo;
    i: integer;
begin
  result := 0;
  while ClassType <> nil do
  begin
    CP := InternalClassProp(ClassType);
    if CP=nil then
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
    ClassType := GetClassParent(ClassType);
  end;
end;

end.

