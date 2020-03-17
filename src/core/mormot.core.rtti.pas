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
  classes,
  mormot.core.base,
  mormot.core.text; // used e.g. on enumerations


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
  // - here below,  we defined rtLString instead of rtAString to match Delphi -
  // see https://lists.freepascal.org/pipermail/fpc-devel/2013-June/032360.html
  // "Compiler uses internally some LongStrings which is not possible to use
  // for variable declarations" so rtLStringOld seems never used in practice
  TRttiKind = (rkUnknown, rkInteger, rkChar, rkEnumeration, rkFloat, rkSet,
    rkMethod, rkSString, rkLStringOld {=rkLString}, rkLString {=rkAString},
    rkWString, rkVariant, rkArray, rkRecord, rkInterface,
    rkClass, rkObject, rkWChar, rkBool, rkInt64, rkQWord,
    rkDynArray, rkInterfaceRaw, rkProcVar, rkUString, rkUChar,
    rkHelper, rkFile, rkClassRef, rkPointer);

const
  /// potentially managed types in TRttiKind enumerate
  // - should match ManagedType*() functions
  rkManagedTypes = [rkLStringOld, rkLString, rkWstring, rkUstring, rkArray,
                    rkObject, rkRecord, rkDynArray, rkInterface, rkVariant];
  /// maps record or object in TRttiKind enumerate
  rkRecordTypes = [rkObject, rkRecord];
  /// maps record or object in TRttiKind enumerate
  rkRecordKinds = [rkObject, rkRecord];

type
  ///  TTypeKind enumerate as defined in Delphi 6 and up
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
  // - redefined here to be shared between SynCommons.pas and mORMot.pas,
  // also leveraging FPC compatibility as much as possible (FPC's typinfo.pp
  // is not convenient to share code with Delphi - see e.g. its rtLString)
  TRttiKind = (rkUnknown, rkInteger, rkChar, rkEnumeration, rkFloat,
    rkString, rkSet, rkClass, rkMethod, rkWChar, rkLString, rkWString,
    rkVariant, rkArray, rkRecord, rkInterface, rkInt64, rkDynArray
    {$ifdef UNICODE}, rkUString, rkClassRef, rkPointer, rkProcedure {$endif});

const
  /// maps record or object in TTypeKind RTTI enumerate
  rkRecordTypes = [rkRecord];
  /// maps record or object in TTypeKind RTTI enumerate
  rkRecordKinds = rkRecord;

{$endif FPC}

  /// maps long string in TRttiKind RTTI enumerate
  rkStringTypes =
    [rkLString, {$ifdef FPC} rkLStringOld, {$endif} rkWString
     {$ifdef HASVARUSTRING} , rkUString {$endif} ];

  /// maps 1, 8, 16, 32 and 64-bit ordinal in TRttiKind RTTI enumerate
  rkOrdinalTypes =
    [rkInteger, rkChar, rkWChar, rkEnumeration, rkSet, rkInt64
     {$ifdef FPC} , rkBool, rkQWord {$endif} ];

  /// quick retrieve how many bytes an ordinal consist in
  ORDTYPE_SIZE: array[TRttiOrd] of byte =
    (1, 1, 2, 2, 4, 4 {$ifdef FPC_NEWRTTI} , 8, 8 {$endif} );

  /// some RTTI methods may return a PShortString pointing to this void text
  NULL_SHORTSTRING: string[1] = '';

  
type
  PRttiKind = ^TRttiKind;
  TRttiKinds = set of TRttiKind;
  PRttiOrd = ^TRttiOrd;
  PRttiFloat = ^TRttiFloat;

type
  /// pointer to low-level RTTI of a type definition, as returned by TypeInfo()
  // system function
  // - equivalency to PTypeInfo as definied in TypInfo RTL unit
  PRttiInfo = ^TRttiInfo;

  /// double-reference to RTTI type definition
  // - Delphi and newer FPC do store all nested TTypeInfo as pointer to pointer,
  // to ease linking of the executable
  PPRttiInfo = ^PRttiInfo;

  /// dynamic array of low-level RTTI type definitions
  PRttiInfoDynArray = array of PRttiInfo;

  /// pointer to a RTTI class property definition as stored in PClassProp.PropList
  PRttiProp = ^TRttiProp;

  /// used to store a chain of properties RTTI
  // - could be used e.g. by TSQLPropInfo to handled flattened properties
  PRttiPropDynArray = array of PRttiProp;

  /// pointer to all RTTI class properties definitions
  // - as returned by PRttiInfo.RttiProps()
  PRttiProps = ^TRttiProps;

  /// a wrapper to published properties of a class, as defined by compiler RTTI
  // - access properties for only a given class level, not inherited properties
  // - start enumeration by getting a PClassProp with PRttiInfo.RttiProps(), then
  // use P := PropList to get the first PPropInfo, and iterate with P^.Next
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

  /// pointer to TClassType, as returned by PTypeInfo.RttiClass()
  PRttiClass = ^TRttiClass;

  /// a wrapper to class type information, as defined by the compiler RTTI
  // - get a PClassType with PTypeInfo.RttiClass()
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
    // - then the PClassProp follows: use the method RttiProps to retrieve its
    // address
    function UnitName: ShortString; {$ifdef HASINLINE} inline; {$endif}
    /// get the information about the published properties of this class
    // - stored after UnitName memory
    function RttiProps: PRttiProps; {$ifdef HASINLINE} inline; {$endif}
    /// fast and easy find if this class inherits from a specific class type
    // - you should rather consider using TRttiInfo.InheritsFrom directly
    function InheritsFrom(AClass: TClass): boolean;
  end;

  /// pointer to TEnumType, as returned by PTypeInfo.EnumBaseType/SetEnumType
  PRttiEnumType = ^TRttiEnumType;

  /// a wrapper to enumeration type information, as defined by the compiler RTTI
  // and returned by PTypeInfo.EnumBaseType/SetEnumType
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
    function MinValue: PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// same as ord(high(type)): not the enumeration count, but the highest index
    function MaxValue: PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// a concatenation of shortstrings, containing the enumeration names
    // - those shortstrings are not aligned whatsoever (even if
    // FPC_REQUIRES_PROPER_ALIGNMENT is set)
    function NameList: PShortString; {$ifdef HASINLINE} inline; {$endif}
    /// get the corresponding enumeration name
    // - return the first one if Value is invalid (>MaxValue)
    function GetEnumNameOrd(Value: cardinal): PShortString; {$ifdef HASINLINE} inline; {$endif}
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
    /// get the corresponding enumeration ordinal value, from its trimmed name
    function GetEnumNameValueTrimmed(Value: PUTF8Char; ValueLen: integer;
      ExactCase: boolean): Integer;
    /// get the corresponding enumeration name, without the first lowercase chars
    // (otDone -> 'Done')
    // - Value will be converted to the matching ordinal value (byte or word)
    function GetEnumNameTrimed(const Value): RawUTF8; {$ifdef HASINLINE} inline; {$endif}
    /// get the enumeration names corresponding to a set value
    function GetSetNameCSV(Value: cardinal; SepChar: AnsiChar = ',';
      FullSetsAsStar: boolean = false): RawUTF8; overload;
    /// get the enumeration names corresponding to a set value
    procedure GetSetNameCSV(W: TAbstractWriter; Value: cardinal; SepChar: AnsiChar = ',';
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
    /// compute how many bytes this type will use to be stored as a set
    function SizeInStorageAsSet: Integer;
    /// store an enumeration value from its ordinal representation
    // - copy SizeInStorageAsEnum bytes from Ordinal to Value pointer
    procedure SetEnumFromOrdinal(out Value; Ordinal: Integer);
  end;

  /// RTTI of a record/object type definition (managed) field
  // - defined as a record since it is the same for FPC and Delphi, and
  // is not available in oldest Delphi's TypInfo.pas
  // - maps TRecordElement in FPC rtti.inc or TManagedField in TypInfo
  TRttiRecordField = record
    /// the RTTI of this managed field
    {$ifdef HASDIRECTTYPEINFO}
    TypeInfo: PRttiInfo;
    {$else}
    TypeInfoRef: PPRttiInfo;
    {$endif}
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
    Size: integer;
    /// how many managed Fields[] are defined in this record
    Count: integer;
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
    /// the value type family
    // - not defined as an inlined function, since first field is always aligned
    Kind: TRttiKind;
    /// the declared name of the type ('String','Word','RawUnicode'...)
    // - won't adjust internal/cardinal names on FPC as with Name method
    RawName: ShortString;
    /// the declared name of the type ('String','Word','RawUnicode'...)
    // - on FPC, will adjust integer/cardinal not as 'longint'/'longword'
    function Name: PShortString;    {$ifdef HASINLINE} inline; {$endif}
    /// for ordinal types, get the storage size and sign
    function RttiOrd: TRttiOrd; {$ifdef HASINLINE} inline; {$endif}
    /// return TRUE if the property is an unsigned 64-bit field (QWord/UInt64)
    function IsQWord: boolean;  {$ifdef HASINLINE} inline; {$endif}
    /// for rtFloat: get the storage size and precision
    function RttiFloat: TRttiFloat;       {$ifdef HASINLINE} inline; {$endif}
    /// for rtEnumeration: get the enumeration type information
    function EnumBaseType: PRttiEnumType; {$ifdef HASINLINE} inline; {$endif}
    /// for rtSet: get the type information of its associated enumeration
    function SetEnumType: PRttiEnumType;  {$ifdef HASINLINE} inline; {$endif}
    /// for rtRecordTypes: get the record size
    function RecordSize: integer;         {$ifdef HASINLINE} inline; {$endif}
    /// for rtRecordTypes: retrieve RTTI information about all managed fields
    // of this record
    // - non managed fields (e.g. integers, double...) are not listed here
    // - also includes the total record size in bytes
    procedure RecordManagedFields(out Fields: TRttiRecordManagedFields);
      {$ifdef HASINLINE} inline; {$endif}
    /// for rtRecordTypes: retrieve enhanced RTTI information about all fields
    // of this record
    // - this information is currently only available since Delphi 2010
    function RecordAllFields(out RecSize: integer): TRttiRecordAllFields;
    /// for rtDynArray: get the dynamic array type information of the stored item
    function DynArrayItemType(aDataSize: PInteger = nil): PRttiInfo;
      {$ifdef HASINLINE} inline; {$endif}
    /// for rtDynArray: get the dynamic array size (in bytes) of the stored item
    function DynArrayItemSize: integer; {$ifdef HASINLINE} inline; {$endif}
    /// for rtArray: get the static array type information of the stored item
    // - returns nil if the array type is unmanaged (i.e. behave like Delphi)
    function ArrayItemType(out aDataSize: integer): PRttiInfo;
      {$ifdef HASINLINE} inline; {$endif}
    /// recognize most used string types, returning their code page
    // - will return the exact code page on FPC and since Delphi 2009, from RTTI
    // - for non Unicode versions of Delphi, will recognize WinAnsiString as
    // CODEPAGE_US, RawUnicode as CP_UTF16, RawByteString as CP_RAWBYTESTRING,
    // AnsiString as 0, and any other type as RawUTF8
    // - warning: it won't recognize TSQLRawBlob as the fake CP_SQLRAWBLOB code
    // page - so caller should first check for TypeInfo(TSQLRawBlob)
    function AnsiStringCodePage: integer; {$ifdef HASCODEPAGE}inline;{$endif}
    /// for rtClass: get the class type information
    function RttiClass: PRttiClass;       {$ifdef HASINLINE} inline; {$endif}
    /// for rtClass: return the number of published properties in this class
    // - you can count the plain fields without any getter function, if you
    // do need only the published properties corresponding to some value
    // actually stored, and ignore e.g. any textual conversion
    function ClassFieldCount(onlyWithoutGetter: boolean): integer;
    /// for rtClass: fast and easy check if a class inherits from this RTTI
    function InheritsFrom(AClass: TClass): boolean;
    /// for rtInterface: get the interface type information
    function InterfaceType: PRttiInterfaceTypeData; {$ifdef HASINLINE} inline; {$endif}
    /// for rtInterface: get the TGUID of a given interface type information
    // - returns nil if this type is not an interface
    function InterfaceGUID: PGUID;
    /// for rtInterface: get the unit name of a given interface type information
    // - returns '' if this type is not an interface
    function InterfaceUnitName: PShortString;
    /// for rtInterface: get the ancestor/parent of a given interface type information
    // - returns nil if this type has no parent
    function InterfaceAncestor: PRttiInfo;
    /// for rtInterface: get all ancestors/parents of a given interface type information
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
    /// raw retrieval of rtInteger,rtEnumeration,rtSet,rtChar,rtWChar,rtBool
    // - rather call GetOrdValue/GetInt64Value
    function GetOrdProp(Instance: TObject): PtrInt;
    /// raw assignment of rtInteger,rtEnumeration,rtSet,rtChar,rtWChar,rtBool
    // - rather call SetOrdValue/SetInt64Value
    procedure SetOrdProp(Instance: TObject; Value: PtrInt);
    /// raw retrieval of rtClass
    function GetObjProp(Instance: TObject): TObject;
    /// raw retrieval of rtInt64,rtQWord
    // - rather call GetInt64Value
    function GetInt64Prop(Instance: TObject): Int64;
    /// raw assignment of rtInt64,rtQWord
    // - rather call SetInt64Value
    procedure SetInt64Prop(Instance: TObject; const Value: Int64);
    /// raw retrieval of rtLString
    procedure GetLongStrProp(Instance: TObject; var Value: RawByteString);
    /// raw assignment of rtLString
    procedure SetLongStrProp(Instance: TObject; const Value: RawByteString);
    /// raw copy of rtLString
    procedure CopyLongStrProp(Source,Dest: TObject);
    /// raw retrieval of rtString into an Ansi7String
    procedure GetShortStrProp(Instance: TObject; var Value: RawByteString);
    /// raw retrieval of rtWString
    procedure GetWideStrProp(Instance: TObject; var Value: WideString);
    /// raw assignment of rtWString
    procedure SetWideStrProp(Instance: TObject; const Value: WideString);
    {$ifdef HASVARUSTRING}
    /// raw retrieval of rtUString
    procedure GetUnicodeStrProp(Instance: TObject; var Value: UnicodeString);
    /// raw assignment of rtUString
    procedure SetUnicodeStrProp(Instance: TObject; const Value: UnicodeString);
    {$endif HASVARUSTRING}
    /// raw retrieval of rtFloat/currency
    // - use instead GetCurrencyValue
    function GetCurrencyProp(Instance: TObject): currency;
    /// raw assignment of rtFloat/currency
    procedure SetCurrencyProp(Instance: TObject; const Value: Currency);
    /// raw retrieval of rtFloat/double
    function GetDoubleProp(Instance: TObject): double;
    /// raw assignment of rtFloat/double
    procedure SetDoubleProp(Instance: TObject; Value: Double);
    /// raw retrieval of rtFloat - with conversion to 64-bit double
    // - use instead GetDoubleValue
    function GetFloatProp(Instance: TObject): double;
    /// raw assignment of rtFloat
    // - use instead SetDoubleValue
    procedure SetFloatProp(Instance: TObject; Value: TSynExtended);
    /// raw retrieval of rtVariant
    procedure GetVariantProp(Instance: TObject; var result: Variant);
    /// raw assignment of rtVariant
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
    // - get the first PPropInfo with RttiProps()^.PropList
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
    function RetrieveFieldSize: integer;
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

    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - return -1 on any error
    function GetOrdValue(Instance: TObject): PtrInt; {$ifdef HASINLINE} inline; {$endif}
    /// low-level getter of the ordinal property value of a given instance
    // - this method will check if the corresponding property is ordinal
    // - ordinal properties smaller than rtInt64 will return an Int64-converted
    // value (e.g. rtInteger)
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
    /// low-level getter of the long string property content of a given instance
    // - just a wrapper around low-level GetLongStrProp() function
    // - call GetLongStrValue() method if you want a conversion into RawUTF8
    // - will work only for Kind=rtLString
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
  end;

const
  NO_DEFAULT = longint($80000000);

{$ifdef ISDELPHI} // Delphi requires those definitions for proper inlining

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

  TPropData = packed record // PPropData not defined in Delphi 7/2007 TypInfo
    PropCount: word;
    PropList: record end;
  end;
  PPropData = ^TPropData;

  TRecordInfo = packed record // rtRecord not defined in Delphi 7/2007 TTypeData
    RecSize: integer;
    ManagedFldCount: integer;
  end;
  PRecordInfo = ^TRecordInfo;

  TArrayInfo = packed record // rtArray not defined in Delphi 7/2007 TTypeData
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
// - just a wrapper around PTypeInfo(aTypeInfo)^.EnumBaseType.GetEnumNameOrd(aIndex)
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


implementation

uses
  TypInfo;

{ some inlined definitions which should be declared before $include code }

function FromRttiOrd(o: TRttiOrd; P: pointer): PtrInt;
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
    {$endif}
  else
    result := 0; // should nro happen
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
    {$endif}
  end;
end;

type
  TGetRttiInterface = class
  public
    Level: integer;
    MethodCount, ArgCount: integer;
    CurrentMethod: PRttiMethod;
    Definition: TRttiInterface;
    procedure AddMethodsFromTypeInfo(aInterface: PTypeInfo);
    procedure AddMethod(const aMethodName: ShortString; aParamCount: integer;
      aKind: TMethodKind);
    procedure AddArgument(aParamName, aTypeName: PShortString; aInfo: PRttiInfo;
      aFlags: TParamFlags);
    procedure RaiseError(const Format: RawUTF8; const Args: array of const);
  end;

const
  PSEUDO_RESULT_NAME: string[6] = 'Result';
  PSEUDO_SELF_NAME: string[4] = 'Self';

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
  result := ORDTYPE_SIZE[RttiOrd]; // MaxValue does not work e.g. with WordBool
end;

function TRttiEnumType.SizeInStorageAsSet: Integer;
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
  with TAbstractWriter.CreateOwnedStream(temp) do
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

procedure TRttiEnumType.GetSetNameCSV(W: TAbstractWriter; Value: cardinal;
  SepChar: AnsiChar; FullSetsAsStar: boolean);
var
  j: integer;
  PS: PShortString;
begin
  W.Add('[');
  if FullSetsAsStar and GetAllBits(Value, MaxValue + 1) then
    W.AddShort('"*"')
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
  W: TAbstractWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TAbstractWriter.CreateOwnedStream(temp);
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

procedure TRttiEnumType.SetEnumFromOrdinal(out Value; Ordinal: Integer);
begin
  ToRttiOrd(RttiOrd, @Value, Ordinal); // MaxValue does not work e.g. with WordBool
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

function TRttiInfo.RttiOrd: TRttiOrd;
begin
  result := TRttiOrd(GetTypeData(@self)^.OrdType);
end;

function TRttiInfo.RttiFloat: TRttiFloat;
begin
  result := TRttiFloat(GetTypeData(@self)^.FloatType);
end;

function TRttiInfo.ClassFieldCount(onlyWithoutGetter: boolean): integer;
begin
  result := ClassFieldCountWithParents(RttiClass^.RttiClass, onlyWithoutGetter);
end;

function TRttiInfo.InheritsFrom(AClass: TClass): boolean;
begin
  result := RttiClass^.InheritsFrom(AClass);
end;

function TRttiInfo.SetEnumType: PRttiEnumType;
begin
  if (@self = nil) or (Kind <> rkSet) then
    result := nil
  else
    result := PRttiEnumType(GetTypeData(@self))^.SetBaseType;
end;

function TRttiInfo.InterfaceType: PRttiInterfaceTypeData;
begin
  result := pointer(GetTypeData(@self));
end;

function TRttiInfo.DynArrayItemSize: integer;
begin
  if DynArrayItemType(@result) = nil then
    result := 0;
end;

function TRttiInfo.AnsiStringCodePage: integer;
begin
  {$ifdef HASCODEPAGE}
  if Kind = rkLString then // has rtLStringOld any codepage? -> UTF-8
    result := PWord(GetTypeData(@self))^
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
    result := @NULL_SHORTSTRING
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

function TRttiProp.RetrieveFieldSize: integer;
var
  info: PRttiInfo;
begin
  info := TypeInfo;
  case info^.Kind of
    rkInteger, rkEnumeration, rkSet, rkChar, rkWChar
    {$ifdef FPC}, rkBool{$endif}:
      result := ORDTYPE_SIZE[info^.RttiOrd];
    rkFloat:
      case info^.RttiFloat of
        rfSingle:
          result := 4;
        rfExtended:
          result := 10;
      else
        result := 8;
      end;
    rkLString, {$ifdef FPC} rkLStringOld, {$endif}
    {$ifdef HASVARUSTRING} rkUString, {$endif}
    rkWString, rkClass, rkInterface, rkDynArray:
      result := SizeOf(pointer);
    rkInt64 {$ifdef FPC}, rkQWord{$endif}:
      result := 8;
    rkVariant:
      result := SizeOf(variant);
  else
    result := 0;
  end;
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

function TRttiProp.GetOrdProp(Instance: TObject): PtrInt;
type
  TGetProc = function: Pointer of object; // pointer result is a PtrInt register
  TGetIndexed = function(Index: Integer): Pointer of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      call.Code := PPointer(call.Data)^;
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
          PPtrInt(call.Data)^ := Value
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
      result := PObject(call.Data)^;
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
      result := PInt64(call.Data)^;
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
      PInt64(call.Data)^ := Value;
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
      PRawByteString(call.Data)^ := Value;
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

procedure TRttiProp.GetShortStrProp(Instance: TObject; var Value: RawByteString);
type
  TGetProc = function: ShortString of object;
  TGetIndexed = function(Index: Integer): ShortString of object;
var
  call: TMethod;
  tmp: ShortString;
begin
  case Getter(Instance, @call) of
    rpcField:
      tmp := PShortString(call.Data)^;
    rpcMethod:
      tmp := TGetProc(call);
    rpcIndexed:
      tmp := TGetIndexed(call)(Index);
  else
    tmp := '';
  end;
  ShortStringToAnsi7String(tmp, RawUTF8(Value));
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
      Value := PWideString(call.Data)^;
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
      PWideString(call.Data)^ := Value;
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
      PUnicodeString(call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

{$endif HASVARUSTRING}

function TRttiProp.GetCurrencyProp(Instance: TObject): currency;
type
  TGetProc = function: currency of object;
  TGetIndexed = function(Index: Integer): currency of object;
var
  call: TMethod;
begin
  case Getter(Instance, @call) of
    rpcField:
      result := PCurrency(call.Data)^;
    rpcMethod:
      result := TGetProc(call);
    rpcIndexed:
      result := TGetIndexed(call)(Index);
  else
    result := 0;
  end;
end;

procedure TRttiProp.SetCurrencyProp(Instance: TObject; const Value: Currency);
type
  TSetProc = procedure(const Value: currency) of object;
  TSetIndexed = procedure(Index: integer; const Value: currency) of object;
var
  call: TMethod;
begin
  case Setter(Instance, @call) of
    rpcField:
      PCurrency(call.Data)^ := Value;
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
      result := unaligned(PDouble(call.Data)^);
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
      unaligned(PDouble(call.Data)^) := Value;
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
  TCurrencyProc = function: Currency of object;
  TCurrencyIndexed = function(Index: Integer): Currency of object;
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
          result := PSingle(call.Data)^;
        rfDouble:
          result := unaligned(PDouble(call.Data)^);
        rfExtended:
          result := PExtended(call.Data)^;
        rfCurr:
          result := PCurrency(call.Data)^;
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
          result := TCurrencyProc(call);
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
          result := TCurrencyIndexed(call)(Index);
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
  TCurrencyProc = procedure(const Value: Currency) of object;
  TCurrencyIndexed = procedure(Index: integer; const Value: Currency) of object;
var
  call: TMethod;
  rf: TRttiFloat;
begin
  Value := 0;
  rf := TypeInfo^.RttiFloat;
  case Setter(Instance, @call) of
    rpcField:
      case rf of
        rfSingle:
          PSingle(call.Data)^ := Value;
        rfDouble:
          unaligned(PDouble(call.Data)^) := Value;
        rfExtended:
          PExtended(call.Data)^ := Value;
        rfCurr:
          PCurrency(call.Data)^ := Value;
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
          TCurrencyProc(call)(Value);
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
          TCurrencyIndexed(call)(Index, Value);
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
      PVariant(call.Data)^ := Value;
    rpcMethod:
      TSetProc(call)(Value);
    rpcIndexed:
      TSetIndexed(call)(Index, Value);
  end;
end;

function TRttiProp.GetOrdValue(Instance: TObject): PtrInt;
begin
  if (Instance <> nil) and (@self <> nil) and
     (TypeInfo^.Kind in [rkInteger, rkEnumeration, rkSet, {$ifdef FPC}rkBool, {$endif} rkClass]) then
    result := GetOrdProp(Instance)
  else
    result := -1;
end;

function TRttiProp.GetInt64Value(Instance: TObject): Int64;
begin
  if (Instance <> nil) and (@self <> nil) then
    case TypeInfo^.Kind of
      rkInteger, rkEnumeration, rkSet, rkChar, rkWChar, rkClass{$ifdef FPC}, rkBool{$endif}:
        result := GetOrdProp(Instance);
      rkInt64{$ifdef FPC}, rkQWord{$endif}:
        result := GetInt64Prop(Instance);
    else
      result := 0;
    end
  else
    result := 0;
end;

function TRttiProp.GetCurrencyValue(Instance: TObject): Currency;
begin
  if (Instance <> nil) and (@self <> nil) then
    with TypeInfo^ do
      if Kind = rkFloat then
        if RttiFloat = rfCurr then
          result := GetCurrencyProp(Instance)
        else
          result := GetFloatProp(Instance)
      else
        result := 0
  else
    result := 0;
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
  if (Instance <> nil) and (@self <> nil) and
     (TypeInfo^.Kind in [rkInteger, rkEnumeration, rkSet, {$ifdef FPC}rkBool, {$endif}rkClass]) then
    SetOrdProp(Instance, Value);
end;

procedure TRttiProp.SetInt64Value(Instance: TObject; Value: Int64);
begin
  if (Instance <> nil) and (@self <> nil) then
    case TypeInfo^.Kind of
      rkInteger, rkEnumeration, rkSet, rkChar, rkWChar, rkClass {$ifdef FPC}, rkBool{$endif}:
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
    [Definition.Name, m, FormatUTF8(Format, Args)]);
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


initialization
  {$ifdef FPC_OR_UNICODE}
  assert(SizeOf(TRttiRecordField) = SizeOf(TManagedField));
  {$endif FPC_OR_UNICODE}
end.

