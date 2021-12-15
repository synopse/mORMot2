/// Framework Core Low-Level Interface/SOLID Processing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.interfaces;

{
  *****************************************************************************

   Implements SOLID Process via Interface types
    - IInvokable Interface Methods and Parameters RTTI Extraction
    - TInterfaceFactory Generating Runtime Implementation Class
    - TInterfaceResolver TInjectableObject for IoC / Dependency Injection
    - TInterfaceStub TInterfaceMock for Dependency Mocking
    - TInterfacedObjectFake with JITted Methods Execution
    - TInterfaceMethodExecute for Method Execution from JSON
    - SetWeak and SetWeakZero Weak Interface Reference

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  {$ifndef FPC}
  typinfo, // for proper Delphi inlining
  {$endif FPC}
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.test, // for TInterfaceMock
  mormot.core.log;


{ ************ IInvokable Interface Methods and Parameters RTTI Extraction }

type
  /// handled kind of parameters for an interface-based service provider method
  // - we do not handle all kind of variables, but provide some enhanced types
  // handled by JsonToObject/ObjectToJson functions (smvObject) or
  // TDynArray.LoadFromJson / TJsonWriter.AddDynArrayJson methods (smvDynArray)
  // - records will be serialized as Base64 string, with our RecordSave/RecordLoad
  // low-level format by default, or as true JSON objects since Delphi 2010 or
  // after a Rtti.RegisterFromText/TRttiJson.RegisterCustomSerializer call
  // - imvRawJson will transmit the raw JSON content, without serialization
  TInterfaceMethodValueType = (
    imvNone,
    imvSelf,
    imvBoolean,
    imvEnum,
    imvSet,
    imvInteger,
    imvCardinal,
    imvInt64,
    imvDouble,
    imvDateTime,
    imvCurrency,
    imvRawUtf8,
    imvString,
    imvRawByteString,
    imvWideString,
    imvBinary,
    imvRecord,
    imvVariant,
    imvObject,
    imvRawJson,
    imvDynArray,
    imvInterface);

  /// handled kind of parameters internal variables for an interface-based method
  // - reference-counted variables will have their own storage
  // - all non referenced-counted variables are stored within some 64-bit content
  // - imvVariant kind of parameter will be handled as a special imvvRecord
  TInterfaceMethodValueVar = (
    imvvNone,
    imvvSelf,
    imvv64,
    imvvRawUtf8,
    imvvString,
    imvvWideString,
    imvvRecord,
    imvvObject,
    imvvDynArray,
    imvvInterface);

  /// set of parameters for an interface-based service provider method
  TInterfaceMethodValueTypes = set of TInterfaceMethodValueType;

  /// handled kind of parameters direction for an interface-based service method
  // - IN, IN/OUT, OUT directions can be applied to arguments, and will
  // be available through our JSON-serialized remote access: smdVar and smdOut
  // kind of parameters will be returned within the "result": JSON array
  // - smdResult is used for a function method, to handle the returned value
  TInterfaceMethodValueDirection = (
    imdConst,
    imdVar,
    imdOut,
    imdResult);

  /// set of parameters direction for an interface-based service method
  TInterfaceMethodValueDirections = set of TInterfaceMethodValueDirection;

  /// set of low-level processing options at assembly level
  // - vIsString is included for imvRawUtf8, imvString, imvRawByteString and
  // imvWideString kind of parameter (imvRecord has it to false, even if they
  // are Base64 encoded within the JSON content, and also imvVariant/imvRawJson)
  // - vPassedByReference is included if the parameter is passed as reference
  // (i.e. defined as var/out, or is a record or a reference-counted type result)
  // - vIsObjArray is set if the dynamic array is a T*ObjArray, so should be
  // cleared with ObjArrClear() and not TDynArray.Clear
  // - vIsSPI indicates that the value contains some Sensitive Personal
  // Information (e.g. a bank card number or a plain password), which type has
  // been previously registered via TInterfaceFactory.RegisterUnsafeSPIType
  // so that low-level logging won't include such values
  // - vIsQword is set for ValueType=imvInt64 over a QWord unsigned 64-bit value
  // - vIsDynArrayString is set for ValueType=imvDynArray of string values
  // - vIsDateTimeMS is set for ValueType=imvDateTime and TDateTimeMS value
  TInterfaceMethodValueAsm = set of (
    vIsString,
    vPassedByReference,
    vIsObjArray,
    vIsSpi,
    vIsQword,
    vIsDynArrayString,
    vIsDateTimeMS);

  /// describe a service provider method argument
  {$ifdef USERECORDWITHMETHODS}
  TInterfaceMethodArgument = record
  {$else}
  TInterfaceMethodArgument = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the argument name, as declared in object pascal
    ParamName: PShortString;
    /// the type name, as declared in object pascal
    ArgTypeName: PShortString;
    /// the low-level RTTI information of this argument
    // - use ArgRtti.Info to retrieve the TypeInfo() of this argument
    ArgRtti: TRttiJson;
    /// we do not handle all kind of object pascal variables
    ValueType: TInterfaceMethodValueType;
    /// the variable direction as defined at code level
    ValueDirection: TInterfaceMethodValueDirection;
    /// how the variable may be stored
    ValueVar: TInterfaceMethodValueVar;
    /// how the variable is to be passed at asm level
    ValueKindAsm: TInterfaceMethodValueAsm;
    /// byte offset in the CPU stack of this argument
    // - may be -1 if pure register parameter with no backup on stack (x86)
    InStackOffset: integer;
    /// used to specify if the argument is passed as register
    // - contains 0 if parameter is not a register
    // - contains 1 for EAX, 2 for EDX and 3 for ECX registers for x86
    // - contains 1 for RCX, 2 for RDX, 3 for R8, and
    // 4 for R9, with a backing store on the stack for x64
    // - contains 1 for R0, 2 R1 ... 4 for R3, with a backing store on the stack for arm
    // - contains 1 for X0, 2 X1 ... 8 for X7, with a backing store on the stack for aarch64
    RegisterIdent: integer;
    /// used to specify if a floating-point argument is passed as register
    // - contains always 0 for x86/x87
    // - contains 1 for XMM0, 2 for XMM1, ..., 4 for XMM3 for x64
    // - contains 1 for D0, 2 for D1, ..., 8 for D7 for armhf
    // - contains 1 for V0, 2 for V1, ..., 8 for V7 for aarch64
    FPRegisterIdent: integer;
    /// size (in bytes) of this argument on the stack
    SizeInStack: integer;
    /// size (in bytes) of this imvv64 ordinal value
    // - e.g. depending of the associated kind of enumeration
    SizeInStorage: integer;
    /// hexadecimal binary size (in bytes) of this imvv64 ordinal value
    // - set only if ValueType=imvBinary
    SizeInBinary: integer;
    /// index of the associated variable in the local array[ArgsUsedCount[]]
    // - for imdConst argument, contains -1 (no need to a local var: the value
    // will be on the stack only)
    IndexVar: integer;
    /// serialize the argument into the TServiceContainer.Contract JSON format
    // - non standard types (e.g. clas, enumerate, dynamic array or record)
    // are identified by their type identifier - so contract does not extend
    // up to the content of such high-level structures
    procedure SerializeToContract(WR: TJsonWriter);
    /// check if the supplied argument value is the default (e.g. 0, '' or null)
    function IsDefault(V: pointer): boolean;
    /// unserialize a JSON value into this argument
    function FromJson(const MethodName: RawUtf8; var R: PUtf8Char; V: pointer;
      Error: PShortString; DVO: TDocVariantOptions): boolean;
    /// append the JSON value corresponding to this argument
    procedure AddJson(WR: TJsonWriter; V: pointer;
      ObjectOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]);
    /// append the value corresponding to this argument as within a JSON string
    // - will escape any JSON string character, and include a pending ','
    procedure AddJsonEscaped(WR: TJsonWriter; V: pointer);
    /// append the JSON value corresponding to this argument, from its text value
    // - includes a pending ','
    procedure AddValueJson(WR: TJsonWriter; const Value: RawUtf8);
    /// append the default JSON value corresponding to this argument
    // - includes a pending ','
    procedure AddDefaultJson(WR: TJsonWriter);
    /// convert a value into its JSON representation
    procedure AsJson(var DestValue: RawUtf8; V: pointer);
    /// convert a value into its variant representation
    // - complex objects will be converted into a TDocVariant, after JSON
    // serialization: variant conversion options may e.g. be retrieve from
    // TInterfaceFactory.DocVariantOptions
    procedure AsVariant(var DestValue: variant; V: pointer;
      Options: TDocVariantOptions);
    /// add a value into a TDocVariant object or array
    // - Dest should already have set its Kind to either dvObject or dvArray
    procedure AddAsVariant(var Dest: TDocVariantData; V: pointer);
    /// normalize a value containing one input or output argument
    // - sets and enumerates will be translated to strings (also in embedded
    // objects and T*ObjArray)
    procedure FixValue(var Value: variant);
    /// normalize a value containing one input or output argument, and add
    // it to a destination variant Document
    // - sets and enumerates will be translated to strings (also in embedded
    // objects and T*ObjArray)
    procedure FixValueAndAddToObject(const Value: variant;
      var DestDoc: TDocVariantData);
  end;

  /// pointer to a service provider method argument
  PInterfaceMethodArgument = ^TInterfaceMethodArgument;

  /// describe a service provider method arguments
  TInterfaceMethodArgumentDynArray = array of TInterfaceMethodArgument;

  /// callback called by TInterfaceMethodExecute to process an interface
  // callback parameter
  // - implementation should set the Obj local variable to an instance of
  // a fake class implementing the aParamInfo interface
  TOnInterfaceMethodExecuteCallback = procedure(var Par: PUtf8Char;
    ParamInterfaceInfo: TRttiJson; out Obj) of object;

  /// how TInterfaceMethod.TInterfaceMethod method will return the generated document
  // - will return either a dvObject or dvArray TDocVariantData, depending on
  // the expected returned document layout
  // - returned content could be "normalized" (for any set or enumerate) if
  // Kind is pdvObjectFixed
  TInterfaceMethodParamsDocVariantKind = (
    pdvArray,
    pdvObject,
    pdvObjectFixed);

  /// describe an interface-based service provider method
  {$ifdef USERECORDWITHMETHODS}
  TInterfaceMethod = record
  {$else}
  TInterfaceMethod = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the method URI, i.e. the method name
    // - as declared in object pascal code, e.g. 'Add' for ICalculator.Add
    // - this property value is hashed internally for faster access
    Uri: RawUtf8;
    /// the method default result, formatted as a JSON array
    // - example of content may be '[]' for a procedure or '[0]' for a function
    // - any var/out and potential function result will be set as a JSON array
    // of values, with 0 for numerical values, "" for textual values,
    // false for booleans, [] for dynamic arrays, a void record serialized
    // as expected (including customized serialization) and null for objects
    DefaultResult: RawUtf8;
    /// the fully qualified dotted method name, including the interface name
    // - as used by TServiceContainerInterfaceMethod.InterfaceDotMethodName
    // - match the URI fullpath name, e.g. 'Calculator.Add'
    InterfaceDotMethodName: RawUtf8;
    /// method index in the original (non emulated) interface
    // - our custom methods start at index 3 (RESERVED_VTABLE_SLOTS), since
    // QueryInterface, _AddRef, and _Release are always defined by default
    // - so it maps TServiceFactory.Interface.Methods[ExecutionMethodIndex-3]
    ExecutionMethodIndex: byte;
    /// TRUE if the method is inherited from another parent interface
    IsInherited: boolean;
    /// the directions of arguments with vIsSpi defined in Args[].ValueKindAsm
    HasSpiParams: TInterfaceMethodValueDirections;
    /// is 0 for the root interface, 1..n for all inherited interfaces
    HierarchyLevel: byte;
    /// describe expected method arguments
    // - Args[0] always is imvSelf
    // - if method is a function, an additional imdResult argument is appended
    Args: TInterfaceMethodArgumentDynArray;
    /// the index of the result pseudo-argument in Args[]
    // - is -1 if the method is defined as a (not a function)
    ArgsResultIndex: shortint;
    /// the index of the first const / var argument in Args[]
    ArgsInFirst: shortint;
    /// the index of the last const / var argument in Args[]
    ArgsInLast: shortint;
    /// the index of the first var / out / result argument in Args[]
    ArgsOutFirst: shortint;
    /// the index of the last var / out / result argument in Args[]
    ArgsOutLast: shortint;
    /// the index of the last argument in Args[], excepting result
    ArgsNotResultLast: shortint;
    /// the index of the last var / out argument in Args[]
    ArgsOutNotResultLast: shortint;
    /// the number of const / var parameters in Args[]
    // - i.e. the number of elements in the input JSON array
    ArgsInputValuesCount: byte;
    /// the number of var / out parameters +  in Args[]
    // - i.e. the number of elements in the output JSON array or object
    ArgsOutputValuesCount: byte;
    /// true if the result is a TServiceCustomAnswer record
    // - that is, a custom Header+Content BLOB transfert, not a JSON object
    ArgsResultIsServiceCustomAnswer: boolean;
    /// true if there is a single input parameter as RawByteString/RawBlob
    // - TRestRoutingRest.ExecuteSoaByInterface will identify binary input
    // with mime-type 'application/octet-stream' as expected
    ArgsInputIsOctetStream: boolean;
    /// the index of the first argument expecting manual stack initialization
    // - set if there is any imvObject,imvDynArray,imvRecord,imvInterface or
    // imvVariant
    ArgsManagedFirst: shortint;
    /// the index of the last argument expecting manual stack initialization
    // - set if there is any imvObject, imvDynArray, imvRecord, imvInterface or
    // imvVariant
    ArgsManagedLast: shortint;
    /// contains all used kind of arguments
    ArgsUsed: TInterfaceMethodValueTypes;
    /// contains the count of variables for all used kind of arguments
    ArgsUsedCount: array[TInterfaceMethodValueVar] of byte;
    /// needed CPU stack size (in bytes) for all arguments
    // - under x64, does not include the backup space for the four registers
    ArgsSizeInStack: cardinal;
    /// retrieve an argument index in Args[] from its name
    // - search is case insensitive
    // - if Input is TRUE, will search within const / var arguments
    // - if Input is FALSE, will search within var / out / result arguments
    // - returns -1 if not found
    function ArgIndex(ArgName: PUtf8Char; ArgNameLen: integer;
      Input: boolean): PtrInt;
    /// find the next input (const / var) argument index in Args[]
    // - returns true if arg is the new value, false otherwise
    function ArgNextInput(var arg: integer): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// find the next output (var / out / result) argument index in Args[]
    // - returns true if arg is the new value, false otherwise
    function ArgNextOutput(var arg: integer): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// convert parameters encoded as a JSON array into a JSON object
    // - if Input is TRUE, will handle const / var arguments
    // - if Input is FALSE, will handle var / out / result arguments
    function ArgsArrayToObject(P: PUtf8Char; Input: boolean): RawUtf8;
    /// convert parameters encoded as name=value or name='"value"' or name='{somejson}'
    // into a JSON object
    // - on Windows, use double-quotes ("") anywhere you expect single-quotes (")
    // - as expected e.g. from a command line tool
    // - if Input is TRUE, will handle const / var arguments
    // - if Input is FALSE, will handle var / out / result arguments
    function ArgsCommandLineToObject(P: PUtf8Char; Input: boolean;
      RaiseExceptionOnUnknownParam: boolean = false): RawUtf8;
    /// returns a dynamic array list of all parameter names
    // - if Input is TRUE, will handle const / var arguments
    // - if Input is FALSE, will handle var / out / result arguments
    function ArgsNames(Input: boolean): TRawUtf8DynArray;
    /// computes a TDocVariant containing the input or output arguments values
    // - Values[] should contain the input/output raw values as variant
    // - Kind will specify the expected returned document layout
    procedure ArgsValuesAsDocVariant(Kind: TInterfaceMethodParamsDocVariantKind;
      out Dest: TDocVariantData; const Values: TVariantDynArray; Input: boolean;
      Options: TDocVariantOptions =
        [dvoReturnNullForUnknownProperty, dvoValueCopiedByReference]);
    /// normalize a TDocVariant containing the input or output arguments values
    // - "normalization" will ensure sets and enums are seralized as text
    // - if Input is TRUE, will handle const / var arguments
    // - if Input is FALSE, will handle var / out / result arguments
    procedure ArgsAsDocVariantFix(var ArgsObject: TDocVariantData; Input: boolean);
    /// convert a TDocVariant array containing the input or output arguments
    // values in order, into an object with named parameters
    // - here sets and enums will keep their current values, mainly numerical
    // - if Input is TRUE, will handle const / var arguments
    // - if Input is FALSE, will handle var / out / result arguments
    procedure ArgsAsDocVariantObject(const ArgsParams: TDocVariantData;
      var ArgsObject: TDocVariantData; Input: boolean);
    /// computes a TDocVariant containing the input or output arguments values
    // - Values[] should point to the input/output raw binary values, as stored
    // in TInterfaceMethodExecute.Values during execution
    procedure ArgsStackAsDocVariant(const Values: TPPointerDynArray;
      out Dest: TDocVariantData; Input: boolean);
  end;

  /// describe all mtehods of an interface-based service provider
  TInterfaceMethodDynArray = array of TInterfaceMethod;

  /// a pointer to an interface-based service provider method description
  // - since TInterfaceFactory instances are shared in a global list, we
  // can safely use such pointers in our code to refer to a particular method
  PInterfaceMethod = ^TInterfaceMethod;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TServiceMethodValueType = TInterfaceMethodValueType;
  TServiceMethodValueTypes = TInterfaceMethodValueTypes;
  TServiceMethodValueVar = TInterfaceMethodValueVar;
  TServiceMethodValueDirection = TInterfaceMethodValueDirection;
  TServiceMethodValueDirections = TInterfaceMethodValueDirections;
  TServiceMethodArgument = TInterfaceMethodArgument;
  PServiceMethodArgument = PInterfaceMethodArgument;
  TServiceMethodArgumentDynArray = TInterfaceMethodArgumentDynArray;
  TServiceMethodParamsDocVariantKind = TInterfaceMethodParamsDocVariantKind;
  TServiceMethod = TInterfaceMethod;
  TServiceMethodDynArray = TInterfaceMethodDynArray;
  PServiceMethod = PInterfaceMethod;

const
  // TServiceMethodValueType = TInterfaceMethodValueType items
  smvNone          = imvNone;
  smvSelf          = imvSelf;
  smvBoolean       = imvBoolean;
  smvEnum          = imvEnum;
  smvSet           = imvSet;
  smvInteger       = imvInteger;
  smvCardinal      = imvCardinal;
  smvInt64         = imvInt64;
  smvDouble        = imvDouble;
  smvDateTime      = imvDateTime;
  smvCurrency      = imvCurrency;
  smvRawUtf8       = imvRawUtf8;
  smvString        = imvString;
  smvRawByteString = imvRawByteString;
  smvWideString    = imvWideString;
  smvBinary        = imvBinary;
  smvRecord        = imvRecord;
  smvVariant       = imvVariant;
  smvObject        = imvObject;
  smvRawJson       = imvRawJson;
  smvDynArray      = imvDynArray;
  smvInterface     = imvInterface;
  // TServiceMethodValueVar = TInterfaceMethodValueVar items
  smvvNone       = imvvNone;
  smvvSelf       = imvvSelf;
  smvv64         = imvv64;
  smvvRawUtf8    = imvvRawUtf8;
  smvvString     = imvvString;
  smvvWideString = imvvWideString;
  smvvRecord     = imvvRecord;
  smvvObject     = imvvObject;
  smvvDynArray   = imvvDynArray;
  smvvInterface  = imvvInterface;
  // TServiceMethodValueDirection = TInterfaceMethodValueDirection items
  smdConst  = imdConst;
  smdVar    = imdVar;
  smdOut    = imdOut;
  smdResult = imdResult;

{$endif PUREMORMOT2}


{ ************  TInterfaceFactory Generating Runtime Implementation Class }

const
  /// maximum number of methods handled by interfaces
  // - if you think this constant is too low, you are about to break
  // the "Interface Segregation" SOLID principle: so don't ask to increase
  // this value, we won't allow to write un-SOLID code! :)
  MAX_METHOD_COUNT = 128;

  /// maximum number of method arguments handled by interfaces
  // - if you consider this as a low value, you should better define some
  // records/classes as DTOs instead of multiplicating parameters: so don't
  // ask to increase this value, we rather encourage writing clean code
  // - used e.g. to avoid creating dynamic arrays if not needed, and
  // ease method calls
  MAX_METHOD_ARGS = 32;

  /// IInterface QueryInterface, _AddRef and _Release methods are hard-coded
  RESERVED_VTABLE_SLOTS = 3;

type
  /// internal pseudo methods when an interface is used as remote service
  // - match TInterfaceFactory MethodIndex 0..3
  TServiceInternalMethod = (
    imFree,
    imContract,
    imSignature,
    imInstance);

const
  /// URI of some pseudo methods when an interface is used as remote service
  // - match TInterfaceFactory MethodIndex 0..3
  SERVICE_PSEUDO_METHOD: array[TServiceInternalMethod] of RawUtf8 = (
    '_free_',
    '_contract_',
    '_signature_',
    '_instance_');

type
  {$M+}
  TInterfaceFactory = class;
  {$M-}

  /// exception dedicated to interface factory, used e.g. for services and mock/stubs
  EInterfaceFactory = class(ESynException);

  /// may be used to store the Methods[] indexes of a TInterfaceFactory
  // - current implementation handles up to 128 methods, a limit above
  // which "Interface Segregation" principle is obviously broken
  TInterfaceFactoryMethodBits = set of 0 .. MAX_METHOD_COUNT - 1;

  /// a dynamic array of TInterfaceFactory instances
  TInterfaceFactoryObjArray = array of TInterfaceFactory;

  /// class handling interface RTTI and fake implementation class
  // - an internal JIT compiler will generate the raw asm opcodes to redirect
  // any interface execution into a fake class
  // - a thread-safe global list of such class instances is implemented to cache
  // information for better speed: use class function TInterfaceFactory.Get()
  // and not manual TInterfaceFactory.Create / Free
  // - if you want to search the interfaces by name or TGUID, call once
  // Get(TypeInfo(IMyInterface)) or RegisterInterfaces() for proper registration
  // - will use TInterfaceFactoryRtti classes generated from compiler RTTI
  TInterfaceFactory = class
  protected
    fInterfaceTypeInfo: PRttiInfo;
    fInterfaceIID: TGUID;
    fInterfaceRtti: TRttiJson;
    fMethodsCount: cardinal;
    fAddMethodsLevel: integer;
    fMethods: TInterfaceMethodDynArray;
    fMethod: TDynArrayHashed;
    // contains e.g. [{"method":"Add","arguments":[...]},{"method":"...}]
    fContract: RawUtf8;
    fInterfaceName: RawUtf8;
    fInterfaceUri: RawUtf8;
    fDocVariantOptions: TDocVariantOptions;
    {$ifdef CPUX86}  // i386 stub requires "ret ArgsSizeInStack"
    fFakeVTable: array of pointer;
    {$endif CPUX86}
    fMethodIndexCallbackReleased: integer;
    fMethodIndexCurrentFrameCallback: integer;
    procedure AddMethodsFromTypeInfo(aInterface: PRttiInfo); virtual; abstract;
    // low-level JIT redirection of the VMT to TInterfacedObjectFake.FakeCall
    function GetMethodsVirtualTable: pointer;
  public
    /// this is the main entry point to the global interface factory cache
    // - access to this method is thread-safe
    // - this method will also register the class to further retrieval
    class function Get(aInterface: PRttiInfo): TInterfaceFactory; overload;
    /// retrieve an interface factory from cache, from its TGUID
    // - access to this method is thread-safe
    // - you shall have registered the interface by a previous call to the
    // overloaded Get(TypeInfo(IMyInterface)) method or RegisterInterfaces()
    // - if the supplied TGUID has not been previously registered, returns nil
    {$ifdef FPC_HAS_CONSTREF}
    class function Get(constref aGuid: TGUID): TInterfaceFactory; overload;
    {$else}
    class function Get(const aGuid: TGUID): TInterfaceFactory; overload;
    {$endif FPC_HAS_CONSTREF}
    /// retrieve an interface factory from cache, from its name (e.g. 'IMyInterface')
    // - access to this method is thread-safe
    // - you shall have registered the interface by a previous call to the
    // overloaded Get(TypeInfo(IMyInterface)) method or RegisterInterfaces()
    // - if the supplied TGUID has not been previously registered, returns nil
    class function Get(const aInterfaceName: RawUtf8): TInterfaceFactory; overload;
    /// register one or several interfaces to the global interface factory cache
    // - so that you can use TInterfaceFactory.Get(aGuid) or Get(aName)
    class procedure RegisterInterfaces(const aInterfaces: array of PRttiInfo);
    /// could be used to retrieve an array of TypeInfo() from their GUID
    class function Guid2TypeInfo(const aGuids: array of TGUID): PRttiInfoDynArray; overload;
    /// could be used to retrieve an array of TypeInfo() from their GUID
    class function Guid2TypeInfo(const aGuid: TGUID): PRttiInfo; overload;
    /// returns the list of all declared TInterfaceFactory
    // - as used by SOA and mocking/stubing features of this unit
    class function GetUsedInterfaces: TSynObjectListLightLocked;
    /// add some TInterfaceFactory instances from their GUID
    class procedure AddToObjArray(var Obj: TInterfaceFactoryObjArray;
      const aGuids: array of TGUID);
    /// register some TypeInfo() containing unsafe parameter values
    // - i.e. any RTTI type containing Sensitive Personal Information, e.g.
    // a bank card number or a plain password
    // - such values will force associated values to be ignored during loging,
    // as a more tuned alternative to optNoLogInput or optNoLogOutput
    class procedure RegisterUnsafeSpiType(const Types: array of PRttiInfo);

    /// initialize the internal properties from the supplied interface RTTI
    // - it will check and retrieve all methods of the supplied interface,
    // and prepare all internal structures for later use
    // - do not call this constructor directly, but TInterfaceFactory.Get()
    constructor Create(aInterface: PRttiInfo);
    /// find the index of a particular method in internal Methods[] list
    // - will search for a match against Methods[].Uri property
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will return -1 if the method is not known
    // - if aMethodName does not have an exact method match, it will try with a
    // trailing underscore, so that e.g. /service/start will match IService._Start()
    function FindMethodIndex(const aMethodName: RawUtf8): integer;
    /// find a particular method in internal Methods[] list
    // - just a wrapper around FindMethodIndex() returing a PInterfaceMethod
    // - will return nil if the method is not known
    function FindMethod(const aMethodName: RawUtf8): PInterfaceMethod;
    /// find the index of a particular interface.method in internal Methods[] list
    // - will search for a match against Methods[].InterfaceDotMethodName property
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will return -1 if the method is not known
    function FindFullMethodIndex(const aFullMethodName: RawUtf8;
      alsoSearchExactMethodName: boolean = false): integer;
    /// find the index of a particular method in internal Methods[] list
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will raise an EInterfaceFactory if the method is not known
    function CheckMethodIndex(const aMethodName: RawUtf8): integer; overload;
    /// find the index of a particular method in internal Methods[] list
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will raise an EInterfaceFactory if the method is not known
    function CheckMethodIndex(aMethodName: PUtf8Char): integer; overload;
    /// returns the method name from its method index
    // - the method index should start at 0 for _free_/_contract_/_signature_
    // pseudo-methods, and start at index 3 for real Methods[]
    function GetMethodName(MethodIndex: integer): RawUtf8;
    /// set the Methods[] indexes bit from some methods names
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will raise an EInterfaceFactory if the method is not known
    procedure CheckMethodIndexes(const aMethodName: array of RawUtf8;
      aSetAllIfNone: boolean; out aBits: TInterfaceFactoryMethodBits);
    /// returns the full 'Interface.MethodName' text, from a method index
    // - the method index should start at 0 for _free_/_contract_/_signature_
    // pseudo-methods, and start at index 3 for real Methods[]
    // - will return plain 'Interface' text, if aMethodIndex is incorrect
    function GetFullMethodName(aMethodIndex: integer): RawUtf8;
    /// the declared internal methods
    // - list does not contain default AddRef/Release/QueryInterface methods
    // - nor the _free_/_contract_/_signature_ pseudo-methods
    property Methods: TInterfaceMethodDynArray
      read fMethods;
    /// the number of internal methods
    // - does not include the default AddRef/Release/QueryInterface methods
    // - nor the _free_/_contract_/_signature_ pseudo-methods
    property MethodsCount: cardinal
      read fMethodsCount;
    /// identifies a CallbackReleased() method in this interface
    // - i.e. the index in Methods[] of the following signature:
    // ! procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUtf8);
    // - this method will be called e.g. by TInterfacedCallback.Destroy, when
    // a callback is released on the client side so that you may be able e.g. to
    // unsubscribe the callback from an interface list (via InterfaceArrayDelete)
    // - contains -1 if no such method do exist in the interface definition
    property MethodIndexCallbackReleased: integer
      read fMethodIndexCallbackReleased;
    /// identifies a CurrentFrame() method in this interface
    // - i.e. the index in Methods[] of the following signature:
    // ! procedure CurrentFrame(isLast: boolean);
    // - this method will be called e.g. by TRestHttpClientWebsockets.CallbackRequest
    // for interface callbacks in case of WebSockets jumbo frames, to allow e.g.
    // faster database access via a batch
    // - contains -1 if no such method do exist in the interface definition
    property MethodIndexCurrentFrameCallback: integer
      read fMethodIndexCurrentFrameCallback;
    /// the registered Interface low-level compiler RTTI type
    property InterfaceTypeInfo: PRttiInfo
      read fInterfaceTypeInfo;
    /// the registered Interface GUID
    property InterfaceIID: TGUID
      read fInterfaceIID;
    /// the interface name, without its initial 'I'
    // - e.g. ICalculator -> 'Calculator'
    property InterfaceUri: RawUtf8
      read fInterfaceUri write fInterfaceUri;
    /// the registered Interface high-level compiler RTTI type
    property InterfaceRtti: TRttiJson
      read fInterfaceRtti;
    /// the service contract as a JSON array
    property Contract: RawUtf8
      read fContract;
    /// how this interface will work with variants (including TDocVariant)
    // - by default, contains JSON_FAST for best performance - i.e.
    // [dvoReturnNullForUnknownProperty,dvoValueCopiedByReference]
    property DocVariantOptions: TDocVariantOptions
      read fDocVariantOptions write fDocVariantOptions;
  published
    /// will return the interface name, e.g. 'ICalculator'
    // - published property to be serializable as JSON e.g. for debbuging info
    property InterfaceName: RawUtf8
      read fInterfaceName;
  end;
  PInterfaceFactory = ^TInterfaceFactory;

  {$ifdef HASINTERFACERTTI}

  /// class handling interface RTTI and fake implementation class
  // - this class only exists for Delphi 6 and up, and newer FPC, which has
  // the expected RTTI - see http://bugs.freepascal.org/view.php?id=26774
  TInterfaceFactoryRtti = class(TInterfaceFactory)
  protected
    procedure AddMethodsFromTypeInfo(aInterface: PRttiInfo); override;
  end;

  {$endif HASINTERFACERTTI}

  /// class handling interface implementation generated from source
  // - this class targets oldest FPC, which did not generate the expected RTTI -
  // see http://bugs.freepascal.org/view.php?id=26774
  // - mormot.soa.codegen.pas will generate a new inherited class, overriding
  // abstract AddMethodsFromTypeInfo() to define the interface methods
  TInterfaceFactoryGenerated = class(TInterfaceFactory)
  protected
    fTempStrings: TRawUtf8DynArray;
    /// the overriden AddMethodsFromTypeInfo() method will call e.g. as
    // ! AddMethod('Add',[
    // !   0,'n1',TypeInfo(integer),
    // !   0,'n2',TypeInfo(integer),
    // !   3,'result',TypeInfo(integer)]);
    // with 0=ord(imdConst) and 3=ord(imdResult)
    procedure AddMethod(const aName: RawUtf8; const aParams: array of const); virtual;
  public
    /// register one interface type definition from the current class
    // - will be called by mormot.soa.codegen generated code, in initialization
    // section, so that the needed type information will be available
    class procedure RegisterInterface(aInterface: PRttiInfo); virtual;
  end;

  /// a record type to be used as result for a function method for custom content
  // for interface-based services
  // - all answers are pure JSON object by default: using this kind of record
  // as result will allow a response of any type (e.g. binary, HTML or text)
  // - this kind of answer will be understood by our TServiceContainerClient
  // implementation, and it may be used with plain AJAX or HTML requests
  // (via POST), to retrieve some custom content
  TServiceCustomAnswer = record
    /// mandatory response type, as encoded in the HTTP header
    // - set the response mime-type - use e.g. JSON_CONTENT_TYPE_HEADER_VAR
    // TEXT_CONTENT_TYPE_HEADER or BINARY_CONTENT_TYPE_HEADER constants or
    // GetMimeContentType() function
    // - if this field is not set, then JSON_CONTENT_TYPE_HEADER will be forced
    Header: RawUtf8;
    /// the response body
    // - corresponding to the response type, as defined in Header
    Content: RawByteString;
    /// the HTTP response code
    // - if not overriden, will default to HTTP_SUCCESS = 200 on server side
    // - on client side, will always contain HTTP_SUCCESS = 200 on success,
    // or any error should be handled as expected by the caller (e.g. using
    // TServiceFactoryClient.GetErrorMessage for decoding REST/SOA errors)
    Status: cardinal;
  end;

  PServiceCustomAnswer = ^TServiceCustomAnswer;


/// returns the interface name of a registered GUID, or its hexadecimal value
function ToText({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}
  aGuid: TGUID): ShortString; overload;

/// low-level function to retrieve the class instance implementing a given interface
// - this will work with interfaces stubs generated by the compiler, but also
// with TInterfaceFactory.CreateFakeInstance kind of classes
// - returns nil if aValue is nil or not recognized
function ObjectFromInterface(const aValue: IInterface): TObject;
  {$ifdef HASINTERFACEASTOBJECT} inline; {$endif}

/// low-level function to check if a class instance, retrieved from its
// interface variable, does in fact implement a given interface
// - this will call ObjectFromInterface(), so will work with interfaces
// stubs generated by the compiler, but also with
// TInterfaceFactory.CreateFakeInstance kind of classes
function ObjectFromInterfaceImplements(const aValue: IInterface;
  const aInterface: TGUID): boolean;



{ ************ TInterfaceResolver TInjectableObject for IoC / Dependency Injection  }

type
  /// exception raised in case of Dependency Injection (aka IoC) issue
  EInterfaceResolver = class(ESynException);

  {$M+}
  /// abstract factory class allowing to call interface resolution in cascade
  // - you can inherit from this class to chain the TryResolve() calls so
  // that several kind of implementations may be asked by a TInjectableObject,
  // e.g. TInterfaceStub, TServiceContainer or TDDDRepositoryRestObjectMapping
  // - this will implement factory pattern, as a safe and thread-safe DI/IoC
  TInterfaceResolver = class
  protected
    /// override this method to resolve an interface from this instance
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; virtual; abstract;
  public
    /// override this method check if this instance implements aInterface RTTI
    // - this default implementation will call TryResolve() on a local IInterface
    // which is somewhat slow, and should better be overriden
    function Implements(aInterface: PRttiInfo): boolean; virtual;
    /// can be used to perform an DI/IoC for a given interface
    // - will search for the supplied interface to its internal list of resolvers
    // - returns TRUE and set the Obj variable with a matching instance
    // - can be used as such to resolve an ICalculator interface:
    // ! var calc: ICalculator;
    // ! begin
    // !   if Catalog.Resolve(TypeInfo(ICalculator),calc) then
    // !   ... use calc methods
    function Resolve(aInterface: PRttiInfo; out Obj): boolean; overload;
    /// can be used to perform an DI/IoC for a given interface
    // - you shall have registered the interface TGUID by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(ICalculator),...])
    // - returns TRUE and set the Obj variable with a matching instance
    // - returns FALSE (or raise aRaiseIfNotFound) if aGuid is not available
    // - can be used as such to resolve an ICalculator interface:
    // ! var calc: ICalculator;
    // ! begin
    // !   if ServiceContainer.Resolve(ICalculator,cal) then
    // !   ... use calc methods
    function Resolve(const aGuid: TGUID; out Obj;
      aRaiseIfNotFound: EInterfaceResolver = nil): boolean; overload;
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TypeInfo,@Instance pairs
    // - raise an EServiceException if any interface can't be resolved, unless
    // aRaiseExceptionIfNotFound is set to FALSE
    procedure ResolveByPair(const aInterfaceObjPairs: array of pointer;
      aRaiseExceptionIfNotFound: boolean = true);
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TGUID and @Instance
    // - you shall have registered the interface TGUID by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(ICalculator),...])
    // - raise an EServiceException if any interface can't be resolved, unless
    // aRaiseExceptionIfNotFound is set to FALSE
    procedure Resolve(const aInterfaces: array of TGUID;
                      const aObjs: array of pointer;
      aRaiseExceptionIfNotFound: boolean = true); overload;
  end;
  {$M-}

  /// used to store a list of TInterfacedObject instances
  TInterfacedObjectObjArray = array of TInterfacedObject;

  /// used to store a list of TInterfaceResolver instances
  TInterfaceResolverObjArray = array of TInterfaceResolver;

  /// abstract factory class targetting a single kind of interface
  TInterfaceResolverForSingleInterface = class(TInterfaceResolver)
  protected
    fInterfaceTypeInfo: PRttiInfo;
    fInterfaceAncestors: PRttiInfoDynArray;
    fInterfaceAncestorsImplementationEntry: TPointerDynArray;
    fImplementationEntry: PInterfaceEntry;
    fImplementation: TRttiCustom;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
    function GetImplementationName: RawUtf8;
    // main IoC/DI virtual method - call fImplementation.CreateNew by default
    function CreateInstance: TInterfacedObject; virtual;
  public
    /// this overriden constructor will check and store the supplied class
    // to implement an interface
    constructor Create(aInterface: PRttiInfo;
      aImplementation: TInterfacedObjectClass); overload;
    /// this overriden constructor will check and store the supplied class
    // to implement an interface by TGUID
    constructor Create(const aInterface: TGUID;
      aImplementation: TInterfacedObjectClass); overload;
    /// you can use this method to resolve the interface as a new instance
    function GetOneInstance(out Obj): boolean;
    /// check if can resolve the supplied interface RTTI
    function Implements(aInterface: PRttiInfo): boolean; override;
  published
    /// the class name which will implement each repository instance
    property ImplementationClass: RawUtf8
      read GetImplementationName;
  end;

type
  /// how TInterfaceResolverList store one interface/class
  TInterfaceResolverListEntry = record
    /// contains TypeInfo(ISomeInterface)
    TypeInfo: PRttiInfo;
    /// the associated RTTI - mainly used to call its ClassNewInstance method
    ImplementationClass: TRttiCustom;
    /// low-level interface VMT information for fast creation
    InterfaceEntry: PInterfaceEntry;
    /// shared instance
    // - will be released with the TInterfaceResolverListEntries array
    Instance: IInterface;
  end;
  PInterfaceResolverListEntry = ^TInterfaceResolverListEntry;

  /// how TInterfaceResolverList store one interface/class
  TInterfaceResolverListEntries = array of TInterfaceResolverListEntry;

  /// event signature used by TInterfaceResolverList.OnCreateInstance
  TOnResolverCreateInstance = procedure(
    Sender: TInterfaceResolver; Instance: TInterfacedObject) of object;

  /// register a thread-safe list of classes to implement some interfaces
  // - as used e.g. by TInterfaceResolverInjected.RegisterGlobal()
  TInterfaceResolverList = class(TInterfaceResolver)
  protected
    fEntry: TInterfaceResolverListEntries;
    fSafe: TRWLightLock;
    fOnCreateInstance: TOnResolverCreateInstance;
    function PrepareAddAndWriteLock(aInterface: PRttiInfo;
      aImplementationClass: TClass): PInterfaceEntry; // fSafe.WriteUnLock after
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
  public
    /// check if a given interface can be resolved, from its RTTI
    function Implements(aInterface: PRttiInfo): boolean; override;
    /// register a given implementaiton class for an interface
    // - a new aImplementationClass instance will be created for each resolution
    procedure Add(aInterface: PRttiInfo;
      aImplementationClass: TInterfacedObjectClass); overload;
    /// register a given implementation class instance for an interface
    // - the shared aImplementation instance will be returned for each resolution
    // - aImplementation will be owned by the internal registration list
    procedure Add(aInterface: PRttiInfo;
      aImplementation: TInterfacedObject); overload;
    /// unregister a given implementation class for an interface
    // - raise EInterfaceResolver if an TInterfacedObject instance was registered
    procedure Delete(aInterface: PRttiInfo);
    /// is called when a new aImplementationClass instance has been created
    property OnCreateInstance: TOnResolverCreateInstance
      read fOnCreateInstance write fOnCreateInstance;
    /// low-level access to the internal registered interface/class list
    // - should be protected via the Safe locking methods
    property Entry: TInterfaceResolverListEntries
      read fEntry;
    /// low-level access to the internal lock for thread-safety
    property Safe: TRWLightLock
      read fSafe;
  end;

  /// abstract factory class targetting any kind of interface
  // - you can inherit from this class to customize dependency injection (DI/IoC),
  // defining the resolution via InjectStub/InjectResolver/InjectInstance methods,
  // and doing the instance resolution using the overloaded Resolve*() methods
  // - TServiceContainer will inherit from this class, as the main entry point
  // for interface-based services of the framework (via TRest.Services)
  // - you can use RegisterGlobal() class method to define some process-wide DI
  TInterfaceResolverInjected = class(TInterfaceResolver)
  protected
    fResolvers: TInterfaceResolverObjArray;
    fResolversToBeReleased: TInterfaceResolverObjArray;
    fDependencies: TInterfacedObjectObjArray;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
  public
    /// define a global class type for interface resolution
    // - most of the time, you will need a local DI/IoC resolution list; but
    // you may use this method to register a set of shared and global resolution
    // patterns, common to the whole injection process
    // - by default, TAutoLocker and TLockedDocVariant will be registered by
    // this unit to implement IAutoLocker and ILockedDocVariant interfaces
    class procedure RegisterGlobal(aInterface: PRttiInfo;
      aImplementationClass: TInterfacedObjectClass); overload;
    /// define a global instance for interface resolution
    // - most of the time, you will need a local DI/IoC resolution list; but
    // you may use this method to register a set of shared and global resolution
    // patterns, common to the whole injection process
    // - the supplied instance will be owned by the global list (incrementing
    // its internal reference count), until it will be released via
    // ! RegisterGlobalDelete()
    // - the supplied instance will be freed in the finalization of this unit,
    // if not previously released via RegisterGlobalDelete()
    class procedure RegisterGlobal(aInterface: PRttiInfo;
      aImplementation: TInterfacedObject); overload;
    /// undefine a global instance for interface resolution
    // - you can unregister a given instance previously defined via
    // ! RegisterGlobal(aInterface,aImplementation)
    // - if you do not call RegisterGlobalDelete(), the remaning instances will
    // be freed in the finalization of this unit
    class procedure RegisterGlobalDelete(aInterface: PRttiInfo);
    /// prepare and setup interface DI/IoC resolution with some blank
    // TInterfaceStub specified by their TGUID
    procedure InjectStub(const aStubsByGUID: array of TGUID); overload; virtual;
    /// prepare and setup interface DI/IoC resolution with TInterfaceResolver
    // kind of factory
    // - e.g. a customized TInterfaceStub/TInterfaceMock, a TServiceContainer,
    // a TDDDRepositoryRestObjectMapping or any factory class
    // - by default, only TInterfaceStub/TInterfaceMock will be owned by this
    // instance, and released by Destroy - unless you set OwnOtherResolvers
    procedure InjectResolver(const aOtherResolvers: array of TInterfaceResolver;
      OwnOtherResolvers: boolean = false); overload; virtual;
    /// prepare and setup interface DI/IoC resolution from a TInterfacedObject instance
    // - any TInterfacedObject declared as dependency will have its reference
    // count increased, and decreased in Destroy
    procedure InjectInstance(const aDependencies: array of TInterfacedObject);
      overload; virtual;
    /// check if a given interface can be resolved, from its RTTI
    function Implements(aInterface: PRttiInfo): boolean; override;
    /// release all used instances
    // - including all TInterfaceStub instances as specified to Inject(aStubsByGUID)
    // - will call _Release on all TInterfacedObject dependencies
    destructor Destroy; override;
  end;

  /// any service implementation class could inherit from this class to
  // allow dependency injection aka SOLID DI/IoC by the framework
  // - once created, the framework will call AddResolver() member, so that its
  // Resolve*() methods could be used to inject any needed dependency for lazy
  // dependency resolution (e.g. within a public property getter)
  // - any interface published property will also be automatically injected
  // - if you implement a SOA service with this class, TRestServer.Services
  // will be auto-injected via TServiceFactoryServer.CreateInstance()
  TInjectableObject = class(TInterfacedObjectWithCustomCreate)
  protected
    fResolver: TInterfaceResolver;
    fResolverOwned: boolean;
    fRtti: TRttiCustom;
    // DI/IoC resolution protected methods
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean;
    /// this method will resolve all interface published properties
    procedure AutoResolve(aRaiseEServiceExceptionIfNotFound: boolean);
  public
    /// initialize an instance, defining one or several mean of dependency resolution
    // - simple TInterfaceStub could be created directly from their TGUID,
    // then any kind of DI/IoC resolver instances could be specified, i.e.
    // either customized TInterfaceStub/TInterfaceMock, a TServiceContainer or
    // a TDDDRepositoryRestObjectMapping, and then any TInterfacedObject
    // instance will be used during dependency resolution:
    // ! procedure TMyTestCase.OneTestCaseMethod;
    // ! var Test: IServiceToBeTested;
    // ! begin
    // !   Test := TServiceToBeTested.CreateInjected(
    // !     [ICalculator],
    // !     [TInterfaceMock.Create(IPersistence,self).
    // !       ExpectsCount('SaveItem',qoEqualTo,1),
    // !      RestInstance.Services],
    // !     [AnyInterfacedObject]);
    // !   ...
    // - note that all the injected stubs/mocks instances will be owned by the
    // TInjectableObject, and therefore released with it
    // - any TInterfacedObject declared as dependency will have its reference
    // count increased, and decreased in Destroy
    // - once DI/IoC is defined, will call the AutoResolve() protected method
    constructor CreateInjected(const aStubsByGUID: array of TGUID;
      const aOtherResolvers: array of TInterfaceResolver;
      const aDependencies: array of TInterfacedObject;
      aRaiseEServiceExceptionIfNotFound: boolean = true); virtual;
    /// initialize an instance, defining one dependency resolver
    // - the resolver may be e.g. a TServiceContainer
    // - once the DI/IoC is defined, will call the AutoResolve() protected method
    // - as called by  TServiceFactoryServer.CreateInstance
    constructor CreateWithResolver(aResolver: TInterfaceResolver;
      aRaiseEServiceExceptionIfNotFound: boolean = true); virtual;
    /// can be used to perform an DI/IoC for a given interface type information
    procedure Resolve(aInterface: PRttiInfo; out Obj); overload;
    /// can be used to perform an DI/IoC for a given interface TGUID
    procedure Resolve(const aGuid: TGUID; out Obj); overload;
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TypeInfo,@Instance pairs
    procedure ResolveByPair(const aInterfaceObjPairs: array of pointer);
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TGUID and pointers
    procedure Resolve(const aInterfaces: array of TGUID;
      const aObjs: array of pointer); overload;
    /// release all used instances
    // - including all TInterfaceStub instances as specified to CreateInjected()
    destructor Destroy; override;
    /// access to the associated dependency resolver, if any
    property Resolver: TInterfaceResolver
      read fResolver;
  end;

  /// class-reference type (metaclass) of a TInjectableObject type
  TInjectableObjectClass = class of TInjectableObject;

var
  /// global thread-safe list for process-wide interfaces resolution
  // - TInterfaceResolverInjected.RegisterGlobal/RegisterGlobalDelete
  // class methods redirect to GlobalInterfaceResolver.Add/Delete
  // - initialization section of this unit will make at startup:
  // ! GlobalInterfaceResolver.Add(TypeInfo(IAutoLocker), TAutoLocker);
  // ! GlobalInterfaceResolver.Add(TypeInfo(ILockedDocVariant), TLockedDocVariant);
  GlobalInterfaceResolver: TInterfaceResolverList;


{ ************ TInterfaceStub TInterfaceMock for Dependency Mocking }

type
  TInterfaceStub = class;

  /// Exception class raised during dependency stubing/mocking process
  EInterfaceStub = class(EInterfaceFactory)
  public
    constructor Create(Sender: TInterfaceStub; const Method: TInterfaceMethod;
      const Error: RawUtf8); overload;
    constructor Create(Sender: TInterfaceStub; const Method: TInterfaceMethod;
      const Format: RawUtf8; const Args: array of const); overload;
  end;


  /// abstract parameters used by TInterfaceStub.Executes() events callbacks
  TOnInterfaceStubExecuteParamsAbstract = class
  protected
    fSender: TInterfaceStub;
    fMethod: PInterfaceMethod;
    fParams: RawUtf8;
    fEventParams: RawUtf8;
    fResult: RawUtf8;
    fFailed: boolean;
    function GetSenderAsMockTestCase: TSynTestCase;
  public
    /// constructor of one parameters marshalling instance
    constructor Create(aSender: TInterfaceStub; aMethod: PInterfaceMethod;
      const aParams, aEventParams: RawUtf8); virtual;
    /// call this method if the callback implementation failed
    procedure Error(const aErrorMessage: RawUtf8); overload;
    /// call this method if the callback implementation failed
    procedure Error(const Format: RawUtf8; const Args: array of const); overload;
    /// the stubbing / mocking generator
    property Sender: TInterfaceStub
      read fSender;
    /// the mocking generator associated test case
    // - will raise an exception if the associated Sender generator is not
    // a TInterfaceMock
    property TestCase: TSynTestCase
      read GetSenderAsMockTestCase;
    /// pointer to the method which is to be executed
    property Method: PInterfaceMethod
      read fMethod;
    /// a custom message, defined at TInterfaceStub.Executes() definition
    property EventParams: RawUtf8
      read fEventParams;
    /// outgoing values array encoded as JSON
    // - every var, out parameter or the function result shall be encoded as
    // a JSON array into this variable, in the same order than the stubbed
    // method declaration
    // - use Returns() method to create the JSON array directly, from an array
    // of values
    property result: RawUtf8
      read fResult;
    /// low-level flag, set to TRUE if one of the Error() method was called
    property Failed: boolean
      read fFailed;
  end;

  /// parameters used by TInterfaceStub.Executes() events callbacks as Variant
  // - this class will expect input and output parameters to specified as
  // variant arrays properties, so is easier (and a bit slower) than the
  // TOnInterfaceStubExecuteParamsJson class
  TOnInterfaceStubExecuteParamsVariant = class(TOnInterfaceStubExecuteParamsAbstract)
  protected
    fInput: TVariantDynArray;
    fOutput: TVariantDynArray;
    function GetInput(Index: integer): variant;
    procedure SetOutput(Index: integer; const Value: variant);
    function GetInNamed(const aParamName: RawUtf8): variant;
    procedure SetOutNamed(const aParamName: RawUtf8; const Value: variant);
    function GetInUtf8(const ParamName: RawUtf8): RawUtf8;
    procedure SetResultFromOutput;
  public
    /// constructor of one parameters marshalling instance
    constructor Create(aSender: TInterfaceStub; aMethod: PInterfaceMethod;
      const aParams, aEventParams: RawUtf8); override;
    /// returns the input parameters as a TDocVariant object or array
    function InputAsDocVariant(Kind: TInterfaceMethodParamsDocVariantKind;
      Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty,
        dvoValueCopiedByReference]): variant;
    /// returns the output parameters as a TDocVariant object or array
    function OutputAsDocVariant(Kind: TInterfaceMethodParamsDocVariantKind;
      Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty,
        dvoValueCopiedByReference]): variant;
    /// log the input or output parameters to a log instance
    procedure AddLog(aLog: TSynLogClass; aOutput: boolean;
      aLevel: TSynLogInfo = sllTrace);
    /// input parameters when calling the method
    // - order shall follow the method const and var parameters
    // ! Stub.Add(10,20) -> Input[0]=10, Input[1]=20
    // - if the supplied Index is out of range, an EInterfaceStub will be raised
    property Input[Index: integer]: variant
      read GetInput;
    /// output parameters returned after method process
    // - order shall follow the method var, out parameters and the function
    // result (if method is not a procedure)
    // - if the supplied Index is out of range, an EInterfaceStub will be raised
    // - can be used as such:
    // !  procedure TFooTestCase.ExecuteBar(Ctxt: TOnInterfaceStubExecuteParamsVariant);
    // !  begin // Input[0]=i
    // !    Ctxt.Output[0] := Ctxt.Input[0]+1;  // i := i+1;
    // !    Ctxt.Output[1] := 42;               // result := 42;
    // !  end; // Output|0]=i, Output[1]=result
    // to emulate this native implementation:
    // ! function Bar(var i: integer): integer;
    // ! begin
    // !    inc(i);
    // !    result := 42;
    // !  end;
    // - consider using the safest Named[] property, to avoid parameters
    // index matching issue
    // - if an Output[]/Named[] item is not set, a default value will be used
    property Output[Index: integer]: variant write SetOutput;
    /// access to input/output parameters when calling the method
    // - if the supplied name is incorrect, an EInterfaceStub will be raised
    // - is a bit slower than Input[]/Output[] indexed properties, but easier
    // to work with, and safer in case of method signature change (like parameter
    // add or rename)
    // - marked as default property, so you can use it e.g. as such:
    // !  procedure TFooTestCase.ExecuteBar(Ctxt: TOnInterfaceStubExecuteParamsVariant);
    // !  begin
    // !    Ctxt['i'] := Ctxt['i']+1;  // i := i+1;
    // !    Ctxt['result'] := 42;      // result := 42;
    // !  end;
    // to emulate this native implementation:
    // ! function Bar(var i: integer): integer;
    // ! begin
    // !    inc(i);
    // !    result := 42;
    // !  end;
    // - using this default Named[] property is recommended over the index-based
    // Output[] property
    // - if an Output[]/Named[] item is not set, a default value will be used
    property Named[const ParamName: RawUtf8]: variant
      read GetInNamed write SetOutNamed; default;
    /// access to UTF-8 input parameters when calling the method
    // - if the supplied name is incorrect, an EInterfaceStub will be raised
    // - is a bit slower than Input[]/Output[] indexed properties, but easier
    // to work with, and safer in case of method signature change (like parameter
    // add or rename)
    // - slightly easier to use Ctxt.U['str'] than ToUtf8(Ctxt.Named['str'])
    property U[const ParamName: RawUtf8]: RawUtf8
      read GetInUtf8;
  end;

  /// parameters used by TInterfaceStub.Executes() events callbacks as JSON
  // - this class will expect input and output parameters to be encoded as
  // JSON arrays, so is faster than TOnInterfaceStubExecuteParamsVariant
  TOnInterfaceStubExecuteParamsJson = class(TOnInterfaceStubExecuteParamsAbstract)
  public
    /// a method to return an array of values into result
    // - just a wrapper around JsonEncodeArrayOfConst([...])
    // - can be used as such:
    // !  procedure TFooTestCase.ExecuteBar(var Ctxt: TOnInterfaceStubExecuteParamsJson);
    // !  begin // Ctxt.Params := '[i]' -> Ctxt.result := '[i+1,42]'
    // !    Ctxt.Returns([GetInteger(pointer(Ctxt.Params))+1,42]);
    // !  end;
    // to emulate this native implementation:
    // ! function Bar(var i: integer): integer;
    // ! begin
    // !    inc(i);
    // !    result := 42;
    // !  end;
    procedure Returns(const Values: array of const); overload;
    /// a method to return a JSON array of values into result
    // - expected format is e.g. '[43,42]'
    procedure Returns(const ValuesJsonArray: RawUtf8); overload;
    /// incoming parameters array encoded as JSON array without braces
    // - order follows the method const and var parameters
    // ! Stub.Add(10,20) -> Params = '10,20';
    property Params: RawUtf8
      read fParams;
  end;

  /// event called by the TInterfaceStub.Executes() fluent method for variant process
  // - by default Ctxt.result shall contain the default JSON array result for
  // this method - use Ctxt.Named[] default properties, e.g. as
  // ! Ctxt['result'] := Ctxt['n1']-Ctxt['n2'];
  // or with Input[] / Output[] properties:
  // ! with Ctxt do Output[0] := Input[0]-Input[1];
  // - you can call Ctxt.Error() to notify the caller for an execution error
  TOnInterfaceStubExecuteVariant = procedure(
    Ctxt: TOnInterfaceStubExecuteParamsVariant) of object;

  /// event called by the TInterfaceStub.Executes() fluent method for JSON process
  // - by default Ctxt.result shall contain the default JSON array result for
  // this method - use Ctxt.Named[] default properties, e.g. as
  // !  P := pointer(Ctxt.Params);
  // !  Ctxt.Returns([GetNextItemDouble(P)-GetNextItemDouble(P)]);
  // - you can call Ctxt.Error() to notify the caller for an execution error
  TOnInterfaceStubExecuteJson = procedure(
    Ctxt: TOnInterfaceStubExecuteParamsJson) of object;

  /// diverse types of stubbing / mocking rules
  // - isUndefined is the first, since it will be a ExpectsCount() weak rule
  // which may be overwritten by the other real run-time rules
  TInterfaceStubRuleKind = (
    isUndefined,
    isExecutesJson,
    isExecutesVariant,
    isRaises,
    isReturns,
    isFails);

  /// supported comparison operators for stubbing / mocking rules
  TInterfaceStubRuleOperator = (
    ioUndefined,
    ioEqualTo,
    ioNotEqualTo,
    ioLessThan,
    ioLessThanOrEqualTo,
    ioGreaterThan,
    ioGreaterThanOrEqualTo,
    ioTraceMatch);

  /// define a mocking / stubing rule used internally by TInterfaceStub
  TInterfaceStubRule = record
    /// optional expected parameters, serialized as a JSON array
    // - if equals '', the rule is not parametrized - i.e. it will be the
    // default for this method
    Params: RawUtf8;
    /// values associated to the rule
    // - for TInterfaceStub.Executes(), is the aEventParams parameter transmitted
    // to Execute event handler (could be used to e.g. customize the handler)
    // - for TInterfaceStub.Raises(), is the Exception.Message associated
    // to one ExceptionClass
    // - for TInterfaceStub.Returns(), is the returned result, serialized as a
    // JSON array (including var / out parameters then any function result)
    // - for TInterfaceStub.Fails() is the returned error message for
    // TInterfaceStub exception or TInterfaceMock associated test case
    Values: RawUtf8;
    /// the type of this rule
    // - isUndefined is used for a TInterfaceStub.ExpectsCount() weak rule
    Kind: TInterfaceStubRuleKind;
    /// the event handler to be executed
    // - for TInterfaceStub.Executes(), Values is transmitted as aResult parameter
    // - either a TOnInterfaceStubExecuteJson, or a TOnInterfaceStubExecuteVariant
    Execute: TMethod;
    /// the exception class to be raised
    // - for TInterfaceStub.Raises(), Values contains Exception.Message
    ExceptionClass: ExceptClass;
    /// the number of times this rule has been executed
    RulePassCount: cardinal;
    /// comparison operator set by TInterfaceStub.ExpectsCount()
    ExpectedPassCountOperator: TInterfaceStubRuleOperator;
    /// expected pass count value set by TInterfaceStub.ExpectsCount()
    // - value to be compared to the number of times this rule has been executed
    // - TInterfaceStub/TInterfaceMock will check it in their Destroy destructor,
    // using the comparison stated by ExpectedPassCountOperator
    ExpectedPassCount: cardinal;
    /// log trace value set by TInterfaceStub.ExpectsTrace()
    // - used in conjunction with ExpectedPassCountOperator=ioTraceMatch
    // - value to be compared to the Hash32() value of the execution log trace
    // - TInterfaceStub/TInterfaceMock will check it in their Destroy destructor,
    // using the fLogs[] content
    ExpectedTraceHash: cardinal;
  end;

  /// define the rules for a given method as used internally by TInterfaceStub
  {$ifdef USERECORDWITHMETHODS}
  TInterfaceStubRules = record
  {$else}
  TInterfaceStubRules = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the mocking / stubing rules associated to this method
    Rules: array of TInterfaceStubRule;
    /// index in Rules[] of the default rule, i.e. the one with Params=''
    DefaultRule: integer;
    /// the number of times this method has been executed
    MethodPassCount: cardinal;
    /// find a rule index from its Params content
    function FindRuleIndex(const aParams: RawUtf8): integer;
    /// find a strong rule index from its Params content
    function FindStrongRuleIndex(const aParams: RawUtf8): integer;
    /// register a rule
    procedure AddRule(Sender: TInterfaceStub; aKind: TInterfaceStubRuleKind;
      const aParams, aValues: RawUtf8; const aEvent: TNotifyEvent = nil;
      aExceptionClass: ExceptClass = nil;
      aExpectedPassCountOperator: TInterfaceStubRuleOperator = ioUndefined;
      aValue: cardinal = 0);
  end;

  /// diverse options available to TInterfaceStub
  // - by default, method execution stack is not recorded - include
  // imoLogMethodCallsAndResults in the options to track all method calls
  // and the returned values; note that ExpectsTrace() method will set it
  // - by default, TInterfaceStub will be released when the stubed/mocked
  // interface is released - include imoFakeInstanceWontReleaseTInterfaceStub
  // in the options to force manual memory handling of TInterfaceStubs
  // - by default, all interfaces will return some default values, unless
  // imoRaiseExceptionIfNoRuleDefined or imoReturnErrorIfNoRuleDefined is
  // included in the options
  // - by default, any TInterfaceMock.Fails() rule execution will notify the
  // TSynTestCase, unless imoMockFailsWillPassTestCase which will let test pass
  TInterfaceStubOption = (
    imoLogMethodCallsAndResults,
    imoFakeInstanceWontReleaseTInterfaceStub,
    imoRaiseExceptionIfNoRuleDefined,
    imoReturnErrorIfNoRuleDefined,
    imoMockFailsWillPassTestCase);

  /// set of options available to TInterfaceStub
  TInterfaceStubOptions = set of TInterfaceStubOption;

  /// every potential part of TInterfaceStubLog.AddAsText() log entry
  TInterfaceStubLogLayout = (
    wName,
    wParams,
    wResults);

  /// set the output layout of TInterfaceStubLog.AddAsText() log entry
  TInterfaceStubLogLayouts = set of TInterfaceStubLogLayout;

  /// used to keep track of one stubbed method call
  {$ifdef USERECORDWITHMETHODS}
  TInterfaceStubLog = record
  {$else}
  TInterfaceStubLog = object
  {$endif USERECORDWITHMETHODS}
  public
    /// call timestamp, in milliseconds
    // - is filled with GetTickCount64() API returned value
    Timestamp64: Int64;
    /// set to TRUE if this calls failed
    // - i.e. if EInterfaceFactory was raised for TInterfaceStub, or
    // if TInterfaceMock did notify its associated TSynTestCase via a Check()
    // - CustomResults/Results will contain the error message
    WasError: boolean;
    /// the method called
    // - a pointer to the existing information in shared TInterfaceFactory
    Method: PInterfaceMethod;
    /// the parameters at execution call, as JSON CSV (i.e. array without [ ])
    Params: RawUtf8;
    /// any non default result returned after execution
    // - if not set (i.e. if equals ''), Method^.DefaultResult has been returned
    // - if WasError is TRUE, always contain the error message
    CustomResults: RawUtf8;
    /// the result returned after execution
    // - this method will return Method^.DefaultResult if CustomResults=''
    function Results: RawUtf8;
    /// append the log in textual format
    // - typical output is as such:
    // $ Add(10,20)=[30],
    // or, if WasError is TRUE:
    // $ Divide(20,0) error "divide by zero",
    procedure AddAsText(WR: TJsonWriter; aScope: TInterfaceStubLogLayouts;
      SepChar: AnsiChar = ',');
  end;

  /// used to keep track of all stubbed methods calls
  TInterfaceStubLogDynArray = array of TInterfaceStubLog;

  /// how TInterfacedObjectFake identify each instance
  // - match the ID used in sicClientDriven mode of a service
  // - match the TInterfacedObjectFakeServer 32-bit identifier of a callback
  TInterfacedObjectFakeID = type cardinal;
  PInterfacedObjectFakeID = ^TInterfacedObjectFakeID;

  /// used to stub an interface implementation
  // - define the expected workflow in a fluent interface using Executes /
  // Fails / Returns / Raises
  // - this class will be inherited by TInterfaceMock which will contain some
  // additional methods dedicated to mocking behavior (e.g. including in tests)
  // - each instance of this class will be owned by its generated fake
  // implementation class (retrieved at constructor out parameter): when the
  // stubed/mocked interface is freed, its associated TInterfaceStub will be
  // freed - so you do not need to protect TInterfaceStub.Create with a
  // try..finally clause, since it will be released when no more needed
  // - inherits from TInterfaceResolver so match TInjectableObject expectations
  TInterfaceStub = class(TInterfaceResolver)
  protected
    fInterface: TInterfaceFactory;
    fRules: array of TInterfaceStubRules;
    fOptions: TInterfaceStubOptions;
    fHasExpects: set of (eCount, eTrace);
    fLogs: TInterfaceStubLogDynArray;
    fLog: TDynArray;
    fLogCount: integer;
    fInterfaceExpectedTraceHash: cardinal;
    fLastInterfacedObjectFake: TInterfacedObject;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
    procedure InternalGetInstance(out aStubbedInterface); virtual;
    function InternalCheck(aValid, aExpectationFailed: boolean;
      const aErrorMsgFmt: RawUtf8; const aErrorMsgArgs: array of const): boolean; virtual;
    // match TOnFakeInstanceInvoke callback signature
    function Invoke(const aMethod: TInterfaceMethod; const aParams: RawUtf8;
      aResult, aErrorMsg: PRawUtf8; aFakeID: PInterfacedObjectFakeID;
      aServiceCustomAnswer: PServiceCustomAnswer): boolean;
    // will launch InternalCheck() process if some expectations defined by
    // ExpectsCount() are not met, i.e. raise an exception for TInterfaceStub
    // or notify the associated test case for TInterfaceMock
    procedure InstanceDestroyed(aFakeID: TInterfacedObjectFakeID);
    procedure IntSetOptions(Options: TInterfaceStubOptions); virtual;
    procedure IntCheckCount(aMethodIndex, aComputed: cardinal;
      aOperator: TInterfaceStubRuleOperator; aCount: cardinal);
    function IntGetLogAsText(asmndx: integer; const aParams: RawUtf8;
      aScope: TInterfaceStubLogLayouts; SepChar: AnsiChar): RawUtf8;
    function GetLogHash: cardinal;
    procedure OnExecuteToLog(Ctxt: TOnInterfaceStubExecuteParamsVariant);
  public
    /// low-level internal constructor
    // - you should not call this method, but the overloaded alternatives
    constructor Create(aFactory: TInterfaceFactory;
      const aInterfaceName: RawUtf8); reintroduce; overload; virtual;
    /// initialize an interface stub from TypeInfo(IMyInterface)
    // - assign the fake class instance to a stubbed interface variable:
    // !var I: ICalculator;
    // !  TInterfaceStub.Create(TypeInfo(ICalculator),I);
    // !  Check(I.Add(10,20)=0,'Default result');
    constructor Create(aInterface: PRttiInfo;
      out aStubbedInterface); reintroduce; overload;
    /// initialize an interface stub from an interface GUID
    // - you shall have registered the interface by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...])
    // - once registered, create and use the fake class instance as such:
    // !var I: ICalculator;
    // !  TInterfaceStub.Create(ICalculator,I);
    // !  Check(I.Add(10,20)=0,'Default result');
    // - if the supplied TGUID has not been previously registered, raise an Exception
    constructor Create(const aGuid: TGUID;
      out aStubbedInterface); reintroduce; overload;
    /// initialize an interface stub from an interface name (e.g. 'IMyInterface')
    // - you shall have registered the interface by a previous call to
    // TInterfaceFactory.Get(TypeInfo(IMyInterface)) or RegisterInterfaces([])
    // - if the supplied name has not been previously registered, raise an Exception
    constructor Create(const aInterfaceName: RawUtf8;
      out aStubbedInterface); reintroduce; overload;
    /// prepare an interface stub from TypeInfo(IMyInterface) for later injection
    // - create several TInterfaceStub instances for a given TInjectableObject
    // ! procedure TMyTestCase.OneTestCaseMethod;
    // ! var Test: IServiceToBeTested;
    // ! begin
    // !   Test := TServiceToBeTested.CreateInjected([],
    // !     TInterfaceStub.Create(TypeInfo(ICalculator)),
    // !     TInterfaceMock.Create(TypeInfo(IPersistence),self).
    // !       ExpectsCount('SaveItem',qoEqualTo,1)]);
    constructor Create(aInterface: PRttiInfo); reintroduce; overload;
    /// prepare an interface stub from a given TGUID for later injection
    // - you shall have registered the interface by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...])
    // - then create TInterfaceStub instances for a given TInjectableObject:
    // ! procedure TMyTestCase.OneTestCaseMethod;
    // ! var Test: IServiceToBeTested;
    // ! begin
    // !   Test := TServiceToBeTested.CreateInjected(
    // !     [IMyInterface],
    // !     TInterfaceMock.Create(IPersistence,self).
    // !       ExpectsCount('SaveItem',qoEqualTo,1)]);
    constructor Create(const aGuid: TGUID); reintroduce; overload;

    /// add an execution rule for a given method, with JSON marshalling
    // - optional aEventParams parameter will be transmitted to aEvent handler
    // - raise an Exception if the method name does not exist for this interface
    function Executes(const aMethodName: RawUtf8;
      const aEvent: TOnInterfaceStubExecuteJson;
      const aEventParams: RawUtf8 = ''): TInterfaceStub; overload;
    /// add an execution rule for a given method and a set of parameters,
    // with JSON marshalling
    // - if execution context matches the supplied aParams value, aEvent is triggered
    // - optional aEventParams parameter will be transmitted to aEvent handler
    // - raise an Exception if the method name does not exist for this interface
    function Executes(const aMethodName, aParams: RawUtf8;
      const aEvent: TOnInterfaceStubExecuteJson;
      const aEventParams: RawUtf8 = ''): TInterfaceStub; overload;
    /// add an execution rule for a given method and a set of parameters,
    // with JSON marshalling
    // - if execution context matches the supplied aParams value, aEvent is triggered
    // - optional aEventParams parameter will be transmitted to aEvent handler
    // - raise an Exception if the method name does not exist for this interface
    function Executes(const aMethodName: RawUtf8; const aParams: array of const;
      const aEvent: TOnInterfaceStubExecuteJson;
      const aEventParams: RawUtf8 = ''): TInterfaceStub; overload;
    /// add an execution rule for a given method, with Variant marshalling
    // - optional aEventParams parameter will be transmitted to aEvent handler
    // - raise an Exception if the method name does not exist for this interface
    function Executes(const aMethodName: RawUtf8;
      const aEvent: TOnInterfaceStubExecuteVariant;
      const aEventParams: RawUtf8 = ''): TInterfaceStub; overload;
    /// add an execution rule for a given method and a set of parameters,
    // with Variant marshalling
    // - if execution context matches the supplied aParams value, aEvent is triggered
    // - optional aEventParams parameter will be transmitted to aEvent handler
    // - raise an Exception if the method name does not exist for this interface
    function Executes(const aMethodName, aParams: RawUtf8;
      const aEvent: TOnInterfaceStubExecuteVariant;
      const aEventParams: RawUtf8 = ''): TInterfaceStub; overload;
    /// add an execution rule for a given method and a set of parameters,
    // with Variant marshalling
    // - if execution context matches the supplied aParams value, aEvent is triggered
    // - optional aEventParams parameter will be transmitted to aEvent handler
    // - raise an Exception if the method name does not exist for this interface
    function Executes(const aMethodName: RawUtf8; const aParams: array of const;
      const aEvent: TOnInterfaceStubExecuteVariant;
      const aEventParams: RawUtf8 = ''): TInterfaceStub; overload;
    /// add an execution rule for all methods, with Variant marshalling
    // - optional aEventParams parameter will be transmitted to aEvent handler
    // - callback's Ctxt: TOnInterfaceStubExecuteParamsVariant's Method field
    // will identify the executed method
    function Executes(const aEvent: TOnInterfaceStubExecuteVariant;
      const aEventParams: RawUtf8 = ''): TInterfaceStub; overload;
    /// will add execution rules for all methods to log the input parameters
    // - aKind will define how the input parameters are serialized in JSON
    function Executes(aLog: TSynLogClass; aLogLevel: TSynLogInfo;
      aKind: TInterfaceMethodParamsDocVariantKind): TInterfaceStub; overload;

    /// add an exception rule for a given method
    // - will create and raise the specified exception for this method
    // - raise an Exception if the method name does not exist for this interface
    function Raises(const aMethodName: RawUtf8; aException: ExceptClass;
      const aMessage: string): TInterfaceStub; overload;
    /// add an exception rule for a given method and a set of parameters
    // - will create and raise the specified exception for this method, if the
    // execution context matches the supplied aParams value
    // - raise an Exception if the method name does not exist for this interface
    function Raises(const aMethodName, aParams: RawUtf8; aException: ExceptClass;
      const aMessage: string): TInterfaceStub; overload;
    /// add an exception rule for a given method and a set of parameters
    // - will create and raise the specified exception for this method, if the
    // execution context matches the supplied aParams value
    // - raise an Exception if the method name does not exist for this interface
    function Raises(const aMethodName: RawUtf8; const aParams: array of const;
      aException: ExceptClass; const aMessage: string): TInterfaceStub; overload;

    /// add an evaluation rule for a given method
    // - aExpectedResults JSON array will be returned to the caller
    // - raise an Exception if the method name does not exist for this interface
    function Returns(const aMethodName,
      aExpectedResults: RawUtf8): TInterfaceStub; overload;
    /// add an evaluation rule for a given method
    // - aExpectedResults will be returned to the caller after conversion to
    // a JSON array
    // - raise an Exception if the method name does not exist for this interface
    function Returns(const aMethodName: RawUtf8;
      const aExpectedResults: array of const): TInterfaceStub; overload;
    /// add an evaluation rule for a given method and a set of parameters
    // - aExpectedResults JSON array will be returned to the caller
    // - raise an Exception if the method name does not exist for this interface
    function Returns(const aMethodName, aParams,
      aExpectedResults: RawUtf8): TInterfaceStub; overload;
    /// add an evaluation rule for a given method and a set of parameters
    // - aExpectedResults JSON array will be returned to the caller
    // - raise an Exception if the method name does not exist for this interface
    function Returns(const aMethodName: RawUtf8;
      const aParams, aExpectedResults: array of const): TInterfaceStub; overload;

    /// add an error rule for a given method
    // - an error will be returned to the caller, with aErrorMsg as message
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function Fails(const aMethodName, aErrorMsg: RawUtf8): TInterfaceStub; overload;
    /// add an error rule for a given method and a set of parameters
    // - an error will be returned to the caller, with aErrorMsg as message
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function Fails(const aMethodName, aParams,
      aErrorMsg: RawUtf8): TInterfaceStub; overload;
    /// add an error rule for a given method and a set of parameters
    // - an error will be returned to the caller, with aErrorMsg as message
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function Fails(const aMethodName: RawUtf8; const aParams: array of const;
      const aErrorMsg: RawUtf8): TInterfaceStub; overload;

    /// add a pass count expectation rule for a given method
    // - those rules will be evaluated at Destroy execution
    // - only qoEqualTo..qoGreaterThanOrEqualTo are relevant here
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsCount(const aMethodName: RawUtf8;
      aOperator: TInterfaceStubRuleOperator; aValue: cardinal): TInterfaceStub; overload;
    /// add a pass count expectation rule for a given method and a set of parameters
    // - those rules will be evaluated at Destroy execution
    // - only qoEqualTo..qoGreaterThanOrEqualTo are relevant here
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsCount(const aMethodName, aParams: RawUtf8;
      aOperator: TInterfaceStubRuleOperator; aValue: cardinal): TInterfaceStub; overload;
    /// add a pass count expectation rule for a given method and a set of parameters
    // - those rules will be evaluated at Destroy execution
    // - only qoEqualTo..qoGreaterThanOrEqualTo are relevant here
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsCount(const aMethodName: RawUtf8;
      const aParams: array of const; aOperator: TInterfaceStubRuleOperator;
      aValue: cardinal): TInterfaceStub; overload;

    /// add a hash-based execution expectation rule for the whole interface
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is a Hash32() of the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    function ExpectsTrace(aValue: cardinal): TInterfaceStub; overload;
    /// add a hash-based execution expectation rule for a given method
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is a Hash32() of the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsTrace(const aMethodName: RawUtf8;
      aValue: cardinal): TInterfaceStub; overload;
    /// add a hash-based execution expectation rule for a given method
    // and a set of parameters
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is a Hash32() of the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsTrace(const aMethodName, aParams: RawUtf8;
      aValue: cardinal): TInterfaceStub; overload;
    /// add a hash-based execution expectation rule for a given method
    // and a set of parameters
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is a Hash32() of the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsTrace(const aMethodName: RawUtf8;
      const aParams: array of const; aValue: cardinal): TInterfaceStub; overload;
    /// add a JSON-based execution expectation rule for the whole interface
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    function ExpectsTrace(const aValue: RawUtf8): TInterfaceStub; overload;
    /// add a JSON-based execution expectation rule for a given method
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsTrace(const aMethodName, aValue: RawUtf8): TInterfaceStub; overload;
    /// add a JSON-based execution expectation rule for a given method
    // and a set of parameters
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsTrace(const aMethodName, aParams,
      aValue: RawUtf8): TInterfaceStub; overload;
    /// add a JSON-based execution expectation rule for a given method
    // and a set of parameters
    // - those rules will be evaluated at Destroy execution
    // - supplied aValue is the trace in LogAsText format
    // - it will raise EInterfaceFactory for TInterfaceStub, but
    // TInterfaceMock will push the failure to the associated test case
    // - raise an Exception if the method name does not exist for this interface
    function ExpectsTrace(const aMethodName: RawUtf8;
      const aParams: array of const;
      const aValue: RawUtf8): TInterfaceStub; overload;

    /// set the optional stubing/mocking options
    // - same as the Options property, but in a fluent-style interface
    function SetOptions(Options: TInterfaceStubOptions): TInterfaceStub;
    /// reset the internal trace
    // - Log, LogAsText, LogHash and LogCount will be initialized
    procedure ClearLog;

    /// the stubbed method execution trace items
    property Log: TInterfaceStubLogDynArray
      read fLogs;
    /// the stubbed method execution trace converted as text
    // - typical output is a list of calls separated by commas:
    // $ Add(10,20)=[30],Divide(20,0) error "divide by zero"
    function LogAsText(SepChar: AnsiChar = ','): RawUtf8;
    /// returns the last created TInterfacedObject instance
    // - e.g. corresponding to the out aStubbedInterface parameter of Create()
    property LastInterfacedObjectFake: TInterfacedObject
      read fLastInterfacedObjectFake;
    /// check if can resolve the supplied interface RTTI
    function Implements(aInterface: PRttiInfo): boolean; override;
  published
    /// access to the registered Interface RTTI information
    property InterfaceFactory: TInterfaceFactory
      read fInterface;
    /// optional stubing/mocking options
    // - you can use the SetOptions() method in a fluent-style interface
    property Options: TInterfaceStubOptions
      read fOptions write IntSetOptions;
    /// the stubbed method execution trace number of items
    property LogCount: integer
      read fLogCount;
    /// the stubbed method execution trace converted as one numerical hash
    // - returns Hash32(LogAsText)
    property LogHash: cardinal
      read GetLogHash;
  end;

  /// used to mock an interface implementation via expect-run-verify pattern
  // - TInterfaceStub will raise an exception on Fails(), ExpectsCount() or
  // ExpectsTrace() rule activation, but TInterfaceMock will call
  // TSynTestCase.Check() with no exception with such rules, as expected by
  // a mocked interface
  // - this class will follow the expect-run-verify pattern, i.e. expectations
  // are defined before running the test, and verification is performed
  // when the instance is released - use TInterfaceMockSpy if you prefer the
  // more explicit run-verify pattern
  TInterfaceMock = class(TInterfaceStub)
  protected
    fTestCase: TSynTestCase;
    function InternalCheck(aValid, aExpectationFailed: boolean;
      const aErrorMsgFmt: RawUtf8;
      const aErrorMsgArgs: array of const): boolean; override;
  public
    /// initialize an interface mock from TypeInfo(IMyInterface)
    // - aTestCase.Check() will be called in case of mocking failure
    // ! procedure TMyTestCase.OneTestCaseMethod;
    // ! var Persist: IPersistence;
    // ! ...
    // !   TInterfaceMock.Create(TypeInfo(IPersistence),Persist,self).
    // !     ExpectsCount('SaveItem',qoEqualTo,1)]);
    constructor Create(aInterface: PRttiInfo; out aMockedInterface;
      aTestCase: TSynTestCase); reintroduce; overload;
    /// initialize an interface mock from an interface TGUID
    // - aTestCase.Check() will be called during validation of all Expects*()
    // - you shall have registered the interface by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IPersistence),...])
    // - once registered, create and use the fake class instance as such:
    // !procedure TMyTestCase.OneTestCaseMethod;
    // !var Persist: IPersistence;
    // ! ...
    // !   TInterfaceMock.Create(IPersistence,Persist,self).
    // !     ExpectsCount('SaveItem',qoEqualTo,1)]);
    // - if the supplied TGUID has not been previously registered, raise an Exception
    constructor Create(const aGuid: TGUID; out aMockedInterface;
      aTestCase: TSynTestCase); reintroduce; overload;
    /// initialize an interface mock from an interface name (e.g. 'IMyInterface')
    // - aTestCase.Check() will be called in case of mocking failure
    // - you shall have registered the interface by a previous call to
    // TInterfaceFactory.Get(TypeInfo(IMyInterface)) or RegisterInterfaces()
    // - if the supplied name has not been previously registered, raise an Exception
    constructor Create(const aInterfaceName: RawUtf8; out aMockedInterface;
      aTestCase: TSynTestCase); reintroduce; overload;
    /// initialize an interface mock from TypeInfo(IMyInterface) for later injection
    // - aTestCase.Check() will be called in case of mocking failure
    constructor Create(aInterface: PRttiInfo; aTestCase: TSynTestCase);
      reintroduce; overload;
    /// initialize an interface mock from TypeInfo(IMyInterface) for later injection
    // - aTestCase.Check() will be called in case of mocking failure
    constructor Create(const aGuid: TGUID; aTestCase: TSynTestCase);
      reintroduce; overload;
    /// the associated test case
    property TestCase: TSynTestCase
      read fTestCase;
  end;

  /// how TInterfaceMockSpy.Verify() shall generate the calls trace
  TInterfaceMockSpyCheck = (
    chkName,
    chkNameParams,
    chkNameParamsResults);

  /// used to mock an interface implementation via run-verify pattern
  // - this class will implement a so called "test-spy" mocking pattern, i.e.
  // no expectation is to be declared at first, but all calls are internally
  // logged (i.e. it force imoLogMethodCallsAndResults option to be defined),
  // and can afterwards been check via Verify() calls
  TInterfaceMockSpy = class(TInterfaceMock)
  protected
    procedure IntSetOptions(Options: TInterfaceStubOptions); override;
  public
    /// this will set and force imoLogMethodCallsAndResults option as needed
    // - you should not call this method, but the overloaded alternatives
    constructor Create(aFactory: TInterfaceFactory;
      const aInterfaceName: RawUtf8); override;
    /// check that a method has been called a specify number of times
    procedure Verify(const aMethodName: RawUtf8;
      aOperator: TInterfaceStubRuleOperator = ioGreaterThan;
      aCount: cardinal = 0); overload;
    /// check a method calls count with a set of parameters
    // - parameters shall be defined as a JSON array of values
    procedure Verify(const aMethodName, aParams: RawUtf8;
      aOperator: TInterfaceStubRuleOperator = ioGreaterThan;
      aCount: cardinal = 0); overload;
    /// check a method calls count with a set of parameters
    // - parameters shall be defined as a JSON array of values
    procedure Verify(const aMethodName: RawUtf8; const aParams: array of const;
      aOperator: TInterfaceStubRuleOperator = ioGreaterThan;
      aCount: cardinal = 0); overload;
    /// check an execution trace for the global interface
    // - text trace format shall follow method calls, e.g.
    // ! Verify('Multiply,Add',chkName);
    // or may include parameters:
    // ! Verify('Multiply(10,30),Add(2,35)',chkNameParams);
    // or include parameters and function results:
    // ! Verify('Multiply(10,30)=[300],Add(2,35)=[37]',chkNameParamsResults);
    procedure Verify(const aTrace: RawUtf8;
      aScope: TInterfaceMockSpyCheck); overload;
    /// check an execution trace for a specified method
    // - text trace format will follow specified scope, e.g.
    // ! Verify('Add','(10,30),(2,35)',chkNameParams);
    // or include parameters and function results:
    // ! Verify('Add','(10,30)=[300],(2,35)=[37]',chkNameParamsResults);
    // - if aMethodName does not exists or aScope=chkName, will raise an exception
    procedure Verify(const aMethodName, aTrace: RawUtf8;
      aScope: TInterfaceMockSpyCheck); overload;
    /// check an execution trace for a specified method and parameters
    // - text trace format shall contain only results, e.g.
    // ! Verify('Add','2,35','[37]');
    procedure Verify(const aMethodName, aParams, aTrace: RawUtf8); overload;
    /// check an execution trace for a specified method and parameters
    // - text trace format shall contain only results, e.g.
    // ! Verify('Add',[2,35],'[37]');
    procedure Verify(const aMethodName: RawUtf8; const aParams: array of const;
      const aTrace: RawUtf8); overload;
  end;

function ToText(c: TInterfaceMockSpyCheck): PShortString; overload;
function ToText(op: TInterfaceStubRuleOperator): PShortString; overload;


{ ************ TInterfacedObjectFake with JITted Methods Execution }

// see http://docwiki.embarcadero.com/RADStudio/en/Program_Control

const
{$ifdef CPU64}
  // maximum stack size at method execution must match .PARAMS 64 (minus 4 regs)
  MAX_EXECSTACK = 60 * 8;
{$else}
  // maximum stack size at method execution
  {$ifdef CPUARM}
  MAX_EXECSTACK = 60 * 4;
  {$else}
  MAX_EXECSTACK = 1024;
  {$endif}
{$endif CPU64}

{$ifdef CPUX86}
  // 32-bit integer param registers (in "register" calling convention)
  REGEAX = 1;
  REGEDX = 2;
  REGECX = 3;
  PARAMREG_FIRST = REGEAX;
  PARAMREG_LAST = REGECX;
  // floating-point params are passed by reference
  VMTSTUBSIZE = 24;
{$endif CPUX86}

{$ifdef CPUX64}
  // 64-bit integer param registers
  {$ifdef SYSVABI}
  REGRDI = 1;
  REGRSI = 2;
  REGRDX = 3;
  REGRCX = 4;
  REGR8 = 5;
  REGR9 = 6;
  PARAMREG_FIRST = REGRDI;
  PARAMREG_RESULT = REGRSI;
  {$else}
  REGRCX = 1;
  REGRDX = 2;
  REGR8 = 3;
  REGR9 = 4;
  PARAMREG_FIRST = REGRCX;
  PARAMREG_RESULT = REGRDX;
  {$endif SYSVABI}
  PARAMREG_LAST = REGR9;
  // 64-bit floating-point (double) registers
  REGXMM0 = 1;
  REGXMM1 = 2;
  REGXMM2 = 3;
  REGXMM3 = 4;
  {$ifdef SYSVABI}
  REGXMM4 = 5;
  REGXMM5 = 6;
  REGXMM6 = 7;
  REGXMM7 = 8;
  FPREG_FIRST = REGXMM0;
  FPREG_LAST = REGXMM7;
  {$else}
  FPREG_FIRST = REGXMM0;
  FPREG_LAST = REGXMM3;
  {$endif SYSVABI}
  {$define HAS_FPREG}
  VMTSTUBSIZE = 24;
{$endif CPUX64}

{$ifdef CPUARM}
  // 32-bit integer param registers
  REGR0 = 1;
  REGR1 = 2;
  REGR2 = 3;
  REGR3 = 4;
  PARAMREG_FIRST = REGR0;
  PARAMREG_LAST = REGR3;
  PARAMREG_RESULT = REGR1;
  // 64-bit floating-point (double) registers
  {$ifdef CPUARMHF}
  REGD0 = 1;
  REGD1 = 2;
  REGD2 = 3;
  REGD3 = 4;
  REGD4 = 5;
  REGD5 = 6;
  REGD6 = 7;
  REGD7 = 8;
  FPREG_FIRST = REGD0;
  FPREG_LAST = REGD7;
  {$define HAS_FPREG}
  {$endif CPUARMHF}
  VMTSTUBSIZE = 16;
{$endif CPUARM}

{$ifdef CPUAARCH64}
  // 64-bit integer param registers
  REGX0 = 1;
  REGX1 = 2;
  REGX2 = 3;
  REGX3 = 4;
  REGX4 = 5;
  REGX5 = 6;
  REGX6 = 7;
  REGX7 = 8;
  PARAMREG_FIRST = REGX0;
  PARAMREG_LAST = REGX7;
  PARAMREG_RESULT = REGX1;
  // 64-bit floating-point (double) registers
  REGD0 = 1; // map REGV0 128-bit NEON register
  REGD1 = 2; // REGV1
  REGD2 = 3; // REGV2
  REGD3 = 4; // REGV3
  REGD4 = 5; // REGV4
  REGD5 = 6; // REGV5
  REGD6 = 7; // REGV6
  REGD7 = 8; // REGV7
  FPREG_FIRST = REGD0;
  FPREG_LAST = REGD7;
  {$define HAS_FPREG}
  VMTSTUBSIZE = 28;
{$endif CPUAARCH64}

  STACKOFFSET_NONE = -1;

  // ordinal values are stored within 64-bit buffer, and records in a RawUtf8
  ARGS_TO_VAR: array[TInterfaceMethodValueType] of TInterfaceMethodValueVar = (
    imvvNone,         // imvNone
    imvvSelf,         // imvSelf
    imvv64,           // imvBoolean
    imvv64,           // imvEnum
    imvv64,           // imvSet
    imvv64,           // imvInteger
    imvv64,           // imvCardinal
    imvv64,           // imvInt64
    imvv64,           // imvDouble
    imvv64,           // imvDateTime
    imvv64,           // imvCurrency
    imvvRawUtf8,      // imvRawUtf8
    imvvString,       // imvString
    imvvRawUtf8,      // imvRawByteString
    imvvWideString,   // imvWideString
    imvv64,           // imvBinary
    imvvRecord,       // imvRecord
    imvvRecord,       // imvVariant
    imvvObject,       // imvObject
    imvvRawUtf8,      // imvRawJson
    imvvDynArray,     // imvDynArray
    imvvInterface);   // imvInterface

  {$ifdef CPU32}
  // parameters are always aligned to 8 bytes boundaries on 64-bit ABI
  ARGS_IN_STACK_SIZE: array[TInterfaceMethodValueType] of cardinal = (
    0,             // imvNone
    POINTERBYTES,  // imvSelf
    POINTERBYTES,  // imvBoolean
    POINTERBYTES,  // imvEnum
    POINTERBYTES,  // imvSet
    POINTERBYTES,  // imvInteger
    POINTERBYTES,  // imvCardinal
    8,             // imvInt64
    8,             // imvDouble
    8,             // imvDateTime
    8,             // imvCurrency
    POINTERBYTES,  // imvRawUtf8
    POINTERBYTES,  // imvString
    POINTERBYTES,  // imvRawByteString
    POINTERBYTES,  // imvWideString
    0,             // imvBinary
    POINTERBYTES,  // imvRecord
    POINTERBYTES,  // imvVariant
    POINTERBYTES,  // imvObject
    POINTERBYTES,  // imvRawJson
    POINTERBYTES,  // imvDynArray
    POINTERBYTES); // imvInterface
  {$endif CPU32}

  ARGS_RESULT_BY_REF: TInterfaceMethodValueTypes =
    [imvRawUtf8,
     imvRawJson,
     imvString,
     imvRawByteString,
     imvWideString,
     imvRecord,
     imvVariant,
     imvDynArray];

type
  /// map the stack memory layout at TInterfacedObjectFake.FakeCall()
  TFakeCallStack = packed record
    {$ifdef CPUX86}
    EDX, ECX, MethodIndex, EBP, Ret: cardinal;
    {$else}
    {$ifdef OSPOSIX}
    ParamRegs: packed array[PARAMREG_FIRST..PARAMREG_LAST] of pointer;
    {$endif OSPOSIX}
    {$ifdef HAS_FPREG}
    FPRegs: packed array[FPREG_FIRST..FPREG_LAST] of double;
    {$endif HAS_FPREG}
    MethodIndex: PtrUInt;
    Frame: pointer;
    Ret: pointer;
    {$ifndef OSPOSIX}
    ParamRegs: packed array[PARAMREG_FIRST..PARAMREG_LAST] of pointer;
    {$endif OSPOSIX}
    {$endif CPUX86}
    {$ifdef CPUARM}
    // alf: on ARM, there is more on the stack than you will expect
    DummyStack: packed array[0..9] of pointer;
    {$endif CPUARM}
    {$ifdef CPUAARCH64}
    // alf: on AARCH64, there is more on the stack than you will expect
    DummyStack: packed array[0..0] of pointer;
    {$endif CPUAARCH64}
    Stack: packed array[word] of byte;
  end;
  PFakeCallStack = ^TFakeCallStack;

  // raw execution context for TInterfacedObjectFakeRaw.FakeCall*() methods
  TFakeCallContext = record
    Stack: PFakeCallStack;
    Method: PInterfaceMethod;
    Result: PInt64;
    ResultType: TInterfaceMethodValueType; // type of value stored into result
    ServiceCustomAnswerPoint: PServiceCustomAnswer;
    Value:     array[0..MAX_METHOD_ARGS - 1] of pointer;
    I64s:      array[0..MAX_METHOD_ARGS - 1] of Int64;
    DynArrays: array[0..MAX_METHOD_ARGS - 1] of TDynArray;
  end;


type
  {$M+}
  /// abstract class handling a generic interface implementation class
  // - implements a simple cross-CPU JIT engine to redirect to FakeCall
  // - note: inheriting from TSynInterfacedObject is not feasible
  TInterfacedObjectFakeRaw = class(TInterfacedObject)
  protected
    fFactory: TInterfaceFactory;
    fVTable: PPointerArray;
    // the JITed asm stubs will redirect to this low-level function(s)
    function FakeCall(stack: PFakeCallStack): Int64;
    procedure FakeCallRaiseError(var ctxt: TFakeCallContext;
      const Format: RawUtf8; const Args: array of const);
    procedure FakeCallGetParamsFromStack(var ctxt: TFakeCallContext);
    procedure FakeCallInternalProcess(var ctxt: TFakeCallContext); virtual; abstract;
    // used internally to compute the actual instance from the FakeCall()
    function SelfFromInterface: TInterfacedObjectFakeRaw;
      {$ifdef HASINLINE} inline; {$endif}
    {$ifdef CPUARM}
    // on ARM, the FakeStub needs to be here, for FakeCall redirection
    procedure ArmFakeStub;
    {$endif CPUARM}
    {$ifdef CPUAARCH64}
    // on Aarch64, the FakeStub needs to be here, for FakeCall redirection
    procedure AArch64FakeStub;
    {$endif CPUAARCH64}
    function FakeQueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      IID: TGUID; out Obj): TIntQry; {$ifdef OSWINDOWS}stdcall{$else}cdecl{$endif};
    function Fake_AddRef: TIntCnt;   {$ifdef OSWINDOWS}stdcall{$else}cdecl{$endif};
    function Fake_Release: TIntCnt;  {$ifdef OSWINDOWS}stdcall{$else}cdecl{$endif};
  public
    /// create an instance, using the specified interface
    constructor Create(aFactory: TInterfaceFactory); reintroduce;
    /// retrieve one instance of this interface, increasing its RefCount
    procedure Get(out Obj);
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve one instance of this interface, without increasing its RefCount
    procedure GetNoAddRef(out Obj);
      {$ifdef HASINLINE} inline; {$endif}
  published
    /// the associated interface factory class
    property Factory: TInterfaceFactory
      read fFactory;
  end;
  {$M-}

  /// event used by TInterfaceFactory and TInterfacedObjectFake to run
  // a method from a fake instance using JSON marshaling for the parameters
  // - aMethod will specify which method is to be executed
  // - aParams will contain the input parameters, encoded as a JSON array,
  // without the [ ] characters (e.g. '1,"arg2",3')
  // - shall return TRUE on success, or FALSE in case of failure, with
  // a corresponding explanation in aErrorMsg
  // - method results shall be serialized as JSON in aResult;  if
  // aServiceCustomAnswer is not nil, the result shall use this record
  // to set HTTP custom content and headers, and ignore aResult content
  // - aClientDrivenID can be set optionally to specify e.g. an URI-level session
  TOnFakeInstanceInvoke = function(const aMethod: TInterfaceMethod;
    const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
    aFakeID: PInterfacedObjectFakeID;
    aServiceCustomAnswer: PServiceCustomAnswer): boolean of object;

  /// event called when destroying a TInterfaceFactory's fake instance
  /// - this method will be run when the fake class instance is destroyed
  // (e.g. if aInstanceCreation is sicClientDriven, to notify the server
  // than the client life time just finished)
  TOnFakeInstanceDestroy = procedure(aFakeID: TInterfacedObjectFakeID) of object;

  /// how TInterfacedObjectFake will perform its execution
  // - by default, fInvoke() will receive standard JSON content, unless
  // ifoJsonAsExtended is set, and extended JSON is used
  // - ifoDontStoreVoidJson will ensure objects and records won't include
  // default void fields in JSON serialization
  TInterfacedObjectFakeOption = (
    ifoJsonAsExtended,
    ifoDontStoreVoidJson);

  /// defines how TInterfacedObjectFakeRaw will perform its execution
  TInterfacedObjectFakeOptions = set of TInterfacedObjectFakeOption;

  /// instances of this class will emulate a given interface over JSON content
  // - as used e.g. by TInterfaceFactoryClient.CreateFakeInstance
  TInterfacedObjectFake = class(TInterfacedObjectFakeRaw)
  protected
    fOptions: TInterfacedObjectFakeOptions;
    fInvoke: TOnFakeInstanceInvoke;
    fNotifyDestroy: TOnFakeInstanceDestroy;
    fFakeID: TInterfacedObjectFakeID;
    fServiceFactory: TObject; // holds a TServiceFactory instance
    // the JITed asm stubs will redirect to these JSON-oriented process
    procedure FakeCallGetJsonFromStack(
      var ctxt: TFakeCallContext; var Json: RawUtf8);
    procedure FakeCallSetJsonToStack(var ctxt: TFakeCallContext; R: PUtf8Char);
    procedure FakeCallInternalProcess(var ctxt: TFakeCallContext); override;
    // should be overriden to support interface parameters (i.e. callbacks)
    procedure InterfaceWrite(W: TJsonWriter; const aMethod: TInterfaceMethod;
      const aParamInfo: TInterfaceMethodArgument; aParamValue: Pointer); virtual;
  public
    /// create an instance, using the specified interface and factory
    constructor Create(aFactory: TInterfaceFactory; aServiceFactory: TObject;
      aOptions: TInterfacedObjectFakeOptions;
      const aInvoke: TOnFakeInstanceInvoke;
      const aNotifyDestroy: TOnFakeInstanceDestroy); reintroduce;
    /// release the remote server instance (in sicClientDriven mode);
    destructor Destroy; override;
  published
    /// how TInterfacedObjectFake identify this instance
    // - match the ID used in sicClientDriven mode of a service
    // - match the TInterfacedObjectFakeServer 32-bit identifier of a callback
    property FakeID: TInterfacedObjectFakeID
      read fFakeID;
  end;

  /// abstract class defining a FakeInvoke() virtual method via a
  // TOnFakeInstanceInvoke signature
  TInterfacedObjectFakeCallback = class(TInterfacedObjectFake)
  protected
    fLogClass: TSynLogClass;
    fName: RawUtf8;
    // default abstract method will do nothing but log the call
    function FakeInvoke(const aMethod: TInterfaceMethod; const aParams: RawUtf8;
      aResult, aErrorMsg: PRawUtf8; aFakeID: PInterfacedObjectFakeID;
      aServiceCustomAnswer: PServiceCustomAnswer): boolean; virtual;
  end;


{ ************ TInterfaceMethodExecute for Method Execution from JSON }

type
  TInterfaceMethodExecuteRaw = class;

  /// possible service provider method options, e.g. about logging or execution
  // - see TServiceMethodOptions for a description of each available option
  TInterfaceMethodOption = (
    optExecLockedPerInterface,
    optExecInPerInterfaceThread,
    optFreeInPerInterfaceThread,
    optExecInMainThread,
    optFreeInMainThread,
    optVariantCopiedByReference,
    optInterceptInputOutput,
    optNoLogInput,
    optNoLogOutput,
    optErrorOnMissingParam,
    optForceStandardJson,
    optDontStoreVoidJson,
    optIgnoreException);

  /// set of per-method execution options for an interface-based service provider
  // - by default, method executions are concurrent, for better server
  // responsiveness; if you set optExecLockedPerInterface, all methods of
  // a given interface will be executed with a critical section
  // - optExecInMainThread will force the method to be called within
  // a RunningThread.Synchronize() call - it can be used e.g. if your
  // implementation rely heavily on COM servers - by default, service methods
  // are called within the thread which received them, on multi-thread server
  // instances (e.g. TSqlite3HttpServer or TRestServerNamedPipeResponse),
  // for better response time and CPU use (this is the technical reason why
  // service implementation methods have to handle multi-threading safety
  // carefully, e.g. by using TRTLCriticalSection mutex on purpose)
  // - optFreeInMainThread will force the _Release/Destroy method to be run
  // in the main thread: setting this option for any method will affect the
  // whole service class - is not set by default, for performance reasons
  // - optExecInPerInterfaceThread and optFreeInPerInterfaceThread will allow
  // creation of a per-interface dedicated thread
  // - if optInterceptInputOutput is set, TServiceFactoryServer.AddInterceptor()
  // events will have their Sender.Input/Output values defined
  // - if optNoLogInput/optNoLogOutput is set, TSynLog and SetServiceLog database
  // won't log any parameter values at input/output - this may be useful for
  // regulatory/safety purposes, e.g. to ensure that no sensitive information
  // (like a credit card number or a password), is logged during process -
  // consider using TInterfaceFactory.RegisterUnsafeSpiType() instead if you
  // prefer a more tuned filtering, for specific high-level types
  // - when parameters are transmitted as JSON object, any missing parameter
  // will be replaced by their default value, unless optErrorOnMissingParam
  // is defined to reject the call
  // - by default, it wil check for the client user agent, and use extended
  // JSON if none is found (e.g. from WebSockets), or if it contains 'mORMot':
  // you can set optForceStandardJson to ensure standard JSON is always returned
  // - optDontStoreVoidJson will reduce the JSON object verbosity by not writing
  // void (e.g. 0 or '') properties when serializing objects and records
  // - any exceptions will be propagated during execution, unless
  // optIgnoreException is set and the exception is trapped (not to be used
  // unless you know what you are doing)
  TInterfaceMethodOptions = set of TInterfaceMethodOption;

  /// available execution options for an interface-based service provider
  // - mimics TServiceFactoryServer homonymous boolean properties
  TInterfaceOption = (
    optByPassAuthentication,
    optResultAsJsonObject,
    optResultAsJsonObjectWithoutResult,
    optResultAsXMLObject,
    optResultAsXMLObjectIfAcceptOnlyXML,
    optExcludeServiceLogCustomAnswer);

  /// set of execution options for an interface-based service provider
  // - mimics TServiceFactoryServer homonymous boolean properties
  // - as used by TServiceFactoryServerAbstract.SetWholeOptions()
  TInterfaceOptions = set of TInterfaceOption;

  /// callback called by TInterfaceMethodExecute to process an interface
  // callback parameter
  // - implementation should set the Obj local variable to an instance of
  // a fake class implementing the aParamInfo interface
  TOnServiceMethodExecuteCallback =
    procedure(var Par: PUtf8Char; ParamInterfaceInfo: TRttiCustom; out Obj) of object;

  /// the current step of a TInterfaceMethodExecute.OnExecute call
  TInterfaceMethodExecuteEventStep = (
    smsUndefined,
    smsBefore,
    smsAfter,
    smsError);

  /// the TInterfaceMethodExecute.OnExecute signature
  // - optInterceptInputOutput should be defined in Options
  // - is called for each Step, i.e. smsBefore/smsAfter
  // - smsError is called when TInterfaceMethodExecute.LastException was raised
  TOnInterfaceMethodExecute = procedure(Sender: TInterfaceMethodExecuteRaw;
    Step: TInterfaceMethodExecuteEventStep) of object;

  /// store one or several TInterfaceMethodExecute.OnExecute signatures
  TInterfaceMethodExecuteEventDynArray = array of TOnInterfaceMethodExecute;

  /// abtract execution of a TInterfacedObject method
  TInterfaceMethodExecuteRaw = class
  protected
    fMethod: PInterfaceMethod;
    fRawUtf8s: TRawUtf8DynArray;
    fStrings: TStringDynArray;
    fWideStrings: TWideStringDynArray;
    fRecords: array of TBytes; // imvRecord or imvVariant
    fInt64s: TInt64DynArray;
    fObjects: TObjectDynArray;
    fInterfaces: TPointerDynArray;
    fDynArrays: array of record
      Value: Pointer;
      Wrapper: TDynArray;
    end;
    fValues: TPPointerDynArray;
    fAlreadyExecuted: boolean;
    fOnExecute: TInterfaceMethodExecuteEventDynArray;
    fCurrentStep: TInterfaceMethodExecuteEventStep;
    fBackgroundExecutionThread: TSynBackgroundThreadMethod;
    fLastException: Exception;
    fExecutedInstancesFailed: TRawUtf8DynArray;
    fOptions: TInterfaceMethodOptions;
    fInput: TDocVariantData;
    fOutput: TDocVariantData;
    procedure BeforeExecute;
    procedure RawExecute(const Instances: PPointerArray; InstancesLast: integer);
    procedure AfterExecute;
  public
    /// initialize the execution instance
    constructor Create(aMethod: PInterfaceMethod);
    /// allow to hook method execution
    // - if optInterceptInputOutput is defined in Options, then Sender.Input/Output
    // fields will contain the execution data context when Hook is called
    procedure AddInterceptor(const Hook: TOnInterfaceMethodExecute);
    /// allow to hook method execution
    // - if optInterceptInputOutput is defined in Options, then Sender.Input/Output
    // fields will contain the execution data context when Hook[] are called
    procedure AddInterceptors(const Hook: TInterfaceMethodExecuteEventDynArray);

    /// low-level direct access to the associated method information
    property Method: PInterfaceMethod
      read fMethod;
    /// low-level direct access to the current input/output parameter values
    // - you should not need to access this, but rather set
    // optInterceptInputOutput in Options, and read Input/Output content
    property Values: TPPointerDynArray
      read fValues;
    /// reference to the actual execution method callbacks
    property OnExecute: TInterfaceMethodExecuteEventDynArray
      read fOnExecute;
    /// the current state of the execution
    property CurrentStep: TInterfaceMethodExecuteEventStep
      read fCurrentStep write fCurrentStep;
    /// only set during AddInterceptor() callback execution, if Step is smsError
    property LastException: Exception
      read fLastException;
    /// reference to the background execution thread, if any
    property BackgroundExecutionThread: TSynBackgroundThreadMethod
      read fBackgroundExecutionThread write fBackgroundExecutionThread;
    /// associated settings, as copied from TServiceFactoryServer.Options
    property Options: TInterfaceMethodOptions
      read fOptions write fOptions;
    /// set if optInterceptInputOutput is defined in TServiceFactoryServer.Options
    // - contains a dvObject with input parameters as "argname":value pairs
    // - this is a read-only property: you cannot change the input content
    property Input: TDocVariantData
      read fInput;
    /// set if optInterceptInputOutput is defined in TServiceFactoryServer.Options
    // - contains a dvObject with output parameters as "argname":value pairs
    // - this is a read-only property: you cannot change the output content
    property Output: TDocVariantData
      read fOutput;
    /// contains exception serialization after ExecuteJson of multiple instances
    // - follows the Instances[] order as supplied to RawExecute/ExecuteJson
    // - if only a single Instances[] is supplied, the exception will be
    // propagated to the caller, unless optIgnoreException option is defined
    // - if more than one Instances[] is supplied, any raised Exception will
    // be serialized using ObjectToJsonDebug(), or this property will be left
    // to its default nil content if no exception occurred
    property ExecutedInstancesFailed: TRawUtf8DynArray
      read fExecutedInstancesFailed;
  end;

  /// execute a method of a TInterfacedObject instance, from/to JSON
  TInterfaceMethodExecute = class(TInterfaceMethodExecuteRaw)
  protected
    fTempTextWriter: TJsonWriter;
    fOnCallback: TOnInterfaceMethodExecuteCallback;
    fServiceCustomAnswerHead: RawUtf8;
    fServiceCustomAnswerStatus: cardinal;
  public
    /// finalize the execution instance
    destructor Destroy; override;
    /// execute the corresponding method of weak IInvokable references
    // - will retrieve a JSON array of parameters from Par (as [1,"par2",3])
    // - will append a JSON array of results in Res, or set an Error message, or
    // a JSON object (with parameter names) in Res if ResultAsJsonObject is set
    // - if one Instances[] is supplied, any exception will be propagated (unless
    // optIgnoreException is set); if more than one Instances[] is supplied,
    // corresponding ExecutedInstancesFailed[] property will be filled with
    // the JSON serialized exception
    function ExecuteJson(const Instances: array of pointer; Par: PUtf8Char;
      Res: TJsonWriter; Error: PShortString = nil; ResAsJsonObject: boolean = false): boolean;
    /// execute the corresponding method of one weak IInvokable reference
    // - exepect no output argument, i.e. no returned data, unless output is set
    // - this version will identify TInterfacedObjectFake implementations,
    // and will call directly fInvoke() if possible, to avoid JSON marshalling
    // - expect params value to be without [ ], just like TOnFakeInstanceInvoke
    function ExecuteJsonCallback(Instance: pointer; const params: RawUtf8;
      output: PRawUtf8): boolean;
    /// execute directly TInterfacedObjectFake.fInvoke()
    // - expect params value to be with [ ], just like ExecuteJson
    function ExecuteJsonFake(Instance: pointer; params: PUtf8Char): boolean;
    /// set from output TServiceCustomAnswer.Header result parameter
    property ServiceCustomAnswerHead: RawUtf8
      read fServiceCustomAnswerHead write fServiceCustomAnswerHead;
    /// set from output TServiceCustomAnswer.Status result parameter
    property ServiceCustomAnswerStatus: cardinal
      read fServiceCustomAnswerStatus write fServiceCustomAnswerStatus;
    /// points e.g. to TRestServerUriContext.ExecuteCallback which
    // redirects to TServiceContainerServer.GetFakeCallback
    property OnCallback: TOnInterfaceMethodExecuteCallback
      read fOnCallback write fOnCallback;
    /// allow to use an instance-specific temporary TOrmWriter
    function TempTextWriter: TJsonWriter;
  end;


/// low-level execution of a procedure of object in a given background thread
procedure BackgroundExecuteThreadMethod(const method: TThreadMethod;
  backgroundThread: TSynBackgroundThreadMethod);

/// low-level execution of TInterfacedObject._Release in a given background thread
procedure BackgroundExecuteInstanceRelease(instance: TObject;
  backgroundThread: TSynBackgroundThreadMethod);


/// low-level internal function returning the TServiceRunningContext threadvar
// - mormot.rest.server.pas' ServiceRunningContext function redirects to this
function PerThreadRunningContextAddress: pointer;


{ ************ SetWeak and SetWeakZero Weak Interface Reference }

/// assign a Weak interface reference, to be used for circular references
// - by default setting aInterface.Field := aValue will increment the internal
// reference count of the implementation object: when underlying objects reference
// each other via interfaces (e.g. as parent and children), what causes the
// reference count to never reach zero, therefore resulting in memory leaks
// - to avoid this issue, use this procedure instead
procedure SetWeak(aInterfaceField: PInterface; const aValue: IInterface);
  {$ifdef FPC}inline;{$endif} // raise Internal Error C2170 on some Delphis

/// assign a Weak interface reference, which will be ZEROed (set to nil) when
// the associated aObject and/or aValue will be released
// - this function is slower than SetWeak, but will avoid any GPF, by
// maintaining a list of per-instance weak interface field references, and
// hook the TObject.FreeInstance virtual method for proper zeroings
// - thread-safe implementation, using per-class locked lists
procedure SetWeakZero(aObject: TObject; aObjectInterfaceField: PInterface;
  const aValue: IInterface);


implementation


{.$define SOA_DEBUG} // write the low-level interface info as json


{ ************ IInvokable Interface Methods and Parameters RTTI Extraction }

procedure TInterfaceMethodArgument.SerializeToContract(WR: TJsonWriter);
const
  ARGDIRTOJSON: array[TInterfaceMethodValueDirection] of string[4] = (
  // convert into generic in/out direction (assume result is out)
    'in', 'both', 'out', 'out');
  // AnsiString (Delphi <2009) may loose data depending on the client
  ARGTYPETOJSON: array[TInterfaceMethodValueType] of string[8] = (
    '??',       // imvNone
    'self',     // imvSelf
    'boolean',  // imvBoolean
    '',         // imvEnum
    '',         // imvSet
    'integer',  // imvInteger
    'cardinal', // imvCardinal
    'int64',    // imvInt64
    'double',   // imvDouble
    'datetime', // imvDateTime
    'currency', // imvCurrency
    'utf8',     // imvRawUtf8
    'utf8',     // imvString
    'utf8',     // imvRawByteString
    'utf8',     // imvWideString
    'utf8',     // imvBinary
    '',         // imvRecord
    'variant',  // imvVariant
    '',         // imvObject
    'json',     // imvRawJson
    '',         // imvDynArray
    '');        // imvInterface
begin
  WR.AddShort('{"argument":"');
  WR.AddShort(ParamName^);
  WR.AddShort('","direction":"');
  WR.AddShort(ARGDIRTOJSON[ValueDirection]);
  WR.AddShort('","type":"');
  if ARGTYPETOJSON[ValueType] = '' then
    WR.AddShort(ArgRtti.Info.Name^)
  else
    WR.AddShort(ARGTYPETOJSON[ValueType]);
{$ifdef SOA_DEBUG}
  WR.Add('"', ',');
  WR.AddPropJsonInt64('index', IndexVar);
  WR.AddPropJsonString('var',
    GetEnumNameTrimed(TypeInfo(TInterfaceMethodValueVar), ValueVar));
  WR.AddPropJsonInt64('stackoffset', InStackOffset);
  WR.AddPropJsonInt64('reg', RegisterIdent);
  WR.AddPropJsonInt64('fpreg', FPRegisterIdent);
  WR.AddPropJsonInt64('stacksize', SizeInStack);
  WR.AddPropJsonInt64('storsize', SizeInStorage);
  if ValueType = imvBinary then
    WR.AddPropJsonInt64('binsize', SizeInBinary);
  WR.AddPropName('asm');
  WR.AddString(GetSetNameCsv(TypeInfo(TInterfaceMethodValueAsm), ValueKindAsm));
  WR.AddShort('}', ',');
{$else}
  WR.AddShorter('"},');
{$endif SOA_DEBUG}
end;

function TInterfaceMethodArgument.IsDefault(V: pointer): boolean;
begin
  result := false;
  case ValueType of
    imvBoolean..imvCurrency:
      case SizeInStorage of
        1:
          result := PByte(V)^ = 0;
        2:
          result := PWord(V)^ = 0;
        4:
          result := PInteger(V)^ = 0;
        8:
          result := PInt64(V)^ = 0;
      end;
    imvRawUtf8..imvWideString,
    imvObject..imvInterface:
      result := PPointer(V)^ = nil;
    imvBinary,
    imvRecord:
      result := IsZeroSmall(V, SizeInStorage);
    imvVariant:
      result := PVarData(V)^.vtype <= varNull;
  end;
end;

const
  JSONPARSER_SERVICE: TJsonParserOptions =
   [jpoHandleCustomVariants, jpoIgnoreUnknownEnum, jpoIgnoreUnknownProperty,
    jpoIgnoreStringType, jpoAllowInt64Hex, jpoNullDontReleaseObjectInstance];

function TInterfaceMethodArgument.FromJson(const MethodName: RawUtf8;
  var R: PUtf8Char; V: pointer; Error: PShortString;
  DVO: TDocVariantOptions): boolean;
var
  tmp: ShortString;
  ctxt: TJsonParserContext;
begin
  ctxt.Init(R, ArgRtti, JSONPARSER_SERVICE, @DVO, nil);
  if ArgRtti.JsonLoad = nil then
    // fallback to raw record RTTI binary unserialization with Base64 encoding
    ctxt.Valid := ctxt.ParseNext and
              (ctxt.Value <> nil) and
              (PCardinal(ctxt.Value)^ and $ffffff = JSON_BASE64_MAGIC_C) and
              BinaryLoadBase64(pointer(ctxt.Value + 3), ctxt.ValueLen - 3,
                V, ctxt.Info.Info, {uri=}false, rkRecordTypes, {withcrc=}false)
  else
    // use direct TRttiJson unserialization
    TRttiJsonLoad(ArgRtti.JsonLoad)(V, ctxt);
  if not ctxt.Valid then
  begin
    FormatShort('I% failed parsing %: % from input JSON',
      [MethodName, ParamName^, ArgTypeName^], tmp);
    if Error = nil then
      raise EInterfaceFactory.CreateUtf8('%', [tmp]);
    Error^ := tmp;
    result := false;
  end
  else
  begin
    R := ctxt.Json;
    result := true;
  end;
end;

procedure TInterfaceMethodArgument.AddJson(WR: TJsonWriter; V: pointer;
  ObjectOptions: TTextWriterWriteObjectOptions);
var
  ctxt: TJsonSaveContext;
begin
  if ArgRtti.JsonSave <> nil then
  begin
    // use direct TRttiJson serialization
    {%H-}ctxt.Init(WR, ObjectOptions, ArgRtti);
    TRttiJsonSave(ArgRtti.JsonSave)(V, ctxt);
  end
  else
    // fallback to raw record RTTI binary serialization with Base64 encoding
    WR.BinarySaveBase64(V, ArgRtti.Info, rkRecordTypes,
      {magic=}true, {withcrc=}false);
end;

procedure TInterfaceMethodArgument.AsJson(var DestValue: RawUtf8; V: pointer);
var
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  case ValueType of  // some direct conversion of simple types into RawUtf8
    imvBoolean:
      DestValue := BOOL_UTF8[PBoolean(V)^];
    imvEnum..imvInt64:
      case SizeInStorage of
        1:
          UInt32ToUtf8(PByte(V)^, DestValue);
        2:
          UInt32ToUtf8(PWord(V)^, DestValue);
        4:
          if ValueType = imvInteger then
            Int32ToUtf8(PInteger(V)^, DestValue)
          else
            UInt32ToUtf8(PCardinal(V)^, DestValue);
        8:
          if vIsQword in ValueKindAsm then
            UInt64ToUtf8(PQword(V)^, DestValue)
          else
            Int64ToUtf8(PInt64(V)^, DestValue);
      end;
    imvDouble:
      DoubleToStr(unaligned(PDouble(V)^), DestValue);
    imvCurrency:
      Curr64ToStr(PInt64(V)^, DestValue);
    imvRawJson:
      DestValue := PRawUtf8(V)^;
  else
    begin
      // use generic AddJson() method for complex "..." content
      W := TJsonWriter.CreateOwnedStream(temp);
      try
        AddJson(W, V);
        W.SetText(DestValue);
      finally
        W.Free;
      end;
    end;
  end;
end;

procedure TInterfaceMethodArgument.AddJsonEscaped(WR: TJsonWriter; V: pointer);
var
  W: TJsonWriter;
begin
  if ValueType in [imvBoolean..imvCurrency, imvInterface] then
    // no need to escape those
    AddJson(WR, V)
  else
  begin
    W := WR.GetTempJsonWriter;
    AddJson(W, V);
    WR.AddJsonEscape(W);
  end;
end;

procedure TInterfaceMethodArgument.AddValueJson(WR: TJsonWriter; const Value: RawUtf8);
begin
  if vIsString in ValueKindAsm then
  begin
    WR.Add('"');
    WR.AddJsonEscape(pointer(Value));
    WR.Add('"', ',');
  end
  else
  begin
    WR.AddString(Value);
    WR.AddComma;
  end;
end;

procedure TInterfaceMethodArgument.AddDefaultJson(WR: TJsonWriter);
begin
  case ValueType of
    imvBoolean:
      WR.AddShorter('false,');
    imvObject:
      WR.AddShorter('null,'); // may raise an error on the client side
    imvInterface:
      WR.AddShorter('0,');
    imvDynArray:
      WR.AddShorter('[],');
    imvRecord:
      begin
        WR.AddVoidRecordJson(ArgRtti.Info);
        WR.AddComma;
      end;
    imvVariant:
      WR.AddShorter('null,');
  else
    if vIsString in ValueKindAsm then
      WR.AddShorter('"",')
    else
      WR.AddShorter('0,');
  end;
end;

procedure TInterfaceMethodArgument.AsVariant(var DestValue: variant; V: pointer;
  Options: TDocVariantOptions);
var
  tmp: RawUtf8;
begin
  case ValueType of // some direct conversion of simple types
    imvBoolean:
      DestValue := PBoolean(V)^;
    imvEnum..imvInt64:
      case SizeInStorage of
        1:
          DestValue := PByte(V)^;
        2:
          DestValue := PWord(V)^;
        4:
          if ValueType = imvInteger then
            DestValue := PInteger(V)^
          else
            DestValue := PCardinal(V)^;
        8:
          if vIsQword in ValueKindAsm then
            DestValue := PQWord(V)^
          else
            DestValue := PInt64(V)^;
      end;
    imvDouble,
    imvDateTime:
      DestValue := unaligned(PDouble(V)^);
    imvCurrency:
      DestValue := PCurrency(V)^;
    imvRawUtf8:
      RawUtf8ToVariant(PRawUtf8(V)^, DestValue);
    imvString:
      begin
        StringToUtf8(PString(V)^, tmp);
        RawUtf8ToVariant(tmp, DestValue);
      end;
    imvWideString:
      begin
        RawUnicodeToUtf8(PPointer(V)^, length(PWideString(V)^), tmp);
        RawUtf8ToVariant(tmp, DestValue);
      end;
    imvVariant:
      DestValue := PVariant(V)^;
  else
    begin
      // use generic AddJson() method for complex "..." content
      AsJson(tmp, V);
      JsonToVariantInPlace(DestValue, pointer(tmp), Options);
    end;
  end;
end;

procedure TInterfaceMethodArgument.AddAsVariant(
  var Dest: TDocVariantData; V: pointer);
var
  tmp: variant;
begin
  AsVariant(tmp, V, Dest.Options);
  if Dest.IsArray then
    Dest.AddItem(tmp)
  else
    Dest.AddValue(ShortStringToAnsi7String(ParamName^), tmp);
end;

procedure TInterfaceMethodArgument.FixValueAndAddToObject(const Value: variant;
  var DestDoc: TDocVariantData);
var
  tempCopy: variant;
begin
  tempCopy := Value;
  FixValue(tempCopy);
  DestDoc.AddValue(ShortStringToAnsi7String(ParamName^), tempCopy);
end;

procedure TInterfaceMethodArgument.FixValue(var Value: variant);
var
  enum: Int64;
  obj: TObject;
  arr: pointer;
  dyn: TDynArray;
  rec: TByteDynArray;
  json: RawUtf8;
begin
  case ValueType of
    imvEnum:
      if VariantToInt64(Value, enum) then
        Value := ArgRtti.Cache.EnumInfo.GetEnumNameOrd(enum)^;
    imvSet:
      if VariantToInt64(Value, enum) then
        Value := SetNameToVariant(enum, ArgRtti);
    imvObject:
      begin
        obj := ArgRtti.ClassNewInstance;
        try
          if DocVariantToObject(_Safe(Value)^, obj, ArgRtti) then
            Value := _ObjFast(obj, [woEnumSetsAsText]);
        finally
          obj.Free;
        end;
      end;
    imvDynArray:
      if _Safe(Value)^.IsArray then
      begin
        arr := nil; // recreate using a proper dynamic array
        dyn.InitRtti(ArgRtti, arr);
        try
          VariantSaveJson(Value, twJsonEscape, json);
          dyn.LoadFromJson(pointer(json));
          json := dyn.SaveToJson({EnumSetsAsText=}true);
          _Json(json, Value, JSON_FAST);
        finally
          dyn.Clear;
        end;
      end;
    imvRecord:
      if _Safe(Value)^.IsObject then
      begin
        SetLength(rec, ArgRtti.Size);
        try
          VariantSaveJson(Value, twJsonEscape, json);
          RecordLoadJson(rec[0], pointer(json), ArgRtti.Info);
          json := SaveJson(rec[0], ArgRtti.Info, {EnumSetsAsText=}true);
          _Json(json, Value, JSON_FAST);
        finally
          ArgRtti.ValueFinalize(pointer(rec));
        end;
      end;
  end;
end;


{ TInterfaceMethod }

function TInterfaceMethod.ArgIndex(ArgName: PUtf8Char; ArgNameLen: integer;
  Input: boolean): PtrInt;
begin
  if ArgNameLen > 0 then
    if Input then
    begin
      for result := ArgsInFirst to ArgsInLast do
        with Args[result] do
          if (ValueDirection in [imdConst, imdVar]) and
             IdemPropName(ParamName^, ArgName, ArgNameLen) then
              exit;
    end
    else
      for result := ArgsOutFirst to ArgsOutLast do
        with Args[result] do
          if (ValueDirection <> imdConst) and
             IdemPropName(ParamName^, ArgName, ArgNameLen) then
              exit;
  result := -1;
end;

function TInterfaceMethod.ArgNextInput(var arg: integer): boolean;
begin
  result := true;
  inc(arg);
  while arg <= ArgsInLast do
    if Args[arg].ValueDirection in [imdConst, imdVar] then
      exit
    else
      inc(arg);
  result := false;
end;

function TInterfaceMethod.ArgNextOutput(var arg: integer): boolean;
begin
  result := true;
  inc(arg);
  while arg <= ArgsOutLast do
    if Args[arg].ValueDirection <> imdConst then
      exit
    else
      inc(arg);
  result := false;
end;

function TInterfaceMethod.ArgsArrayToObject(P: PUtf8Char; Input: boolean): RawUtf8;
var
  i: integer;
  W: TJsonWriter;
  Value: PUtf8Char;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.Add('{');
    if (P = nil) or
       (P^ <> '[') then
      P := nil
    else
      inc(P);
    for i := 1 to length(Args) - 1 do
      if P = nil then
        break
      else
        with Args[i] do
        begin
          if Input then
          begin
            if ValueDirection in [imdOut, imdResult] then
              continue;
          end
          else if ValueDirection = imdConst then
            continue;
          W.AddPropName(ParamName^);
          P := GotoNextNotSpace(P);
          Value := P;
          P := GotoEndJsonItem(P);
          if P^ = ',' then
            inc(P); // include ending ','
          W.AddNoJsonEscape(Value, P - Value);
        end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TInterfaceMethod.ArgsCommandLineToObject(P: PUtf8Char;
  Input, RaiseExceptionOnUnknownParam: boolean): RawUtf8;
var
  i: integer;
  W: TJsonWriter;
  B: PUtf8Char;
  arginfo: PInterfaceMethodArgument;
  arg, value: RawUtf8;
  ok: boolean;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.Add('{');
    while (P <> nil) and
          GetNextFieldProp(P, arg) and
          (P <> nil) and
          (arg <> '') do
    begin
      ok := true;
      i := ArgIndex(pointer(arg), length(arg), Input);
      if i < 0 then
        if RaiseExceptionOnUnknownParam then
          raise EInterfaceFactory.CreateUtf8('Unexpected [%] parameter for %',
            [arg, InterfaceDotMethodName])
        else
          ok := false;
      arginfo := @Args[i];
      if ok then
        W.AddPropName(arginfo^.ParamName^);
      if not (P^ in [':', '=']) then
        raise EInterfaceFactory.CreateUtf8('"%" parameter has no = for %',
          [arg, InterfaceDotMethodName]);
      P := GotoNextNotSpace(P + 1);
      if P^ in ['"', '[', '{'] then
      begin
        // name='"value"' or name='{somejson}'
        B := P;
        P := GotoEndJsonItem(P);
        if P = nil then
          raise EInterfaceFactory.CreateUtf8('%= parameter has invalid content for %',
            [arg, InterfaceDotMethodName]);
        if not ok then
          continue;
        W.AddNoJsonEscape(B, P - B);
      end
      else
      begin
        // name=value
        GetNextItem(P, ' ', value);
        if not ok then
          continue;
        if arginfo^.ValueType = imvDynArray then
          // write [value] or ["value"]
          W.Add('[');
        if arginfo^.ValueKindAsm * [vIsString, vIsDynArrayString] <> [] then
          W.AddJsonString(value)
        else
          W.AddNoJsonEscape(pointer(value), length(value));
        if arginfo^.ValueType = imvDynArray then
          W.Add(']');
      end;
      W.AddComma;
    end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TInterfaceMethod.ArgsNames(Input: boolean): TRawUtf8DynArray;
var
  a, n: PtrInt;
begin
  result := nil;
  if Input then
  begin
    SetLength(result, ArgsInputValuesCount);
    n := 0;
    for a := ArgsInFirst to ArgsInLast do
      if Args[a].ValueDirection in [imdConst, imdVar] then
      begin
        ShortStringToAnsi7String(Args[a].ParamName^, result[n]);
        inc(n);
      end;
  end
  else
  begin
    SetLength(result, ArgsOutputValuesCount);
    n := 0;
    for a := ArgsOutFirst to ArgsOutLast do
      if Args[a].ValueDirection <> imdConst then
      begin
        ShortStringToAnsi7String(Args[a].ParamName^, result[n]);
        inc(n);
      end;
  end;
end;

procedure TInterfaceMethod.ArgsStackAsDocVariant(const Values: TPPointerDynArray;
  out Dest: TDocVariantData; Input: boolean);
var
  a: PtrInt;
begin
  if Input then
  begin
    Dest.InitFast(ArgsInputValuesCount, dvObject);
    for a := ArgsInFirst to ArgsInLast do
      if Args[a].ValueDirection in [imdConst, imdVar] then
        Args[a].AddAsVariant(Dest, Values[a]);
  end
  else
  begin
    Dest.InitFast(ArgsOutputValuesCount, dvObject);
    for a := ArgsOutFirst to ArgsOutLast do
      if Args[a].ValueDirection <> imdConst then
        Args[a].AddAsVariant(Dest, Values[a]);
  end;
end;

procedure TInterfaceMethod.ArgsValuesAsDocVariant(
  Kind: TInterfaceMethodParamsDocVariantKind; out Dest: TDocVariantData;
  const Values: TVariantDynArray; Input: boolean; Options: TDocVariantOptions);
begin
  case Kind of
    pdvObject,
    pdvObjectFixed:
      begin
        Dest.InitObjectFromVariants(ArgsNames(Input), Values, Options);
        if Kind = pdvObjectFixed then
          ArgsAsDocVariantFix(Dest, Input);
      end;
    pdvArray:
      Dest.InitArrayFromVariants(Values, Options);
  else
    Dest.Init(Options);
  end;
end;

procedure TInterfaceMethod.ArgsAsDocVariantObject(
  const ArgsParams: TDocVariantData; var ArgsObject: TDocVariantData;
  Input: boolean);
var
  a, n: PtrInt;
begin
  if (ArgsParams.Count = 0) or
     (ArgsParams.Kind <> dvArray) then
    exit;
  if ArgsObject.Kind = dvUndefined then
    ArgsObject.Init(ArgsParams.Options);
  ArgsObject.Capacity := ArgsObject.Count + ArgsParams.Count;
  n := 0;
  if Input then
  begin
    if ArgsParams.Count = integer(ArgsInputValuesCount) then
      for a := ArgsInFirst to ArgsInLast do
        if Args[a].ValueDirection in [imdConst, imdVar] then
        begin
          ArgsObject.AddValue(
            ShortStringToAnsi7String(Args[a].ParamName^),
            ArgsParams.Values[n]);
          inc(n);
        end;
  end
  else
  begin
    if ArgsParams.Count = integer(ArgsOutputValuesCount) then
      for a := ArgsOutFirst to ArgsOutLast do
        if Args[a].ValueDirection <> imdConst then
        begin
          ArgsObject.AddValue(
            ShortStringToAnsi7String(Args[a].ParamName^),
            ArgsParams.Values[n]);
          inc(n);
        end;
  end;
end;

procedure TInterfaceMethod.ArgsAsDocVariantFix(var ArgsObject: TDocVariantData;
  Input: boolean);
var
  a, ndx: PtrInt;
  doc: TDocVariantData;
begin
  if ArgsObject.Count > 0 then
    case ArgsObject.Kind of
      dvObject:
        for a := 0 to ArgsObject.Count - 1 do
        begin
          ndx := ArgIndex(
            pointer(ArgsObject.Names[a]), length(ArgsObject.Names[a]), Input);
          if ndx >= 0 then
            Args[ndx].FixValue(ArgsObject.Values[a]);
        end;
      dvArray:
        if Input then
        begin
          if ArgsObject.Count <> integer(ArgsInputValuesCount) then
            exit;
          doc.Init(ArgsObject.Options);
          for a := ArgsInFirst to ArgsInLast do
            if Args[a].ValueDirection in [imdConst, imdVar] then
              Args[a].FixValueAndAddToObject(ArgsObject.Values[doc.Count], doc);
          ArgsObject := doc;
        end
        else
        begin
          if ArgsObject.Count <> integer(ArgsOutputValuesCount) then
            exit;
          doc.Init(ArgsObject.Options);
          for a := ArgsOutFirst to ArgsOutLast do
            if Args[a].ValueDirection <> imdConst then
              Args[a].FixValueAndAddToObject(ArgsObject.Values[doc.Count], doc);
          ArgsObject := doc;
        end;
    end;
end;


{ ************ TInterfacedObjectFake with JITted Methods Execution }

{ TInterfacedObjectFakeRaw }

constructor TInterfacedObjectFakeRaw.Create(aFactory: TInterfaceFactory);
begin
  inherited Create;
  fFactory := aFactory;
  fVTable := fFactory.GetMethodsVirtualTable;
end;

procedure TInterfacedObjectFakeRaw.FakeCallRaiseError(
  var ctxt: TFakeCallContext; const Format: RawUtf8; const Args: array of const);
var
  msg: RawUtf8;
begin
  msg := FormatUtf8(Format, Args);
  raise EInterfaceFactory.CreateUtf8('%.FakeCall(%.%) failed: %',
    [self, fFactory.fInterfaceName, ctxt.method^.Uri, msg]);
end;

procedure TInterfacedObjectFakeRaw.FakeCallGetParamsFromStack(
  var ctxt: TFakeCallContext);
var
  V: PPointer;
  arg: integer;
begin
  FillCharFast(ctxt.I64s, ctxt.Method^.ArgsUsedCount[imvv64] * SizeOf(Int64), 0);
  for arg := 1 to high(ctxt.Method^.Args) do
    with ctxt.Method^.Args[arg] do
      if ValueType > imvSelf then
      begin
        V := nil;
        {$ifdef CPUX86}
        case RegisterIdent of
          REGEAX:
            FakeCallRaiseError(ctxt, 'unexpected self', []);
          REGEDX:
            V := @ctxt.Stack.EDX;
          REGECX:
            V := @ctxt.Stack.ECX;
        else
        {$else}
        {$ifdef HAS_FPREG} // x64, armhf, aarch64
        if FPRegisterIdent > 0 then
          V := @ctxt.Stack.FPRegs[FPRegisterIdent + (FPREG_FIRST - 1)]
        else
        {$endif HAS_FPREG}
          if RegisterIdent > 0 then
            V := @ctxt.Stack.ParamRegs[RegisterIdent + (PARAMREG_FIRST - 1)];
        if RegisterIdent = PARAMREG_FIRST then
          FakeCallRaiseError(ctxt, 'unexpected self', []);
        {$endif CPUX86}
          if V = nil then
            if (SizeInStack > 0) and
               (InStackOffset <> STACKOFFSET_NONE) then
              V := @ctxt.Stack.Stack[InStackOffset]
            else
              V := @ctxt.I64s[IndexVar]; // for results in CPU
        {$ifdef CPUX86}
        end; // case RegisterIdent of
        {$endif CPUX86}
        if vPassedByReference in ValueKindAsm then
          V := PPointer(V)^;
        if ValueType = imvDynArray then
          {%H-}ctxt.DynArrays[IndexVar].InitRtti(ArgRtti, V^);
        ctxt.Value[arg] := V;
      end;
  if ctxt.Method^.ArgsResultIsServiceCustomAnswer then
    ctxt.ServiceCustomAnswerPoint := ctxt.Value[ctxt.Method^.ArgsResultIndex]
  else
    ctxt.ServiceCustomAnswerPoint := nil;
end;

{$ifdef HASINLINE}
function TInterfacedObjectFakeRaw.SelfFromInterface: TInterfacedObjectFakeRaw;
begin
  // obfucated but very efficient once inlined
  result := pointer(PAnsiChar(self) - PAnsiChar(@TInterfacedObjectFake(nil).fVTable));
end;
{$else}
function TInterfacedObjectFakeRaw.SelfFromInterface: TInterfacedObjectFakeRaw;
asm
        // asm version for oldest Delphi 7
        sub     eax, TInterfacedObjectFake.fVTable
end;
{$endif HASINLINE}

function TInterfacedObjectFakeRaw.FakeCall(stack: PFakeCallStack): Int64;
var
  ctxt: TFakeCallContext;
begin
  (*
     WELCOME ABOARD: you just landed in TInterfacedObjectFake.FakeCall() !
     if your debugger reached here, you are executing a "fake" interface
     forged to call a remote SOA server or mock/stub an interface
  *)
  self := SelfFromInterface;
  // setup context
  ctxt.Stack := stack;
  if stack.MethodIndex >= fFactory.MethodsCount then
    raise EInterfaceFactory.CreateUtf8('%.FakeCall(%) failed: out of range %',
      [self, fFactory.fInterfaceName, stack.MethodIndex]);
  ctxt.Method := @fFactory.fMethods[stack.MethodIndex];
  ctxt.ResultType := imvNone;
  ctxt.Result := @result;
  // call execution virtual method
  result := 0;
  self.FakeCallInternalProcess(ctxt);
  // handle float result if needed (ordinals are already stored in result)
  {$ifdef HAS_FPREG} // result float is returned in FP first register
  if ctxt.ResultType in [imvDouble, imvDateTime] then
    PInt64(@stack.FPRegs[FPREG_FIRST])^ := result;
  {$else}
  {$ifdef CPUINTEL} // x87 ABI expects floats to be in st(0) FPU stack
  case ctxt.ResultType of
    imvDouble,
    imvDateTime:
      asm
        fld     qword ptr [result]
      end;
    imvCurrency:
      asm
        fild    qword ptr [result]
      end;
  end;
  {$endif CPUINTEL}
  {$endif HAS_FPREG}
end;

function TInterfacedObjectFakeRaw.Fake_AddRef: TIntCnt;
begin
  result := SelfFromInterface._AddRef;
end;

function TInterfacedObjectFakeRaw.Fake_Release: TIntCnt;
begin
  result := SelfFromInterface._Release;
end;

function TInterfacedObjectFakeRaw.FakeQueryInterface(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} IID: TGUID;
  out Obj): TIntQry;
begin
  self := SelfFromInterface;
  if IsEqualGuid(@IID, @fFactory.fInterfaceIID) then
  begin
    pointer(Obj) := @fVTable;
    _AddRef;
    result := S_OK;
  end
  else if GetInterface(IID, Obj) then
    result := S_OK
  else
    result := TIntQry(E_NOINTERFACE);
end;

procedure TInterfacedObjectFakeRaw.Get(out Obj);
begin
  pointer(Obj) := @fVTable;
  _AddRef;
end;

procedure TInterfacedObjectFakeRaw.GetNoAddRef(out Obj);
begin
  pointer(Obj) := @fVTable;
end;


{ TInterfacedObjectFake }

constructor TInterfacedObjectFake.Create(aFactory: TInterfaceFactory;
  aServiceFactory: TObject; aOptions: TInterfacedObjectFakeOptions;
  const aInvoke: TOnFakeInstanceInvoke; const aNotifyDestroy: TOnFakeInstanceDestroy);
begin
  inherited Create(aFactory);
  fOptions := aOptions;
  fInvoke := aInvoke;
  fNotifyDestroy := aNotifyDestroy;
  fServiceFactory := aServiceFactory;
end;

destructor TInterfacedObjectFake.Destroy;
var
  C: TClass;
begin
  if Assigned(fNotifyDestroy) then
  try // release server instance
    fNotifyDestroy(fFakeID);
  except
    on E: Exception do
    begin
      C := E.ClassType;
      if C.InheritsFrom(EInterfaceFactory) or
         (C = EAccessViolation) or
         (C = EInvalidPointer) then
        raise; // propagate only dangerous exceptions
    end;
  end;
  inherited Destroy;
end;

procedure TInterfacedObjectFake.FakeCallGetJsonFromStack(
  var ctxt: TFakeCallContext; var Json: RawUtf8);
var
  Params: TJsonWriter;
  opt: TTextWriterWriteObjectOptions;
  arg: integer;
  V: PPointer;
  temp: TTextWriterStackBuffer;
begin
  FakeCallGetParamsFromStack(ctxt);
  // generate the ParamsJson input from c^.Value[]
  Params := TJsonWriter.CreateOwnedStream(temp);
  try
    if ifoJsonAsExtended in fOptions then
      Params.CustomOptions := Params.CustomOptions + [twoForceJsonExtended]
    else // e.g. for AJAX
      Params.CustomOptions := Params.CustomOptions + [twoForceJsonStandard];
    if ifoDontStoreVoidJson in fOptions then
    begin
      opt := DEFAULT_WRITEOPTIONS[true];
      Params.CustomOptions := Params.CustomOptions + [twoIgnoreDefaultInRecord];
    end
    else
      opt := DEFAULT_WRITEOPTIONS[false];
    for arg := 1 to high(ctxt.Method^.Args) do
      with ctxt.Method^.Args[arg] do
        if (ValueType > imvSelf) and
           (ValueDirection in [imdConst, imdVar]) then
        begin
          V := ctxt.Value[arg];
          case ValueType of
            imvInterface:
              InterfaceWrite(Params, ctxt.Method^, ctxt.Method^.Args[arg], V^);
            imvDynArray:
              begin
                if vIsObjArray in ValueKindAsm then
                  Params.AddObjArrayJson(V^, opt)
                else
                  Params.AddDynArrayJson(ctxt.DynArrays[IndexVar]);
                Params.AddComma;
              end;
          else
            begin
              AddJson(Params, V, opt);
              Params.AddComma;
            end;
          end;
        end;
    Params.CancelLastComma;
    Params.SetText(Json); // without [ ]
  finally
    Params.Free;
  end;
end;

procedure TInterfacedObjectFake.FakeCallSetJsonToStack(
  var ctxt: TFakeCallContext; R: PUtf8Char);
var
  arg, ValLen: integer;
  V: PPointer;
  Val: PUtf8Char;
  resultAsJsonObject: boolean;
begin
  if R <> nil then
  begin
    if R^ in [#1..' '] then
      repeat
        inc(R)
      until not (R^ in [#1..' ']);
    resultAsJsonObject := false; // [value,...] JSON array format
    if R^ = '{' then
      // {"paramname":value,...} JSON object format
      resultAsJsonObject := true
    else if R^ <> '[' then
      FakeCallRaiseError(ctxt, 'JSON array/object result expected', []);
    inc(R);
    arg := ctxt.Method^.ArgsOutFirst;
    if arg > 0 then
      repeat
        if resultAsJsonObject then
        begin
          Val := GetJsonPropName(R, @ValLen);
          if Val = nil then
            // end of JSON object
            break;
          // optimistic process of JSON object with in-order parameters
          if (arg > 0) and
            not IdemPropName(ctxt.Method^.Args[arg].ParamName^, Val, ValLen) then
          begin
            // slower but safe ctxt.Method when not in-order
            arg := ctxt.Method^.ArgIndex(Val, ValLen, false);
            if arg < 0 then
              FakeCallRaiseError(ctxt, 'unexpected parameter [%]', [Val]);
          end;
        end;
        with ctxt.Method^.Args[arg] do
        begin
          //assert(ValueDirection in [imdVar,imdOut,imdResult]);
          V := ctxt.Value[arg];
          FromJson(ctxt.Method^.InterfaceDotMethodName, R, V, nil,
            fFactory.DocVariantOptions);
          if ValueDirection = imdResult then
          begin
            ctxt.ResultType := ValueType;
            if ValueType in [imvBoolean..imvCurrency] then
              // ordinal/real result values to CPU/FPU registers
              MoveFast(V^, ctxt.Result^, SizeInStorage);
          end;
        end;
        if R = nil then
          break;
        if R^ in [#1..' '] then
          repeat
            inc(R)
          until not (R^ in [#1..' ']);
        if resultAsJsonObject then
        begin
          if (R^ = #0) or
             (R^ = '}') then
            break
          else
          // end of JSON object
          if not ctxt.Method^.ArgNextOutput(arg) then
            // no next result argument -> force manual search
            arg := 0;
        end
        else if not ctxt.Method^.ArgNextOutput(arg) then
          // end of JSON array
          break;
      until false;
  end
  else if ctxt.Method^.ArgsOutputValuesCount > 0 then
    FakeCallRaiseError(ctxt, 'method returned value, but OutputJson=''''', []);
end;

procedure TInterfacedObjectFake.FakeCallInternalProcess(var ctxt: TFakeCallContext);
var
  Error, OutputJson, InputJson: RawUtf8;
begin
  // serialize const/var input parameters into InputJson as array without []
  if not Assigned(fInvoke) then
    FakeCallRaiseError(ctxt, 'fInvoke=nil', []);
  FakeCallGetJsonFromStack(ctxt, InputJson);
  // call remote server or stub implementation using JSON input/output
  if not fInvoke(ctxt.Method^, InputJson,
      @OutputJson, @Error, @fFakeID, ctxt.ServiceCustomAnswerPoint) then
    FakeCallRaiseError(ctxt, '''%''', [Error]);
  // unserialize var/out/result parameters from OutputJson array/object
  if ctxt.ServiceCustomAnswerPoint = nil then
    FakeCallSetJsonToStack(ctxt, pointer(OutputJson));
end;

procedure TInterfacedObjectFake.InterfaceWrite(W: TJsonWriter;
  const aMethod: TInterfaceMethod; const aParamInfo: TInterfaceMethodArgument;
  aParamValue: Pointer);
begin
  raise EInterfaceFactory.CreateUtf8('%: unhandled %.%(%: %) argument',
    [self, fFactory.fInterfaceName, aMethod.Uri, aParamInfo.ParamName^,
     aParamInfo.ArgTypeName^]);
end;



{ ************  TInterfaceFactory Generating Runtime Implementation Class }

{ TInterfaceFactory }

const
  {$ifdef UNICODE}
  imvSynUnicode = imvNone;
  imvUnicodeString = imvString;
  {$else}
  imvSynUnicode = imvWideString;
  imvUnicodeString = imvNone;
  {$endif UNICODE}

  /// which TRttiParserType are actually serialized as JSON Strings
  _SMV_STRING =
    [imvRawUtf8..imvBinary, imvDateTime];

  _FROM_RTTI: array[TRttiParserType] of TInterfaceMethodValueType = (
    imvNone,           //  ptNone
    imvNone,           //  ptArray
    imvBoolean,        //  ptBoolean
    imvNone,           //  ptByte
    imvCardinal,       //  ptCardinal
    imvCurrency,       //  ptCurrency
    imvDouble,         //  ptDouble
    imvNone,           //  ptExtended
    imvInt64,          //  ptInt64
    imvInteger,        //  ptInteger
    imvInt64,          //  ptQWord
    imvRawByteString,  //  ptRawByteString
    imvRawJson,        //  ptRawJson
    imvRawUtf8,        //  ptRawUtf8
    imvRecord,         //  ptRecord
    imvDouble,         //  ptSingle
    imvString,         //  ptString
    imvSynUnicode,     //  ptSynUnicode
    imvDateTime,       //  ptDateTime
    imvDateTime,       //  ptDateTimeMS
    imvBinary,         //  ptGuid
    imvBinary,         //  ptHash128
    imvBinary,         //  ptHash256
    imvBinary,         //  ptHash512
    imvInt64,          //  ptOrm
    imvInt64,          //  ptTimeLog
    imvUnicodeString,  //  ptUnicodeString
    imvInt64,          //  ptUnixTime
    imvInt64,          //  ptUnixMSTime
    imvVariant,        //  ptVariant
    imvWideString,     //  ptWideString
    imvRawUtf8,        //  ptWinAnsi
    imvNone,           //  ptWord
    imvEnum,           //  ptEnumeration
    imvSet,            //  ptSet
    imvObject,         //  ptClass
    imvDynArray,       //  ptDynArray
    imvInterface,      //  ptInterface
    imvNone);          //  ptCustom

var
  InterfaceFactoryCache: TSynObjectListLightLocked;

function InitializeInterfaceFactoryCache: TSynObjectListLightLocked;
begin
  GlobalLock;
  try
    if InterfaceFactoryCache = nil then // paranoid thread-safety
      InterfaceFactoryCache :=
        RegisterGlobalShutdownRelease(TSynObjectListLightLocked.Create);
  finally
    GlobalUnLock;
  end;
  result := InterfaceFactoryCache;
end;

function FactorySearch(F: PInterfaceFactory; n: integer; nfo: PRttiInfo): TInterfaceFactory;
begin
  if n <> 0 then
    repeat
      result := F^;
      if result.fInterfaceTypeInfo = nfo then
        exit;
      inc(F);
      dec(n);
    until n = 0;
  result := nil;
end;

class function TInterfaceFactory.Get(aInterface: PRttiInfo): TInterfaceFactory;
var
  cache: TSynObjectListLightLocked;
begin
  if (aInterface = nil) or
     (aInterface^.Kind <> rkInterface) then
    raise EInterfaceFactory.CreateUtf8('%.Get(invalid)', [self]);
  cache := InterfaceFactoryCache;
  if cache = nil then
    cache := InitializeInterfaceFactoryCache
  else
  begin
    cache.Safe.ReadLock; // multiple reads lock
    result := FactorySearch(pointer(cache.List), cache.Count, aInterface);
    cache.Safe.ReadUnLock;
    if result <> nil then
      exit; // retrieved from cache
  end;
  cache.Safe.WriteLock; // exclusive write lock
  try
    result := FactorySearch(pointer(cache.List), cache.Count, aInterface);
    if result <> nil then
      exit; // paranoid
    // not existing -> create new instance from RTTI
    {$ifdef HASINTERFACERTTI}
    result := TInterfaceFactoryRtti.Create(aInterface);
    cache.Add(result);
    {$else}
    result := nil; // make compiler happy
    raise EInterfaceFactory.CreateUtf8('No RTTI available for I%: please ' +
      'define the methods using a TInterfaceFactoryGenerated wrapper',
      [aInterface^.RawName]);
    {$endif HASINTERFACERTTI}
  finally
    cache.Safe.WriteUnLock;
  end;
end;

{$ifdef HASINTERFACERTTI}

class procedure TInterfaceFactory.RegisterInterfaces(
  const aInterfaces: array of PRttiInfo);
var
  i: PtrInt;
begin
  for i := 0 to high(aInterfaces) do
    Get(aInterfaces[i]);
end;

{$else}

class procedure TInterfaceFactory.RegisterInterfaces(
  const aInterfaces: array of PRttiInfo);
begin
  // no-op if no RTTI is available -> will be checked later when resolved
  // in fact, TInterfaceFactoryGenerated.RegisterInterface() should be done
end;

{$endif HASINTERFACERTTI}

function FindGuid(f: PInterfaceFactory; n: integer;
  {$ifdef CPU64} gL, gH : QWord {$else} guid: PHash128Rec {$endif}): TInterfaceFactory;
begin
  if n > 0 then
    repeat
      result := f^;
      with PHash128Rec(@result.fInterfaceIID)^ do
        {$ifdef CPU64}
        if (L = gL) and
           (H = gH) then
        {$else}
        if (c0 = guid^.c0) and
           (c1 = guid^.c1) and
           (c2 = guid^.c2) and
           (c3 = guid^.c3) then
        {$endif CPU64}
          exit;
      inc(f);
      dec(n);
    until n = 0;
  result := nil;
end;

{$ifdef FPC_HAS_CONSTREF}
class function TInterfaceFactory.Get(constref aGuid: TGUID): TInterfaceFactory;
{$else}
class function TInterfaceFactory.Get(const aGuid: TGUID): TInterfaceFactory;
{$endif FPC_HAS_CONSTREF}
var
  cache: TSynObjectListLightLocked;
begin
  cache := InterfaceFactoryCache;
  if cache <> nil then
  begin
    cache.Safe.ReadLock; // no GPF expected within loop -> no try...finally
    result := FindGuid(pointer(cache.List), cache.Count,
      {$ifdef CPU64} PHash128Rec(@aGuid)^.L, PHash128Rec(@aGuid)^.H
      {$else} @aGuid {$endif});
    cache.Safe.ReadUnLock;
  end
  else
    result := nil;
end;

class procedure TInterfaceFactory.AddToObjArray(var Obj: TInterfaceFactoryObjArray;
  const aGuids: array of TGUID);
var
  i: PtrInt;
  fac: TInterfaceFactory;
begin
  for i := 0 to high(aGuids) do
  begin
    fac := Get(aGuids[i]);
    if fac <> nil then
      ObjArrayAddOnce(Obj, fac);
  end;
end;

class function TInterfaceFactory.Guid2TypeInfo(
  const aGuids: array of TGUID): PRttiInfoDynArray;
var
  i: PtrInt;
begin
  result := nil;
  SetLength(result, length(aGuids));
  for i := 0 to high(aGuids) do
    result[i] := Guid2TypeInfo(aGuids[i]);
end;

class function TInterfaceFactory.Guid2TypeInfo(const aGuid: TGUID): PRttiInfo;
var
  fact: TInterfaceFactory;
begin
  fact := Get(aGuid);
  if fact = nil then
    raise EInterfaceFactory.CreateUtf8('%.Guid2TypeInfo(%): Interface not ' +
      'registered - use %.RegisterInterfaces()', [self, GuidToShort(aGuid), self]);
  result := fact.fInterfaceTypeInfo;
end;

class function TInterfaceFactory.Get(const aInterfaceName: RawUtf8): TInterfaceFactory;
var
  L, i: integer;
  f: ^TInterfaceFactory;
  cache: TSynObjectListLightLocked;
begin
  result := nil;
  L := length(aInterfaceName);
  cache := InterfaceFactoryCache;
  if (cache <> nil) and
     (L <> 0) then
  begin
    cache.Safe.ReadLock;
    try
      f := pointer(cache.List);
      for i := 1 to cache.Count do
        if IdemPropNameU(f^.fInterfaceName, pointer(aInterfaceName), L) then
        begin
          result := f^;
          exit; // retrieved from cache
        end
        else
          inc(f);
    finally
      cache.Safe.ReadUnLock;
    end;
  end;
end;

class function TInterfaceFactory.GetUsedInterfaces: TSynObjectListLightLocked;
begin
  result := InterfaceFactoryCache;
end;

class procedure TInterfaceFactory.RegisterUnsafeSpiType(const Types: array of PRttiInfo);
begin
  Rtti.RegisterUnsafeSpiType(Types);
end;

constructor TInterfaceFactory.Create(aInterface: PRttiInfo);
var
  m, a, reg: integer;
  WR: TJsonWriter;
  ErrorMsg: RawUtf8;
  {$ifdef HAS_FPREG}
  ValueIsInFPR: boolean;
  {$endif HAS_FPREG}
  {$ifdef CPUX86}
  offs: integer;
  {$else}
  {$ifdef OSPOSIX} // not used for Win64
  {$ifdef HAS_FPREG}
  fpreg: integer;
  {$endif HAS_FPREG}
  {$endif OSPOSIX}
  {$endif CPUX86}
begin
  // validate supplied TypeInfo() RTTI input
  if aInterface = nil then
    raise EInterfaceFactory.CreateUtf8('%.Create(nil)', [self]);
  if aInterface^.Kind <> rkInterface then
    raise EInterfaceFactory.CreateUtf8('%.Create: % is not an interface',
      [self, aInterface^.RawName]);
  fDocVariantOptions := JSON_FAST;
  fInterfaceTypeInfo := aInterface;
  fInterfaceIID := aInterface^.InterfaceGuid^;
  if IsNullGuid(fInterfaceIID) then
    raise EInterfaceFactory.CreateUtf8(
      '%.Create: % has no GUID', [self, aInterface^.RawName]);
  fInterfaceRtti := Rtti.RegisterType(aInterface) as TRttiJson;
  fInterfaceName := fInterfaceRtti.Name;
  fInterfaceUri := fInterfaceName;
  if fInterfaceUri[1] in ['i','I'] then
    // as in TServiceFactory.Create
    delete(fInterfaceUri, 1, 1);
  // retrieve all interface methods (recursively including ancestors)
  fMethod.InitSpecific(TypeInfo(TInterfaceMethodDynArray), fMethods, ptRawUtf8,
    @fMethodsCount, {caseinsens=}true);
  AddMethodsFromTypeInfo(aInterface); // from RTTI or generated code
  if fMethodsCount = 0 then
    raise EInterfaceFactory.CreateUtf8('%.Create(%): interface has ' +
      'no RTTI - it should inherit from IInvokable or add some methods',
      [self, fInterfaceName]);
  if MethodsCount > MAX_METHOD_COUNT then
    raise EInterfaceFactory.CreateUtf8(
      '%.Create(%): interface has too many methods (%), so breaks the ' +
      'Interface Segregation Principle and our internal buffers provision',
      [self, fInterfaceName, MethodsCount]);
  fMethodIndexCurrentFrameCallback := -1;
  fMethodIndexCallbackReleased := -1;
  SetLength(fMethods, MethodsCount);
  // compute additional information for each method
  for m := 0 to MethodsCount - 1 do
  with fMethods[m] do
  begin
    InterfaceDotMethodName := fInterfaceUri + '.' + URI;
    IsInherited := HierarchyLevel <> fAddMethodsLevel;
    ExecutionMethodIndex := m + RESERVED_VTABLE_SLOTS;
    ArgsInFirst := -1;
    ArgsInLast := -2;
    ArgsOutFirst := -1;
    ArgsOutLast := -2;
    ArgsNotResultLast := -2;
    ArgsOutNotResultLast := -2;
    ArgsResultIndex := -1;
    ArgsManagedFirst := -1;
    ArgsManagedLast := -2;
    Args[0].ValueType := imvSelf;
    for a := 1 to high(Args) do
    with Args[a] do
    begin
      ValueType := _FROM_RTTI[ArgRtti.Parser];
      ErrorMsg := ''; // seems supported
      case ValueType of
      imvNone:
        case ArgRtti.Info^.Kind of
          rkInteger:
            ErrorMsg := ' - use integer/cardinal instead';
          rkFloat:
            ErrorMsg := ' - use double/currency instead';
        else
          FormatUtf8(' (%)', [ToText(ArgRtti.Info^.Kind)], ErrorMsg);
        end;
      imvObject:
        if ArgRtti.ValueRtlClass = vcList then
          ErrorMsg := ' - use TObjectList or T*ObjArray instead'
        else if (ArgRtti.ValueRtlClass = vcCollection) and
                (ArgRtti.CollectionItem = nil) then
          ErrorMsg := ' - inherit from TInterfacedCollection or ' +
            'call Rtti.RegisterCollection() first'
        else if ValueDirection = imdResult then
          ErrorMsg := ' - class not allowed as function result: ' +
            'use a var/out parameter';
      imvInterface:
        if ValueDirection <> imdConst then
          ErrorMsg := ' - interface not allowed as output: ' +
            'use a const parameter';
      end;
      if ErrorMsg <> '' then
        raise EInterfaceFactory.CreateUtf8(
          '%.Create: %.% [%] parameter has unexpected type %%',
          [self, aInterface^.RawName, URI, ParamName^, ArgRtti.Name, ErrorMsg]);
      if ValueDirection = imdResult then
        ArgsResultIndex := a
      else
      begin
        ArgsNotResultLast := a;
        if ValueDirection <> imdOut then
        begin
          inc(ArgsInputValuesCount);
          if ArgsInFirst < 0 then
            ArgsInFirst := a;
          ArgsInLast := a;
        end;
        if ValueDirection <> imdConst then
          ArgsOutNotResultLast := a;
      end;
      if ValueDirection <> imdConst then
      begin
        if ArgsOutFirst < 0 then
          ArgsOutFirst := a;
        ArgsOutLast := a;
        inc(ArgsOutputValuesCount);
      end;
      if ValueType in [imvObject, imvDynArray, imvRecord,
           imvInterface, imvVariant] then
      begin
        if ArgsManagedFirst < 0 then
          ArgsManagedFirst := a;
        ArgsManagedLast := a;
      end;
      if rcfSpi in ArgRtti.Flags then
      begin
        // as defined by Rtti.RegisterUnsafeSpiType()
        include(ValueKindAsm, vIsSpi);
        include(HasSpiParams, ValueDirection);
      end;
    end;
    if ArgsOutputValuesCount = 0 then
      // plain procedure with no out param -> recognize some known signatures
      case ArgsInputValuesCount of
        1:
          if Args[1].ValueType = imvBoolean then
            if IdemPropNameU(URI, 'CurrentFrame') then
              fMethodIndexCurrentFrameCallback := m;
        2:
          if (Args[1].ValueType = imvInterface) and
             (Args[1].ArgRtti.Info = TypeInfo(IInvokable)) and
             (Args[2].ValueType = imvRawUtf8) and
             IdemPropNameU(URI, 'CallbackReleased') then
            fMethodIndexCallbackReleased := m;
      end;
    if ArgsResultIndex >= 0 then
      with Args[ArgsResultIndex] do
      case ValueType of
        imvNone,
        imvObject,
        imvInterface:
          raise EInterfaceFactory.CreateUtf8(
            '%.Create: I% unexpected result type %',
            [self, InterfaceDotMethodName, ArgTypeName^]);
        imvRecord:
          if ArgRtti.Info = TypeInfo(TServiceCustomAnswer) then
          begin
            for a := ArgsOutFirst to ArgsOutLast do
              if Args[a].ValueDirection in [imdVar, imdOut] then
                raise EInterfaceFactory.CreateUtf8('%.Create: I% var/out ' +
                  'parameter [%] not allowed with TServiceCustomAnswer result',
                  [self, InterfaceDotMethodName, Args[a].ParamName^]);
            ArgsResultIsServiceCustomAnswer := true;
          end
        {$ifdef CPUAARCH64}
        // FPC uses registers for managed records, but follows the ABI otherwise
        // which requires the result to be in X8 which is not handled yet
        // - see aarch64/cpupara.pas: tcpuparamanager.create_paraloc_info_intern
        else if not (rcfIsManaged in ArgRtti.Flags) then
          raise EInterfaceFactory.CreateUtf8(
            '%.Create: I% record result type % is unsupported on aarch64:' +
            'use an OUT parameter instead, or include a managed field',
            [self, InterfaceDotMethodName, ArgTypeName^]);
        {$endif CPUAARCH64}
      end;
    if (ArgsInputValuesCount = 1) and
       (Args[1].ValueType = imvRawByteString) then
      ArgsInputIsOctetStream := true;
  end;
  // compute asm low-level layout of the parameters for each method
  for m := 0 to MethodsCount - 1 do
  with fMethods[m] do
  begin
    // prepare stack and register layout
    reg := PARAMREG_FIRST;
    {$ifdef HAS_FPREG}
    {$ifdef OSPOSIX}
    fpreg := FPREG_FIRST;
    {$endif OSPOSIX}
    {$endif HAS_FPREG}
    for a := 0 to high(Args) do
    with Args[a] do
    begin
      RegisterIdent := 0;
      {$ifdef HAS_FPREG}
      FPRegisterIdent := 0;
      ValueIsInFPR := false;
      {$endif HAS_FPREG}
      ValueVar := ARGS_TO_VAR[ValueType];
      IndexVar := ArgsUsedCount[ValueVar];
      inc(ArgsUsedCount[ValueVar]);
      include(ArgsUsed, ValueType);
      if (ValueType in [imvRecord, imvVariant]) or
         (ValueDirection in [imdVar, imdOut]) or
         ((ValueDirection = imdResult) and
          (ValueType in ARGS_RESULT_BY_REF)) then
        Include(ValueKindAsm, vPassedByReference);
      if ValueType in _SMV_STRING then
        Include(ValueKindAsm, vIsString);
      case ValueType of
        imvInteger,
        imvCardinal,
        imvInt64:
          if rcfQWord in ArgRtti.Cache.Flags then
            Include(ValueKindAsm,vIsQword);
        imvDouble,
        imvDateTime:
          begin
            {$ifdef HAS_FPREG}
            ValueIsInFPR := not (vPassedByReference in ValueKindAsm);
            {$endif HAS_FPREG}
            if ValueType = imvDateTime then
            begin
              include(ValueKindAsm, vIsString);
              if ArgRtti.Parser = ptDateTimeMS then
                include(ValueKindAsm, vIsDateTimeMS);
            end;
          end;
        imvDynArray:
          if rcfObjArray in ArgRtti.Flags then
            Include(ValueKindAsm, vIsObjArray)
          else if (ArgRtti.ArrayRtti<>nil) and
                  (_FROM_RTTI[ArgRtti.ArrayRtti.Parser] in _SMV_STRING) then
            Include(ValueKindAsm, vIsDynArrayString);
      end;
      case ValueType of
        imvBoolean:
          SizeInStorage := 1;
        imvInteger,
        imvCardinal:
          SizeInStorage := 4;
        imvInt64,
        imvDouble,
        imvDateTime,
        imvCurrency:
          SizeInStorage := 8;
        imvEnum:
          SizeInStorage := ArgRtti.Cache.EnumInfo.SizeInStorageAsEnum;
        imvSet:
          begin
            SizeInStorage := ArgRtti.Cache.EnumInfo.SizeInStorageAsSet;
            if not (SizeInStorage in [1, 2, 4]) then
              raise EInterfaceFactory.CreateUtf8(
                '%.Create: unexpected SizeInStorage=% in %.% method % parameter' +
                ' for % set - we support only byte/integer/Int64 sizes',
                [self, SizeInStorage, fInterfaceName, URI, ParamName^, ArgTypeName^]);
          end;
        imvRecord:
          if ArgRtti.Size <= POINTERBYTES then
            raise EInterfaceFactory.CreateUtf8(
              '%.Create: % record too small in %.% method % parameter: it ' +
              'should be at least % bytes (i.e. a pointer) to be on stack',
              [self, ArgTypeName^, fInterfaceName, URI, ParamName^, POINTERBYTES])
          else
            SizeInStorage := POINTERBYTES; // handle only records when passed by ref
        imvBinary:
          ; // already set SizeInStorage
      else
        SizeInStorage := POINTERBYTES;
      end;
      if ValueDirection = imdResult then
      begin
        if not (ValueType in ARGS_RESULT_BY_REF) then
          continue; // ordinal/real/class results are returned in CPU/FPU registers
        {$ifndef CPUX86}
        InStackOffset := STACKOFFSET_NONE;
        RegisterIdent := PARAMREG_RESULT;
        continue;
        {$endif CPUX86}
        // CPUX86 will add an additional by-ref parameter
      end;
      {$ifdef CPU32}
      if ValueDirection = imdConst then
        if ValueType = imvBinary then
          SizeInStack := SizeInBinary
        else
          SizeInStack := ARGS_IN_STACK_SIZE[ValueType]
      else
      {$endif CPU32}
        SizeInStack := POINTERBYTES; // always aligned to 8 bytes boundaries for 64-bit
      if
        {$ifndef CPUARM}
        // on ARM, ordinals>POINTERBYTES can also be placed in the normal registers !!
        (SizeInStack <> POINTERBYTES) or
        {$endif CPUARM}
        {$ifdef HAS_FPREG}
        {$ifdef OSPOSIX}  // Linux x64, armhf, aarch64
        ((ValueIsInFPR) and (fpreg > FPREG_LAST)) or
        ((not ValueIsInFPR) and (reg > PARAMREG_LAST))
        {$else}
        (reg > PARAMREG_LAST) // Win64: XMMs overlap regular registers
        {$endif OSPOSIX}
        {$else}
        (reg > PARAMREG_LAST) // Win32, Linux x86, armel
        {$endif HAS_FPREG}
        {$ifdef FPC}
        or ((ValueType in [imvRecord]) and
          // trunk i386/x86_64\cpupara.pas: DynArray const is passed as register
          not (vPassedByReference in ValueKindAsm))
        {$endif FPC} then
      begin
        // this parameter will go on the stack
        {$ifdef CPUARM}
        // parameter must be aligned on a SizeInStack boundary
        if SizeInStack > POINTERBYTES then
          Inc(ArgsSizeInStack, ArgsSizeInStack mod cardinal(SizeInStack));
        {$endif CPUARM}
        InStackOffset := ArgsSizeInStack;
        inc(ArgsSizeInStack, SizeInStack);
      end
      else
      begin
        // this parameter will go in a register
        InStackOffset := STACKOFFSET_NONE;
        {$ifndef CPUX86}
        if (ArgsResultIndex >= 0) and
           (reg = PARAMREG_RESULT) and
           (Args[ArgsResultIndex].ValueType in ARGS_RESULT_BY_REF) then
          inc(reg); // this register is reserved for method result pointer
        {$endif CPUX86}
        {$ifdef HAS_FPREG}
        if ValueIsInFPR then
        begin
          // put in a floating-point register
          {$ifdef OSPOSIX}
          FPRegisterIdent := fpreg;
          inc(fpreg);
          {$else}
          FPRegisterIdent := reg; // Win64 ABI: reg and fpreg do overlap
          inc(reg);
          {$endif OSPOSIX}
        end
        else
        {$endif HAS_FPREG}
        begin
          // put in an integer register
          {$ifdef CPUARM}
          // on 32-bit ARM, ordinals>POINTERBYTES are also placed in registers
          if (SizeInStack>POINTERBYTES) and
             ((reg and 1) = 0) then
            inc(reg); // must be aligned on even boundary
          // check if we have still enough registers, after previous increments
          if ((PARAMREG_LAST - reg + 1) * POINTERBYTES) < SizeInStack then
          begin
            // no space, put on stack
            InStackOffset := ArgsSizeInStack;
            inc(ArgsSizeInStack, SizeInStack);
            // all params following the current one, must also be placed on stack
            reg := PARAMREG_LAST + 1;
            continue;
          end;
          RegisterIdent := reg;
          if SizeInStack > POINTERBYTES then
            inc(reg, SizeInStack shr POINTERSHR)
          else
            inc(reg);
          {$else}
          RegisterIdent := reg;
          inc(reg);
          {$endif CPUARM}
        end;
      end;
    end;
    if ArgsSizeInStack > MAX_EXECSTACK then
      raise EInterfaceFactory.CreateUtf8(
        '%.Create: Stack size % > % for %.% method parameters',
        [self, ArgsSizeInStack, MAX_EXECSTACK, fInterfaceName, URI]);
    {$ifdef CPUX86}
    // pascal/register convention are passed left-to-right -> reverse order
    offs := ArgsSizeInStack;
    for a := 0 to high(Args) do
      with Args[a] do
      if InStackOffset >= 0 then
      begin
        dec(offs,SizeInStack);
        InStackOffset := offs;
      end;
    //assert(offs=0);
    {$endif CPUX86}
  end;
  WR := TJsonWriter.CreateOwnedStream;
  try
    // compute the default results JSON array for all methods
    for m := 0 to MethodsCount - 1 do
    with fMethods[m] do
    begin
      WR.CancelAll;
      WR.Add('[');
      for a := ArgsOutFirst to ArgsOutLast do
        with Args[a] do
        if ValueDirection <> imdConst then
          AddDefaultJson(WR);
      WR.CancelLastComma;
      WR.Add(']');
      WR.SetText(DefaultResult);
    end;
    // compute the service contract as a JSON array
    WR.CancelAll;
    WR.Add('[');
    for m := 0 to MethodsCount - 1 do
    with fMethods[m] do
    begin
      WR.Add('{"method":"%","arguments":[',[URI]);
      for a := 0 to High(Args) do
        Args[a].SerializeToContract(WR);
      WR.CancelLastComma;
      WR.AddShorter(']},');
    end;
    WR.CancelLastComma;
    WR.Add(']');
    WR.SetText(fContract);
    {$ifdef SOA_DEBUG}
    JsonReformatToFile(fContract,TFileName(fInterfaceName + '-' +
      COMP_TEXT + OS_TEXT + CPU_ARCH_TEXT + '.json'));
    {$endif SOA_DEBUG}
  finally
    WR.Free;
  end;
end;

function TInterfaceFactory.FindMethodIndex(const aMethodName: RawUtf8): integer;
begin
  if (self = nil) or
     (aMethodName = '') then
    result := -1
  else
  begin
    if MethodsCount < 10 then
    begin
      for result := 0 to MethodsCount - 1 do
        if IdemPropNameU(fMethods[result].Uri, aMethodName) then
          exit;
      result := -1;
    end
    else
      result := fMethod.FindHashed(aMethodName);
    if (result < 0) and
       (aMethodName[1] <> '_') then
      result := FindMethodIndex('_' + aMethodName);
  end;
end;

function TInterfaceFactory.FindMethod(const aMethodName: RawUtf8): PInterfaceMethod;
var
  i: PtrInt;
begin
  i := FindMethodIndex(aMethodName);
  if i < 0 then
    result := nil
  else
    result := @fMethods[i];
end;

function TInterfaceFactory.FindFullMethodIndex(const aFullMethodName: RawUtf8;
  alsoSearchExactMethodName: boolean): integer;
begin
  if PosExChar('.', aFullMethodName) <> 0 then
    for result := 0 to MethodsCount - 1 do
      if IdemPropNameU(fMethods[result].InterfaceDotMethodName, aFullMethodName) then
        exit;
  if alsoSearchExactMethodName then
    result := FindMethodIndex(aFullMethodName)
  else
    result := -1;
end;

function TInterfaceFactory.CheckMethodIndex(const aMethodName: RawUtf8): integer;
begin
  if self = nil then
    raise EInterfaceFactory.Create('TInterfaceFactory(nil).CheckMethodIndex');
  result := FindMethodIndex(aMethodName);
  if result < 0 then
    raise EInterfaceFactory.CreateUtf8('%.CheckMethodIndex: %.% not found',
      [self, fInterfaceName, aMethodName]);
end;

function TInterfaceFactory.CheckMethodIndex(aMethodName: PUtf8Char): integer;
begin
  result := CheckMethodIndex(RawUtf8(aMethodName));
end;

procedure TInterfaceFactory.CheckMethodIndexes(
  const aMethodName: array of RawUtf8; aSetAllIfNone: boolean;
  out aBits: TInterfaceFactoryMethodBits);
var
  i: PtrInt;
begin
  if aSetAllIfNone and
     (high(aMethodName) < 0) then
  begin
    FillCharFast(aBits, SizeOf(aBits), 255);
    exit;
  end;
  FillCharFast(aBits, SizeOf(aBits), 0);
  for i := 0 to high(aMethodName) do
    include(aBits, CheckMethodIndex(aMethodName[i]));
end;

function TInterfaceFactory.GetMethodName(MethodIndex: integer): RawUtf8;
begin
  if (MethodIndex < 0) or
     (self = nil) then
    result := ''
  else if MethodIndex < Length(SERVICE_PSEUDO_METHOD) then
    result := SERVICE_PSEUDO_METHOD[TServiceInternalMethod(MethodIndex)]
  else
  begin
    dec(MethodIndex, Length(SERVICE_PSEUDO_METHOD));
    if cardinal(MethodIndex) < MethodsCount then
      result := fMethods[MethodIndex].Uri
    else
      result := '';
  end;
end;

function TInterfaceFactory.GetFullMethodName(aMethodIndex: integer): RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    result := GetMethodName(aMethodIndex);
    if result = '' then
      result := fInterfaceName
    else
      result := fInterfaceName + '.' + result;
  end;
end;

{ low-level ASM for TInterfaceFactory.GetMethodsVirtualTable
  - initial ARM, AARCH64 and Linux64 code below was provided by ALF! Thanks! :}

{$ifdef FPC}

{$ifdef CPUARM}
{$ifdef ASMORIG}
procedure TInterfacedObjectFakeRaw.ArmFakeStub;
var
  // warning: exact local variables order should match TFakeCallStack
  smetndx: pointer;
  {$ifdef HAS_FPREG}
  sd7, sd6, sd5, sd4, sd3, sd2, sd1, sd0: double;
  {$endif HAS_FPREG}
  sr3,sr2,sr1,sr0: pointer;
asm
    // get method index
    str  v1,smetndx
    // store registers
    {$ifdef HAS_FPREG}
    vstr d0,sd0
    vstr d1,sd1
    vstr d2,sd2
    vstr d3,sd3
    vstr d4,sd4
    vstr d5,sd5
    vstr d6,sd6
    vstr d7,sd7
    {$endif HAS_FPREG}
    str  r0,sr0
    str  r1,sr1
    str  r2,sr2
    str  r3,sr3
    // TFakeCallStack address as 2nd parameter
    add  r1,sp, #12
    // branch to the FakeCall function
    bl   FakeCall
    // FakeCall should set Int64 result in method result,
    // and float in aCall.FPRegs["sd0"]
    {$ifdef HAS_FPREG}
    vstr d0,sd0
    {$endif HAS_FPREG}
end;
{$else}
procedure TInterfacedObjectFakeRaw.ArmFakeStub; nostackframe;assembler;
asm
      // get method index
      str   r12,[r13, #-52]
      // create stack space
      mov   r12,r13
      stmfd r13!,{r11,r12,r14,r15}
      sub   r11,r12,#4
      {$ifdef HAS_FPREG}
      sub   r13,r13,#128
      // store registers
      vstr  d0,[r11, #-112]
      vstr  d1,[r11, #-104]
      vstr  d2,[r11, #-96]
      vstr  d3,[r11, #-88]
      vstr  d4,[r11, #-80]
      vstr  d5,[r11, #-72]
      vstr  d6,[r11, #-64]
      vstr  d7,[r11, #-56]
      str   r0,[r11, #-128]
      str   r1,[r11, #-124]
      str   r2,[r11, #-120]
      str   r3,[r11, #-116]
      {$else}
      sub   r13,r13,#64
      // store registers
      str   r0,[r11, #-64]
      str   r1,[r11, #-60]
      str   r2,[r11, #-56]
      str   r3,[r11, #-52]
      {$endif HAS_FPREG}
      // set stack address
      add   r1,r13, #12
      // branch to the FakeCall function
      bl    FakeCall
      // store result
      {$ifdef HAS_FPREG}
      vstr  d0,[r11, #-112]
      {$endif HAS_FPREG}
      ldmea r11,{r11,r13,r15}
end;
{$endif ASMORIG}
{$endif CPUARM}
{$ifdef CPUAARCH64}
procedure TInterfacedObjectFakeRaw.AArch64FakeStub;
var
  // warning: exact local variables order should match TFakeCallStack
  sx0, sx1, sx2, sx3, sx4, sx5, sx6, sx7: pointer;
  sd0, sd1, sd2, sd3, sd4, sd5, sd6, sd7: double;
  smetndx: pointer;
asm
    // get method index from IP0 [x16/r16]
    str x16,smetndx
    // store registers
    str d0,sd0
    str d1,sd1
    str d2,sd2
    str d3,sd3
    str d4,sd4
    str d5,sd5
    str d6,sd6
    str d7,sd7
    str x0,sx0
    str x1,sx1
    str x2,sx2
    str x3,sx3
    str x4,sx4
    str x5,sx5
    str x6,sx6
    str x7,sx7
    // TFakeCallStack address as 2nd parameter
    // sx0 is at the stack pointer !
    // local variables are stored in reverse on the stack
    add x1, sp, #0
    // branch to the FakeCall function
    bl  FakeCall
    // FakeCall should set Int64 result in method result,
    // and float in aCall.FPRegs["sd0"]
    str d0,sd0
end;
{$endif CPUAARCH64}

{$endif FPC}

{$ifdef CPUX64}

{$ifdef FPC}
  {$WARN 7102 off : Use of +offset(%ebp) for parameters invalid here}
  {$WARN 7105 off : Use of -offset(%esp), access may cause a crash or value may be lost}
  {$WARN 7121 off : Check size of memory operand}
{$endif FPC}

procedure x64FakeStub;
var // warning: exact local variables order should match TFakeCallStack
  smetndx,
  {$ifdef OSPOSIX}
  sxmm7, sxmm6, sxmm5, sxmm4,
  {$endif OSPOSIX}
  sxmm3, sxmm2, sxmm1, sxmm0: double;
  {$ifdef OSPOSIX}
  sr9, sr8, srcx, srdx, srsi, srdi: pointer;
  {$endif OSPOSIX}
asm     // caller = mov eax,{MethodIndex}; jmp x64FakeStub
        {$ifndef FPC}
        // FakeCall(self: TInterfacedObjectFake; var aCall: TFakeCallStack): Int64
        // So, make space for two variables (+shadow space)
        // adds $50 to stack, so rcx .. at rpb+$10+$50 = rpb+$60
       .params 2
        {$endif FPC}
        mov     smetndx, rax
        movlpd  sxmm0, xmm0 // movlpd to ignore upper 64-bit of 128-bit xmm reg
        movlpd  sxmm1, xmm1
        movlpd  sxmm2, xmm2
        movlpd  sxmm3, xmm3
        {$ifdef OSPOSIX}
        movlpd  sxmm4, xmm4
        movlpd  sxmm5, xmm5
        movlpd  sxmm6, xmm6
        movlpd  sxmm7, xmm7
        mov     sr9, r9
        mov     sr8, r8
        mov     srcx, rcx
        mov     srdx, rdx
        mov     srsi, rsi
        mov     srdi, rdi
        lea     rsi, srdi // TFakeCallStack address as 2nd parameter
        {$else}
        {$ifndef FPC}
        mov     [rbp + $60], rcx
        mov     [rbp + $68], rdx
        mov     [rbp + $70], r8
        mov     [rbp + $78], r9
        {$else}
        mov     qword ptr [rbp + $10], rcx
        mov     qword ptr [rbp + $18], rdx
        mov     qword ptr [rbp + $20], r8
        mov     qword ptr [rbp + $28], r9
        {$endif FPC}
        lea     rdx, sxmm0 // TFakeCallStack address as 2nd parameter
        {$endif OSPOSIX}
        call    TInterfacedObjectFake.FakeCall
        // FakeCall should set Int64 result in method result,
        // and float in aCall.FPRegs["XMM0"]
        movsd   xmm0, qword ptr sxmm0 // movsd for zero extension
end;

{$endif CPUX64}

var
  VmtSafe: TLightLock;

{$ifdef CPUX86}  // i386 stub requires "ret ArgsSizeInStack"

function TInterfaceFactory.GetMethodsVirtualTable: pointer;
var
  P: PCardinal;
  i: PtrInt;
begin
  result := fFakeVTable;
  if result <> nil then
    exit;
  // it is the first time we use this interface -> create JITed VMT
  VmtSafe.Lock;
  try
    // we need to JIT with an explicit ArgsSizeInStack adjustement
    if fFakeVTable = nil then // avoid race condition
    begin
      SetLength(fFakeVTable, MethodsCount + RESERVED_VTABLE_SLOTS);
      // set IInterface required methods
      fFakeVTable[0] := @TInterfacedObjectFake.FakeQueryInterface;
      fFakeVTable[1] := @TInterfacedObjectFake.Fake_AddRef;
      fFakeVTable[2] := @TInterfacedObjectFake.Fake_Release;
      // set JITted VMT stubs for each method of this interface
      if MethodsCount <> 0 then
      begin
        P := ReserveExecutableMemory(MethodsCount * VMTSTUBSIZE);
        for i := 0 to MethodsCount - 1 do
        begin
          fFakeVTable[RESERVED_VTABLE_SLOTS + i] := P;
          P^ := $68ec8b55;
          inc(P);                 // push ebp; mov ebp, esp
          P^ := i;
          inc(P);                 // push {MethodIndex}
          P^ := $e2895251;
          inc(P);                 // push ecx; push edx; mov edx, esp
          PByte(P)^ := $e8;
          inc(PByte(P));          // call FakeCall
          P^ := PtrUInt(@TInterfacedObjectFake.FakeCall) - PtrUInt(P) - 4;
          inc(P);
          P^ := $c25dec89;        // mov esp, ebp; pop ebp; ret {StackSize}
          inc(PByte(P), 3);       // overlap c2=ret to avoid GPF
          P^ := (fMethods[i].ArgsSizeInStack shl 8) or $900000c2;
          inc(P);
        end;
        ReserveExecutableMemoryPageAccess(
          fFakeVTable[RESERVED_VTABLE_SLOTS], {exec=}true);
      end;
    end;
    result := pointer(fFakeVTable);
  finally
    VmtSafe.UnLock;
  end;
end;

{$else}

var
  // reuse the very same JITted stubs for all interfaces
  _FAKEVMT: array of pointer;

// JIT MAX_METHOD_COUNT VMT stubs for every method of any interface
// - internal function protected by VmtSafe.Lock
procedure Compute_FAKEVMT;
var
  P: PCardinal;
  i: PtrInt;
  {$ifdef CPUARM3264}
  stub {$ifdef CPUAARCH64} , tmp {$endif}: PtrUInt;
  {$endif CPUARM3264}
begin
  // reserve executable memory for JIT (aligned to 8 bytes)
  P := ReserveExecutableMemory(MAX_METHOD_COUNT * VMTSTUBSIZE
    {$ifdef CPUAARCH64} + ($120 shr 2) {$endif CPUAARCH64});
  // populate _FAKEVMT[] with JITted stubs
  SetLength(_FAKEVMT, MAX_METHOD_COUNT + RESERVED_VTABLE_SLOTS);
  // set IInterface RESERVED_VTABLE_SLOTS required methods
  _FAKEVMT[0] := @TInterfacedObjectFake.FakeQueryInterface;
  _FAKEVMT[1] := @TInterfacedObjectFake.Fake_AddRef;
  _FAKEVMT[2] := @TInterfacedObjectFake.Fake_Release;
  // JIT all potential custom method stubs
  for i := 0 to MAX_METHOD_COUNT - 1 do
  begin
    _FAKEVMT[i + RESERVED_VTABLE_SLOTS] := P;
    {$ifdef CPUX64}    // note: on Posix, (stub-P) > 32-bit -> need absolute jmp
    P^ := $ba49;       // mov r10, x64FakeStub
    inc(PWord(P));
    PPointer(P)^ := @x64FakeStub;
    inc(PPointer(P));
    PByte(P)^ := $b8;  // mov eax, MethodIndex
    inc(PByte(P));
    P^ := i;
    inc(P);
    P^ := $66e2ff41;   // jmp r10  (faster than push + ret)
    inc(P);
    P^ := $00441f0f;   // multi-byte nop
    inc(PByte(P), 5);
    {$endif CPUX64}
    {$ifdef CPUARM}
    {$ifdef ASMORIG}
    P^ := ($e3a040 shl 8) + i;
    inc(P); // mov r4 (v1),{MethodIndex} : store method index in register
    {$else}
    P^ := ($e3a0c0 shl 8) + cardinal(i);
    inc(P); // mov r12 (ip),{MethodIndex} : store method index in register
    {$endif ASMORIG}
    stub := ((PtrUInt(@TInterfacedObjectFake.ArmFakeStub) -
             PtrUInt(P)) shr 2) - 2;
    // branch ArmFakeStub (24bit relative, word aligned)
    P^ := ($ea shl 24) + (stub and $00ffffff);
    inc(P);
    P^ := $e320f000;
    inc(P);
    {$endif CPUARM}
    {$ifdef CPUAARCH64}
    // store method index in register r16 [IP0]
    // $10 = r16 ... loop to $1F -> number shifted * $20
    P^ := ($d280 shl 16) + (i shl 5) + $10;
    inc(P);  // mov r16 ,{MethodIndex}
    // we are using a register branch here
    // fill register x10 with address
    stub := PtrUInt(@TInterfacedObjectFake.AArch64FakeStub);
    tmp := (stub shr 0) and $ffff;
    P^ := ($d280 shl 16) + (tmp shl 5) + $0a;
    inc(P);
    tmp := (stub shr 16) and $ffff;
    P^ := ($f2a0 shl 16) + (tmp shl 5) + $0a;
    inc(P);
    tmp := (stub shr 32) and $ffff;
    P^ := ($f2c0 shl 16) + (tmp shl 5) + $0a;
    inc(P);
    tmp := (stub shr 48) and $ffff;
    P^ := ($f2e0 shl 16) + (tmp shl 5) + $0a;
    inc(P);
    // branch to address in x10 register
    P^ := $d61f0140;
    inc(P);
    P^ := $d503201f;
    inc(P);
    {$endif CPUAARCH64}
  end;
  // reenable execution permission of JITed memory as expected by the VMT
  ReserveExecutableMemoryPageAccess(
    _FAKEVMT[RESERVED_VTABLE_SLOTS], {exec=}true);
end;

function TInterfaceFactory.GetMethodsVirtualTable: pointer;
begin
  result := pointer(_FAKEVMT);
  if result <> nil then
    exit;
  // it is the first time we use this interface -> create JITed VMT
  VmtSafe.Lock;
  try
    if _FAKEVMT = nil then
      Compute_FAKEVMT;
  finally
    VmtSafe.UnLock;
  end;
  result := pointer(_FAKEVMT);  // we can reuse pre-JITted stubs
end;

{$endif CPUX86}


{$ifdef HASINTERFACERTTI}

{ TInterfaceFactoryRtti }

procedure TInterfaceFactoryRtti.AddMethodsFromTypeInfo(aInterface: PRttiInfo);
var
  info: TRttiInterface;
  nm, na: integer;
  m: PRttiMethod;
  sm: PInterfaceMethod;
  a: PRttiMethodArg;
  sa: PInterfaceMethodArgument;
begin
  nm := GetRttiInterface(aInterface, info); // call mormot.core.rtti logic
  fMethod.Capacity := nm;
  m := pointer(info.Methods);
  while nm > 0 do
  begin
    sm := fMethod.AddUniqueName(m^.Name, '%.% method: duplicated name for %',
      [info.Name, m^.Name, self]);
    sm^.HierarchyLevel := m^.HierarchyLevel;
    na := length(m^.Args);
    SetLength(sm^.Args, na);
    sa := pointer(sm^.Args);
    a := pointer(m^.Args);
    while na > 0 do
    begin
      sa^.ParamName := a^.ParamName;
      sa^.ArgTypeName := a^.TypeName;
      sa^.ArgRtti := Rtti.RegisterType(a^.TypeInfo) as TRttiJson;
      sa^.ValueDirection := TInterfaceMethodValueDirection(a^.Direction);
      inc(sa);
      inc(a);
      dec(na);
    end;
    inc(m);
    dec(nm);
  end;
end;

{$endif HASINTERFACERTTI}


{ TInterfaceFactoryGenerated }

procedure TInterfaceFactoryGenerated.AddMethod(const aName: RawUtf8;
  const aParams: array of const);
const
  ARGPERARG = 3; // aParams = [ 0,'n1',TypeInfo(integer), ... ]
var
  meth: PInterfaceMethod;
  arg: ^TInterfaceMethodArgument;
  na, ns, a: PtrInt;
  u: RawUtf8;
begin
  if Length(aParams) mod ARGPERARG <> 0 then
    raise EInterfaceFactory.CreateUtf8(
      '%: invalid aParams count for %.AddMethod("%")', [fInterfaceName, self, aName]);
  meth := fMethod.AddUniqueName(aName, '%.% method: duplicated generated name for %',
    [fInterfaceName, aName, self]);
  na := length(aParams) div ARGPERARG;
  SetLength(meth^.Args, na + 1); // always include Args[0]=self
  with meth^.Args[0] do
  begin
    ParamName := @PSEUDO_SELF_NAME;
    ArgRtti := fInterfaceRtti;
    ArgTypeName := fInterfaceTypeInfo^.Name;
  end;
  ns := length(fTempStrings);
  SetLength(fTempStrings, ns + na);
  for a := 0 to na - 1 do
  begin
    arg := @meth^.Args[a + 1];
    if aParams[a * ARGPERARG].VType <> vtInteger then
      raise EInterfaceFactory.CreateUtf8(
        '%: invalid param type #% for %.AddMethod("%")',
        [fInterfaceName, a, self, aName]);
    arg^.ValueDirection :=
      TInterfaceMethodValueDirection(aParams[a * ARGPERARG].VInteger);
    VarRecToUtf8(aParams[a * ARGPERARG + 1], u);
    if u = '' then
      raise EInterfaceFactory.CreateUtf8(
        '%: invalid param name #% for %.AddMethod("%")',
        [fInterfaceName, a, self, aName]);
    insert(AnsiChar(Length(u)), u, 1); // create fake PShortString
    arg^.ParamName := pointer(u);
    fTempStrings[ns + a] := u;
    if aParams[a * ARGPERARG + 2].VType <> vtPointer then
      raise EInterfaceFactory.CreateUtf8(
        '%: expect TypeInfo() at #% for %.AddMethod("%")',
        [fInterfaceName, a, self, aName]);
    arg^.ArgRtti := Rtti.RegisterType(aParams[a * ARGPERARG + 2].VPointer) as TRttiJson;
    arg^.ArgTypeName := arg^.ArgRtti.Info^.Name;
  end;
end;

class procedure TInterfaceFactoryGenerated.RegisterInterface(aInterface: PRttiInfo);
var
  cache: TSynObjectListLightLocked;
begin
  if (aInterface = nil) or
     (self = TInterfaceFactoryGenerated) then
    raise EInterfaceFactory.CreateUtf8('%.RegisterInterface(nil)', [self]);
  cache := InterfaceFactoryCache;
  if cache = nil then
    cache := InitializeInterfaceFactoryCache;
  cache.Safe.WriteLock;
  try
    if FactorySearch(pointer(cache.List), cache.Count, aInterface) <> nil then
      raise EInterfaceFactory.CreateUtf8('Duplicated %.RegisterInterface(%)',
        [self, aInterface^.RawName]);
    cache.Add(Create(aInterface));
  finally
    cache.Safe.WriteUnLock;
  end;
end;


function ToText({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  aGuid: TGUID): ShortString;
var
  fact: TInterfaceFactory;
begin
  fact := TInterfaceFactory.Get(aGuid);
  if fact = nil then
    GuidToShort(aGuid, PGuidShortString(@result)^)
  else
    result := fact.fInterfaceTypeInfo^.RawName;
end;

{$ifdef HASINTERFACEASTOBJECT}
function ObjectFromInterface(const aValue: IInterface): TObject;
begin
  if aValue <> nil then
    // calling the RTL is the standard way, and fast enough on FPC
    result := aValue as TObject
  else
    result := nil;
end;
{$else}
function ObjectFromInterface(const aValue: IInterface): TObject;
  type
    // allow in-place decompilation of Delphi interface VMT redirection asm
    TObjectFromInterfaceStub = packed record
      Stub: cardinal;
      case integer of
        0:
          (ShortJmp: shortint);
        1:
          (LongJmp:  integer)
    end;
    PObjectFromInterfaceStub = ^TObjectFromInterfaceStub;
begin
  if aValue <> nil then
    with PObjectFromInterfaceStub(PPointer(PPointer(aValue)^)^)^ do
    case Stub of
      // check first asm opcodes of VMT[0] entry, i.e. QueryInterface()
      $04244483:
        begin
          result := pointer(PtrInt(aValue) + ShortJmp);
          exit;
        end;
      $04244481:
        begin
          result := pointer(PtrInt(aValue) + LongJmp);
          exit;
        end;
      else if Stub = PCardinal(@TInterfacedObjectFake.FakeQueryInterface)^ then
      begin
        // recognized TInterfaceFactory.CreateFakeInstance() stub/mock
        result := TInterfacedObjectFake(pointer(aValue)).SelfFromInterface;
        exit;
      end
      else
      begin
        result := nil;
        exit;
      end;
    end
  else
    result := nil;
end;
{$endif HASINTERFACEASTOBJECT}

function ObjectFromInterfaceImplements(const aValue: IInterface;
  const aInterface: TGUID): boolean;
var
  obj: TObject;
begin
  obj := ObjectFromInterface(aValue);
  if obj = nil then
    result := false
  else
    result := obj.GetInterfaceEntry(aInterface) <> nil;
end;



{ ************ TInterfaceResolver TInjectableObject for Dependency Injection  }

{ TInterfaceResolver }

function TInterfaceResolver.Implements(aInterface: PRttiInfo): boolean;
var
  dummy: IInterface;
begin
  result := TryResolve(aInterface, dummy);
end;

function TInterfaceResolver.Resolve(aInterface: PRttiInfo; out Obj): boolean;
begin
  if self = nil then
    result := false
  else
    result := TryResolve(aInterface, Obj);
end;

function TInterfaceResolver.Resolve(const aGuid: TGUID; out Obj;
  aRaiseIfNotFound: EInterfaceResolver): boolean;
var
  known: TInterfaceFactory;
begin
  if self = nil then
    result := false
  else
  begin
    known := TInterfaceFactory.Get(aGuid);
    if known <> nil then
      result := TryResolve(known.fInterfaceTypeInfo, Obj)
    else
      result := false;
  end;
  if (aRaiseIfNotFound <> nil) and
     not result then
    raise aRaiseIfNotFound.CreateUtf8('%.Resolve(%) unsatisfied',
      [self, GuidToShort(aGUID)]);
end;

procedure TInterfaceResolver.ResolveByPair(
  const aInterfaceObjPairs: array of pointer; aRaiseExceptionIfNotFound: boolean);
var
  n, i: PtrInt;
begin
  n := length(aInterfaceObjPairs);
  if (self = nil) or
     (n = 0) or
     (n and 1 = 1) then
    raise EInterfaceResolver.CreateUtf8('%.Resolve([odd])', [self]);
  for i := 0 to (n shr 1) - 1 do
    if not TryResolve(aInterfaceObjPairs[i * 2], aInterfaceObjPairs[i * 2 + 1]^) then
      if aRaiseExceptionIfNotFound then
        raise EInterfaceResolver.CreateUtf8('%.ResolveByPair(%) unsatisfied',
          [self, PRttiInfo(aInterfaceObjPairs[i * 2])^.RawName]);
end;

procedure TInterfaceResolver.Resolve(const aInterfaces: array of TGUID;
  const aObjs: array of pointer; aRaiseExceptionIfNotFound: boolean);
var
  n, i: PtrInt;
  info: PRttiInfo;
begin
  n := length(aInterfaces);
  if (self = nil) or
     (n = 0) or
     (n <> length(aObjs)) then
    raise EInterfaceResolver.CreateUtf8('%.Resolve([?,?])', [self]);
  for i := 0 to n - 1 do
    if PPointer(aObjs[i])^ = nil then
    begin
      info := TInterfaceFactory.Guid2TypeInfo(aInterfaces[i]);
      if not TryResolve(info, aObjs[i]^) then
        if aRaiseExceptionIfNotFound then
          raise EInterfaceResolver.CreateUtf8('%.Resolve(%) unsatisfied',
            [self, info^.RawName]);
    end;
end;

{ TInterfaceResolverForSingleInterface }

constructor TInterfaceResolverForSingleInterface.Create(aInterface: PRttiInfo;
  aImplementation: TInterfacedObjectClass);
var
  guid: PGUID;
begin
  fInterfaceTypeInfo := aInterface;
  guid := aInterface^.InterfaceGuid;
  if guid = nil then
    raise EInterfaceResolver.CreateUtf8('%.Create expects an Interface', [self]);
  fImplementationEntry := aImplementation.GetInterfaceEntry(guid^);
  if fImplementationEntry = nil then
    raise EInterfaceResolver.CreateUtf8('%.Create: % does not implement %',
      [self, aImplementation, fInterfaceTypeInfo^.RawName]);
  aInterface^.InterfaceAncestors(fInterfaceAncestors, aImplementation,
    fInterfaceAncestorsImplementationEntry);
  fImplementation := Rtti.RegisterClass(aImplementation);
end;

constructor TInterfaceResolverForSingleInterface.Create(const aInterface: TGUID;
  aImplementation: TInterfacedObjectClass);
begin
  Create(TInterfaceFactory.Guid2TypeInfo(aInterface), aImplementation);
end;

function TInterfaceResolverForSingleInterface.CreateInstance: TInterfacedObject;
begin
  result := TInterfacedObject(fImplementation.ClassNewInstance);
end;

function TInterfaceResolverForSingleInterface.GetImplementationName: RawUtf8;
begin
  if (self = nil) or
     (fImplementation.ValueClass = nil) then
    result := ''
  else
    result := fImplementation.Name;
end;

function TInterfaceResolverForSingleInterface.GetOneInstance(out Obj): boolean;
begin
  if (self = nil) or
     (fImplementation.ValueClass = nil) then
    result := false
  else
    // here we know that CreateInstance will implement the interface
    result := GetInterfaceFromEntry(CreateInstance, fImplementationEntry, Obj);
end;

function TInterfaceResolverForSingleInterface.TryResolve(aInterface: PRttiInfo;
  out Obj): boolean;
var
  i: PtrInt;
begin
  if fImplementation.ValueClass = nil then
    result := false
  else if fInterfaceTypeInfo = aInterface then
    result := GetInterfaceFromEntry(CreateInstance, fImplementationEntry, Obj)
  else
  begin
    // if not found exact interface, try any parent/ancestor interface
    for i := 0 to length(fInterfaceAncestors) - 1 do
      if fInterfaceAncestors[i] = aInterface then
      begin
        // here we know that CreateInstance will implement fInterfaceAncestors[]
        result := GetInterfaceFromEntry(CreateInstance,
          fInterfaceAncestorsImplementationEntry[i], Obj);
        exit;
      end;
    result := false;
  end;
end;

function TInterfaceResolverForSingleInterface.Implements(aInterface: PRttiInfo): boolean;
var
  i: PtrInt;
begin
  result := true;
  if fInterfaceTypeInfo = aInterface then
    // found exact interface
    exit;
  for i := 0 to length(fInterfaceAncestors) - 1 do
    if fInterfaceAncestors[i] = aInterface then
      // found any parent/ancestor interface
      exit;
  result := false;
end;



{ TInterfaceResolverList }

function TInterfaceResolverList.PrepareAddAndWriteLock(aInterface: PRttiInfo;
  aImplementationClass: TClass): PInterfaceEntry;
var
  i: PtrInt;
begin
  if (aInterface = nil) or
     (aImplementationClass = nil) then
    raise EInterfaceResolver.CreateUtf8('%.Add(nil)', [self]);
  if aInterface^.Kind <> rkInterface then
    raise EInterfaceResolver.CreateUtf8('%.Add(%): % is not an interface',
      [self, aInterface^.RawName]);
  result := aImplementationClass.GetInterfaceEntry(aInterface^.InterfaceGuid^);
  if result = nil then
    raise EInterfaceResolver.CreateUtf8('%.Add(): % does not implement %',
      [self, aImplementationClass, aInterface^.RawName]);
  fSafe.WriteLock;
  for i := 0 to length(fEntry) - 1 do
    if fEntry[i].TypeInfo = aInterface then
    begin
      fSafe.WriteUnLock;
      raise EInterfaceResolver.CreateUtf8('%.Add(%): % already registered',
        [self, aImplementationClass, aInterface^.RawName]);
    end;
end; // caller should explicitly call fSafe.WriteUnLock

procedure TInterfaceResolverList.Add(aInterface: PRttiInfo;
  aImplementationClass: TInterfacedObjectClass);
var
  e: PInterfaceEntry;
  n: PtrInt;
begin
  e := PrepareAddAndWriteLock(aInterface, aImplementationClass);
  try
    // here we are protected within a fSafe.WriteLock
    n := length(fEntry);
    SetLength(fEntry, n + 1);
    with fEntry[n] do
    begin
      TypeInfo := aInterface;
      ImplementationClass := Rtti.RegisterClass(aImplementationClass);
      InterfaceEntry := e;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TInterfaceResolverList.Add(aInterface: PRttiInfo;
  aImplementation: TInterfacedObject);
var
  e: PInterfaceEntry;
  n: PtrInt;
begin
  e := PrepareAddAndWriteLock(aInterface, aImplementation.ClassType);
  try
    // here we are protected within a fSafe.WriteLock
    n := length(fEntry);
    SetLength(fEntry, n + 1);
    with fEntry[n] do
    begin
      TypeInfo := aInterface;
      if not GetInterfaceFromEntry(aImplementation, e, Instance) then
        raise EInterfaceResolver.CreateUtf8('Unexcepted %.Add(%,%)',
          [self, aInterface^.RawName, aImplementation]);
      InterfaceEntry := e;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TInterfaceResolverList.Delete(aInterface: PRttiInfo);
var
  i, last: PtrInt;
begin
  if (aInterface = nil) or
     (aInterface^.Kind <> rkInterface) then
    raise EInterfaceResolver.CreateUtf8('%.Delete(?)', [self]);
  fSafe.WriteLock;
  try
    last := length(fEntry) - 1;
    for i := 0 to last do
      with fEntry[i] do
        if TypeInfo = aInterface then
        begin
          if Instance = nil then
            raise EInterfaceResolver.CreateUtf8(
              '%.Delete(%) does not match an instance, but a class',
              [self, aInterface^.RawName]);
          Instance := nil; // avoid GPF
          if last > i then
            MoveFast(fEntry[i + 1], fEntry[i], (last - i) * SizeOf(fEntry[i]));
          SetLength(fEntry, last);
          exit;
        end;
  finally
    fSafe.WriteUnLock;
  end;
end;

function LockedFind(aList: TInterfaceResolverList;
  aInterface: PRttiInfo): PInterfaceResolverListEntry;
  {$ifdef HASINLINE} inline; {$endif}
var
  n: integer;
begin
  result := pointer(aList.fEntry);
  if result = nil then
    exit;
  // fast brute-force search in L1 cache (TInterfaceResolverListEntries)
  n := PDALen(PAnsiChar(result) - _DALEN)^ + _DAOFF;
  repeat
    if result^.TypeInfo = aInterface then
      exit;
    inc(result);
    dec(n);
  until n = 0;
  result := nil;
end;

function TInterfaceResolverList.TryResolve(aInterface: PRttiInfo;
  out Obj): boolean;
var
  e: PInterfaceResolverListEntry;
  new: TInterfacedObject;
begin
  new := nil;
  result := true;
  fSafe.ReadLock; // Multiple Read / Exclusive Write lock
  try
    e := LockedFind(self, aInterface);
    if e <> nil then
    begin
      if e^.Instance <> nil then
      begin
        // will increase the reference count of the shared instance
        IInterface(Obj) := e^.Instance;
        exit;
      end
      else
      begin
        // create a new instance of this registered implementation class
        new := e^.ImplementationClass.ClassNewInstance;
        if not GetInterfaceFromEntry(new , e^.InterfaceEntry, Obj) then
          FreeAndNilSafe(new); // avoid memory leak (paranoid)
      end;
    end;
  finally
    fSafe.ReadUnLock;
  end;
  if new <> nil then
  begin
    if Assigned(fOnCreateInstance) then
      // this event should be called outside fSafe lock
      fOnCreateInstance(self, new);
    exit;
  end;
  result := false;
end;

function TInterfaceResolverList.Implements(aInterface: PRttiInfo): boolean;
begin
  fSafe.ReadLock; // Multiple Read / Exclusive Write lock
  result := LockedFind(self, aInterface) <> nil;
  fSafe.ReadUnLock;
end;


{ TInterfaceResolverInjected }

class procedure TInterfaceResolverInjected.RegisterGlobal(aInterface: PRttiInfo;
  aImplementationClass: TInterfacedObjectClass);
begin
  GlobalInterfaceResolver.Add(aInterface, aImplementationClass);
end;

class procedure TInterfaceResolverInjected.RegisterGlobal(aInterface: PRttiInfo;
  aImplementation: TInterfacedObject);
begin
  GlobalInterfaceResolver.Add(aInterface, aImplementation);
end;

class procedure TInterfaceResolverInjected.RegisterGlobalDelete(aInterface: PRttiInfo);
begin
  GlobalInterfaceResolver.Delete(aInterface);
end;

function TInterfaceResolverInjected.TryResolve(aInterface: PRttiInfo;
  out Obj): boolean;
var
  i: PtrInt;
begin
  if aInterface <> nil then
  begin
    result := true;
    // first check local DI/IoC
    if fResolvers <> nil then
      for i := 0 to length(fResolvers) - 1 do
        if fResolvers[i].TryResolve(aInterface, Obj) then
          exit;
    if fDependencies <> nil then
      for i := 0 to Length(fDependencies) - 1 do
        if fDependencies[i].GetInterface(aInterface^.InterfaceGuid^, Obj) then
          exit;
    // then try global shared DI/IoC
    if GlobalInterfaceResolver.TryResolve(aInterface, Obj) then
      exit;
  end;
  result := false;
end;

function TInterfaceResolverInjected.Implements(aInterface: PRttiInfo): boolean;
var
  i: PtrInt;
begin
  result := true;
  if (self <> nil) and
     (aInterface <> nil) and
     (fResolvers <> nil) then
    for i := 0 to length(fResolvers) - 1 do
      if fResolvers[i].Implements(aInterface) then
        exit;
  result := false;
end;

procedure TInterfaceResolverInjected.InjectStub(const aStubsByGUID: array of TGUID);
var
  i: PtrInt;
begin
  for i := 0 to high(aStubsByGUID) do
    InjectResolver([TInterfaceStub.Create(aStubsByGUID[i])]);
end;

procedure TInterfaceResolverInjected.InjectResolver(
  const aOtherResolvers: array of TInterfaceResolver; OwnOtherResolvers: boolean);
var
  i: PtrInt;
begin
  for i := 0 to high(aOtherResolvers) do
    if aOtherResolvers[i] <> nil then
    begin
      if aOtherResolvers[i].InheritsFrom(TInterfaceStub) then
      begin
        include(TInterfaceStub(aOtherResolvers[i]).fOptions,
          imoFakeInstanceWontReleaseTInterfaceStub);
        ObjArrayAdd(fResolversToBeReleased, aOtherResolvers[i]);
      end
      else if OwnOtherResolvers then
        ObjArrayAdd(fResolversToBeReleased, aOtherResolvers[i]);
      ObjArrayAddOnce(fResolvers, aOtherResolvers[i]);
    end;
end;

procedure TInterfaceResolverInjected.InjectInstance(
  const aDependencies: array of TInterfacedObject);
var
  i: PtrInt;
begin
  for i := 0 to high(aDependencies) do
    if aDependencies[i] <> nil then
    begin
      IInterface(aDependencies[i])._AddRef; // Destroy will do _Release
      ObjArrayAdd(fDependencies, aDependencies[i]);
    end;
end;

destructor TInterfaceResolverInjected.Destroy;
var
  i: PtrInt;
begin
  try
    ObjArrayClear(fResolversToBeReleased);
    for i := 0 to length(fDependencies) - 1 do
      IInterface(fDependencies[i])._Release;
  finally
    inherited Destroy;
  end;
end;


{ TInjectableObject }

function TInjectableObject.TryResolve(aInterface: PRttiInfo; out Obj): boolean;
begin
  if (self <> nil) and
     (aInterface <> nil) and
     (fResolver <> nil) then
    result := fResolver.TryResolve(aInterface, Obj)
  else
    result := false;
end;

procedure TInjectableObject.Resolve(aInterface: PRttiInfo; out Obj);
begin
  if not TryResolve(aInterface, Obj) then
    raise EInterfaceResolver.CreateUtf8('%.Resolve(%) unsatisfied', [self,
      aInterface^.RawName]);
end;

procedure TInjectableObject.Resolve(const aGuid: TGUID; out Obj);
var
  info: PRttiInfo;
begin
  info := TInterfaceFactory.Guid2TypeInfo(aGuid);
  if not TryResolve(info, Obj) then
    raise EInterfaceResolver.CreateUtf8('%.Resolve(%): unsatisfied',
      [self, info^.RawName]);
end;

procedure TInjectableObject.ResolveByPair(const aInterfaceObjPairs: array of pointer);
begin
  if fResolver.InheritsFrom(TInterfaceResolverInjected) then
    TInterfaceResolverInjected(fResolver).ResolveByPair(aInterfaceObjPairs)
  else if high(aInterfaceObjPairs) = 1 then
    Resolve(aInterfaceObjPairs[0], aInterfaceObjPairs[1]^)
  else
    raise EInterfaceResolver.CreateUtf8('%.ResolveByPair(?)', [self]);
end;

procedure TInjectableObject.Resolve(const aInterfaces: array of TGUID;
  const aObjs: array of pointer);
begin
  if fResolver.InheritsFrom(TInterfaceResolverInjected) then
    TInterfaceResolverInjected(fResolver).Resolve(aInterfaces, aObjs)
  else if (high(aInterfaces) = 0) and
          (high(aObjs) = 0) then
    Resolve(aInterfaces[0], aObjs[0]^)
  else
    raise EInterfaceResolver.CreateUtf8('%.Resolve(?,?)', [self]);
end;

type // to access fAutoCreateInterfaces protected field
  TRttiCustomWrapper = class(TRttiJson);

procedure TInjectableObject.AutoResolve(aRaiseEServiceExceptionIfNotFound: boolean);
var
  rtti: TRttiJson;
  n: integer;
  p: ^PRttiCustomProp;
  addr: pointer;
begin
  if (self = nil) or
     (fResolver = nil) then
    raise EInterfaceResolver.CreateUtf8(
      '%.AutoResolve with no prior registration', [self]);
  // inlined Rtti.RegisterClass()
  rtti := PPointer(PPAnsiChar(self)^ + vmtAutoTable)^;
  if (rtti = nil) or
     not (rcfAutoCreateFields in rtti.Flags) then
    rtti := DoRegisterAutoCreateFields(self);
  // resolve all published interface fields
  p := pointer(TRttiCustomWrapper(rtti).fAutoCreateInterfaces);
  if p = nil then
    exit;
  n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF; // length(AutoCreateClasses)
  repeat
    addr := PAnsiChar(self) + p^^.OffsetGet;
    if not TryResolve(p^^.Value.Info, addr^) then
      if aRaiseEServiceExceptionIfNotFound then
        raise EInterfaceResolver.CreateUtf8(
          '%.AutoResolve: impossible to resolve published property %: %',
          [self, p^^.Name, p^^.Value.Name]);
    inc(p);
    dec(n);
  until n = 0;
end;

constructor TInjectableObject.CreateInjected(const aStubsByGUID: array of TGUID;
  const aOtherResolvers: array of TInterfaceResolver;
  const aDependencies: array of TInterfacedObject;
  aRaiseEServiceExceptionIfNotFound: boolean);
begin
  fResolver := TInterfaceResolverInjected.Create;
  fResolverOwned := true;
  TInterfaceResolverInjected(fResolver).InjectStub(aStubsByGUID);
  TInterfaceResolverInjected(fResolver).InjectResolver(aOtherResolvers);
  TInterfaceResolverInjected(fResolver).InjectInstance(aDependencies);
  Create;
  AutoResolve(aRaiseEServiceExceptionIfNotFound);
end;

constructor TInjectableObject.CreateWithResolver(aResolver: TInterfaceResolver;
  aRaiseEServiceExceptionIfNotFound: boolean);
begin
  if fResolver <> nil then
    exit; // inject once!
  if aResolver = nil then
    raise EInterfaceResolver.CreateUtf8('%.CreateWithResolver(nil)', [self]);
  fResolver := aResolver; // may be needed by overriden Create
  Create;
  AutoResolve(aRaiseEServiceExceptionIfNotFound);
end;

destructor TInjectableObject.Destroy;
begin
  inherited Destroy;
  CleanupInstance; // ensure creatures are released before their creator
  if fResolverOwned then
    FreeAndNilSafe(fResolver); // let the creator move away
end;


{ ************ TInterfaceStub TInterfaceMock for Dependency Mocking }

{ EInterfaceStub }

constructor EInterfaceStub.Create(Sender: TInterfaceStub;
  const Method: TInterfaceMethod; const Error: RawUtf8);
begin
  inherited CreateUtf8('Error in % for %.% - %', [Sender, Sender.fInterface.fInterfaceName,
    Method.Uri, Error]);
end;

constructor EInterfaceStub.Create(Sender: TInterfaceStub;
  const Method: TInterfaceMethod; const Format: RawUtf8; const Args: array of const);
begin
  Create(Sender, Method, FormatUtf8(Format, Args));
end;


{ TInterfaceStubRules }

function TInterfaceStubRules.FindRuleIndex(const aParams: RawUtf8): integer;
begin
  for result := 0 to length(Rules) - 1 do
    if Rules[result].Params = aParams then
      exit;
  result := -1;
end;

function TInterfaceStubRules.FindStrongRuleIndex(const aParams: RawUtf8): integer;
begin
  for result := 0 to length(Rules) - 1 do
    if (Rules[result].Kind <> isUndefined) and
       (Rules[result].Params = aParams) then
      exit;
  result := -1;
end;

procedure TInterfaceStubRules.AddRule(Sender: TInterfaceStub;
  aKind: TInterfaceStubRuleKind; const aParams, aValues: RawUtf8;
  const aEvent: TNotifyEvent; aExceptionClass: ExceptClass;
  aExpectedPassCountOperator: TInterfaceStubRuleOperator; aValue: cardinal);
var
  n, ndx: integer;
begin
  ndx := FindRuleIndex(aParams);
  n := length(Rules);
  if ndx < 0 then
    SetLength(Rules, n + 1)
  else
    n := ndx;
  if (aParams = '') and
     (aKind <> isUndefined) then
    DefaultRule := n;
  with Rules[n] do
  begin
    Params := aParams;
    case aKind of
      isUndefined:
        ; // do not overwrite Values for weak rules like ExpectsCount/ExpectsTrace
      isReturns:
        Values := '[' + aValues + ']';
      isFails:
        Values := ToText(Sender.ClassType) + ' returned error: ' + aValues;
    else
      Values := aValues;
    end;
    if aKind = isUndefined then
      if aExpectedPassCountOperator = ioTraceMatch then
        ExpectedTraceHash := aValue
      else
      begin
        ExpectedPassCountOperator := aExpectedPassCountOperator;
        ExpectedPassCount := aValue;
      end
    else
    begin
      Kind := aKind;
      Execute := TMethod(aEvent);
      ExceptionClass := aExceptionClass;
    end;
  end;
end;


{ TInterfaceStubLog }

function TInterfaceStubLog.Results: RawUtf8;
begin
  if CustomResults = '' then
    result := method^.DefaultResult
  else
    result := CustomResults;
end;

procedure TInterfaceStubLog.AddAsText(WR: TJsonWriter; aScope:
  TInterfaceStubLogLayouts; SepChar: AnsiChar);
begin
  if wName in aScope then
    WR.AddString(method^.Uri);
  if wParams in aScope then
  begin
    WR.Add('(');
    WR.AddString(Params);
    WR.Add(')');
  end;
  if WasError then
  begin
    WR.AddShort(' error "');
    WR.AddString(CustomResults);
    WR.Add('"');
  end
  else if (wResults in aScope) and
          (method^.ArgsResultIndex >= 0) then
  begin
    if (wName in aScope) or
       (wParams in aScope) then
      WR.Add('=');
    if CustomResults = '' then
      WR.AddString(method^.DefaultResult)
    else
      WR.AddString(CustomResults);
  end;
  WR.Add(SepChar);
end;


{ TOnInterfaceStubExecuteParamsAbstract }

constructor TOnInterfaceStubExecuteParamsAbstract.Create(aSender: TInterfaceStub;
  aMethod: PInterfaceMethod; const aParams, aEventParams: RawUtf8);
begin
  fSender := aSender;
  fMethod := aMethod;
  fParams := aParams;
  fEventParams := aEventParams;
end;

procedure TOnInterfaceStubExecuteParamsAbstract.Error(const Format: RawUtf8;
  const Args: array of const);
begin
  Error(FormatUtf8(Format, Args));
end;

procedure TOnInterfaceStubExecuteParamsAbstract.Error(const aErrorMessage: RawUtf8);
begin
  fFailed := true;
  fResult := aErrorMessage;
end;

function TOnInterfaceStubExecuteParamsAbstract.GetSenderAsMockTestCase: TSynTestCase;
begin
  result := (fSender as TInterfaceMock).TestCase;
end;

{ TOnInterfaceStubExecuteParamsJson }

procedure TOnInterfaceStubExecuteParamsJson.Returns(const Values: array of const);
begin
  JsonEncodeArrayOfConst(Values, false, fResult);
end;

procedure TOnInterfaceStubExecuteParamsJson.Returns(const ValuesJsonArray: RawUtf8);
begin
  fResult := ValuesJsonArray;
end;

{ TOnInterfaceStubExecuteParamsVariant }

constructor TOnInterfaceStubExecuteParamsVariant.Create(aSender: TInterfaceStub;
  aMethod: PInterfaceMethod; const aParams, aEventParams: RawUtf8);
var
  i: PtrInt;
  P: PUtf8Char;
  tmp: TSynTempBuffer;
begin
  inherited;
  SetLength(fInput, fMethod^.ArgsInputValuesCount);
  tmp.Init(aParams);
  try
    P := tmp.buf;
    for i := 0 to fMethod^.ArgsInputValuesCount - 1 do
      GetJsonToAnyVariant(
        fInput[i], P, nil, @aSender.fInterface.DocVariantOptions, false);
  finally
    tmp.Done;
  end;
  SetLength(fOutput, fMethod^.ArgsOutputValuesCount);
end;

function TOnInterfaceStubExecuteParamsVariant.GetInput(Index: integer): variant;
begin
  if cardinal(Index) >= fMethod^.ArgsInputValuesCount then
    raise EInterfaceStub.Create(fSender, fMethod^, 'Input[%>=%]', [Index,
      fMethod^.ArgsInputValuesCount])
  else
    result := fInput[Index];
end;

procedure TOnInterfaceStubExecuteParamsVariant.SetOutput(Index: integer;
  const Value: variant);
begin
  if cardinal(Index) >= fMethod^.ArgsOutputValuesCount then
    raise EInterfaceStub.Create(fSender, fMethod^, 'Output[%>=%]',
      [Index, fMethod^.ArgsOutputValuesCount])
  else
    fOutput[Index] := Value;
end;

function TOnInterfaceStubExecuteParamsVariant.GetInNamed(
  const aParamName: RawUtf8): variant;
var
  L, a, ndx: integer;
begin
  L := Length(aParamName);
  ndx := 0;
  if (L > 0) and
     (fInput <> nil) then
    for a := fMethod^.ArgsInFirst to fMethod^.ArgsInLast do
      with fMethod^.Args[a] do
        if ValueDirection in [imdConst, imdVar] then
        begin
          if IdemPropName(ParamName^, pointer(aParamName), L) then
          begin
            result := fInput[ndx];
            exit;
          end;
          inc(ndx);
          if cardinal(ndx) >= cardinal(fMethod^.ArgsInputValuesCount) then
            break;
        end;
  raise EInterfaceStub.Create(fSender, fMethod^, 'unknown input parameter [%]',
    [aParamName]);
end;

function TOnInterfaceStubExecuteParamsVariant.GetInUtf8(
  const ParamName: RawUtf8): RawUtf8;
var
  wasString: boolean;
begin
  result := '';
  VariantToUtf8(GetInNamed(ParamName), result, wasString);
end;

procedure TOnInterfaceStubExecuteParamsVariant.SetOutNamed(
  const aParamName: RawUtf8; const Value: variant);
var
  L, a, ndx: integer;
begin
  L := Length(aParamName);
  ndx := 0;
  if (L > 0) and
     (fOutput <> nil) then
    for a := fMethod^.ArgsOutFirst to fMethod^.ArgsOutLast do
      with fMethod^.Args[a] do
        if ValueDirection <> imdConst then
        begin
          if IdemPropName(ParamName^, pointer(aParamName), L) then
          begin
            fOutput[ndx] := Value;
            exit;
          end;
          inc(ndx);
          if cardinal(ndx) >= cardinal(fMethod^.ArgsOutputValuesCount) then
            break;
        end;
  raise EInterfaceStub.Create(fSender, fMethod^, 'unknown output parameter [%]',
    [aParamName]);
end;

procedure TOnInterfaceStubExecuteParamsVariant.SetResultFromOutput;
var
  a, ndx: integer;
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  fResult := '';
  if fOutput = nil then
    exit;
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.Add('[');
    ndx := 0;
    for a := fMethod^.ArgsOutFirst to fMethod^.ArgsOutLast do
      with fMethod^.Args[a] do
        if ValueDirection <> imdConst then
        begin
          if TVarData(fOutput[ndx]).VType = varEmpty then
            AddDefaultJson(W)
          else
          begin
            W.AddVariant(fOutput[ndx], twJsonEscape);
            W.AddComma;
          end;
          inc(ndx);
          if cardinal(ndx) >= cardinal(fMethod^.ArgsOutputValuesCount) then
            break;
        end;
    W.CancelLastComma;
    W.Add(']');
    W.SetText(fResult);
  finally
    W.Free;
  end;
end;

function TOnInterfaceStubExecuteParamsVariant.InputAsDocVariant(
  Kind: TInterfaceMethodParamsDocVariantKind; Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  fMethod^.ArgsValuesAsDocVariant(Kind, TDocVariantData(result), fInput, true, Options);
end;

function TOnInterfaceStubExecuteParamsVariant.OutputAsDocVariant(
  Kind: TInterfaceMethodParamsDocVariantKind; Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  fMethod^.ArgsValuesAsDocVariant(Kind, TDocVariantData(result), fOutput, false, Options);
end;

procedure TOnInterfaceStubExecuteParamsVariant.AddLog(aLog: TSynLogClass;
  aOutput: boolean; aLevel: TSynLogInfo);
var
  val: variant;
begin
  if aLog = nil then
    exit;
  with aLog.Family do
    if aLevel in Level then
    begin
      if aOutput then
        val := OutputAsDocVariant(pdvObjectFixed)
      else
        val := InputAsDocVariant(pdvObjectFixed);
      SynLog.Log(aLevel, '%(%)', [fMethod^.InterfaceDotMethodName,
         _Safe(val)^.ToTextPairs('=', ',', twJsonEscape)], self);
    end;
end;


{ TInterfaceStub }

constructor TInterfaceStub.Create(aFactory: TInterfaceFactory;
  const aInterfaceName: RawUtf8);
var
  i: PtrInt;
begin
  if aFactory = nil then
    raise EInterfaceStub.CreateUtf8(
      '%.Create(%): Interface not registered - you could use ' +
      'TInterfaceFactory.RegisterInterfaces()', [self, aInterfaceName]);
  fInterface := aFactory;
  SetLength(fRules, fInterface.MethodsCount);
  for i := 0 to fInterface.MethodsCount - 1 do
    fRules[i].DefaultRule := -1;
  fLog.Init(TypeInfo(TInterfaceStubLogDynArray), fLogs, @fLogCount);
end;

procedure TInterfaceStub.InternalGetInstance(out aStubbedInterface);
var
  fake: TInterfacedObjectFake;
begin
  fake := TInterfacedObjectFake.Create(fInterface, nil,
    [ifoJsonAsExtended, ifoDontStoreVoidJson], Invoke, InstanceDestroyed);
  pointer(aStubbedInterface) := @fake.fVTable;
  fake._AddRef;
  fLastInterfacedObjectFake := fake;
end;

function TInterfaceStub.InternalCheck(aValid, aExpectationFailed: boolean;
  const aErrorMsgFmt: RawUtf8; const aErrorMsgArgs: array of const): boolean;
begin
  result := aValid;
  if aExpectationFailed and
     not aValid then
    raise EInterfaceStub.CreateUtf8('%.InternalCheck(%) failed: %', [self,
      fInterface.fInterfaceName, FormatUtf8(aErrorMsgFmt, aErrorMsgArgs)]);
end;

constructor TInterfaceStub.Create(const aInterfaceName: RawUtf8; out aStubbedInterface);
begin
  Create(TInterfaceFactory.Get(aInterfaceName), aInterfaceName);
  InternalGetInstance(aStubbedInterface);
end;

constructor TInterfaceStub.Create(const aGuid: TGUID; out aStubbedInterface);
begin
  Create(TInterfaceFactory.Get(aGuid), GuidToRawUtf8(aGuid));
  InternalGetInstance(aStubbedInterface);
end;

constructor TInterfaceStub.Create(aInterface: PRttiInfo; out aStubbedInterface);
begin
  Create(aInterface);
  InternalGetInstance(aStubbedInterface);
end;

constructor TInterfaceStub.Create(aInterface: PRttiInfo);
begin
  Create(TInterfaceFactory.Get(aInterface), ToUtf8(aInterface^.RawName));
end;

constructor TInterfaceStub.Create(const aGuid: TGUID);
begin
  Create(TInterfaceFactory.Get(aGuid), ToUtf8(aGuid));
end;

procedure TInterfaceStub.IntSetOptions(Options: TInterfaceStubOptions);
begin
  if Options = fOptions then
    exit;
  fOptions := Options;
end;

procedure TInterfaceStub.IntCheckCount(aMethodIndex, aComputed: cardinal;
  aOperator: TInterfaceStubRuleOperator; aCount: cardinal);
var
  ok: boolean;
begin
  case aOperator of
    ioEqualTo:
      ok := aComputed = aCount;
    ioNotEqualTo:
      ok := aComputed <> aCount;
    ioLessThan:
      ok := aComputed < aCount;
    ioLessThanOrEqualTo:
      ok := aComputed <= aCount;
    ioGreaterThan:
      ok := aComputed > aCount;
    ioGreaterThanOrEqualTo:
      ok := aComputed >= aCount;
  else
    raise EInterfaceStub.CreateUtf8(
      '%.IntCheckCount(): Unexpected % operator', [self, Ord(aOperator)]);
  end;
  InternalCheck(ok, True, 'ExpectsCount(''%'',%,%) failed: count=%',
    [fInterface.Methods[aMethodIndex].Uri, ToText(aOperator)^, aCount, aComputed]);
end;

procedure TInterfaceStub.InstanceDestroyed(aFakeID: TInterfacedObjectFakeID);
var
  m, r, asmndx: integer;
  num: cardinal;
begin
  if self <> nil then
  try
    if eCount in fHasExpects then
      for m := 0 to fInterface.MethodsCount - 1 do
        with fRules[m] do
          for r := 0 to high(Rules) do
            with Rules[r] do
              if ExpectedPassCountOperator <> ioUndefined then
              begin
                if Params = '' then
                  num := MethodPassCount
                else
                  num := RulePassCount;
                IntCheckCount(m, num, ExpectedPassCountOperator, ExpectedPassCount);
              end;
    if fInterfaceExpectedTraceHash <> 0 then
      InternalCheck(LogHash = fInterfaceExpectedTraceHash, True,
        'ExpectsTrace(%) returned %', [fInterfaceExpectedTraceHash, LogHash]);
    if eTrace in fHasExpects then
      for m := 0 to fInterface.MethodsCount - 1 do
        with fRules[m] do
        begin
          asmndx := m + RESERVED_VTABLE_SLOTS;
          for r := 0 to high(Rules) do
            with Rules[r] do
              if ExpectedTraceHash <> 0 then
                InternalCheck(
                  ExpectedTraceHash = Hash32(IntGetLogAsText(asmndx, Params,
                    [wName, wParams, wResults], ',')), True,
                  'ExpectsTrace(''%'') failed', [fInterface.Methods[m].Uri]);
        end;
  finally
    if not (imoFakeInstanceWontReleaseTInterfaceStub in Options) then
      Free; // creature will release its creator
  end;
end;

function TInterfaceStub.SetOptions(Options: TInterfaceStubOptions): TInterfaceStub;
begin
  IntSetOptions(Options);
  result := self;
end;

function TInterfaceStub.Executes(const aMethodName, aParams: RawUtf8;
  const aEvent: TOnInterfaceStubExecuteJson; const aEventParams: RawUtf8): TInterfaceStub;
begin
  fRules[fInterface.CheckMethodIndex(aMethodName)].AddRule(self, isExecutesJson,
    aParams, aEventParams, TNotifyEvent(aEvent));
  result := self;
end;

function TInterfaceStub.Executes(const aMethodName: RawUtf8;
  const aEvent: TOnInterfaceStubExecuteJson; const aEventParams: RawUtf8): TInterfaceStub;
begin
  result := Executes(aMethodName, '', aEvent, aEventParams);
end;

function TInterfaceStub.Executes(const aMethodName: RawUtf8;
  const aParams: array of const; const aEvent: TOnInterfaceStubExecuteJson;
  const aEventParams: RawUtf8): TInterfaceStub;
begin
  result := Executes(aMethodName, JsonEncodeArrayOfConst(aParams, true),
    aEvent, aEventParams);
end;

function TInterfaceStub.Executes(const aMethodName, aParams: RawUtf8;
  const aEvent: TOnInterfaceStubExecuteVariant;
  const aEventParams: RawUtf8): TInterfaceStub;
begin
  fRules[fInterface.CheckMethodIndex(aMethodName)].AddRule(self,
    isExecutesVariant, aParams, aEventParams, TNotifyEvent(aEvent));
  result := self;
end;

function TInterfaceStub.Executes(const aMethodName: RawUtf8;
  const aEvent: TOnInterfaceStubExecuteVariant;
  const aEventParams: RawUtf8): TInterfaceStub;
begin
  result := Executes(aMethodName, '', aEvent, aEventParams);
end;

function TInterfaceStub.Executes(const aMethodName: RawUtf8;
  const aParams: array of const; const aEvent: TOnInterfaceStubExecuteVariant;
  const aEventParams: RawUtf8): TInterfaceStub;
begin
  result := Executes(aMethodName, JsonEncodeArrayOfConst(aParams, true),
    aEvent, aEventParams);
end;

function TInterfaceStub.Executes(const aEvent: TOnInterfaceStubExecuteVariant;
  const aEventParams: RawUtf8): TInterfaceStub;
var
  i: PtrInt;
begin
  for i := 0 to fInterface.MethodsCount - 1 do
    fRules[i].AddRule(
      self, isExecutesVariant, '', aEventParams, TNotifyEvent(aEvent));
  result := self;
end;

type
  TInterfaceStubExecutesToLog = packed record
    Log: TSynLogClass;
    LogLevel: TSynLogInfo;
    Kind: TInterfaceMethodParamsDocVariantKind;
  end;
  PInterfaceStubExecutesToLog = ^TInterfaceStubExecutesToLog;

procedure TInterfaceStub.OnExecuteToLog(Ctxt: TOnInterfaceStubExecuteParamsVariant);
begin
  if length(Ctxt.EventParams) = SizeOf(TInterfaceStubExecutesToLog) then
    with PInterfaceStubExecutesToLog(Ctxt.EventParams)^ do
      Log.Add.Log(LogLevel, '% %', [Ctxt.Method^.InterfaceDotMethodName,
       Ctxt.InputAsDocVariant(Kind, JSON_FAST_EXTENDED)]);
end;

function TInterfaceStub.Executes(aLog: TSynLogClass; aLogLevel: TSynLogInfo;
  aKind: TInterfaceMethodParamsDocVariantKind): TInterfaceStub;
var
  tmp: RawUtf8;
begin
  SetLength(tmp, SizeOf(TInterfaceStubExecutesToLog));
  with PInterfaceStubExecutesToLog(tmp)^ do
  begin
    Log := aLog;
    LogLevel := aLogLevel;
    Kind := aKind;
  end;
  Executes(OnExecuteToLog, tmp);
  result := self;
end;

function TInterfaceStub.ExpectsCount(const aMethodName: RawUtf8;
  aOperator: TInterfaceStubRuleOperator; aValue: cardinal): TInterfaceStub;
begin
  result := ExpectsCount(aMethodName, '', aOperator, aValue);
end;

function TInterfaceStub.ExpectsCount(const aMethodName, aParams: RawUtf8;
  aOperator: TInterfaceStubRuleOperator; aValue: cardinal): TInterfaceStub;
var
  ndx: integer;
begin
  ndx := fInterface.CheckMethodIndex(aMethodName);
  if aOperator in [ioEqualTo..ioGreaterThanOrEqualTo] then
    with fRules[ndx] do
      AddRule(self, isUndefined, aParams, '', nil, nil, aOperator, aValue)
  else
    raise EInterfaceStub.Create(self, fInterface.fMethods[ndx],
      'ExpectsCount(aOperator=%)', [ord(aOperator)]);
  include(fHasExpects, eCount);
  result := self;
end;

function TInterfaceStub.ExpectsCount(const aMethodName: RawUtf8;
  const aParams: array of const; aOperator: TInterfaceStubRuleOperator;
  aValue: cardinal): TInterfaceStub;
begin
  result := ExpectsCount(aMethodName, JsonEncodeArrayOfConst(aParams, true),
    aOperator, aValue);
end;

function TInterfaceStub.ExpectsTrace(aValue: cardinal): TInterfaceStub;
begin
  include(fOptions, imoLogMethodCallsAndResults);
  fInterfaceExpectedTraceHash := aValue;
  result := self;
end;

function TInterfaceStub.ExpectsTrace(const aMethodName: RawUtf8;
  aValue: cardinal): TInterfaceStub;
begin
  result := ExpectsTrace(aMethodName, '', aValue);
end;

function TInterfaceStub.ExpectsTrace(const aMethodName, aParams: RawUtf8;
  aValue: cardinal): TInterfaceStub;
begin
  fRules[fInterface.CheckMethodIndex(aMethodName)].AddRule(
    self, isUndefined, aParams, '', nil, nil, ioTraceMatch, aValue);
  include(fOptions, imoLogMethodCallsAndResults);
  include(fHasExpects, eTrace);
  result := self;
end;

function TInterfaceStub.ExpectsTrace(const aMethodName: RawUtf8;
  const aParams: array of const; aValue: cardinal): TInterfaceStub;
begin
  result := ExpectsTrace(aMethodName, JsonEncodeArrayOfConst(aParams, true), aValue);
end;

function TInterfaceStub.ExpectsTrace(const aValue: RawUtf8): TInterfaceStub;
begin
  result := ExpectsTrace(Hash32(aValue));
end;

function TInterfaceStub.ExpectsTrace(const aMethodName, aValue: RawUtf8): TInterfaceStub;
begin
  result := ExpectsTrace(aMethodName, Hash32(aValue));
end;

function TInterfaceStub.ExpectsTrace(const aMethodName, aParams, aValue: RawUtf8):
  TInterfaceStub;
begin
  result := ExpectsTrace(aMethodName, aParams, Hash32(aValue));
end;

function TInterfaceStub.ExpectsTrace(const aMethodName: RawUtf8;
  const aParams: array of const; const aValue: RawUtf8): TInterfaceStub;
begin
  result := ExpectsTrace(aMethodName, aParams, Hash32(aValue));
end;

function TInterfaceStub.Fails(const aMethodName, aErrorMsg: RawUtf8): TInterfaceStub;
begin
  result := Fails(aMethodName, '', aErrorMsg);
end;

function TInterfaceStub.Fails(const aMethodName, aParams,
  aErrorMsg: RawUtf8): TInterfaceStub;
begin
  fRules[fInterface.CheckMethodIndex(aMethodName)].AddRule(
    self, isFails, aParams, aErrorMsg);
  result := self;
end;

function TInterfaceStub.Fails(const aMethodName: RawUtf8;
  const aParams: array of const; const aErrorMsg: RawUtf8): TInterfaceStub;
begin
  result := Fails(aMethodName, JsonEncodeArrayOfConst(aParams, true), aErrorMsg);
end;

function TInterfaceStub.Raises(const aMethodName, aParams: RawUtf8;
  aException: ExceptClass; const aMessage: string): TInterfaceStub;
begin
  fRules[fInterface.CheckMethodIndex(aMethodName)].AddRule(
    self, isRaises, aParams, StringToUtf8(aMessage), nil, aException);
  result := self;
end;

function TInterfaceStub.Raises(const aMethodName: RawUtf8;
  const aParams: array of const; aException: ExceptClass;
  const aMessage: string): TInterfaceStub;
begin
  result := Raises(aMethodName, JsonEncodeArrayOfConst(aParams, true),
    aException, aMessage);
end;

function TInterfaceStub.Raises(const aMethodName: RawUtf8;
  aException: ExceptClass; const aMessage: string): TInterfaceStub;
begin
  result := Raises(aMethodName, '', aException, aMessage);
end;

function TInterfaceStub.Returns(const aMethodName, aParams, aExpectedResults:
  RawUtf8): TInterfaceStub;
begin
  fRules[fInterface.CheckMethodIndex(aMethodName)].AddRule(
    self, isReturns, aParams, aExpectedResults);
  result := self;
end;

function TInterfaceStub.Returns(const aMethodName: RawUtf8;
  const aParams, aExpectedResults: array of const): TInterfaceStub;
begin
  result := Returns(aMethodName, JsonEncodeArrayOfConst(aParams, true),
    JsonEncodeArrayOfConst(aExpectedResults, true));
end;

function TInterfaceStub.Returns(const aMethodName,
  aExpectedResults: RawUtf8): TInterfaceStub;
begin
  result := Returns(aMethodName, '', aExpectedResults);
end;

function TInterfaceStub.Returns(const aMethodName: RawUtf8;
  const aExpectedResults: array of const): TInterfaceStub;
begin
  result := Returns(aMethodName, '', JsonEncodeArrayOfConst(aExpectedResults, true));
end;

function TInterfaceStub.Invoke(const aMethod: TInterfaceMethod;
  const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
  aFakeID: PInterfacedObjectFakeID;
  aServiceCustomAnswer: PServiceCustomAnswer): boolean;
var
  ndx: cardinal;
  rule: integer;
  ExecutesCtxtJson: TOnInterfaceStubExecuteParamsJson;
  ExecutesCtxtVariant: TOnInterfaceStubExecuteParamsVariant;
  log: TInterfaceStubLog;
begin
  ndx := aMethod.ExecutionMethodIndex - RESERVED_VTABLE_SLOTS;
  if ndx >= fInterface.MethodsCount then
    result := false
  else
    with fRules[ndx] do
    begin
      inc(MethodPassCount);
      rule := FindStrongRuleIndex(aParams);
      if rule < 0 then
      begin
        rule := FindRuleIndex(aParams);
        if (rule >= 0) and
           (DefaultRule >= 0) then
          inc(Rules[rule].RulePassCount);
        rule := DefaultRule;
      end;
      if rule < 0 then
        if imoRaiseExceptionIfNoRuleDefined in Options then
          raise EInterfaceStub.Create(self, aMethod, 'No rule defined')
        else
        begin
          rule := FindRuleIndex(aParams);
          if rule >= 0 then
            inc(Rules[rule].RulePassCount);
          if imoReturnErrorIfNoRuleDefined in Options then
          begin
            result := false;
            FormatUtf8('No stubbing rule defined for %.%',
              [fInterface.fInterfaceName, aMethod.Uri], log.CustomResults);
          end
          else
            result := true;
        end
      else
        with Rules[rule] do
        begin
          inc(RulePassCount);
          case Kind of
            isExecutesJson:
              begin
                ExecutesCtxtJson := TOnInterfaceStubExecuteParamsJson.Create(
                  self, @aMethod, aParams, Values);
                try
                  TOnInterfaceStubExecuteJson(Execute)(ExecutesCtxtJson);
                  result := not ExecutesCtxtJson.Failed;
                  log.CustomResults := ExecutesCtxtJson.result;
                finally
                  ExecutesCtxtJson.Free;
                end;
              end;
            isExecutesVariant:
              begin
                ExecutesCtxtVariant := TOnInterfaceStubExecuteParamsVariant.Create(
                  self, @aMethod, aParams, Values);
                try
                  TOnInterfaceStubExecuteVariant(Execute)(ExecutesCtxtVariant);
                  result := not ExecutesCtxtVariant.Failed;
                  if result then
                  begin
                    ExecutesCtxtVariant.SetResultFromOutput;
                    log.CustomResults := ExecutesCtxtVariant.result;
                  end;
                finally
                  ExecutesCtxtVariant.Free;
                end;
              end;
            isRaises:
              raise ExceptionClass.Create(Utf8ToString(Values));
            isReturns:
              begin
                result := true;
                log.CustomResults := Values;
              end;
            isFails:
              begin
                result := InternalCheck(false, false, '%', [Values]);
                if not result then
                  log.CustomResults := Values;
              end;
          else
            // ignore isUndefined (ExpectsCount only) rules
            result := true;
          end;
        end;
      if result then
      begin
        if aResult <> nil then
          // make unique due to JsonDecode() by caller
          if log.CustomResults = '' then
            FastSetString(aResult^, pointer(aMethod.DefaultResult), length(aMethod.DefaultResult))
          else
            FastSetString(aResult^, pointer(log.CustomResults), length(log.CustomResults));
      end
      else if aErrorMsg <> nil then
        aErrorMsg^ := log.CustomResults;
      if imoLogMethodCallsAndResults in Options then
      begin
        log.Timestamp64 := GetTickCount64;
        log.WasError := not result;
        log.Method := @aMethod;
        log.Params := aParams;
        fLog.Add(log);
      end;
    end;
end;

function TInterfaceStub.LogAsText(SepChar: AnsiChar): RawUtf8;
begin
  result := IntGetLogAsText(0, '', [wName, wParams, wResults], SepChar);
end;

procedure TInterfaceStub.ClearLog;
begin
  fLog.Clear;
end;

function TInterfaceStub.IntGetLogAsText(asmndx: integer; const aParams: RawUtf8;
  aScope: TInterfaceStubLogLayouts; SepChar: AnsiChar): RawUtf8;
var
  i: integer;
  WR: TJsonWriter;
  temp: TTextWriterStackBuffer;
  log: ^TInterfaceStubLog;
begin
  if fLogCount = 0 then
    result := ''
  else
  begin
    WR := TJsonWriter.CreateOwnedStream(temp);
    try
      log := Pointer(fLogs);
      if asmndx < RESERVED_VTABLE_SLOTS then
        for i := 1 to fLogCount do
        begin
          log^.AddAsText(WR, aScope, SepChar);
          inc(log);
        end
      else
        for i := 1 to fLogCount do
        begin
          if log^.method^.ExecutionMethodIndex = asmndx then
            if (aParams = '') or
               (log^.Params = aParams) then
              log^.AddAsText(WR, aScope, SepChar);
          inc(log);
        end;
      WR.CancelLastChar(SepChar);
      WR.SetText(result);
    finally
      WR.Free;
    end;
  end;
end;

function TInterfaceStub.GetLogHash: cardinal;
begin
  result := Hash32(LogAsText);
end;

function TInterfaceStub.TryResolve(aInterface: PRttiInfo; out Obj): boolean;
begin
  if aInterface <> fInterface.fInterfaceTypeInfo then
    result := false
  else
  begin
    InternalGetInstance(Obj);
    result := true;
  end;
end;

function TInterfaceStub.Implements(aInterface: PRttiInfo): boolean;
begin
  result := fInterface.fInterfaceTypeInfo = aInterface;
end;


{ TInterfaceMock }

constructor TInterfaceMock.Create(aInterface: PRttiInfo; out aMockedInterface;
  aTestCase: TSynTestCase);
begin
  inherited Create(aInterface, aMockedInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(const aGuid: TGUID; out aMockedInterface;
  aTestCase: TSynTestCase);
begin
  inherited Create(aGuid, aMockedInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(const aInterfaceName: RawUtf8;
  out aMockedInterface; aTestCase: TSynTestCase);
begin
  inherited Create(aInterfaceName, aMockedInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(aInterface: PRttiInfo; aTestCase: TSynTestCase);
begin
  inherited Create(aInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(const aGuid: TGUID; aTestCase: TSynTestCase);
begin
  inherited Create(aGuid);
  fTestCase := aTestCase;
end;

function TInterfaceMock.InternalCheck(aValid, aExpectationFailed: boolean;
  const aErrorMsgFmt: RawUtf8; const aErrorMsgArgs: array of const): boolean;
begin
  if fTestCase = nil then
    result := inherited InternalCheck(
      aValid, aExpectationFailed, aErrorMsgFmt, aErrorMsgArgs)
  else
  begin
    result := true; // do not raise any exception at this stage for TInterfaceMock
    if aValid xor (imoMockFailsWillPassTestCase in Options) then
      fTestCase.Check(true)
    else
      fTestCase.Check(false, Utf8ToString(FormatUtf8(aErrorMsgFmt, aErrorMsgArgs)));
  end;
end;


{ TInterfaceMockSpy }

constructor TInterfaceMockSpy.Create(aFactory: TInterfaceFactory;
  const aInterfaceName: RawUtf8);
begin
  inherited Create(aFactory, aInterfaceName);
  include(fOptions, imoLogMethodCallsAndResults);
end;

procedure TInterfaceMockSpy.IntSetOptions(Options: TInterfaceStubOptions);
begin
  include(Options, imoLogMethodCallsAndResults);
  inherited IntSetOptions(Options);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName: RawUtf8;
  const aParams: array of const; aOperator: TInterfaceStubRuleOperator;
  aCount: cardinal);
begin
  Verify(aMethodName, JsonEncodeArrayOfConst(aParams, true), aOperator, aCount);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName: RawUtf8;
  const aParams: array of const; const aTrace: RawUtf8);
begin
  Verify(aMethodName, JsonEncodeArrayOfConst(aParams, true), aTrace);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName: RawUtf8;
  aOperator: TInterfaceStubRuleOperator; aCount: cardinal);
var
  m: integer;
begin
  m := fInterface.CheckMethodIndex(aMethodName);
  IntCheckCount(m, fRules[m].MethodPassCount, aOperator, aCount);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName, aParams: RawUtf8;
  aOperator: TInterfaceStubRuleOperator; aCount: cardinal);
var
  asmndx, i: PtrInt;
  c: cardinal;
begin
  asmndx := fInterface.CheckMethodIndex(aMethodName) + RESERVED_VTABLE_SLOTS;
  if aParams = '' then
    c := fRules[asmndx - RESERVED_VTABLE_SLOTS].MethodPassCount
  else
  begin
    c := 0;
    for i := 0 to fLogCount - 1 do
      with fLogs[i] do
        if (method.ExecutionMethodIndex = asmndx) and
           (Params = aParams) then
          inc(c);
  end;
  IntCheckCount(asmndx - RESERVED_VTABLE_SLOTS, c, aOperator, aCount);
end;

procedure TInterfaceMockSpy.Verify(const aTrace: RawUtf8; aScope: TInterfaceMockSpyCheck);
const
  VERIFY_SCOPE: array[TInterfaceMockSpyCheck] of TInterfaceStubLogLayouts = (
    [wName], [wName, wParams], [wName, wParams, wResults]);
begin
  InternalCheck(IntGetLogAsText(0, '', VERIFY_SCOPE[aScope], ',') = aTrace,
    true, 'Verify(''%'',%) failed', [aTrace, ToText(aScope)^]);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName, aParams, aTrace: RawUtf8);
var
  m: integer;
begin
  m := fInterface.CheckMethodIndex(aMethodName);
  InternalCheck(
    IntGetLogAsText(m + RESERVED_VTABLE_SLOTS, aParams, [wResults], ',') = aTrace,
    true, 'Verify(''%'',''%'',''%'') failed', [aMethodName, aParams, aTrace]);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName, aTrace: RawUtf8;
  aScope: TInterfaceMockSpyCheck);
const
  VERIFY_SCOPE: array[TInterfaceMockSpyCheck] of TInterfaceStubLogLayouts = (
    [], [wParams], [wParams, wResults]);
var
  m: integer;
begin
  m := fInterface.CheckMethodIndex(aMethodName);
  if aScope = chkName then
    raise EInterfaceStub.Create(self, fInterface.Methods[m],
      'Invalid scope for Verify()');
  InternalCheck(
    IntGetLogAsText(m + RESERVED_VTABLE_SLOTS, '', VERIFY_SCOPE[aScope], ',') = aTrace,
    true, 'Verify(''%'',''%'',%) failed', [aMethodName, aTrace, ToText(aScope)^]);
end;


function ToText(c: TInterfaceMockSpyCheck): PShortString;
begin
  result := GetEnumName(TypeInfo(TInterfaceMockSpyCheck), ord(c));
end;

function ToText(op: TInterfaceStubRuleOperator): PShortString;
begin
  result := GetEnumName(TypeInfo(TInterfaceStubRuleOperator), ord(op));
end;


{ ************ TInterfaceMethodExecute for Method Execution from JSON }

type
  // map TServiceRunningContext from mormot.rest.server.pas
  TPerThreadRunningContext = record
    Factory: TObject; // TServiceFactoryServer
    Request: TObject; // TRestServerUriContext
    RunningThread: TThread;
  end;
  PPerThreadRunningContext = ^TPerThreadRunningContext;

threadvar
  PerThreadRunningContext: TPerThreadRunningContext;

function PerThreadRunningContextAddress: pointer;
begin
  result := @PerThreadRunningContext;
end;

type
  TInterfacedObjectHooked = class(TInterfacedObject)
  public
    procedure InternalRelease;
  end;

  TBackgroundLauncherAction = (
    doCallMethod,
    doInstanceRelease,
    doThreadMethod);

  PBackgroundLauncher = ^TBackgroundLauncher;
  TBackgroundLauncher = record
    Context: PPerThreadRunningContext;
    case Action: TBackgroundLauncherAction of
      doCallMethod: (
        CallMethodArgs: pointer); // forward PCallMethodArgs
      doInstanceRelease: (
        Instance: TInterfacedObjectHooked);
      doThreadMethod: (
        ThreadMethod: TThreadMethod)
  end;

procedure TInterfacedObjectHooked.InternalRelease;
begin
  if self <> nil then
    IInterface(self)._Release; // call the release interface to dec(RefCount)
end;

procedure BackgroundExecuteProc(Call: pointer); forward;

procedure BackGroundExecute(var synch: TBackgroundLauncher;
  backgroundThread: TSynBackgroundThreadMethod);
var
  event: TThreadMethod;
begin
  synch.Context := @PerThreadRunningContext;
  TMethod(event).Code := @BackgroundExecuteProc;
  TMethod(event).Data := @synch;
  if backgroundThread = nil then
    if GetCurrentThreadID = MainThreadID then
      event
    else
      TThread.Synchronize(synch.Context^.RunningThread, event)
  else
    backgroundThread.RunAndWait(event);
end;

procedure BackgroundExecuteCallMethod(args: pointer;
  backgroundThread: TSynBackgroundThreadMethod);
var
  synch: TBackgroundLauncher;
begin
  synch.Action := doCallMethod;
  synch.CallMethodArgs := args;
  BackGroundExecute(synch, backgroundThread);
end;

procedure BackgroundExecuteThreadMethod(const method: TThreadMethod;
  backgroundThread: TSynBackgroundThreadMethod);
var
  synch: TBackgroundLauncher;
begin
  synch.Action := doThreadMethod;
  synch.ThreadMethod := method;
  BackGroundExecute(synch, backgroundThread);
end;

procedure BackgroundExecuteInstanceRelease(instance: TObject;
  backgroundThread: TSynBackgroundThreadMethod);
var
  synch: TBackgroundLauncher;
begin
  synch.Action := doInstanceRelease;
  if not instance.InheritsFrom(TInterfacedObject) then
    raise EInterfaceFactory.CreateUtf8('BackgroundExecuteInstanceRelease(%)', [instance]);
  synch.Instance := TInterfacedObjectHooked(instance);
  BackGroundExecute(synch, backgroundThread);
end;


{ TCallMethodArgs }

type
  PCallMethodArgs = ^TCallMethodArgs;
  {$ifdef FPC}
  {$push}
  {$packrecords 16} // stack is aligned on 16 bytes
  {$endif FPC}
  TCallMethodArgs = record
    StackSize: PtrInt;
    StackAddr, method: PtrInt;
    ParamRegs: array[PARAMREG_FIRST..PARAMREG_LAST] of PtrInt;
    {$ifdef HAS_FPREG}
    FPRegs: array[FPREG_FIRST..FPREG_LAST] of Double;
    {$endif HAS_FPREG}
    res64: Int64Rec;
    resKind: TInterfaceMethodValueType;
  end;
  {$ifdef FPC}
  {$pop}
  {$endif FPC}

// ARM/AARCH64 code below provided by ALF, greatly inspired by pascalscript
{$ifdef CPUARM}

procedure CallMethod(var Args: TCallMethodArgs); assembler; nostackframe;
label
  {$ifdef HAS_FPREG}
  asmcall_end, float_result,
  {$endif HAS_FPREG}
  stack_loop, load_regs;
asm
   //name  r#(normally, darwin can differ)
   //a1    0           argument 1 / integer result / scratch register
   //a2    1           argument 2 / scratch register
   //a3    2           argument 3 / scratch register
   //a4    3           argument 4 / scratch register
   //v1    4           register variable
   //v2    5           register variable
   //v3    6           register variable
   //v4    7           register variable
   //v5    8           register variable
   //sb    9           static base / register variable
   //sl    10          stack limit / stack chunk handle / reg. variable
   //fp    11          frame pointer
   //ip    12          scratch register / new-sb in inter-link-unit calls
   //sp    13          lower end of current stack frame
   //lr    14          link address / scratch register
   //pc    15          program counter

   // sometimes, the entry-point is not exact ... give some room for errors
   nop
   nop
   // prolog
   mov	 ip, sp // sp is the stack pointer ; ip is the Intra-Procedure-call scratch register
   stmfd sp!, {v1, v2, sb, sl, fp, ip, lr, pc}
   sub	 fp, ip, #4
   // make space on stack
   sub	 sp, sp, #MAX_EXECSTACK
   // align stack
   bic	 sp, sp, #7
   mov   v2, Args
   // copy (push) stack content (if any)
   ldr   a1, [v2,#TCallMethodArgs.StackSize]
   // if there is no stack content, do nothing
   cmp  a1, #0
   beq  load_regs
   // point a2 to bottom of stack.
   mov  a2, sp
   // load a3 with CallMethod stack address
   ldr  a3, [v2,#TCallMethodArgs.StackAddr]
stack_loop:
   // copy a3 to a4 and increment a3 (a3 = StackAddr)
   ldmia a3!, {a4}
   // copy a4 to a2 and increment a2 (a2 = StackPointer)
   stmia a2!, {a4}
   // decrement stacksize counter, with update of flags for loop
   subs  a1, a1, #1
   bne  stack_loop
load_regs:
   ldr   r0, [v2,#TCallMethodArgs.ParamRegs+REGR0*4-4]
   ldr   r1, [v2,#TCallMethodArgs.ParamRegs+REGR1*4-4]
   ldr   r2, [v2,#TCallMethodArgs.ParamRegs+REGR2*4-4]
   ldr   r3, [v2,#TCallMethodArgs.ParamRegs+REGR3*4-4]
   {$ifdef HAS_FPREG}
   vldr  d0, [v2,#TCallMethodArgs.FPRegs+REGD0*8-8]
   vldr  d1, [v2,#TCallMethodArgs.FPRegs+REGD1*8-8]
   vldr  d2, [v2,#TCallMethodArgs.FPRegs+REGD2*8-8]
   vldr  d3, [v2,#TCallMethodArgs.FPRegs+REGD3*8-8]
   vldr  d4, [v2,#TCallMethodArgs.FPRegs+REGD4*8-8]
   vldr  d5, [v2,#TCallMethodArgs.FPRegs+REGD5*8-8]
   vldr  d6, [v2,#TCallMethodArgs.FPRegs+REGD6*8-8]
   vldr  d7, [v2,#TCallMethodArgs.FPRegs+REGD7*8-8]
   {$endif HAS_FPREG}
   ldr   v1, [v2,#TCallMethodArgs.method]
   {$ifdef CPUARM_HAS_BLX}
   blx   v1
   {$else}
   mov lr, pc
   {$ifdef CPUARM_HAS_BX}
   bx  v1
   {$else}
   mov pc, v1
   {$endif CPUARM_HAS_BX}
   {$endif CPUARM_HAS_BLX}
   str   r0, [v2,#TCallMethodArgs.res64.Lo]
   str   r1, [v2,#TCallMethodArgs.res64.Hi]
{$ifdef HAS_FPREG}
   ldr   r2, [v2,#TCallMethodArgs.resKind]
   cmp   r2, imvDouble
   beq   float_result
   cmp   r2, imvDateTime
   beq   float_result
   cmp   r2, imvCurrency
   bne   asmcall_end
   // store double result in res64
float_result:
   vstr  d0, [v2,#TCallMethodArgs.res64]
asmcall_end:
{$endif HAS_FPREG}
   // epilog
   ldmea fp, {v1, v2, sb, sl, fp, sp, pc}
end;

{$endif CPUARM}

{$ifdef CPUAARCH64}

procedure CallMethod(var Args: TCallMethodArgs); assembler; nostackframe;
label stack_loop,load_regs,asmcall_end,float_result;
asm
   // inspired by pascal script
   // fp       x29
   // lr       x30
   // sp       sp

   // sometimes, the entry-point is not exact ... give some room for errors
   nop
   nop
   // prolog
   stp  x29, x30, [sp, #-16]!
   mov  x29, sp
   stp  x19, x19, [sp, #-16]!
   // make space on stack
   sub	sp, sp, #MAX_EXECSTACK
   //and  sp, sp, #-16   // Always align sp.
   mov  x19, Args
   // prepare to copy (push) stack content (if any)
   ldr  x2, [x19,#TCallMethodArgs.StackSize]
   // if there is no stack content, do nothing
   cmp	x2, #0
   b.eq	load_regs
   // point x3 to bottom of stack.
   mov	x3, sp
   // load x4 with CallMethod stack address
   ldr	x4, [x19,#TCallMethodArgs.StackAddr]
stack_loop:
   // load x5 and x6 with stack contents
   ldr  x5, [x4]
   ldr  x6, [x4,#8]
   // store contents at "real" stack and increment address counter x3
   stp	x5, x6, [x3], #16
   // with update of flags for loop
   // (mandatory: stacksize must be a multiple of 2 [16 bytes] !!)
   // inc stackaddr counter by 16 (2 registers are pushed every loop)
   add	x4, x4, #16
   // decrement stacksize counter by 2 (2 registers are pushed every loop),
   // with update of flags for loop
   subs	x2, x2, #2
   b.ne stack_loop
load_regs:
   ldr  x0, [x19,#TCallMethodArgs.ParamRegs+REGX0*8-8]
   ldr  x1, [x19,#TCallMethodArgs.ParamRegs+REGX1*8-8]
   ldr  x2, [x19,#TCallMethodArgs.ParamRegs+REGX2*8-8]
   ldr  x3, [x19,#TCallMethodArgs.ParamRegs+REGX3*8-8]
   ldr  x4, [x19,#TCallMethodArgs.ParamRegs+REGX4*8-8]
   ldr  x5, [x19,#TCallMethodArgs.ParamRegs+REGX5*8-8]
   ldr  x6, [x19,#TCallMethodArgs.ParamRegs+REGX6*8-8]
   ldr  x7, [x19,#TCallMethodArgs.ParamRegs+REGX7*8-8]
   ldr  d0, [x19,#TCallMethodArgs.FPRegs+REGD0*8-8]
   ldr  d1, [x19,#TCallMethodArgs.FPRegs+REGD1*8-8]
   ldr  d2, [x19,#TCallMethodArgs.FPRegs+REGD2*8-8]
   ldr  d3, [x19,#TCallMethodArgs.FPRegs+REGD3*8-8]
   ldr  d4, [x19,#TCallMethodArgs.FPRegs+REGD4*8-8]
   ldr  d5, [x19,#TCallMethodArgs.FPRegs+REGD5*8-8]
   ldr  d6, [x19,#TCallMethodArgs.FPRegs+REGD6*8-8]
   ldr  d7, [x19,#TCallMethodArgs.FPRegs+REGD7*8-8]
   // call TCallMethodArgs.method
   ldr  x15, [x19,#TCallMethodArgs.method]
   blr  x15
   // store normal result
   str  x0, [x19, #TCallMethodArgs.res64]
   ldr  x15, [x19, #TCallMethodArgs.resKind]
   cmp  x15, imvDouble
   b.eq float_result
   cmp  x15, imvDateTime
   b.eq float_result
   cmp  x15, imvCurrency
   b.ne asmcall_end
   // store double result in res64
float_result:
   str  d0, [x19,#TCallMethodArgs.res64]
asmcall_end:
   add  sp, sp, #MAX_EXECSTACK
   ldr  x19,[sp], #16
   ldp  x29,x30,[sp], #16
   ret
end;

{$endif CPUAARCH64}

{$ifdef CPUX64}

procedure CallMethod(var Args: TCallMethodArgs); assembler;
{$ifdef FPC} nostackframe;
asm
        push    rbp
        push    r12
        mov     rbp, rsp
        // simulate .params 60 ... size for 60 parameters
        lea     rsp, [rsp - MAX_EXECSTACK]
        // align stack to 16 bytes
        and     rsp, -16
{$else DELPHI} // ensure we use regular .params command for easier debugging
asm
        .params 64     // size for 64 parameters
        .pushnv r12   // generate prolog+epilog to save and restore non-volatile r12
{$endif FPC}
        // get Args
        mov     r12, Args
        // copy (push) stack content (if any)
        mov     rcx, [r12].TCallMethodArgs.StackSize
        mov     rdx, [r12].TCallMethodArgs.StackAddr
        jmp     @checkstack
@addstack:
        dec     ecx
        push    qword ptr [rdx]
        sub     rdx, 8
@checkstack:
        test    ecx, ecx
        jnz     @addstack
        // fill registers and call method
        {$ifdef OSPOSIX}
        // Linux/BSD System V AMD64 ABI
        mov     rdi, [r12 + TCallMethodArgs.ParamRegs + REGRDI * 8 - 8]
        mov     rsi, [r12 + TCallMethodArgs.ParamRegs + REGRSI * 8 - 8]
        mov     rdx, [r12 + TCallMethodArgs.ParamRegs + REGRDX * 8 - 8]
        mov     rcx, [r12 + TCallMethodArgs.ParamRegs + REGRCX * 8 - 8]
        mov     r8, [r12 + TCallMethodArgs.ParamRegs + REGR8 * 8 - 8]
        mov     r9, [r12 + TCallMethodArgs.ParamRegs + REGR9 * 8 - 8]
        movsd   xmm0, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM0 * 8 - 8]
        movsd   xmm1, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM1 * 8 - 8]
        movsd   xmm2, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM2 * 8 - 8]
        movsd   xmm3, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM3 * 8 - 8]
        movsd   xmm4, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM4 * 8 - 8]
        movsd   xmm5, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM5 * 8 - 8]
        movsd   xmm6, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM6 * 8 - 8]
        movsd   xmm7, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM7 * 8 - 8]
        call    [r12].TCallMethodArgs.method
        {$else}
        // Win64 ABI
        mov     rcx, [r12 + TCallMethodArgs.ParamRegs + REGRCX * 8 - 8]
        mov     rdx, [r12 + TCallMethodArgs.ParamRegs + REGRDX * 8 - 8]
        mov     r8, [r12 + TCallMethodArgs.ParamRegs + REGR8 * 8 - 8]
        mov     r9, [r12 + TCallMethodArgs.ParamRegs + REGR9 * 8 - 8]
        movsd   xmm0, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM0 * 8 - 8]
        movsd   xmm1, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM1 * 8 - 8]
        movsd   xmm2, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM2 * 8 - 8]
        movsd   xmm3, qword ptr [r12 + TCallMethodArgs.FPRegs + REGXMM3 * 8 - 8]
        sub     rsp, 8 * 4   // reserve shadow-space for RCX,RDX,R8,R9 registers
        call    [r12].TCallMethodArgs.method
        add     rsp, 8 * 4
        {$endif OSPOSIX}
        // retrieve result
        mov     [r12].TCallMethodArgs.res64, rax
        mov     cl, [r12].TCallMethodArgs.resKind
        cmp     cl, imvDouble
        je      @d
        cmp     cl, imvDateTime
        je      @d
        cmp     cl, imvCurrency
        jne     @e
@d:     movlpd  qword ptr [r12].TCallMethodArgs.res64, xmm0
        // movlpd to ignore upper 64-bit of 128-bit xmm0 reg
@e:     {$ifdef FPC}
        mov     rsp, rbp
        pop     r12
        pop     rbp
        {$endif FPC}
end;

{$endif CPUX64}

{$ifdef CPUX86}

procedure CallMethod(var Args: TCallMethodArgs);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    esi
        push    ebp
        push    eax // keep stack aligned on 16 bytes - required on DARWIN
        mov     ebp, esp
        mov     esi, Args
        // copy stack content (if any)
        mov     eax, [esi].TCallMethodArgs.StackSize
        mov     edx, dword ptr [esi].TCallMethodArgs.StackAddr
        add     edx, eax // pascal/register convention = left-to-right
        shr     eax, 2
        jz      @z
@n:     sub     edx, 4
        mov     ecx, [edx]
        push    ecx
        dec     eax
        jnz     @n
        // before a call instruction, esp should be divisible by 16:
        // mandatory on Darwin, and also on Linux i386 as stated by Florian in
        // https://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg38885.html
@z:     mov     eax, [esi + TCallMethodArgs.ParamRegs + REGEAX * 4 - 4]
        mov     edx, [esi + TCallMethodArgs.ParamRegs + REGEDX * 4 - 4]
        mov     ecx, [esi + TCallMethodArgs.ParamRegs + REGECX * 4 - 4]
        call    [esi].TCallMethodArgs.method
        // retrieve result
        mov     cl, [esi].TCallMethodArgs.resKind
        cmp     cl, imvDouble
        je      @d
        cmp     cl, imvDateTime
        je      @d
        cmp     cl, imvCurrency
        jne     @i
        fistp   qword [esi].TCallMethodArgs.res64
        jmp     @e
@d:     fstp    qword [esi].TCallMethodArgs.res64
        jmp     @e
@i:     mov     [esi].TCallMethodArgs.res64.Lo, eax
        mov     [esi].TCallMethodArgs.res64.Hi, edx
@e:     mov     esp, ebp
        pop     eax
        pop     ebp
        pop     esi
end;

{$endif CPUX86}

procedure BackgroundExecuteProc(Call: pointer);
var
  synch: PBackgroundLauncher absolute Call;
  threadContext: PPerThreadRunningContext;
  backup: TPerThreadRunningContext;
begin
  threadContext := @PerThreadRunningContext; // faster to use a pointer below
  backup := threadContext^;
  threadContext^.Factory := synch^.Context^.Factory;
  threadContext^.Request := synch^.Context^.Request;
  try
    case synch^.Action of
      doCallMethod:
        CallMethod(PCallMethodArgs(synch^.CallMethodArgs)^);
      doInstanceRelease:
        synch^.Instance.InternalRelease;
      doThreadMethod:
        synch^.ThreadMethod;
    end;
  finally
    threadContext^ := backup;
  end;
end;


{ TInterfaceMethodExecuteRaw }

constructor TInterfaceMethodExecuteRaw.Create(aMethod: PInterfaceMethod);
var
  a: PtrInt;
begin
  with aMethod^ do
  begin
    if ArgsUsedCount[imvv64] > 0 then
      SetLength(fInt64s, ArgsUsedCount[imvv64]);
    if ArgsUsedCount[imvvObject] > 0 then
      SetLength(fObjects, ArgsUsedCount[imvvObject]);
    if ArgsUsedCount[imvvInterface] > 0 then
      SetLength(fInterfaces, ArgsUsedCount[imvvInterface]);
    if ArgsUsedCount[imvvRecord] > 0 then
      SetLength(fRecords, ArgsUsedCount[imvvRecord]);
    if ArgsUsedCount[imvvDynArray] > 0 then
      SetLength(fDynArrays, ArgsUsedCount[imvvDynArray]);
    SetLength(fValues, length(Args));
    for a := ArgsManagedFirst to ArgsManagedLast do
    with Args[a] do
      case ValueType of
        imvDynArray:
          with fDynArrays[IndexVar] do
            Wrapper.InitRtti(ArgRtti, Value);
        imvRecord:
          SetLength(fRecords[IndexVar], ArgRtti.Size);
        imvVariant:
          SetLength(fRecords[IndexVar], SizeOf(Variant));
      end;
  end;
  fMethod := aMethod;
end;

procedure TInterfaceMethodExecuteRaw.AddInterceptor(
  const Hook: TOnInterfaceMethodExecute);
begin
  MultiEventAdd(fOnExecute, TMethod(Hook));
end;

procedure TInterfaceMethodExecuteRaw.AddInterceptors(
  const Hook: TInterfaceMethodExecuteEventDynArray);
begin
  MultiEventMerge(fOnExecute, Hook);
end;

procedure TInterfaceMethodExecuteRaw.BeforeExecute;
var
  a: PtrInt;
  V: PPointer;
begin
  fExecutedInstancesFailed := nil;
  with fMethod^ do
  begin
    // recreate managed arrays for each call
    if ArgsUsedCount[imvvRawUtf8] > 0 then
      SetLength(fRawUtf8s, ArgsUsedCount[imvvRawUtf8]);
    if ArgsUsedCount[imvvString] > 0 then
      SetLength(fStrings, ArgsUsedCount[imvvString]);
    if ArgsUsedCount[imvvWideString] > 0 then
      SetLength(fWideStrings, ArgsUsedCount[imvvWideString]);
    if fAlreadyExecuted then
    begin
      // reset unmanaged values for each call
      if ArgsUsedCount[imvvObject] > 0 then
        FillCharFast(fObjects,ArgsUsedCount[imvvObject] * SizeOf(TObject), 0);
      if ArgsUsedCount[imvv64] > 0 then
        FillCharFast(fInt64s, ArgsUsedCount[imvv64] * SizeOf(Int64), 0);
      if ArgsUsedCount[imvvInterface] > 0 then
        FillCharFast(fInterfaces, ArgsUsedCount[imvvInterface] * SizeOf(pointer), 0);
    end;
    V := @fValues[1];
    for a := 1 to high(Args) do
    with Args[a] do
    begin
      case ValueVar of
        imvv64:
          V^ := @fInt64s[IndexVar];
        imvvRawUtf8:
          V^ := @fRawUtf8s[IndexVar];
        imvvString:
          V^ := @fStrings[IndexVar];
        imvvWideString:
          V^ := @fWideStrings[IndexVar];
        imvvObject:
          begin
            V^ := @fObjects[IndexVar];
            PPointer(V^)^ := ArgRtti.ClassNewInstance;
          end;
        imvvInterface:
          V^ := @fInterfaces[IndexVar];
        imvvRecord:
          begin
            V^ := pointer(fRecords[IndexVar]);
            if fAlreadyExecuted then
              FillCharFast(V^^, ArgRtti.Size, 0);
          end;
        imvvDynArray:
          V^ := @fDynArrays[IndexVar].Value;
      else
        raise EInterfaceFactory.CreateUtf8('I%.%:% ValueType=%',
          [InterfaceDotMethodName, ParamName^, ArgTypeName^, ord(ValueType)]);
      end;
      inc(V);
    end;
    if optInterceptInputOutput in fOptions then
    begin
      Input.InitFast(ArgsInputValuesCount, dvObject);
      Output.InitFast(ArgsOutputValuesCount, dvObject);
    end;
  end;
  fAlreadyExecuted := true;
end;

procedure TInterfaceMethodExecuteRaw.RawExecute(const Instances: PPointerArray;
  InstancesLast: integer);
var
  Value: pointer;
  a, e, i: PtrInt;
  call: TCallMethodArgs;
  Stack: packed array[0 .. MAX_EXECSTACK - 1] of byte;
begin
  FillCharFast(call, SizeOf(call), 0);
  with fMethod^ do
  begin
    // create the stack and register content from fValues[]
    {$ifdef CPUX86}
    call.StackAddr := PtrInt(@Stack[0]);
    call.StackSize := ArgsSizeInStack;
    {$ifdef OSPOSIX} // ensure always aligned by 16 bytes on POSIX
    while call.StackSize and 15 <> 0 do
      inc(call.StackSize,POINTERBYTES); // needed for Darwin and Linux i386
    {$endif OSPOSIX}
    {$else}
    {$ifdef CPUINTEL}
    call.StackSize := ArgsSizeInStack shr 3;
    // ensure stack aligned on 16 bytes (paranoid)
    if call.StackSize and 1 <> 0 then
      inc(call.StackSize);
    // stack is filled reversed (RTL)
    call.StackAddr := PtrInt(@Stack[call.StackSize * 8 - 8]);
    {$else}
    // stack is filled normally (LTR)
    call.StackAddr := PtrInt(@Stack[0]);
    {$ifdef CPUAARCH64}
    call.StackSize := ArgsSizeInStack shr 3;
    // ensure stack aligned on 16 bytes (mandatory: needed for correct low level asm)
    if call.StackSize and 1 <> 0 then
      inc(call.StackSize);
    {$else}
    call.StackSize := ArgsSizeInStack shr 2;
    {$endif CPUAARCH64}
    {$endif CPUINTEL}
    {$endif CPUX86}
    for a := 1 to length(Args)-1 do
    with Args[a] do
    begin
      Value := fValues[a];
      if (ValueDirection <> imdConst) or
         (ValueType in [imvRecord, imvVariant]) then
       begin
        // pass by reference
        if (RegisterIdent = 0) and
           (FPRegisterIdent = 0) and
           (SizeInStack > 0) then
          MoveFast(Value, Stack[InStackOffset], SizeInStack)
        else
        begin
          if RegisterIdent > 0 then
            call.ParamRegs[RegisterIdent] := PtrInt(Value);
          if FPRegisterIdent > 0 then
            raise EInterfaceFactory.CreateUtf8('Unexpected % FPReg=%',
              [ParamName^, FPRegisterIdent]); // should never happen
        end;
      end
      else
      begin
        // pass by value
        if (RegisterIdent = 0) and
           (FPRegisterIdent = 0) and
           (SizeInStack > 0) then
          MoveFast(Value^, Stack[InStackOffset], SizeInStack)
        else
        begin
          if RegisterIdent > 0 then
             begin
            call.ParamRegs[RegisterIdent] := PPtrInt(Value)^;
            {$ifdef CPUARM}
            // for e.g. INT64 on 32-bit ARM systems; these are also passed in registers
            if SizeInStack > POINTERBYTES then
              call.ParamRegs[RegisterIdent + 1] := PPtrInt(Value + POINTERBYTES)^;
            {$endif CPUARM}
          end;
          {$ifdef HAS_FPREG}
          if FPRegisterIdent > 0 then
            call.FPRegs[FPRegisterIdent] := unaligned(PDouble(Value)^);
          if (RegisterIdent > 0) and
             (FPRegisterIdent > 0) then
            raise EInterfaceFactory.CreateUtf8('Unexpected % reg=% FP=%',
              [ParamName^, RegisterIdent, FPRegisterIdent]); // should never happen
          {$endif HAS_FPREG}
        end;
      end;
    end;
    // execute the method
    for i := 0 to InstancesLast do
    begin
      // handle method execution smsBefore interception
      fCurrentStep := smsBefore;
      if fOnExecute <> nil then
      begin
        if (Input.Count = 0) and
           (optInterceptInputOutput in Options) then
          ArgsStackAsDocVariant(fValues, fInput, true);
        for e := 0 to length(fOnExecute) - 1 do
        try
          fOnExecute[e](self, smsBefore);
        except // ignore any exception during interception
        end;
      end;
      // prepare the low-level call context for the asm stub
      call.ParamRegs[PARAMREG_FIRST] := PtrInt(Instances[i]); // pass self
      call.method := PPtrIntArray(PPointer(Instances[i])^)^[ExecutionMethodIndex];
      if ArgsResultIndex >= 0 then
        call.resKind := Args[ArgsResultIndex].ValueType
      else
        call.resKind := imvNone;
      // launch the asm stub in the expected execution context
      try
        if (optExecInMainThread in Options) and
           (GetCurrentThreadID <> MainThreadID) then
          BackgroundExecuteCallMethod(@call, nil) else
        if optExecInPerInterfaceThread in Options then
          if Assigned(BackgroundExecutionThread) then
            BackgroundExecuteCallMethod(@call, BackgroundExecutionThread)
          else
            raise EInterfaceFactory.Create('optExecInPerInterfaceThread' +
              ' with BackgroundExecutionThread=nil')
        else
          CallMethod(call); // actual asm stub
        if (ArgsResultIndex >= 0) and
           (Args[ArgsResultIndex].ValueVar = imvv64) then
          PInt64Rec(fValues[ArgsResultIndex])^ := call.res64;
        // handle method execution smsAfter interception
        fCurrentStep := smsAfter;
        if fOnExecute <> nil then
        begin
          if (Output.Count = 0) and
             (optInterceptInputOutput in Options) then
            ArgsStackAsDocVariant(fValues, fOutput, false);
          for e := 0 to length(fOnExecute) - 1 do
          try
            fOnExecute[e](self, smsAfter);
          except // ignore any exception during interception
          end;
        end;
      except
        // intercept any Exception as smsError
        on Exc: Exception do
        begin
          fCurrentStep := smsError;
          if fOnExecute <> nil then
          begin
            fLastException := Exc;
            for e := 0 to length(fOnExecute) - 1 do
            try
              fOnExecute[e](self, smsError);
            except // ignore any exception during interception
            end;
            fLastException := nil;
          end;
          if (InstancesLast = 0) and
             not (optIgnoreException in Options) then
            raise; // single caller expects exception to be propagated
          // multiple Instances[] notifies with fExecutedInstancesFailed[]
          if fExecutedInstancesFailed = nil then
            SetLength(fExecutedInstancesFailed, InstancesLast + 1);
          fExecutedInstancesFailed[i] := ObjectToJsonDebug(Exc);
        end;
      end;
    end;
  end;
end;

procedure TInterfaceMethodExecuteRaw.AfterExecute;
var
  i, a: PtrInt;
begin
  // finalize managed arrays for each call
  Finalize(fRawUtf8s);
  Finalize(fStrings);
  Finalize(fWideStrings);
  with fMethod^ do
    if ArgsManagedFirst >= 0 then
    begin
      for i := 0 to ArgsUsedCount[imvvObject] - 1 do
        fObjects[i].Free;
      for i := 0 to ArgsUsedCount[imvvInterface] - 1 do
        IUnknown(fInterfaces[i]) := nil;
      for i := 0 to ArgsUsedCount[imvvDynArray] - 1 do
        // will handle T*ObjArray, and set Value^=nil
        fDynArrays[i].Wrapper.Clear;
      if fRecords <> nil then
      begin
        i := 0;
        for a := ArgsManagedFirst to ArgsManagedLast do
          with Args[a] do
          case ValueType of
            imvRecord:
              begin
                FastRecordClear(pointer(fRecords[i]), ArgRtti.Info);
                inc(i);
              end;
            imvVariant:
              begin
                VarClear(PVariant(fRecords[i])^); // fast, even for simple types
                inc(i);
              end;
          end;
      end;
    end;
end;


{ TInterfaceMethodExecute }

destructor TInterfaceMethodExecute.Destroy;
begin
  fTempTextWriter.Free;
  inherited Destroy;
end;

function TInterfaceMethodExecute.TempTextWriter: TJsonWriter;
begin
  if fTempTextWriter = nil then
  begin
    fTempTextWriter := TJsonWriter.CreateOwnedStream;
    fTempTextWriter.CustomOptions := fTempTextWriter.CustomOptions +
      [twoForceJsonExtended, twoIgnoreDefaultInRecord]; // shorter
  end;
  result := fTempTextWriter;
end;

function TInterfaceMethodExecute.ExecuteJsonCallback(Instance: pointer;
  const params: RawUtf8; output: PRawUtf8): boolean;
var
  fake: TInterfacedObjectFake;
  WR: TJsonWriter;
  n: integer;
  tmp: TSynTempBuffer;
begin
  result := false;
  if Instance = nil then
    exit;
  // efficient detection of a TInterfacedObjectFake to bypass JSON marshalling
  if PCardinal(PPointer(PPointer(Instance)^)^)^ =
       PCardinal(@TInterfacedObjectFake.FakeQueryInterface)^ then
  begin
    fake := TInterfacedObjectFake(Instance).SelfFromInterface as TInterfacedObjectFake;
    if Assigned(fake.fInvoke) then
    begin
      // call SOA/fake interface? -> bypass all JSON marshalling
      if (output = nil) and
         (fMethod^.ArgsOutputValuesCount > 0) then
        exit; // ensure a function has a TOnAsyncRedirectResult callback
      result := fake.fInvoke(fMethod^, params, output, nil, nil, nil);
      exit;
    end;
  end;
  n := length(params);
  tmp.Init(n + 2);
  try
    PAnsiChar(tmp.buf)[0] := '[';
    MoveFast(pointer(params)^, PAnsiChar(tmp.buf)[1], n);
    PWord(PAnsiChar(tmp.buf) + n + 1)^ := ord(']'); // ']'#0
    if output <> nil then
    begin
      WR := TempTextWriter;
      WR.CancelAll;
    end
    else if fMethod^.ArgsOutputValuesCount > 0 then
      exit
    else // ensure a function has a TOnAsyncRedirectResult callback
      WR := nil;
    result := ExecuteJson([Instance], tmp.buf, WR);
    if WR <> nil then
      WR.SetText(output^);
  finally
    tmp.Done;
  end;
end;

function TInterfaceMethodExecute.ExecuteJsonFake(
  Instance: pointer; params: PUtf8Char): boolean;
var
  tmp: RawUtf8;
  len: integer;
begin
  result := false;
  if not Assigned(TInterfacedObjectFake(Instance).fInvoke) then
    exit;
  if params <> nil then
  begin
    if params^ <> '[' then
      exit;
    inc(params);
    len := StrLen(params);
    if params[len - 1] = ']' then
      dec(len);
    FastSetString(tmp, params, len);
  end;
  result := TInterfacedObjectFake(Instance).fInvoke(
    fMethod^, tmp, nil, nil, nil, nil);
end;

function TInterfaceMethodExecute.ExecuteJson(const Instances: array of pointer;
  Par: PUtf8Char; Res: TJsonWriter; Error: PShortString; ResAsJsonObject: boolean): boolean;
var
  a, a1: integer;
  Val, Name: PUtf8Char;
  NameLen: integer;
  EndOfObject: AnsiChar;
  opt: array[{smdVar=}boolean] of TTextWriterWriteObjectOptions;
  ParObjValuesUsed: boolean;
  ParObjValues: array[0 .. MAX_METHOD_ARGS - 1] of PUtf8Char;
begin
  // prepare all fValues[] pointers for the input/output arguments
  result := false;
  BeforeExecute;
  with fMethod^ do
  try
    // locate input arguments from JSON array or object
    ParObjValuesUsed := false;
    if (ArgsInputValuesCount <> 0) and
       (Par <> nil) then
    begin
      if Par^ in [#1..' '] then
        repeat
          inc(Par);
        until not (Par^ in [#1..' ']);
      case Par^ of
        '[':
          // input arguments as a JSON array , e.g. '[1,2,"three"]' (default)
          inc(Par);
        '{':
          begin
            // retrieve arguments values from JSON object -> field name lookup
            repeat
              inc(Par);
            until not (Par^ in [#1..' ']);
            if Par <> '}' then
            begin
              ParObjValuesUsed := true;
              FillCharFast(ParObjValues, (ArgsInLast + 1) * SizeOf(pointer), 0);
              a1 := ArgsInFirst;
              repeat
                Name := GetJsonPropName(Par, @NameLen);
                if Name = nil then
                  exit; // invalid JSON object in input
                Val := Par;
                Par := GotoNextJsonItem(Par, 1, @EndOfObject);
                for a := a1 to ArgsInLast do
                with Args[a] do
                  if ValueDirection <> imdOut then
                    if IdemPropName(ParamName^,Name,NameLen) then
                    begin
                      ParObjValues[a] := Val; // fast redirection, without allocation
                      if a = a1 then
                        inc(a1); // enable optimistic O(1) search for in-order input
                      break;
                    end;
              until (Par = nil) or
                    (EndOfObject = '}');
            end;
            Par := nil;
          end;
      else
        if PInteger(Par)^ = NULL_LOW then
          Par := nil
        else
          exit; // only support JSON array or JSON object as input
      end;
    end;
    // parse and decode JSON input const/var arguments (if any) into fValues[]
    if (Par = nil) and
       not ParObjValuesUsed then
    begin
      if (ArgsInputValuesCount > 0) and
         (optErrorOnMissingParam in Options) then
        exit; // paranoid setting
    end
    else
      for a := ArgsInFirst to ArgsInLast do
      with Args[a] do
      if ValueDirection <> imdOut then // imdResult is excluded from ArgsInLast
      begin
        if ParObjValuesUsed then
          if ParObjValues[a] = nil then // missing parameter in input JSON
            if optErrorOnMissingParam in Options then
              exit
            else
              // ignore and leave void value by default
              continue
          else
            // value to be retrieved from JSON object
            Par := ParObjValues[a]
        else if Par = nil then
          break; // premature end of ..] (ParObjValuesUsed=false)
        case ValueType of
          imvInterface:
            if Assigned(OnCallback) then
              // retrieve TRestServerUriContext.ExecuteCallback fake interface
              // via TServiceContainerServer.GetFakeCallback
              OnCallback(Par, ArgRtti, fInterfaces[IndexVar])
            else
              raise EInterfaceFactory.CreateUtf8('OnCallback=nil for %(%: %)',
                [InterfaceDotMethodName, ParamName^, ArgTypeName^]);
          imvDynArray:
            begin
              Par := fDynArrays[IndexVar].Wrapper.LoadFromJson(Par);
              if Par = nil then
                exit;
              IgnoreComma(Par);
            end;
        else
          if not FromJson(InterfaceDotMethodName, Par, fValues[a], Error,
             JSON_OPTIONS[optVariantCopiedByReference in Options]) then
            exit;
        end;
      end;
    // execute the method, using prepared input/output fValues[]
    RawExecute(@Instances[0], high(Instances));
    // send back any var/out output arguments as JSON
    if Res <> nil then
    begin
      // handle custom content (not JSON array/object answer)
      if ArgsResultIsServiceCustomAnswer then
        with PServiceCustomAnswer(fValues[ArgsResultIndex])^ do
        begin
          if Header = '' then
            // set to 'Content-Type: application/json; charset=UTF-8' by default
            fServiceCustomAnswerHead := JSON_CONTENT_TYPE_HEADER_VAR
          else
            // implementation could override the Header content
            fServiceCustomAnswerHead := Header;
          Res.ForceContent(Content);
          if Status = 0 then
            // Values[]=@Records[] is filled with 0 by default
            fServiceCustomAnswerStatus := HTTP_SUCCESS
          else
            fServiceCustomAnswerStatus := Status;
          result := true;
          exit;
        end;
      // write the '{"result":[...' array or object
      opt[{smdVar=}false] := DEFAULT_WRITEOPTIONS[optDontStoreVoidJson in Options];
      opt[{smdVar=}true] := []; // let var params override void/default values
      for a := ArgsOutFirst to ArgsOutLast do
        with Args[a] do
        if ValueDirection <> imdConst then
        begin
          if ResAsJsonObject then
            Res.AddPropName(ParamName^);
          case ValueType of
            imvDynArray:
                if vIsObjArray in ValueKindAsm then
                  Res.AddObjArrayJson(fValues[a]^, opt[ValueDirection = imdVar])
                else
                  Res.AddDynArrayJson(fDynArrays[IndexVar].Wrapper);
          else
            AddJson(Res, fValues[a], opt[ValueDirection = imdVar]);
          end;
          Res.AddComma;
        end;
      Res.CancelLastComma;
    end;
    result := true;
  finally
    // release any managed input/output parameters from fValues[]
    AfterExecute;
  end;
end;


{ TInterfacedObjectFakeCallback }

function TInterfacedObjectFakeCallback.FakeInvoke(
  const aMethod: TInterfaceMethod; const aParams: RawUtf8;
  aResult, aErrorMsg: PRawUtf8; aFakeID: PInterfacedObjectFakeID;
  aServiceCustomAnswer: PServiceCustomAnswer): boolean;
begin
  if fLogClass <> nil then
    fLogClass.Add.Log(sllTrace, 'FakeInvoke %(%)',
      [aMethod.InterfaceDotMethodName, aParams], self);
  if aMethod.ArgsOutputValuesCount > 0 then
  begin
    if aErrorMsg <> nil then
      FormatUtf8('%.FakeInvoke [%]: % has out parameters',
        [self, fName, aMethod.InterfaceDotMethodName], aErrorMsg^);
    result := false;
  end
  else
    result := true;
end;


{ ************ SetWeak and SetWeakZero Weak Interface Reference }

procedure SetWeak(aInterfaceField: PInterface; const aValue: IInterface);
begin
  PPointer(aInterfaceField)^ := Pointer(aValue);
end;


{ TSetWeakZero maintains a thread-safe per-instance reference list }

type
  TSetWeakZero = class(TSynDictionary) // TClass / TPointerDynArray map
  protected
    fHookedFreeInstance: PtrUInt;
  public
    constructor Create(aClass: TClass); reintroduce;
  end;

type
  TFreeInstanceMethod = procedure(self: TObject);

procedure HookedFreeInstance(self: TObject);
var
  inst: TSetWeakZero;
  i: PtrInt;
  fields: PPointerArray; // holds a TPointerDynArray but avoid try..finally
begin
  inst := Rtti.Find(PClass(self)^).GetPrivateSlot(TSetWeakZero);
  fields := nil;
  if inst.FindAndExtract(self, fields) and
     (fields <> nil) then
  begin
    // zeroing of weak references in object fields
    for i := 0 to PDALen(PAnsiChar(fields) - _DALEN)^ + (_DAOFF - 1) do
      PPointer(fields[i])^ := nil;
    FastDynArrayClear(@fields, nil);
  end;
  TFreeInstanceMethod(inst.fHookedFreeInstance)(self); // CleanupInstance + FreeMem()
end;

constructor TSetWeakZero.Create(aClass: TClass);
var
  P: PPtrUInt;
begin
  // key = instance TObject, value = dynarray field(s) to be zeroed
  inherited Create(TypeInfo(TPointerDynArray), TypeInfo(TPointerDynArrayDynArray));
  P := pointer(PAnsiChar(aClass) + vmtFreeInstance);
  if P^ = PtrUInt(@HookedFreeInstance) then
    // hook once - Create may be done twice in GetWeakZero() for SetPrivateSlot
    exit;
  fHookedFreeInstance := P^;
  PatchCodePtrUInt(P, PtrUInt(@HookedFreeInstance));
end;

function GetWeakZero(aClass: TClass; CreateIfNonExisting: boolean): TSetWeakZero;
var
  rc: TRttiCustom;
begin
  rc := Rtti.RegisterClass(aClass);
  result := rc.GetPrivateSlot(TSetWeakZero);
  if (result = nil) and
     CreateIfNonExisting then
    result := rc.SetPrivateSlot(TSetWeakZero.Create(aClass));
end;

procedure SetWeakZero(aObject: TObject; aObjectInterfaceField: PInterface;
  const aValue: IInterface);
var
  c: TObject;
  o, v: TSetWeakZero;
begin
  if (aObjectInterfaceField = nil) or
     (aObject = nil) or
     (aObjectInterfaceField^ = aValue) then
    exit;
  o := GetWeakZero(PClass(aObject)^, {createifneeded=}false);
  if aObjectInterfaceField^ <> nil then
  begin
    if (aValue = nil) and
       (o <> nil) then
      o.DeleteInArray(aObject, aObjectInterfaceField);
    c := ObjectFromInterface(aObjectInterfaceField^);
    v := GetWeakZero(PClass(c)^, {createifneeded=}false);
    if v <> nil then
      v.DeleteInArray(c, aObjectInterfaceField);
    PPointer(aObjectInterfaceField)^ := nil;
    if aValue = nil then
      exit;
  end;
  if o = nil then
    o := GetWeakZero(PClass(aObject)^, {createifneeded=}true);
  o.AddInArrayForced(aObject, aObjectInterfaceField);
  if aValue <> nil then
  begin
    c := ObjectFromInterface(aValue);
    v := GetWeakZero(PClass(c)^, {createifneeded=}true);
    v.AddInArrayForced(c, aObjectInterfaceField);
  end;
  PPointer(aObjectInterfaceField)^ := pointer(aValue);
end;


procedure InitializeUnit;
begin
  {$ifdef CPUARM}
  ArmFakeStubAddr := @TInterfacedObjectFake.ArmFakeStub;
  {$endif CPUARM}
  GlobalInterfaceResolver := TInterfaceResolverList.Create;
  RegisterGlobalShutdownRelease(GlobalInterfaceResolver);
  GlobalInterfaceResolver.Add(TypeInfo(IAutoLocker), TAutoLocker);
  GlobalInterfaceResolver.Add(TypeInfo(ILockedDocVariant), TLockedDocVariant);
end;


initialization
  InitializeUnit;


end.

