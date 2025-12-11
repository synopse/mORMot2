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
    - TInterfaceStub for Dependency Stubbing/Mocking
    - TInterfacedObjectFake with JITted Methods Execution
    - TInterfaceMethodExecute for Method Execution from JSON
    - SetWeak and SetWeakZero Weak Interface Reference
    - Code/Documentation Generation Logic Extraction from RTTI
    - Documentation Extraction from Source Code Comments

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  {$ifdef ISDELPHI}
  typinfo, // for proper Delphi inlining
  {$endif ISDELPHI}
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.json,
  mormot.core.threads,
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
  // - vPassedByReference is included if the parameter is passed as reference
  // (i.e. defined as var/out, or is a record or a reference-counted type result)
  // - vIsQword is set for ValueType=imvInt64 over a QWord unsigned 64-bit value
  // - vIsDynArrayString is set for ValueType=imvDynArray of string values
  // - vIsInterfaceJson is set for an interface with custom JSON serializers
  // - vIsOnStack is set when the Value is to be located on stack
  // - vIsHFA is set for Homogeneous Floating-point Aggregate records (at most
  // four continuous floating point members on SYSVABI) - pointless on x86
  TInterfaceMethodValueAsm = set of (
    vPassedByReference,
    vIsQword,
    vIsDynArrayString,
    vIsInterfaceJson,
    vIsOnStack,
    vIsHFA);

  /// a pointer to an interface-based service provider method description
  // - since TInterfaceFactory instances are shared in a global list, we
  // can safely use such pointers in our code to refer to a particular method
  PInterfaceMethod = ^TInterfaceMethod;

  /// describe a service provider method argument
  {$ifdef USERECORDWITHMETHODS}
  TInterfaceMethodArgument = record
  {$else}
  TInterfaceMethodArgument = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the argument name, as declared in Object Pascal
    // - see also TInterfaceMethod.ArgsName[] array if you need a RawUtf8
    ParamName: PShortString;
    /// the type name, as declared in Object Pascal
    ArgTypeName: PShortString;
    /// the low-level RTTI information of this argument
    // - use ArgRtti.Info to retrieve the TypeInfo() of this argument
    ArgRtti: TRttiJson;
    /// we do not handle all kind of Object Pascal variables
    ValueType: TInterfaceMethodValueType;
    /// the variable direction as defined at code level
    // - you may rather use high-level IsInput/IsOutput inlined methods
    ValueDirection: TInterfaceMethodValueDirection;
    /// how the variable may be stored
    ValueVar: TInterfaceMethodValueVar;
    /// how the variable is to be passed at asm level
    ValueKindAsm: TInterfaceMethodValueAsm;
    /// specify if the argument is passed as register
    // - contains 0 if parameter is not a register
    // - i386: 1 for EAX, 2 for EDX and 3 for ECX registers
    // - x86_64: 1=RCX/RDI 2=RDX/RSI 3=R8/RDX 4=R9/RCX, with stack backing store
    // - ARM: 1=R0 2=R1 3=R2 4=R3, with a backing store on the stack
    // - AARCH64: 1=X0 2=X1, ..., 8=X7, with a backing store on the stack
    RegisterIdent: byte;
    /// specify if a floating-point argument is passed as register
    // - i386/x87: contains always 0 - the HAS_FPREG conditional is not defined
    // - x86_64: 1 for XMM0, 2 for XMM1, , ..., 8 for XMM7
    // - ARMHF: 1 for D0, 2 for D1, ..., 8 for D7
    // - AARCH64: 1 for V0, 2 for V1, ..., 8 for V7
    FPRegisterIdent: byte;
    /// index of the associated variable in the local array[ArgsUsedCount[]]
    IndexVar: byte;
    /// size (in bytes) of this argument on the stack
    SizeInStack: byte;
    /// byte offset in the CPU stack of this argument (16-bit)
    // - may be -1 if pure register parameter with no backup on stack (x86)
    InStackOffset: SmallInt;
    /// how TInterfaceMethodExecuteRaw.RawExecute should handle this value
    RawExecute: (reValReg, reValRegs, reValStack, reRefReg, reRefStack,
                 reValFpReg, reValFpRegs, reNone);
    /// 64-bit aligned position in TInterfaceMethod.ArgsSizeAsValue memory
    OffsetAsValue: cardinal;
    /// true if is a const/var input argument
    function IsInput: boolean;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// true if is a var/out/result output argument
    function IsOutput: boolean;
      {$ifdef HASSAFEINLINE} inline; {$endif}
    /// serialize the argument into the TServiceContainer.Contract JSON format
    // - non standard types (e.g. class, enumerate, dynamic array or record)
    // are identified by their type identifier - so contract does not extend
    // up to the content of such high-level structures
    procedure SerializeToContract(WR: TJsonWriter);
    /// unserialize a JSON value into this argument
    function SetFromJson(var Ctxt: TJsonParserContext; Method: PInterfaceMethod;
      V: pointer; Error: PShortString): boolean;
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
    /// normalize a value containing one input or output argument
    // - sets and enumerates will be translated into text (also in embedded
    // objects and T*ObjArray), and record/class/arrays into TDocVariantData
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
  TOnInterfaceMethodExecuteCallback = procedure(var Ctxt: TJsonParserContext;
    ParamInterfaceInfo: TRttiJson; out Obj) of object;

  /// how TInterfaceMethod.ArgsValuesAsDocVariant will return the generated document
  // - will return either a dvObject or dvArray TDocVariantData, depending on
  // the expected returned document layout
  // - returned content could be "normalized" (for any set or enumerate) if
  // Kind is pdvObjectFixed
  TInterfaceMethodParamsDocVariantKind = (
    pdvArray,
    pdvObject,
    pdvObjectFixed);

  /// refine one TInterfaceMethod processing
  // - imfIsInherited if the method is inherited from another parent interface
  // - imfResultIsServiceCustomAnswer is set if the result is a
  // TServiceCustomAnswer record and requires specific raw HTTP-level process
  // - imfResultIsServiceCustomStatus is set if the result is a
  // TServiceCustomStatus integer, i.e. a custom HTTP status
  // - imfInputIsOctetStream is set if there is a single input parameter as
  // RawByteString/RawBlob so that TRestRoutingRest.ExecuteSoaByInterface will
  // identify binary body input with mime-type 'application/octet-stream'
  TInterfaceMethodFlag = (
    imfIsInherited,
    imfResultIsServiceCustomAnswer,
    imfResultIsServiceCustomStatus,
    imfInputIsOctetStream);
  /// how TInterfaceMethod should implement this service provider method
  TInterfaceMethodFlags = set of TInterfaceMethodFlag;

  /// describe an interface-based service provider method
  {$ifdef USERECORDWITHMETHODS}
  TInterfaceMethod = record
  {$else}
  TInterfaceMethod = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the method URI, i.e. the method name
    // - as declared in Object Pascal code, e.g. 'Add' for ICalculator.Add
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
    // - so it maps TServiceFactory.Interface.Methods[ExecutionMethodIndex - 3]
    ExecutionMethodIndex: byte;
    /// how this method is defined and should be processed
    Flags: TInterfaceMethodFlags;
    /// the directions of arguments with SPI parameters defined
    HasSpiParams: TInterfaceMethodValueDirections;
    /// is 0 for the root interface, 1..n for all inherited interfaces
    HierarchyLevel: byte;
    /// the number of const / var parameters in Args[]
    // - i.e. the number of elements in the input JSON array
    ArgsInputValuesCount: byte;
    /// the number of var / out parameters +  in Args[]
    // - i.e. the number of elements in the output JSON array or object
    ArgsOutputValuesCount: byte;
    /// needed CPU stack size (in bytes) for all arguments
    // - under x64, does not include the backup space for the four registers
    ArgsSizeInStack: word;
    /// describe expected method arguments
    // - Args[0] is always imvSelf
    // - if method is a function, an additional imdResult argument is appended
    Args: TInterfaceMethodArgumentDynArray;
    /// the index of the result pseudo-argument in Args[] (signed 8-bit)
    // - is -1 if the method is defined as a procedure (not a function)
    ArgsResultIndex: ShortInt;
    /// the index of the first const / var argument in Args[] (signed 8-bit)
    ArgsInFirst: ShortInt;
    /// the index of the last const / var argument in Args[] (signed 8-bit)
    ArgsInLast: ShortInt;
    /// the index of the first var / out / result argument in Args[] (signed 8-bit)
    ArgsOutFirst: ShortInt;
    /// the index of the last var / out / result argument in Args[] (signed 8-bit)
    ArgsOutLast: ShortInt;
    /// the index of the last argument in Args[], excepting result (signed 8-bit)
    ArgsNotResultLast: ShortInt;
    /// the index of the last var / out argument in Args[] (signed 8-bit)
    ArgsOutNotResultLast: ShortInt;
    /// the index of the first argument expecting manual stack initialization
    // - set for Args[].ValueVar >= imvvRawUtf8 (signed 8-bit)
    ArgsManagedFirst: ShortInt;
    /// how manual stack initialization arguments are defined
    // - set for Args[].ValueVar >= imvvRawUtf8
    ArgsManagedCount: byte;
    /// contains all used kind of arguments
    ArgsUsed: TInterfaceMethodValueTypes;
    /// 64-bit aligned cumulative size for all arguments values
    // - follow Args[].OffsetAsValue distribution, and used to allocate/reset
    // the stack memory buffer before execution
    ArgsSizeAsValue: cardinal;
    /// the RawUtf8 names of all arguments, as declared in Object Pascal
    ArgsName: TRawUtf8DynArray;
    /// the RawUtf8 names of all input arguments, as declared in Object Pascal
    ArgsInputName: TRawUtf8DynArray;
    /// the RawUtf8 names of all output arguments, as declared in Object Pascal
    ArgsOutputName: TRawUtf8DynArray;
    /// contains the count of variables for all used kind of arguments
    ArgsUsedCount: array[TInterfaceMethodValueVar] of byte;
    /// retrieve a const / var argument in Args[] from its name
    // - search is case insensitive, returns -1 if not found
    function ArgInput(ArgName: PUtf8Char; ArgNameLen: PtrInt;
      ArgIndex: PInteger = nil): PInterfaceMethodArgument;
    /// retrieve a var / out / result argument Args[] from its name
    // - search is case insensitive, returns -1 if not found
    function ArgOutput(ArgName: PUtf8Char; ArgNameLen: PtrInt;
      ArgIndex: PInteger = nil): PInterfaceMethodArgument;
    /// retrieve an argument index in Args[] from its name
    // - search is case insensitive, returns -1 if not found
    function ArgInputOutput(const ArgName: RawUtf8;
      Input: boolean): PInterfaceMethodArgument;
    /// find the next input or output argument 32-bit index in Args[]
    // - returns true if arg is the new value, false otherwise
    function ArgNext(var Arg: integer; Input: boolean): boolean;
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
    procedure ArgsStackAsDocVariant(Values: PPointerArray;
      out Dest: TDocVariantData; Input: boolean);
    /// create new input and output TObject instances before execution
    // - caller should ensure that ArgsUsedCount[imvvObject] <> 0
    procedure ArgsClassNewInstance(V: PPPointer);
    /// finalize all managed values after an execution
    // - caller should ensure that ArgsManagedCount <> 0
    procedure ArgsReleaseValues(V: PPointer);
  end;

  /// describe all mtehods of an interface-based service provider
  TInterfaceMethodDynArray = array of TInterfaceMethod;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TServiceMethodValueType            = TInterfaceMethodValueType;
  TServiceMethodValueTypes           = TInterfaceMethodValueTypes;
  TServiceMethodValueVar             = TInterfaceMethodValueVar;
  TServiceMethodValueDirection       = TInterfaceMethodValueDirection;
  TServiceMethodValueDirections      = TInterfaceMethodValueDirections;
  TServiceMethodArgument             = TInterfaceMethodArgument;
  PServiceMethodArgument             = PInterfaceMethodArgument;
  TServiceMethodArgumentDynArray     = TInterfaceMethodArgumentDynArray;
  TServiceMethodParamsDocVariantKind = TInterfaceMethodParamsDocVariantKind;
  TServiceMethod                     = TInterfaceMethod;
  TServiceMethodDynArray             = TInterfaceMethodDynArray;
  PServiceMethod                     = PInterfaceMethod;

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
  // smvBinary = imvBinary; not defined any more (handle by RTTI itself)
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
  // - if you think this constant is too low, you are clearing breaking
  // the "Interface Segregation" SOLID principle: so don't ask to increase
  // this value, we won't allow to write obviously un-SOLID code! :)
  // - indexes would also fit in a signed [-1..127] 8-bit ShortInt
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
  // - imFree expects an associated ClientID, other methods won't
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

  /// the number of MethodIndex which are a TServiceInternalMethod, i.e. 4
  SERVICE_PSEUDO_METHOD_COUNT = length(SERVICE_PSEUDO_METHOD);

var
  /// default value for TInterfaceFactory.JsonParserOptions - tolerant enough
  JSONPARSER_SERVICE: TJsonParserOptions =
    [jpoHandleCustomVariants,
     jpoIgnoreUnknownEnum,
     jpoIgnoreUnknownProperty,
     jpoIgnoreStringType,
     jpoAllowInt64Hex,
     jpoNullDontReleaseObjectInstance];

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

  /// index-based reference to one TInterfaceFactory argument
  TInterfaceFactoryArgument = record
    /// the index of the method argument in TInterfaceFactory.Methods[]
    MethodIndex: byte;
    /// the index of the method argument in TInterfaceFactory.Methods[].Args[]
    ArgIndex: byte;
  end;

  /// index-based reference to several TInterfaceFactory argument
  TInterfaceFactoryArgumentDynArray = array of TInterfaceFactoryArgument;

  /// per-type reference of TInterfaceFactory arguments
  TInterfaceFactoryPerArgumentDynArray =
     array[TInterfaceMethodValueType] of TInterfaceFactoryArgumentDynArray;

  /// a dynamic array of TInterfaceFactory instances
  TInterfaceFactoryObjArray = array of TInterfaceFactory;

  /// class handling interface RTTI and fake implementation class
  // - an internal JIT compiler will generate the raw asm opcodes to redirect
  // any interface execution into a fake class
  // - a thread-safe global list of such class instances is implemented to cache
  // information for better speed: use class function TInterfaceFactory.Get()
  // and not manual TInterfaceFactory.Create / Free
  // - if you want to search the interfaces by name or TGuid, call once
  // Get(TypeInfo(IMyInterface)) or RegisterInterfaces() for proper registration
  // - will use TInterfaceFactoryRtti classes generated from compiler RTTI
  TInterfaceFactory = class
  protected
    fInterfaceRtti: TRttiJson;
    fMethods: TInterfaceMethodDynArray;
    fInterfaceName: RawUtf8;
    fInterfaceUri: RawUtf8;
    fDocVariantOptions: TDocVariantOptions;     // (16-bit)
    fJsonParserOptions: TJsonParserOptions;     // (16-bit)
    fMethodsCount: byte;                        // (8-bit)
    fAddMethodsLevel: byte;                     // (8-bit)
    fMethodIndexCallbackReleased: ShortInt;     // (8-bit)
    fMethodIndexCurrentFrameCallback: ShortInt; // (8-bit)
    fArgUsed: TInterfaceFactoryPerArgumentDynArray;
    // contains e.g. [{"method":"Add","arguments":[...]},{"method":"...}]
    fContract: RawUtf8;
    {$ifdef CPUX86}  // i386 stub requires "ret ArgsSizeInStack"
    fFakeVTable: TPointerDynArray;
    {$endif CPUX86}
    procedure AddMethodsFromTypeInfo(aInterface: PRttiInfo); virtual; abstract;
    // low-level JIT redirection of the VMT to TInterfacedObjectFake.FakeCall
    function GetMethodsVirtualTable: pointer;
  public
    /// this is the main entry point to the global interface factory cache
    // - access to this method is thread-safe
    // - this method will also register the class to further retrieval
    class function Get(aInterface: PRttiInfo): TInterfaceFactory; overload;
    /// retrieve an interface factory from cache, from its TGuid
    // - access to this method is thread-safe
    // - you shall have registered the interface by a previous call to the
    // overloaded Get(TypeInfo(IMyInterface)) method or RegisterInterfaces()
    // - if the supplied TGuid has not been previously registered, returns nil
    class function Get({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      aGuid: TGuid): TInterfaceFactory; overload;
    /// retrieve an interface factory from cache, from its name (e.g. 'IMyInterface')
    // - access to this method is thread-safe
    // - you shall have registered the interface by a previous call to the
    // overloaded Get(TypeInfo(IMyInterface)) method or RegisterInterfaces()
    // - if the supplied TGuid has not been previously registered, returns nil
    class function Get(const aInterfaceName: RawUtf8): TInterfaceFactory; overload;
    /// register one or several interfaces to the global interface factory cache
    // - so that you can use TInterfaceFactory.Get(aGuid) or Get(aName)
    class procedure RegisterInterfaces(const aInterfaces: array of PRttiInfo);
    /// could be used to retrieve an array of TypeInfo() from their Guid
    class function Guid2TypeInfo(const aGuids: array of TGuid): PRttiInfoDynArray; overload;
    /// could be used to retrieve an array of TypeInfo() from their Guid
    class function Guid2TypeInfo({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      aGuid: TGuid): PRttiInfo; overload;
    /// returns the list of all declared TInterfaceFactory
    // - as used by SOA and mocking/stubing features of this unit
    class function GetUsedInterfaces: TSynObjectListLightLocked;
    /// add some TInterfaceFactory instances from their Guid
    class procedure AddToObjArray(var Obj: TInterfaceFactoryObjArray;
      const aGuids: array of TGuid);
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
    /// find the index of a particular URI in internal Methods[] list
    // - will search for a match against Methods[].Uri property
    // - won't find the default AddRef/Release/QueryInterface methods,
    // nor the _free_/_instance_/... pseudo-methods
    // - will return -1 if the method is not known
    // - if aUrl does not have an exact method match, it will try with a
    // trailing underscore, so that e.g. /service/start will match IService._Start()
    function FindMethodIndex(const aUrl: RawUtf8): PtrInt;
    /// find the index of a particular method in internal Methods[] list
    // - without accepting /service/start for IService._Start()
    function FindMethodIndexExact(const aMethodName: RawUtf8): PtrInt;
    /// find a particular method in internal Methods[] list
    // - just a wrapper around FindMethodIndex() returing a PInterfaceMethod
    // - will return nil if the method is not known
    function FindMethod(const aUrl: RawUtf8): PInterfaceMethod;
    /// find the index of a particular interface.method in internal Methods[] list
    // - will search for a match against Methods[].InterfaceDotMethodName property
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will return -1 if the method is not known
    function FindFullMethodIndex(const aFullMethodName: RawUtf8;
      alsoSearchExactMethodName: boolean = false): integer;
    /// find the index of a particular method in internal Methods[] list
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will raise an EInterfaceFactory if the method is not known
    function CheckMethodIndex(const aUrl: RawUtf8): PtrInt; overload;
    /// find the index of a particular method in internal Methods[] list
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will raise an EInterfaceFactory if the method is not known
    function CheckMethodIndex(aUrl: PUtf8Char): integer; overload;
    /// returns the method name from its method index
    // - the method index should start at 0 for _free_/_contract_/_signature_
    // pseudo-methods, and start at index 3 for real Methods[]
    function GetMethodName(aMethodIndex: integer): RawUtf8;
    /// set the Methods[] indexes bit from some methods names
    // - won't find the default AddRef/Release/QueryInterface methods
    // - will raise an EInterfaceFactory if the method is not known
    procedure CheckMethodIndexes(const aUrl: array of RawUtf8;
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
    // - nor the _free_/_contract_/_signature_ pseudo-methods: so you should
    // add SERVICE_PSEUDO_METHOD_COUNT to compute the regular MethodIndex
    property MethodsCount: byte
      read fMethodsCount;
    /// reference all known interface arguments per value type
    property ArgUsed: TInterfaceFactoryPerArgumentDynArray
      read fArgUsed;
    /// identifies a CallbackReleased() method in this interface (signed 8-bit)
    // - i.e. the index in Methods[] of the following signature:
    // ! procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUtf8);
    // - this method will be called e.g. by TInterfacedCallback.Destroy, when
    // a callback is released on the client side so that you may be able e.g. to
    // unsubscribe the callback from an interface list (via InterfaceArrayDelete)
    // - contains -1 if no such method do exist in the interface definition
    property MethodIndexCallbackReleased: ShortInt
      read fMethodIndexCallbackReleased;
    /// identifies a CurrentFrame() method in this interface (signed 8-bit)
    // - i.e. the index in Methods[] of the following signature:
    // ! procedure CurrentFrame(isLast: boolean);
    // - this method will be called e.g. by TRestHttpClientWebsockets.CallbackRequest
    // for interface callbacks in case of WebSockets jumbo frames, to allow e.g.
    // faster database access via a batch
    // - contains -1 if no such method do exist in the interface definition
    property MethodIndexCurrentFrameCallback: ShortInt
      read fMethodIndexCurrentFrameCallback;
    /// the interface name, without its initial 'I'
    // - e.g. ICalculator -> 'Calculator'
    property InterfaceUri: RawUtf8
      read fInterfaceUri write fInterfaceUri;
    /// the registered Interface high-level compiler RTTI type
    property InterfaceRtti: TRttiJson
      read fInterfaceRtti;
    /// the interface TGUID, as stored in the RTTI
    function InterfaceGuid: PGuid;
      {$ifdef HASINLINE} inline; {$endif}
    /// the service contract as a JSON array
    property Contract: RawUtf8
      read fContract;
    /// how this interface will work with variants (including TDocVariant)
    // - by default, contains JSON_FAST_FLOAT for best performance - i.e.
    // [dvoReturnNullForUnknownProperty,dvoValueCopiedByReference, dvoAllowDoubleValue]
    property DocVariantOptions: TDocVariantOptions
      read fDocVariantOptions write fDocVariantOptions;
    /// how this interface will process its JSON parsing
    // - by default, contains JSONPARSER_SERVICE very relaxed parsing options
    property JsonParserOptions: TJsonParserOptions
      read fJsonParserOptions write fJsonParserOptions;
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

  /// an integer type to be used as result for a function method to customize
  // the HTTP response code for interface-based services
  // - by default, our protocol returns HTTP_SUCCESS = 200 for any process
  // - using this type as result allow to return the execution error code as a
  // regular HTTP_* response code, in addition to the regular JSON answer - i.e.
  // there will be a "result" member in the transmitted JSON anyway
  // - the returned value should be in HTTP response code range, i.e. 200..599
  // - by design, HTTP_NOCONTENT can/should not be used: return HTTP_SUCCESS and
  // set rsoHttp200WithNoBodyReturns204 option to let TRestServer.Uri decide and
  // return HTTP_SUCCESS if there is an output body, or HTTP_NOCONTENT if void
  TServiceCustomStatus = type cardinal;

  /// indicates how TWebSocketProcess.NotifyCallback() will work
  // - published early in this unit to be used at pure REST/SOA level
  // - sometimes respectively logged as 'B', 'W' or 'N'
  TWebSocketProcessNotifyCallback = (
    wscBlockWithAnswer,
    wscBlockWithoutAnswer,
    wscNonBlockWithoutAnswer);

/// returns the interface name of a registered Guid, or its hexadecimal value
function ToText({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  aGuid: TGuid): ShortString; overload;


{ ************ TInterfaceResolver TInjectableObject for IoC / Dependency Injection  }

type
  /// exception raised in case of Dependency Injection (aka IoC) issue
  EInterfaceResolver = class(ESynException);

  /// abstract factory class allowing to call interface resolution in cascade
  // - you can inherit from this class to chain the TryResolve() calls so
  // that several kind of implementations may be asked by a TInjectableObject,
  // e.g. TInterfaceStub, TServiceContainer or TDDDRepositoryRestObjectMapping
  // - this will implement factory pattern, as a safe and thread-safe DI/IoC
  TInterfaceResolver = class(TSynPersistent)
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
    // - you shall have registered the interface TGuid by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(ICalculator),...])
    // - returns TRUE and set the Obj variable with a matching instance
    // - returns FALSE (or raise aRaiseIfNotFound) if aGuid is not available
    // - can be used as such to resolve an ICalculator interface:
    // ! var calc: ICalculator;
    // ! begin
    // !   if ServiceContainer.Resolve(ICalculator,cal) then
    // !   ... use calc methods
    function Resolve({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}aGuid: TGuid;
      out Obj; aRaiseIfNotFound: ESynExceptionClass = nil): boolean; overload;
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TypeInfo,@Instance pairs
    // - raise an EServiceException if any interface can't be resolved, unless
    // aRaiseExceptionIfNotFound is set to FALSE
    procedure ResolveByPair(const aInterfaceObjPairs: array of pointer;
      aRaiseExceptionIfNotFound: boolean = true);
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TGuid and @Instance
    // - you shall have registered the interface TGuid by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(ICalculator),...])
    // - raise an EServiceException if any interface can't be resolved, unless
    // aRaiseExceptionIfNotFound is set to FALSE
    procedure Resolve(const aInterfaces: array of TGuid;
                      const aObjs: array of pointer;
      aRaiseExceptionIfNotFound: boolean = true); overload;
  end;

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
      aImplementation: TInterfacedObjectClass); reintroduce; overload;
    /// this overriden constructor will check and store the supplied class
    // to implement an interface by TGuid
    constructor Create(const aInterface: TGuid;
      aImplementation: TInterfacedObjectClass); reintroduce;overload;
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
    fSafe: TRWLightLock;
    fEntry: TInterfaceResolverListEntries;
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
    // internal function matching Implements() for ickFromInjectedResolver
    function TryResolveImplements(aInterface: PRttiInfo; out Obj): boolean;
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
    // TInterfaceStub specified by their TGuid
    procedure InjectStub(const aStubsByGuid: array of TGuid); overload; virtual;
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
    // - will only check internal InjectResolver() list, not InjectInstance()
    // nor RegisterGlobal() resolution
    function Implements(aInterface: PRttiInfo): boolean; override;
    /// delete a previously registered resolver
    // - aResolver can be re-registered afterwards
    procedure DeleteResolver(aResolver: TInterfaceResolver);
    /// release all used instances
    // - including all TInterfaceStub instances as specified to Inject(aStubsByGuid)
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
  TInjectableObject = class(TInterfacedPersistent)
  protected
    fResolver: TInterfaceResolver;
    fResolverOwned: boolean;
    // DI/IoC resolution protected methods
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean;
    /// this method will resolve all interface published properties
    procedure AutoResolve(aRaiseEServiceExceptionIfNotFound: boolean);
  public
    /// initialize an instance, defining one or several mean of dependency resolution
    // - simple TInterfaceStub could be created directly from their TGuid,
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
    constructor CreateInjected(const aStubsByGuid: array of TGuid;
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
    /// can be used to perform an DI/IoC for a given interface TGuid
    procedure Resolve({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      aGuid: TGuid; out Obj); overload;
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TypeInfo,@Instance pairs
    procedure ResolveByPair(const aInterfaceObjPairs: array of pointer);
    /// can be used to perform several DI/IoC for a given set of interfaces
    // - here interfaces and instances are provided as TGuid and pointers
    procedure Resolve(const aInterfaces: array of TGuid;
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


{ ************ TInterfaceStub for Dependency Stubbing/Mocking }

// note: TInterfaceMock is defined in mormot.core.test.pas - as expected

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
  public
    /// constructor of one parameters marshalling instance
    constructor Create(aSender: TInterfaceStub; aMethod: PInterfaceMethod;
      const aParams, aEventParams: RawUtf8); virtual;
    /// call this method if the callback implementation failed
    procedure Error(const aErrorMessage: RawUtf8); overload;
    /// call this method if the callback implementation failed
    procedure Error(const Format: RawUtf8; const Args: array of const); overload;
    /// the stubbing / mocking generator
    // - use e.g. (Sender as TInterfaceMock).TestCase to retrieve the test case
    property Sender: TInterfaceStub
      read fSender;
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
      aLevel: TSynLogLevel = sllTrace);
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
    // - just a wrapper around JsonEncodeArray([...])
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
  PInterfaceStubRule = ^TInterfaceStubRule;

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
  PInterfaceStubRules = ^TInterfaceStubRules;

  /// how TInterfacedObjectFake identify each instance
  // - match the ID used in sicClientDriven mode of a service
  // - match the TInterfacedObjectFakeServer 32-bit identifier of a callback
  TInterfacedObjectFakeID = type cardinal;
  PInterfacedObjectFakeID = ^TInterfacedObjectFakeID;

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
    // !var
    // !  I: ICalculator;
    // ! ...
    // !  TInterfaceStub.Create(TypeInfo(ICalculator),I);
    // !  Check(I.Add(10,20)=0,'Default result');
    constructor Create(aInterface: PRttiInfo;
      out aStubbedInterface); reintroduce; overload;
    /// initialize an interface stub from an interface Guid
    // - you shall have registered the interface by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...])
    // - once registered, create and use the fake class instance as such:
    // !var
    // !  I: ICalculator;
    // ! ...
    // !  TInterfaceStub.Create(ICalculator,I);
    // !  Check(I.Add(10,20)=0,'Default result');
    // - if the supplied TGuid has not been previously registered, raise an Exception
    constructor Create(const aGuid: TGuid;
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
    /// prepare an interface stub from a given TGuid for later injection
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
    constructor Create(const aGuid: TGuid); reintroduce; overload;

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
    function Executes(aLog: TSynLogClass; aLogLevel: TSynLogLevel;
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

function ToText(op: TInterfaceStubRuleOperator): PShortString; overload;


{ ************ TInterfacedObjectFake with JITted Methods Execution }

{ some reference material
WIN32 i386 (Delphi + FPC "register" calling convention):
 http://docwiki.embarcadero.com/RADStudio/en/Program_Control
WIN64 x64:
 https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention#parameter-passing
WIN aarch64:
 https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions#parameter-passing
SYSV x64:
 https://gitlab.com/x86-psABIs/x86-64-ABI/-/jobs/artifacts/master/raw/x86-64-ABI/abi.pdf?job=build
SYSV aarch64:
 https://c9x.me/compile/bib/abi-arm64.pdf
}

const
{$ifdef CPUX86}
  MAX_EXECSTACK = 1024;
  VMTSTUBSIZE = 24 {$ifdef OSPOSIX} + 4 {$endif};
  // 32-bit integer param registers (in Delphi + FPC calling convention)
  REGEAX = 1;
  REGEDX = 2;
  REGECX = 3;
  PARAMREG_FIRST = REGEAX;
  PARAMREG_LAST  = REGECX;
  // x87 floating-point params are passed by reference, but result with fstp
  {$undef HAS_FPREG}
{$endif CPUX86}

{$ifdef CPUX64}
  MAX_EXECSTACK = MAX_METHOD_ARGS * 8; // match .PARAMS 32
  VMTSTUBSIZE = 24;
  // 64-bit integer param registers
  {$ifdef SYSVABI}
  REGRDI = 1;
  REGRSI = 2;
  REGRDX = 3;
  REGRCX = 4;
  REGR8  = 5;
  REGR9  = 6;
  PARAMREG_FIRST  = REGRDI;
  PARAMREG_RESULT = REGRSI;
  {$else}
  REGRCX = 1;
  REGRDX = 2;
  REGR8  = 3;
  REGR9  = 4;
  PARAMREG_FIRST  = REGRCX;
  PARAMREG_RESULT = REGRDX;
  {$endif SYSVABI}
  PARAMREG_LAST = REGR9;
  // 64-bit floating-point (double) registers
  {$define HAS_FPREG} // XMM0..XMM3 (WIN64ABI) or XMM0..XMM7 (SYSVABI)
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
  FPREG_LAST  = REGXMM7;
  {$else}
  FPREG_FIRST = REGXMM0;
  FPREG_LAST  = REGXMM3;
  {$endif SYSVABI}
{$endif CPUX64}

{$ifdef CPUARM}
  MAX_EXECSTACK = (MAX_METHOD_ARGS - 4) * 8; // may store only doubles on stack
  VMTSTUBSIZE = 16;
  // 32-bit integer param registers
  REGR0 = 1;
  REGR1 = 2;
  REGR2 = 3;
  REGR3 = 4;
  PARAMREG_FIRST  = REGR0;
  PARAMREG_LAST   = REGR3;
  PARAMREG_RESULT = REGR1;
  // 64-bit floating-point (double) registers
  {$define HAS_FPREG} // D0..D7
  // assume CPUARMHF target
  REGD0 = 1;
  REGD1 = 2;
  REGD2 = 3;
  REGD3 = 4;
  REGD4 = 5;
  REGD5 = 6;
  REGD6 = 7;
  REGD7 = 8;
  FPREG_FIRST = REGD0;
  FPREG_LAST  = REGD7;
{$endif CPUARM}

{$ifdef CPUAARCH64}
  MAX_EXECSTACK = (MAX_METHOD_ARGS - 8) * 8;
  VMTSTUBSIZE = 28;
  // 64-bit integer param registers
  REGX0 = 1;
  REGX1 = 2;
  REGX2 = 3;
  REGX3 = 4;
  REGX4 = 5;
  REGX5 = 6;
  REGX6 = 7;
  REGX7 = 8;
  PARAMREG_FIRST  = REGX0;
  PARAMREG_LAST   = REGX7;
  PARAMREG_RESULT = REGX1;
  // 64-bit floating-point (double) registers
  {$define HAS_FPREG} // D0..D7
  REGD0 = 1; // map REGV0 128-bit NEON register
  REGD1 = 2; // REGV1
  REGD2 = 3; // REGV2
  REGD3 = 4; // REGV3
  REGD4 = 5; // REGV4
  REGD5 = 6; // REGV5
  REGD6 = 7; // REGV6
  REGD7 = 8; // REGV7
  FPREG_FIRST = REGD0;
  FPREG_LAST  = REGD7;
{$endif CPUAARCH64}

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
    imvvRecord,       // imvRecord
    imvvRecord,       // imvVariant
    imvvObject,       // imvObject
    imvvRawUtf8,      // imvRawJson
    imvvDynArray,     // imvDynArray
    imvvInterface);   // imvInterface

  {$ifdef CPU32}
  // parameters are always aligned to 8 bytes boundaries on 64-bit ABI
  // but may be on 32-bit or 64-bit on 32-bit CPU
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
    ParamRegs: packed array[PARAMREG_FIRST .. PARAMREG_LAST] of pointer;
    {$endif OSPOSIX}
    {$ifdef HAS_FPREG}
    FPRegs: packed array[FPREG_FIRST..FPREG_LAST] of double;
    {$endif HAS_FPREG}
    MethodIndex: PtrUInt;
    Frame: pointer;
    Ret: pointer;
    {$ifndef OSPOSIX}
    ParamRegs: packed array[PARAMREG_FIRST .. PARAMREG_LAST] of pointer;
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
    Value: array[0..MAX_METHOD_ARGS - 1] of pointer;
    I64s:  array[0..MAX_METHOD_ARGS - 1] of Int64; // to store register results
  end;

type
  {$M+}
  /// abstract class handling a generic interface implementation class
  // - implements a simple cross-CPU JIT engine to redirect to FakeCall to
  // its FakeCallInternalProcess() virtual method
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
      IID: TGuid; out Obj): TIntQry; {$ifdef OSWINDOWS}stdcall{$else}cdecl{$endif};
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
    fParamsSafe: TLightLock;  // thread-safe fParams - topmost for aarch64 align
    fFakeID: TInterfacedObjectFakeID;
    fOptions: TInterfacedObjectFakeOptions;
    fInvoke: TOnFakeInstanceInvoke;
    fServiceFactory: TObject; // holds a TServiceFactory instance
    fParams: TJsonWriter;     // reused if possible between calls
    fNotifyDestroy: TOnFakeInstanceDestroy;
    // the JITed asm stubs will redirect to these JSON-oriented process
    procedure FakeCallGetJsonFromStack(
      var ctxt: TFakeCallContext; var Json: RawUtf8); virtual;
    procedure FakeCallSetJsonToStack(var ctxt: TFakeCallContext; R: PUtf8Char);
    procedure FakeCallInternalProcess(var ctxt: TFakeCallContext); override;
    // should be overriden to support interface parameters (i.e. callbacks)
    procedure InterfaceWrite(W: TJsonWriter; const aMethod: TInterfaceMethod;
      const aParamInfo: TInterfaceMethodArgument; aParamValue: pointer); virtual;
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
  // - see TInterfaceMethodOptions for a description of each available option
  TInterfaceMethodOption = (
    optExecGlobalLocked,
    optFreeGlobalLocked,
    optExecLockedPerInterface,
    optFreeLockedPerInterface,
    optExecInPerInterfaceThread,
    optFreeInPerInterfaceThread,
    optFreeDelayed,
    optExecInMainThread,
    optFreeInMainThread,
    optVariantCopiedByReference,
    optVariantFloatAllowed,
    optInterceptInputOutput,
    optNoLogInput,
    optNoLogOutput,
    optErrorOnMissingParam,
    optForceStandardJson,
    optDontStoreVoidJson,
    optIgnoreException,
    optFreeTimeout);

  /// set of per-method execution options for an interface-based service provider
  // - by default, method executions are concurrent, for better server
  // responsiveness; if you set optExecLockedPerInterface/optFreeLockedPerInterface,
  // all methods of a given interface will be executed within a critical section;
  // if you set optExecGlobalLocked/optFreeGlobalLocked, execution is done within
  // a critical section global to all interfaces
  // - optExecInMainThread will force the method to be called within
  // a RunningThread.Synchronize() call - it can be used e.g. if your
  // implementation rely heavily on COM servers - by default, service methods
  // are called within the thread which received them, on multi-thread server
  // instances (e.g. TSqlite3HttpServer or TRestServerNamedPipeResponse),
  // for better response time and CPU use (this is the technical reason why
  // service implementation methods have to handle multi-threading safety
  // carefully, e.g. by using TOSLock mutex on purpose) - warning:
  // a Windows Service has no 'main thread' concept, so should not use it
  // - optFreeInMainThread will force the _Release/Destroy method to be run
  // in the main thread: setting this option for any method will affect the
  // whole service class - is not set by default, for performance reasons
  // - optExecInPerInterfaceThread and optFreeInPerInterfaceThread will allow
  // creation of a per-interface dedicated thread
  // - by default _Release is done within the service lock, unless optFreeDelayed
  // is set and instances are released later (e.g. if may take some time)
  // - optVariantCopiedByReference and optVariantFloatAllowed are used to
  // generate the proper TDocVariantData options
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
  // - optFreeTimeout will enable the time check of the _Release call using
  // TRestServer.ServiceReleaseTimeoutMicrosec delay
  TInterfaceMethodOptions = set of TInterfaceMethodOption;

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
  // - optInterceptInputOutput should be defined in Options so that
  // Sender.Input/Output values will be filled with parameters (slower but
  // easier to consume than Sender.Values raw pointers)
  // - is called for each Step, i.e. smsBefore/smsAfter
  // - smsError is called when TInterfaceMethodExecute.LastException was raised
  TOnInterfaceMethodExecute = procedure(Sender: TInterfaceMethodExecuteRaw;
    Step: TInterfaceMethodExecuteEventStep) of object;

  /// store one or several TInterfaceMethodExecute.OnExecute signatures
  TInterfaceMethodExecuteEventDynArray = array of TOnInterfaceMethodExecute;

  /// abtract execution of a TInterfacedObject method
  TInterfaceMethodExecuteRaw = class
  protected
    fFactory: TInterfaceFactory;
    fMethod: PInterfaceMethod;
    fStorage: TByteDynArray;
    fValues: PPointerArray;
    fAlreadyExecuted: boolean;
    fCurrentStep: TInterfaceMethodExecuteEventStep;
    fOptions: TInterfaceMethodOptions;
    fDocVariantOptions: TDocVariantOptions;
    fOnExecute: TInterfaceMethodExecuteEventDynArray;
    fBackgroundExecutionThread: TSynBackgroundThreadMethod;
    fLastException: Exception;
    fExecutedInstancesFailed: TRawUtf8DynArray;
    fInput: TDocVariantData;
    fOutput: TDocVariantData;
    procedure SetOptions(const Value: TInterfaceMethodOptions);
    procedure BeforeExecute; {$ifdef HASINLINE} inline; {$endif}
    procedure RawExecute(const Instances: PPointerArray; InstancesLast: integer);
    procedure AfterExecute;  {$ifdef HASINLINE} inline; {$endif}
  public
    /// initialize the execution instance
    constructor Create(aFactory: TInterfaceFactory; aMethod: PInterfaceMethod;
      const aOptions: TInterfaceMethodOptions); virtual;
    /// allow to hook method execution
    // - if optInterceptInputOutput is defined in Options, then Sender.Input/Output
    // fields will contain the execution data context when Hook is called
    procedure AddInterceptor(const Hook: TOnInterfaceMethodExecute);
    /// allow to hook method execution
    // - if optInterceptInputOutput is defined in Options, then Sender.Input/Output
    // fields will contain the execution data context when Hook[] are called
    procedure AddInterceptors(const Hook: TInterfaceMethodExecuteEventDynArray);

    /// low-level direct access to the associated interface factory information
    property Factory: TInterfaceFactory
      read fFactory;
    /// low-level direct access to the associated method information
    property Method: PInterfaceMethod
      read fMethod;
    /// low-level direct access to the current input/output parameter values
    // - you should not need to access this, but rather set
    // optInterceptInputOutput in Options, and read Input/Output content
    property Values: PPointerArray
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
      read fOptions write SetOptions;
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
    function ExecuteJsonParse(var Ctxt: TJsonParserContext; Error: PShortString): boolean;
  public
    /// finalize the execution instance
    destructor Destroy; override;
    /// execute the corresponding method of weak IInvokable references
    // - will retrieve a JSON array of parameters from P buffer (as [1,"par2",3])
    // - will append a JSON array of results in Res, or set an Error message, or
    // a JSON object (with parameter names) in Res if ResultAsJsonObject is set
    // - if one Instances[] is supplied, any exception will be propagated (unless
    // optIgnoreException is set); if more than one Instances[] is supplied,
    // corresponding ExecutedInstancesFailed[] property will be filled with
    // the JSON serialized exception
    function ExecuteJson(const Instances: array of pointer; P: PUtf8Char;
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
    /// set from output TServiceCustomAnswer.Status or TServiceCustomStatus
    // result parameter
    property ServiceCustomAnswerStatus: cardinal
      read fServiceCustomAnswerStatus write fServiceCustomAnswerStatus;
    /// points e.g. to TRestServerUriContext.ExecuteCallback which
    // redirects to TServiceContainerServer.GetFakeCallback
    property OnCallback: TOnInterfaceMethodExecuteCallback
      read fOnCallback write fOnCallback;
    /// allow to use an instance-specific temporary TOrmWriter
    function TempTextWriter: TJsonWriter;
  end;

  TInterfaceMethodExecuteCached = class;

  /// set reusable interface methods execution from/to JSON
  TInterfaceMethodExecuteCachedDynArray = array of TInterfaceMethodExecuteCached;

  /// reusable interface method execution from/to JSON
  // - used e.g. by TServiceFactoryServer.ExecuteJson
  TInterfaceMethodExecuteCached = class(TInterfaceMethodExecute)
  protected
    fCached: TLightLock; // thread-safe acquisition of fCachedWR
    fWR: TJsonWriter;
  public
    /// initialize a TInterfaceMethodExecuteCachedDynArray of per-method caches
    class procedure Prepare(aFactory: TInterfaceFactory;
      out Cached: TInterfaceMethodExecuteCachedDynArray);
    /// initialize the execution instance
    constructor Create(aFactory: TInterfaceFactory; aMethod: PInterfaceMethod;
      const aOptions: TInterfaceMethodOptions; aShared: boolean = false); reintroduce;
    /// finalize this execution context
    destructor Destroy; override;
    /// will use this instance if possible, or create a temporary one
    function Acquire(ExecuteOptions: TInterfaceMethodOptions = [];
      WROptions: TTextWriterOptions = []): TInterfaceMethodExecuteCached;
    /// will release this instance if was acquired, or free a temporary one
    procedure Release(exec: TInterfaceMethodExecuteCached);
      {$ifdef HASINLINE} inline; {$endif}
    /// each instance will own their own TJsonWriter associated serializer
    property WR: TJsonWriter
      read fWR;
  end;


/// low-level execution of a procedure of object in a given background thread
procedure BackgroundExecuteThreadMethod(const method: TThreadMethod;
  backgroundThread: TSynBackgroundThreadMethod);

/// low-level execution of TInterfacedObject._Release in a given background thread
procedure BackgroundExecuteInstanceRelease(instance: TObject;
  backgroundThread: TSynBackgroundThreadMethod);


/// low-level internal function returning the TServiceRunningContext threadvar
// - mormot.rest.server.pas ServiceRunningContext function redirects to this
// - not inlined to ensure the associated threadvar is always properly linked
function PerThreadRunningContextAddress: pointer;

const
  /// marker used internally to pass ServiceMethod^.ArgsInputIsOctetStream
  // - used by both TRestServerRoutingRest.ExecuteSoaByInterface and
  // TInterfaceMethodExecute.ExecuteJson, followed by a RawByteString pointer
  // - is U+FFF2 UTF-8 marker, ending with a #0 so to be identified within JSON
  JSON_BIN_MAGIC_C = $b2bfef;

  /// the TInterfaceMethodOptions which are related to custom thread execution
  INTERFACEMETHOD_THREADOPTIONS = [
    optExecGlobalLocked,
    optFreeGlobalLocked,
    optExecLockedPerInterface,
    optFreeLockedPerInterface,
    optExecInPerInterfaceThread,
    optFreeInPerInterfaceThread,
    optExecInMainThread,
    optFreeInMainThread];

  /// the related TInterfaceMethodOptions, grouped per thread mode
  INTERFACEMETHOD_PERTHREADOPTIONS: array[0..3] of TInterfaceMethodOptions = (
    [optExecGlobalLocked, optFreeGlobalLocked],
    [optExecLockedPerInterface, optFreeLockedPerInterface],
    [optExecInPerInterfaceThread, optFreeInPerInterfaceThread],
    [optExecInMainThread, optFreeInMainThread]);

/// return the interface execution options set as text
function ToText(opt: TInterfaceMethodOptions): ShortString; overload;


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


{ ************ Code/Documentation Generation Logic Extraction from RTTI }

type
  /// types recognized and handled for code/documentation generation
  TWrapperType = (
    wUnknown,
    wBoolean,
    wEnum,
    wSet,
    wByte,
    wWord,
    wInteger,
    wCardinal,
    wInt64,
    wQWord,
    wID,
    wReference,
    wTimeLog,
    wModTime,
    wCreateTime,
    wCurrency,
    wSingle,
    wDouble,
    wDateTime,
    wRawUtf8,
    wString,
    wRawJson,
    wBlob,
    wGuid,
    wCustomAnswer,
    wRecord,
    wArray,
    wVariant,
    wObject,
    wORM, // was wSQLRecord
    wInterface,
    wRecordVersion);

  /// supported languages typesets
  TWrapperLanguage = (
    lngDelphi,
    lngPascal,
    lngCS,
    lngJava,
    lngTypeScript,
    lngSwagger);

  EWrapperContext = class(ESynException);

  // used internally to extract info from RTTI - inherited in mormot.soa.codegen
  TWrapperContext = class
  protected
    fORM, fSOA, fRecords, fEnumerates, fSets, fArrays: TDocVariantData;
    fUnits, fDescriptions: TDocVariantData;
    fSourcePath: TFileNameDynArray;
    fNestedId: integer;     // for unique nested type names if no RTTI
    function CustomType(rtti: TRttiCustom): TWrapperType; virtual;
    function ContextFromRtti(typ: TWrapperType; rtti: TRttiCustom = nil;
      typName: RawUtf8 = ''; const parentName: RawUtf8 = ''): variant;
    function ContextNestedProperties(rtti: TRttiCustom;
      const parentName: RawUtf8): variant;
    function ContextOneProperty(const prop: TRttiCustomProp;
      const parentName: RawUtf8): variant;
    function ContextFromMethods(int: TInterfaceFactory): variant;
    function ContextFromMethod(const meth: TInterfaceMethod): variant;
    function ContextArgsFromMethod(const meth: TInterfaceMethod): variant;
    procedure AddUnit(const aUnitName: ShortString; addAsProperty: PVariant);
  public
    constructor Create(const aSourcePath, aDescriptions: TFileName);
    constructor CreateFromUsedInterfaces(
      const aSourcePath, aDescriptions: TFileName);
    function Context: variant; virtual;
  end;

const
  // Swagger numerical types
  SWI32 = '{"type":"integer"}';
  SWI64 = '{"type":"integer","format":"int64"}';
  SWD32 = '{"type":"number","format":"float"}';
  SWD64 = '{"type":"number","format":"double"}';
  // per-language type names
  TYPES_LANG: array[TWrapperLanguage, TWrapperType] of RawUtf8 = (
    // lngDelphi
    ('', 'Boolean', '', '', 'Byte', 'Word', 'Integer', 'Cardinal', 'Int64',
    'UInt64', 'TID', 'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime',
    'Currency', 'Single', 'Double', 'TDateTime', 'RawUtf8', 'String', 'RawJson',
    'RawBlob', 'TGuid', 'TServiceCustomAnswer', '', '', 'Variant', '', '', '',
    'TRecordVersion'),
   // lngPascal
    ('', 'Boolean', '', '', 'Byte', 'Word', 'Integer', 'Cardinal', 'Int64',
    'UInt64', 'TID', 'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime',
    'Currency', 'Single', 'Double', 'TDateTime', 'String', 'String', 'Variant',
    'RawBlob', 'TGuid', 'THttpBody', '', '', 'Variant', '', 'TID', '', 'TRecordVersion'),
   // lngCS
    ('', 'bool', '', '', 'byte', 'word', 'integer', 'uint', 'long', 'ulong',
    'TID', 'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime', 'decimal',
    'single', 'double', 'double', 'string', 'string', 'dynamic', 'byte[]',
    'Guid', 'byte[]', '', '', 'dynamic', '', 'TID', '', 'TRecordVersion'),
   // lngJava
    ('', 'boolean', '', '', 'byte', 'int', 'int', 'long', 'long', 'long', 'TID',
    'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime', 'BigDecimal',
    'single', 'double', 'double', 'String', 'String', 'Object', 'byte[]',
    'String', 'byte[]', '', '', 'Object', '', 'TID', '', 'TRecordVersion'),
   // lngTypeScript
    ('', 'boolean', '', '', 'number', 'number', 'number', 'number', 'number',
    'number', 'mORMot.TID', 'mORMot.TRecordReference', 'mORMot.TTimeLog',
    'mORMot.TModTime', 'mORMot.TCreateTime', 'number', 'number', 'number',
    'mORMot.TDateTime', 'string', 'string', 'any', 'mORMot.RawBlob',
    'mORMot.TGuid', 'mORMot.THttpBody', '', '', 'any', '', '', '',
    'mORMot.TRecordVersion'),
   // lngSwagger
    ('', '{"type":"boolean"}', '', '', SWI32, SWI32, SWI32, SWI32, SWI64, SWI64,
    SWI64, SWI64, SWI64, SWI64, SWI64, SWD64, SWD32, SWD64,
    '{"type":"string","format":"date-time"}', // wDateTime
    '{"type":"string"}', '{"type":"string"}', '{"type":"object"}', //FIXME! //wRawJson
    '{"type":"string","format":"binary"}', '{"type":"string"}', //wBlob,wGuid
    '', '', '', '', //wCustomAnswer, wRecord, wArray, wVariant
    '', SWI64, '', '' //wObject, wORM, wInterface, wRecordVersion
    ));

  TYPES_SOA: array[TInterfaceMethodValueType] of TWrapperType = (
    wUnknown,  // imvNone
    wUnknown,  // imvSelf
    wBoolean,  // imvBoolean
    wEnum,     // imvEnum
    wSet,      // imvSet
    wUnknown,  // imvInteger
    wUnknown,  // imvCardinal
    wUnknown,  // imvInt64
    wDouble,   // imvDouble
    wDateTime, // imvDateTime
    wCurrency, // imvCurrency
    wRawUtf8,  // imvRawUtf8
    wString,   // imvString
    wRawUtf8,  // imvRawByteString
    wRawUtf8,  // imvWideString
    wRecord,   // imvRecord
    wVariant,  // imvVariant
    wObject,   // imvObject
    wRawJson,  // imvRawJson
    wArray,    // imvDynArray
    wUnknown); // imvInterface
    // integers are wUnknown to force best type recognition

/// compute the SOA information, ready to be exported as JSON
// - will publish the ORM and SOA properties
// - to be used e.g. for client code generation via Mustache templates
function ContextFromUsedInterfaces(const aSourcePath: TFileName = '';
  const aDescriptions: TFileName = ''): variant;

/// compute the information of an interface method, ready to be exported as JSON
// - to be used e.g. for the implementation of the MVC controller via interfaces
// - no description text will be included - use ContextFromModel() if needed
function ContextFromMethod(const method: TInterfaceMethod): variant;

/// compute the information of an interface, ready to be exported as JSON
// - to be used e.g. for the implementation of the MVC controller via interfaces
// by TMvcApplication.GetMvcInfo from mormot.core.mvc
// - no description text will be included - use ContextFromModel() if needed
function ContextFromMethods(int: TInterfaceFactory): variant;


{ ************ Documentation Extraction from Source Code Comments }

/// rough parsing of the supplied .pas unit, adding the /// commentaries
// into a TDocVariant content
procedure FillDescriptionFromSource(var Descriptions: TDocVariantData;
  const SourceFileName: TFileName);

/// rough parsing of the supplied .pas unit, adding the /// commentaries
// into a compressed binary resource
// - could be then compiled into a WRAPPER_RESOURCENAME resource, e.g. via the
// following .rc source file, assuming ResourceDestFileName='wrapper.desc':
// $ WrappersDescription 10 "wrapper.desc"
// - you may specify a .json file name, for debugging/validation purposes
// - calls internally FillDescriptionFromSource
// - returns the TDocVariant JSON object corresponding to all decriptions
function ResourceDescriptionFromSource(const ResourceDestFileName: TFileName;
  const SourceFileNames: array of TFileName;
  const JsonDestFileName: TFileName = ''): variant;

var
  /// how FillDescriptionFromSource() handles trailing '-' in parsed comments
  // - default is [*], as expected by buggy AsciiDoc format
  DESCRIPTION_ITEM_PREFIX: RawUtf8 = ' [*]';

const
  /// internal Resource name used for bounded description
  // - as generated by FillDescriptionFromSource/ResourceDescriptionFromSource
  // - would be used e.g. by TWrapperContext.Create to inject the available
  // text description from any matching resource
  WRAPPER_RESOURCENAME = 'WrappersDescription';



implementation


{.$define SOA_DEBUG} // write the low-level interface info as json


{ ************ IInvokable Interface Methods and Parameters RTTI Extraction }

const
  // convert into generic in/out direction (assume result is out)
  ARGDIRTOJSON: array[TInterfaceMethodValueDirection] of string[4] = (
    'in', 'both', 'out', 'out');
  // normalize simple type names e.g. int64=qword or all strings to "utf8"
  // - note: AnsiString (Delphi <2009) may loose data depending on the client
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
    '',         // imvRecord
    'variant',  // imvVariant
    '',         // imvObject
    'json',     // imvRawJson
    '',         // imvDynArray
    '');        // imvInterface

procedure TInterfaceMethodArgument.SerializeToContract(WR: TJsonWriter);
begin
  WR.AddShort('{"argument":"');
  WR.AddShort(ParamName^);
  WR.AddShort('","direction":"');
  WR.AddShort(ARGDIRTOJSON[ValueDirection]);
  WR.AddShort('","type":"');
  if ARGTYPETOJSON[ValueType] = '' then
    WR.AddString(ArgRtti.Name) // use pascal type name
  else
    WR.AddShort(ARGTYPETOJSON[ValueType]); // normalized
  {$ifdef SOA_DEBUG}
  WR.AddDirect('"', ',');
  WR.AddPropInt64('index', IndexVar);
  WR.AddPropJsonString('var',
    GetEnumNameTrimed(TypeInfo(TInterfaceMethodValueVar), ValueVar));
  WR.AddPropInt64('stackoffset', InStackOffset);
  WR.AddPropInt64('reg', RegisterIdent);
  WR.AddPropInt64('fpreg', FPRegisterIdent);
  WR.AddPropInt64('stacksize', SizeInStack);
  WR.AddPropName('asm');
  WR.AddString(GetSetNameJsonArray(TypeInfo(TInterfaceMethodValueAsm), ValueKindAsm));
  WR.AddDirect('}', ',');
  {$else}
  WR.AddDirect('"', '}', ',');
  {$endif SOA_DEBUG}
end;

function TInterfaceMethodArgument.IsInput: boolean;
begin
  result := ValueDirection <= imdVar;
end;

function TInterfaceMethodArgument.IsOutput: boolean;
begin
  result := ValueDirection <> imdConst;
end;

function TInterfaceMethodArgument.SetFromJson(var Ctxt: TJsonParserContext;
  Method: PInterfaceMethod; V: pointer; Error: PShortString): boolean;
var
  tmp: ShortString;
begin
  ctxt.Info := ArgRtti;
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
      [Method^.InterfaceDotMethodName, ParamName^, ArgTypeName^], tmp);
    if Error = nil then
      EInterfaceFactory.RaiseUtf8('%', [tmp]);
    Error^ := tmp;
    result := false;
  end
  else
    result := true;
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

procedure TInterfaceMethodArgument.AddValueJson(WR: TJsonWriter;
  const Value: RawUtf8);
begin
  if rcfJsonString in ArgRtti.Flags then
  begin
    WR.Add('"');
    WR.AddJsonEscape(pointer(Value));
    WR.AddDirect('"', ',');
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
    imvObject,
    imvRawJson:
      WR.AddShorter('null,'); // may raise an error on client side for imvObject
    imvInterface:
      if vIsInterfaceJson in ValueKindAsm then // e.g. IDocList
        WR.AddShorter('null,')
      else
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
    if rcfJsonString in ArgRtti.Flags then
      WR.AddShorter('"",')
    else
      WR.AddShorter('0,');
  end;
end;

procedure TInterfaceMethodArgument.FixValueAndAddToObject(const Value: variant;
  var DestDoc: TDocVariantData);
var
  tempCopy: variant;
begin
  tempCopy := Value;
  FixValue(tempCopy);
  DestDoc.AddValueNameLen(@ParamName^[1], ord(ParamName^[0]), tempCopy);
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
      if VariantToInt64(Value, enum) then // from ordinal to PShortString
        Value := ArgRtti.Cache.EnumInfo.GetEnumNameOrd(enum)^;
    imvSet:
      if VariantToInt64(Value, enum) then // to TDocVariantData array
        Value := SetNameToVariant(enum, ArgRtti);
    imvObject:
      begin
        obj := ArgRtti.ClassNewInstance; // to TDocVariantData object
        try
          if DocVariantToObject(_Safe(Value)^, obj, ArgRtti) then
            Value := _ObjFast(obj, [woEnumSetsAsText]);
        finally
          obj.Free;
        end;
      end;
    imvDynArray:
      if _Safe(Value)^.IsArray then // to TDocVariantData array
      begin
        DocVariantType.ToJson(@Value, json);
        arr := nil; // recreate using a proper dynamic array
        dyn.InitRtti(ArgRtti, arr);
        try
          dyn.LoadFromJson(pointer(json));
          json := dyn.SaveToJson({EnumSetsAsText=}true);
          _Json(json, Value, JSON_FAST);
        finally
          dyn.Clear;
        end;
      end;
    imvRecord:
      if _Safe(Value)^.IsObject then // to TDocVariantData object
      begin
        DocVariantType.ToJson(@Value, json);
        SetLength(rec, ArgRtti.Size);
        try
          RecordLoadJsonInPlace(rec[0], pointer(json), ArgRtti.Info);
          json := SaveJson(rec[0], ArgRtti.Info, {EnumSetsAsText=}true);
          _Json(json, Value, JSON_FAST);
        finally
          ArgRtti.ValueFinalize(pointer(rec));
        end;
      end;
  end;
end;


{ TInterfaceMethod }

function TInterfaceMethod.ArgInput(ArgName: PUtf8Char; ArgNameLen: PtrInt;
  ArgIndex: PInteger): PInterfaceMethodArgument;
var
  a: PtrInt;
begin
  if ArgNameLen >= 0 then
  begin
    a := ArgsInFirst;
    result := @Args[a];
    while a <= ArgsInLast do
    begin
      if result^.IsInput and
         IdemPropNameNotNull(result^.ParamName^, ArgName, ArgNameLen) then
      begin
        if ArgIndex <> nil then
          ArgIndex^ := a;
        exit;
      end;
      inc(result);
      inc(a);
    end;
  end;
  result := nil;
end;

function TInterfaceMethod.ArgOutput(ArgName: PUtf8Char; ArgNameLen: PtrInt;
  ArgIndex: PInteger): PInterfaceMethodArgument;
var
  a: PtrInt;
begin
  if ArgNameLen >= 0 then
  begin
    a := ArgsOutFirst;
    result := @Args[a];
    while a <= ArgsOutLast do
    begin
      if result^.IsOutput and
         IdemPropNameNotNull(result^.ParamName^, ArgName, ArgNameLen) then
       begin
         if ArgIndex <> nil then
           ArgIndex^ := a;
         exit;
       end;
      inc(result);
      inc(a);
    end;
  end;
  result := nil;
end;

function TInterfaceMethod.ArgInputOutput(const ArgName: RawUtf8;
  Input: boolean): PInterfaceMethodArgument;
begin
  result := pointer(ArgName);
  if result <> nil then
    if Input then
      result := ArgInput(pointer(ArgName), PStrLen(PAnsiChar(result) - _STRLEN)^)
    else
      result := ArgOutput(pointer(ArgName), PStrLen(PAnsiChar(result) - _STRLEN)^);
end;

function TInterfaceMethod.ArgNext(var Arg: integer; Input: boolean): boolean;
begin
  result := true;
  inc(Arg);
  if Input then
    while Arg <= ArgsInLast do
      if Args[Arg].IsInput then
        exit
      else
        inc(Arg)
  else
    while Arg <= ArgsOutLast do
      if Args[Arg].IsOutput then
        exit
      else
        inc(Arg);
  result := false;
end;

function TInterfaceMethod.ArgsArrayToObject(P: PUtf8Char; Input: boolean): RawUtf8;
var
  i: integer;
  W: TJsonWriter;
  Value: PUtf8Char;
  a: PInterfaceMethodArgument;
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.AddDirect('{');
    if (P = nil) or
       (P^ <> '[') then
      P := nil
    else
      inc(P);
    a := pointer(Args);
    for i := 1 to length(Args) - 1 do
    begin
      if P = nil then
        break;
      inc(a);
      if Input then
      begin
        if not a^.IsInput then
          continue;
      end
      else if not a^.IsOutput then
        continue;
      W.AddPropName(a^.ParamName^);
      P := GotoNextNotSpace(P);
      Value := P;
      P := GotoEndJsonItem(P);
      if P = nil then
      begin
        W.AddNull; // malformatted input (or JSON_BIN_MAGIC_C)
        break;
      end;
      if P^ = ',' then
        inc(P); // include ending ','
      W.AddNoJsonEscape(Value, P - Value);
    end;
    W.CancelLastComma('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TInterfaceMethod.ArgsCommandLineToObject(P: PUtf8Char; Input: boolean;
  RaiseExceptionOnUnknownParam: boolean): RawUtf8;
var
  W: TJsonWriter;
  B: PUtf8Char;
  arginfo: PInterfaceMethodArgument;
  arg, value: RawUtf8;
  ok: boolean;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.AddDirect('{');
    while (P <> nil) and
          GetNextFieldProp(P, arg) and
          (P <> nil) and
          (arg <> '') do
    begin
      ok := true;
      arginfo := ArgInputOutput(arg, Input);
      if arginfo = nil then
        if RaiseExceptionOnUnknownParam then
          EInterfaceFactory.RaiseUtf8('Unexpected [%] parameter for %',
            [arg, InterfaceDotMethodName])
        else
          ok := false;
      if ok then
        W.AddPropName(arginfo^.ParamName^);
      if not (P^ in [':', '=']) then
        EInterfaceFactory.RaiseUtf8('"%" parameter has no = for %',
          [arg, InterfaceDotMethodName]);
      P := IgnoreAndGotoNextNotSpace(P);
      if P^ in ['"', '[', '{'] then
      begin
        // name='"value"' or name='{somejson}'
        B := P;
        P := GotoEndJsonItem(P);
        if P = nil then
          EInterfaceFactory.RaiseUtf8('%= parameter has invalid content for %',
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
          W.AddDirect('[');
        if (rcfJsonString in arginfo^.ArgRtti.Flags) or
           (vIsDynArrayString in arginfo^.ValueKindAsm) then
          W.AddJsonString(value)
        else
          W.AddShort(pointer(value), length(value));
        if arginfo^.ValueType = imvDynArray then
          W.AddDirect(']');
      end;
      W.AddComma;
    end;
    W.CancelLastComma('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TInterfaceMethod.ArgsStackAsDocVariant(Values: PPointerArray;
  out Dest: TDocVariantData; Input: boolean);
var
  a: PtrInt;
  arg: PInterfaceMethodArgument;
begin
  if Input then
  begin
    Dest.InitFast(ArgsInputValuesCount, dvObject);
    a := ArgsInFirst;
    arg := @Args[a];
    while a <= ArgsInLast do
    begin
      if arg^.IsInput then
        Dest.AddValueRtti(ArgsName[a], Values[a], arg^.ArgRtti);
      inc(arg);
      inc(a);
    end;
  end
  else
  begin
    Dest.InitFast(ArgsOutputValuesCount, dvObject);
    a := ArgsOutFirst;
    arg := @Args[a];
    while a <= ArgsOutLast do
    begin
      if arg^.IsOutput then
        Dest.AddValueRtti(ArgsName[a], Values[a], arg^.ArgRtti);
      inc(arg);
      inc(a);
    end;
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
        Dest.InitObjectFromVariants(ArgsInputName, Values, Options);
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
  arg: PInterfaceMethodArgument;
begin
  if (ArgsParams.Count = 0) or
     (ArgsParams.Kind <> dvArray) then
    exit;
  if ArgsObject.Kind = dvUndefined then
    ArgsObject.Init(ArgsParams.Options, dvObject);
  ArgsObject.Capacity := ArgsObject.Count + ArgsParams.Count;
  n := 0;
  if Input then
  begin
    if ArgsParams.Count <> integer(ArgsInputValuesCount) then
      exit;
    a := ArgsInFirst;
    arg := @Args[a];
    while a <= ArgsInLast do
    begin
      if arg^.IsInput then
      begin
        ArgsObject.AddValue(ArgsName[a], ArgsParams.Values[n]);
        inc(n);
      end;
      inc(arg);
      inc(a);
    end;
  end
  else
  begin
    if ArgsParams.Count <> integer(ArgsOutputValuesCount) then
      exit;
    a := ArgsOutFirst;
    arg := @Args[a];
    while a <= ArgsOutLast do
    begin
      if arg^.IsOutput then
      begin
        ArgsObject.AddValue(ArgsName[a], ArgsParams.Values[n]);
        inc(n);
      end;
      inc(arg);
      inc(a);
    end;
  end;
end;

procedure TInterfaceMethod.ArgsAsDocVariantFix(var ArgsObject: TDocVariantData;
  Input: boolean);
var
  a: PtrInt;
  arg: PInterfaceMethodArgument;
  doc: TDocVariantData;
begin
  if ArgsObject.Count > 0 then
    case ArgsObject.Kind of
      dvObject:
        for a := 0 to ArgsObject.Count - 1 do
        begin
          arg := ArgInputOutput(ArgsObject.Names[a], Input);
          if arg <> nil then
            arg^.FixValue(ArgsObject.Values[a]);
        end;
      dvArray:
        begin
          {%H-}doc.Init(ArgsObject.Options, dvObject);
          doc.Capacity := ArgsObject.Count;
          if Input then
          begin
            if ArgsObject.Count <> integer(ArgsInputValuesCount) then
              exit;
            a := ArgsInFirst;
            arg := @Args[a];
            while a <= ArgsInLast do
            begin
              if arg^.IsInput then
                arg^.FixValueAndAddToObject(ArgsObject.Values[doc.Count], doc);
              inc(arg);
              inc(a);
            end;
          end
          else
          begin
            if ArgsObject.Count <> integer(ArgsOutputValuesCount) then
              exit;
            a := ArgsOutFirst;
            arg := @Args[a];
            while a <= ArgsOutLast do
            begin
              if arg^.IsOutput then
                arg^.FixValueAndAddToObject(ArgsObject.Values[doc.Count], doc);
              inc(arg);
              inc(a);
            end;
          end;
          ArgsObject := doc;
        end;
    end;
end;

procedure TInterfaceMethod.ArgsClassNewInstance(V: PPPointer);
var
  a: PInterfaceMethodArgument;
  n: PtrInt;
begin
  n := ArgsManagedFirst;
  a := @Args[n];
  inc(V, n);
  n := ArgsUsedCount[imvvObject]; // caller ensured <> 0
  repeat
    if a^.ValueType = imvObject then
    begin
      V^^ := a^.ArgRtti.ClassNewInstance;
      dec(n);
      if n = 0 then
        exit;
    end;
    inc(V);
    inc(a);
  until false;
end;

procedure TInterfaceMethod.ArgsReleaseValues(V: PPointer);
var
  a: PInterfaceMethodArgument;
  n: PtrInt;
begin
  n := ArgsManagedFirst;
  a := @Args[n];
  inc(V, n);
  n := ArgsManagedCount; // caller ensured <> 0
  repeat
    if a^.ValueVar >=  imvvRawUtf8 then // match ArgsManagedCount definition
    begin
      a^.ArgRtti.ValueFinalize(V^);
      dec(n);
      if n = 0 then
        break;
    end;
    inc(a);
    inc(V);
  until false;
end;


{ ************ TInterfacedObjectFake with JITted Methods Execution }

{ TInterfacedObjectFakeRaw }

{$ifndef HASINTERFACEASTOBJECT} // for Delphi 7/2007
function _FakeObjectFromInterface(Stub: cardinal; Ptr: pointer): TObject;
begin
  result := nil;
  // recognize TInterfaceFactory.CreateFakeInstance() stub/mock
  if Stub = PCardinal(@TInterfacedObjectFakeRaw.FakeQueryInterface)^ then
    result := TInterfacedObjectFakeRaw(Ptr).SelfFromInterface;
end;
{$endif HASINTERFACEASTOBJECT}

constructor TInterfacedObjectFakeRaw.Create(aFactory: TInterfaceFactory);
begin
  inherited Create;
  fFactory := aFactory;
  fVTable := fFactory.GetMethodsVirtualTable;
  {$ifndef HASINTERFACEASTOBJECT} // Delphi 7/2007 specific code
  if not Assigned(@FakeObjectFromInterface) then
    FakeObjectFromInterface := @_FakeObjectFromInterface; // to recognize it
  {$endif HASINTERFACEASTOBJECT}
end;

procedure TInterfacedObjectFakeRaw.FakeCallRaiseError(
  var ctxt: TFakeCallContext; const Format: RawUtf8; const Args: array of const);
var
  msg: RawUtf8;
begin
  FormatUtf8(Format, Args, msg);
  EInterfaceFactory.RaiseUtf8('%.FakeCall(%.%) failed: %',
    [self, fFactory.fInterfaceName, ctxt.method^.Uri, msg]);
end;

procedure TInterfacedObjectFakeRaw.FakeCallGetParamsFromStack(
  var ctxt: TFakeCallContext);
var
  V: PPointer;
  a: PInterfaceMethodArgument;
  arg: integer;
begin
  FillCharFast(ctxt.I64s, ctxt.Method^.ArgsUsedCount[imvv64] * SizeOf(Int64), 0);
  a := pointer(ctxt.Method^.Args); // always <> nil
  for arg := 1 to PDALen(PAnsiChar(a) - _DALEN)^ + (_DAOFF - 1) do
  begin
    inc(a); // increase first, to ignore self
    V := nil;
    {$ifdef CPUX86}
    case a^.RegisterIdent of
      REGEAX:
        FakeCallRaiseError(ctxt, 'unexpected self', []);
      REGEDX:
        V := @ctxt.Stack.EDX;
      REGECX:
        V := @ctxt.Stack.ECX;
    else
    {$else}
    {$ifdef HAS_FPREG} // x64, armhf, aarch64
    if a^.FPRegisterIdent > 0 then
      V := @ctxt.Stack.FPRegs[a^.FPRegisterIdent + (FPREG_FIRST - 1)]
    else
    {$endif HAS_FPREG}
      if a^.RegisterIdent > 0 then
        V := @ctxt.Stack.ParamRegs[a^.RegisterIdent + (PARAMREG_FIRST - 1)];
    if a^.RegisterIdent = PARAMREG_FIRST then
      FakeCallRaiseError(ctxt, 'unexpected self', []);
    if V = nil then
    begin
    {$endif CPUX86}
      if vIsOnStack in a^.ValueKindAsm then
        V := @ctxt.Stack.Stack[a^.InStackOffset] // value is on stack
      else
        V := @ctxt.I64s[a^.IndexVar]; // for results in registers
    end;
    if vPassedByReference in a^.ValueKindAsm then
      V := PPointer(V)^;
    ctxt.Value[arg] := V;
  end;
  if imfResultIsServiceCustomAnswer in ctxt.Method^.Flags then
    ctxt.ServiceCustomAnswerPoint := ctxt.Value[ctxt.Method^.ArgsResultIndex]
  else
    ctxt.ServiceCustomAnswerPoint := nil;
end;

{$ifdef HASINLINE}
function TInterfacedObjectFakeRaw.SelfFromInterface: TInterfacedObjectFakeRaw;
begin
  // obfucated but very efficient once inlined
  result := pointer(PAnsiChar(self) - PAnsiChar(@TInterfacedObjectFakeRaw(nil).fVTable));
end;
{$else}
function TInterfacedObjectFakeRaw.SelfFromInterface: TInterfacedObjectFakeRaw;
asm
        // asm version for oldest Delphi 7
        sub     eax, TInterfacedObjectFake.fVTable
end;
{$endif HASINLINE}

procedure FakeCallRaise(Fake: TInterfacedObjectFakeRaw; MethodIndex: PtrUInt);
begin
  EInterfaceFactory.RaiseUtf8('%.FakeCall(%) failed: out of range %',
    [Fake, Fake.fFactory.fInterfaceName, MethodIndex]);
end;

function TInterfacedObjectFakeRaw.FakeCall(stack: PFakeCallStack): Int64;
var
  me: TInterfacedObjectFakeRaw; // self may be broken by compiler optimizations
  ctxt: TFakeCallContext;
begin
  (*
     WELCOME ABOARD: you just landed in TInterfacedObjectFake.FakeCall() !
     if your debugger reached here, you are executing a "fake" interface
     forged to call a remote SOA server or mock/stub an interface
  *)
  me := SelfFromInterface;
  // setup context
  ctxt.Stack := stack;
  if stack.MethodIndex >= PtrUInt(me.fFactory.MethodsCount) then
    FakeCallRaise(me, stack.MethodIndex);
  ctxt.Method := @me.fFactory.fMethods[stack.MethodIndex];
  ctxt.ResultType := imvNone;
  ctxt.Result := @result;
  // call main execution virtual method
  result := 0;
  me.FakeCallInternalProcess(ctxt);
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
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} IID: TGuid;
  out Obj): TIntQry;
var
  me: TInterfacedObjectFakeRaw; // self may be broken by compiler optimizations
begin
  me := SelfFromInterface;
  if IsEqualGuid(@IID, @me.fFactory.fInterfaceRtti.Cache.InterfaceGuid^) then
  begin
    pointer(Obj) := @me.fVTable;
    me._AddRef;
    result := S_OK;
  end
  else if me.GetInterface(IID, Obj) then
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
  fParams := TJsonWriter.CreateOwnedStream(8192, {nosharedstream=}true);
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
      C := PClass(E)^;
      if C.InheritsFrom(EInterfaceFactory) or
         (C = EAccessViolation) or
         (C = EInvalidPointer) then
        raise; // propagate only dangerous exceptions
    end;
  end;
  inherited Destroy;
  fParams.Free;
end;

procedure TInterfacedObjectFake.FakeCallGetJsonFromStack(
  var ctxt: TFakeCallContext; var Json: RawUtf8);
var
  W: TJsonWriter;
  wopt: TTextWriterOptions;
  oopt: TTextWriterWriteObjectOptions;
  arg: PtrInt;
  a: PInterfaceMethodArgument;
  V: PPointer;
begin
  // fill ctxt.Value[]
  FakeCallGetParamsFromStack(ctxt);
  // generate the ParamsJson input from ctxt.Value[]
  if fParamsSafe.TryLock then
  begin
    W := fParams; // reuse a per-callback TJsonWriter instance
    W.CancelAllAsNew;
  end
  else 
    // paranoid thread-safety call with its own temp buffer (hardly called)
    W := TJsonWriter.CreateOwnedStream(8192);
  try
    wopt := [twoForceJsonStandard]; // e.g. for AJAX
    if ifoJsonAsExtended in fOptions then
      wopt := [twoForceJsonExtended];
    if ifoDontStoreVoidJson in fOptions then
    begin
      oopt := DEFAULT_WRITEOPTIONS[true];
      include(wopt, twoIgnoreDefaultInRecord);
    end
    else
      oopt := DEFAULT_WRITEOPTIONS[false];
    W.CustomOptions := wopt;
    arg := ctxt.Method^.ArgsInFirst;
    a := @ctxt.Method^.Args[arg];
    while arg <= ctxt.Method^.ArgsInLast do
    begin
      if a^.IsInput then
      begin
        V := ctxt.Value[arg];
        if (a^.ValueType = imvInterface) and
           not (vIsInterfaceJson in a^.ValueKindAsm) then // e.g. not IDocList
          InterfaceWrite(W, ctxt.Method^, a^, V^)
        else
        begin
          a^.AddJson(W, V, oopt);
          W.AddComma;
        end;
      end;
      inc(a);
      inc(arg);
    end;
    W.CancelLastComma;
    W.SetText(Json); // without [ ]
  finally
    if W = fParams then
      fParamsSafe.UnLock
    else
      W.Free;
  end;
end;

procedure TInterfacedObjectFake.FakeCallSetJsonToStack(
  var ctxt: TFakeCallContext; R: PUtf8Char);
var
  arg: integer; // should be integer, not PtrInt
  V: PPointer;
  a: PInterfaceMethodArgument;
  asJsonObject: boolean;
  c: TJsonParserContext;
begin
  if ctxt.Method^.ArgsOutputValuesCount = 0 then
    exit;
  if R = nil then
    FakeCallRaiseError(ctxt, 'method returned value, but OutputJson=''''', []);
  if R^ in [#1..' '] then
    repeat
      inc(R)
    until not (R^ in [#1..' ']);
  asJsonObject := false; // [value,...] JSON array format
  if R^ <> '[' then
    if R^ = '{' then
      // {"paramname":value,...} JSON object format
      asJsonObject := true
    else
      FakeCallRaiseError(ctxt, 'JSON array/object result expected', []);
  c.InitParser(R + 1, nil, fFactory.JsonParserOptions, @fFactory.DocVariantOptions);
  arg := ctxt.Method^.ArgsOutFirst;
  a := @ctxt.Method^.Args[arg];
  repeat
    if asJsonObject then
    begin
      if not c.GetJsonFieldName then
        break; // end of JSON object
      if (arg = 0) or // arg := 0 below to force search
         // optimistic process of JSON object with in-order parameters
         not IdemPropName(a^.ParamName^, c.Value, c.ValueLen) then
      begin
        // slower but safe ctxt.Method when not in-order (unlikely)
        a := ctxt.Method^.ArgOutput(c.Value, c.ValueLen, @arg);
        if a = nil then
          FakeCallRaiseError(ctxt, 'unexpected parameter [%]', [c.Value]);
      end;
    end;
    V := ctxt.Value[arg];
    a^.SetFromJson(c, ctxt.Method, V, nil);
    if a^.ValueDirection = imdResult then
    begin
      ctxt.ResultType := a^.ValueType;
      if a^.ValueType in [imvBoolean..imvCurrency] then
        // ordinal/real result values to CPU/FPU registers
        MoveFast(V^, ctxt.Result^, a^.ArgRtti.Size);
    end;
    if c.Json = nil then
      break;
    c.Json := GotoNextNotSpace(c.Json);
    if asJsonObject then
    begin
      if c.Json^ in [#0, '}'] then
        break; // end of JSON object
      if arg = 0 then
        continue;
    end;
    repeat
      inc(arg);
      if arg > ctxt.Method^.ArgsOutLast then
      begin
        if not asJsonObject then
          exit;
        arg := 0;
        break;
      end;
      inc(a);
    until a^.IsOutput;
  until false;
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
  aParamValue: pointer);
begin
  EInterfaceFactory.RaiseUtf8('%: unhandled %.%(%: %) argument',
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
    [imvRawUtf8..imvWideString, imvDateTime];

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
    imvRecord,         //  ptGuid
    imvRecord,         //  ptHash128
    imvRecord,         //  ptHash256
    imvRecord,         //  ptHash512
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
    imvNone,           //  ptPUtf8Char
    imvNone);          //  ptCustom

var
  InterfaceFactoryCache: TSynObjectListLightLocked;

function FactorySearch(F: PInterfaceFactory; n: integer; nfo: PRttiInfo): TInterfaceFactory;
begin
  if n <> 0 then
    repeat
      result := F^;
      if result.fInterfaceRtti.Info = nfo then
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
    EInterfaceFactory.RaiseUtf8('%.Get(invalid)', [self]);
  cache := InterfaceFactoryCache;
  cache.Safe.ReadLock; // multiple reads lock
  result := FactorySearch(pointer(cache.List), cache.Count, aInterface);
  cache.Safe.ReadUnLock;
  if result <> nil then
    exit; // retrieved from cache
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
    EInterfaceFactory.RaiseUtf8('No RTTI available for I%: please ' +
      'define the methods using a TInterfaceFactoryGenerated wrapper',
      [aInterface^.RawName]);
    {$endif HASINTERFACERTTI}
  finally
    cache.Safe.WriteUnLock;
  end;
end;

function TInterfaceFactory.InterfaceGuid: PGuid;
begin
  result := fInterfaceRtti.Cache.InterfaceGuid;
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
  {$ifdef CPU64} gL, gH : QWord {$else} g: PHash128Rec {$endif}): TInterfaceFactory;
begin
  if n > 0 then
    repeat
      result := f^;
      with PHash128Rec(result.fInterfaceRtti.Cache.InterfaceGuid)^ do
        {$ifdef CPU64}
        if (L = gL) and
           (H = gH) then
        {$else}
        if (c0 = g^.c0) and
           (c1 = g^.c1) and
           (c2 = g^.c2) and
           (c3 = g^.c3) then
        {$endif CPU64}
          exit;
      inc(f);
      dec(n);
    until n = 0;
  result := nil;
end;

class function TInterfaceFactory.Get({$ifdef FPC_HAS_CONSTREF}constref{$else}
  const{$endif}aGuid: TGuid): TInterfaceFactory;
var
  cache: TSynObjectListLightLocked;
  h: THash128Rec absolute aGuid;
begin
  cache := InterfaceFactoryCache;
  if cache <> nil then
  begin
    cache.Safe.ReadLock; // no GPF expected within loop -> no try...finally
    result := FindGuid(pointer(cache.List), cache.Count,
                {$ifdef CPU64} h.L, h.H {$else} @h{$endif});
    cache.Safe.ReadUnLock;
  end
  else
    result := nil;
end;

class procedure TInterfaceFactory.AddToObjArray(var Obj: TInterfaceFactoryObjArray;
  const aGuids: array of TGuid);
var
  i: PtrInt;
  fac: TInterfaceFactory;
begin
  for i := 0 to high(aGuids) do
  begin
    fac := Get(aGuids[i]);
    if fac <> nil then
      PtrArrayAddOnce(Obj, fac);
  end;
end;

class function TInterfaceFactory.Guid2TypeInfo(
  const aGuids: array of TGuid): PRttiInfoDynArray;
var
  i: PtrInt;
begin
  result := nil;
  SetLength(result, length(aGuids));
  for i := 0 to high(aGuids) do
    result[i] := Guid2TypeInfo(aGuids[i]);
end;

class function TInterfaceFactory.Guid2TypeInfo(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} aGuid: TGuid): PRttiInfo;
var
  fact: TInterfaceFactory;
begin
  fact := Get(aGuid);
  if fact = nil then
    EInterfaceFactory.RaiseUtf8('%.Guid2TypeInfo(%): Interface not ' +
      'registered - use %.RegisterInterfaces()', [self, GuidToShort(aGuid), self]);
  result := fact.fInterfaceRtti.Info;
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

function RecordIsHfa(const Props: TRttiCustomProps): boolean;
var
  i: PtrInt;
begin
  result := false;
  if not (Props.Count in [1 .. 4]) then
    exit;
  for i := 0 to Props.Count - 1 do
    with Props.List[i] do
      if (Value.Cache.Kind <> rkFloat) or
         (Value.Cache.RttiFloat <> rfDouble) or
         (OffsetGet <> i * SizeOf(double)) then
        exit; // we only support records of packed double/TDateTime fields
  result := true;
end;

constructor TInterfaceFactory.Create(aInterface: PRttiInfo);
var
  nm, na, reg: integer;
  a: PInterfaceMethodArgument;
  m: PInterfaceMethod;
  WR: TJsonWriter;
  vt: TInterfaceMethodValueType;
  used: array[TInterfaceMethodValueType] of word;
  u: PRawUtf8;
  ErrorMsg: RawUtf8;
  {$ifdef HAS_FPREG}
  SizeInFPR: integer; // 0 if not in FPR, or the number of FPR involved
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
    EInterfaceFactory.RaiseUtf8('%.Create(nil)', [self]);
  if aInterface^.Kind <> rkInterface then
    EInterfaceFactory.RaiseUtf8('%.Create: % is not an interface',
      [self, aInterface^.RawName]);
  if IsNullGuid(aInterface^.InterfaceGuid^) then
    EInterfaceFactory.RaiseUtf8('%.Create: % has no GUID',
      [self, aInterface^.RawName]);
  fDocVariantOptions := JSON_FAST_FLOAT;
  fJsonParserOptions := JSONPARSER_SERVICE;
  fInterfaceRtti := Rtti.RegisterType(aInterface) as TRttiJson;
  PRttiCache(@fInterfaceRtti.Cache)^.InterfaceFactory := self; // fast lookup
  fInterfaceName := fInterfaceRtti.Name;
  fInterfaceUri := fInterfaceName;
  if fInterfaceUri[1] in ['i','I'] then
    // as in TServiceFactory.Create
    delete(fInterfaceUri, 1, 1);
  // retrieve all interface methods (recursively including ancestors)
  AddMethodsFromTypeInfo(aInterface); // from RTTI or generated code
  if fMethodsCount = 0 then
    EInterfaceFactory.RaiseUtf8('%.Create(%): interface has ' +
      'no RTTI - it should inherit from IInvokable or add some methods',
      [self, fInterfaceName]);
  if MethodsCount > MAX_METHOD_COUNT then
    EInterfaceFactory.RaiseUtf8(
      '%.Create(%): interface has too many methods (%), so breaks the ' +
      'Interface Segregation Principle and our internal buffers provision',
      [self, fInterfaceName, MethodsCount]);
  fMethodIndexCurrentFrameCallback := -1;
  fMethodIndexCallbackReleased := -1;
  SetLength(fMethods, MethodsCount);
  // compute additional information for each method
  FillCharFast(used, SizeOf(used), 0);
  m := pointer(fMethods);
  for nm := 0 to MethodsCount - 1 do
  begin
    // setup method information
    Join([fInterfaceUri, '.', m^.URI], m^.InterfaceDotMethodName);
    if m^.HierarchyLevel <> fAddMethodsLevel then
      include(m^.Flags, imfIsInherited);
    m^.ExecutionMethodIndex := nm + RESERVED_VTABLE_SLOTS;
    // first pass to recognize the parameters layout
    m^.ArgsInFirst := -1;
    m^.ArgsInLast := -2;
    m^.ArgsOutFirst := -1;
    m^.ArgsOutLast := -2;
    m^.ArgsNotResultLast := -2;
    m^.ArgsOutNotResultLast := -2;
    m^.ArgsResultIndex := -1;
    m^.ArgsManagedFirst := -1;
    a := pointer(m^.Args);
    a^.ValueType := imvSelf;
    inc(a);
    for na := 1 to length(m^.Args) - 1 do
    begin
      a^.ValueType := _FROM_RTTI[a^.ArgRtti.Parser];
      a^.ValueVar := ARGS_TO_VAR[a^.ValueType];
      ErrorMsg := ''; // seems supported
      inc(used[a^.ValueType]);
      case a^.ValueType of
        imvNone:
          case a^.ArgRtti.Info^.Kind of
            rkInteger:
              ErrorMsg := ' - use integer/cardinal instead';
            rkFloat:
              ErrorMsg := ' - use double/currency instead';
          else
            FormatUtf8(' (%)', [ToText(a^.ArgRtti.Info^.Kind)^], ErrorMsg);
          end;
        imvObject:
          if a^.ArgRtti.ValueRtlClass = vcList then
            ErrorMsg := ' - use TObjectList or T*ObjArray instead'
          else if (a^.ArgRtti.ValueRtlClass = vcCollection) and
                  (a^.ArgRtti.CollectionItem = nil) then
            ErrorMsg := ' - inherit from TInterfacedCollection or ' +
              'call Rtti.RegisterCollection() first'
          else if a^.ValueDirection = imdResult then
            ErrorMsg := ' - class not allowed as function result: ' +
              'use a var/out parameter';
        imvInterface:
          if Assigned(a^.ArgRtti.JsonWriter.Code) then
            include(a^.ValueKindAsm, vIsInterfaceJson) // e.g. IDocList
          else if a^.IsOutput then
            ErrorMsg := ' - interface not allowed as output: ' +
              'use a const parameter';
      end;
      if ErrorMsg <> '' then
        EInterfaceFactory.RaiseUtf8(
          '%.Create: %.% [%] parameter has unexpected type %%', [self,
          aInterface^.RawName, m^.URI, a^.ParamName^, a^.ArgRtti.Name, ErrorMsg]);
      if a^.ValueDirection = imdResult then
        m^.ArgsResultIndex := na
      else
      begin
        m^.ArgsNotResultLast := na;
        if a^.IsInput then
        begin
          inc(m^.ArgsInputValuesCount);
          if m^.ArgsInFirst < 0 then
            m^.ArgsInFirst := na;
          m^.ArgsInLast := na;
        end;
        if a^.IsOutput then
          m^.ArgsOutNotResultLast := na;
      end;
      if a^.IsOutput then
      begin
        if m^.ArgsOutFirst < 0 then
          m^.ArgsOutFirst := na;
        m^.ArgsOutLast := na;
        inc(m^.ArgsOutputValuesCount);
      end;
      if a^.ValueVar >= imvvRawUtf8 then
      begin
        if m^.ArgsManagedFirst < 0 then
          m^.ArgsManagedFirst := na;
        inc(m^.ArgsManagedCount);
      end;
      if rcfSpi in a^.ArgRtti.Flags then
        // as defined by Rtti.RegisterUnsafeSpiType()
        include(m^.HasSpiParams, a^.ValueDirection);
      inc(a);
    end;
    if m^.ArgsOutputValuesCount = 0 then
      // plain procedure with no out param -> recognize some known signatures
      case m^.ArgsInputValuesCount of
        1:
          if m^.Args[1].ValueType = imvBoolean then
            if PropNameEquals(m^.URI, 'CurrentFrame') then
              // procedure CurrentFrame(isLast: boolean);
              fMethodIndexCurrentFrameCallback := nm;
        2:
          if (m^.Args[1].ValueType = imvInterface) and
             (m^.Args[1].ArgRtti.Info = TypeInfo(IInvokable)) and
             (m^.Args[2].ValueType = imvRawUtf8) and
             PropNameEquals(m^.URI, 'CallbackReleased') then
// procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUtf8);
            fMethodIndexCallbackReleased := nm;
      end;
    if m^.ArgsResultIndex >= 0 then
    begin
      a := @m^.Args[m^.ArgsResultIndex];
      case a^.ValueType of
        imvNone,
        imvObject,
        imvInterface:
          EInterfaceFactory.RaiseUtf8('%.Create: I% unexpected result type %',
            [self, m^.InterfaceDotMethodName, a^.ArgTypeName^]);
        imvCardinal:
          if a^.ArgRtti.Info = TypeInfo(TServiceCustomStatus) then
            include(m^.Flags, imfResultIsServiceCustomStatus);
        imvRecord:
          if a^.ArgRtti.Info = TypeInfo(TServiceCustomAnswer) then
          begin
            for na := m^.ArgsOutFirst to m^.ArgsOutLast do
              if m^.Args[na].ValueDirection in [imdVar, imdOut] then
                EInterfaceFactory.RaiseUtf8('%.Create: I% var/out ' +
                  'parameter [%] not allowed with TServiceCustomAnswer result',
                  [self, m^.InterfaceDotMethodName, m^.Args[na].ParamName^]);
            include(m^.Flags, imfResultIsServiceCustomAnswer);
          end
        {$ifdef CPUAARCH64}
        // FPC uses registers for managed records, but follows the ABI otherwise
        // which requires the result to be in X8 which is not handled yet
        // - see aarch64/cpupara.pas: tcpuparamanager.create_paraloc_info_intern
        else if not (rcfIsManaged in a^.ArgRtti.Flags) then
          EInterfaceFactory.RaiseUtf8(
            '%.Create: I% record result type % is unsupported on aarch64:' +
            'use an OUT parameter instead, or include a managed field',
            [self, m^.InterfaceDotMethodName, a^.ArgTypeName^]);
        {$endif CPUAARCH64}
      end;
    end;
    if (m^.ArgsInputValuesCount = 1) and
       (m^.Args[1].ValueType = imvRawByteString) then
      include(m^.Flags, imfInputIsOctetStream);
    inc(m);
  end;
  for vt := low(vt) to high(vt) do
    SetLength(fArgUsed[vt], used[vt]);
  // compute asm low-level layout of the parameters for each method
  FillCharFast(used, SizeOf(used), 0); // used as fArgUsed[] index below
  m := pointer(fMethods);
  for nm := 0 to MethodsCount - 1 do
  begin
    // setup parameter names
    SetLength(m^.ArgsInputName, m^.ArgsInputValuesCount);
    u := pointer(m^.ArgsInputName);
    for na := m^.ArgsInFirst to m^.ArgsInLast do
      if m^.Args[na].IsInput then
      begin
        ShortStringToAnsi7String(m^.Args[na].ParamName^, u^);
        inc(u);
      end;
    SetLength(m^.ArgsOutputName, m^.ArgsOutputValuesCount);
    u := pointer(m^.ArgsOutputName);
    for na := m^.ArgsOutFirst to m^.ArgsOutLast do
      if m^.Args[na].IsOutput then
      begin
        ShortStringToAnsi7String(m^.Args[na].ParamName^, u^);
        inc(u);
      end;
    SetLength(m^.ArgsName, length(m^.Args));
    u := pointer(m^.ArgsName);
    // prepare stack and register layout
    reg := PARAMREG_FIRST;
    {$ifdef HAS_FPREG}
    {$ifdef OSPOSIX}
    fpreg := FPREG_FIRST;
    {$endif OSPOSIX}
    {$endif HAS_FPREG}
    a := pointer(m^.Args);
    for na := 0 to high(m^.Args) do
    begin
      ShortStringToAnsi7String(a^.ParamName^, u^);
      inc(u);
      if na <> 0 then
      begin
        with fArgUsed[a^.ValueType, used[a^.ValueType]] do
        begin
          MethodIndex := nm;
          ArgIndex := na;
        end;
        inc(used[a^.ValueType]);
      end;
      a^.RegisterIdent := 0;
      {$ifdef HAS_FPREG}
      a^.FPRegisterIdent := 0;
      SizeInFPR := 0;
      {$endif HAS_FPREG}
      a^.IndexVar := m^.ArgsUsedCount[a^.ValueVar];
      inc(m^.ArgsUsedCount[a^.ValueVar]);
      include(m^.ArgsUsed, a^.ValueType);
      if (a^.ValueType in [imvRecord, imvVariant]) or
         (a^.ValueDirection in [imdVar, imdOut]) or
         ((a^.ValueDirection = imdResult) and
          (a^.ValueType in ARGS_RESULT_BY_REF)) then
        include(a^.ValueKindAsm, vPassedByReference);
      case a^.ValueType of
        imvInteger,
        imvCardinal,
        imvInt64:
          if rcfQWord in a^.ArgRtti.Cache.Flags then
            include(a^.ValueKindAsm, vIsQword);
        {$ifdef HAS_FPREG}
        imvDouble,
        imvDateTime:
          if not (vPassedByReference in a^.ValueKindAsm) then
            SizeInFPR := 1; // stored in one double
        {$endif HAS_FPREG}
        imvDynArray:
          if (a^.ArgRtti.ArrayRtti <> nil) and
             ((rcfBinary in a^.ArgRtti.ArrayRtti.Flags) or
              (_FROM_RTTI[a^.ArgRtti.ArrayRtti.Parser] in _SMV_STRING)) then
            include(a^.ValueKindAsm, vIsDynArrayString);
        imvSet:
          if not (a^.ArgRtti.Size in [1, 2, 4, 8]) then
            EInterfaceFactory.RaiseUtf8(
              '%.Create: unexpected RTTI size = % in %.% method % parameter ' +
              'for % set - should match byte/word/integer/Int64 (1,2,4,8) sizes',
              [self, a^.ArgRtti.Size, fInterfaceName, m^.URI,
               a^.ParamName^, a^.ArgTypeName^]);
        imvRecord:
          begin
            if a^.ArgRtti.Size <= POINTERBYTES then
              // handle records only when passed by ref
              EInterfaceFactory.RaiseUtf8(
                '%.Create: % record too small in %.% method % parameter: it ' +
                'should be at least % bytes (i.e. bigger than a pointer) to be on stack',
                [self, a^.ArgTypeName^, fInterfaceName, m^.URI,
                 a^.ParamName^, POINTERBYTES + 1]);
              // to be fair, both WIN64ABI and SYSVABI could handle those and
              // transmit them within a register
            if RecordIsHfa(a^.ArgRtti.Props) then
            begin
              include(a^.ValueKindAsm, vIsHFA); // e.g. record x, y: double end;
              {$ifdef HAS_FPREG}
              SizeInFPR := a^.ArgRtti.Size shr 3;
              {$endif HAS_FPREG}
            end;
         end;
      end;
      a^.OffsetAsValue := m^.ArgsSizeAsValue;
      inc(m^.ArgsSizeAsValue, a^.ArgRtti.Size);
      while m^.ArgsSizeAsValue and 7 <> 0 do
        inc(m^.ArgsSizeAsValue); // align to 64-bit
      if a^.ValueDirection = imdResult then
      begin
        if not (a^.ValueType in ARGS_RESULT_BY_REF) then
        begin
          inc(a);
          continue; // ordinal/real/class results are returned in CPU/FPU registers
        end;
        {$ifndef CPUX86}
        a^.InStackOffset := -1;
        a^.RegisterIdent := PARAMREG_RESULT;
        inc(a);
        continue;
        {$endif CPUX86}
        // CPUX86 will add an additional by-ref parameter
      end;
      {$ifdef CPU32}
      if a^.ValueDirection = imdConst then
        a^.SizeInStack := ARGS_IN_STACK_SIZE[a^.ValueType]
      else
      {$endif CPU32}
        a^.SizeInStack := POINTERBYTES; // always 8 bytes aligned on 64-bit
      if
        {$ifndef CPUARM}
        // on ARM, ordinals>POINTERBYTES can also be placed in the normal registers !!
        (a^.SizeInStack <> POINTERBYTES) or
        {$endif CPUARM}
        {$ifdef HAS_FPREG}
        {$ifdef OSPOSIX}  // Linux x64, armhf, aarch64
        ((SizeInFPR = 1) and (fpreg > FPREG_LAST)) or // too many FP registers
        ((SizeInFPR = 0) and (reg > PARAMREG_LAST))  // too many int registers
        {$else}
        (reg > PARAMREG_LAST) // Win64: XMMs overlap regular registers
        {$endif OSPOSIX}
        {$else}
        (reg > PARAMREG_LAST) // Win32, Linux x86, armel
        {$endif HAS_FPREG}
        {$ifdef FPC}
        or ((a^.ValueType in [imvRecord]) and
          // trunk i386/x86_64\cpupara.pas: DynArray const is passed as register
           not (vPassedByReference in a^.ValueKindAsm))
        {$endif FPC} then
      begin
        // this parameter will go on the stack
        {$ifdef OSDARWINARM}
        // the Mac M1 does NOT follow the ARM ABI standard on stack :(
        // https://developer.apple.com/documentation/xcode/
        //    writing-arm64-code-for-apple-platforms#Pass-arguments-to-functions-correctly
        // "arguments may consume slots on the stack that are not multiples of 8 bytes"
        if a^.ValueDirection = imdConst then
          a^.SizeInStack := a^.ArgRtti.Size;
        {$else}
        {$ifdef CPUARM}
        // parameter must be aligned on a SizeInStack boundary
        if a^.SizeInStack > POINTERBYTES then
          inc(m^.ArgsSizeInStack, m^.ArgsSizeInStack mod cardinal(a^.SizeInStack));
        {$endif CPUARM}
        {$endif OSDARWINARM}
        a^.InStackOffset := m^.ArgsSizeInStack;
        inc(m^.ArgsSizeInStack, a^.SizeInStack);
      end
      else
      begin
        // this parameter will go in a register
        a^.InStackOffset := -1;
        {$ifndef CPUX86}
        if (m^.ArgsResultIndex >= 0) and
           (reg = PARAMREG_RESULT) and
           (m^.Args[m^.ArgsResultIndex].ValueType in ARGS_RESULT_BY_REF) then
          inc(reg); // this register is reserved for method result pointer
        {$endif CPUX86}
        {$ifdef HAS_FPREG}
        if SizeInFPR = 1 then
        begin
          // put in next floating-point register
          {$ifdef OSPOSIX}
          a^.FPRegisterIdent := fpreg; // SYSVABI has its own FP registers index
          inc(fpreg);
          {$else}
          a^.FPRegisterIdent := reg; // Win64 ABI: reg and fpreg do overlap
          inc(reg);
          {$endif OSPOSIX}
        end
        else
        {$endif HAS_FPREG}
        begin
          // put in an integer register
          {$ifdef CPUARM}
          // on 32-bit ARM, ordinals>POINTERBYTES are also placed in registers
          if (a^.SizeInStack > POINTERBYTES) and
             ((reg and 1) = 0) then
            inc(reg); // must be aligned on even boundary
          // check if we have still enough registers, after previous increments
          if ((PARAMREG_LAST - reg + 1) * POINTERBYTES) < a^.SizeInStack then
          begin
            // no space, put on stack
            a^.InStackOffset := m^.ArgsSizeInStack;
            inc(m^.ArgsSizeInStack, a^.SizeInStack);
            // all params following the current one, must also be placed on stack
            reg := PARAMREG_LAST + 1;
            inc(a);
            continue;
          end;
          a^.RegisterIdent := reg;
          if a^.SizeInStack > POINTERBYTES then
            inc(reg, a^.SizeInStack shr POINTERSHR)
          else
            inc(reg);
          {$else}
          a^.RegisterIdent := reg;
          inc(reg);
          {$endif CPUARM}
        end;
      end;
      inc(a);
    end;
    // pre-compute the TInterfaceMethodExecuteRaw.RawExecute expectations
    a := pointer(m^.Args);
    for na := 0 to high(m^.Args) do
    begin
      {$ifdef HAS_FPREG}
      if a^.FPRegisterIdent > 0 then
        if (a^.RegisterIdent > 0) or
           (vPassedByReference in a^.ValueKindAsm) then
          EInterfaceFactory.RaiseUtf8('Unexpected I% % Reg=% FPReg=%',
            [m^.InterfaceDotMethodName, a^.ParamName^,
             a^.RegisterIdent, a^.FPRegisterIdent]);
      {$endif HAS_FPREG}
      if (a^.RegisterIdent = 0) and
         (a^.FPRegisterIdent = 0) and
         (a^.SizeInStack > 0) then
        include(a^.ValueKindAsm, vIsOnStack);
      if vPassedByReference in a^.ValueKindAsm then
        if vIsOnStack in a^.ValueKindAsm then
          if a^.SizeInStack <> POINTERBYTES then
            EInterfaceFactory.RaiseUtf8('Unexpected I% % ref with no pointer',
              [m^.InterfaceDotMethodName, a^.ParamName^])
          else
            a^.RawExecute := reRefStack
        else if a^.RegisterIdent > 0 then
          a^.RawExecute := reRefReg
        else
          EInterfaceFactory.RaiseUtf8('Unexpected I% % reference with no slot',
            [m^.InterfaceDotMethodName, a^.ParamName^])
      else // pass by value
        if vIsOnStack in a^.ValueKindAsm then
          a^.RawExecute := reValStack
        else if a^.RegisterIdent > 0 then
          if a^.SizeInStack = POINTERBYTES then
            a^.RawExecute := reValReg   // use a single register
          else
            a^.RawExecute := reValRegs  // several registers (e.g. SYSVABI TGuid)
        {$ifdef HAS_FPREG}
        else if a^.FPRegisterIdent > 0 then
          if a^.SizeInStack = SizeOf(double) then
            a^.RawExecute := reValFpReg  // use a single FP register
          else
            a^.RawExecute := reValFpRegs // several FP registers (e.g. SYSVABI HFA)
        {$endif HAS_FPREG}
        else
          a^.RawExecute := reNone; // e.g. for a result register
      inc(a);
    end;
    {$ifdef OSDARWINARM}
    // the Mac M1 does NOT follow the ARM ABI standard on stack :(
    while m^.ArgsSizeInStack and 7 <> 0 do
      inc(m^.ArgsSizeInStack); // ensure pointer-aligned
    {$endif OSDARWINARM}
    if m^.ArgsSizeInStack > MAX_EXECSTACK then
      EInterfaceFactory.RaiseUtf8(
        '%.Create: Stack size % > % for %.% method % parameters',
        [self, m^.ArgsSizeInStack, MAX_EXECSTACK, fInterfaceName, m^.URI,
         m^.ArgsInputValuesCount]);
    {$ifdef CPUX86}
    // pascal/register convention are passed left-to-right -> reverse order
    offs := m^.ArgsSizeInStack;
    a := pointer(m^.Args);
    for na := 0 to high(m^.Args) do
    begin
      if a^.InStackOffset >= 0 then
      begin
        dec(offs, a^.SizeInStack);
        a^.InStackOffset := offs;
      end;
      inc(a);
    end;
    //assert(offs=0);
    {$endif CPUX86}
    inc(m);
  end;
  WR := TJsonWriter.CreateOwnedStream;
  try
    // compute the default results JSON array for all methods
    m := pointer(fMethods);
    for nm := 0 to MethodsCount - 1 do
    begin
      WR.CancelAll;
      WR.AddDirect('[');
      for na := m^.ArgsOutFirst to m^.ArgsOutLast do
        with m^.Args[na] do
          if IsOutput then
            AddDefaultJson(WR);
      WR.CancelLastComma(']');
      WR.SetText(m^.DefaultResult);
      inc(m);
    end;
    // compute the service contract as a JSON array
    WR.CancelAll;
    WR.AddDirect('[');
    m := pointer(fMethods);
    for nm := 0 to MethodsCount - 1 do
    begin
      WR.Add('{"method":"%","arguments":[', [m^.URI]);
      for na := 0 to High(m^.Args) do
        m^.Args[na].SerializeToContract(WR);
      WR.CancelLastComma;
      WR.AddDirect(']', '}', ',');
      inc(m);
    end;
    WR.CancelLastComma(']');
    WR.SetText(fContract);
    {$ifdef SOA_DEBUG}
    JsonReformatToFile(fContract,TFileName(fInterfaceName + '-' +
      COMP_TEXT + OS_TEXT + CPU_ARCH_TEXT + '.json'));
    {$endif SOA_DEBUG}
  finally
    WR.Free;
  end;
end;

function FastFindName(m: PInterfaceMethod; pn: PUtf8Char; n: PtrInt): PtrInt;
var
  pm: PUtf8Char;
  lm, ln, alt: PtrInt;
begin // very efficient O(n) search sub-function
  alt := -1;
  result := 0;
  ln := PStrLen(pn - _STRLEN)^;
  repeat
    pm := pointer(m^.Uri);              // method name
    lm := PStrLen(pm - _STRLEN)^ - ln;  // method name length
    if lm = 0 then // same length
    begin
      if IdemPropNameUSameLenNotNull(pm, pn, ln) then // inlined on FPC
        exit;
    end
    else if pm^ = '_' then // IService._Start() will match /service/start
    begin
      dec(lm);
      if lm = 0 then
        if IdemPropNameUSameLenNotNull(pm + 1, pn, ln) then // inlined on FPC
          alt := result;
    end;
    inc(m);
    inc(result);
  until result = n;
  result := alt; // use IServer._Method if no IServer.Method
end;

function TInterfaceFactory.FindMethodIndex(const aUrl: RawUtf8): PtrInt;
begin
  // called e.g. at startup, or by TServiceFactoryClient or TRestServerRoutingJsonRpc
  if (self <> nil) and
     (aUrl<> '') and
     (fMethodsCount <> 0) then
    result := FastFindName(pointer(fMethods), pointer(aUrl), fMethodsCount)
  else
    result := -1
end;

function TInterfaceFactory.FindMethodIndexExact(const aMethodName: RawUtf8): PtrInt;
begin
  for result := 0 to fMethodsCount - 1 do // no need to be fast (seldom called)
    if IdemPropNameU(fMethods[result].Uri, aMethodName) then
      exit;
  result := -1;
end;

function TInterfaceFactory.FindMethod(const aUrl: RawUtf8): PInterfaceMethod;
var
  i: PtrInt;
begin // this method is not called by the framework in normal use
  i := FindMethodIndex(aUrl);
  if i < 0 then
    result := nil
  else
    result := @fMethods[i];
end;

function TInterfaceFactory.FindFullMethodIndex(const aFullMethodName: RawUtf8;
  alsoSearchExactMethodName: boolean): integer;
begin
  if PosExChar('.', aFullMethodName) <> 0 then
    for result := 0 to fMethodsCount - 1 do
      if IdemPropNameU(fMethods[result].InterfaceDotMethodName, aFullMethodName) then
        exit;
  if alsoSearchExactMethodName then
    result := FindMethodIndexExact(aFullMethodName)
  else
    result := -1;
end;

function TInterfaceFactory.CheckMethodIndex(const aUrl: RawUtf8): PtrInt;
begin
  if self = nil then
    raise EInterfaceFactory.Create('TInterfaceFactory(nil).CheckMethodIndex');
  result := FindMethodIndex(aUrl);
  if result < 0 then
    EInterfaceFactory.RaiseUtf8('%.CheckMethodIndex: %.% not found',
      [self, fInterfaceName, aUrl]);
end;

function TInterfaceFactory.CheckMethodIndex(aUrl: PUtf8Char): integer;
begin
  result := CheckMethodIndex(RawUtf8(aUrl));
end;

procedure TInterfaceFactory.CheckMethodIndexes(const aUrl: array of RawUtf8;
  aSetAllIfNone: boolean; out aBits: TInterfaceFactoryMethodBits);
var
  i: PtrInt;
begin
  if aSetAllIfNone and
     (high(aUrl) < 0) then
  begin
    FillCharFast(aBits, SizeOf(aBits), 255);
    exit;
  end;
  FillCharFast(aBits, SizeOf(aBits), 0);
  for i := 0 to high(aUrl) do
    include(aBits, CheckMethodIndex(aUrl[i]));
end;

function TInterfaceFactory.GetMethodName(aMethodIndex: integer): RawUtf8;
begin
  if (aMethodIndex < 0) or
     (self = nil) then
    result := ''
  else if aMethodIndex < SERVICE_PSEUDO_METHOD_COUNT then
    result := SERVICE_PSEUDO_METHOD[TServiceInternalMethod(aMethodIndex)]
  else
  begin
    dec(aMethodIndex, SERVICE_PSEUDO_METHOD_COUNT);
    if cardinal(aMethodIndex) < cardinal(fMethodsCount) then
      result := fMethods[aMethodIndex].Uri
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
      result := Join([fInterfaceName, '.', result]);
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
        {$ifdef ISDELPHI}
        // FakeCall(self: TInterfacedObjectFake; var aCall: TFakeCallStack): Int64
        // So, make space for two variables (+shadow space)
        // adds $50 to stack, so rcx .. at rpb+$10+$50 = rpb+$60
       .params 2
        {$endif ISDELPHI}
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
        {$ifdef ISDELPHI}
        mov     [rbp + $60], rcx
        mov     [rbp + $68], rdx
        mov     [rbp + $70], r8
        mov     [rbp + $78], r9
        {$else}
        mov     qword ptr [rbp + $10], rcx
        mov     qword ptr [rbp + $18], rdx
        mov     qword ptr [rbp + $20], r8
        mov     qword ptr [rbp + $28], r9
        {$endif ISDELPHI}
        lea     rdx, sxmm0 // TFakeCallStack address as 2nd parameter
        {$endif OSPOSIX}
        call    TInterfacedObjectFakeRaw.FakeCall
        // FakeCall should set rax: Int64 in method result,
        // and float in aCall.FPRegs["XMM0"]
        movsd   xmm0, qword ptr sxmm0 // movsd for zero extension
end;

{$endif CPUX64}

var
  // just called once for _FAKEVMT creation (once per interface type on i386)
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
      fFakeVTable[0] := @TInterfacedObjectFakeRaw.FakeQueryInterface;
      fFakeVTable[1] := @TInterfacedObjectFakeRaw.Fake_AddRef;
      fFakeVTable[2] := @TInterfacedObjectFakeRaw.Fake_Release;
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
          {$ifdef OSPOSIX}        // align stack by 16 bytes
          P^ := $e8505050;        // push eax; push eax; push eax (align stack)
          inc(P);                 // call FakeCall
          {$else}
          PByte(P)^ := $e8;
          inc(PByte(P));          // call FakeCall
          {$endif OSPOSIX}
          P^ := PtrUInt(@TInterfacedObjectFakeRaw.FakeCall) - PtrUInt(P) - 4;
          inc(P);
          P^ := $c25dec89;        // mov esp, ebp; pop ebp; ret {StackSize}
          inc(PByte(P), 3);       // overlap c2=ret to avoid GPF
          P^ := (fMethods[i].ArgsSizeInStack shl 8) or $900000c2;
          inc(P);
          {$ifdef OSPOSIX}        // align code by 4 bytes
          inc(PByte(P));
          {$endif OSPOSIX}        // VMTSTUBSIZE = 24 (OSPOSIX: + 4 )
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
  _FAKEVMT: TPointerDynArray;

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
    {$ifdef CPUAARCH64} + ($120 shr 2) {$endif CPUAARCH64}
    {$ifdef CPUARM}, @TInterfacedObjectFakeRaw.ArmFakeStub {$endif CPUARM});
  // populate _FAKEVMT[] with JITted stubs
  SetLength(_FAKEVMT, MAX_METHOD_COUNT + RESERVED_VTABLE_SLOTS);
  // set IInterface RESERVED_VTABLE_SLOTS required methods
  _FAKEVMT[0] := @TInterfacedObjectFakeRaw.FakeQueryInterface;
  _FAKEVMT[1] := @TInterfacedObjectFakeRaw.Fake_AddRef;
  _FAKEVMT[2] := @TInterfacedObjectFakeRaw.Fake_Release;
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
    inc(PByte(P), 5);  // VMTSTUBSIZE = 24
    {$endif CPUX64}
    {$ifdef CPUARM}
    {$ifdef ASMORIG}
    P^ := ($e3a040 shl 8) + i;
    inc(P); // mov r4 (v1),{MethodIndex} : store method index in register
    {$else}
    P^ := ($e3a0c0 shl 8) + cardinal(i);
    inc(P); // mov r12 (ip),{MethodIndex} : store method index in register
    {$endif ASMORIG}
    // branch ArmFakeStub (24bit relative, word aligned)
    stub :=
      ((PtrUInt(@TInterfacedObjectFakeRaw.ArmFakeStub) - PtrUInt(P)) shr 2) - 2;
    P^ := ($ea shl 24) + (stub and $00ffffff); // note: stub may be < 0
    inc(P);
    P^ := $e320f000;  // VMTSTUBSIZE = 16
    inc(P);
    {$endif CPUARM}
    {$ifdef CPUAARCH64}
    // store method index in register r16 [IP0]
    // $10 = r16 ... loop to $1F -> number shifted * $20
    P^ := ($d280 shl 16) + (i shl 5) + $10;
    inc(P);  // mov r16 ,{MethodIndex}
    // we are using a register branch here
    // fill register x10 with address
    stub := PtrUInt(@TInterfacedObjectFakeRaw.AArch64FakeStub);
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
    inc(P); // VMTSTUBSIZE = 28
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
  if (fMethods <> nil) or
     (fMethodsCount <> 0) or
     (nm > 255) then
    EInterfaceFactory.RaiseUtf8('%.AddMethodsFromTypeInfo(%)', [self, info.Name]);
  if nm = 0 then
    exit;
  SetLength(fMethods, nm);
  sm := pointer(fMethods);
  m := pointer(info.Methods);
  repeat
    if FindMethodIndexExact(m^.Name) >= 0 then
      EInterfaceFactory.RaiseUtf8('%.AddMethodsFromTypeInfo: duplicated %.%',
        [self, info.Name, m^.Name]);
    sm^.Uri := m^.Name;
    sm^.HierarchyLevel := m^.HierarchyLevel;
    na := length(m^.Args);
    SetLength(sm^.Args, na);
    sa := pointer(sm^.Args);
    a := pointer(m^.Args);
    while na > 0 do
    begin
      sa^.ParamName := a^.ParamName;
      sa^.ArgTypeName := a^.TypeName;
      if a^.TypeInfo = nil then // happens e.g. for enumerates with values
        EInterfaceFactory.RaiseUtf8(
          '%.AddMethodsFromTypeInfo: parameter %: % in method %.% has no RTTI',
          [self, a^.ParamName^, a^.TypeName^, info.Name, m^.Name]);
      sa^.ArgRtti := Rtti.RegisterType(a^.TypeInfo) as TRttiJson;
      sa^.ValueDirection := TInterfaceMethodValueDirection(a^.Direction);
      inc(sa);
      inc(a);
      dec(na);
    end;
    inc(m);
    inc(sm);
    inc(fMethodsCount); // update one by one for FindMethodIndexExact() above
    dec(nm);
  until nm = 0;
end;

{$endif HASINTERFACERTTI}


{ TInterfaceFactoryGenerated }

procedure TInterfaceFactoryGenerated.AddMethod(const aName: RawUtf8;
  const aParams: array of const);
var
  meth: PInterfaceMethod;
  arg: PInterfaceMethodArgument;
  par: PVarRecArray; // aParams = [ ord(Direction),'n1',TypeInfo(integer), ... ]
  na, ns, a: PtrInt;
  u: RawUtf8;
begin
  if Length(aParams) mod 3 <> 0 then
    EInterfaceFactory.RaiseUtf8('%: invalid aParams count=% for %.AddMethod("%")',
      [fInterfaceName, Length(aParams), self, aName]);
  if FindMethodIndexExact(aName) >= 0 then
    EInterfaceFactory.RaiseUtf8('%.AddMethod: duplicated generated name %.%',
      [self, fInterfaceName, aName]);
  if fMethodsCount > MAX_METHOD_COUNT then
    exit; // caller would raise exception, but not exceed 255
  SetLength(fMethods, fMethodsCount + 1);
  meth := @fMethods[fMethodsCount];
  inc(fMethodsCount);
  meth^.Uri := aName;
  na := length(aParams) div 3;
  SetLength(meth^.Args, na + 1); // always include Args[0]=self
  arg := pointer(meth^.Args);
  arg^.ParamName := @PSEUDO_SELF_NAME;
  arg^.ArgRtti := fInterfaceRtti;
  arg^.ArgTypeName := @fInterfaceRtti.Info^.RawName;
  ns := length(fTempStrings);
  SetLength(fTempStrings, ns + na);
  par := @aParams[0];
  for a := 0 to na - 1 do
  begin
    inc(arg);
    if par[0].VType <> vtInteger then
      EInterfaceFactory.RaiseUtf8('%: invalid param type #% for %.AddMethod("%")',
        [fInterfaceName, a, self, aName]);
    arg^.ValueDirection := TInterfaceMethodValueDirection(par[0].VInteger);
    VarRecToUtf8(@par[1], u);
    if u = '' then
      EInterfaceFactory.RaiseUtf8('%: invalid param name #% for %.AddMethod("%")',
        [fInterfaceName, a, self, aName]);
    insert(AnsiChar(Length(u)), u, 1); // create fake PShortString
    arg^.ParamName := pointer(u);
    fTempStrings[ns + a] := u;
    if par[2].VType <> vtPointer then
      EInterfaceFactory.RaiseUtf8('%: expect TypeInfo() at #% for %.AddMethod("%")',
        [fInterfaceName, a, self, aName]);
    arg^.ArgRtti := Rtti.RegisterType(par[2].VPointer) as TRttiJson;
    arg^.ArgTypeName := arg^.ArgRtti.Info^.Name;
    par := @par[3]; // next argument tripple
  end;
end;

class procedure TInterfaceFactoryGenerated.RegisterInterface(aInterface: PRttiInfo);
var
  cache: TSynObjectListLightLocked;
begin
  if (aInterface = nil) or
     (self = TInterfaceFactoryGenerated) then
    EInterfaceFactory.RaiseUtf8('%.RegisterInterface(nil)', [self]);
  cache := InterfaceFactoryCache;
  cache.Safe.WriteLock;
  try
    if FactorySearch(pointer(cache.List), cache.Count, aInterface) <> nil then
      EInterfaceFactory.RaiseUtf8('Duplicated %.RegisterInterface(%)',
        [self, aInterface^.RawName]);
    cache.Add(Create(aInterface));
  finally
    cache.Safe.WriteUnLock;
  end;
end;


function ToText({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  aGuid: TGuid): ShortString;
var
  fact: TInterfaceFactory;
begin
  fact := TInterfaceFactory.Get(aGuid);
  if fact = nil then
    GuidToShort(aGuid, PShortGuid(@result)^)
  else
    result := fact.fInterfaceRtti.Info^.RawName;
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

function TInterfaceResolver.Resolve({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  aGuid: TGuid; out Obj; aRaiseIfNotFound: ESynExceptionClass): boolean;
var
  known: TInterfaceFactory;
begin
  result := false;
  if self <> nil then
  begin
    known := TInterfaceFactory.Get(aGuid);
    if known <> nil then
      result := TryResolve(known.fInterfaceRtti.Info, Obj);
  end;
  if (aRaiseIfNotFound <> nil) and
     not result then
    aRaiseIfNotFound.RaiseUtf8('%.Resolve(%) unsatisfied',
      [self, GuidToShort(aGuid)]);
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
    EInterfaceResolver.RaiseUtf8('%.Resolve([odd])', [self]);
  for i := 0 to (n shr 1) - 1 do
    if not TryResolve(aInterfaceObjPairs[i * 2], aInterfaceObjPairs[i * 2 + 1]^) then
      if aRaiseExceptionIfNotFound then
        EInterfaceResolver.RaiseUtf8('%.ResolveByPair(%) unsatisfied',
          [self, PRttiInfo(aInterfaceObjPairs[i * 2])^.RawName]);
end;

procedure TInterfaceResolver.Resolve(const aInterfaces: array of TGuid;
  const aObjs: array of pointer; aRaiseExceptionIfNotFound: boolean);
var
  n, i: PtrInt;
  info: PRttiInfo;
begin
  n := length(aInterfaces);
  if (self = nil) or
     (n = 0) or
     (n <> length(aObjs)) then
    EInterfaceResolver.RaiseUtf8('%.Resolve([?,?])', [self]);
  for i := 0 to n - 1 do
    if PPointer(aObjs[i])^ = nil then
    begin
      info := TInterfaceFactory.Guid2TypeInfo(aInterfaces[i]);
      if not TryResolve(info, aObjs[i]^) then
        if aRaiseExceptionIfNotFound then
          EInterfaceResolver.RaiseUtf8('%.Resolve(%) unsatisfied',
            [self, info^.RawName]);
    end;
end;

{ TInterfaceResolverForSingleInterface }

constructor TInterfaceResolverForSingleInterface.Create(aInterface: PRttiInfo;
  aImplementation: TInterfacedObjectClass);
var
  guid: PGuid;
begin
  fInterfaceTypeInfo := aInterface;
  guid := aInterface^.InterfaceGuid;
  if guid = nil then
    EInterfaceResolver.RaiseUtf8('%.Create expects an Interface', [self]);
  fImplementationEntry := aImplementation.GetInterfaceEntry(guid^);
  if fImplementationEntry = nil then
    EInterfaceResolver.RaiseUtf8('%.Create: % does not implement %',
      [self, aImplementation, fInterfaceTypeInfo^.RawName]);
  aInterface^.InterfaceAncestors(fInterfaceAncestors, aImplementation,
    fInterfaceAncestorsImplementationEntry);
  fImplementation := Rtti.RegisterClass(aImplementation);
end;

constructor TInterfaceResolverForSingleInterface.Create(const aInterface: TGuid;
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
    EInterfaceResolver.RaiseUtf8('%.Add(nil)', [self]);
  if aInterface^.Kind <> rkInterface then
    EInterfaceResolver.RaiseUtf8('%.Add(%): % is not an interface',
      [self, aInterface^.RawName]);
  result := aImplementationClass.GetInterfaceEntry(aInterface^.InterfaceGuid^);
  if result = nil then
    EInterfaceResolver.RaiseUtf8('%.Add(): % does not implement %',
      [self, aImplementationClass, aInterface^.RawName]);
  fSafe.WriteLock;
  for i := 0 to length(fEntry) - 1 do
    if fEntry[i].TypeInfo = aInterface then
    begin
      fSafe.WriteUnLock;
      EInterfaceResolver.RaiseUtf8('%.Add(%): % already registered',
        [self, aImplementationClass, aInterface^.RawName]);
    end;
end; // caller should explicitly call fSafe.WriteUnLock

procedure TInterfaceResolverList.Add(aInterface: PRttiInfo;
  aImplementationClass: TInterfacedObjectClass);
var
  e: PInterfaceEntry;
  en: PInterfaceResolverListEntry;
  n: PtrInt;
begin
  e := PrepareAddAndWriteLock(aInterface, aImplementationClass);
  try
    // here we are protected within a fSafe.WriteLock
    n := length(fEntry);
    SetLength(fEntry, n + 1);
    en := @fEntry[n];
    en^.TypeInfo := aInterface;
    en^.ImplementationClass := Rtti.RegisterClass(aImplementationClass);
    en^.InterfaceEntry := e;
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TInterfaceResolverList.Add(aInterface: PRttiInfo;
  aImplementation: TInterfacedObject);
var
  e: PInterfaceEntry;
  en: PInterfaceResolverListEntry;
  n: PtrInt;
begin
  e := PrepareAddAndWriteLock(aInterface, PClass(aImplementation)^);
  try
    // here we are protected within a fSafe.WriteLock
    n := length(fEntry);
    SetLength(fEntry, n + 1);
    begin
      en := @fEntry[n];
      en^.TypeInfo := aInterface;
      if not GetInterfaceFromEntry(aImplementation, e, en^.Instance) then
        EInterfaceResolver.RaiseUtf8('Unexcepted %.Add(%,%)',
          [self, aInterface^.RawName, aImplementation]);
      en^.InterfaceEntry := e;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TInterfaceResolverList.Delete(aInterface: PRttiInfo);
var
  i, last: PtrInt;
  e: PInterfaceResolverListEntry;
begin
  if (aInterface = nil) or
     (aInterface^.Kind <> rkInterface) then
    EInterfaceResolver.RaiseUtf8('%.Delete(?)', [self]);
  fSafe.WriteLock;
  try
    last := length(fEntry) - 1;
    e := pointer(fEntry);
    for i := 0 to last do
      if e^.TypeInfo = aInterface then
      begin
        if e^.Instance = nil then
          EInterfaceResolver.RaiseUtf8(
            '%.Delete(%) does not match an instance, but a class',
            [self, aInterface^.RawName]);
        e^.Instance := nil; // avoid GPF
        if last = 0 then
          fEntry := nil
        else
          DynArrayFakeDelete(fEntry, i, last, SizeOf(e^));
        exit;
      end
      else
        inc(e);
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

function TInterfaceResolverInjected.TryResolveImplements(aInterface: PRttiInfo;
  out Obj): boolean;
var
  i: PtrInt;
begin
  // only check local resolvers, as Implements() does
  result := true;
  if (self <> nil) and
     (aInterface <> nil) and
     (fResolvers <> nil) then
    for i := 0 to length(fResolvers) - 1 do
      if fResolvers[i].TryResolve(aInterface, Obj) then
        exit;
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

procedure TInterfaceResolverInjected.InjectStub(const aStubsByGuid: array of TGuid);
var
  i: PtrInt;
begin
  for i := 0 to high(aStubsByGuid) do
    InjectResolver([TInterfaceStub.Create(aStubsByGuid[i])]);
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
        PtrArrayAdd(fResolversToBeReleased, aOtherResolvers[i]);
      end
      else if OwnOtherResolvers then
        PtrArrayAdd(fResolversToBeReleased, aOtherResolvers[i]);
      PtrArrayAddOnce(fResolvers, aOtherResolvers[i]);
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
      PtrArrayAdd(fDependencies, aDependencies[i]);
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

procedure TInterfaceResolverInjected.DeleteResolver(aResolver: TInterfaceResolver);
begin
  ObjArrayDelete(fResolversToBeReleased, aResolver);
  ObjArrayDelete(fResolvers, aResolver);
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
    EInterfaceResolver.RaiseUtf8('%.Resolve(%) unsatisfied', [self,
      aInterface^.RawName]);
end;

procedure TInjectableObject.Resolve(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} aGuid: TGuid; out Obj);
var
  info: PRttiInfo;
begin
  info := TInterfaceFactory.Guid2TypeInfo(aGuid);
  if not TryResolve(info, Obj) then
    EInterfaceResolver.RaiseUtf8('%.Resolve(%): unsatisfied',
      [self, info^.RawName]);
end;

procedure TInjectableObject.ResolveByPair(const aInterfaceObjPairs: array of pointer);
begin
  if fResolver.InheritsFrom(TInterfaceResolverInjected) then
    TInterfaceResolverInjected(fResolver).ResolveByPair(aInterfaceObjPairs)
  else if high(aInterfaceObjPairs) = 1 then
    Resolve(aInterfaceObjPairs[0], aInterfaceObjPairs[1]^)
  else
    EInterfaceResolver.RaiseUtf8('%.ResolveByPair(?)', [self]);
end;

procedure TInjectableObject.Resolve(const aInterfaces: array of TGuid;
  const aObjs: array of pointer);
begin
  if fResolver.InheritsFrom(TInterfaceResolverInjected) then
    TInterfaceResolverInjected(fResolver).Resolve(aInterfaces, aObjs)
  else if (high(aInterfaces) = 0) and
          (high(aObjs) = 0) then
    Resolve(aInterfaces[0], aObjs[0]^)
  else
    EInterfaceResolver.RaiseUtf8('%.Resolve(?,?)', [self]);
end;

type // to access fAutoResolveInterfaces protected field
  TRttiCustomWrapper = class(TRttiJson);

procedure TInjectableObject.AutoResolve(aRaiseEServiceExceptionIfNotFound: boolean);
var
  r: TRttiJson;
  n: integer;
  p: PPRttiCustomProp;
  addr: pointer;
begin
  if (self = nil) or
     (fResolver = nil) then
    EInterfaceResolver.RaiseUtf8('%.AutoResolve with no prior registration', [self]);
  // inlined Rtti.RegisterClass()
  {$ifdef NOPATCHVMT}
  r := pointer(Rtti.FindType(PPointer(PPAnsiChar(self)^ + vmtTypeInfo)^));
  {$else}
  r := PPointer(PPAnsiChar(self)^ + vmtAutoTable)^;
  {$endif NOPATCHVMT}
  if (r = nil) or
     not (rcfAutoCreateFields in r.Flags) then
    r := DoRegisterAutoCreateFields(self);
  // resolve all published interface fields
  p := pointer(TRttiCustomWrapper(r).fAutoResolveInterfaces);
  if p = nil then
    exit;
  n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF; // length(AutoCreateClasses)
  repeat
    addr := PAnsiChar(self) + p^^.OffsetGet;
    if not TryResolve(p^^.Value.Info, addr^) then
      if aRaiseEServiceExceptionIfNotFound then
        EInterfaceResolver.RaiseUtf8(
          '%.AutoResolve: impossible to resolve published property %: %',
          [self, p^^.Name, p^^.Value.Name]);
    inc(p);
    dec(n);
  until n = 0;
end;

constructor TInjectableObject.CreateInjected(const aStubsByGuid: array of TGuid;
  const aOtherResolvers: array of TInterfaceResolver;
  const aDependencies: array of TInterfacedObject;
  aRaiseEServiceExceptionIfNotFound: boolean);
begin
  fResolver := TInterfaceResolverInjected.Create;
  fResolverOwned := true;
  TInterfaceResolverInjected(fResolver).InjectStub(aStubsByGuid);
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
    EInterfaceResolver.RaiseUtf8('%.CreateWithResolver(nil)', [self]);
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


{ ************ TInterfaceStub for Dependency Stubbing/Mocking }

// note: TInterfaceMock is defined in mormot.core.test.pas

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
  r: PInterfaceStubRule;
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
  r := @Rules[n];
  r^.Params := aParams;
  case aKind of
    isUndefined:
      ; // do not overwrite Values for weak rules like ExpectsCount/ExpectsTrace
    isReturns:
      Join(['[', aValues, ']'], r^.Values);
    isFails:
      FormatUtf8('% returned error: %', [Sender, aValues], r^.Values);
  else
    r^.Values := aValues;
  end;
  if aKind = isUndefined then
    if aExpectedPassCountOperator = ioTraceMatch then
      r^.ExpectedTraceHash := aValue
    else
    begin
      r^.ExpectedPassCountOperator := aExpectedPassCountOperator;
      r^.ExpectedPassCount := aValue;
    end
  else
  begin
    r^.Kind := aKind;
    r^.Execute := TMethod(aEvent);
    r^.ExceptionClass := aExceptionClass;
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
  info: TGetJsonField;
  tmp: TSynTempBuffer;
begin
  inherited;
  SetLength(fInput, fMethod^.ArgsInputValuesCount);
  tmp.Init(aParams);
  try
    info.Json := tmp.buf;
    for i := 0 to fMethod^.ArgsInputValuesCount - 1 do
      JsonToAnyVariant(fInput[i], info, @aSender.fInterface.DocVariantOptions, false);
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
    {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
  else
    result := fInput[Index];
end;

procedure TOnInterfaceStubExecuteParamsVariant.SetOutput(Index: integer;
  const Value: variant);
begin
  if cardinal(Index) >= fMethod^.ArgsOutputValuesCount then
    raise EInterfaceStub.Create(fSender, fMethod^, 'Output[%>=%]',
      [Index, fMethod^.ArgsOutputValuesCount])
    {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
  else
    fOutput[Index] := Value;
end;

function TOnInterfaceStubExecuteParamsVariant.GetInNamed(
  const aParamName: RawUtf8): variant;
var
  ndx: PtrInt;
begin
  if fInput <> nil then
  begin
    ndx := FindPropName(pointer(fMethod^.ArgsInputName), aParamName,
      fMethod^.ArgsInputValuesCount);
    if ndx >= 0 then
    begin
      result := fInput[ndx];
      exit;
    end;
  end;
  raise EInterfaceStub.Create(fSender, fMethod^,
    'unknown input parameter [%]', [aParamName])
    {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
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
  ndx: PtrInt;
begin
  if fOutput <> nil then
  begin
    ndx := FindPropName(pointer(fMethod^.ArgsOutputName), aParamName,
        fMethod^.ArgsOutputValuesCount);
    if ndx >= 0 then
    begin
      fOutput[ndx] := Value;
      exit;
    end;
  end;
  raise EInterfaceStub.Create(fSender, fMethod^,
    'unknown output parameter [%]', [aParamName])
    {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
end;

procedure TOnInterfaceStubExecuteParamsVariant.SetResultFromOutput;
var
  a: PtrInt;
  W: TJsonWriter;
  arg: PInterfaceMethodArgument;
  o: PVarData;
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
begin
  fResult := '';
  if fOutput = nil then
    exit;
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.Add('[');
    o := pointer(fOutput);
    a := fMethod^.ArgsOutFirst;
    arg := @fMethod^.Args[a];
    while a <= fMethod^.ArgsOutLast do
    begin
      if arg^.IsOutput then
      begin
        if cardinal(o^.VType) = varEmpty then
          arg^.AddDefaultJson(W)
        else
        begin
          W.AddVariant(PVariant(o)^, twJsonEscape);
          W.AddComma;
        end;
        inc(o);
      end;
      inc(arg);
      inc(a);
    end;
    W.CancelLastComma(']');
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
  aOutput: boolean; aLevel: TSynLogLevel);
var
  val: variant;
begin
  if (aLog = nil) or
     not (aLevel in aLog.Family.Level) then
    exit;
  if aOutput then
    val := OutputAsDocVariant(pdvObjectFixed)
  else
    val := InputAsDocVariant(pdvObjectFixed);
  aLog.Add.Log(aLevel, '%(%)', [fMethod^.InterfaceDotMethodName,
     _Safe(val)^.ToTextPairs('=', ',', twJsonEscape)], self);
end;


{ TInterfaceStub }

constructor TInterfaceStub.Create(aFactory: TInterfaceFactory;
  const aInterfaceName: RawUtf8);
var
  i: PtrInt;
begin
  if aFactory = nil then
    EInterfaceStub.RaiseUtf8('%.Create(%): Interface not registered - please' +
      ' use TInterfaceFactory.RegisterInterfaces()', [self, aInterfaceName]);
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
    EInterfaceStub.RaiseUtf8('%.InternalCheck(%) failed: %', [self,
      fInterface.fInterfaceName, FormatUtf8(aErrorMsgFmt, aErrorMsgArgs)]);
end;

constructor TInterfaceStub.Create(const aInterfaceName: RawUtf8; out aStubbedInterface);
begin
  Create(TInterfaceFactory.Get(aInterfaceName), aInterfaceName);
  InternalGetInstance(aStubbedInterface);
end;

constructor TInterfaceStub.Create(const aGuid: TGuid; out aStubbedInterface);
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

constructor TInterfaceStub.Create(const aGuid: TGuid);
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
    raise EInterfaceStub.CreateUtf8( // no RaiseUtf8() for Delphi
      '%.IntCheckCount(): Unexpected % operator', [self, Ord(aOperator)]);
  end;
  InternalCheck(ok, true, 'ExpectsCount(''%'',%,%) failed: count=%',
    [fInterface.Methods[aMethodIndex].Uri, ToText(aOperator)^, aCount, aComputed]);
end;

procedure TInterfaceStub.InstanceDestroyed(aFakeID: TInterfacedObjectFakeID);
var
  m, r, asmndx: PtrInt;
  num: cardinal;
begin
  if self <> nil then
  try
    if eCount in fHasExpects then
      for m := 0 to fInterface.MethodsCount - 1 do
        with fRules[m] do
          for r := 0 to length(Rules) - 1 do
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
      InternalCheck(LogHash = fInterfaceExpectedTraceHash, true,
        'ExpectsTrace(%) returned %', [fInterfaceExpectedTraceHash, LogHash]);
    if eTrace in fHasExpects then
      for m := 0 to fInterface.MethodsCount - 1 do
        with fRules[m] do
        begin
          asmndx := m + RESERVED_VTABLE_SLOTS;
          for r := 0 to length(Rules) - 1 do
            with Rules[r] do
              if ExpectedTraceHash <> 0 then
                InternalCheck(
                  ExpectedTraceHash = Hash32(IntGetLogAsText(asmndx, Params,
                    [wName, wParams, wResults], ',')), true,
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
  result := Executes(aMethodName,
              JsonEncodeArray(aParams, true), aEvent, aEventParams);
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
  result := Executes(aMethodName,
              JsonEncodeArray(aParams, true), aEvent, aEventParams);
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
    LogLevel: TSynLogLevel;
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

function TInterfaceStub.Executes(aLog: TSynLogClass; aLogLevel: TSynLogLevel;
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
  result := ExpectsCount(aMethodName,
              JsonEncodeArray(aParams, true), aOperator, aValue);
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
  result := ExpectsTrace(aMethodName, JsonEncodeArray(aParams, true), aValue);
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
  result := Fails(aMethodName, JsonEncodeArray(aParams, true), aErrorMsg);
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
  result := Raises(aMethodName,
              JsonEncodeArray(aParams, true), aException, aMessage);
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
  result := Returns(aMethodName, JsonEncodeArray(aParams, true),
              JsonEncodeArray(aExpectedResults, true));
end;

function TInterfaceStub.Returns(const aMethodName,
  aExpectedResults: RawUtf8): TInterfaceStub;
begin
  result := Returns(aMethodName, '', aExpectedResults);
end;

function TInterfaceStub.Returns(const aMethodName: RawUtf8;
  const aExpectedResults: array of const): TInterfaceStub;
begin
  result := Returns(aMethodName, '', JsonEncodeArray(aExpectedResults, true));
end;

function TInterfaceStub.Invoke(const aMethod: TInterfaceMethod;
  const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
  aFakeID: PInterfacedObjectFakeID;
  aServiceCustomAnswer: PServiceCustomAnswer): boolean;
var
  ndx: PtrUInt;
  rule: integer;
  ExecutesCtxtJson: TOnInterfaceStubExecuteParamsJson;
  ExecutesCtxtVariant: TOnInterfaceStubExecuteParamsVariant;
  r: PInterfaceStubRule;
  rr: PInterfaceStubRules;
  log: TInterfaceStubLog;
begin
  ndx := aMethod.ExecutionMethodIndex - RESERVED_VTABLE_SLOTS;
  if ndx >= PtrUInt(fInterface.MethodsCount) then
    result := false
  else
    begin
      rr := @fRules[ndx];
      inc(rr^.MethodPassCount);
      rule := rr^.FindStrongRuleIndex(aParams);
      if rule < 0 then
      begin
        rule := rr^.FindRuleIndex(aParams);
        if (rule >= 0) and
           (rr^.DefaultRule >= 0) then
          inc(rr^.Rules[rule].RulePassCount);
        rule := rr^.DefaultRule;
      end;
      if rule < 0 then
        if imoRaiseExceptionIfNoRuleDefined in Options then
          raise EInterfaceStub.Create(self, aMethod, 'No rule defined')
        else
        begin
          rule := rr^.FindRuleIndex(aParams);
          if rule >= 0 then
            inc(rr^.Rules[rule].RulePassCount);
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
        begin
          r := @rr^.Rules[rule];
          inc(r^.RulePassCount);
          case r^.Kind of
            isExecutesJson:
              begin
                ExecutesCtxtJson := TOnInterfaceStubExecuteParamsJson.Create(
                  self, @aMethod, aParams, r^.Values);
                try
                  TOnInterfaceStubExecuteJson(r^.Execute)(ExecutesCtxtJson);
                  result := not ExecutesCtxtJson.Failed;
                  log.CustomResults := ExecutesCtxtJson.result;
                finally
                  ExecutesCtxtJson.Free;
                end;
              end;
            isExecutesVariant:
              begin
                ExecutesCtxtVariant := TOnInterfaceStubExecuteParamsVariant.Create(
                  self, @aMethod, aParams, r^.Values);
                try
                  TOnInterfaceStubExecuteVariant(r^.Execute)(ExecutesCtxtVariant);
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
              raise r^.ExceptionClass.Create(Utf8ToString(r^.Values));
            isReturns:
              begin
                result := true;
                log.CustomResults := r^.Values;
              end;
            isFails:
              begin
                result := InternalCheck(false, false, '%', [r^.Values]);
                if not result then
                  log.CustomResults := r^.Values;
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
            FastSetString(aResult^,
              pointer(aMethod.DefaultResult), length(aMethod.DefaultResult))
          else
            FastSetString(aResult^,
              pointer(log.CustomResults), length(log.CustomResults));
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
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
  log: ^TInterfaceStubLog;
begin
  if fLogCount = 0 then
    result := ''
  else
  begin
    WR := TJsonWriter.CreateOwnedStream(temp);
    try
      log := pointer(fLogs);
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
  if aInterface <> fInterface.fInterfaceRtti.Info then
    result := false
  else
  begin
    InternalGetInstance(Obj);
    result := true;
  end;
end;

function TInterfaceStub.Implements(aInterface: PRttiInfo): boolean;
begin
  result := fInterface.fInterfaceRtti.Info = aInterface;
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

threadvar // do not publish for compilation within Delphi packages
  PerThreadRunningContext: TPerThreadRunningContext;

function PerThreadRunningContextAddress: pointer;
begin
  result := @PerThreadRunningContext;
end;

function ToText(opt: TInterfaceMethodOptions): ShortString;
begin
  GetSetNameShort(TypeInfo(TInterfaceMethodOptions), opt, result);
end;

type
  TBackgroundLauncherAction = (
    doCallMethod,
    doInstanceRelease,
    doThreadMethod);

  TBackgroundLauncher = record
    Context: PPerThreadRunningContext;
    case Action: TBackgroundLauncherAction of
      doCallMethod: (
        CallMethodArgs: pointer); // forward PCallMethodArgs
      doInstanceRelease: (
        Instance: TInterfacedObject);
      doThreadMethod: (
        ThreadMethod: TThreadMethod)
  end;
  PBackgroundLauncher = ^TBackgroundLauncher;

procedure BackgroundExecuteProc(Call: pointer); forward;

procedure BackgroundExecute(var synch: TBackgroundLauncher;
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
    {$ifdef OSWINDOWS}
    if Assigned(ServiceSingle) then
       ESynThread.RaiseUtf8('BackgroundExecute(%,backgroundThread=nil)' +
         'is not compatible with a Windows Service which has no main thread',
         [GetEnumName(TypeInfo(TBackgroundLauncherAction), ord(synch.Action))^])
    else
    {$endif OSWINDOWS}
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
  BackgroundExecute(synch, backgroundThread);
end;

procedure BackgroundExecuteThreadMethod(const method: TThreadMethod;
  backgroundThread: TSynBackgroundThreadMethod);
var
  synch: TBackgroundLauncher;
begin
  synch.Action := doThreadMethod;
  synch.ThreadMethod := method;
  BackgroundExecute(synch, backgroundThread);
end;

procedure BackgroundExecuteInstanceRelease(instance: TObject;
  backgroundThread: TSynBackgroundThreadMethod);
var
  synch: TBackgroundLauncher;
begin
  synch.Action := doInstanceRelease;
  synch.Instance := instance as TInterfacedObject;
  BackgroundExecute(synch, backgroundThread);
end;


{ TCallMethodArgs }

type
  PCallMethodArgs = ^TCallMethodArgs;
  TCallMethodArgs = record
    StackSize: PtrInt;
    StackAddr, method: PtrInt;
    ParamRegs: array[PARAMREG_FIRST .. PARAMREG_LAST] of PtrInt;
    {$ifdef HAS_FPREG}
    FPRegs: array[FPREG_FIRST .. FPREG_LAST] of Double;
    {$endif HAS_FPREG}
    res64: Int64Rec;
    resKind: TInterfaceMethodValueType;
  end;

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
   ldr  a3, [v2, #TCallMethodArgs.StackAddr]
stack_loop:
   // copy a3 to a4 and increment a3 (a3 = StackAddr)
   ldmia a3!, {a4}
   // copy a4 to a2 and increment a2 (a2 = StackPointer)
   stmia a2!, {a4}
   // decrement stacksize counter, with update of flags for loop
   subs  a1, a1, #1
   bne  stack_loop
load_regs:
   ldr   r0, [v2, #TCallMethodArgs.ParamRegs + REGR0 * 4 - 4]
   ldr   r1, [v2, #TCallMethodArgs.ParamRegs + REGR1 * 4 - 4]
   ldr   r2, [v2, #TCallMethodArgs.ParamRegs + REGR2 * 4 - 4]
   ldr   r3, [v2, #TCallMethodArgs.ParamRegs + REGR3 * 4 - 4]
   {$ifdef HAS_FPREG}
   vldr  d0, [v2, #TCallMethodArgs.FPRegs + REGD0 * 8 - 8]
   vldr  d1, [v2, #TCallMethodArgs.FPRegs + REGD1 * 8 - 8]
   vldr  d2, [v2, #TCallMethodArgs.FPRegs + REGD2 * 8 - 8]
   vldr  d3, [v2, #TCallMethodArgs.FPRegs + REGD3 * 8 - 8]
   vldr  d4, [v2, #TCallMethodArgs.FPRegs + REGD4 * 8 - 8]
   vldr  d5, [v2, #TCallMethodArgs.FPRegs + REGD5 * 8 - 8]
   vldr  d6, [v2, #TCallMethodArgs.FPRegs + REGD6 * 8 - 8]
   vldr  d7, [v2, #TCallMethodArgs.FPRegs + REGD7 * 8 - 8]
   {$endif HAS_FPREG}
   ldr   v1, [v2, #TCallMethodArgs.method]
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
   str   r0, [v2, #TCallMethodArgs.res64.Lo]
   str   r1, [v2, #TCallMethodArgs.res64.Hi]
{$ifdef HAS_FPREG}
   ldr   r2, [v2, #TCallMethodArgs.resKind]
   cmp   r2, imvDouble
   beq   float_result
   cmp   r2, imvDateTime
   beq   float_result
   cmp   r2, imvCurrency
   bne   asmcall_end
   // store double result in res64
float_result:
   vstr  d0, [v2, #TCallMethodArgs.res64]
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
   ldr  x2, [x19, #TCallMethodArgs.StackSize]
   // if there is no stack content, do nothing
   cmp	x2, #0
   b.eq	load_regs
   // point x3 to bottom of stack.
   mov	x3, sp
   // load x4 with CallMethod stack address
   ldr	x4, [x19, #TCallMethodArgs.StackAddr]
stack_loop:
   // load x5 and x6 with stack contents
   ldr  x5, [x4]
   ldr  x6, [x4, #8]
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
   ldr  x0, [x19, #TCallMethodArgs.ParamRegs + REGX0 * 8 - 8]
   ldr  x1, [x19, #TCallMethodArgs.ParamRegs + REGX1 * 8 - 8]
   ldr  x2, [x19, #TCallMethodArgs.ParamRegs + REGX2 * 8 - 8]
   ldr  x3, [x19, #TCallMethodArgs.ParamRegs + REGX3 * 8 - 8]
   ldr  x4, [x19, #TCallMethodArgs.ParamRegs + REGX4 * 8 - 8]
   ldr  x5, [x19, #TCallMethodArgs.ParamRegs + REGX5 * 8 - 8]
   ldr  x6, [x19, #TCallMethodArgs.ParamRegs + REGX6 * 8 - 8]
   ldr  x7, [x19, #TCallMethodArgs.ParamRegs + REGX7 * 8 - 8]
   ldr  d0, [x19, #TCallMethodArgs.FPRegs    + REGD0 * 8 - 8]
   ldr  d1, [x19, #TCallMethodArgs.FPRegs    + REGD1 * 8 - 8]
   ldr  d2, [x19, #TCallMethodArgs.FPRegs    + REGD2 * 8 - 8]
   ldr  d3, [x19, #TCallMethodArgs.FPRegs    + REGD3 * 8 - 8]
   ldr  d4, [x19, #TCallMethodArgs.FPRegs    + REGD4 * 8 - 8]
   ldr  d5, [x19, #TCallMethodArgs.FPRegs    + REGD5 * 8 - 8]
   ldr  d6, [x19, #TCallMethodArgs.FPRegs    + REGD6 * 8 - 8]
   ldr  d7, [x19, #TCallMethodArgs.FPRegs    + REGD7 * 8 - 8]
   // call TCallMethodArgs.method
   ldr  x15, [x19, #TCallMethodArgs.method]
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
   str  d0, [x19, #TCallMethodArgs.res64]
asmcall_end:
   add  sp, sp, #MAX_EXECSTACK
   ldr  x19, [sp], #16
   ldp  x29, x30, [sp], #16
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
        // simulate .params 32
        lea     rsp, [rsp - MAX_EXECSTACK]
        // align stack to 16 bytes
        and     rsp, -16
{$else DELPHI} // ensure we use regular .params command for easier debugging
asm
        .params 32    // size for MAX_METHOD_ARG = 32 parameters
        .pushnv r12   // generate prolog+epilog to save and restore non-volatile r12
{$endif FPC}
        // get Args
        mov     r12, Args
        // copy (push) stack content (if any)
        mov     rcx, [r12].TCallMethodArgs.StackSize
        mov     rdx, [r12].TCallMethodArgs.StackAddr
        test    ecx, ecx
        jz      @z
@s:     push    qword ptr [rdx]
        sub     rdx, 8
        sub     ecx, 1
        jnz     @s
@z:     // fill registers and call method
        {$ifdef OSPOSIX}
        // Linux/BSD System V AMD64 ABI
        mov     rdi, [r12 + TCallMethodArgs.ParamRegs + REGRDI * 8 - 8]
        mov     rsi, [r12 + TCallMethodArgs.ParamRegs + REGRSI * 8 - 8]
        mov     rdx, [r12 + TCallMethodArgs.ParamRegs + REGRDX * 8 - 8]
        mov     rcx, [r12 + TCallMethodArgs.ParamRegs + REGRCX * 8 - 8]
        mov     r8,  [r12 + TCallMethodArgs.ParamRegs + REGR8  * 8 - 8]
        mov     r9,  [r12 + TCallMethodArgs.ParamRegs + REGR9  * 8 - 8]
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
        mov     r8,  [r12 + TCallMethodArgs.ParamRegs + REGR8  * 8 - 8]
        mov     r9,  [r12 + TCallMethodArgs.ParamRegs + REGR9  * 8 - 8]
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
        IInterface(synch^.Instance)._Release; // dec(RefCount) + Free if 0
      doThreadMethod:
        synch^.ThreadMethod;
    end;
  finally
    threadContext^ := backup;
  end;
end;


{ TInterfaceMethodExecuteRaw }

constructor TInterfaceMethodExecuteRaw.Create(aFactory: TInterfaceFactory;
  aMethod: PInterfaceMethod; const aOptions: TInterfaceMethodOptions);
var
  a: PtrInt;
  arg: PInterfaceMethodArgument;
  pv: PPointer;
begin
  // set parameters
  fFactory := aFactory;
  fMethod := aMethod;
  SetOptions(aOptions);
  // initialize temporary storage for all call arguments and fValues pointers
  SetLength(fStorage, integer(fMethod^.ArgsSizeAsValue) +
                      length(aMethod^.Args) shl POINTERSHR);
  // assign the parameters storage to the fValues[] pointers
  pv := @fStorage[fMethod^.ArgsSizeAsValue];
  fValues := pointer(pv);
  arg := pointer(fMethod^.Args);
  for a := 1 to length(fMethod^.Args) - 1 do
  begin
    inc(arg);
    inc(pv);
    pv^ := @fStorage[arg^.OffsetAsValue];
  end;
end;

procedure TInterfaceMethodExecuteRaw.SetOptions(
  const Value: TInterfaceMethodOptions);
begin
  fOptions := Value;
  fDocVariantOptions := fFactory.DocVariantOptions;
  if optVariantCopiedByReference in Value then
    include(fDocVariantOptions, dvoValueCopiedByReference);
  if optVariantFloatAllowed in Value then
    include(fDocVariantOptions, dvoAllowDoubleValue);
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
begin
  if fExecutedInstancesFailed <> nil then
    fExecutedInstancesFailed := nil;
  if fAlreadyExecuted then
    FillCharFast(pointer(fStorage)^, fMethod^.ArgsSizeAsValue, 0)
  else
    fAlreadyExecuted := true;
  if fMethod.ArgsUsedCount[imvvObject] <> 0 then
    // set new input and output TObject instances as expected by the call
    fMethod^.ArgsClassNewInstance(pointer(fValues));
end;

procedure TInterfaceMethodExecuteRaw.RawExecute(
  const Instances: PPointerArray; InstancesLast: integer);
var
  pv: PPointer;
  arg: PInterfaceMethodArgument;
  e, i: PtrInt;
  call: TCallMethodArgs;
  Stack: packed array[0 .. MAX_EXECSTACK - 1] of byte;
begin
  FillCharFast(call, SizeOf(call), 0);
  // create the stack layout with proper alignment
  {$ifdef CPUX86}
  call.StackAddr := PtrInt(@Stack[0]);
  call.StackSize := fMethod^.ArgsSizeInStack;
  {$ifdef OSPOSIX} // ensure always aligned by 16 bytes on POSIX
  while call.StackSize and 15 <> 0 do
    inc(call.StackSize, POINTERBYTES); // needed for Darwin and Linux i386
  {$endif OSPOSIX}
  {$else}
  {$ifdef CPUINTEL}
  call.StackSize := fMethod^.ArgsSizeInStack shr 3;
  // ensure stack aligned on 16 bytes (mandatory on some architectures)
  if call.StackSize and 1 <> 0 then
    inc(call.StackSize);
  // stack is filled reversed (RTL)
  call.StackAddr := PtrInt(@Stack[call.StackSize * 8 - 8]);
  {$else}
  // stack is filled normally (LTR)
  call.StackAddr := PtrInt(@Stack[0]);
  {$ifdef CPUAARCH64}
  call.StackSize := fMethod^.ArgsSizeInStack shr 3;
  // ensure stack aligned on 16 bytes (mandatory: needed for correct low level asm)
  if call.StackSize and 1 <> 0 then
    inc(call.StackSize);
  {$else}
  call.StackSize := fMethod^.ArgsSizeInStack shr 2;
  {$endif CPUAARCH64}
  {$endif CPUINTEL}
  {$endif CPUX86}
  // assign content from fValues[] into the stack
  pv := pointer(fValues);
  arg := pointer(fMethod^.Args);
  for i := 1 to PDALen(PAnsiChar(arg) - _DALEN)^ + (_DAOFF - 1) do
  begin
    inc(arg);
    inc(pv);
    case arg^.RawExecute of
      reValReg:
        call.ParamRegs[arg^.RegisterIdent] := PPtrInt(pv^)^;
      reValRegs:
        // e.g. Int64 on 32-bit ARM systems are passed in two registers
        MoveFast(pv^^, call.ParamRegs[arg^.RegisterIdent], arg^.SizeInStack);
      reValStack:
        MoveFast(pv^^, Stack[arg^.InStackOffset], arg^.SizeInStack);
      reRefReg:
        call.ParamRegs[arg^.RegisterIdent] := PPtrInt(pv)^;
      reRefStack:
        PPointer(@Stack[arg^.InStackOffset])^ := pv^;
      {$ifdef HAS_FPREG}
      reValFpReg:
        PInt64(@call.FPRegs[arg^.FPRegisterIdent])^ := PInt64(pv^)^;
      reValFpRegs:
        // e.g. HFA on SYSVABI systems are passed in several FP registers
        MoveFast(pv^^, call.FPRegs[arg^.FPRegisterIdent], arg^.SizeInStack);
      {$endif HAS_FPREG}
    end;
  end;
  // execute the method
  for i := 0 to InstancesLast do
  begin
    // handle method execution smsBefore interception
    fCurrentStep := smsBefore;
    if fOnExecute <> nil then
    begin
      if (optInterceptInputOutput in Options) and
         (fMethod^.ArgsInputValuesCount <> 0) then
        fMethod^.ArgsStackAsDocVariant(fValues, fInput, true);
      for e := 0 to length(fOnExecute) - 1 do
      try
        fOnExecute[e](self, smsBefore);
      except // ignore any exception during interception
      end;
    end;
    // prepare the low-level call context for the asm stub
    call.ParamRegs[PARAMREG_FIRST] := PtrInt(Instances[i]); // pass self
    call.method := PPtrIntArray(PPointer(Instances[i])^)^[
      fMethod^.ExecutionMethodIndex];
    if fMethod^.ArgsResultIndex >= 0 then
      call.resKind := fMethod^.Args[fMethod^.ArgsResultIndex].ValueType
    else
      call.resKind := imvNone;
    // launch the asm stub in the expected execution context
    try
      if (optExecInMainThread in Options) and
         (GetCurrentThreadID <> MainThreadID) then
        BackgroundExecuteCallMethod(@call, nil)
      else if optExecInPerInterfaceThread in Options then
        if Assigned(BackgroundExecutionThread) then
          BackgroundExecuteCallMethod(@call, BackgroundExecutionThread)
        else
          EInterfaceFactory.RaiseU('optExecInPerInterfaceThread' +
            ' with BackgroundExecutionThread=nil')
      else
        CallMethod(call); // actual asm stub
      if (fMethod^.ArgsResultIndex >= 0) and
         (fMethod^.Args[fMethod^.ArgsResultIndex].ValueVar = imvv64) then
        PInt64Rec(fValues[fMethod^.ArgsResultIndex])^ := call.res64;
      // handle method execution smsAfter interception
      fCurrentStep := smsAfter;
      if fOnExecute <> nil then
      begin
        if (optInterceptInputOutput in Options) and
           (fMethod^.ArgsOutputValuesCount <> 0) then
          fMethod^.ArgsStackAsDocVariant(fValues, fOutput, false);
        for e := 0 to length(fOnExecute) - 1 do
        try
          fOnExecute[e](self, smsAfter);
        except // ignore any exception during interception
        end;
        if optInterceptInputOutput in Options then
        begin
          fInput.Clear;
          fOutput.Clear;
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
        ObjectToJson(Exc, fExecutedInstancesFailed[i], TEXTWRITEROPTIONS_DEBUG);
      end;
    end;
  end;
end;

procedure TInterfaceMethodExecuteRaw.AfterExecute;
begin
  // finalize managed parameters after each call
  if fMethod^.ArgsManagedCount <> 0 then
    fMethod^.ArgsReleaseValues(pointer(fValues)); // use TRttiCustom info
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
    fTempTextWriter.CustomOptions :=
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
  if PCardinal(PPPointer(Instance)^^)^ =
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
      WR.CancelAll; // not CancelAllAsNew to keep twoForceJsonExtended
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

function TInterfaceMethodExecute.ExecuteJsonParse(var Ctxt: TJsonParserContext;
  Error: PShortString): boolean;
var
  arg: integer; // should be integer, not PtrInt
  asJsonObject: boolean;
  a: PInterfaceMethodArgument;
  V: pointer;
  magic: PCardinal;
begin
  result := false;
  case Ctxt.Json^ of
    '[': // default array of plain in-ordered values
      asJsonObject := false;
    '{': // retrieve arguments values from JSON object -> field name lookup
      asJsonObject := true;
  else
    exit;
  end;
  Ctxt.Json := IgnoreAndGotoNextNotSpace(Ctxt.Json);
  arg := fMethod^.ArgsInFirst;
  a := @fMethod^.Args[arg];
  repeat
    if asJsonObject then
    begin
      if not Ctxt.GetJsonFieldName then
        break; // end of JSON object
      if (arg = 0) or // arg := 0 below to force search
         // optimistic process of JSON object with in-order parameters
         not IdemPropName(a^.ParamName^, Ctxt.Value, Ctxt.ValueLen) then
      begin
        // slower but safe ctxt.Method when not in-order
        a := fMethod^.ArgInput(Ctxt.Value, Ctxt.ValueLen, @arg);
        if a = nil then
          if optErrorOnMissingParam in fOptions then
            exit
          else
            arg := 0;
      end;
    end;
    if a <> nil then
    begin
      V := fValues[arg];
      if imfInputIsOctetStream in fMethod.Flags then
      begin
        magic := pointer(Ctxt.Get.Json);
        if magic^ = JSON_BIN_MAGIC_C then
        begin
          // passed as pointer from TRestServerRoutingRest.ExecuteSoaByInterface
          inc(magic);
          PRawByteString(V)^ := PRawByteString(magic)^;
          break; // single parameter
        end;
      end;
      if (a^.ValueType = imvInterface) and
         not (vIsInterfaceJson in a^.ValueKindAsm) then // e.g. not IDocList
        if Assigned(OnCallback) then
          // retrieve TRestServerUriContext.ExecuteCallback fake interface
          // via TServiceContainerServer.GetFakeCallback
          OnCallback(Ctxt, a^.ArgRtti, PInterface(fValues[arg])^)
        else
          EInterfaceFactory.RaiseUtf8('OnCallback=nil for %(%: %)',
            [fMethod^.InterfaceDotMethodName, a^.ParamName^,
             a^.ArgTypeName^]) // paranoid (already checked before)
      else if not a^.SetFromJson(Ctxt, fMethod, V, Error) then
        exit;
    end
    else // ignore this unknown asJsonObject field value
      Ctxt.Json := GotoNextJsonItem(Ctxt.Json, Ctxt.Get.EndOfObject);
    if Ctxt.Json = nil then
      break;
    Ctxt.Json := GotoNextNotSpace(Ctxt.Json);
    if asJsonObject then
    begin
      if Ctxt.Json^ in [#0, '}'] then
        break; // end of JSON object
      if arg = 0 then
        continue; // continue manual search until finished JSON object
    end;
    repeat
      inc(arg);
      if arg > fMethod^.ArgsInLast then
      begin
        arg := 0; // no next result argument -> force manual search
        break;
      end;
      inc(a);
    until a^.IsInput;
  until (arg = 0) and
        not asJsonObject;
  result := true;
end;

function TInterfaceMethodExecute.ExecuteJson(const Instances: array of pointer;
  P: PUtf8Char; Res: TJsonWriter; Error: PShortString; ResAsJsonObject: boolean): boolean;
var
  arg: integer; // should be integer, not PtrInt
  opt: TTextWriterWriteObjectOptionsBoolean;
  a: PInterfaceMethodArgument;
  custom: PServiceCustomAnswer;
  c: TJsonParserContext;
begin
  // 1. prepare all fValues[] pointers for the input/output arguments
  result := false;
  BeforeExecute;
  try
    // 2. locate input arguments from JSON array or object
    if (P <> nil) and
       (PInteger(P)^ = NULL_LOW) then
      P := nil;
    if fMethod^.ArgsInputValuesCount <> 0 then
      if P <> nil then
      begin
        c.InitParser(GotoNextNotSpace(P), nil, fFactory.JsonParserOptions, @fDocVariantOptions);
        if not ExecuteJsonParse(c, Error) then
          exit;
      end
      else if optErrorOnMissingParam in fOptions then
        exit; // paranoid setting
    // 3. execute the method, using prepared input/output fValues[]
    RawExecute(@Instances[0], high(Instances));
    // 4. send back any var/out output arguments as JSON
    if Res <> nil then
    begin
      // handle custom content (not JSON array/object answer)
      if imfResultIsServiceCustomAnswer in fMethod^.Flags then
      begin
        custom := fValues[fMethod^.ArgsResultIndex];
        if custom^.Header = '' then
          // set to 'Content-Type: application/json' by default
          custom^.Header := JSON_CONTENT_TYPE_HEADER_VAR;
        // implementation could override the Header content
        fServiceCustomAnswerHead := custom^.Header;
        Res.ForceContent(custom^.Content);
        if custom^.Status = 0 then
          // Values[]=@Records[] is filled with 0 by default
          custom^.Status := HTTP_SUCCESS;
        fServiceCustomAnswerStatus := custom^.Status;
        result := true;
        exit;
      end
      else if imfResultIsServiceCustomStatus in fMethod^.Flags then
        fServiceCustomAnswerStatus := PCardinal(fValues[fMethod^.ArgsResultIndex])^;
      // write the '{"result":[...' array or object
      opt[{smdVar=}false] := DEFAULT_WRITEOPTIONS[optDontStoreVoidJson in fOptions];
      opt[{smdVar=}true] := []; // let var params override void/default values
      arg := fMethod^.ArgsOutFirst;
      a := @fMethod^.Args[arg];
      while arg <= fMethod^.ArgsOutLast do
      begin
        if a^.IsOutput then
        begin
          if ResAsJsonObject then
            Res.AddPropName(a^.ParamName^);
          a^.AddJson(Res, fValues[arg], opt[a^.ValueDirection = imdVar]);
          Res.AddComma;
        end;
        inc(arg);
        inc(a);
      end;
      Res.CancelLastComma;
    end;
    result := true;
  finally
    // 5. release any managed input/output parameters from fValues[]
    AfterExecute;
  end;
end;


{ TInterfaceMethodExecuteCached }

class procedure TInterfaceMethodExecuteCached.Prepare(aFactory: TInterfaceFactory;
  out Cached: TInterfaceMethodExecuteCachedDynArray);
var
  i: PtrInt;
begin
  // prepare some reusable execution context (avoid most memory allocations)
  SetLength(Cached, aFactory.MethodsCount);
  for i := 0 to aFactory.MethodsCount - 1 do
    // pre-allocate with a 32KB generous non-resizable work buffer memory
    Cached[i] := Create(aFactory, @aFactory.Methods[i], [], {shared=}true);
end;

constructor TInterfaceMethodExecuteCached.Create(aFactory: TInterfaceFactory;
  aMethod: PInterfaceMethod; const aOptions: TInterfaceMethodOptions; aShared: boolean);
begin
  inherited Create(aFactory, aMethod, aOptions);
  if aShared then
  begin
    // the shared instance has a generous 32KB non resizable work buffer
    fWR := TJsonWriter.CreateOwnedStream(32768, {nosharedstream=}true);
    fWR.FlushToStreamNoAutoResize := true; // stick to BufferSize
  end
  else
    // start with a resizable 2KB buffer (medium blocks are > 2600 bytes in MM)
    fWR := TJsonWriter.CreateOwnedStream(2048, {nosharedstream=}true);
end;

destructor TInterfaceMethodExecuteCached.Destroy;
begin
  inherited Destroy;
  fWR.Free;
end;

function TInterfaceMethodExecuteCached.Acquire(
  ExecuteOptions: TInterfaceMethodOptions;
  WROptions: TTextWriterOptions): TInterfaceMethodExecuteCached;
begin
  if fCached.TryLock then
  begin
    // reuse this shared instance between calls
    result := self;
    SetOptions(ExecuteOptions);
    fWR.CancelAllAsNew;
  end
  else
    // on thread contention, will use a transient temporary instance
    result := TInterfaceMethodExecuteCached.Create(
      fFactory, fMethod, ExecuteOptions);
  fWR.CustomOptions := WROptions;
end;

procedure TInterfaceMethodExecuteCached.Release(exec: TInterfaceMethodExecuteCached);
begin
  if exec = self then
    fCached.UnLock // ready for the next call
  else
    exec.Free;     // remove the thread-specific temporary instance
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
  PPointer(aInterfaceField)^ := pointer(aValue);
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
  inst := Rtti.FindClass(PClass(self)^).GetPrivateSlot(TSetWeakZero);
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
  PatchCodePtrUInt(P, PtrUInt(@HookedFreeInstance), {leaveunprot=}false);
end;

function GetWeakZero(aClass: TClass; CreateIfNonExisting: boolean): TSetWeakZero;
var
  rc: TRttiCustom;
begin
  {$ifdef NOPATCHVMT}
  EInterfaceFactory.RaiseU('Unsupported SetWeakZero() on this context');
  {$endif NOPATCHVMT}
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


{ ************ Code/Documentation Generation Logic Extraction from RTTI }

const
  SIZETODELPHI: array[0..8] of string[7] = (
    'integer', 'byte', 'word', 'integer',
    'integer', 'int64', 'int64', 'int64', 'int64');

  TYPES_SIZE: array[0..8] of TWrapperType = (
    wInteger, wByte, wWord, wInteger,
    wInteger, wInt64, wInt64, wInt64, wInt64);

  TYPES_SIMPLE: array[TRttiParserType] of TWrapperType = (
    wUnknown,  //  ptNone
    wArray,    //  ptArray
    wBoolean,  //  ptBoolean
    wByte,     //  ptByte
    wCardinal, //  ptCardinal
    wCurrency, //  ptCurrency
    wDouble,   //  ptDouble
    wDouble,   //  ptExtended
    wInt64,    //  ptInt64
    wInteger,  //  ptInteger
    wQWord,    //  ptQWord
    wBlob,     //  ptRawByteString
    wRawJson,  //  ptRawJson
    wRawUtf8,  //  ptRawUtf8
    wRecord,   //  ptRecord
    wSingle,   //  ptSingle
    wString,   //  ptString
    wRawUtf8,  //  ptSynUnicode
    wDateTime, //  ptDateTime
    wDateTime, //  ptDateTimeMS
    wGuid,     //  ptGuid
    wBlob,     //  ptHash128
    wBlob,     //  ptHash256
    wBlob,     //  ptHash512
    wID,       //  ptOrm
    wTimeLog,  //  ptTimeLog
    wRawUtf8,  //  ptUnicodeString
    wInt64,    //  ptUnixTime
    wInt64,    //  ptUnixMSTime
    wVariant,  //  ptVariant
    wRawUtf8,  //  ptWideString
    wRawUtf8,  //  ptWinAnsi
    wWord,     //  ptWord
    wEnum,     //  ptEnumeration
    wSet,      //  ptSet
    wUnknown,  //  ptClass
    wArray,    //  ptDynArray - with specific code below
    wUnknown,  //  ptInterface
    wRawUtf8,  //  ptPUtf8Char
    wUnknown); //  ptCustom

{ TWrapperContext }

constructor TWrapperContext.Create(const aSourcePath, aDescriptions: TFileName);
var
  desc: RawByteString;
  source: TFileName;
  src: PChar;
  n: PtrInt;
begin
  TDocVariant.NewFast([
    @fORM,
    @fRecords,
    @fEnumerates,
    @fSets,
    @fArrays,
    @fUnits,
    @fDescriptions]);
  if aDescriptions <> '' then
    desc := StringFromFile(aDescriptions);
  if {%H-}desc = '' then
    ResourceSynLZToRawByteString(WRAPPER_RESOURCENAME, desc);
  if desc <> '' then
    fDescriptions.InitJsonInPlace(pointer(desc), JSON_FAST);
  if aSourcePath <> '' then
  begin
    src := pointer(aSourcePath);
    n := 0;
    repeat
      source := GetNextItemString(src, ';');
      if (source <> '') and
         DirectoryExists(source) then
      begin
        SetLength(fSourcePath, n + 1);
        fSourcePath[n] := IncludeTrailingPathDelimiter(source);
        inc(n);
      end;
    until src = nil;
  end;
end;

function TWrapperContext.CustomType(rtti: TRttiCustom): TWrapperType;
begin
  result := wUnknown;
end;

function TWrapperContext.ContextNestedProperties(rtti: TRttiCustom;
  const parentName: RawUtf8): variant;
var
  i: PtrInt;
begin
  SetVariantNull(result);
  case rtti.Parser of
    ptRecord,
    ptClass:
      ; // use rtti.Props
    ptArray,
    ptDynArray:  // use array item (may be nil for static unmanaged)
      rtti := rtti.ArrayRtti;
  else
    exit; // no nested properties
  end;
  TDocVariant.NewFast(result);
  if rtti <> nil then
    for i := 0 to rtti.Props.Count - 1 do
      TDocVariantData(result).AddItem(
        ContextOneProperty(rtti.Props.List[i], parentName));
end;

function ClassToWrapperType(c: TClass): TWrapperType;
begin
  if ClassInheritsFromName(c, 'TOrm') then
    result := wORM
  else
    result := wObject;
end;

function TWrapperContext.ContextFromRtti(typ: TWrapperType; rtti: TRttiCustom;
  typName: RawUtf8; const parentName: RawUtf8): variant;
var
  typAsName: PShortString;

  function VarName(lng: TWrapperLanguage): variant;
  begin
    { TODO: refactor TID and Int64 for JavaScript? (integers truncated to 53-bit) }
    if TYPES_LANG[lng, typ] <> '' then
      RawUtf8ToVariant(TYPES_LANG[lng, typ], result)
    else if typName = '' then
      SetVariantNull(result)
    else
      RawUtf8ToVariant(typName, result);
  end;

  function VarSwagger: variant;
  begin
    if TYPES_LANG[lngSwagger, typ] <> '' then
      result := _JsonFast(TYPES_LANG[lngSwagger, typ])
    else if typName = '' then
      SetVariantNull(result)
    else
      RawUtf8ToVariant(typName, result);
  end;

  procedure RegisterType(var list: TDocVariantData);
  var
    info: variant;
  begin
    if list.SearchItemByProp('name', typName, false) >= 0 then
      // already registered
      exit;
    if rtti = nil then
      EWrapperContext.RaiseUtf8('%.RegisterType(%): no RTTI', [typAsName^, typName]);
    case typ of
      wEnum,
      wSet:
        // include (untrimed) identifier: values[] may be trimmed at mustache level
        info := _JsonFastFmt('{name:?,values:%}',
          [rtti.Cache.EnumInfo^.GetEnumNameAllAsJsonArray(false)], [typName]);
      wRecord:
        if rtti.Props.Count <> 0 then
          info := _ObjFast([
            'name',      typName,
            'camelName', LowerCamelCase(typName),
            'snakeName', SnakeCase(typName),
            'fields',    ContextNestedProperties(rtti, parentName)]);
      wArray:
        begin
          if rtti.ObjArrayClass <> nil then
          begin
            info := ContextFromRtti(
              ClassToWrapperType(rtti.ObjArrayClass), rtti.ArrayRtti);
            _Safe(info)^.AddValue('isObjArray', true);
          end
          else
          begin
            if rtti.ArrayRtti = nil then
              if rtti.Cache.ItemSize > high(TYPES_SIZE) then
                // to avoid buffer overflow
                info := ContextFromRtti(wRawUtf8)
              else
                info := ContextFromRtti(TYPES_SIZE[rtti.Cache.ItemSize])
            else if rcfBinary in rtti.ArrayRtti.Flags then
              info := ContextFromRtti(wRawUtf8)
            else
              info := ContextFromRtti(wUnknown, rtti.ArrayRtti);
          end;
          // can be used to create static array (dynamic arrays have ItemCount=0)
          //  array{{#staticMaxIndex}}[0..{{staticMaxIndex}}]{{/staticMaxIndex}} of
          _ObjAddProps([
            'name',      typName,
            'camelName', LowerCamelCase(typName),
            'snakeName', SnakeCase(typName)], info);
          if rtti.Cache.ItemCount > 0 then
            _Safe(info)^.AddValue('staticMaxIndex', rtti.Cache.ItemCount-1);
        end;
    end;
    if not VarIsEmptyOrNull(info) then
      // null e.g. for a record without custom text definition
      list.AddItem(info);
  end;

begin
  // retrieve typ from RTTI if needed
  if typ = wUnknown then
  begin
    if rtti = nil then
      EWrapperContext.RaiseUtf8(
        '%.ContextFromRtti: No RTTI nor typ for [%]', [self, typName]);
    if self <> nil then
      typ := CustomType(rtti); // may be overriden in TWrapperContextRest
    if typ = wUnknown then
    begin
      typ := TYPES_SIMPLE[rtti.Parser];
      if typ = wUnknown then
        case rtti.Kind of
          {$ifdef FPC}rkObject,{$else}{$ifdef UNICODE}rkMRecord,{$endif}{$endif}
          rkRecord:
            typ := wRecord;
          rkInterface:
            typ := wInterface;
        else
          EWrapperContext.RaiseUtf8(
            '%.ContextFromRtti: Not enough RTTI for [%]', [self, rtti.Name]);
        end;
    end;
  end;
  // recognize some specific types
  case typ of
    wRecord:
      case FindPropName(['TGuid', 'TServiceCustomAnswer',
        'TBcd', 'RawSid', 'RawSecurityDescriptor', 'TSecAccessMask'], typName) of
        0:
          typ := wGuid;
        1:
          typ := wCustomAnswer;
        2, 3, 4, 5:
          typ := wRawUtf8; // as in mormot.crypt.secure/mormot.db.rad units
      end;
    wObject:
      if (rtti <> nil) and
         (rtti.Kind = rkClass) then
        typ := ClassToWrapperType(rtti.ValueClass); // recognize e.g. wOrm
  end;
  // set typName/typAsName
  if typName = '' then
    if rtti <> nil then
      if rcfWithoutRtti in rtti.Flags then // undefined nested fields
        Make(['T', parentName, InterlockedIncrement(fNestedId)], typName)
      else
        typName := rtti.Name
    else
      typName := TYPES_LANG[lngDelphi, typ];
  typAsName := GetEnumName(TypeInfo(TWrapperType), ord(typ));
  // generate basic context as TDocVariant fields
  result := _ObjFast([
    'typeWrapper', typAsName^,
    'typeSource',  typName,
    'typeDelphi',  VarName(lngDelphi),
    'typePascal',  VarName(lngPascal),
    'typeCS',      VarName(lngCS),
    'typeJava',    VarName(lngJava),
    'typeTS',      VarName(lngTypeScript),
    'typeSwagger', VarSwagger]);
  if self = nil then
    // no need to have full info if called e.g. from MVC
    exit;
  // add special marshalling information
  if rtti <> nil then
    case rtti.Kind of
      rkClass:
        AddUnit(rtti.Info^.RttiClass^.UnitName^, @result);
    end;
  case typ of
    wBoolean,
    wByte,
    wWord,
    wInteger,
    wCardinal,
    wInt64,
    wQWord,
    wID,
    wReference,
    wTimeLog,
    wModTime,
    wCreateTime,
    wSingle,
    wDouble,
    wRawUtf8,
    wString:
      ; // simple types have no special marshalling
    wDateTime:
      _ObjAddProps(['isDateTime',  true,
                    'toVariant',   'DateTimeToIso8601',
                    'fromVariant', 'Iso8601ToDateTime'], result);
    wRecordVersion:
      _ObjAddProp('isRecordVersion', true, result);
    wCurrency:
      _ObjAddProp('isCurrency', true, result);
    wVariant:
      _ObjAddProp('isVariant', true, result);
    wRawJson:
      _ObjAddProp('isJson', true, result);
    wEnum:
      begin
        _ObjAddProps(['isEnum',      true,
                      'toVariant',   'ord',
                      'fromVariant', 'Variant2' + typName], result);
        if self <> nil then
          RegisterType(fEnumerates);
      end;
    wSet:
      begin
        _ObjAddProps(['isSet',      true,
                      'toVariant',
                        SIZETODELPHI[rtti.Cache.EnumInfo.SizeInStorageAsSet],
                      'fromVariant', typName], result);
        if self <> nil then
          RegisterType(fSets);
      end;
    wGuid:
      _ObjAddProps(['toVariant',   'GuidToVariant',
                    'fromVariant', 'VariantToGuid'], result);
    wCustomAnswer:
      _ObjAddProps(['toVariant',   'HttpBodyToVariant',
                    'fromVariant', 'VariantToHttpBody'], result);
    wRecord:
      begin
        _ObjAddProp('isRecord', true, result);
        if rtti <> nil then
        begin
          _ObjAddProps(['toVariant',   typName + '2Variant',
                        'fromVariant', 'Variant2' + typName], result);
          if self <> nil then
            RegisterType(fRecords);
        end;
      end;
    wOrm:
      _ObjAddProps(['isSQLRecord',  true,
                    'isOrm', true], result);
    wObject:
      begin
        _ObjAddProp('isObject', true, result);
        if rtti <> nil then
          _ObjAddProps(['toVariant',   'ObjectToVariant',
                        'fromVariant', typName + '.CreateFromVariant'], result);
      end;
    wArray:
      begin
        _ObjAddProp('isArray', true, result);
        if rtti <> nil then
        begin
          _ObjAddProps(['toVariant',   typName + '2Variant',
                        'fromVariant', 'Variant2' + typName], result);
          if self <> nil then
            RegisterType(fArrays);
        end;
      end;
    wBlob:
      _ObjAddProps(['isBlob',      true,
                    'toVariant',   'BlobToVariant',
                    'fromVariant', 'VariantToBlob'], result);
    wInterface:
      _ObjAddProp('isInterface', true, result);
  else
    EWrapperContext.RaiseUtf8(
      'Unexpected type % (%) for [%]', [typAsName^, ord(typ), typName]);
  end;
end;

constructor TWrapperContext.CreateFromUsedInterfaces(
  const aSourcePath, aDescriptions: TFileName);
var
  cache: TSynObjectListLightLocked;
  services: TDocVariantData;
  i: PtrInt;
begin
  Create(aSourcePath, aDescriptions);
  cache := TInterfaceFactory.GetUsedInterfaces;
  if cache = nil then
    exit;
  {%H-}services.InitFast;
  cache.Safe.ReadLock;
  try
    for i := 0 to cache.Count - 1 do
      services.AddItem(_ObjFast([
        'interfaceName',
          TInterfaceFactory(cache.List[i]).InterfaceRtti.Name,
        'methods', ContextFromMethods(cache.List[i])]));
  finally
    cache.Safe.ReadUnLock;
  end;
  fSOA.InitObject(['enabled',  true,
                   'services', variant(services)], JSON_FAST);
end;

function TWrapperContext.ContextArgsFromMethod(
  const meth: TInterfaceMethod): variant;
const
  DIRTODELPHI: array[TInterfaceMethodValueDirection] of string[7] = (
    'const', 'var', 'out', 'result');
  DIRTOSMS: array[TInterfaceMethodValueDirection] of string[7] = (
    // no OUT in DWS/SMS -> VAR instead
    'const', 'var', 'var', 'result');
var
  a, r: PtrInt;
  ma: PInterfaceMethodArgument;
  arg: variant;
  n: RawUtf8;
begin
  TDocVariant.NewFast(result);
  r := 0;
  ma := pointer(meth.Args);
  for a := 1 to high(meth.Args) do
  begin
    inc(ma);
    arg := ContextFromRtti(TYPES_SOA[ma^.ValueType], ma^.ArgRtti);
    n := meth.ArgsName[a];
    _ObjAddProps([
      'argName',   n,
      'lowerName', LowerCase(n),
      'camelName', LowerCamelCase(n),
      'snakeName', SnakeCase(n),
      'uriName',   UriCase(n),
      'argType',   ma^.ArgTypeName^,
      'dir',       ord(ma^.ValueDirection),
      'dirName',   DIRTODELPHI[ma^.ValueDirection],
      'dirNoOut',  DIRTOSMS[ma^.ValueDirection]], arg);
    if ma^.IsInput then
      _ObjAddProp('dirInput', true, arg);
    if ma^.IsOutput then
      _ObjAddProp('dirOutput', true, arg);
    if ma^.ValueDirection = imdResult then
      _ObjAddProp('dirResult', true, arg);
    if a < meth.ArgsNotResultLast then
      _ObjAddPropU('commaArg', '; ', arg);
    if a = high(meth.Args) then
      _ObjAddProp('isArgLast', true, arg);
    if (ma^.IsInput) and
       (a < meth.ArgsInLast) then
      _ObjAddPropU('commaInSingle', ',', arg);
    if (ma^.ValueDirection in [imdVar, imdOut]) and
       (a < meth.ArgsOutNotResultLast) then
      _ObjAddPropU('commaOut', '; ', arg);
    if ma^.IsOutput then
    begin
      _ObjAddProps(['indexOutResult', UInt32ToUtf8(r) + ']'], arg);
      inc(r);
      if a < meth.ArgsOutLast then
        _ObjAddPropU('commaOutResult', '; ', arg);
    end;
    TDocVariantData(result).AddItem(arg);
  end;
end;

function TWrapperContext.ContextFromMethod(
  const meth: TInterfaceMethod): variant;
const
  VERB_DELPHI: array[boolean] of string[9] = (
    'procedure', 'function');
var
  d: variant;
begin
  result := _ObjFast([
    'methodName',      meth.Uri,
    'camelName',       LowerCamelCase(meth.Uri),
    'snakeName',       SnakeCase(meth.Uri),
    'uriName',         UriCase(meth.Uri),
    'methodIndex',     meth.ExecutionMethodIndex,
    'verb',            VERB_DELPHI[meth.ArgsResultIndex >= 0],
    'args',            ContextArgsFromMethod(meth),
    'argsOutputCount', meth.ArgsOutputValuesCount]);
  if self <> nil then
  begin
    // can be called as TWraperContext(nil).ContextFromMethod
    d := fDescriptions.GetValueOrNull(meth.InterfaceDotMethodName);
    if VarIsEmptyOrNull(d) then
      RawUtf8ToVariant(meth.InterfaceDotMethodName, d);
    _ObjAddProp('methodDescription', d, result);
  end;
  if meth.ArgsInFirst >= 0 then
    _ObjAddProp('hasInParams', true, result);
  if meth.ArgsOutFirst >= 0 then
  begin
    _ObjAddProp('hasOutParams', true, result);
    if meth.ArgsOutNotResultLast > 0 then
      _ObjAddProp('hasOutNotResultParams', true, result);
  end;
  if imfResultIsServiceCustomAnswer in meth.Flags then
    _ObjAddProp('resultIsServiceCustomAnswer', true, result);
  if imfIsInherited in meth.Flags then
    _ObjAddProp('isInherited', true, result);
end;

procedure TWrapperContext.AddUnit(
  const aUnitName: ShortString; addAsProperty: PVariant);
var
  unitName: variant;
  i: PtrInt;
begin
  if (aUnitName = '') or
     IdemPropName(aUnitName, 'mORMot') then
    exit;
  RawUtf8ToVariant(@aUnitName[1], ord(aUnitName[0]), unitName);
  if addAsProperty <> nil then
    _ObjAddProp('unitName', unitName, addAsProperty^);
  if (self = nil) or
     (fUnits.SearchItemByValue(unitName) >= 0) then
    // already registered
    exit;
  fUnits.AddItem(unitName);
  if fSourcePath = nil then
    exit;
  for i := 0 to high(fSourcePath) do
    FillDescriptionFromSource(fDescriptions,
      FormatString('%%.pas', [fSourcePath[i], aUnitName]));
end;

function TWrapperContext.ContextFromMethods(int: TInterfaceFactory): variant;
var
  m: PtrInt;
  methods: TDocVariantData; // circumvent FPC -O2 memory leak
begin
  AddUnit(int.InterfaceRtti.Info^.InterfaceUnitName^, nil);
  {%H-}methods.InitFast;
  for m := 0 to int.MethodsCount - 1 do
    methods.AddItem(ContextFromMethod(int.Methods[m]));
  result := variant(methods);
end;

function TWrapperContext.ContextOneProperty(const prop: TRttiCustomProp;
  const parentName: RawUtf8): variant;
var
  l, level: PtrInt;
  fullName: RawUtf8;
  isSimple: variant;
begin
  level := 0;
  if parentName = '' then
    fullName := prop.Name
  else
  begin
    Join([parentName, '.', prop.Name], fullName);
    for l := 1 to length(fullName) do
      if fullName[l] = '.' then
        inc(level);
  end;
  result := ContextFromRtti(wUnknown, prop.Value, '', fullName);
  _ObjAddProps([
    'propName',     prop.Name,
    'camelName',    LowerCamelCase(prop.Name),
    'snakeName',    SnakeCase(prop.Name),
    'uriName',      UriCase(prop.Name),
    'fullPropName', fullName], result);
  if level > 0 then
    _ObjAddPropU('nestedIdentation', RawUtf8OfChar(' ', level * 2), result);
  SetVariantNull(isSimple);
  if rcfWithoutRtti in prop.Value.Flags then
    case prop.Value.Parser of
      ptRecord:
        _ObjAddProps([
          'nestedRecord', _ObjFast([
            'nestedRecord', null,
            'fields',  ContextNestedProperties(prop.Value, fullName)])], result);
      ptArray,
      ptDynArray:
        _ObjAddProps([
          'nestedRecordArray', _ObjFast([
            'nestedRecordArray', null,
            'fields', ContextNestedProperties(prop.Value, fullName)])], result);
    else
      if not TDocVariantData(result).Exists('toVariant') then
        isSimple := true;
    end
  else if not TDocVariantData(result).Exists('toVariant') then
    isSimple := true;
  _ObjAddProp('isSimple', isSimple, result);
end;

function TWrapperContext.Context: variant;

  procedure AddDescription(var list: TDocVariantData;
    const propName, descriptionName: RawUtf8);
  var
    i: PtrInt;
    propValue: RawUtf8;
  begin
    if (list.Kind <> dvArray) or
       (fDescriptions.Count = 0) then
      exit;
    for i := 0 to list.Count - 1 do
      with _Safe(list.Values[i])^ do
        if GetAsRawUtf8(propName, propValue) then
          AddValue(descriptionName, fDescriptions.GetValueOrNull(propValue));
  end;

begin
  // compute the Model information as JSON
  result := _ObjFast([
    'time',          NowToString,
    'year',          CurrentYear,
    'mORMotVersion', SYNOPSE_FRAMEWORK_VERSION,
    'Executable',    VarStringOrNull(StringToUtf8(Executable.Version.DetailedOrVoid)),
    'exeInfo',       Executable.Version.VersionInfo,
    'exeName',       Executable.ProgramName]);
  if fORM.Count > 0 then
    _ObjAddProps([
      'hasorm', true,
      'orm',  variant(fORM)], result);
  if fSOA.Count > 0 then
    _ObjAddProp('soa', variant(fSOA), result);
  if fRecords.Count > 0 then
  begin
    AddDescription(fRecords, 'name', 'recordDescription');
    _ObjAddProps(['records',     variant(fRecords),
                  'withRecords', true,
                  'withHelpers', true], result);
  end;
  if fEnumerates.Count > 0 then
  begin
    AddDescription(fEnumerates, 'name', 'enumDescription');
    _ObjAddProps(['enumerates',     variant(fEnumerates),
                  'withEnumerates', true,
                  'withHelpers',    true], result);
  end;
  if fSets.Count > 0 then
  begin
    AddDescription(fSets, 'name', 'setDescription');
    _ObjAddProps(['sets',        variant(fSets),
                  'withsets',    true,
                  'withHelpers', true], result);
  end;
  if fArrays.Count > 0 then
  begin
    _ObjAddProps(['arrays',      variant(fArrays),
                  'withArrays',  true,
                  'withHelpers', true], result);
  end;
  if fUnits.Count > 0 then
    _ObjAddProp('units', fUnits, result);
end;


function ContextFromUsedInterfaces(
  const aSourcePath, aDescriptions: TFileName): variant;
begin
  with TWrapperContext.CreateFromUsedInterfaces(aSourcePath, aDescriptions) do
  try
    result := Context;
  finally
    Free;
  end;
end;

function ContextFromMethod(const method: TInterfaceMethod): variant;
begin
  result := TWrapperContext(nil).ContextFromMethod(method);
end;

function ContextFromMethods(int: TInterfaceFactory): variant;
begin
  result := TWrapperContext(nil).ContextFromMethods(int);
end;


{ ************ Documentation Extraction from Source Code Comments }

function ResourceDescriptionFromSource(const ResourceDestFileName: TFileName;
  const SourceFileNames: array of TFileName;
  const JsonDestFileName: TFileName): variant;
var
  desc: TDocVariantData absolute result;
  i: PtrInt;
  json: RawUtf8;
begin
  VarClear(result);
  desc.InitFast;
  for i := 0 to high(SourceFileNames) do
    FillDescriptionFromSource(desc, SourceFileNames[i]);
  json := desc.ToJson;
  if JsonDestFileName <> '' then
    JsonReformatToFile(json, JsonDestFileName);
  FileFromString(AlgoSynLZ.Compress(json), ResourceDestFileName);
end;

procedure FillDescriptionFromSource(var Descriptions: TDocVariantData;
  const SourceFileName: TFileName);
var
  desc, typeName, interfaceName: RawUtf8;
  P: PUtf8Char;
  withinCode: boolean;

  procedure IgnoreIfDef;
  begin
    // ignore any $ifdef ... $endif lines (should be at the line beginning)
    repeat
      P := GotoNextLine(P);
      if P = nil then
        exit;
    until IdemPChar(GotoNextNotSpace(P), '{$ENDIF');
    P := GotoNextLine(P);
  end;

begin
  P := pointer(StringFromFile(SourceFileName));
  if P = nil then
    exit;
  withinCode := false;
  repeat
    // rough parsing of the .pas unit file to extract /// description
    P := GotoNextNotSpace(P);
    if IdemPChar(P, 'IMPLEMENTATION') then
      break; // only the "interface" section is parsed
    if IdemPChar(P, '{$IFDEF ') then
    begin
      IgnoreIfDef;
      P := GotoNextNotSpace(P);
    end;
    if (P[0] = '/') and
       (P[1] = '/') and
       (P[2] = '/') then
    begin
      desc := GetNextLine(GotoNextNotSpace(P + 3), P);
      if desc = '' then
        break;
      desc[1] := UpCase(desc[1]);
      repeat
        if P = nil then
          exit;
        P := GotoNextNotSpace(P);
        if IdemPChar(P, '{$IFDEF ') then
          IgnoreIfDef
        else if (P[0] = '/') and
                (P[1] = '/') then
        begin
          if P[2] = '/' then
            inc(P, 3)
          else
            inc(P, 2);
          P := GotoNextNotSpace(P);
          if P^ in ['$', '!'] then
          begin
            if not withinCode then
            begin
              withinCode := true;
              desc := desc + #13#10#13#10'----'; // AsciiDoc source code block
            end;
            desc := desc + #13#10;
            inc(P);
          end
          else if P^ = '-' then
          begin
            desc := desc + #13#10#13#10'-' + DESCRIPTION_ITEM_PREFIX;
            inc(P);
          end
          else
            desc := desc + ' ';
          desc := desc + GetNextLine(P, P);
        end
        else
          break;
      until false;
      if withinCode then
      begin
        // code block should end the description
        desc := desc + #13#10'----';
        withinCode := false;
      end;
      GetNextItem(P, ' ', typeName);
      if P = nil then
        exit;
      if typeName <> '' then
        if P^ = '=' then
        begin
          // simple type (record, array, enumeration, set)
          if Descriptions.GetValueIndex(typeName) < 0 then
          begin
            Descriptions.AddValue(typeName, RawUtf8ToVariant(desc));
            if typeName[1] = 'I' then
              interfaceName := Copy(typeName, 2, 128)
            else
              interfaceName := '';
          end;
        end
        else if {%H-}interfaceName <> '' then
          if PropNameEquals(typeName, 'function') or
             PropNameEquals(typeName, 'procedure') then
            if GetNextFieldProp(P, typeName) then
              Descriptions.AddValue(interfaceName + '.' + typeName,
                RawUtf8ToVariant(desc));
    end
    else
      P := GotoNextLine(P);
  until (P = nil);
end;


procedure InitializeUnit;
begin
  GlobalInterfaceResolver :=
    RegisterGlobalShutdownRelease(TInterfaceResolverList.Create);
  GlobalInterfaceResolver.Add(TypeInfo(IAutoLocker), TAutoLocker);
  GlobalInterfaceResolver.Add(TypeInfo(ILockedDocVariant), TLockedDocVariant);
  InterfaceFactoryCache :=
    RegisterGlobalShutdownRelease(TSynObjectListLightLocked.Create);
end;


initialization
  InitializeUnit;

end.

