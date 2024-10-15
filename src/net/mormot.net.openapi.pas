/// OpenAPI Client Code Generation
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.openapi;

{
  *****************************************************************************

  OpenAPI Language-agnostic Interface to HTTP APIs
  - OpenAPI Document Wrappers
  - FPC/Delphi Pascal Client Code Generation

  *****************************************************************************

  In Respect to existing OpenAPI wrappers in Delphi (or FPC):
   - Use high-level pascal records and dynamic arrays for "object" DTOs
   - Use high-level pascal enumerations and sets for "enum" values
   - Translate HTTP status error codes into high-level pascal Exceptions
   - Recognize similar "properties" or "enum" to reuse the same pascal type
   - Support of nested "$ref" for objects, parameters or types
   - Support "allOf" attribute, with proper properties inheritance/overloading
   - Support "oneOf" attribute, for strings or alternate record types
   - Support of "in":"header" and "in":"cookie" parameter attributes
   - Fallback to variant pascal type for "oneOf" or "anyOf" JSON values
   - Generated source code units are very small and easy to use, read and debug
   - Can generate very detailed comment documentation in the unit source code
   - Tunable engine, with plenty of generation options (e.g. about verbosity)
   - Leverage the mORMot RTTI and JSON kernel for its internal plumbing
   - Compatible with FPC and oldest Delphi (7-2009)
   - Tested with several Swagger 2 and OpenAPI 3 reference content
  But still not fully compliant to all existing files: feedback is welcome!

  REFERENCE: https://swagger.io/docs/specification
  TODO:  - operation: "multipart/form-data" in "consumes" and "in":"formdata"
         - authentification as in global "securityDefinitions"
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
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.data, // for TRawUtf8List
  mormot.core.variants;


{ ************************************ OpenAPI Document Wrappers }

type
  /// exception class raised by this Unit
  EOpenApi = class(ESynException);

  TOpenApiParser = class; // forward declaration

  /// define the Swagger (oav2) or OpenAPI 3.x (oav3) schema
  TOpenApiVersion = (
    oavUnknown,
    oav2,
    oav3);

  /// define the native built-in pascal types
  TOpenApiBuiltInType = (
    obtVariant,
    obtRecord,
    obtInteger,
    obtInt64,
    obtBoolean,
    obtEnumerationOrSet,
    obtSingle,
    obtDouble,
    obtDate,
    obtDateTime,
    obtGuid,
    obtRawUtf8,
    obtSpiUtf8,
    obtString,
    obtRawByteString);

  /// define the location of an OpenAPI parameter ('"in" field)
  TOpenApiParamLocation = (
    oplUnsupported,
    oplPath,
    oplQuery,
    oplBody,
    oplHeader,
    oplCookie,
    oplFormData);

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Schema
  POpenApiSchema = ^TOpenApiSchema;
  /// a dynamic array of pointers wrapper to OpenAPI Schema definition(s)
  POpenApiSchemaDynArray = array of POpenApiSchema;

  /// high-level OpenAPI Schema wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiSchema = record
  {$else}
  TOpenApiSchema = object
  {$endif USERECORDWITHMETHODS}
  private
    function GetPropertyByName(const aName: RawUtf8): POpenApiSchema;
  public
    /// transtype the POpenApiSchema pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Schema information
    function _Type: RawUtf8;
    function _Format: RawUtf8;
    function Required: PDocVariantData;
    function Enum: PDocVariantData;
    function Nullable: boolean;
    function Description: RawUtf8;
    function ExampleAsText: RawUtf8;
    function PatternAsText: RawUtf8;
    function Reference: RawUtf8;
    function AllOf: POpenApiSchemaDynArray;
    function Items: POpenApiSchema;
    function ItemsOrNil: POpenApiSchema;
    function Properties: PDocVariantData;
    property _Property[const aName: RawUtf8]: POpenApiSchema
      read GetPropertyByName;
    // high-level OpenAPI Schema Helpers
    function IsArray: boolean;
    function IsObject: boolean;
    function IsNamedEnum: boolean;
    function BuiltInType(Parser: TOpenApiParser): TOpenApiBuiltInType;
    function HasDescription: boolean;
    function HasItems: boolean;
    function HasProperties: boolean;
    function HasExample: boolean;
    function HasPattern: boolean;
  end;

  /// high-level OpenAPI Schema wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiResponse = record
  {$else}
  TOpenApiResponse = object
  {$endif USERECORDWITHMETHODS}
  public
    /// transtype the POpenApiResponse pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Response information
    function Description: RawUtf8;
    function Schema(Parser: TOpenApiParser): POpenApiSchema;
  end;
  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Response
  POpenApiResponse = ^TOpenApiResponse;

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI RequestBody
  // - share the very same fields as TOpenApiResponse
  POpenApiRequestBody = {$ifdef USERECORDWITHMETHODS} type {$endif}POpenApiResponse;

  /// high-level OpenAPI Parameter wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiParameter = record
  {$else}
  TOpenApiParameter = object
  {$endif USERECORDWITHMETHODS}
  public
    /// transtype the POpenApiParameter pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Parameter information
    function Name: RawUtf8;
    function Description: RawUtf8;
    function AsPascalName: RawUtf8;
    function _In: RawUtf8;
    function Location: TOpenApiParamLocation;
    function AllowEmptyValues: boolean;
    function Default: PVariant;
    function Required: boolean;
    function Schema(Parser: TOpenApiParser): POpenApiSchema;
  end;
  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Parameter
  POpenApiParameter = ^TOpenApiParameter;

  /// high-level OpenAPI Parameter wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiParameters = record
  {$else}
  TOpenApiParameters = object
  {$endif USERECORDWITHMETHODS}
  private
    function GetParameterByIndex(aIndex: integer): POpenApiParameter;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// transtype the POpenApiParameters pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Parameters information
    function Count: integer;
    property Parameter[aIndex: integer]: POpenApiParameter
      read GetParameterByIndex;
  end;
  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Parameters
  POpenApiParameters = ^TOpenApiParameters;

  /// high-level OpenAPI Operation wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiOperation = record
  {$else}
  TOpenApiOperation = object
  {$endif USERECORDWITHMETHODS}
  private
    function GetResponseForStatusCode(aStatusCode: integer): POpenApiResponse;
  public
    /// transtype the POpenApiOperation pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Operation information
    function Id: RawUtf8;
    function Summary: RawUtf8;
    function Description: RawUtf8;
    function Tags: TRawUtf8DynArray;
    function Deprecated: boolean;
    function Parameters: POpenApiParameters;
    function RequestBody(Parser: TOpenApiParser): POpenApiRequestBody;
    function Responses: PDocVariantData;
    property Response[aStatusCode: integer]: POpenApiResponse
      read GetResponseForStatusCode;
  end;
  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Operation
  POpenApiOperation = ^TOpenApiOperation;

  /// high-level OpenAPI Path Item wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiPathItem = record
  {$else}
  TOpenApiPathItem = object
  {$endif USERECORDWITHMETHODS}
  private
    function GetOperationByMethod(aMethod: TUriMethod): POpenApiOperation;
  public
    /// transtype the POpenApiPathItem pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Path Item information
    function Summary: RawUtf8;
    function Description: RawUtf8;
    function Parameters: POpenApiParameters;
    property Method[aMethod: TUriMethod]: POpenApiOperation
      read GetOperationByMethod;
  end;
  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Path Item
  POpenApiPathItem = ^TOpenApiPathItem;

  /// high-level OpenAPI Tag wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiTag = record
  {$else}
  TOpenApiTag = object
  {$endif USERECORDWITHMETHODS}
  public
    /// transtype the POpenApiTag pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Tag information
    function Description: RawUtf8;
    function Name: RawUtf8;
  end;
  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Tag
  POpenApiTag = ^TOpenApiTag;

  /// high-level OpenAPI Specations wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiSpecs = record
  {$else}
  TOpenApiSpecs = object
  {$endif USERECORDWITHMETHODS}
  private
    function GetPathItemByName(const aPath: RawUtf8): POpenApiPathItem;
  public
    /// transtype the POpenApiSpecs pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the main OpenAPI Specifications information
    function Info: PDocVariantData;
    function BasePath: RawUtf8;
    function Paths: PDocVariantData;
    function Tags: PDocVariantData;
    function Version: RawUtf8;
    function VersionEnum: TOpenApiVersion;
    property Path[const aPath: RawUtf8]: POpenApiPathItem
      read GetPathItemByName;
    function Components: PDocVariantData;
    function Schemas(aVersion: TOpenApiVersion): PDocVariantData;
  end;
  /// pointer wrapper to TDocVariantData / variant content of OpenAPI Specs
  POpenApiSpecs = ^TOpenApiSpecs;


{ ************************************ FPC/Delphi Pascal Client Code Generation }

  TPascalCustomType = class;
  TPascalParameter = class;
  TPascalRecord = class;
  TPascalRecordDynArray = array of TPascalRecord;

  /// define any Pascal type, as basic type of custom type
  TPascalType = class
  protected
    fParser: TOpenApiParser;
    fBuiltinSchema: POpenApiSchema;
    fBuiltInTypeName: RawUtf8;
    fCustomType: TPascalCustomType;
    fBuiltInType: TOpenApiBuiltInType;
    fIsArray, fNoConst: boolean;
    function GetSchema: POpenApiSchema;
    procedure SetArray(AValue: boolean);
  public
    constructor CreateBuiltin(aParser: TOpenApiParser;
      aBuiltInType: TOpenApiBuiltInType; aSchema: POpenApiSchema = nil;
      aIsArray: boolean = false); overload;
    constructor CreateCustom(aCustomType: TPascalCustomType); overload;

    // TODO: Handle RecordArrayType in RTTI definition
    function ToPascalName(AsFinalType: boolean = true;
      NoRecordArrayTypes: boolean = false): RawUtf8;
    function ToFormatUtf8Arg(const VarName: RawUtf8): RawUtf8;
    function ToDefaultParameterValue(aParam: TPascalParameter): RawUtf8;

    function IsBuiltin: boolean;
    function IsEnum: boolean;
    function IsRecord: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    property IsArray: boolean
      read fIsArray write SetArray;

    property CustomType: TPascalCustomType
      read fCustomType;
    property BuiltInType: TOpenApiBuiltInType
      read fBuiltInType;
    property Schema: POpenApiSchema
      read GetSchema;
  end;
  TPascalTypeObjArray = array of TPascalType;

  /// abstract parent class to define Pascal grammar items
  TPascalAbstract = class
  protected
    fName: RawUtf8;
    fPascalName: RawUtf8;
    fParser: TOpenApiParser;
    fSchema: POpenApiSchema;
  public
    property Name: RawUtf8
      read fName;
    property PascalName: RawUtf8
      read fPascalName;
    property Schema: POpenApiSchema
      read fSchema;
  end;

  /// define a Pascal property
  TPascalProperty = class(TPascalAbstract)
  private
    fType: TPascalType;
    fTypeOwned: boolean;
  public
    constructor CreateFromSchema(aParser: TOpenApiParser; const aName: RawUtf8;
      aSchema: POpenApiSchema);
    constructor CreateFrom(aAnother: TPascalProperty);
    constructor CreateBuiltin(aParser: TOpenApiParser; const aName, aPascalName: RawUtf8;
      aBuiltInType: TOpenApiBuiltInType);
    procedure ConvertToVariant;
    destructor Destroy; override;
    property PropType: TPascalType
      read fType;
  end;

  /// abstract parent class holder for complex types
  TPascalCustomType = class(TPascalAbstract)
  protected
    fFromRef: RawUtf8;
    fRequiresArrayDefinition: boolean;
  public
    constructor Create(aParser: TOpenApiParser);
    procedure ToTypeDefinition(W: TTextWriter); virtual; abstract;
    function ToArrayTypeName(AsFinalType: boolean = true): RawUtf8; virtual;
    function ToArrayTypeDefinition: RawUtf8; virtual;
  end;

  /// define a Pascal data structure, as a packed record with RTTI
  TPascalRecord = class(TPascalCustomType)
  private
    fProperties: TRawUtf8List; // Objects are owned TPascalProperty
    fRttiTextRepresentation: RawUtf8;
    fTypes: set of TOpenApiBuiltInType;
    fNeedsDummyField, fIsVoidVariant: boolean;
    procedure ResolveDependencies(var all, pending: TPascalRecordDynArray);
  public
    constructor Create(aParser: TOpenApiParser; const SchemaName: RawUtf8;
      Schema: POpenApiSchema = nil);
    destructor Destroy; override;
    procedure CopyProperties(aDest: TPascalRecord);
    procedure ToTypeDefinition(W: TTextWriter); override;
    function ToRttiTextRepresentation: RawUtf8;
    function ToRttiRegisterDefinitions: RawUtf8;
    property Properties: TRawUtf8List
      read fProperties;
  end;

  /// define a Pascal enumeration type
  TPascalEnum = class(TPascalCustomType)
  private
    fPrefix, fConstTextArrayName: RawUtf8;
    fChoices: TDocVariantData;
    fDynArrayEnum: boolean; // true if fChoices.Count > ENUM_MAX = 64
  public
    constructor Create(aParser: TOpenApiParser; const aName: RawUtf8;
      aSchema: POpenApiSchema);
    procedure ToTypeDefinition(W: TTextWriter); override;
    procedure ToRegisterCustom(W: TTextWriter);
    function ToArrayTypeName(AsFinalType: boolean = true): RawUtf8; override;
    procedure ToConstTextArray(W: TTextWriter);
    property Prefix: RawUtf8
      read fPrefix;
  end;

  /// define a custom Pascal EJsonClient type (used for 4xx responses)
  TPascalException = class(TPascalCustomType)
  private
    fResponse: POpenApiResponse;
    fErrorType: TPascalType;
    fErrorTypeName: RawUtf8;
    fErrorCode: RawUtf8;
  public
    constructor Create(aParser: TOpenApiParser; const aCode: RawUtf8;
      aResponse: POpenApiResponse = nil);
    destructor Destroy; override;
    procedure ToTypeDefinition(W: TTextWriter); override;
    procedure Body(W: TTextWriter);
    property Response: POpenApiResponse
      read fResponse;
    property ErrorType: TPascalType
      read fErrorType;
    property ErrorCode: RawUtf8
      read fErrorCode;
  end;

  /// define a Pascal method parameter matching an OpenAPI operation parameter
  TPascalParameter = class(TPascalAbstract)
  protected
    fParameter: POpenApiParameter;
    fLocation: TOpenApiParamLocation;
    fType: TPascalType;
    fRequired: boolean;
    fDefault: PVariant;
  public
    constructor Create(aParser: TOpenApiParser; aParam: POpenApiParameter);
    destructor Destroy; override;
    property Parameter: POpenApiParameter
      read fParameter;
    property Location: TOpenApiParamLocation
      read fLocation;
    property ParamType: TPascalType
      read fType;
  end;
  TPascalParameterDynArray = array of TPascalParameter;

  /// define a Pascal method matching an OpenAPI operation
  TPascalOperation = class
  private
    fParser: TOpenApiParser;
    fOperationId: RawUtf8;
    fFunctionName: RawUtf8;
    fMethod, fPath: RawUtf8;
    fPathItem: POpenApiPathItem;
    fOperation: POpenApiOperation;
    fRequestBody: POpenApiRequestBody;
    fRequestBodySchema: POpenApiSchema;
    fPayloadParameterType: TPascalType;
    fSuccessResponseType: TPascalType;
    fSuccessResponseCode: integer;
    fOnErrorIndex: integer;
    fParameters: TPascalParameterDynArray;
  public
    constructor Create(aParser: TOpenApiParser; const aMethod, aPath: RawUtf8;
      aPathItem: POpenApiPathItem; aOperation: POpenApiOperation);
    destructor Destroy; override;
    procedure ResolveResponseTypes;
    procedure Documentation(W: TTextWriter);
    procedure Declaration(W: TTextWriter; const ClassName: RawUtf8;
      InImplementation: boolean);
    procedure Body(W: TTextWriter; const ClassName, BasePath: RawUtf8);
    property Operation: POpenApiOperation
      read fOperation;
    property OperationId: RawUtf8
      read fOperationId;
    property FunctionName: RawUtf8
      read fFunctionName;
    property Parameters: TPascalParameterDynArray
      read fParameters;
  end;
  TPascalOperationDynArray = array of TPascalOperation;

  TPascalOperationsByTag = record
    TagName: RawUtf8;
    Tag: POpenApiTag;
    Operations: TPascalOperationDynArray;
  end;
  TPascalOperationsByTagDynArray = array of TPascalOperationsByTag;


  /// allow to customize TOpenApiParser process
  // - opoNoEnum disables any pascal enumeration type generation
  // - opoNoDateTime disables any pascal TDate/TDateTime type generation
  // - opoDtoNoDescription generates no Description comment for the DTOs
  // - opoDtoNoRefFrom generates no 'from #/....' comment for the DTOs
  // - opoDtoNoExample generates no 'Example:' comment for the DTOs
  // - opoDtoNoPattern generates no 'Pattern:' comment for the DTOs
  // - opoClientExcludeDeprecated removes any operation marked as deprecated
  // - opoClientNoDescription generates only the minimal comments for the client
  // - opoClientNoException won't generate any exception, and fallback to EJsonClient
  // - opoClientOnlySummary will reduce the verbosity of operation comments
  // - opoGenerateSingleApiUnit will let GenerateClient return a single
  // {name}.api unit, containing both the needed DTOs and the client class
  // - opoGenerateStringType will generate plain string types instead of RawUtf8
  // - opoGenerateOldDelphiCompatible will generate a void/dummy managed field for
  // Delphi 7/2007/2009 compatibility and avoid 'T... has no type info' errors,
  // and also properly support Unicode or unfinished/nested record type definitions
  // - see e.g. OPENAPI_CONCISE for a single unit, simple and undocumented output
  TOpenApiParserOption = (
    opoNoEnum,
    opoNoDateTime,
    opoDtoNoDescription,
    opoDtoNoRefFrom,
    opoDtoNoExample,
    opoDtoNoPattern,
    opoClientExcludeDeprecated,
    opoClientNoDescription,
    opoClientNoException,
    opoClientOnlySummary,
    opoGenerateSingleApiUnit,
    opoGenerateStringType,
    opoGenerateOldDelphiCompatible);
  TOpenApiParserOptions = set of TOpenApiParserOption;

  /// the main OpenAPI parser and pascal code generator class
  TOpenApiParser = class
  protected
    fLineEnd: RawUtf8;
    fLineIndent: RawUtf8;
    fSpecs: TOpenApiSpecs;
    fInfo: PDocVariantData;
    fSchemas: PDocVariantData;
    fRecords: TRawUtf8List;    // objects are owned TPascalRecord
    fEnums: TRawUtf8List;      // objects are owned TPascalEnum
    fExceptions: TRawUtf8List; // objects are owned TPascalException
    fErrorHandler: TRawUtf8DynArray;
    fOperations: TPascalOperationDynArray;
    fName: RawUtf8;
    fTitle, fGeneratedBy, fGeneratedByLine: RawUtf8;
    fVersion: TOpenApiVersion;
    fOptions: TOpenApiParserOptions;
    fEnumCounter, fDtoCounter: integer;
    fDtoUnitName, fClientUnitName, fClientClassName: RawUtf8;
    fOrderedRecords: TPascalRecordDynArray;
    fEnumPrefix: TRawUtf8DynArray;
    function ParseRecordDefinition(const aDefinitionName: RawUtf8;
      aSchema: POpenApiSchema; aTemporary: boolean = false): TPascalRecord;
    procedure ParsePath(const aPath: RawUtf8; aPathItem: POpenApiPathItem);
    function NewPascalTypeFromSchema(aSchema: POpenApiSchema;
      aSchemaName: RawUtf8 = ''): TPascalType;
    function GetRecord(aRecordName: RawUtf8; aSchema: POpenApiSchema;
      NameIsReference: boolean = false): TPascalRecord;
    function GetOrderedRecords: TPascalRecordDynArray;
    function GetOperationsByTag: TPascalOperationsByTagDynArray;
    function GetSchemaByName(const aName: RawUtf8): POpenApiSchema;
    function GetRef(aRef: RawUtf8): pointer;
    procedure Description(W: TTextWriter; const Described: RawUtf8);
    procedure Comment(W: TTextWriter; const Args: array of const;
      const Desc: RawUtf8 = '');
    procedure Code(W: TTextWriter; var Line: RawUtf8; const Args: array of const);
    // main internal parsing function
    procedure ParseSpecs;
    // main internal code generation methods
    procedure GenerateDtoInterface(w: TTextWriter);
    procedure GenerateDtoImplementation(w: TTextWriter);
  public
    /// initialize this parser instance
    // - aName is a short identifier used for naming the units and types
    constructor Create(const aName: RawUtf8; aOptions: TOpenApiParserOptions = []);
    /// finalize this parser instance
    destructor Destroy; override;

    /// parse a JSON Swagger/OpenAPI file content
    procedure ParseFile(const aJsonFile: TFileName);
    /// parse a JSON Swagger/OpenAPI content
    procedure ParseJson(const aJson: RawUtf8);
    /// parse a Swagger/OpenAPI content tree from an existing TDocVariant
    procedure ParseData(const aSpecs: TDocVariantData);
    /// clear all internal information of this parser instance
    procedure Clear;

    /// generate the DTO unit content
    function GenerateDtoUnit: RawUtf8;
    /// generate the main Client unit content
    // - including all DTOs if the opoGenerateSingleApiUnit option is defined
    function GenerateClientUnit: RawUtf8;
    /// generate the DTO and main Client unit .pas files in a given folder
    // - no DTO unit is written if the opoGenerateSingleApiUnit option is defined
    procedure ExportToDirectory(const DirectoryName: TFileName = '');

    /// allow to customize the code generation
    property Options: TOpenApiParserOptions
      read fOptions write fOptions;
    /// the line feed content to be used for code generation
    property LineEnd: RawUtf8
      read fLineEnd;
    /// short identifier of a few chars, used as prefix for code generation
    // - used e.g. for computing the default unit names
    // - also used when generating TDto### and TEnum### types, to avoid any
    // conflict at RTTI level between the names, especially if several OpenAPI
    // generated units do coexist in the project
    property Name: RawUtf8
      read fName write fName;
    /// the unit identifier name used for the DTO unit, without the '.pas' extension
    // - default value will be lowercase '{name}.dto'
    property DtoUnitName: RawUtf8
      read fDtoUnitName write fDtoUnitName;
    /// the unit identifier name used for the Client unit, without the '.pas' extension
    // - default value will be lowercase '{name}.client' or '{name}.api' if
    // the opoGenerateSingleApiUnit option is defined
    property ClientUnitName: RawUtf8
      read fClientUnitName write fClientUnitName;
    /// the class identifier name, by default 'T{Name}Client'
    property ClientClassName: RawUtf8
      read fClientClassName write fClientClassName;

    /// main storage of the whole OpenAPI specifications tree
    property Specs: TOpenApiSpecs
      read fSpecs;
    /// the layout version of the parsed specifications
    property Version: TOpenApiVersion
      read fVersion;
    /// the main title, as stored in the parsed specifications
    property Title: RawUtf8
      read fTitle;
    /// resolve a given Schema by its name
    property Schema[const aName: RawUtf8]: POpenApiSchema
      read GetSchemaByName;
    /// list all operations (aka Pascal methods) stored in the parsed specifications
    property Operations: TPascalOperationDynArray
      read fOperations;
  end;


const
  /// TOpenApiParser options to generate a single, simple and undocumented unit
  // - if the default two-units setup with full documentation seems too verbose
  OPENAPI_CONCISE = [
    opoGenerateSingleApiUnit,
    opoDtoNoDescription,
    opoClientNoDescription];


function ToText(t: TOpenApiBuiltInType): PShortString; overload;


implementation


{ ************************************ OpenAPI Document Wrappers }

{ TOpenApiSchema }

function TOpenApiSchema.AllOf: POpenApiSchemaDynArray;
var
  all: PDocVariantData;
  i: PtrInt;
begin
  result := nil;
  if not Data.GetAsArray('allOf', all) then
    exit;
  SetLength(result, all^.Count);
  for i := 0 to high(result) do
    result[i] := pointer(_Safe(all^.Values[i]));
end;

function TOpenApiSchema.Items: POpenApiSchema;
begin
  if not IsArray then
    EOpenApi.RaiseUtf8('TOpenApiSchema.Items on a non array: %', [_Type]);
  if not Data.GetAsObject('items', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSchema.ItemsOrNil: POpenApiSchema;
begin
  if not IsArray or
     not Data.GetAsObject('items', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSchema.HasItems: boolean;
begin
  result := IsArray and
            Data.Exists('items');
end;

function TOpenApiSchema.HasProperties: boolean;
begin
  result := Data.Exists('properties');
end;

function TOpenApiSchema.Properties: PDocVariantData;
begin
  if not Data.GetAsObject('properties', result) then
    result := nil;
end;

function TOpenApiSchema.GetPropertyByName(const aName: RawUtf8): POpenApiSchema;
begin
  if not Properties^.GetAsObject(aName, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSchema.Reference: RawUtf8;
begin
  result := Data.U['$ref']
end;

function TOpenApiSchema.Nullable: boolean;
begin
  result := Data.B['nullable'];
end;

function TOpenApiSchema.Required: PDocVariantData;
begin
  result := Data.A['required']
end;

function TOpenApiSchema.Description: RawUtf8;
begin
  result := TrimU(Data.U['description']);
end;

function TOpenApiSchema.HasDescription: boolean;
begin
  result := Description <> '';
end;

function TOpenApiSchema.Enum: PDocVariantData;
begin
  if not Data.GetAsArray('enum', result) then
    result := nil;
end;

function TOpenApiSchema.ExampleAsText: RawUtf8;
begin
  result := VariantSaveJson(Data.Value['example']);
end;

function TOpenApiSchema.PatternAsText: RawUtf8;
begin
  result := VariantSaveJson(Data.Value['pattern']);
end;

function TOpenApiSchema.HasExample: boolean;
begin
  result := Data.Exists('example');
end;

function TOpenApiSchema.HasPattern: boolean;
begin
  result := Data.Exists('pattern');
end;

function TOpenApiSchema._Format: RawUtf8;
begin
  result := Data.U['format'];
end;

function TOpenApiSchema._Type: RawUtf8;
begin
  result := Data.U['type'];
end;

function TOpenApiSchema.IsArray: boolean;
begin
  result := _Type = 'array';
end;

function TOpenApiSchema.IsObject: boolean;
begin
  result := _Type = 'object';
end;

function TOpenApiSchema.IsNamedEnum: boolean;
begin
  result := (Enum <> nil) and
            (_Format <> '');
end;

function TOpenApiSchema.BuiltInType(Parser: TOpenApiParser): TOpenApiBuiltInType;
var
  t, f, ref: RawUtf8;
  oo: PDocVariantData;
  s: POpenApiSchema;
  r: TPascalRecord;
  i: PtrInt;
begin
  result := obtVariant; // fallback type: anything up to TDocVariantData
  t := _Type;
  if t = '' then
  begin
    if Data.GetAsArray('oneOf', oo) then
    begin
      s := @oo^.Values[0];
      result := s^.BuiltInType(Parser);
      if result = obtVariant then
      begin
        if (s^.Reference = '') or
           (Description <> '') then
          exit;
        for i := 0 to oo^.Count - 1 do
        begin
          ref := POpenApiSchema(@oo^.Values[i])^.Reference;
          if ref = '' then
            continue;
          r := Parser.GetRecord(ref, nil, {isref=}true);
          if r <> nil then
            Append(t, r.PascalName, ' ');
        end;
        if t <> '' then
          Data.U['description'] := Make(['From the JSON of ', t]);
      end
      else
        for i := 1 to oo^.Count - 1 do
          if POpenApiSchema(@oo^.Values[i])^.BuiltInType(Parser) <> result then
          begin
            result := obtVariant; // all simple "type" should match
            exit;
          end;
    end;
  end
  else
  begin
    f := _Format;
    if t = 'integer' then
      if f = 'int64' then
        result := obtInt64
      else
        result := obtInteger
    else if t = 'number' then
      if f = 'float' then
        result := obtSingle
      else
        result := obtDouble
    else if t = 'boolean' then
      result := obtBoolean
    else if t = 'string' then
    begin
      result := obtRawUtf8;
      if f <> '' then
        if f = 'uuid' then
          result := obtGuid
        else if f = 'binary' then
          result := obtRawByteString
        else if f = 'password' then
          result := obtSpiUtf8
        else if not (opoNoDateTime in Parser.Options) then
          if f = 'date' then
            result := obtDate
          else if f = 'date-time' then
            result := obtDateTime;
    end;
  end;
  if opoGenerateStringType in Parser.Options then
    if result in [obtRawUtf8, obtSpiUtf8] then
      result := obtString;
end;


{ TOpenApiResponse }

function TOpenApiResponse.Description: RawUtf8;
begin
  result := TrimU(Data.U['description']);
end;

function TOpenApiResponse.Schema(Parser: TOpenApiParser): POpenApiSchema;
var
  ref, n: RawUtf8;
  c, o: PDocVariantData;
  i: PtrInt;
begin
  if @self = nil then
    result := nil
  else if Data.GetAsRawUtf8('$ref', ref) then
    result := POpenApiResponse(Parser.GetRef(ref)).Schema(Parser)
  else if Parser.Version = oav2 then
    result := POpenApiSchema(Data.O['schema'])
  else
  begin
    // we only support application/json (and some variations) here
    result := nil;
    c := Data.O['content'];
    for i := 0 to c.Count - 1 do
      if result = nil then
      begin
        n := c.Names[i];
        if IsContentTypeJson(pointer(n)) or
           (n = '*/*') or
           IdemPropNameU(n, 'application/jwt') then // exists in the wild :(
          if _SafeObject(c.Values[i], o) then
            result := POpenApiSchema(o.O['schema']);
      end;
    if result = nil then // allow fallback e.g. to text/plain schema:type:string
      for i := 0 to c.Count - 1 do
        if result = nil then
          if IdemPChar(pointer(c.Names[i]), 'TEXT/PLAIN') and
             _SafeObject(c.Values[i], o) then
               result := POpenApiSchema(o.O['schema']);
  end;
  if (result <> nil) and
     (result^.Data.Count = 0) then
    result := nil; // no such schema
end;


{ TOpenApiParameter }

function TOpenApiParameter.AllowEmptyValues: boolean;
begin
  result := Data.B['allowEmptyValue'];
end;

function TOpenApiParameter.Description: RawUtf8;
begin
  result := TrimU(Data.U['description']);
end;

function TOpenApiParameter.Default: PVariant;
begin
  result := Data.GetPVariantByPath('default');
end;

function TOpenApiParameter._In: RawUtf8;
begin
  result := Data.U['in'];
end;

function TOpenApiParameter.Location: TOpenApiParamLocation;
var
  i: integer;
begin
  i := GetEnumNameValue(TypeInfo(TOpenApiParamLocation), _In, {trimlowcase=}true);
  result := oplUnsupported;
  if i >= 0 then
    result := TOpenApiParamLocation(i);
end;

function TOpenApiParameter.Name: RawUtf8;
begin
  result := Data.U['name'];
end;

function TOpenApiParameter.Required: boolean;
begin
  result := Data.B['required'];
end;

function TOpenApiParameter.Schema(Parser: TOpenApiParser): POpenApiSchema;
begin
  if Parser.Version = oav2 then
    result := POpenApiSchema(@self)
  else if not Data.GetAsObject('schema', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiParameter.AsPascalName: RawUtf8;
begin
  result := Name;
  if result <> '' then
    result := SanitizePascalName(result, {keywordcheck:}true);
end;


{ TOpenApiTag }

function TOpenApiTag.Description: RawUtf8;
begin
  if @self = nil then
    result := ''
  else
    result := TrimU(Data.U['description']);
end;

function TOpenApiTag.Name: RawUtf8;
begin
  result := Data.U['name'];
end;


{ TOpenApiPathItem }

function TOpenApiPathItem.Description: RawUtf8;
begin
  result := TrimU(Data.U['description']);
end;

function TOpenApiPathItem.GetOperationByMethod(aMethod: TUriMethod): POpenApiOperation;
begin
  if not Data.GetAsObject(ToText(aMethod), PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiPathItem.Parameters: POpenApiParameters;
begin
  result := POpenApiParameters(Data.A['parameters']);
end;

function TOpenApiPathItem.Summary: RawUtf8;
begin
  result := Data.U['summary'];
end;


{ TOpenApiParameters }

function TOpenApiParameters.GetParameterByIndex(aIndex: integer): POpenApiParameter;
begin
  if cardinal(aIndex) >= cardinal(Data.Count) then
    EOpenApi.RaiseUtf8('Out of range TOpenApiParameters[%]', [aIndex]);
  result := @Data.Values[aIndex];
end;

function TOpenApiParameters.Count: integer;
begin
  result := Data.Count;
end;


{ TOpenApiOperation }

function TOpenApiOperation.Deprecated: boolean;
begin
  result := Data.B['deprecated'];
end;

function TOpenApiOperation.Description: RawUtf8;
begin
  result := TrimU(Data.U['description']);
end;

function TOpenApiOperation.Id: RawUtf8;
begin
  result := Data.U['operationId'];
end;

function TOpenApiOperation.Parameters: POpenApiParameters;
begin
  result := POpenApiParameters(Data.A['parameters']);
end;

function TOpenApiOperation.RequestBody(Parser: TOpenApiParser): POpenApiRequestBody;
var
  p: POpenApiParameters;
  i: integer;
begin
  if Parser.Version = oav2 then
  begin
    p := Parameters;
    if p <> nil then
      for i := 0 to p.Count - 1 do
        if p.Parameter[i]._In = 'body' then
        begin
          result := POpenApiRequestBody(p.Parameter[i]);
          exit;
        end;
    result := nil;
  end
  else if not Data.GetAsObject('requestBody', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiOperation.GetResponseForStatusCode(
  aStatusCode: integer): POpenApiResponse;
var
  nam: RawUtf8;
begin
  if aStatusCode = 0 then
    nam := 'default'
  else
    UInt32ToUtf8(aStatusCode, nam);
  if not Responses^.GetAsObject(nam, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiOperation.Responses: PDocVariantData;
begin
  result := Data.O['responses'];
end;

function TOpenApiOperation.Summary: RawUtf8;
begin
  result := TrimU(Data.U['summary']);
end;

function TOpenApiOperation.Tags: TRawUtf8DynArray;
begin
  Data.A['tags']^.ToRawUtf8DynArray(result);
end;


{ TOpenApiSpecs }

function TOpenApiSpecs.GetPathItemByName(const aPath: RawUtf8): POpenApiPathItem;
begin
  if not Paths^.GetAsObject(aPath, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSpecs.Info: PDocVariantData;
begin
  result := Data.O['info'];
end;

function TOpenApiSpecs.BasePath: RawUtf8;
begin
  result := Data.U['basePath'];
end;

function TOpenApiSpecs.Paths: PDocVariantData;
begin
  result := Data.O['paths'];
end;

function TOpenApiSpecs.Tags: PDocVariantData;
begin
  result := Data.A['tags'];
end;

function TOpenApiSpecs.Version: RawUtf8;
begin
  if not Data.GetAsRawUtf8('swagger', result) then
    result := Data.U['openapi'];
end;

function TOpenApiSpecs.VersionEnum: TOpenApiVersion;
var
  v: RawUtf8;
begin
  result := oavUnknown;
  v := Version;
  if v <> '' then
    if v[1] = '2' then
      result := oav2  // Swagger 2.0 layout
    else if v[1] = '3' then
      result := oav3; // OpenAPI 3.x layout
end;

function TOpenApiSpecs.Components: PDocVariantData;
begin
  result := Data.O['components'];
end;

function TOpenApiSpecs.Schemas(aVersion: TOpenApiVersion): PDocVariantData;
begin
  if aVersion = oav2 then
    result := Data.O['definitions']
  else
    result := Components.O['schemas'];
end;



{ ************************************ FPC/Delphi Pascal Code Generation }

function ToText(t: TOpenApiBuiltInType): PShortString;
begin
  result := GetEnumName(TypeInfo(TOpenApiBuiltInType), ord(t));
end;


{ TPascalCustomType }

constructor TPascalCustomType.Create(aParser: TOpenApiParser);
begin
  // inheriting constructor should have set fName
  if fName = '' then
    EOpenApi.RaiseUtf8('%.Create(name?)', [self]);
  fParser := aParser;
  if (fName[1] = '#') or // ensure type name is not too long
     (length(fName) > 70) or
     // DotNet generates e.g. /schemas/System.Tupple`2[[...,...]]`
     (PosExChar('`', fName) > 0) then
  begin
    inc(fParser.fDtoCounter); // TDto### is simple and convenient
    Make(['TDto', fParser.Name, fParser.fDtoCounter], fPascalName);
  end
  else
    fPascalName := 'T' + SanitizePascalName(fName, {keywordcheck:}false);
end;

function TPascalCustomType.ToArrayTypeName(AsFinalType: boolean): RawUtf8;
begin
  if AsFinalType then
    FormatUtf8('%DynArray', [PascalName], result)
  else
    FormatUtf8('array of %', [PascalName], result);
end;

function TPascalCustomType.ToArrayTypeDefinition: RawUtf8;
begin
  if fRequiresArrayDefinition then
    FormatUtf8('%% = %;%', [fParser.fLineIndent, ToArrayTypeName({final=}true),
      ToArrayTypeName(false), fParser.LineEnd], result)
  else
    result := '';
end;


{ TPascalParameter }

constructor TPascalParameter.Create(aParser: TOpenApiParser;
  aParam: POpenApiParameter);
begin
  fParser := aParser;
  if (aParam.Data.Count = 1) and
     (aParam.Data.Names[0] = '$ref') then // resolve as reference
    aParam := fParser.GetRef(ToUtf8(aParam.Data.Values[0]));
  fParameter := aParam;
  fName := aParam.Name;
  fPascalName := aParam.AsPascalName;
  fLocation := aParam.Location;
  fRequired := aParam.Required;
  fDefault := aParam.Default;
  fSchema := aParam.Schema(fParser);
  fType := fParser.NewPascalTypeFromSchema(aParam.Schema(fParser));
end;

destructor TPascalParameter.Destroy;
begin
  fType.Free;
  inherited Destroy;
end;


{ TPascalOperation }

constructor TPascalOperation.Create(aParser: TOpenApiParser;
  const aMethod, aPath: RawUtf8; aPathItem: POpenApiPathItem;
  aOperation: POpenApiOperation);
var
  p, o: POpenApiParameters;
  pn, i, j, c: PtrInt;
  n: RawUtf8;
begin
  fParser := aParser;
  fPath := aPath;
  fPathItem := aPathItem;
  fOperation := aOperation;
  fMethod := aMethod;
  // setup parameters
  p := fPathItem^.Parameters;
  pn := p.Count;
  o := fOperation^.Parameters;
  SetLength(fParameters, pn + o.Count);
  for i := 0 to pn - 1 do
    fParameters[i] := TPascalParameter.Create(fParser, p^.Parameter[i]);
  for i := 0 to o.Count - 1 do
    fParameters[pn + i] := TPascalParameter.Create(fParser, o^.Parameter[i]);
  for i := 1 to length(fParameters) - 1 do
  begin
    n := fParameters[i].PascalName;
    c := 0;
    for j := 0 to i - 1 do
      if IdemPropNameU(fParameters[j].PascalName, n) then // dup name
        inc(c);
    if c <> 0 then
      fParameters[i].fPascalName := n + UInt32ToUtf8(c + 1); // make unique
  end;
  // setup any request body
  fRequestBody := fOperation^.RequestBody(fParser);
  if fRequestBody <> nil then
    fRequestBodySchema := fRequestBody^.Schema(fParser);
  if fRequestBodySchema <> nil then
    fPayloadParameterType := fParser.NewPascalTypeFromSchema(fRequestBodySchema);
  // setup method/operation
  fOperationId := fOperation^.Id;
  if fOperationId = '' then // method + api as fallback to have something <> ''
    fOperationId := aMethod + aPath;
  fFunctionName := SanitizePascalName(fOperationId, {keywordcheck:}true);
  if fOperation^.Deprecated then
    Append(fFunctionName, '_deprecated');
end;

destructor TPascalOperation.Destroy;
begin
  fPayloadParameterType.Free;
  fSuccessResponseType.Free;
  ObjArrayClear(fParameters);
  inherited Destroy;
end;

procedure TPascalOperation.ResolveResponseTypes;
var
  code: integer;
  v: PDocVariantData;
  i: PtrInt;
  e: TPascalException;
  allexceptions: array of TPascalException;
  status, err, deferr, json: RawUtf8;
  resp: POpenApiResponse;
  schema: POpenApiSchema;
begin
  v := fOperation.Responses;
  v^.SortByName(@StrCompByNumber); // sort in-place by status number
  for i := 0 to v^.Count - 1 do
  begin
    status := v^.Names[i];
    resp := @v^.Values[i];
    code := Utf8ToInteger(status, 0);
    schema := resp^.Schema(fParser);
    if StatusCodeIsSuccess(code) or
       ((fSuccessResponseCode = 0) and
        (status = 'default')) then
    begin
      if fSuccessResponseType = nil then // handle the first success
      begin
        fSuccessResponseCode := code;
        if schema <> nil then
          fSuccessResponseType := fParser.NewPascalTypeFromSchema(schema);
      end;
    end
    else if Assigned(schema) and
            not (opoClientNoException in fParser.Options) then
    begin
      // generate a custom EJsonClient exception class for 4xx errors
      json := schema^.Data.ToJson;
      // we don't know PascalName until it is parsed so is json-indexed
      e := fParser.fExceptions.GetObjectFrom(json);
      if e = nil then
      begin
        e := TPascalException.Create(fParser, status, resp);
        fParser.fExceptions.AddObject(json, e);
      end
      else if e.ErrorCode <> status then
        e.fErrorcode := ''; // exception reused between status codes
      PtrArrayAddOnce(allexceptions, e);
      if code <> 0 then // generate "case of" block
        Append(err, ['    ', code, ':', fParser.LineEnd,
                     '      e := ',e.PascalName, ';', fParser.LineEnd])
      else if status = 'default' then
        Make(['  else', fParser.LineEnd,
              '    e := ', e.PascalName, ';', fParser.LineEnd], deferr);
    end;
  end;
  err := err + deferr; // for proper OnError## callback generation
  if err <> '' then
  begin
    if (length(allexceptions) = 1) and
       (deferr <> '') then
      err := deferr; // all codes pointed to the same default exception
    fOnErrorIndex := FindRawUtf8(fParser.fErrorHandler, err) + 1;
    if fOnErrorIndex = 0 then // new TOnJsonClientError
      fOnErrorIndex := AddRawUtf8(fParser.fErrorHandler, err) + 1;
  end;
end;

procedure TPascalOperation.Documentation(W: TTextWriter);
var
  p: TPascalParameter;
  v: PDocVariantData;
  status, desc, line: RawUtf8;
  code: integer;
  r: POpenApiResponse;
  i: PtrInt;
  rs: POpenApiSchema;
  e: TPascalException;
begin
  // Request Definition
  w.AddStrings([fParser.LineEnd, fParser.fLineIndent, '// ']);
  if fOperation^.Deprecated then
     w.AddShort('[DEPRECATED] ');
   w.AddStrings([ // do not use fOperationID here because may = Description
     fOperation^.Id, ' [', fMethod, '] ', fPath, fParser.LineEnd,
     fParser.fLineIndent, '//', fParser.LineEnd]);
  // Summary
  desc := fOperation^.Summary;
  if desc <> '' then
    fParser.Comment(w, ['Summary: ', desc]);
  // Description
  desc := fOperation^.Description;
  if desc <> '' then
  begin
    desc := StringReplaceAll(StringReplaceAll(desc, #10,
      FormatUtf8('%%//   ', [fParser.LineEnd, fParser.fLineIndent])), #13, '');
    w.AddStrings([
      fParser.fLineIndent, '// Description:', fParser.LineEnd,
      fParser.fLineIndent, '//   ', desc, fParser.LineEnd]);
  end;
  if (fParameters <> nil) or
     (fRequestBodySchema <> nil) then
  begin
    // Parameters
    w.AddStrings([fParser.fLineIndent, '//', fParser.LineEnd,
           fParser.fLineIndent, '// Params:', fParser.LineEnd]);
    for i := 0 to high(fParameters) do
    begin
      p := fParameters[i];
      if p.Location = oplBody then
        continue; // fRequestBodySchema is handled below
      Make(['- [', p.fParameter^._In, '] ', p.PascalName], line);
      if p.Location in [oplUnsupported, oplFormData] then
        Append(line, ' (unsupported)')
      else
      begin
        if p.fRequired then
          Append(line, ' (required)');
        if p.fDefault <> nil then
          Append(line, [' (default=', p.fDefault^, ')']);
      end;
      fParser.Comment(w, [line], p.Parameter^.Description);
    end;
    // Request body
    if Assigned(fRequestBodySchema) then
    begin
      line := '- [body] Payload  (required)';
      fParser.Comment(w, [line], fRequestBodySchema^.Description);
    end;
  end;
  // Responses
  v := fOperation^.Responses;
  if v^.Count > 0 then
  begin
    w.AddStrings([fParser.fLineIndent, '//', fParser.LineEnd,
                  fParser.fLineIndent, '// Responses:', fParser.LineEnd]);
    for i := 0 to v^.Count - 1 do
    begin
      status := v^.Names[i];
      code := Utf8ToInteger(status, 0);
      r := @v^.Values[i];
      rs := r^.Schema(fParser);
      Make(['- ', status], line);
      if code = fSuccessResponseCode then
        Append(line, ' (main)')
      else if Assigned(rs) and
              not (opoClientNoException in fParser.Options) then
      begin
        e := fParser.fExceptions.GetObjectFrom(rs^.Data.ToJson);
        if e <> nil then // no need to parse, just recognize schema
          Append(line, [' [', e.PascalName, ']']);
      end;
      fParser.Comment(w, [line], r^.Description);
    end;
  end;
end;

const
  _CONST: array[boolean] of string[7] = ('const ', '');

procedure TPascalOperation.Declaration(W: TTextWriter; const ClassName: RawUtf8;
  InImplementation: boolean);
var
  p: TPascalParameter;
  i: PtrInt;
  decl, line: RawUtf8;
  prev, hasdefault: boolean;
  def: TRawUtf8DynArray;

  procedure AddParam(const Args: array of const);
  begin
    if prev then
      Append(line, '; ')
    else
      prev := true;
    fParser.Code(W, line, Args);
  end;

begin
  prev := false;
  if Assigned(fSuccessResponseType) then
    line := 'function '
  else
    line := 'procedure ';
  if ClassName <> '' then
    Append(line, ClassName, '.');
  Append(line, FunctionName, '(');
  // path + query parmaeters
  for i := 0 to Length(fParameters) - 1 do
  begin
    p := fParameters[i];
    if p.Location in [oplPath, oplQuery, oplHeader, oplCookie] then
    begin
      hasdefault := ((p.fDefault <> nil) or
                     not p.fRequired) and
                    (p.fType.IsArray or
                     not (p.fType.fBuiltInType in [obtVariant, obtRecord, obtGuid]));
      Make([_CONST[p.fType.fNoConst], p.PascalName, ': ',
        p.fType.ToPascalName({final=}true)], decl);
      // here p.fType.ToPascalName needs final=true for nil default value
      if hasdefault then
        if InImplementation then // same order, but no "= default" statement
          AddRawUtf8(def, decl)
        else
          AddRawUtf8(def, Make([decl, ' = ', p.fType.ToDefaultParameterValue(p)]))
      else
        AddParam([decl]);
    end;
  end;
  // body parameter as payload record type
  if Assigned(fPayloadParameterType) then
    AddParam(['const Payload: ', fPayloadParameterType.ToPascalName]);
  // eventual default parameters
  for i := 0 to high(def) do
    AddParam([def[i]]);
  // function result
  if Assigned(fSuccessResponseType) then
    Append(line, ['): ', fSuccessResponseType.ToPascalName, ';'])
  else
    Append(line, ');');
  w.AddString(Line);
end;

procedure TPascalOperation.Body(W: TTextWriter;
  const ClassName, BasePath: RawUtf8);
var
  url: RawUtf8;
  urlName: TRawUtf8DynArray;
  urlParam, queryParam, headerParam: TIntegerDynArray;
  i, j, o: PtrInt;
  p: TPascalParameter;

  procedure AppendParams(const params: TIntegerDynArray);
  var
    i, j: PtrInt;
    p: TPascalParameter;
  begin
    if params <> nil then
    begin
      w.AddStrings([', [', fParser.LineEnd]);
      for i := 0 to high(params) do
      begin
        j := params[i];
        p := fParameters[j];
        if i > 0 then
          w.AddStrings([',', fParser.LineEnd]);
        w.AddShorter('    ''');
        case p.Location of
          oplQuery:
            if p.ParamType.IsArray then
              w.AddDirect('*'); // ueStarNameIsCsv for UrlEncodeFull()
          // oplHeader uses natively CSV in OpenAPI default "simple" style
          oplCookie:
            w.AddShorter('Cookie: ');
            // warning: arrays may not be properly written in cookies
        end;
        w.AddStrings([p.Name, ''', ', p.ParamType.ToFormatUtf8Arg(p.PascalName)]);
      end;
      w.AddDirect(']');
    end
    else
      w.AddShorter(', []');
  end;

begin
  // parse the URI and extract all {parameter} names
  url := BasePath;
  o := 1;
  repeat // /pets/{petId}/  -> /pets/%/
    i := PosEx('{', fPath, o);
    if i = 0 then
      break;
    j := PosEx('}', fPath, i);
    if j = 0 then
      EOpenApi.RaiseUtf8('%.Body: missing } in [%]', [self, fPath]);
    AddRawUtf8(urlName, copy(fPath, i + 1, j - i - 1));
    Append(url, [copy(fPath, o, i - o), '%']);
    o := j + 1;
  until false;
  Append(url, copy(fPath, o, 255));
  if length(urlName) > 1 then
    url := url;
  SetLength(urlParam, length(urlName));
  for i := 0 to high(urlParam) do
    urlParam[i] := -1;
  // recognize supplied parameters
  for i := 0 to high(fParameters) do
  begin
    p := fParameters[i];
    case p.Location of
      oplPath:
        begin
          j := FindPropName(urlName, p.Name);
          if j < 0 then
            EOpenApi.RaiseUtf8('%.Body: unknown % in [%]', [self, p.Name, fPath]);
          urlParam[j] := i;
        end;
      oplQuery:
        AddInteger(queryParam, i);
      oplHeader,
      oplCookie:
        AddInteger(headerParam, i);
    end;
  end;
  for i := 0 to high(urlParam) do
    if urlParam[i] < 0 then
      EOpenApi.RaiseUtf8('%.Body: missing {%} in [%]', [self, urlName[i], fPath]);
  // emit the body block with its declaration and Request() call
  Declaration(w, ClassName, {implemtation=}true);
  w.AddStrings([fParser.LineEnd, 'begin', fParser.LineEnd,
         '  fClient.Request(''', UpperCase(fMethod), ''', ''', url, '''']);
  // Path parameters
  w.AddShorter(', [');
  for i := 0 to Length(urlName) - 1 do
  begin
    j := urlParam[i];
    if j < 0 then
      EOpenApi.RaiseUtf8('%.Body: unknown {%} in [%]', [self, urlName[i], fPath]);
    p := fParameters[j];
    if i > 0 then
      w.AddShorter(', ');
    w.AddString(p.ParamType.ToFormatUtf8Arg(p.PascalName));
  end;
  w.AddDirect(']');
  // Query and Header parameters
  AppendParams(queryParam);
  AppendParams(headerParam);
  // Payload and potentially result
  if Assigned(fPayloadParameterType) then
  begin
    w.AddStrings([',', fParser.LineEnd,
      '    Payload, ']);
    if Assigned(fSuccessResponseType) then
      w.AddShorter('result')
    else
      w.AddShort('{dummy:}self');
    w.AddStrings([', TypeInfo(', fPayloadParameterType.ToPascalName, '), ']);
    if Assigned(fSuccessResponseType) then
      w.AddStrings(['TypeInfo(', fSuccessResponseType.ToPascalName, ')'])
    else
      w.AddShorter('nil');
  end
  // result with no Payload
  else if Assigned(fSuccessResponseType) then
    w.AddStrings([',', fParser.LineEnd,
      '    result, TypeInfo(', fSuccessResponseType.ToPascalName, ')']);
  // custom Exception error callback
  if fOnErrorIndex <> 0 then
    w.AddStrings([', OnError', SmallUInt32Utf8[fOnErrorIndex]]);
  // end body block
  w.AddStrings([');', fParser.LineEnd,
                'end;', fParser.LineEnd, fParser.LineEnd]);
end;


{ TPascalProperty }

constructor TPascalProperty.CreateFromSchema(aParser: TOpenApiParser;
  const aName: RawUtf8; aSchema: POpenApiSchema);
begin
  fParser := aParser;
  fType := aParser.NewPascalTypeFromSchema(aSchema, '');
  fTypeOwned := true;
  fName := aName;
  fSchema := aSchema;
  fPascalName := SanitizePascalName(fName, {keywordcheck:}true);
end;

constructor TPascalProperty.CreateFrom(aAnother: TPascalProperty);
begin
  fParser := aAnother.fParser;
  fType := aAnother.PropType; // weak copy (keep fTypeOwned=false)
  fName := aAnother.Name;
  fSchema := aAnother.Schema;
  fPascalName := aAnother.PascalName;
end;

constructor TPascalProperty.CreateBuiltin(aParser: TOpenApiParser;
  const aName, aPascalName: RawUtf8; aBuiltInType: TOpenApiBuiltInType);
begin
  fParser := aParser;
  fType := TPascalType.CreateBuiltin(fParser, aBuiltInType);
  fTypeOwned := true;
  fName := aName;
  fPascalName := aPascalName;
end;

destructor TPascalProperty.Destroy;
begin
  if fTypeOwned then
    fType.Free;
  inherited Destroy;
end;

procedure TPascalProperty.ConvertToVariant;
begin
  if fTypeOwned then
    fType.Free;
  fType := TPascalType.CreateBuiltin(fParser, obtVariant); // e.g. a TDocVariant
end;


{ TPascalEnum }

constructor TPascalEnum.Create(aParser: TOpenApiParser;
  const aName: RawUtf8; aSchema: POpenApiSchema);
var
  i: PtrInt;
  p: RawUtf8;
begin
  fName := aName;
  inherited Create(aParser);
  fSchema := aSchema;
  fChoices.InitCopy(Variant(aSchema^.Enum^), JSON_FAST);
  fChoices.AddItem('None', 0); // always prepend a first void item
  fDynArrayEnum := fChoices.Count > ENUM_MAX; // use dynamic array, not set
  for i := 2 to length(fPascalName) do
    if length(p) >= 3 then
      break
    else if fPascalName[i] in ['A' .. 'Z'] then
      Append(p, fPascalName[i]);
  if p = '' then
    p := 'e';
  fPrefix := p; // TUserRole -> 'UR'
  if FindPropName(fParser.fEnumPrefix, fPrefix) >= 0 then // 'ur' already exists
    for i := 2 to 100 do
    begin
      Make([p, i], fPrefix);
      if FindPropName(fParser.fEnumPrefix, fPrefix) < 0 then
        break; // TUserRole -> 'ur2' unique prefix for this enum type
    end;
  LowerCaseSelf(fPrefix); // TUserRole -> 'ur'
  AddRawUtf8(fParser.fEnumPrefix, fPrefix);
  FormatUtf8('%_TXT', [UpperCase(copy(fPascalName, 2, 100))], fConstTextArrayName);
end;

procedure TPascalEnum.ToTypeDefinition(W: TTextWriter);
var
  line, item: RawUtf8;
  items: TRawUtf8DynArray;
  i: PtrInt;
begin
  if fSchema^.HasDescription and
     not (opoDtoNoDescription in fParser.Options) then
    fParser.Comment(W, [fSchema^.Description]);
  w.AddStrings([fParser.fLineIndent, PascalName, ' = (', fParser.LineEnd,
    fParser.fLineIndent, '  ']);
  for i := 0 to fChoices.Count - 1 do
  begin
    if i = 0 then
      item := 'None'
    else
    begin
      Append(line, ', ');
      CamelCase(ToUtf8(fChoices.Values[i]), item);
      if item <> '' then
        item[1] := UpCase(item[1]);
      if (item = '') or
         (FindPropName(items, item) >= 0) then
        Append(item, [i]); // duplicated, or no ascii within -> make unique
    end;
    AddRawUtf8(items, item);
    fParser.Code(w, line, [fPrefix, item]);
  end;
  w.AddStrings([line, ');', fParser.LineEnd,
    ToArrayTypeDefinition]);
end;

procedure TPascalEnum.ToRegisterCustom(W: TTextWriter);
begin
  w.AddStrings(['    TypeInfo(', fPascalName, ')']);
  if fRequiresArrayDefinition and
     not fDynArrayEnum then
    w.AddStrings([', TypeInfo(', ToArrayTypeName, ')'])
  else
    w.AddShorter(', nil');
  w.AddStrings([', @', fConstTextArrayName]);
end;

function TPascalEnum.ToArrayTypeName(AsFinalType: boolean): RawUtf8;
begin
  if fDynArrayEnum then
    result := inherited ToArrayTypeName(AsFinalType)
  else if AsFinalType then
    FormatUtf8('%Set', [PascalName], result)
  else
    FormatUtf8('set of %', [PascalName], result);
end;

procedure TPascalEnum.ToConstTextArray(W: TTextWriter);
var
  i: integer;
  line, item: RawUtf8;
begin
  w.AddStrings([
    fParser.fLineIndent, fConstTextArrayName,
      ': array[', PascalName, '] of RawUtf8 = (', fParser.LineEnd]);
  line := fParser.fLineIndent + '  '''', ';  // first entry is for None/Default
  for i := 1 to fChoices.Count - 1 do
  begin
    item := mormot.core.unicode.QuotedStr(VariantToUtf8(fChoices.Values[i]));
    if i < fChoices.Count - 1 then
      Append(item, ', ');
    fParser.Code(w, line, [item]);
  end;
  w.AddStrings([line, ');', fParser.LineEnd]);
end;


{ TPascalType }

function TPascalType.IsBuiltin: boolean;
begin
  result := not Assigned(fCustomType);
end;

function TPascalType.GetSchema: POpenApiSchema;
begin
  if Assigned(CustomType) then
    result := CustomType.Schema
  else
    result := fBuiltinSchema;
end;

function TPascalType.IsEnum: boolean;
begin
  result := Assigned(fCustomType) and
            (fCustomType is TPascalEnum);
end;

function TPascalType.IsRecord: boolean;
begin
  result := Assigned(fCustomType) and
            (fCustomType is TPascalRecord);
end;

procedure TPascalType.SetArray(AValue: boolean);
begin
  fIsArray := AValue;
  if AValue then
  begin
    fNoConst := false;
    if Assigned(fCustomType) then
      fCustomType.fRequiresArrayDefinition := true;
  end
  else
    fNoConst := fBuiltInType in [obtInteger .. obtDateTime]
end;

const
  OBT_TXT: array[TOpenApiBuiltInType] of RawUtf8 = (
    'variant',        // obtVariant
    '',               // obtRecord
    'integer',        // obtInteger
    'Int64',          // obtInt64
    'boolean',        // obtBoolean
    '',               // obtEnumerationOrSet
    'single',         // obtSingle
    'double',         // obtDouble
    'TDate',          // obtDate
    'TDateTime',      // obtDateTime
    'TGuid',          // obtGuid
    'RawUtf8',        // obtRawUtf8
    'SpiUtf8',        // obtSpiUtf8
    'string',         // obtString
    'RawByteString'); // obtRawByteString

  OBT_DEFAULT: array[TOpenApiBuiltInType] of RawUtf8 = (
    'null',           // obtVariant
    '',               // obtRecord
    '0',              // obtInteger
    '0',              // obtInt64
    'false',          // obtBoolean
    '',               // obtEnumerationOrSet
    '0',              // obtSingle
    '0',              // obtDouble
    '0',              // obtDate
    '0',              // obtDateTime
    '',               // obtGuid
    '''''',           // obtRawUtf8
    '''''',           // obtSpiUtf8
    '''''',           // obtString
    '''''');          // obtRawByteString

constructor TPascalType.CreateBuiltin(aParser: TOpenApiParser;
  aBuiltInType: TOpenApiBuiltInType; aSchema: POpenApiSchema; aIsArray: boolean);
begin
  fParser := aParser;
  fBuiltInType := aBuiltInType;
  fBuiltInTypeName := OBT_TXT[aBuiltInType];
  if fBuiltInTypeName = '' then
    EOpenApi.RaiseUtf8('Unexpected %.CreateBuiltin(%)', [self, ToText(aBuiltInType)^]);
  fBuiltinSchema := aSchema;
  SetArray(aIsArray);
end;

constructor TPascalType.CreateCustom(aCustomType: TPascalCustomType);
begin
  fParser := aCustomType.fParser;
  fCustomType := aCustomType;
  if IsEnum then
    fBuiltInType := obtEnumerationOrSet
  else if IsRecord then
    fBuiltInType := obtRecord;
end;

function TPascalType.ToPascalName(AsFinalType, NoRecordArrayTypes: boolean): RawUtf8;
begin
  if Assigned(CustomType) then
  begin
    result := CustomType.PascalName;
    if (result = 'variant') or
       not IsArray then
      exit;
    if AsFinalType and NoRecordArrayTypes and IsRecord then
      AsFinalType := false;
    result := CustomType.ToArrayTypeName(AsFinalType);
  end
  else
  begin
    result := fBuiltInTypeName;
    if (fBuiltInType = obtVariant) or // emit "array of variant" as "variant"
       not IsArray then
      exit;
    if AsFinalType then
    begin
      if result[1] <> 'T' then
      begin
        result[1] := UpCase(result[1]);
        insert('T', result, 1);
      end;
      Append(result, 'DynArray'); // use mormot.core.base arrays
    end
    else
      result := Make(['array of ', result]);
  end;
end;

function TPascalType.ToFormatUtf8Arg(const VarName: RawUtf8): RawUtf8;
var
  func: RawUtf8;
  e: TPascalEnum;
begin
  result := VarName; // default to direct value
  if IsBuiltin then
    if IsArray then
      case fBuiltInType of
        obtInteger:
          func := 'IntegerDynArrayToCsv(%)';
        obtInt64:
          func := 'Int64DynArrayToCsv(%)';
        obtRawUtf8,
        obtSpiUtf8:
          func := 'RawUtf8ArrayToCsv(%)';
        obtString:
          func := 'RawUtf8ArrayToCsv(StringDynArrayToRawUtf8DynArray(%))';
        obtGuid:
          func := 'GuidArrayToCsv(%)';
        // other types would just fail to compile
      end
    else
    case fBuiltInType of
      obtDate:
        func := 'DateToIso8601(%, true)';
      obtDateTime:
        func := 'DateTimeToIso8601(%, true)';
      obtGuid:
        func := 'NotNullGuidToUtf8(%)'; // GUID_NULL = '' to ignore param
      obtRawByteString:
        func := 'mormot.core.buffers.BinToBase64(%)';
      obtString:
        if opoGenerateOldDelphiCompatible in fParser.Options then
          func := 'StringToUtf8(%)'; // not needed if has a codepage
    end
  else if IsEnum then
  begin
    e := fCustomType as TPascalEnum;
    func := e.fConstTextArrayName; // ###_TXT[]
    if IsArray then
      if e.fDynArrayEnum then
        FormatUtf8('GetEnumArrayNameCustom(%, %, @%)',
          [VarName, e.fChoices.Count, func], result)
      else
        FormatUtf8('GetSetNameCustom(TypeInfo(%), %, @%)',
          [e.PascalName, VarName, func], result)
    else
      FormatUtf8('%[%]', [func, VarName], result);
    exit;
  end;
  if func <> '' then
    FormatUtf8(func, [VarName], result);
end;

function TPascalType.ToDefaultParameterValue(aParam: TPascalParameter): RawUtf8;
var
  def: PVariant;
  e: TPascalEnum;
begin
  def := aParam.fDefault;
  if Assigned(def) and
     not VarIsEmptyOrNull(def^) then
  begin
    // explicit default value
    if PVarData(def)^.VType = varBoolean then
      result := BOOL_UTF8[PVarData(def)^.VBoolean] // normalize
    else if VariantToUtf8(def^, result) then
      result := QuotedStr(result); // single quoted pascal string
  end
  else if IsEnum then
  begin
    e := CustomType as TPascalEnum;
    if IsArray then
      if e.fDynArrayEnum then
        result := 'nil' // dynamic array
      else
        result := '[]' // set
    else
      result := e.Prefix + 'None'; // first enum
  end
  else if IsArray then
    result := 'nil'
  else
  begin
    result := OBT_DEFAULT[BuiltInType];
    if result = '' then
      EOpenApi.RaiseUtf8('Unsupported %.ToDefaultParameterValue(%)',
        [self, ToText(BuiltInType)^]);
  end;
end;


{ TPascalRecord }

constructor TPascalRecord.Create(aParser: TOpenApiParser;
  const SchemaName: RawUtf8; Schema: POpenApiSchema);
begin
  fName := SchemaName;
  fSchema := Schema;
  fProperties := TRawUtf8List.CreateEx([fObjectsOwned, fCaseSensitive, fNoDuplicate]);
  inherited Create(aParser);
end;

destructor TPascalRecord.Destroy;
begin
  fProperties.Free;
  inherited Destroy;
end;

procedure TPascalRecord.ToTypeDefinition(W: TTextWriter);
var
  i: PtrInt;
  p: TPascalProperty;
  s: POpenApiSchema;
begin
  if fIsVoidVariant then
    exit;
  if (fFromRef <> '') and
     (fParser.Options * [opoDtoNoRefFrom, opoDtoNoDescription] = []) then
    fParser.Comment(w, ['from ', fFromRef]);
  // generate the record type definition
  w.AddStrings([fParser.fLineIndent, PascalName, ' = packed record', fParser.LineEnd]);
  for i := 0 to fProperties.Count - 1 do
  begin
    p := fProperties.ObjectPtr[i];
    if not (opoDtoNoDescription in fParser.Options) then
    begin
      s := p.fSchema;
      if Assigned(s) and
         not s.HasDescription then
        s := pointer(s.ItemsOrNil); // fallback to enum "description"
      if Assigned(s) and
         s^.HasDescription then
      begin
        fParser.fLineIndent := fParser.fLineIndent + '  ';
        fParser.Comment(w, [s^.Description]);
        SetLength(fParser.fLineIndent, length(fParser.fLineIndent) - 2);
        if (not (opoDtoNoExample in fParser.Options)) and
           s^.HasExample then
          w.AddStrings([fParser.fLineIndent,
            '  // - Example: ', s^.ExampleAsText, fParser.LineEnd]);
        if (not (opoDtoNoPattern in fParser.Options)) and
           s^.HasPattern then
          w.AddStrings([fParser.fLineIndent,
            '  // - Pattern: ', s^.PatternAsText, fParser.LineEnd]);
      end;
    end;
    w.AddStrings([
      fParser.fLineIndent, '  ', p.PascalName, ': ',
        p.PropType.ToPascalName({final=}true, {noarray=}p.PropType.CustomType = self),
        ';', fParser.LineEnd]);
  end;
  if fNeedsDummyField then
    w.AddStrings([
      fParser.fLineIndent, '  // for Delphi 7-2009 compatibility', fParser.LineEnd,
      fParser.fLineIndent, '  dummy_: RawUtf8;', fParser.LineEnd]);
  // associated pointer (and dynamic array if needed) definitions
  w.AddStrings([fParser.fLineIndent, 'end;', fParser.LineEnd,
    fParser.fLineIndent, 'P', copy(PascalName, 2, length(PascalName)),
      ' = ^', PascalName, ';', fParser.LineEnd,
    ToArrayTypeDefinition, fParser.LineEnd]);
end;

function TPascalRecord.ToRttiTextRepresentation: RawUtf8;
var
  i: PtrInt;
  p: TPascalProperty;
  line, name: RawUtf8;
begin
  result := fRttiTextRepresentation;
  if (result <> '') or
     fIsVoidVariant then
    exit;
  FormatUtf8('_% = ''', [PascalName], line);
  for i := 0 to fProperties.Count - 1 do
  begin
    if length(line) > 70 then // Delphi IDE is limited to 255 chars per line
    begin
      Append(result, [line, ''' +', fParser.LineEnd, '    ''']);
      line := '';
    end;
    name := fProperties[i];
    if not PropNameValid(pointer(name)) then
      name := QuotedStr(name, '"');
    p := fProperties.ObjectPtr[i];
    Append(line, [name, ':', p.PropType.ToPascalName(true, true), ' ']);
  end;
  if fNeedsDummyField then
    Append(line, '_:RawUtf8''')
  else
    line[length(line)] := '''';
  Append(result, line, ';');
  fRttiTextRepresentation := result; // cached internally
end;

function TPascalRecord.ToRttiRegisterDefinitions: RawUtf8;
begin
  FormatUtf8('TypeInfo(%), _%', [PascalName, PascalName], result);
end;

procedure TPascalRecord.CopyProperties(aDest: TPascalRecord);
var
  i: PtrInt;
  p: TPascalProperty;
begin
  if (self <> nil) and
     (aDest <> nil) then
    for i := 0 to fProperties.Count - 1 do
    begin
      p := fProperties.ObjectPtr[i];
      include(aDest.fTypes, p.fType.fBuiltInType);
      aDest.fProperties.AddObject(fProperties[i], TPascalProperty.CreateFrom(p));
    end;
end;

procedure TPascalRecord.ResolveDependencies(var all, pending: TPascalRecordDynArray);
var
  i: PtrInt;
  p: TPascalProperty;
  t: TPascalType;
begin
  PtrArrayAdd(pending, self); // avoid infinite recursion on nested fields
  for i := 0 to fProperties.Count - 1 do
  begin
    p := fProperties.ObjectPtr[i];
    t := p.PropType;
    if t.IsRecord and
       (t.CustomType <> self) and // ParseRecordDefinition() handled it
       (ObjArrayFind(all, t.CustomType) < 0) then
      if ObjArrayFind(pending, t.CustomType) < 0 then
        TPascalRecord(t.CustomType).ResolveDependencies(all, pending)
      else
        p.ConvertToVariant; // avoid infinite recursive definition
  end;
  PtrArrayAddOnce(all, self); // add eventually, if not already present
  PtrArrayDelete(pending, self);
end;


{ TPascalException }

constructor TPascalException.Create(aParser: TOpenApiParser; const aCode: RawUtf8;
  aResponse: POpenApiResponse);
begin
  fErrorCode := aCode;
  fResponse := aResponse;
  fErrorType := aParser.NewPascalTypeFromSchema(aResponse^.Schema(aParser));
  if Assigned(fErrorType.CustomType) then
    fName := fErrorType.CustomType.Name
  else if fErrorType.BuiltInType = obtRawUtf8 then
    fName := 'TextResponse'
  else
    EOpenApi.RaiseUtf8('%.Create: unsupported schema for %', [self, aResponse^.Data.ToJson]);
  fErrorTypeName := fErrorType.ToPascalName;
  inherited Create(aParser);
  fPascalName[1] := 'E'; // Txxxx -> Exxxx
end;

destructor TPascalException.Destroy;
begin
  fErrorType.Free;
  inherited Destroy;
end;

procedure TPascalException.ToTypeDefinition(W: TTextWriter);
begin
  if (fErrorCode <> '') and
     not (opoClientNoDescription in fParser.Options) then
    fParser.Comment(w, [
      'exception raised on ', fResponse.Description, ' (', fErrorCode, ')']);
  w.AddStrings([
    fParser.fLineIndent, PascalName, ' = class(EJsonClient)', fParser.LineEnd,
    fParser.fLineIndent, 'protected', fParser.LineEnd,
    fParser.fLineIndent, '  fError: ', fErrorTypeName, ';', fParser.LineEnd,
    fParser.fLineIndent, 'public', fParser.LineEnd,
    fParser.fLineIndent, '  constructor CreateResp(const Format: RawUtf8; const Args: array of const;', fParser.LineEnd,
    fParser.fLineIndent, '    const Resp: TJsonResponse); override;', fParser.LineEnd,
    fParser.fLineIndent, '  property Error: ', fErrorTypeName, fParser.LineEnd,
    fParser.fLineIndent, '    read fError;', fParser.LineEnd,
    fParser.fLineIndent, 'end;', fParser.LineEnd, fParser.LineEnd]);
end;

procedure TPascalException.Body(W: TTextWriter);
begin
  w.AddStrings(['{ ', PascalName, ' }', fParser.LineEnd, fParser.LineEnd,
    'constructor ', PascalName, '.CreateResp(const Format: RawUtf8;', fParser.LineEnd,
    '  const Args: array of const; const Resp: TJsonResponse);', fParser.LineEnd,
    'begin', fParser.LineEnd,
    '  inherited CreateResp(Format, Args, Resp);', fParser.LineEnd]);
  if fErrorType.BuiltInType = obtRawUtf8 then
    w.AddString(
      '  fError := Resp.Content;')
  else
    w.AddStrings([
      '  LoadJson(fError, Resp.Content, TypeInfo(', fErrorTypeName,'));']);
  w.AddStrings([fParser.LineEnd,
    'end;', fParser.LineEnd, fParser.LineEnd]);
end;


{ TOpenApiParser }

constructor TOpenApiParser.Create(const aName: RawUtf8; aOptions: TOpenApiParserOptions);
begin
  fName := aName;
  if fName <> '' then
    fName[1] := UpCase(fName[1]);
  fOptions := aOptions;
  fRecords := TRawUtf8List.CreateEx([fObjectsOwned, fCaseSensitive, fNoDuplicate]);
  fEnums := TRawUtf8List.CreateEx([fObjectsOwned, fCaseSensitive, fNoDuplicate]);
  fExceptions := TRawUtf8List.CreateEx([fObjectsOwned, fCaseSensitive, fNoDuplicate]);
  fLineEnd := CRLF; // default to OS value
  FormatUtf8('Generated % by % via % - DO NOT MODIFY BY HAND!',
    [NowTextDateShort, Executable.User, Executable.ProgramName], fGeneratedBy);
  fGeneratedByLine := RawUtf8OfChar('-', length(fGeneratedBy));
end;

destructor TOpenApiParser.Destroy;
begin
  fRecords.Free;
  fEnums.Free;
  fExceptions.Free;
  ObjArrayClear(fOperations);
  inherited Destroy;
end;

procedure TOpenApiParser.Clear;
begin
  fSpecs.Data.Clear;
  fRecords.Clear;
  fEnums.Clear;
  fExceptions.Clear;
  ObjArrayClear(fOperations);
  fInfo := nil;
  fSchemas := nil;
  fErrorHandler := nil;
  fOrderedRecords := nil;
  fEnumPrefix := nil;
  fEnumCounter := 0;
  fDtoCounter := 0;
  fDtoUnitName := '';
  fClientUnitName := '';
  fClientClassName := '';
  fTitle := '';
end;

procedure TOpenApiParser.ParseSpecs;
var
  i: PtrInt;
  s: POpenApiSchema;
  n: RawUtf8;
  v: PDocVariantData;
begin
  fVersion := fSpecs.VersionEnum;
  fInfo := fSpecs.Info;
  fTitle := fInfo^.U['title'];
  fSchemas := fSpecs.Schemas(fVersion);
  // generate all main DTOs - is not mandatory, but may help
  for i := 0 to fSchemas^.Count - 1 do
  begin
    s := @fSchemas^.Values[i];
    n := fSchemas^.Names[i];
    if s^.IsObject then
      if not fRecords.Exists(n) then // parse object once
        ParseRecordDefinition(n, s);
  end;
  // parse all operations
  v := fSpecs.Paths;
  for i := 0 to v^.Count - 1 do
    ParsePath(v^.Names[i], @v^.Values[i]);
end;

procedure TOpenApiParser.ParseData(const aSpecs: TDocVariantData);
begin
  Clear;
  fSpecs.Data := aSpecs; // fast by reference copy
  ParseSpecs;
end;

procedure TOpenApiParser.ParseJson(const aJson: RawUtf8);
begin
  Clear;
  fSpecs.Data.InitJson(aJson, JSON_FAST + [dvoInternNames]);
  ParseSpecs;
end;

procedure TOpenApiParser.ParseFile(const aJsonFile: TFileName);
begin
  Clear;
  fSpecs.Data.InitJsonFromFile(aJsonFile, JSON_FAST + [dvoInternNames]);
  ParseSpecs;
end;

function TOpenApiParser.GetSchemaByName(const aName: RawUtf8): POpenApiSchema;
begin
  if not fSchemas^.GetAsObject(aName, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiParser.GetRef(aRef: RawUtf8): pointer;
begin
  // e.g. "$ref": "#/components/parameters/JobID"
  result := nil;
  if (aRef = '') or
     (aRef[1] <> '#') then
    exit;
  if aRef[2] = '/' then
    delete(aRef, 1, 2)
  else
    delete(aRef, 1, 1); // malformed "#components/parameters/JobID" link
  fSpecs.Data.GetDocVariantByPath(aRef, PDocVariantData(result), '/');
end;

function PascalNameFromRef(const ref: RawUtf8): RawUtf8;
begin
  result := SplitRight(ref, '/');
end;

function TOpenApiParser.NewPascalTypeFromSchema(aSchema: POpenApiSchema;
  aSchemaName: RawUtf8): TPascalType;
var
  all: POpenApiSchemaDynArray;
  ref, fmt, nam: RawUtf8;
  i: integer;
  rec, rectemp: TPascalRecord;
  items: POpenApiSchema;
  enum, props: PDocVariantData;
  enumType: TPascalEnum;
begin
  if aSchema = nil then
    EOpenApi.RaiseUtf8('NewPascalTypeFromSchema(%): aSchema=nil', [aSchemaName]);
  all := aSchema^.AllOf;
  if length(all) = 1 then
    aSchema := all[0]; // typically a single "$ref": "..." entry
  ref := aSchema^.Reference;
  if ref <> '' then
  begin
    // #/definitions/NewPet -> NewPet
    aSchema := GetRef(ref); // resolve from main Specs
    if aSchema = nil then
      EOpenApi.RaiseUtf8('NewPascalTypeFromSchema: unknown $ref=%', [ref]);
    result := NewPascalTypeFromSchema(aSchema, PascalNameFromRef(ref));
    if (result.CustomType <> nil) and
       (result.CustomType.fFromRef = '') then
      result.CustomType.fFromRef := ref;
  end
  else if (all <> nil) or
          aSchema^.IsObject or
          aSchema^.HasProperties then
  begin
    // return a TPascalRecord custom type
    if aSchemaName = '' then
    begin
      // not from a $ref: need to compute a schema/type name from the properties
      props := aSchema^.Properties;
      if (props = nil) or
         (props^.Count = 0) then
      begin // object, but no properties: use a plain TDocVariant
        result := TPascalType.CreateBuiltin(self, obtVariant, aSchema);
        exit;
      end;
      nam := '#' + RawUtf8ArrayToCsv(props^.GetNames, '_'); // unique
      aSchemaName := nam;
      for i := 2 to 20 do // try if this type does not already exist as such
      begin
        rec := fRecords.GetObjectFrom(aSchemaName);
        if rec = nil then
          break;
        if fmt = '' then
        begin
          rectemp := ParseRecordDefinition(aSchemaName, aSchema, {temp=}true);
          fmt := rectemp.ToRttiTextRepresentation; // just field names and types
          rectemp.Free;
        end;
        if rec.ToRttiTextRepresentation = fmt then // same raw pascal definition
        begin
          result := TPascalType.CreateCustom(rec);
          exit;
        end;
        Make([nam, i], aSchemaName);
      end;
    end;
    result := TPascalType.CreateCustom(GetRecord(aSchemaName, aSchema));
  end
  else if aSchema^.IsArray then
  begin
    // retrieve the main item type but apply the "IsArray" flag
    items := aSchema^.Items;
    if (items = nil) or
       (items^.Data.Count = 0) then
      result := TPascalType.CreateBuiltin(self, obtVariant) // no definition
    else
    begin
      result := NewPascalTypeFromSchema(items, aSchemaName);
      result.fBuiltinSchema := aSchema;
      result.SetArray(true);
    end;
  end
  else
  begin
    // return a TPascalEnum custom type
    enum := aSchema^.Enum;
    if (enum <> nil) and
       not (opoNoEnum in fOptions) then
    begin
      fmt := aSchema^._Format;
      if (fmt = '') and // if no "format" type name is supplied
         (aSchema^._Type = 'string') then
      begin
        enum^.SortByValue;  // won't care about the actual order, just the values
        fmt := enum^.ToCsv('_'); // use string values to make it genuine
      end
      else
        nam := fmt; // we have an explicit type name
      if fmt <> '' then
      begin
        enumType := fEnums.GetObjectFrom(fmt);
        if enumType = nil then
        begin
          if nam = '' then
            nam := aSchemaName;
          if nam = '' then
          begin
            inc(fEnumCounter); // TEnum### seems easier
            Make(['Enum', fName, fEnumCounter], nam);
          end;
          enumType := TPascalEnum.Create(self, nam, aSchema);
          fEnums.AddObject(fmt, enumType);
        end;
        result := TPascalType.CreateCustom(enumType);
        exit;
      end;
    end;
    result := TPascalType.CreateBuiltin(self, aSchema.BuiltInType(self), aSchema);
  end;
end;

procedure TOpenApiParser.Comment(W: TTextWriter; const Args: array of const;
  const Desc: RawUtf8);
var
  all, line, feed: RawUtf8;
  p: PUtf8Char;
  i, j, o: PtrInt;
begin
  all := TrimU(Make(Args));
  if Desc <> '' then
    Append(all, ': ', Desc);
  p := pointer(all);
  repeat
    line := GetNextLine(p, p, {trim=}true);
    if line = '' then
      continue;
    o := 0;
    while length(line) - o > 80 do // insert line feeds on huge comment
    begin
      i := PosEx(' ', line, o + 75);   // try to end at a space position
      if (i = 0) or
         (i > o + 100) then
      begin
        j := PosEx(',', line, o + 75); // comma may appear sooner
        if j <> 0 then
          if (i = 0) or
             (j < i) then
            i := j + 1;
      end;
      if i = 0 then
        break;
      if feed = '' then
        Make([LineEnd, fLineIndent, '//'], feed);
      insert(feed, line, i);
      o := i + length(feed);
    end;
    w.AddStrings([fLineIndent, '// ', line, LineEnd]);
  until p = nil;
end;

procedure TOpenApiParser.Code(W: TTextWriter; var Line: RawUtf8;
  const Args: array of const);
begin
  if length(Line) > 70 then
  begin
    W.AddStrings([TrimRight(Line), LineEnd]);
    Line := fLineIndent + '  ';
  end;
  Append(Line, Args);
end;

procedure TOpenApiParser.Description(W: TTextWriter; const Described: RawUtf8);
var
  u: RawUtf8;
  v: variant;
begin
  if opoClientNoDescription in fOptions then
    exit;
  if fTitle = '' then
    fTitle := fName;
  Comment(w, [Described, ' ', fTitle]);
  if fInfo^.GetAsRawUtf8('description', u) then
    Comment(w, [' - ', u]);
  if fLineIndent <> '' then
    exit;
  if fInfo^.GetAsRawUtf8('version', u) then
    Comment(w, ['- version ', u]);
  if fInfo^.GetValueByPath('license.name', v) then
    Comment(w, ['- OpenAPI definition licensed under ', v, ' terms']);
end;

function TOpenApiParser.ParseRecordDefinition(const aDefinitionName: RawUtf8;
  aSchema: POpenApiSchema; aTemporary: boolean): TPascalRecord;
var
  i, j: PtrInt;
  def: POpenApiSchemaDynArray;
  ref, n: RawUtf8;
  v: PDocVariantData;
  p: TPascalProperty;
begin
  // setup the new TPascalRecord instance
  if aSchema = nil then
    aSchema := Schema[aDefinitionName];
  if aSchema = nil then
    EOpenApi.RaiseUtf8('%.ParseRecordDefinition: no % definition in schema',
      [self, aDefinitionName]);
  result := TPascalRecord.Create(self, aDefinitionName, aSchema);
  if not aTemporary then
    fRecords.AddObject(aDefinitionName, result); // allow recursive props
  // aggregate all needed information
  def := aSchema^.AllOf;
  if def = nil then
    if not (aSchema^.IsObject or
            aSchema^.HasProperties) then
      EOpenApi.RaiseUtf8('%.ParseRecordDefinition: % is %, not object',
        [self, aDefinitionName, aSchema^._Type])
    else
    begin
      SetLength(def, 1);
      def[0] := aSchema;
    end;
  // append all fields to result.Properties
  for i := 0 to high(def) do
  begin
    aSchema := def[i];
    // append $ref properties first - making copy of each TPascalProperty
    ref := aSchema^.Reference;
    if ref <> '' then
      GetRecord(ref, nil, {isref=}true).CopyProperties(result);
    // append specific fields
    v := aSchema^.Properties;
    if v <> nil then
      for j := 0 to v^.Count - 1 do
      begin
        n := v^.Names[j];
        p := TPascalProperty.CreateFromSchema(self, n, @v^.Values[j]);
        if p.PropType.CustomType = result then // most obvious nested definition
          // e.g. TGroup = record Another: TGroup; ...
          //   or TGroup = record Groups: array of TGroup; ...
          // TPascalRecord.ResolveDependencies will detect deeper recursion levels
          if (opoGenerateOldDelphiCompatible in fOptions) or
             not p.PropType.IsArray then
            p.ConvertToVariant; // fallback to TDocVariant/JSON
        include(result.fTypes, p.fType.fBuiltInType);
        result.fProperties.AddObject(n, p, {raise=}false, {free=}nil, {replace=}true);
      end;
  end;
  if result.fProperties.Count = 0 then
  begin
    // this is no fixed record, but maybe a "oneOf" or "anyOf" object
    result.fIsVoidVariant := true;
    result.fPascalName := 'variant';
  end
  else
    result.fNeedsDummyField := (opoGenerateOldDelphiCompatible in fOptions) and
                               (result.fTypes - [obtInteger .. obtGuid] = []);
end;

procedure TOpenApiParser.ParsePath(
  const aPath: RawUtf8; aPathItem: POpenApiPathItem);
var
  i: PtrInt;
  met: RawUtf8;
  s: POpenApiOperation;
  op: TPascalOperation;
begin
  // https://swagger.io/docs/specification/paths-and-operations
  for i := 0 to aPathItem^.Data.Count - 1 do
  begin
    met := aPathItem^.Data.Names[i];
    // OpenAPI does not support all our TUriMethod, but its own set
    if FindPropName(['get', 'post', 'put', 'patch', 'delete',
                     'head', 'options', 'trace'], met) < 0 then
      continue; // e.g. "parameters" may also appear here
    s := @aPathItem^.Data.Values[i];
    if (opoClientExcludeDeprecated in fOptions) and
       s^.Deprecated then
       continue;
    op := TPascalOperation.Create(self, met, aPath, aPathItem, s);
    op.ResolveResponseTypes;
    ObjArrayAdd(fOperations, op);
  end;
end;

function TOpenApiParser.GetRecord(aRecordName: RawUtf8; aSchema: POpenApiSchema;
  NameIsReference: boolean): TPascalRecord;
begin
  if NameIsReference then
    // #/definitions/NewPet -> NewPet
    aRecordName := PascalNameFromRef(aRecordName);
  result := fRecords.GetObjectFrom(aRecordName);
  if result = nil then
    result := ParseRecordDefinition(aRecordName, aSchema);
end;

function TOpenApiParser.GetOrderedRecords: TPascalRecordDynArray;
var
  i: PtrInt;
  pending: TPascalRecordDynArray;
begin
  result := fOrderedRecords; // first check if not already cached
  if result <> nil then
    exit;
  for i := 0 to fRecords.Count - 1 do
    TPascalRecord(fRecords.ObjectPtr[i]).ResolveDependencies(result, pending);
  if pending <> nil then // paranoid
    EOpenApi.RaiseUtf8('%.GetOrderedRecords: pending=%', [self, length(pending)]);
  fOrderedRecords := result; // compute once
end;

function TOpenApiParser.GetOperationsByTag: TPascalOperationsByTagDynArray;
var
  main: PDocVariantData;
  tag: TRawUtf8DynArray;
  i, j, k, count, ndx: PtrInt;
begin
  result := nil;
  if fOperations = nil then
    exit;
  // regroup operations per tags, eventually with result[0].Tag=nil
  count := 1;
  SetLength(result, length(fOperations) + 1);
  main := fSpecs.Tags;
  for i := 0 to length(fOperations) - 1 do
  begin
    tag := fOperations[i].Operation^.Tags;
    if tag <> nil then
    begin
      // add to all tags by name in result[1..]
      for j := 0 to high(tag) do
      begin
        ndx := -1;
        for k := 1 to count - 1 do
          if result[k].TagName = tag[j] then
          begin
            ndx := k;
            break;
          end;
        if ndx < 0 then
        begin
          ndx := count;
          inc(count);
          result[ndx].TagName := tag[j];
          main.GetDocVariantByProp('name', tag[j], {casesens:}true,
            PDocVariantData(result[ndx].Tag)); // maybe nil
        end;
        ObjArrayAdd(result[ndx].Operations, fOperations[i]);
      end;
    end
    else
      // if not tag involved, just append in result[0]
      ObjArrayAdd(result[0].Operations, fOperations[i]);
  end;
  SetLength(result, count);
  // caller will ensure a single fOperations[] method will be generated
end;

procedure TOpenApiParser.GenerateDtoInterface(w: TTextWriter);
var
  rec: TPascalRecordDynArray;
  i: PtrInt;
begin
  // append all enumeration types
  fLineIndent := '  ';
  w.AddStrings(['type', LineEnd, LineEnd]);
  if fEnums.Count > 0 then
  begin
    w.AddStrings(['{ ************ Enumerations and Sets }', LineEnd, LineEnd]);
    for i := 0 to fEnums.Count - 1 do
      TPascalEnum(fEnums.ObjectPtr[i]).ToTypeDefinition(w);
    w.AddStrings([LineEnd, LineEnd]);
  end;
  // append all records
  w.AddStrings(['{ ************ Data Transfert Objects }', LineEnd, LineEnd]);
  rec := GetOrderedRecords;
  for i := 0 to high(rec) do
    rec[i].ToTypeDefinition(w);
  // enumeration-to-text constants
  if fEnums.Count > 0 then
  begin
    w.AddStrings([LineEnd, LineEnd, 'const', LineEnd,
      '  // define how enums/sets are actually transmitted as JSON array of string', LineEnd]);
    for i := 0 to fEnums.Count - 1 do
      TPascalEnum(fEnums.ObjectPtr[i]).ToConstTextArray(w);
  end;
end;

procedure TOpenApiParser.GenerateDtoImplementation(w: TTextWriter);
var
  rec: TPascalRecordDynArray;
  i: PtrInt;
begin
  w.AddStrings([LineEnd,
    '{ ************ Custom RTTI/JSON initialization }', LineEnd, LineEnd]);
  fLineIndent := '  ';
  // output the text representation of all records
  // with proper json names (overriding the RTTI definitions)
  rec := GetOrderedRecords;
  if rec <> nil then
  begin
    w.AddStrings(['const', LineEnd,
      '  // exact definition of the DTOs expected JSON serialization', LineEnd]);
    for i := 0 to high(rec) do
      if rec[i].Properties.Count <> 0 then
        w.AddStrings([fLineIndent, rec[i].ToRttiTextRepresentation, LineEnd]);
  end;
  // define the private RTTI registration procedure
  w.AddStrings([LineEnd, LineEnd,
    'procedure RegisterRtti;', LineEnd,
    'begin', LineEnd]);
  // register all needed enum types RTTI
  if fEnums.Count > 0 then
  begin
    w.AddStrings(['  TRttiJson.RegisterCustomEnumValues([', LineEnd]);
    for i := 0 to fEnums.Count - 1 do
    begin
      if i > 0 then
        w.AddStrings([',', LineEnd]);
      TPascalEnum(fEnums.ObjectPtr[i]).ToRegisterCustom(w);
    end;
    w.AddStrings([']);', LineEnd]);
  end;
  // register all record types RTTI
  if rec <> nil then
  begin
    w.AddStrings(['  Rtti.RegisterFromText([', LineEnd]);
    for i := 0 to high(rec) do
      if rec[i].Properties.Count <> 0 then
      begin
        if i > 0 then
          w.AddStrings([',', LineEnd]);
        w.AddStrings(['    ', rec[i].ToRttiRegisterDefinitions]);
      end;
    w.AddStrings([']);', LineEnd,
      'end;', LineEnd, LineEnd]);
  end;
  // initialization
  w.AddStrings([
    'initialization', LineEnd,
    '  RegisterRtti;', LineEnd, LineEnd,
    'end.', LineEnd]);
end;

function TOpenApiParser.GenerateDtoUnit: RawUtf8;
var
  temp: TTextWriterStackBuffer;
  w: TTextWriter;
begin
  if fDtoUnitName = '' then
    Make([LowerCaseU(fName), '.dto'], fDtoUnitName);
  w := TTextWriter.CreateOwnedStream(temp);
  try
    // header section
    fLineIndent := '';
    Description(w, 'DTOs for');
    w.AddStrings([
      'unit ', fDtoUnitName, ';', LineEnd , LineEnd,
      '{$I mormot.defines.inc}', LineEnd ,
      LineEnd,
      'interface', LineEnd,
      LineEnd,
      '{', LineEnd,
      '  ', fGeneratedByLine, LineEnd,
      '  ', UpperCaseU(fTitle), ' DTOs', LineEnd, LineEnd,
      '  ', fGeneratedBy, LineEnd,
      '  ', fGeneratedByLine, LineEnd,
      '}', LineEnd,
      LineEnd,
      'uses', LineEnd,
      '  classes,', LineEnd,
      '  sysutils,', LineEnd,
      '  mormot.core.base,', LineEnd,
      '  mormot.core.rtti,', LineEnd,
      '  mormot.core.json;', LineEnd,
      LineEnd]);
    // interface section
    GenerateDtoInterface(w);
    // implementation section
    w.AddStrings([LineEnd, LineEnd,
      'implementation', LineEnd]);
    GenerateDtoImplementation(w);
    w.SetText(result);
  finally
    w.Free;
  end;
end;

function TOpenApiParser.GenerateClientUnit: RawUtf8;
var
  bytag: TPascalOperationsByTagDynArray;
  done: TRawUtf8DynArray;
  ops: TPascalOperationsByTag;
  op: TPascalOperation;
  desc, dots, err: RawUtf8;
  i, j: PtrInt;
  banner: boolean;
  temp: TTextWriterStackBuffer;
  w: TTextWriter;
begin
  if opoGenerateSingleApiUnit in fOptions then
    desc := '.api'
  else
    desc := '.client';
  if fClientUnitName = '' then
    Make([LowerCase(fName), desc], fClientUnitName);
  if fClientClassName = '' then
    Make(['T', fName, 'Client'], fClientClassName);
  w := TTextWriter.CreateOwnedStream(temp);
  try
    // unit common definitions
    fLineIndent := '';
    Description(w, 'Client unit for');
    w.AddStrings([
      'unit ', fClientUnitName, ';', LineEnd,
      LineEnd,
      '{$I mormot.defines.inc}', LineEnd ,
      LineEnd,
      'interface', LineEnd,
      LineEnd,
      '{', LineEnd,
      '  ', fGeneratedByLine, LineEnd,
      '  ', UpperCaseU(fTitle), ' client as ', fClientClassName, ' class', LineEnd, LineEnd,
      '  ', fGeneratedBy, LineEnd,
      '  ', fGeneratedByLine, LineEnd,
      '}', LineEnd,
      LineEnd,
      'uses', LineEnd,
      '  classes,', LineEnd,
      '  sysutils,', LineEnd,
      '  mormot.core.base,', LineEnd,
      '  mormot.core.unicode,', LineEnd,
      '  mormot.core.text,', LineEnd,
      '  mormot.core.buffers,', LineEnd,
      '  mormot.core.datetime,', LineEnd,
      '  mormot.core.rtti,', LineEnd,
      '  mormot.core.json,', LineEnd,
      '  mormot.core.variants,', LineEnd,
      '  mormot.net.client']);
    // include DTO definitions within this single API unit or as external unit
    if opoGenerateSingleApiUnit in fOptions then
    begin
      w.AddStrings([';', LineEnd, LineEnd]);
      GenerateDtoInterface(w)
    end
    else
      w.AddStrings([',', LineEnd, '  ', fDtoUnitName, ';', LineEnd]);
    w.AddStrings([LineEnd, 'type', LineEnd]);
    // custom exceptions definitions
    fLineIndent := '  ';
    if fExceptions.Count > 0 then
    begin
      w.AddStrings([LineEnd, '{ ************ Custom Exceptions }', LineEnd, LineEnd]);
      for i := 0 to fExceptions.Count - 1 do
        TPascalException(fExceptions.ObjectPtr[i]).ToTypeDefinition(w);
    end;
    // main client class definition
    w.AddStrings([LineEnd,
      '{ ************ Main ', fClientClassName, ' Class }', LineEnd, LineEnd]);
    Description(w,'Client class for');
    w.AddStrings([
      '  ', fClientClassName, ' = class', LineEnd,
      '  private', LineEnd,
      '    fClient: IJsonClient;', LineEnd]);
    // status responses to exception events
    if fErrorHandler <> nil then
    begin
      w.AddStrings(['    // TOnJsonClientError event handler',
        PLURAL_FORM[length(fErrorHandler) > 1], LineEnd]);
      for i := 0 to high(fErrorHandler) do
        w.AddStrings([
          '    procedure OnError', SmallUInt32Utf8[i + 1],
          '(const Sender: IJsonClient;', LineEnd,
          '      const Response: TJsonResponse; const ErrorMsg: shortstring);', LineEnd]);
    end;
    w.AddStrings([
      '  public', LineEnd, LineEnd,
      '    // initialize this Client with an associated HTTP/JSON request', LineEnd,
      '    constructor Create(const aClient: IJsonClient);', LineEnd]);
    // append all methods, regrouped per tag (if any)
    fLineIndent := '    ';
    bytag := GetOperationsByTag;
    for i := 0 to high(bytag) do
    begin
      ops := bytag[i];
      if ops.Operations = nil then
        continue; // nothing to add (e.g. i=0)
      banner := false;
      for j := 0 to high(ops.Operations) do
      begin
        op := ops.Operations[j];
        if FindRawUtf8(done, op.OperationId) >= 0 then
          // operations can be in multiple tags but should be written once
          continue;
        AddRawUtf8(done, op.OperationId);
        if not banner then
        begin
          banner := true;
          desc := ops.TagName;
          if desc = '' then
            desc := fTitle;
          if opoClientNoDescription in fOptions then // minimal comment
             w.AddStrings([LineEnd, '    // ', desc, ' methods', LineEnd])
          else
          begin
            if not (opoClientOnlySummary in fOptions) then
              FormatUtf8('    // %%',
                [RawUtf8OfChar('-', length(desc) + 10), LineEnd], dots);
            w.AddStrings([LineEnd, LineEnd, dots,
              '    //  ', UpperCaseU(desc), ' METHODS', LineEnd, dots]);
            desc := ops.Tag^.Description;
            if desc <> '' then
            begin
              Comment(w, ['- ', desc]);
              w.AddString(LineEnd);
            end;
          end;
        end;
        if not (opoClientNoDescription in fOptions) then
          if opoClientOnlySummary in fOptions then
          begin
            desc := op.Operation^.Summary;
            if desc <> '' then
              Comment(w, [desc]);
          end
          else
            op.Documentation(w);
        w.AddString(fLineIndent);
        op.Declaration(w, '', {implem=}false);
        w.AddString(LineEnd);
      end;
    end;
    // finalize the class definition and start the implementation section
    w.AddStrings([LineEnd,
      '    // access to the associated HTTP/JSON client instance', LineEnd,
      '    property JsonClient: IJsonClient', LineEnd,
      '      read fClient write fClient;', LineEnd,
      '  end;', LineEnd,
      LineEnd, LineEnd,
      'implementation', LineEnd, LineEnd, LineEnd]);
    // custom exceptions implementations
    if fExceptions.Count > 0 then
    begin
      w.AddStrings(['{ ************ Custom Exceptions }', LineEnd, LineEnd]);
      for i := 0 to fExceptions.Count - 1 do
        TPascalException(fExceptions.ObjectPtr[i]).Body(w);
    end;
    // main client class implementation
    fLineIndent := '';
    w.AddStrings([LineEnd,
      '{ ************ Main ', fClientClassName, ' Class }', LineEnd, LineEnd,
      '{ ', fClientClassName, '}', LineEnd, LineEnd,
      'constructor ', fClientClassName, '.Create(const aClient: IJsonClient);', LineEnd,
      'begin', LineEnd,
      '  fClient := aClient;', LineEnd,
      '  fClient.Options := [jcoParseTolerant, jcoHttpErrorRaise];', LineEnd,
      '  fClient.UrlEncoder :=', LineEnd,
      '    [ueEncodeNames, ueSkipVoidString, ueSkipVoidValue, ueStarNameIsCsv];', LineEnd,
      'end;', LineEnd, LineEnd]);
    // status responses to exception events
    for i := 0 to high(fErrorHandler) do
    begin
      w.AddStrings([
        'procedure ', fClientClassName, '.OnError', SmallUInt32Utf8[i + 1],
            '(const Sender: IJsonClient;', LineEnd,
        '  const Response: TJsonResponse; const ErrorMsg: shortstring);', LineEnd]);
      err := fErrorHandler[i];
      j := PosEx('  else', err);
      if j = 1 then
        w.AddStrings([ // single default exception
          'begin', LineEnd,
          '  raise', Split(SplitRight(err, '='), ';')]) // extract class name
      else
      begin
        w.AddStrings([
          'var', LineEnd,
          '  e: EJsonClientClass;', LineEnd,
          'begin', LineEnd,
          '  case Response.Status of', LineEnd,
          err]); // exception block
        if j = 0 then
          w.AddStrings([
            '  else', LineEnd,
            '    e := EJsonClient;', LineEnd]); // no default exception class
        w.AddStrings([
          '  end;', LineEnd,
          '  raise e']);
      end;
      w.AddStrings([
        '.CreateResp(''%.%'', [self, ErrorMsg], Response);', LineEnd,
        'end;', LineEnd, LineEnd]);
    end;
    // append all methods, in native order (no need to follow tag ordering)
    for i := 0 to high(Operations) do
      Operations[i].Body(w, fClientClassName, fSpecs.BasePath);
    // include DTOs registration for single API unit
    if opoGenerateSingleApiUnit in fOptions then
      GenerateDtoImplementation(w)
    else
      w.AddStrings([LineEnd, 'end.']);
    w.SetText(result);
  finally
    w.Free;
  end;
end;

procedure TOpenApiParser.ExportToDirectory(const DirectoryName: TFileName);
var
  dto, client: RawUtf8;
begin
  if not (opoGenerateSingleApiUnit in fOptions) then
  begin
    dto := GenerateDtoUnit;
    FileFromString(dto, MakePath([DirectoryName, fDtoUnitName + '.pas']));
  end;
  client := GenerateClientUnit;
  FileFromString(client, MakePath([DirectoryName, fClientUnitName + '.pas']));
end;


end.

