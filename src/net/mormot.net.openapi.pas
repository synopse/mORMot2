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
   - Use high-level pascal records and dynamic arrays for DTOs
   - Use high-level pascal enumerations and sets for "enum" values
   - Translate HTTP status error codes into high-level pascal Exceptions
   - Recognize similar "properties" or "enum" to reuse the same pascal type
   - Support of nested "$ref" for objects, parameters or types
   - Support "allOf" attribute, with proper properties inheritance/overloading
   - Support "oneOf" attribute, for strings or alternate record types
   - Fallback to variant pascal type for "oneOf" or "anyOf" JSON values
   - Generated source code units are very small and easy to use, read and debug
   - Can generate very detailed comment documentation in the unit source code
   - Tunable engine, with plenty of generation options (e.g. about verbosity)
   - Leverage the mORMot RTTI and JSON kernel for its internal plumbing
   - Compatible with FPC and oldest Delphi (7-2007)
   - Tested with several Swagger 2 and OpenAPI 3 reference content
  But still not fully compliant to all existing files: feedback is welcome!

  TODO:  - operation: "in": "header" in "parameters"
         - operation: "multipart/form-data" in "consumes"
         - authentification in global "securityDefinitions"
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

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Response
  POpenApiResponse = ^TOpenApiResponse;
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


  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI RequestBody
  // - share the very same fields as TOpenApiResponse
  POpenApiRequestBody = {$ifdef USERECORDWITHMETHODS} type {$endif}POpenApiResponse;

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Parameter
  POpenApiParameter = ^TOpenApiParameter;
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
    /// true if Default or not Required
    function HasDefaultValue: boolean;
    function Required: boolean;
    function Schema(Parser: TOpenApiParser): POpenApiSchema;
  end;
  /// a dynamic array of pointers wrapper to OpenAPI Parameter(s)
  POpenApiParameterDynArray = array of POpenApiParameter;

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Parameters
  POpenApiParameters = ^TOpenApiParameters;
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

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Operation
  POpenApiOperation = ^TOpenApiOperation;
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

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Path Item
  POpenApiPathItem = ^TOpenApiPathItem;
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

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Tag
  POpenApiTag = ^TOpenApiTag;
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

  /// pointer wrapper to TDocVariantData / variant content of OpenAPI Specs
  POpenApiSpecs = ^TOpenApiSpecs;
  /// high-level OpenAPI Specs wrapper to TDocVariantData / variant content
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
    // access to the OpenAPI Specs information
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


{ ************************************ FPC/Delphi Pascal Client Code Generation }

  TPascalCustomType = class;

  /// define any Pascal type, as basic type of custom type
  TPascalType = class
  private
    fBuiltinSchema: POpenApiSchema;
    fBuiltInTypeName: RawUtf8;
    fCustomType: TPascalCustomType;
    fBuiltInType: TOpenApiBuiltInType;
    fIsArray, fNoConst: boolean;
    function GetSchema: POpenApiSchema;
    procedure SetArray(AValue: boolean);
  public
    constructor CreateBuiltin(aBuiltInType: TOpenApiBuiltInType;
      aSchema: POpenApiSchema = nil; aIsArray: boolean = false); overload;
    constructor CreateCustom(aCustomType: TPascalCustomType); overload;

    // TODO: Handle RecordArrayType in RTTI definition
    function ToPascalName(AsFinalType: boolean = true;
      NoRecordArrayTypes: boolean = false): RawUtf8;
    function ToFormatUtf8Arg(const VarName: RawUtf8): RawUtf8;
    function ToDefaultParameterValue(aParam: POpenApiParameter;
      Parser: TOpenApiParser): RawUtf8;

    function IsBuiltin: boolean;
    function IsEnum: boolean;
    function IsRecord: boolean;
    property IsArray: boolean
      read fIsArray write SetArray;

    property CustomType: TPascalCustomType
      read fCustomType;
    property Schema: POpenApiSchema
      read GetSchema;
  end;
  TPascalTypeObjArray = array of TPascalType;

  /// define a Pascal property
  TPascalProperty = class
  private
    fType: TPascalType;
    fSchema: POpenApiSchema;
    fName: RawUtf8;
    fPascalName: RawUtf8;
    fTypeOwned: boolean;
  public
    constructor CreateFromSchema(aOwner: TOpenApiParser; const aName: RawUtf8;
      aSchema: POpenApiSchema);
    constructor CreateFrom(aAnother: TPascalProperty);
    constructor CreateBuiltin(const aName, aPascalName: RawUtf8;
      aBuiltInType: TOpenApiBuiltInType);
    destructor Destroy; override;
    property PropType: TPascalType
      read fType;
    property Schema: POpenApiSchema
      read fSchema;
    property Name: RawUtf8
      read fName;
    property PascalName: RawUtf8
      read fPascalName;
  end;

  /// abstract parent class holder for complex types
  TPascalCustomType = class
  private
    fName: RawUtf8;
    fPascalName: RawUtf8;
    fFromRef: RawUtf8;
    fSchema: POpenApiSchema;
    fParser: TOpenApiParser;
    fRequiresArrayDefinition: boolean;
  public
    constructor Create(aOwner: TOpenApiParser);
    procedure ToTypeDefinition(W: TTextWriter); virtual; abstract;
    function ToArrayTypeName(AsFinalType: boolean = true): RawUtf8; virtual;
    function ToArrayTypeDefinition: RawUtf8; virtual;
    property Name: RawUtf8
      read fName;
    property PascalName: RawUtf8
      read fPascalName;
    property Schema: POpenApiSchema
      read fSchema;
  end;

  /// define a Pascal data structure, as a packed record with RTTI
  TPascalRecord = class(TPascalCustomType)
  private
    fProperties: TRawUtf8List; // Objects are owned TPascalProperty
    fDependencies: TRawUtf8DynArray;
    fRttiTextRepresentation: RawUtf8;
    fTypes: set of TOpenApiBuiltInType;
    fNeedsDummyField: boolean;
  public
    constructor Create(aOwner: TOpenApiParser; const SchemaName: RawUtf8;
      Schema: POpenApiSchema = nil);
    destructor Destroy; override;
    procedure CopyProperties(aDest: TPascalRecord);
    procedure ToTypeDefinition(W: TTextWriter); override;
    function ToRttiTextRepresentation: RawUtf8;
    function ToRttiRegisterDefinitions: RawUtf8;
    property Properties: TRawUtf8List
      read fProperties;
  end;
  TPascalRecordDynArray = array of TPascalRecord;

  /// define a Pascal enumeration type
  TPascalEnum = class(TPascalCustomType)
  private
    fPrefix, fConstTextArrayName: RawUtf8;
    fChoices: TDocVariantData;
  public
    constructor Create(aOwner: TOpenApiParser; const aName: RawUtf8;
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
    constructor Create(aOwner: TOpenApiParser; const aCode: RawUtf8;
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

  /// define a Pascal method matching an OpenAPI operation
  TPascalOperation = class
  private
    fParser: TOpenApiParser;
    fOperationId: RawUtf8;
    fFunctionName: RawUtf8;
    fPath: RawUtf8;
    fPathItem: POpenApiPathItem;
    fOperation: POpenApiOperation;
    fRequestBody: POpenApiRequestBody;
    fRequestBodySchema: POpenApiSchema;
    fMethod: TUriMethod;
    fPayloadParameterType: TPascalType;
    fSuccessResponseType: TPascalType;
    fSuccessResponseCode: integer;
    fOnErrorIndex: integer;
    fParameters: POpenApiParameterDynArray;
    fParameterLocation: array of TOpenApiParamLocation;
    fParameterTypes: TPascalTypeObjArray;
    procedure ResolveParameter(var p: POpenApiParameter);
  public
    constructor Create(aParser: TOpenApiParser; const aPath: RawUtf8;
      aPathItem: POpenApiPathItem; aOperation: POpenApiOperation; aMethod: TUriMethod);
    destructor Destroy; override;
    procedure ResolveResponseTypes;
    procedure ResolveParameters;
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
    property Parameters: POpenApiParameterDynArray
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
  // - opoGenerateOldDelphiCompatible will generate a void/dummy managed field for
  // Delphi 7/2007/2009 compatibility and avoid 'T... has no type info' errors
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
    fTypeSuffix: RawUtf8;
    fTitle, fGeneratedBy, fGeneratedByLine: RawUtf8;
    fVersion: TOpenApiVersion;
    fOptions: TOpenApiParserOptions;
    fEnumCounter, fDtoCounter: integer;
    procedure ParseSpecs;
    function ParseRecordDefinition(const aDefinitionName: RawUtf8;
      aSchema: POpenApiSchema): TPascalRecord;
    procedure ParsePath(const aPath: RawUtf8);
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
  public
    /// initialize this parser instance
    // - the supplied aTypeSuffix will be used when generating TDto### and
    // TEnum### types, to avoid any conflict at RTTI level between the names,
    // especially if several OpenAPI generated units do coexist in the project
    constructor Create(const aTypeSuffix: RawUtf8 = '';
      aOptions: TOpenApiParserOptions = []);
    /// finalize this parser instance
    destructor Destroy; override;

    /// clear all internal information of this parser instance
    procedure Clear;
    /// parse a JSON Swagger/OpenAPI file content
    procedure ParseFile(const aJsonFile: TFileName);
    /// parse a JSON Swagger/OpenAPI content
    procedure ParseJson(const aJson: RawUtf8);
    /// parse a Swagger/OpenAPI content from an existing TDocVariant
    procedure Parse(const aSpecs: TDocVariantData);
    /// generate the dto and client unit .pas file in a given direction
    procedure ExportToDirectory(const Name: RawUtf8;
      const DirectoryName: TFileName = './'; const UnitPrefix: RawUtf8 = '');

    function GetDtosUnit(const UnitName: RawUtf8): RawUtf8;
    function GetClientUnit(const UnitName, ClientClassName, DtoUnitName: RawUtf8): RawUtf8;

    property Options: TOpenApiParserOptions
      read fOptions write fOptions;
    property Specs: TOpenApiSpecs
      read fSpecs;
    property Schema[const aName: RawUtf8]: POpenApiSchema
      read GetSchemaByName;
    property Operations: TPascalOperationDynArray
      read fOperations;
    property LineEnd: RawUtf8
      read fLineEnd;
    property LineIndent: RawUtf8
      read fLineIndent;
    property Version: TOpenApiVersion
      read fVersion;
  end;


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
    exit;
  end;
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
  end
end;


{ TOpenApiResponse }

function TOpenApiResponse.Description: RawUtf8;
begin
  result := TrimU(Data.U['description']);
end;

function TOpenApiResponse.Schema(Parser: TOpenApiParser): POpenApiSchema;
var
  ref: RawUtf8;
  c, o: PDocVariantData;
begin
  if @self = nil then
    result := nil
  else if Data.GetAsRawUtf8('$ref', ref) then
    result := POpenApiResponse(Parser.GetRef(ref)).Schema(Parser)
  else if Parser.Version = oav2 then
    result := POpenApiSchema(Data.O['schema'])
  else
  begin
    // we only support application/json in our wrapper classes
    result := nil;
    c := Data.O['content'];
    c.Options := c.Options - [dvoNameCaseSensitive];
    if c.GetAsObject(JSON_CONTENT_TYPE, o) or
       c.GetAsObject('*/*', o) or
       c.GetAsObject('application/jwt', o) then // seen in the wild :(
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

function TOpenApiParameter.HasDefaultValue: boolean;
begin
  result := (Default <> nil) or
            not Required;
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

constructor TPascalCustomType.Create(aOwner: TOpenApiParser);
begin
  // inheriting constructor should have set fName
  if fName = '' then
    EOpenApi.RaiseUtf8('%.Create(name?)', [self]);
  fParser := aOwner;
  if fName[1] = '#' then // ensure type name is not too long
  begin
    inc(fParser.fDtoCounter); // TDto### is simple and convenient
    Make(['TDto', fParser.fTypeSuffix, fParser.fDtoCounter], fPascalName);
  end
  else
    fPascalName := 'T' + SanitizePascalName(fName, {keywordcheck:}false);
end;

function TPascalCustomType.ToArrayTypeName(AsFinalType: boolean): RawUtf8;
begin
  if AsFinalType then
    result := FormatUtf8('%DynArray', [PascalName])
  else
    result := FormatUtf8('array of %', [PascalName]);
end;

function TPascalCustomType.ToArrayTypeDefinition: RawUtf8;
begin
  if fRequiresArrayDefinition then
    result := FormatUtf8('%% = %;%', [fParser.LineIndent,
      ToArrayTypeName({final=}true), ToArrayTypeName(false), fParser.LineEnd])
  else
    result := '';
end;


{ TPascalOperation }

constructor TPascalOperation.Create(aParser: TOpenApiParser;
  const aPath: RawUtf8; aPathItem: POpenApiPathItem;
  aOperation: POpenApiOperation; aMethod: TUriMethod);
var
  p, o: POpenApiParameters;
  pn, i: integer;
begin
  fParser := aParser;
  fPath := aPath;
  fPathItem := aPathItem;
  fOperation := aOperation;
  fMethod := aMethod;
  p := fPathItem^.Parameters;
  pn := p.Count;
  o := fOperation^.Parameters;
  SetLength(fParameters, pn + o.Count);
  for i := 0 to pn - 1 do
    fParameters[i] := p^.Parameter[i];
  for i := 0 to o.Count - 1 do
    fParameters[pn + i] := o^.Parameter[i];
  for i := 0 to high(fParameters) do
    ResolveParameter(fParameters[i]);
  fOperationId := fOperation^.Id;
  if fOperationId = '' then // fallback of the poor to have something <> ''
    fOperationId := TrimChar(fOperation^.Description, [#0 .. #31]);
  fFunctionName := SanitizePascalName(fOperationId, {keywordcheck:}true);
  if fOperation^.Deprecated then
    Append(fFunctionName, '_deprecated');
  fRequestBody := fOperation^.RequestBody(fParser);
  if fRequestBody <> nil then
    fRequestBodySchema := fRequestBody^.Schema(fParser);
  if fRequestBodySchema <> nil then
    fPayloadParameterType := fParser.NewPascalTypeFromSchema(fRequestBodySchema);
end;

destructor TPascalOperation.Destroy;
begin
  fPayloadParameterType.Free;
  fSuccessResponseType.Free;
  ObjArrayClear(fParameterTypes);
  inherited Destroy;
end;

procedure TPascalOperation.ResolveParameter(var p: POpenApiParameter);
begin
  if (p.Data.Count = 1) and
     (p.Data.Names[0] = '$ref') then
    p := fParser.GetRef(ToUtf8(p.Data.Values[0])); // resolve as reference
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

procedure TPascalOperation.ResolveParameters;
var
  n, i: PtrInt;
begin
  n := length(fParameters);
  SetLength(fParameterTypes, n);
  SetLength(fParameterLocation, n);
  for i := 0 to n - 1 do
    with fParameters[i]^ do
    begin
      fParameterTypes[i] := fParser.NewPascalTypeFromSchema(Schema(fParser));
      fParameterLocation[i] := Location;
    end;
end;

procedure TOpenApiParser.Comment(W: TTextWriter; const Args: array of const;
  const Desc: RawUtf8);
var
  all, line, feed: RawUtf8;
  p: PUtf8Char;
  i, o: PtrInt;
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
      i := PosEx(' ', line, o + 75);
      if i = 0 then
        break;
      if feed = '' then
        Make([LineEnd, LineIndent, '//'], feed);
      insert(feed, line, i);
      o := i + length(feed);
    end;
    w.AddStrings([LineIndent, '// ', line, LineEnd]);
  until p = nil;
end;

procedure TOpenApiParser.Code(W: TTextWriter; var Line: RawUtf8;
  const Args: array of const);
begin
  if length(Line) > 70 then
  begin
    W.AddStrings([TrimRight(Line), LineEnd]);
    Line := LineIndent + '  ';
  end;
  Append(Line, Args);
end;

procedure TPascalOperation.Documentation(W: TTextWriter);
var
  p: POpenApiParameter;
  v: PDocVariantData;
  status, desc, line: RawUtf8;
  code: integer;
  r: POpenApiResponse;
  i: PtrInt;
  rs: POpenApiSchema;
  e: TPascalException;
begin
  // Request Definition
  w.AddStrings([fParser.LineEnd, fParser.LineIndent, '// ']);
  if fOperation^.Deprecated then
     w.AddShort('[DEPRECATED] ');
   w.AddStrings([ // do not use fOperationID here because may = Description
     fOperation^.Id, ' [', ToText(fMethod), '] ', fPath, fParser.LineEnd,
     fParser.LineIndent, '//', fParser.LineEnd]);
  // Summary
  desc := fOperation^.Summary;
  if desc <> '' then
    fParser.Comment(w, ['Summary: ', desc]);
  // Description
  desc := fOperation^.Description;
  if desc <> '' then
  begin
    desc := StringReplaceAll(StringReplaceAll(desc, #10,
      FormatUtf8('%%//   ', [fParser.LineEnd, fParser.LineIndent])), #13, '');
    w.AddStrings([
      fParser.LineIndent, '// Description:', fParser.LineEnd,
      fParser.LineIndent, '//   ', desc, fParser.LineEnd]);
  end;
  // params
  if (fParameters <> nil) or
     (fRequestBodySchema <> nil) then
  begin
    w.AddStrings([fParser.LineIndent, '//', fParser.LineEnd,
           fParser.LineIndent, '// Params:', fParser.LineEnd]);
    for i := 0 to high(fParameters) do
    begin
      if fParameterLocation[i] = oplBody then
        continue; // fRequestBodySchema is handled below
      p := fParameters[i];
      Make(['- [', p^._In, '] ', p^.AsPascalName], line);
      if p^.Required then
        Append(line, '  (required)');
      if p^.Default <> nil then
        Append(line, [' (default=', p^.Default^, ')']);
      fParser.Comment(w, [line], p^.Description);
    end;
    // Request body
    if Assigned(fRequestBodySchema) then
    begin
      line := '- [body] Payload (required)';
      fParser.Comment(w, [line], fRequestBodySchema^.Description);
    end;
  end;
  // Responses
  v := fOperation^.Responses;
  if v^.Count > 0 then
  begin
    w.AddStrings([fParser.LineIndent, '//', fParser.LineEnd,
                  fParser.LineIndent, '// Responses:', fParser.LineEnd]);
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
  p: POpenApiParameter;
  pt: TPascalType;
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
    if fParameterLocation[i] in [oplPath, oplQuery, oplHeader] then
    begin
      pt := fParameterTypes[i];
      hasdefault := p^.HasDefaultValue and
                    (pt.IsArray or
                     not (pt.fBuiltInType in [obtVariant, obtRecord, obtGuid]));
      Make([_CONST[pt.fNoConst], p^.AsPascalName, ': ', pt.ToPascalName], decl);
      if hasdefault then
        if InImplementation then // same order, but no "= default" statement
          AddRawUtf8(def, decl)
        else
          AddRawUtf8(def, Make([decl, ' = ', pt.ToDefaultParameterValue(p, fParser)]))
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
  p: POpenApiParameter;

  procedure AppendParams(const params: TIntegerDynArray; opl: TOpenApiParamLocation);
  var
    i, j: PtrInt;
    p: POpenApiParameter;
    pt: TPascalType;
  begin
    if params <> nil then
    begin
      w.AddStrings([', [', fParser.LineEnd]);
      for i := 0 to high(params) do
      begin
        j := params[i];
        p := fParameters[j];
        pt := fParameterTypes[j];
        if i > 0 then
          w.AddStrings([',', fParser.LineEnd]);
        w.AddShorter('    ''');
        case opl of
          oplQuery:
            if pt.IsArray then
              w.AddDirect('*'); // ueStarNameIsCsv
          // oplHeader uses natively CSV in OpenAPI default "simple" style
        end;
        w.AddStrings([p.Name, ''', ', pt.ToFormatUtf8Arg(p.AsPascalName)]);
      end;
      w.AddDirect(']');
    end
    else
      w.AddShorter(', []');
  end;

begin
  // parse the URI and extract parameter names
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
    case fParameterLocation[i] of
      oplPath:
        begin
          p := fParameters[i];
          j := FindPropName(urlName, p^.Name);
          if j < 0 then
            EOpenApi.RaiseUtf8('%.Body: unknown % in [%]', [self, p^.Name, fPath]);
          urlParam[j] := i;
        end;
      oplQuery:
        AddInteger(queryParam, i);
      oplHeader:
        AddInteger(headerParam, i);
    end;
  for i := 0 to high(urlParam) do
    if urlParam[i] < 0 then
      EOpenApi.RaiseUtf8('%.Body: missing {%} in [%]', [self, urlName[i], fPath]);
  // emit the body block with its declaration and Request() call
  Declaration(w, ClassName, {implemtation=}true);
  w.AddStrings([fParser.LineEnd, 'begin', fParser.LineEnd,
         '  fClient.Request(''', ToText(fMethod), ''', ''', url, '''']);
  // Path parameters
  w.AddShorter(', [');
  for i := 0 to Length(urlName) - 1 do
  begin
    j := urlParam[i];
    if j < 0 then
      EOpenApi.RaiseUtf8('%.Body: unknown {%} in [%]', [self, urlName[i], fPath]);
    if i > 0 then
      w.AddShorter(', ');
    w.AddString(fParameterTypes[j].ToFormatUtf8Arg(fParameters[j].AsPascalName));
  end;
  w.AddDirect(']');
  // Query and Header parameters
  AppendParams(queryParam,  oplQuery);
  AppendParams(headerParam, oplHeader);
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

constructor TPascalProperty.CreateFromSchema(aOwner: TOpenApiParser;
  const aName: RawUtf8; aSchema: POpenApiSchema);
begin
  fType := aOwner.NewPascalTypeFromSchema(aSchema, '');
  fTypeOwned := true;
  fName := aName;
  fSchema := aSchema;
  fPascalName := SanitizePascalName(fName, {keywordcheck:}true);
end;

constructor TPascalProperty.CreateFrom(aAnother: TPascalProperty);
begin
  fType := aAnother.PropType; // weak copy (keep fTypeOwned=false)
  fName := aAnother.Name;
  fSchema := aAnother.Schema;
  fPascalName := aAnother.PascalName;
end;

constructor TPascalProperty.CreateBuiltin(const aName, aPascalName: RawUtf8;
  aBuiltInType: TOpenApiBuiltInType);
begin
  fType := TPascalType.CreateBuiltin(aBuiltInType);
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


{ TPascalEnum }

constructor TPascalEnum.Create(aOwner: TOpenApiParser;
  const aName: RawUtf8; aSchema: POpenApiSchema);
var
  i: PtrInt;
begin
  fName := aName;
  inherited Create(aOwner);
  fSchema := aSchema;
  fChoices.InitCopy(Variant(aSchema^.Enum^), JSON_FAST);
  fChoices.AddItem('None', 0); // always prepend a first void item
  if StartWithExact(aName, 'Enum') and
     (aName[5 + length(fParser.fTypeSuffix)]  in ['1' .. '9']) then
    // TEnumXxxxx2 = (ex2None, ex2...);
    Make(['e', LowerCase(copy(fParser.fTypeSuffix, 1, 1)),
          copy(aName, 5 + length(fParser.fTypeSuffix), 5)], fPrefix)
  else
    for i := 2 to length(fPascalName) do
      if length(fPrefix) >= 4 then
        break
      else if fPascalName[i] in ['A' .. 'Z'] then
        Append(fPrefix, fPascalName[i]);
  LowerCaseSelf(fPrefix); // TUserRole -> 'ur'
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
  w.AddStrings([fParser.LineIndent, PascalName, ' = (', fParser.LineEnd,
    fParser.LineIndent, '  ']);
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
  if fRequiresArrayDefinition then
    w.AddStrings([', TypeInfo(', ToArrayTypeName, ')'])
  else
    w.AddShorter(', nil');
  w.AddStrings([', @', fConstTextArrayName]);
end;

function TPascalEnum.ToArrayTypeName(AsFinalType: boolean): RawUtf8;
begin
  if AsFinalType then
    result := FormatUtf8('%Set', [PascalName])
  else
    result := FormatUtf8('set of %', [PascalName]);
end;

procedure TPascalEnum.ToConstTextArray(W: TTextWriter);
var
  i: integer;
  line, item: RawUtf8;
begin
  w.AddStrings([
    fParser.LineIndent, fConstTextArrayName,
      ': array[', PascalName, '] of RawUtf8 = (', fParser.LineEnd]);
  line := fParser.LineIndent + '  '''', ';  // first entry is for None/Default
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
  if AValue and
     Assigned(fCustomType) then
    fCustomType.fRequiresArrayDefinition := true;
end;

const
  OBT_TXT: array[TOpenApiBuiltInType] of RawUtf8 = (
    'variant', '', 'integer', 'Int64', 'boolean', '', 'single', 'double',
    'TDate', 'TDateTime', 'TGuid', 'RawUtf8', 'SpiUtf8', 'RawByteString');

constructor TPascalType.CreateBuiltin(aBuiltInType: TOpenApiBuiltInType;
  aSchema: POpenApiSchema; aIsArray: boolean);
begin
  fBuiltInType := aBuiltInType;
  fBuiltInTypeName := OBT_TXT[aBuiltInType];
  if fBuiltInTypeName = '' then
    EOpenApi.RaiseUtf8('Unexpected %.CreateBuiltin(%)', [self, ToText(aBuiltInType)^]);
  fBuiltinSchema := aSchema;
  if not aIsArray then
    fNoConst := aBuiltInType in [obtInteger .. obtDateTime];
  SetArray(aIsArray);
end;

constructor TPascalType.CreateCustom(aCustomType: TPascalCustomType);
begin
  fCustomType := aCustomType;
  fNoConst := IsEnum;
  if fNoConst then
    fBuiltInType := obtEnumerationOrSet
  else if IsRecord then
    fBuiltInType := obtRecord;
end;

function TOpenApiParser.NewPascalTypeFromSchema(aSchema: POpenApiSchema;
  aSchemaName: RawUtf8): TPascalType;
var
  all: POpenApiSchemaDynArray;
  ref, fmt, nam: RawUtf8;
  i: integer;
  rec, rectemp: TPascalRecord;
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
    result := NewPascalTypeFromSchema(aSchema, SplitRight(ref, '/'));
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
        result := TPascalType.CreateBuiltin(obtVariant, aSchema);
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
          rectemp := ParseRecordDefinition(aSchemaName, aSchema);
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
    result := NewPascalTypeFromSchema(aSchema^.Items, aSchemaName);
    result.fBuiltinSchema := aSchema;
    result.IsArray := true;
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
          begin
            inc(fEnumCounter); // TEnum### seems easier
            Make(['Enum', fTypeSuffix, fEnumCounter], nam);
          end;
          enumType := TPascalEnum.Create(self, nam, aSchema);
          fEnums.AddObject(fmt, enumType);
        end;
        result := TPascalType.CreateCustom(enumType);
        exit;
      end;
    end;
    result := TPascalType.CreateBuiltin(aSchema.BuiltInType(self), aSchema);
  end;
end;

function TPascalType.ToPascalName(AsFinalType, NoRecordArrayTypes: boolean): RawUtf8;
var
  finaltype: boolean;
begin
  if Assigned(CustomType) then
  begin
    result := CustomType.PascalName;
    if not IsArray then
      exit;
    finaltype := AsFinalType;
    if NoRecordArrayTypes and AsFinalType and IsRecord then
      finaltype := false;
    result := CustomType.ToArrayTypeName(finaltype);
  end
  else
  begin
    result := fBuiltInTypeName;
    if not IsArray then
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
        func := 'ToUtf8(%)';
      obtRawByteString:
        func := 'mormot.core.buffers.BinToBase64(%)';
    end
  else if IsEnum then
  begin
    func := (fCustomType as TPascalEnum).fConstTextArrayName;
    if IsArray then
      FormatUtf8('GetSetNameCustom(TypeInfo(%), %, @%)',
        [(fCustomType as TPascalEnum).PascalName, VarName, func], result)
    else
      FormatUtf8('%[%]', [func, VarName], result);
    exit;
  end;
  if func <> '' then
    FormatUtf8(func, [VarName], result);
end;

function TPascalType.ToDefaultParameterValue(aParam: POpenApiParameter;
  Parser: TOpenApiParser): RawUtf8;
var
  def: PVariant;
  t: RawUtf8;
begin
  def := aParam^.Default;
  if Assigned(def) and
     not VarIsEmptyOrNull(def^) then
  begin
    // explicit default value
    if PVarData(def)^.VType = varBoolean then
      result := BOOL_UTF8[PVarData(def)^.VBoolean] // normalize
    else if VariantToUtf8(def^, result) then
      result := QuotedStr(result);
  end
  else if IsEnum then
    result := (CustomType as TPascalEnum).Prefix + 'None'
  else if IsArray then
    result := 'nil'
  else
  begin
    // default from type
    t := aParam^.Schema(Parser)^._Type;
    if t = 'string' then
      result := ''''''
    else if (t = 'number') or
            (t = 'integer') then
      result := '0'
    else if t = 'boolean' then
      result := 'false'
    else
      EOpenApi.RaiseUtf8('Unsupported %.ToDefaultParameterValue(%)', [self, t]);
  end;
end;


{ TPascalRecord }

constructor TPascalRecord.Create(aOwner: TOpenApiParser;
  const SchemaName: RawUtf8; Schema: POpenApiSchema);
begin
  fName := SchemaName;
  fSchema := Schema;
  fProperties := TRawUtf8List.CreateEx([fObjectsOwned, fCaseSensitive, fNoDuplicate]);
  inherited Create(aOwner);
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
  if fProperties.Count = 0 then
  begin
    // this is no fixed record, but maybe a "oneOf" or "anyOf" object
    fPascalName := 'variant';
    exit;
  end;
  if (fFromRef <> '') and
     (fParser.Options * [opoDtoNoRefFrom, opoDtoNoDescription] = []) then
    fParser.Comment(w, ['from ', fFromRef]);
  // generate the record type definition
  w.AddStrings([fParser.LineIndent, PascalName, ' = packed record', fParser.LineEnd]);
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
          w.AddStrings([fParser.LineIndent,
            '  // - Example: ', s^.ExampleAsText, fParser.LineEnd]);
        if (not (opoDtoNoPattern in fParser.Options)) and
           s^.HasPattern then
          w.AddStrings([fParser.LineIndent,
            '  // - Pattern: ', s^.PatternAsText, fParser.LineEnd]);
      end;
    end;
    w.AddStrings([fParser.LineIndent, '  ', p.PascalName, ': ',
      p.PropType.ToPascalName, ';', fParser.LineEnd]);
  end;
  if fNeedsDummyField then
    w.AddStrings([
      fParser.LineIndent, '  // for Delphi 7-2007 compatibility', fParser.LineEnd,
      fParser.LineIndent, '  dummy_: RawUtf8;', fParser.LineEnd]);
  // associated pointer (and dynamic array if needed) definitions
  w.AddStrings([fParser.LineIndent, 'end;', fParser.LineEnd,
    fParser.LineIndent, 'P', copy(PascalName, 2, length(PascalName)),
      ' = ^', PascalName, ';', fParser.LineEnd,
    ToArrayTypeDefinition, fParser.LineEnd]);
end;

function TPascalRecord.ToRttiTextRepresentation: RawUtf8;
var
  i: PtrInt;
  p: TPascalProperty;
  line, name: RawUtf8;
begin
  if fProperties.Count = 0 then
    // this is no real record
    fPascalName := 'variant';
  result := fRttiTextRepresentation;
  if result <> '' then
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
  fRttiTextRepresentation := result;
end;

function TPascalRecord.ToRttiRegisterDefinitions: RawUtf8;
begin
  result := FormatUtf8('TypeInfo(%), _%', [PascalName, PascalName]);
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


{ TPascalException }

constructor TPascalException.Create(aOwner: TOpenApiParser; const aCode: RawUtf8;
  aResponse: POpenApiResponse);
begin
  fErrorCode := aCode;
  fResponse := aResponse;
  fErrorType := aOwner.NewPascalTypeFromSchema(aResponse^.Schema(aOwner));
  if Assigned(fErrorType.CustomType) then
    fName := fErrorType.CustomType.Name
  else
    EOpenApi.RaiseUtf8('%.Create: no schema for %', [self, aResponse^.Data.ToJson]);
  fErrorTypeName := fErrorType.ToPascalName;
  inherited Create(aOwner);
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
    fParser.LineIndent, PascalName, ' = class(EJsonClient)', fParser.LineEnd,
    fParser.LineIndent, 'protected', fParser.LineEnd,
    fParser.LineIndent, '  fError: ', fErrorTypeName, ';', fParser.LineEnd,
    fParser.LineIndent, 'public', fParser.LineEnd,
    fParser.LineIndent, '  constructor CreateResp(const Format: RawUtf8; const Args: array of const;', fParser.LineEnd,
    fParser.LineIndent, '    const Resp: TJsonResponse); override;', fParser.LineEnd,
    fParser.LineIndent, '  property Error: ', fErrorTypeName, fParser.LineEnd,
    fParser.LineIndent, '    read fError;', fParser.LineEnd,
    fParser.LineIndent, 'end;', fParser.LineEnd, fParser.LineEnd]);
end;

procedure TPascalException.Body(W: TTextWriter);
begin
  w.AddStrings(['{ ', PascalName, ' }', fParser.LineEnd, fParser.LineEnd,
    'constructor ', PascalName, '.CreateResp(const Format: RawUtf8;', fParser.LineEnd,
    '  const Args: array of const; const Resp: TJsonResponse);', fParser.LineEnd,
    'begin', fParser.LineEnd,
    '  inherited CreateResp(Format, Args, Resp);', fParser.LineEnd,
    '  LoadJson(fError, Resp.Content, TypeInfo(', fErrorTypeName,'));', fParser.LineEnd,
    'end;', fParser.LineEnd, fParser.LineEnd]);
end;


{ TOpenApiParser }

constructor TOpenApiParser.Create(const aTypeSuffix: RawUtf8;
  aOptions: TOpenApiParserOptions);
begin
  fTypeSuffix := aTypeSuffix;
  if fTypeSuffix <> '' then
    fTypeSuffix[1] := UpCase(fTypeSuffix[1]);
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
  fSchemas := nil;
  fErrorHandler := nil;
  ObjArrayClear(fOperations);
end;

procedure TOpenApiParser.Parse(const aSpecs: TDocVariantData);
begin
  Clear;
  fSpecs.Data.InitCopy(Variant(aSpecs), JSON_FAST);
  ParseSpecs;
end;

procedure TOpenApiParser.ParseJson(const aJson: RawUtf8);
begin
  Clear;
  fSpecs.Data.InitJson(aJson, JSON_FAST);
  ParseSpecs;
end;

procedure TOpenApiParser.ParseFile(const aJsonFile: TFileName);
begin
  Clear;
  fSpecs.Data.InitJsonFromFile(aJsonFile, JSON_FAST);
  ParseSpecs;
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
        fRecords.AddObject(n, ParseRecordDefinition(n, s));
  end;
  // parse all operations
  v := fSpecs.Paths;
  for i := 0 to v^.Count - 1 do
    ParsePath(v^.Names[i]);
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

procedure TOpenApiParser.Description(W: TTextWriter; const Described: RawUtf8);
var
  u: RawUtf8;
  v: variant;
begin
  if opoClientNoDescription in fOptions then
    exit;
  Comment(w, [Described, ' ', fTitle]);
  if fInfo^.GetAsRawUtf8('description', u) then
    Comment(w, [' - ', u]);
  if LineIndent <> '' then
    exit;
  if fInfo^.GetAsRawUtf8('version', u) then
    Comment(w, ['- version ', u]);
  if fInfo^.GetValueByPath('license.name', v) then
    Comment(w, ['- OpenAPI definition licensed under ', v, ' terms']);
end;

function TOpenApiParser.ParseRecordDefinition(const aDefinitionName: RawUtf8;
  aSchema: POpenApiSchema): TPascalRecord;
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
        include(result.fTypes, p.fType.fBuiltInType);
        result.fProperties.AddObject(n, p, {raise=}false, {free=}nil, {replace=}true);
      end;
  end;
  result.fNeedsDummyField := (opoGenerateOldDelphiCompatible in fOptions) and
                             (result.fTypes - [obtInteger .. obtGuid] = []);
end;

procedure TOpenApiParser.ParsePath(const aPath: RawUtf8);
var
  m: TUriMethod;
  p: POpenApiPathItem;
  s: POpenApiOperation;
  op: TPascalOperation;
begin
  p := fSpecs.Path[aPath];
  for m := low(m) to high(m) do
  begin
    s := p^.Method[m];
    if not Assigned(s) or
       ((opoClientExcludeDeprecated in fOptions) and
        s^.Deprecated) then
      continue;
    op := TPascalOperation.Create(self, aPath, p, s, m);
    op.ResolveResponseTypes;
    op.ResolveParameters;
    ObjArrayAdd(fOperations, op);
  end;
end;

function TOpenApiParser.GetRecord(aRecordName: RawUtf8; aSchema: POpenApiSchema;
  NameIsReference: boolean): TPascalRecord;
begin
  if NameIsReference then
    // #/definitions/NewPet -> NewPet
    aRecordName := SplitRight(aRecordName, '/');
  result := fRecords.GetObjectFrom(aRecordName);
  if result <> nil then
    exit;
  result := ParseRecordDefinition(aRecordName, aSchema);
  fRecords.AddObject(aRecordName, result);
end;

function HasDependencies(const Sources: TPascalRecordDynArray;
  const Searched: TRawUtf8DynArray): boolean;
var
  found: boolean;
  name: RawUtf8;
  i, j: PtrInt;
begin
  result := false;
  for i := 0 to high(Searched) do
  begin
    name := Searched[i];
    found := false;
    for j := 0 to high(Sources) do
      if Sources[j].Name = name then
      begin
        found := true;
        break;
      end;
    if not found then
      exit;
  end;
  result := true;
end;

function TOpenApiParser.GetOrderedRecords: TPascalRecordDynArray;
var
  pending, missing: TPascalRecordDynArray;
  r: TPascalRecord;
  i: PtrInt;
begin
  result := nil;
  // direct resolution
  for i := 0 to fRecords.Count - 1 do
  begin
    r := fRecords.ObjectPtr[i];
    if not Assigned(r.fDependencies) or
           HasDependencies(result, r.fDependencies) then
      ObjArrayAdd(result, r)
    else
      ObjArrayAdd(pending, r);
  end;
  // nested resolution
  while pending <> nil do
  begin
    missing := nil;
    for i := 0 to high(pending) do
      if HasDependencies(result, pending[i].fDependencies) then
        ObjArrayAdd(result, pending[i])
      else
        ObjArrayAdd(missing, pending[i]);
    pending := missing;
  end;
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

function TOpenApiParser.GetDtosUnit(const UnitName: RawUtf8): RawUtf8;
var
  rec: TPascalRecordDynArray;
  i: PtrInt;
  temp: TTextWriterStackBuffer;
  w: TTextWriter;
begin
  w := TTextWriter.CreateOwnedStream(temp);
  try
    // unit common definitions
    fLineIndent := '';
    Description(w, 'DTOs for');
    w.AddStrings([
      'unit ', UnitName, ';', LineEnd , LineEnd,
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
      LineEnd,
      'type', LineEnd, LineEnd]);
    // append all enumeration types
    fLineIndent := '  ';
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
    // start implementation section
    w.AddStrings([LineEnd, LineEnd,
      'implementation', LineEnd, LineEnd,
      '{ ************ Custom RTTI/JSON initialization }', LineEnd, LineEnd]);
    // output the text representation of all records
    // with proper json names (overriding the RTTI definitions)
    if rec <> nil then
    begin
      w.AddStrings(['const', LineEnd,
        '  // exact definition of the DTOs expected JSON serialization', LineEnd]);
      for i := 0 to high(rec) do
        if rec[i].Properties.Count <> 0 then
          w.AddStrings([LineIndent, rec[i].ToRttiTextRepresentation, LineEnd]);
    end;
    // define the RTTI registratoin procedure
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
    // finish the unit
    w.AddStrings([
      'initialization', LineEnd,
      '  RegisterRtti;', LineEnd,
      LineEnd,
      'end.', LineEnd]);
    w.SetText(result);
  finally
    w.Free;
  end;
end;

function TOpenApiParser.GetClientUnit(
  const UnitName, ClientClassName, DtoUnitName: RawUtf8): RawUtf8;
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
  w := TTextWriter.CreateOwnedStream(temp);
  try
    // unit common definitions
    fLineIndent := '';
    Description(w, 'Client unit for');
    w.AddStrings([
      'unit ', UnitName, ';', LineEnd,
      LineEnd,
      '{$I mormot.defines.inc}', LineEnd ,
      LineEnd,
      'interface', LineEnd,
      LineEnd,
      '{', LineEnd,
      '  ', fGeneratedByLine, LineEnd,
      '  ', UpperCaseU(fTitle), ' client as ', ClientClassName, ' class', LineEnd, LineEnd,
      '  ', fGeneratedBy, LineEnd,
      '  ', fGeneratedByLine, LineEnd,
      '}', LineEnd,
      LineEnd,
      'uses', LineEnd,
      '  classes,', LineEnd,
      '  sysutils,', LineEnd,
      '  mormot.core.base,', LineEnd,
      '  mormot.core.text,', LineEnd,
      '  mormot.core.buffers,', LineEnd,
      '  mormot.core.datetime,', LineEnd,
      '  mormot.core.rtti,', LineEnd,
      '  mormot.core.json,', LineEnd,
      '  mormot.core.variants,', LineEnd,
      '  mormot.net.client,', LineEnd,
      '  ', DtoUnitName, ';', LineEnd, LineEnd,
      'type', LineEnd]);
    fLineIndent := '  ';
    // custom exceptions definitions
    if fExceptions.Count > 0 then
    begin
      w.AddStrings([LineEnd, '{ ************ Custom Exceptions }', LineEnd, LineEnd]);
      for i := 0 to fExceptions.Count - 1 do
        TPascalException(fExceptions.ObjectPtr[i]).ToTypeDefinition(w);
    end;
    // main client class definition
    w.AddStrings([LineEnd,
      '{ ************ Main ', ClientClassName, ' Class }', LineEnd, LineEnd]);
    Description(w,'Client class for');
    w.AddStrings([
      '  ', ClientClassName, ' = class', LineEnd,
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
      '    constructor Create(const aClient: IJsonClient = nil);', LineEnd]);
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
        w.AddString(LineIndent);
        op.Declaration(w, '', {implem=}false);
        w.AddString(LineEnd);
      end;
    end;
    // finalize the class definition and start the implementation section
    w.AddStrings([LineEnd,
      '    // access to the associated HTTP/JSON request', LineEnd,
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
      '{ ************ Main ', ClientClassName, ' Class }', LineEnd, LineEnd,
      '{ ', ClientClassName, '}', LineEnd, LineEnd,
      'constructor ', ClientClassName, '.Create(const aClient: IJsonClient);', LineEnd,
      'begin', LineEnd,
      '  fClient := aClient;', LineEnd,
      '  fClient.UrlEncoder :=', LineEnd,
      '    [ueEncodeNames, ueSkipVoidString, ueSkipVoidValue, ueStarNameIsCsv];', LineEnd,
      'end;', LineEnd, LineEnd]);
    // status responses to exception events
    for i := 0 to high(fErrorHandler) do
    begin
      w.AddStrings([
        'procedure ', ClientClassName, '.OnError', SmallUInt32Utf8[i + 1],
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
      Operations[i].Body(w, ClientClassName, fSpecs.BasePath);
    w.AddStrings([LineEnd, 'end.']);
    w.SetText(result);
  finally
    w.Free;
  end;
end;

procedure TOpenApiParser.ExportToDirectory(const Name: RawUtf8;
  const DirectoryName: TFileName; const UnitPrefix: RawUtf8);
var
  dtounit, clientunit: RawUtf8;
  dtofn, clientfn: TFileName;
begin
  dtounit := FormatUtf8('%%.dto', [UnitPrefix, Name]);
  dtofn := MakePath([DirectoryName, dtounit + '.pas']);
  clientunit := FormatUtf8('%%.client', [UnitPrefix, Name]);
  clientfn := MakePath([DirectoryName, clientunit + '.pas']);
  FileFromString(GetDtosUnit(dtounit), dtofn);
  FileFromString(GetClientUnit(clientunit, FormatUtf8('T%Client', [Name]), dtounit), clientfn);
end;


end.

