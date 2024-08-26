/// OpenAPI Client Code Generation
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.openapi;

{
  *****************************************************************************

  OpenAPI Language-agnostic Interface to HTTP APIs
  - OpenAPI Document Wrappers
  - FPC/Delphi Pascal Code Generation

  *****************************************************************************
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
  mormot.core.variants,
  mormot.core.collections,
  mormot.rest.core;


{ ************************************ OpenAPI Document Wrappers }

const
  SCHEMA_TYPE_ARRAY: RawUtf8 = 'array';
  SCHEMA_TYPE_OBJECT: RawUtf8 = 'object';
  LINE_END: RawUtf8 = #10;

type
  /// exception class raised by this Unit
  EOpenApi = class(ESynException);

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Schema
  POpenApiSchema = ^TOpenApiSchema;

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
    function Nullable: Boolean;
    function Description: RawUtf8;
    function Example: variant;
    function Reference: RawUtf8;
    function AllOf: PDocVariantData;
    function Items: POpenApiSchema;
    function Properties: PDocVariantData;
    property _Property[const aName: RawUtf8]: POpenApiSchema
      read GetPropertyByName;
    // high-level OpenAPI Schema Helpers
    function IsArray: Boolean;
    function IsObject: Boolean;
    function IsNamedEnum: Boolean;
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
    function Schema: POpenApiSchema;
  end;

  /// pointer wrapper to TDocVariantData / variant content of an OpenAPI Parameter
  POpenApiParameter = ^TOpenApiParameter;
  /// high-level OpenAPI Parameter wrapper to TDocVariantData / variant content
  {$ifdef USERECORDWITHMETHODS}
  TOpenApiParameter = record
  {$else}
  TOpenApiParameter = object
  {$endif USERECORDWITHMETHODS}
  public
    /// transtype the POpenApiParametger pointer into a TDocVariantData content
    Data: TDocVariantData;
    // access to the OpenAPI Parameter information
    function AsSchema: POpenApiSchema;
    function Name: RawUtf8;
    function AsFpcName: RawUtf8;
    function _In: RawUtf8;
    function AllowEmptyValues: Boolean;
    function Default: PVariant;
    function Required: Boolean;
    function Schema: POpenApiSchema;
  end;
  /// a dynamic array of pointers wrapper to OpenAPI Parameter(s)
  POpenApiParameterDynArray = array of POpenApiParameter;

  TOpenApiOperation = record
  private
    function GetDeprecated: Boolean;
    function GetDescription: RawUtf8;
    function GetDV: PDocVariantData;
    function GetId: RawUtf8;
    function GetParameterByIndex(aIndex: Integer): POpenApiParameter;
    function GetParameters: PDocVariantData;
    function GetPayloadParameter: POpenApiParameter;
    function GetResponseForStatusCode(aStatusCode: Integer): POpenApiResponse;
    function GetResponses: PDocVariantData;
    function GetSummary: RawUtf8;
    function GetTags: PDocVariantData;
  public
    property DocVariant: PDocVariantData read GetDV;

    property Id: RawUtf8 read GetId;
    property Summary: RawUtf8 read GetSummary;
    property Description: RawUtf8 read GetDescription;
    property Tags: PDocVariantData read GetTags;
    property Deprecated: Boolean read GetDeprecated;

    property Parameters: PDocVariantData read GetParameters;
    property Parameter[aIndex: Integer]: POpenApiParameter read GetParameterByIndex;
    property PayloadParameter: POpenApiParameter read GetPayloadParameter;

    property Responses: PDocVariantData read GetResponses;
    property Response[aStatusCode: Integer]: POpenApiResponse read GetResponseForStatusCode;
  end;
  POpenApiOperation = ^TOpenApiOperation;

  TOpenApiPathItem = record
  private
    function GetDescription: RawUtf8;
    function GetDV: PDocVariantData;
    function GetOperationByMethod(aMethod: TUriMethod): POpenApiOperation;
    function GetParameterByIndex(aIndex: Integer): POpenApiParameter;
    function GetParameters: PDocVariantData;
    function GetSummary: RawUtf8;
  public
    property DocVariant: PDocVariantData read GetDV;

    property Summary: RawUtf8 read GetSummary;
    property Description: RawUtf8 read GetDescription;

    property Method[aMethod: TUriMethod]: POpenApiOperation read GetOperationByMethod;
    property Parameters: PDocVariantData read GetParameters;
    property Parameter[aIndex: Integer]: POpenApiParameter read GetParameterByIndex;
  end;
  POpenApiPathItem = ^TOpenApiPathItem;

  TOpenApiTag = record
  private
    function GetDescription: RawUtf8;
    function GetDV: PDocVariantData;
    function GetName: RawUtf8;
  public
    property DocVariant: PDocVariantData read GetDV;

    property Name: RawUtf8 read GetName;
    property Description: RawUtf8 read GetDescription;
  end;
  POpenApiTag = ^TOpenApiTag;

  TOpenApiSpecs = record
  private
    function GetBasePath: RawUtf8;
    function GetDefinitionByName(aName: RawUtf8): POpenApiSchema;
    function GetDefinitions: PDocVariantData;
    function GetDV: PDocVariantData;
    function GetPathItemByName(aPath: RawUtf8): POpenApiPathItem;
    function GetPaths: PDocVariantData;
    function GetTags: PDocVariantData;
    function GetVersion: RawUtf8;
  public
    property DocVariant: PDocVariantData read GetDV;

    property Definitions: PDocVariantData read GetDefinitions;
    property Definition[aName: RawUtf8]: POpenApiSchema read GetDefinitionByName;
    property Paths: PDocVariantData read GetPaths;
    property Path[aPath: RawUtf8]: POpenApiPathItem read GetPathItemByName;
    property Version: RawUtf8 read GetVersion;
    property BasePath: RawUtf8 read GetBasePath;
    property Tags: PDocVariantData read GetTags;
  end;
  POpenApiSpecs = ^TOpenApiSpecs;


{ ************************************ FPC/Delphi Pascal Code Generation }

type
  TOpenApiParser = class;

  TFpcCustomType = class
  private
    fRequiresArrayDefinition: Boolean;
    fFpcName: RawUtf8;
    function GetSchema: POpenApiSchema; virtual;
  public
    constructor Create(aFpcName: RawUtf8 = '');

    function ToTypeDefinition(LineIndentation: RawUtf8 = ''): RawUtf8; virtual; abstract;
    function ToArrayTypeName(AllowArrayType: Boolean = True): RawUtf8; virtual;
    function ToArrayTypeDefinition(LineIndentation: RawUtf8 = ''): RawUtf8; virtual;

    property FpcName: RawUtf8 read fFpcName;
    property Schema: POpenApiSchema read GetSchema;
  end;

  TFpcType = class
  private
    fBuiltinSchema: POpenApiSchema;
    fBuiltinTypeName: RawUtf8;
    fCustomType: TFpcCustomType;
    fIsArray: Boolean;
    fIsParent: Boolean;
    function GetSchema: POpenApiSchema;
    procedure SetArray(AValue: Boolean);
  public
    constructor CreateBuiltin(aBuiltinTypeName: RawUtf8; aSchema: POpenApiSchema = nil;
      aIsArray: Boolean = False); overload;
    constructor CreateCustom(aCustomType: TFpcCustomType; aIsArray: Boolean = False; aIsParent: Boolean = False); overload;

    class function LoadFromSchema(Schema: POpenApiSchema; Parser: TOpenApiParser): TFpcType;
    class function GetBuiltinType(Schema: POpenApiSchema): RawUtf8;

    // TODO: Handle RecordArrayType in RTTI definition
    function ToFpcName(AllowArrayType: Boolean = True; NoRecordArrayTypes: Boolean = False): RawUtf8;
    function ToFormatUtf8Arg(FpcVarName: RawUtf8): RawUtf8;



    property IsArray: Boolean read fIsArray write SetArray;
    property IsParent: Boolean read fIsParent;
    function IsBuiltin: Boolean;
    function IsEnum: Boolean;
    function IsRecord: Boolean;

    property CustomType: TFpcCustomType read fCustomType;
    property Schema: POpenApiSchema read GetSchema;
  end;

  TFpcProperty = class
  private
    fType: TFpcType;
    fSchema: POpenApiSchema;
    fName: RawUtf8;
    fFpcName: RawUtf8;
  public
    constructor Create(aName: RawUtf8; aSchema: POpenApiSchema; aType: TFpcType);
    constructor CreateFromSchema(aName: RawUtf8; aSchema: POpenApiSchema; Parser: TOpenApiParser; ParentField: Boolean = False);

    class function SanitizePropertyName(aName: RawUtf8): RawUtf8;
  end;

  // Find a better name for this class
  TFpcRecord = class(TFpcCustomType)
  private
    fName: RawUtf8;
    fSchema: POpenApiSchema;
    fProperties: IKeyValue<RawUtf8, TFpcProperty>;
    fDependencies: TRawUtf8DynArray;

    function GetSchema: POpenApiSchema; override;
  public
    constructor Create(SchemaName: RawUtf8; Schema: POpenApiSchema = nil);


    function ToTypeDefinition(LineIndentation: RawUtf8 = ''): RawUtf8; override;
    function ToRttiTextRepresentation(WithClassName: Boolean = True): RawUtf8;
    function ToRttiRegisterDefinitions: RawUtf8;
  end;
  TFpcRecordDynArray = array of TFpcRecord;

  TFpcEnum = class(TFpcCustomType)
  private
    fSchema: POpenApiSchema;
    fName: RawUtf8;
    fPrefix: RawUtf8;
    fChoices: TDocVariantData;

    function GetSchema: POpenApiSchema; override;
  public
    constructor Create(SchemaName: RawUtf8; aSchema: POpenApiSchema);

    function ToTypeDefinition(LineIndentation: RawUtf8 = ''): RawUtf8; override;
    function ToArrayTypeName(AllowArrayType: Boolean = True): RawUtf8; override;
    function ToConstTextArrayName: RawUtf8;
    function ToConstTextArray: RawUtf8;
  end;

  TFpcOperation = class
  private
    fPath: RawUtf8;
    fPathItem: POpenApiPathItem;
    fOperation: POpenApiOperation;
    fMethod: TUriMethod;
    fPayloadParameterType: TFpcType;
    fSuccessResponseCode: Integer;
    fSuccessResponseType: TFpcType;
  public
    constructor Create(aPath: RawUtf8; aPathItem: POpenApiPathItem; aOperation: POpenApiOperation; aMethod: TUriMethod);

    // Resolve parameters/responses types
    procedure ResolveTypes(Parser: TOpenApiParser);

    function GetAllParameters: POpenApiParameterDynArray;
    function Documentation(LineIndent: RawUtf8 = ''): RawUtf8;
    function Declaration(ClassName: RawUtf8; Parser: TOpenApiParser): RawUtf8;
    function Body(ClassName: RawUtf8; BasePath: RawUtf8; Parser: TOpenApiParser): RawUtf8;
    function FunctionName: RawUtf8;
  end;
  TFpcOperationDynArray = array of TFpcOperation;

  TOpenApiParser = class
  private
    fSpecs: TDocVariantData;

    fRecords: IKeyValue<RawUtf8, TFpcRecord>;
    fEnums: IKeyValue<RawUtf8, TFpcEnum>;
    fOperations: TFpcOperationDynArray;

    function GetSpecs: POpenApiSpecs;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Parse(aSpecs: TDocVariantData);
    function ParseDefinition(aDefinitionName: RawUtf8): TFpcRecord;
    procedure ParsePath(aPath: RawUtf8);

    function GetRecord(aRecordName: RawUtf8; NameIsReference: Boolean = False): TFpcRecord;
    function GetOrderedRecords: TFpcRecordDynArray;
    function GetOperationsByTag: IKeyValue<POpenApiTag, IList<TFpcOperation>>;

    procedure Dump;
    function GetDtosUnit(UnitName: RawUtf8): RawUtf8;
    function GetClientUnit(UnitName, ClientClassName, DtoUnitName: RawUtf8): RawUtf8;
    procedure ExportToDirectory(Name: RawUtf8; DirectoryName: RawUtf8 = './'; UnitPrefix: RawUtf8 = '');

    property Specs: POpenApiSpecs read GetSpecs;
  end;


implementation


{ ************************************ OpenAPI Document Wrappers }

{ TOpenApiSchema }

function TOpenApiSchema.AllOf: PDocVariantData;
begin
  if not Data.GetAsArray('allOf', result) then
    result := nil;
end;

function TOpenApiSchema.Items: POpenApiSchema;
begin
  if not IsArray then
    EOpenApi.RaiseUtf8('Trying to access items field on a non array schema: %', [_Type]);
  if not Data.GetAsObject('items', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSchema.Properties: PDocVariantData;
begin
  if not IsObject then
    EOpenApi.RaiseUtf8('Trying to access properties field on a non object schema: %', [_Type]);
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

function TOpenApiSchema.Nullable: Boolean;
begin
  result := Data.B['nullable'];
end;

function TOpenApiSchema.Required: PDocVariantData;
begin
  result := Data.A['required']
end;

function TOpenApiSchema.Description: RawUtf8;
begin
  result := Data.U['description']
end;

function TOpenApiSchema.Enum: PDocVariantData;
begin
  if not Data.GetAsArray('enum', result) then
    result := nil;
end;

function TOpenApiSchema.Example: Variant;
begin
  result := Data.Value['example'];
end;

function TOpenApiSchema._Format: RawUtf8;
begin
  result := Data.U['format'];
end;

function TOpenApiSchema._Type: RawUtf8;
begin
  result := Data.U['type'];
end;

function TOpenApiSchema.IsArray: Boolean;
begin
  result := _Type = SCHEMA_TYPE_ARRAY;
end;

function TOpenApiSchema.IsObject: Boolean;
begin
  result := _Type = SCHEMA_TYPE_OBJECT;
end;

function TOpenApiSchema.IsNamedEnum: Boolean;
begin
  Result := Assigned(Enum) and
            (_Format <> '');
end;


{ TOpenApiResponse }

function TOpenApiResponse.Description: RawUtf8;
begin
  result := Data.U['description'];
end;

function TOpenApiResponse.Schema: POpenApiSchema;
begin
  result := POpenApiSchema(Data.O['schema']);
end;



{ TOpenApiParameter }

function TOpenApiParameter.AllowEmptyValues: Boolean;
begin
  Result := Data.B['allowEmptyValue'];
end;

function TOpenApiParameter.AsSchema: POpenApiSchema;
begin
  result := POpenApiSchema(@self);
end;

function TOpenApiParameter.Default: PVariant;
begin
  // TODO: Check this
  result := Data.GetPVariantByPath('default');
end;

function TOpenApiParameter._In: RawUtf8;
begin
  result := Data.U['in'];
end;

function TOpenApiParameter.Name: RawUtf8;
begin
  result := Data.U['name'];
end;

function TOpenApiParameter.Required: Boolean;
begin
  result := Data.B['required'];
end;

function TOpenApiParameter.Schema: POpenApiSchema;
begin
  if not Data.GetAsObject('schema', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiParameter.AsFpcName: RawUtf8;
begin
  result := TFpcProperty.SanitizePropertyName(Name);
end;


const
  FPC_RESERVED_KEYWORDS: array[0..73] of RawUtf8 = (
    'absolute', 'and', 'array', 'as', 'asm', 'begin', 'case', 'const', 'constructor',
    'destructor', 'div', 'do', 'else', 'end', 'except', 'exports', 'external', 'false',
    'true', 'far', 'file', 'finalization', 'finally', 'for', 'forward', 'function',
    'goto', 'if', 'then', 'implementation', 'in', 'inherited', 'initialization',
    'interface', 'is', 'label', 'library', 'mod', 'near', 'new', 'nil', 'not',
    'object', 'of', 'on', 'operator', 'or', 'override', 'packed', 'procedure',
    'program', 'property', 'protected', 'public', 'published', 'raise', 'read',
    'reintroduce', 'repeat', 'self', 'shl', 'shr', 'threadvar', 'to', 'try','type',
    'unit', 'uses', 'var', 'virtual', 'while', 'with', 'write', 'xor');

function TOpenApiTag.GetDescription: RawUtf8;
begin
  result := DocVariant^.U['description'];
end;

function TOpenApiTag.GetDV: PDocVariantData;
begin
  result := PDocVariantData(@self);
end;

function TOpenApiTag.GetName: RawUtf8;
begin
  result := DocVariant^.U['name'];
end;


{ TOpenApiPathItem }

function TOpenApiPathItem.GetDescription: RawUtf8;
begin
  result := DocVariant^.U['description'];
end;

function TOpenApiPathItem.GetDV: PDocVariantData;
begin
  result := PDocVariantData(@self);
end;

function TOpenApiPathItem.GetOperationByMethod(aMethod: TUriMethod
  ): POpenApiOperation;
begin
  if not DocVariant^.GetAsObject(ToText(aMethod), PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiPathItem.GetParameterByIndex(aIndex: Integer
  ): POpenApiParameter;
begin
  result := POpenApiParameter(@Parameters^.Values[aIndex]);
end;

function TOpenApiPathItem.GetParameters: PDocVariantData;
begin
  result := DocVariant^.A['parameters'];
end;

function TOpenApiPathItem.GetSummary: RawUtf8;
begin
  result := DocVariant^.U['summary'];
end;


{ TOpenApiOperation }

function TOpenApiOperation.GetDeprecated: Boolean;
begin
  result := DocVariant^.B['deprecated'];
end;

function TOpenApiOperation.GetDescription: RawUtf8;
begin
  result := DocVariant^.U['description'];
end;

function TOpenApiOperation.GetDV: PDocVariantData;
begin
  result := PDocVariantData(@self);
end;

function TOpenApiOperation.GetId: RawUtf8;
begin
  result := DocVariant^.U['operationId'];
end;

function TOpenApiOperation.GetParameterByIndex(aIndex: Integer
  ): POpenApiParameter;
begin
  result := POpenApiParameter(@Parameters.Values[aIndex]);
end;

function TOpenApiOperation.GetParameters: PDocVariantData;
begin
  result := DocVariant^.A['parameters'];
end;

function TOpenApiOperation.GetPayloadParameter: POpenApiParameter;
var
  p: PVariant;
begin
  result := nil;
  if Assigned(Parameters) then
    for p in Parameters^.Items do
    begin
      result := POpenApiParameter(p);
      if (result^._In = 'body') and (result^.Name = 'payload') then
        exit;
    end;
  result := nil;
end;

function TOpenApiOperation.GetResponseForStatusCode(aStatusCode: Integer
  ): POpenApiResponse;
var
  StatusUtf8: RawUtf8;
begin
  if aStatusCode = 0 then
    StatusUtf8 := 'default'
  else
    StatusUtf8 := ToUtf8(aStatusCode);
  if not Responses^.GetAsObject(StatusUtf8, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiOperation.GetResponses: PDocVariantData;
begin
  result := DocVariant^.O['responses'];
end;

function TOpenApiOperation.GetSummary: RawUtf8;
begin
  result := DocVariant^.U['summary'];
end;

function TOpenApiOperation.GetTags: PDocVariantData;
begin
  result := DocVariant^.A['tags'];
end;


{ TOpenApiSpecs }

function TOpenApiSpecs.GetBasePath: RawUtf8;
begin
  Result := DocVariant^.U['basePath'];
end;

function TOpenApiSpecs.GetDefinitionByName(aName: RawUtf8): POpenApiSchema;
begin
  if not Definitions^.GetAsObject(aName, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSpecs.GetDefinitions: PDocVariantData;
begin
  Result := DocVariant^.O['definitions'];
end;

function TOpenApiSpecs.GetDV: PDocVariantData;
begin
  result := PDocVariantData(@self);
end;

function TOpenApiSpecs.GetPathItemByName(aPath: RawUtf8): POpenApiPathItem;
begin
  if not Paths^.GetAsObject(aPath, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSpecs.GetPaths: PDocVariantData;
begin
  result := DocVariant^.O['paths'];
end;

function TOpenApiSpecs.GetTags: PDocVariantData;
begin
  result := DocVariant^.A['tags'];
end;

function TOpenApiSpecs.GetVersion: RawUtf8;
begin
  if DocVariant.Exists('swagger') then
    result := DocVariant.U['swagger']
  else
    result := DocVariant.U['openapi'];
end;



{ ************************************ FPC/Delphi Pascal Code Generation }

{ TFpcOperation }

constructor TFpcOperation.Create(aPath: RawUtf8; aPathItem: POpenApiPathItem;
  aOperation: POpenApiOperation; aMethod: TUriMethod);
begin
  fPath := aPath;
  fPathItem := aPathItem;
  fOperation := aOperation;
  fMethod := aMethod;
end;

procedure TFpcOperation.ResolveTypes(Parser: TOpenApiParser);
var
  PayloadParam: POpenApiParameter;
  StatusCode: PRawUtf8;
  StatusCodeInt: Int64;
begin
  PayloadParam := fOperation^.PayloadParameter;
  if Assigned(PayloadParam) then
    fPayloadParameterType := TFpcType.LoadFromSchema(PayloadParam^.Schema, Parser);

  for StatusCode in fOperation.Responses^.FieldNames do
  begin
    StatusCodeInt := StrToInt64Def(StatusCode^, 0);
    if (StatusCode^ = 'default') or ((StatusCodeInt >= 200) and (StatusCodeInt < 400)) then
    begin
      fSuccessResponseCode := StatusCodeInt;
      fSuccessResponseType := TFpcType.LoadFromSchema(fOperation.Response[fSuccessResponseCode]^.Schema, Parser);
    end;
  end;
end;

function TFpcOperation.GetAllParameters: POpenApiParameterDynArray;
var
  i, PathItemParamsCount: Integer;
begin
  PathItemParamsCount := fPathItem^.Parameters^.Count;
  SetLength(result, PathItemParamsCount + fOperation^.Parameters^.Count);

  for i := 0 to PathItemParamsCount - 1 do
    result[i] := fPathItem^.Parameter[i];
  for i := 0 to fOperation^.Parameters^.Count - 1 do
    result[i + PathItemParamsCount] := fOperation^.Parameter[i];
end;

function TFpcOperation.Documentation(LineIndent: RawUtf8): RawUtf8;
var
  Parameters: POpenApiParameterDynArray;
  Param: POpenApiParameter;
  ResponsesDv: PDocVariantData;
  StatusCode: PRawUtf8;
  StatusCodeInt: Int64;
  Response: POpenApiResponse;
begin
  result := FormatUtf8('%// [%] %%', [LineIndent, ToText(fMethod), fPath, LINE_END]);

  // Summary
  if fOperation^.Summary <> '' then
    Append(result, [LineIndent, '// Summary: ', fOperation^.Summary, LINE_END]);
  // Description
  if fOperation^.Description <> '' then
    Append(result, [LineIndent, '// Description:', LINE_END,
      LineIndent, '//   ', StringReplaceAll(fOperation^.Description, LINE_END, FormatUtf8('%%//   ', [LINE_END + LineIndent])), LINE_END]);

  // Parameters
  Parameters := GetAllParameters;
  if Assigned(Parameters) then
  begin
    Append(result, [LineIndent, '//', LINE_END,
                    LineIndent, '// Parameters:', LINE_END]);
    for Param in  Parameters do
    begin
      Append(result, [LineIndent, '// - [', Param^._In, '] ', Param^.AsFpcName]);
      if Param^.Required then
        Append(result, '*');
      if Assigned(Param^.Default) then
        Append(result, [' (default=', VariantToUtf8(Param^.Default^), ')']);
      if Param^.AsSchema^.Description <> '' then
        Append(result, ': ', Param^.AsSchema^.Description);
      Append(result, LINE_END);
    end;
  end;

  // Responses: TODO order by incremental status code
  ResponsesDv := fOperation^.Responses;
  if ResponsesDv^.Count > 0 then
  begin
    Append(result, [LineIndent, '//', LINE_END,
                    LineIndent, '// Responses:', LINE_END]);
    for StatusCode in ResponsesDv^.FieldNames do
    begin
      StatusCodeInt := StrToInt64Def(StatusCode^, 0);
      Append(result, [LineIndent, '// - ', StatusCode^]);
      if StatusCodeInt = fSuccessResponseCode then
        Append(result, '*');
      Response := POpenApiResponse(ResponsesDv^.O[StatusCode^]);
      if Response^.Description <> '' then
        Append(result, [': ', Response^.Description, LINE_END])
      else
        Append(result, ': No Description', LINE_END);
    end;
  end;
end;

function TFpcOperation.Declaration(ClassName: RawUtf8; Parser: TOpenApiParser): RawUtf8;
var
  Parameters: POpenApiParameterDynArray;
  Param: POpenApiParameter;
  ParamType: TFpcType;
  i, FctParamIndex: Integer;
begin
  if Assigned(fSuccessResponseType) then
    result := 'function '
  else
    result := 'procedure ';

  if ClassName <> '' then
    Append(Result, ClassName, '.');
  Append(result, FunctionName, '(');

  Parameters := GetAllParameters;
  FctParamIndex := 0;
  for i := 0 to Length(Parameters) - 1 do
  begin
    Param := Parameters[i];
    if (Param^._In = 'path') or (Param^._In = 'query') then
    begin
      if FctParamIndex > 0 then
        Append(result, '; ');
      ParamType := TFpcType.LoadFromSchema(Param^.AsSchema, Parser);
      Append(result, [Param^.AsFpcName, ': ', ParamType.ToFpcName]);
      Inc(FctParamIndex);
    end;
  end;

  if Assigned(fPayloadParameterType) then
  begin
    if FctParamIndex > 0 then
      Append(result, '; const payload: ', fPayloadParameterType.ToFpcName)
    else
      Append(result, 'const payload: ', fPayloadParameterType.ToFpcName);
  end;

  if Assigned(fSuccessResponseType) then
    Append(result, ['): ', fSuccessResponseType.ToFpcName, ';'])
  else
    Append(result, ');');
end;

function TFpcOperation.Body(ClassName: RawUtf8; BasePath: RawUtf8;
  Parser: TOpenApiParser): RawUtf8;
var
  Action: RawUtf8;
  ActionArgs: TRawUtf8DynArray;
  i: Integer;
  QueryParameters: TDocVariantData;
  Parameters: POpenApiParameterDynArray;
  Param: POpenApiParameter;
  ParamType: TFpcType;
begin
  Action := BasePath + fPath;
  ActionArgs := nil;
  QueryParameters.InitObject([], JSON_FAST);
  Parameters := GetAllParameters;

  for Param in Parameters do
  begin
    ParamType := TFpcType.LoadFromSchema(Param^.AsSchema, Parser);
    if Param^._In = 'path' then
    begin
      Action := StringReplaceAll(Action, FormatUtf8('{%}', [Param^.Name]), '%');
      SetLength(ActionArgs, Length(ActionArgs) + 1);
      ActionArgs[Length(ActionArgs) - 1] := ParamType.ToFormatUtf8Arg(Param^.AsFpcName);
    end
    else if Param^._In = 'query' then
    begin
      QueryParameters.AddValue(Param^.Name, ParamType.ToFormatUtf8Arg(Param^.AsFpcName));
    end;
  end;

   result := FormatUtf8('%%begin%  JsonClient.Request(''%'', ''%''', [Declaration(ClassName, Parser), LINE_END, LINE_END, ToText(fMethod), Action]);
   // Path parameters
   if Length(ActionArgs) > 0 then
   begin
     Append(result, ', [');
     for i := 0 to Length(ActionArgs) - 1 do
     begin
       if i > 0 then
         Append(result, ', ');
       Append(result, ActionArgs[i]);
     end;
     Append(result, ']');
   end
   // Either ActionArgs and QueryArgs or None of them (for Request parameters)
   else if (QueryParameters.Count > 0) or Assigned(fPayloadParameterType) then
     Append(result, ', []');

   // Query parameters
   if QueryParameters.Count > 0 then
   begin
     Append(result, ', [', LINE_END);
     for i := 0 to QueryParameters.Count - 1 do
     //for QueryParam in QueryParameters.Fields do
     begin
       if i > 0 then
         Append(result, ',', LINE_END);
       Append(result, ['    ''', QueryParameters.Names[i], ''', ', VariantToUtf8(QueryParameters.Values[i])]);
     end;
     Append(result, LINE_END, '    ]');
   end
   // Either ActionArgs and QueryArgs or None of them (for Request parameters)
   else if (Length(ActionArgs) > 0) or Assigned(fPayloadParameterType) then
     Append(result, ', []');

   // Payload if any
   if Assigned(fPayloadParameterType) then
   begin
     Append(result, ', Payload, ');
     if Assigned(fSuccessResponseType) then
       Append(result, 'Result, ')
     else
       Append(result, '{Not used, but need to send a pointer}Self, ');

     Append(result, ['TypeInfo(', fPayloadParameterType.ToFpcName, '), ']);
     if Assigned(fSuccessResponseType) then
       Append(result, ['TypeInfo(', fSuccessResponseType.ToFpcName, ')'])
     else
       Append(result, 'nil');
   end
   // Response type if any
   else if Assigned(fSuccessResponseType) then
   begin
     Append(result, [', Result, TypeInfo(', fSuccessResponseType.ToFpcName, ')']);
   end;

  Append(result, [');', LINE_END, 'end;', LINE_END]);
end;

function TFpcOperation.FunctionName: RawUtf8;
begin
  Result := fOperation^.Id;
end;


constructor TFpcProperty.Create(aName: RawUtf8; aSchema: POpenApiSchema;
  aType: TFpcType);
begin
  fName := aName;
  fSchema := aSchema;
  fType := aType;
  fFpcName := SanitizePropertyName(fName);
end;

constructor TFpcProperty.CreateFromSchema(aName: RawUtf8;
  aSchema: POpenApiSchema; Parser: TOpenApiParser; ParentField: Boolean);
begin
  fType := TFpcType.LoadFromSchema(aSchema, Parser);
  fType.fIsParent := ParentField;
  fName := aName;
  fSchema := aSchema;
  fFpcName := SanitizePropertyName(fName);
end;

class function TFpcProperty.SanitizePropertyName(aName: RawUtf8): RawUtf8;
begin
  CamelCase(aName, result);
  result[1] := UpCase(result[1]);
  if FindRawUtf8(FPC_RESERVED_KEYWORDS, result, False) <> -1 then
    result := '_' + result;
end;

function TFpcEnum.GetSchema: POpenApiSchema;
begin
  Result := fSchema;
end;

constructor TFpcEnum.Create(SchemaName: RawUtf8; aSchema: POpenApiSchema);
var
  c: Char;
begin
  inherited Create('T' + SchemaName);
  fName := SchemaName;
  fSchema := aSchema;
  fChoices.InitCopy(Variant(aSchema^.Enum^), JSON_FAST);
  fChoices.AddItem('None', 0);

  fPrefix := '';
  for c in SchemaName do
  begin
    // TODO: Find a IsUpperChar method somewhere in mORMot
    if c <> LowerCase(c) then
      Append(fPrefix, LowerCase(c));
  end;
end;

function TFpcEnum.ToTypeDefinition(LineIndentation: RawUtf8): RawUtf8;
var
  Choice: RawUtf8;
  i: Integer;
begin
  result := FormatUtf8('%% = (', [LineIndentation, FpcName]);

  for i := 0 to fChoices.Count - 1 do
  begin
    if i > 0 then
      Append(result,  ', ');

    Choice := StringReplaceAll(VariantToUtf8(fChoices[i]), ' ', '');
    Choice[1] := UpCase(Choice[1]);
    Append(Result, fPrefix, Choice);
  end;
  Append(result, [');', LINE_END, ToArrayTypeDefinition(LineIndentation)]);
end;

function TFpcEnum.ToArrayTypeName(AllowArrayType: Boolean): RawUtf8;
begin
  if AllowArrayType then
    result := FormatUtf8('%Set', [FpcName])
  else
    result := FormatUtf8('set of %', [FpcName]);
end;

function TFpcEnum.ToConstTextArrayName: RawUtf8;
begin
  result := FormatUtf8('%_2TXT', [UpperCase(fName)]);
end;

function TFpcEnum.ToConstTextArray: RawUtf8;
var
  i: Integer;
begin
  result := FormatUtf8('%: array[%] of RawUtf8 = (', [ToConstTextArrayName, FpcName]);
  for i := 0 to fChoices.Count - 1 do
  begin
    if i = 0 then
      Append(result, '''''') // None entry
    else
      Append(result, [', ''', StringReplaceAll(VariantToUtf8(fChoices[i]), ' ', ''), '''']);
  end;
  Append(result, ');');
end;


function TFpcType.IsBuiltin: Boolean;
begin
  result := not Assigned(fCustomType);
end;

function TFpcType.GetSchema: POpenApiSchema;
begin
  if Assigned(CustomType) then
    result := CustomType.GetSchema
  else
    result := fBuiltinSchema;
end;

function TFpcType.IsEnum: Boolean;
begin
  result := Assigned(fCustomType) and (fCustomType is TFpcEnum);
end;

function TFpcType.IsRecord: Boolean;
begin
  result := Assigned(fCustomType) and (fCustomType is TFpcRecord);
end;

procedure TFpcType.SetArray(AValue: Boolean);
begin
  fIsArray := AValue;
  if AValue and Assigned(CustomType) then
    CustomType.fRequiresArrayDefinition := True;
end;

constructor TFpcType.CreateBuiltin(aBuiltinTypeName: RawUtf8;
  aSchema: POpenApiSchema; aIsArray: Boolean);
begin
  fBuiltinTypeName := aBuiltinTypeName;
  fBuiltinSchema := aSchema;
  IsArray := aIsArray;
end;

constructor TFpcType.CreateCustom(aCustomType: TFpcCustomType;
  aIsArray: Boolean; aIsParent: Boolean);
begin
  fCustomType := aCustomType;
  IsArray := aIsArray;
  fIsParent := aIsParent;
end;

class function TFpcType.LoadFromSchema(Schema: POpenApiSchema;
  Parser: TOpenApiParser): TFpcType;
var
  Rec: TFpcRecord;
begin
  if (Schema^.Reference <> '') or Assigned(Schema^.AllOf) then
  begin
    if Assigned(Schema^.AllOf) then
    begin
      if Schema^.AllOf^.Count <> 1 then
        raise ENotImplemented.Create('TFpcType.LoadFromSchema only handles "allOf" attribute with a single reference');
      Rec := Parser.GetRecord(POpenApiSchema(@Schema^.AllOf^.Values[0])^.Reference, True);
    end else
      Rec := Parser.GetRecord(Schema^.Reference, True);
    result := TFpcType.CreateCustom(Rec);
  end
  else if Schema^.IsArray then
  begin
    // TODO: Handle array of arrays
    result := LoadFromSchema(Schema^.Items, Parser);
    result.fBuiltinSchema := Schema;
    result.IsArray := True;
  end
  else if Schema^.IsNamedEnum then
  begin
    if not Parser.fEnums.ContainsKey(Schema^._Format) then
    begin
      result := TFpcType.CreateCustom(TFpcEnum.Create(Schema^._Format, Schema));
      Parser.fEnums.Add(Schema^._Format, result.CustomType as TFpcEnum);
    end
    else
      result := TFpcType.CreateCustom(Parser.fEnums.GetItem(Schema^._Format));
  end
  else
    result := TFpcType.CreateBuiltin(GetBuiltinType(Schema), Schema);
end;

class function TFpcType.GetBuiltinType(Schema: POpenApiSchema): RawUtf8;
var
  _Type, _Format: RawUtf8;
begin
  _Type := Schema._Type;
  _Format := Schema._Format;

  if _Type = 'integer' then
  begin
    if _Format = 'int64' then
        result := 'Int64'
    else
      result := 'Integer';
  end
  else if _Type = 'number' then
  begin
    if _Format = 'float' then
      result := 'Single'
    else
      result := 'Double';
  end
  else if _Type = 'string' then
  begin
    if _Format = 'date' then
      result := 'TDate'
    else if _Format = 'date-time' then
      result := 'TDateTime'
    else if _Format = 'uuid' then
      result := 'TGuid'
    else
      result := 'RawUtf8';
  end
  else if _Type = 'boolean' then
    result := 'Boolean'
  else
    result := 'TDocVariantData';
end;

function TFpcType.ToFpcName(AllowArrayType: Boolean; NoRecordArrayTypes: Boolean
  ): RawUtf8;
var
  OkArrayType: Boolean;
begin
  if Assigned(CustomType) then
    result := CustomType.FpcName
  else
    result := fBuiltinTypeName;
  if not AllowArrayType or NoRecordArrayTypes and (result = 'TDocVariantData') then
    result := 'Variant';

  if IsArray then
  begin
    if Assigned(CustomType) then
    begin
      OkArrayType := AllowArrayType;
      if NoRecordArrayTypes and AllowArrayType and IsRecord then
         OkArrayType := false;
      result := CustomType.ToArrayTypeName(OkArrayType);
    end
    else
      result := FormatUtf8('array of %', [result]);
  end;
end;

function TFpcType.ToFormatUtf8Arg(FpcVarName: RawUtf8): RawUtf8;
begin
  if IsBuiltin and (fBuiltinTypeName = 'TGuid') then
    result := FormatUtf8('ToUtf8(%)', [FpcVarName])
  else if IsEnum then
    result := FormatUtf8('%[%]', [(fCustomType as TFpcEnum).ToConstTextArrayName, FpcVarName])
  else
    result := FpcVarName;
end;

function TFpcRecord.GetSchema: POpenApiSchema;
begin
  Result := fSchema;
end;

constructor TFpcRecord.Create(SchemaName: RawUtf8; Schema: POpenApiSchema);
begin
  fName := SchemaName;
  fSchema := Schema;
  fProperties := Collections.NewKeyValue<RawUtf8, TFpcProperty>;
  fDependencies := nil;
  inherited Create('T' + fName);
end;

function TFpcRecord.ToTypeDefinition(LineIndentation: RawUtf8): RawUtf8;
var
  aProp: TPair<RawUtf8, TFpcProperty>;
  PropSchema: POpenApiSchema;
begin
  result := LineIndentation;
  Append(result, [FpcName, ' = packed record', LINE_END]);
  for aProp in fProperties do
  begin
    PropSchema := aProp.Value.fSchema;
    if Assigned(PropSchema) and (PropSchema^.Description <> '') then
      Append(result, [LineIndentation, '  /// ', PropSchema^.Description, LINE_END]);
    Append(result, [LineIndentation, '  ', aProp.Value.fFpcName, ': ', aProp.Value.fType.ToFpcName, ';', LINE_END]);
  end;
  Append(result, [LineIndentation, 'end;', LINE_END, ToArrayTypeDefinition(LineIndentation)]);
end;

function TFpcRecord.ToRttiTextRepresentation(WithClassName: Boolean): RawUtf8;
var
  aProp: TPair<RawUtf8, TFpcProperty>;
begin
  if WithClassName then
    result := FormatUtf8('_% = ''', [FpcName])
  else
    result := '';

  for aProp in fProperties do
  begin
    // Only records can be parent types
    if aProp.Value.fType.IsParent then
      Append(result, (aProp.Value.fType.CustomType as TFpcRecord).ToRttiTextRepresentation(False))
    else
      Append(result, [aProp.Key, ': ', aProp.Value.fType.ToFpcName(True, True), '; ']);
  end;

  if WithClassName then
    Append(result, ''';');
end;

function TFpcRecord.ToRttiRegisterDefinitions: RawUtf8;
begin
  result := FormatUtf8('TypeInfo(%), _%', [FpcName, FpcName]);
end;

function TFpcCustomType.GetSchema: POpenApiSchema;
begin
  Result := nil;
end;

constructor TFpcCustomType.Create(aFpcName: RawUtf8);
begin
  fFpcName := aFpcName;
  fRequiresArrayDefinition := False;
end;

function TFpcCustomType.ToArrayTypeName(AllowArrayType: Boolean): RawUtf8;
begin
  if AllowArrayType then
    result := FormatUtf8('%DynArray', [FpcName])
  else
    result := FormatUtf8('array of %', [FpcName]);
end;

function TFpcCustomType.ToArrayTypeDefinition(LineIndentation: RawUtf8
  ): RawUtf8;
begin
  if fRequiresArrayDefinition then
    result := FormatUtf8('%% = %;'#10, [LineIndentation, ToArrayTypeName(True), ToArrayTypeName(False)])
  else
    result := '';
end;

function TOpenApiParser.GetSpecs: POpenApiSpecs;
begin
  Result := POpenApiSpecs(@fSpecs);
end;

constructor TOpenApiParser.Create;
begin
  fSpecs.FillZero;
  fRecords := Collections.NewKeyValue<RawUtf8, TFpcRecord>;
  fEnums := Collections.NewKeyValue<RawUtf8, TFpcEnum>;
end;

destructor TOpenApiParser.Destroy;
begin
  // TODO: Free all FpcTypes
  fRecords := nil;
  fEnums := nil;
  inherited Destroy;
end;

procedure TOpenApiParser.Clear;
begin
  fRecords.Clear;
  if not fSpecs.IsVoid then
    fSpecs.Clear;
end;

procedure TOpenApiParser.Parse(aSpecs: TDocVariantData);
var
  DefinitionName, PathName: PRawUtf8;
begin
  Clear;
  fSpecs.InitCopy(Variant(aSpecs), JSON_FAST);

  for DefinitionName in Specs^.Definitions^.FieldNames do
  begin
    if not fRecords.ContainsKey(DefinitionName^) then
      fRecords.SetItem(DefinitionName^, ParseDefinition(DefinitionName^));
  end;

  for PathName in Specs^.Paths^.FieldNames do
    ParsePath(PathName^);
end;

function TOpenApiParser.ParseDefinition(aDefinitionName: RawUtf8): TFpcRecord;
var
  Schema: POpenApiSchema;
  PropertyName: PRawUtf8;
  ParentCounts, i: Integer;
  Schemas: array of POpenApiSchema;
  ParentRecord: TFpcRecord;
  ParentPropName: String;
  aSchema: POpenApiSchema;
begin
  Schema := Specs^.Definition[aDefinitionName];
  if not Assigned(Schema) then
    EOpenApi.RaiseUtf8('Cannot parse missing definition: %', [aDefinitionName]);

  result := TFpcRecord.Create(aDefinitionName, Schema);


  ParentCounts := 0;
  if Assigned(result.Schema^.AllOf) then
  begin
    SetLength(Schemas, Schema.AllOf^.Count);
    for i := 0 to Schema.AllOf^.Count - 1 do
      Schemas[i] := POpenApiSchema(@Schema.AllOf^.Values[i]);
  end
  else
  begin
    SetLength(Schemas, 1);
    Schemas[0] := Schema;
  end;

  for aSchema in Schemas do
  begin
    if aSchema^.Reference <> '' then
    begin
      ParentRecord := GetRecord(aSchema^.Reference, True);
      if ParentCounts = 0 then
        ParentPropName := 'base'
      else
        ParentPropName := FormatUtf8('base_%', [ParentCounts]);
      Inc(ParentCounts);
      result.fProperties.Add(ParentPropName, TFpcProperty.CreateFromSchema(ParentPropName, aSchema, Self, True));
    end
    else if aSchema^.IsObject then
      for PropertyName in aSchema^.Properties^.FieldNames do
        result.fProperties.Add(PropertyName^, TFpcProperty.CreateFromSchema(PropertyName^, aSchema^._Property[PropertyName^], Self));
  end;
end;

procedure TOpenApiParser.ParsePath(aPath: RawUtf8);
var
  Method: TUriMethod;
  PathItem: POpenApiPathItem;
  OperationSchema: POpenApiOperation;
  Operation: TFpcOperation;
begin
  PathItem := Specs^.Path[aPath];

  for Method in TUriMethod do
  begin
    OperationSchema := PathItem^.Method[method];
    if not Assigned(OperationSchema) or OperationSchema^.Deprecated then
      continue;
    Operation := TFpcOperation.Create(aPath, PathItem, OperationSchema, Method);
    Operation.ResolveTypes(Self);
    SetLength(fOperations, Length(fOperations) + 1);
    fOperations[Length(fOperations) - 1] := Operation;
  end;
end;

function TOpenApiParser.GetRecord(aRecordName: RawUtf8; NameIsReference: Boolean
  ): TFpcRecord;
begin
  if NameIsReference then
    aRecordName := SplitRight(aRecordName, '/');
  if not fRecords.ContainsKey(aRecordName) then
  begin
    result := ParseDefinition(aRecordName);
    fRecords.Add(aRecordName, Result);
  end else
    result := fRecords.GetItem(aRecordName);
end;

function TOpenApiParser.GetOrderedRecords: TFpcRecordDynArray;
var
  UnresolvedDependencies, Missings: TFpcRecordDynArray;
  Rec: TPair<RawUtf8, TFpcRecord>;
  UnresolvedRec: TFpcRecord;

  function HasDependencies(Sources: TFpcRecordDynArray; Searched: TRawUtf8DynArray): Boolean;
  var
    RecName: RawUtf8;
    r: TFpcRecord;
    RecResolved: Boolean;
  begin
    result := False;
    for RecName in Searched do
    begin
      RecResolved := False;
      for r in Sources do
        if r.fName = RecName then
          RecResolved := True;
      if not RecResolved then
        exit;
    end;
    result := True;
  end;

begin
  result := nil;
  UnresolvedDependencies := nil;

  for Rec in fRecords do
  begin
    if not Assigned(Rec.Value.fDependencies) or HasDependencies(result, Rec.Value.fDependencies) then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result) - 1] := Rec.Value;
    end else
    begin
      SetLength(UnresolvedDependencies, Length(UnresolvedDependencies) + 1);
      UnresolvedDependencies[Length(UnresolvedDependencies) - 1] := Rec.Value;
    end;
  end;

  while Length(UnresolvedDependencies) > 0 do
  begin
    Missings := nil;
    for UnresolvedRec in UnresolvedDependencies do
    begin
      if HasDependencies(result, UnresolvedRec.fDependencies) then
      begin
        SetLength(result, Length(result) + 1);
        result[Length(result) - 1] := UnresolvedRec;

      end else
      begin
        SetLength(Missings, Length(Missings) + 1);
        Missings[Length(Missings) - 1] := UnresolvedRec;
      end;
    end;
    UnresolvedDependencies := Missings;
  end;
end;

function TOpenApiParser.GetOperationsByTag: IKeyValue<POpenApiTag, IList<TFpcOperation>>;
var
  TagByName: IKeyValue<RawUtf8, POpenApiTag>;
  tag, opTag: PVariant;
  Op: TFpcOperation;
begin
  // TODO: Juse use TDocVariant copying by reference ?
  TagByName := Collections.NewKeyValue<RawUtf8, POpenApiTag>;
  result := Collections.NewKeyValue<POpenApiTag, IList<TFpcOperation>>;

  for tag in Specs^.Tags^.Items do
  begin
    TagByName.Add(POpenApiTag(tag)^.Name, POpenApiTag(tag));
    result.Add(POpenApiTag(tag), Collections.NewList<TFpcOperation>);
  end;

  for Op in fOperations do
  begin
    for opTag in op.fOperation^.Tags^.Items do
      result.GetItem(TagByName.GetItem(VariantToUtf8(opTag^))).Add(Op);
  end;
end;

procedure TOpenApiParser.Dump;
var
  OrderedRecs: TFpcRecordDynArray;
  rec: TFpcRecord;
begin
  OrderedRecs := GetOrderedRecords;
  for rec in OrderedRecs do
  begin
    WriteLn(rec.ToTypeDefinition);
  end;
end;

function TOpenApiParser.GetDtosUnit(UnitName: RawUtf8): RawUtf8;
var
  OrderedRecords: TFpcRecordDynArray;
  e: TPair<RawUtf8, TFpcEnum>;
  rec: TFpcRecord;
  i: Integer;
begin
  OrderedRecords := GetOrderedRecords;

  result := '';
  Append(result, [
    'unit ', UnitName, ';', LINE_END , LINE_END,
    '{$mode ObjFPC}{$H+}', LINE_END ,
    '{$modeSwitch advancedRecords}', LINE_END ,
    LINE_END,
    'interface', LINE_END,
    LINE_END,
    'uses', LINE_END,
    '  Classes,', LINE_END,
    '  SysUtils,', LINE_END,
    '  mormot.core.base,', LINE_END,
    '  mormot.core.variants;', LINE_END,
    LINE_END,
    'type', LINE_END, LINE_END, LINE_END]);

  for e in fEnums do
    Append(result, e.Value.ToTypeDefinition('  '));
  if fEnums.Count > 0 then
    Append(result, LINE_END, LINE_END);

  for rec in OrderedRecords do
    Append(result, rec.ToTypeDefinition('  '), LINE_END);

  if self.fEnums.Count > 0 then
  begin
    Append(result, [LINE_END, LINE_END, 'const', LINE_END, LINE_END]);
    for e in fEnums do
      Append(result, ['  ', e.Value.ToConstTextArray, LINE_END]);
  end;

  Append(result, [LINE_END, LINE_END,
  'implementation', LINE_END, LINE_END,
  'uses', LINE_END,
  '  mormot.core.rtti;', LINE_END, LINE_END,
  'const', LINE_END]);

  for rec in OrderedRecords do
    Append(result, ['  ', rec.ToRttiTextRepresentation, LINE_END]);

  Append(result, [LINE_END, LINE_END,
  'procedure RegisterRecordsRttis;', LINE_END,
  'begin', LINE_END]);

  if self.fEnums.Count > 0 then
  begin
    Append(result, '  Rtti.RegisterTypes([', LINE_END);
    for i := 0 to fEnums.Count - 1 do
    begin
      if i > 0 then
        Append(result, ',', LINE_END);
      Append(result, ['    TypeInfo(', fEnums.GetValue(i).FpcName, ')']);
      if fEnums.GetValue(i).fRequiresArrayDefinition then
        Append(result, [',', LINE_END, '    TypeInfo(', fEnums.GetValue(i).ToArrayTypeName, ')']);
    end;
    Append(result, LINE_END, '  ]);');
  end;

  Append(result, [LINE_END, '  Rtti.RegisterFromText([', LINE_END]);

  for i := 0 to Length(OrderedRecords) - 1 do
  begin
    if i > 0 then
      Append(result, ',', LINE_END);
    Append(result, '    ', OrderedRecords[i].ToRttiRegisterDefinitions);
  end;

  Append(result, [']);', LINE_END,
  'end;', LINE_END,
  LINE_END,
  'initialization', LINE_END,
  '  RegisterRecordsRttis;', LINE_END,
  LINE_END,
  'end.', LINE_END]);
end;

function TOpenApiParser.GetClientUnit(UnitName, ClientClassName,
  DtoUnitName: RawUtf8): RawUtf8;
var
  OperationsByTag: IKeyValue<POpenApiTag, IList<TFpcOperation>>;
  DeclaredOperations: TRawUtf8DynArray;
  TagOperations: TPair<POpenApiTag, IList<TFpcOperation>>;
  BannerLength: SizeInt;
  Operation: TFpcOperation;
begin
  result := '';
  Append(result, [
    'unit ', UnitName, ';', LINE_END,
    LINE_END,
    '{$mode ObjFPC}{$H+}', LINE_END,
    LINE_END,
    'interface', LINE_END,
    LINE_END,
    'uses', LINE_END,
    '  Classes,', LINE_END,
    '  SysUtils,', LINE_END,
    '  mormot.core.base,', LINE_END,
    '  mormot.core.variants,', LINE_END,
    '  mormot.net.client,', LINE_END,
    '  ', DtoUnitName, ';', LINE_END,
    LINE_END,
    'type',
    LINE_END,
    '  ', ClientClassName, ' = class', LINE_END,
    '  private', LINE_END,
    '    fClient: IJsonClient;', LINE_END,
    '  public', LINE_END,
    '    constructor Create(aClient: IJsonClient = nil);', LINE_END,
    '    destructor Destroy; override;', LINE_END]);

  OperationsByTag := GetOperationsByTag;
  DeclaredOperations := [];

  for TagOperations in OperationsByTag do
  begin
    BannerLength := 18 + Length(TagOperations.Key^.Name) + Length(TagOperations.Key^.Description);
    Append(result, [LINE_END, LINE_END, LINE_END,
      '    ////', StringOfChar('/', BannerLength), LINE_END,
      '    //// ---- ', TagOperations.Key^.Name, ': ', TagOperations.Key^.Description, ' ---- ////', LINE_END,
      '    ////', StringOfChar('/', BannerLength), LINE_END, LINE_END]);

    for Operation in TagOperations.Value do
    begin
      // Operations can be in multiple tags but can't be defined multiple times in same class
      if FindRawUtf8(DeclaredOperations, Operation.fOperation^.Id) <> -1 then
        continue;
      SetLength(DeclaredOperations, Length(DeclaredOperations) + 1);
      DeclaredOperations[Length(DeclaredOperations) - 1] := Operation.fOperation^.Id;

      Append(result, Operation.Documentation('    '));
      Append(result, ['    ', Operation.Declaration('', Self), LINE_END]);
    end;
  end;

  Append(result, [LINE_END, LINE_END,
    '    property JsonClient: IJsonClient read fClient write fClient;', LINE_END,
    '  end;', LINE_END,
    LINE_END,
    'implementation', LINE_END,
    LINE_END,
    'uses', LINE_END,
    '  mormot.core.text,', LINE_END,
    '  mormot.core.rtti;', LINE_END,
    LINE_END,
    'constructor ', ClientClassName, '.Create(aClient: IJsonClient);', LINE_END,
    'begin', LINE_END,
    '  fClient := aClient;', LINE_END,
    'end;', LINE_END,
    LINE_END,
    'destructor ', ClientClassName, '.Destroy;', LINE_END,
    'begin', LINE_END,
    '  fClient := nil;', LINE_END,
    '  inherited Destroy;', LINE_END,
    'end;', LINE_END, LINE_END]);

  for Operation in fOperations do
    Append(result, Operation.Body(ClientClassName, Specs^.BasePath, Self), LINE_END);

  Append(result, LINE_END, 'end.');
end;

procedure TOpenApiParser.ExportToDirectory(Name: RawUtf8;
  DirectoryName: RawUtf8; UnitPrefix: RawUtf8);
var
  DtoUnitName, ClientUnitName: RawUtf8;
  DtoUnitPath, ClientUnitPath: TFileName;
begin
  DtoUnitName := FormatUtf8('%%Dtos', [UnitPrefix, Name]);
  DtoUnitPath := MakePath([DirectoryName, DtoUnitName + '.pas']);
  ClientUnitName := FormatUtf8('%%Client', [UnitPrefix, Name]);
  ClientUnitPath := MakePath([DirectoryName, ClientUnitName + '.pas']);

  FileFromString(GetDtosUnit(DtoUnitName), DtoUnitPath);
  FileFromString(GetClientUnit(ClientUnitName, FormatUtf8('T%Client', [Name]), DtoUnitName), ClientUnitPath);
end;


end.

