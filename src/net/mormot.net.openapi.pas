unit mormot.net.openapi;

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.text,
  mormot.core.collections;

const
  SCHEMA_TYPE_ARRAY: RawUtf8 = 'array';
  SCHEMA_TYPE_OBJECT: RawUtf8 = 'object';
  LINE_END: RawUtf8 = #10;

type

  POpenApiSchema = ^TOpenApiSchema;
  TOpenApiSchema = record
  private
    function GetAllOf: PDocVariantData;
    function GetItems: POpenApiSchema;
    function GetProperties: PDocVariantData;
    function GetPropertyByName(aName: RawUtf8): POpenApiSchema;
    function GetReference: RawUtf8;
    function IsNullable: Boolean;
    function GetRequired: PDocVariantData;
    function GetDescription: RawUtf8;
    function GetDV: PDocVariantData;
    function GetEnum: PDocVariantData;
    function GetExample: Variant;
    function GetFormat: RawUtf8;
    function GetType: RawUtf8;
  public
    property DocVariant: PDocVariantData read GetDV;

    property _Type: RawUtf8 read GetType;
    property _Format: RawUtf8 read GetFormat;
    property Required: PDocVariantData read GetRequired;
    property Enum: PDocVariantData read GetEnum;
    property Nullable: Boolean read IsNullable;
    property Description: RawUtf8 read GetDescription;
    property Example: Variant read GetExample;
    property Reference: RawUtf8 read GetReference;
    property AllOf: PDocVariantData read GetAllOf;
  // allOf, schema ?

    property Items: POpenApiSchema read GetItems;
    property Properties: PDocVariantData read GetProperties;
    property _Property[aName: RawUtf8]: POpenApiSchema read GetPropertyByName;

    // Helpers
    function IsArray: Boolean;
    function IsObject: Boolean;
    function IsNamedEnum: Boolean;
  end;


  TOpenApiSpecs = record
  private
    function GetDefinitionByName(aName: RawUtf8): POpenApiSchema;
    function GetDefinitions: PDocVariantData;
    function GetDV: PDocVariantData;
    function GetVersion: RawUtf8;
  public
    property DocVariant: PDocVariantData read GetDV;

    property Definitions: PDocVariantData read GetDefinitions;
    property Definition[aName: RawUtf8]: POpenApiSchema read GetDefinitionByName;
    property Version: RawUtf8 read GetVersion;
  end;
  POpenApiSpecs = ^TOpenApiSpecs;

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

  TOpenApiParser = class
  private
    fSpecs: TDocVariantData;

    fRecords: IKeyValue<RawUtf8, TFpcRecord>;
    fEnums: IKeyValue<RawUtf8, TFpcEnum>;

    function GetSpecs: POpenApiSpecs;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Parse(aSpecs: TDocVariantData);
    function ParseDefinition(aDefinitionName: RawUtf8): TFpcRecord;

    function GetRecord(aRecordName: RawUtf8; NameIsReference: Boolean = False): TFpcRecord;
    function GetOrderedRecords: TFpcRecordDynArray;

    procedure Dump;
    function GetDtosUnit(UnitName: RawUtf8): RawUtf8;

    property Specs: POpenApiSpecs read GetSpecs;
  end;

  EOpenApi = class(ESynException)
  public
  end;


implementation

uses
  mormot.core.unicode;

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
  DefinitionName: PRawUtf8;
begin
  Clear;
  fSpecs.InitCopy(Variant(aSpecs), JSON_FAST);

  for DefinitionName in Specs^.Definitions^.FieldNames do
  begin
    if not fRecords.ContainsKey(DefinitionName^) then
      fRecords.SetItem(DefinitionName^, ParseDefinition(DefinitionName^));
  end;
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
    raise EOpenApi.CreateUtf8('Cannot parse missing definition: %', [aDefinitionName]);

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
  'begin', LINE_END,
  '  Rtti.RegisterFromText([', LINE_END]);

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

function TOpenApiSchema.GetAllOf: PDocVariantData;
begin
  if not DocVariant^.GetAsArray('allOf', result) then
    result := nil;
end;

function TOpenApiSchema.GetItems: POpenApiSchema;
begin
  if not IsArray then
    raise EOpenApi.CreateUtf8('Trying to access items field on a non array schema: %', [_Type]);
  if not DocVariant^.GetAsObject('items', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSchema.GetProperties: PDocVariantData;
begin
  if not IsObject then
    raise EOpenApi.CreateUtf8('Trying to access properties field on a non object schema: %', [_Type]);
  if not DocVariant^.GetAsObject('properties', PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSchema.GetPropertyByName(aName: RawUtf8): POpenApiSchema;
begin
  if not Properties^.GetAsObject(aName, PDocVariantData(result)) then
    result := nil;
end;

function TOpenApiSchema.GetReference: RawUtf8;
begin
  result := DocVariant^.U['$ref']
end;

function TOpenApiSchema.IsNullable: Boolean;
begin
  result := DocVariant^.B['nullable'];
end;

function TOpenApiSchema.GetRequired: PDocVariantData;
begin
  result := DocVariant^.A['required']
end;

function TOpenApiSchema.GetDescription: RawUtf8;
begin
  result := DocVariant^.U['description']
end;

function TOpenApiSchema.GetDV: PDocVariantData;
begin
  result := PDocVariantData(@self);
end;

function TOpenApiSchema.GetEnum: PDocVariantData;
begin
  if not DocVariant^.GetAsArray('enum', result) then
    result := nil;
end;

function TOpenApiSchema.GetExample: Variant;
begin
  result := DocVariant^.Value['example'];
end;

function TOpenApiSchema.GetFormat: RawUtf8;
begin
  result := DocVariant^.U['format'];
end;

function TOpenApiSchema.GetType: RawUtf8;
begin
  result := DocVariant^.U['type'];
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
  Result := Assigned(Enum) and (_Format <> '');
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

function TOpenApiSpecs.GetVersion: RawUtf8;
begin
  if DocVariant.Exists('swagger') then
    result := DocVariant.U['swagger']
  else
    result := DocVariant.U['openapi'];
end;

end.

