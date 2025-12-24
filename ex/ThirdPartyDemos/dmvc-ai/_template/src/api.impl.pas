unit api.impl;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.orm.core,
  api.interfaces,
  entities;

type
  /// implement the Template Sample API
  TTemplateSample = class(TInterfacedObject, ITemplateSample)
  protected
    fRest: IRestOrm;
    fSampleMap: TRttiMap;
  public
    constructor Create(const aRest: IRestOrm); reintroduce;
    // ITemplateSample methods
    function CreateSample(const name, description: RawUtf8): TSampleID;
    function GetSample(id: TSampleID): TSampleDTO;
    function GetAllSamples: TSampleDTOs;
    function UpdateSample(id: TSampleID; const name, description: RawUtf8): boolean;
    function DeleteSample(id: TSampleID): boolean;
  end;


implementation


{ TTemplateSample }

constructor TTemplateSample.Create(const aRest: IRestOrm);
begin
  fRest := aRest;
  // Map between ORM entity and DTO
  fSampleMap.Init(TypeInfo(TOrmSample), TypeInfo(TSampleDTO)).
    Map(['id', 'id',
         'name', 'name',
         'description', 'desc',
         'createdat', 'created']);
end;

function TTemplateSample.CreateSample(const name, description: RawUtf8): TSampleID;
var
  sample: TOrmSample;
begin
  result := 0;
  if not Assigned(fRest) then
    exit;
  sample := TOrmSample.Create;
  try
    sample.Name := name;
    sample.Description := description;
    sample.CreatedAt := UnixMSTimeUtcFast;
    if not sample.HasAllNeededFields then
      exit;
    result := fRest.Add(sample, {sendData=}true);
  finally
    sample.Free;
  end;
end;

function TTemplateSample.GetSample(id: TSampleID): TSampleDTO;
var
  sample: TOrmSample;
begin
  FillCharFast(result, SizeOf(result), 0);
  if not Assigned(fRest) then
    exit;
  sample := TOrmSample.Create(fRest, id);
  try
    if sample.ID <> 0 then
      fSampleMap.ToRecord(sample, result);
  finally
    sample.Free;
  end;
end;

function TTemplateSample.GetAllSamples: TSampleDTOs;
var
  samples: TOrmSampleObjArray;
begin
  result := nil;
  if not Assigned(fRest) then
    exit;
  try
    fRest.RetrieveListObjArray(samples, TOrmSample, '', []);
    fSampleMap.ToArrayB(samples, result);
  finally
    ObjArrayClear(samples);
  end;
end;

function TTemplateSample.UpdateSample(id: TSampleID;
  const name, description: RawUtf8): boolean;
var
  sample: TOrmSample;
begin
  result := false;
  if not Assigned(fRest) then
    exit;
  sample := TOrmSample.Create(fRest, id);
  try
    if sample.ID = 0 then
      exit; // not found
    sample.Name := name;
    sample.Description := description;
    if not sample.HasAllNeededFields then
      exit;
    result := fRest.Update(sample);
  finally
    sample.Free;
  end;
end;

function TTemplateSample.DeleteSample(id: TSampleID): boolean;
begin
  result := false;
  if not Assigned(fRest) then
    exit;
  result := fRest.Delete(TOrmSample, id);
end;


end.
