unit DomServiceImplementation;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.soa.server,
  DomTypes,
  DomServiceInterfaces,
  DomRepositoryInterfaces;

type
  TSampleService = class(TInjectableObjectRest, ISampleService)
  private
    FRepository: ISampleRepository;
  public
    constructor Create(ARepository: ISampleRepository); reintroduce;
    function AddSample(var ASample: TSample): TSampleServiceError;
    function FindSample(var ASample: TSample): TSampleServiceError;
    function ListSamples(out ASamples: TSampleInfoDynArray): TSampleServiceError;
    function DeleteSample(AID: TID): TSampleServiceError;
  end;

implementation

{
******************************** TSampleService ********************************
}
constructor TSampleService.Create(ARepository: ISampleRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;

function TSampleService.AddSample(var ASample: TSample): TSampleServiceError;
begin
  if (ASample.Name = '') or (ASample.Question = '') then
  begin
    Result := sMissingField;
    exit;
  end;
  if FRepository.SaveNewSample(ASample) = srSuccess then
    Result := sSuccess
  else
    Result := sPersistenceError;
end;

function TSampleService.FindSample(var ASample: TSample): TSampleServiceError;
begin
  if FRepository.RetrieveSample(ASample) = srSuccess then
    Result := sSuccess
  else
    Result := sNotFound;
end;

function TSampleService.ListSamples(out ASamples: TSampleInfoDynArray): TSampleServiceError;
begin
  if FRepository.ListSamples(ASamples) = srSuccess then
    Result := sSuccess
  else
    Result := sPersistenceError;
end;

function TSampleService.DeleteSample(AID: TID): TSampleServiceError;
begin
  if FRepository.DeleteSample(AID) = srSuccess then
    Result := sSuccess
  else
    Result := sNotFound;
end;


end.
