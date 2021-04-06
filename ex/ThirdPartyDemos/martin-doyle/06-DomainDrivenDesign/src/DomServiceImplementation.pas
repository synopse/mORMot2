unit DomServiceImplementation;

interface

{$I mormot.defines.inc}
uses
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


end.
