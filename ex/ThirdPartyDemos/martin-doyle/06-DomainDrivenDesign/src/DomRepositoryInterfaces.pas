unit DomRepositoryInterfaces;

interface

{$I mormot.defines.inc}
uses
  mormot.core.interfaces,
  DomTypes;

type
  TSampleRepositoryError = (
    srSuccess, srNotFound, srDuplicatedInfo, srWriteFailure);

type
  ISampleRepository = interface(IInvokable)
    ['{F953DFBC-F69C-4E4C-967B-CDD40EA9DF5F}']
    function RetrieveSample(var ASample: TSample): TSampleRepositoryError;
    function SaveNewSample(var ASample: TSample): TSampleRepositoryError;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(ISampleRepository)
    ]);
end.
