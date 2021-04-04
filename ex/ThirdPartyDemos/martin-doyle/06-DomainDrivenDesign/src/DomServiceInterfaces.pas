unit DomServiceInterfaces;

interface

{$I mormot.defines.inc}
uses
  mormot.core.interfaces,
  DomTypes;

const
    EXAMPLE_CONTRACT = 'SampleService';

type
  TSampleServiceError = (
    sSuccess, sNotFound, sMissingField, sPersistenceError);

type
  ISampleService = interface(IInvokable)
    ['{B8DE093D-A027-4C61-A67B-638FB0F23242}']
    function AddSample(var ASample: TSample): TSampleServiceError;
    function FindSample(var ASample: TSample): TSampleServiceError;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(ISampleService)
    ]);
end.
