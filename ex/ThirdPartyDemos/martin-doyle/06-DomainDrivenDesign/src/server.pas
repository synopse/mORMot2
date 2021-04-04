unit server;

interface

{$I mormot.defines.inc}
uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.unicode,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.memserver,
  DomServiceInterfaces,
  DomServiceImplementation,
  DomRepositoryInterfaces,
  InfraRepositoryImplementation;

type
  TServiceServer = class(TRestServerFullMemory)
  public
    constructor Create(ASampleRepository: ISampleRepository); overload;
  end;

implementation

{
******************************** TServiceServer ********************************
}
constructor TServiceServer.Create(ASampleRepository: ISampleRepository);
var
  Model: TOrmModel;
  ExampleService: TSampleService;
begin
  Model := TOrmModel.Create([]);
  inherited Create(Model, false);
  ExampleService := TSampleService.Create(ASampleRepository);
  ServiceDefine(ExampleService, [ISampleService], EXAMPLE_CONTRACT);
end;

end.
