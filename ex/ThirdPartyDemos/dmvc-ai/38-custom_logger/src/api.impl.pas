unit api.impl;

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.core.interfaces,
  custom.logger,
  api.interfaces;

type
  TApiService = class(TInjectableObject, IApiService)
  public
    function Index: RawUtf8;
    function Error: RawUtf8;
  end;

implementation

{ TApiService }

function TApiService.Index: RawUtf8;
begin
  TCustomSynLog.Add.Log(sllInfo, 'Index method called');
  Result := '{"message":"Hello from custom logger sample!"}';
end;

function TApiService.Error: RawUtf8;
begin
  TCustomSynLog.Add.Log(sllError, 'Error method called - simulating error');
  raise Exception.Create('Simulated error for logging demonstration');
end;

end.
