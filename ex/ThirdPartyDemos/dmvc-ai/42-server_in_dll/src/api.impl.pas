unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  api.interfaces;

type
  /// Implementation of IMainApi interface
  TMainApi = class(TInjectableObject, IMainApi)
  public
    function GetMessage: RawUtf8;
    function Divide(a, b: Integer): Double;
  end;

implementation

{ TMainApi }

function TMainApi.GetMessage: RawUtf8;
begin
  Result := 'Hello from DelphiMVCFramework REST server hosted in DLL (mORMot2 port)';
end;

function TMainApi.Divide(a, b: Integer): Double;
begin
  if b = 0 then
    raise Exception.Create('Division by zero');
  Result := a / b;
end;

end.
