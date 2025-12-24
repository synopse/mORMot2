unit api.impl;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.datetime,
  mormot.core.os,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.server,
  api.interfaces;

type
  /// Calculator service implementation
  TCalculator = class(TInjectableObjectRest, ICalculator)
  public
    function Divide(const a, b: Double): Double;
    function GetTimestamp: RawUtf8;
  end;


implementation


{ TCalculator }

function TCalculator.Divide(const a, b: Double): Double;
begin
  if b = 0 then
    raise Exception.Create('Division by zero not allowed');
  result := a / b;
end;

function TCalculator.GetTimestamp: RawUtf8;
begin
  result := DateTimeToIso8601(NowUtc, true);
end;


end.
