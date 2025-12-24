unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  StrUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.core.unicode,
  api.interfaces;

type
  /// Implementation of the API service
  TApiService = class(TInjectableObject, IApiService)
  public
    function GetWelcome: RawUtf8;
    function ReverseString(const Value: RawUtf8): RawUtf8;
  end;

implementation

{ TApiService }

function TApiService.GetWelcome: RawUtf8;
begin
  result := 'Welcome to mORMot2 Static Files Sample!';
end;

function TApiService.ReverseString(const Value: RawUtf8): RawUtf8;
begin
  result := StringToUtf8(System.StrUtils.ReverseString(Trim(Utf8ToString(Value))));
end;

end.
