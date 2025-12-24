unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.json,
  entities,
  api.interfaces;

type
  /// Implementation of Service 1 (general API info)
  TService1 = class(TInjectableObject, IService1)
  public
    function GetInfo: TPerson;
  end;

  /// Implementation of Service 2 (person data)
  TService2 = class(TInjectableObject, IService2)
  public
    function GetPerson: TPerson;
  end;

implementation

{ TService1 }

function TService1.GetInfo: TPerson;
begin
  // Port of MyController1.Index method
  // Original returned JSON with: application, online, serverdatetime
  // We'll reuse TPerson class to return similar structure
  Result := TPerson.Create;
  Result.Name := 'MyServerName'; // application
  Result.Age := 1; // online (true=1)
  Result.Country := DateTimeToIso8601(Now, True); // serverdatetime
end;

{ TService2 }

function TService2.GetPerson: TPerson;
begin
  // Port of MyController2.GetPerson method
  Result := TPerson.Create;
  Result.Name := 'João Antônio Duarte';
  Result.Age := 26;
  Result.Country := 'Brasil';
end;

end.
