unit api.impl;

{$I mormot.defines.inc}

interface

uses
  System.SysUtils,
  mormot.core.base,
  mormot.core.json,
  mormot.core.data,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces;

type
  TJSONWriterSample = class(TInjectableObjectRest, IJSONWriterSample)
  public
    function GetUsers: TUsersListDTO;
  end;

implementation

{ TJSONWriterSample }

function TJSONWriterSample.GetUsers: TUsersListDTO;
begin
  // Create sample users array (equivalent to DMVC's inline array)
  SetLength(result.Users, 3);
  result.Users[0].UserName := 'Daniele';
  result.Users[1].UserName := 'Peter';
  result.Users[2].UserName := 'Scott';
end;

end.
