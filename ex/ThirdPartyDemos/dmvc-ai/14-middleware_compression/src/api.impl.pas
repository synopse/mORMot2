unit api.impl;

interface

uses
  mormot.rest.core,
  mormot.core.base,
  mormot.core.json,
  mormot.soa.server,
  api.interfaces;

type
  TCustomersApi = class(TInjectableObjectRest, ICustomersApi)
  public
    function GetCustomers: RawJson;
    function GetTallCustomers: RawJson;
  end;

implementation

uses
  System.Generics.Collections,
  mormot.core.text,
  entities,
  data.generator;

{ TCustomersApi }

function TCustomersApi.GetCustomers: RawJson;
var
  list: TObjectList<TPerson>;
begin
  list := GetPeopleList;
  // Serialize the large list - this should trigger compression
  Result := JsonEncode(['data', ObjectToJson(list, [woHumanReadable])]);
end;

function TCustomersApi.GetTallCustomers: RawJson;
var
  list: TObjectList<TPerson>;
begin
  list := GetPeopleSmallList;
  // Serialize the small list - this should NOT trigger compression (< 1024 bytes)
  Result := JsonEncode(['data', ObjectToJson(list, [woHumanReadable])]);
end;

end.
