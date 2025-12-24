unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.soa.server,
  api.interfaces,
  entities;

type
  TMyApi = class(TInjectableObject, IMyApi)
  public
    function Index: RawJson;
    function GetPeople: RawJson;
  end;

implementation

{ TMyApi }

function TMyApi.Index: RawJson;
var
  person: TPerson;
begin
  person := TPerson.Create;
  try
    person.FirstName := 'Daniele';
    person.LastName := 'Teti';
    person.Age := 38;
    result := ObjectToJson(person, []);
  finally
    person.Free;
  end;
end;

function TMyApi.GetPeople: RawJson;
var
  peopleList: TDocVariantData;
  person: variant;
begin
  peopleList.InitFast;

  TDocVariant.NewFast(person);
  TDocVariantData(person).AddNameValuesToObject([
    'FirstName', 'Daniele',
    'LastName', 'Teti',
    'Age', 38
  ]);
  peopleList.AddItem(person);

  TDocVariant.NewFast(person);
  TDocVariantData(person).AddNameValuesToObject([
    'FirstName', 'John',
    'LastName', 'Doe',
    'Age', 35
  ]);
  peopleList.AddItem(person);

  TDocVariant.NewFast(person);
  TDocVariantData(person).AddNameValuesToObject([
    'FirstName', 'Jane',
    'LastName', 'Doe',
    'Age', 32
  ]);
  peopleList.AddItem(person);

  TDocVariant.NewFast(person);
  TDocVariantData(person).AddNameValuesToObject([
    'FirstName', 'Bruce',
    'LastName', 'Banner',
    'Age', 60
  ]);
  peopleList.AddItem(person);

  result := peopleList.ToJson;
end;

end.
