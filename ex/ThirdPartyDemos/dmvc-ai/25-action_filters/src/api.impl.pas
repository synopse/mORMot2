unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.core.datetime,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces,
  BusinessObjectsU;

type
  /// Implementation of IActionFiltersApi
  // Port of DMVC TActionFiltersController
  TActionFiltersApiService = class(TInjectableObjectRest, IActionFiltersApi)
  public
    /// Get person by ID
    // Port of TActionFiltersController.GetPerson
    function GetPerson(const id: RawUtf8): RawUtf8;
  end;

implementation

{ TActionFiltersApiService }

function TActionFiltersApiService.GetPerson(const id: RawUtf8): RawUtf8;
var
  P: TPerson;
begin
  {
    Use ID to load the person from a database...
    In this example, we're creating a fake person
  }
  TSynLog.Add.Log(sllInfo, 'GetPerson called with id=%', [id], self);

  P := TPerson.Create;
  try
    P.FirstName := 'Daniele';
    P.LastName := 'Teti';
    P.DOB := Iso8601ToDateTime('1975-05-02');
    P.Married := True;

    // Serialize to JSON
    Result := ObjectToJson(P, [woHumanReadable]);

    TSynLog.Add.Log(sllDebug, 'GetPerson returning JSON: %', [Result], self);
  finally
    P.Free;
  end;
end;

end.
