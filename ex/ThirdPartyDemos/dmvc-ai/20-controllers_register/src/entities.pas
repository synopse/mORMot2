unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.orm.core;

type
  /// Domain entity for Person (simple data class)
  TPerson = class(TSynAutoCreateFields)
  private
    fName: RawUtf8;
    fAge: Integer;
    fCountry: RawUtf8;
  published
    property Name: RawUtf8 read fName write fName;
    property Age: Integer read fAge write fAge;
    property Country: RawUtf8 read fCountry write fCountry;
  end;

implementation

end.
