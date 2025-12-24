unit entities;

{$I mormot.defines.inc}

interface

uses
  System.Classes,
  mormot.core.base,
  mormot.core.json;

type
  TPerson = class(TPersistent)
  private
    FFirstName: string;
    FLastName: string;
    FAge: Integer;
  published
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property Age: Integer read FAge write FAge;
  end;

implementation

end.
