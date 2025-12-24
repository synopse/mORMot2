unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.orm.core;

type
  /// Sample ORM entity for ISAPI demonstration
  TOrmSample = class(TOrm)
  private
    fName: RawUtf8;
    fValue: Integer;
    fCreatedAt: TDateTime;
  published
    property Name: RawUtf8 read fName write fName;
    property Value: Integer read fValue write fValue;
    property CreatedAt: TDateTime read fCreatedAt write fCreatedAt;
  end;


implementation


end.
