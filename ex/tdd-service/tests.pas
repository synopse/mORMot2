unit tests;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.test,
  dom.entities,
  dom.infra;


type
  TAuditTrailTests = class(TSynTestsLogged)
  published
    procedure Domain;
    procedure MobileApi;
  end;

  TAuditTrailDomainTests = class(TSynTestCase)
  published
    // some unit tests for Domain entities
    procedure Entities;
  end;

  TAuditTrailMobileApiTests = class(TSynTestCase)
  published
    procedure AddEvent;
  end;


implementation


{ TAuditTrailTests }

procedure TAuditTrailTests.Domain;
begin
  AddCase([TAuditTrailDomainTests]);
end;

procedure TAuditTrailTests.MobileApi;
begin
  AddCase([TAuditTrailMobileApiTests]);
end;


{ TAuditTrailDomainTests }

procedure TAuditTrailDomainTests.Entities;
var
  e: TDomEvent;
begin
  e := TDomEvent.Create;
  try
    e.Normalize;
    Check(not e.HasAllNeededFields);
    e.Source := 1;
    Check(not e.HasAllNeededFields);
    e.Description := 'toto';
    e.Context := 123456;
    Check(e.HasAllNeededFields);
    e.Description := ' ';
    Check(not e.HasAllNeededFields);
    e.Description := 'titi';
    Check(e.HasAllNeededFields);
  finally
    e.Free;
  end;
end;


{ TAuditTrailMobileApiTests }

procedure TAuditTrailMobileApiTests.AddEvent;
begin

end;

end.
