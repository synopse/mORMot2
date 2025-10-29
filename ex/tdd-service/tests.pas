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
begin

end;


{ TAuditTrailMobileApiTests }

procedure TAuditTrailMobileApiTests.AddEvent;
begin

end;

end.
