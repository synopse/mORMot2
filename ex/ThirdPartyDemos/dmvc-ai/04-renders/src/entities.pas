unit entities;

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.orm.core,
  System.SysUtils,
  System.Classes;

type
  // Simple person entity
  TOrmPerson = class(TOrm)
  protected
    fFirstName: RawUtf8;
    fLastName: RawUtf8;
    fDOB: TDateTime;
    fMarried: Boolean;
  published
    property FirstName: RawUtf8 read fFirstName write fFirstName;
    property LastName: RawUtf8 read fLastName write fLastName;
    property DOB: TDateTime read fDOB write fDOB;
    property Married: Boolean read fMarried write fMarried;
  end;

  // Customer entity with more fields
  TOrmCustomer = class(TOrm)
  protected
    fName: RawUtf8;
    fContactFirst: RawUtf8;
    fContactLast: RawUtf8;
    fAddressLine1: RawUtf8;
    fCity: RawUtf8;
  published
    property Name: RawUtf8 read fName write fName;
    property ContactFirst: RawUtf8 read fContactFirst write fContactFirst;
    property ContactLast: RawUtf8 read fContactLast write fContactLast;
    property AddressLine1: RawUtf8 read fAddressLine1 write fAddressLine1;
    property City: RawUtf8 read fCity write fCity;
  end;

  // Dynamic arrays for ORM entities
  TOrmPersonObjArray = array of TOrmPerson;
  TOrmCustomerObjArray = array of TOrmCustomer;

implementation

end.
