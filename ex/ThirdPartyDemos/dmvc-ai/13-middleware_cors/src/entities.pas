unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.orm.core;

type
  /// Sample entity for customer data
  // - Simple customer record to demonstrate CORS-enabled CRUD operations
  TOrmCustomer = class(TOrm)
  protected
    fName: RawUtf8;
    fEmail: RawUtf8;
  published
    property Name: RawUtf8 read fName write fName;
    property Email: RawUtf8 read fEmail write fEmail;
  end;


implementation


end.
