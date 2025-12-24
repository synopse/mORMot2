unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.search,
  mormot.orm.core;

type
  /// Customer entity
  // Port of DMVC Angular sample customer entity to mORMot2 ORM
  TOrmCustomer = class(TOrm)
  private
    fFirstName: RawUtf8;
    fLastName: RawUtf8;
    fEmail: RawUtf8;
    fPhone: RawUtf8;
    fCity: RawUtf8;
    fCountry: RawUtf8;
  published
    /// Customer first name
    property FirstName: RawUtf8 index 50 read fFirstName write fFirstName;
    /// Customer last name
    property LastName: RawUtf8 index 50 read fLastName write fLastName;
    /// Customer email address
    property Email: RawUtf8 index 100 read fEmail write fEmail;
    /// Customer phone number
    property Phone: RawUtf8 index 20 read fPhone write fPhone;
    /// Customer city
    property City: RawUtf8 index 50 read fCity write fCity;
    /// Customer country
    property Country: RawUtf8 index 50 read fCountry write fCountry;
  end;

  /// Array of TOrmCustomer objects
  TOrmCustomers = array of TOrmCustomer;


implementation


initialization
  TOrmCustomer.AddFilterOrValidate('Email', TSynValidateEmail.Create);

end.
