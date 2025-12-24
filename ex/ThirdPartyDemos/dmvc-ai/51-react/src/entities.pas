unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.text,
  mormot.orm.core;

type
  /// Customer entity
  // Port of DMVC TCustomer (Model.Customer.pas) to mORMot2 ORM
  // Maps to customers table with fields: ID, CODE, DESCRIPTION, CITY, NOTE, RATING
  TOrmCustomer = class(TOrm)
  private
    fCode: RawUtf8;
    fDescription: RawUtf8;
    fCity: RawUtf8;
    fNote: RawUtf8;
    fRating: Integer;
  published
    /// Customer code (max length 15)
    property Code: RawUtf8 index 15 read fCode write fCode;
    /// Customer description (max length 50, required)
    property Description: RawUtf8 index 50 read fDescription write fDescription;
    /// Customer city
    property City: RawUtf8 index 50 read fCity write fCity;
    /// Customer note
    property Note: RawUtf8 index 255 read fNote write fNote;
    /// Customer rating (0-5)
    property Rating: Integer read fRating write fRating;
  end;

  /// Array of TOrmCustomer objects
  TOrmCustomers = array of TOrmCustomer;

  /// Customer DTO for API responses
  TCustomerDto = packed record
    ID: TID;
    Code: RawUtf8;
    Description: RawUtf8;
    City: RawUtf8;
    Note: RawUtf8;
    Rating: Integer;
  end;

  /// Array of customer DTOs
  TCustomerDtos = array of TCustomerDto;


implementation


end.
