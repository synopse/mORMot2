unit entities;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.text,
  mormot.orm.core;

type
  /// ORM entity for Customer records
  // Demonstrates mORMot2 ORM capabilities similar to DMVC ActiveRecord
  TOrmCustomer = class(TOrm)
  private
    fCode: RawUtf8;
    fCompanyName: RawUtf8;
    fCity: RawUtf8;
    fLastContact: TDateTime;
    fRating: integer;
    fNote: RawUtf8;
  published
    property Code: RawUtf8 read fCode write fCode;
    property CompanyName: RawUtf8 read fCompanyName write fCompanyName;
    property City: RawUtf8 read fCity write fCity;
    property LastContact: TDateTime read fLastContact write fLastContact;
    property Rating: integer read fRating write fRating;
    property Note: RawUtf8 read fNote write fNote;
  end;

  /// ORM entity for Order records
  // Demonstrates relationships and foreign keys
  TOrmOrder = class(TOrm)
  private
    fCustomerID: TID;
    fOrderDate: TDateTime;
    fOrderNumber: RawUtf8;
    fAmount: currency;
    fStatus: RawUtf8;
  published
    property CustomerID: TID read fCustomerID write fCustomerID;
    property OrderDate: TDateTime read fOrderDate write fOrderDate;
    property OrderNumber: RawUtf8 read fOrderNumber write fOrderNumber;
    property Amount: currency read fAmount write fAmount;
    property Status: RawUtf8 read fStatus write fStatus;
  end;

  /// Record structure for bulk customer data
  // Used for high-performance batch operations
  TCustomerData = packed record
    Code: RawUtf8;
    CompanyName: RawUtf8;
    City: RawUtf8;
    LastContact: TDateTime;
    Rating: integer;
    Note: RawUtf8;
  end;
  TCustomerDataArray = array of TCustomerData;

  /// Record structure for bulk order data
  TOrderData = packed record
    CustomerID: Int64;
    OrderDate: TDateTime;
    OrderNumber: RawUtf8;
    Amount: double;
    Status: RawUtf8;
  end;
  TOrderDataArray = array of TOrderData;

implementation

end.
