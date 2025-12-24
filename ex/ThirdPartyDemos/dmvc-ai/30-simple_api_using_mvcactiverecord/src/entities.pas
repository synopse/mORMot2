unit entities;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.orm.core;

type
  /// Simple Customer entity
  // Maps to DMVC's TCustomer (TMVCActiveRecord)
  TCustomerOrm = class(TOrm)
  private
    fCode: RawUtf8;
    fCompanyName: RawUtf8;
    fCity: RawUtf8;
    fRating: Integer;
    fNote: RawUtf8;
  published
    property Code: RawUtf8 read fCode write fCode;
    property CompanyName: RawUtf8 read fCompanyName write fCompanyName;
    property City: RawUtf8 read fCity write fCity;
    property Rating: Integer read fRating write fRating;
    property Note: RawUtf8 read fNote write fNote;
  end;

  /// Array of TCustomerOrm for bulk operations
  TOrmCustomerObjArray = array of TCustomerOrm;

implementation

end.
