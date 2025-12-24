unit entities;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.variants,
  mormot.orm.core;

type
  /// Customer entity with various field types
  // Showcases mORMot2 ORM features similar to DMVC ActiveRecord
  TCustomerOrm = class(TOrm)
  private
    fCode: RawUtf8;
    fCompanyName: RawUtf8;
    fCity: RawUtf8;
    fRating: Integer;
    fNote: RawUtf8;
    fLastContact: TDateTime;
    fIsActive: Boolean;
  published
    property Code: RawUtf8 read fCode write fCode;
    property CompanyName: RawUtf8 read fCompanyName write fCompanyName;
    property City: RawUtf8 read fCity write fCity;
    property Rating: Integer read fRating write fRating;
    property Note: RawUtf8 read fNote write fNote;
    property LastContact: TDateTime read fLastContact write fLastContact;
    property IsActive: Boolean read fIsActive write fIsActive;
  end;

  /// Article entity
  TArticleOrm = class(TOrm)
  private
    fCode: RawUtf8;
    fDescription: RawUtf8;
    fPrice: Currency;
    fQuantity: Integer;
  published
    property Code: RawUtf8 read fCode write fCode;
    property Description: RawUtf8 read fDescription write fDescription;
    property Price: Currency read fPrice write fPrice;
    property Quantity: Integer read fQuantity write fQuantity;
  end;

  /// Order entity (demonstrates relationships)
  TOrderOrm = class(TOrm)
  private
    fCustomerID: TID;
    fOrderDate: TDateTime;
    fTotalAmount: Currency;
    fStatus: RawUtf8;
  published
    property CustomerID: TID read fCustomerID write fCustomerID;
    property OrderDate: TDateTime read fOrderDate write fOrderDate;
    property TotalAmount: Currency read fTotalAmount write fTotalAmount;
    property Status: RawUtf8 read fStatus write fStatus;
  end;

  /// Typed ObjArray types for efficient array handling
  TCustomerOrmObjArray = array of TCustomerOrm;
  TArticleOrmObjArray = array of TArticleOrm;
  TOrderOrmObjArray = array of TOrderOrm;

implementation

end.
