unit entities;

interface

uses
  mormot.core.base,
  mormot.orm.core;

type
  // Order item (detail) - represents a line in an order
  TOrmOrderItem = class(TOrm)
  private
    fOrderID: TID;
    fArticleID: TID;
    fUnitPrice: Currency;
    fDiscount: Integer;
    fQuantity: Integer;
    fDescription: RawUtf8;
    fTotal: Currency;
  public
    procedure ComputeTotal;
  published
    property OrderID: TID read fOrderID write fOrderID;
    property ArticleID: TID read fArticleID write fArticleID;
    property UnitPrice: Currency read fUnitPrice write fUnitPrice;
    property Discount: Integer read fDiscount write fDiscount;
    property Quantity: Integer read fQuantity write fQuantity;
    property Description: RawUtf8 read fDescription write fDescription;
    property Total: Currency read fTotal write fTotal;
  end;

  // Order (master) - represents an order header
  TOrmOrder = class(TOrm)
  private
    fCustomerID: Integer;
    fOrderDate: TDateTime;
    fTotal: Currency;
  published
    property CustomerID: Integer read fCustomerID write fCustomerID;
    property OrderDate: TDateTime read fOrderDate write fOrderDate;
    property Total: Currency read fTotal write fTotal;
  end;

  // Article - product catalog
  TOrmArticle = class(TOrm)
  private
    fDescription: RawUtf8;
    fPrice: Currency;
  published
    property Description: RawUtf8 read fDescription write fDescription;
    property Price: Currency read fPrice write fPrice;
  end;

  // Dynamic arrays for ORM entities
  TOrmOrderItemObjArray = array of TOrmOrderItem;
  TOrmOrderObjArray = array of TOrmOrder;
  TOrmArticleObjArray = array of TOrmArticle;

implementation

{ TOrmOrderItem }

procedure TOrmOrderItem.ComputeTotal;
begin
  fTotal := fUnitPrice * fQuantity * (1 - fDiscount / 100);
end;

end.
