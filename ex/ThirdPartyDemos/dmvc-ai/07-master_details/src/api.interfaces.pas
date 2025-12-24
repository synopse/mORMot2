unit api.interfaces;

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.rest.core;

type
  // DTOs for data transfer

  // Order item DTO
  TOrderItemDTO = packed record
    id: TID;
    orderid: TID;  // Not serialized when nested
    articleid: TID;
    unitprice: Currency;
    discount: Integer;
    quantity: Integer;
    description: RawUtf8;
    total: Currency;
  end;

  TOrderItemDTODynArray = array of TOrderItemDTO;

  // Order DTO with nested items
  TOrderDTO = packed record
    id: TID;
    customerid: Integer;
    orderdate: TDateTime;
    total: Currency;
    items: TOrderItemDTODynArray;  // Nested order items
  end;

  TOrderDTODynArray = array of TOrderDTO;

  // Order input DTO (for create/update without items)
  TOrderInputDTO = packed record
    customerid: Integer;
    orderdate: TDateTime;
  end;

  // Order item input DTO (for adding items to order)
  TOrderItemInputDTO = packed record
    articleid: TID;
    unitprice: Currency;
    discount: Integer;
    quantity: Integer;
    description: RawUtf8;
  end;

  // Article DTO
  TArticleDTO = packed record
    id: TID;
    description: RawUtf8;
    price: Currency;
  end;

  TArticleDTODynArray = array of TArticleDTO;

  // Main API interface for orders management
  IOrdersAPI = interface(IInvokable)
    ['{B7C8D9E0-F1A2-4B3C-5D6E-7F8A9B0C1D2E}']

    // Orders CRUD
    function GetOrders: TOrderDTODynArray;
    function GetOrderByID(id: TID): TOrderDTO;
    function CreateOrder(const order: TOrderInputDTO): TID;
    function UpdateOrder(id: TID; const order: TOrderInputDTO): Boolean;
    function DeleteOrder(id: TID): Boolean;

    // Order items management
    function AddItemToOrder(orderid: TID; const item: TOrderItemInputDTO): TID;
    function UpdateOrderItem(id: TID; const item: TOrderItemInputDTO): Boolean;
    function RemoveItemFromOrder(orderid: TID; itemid: TID): Boolean;

    // Articles management (for product catalog)
    function GetArticles: TArticleDTODynArray;
    function GetArticleByID(id: TID): TArticleDTO;

    // Search
    function GetOrdersByTotalGreaterThan(total: Currency): TOrderDTODynArray;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IOrdersAPI)
  ]);

end.
