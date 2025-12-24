unit api.impl;

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.json,
  mormot.core.data,
  mormot.core.rtti,
  mormot.rest.core,
  mormot.orm.core,
  api.interfaces,
  entities,
  System.SysUtils,
  System.Classes;

type
  TOrdersAPI = class(TInterfacedObject, IOrdersAPI)
  private
    fRest: IRestOrm;

    // Helper methods
    function OrmOrderToDTO(const aOrder: TOrmOrder): TOrderDTO;
    function OrmOrderItemToDTO(const aItem: TOrmOrderItem): TOrderItemDTO;
    function LoadOrderItems(orderid: TID): TOrderItemDTODynArray;
    procedure RecalculateOrderTotal(orderid: TID);
    function CreateSampleArticle(const aDescription: RawUtf8;
      const aPrice: Currency): TID;
  public
    constructor Create(const aRest: IRestOrm); reintroduce;

    // IOrdersAPI implementation
    function GetOrders: TOrderDTODynArray;
    function GetOrderByID(id: TID): TOrderDTO;
    function CreateOrder(const order: TOrderInputDTO): TID;
    function UpdateOrder(id: TID; const order: TOrderInputDTO): Boolean;
    function DeleteOrder(id: TID): Boolean;

    function AddItemToOrder(orderid: TID; const item: TOrderItemInputDTO): TID;
    function UpdateOrderItem(id: TID; const item: TOrderItemInputDTO): Boolean;
    function RemoveItemFromOrder(orderid: TID; itemid: TID): Boolean;

    function GetArticles: TArticleDTODynArray;
    function GetArticleByID(id: TID): TArticleDTO;

    function GetOrdersByTotalGreaterThan(total: Currency): TOrderDTODynArray;
  end;

implementation

{ TOrdersAPI }

constructor TOrdersAPI.Create(const aRest: IRestOrm);
begin
  inherited Create;
  fRest := aRest;

  // Create sample articles if database is empty
  if fRest.TableRowCount(TOrmArticle) = 0 then
  begin
    CreateSampleArticle('Widget A', 10.50);
    CreateSampleArticle('Widget B', 25.00);
    CreateSampleArticle('Widget C', 5.75);
    CreateSampleArticle('Premium Widget', 99.99);
  end;
end;

function TOrdersAPI.CreateSampleArticle(const aDescription: RawUtf8;
  const aPrice: Currency): TID;
var
  article: TOrmArticle;
begin
  article := TOrmArticle.Create;
  try
    article.Description := aDescription;
    article.Price := aPrice;
    Result := fRest.Add(article, True);
  finally
    article.Free;
  end;
end;

function TOrdersAPI.OrmOrderToDTO(const aOrder: TOrmOrder): TOrderDTO;
begin
  Result.id := aOrder.ID;
  Result.customerid := aOrder.CustomerID;
  Result.orderdate := aOrder.OrderDate;
  Result.total := aOrder.Total;
  // Items will be loaded separately
end;

function TOrdersAPI.OrmOrderItemToDTO(const aItem: TOrmOrderItem): TOrderItemDTO;
begin
  Result.id := aItem.ID;
  Result.orderid := aItem.OrderID;
  Result.articleid := aItem.ArticleID;
  Result.unitprice := aItem.UnitPrice;
  Result.discount := aItem.Discount;
  Result.quantity := aItem.Quantity;
  Result.description := aItem.Description;
  Result.total := aItem.Total;
end;

function TOrdersAPI.LoadOrderItems(orderid: TID): TOrderItemDTODynArray;
var
  items: TOrmOrderItemObjArray;
  i: PtrInt;
begin
  if fRest.RetrieveListObjArray(items, TOrmOrderItem,
    'OrderID=?', [orderid]) then
  try
    SetLength(Result, Length(items));
    for i := 0 to High(items) do
      Result[i] := OrmOrderItemToDTO(items[i]);
  finally
    ObjArrayClear(items);
  end;
end;

function TOrdersAPI.GetOrders: TOrderDTODynArray;
var
  orders: TOrmOrderObjArray;
  i: PtrInt;
begin
  if fRest.RetrieveListObjArray(orders, TOrmOrder, '', []) then
  try
    SetLength(Result, Length(orders));
    for i := 0 to High(orders) do
    begin
      Result[i] := OrmOrderToDTO(orders[i]);
      Result[i].items := LoadOrderItems(orders[i].ID);
    end;
  finally
    ObjArrayClear(orders);
  end;
end;

function TOrdersAPI.GetOrderByID(id: TID): TOrderDTO;
var
  order: TOrmOrder;
begin
  order := TOrmOrder.Create(fRest, id);
  try
    if order.ID = 0 then
      raise ERestException.CreateUtf8('Order with ID % not found', [id]);

    Result := OrmOrderToDTO(order);
    Result.items := LoadOrderItems(order.ID);
  finally
    order.Free;
  end;
end;

function TOrdersAPI.CreateOrder(const order: TOrderInputDTO): TID;
var
  newOrder: TOrmOrder;
begin
  newOrder := TOrmOrder.Create;
  try
    newOrder.CustomerID := order.customerid;
    newOrder.OrderDate := order.orderdate;
    newOrder.Total := 0;  // Will be calculated when items are added
    Result := fRest.Add(newOrder, True);
  finally
    newOrder.Free;
  end;
end;

function TOrdersAPI.UpdateOrder(id: TID; const order: TOrderInputDTO): Boolean;
var
  existingOrder: TOrmOrder;
begin
  existingOrder := TOrmOrder.Create(fRest, id);
  try
    if existingOrder.ID = 0 then
      raise ERestException.CreateUtf8('Order with ID % not found', [id]);

    existingOrder.CustomerID := order.customerid;
    existingOrder.OrderDate := order.orderdate;
    Result := fRest.Update(existingOrder);
  finally
    existingOrder.Free;
  end;
end;

function TOrdersAPI.DeleteOrder(id: TID): Boolean;
var
  items: TOrmOrderItemObjArray;
  i: PtrInt;
begin
  // First delete all order items (cascade delete)
  if fRest.RetrieveListObjArray(items, TOrmOrderItem,
    'OrderID=?', [id]) then
  try
    for i := 0 to High(items) do
      fRest.Delete(TOrmOrderItem, items[i].ID);
  finally
    ObjArrayClear(items);
  end;

  // Then delete the order itself
  Result := fRest.Delete(TOrmOrder, id);
end;

procedure TOrdersAPI.RecalculateOrderTotal(orderid: TID);
var
  order: TOrmOrder;
  items: TOrmOrderItemObjArray;
  total: Currency;
  i: PtrInt;
begin
  order := TOrmOrder.Create(fRest, orderid);
  try
    if order.ID = 0 then
      Exit;

    total := 0;
    if fRest.RetrieveListObjArray(items, TOrmOrderItem,
      'OrderID=?', [orderid]) then
    try
      for i := 0 to High(items) do
        total := total + items[i].Total;
    finally
      ObjArrayClear(items);
    end;

    order.Total := total;
    fRest.Update(order);
  finally
    order.Free;
  end;
end;

function TOrdersAPI.AddItemToOrder(orderid: TID;
  const item: TOrderItemInputDTO): TID;
var
  newItem: TOrmOrderItem;
  order: TOrmOrder;
begin
  // Verify order exists
  order := TOrmOrder.Create(fRest, orderid);
  try
    if order.ID = 0 then
      raise ERestException.CreateUtf8('Order with ID % not found', [orderid]);
  finally
    order.Free;
  end;

  newItem := TOrmOrderItem.Create;
  try
    newItem.OrderID := orderid;
    newItem.ArticleID := item.articleid;
    newItem.UnitPrice := item.unitprice;
    newItem.Discount := item.discount;
    newItem.Quantity := item.quantity;
    newItem.Description := item.description;
    newItem.ComputeTotal;

    Result := fRest.Add(newItem, True);

    // Recalculate order total
    RecalculateOrderTotal(orderid);
  finally
    newItem.Free;
  end;
end;

function TOrdersAPI.UpdateOrderItem(id: TID;
  const item: TOrderItemInputDTO): Boolean;
var
  existingItem: TOrmOrderItem;
begin
  existingItem := TOrmOrderItem.Create(fRest, id);
  try
    if existingItem.ID = 0 then
      raise ERestException.CreateUtf8('Order item with ID % not found', [id]);

    existingItem.ArticleID := item.articleid;
    existingItem.UnitPrice := item.unitprice;
    existingItem.Discount := item.discount;
    existingItem.Quantity := item.quantity;
    existingItem.Description := item.description;
    existingItem.ComputeTotal;

    Result := fRest.Update(existingItem);

    // Recalculate order total
    if Result then
      RecalculateOrderTotal(existingItem.OrderID);
  finally
    existingItem.Free;
  end;
end;

function TOrdersAPI.RemoveItemFromOrder(orderid, itemid: TID): Boolean;
var
  item: TOrmOrderItem;
begin
  item := TOrmOrderItem.Create(fRest, itemid);
  try
    if item.ID = 0 then
      raise ERestException.CreateUtf8('Order item with ID % not found', [itemid]);

    if item.OrderID <> orderid then
      raise ERestException.CreateUtf8(
        'Order item % does not belong to order %', [itemid, orderid]);

    Result := fRest.Delete(TOrmOrderItem, itemid);

    // Recalculate order total
    if Result then
      RecalculateOrderTotal(orderid);
  finally
    item.Free;
  end;
end;

function TOrdersAPI.GetArticles: TArticleDTODynArray;
var
  articles: TOrmArticleObjArray;
  i: PtrInt;
begin
  if fRest.RetrieveListObjArray(articles, TOrmArticle, '', []) then
  try
    SetLength(Result, Length(articles));
    for i := 0 to High(articles) do
    begin
      Result[i].id := articles[i].ID;
      Result[i].description := articles[i].Description;
      Result[i].price := articles[i].Price;
    end;
  finally
    ObjArrayClear(articles);
  end;
end;

function TOrdersAPI.GetArticleByID(id: TID): TArticleDTO;
var
  article: TOrmArticle;
begin
  article := TOrmArticle.Create(fRest, id);
  try
    if article.ID = 0 then
      raise ERestException.CreateUtf8('Article with ID % not found', [id]);

    Result.id := article.ID;
    Result.description := article.Description;
    Result.price := article.Price;
  finally
    article.Free;
  end;
end;

function TOrdersAPI.GetOrdersByTotalGreaterThan(total: Currency): TOrderDTODynArray;
var
  orders: TOrmOrderObjArray;
  i: PtrInt;
begin
  if fRest.RetrieveListObjArray(orders, TOrmOrder,
    'Total>=?', [total]) then
  try
    SetLength(Result, Length(orders));
    for i := 0 to High(orders) do
    begin
      Result[i] := OrmOrderToDTO(orders[i]);
      Result[i].items := LoadOrderItems(orders[i].ID);
    end;
  finally
    ObjArrayClear(orders);
  end;
end;

end.
