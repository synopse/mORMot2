unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.interfaces,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.data,
  mormot.core.text,
  mormot.orm.base,
  mormot.orm.core,
  mormot.soa.server,
  entities,
  api.interfaces;

type
  /// ActiveRecord Showcase implementation
  TActiveRecordShowcaseService = class(TInjectableObjectRest, IActiveRecordShowcaseApi)
  private
    function OrmToVariant(orm: TOrm): Variant;
  public
    // Basic CRUD
    function CreateCustomer(const code, companyName, city, note: RawUtf8;
      rating: Integer): TID;
    function GetCustomer(id: TID): Variant;
    function GetAllCustomers: TVariantDynArray;
    function UpdateCustomer(id: TID; const code, companyName, city,
      note: RawUtf8; rating: Integer): Boolean;
    function DeleteCustomer(id: TID): Boolean;

    // Filtering and searching
    function GetCustomersByCity(const city: RawUtf8): TVariantDynArray;
    function GetCustomersByRating(minRating: Integer): TVariantDynArray;
    function GetActiveCustomers: TVariantDynArray;

    // Aggregation
    function GetCustomerCount: Integer;
    function GetAverageRating: Double;
    function GetCustomerStats: Variant;

    // Batch operations
    function BulkCreateCustomers(const customers: TVariantDynArray): Integer;
    function BulkUpdateRating(minRating, newRating: Integer): Integer;

    // Articles
    function CreateArticle(const code, description: RawUtf8;
      price: Currency; quantity: Integer): TID;
    function GetArticle(id: TID): Variant;
    function GetLowStockArticles(threshold: Integer): TVariantDynArray;

    // Orders
    function CreateOrder(customerID: TID; totalAmount: Currency;
      const status: RawUtf8): TID;
    function GetOrdersByCustomer(customerID: TID): TVariantDynArray;
    function GetOrderWithCustomer(orderID: TID): Variant;
  end;

implementation

{ TActiveRecordShowcaseService }

function TActiveRecordShowcaseService.OrmToVariant(orm: TOrm): Variant;
var
  json: RawUtf8;
begin
  if orm = nil then
    Exit(Null);
  json := orm.GetJsonValues(true, true, ooSelect);
  Result := _JsonFast(json);
end;

// Basic CRUD

function TActiveRecordShowcaseService.CreateCustomer(const code, companyName,
  city, note: RawUtf8; rating: Integer): TID;
var
  customer: TCustomerOrm;
begin
  customer := TCustomerOrm.Create;
  try
    customer.Code := code;
    customer.CompanyName := companyName;
    customer.City := city;
    customer.Rating := rating;
    customer.Note := note;
    customer.LastContact := Now;
    customer.IsActive := true;
    Result := Server.Orm.Add(customer, true);
  finally
    customer.Free;
  end;
end;

function TActiveRecordShowcaseService.GetCustomer(id: TID): Variant;
var
  customer: TCustomerOrm;
begin
  customer := TCustomerOrm.Create(Server.Orm, id);
  try
    if customer.ID = 0 then
      Result := Null
    else
      Result := OrmToVariant(customer);
  finally
    customer.Free;
  end;
end;

function TActiveRecordShowcaseService.GetAllCustomers: TVariantDynArray;
var
  customers: TCustomerOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(customers, TCustomerOrm, '', []) then
  try
    SetLength(Result, length(customers));
    for i := 0 to High(customers) do
      Result[i] := OrmToVariant(customers[i]);
  finally
    ObjArrayClear(customers);
  end;
end;

function TActiveRecordShowcaseService.UpdateCustomer(id: TID; const code,
  companyName, city, note: RawUtf8; rating: Integer): Boolean;
var
  customer: TCustomerOrm;
begin
  customer := TCustomerOrm.Create(Server.Orm, id);
  try
    if customer.ID = 0 then
      Exit(false);
    customer.Code := code;
    customer.CompanyName := companyName;
    customer.City := city;
    customer.Rating := rating;
    customer.Note := note;
    customer.LastContact := Now;
    Result := Server.Orm.Update(customer);
  finally
    customer.Free;
  end;
end;

function TActiveRecordShowcaseService.DeleteCustomer(id: TID): Boolean;
begin
  Result := Server.Orm.Delete(TCustomerOrm, id);
end;

// Filtering and searching

function TActiveRecordShowcaseService.GetCustomersByCity(
  const city: RawUtf8): TVariantDynArray;
var
  customers: TCustomerOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(customers, TCustomerOrm,
    'City=?', [city]) then
  try
    SetLength(Result, length(customers));
    for i := 0 to High(customers) do
      Result[i] := OrmToVariant(customers[i]);
  finally
    ObjArrayClear(customers);
  end;
end;

function TActiveRecordShowcaseService.GetCustomersByRating(
  minRating: Integer): TVariantDynArray;
var
  customers: TCustomerOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(customers, TCustomerOrm,
    'Rating>=?', [minRating]) then
  try
    SetLength(Result, length(customers));
    for i := 0 to High(customers) do
      Result[i] := OrmToVariant(customers[i]);
  finally
    ObjArrayClear(customers);
  end;
end;

function TActiveRecordShowcaseService.GetActiveCustomers: TVariantDynArray;
var
  customers: TCustomerOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(customers, TCustomerOrm,
    'IsActive=?', [true]) then
  try
    SetLength(Result, length(customers));
    for i := 0 to High(customers) do
      Result[i] := OrmToVariant(customers[i]);
  finally
    ObjArrayClear(customers);
  end;
end;

// Aggregation

function TActiveRecordShowcaseService.GetCustomerCount: Integer;
begin
  Result := Server.Orm.TableRowCount(TCustomerOrm);
end;

function TActiveRecordShowcaseService.GetAverageRating: Double;
var
  json: RawUtf8;
  doc: variant;
begin
  Result := 0;
  json := Server.Orm.ExecuteJson([],
    'SELECT AVG(Rating) as AvgRating FROM CustomerOrm', false, nil);
  if json <> '[]' then
  begin
    doc := _JsonFast(json);
    if DocVariantType.IsOfType(doc) and (DocVariantData(doc)^.Count > 0) then
      Result := DocVariantData(doc)^.Values[0].AvgRating;
  end;
end;

function TActiveRecordShowcaseService.GetCustomerStats: Variant;
var
  json: RawUtf8;
  doc: variant;
begin
  json := Server.Orm.ExecuteJson([],
    'SELECT COUNT(*) as TotalCustomers, ' +
    'AVG(Rating) as AverageRating, ' +
    'MIN(Rating) as MinRating, ' +
    'MAX(Rating) as MaxRating, ' +
    'SUM(CASE WHEN IsActive=1 THEN 1 ELSE 0 END) as ActiveCustomers ' +
    'FROM CustomerOrm', false, nil);
  if json <> '[]' then
  begin
    doc := _JsonFast(json);
    if DocVariantType.IsOfType(doc) and (DocVariantData(doc)^.Count > 0) then
      Result := DocVariantData(doc)^.Values[0]
    else
      Result := Null;
  end
  else
    Result := Null;
end;

// Batch operations

function TActiveRecordShowcaseService.BulkCreateCustomers(
  const customers: TVariantDynArray): Integer;
var
  i: Integer;
  customer: TCustomerOrm;
  v: PVariant;
begin
  Result := 0;
  if Server.Orm.TransactionBegin(TCustomerOrm) then
  try
    for i := 0 to High(customers) do
    begin
      v := @customers[i];
      customer := TCustomerOrm.Create;
      try
        customer.Code := VariantToUtf8(DocVariantData(v^)^.U['code']);
        customer.CompanyName := VariantToUtf8(DocVariantData(v^)^.U['companyName']);
        customer.City := VariantToUtf8(DocVariantData(v^)^.U['city']);
        customer.Rating := DocVariantData(v^)^.I['rating'];
        customer.Note := VariantToUtf8(DocVariantData(v^)^.U['note']);
        customer.LastContact := Now;
        customer.IsActive := true;
        if Server.Orm.Add(customer, true) > 0 then
          Inc(Result);
      finally
        customer.Free;
      end;
    end;
    Server.Orm.Commit;
  except
    Server.Orm.RollBack;
    raise;
  end;
end;

function TActiveRecordShowcaseService.BulkUpdateRating(minRating,
  newRating: Integer): Integer;
var
  json: RawUtf8;
  doc: variant;
begin
  Result := 0;
  Server.Orm.ExecuteFmt(
    'UPDATE CustomerOrm SET Rating=% WHERE Rating>=%', [], [newRating, minRating]);

  json := Server.Orm.ExecuteJson([],
    'SELECT changes() as Changed', false, nil);
  if json <> '[]' then
  begin
    doc := _JsonFast(json);
    if DocVariantType.IsOfType(doc) and (DocVariantData(doc)^.Count > 0) then
      Result := DocVariantData(doc)^.Values[0].Changed;
  end;
end;

// Articles

function TActiveRecordShowcaseService.CreateArticle(const code,
  description: RawUtf8; price: Currency; quantity: Integer): TID;
var
  article: TArticleOrm;
begin
  article := TArticleOrm.Create;
  try
    article.Code := code;
    article.Description := description;
    article.Price := price;
    article.Quantity := quantity;
    Result := Server.Orm.Add(article, true);
  finally
    article.Free;
  end;
end;

function TActiveRecordShowcaseService.GetArticle(id: TID): Variant;
var
  article: TArticleOrm;
begin
  article := TArticleOrm.Create(Server.Orm, id);
  try
    if article.ID = 0 then
      Result := Null
    else
      Result := OrmToVariant(article);
  finally
    article.Free;
  end;
end;

function TActiveRecordShowcaseService.GetLowStockArticles(
  threshold: Integer): TVariantDynArray;
var
  articles: TArticleOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(articles, TArticleOrm,
    'Quantity<?', [threshold]) then
  try
    SetLength(Result, length(articles));
    for i := 0 to High(articles) do
      Result[i] := OrmToVariant(articles[i]);
  finally
    ObjArrayClear(articles);
  end;
end;

// Orders (demonstrates relationships)

function TActiveRecordShowcaseService.CreateOrder(customerID: TID;
  totalAmount: Currency; const status: RawUtf8): TID;
var
  order: TOrderOrm;
begin
  order := TOrderOrm.Create;
  try
    order.CustomerID := customerID;
    order.OrderDate := Now;
    order.TotalAmount := totalAmount;
    order.Status := status;
    Result := Server.Orm.Add(order, true);
  finally
    order.Free;
  end;
end;

function TActiveRecordShowcaseService.GetOrdersByCustomer(
  customerID: TID): TVariantDynArray;
var
  orders: TOrderOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(orders, TOrderOrm,
    'CustomerID=?', [customerID]) then
  try
    SetLength(Result, length(orders));
    for i := 0 to High(orders) do
      Result[i] := OrmToVariant(orders[i]);
  finally
    ObjArrayClear(orders);
  end;
end;

function TActiveRecordShowcaseService.GetOrderWithCustomer(
  orderID: TID): Variant;
var
  json: RawUtf8;
  doc: variant;
  sql: RawUtf8;
begin
  // Demonstrate SQL JOIN to get order with customer details
  FormatUtf8('SELECT o.RowID as OrderID, o.OrderDate, o.TotalAmount, o.Status, ' +
    'c.Code as CustomerCode, c.CompanyName, c.City ' +
    'FROM OrderOrm o ' +
    'INNER JOIN CustomerOrm c ON o.CustomerID = c.RowID ' +
    'WHERE o.RowID=%', [orderID], sql);

  json := Server.Orm.ExecuteJson([], sql, false, nil);

  if json <> '[]' then
  begin
    doc := _JsonFast(json);
    if DocVariantType.IsOfType(doc) and (DocVariantData(doc)^.Count > 0) then
      Result := DocVariantData(doc)^.Values[0]
    else
      Result := Null;
  end
  else
    Result := Null;
end;

end.
