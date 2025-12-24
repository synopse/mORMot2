# mORMot2 Master-Details (Relationships) Sample

**Port of DMVCFramework's master_details sample to mORMot2**

This sample demonstrates how to implement **1-N relationships** (master-detail pattern) in mORMot2, featuring:

- **Master-Detail Relationships**: Orders (master) with OrderItems (details)
- **Cascading Deletes**: Deleting an order automatically removes all its items
- **Nested JSON Responses**: Orders returned with embedded items array
- **Automatic Total Calculation**: Order total recalculated when items change
- **Complete CRUD Operations**: Full create, read, update, delete for both orders and items

## Database Schema

The sample uses three tables:

### Articles (Product Catalog)
```sql
CREATE TABLE Articles (
  ID INTEGER PRIMARY KEY,
  Description TEXT,
  Price REAL
);
```

### Orders (Master)
```sql
CREATE TABLE Orders (
  ID INTEGER PRIMARY KEY,
  CustomerID INTEGER,
  OrderDate DATETIME,
  Total REAL
);
```

### OrderItems (Detail)
```sql
CREATE TABLE OrderItems (
  ID INTEGER PRIMARY KEY,
  OrderID INTEGER,           -- Foreign key to Orders
  ArticleID INTEGER,         -- Reference to Articles
  UnitPrice REAL,
  Discount INTEGER,
  Quantity INTEGER,
  Description TEXT,
  Total REAL
);
```

## Key Features Demonstrated

### 1. Nested JSON Responses

When retrieving an order, the response includes all order items:

```json
{
  "id": 1,
  "customerid": 100,
  "orderdate": "2025-12-19T00:00:00",
  "total": 42.00,
  "items": [
    {
      "id": 1,
      "articleid": 1,
      "unitprice": 10.50,
      "discount": 0,
      "quantity": 2,
      "description": "Widget A",
      "total": 21.00
    },
    {
      "id": 2,
      "articleid": 2,
      "unitprice": 25.00,
      "discount": 10,
      "quantity": 1,
      "description": "Widget B",
      "total": 22.50
    }
  ]
}
```

### 2. Cascading Deletes

Implemented in application code (see `TOrdersAPI.DeleteOrder`):

```pascal
function TOrdersAPI.DeleteOrder(id: TID): Boolean;
begin
  // First delete all order items
  if fRest.RetrieveListObjArray(items, TOrmOrderItem, 'OrderID=?', [id]) then
    for i := 0 to High(items) do
      fRest.Delete(TOrmOrderItem, items[i].ID);

  // Then delete the order itself
  Result := fRest.Delete(TOrmOrder, id);
end;
```

### 3. Automatic Total Recalculation

Order totals are automatically recalculated when:
- Adding a new item to an order
- Updating an existing item
- Removing an item from an order

```pascal
procedure TOrdersAPI.RecalculateOrderTotal(orderid: TID);
var
  total: Currency;
begin
  // Sum all item totals
  for item in items do
    total := total + item.Total;

  // Update order
  order.Total := total;
  fRest.Update(order);
end;
```

### 4. Item Total Calculation with Discount

Each order item calculates its total based on quantity, unit price, and discount:

```pascal
procedure TOrmOrderItem.ComputeTotal;
begin
  fTotal := fUnitPrice * fQuantity * (1 - fDiscount / 100);
end;
```

## API Endpoints

### Articles Management

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/OrdersAPI/GetArticles` | List all articles |
| GET | `/OrdersAPI/GetArticleByID?id=1` | Get single article |

### Orders Management

| Method | Endpoint | Body | Description |
|--------|----------|------|-------------|
| GET | `/OrdersAPI/GetOrders` | - | List all orders with items |
| GET | `/OrdersAPI/GetOrderByID?id=1` | - | Get single order with items |
| POST | `/OrdersAPI/CreateOrder` | `TOrderInputDTO` | Create new order |
| POST | `/OrdersAPI/UpdateOrder?id=1` | `TOrderInputDTO` | Update order header |
| GET | `/OrdersAPI/DeleteOrder?id=1` | - | Delete order (cascades) |

### Order Items Management

| Method | Endpoint | Body | Description |
|--------|----------|------|-------------|
| POST | `/OrdersAPI/AddItemToOrder?orderid=1` | `TOrderItemInputDTO` | Add item to order |
| POST | `/OrdersAPI/UpdateOrderItem?id=1` | `TOrderItemInputDTO` | Update order item |
| GET | `/OrdersAPI/RemoveItemFromOrder?orderid=1&itemid=1` | - | Remove item from order |

### Search

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/OrdersAPI/GetOrdersByTotalGreaterThan?total=50.00` | Find orders by total |

## Request/Response Examples

### Create Order
```bash
curl -X POST http://localhost:8080/OrdersAPI/CreateOrder \
  -H "Content-Type: application/json" \
  -d '{"customerid":100,"orderdate":"2025-12-19T00:00:00"}'
```

Response:
```json
{"result":1}
```

### Add Item to Order
```bash
curl -X POST "http://localhost:8080/OrdersAPI/AddItemToOrder?orderid=1" \
  -H "Content-Type: application/json" \
  -d '{"articleid":1,"unitprice":10.5,"discount":0,"quantity":2,"description":"Widget A"}'
```

Response:
```json
{"result":1}
```

### Get Order with Items
```bash
curl http://localhost:8080/OrdersAPI/GetOrderByID?id=1
```

Response:
```json
{
  "result": {
    "id": 1,
    "customerid": 100,
    "orderdate": "2025-12-19T00:00:00",
    "total": 21.00,
    "items": [
      {
        "id": 1,
        "articleid": 1,
        "unitprice": 10.50,
        "discount": 0,
        "quantity": 2,
        "description": "Widget A",
        "total": 21.00
      }
    ]
  }
}
```

### Delete Order (Cascading)
```bash
curl http://localhost:8080/OrdersAPI/DeleteOrder?id=1
```

Response:
```json
{"result":true}
```

## Architecture

### File Structure
```
07-master_details/
├── MasterDetailsSample.dpr       # Main program
├── MasterDetailsSample.dproj     # Delphi project file
├── README.md                     # This file
└── src/
    ├── entities.pas              # ORM entities (Orders, OrderItems, Articles)
    ├── api.interfaces.pas        # API interface definitions and DTOs
    ├── api.impl.pas              # API implementation
    └── server.pas                # Server wrapper
```

### Key Concepts

**1. Interface-Based Services**

mORMot2 uses interface-based services instead of DMVC's controller classes:

```pascal
IOrdersAPI = interface(IInvokable)
  function GetOrders: TOrderDTODynArray;
  function CreateOrder(const order: TOrderInputDTO): TID;
  // ... more methods
end;
```

**2. ORM Entities**

Tables are defined as `TOrm` descendants:

```pascal
TOrmOrder = class(TOrm)
published
  property CustomerID: Integer read fCustomerID write fCustomerID;
  property OrderDate: TDateTime read fOrderDate write fOrderDate;
  property Total: Currency read fTotal write fTotal;
end;
```

**3. DTOs for Data Transfer**

Separate DTOs are used for API communication:

```pascal
TOrderDTO = packed record
  id: TID;
  customerid: Integer;
  orderdate: TDateTime;
  total: Currency;
  items: TOrderItemDTODynArray;  // Nested items
end;
```

**4. Manual Relationship Loading**

Unlike ActiveRecord's `OnAfterLoad`, mORMot2 requires explicit loading of related entities:

```pascal
function LoadOrderItems(orderid: TID): TOrderItemDTODynArray;
begin
  if fRest.RetrieveListObjArray(items, TOrmOrderItem, 'OrderID=?', [orderid]) then
    // Convert to DTOs
end;
```

## Comparison with DMVCFramework

| Feature | DMVCFramework | mORMot2 |
|---------|---------------|---------|
| **Service Definition** | Controller classes | Interfaces (IInvokable) |
| **Routing** | Attributes (`[MVCPath]`) | Automatic from interface |
| **ORM Pattern** | ActiveRecord | Repository pattern |
| **Relationships** | Auto-loaded (`OnAfterLoad`) | Manual loading |
| **Cascade Delete** | Configured in DB | Application code |
| **Validation** | Attributes | Manual validation |
| **Serialization** | Automatic | Automatic (DTOs) |

## Implementation Notes

### Manual Cascade Delete

mORMot2 doesn't support database-level cascade deletes out of the box, so we implement it in the service:

```pascal
function DeleteOrder(id: TID): Boolean;
begin
  // 1. Delete all related items
  DeleteOrderItems(id);

  // 2. Delete the order
  Result := fRest.Delete(TOrmOrder, id);
end;
```

### Total Recalculation

Unlike DMVC's ActiveRecord `OnBeforeInsertOrUpdate`, we explicitly call recalculation:

```pascal
function AddItemToOrder(...): TID;
begin
  // Add item
  Result := fRest.Add(newItem, True);

  // Recalculate order total
  RecalculateOrderTotal(orderid);
end;
```

### DTO vs ORM Separation

mORMot2 best practice is to separate ORM entities from DTOs:

- **ORM entities** (`TOrmOrder`): Database structure
- **DTOs** (`TOrderDTO`): API communication with nested data

This allows:
- Adding computed fields (like nested items)
- Hiding internal fields
- API versioning without DB changes

## Running the Sample

1. **Compile the project**:
   ```bash
   cd /mnt/w/mORMot2/ex/dmvc/07-master_details
   dcc32 MasterDetailsSample.dpr
   ```

2. **Run the server**:
   ```bash
   ./Win32/Debug/MasterDetailsSample.exe
   ```

3. **Test the API**:
   ```bash
   # List articles
   curl http://localhost:8080/OrdersAPI/GetArticles

   # Create order
   curl -X POST http://localhost:8080/OrdersAPI/CreateOrder \
     -H "Content-Type: application/json" \
     -d '{"customerid":100,"orderdate":"2025-12-19T00:00:00"}'

   # Add item
   curl -X POST "http://localhost:8080/OrdersAPI/AddItemToOrder?orderid=1" \
     -H "Content-Type: application/json" \
     -d '{"articleid":1,"unitprice":10.5,"discount":0,"quantity":2,"description":"Widget A"}'

   # View order
   curl http://localhost:8080/OrdersAPI/GetOrderByID?id=1
   ```

## Sample Data

The server automatically creates sample articles on first run:

| ID | Description | Price |
|----|-------------|-------|
| 1 | Widget A | 10.50 |
| 2 | Widget B | 25.00 |
| 3 | Widget C | 5.75 |
| 4 | Premium Widget | 99.99 |

## References

- **Original DMVC Sample**: `/mnt/w/DMVCframework/samples/master_details/`
- **mORMot2 Documentation**: https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html
- **mORMot2 ORM Guide**: https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html#TITL_93

## License

This sample is part of the mORMot2 framework examples and follows the same MPL 1.1/GPL 2.0/LGPL 2.1 tri-license as mORMot2.
