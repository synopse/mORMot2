# 29-activerecord_showcase - ActiveRecord Feature Showcase

**Port of**: DMVCFramework `samples/activerecord_showcase`
**Difficulty**: Intermediate
**Demonstrates**: ORM features, filtering, aggregation, batch operations, relationships

## Overview

A comprehensive showcase of mORMot2's ORM capabilities, demonstrating patterns equivalent to DMVC's ActiveRecord. This sample includes CRUD, filtering, aggregation, batch operations, and table relationships.

## Features Demonstrated

### 1. Basic CRUD Operations
- Create, Read, Update, Delete entities
- Type-safe operations through ORM
- Automatic JSON serialization

### 2. Filtering & Searching
- Filter by single field (`GetCustomersByCity`)
- Filter by comparison (`GetCustomersByRating`)
- Filter by boolean (`GetActiveCustomers`)

### 3. Aggregation
- Count records (`GetCustomerCount`)
- Average values (`GetAverageRating`)
- Multiple aggregates (`GetCustomerStats`)

### 4. Batch Operations
- Bulk insert with transactions (`BulkCreateCustomers`)
- Bulk update (`BulkUpdateRating`)
- Transaction rollback on error

### 5. Multiple Entities
- Customers, Articles, Orders
- Demonstrates various field types (string, integer, currency, datetime, boolean)

### 6. Relationships
- Foreign keys (OrderOrm.CustomerID → CustomerOrm.ID)
- JOIN queries (`GetOrderWithCustomer`)
- One-to-many navigation

## Building & Running

```bash
# Compile
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 29-activerecord_showcase.dproj

# Run
29-activerecord_showcase.exe
```

## Example API Calls

```bash
# Basic CRUD
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetAllCustomers
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetCustomer?id=1

# Filtering
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetCustomersByCity?city=Gotham
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetCustomersByRating?minRating=4
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetActiveCustomers

# Aggregation
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetCustomerCount
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetAverageRating
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetCustomerStats

# Batch operations
curl -X POST http://localhost:8080/showcase/ActiveRecordShowcaseApi/BulkCreateCustomers \
  -H "Content-Type: application/json" \
  -d '[{"code":"A","companyName":"A Corp","city":"NY","rating":5,"note":""}]'

# Articles
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetLowStockArticles?threshold=10

# Orders & Relationships
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetOrdersByCustomer?customerID=1
curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetOrderWithCustomer?orderID=1
```

## DMVC → mORMot2 Feature Mapping

| DMVC ActiveRecord Feature | mORMot2 ORM Equivalent |
|---------------------------|------------------------|
| `Where()` clause builder | `RetrieveListObjArray('field=?', [value])` |
| Named queries | Direct SQL with `ExecuteJson()` |
| `Count()` | `TableRowCount()` or SQL aggregate |
| `Sum()`, `Avg()` | SQL aggregates via `ExecuteJson()` |
| `Store()` batch | Transaction with multiple `Add()` |
| Relationships | Foreign key fields + JOIN queries |
| `OnBeforeInsert` | Service-level validation |
| Field attributes | Published properties |

## Code Highlights

### Filtering Example
```pascal
// Get customers by city
function GetCustomersByCity(const city: RawUtf8): TVariantDynArray;
begin
  if Server.Orm.RetrieveListObjArray(customers, TCustomerOrm,
    'City=?', [city]) then
  begin
    // Convert to variant array for JSON response
    for i := 0 to High(customers) do
      Result[i] := OrmToVariant(customers[i]);
  end;
end;
```

### Aggregation Example
```pascal
// Get customer statistics
function GetCustomerStats: Variant;
begin
  json := Server.Orm.ExecuteJson([],
    'SELECT COUNT(*) as TotalCustomers, ' +
    'AVG(Rating) as AverageRating, ' +
    'MIN(Rating) as MinRating, ' +
    'MAX(Rating) as MaxRating ' +
    'FROM CustomerOrm');
  Result := JsonArrayToVariants(json)[0];
end;
```

### Batch Operation with Transaction
```pascal
// Bulk create with rollback on error
function BulkCreateCustomers(customers: TVariantDynArray): Integer;
begin
  if Server.Orm.TransactionBegin(TCustomerOrm) then
  try
    for each customer do
      Server.Orm.Add(customer, true);
    Server.Orm.Commit;
  except
    Server.Orm.RollBack;
    raise;
  end;
end;
```

### JOIN Query Example
```pascal
// Get order with customer details (JOIN)
function GetOrderWithCustomer(orderID: TID): Variant;
begin
  json := Server.Orm.ExecuteJson([],
    'SELECT o.*, c.Code, c.CompanyName, c.City ' +
    'FROM OrderOrm o ' +
    'INNER JOIN CustomerOrm c ON o.CustomerID = c.RowID ' +
    'WHERE o.RowID=?', [], [orderID]);
end;
```

## Architecture Patterns

### ORM Layer
- Entities extend `TOrm`
- Published properties auto-map to database fields
- RowID is auto-generated primary key

### Service Layer
- Services implement `IInvokable` interfaces
- Services inject `Server` for ORM access
- Handle business logic and validation

### Data Access
- ORM methods: `Add()`, `Update()`, `Delete()`, `Retrieve*()`
- SQL queries: `ExecuteJson()`, `ExecuteNoResult()`
- Transactions: `TransactionBegin()`, `Commit()`, `RollBack()`

## See Also

- **Sample 28** - Full CRUD with multiple entities
- **Sample 30** - Simple customer API
- **Sample 31** - Dataset-based approach (no ORM)

---

**Created**: 2025-12-20
**Demonstrates**: 20+ ORM features and patterns
