# Implementation Notes - Master-Details Sample

**Technical documentation for the mORMot2 master-details implementation**

## Overview

This document details the specific implementation decisions, challenges, and solutions in porting the DMVCFramework master_details sample to mORMot2.

## Database Design

### Schema Definition

The sample uses three tables with relationships:

```pascal
// Product catalog
TOrmArticle = class(TOrm)
  property Description: RawUtf8
  property Price: Currency
end;

// Order header (master)
TOrmOrder = class(TOrm)
  property CustomerID: Integer
  property OrderDate: TDateTime
  property Total: Currency
end;

// Order items (detail)
TOrmOrderItem = class(TOrm)
  property OrderID: TID        // FK to TOrmOrder
  property ArticleID: TID      // Reference to TOrmArticle
  property UnitPrice: Currency
  property Discount: Integer
  property Quantity: Integer
  property Description: RawUtf8
  property Total: Currency
end;
```

### Relationship Management

**Foreign Key Constraint:**
- SQLite3 doesn't enforce FK constraints by default
- Cascade delete is implemented in application code
- This ensures consistent behavior across database backends

**Implementation Choice:**
```pascal
// Explicit cascade delete in DeleteOrder()
// Pros: Database-agnostic, explicit control
// Cons: More code than database FK
```

## DTO Design Patterns

### Separation of Concerns

**ORM Entities** (entities.pas):
- Map directly to database tables
- Simple property storage
- Minimal business logic

**DTOs** (api.interfaces.pas):
- Define API contract
- Include nested relationships
- Can have computed fields
- Version-independent

### Nested DTO Pattern

```pascal
TOrderDTO = packed record
  id: TID;
  customerid: Integer;
  orderdate: TDateTime;
  total: Currency;
  items: TOrderItemDTODynArray;  // Nested array
end;
```

**Benefits:**
- Single API call returns complete order
- Client doesn't need to make multiple requests
- Type-safe structure
- Automatic JSON serialization

**Implementation:**
```pascal
function GetOrderByID(id: TID): TOrderDTO;
begin
  // 1. Load master record
  Result := OrmOrderToDTO(order);

  // 2. Explicitly load details
  Result.items := LoadOrderItems(order.ID);
end;
```

## Business Logic Implementation

### Automatic Total Recalculation

**Design Decision:** Totals are recalculated server-side after every item change.

**Rationale:**
- Prevents client from sending incorrect totals
- Single source of truth
- Discount/price calculations centralized

**Implementation:**
```pascal
procedure RecalculateOrderTotal(orderid: TID);
begin
  // 1. Load all items for order
  // 2. Sum item totals
  // 3. Update order total
end;

// Called after:
// - AddItemToOrder
// - UpdateOrderItem
// - RemoveItemFromOrder
```

**Alternative Considered:**
- Client calculates and sends total
- Rejected: Trusts client calculations, prone to errors

### Item Total Calculation

```pascal
procedure TOrmOrderItem.ComputeTotal;
begin
  fTotal := fUnitPrice * fQuantity * (1 - fDiscount / 100);
end;
```

**Called:**
- Before inserting new item
- Before updating existing item

**Formula:**
```
Total = UnitPrice × Quantity × (1 - Discount/100)

Examples:
  10.50 × 2 × (1 - 0/100)  = 21.00  (no discount)
  25.00 × 1 × (1 - 10/100) = 22.50  (10% discount)
```

## Transaction Management

### No Explicit Transactions

**Current Implementation:** Each operation is atomic.

**Rationale:**
- SQLite3 has implicit transaction per write
- Simpler code, fewer locks
- Sufficient for this sample

**Future Enhancement:**
```pascal
// For complex multi-step operations
fRest.TransactionBegin(TOrmOrder);
try
  fRest.Add(order, True);
  for item in items do
    fRest.Add(item, True);
  RecalculateOrderTotal(order.ID);
  fRest.Commit;
except
  fRest.Rollback;
  raise;
end;
```

## Error Handling Strategy

### Validation

**At Service Layer:**
```pascal
function AddItemToOrder(orderid: TID; ...): TID;
begin
  // 1. Verify order exists
  if not fRest.Retrieve(orderid, TOrmOrder) then
    raise ERestException.CreateUtf8('Order with ID % not found', [orderid]);

  // 2. Proceed with operation
end;
```

**Benefits:**
- Clear error messages
- Fails fast
- RESTful 404/400 responses

### Exception Types

```pascal
ERestException - Used for:
  - Entity not found (404)
  - Business rule violations (400)
  - Data validation errors (400)
```

**Client receives:**
```json
{
  "errorCode": 404,
  "errorText": "Order with ID 999 not found"
}
```

## Performance Considerations

### Loading Strategies

**Current: Eager Loading**
```pascal
function GetOrders: TOrderDTODynArray;
begin
  // For each order, load all items
  for i := 0 to High(orders) do
    Result[i].items := LoadOrderItems(orders[i].ID);
end;
```

**N+1 Problem:**
- 1 query for orders
- N queries for items (one per order)

**Future Optimization:**
```pascal
// Load all items in one query
items := fRest.RetrieveListObjArray(TOrmOrderItem, '');

// Group by OrderID in memory
for item in items do
  orderMap[item.OrderID].Add(item);
```

### Memory Management

**ObjArray Pattern:**
```pascal
if fRest.RetrieveListObjArray(items, TOrmOrderItem, ...) then
try
  // Use items
finally
  ObjArrayClear(items);  // Frees all objects
end;
```

**Benefits:**
- Automatic cleanup
- No memory leaks
- Stack-based error handling

## Sample Data Strategy

### Auto-Population

```pascal
constructor TOrdersAPI.Create(const aRest: IRestOrm);
begin
  if fRest.TableRowCount(TOrmArticle) = 0 then
  begin
    CreateSampleArticle('Widget A', 10.50);
    CreateSampleArticle('Widget B', 25.00);
    // ...
  end;
end;
```

**Rationale:**
- Enables immediate testing
- No manual database setup required
- Self-documenting (shows entity structure)

**Production Consideration:**
- Remove auto-population
- Use separate data seeding script

## API Design Decisions

### Method Naming Convention

```pascal
// Pattern: Verb + [Modifier] + Entity + [ByRelationship]

GetOrders                    // List all
GetOrderByID                 // Single by ID
CreateOrder                  // Create new
UpdateOrder                  // Update existing
DeleteOrder                  // Delete

AddItemToOrder               // Add to relationship
RemoveItemFromOrder          // Remove from relationship
GetOrdersByTotalGreaterThan  // Search/filter
```

**Benefits:**
- Self-documenting URLs
- RESTful semantics
- Consistent pattern

### Input vs Output DTOs

**Input DTOs:** For create/update operations
```pascal
TOrderInputDTO = packed record
  customerid: Integer;
  orderdate: TDateTime;
  // Note: No ID (server-generated)
  // Note: No total (server-calculated)
  // Note: No items (managed separately)
end;
```

**Output DTOs:** For query operations
```pascal
TOrderDTO = packed record
  id: TID;                      // Server-generated
  customerid: Integer;
  orderdate: TDateTime;
  total: Currency;              // Server-calculated
  items: TOrderItemDTODynArray; // Server-loaded
end;
```

**Rationale:**
- Clear separation of client/server responsibilities
- Prevents clients from sending invalid data
- Documents what server will compute

## Testing Strategy

### Test Script (test-master-details.sh)

**Coverage:**
1. Get articles (read catalog)
2. Create order (master record)
3. Add items (detail records)
4. Update item (triggers recalculation)
5. Search orders (query)
6. Remove item (triggers recalculation)
7. Delete order (cascade delete)
8. Verify cascade (entity gone)

**Test Data Flow:**
```
1. Create order → Returns ID=1
2. Add item 1 (2 × $10.50) → Total: $21.00
3. Add item 2 (1 × $25.00 - 10%) → Total: $43.50
4. Update item 1 (5 × $10.50 - 15%) → Total: $67.13
5. Remove item 2 → Total: $44.63
6. Delete order → Items also deleted
```

## Future Enhancements

### 1. Batch Operations

```pascal
function CreateOrderWithItems(
  const order: TOrderInputDTO;
  const items: TOrderItemInputDTODynArray
): TID;
begin
  fRest.TransactionBegin(TOrmOrder);
  try
    // Create order
    orderID := fRest.Add(newOrder, True);

    // Create all items
    for item in items do
      AddItemToOrder(orderID, item);

    fRest.Commit;
  except
    fRest.Rollback;
    raise;
  end;
end;
```

**Benefits:**
- Single API call
- Atomic operation
- Reduced round-trips

### 2. Pagination

```pascal
function GetOrders(offset, limit: Integer): TOrderDTODynArray;
begin
  // Use LIMIT/OFFSET for large datasets
end;
```

### 3. Advanced Search

```pascal
function SearchOrders(
  const criteria: TOrderSearchCriteria
): TOrderDTODynArray;
begin
  // Build WHERE clause from criteria
  // Support: date range, customer, min/max total
end;
```

### 4. Soft Deletes

```pascal
TOrmOrder = class(TOrm)
published
  property DeletedAt: TDateTime;
end;

// Instead of Delete():
order.DeletedAt := Now;
fRest.Update(order);
```

## Comparison with DMVC Original

### Lines of Code

| File | DMVC | mORMot2 | Difference |
|------|------|---------|------------|
| Business Objects | 336 | 58 (entities.pas) | -83% |
| Controllers/Services | 283 | 86 (interfaces) + 372 (impl) | +62% |
| Server Setup | 49 | 73 | +49% |
| **Total** | **668** | **589** | **-12%** |

**Analysis:**
- mORMot2 entities are simpler (no lifecycle hooks)
- mORMot2 services have more explicit code
- Overall: Similar complexity, different distribution

### Key Differences

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| **Relationship Loading** | Automatic (OnAfterLoad) | Explicit (LoadOrderItems) |
| **Total Calculation** | Automatic (OnBeforeInsert) | Explicit (RecalculateOrderTotal) |
| **Cascade Delete** | Implicit (relies on DB) | Explicit (application code) |
| **Response Format** | Custom wrapper | Standard {"result": ...} |
| **Transaction Control** | Manual | Manual |

## Conclusion

The mORMot2 implementation achieves the same functionality as the DMVC original while:

1. **Using explicit control flow** instead of lifecycle hooks
2. **Separating DTOs from ORM** for cleaner API contracts
3. **Implementing cascade deletes** in application code
4. **Providing automatic JSON serialization** via interface return types
5. **Following mORMot2 conventions** for routing and error handling

The result is a maintainable, type-safe implementation that demonstrates mORMot2's approach to master-detail relationships.
