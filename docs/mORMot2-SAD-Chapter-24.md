# 24. Domain-Driven Design

*Building Maintainable Business Applications*

Domain-Driven Design (DDD) provides a set of patterns and practices for building complex business applications. mORMot's architecture aligns naturally with DDD principles through its ORM, SOA, and REST layers.

---

## 24.1. Introduction to DDD

### 24.1.1. What is DDD?

Domain-Driven Design is a software development approach that:

- Focuses on the **core domain** and domain logic
- Bases complex designs on a **model** of the domain
- Initiates creative collaboration between technical and domain experts

### 24.1.2. Why DDD with mORMot?

| mORMot Feature | DDD Concept |
|----------------|-------------|
| `TOrm` classes | Entities, Value Objects |
| `IInvokable` interfaces | Domain Services |
| `IRestOrm` | Repository Pattern |
| `TRestBatch` | Unit of Work |
| JSON serialization | DTOs |
| Interface-based services | Application Services |

---

## 24.2. DDD Building Blocks

### 24.2.1. Core Concepts

```
┌─────────────────────────────────────────────────────────────────┐
│                    DDD Building Blocks                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────┐  ┌─────────────────┐                       │
│  │ Value Objects   │  │ Entities        │                       │
│  │                 │  │                 │                       │
│  │ • Immutable     │  │ • Identity      │                       │
│  │ • No identity   │  │ • Lifecycle     │                       │
│  │ • Equality by   │  │ • Equality by   │                       │
│  │   value         │  │   ID            │                       │
│  └─────────────────┘  └─────────────────┘                       │
│                              │                                  │
│                              ▼                                  │
│                    ┌─────────────────┐                          │
│                    │ Aggregates      │                          │
│                    │                 │                          │
│                    │ • Root Entity   │                          │
│                    │ • Consistency   │                          │
│                    │   boundary      │                          │
│                    │ • Transactional │                          │
│                    └─────────────────┘                          │
│                              │                                  │
│              ┌───────────────┼───────────────┐                  │
│              ▼               ▼               ▼                  │
│     ┌──────────────┐ ┌──────────────┐ ┌──────────────┐          │
│     │ Repository   │ │ Factory      │ │ Services     │          │
│     │ (IRestOrm)   │ │ (Create)     │ │ (IInvokable) │          │
│     └──────────────┘ └──────────────┘ └──────────────┘          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 24.3. Ubiquitous Language

### 24.3.1. The Foundation of DDD

The **Ubiquitous Language** is a shared vocabulary between developers and domain experts:

```pascal
// ❌ Technical naming (unclear domain meaning)
type
  TData = class(TOrm)
    property S: RawUtf8;      // What is S?
    property N: Integer;       // What is N?
    property F: Boolean;       // What is F?
  end;

// ✓ Ubiquitous Language (domain-clear)
type
  TCustomerOrder = class(TOrm)
    property CustomerName: RawUtf8;
    property OrderNumber: Integer;
    property IsFulfilled: Boolean;
  end;
```

### 24.3.2. Specialized Types

Use type aliases to express domain concepts:

```pascal
type
  // Make implicit explicit
  TCustomerName = type RawUtf8;
  TEmailAddress = type RawUtf8;
  TOrderNumber = type Integer;
  TCurrency = type Currency;

  TCustomer = class(TOrm)
  published
    property Name: TCustomerName read fName write fName;
    property Email: TEmailAddress read fEmail write fEmail;
  end;
```

Benefits:
- Compiler catches type mismatches
- Self-documenting code
- Domain concepts explicit in code

---

## 24.4. Value Objects

### 24.4.1. Characteristics

Value Objects:
- Are **immutable** (no setters after creation)
- Have **no identity** (compared by value)
- Represent domain concepts (Money, Address, DateRange)

### 24.4.2. Implementation with Records

```pascal
type
  /// Money value object - immutable
  TMoney = packed record
  private
    fAmount: Currency;
    fCurrency: RawUtf8;
  public
    class function Create(aAmount: Currency; const aCurrency: RawUtf8): TMoney; static;
    function Add(const Other: TMoney): TMoney;
    function Equals(const Other: TMoney): Boolean;
    property Amount: Currency read fAmount;
    property CurrencyCode: RawUtf8 read fCurrency;
  end;

class function TMoney.Create(aAmount: Currency; const aCurrency: RawUtf8): TMoney;
begin
  Result.fAmount := aAmount;
  Result.fCurrency := aCurrency;
end;

function TMoney.Add(const Other: TMoney): TMoney;
begin
  if fCurrency <> Other.fCurrency then
    raise EDomainError.Create('Cannot add different currencies');
  Result := TMoney.Create(fAmount + Other.fAmount, fCurrency);
end;
```

### 24.4.3. Implementation with Classes

```pascal
type
  TAddress = class(TSynPersistent)
  private
    fStreet: RawUtf8;
    fCity: RawUtf8;
    fPostalCode: RawUtf8;
    fCountry: RawUtf8;
  public
    constructor Create(const aStreet, aCity, aPostalCode, aCountry: RawUtf8);
    function Equals(Other: TAddress): Boolean;
  published
    property Street: RawUtf8 read fStreet;      // No setter = immutable
    property City: RawUtf8 read fCity;
    property PostalCode: RawUtf8 read fPostalCode;
    property Country: RawUtf8 read fCountry;
  end;
```

---

## 24.5. Entities

### 24.5.1. Characteristics

Entities:
- Have **identity** (unique ID)
- Have a **lifecycle** (created, modified, deleted)
- Are compared by **ID**, not values

### 24.5.2. Implementation with TOrm

```pascal
type
  TCustomer = class(TOrm)
  private
    fName: RawUtf8;
    fEmail: RawUtf8;
    fRegistrationDate: TDateTime;
    fStatus: TCustomerStatus;
  public
    // Domain behavior
    procedure Activate;
    procedure Deactivate;
    function CanPlaceOrder: Boolean;
  published
    property Name: RawUtf8 read fName write fName;
    property Email: RawUtf8 read fEmail write fEmail;
    property RegistrationDate: TDateTime read fRegistrationDate write fRegistrationDate;
    property Status: TCustomerStatus read fStatus write fStatus;
  end;

procedure TCustomer.Activate;
begin
  if fStatus = csDeactivated then
    fStatus := csActive;
end;

function TCustomer.CanPlaceOrder: Boolean;
begin
  Result := (fStatus = csActive) and (fEmail <> '');
end;
```

---

## 24.6. Aggregates

### 24.6.1. Concept

An **Aggregate** is a cluster of domain objects treated as a single unit:

- Has a **Root Entity** (the only entry point)
- Defines a **consistency boundary**
- External objects can only reference the root

### 24.6.2. Order Aggregate Example

```pascal
type
  // Aggregate Root
  TOrder = class(TOrm)
  private
    fCustomerID: TID;
    fOrderDate: TDateTime;
    fStatus: TOrderStatus;
    fItems: TOrmMany;  // Nested entities
    fTotalAmount: Currency;
  public
    // Only aggregate root exposes behavior
    procedure AddItem(ProductID: TID; Quantity: Integer; UnitPrice: Currency);
    procedure RemoveItem(ItemID: TID);
    procedure Submit;
    procedure Cancel;
    function CalculateTotal: Currency;
  published
    property CustomerID: TID read fCustomerID write fCustomerID;
    property OrderDate: TDateTime read fOrderDate write fOrderDate;
    property Status: TOrderStatus read fStatus;
    property Items: TOrmMany read fItems;  // Read-only access
    property TotalAmount: Currency read fTotalAmount;
  end;

  // Nested entity (only accessible via TOrder)
  TOrderItem = class(TOrm)
  private
    fOrderID: TID;
    fProductID: TID;
    fQuantity: Integer;
    fUnitPrice: Currency;
  published
    property OrderID: TID read fOrderID write fOrderID;
    property ProductID: TID read fProductID write fProductID;
    property Quantity: Integer read fQuantity write fQuantity;
    property UnitPrice: Currency read fUnitPrice write fUnitPrice;
  end;

procedure TOrder.AddItem(ProductID: TID; Quantity: Integer; UnitPrice: Currency);
begin
  if fStatus <> osCreated then
    raise EDomainError.Create('Cannot modify submitted order');
  // Add item logic...
  fTotalAmount := CalculateTotal;
end;

procedure TOrder.Submit;
begin
  if Items.Count = 0 then
    raise EDomainError.Create('Cannot submit empty order');
  fStatus := osSubmitted;
end;
```

---

## 24.7. Repository Pattern

### 24.7.1. Concept

Repositories provide an abstraction over data access:

```
┌───────────────────┐     ┌───────────────────┐
│   Domain Layer    │     │ Infrastructure    │
├───────────────────┤     ├───────────────────┤
│                   │     │                   │
│  IOrderRepository │────►│ TOrmOrderRepo     │
│  (interface)      │     │ (implementation)  │
│                   │     │                   │
└───────────────────┘     └───────────────────┘
```

### 24.7.2. Repository Interface

```pascal
type
  IOrderRepository = interface(IInvokable)
    ['{A1B2C3D4-...}']
    function GetByID(ID: TID): TOrder;
    function GetByCustomer(CustomerID: TID): TOrderObjArray;
    procedure Save(Order: TOrder);
    procedure Delete(Order: TOrder);
  end;
```

### 24.7.3. Implementation with IRestOrm

```pascal
type
  TOrmOrderRepository = class(TInterfacedObject, IOrderRepository)
  private
    fOrm: IRestOrm;
  public
    constructor Create(const aOrm: IRestOrm);
    function GetByID(ID: TID): TOrder;
    function GetByCustomer(CustomerID: TID): TOrderObjArray;
    procedure Save(Order: TOrder);
    procedure Delete(Order: TOrder);
  end;

function TOrmOrderRepository.GetByID(ID: TID): TOrder;
begin
  Result := TOrder.Create;
  if not fOrm.Retrieve(ID, Result) then
    FreeAndNil(Result);
end;

procedure TOrmOrderRepository.Save(Order: TOrder);
begin
  if Order.ID = 0 then
    fOrm.Add(Order, True)
  else
    fOrm.Update(Order);
end;
```

---

## 24.8. Domain Services

### 24.8.1. When to Use

Domain Services handle operations that:
- Don't belong to any single Entity
- Involve multiple Aggregates
- Represent domain concepts (not CRUD)

### 24.8.2. Service Interface

```pascal
type
  IOrderProcessingService = interface(IInvokable)
    ['{E5F6G7H8-...}']
    function PlaceOrder(CustomerID: TID; const Items: TOrderItemArray): TID;
    function CancelOrder(OrderID: TID): Boolean;
    function CalculateShipping(OrderID: TID): Currency;
  end;

  IPricingService = interface(IInvokable)
    ['{I9J0K1L2-...}']
    function CalculateDiscount(CustomerID: TID; Amount: Currency): Currency;
    function ApplyPromotion(const Code: RawUtf8; Amount: Currency): Currency;
  end;
```

### 24.8.3. Service Implementation

```pascal
type
  TOrderProcessingService = class(TInjectableObject, IOrderProcessingService)
  private
    fOrders: IOrderRepository;
    fCustomers: ICustomerRepository;
    fPricing: IPricingService;
  public
    constructor Create(const aOrders: IOrderRepository;
                       const aCustomers: ICustomerRepository;
                       const aPricing: IPricingService);
    function PlaceOrder(CustomerID: TID; const Items: TOrderItemArray): TID;
  end;

function TOrderProcessingService.PlaceOrder(CustomerID: TID;
  const Items: TOrderItemArray): TID;
var
  Customer: TCustomer;
  Order: TOrder;
  i: Integer;
begin
  // Domain validation
  Customer := fCustomers.GetByID(CustomerID);
  if Customer = nil then
    raise EDomainError.Create('Customer not found');
  if not Customer.CanPlaceOrder then
    raise EDomainError.Create('Customer cannot place orders');

  // Create aggregate
  Order := TOrder.Create;
  try
    Order.CustomerID := CustomerID;
    Order.OrderDate := Now;

    for i := 0 to High(Items) do
      Order.AddItem(Items[i].ProductID, Items[i].Quantity, Items[i].UnitPrice);

    // Apply domain rules
    Order.TotalAmount := fPricing.CalculateDiscount(CustomerID, Order.CalculateTotal);

    Order.Submit;
    fOrders.Save(Order);
    Result := Order.ID;
  finally
    Order.Free;
  end;
end;
```

---

## 24.9. Application Services

### 24.9.1. Role

Application Services:
- Orchestrate domain objects and services
- Handle transactions (Unit of Work)
- Don't contain business logic
- Convert between DTOs and domain objects

### 24.9.2. Implementation

```pascal
type
  IOrderApplicationService = interface(IInvokable)
    ['{M3N4O5P6-...}']
    function CreateOrder(const Request: TCreateOrderRequest): TCreateOrderResponse;
    function GetOrderStatus(OrderID: TID): TOrderStatusResponse;
  end;

  TOrderApplicationService = class(TInjectableObject, IOrderApplicationService)
  private
    fOrderService: IOrderProcessingService;
    fOrders: IOrderRepository;
  public
    function CreateOrder(const Request: TCreateOrderRequest): TCreateOrderResponse;
  end;

function TOrderApplicationService.CreateOrder(
  const Request: TCreateOrderRequest): TCreateOrderResponse;
begin
  try
    Result.OrderID := fOrderService.PlaceOrder(Request.CustomerID, Request.Items);
    Result.Success := True;
    Result.Message := 'Order created successfully';
  except
    on E: EDomainError do
    begin
      Result.Success := False;
      Result.Message := E.Message;
    end;
  end;
end;
```

---

## 24.10. Data Transfer Objects (DTOs)

### 24.10.1. Purpose

DTOs:
- Separate domain from external interfaces
- Define API contracts
- Allow domain to evolve independently

### 24.10.2. Implementation

```pascal
type
  // Request DTO
  TCreateOrderRequest = packed record
    CustomerID: TID;
    Items: TOrderItemDtoArray;
  end;

  TOrderItemDto = packed record
    ProductID: TID;
    Quantity: Integer;
    UnitPrice: Currency;
  end;

  // Response DTO
  TCreateOrderResponse = packed record
    Success: Boolean;
    OrderID: TID;
    Message: RawUtf8;
  end;

  TOrderStatusResponse = packed record
    OrderID: TID;
    Status: RawUtf8;
    TotalAmount: Currency;
    ItemCount: Integer;
  end;
```

---

## 24.11. Clean Architecture

### 24.11.1. Layer Structure

```
┌─────────────────────────────────────────────────────────────────┐
│                     Infrastructure Layer                         │
│     (Database, External Services, UI, HTTP Server)               │
├─────────────────────────────────────────────────────────────────┤
│                     Application Layer                            │
│           (Use Cases, DTOs, Application Services)                │
├─────────────────────────────────────────────────────────────────┤
│                      Domain Layer                                │
│   (Entities, Value Objects, Aggregates, Domain Services)         │
└─────────────────────────────────────────────────────────────────┘

Dependencies point inward → Domain has NO external dependencies
```

### 24.11.2. mORMot Architecture Mapping

| Layer | mORMot Components |
|-------|------------------|
| Domain | `TOrm` entities, `TSynPersistent` value objects |
| Application | `IInvokable` service interfaces |
| Infrastructure | `TRestServer`, `TRestHttpServer`, SQL/NoSQL |

### 24.11.3. Dependency Injection

```pascal
var
  Server: TRestServer;
begin
  Server := TRestServerDB.Create(Model, 'data.db3', True);

  // Register services with DI
  Server.ServiceDefine(TOrderProcessingService, [IOrderProcessingService], sicShared);
  Server.ServiceDefine(TPricingService, [IPricingService], sicShared);
  Server.ServiceDefine(TOrderApplicationService, [IOrderApplicationService], sicShared);

  // Dependency resolution is automatic for constructor injection
end;
```

---

## 24.12. Unit of Work Pattern

### 24.12.1. Using TRestBatch

```pascal
procedure SaveOrderWithItems(Server: TRestServer; Order: TOrder);
var
  Batch: TRestBatch;
  i: Integer;
begin
  Batch := TRestBatch.Create(Server, nil, 1000);
  try
    // Add order (will get ID after send)
    Batch.Add(Order, True);

    // Add all items
    for i := 0 to Order.Items.Count - 1 do
      Batch.Add(Order.Items[i], True);

    // Atomic commit
    if Server.BatchSend(Batch) <> HTTP_SUCCESS then
      raise EDomainError.Create('Failed to save order');
  finally
    Batch.Free;
  end;
end;
```

---

## 24.13. Event-Driven Design

### 24.13.1. Domain Events

```pascal
type
  TDomainEvent = class(TSynPersistent)
  private
    fTimestamp: TDateTime;
    fAggregateID: TID;
  public
    constructor Create(AggregateID: TID);
  published
    property Timestamp: TDateTime read fTimestamp;
    property AggregateID: TID read fAggregateID;
  end;

  TOrderPlacedEvent = class(TDomainEvent)
  private
    fCustomerID: TID;
    fTotalAmount: Currency;
  published
    property CustomerID: TID read fCustomerID write fCustomerID;
    property TotalAmount: Currency read fTotalAmount write fTotalAmount;
  end;
```

### 24.13.2. Event Handling

```pascal
type
  IDomainEventHandler = interface
    procedure Handle(Event: TDomainEvent);
  end;

  TOrderPlacedHandler = class(TInterfacedObject, IDomainEventHandler)
  public
    procedure Handle(Event: TDomainEvent);
  end;

procedure TOrderPlacedHandler.Handle(Event: TDomainEvent);
var
  OrderEvent: TOrderPlacedEvent;
begin
  if Event is TOrderPlacedEvent then
  begin
    OrderEvent := TOrderPlacedEvent(Event);
    // Send notification, update inventory, etc.
    SendOrderConfirmationEmail(OrderEvent.CustomerID, OrderEvent.AggregateID);
  end;
end;
```

---

## 24.14. Testing DDD Code

### 24.14.1. Domain Unit Tests

```pascal
procedure TTestOrder.TestCannotAddItemToSubmittedOrder;
var
  Order: TOrder;
begin
  Order := TOrder.Create;
  try
    Order.AddItem(1, 2, 10.00);
    Order.Submit;

    // Should raise exception
    CheckException(
      procedure begin Order.AddItem(2, 1, 5.00); end,
      EDomainError,
      'Cannot modify submitted order'
    );
  finally
    Order.Free;
  end;
end;
```

### 24.14.2. Service Tests with Mocks

```pascal
procedure TTestOrderService.TestPlaceOrderWithDiscount;
var
  MockOrders: IOrderRepository;
  MockCustomers: ICustomerRepository;
  MockPricing: IPricingService;
  Service: IOrderProcessingService;
begin
  // Setup mocks
  MockOrders := TMockOrderRepository.Create;
  MockCustomers := TMockCustomerRepository.Create;
  MockPricing := TMockPricingService.Create;

  Service := TOrderProcessingService.Create(MockOrders, MockCustomers, MockPricing);

  // Test
  // ...
end;
```

---

## 24.15. Summary

### 24.15.1. Key Patterns

| Pattern | mORMot Implementation |
|---------|----------------------|
| Entity | `TOrm` class with business methods |
| Value Object | `record` or immutable `TSynPersistent` |
| Aggregate | `TOrm` with `TOrmMany` relations |
| Repository | `IRestOrm` or custom interface |
| Domain Service | `IInvokable` interface |
| Application Service | `IInvokable` with DTO I/O |
| Unit of Work | `TRestBatch` |

### 24.15.2. Best Practices

1. **Start with the domain** - Define entities and value objects first
2. **Use ubiquitous language** - Name types after domain concepts
3. **Keep domain pure** - No infrastructure dependencies
4. **Define clear boundaries** - One aggregate per transaction
5. **Test domain logic** - Unit tests for business rules
6. **Use interfaces** - Enable dependency injection and testing

---

*Next: Chapter 25 covers Testing and Logging.*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 23: Asymmetric Encryption](mORMot2-SAD-Chapter-23.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 25: Testing and Logging](mORMot2-SAD-Chapter-25.md) |
