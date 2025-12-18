# 13. Server-Side ORM Processing

*Behind the Scenes*

This chapter explores how the server processes ORM requests, including URI routing, SQL generation, virtual tables, and server-side customization.

---

## 13.1. Request Processing Flow

### 13.1.1. URI to SQL Pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│  Client Request: GET /api/Customer/123                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  1. HTTP Server receives request                                │
│     TRestHttpServer.Request()                                   │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  2. Router matches URI pattern                                  │
│     TRestRouter → /api/Customer/123 → rnTableID                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  3. Authentication check                                        │
│     TRestServer.SessionGetUser()                                │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  4. ORM processes request                                       │
│     IRestOrm.Retrieve() → SQL: SELECT * FROM Customer WHERE ID=1│
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  5. Response as JSON                                            │
│     {"ID":123,"Name":"ACME","Email":"..."}                      │
└─────────────────────────────────────────────────────────────────┘
```

### 13.1.2. Router Node Types

| Node | URI Pattern | HTTP Methods |
|------|-------------|--------------|
| `rnTable` | `/root/TableName` | GET (list), POST (create) |
| `rnTableID` | `/root/TableName/<id>` | GET, PUT, DELETE |
| `rnTableIDBlob` | `/root/TableName/<id>/BlobField` | GET, PUT |
| `rnTableMethod` | `/root/TableName/<method>` | GET, POST |
| `rnMethod` | `/root/<method>` | GET, POST |
| `rnInterface` | `/root/Interface.Method` | POST |

---

## 13.2. SQL Generation

### 13.2.1. Automatic SQL from URI

The ORM translates REST requests to SQL:

```
GET /api/Customer
→ SELECT ID, Name, Email, ... FROM Customer

GET /api/Customer/123
→ SELECT ID, Name, Email, ... FROM Customer WHERE ID=123

GET /api/Customer?where=Country%3D%27USA%27
→ SELECT ID, Name, Email, ... FROM Customer WHERE Country='USA'

POST /api/Customer (body: {"Name":"ACME"})
→ INSERT INTO Customer (Name) VALUES ('ACME')

PUT /api/Customer/123 (body: {"Name":"Updated"})
→ UPDATE Customer SET Name='Updated' WHERE ID=123

DELETE /api/Customer/123
→ DELETE FROM Customer WHERE ID=123
```

### 13.2.2. Query Parameters

| Parameter | Description | Example |
|-----------|-------------|---------|
| `where` | WHERE clause | `?where=Country='USA'` |
| `select` | Fields to return | `?select=Name,Email` |
| `limit` | Max results | `?limit=100` |
| `offset` | Skip results | `?offset=50` |
| `order` | ORDER BY | `?order=Name` |

### 13.2.3. Inlined JSON Parameters

Parameters can be embedded in WHERE clause:

```pascal
// Client sends
'Name = :("John"):AND Age > :(30):'

// Server extracts and binds
SQL: 'SELECT ... WHERE Name = ? AND Age > ?'
Params: ['John', 30]
```

---

## 13.3. Virtual Tables

### 13.3.1. Storage Backends

The ORM can mix multiple storage backends:

```pascal
Model := TOrmModel.Create([
  TOrmUser,      // Internal SQLite3
  TOrmProduct,   // External PostgreSQL
  TOrmLog,       // MongoDB
  TOrmCache      // In-memory
]);

// Map to different backends
OrmMapExternal(Model, TOrmProduct, PostgresProps);
OrmMapMongoDB(Model, TOrmLog, MongoClient.Database['logs']);

// Create server first, then add in-memory storage
Server := TRestServerDB.Create(Model, 'main.db3');
OrmMapInMemory(Server.OrmInstance, TOrmCache);  // In-memory table
```

### 13.3.2. How Virtual Tables Work

```
┌─────────────────────────────────────────────────────────────────┐
│                      SQLite3 Core                     │
│  SELECT * FROM Product, User WHERE Product.UserID = Us│
└─────────────────────────────────────────────────────────────────┘
        │                                    │
        │ Virtual Table                      │ Native Table
        ▼                                    ▼
┌───────────────────┐               ┌───────────────────┐
│   PostgreSQL      │               │   SQLite3 File    │
│   (Product)       │               │   (User)          │
└───────────────────┘               └───────────────────┘
```

### 13.3.3. Cross-Database JOINs

```pascal
// This works even with mixed backends!
Server.Orm.ExecuteList([TOrmProduct, TOrmUser],
  'SELECT Product.Name, User.Email ' +
  'FROM Product, User ' +
  'WHERE Product.UserID = User.ID');
```

---

## 13.4. Server-Side Events

> **Note**: mORMot 2 uses a **callback-based event system** rather than virtual methods to override. Server-side events are handled through the `OnUpdateEvent` property and `ComputeFieldsBeforeWrite` method.

### 13.4.1. OnUpdateEvent Callback

The `TRestOrmServer.OnUpdateEvent` property receives notifications for all ORM operations:

```pascal
type
  TOnOrmEvent = function(Sender: TRestServer; Event: TOrmEvent;
    aTable: TOrmClass; const aID: TID;
    const aSentData: RawUtf8): boolean of object;
```

**Event timing**:
- Called **AFTER** insert (`oeAdd`) and update (`oeUpdate`)
- Called **BEFORE** delete (`oeDelete`)
- `aSentData` contains JSON of affected fields (not full record)

```pascal
type
  TMyEventHandler = class
    function HandleOrmEvent(Sender: TRestServer; Event: TOrmEvent;
      aTable: TOrmClass; const aID: TID;
      const aSentData: RawUtf8): boolean;
  end;

function TMyEventHandler.HandleOrmEvent(Sender: TRestServer; Event: TOrmEvent;
  aTable: TOrmClass; const aID: TID; const aSentData: RawUtf8): boolean;
begin
  Result := True;  // Continue processing

  case Event of
    oeAdd:
      LogEvent(Format('Created %s #%d', [aTable.ClassName, aID]));
    oeUpdate:
      LogEvent(Format('Updated %s #%d: %s', [aTable.ClassName, aID, aSentData]));
    oeDelete:
      begin
        LogEvent(Format('Deleting %s #%d', [aTable.ClassName, aID]));
        // Cascade operations (called BEFORE actual delete)
        if aTable = TOrmCustomer then
          Sender.Orm.Delete(TOrmOrder, 'CustomerID = ?', [aID]);
      end;
  end;
end;

// Register the callback
var
  Handler: TMyEventHandler;
begin
  Handler := TMyEventHandler.Create;
  TRestOrmServer(Server.OrmInstance).OnUpdateEvent := Handler.HandleOrmEvent;
end;
```

### 13.4.2. TOrmEvent Values

| Event | Description | Timing |
|-------|-------------|--------|
| `oeAdd` | Record inserted | After INSERT |
| `oeUpdate` | Record updated | After UPDATE |
| `oeDelete` | Record deleted | **Before** DELETE |
| `oeUpdateBlob` | BLOB field updated | After BLOB update |

### 13.4.3. Validation with ComputeFieldsBeforeWrite

For **pre-write validation and computed fields**, override `ComputeFieldsBeforeWrite` in your `TOrm` class:

```pascal
type
  TOrmCustomer = class(TOrm)
  private
    fName: RawUtf8;
    fEmail: RawUtf8;
    fSearchText: RawUtf8;
  protected
    procedure ComputeFieldsBeforeWrite(const aRest: IRestOrm;
      aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog = 0); override;
  published
    property Name: RawUtf8 read fName write fName;
    property Email: RawUtf8 read fEmail write fEmail;
    property SearchText: RawUtf8 read fSearchText write fSearchText;
  end;

procedure TOrmCustomer.ComputeFieldsBeforeWrite(const aRest: IRestOrm;
  aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog);
begin
  inherited;

  // Validate before insert/update
  if aOccasion in [oeAdd, oeUpdate] then
  begin
    if fEmail = '' then
      raise EOrmException.Create('Email is required');
    if not IsValidEmail(fEmail) then
      raise EOrmException.Create('Invalid email format');
  end;

  // Compute derived fields
  fSearchText := LowerCase(fName + ' ' + fEmail);
end;
```

> **Important**: Enable server-side execution with `rsoComputeFieldsBeforeWriteOnServerSide` in `TRestServer.Options` to ensure this runs on the server even for client-initiated requests.

### 13.4.4. Choosing the Right Pattern

| Pattern | Use Case | Timing |
|---------|----------|--------|
| `ComputeFieldsBeforeWrite` | Validation, computed fields, record-level logic | Before write |
| `OnUpdateEvent` | Audit trails, notifications, cascade operations | After insert/update, before delete |

**Best practices**:
- Use `ComputeFieldsBeforeWrite` for validation (can reject via exception)
- Use `OnUpdateEvent` for logging and side effects (cannot reject)
- Avoid complex operations in events to maintain performance

### 13.4.5. TOrm Event Methods (Legacy Pattern)

Override in TOrm class for record-level events:

```pascal
type
  TOrmCustomer = class(TOrm)
  protected
    procedure ComputeFieldsBeforeWrite(const aRest: IRestOrm;
      aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog = 0); override;
  end;

procedure TOrmCustomer.ComputeFieldsBeforeWrite(const aRest: IRestOrm;
  aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog);
begin
  inherited;
  // Auto-compute fields before save
  if aOccasion in [oeAdd, oeUpdate] then
    fSearchText := LowerCase(fName + ' ' + fEmail);
end;
```

---

## 13.5. Server-Side Filtering

### 13.5.1. Access Control per Table

Access control in mORMot 2 is managed through `TAuthGroup.OrmAccessRights`:

```pascal
// Configure access rights in TAuthGroup record
Group := TAuthGroup.Create;
try
  Group.Ident := 'Limited';
  // Grant read/write to specific tables only
  Group.OrmAccessRights.Edit([TOrmCustomer, TOrmOrder], [arRead, arWrite]);
  // Deny access to settings table
  Group.OrmAccessRights.Edit([TOrmSettings], []);  // No rights
  Server.Orm.Add(Group, True);
finally
  Group.Free;
end;

// Assign users to groups for automatic access control
User.GroupRights := GroupID;  // TAuthGroup.ID
```

For custom per-request authorization, use method-based services with explicit checks:

```pascal
procedure TMyService.GetSettings(Ctxt: TRestServerUriContext);
begin
  // Check user group manually
  if not Ctxt.Session.User.GroupRights.SqlAccessRights
    .CanExecuteOrm(TOrmSettings, arRead) then
  begin
    Ctxt.Error('Access denied', HTTP_FORBIDDEN);
    Exit;
  end;
  // ... proceed with operation
end;
```

### 13.5.2. Field-Level Security

```pascal
type
  TOrmUser = class(TOrm)
  private
    fName: RawUtf8;
    fPassword: RawUtf8;
    fInternalNote: RawUtf8;  // Never expose to clients
  published
    property Name: RawUtf8 read fName write fName;
    property Password: RawUtf8 read fPassword write fPassword;
    property InternalNote: RawUtf8 read fInternalNote write fInternalNote
      stored False;  // Not transmitted over REST
  end;
```

### 13.5.3. Dynamic WHERE Injection

For multi-tenant filtering, implement filtering in your `TOrm` class or use custom storage:

```pascal
// Pattern 1: Override TOrm.FillPrepare for client-side filtering
// (limited - doesn't protect server-side queries)

// Pattern 2: Use custom TRestStorage descendant for server-side filtering
type
  TRestStorageTenant = class(TRestStorageInMemory)
  protected
    function EngineList(TableModelIndex: integer;
      const SQL: RawUtf8): RawUtf8; override;
  public
    TenantID: TID;
  end;

function TRestStorageTenant.EngineList(TableModelIndex: integer;
  const SQL: RawUtf8): RawUtf8;
var
  FilteredSQL: RawUtf8;
begin
  // Inject tenant filter into all queries
  FilteredSQL := InjectWhereClause(SQL, FormatUtf8('TenantID = %', [TenantID]));
  Result := inherited EngineList(TableModelIndex, FilteredSQL);
end;

// Pattern 3: Use method-based services for controlled data access
// (recommended for multi-tenant applications)
procedure TCustomerService.GetCustomers(Ctxt: TRestServerUriContext);
begin
  Ctxt.Returns(Server.Orm.RetrieveListJson(TOrmCustomer,
    'TenantID = ?', [GetCurrentTenantID(Ctxt)]));
end;
```

> **Note**: mORMot 2 does not have a built-in `OnBeforeUriExecute` callback for WHERE injection. Multi-tenant filtering should be implemented through custom storage classes or method-based services.

---

## 13.6. Server-Side Caching

### 13.6.1. Enable Table Caching

```pascal
// Cache entire table in memory
Server.Cache.SetCache(TOrmProduct);

// Cache with timeout
Server.Cache.SetTimeOut(TOrmProduct, 300000);  // 5 minutes

// Cache frequently accessed records
Server.Cache.SetCache(TOrmSettings, True);  // Force all records cached
```

### 13.6.2. Cache Statistics

> **Note**: `TOrmCache` does not expose hit/miss statistics directly. For server monitoring, use `TRestServerMonitor`:

```pascal
// Enable server monitoring
Server.CreateMissingTables;

// Access statistics via TRestServerMonitor
if Server.Stats <> nil then
begin
  WriteLn('Total ORM reads: ', Server.Stats.NotifyOrmTable);
  WriteLn('ORM cache saved: ', Server.Stats.NotifyOrmCache, ' lookups');
end;

// For detailed per-table cache info, iterate cached tables
for TableIndex := 0 to Server.Model.TablesMax do
  if Server.Cache.IsCached(Server.Model.Tables[TableIndex]) then
    WriteLn(Server.Model.Tables[TableIndex].ClassName, ': cached');
```

### 13.6.3. Manual Cache Invalidation

```pascal
// Clear specific record
Server.Cache.NotifyDeletion(TOrmProduct, ProductID);

// Clear entire table
Server.Cache.Clear(TOrmProduct);

// Clear all caches
Server.Cache.Clear;
```

---

## 13.7. Write Modes

### 13.7.1. Direct vs Batch Mode

```pascal
// Direct mode (default): Each write goes to database immediately
Server.Orm.Add(Customer, True);  // INSERT executed now

// Batch mode on server: Use TRestBatch directly
var
  Batch: TRestBatch;
  Results: TIDDynArray;
begin
  Batch := TRestBatch.Create(Server.Orm, TOrmCustomer);
  try
    Batch.Add(Customer1, True);  // Queued
    Batch.Add(Customer2, True);  // Queued
    Server.Orm.BatchSend(Batch, Results);  // All INSERTs now
  finally
    Batch.Free;
  end;
end;
```

**Note**: `BatchStart`/`BatchSend` methods without parameters are on `TRestClientUri`. Server-side code should use `TRestBatch` directly.

### 13.7.2. Transaction Handling

```pascal
const
  SESSION_ID = 1;  // Current session ID
begin
  // Automatic transactions per batch
  Server.TransactionBegin(TOrmCustomer, SESSION_ID);
  try
    Server.Orm.Add(Customer1, True);
    Server.Orm.Add(Customer2, True);
    Server.Commit(SESSION_ID, True);  // RaiseException=True
  except
    Server.RollBack(SESSION_ID);
    raise;
  end;
end;
```

**Note**: Transaction methods require a `SessionID` parameter on the server.

### 13.7.3. Write Acknowledgment

```pascal
// Control write confirmation
Server.AcquireWriteMode := amLocked;       // Wait for write completion
Server.AcquireWriteMode := amUnlocked;     // Fire and forget (faster)
Server.AcquireWriteMode := amBackgroundThread;  // Queue to background
```

---

## 13.8. Static Storage

### 13.8.1. In-Memory Tables

```pascal
uses
  mormot.orm.storage,
  mormot.rest.memserver;

// Method 1: Use OrmMapInMemory helper (recommended)
OrmMapInMemory(Server.OrmInstance, TOrmCache);

// Method 2: Use StaticDataCreate helper with persistence
Storage := StaticDataCreate(Server.OrmInstance, TOrmCache, 'cache.json');

// Method 3: Manual creation with StaticTableSetup
Storage := TRestStorageInMemory.Create(TOrmCache, Server.OrmInstance);
TRestOrmServer(Server.OrmInstance).StaticTableSetup(
  Server.Model.GetTableIndex(TOrmCache), Storage, sStaticDataTable);
```

### 13.8.2. Persistence Options

```pascal
// JSON persistence
Storage := TRestStorageInMemory.Create(TOrmCache, Server);
Storage.FileName := 'cache.json';

// Binary persistence (faster, smaller)
Storage.BinaryFile := True;
Storage.FileName := 'cache.data';

// Manual save/load
Storage.SaveToFile('backup.json');
Storage.LoadFromFile('backup.json');
```

### 13.8.3. Static vs Virtual

| Feature | Static | Virtual |
|---------|--------|---------|
| SQL JOINs | No | Yes |
| Speed | Faster | Slightly slower |
| Memory | Dedicated | Shared with SQLite3 |
| Use case | Simple CRUD | Complex queries |

---

## 13.9. Performance Monitoring

### 13.9.1. Server Statistics

```pascal
// Enable monitoring
Server.CreateMissingTables;

// Access statistics
WriteLn('Total requests: ', Server.Stats.TotalRequestCount);
WriteLn('Success: ', Server.Stats.SuccessRequestCount);
WriteLn('Errors: ', Server.Stats.ErrorRequestCount);
WriteLn('Avg response time: ', Server.Stats.AverageResponseTime, ' ms');
```

### 13.9.2. Per-Table Statistics

```pascal
for i := 0 to Server.Model.TablesMax do
begin
  Stats := Server.Stats[i];
  if Stats <> nil then
    WriteLn(Server.Model.Tables[i].SqlTableName, ': ',
      Stats.SelectCount, ' reads, ', Stats.InsertCount, ' inserts');
end;
```

### 13.9.3. SQL Execution Logging

```pascal
// Enable SQL logging
TSynLog.Add.Level := [sllSQL, sllDB];

// Or specific callback
Server.OnSqlExecute := procedure(const SQL: RawUtf8; const TimeMS: Int64)
begin
  if TimeMS > 100 then  // Log slow queries
    WriteLn('SLOW QUERY (', TimeMS, 'ms): ', SQL);
end;
```

---

## 13.10. Custom ORM Extensions

### 13.10.1. Custom SQL Functions

```pascal
// Register custom SQLite3 function
Server.DB.RegisterSQLFunction(
  procedure(Context: TSqlite3FunctionContext; argc: Integer;
    var argv: TSqlite3ValueArray)
  begin
    // Custom function implementation
    sqlite3.result_int64(Context, CalculateHash(argv[0]));
  end,
  'MYHASH', 1);

// Use in queries
Server.Orm.ExecuteList(TOrmCustomer,
  'SELECT * FROM Customer WHERE MYHASH(Name) = ?', [HashValue]);
```

### 13.10.2. Computed Fields

```pascal
type
  TOrmOrder = class(TOrm)
  private
    fQuantity: Integer;
    fUnitPrice: Currency;
    fTotal: Currency;
  protected
    procedure ComputeFieldsBeforeWrite(const aRest: IRestOrm;
      aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog = 0); override;
  published
    property Quantity: Integer read fQuantity write fQuantity;
    property UnitPrice: Currency read fUnitPrice write fUnitPrice;
    property Total: Currency read fTotal write fTotal stored False;  // Computed
  end;

procedure TOrmOrder.ComputeFieldsBeforeWrite(const aRest: IRestOrm;
  aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog);
begin
  inherited;
  fTotal := fQuantity * fUnitPrice;
end;
```

---

## 13.11. Migration from mORMot 1

### 13.11.1. ORM Access Change

```pascal
// mORMot 1: Direct access
Server.Add(Customer, True);
Server.Retrieve(123, Customer);

// mORMot 2: Via Orm property
Server.Orm.Add(Customer, True);
Server.Orm.Retrieve(123, Customer);
```

### 13.11.2. Event Method Changes

```pascal
// mORMot 1: Virtual methods to override
procedure TSQLRestServerDB.BeforeAdd;
procedure TSQLRestServerDB.AfterAdd;

// mORMot 2: Callback-based events (see Section 13.4)
TRestOrmServer(Server.OrmInstance).OnUpdateEvent := MyEventHandler;
// For validation, use TOrm.ComputeFieldsBeforeWrite override
```

### 13.11.3. Static Storage Registration

```pascal
// mORMot 1
Server.StaticDataCreate(TOrmCache, '', False, True);

// mORMot 2: Use OrmMapInMemory or StaticDataCreate
OrmMapInMemory(Server.OrmInstance, TOrmCache);
// Or with file persistence:
StaticDataCreate(Server.OrmInstance, TOrmCache, 'cache.json', False);
```

---

*Next Chapter: Method-Based Services*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 12: Client-Server ORM Operations](mORMot2-SAD-Chapter-12.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 14: Client-Server Services via Methods](mORMot2-SAD-Chapter-14.md) |
