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
Model.Props[TOrmCache].SetStorage(TRestStorageInMemory);
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

### 13.4.1. OnBefore* Events

Execute before ORM operations:

```pascal
type
  TMyServer = class(TRestServerDB)
  protected
    function OnBeforeAdd(Table: TOrmClass; const Rec: TOrm): Boolean; override;
  end;

function TMyServer.OnBeforeAdd(Table: TOrmClass; const Rec: TOrm): Boolean;
begin
  // Validate before insert
  if Table = TOrmCustomer then
    if TOrmCustomer(Rec).Email = '' then
    begin
      Result := False;  // Reject insert
      Exit;
    end;
  Result := True;  // Allow insert
end;
```

### 13.4.2. OnAfter* Events

Execute after ORM operations:

```pascal
procedure TMyServer.OnAfterDelete(Table: TOrmClass; const aID: TID);
begin
  // Audit trail
  LogEvent(Format('Deleted %s #%d', [Table.ClassName, aID]));

  // Cascade operations
  if Table = TOrmCustomer then
    Orm.Delete(TOrmOrder, 'CustomerID = ?', [aID]);
end;
```

### 13.4.3. Event Signatures

| Event | Signature | Return |
|-------|-----------|--------|
| `OnBeforeAdd` | `(Table, Rec): Boolean` | False = reject |
| `OnAfterAdd` | `(Table, aID)` | - |
| `OnBeforeUpdate` | `(Table, Rec): Boolean` | False = reject |
| `OnAfterUpdate` | `(Table, Rec)` | - |
| `OnBeforeDelete` | `(Table, aID): Boolean` | False = reject |
| `OnAfterDelete` | `(Table, aID)` | - |

### 13.4.4. TOrm Event Methods

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

```pascal
// Restrict access to specific tables
Server.OnCanExecute := function(Sender: TRest; Context: TRestServerUriContext;
  Table: TOrmClass; const TableID: TID): Boolean
begin
  // Only admins can access TOrmSettings
  if Table = TOrmSettings then
    Result := Context.Session.User.GroupRights.HasRight(arAdmin)
  else
    Result := True;
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

Force additional conditions on all queries:

```pascal
// All Customer queries filtered by tenant
Server.OnBeforeUriExecute := procedure(Sender: TRest; var SqlWhere: RawUtf8;
  Table: TOrmClass)
begin
  if Table = TOrmCustomer then
  begin
    if SqlWhere <> '' then
      SqlWhere := SqlWhere + ' AND ';
    SqlWhere := SqlWhere + FormatUtf8('TenantID = %', [CurrentTenantID]);
  end;
end;
```

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

```pascal
WriteLn('Cache hits: ', Server.Cache.CacheHits);
WriteLn('Cache misses: ', Server.Cache.CacheMisses);
WriteLn('Hit ratio: ', Server.Cache.CacheHits /
  (Server.Cache.CacheHits + Server.Cache.CacheMisses) * 100:0:1, '%');
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
  mormot.orm.storage;

// Register before server creation
Model.Props[TOrmCache].SetStorage(TRestStorageInMemory);

// Or add after server creation
Storage := TRestStorageInMemory.Create(TOrmCache, Server);
Server.StaticDataAdd(Storage);
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
// mORMot 1
procedure TSQLRestServerDB.BeforeAdd;
procedure TSQLRestServerDB.AfterAdd;

// mORMot 2
function TRestServerDB.OnBeforeAdd: Boolean;
procedure TRestServerDB.OnAfterAdd;
```

### 13.11.3. Static Storage Registration

```pascal
// mORMot 1
Server.StaticDataCreate(TOrmCache, '', False, True);

// mORMot 2
Model.Props[TOrmCache].SetStorage(TRestStorageInMemory);
```

---

*Next Chapter: Method-Based Services*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 12: Client-Server ORM Operations](mORMot2-SAD-Chapter-12.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 14: Client-Server Services via Methods](mORMot2-SAD-Chapter-14.md) |
