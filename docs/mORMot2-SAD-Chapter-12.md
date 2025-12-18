# 12. Client-Server ORM Operations

*Remote Data Access*

This chapter covers how to perform ORM operations over the network, including remote CRUD, caching, batch operations, and synchronization.

---

## 12.1. Remote ORM Basics

### 12.1.1. Transparent Remote Access

The ORM works identically whether local or remote:

```pascal
// Local (in-process)
Client := TRestClientDB.Create(Model, nil, 'data.db3', TRestServerDB);

// Remote (HTTP)
Client := TRestHttpClientWinHttp.Create('server', '8080', Model);

// Same ORM API for both
Client.Orm.Add(Customer, True);
Client.Orm.Retrieve(123, Customer);
Client.Orm.Update(Customer);
Client.Orm.Delete(TOrmCustomer, 123);
```

### 12.1.2. Authentication Required

Remote ORM operations require authentication:

```pascal
var
  Client: TRestHttpClient;
begin
  Client := TRestHttpClientWinHttp.Create('localhost', '8080', Model);
  try
    // Must authenticate first
    if not Client.SetUser('username', 'password') then
      raise Exception.Create('Authentication failed');

    // Now ORM operations work
    Client.Orm.Add(Customer, True);
  finally
    Client.Free;
  end;
end;
```

---

## 12.2. CRUD Operations Over Network

### 12.2.1. Create (Add)

```pascal
var
  Customer: TOrmCustomer;
  ID: TID;
begin
  Customer := TOrmCustomer.Create;
  try
    Customer.Name := 'ACME Corp';
    Customer.Email := 'contact@acme.com';

    // Remote add - returns new ID
    ID := Client.Orm.Add(Customer, True);

    if ID > 0 then
      WriteLn('Created customer #', ID)
    else
      WriteLn('Failed to create customer');
  finally
    Customer.Free;
  end;
end;
```

### 12.2.2. Read (Retrieve)

```pascal
var
  Customer: TOrmCustomer;
begin
  // By ID
  Customer := TOrmCustomer.Create(Client.Orm, 123);
  try
    WriteLn(Customer.Name);
  finally
    Customer.Free;
  end;

  // Alternative: Retrieve into existing instance
  Customer := TOrmCustomer.Create;
  try
    if Client.Orm.Retrieve(123, Customer) then
      WriteLn(Customer.Name);
  finally
    Customer.Free;
  end;

  // By unique field
  Customer := TOrmCustomer.Create;
  try
    if Client.Orm.Retrieve('Email = ?', [], ['contact@acme.com'], Customer) then
      WriteLn(Customer.Name);
  finally
    Customer.Free;
  end;
end;
```

### 12.2.3. Update

```pascal
var
  Customer: TOrmCustomer;
begin
  Customer := TOrmCustomer.Create(Client.Orm, 123);
  try
    Customer.Email := 'new@acme.com';

    if Client.Orm.Update(Customer) then
      WriteLn('Updated successfully');
  finally
    Customer.Free;
  end;

  // Update specific fields only
  Customer := TOrmCustomer.Create;
  try
    Customer.ID := 123;
    Customer.Email := 'updated@acme.com';

    // Only update Email field
    Client.Orm.Update(Customer, 'Email');
  finally
    Customer.Free;
  end;
end;
```

### 12.2.4. Delete

```pascal
// By ID
Client.Orm.Delete(TOrmCustomer, 123);

// By condition
Client.Orm.Delete(TOrmCustomer, 'Status = ?', [Ord(csInactive)]);
```

---

## 12.3. Query Operations

### 12.3.1. FillPrepare Pattern

Efficient iteration over remote results:

```pascal
var
  Customer: TOrmCustomer;
begin
  Customer := TOrmCustomer.CreateAndFillPrepare(Client.Orm,
    'Country = ?', ['USA']);
  try
    while Customer.FillOne do
      WriteLn(Customer.Name, ': ', Customer.Email);
  finally
    Customer.Free;
  end;
end;
```

### 12.3.2. Select Specific Fields

Reduce network traffic by requesting only needed fields:

```pascal
// Only transfer Name and Email fields
Customer := TOrmCustomer.CreateAndFillPrepare(Client.Orm,
  'Country = ?', ['USA'],
  'Name,Email');  // Fields to retrieve
```

### 12.3.3. RetrieveList

Get results as object list:

```pascal
var
  List: TObjectList<TOrmCustomer>;
  Customer: TOrmCustomer;
begin
  List := Client.Orm.RetrieveList<TOrmCustomer>(
    'Country = ?', ['USA']);
  try
    for Customer in List do
      WriteLn(Customer.Name);
  finally
    List.Free;
  end;
end;
```

### 12.3.4. Direct JSON Results

```pascal
var
  Json: RawUtf8;
begin
  // Get results directly as JSON array
  Json := Client.Orm.RetrieveListJson(TOrmCustomer,
    'Country = ?', ['USA'], 'Name,Email');
  // Result: [{"Name":"ACME","Email":"..."},...]
end;
```

---

## 12.4. Client-Side Caching

### 12.4.1. Enable Table Caching

```pascal
// Cache specific tables on client
Client.Cache.SetCache(TOrmCustomer);   // Cache all customers
Client.Cache.SetCache(TOrmProduct);    // Cache all products

// With timeout (milliseconds)
Client.Cache.SetTimeOut(TOrmCustomer, 60000);  // 1 minute cache
```

> **Note**: `Cache` is actually a function that returns a `TOrmCache` instance, not a property. However, it can be called repeatedly without overhead as it returns the same instance. This design allows the cache to be lazily initialized only when needed.

### 12.4.2. How Caching Works

```
┌─────────────────────────────────────────────────────────────┐
│  Client.Orm.Retrieve(123, Customer)                         │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
                  ┌─────────────────┐
                  │ Check Cache     │
                  └────────┬────────┘
                           │
            ┌──────────────┴──────────────┐
            │                             │
            ▼                             ▼
    ┌───────────────┐            ┌───────────────┐
    │ Cache Hit     │            │ Cache Miss    │
    │ Return cached │            │ Fetch from    │
    │ data          │            │ server        │
    └───────────────┘            └───────┬───────┘
                                         │
                                         ▼
                                 ┌───────────────┐
                                 │ Update cache  │
                                 │ Return data   │
                                 └───────────────┘
```

### 12.4.3. Cache Invalidation

```pascal
// Clear specific record from cache
Client.Cache.NotifyDeletion(TOrmCustomer, 123);

// Reset table cache (re-enable caching, which clears existing entries)
Client.Cache.SetCache(TOrmCustomer);

// Clear all caches and reset settings
Client.Cache.Clear;
```

### 12.4.4. Server-Side Cache Notification

The server can track changes via `TRestOrmServer.OnUpdateEvent`:

```pascal
type
  TMyServer = class
    function HandleUpdate(Sender: TRestServer; Event: TOrmEvent;
      aTable: TOrmClass; const aID: TID;
      const aSentData: RawUtf8): boolean;
  end;

// Server-side: Enable change notifications
// OnUpdateEvent requires a method of object (not anonymous procedure)
(Server.OrmInstance as TRestOrmServer).OnUpdateEvent := MyServer.HandleUpdate;
```

---

## 12.5. Batch Operations

### 12.5.1. TRestBatch for Bulk Operations

Dramatically improve performance for bulk operations:

```pascal
uses
  mormot.orm.core;

var
  Batch: TRestBatch;
  Customer: TOrmCustomer;
  Results: TIDDynArray;
  i: Integer;
begin
  Batch := TRestBatch.Create(Client.Orm, TOrmCustomer, 1000);
  try
    for i := 1 to 10000 do
    begin
      Customer := TOrmCustomer.Create;
      Customer.Name := FormatUtf8('Customer %', [i]);
      Customer.Email := FormatUtf8('cust%@example.com', [i]);
      Batch.Add(Customer, True);  // Batch owns Customer
    end;

    // Single network roundtrip for all 10000 records
    if Client.Orm.BatchSend(Batch, Results) = HTTP_SUCCESS then
      WriteLn('Inserted ', Length(Results), ' records');
  finally
    Batch.Free;
  end;
end;
```

### 12.5.2. Performance Comparison

| Operation | Individual | Batch | Improvement |
|-----------|------------|-------|-------------|
| 10,000 inserts (local) | 10 sec | 0.1 sec | 100x |
| 10,000 inserts (network) | 100 sec | 1 sec | 100x |
| 10,000 updates | 15 sec | 0.2 sec | 75x |

### 12.5.3. Batch Updates and Deletes

```pascal
var
  Batch: TRestBatch;
  Customer: TOrmCustomer;
begin
  Batch := TRestBatch.Create(Client.Orm, TOrmCustomer);
  try
    // Mix operations in single batch
    Customer := TOrmCustomer.Create;
    Customer.Name := 'New Customer';
    Batch.Add(Customer, True);

    Customer := TOrmCustomer.Create(Client.Orm, 123);
    Customer.Status := csActive;
    Batch.Update(Customer);

    Batch.Delete(TOrmCustomer, 456);

    Client.Orm.BatchSend(Batch);
  finally
    Batch.Free;
  end;
end;
```

### 12.5.4. Automatic Batching

For transparent batching:

```pascal
var
  Results: TIDDynArray;
begin
  // Start automatic batching
  Client.BatchStart(TOrmCustomer, 1000);
  try
    for i := 1 to 10000 do
    begin
      Customer := TOrmCustomer.Create;
      Customer.Name := FormatUtf8('Customer %', [i]);
      Client.Orm.Add(Customer, True);  // Automatically batched
      Customer.Free;
    end;
  finally
    Client.BatchSend(Results);  // Send remaining batch
  end;
end;
```

---

## 12.6. BLOB Handling

### 12.6.1. BLOB Fields

BLOBs are not included in normal record transfers:

```pascal
type
  TOrmDocument = class(TOrm)
  private
    fName: RawUtf8;
    fContent: RawBlob;  // Not transferred with other fields
  published
    property Name: RawUtf8 read fName write fName;
    property Content: RawBlob read fContent write fContent;
  end;
```

### 12.6.2. Upload BLOB

```pascal
var
  Doc: TOrmDocument;
  Data: RawBlob;
begin
  Doc := TOrmDocument.Create;
  try
    Doc.Name := 'report.pdf';
    Client.Orm.Add(Doc, True);

    // Upload BLOB separately
    Data := StringFromFile('report.pdf');
    Client.Orm.UpdateBlob(TOrmDocument, Doc.ID, 'Content', Data);
  finally
    Doc.Free;
  end;
end;
```

### 12.6.3. Download BLOB

```pascal
var
  Data: RawBlob;
begin
  if Client.Orm.RetrieveBlob(TOrmDocument, DocID, 'Content', Data) then
    FileFromString(Data, 'downloaded.pdf');
end;
```

### 12.6.4. Streaming BLOBs

For large files, use streaming with `RetrieveBlob` overloads:

```pascal
// Overload 1: Provide your own stream (caller owns it)
var
  Stream: TStream;
begin
  Stream := TFileStream.Create('large_file.zip', fmCreate);
  try
    if Client.Orm.RetrieveBlob(TOrmDocument, DocID, 'Content', Stream) then
      // Stream now contains the BLOB data
  finally
    Stream.Free;  // Caller must free
  end;
end;

// Overload 2: Let mORMot create a memory stream (caller must free)
var
  MemStream: TCustomMemoryStream;
begin
  if Client.Orm.RetrieveBlob(TOrmDocument, DocID, 'Content', MemStream) then
  try
    // MemStream created by mORMot, contains BLOB data
    ProcessStream(MemStream);
  finally
    MemStream.Free;  // IMPORTANT: Caller must free the returned stream
  end;
end;
```

> **Note**: When using the `out TCustomMemoryStream` overload, mORMot creates the stream instance, but the **caller is responsible for freeing it**. The stream is allocated only if the function returns `True`.

---

## 12.7. Locking and Concurrency

### 12.7.1. Optimistic Locking

Default behavior - last write wins:

```pascal
// Two clients modify same record
Client1.Orm.Retrieve(123, Customer1);
Client2.Orm.Retrieve(123, Customer2);

Customer1.Email := 'client1@example.com';
Customer2.Email := 'client2@example.com';

Client1.Orm.Update(Customer1);  // Succeeds
Client2.Orm.Update(Customer2);  // Overwrites Client1's change
```

### 12.7.2. TRecordVersion for Change Tracking

```pascal
type
  TOrmCustomer = class(TOrm)
  private
    fName: RawUtf8;
    fVersion: TRecordVersion;  // Auto-incremented on changes
  published
    property Name: RawUtf8 read fName write fName;
    property Version: TRecordVersion read fVersion write fVersion;
  end;

// Server automatically increments Version on each update
```

### 12.7.3. Conflict Detection

```pascal
var
  Customer: TOrmCustomer;
  OriginalVersion: TRecordVersion;
  CurrentVersion: Int64;
begin
  Customer := TOrmCustomer.Create(Client.Orm, 123);
  try
    OriginalVersion := Customer.Version;

    // Make changes
    Customer.Name := 'Updated Name';

    // Re-fetch to check version using OneFieldValueInt64
    // Note: takes WhereClause as RawUtf8, not array of const
    CurrentVersion := Client.Orm.OneFieldValueInt64(TOrmCustomer,
      'Version', FormatUtf8('ID = %', [Customer.ID]));
    if CurrentVersion <> Int64(OriginalVersion) then
      raise Exception.Create('Record modified by another user');

    Client.Orm.Update(Customer);
  finally
    Customer.Free;
  end;
end;
```

---

## 12.8. Synchronization

### 12.8.1. Master-Slave Replication

```pascal
// On slave: One-shot sync from master
var
  MasterClient: TRestHttpClient;
  SlaveServer: TRestServerDB;
  MasterOrm: IRestOrmServer;
begin
  MasterClient := TRestHttpClientWinHttp.Create('master', '8080', Model);
  SlaveServer := TRestServerDB.Create(Model, 'slave.db3');

  // One-shot sync (var parameter requires IRestOrmServer)
  MasterOrm := MasterClient.Orm as IRestOrmServer;
  SlaveServer.RecordVersionSynchronizeSlave(
    TOrmCustomer, MasterOrm, nil, 1000);  // 1000 = MaxRowsInOneBatch
end;

// For continuous synchronization (recommended):
SlaveServer.RecordVersionSynchronizeSlaveStart(
  TOrmCustomer, MasterClient.Orm, 1000);  // Check every 1000ms
```

### 12.8.2. Change Tracking

mORMot2 provides `TOrmHistory` in `mormot.orm.core` for tracking record changes:

```pascal
// TOrmHistory is defined in mormot.orm.core with fields for:
// - ModifiedRecord: TID (reference to modified record)
// - Event: TOrmHistoryEvent
// - SentDataJson: RawBlob
// - Timestamp: TModTime

// Server tracks all changes via TRestOrmServer
var
  OrmServer: TRestOrmServer;
begin
  OrmServer := Server.OrmInstance as TRestOrmServer;
  OrmServer.TrackChanges([TOrmCustomer, TOrmOrder]);
end;
```

---

## 12.9. Error Handling

### 12.9.1. Check Return Values

```pascal
var
  ID: TID;
begin
  ID := Client.Orm.Add(Customer, True);
  if ID = 0 then
  begin
    // Check last error
    WriteLn('Error: ', Client.LastErrorMessage);
    WriteLn('Code: ', Client.LastErrorCode);
  end;
end;
```

### 12.9.2. HTTP Status Codes

| Code | Meaning | mORMot Constant |
|------|---------|-----------------|
| 200 | Success | `HTTP_SUCCESS` |
| 201 | Created | `HTTP_CREATED` |
| 400 | Bad Request | `HTTP_BADREQUEST` |
| 401 | Unauthorized | `HTTP_UNAUTHORIZED` |
| 403 | Forbidden | `HTTP_FORBIDDEN` |
| 404 | Not Found | `HTTP_NOTFOUND` |
| 500 | Server Error | `HTTP_SERVERERROR` |

### 12.9.3. Connection Resilience

```pascal
// Auto-reconnect on connection loss
Client.RetryOnceOnTimeout := True;

// Custom retry logic
for Attempt := 1 to 3 do
begin
  if Client.Orm.Retrieve(ID, Customer) then
    Break;
  if Attempt = 3 then
    raise Exception.Create('Failed after 3 attempts');
  Sleep(1000 * Attempt);  // Exponential backoff
end;
```

---

## 12.10. Migration from mORMot 1

### 12.10.1. ORM Access Pattern

```pascal
// mORMot 1: Direct on TRest
Client.Add(Customer, True);
Client.Retrieve(123, Customer);

// mORMot 2: Via Orm interface
Client.Orm.Add(Customer, True);
Client.Orm.Retrieve(123, Customer);
```

### 12.10.2. Batch Changes

```pascal
// mORMot 1
Client.BatchStart(TOrmCustomer);
Client.BatchAdd(Customer);
Client.BatchSend;

// mORMot 2
Batch := TRestBatch.Create(Client.Orm, TOrmCustomer);
Batch.Add(Customer, True);
Client.Orm.BatchSend(Batch);
```

---

*Next Chapter: Server-Side ORM Processing*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 11: Client-Server Architecture](mORMot2-SAD-Chapter-11.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 13: Server-Side ORM Processing](mORMot2-SAD-Chapter-13.md) |
