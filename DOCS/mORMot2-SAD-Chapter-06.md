# 6. Daily ORM

*Practical Patterns for Everyday Use*

When comparing ORM to raw SQL, several advantages stand out:

- **No field order concerns**: Access properties directly with IDE completion
- **Readable code**: No context-switching between Pascal and SQL syntax
- **Naming consistency**: Refactoring table/field names is handled by the compiler
- **Type safety**: Compile-time checking prevents runtime type mismatches
- **Database agnostic**: Same code works across SQLite3, PostgreSQL, MongoDB, etc.

This chapter covers practical patterns for daily ORM usage in mORMot 2.

---

## 6.1. ORM is Not Just Database

The ORM should not be thought of as simply mapping an existing database schema. Instead:

- **Think objects, not tables**: Use high-level types, not just text/numbers
- **Think logical units, not Master/Detail**: Group related data in single records
- **Think classes, not SQL**: Design your domain first
- **Think "What data do I need?", not "How will I store it?"**

### 6.1.1. Objects, Not Tables

With an ORM, you often define **fewer tables** than in traditional RDBMS design. Use `TDocVariant`, dynamic arrays, `TCollection`, or `TPersistent` properties to store nested data within a single record:

```pascal
type
  TOrmInvoice = class(TOrm)
  private
    fCustomerName: RawUtf8;
    fLines: Variant;  // TDocVariant array of line items
    fMetadata: Variant;  // TDocVariant object for flexible data
  published
    property CustomerName: RawUtf8 read fCustomerName write fCustomerName;
    property Lines: Variant read fLines write fLines;
    property Metadata: Variant read fMetadata write fMetadata;
  end;

// Usage - no separate InvoiceLine table needed
Invoice.Lines := _JsonFast('[
  {"product":"Widget","qty":10,"price":9.99},
  {"product":"Gadget","qty":5,"price":19.99}
]');
Invoice.Metadata := _ObjFast(['region', 'US', 'priority', 'high']);
```

### 6.1.2. Methods, Not SQL

**Anti-pattern** (direct SQL):

```pascal
// DON'T do this
Server.DB.Execute('CREATE TABLE IF NOT EXISTS drives...');
Server.DB.Execute('INSERT OR IGNORE INTO drives (drive) VALUES ("A:")');
```

**Correct ORM approach**:

```pascal
// DO this
Server.Server.CreateMissingTables;  // Creates tables from model

if Server.Orm.TableRowCount(TOrmDrive) = 0 then
begin
  Drive := TOrmDrive.Create;
  try
    for C := 'A' to 'Z' do
    begin
      Drive.Letter := C;
      Server.Orm.Add(Drive, True);
    end;
  finally
    Drive.Free;
  end;
end;
```

---

## 6.2. Working with Objects

### 6.2.1. CRUD Operations

The fundamental pattern uses `Add/Retrieve/Update/Delete` methods:

```pascal
uses
  mormot.orm.core;

procedure CrudExample(const Orm: IRestOrm);
var
  Baby: TOrmBaby;
  ID: TID;
begin
  // CREATE - Add a new record
  Baby := TOrmBaby.Create;
  try
    Baby.Name := 'Smith';
    Baby.Address := 'New York City';
    Baby.BirthDate := Date;
    Baby.Sex := sMale;
    ID := Orm.Add(Baby, True);  // True = include all fields
  finally
    Baby.Free;
  end;

  // RETRIEVE - Load by ID
  Baby := TOrmBaby.Create(Orm, ID);  // Constructor loads the record
  try
    Assert(Baby.Name = 'Smith');

    // UPDATE - Modify and save
    Baby.Name := 'Smythe';
    Orm.Update(Baby);
  finally
    Baby.Free;
  end;

  // Alternative RETRIEVE into existing instance
  Baby := TOrmBaby.Create;
  try
    if Orm.Retrieve(ID, Baby) then
      WriteLn('Found: ', Baby.Name);
  finally
    Baby.Free;
  end;

  // DELETE - Remove by ID
  Orm.Delete(TOrmBaby, ID);
end;
```

### 6.2.2. Reusing Instances

A single `TOrm` instance can be reused for multiple operations:

```pascal
var
  Baby: TOrmBaby;
begin
  Baby := TOrmBaby.Create;
  try
    // Add first record
    Baby.Name := 'Alice';
    Orm.Add(Baby, True);

    // Reuse for second record
    Baby.ClearProperties;  // Reset fields
    Baby.Name := 'Bob';
    Orm.Add(Baby, True);
  finally
    Baby.Free;
  end;
end;
```

---

## 6.3. Queries

### 6.3.1. FillPrepare / FillOne Pattern

The most efficient way to iterate through query results:

```pascal
var
  Baby: TOrmBaby;
begin
  Baby := TOrmBaby.CreateAndFillPrepare(Orm,
    'Name LIKE ? AND Sex = ?', ['A%', Ord(sMale)]);
  try
    while Baby.FillOne do
      DoSomethingWith(Baby);  // Process each matching record
  finally
    Baby.Free;
  end;
end;
```

**Key benefits**:
- Single instance reused for all rows
- No separate `TOrmTable` handling needed
- Minimal memory allocation

### 6.3.2. Selecting Specific Fields

Save bandwidth by specifying only needed fields:

```pascal
// Load only Name and BirthDate fields
Baby := TOrmBaby.CreateAndFillPrepare(Orm,
  'Sex = ?', [Ord(sFemale)],
  'Name,BirthDate');  // aCustomFieldsCSV parameter
try
  while Baby.FillOne do
    WriteLn(Baby.Name, ': ', DateToStr(Baby.BirthDate));
finally
  Baby.Free;
end;
```

**Warning**: After partial field retrieval, calling `Orm.Update(Baby)` will only update the retrieved fields, not the entire record.

### 6.3.3. Query Parameters

Parameters are bound using `?` placeholders:

```pascal
// String and integer parameters
Baby.CreateAndFillPrepare(Orm,
  'Name LIKE ? AND Sex = ?',
  ['A%', Ord(sMale)]);

// Date parameters - use DateToSql/DateTimeToSql
Baby.CreateAndFillPrepare(Orm,
  'BirthDate >= ?',
  [DateToSql(EncodeDate(2020, 1, 1))]);

// Building complex WHERE clauses
var
  Where: RawUtf8;
begin
  Where := FormatUtf8('ID >= ?', [], [MinID]);
  if OnlyActive then
    Where := FormatUtf8('% AND Active = ?', [Where], [True]);
  if not Category.IsEmpty then
    Where := FormatUtf8('% AND Category = ?', [Where], [Category]);

  Baby := TOrmBaby.CreateAndFillPrepare(Orm, Where);
end;
```

### 6.3.4. IList<T> Alternative

For simpler code using mORMot2's generic interface (from `mormot.core.collections`):

```pascal
var
  List: IList<TOrmBaby>;
  Baby: TOrmBaby;
begin
  if Orm.RetrieveIList(TOrmBaby, List, 'Name,Sex,BirthDate') then
    for Baby in List do
      DoSomethingWith(Baby);
  // IList is reference-counted, no Free needed
end;
```

**Trade-off**: Creates all instances at once vs. `FillPrepare`'s single-instance reuse.

> **Note**: mORMot2 also provides `RetrieveList()` returning a non-generic `TObjectList` if you prefer that pattern.

### 6.3.5. TOrmTable for Raw Results

Direct access to query results as a table:

```pascal
var
  Table: TOrmTable;
  Row: Integer;
begin
  // ExecuteList takes SQL only (no bounds parameter) - use FormatUtf8 for parameters
  Table := Orm.ExecuteList([TOrmBaby],
    FormatUtf8('SELECT ID, Name, BirthDate FROM Baby WHERE Sex = ?', [], [Ord(sMale)]));
  try
    for Row := 1 to Table.RowCount do
      WriteLn(
        'ID=', Table.GetAsInteger(Row, 0),
        ' Name=', Table.GetU(Row, 1),
        ' Born=', Table.GetU(Row, 2));
  finally
    Table.Free;
  end;
end;
```

Or using cursor-style `Step`:

```pascal
Table := Orm.MultiFieldValues(TOrmBaby, 'ID,Name',
  'Sex = ?', [Ord(sMale)]);
try
  while Table.Step do
    WriteLn('ID=', Table.Field(0), ' Name=', Table.Field(1));
finally
  Table.Free;
end;
```

### 6.3.6. Late-Binding Variant Access

For convenient but slower access:

```pascal
var
  Baby: Variant;
begin
  with Orm.MultiFieldValues(TOrmBaby, 'ID,Name,BirthDate', 'Sex = ?', [Ord(sMale)]) do
  try
    while Step(False, @Baby) do
      WriteLn('ID=', Baby.ID, ' Name=', Baby.Name);
  finally
    Free;
  end;
end;
```

---

## 6.4. Helper Methods

### 6.4.1. Single-Value Retrieval

```pascal
// Get one field value (returns RawUtf8)
var
  Name: RawUtf8;
begin
  Name := Orm.OneFieldValue(TOrmBaby, 'Name', 'ID = ?', [], [123]);
  if Name <> '' then
    WriteLn('Found: ', Name);
end;

// Get count - use OneFieldValueInt64 for integer results
var
  Count: Int64;
begin
  Count := Orm.TableRowCount(TOrmBaby);
  WriteLn('Total babies: ', Count);

  // OneFieldValueInt64 takes WhereClause without bounds - use FormatUtf8
  Count := Orm.OneFieldValueInt64(TOrmBaby, 'COUNT(*)',
    FormatUtf8('Sex = ?', [], [Ord(sMale)]));
  WriteLn('Male babies: ', Count);
end;

// Alternative: OneFieldValue with out parameter for Int64
var
  Count: Int64;
begin
  if Orm.OneFieldValue(TOrmBaby, 'COUNT(*)', 'Sex = ?', [], [Ord(sMale)], Count) then
    WriteLn('Male babies: ', Count);
end;
```

### 6.4.2. Multiple Values to Dynamic Array

```pascal
var
  IDs: TInt64DynArray;  // Note: mORMot2 uses TInt64DynArray for integer fields
  Names: TRawUtf8DynArray;
  i: Integer;
begin
  // Get all IDs matching criteria
  // OneFieldValues takes WhereClause directly - use FormatUtf8 for parameters
  Orm.OneFieldValues(TOrmBaby, 'ID',
    FormatUtf8('Sex = ?', [], [Ord(sFemale)]), IDs);

  // Get names into array
  Orm.OneFieldValues(TOrmBaby, 'Name', '', Names);
  for i := 0 to High(Names) do
    WriteLn(Names[i]);
end;
```

### 6.4.3. Existence Check

```pascal
if Orm.Retrieve('Email = ?', [], [Email], Customer) then
  WriteLn('Customer exists: ', Customer.Name)
else
  WriteLn('Not found');
```

---

## 6.5. Automatic Memory Management

### 6.5.1. AutoFree Pattern

Avoid manual `try..finally` blocks:

```pascal
function CreateNewBaby(const Orm: IRestOrm; const Name: RawUtf8): TID;
var
  Baby: TOrmBaby;
begin
  TOrmBaby.AutoFree(Baby);  // Auto-releases at end of function
  Baby.Name := Name;
  Baby.BirthDate := Date;
  Result := Orm.Add(Baby, True);
end;  // Baby automatically freed here
```

### 6.5.2. AutoFree with Query

```pascal
var
  Baby: TOrmBaby;
begin
  TOrmBaby.AutoFree(Baby, Orm, 'Name LIKE ?', ['A%']);
  while Baby.FillOne do
    ProcessBaby(Baby);
end;  // Baby automatically freed
```

### 6.5.3. FPC Compatibility

With FPC, assign the result to a local `IAutoFree` variable:

```pascal
var
  Baby: TOrmBaby;
  Auto: IAutoFree;  // Required for FPC
begin
  Auto := TOrmBaby.AutoFree(Baby, Orm, 'Name LIKE ?', ['A%']);
  while Baby.FillOne do
    ProcessBaby(Baby);
end;
```

---

## 6.6. Object Relationships

### 6.6.1. One-to-One / One-to-Many

Use `TOrm` published properties (storing IDs, not instances):

```pascal
type
  TOrmFileInfo = class(TOrm)
  published
    property FileDate: TDateTime read fFileDate write fFileDate;
    property FileSize: Int64 read fFileSize write fFileSize;
  end;

  TOrmFile = class(TOrm)
  published
    property FileName: RawUtf8 read fFileName write fFileName;
    property Info: TOrmFileInfo read fInfo write fInfo;  // Foreign key
  end;

// Creating linked records
Info := TOrmFileInfo.Create;
MyFile := TOrmFile.Create;
try
  Info.FileDate := Now;
  Info.FileSize := 12345;
  Orm.Add(Info, True);

  MyFile.FileName := 'document.pdf';
  MyFile.Info := Info.AsTOrm;  // Store the ID
  Orm.Add(MyFile, True);
finally
  MyFile.Free;
  Info.Free;
end;

// Retrieving - use CreateJoined for automatic loading
MyFile := TOrmFile.CreateJoined(Orm, FileID);
try
  WriteLn(MyFile.Info.FileSize);  // Info is now a real instance
finally
  MyFile.Free;  // Also frees MyFile.Info
end;
```

### 6.6.2. Many-to-Many with TOrmMany

For pivot tables (e.g., Authors ↔ Books):

```pascal
type
  TOrmAuthor = class(TOrm)
  published
    property Name: RawUtf8 read fName write fName;
  end;

  TOrmBook = class(TOrm)
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Authors: TOrmAuthorBookLink read fAuthors;  // Auto-instantiated
  end;

  TOrmAuthorBookLink = class(TOrmMany)
  published
    property Source: TOrmBook read fSource;     // Book side
    property Dest: TOrmAuthor read fDest;       // Author side
    property Contribution: RawUtf8 read fContribution write fContribution;
  end;

// Adding a many-to-many relationship
Book.Authors.ManyAdd(Orm, Book.ID, AuthorID);

// Query all authors for a book
if Book.Authors.FillMany(Orm, Book.ID) then
  while Book.Authors.FillOne do
    WriteLn('Author: ', TOrmAuthor(Book.Authors.Dest).Name);

// Query all books for an author
if Book.Authors.FillManyFromDest(Orm, AuthorID) then
  while Book.Authors.FillOne do
    WriteLn('Book: ', TOrmBook(Book.Authors.Source).Title);
```

### 6.6.3. Data Sharding (Embedded Documents)

Instead of pivot tables, embed data using `Variant` or dynamic arrays:

```pascal
type
  TOrmOrder = class(TOrm)
  private
    fCustomerName: RawUtf8;
    fLines: Variant;  // Embedded array of line items
  published
    property CustomerName: RawUtf8 read fCustomerName write fCustomerName;
    property Lines: Variant read fLines write fLines;  // JSON array
  end;

// Usage
Order.Lines := _JsonFast('[
  {"sku":"ABC123","qty":2,"price":29.99},
  {"sku":"XYZ789","qty":1,"price":49.99}
]');

// Query line items
for i := 0 to _Safe(Order.Lines)^._Count - 1 do
  WriteLn('SKU: ', _Safe(Order.Lines)^.Value[i].sku);
```

**Benefits**:
- Self-contained records (no joins)
- Better for MongoDB/NoSQL
- Simpler schema
- Natural for domain modeling

---

## 6.7. Batch Operations

### 6.7.1. TRestBatch for Bulk Inserts

High-performance bulk operations with single network roundtrip:

```pascal
uses
  mormot.orm.core;

var
  Batch: TRestBatch;
  Results: TIDDynArray;
  Baby: TOrmBaby;
  i: Integer;
begin
  Batch := TRestBatch.Create(Orm, TOrmBaby, 1000);  // Auto-flush every 1000
  try
    for i := 1 to 10000 do
    begin
      Baby := TOrmBaby.Create;
      Baby.Name := FormatUtf8('Baby %', [i]);
      Baby.BirthDate := Date - Random(365);
      Batch.Add(Baby, True);  // True = Batch owns Baby, auto-frees
    end;
    Orm.BatchSend(Batch, Results);  // Single network call
    WriteLn('Inserted ', Length(Results), ' records');
  finally
    Batch.Free;
  end;
end;
```

### 6.7.2. Batch Updates and Deletes

```pascal
Batch := TRestBatch.Create(Orm, TOrmBaby, 100, [boExtendedJson]);
try
  // Update multiple records
  Baby := TOrmBaby.CreateAndFillPrepare(Orm, 'Active = ?', [False]);
  try
    while Baby.FillOne do
    begin
      Baby.Status := sArchived;
      Batch.Update(Baby);
    end;
  finally
    Baby.Free;
  end;

  // Delete multiple records
  for ID in ObsoleteIDs do
    Batch.Delete(TOrmBaby, ID);

  Orm.BatchSend(Batch, Results);
finally
  Batch.Free;
end;
```

---

## 6.8. The Best ORM is the One You Need

mORMot offers multiple persistence patterns:

| Pattern | Use Case |
|---------|----------|
| **Native ORM** (`TOrm` → SQLite3) | Embedded apps, single-server |
| **External SQL** (`OrmMapExternal`) | Enterprise databases |
| **MongoDB ODM** (`OrmMapMongoDB`) | Document store, horizontal scaling |
| **In-Memory** (`TRestStorageInMemory`) | Caching, temporary data |
| **Repository Services** | DDD, clean architecture |

### 6.8.1. Mix and Match

Different tables can use different backends:

```pascal
// SQLite3 for local data
// (default - no mapping needed)

// PostgreSQL for shared data
OrmMapExternal(Model, TOrmCustomer, PostgresProps);
OrmMapExternal(Model, TOrmOrder, PostgresProps);

// MongoDB for logs
OrmMapMongoDB(Model, TOrmAuditLog, MongoClient.Database['logs']);

// In-memory for cache
Model.Props[TOrmSessionCache].SetStorage(TRestStorageInMemory);
```

### 6.8.2. Think Multi-Tier

Design your architecture with layers:

```
┌────────────────────────────────────┐
│  Presentation Layer                │
│  (VCL/FMX Forms, Web UI)           │
│  string, TDataSet, JSON            │
└────────────────────────────────────┘
                │
┌────────────────────────────────────┐
│  Application Layer                 │
│  (Services, Controllers)           │
│  RawUtf8, TOrm, IRestOrm           │
└────────────────────────────────────┘
                │
┌────────────────────────────────────┐
│  Domain Layer                      │
│  (Business Logic, Entities)        │
│  RawUtf8, Domain Objects           │
└────────────────────────────────────┘
                │
┌────────────────────────────────────┐
│  Infrastructure Layer              │
│  (Repositories, DB Access)         │
│  TOrm, IRestOrm, External DB       │
└────────────────────────────────────┘
```

---

## 6.9. Summary of Best Practices

1. **Use `IRestOrm` interface**, not concrete classes
2. **Use `RawUtf8`** for all text properties
3. **Use `FillPrepare/FillOne`** for memory-efficient queries
4. **Use `AutoFree`** to reduce boilerplate
5. **Use `TDocVariant`** for schema-less embedded data
6. **Use `TRestBatch`** for bulk operations
7. **Use `CreateJoined`** when you need nested objects
8. **Specify fields** in queries when you don't need all columns
9. **Design domain-first**, let the ORM handle persistence
10. **Test with `TRestStorageInMemory`** for fast unit tests

---

*Next Chapter: Database Layer (SQLite3, Virtual Tables)*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 5: Object-Relational Mapping](mORMot2-SAD-Chapter-05.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 7: Database Layer](mORMot2-SAD-Chapter-07.md) |
