# 7. Database Layer

*SQLite3 at the Core*

mORMot 2's persistence architecture is centered on SQLite3 but not limited to it. The framework supports multiple database backends, all accessible through a unified interface.

---

## 7.1. SQLite3-Powered, Not SQLite3-Limited

The core database of the framework uses SQLite3 - a free, secure, zero-configuration, server-less, cross-platform database engine.

### 7.1.1. Persistence Options

```
┌─────────────────────────────────────────────────────────────────────┐
│                        mORMot ORM / REST                            │
└─────────────────────────────────────────────────────────────────────┘
                                │
     ┌──────────────────────────┼──────────────────────────┐
     │                          │                          │
     ▼                          ▼                          ▼
┌─────────────┐          ┌─────────────┐          ┌─────────────┐      
│  SQLite3    │          │ External DB │          │   NoSQL           │
│  (native)   │          │  (via SQL)  │          │  (MongoDB)        │
└─────────────┘          └─────────────┘          └─────────────┘      
     │                          │                          │
     ▼                          ▼                          ▼
   File/Mem             PostgreSQL/Oracle          Document Store
                        MSSQL/MySQL/etc.
```

| Storage Backend | Unit(s) | Use Case |
|----------------|---------|----------|
| Internal SQLite3 | `mormot.orm.sqlite3` | Default, embedded, full SQL |
| In-Memory `TObjectList` | `mormot.orm.storage` | Fastest, no ACID, limited SQL |
| External RDBMS | `mormot.orm.sql` + `mormot.db.sql.*` | Enterprise databases |
| MongoDB | `mormot.orm.mongodb` | NoSQL document store |

### 7.1.2. SQLite3 as Core

The framework uses compiled SQLite3 code, included natively in Delphi/FPC:

- **Static linking** (recommended) or external `sqlite3.dll`
- **Optimized performance** via `FastMM4` / FPC memory manager
- **Optional encryption** (AES-256) on disk
- **Record-level locking** (SQLite3 only has file-level)
- **Client-Server support** (SQLite3 is normally standalone-only)

**Compilation options include:**
- ISO 8601 date/time handling
- R-Tree extension for range queries
- FTS3/FTS4/FTS5 full-text search
- PCRE-based `REGEXP` operator
- Custom SQL functions in Delphi

### 7.1.3. Virtual Tables Magic

SQLite3's Virtual Table mechanism allows mORMot to:

- Mix internal SQLite3 tables with external databases in the same query
- JOIN across different database engines
- Use `TObjectList` storage with SQL queries
- Present MongoDB collections as SQL tables

```pascal
// One model, multiple backends
Model := TOrmModel.Create([
  TOrmCustomer,   // Internal SQLite3
  TOrmProduct,    // External PostgreSQL
  TOrmOrder,      // External PostgreSQL
  TOrmAuditLog    // MongoDB
]);

// Mix in single query (via virtual tables)
Server.Orm.ExecuteList([TOrmOrder, TOrmCustomer],
  'SELECT Order.ID, Customer.Name FROM Order, Customer ' +
  'WHERE Order.CustomerID = Customer.ID');
```

---

## 7.2. SQLite3 Implementation

### 7.2.1. Unit Structure

```
mormot.db.raw.sqlite3.pas   → Low-level SQLite3 C API wrapper
        ↓
mormot.db.sql.sqlite3.pas   → TSqlDB* implementation for SQLite3
        ↓
mormot.orm.sqlite3.pas      → ORM integration (TRestServerDB)
```

### 7.2.2. Static vs Dynamic Linking

| Mode | Unit | Deployment |
|------|------|------------|
| Static | `mormot.db.raw.sqlite3.static` | Embedded in EXE (~1MB) |
| Dynamic | `mormot.db.raw.sqlite3` | External `sqlite3.dll` |

**Static linking** (recommended for Windows):
```pascal
uses
  mormot.db.raw.sqlite3.static,  // Include static .obj
  mormot.orm.sqlite3;
```

**Dynamic linking** (required for some platforms):
```pascal
uses
  mormot.db.raw.sqlite3,
  mormot.orm.sqlite3;

// Load external DLL
sqlite3 := TSqlite3LibraryDynamic.Create;
```

### 7.2.3. Database Modes

```pascal
uses
  mormot.orm.sqlite3,
  mormot.rest.sqlite3;

var
  Server: TRestServerDB;
begin
  // File-based (default - ACID, persistent)
  Server := TRestServerDB.Create(Model, 'data.db3');

  // In-memory (fast, non-persistent)
  Server := TRestServerDB.Create(Model, ':memory:');

  // Performance tuning
  Server.DB.Synchronous := smOff;       // Faster writes (less safe)
  Server.DB.LockingMode := lmExclusive; // Single-process access
end;
```

### 7.2.4. Performance Modes

| Mode | Safety | Speed | Use Case |
|------|--------|-------|----------|
| `smFull` (default) | ACID | Slow writes | Production with crash safety |
| `smOff` | Risk on crash | Fast | Batch imports, dev/test |
| `lmExclusive` | Single process | Fastest | Embedded applications |

**Batch write example:**
```pascal
Server.DB.Synchronous := smOff;
Server.DB.LockingMode := lmExclusive;
try
  // 100x faster bulk insert
  for i := 1 to 100000 do
    Server.Orm.Add(CreateRecord(i), True);
finally
  Server.DB.Synchronous := smFull;  // Restore safety
end;
```

---

## 7.3. Prepared Statements

mORMot automatically caches prepared SQL statements for reuse.

### 7.3.1. Parameter Binding

Use `?` placeholders for parameters:

```pascal
// Parameters bound safely (no SQL injection)
Rec := TOrm.CreateAndFillPrepare(Orm,
  'Name LIKE ? AND Active = ?', ['John%', True]);

// Date parameters
Rec := TOrm.CreateAndFillPrepare(Orm,
  'Created >= ?', [DateToSql(EncodeDate(2024, 1, 1))]);
```

### 7.3.2. Internal Caching

The framework caches prepared statements internally:

```pascal
// First call: Parse SQL, prepare statement, execute
Orm.Retrieve('Name = ?', [], ['John'], Rec);

// Subsequent calls: Reuse prepared statement, rebind parameters
Orm.Retrieve('Name = ?', [], ['Jane'], Rec);  // Much faster
```

### 7.3.3. JSON Inlined Parameters

Internally, parameters are encoded as `:(value):` in JSON:

```pascal
// API call
Orm.Retrieve('ID = ?', [], [42], Rec);

// Transmitted as JSON
'{"Where":"ID=:(42):"}'

// Prepared SQL
'SELECT * FROM TableName WHERE ID = ?'  // With 42 bound
```

---

## 7.4. R-Tree Extension

R-Trees provide fast range queries for multi-dimensional data (geospatial, temporal).

### 7.4.1. Defining R-Tree Tables

```pascal
type
  TOrmMapBox = class(TOrmRTree)
  private
    fMinX, fMaxX: Double;
    fMinY, fMaxY: Double;
  published
    property MinX: Double read fMinX write fMinX;
    property MaxX: Double read fMaxX write fMaxX;
    property MinY: Double read fMinY write fMinY;
    property MaxY: Double read fMaxY write fMaxY;
  end;
```

### 7.4.2. R-Tree Queries

```pascal
// Find all boxes containing point (10, 20)
Boxes := TOrmMapBox.CreateAndFillPrepare(Orm,
  'MinX <= ? AND MaxX >= ? AND MinY <= ? AND MaxY >= ?',
  [10, 10, 20, 20]);

// Or use RTreeMatch for complex queries
Orm.RTreeMatch(TOrmMapData, 'BlobField', TOrmMapBox,
  MapData.BlobField, ResultIDs);
```

---

## 7.5. Full-Text Search (FTS5)

FTS5 provides fast full-text search capabilities.

### 7.5.1. FTS Classes

| Class | Tokenizer | Use Case |
|-------|-----------|----------|
| `TOrmFTS5` | Simple | Basic text search |
| `TOrmFTS5Porter` | Porter stemmer | English text |
| `TOrmFTS5Unicode61` | Unicode61 | Non-Latin languages |

### 7.5.2. Defining FTS Tables

```pascal
type
  TOrmArticleFTS = class(TOrmFTS5Porter)
  private
    fTitle: RawUtf8;
    fBody: RawUtf8;
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Body: RawUtf8 read fBody write fBody;
  end;
```

### 7.5.3. Indexing Content

```pascal
// Link FTS to main table via DocID
FTS := TOrmArticleFTS.Create;
FTS.DocID := Article.ID;  // Link to TOrmArticle
FTS.Title := Article.Title;
FTS.Body := Article.Content;
Orm.Add(FTS, True);

// Optimize after bulk inserts
// Note: Method is named FTS3 because FTS5 inherits from FTS4 which inherits from FTS3
TOrmArticleFTS.OptimizeFTS3Index(Server.OrmInstance as IRestOrmServer);
```

### 7.5.4. Searching

```pascal
var
  IDs: TIDDynArray;
begin
  // Basic search
  Orm.FTSMatch(TOrmArticleFTS, 'database optimization', IDs);

  // With field weighting (Title=2x, Body=1x)
  Orm.FTSMatch(TOrmArticleFTS, 'database', IDs, [2.0, 1.0]);

  // Complex queries
  Orm.FTSMatch(TOrmArticleFTS, 'Title:database AND Body:performance', IDs);
end;
```

### 7.5.5. FTS Query Syntax

| Pattern | Meaning |
|---------|---------|
| `word` | Match exact word |
| `word*` | Prefix match |
| `"exact phrase"` | Match phrase |
| `word1 AND word2` | Both required |
| `word1 OR word2` | Either matches |
| `word1 NOT word2` | Exclude word2 |
| `NEAR(word1 word2, 5)` | Within 5 tokens |
| `Title:word` | Match in specific column |

---

## 7.6. In-Memory Storage

### 7.6.1. TRestStorageInMemory

Ultra-fast storage using `TObjectList`:

```pascal
uses
  mormot.orm.storage,
  mormot.orm.server;

// Create in-memory storage and register with ORM server
var
  Storage: TRestStorageInMemory;
  OrmServer: TRestOrmServer;
  TableIndex: Integer;
begin
  OrmServer := Server.OrmInstance as TRestOrmServer;
  Storage := TRestStorageInMemory.Create(TOrmCache, OrmServer);
  TableIndex := OrmServer.Model.GetTableIndexExisting(TOrmCache);
  OrmServer.StaticTableSetup(TableIndex, Storage, sStaticDataTable);
end;
```

### 7.6.2. Features and Limitations

| Feature | In-Memory | SQLite3 |
|---------|-----------|---------|
| Speed | Fastest | Fast |
| ACID | No | Yes |
| SQL Joins | Limited | Full |
| Max Size | RAM | Disk |
| Persistence | Optional (JSON/Binary) | Yes |
| Unique Index | O(1) hash | B-Tree |

### 7.6.3. Persistence Options

```pascal
var
  Storage: TRestStorageInMemory;
  Stream: TFileStream;
  JsonContent: RawUtf8;
begin
  Storage := TRestStorageInMemory.Create(TOrmCache, OrmServer);

  // Save to JSON file - SaveToJson returns RawUtf8
  JsonContent := Storage.SaveToJson(True);  // True = expand JSON
  FileFromString(JsonContent, 'cache.json');

  // Load from JSON file - LoadFromJson takes RawUtf8, not Stream
  JsonContent := StringFromFile('cache.json');
  Storage.LoadFromJson(JsonContent);

  // Save binary (via stream)
  Stream := TFileStream.Create('cache.data', fmCreate);
  try
    Storage.SaveToBinary(Stream);
  finally
    Stream.Free;
  end;

  // Load binary (via stream)
  Stream := TFileStream.Create('cache.data', fmOpenRead);
  try
    Storage.LoadFromBinary(Stream);
  finally
    Stream.Free;
  end;
end;
```

### 7.6.4. Virtual Table Mode

Register in-memory as virtual table for SQL access:

```pascal
// For SQL joins, register as virtual table BEFORE server creation
Model.VirtualTableRegister(TOrmCache, TOrmVirtualTableJson);

// Then create server - the virtual table will be available
Server := TRestServerDB.Create(Model, 'main.db3');
```

---

## 7.7. Virtual Tables

### 7.7.1. Built-in Virtual Table Classes

| Class | Storage | Persistence |
|-------|---------|-------------|
| `TOrmVirtualTableJson` | In-memory | JSON file |
| `TOrmVirtualTableBinary` | In-memory | Binary file |
| `TOrmVirtualTableExternal` | External DB | Database |
| `TOrmVirtualTableMongoDB` | MongoDB | MongoDB |

### 7.7.2. Registering Virtual Tables

```pascal
// Must register BEFORE creating server
Model.VirtualTableRegister(TOrmTempData, TOrmVirtualTableJson);
Model.VirtualTableRegister(TOrmCustomer, TOrmVirtualTableExternal);

// Then create server
Server := TRestServerDB.Create(Model, 'main.db3');
```

### 7.7.3. Custom Virtual Tables

```pascal
type
  TMyVirtualTable = class(TOrmVirtualTable)
  public
    class function ModuleName: RawUtf8; override;
    function Prepare(var Prepared: TOrmVirtualTablePrepared): boolean; override;
    function Search(var Prepared: TOrmVirtualTablePrepared): boolean; override;
  end;
```

---

## 7.8. JSON Functions in SQLite3

mORMot adds JSON manipulation functions to SQLite3:

### 7.8.1. JsonGet

Extract values from JSON columns:

```sql
-- Get property value
SELECT JsonGet(DataColumn, 'name') FROM Table WHERE ID=1;

-- Get nested property
SELECT JsonGet(DataColumn, 'address.city') FROM Table;

-- Get multiple properties
SELECT JsonGet(DataColumn, 'name,email') FROM Table;

-- Wildcard match
SELECT JsonGet(DataColumn, 'user.*') FROM Table;
```

### 7.8.2. JsonHas

Check property existence:

```sql
-- Check if property exists
SELECT * FROM Table WHERE JsonHas(DataColumn, 'premium') = 1;

-- Check nested property
SELECT * FROM Table WHERE JsonHas(DataColumn, 'settings.darkMode') = 1;
```

---

## 7.9. Backup and Recovery

### 7.9.1. Online Backup

```pascal
// Hot backup (while server is running)
Server.DB.BackupBackground('backup.db3', 100, 10,
  procedure(Sender: TSqlDatabase; Step: integer)
  begin
    WriteLn('Backup progress: ', Step);
  end);
```

### 7.9.2. Restore

```pascal
// Stop server, copy backup file, restart
Server.Free;
CopyFile('backup.db3', 'data.db3');
Server := TRestServerDB.Create(Model, 'data.db3');
```

---

## 7.10. Performance Tips

### 7.10.1. General Guidelines

1. **Use transactions** for bulk operations
2. **Use batch operations** (`TRestBatch`) for inserts
3. **Use prepared statements** (automatic with `?` parameters)
4. **Index foreign keys** and frequently queried columns
5. **Use `smOff` temporarily** for bulk imports
6. **Use `lmExclusive`** for single-process applications

### 7.10.2. Benchmark Reference

Typical performance on modern hardware (SSD, Core i7):

| Operation | In-Memory | SQLite3 (File) | External PostgreSQL |
|-----------|-----------|----------------|---------------------|
| Insert (single) | 300,000/s | 500/s | 5,000/s |
| Insert (batch) | 500,000/s | 200,000/s | 50,000/s |
| Read (by ID) | 900,000/s | 130,000/s | 10,000/s |
| Read (all) | 900,000/s | 550,000/s | 150,000/s |

*Note: Actual performance varies based on hardware, network, and data complexity.*

---

## 7.11. Migration from mORMot 1

### 7.11.1. Unit Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `SynSQLite3.pas` | `mormot.db.raw.sqlite3.pas` |
| `SynSQLite3Static.pas` | `mormot.db.raw.sqlite3.static.pas` |
| `mORMotSQLite3.pas` | `mormot.orm.sqlite3.pas` + `mormot.rest.sqlite3.pas` |

### 7.11.2. Class Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `TSQLRestServerDB` | `TRestServerDB` |
| `TSQLDatabase` | `TSqlDatabase` |
| `TSQLRecordFTS3` | `TOrmFTS3` |
| `TSQLRecordFTS5` | `TOrmFTS5` |
| `TSQLRecordRTree` | `TOrmRTree` |
| `TSQLRestStorageInMemory` | `TRestStorageInMemory` |

---

*Next Chapter: External SQL Database Access*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 6: Daily ORM](mORMot2-SAD-Chapter-06.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 8: External SQL Database Access](mORMot2-SAD-Chapter-08.md) |
