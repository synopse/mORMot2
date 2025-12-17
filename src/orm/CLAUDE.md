# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This folder contains the **RESTful ORM (Object-Relational-Mapping)** layer of the mORMot 2 framework. It provides persistent-agnostic data access through a clean interface-based architecture that can work with SQLite3, external SQL databases, MongoDB (ODM), or in-memory storage.

**Key Design Principle**: The ORM layer is decoupled from REST transport (`mormot.rest.core`) and can be used as a pure persistence layer.

## Architecture Layers

### Core Abstraction (Bottom → Top)

```
mormot.orm.base         → Low-level types, TOrmWriter, TOrmPropInfo
         ↓
mormot.orm.core         → TOrm, IRestOrm, TOrmModel, TOrmTable
         ↓
mormot.orm.rest         → TRestOrm (base implementation)
         ↓
   ┌────────────────────┴────────────────────┐
   ↓                                         ↓
mormot.orm.client                    mormot.orm.server
TRestOrmClient                       TRestOrmServer
TRestOrmClientUri                    + fStaticData[] (pure in-memory)
                                     + fStaticVirtualTable[] (virtual tables)
```

### Storage Backends

```
mormot.orm.storage      → TRestStorage, TRestStorageInMemory
                          TOrmVirtualTable (JSON/Binary)
         ↓
   ┌────────────────┬────────────────┐
   ↓                ↓                ↓
mormot.orm.sqlite3  mormot.orm.sql  mormot.orm.mongodb
TRestOrmServerDB    TRestStorageExternal  TRestStorageMongoDB
(SQLite3 native)    (via mormot.db.sql)   (NoSQL/ODM)
```

## Key Interfaces & Classes

### Main Entry Point: `IRestOrm`

**The SOLID interface for all ORM operations** - always code against this interface, not concrete classes.

```pascal
// Defined in mormot.orm.core
IRestOrm
  ├─ IRestOrmClient  // Client-side extensions (BatchStart, UpdateFromServer)
  └─ IRestOrmServer  // Server-side extensions (CreateMissingTables, AfterDeleteForceCoherency)
```

**Common usage pattern**:
```pascal
var
  Orm: IRestOrm;  // ← Use interface, not TRestOrm
begin
  Orm := TRestOrmClient.Create(...);  // or TRestOrmServer
  Orm.Retrieve(...);
  Orm.Add(...);
end;
```

### TOrm: The Base Persistent Class

All persistent classes inherit from `TOrm` (defined in `mormot.orm.core`).

**Special auto-computed field types** (set `oftXxx` in `TOrmPropInfo`):
- `TModTime` / `TCreateTime` - Timestamps (auto-updated by server)
- `TSessionUserID` - Current user ID (auto-filled from session)
- `TRecordVersion` - Monotonic change counter (for conflict detection)
- `TRecordReference` - Reference to any table (ON DELETE SET DEFAULT behavior)
- `TRecordReferenceToBeDeleted` - Reference with cascade delete (ON DELETE CASCADE)
- `TID` subtypes - Custom typed IDs (e.g., `TOrmClientID = type TID`)

**Key methods**:
- `OrmProps` - Access RTTI/field metadata (`TOrmProperties`)
- `FillPrepare()` / `FillOne()` - Cursor-based result iteration
- `GetAsVariant()` / `SetAsVariant()` - Dynamic field access

### TOrmModel: Database Schema Definition

Defines which `TOrm` classes form your database schema.

```pascal
Model := TOrmModel.Create([TOrmCustomer, TOrmOrder, TOrmProduct], 'root');
// Table order matters for TRecordReference encoding (don't change after deployment)
```

**Critical**: `TRecordReference` encodes table index in high bits - changing table order breaks existing references.

### TOrmTable: Query Results

Read-only result set (like a `TDataSet` but JSON-native).

```pascal
Table := Orm.ExecuteList(TOrmOrder, 'Status=?', [1]);
for i := 0 to Table.RowCount - 1 do
  WriteLn(Table.Get(i, 'CustomerName'));
```

**Variant row access**: Use `TOrmTableRowVariant` for dynamic field access (slower but convenient).

## Virtual Tables

**Pattern**: Delphi-side implementation of SQLite virtual tables (or in-memory/external storage).

### Base classes:
- `TOrmVirtualTableForcedID` - Manual ID assignment
- `TOrmVirtualTableAutoID` - Auto-generated ID

### Built-in modules:
- `TOrmVirtualTableJson` - JSON file storage
- `TOrmVirtualTableBinary` - Binary file storage
- `TOrmVirtualTableExternal` - SQL database redirection (via `mormot.orm.sql`)

**Registration pattern**:
```pascal
Model.VirtualTableRegister(TOrmMyData, TOrmVirtualTableJson);
// Must be called BEFORE TRestOrmServer.Create
```

## Batch Operations: TRestBatch

High-performance bulk inserts/updates/deletes with a single server roundtrip.

```pascal
Batch := TRestBatch.Create(Orm, TOrmOrder, 1000);  // 1000 = auto-commit threshold
try
  for i := 1 to 10000 do
  begin
    Order := TOrmOrder.Create;
    // ... populate Order
    Batch.Add(Order, true);  // true = SendData owns Order
  end;
  Orm.BatchSend(Batch, Results);  // ← Single network call
finally
  Batch.Free;
end;
```

**Encoding schemes** (`TRestBatchEncoding`):
- `encPost` / `encPostHex` - INSERT operations
- `encPut` / `encPutHex` - UPDATE operations
- `encDelete` - DELETE operations

## Important Patterns

### 1. Static Tables (Server-Side)

**Two types**:
- `fStaticData[]` - Pure in-memory (NOT available in SQL joins)
- `fStaticVirtualTable[]` - Virtual tables (available in SQL joins)

Both use `TRestStorage` descendants but differ in SQLite3 visibility.

### 2. Custom Property Types

Define custom serialization via:
```pascal
class procedure InternalRegisterCustomProperties(Props: TOrmProperties); override;
begin
  Props.RegisterCustomFixedSizeRecordProperty(...);
  Props.RegisterCustomRttiRecordProperty(...);
end;
```

### 3. Validation & Filters

Add at class level (not instance level):
```pascal
class procedure InternalDefineModel(Props: TOrmProperties); override;
begin
  inherited;
  TOrmOrder.AddFilterNotVoidText(['CustomerName', 'Address']);
  TOrmOrder.AddFilterOrValidate('Email', TSynValidateEmail.Create);
end;
```

### 4. Typed TID Fields

Pattern for referential integrity:
```pascal
type
  TOrmClientID = type TID;  // Links to TOrmClient
  TOrmClientToBeDeletedID = type TID;  // CASCADE delete

  TOrmOrder = class(TOrm)
  published
    property Client: TOrmClientID read fClient write fClient;
    // ↑ Auto-reset to 0 when TOrmClient deleted

    property Owner: TOrmClientToBeDeletedID read fOwner write fOwner;
    // ↑ Order deleted when TOrmClient deleted
  end;
```

**Naming convention**: Type name must match `TOrm{ClassName}[ToBeDeleted]ID`.

## Storage Engine Selection Guide

| Backend | Use When | Pros | Cons |
|---------|----------|------|------|
| **TRestOrmServerDB** (SQLite3) | Embedded, single-server | Fast, ACID, SQL joins, FTS | No horizontal scaling |
| **TRestStorageExternal** | Enterprise SQL DB | Scalable, existing infra | Connection overhead |
| **TRestStorageMongoDB** | Document store, sharding | Flexible schema, horizontal scaling | No SQL joins |
| **TRestStorageInMemory** | Caching, temp data | Fastest, JSON/binary persistence | RAM-limited, no ACID |

## Unit Dependencies

**Minimal ORM** (no REST transport):
```pascal
uses
  mormot.orm.base,   // Low-level types
  mormot.orm.core;   // TOrm, IRestOrm, TOrmModel
```

**Client-side**:
```pascal
uses
  mormot.orm.core,
  mormot.orm.client;
```

**Server-side with SQLite3**:
```pascal
uses
  mormot.orm.core,
  mormot.orm.server,
  mormot.orm.sqlite3;
```

**External SQL database**:
```pascal
uses
  mormot.orm.core,
  mormot.orm.server,
  mormot.orm.sql,
  mormot.db.sql.{provider};  // .postgres, .mssql, .oracle, etc.
```

## Common Pitfalls

1. **TOrmModel table order**: Never reorder tables after deployment - breaks `TRecordReference`.

2. **Virtual table registration timing**: Must call `Model.VirtualTableRegister()` BEFORE `TRestOrmServer.Create`.

3. **Batch ownership**: When using `Batch.Add(Obj, true)`, the batch owns the object - don't free manually.

4. **IRestOrm vs TRestOrm**: Always declare variables as `IRestOrm` (interface), not `TRestOrm` (implementation).

5. **Static table limitations**: `fStaticData[]` tables are NOT visible in SQL joins - use `fStaticVirtualTable[]` if joins needed.

6. **TModTime fields**: Only updated by server operations - direct SQL updates won't trigger auto-update.

## Thread Safety

- **TOrmModel**: Read-only after creation → thread-safe
- **TOrm instances**: NOT thread-safe (each thread needs own instances)
- **IRestOrm methods**: Thread-safe if underlying storage is (check provider docs)
- **TOrmCache**: Thread-safe (uses locks internally)
- **TRestBatch**: NOT thread-safe (use one per thread)

## Testing & Debugging

**Test suites**: See `/mnt/w/mORMot2/test/test.orm.*.pas` for comprehensive examples.

**Logging**: Enable SQL logging via:
```pascal
Orm.LogFamily.Level := LOG_VERBOSE;  // Logs all SQL statements
```

**JSON inspection**: Use `TOrm.GetJSONValues()` to debug serialization issues.

## Related Documentation

**📖 SAD Chapters**:
- [Chapter 5: ORM](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-05.md) - TOrm, TOrmModel, field types
- [Chapter 6: ORM Advanced](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-06.md) - Filtering, virtual tables, caching

**Framework References**:
- **Main README**: `/mnt/w/mORMot2/src/orm/README.md`
- **Official Docs**: https://synopse.info/files/doc/mORMot2.html
- **Core RTTI**: `/mnt/w/mORMot2/src/core/mormot.core.rtti.pas`
- **Database layer**: `/mnt/w/mORMot2/src/db/`
- **REST transport**: `/mnt/w/mORMot2/src/rest/`
