# 5. Object-Relational Mapping

*Persist Your Objects*

The ORM layer of mORMot 2 is implemented across the `mormot.orm.*` units. It provides database-agnostic persistence for Delphi objects, supporting SQLite3, external SQL databases (PostgreSQL, Oracle, MS SQL, MySQL, etc.), MongoDB (ODM), and in-memory storage.

Generic data access is implemented by defining high-level objects as Delphi classes descending from `TOrm`. These classes serve multiple purposes:

- **Database persistence**: CRUD operations (SELECT/INSERT/UPDATE/DELETE) without writing SQL
- **RESTful resources**: Business logic objects accessible via REST/JSON
- **UI integration**: Automatic form generation, grid binding, and reporting

---

## 5.1. TOrm Field Definition

All ORM functionality relies on the `TOrm` class (defined in `mormot.orm.core`). This abstract class provides built-in methods for generic ORM processing.

### 5.1.1. Primary Key

`TOrm` defines a primary key field as `ID: TID` (where `TID = type Int64`):

```pascal
type
  TID = type Int64;

  TOrm = class(TObject)
    // ...
    property ID: TID read GetID;  // Read-only; use IDValue for write access
  end;
```

The ORM relies on `Int64` primary keys, matching SQLite3's `RowID`. While you might prefer `TEXT` or `GUID` primary keys in traditional RDBMS design, integer keys are more efficient for ORM internals. You can always define secondary unique keys using `stored AS_UNIQUE`.

### 5.1.2. Defining a Table

All `published` properties of `TOrm` descendants are automatically mapped to database columns:

```pascal
uses
  mormot.core.base,
  mormot.orm.core;

type
  /// Enumeration for gender
  TSex = (sFemale, sMale);

  /// Table for Baby records
  TOrmBaby = class(TOrm)
  private
    fName: RawUtf8;
    fAddress: RawUtf8;
    fBirthDate: TDateTime;
    fSex: TSex;
  published
    property Name: RawUtf8 read fName write fName;
    property Address: RawUtf8 read fAddress write fAddress;
    property BirthDate: TDateTime read fBirthDate write fBirthDate;
    property Sex: TSex read fSex write fSex;
  end;
```

By adding `TOrmBaby` to a `TOrmModel`, the corresponding `Baby` table is automatically created. No SQL required.

### 5.1.3. Supported Property Types

| Delphi Type | SQLite3 Type | Notes |
|-------------|--------------|-------|
| `Byte`, `Word`, `Integer`, `Cardinal`, `Int64` | INTEGER | |
| `Boolean` | INTEGER | 0 = false, non-zero = true |
| Enumeration | INTEGER | Stored as ordinal value |
| Set | INTEGER | Bit-packed (up to 64 elements) |
| `Single`, `Double`, `Extended` | FLOAT | Extended stored as double |
| `Currency` | FLOAT | Fixed 4 decimals, no rounding errors |
| `RawUtf8` | TEXT | **Preferred** for text fields |
| `string` | TEXT | Use `RawUtf8` instead when possible |
| `TDateTime` | TEXT | ISO 8601 with second resolution |
| `TDateTimeMS` | TEXT | ISO 8601 with millisecond resolution |
| `TTimeLog` | INTEGER | Compact proprietary format |
| `TModTime` | INTEGER | Auto-updated on modification |
| `TCreateTime` | INTEGER | Auto-set on creation |
| `TUnixTime` | INTEGER | Seconds since 1970-01-01 |
| `TUnixMSTime` | INTEGER | Milliseconds since 1970-01-01 |
| `TOrm` | INTEGER | Foreign key (RowID of another table) |
| `TID` | INTEGER | 64-bit foreign key (no table info) |
| `TOrmMany` | (pivot table) | Many-to-many relationship |
| `TRecordReference` | INTEGER | Reference to any table in model |
| `TSessionUserID` | INTEGER | Auto-filled with current user ID |
| `TPersistent` | TEXT | JSON object |
| `TCollection` | TEXT | JSON array of objects |
| `TObjectList` | TEXT | JSON array (requires registration) |
| `TStrings` | TEXT | JSON array of strings |
| `RawBlob` | BLOB | Binary data |
| Dynamic arrays | BLOB | Binary format via `TDynArray.SaveTo` |
| `Variant` | TEXT | JSON (or TDocVariant) |
| `TNullableInteger`, etc. | varies | Nullable types supporting SQL NULL |
| `record` | TEXT | JSON (Delphi XE5+) |
| `TRecordVersion` | INTEGER | Monotonic change counter |

### 5.1.4. Property Attributes

Use special attributes in property declarations:

```pascal
type
  TOrmDiaper = class(TOrm)
  private
    fSerialNumber: RawUtf8;
    fModel: TOrmDiaperModel;
    fBaby: TOrmBaby;
  published
    property SerialNumber: RawUtf8
      index 30                    // Max 30 chars for external DB
      read fSerialNumber write fSerialNumber
      stored AS_UNIQUE;           // Creates unique index
    property Model: TOrmDiaperModel read fModel write fModel;
    property Baby: TOrmBaby read fBaby write fBaby;
  end;
```

- **`stored AS_UNIQUE`**: Creates a unique database index
- **`index N`**: Maximum character length for external databases
- **`index N`** (dynamic arrays): Used for `TOrm.DynArray(N)` wrapper access

---

## 5.2. Text Fields

The preferred type for text storage is `RawUtf8`. This ensures:

- Consistent UTF-8 encoding across all database engines
- No conversion overhead during JSON serialization
- Compatibility with all Delphi versions

```pascal
// Business layer: Use RawUtf8
property CustomerName: RawUtf8 read fCustomerName write fCustomerName;

// UI layer: Convert for display
var
  displayName: string;
begin
  displayName := Utf8ToString(Customer.CustomerName);
  Edit1.Text := displayName;
end;
```

**Domain-Driven Tip**: Using `RawUtf8` in your domain layer prevents accidental coupling between business logic and presentation.

---

## 5.3. Date and Time Fields

### 5.3.1. TDateTime / TDateTimeMS

Stored as ISO 8601 text in the database:

```pascal
property CreatedAt: TDateTime read fCreatedAt write fCreatedAt;
property PreciseTime: TDateTimeMS read fPreciseTime write fPreciseTime;  // With milliseconds
```

### 5.3.2. TTimeLog / TModTime / TCreateTime

Stored as INTEGER for fast comparison:

```pascal
property LastModified: TModTime read fLastModified write fLastModified;  // Auto-updated
property Created: TCreateTime read fCreated write fCreated;              // Auto-set once
property Timestamp: TTimeLog read fTimestamp write fTimestamp;           // Manual
```

`TModTime` and `TCreateTime` are automatically updated by the server:
- `TCreateTime`: Set when the record is first inserted
- `TModTime`: Updated on every modification

### 5.3.3. TUnixTime / TUnixMSTime

For interoperability with JavaScript/C#/Java:

```pascal
property UnixTs: TUnixTime read fUnixTs write fUnixTs;        // Seconds
property UnixMsTs: TUnixMSTime read fUnixMsTs write fUnixMsTs; // Milliseconds
```

---

## 5.4. TOrm Foreign Key Fields

### 5.4.1. TOrm Published Properties (NOT Instances)

**Critical**: `TOrm` published properties are **NOT** class instances. They store `pointer(RowID)`:

```pascal
type
  TOrmOrder = class(TOrm)
  published
    property Customer: TOrmCustomer read fCustomer write fCustomer;  // Stores ID, not instance!
  end;

// WRONG - will cause Access Violation:
WriteLn(Order.Customer.Name);  // AV! Customer is not an instance

// CORRECT - retrieve separately:
var
  cust: TOrmCustomer;
begin
  cust := TOrmCustomer.Create(Client, Order.Customer);  // Load by ID
  try
    WriteLn(cust.Name);
  finally
    cust.Free;
  end;
end;
```

### 5.4.2. Setting Foreign Keys

```pascal
var
  Order: TOrmOrder;
  Customer: TOrmCustomer;
begin
  Customer := TOrmCustomer.Create;
  Order := TOrmOrder.Create;
  try
    Customer.Name := 'ACME Corp';
    Client.Add(Customer, True);  // Customer.ID is now set

    Order.Customer := Customer.AsTOrm;  // Use AsTOrm for cross-platform
    // or: Order.Customer := pointer(Customer.ID);  // 32-bit only
    Client.Add(Order, True);
  finally
    Order.Free;
    Customer.Free;
  end;
end;
```

### 5.4.3. CreateJoined for Automatic Loading

Use `CreateJoined` to auto-instantiate and load all `TOrm` properties:

```pascal
var
  Order: TOrmOrder;
begin
  Order := TOrmOrder.CreateJoined(Client, OrderID);
  try
    // Now Order.Customer is a real instance, loaded via JOIN
    WriteLn(Order.Customer.Name);  // Safe!
  finally
    Order.Free;  // Also frees Order.Customer
  end;
end;
```

### 5.4.4. Deletion Tracking

The ORM automatically handles foreign key integrity (emulated, not via SQL constraints):

| Type | Index | Deletion Behavior |
|------|-------|-------------------|
| `TOrm` property | Yes | Field reset to 0 |
| `TID` | Yes | None (no table info) |
| `TOrmClassNameID` | Yes | Field reset to 0 |
| `TOrmClassNameToBeDeletedID` | Yes | Row deleted (cascade) |
| `TRecordReference` | Yes | Field reset to 0 |
| `TRecordReferenceToBeDeleted` | Yes | Row deleted (cascade) |

Define typed IDs for explicit cascade behavior:

```pascal
type
  TOrmCustomerID = type TID;              // Reset to 0 on delete
  TOrmCustomerToBeDeletedID = type TID;   // Cascade delete

  TOrmOrder = class(TOrm)
  published
    property Customer: TOrmCustomerID read fCustomer write fCustomer;
    property OwnerCustomer: TOrmCustomerToBeDeletedID read fOwner write fOwner;
  end;
```

---

## 5.5. TRecordReference: Cross-Table References

`TRecordReference` stores a reference to **any** table in the model:

```pascal
type
  TOrmAuditLog = class(TOrm)
  published
    property RelatedRecord: TRecordReference read fRelated write fRelated;
  end;

// Usage
var
  Log: TOrmAuditLog;
  Ref: RecordRef;  // Helper record
begin
  // Store reference to any record (requires Model for table index lookup)
  Log.RelatedRecord := RecordReference(Model, TOrmCustomer, CustomerID);

  // Retrieve via helper
  Ref.Value := Log.RelatedRecord;
  WriteLn('Table: ', Ref.Table(Model).SqlTableName);
  WriteLn('ID: ', Ref.ID);

  // Load the referenced record directly
  Rec := Client.Retrieve(Log.RelatedRecord);
end;
```

**Warning**: `TRecordReference` encodes table index in high bits. **Never change table order** in `TOrmModel` after deployment.

---

## 5.6. Variant Fields and TDocVariant

`Variant` fields are stored as JSON TEXT:

```pascal
type
  TOrmDocument = class(TOrm)
  published
    property Data: Variant read fData write fData;
  end;

// Usage - schema-less storage
var
  Doc: TOrmDocument;
begin
  Doc := TOrmDocument.Create;
  Doc.Data := _ObjFast(['name', 'John', 'tags', _Arr(['admin', 'user'])]);
  Client.Add(Doc, True);

  // Later retrieval
  Doc := TOrmDocument.Create(Client, DocID);
  WriteLn(Doc.Data.name);        // 'John'
  WriteLn(Doc.Data.tags._Count); // 2
end;
```

For MongoDB ODM, variants are stored as native BSON documents with full query support.

---

## 5.7. Dynamic Array Fields

Dynamic arrays are stored as BLOB in binary format:

```pascal
type
  TIntegerArray = array of Integer;

  TOrmWithArray = class(TOrm)
  private
    fScores: TIntegerArray;
  published
    property Scores: TIntegerArray index 1 read fScores write fScores;
  end;
```

Access via `TDynArray` wrapper:

```pascal
var
  Rec: TOrmWithArray;
  DA: TDynArray;
  Value: Integer;
begin
  DA := Rec.DynArray(1);  // Get wrapper for Scores
  Value := 100;
  DA.Add(Value);  // TDynArray.Add takes a reference
  Value := 200;
  DA.Add(Value);
end;
```

---

## 5.8. TNullable* Types

For SQL NULL support, use nullable types:

```pascal
type
  TOrmNullableRecord = class(TOrm)
  published
    property OptionalInt: TNullableInteger read fOptionalInt write fOptionalInt;
    property OptionalText: TNullableUtf8Text index 100 read fOptionalText write fOptionalText;
    property OptionalDate: TNullableDateTime read fOptionalDate write fOptionalDate;
  end;

// Usage
var
  Rec: TOrmNullableRecord;
begin
  Rec := TOrmNullableRecord.Create;
  Rec.OptionalInt := NullableInteger(42);        // Has value
  Rec.OptionalText := NullableUtf8TextNull;      // Is NULL
  Rec.OptionalDate := NullableDateTime(Now);     // Has value

  if NullableIntegerIsEmptyOrNull(Rec.OptionalInt) then
    WriteLn('No value')
  else
    WriteLn('Value: ', NullableIntegerToValue(Rec.OptionalInt));
end;
```

Available nullable types:
- `TNullableInteger` (Int64)
- `TNullableBoolean`
- `TNullableFloat` (Double)
- `TNullableCurrency`
- `TNullableDateTime`
- `TNullableTimeLog`
- `TNullableUtf8Text`

---

## 5.9. TOrmModel: Schema Definition

`TOrmModel` defines which `TOrm` classes form your database:

```pascal
uses
  mormot.orm.core;

var
  Model: TOrmModel;
begin
  Model := TOrmModel.Create([
    TOrmCustomer,
    TOrmProduct,
    TOrmOrder,
    TOrmOrderLine
  ], 'api');  // 'api' is the root URI

  // Table order matters for TRecordReference - don't change after deployment!
end;
```

### 5.9.1. Field Validation and Constraints

```pascal
// Add unique validation at runtime (alternative to stored AS_UNIQUE)
TOrmCustomer.AddFilterOrValidate('Email', TSynValidateUniqueField.Create);

// For multi-field uniqueness
TOrmOrder.AddFilterOrValidate('OrderNumber',
  TSynValidateUniqueFields.Create('{"FieldNames":"CustomerID,OrderNumber"}'));
```

**Note**: Prefer using `stored AS_UNIQUE` in property declarations for compile-time constraints. Runtime validation is useful for conditional or complex uniqueness rules.

---

## 5.10. IRestOrm: The ORM Interface

**Always code against `IRestOrm` interface**, not concrete classes:

```pascal
uses
  mormot.orm.core;

procedure DoWork(const Orm: IRestOrm);  // Interface parameter
var
  Customer: TOrmCustomer;
begin
  Customer := TOrmCustomer.Create;
  try
    Customer.Name := 'ACME Corp';
    Orm.Add(Customer, True);    // Add via interface
  finally
    Customer.Free;
  end;
end;
```

### 5.10.1. CRUD Operations

```pascal
var
  Orm: IRestOrm;
  Customer: TOrmCustomer;
  ID: TID;
begin
  // CREATE
  Customer := TOrmCustomer.Create;
  Customer.Name := 'New Customer';
  ID := Orm.Add(Customer, True);  // Returns new ID

  // READ
  Customer := TOrmCustomer.Create(Orm, ID);  // Load by ID
  // or
  Orm.Retrieve(ID, Customer);                // Load into existing instance

  // UPDATE
  Customer.Name := 'Updated Name';
  Orm.Update(Customer);

  // DELETE
  Orm.Delete(TOrmCustomer, ID);
end;
```

### 5.10.2. Accessing ORM from TRest

In mORMot 2, ORM is accessed via the `.Orm` property:

```pascal
// mORMot 1 style (deprecated):
Server.Add(Customer, True);

// mORMot 2 style:
Server.Orm.Add(Customer, True);

// Or store interface:
var
  Orm: IRestOrm;
begin
  Orm := Server.Orm;
  Orm.Add(Customer, True);
end;
```

---

## 5.11. Virtual Tables and Storage Backends

### 5.11.1. In-Memory Storage

```pascal
uses
  mormot.rest.memserver;

// Use TRestServerFullMemory for pure in-memory ORM
Server := TRestServerFullMemory.Create(Model, '', False, False);
// Or with file persistence:
Server := TRestServerFullMemory.Create(Model, 'data.json', False, False);
```

### 5.11.2. External SQL Database

```pascal
uses
  mormot.orm.sql,
  mormot.db.sql.postgres;

var
  Props: TSqlDBPostgresConnectionProperties;
begin
  Props := TSqlDBPostgresConnectionProperties.Create(
    'localhost:5432', 'mydb', 'user', 'pass');

  // Map TOrm to external database
  OrmMapExternal(Model, TOrmCustomer, Props);
  OrmMapExternal(Model, TOrmOrder, Props);
end;
```

### 5.11.3. MongoDB (ODM)

```pascal
uses
  mormot.orm.mongodb,
  mormot.db.nosql.mongodb;

var
  Client: TMongoClient;
  Server: TRestServer;
begin
  Client := TMongoClient.Create('localhost', 27017);
  // OrmMapMongoDB signature: (aClass, aServer: TRestOrm, aMongoDatabase, aCollectionName)
  OrmMapMongoDB(TOrmDocument, Server, Client.Database['mydb']);
end;
```

---

## 5.12. Migration from mORMot 1

### 5.12.1. Class Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `TSQLRecord` | `TOrm` |
| `TSQLRecordClass` | `TOrmClass` |
| `TSQLModel` | `TOrmModel` |
| `TSQLTable` | `TOrmTable` |
| `TSQLTableJSON` | `TOrmTableJson` |
| `TSQLRest` | `TRest` |
| `TSQLRest.Add()` | `TRest.Orm.Add()` |
| `TSQLRecordMany` | `TOrmMany` |
| `TSQLRawBlob` | `RawBlob` |

### 5.12.2. Unit Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `mORMot.pas` | `mormot.orm.core` + `mormot.rest.core` |
| `mORMotSQLite3.pas` | `mormot.orm.sqlite3` + `mormot.rest.sqlite3` |
| `mORMotDB.pas` | `mormot.orm.sql` |
| `mORMotMongoDB.pas` | `mormot.orm.mongodb` |

### 5.12.3. Backward Compatibility

By default, type aliases are provided:
```pascal
type
  TSQLRecord = TOrm;
  TSQLModel = TOrmModel;
  // etc.
```

Define `PUREMORMOT2` to disable these and use only new names.

---

*Next Chapter: Daily ORM (Working with Objects and Queries)*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 4: Core Units](mORMot2-SAD-Chapter-04.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 6: Daily ORM](mORMot2-SAD-Chapter-06.md) |
