# 8. External SQL Database Access

*Connect to Any RDBMS*

The mORMot framework provides direct access to external SQL databases through the `mormot.db.sql.*` units. These databases can be used standalone or integrated with the ORM via virtual tables.

---

## 8.1. Architecture Overview

### 8.1.1. SynDB Layer (mORMot 2: mormot.db.*)

```
┌─────────────────────────────────────────────────────────────────┐
│                     mormot.db.sql.pas                            │
│       TSqlDBConnectionProperties (base class, virtual methods)   │
│              TSqlDBConnection, TSqlDBStatement                   │
└─────────────────────────────────────────────────────────────────┘ 
                              │
      ┌───────────────────────┼───────────────────────┐
      │                       │                       │
      ▼                       ▼                       ▼
┌─────────────┐       ┌─────────────┐       ┌─────────────┐         
│   Direct    │       │    ODBC     │       │  TDataSet            │
│   Access    │       │   Access    │       │   Bridge             │
└─────────────┘       └─────────────┘       └─────────────┘         
      │                       │                       │
      ▼                       ▼                       ▼
  PostgreSQL              Any ODBC              FireDAC
  Oracle (OCI)            Driver                UniDAC
  SQLite3                                       NexusDB
```

### 8.1.2. Provider Units

| Provider | Unit | Direct Access |
|----------|------|---------------|
| PostgreSQL | `mormot.db.sql.postgres` | Yes (libpq) |
| Oracle | `mormot.db.sql.oracle` | Yes (OCI) |
| SQLite3 | `mormot.db.sql.sqlite3` | Yes |
| ODBC | `mormot.db.sql.odbc` | Yes |
| OleDB | `mormot.db.sql.oledb` | Yes |
| Zeos/ZDBC | `mormot.db.sql.zeos` | Yes (recommended) |
| FireDAC | `mormot.db.rad.firedac` | Via TDataSet |
| UniDAC | `mormot.db.rad.unidac` | Via TDataSet |
| NexusDB | `mormot.db.rad.nexusdb` | Via TDataSet |

### 8.1.3. Database Engines Supported

| Database | Direct | ODBC | OleDB | Zeos | TDataSet |
|----------|--------|------|-------|------|----------|
| PostgreSQL | ✓ | ✓ | - | ✓ | ✓ |
| Oracle | ✓ | ✓ | ✓ | ✓ | ✓ |
| MS SQL Server | - | ✓ | ✓ | ✓ | ✓ |
| MySQL/MariaDB | - | ✓ | - | ✓ | ✓ |
| SQLite3 | ✓ | ✓ | - | ✓ | ✓ |
| Firebird | - | ✓ | - | ✓ | ✓ |
| IBM DB2 | - | ✓ | ✓ | ✓ | ✓ |
| Informix | - | ✓ | - | - | ✓ |

---

## 8.2. Connection Properties

### 8.2.1. Creating Connections

```pascal
uses
  mormot.db.sql,
  mormot.db.sql.postgres;

var
  Props: TSqlDBPostgresConnectionProperties;
begin
  // PostgreSQL direct
  Props := TSqlDBPostgresConnectionProperties.Create(
    'localhost:5432', 'mydb', 'user', 'password');

  // Connection string alternative
  Props := TSqlDBPostgresConnectionProperties.Create(
    'host=localhost port=5432 dbname=mydb', '', 'user', 'password');
end;
```

### 8.2.2. Provider-Specific Examples

**Oracle (direct OCI):**
```pascal
uses
  mormot.db.sql.oracle;

Props := TSqlDBOracleConnectionProperties.Create(
  'myserver/orcl', '', 'scott', 'tiger');

// With TNS alias
Props := TSqlDBOracleConnectionProperties.Create(
  'PROD_DB', '', 'user', 'password');
```

**MS SQL Server (ODBC):**
```pascal
uses
  mormot.db.sql.odbc;

Props := TSqlDBOdbcConnectionProperties.Create(
  'Driver={SQL Server};Server=.\SQLEXPRESS;Database=mydb',
  '', 'user', 'password');

// Windows authentication
Props := TSqlDBOdbcConnectionProperties.Create(
  'Driver={SQL Server};Server=.\SQLEXPRESS;Database=mydb;Trusted_Connection=Yes',
  '', '', '');
```

**Zeos (recommended for cross-database):**
```pascal
uses
  mormot.db.sql.zeos;

// PostgreSQL via Zeos
Props := TSqlDBZeosConnectionProperties.Create(
  'zdbc:postgresql://localhost:5432/mydb', '', 'user', 'password');

// MySQL via Zeos
Props := TSqlDBZeosConnectionProperties.Create(
  'zdbc:mysql://localhost:3306/mydb', '', 'user', 'password');

// Firebird via Zeos
Props := TSqlDBZeosConnectionProperties.Create(
  'zdbc:firebird://localhost/C:\Data\mydb.fdb', '', 'SYSDBA', 'masterkey');
```

### 8.2.3. Connection Properties Lifecycle

```pascal
var
  Props: TSqlDBConnectionProperties;
begin
  // Create once, use throughout application
  Props := TSqlDBPostgresConnectionProperties.Create(...);
  try
    // Props manages connection pool internally
    UseDatabase(Props);
  finally
    Props.Free;  // Release all connections
  end;
end;
```

**Important**: Create `TSqlDBConnectionProperties` once per application. It manages connection pooling automatically.

---

## 8.3. Executing Queries

### 8.3.1. ISqlDBRows Interface

The simplest and safest pattern:

```pascal
var
  Rows: ISqlDBRows;
begin
  Rows := Props.Execute(
    'SELECT * FROM Customer WHERE Country = ?', ['USA']);

  while Rows.Step do
    WriteLn(Rows['Name'], ' - ', Rows['City']);

  // No try..finally needed - interface auto-releases
end;
```

### 8.3.2. Statement-Based Access

For more control:

```pascal
var
  Conn: TSqlDBConnection;
  Stmt: ISqlDBStatement;  // Interface - auto-released
begin
  Conn := Props.ThreadSafeConnection;  // Thread-safe connection
  Stmt := Conn.NewStatementPrepared(
    'SELECT * FROM Customer WHERE Country = ?', True);  // True = cache

  Stmt.BindTextU(1, 'USA');  // Use BindTextU for RawUtf8 strings
  Stmt.ExecutePrepared;

  while Stmt.Step do
    WriteLn(Stmt.ColumnUtf8(0));
  // No try..finally needed - interface auto-releases
end;
```

### 8.3.3. Late-Binding Access

For convenient (but slower) field access:

```pascal
var
  Row: Variant;
begin
  with Props.Execute(
    'SELECT * FROM Customer WHERE Country = ?',
    ['USA'], @Row) do
  while Step do
    WriteLn(Row.Name, ' - ', Row.City);  // Late-binding!
end;
```

### 8.3.4. Direct JSON Output

Export query results directly to JSON:

```pascal
var
  Json: RawUtf8;
begin
  Json := Props.Execute(
    'SELECT ID, Name, Email FROM Customer', []).FetchAllAsJson(True);
  // Returns: [{"ID":1,"Name":"John","Email":"john@example.com"},...]
end;
```

---

## 8.4. Field Types

### 8.4.1. TSqlDBFieldType

| Type | Delphi Type | Description |
|------|-------------|-------------|
| `ftNull` | - | SQL NULL |
| `ftInt64` | `Int64` | Any integer |
| `ftDouble` | `Double` | Floating-point |
| `ftCurrency` | `Currency` | Fixed 4 decimals |
| `ftDate` | `TDateTime` | Date and time |
| `ftUtf8` | `RawUtf8` | Unicode text |
| `ftBlob` | `RawByteString` | Binary data |

### 8.4.2. Column Access Methods

```pascal
// Type-safe access
Value := Stmt.ColumnInt(0);       // Int64
Value := Stmt.ColumnDouble(1);    // Double
Value := Stmt.ColumnCurrency(2);  // Currency
Value := Stmt.ColumnUtf8(3);      // RawUtf8
Value := Stmt.ColumnDateTime(4);  // TDateTime
Value := Stmt.ColumnBlob(5);      // RawByteString

// Null checking
if Stmt.ColumnNull(0) then
  WriteLn('Value is NULL');

// Variant access (auto-conversion)
Value := Stmt.Column[0];  // Returns Variant
Value := Stmt['ColumnName'];  // By name
```

---

## 8.5. Parameter Binding

### 8.5.1. Positional Parameters

```pascal
// Use ? for positional parameters
Stmt := Conn.NewStatementPrepared(
  'INSERT INTO Customer (Name, Email, Age) VALUES (?, ?, ?)', True);
Stmt.BindTextU(1, 'John Doe');       // Use BindTextU for RawUtf8 strings
Stmt.BindTextU(2, 'john@example.com');
Stmt.Bind(3, Int64(30));             // Use Bind with Int64 for integers
Stmt.ExecutePrepared;
```

### 8.5.2. Array Binding (Bulk Insert)

High-performance bulk operations:

```pascal
// PostgreSQL, Oracle, and MSSQL support array binding
Stmt := Conn.NewStatementPrepared(
  'INSERT INTO Log (Timestamp, Message) VALUES (?, ?)', True);

Stmt.BindArray(1, DateTimes);   // TDateTimeDynArray
Stmt.BindArray(2, Messages);    // TRawUtf8DynArray
Stmt.ExecutePrepared;           // Single network roundtrip!
```

### 8.5.3. BLOB Parameters

```pascal
var
  Data: RawByteString;
begin
  Data := StringFromFile('image.png');
  Stmt.Bind(1, ftBlob, Data);  // Explicit type
  Stmt.ExecutePrepared;
end;
```

---

## 8.6. Transactions

### 8.6.1. Explicit Transactions

```pascal
var
  Conn: TSqlDBConnection;
begin
  Conn := Props.ThreadSafeConnection;
  Conn.StartTransaction;
  try
    Conn.Execute('UPDATE Account SET Balance = Balance - 100 WHERE ID = 1');
    Conn.Execute('UPDATE Account SET Balance = Balance + 100 WHERE ID = 2');
    Conn.Commit;
  except
    Conn.Rollback;
    raise;
  end;
end;
```

### 8.6.2. Nested Transactions

```pascal
// Most databases don't support true nested transactions
// mORMot uses SAVEPOINTs where available
Conn.StartTransaction;      // Begin
try
  Conn.StartTransaction;    // SAVEPOINT (if supported)
  try
    // Operations
    Conn.Commit;            // RELEASE SAVEPOINT
  except
    Conn.Rollback;          // ROLLBACK TO SAVEPOINT
    raise;
  end;
  Conn.Commit;              // COMMIT
except
  Conn.Rollback;            // ROLLBACK
end;
```

---

## 8.7. ORM Integration

### 8.7.1. Mapping TOrm to External Database

```pascal
uses
  mormot.orm.sql,
  mormot.db.sql.postgres;

var
  Props: TSqlDBPostgresConnectionProperties;
  Model: TOrmModel;
  Server: TRestServerDB;
begin
  // Create connection properties
  Props := TSqlDBPostgresConnectionProperties.Create(
    'localhost:5432', 'mydb', 'user', 'password');

  // Create model
  Model := TOrmModel.Create([TOrmCustomer, TOrmOrder]);

  // Map ORM classes to external database
  OrmMapExternal(Model, TOrmCustomer, Props);
  OrmMapExternal(Model, TOrmOrder, Props);

  // Create server (SQLite3 for other tables)
  Server := TRestServerDB.Create(Model, ':memory:');

  // Now ORM operations go to PostgreSQL
  Server.Orm.Add(Customer, True);
end;
```

### 8.7.2. Table and Field Name Mapping

```pascal
// Custom table name
OrmMapExternal(Model, TOrmCustomer, Props, 'CUSTOMERS');

// Custom field mapping (fluent interface via Props)
Model.Props[TOrmCustomer].ExternalDB
  .MapField('Name', 'CUSTOMER_NAME')
  .MapField('Email', 'EMAIL_ADDRESS');

// Or in one call with table name
OrmMapExternal(Model, TOrmCustomer, Props, 'CUSTOMERS');
Model.Props[TOrmCustomer].ExternalDB.MapField('Name', 'CUSTOMER_NAME');
```

### 8.7.3. Auto-Create Tables

```pascal
// mORMot can create tables in external database
Server.CreateMissingTables;  // Creates in PostgreSQL
```

---

## 8.8. Multi-Database Scenarios

### 8.8.1. Mixing Storage Backends

```pascal
var
  PostgresProps, OracleProps: TSqlDBConnectionProperties;
begin
  // Different databases for different tables
  PostgresProps := TSqlDBPostgresConnectionProperties.Create(...);
  OracleProps := TSqlDBOracleConnectionProperties.Create(...);

  Model := TOrmModel.Create([
    TOrmUser,     // Internal SQLite3
    TOrmProduct,  // PostgreSQL
    TOrmLegacy    // Oracle
  ]);

  OrmMapExternal(Model, TOrmProduct, PostgresProps);
  OrmMapExternal(Model, TOrmLegacy, OracleProps);

  Server := TRestServerDB.Create(Model, 'users.db3');
end;
```

### 8.8.2. Cross-Database Queries

Thanks to SQLite3 virtual tables, you can JOIN across databases:

```pascal
// This works even though Product is in PostgreSQL and User is in SQLite3!
Server.Orm.ExecuteList([TOrmProduct, TOrmUser],
  'SELECT Product.Name, User.Email ' +
  'FROM Product, User ' +
  'WHERE Product.OwnerID = User.ID');
```

---

## 8.9. Database-Specific Notes

### 8.9.1. PostgreSQL

```pascal
// Direct libpq access (fastest)
uses mormot.db.sql.postgres;

Props := TSqlDBPostgresConnectionProperties.Create(
  'localhost:5432', 'dbname', 'user', 'pass');

// Connection string options
Props := TSqlDBPostgresConnectionProperties.Create(
  'host=localhost port=5432 dbname=mydb sslmode=require',
  '', 'user', 'pass');
```

**Tips:**
- Use array binding for bulk inserts
- PostgreSQL handles UTF-8 natively
- JSONB columns work well with `TDocVariant`

### 8.9.2. Oracle

```pascal
// Direct OCI access (fastest for Oracle)
uses mormot.db.sql.oracle;

Props := TSqlDBOracleConnectionProperties.Create(
  'server/service', '', 'user', 'pass');

// Array binding for bulk operations (use BindArray method)
Stmt := Conn.NewStatementPrepared('INSERT INTO items VALUES (?,?)', false);
Stmt.BindArray(1, IDs);      // Bind array of IDs
Stmt.BindArray(2, Names);    // Bind array of names
Stmt.ExecutePrepared;
```

**Tips:**
- Use array binding via `ISqlDBStatement.BindArray()` method (Oracle excels at this)
- Direct OCI is much faster than ODBC/OleDB
- Large BLOBs require special handling

### 8.9.3. MS SQL Server

```pascal
// ODBC (recommended)
uses mormot.db.sql.odbc;

Props := TSqlDBOdbcConnectionProperties.Create(
  'Driver={ODBC Driver 17 for SQL Server};Server=.\SQLEXPRESS;Database=mydb',
  '', 'user', 'pass');

// OleDB alternative
uses mormot.db.sql.oledb;

Props := TSqlDBOleDBConnectionProperties.Create(
  'Provider=SQLNCLI11;Server=.\SQLEXPRESS;Database=mydb',
  '', 'user', 'pass');
```

**Tips:**
- Use ODBC Driver 17+ for best performance
- Windows authentication: `Trusted_Connection=Yes`
- MARS (Multiple Active Result Sets) may cause issues

### 8.9.4. MySQL/MariaDB

```pascal
// Zeos (recommended)
uses mormot.db.sql.zeos;

Props := TSqlDBZeosConnectionProperties.Create(
  'zdbc:mysql://localhost:3306/mydb', '', 'user', 'pass');

// ODBC alternative
Props := TSqlDBOdbcConnectionProperties.Create(
  'Driver={MySQL ODBC 8.0 Driver};Server=localhost;Database=mydb',
  '', 'user', 'pass');
```

**Tips:**
- Use UTF-8 character set (`SET NAMES utf8mb4`)
- InnoDB engine for transactions
- MariaDB is generally faster than MySQL

---

## 8.10. Performance Tuning

### 8.10.1. Connection Pooling

```pascal
// Connection pool is automatic and internally managed
Props := TSqlDBPostgresConnectionProperties.Create(...);

// Pool size is determined by connection usage patterns
// Access current pool count (read-only):
WriteLn('Active connections: ', Props.ConnectionPoolCount);
```

> **Note**: Connection pooling is automatic. The pool grows dynamically based on usage and is internally managed. There are no configurable max count or timeout properties.

### 8.10.2. Statement Caching

```pascal
// Enable statement caching (True = cache)
Stmt := Conn.NewStatementPrepared(SQL, True);

// Clear cache if needed
Conn.ClearStatementCache;
```

### 8.10.3. Batch Operations

```pascal
// Use ExecuteNoResult for multiple statements
Conn.ExecuteNoResult('DELETE FROM TempTable');
Conn.ExecuteNoResult('INSERT INTO TempTable SELECT * FROM Source');

// Or use batch interface
Batch := TRestBatch.Create(Server.Orm, TOrmCustomer);
try
  for i := 1 to 10000 do
    Batch.Add(CreateCustomer(i), True);
  Server.Orm.BatchSend(Batch);  // Single transaction
finally
  Batch.Free;
end;
```

---

## 8.11. TDataSet Integration

For VCL/FMX applications that need `TDataSet`:

### 8.11.1. Read-Only DataSet

```pascal
uses
  mormot.db.rad.ui.sql;

var
  DataSet: TSqlDataSet;
begin
  DataSet := TSqlDataSet.Create(Self);
  DataSet.Connection := Props;
  DataSet.SQL.Text := 'SELECT * FROM Customer';
  DataSet.Open;

  DBGrid1.DataSource.DataSet := DataSet;
end;
```

### 8.11.2. From ISqlDBRows

```pascal
uses
  mormot.db.rad.ui.sql;

var
  Rows: ISqlDBRows;
begin
  Rows := Props.Execute('SELECT * FROM Customer', []);
  DBGrid1.DataSource.DataSet := ToDataSet(Self, Rows);
end;
```

---

## 8.12. Migration from mORMot 1

### 8.12.1. Unit Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `SynDB.pas` | `mormot.db.sql.pas` |
| `SynDBOracle.pas` | `mormot.db.sql.oracle.pas` |
| `SynDBODBC.pas` | `mormot.db.sql.odbc.pas` |
| `SynOleDB.pas` | `mormot.db.sql.oledb.pas` |
| `SynDBZeos.pas` | `mormot.db.sql.zeos.pas` |
| `SynDBSQLite3.pas` | `mormot.db.sql.sqlite3.pas` |
| `SynDBDataset.pas` | `mormot.db.rad.pas` |
| `SynDBFireDAC.pas` | `mormot.db.rad.firedac.pas` |
| `mORMotDB.pas` | `mormot.orm.sql.pas` |

### 8.12.2. Class Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `TSQLDBConnectionProperties` | `TSqlDBConnectionProperties` |
| `TSQLDBConnection` | `TSqlDBConnection` |
| `TSQLDBStatement` | `TSqlDBStatement` |
| `ISQLDBRows` | `ISqlDBRows` |
| `TSQLDBFieldType` | `TSqlDBFieldType` |
| `VirtualTableExternalRegister` | `OrmMapExternal` |

### 8.12.3. API Changes

```pascal
// mORMot 1
VirtualTableExternalRegister(Model, TSQLCustomer, Props);

// mORMot 2
OrmMapExternal(Model, TOrmCustomer, Props);
```

---

*Next Chapter: External NoSQL Database Access (MongoDB)*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 7: Database Layer](mORMot2-SAD-Chapter-07.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 9: External NoSQL Database Access](mORMot2-SAD-Chapter-09.md) |
