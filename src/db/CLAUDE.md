# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `/mnt/w/mORMot2/src/db` directory contains mORMot 2's **Data Access Layer** - a lightweight, high-performance SQL and NoSQL database abstraction that deliberately avoids the complexity and overhead of Delphi's traditional `TDataSet`/`DB.pas` architecture.

**Design Philosophy:**
- Simple, focused API with minimal types
- Direct JSON integration for REST/ORM
- Highest possible performance
- Cross-database compatibility
- Works standalone (no dependency on `mormot.orm.*`)

## Architecture Layers

### Core Abstractions (`mormot.db.core.pas` + `mormot.db.sql.pas`)

**Key Types:**
- `TSqlDBFieldType` - Abstract field types (ftInt64, ftUtf8, ftBlob, ftCurrency, ftDate, etc.)
- `TSqlDBDefinition` - Database engine types: dOracle, dMSSQL, dSQLite, dPostgreSQL, dMySQL, dMariaDB, dFirebird, dNexusDB, dDB2, dInformix, dJet
- `TSqlDBConnection` - Abstract database connection (not thread-safe)
- `TSqlDBConnectionThreadSafe` - Thread-safe connection wrapper
- `TSqlDBStatement` - Abstract prepared statement execution
- `TSqlDBConnectionProperties` - Connection factory with metadata access

**Core Constants:**
- `MAX_SQLFIELDS` - Maximum columns per table (default 64, configurable to 128/192/256)
- `DB_FIELDS[TSqlDBDefinition]` - SQL type definitions per engine
- `DB_PARAMSMAX[TSqlDBDefinition]` - Max bound parameters per engine
- `DB_SQLLIMITCLAUSE[TSqlDBDefinition]` - LIMIT syntax per engine

### Database Provider Hierarchy

**Direct Database Access** (`mormot.db.raw.*.pas` → `mormot.db.sql.*.pas`):
```
mormot.db.raw.sqlite3.pas    → mormot.db.sql.sqlite3.pas
mormot.db.raw.postgres.pas   → mormot.db.sql.postgres.pas
mormot.db.raw.oracle.pas     → mormot.db.sql.oracle.pas
mormot.db.raw.odbc.pas       → mormot.db.sql.odbc.pas
mormot.db.raw.oledb.pas      → mormot.db.sql.oledb.pas
```

**Recommended:** `mormot.db.sql.zeos.pas` for cross-database support (uses ZDBC library)

**Third-Party `TDataSet` Bridges** (`mormot.db.rad.*.pas`):
```
mormot.db.rad.pas          - Base TDataSet wrapper
mormot.db.rad.firedac.pas  - FireDac integration
mormot.db.rad.unidac.pas   - UniDac integration
mormot.db.rad.bde.pas      - BDE integration
mormot.db.rad.nexusdb.pas  - NexusDB integration
```

**UI Integration** (VCL/LCL/FMX read-only datasets):
```
mormot.db.rad.ui.pas       - TVirtualDataSet base
mormot.db.rad.ui.sql.pas   - SQL statement to TDataSet
mormot.db.rad.ui.orm.pas   - TOrmTable to TDataSet
mormot.db.rad.ui.cds.pas   - TClientDataSet wrapper (read-write)
```

### NoSQL Support

**MongoDB** (`mormot.db.nosql.*.pas`):
- `mormot.db.nosql.bson.pas` - BSON encoding/decoding, variant support
- `mormot.db.nosql.mongodb.pas` - Wire protocol client (supports MongoDB 5.1+)
  - Uses new OP_MSG/OP_COMPRESSED protocol
  - Define `MONGO_OLDPROTOCOL` for MongoDB < 3.6

### Remote Database Proxy

**HTTP-Based Relay** (`mormot.db.proxy.pas`):
- Expose any `TSqlDBConnection` via HTTP
- Binary protocol for efficient remote access
- Client/server classes for transparent usage

## Common Patterns

### Creating a Connection

```pascal
// Direct PostgreSQL connection
Props := TSqlDBPostgresConnectionProperties.Create(
  'localhost:5432', 'mydb', 'user', 'pass');
Conn := Props.ThreadSafeConnection; // Thread-safe wrapper

// Zeos (cross-database)
Props := TSqlDBZeosConnectionProperties.Create(
  'postgresql://localhost:5432/mydb', '', 'user', 'pass');
```

### Executing Queries

```pascal
Stmt := Conn.NewStatementPrepared('SELECT * FROM users WHERE id=?', True);
try
  Stmt.Bind(1, 42);
  Stmt.ExecutePrepared;
  while Stmt.Step do
    WriteLn(Stmt.ColumnUtf8(0));
finally
  Stmt.Free;
end;
```

### Database-Specific Behavior

**LIMIT Syntax Handling:**
- Oracle: `WHERE rownum<=N`
- MSSQL: `SELECT TOP(N)`
- MySQL/SQLite/PostgreSQL: `LIMIT N`
- Firebird: `SELECT FIRST N`

Use `TSqlDBConnectionProperties.AdaptSqlLimitForEngineList()` for automatic conversion.

**Field Size Limits:**
- Oracle: 1333 chars (4000 bytes / 3 for UTF-8)
- MSSQL/MySQL/MariaDB: 4000 chars
- Firebird: 32760 chars
- PostgreSQL/SQLite: No limit (set to 0)

**Batch Parameters:**
- SQLite: 500 (theoretical 999)
- Oracle/MSSQL/MySQL/PostgreSQL: 500
- NexusDB: 100 (empirical)

## Key Differences from DB.pas

| Feature | mORMot DB | TDataSet/DB.pas |
|---------|-----------|-----------------|
| JSON Support | Native | Manual |
| Threading | Explicit thread-safe wrappers | Not thread-safe |
| Memory | Minimal overhead | Heavy component stack |
| Delphi CE | Fully compatible | Restricted |
| NoSQL | MongoDB support | SQL only |
| Remote Access | Built-in proxy | Requires DataSnap/etc |

## Important Notes

- **Thread Safety:** Use `TSqlDBConnectionThreadSafe` or `Props.ThreadSafeConnection` for multi-threaded access. Base `TSqlDBConnection` is NOT thread-safe.
- **Statement Caching:** Prepared statements are cached by connection. Use `NewStatementPrepared(..., True)` to enable caching.
- **Parameter Binding:** Always use `?` placeholders and `Bind()` methods. Framework handles conversion to engine-specific syntax (`:AA`, `$1`, etc.).
- **JSON Integration:** Use `FetchAllAsJson()`, `ColumnBlob()` with JSON, or `TResultsWriter` for direct JSON export.
- **Logging:** Define `SYNDB_SILENCE` to disable statement execution logging.

## DuckDB Provider: Review Notes and Future Optimizations

Source: Arnaud Bouchez review of PR #666 (`mormot.db.raw.duckdb` / `mormot.db.sql.duckdb`),
https://github.com/synopse/mORMot2/issues/423#issuecomment-6218806358 (2026-10-16).
He merged the PR and started a cleanup on master (commits `2d1593c57`..`587ce1674`), but
said it is not finished. Read the current trunk code before touching these units.

**What DuckDB is for in mORMot:** an analytical (OLAP) query engine over large datasets
(CSV, Parquet, attached PostgreSQL...), not a replacement for SQLite3 as in-process
transactional/memory database. Row-by-row INSERT speed is far below SQLite3 - do not
benchmark or sell it on insertion numbers.

### Open TODOs (priority order)

1. **Implement `TOnBatchInsert` for DuckDB** (`TSqlDBConnectionProperties.OnBatchInsert`,
   see `mormot.db.sql.pas`). This is the minimum: it is used directly by the ORM batch
   process, and is easily implemented on top of the `TSqlDBDuckDBAppender` logic.
2. **Remove the array binding (`BindArray`) feature** from `TSqlDBDuckDBStatement`
   (`BindArrayRow` and the per-row loop in `ExecutePrepared`). It still executes one
   statement per row, so it is *slower* than plain manual inserts - it is just bloat.
   Note: mormot.db.proxy batches (e.g. a DuckDB proxy server) rely on it today, so route
   proxy batches to the appender / `OnBatchInsert` first.
3. **Maybe** use `duckdb_append_data_chunk` for bulk loading - unclear whether it is
   worth it; measure against the appender before adopting.
4. **Static linking** of the `.a` (shipped with each DuckDB release) - low priority:
   the library is ~60MB, not a trivial dependency.
5. Fix Delphi warning W1073 in `mormot.db.raw.duckdb.pas` (`DuckTimeToText`: signed
   constant mixed with `QWord` in `u mod DUCKDB_MICROSPERSEC`).

### VARIANT columns (DuckDB 1.5+)

VARIANT results are currently returned as JSON text, **without decoding the binary
layout**: `DuckDBVariantSql()` re-executes the statement wrapped in
`SELECT #1 AS "a", NULLIF(#2::JSON, 'null'::JSON) AS "b" FROM (sql)` so that DuckDB
does the conversion (embedded: `TSqlDBDuckDBStatement.ExecuteVariantAsJson`, Quack:
`TSqlDBQuackStatement.ExecutePrepared`, detected via `TQuackResponse.VariantColumns`).
Known limits: the statement runs twice, a VARIANT in a multi-statement SQL (dynamic
PIVOT, `fDirectQuery`) raises, and values are JSON text, not typed.

Possible future work - decode the binary format natively (est. 400-600 lines):
- Physical layout: `STRUCT(keys LIST(VARCHAR), children LIST(STRUCT(keys_index,
  values_index)), values LIST(STRUCT(type_id, byte_offset)), data BLOB)`, see DuckDB
  `variant.hpp` (LogicalTypeId 109, C API `DUCKDB_TYPE_VARIANT` = 41).
- Walk from `values[0]`: primitives are fixed-size little-endian at `byte_offset`,
  VARCHAR/BLOB are LEB128 length + bytes, DECIMAL has width/scale, OBJECT/ARRAY have a
  LEB128 count and a start index in `children`. Output as JSON or `TDocVariant`.
- Benefits: no second execution, no multi-statement limit, faster, typed values.
- Risks: the format is not documented as stable (type ids and layout may differ between
  1.5.x and 2.x), and on-disk shredded variants must be checked. A wrong decoding is a
  silent data error: keep the SQL JSON path as fallback for any unknown `type_id`, and
  compare against `::JSON` on real data in the tests.

### Code quality

The general code-quality rules derived from this review (parse once, no duplicated
code, reuse core units, no transient RawUtf8, benchmark against the naive path...) are in
the root `CLAUDE.md` and apply to all code in this repository.

## Testing

The database layer is tested through the mORMot ORM test suite. Individual database providers require their respective client libraries installed.

## File Navigation

- **Core abstractions:** `mormot.db.core.pas`, `mormot.db.sql.pas`
- **Direct providers:** Search `mormot.db.sql.*.pas` for your database
- **RAD bridges:** Check `mormot.db.rad.*.pas` for TDataSet integration
- **NoSQL:** Start with `mormot.db.nosql.mongodb.pas`
- **Remote:** See `mormot.db.proxy.pas` for HTTP relay

## Related Documentation

**📖 SAD Chapters**:
- [Chapter 7: SQL Database Access](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-07.md) - SynDB architecture, connections
- [Chapter 8: SQLite3 Database](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-08.md) - SQLite3 features, virtual tables
- [Chapter 9: External NoSQL Database Access](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-09.md) - MongoDB integration

**Framework References**:
- Main README: `/mnt/w/mORMot2/src/db/README.md`
- ORM layer: `/mnt/w/mORMot2/src/orm/`
- REST layer: `/mnt/w/mORMot2/src/rest/`
- Core types: `/mnt/w/mORMot2/src/core/`
