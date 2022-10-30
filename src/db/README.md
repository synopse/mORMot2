# mORMot Database Units

## Folder Content

This folder hosts the SQL and NoSQL *Data Access Layer* of the *mORMot* Open Source framework, version 2.

We redefined a genuine hierarchy of DB classes, to directly access the database engines. It is not grounded on the Delphi/FPC regular DB classes, to avoid the complexity of the `DB.pas` unit, mainly the `TDataSet` component and all the low-level supported types, rooted on last century databases limitations.

These classes focus on:
- Easy to use, via a small set of types, classes and interfaces;
- Perfect integration with our RESTful ORM/ODM, especially via direct JSON support;
- Highest performance possible.

Note that those units can be used standalone, without our ORM, when `DB.pas` is not an option, for instance:

- with the Delphi Community Edition, 
- or if you don't need/like the RAD approach, but expect efficient SQL execution.

In addition to regular SQL data sources, we support NoSQL databases, like *MongoDB*. Some SQL-to-NoSQL translation layer is available and used by `mormot.core.orm`, to convert most simple SELECT statements to the corresponding *MongoDB* pipelines.

## Access Databases

There are several ways of access your data with our framework:

- `mormot.db.core.pas` for SQL and NoSQL high-level shared definitions;
- `mormot.db.raw.*.pas` units define Direct access to the database servers;
- `mormot.db.sql.*.pas` units give access to SQL databases;
- `mormot.db.rad.*.pas` units interfacing `TDataSet` third-party SQL libraries like FireDac;
- `mormot.db.nosql.*.pas` units give access to NoSQL databases, like *MongoDB*.

For cross-databases support, `mormot.db.sql.zeos.pas` is the preferred way. But you also have direct access to MSSQL via *ODBC/OleDB*, Oracle via *OCI*, or PostgreSQL via *libpq*, to reduce the dependency chain.

For instance, `mormot.db.raw.postgres.pas` defines the low-level PostgreSQL client to the `libpq` provider, whereas `mormot.db.sql.postgres.pas` will use it to implement `mormot.db.sql.pas` compatible SQL requests.

The `mormot.db.core.pas` unit is shared against all those units and by `mormot.orm.core`.


## Units Presentation

### mormot.db.core

Shared Types and Definitions for Database Access
- Shared Database Fields and Values Definitions
- Nullable Values Stored as Variant
- Date/Time SQL encoding
- SQL Parameters Inlining and Processing
- `TResultsWriter` Specialized for Database Export
- `TSelectStatement` SQL SELECT Parser
- JSON Object Decoder and SQL Generation
- `TID` Processing Functions

This unit is used by both `mormot.db.*` units and `mormot.orm.*` units.

### mormot.db.sql

Shared Types and Definitions for SQL Database Access
- SQL Fields and Columns Definitions
- Define Database Engine Specific Behavior (as used by our ORM)
- General SQL Processing Functions
- Abstract SQL DB Classes and Interfaces
- Parent Classes for Thread-Safe and Parametrized Connections

And the associated `mormot.db.sql.*.pas` / `mormot.db.raw.*.pas` units for ODBC, OleDB, Zeos/ZDBC providers, and direct Oracle, PostgreSQL, SQLite3, FireBird/IBX database clients.

### mormot.db.rad

Parent Classes for `TDataSet` / `DB.pas` Database Access
- Shared Wrappers Around `DB.pas` Classes and Functions
- Database-Aware BCD Values Support
- `mormot.db.sql` Abstract Connection for `DB.pas TDataSet`

And the associated `mormot.db.rad.*.pas` units for FireDac, UniDac, BDE, NexusDB.

### mormot.db.rad.ui

Efficient Read/Only Abstract `TDataSet` for VCL/LCL/FMX UI
- Cross-Compiler `TVirtualDataSet` Read/Only Data Access
- JSON and Variants `TDataSet` Support

### mormot.db.rad.ui.sql

Efficient Read/Only `TDataSet` Working With `mormot.db.sql`
- `TBinaryDataSet` Filled From a `TSqlDBStatement` ResultSet
- `TSqlDataSet` For Direct `TSqlDBConnection` Sql Execution

### mormot.db.rad.ui.orm

Efficient Read/Only `TDataSet` for ORM and JSON Process
- `TOrmTableDataSet` for `TOrmTable`/JSON access
- JSON/ORM to `TDataSet` Wrappers Functions

### mormot.db.rad.ui.cds

Efficient Read/Write `TClientDataSet` for ORM and JSON Process
- Fill a VCL TClientDataset from `TOrmTable`/`TOrmTableJson` data

### mormot.db.nosql.bson

Efficient BSON Support for MongoDB Clients
- BSON Decimal128 Value
- BSON ObjectID Value
- `TBSONVariantData` / `TBSONVariant` Custom Variant Storage
- `TBSONElement` / `TBSONIterator` for BSON Decoding
- `TBSONWriter` for BSON Encoding
- High-Level BSON/JSON Function Helpers

### mormot.db.nosql.mongodb

MongoDB Client for NoSQL Data Access
- MongoDB Wire Protocol Definitions
- MongoDB Protocol Classes
- MongoDB Client Classes

### mormot.db.proxy

Allow Remote HTTP Access of any `mormot.db.sql` connections via a Relay
- Shared Proxy Information
- Server-Side Proxy Remote Protocol
- Client-Side Proxy Remote Protocol
- HTTP Server Classes for Remote Access
- HTTP Client Classes for Remote Access
