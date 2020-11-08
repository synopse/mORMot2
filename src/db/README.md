# mORMot Database Units

## Folder Content

This folder hosts the SQL and NoSQL *Data Access Layer* of the *mORMot* Open Source framework, version 2.

We redefined a genuine hierarchy of DB classes, to directly access the database engines. It is not grounded on the Delphi/FPC regular DB classes, to avoid the complexity of the `DB.pas` unit, mainly the `TDataSet` component and all the low-level supported types.

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
- `mormot.db.rad.*.pas` units publishing `TDataSet` third-party SQL libraries;
- `mormot.db.nosql.*.pas` units give access to NoSQL databases, like *MongoDB*.

For cross-databases support, `mormot.db.sql.zeos.pas` is the preferred way. But you also have direct access to MSSQL via *ODBC/OleDB*, Oracle via *OCI*, or PostgreSQL via *libpq*, to reduce the dependency chain.

For instance, `mormot.db.raw.postgres.pas` defines the low-level PostgreSQL client to the `libpq` provider, whereas `mormot.db.sql.postgres.pas` will use it to implement `mormot.db.sql.pas` compatible SQL requests.

The `mormot.db.core.pas` unit is shared against all those units and by `mormot.orm.core`.
