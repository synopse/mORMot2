# mORMot Database Units

## Folder Content

This folder hosts the *Data Acces layer* of the *mORMot* Open Source framework, version 2.

## Access Databases

There are several ways of access your data with our framework:

- `mormot.db.core` for SQL and NoSQL high-level definitions;
- `mormot.db.sql` shared unit for abstract direct SQL connection;
- Direct access to the database server, via `mormot.db.raw.*`;
- Access to SQL databases via their regular clients, via `mormot.db.sql.*` units - either directly speaking with the DB provider, or via `TDataSet` and third-party libraries - see `mormot.db.sql.ds.*` units;
- Direct access to NoSQL databases, via `mormot.db.nosql.*` units.

A `mormot.db.core` unit is shared against all those units.