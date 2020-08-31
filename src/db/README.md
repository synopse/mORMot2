# mORMot Database Units

## Folder Content

This folder hosts the *Data Acces layer* of the *mORMot* Open Source framework, version 2.

## Access Databases

There are several ways of access your data with our framework:

- `mormot.db.core` for SQL and NoSQL high-level definitions;
- `mormot.db.raw.*` units define Direct access to the database servers;
- `mormot.db.core` shared unit for abstract direct SQL connection;
- `mormot.db.sql.*` units give access to SQL databases;
- `mormot.db.rad.*` units publishing `TDataSet` third-party SQL libraries;
- `mormot.db.nosql.*` units give access to NoSQL databases.

A `mormot.db.core` unit is shared against all those units.