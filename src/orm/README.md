# mORMot REST Object-Relational-Mapping ORM Units

## Folder Content

This folder hosts the *RESTful ORM* high-level features of the *mORMot* Open Source framework, version 2.

## ORM/ODM Features

*mORMot* implements a simple and very efficient Object-Relational-Mapping:

- The parent `TOrm` class give access to ORM methods at object level;
- `IRestOrm IRestOrmServer IRestOrmClient` interfaces can be used in business code for abstract and persistent-agnostic data access;
- UTF-8 and JSON are used from the ground up, to make our ORM easy to consume in a REST approach, as defined in the `/src/rest` folder;
- It can leverage *SQlite3* as its kernel, to access several data sources over virtual tables;
- A high-performance in-memory engine, using JSON or binary on disk, can be used instead of *SQlite3*;
- Switch to a *NoSQL* database like *MongoDB* is possible, transforming our ORM to an ODM - aka *Object-Document-Mapping*.

## Units Presentation

### mormot.orm.base

Low-Level Basic Types and Definitions for our RESTful ORM
- Shared ORM/JSON Fields and Values Definitions
- JSON Object Decoder and SQL Generation
- ORM Ready UTF-8 Comparison Functions
- `TJsonSerializer` Class for `TOrm` Serialization
- Abstract `TOrmPropInfo` Parent Class
- Abstract `TOrmTableAbstract` Parent Class
- `TOrmTableRowVariant` Custom Variant Type

### mormot.orm.core

Main Shared Types and Definitions for our RESTful ORM
- `TOrmPropInfo` Classes for Efficient ORM Processing
- `IRestOrm IRestOrmServer IRestOrmClient` Definitions
- `TOrm` Definition
- `RecordRef` Wrapper Definition
- `TOrmTable TOrmTableJSON` Definitions
- `TOrmMany` Definition
- `TOrmVirtual` Definitions
- `TOrmProperties` Definitions
- `TOrmModel TOrmModelProperties` Definitions
- `TRestCache` Definition
- `TRestBatch TRestBatchLocked` Definitions
- `TSynValidateRest TSynValidateUniqueField` Definitions
- `TOrmAccessRights` Definition
- `TOrm` High-Level Parents

This unit is not depending from `mormot.rest.core` so can be used as a pure ORM layer for your projects.

The `IRestOrm` interface is the main SOLID entry point of all ORM process.

### mormot.orm.rest

`IRestOrm` Implementation as used by `TRest`
- Some definitions Used by `TRestOrm` Implementation
- `TRestOrm` Parent Class for abstract REST client/server

### mormot.orm.client

Client-Side Object-Relational-Mapping (ORM) Process
- `TRestOrmClient` Abstract Client
- `TRestOrmClientURI` REST Client from URI

### mormot.orm.server

Server-Side Object-Relational-Mapping (ORM) Process
- `TRestOrmServer` Abstract Server

### mormot.orm.storage

Server-Side Storage Process using JSON or Binary Persistence
- Virtual Table ORM Support
- `TRestStorage` Abstract Class for ORM/REST Storage
- `TRestStorageInMemory` as Stand-Alone JSON/Binary Storage
- `TOrmVirtualTableJSON`/`TOrmVirtualTableBinary` Virtual Tables
- `TRestStorageRemote` for CRUD Redirection
- `TRestStorageShard` as Abstract Sharded Storage Engine

### mormot.orm.sql

ORM SQL Database Access using `mormot.db.sql` units
- `TRestStorageExternal` for ORM/REST Storage over SQL
- `TOrmVirtualTableExternal` for External SQL Virtual Tables
- External SQL Database Engines Registration Functions

### mormot.orm.sqlite3

ORM SQLite3 Database Access using `mormot.db.raw.sqlite3` unit
- `TOrmTableDB` as Efficient ORM-Aware TOrmTable
- `TOrmVirtualTableModuleServerDB` for SQLite3 Virtual Tables
- `TRestStorageShardDB` for REST Storage Sharded Over SQlite3 Files
- `TRestOrmServerDB` REST Server ORM Engine over SQLite3
- `TRestOrmClientDB` REST Client ORM Engine over SQLite3

### mormot.orm.mongodb

ORM/ODM MongoDB Database Access using `mormot.db.nosql.mongodb` unit
- `TRestStorageMongoDB` for REST Storage Over MongoDB
- High-Level Functions to Initialize MongoDB ORM
