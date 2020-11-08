# mORMot REST Object-Relational-Mapping ORM Units

## Folder Content

This folder hosts the *RESTful ORM* high-level features of the *mORMot* Open Source framework, version 2.

## ORM/ODM Features

*mORMot* implements a simple and very efficient Object-Relational-Mapping:

- The parent `TOrm` class give access to ORM methods at object level;
- `IRestOrm IRestOrmServer IRestOrmClient` interfaces can be used in business code for abstract and persistent-agnostic data access;
- UTF-8 and JSON are used from the ground up, to make our ORM easy to consume in a REST approach, as defined in the `/src/rest` folder;
- It can leverage *SQlite3* as its kernel, to access several data sources;
- A high-performance in-memory engine, using JSON or binary on disk, is available;
- Switch to a *NoSQL* database like *MongoDB* is possible, transforming our ORM to an ODM - aka *Object-Document-Mapping*.
