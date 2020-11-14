# mORMot REST Service-Oriented-Architecture Units

## Folder Content

This folder hosts the *RESTful SOA* high-level features of the *mORMot* Open Source framework, version 2.

## Units Presentation

### mormot.soa.core

Shared Interface-based Service Oriented Architecture (SOA) Process
- `TOrmServiceLog TOrmServiceNotifications` Classes
- `TServiceFactory` Abstract Service Provider
- `TServiceFactoryServerAbstract` Abstract Service Provider
- `TServiceContainer` Abstract Services Holder
- SOA Related Interfaces
- `TServicesPublishedInterfacesList` Services Catalog

### mormot.soa.client

Client-Side Interface-based Service Oriented Architecture (SOA) Process
- `TServiceFactoryClient` Service Provider
- `TServiceContainerClientAbstract` Service Provider
- `TServiceContainerClient` Services Holder

### mormot.soa.server

Server-Side Interface-based Service Oriented Architecture (SOA) Process
- `TInjectableObjectRest` Service Implementation Parent Class
- `TServiceFactoryServer` Service Provider
- `TServiceContainerServer` Services Holder
- Asynchronous REST Synchronisation Classes

### mormot.soa.codegen

SOA API Code and Documentation Generation
- ORM and SOA Logic Extraction from RTTI
- Documentation Extraction from Source Code Comments
- Doc/CodeGen wrapper Functions on Server Side
- FPC Dedicated Generators
- Compute Asynchronous Code from Synchronous Interface
- Generate Code and Doc from Command-Line
