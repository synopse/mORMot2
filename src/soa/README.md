# mORMot REST Service-Oriented-Architecture Units

## Folder Content

This folder hosts the *RESTful SOA* high-level features of the *mORMot* Open Source framework, version 2.

## Service-Oriented-Architecture over Interface

The framework offers two ways of implementing SOA:
- Defining a *class method* at `TRest` level;
- Defining an `interface` and the associated implementation `class`.

The method-based services:
- leverage full access to the input/output of the HTTP request on the server side;
- offer the best performance;
- but require to write the client-side by hand.

The `interface`-based services:
- are more convenient to define the services contracts, in regular code;
- can generate a fake implementation `class` on the client side;
- are more natural when called from object pascal code;
- offers natural bi-directional communication over WebSockets, by defining `interface` parameters in the contract;
- has built-in options for authorization/security, threading abilites, JSON/XML marshalling.

In a nutshell, when building RESTful services, use `interface`-based services as provided by these `mormot.soa.*` units. But you can mix the two, so some dedicated endpoints could be implemented using methods.


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
