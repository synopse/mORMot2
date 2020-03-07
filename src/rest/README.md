# mORMot REST ORM/SOA/MVC Units

## Folder Content

This folder hosts the *RESTful ORM/SOA/MVC* high-level features of the *mORMot* Open Source framework, version 2.

## RESTful

REST is a per-representation way of communicating data, which is the main entry point of our Client-Server framework.

Your high-level business logic can use for instance:

- Abstract REST endpoints, via `mormot.rest.core.*` unit;
- HTTP or WebSockets communication via `mormot.rest.http.*` units;
- ORM access to the data, via `mormot.rest.orm.*` units;
- SOA interface-based services, via `mormot.rest.soa.*` units;
- MVC web server, via `mormot.rest.mvc` unit;
