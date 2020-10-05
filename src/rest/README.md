# mORMot REST Client-Server Units

## REST In mORMot

*REST* stands for *RE*presentation *S*tate *T*ranfer, and is a way of communicating between clients and servers. Our framework uses JSON for the REpresentation of data between two ends. In practice, you will use HTTP/HTTPS or WebSockets as communication layer of the REST process. But our parent `TSQLRest` class is abstract for any actual mean of communication, and could actually involve a direct in-process call.

The framework has a *REST* orientation, but is not trully *RESTful*. You could let *mORMot* publish the ORM layer over *REST*, but it is better not to be used on production, for obvious security and design reasons. So the framework will offer a *REST* set of services, which will be defined as methods-based-services or interface-based-services - the later being the most efficient and clean way of defining endpoints. Depending on how those services are defined, they be more *RPC* (Remote-Processing-Call) oriented than *REST* (resource) oriented.

JSON has been found out to be a simple but very powerful mean of data representation, and our framework tries to leverage its usage from the ground up. For instance, the ORM layer (from `mormot.orm.core.pas`) is able to create efficient JSON from the lowest level of the supported Database engines, and the client side can consume this JSON with very few resource consumption, on all possible platforms - including JavaScript.

## Folder Content

This folder hosts the *RESTful* oriented features of the *mORMot* Open Source framework, version 2.



