# Synopse *mORMot* Framework

An Open Source Client-Server ORM/SOA framework, version 2

(c) 2008-2020 Synopse Informatique - Arnaud Bouchez

https://synopse.info  - http://mORMot.net

See CONTRIBUTORS.md for a list of contributors.

**WARNING: This set of units is still a work-in-progress, and not yet ready for production. Please still [use *mORMot 1.18*](https://github.com/synopse/mORMot) for your projects.**

## Version 2

The *mORMot* framework stayed in revision 1.18 for years, and is was time for a full refactoring.

The main refactoring points tried to better follow SOLID principles:
 - Switch to Semantic Versioning - see https://semver.org
 - Separate main big units (`SynCommons.pas`, `mORMot.pas`) into smaller scope-refined units;
 - All unit names changed, to avoid collision between versions;
 - Favor composition over inheritance (e.g. TSQLRest class split into proper REST/ORM/SOA classes);
 - Rewrite some code to avoid internal errors on Delphi (e.g. untyped const/var changed into pointers);
 - Add MVC support for SOA rich clients on Delphi VCL and Lazarus LCL;
 - Introduce high-level non-visual TComponent version of our classes for a more RAD approach.

We therefore created a whole new project and repository, since switching to version 2 induced some backward uncompatible changes. New unit names were used, to avoid unexpected issues during migration, or if 1.18 is to remain installed for a compatibility project.

(See below if you are upgrading from 1.18 branch)


## Presentation

Synopse *mORMot* is an Open Source Client-Server ORM SOA MVC framework for Delphi 7 up to Delphi 10.3 Rio and FPC, targeting Windows/Linux for servers, and any platform for clients (including mobile or AJAX).

The main features of *mORMot* are therefore:

 - ORM/ODM: objects persistence on almost any database (SQL or NoSQL);
 - SOA: organize your business logic into REST services;
 - Clients: consume your data or services from any platform, via ORM/SOA APIs;
 - Web MVC: publish your ORM/SOA process as responsive Web Applications.

With local or remote access, via an convention-over-configuration Client-Server REST design.

Due to its modular design, switch from a Client-Server architecture over HTTP, named pipes or GDI messages into a stand-alone application is just a matter of *mORMot* classes initialization.
For instance, the very same executable can even be running stand-alone, as a server, as a service, or a client, depending on some run-time parameters!

Emphasizing speed and versatility, *mORMot* is a incredibly well documented Open Source project easy enough to add basic ORM or Client-Server features to simple applications for hobbyists, or let experienced users develop scaling and strong service-based projects for their customers, with the advantages of native code and easy-to-deploy solutions, reducing deployment cost and increasing ROI.

## Domain-Driven-Design Ready

It provides an Open Source self-sufficient set of units (even Delphi starter edition is enough) for creating any application, from a stand-alone solution up to the most complex Domain-Driven Design (DDD):

 - Presentation layer featuring MVC UI generation with i18n and reporting (with pdf export) for rich Delphi clients, MVC web clients (with logic-less Mustache templates) or rich AJAX clients (via native JSON/REST access);

 - Application layer implementing Service Oriented Architecture via interface-based services (like WCF) and Client-Server ORM (including method-based services) - following a RESTful model using JSON over several communication protocols (e.g. HTTP/1.1);

 - Domain Model layer handling all the needed business logic in plain Delphi objects, including high-level managed types like dynamic arrays or records for Value Objects, dedicated classes for Entities or Aggregates, and variant storage with late-binding for dynamic documents;

 - Data persistence infrastructure layer with ORM operations on direct Oracle, MS SQL, OleDB, ODBC, ZEOS/ZDBC access or any TDataSet provider (e.g. FireDAC/AnyDAC, UniDAC, NexusDB, BDE...), with a powerful SQLite3 kernel, and optional SQL access if needed, with amazing performance and advanced features like Array DML, auto-generating SQL for SQLite3, Oracle, 
 Jet/MSAccess, MS SQL, Firebird, DB2, PostgreSQL, MySQL and NexusDB - and alternative high-speed MongoDB NoSQL database access for ODM persistence;

 - Cross-Cutting infrastructure layers for handling data filtering and validation, security (e.g. Windows authentication or any custom model), caching, logging and testing (framework uses test-driven approach and features interface stubbing and mocking).

## Client-Server Fully Integrated Framework

With *mORMot*, ORM/ODM is not used only for data persistence of objects (like in other implementations), but as part of a global n-Tier, Service Oriented Architecture (SOA), ready to implement Domain-Driven solutions. This framework is not an ORM on which a transmission layer has been added, like almost everything existing in Delphi, C# or Java: this is a full Client-Server ORM/SOA from the ground up. This really makes the difference.

The business logic of your applications will be easily exposed as Services, and will be accessible from light clients (written in Delphi or any other mean, including AJAX).

The SpiderMonkey JavaScript engine has been integrated on the server side and can be used to define business rules or any process (including MVC web rendering) - just like node.js, but with a multi-threaded core, and the full power of our optimized Delphi libraries at hand.

The framework Core is non-visual: you will get everything you need in a consistent set of classes to be used from code. In order to let you focus on your business, using *mORMot*'s KISS/DRY/SOC/YAGNI/TDD and Convention-Over-Configuration patterns. But you have also some UI units available (including screen auto-generation, reporting and ribbon GUI), and you can use it from
any RAD, web, or AJAX clients (via JavaScript or Smart Mobile Studio).

No dependency is needed on the client side (no DB driver, or third-party runtime): it is able to connect via standard HTTP, even through a corporate proxy or a VPN. Rich Delphi clients can be deployed just by copying and running
a stand-alone small executable, with no installation process. Stream can be encrypted via HTTS or with proven SHA/AES-256. Endpoints are configured automatically for each published interface on both server and client sides, and creating a load-balancing proxy is a matter of one method call.
Speed and scalability has been implemented from the ground up: a genuine optimized multi-threaded core let a single server handle more than 50,000 concurrent clients, faster than DataSnap, WCF or node.js, and our rich SOA design is able to implement both vertical and horizontal scalable hosting, using recognized enterprise-level SQL or NoSQL databases for storage.

Even if *mORMot* will be more easily used in a project designed from scratch, it fits very well the purpose of evolving any existing Delphi project, or creating the server side part of an AJAX application.

## Units

Some units (e.g. `...`) are used by *mORMot*, but do not require the whole framework to be linked.

That is, you can use e.g. only  PDF generation, SynDB fast database access, a static-linked SQLite3 engine, direct MongoDB access, Mustache templates, SpiderMonkey JavaSCript engine, code-generated reports, or the TDocVariant, TDynArray, TSynLog classes of SynCommons, without using the main *mORMot* units and features (ORM, Client-Server, services, UI).

## Compiler targets

The framework source code:
 - Tries to stay compatible with FPC trunk and Delphi 7 up to 10.3; 
 - Is validated against FPC trunk SVN 40491, Delphi 7, XE4, XE7 and 10.3;
 - Please submit pull requests for non-validated versions.

## MPL/GPL/LGPL Three-License

Licensed under a disjunctive three-license giving you the choice of one of the three following sets of free software/open source licensing terms:
 - Mozilla Public License, version 1.1 or later;
 - GNU General Public License, version 2.0 or later;
 - GNU Lesser General Public License, version 2.1 or later.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copy-left on code we wrote.

See LICENSE.md for the licensing terms.

## Links

(to be fixed)

Main project page:
http://mORMot.net

Documentation:
https://synopse.info/files/html/Synopse%20*mORMot*%20Framework%20SAD%201.18.html

Installation:
https://synopse.info/files/html/Synopse%20*mORMot*%20Framework%20SAD%201.18.html#TITL_113

FAQ:
https://synopse.info/files/html/Synopse%20*mORMot*%20Framework%20SAD%201.18.html#TITL_123

How to get the source:
https://synopse.info/fossil/wiki?name=Get+the+source

A forum is dedicated to support:
https://synopse.info

A blog is available:
http://blog.synopse.info

Issues and feature requests can be posted (take a look at the forums and latest unstable version first!):
https://synopse.info/fossil/reportlist

You can also monitor/fork our projects on GitHub:
https://github.com/synopse/mORMot

You may also install it as a Delphinus package: Delphinus-Support

## Documentation

Don't forget to download the documentation (available online or as pdf files, created by our SynProject tool).
In particular, you should take a look at all general introduction chapters of the SAD document. It will cover all key-concepts and code modelling used by the framework.
A developer guide is included in this SAD document, in its 2nd part. You'll get good practice guidance, presentation of the ORM/SOA approach and other underlying concepts.

## Contribute

Feel free to contribute by posting enhancements and patches to this quickly evolving project.
  
Enjoy!

## Upgrading From 1.18 Revision

Quick Steps when upgrading from a previous 1.18 revision:

1) Note all units where split and renamed, and some breaking changes introduced for enhanced features, therefore a direct update is not possible - nor wanted

2) Switch to a new folder, e.g. #\lib2 instead of #\lib

3) Download latest 2.# revision files as stated just above
  
4) Change your references to *mORMot* units:
 - All unit names changed, to avoid collision between versions.
 
5) Consult the documentation about breaking changes from 1.18, mainly:
 - Units refactoring (see point 4 above);
 - Delphi 5-6 compatibility removed;
 - ...

