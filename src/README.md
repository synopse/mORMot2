# mORMot Source Code

## Folder Content

This folder hosts the source code of the *mORMot* Open Source framework, version 2.

## MPL 1.1/GPL 2.0/LGPL 2.1 three-license

The framework source code is licensed under a disjunctive three-license giving the user the choice of one of the three following sets of free software/open source licensing terms:
- *Mozilla Public License*, version 1.1 or later (MPL);
- *GNU General Public License*, version 2.0 or later (GPL);
- *GNU Lesser General Public License*, version 2.1 or later (LGPL), with *linking exception* of the *FPC modified LGPL*.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copy-left on code we wrote.

See [the full licensing terms](../LICENCE.md) in the root folder of this repository for more information.

## Sub-Folders

The source code tree is split into the following sub-folders:

- [`core`](core) for low-level shared components like text, RTTI, JSON, compression, crypto;
- [`lib`](lib) for external third-party libraries like *zlib* or *openssl*;
- [`net`](net) for the client/server communication layer;
- [`db`](db) for our *SQLite3* kernel, and SQL/NoSQL direct access;
- [`rest`](rest) for the RESTful client/server processing;
- [`orm`](orm) for high-level ORM features;
- [`soa`](soa) for high-level SOA features;
- [`app`](app) for hosting (micro)services/daemons and applications;
- [`ddd`](ddd) for *Domain-Driven-Design* related code;
- [`tools`](tools) for some useful tools tied to our framework.


## Units Naming

By convention:
- Unit names are lowercased, to allow simple access on POSIX or Windows file systems;
- Unit names are dot-separated, and start with the `mormot.` prefix;
- Unit names follow their location in the `src` sub folder, e.g. `mormot.core.json.pas` is located in the `src/core` folder.

## Types Naming

In respect to *mORMot 1.18*, some confusing/deprecated naming like `TSQLRecord` or `TSQLRest` prefix have been renamed as `TOrm` and `TRest`, since our ORM is not SQL-only, but works e.g. with NoSQL engines like MongoDB.

Generally speaking, we followed the [Kotlin good naming rules](https://kotlinlang.org/docs/reference/coding-conventions.html#choosing-good-names):
>  When using an acronym as part of a declaration name, capitalize it if it consists of two letters (IOStream); capitalize only the first letter if it is longer (XmlFormatter, HttpInputStream).

Some types have been changed or enhanced:
- `TSQLRawBlob` renamed as `RawBlob`;
- `RawUTF8` is an alias to `System.UTF8String` type so you can use either of them in your code.

Note that the `PUREMORMOT2` conditional can be defined for your project, to disable the type names backward compatible redirection enabled by default.

## Include Files

To clean the design and enhance source maintainibility, some units have associated `*.inc` source files:
- To regroup Operating-System specific code - e.g. `mormot.core.os.posix.inc` to include non-Windows OS calls in `mormot.core.os`;
- To regroupe Compiler-specific code - e.g. `mormot.core.rtti.fpc.inc` to include FPC RTTI in `mormot.core.rtti`;
- To regroup CPU-specific (asm) code - e.g. `mormot.core.crypto.asmx64.inc` to include `x86_64` assembly in `mormot.core.crypto`.
