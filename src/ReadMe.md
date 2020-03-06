# mORMot Source Code

## Folder Content

This folder hosts the source code of the *mORMot* Open Source framework, version 2.

## MPL/GPL/LGPL Tri-License

This Source Code is licensed under a disjunctive tri-license giving you the choice of one of the three following sets of free software/open source licensing terms:
 - Mozilla Public License, version 1.1 or later;
 - GNU General Public License, version 2.0 or later;
 - GNU Lesser General Public License, version 2.1 or later.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copy-left on code we wrote.

## Sub-Folders

The source code tree is split into the following sub-folders:

- `core` for low-level shared components like JSON, compression, crypto, network;
- `db` for our SQLite3 kernel, and SQL/NoSQL direct access;
- `rest` for high-level ORM, SOA and MVC features.

## Units Naming

By convention:
- Unit names are lowercased, to allow simple access on POSIX or Windows file systems;
- Unit names are dot-separated, and start with the `mormot.` prefix;
- Unit names follow their location in the `src` sub folder, e.g. `mormot.core.json.pas` is located in the `src/core` folder.
