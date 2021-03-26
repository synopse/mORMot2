# mORMot Resources

## Folder Content

This folder holds some resource files used by the *mORMot* Open Source framework, version 2.

## MPL 1.1/GPL 2.0/LGPL 2.1 three-license

The framework source code is licensed under a disjunctive three-license giving the user the choice of one of the three following sets of free software/open source licensing terms:
- *Mozilla Public License*, version 1.1 or later (MPL);
- *GNU General Public License*, version 2.0 or later (GPL);
- *GNU Lesser General Public License*, version 2.1 or later (LGPL), with *linking exception* of the *FPC modified LGPL*.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copy-left on code we wrote.

See [the full licensing terms](../LICENCE.md) in the root folder of this repository for more information.

## Resources From Source

Resources are compiled from `*.rc` source and associated files into `*.res` binary files, which will be linked to the executable during compilation.

## Third-Party C Libraries

Even if the framework is stand-alone, it has some optional third-party C code, to be linked as static binaries, which should be available in the [`static`](../static) sub-folder of this repository.

They are to be downloaded from the latest https://github.com/synopse/mORMot2/releases 

You will find in the [`res/static`](static) folder some reference code, script and text to rebuild those third-party libraries from the source.
