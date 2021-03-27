# mORMot Static Compilation Reference

## Folder Content

This folder holds some files to compile the (static) third-party C libraries used by the *mORMot* Open Source framework, version 2.

**This source code is included as reference. Please don't try to compile the static files by yourself, but download them from the latest https://github.com/synopse/mORMot2/releases - we won't support your self-compiled version!**

 
## Static Compilation

Each sub-folder contains the compilation scripts of the static libraries, and the patched source:

- [`libsqlite3`](libsqlite3) for `SQlite3` engine, with encryption support and most extensions included;
- [`libdeflate`](libdeflate) for fast in-memory `zlib`/`deflate` compression;
- [`liblizard`](liblizard) for the `LZ5`/`Lizard` compression library;
- [`libquickjs`](libquickjs) for the `QuickJS` JavaScript interpreter;
- [`libspidermonkey`](libspidermonkey) for the `SpiderMonkey` JavaScript Engine.


## Licensing

Please follow the license terms of each third-party library.

In addition:

- Our patches follow the third-party libraries terms;
- Our own compilation scripts are published as GPL v3.
