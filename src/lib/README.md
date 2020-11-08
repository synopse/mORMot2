# mORMot External Libraries

## Folder Content

This folder gives access to the *External Libraries* used by the *mORMot* Open Source framework, version 2.

## External Libraries

All `mormot.lib.*.pas` units define direct access to external libraries, like zlib, lizard, curl or openssl. 

We define "external" libraries as some code which is statically linked or dynamically linked into your executable, as dependencies, and are not part of the *mORMot* framework itself and its licensing terms.

Notes:

- The mandatory libraries which are meant to be part of the Operating System - e.g. the Windows API or the `libc`/`pthread` API - are defined in `mormot.core.os.pas`.
- Access to the SQL database client libraries won't be in this folder, but defined as `mormot.db.raw.*.pas` units in the `src/db` folder.

## Thin Wrappers

Those `mormot.lib.*.pas` units are just wrappers to the `c`/`stdcall` external API of the libraries. They are then encapsulated in higher level units, which are meant to be used by the framework.

For instance 

- `mormot.lib.z.pas` contains the raw access to the `zlib` API, 
- whereas `mormot.core.zip.pas` contains the actual deflate and .zip file process.

On Windows, some Operating-System high-level features like Windows HTTP and WebSockets client/server API, or SSPI/SChannel API are also defined in this folder, to leverage `mormot.core.os.pas` focusing on core cross-platform features.

