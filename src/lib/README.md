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


## Units Presentation

### mormot.lib.z

Cross-Platform and Cross-Compiler `zlib` API
- Low-Level ZLib Streaming Access
- Simple Wrapper Functions for Deflate/ZLib Process

### mormot.lib.lizard

Cross-Platform and Cross-Compiler `Lizard` (LZ5) API
- Low-Level Lizard API Process
- `TAlgoLizard TAlgoLizardFast TAlgoLizardHuffman` High-Level Algorithms

### mormot.lib.curl

Cross-Platform and Cross-Compiler `libcurl` API
- CURL Low-Level Constants and Types
- CURL Functions API

### mormot.lib.winhttp

Windows HTTP and WebSockets API Libraries
- WinINet API Additional Wrappers
- http.sys / HTTP Server API low-level direct access
- winhttp.dll Windows API Definitions
- websocket.dll Windows API Definitions

### mormot.lib.sspi

Security Support Provider Interface (SSPI) Support on Windows
- Low-Level SSPI/SChannel Functions
- Middle-Level SSPI Wrappers
- High-Level Client and Server Authentication using SSPI

### mormot.lib.gssapi

Generic Security Service API on POSIX/Linux
- Low-Level libgssapi_krb5/libgssapi.so Library Access
- Middle-Level GSSAPI Wrappers
- High-Level Client and Server Authentication using GSSAPI
