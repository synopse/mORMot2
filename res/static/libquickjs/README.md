
# mORMot Static Compilation Notice

The following modifications have been made by Synopse to the original source code:

- started not from original Bellard's repository, but https://github.com/c-smile/quickjspp fork;
- merged back the official https://github.com/bellard/quickjs updates, even when the `c-smile` fork is outdated;
- enabled `BIGNUM`, `JSX` and debugger extensions by default;
- forced `JS_STRICT_NAN_BOXING` on all targets, for a single 64-bit contained `JSValue` definition;
- `quickjs-libc` is not linked to enhance cross-compilation and to be reimplemented in native pascal instead of native c - `js_module_set_import_meta/js_std_eval_binary` were moved to the main `quickjs.c` file;
- regrouped all source code into a single `quickjs2.c` amalgamation source file (to fix cyclic static definitions) and facilitate Delphi static linking;
- `assert()` and `abort()` redirect to less painful pascal `Exception`s (how want a `SIG_ABRT` to kill your service?);
- `pas_malloc/free` redirection to the pascal MM for best performance and less memory fragmentation;
- simple per-target compilation scripts (no complex make files) dedicated to FPC and Delphi static linking using the [FpcUpDeluxe cross-compilers](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases).

With some proper tricks, we were able to compile QuickJS using mingw/gcc for FPC (the easy part), and also for Delphi Win32/Win64 - even if they don't support their format. \o/

This source code is included as reference. Please don't try to compile the static files by yourself, but download them from the latest https://github.com/synopse/mORMot2/releases or https://synopse.info/files/mormot2static.7z


# QuickJS Javascript Engine 

Authors: Fabrice Bellard and Charlie Gordon

Ported from https://bellard.org/quickjs/ and its official GitHub mirror https://github.com/bellard/quickjs

By Andrew Fedoniouk (a.k.a. c-smile)

This version is 

* Microsoft Visual C++ compatible/compileable
* Is used in Sciter.JS
* It contains extras: 
  * [JSX](doc/jsx.md) - built-in [facebook::JSX](https://facebook.github.io/jsx/) support with Sciter specific extras.
  * Built-in [Persistence](storage/doc/README.md) - you can think of it as local MongoDB (NoSQL) DB embedded into the language. Pretty small, adds just 70kb into binary.

The main documentation is in doc/quickjs.pdf or [doc/quickjs.html](doc/quickjs.html).

# Build using Microsoft Visual Studio (2017 or 2019)

Prerequisite: **premake5** - [download](https://premake.github.io/download.html) and install it.

Then go to /win folder and run premake-vs2017.bat or premake-vs2019.bat . 

It will generate .build/vs2017/quickjs-msvc.sln and open it in Microsoft Visual Studio.

Press F5 to compile it and run qjs - interactive JS command line application.

# Premake5 and build on other platforms/compilers/ide  

Supported premake options:

* ```--jsx``` - include JSX support;
* ```--storage``` - include Persistent Storage support;

Supported targets (these are built into [Premake](https://premake.github.io/) itself):

* vs2017 - MS Visual Studio 2017
* vs2019 - MS Visual Studio 2019
* gmake2 - gmake files
* etc...

Few examples of other possible configurations: 
```bat
premake5 vs2019 --jsx --storage
premake5 codeblocks --cc=gcc --jsx --storage
premake5 gmake2 --cc=gcc --jsx --storage
premake5 gmake2 --cc=clang --jsx --storage
premake5 gmake2 --cc=clang --jsx --storage
premake5 xcode4 --jsx --storage
```





