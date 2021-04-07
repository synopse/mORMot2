# mORMot Framework Static Files

## Folder Content

This folder should contain all raw binary files needed for FPC and Delphi static linking.

Please download latest https://synopse.info/files/mormot2static.7z and extract its content to this `/static` sub-folder of the mORMot 2 repository. As an alternative, consider download a matching release from https://github.com/synopse/mORMot2/releases

Usually, static files are in synch with [SQlite3 releases](https://www.sqlite.org/chronology.html), perhaps with some days of delay, to ensure the *SQlite3* release is actually stabilized (it is usual to have build releases following a major SQLite3 release, when some bugs are discovered and fixed in the wild).

Anyway, https://github.com/synopse/mORMot2/releases will maintain a list of official *mORMot 2* releases, including the associated static binaries.


## Static Linking

Those .o/.obj files were compiled from optimized C/asm, for the best performance, and reduce dependencies or version problems.

Note that such external files are not mandatory to compile the framework source code. There is always a "pure pascal" fallback code available, or use e.g. the official external sqlite3 library.

## Delphi Setup

The framework source code uses relative paths to include the expected .o/.obj files from the static\delphi sub-folder, so nothing special is needed.


## FPC Cross-Platform Setup

If you use the `packages/lazarus/mormot2.lpk` package, the static folders should be set for you.

If you don't use the package, e.g. for command-line compilation, ensure that "Libraries -fFl" in your FPC project options is defined as:

      ..\static\$(TargetCPU)-$(TargetOS)

(replace `..\static` by an absolute/relative path to this folder)

It will ensure that when (cross-)compiling your project, FPC will link the expected .o binary files, depending on the target system.

## Keep In Synch

Ensure you keep in synch these binaries with the main framework source code.
Otherwise, some random/unexpected errors may occur.

In doubt, download latest https://synopse.info/files/mormot2static.7z and extract its content.

## Compile From Source

Take a look at [the `/res/static` folder](../res/static) for the reference C source code used to generate those static files. 
