# mORMot Framework Static Files

## Folder Content

This folder contains all raw binary files needed for FPC and Delphi static linking.

If this folder is void (e.g. when retrieved from https://synopse.info/fossil), you can download all the needed sub-folders from http://synopse.info/files/sqlite3fpc.7z

## Static Linking

Those .o/.obj files were compiled from optimized C/asm, for the best performance, and reduce dependencies or version problems.

Note that such external files are not mandatory to compile the framework source code. There is always a "pure pascal" fallback code available, or use e.g. the official external sqlite3 library.

## FPC Cross-Platform Setup

Ensure that "Libraries -fFl" in your FPC project options is defined as:
  ..\static\$(TargetCPU)-$(TargetOS)
(replace ..\static by an absolute/relative path to this folder)

## Keep In Synch

Ensure you keep in synch these binaries with the main framework source code.
Otherwise, some random/unexpected errors may occur.