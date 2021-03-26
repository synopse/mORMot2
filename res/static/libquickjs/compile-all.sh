#!/bin/sh

echo
echo Compute quickjs2.c amalgamation source

cat cutils.h cutils.c libbf.c libregexp.c libunicode.c quickjs.c > quickjs2.c
# quickjs-libc.c  excluded to reduce linking needs - we implement it in native code

echo
echo Use Native compilers for FPC i386/x86_64-linux

./compile-fpc-i386-linux.sh
./compile-fpc-x86_64-linux.sh

echo
echo Use Native cross-compilers for FPC Win32/Win64

./compile-fpc-i386-win32.sh
./compile-fpc-x86_64-win64.sh

echo
echo Use Native cross-compilers for Delphi Win32/Win64

./compile-delphi-win32.sh
./compile-delphi-win64.sh

echo
echo Use Native fpcupdeluxe cross-compilers for FPC Linux arm/aarch64-linux

./compile-fpc-arm-linux.sh
./compile-fpc-aarch64-linux.sh
 
