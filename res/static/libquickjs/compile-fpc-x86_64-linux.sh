#!/bin/sh

FPCARCH=x86_64-linux
DEST=../../dev/lib2/static/$FPCARCH
GCC=gcc-7

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/quickjs.o
rm *.o

$GCC -static -w -fno-stack-protector -O2 -m64 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -Wall -Wundef -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c quickjs2.c

strip -d -x quickjs2.o

cp quickjs2.o $DEST/quickjs.o