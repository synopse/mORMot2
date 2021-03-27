#!/bin/sh

FPCARCH=aarch64-linux
DEST=../../dev/lib2/static/$FPCARCH
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/$FPCARCH-gcc 

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/quickjs.o
rm *.o 

$GCC -static -w -fno-stack-protector -O1 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -I$CROSS/include -Wall -Wundef -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c quickjs2.c 

$CROSS/$FPCARCH-strip -d -x *.o

cp quickjs2.o $DEST/quickjs.o
