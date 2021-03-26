#!/bin/sh

FPCARCH=i386-win32
DEST=../../dev/lib2/static/$FPCARCH
MINGW=x86_64-w64-mingw32
GCC=$MINGW-gcc 

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/quickjs.o
rm *.o

$GCC -static -w -fno-stack-protector -fno-pic -O2 -m32 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -Wall -Wundef -DWIN32 -DGCCWIN -D_WINDOWS -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c quickjs2.c

$MINGW-strip -d -x *.o

cp quickjs2.o $DEST/quickjs.o