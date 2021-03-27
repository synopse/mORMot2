#!/bin/sh

FPCARCH=aarch64-linux
DEST=../../dev/lib2/static/$FPCARCH
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/$FPCARCH-gcc 

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/libquickjs.a
rm *.o

CFILES="cutils.c libregexp.c libunicode.c libbf.c quickjs.c"
# quickjs-libc.c  excluded to reduce linking needs - we implement it in native code

$GCC -static -fno-stack-protector -O1 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -I$CROSS/include -Wall -Wundef -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c  $CFILES

$CROSS/$FPCARCH-strip -d -x *.o

$CROSS/$FPCARCH-ar rcs libquickjs.a cutils.o libregexp.o libunicode.o libbf.o quickjs.o

mv libquickjs.a $DEST
