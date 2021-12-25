#!/bin/sh

FPCARCH=arm-linux
DEST=../../dev/lib2/static/$FPCARCH
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/arm-linux-gnueabihf-gcc 

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/libquickjs.a
rm *.o

CFILES="cutils.c libregexp.c libunicode.c libbf.c quickjs.c"
# quickjs-libc.c  excluded to reduce linking needs - we implement it in native code

$GCC -static -fno-stack-protector -O1 -marm -march=armv7-a+fp -D__ARM_PCS_VFP -mfloat-abi=hard -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -I$CROSS/include -Wall -Wundef -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c  $CFILES

$CROSS/arm-linux-gnueabihf-strip -d -x *.o

$CROSS/arm-linux-gnueabihf-ar rcs libquickjs.a cutils.o libregexp.o libunicode.o libbf.o quickjs.o

mv libquickjs.a $DEST
