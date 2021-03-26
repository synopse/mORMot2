#!/bin/sh

FPCARCH=arm-android
DEST=../../dev/lib2/static/$FPCARCH
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/arm-linux-androideabi-gcc 
# gcc not available yet in FpcUpDeluxe cross compiler package :(

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/quickjs.o
rm *.o

#note: required -mfloat-abi=softfp
# see https://android.googlesource.com/platform/ndk/+/ndk-r12-release/docs/HardFloatAbi.md

$GCC -static -w -fno-stack-protector -O1 -marm -march=armv7-a+fp -D__ARM_PCS_VFP -mfloat-abi=softfp -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -I$CROSS/include -Wall -Wundef -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c quickjs2.c

$CROSS/arm-linux-androideabi-strip -d -x *.o

cp quickjs2.o $DEST/quickjs.o