#!/bin/sh

FPCARCH=arm-linux 
DEST=../../../static/$FPCARCH
CROSS=$HOME/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/arm-linux-gnueabihf-gcc

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/libz.a
rm *.o

FLAGS="Wall -Wextra -Wcast-qual -Wcast-align -Wshadow -Wswitch-enum -Wdeclaration-after-statement -Wstrict-prototypes -Wundef -Wpointer-arith -Wstrict-aliasing=1"
CFILES="adler32.c crc32.c deflate.c infback.c inffast.c inftrees.c inflate.c trees.c compress.c"

$GCC -static -fPIC -fno-stack-protector -O2 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables  -fno-unwind-tables -std=c99 -I. -I$CROSS -I$CROSS/include -I$CROSS/gnu -I$CROSS/bits -nostdlib -fvisibility=hidden -D_ANSI_SOURCE  -c  $CFILES -D__ARM_PCS_VFP -marm -march=armv7-a+fp+simd -mfpu=vfpv3 -mfloat-abi=hard

$CROSS/arm-linux-gnueabihf-strip -d -x *.o

$CROSS/arm-linux-gnueabihf-ar rcs libz.a adler32.o compress.o crc32.o deflate.o infback.o inffast.o inflate.o inftrees.o trees.o

mv libz.a $DEST/
