#!/bin/sh

FPCARCH=arm-linux 
DEST=../../../../static/$FPCARCH
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/arm-linux-gnueabihf-gcc

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

cd lib

rm $DEST/liblizard.a
rm *.o

FLAGS="Wall -Wextra -Wcast-qual -Wcast-align -Wshadow -Wswitch-enum -Wdeclaration-after-statement -Wstrict-prototypes -Wundef -Wpointer-arith -Wstrict-aliasing=1"
CFILES="entropy_common.c fse_compress.c fse_decompress.c huf_compress.c huf_decompress.c lizard_compress.c lizard_decompress.c"

$GCC -static -fno-pic -fno-stack-protector -O3 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables  -fno-unwind-tables -std=c99 -I. -I$CROSS -I$CROSS/include -I$CROSS/gnu -I$CROSS/bits -nostdlib -fvisibility=hidden -D_ANSI_SOURCE  -c  $CFILES -D__ARM_PCS_VFP -marm -march=armv7-a+fp+simd -mfpu=vfpv3 -mfloat-abi=hard

$CROSS/arm-linux-gnueabihf-strip -d -x *.o

$CROSS/arm-linux-gnueabihf-ar rcs liblizard.a entropy_common.o fse_compress.o fse_decompress.o huf_compress.o huf_decompress.o lizard_compress.o lizard_decompress.o

mv liblizard.a $DEST/
