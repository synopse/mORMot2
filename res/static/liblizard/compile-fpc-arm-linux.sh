#!/bin/sh

FPCARCH=arm-linux
DEST=../../../../static/$FPCARCH
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/arm-linux-gnueabihf-gcc

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/libdeflate_*.o
rm *.o

$GCC -static -fno-pic -fno-stack-protector -O2 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables  -fno-unwind-tables -std=c99 -I. -Wall -Wundef -ffreestanding -nostdlib -fvisibility=hidden -I$CROSS -I$CROSS/include -I$CROSS/gnu -I$CROSS/bits -D_ANSI_SOURCE -D__ARM_PCS_VFP -marm -march=armv7-a+fp+simd -mfpu=vfpv3 -mfloat-abi=hard -c  lib/deflate_decompress.c lib/utils.c lib/arm/cpu_features.c lib/deflate_compress.c lib/adler32.c lib/crc32.c lib/zlib_decompress.c  lib/zlib_compress.c

mv cpu_features.o libdeflate_cf.o
mv utils.o libdeflate_u.o
mv adler32.o libdeflate_a.o
mv crc32.o libdeflate_c.o
mv deflate_compress.o libdeflate_dc.o
mv deflate_decompress.o libdeflate_dd.o
mv zlib_compress.o libdeflate_zc.o
mv zlib_decompress.o libdeflate_zd.o

$CROSS/arm-linux-gnueabihf-strip -d *.o
mv *.o $DEST