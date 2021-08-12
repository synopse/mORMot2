#!/bin/sh

FPCARCH=aarch64-linux 
DEST=../../../../static/$FPCARCH
CROSS=/home/ab/fpcup/cross/bin/$FPCARCH
GCC=$CROSS/$FPCARCH-gcc

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

cd lib

rm $DEST/liblizard.a
rm *.o

FLAGS="Wall -Wextra -Wcast-qual -Wcast-align -Wshadow -Wswitch-enum -Wdeclaration-after-statement -Wstrict-prototypes -Wundef -Wpointer-arith -Wstrict-aliasing=1"
CFILES="entropy_common.c fse_compress.c fse_decompress.c huf_compress.c huf_decompress.c lizard_compress.c lizard_decompress.c"

$GCC -static -fno-pic -fno-stack-protector -O3 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables  -fno-unwind-tables -std=c99 -I. -nostdlib -fvisibility=hidden -D_ANSI_SOURCE  -c  $CFILES

$CROSS/$FPCARCH-strip -d -x *.o

$CROSS/$FPCARCH-ar rcs liblizard.a entropy_common.o fse_compress.o fse_decompress.o huf_compress.o huf_decompress.o lizard_compress.o lizard_decompress.o

mv liblizard.a $DEST/
