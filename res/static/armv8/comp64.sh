#!/bin/sh

ARCH=aarch64-linux

CROSS=/home/ab/fpcup/cross/bin/$ARCH
GCC=$CROSS/$ARCH-gcc

rm $DST2

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC

$GCC -static -c -O3 -mcpu=generic+crypto sha256-armv8-aarch64.S -o sha256armv8.o
$GCC -static -c -O3 -mcpu=generic+crc+crypto armv8.c -o armv8.o

#cp sqlite3-$ARCH.o $DST2

