#!/bin/sh

FPCARCH=i386-win32
MINGW=x86_64-w64-mingw32
GCC=$MINGW-gcc 

echo
echo ---------------------------------------------------
echo Compiling for Delphi on $FPCARCH using $GCC

rm *.o

$GCC -static -w -fno-reorder-functions -fno-stack-protector -O2 -m32 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -Wall -Wundef -DWIN32 -DGCCWIN -D_WINDOWS -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c  quickjs2.c
# -fno-reorder-functions for Delphi32

$MINGW-strip -d -x quickjs2.o

echo ---------------------------------------------------
echo Patching $FPCARCH statics for Delphi

cd delphi32
wine cmd /c o2delphi.bat 
