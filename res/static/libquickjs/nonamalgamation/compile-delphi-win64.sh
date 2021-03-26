#!/bin/sh

FPCARCH=x86_64-win64
MINGW=x86_64-w64-mingw32
GCC=$MINGW-gcc 

echo
echo ---------------------------------------------------
echo Compiling for Delphi on $FPCARCH using $GCC

rm *.o

# quickjs-libc.c  excluded to reduce linking needs - we implement it in native code

$GCC -static -fno-stack-protector -O2 -m64 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -Wall -Wundef -DWIN32 -DGCCWIN -D_WINDOWS -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c  quickjs2.c
# note: QuickJS expects Win32 even on Win64 ;)

$MINGW-strip -d -x quickjs2.o

echo ---------------------------------------------------
echo Patching $FPCARCH statics for Delphi

cd delphi64
wine cmd /c o2delphi.bat 

#echo Compiling for Delphi Win64
#wine cmd /c compile-win64-delphi.bat