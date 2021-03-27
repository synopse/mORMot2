#!/bin/sh

FPCARCH=x86_64-win64
DEST=../../dev/lib2/static/$FPCARCH
MINGW=x86_64-w64-mingw32
GCC=$MINGW-gcc 

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC

rm $DEST/libquickjs.a
rm *.o

CFILES="cutils.c libregexp.c libunicode.c libbf.c quickjs.c"
# quickjs-libc.c  excluded to reduce linking needs - we implement it in native code

$GCC -static -fno-stack-protector -O2 -m64 -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -std=c99 -I. -Wall -Wundef -DWIN32 -DGCCWIN -D_WINDOWS -D_GNU_SOURCE -DCONFIG_BIGNUM -DJS_STRICT_NAN_BOXING -DCONFIG_JSX -DCONFIG_DEBUGGER -c  $CFILES
# note: QuickJS expects Win32 even on Win64 ;)

$MINGW-strip -d -x *.o

$MINGW-ar rcs libquickjs.a cutils.o libregexp.o libunicode.o libbf.o quickjs.o

mv libquickjs.a $DEST

#echo ---------------------------------------------------
#echo Patching $FPCARCH statics for Delphi

#cd delphi64
# wine cmd /c o2delphi.bat 

#echo Compiling for Delphi Win64
#wine cmd /c compile-win64-delphi.bat