#!/bin/sh

gcc -c delphi-linux-x64.s -o delphi-linux-x64.o

# aarch64 needs the preprocessor, and one object per binary format/platform:
# use the clang of the NDK shipped with the RAD Studio Android SDK
CLANG=${CLANG:-clang}
ASM="-x assembler-with-cpp -c delphi-aarch64.s"
$CLANG $ASM --target=aarch64-linux-android23 -o delphi-android-arm64.o
$CLANG $ASM --target=arm64-apple-ios15.0 -o delphi-ios-arm64.o
$CLANG $ASM --target=arm64-apple-ios15.0-simulator -o delphi-iossim-arm64.o

# 32-bit ARM (Android armeabi-v7a): ARM mode, VFP hard-float, EHABI unwind tables
$CLANG -c delphi-arm.s --target=armv7a-linux-androideabi23 -o delphi-android-arm.o
