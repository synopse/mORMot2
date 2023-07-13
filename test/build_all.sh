#!/bin/bash

ARCH=i386 TARGET=linux ./build_fpc.sh
ARCH=x86_64 TARGET=linux ./build_fpc.sh
ARCH=arm TARGET=linux ./build_fpc.sh
ARCH=aarch64 TARGET=linux ./build_fpc.sh

ARCH=i386 TARGET=win32 ./build_fpc.sh
ARCH=x86_64 TARGET=win64 ./build_fpc.sh

ARCH=i386 TARGET=freebsd ./build_fpc.sh
ARCH=x86_64 TARGET=freebsd ./build_fpc.sh

ARCH=x86_64 TARGET=darwin ./build_fpc.sh
ARCH=aarch64 TARGET=darwin ./build_fpc.sh
