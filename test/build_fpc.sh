#!/bin/bash

FPCUP=/home/ab/fpcup
FPC_SYS="$FPCUP/fpc/bin/x86_64-linux/fpc.sh"
FPC_OOTB="$FPCUP/fpc-ootb/fpc-ootb-64"

# Build a mORMot2 tests
# Require an FPC3.2 compiler to be installed:
#   wget -O fpc-laz_3.2.0-1_amd64.deb https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/fpc-laz_3.2.0-1_amd64.deb/download
#   sudo apt install ./fpc-laz_3.2.0-1_amd64.deb
#
# Or you can download a stand-alone fpc-ootb binary from
#  https://github.com/fredvs/freepascal-ootb/releases/download/3.2.2/fpc-ootb-322-x86_64-linux_glibc225.zip
#
# Caller may have defined the following variables:
# TARGET=linux - compile target (win32, win64 etc. in case cross compiler is installed). Default is `linux`
# ARCH=x86_64 - compile arch(i386, arm etc.). Default is `x86_64`
# BIN=/tmp/mormot2 - output folder. Default is `..lib2../test/bin`
#
# Call example to cross compile from linux to win64:
# TARGET=win64 ./build_fpc.sh


# Used fpc command line switches:
# -Scgi 	- Support operators like C; Enable LABEL and GOTO(default for -MDelphi); Inlining
# -Cg PIC code 	- for Linux library only (slowed code for program)
# -Ci 		- IO checking
# -O2 		- optimization level
# -g -gl -gw2 -Xg- Generate debug information; Use line info unit (show more info with backtraces); DWARFv2 debug info; debug info in separate file
# -k'-rpath=$ORIGIN' - link to a library in the same folder as program
# -veiq -vw-n-h - verbose(errors, info, message numbers) no warnings, no notes, no hints
# -B 		- build all
# -Se10 	- halts after 10 error 
# to switch to x64MM -dFPC_SYNCMEM should be removed and -dFPC_X64MM -dFPCMM_SERVER added

if [ -t 1 ] ; then #stdout is a terminal
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  NC='\033[0m' # No Color
else
  RED=''
  GREEN=''
  NC=''
fi
script_successful(){
  echo -e "$GREEN Build for $ARCH_TG success. Tests can be executed from\n $BIN/fpc-$ARCH_TG/$dest_fn$NC"
  exit 0
}
script_aborted() {
  echo -e "$RED******Build for $ARCH_TG fail******$NC"
  exit 1
}
# On error
err_report() {
  >&2 echo "Error in $0 on line $1"
  script_aborted
}
trap 'err_report $LINENO' ERR

# uncomment line below to echo commands to console
# set -x

# get a mORMot folder name based on this script location
MORMOT2_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"
TARGET="${TARGET:-linux}"
ARCH="${ARCH:-x86_64}"
ARCH_TG="$ARCH-$TARGET"

LIB2="${LIB2:-$MORMOT2_ROOT}"
BIN="${BIN:-$LIB2/test/bin}"
STATIC="${STATIC:-$LIB2/static}"
SRC="$LIB2/src"
UNITS="$SRC/app;$SRC/core;$SRC/crypt;$SRC/db;$SRC/lib;$SRC/net;$SRC/orm;$SRC/rest;$SRC/soa;$SRC/script;$SRC/misc"
INCLUDES="$SRC;$SRC/core;$SRC/net"

mkdir -p "$BIN/fpc-$ARCH_TG/lib"
rm -f "$BIN"/fpc-"$ARCH_TG"/lib/*

if [ -f "$LIB2"/test/mormot2tests.cfg ]; then
  mv -f "$LIB2/test/mormot2tests.cfg"  "$LIB2/test/mormot2tests.cfg.bak"
fi

dest_fn=mormot2tests
if [ $ARCH = "x86_64" ]; then
  # use O3 and mormot.core.fpcx64mm.pas
  CONDITIONALS="-O3 -dFPC_NO_DEFAULT_MEMORYMANAGER -dFPC_X64MM -dFPCMM_SERVER -dFPCMM_REPORTMEMORYLEAKS"
else
  # use O2 and FPC RTL memory manager
  CONDITIONALS="-O2"
fi
if [[ $TARGET == win* ]]; then
  # specific windows extension for executables
  dest_fn="$dest_fn.exe"
  CONDITIONALS="$CONDITIONALS -CX -XX"
fi
if [ $TARGET = "linux" ]; then
  CONDITIONALS="$CONDITIONALS -CX -XX"
fi
if [ $TARGET = "darwin" ]; then
  CONDITIONALS="$CONDITIONALS -Cg-"
fi

# suppress some paranoid warnings
SUPRESS_WARN=-vm11047,6058,6018,5093,5092,5091,5060,5058,5057,5044,5028,5024,5023,4082,4081,4079,4056,4055,3175,3177,3187,3124,3123,5059,5033,5036,5043,5037,5089,5090

set -o pipefail
echo "compiling for $ARCH_TG as $CONDITIONALS"

if [ $ARCH_TG = "x86_64-linux" ]; then
  FPC="$FPC_OOTB"
  echo "using $FPC"
else
  FPC="$FPC_SYS"
fi

# this is the main compilation command
$FPC -MDelphi -Sci -Ci -g -gl -gw2 -Xg -k'-rpath=$ORIGIN' -k-L$BIN \
  -T$TARGET -P$ARCH $CONDITIONALS \
  -veiq -v-n-h- $SUPRESS_WARN \
  -Fi$INCLUDES \
  -Fu$UNITS \
  -Fl"$STATIC/$ARCH-$TARGET" \
  -FU"$BIN/fpc-$ARCH_TG/lib" -FE"$BIN/fpc-$ARCH_TG" -o"$BIN/fpc-$ARCH_TG/$dest_fn" \
  -B -Se1 "$LIB2/test/mormot2tests.dpr" >"$BIN/fpc-$ARCH_TG.log"
# |rep "[Warning|Error|Fatal]:"

if [ -f "$LIB2/test/mormot2tests.cfg.bak" ]; then
  mv -f "$LIB2/test/mormot2tests.cfg.bak"  "$LIB2/test/mormot2tests.cfg"
fi

script_successful
