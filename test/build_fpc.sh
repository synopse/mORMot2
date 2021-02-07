#!/bin/bash

# Build a mORMot2 tests
# Require an FPC3.2 compiler to be installed:
#   wget -O fpc-laz_3.2.0-1_amd64.deb https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/fpc-laz_3.2.0-1_amd64.deb/download
#   sudo apt install ./fpc-laz_3.2.0-1_amd64.deb
#
# Caller may have defined the following variables:
# TARGET=linux - compile target (win32, win64 etc. in case cross compiler is installed). Default is `linux`
# ARCH=x86_64 - compile arch(i386, arm etc.). Default is `x86_64`
# BIN=/tmp/mormot2 - output folder. Default is `/tmp/mormot2`

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
  echo -e "$GREEN Build for $ARCH_TG success. Tests can be executed from\n $BIN/fpc-$ARCH_TG/mormot2tests$NC"
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

# get a mORMot folder name based on this script location
MORMOT2_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"
TARGET="${TARGET:-linux}"
ARCH="${ARCH:-x86_64}"
ARCH_TG="$ARCH-$TARGET"

LIB2="${LIB2:-$MORMOT2_ROOT}"
BIN="${BIN:-/tmp/mormot2}"
STATIC="${STATIC:-$LIB2/static}"

mkdir -p "$BIN/fpc-$ARCH_TG/.dcu"
rm -rf "$BIN/fpc-$ARCH_TG/.dcu/*"

if [ -f $LIB2/test/mormot2tests.cfg]; then
  mv -f $LIB2/test/mormot2tests.cfg  $LIB2/test/mormot2tests.cfg.bak
fi

fpc -MDelphi -Sci -Ci -O2 -g -gl -gw2 -Xg -k'-rpath=$ORIGIN' -k-L$BIN \
  -veiq -vw-n-h- \
  -Fi"$BIN/fpc-$ARCH_TG/.dcu" -Fi"$LIB2/src/core" -Fi"$LIB2/src/db" -Fi"$LIB2/src/rest" \
  -Fl"$STATIC/$ARCH-$TARGET" \
  -Fu"$LIB2/src/core" -Fu"$LIB2/src/db" -Fu"$LIB2/src/rest" \
  -FU"$BIN/fpc-$ARCH_TG/.dcu" -FE"$BIN/fpc-$ARCH_TG" -o"$BIN/fpc-$ARCH_TG/mormot2tests" \
  -dFPC_SYNCMEM -dDOPATCHTRTL -dFPCUSEVERSIONINFO1 \
  -B -Se1 $LIB2/test/mormot2tests.dpr

if [ -f $LIB2/test/mormot2tests.cfg.bak]; then
  mv -f $LIB2/test/mormot2tests.cfg.bak  $LIB2/test/mormot2tests.cfg
fi

script_successful
