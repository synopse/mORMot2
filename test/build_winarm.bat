@echo off
rem  Build the mORMot2 regression tests for Windows on ARM64 (aarch64-win64)
rem  - this script is a part of the Open Source Synopse mORMot framework 2,
rem    licensed under a MPL/GPL/LGPL three license - see LICENSE.md
rem
rem  Requires a native aarch64-win64 FPC compiler, e.g. installed by fpcupdeluxe
rem  on a Windows on ARM computer. Its location is searched in this order:
rem   1. the %FPC% environment variable, if it points to a fpc.exe
rem   2. fpc.exe from the %PATH%
rem
rem  Call example with an explicit compiler location:
rem    set FPC=D:\fpcupdeluxe\fpc\bin\aarch64-win64\fpc.exe
rem    build_winarm.bat
rem
rem  Note: there are no static .o files for aarch64-win64 yet, so mormot.defines.inc
rem  defines NOLIBCSTATIC/NOSQLITE3STATIC/NOLIBDEFLATESTATIC for this target: the
rem  pure pascal fallbacks are used, and the ORM tests expect an external
rem  sqlite3-64.dll (aarch64) to be available next to the generated executable.

setlocal

if not "%FPC%" == "" goto :gotfpc
for %%F in (fpc.exe) do set FPC=%%~$PATH:F
if not "%FPC%" == "" goto :gotfpc
echo Error: no fpc.exe found - please set the FPC environment variable to the
echo full path of a native aarch64-win64 compiler, e.g.
echo   set FPC=D:\fpcupdeluxe\fpc\bin\aarch64-win64\fpc.exe
exit /b 1
:gotfpc
echo Using %FPC%

rem  this script is located in the /test sub-folder of the mORMot 2 repository
for %%D in ("%~dp0..") do set LIB2=%%~fD
set SRC=%LIB2%\src
set BIN=%LIB2%\test\bin\fpc-aarch64-win64

set UNITS=%SRC%\app;%SRC%\core;%SRC%\crypt;%SRC%\db;%SRC%\lib;%SRC%\net;%SRC%\orm;%SRC%\rest;%SRC%\soa;%SRC%\script;%SRC%\misc;%SRC%\tools\agl;%SRC%\tools\ecc;%SRC%\tools\mab;%SRC%\tools\mget;%SRC%\tools\mopenapi
set INCLUDES=%SRC%;%SRC%\core;%SRC%\net

rem  Used fpc command line switches:
rem  -MDelphi   - Delphi compatible syntax
rem  -Sci       - Support operators like C; Enable inlining
rem  -Ci        - IO checking
rem  -O2        - optimization level (no -O3/x64MM as on x86_64)
rem  -g -gl -gw2 -Xg - debug information, line info, DWARFv2, in a separate file
rem  -CX -XX    - smart linking
rem  -veiq -v-n-h- - verbose(errors, info, message numbers) no warnings/notes/hints
rem  -B         - build all
rem  -Se10      - halt after 10 errors
set SUPRESS_WARN=-vm11047,6058,6018,5093,5092,5091,5060,5058,5057,5044,5028,5024,5023,4082,4081,4079,4056,4055,3175,3177,3187,3124,3123,5059,5033,5036,5043,5037,5089,5090

if not exist "%BIN%\lib" md "%BIN%\lib"
del /q "%BIN%\lib\*" >nul 2>&1

rem  a local mormot2tests.cfg would override our command line switches
if exist "%LIB2%\test\mormot2tests.cfg" ren "%LIB2%\test\mormot2tests.cfg" mormot2tests.cfg.bak

echo Compiling for aarch64-win64 into %BIN%
"%FPC%" -MDelphi -Sci -Ci -O2 -g -gl -gw2 -Xg -CX -XX ^
  -Twin64 -Paarch64 ^
  -veiq -v-n-h- %SUPRESS_WARN% ^
  -Fi"%INCLUDES%" ^
  -Fu"%UNITS%" ^
  -Fl"%LIB2%\static\aarch64-win64" ^
  -FU"%BIN%\lib" -FE"%BIN%" -o"%BIN%\mormot2tests.exe" ^
  -B -Se10 "%LIB2%\test\mormot2tests.dpr"
set ERR=%ERRORLEVEL%

if exist "%LIB2%\test\mormot2tests.cfg.bak" ren "%LIB2%\test\mormot2tests.cfg.bak" mormot2tests.cfg

if not "%ERR%" == "0" (
  echo ******Build for aarch64-win64 fail******
  exit /b %ERR%
)
echo Build for aarch64-win64 success. Tests can be executed from
echo  %BIN%\mormot2tests.exe
exit /b 0
