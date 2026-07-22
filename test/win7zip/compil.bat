@echo off
rem ---------------------------------------------------------------------------
rem  Build win7ziptest.dpr from the command line, with no machine-specific paths.
rem
rem  Usage (from a "RAD Studio Command Prompt", or after running rsvars.bat, so
rem  that dcc32/dcc64 is on PATH):
rem
rem      compil.bat            builds Win32 (dcc32)
rem      compil.bat win64      builds Win64 (dcc64)
rem
rem  All framework search paths are derived from this script's own location, so
rem  the branch builds anywhere it is checked out. Output goes to .\build .
rem ---------------------------------------------------------------------------
setlocal

rem repo root = two folders up from this script (test\win7zip -> repo root)
rem (separate lines: %CD% must be read AFTER pushd, not on a single &-chained line)
pushd "%~dp0..\.."
set "LIB2=%CD%"
popd

set "DCC=dcc32"
if /I "%~1"=="win64" set "DCC=dcc64"

where %DCC% >nul 2>nul
if errorlevel 1 (
    echo.
    echo   Could not find %DCC% on PATH.
    echo   Open a "RAD Studio Command Prompt", or run your Delphi rsvars.bat first,
    echo   then re-run this script.
    exit /b 1
)

set "OUT=%~dp0build"
if not exist "%OUT%\dcu" mkdir "%OUT%\dcu"

set "UNITS=%LIB2%\src\core;%LIB2%\src\net;%LIB2%\src\lib;%LIB2%\src\db;%LIB2%\src\rest;%LIB2%\src\soa;%LIB2%\src\crypt;%LIB2%\src\script;%LIB2%\src\orm;%LIB2%\src\app;%LIB2%\src\ui;%LIB2%\src\misc"

echo Building win7ziptest with %DCC% (mORMot root: %LIB2%)
%DCC% "%~dp0win7ziptest.dpr" -B -Q -GD ^
  -R%LIB2%\src ^
  -I%LIB2%\src;%LIB2%\src\core ^
  -U%UNITS% ^
  -E"%OUT%" -N"%OUT%\dcu" ^
  -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;Vcl;System.Win
if errorlevel 1 (
    echo.
    echo   BUILD FAILED
    exit /b 1
)

echo.
echo   Built: %OUT%\win7ziptest.exe
echo   Note: run a build whose bitness matches your installed 7z.dll
echo         ^(Win32 exe needs 32-bit 7z.dll, Win64 exe needs 64-bit 7z.dll^).
endlocal
