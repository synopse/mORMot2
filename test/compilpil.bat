@echo off

rem Caller may have defined the following variables:
rem   set DCC=...\bin\dcc32.exe
rem   set DelphiVersion=Delphi ##
rem   set lib2=... root framework library folder
rem   set bin=... compilation output folder
rem   set target=win32 (or win64)
rem   call compilpil.bat

if "%lib2%"==""   set lib2=d:\dev\lib2
if "%bin%"==""    set bin=c:\temp\tempbuild
if "%target%"=="" set target=win32

set units=%lib2%\src\core;%lib2%\src\db;%lib2%\src\rest
set sw=-B -Q -GD -R%lib2%\src -I%lib2%\src;%lib2%\src\core -U%units% -O%lib2%\static\delphi-%target% -E%bin%\exe -N%bin%\dcu -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;Vcl;System.Win

if "%DelphiVersion%"=="" (
	rem ** Default compiler is Delphi 7
	set DCC=c:\progs\delphi7\bin\dcc32.exe
	set DelphiVersion=Delphi 7
	set sw=%sw% -U%lib2%\..\lib\rtl7 -I%lib2%\..\lib\rtl7
)

if not exist %DCC% goto NoDCCCompiler

rem echo %sw%

echo.
echo ***** mORMot 2 integration using %DelphiVersion% for %target% *****
if not exist %bin%\exe (
	mkdir %bin%\exe
	mkdir %bin%\dcu
) else (
	del /q %bin%\dcu\*.dcu
	del /q %bin%\exe\*.exe
	del /q %bin%\exe\*.drc
	del /q %bin%\exe\*.map
	del /q %bin%\exe\*.db3
	del /q %bin%\exe\*.ini
	del /q %bin%\exe\*.data
	del /q %bin%\exe\*.mdb
	del /q %bin%\exe\TestSQL3.*
)
echo.

cd %lib2%\test
copy mormot2tests.cfg mormot2tests.cfg.bak /Y> nul
del mormot2tests.cfg
echo %CD%
%DCC% mormot2tests.dpr %sw%
@if errorlevel 1 pause
copy mormot2tests.cfg.bak mormot2tests.cfg /Y> nul


:NoDCCCompiler
cd %lib2%\test
set DCC=
set DelphiVersion=
set lib2=
set bin=
set target=
rem pause
