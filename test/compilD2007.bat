@echo off

set DCC="c:\progs\CodeGear\RAD Studio\5.0\bin\dcc32.exe"
if not exist %DCC% set DCC="c:\progs\Delphi2007\bin\dcc32.exe"
set DelphiVersion=Delphi 2007
call compilpil.bat

c:
cd \temp\tempbuild\exe
mormot2tests

pause