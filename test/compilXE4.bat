@echo off
cls

set DCC="c:\progs\Embarcadero\RAD Studio\11.0\bin\dcc32.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE4\bin\dcc32.exe"
set DelphiVersion=Delphi XE4
set target=Win32
call compilpil.bat

set DCC="c:\progs\Embarcadero\RAD Studio\11.0\bin\dcc64.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE4\bin\dcc64.exe"
set DelphiVersion=Delphi XE4
set target=Win64
call compilpil.bat

pause