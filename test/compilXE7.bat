@echo off
cls

set DCC="c:\progs\Embarcadero\XE7\bin\fastdcc32.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE7\bin\dcc32.exe"
set DelphiVersion=Delphi XE7
set target=Win32
call compilpil.bat

set DCC="c:\progs\Embarcadero\XE7\bin\fastdcc64.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE7\bin\dcc64.exe"
set DelphiVersion=Delphi XE7
set target=Win64
call compilpil.bat

pause