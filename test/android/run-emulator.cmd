@echo off
setlocal EnableExtensions
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0run-emulator.ps1" %*
exit /b %ERRORLEVEL%
