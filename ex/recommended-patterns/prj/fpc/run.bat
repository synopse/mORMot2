@echo off
REM mORMot2 Task Manager - Run Script (Windows)

echo Starting mORMot2 Task Manager Server...
echo =======================================
echo.

REM Determine the project root (two levels above this script) and switch to it
cd /d "%~dp0..\.."

if not exist "bin\task_manager.exe" (
    echo Error: task_manager binary not found
    echo Please run prj\fpc\compile.bat first
    exit /b 1
)

REM Create data directory if it doesn't exist
if not exist data mkdir data

REM Run the server
bin\task_manager.exe
