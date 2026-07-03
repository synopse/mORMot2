@echo off
REM mORMot2 Task Manager - Run Script (Windows)

echo Starting mORMot2 Task Manager Server...
echo =======================================
echo.

set "SCRIPT_DIR=%~dp0"
cd /d "%SCRIPT_DIR%"

if not exist "bin\task_manager.exe" (
    echo Error: task_manager binary not found
    echo Please run compile.bat first
    exit /b 1
)

REM Create data directory if it doesn't exist
if not exist data mkdir data

REM Run the server
bin\task_manager.exe
