@echo off
REM mORMot2 Task Manager - Compilation Script (Windows)

echo mORMot2 Task Manager - Compilation Script
echo ==========================================
echo.

REM Determine the script directory and switch to it
set "SCRIPT_DIR=%~dp0"
cd /d "%SCRIPT_DIR%"

if not exist bin mkdir bin
if not exist bin\units mkdir bin\units

REM Check if FPC is installed
where fpc >nul 2>nul
if errorlevel 1 (
    echo Error: Free Pascal Compiler ^(fpc^) is not installed
    exit /b 1
)

echo Compiling task_manager...
fpc src/task_manager.pas ^
    -obin/task_manager ^
    -FUbin/units ^
    -Fl"%SCRIPT_DIR%..\..\static\x86_64-win64" ^
    -O1 -Mobjfpc ^
    -Fi../../src ^
    -Fusrc ^
    -Fusrc/dom/tasks ^
    -Fusrc/dom/tags ^
    -Fusrc/infra/tasks ^
    -Fusrc/infra/tags ^
    -Fusrc/app/tasks ^
    -Fusrc/app/tags ^
    -Fusrc/serv/app ^
    -Futests/tasks ^
    -Futests/tags ^
    -Fu../../src/core ^
    -Fu../../src/lib ^
    -Fu../../src/crypt ^
    -Fu../../src/orm ^
    -Fu../../src/rest ^
    -Fu../../src/db ^
    -Fu../../src/net ^
    -Fu../../src/soa ^
    -Fu../../src/app

if errorlevel 1 (
    echo.
    echo Compilation failed!
    exit /b 1
)

echo.
echo Compilation successful!
echo Binary: bin\task_manager.exe
echo.
echo To run the server:
echo   cd %SCRIPT_DIR% ^&^& bin\task_manager.exe
echo.
echo The server will be available at:
echo   http://localhost:8080
echo.
echo CQRS Services:
echo   http://localhost:8080/taskmanager/TaskQuery
echo   http://localhost:8080/taskmanager/TaskCommand
echo   http://localhost:8080/taskmanager/TagQuery
echo   http://localhost:8080/taskmanager/TagCommand
