@echo off
REM CLI Client - Compilation Script (Windows)
REM Usage:
REM   compile_cli.bat         # Remote mode (HTTP client)
REM   compile_cli.bat local   # Local mode (embedded SQLite)

set "SCRIPT_DIR=%~dp0"
cd /d "%SCRIPT_DIR%"

if not exist bin mkdir bin
if not exist bin\units mkdir bin\units

where fpc >nul 2>nul
if errorlevel 1 (
    echo Error: Free Pascal Compiler ^(fpc^) is not installed
    exit /b 1
)

set "DEFINE_FLAG="
set "MODE=REMOTE"
if "%~1"=="local" (
    set "DEFINE_FLAG=-dLOCAL_MODE"
    set "MODE=LOCAL"
)

echo Compiling cli_client (%MODE% mode)...

fpc src/cli_client.pas ^
    -obin/cli_client ^
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
    -Fu../../src/core ^
    -Fu../../src/lib ^
    -Fu../../src/crypt ^
    -Fu../../src/orm ^
    -Fu../../src/rest ^
    -Fu../../src/db ^
    -Fu../../src/net ^
    -Fu../../src/soa ^
    -Fu../../src/app ^
    %DEFINE_FLAG%

if errorlevel 1 (
    echo.
    echo Compilation failed!
    exit /b 1
)

echo.
echo Compilation successful! Binary: bin\cli_client.exe (%MODE% mode)
echo.
echo Usage:
echo   bin\cli_client.exe list [status]
echo   bin\cli_client.exe add ^<title^> ^<description^> ^<priority^>
echo   bin\cli_client.exe get ^<id^>
echo   bin\cli_client.exe complete ^<id^>
echo   bin\cli_client.exe delete ^<id^>
echo   bin\cli_client.exe search ^<term^>
echo   bin\cli_client.exe comment ^<task_id^> ^<content^> ^<author^>
echo   bin\cli_client.exe help
