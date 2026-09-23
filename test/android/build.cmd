@echo off
setlocal EnableExtensions
set "PROJECT_DIR=%~dp0"
set "APK=%PROJECT_DIR%mormot2tests\bin\mormot2tests.apk"
set "CONFIG=%~1"
if not defined CONFIG set "CONFIG=Debug"
if /I not "%CONFIG%"=="Debug" if /I not "%CONFIG%"=="Release" (
  echo Usage: build.cmd [Debug^|Release]
  exit /b 2
)

set "NATIVE_DIR=%PROJECT_DIR%..\..\static\arm64-v8a"
if not exist "%NATIVE_DIR%\libcrypto-android64.a" goto :missing_native
if not exist "%NATIVE_DIR%\libssl-android64.a" goto :missing_native
if not exist "%NATIVE_DIR%\libsqlite.so" goto :missing_native

if not defined BDS call :setup_delphi
if errorlevel 1 exit /b 2

pushd "%PROJECT_DIR%"
msbuild.exe "mormot2tests.dproj" /nologo /verbosity:minimal /target:Make /property:Config=%CONFIG%;Platform=Android64
if errorlevel 1 goto :build_failed
msbuild.exe "mormot2tests.dproj" /nologo /verbosity:minimal /target:Deploy /property:Config=%CONFIG%;Platform=Android64
if errorlevel 1 goto :build_failed
if not exist "%APK%" goto :build_failed
powershell.exe -NoProfile -Command "$ErrorActionPreference='Stop'; Add-Type -AssemblyName System.IO.Compression.FileSystem; $z=[IO.Compression.ZipFile]::OpenRead($env:APK); try { if (-not $z.GetEntry('classes.dex') -or -not $z.GetEntry('lib/arm64-v8a/libmormot2tests.so')) { exit 1 } } finally { $z.Dispose() }"
if errorlevel 1 goto :invalid_apk
popd
echo APK: %APK%
exit /b 0

:missing_native
echo Missing Android64 native libraries in "%NATIVE_DIR%".
echo Required: libcrypto-android64.a, libssl-android64.a, libsqlite.so
exit /b 2

:build_failed
popd
echo Android64 build or packaging failed.
exit /b 1

:invalid_apk
popd
echo APK is missing Android code or the Android64 native library.
exit /b 1

:setup_delphi
set "RSVARS="
if defined RADSTUDIO_ROOT if exist "%RADSTUDIO_ROOT%\bin\rsvars.bat" set "RSVARS=%RADSTUDIO_ROOT%\bin\rsvars.bat"
if not defined RSVARS for /d %%D in ("%ProgramFiles(x86)%\Embarcadero\Studio\*") do if exist "%%~fD\bin\rsvars.bat" set "RSVARS=%%~fD\bin\rsvars.bat"
if not defined RSVARS (
  echo Delphi rsvars.bat was not found. Set RADSTUDIO_ROOT or run from a RAD Studio command prompt.
  exit /b 1
)
call "%RSVARS%"
exit /b %ERRORLEVEL%
