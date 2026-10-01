@echo off
setlocal EnableExtensions
set "PROJECT_DIR=%~dp0"
set "APK=%PROJECT_DIR%mormot2tests\bin\mormot2tests.apk"
set "PACKAGE=org.mormot.tests.android"
set "ACTIVITY=com.embarcadero.firemonkey.FMXNativeActivity"

if not exist "%APK%" (
  echo APK not found: "%APK%"
  echo Run build.cmd first.
  exit /b 2
)

set "ADB=adb.exe"
if defined ANDROID_SDK_ROOT if exist "%ANDROID_SDK_ROOT%\platform-tools\adb.exe" set "ADB=%ANDROID_SDK_ROOT%\platform-tools\adb.exe"
if defined ANDROID_HOME if exist "%ANDROID_HOME%\platform-tools\adb.exe" set "ADB=%ANDROID_HOME%\platform-tools\adb.exe"
if defined ANDROID_ADB set "ADB=%ANDROID_ADB%"
"%ADB%" get-state >nul
if errorlevel 1 (
  echo No single authorized Android device was found. Check adb and ANDROID_SERIAL.
  exit /b 2
)
"%ADB%" install -r "%APK%"
if errorlevel 1 exit /b 1
"%ADB%" shell am start -n "%PACKAGE%/%ACTIVITY%"
if errorlevel 1 exit /b 1
echo App started. Tap "Run tests" on the device to begin.
exit /b 0
