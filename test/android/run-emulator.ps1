param(
  [string]$Avd,
  [string]$SdkRoot,
  [int]$TimeoutSeconds = 600
)

$ErrorActionPreference = 'Stop'
try {
$projectDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$apk = Join-Path $projectDir 'mormot2tests\bin\mormot2tests.apk'
$package = 'org.mormot.tests.android'
$activity = 'com.embarcadero.firemonkey.FMXNativeActivity'

if (-not (Test-Path -LiteralPath $apk)) {
  throw "APK not found: $apk. Run build.cmd first."
}
if ($TimeoutSeconds -lt 1) {
  throw 'TimeoutSeconds must be greater than zero.'
}

$sdkCandidates = @($SdkRoot, $env:ANDROID_SDK_ROOT, $env:ANDROID_HOME)
if ($env:LOCALAPPDATA) {
  $sdkCandidates += (Join-Path $env:LOCALAPPDATA 'Android\Sdk')
}
$sdk = $null
foreach ($candidate in $sdkCandidates) {
  if ($candidate -and
      (Test-Path -LiteralPath (Join-Path $candidate 'emulator\emulator.exe')) -and
      (Test-Path -LiteralPath (Join-Path $candidate 'platform-tools\adb.exe'))) {
    $sdk = (Resolve-Path -LiteralPath $candidate).Path
    break
  }
}
if (-not $sdk) {
  throw 'Android SDK with emulator.exe and adb.exe not found. Pass -SdkRoot or set ANDROID_SDK_ROOT.'
}
$emulator = Join-Path $sdk 'emulator\emulator.exe'
$adb = Join-Path $sdk 'platform-tools\adb.exe'
$savedErrorActionPreference = $ErrorActionPreference
$ErrorActionPreference = 'Continue'
try {
  & $adb start-server *> $null
  if ($LASTEXITCODE -ne 0) {
    throw 'Could not start the ADB server.'
  }
} finally {
  $ErrorActionPreference = $savedErrorActionPreference
}

$avds = @(& $emulator -list-avds 2>$null | ForEach-Object { $_.Trim() } | Where-Object { $_ })
if ($LASTEXITCODE -ne 0) {
  throw "Could not list Android virtual devices with $emulator."
}
if (-not $avds.Count) {
  throw 'No AVD exists. In Android Studio, open Device Manager and create a virtual device with an ARM64-compatible system image (API 23 or newer).'
}
if (-not $Avd) {
  if ($avds.Count -ne 1) {
    throw "Choose an AVD with -Avd. Available: $($avds -join ', ')"
  }
  $Avd = $avds[0]
}
if ($avds -cnotcontains $Avd) {
  throw "AVD '$Avd' was not found. Available: $($avds -join ', ')"
}

function Get-RunningEmulatorSerial {
  $deviceLines = @(& $adb devices 2>$null)
  foreach ($line in $deviceLines) {
    if ($line -match '^(emulator-\d+)\s+device\b') {
      $serial = $Matches[1]
      $nameLines = @(& $adb -s $serial emu avd name 2>$null)
      if ($LASTEXITCODE -eq 0 -and ($nameLines -ccontains $Avd)) {
        return $serial
      }
    }
  }
  return $null
}

$serial = Get-RunningEmulatorSerial
if (-not $serial) {
  Write-Host "Starting Android Studio AVD: $Avd"
  $emulatorProcess = Start-Process -FilePath $emulator -ArgumentList ('-avd "{0}"' -f $Avd) -WorkingDirectory (Split-Path $emulator) -PassThru
}

$deadline = (Get-Date).AddSeconds($TimeoutSeconds)
do {
  if (-not $serial) {
    $serial = Get-RunningEmulatorSerial
  }
  if ($serial) {
    $bootCompleted = (& $adb -s $serial shell getprop sys.boot_completed 2>$null | Out-String).Trim()
    if ($LASTEXITCODE -eq 0 -and $bootCompleted -eq '1') {
      break
    }
  }
  if ($emulatorProcess -and $emulatorProcess.HasExited) {
    throw "The Android emulator exited with code $($emulatorProcess.ExitCode)."
  }
  Start-Sleep -Seconds 3
} while ((Get-Date) -lt $deadline)

if (-not $serial -or $bootCompleted -ne '1') {
  throw "AVD '$Avd' did not finish booting within $TimeoutSeconds seconds."
}

$guestAbis = (& $adb -s $serial shell getprop ro.product.cpu.abilist 2>$null | Out-String).Trim()
if ($LASTEXITCODE -ne 0 -or ($guestAbis -split ',') -notcontains 'arm64-v8a') {
  throw "AVD '$Avd' reports ABIs '$guestAbis'. This APK needs arm64-v8a. Select a compatible system image in Android Studio Device Manager."
}

Write-Host "Installing APK on $serial ($Avd)..."
& $adb -s $serial install -r $apk
if ($LASTEXITCODE -ne 0) {
  throw 'APK installation failed.'
}
& $adb -s $serial shell am start -n "$package/$activity"
if ($LASTEXITCODE -ne 0) {
  throw 'Could not start the test runner.'
}
Write-Host 'App started. Tap "Run tests" in the emulator to begin.'
} catch {
  [Console]::Error.WriteLine('ERROR: ' + $_.Exception.Message)
  exit 2
}
