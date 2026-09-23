# mORMot2 Android64 test runner

Open `mormot2tests-android.dproj` in Delphi and select Android64. The project
uses relative paths to the mORMot source and test units. The app waits for the
user to tap **Run tests**; launching it does not start the suite.

**Native libraries are required before compilation and packaging.** Place
these ARM64 files in `../../static/arm64-v8a`:

- `libcrypto-android64.a`
- `libssl-android64.a`
- `libsqlite.so`

Those native binaries are local dependencies in this repository's ignored
`static` directory. See [ANDROID-README.md](ANDROID-README.md) for their
sources and build instructions. Supply compatible files before compiling or
packaging.
The Delphi Android64 SDK and a configured Android platform are also required.

From a RAD Studio command prompt, or a terminal with `RADSTUDIO_ROOT` set:

```bat
build.cmd Debug
deploy-run.cmd
```

`build.cmd` compiles and packages a debug signed APK. Its optional `Release`
argument selects optimized compilation with the same local debug packaging.
`deploy-run.cmd` installs the
existing APK on an ADB connected device and opens the app. Set
`ANDROID_SERIAL` if multiple devices are connected. `ANDROID_ADB` can point
to a specific `adb.exe`.

## Android Studio emulator

The Android Studio installation supplies the emulator through its Android SDK.
Create an AVD in Android Studio's **Device Manager** with a system image that
supports `arm64-v8a` and API 23 or newer. The APK contains only ARM64 native
libraries, so an AVD that reports only `x86` or `x86_64` cannot run it. On an
Intel or AMD host, an ARM64 system image may run slowly without VM acceleration.

After `build.cmd`, start, install, and open the app with:

```bat
run-emulator.cmd
run-emulator.cmd -Avd My_Android_AVD
```

When exactly one AVD exists, the first command selects it. With multiple AVDs,
pass the name shown by `emulator -list-avds`. The script locates the SDK via
`ANDROID_SDK_ROOT`, `ANDROID_HOME`, or the standard per-user SDK location. Use
`-SdkRoot` for another location. It selects the named emulator even when a
physical device is connected, checks the guest ABI, and waits for Android to
finish booting. The test suite still starts only when **Run tests** is tapped.

The suite covers core, ORM, and SOA cases. It may take several minutes on a
phone. Results appear in the app and can be shared after completion.
