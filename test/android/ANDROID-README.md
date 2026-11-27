# Android64 native libraries

The FMX test project compiles with Delphi 13 and Android64. It also requires
three native ARM64 files in the repository's `static/arm64-v8a` directory:

| File | Purpose | Source |
| --- | --- | --- |
| `libcrypto-android64.a` | Static OpenSSL crypto library | Build OpenSSL 1.1.1 for Android ARM64 as described in [ANDROID_OPENSSL.md](../../docs/ANDROID_OPENSSL.md). |
| `libssl-android64.a` | Static OpenSSL TLS library | Same OpenSSL build. |
| `libsqlite.so` | SQLite shared library packaged in the APK | Link the Android64 `libsqlite.a` supplied with Delphi as shown below. |

The `static` directory is ignored by Git. The libraries are not included in
the project commit. Each developer must provide compatible ARM64 binaries
before compiling or packaging `mormot2tests.dproj`. The project uses only
relative paths to these files. Do not place machine-specific SDK paths in the
project file.

## Build `libsqlite.so` from Delphi's archive

Delphi supplies `libsqlite.a` under
`%BDS%\lib\android64\release\libsqlite.a`. Use the Android NDK's ARM64
`clang` to turn it into a shared library. For example, from a Windows command
prompt with `BDS` and `ANDROID_NDK_HOME` set:

```bat
"%ANDROID_NDK_HOME%\toolchains\llvm\prebuilt\windows-x86_64\bin\clang.exe" ^
  --target=aarch64-linux-android23 ^
  --sysroot="%ANDROID_NDK_HOME%\toolchains\llvm\prebuilt\windows-x86_64\sysroot" ^
  -shared -Wl,-Bsymbolic -Wl,--no-undefined -Wl,-z,max-page-size=16384 ^
  -Wl,-soname,libsqlite.so -Wl,--whole-archive ^
  "%BDS%\lib\android64\release\libsqlite.a" ^
  -Wl,--no-whole-archive -o "static\arm64-v8a\libsqlite.so" -lc -lm -ldl
```

Run the command from the repository root after creating `static\arm64-v8a`.
The Delphi archive and the OpenSSL archives must be compatible with the
Android64 compiler and NDK used for the APK. Check the redistribution terms
of any native binaries before sharing them.

## Build and launch

Run `test\android\build.cmd Debug` from the repository root,
then `deploy-run.cmd` for an ADB-connected phone or `run-emulator.cmd` for an
Android Studio AVD. The APK contains ARM64 native code, so the target Android
device must report `arm64-v8a` among its supported ABIs.
