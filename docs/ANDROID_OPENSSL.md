# Building OpenSSL for Android ARM64

This document describes the OpenSSL libraries used by the Android64 mORMot2
test runner.

By default, `mormot.lib.openssl11.pas` loads OpenSSL 3.x (or 4.x) dynamically
on Android from `libcrypto.so` and `libssl.so` packaged in the APK
`lib/arm64-v8a` folder. The legacy static OpenSSL 1.1 link is still available
with the `OPENSSLANDROIDSTATIC` conditional (see below).

## Source

Use an official OpenSSL 3.x source archive, preferably an LTS release:

<https://openssl-library.org/source/>

Tested with OpenSSL 3.5.9 (LTS) and OpenSSL 4.1.0. Verify the SHA-256 checksum
published next to the archive.

## Prerequisites

- Delphi 13 with the Android64 platform installed
- The Android NDK installed with Delphi (verified: NDK 27.1.12297006)
- MSYS2 Bash with Perl and GNU Make (`pacman -S perl make`)

## Build OpenSSL 3.x

Android only packages and loads unversioned `lib*.so` file names, so the
libraries must be linked with `SONAME` set to `libcrypto.so` and `libssl.so`.
Android 15+ devices with 16 KB pages also require a 16 KB segment alignment.

Run the following from MSYS2 Bash after adjusting the paths:

```bash
NDK=/c/path/to/ndk/27.1.12297006
BIN=$NDK/toolchains/llvm/prebuilt/windows-x86_64/bin
export ANDROID_NDK_ROOT=$NDK
export PATH="$BIN:/usr/bin:$PATH"

cd openssl-3.5.9
perl Configure android-arm64 -D__ANDROID_API__=23 \
  shared no-tests no-docs no-apps no-module \
  -Wl,-z,max-page-size=16384
make -j4 build_libs
```

On Windows, the final link of `libcrypto.so` may fail with
`clang.exe: Argument list too long`: the NDK `aarch64-linux-android*-clang`
wrapper is a shell script, and the object list exceeds the Windows command
line limit. In this case, the already compiled `*-shlib-*.o` objects can be
linked through response files - the object lists are the prerequisites of the
`libcrypto.so:` and `libssl.so:` Makefile rules:

```bash
deps() {
  awk -v t="$1:" '$1==t {f=1} f {print} f && !/[\\]$/ {exit}' Makefile |
    tr -s ' \t' '\n\n' | grep -E '[.]o$'
}
deps libcrypto.so > crypto.rsp
deps libssl.so > ssl.rsp
make $(cat crypto.rsp ssl.rsp | sort -u) libcrypto.ld libssl.ld \
  providers/libdefault.a providers/liblegacy.a providers/libcommon.a

CL="$BIN/clang.exe --target=aarch64-linux-android23"
LDF="-shared -Wl,-z,max-page-size=16384 -Wl,--no-undefined -Wl,-Bsymbolic"
$CL $LDF -Wl,-soname,libcrypto.so -Wl,--version-script=libcrypto.ld \
  @crypto.rsp providers/libdefault.a providers/liblegacy.a \
  providers/libcommon.a -o libcrypto.so -ldl -lc -lm
$CL $LDF -Wl,-soname,libssl.so -Wl,--version-script=libssl.ld \
  @ssl.rsp -L. -lcrypto -o libssl.so -ldl -lc
llvm-strip --strip-unneeded libcrypto.so libssl.so
```

Check the result:

```bash
llvm-readelf -d libssl.so | grep -E 'SONAME|NEEDED'   # libssl.so, libcrypto.so
llvm-readelf -lW libcrypto.so | grep LOAD             # alignment 0x4000
```

## Install into the test project

Copy the libraries into this directory:

```text
static/arm64-v8a/
```

The required filenames are:

```text
libcrypto.so
libssl.so
```

`test/android/mormot2tests.deployproj` packages both files into the APK
`lib/arm64-v8a` folder, next to `libsqlite.so`. The build script
`test/android/build.cmd` checks that they exist before invoking MSBuild. See
[`ANDROID-README.md`](../test/android/ANDROID-README.md) for all native library
requirements.

## Build and verify

From the repository root:

```bat
test\android\build.cmd Debug
```

The Android runner loads OpenSSL when the user starts the test suite with
**Run tests**.

The full Android suite also exercises `TRestHttpsServer` and
`TRestHttpsClient`. Keep the Android runner on a worker thread; running the
complete suite on the FMX UI thread can trigger Android's "Application Not
Responding" watchdog.

## Legacy: static OpenSSL 1.1

Define `OPENSSLANDROIDSTATIC` to link OpenSSL 1.1.1 statically instead, from
`libcrypto-android64.a` and `libssl-android64.a` in `static/arm64-v8a`
(`libcrypto-android32.a` and `libssl-android32.a` for 32-bit ARM). Build them
with the same toolchain, using `no-shared no-tests` in the OpenSSL 1.1.1w
`Configure` command. The libraries must then be removed from the deployment
and from the `build.cmd` check. OpenSSL 1.1.1 is end-of-life; use this only
where an existing project still requires it.

## Notes

- Do not add FireDAC units for this setup.
- Keep the OpenSSL linker and deployment changes restricted to Android64.
