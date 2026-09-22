# Building OpenSSL 1.1.1 for Android ARM64

This document describes the static OpenSSL build used by the Android64
mORMot2 test runner.

## Source

Use the official OpenSSL 1.1.1 source archive:

<https://openssl-library.org/source/old/1.1.1/>

The tested source version is OpenSSL 1.1.1w. OpenSSL 3.x is not used for the
Android64 test runner because the mORMot2 Android configuration expects the
OpenSSL 1.1 static library names.

## Prerequisites

- Delphi 13 with the Android64 platform installed
- Android NDK with the ARM64 LLVM toolchain
- MSYS2 Bash
- Perl (Strawberry Perl works on Windows)
- GNU Make

The NDK used for the verified build was Android NDK r26d. The Delphi linker
uses its own Android NDK during the final link step; the produced ARM64
archives are ABI-compatible with that toolchain.

## Build OpenSSL

Run the following from MSYS2 Bash after adjusting the paths:

```bash
export ANDROID_NDK_HOME=/c/mormot2/android-toolchain/android-ndk-r26d
export PATH="$ANDROID_NDK_HOME/toolchains/llvm/prebuilt/windows-x86_64/bin:/usr/bin:$PATH"

cd /c/mormot2/android-toolchain/openssl-1.1.1w-android64

perl Configure android-arm64 \
  -D__ANDROID_API__=23 \
  no-shared no-tests \
  --prefix=/c/mormot2/android-toolchain/openssl-install-android64

make -j4 build_libs
```

The build must produce static libraries containing AArch64 objects.

## Install into the test project

Copy or build the archives into this directory:

```text
test/android-libs/arm64-v8a/
```

The required filenames are:

```text
libcrypto-android64.a
libssl-android64.a
```

The Android64 project links this directory with `-L` and deploys both
archives into the APK's `lib/arm64-v8a` deployment area. The build script
`test/build_android_runner.cmd` checks that both files exist before invoking
MSBuild.

## Build and verify

From the repository root:

```bat
test\build_android_runner.cmd
```

The Android runner performs an OpenSSL availability smoke test before running
the test suite. A successful run writes:

```text
OpenSSL TLS smoke check passed
```

The full Android suite also exercises `TRestHttpsServer` and
`TRestHttpsClient`. Keep the Android runner on a worker thread; running the
complete suite on the FMX UI thread can trigger Android's "Application Not
Responding" watchdog.

## Notes

- Do not add FireDAC units for this setup.
- Keep the OpenSSL linker and deployment changes restricted to Android64.
- OpenSSL 1.1.1 is end-of-life; use it here only where compatibility with the
  current mORMot2 Android static-link configuration is required.
