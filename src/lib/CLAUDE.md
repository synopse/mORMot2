# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `/mnt/w/mORMot2/src/lib` directory contains thin wrappers to external C/stdcall libraries used by the mORMot 2 framework. These are low-level API bindings that are then encapsulated in higher-level units throughout the framework.

**Key Principle**: These units provide raw FFI (Foreign Function Interface) access only. Application code should use the high-level wrapper units instead.

## Architecture Pattern

```
Low-Level (this directory)          High-Level (other src folders)
mormot.lib.z.pas             --->   mormot.core.zip.pas
mormot.lib.openssl11.pas     --->   mormot.crypt.openssl.pas
mormot.lib.curl.pas          --->   mormot.net.client.pas / mormot.net.http.pas
mormot.lib.quickjs.pas       --->   mormot.script.quickjs.pas
mormot.lib.lizard.pas        --->   mormot.core.buffers.pas (TAlgoLizard*)
mormot.lib.gdiplus.pas       --->   mormot.ui.gdiplus.pas
mormot.lib.win7zip.pas       --->   I7zReader/I7zWriter interfaces
```

**Never use `mormot.lib.*` units directly in application code** - they are framework internals.

## Library Linking Strategies

Each library unit supports multiple linking modes via conditionals (defined in `mormot.defines.inc` or unit-specific):

### zlib (`mormot.lib.z.pas`)
- `ZLIBSTATIC` - Static .o/.obj linking (FPC Win32/Win64, Delphi Win32)
- `ZLIBPAS` - Pure Pascal paszlib (FPC Android, FPC ARM Windows)
- `ZLIBEXT` - Dynamic libz.so (FPC Linux/BSD/Mac)
- `ZLIBRTL` - Delphi RTL's system.zlib.pas (Delphi Win64)
- `LIBDEFLATESTATIC` - Faster libdeflate for in-memory compression (FPC Intel Linux/Win32)

### OpenSSL (`mormot.lib.openssl11.pas`)
- Dynamic loading by default (libcrypto/libssl .dll/.so/.dylib)
- Supports OpenSSL 1.1.x (deprecated) and 3.x (current)
- **Windows**: SChannel used by default until `OpenSslInitialize()` called
- **POSIX**: Always attempts to load OpenSSL at startup
- **macOS**: System .dylib unstable - use pre-built from https://synopse.info/files/

### libcurl (`mormot.lib.curl.pas`)
- `LIBCURLSTATIC` - Static libcurl.a (mainly Android)
- Dynamic by default (libcurl.so/.dll)
- `LIBCURLMULTI` - Include advanced "multi session" API

### QuickJS (`mormot.lib.quickjs.pas`)
- `LIBQUICKJSSTATIC` - Static linking (no external .dll/.so)
- Uses patched https://github.com/c-smile/quickjspp fork
- ES2020 support: modules, async generators, proxies, BigInt

### Windows-Only Libraries
- `mormot.lib.winhttp.pas` - WinHTTP, WebSockets, http.sys APIs
- `mormot.lib.sspi.pas` - SSPI/SChannel authentication
- `mormot.lib.gdiplus.pas` - GDI+ graphics
- `mormot.lib.win7zip.pas` - 7-Zip DLL wrapper (requires 7z.dll)
- `mormot.lib.uniscribe.pas` - Windows text rendering

### POSIX-Only Libraries
- `mormot.lib.gssapi.pas` - GSSAPI/Kerberos authentication

## Static Library Files

Static binaries (.o/.obj) are stored in `/mnt/w/mORMot2/static/` and must be downloaded separately:
```bash
# Download and extract to mORMot2/static/
https://synopse.info/files/mormot2static.7z
https://synopse.info/files/mormot2static.tgz
```

The `mormot.lib.static.pas` unit provides:
- Linking constants (`_PREFIX` for symbol names)
- Minimal libc replacement for Windows static linking
- GCC math function wrappers
- FPU exception masking

## Common Development Tasks

### Adding a New External Library
1. Create `mormot.lib.newlib.pas` with raw API declarations
2. Define linking conditionals (`NEWLIBSTATIC`, `NEWLIBEXT`, etc.)
3. Add to `mormot.defines.inc` if platform-specific defaults needed
4. Create high-level wrapper in appropriate `src/` subfolder
5. Update `src/lib/README.md` with new library documentation

### Debugging Library Loading Issues
```pascal
// OpenSSL
if not OpenSslIsAvailable then
  raise Exception.Create(OpenSslVersion); // shows error

// libcurl
curl.GlobalInitialize; // raises ECurl if library not found

// QuickJS
{$ifdef LIBQUICKJS}
// code here won't compile if QuickJS unavailable
{$endif}
```

### Platform-Specific Conditionals
All library units include `{$I ..\mormot.defines.inc}` which centralizes:
- Compiler detection (FPC vs Delphi)
- OS detection (OSWINDOWS, OSPOSIX, OSDARWIN, etc.)
- CPU architecture (CPU32, CPU64, CPUINTEL, CPUARM, etc.)
- Feature flags (USE_OPENSSL, LIBDEFLATESTATIC, etc.)

## Important Notes

### OpenSSL Warnings
- **Windows**: May load obsolete DLLs from system PATH - place correct version in EXE directory
- **macOS**: System dylibs are unstable - use pre-built binaries
- **Legal**: Comply with cryptographic software restrictions in your country (see LICENSE.md)

### 7-Zip Licensing
Using `mormot.lib.win7zip.pas` requires:
1. Documenting use of 7-Zip in your application
2. Stating 7-Zip is LGPL licensed
3. Linking to www.7-zip.org for source code

### Memory Management
- `OPENSSLUSERTLMM` - Make OpenSSL use Pascal RTL memory (risky - OpenSSL leaks)
- Most libraries use their own allocators
- Static linking on Windows redirects malloc/free to msvcrt.dll via `mormot.lib.static.pas`

## Cross-Compiler Compatibility

These units maintain compatibility with:
- **FPC**: 3.2.x stable and trunk
- **Delphi**: 7, 2007-2010, XE4-XE8, 10.x-12.x

Platform support:
- **Full support**: Windows (x86/x64), Linux (x86/x64/ARM), BSD, macOS (x64/ARM)
- **Partial support**: Android, iOS (client-only units)

## Related Documentation

**📖 SAD Chapters**:
- [Chapter 3: Framework Architecture](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-03.md) - Library linking strategies, static files

**Framework References**:
- **Main README**: `/mnt/w/mORMot2/README.md`
- **Source Overview**: `/mnt/w/mORMot2/src/README.md`
- **Static Binaries**: `/mnt/w/mORMot2/static/README.md`
- **Defines Reference**: `/mnt/w/mORMot2/src/mormot.defines.inc` (user-triggered conditionals documented inline)

## Quick Reference

**When working in `src/lib/`:**
- These are thin wrappers, not application APIs
- Follow existing patterns (TZLib record, OpenSSL functions)
- Document conditionals in unit header
- Test on multiple platforms if changing linking logic
- Update corresponding high-level wrapper if API changes

**When using external libraries:**
- Import high-level units (`mormot.core.zip`, `mormot.crypt.openssl`)
- Check availability at runtime if optional (`OpenSslIsAvailable`)
- Handle missing libraries gracefully (catch exceptions)
- Document external dependencies in your project

---

**Framework**: Synopse mORMot 2
**License**: MPL 1.1 / GPL 2.0 / LGPL 2.1 (disjunctive tri-license)
**Website**: https://synopse.info - http://mORMot.net
