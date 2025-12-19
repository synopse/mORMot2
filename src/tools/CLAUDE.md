# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `/mnt/w/mORMot2/src/tools` directory contains command-line utilities built on the mORMot 2 framework. Each tool is a standalone executable targeting specific use cases: cryptography, service management, HTTP downloads, debugging, and code generation.

## Tool Architecture

### Common Structure
All tools follow a consistent pattern:
- **Entry point**: `<tool>.dpr` (console application)
- **Implementation**: Either inline in `.dpr` or separate `mormot.tools.<tool>.pas`
- **Dependencies**: Relative paths to mORMot core units (e.g., `..\..\core\mormot.core.base.pas`)
- **Cross-platform**: Windows (console), Linux, macOS support

### Build Requirements
- **Delphi**: RAD Studio (uses `.dproj` when available)
- **FPC**: Uses `.lpi` project files (Free Pascal Compiler)
- **Compiler directives**: `{$I ..\..\mormot.defines.inc}` and `{$I ..\..\mormot.uses.inc}`

## Tools Reference

### ecc - Public Key Cryptography Tool
**Location**: `/mnt/w/mORMot2/src/tools/ecc/`
**Implementation**: `mormot.tools.ecc.pas` (1,000+ lines of ECC functions)

**Purpose**: Certificate-based public-key cryptography using ECC-secp256r1

**Key Operations**:
- Key management: `new`, `rekey`, `source`, `infopriv`
- Signing: `sign`, `verify` (ECDSA digital signatures)
- Encryption: `crypt`, `decrypt`, `infocrypt` (ECIES encryption)
- Symmetric: `aeadcrypt`, `aeaddecrypt`
- Certificate chains: `chain`, `chainall`
- Password manager: `cheatinit`, `cheat`

**Architecture Notes**:
- Uses `TEccCertificate` for key pair management
- Password-protected private keys with configurable PBKDF2 rounds
- `.synecc` format for encrypted files with embedded metadata
- Certificate chaining produces `.ca` JSON arrays

### agl (Angelize) - Cross-Platform Services Manager
**Location**: `/mnt/w/mORMot2/src/tools/agl/`
**Implementation**: Uses `mormot.app.agl.pas` and `mormot.app.daemon.pas`

**Purpose**: Manage multiple executables as Windows Services or POSIX Daemons

**Key Features**:
- Main service (`TSynAngelize`) manages sub-processes
- JSON configuration files per sub-service
- Leveled startup (dependency rings)
- Auto-restart on crash with backoff
- HTTP/HTTPS health check validation
- Console output redirection
- HTML status page generation

**Commands**:
- `/install`, `/start`, `/stop`, `/uninstall` - Service lifecycle
- `/run`, `/verbose` - Console mode for testing
- `/list` - Current status of all sub-processes
- `/retry` - Force restart of failed service
- `/new` - Create new service config
- `/settings` - Validate JSON configuration

**Architecture Notes**:
- `TSynAngelize` extends `TSynDaemon` (base daemon class)
- Thread-safe process monitoring with watchdog timers
- Exit code handling (fatal vs. retryable errors)
- Graceful shutdown (`WM_QUIT`/`SIGTERM`) with fallback kill

### mget - HTTP/HTTPS Downloading Tool
**Location**: `/mnt/w/mORMot2/src/tools/mget/`
**Implementation**: `mormot.tools.mget.pas`

**Purpose**: Retrieve files via HTTP/HTTPS with unique features

**Key Features**:
1. **Resume Downloads**: Uses `RANGE` headers, `.part` extension for incomplete files
2. **Hash Verification**: MD5/SHA1/SHA256 validation (SHA-NI acceleration on x64)
   - Download hash from `<url>.sha256` or specify on command line
3. **Peer-to-Peer Cache**: LAN-based file sharing to reduce WAN bandwidth
   - UDP broadcast discovery (AES-GCM-128 authenticated)
   - Local HTTP cache server with bearer authentication
   - Hash-based storage prevents tampering

**Common Usage**:
```bash
mget https://someuri/some/file.ext                        # Basic download with resume
mget 4544b3...68ba7a@http://someuri/some/file.ext        # With hash verification
mget https://someuri/some/file.ext --hashAlgo sha256     # Download hash first
mget --prompt                                             # Interactive mode
mget --prompt --peer                                      # With PeerCache enabled
```

**Architecture Notes**:
- Built on `THttpClientSocket.WGet()` from `mormot.net.client`
- PeerCache uses `THttpPeerCache` from `mormot.net.server`
- Secret-derived AES-GCM for UDP/HTTP authentication
- IP banning for invalid requests (DOS protection)

### mab - Debugging Symbol Converter
**Location**: `/mnt/w/mORMot2/src/tools/mab/`
**Implementation**: Inline in `mab.dpr`

**Purpose**: Convert debugging symbols to `.mab` format for stack trace resolution

**Operations**:
- Input: `.map` (Delphi) or DWARF (FPC) debugging info
- Output: `.mab` binary format (mORMot Advanced Binding)
- Can embed `.mab` into executables (`.exe`, `.dll`, `.bpl`, `.ocx`)

**Usage**:
```bash
mab *.map              # Convert all .map files to .mab
mab myapp.exe          # Process myapp.map and embed into myapp.exe
mab somefile.map       # Create somefile.mab
```

**Architecture Notes**:
- Validates `.map` is newer than executable before processing
- Embeds `.mab` as resource or appended data
- Used by mORMot logging for stack trace symbolication

### mopenapi - OpenAPI/Swagger Code Generator
**Location**: `/mnt/w/mORMot2/src/tools/mopenapi/`
**Implementation**: Uses `mormot.net.openapi.pas`

**Purpose**: Generate Delphi/FPC client `.pas` units from OpenAPI/Swagger `.json` specifications

**Usage**:
```bash
mopenapi --help
mopenapi swagger.json PetStore
mopenapi OpenApiAuth.json /concise
mopenapi test.json --options=DtoNoExample,DtoNoPattern
```

**Architecture Notes**:
- RTTI-based command-line parsing (`TOptions` class)
- `TOpenApiParserOptions` for generation customization
- Outputs type-safe Pascal interfaces and DTOs

## Development Patterns

### Adding New Tools
1. Create subdirectory: `/mnt/w/mORMot2/src/tools/<toolname>/`
2. Create `<toolname>.dpr` with standard header and relative includes
3. Implement in separate `.pas` file if complex (e.g., `mormot.tools.<toolname>.pas`)
4. Update this `README.md` with tool description
5. Add both Delphi (`.dproj`) and FPC (`.lpi`) project files if needed

### Common Includes Pattern
```pascal
{$I ..\..\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I ..\..\mormot.uses.inc}
  classes,
  sysutils,
  mormot.core.base         in '..\..\core\mormot.core.base.pas',
  mormot.core.os           in '..\..\core\mormot.core.os.pas',
  // ... more units
```

### Logging
All tools use `mormot.core.log.pas` for structured logging:
- Console output for user feedback
- Optional log files for diagnostics
- Cross-platform (Windows/Linux/macOS)

### Console Application Framework
Built on `mormot.app.console.pas` (`TConsoleApplication` base class):
- Command-line parsing with switches
- Help text generation
- Error handling and exit codes
- RTTI-based option parsing

## Key Architectural Components

### Cryptography Stack (ecc)
```
mormot.crypt.ecc256r1.pas    # Low-level ECC primitives
         ↓
mormot.crypt.ecc.pas         # Certificate management
         ↓
mormot.tools.ecc.pas         # End-user commands
         ↓
ecc.dpr                      # CLI entry point
```

### Service Management Stack (agl)
```
mormot.app.daemon.pas        # Base daemon/service (TSynDaemon)
         ↓
mormot.app.agl.pas           # Multi-process manager (TSynAngelize)
         ↓
agl.dpr                      # CLI entry point
```

### HTTP Stack (mget)
```
mormot.net.client.pas        # THttpClientSocket.WGet()
mormot.net.server.pas        # THttpPeerCache
         ↓
mormot.tools.mget.pas        # CLI implementation
         ↓
mget.dpr                     # CLI entry point
```

## Testing Tools

### Manual Testing
Each tool should be tested with:
1. `<tool> --help` or `/<tool> /help` - Verify help output
2. Basic operations with sample files
3. Error conditions (missing files, invalid input)
4. Cross-platform builds (Windows, Linux, macOS)

### Integration with mORMot Test Suite
Tools can be tested via `mormot.test.*.pas` units in `/mnt/w/mORMot2/test/`

## Security Considerations

### ecc Tool
- Private keys encrypted with PBKDF2-derived passwords
- Configurable iteration rounds (default: 60,000)
- Secure key deletion (memory wiping)
- Certificate expiration validation

### mget PeerCache
- Shared secret required for all peer communication
- AES-GCM-128 encryption/authentication for UDP/HTTP
- IP banning after invalid requests
- Optional HTTPS for local transfers (`SelfSignedHttps`)
- Local cache requires proper filesystem ACLs

### agl Service Manager
- Runs as SYSTEM/root by default
- JSON config files should have restricted permissions
- Sub-process impersonation (planned feature)
- Watchdog prevents runaway processes

## Performance Notes

- **Hash calculation**: SHA-NI hardware acceleration on modern x64 CPUs (mget, ecc)
- **ECC operations**: Optimized secp256r1 curve implementation
- **HTTP transfers**: Streaming (no full file buffering)
- **PeerCache**: Local network transfers avoid WAN bottleneck

## Related Documentation

**📖 SAD Documentation**: See [SAD Chapter 26: Source Code](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-26.md) for:
- Repository structure and organization
- Static libraries setup and deployment
- Cross-platform compilation guidelines

**Framework References**:
- mORMot 2 main documentation: `/mnt/w/mORMot2/README.md`
- Core framework: `/mnt/w/mORMot2/src/core/`
- Network layer: `/mnt/w/mORMot2/src/net/`
- Application framework: `/mnt/w/mORMot2/src/app/`
- Cryptography: `/mnt/w/mORMot2/src/crypt/`

## TODO Items

### agl (from README.md)
- Thread/CPU affinity per sub-process
- User impersonation and POSIX chroot
- Enhanced monitoring (CPU, RAM metrics)
- `/restart` command shortcut
- Improved "Watch" feature stability

### General
- Windows code signing for all tools
- Homebrew formula for macOS installation
- Snap/AppImage packages for Linux
- Continuous integration builds

---

**Last Updated**: 2025-10-10
**mORMot Version**: 2.x
**Maintained By**: Synopse mORMot Team
