# Chapter 26: Source Code

*Adopt a mORMot*

This chapter covers the licensing terms of the *mORMot 2* framework, its source code availability, and complete installation instructions for both Delphi and Free Pascal/Lazarus development environments. Understanding how to properly set up your development environment is essential for working effectively with *mORMot 2*.

## 26.1. License

### 26.1.1. Three Licenses Model

The framework source code is licensed under a disjunctive three-license giving the user the choice of one of the three following sets of free software/open source licensing terms:
- *Mozilla Public License*, version 1.1 or later (MPL);
- *GNU General Public License*, version 2.0 or later (GPL);
- *GNU Lesser General Public License*, version 2.1 or later (LGPL), with *linking exception* of the *FPC modified LGPL*.

*FPC modified LGPL* is the *Library GNU General Public License* with the following modification:

*As a special exception of the LGPL, the copyright holders of this library give you permission to link this library with independent modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions of the license of that module. An independent module is a module which is not derived from or based on this library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception statement from your version.*

This allows the use of the framework code in a wide variety of software projects, while still maintaining intellectual rights on library code.

In short:
- For GPL projects, use the GPL license - see http://www.gnu.org/licenses/gpl-2.0.html
- For LGPL projects, use the LGPL license - see http://www.gnu.org/licenses/lgpl-2.1.html
- For commercial projects, use the MPL License - see http://www.mozilla.org/MPL/MPL-1.1.html - which is the most permissive, or the FPC modified LGPL license, thanks to its linking exception - see http://wiki.freepascal.org/modified_LGPL

### 26.1.2. Publish Modifications and Credit for the Library

In all cases, any modification made to this source code **should** be published by any mean (e.g. a download link), even in case of MPL. If you need any additional feature, use the forums and we may introduce a patch to the main framework trunk.

You do not have to pay any fee for using our MPL/GPL/LGPL libraries.

But please do not forget to put somewhere in your credit window or documentation, a link to https://synopse.info if you use any of the units published under this tri-license.

For instance, if you select the MPL license, here are the requirements:
- You accept the license terms with no restriction - see http://www.mozilla.org/MPL/2.0/FAQ.html for additional information;
- You have to publish any modified unit in a public web site (e.g. `http://YourSoftwareCompany.com/MPL`), with a description of applied modifications, and no removal of the original license header in source code;
- You make appear some notice available in the program (About box, documentation, online help), stating e.g.

*This software uses some third-party code of the Synopse mORMot framework (C) 2026 Arnaud Bouchez - https://synopse.info - under Mozilla Public License 1.1; modified source code is available at http://SoftwareCompany.com/MPL*

Note that this documentation is under GPL 3.0 license only, as stated in this document front page.

### 26.1.3. Derivate Open Source Works

If you want to include part of the framework source code in your own open-source project, you may publish it with a comment similar to this one:

```pascal
{
  Sample based on official mORMot 2 sample

  Synopse mORMot 2 framework. Copyright (C) 2026 Arnaud Bouchez
    Synopse Informatique - https://synopse.info
  Original tri-license: MPL 1.1/GPL 2.0/LGPL 2.1
}
```

You need to ensure that your Open Source project licensing is compatible with our Licensing Terms, and, if possible, notify us that you use our code.

### 26.1.4. Legal Notice

There are countries that restrict the use, import, export of cryptographic software. Before keeping, using, or distributing the software, make sure that you comply with these restrictions. If (for any reason) you are unable to do so, you are not allowed to download, use, or distribute the software.

If you are residing in a country that allows software patents you must verify that no part of the software is covered by a patent in your country. If (for any reason) you are unable to do so, you are not allowed to use or distribute the software.

### 26.1.5. Commercial Licenses

Even though our libraries are Open Source with permissive licenses, some users want to obtain a license anyway. For instance, you may want to hold a tangible legal document as evidence that you have the legal right to use and distribute your software containing our library code, or, more likely, your legal department tells you that you have to purchase a license.

If you feel like you really have to purchase a license for our libraries, *Synopse*, the company that employs the architect and principal developer of the library, will sell you one. Please contact us directly for a contract proposal.

## 26.2. Availability

As a true *Open Source* project, all source code of the framework is available. The primary location for *mORMot 2* source code is:

**GitHub Repository**: https://github.com/synopse/mORMot2

The source has been commented following the scheme used by documentation tools. All interface definitions of the units have special comments which provide inline documentation.

### 26.2.1. Obtaining the Source Code

There are two primary methods to obtain the *mORMot 2* source code:

#### Method 1: Clone the Repository (Recommended)

Cloning the Git repository is the preferred method, as it allows you to easily update to the latest version:

```bash
git clone https://github.com/synopse/mORMot2.git
```

For example, clone into `c:\github\mORMot2` on Windows or `~/github/mORMot2` on Linux/macOS.

#### Method 2: Download a Release Archive

For a specific stable version (e.g., for use in a build script):
1. Go to https://github.com/synopse/mORMot2/releases
2. Download the *Source code (zip)* for your desired release
3. Extract it to your chosen location (e.g., `d:\mormot2`)

### 26.2.2. Static Libraries

After obtaining the source code, you need to download the static libraries for SQLite3 and other compiled C code:

**Download Options:**
- https://synopse.info/files/mormot2static.7z (Windows-friendly, 7-Zip compressed)
- https://synopse.info/files/mormot2static.tgz (POSIX-friendly, tar/gzip compressed)
- From the matching GitHub release page

**Important:** Extract the static files into the `static` sub-folder of your *mORMot 2* installation.

For example, if you cloned to `c:\github\mORMot2`, the static files should be in `c:\github\mORMot2\static\`.

For safety, the SHA-256 checksums of the current version of the downloaded binary files are available in the `static/dev.sha256` file.

### 26.2.3. Version Synchronization

**Important:** Always keep the static binaries in sync with the framework source code. Version mismatches can cause unexpected errors.

The static files are typically updated to match SQLite3 releases, sometimes with a short delay to ensure stability after major SQLite3 releases.

## 26.3. Repository Structure

The *mORMot 2* repository is organized into the following main folders:

| Folder | Description |
|--------|-------------|
| `src/` | Main source code folder containing the framework units |
| `packages/` | IDE packages and tools for development environment setup |
| `static/` | Pre-compiled binary `.o`/`.obj` files for static linking |
| `test/` | Regression tests for all framework features |
| `res/` | Resources used within `src/`, including static third-party binaries source |
| `doc/` | Framework documentation |
| `ex/` | Example projects and samples |

### 26.3.1. Source Code Organization (src/)

The `src/` folder contains the framework source code organized into logical layers:

| Sub-folder | Description | Key Units |
|------------|-------------|-----------|
| `core/` | Core utilities: RTTI, JSON, text, logging, threads | `mormot.core.base.pas`, `mormot.core.json.pas`, `mormot.core.log.pas` |
| `lib/` | Raw API definitions for external libraries | `mormot.lib.openssl11.pas`, `mormot.lib.curl.pas` |
| `crypt/` | Cryptographic primitives and secure protocols | `mormot.crypt.core.pas`, `mormot.crypt.jwt.pas`, `mormot.crypt.openssl.pas` |
| `net/` | Network layer: HTTP, WebSockets, async servers | `mormot.net.http.pas`, `mormot.net.server.pas`, `mormot.net.ws.pas` |
| `db/` | Database access: SynDB, SQLite3, SQL/NoSQL | `mormot.db.core.pas`, `mormot.db.sql.pas`, `mormot.db.nosql.mongodb.pas` |
| `orm/` | Object-Relational Mapping | `mormot.orm.core.pas`, `mormot.orm.sql.pas` |
| `rest/` | REST client and server | `mormot.rest.core.pas`, `mormot.rest.server.pas`, `mormot.rest.http.server.pas` |
| `soa/` | Service-Oriented Architecture (interfaces) | `mormot.soa.core.pas`, `mormot.soa.server.pas` |
| `app/` | Application layer: daemon, console helpers | `mormot.app.daemon.pas`, `mormot.app.console.pas` |
| `script/` | Scripting engine support (QuickJS) | `mormot.script.quickjs.pas` |
| `ui/` | User interface components (VCL/LCL) | `mormot.ui.controls.pas` |
| `ddd/` | Domain-Driven Design support | DDD infrastructure and patterns |
| `tools/` | Command-line tools source code | ECC tool, etc. |
| `misc/` | Miscellaneous utilities | Various helper units |

### 26.3.2. Key Include Files

The `src/` folder contains important include files:

| File | Description |
|------|-------------|
| `mormot.defines.inc` | Global compiler conditionals and settings |
| `mormot.uses.inc` | Common uses clause for console applications |
| `mormot.commit.inc` | Current commit hash (updated automatically) |

### 26.3.3. Static Libraries Structure

The `static/` folder contains pre-compiled binaries organized by target platform:

**For FPC (cross-platform):**
```
static/
├── i386-win32/          # Windows 32-bit
├── x86_64-win64/        # Windows 64-bit
├── i386-linux/          # Linux 32-bit
├── x86_64-linux/        # Linux 64-bit
├── aarch64-linux/       # Linux ARM64
├── arm-linux/           # Linux ARM32
├── i386-darwin/         # macOS 32-bit (legacy)
├── x86_64-darwin/       # macOS 64-bit Intel
├── aarch64-darwin/      # macOS ARM64 (Apple Silicon)
├── x86_64-freebsd/      # FreeBSD 64-bit
└── x86_64-openbsd/      # OpenBSD 64-bit
```

**For Delphi:**
```
static/
└── delphi/              # Win32 .obj and Win64 .o files
```

## 26.4. Expected Compilation Targets

### 26.4.1. Compiler Support

The framework source code:
- Tries to stay compatible with FPC stable and Delphi 7 and up
- Is currently validated against:
  - **FPC**: 3.2.3 (fixes-3_2 branch) and Lazarus 2.2.5 (fixes_2_2 branch)
  - **Delphi**: 7, 2007, 2009, 2010, XE4, XE7, XE8, 10.4, 11.1, 12.2 Athenes

**Note:** FPC 3.2.2 has a regression with variant late binding. Use the FPC 3.2.3 fixes branch instead.

### 26.4.2. Platform Support

**Server-Side (Full Framework):**

| Platform | FPC | Delphi |
|----------|-----|--------|
| Windows 32-bit | ✓ | ✓ |
| Windows 64-bit | ✓ | ✓ (XE2+) |
| Linux x86_64 | ✓ | - |
| Linux i386 | ✓ | - |
| Linux ARM64 | ✓ | - |
| Linux ARM32 | ✓ | - |
| macOS Intel | ✓ | - |
| macOS Apple Silicon | ✓ | - |
| FreeBSD | ✓ | - |
| OpenBSD | ✓ | - |

**Client-Side (Cross-Platform Units):**
- All Delphi targets (Windows, iOS, Android, macOS, Linux) can use the cross-platform client units
- FPC provides full server and client support on all listed platforms

### 26.4.3. Pure Pascal Fallbacks

The static `.o`/`.obj` files are **not mandatory** to compile the framework. There is always a "pure Pascal" fallback code available for:
- SQLite3 engine (use external dynamic library instead)
- Cryptographic routines
- Compression algorithms

However, the static-linked versions typically provide better performance and simpler deployment.

## 26.5. Delphi Installation

### 26.5.1. Step-by-Step Setup

Follow these steps to set up *mORMot 2* for Delphi:

**Step 1: Get the Source Code**

Clone the repository or download a release:
```bash
cd c:\github
git clone https://github.com/synopse/mORMot2.git
```

**Step 2: Download Static Libraries**

Download and extract `mormot2static.7z` from https://synopse.info/files/mormot2static.7z into `c:\github\mORMot2\static\`.

**Step 3: Create Environment Variable**

In Delphi IDE:
1. Go to *Tools* → *Options* → *IDE* → *Environment Variables*
2. Create a new **User System Override** variable:
   - Name: `mormot2`
   - Value: `c:\github\mORMot2\src` (or your chosen path)

**Step 4: Configure Library Paths**

In Delphi IDE:
1. Go to *Tools* → *Options* → *Language* → *Delphi Options* → *Library*
2. Add the following to the **Library path** for each target platform (Win32, Win64):

```
$(mormot2);$(mormot2)\core;$(mormot2)\lib;$(mormot2)\crypt;$(mormot2)\net;$(mormot2)\db;$(mormot2)\rest;$(mormot2)\orm;$(mormot2)\soa;$(mormot2)\app;$(mormot2)\script;$(mormot2)\ui;$(mormot2)\tools;$(mormot2)\misc
```

**Step 5: Verify Installation**

1. Open `test/mormot2tests.dpr` in the IDE
2. Compile and run the regression tests
3. All tests should pass on your machine

### 26.5.2. Quick Path Setup Reference

Here's a condensed version of the library path string to copy:

```
$(mormot2);$(mormot2)\core;$(mormot2)\lib;$(mormot2)\crypt;$(mormot2)\net;$(mormot2)\db;$(mormot2)\rest;$(mormot2)\orm;$(mormot2)\soa;$(mormot2)\app;$(mormot2)\script;$(mormot2)\ui;$(mormot2)\tools;$(mormot2)\misc
```

### 26.5.3. No Packages Required

Unlike some frameworks, *mORMot 2* does not require installing IDE packages for Delphi. Simply configure the library paths and you're ready to go.

The framework uses relative paths in its source code to include the expected `.o`/`.obj` files from the `static\delphi` sub-folder automatically.

### 26.5.4. Testing Your Installation

After setup, compile and run the test project:

1. Open `test/mormot2tests.dpr`
2. Select your target platform (Win32 or Win64)
3. Build and run
4. Review the test results

The test suite covers all framework features and serves as both validation and usage examples.

## 26.6. FreePascal / Lazarus Installation

### 26.6.1. Supported Targets

You can use the *FreePascal Compiler* (FPC) to (cross-)compile *mORMot 2* for the following platforms:

**Windows:**
- i386-win32
- x86_64-win64

**Linux:**
- i386-linux
- x86_64-linux
- arm-linux
- aarch64-linux

**macOS:**
- x86_64-darwin
- aarch64-darwin (Apple Silicon)

**BSD:**
- x86_64-freebsd
- x86_64-openbsd
- i386-freebsd

Linux is a premium target for efficient server hosting. Since *mORMot 2* has minimal dependencies, installing a new server is as simple as copying the executable to a blank Linux host. No runtime frameworks or virtual machines needed.

### 26.6.2. Using the Lazarus Package (Recommended)

The easiest way to set up *mORMot 2* for Lazarus is to use the provided package:

**Step 1: Get Source and Static Files**

```bash
git clone https://github.com/synopse/mORMot2.git
cd mORMot2
# Download and extract mormot2static.tgz into the static/ folder
```

**Step 2: Install the Package**

1. Open Lazarus IDE
2. Go to *Package* → *Open Package File (.lpk)*
3. Navigate to `packages/lazarus/mormot2.lpk`
4. Click *Compile* (not *Install* - it's a runtime package)

For UI components, also compile `packages/lazarus/mormot2ui.lpk`.

**Step 3: Configure Your Project**

Add `mormot2` package as a dependency to your Lazarus project:
1. Open your project
2. Go to *Project* → *Project Inspector*
3. Click *Add* → *New Requirement*
4. Select `mormot2`

### 26.6.3. Manual FPC Setup (Without Package)

For command-line compilation or when not using the package:

**Project Options:**

Add to *Other unit files (-Fu)*:
```
/path/to/mORMot2/src;/path/to/mORMot2/src/core;/path/to/mORMot2/src/lib;/path/to/mORMot2/src/crypt;/path/to/mORMot2/src/net;/path/to/mORMot2/src/db;/path/to/mORMot2/src/rest;/path/to/mORMot2/src/orm;/path/to/mORMot2/src/soa;/path/to/mORMot2/src/app
```

Add to *Include files (-Fi)*:
```
/path/to/mORMot2/src
```

Add to *Libraries (-Fl)*:
```
/path/to/mORMot2/static/$(TargetCPU)-$(TargetOS)
```

The `$(TargetCPU)-$(TargetOS)` macro automatically selects the correct static library folder based on your compilation target.

### 26.6.4. Setting Up FPC with fpcupdeluxe

We recommend using *fpcupdeluxe* to set up a stable FPC/Lazarus environment:

1. Download from https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases
2. Run the executable
3. Select FPC version `3.2` and Lazarus version matching your needs
4. Click "Install/update FPC+Laz"

**Cross-Compilation:**

*fpcupdeluxe* makes cross-compilation easy:
1. Go to the "Cross" tab
2. Select your target CPU and OS
3. Click "Install compiler"
4. Download cross-compiler binaries when prompted

This allows you to build Linux executables from Windows, or vice versa.

### 26.6.5. FPC Version Considerations

**Recommended:** FPC 3.2.3 (fixes-3_2 branch)

**Known Issues:**
- FPC 3.2.2 has a regression with variant late binding - avoid using it
- FPC trunk may have breaking changes; stick to stable branches for production

### 26.6.6. Minimal FPC Console Application

Here's a minimal FPC project structure:

```pascal
program MyMormotApp;

{$I mormot.defines.inc}
{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}  // Includes FPC-specific units for Linux
  mormot.core.base,
  mormot.core.text;

begin
  writeln('Hello from mORMot 2!');
  writeln('Current UTC: ', DateTimeToIso8601(NowUtc, true));
end.
```

The `mormot.uses.inc` file automatically includes the necessary units for FPC on various platforms (e.g., `cthreads`, `cwstring` on Linux).

## 26.7. Writing Cross-Platform Code

### 26.7.1. Common Include File

In all your source code files, include the *mORMot 2* defines file to set all compiler options and conditionals:

```pascal
{$I mormot.defines.inc}
```

This include file defines essential conditionals like `HASINLINE`, `OSWINDOWS`, `OSPOSIX`, CPU architecture flags, and more.

### 26.7.2. Cross-Platform Guidelines

To ensure your code compiles on both Delphi and FPC, and on multiple platforms:

1. **Avoid Direct Windows Unit Usage**: Don't directly reference the `Windows` unit in cross-platform code. Use `mormot.core.os` instead.

2. **Use Framework Types**: Rely on *mORMot 2* types like `RawUtf8` for string handling in business logic.

3. **Conditional Compilation**: When platform-specific code is needed:

```pascal
{$ifdef OSWINDOWS}
  // Windows-specific code
{$endif OSWINDOWS}

{$ifdef OSPOSIX}
  // Linux/macOS/BSD code
{$endif OSPOSIX}
```

4. **Use mormot.uses.inc**: In your `.dpr`/`.lpr` files:

```pascal
uses
  {$I mormot.uses.inc}  // Handles platform-specific units
  mormot.core.base,
  // ... your units
```

### 26.7.3. Conditional Defines Reference

Key conditionals defined in `mormot.defines.inc`:

| Conditional | Description |
|-------------|-------------|
| `OSWINDOWS` | Compiling for Windows |
| `OSPOSIX` | Compiling for POSIX (Linux, macOS, BSD) |
| `OSLINUX` | Compiling for Linux specifically |
| `OSDARWIN` | Compiling for macOS |
| `CPU32` | 32-bit architecture |
| `CPU64` | 64-bit architecture |
| `CPUINTEL` | x86 or x86_64 architecture |
| `CPUARM` | ARM architecture |
| `FPC` | FreePascal Compiler |
| `ISDELPHI` | Delphi compiler |
| `HASINLINE` | Inline functions supported |
| `PUREMORMOT2` | Use only mORMot 2 type names |

### 26.7.4. Recommended Project Structure

For cross-platform projects:

```
myproject/
├── src/
│   ├── myproject.core.pas      # Business logic (cross-platform)
│   └── myproject.server.pas    # Server components
├── bin/
│   ├── win32/                  # Windows 32-bit output
│   ├── win64/                  # Windows 64-bit output
│   └── linux64/                # Linux 64-bit output
├── test/
│   └── myproject.tests.dpr     # Regression tests
├── myproject.server.dpr        # Delphi project
├── myproject.server.lpr        # Lazarus project
└── myproject.server.lpi        # Lazarus project info
```

## 26.8. Server Deployment

### 26.8.1. Linux Deployment

Deploying a *mORMot 2* server on Linux is straightforward:

1. **Cross-compile** your application for `x86_64-linux` (or your target architecture)
2. **Copy** the executable to your Linux server
3. **Run** it directly - no runtime dependencies needed

The static linking of SQLite3 and other libraries means you don't need to install anything on the server.

**Minimal Dependencies:**

A typical *mORMot 2* Linux executable only requires:
```
libpthread.so.0   # Threading (standard)
libdl.so.2        # Dynamic loading (standard)
libc.so.6         # C library (standard)
```

These are present on every Linux distribution.

### 26.8.2. Docker Deployment

*mORMot 2* works well in Docker containers. A minimal Dockerfile:

```dockerfile
FROM scratch
COPY myserver /
ENTRYPOINT ["/myserver"]
```

Since *mORMot 2* executables are statically linked, you can even use a `scratch` (empty) base image.

### 26.8.3. Windows Services

For Windows service deployment, use `mormot.app.daemon` which provides cross-platform daemon/service functionality. The same code can run as:
- A Windows Service
- A Linux systemd daemon
- A console application (for debugging)

## 26.9. Upgrading from mORMot 1.18

### 26.9.1. Why a New Version?

*mORMot 2* is a complete rewrite of the framework, addressing:
- Better SOLID principles adherence
- Split of large monolithic units into focused components
- Cleaner type names (`TOrm` instead of `TSQLRecord`)
- Modern features (OpenSSL, async servers, QuickJS)
- Improved performance through optimized kernels

### 26.9.2. Migration Steps

1. **Create a New Folder**: Don't replace mORMot 1.18; install mORMot 2 in a separate location

2. **Update Unit Names**: All units have been renamed:
   - `SynCommons.pas` → `mormot.core.*.pas` (split into multiple units)
   - `mORMot.pas` → `mormot.orm.*.pas` + `mormot.rest.*.pas`
   - `SynDB*.pas` → `mormot.db.*.pas`
   - See Chapter 3 for complete unit mapping

3. **Update Type Names**: In `PUREMORMOT2` mode:
   - `TSQLRecord` → `TOrm`
   - `TSQLRest` → `TRest`
   - `TSQLModel` → `TOrmModel`

4. **Review Breaking Changes**:
   - Delphi 5-6 and Kylix support removed
   - BigTable, LVCL, RTTI-UI deprecated
   - Some internal APIs changed

5. **Consult Examples**: The `ex/` folder contains updated examples showing new patterns

### 26.9.3. Parallel Installation

You can maintain both mORMot 1.18 and mORMot 2 on the same system:
- Keep mORMot 1.18 for legacy projects
- Use mORMot 2 for new development
- Unit names don't conflict, allowing gradual migration

## 26.10. Getting Help

### 26.10.1. Resources

- **Official Documentation**: https://synopse.info/files/doc/mORMot2.html
- **GitHub Repository**: https://github.com/synopse/mORMot2
- **Forum**: https://synopse.info/forum/viewforum.php?id=24
- **Blog**: https://blog.synopse.info
- **Telegram Group**: https://t.me/synopse_mormot
- **Discord Server**: https://discord.gg/BcmcpY6afj

### 26.10.2. Sample Projects

The `ex/` folder contains many examples:
- Basic ORM usage
- HTTP client/server
- Interface-based services
- Domain-Driven Design patterns
- Web MVC applications

The *Thomas Tutorials* (`ex/ThirdPartyDemos/tbo/`) provide particularly good step-by-step learning resources.

### 26.10.3. Contributing

Contributions are welcome:
- Submit pull requests via GitHub
- Report issues in the GitHub issue tracker
- Share your experiences on the forum

Consider [sponsoring mORMot 2 development](https://github.com/synopse/mORMot2/blob/master/DONATE.md) if you find it valuable for your projects.

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 25: Testing and Logging](mORMot2-SAD-Chapter-25.md) | [Index](mORMot2-SAD-Index.md) | - |
