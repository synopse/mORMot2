# mORMot Core Units

## Folder Content

This folder hosts the Core Units of the *mORMot* Open Source framework, version 2.

## Core Units

With "Core Units", we mean units implementing shared basic functionality of our framework:

- Uncoupled reusable bricks to process files, text, JSON, compression, encryption, network, RTTI, potentially with optimized asm;
- Other higher level features, like ORM, SOA or database access are built on top of those bricks, and are located in the parent folder;
- Cross-Platform and Cross-Compiler: ensure the same code would compile on both FPC and Delphi, on any support platform, regardless the RTL, Operating System, or CPU.

## Units Presentation

### mormot.core.fpcx64mm

A Multi-thread Friendly Memory Manager for FPC written in x86_64 assembly
- targetting Linux (and Windows) multi-threaded Services
- only for FPC on the x86_64 target - use the RTL MM on Delphi or ARM
- based on FastMM4 proven algorithms by Pierre le Riche
- code has been reduced to the only necessary featureset for production
- deep asm refactoring for cross-platform, compactness and efficiency
- can report detailed statistics (with threads contention and memory leaks)
- mremap() makes large block ReallocMem a breeze on Linux :)
- inlined SSE2 movaps loop is more efficient that subfunction(s)
- lockless round-robin of tiny blocks (<=128/256 bytes) for better scaling
- optional lockless bin list to avoid freemem() thread contention
- three app modes: default mono-thread friendly, `FPCMM_SERVER` or `FPCMM_BOOST`

### mormot.core.base

Basic types and reusable stand-alone functions shared by all framework units
- Framework Version and Information
- Common Types Used for Compatibility Between Compilers and CPU
- Numbers (floats and integers) Low-level Definitions
- integer Arrays Manipulation
- `ObjArray` `PtrArray` `InterfaceArray` Wrapper Functions
- Low-level Types Mapping Binary or Bits Structures
- Buffers (e.g. Hashing and SynLZ compression) Raw Functions
- Date / Time Processing
- Efficient `Variant` Values Conversion
- Sorting/Comparison Functions
- Some Convenient `TStream` descendants and File access functions
- Faster Alternative to RTL Standard Functions
- Raw Shared Constants / Types Definitions

Aim of those types and functions is to be cross-platform and cross-compiler, without any dependency but the main FPC/Delphi RTL. It also detects the kind of Intel/AMD it runs on, to adapt to the fastest asm version available. It is the main unit where x86_64 or i386 asm stubs are included.

### mormot.core.os

Cross-platform functions shared by all framework units
- Gather Operating System Information
- Operating System Specific Types (e.g. `TWinRegistry`)
- Unicode, Time, File, Console, Library process
- Per Class Properties O(1) Lookup via `vmtAutoTable` Slot (e.g. for RTTI cache)
- `TSynLocker`/`TSynLocked` and Low-Level Threading Features
- Unix Daemon and Windows Service Support

Aim of this unit is to centralize most used OS-specific API calls, like a `SysUtils` unit on steroids, to avoid `$ifdef/$endif` in "uses" clauses.

In practice, no "Windows", nor "Linux/Unix" reference should be needed inregular units, once mormot.core.os is included. :)

### mormot.core.unicode

Efficient Unicode Conversion Classes shared by all framework units
- UTF-8 Efficient Encoding / Decoding
- UTF-8 / UTF-16 / Ansi Conversion Classes
- Low-Level String Conversion Functions
- Text Case-(in)sensitive Conversion and Comparison

### mormot.core.text

Text Processing functions shared by all framework units
- UTF-8 String Manipulation Functions
- `TRawUTF8DynArray` Processing Functions
- CSV-like Iterations over Text Buffers
- `TBaseWriter` parent class for Text Generation
- Numbers (integers or floats) and Variants to Text Conversion
- Hexadecimal Text And Binary Conversion
- Text Formatting functions and `ESynException` class
- Resource and Time Functions

### mormot.core.datetime

Date and Time definitions and process shared by all framework units
- *ISO-8601* Compatible Date/Time Text Encoding
- `TSynDate` / `TSynDateTime` / `TSynSystemTime` High-Level objects
- `TUnixTime` / `TUnixMSTime` POSIX Epoch Compatible 64-bit date/time
- `TTimeLog` efficient 64-bit custom date/time encoding

### mormot.core.rtti

Cross-Compiler RTTI Definitions shared by all framework units
- Low-Level Cross-Compiler RTTI Definitions
- Enumerations RTTI
- Published `class` Properties and Methods RTTI
- `IInvokable` Interface RTTI
- Efficient Dynamic Arrays and Records Process
- Managed Types Finalization or Copy
- RTTI Value Types used for JSON Parsing
- RTTI-based Registration for Custom JSON Parsing
- Redirect Most Used FPC RTL Functions to Optimized x86_64 Assembly

Purpose of this unit is to avoid any direct use of `TypInfo.pas` RTL unit, which is not exactly compatible between compilers, and lacks of direct RTTI access with no memory allocation. We define pointers to RTTI record/object to access `TypeInfo()` via a set of explicit methods. Here fake record/objects are just wrappers around pointers defined in Delphi/FPC RTL's `TypInfo.pas` with the magic of inlining. We redefined all RTTI definitions as `TRtti*` types to avoid confusion with type names as published by the `TypInfo` unit.

At higher level, the new `TRttiCustom` class is the main cached entry of our customizable RTTI,accessible from the global `Rtti.*` methods. It is enhanced in the `mormot.core.json` unit to support JSON.

### mormot.core.buffers

Low-Level Memory Buffers Processing Functions shared by all framework units
- Variable Length integer Encoding / Decoding
- `TAlgoCompress` Compression/Decompression Classes - with `AlgoSynLZ`
- `TFastReader` / `TBufferWriter` Binary Streams
- Base64, Base64URI and Baudot Encoding / Decoding
- URI-Encoded Text Buffer Process
- Basic MIME Content Types Support
- Text Memory Buffers and Files
- Markup (e.g. HTML or Emoji) process

### mormot.core.data

Low-Level Data Processing Functions shared by all framework units
- RTL `TPersistent` / `TInterfacedObject` with Custom Constructor
- `TSynPersistent*` / `TSyn*List` classes
- `TSynPersistentStore` with proper Binary Serialization
- INI Files and In-memory Access
- Efficient RTTI Values Binary Serialization and Comparison
- `TDynArray`, `TDynArrayHashed` and `TSynQueue` Wrappers
- `RawUTF8` String Values Interning and `TRawUTF8List`

### mormot.core.json

JSON functions shared by all framework units
- Low-Level JSON Processing Functions
- `TTextWriter` class with proper JSON escaping and `WriteObject()` support
- JSON-aware `TSynNameValue` `TSynPersistentStoreJson` `&TRawByteStringGroup`
- JSON-aware `TSynDictionary` Storage
- JSON Unserialization for any kind of Values
- JSON Serialization Wrapper Functions
- Abstract Classes with Auto-Create-Fields

### mormot.core.variants

`Variant` / `TDocVariant` feature shared by all framework units
- Low-Level `Variant` Wrappers
- Custom `Variant` Types with JSON support
- `TDocVariant` Object/Array Document Holder with JSON support
- JSON Parsing into `Variant`

### mormot.core.search

Several Indexing and Search Engines, as used by other parts of the framework
- Files Search in Folders
- GLOB and SOUNDEX Text Search
- Versatile Expression Search Engine
- *Bloom Filter* Probabilistic Index
- `TDynArray` Low-Level Binary Search
- `TSynFilter` and `TSynValidate` Processing Classes
- Cross-Platform `TSynTimeZone` Time Zones

### mormot.core.log

Logging functions shared by all framework units
- Executable Symbols Processing
- Logging via `TSynLogFamily` `TSynLog` `ISynLog`
- High-Level Logs and Exception Related Features
- Efficient `.log` File Access via `TSynLogFile`

### mormot.core.perf

Performance Monitoring functions shared by all framework units
- Performance Counters
- `TSynMonitor` Process Information Classes
- `TSynMonitorUsage` Process Information Database Storage
- Operating System Monitoring
- `TSynFPUException` Wrapper for FPU Flags Preservation

### mormot.core.threads

High-Level Multi-Threading features shared by all framework units
- Thread-Safe Pending Tasks List
- Background Thread Processing
- Parallel Execution in a Thread Pool
- Server Process Oriented Thread Pool

### mormot.core.crypto

High-Performance Cryptographic features shared by all framework units
- Low-Level Memory Buffers Helper Functions
- AES Encoding/Decoding with optimized asm and AES-NI support
- AES-256 Cryptographic Pseudorandom Number Generator (CSPRNG)
- SHA-2 SHA-3 Secure Hashing
- HMAC Authentication over SHA and CRC32C
- PBKDF2 Key Derivation over SHA and CRC32C
- Digest/Hash to Hexadecimal Text Conversion
- Deprecated MD5 RC4 SHA-1 Algorithms
- Deprecated Weak AES/SHA Process

Optimized x86_64 or i386 asm stubs, featuring e.g. AES-NI, are included.

### mormot.core.ecc256r1

High-Performance *secp256r1/NISTP-256/prime256v1* Elliptic-Curve Cryptography
- Low-Level ECC *secp256r1* ECDSA and ECDH Functions
- Middle-Level Certificate-based Public Key Cryptography

Pascal and optimized gcc static binaries are included.

### mormot.core.ecc

Certificate-based Public Key Cryptography Classes
- High-Level Certificate-based Public Key Cryptography
- `IProtocol` Implemented using Public Key Cryptography

### mormot.core.jwt

JSON Web Tokens (JWT) Implementation - see RFC 7797
- Abstract JWT Parsing and Computation
- JWT Implementation of `HS*` and `S3*` Symmetric Algorithms
- JWT Implementation of `ES256` Asymmetric Algorithm

### mormot.core.secure

Authentication and Security types shared by all framework units.
- `TSyn*Password` and `TSynConnectionDefinition` Classes
- Reusable Authentication Classes
- High-Level `TSynSigner`/`TSynHasher` Multi-Algorithm Wrappers
- 64-bit `TSynUniqueIdentifier` and its efficient Generator
- `IProtocol` Safe Communication with Unilateral or Mutual Authentication

### mormot.core.zip

High-Level Zip/Deflate Compression features shared by all framework units
- `TSynZipCompressor` Stream Class
- GZ Read/Write Support
- `.zip` Archive File Support

### mormot.core.mustache

Logic-Less `{{Mustache}}` Templates Rendering
- *Mustache* Execution Data Context Types
- `TSynMustache` Template Processing

### mormot.core.interfaces

Implements SOLID Process via Interface types
- `IInvokable` Interface Methods and Parameters RTTI Extraction
- `TInterfaceFactory` Generating Runtime Implementation Class
- `TInterfaceResolver` `TInjectableObject` for IoC / Dependency Injection
- `TInterfaceStub` `TInterfaceMock` for Dependency Mocking
- `TInterfaceMethodExecute` for Method Execution from JSON

### mormot.core.test

Testing functions shared by all framework units
- Unit-Testing classes and functions

