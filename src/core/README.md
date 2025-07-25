# mORMot Core Units

## Folder Content

This folder hosts the Core Units of the *mORMot* Open Source framework, version 2.

## Core Units

With "Core Units", we mean units implementing shared basic functionality of our framework:

- Uncoupled reusable bricks to process files, text, JSON, compression, encryption, network, RTTI, potentially with optimized asm;
- Other higher level features, like ORM, SOA or database access are built on top of those bricks, and are located in the parent folder;
- Cross-Platform and Cross-Compiler: ensure the same code would compile on both FPC and Delphi, on any support platform, regardless the RTL, Operating System, or CPU.

## Units Presentation

### mormot.core.base

Basic types and reusable stand-alone functions shared by all framework units
- Framework Version and Information
- Common Types Used for Compatibility Between Compilers and CPU
- Numbers (floats and integers) Low-level Definitions
- Integer Arrays Manipulation
- `ObjArray` `PtrArray` `InterfaceArray` Wrapper Functions
- Low-level Types Mapping Binary or Bits Structures
- Buffers (e.g. Hashing and SynLZ compression) Raw Functions
- Efficient `Variant` Values Conversion
- Sorting/Comparison Functions
- Some Convenient `TStream` descendants and File access functions
- Faster Alternative to RTL Standard Functions
- Raw Shared Types Definitions

Aim of those types and functions is to be cross-platform and cross-compiler, without any dependency but the main FPC/Delphi RTL. It also detects the kind of Intel/AMD it runs on, to adapt to the fastest asm version available. It is the main unit where x86_64 or i386 asm stubs are included.

### mormot.core.os

Cross-platform functions shared by all framework units
- Some Cross-System Type and Constant Definitions
- Gather Operating System Information
- Operating System Specific Types (e.g. `TWinRegistry`)
- Unicode, Time, File, Console, Library process
- Per Class Properties O(1) Lookup via `vmtAutoTable` Slot (e.g. for RTTI cache)
- `TSynLocker`/`TSynLocked` and Low-Level Threading Features
- Unix Daemon and Windows Service Support

Aim of this unit is to centralize most used OS-specific API calls, like a `SysUtils` unit on steroids, to avoid `$ifdef/$endif` in "uses" clauses.

In practice, no "Windows", nor "Linux/Unix" reference should be needed in regular units, once `mormot.core.os` is included. :)

See `mormot.core.os.mac.pas` and `mormot.core.os.security.pas` units for completion.

### mormot.core.os.mac

MacOS API calls for FPC, as injected to `mormot.core.os.pas`
- Gather MacOS Specific Operating System Information

This unit uses MacOSAll and link several toolkits, so was not included in `mormot.core.os.pas` to reduce executable size, but inject this methods at runtime: just include "`uses mormot.core.os.mac`" in programs needing it.

### mormot.core.os.security

Cross-Platform Operating System Security Definitions
- Security IDentifier (SID) Definitions
- Security Descriptor Self-Relative Binary Structures
- Access Control List (DACL/SACL) Definitions
- Conditional ACE Expressions SDDL and Binary Support
- Active Directory Definitions
- Security Descriptor Definition Language (SDDL)
- `TSecurityDescriptor` Wrapper Object
- Kerberos KeyTab File Support
- Basic ASN.1 Support
- Windows API Specific Security Types and Functions

Even if most of those security definitions comes from the Windows/AD world, our framework (re)implemented them in a cross-platform way.
Implementation follows and refers to the official `[MS-DTYP]` Windows Open Specifications document.
This low-level unit only refers to `mormot.core.base.pas` and `mormot.core.os.pas`.

### mormot.core.unicode

Efficient Unicode Conversion Classes shared by all framework units
- UTF-8 Efficient Encoding / Decoding
- Cross-Platform Charset and CodePage Support
- UTF-8 / UTF-16 / Ansi Conversion Classes
- Text File Loading with BOM/Unicode Support
- Low-Level String Conversion Functions
- Text Case-(in)sensitive Conversion and Comparison
- UTF-8 String Manipulation Functions
- `TRawUtf8DynArray` Processing Functions
- Operating-System Independent Unicode Process

### mormot.core.text

Text Processing functions shared by all framework units
- CSV-like Iterations over Text Buffers
- `TTextWriter` parent class for Text Generation
- Numbers (integers or floats) and Variants to Text Conversion
- Text Formatting functions
- Resource and Time Functions
- HTTP/REST Common Headers Parsing (e.g. cookies)
- `ESynException` class
- Hexadecimal Text And Binary Conversion

### mormot.core.datetime

Date and Time definitions and process shared by all framework units
- ISO-8601 Compatible Date/Time Text Encoding
- `TSynDate` / `TSynDateTime` / `TSynSystemTime` High-Level objects
- `TUnixTime` / `TUnixMSTime` POSIX Epoch Compatible 64-bit date/time
- `TTimeLog` efficient 64-bit custom date/time encoding
- `TTextDateWriter` supporting date/time ISO-8601 serialization
- `TValuePUtf8Char` text value wrapper record

### mormot.core.rtti

Cross-Compiler RTTI Definitions shared by all framework units
- Low-Level Cross-Compiler RTTI Definitions
- Enumerations RTTI
- Published `class` Properties and Methods RTTI
- `IInvokable` Interface RTTI
- Efficient Dynamic Arrays and Records Process
- Managed Types Finalization, Random or Copy
- RTTI Value Types used for JSON Parsing
- RTTI-based Registration for Custom JSON Parsing
- `TRttiMap` Field Mapping (e.g. DTO/Domain Objects)
- `TObjectWithRttiMethods` `TObjectWithID` `TClonable` Classes
- Redirect Most Used FPC RTL Functions to Optimized x86_64 Assembly

Purpose of this unit is to avoid any direct use of `TypInfo.pas` RTL unit, which is not exactly compatible between compilers, and lacks of direct RTTI access with no memory allocation. We define pointers to RTTI record/object to access `TypeInfo()` via a set of explicit methods. Here fake record/objects are just wrappers around pointers defined in Delphi/FPC RTL's `TypInfo.pas` with the magic of inlining. We redefined all RTTI definitions as `TRtti*` types to avoid confusion with type names as published by the `TypInfo` unit.

At higher level, the new `TRttiCustom` class is the main cached entry of our customizable RTTI,accessible from the global `Rtti.*` methods. It is enhanced as `TRttiJson` in the `mormot.core.json` unit to support JSON.

### mormot.core.buffers

Low-Level Memory Buffers Processing Functions shared by all framework units
- *Variable Length Integer* Encoding / Decoding
- `TAlgoCompress` Compression/Decompression Classes - with `AlgoSynLZ` `AlgoRleLZ`
- `TFastReader` / `TBufferWriter` Binary Streams
- Base64, Base64URI, Base58 and Baudot Encoding / Decoding
- URI-Encoded Text Buffer Process
- Basic MIME Content Types Support
- Text Memory Buffers and Files
- `TStreamRedirect` and other Hash process
- Markup (e.g. HTML or Emoji) process
- `RawByteString` Buffers Aggregation via `TRawByteStringGroup`

### mormot.core.data

Low-Level Data Processing Functions shared by all framework units
- RTL `TPersistent` or Root Classes with Custom Constructor
- `IAutoFree` and `IAutoLocker` Reference-Counted Process
- `TSynList` `TSynObjectList` `TSynLocker` classes
- `TObjectStore` with proper Binary Serialization
- INI Files and In-memory Access
- Efficient RTTI Values Binary Serialization and Comparison
- `TDynArray` and `TDynArrayHashed` Wrappers
- `Integer` Arrays Extended Process
- `RawUtf8` String Values Interning and `TRawUtf8List`
- Abstract Radix Tree Classes

### mormot.core.json

JSON functions shared by all framework units
- Low-Level JSON Processing Functions
- `TTextWriter` class with proper JSON escaping and `WriteObject()` support
- JSON-aware `TSynNameValue` `TObjectStoreJson`
- JSON-aware `TSynDictionary` Storage
- JSON Unserialization for any kind of Values
- JSON Serialization Wrapper Functions
- Abstract Classes with Auto-Create-Fields

### mormot.core.collections

 Generics Collections as used by all framework units
 - JSON-aware `IList<>` List Storage
 - JSON-aware `IKeyValue<>` Dictionary Storage
 - Collections Factory for `IList<>` and `IKeyValue<>` Instances

In respect to `generics.collections` from the Delphi or FPC RTL, this unit uses `interface` as variable holders, and leverage them to reduce the generated code as much as possible, as the *Spring4D 2.0 framework* does, but for both Delphi and FPC. As a result, compiled units (`.dcu`/`.ppu`) and executable are much smaller, and faster to compile.

Those `interfaces` publish `TDynArray` and `TSynDictionary` high-level features like indexing, sorting, JSON/binary serialization or thread safety as Generics strong typing.

Use `Collections.NewList<T>` and `Collections.NewKeyValue<TKey, TValue>` factories as main entry points of these efficient data structures.
   
### mormot.core.variants

`Variant` / `TDocVariant` feature shared by all framework units
- Low-Level `Variant` Wrappers
- Custom `Variant` Types with JSON support
- `TDocVariant` Object/Array Document Holder with JSON support
- `IDocList`/`IDocDict` advanced Wrappers of `TDocVariant` Documents
- JSON Parsing into `Variant`
- `Variant` Binary Serialization

### mormot.core.search

Several Indexing and Search Engines, as used by other parts of the framework
- Files Search in Folders
- ScanUtf8, GLOB and SOUNDEX Text Search
- Efficient CSV Parsing using RTTI
- Versatile Expression Search Engine
- *Bloom Filter* Probabilistic Index
- Binary Buffers Delta Compression
- `TDynArray` Low-Level Binary Search
- `TSynFilter` and `TSynValidate` Processing Classes
- Cross-Platform `TSynTimeZone` Time Zones

### mormot.core.log

Logging functions shared by all framework units
- Debug Symbols Processing from Delphi .map or FPC/GDB DWARF
- Logging via `TSynLogFamily` `TSynLog` `ISynLog`
- High-Level Logs and Exception Related Features
- Efficient `.log` File Access via `TSynLogFile`
- SysLog Messages Support as defined by RFC 5424

### mormot.core.perf

Performance Monitoring functions shared by all framework units
- Performance Counters
- `TSynMonitor` Process Information Classes
- `TSynMonitorUsage` Process Information Database Storage
- Operating System Monitoring
- `DMI`/`SMBIOS` Binary Decoder
- `TSynFPUException` Wrapper for FPU Flags Preservation

### mormot.core.threads

High-Level Multi-Threading features shared by all framework units
- Thread-Safe `TSynQueue` and `TPendingTaskList`
- Thread-Safe `ILockedDocVariant` Storage
- Background Thread Processing
- Parallel Execution in a Thread Pool
- Server Process Oriented Thread Pool

### mormot.core.zip

High-Level Zip/Deflate Compression features shared by all framework units
- `TSynZipCompressor` Stream Class
- GZ Read/Write Support
- `.zip` Archive File Support
- `TAlgoDeflate` and `TAlgoGZ` High-Level Compression Algorithms

### mormot.core.mustache

Logic-Less `{{Mustache}}` Templates Rendering
- *Mustache* Execution Data Context Types
- `TSynMustache` Template Processing

### mormot.core.interfaces

Implements SOLID Process via Interface types
- `IInvokable` Interface Methods and Parameters RTTI Extraction
- `TInterfaceFactory` Generating Runtime Implementation Class
- `TInterfaceResolver` `TInjectableObject` for IoC / Dependency Injection
- `TInterfaceStub` for Dependency Stubbing/Mocking
- `TInterfacedObjectFake` with JITted Methods Execution
- `TInterfaceMethodExecute` for Method Execution from JSON
- `SetWeak` and `SetWeakZero` Weak Interface Reference Functions

### mormot.core.test

Testing functions shared by all framework units
- Unit-Testing classes and functions
- `TInterfaceMock` for Dependency Mocking

### mormot.core.fpcx64mm

An (optional) Multi-thread Friendly Memory Manager for FPC written in x86_64 assembly
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

