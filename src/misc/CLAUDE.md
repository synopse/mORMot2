# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Purpose

The `mORMot2/src/misc` folder contains specialized binary file format parsers that are too specific for the core framework but not tied to ORM/SOA/MVC features. These are standalone utilities for reading and analyzing file structures.

## Units Overview

### mormot.misc.pecoff (Production Ready)

**Purpose**: Cross-platform PE (Portable Executable) and COFF file format parser for Windows executables and libraries (.exe, .dll).

**Key Features**:
- Parse PE32 (32-bit) and PE32+ (64-bit) executables
- Extract version information from resources
- Read/parse all PE sections (.text, .data, .reloc, .edata, etc.)
- Digital signature stuffing (hide arbitrary data in executable signatures)
- Cross-platform (works on Linux/macOS to analyze Windows executables)

**Main Classes**:
```pascal
TSynPELoader = class
  // High-level PE file parser
  function LoadFromFile(const Filename: TFileName): boolean;
  function ParseResources: boolean;
  function ParseStringFileInfoEntries: boolean;

  // Navigation
  function GetSectionByName(const AName: RawUtf8): PImageSectionHeader;
  function GetSectionByRVA(RVA: cardinal): PImageSectionHeader;
  function OffsetFrom(RVA: cardinal): cardinal;

  // Properties
  property Architecture: TCoffArch;      // caI386, caAmd64, caArm64, etc.
  property IsPE64: boolean;
  property CoffHeader: PImageFileHeader;
  property PE32/PE64: PImageNtHeaders32/64;
  property StringFileInfoEntries: TDocVariantData;
  function FileVersionStr: RawUtf8;      // '[major].[minor].[patch].[build]'
end;
```

**Key Functions**:
```pascal
// Extract version info as TDocVariant document
function GetPEFileVersion(const aFileName: TFileName): TDocVariantData;

// Hide arbitrary text in executable digital signature
procedure StuffExeCertificate(const MainFile, NewFile: TFileName;
  const Stuff: RawUtf8; UseInternalCertificate: boolean = false);

// Retrieve stuffed text from signature
function FindStuffExeCertificate(const FileName: TFileName): RawUtf8;
```

**Usage Example**:
```pascal
uses mormot.misc.pecoff;

var
  pe: TSynPELoader;
  info: TDocVariantData;
begin
  pe := TSynPELoader.Create;
  try
    if pe.LoadFromFile('myapp.exe') then
    begin
      pe.ParseStringFileInfoEntries;
      WriteLn('Architecture: ', pe.ArchitectureName);
      WriteLn('Version: ', pe.FileVersionStr);
      WriteLn('64-bit: ', pe.IsPE64);

      // Get all version info as variant document
      info := pe.StringFileInfoEntries;
      WriteLn('Company: ', info.S['CompanyName']);
      WriteLn('Product: ', info.S['ProductName']);
    end;
  finally
    pe.Free;
  end;
end;
```

**Low-Level Structures**: Complete PE/COFF format definitions with all headers, sections, and resource structures following Microsoft PE format specification.

**Common Use Cases**:
- Extract version info from executables programmatically
- Analyze PE structure (sections, imports, exports, resources)
- Validate executable signatures
- Hide metadata/licensing info in signatures without breaking code signing
- Cross-platform analysis of Windows executables

---

### mormot.misc.iso (Early Draft - NOT FUNCTIONAL)

**Purpose**: ISO 9660 CD/DVD file system reader (optical disc media format).

**Status**: This unit is just in early draft state - nothing works yet.

**Structure Definitions**:
- Low-level ISO 9660 encoding structures (volume descriptors, directory records, path tables)
- Low-level UDF (Universal Disk Format) encoding structures
- Placeholder types for high-level .iso file reader

**Note**: Do NOT use this unit in production. It only contains type definitions without implementation.

---

## Architecture Notes

### Design Philosophy

Both units follow mORMot's standard pattern:
1. **Low-level structures** - Packed records matching binary format specs
2. **High-level reader classes** - Memory-mapped file access with lazy parsing
3. **Zero-copy approach** - Return pointers to mapped memory instead of copying data

### Memory Management

- Files are memory-mapped via `TMemoryMap` for efficiency
- Pointers returned are valid only while the loader object is alive
- No automatic memory copies - caller must copy if persistence is needed

### Cross-Platform

These parsers work on any platform (Windows/Linux/macOS/FreeBSD) since they only read binary file formats without OS-specific APIs.

---

## Dependencies

Minimal dependencies from mORMot core:
- `mormot.core.base` - Basic types, UTF-8, memory mapping
- `mormot.core.os` - File system access
- `mormot.core.unicode` - String conversions (pecoff only)
- `mormot.core.text` - Text utilities (pecoff only)
- `mormot.core.rtti` - Runtime type info (pecoff only)
- `mormot.core.variants` - TDocVariant (pecoff only)

---

## Testing

The PE COFF parser is production-tested. To verify:
```pascal
// Test with your own executables
var
  info: TDocVariantData;
begin
  info := GetPEFileVersion('C:\Windows\System32\notepad.exe');
  WriteLn(info.ToJSON('', '', jsonHumanReadable));
end;
```

---

## References

**PE/COFF Format**:
- https://learn.microsoft.com/en-us/windows/win32/debug/pe-format
- https://0xrick.github.io/win-internals/pe2

**ISO 9660**:
- ECMA-119 specification (ISO 9660 standard)

---

## Common Pitfalls

### PE Parser

1. **Lifetime management**: Pointers are only valid while `TSynPELoader` is alive
   ```pascal
   // WRONG - pointer becomes invalid
   function GetVersion: RawUtf8;
   var pe: TSynPELoader;
   begin
     pe := TSynPELoader.Create;
     pe.LoadFromFile('app.exe');
     Result := pe.FileVersionStr; // OK - returns copy
     pe.Free; // Now pe.StringFileInfo is invalid!
   end;
   ```

2. **Architecture detection**: Always check `Architecture` property, not just `IsPE64`
   ```pascal
   case pe.Architecture of
     caI386:   WriteLn('32-bit x86');
     caAmd64:  WriteLn('64-bit x64');
     caArm64:  WriteLn('64-bit ARM');
   end;
   ```

3. **Resource parsing**: Must call `ParseResources` before accessing version info
   ```pascal
   pe.LoadFromFile('app.exe');
   pe.ParseResources; // Required!
   WriteLn(pe.FileVersionStr);
   ```

4. **Digital signature stuffing**: Only works with executables that have valid signatures
   - Use `UseInternalCertificate := true` if OpenSSL not available
   - Stuffed data is hidden in signature, doesn't affect code execution
   - Maximum size depends on certificate structure (typically several KB)

---

## When to Use These Units

**Use `mormot.misc.pecoff` when**:
- Extracting version/metadata from executables at runtime
- Building deployment tools that analyze binaries
- Implementing software inventory/cataloging
- Adding licensing/metadata without modifying code sections
- Cross-platform analysis of Windows executables

**Do NOT use `mormot.misc.iso`** - it's incomplete and non-functional

---

Last updated: 2025-10-10
