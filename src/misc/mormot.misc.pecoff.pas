/// Microsoft PE COFF File Reader
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.misc.pecoff;

{
  *****************************************************************************

   Cross-Platform Portable Executable (PE) and COFF File Reader
    - Low-Level DOS/PE/COFF Encoding Structures
    - High-Level PE (.exe, .dll...) File Reader

  *****************************************************************************
}

{$I ..\mormot.defines.inc}

interface

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.variants;


{ ************ Low-Level DOS/PE/COFF Encoding Structures }

type
  /// exception raised by this unit during parsing
  EPeCoffLoader = class(ESynException);

const
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-data-directories-image-only

  /// index of Export Directory
  IMAGE_DIRECTORY_ENTRY_EXPORT    = 0;
  /// index of Import Directory
  IMAGE_DIRECTORY_ENTRY_IMPORT    = 1;
  /// index of Resource Directory
  IMAGE_DIRECTORY_ENTRY_RESOURCE  = 2;
  /// index of Exception Directory
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3;
  /// index of Security Directory
  IMAGE_DIRECTORY_ENTRY_SECURITY  = 4;
  /// index of Base Relocation Table
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5;
  /// index of Debug Directory
  IMAGE_DIRECTORY_ENTRY_DEBUG     = 6;
  /// index of X86 usage
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT      = 7;
  /// index of Architecture Specific Data
  IMAGE_DIRECTORY_ENTRY_ARCHITECTURE   = 7;
  /// index of RVA of GP
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR      = 8;
  /// index of TLS Directory
  IMAGE_DIRECTORY_ENTRY_TLS            = 9;
  /// index of Load Configuration Directory
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    = 10;
  /// index of Bound Import Directory in headers
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   = 11;
   /// index of Import Address Table
  IMAGE_DIRECTORY_ENTRY_IAT            = 12;
  /// index of Delay Load Import Descriptors
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   = 13;
  /// index of COM Runtime descriptor
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14;

  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

  /// Predefined Resource Types
  // - see https://learn.microsoft.com/en-us/windows/win32/menurc/resource-types
  RT_CURSOR       = 1;
  RT_BITMAP       = 2;
  RT_ICON         = 3;
  RT_MENU         = 4;
  RT_DIALOG       = 5;
  RT_STRING       = 6;
  RT_FONTDIR      = 7;
  RT_FONT         = 8;
  RT_ACCELERATOR  = 9;
  RT_RCDATA       = 10;
  RT_MESSAGETABLE = 11;
  RT_GROUP_CURSOR = 12;
  RT_GROUP_ICON   = 14;
  RT_VERSION      = 16;
  RT_DLGINCLUDE   = 17;
  RT_PLUGPLAY     = 19;
  RT_VXD          = 20;
  RT_ANICURSOR    = 21;
  RT_ANIICON      = 22;
  RT_HTML         = 23;
  RT_MANIFEST     = 24;

  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#signature-image-only

  /// DOS Magic number
  DOS_HEADER_MAGIC = $5A4D;
  /// PE Magic number
  PE_HEADER_MAGIC = $4550;

  PE_32_MAGIC = $10b;
  PE_64_MAGIC = $20b;

  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-image-only


type
  {$A-} // every record (or object) is packed from now on

  /// DOS HEADER
  // - see https://referencesource.microsoft.com/#System.Deployment/System/Deployment/Application/PEStream.cs,74a6abbcc7f5a6da
  _IMAGE_DOS_HEADER = record
    e_magic: word;
    e_cblp: word;
    e_cp: word;
    e_crlc: word;
    e_cparhdr: word;
    e_minalloc: word;
    e_maxalloc: word;
    e_ss: word;
    e_sp: word;
    e_csum: word;
    e_ip: word;
    e_cs: word;
    e_lfarlc: word;
    e_ovno: word;
    e_res: array[0..3] of word;
    e_oemid: word;
    e_oeminfo: word;
    e_res2: array[0..9] of word;
    case boolean of
      true:
        (e_lfanew: integer);
      false:
        (_lfanew: integer); // delphi naming
  end;
  TImageDOSHeader = _IMAGE_DOS_HEADER;
  PImageDOSHeader = ^_IMAGE_DOS_HEADER;

  /// COFF Header
  // - see https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#coff-file-header-object-and-image
  _IMAGE_FILE_HEADER = record
    Signature: cardinal; // 0x50450000 ('P', 'E', 0, 0) (Not in fpc)
    Machine: word;
    NumberOfSections: word;
    TimeDateStamp: cardinal;
    PointerToSymbolTable: cardinal;
    NumberOfSymbols: cardinal;
    SizeOfOptionalHeader: word;
    Characteristics: word;
  end;
  TImageFileHeader = _IMAGE_FILE_HEADER;
  PImageFileHeader = ^_IMAGE_FILE_HEADER;

  /// Data directory.
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-data-directories-image-only
  _IMAGE_DATA_DIRECTORY = record
    VirtualAddress: cardinal;
    Size: cardinal;
  end;
  TImageDataDirectory = _IMAGE_DATA_DIRECTORY;
  PImageDataDirectory = ^_IMAGE_DATA_DIRECTORY;

  /// Optional Header 32bit
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-standard-fields-image-only
  _IMAGE_OPTIONAL_HEADER = record
    // Standard COFF fields
    Magic: word;
    MajorLinkerVersion: byte;
    MinorLinkerVersion: byte;
    SizeOfCode: cardinal;
    SizeOfInitializedData: cardinal;
    SizeOfUninitializedData: cardinal;
    AddressOfEntryPoint: cardinal;
    BaseOfCode: cardinal;
    BaseOfData: cardinal;
    // Windows Specific fields
    ImageBase: cardinal;
    SectionAlignment: cardinal;
    FileAlignment: cardinal;
    MajorOperatingSystemVersion: word;
    MinorOperatingSystemVersion: word;
    MajorImageVersion: word;
    MinorImageVersion: word;
    MajorSubsystemVersion: word;
    MinorSubsystemVersion: word;
    Win32VersionValue: cardinal;
    SizeOfImage: cardinal;
    SizeOfHeaders: cardinal;
    CheckSum: cardinal;
    Subsystem: word;
    DllCharacteristics: word;
    SizeOfStackReserve: cardinal;
    SizeOfStackCommit: cardinal;
    SizeOfHeapReserve: cardinal;
    SizeOfHeapCommit: cardinal;
    LoaderFlags: cardinal;
    NumberOfRvaAndSizes: cardinal;
    DataDirectory: array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;
  TImageOptionalHeader32 = _IMAGE_OPTIONAL_HEADER;
  PImageOptionalHeader32 = ^_IMAGE_OPTIONAL_HEADER;

  /// Optional Header 64bit
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-standard-fields-image-only
  _IMAGE_OPTIONAL_HEADER64 = record
    // Standard COFF fields
    Magic: word;
    MajorLinkerVersion: byte;
    MinorLinkerVersion: byte;
    SizeOfCode: cardinal;
    SizeOfInitializedData: cardinal;
    SizeOfUninitializedData: cardinal;
    AddressOfEntryPoint: cardinal;
    BaseOfCode: cardinal;
    // Windows Specific fields
    ImageBase: Int64;
    SectionAlignment: cardinal;
    FileAlignment: cardinal;
    MajorOperatingSystemVersion: word;
    MinorOperatingSystemVersion: word;
    MajorImageVersion: word;
    MinorImageVersion: word;
    MajorSubsystemVersion: word;
    MinorSubsystemVersion: word;
    Win32VersionValue: cardinal;
    SizeOfImage: cardinal;
    SizeOfHeaders: cardinal;
    CheckSum: cardinal;
    Subsystem: word;
    DllCharacteristics: word;
    SizeOfStackReserve: Int64;
    SizeOfStackCommit: Int64;
    SizeOfHeapReserve: Int64;
    SizeOfHeapCommit: Int64;
    LoaderFlags: cardinal;
    NumberOfRvaAndSizes: cardinal;
    DataDirectory: array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;
  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
  PImageOptionalHeader64 = ^_IMAGE_OPTIONAL_HEADER64;

  /// Common OptionalHeader (union for 32/64 bit)
  TImageOptionalHeader = record
    case integer of
      0:
        (PHeader32: PImageOptionalHeader32);
      1:
        (PHeader64: PImageOptionalHeader64);
  end;
  PImageOptionalHeader = ^TImageOptionalHeader;

  /// Complete PE Header - 32bit version
  // - COFF Header + Optional Header
  _IMAGE_NT_HEADERS = record
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader32;
  end;
  TImageNtHeaders32 = _IMAGE_NT_HEADERS;
  PImageNtHeaders32 = ^_IMAGE_NT_HEADERS;

  /// Complete PE Header - 64bit version
  // - COFF Header + Optional Header
  _IMAGE_NT_HEADERS64 = record
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader64;
  end;
  TImageNtHeaders64 = _IMAGE_NT_HEADERS64;
  PImageNtHeaders64 = ^_IMAGE_NT_HEADERS64;

  /// Common NtHeaders (union for 32/64 bit)
  TImageNtHeaders = record
    case integer of
      0:
        (PHeaders32: PImageNtHeaders32);
      1:
        (PHeaders64: PImageNtHeaders64);
      2:
        (PHeaders: pointer);
  end;
  PImageNtHeaders = ^TImageNtHeaders;

  /// Section Table
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#section-table-section-headers
  _IMAGE_SECTION_HEADER = object
    Name8: array[0..7] of AnsiChar;
    VirtualSize: cardinal;
    VirtualAddress: cardinal;
    SizeOfRawData: cardinal;
    PointerToRawData: cardinal;
    PointerToRelocations: cardinal;
    PointerToLinenumbers: cardinal;
    NumberOfRelocations: word;
    NumberOfLinenumbers: word;
    Characteristics: cardinal;
    function Name: RawUtf8;
  end;
  TImageSectionHeader = _IMAGE_SECTION_HEADER;
  PImageSectionHeader = ^_IMAGE_SECTION_HEADER;

  /// .reloc Section header
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#coff-relocations-object-only
  TImageBaseRelocation = record
    VirtualAddress: cardinal;
    SymbolTableIndex: cardinal;
    RelocType: word;
  end;

  /// .edata Section header
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#export-directory-table
  _IMAGE_EXPORT_DIRECTORY = record
    Characteristics: cardinal;
    TimeDateStamp: cardinal;
    MajorVersion: word;
    MinorVersion: word;
    Name: cardinal;
    Base: cardinal;
    NumberOfFunctions: cardinal;
    NumberOfNames: cardinal;
    AddressOfFunctions: cardinal;     { RVA from base of image }
    AddressOfNames: cardinal;        { RVA from base of image }
    AddressOfNameOrdinals: cardinal;  { RVA from base of image }
    //ForAligmentBuf: array[1..16] of byte;
  end;
  TImageExportDirectory = _IMAGE_EXPORT_DIRECTORY;
  PImageExportDirectory = ^_IMAGE_EXPORT_DIRECTORY;

  /// .idata Section Header
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#import-directory-table
  _IMAGE_IMPORT_DESCRIPTOR = record
    case integer of
      0:
        (Characteristics: cardinal);     // 0 for terminating null import descriptor
      1:
        (OriginalFirstThunk: cardinal;   // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
         TimeDateStamp: cardinal;        // 0 if not bound,
                                         // -1 if bound, and real date\time stamp
                                         //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                         // O.W. date/time stamp of DLL bound to (Old BIND)
         ForwarderChain: cardinal;       // -1 if no forwarders
         Name: cardinal;
         FirstThunk: cardinal;           // RVA to IAT (if bound this IAT has actual addresses)
        );
  end;
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  PImageImportDescriptor = ^_IMAGE_IMPORT_DESCRIPTOR;

  /// .tls Section Header
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#the-tls-directory
  _IMAGE_TLS_DIRECTORY32 = record
    StartAddressOfRawData: cardinal;
    EndAddressOfRawData: cardinal;
    AddressOfIndex: cardinal;       // PDWORD
    AddressOfCallBacks: cardinal;   // PIMAGE_TLS_CALLBACK *
    SizeOfZeroFill: cardinal;
    Characteristics: cardinal; // Reserved:20-bit, Alignment:4-bit, Reserved:8-bit
  end;
  TImageTSLDirectory = _IMAGE_TLS_DIRECTORY32;
  PImageTSLDirectory = ^_IMAGE_TLS_DIRECTORY32;

  /// .rsrc Section header
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#resource-directory-table
  PImageResourceDirectoryEntry = ^_IMAGE_RESOURCE_DIRECTORY_ENTRY;
  _IMAGE_RESOURCE_DIRECTORY = object
  private
    function GetEntry(Index: integer): PImageResourceDirectoryEntry;
  public
    Characteristics: cardinal;
    TimeDateStamp: cardinal;
    MajorVersion: word;
    MinorVersion: word;
    NumberOfNamedEntries: word;
    NumberOfIdEntries: word;
    /// Get the total number of entries
    // - Sum of Named entries and id entries
    // - High boundary of Entries indexes
    function NumberOfEntries: cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// Get the entry at the given index
    // - Return nil if out of bounds
    property Entries[Index: integer]: PImageResourceDirectoryEntry
      read GetEntry;
  end;
  TImageResourceDirectory = _IMAGE_RESOURCE_DIRECTORY;
  PImageResourceDirectory = ^_IMAGE_RESOURCE_DIRECTORY;

  /// Resource directory entries
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#resource-directory-entries
  PImageResourceDataEntry = ^_IMAGE_RESOURCE_DATA_ENTRY;
  _IMAGE_RESOURCE_DIRECTORY_ENTRY = object
    // Nested record to use a variable part not at the end of the record
    Identifier: record
      case integer of
        0:
          (NameOffset: cardinal);
        1:
          (Id: cardinal);
    end;
    OffsetToData: cardinal;
    /// Check if the entry is a directory entry or a data entry
    // - An entry is a directory entry if the high bit of OffsetToData is set
    function IsDirectory: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// Get the offset to the directory
    function OffsetToDirectory: cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// Get the subdirectory
    // - StartAddress is the resource directory table address
    // - Return nil if the entry is not a directory entry
    function Directory(StartAddress: pointer): PImageResourceDirectory;
    /// Get the entry data
    // - StartAddress is the resource directory table address
    // - Return nil if the entry is not a data entry
    function Data(StartAddress: pointer): PImageResourceDataEntry;
  end;
  TImageResourceDirectoryEntry = _IMAGE_RESOURCE_DIRECTORY_ENTRY;

  /// Resource data entry
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#resource-data-entry
  _IMAGE_RESOURCE_DATA_ENTRY = record
    DataRVA: cardinal;
    Size: cardinal;
    CodePage: cardinal;
    Reserved: cardinal;
  end;
  TImageResourceDataEntry = _IMAGE_RESOURCE_DATA_ENTRY;

  /// Version information
  // - https://learn.microsoft.com/en-us/windows/win32/menurc/vs-versioninfo
  _VS_VERSIONINFO = record
    Length: word;
    ValueLength: word;
    VersionType: word;
  end;
  TVsVersionInfo = _VS_VERSIONINFO;
  PVsVersionInfo = ^_VS_VERSIONINFO;

  /// Fixed file info
  // - https://learn.microsoft.com/en-us/windows/win32/api/VerRsrc/ns-verrsrc-vs_fixedfileinfo
  _VS_FIXEDFILEINFO = object
    Signature: cardinal;
    StructVersion: cardinal;
    FileVersionMS: cardinal;
    FileVersionLS: cardinal;
    ProductVersionMS: cardinal;
    ProductVersionLS: cardinal;
    FileFlagsMask: cardinal;
    FileFlags: cardinal;
    FileOS: cardinal;
    FileType: cardinal;
    FileSubtype: cardinal;
    FileDateMS: cardinal;
    FileDateLS: cardinal;
    /// Get the file major version number
    function FileMajorVersion: cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// Get the file minor version number
    function FileMinorVersion: cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// Get the file patch version number
    function FilePatchVersion: cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// Get the file build version number
    function FileBuildVersion: cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// Get the file version as a string
    // - Format is '[major].[minor].[patch].[build]'
    function FileVersionStr: string;
  end;
  TVSFixedFileInfo = _VS_FIXEDFILEINFO;
  PVSFixedFileInfo = ^_VS_FIXEDFILEINFO;

  /// StringFileInfo struct, same as VS_VERSIONINFO
  // https://learn.microsoft.com/en-us/windows/win32/menurc/stringfileinfo
  TStringFileInfo = TVsVersionInfo;
  PStringFileInfo = ^TStringFileInfo;

  /// StringTable struct, same as VS_VERSIONINFO
  // https://learn.microsoft.com/en-us/windows/win32/menurc/stringtable
  TStringTable = TVsVersionInfo;
  PStringTable = ^TStringTable;

  /// string struct, same as VS_VERSIONINFO
  // https://learn.microsoft.com/en-us/windows/win32/menurc/string-str
  TStringTableEntry = TVsVersionInfo;
  PStringTableEntry = ^TStringTableEntry;

  /// VarFileInfo struct, same as VS_VERSIONINFO
  // https://learn.microsoft.com/en-us/windows/win32/menurc/varfileinfo
  TVarFileInfo = TVsVersionInfo;
  PVarFileInfo = ^TVarFileInfo;

  {$A+} // back to regular field alignment


/// Align an offset with a base address
// - Resulting offset is the first aligned offset starting from the input offset.
// - An offset is aligned if Base - Offset is a cardinal size (4 bytes) multiple
function DWordAlign(Offset: cardinal; Base: cardinal = 0): cardinal;
  {$ifdef HASINLINE} inline; {$endif}


{ ************ High-Level PE (.exe, .dll...) File Reader }

{ TSynPELoader }

type
  /// Cross platform PE (Portable Executable) file parser
  // - see @https://learn.microsoft.com/en-us/windows/win32/debug/pe-format
  // - the file is mapped in memory and most of the getters will return pointer
  // to the mapped memory instead of copy, however some data is cached when parsed
  // to simplify later access (like StringFileInfoEntries)
  // - when parsing specific sections (like ParseResources) pointers to the inner
  // data are saved and later accessible using associated properties
  // - see GetPEFileVersion() as a wrapper to this class
  TSynPELoader = class
  private
    // Saved pointers
    // - Headers
    fPEHeader: TImageNtHeaders;
    fCoffHeader: PImageFileHeader;
    fSectionHeadersStart: PImageSectionHeader;
    // - Resource Section pointers
    fNumberOfSections: cardinal;
    fVersionInfo: PVsVersionInfo;
    fFixedFileInfo: PVSFixedFileInfo;
    fStringFileInfo: PStringFileInfo;
    fFirstStringTable: PStringTable;
    fVarFileInfo: PVarFileInfo;
    // Cached data
    // - Resource Section data
    fStringFileInfoEntries: TDocVariantData;
    // raw PE File
    fMap: TMemoryMap;
    function GetImageDataDirectory(DirectoryId: cardinal): PImageDataDirectory;
    function GetSectionHeader(SectionId: cardinal): PImageSectionHeader;
    /// Parse a StringFileInfo or VarFileInfo struct.
    // - Address is the starting address of a StringFileInfo/VarFileInfo struct
    // - Set fStringFileInfo or fVarFileInfo depending on the struct at the given address
    // - Returns the end address of the file info
    // - Called by ParseResources
    function ParseFileInfo(Address: pointer): pointer;
  public
    /// Constructor which initializes all saved pointers to nil
    constructor Create;
    /// Destructor which unloads the current file
    destructor Destroy; override;
    /// Load a PE file in memory
    // - Return true if the file has been successfully loaded
    function LoadFromFile(const Filename: TFileName): boolean;
    /// Unload the current file in memory
    // - Can be called even if no file is loaded
    procedure Unload;

    /// Search the section containing the given RVA
    // - Return the section index (see SectionHeaders property)
    // - If no section is found, return -1
    function GetSectionIndexByRVA(RVA: cardinal): integer;
    /// Search the section named AName
    // - Return the section index (see SectionHeaders property)
    // - If no section is found, return -1
    function GetSectionIndexByName(const AName: RawUtf8): integer;
    /// Search the section associated to the given directory
    // see IMAGE_DIRECTORY_ENTRY_* consts
    // - Return the section index (see SectionHeaders property)
    // - If no section is found, return -1
    function GetSectionIndexFromDirectory(DirectoryId: cardinal): integer;

    /// Translate RVA to physical address
    // - ASectionId is the section containing the RVA
    // - Doesn't verify section id, an invalid section id will lead to access violation
    // - Return the physical address, ie the offset from the file first byte
    function GetPhAddByRVA(RVA: cardinal; ASectionId: cardinal): cardinal; overload;
    /// Translate RVA to physical address
    // - Return the physical address, ie the offset from the file first byte
    // - If the RVA is not contained by any section, 0 is returned
    function GetPhAddByRVA(RVA: cardinal): cardinal; overload;

    /// Parse the Resource directory associated section
    // see https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#the-rsrc-section
    // - Return false if there is no resource section
    // - Set VersionInfo, FixedFileInfo, StringFileInfo, FirstStringTable
    // VarFileInfo pointers if found
    function ParseResources: boolean;
    /// Parse the StringFileInfo entries
    // - Call ParseResources if StringFileInfo is nil before returning false if
    // it is still nil
    // - Parsed entries are accessible using the StringFileInfoEntries property
    function ParseStringFileInfoEntries: boolean;
    /// Check whether there is a valid PE file loaded
    function IsLoaded: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// Check whether the PE file is an x64 arch
    function Is64: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// Number of sections
    // - It is the high boundary of the SectionHeaders property
    property NumberOfSections: cardinal
      read fNumberOfSections;

    /// PE Header pointer for 32bit arch
    // - See Is64 to check arch
    // - Regroup the COFF Header and the Optional Header
    property PEHeader32: PImageNtHeaders32
      read fPEHeader.PHeaders32;
    /// PE Header pointer for 64bit arch
    // - See Is64 to check arch
    // - Regroup the COFF Header and the Optional Header
    property PEHeader64: PImageNtHeaders64
      read fPEHeader.PHeaders64;
    /// COFF Header (start of PE Header)
    property CoffHeader: PImageFileHeader
      read fCoffHeader;
    /// First image section header (end of PE Header)
    property SectionHeadersStart: PImageSectionHeader
      read fSectionHeadersStart;
    /// Get the image data directory struct at the given id
    // - See IMAGE_DIRECTORY_ENTRY_* consts
    // - Return -1 if the DirectoryId is out of bounds
    property ImageDataDirectory[DirectoryId: cardinal]: PImageDataDirectory
     read GetImageDataDirectory;
    /// Get the section header at the given section id
    // - Return nil if the id is out of bounds
    property SectionHeaders[SectionId: cardinal]: PImageSectionHeader
      read GetSectionHeader;

    /// VersionInfo Resource pointer, set by ParseResources
    property VersionInfo: PVsVersionInfo
      read fVersionInfo;
    /// FixedFileInfo Resource pointer, set by ParseResources
    property FixedFileInfo: PVSFixedFileInfo
      read fFixedFileInfo;
    /// StringFileInfo Resource pointer, set by ParseResources
    property StringFileInfo: PStringFileInfo
      read fStringFileInfo;
    /// FirstStringTable Resource pointer, set by ParseResources
    property FirstStringTable: PStringTable
      read fFirstStringTable;
    /// VarFileInfo Resource pointer, set by ParseResources
    property VarFileInfo: PVarFileInfo
      read fVarFileInfo;
    /// Get the file version as a string
    // - Format is '[major].[minor].[patch].[build]'
    // - is a wrapper around FixedFileInfo.FileVersionStr
    function FileVersionStr: string;
    /// StringFileInfo entries, parsed a TDocVariantDat
    // - Populated by ParseStringFileInfo
    property StringFileInfoEntries: TDocVariantData
      read fStringFileInfoEntries;
  end;


/// return all version information from a Portable Executable (Win32/Win64) file
// - returns a TDocVariant with all parsed string versions, and 'FileVersionNum'
// as TSynPELoader.FileVersionStr from _VS_FIXEDFILEINFO resource, and 'IsWin64'
// as boolean TSynPELoader.Is64 value
// - returns a void document if the file does not exist, or has no info resource
function GetPEFileVersion(const aFileName: TFileName): TDocVariantData;


implementation

  
{ ************ Low-Level DOS/PE/COFF Encoding Structures }

{ _VS_FIXEDFILEINFO }

function _VS_FIXEDFILEINFO.FileMajorVersion: cardinal;
begin
  result := FileVersionMS shr 16;
end;

function _VS_FIXEDFILEINFO.FileMinorVersion: cardinal;
begin
  result := FileVersionMS and $ffff;
end;

function _VS_FIXEDFILEINFO.FilePatchVersion: cardinal;
begin
  result := FileVersionLS shr 16;
end;

function _VS_FIXEDFILEINFO.FileBuildVersion: cardinal;
begin
  result := FileVersionLS and $ffff;
end;

function _VS_FIXEDFILEINFO.FileVersionStr: string;
begin
  if @self = nil then
    result := ''
  else
    result := format('%d.%d.%d.%d',
      [FileMajorVersion, FileMinorVersion, FilePatchVersion, FileBuildVersion]);
end;


{ _IMAGE_RESOURCE_DIRECTORY }

function _IMAGE_RESOURCE_DIRECTORY.NumberOfEntries: cardinal;
begin
  result := NumberOfIdEntries + NumberOfNamedEntries;
end;

function _IMAGE_RESOURCE_DIRECTORY.GetEntry(Index: integer): PImageResourceDirectoryEntry;
begin
  if cardinal(Index) >= NumberOfEntries then
    raise EPeCoffLoader.Create('_IMAGE_RESOURCE_DIRECTORY');
  result := @PByteArray(@self)[
    SizeOf(self) + Index * SizeOf(TImageResourceDirectoryEntry)];
end;


{ _IMAGE_RESOURCE_DIRECTORY_ENTRY }

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.IsDirectory: boolean;
begin
  result := ((OffsetToData and $80000000) shr 31) = 1;
end;

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.OffsetToDirectory: cardinal;
begin
  result := OffsetToData and $7fffffff;
end;

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.Directory(
  StartAddress: pointer): PImageResourceDirectory;
begin
  if not IsDirectory then
    raise EPeCoffLoader.Create('_IMAGE_RESOURCE_DIRECTORY_ENTRY.Directory?');
  result := @PByteArray(StartAddress)[OffsetToDirectory];
end;

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.Data(
  StartAddress: pointer): PImageResourceDataEntry;
begin
  if IsDirectory then
    raise EPeCoffLoader.Create('_IMAGE_RESOURCE_DIRECTORY_ENTRY.Data?');
  result := @PByteArray(StartAddress)[OffsetToData];
end;


{ _IMAGE_SECTION_HEADER }

function _IMAGE_SECTION_HEADER.Name: RawUtf8;
begin
  if Name8[7] <> #0 then
    FastSetString(result, @Name8, 8)
  else
    FastSetString(result, @Name8, StrLen(@Name8));
end;


function DWordAlign(Offset, Base: cardinal): cardinal;
begin
  result := ((Offset + Base + 3) and $FFFFFFFC) - (Base and $FFFFFFFC);
end;



{ ************ High-Level PE (.exe, .dll...) File Reader }

{ TSynPELoader }

function TSynPELoader.IsLoaded: boolean;
begin
  result := fMap.Buffer <> nil;
end;

function TSynPELoader.Is64: boolean;
begin
  result := PEHeader32^.OptionalHeader.Magic = PE_64_MAGIC;
end;

function TSynPELoader.GetSectionHeader(SectionId: cardinal): PImageSectionHeader;
begin
  if (fSectionHeadersStart = nil) or
     (SectionId >= NumberOfSections) then
    raise EPeCoffLoader.Create('_IMAGE_RESOURCE_DIRECTORY_ENTRY.Data?');
  result := pointer(PAnsiChar(fSectionHeadersStart) +
    SizeOf(TImageSectionHeader) * SectionId);
end;

constructor TSynPELoader.Create;
begin
  fStringFileInfoEntries.InitFast(dvObject);
end;

destructor TSynPELoader.Destroy;
begin
  Unload;
  inherited Destroy;
end;

procedure TSynPELoader.Unload;
begin
  fMap.UnMap;
  fVersionInfo := nil;
  fCoffHeader := nil;
  fSectionHeadersStart := nil;
  fFixedFileInfo := nil;
  fStringFileInfo := nil;
  fFirstStringTable := nil;
  fVarFileInfo := nil;
  fNumberOfSections := 0;
  fStringFileInfoEntries.Reset;
end;

function TSynPELoader.GetSectionIndexByRVA(RVA: cardinal): integer;
begin
  if IsLoaded then
    for result := 0 to NumberOfSections - 1 do
      with SectionHeaders[result]^ do
        if (RVA >= VirtualAddress) and
           (RVA < VirtualAddress + VirtualSize) then
          exit;
  result := -1;
end;

function TSynPELoader.GetSectionIndexByName(const AName: RawUtf8): integer;
begin
  if IsLoaded then
    for result := 0 to NumberOfSections - 1 do
      if PropNameEquals(SectionHeaders[result]^.Name, AName) then
        exit;
  result := -1;
end;

function TSynPELoader.GetSectionIndexFromDirectory(DirectoryId: cardinal): integer;
begin
  if DirectoryId >= IMAGE_NUMBEROF_DIRECTORY_ENTRIES then
    result := -1
  else
    result := GetSectionIndexByRVA(ImageDataDirectory[DirectoryId]^.VirtualAddress);
end;

function TSynPELoader.GetPhAddByRVA(RVA: cardinal; ASectionId: cardinal): cardinal;
begin
  result := RVA - SectionHeaders[ASectionId]^.VirtualAddress +
                  SectionHeaders[ASectionId]^.PointerToRawData;
end;

function TSynPELoader.GetPhAddByRVA(RVA: cardinal): cardinal;
var
  SectionID: integer;
begin
  SectionID := GetSectionIndexByRVA(RVA);
  if SectionID >= 0 then
    result := GetPhAddByRVA(RVA, SectionID)
  else
    result := 0;
end;

function TSynPELoader.GetImageDataDirectory(DirectoryId: cardinal): PImageDataDirectory;
begin
  if DirectoryId >= IMAGE_NUMBEROF_DIRECTORY_ENTRIES then
    raise EPeCoffLoader.CreateUtf8('%.ImageDataDirectory[%]', [self, DirectoryID]);
  if Is64 then
    result := @PEHeader64^.OptionalHeader.DataDirectory[DirectoryId]
  else
    result := @PEHeader32^.OptionalHeader.DataDirectory[DirectoryId];
end;

function TSynPELoader.ParseResources: boolean;
var
  Directory, VersionEntriesDir: PImageResourceDirectory;
  EntryId, i: integer;
  Entry: PImageResourceDirectoryEntry;
  VersionData: PImageResourceDataEntry;
  VersionInfoStr: PWideChar;
  ResourceSct: PImageSectionHeader;
  NextAddress: pointer;
begin
  result := false;
  ResourceSct := SectionHeaders[GetSectionIndexFromDirectory(IMAGE_DIRECTORY_ENTRY_RESOURCE)];
  if not Assigned(ResourceSct) then
    exit;
  Directory := pointer(fMap.Buffer + ResourceSct^.PointerToRawData);
  result := true;
  for EntryId := 0 to Directory^.NumberOfEntries - 1 do
  begin
    Entry := Directory^.Entries[EntryId];
    // Version Resource
    if Entry^.Identifier.Id = RT_VERSION then
    begin
      VersionEntriesDir := Entry^.Directory(Directory)^.Entries[0]^.Directory(Directory);
      for i := 0 to VersionEntriesDir^.NumberOfEntries - 1 do
      begin
        VersionData := VersionEntriesDir^.Entries[i]^.Data(Directory);
        fVersionInfo := pointer(fMap.Buffer + GetPhAddByRVA(VersionData^.DataRVA));
        // 'VS_VERSION_INFO', Strings are UTF-16 encoded which explains the
        // 2 * StrLenW (2 bytes per WideChar) in later offset
        VersionInfoStr := pointer(PAnsiChar(VersionInfo) + SizeOf(VersionInfo^));
        // Invalid Entry
        if StrCompW(VersionInfoStr, 'VS_VERSION_INFO') <> 0 then
          break;
        // Fixed File Info
        fFixedFileInfo := pointer(PAnsiChar(VersionInfo) +
          DWordAlign(SizeOf(VersionInfo^) + 2 * (StrLenW(VersionInfoStr) + 1),
            VersionData^.DataRVA));
        // string File Info / Var File Info
        NextAddress := pointer(PAnsiChar(fFixedFileInfo) + SizeOf(fFixedFileInfo^));
        while NextAddress <> nil do
          NextAddress := ParseFileInfo(NextAddress);
      end;
    end;
  end;
end;

function TSynPELoader.ParseStringFileInfoEntries: boolean;
var
  StringTable, StringTableEnd: PStringTable;
  LangID, Key, Value: PWideChar;
  Offset: integer;
  StrEntry, StrEntryEnd: PStringTableEntry;
begin
  result := false;
  if StringFileInfo = nil then
    ParseResources;
  if (StringFileInfo = nil) or
     (FirstStringTable = nil) then
    exit;
  result := true;
  Offset := PAnsiChar(StringFileInfo) - pointer(VersionInfo);
  StringTable := FirstStringTable;
  StringTableEnd := pointer(PAnsiChar(StringFileInfo) + StringFileInfo^.Length); 
  repeat
    LangID := pointer(PAnsiChar(StringTable) + SizeOf(StringTable^));
    StrEntry := pointer(PAnsiChar(StringTable) +
      DWordAlign(SizeOf(StringTable^) + 2 * (StrLenW(LangID) + 1), Offset));
    StrEntryEnd := pointer(PAnsiChar(StringTable) + StringTable^.Length);
    while PtrUInt(StrEntry) < PtrUInt(StrEntryEnd) do
    begin
      Key := pointer(PAnsiChar(StrEntry) + SizeOf(StrEntry^));
      Value := pointer(PAnsiChar(StrEntry) +
        DWordAlign(SizeOf(StrEntry^) + 2 * (StrLenW(Key) + 1), Offset));
      fStringFileInfoEntries.AddValue(UnicodeBufferToUtf8(Key), UnicodeBufferToUtf8(Value));
      if StrEntry^.Length = 0 then
        StrEntry := StrEntryEnd // end
      else
        StrEntry := pointer(PAnsiChar(StrEntry) +
          DWordAlign(StrEntry^.Length, Offset));
    end;
    if StringTable^.Length = 0 then
      break;
    inc(PByte(StringTable), DWordAlign(StringTable^.Length, Offset));
  until PtrUInt(StringTable) >= PtrUInt(StringTableEnd);
end;

function TSynPELoader.ParseFileInfo(Address: pointer): pointer;
var
  FileInfoStruct: PVsVersionInfo;
  FileInfoStr: PWideChar;
  Offset: cardinal;
begin
  // string File Info / Var File Info
  Offset := PAnsiChar(VersionInfo) - Address;
  FileInfoStruct := Address;
  FileInfoStr := pointer(PAnsiChar(FileInfoStruct) + SizeOf(StringFileInfo^));
  if StrCompW(FileInfoStr, 'StringFileInfo') = 0 then
  begin
    fStringFileInfo := pointer(FileInfoStruct);
    fFirstStringTable := pointer(PAnsiChar(StringFileInfo) +
      DWordAlign(SizeOf(StringFileInfo^) + 2 * (StrLenW(FileInfoStr) + 1), Offset));
  end
  else if StrCompW(FileInfoStr, 'VarFileInfo') = 0 then
    fVarFileInfo := pointer(FileInfoStruct); // never found (bug?) but not needed
  if (FileInfoStruct^.Length = 0) or
     (PAnsiChar(Address) + FileInfoStruct^.Length >=
      PAnsiChar(VersionInfo) + VersionInfo^.Length) then
    result := nil
  else
    result := PAnsiChar(Address) + FileInfoStruct^.Length;
end;

function TSynPELoader.LoadFromFile(const Filename: TFileName): boolean;
var
  DOSHeader: PImageDOSHeader;
begin
  result := false;
  // Unloading the previous PE
  UnLoad;
  // map the executable in memory, and parse its header
  if fMap.Map(FileName) then
  try
    DOSHeader := pointer(fMap.Buffer);
    if (fMap.Size > SizeOf(DOSHeader^)) and
       (DOSHeader^.e_magic = DOS_HEADER_MAGIC) then
    begin
      // e_lfanew is pointer to PE Header (0x3c after start of file)
      // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#signature-image-only
      fPEHeader.PHeaders := pointer(fMap.Buffer + DOSHeader^.e_lfanew);
      fCoffHeader := @PEHeader32^.FileHeader; // doesn't change in 32/64bit
      fNumberOfSections := fCoffHeader^.NumberOfSections;
      fSectionHeadersStart := pointer(PAnsiChar(fPEHeader.PHeaders) +
        SizeOf(CoffHeader^) + CoffHeader^.SizeOfOptionalHeader);
      // Invalid PE Header Magic number
      // Or Invalid Optional header magic number (must be 0x10b in 32bit, 0x20b in 64bit)
      result := (CoffHeader^.Signature = PE_HEADER_MAGIC) and
                (CoffHeader^.SizeOfOptionalHeader <> 0) and
                ((PEHeader32^.OptionalHeader.Magic = PE_32_MAGIC) or
                 (PEHeader32^.OptionalHeader.Magic = PE_64_MAGIC));
    end;
  finally
    if not result then
      UnLoad; // clear on parsing error
  end;
end;

function TSynPELoader.FileVersionStr: string;
begin
  result := fFixedFileInfo.FileVersionStr;
end;


function GetPEFileVersion(const aFileName: TFileName): TDocVariantData;
begin
  result.InitFast;
  with TSynPELoader.Create do
  try
    if LoadFromFile(aFileName) then
      if ParseStringFileInfoEntries then
      begin
        result := StringFileInfoEntries;
        result.AddNameValuesToObject([
          'FileVersionNum', FileVersionStr,
          'IsWin64',        Is64]);
      end;
  finally
    Free;
  end;
end;


end.

