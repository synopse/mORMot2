/// Microsoft PE COFF File Reader
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.misc.pecoff;

{
  *****************************************************************************

   Cross-Platform Portable Executable (PE) and COFF File Reader
    - Low-Level DOS/PE/COFF Encoding Structures
    - High-Level PE (.exe, .dll...) File Reader
    - Windows Executable Digital Signature Stuffing

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
  public
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
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#the-tls-directory
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
  // - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#resource-directory-entries
  PImageResourceDataEntry = ^_IMAGE_RESOURCE_DATA_ENTRY;
  _IMAGE_RESOURCE_DIRECTORY_ENTRY = object
  public
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
  public
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
    /// Get the file version as a text
    // - Format is '[major].[minor].[patch].[build]'
    function FileVersionStr: RawUtf8;
  end;
  TVSFixedFileInfo = _VS_FIXEDFILEINFO;
  PVSFixedFileInfo = ^_VS_FIXEDFILEINFO;

  /// StringFileInfo struct, same as VS_VERSIONINFO
  // - https://learn.microsoft.com/en-us/windows/win32/menurc/stringfileinfo
  TStringFileInfo = TVsVersionInfo;
  PStringFileInfo = ^TStringFileInfo;

  /// StringTable struct, same as VS_VERSIONINFO
  // - https://learn.microsoft.com/en-us/windows/win32/menurc/stringtable
  TStringTable = TVsVersionInfo;
  PStringTable = ^TStringTable;

  /// string struct, same as VS_VERSIONINFO
  // - https://learn.microsoft.com/en-us/windows/win32/menurc/string-str
  TStringTableEntry = TVsVersionInfo;
  PStringTableEntry = ^TStringTableEntry;

  /// VarFileInfo struct, same as VS_VERSIONINFO
  // - https://learn.microsoft.com/en-us/windows/win32/menurc/varfileinfo
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
  // - the file is mapped in memory and most of the getters will return pointers
  // to the mapped memory instead of copying the information, however some data
  // is cached when parsed to simplify later access (e.g. StringFileInfoEntries)
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
    // raw PE File as mapped in memory
    fMap: TMemoryMap;
    function GetImageDataDirectory(DirectoryId: cardinal): PImageDataDirectory;
    function GetSectionHeader(SectionId: cardinal): PImageSectionHeader;
    /// parse a StringFileInfo or VarFileInfo struct.
    // - Address is the starting address of a StringFileInfo/VarFileInfo struct
    // - set fStringFileInfo or fVarFileInfo depending on the struct at the given address
    // - returns the end address of the file info
    // - called by ParseResources
    function ParseFileInfo(Address: pointer): pointer;
  public
    /// constructor which initializes all saved pointers to nil
    constructor Create;
    /// destructor which unloads the current file
    destructor Destroy; override;
    /// load a PE file in memory
    // - return true if the file has been successfully loaded
    function LoadFromFile(const Filename: TFileName): boolean;
    /// unload the current file in memory
    // - can be called even if no file is loaded
    procedure Unload;

    /// search the section containing the given RVA
    // - return the section index (see SectionHeaders property)
    // - if no section is found, return -1
    function GetSectionIndexByRVA(RVA: cardinal): integer;
    /// search the section named AName
    // - return the section index (see SectionHeaders property)
    // - if no section is found, return -1
    function GetSectionIndexByName(const AName: RawUtf8): integer;
    /// search the section associated to the given directory
    // - accepts IMAGE_DIRECTORY_ENTRY_EXPORT ... constants
    // - return the section index (see SectionHeaders property)
    // - if no section is found, return -1
    function GetSectionIndexFromDirectory(DirectoryId: cardinal): integer;

    /// translate RVA to physical address
    // - ASectionId is the section containing the RVA
    // - doesn't verify section id, an invalid section id will lead to access violation
    // - return the physical address, ie the offset from the file first byte
    function GetPhAddByRVA(RVA: cardinal; ASectionId: cardinal): cardinal; overload;
    /// translate RVA to physical address
    // - return the physical address, ie the offset from the file first byte
    // - if the RVA is not contained by any section, 0 is returned
    function GetPhAddByRVA(RVA: cardinal): cardinal; overload;

    /// parse the Resource directory associated section
    // - see https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#the-rsrc-section
    // - return false if there is no resource section
    // - set VersionInfo, FixedFileInfo, StringFileInfo, FirstStringTable and
    // VarFileInfo pointers if found
    function ParseResources: boolean;
    /// parse the StringFileInfo entries
    // - this is the main method to be called after Create
    // - call ParseResources if needed
    // - parsed entries are accessible using the StringFileInfoEntries property
    function ParseStringFileInfoEntries: boolean;

    /// check whether there is a valid PE file loaded
    function IsLoaded: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check whether the PE file is an x64 arch
    function Is64: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// number of sections in this PE file
    // - it is the high boundary of the SectionHeaders property
    property NumberOfSections: cardinal
      read fNumberOfSections;

    /// PE Header pointer for 32bit arch
    // - see Is64 to check arch
    // - regroup the COFF Header and the Optional Header
    property PEHeader32: PImageNtHeaders32
      read fPEHeader.PHeaders32;
    /// PE Header pointer for 64bit arch
    // - see Is64 to check arch
    // - regroup the COFF Header and the Optional Header
    property PEHeader64: PImageNtHeaders64
      read fPEHeader.PHeaders64;
    /// COFF Header (start of PE Header)
    property CoffHeader: PImageFileHeader
      read fCoffHeader;
    /// first image section header (end of PE Header)
    property SectionHeadersStart: PImageSectionHeader
      read fSectionHeadersStart;
    /// get the image data directory struct at the given id
    // - see IMAGE_DIRECTORY_ENTRY_* consts
    // - return -1 if the DirectoryId is out of bounds
    property ImageDataDirectory[DirectoryId: cardinal]: PImageDataDirectory
     read GetImageDataDirectory;
    /// get the section header at the given section id
    // - return nil if the id is out of bounds
    property SectionHeaders[SectionId: cardinal]: PImageSectionHeader
      read GetSectionHeader;

    /// VersionInfo Resource pointer, as set by ParseResources
    property VersionInfo: PVsVersionInfo
      read fVersionInfo;
    /// FixedFileInfo Resource pointer, as set by ParseResources
    property FixedFileInfo: PVSFixedFileInfo
      read fFixedFileInfo;
    /// StringFileInfo Resource pointer, as set by ParseResources
    property StringFileInfo: PStringFileInfo
      read fStringFileInfo;
    /// FirstStringTable Resource pointer, as set by ParseResources
    property FirstStringTable: PStringTable
      read fFirstStringTable;
    /// VarFileInfo Resource pointer, as set by ParseResources
    property VarFileInfo: PVarFileInfo
      read fVarFileInfo;
    /// get the file version as a string
    // - format is '[major].[minor].[patch].[build]'
    // - is a wrapper around FixedFileInfo.FileVersionStr
    function FileVersionStr: RawUtf8;
    /// StringFileInfo entries, parsed as a TDocVariant object document
    // - populated by ParseStringFileInfo
    property StringFileInfoEntries: TDocVariantData
      read fStringFileInfoEntries;
  end;


/// return all version information from a Portable Executable (Win32/Win64) file
// as a TDocVariant object document
// - returns an obhect with all parsed string versions, and "FileVersionNum"
// as TSynPELoader.FileVersionStr from _VS_FIXEDFILEINFO resource, "IsWin64"
// as boolean TSynPELoader.Is64 value, and "FullFileName" as aFileName value
// - returns a void document if the file does not exist, or has no info resource
function GetPEFileVersion(const aFileName: TFileName): TDocVariantData;


{ ************** Windows Executable Digital Signature Stuffing }

type
  /// exception raised by StuffExeCertificate() in case of processing error
  EStuffExe = class(ESynException);


/// create a NewFile executable from adding some text to MainFile digital signature
// - up to 60KB of text will be stuffed within a dummy certificate inside the
// existing digital signature, so you don't need to sign the executable again
// - FileAppend() method of mormot.core.zip has been disallowed in latest
// Windows versions, so this method is the right way for adding some custom
// information at runtime to an already-signed Windows executable
// - raise EStuffExe if MainFile has no supported signature, is already stuffed,
// or the stuffed data is too big
// - this function does not require mormot.crypt.openssl but may use it if
// available to generate a genuine dummy certificate - if UseInternalCertificate
// is true, or OpenSSL is not available, it will use a fixed constant certificate
// - use FindStuffExeCertificate() to retrieve the stuffed text
procedure StuffExeCertificate(const MainFile, NewFile: TFileName;
  const Stuff: RawUtf8; UseInternalCertificate: boolean = false);

/// retrieve the text inserted by StuffExeCertificate() into the Windows
// executable digital signature
// - raise EStuffExe if MainFile has no supported signature
// - returns the stuffed text, or '' if no text has been included
// - this function does not require mormot.crypt.openssl
function FindStuffExeCertificate(const FileName: TFileName): RawUtf8;


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

function _VS_FIXEDFILEINFO.FileVersionStr: RawUtf8;
begin
  if @self = nil then
    result := ''
  else
    FormatUtf8('%.%.%.%', [FileMajorVersion, FileMinorVersion,
      FilePatchVersion, FileBuildVersion], result);
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
  result := ((Offset + Base + 3) and $fffffffc) - (Base and $fffffffc);
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
    EPeCoffLoader.RaiseUtf8('%.ImageDataDirectory[%]', [self, DirectoryID]);
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
  try
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
  except
    result := false; // on malformatted input: intercept the GPF/EPeCoffLoader
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
  try
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
        fStringFileInfoEntries.AddValue(
          UnicodeBufferToUtf8(Key), UnicodeBufferToVariant(Value));
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
  except
    result := false; // on malformatted input: intercept the GPF/EPeCoffLoader
  end;
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
    try
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
    except
      result := false; // on malformatted input: intercept the GPF
    end;
  finally
    if not result then
      UnLoad; // clear on any parsing error
  end;
end;

function TSynPELoader.FileVersionStr: RawUtf8;
begin
  result := fFixedFileInfo.FileVersionStr;
end;


function GetPEFileVersion(const aFileName: TFileName): TDocVariantData;
var
  pe: TSynPELoader;
begin
  result.InitFast;
  pe := TSynPELoader.Create;
  try
    if pe.LoadFromFile(aFileName) and
       pe.ParseStringFileInfoEntries then
    begin
      result.AddNameValuesToObject([
        'FullFileName',   aFileName,
        'FileVersionNum', pe.FileVersionStr,
        'IsWin64',        pe.Is64]);
      result.AddFrom(variant(pe.StringFileInfoEntries));
    end;
  finally
    pe.Free;
  end;
end;


{ ************** Windows Executable Digital Signature Stuffing }

type
  // see http://msdn.microsoft.com/en-us/library/ms920091
  WIN_CERTIFICATE = record
    dwLength: cardinal;
    wRevision, wCertType: word;
  end;

const
  PE_ENTRY_OFFSET = $3c;
  CERTIFICATE_ENTRY_OFFSET = $98;
  WIN_CERT_TYPE_PKCS_SIGNED_DATA = 2;

function FindExeCertificate(const MainFile: TFileName; OutFile: THandleStream;
  out wc: WIN_CERTIFICATE; lenoffs, offs: PCardinal): RawByteString;
var
  M: TStream;
  i, read: PtrInt;
  certoffs, certlenoffs, certlen: cardinal;
  firstbuf: boolean;
  buf: array[0 .. 128 shl 10 - 1] of byte; // 128KB of temp copy buffer on stack
begin
  result := '';
  firstbuf := true;
  M := TFileStreamEx.Create(MainFile, fmOpenReadShared);
  try
    repeat
      read := M.Read(buf{%H-}, SizeOf(buf));
      if firstbuf then
      begin
        // search for COFF/PE header in the first block
        if read < 1024 then
          EStuffExe.RaiseUtf8('% read error', [MainFile]);
        i := PCardinal(@buf[PE_ENTRY_OFFSET])^; // read DOS header offset
        if (i >= read) or
           (PCardinal(@buf[i])^ <> ord('P') + ord('E') shl 8) then
          EStuffExe.RaiseUtf8('% is not a PE executable', [MainFile]);
        // parse PE header
        inc(i, CERTIFICATE_ENTRY_OFFSET);
        certoffs := PCardinal(@buf[i])^;
        certlenoffs := i + 4;
        certlen := PCardinal(@buf[certlenoffs])^;
        if (certoffs = 0) or
           (certlen = 0) then
           EStuffExe.RaiseUtf8('% has no signature', [MainFile]);
        // parse certificate table
        if certoffs + certlen <> M.Size then
          EStuffExe.RaiseUtf8('% should end with a certificate', [MainFile]);
        M.Seek(certoffs, soBeginning);
        if (M.Read(wc{%H-}, SizeOf(wc)) <> SizeOf(wc)) or
           (wc.dwLength <> certlen) or
           (wc.wRevision <> $200) or
           (wc.wCertType <> WIN_CERT_TYPE_PKCS_SIGNED_DATA) then
          EStuffExe.RaiseUtf8('% unsupported signature', [MainFile]);
        // read original signature
        dec(certlen, SizeOf(wc));
        SetLength(result, certlen);
        if cardinal(M.Read(pointer(result)^, certlen)) <> certlen then
          EStuffExe.RaiseUtf8('% certificate reading', [MainFile]);
        // note: don't remove ending #0 padding because some may be needed
        if lenoffs <> nil then
          lenoffs^ := certlenoffs;
        if offs <> nil then
          offs^ := certoffs;
        if Outfile = nil then
          break;
        M.Seek(read, soBeginning);
        firstbuf := false; // do it once
      end;
      if read > 0 then
        OutFile.WriteBuffer(buf, read);
    until read < SizeOf(buf);
  finally
    M.Free;
  end;
end;

function Asn1Len(var p: PByte): PtrUInt; // function dedicated to cert stuffing
begin
  result := p^;
  inc(p);
  if result <= $7f then
    exit;
  result := result and $7f;
  if result = 1 then
  begin
    result := p^;
    inc(p);
    exit;
  end;
  if result <> 2 then
    EStuffExe.RaiseUtf8('Parsing error: Asn1Len=%', [result]);
  result := p^;
  inc(p);
  result := (result shl 8) + p^;
  inc(p);
end;

function Asn1Next(var p: PAnsiChar; expected: byte; moveafter: boolean;
  const Ctxt: shortstring): PtrInt;
begin
  if p^ <> AnsiChar(expected) then
    EStuffExe.RaiseUtf8('Parsing %: % instead of %',
      [Ctxt, byte(p^), expected]);
  inc(p);
  result := Asn1Len(PByte(p));
  if moveafter then
    inc(p, result);
end;

procedure Asn1FixMe(fixme: PPAnsiChar; n: cardinal; added: PtrInt;
  const MainFile: TFileName);
var
  one: PAnsiChar;
begin
  repeat
    one := fixme^;
    if one[1] <> #$82 then
      EStuffExe.RaiseUtf8('Wrong fixme in %', [MainFile])
    else
      PWord(one + 2)^ := swap(word(PtrInt(swap(PWord(one + 2)^)) + added));
    inc(fixme);
    dec(n);
  until n = 0;
end;

const
  // compressed from a mormot.crypt.openssl _CreateDummyCertificate() call
  _DUMMY: array[0..405] of byte = (
    $30, $82, $02, $ba, $30, $82, $02, $62, $a0, $03, $02, $01, $02, $02, $14,
    $20, $75, $2b, $9b, $18, $86, $4e, $b4, $c2, $da, $6c, $ce, $9d, $c9, $62,
    $d0, $71, $5d, $85, $58, $30, $09, $06, $07, $2a, $86, $48, $ce, $3d, $04,
    $01, $30, $15, $31, $13, $30, $11, $06, $03, $55, $04, $03, $0c, $0a, $44,
    $75, $6d, $6d, $79, $20, $43, $65, $72, $74, $30, $1e, $17, $0d, $30, $30,
    $30, $31, $30, $31, $5a, $06, $30, $5a, $01, $5a, $17, $0d, $30, $30, $30,
    $31, $30, $31, $5a, $06, $30, $5a, $01, $5a, $30, $15, $31, $13, $30, $11,
    $06, $03, $55, $04, $03, $0c, $0a, $44, $75, $6d, $6d, $79, $20, $43, $65,
    $72, $74, $30, $59, $30, $13, $06, $07, $2a, $86, $48, $ce, $3d, $02, $01,
    $06, $08, $2a, $86, $48, $ce, $3d, $03, $01, $07, $03, $42, $00, $04, $16,
    $62, $8e, $6c, $ac, $1a, $03, $79, $17, $ae, $fe, $58, $65, $36, $6b, $9a,
    $e5, $f8, $ec, $07, $a4, $71, $03, $b7, $7d, $bc, $53, $70, $e6, $14, $17,
    $cc, $7e, $0f, $ff, $69, $52, $bb, $fb, $66, $91, $ec, $8e, $50, $9e, $35,
    $5e, $61, $95, $38, $8c, $6b, $be, $9f, $4b, $55, $da, $85, $1d, $07, $e8,
    $77, $47, $af, $a3, $82, $01, $8f, $30, $82, $01, $8b, $30, $0f, $06, $03,
    $55, $1d, $13, $01, $01, $ff, $04, $05, $30, $03, $01, $01, $ff, $30, $1d,
    $06, $03, $55, $1d, $0e, $04, $16, $04, $14, $da, $39, $a3, $ee, $5e, $6b,
    $4b, $0d, $32, $55, $bf, $ef, $95, $60, $18, $90, $af, $d8, $07, $09, $30,
    $0e, $06, $03, $55, $1d, $0f, $01, $01, $ff, $04, $04, $03, $02, $02, $04,
    $30, $82, $01, $47, $06, $09, $60, $86, $48, $01, $86, $f8, $42, $01, $0d,
    $04, $82, $01, $38, $16, $82, $01, $34, $a5, $ab, $02, $01, $32, $43, $30,
    $31, $5a, $ff, $2d, $5a, $2d, $2d, $30, $09, $06, $07, $2a, $86, $48, $ce,
    $3d, $04, $01, $03, $47, $00, $30, $44, $02, $20, $3d, $5e, $56, $cb, $e2,
    $2a, $1e, $ca, $1b, $bd, $7d, $e7, $a5, $95, $3b, $d0, $aa, $df, $20, $9f,
    $df, $7e, $0f, $ce, $52, $bf, $4e, $f4, $7c, $a9, $3c, $13, $02, $20, $1e,
    $30, $a4, $d6, $65, $d7, $d8, $6c, $04, $c9, $54, $5f, $c1, $b4, $65, $44,
    $18, $65, $d9, $ef, $f5, $88, $62, $fd, $14, $85, $d7, $14, $ad, $93, $e2,
    $46);
  _DUMMYLEN = 702;
  _DUMMYSTUFFLEN = 300; // > 255 since Asn1FixMe() expects $82 lengths

function _CreateDummyCertificate(const Stuff: RawUtf8;
  const CertName: RawUtf8; Marker: cardinal): RawByteString;
var
  dummy: RawByteString;
  len: PtrInt;
  p: PAnsiChar;
  fixme: array[0..6] of PAnsiChar;
begin
  // limitation: CertName is ignored and 'Dummy Cert' is forced
  result := '';
  FastNewRawByteString(dummy, _DUMMYLEN);
  if RleUnCompress(@_DUMMY, pointer(dummy), SizeOf(_DUMMY)) <> _DUMMYLEN then
    exit;
  p := pointer(dummy);
  if (PCardinal(p + 310)^ <> $0102aba5) or
     (PCardinal(p + 318)^ <> $2d2d2d2d) then
    exit;
  fixme[0] := p;
  fixme[1] := p + 4;
  fixme[2] := p + 215;
  fixme[3] := p + 219;
  fixme[4] := p + 287;
  fixme[5] := p + 302;
  fixme[6] := p + 306;
  PCardinal(p + 310)^ := Marker; // but we can overwrite the marker
  len := length(Stuff);
  mormot.core.text.BinToHex(@len, p + 314, 2); // 16-bit hexa len
  Asn1FixMe(@fixme, 7, len - _DUMMYSTUFFLEN, 'CreateDummyCertificate');
  delete(dummy, 319, _DUMMYSTUFFLEN);
  insert(Stuff, dummy, 319);
  result := dummy;
end;

const
  ASN1_INT   = $02;
  ASN1_OBJID = $06;
  ASN1_SEQ   = $30;
  ASN1_SETOF = $31;
  ASN1_CTC0  = $a0;

  _CERTNAME_ = 'Dummy Cert';
  _MARKER_ = $0102aba5;

procedure StuffExeCertificate(const MainFile, NewFile: TFileName;
  const Stuff: RawUtf8; UseInternalCertificate: boolean);
var
  O: THandleStream;
  certoffs, certlenoffs: cardinal;
  certslen, certsend: PtrInt;
  sig, newcert: RawByteString;
  p: PAnsiChar;
  wc: WIN_CERTIFICATE;
  fixme: array[0..3] of PAnsiChar;
begin
  if NewFile = MainFile then
    EStuffExe.RaiseUtf8('MainFile=NewFile=%', [MainFile]);
  if Stuff = '' then
    EStuffExe.RaiseUtf8('Nothing to Stuff in %', [MainFile]);
  if length(Stuff) > 60000 then // encoded as 16-bit hexa (and ASN1_SEQ)
    EStuffExe.RaiseUtf8('Too much data (%) to Stuff within %',
      [KB(Stuff), MainFile]);
  if StrLen(pointer(Stuff)) <> length(Stuff) then
    EStuffExe.RaiseUtf8('Stuff should be pure Text for %', [MainFile]);
  certoffs := 0;
  certlenoffs := 0;
  O := TFileStreamEx.Create(NewFile, fmCreate);
  try
    try
      // copy MainFile source file, parsing the PE header and cert chain
      sig := FindExeCertificate(MainFile, O, wc, @certlenoffs, @certoffs);
      if length(sig) < 4000 then
        EStuffExe.RaiseUtf8('No signature found in %', [MainFile]);
      if length(Stuff) + length(sig) > 64000 then // avoid ASN.1 overflow
        EStuffExe.RaiseUtf8('Too much data (%) to Stuff within %',
          [KB(Stuff), MainFile]);
      if PosEx(_CERTNAME_, sig) <> 0 then
        EStuffExe.RaiseUtf8('% is already stuffed', [MainFile]);
      // parse the original PKCS#7 signed data
      p := pointer(sig);
      fixme[0] := p;
      if Asn1Next(p, ASN1_SEQ, {moveafter=}false, 'SEQ') + 4 > length(sig) then
        EStuffExe.RaiseUtf8('Truncated signature in %', [MainFile]);
      Asn1Next(p, ASN1_OBJID, true, 'OID');
      fixme[1] := p;
      Asn1Next(p, ASN1_CTC0, false, 'ARR');
      fixme[2] := p;
      Asn1Next(p, ASN1_SEQ, false,  'PKCS#7');
      Asn1Next(p, ASN1_INT, true,   'Version');
      Asn1Next(p, ASN1_SETOF, true, 'Digest');
      Asn1Next(p, ASN1_SEQ, true,   'Context');
      fixme[3] := p;
      certslen := Asn1Next(p, ASN1_CTC0, false, 'Certs');
      inc(p, certslen);
      certsend := p - pointer(sig);
      Asn1Next(p, ASN1_SETOF, true, 'SignerInfo');
      if p - pointer(sig) > length(sig) then
        EStuffExe.RaiseUtf8('Wrong cert ending in %', [MainFile]);
      // append the stuffed data within a dummy certificate
      if UseInternalCertificate then
        newcert := _CreateDummyCertificate(Stuff, _CERTNAME_, _MARKER_)
      else // may come from OpenSSL
        newcert := CreateDummyCertificate(Stuff, _CERTNAME_, _MARKER_);
      if newcert = '' then
        EStuffExe.RaiseUtf8('CreateDummyCertificate for %', [MainFile]);
      Asn1FixMe(@fixme, length(fixme), length(newcert), MainFile);
      insert(newcert, sig, certsend + 1);
      // write back the stuffed signature
      wc.dwLength := length(sig) + SizeOf(wc);
      while wc.dwLength and 7 <> 0 do // 64-bit padding
      begin
        inc(wc.dwLength);
        SetLength(sig, length(sig) + 1); // wc.dwLength <> length(sig)
        sig[length(sig)] := #0; // padded with #0
      end;
      O.Seek(certlenoffs, soBeginning); // in PE header
      O.WriteBuffer(wc.dwLength, 4);
      O.Seek(certoffs, soBeginning);    // in the certificate table
      O.WriteBuffer(wc, SizeOf(wc));
      O.WriteBuffer(pointer(sig)^, wc.dwLength - SizeOf(wc));
    except
      DeleteFile(NewFile); // aborted file is clearly invalid
    end;
  finally
    O.Free;
  end;
end;

function FindStuffExeCertificate(const FileName: TFileName): RawUtf8;
var
  wc: WIN_CERTIFICATE;
  i, j, len: PtrInt;
  P: PAnsiChar;
  cert: RawByteString;
begin
  result := '';
  cert := FindExeCertificate(FileName, nil, wc, nil, nil);
  i := PosEx(_CERTNAME_, cert);
  if i = 0 then
    exit;
  P := pointer(Cert);
  for j := i to length(cert) - 16 do
    if PCardinal(P + j)^ = _MARKER_ then
    begin
      len := 0; // length is encoded as 16-bit hexadecimal
      if mormot.core.text.HexToBin(P + j + 4, @len, 2) and
         (len + j + 8 < length(cert)) then
      begin
        FastSetString(result, P + j + 8, len);
        exit;
      end;
    end;
end;

procedure InitializeUnit;
begin
  if not Assigned(@CreateDummyCertificate) then
    CreateDummyCertificate := _CreateDummyCertificate;
end;


initialization
  InitializeUnit;

end.

