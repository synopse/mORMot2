/// Microsoft PE COFF File Reader
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.misc.pecoff;

{
  *****************************************************************************

   ISO 9660 File System Reader, as used for optical disc media
    - Low-Level PE Encoding Structures
    - High-Level PE (.exe, .dll...) File Reader

  *****************************************************************************
}

//{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

{$I ..\mormot.defines.inc}

interface

uses
  Classes, SysUtils, mormot.core.variants;

const

  /// Data directories indexes
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-data-directories-image-only
  IMAGE_DIRECTORY_ENTRY_EXPORT    = 0; // Export Directory
  IMAGE_DIRECTORY_ENTRY_IMPORT    = 1; // Import Directory
  IMAGE_DIRECTORY_ENTRY_RESOURCE  = 2; // Resource Directory
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3; // Exception Directory
  IMAGE_DIRECTORY_ENTRY_SECURITY  = 4; // Security Directory
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5; // Base Relocation Table
  IMAGE_DIRECTORY_ENTRY_DEBUG     = 6; // Debug Directory
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT      = 7; // (X86 usage)
  IMAGE_DIRECTORY_ENTRY_ARCHITECTURE   = 7; // Architecture Specific Data
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR      = 8; // RVA of GP
  IMAGE_DIRECTORY_ENTRY_TLS            = 9; // TLS Directory
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    = 10; // Load Configuration Directory
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   = 11; // Bound Import Directory in headers
  IMAGE_DIRECTORY_ENTRY_IAT            = 12; // Import Address Table
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   = 13; // Delay Load Import Descriptors
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14; // COM Runtime descriptor
  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

  /// Predefined Resource Types
  // https://learn.microsoft.com/en-us/windows/win32/menurc/resource-types
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

  /// Magic numbers
  DOS_HEADER_MAGIC = $5A4D;
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#signature-image-only
  PE_HEADER_MAGIC = $4550;
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-image-only
  PE_32_MAGIC = $10b;
  PE_64_MAGIC = $20b;

type
  ULONG  = cardinal;
  LONG  = longint;
  LPDWORD = ^DWORD;

  { ************ Low-Level DOS/PE/COFF Encoding Structures }

  /// DOS HEADER
  // https://referencesource.microsoft.com/#System.Deployment/System/Deployment/Application/PEStream.cs,74a6abbcc7f5a6da
  _IMAGE_DOS_HEADER = packed record
    e_magic : WORD;
    e_cblp : WORD;
    e_cp : WORD;
    e_crlc : WORD;

    e_cparhdr : WORD;
    e_minalloc : WORD;
    e_maxalloc : WORD;
    e_ss : WORD;

    e_sp : WORD;
    e_csum : WORD;
    e_ip : WORD;
    e_cs : WORD;

    e_lfarlc : WORD;
    e_ovno : WORD;
    e_res : array[0..3] of WORD;
    e_oemid : WORD;
    e_oeminfo : WORD;

    e_res2 : array[0..9] of WORD;
    case boolean of
       true : (e_lfanew : LONG);
       false: (_lfanew : LONG); // delphi naming
  end;
  TImageDOSHeader = _IMAGE_DOS_HEADER;
  PImageDOSHeader = ^_IMAGE_DOS_HEADER;

  /// COFF Header
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#coff-file-header-object-and-image
  _IMAGE_FILE_HEADER = packed record
    Signature: DWORD; // 0x50450000 ('P', 'E', 0, 0) (Not in fpc)
    Machine: WORD;
    NumberOfSections: WORD;
    TimeDateStamp: DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols: DWORD;
    SizeOfOptionalHeader: WORD;
    Characteristics: WORD;
  end;
  TImageFileHeader = _IMAGE_FILE_HEADER;
  PImageFileHeader = ^_IMAGE_FILE_HEADER;

  /// Data directory.
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-data-directories-image-only
  _IMAGE_DATA_DIRECTORY = packed record
    VirtualAddress: DWORD;
    Size: DWORD;
  end;
  TImageDataDirectory = _IMAGE_DATA_DIRECTORY;
  PImageDataDirectory = ^_IMAGE_DATA_DIRECTORY;

  /// Optional Header 32bit
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-standard-fields-image-only
  _IMAGE_OPTIONAL_HEADER = packed record
    //
    // Standard COFF fields.
    //
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    BaseOfData: DWORD;
    //
    // Windows Specific fields.
    //
    ImageBase: DWORD;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: DWORD;
    SizeOfStackCommit: DWORD;
    SizeOfHeapReserve: DWORD;
    SizeOfHeapCommit: DWORD;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;
  TImageOptionalHeader32 = _IMAGE_OPTIONAL_HEADER;
  PImageOptionalHeader32 = ^_IMAGE_OPTIONAL_HEADER;

  /// Optional Header 64bit
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#optional-header-standard-fields-image-only
  _IMAGE_OPTIONAL_HEADER64 = packed record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    ImageBase: Int64;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Int64;
    SizeOfStackCommit: Int64;
    SizeOfHeapReserve: Int64;
    SizeOfHeapCommit: Int64;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;
  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
  PImageOptionalHeader64 = ^_IMAGE_OPTIONAL_HEADER64;

  /// Common OptionalHeader (union for 32/64 bit)
  TImageOptionalHeader = packed record
    case longint of
      0 : (PHeader32: PImageOptionalHeader32);
      1 : (PHeader64: PImageOptionalHeader64);
  end;
  PImageOptionalHeader = ^TImageOptionalHeader;

  /// Complete PE Header
  // - COFF Header + Optional Header
  // - 32bit version
  _IMAGE_NT_HEADERS = packed record
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader32;
  end;
  TImageNtHeaders32 = _IMAGE_NT_HEADERS;
  PImageNtHeaders32 = ^_IMAGE_NT_HEADERS;

  /// Complete PE Header
  // - COFF Header + Optional Header
  // - 64bit version
  _IMAGE_NT_HEADERS64 = packed record
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader64;
  end;
  TImageNtHeaders64 = _IMAGE_NT_HEADERS64;
  PImageNtHeaders64 = ^_IMAGE_NT_HEADERS64;

  /// Common NtHeaders (union for 32/64 bit)
  TImageNtHeaders = packed record
    case longint of
      0 : (PHeaders32: PImageNtHeaders32);
      1 : (PHeaders64: PImageNtHeaders64);
      2 : (PHeaders: pointer);
  end;
  PImageNtHeaders = ^TImageNtHeaders;

  /// Section Table
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#section-table-section-headers
  _IMAGE_SECTION_HEADER = packed record
    Name : packed array[0..7] of char;
    VirtualSize : DWORD;
    VirtualAddress : DWORD;
    SizeOfRawData : DWORD;
    PointerToRawData : DWORD;
    PointerToRelocations : DWORD;
    PointerToLinenumbers : DWORD;
    NumberOfRelocations : WORD;
    NumberOfLinenumbers : WORD;
    Characteristics : DWORD;
   end;
   TImageSectionHeader = _IMAGE_SECTION_HEADER;
   PImageSectionHeader = ^_IMAGE_SECTION_HEADER;

  /// .reloc Section header
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#coff-relocations-object-only
  TImageBaseRelocation=packed record
    VirtualAddress  : DWORD;
    SymbolTableIndex: DWORD;
    RelocType       : WORD;
  end;

  /// .edata Section header
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#export-directory-table
  _IMAGE_EXPORT_DIRECTORY = packed record
      Characteristics : DWORD;
      TimeDateStamp   : DWORD;
      MajorVersion    : WORD;
      MinorVersion    : WORD;
      Name 	        : DWORD;
      Base 		    : DWORD;
      NumberOfFunctions : DWORD;
      NumberOfNames   : DWORD;
      AddressOfFunctions : DWORD;     { RVA from base of image }
      AddressOfNames  : DWORD;        { RVA from base of image }
      AddressOfNameOrdinals : DWORD;  { RVA from base of image }
      //ForAligmentBuf : array[1..16] of byte;
    end;
  TImageExportDirectory = _IMAGE_EXPORT_DIRECTORY;
  PImageExportDirectory = ^_IMAGE_EXPORT_DIRECTORY;

  /// .idata Section Header
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#import-directory-table
  _IMAGE_IMPORT_DESCRIPTOR =  packed record
          case longint of
            0 : ( Characteristics : DWORD );     { 0 for terminating null import descriptor }
            1 : ( OriginalFirstThunk : DWORD;    { RVA to original unbound IAT (PIMAGE_THUNK_DATA) }
                  TimeDateStamp : DWORD;         { 0 if not bound, }
                                                 // -1 if bound, and real date\time stamp
                                                 //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                                 // O.W. date/time stamp of DLL bound to (Old BIND)
                  ForwarderChain : DWORD;        // -1 if no forwarders
                  Name : DWORD;
                  FirstThunk : DWORD;            // RVA to IAT (if bound this IAT has actual addresses)
                );
    end;
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  PImageImportDescriptor = ^_IMAGE_IMPORT_DESCRIPTOR;

  /// .tls Section Header
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#the-tls-directory
  _IMAGE_TLS_DIRECTORY32 =  packed record
      StartAddressOfRawData : DWORD;
      EndAddressOfRawData : DWORD;
      AddressOfIndex : DWORD;                      { PDWORD }
      AddressOfCallBacks : DWORD;                  { PIMAGE_TLS_CALLBACK * }
      SizeOfZeroFill : DWORD;
          case longint of
            0 : ( Characteristics : DWORD );
            1 : ( CharacteristicsFields : bitpacked  record
                                 Reserved0 : 0..$FFFFF; // 5 nibbles=20 bits
                                 Alignment : 0..$F;      // 4 bits
                                 Reserved1 : 0..$FF;     // 8 bits
              end );

    end;
  TImageTSLDirectory = _IMAGE_TLS_DIRECTORY32;
  PImageTSLDirectory = ^_IMAGE_TLS_DIRECTORY32;

  /// .rsrc Section header
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#resource-directory-table
  PImageResourceDirectoryEntry = ^_IMAGE_RESOURCE_DIRECTORY_ENTRY;
  _IMAGE_RESOURCE_DIRECTORY = packed record
  private
    function GetEntry(Index: Integer): PImageResourceDirectoryEntry;
  public
    Characteristics : DWORD;
    TimeDateStamp   : DWORD;
    MajorVersion    : WORD;
    MinorVersion    : WORD;
    NumberOfNamedEntries: WORD;
    NumberOfIdEntries: WORD;

    /// Get the total number of entries
    // - Sum of Named entries and id entries
    // - High boundary of Entries indexes
    function NumberOfEntries: DWORD;
    /// Get the entry at the given index
    // - Return nil if out of bounds
    property Entries[Index: Integer]: PImageResourceDirectoryEntry read GetEntry;
  end;
  TImageResourceDirectory = _IMAGE_RESOURCE_DIRECTORY;
  PImageResourceDirectory = ^_IMAGE_RESOURCE_DIRECTORY;

  /// Resource directory entries
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#resource-directory-entries
  PImageResourceDataEntry = ^_IMAGE_RESOURCE_DATA_ENTRY;
  _IMAGE_RESOURCE_DIRECTORY_ENTRY = packed record
    // Nested record to use a variable part not at the end of the record
    Identifier: record
      case longint of
        0 : (NameOffset : DWord);
        1 : (Id : DWord);
    end;
    OffsetToData: DWord;

    /// Check if the entry is a directory entry or a data entry
    // - An entry is a directory entry if the high bit of OffsetToData is set
    function IsDirectory: Boolean;
    /// Get the offset to the directory
    function OffsetToDirectory: DWord;
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
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#resource-data-entry
  _IMAGE_RESOURCE_DATA_ENTRY = packed record
    DataRVA: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;
  TImageResourceDataEntry = _IMAGE_RESOURCE_DATA_ENTRY;

  /// Version information
  // https://learn.microsoft.com/en-us/windows/win32/menurc/vs-versioninfo
  _VS_VERSIONINFO = packed record
    Length: WORD;
    ValueLength: WORD;
    VersionType: WORD;
  end;
  TVsVersionInfo = _VS_VERSIONINFO;
  PVsVersionInfo = ^_VS_VERSIONINFO;

  /// Fixed file info
  // https://learn.microsoft.com/en-us/windows/win32/api/VerRsrc/ns-verrsrc-vs_fixedfileinfo
  _VS_FIXEDFILEINFO = packed record
    Signature: DWORD;
    StructVersion: DWORD;
    FileVersionMS: DWORD;
    FileVersionLS: DWORD;
    ProductVersionMS: DWORD;
    ProductVersionLS: DWORD;
    FileFlagsMask: DWORD;
    FileFlags: DWORD;
    FileOS: DWORD;
    FileType: DWORD;
    FileSubtype: DWORD;
    FileDateMS: DWORD;
    FileDateLS: DWORD;

    /// Get the file major version number
    function FileMajorVersion: DWORD;
    /// Get the file minor version number
    function FileMinorVersion: DWORD;
    /// Get the file patch version number
    function FilePatchVersion: DWORD;
    /// Get the file build version number
    function FileBuildVersion: DWORD;
    /// Get the file version as a string
    // - Format is [major].[minor].[patch].[build]
    function FileVersionStr: String;
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

  /// String struct, same as VS_VERSIONINFO
  // https://learn.microsoft.com/en-us/windows/win32/menurc/string-str
  TStringTableEntry = TVsVersionInfo;
  PStringTableEntry = ^TStringTableEntry;

  /// VarFileInfo struct, same as VS_VERSIONINFO
  // https://learn.microsoft.com/en-us/windows/win32/menurc/varfileinfo
  TVarFileInfo = TVsVersionInfo;
  PVarFileInfo = ^TVarFileInfo;


  { ************ High-Level PE (.exe, .dll...) File Reader }

  { TSynPELoader }

  /// Cross platform PE (Portable Executable) file parser
  // see @https://learn.microsoft.com/en-us/windows/win32/debug/pe-format
  // - the file is loaded in memory and most of the getters will return pointer
  // to the file memory instead of copy, however some data is cached when parsed
  // to simplify later access (like StringFileInfoEntries)
  // - when parsing specific sections (like ParseResources) pointers to the inner
  // data are saved and later accessible using associated properties. Some data
  TSynPELoader = class
  private
    /// Saved pointers
    // - Headers
    fPEHeader: TImageNtHeaders;
    // - Resource Section pointers
    fVersionInfo: PVsVersionInfo;
    fFixedFileInfo: PVSFixedFileInfo;
    fStringFileInfo : PStringFileInfo;
    fFirstStringTable: PStringTable;
    fVarFileInfo: PVarFileInfo;
    /// Cached data
    // - Resource Section data
    fStringFileInfoEntries: TDocVariantData;
    /// raw PE File
    FS : TMemoryStream;

    function GetFileHeader: PImageFileHeader;
    function GetImageDataDirectory(DirectoryId: Word): PImageDataDirectory;
    function GetNumberOfSections: WORD;
    function GetPEHeader32: PImageNtHeaders32;
    function GetPEHeader64: PImageNtHeaders64;
    function GetSectionHeader(SectionId: Word): PImageSectionHeader;
    function GetSectionHeadersStart: PImageSectionHeader;
    function GetStartAddress: pointer;
    /// Align an offset with a base address
    // - Resulting offset is the first aligned offset starting from the input offset.
    // - An offset is aligned if Base - Offset is a DWord size (4 bytes) multiple
    function DWordAlign(Offset: DWORD; Base: DWORD = 0): DWORD;
    /// Parse a StringFileInfo or VarFileInfo struct.
    // - Address is the starting address of a StringFileInfo/VarFileInfo struct
    // - Set fStringFileInfo or fVarFileInfo depending on the struct at the given address
    // - Returns the end address of the file info
    // - Called by ParseResources
    function ParseFileInfo(Address: Pointer): Pointer;
  public
    /// Constructor which initialize all saved pointers to nil
    constructor Create;
    /// Destructor which unload the current file
    destructor Destroy; override;
    /// Load a PE file in memory
    // - Return true if the file has been successfully loaded
    function LoadFromFile(Filename : string) : boolean;
    /// Unload the current file in memory
    // - Can be called even if no file is loaded
    procedure Unload;

    /// Search the section containing the given RVA
    // - Return the section index (see SectionHeaders property)
    // - If no section is found, return -1
    function GetSectionIndexByRVA(RVA : DWORD) : integer;
    /// Search the section named AName
    // - Return the section index (see SectionHeaders property)
    // - If no section is found, return -1
    function GetSectionIndexByName(const AName : string) : integer;
    /// Search the section associated to the given directory
    // see IMAGE_DIRECTORY_ENTRY_* consts
    // - Return the section index (see SectionHeaders property)
    // - If no section is found, return -1
    function GetSectionIndexFromDirectory(DirectoryId: DWORD): integer;

    /// Translate RVA to physical address
    // - ASectionId is the section containing the RVA
    // - Doesn't verify section id, an invalid section id will lead to access violation
    // - Return the physical address, ie the offset from the file first byte
    function GetPhAddByRVA(RVA : DWORD; ASectionId : DWORD) : DWORD; overload;
    /// Translate RVA to physical address
    // - Return the physical address, ie the offset from the file first byte
    // - If the RVA is not contained by any section, 0 is returned
    function GetPhAddByRVA(RVA : DWORD) : DWORD; overload;

    /// Parse the Resource directory associated section
    // see https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#the-rsrc-section
    // - Return False if there is no resource section
    // - Set VS_VERSIONINFO, VS_FIXEDFILEINFO, StringFileInfo, FirstStringTable
    // VarFileInfo pointers if found
    function ParseResources: Boolean;
    /// Parse the StringFileInfo entries
    // - Call ParseResources if StringFileInfo is nil before returning false if
    // it is still nil
    // - Parsed entries are accessible using the StringFileInfoEntries property
    function ParseStringFileInfoEntries: Boolean;
    /// Check whether there is a valid PE file loaded
    function IsLoaded: Boolean;
    /// Check whether the PE file is an x64 arch
    function Is64: Boolean;
    /// Number of sections
    // - It is the high boundary of the SectionHeaders property
    property NumberOfSections: WORD read GetNumberOfSections;

    /// Adress to the first byte of the file
    property StartAddress: pointer read GetStartAddress;
    /// PE Header pointer for 32bit arch
    // - See Is64 to check arch
    // - Regroup the COFF Header and the Optional Header
    property PEHeader32: PImageNtHeaders32 read GetPEHeader32;
    /// PE Header pointer for 64bit arch
    // - See Is64 to check arch
    // - Regroup the COFF Header and the Optional Header
    property PEHeader64: PImageNtHeaders64 read GetPEHeader64;
    /// COFF Header (start of PE Header)
    property COFFHeader: PImageFileHeader read GetFileHeader;
    /// Get the image data directory struct at the given id
    // - See IMAGE_DIRECTORY_ENTRY_* consts
    // - Return -1 if the DirectoryId is out of bounds
    property ImageDataDirectory[DirectoryId: Word]: PImageDataDirectory read GetImageDataDirectory;
    /// First image section header (end of PE Header)
    property SectionHeadersStart: PImageSectionHeader read GetSectionHeadersStart;
    /// Get the section header at the given section id
    // - Return nil if the id is out of bounds
    property SectionHeaders[SectionId: Word]: PImageSectionHeader read GetSectionHeader;

    /// Resources pointers, set by ParseResources
    property VS_VERSIONINFO: PVsVersionInfo read fVersionInfo;
    property VS_FIXEDFILEINFO: PVSFixedFileInfo read fFixedFileInfo;
    property StringFileInfo: PStringFileInfo read fStringFileInfo;
    property FirstStringTable: PStringTable read fFirstStringTable;
    property VarFileInfo: PVarFileInfo read fVarFileInfo;
    /// Parsed StringFileInfo entries
    // - Populated by ParseStringFileInfo
    property StringFileInfoEntries: TDocVariantData read fStringFileInfoEntries;
  end;

implementation

uses
  mormot.core.unicode;

{ ************ Low-Level DOS/PE/COFF Encoding Structures }

{ _VS_FIXEDFILEINFO }

function _VS_FIXEDFILEINFO.FileMajorVersion: DWORD;
begin
  Result := FileVersionMS shr 16;
end;

function _VS_FIXEDFILEINFO.FileMinorVersion: DWORD;
begin
  Result := FileVersionMS and $ffff;
end;

function _VS_FIXEDFILEINFO.FilePatchVersion: DWORD;
begin
  Result := FileVersionLS shr 16;
end;

function _VS_FIXEDFILEINFO.FileBuildVersion: DWORD;
begin
  Result := FileVersionLS and $ffff;
end;

function _VS_FIXEDFILEINFO.FileVersionStr: String;
begin
  Result := IntToStr(FileMajorVersion) + '.' +
            IntToStr(FileMinorVersion) + '.' +
            IntToStr(FilePatchVersion) + '.' +
            IntToStr(FileBuildVersion);
end;

{ _IMAGE_RESOURCE_DIRECTORY }

function _IMAGE_RESOURCE_DIRECTORY.GetEntry(Index: Integer
  ): PImageResourceDirectoryEntry;
begin
  if (Index < 0) or (DWORD(Index) >= NumberOfEntries) then
    Result := nil
  else
    Result := pointer(@Self) + sizeof(Self) + Index * sizeof(TImageResourceDirectoryEntry);
end;

function _IMAGE_RESOURCE_DIRECTORY.NumberOfEntries: DWORD;
begin
  Result := NumberOfIdEntries + NumberOfNamedEntries;
end;

{ _IMAGE_RESOURCE_DIRECTORY_ENTRY }

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.IsDirectory: Boolean;
begin
  Result := ((OffsetToData and $80000000) shr 31) = 1;
end;

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.OffsetToDirectory: DWord;
begin
  Result := OffsetToData and $7fffffff;
end;

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.Directory(StartAddress: pointer
  ): PImageResourceDirectory;
begin
  if not IsDirectory then
    Result := nil
  else
    Result := StartAddress + OffsetToDirectory;
end;

function _IMAGE_RESOURCE_DIRECTORY_ENTRY.Data(StartAddress: pointer
  ): PImageResourceDataEntry;
begin
  if IsDirectory then
    Result := nil
  else
    Result := StartAddress + OffsetToData;
end;

{ ************ High-Level PE (.exe, .dll...) File Reader }

{ TSynPELoader }

function TSynPELoader.GetNumberOfSections: WORD;
begin
  if not IsLoaded then
    Result := 0
  else
    Result := COFFHeader^.NumberOfSections;
end;

function TSynPELoader.GetPEHeader32: PImageNtHeaders32;
begin
  Result := fPEHeader.PHeaders32;
end;

function TSynPELoader.GetPEHeader64: PImageNtHeaders64;
begin
  Result := fPEHeader.PHeaders64;
end;

function TSynPELoader.GetSectionHeader(SectionId: Word): PImageSectionHeader;
begin
  if (not IsLoaded) or (SectionId >= GetNumberOfSections) then
    Result := nil
  else
    Result := pointer(SectionHeadersStart) + sizeof(TImageSectionHeader) * SectionId;
end;

function TSynPELoader.GetSectionHeadersStart: PImageSectionHeader;
begin
  Result := fPEHeader.PHeaders + sizeof(COFFHeader^) + COFFHeader^.SizeOfOptionalHeader;
end;

function TSynPELoader.DWordAlign(Offset: DWORD; Base: DWORD): DWORD;
begin
  Result := ((Offset + Base + 3) and $FFFFFFFC) - (base and $FFFFFFFC);
end;

constructor TSynPELoader.Create;
begin
  FS := TMemoryStream.Create;
  // Needed because Unload calls Clear, and clear without init cause undefined behavior
  fStringFileInfoEntries.InitObject([], JSON_FAST);
  // Initialize all pointers to nil
  Unload;
end;

destructor TSynPELoader.Destroy;
begin
  Unload;
  FS.Free;
  inherited Destroy;
end;

procedure TSynPELoader.Unload;
begin
  FS.Clear;

  fVersionInfo := nil;
  fFixedFileInfo := nil;
  fStringFileInfo := nil;
  fFirstStringTable := nil;
  fVarFileInfo := nil;
  fStringFileInfoEntries.Clear;
  fStringFileInfoEntries.InitObject([], JSON_FAST);
end;

function TSynPELoader.GetSectionIndexByRVA(RVA: DWORD): integer;
var
  i : integer;
begin
  Result := -1;
  if not IsLoaded then
    Exit;

  for i := 0 to NumberOfSections - 1 do
    if (RVA >= SectionHeaders[i]^.VirtualAddress) and
       (RVA < SectionHeaders[i]^.VirtualAddress + SectionHeaders[i]^.VirtualSize) then
    begin
      Result := i;
      Break;
    end;
end;

function TSynPELoader.GetSectionIndexByName(const AName: string): integer;
var
  i : integer;
  LowerName: String;
begin
  Result := -1;
  if not IsLoaded then
    Exit;

  LowerName := Trim(LowerCase(AName));
  for i := 0 to NumberOfSections - 1 do
    if (LowerName = LowerCase(Trim(SectionHeaders[i]^.Name))) then
    begin
      Result := i;
      Break;
    end;
end;

function TSynPELoader.GetSectionIndexFromDirectory(DirectoryId: DWORD): integer;
begin
  if DirectoryId >= IMAGE_NUMBEROF_DIRECTORY_ENTRIES then
    Result := -1
  else
    Result := GetSectionIndexByRVA(ImageDataDirectory[DirectoryId]^.VirtualAddress);
end;

function TSynPELoader.GetStartAddress: pointer;
begin
  if not IsLoaded then
    Result := nil
  else
    Result := FS.Memory;
end;

function TSynPELoader.ParseResources: Boolean;
var
  Directory, VersionEntriesDir: PImageResourceDirectory;
  EntryId, i: Integer;
  Entry: PImageResourceDirectoryEntry;
  VersionData: PImageResourceDataEntry;
  VersionInfoStr: PWideChar;
  ResourceSct: PImageSectionHeader;
  NextAddress: Pointer;
begin
  ResourceSct := SectionHeaders[GetSectionIndexFromDirectory(IMAGE_DIRECTORY_ENTRY_RESOURCE)];
  if not Assigned(ResourceSct) then
    Exit(False);
  Directory := StartAddress + ResourceSct^.PointerToRawData;
  Result := True;

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
        fVersionInfo  := StartAddress + GetPhAddByRVA(VersionData^.DataRVA);
        // 'VS_VERSION_INFO', Strings are UTF-16 which explain the 2 * Length (2 bytes per char) in later offset
        VersionInfoStr := pointer(VS_VERSIONINFO) + sizeof(VS_VERSIONINFO^);
        // Invalid Entry
        if UnicodeBufferToUtf8(VersionInfoStr) <> 'VS_VERSION_INFO' then
          break;

         // Fixed File Info
         fFixedFileInfo := pointer(VS_VERSIONINFO) +
                           DWordAlign(SizeOf(VS_VERSIONINFO^) + 2 * (Length(VersionInfoStr) + 1), VersionData^.DataRVA);

         // String File Info / Var File Info
         NextAddress := pointer(VS_FIXEDFILEINFO) + sizeof(fFixedFileInfo^);
         while NextAddress <> nil do
           NextAddress := ParseFileInfo(NextAddress);
      end;
    end;
  end;
end;

function TSynPELoader.ParseStringFileInfoEntries: Boolean;
var
  StringTable: PStringTable;
  LangID, Key, Value: PWideChar;
  Offset: LongInt;
  StrEntry: PStringTableEntry;
begin
  if StringFileInfo = nil then
    ParseResources;
  if (StringFileInfo = nil) or (FirstStringTable = nil) then
    Exit(False);
  Result := True;

  Offset := pointer(StringFileInfo) - pointer(VS_VERSIONINFO);
  StringTable := FirstStringTable;
  while True do
  begin
    LangID := pointer(StringTable) + sizeof(StringTable^);
    StrEntry := pointer(StringTable) +
                DWordAlign(SizeOf(StringTable^) + 2 * (Length(LangID) + 1), Offset);

    while pointer(StrEntry) < (pointer(StringTable) + StringTable^.Length) do
    begin
      Key := pointer(StrEntry) + SizeOf(StrEntry^);
      Value := pointer(StrEntry) +
               DWordAlign(SizeOf(StrEntry^) + 2 * (Length(Key) + 1), Offset);
      fStringFileInfoEntries[UnicodeBufferToUtf8(Key)] := UnicodeBufferToUtf8(Value);

      if StrEntry^.Length = 0 then
        StrEntry := pointer(StringTable) + StringTable^.Length // End the while condition
      else
        StrEntry := pointer(StrEntry) + DWordAlign(StrEntry^.Length, Offset);
      Value := nil;
    end;
    if StringTable^.Length = 0 then
      break;
    StringTable := pointer(StringTable) + DWordAlign(StringTable^.Length, Offset);
    if pointer(StringTable) >= (pointer(StringFileInfo) + StringFileInfo^.Length) then
      break;
  end;
end;

function TSynPELoader.ParseFileInfo(Address: Pointer): Pointer;
var
  FileInfoStruct: PVsVersionInfo;
  FileInfoStr: PWideChar;
  Offset: DWORD;
begin
  // String File Info / Var File Info
  Offset := pointer(VS_VERSIONINFO) - Address;
  FileInfoStruct := Address;
  FileInfoStr := pointer(fileInfoStruct) + sizeof(StringFileInfo^);

  if UnicodeBufferToUtf8(FileInfoStr) = 'StringFileInfo' then
  begin
    fStringFileInfo := FileInfoStruct;
    fFirstStringTable := pointer(StringFileInfo) +
                         DWordAlign(Sizeof(StringFileInfo^) + 2* (Length(FileInfoStr) + 1), Offset);
  end
  else if UnicodeBufferToUtf8(FileInfoStr) = 'VarFileInfo' then
    fVarFileInfo := FileInfoStruct;

  if (FileInfoStruct^.Length = 0) or
     ((Address + FileInfoStruct^.Length) >= (pointer(VS_VERSIONINFO) + VS_VERSIONINFO^.Length)) then
    Result := nil
  else
    Result := Address + FileInfoStruct^.Length;
end;

function TSynPELoader.GetPhAddByRVA(RVA: DWORD; ASectionId: DWORD): DWORD;
begin
 Result := RVA - SectionHeaders[ASectionId]^.VirtualAddress +
           SectionHeaders[ASectionId]^.PointerToRawData;
end;

function TSynPELoader.GetPhAddByRVA(RVA: DWORD): DWORD;
var
  SectionID : integer;
begin
  Result := 0;
  SectionID := GetSectionIndexByRVA(RVA);
  if SectionID < 0 then
    Exit;
  Result := GetPhAddByRVA(RVA, SectionID);
end;

function TSynPELoader.LoadFromFile(Filename: string): boolean;
var
  TempFS: TFileStream;
  DOSHeader: PImageDOSHeader;
begin
  Result := False;
  // Unloading the previous PE
  UnLoad;
  try
    TempFS  := TFileStream.Create(Filename, fmOpenRead  or fmShareDenyNone);
  except
    exit;
  end;
  try
    try
      FS.LoadFromStream(TempFS);
    except
      exit;
    end;
  finally
    TempFS.Free;
    TempFS := nil;
  end;
  // Move to the beginning of the file and load the header into the buffer
  FS.Seek(0,0);

  // Can't contain PE Header address (at end of DOS Header)
  if FS.Size < SizeOf(TImageDOSHeader) then
  begin
    Unload;
    Exit;
  end;

  DOSHeader := FS.Memory;
  // Invalid DOS Header magic number
  if DOSHeader^.e_magic <> DOS_HEADER_MAGIC then
  begin
    Unload;
    Exit;
  end;

  // e_lfanew is Pointer to PE Header (0x3c after start of file)
  // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#signature-image-only
  fPEHeader.PHeaders := StartAddress + DOSHeader^.e_lfanew;
  // Invalid PE Header Magic number
  // Or Invalid Optional header magic number (must be 0x10b in 32bit, 0x20b in 64bit)
  if (COFFHeader^.Signature <> PE_HEADER_MAGIC) or
     ((COFFHeader^.SizeOfOptionalHeader > 0) and (PEHeader32^.OptionalHeader.Magic <> PE_32_MAGIC) and
                                                 (PEHeader32^.OptionalHeader.Magic <> PE_64_MAGIC)) then
  begin
    Unload;
    Exit;
  end;
  Result := True;
end;

function TSynPELoader.IsLoaded: Boolean;
begin
  Result := FS.Size > 0;
end;

function TSynPELoader.Is64: Boolean;
begin
  Result := PEHeader32^.OptionalHeader.Magic = PE_64_MAGIC;
end;

function TSynPELoader.GetImageDataDirectory(DirectoryId: Word
  ): PImageDataDirectory;
begin
  if DirectoryId >= IMAGE_NUMBEROF_DIRECTORY_ENTRIES then
    Exit(nil);
  if Is64 then
    Result := @PEHeader64^.OptionalHeader.DataDirectory[DirectoryId]
  else
    Result := @PEHeader32^.OptionalHeader.DataDirectory[DirectoryId];
end;

function TSynPELoader.GetFileHeader: PImageFileHeader;
begin
  // FileHeader doesn't change in 32/64bit
  Result := @PEHeader32^.FileHeader;
end;

end.

