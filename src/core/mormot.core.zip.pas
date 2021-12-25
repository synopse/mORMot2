/// Framework Core Zip/Deflate Compression Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.zip;

{
  *****************************************************************************

   High-Level Zip/Deflate Compression features shared by all framework units
    - TSynZipCompressor Stream Class
    - GZ Read/Write Support
    - .ZIP Archive File Support
    - TAlgoDeflate and TAlgoDeflate High-Level Compression Algorithms

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers, // for TAlgoCompress
  mormot.lib.z;


{ ************ TSynZipCompressor Stream Class }

type
  /// exception type raised by this unit
  ESynZip = class(ESynException);

  /// the format used for storing data
  TSynZipCompressorFormat = (
    szcfRaw,
    szcfZip,
    szcfGZ);

  /// abstract TStream as inherited by TSynZipCompressor/TSynZipDecompressor
  TSynZipStream = class(TStream)
  protected
    fInitialized: boolean;
    fFormat: TSynZipCompressorFormat;
    fDestStream: TStream;
    Z: TZLib;
    fCRC: cardinal;
    fSizeIn, fSizeOut: Int64;
    {$ifdef FPC}
    function GetPosition: Int64; override;
    {$endif FPC}
    function GetSize: Int64; override;
  public
    /// this method will raise an error: it's a compression-only stream
    function Read(var Buffer; Count: Longint): Longint; override;
    /// used to return the current position, i.e. the real byte written count
    // - for real seek, this method will raise an error: it's a compression-only stream
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// used to return the current position, i.e. the real byte written count
    // - for real seek, this method will raise an error: it's a compression-only stream
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    /// the number of byte read, i.e. the current (un)compressed size
    property SizeIn: Int64
      read fSizeIn;
    /// the number of byte sent to the destination stream, i.e. the current
    // (un)compressed size
    property SizeOut: Int64
      read fSizeOut;
    /// the current crc32 of the read or written data, i.e. the uncompressed CRC
    property CRC: cardinal
      read fCRC;
  end;

  /// a TStream descendant for compressing data into a stream using Zip/Deflate
  // - supports forward only Write() compression
  TSynZipCompressor = class(TSynZipStream)
  public
    /// create a compression stream, writting the compressed data into
    // the specified stream (e.g. a file stream)
    constructor Create(outStream: TStream; CompressionLevel: integer;
      Format: TSynZipCompressorFormat = szcfRaw);
    /// release memory
    destructor Destroy; override;
    /// update the global CRC and compress some data
    function Write(const Buffer; Count: Longint): Longint; override;
    /// write all pending compressed data into outStream
    procedure Flush;
  end;

  /// a TStream descendant for uncompressing data into a stream using Zip/Deflate
  // - supports forward only Write() decompression
  TSynZipDecompressor = class(TSynZipStream)
  public
    /// create a decompression stream, writting the uncompressed data into
    // the specified stream (e.g. a file stream)
    // - only supported formats are szcfRaw and szcfZip (not szcfGZ)
    constructor Create(outStream: TStream;
      Format: TSynZipCompressorFormat = szcfRaw);
    /// release memory
    destructor Destroy; override;
    /// decompress some data and update the global CRC
    function Write(const Buffer; Count: Longint): Longint; override;
    /// write all pending uncompressed data into outStream
    procedure Flush;
  end;

{ ************  GZ Read/Write Support }

type
  /// simple wrapper class to decompress a .gz file into memory or stream/file
  TGZRead = object
  private
    comp, zsdest: pointer;
    zscrc: cardinal;
    zssize, zscode: integer;
    z: TZLib;
  public
    complen: PtrInt;
    uncomplen32: cardinal; // modulo 2^32 by gzip design
    crc32: cardinal;
    unixmodtime: cardinal;
    fname, fcomment, extra: PAnsiChar;
    /// read and validate the .gz header
    // - on success, return true and fill complen/uncomplen/crc32c properties
    function Init(gz: PAnsiChar; gzLen: PtrInt): boolean;
    /// uncompress the .gz content into a memory buffer
    // - warning: won't work as expected if uncomplen32 was truncated to 2^32
    function ToMem: RawByteString;
    /// uncompress the .gz content into a stream
    function ToStream(stream: TStream; tempBufSize: integer = 0): boolean;
    /// uncompress the .gz content into a file
    function ToFile(const filename: TFileName; tempBufSize: integer = 0): boolean;
    /// allow low level iterative decompression using an internal TZLib structure
    function ZStreamStart(dest: pointer; destsize: integer): boolean;
    /// return true if ZStreamStart() has been successfully called
    function ZStreamStarted: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// will uncompress into dest/destsize buffer as supplied to ZStreamStart
    // - return the number of bytes uncompressed (<=destsize)
    // - return 0 if the input stream is finished
    function ZStreamNext: integer;
    /// any successfull call to ZStreamStart should always run ZStreamDone
    // - return true if the crc and the uncompressed size are ok
    function ZStreamDone: boolean;
  end;


/// uncompress a .gz file content
// - return '' if the .gz content is invalid (e.g. bad crc)
function GZRead(gz: PAnsiChar; gzLen: integer): RawByteString;

/// compress a file content into a new .gz file
// - will use TSynZipCompressor for minimal memory use during file compression
function GZFile(const orig, destgz: TFileName; CompressionLevel: integer = 6): boolean;


{ ************  .ZIP Archive File Support }

{$A-}

type
  PFileInfo = ^TFileInfo;

  /// generic file information structure, as used in .zip file format
  // - used in any header, contains info about following block
  TFileInfo = object
    /// ZIP_VERSION[] is either 20 for regular .zip or 45 for Zip64/4.5
    // - use ToByte() for the format version - high 8-bit may identify the OS
    neededVersion: word;
    /// 0
    flags: word;
    /// 0=Z_STORED 8=Z_DEFLATED 12=BZ2 14=LZMA
    zzipMethod: word;
    /// time in DOS 32-bit encoding
    zlastMod: integer;
    /// crc32 checksum of uncompressed data
    zcrc32: cardinal;
    /// 32-bit size of compressed data
    zzipSize: cardinal;
    /// 32-bit size of uncompressed data
    zfullSize: cardinal;
    /// length(name) as appended just after this block
    nameLen: word;
    /// length(extra) - e.g. SizeOf(TFileInfoExtra64)
    extraLen: word;
    /// guess/comparison of the content of two TFileInfo - check zcrc32 + sizes
    function SameAs(aInfo: PFileInfo): boolean;
    /// set the the UTF-Language encoding flag (EFS)
    procedure SetUtf8FileName;
      {$ifdef HASINLINE}inline;{$endif}
    /// remove the the UTF-Language encoding flag (EFS)
    procedure UnSetUtf8FileName;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if flags contains the UTF-Language encoding flag (EFS)
    function GetUtf8FileName: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// set our custom proprietary algorithm used
    // - 1..15  (1=SynLZ e.g.) from flags bits 7..10 and method=Z_STORED
    procedure SetAlgoID(Algorithm: integer);
    /// retrieve our custom proprietary algorithm used
    function GetAlgoID: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if neededVersion was created from ZIP_VERSION[{zip64=}true]
    function IsZip64: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  //// extra file information structure, as used in zip64 file format
  TFileInfoExtra64 = record
    /// ZIP64_EXTRA_ID = 0x0001
    zip64id: word;
    /// = 16 (2*QWord) after local info, = 24 including offset in directory
    size: word;
    /// 64-bit size of uncompressed data
    zfullSize: QWord;
    /// 64-bit size of compressed data
    zzipSize: QWord;
    /// 64-bit offset of the data
    // - not stored after local info, but only after trailing TFileHeader
    offset: QWord;
  end;
  PFileInfoExtra64 = ^TFileInfoExtra64;

  /// information returned by RetrieveFileInfo()
  TFileInfoFull = record
    /// standard zip file information
    // - if f32.IsZip64 is true, f64 has been populated
    f32: TFileInfo;
    /// zip64 extended information
    // - those zzipSize/zfullSize fields are to be taken into consideration
    f64: TFileInfoExtra64;
    /// actual size of the stored local file header
    localfileheadersize: PtrInt;
  end;

  //// directory file information structure, as used in .zip file format
  // - used at the end of the zip file to recap all entries
  TFileHeader = object
    /// $02014b50 PK#1#2 = ENTRY_SIGNATURE_INC - 1
    signature: cardinal;
    /// ZIP_VERSION[] is either 20 for regular .zip or 45 for Zip64/4.5
    madeBy: word;
    /// file information - copied from the TLocalFileHeader before each data
    fileInfo: TFileInfo;
    /// 0
    commentLen: word;
    /// 0
    firstDiskNo: word;
    /// 0 = binary; 1 = text
    intFileAttr: word;
    /// dos file attributes
    extFileAttr: cardinal;
    /// 32-bit offset to @TLocalFileHeader
    localHeadOff: cardinal;
    /// check if extFileAttr contains the folder flag (bit 4)
    function IsFolder: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize the signature/madeby/fileinfo field information
    procedure SetVersion(NeedZip64: boolean);
  end;
  PFileHeader = ^TFileHeader;

  //// internal file information structure, as used in .zip file format
  // - used locally inside the file stream, followed by the name and the data
  TLocalFileHeader = object
    /// $04034b50 PK#3#4 = FIRSTHEADER_SIGNATURE_INC - 1
    signature: cardinal;
    /// information about the following file
    fileInfo: TFileInfo;
    /// returns the stored header size
    // - i.e. SizeOf(self) + fileInfo.extraLen + fileInfo.nameLen bytes
    function Size: PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// point to the data part of this PLocalFileHeader
    // - jump over fileInfo.extraLen + fileInfo.nameLen bytes
    function Data: PAnsiChar;
    /// move Source position to the data part of this file content
    // - jump over fileInfo.extraLen + fileInfo.nameLen bytes
    procedure DataSeek(Source: TStream; LocalOffset: Int64);
  end;
  PLocalFileHeader = ^TLocalFileHeader;

  //// last header structure, as used in .zip file format
  // - this header ends the file and is used to find the TFileHeader entries
  TLastHeader = record
    /// $06054b50 PK#5#6 = LASTHEADER_SIGNATURE_INC -
    signature: cardinal;
    /// 0
    thisDisk: word;
    /// 0
    headerDisk: word;
    /// 1
    thisFiles: word;
    /// 1
    totalFiles: word;
    /// SizeOf(TFileHeaders + names)
    headerSize: cardinal;
    /// 32-bit offset to the central directory, i.e. the first TFileHeader
    headerOffset: cardinal;
    /// 0
    commentLen: word;
  end;
  PLastHeader = ^TLastHeader;

  //// last header structure, as used in zip64 file format
  // - this header ends the file and is used to find the TFileHeader entries
  TLastHeader64 = record
    /// $06064b50 PK#6#6 = LASTHEADER64_SIGNATURE_INC - 1
    signature: cardinal;
    /// length in bytes of this header
    recordsize: QWord;
    /// ZIP_VERSION[true] i.e. 45 for Zip64/4.5
    madeBy: word;
    /// ZIP_VERSION[true] i.e. 45 for Zip64/4.5
    neededVersion: word;
    /// 0
    thisDisk: cardinal;
    /// 0
    headerDisk: cardinal;
    /// 1
    thisFiles: QWord;
    /// 1
    totalFiles: QWord;
    /// SizeOf(TFileHeaders + names)
    headerSize: QWord;
    /// 64-bit offset to the central directory, i.e. the first TFileHeader
    headerOffset: Qword;
  end;
  PLastHeader64 = ^TLastHeader64;

  //// locator structure, as used in zip64 file format
  // - this header ends the file and is used to find the TFileHeader entries
  TLocator64 = record
    /// $07064b50 PK#6#7 = LASTHEADERLOCATOR64_SIGNATURE_INC - 1
    signature: cardinal;
    /// 0
    thisDisk: cardinal;
    /// 64-bit offset to the TLastHeader64
    headerOffset: QWord;
    /// 1
    totalDisks: cardinal;
  end;
  PLocator64 = ^TLocator64;

{$A+}

type
  /// used internally by TZipRead to store the zip entries
  TZipReadEntry = record
    /// the information of this file, as stored at the end of the .zip archive
    // - may differ from local^ content, depending of the zipper tool used
    dir: PFileHeader;
    /// the zip64 information of this file, as stored just after dir
    // - may be nil for regular zip 2.0 entry
    dir64: PFileInfoExtra64;
    /// points to the local file header in the .zip archive, stored in memory
    // - local^.data points to the stored/deflated data
    // - may be nil if the file size is bigger than WorkingMem
    local: PLocalFileHeader;
    /// offset to the local file header in the .zip archive
    // - use TLocalFileHeader.DataSeek to load and seek the stream
    localoffs: QWord;
    /// name of the file inside the .zip archive
    // - not ASCIIZ: length = dir^.fileInfo.nameLen
    storedName: PAnsiChar;
    /// name of the file inside the .zip archive
    // - converted from DOS/OEM or UTF-8 into generic (Unicode) string
    zipName: TFileName;
  end;
  PZipReadEntry = ^TZipReadEntry;
  TZipReadEntryDynArray = array of TZipReadEntry;

  TZipAbstract = class
  protected
    fCount: integer;
    fZipNamePathDelim, fZipNamePathDelimReversed: AnsiChar;
    fZipNamePathDelimString, fZipNamePathDelimReversedString: string;
    fFileName: TFileName;
    fInfo: TProgressInfo;
    procedure InfoStart(ExpectedSize: Int64; const Action, Name: TFileName);
    procedure SetZipNamePathDelim(Value: AnsiChar);
    function NormalizeZipName(const aZipName: TFileName): TFileName;
  public
    /// initialize this class
    constructor Create;
    /// the number of files inside a .zip archive
    property Count: integer
      read fCount;
    /// how sub folders names are handled in the ZIP
    // - by default, is '/' to follow 4.4.17 of reference PKware appnote
    // - you can force '\' if you want backward compatibility
    property ZipNamePathDelim: AnsiChar
      read fZipNamePathDelim write SetZipNamePathDelim;
    /// TSynLog.DoLog can be assigned for low-level Zip/Unzip progression logging
    property OnLog: TSynLogProc
      read fInfo.OnLog write fInfo.OnLog;
    /// optional TOnInfoProgress callback triggered during Zip/Unzip
    // - at least at process startup and finish, and every ReportDelay ms
    property OnProgress: TOnInfoProgress
      read fInfo.OnProgress write fInfo.OnProgress;
    /// number of milliseconds between each OnLog/OnProgress notification
    // - default is 200 ms
    property ReportDelay: Int64
      read fInfo.ReportDelay write fInfo.ReportDelay;
  end;

  /// read-only access to a .zip archive file
  // - can open directly a specified .zip file - only trailing WorkingMem bytes
  // are read in the memory, and should contain at least the Central Directory
  // - can open a .zip archive file content from a resource (embedded in the executable)
  // - can open a .zip archive file content from memory
  TZipRead = class(TZipAbstract)
  private
    fEntry: TZipReadEntryDynArray;
    fSource: TStream; // if .zip is a file bigger than 1MB
    fSourceOffset: QWord; // where the .zip start in fSource (if appended)
    fSourceBuffer: RawByteString; // last 1MB of fSource (central dir)
    fCentralDirectoryOffset: Int64;
    fCentralDirectoryFirstFile: PFileHeader;
    fResource: TExecutableResource;
    function UnZipStream(aIndex: integer; const aInfo: TFileInfoFull;
      aDest: TStream): boolean;
  public
    /// open a .zip archive file as Read Only
    // - if the .zip content has been appended to the file, search for its
    // first local zip header at ZipStartOffset
    constructor Create(const aFileName: TFileName; ZipStartOffset: QWord = 0;
      Size: QWord = 0; WorkingMem: QWord = 1 shl 20); overload;
    /// open a .zip archive file from its File Handle
    constructor Create(aFile: THandle; ZipStartOffset: QWord = 0;
      Size: QWord = 0; WorkingMem: QWord = 1 shl 20;
      DontReleaseHandle: boolean = false); overload;
    /// open a .zip archive file directly from a resource
    constructor Create(Instance: THandle; const ResName: string;
      ResType: PChar); overload;
    /// open a .zip archive file directly from memory
    // - supplied BufZip[0 .. Size - 1] buffer contain at least the zip central
    // directory, and should remain until Destroy
    // - main initalization process, called by overloaded Create() constructors
    // - Offset refers to the position of BufZip[] in respect to the .zip start
    constructor Create(BufZip: PByteArray; Size: PtrInt; Offset: Int64 = 0); overload;
    /// release associated memory
    destructor Destroy; override;

    /// get the index of a file inside the .zip archive
    function NameToIndex(const aName: TFileName): integer;
    /// uncompress a file stored inside the .zip archive into memory
    // - will refuse to uncompress more than 128 MB of content
    function UnZip(aIndex: integer): RawByteString; overload;
    /// uncompress a file stored inside the .zip archive into a stream
    function UnZip(aIndex: integer; aDest: TStream): boolean; overload;
    /// uncompress a file stored inside the .zip archive into a destination directory
    function UnZip(aIndex: integer; const DestDir: TFileName;
      DestDirIsFileName: boolean = false): boolean; overload;
    /// uncompress a file stored inside the .zip archive into memory
    function UnZip(const aName: TFileName): RawByteString; overload;
    /// uncompress a file stored inside the .zip archive into a destination directory
    function UnZip(const aName, DestDir: TFileName;
      DestDirIsFileName: boolean = false): boolean; overload;
    /// uncompress all files stored inside the .zip archive into the supplied
    // destination directory
    // - returns -1 on success, or the index in Entry[] of the failing file
    function UnZipAll(DestDir: TFileName): integer;
    /// uncompress and check the crc of all files of this .zip archive
    function TestAll: boolean;
    /// retrieve information about a file
    // - in some cases (e.g. for a .zip created by latest Java JRE),
    // Info.local^.zzipSize/zfullSize/zcrc32 may equal 0: this method is able
    // to retrieve the information either from the ending "central directory",
    // or by searching the "data descriptor" block
    // - returns TRUE if the Index is correct and the info was retrieved
    // - returns FALSE if the information was not successfully retrieved
    function RetrieveFileInfo(Index: integer; out Info: TFileInfoFull): boolean;
    /// retrieve low-level local information about a file
    // - to be used only for test purposes to validate the low-level zip format
    function RetrieveLocalFileHeader(Index: integer;
      out Header: TLocalFileHeader): boolean;

    /// the files inside the .zip archive
    property Entry: TZipReadEntryDynArray
      read fEntry;
  end;

  /// used internally by TZipWrite to store the zip entries
  TZipWriteEntry = record
    /// the file name, as stored in the .zip internal directory
    // - may be Ansi or UTF-8 if zweUtf8name flag is set
    intName: RawByteString;
    /// low-level zip header
    h32: TFileHeader;
    /// low-level zip64 extra header
    h64: TFileInfoExtra64;
    /// convenient internal content description
    flags: set of (zweZip64, zweUtf8Name);
  end;
  PZipWriteEntry = ^TZipWriteEntry;
  TZipWriteEntryDynArray = array of TZipWriteEntry;

  /// callback used by TZipWrite.AddFolder() to customize the file creation
  // - return false to ignore this file, true to add this file
  // - you can customize the CompressLevel (-1 = Z_STORED) and ZipFolder/ZipName
  TOnZipWriteAdd = function(const FolderName, FileName: TFileName;
    var CompressLevel: integer;
    var ZipFolder, ZipName: TFileName): boolean of object;

  /// callback used by TZipWrite.CreateFrom() to delete some files in the .zip
  // - return false to ignore this file, true to keep this file
  // - note that Entry.ZipName internal path delimiters are normalized to '/'
  // by default
  TOnZipWriteCreateFrom = function(
    const Entry: TZipReadEntry): boolean of object;

  /// the current step of TOnZipWrite
  // - zwsReadFile is triggerred during CreateFrom(TZipRead)
  // - zwsWriteMem/zwsWriteFile is set during AddDeflated/AddStored process
  TOnZipWriteStep = (
    zwsReadFile,
    zwsWriteMem,
    zwsWriteFile);

  /// write-only access for creating a .zip archive
  // - update can be done manualy by using CreateFrom()
  TZipWrite = class(TZipAbstract)
  protected
    fDest: TStream;
    fEntry: TZipWriteEntryDynArray;
    fAppendOffset: QWord;
    fNeedZip64: boolean;
    fDestOwned: boolean;
    fForceZip64: boolean;
    fOnCreateFromFilesIgnore: TFileNameDynArray;
    fOnProgressStep: TOnZipWriteStep;
    function OnCreateFrom(const Entry: TZipReadEntry): boolean;
    // returns @Entry[Count], allocating if necessary
    function NextEntry: PZipWriteEntry;
    function NewEntry(method, crc32, fileage: cardinal): PZipWriteEntry;
    // set offset, and write TFileInfo+TFileInfoExtra64 for LastEntry^
    function WriteHeader(const zipName: TFileName): PtrInt;
    function WriteRawHeader: PtrInt;
    /// write trailer and close destination file, then release associated memory
    procedure FinalFlush;
  public
    /// initialize the .zip archive
    // - a new .zip file content is prepared
    constructor Create(const aDestFileName: TFileName); overload;
    /// initialize the .zip archive
    // - a new .zip file content is prepared
    constructor Create(aDest: TStream); overload;
    /// initialize the .zip archive from a file handle
    // - a new .zip file content is prepared
    constructor Create(aDest: THandle; const aDestFileName: TFileName = ''); overload;
    /// initialize the .zip archive to be appended to an existing TStream
    // - the .zip content will be appended at aDest current position
    constructor CreateAppend(aDest: TStream; const aDestFileName: TFileName = '');
    /// open an existing .zip archive, ready to add some new files
    // - if the OnAdd callback returns false, the file won't be added to
    // Entry[]/Count list and the .zip content will be moved accordingly
    // - Dest stream is positioned next after the existing data (possibly
    // ignoring OnAdd files), ready to call AddDeflated/AddStored
    constructor CreateFrom(const aFileName: TFileName;
      WorkingMem: QWord = 1 shl 20; const OnAdd: TOnZipWriteCreateFrom = nil;
      const OnInfoProgress: TOnInfoProgress = nil);
    /// open an existing .zip archive, ready to add some new files
    // - overloaded constructor converting a file list into a corresponding
    // TOnZipWriteCreateFrom callback for case-insensitive file exclusion
    // - this is a convenient way of updating a .zip in-place: e.g. to replace a
    // file, supply it to the IgnoreZipFiles array, then call AddDeflate
    constructor CreateFromIgnore(const aFileName: TFileName;
      const IgnoreZipFiles: array of TFileName; WorkingMem: QWord = 1 shl 20;
      const OnInfoProgress: TOnInfoProgress = nil);
    /// flush pending content, then release associated memory
    destructor Destroy; override;
    /// compress (using the deflate method) a memory buffer, and add it to the zip file
    // - by default, current date/time is used if no FileAge is supplied; see also
    // DateTimeToWindowsFileTime() and FileAgeToWindowsTime()
    // - can use faster libdeflate instead of plain zlib if available
    // - will call IsContentCompressed() to detect and use Z_STORED if applying
    procedure AddDeflated(const aZipName: TFileName; Buf: pointer; Size: PtrInt;
      CompressLevel: integer = 6; FileAge: integer = 0); overload;
    /// add a memory buffer to the zip file, without compression
    // - content is stored, not deflated
    // - by default, current date/time is used if no FileAge is supplied; see also
    // DateTimeToWindowsFileTime() and FileAgeToWindowsTime()
    procedure AddStored(const aZipName: TFileName; Buf: pointer; Size: PtrInt;
      FileAge: integer = 0); overload;
    /// compress an existing file, and add it to the zip
    // - deflate reading 1MB chunks of input, triggerring zip64 format if needed
    // - can use faster libdeflate (if available) for files up to 64 MB, but
    // fallback to zlib with 1MB chunks for bigger files
    procedure AddDeflated(const aFileName: TFileName;
      RemovePath: boolean = true; CompressLevel: integer = 6;
      ZipName: TFileName = ''); overload;
    /// add a (huge) file to the zip file, without compression
    // - copy reading 1MB chunks of input, triggerring zip64 format if needed
    // - just a wrapper around AddDeflated() with CompressLevel=-1
    procedure AddStored(const aFileName: TFileName;
      RemovePath: boolean = true; ZipName: TFileName = ''); overload;
    /// compress (using AddDeflate) all files within a folder, and
    // add it to the zip file
    // - if Recursive is TRUE, would include files from nested sub-folders
    // - you may set CompressLevel=-1 to force Z_STORED method with no deflate
    // - OnAdd callback could be used to customize the process
    procedure AddFolder(const FolderName: TFileName;
      const Mask: TFileName = FILES_ALL; Recursive: boolean = true;
      CompressLevel: integer = 6; const OnAdd: TOnZipWriteAdd = nil);
    /// compress (using AddDeflate) the supplied files
    // - you may set CompressLevel=-1 to force Z_STORED method with no deflate
    procedure AddFiles(const aFiles: array of TFileName;
      RemovePath: boolean = true; CompressLevel: integer = 6);
    /// add a file from an already compressed zip entry
    procedure AddFromZip(ZipSource: TZipRead; ZipEntry: integer);
    /// append a file content into the destination file
    // - should be called before any file has been added
    // - useful  e.g. to add initial Setup.exe file before zipping some files
    // - see also FileAppend/ZipAppendFolder/ZipAppendFiles functions
    procedure Append(const Content: RawByteString);
    /// the destination file name, as given to the Create(TFileName) constructor
    property FileName: TFileName
      read fFileName;
    /// set to true when the archive needs the 64-bit extension
    property NeedZip64: boolean
      read fNeedZip64;
    /// low-level access to the TStream used to write the .zip content
    property Dest: TStream
      read fDest;
    /// the resulting file entries, ready to be written as a .zip catalog
    // - those will be appended after the data blocks at the end of the .zip file
    property Entry: TZipWriteEntryDynArray
      read fEntry;
    /// mainly used during debugging - default false is safe and more efficient
    property ForceZip64: boolean
      read fForceZip64 write fForceZip64;
    /// some details about the context when OnProgress is called
    property OnProgressStep: TOnZipWriteStep
      read fOnProgressStep;
  end;


{$ifndef PUREMORMOT2}
/// deprecated types: use TZipWrite instead
type
  TZipWriteAbstract = TZipWrite;
  TZipWriteToStream = TZipWrite;
{$endif PUREMORMOT2}


/// a TSynLogArchiveEvent handler which will compress older .log files
// into .zip archive files
// - resulting file will be named YYYYMM.zip and will be located in the
// aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM.zip'
function EventArchiveZip(const aOldLogFileName, aDestinationPath: TFileName): boolean;

/// check the content of a .zip file, decompressing and checking all crc
// - just a wrapper around TZipRead.TestAll
function ZipTest(const ZipName: TFileName): boolean;

/// add AppendFile after the end of MainFile with a magic markup for its position
// - the magic will be used as trailer to locate the offset of appended data
// - could be used e.g. to add a .zip to an executable
procedure FileAppend(const MainFile, AppendFile: TFileName); overload;

/// add AppendFile after the end of MainFile into NewFile
// - could be used e.g. to add a .zip to an executable
// - if PreserveWinDigSign is set, the Windows PE digital signature is kept
// https://blog.barthe.ph/2009/02/22/change-signed-executable/
procedure FileAppend(const MainFile, AppendFile, NewFile: TFileName;
  PreserveWinDigSign: boolean = false); overload;

/// zip a folder content after the end of MainFile into NewFile
// - if PreserveWinDigSign is set, the Windows PE digital signature is kept
procedure ZipAppendFolder(const MainFile, NewFile, ZipFolder: TFileName;
  const Mask: TFileName = FILES_ALL; Recursive: boolean = true;
  CompressionLevel: integer = 6; const OnAdd: TOnZipWriteAdd = nil;
  PreserveWinDigSign: boolean = false);

/// zip a some files after the end of MainFile into NewFile
// - if PreserveWinDigSign is set, the Windows PE digital signature is kept
procedure ZipAppendFiles(const MainFile, NewFile: TFileName;
  const ZipFiles: array of TFileName; RemovePath: boolean = true;
  CompressionLevel: integer = 6; PreserveWinDigSign: boolean = false);


/// (un)compress a data content using the gzip algorithm
// - as expected by THttpSocket.RegisterCompress for 'Content-Encoding: gzip'
// - use internally a level compression of 1, i.e. fastest available (content of
// 4803 bytes is compressed into 700, and request is 440 us instead of 220 us)
// - can use faster libdeflate instead of plain zlib if available
function CompressGZip(var Data: RawByteString; Compress: boolean): RawUtf8;

/// (un)compress a data content using the Deflate algorithm (i.e. "raw deflate")
// - as expected by THttpSocket.RegisterCompress
// - use internally a level compression of 1, i.e. fastest available
// - HTTP 'Content-Encoding: deflate' is pretty inconsistent in practice on client
// side, so use CompressGZip() instead - https://stackoverflow.com/a/5186177
// - can use faster libdeflate instead of plain zlib if available
function CompressDeflate(var Data: RawByteString; Compress: boolean): RawUtf8;

/// (un)compress a data content using the zlib algorithm
// - as expected by THttpSocket.RegisterCompress
// - use internally a level compression of 1, i.e. fastest available
// - HTTP 'Content-Encoding: zlib' is pretty inconsistent in practice on client
// side, so use CompressGZip() instead - https://stackoverflow.com/a/5186177
// - can use faster libdeflate instead of plain zlib if available
function CompressZLib(var Data: RawByteString; Compress: boolean): RawUtf8;

{$ifndef PUREMORMOT2}
// backward compatibility functions

/// compress some data, with a proprietary format (deflate + Adler32)
// - for backward compatibility - consider using TAlgoCompress instead
function CompressString(const data: RawByteString; failIfGrow: boolean = false;
  CompressionLevel: integer = 6) : RawByteString;

/// uncompress some data, with a proprietary format (deflate + Adler32)
// - for backward compatibility - consider using TAlgoCompress instead
// - return '' in case of a decompression failure
function UncompressString(const data: RawByteString) : RawByteString;

{$endif PUREMORMOT2}


type
  /// TStreamRedirect with crc32 32-bit checksum
  TStreamRedirectCrc32 = class(TStreamRedirectHasher)
  protected
    procedure DoHash(data: pointer; len: integer); override;
  public
    class function GetHashFileExt: RawUtf8; override;
  end;


/// THasher-compatible wrapper to the mormot.lib.z.crc32() low-level function
function crc32wrapper(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;

/// compute the crc32 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileCrc32(const FileName: TFileName): RawUtf8;



{ ************ TAlgoDeflate and TAlgoDeflate High-Level Compression Algorithms }

var
  /// acccess to Zip Deflate compression in level 6 as a TAlgoCompress class
  // - can use faster libdeflate instead of plain zlib if available
  AlgoDeflate: TAlgoCompress;

  /// acccess to Zip Deflate compression in level 1 as a TAlgoCompress class
  // - can use faster libdeflate instead of plain zlib if available
  AlgoDeflateFast: TAlgoCompress;




implementation

{ ************ TSynZipCompressor Stream Class }

const
  GZHEAD_SIZE = 10;
  GZHEAD: array[0..2] of cardinal = (
    $088B1F, 0, 0);

  {$ifdef LIBDEFLATESTATIC}
  // libdeflate + AVX is much faster than zlib, but its API expects only buffers
  // - files up to 64MB will call libdeflate and a temporary memory buffer
  LIBDEFLATE_MAXSIZE = 64 shl 20;
  {$endif LIBDEFLATESTATIC}


{ TSynZipStream }

{$ifdef FPC}
function TSynZipStream.GetPosition: Int64;
begin
  result := fSizeIn;
end;
{$endif FPC}

function TSynZipStream.GetSize: Int64;
begin
  result := fSizeIn;
end;

function TSynZipStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  result := Seek(Offset, TSeekOrigin(Origin));
end;

function TSynZipStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if not fInitialized then
    result := 0
  else if (Offset = 0) and
          (Origin = soCurrent) then
    // for TStream.Position on Delphi
    result := fSizeIn
  else
  begin
    result := 0;
    if (Offset <> 0) or
       (Origin <> soBeginning) or
       (fSizeIn <> 0) then
      raise ESynZip.CreateUtf8('Unexpected %.Seek', [self]);
  end;
end;

function TSynZipStream.{%H-}Read(var Buffer; Count: Longint): Longint;
begin
  {$ifdef DELPHI20062007}
  result := 0;
  {$endif DELPHI20062007}
  raise ESynZip.CreateUtf8('%.Read is not supported', [self]);
end;



{ TSynZipCompressor }

constructor TSynZipCompressor.Create(outStream: TStream; CompressionLevel: integer;
  Format: TSynZipCompressorFormat);
begin
  fDestStream := outStream;
  fFormat := Format;
  if fFormat = szcfGZ then
    fDestStream.WriteBuffer(GZHEAD, GZHEAD_SIZE);
  Z.Init(nil, 0, outStream, nil, nil, 0, 128 shl 10);
  fInitialized := Z.CompressInit(CompressionLevel, fFormat = szcfZip);
end;

destructor TSynZipCompressor.Destroy;
begin
  if fInitialized then
  begin
    try
      Flush;
      if fFormat = szcfGZ then
      begin
        // .gz format expected a trailing header
        fDestStream.WriteBuffer(fCRC, 4); // CRC of the uncompressed data
        fDestStream.WriteBuffer(Z.Stream.total_in, 4); // truncated to 32-bit
      end;
    except
      // ignore any exception e.g. when zip was aborted
    end;
    Z.CompressEnd;
  end;
  inherited Destroy;
end;

function TSynZipCompressor.Write(const Buffer; Count: Longint): Longint;
var
  code: integer;
begin
  if (self = nil) or
     not fInitialized or
     (Count <= 0) then
  begin
    result := 0;
    exit;
  end;
  inc(fSizeIn, Count);
  Z.Stream.next_in := pointer(@Buffer);
  Z.Stream.avail_in := Count;
  fCRC := mormot.lib.z.crc32(fCRC, @Buffer, Count); // manual crc32
  while Z.Stream.avail_in > 0 do
  begin
    // compress pending data
    code := Z.Check(Z.Compress(Z_NO_FLUSH), [Z_OK], 'TSynZipCompressor.Write');
    if Z.Stream.avail_out = 0 then
      Z.DoFlush(code);
  end;
  Z.Stream.next_in := nil;
  Z.Stream.avail_in := 0;
  fSizeOut := Z.Written;
  result := Count;
end;

procedure TSynZipCompressor.Flush;
begin
  if not fInitialized then
    exit;
  while (Z.Check(Z.Compress(Z_FINISH),
          [Z_OK, Z_STREAM_END], 'TSynZipCompressor.Flush') <> Z_STREAM_END) and
        (Z.Stream.avail_out = 0) do
    Z.DoFlush(Z_OK);
  Z.DoFlush(Z_STREAM_END);
  fSizeOut := Z.Written;
end;


{ TSynZipDecompressor }

constructor TSynZipDecompressor.Create(outStream: TStream;
  Format: TSynZipCompressorFormat);
begin
  if fFormat = szcfGZ then
    raise ESynZip.CreateUtf8('%.Create: unsupported szcfGZ', [self]);
  fDestStream := outStream;
  fFormat := Format;
  Z.Init(nil, 0, outStream, @fCRC, nil, 0, 128 shl 10);
  fInitialized := Z.UncompressInit(fFormat = szcfZip);
end;

destructor TSynZipDecompressor.Destroy;
begin
  if fInitialized then
  begin
    try
      Flush;
    except
      // ignore any exception e.g. when unzip was aborted
    end;
    Z.UncompressEnd;
  end;
  inherited Destroy;
end;

function TSynZipDecompressor.Write(const Buffer; Count: Longint): Longint;
begin
  if (self = nil) or
     not fInitialized or
     (Count <= 0) then
  begin
    result := 0;
    exit;
  end;
  inc(fSizeIn, Count);
  Z.Stream.next_in := pointer(@Buffer);
  Z.Stream.avail_in := Count;
  while Z.Stream.avail_in > 0 do
  begin
    // uncompress pending data
    result := Z.Check(Z.Uncompress(Z_NO_FLUSH), [Z_OK, Z_STREAM_END],
      'TSynZipDecompressor.Write');
    if Z.Stream.avail_out = 0 then
      Z.DoFlush(Z_OK);
    if result = Z_STREAM_END then
      break;
  end;
  Z.Stream.next_in := nil;
  Z.Stream.avail_in := 0;
  fSizeOut := Z.Written;
  result := Count;
end;

procedure TSynZipDecompressor.Flush;
begin
  if not fInitialized then
    exit;
  while (Z.Check(Z.Uncompress(Z_FINISH),
          [Z_OK, Z_STREAM_END], 'TSynZipDecompressor.Flush') <> Z_STREAM_END) and
        (Z.Stream.avail_out = 0) do
    Z.DoFlush(Z_OK);
  Z.DoFlush(Z_STREAM_END);
  fSizeOut := Z.Written;
end;



{ ************  GZ Read/Write Support }

type
  // low-level flags used in the .gz file header
  TGZFlags = set of (
    gzfText,
    gzfHCRC,
    gzfExtra,
    gzfName,
    gzfComment);

{ TGZRead }

function TGZRead.Init(gz: PAnsiChar; gzLen: PtrInt): boolean;
var
  offset: PtrInt;
  flags: TGZFlags;
begin
  // see https://www.ietf.org/rfc/rfc1952.txt
  comp := nil;
  complen := 0;
  uncomplen32 := 0;
  zsdest := nil;
  result := false;
  extra := nil;
  fname := nil;
  fcomment := nil;
  if (gz = nil) or
     (gzLen <= 18) or
     (PCardinal(gz)^ and $ffffff <> GZHEAD[0]) then
    exit; // .gz file as header + compressed + crc32 + len32 format
  flags := TGZFlags(gz[3]);
  unixmodtime := PCardinal(gz + 4)^;
  offset := GZHEAD_SIZE;
  if gzfExtra in flags then
  begin
    extra := gz + offset;
    inc(offset, PWord(extra)^ + SizeOf(word));
  end;
  if gzfName in flags then
  begin
    // FNAME flag (as created e.g. by 7Zip)
    fname := gz + offset;
    while (offset < gzLen) and
          (gz[offset] <> #0) do
      inc(offset);
    inc(offset);
  end;
  if gzfComment in flags then
  begin
    fcomment := gz + offset;
    while (offset < gzLen) and
          (gz[offset] <> #0) do
      inc(offset);
    inc(offset);
  end;
  if gzfHCRC in flags then
    if PWord(gz + offset)^ <> mormot.lib.z.crc32(0, gz, offset) and $ffff then
      exit
    else
      inc(offset, SizeOf(word));
  if offset >= gzLen - 8 then
    exit;
  uncomplen32 := PCardinal(@gz[gzLen - 4])^; // modulo 2^32 by design (may be 0)
  comp := gz + offset;
  complen := gzLen - offset - 8;
  crc32 := PCardinal(@gz[gzLen - 8])^;
  result := true;
end;

function TGZRead.ToMem: RawByteString;
begin
  result := '';
  if (comp = nil) or
     ((uncomplen32 = 0) and
      (crc32 = 0)) then
    // 0 length stream
    exit;
  SetLength(result, uncomplen32);
  if (cardinal(UnCompressMem(
       comp, pointer(result), complen, uncomplen32)) <> uncomplen32) or
     (mormot.lib.z.crc32(0, pointer(result), uncomplen32) <> crc32) then
    result := ''; // invalid CRC or truncated uncomplen32
end;

function TGZRead.ToStream(stream: TStream; tempBufSize: integer): boolean;
var
  crc: cardinal;
begin
  crc := 0;
  if (comp = nil) or
     ((uncomplen32 = 0) and
      (crc32 = 0)) then
    // weird .gz file of 0 length stream, with only header
    result := true
  else
    result := (comp <> nil) and
              (stream <> nil) and
              (UnCompressStream(comp, complen,
                stream, @crc, {zlib=}false, tempBufSize) = uncomplen32) and
              (crc = crc32);
end;

function TGZRead.ToFile(const filename: TFileName; tempBufSize: integer): boolean;
var
  f: TStream;
begin
  result := false;
  if (comp = nil) or
     (filename = '') then
    exit;
  {$ifdef LIBDEFLATESTATIC}
  // files up to 64MB will call libdeflate using a temporary memory buffer
  if uncomplen32 < LIBDEFLATE_MAXSIZE then
    FileFromString(ToMem, filename)
  else
  {$endif LIBDEFLATESTATIC}
  begin
    f := TFileStream.Create(filename, fmCreate);
    try
      result := ToStream(f, tempBufSize);
    finally
      f.Free;
    end;
  end;
end;

function TGZRead.ZStreamStart(dest: pointer; destsize: integer): boolean;
begin
  result := false;
  zscode := Z_STREAM_ERROR;
  if (comp = nil) or
     (dest = nil) or
     (destsize <= 0) then
    exit;
  z.Init(comp, dest, complen, destsize);
  if z.UncompressInit({zlibformat=}false) then
  begin
    zscode := Z_OK;
    zscrc := 0;
    zsdest := dest;
    zssize := destsize;
    result := true;
  end;
end;

function TGZRead.ZStreamStarted: boolean;
begin
  result := zsdest <> nil;
end;

function TGZRead.ZStreamNext: integer;
begin
  result := 0;
  if (comp = nil) or
     (zsdest = nil) or
     not ((zscode = Z_OK) or
     (zscode = Z_STREAM_END) or
     (zscode = Z_BUF_ERROR)) then
    exit;
  if zscode <> Z_STREAM_END then
  begin
    zscode := z.Check(z.Uncompress(Z_FINISH),
      [Z_OK, Z_STREAM_END, Z_BUF_ERROR], 'TGZRead.ZStreamNext');
    result := zssize - integer(z.Stream.avail_out);
    if result = 0 then
      exit;
    zscrc := mormot.lib.z.crc32(zscrc, zsdest, result);
    z.Stream.next_out := zsdest;
    z.Stream.avail_out := zssize;
  end;
end;

function TGZRead.ZStreamDone: boolean;
begin
  result := false;
  if (comp <> nil) and
     (zsdest <> nil) then
  begin
    z.UncompressEnd;
    zsdest := nil;
    result := (zscrc = crc32) and
              (cardinal(z.Stream.total_out) = uncomplen32);
  end;
end;

function GZRead(gz: PAnsiChar; gzLen: integer): RawByteString;
var
  gzr: TGZRead;
begin
  if gzr.Init(gz, gzLen) then
    result := gzr.ToMem
  else
    result := '';
end;

function GZFile(const orig, destgz: TFileName; CompressionLevel: integer): boolean;
var
  gz: TSynZipCompressor;
  s, d: TFileStream;
begin
  try
    s := TFileStream.Create(orig, fmOpenRead or fmShareDenyNone);
    try
      d := TFileStream.Create(destgz, fmCreate);
      try
        gz := TSynZipCompressor.Create(d, CompressionLevel, szcfGZ);
        try
          gz.CopyFrom(s, 0);  // Count=0 for whole stream copy
          result := true;
        finally
          gz.Free;
        end;
      finally
        d.Free;
      end;
    finally
      s.Free;
    end;
  except
    result := false;
  end;
end;


{ ************  .ZIP Archive File Support }

const
  // those constants have +1 to avoid finding it in the exe
  ENTRY_SIGNATURE_INC = $02014b50 + 1;                 // PK#1#2
  FIRSTHEADER_SIGNATURE_INC = $04034b50 + 1;           // PK#3#4
  LASTHEADER_SIGNATURE_INC = $06054b50 + 1;            // PK#5#6
  LASTHEADER64_SIGNATURE_INC = $06064b50 + 1;          // PK#6#6
  LASTHEADERLOCATOR64_SIGNATURE_INC = $07064b50 + 1;   // PK#6#7
  FILEAPPEND_SIGNATURE_INC = $a5ababa5 + 1; // as marked by FileAppendSignature

  // identify the OS used to forge the .zip - see PKware appnote 4.4.2
  ZIP_OS = (
      {$ifdef OSDARWIN}
        19 // OS X (Darwin)
      {$else}
      {$ifdef OSWINDOWS}
        3 // UNIX was reported to work with UTF-8 by MPV
          // 10 = Windows NTFS is not recognized as such, and 0 = MS_DOS
      {$else}
        3  // UNIX
      {$endif OSWINDOWS}
      {$endif OSDARWIN}) shl 8;

  // regular .zip format is version 2.0, Zip64 format has version 4.5
  ZIP_VERSION: array[{zip64:}boolean] of cardinal = (
    20 + ZIP_OS,
    45 + ZIP_OS);

  // some tools (e.g. MacOS) have local size+crc=0 and append a descriptor
  FLAG_DATADESCRIPTOR = 8;
  SIGNATURE_DATADESCRIPTOR = $08074b50;

  ZIP64_EXTRA_ID = $0001; // Zip64 extended information
  NTFS_EXTRA_ID = $000a; // NTFS
  UNIX_EXTRA_ID = $000d; // UNIX
  EXT_TIME_EXTRA_ID = $5455; // Extended timestamp
  INFOZIP_UNIX_EXTRAID = $5855; // Info-ZIP Unix extension

  ZIP_MINSIZE_DEFLATE = 256; // size < 256 bytes -> Z_STORED

  ZIP32_MAXSIZE = cardinal(-1);   // > trigger size for ZIP64 format
  ZIP32_MAXFILE = (1 shl 16) - 1; // > trigger file count for ZIP64 format


{ TLocalFileHeader }

function TLocalFileHeader.Size: PtrInt;
begin
  result := SizeOf(self) + fileInfo.extraLen + fileInfo.nameLen;
end;

function TLocalFileHeader.Data: PAnsiChar;
begin
  result := @Self;
  inc(result, Size);
end;

procedure TLocalFileHeader.DataSeek(Source: TStream; LocalOffset: Int64);
begin
  if Source = nil then
    raise ESynZip.Create('Zip: DataSeek with Source=nil');
  // we read the real local header content before calling Size for its offset
  Source.Seek(LocalOffset, soBeginning);
  if Source.Read(self, SizeOf(self)) <> SizeOf(self) then
    raise ESynZip.Create('Zip: DataSeek reading error');
  // now we can compute and ignore the real header size -> seek on data
  Source.Seek(LocalOffset + Size, soBeginning);
end;


{ TFileHeader }

function TFileHeader.IsFolder: boolean;
begin
  result := (@self <> nil) and
            (extFileAttr and $00000010 <> 0);
end;

procedure TFileHeader.SetVersion(NeedZip64: boolean);
begin
  signature := ENTRY_SIGNATURE_INC;
  dec(signature); // constant was +1 to avoid finding it in the exe
  madeBy := ZIP_VERSION[NeedZip64];
  extFileAttr := $A0; // archive, normal
  fileInfo.neededVersion := madeBy;
end;


{ TFileInfo }

function TFileInfo.GetAlgoID: integer;
begin
  // in PKware appnote, bits 7..10 of general purpose bit flag are not used
  result := (flags shr 7) and 15; // proprietary flag for mormot.core.zip.pas
end;

function TFileInfo.SameAs(aInfo: PFileInfo): boolean;
begin
  // checking 32-bit sizes (which may be -1 for zip64) + zcrc32 seems enough
  // (i.e. tolerate a time change through a network)
  if (zzipSize = 0) or
     (aInfo.zzipSize = 0) then
    raise ESynZip.Create('SameAs() with crc+sizes in "data descriptor"');
  result := (zzipMethod = aInfo.zzipMethod) and
            (flags = aInfo.flags) and
            (zzipSize = aInfo.zzipSize) and    // =cardinal(-1) if zip64
            (zfullSize = aInfo.zfullSize) and  // =cardinal(-1) if zip64
            (zcrc32 = aInfo.zcrc32);
end;

procedure TFileInfo.SetAlgoID(Algorithm: integer);
begin
  zzipMethod := Z_STORED; // file is stored, accorging to .ZIP standard
  // in PKware appnote, bits 7..10 of general purpose bit flag are not used
  flags := (flags and $F87F) or
           (Algorithm and 15) shl 7; // proprietary flag for mormot.core.zip.pas
end;

function TFileInfo.GetUtf8FileName: boolean;
begin
  // from PKware appnote, Bit 11: Language encoding flag (EFS)
  result := (flags and (1 shl 11)) <> 0;
end;

procedure TFileInfo.SetUtf8FileName;
begin
  flags := flags or (1 shl 11);
end;

procedure TFileInfo.UnSetUtf8FileName;
begin
  flags := flags and not (1 shl 11);
end;

function TFileInfo.IsZip64: boolean;
begin
  result := ToByte(neededVersion) >= 45; // ignore OS flag from ZIP_VERSION[]
end;

function Is7BitAnsi(P: PChar): boolean;
begin
  result := false;
  if P <> nil then
    while true do
      if ord(P^) = 0 then
        break
      else if ord(P^) <= 127 then
        inc(P)
      else
        exit;
  result := true;
end;


{ TZipAbstract }

constructor TZipAbstract.Create;
begin
  SetZipNamePathDelim('/'); // PKware appnote 4.4.17: MUST be forward slashes
  fInfo.ReportDelay := 200; // report to callbacks every 200 ms by default
end;

procedure TZipAbstract.InfoStart(
  ExpectedSize: Int64; const Action, Name: TFileName);
begin
  if Assigned(OnLog) or
     Assigned(OnProgress) then
    fInfo.DoStart(self, ExpectedSize, Action + Name)
  else
    fInfo.ExpectedSize := 0;
end;

procedure TZipAbstract.SetZipNamePathDelim(Value: AnsiChar);
begin
  if Value = fZipNamePathDelim then
    exit;
  if Value = '/' then
    fZipNamePathDelimReversed := '\'
  else if Value = '\' then
    fZipNamePathDelimReversed := '/'
  else
    exit; // do nothing if not one of two common folder delimiters
  fZipNamePathDelim := Value;
  fZipNamePathDelimString := string(Value);
  fZipNamePathDelimReversedString := string(fZipNamePathDelimReversed);
end;

function TZipAbstract.NormalizeZipName(const aZipName: TFileName): TFileName;
begin
  result := aZipName;
  if Pos(fZipNamePathDelimReversedString, aZipName) > 0 then
    result := StringReplace(aZipName, fZipNamePathDelimReversedString,
      fZipNamePathDelimString, [rfReplaceAll]);
end;


{ TZipWrite }

constructor TZipWrite.Create(aDest: TStream);
begin
  Create;
  fDest := aDest;
end;

constructor TZipWrite.Create(aDest: THandle; const aDestFileName: TFileName);
begin
  fFileName := aDestFileName;
  fDestOwned := true;
  Create(TFileStreamFromHandle.Create(aDest));
end;

constructor TZipWrite.Create(const aDestFileName: TFileName);
begin
  fFileName := aDestFileName;
  fDestOwned := true;
  Create(TFileStream.Create(aDestFileName, fmCreate));
end;

constructor TZipWrite.CreateAppend(aDest: TStream; const aDestFileName: TFileName);
begin
  fFileName := aDestFileName;
  fAppendOffset := aDest.Position;
  Create(aDest);
end;

constructor TZipWrite.CreateFrom(const aFileName: TFileName; WorkingMem: QWord;
  const OnAdd: TOnZipWriteCreateFrom; const OnInfoProgress: TOnInfoProgress);
var
  R: TZipRead;
  h: THandle;
  s: PZipReadEntry;
  d: PZipWriteEntry;
  writepos, readpos, len: Int64;
  info: TFileInfoFull;
  i, read: integer;
  tomove: boolean;
  tmp: RawByteString;
begin
  fInfo.OnProgress := OnInfoProgress;
  h := FileOpen(aFileName, fmOpenReadWrite or fmShareDenyNone);
  if ValidHandle(h) then
  begin
    // we need fDest for WriteRawHeader below
    Create(h, aFileName);
    // read the existing .zip directory
    R := TZipRead.Create(h, 0, 0, WorkingMem, {nohandleclose=}true);
    try
      if (R.fSourceOffset <> 0) or
         (fAppendOffset <> 0) then
        raise ESynZip.CreateUtf8('%.CreateFrom: % not a plain .zip file',
          [self, aFileName]);
      SetLength(fEntry, R.Count + 10);
      writepos := 0; // where to add new files
      tomove := false;
      s := pointer(R.Entry);
      d := pointer(fEntry);
      for i := 0 to R.Count - 1 do
      begin
        if Assigned(OnAdd) and
           not OnAdd(s^) then
          // we were asked to ignore this file -> overwrite/move its content
          tomove := true
        else
        begin
          // append this entry to the TZipWrite directory
          if not R.RetrieveFileInfo(i, info) then
            raise ESynZip.CreateUtf8('%.CreateFrom(%) failed on %',
              [self, aFileName, s^.zipName]);
          d^.h64 := info.f64;
          d^.h32.fileInfo := info.f32;
          if tomove then
          begin
            // some files were ignored -> move content over deleted file(s)
            len := info.f64.zzipSize;
            if writepos >= s^.localoffs then
              raise ESynZip.CreateUtf8('%.CreateFrom deletion overlap', [self]);
            FileSeek64(h, writepos, soFromBeginning);
            inc(writepos, WriteHeader(s^.zipName));
            if len > 0 then
              if s^.local <> nil then
              begin
                // this file is small enough to be in the current work memory
                FileWrite(h, s^.local^.Data^, len);
                inc(writepos, len);
              end
              else
              begin
                // read and move the file by 1MB chunks
                InfoStart(len, 'Read ', s^.zipName);
                if tmp = '' then
                  SetString(tmp, nil, 1 shl 20);
                readpos := Int64(s^.localoffs) + info.localfileheadersize;
                repeat
                  FileSeek64(h, readpos, soFromBeginning);
                  read := length(tmp);
                  if len < read then
                    read := len;
                  read := FileRead(h, pointer(tmp)^, read);
                  FileSeek64(h, writepos, soFromBeginning);
                  FileWrite(h, pointer(tmp)^, read);
                  inc(readpos, read);
                  inc(writepos, read);
                  if fInfo.ExpectedSize <> 0 then
                    fInfo.DoAfter(self, read);
                  dec(len, read)
                until len = 0;
              end;
          end
          else
          begin
            // we can keep the file content in-place -> just update dir entry
            d^.h32.SetVersion(info.f32.IsZip64);
            d^.h64.offset := writepos;
            if d^.h64.zip64id = 0 then
              d^.h32.localHeadOff := writepos
            else
            begin
              // zip64 input
              assert(d^.h32.fileInfo.extraLen = SizeOf(d^.h64));
              dec(d^.h32.fileInfo.extraLen, SizeOf(d^.h64.offset));
              dec(d^.h64.size, SizeOf(d^.h64.offset));
            end;
            SetString(d^.intName, s^.storedName, d^.h32.fileInfo.nameLen);
            inc(writepos, info.localfileheadersize + Int64(info.f64.zzipSize));
          end;
          inc(fCount);
          inc(d);
        end;
        inc(s);
      end;
      // rewind to the position fitted for new files appending
      FileSeek64(h, writepos, soFromBeginning);
    finally
      R.Free;
    end;
  end
  else
    // we need to create a new .zip file
    Create(FileCreate(aFileName), aFileName);
end;

function TZipWrite.OnCreateFrom(const Entry: TZipReadEntry): boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 0 to length(fOnCreateFromFilesIgnore) - 1 do
    // case-insensitive even on POSIX (no AnsiCompareFileName)
    if CompareText(Entry.zipName, fOnCreateFromFilesIgnore[i]) = 0 then
      exit;
  result := true;
end;

constructor TZipWrite.CreateFromIgnore(const aFileName: TFileName;
  const IgnoreZipFiles: array of TFileName; WorkingMem: QWord;
  const OnInfoProgress: TOnInfoProgress);
var
  i: PtrInt;
begin
  Create; // for proper SetZipNamePathDelim() call
  SetLength(fOnCreateFromFilesIgnore, length(IgnoreZipFiles));
  for i := 0 to high(IgnoreZipFiles) do
    fOnCreateFromFilesIgnore[i] := NormalizeZipName(IgnoreZipFiles[i]);
  CreateFrom(aFileName, WorkingMem, OnCreateFrom, OnInfoProgress);
  fOnCreateFromFilesIgnore := nil;
end;

destructor TZipWrite.Destroy;
begin
  FinalFlush;
  inherited Destroy;
  if fDestOwned then
    fDest.Free;
end;

function TZipWrite.NextEntry: PZipWriteEntry;
begin
  if Count >= length(fEntry) then
    SetLength(fEntry, NextGrow(length(fEntry)));
  result := @fEntry[Count];
end;

function TZipWrite.NewEntry(method, crc32, fileage: cardinal): PZipWriteEntry;
begin
  result := NextEntry;
  FillCharFast(result^, SizeOf(result^), 0);
  result^.h32.fileInfo.zzipMethod := method;
  result^.h32.fileInfo.zcrc32 := crc32;
  if fileage = 0 then
    result^.h32.fileInfo.zlastMod := DateTimeToWindowsFileTime(Now)
  else
    result^.h32.fileInfo.zlastMod := fileage;
end;

function TZipWrite.WriteHeader(const zipName: TFileName): PtrInt;
begin
  with Entry[Count] do
  begin
    // caller should have set h64.zzipSize64/zfullSize64
    // and h32.zzipMethod/zcrc32/zlastMod
    h64.offset := QWord(fDest.Position) - fAppendOffset;
    if ForceZip64 or
       (h64.zzipSize >= ZIP32_MAXSIZE) or
       (h64.zfullSize >= ZIP32_MAXSIZE) or
       (h64.offset >= ZIP32_MAXSIZE) then
      // big files requires the zip64 format with h64 extra information
      include(flags, zweZip64);
    h32.SetVersion(zweZip64 in flags);
    if zweZip64 in flags then
    begin
      fNeedZip64 := true;
      h32.fileInfo.zzipSize := ZIP32_MAXSIZE;
      h32.fileInfo.zfullSize := ZIP32_MAXSIZE;
      h32.fileInfo.extraLen := SizeOf(h64) - SizeOf(h64.offset);
      h32.localHeadOff := ZIP32_MAXSIZE;
      h64.zip64id := ZIP64_EXTRA_ID;
      h64.size := SizeOf(h64.zzipSize) + SizeOf(h64.zfullSize);
    end
    else
    begin
      h32.fileInfo.zzipSize := h64.zzipSize;
      h32.fileInfo.zfullSize := h64.zfullSize;
      h32.localHeadOff := h64.offset;
      h32.fileInfo.extraLen := 0; // AddFromZip() source may have something here
    end;
    if Is7BitAnsi(pointer(zipName)) then
      // ZIP should handle CP437 charset, but fails sometimes (e.g. Korean)
      intName := StringToAnsi7(zipName)
    else
    begin
      // safe UTF-8 file name encoding
      intName := StringToUtf8(zipName);
      include(flags, zweUtf8Name);
      h32.fileInfo.SetUtf8FileName;
    end;
    h32.fileInfo.nameLen := length(intName);
    result := WriteRawHeader;
  end;
end;

function TZipWrite.WriteRawHeader: PtrInt;
var
  P: PAnsiChar;
  tmp: TSynTempBuffer;
begin
  with Entry[Count] do
  begin
    P := tmp.Init(SizeOf(cardinal) +
      SizeOf(h32.fileInfo) + h32.fileInfo.nameLen + h32.fileInfo.extraLen);
    PCardinal(P)^ := FIRSTHEADER_SIGNATURE_INC;
    dec(PCardinal(P)^); // +1 to avoid finding it in the exe generated code
    inc(PCardinal(P));
    PFileInfo(P)^ := h32.fileInfo;
    inc(PFileInfo(P));
    MoveFast(pointer(intName)^, P^, h32.fileInfo.nameLen);
    inc(P, h32.fileInfo.nameLen);
    if h32.fileInfo.extraLen <> 0 then
      MoveFast(h64, P^, h32.fileInfo.extraLen);
    fDest.WriteBuffer(tmp.buf^, tmp.len); // write once to disk/stream
    result := tmp.Len; // returns the number of bytes written
    tmp.Done;
  end;
end;

procedure TZipWrite.AddDeflated(const aZipName: TFileName;
  Buf: pointer; Size: PtrInt; CompressLevel: integer; FileAge: integer);
var
  e: PZipWriteEntry;
  tmp: TSynTempBuffer;
begin
  if self = nil then
    exit;
  if (Size < ZIP_MINSIZE_DEFLATE) or
     IsContentCompressed(Buf, Size)  then
  begin
    AddStored(aZipName, Buf, Size, FileAge);
    exit;
  end;
  // may call libdeflate_crc32 / libdeflate_deflate_compress
  e := NewEntry(Z_DEFLATED, mormot.lib.z.crc32(0, Buf, Size), FileAge);
  e^.h64.zfullSize := Size;
  fOnProgressStep := zwsWriteMem;
  InfoStart(Size, 'Deflate ', aZipName);
  tmp.Init(zlibCompressMax(Size));
  try
    e^.h64.zzipSize := CompressMem(Buf, tmp.buf, Size, tmp.len, CompressLevel);
    WriteHeader(aZipName);
    fDest.WriteBuffer(tmp.buf^, e^.h64.zzipSize); // write compressed data
    inc(fCount);
  finally
    tmp.Done;
  end;
  if fInfo.ExpectedSize <> 0 then
    fInfo.DoAfter(self, Size);
end;

procedure TZipWrite.AddStored(const aZipName: TFileName;
  Buf: pointer; Size: PtrInt; FileAge: integer);
begin
  if self = nil then
    exit;
  fOnProgressStep := zwsWriteMem;
  InfoStart(Size, 'Store ', aZipName);
  // may call libdeflate_crc32
  with NewEntry(Z_STORED, mormot.lib.z.crc32(0, Buf, Size), FileAge)^ do
  begin
    h64.zfullSize := Size;
    h64.zzipSize := Size;
    WriteHeader(aZipName);
    fDest.WriteBuffer(Buf^, Size); // write stored data
    inc(fCount);
  end;
  if fInfo.ExpectedSize <> 0 then
    fInfo.DoAfter(self, Size);
end;

procedure TZipWrite.AddStored(const aFileName: TFileName;
  RemovePath: boolean; ZipName: TFileName);
begin
  AddDeflated(aFileName, RemovePath, {level=} -1, ZipName);
end;

procedure TZipWrite.AddDeflated(const aFileName: TFileName;
  RemovePath: boolean; CompressLevel: integer; ZipName: TFileName);
var
  f: THandle;
  headerpos, datapos, newpos, todo, len: QWord;
  met, age: cardinal;
  deflate: TSynZipCompressor;
  tmp: RawByteString;
begin
  if self = nil then
    exit;
  // compute the name inside the .zip if not specified
  if ZipName = '' then
    if RemovePath then
      ZipName := ExtractFileName(aFileName) // no path = normalized for sure
    else
      ZipName := NormalizeZipName(aFileName);
  // open the input file
  f := FileOpen(aFileName, fmOpenRead or fmShareDenyNone);
  if ValidHandle(f) then
    try
      todo := FileSeek64(f, 0, soFromEnd);
      FileSeek64(f, 0, soFromBeginning);
      age := FileAgeToWindowsTime(aFileName);
      // prepare and write initial version of the local file header
      met := Z_DEFLATED;
      if (CompressLevel < 0) or
         (todo < ZIP_MINSIZE_DEFLATE) then
        met := Z_STORED; // tiny file, or called from AddStored()
      {$ifdef LIBDEFLATESTATIC}
      // libdeflate is much faster than zlib, but its API expects only buffers
      if (met = Z_DEFLATED) and
         (todo <= LIBDEFLATE_MAXSIZE) then
      begin
        // files up to 64MB will be loaded into memory and call libdeflate
        Setlength(tmp, todo);
        len := FileRead(f, pointer(tmp)^, todo);
        if len <> todo then
          raise ESynZip.CreateUtf8('%: failed to read % [%]',
            [self, aFileName, GetLastError]);
        AddDeflated(ZipName, pointer(tmp), todo, CompressLevel, age);
        exit;
      end;
      // bigger/stored files will fallback to (zlib and its) streaming methods
      {$endif LIBDEFLATESTATIC}
      fOnProgressStep := zwsWriteFile;
      InfoStart(todo, 'Deflate ', ZipName);
      with NewEntry(met, 0, age)^ do
      begin
        h64.zfullSize := todo;
        if met = Z_STORED then
          h64.zzipSize := todo
        else
          h64.zzipSize := zlibCompressMax(todo);
        headerpos := fDest.Position;
        WriteHeader(ZipName);
        // append the stored/deflated data
        datapos := fDest.Position;
        len := 1 shl 20;
        if todo < len then
          len := todo;
        SetLength(tmp, len); // 1MB temporary chunk for reading
        deflate := nil;
        if met = Z_DEFLATED then
          deflate := TSynZipCompressor.Create(fDest, CompressLevel, szcfRaw);
        try
          while todo <> 0 do
          begin
            len := FileRead(f, pointer(tmp)^, length(tmp));
            if integer(len) <= 0 then
              raise ESynZip.CreateUtf8('%: failed to read % [%]',
                [self, aFileName, GetLastError]);
            if deflate = nil then
            begin
              h32.fileInfo.zcrc32 := // manual (libdeflate) crc computation
                mormot.lib.z.crc32(h32.fileInfo.zcrc32, pointer(tmp), len);
              fDest.WriteBuffer(pointer(tmp)^, len); // store
            end
            else
              deflate.WriteBuffer(pointer(tmp)^, len); // crc and compress
            if fInfo.ExpectedSize <> 0 then
              fInfo.DoAfter(self, len);
            dec(todo, len);
          end;
          if deflate <> nil then
          begin
            deflate.Flush;
            assert(deflate.SizeIn = h64.zfullSize);
            h64.zzipSize := deflate.SizeOut;
            h32.fileInfo.zcrc32 := deflate.CRC;
          end;
          if h32.fileInfo.extraLen = 0 then
            h32.fileInfo.zzipSize := h64.zzipSize;
        finally
          deflate.Free;
        end;
        // overwrite the local file header with the exact values
        newpos := fDest.Position;
        fDest.Seek(headerpos, soBeginning);
        WriteRawHeader;
        assert(QWord(fDest.Position) = datapos);
        fDest.Seek(newpos, soBeginning);
      end;
      inc(fCount);
    finally
      FileClose(f);
    end;
end;

procedure TZipWrite.AddFolder(const FolderName: TFileName;
  const Mask: TFileName; Recursive: boolean; CompressLevel: integer;
  const OnAdd: TOnZipWriteAdd);

  procedure RecursiveAdd(const fileDir, zipDir: TFileName);
  var
    f: TSearchRec;
    cl: integer;
    zf, zn: TFileName;
  begin
    if Recursive then
      if FindFirst(fileDir + FILES_ALL, faDirectory, f) = 0 then
      begin
        repeat
          if SearchRecValidFolder(f) then
            RecursiveAdd(fileDir + f.Name + PathDelim,
              zipDir + f.Name + fZipNamePathDelimString);
        until FindNext(f) <> 0;
        FindClose(f);
      end;
    if FindFirst(fileDir + Mask, faAnyfile - faDirectory, f) = 0 then
    begin
      repeat
        if SearchRecValidFile(f) then
        begin
          cl := CompressLevel;
          zf := zipDir;
          zn := f.Name;
          if (not Assigned(OnAdd)) or
             OnAdd(fileDir, f.Name, cl, zf, zn) then
            AddDeflated(fileDir + f.Name, {removepath=}false, cl, zf + zn);
        end;
      until FindNext(f) <> 0;
      FindClose(f);
    end;
  end;

begin
  RecursiveAdd(IncludeTrailingPathDelimiter(FolderName), '');
end;

procedure TZipWrite.AddFiles(const aFiles: array of TFileName;
  RemovePath: boolean; CompressLevel: integer);
var
  i: PtrInt;
begin
  for i := 0 to high(aFiles) do
    AddDeflated(aFiles[i], RemovePath, CompressLevel);
end;

procedure TZipWrite.AddFromZip(ZipSource: TZipRead; ZipEntry: integer);
var
  local: TLocalFileHeader;
  z: PZipReadEntry;
begin
  if (self <> nil) and
     (ZipSource <> nil) and
     (cardinal(ZipEntry) < cardinal(ZipSource.Count)) then
    with NextEntry^ do
    begin
      // retrieve file information, as expected by WriteHeader()
      z := @ZipSource.Entry[ZipEntry];
      h32 := z^.dir^;
      if z^.dir64 = nil then
      begin
        if (h32.fileInfo.zfullSize = ZIP32_MAXSIZE) or
           (h32.fileInfo.zzipSize = ZIP32_MAXSIZE) or
           (h32.fileInfo.flags and FLAG_DATADESCRIPTOR <> 0) then
          raise ESynZip.CreateUtf8('%.AddFromZip failed on %: unexpected ' +
            'data descriptor (MacOS) format', [self, z^.zipName]);
        h64.zfullSize := h32.fileInfo.zfullSize;
        h64.zzipSize := h32.fileInfo.zzipSize;
      end
      else
        h64 := z^.dir64^; // proper Zip64 support
      // append new header and file content
      fOnProgressStep := zwsWriteFile;
      InfoStart(h64.zzipSize, 'Add ', z^.zipName);
      WriteHeader(z^.zipName);
      if z^.local = nil then
      begin
        local.DataSeek(ZipSource.fSource, z^.localoffs);
        fDest.CopyFrom(ZipSource.fSource, h64.zzipSize);
      end
      else
        fDest.WriteBuffer(z^.local^.Data^, h64.zzipSize);
      if fInfo.ExpectedSize <> 0 then
        fInfo.DoAfter(self, h64.zzipSize);
      inc(fCount);
    end;
end;

procedure TZipWrite.Append(const Content: RawByteString);
begin
  if (self = nil) or
     (Count <> 0) then
    raise ESynZip.Create('TZipWrite.Append: invalid call');
  inc(fAppendOffset, length(Content));
  fDest.WriteBuffer(pointer(Content)^, length(Content));
end;

procedure TZipWrite.FinalFlush;
var
  lh: TLastHeader;
  lh64: TLastHeader64;
  loc64: TLocator64;
  tmp: RawByteString; // efficient single write of all central directory
  P: PAnsiChar;
  i: PtrInt;
begin
  FillcharFast(lh, SizeOf(lh), 0);
  lh.signature := LASTHEADER_SIGNATURE_INC;
  dec(lh.signature); // +1 to avoid finding it in the exe
  FillcharFast(lh64, SizeOf(lh64), 0);
  lh64.headerOffset := QWord(fDest.Position) - fAppendOffset;
  lh64.headerSize := SizeOf(TFileHeader) * Count;
  for i := 0 to Count - 1 do
    with Entry[i] do
    begin
      inc(lh64.headerSize, h32.fileInfo.nameLen);
      if h32.fileInfo.extraLen <> 0 then
      begin
        // zip64 directory header contains an additional 64-bit offset
        inc(h32.fileInfo.extraLen, SizeOf(h64.offset));
        inc(h64.size, SizeOf(h64.offset));
        inc(lh64.headerSize, SizeOf(h64));
      end;
    end;
  SetLength(tmp, lh64.headerSize + SizeOf(lh) + SizeOf(lh64) + SizeOf(loc64));
  P := pointer(tmp);
  for i := 0 to Count - 1 do
    with Entry[i] do
    begin
      PFileHeader(P)^ := h32;
      inc(PFileHeader(P));
      MoveFast(pointer(IntName)^, P^, h32.fileInfo.nameLen);
      inc(P, h32.fileInfo.nameLen);
      if h32.fileInfo.extraLen <> 0 then
      begin
        MoveFast(h64, P^, h32.fileInfo.extraLen);
        inc(P, h32.fileInfo.extraLen);
      end;
    end;
  assert(P - pointer(tmp) = lh64.headerSize);
  if fNeedZip64 or
     (Count >= ZIP32_MAXFILE) or
     (lh64.headerOffset >= ZIP32_MAXSIZE) then
  begin
    // need zip64 format to store more than 65534 files or more than 2GB
    lh64.signature := LASTHEADER64_SIGNATURE_INC;
    dec(lh64.Signature);
    lh64.recordsize := SizeOf(lh64) - SizeOf(lh64.signature) - SizeOf(lh64.recordsize);
    lh64.madeBy := ZIP_VERSION[{zip64=}true];
    lh64.neededVersion := lh64.madeBy;
    lh64.thisFiles := Count;
    lh64.totalFiles := Count;
    loc64.signature := LASTHEADERLOCATOR64_SIGNATURE_INC;
    dec(loc64.signature);
    loc64.thisDisk := 0;
    loc64.headerOffset := lh64.headerOffset + lh64.headerSize; // lh64 offset
    loc64.totalDisks := 1;
    MoveFast(lh64, P^, SizeOf(lh64));
    inc(P, SizeOf(lh64));
    MoveFast(loc64, P^, SizeOf(loc64));
    inc(P, SizeOf(loc64));
    lh.thisFiles := ZIP32_MAXFILE;
    lh.totalFiles := ZIP32_MAXFILE;
    lh.headerSize := ZIP32_MAXSIZE;
    lh.headerOffset := ZIP32_MAXSIZE;
  end
  else
  begin
    // regular .zip format
    lh.thisFiles := Count;
    lh.totalFiles := Count;
    lh.headerSize := lh64.headerSize;
    lh.headerOffset := lh64.headerOffset;
  end;
  PLastHeader(P)^ := lh;
  inc(PLastHeader(P));
  fDest.WriteBuffer(pointer(tmp)^, P - pointer(tmp)); // write at once to fDest
  if fDest.InheritsFrom(THandleStream) then
    SetEndOfFile(THandleStream(fDest).Handle); // may need to be truncated
end;


{ TZipRead }

constructor TZipRead.Create(BufZip: PByteArray; Size: PtrInt; Offset: Int64);
var
  lh: PLastHeader;
  lh64: TLastHeader64;
  loc64: PLocator64;
  h, hnext: PFileHeader;
  e: PZipReadEntry;
  i: PtrInt;
  isascii7: boolean;
  P: PAnsiChar;
  tmp: RawByteString;
begin
  Create;
  if (BufZip = nil) or
     (Size < SizeOf(TLastHeader)) then
     raise ESynZip.CreateUtf8('%.Create(nil)', [self]);
  for i := 0 to 127 do
  begin
    // resources size may be rounded up -> search in trailing 128 bytes
    lh := @BufZip[Size - SizeOf(TLastHeader)];
    if lh^.signature + 1 = LASTHEADER_SIGNATURE_INC then
      break;
    dec(Size);
    if Size <= SizeOf(TLastHeader) then
      break;
  end;
  if lh^.signature + 1 <> LASTHEADER_SIGNATURE_INC then
    raise ESynZip.CreateUtf8('%.Create(%): zip trailer signature not found',
      [self, fFileName]);
  if (lh^.thisFiles = ZIP32_MAXFILE) or
     (lh^.totalFiles = ZIP32_MAXFILE) or
     (lh^.headerSize = ZIP32_MAXSIZE) or
     (lh^.headerOffset = ZIP32_MAXSIZE) then
  begin
    // validate zip64 trailer
    loc64 := pointer(lh);
    dec(loc64);
    if (PtrUInt(loc64) < PtrUInt(BufZip)) or
       (loc64^.signature + 1 <> LASTHEADERLOCATOR64_SIGNATURE_INC) or
       (loc64^.headerOffset + SizeOf(lh64) >= QWord(Offset + Size)) then
      raise ESynZip.CreateUtf8('%.Create(%): zip64 header signature not found',
        [self, fFileName]);
    lh64 := PLastHeader64(@BufZip[loc64^.headerOffset - QWord(Offset)])^;
    if lh64.signature + 1 <> LASTHEADER64_SIGNATURE_INC then
      raise ESynZip.CreateUtf8('%.Create(%): zip64 trailer signature not found',
        [self, fFileName]);
  end
  else
  begin
    // regular .zip content
    lh64.totalFiles := lh^.totalFiles;
    lh64.headerOffset := lh^.headerOffset;
  end;
  fCentralDirectoryOffset := lh64.headerOffset;
  if (fCentralDirectoryOffset <= Offset) or
     (fCentralDirectoryOffset +
       Int64(lh64.totalFiles * SizeOf(TFileHeader)) >= Offset + Size) then
    raise ESynZip.CreateUtf8('%.Create: corrupted Central Dir Offset=% %',
      [self, fCentralDirectoryOffset, fFileName]);
  fCentralDirectoryFirstFile := @BufZip[fCentralDirectoryOffset - Offset];
  SetLength(fEntry, lh64.totalFiles); // Entry[] will contain the Zip headers
  e := pointer(fEntry);
  h := fCentralDirectoryFirstFile;
  for i := 1 to lh64.totalFiles do
  begin
    if (PtrUInt(h) + SizeOf(TFileHeader) >= PtrUInt(@BufZip[Size])) or
       (h^.signature + 1 <> ENTRY_SIGNATURE_INC) or
       (h^.fileInfo.nameLen = 0) then
      raise ESynZip.CreateUtf8('%.Create: corrupted file header #%/% %',
        [self, i, lh64.totalFiles, fFileName]);
    hnext := pointer(PtrUInt(h) + SizeOf(h^) + h^.fileInfo.NameLen +
      h^.fileInfo.extraLen + h^.commentLen);
    if PtrUInt(hnext) >= PtrUInt(@BufZip[Size]) then
      raise ESynZip.CreateUtf8('%: corrupted header %', [self, fFileName]);
    e^.dir := h;
    e^.storedName := PAnsiChar(h) + SizeOf(h^);
    SetString(tmp, e^.storedName, h^.fileInfo.nameLen);
    isascii7 := true;
    P := pointer(tmp);
    repeat
      if P^ = fZipNamePathDelimReversed then // normalize path delimiter
        P^ := fZipNamePathDelim
      else if P^ > #127 then
        isascii7 := false;
      inc(P);
    until P^ = #0;
    if P[-1] = fZipNamePathDelim then
    begin
      h := hnext;
      continue; // ignore void folder entry
    end;
    if isascii7 then
      e^.zipName := Ansi7ToString(tmp)
    else if h^.fileInfo.GetUtf8FileName then
      // decode UTF-8 file name into native string/TFileName type
      Utf8ToFileName(tmp, e^.zipName)
    else
      // legacy Windows-OEM-CP437 encoding - from mormot.core.os
      e^.zipName := OemToFileName(tmp);
    if not (h^.fileInfo.zZipMethod in [Z_STORED, Z_DEFLATED]) then
      raise ESynZip.CreateUtf8('%.Create: Unsupported zipmethod % for % %',
        [self, h^.fileInfo.zZipMethod, e^.zipName, fFileName]);
    if (h^.localHeadOff = ZIP32_MAXSIZE) or
       (h^.fileInfo.zfullSize = ZIP32_MAXSIZE) or
       (h^.fileInfo.zzipSize = ZIP32_MAXSIZE) then
    begin
      // zip64 format: retrieve TFileInfoExtra64
      if (h^.fileInfo.extraLen < SizeOf(TFileInfoExtra64)) or
         not h^.fileInfo.IsZip64 then
        raise ESynZip.CreateUtf8('%.Create: zip64 format error for % %',
          [self, e^.zipName, fFileName]);
      e^.dir64 := pointer(h);
      inc(PByte(e^.dir64), SizeOf(h^) + h^.fileInfo.nameLen);
      if (e^.dir64^.zip64id <> ZIP64_EXTRA_ID) or
         (e^.dir64^.size <> 3 * SizeOf(QWord)) then
        raise ESynZip.CreateUtf8('%.Create: zip64 extra not found for % %',
          [self, e^.zipName, fFileName]);
      e^.localoffs := e^.dir64.offset;
    end
    else
      // regular zip 2.0 format
      e^.localoffs := h^.localHeadOff;
    if e^.localoffs >= Offset then
    begin
      // can unzip directly from existing memory buffer
      e^.local := @BufZip[Int64(e^.localoffs) - Offset];
      with e^.local^.fileInfo do
        if flags and FLAG_DATADESCRIPTOR <> 0 then
          // crc+sizes in "data descriptor" -> call RetrieveFileInfo()
          if (zcrc32 <> 0) or
             (zzipSize <> 0) or
             (zfullSize <> 0) then
            raise ESynZip.CreateUtf8('%.Create: data descriptor (MacOS) with ' +
              'sizes for % %', [self, e^.zipName, fFileName]);
    end;
    h := hnext;
    inc(fCount); // add file to Entry[]
    inc(e);
  end;
end;

constructor TZipRead.Create(Instance: THandle;
  const ResName: string; ResType: PChar);
// resources are memory maps of the executable -> direct access
begin
  if fResource.Open(ResName, ResType, Instance) then
    // warning: resources size may be aligned rounded up -> handled in Create()
    Create(fResource.Buffer, fResource.Size);
end;

function IsZipStart(P: PCardinal): boolean;
  {$ifdef HASINLINE} inline; {$endif}
begin
  // we need to check more than the signature because of false positives
  case P^ + 1 of
    FIRSTHEADER_SIGNATURE_INC:
      with PLocalFileHeader(P)^.fileInfo do
        result := (ToByte(neededVersion) in [10, 20, 45]) and
                  (zzipMethod in [Z_STORED, Z_DEFLATED]) and
                  (extraLen < 100) and
                  (nameLen < 512);
    LASTHEADER_SIGNATURE_INC:
      result := PInt64(@PLastHeader(P)^.totalFiles)^ = 0; // *Disk=0
  else
    result := false;
  end;
end;

constructor TZipRead.Create(aFile: THandle;
  ZipStartOffset, Size, WorkingMem: QWord; DontReleaseHandle: boolean);
var
  read, i: PtrInt;
  P: PByteArray;
  local: TLocalFileHeader;
begin
  if not ValidHandle(aFile) then
    exit;
  // prepare the internal buffer - contains at least the central directory
  if Size = 0 then
    Size := FileSize(aFile);
  if Size < WorkingMem then
    WorkingMem := Size; // up to 1MB by default
  if WorkingMem < 32 then
    raise ESynZip.CreateUtf8('%.Create: No ZIP header found %', [self, fFileName]);
  SetString(fSourceBuffer, nil, WorkingMem);
  P := pointer(fSourceBuffer);
  // search for the first zip file local header
  fSource := TFileStreamFromHandle.Create(aFile);
  TFileStreamFromHandle(fSource).DontReleaseHandle := DontReleaseHandle;
  if ZipStartOffset = 0 then
  begin
    fSource.Seek(0, soBeginning);
    if (fSource.Read(local, SizeOf(local)) = SizeOf(local)) and
       IsZipStart(@local) then
    begin
      // it seems to be a regular .zip -> read WorkingMem trailing content
      fSource.Seek(Size - WorkingMem, soBeginning);
      fSource.Read(P^, WorkingMem);
      Create(P, WorkingMem, Size - WorkingMem);
      exit;
    end;
  end;
  // search FileAppendSignature() mark from the trailing bytes
  fSource.Seek(Size - WorkingMem, soBeginning);
  fSource.Read(P^, WorkingMem);
  for i := WorkingMem - 16 downto WorkingMem - 32 do
    if (i >= 0) and  // expects magic4+offset8+magic4 pattern
       (PCardinal(@P[i])^ + 1 = FILEAPPEND_SIGNATURE_INC) and
       (PCardinal(@P[i + 12])^ + 1 = FILEAPPEND_SIGNATURE_INC) then
    begin
      fSourceOffset := PQWord(@P[i + 4])^;
      if (fSourceOffset > 0) and
         (fSourceOffset < Size - 16) then
      begin
         fSource.Seek(fSourceOffset, soBeginning);
         if (fSource.Read(local, SizeOf(local)) = SizeOf(local)) and
            IsZipStart(@local) then
         begin
           Create(P, i, Size - WorkingMem - fSourceOffset);
           exit;
         end;
      end;
    end;
  // manual search of the first zip local file header
  repeat
    fSource.Seek(ZipStartOffset, soBeginning);
    read := fSource.Read(P^, WorkingMem);
    for i := 0 to read - SizeOf(TLocalFileHeader) do
      if IsZipStart(@P[i]) then
      begin
        fSourceOffset := ZipStartOffset + Qword(i);
        if Size = WorkingMem then
          // small files could reuse the existing buffer
          Create(@P[i], read - i, 0)
        else
        begin
          // big files need to read the last WorkingMem
          fSource.Seek(Size - WorkingMem, soBeginning);
          fSource.Read(P^, WorkingMem);
          Create(P, WorkingMem, Size - WorkingMem - fSourceOffset);
        end;
        exit;
      end;
    inc(ZipStartOffset, WorkingMem - SizeOf(TLocalFileHeader)); // search next
  until read <> WorkingMem;
  raise ESynZip.CreateUtf8('%.Create: No ZIP header found %', [self, fFileName]);
end;

constructor TZipRead.Create(const aFileName: TFileName;
  ZipStartOffset, Size, WorkingMem: QWord);
begin
  fFileName := aFileName;
  Create(FileOpen(aFileName, fmOpenRead or fmShareDenyNone),
    ZipStartOffset, Size, WorkingMem);
end;

destructor TZipRead.Destroy;
begin
  fResource.Close;
  FreeAndNilSafe(fSource);
  inherited Destroy;
end;

function TZipRead.NameToIndex(const aName: TFileName): integer;
var
  normalized: TFileName;
begin
  if (self <> nil) and
     (aName <> '') then
  begin
    // TZipRead did ensure ZipNamePathDelim was stored in Entry[].zipName
    normalized := NormalizeZipName(aName);
    for result := 0 to Count - 1 do
      if SameText(Entry[result].zipName, normalized) then
        exit;
  end;
  result := -1;
end;

type
  // some tools store 0 within local header, and append a "data descriptor"
  TDataDescriptor = packed record
    signature: cardinal;
    crc32: cardinal;
    zipSize: cardinal;
    fullSize: cardinal;
  end;
  PDataDescriptor = ^TDataDescriptor;

function TZipRead.RetrieveLocalFileHeader(Index: integer;
  out Header: TLocalFileHeader): boolean;
var
  e: PZipReadEntry;
begin
  result := false;
  if (self = nil) or
     (cardinal(Index) >= cardinal(Count)) then
    exit;
  // try to get information from central directory
  e := @Entry[Index];
  if e^.local = nil then
    Header.DataSeek(fSource, e^.localoffs + fSourceOffset)
  else
    Header := e^.local^;
  result := true;
end;

function TZipRead.RetrieveFileInfo(Index: integer;
  out Info: TFileInfoFull): boolean;
var
  desc: ^TDataDescriptor;
  e: PZipReadEntry;
  PDataStart: PtrUInt;
  tmp: RawByteString;
  tmplen: integer;
  local: TLocalFileHeader;
begin
  result := false;
  if (self = nil) or
     (cardinal(Index) >= cardinal(Count)) then
    exit;
  // try to get information from central directory
  e := @Entry[Index];
  Info.f32 := e^.dir^.fileInfo;
  FillCharFast(Info.f64, SizeOf(Info.f64), 0);

  if nil = e^.local then
  // this local isn't in our WorkMem, load from disk
    local.DataSeek(fSource, e^.localoffs + fSourceOffset)
  else
    local := e^.local^;
  Info.localfileheadersize := local.Size;

  if 0 <> local.fileInfo.flags and FLAG_DATADESCRIPTOR then
  // we need extract actual size from data descriptor in WorkMem
  begin
    if nil <> e^.local then
    // find data descriptor and extract size from WorkMem
    begin
      if Index < Count - 2 then
        desc := pointer(Entry[Index + 1].local) // search backward from next file
      else
        desc := pointer(fCentralDirectoryFirstFile); // search from central dir
      PDataStart := PtrUInt(e^.local^.Data);
    end else
    // find data descriptor and extract zipped&unzipped size from disk
    begin
      if Index < Count - 2 then
      // read from current data to next local file header
      // maybe we should reduce this tmplen to lower disk-io & mem-usage
        tmplen := Entry[Index + 1].localoffs - e^.localoffs - local.Size + 1
      else
        tmplen := fCentralDirectoryOffset - e^.localoffs - local.Size + 1;
      SetString(tmp, nil, tmplen);
      fSource.Seek(e^.localoffs + local.Size + fSourceOffset, soBeginning);
      if tmplen <> fSource.Read(tmp[1], tmplen) then
        raise ESynZip.CreateUtf8('Error on read data descriptor buffer', []);
      desc := Pointer(@tmp[tmplen]);
      PDataStart := PtrUInt(@tmp[1]);
    end;
    // now we have prepared pointer and buffer
    Dec(desc);
    while PtrUInt(desc) > PDataStart do
    // same pattern than ReadLocalItemDescriptor() in 7-Zip's ZipIn.cpp
    // but here, search is done backwards (much faster than 7-Zip algorithm)
    if (desc^.signature = SIGNATURE_DATADESCRIPTOR) and
       (desc^.zipSize = PtrUInt(desc) - PDataStart) then
    begin
      if (desc^.fullSize = 0) or
         (desc^.zipSize = ZIP32_MAXSIZE) or
         (desc^.fullSize = ZIP32_MAXSIZE) then
        // we expect 32-bit sizes to be available
        exit;
      Info.f32.zcrc32 := desc^.crc32;
      Info.f32.zzipSize := desc^.zipSize;
      Info.f32.zfullSize := desc^.fullSize;
      Info.f64.zzipSize := desc^.zipSize;
      Info.f64.zfullSize := desc^.fullSize;
      result := true;
      exit;
    end
    else
      dec(PByte(desc));
    exit;
  end;
end;

function TZipRead.UnZip(aIndex: integer): RawByteString;
var
  e: PZipReadEntry;
  len: PtrUInt;
  data: pointer;
  tmp: RawByteString;
  info: TFileInfoFull;
  local: TLocalFileHeader;
begin
  if not RetrieveFileInfo(aIndex, info) or
     (info.f64.zzipSize > 128 shl 20) or
     (info.f64.zfullSize > 128 shl 20) then
  begin
    result := '';
    exit;
  end;
  // may call libdeflate_crc32 / libdeflate_deflate_decompress
  SetString(result, nil, info.f64.zfullSize);
  e := @Entry[aIndex];
  if e^.local = nil then
  begin
    local.DataSeek(fSource, e^.localoffs + fSourceOffset);
    case info.f32.zZipMethod of
      Z_STORED:
        begin
          len := info.f64.zfullsize;
          fSource.Read(pointer(result)^, len);
        end;
      Z_DEFLATED:
        begin
          SetString(tmp, nil, info.f64.zzipSize);
          fSource.Read(pointer(tmp)^, info.f64.zzipSize);
          len := UnCompressMem(pointer(tmp), pointer(result),
            info.f64.zzipsize, info.f64.zfullsize);
        end;
    else
      raise ESynZip.CreateUtf8('%.UnZip: Unsupported method % for % %',
        [self, info.f32.zZipMethod, e^.zipName, fFileName]);
    end;
  end
  else
  begin
    data := e^.local^.Data;
    case info.f32.zZipMethod of
      Z_STORED:
        begin
          len := info.f64.zfullsize;
          MoveFast(data^, pointer(result)^, len);
        end;
      Z_DEFLATED:
        len := UnCompressMem(data, pointer(result),
          info.f64.zzipsize, info.f64.zfullsize);
    else
      raise ESynZip.CreateUtf8('%.UnZip: Unsupported method % for % %',
        [self, info.f32.zZipMethod, e^.zipName, fFileName]);
    end;
  end;
  if (len <> info.f64.zfullsize) or
     (mormot.lib.z.crc32(0, pointer(result), len) <> info.f32.zcrc32) then
    raise ESynZip.CreateUtf8('%.UnZip: crc error for % %',
      [self, e^.zipName, fFileName]);
end;

function TZipRead.UnZipStream(aIndex: integer; const aInfo: TFileInfoFull;
  aDest: TStream): boolean;
var
  e: PZipReadEntry;
  crc: cardinal;
  data: pointer;
  deflate: TSynZipDecompressor;
  tmp: RawByteString;
  tmplen, len, written: Int64;
  read: PtrInt;
  local: TLocalFileHeader;
begin
  result := false;
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(Count)) then
    exit;
  len := aInfo.f64.zfullsize;
  if len = 0 then
  begin
    result := true;
    exit;
  end;
  e := @Entry[aIndex];
  if not (aInfo.f32.zZipMethod in [Z_STORED, Z_DEFLATED]) then
    raise ESynZip.CreateUtf8('%.UnZipStream: Unsupported method % for % %',
      [self, aInfo.f32.zZipMethod, e^.zipName, fFileName]);
  InfoStart(len, 'UnZip ', e^.zipName);
  if e^.local = nil then
  begin
    local.DataSeek(fSource, e^.localoffs + fSourceOffset);
    {$ifdef LIBDEFLATESTATIC}
    // files up to 64MB will call libdeflate using a temporary memory buffer
    if (aInfo.f32.zZipMethod = Z_DEFLATED) and
       (len < LIBDEFLATE_MAXSIZE) then
      with aInfo.f64 do
      begin
        SetString(tmp, nil, zzipSize + len); // alloc zip+unziped
        if fSource.Read(pointer(tmp)^, zzipSize) <> zzipSize then
          exit;
        data := @PByteArray(tmp)[zzipsize];
        if UnCompressMem(pointer(tmp), data, zzipsize, len) <> len then
           exit;
        crc := mormot.lib.z.crc32(0, data, len);
        aDest.WriteBuffer(data^, len);
      end
    else
    // big files will use the slower but unlimited zlib streaming abilities
    {$endif LIBDEFLATESTATIC}
    begin
      deflate := nil;
      if aInfo.f32.zZipMethod = Z_DEFLATED then
        deflate := TSynZipDecompressor.Create(aDest, szcfRaw);
      try
        len := aInfo.f64.zzipSize;
        tmpLen := 1 shl 20;
        if len < tmpLen then
          tmpLen := len;
        SetString(tmp, nil, tmpLen);
        crc := 0; // for Z_STORED
        repeat
          if len < tmpLen then
            tmpLen := len;
          read := fSource.Read(pointer(tmp)^, tmpLen);
          if read <= 0 then
            exit; // error reading
          dec(len, read);
          if deflate = nil then
          begin
            crc := mormot.lib.z.crc32(crc, pointer(tmp), read);
            aDest.WriteBuffer(pointer(tmp)^, read);
            written := read;
          end
          else
          begin
            written := deflate.SizeOut;
            deflate.Write(pointer(tmp)^, read);
            if len = 0 then
            begin
              deflate.Flush; // needed for accurate CRC final value
              crc := deflate.CRC;
            end;
            written := deflate.SizeOut - written;
          end;
          if fInfo.ExpectedSize <> 0 then
            fInfo.DoAfter(self, written);
        until len = 0;
      finally
        deflate.Free;
      end;
      fInfo.ExpectedSize := 0; // no final DoAfter() below needed
    end;
  end
  else
  begin
    // directly decompress from the .zip content memory buffer
    data := e^.local^.Data;
    case aInfo.f32.zZipMethod of
      Z_STORED:
        begin
          aDest.WriteBuffer(data^, len);
          crc := mormot.lib.z.crc32(0, data, len);
        end;
      Z_DEFLATED:
        with aInfo.f64 do
        {$ifdef LIBDEFLATESTATIC}
        // files up to 64MB will call libdeflate using a temporary memory buffer
        if len < LIBDEFLATE_MAXSIZE then
        begin
          SetString(tmp, nil, len);
          if UnCompressMem(data, pointer(tmp), zzipsize, len) <> len then
             exit;
          crc := mormot.lib.z.crc32(0, pointer(tmp), len);
          aDest.WriteBuffer(pointer(tmp)^, len);
        end
        else
        // big files will use the slower but unlimited zlib streaming abilities
        {$endif LIBDEFLATESTATIC}
        if UnCompressStream(data, zzipsize, aDest, @crc) <> len then
          exit;
    end;
  end;
  if fInfo.ExpectedSize <> 0 then // notify final if was not done above
    fInfo.DoAfter(self, aInfo.f64.zfullSize);
  result := crc = aInfo.f32.zcrc32;
end;

function TZipRead.UnZip(aIndex: integer; aDest: TStream): boolean;
var
  info: TFileInfoFull;
begin
  if not RetrieveFileInfo(aIndex, info) then
    result := false
  else
    result := UnZipStream(aIndex, info, aDest);
end;

function TZipRead.UnZip(aIndex: integer; const DestDir: TFileName;
  DestDirIsFileName: boolean): boolean;
var
  FS: TFileStream;
  LocalZipName, Dest: TFileName;
  info: TFileInfoFull;
begin
  result := false;
  if not RetrieveFileInfo(aIndex, info) then
    exit;
  if DestDirIsFileName then
    Dest := DestDir
  else
  begin
    LocalZipName := Entry[aIndex].zipName;
    if fZipNamePathDelim <> PathDelim then
      LocalZipName := StringReplace(
        LocalZipName, fZipNamePathDelimString, PathDelim, [rfReplaceAll]);
    if not SafeFileName(LocalZipName) then
      raise ESynZip.CreateUtf8('%.UnZip(%): unsafe file name ''%''',
        [self, fFileName, LocalZipName]);
    Dest := EnsureDirectoryExists(DestDir + ExtractFilePath(LocalZipName));
    if Dest = '' then
      exit;
    Dest := Dest + ExtractFileName(LocalZipName);
  end;
  if Entry[aIndex].dir^.IsFolder then
    result := EnsureDirectoryExists(Dest) <> ''
  else
  begin
    FS := TFileStream.Create(Dest, fmCreate);
    try
      result := UnZipStream(aIndex, info, FS);
    finally
      FS.Free;
    end;
  end;
  FileSetDateFromWindowsTime(Dest, info.f32.zlastMod);
end;

function TZipRead.UnZipAll(DestDir: TFileName): integer;
begin
  DestDir := EnsureDirectoryExists(DestDir);
  for result := 0 to Count - 1 do
    if not UnZip(result, DestDir) then
      exit;
  result := -1;
end;

function TZipRead.TestAll: boolean;
var
  i: integer;
  fake: TStream;
begin
  result := false;
  fake := TFakeWriterStream.Create;
  try
    for i := 0 to Count - 1 do
      if not UnZip(i, fake) then
        exit;
  finally
    fake.Free;
  end;
  result := true;
end;

function TZipRead.UnZip(const aName, DestDir: TFileName;
  DestDirIsFileName: boolean): boolean;
var
  aIndex: integer;
begin
  aIndex := NameToIndex(aName);
  if aIndex < 0 then
    result := false
  else
    result := UnZip(aIndex, DestDir, DestDirIsFileName);
end;

function TZipRead.UnZip(const aName: TFileName): RawByteString;
var
  aIndex: integer;
begin
  aIndex := NameToIndex(aName);
  if aIndex < 0 then
    result := ''
  else
    result := UnZip(aIndex);
end;


var
  EventArchiveZipWrite: TZipWrite = nil;

function EventArchiveZip(const aOldLogFileName, aDestinationPath: TFileName): boolean;
var
  n: integer;
begin
  result := false;
  if aOldLogFileName = '' then
    FreeAndNilSafe(EventArchiveZipWrite)
  else
  begin
    if not FileExists(aOldLogFileName) then
      exit;
    if EventArchiveZipWrite = nil then
      EventArchiveZipWrite := TZipWrite.CreateFrom(
        copy(aDestinationPath, 1, length(aDestinationPath) - 1) + '.zip');
    n := EventArchiveZipWrite.Count;
    EventArchiveZipWrite.AddDeflated(aOldLogFileName, {removepath=}true);
    if (EventArchiveZipWrite.Count = n + 1) and
       DeleteFile(aOldLogFileName) then
      result := true;
  end;
end;

function ZipTest(const ZipName: TFileName): boolean;
var
  ZR: TZipRead;
begin
  if FileExists(ZipName) then
    try
      ZR := TZipRead.Create(ZipName);
      try
        result := ZR.TestAll;
      finally
        ZR.Free;
      end;
    except
      result := false; // interpret exception as wrong .zip format
    end
  else
    result := false;
end;

procedure FileAppendSignature(O: TStream; APos: Int64);
var
  magic: cardinal;
begin
  magic := FILEAPPEND_SIGNATURE_INC; // searched by TZipRead() to retrieve APos
  dec(magic);
  O.WriteBuffer(magic, SizeOf(magic));
  O.WriteBuffer(APos, SizeOf(APos));
  O.WriteBuffer(magic, SizeOf(magic));
end;


procedure FileAppend(const MainFile, AppendFile: TFileName);
var
  M, A: TStream;
  APos: Int64;
begin
  M := TFileStream.Create(MainFile, fmOpenReadWrite);
  try
    APos := M.Seek(0, soEnd);
    A := TFileStream.Create(AppendFile, fmOpenRead);
    try
      M.CopyFrom(A, 0);
      FileAppendSignature(M, APos);
    finally
      A.Free;
    end;
  finally
    M.Free;
  end;
end;

procedure InternalFileAppend(const ctxt: ShortString;
  const main, append, new, zipfolder, mask: TFileName; flag: boolean;
  const onadd: TOnZipWriteAdd; const zipfiles: array of TFileName;
  level: integer; keepdigitalsign: boolean);
const
  CERTIFICATE_ENTRY_OFFSET = 148;
var
  M, A, O: TStream;
  i, read: PtrInt;
  Apos, Asize: Int64;
  parsePEheader: boolean;
  certoffs, certlenoffs, certlen, certlen2: cardinal;
  zip: TZipWrite;
  buf: array[0 .. 128 shl 10 - 1] of byte; // 128KB of temp copy buffer on stack
begin
  if new = main then
    raise ESynZip.CreateUtf8('%: main=new=%', [ctxt, main]);
  certoffs := 0;
  certlenoffs := 0;
  O := TFileStream.Create(new, fmCreate);
  try
    try
      parsePEheader := keepdigitalsign;
      // copy main source file
      M := TFileStream.Create(main, fmOpenRead or fmShareDenyNone);
      try
        repeat
          read := M.Read(buf, SizeOf(buf));
          if read < 0 then
            raise ESynZip.CreateUtf8('%: % read error', [ctxt, main]);
          if parsePEheader then
          begin
            // search for COFF/PE header in the first block
            i := 0;
            repeat
              if i >= read - (CERTIFICATE_ENTRY_OFFSET + 8) then
                raise ESynZip.CreateUtf8(
                  '%: % is not a PE executable', [ctxt, main]);
              if PCardinal(@buf[i])^ = ord('P') + ord('E') shl 8 then
                break; // typical DOS header is $100
              inc(i);
            until false;
            // parse PE header
            inc(i, CERTIFICATE_ENTRY_OFFSET);
            certoffs := PCardinal(@buf[i])^;
            certlenoffs := i + 4;
            certlen  := PCardinal(@buf[certlenoffs])^;
            // parse certificate table
            M.Seek(certoffs, soBeginning);
            if (M.Read(certlen2, 4) <> 4) or
               (certlen2 <> certlen) then
              raise ESynZip.CreateUtf8('%: % has no certificate', [ctxt, main]);
            if certoffs + certlen <> M.Size then
              raise ESynZip.CreateUtf8(
                '%: % should end with a certificate', [ctxt, main]);
            M.Seek(read, soBeginning);
            parsePEheader := false; // do it once
          end;
          if read > 0 then
            O.WriteBuffer(buf, read);
        until read < SizeOf(buf);
      finally
        M.Free;
      end;
      // append the additional payload - which may be some zipped files
      APos := O.Position;
      if append <> '' then
      begin
        A := TFileStream.Create(append, fmOpenRead or fmShareDenyNone);
        try
          repeat
            read := A.Read(buf, SizeOf(buf));
            if read > 0 then
              O.WriteBuffer(buf, read);
          until read < SizeOf(buf);
        finally
          A.Free;
        end;
      end
      else
      begin
        zip := TZipWrite.CreateAppend(O, new);
        try
          if zipfolder <> '' then
            zip.AddFolder(zipfolder, mask, flag, level, onadd)
          else if high(zipfiles) >= 0 then
            zip.AddFiles(zipfiles, flag, level)
          else
            raise ESynZip.CreateUtf8('%: invalid call for %', [ctxt, main]);
        finally
          zip.Free;
        end;
      end;
      FileAppendSignature(O, APos);
      if keepdigitalsign then
      begin
        // the certificate length should be 64-bit aligned -> pad the payload
        ASize := O.Position - APos;
        while ASize and 7 <> 0 do
        begin
          O.WriteBuffer(buf[0], 1);
          inc(ASize);
        end;
        // include appended content to the certificate length
        inc(certlen, ASize);
        O.Seek(certlenoffs, soBeginning); // in PE header
        O.WriteBuffer(certlen, 4);
        O.Seek(certoffs, soBeginning);    // in the certificate table
        O.WriteBuffer(certlen, 4);
      end;
    except
      DeleteFile(new); // aborted file is clearly invalid
    end;
  finally
    O.Free;
  end;
end;

procedure FileAppend(const MainFile, AppendFile, NewFile: TFileName;
  PreserveWinDigSign: boolean);
begin
  InternalFileAppend('FileAppend', MainFile, AppendFile, NewFile,
    '', '', false, nil, [], 0, PreserveWinDigSign);
end;

procedure ZipAppendFolder(const MainFile, NewFile, ZipFolder, Mask: TFileName;
  Recursive: boolean; CompressionLevel: integer;
  const OnAdd: TOnZipWriteAdd; PreserveWinDigSign: boolean);
begin
  InternalFileAppend('ZipAppendFolder', MainFile, '', NewFile,
    ZipFolder, Mask, Recursive, OnAdd, [], CompressionLevel, PreserveWinDigSign);
end;

procedure ZipAppendFiles(const MainFile, NewFile: TFileName;
  const ZipFiles: array of TFileName; RemovePath: boolean;
  CompressionLevel: integer; PreserveWinDigSign: boolean);
begin
  InternalFileAppend('ZipAppendFiles', MainFile, '', NewFile,
    '', '', RemovePath, nil, ZipFiles, CompressionLevel, PreserveWinDigSign);
end;

const
  HTTP_LEVEL = 1; // 6 is standard, but 1 is enough and faster

function CompressGZip(var Data: RawByteString; Compress: boolean): RawUtf8;
var
  max, L, C: integer;
  P: PAnsiChar;
  tmp: RawByteString;
begin
  L := length(Data);
  if Compress then
  begin
    max := zlibCompressMax(L);
    SetString(tmp, nil, max + (GZHEAD_SIZE + 8));
    P := pointer(tmp);
    MoveFast(GZHEAD, P^, GZHEAD_SIZE);
    inc(P, GZHEAD_SIZE);
    C := CompressMem(pointer(Data), P, L, max, HTTP_LEVEL);
    if C <= 0 then // error (maybe from libdeflate_deflate_compress)
      Data := ''
    else
    begin
      inc(P, C);
      PCardinal(P)^ := crc32(0, pointer(Data), L); // maybe libdeflate_crc32
      inc(P, 4);
      PCardinal(P)^ := L;
      inc(P, 4);
      PStrLen(PAnsiChar(pointer(tmp)) - _STRLEN)^ := P - pointer(tmp); // no realloc
      Data := tmp;
    end;
  end
  else
    Data := gzread(pointer(Data), L);
  result := 'gzip';
end;

procedure CompressRaw(var Data: RawByteString; Compress, ZLib: boolean);
var
  src: RawByteString;
  max, L: integer;
begin
  src := Data;
  L := length(src);
  if Compress then
  begin
    max := zlibCompressMax(L);
    SetString(Data, nil, max);
    L := CompressMem(pointer(src), pointer(Data), L, max, HTTP_LEVEL, ZLib);
    if L <= 0 then
      Data := ''
    else
      PStrLen(PAnsiChar(pointer(Data)) - _STRLEN)^ := L; // fake len: no realloc
  end
  else
    Data := UnCompressZipString(pointer(src), L, nil, ZLib, 0);
end;

function CompressDeflate(var Data: RawByteString; Compress: boolean): RawUtf8;
begin
  CompressRaw(Data, Compress, {zlib=}false);
  result := 'deflate';
end;

function CompressZLib(var Data: RawByteString; Compress: boolean): RawUtf8;
begin
  CompressRaw(Data, Compress, {zlib=}true);
  result := 'zlib';
end;

{$ifndef PUREMORMOT2}

function CompressString(const data: RawByteString; failIfGrow: boolean;
  CompressionLevel: integer): RawByteString;
var
  len : integer;
begin
  result := '';
  SetLength(result, 12 + zlibCompressMax(length(data)));
  PInt64(result)^ := length(data);
  PCardinalArray(result)^[2] := adler32(0, pointer(data), length(data));
  // use faster libdeflate instead of plain zlib if available
  len := CompressMem(pointer(data), @PByteArray(result)[12], length(data),
    length(result) - 12, CompressionLevel);
  if (len > 0) and
     ( (12 + len < length(data)) or
       not failIfGrow ) then
    SetLength(result, 12 + len)
  else
    result := '';
end;

function UncompressString(const data: RawByteString): RawByteString;
begin
  result := '';
  if Length(data) > 12 then
  begin
    SetLength(result, PCardinal(data)^);
    // use faster libdeflate instead of plain zlib if available
    SetLength(result, UncompressMem(@PByteArray(data)[12], pointer(result),
      length(data) - 12, length(result)));
    if (result <> '') and
       ((Adler32(0, pointer(result), length(result))) <> PCardinalArray(data)^[2]) then
      result := '';
  end;
end;

{$endif PUREMORMOT2}


{ TStreamRedirectCrc32 }

procedure TStreamRedirectCrc32.DoHash(data: pointer; len: integer);
begin
  fHash := mormot.lib.z.crc32(fHash, data, len);
end;

class function TStreamRedirectCrc32.GetHashFileExt: RawUtf8;
begin
  result := '.crc32';
end;


function crc32wrapper(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := mormot.lib.z.crc32(crc, buf, len);
end;

function HashFileCrc32(const FileName: TFileName): RawUtf8;
begin
  result := CardinalToHexLower(HashFile(FileName, crc32wrapper));
end;


{ ************ TAlgoDeflate and TAlgoDeflate High-Level Compression Algorithms }

{ TAlgoDeflate }

type
  // implements the AlgoDeflate global variable
  TAlgoDeflate = class(TAlgoCompressWithNoDestLen)
  protected
    fDeflateLevel: integer;
    function RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer; override;
  public
    /// set AlgoID = 2 as genuine byte identifier for Deflate
    constructor Create; override;
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
  end;

constructor TAlgoDeflate.Create;
begin
  if fAlgoID = 0 then
    fAlgoID := 2;
  inherited Create;
  fDeflateLevel := 6;
end;

function TAlgoDeflate.RawProcess(src, dst: pointer; srcLen, dstLen,
  dstMax: integer; process: TAlgoCompressWithNoDestLenProcess): integer;
begin
  case process of
    doCompress:
      result := mormot.lib.z.CompressMem(
        src, dst, srcLen, dstLen, fDeflateLevel);
    doUnCompress,
    doUncompressPartial:
      result := mormot.lib.z.UnCompressMem(
        src, dst, srcLen, dstLen);
  else
    result := 0;
  end;
end;

function TAlgoDeflate.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  result := zlibCompressMax(PlainLen);
end;


{ TAlgoDeflateFast }

type
  // implements the AlgoDeflateFast global variable
  TAlgoDeflateFast = class(TAlgoDeflate)
  public
    /// set AlgoID = 2 as genuine byte identifier for Deflate Fast
    constructor Create; override;
  end;

constructor TAlgoDeflateFast.Create;
begin
  fAlgoID := 3;
  inherited Create;
  fDeflateLevel := 1;
end;


initialization
  AlgoDeflate := TAlgoDeflate.Create;
  AlgoDeflateFast := TAlgoDeflateFast.Create;
  
end.

