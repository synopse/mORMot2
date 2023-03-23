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
    /// 32-bit size of compressed data - may equal ZIP32_MAXSIZE on Zip64
    zzipSize: cardinal;
    /// 32-bit size of uncompressed data - may equal ZIP32_MAXSIZE on Zip64
    zfullSize: cardinal;
    /// length(name) as appended just after this block
    nameLen: word;
    /// length(extra) - e.g. SizeOf(TFileInfoExtra64)
    extraLen: word;
    /// guess/comparison of the content of two TFileInfo - check zcrc32 + sizes
    function SameAs(aInfo: PFileInfo): boolean;
    /// set the the UTF-Language encoding flag (EFS)
    procedure SetUtf8FileNameFlag;
      {$ifdef HASINLINE}inline;{$endif}
    /// remove the the UTF-Language encoding flag (EFS)
    procedure UnSetUtf8FileNameFlag;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if flags contains the UTF-Language encoding flag (EFS)
    function GetUtf8FileNameFlag: boolean;
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

  //// extra file information structure header
  TFileInfoExtra = record
    /// contains the ID of the block, e.g. ZIP64_EXTRA_ID or UNICODEPATH_EXTRA_ID
    id: word;
    /// block size in bytes
    size: word;
  end;
  PFileInfoExtra = ^TFileInfoExtra;

  //// extra file information structure, as used in zip64 file format
  // - warning: some fields may be missing on disk - here only if ZIP32_MAXSIZE
  TFileInfoExtra64 = record
    /// contains ZIP64_EXTRA_ID = 0x0001
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

  //// extra file information structure, storing UTF-8 path name
  // - as generated e.g. by the build-in zip on latest Windows 10 Home
  // - see 'Unicode Path Extra Field' in Info-ZIP APPNOTE.TXT
  TFileInfoExtraName = record
    /// contains UNICODEPATH_EXTRA_ID = $7075
    id: word;
    /// block size
    size: word;
    /// version of this extra field, currently 1
    version: byte;
    /// File Name Field CRC32 Checksum
    nameCRC32: cardinal;
    /// UTF-8 version of the entry File Name of size bytes length
    utf8Name: AnsiChar;
  end;
  PFileInfoExtraName = ^TFileInfoExtraName;

  /// information returned by RetrieveFileInfo()
  TFileInfoFull = record
    /// standard zip file information, as written on disk
    // - use f64 field values for actual 64-bit zzipSize/zfullSize/offset
    f32: TFileInfo;
    /// 64-bit file zzipSize/zfullSize/offset information
    // - contains f32 values or parsed zip64 extension if f32.IsZip64 is true
    f64: TFileInfoExtra64;
    /// actual size of the stored local file header
    localsize: PtrInt;
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
    /// system-depending file attributes, typically $A0 on MSDOS
    extFileAttr: cardinal;
    /// 32-bit offset to @TLocalFileHeader
    localHeadOff: cardinal;
    /// check if extFileAttr contains the folder flag (bit 4)
    // - not to be used in practice, since it is system-dependent: checking
    // for trailing / \ character seems the way to go
    function IsDosFolder: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize the signature/madeby/fileinfo field information
    procedure SetVersion(NeedZip64: boolean);
    /// search for a file info extension up to extraLen bytes
    function LocateExtra(aID: word): pointer;
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
    /// fill the local file header from Source input stream
    procedure Load(Source: TStream; LocalOffset: Int64);
    /// move Source position to the data part of this file content
    // - jump over fileInfo.extraLen + fileInfo.nameLen bytes
    procedure LoadAndDataSeek(Source: TStream; LocalOffset: Int64);
  end;
  PLocalFileHeader = ^TLocalFileHeader;

  //// last header structure, as used in .zip file format
  // - those 22 bytes end the file and are used to find the TFileHeader entries
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

  /// locator structure, as used in zip64 file format
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
    // - warning: some fields may be missing - match ZIP32_MAXSIZE marked fields
    dir64: PFileInfoExtra64;
    /// points to the local file header in the .zip archive, stored in memory
    // - local^.data points to the stored/deflated data
    // - may be nil if the file size is bigger than WorkingMem
    local: PLocalFileHeader;
    /// parsed file information, zip64-ready
    fileinfo: TFileInfoExtra64;
    /// name of the file inside the .zip archive
    // - not ASCIIZ: length = dir^.fileInfo.nameLen
    storedName: PAnsiChar;
    /// points to the next local file header in the archive, stored in memory
    // - may be a folder, which do not appear with regular Entry[]
    // - only used for "data descriptor" files (MacOS)
    nextlocal: PLocalFileHeader;
    /// offset to the next local file header in the .zip archive
    // - only used for "data descriptor" files (MacOS)
    nextlocaloffs: QWord;
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
    procedure NormalizeIntZipName(var intName: RawByteString);
  public
    /// initialize this class
    constructor Create;
    /// the number of files inside a .zip archive
    // - never trust the Entry[] array, which length is used as growing
    // capacity so is likely to be bigger than the actual Count
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
    // - at least at process startup and finish, and following ReportDelay ms
    // for the methods supporting it, i.e. if streams are used, not libdeflate
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
    fCentralDirectory: PFileHeader;
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
    constructor Create(Instance: TLibHandle; const ResName: string;
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
    // - will refuse to uncompress more than aMaxSize - i.e. 128 MB of content
    function UnZip(aIndex: integer; aMaxSize: Int64 = 128 shl 20): RawByteString; overload;
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
    // - use the Count property to find out how many files are stored
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
    /// add an empty folder to the zip file
    // - warning: aZipName is expected to have a / or \ trailing path delimiter
    procedure AddEmptyFolder(const aZipName: TFileName; FileAge: integer = 0);
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
    // - IncludeVoidFolders=TRUE would include void folders entries to the zip
    // - returns the number of files added
    function AddFolder(const FolderName: TFileName;
      const Mask: TFileName = FILES_ALL; Recursive: boolean = true;
      CompressLevel: integer = 6; const OnAdd: TOnZipWriteAdd = nil;
      IncludeVoidFolders: boolean = false): integer;
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
    // - this array is likely to have a capacity bigger than the actual Count:
    // always use Count, not length(Entry) or "for entry in ZipWrite.Entry"
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
// - if PreserveWinDigSign is set, the Windows PE digital signature is kept using
// https://blog.barthe.ph/2009/02/22/change-signed-executable legacy method -
// but such naive "append" is now rejected by Windows, so StuffExeCertificate()
// from mormot.crypt.secure is to be used instead
procedure FileAppend(const MainFile, AppendFile, NewFile: TFileName;
  PreserveWinDigSign: boolean = false); overload;

/// zip a folder content after the end of MainFile into NewFile
// - PreserveWinDigSign legacy method is rejected by modern Windows, so
// StuffExeCertificate() from mormot.crypt.secure is to be used instead
procedure ZipAppendFolder(const MainFile, NewFile, ZipFolder: TFileName;
  const Mask: TFileName = FILES_ALL; Recursive: boolean = true;
  CompressionLevel: integer = 6; const OnAdd: TOnZipWriteAdd = nil;
  PreserveWinDigSign: boolean = false);

/// zip a some files after the end of MainFile into NewFile
// - PreserveWinDigSign legacy method is rejected by modern Windows, so
// StuffExeCertificate() from mormot.crypt.secure is to be used instead
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
var
  // libdeflate + AVX is much faster than zlib, but its API expects only buffers
  // - files up to 64MB will call libdeflate and a temporary memory buffer
  LIBDEFLATE_MAXSIZE: Int64 = 64 shl 20;
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
          (Origin in [soCurrent, soEnd]) then
    // for TStream.Position/GetSize on Delphi
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
    f := TFileStreamEx.Create(filename, fmCreate);
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
  s, d: TStream;
begin
  try
    s := TFileStreamEx.Create(orig, fmOpenReadDenyNone);
    try
      d := TFileStreamEx.Create(destgz, fmCreate);
      try
        gz := TSynZipCompressor.Create(d, CompressionLevel, szcfGZ);
        try
          StreamCopyUntilEnd(s, gz); // faster and safer than gz.CopyFrom(s, 0);
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

  ZIP64_EXTRA_ID       = $0001; // Zip64 extended information
  NTFS_EXTRA_ID        = $000a; // NTFS
  UNIX_EXTRA_ID        = $000d; // UNIX
  EXT_TIME_EXTRA_ID    = $5455; // Extended timestamp
  INFOZIP_UNIX_EXTRAID = $5855; // Info-ZIP Unix extension
  UNICODEPATH_EXTRA_ID = $7075; // Unicode Path extended information

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

procedure TLocalFileHeader.Load(Source: TStream; LocalOffset: Int64);
begin
  if Source = nil then
    raise ESynZip.Create('Zip: LoadAndDataSeek with Source=nil');
  // we read the real local header content before calling Size for its offset
  Source.Seek(LocalOffset, soBeginning);
  if Source.Read(self, SizeOf(self)) <> SizeOf(self) then
    raise ESynZip.Create('Zip: LoadAndDataSeek reading error');
end;

procedure TLocalFileHeader.LoadAndDataSeek(Source: TStream; LocalOffset: Int64);
begin
  Load(Source, LocalOffset);
  // now we can compute and ignore the local header size -> seek on data
  Source.Seek(LocalOffset + Size, soBeginning);
end;


{ TFileHeader }

function TFileHeader.IsDosFolder: boolean;
begin
  result := (@self <> nil) and
            (extFileAttr and $10 <> 0);
end;

procedure TFileHeader.SetVersion(NeedZip64: boolean);
begin
  signature := ENTRY_SIGNATURE_INC;
  dec(signature); // constant was +1 to avoid finding it in the exe
  madeBy := ZIP_VERSION[NeedZip64];
  extFileAttr := $A0; // archive, normal (ignored by most readers)
  fileInfo.neededVersion := madeBy;
end;

function TFileHeader.LocateExtra(aID: word): pointer;
var
  p: PFileInfoExtra;
  remaining, len: PtrInt;
begin
  result := nil;
  remaining := fileInfo.extraLen;
  if remaining = 0 then
    exit;
  p := pointer(PAnsiChar(@self) + sizeof(self) + fileInfo.NameLen);
  repeat
    if p^.id = aID then
    begin
      result := p; // we found it
      exit;
    end;
    len := p^.size + SizeOf(p^);
    inc(PByte(p), len);
    dec(remaining, len);
  until remaining <= 0;
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

function TFileInfo.GetUtf8FileNameFlag: boolean;
begin
  // from PKware appnote, Bit 11: Language encoding flag (EFS)
  result := (flags and (1 shl 11)) <> 0;
end;

procedure TFileInfo.SetUtf8FileNameFlag;
begin
  flags := flags or (1 shl 11);
end;

procedure TFileInfo.UnSetUtf8FileNameFlag;
begin
  flags := flags and not (1 shl 11);
end;

function TFileInfo.IsZip64: boolean;
begin
  result := ToByte(neededVersion) >= 45; // ignore OS flag from ZIP_VERSION[]
end;

function IsFolder(const zipName: TFileName): boolean;
begin
  result := (zipName <> '') and
            (ord(zipName[length(zipName)]) in [ord('\'), ord('/')]);
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

procedure TZipAbstract.NormalizeIntZipName(var intName: RawByteString);
var
  i, L: PtrInt;
begin
  // normalize TZipWriteEntry.intName
  L := length(intName);
  i := ByteScanIndex(pointer(intName), L, ord(fZipNamePathDelimReversed));
  if i >= 0 then
    repeat
      inc(i);
      if intName[i] = fZipNamePathDelimReversed then
        intName[i] := fZipNamePathDelim;
    until i = L;
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
  Create(TFileStreamEx.Create(aDestFileName, fmCreate));
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
            if writepos >= Int64(s^.fileinfo.offset) then
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
                  FastSetRawByteString(tmp, nil, 1 shl 20);
                readpos := Int64(s^.fileinfo.offset) + info.localsize;
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
              if d^.h32.fileInfo.extraLen <> SizeOf(d^.h64) then
                // e.g. TFileInfoExtra64 with TFileInfoExtraName extension
                raise ESynZip.CreateUtf8(
                  '%.CreateFrom unsupported fileinfo.extralen for % in %',
                  [self, s^.zipName, aFileName]);
              d^.h32.localHeadOff := ZIP32_MAXSIZE;
              dec(d^.h32.fileInfo.extraLen, SizeOf(d^.h64.offset));
              dec(d^.h64.size, SizeOf(d^.h64.offset));
            end;
            SetString(d^.intName, s^.storedName, d^.h32.fileInfo.nameLen);
            NormalizeIntZipName(d^.intName);
            inc(writepos, info.localsize + Int64(info.f64.zzipSize));
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
    {$ifdef UNICODE}
    if IsAnsiCompatibleW(pointer(zipName)) then
    {$else}
    if IsAnsiCompatible(zipName) then
    {$endif UNICODE}
      // ZIP should handle CP437 charset, but fails sometimes (e.g. Korean)
      intName := StringToAnsi7(zipName)
    else
    begin
      // safe UTF-8 file name encoding
      intName := StringToUtf8(zipName);
      include(flags, zweUtf8Name);
      h32.fileInfo.SetUtf8FileNameFlag;
    end;
    h32.fileInfo.nameLen := length(intName);
    NormalizeIntZipName(intName);
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
    if ByteScanIndex(pointer(P), h32.fileInfo.nameLen, ord(fZipNamePathDelimReversed)) >= 0 then
      raise ESynZip.CreateUtf8('%.WriteRawHeader(%)', [self, intName]); // paranoid
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

procedure TZipWrite.AddEmptyFolder(const aZipName: TFileName; FileAge: integer);
begin
  NewEntry(Z_STORED, cardinal(-1), FileAge);
  WriteHeader(aZipName); // no data to write nor any fInfo progress to notify
  inc(fCount);
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
  f := FileOpen(aFileName, fmOpenReadDenyNone);
  if ValidHandle(f) then
    try
      // retrieve file size and date
      todo := FileSeek64(f, 0, soFromEnd);
      FileSeek64(f, 0, soFromBeginning);
      age := FileAgeToWindowsTime(aFileName);
      // check if the file should be stored or use libdeflate
      met := Z_DEFLATED;
      if (CompressLevel < 0) or
         (todo < ZIP_MINSIZE_DEFLATE) then
        met := Z_STORED; // tiny file, or called from AddStored()
      {$ifdef LIBDEFLATESTATIC}
      // libdeflate is much faster than zlib, but its API expects only buffers
      if (met = Z_DEFLATED) and
         (Int64(todo) <= LIBDEFLATE_MAXSIZE) then
      begin
        // files up to 64MB will be loaded into memory and call libdeflate
        Setlength(tmp, todo);
        if not FileReadAll(f, pointer(tmp), todo) then
          raise ESynZip.CreateUtf8('%.AddDeflated: failed to read % [%]',
            [self, aFileName, GetLastError]);
        AddDeflated(ZipName, pointer(tmp), todo, CompressLevel, age);
        exit;
      end;
      // bigger/stored files will fallback to zlib streaming methods
      {$endif LIBDEFLATESTATIC}
      // prepare new entry and write initial version of the local file header
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
        // append the stored/deflated data using TSynZipCompressor stream
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
              raise ESynZip.CreateUtf8('%.AddDeflated: failed to read % [%]',
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
            if deflate.SizeIn <> h64.zfullSize then
              raise ESynZip.CreateUtf8('%.AddDeflated: failed to deflate %',
                [self, aFileName]);
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

function TZipWrite.AddFolder(const FolderName: TFileName;
  const Mask: TFileName; Recursive: boolean; CompressLevel: integer;
  const OnAdd: TOnZipWriteAdd; IncludeVoidFolders: boolean): integer;

  function RecursiveAdd(const fileDir, zipDir: TFileName): integer;
  var
    f: TSearchRec;
    cl: integer;
    zf, zn: TFileName;
  begin
    result := 0;
    if Recursive then
      if FindFirst(fileDir + FILES_ALL, faDirectory, f) = 0 then
      begin
        repeat
          if SearchRecValidFolder(f) then
          begin
            zf := zipDir + f.Name + fZipNamePathDelimString;
            inc(result, RecursiveAdd(fileDir + f.Name + PathDelim, zf));
          end;
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
          begin
            AddDeflated(fileDir + f.Name, {removepath=}false, cl, zf + zn);
            inc(result);
          end;
        end;
      until FindNext(f) <> 0;
      FindClose(f);
    end;
    if IncludeVoidFolders and
       (zipDir <> '') and
       (result = 0) and
       (Mask = FILES_ALL) then
    begin
      AddEmptyFolder(zipDir, SearchRecToWindowsTime(f));
      inc(result);
    end;
  end;

begin
  result := RecursiveAdd(IncludeTrailingPathDelimiter(FolderName), '');
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
      if (z^.dir64 = nil) and
         (h32.fileInfo.flags and FLAG_DATADESCRIPTOR <> 0) then
        raise ESynZip.CreateUtf8('%.AddFromZip failed on %: unexpected ' +
          'data descriptor (MacOS) format', [self, z^.zipName]);
      h32 := z^.dir^;
      h64:= z^.fileinfo; // from TZipRead.Create()
      // append new header and file content
      fOnProgressStep := zwsWriteFile;
      InfoStart(h64.zzipSize, 'Add ', z^.zipName);
      WriteHeader(z^.zipName);
      if h64.zzipSize <> 0 then
        if z^.local = nil then
        begin
          local.LoadAndDataSeek(ZipSource.fSource, z^.fileinfo.offset);
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

const
  /// direct conversion from code page 437 to UTF-16, as zip appnote requires
  // - on POSIX, a constant table is easier and lighter than running external
  // iconv/ICU from mormot.core.os (in UTF-8 POSIX, we don't need code pages)
  CP_437: array[128..255] of word = (
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8,
    $00EF, $00EE, $00EC, $00C4, $00C5, $00C9, $00E6, $00C6, $00F4, $00F6, $00F2,
    $00FB, $00F9, $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192, $00E1,
    $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $2310, $00AC, $00BD,
    $00BC, $00A1, $00AB, $00BB, $2591, $2592, $2593, $2502, $2524, $2561, $2562,
    $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510, $2514, $2534,
    $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560,
    $2550, $256C, $2567, $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
    $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580, $03B1, $00DF, $0393,
    $03C0, $03A3, $03C3, $03BC, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6,
    $03B5, $2229, $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0,
    $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);

procedure Cp437ToFileName(const oem: RawByteString; out filename: TFileName);
var
  len, i, c: PtrInt;
  utf16: SynUnicode;
begin
  len := length(oem);
  SetLength(utf16, len);
  for i := 0 to len - 1 do
  begin
    c := PByteArray(oem)[i];
    if c > 127 then
      c := CP_437[c];
    PWordArray(utf16)[i] := c;
  end;
  filename := SynUnicodeToString(utf16);
end;


{ TZipRead }

function LocateLastHeader(BufZip: PByteArray; var Size: PtrInt;
  Offset: Int64; out head64: PLastHeader64): PLastHeader;
var
  i: PtrInt;
  loc64: PLocator64;
begin
  if (BufZip = nil) or
     (Size < SizeOf(TLastHeader)) then
     raise ESynZip.Create('TZipRead.Create(nil)');
  for i := 0 to 127 do
  begin
    // resources size may be rounded up -> search in trailing 128 bytes
    result := @BufZip[Size - SizeOf(TLastHeader)];
    if result^.signature + 1 = LASTHEADER_SIGNATURE_INC then
    begin
      if (result^.thisFiles = ZIP32_MAXFILE) or
         (result^.totalFiles = ZIP32_MAXFILE) or
         (result^.headerSize = ZIP32_MAXSIZE) or
         (result^.headerOffset = ZIP32_MAXSIZE) then
      begin
        // validate zip64 trailer
        loc64 := pointer(result);
        dec(loc64);
        if (PtrUInt(loc64) < PtrUInt(BufZip)) or
           (loc64^.signature + 1 <> LASTHEADERLOCATOR64_SIGNATURE_INC) or
           (loc64^.headerOffset + SizeOf({%H-}head64^) >= QWord(Offset + Size)) then
          raise ESynZip.Create('zip64 header signature not found');
        head64 := @BufZip[loc64^.headerOffset - QWord(Offset)];
        if head64^.signature + 1 <> LASTHEADER64_SIGNATURE_INC then
          raise ESynZip.Create('zip64 trailer signature not found');
      end
      else
        // regular zip 2.0 trailer
        head64 := nil;
      exit;
    end;
    dec(Size);
    if Size <= SizeOf(TLastHeader) then
      break;
  end;
  result := nil;
end;

function LocateCentralDirectoryOffset(BufZip: PByteArray; Size: PtrInt;
  Offset: Int64): Int64;
var
  lh32: PLastHeader;
  lh64: PLastHeader64;
begin
  lh32 := LocateLastHeader(BufZip, Size, Offset, lh64);
  if lh32 = nil then
    raise ESynZip.Create('zip trailer signature not found');
  if lh64 <> nil then
    result := lh64^.headerOffset
  else
    result := lh32^.headerOffset;
end;

constructor TZipRead.Create(BufZip: PByteArray; Size: PtrInt; Offset: Int64);
var
  lh32: PLastHeader;
  lh64: PLastHeader64;
  lastheader: TLastHeader64;
  h, hnext: PFileHeader;
  extraname: PFileInfoExtraName;
  e, prev: PZipReadEntry;
  i: PtrInt;
  isascii7: boolean;
  P: PAnsiChar;
  p64: PQWord;
  tmp: RawByteString;
begin
  Create;
  if (BufZip = nil) or
     (Size < SizeOf(TLastHeader)) then
     raise ESynZip.CreateUtf8('%.Create(nil)', [self]);
  lh32 := LocateLastHeader(BufZip, Size, Offset, lh64);
  if lh32 = nil then
    raise ESynZip.CreateUtf8('%.Create(%): zip trailer signature not found',
      [self, fFileName]);
  if lh64 <> nil then
    // zip64 support
    lastheader := lh64^
  else
  begin
    // regular .zip content
    lastheader.totalFiles := lh32^.totalFiles;
    lastheader.headerOffset := lh32^.headerOffset;
  end;
  fCentralDirectoryOffset := lastheader.headerOffset;
  if (fCentralDirectoryOffset <= Offset) or
     (fCentralDirectoryOffset +
       Int64(lastheader.totalFiles * SizeOf(TFileHeader)) >= Offset + Size) then
    raise ESynZip.CreateUtf8(
      '%.Create: corrupted Central Directory or too small WorkMem (Offset=%) %',
      [self, fCentralDirectoryOffset, fFileName]);
  fCentralDirectory := @BufZip[fCentralDirectoryOffset - Offset];
  SetLength(fEntry, lastheader.totalFiles); // Entry[] contain the Zip headers
  e := pointer(fEntry);
  prev := nil;
  h := fCentralDirectory;
  for i := 1 to lastheader.totalFiles do
  begin
    if (PtrUInt(h) + SizeOf(TFileHeader) >= PtrUInt(@BufZip[Size])) or
       (h^.signature + 1 <> ENTRY_SIGNATURE_INC) or
       (h^.fileInfo.nameLen = 0) then
      raise ESynZip.CreateUtf8('%.Create: corrupted file header #%/% %',
        [self, i, lastheader.totalFiles, fFileName]);
    hnext := pointer(PtrUInt(h) + SizeOf(h^) + h^.fileInfo.NameLen +
      h^.fileInfo.extraLen + h^.commentLen);
    if PtrUInt(hnext) >= PtrUInt(@BufZip[Size]) then
      raise ESynZip.CreateUtf8('%: corrupted header in %', [self, fFileName]);
    e^.dir := h;
    e^.storedName := PAnsiChar(h) + SizeOf(h^);
    SetString(tmp, e^.storedName, h^.fileInfo.nameLen); // better for FPC
    isascii7 := true;
    P := pointer(tmp);
    repeat
      if P^ = fZipNamePathDelimReversed then // normalize path delimiter
        P^ := fZipNamePathDelim
      else if P^ > #127 then
        isascii7 := false;
      inc(P);
    until P^ = #0;
    if isascii7 then
      // plain ASCII file name need no conversion
      e^.zipName := Ansi7ToString(tmp)
    else
    begin
      extraname := h^.LocateExtra(UNICODEPATH_EXTRA_ID);
      if extraname <> nil then
      begin
        // unicode path stored in file info extra block
        SetString(tmp, pansichar(@extraname.utf8Name), extraname.size - 5{version+nameCrc});
        Utf8ToFileName(tmp, e^.zipName)
      end
      else if h^.fileInfo.GetUtf8FileNameFlag or
              IsValidUtf8(tmp) then
        // flag let decode UTF-8 file name into native string/TFileName type
        // also detects UTF-8 without the flag (happens from POSIX zippers)
        Utf8ToFileName(tmp, e^.zipName)
      else
        // legacy Windows-OEM-CP437 encoding
        Cp437ToFileName(tmp, e^.zipName);
    end;
    if not (h^.fileInfo.zZipMethod in [Z_STORED, Z_DEFLATED]) and
       not IsFolder(e^.zipName) then
      raise ESynZip.CreateUtf8('%.Create: Unsupported zipmethod % for % in %',
        [self, h^.fileInfo.zZipMethod, e^.zipName, fFileName]);
    { TFileInfoExtra64 is the layout of the zip64 extended info "extra" block.
      If one of the size or offset fields in the Local or Central directory
      record is too small to hold the required data, a Zip64 extended information
      record is created. The order of the fields in the zip64 extended information
      record is fixed, but the fields **MUST only appear** if the corresponding
      Local or Central directory record field is set to 0xFFFF or 0xFFFFFFFF. }
    if (h^.localHeadOff = ZIP32_MAXSIZE) or
       (h^.fileInfo.zfullSize = ZIP32_MAXSIZE) or
       (h^.fileInfo.zzipSize = ZIP32_MAXSIZE) then
      // zip64 format: retrieve TFileInfoExtra64 position in dir64
      if h^.fileInfo.IsZip64 then
      begin
        e^.dir64 := h^.LocateExtra(ZIP64_EXTRA_ID);
        if e^.dir64 = nil then
          raise ESynZip.CreateUtf8('%.Create: zip64 extra not found for % in %',
            [self, e^.zipName, fFileName]);
        e^.fileinfo.zip64id := ZIP64_EXTRA_ID;
        e^.fileinfo.size := 3 * SizeOf(QWord);
      end
      else
        raise ESynZip.CreateUtf8('%.Create: zip64 version % error for % in %',
          [self, ToByte(h^.fileInfo.neededVersion), e^.zipName, fFileName]);
    p64 := @e^.dir64^.zfullSize; // where 64-bit field(s) may appear
    if e^.dir^.fileInfo.zfullSize = ZIP32_MAXSIZE then
    begin
      if e^.dir64 = nil then
        raise ESynZip.CreateUtf8('zip64 FS format error for % in %',
          [e^.zipName, fFileName]);
      e^.fileinfo.zfullSize := p64^;
      inc(p64); // go to the next field
    end
    else
      e^.fileinfo.zfullSize := e^.dir^.fileInfo.zfullSize;
    if e^.dir^.fileInfo.zzipSize = ZIP32_MAXSIZE then
    begin
      if e^.dir64 = nil then
        raise ESynZip.CreateUtf8('zip64 ZS format error for % in %',
          [e^.zipName, fFileName]);
      e^.fileinfo.zzipSize := p64^;
      inc(p64);
    end
    else
      e^.fileinfo.zzipSize := e^.dir^.fileInfo.zzipSize;
    if e^.dir^.localHeadOff = ZIP32_MAXSIZE then
      if e^.dir64 = nil then
        raise ESynZip.CreateUtf8('zip64 HO format error for % in %',
          [e^.zipName, fFileName])
      else
        e^.fileinfo.offset := p64^
    else
      e^.fileinfo.offset := e^.dir^.localHeadOff;
    if e^.fileinfo.offset >= QWord(Offset) then
    begin
      // we can unzip directly from the existing memory buffer: store pointer
      e^.local := @BufZip[Int64(e^.fileinfo.offset) - Offset];
      with e^.local^.fileInfo do
        if flags and FLAG_DATADESCRIPTOR <> 0 then
          // crc+sizes in "data descriptor" -> call RetrieveFileInfo()
          if (zcrc32 <> 0) or
             (zzipSize <> 0) or
             (zfullSize <> 0) then
            raise ESynZip.CreateUtf8('%.Create: data descriptor (MacOS) with ' +
              'sizes for % %', [self, e^.zipName, fFileName]);
      if prev <> nil then
        prev^.nextlocal := e^.local;
    end;
    if prev <> nil then
      prev^.nextlocaloffs := e^.fileinfo.offset;
    prev := e;
    inc(fCount); // add file (or folder) to Entry[]
    inc(e);
    h := hnext;
  end;
  if prev <> nil then
    prev^.nextlocaloffs := fCentralDirectoryOffset; // last file backward search
  if fCount = 0 then
    fEntry := nil
  else if fCount <> lastheader.totalFiles then
    DynArrayFakeLength(fEntry, fCount); // so that length(Entry)=Count 
end;

constructor TZipRead.Create(Instance: TLibHandle;
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
  centraldirsize: Int64;
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
  FastSetRawByteString(fSourceBuffer, nil, WorkingMem);
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
      fSource.ReadBuffer(P^, WorkingMem);
      centraldirsize := Int64(Size) - LocateCentralDirectoryOffset(
        P, WorkingMem, Size - WorkingMem);
      if centraldirsize > Int64(WorkingMem) then
      begin
        // 1MB of WorkingMem was not enough (a lot of files indeed!)
        WorkingMem := centraldirsize + 1024;
        if WorkingMem > Size then
          WorkingMem := Size;
        FastSetRawByteString(fSourceBuffer, nil, WorkingMem); // alloc bigger
        P := pointer(fSourceBuffer);
        fSource.Seek(Size - WorkingMem, soBeginning);
        fSource.ReadBuffer(P^, WorkingMem);
      end;
      Create(P, WorkingMem, Size - WorkingMem);
      exit;
    end;
  end;
  // search FileAppendSignature() mark from the trailing bytes
  fSource.Seek(Size - WorkingMem, soBeginning);
  fSource.ReadBuffer(P^, WorkingMem);
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
          fSource.ReadBuffer(P^, WorkingMem);
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
var
  h: TLibHandle;
begin
  fFileName := aFileName;
  h := FileOpen(aFileName, fmOpenReadDenyNone);
  Create(h, ZipStartOffset, Size, WorkingMem);
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
  // some (MacOs) tools store 0 within local header, and append "data descriptor"
  TDataDescriptor = packed record
    signature: cardinal;
    crc32: cardinal;
    zipSize: cardinal;
    fullSize: cardinal;
  end;

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
    Header.Load(fSource, e^.fileinfo.offset + fSourceOffset)
  else
    Header := e^.local^;
  result := true;
end;

function TZipRead.RetrieveFileInfo(Index: integer;
  out Info: TFileInfoFull): boolean;
var
  desc: ^TDataDescriptor;
  e: PZipReadEntry;
  descmin, tmpLen: PtrUInt;
  posi: Int64;
  local: TLocalFileHeader;
  tmp: array[0..63] of TDataDescriptor; // search for a few iterations is enough
begin
  result := false;
  if (self = nil) or
     (cardinal(Index) >= cardinal(Count)) then
    exit;
  // try to get information from central directory
  e := @Entry[Index];
  Info.f32 := e^.dir^.fileInfo;
  FillCharFast(Info.f64, SizeOf(Info.f64), 0);
  if e^.local <> nil then
    local := e^.local^
  else
    local.Load(fSource, e^.fileinfo.offset + fSourceOffset);
  Info.localsize := local.Size;
  if local.fileInfo.flags and FLAG_DATADESCRIPTOR = 0 then
  begin
    // it seems we can use the central directory information
    Info.f64 := e^.fileinfo;
    result := true;
    exit;
  end;
  // MacOS: search manually the "data descriptor" from the binary local data
  descmin := 0;
  if e^.local <> nil then
    descmin := PtrUInt(e^.local^.Data);
  // search backward from next file to current file
  desc := pointer(e^.nextlocal);
  if (desc = nil) or
     (descmin = 0) then
  begin
    // this file is not within WorkingMem: search from disk
    if e^.nextlocaloffs = 0 then // paranoid
      raise ESynZip.CreateUtf8('%: datadesc with nextlocaloffs=0', [self]);
    tmpLen := e^.nextlocaloffs - e^.fileinfo.offset;
    if tmpLen > SizeOf(tmp) then
      tmpLen := SizeOf(tmp); // search backward up to 1024 bytes
    repeat
      posi := e^.nextlocaloffs + fSourceOffset - tmpLen;
      if posi >= 0 then
        break;
      dec(tmpLen); // paranoid resize for last file of a very small zip
    until tmpLen = 0;
    fSource.Seek(posi, soBeginning);
    if PtrUInt(fSource.Read(tmp, tmpLen)) <> tmpLen then
      raise ESynZip.CreateUtf8('%: data descriptor read error on % %',
        [self, e^.zipName, fFileName]);
    descmin := PtrUInt(@tmp);
    desc := pointer(descmin + tmpLen);
    e := nil; // indicates below that descmin does not match zipSize
  end;
  dec(desc);
  while PtrUInt(desc) > descmin do
    // same pattern than ReadLocalItemDescriptor() in 7-Zip's ZipIn.cpp
    // but here, search is done backwards (much faster than 7-Zip algorithm)
    if (desc^.signature = SIGNATURE_DATADESCRIPTOR) and
       ((e = nil) or
        (desc^.zipSize = PtrUInt(desc) - descmin)) then
    begin
      if (desc^.fullSize = 0) or
         (desc^.zipSize  = ZIP32_MAXSIZE) or
         (desc^.fullSize = ZIP32_MAXSIZE) then
        // we expect 32-bit sizes only (no Zip64 support from MacOS)
        exit;
      Info.f32.zcrc32    := desc^.crc32;
      Info.f32.zzipSize  := desc^.zipSize;
      Info.f32.zfullSize := desc^.fullSize;
      Info.f64.zzipSize  := desc^.zipSize;
      Info.f64.zfullSize := desc^.fullSize;
      result := true;
      exit;
    end
    else
      dec(PByte(desc));
end;

function TZipRead.UnZip(aIndex: integer; aMaxSize: Int64): RawByteString;
var
  e: PZipReadEntry;
  len: PtrUInt;
  data: pointer;
  tmp: RawByteString;
  info: TFileInfoFull;
begin
  if not RetrieveFileInfo(aIndex, info) or
     (info.f64.zfullSize = 0) or
     (info.f64.zzipSize > aMaxSize) or
     (info.f64.zfullSize > aMaxSize) then
  begin
    result := '';
    exit;
  end;
  // call libdeflate_crc32 / libdeflate_deflate_decompress if available
  FastSetRawByteString(result, nil, info.f64.zfullSize);
  e := @Entry[aIndex];
  if e^.local = nil then
  begin
    fSource.Seek(
      e^.fileinfo.offset + fSourceOffset + PtrUInt(info.localsize), soBeginning);
    case info.f32.zZipMethod of
      Z_STORED:
        begin
          len := info.f64.zfullsize;
          fSource.ReadBuffer(pointer(result)^, len);
        end;
      Z_DEFLATED:
        begin
          FastSetRawByteString(tmp, nil, info.f64.zzipSize);
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
    local.LoadAndDataSeek(fSource, e^.fileinfo.offset + fSourceOffset);
    {$ifdef LIBDEFLATESTATIC}
    // files up to 64MB will call libdeflate using a temporary memory buffer
    if (aInfo.f32.zZipMethod = Z_DEFLATED) and
       (len < LIBDEFLATE_MAXSIZE) then
      with aInfo.f64 do
      begin
        FastSetRawByteString(tmp, nil, zzipSize + len); // alloc zip+unziped
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
        FastSetRawByteString(tmp, nil, tmpLen);
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
          FastSetRawByteString(tmp, nil, len);
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
  FS: TStream;
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
  if IsFolder(Entry[aIndex].zipName) then
    result := EnsureDirectoryExists(Dest) <> ''
  else
  begin
    FS := TFileStreamEx.Create(Dest, fmCreate);
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
  pos: Int64;
begin
  M := TFileStreamEx.Create(MainFile, fmOpenReadWrite);
  try
    pos := M.Seek(0, soEnd);
    A := TFileStreamEx.Create(AppendFile, fmOpenReadDenyNone);
    try
      StreamCopyUntilEnd(A, M); // faster than M.CopyFrom(A, 0);
      FileAppendSignature(M, pos);
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
  CERTIFICATE_ENTRY_OFFSET = 152;
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
  O := TFileStreamEx.Create(new, fmCreate);
  try
    try
      parsePEheader := keepdigitalsign;
      // copy main source file
      M := TFileStreamEx.Create(main, fmOpenReadDenyNone);
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
        A := TFileStreamEx.Create(append, fmOpenReadDenyNone);
        try
          StreamCopyUntilEnd(A, O);
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
        buf[0] := 0;
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
    FastSetRawByteString(tmp, nil, max + (GZHEAD_SIZE + 8));
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
      FakeLength(tmp, P - pointer(tmp)); // no realloc
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
    FastSetRawByteString(Data, nil, max);
    L := CompressMem(pointer(src), pointer(Data), L, max, HTTP_LEVEL, ZLib);
    if L <= 0 then
      Data := ''
    else
      FakeLength(Data, L);
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
  if fAlgoID = 0 then // TAlgoDeflateFast.Create may have already set it
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
    /// set AlgoID = 3 as genuine byte identifier for Deflate Fast
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

