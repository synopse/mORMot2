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
  mormot.core.buffers, // for TAlgoCompress
  mormot.core.unicode,
  mormot.lib.z;


{ ************ TSynZipCompressor Stream Class }

type
  /// the format used for storing data
  TSynZipCompressorFormat = (
    szcfRaw,
    szcfZip,
    szcfGZ);

  /// a TStream descendant for compressing data into a stream using Zip/Deflate
  TSynZipCompressor = class(TStream)
  private
    fInitialized: boolean;
    fDestStream: TStream;
    Z: TZLib;
    fCRC: cardinal;
    fFormat: TSynZipCompressorFormat;
    {$ifdef FPC}
    function GetPosition: Int64; override;
    {$endif FPC}
  public
    /// create a compression stream, writting the compressed data into
    // the specified stream (e.g. a file stream)
    constructor Create(outStream: TStream; CompressionLevel: integer;
      Format: TSynZipCompressorFormat = szcfRaw);
    /// release memory
    destructor Destroy; override;
    /// this method will raise an error: it's a compression-only stream
    function Read(var Buffer; Count: Longint): Longint; override;
    /// add some data to be compressed
    function Write(const Buffer; Count: Longint): Longint; override;
    /// used to return the current position, i.e. the real byte written count
    // - for real seek, this method will raise an error: it's a compression-only stream
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// write all pending compressed data into outStream
    procedure Flush;
    /// the number of byte written, i.e. the current uncompressed size
    function SizeIn: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// the number of byte sent to the destination stream, i.e. the current
    // compressed size
    function SizeOut: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// the current CRC of the written data, i.e. the uncompressed data CRC
    property CRC: cardinal
      read fCRC;
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
    neededVersion : word;            // $14
    flags         : word;            // 0
    zzipMethod    : word;            // 0=Z_STORED 8=Z_DEFLATED 12=BZ2 14=LZMA
    zlastMod      : integer;         // time in dos format
    zcrc32        : cardinal;           // crc32 checksum of uncompressed data
    zzipSize      : cardinal;           // size of compressed data
    zfullSize     : cardinal;           // size of uncompressed data
    nameLen       : word;            // length(name)
    extraLen      : word;            // 0
    function SameAs(aInfo: PFileInfo): boolean;
    // 1..15  (1=SynLZ e.g.) from flags bits 7..10 and method=Z_STORED
    procedure SetAlgoID(Algorithm: integer);
    function GetAlgoID: integer;
      {$ifdef HASINLINE}inline;{$endif}
    function GetUTF8FileName: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetUTF8FileName;
      {$ifdef HASINLINE}inline;{$endif}
    procedure UnSetUTF8FileName;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// directory file information structure, as used in .zip file format
  // - used at the end of the zip file to recap all entries
  TFileHeader = object
    signature     : cardinal;           // $02014b50 PK#1#2
    madeBy        : word;            // $14
    fileInfo      : TFileInfo;
    commentLen    : word;            // 0
    firstDiskNo   : word;            // 0
    intFileAttr   : word;            // 0 = binary; 1 = text
    extFileAttr   : cardinal;           // dos file attributes
    localHeadOff  : cardinal;           // @TLocalFileHeader
    function IsFolder: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    procedure Init;
  end;
  PFileHeader = ^TFileHeader;

  /// internal file information structure, as used in .zip file format
  // - used locally inside the file stream, followed by the name and then the data
  TLocalFileHeader = object
    signature     : cardinal;           // $04034b50 PK#3#4
    fileInfo      : TFileInfo;
    function LocalData: PAnsiChar;
  end;
  PLocalFileHeader = ^TLocalFileHeader;

  /// last header structure, as used in .zip file format
  // - this header ends the file and is used to find the TFileHeader entries
  TLastHeader = record
    signature     : cardinal;           // $06054b50 PK#5#6
    thisDisk      : word;            // 0
    headerDisk    : word;            // 0
    thisFiles     : word;            // 1
    totalFiles    : word;            // 1
    headerSize    : cardinal;           // sizeOf(TFileHeaders + names)
    headerOffset  : cardinal;           // @TFileHeader
    commentLen    : word;            // 0
  end;
  PLastHeader = ^TLastHeader;

{$A+}

type
  /// stores an entry of a file inside a .zip archive
  TZipEntry = record
    /// the information of this file, as stored locally in the .zip archive
    // - note that infoLocal^.zzipSize/zfullSize/zcrc32 may be 0 if the info
    // was stored in a "data descriptor" block after the data: in this case,
    // you should use TZipRead.RetrieveFileInfo() instead of this structure
    infoLocal: PFileInfo;
    /// the information of this file, as stored at the end of the .zip archive
    // - may differ from infoLocal^ content, depending of the zipper tool used
    infoDirectory: PFileHeader;
    /// points to the compressed data in the .zip archive, mapped in memory
    data: PAnsiChar;
    /// name of the file inside the .zip archive
    // - not ASCIIZ: length = infoLocal.nameLen
    storedName: PAnsiChar;
    /// name of the file inside the .zip archive
    // - converted from DOS/OEM or UTF-8 into generic (Unicode) string
    zipName: TFileName;
  end;

  /// read-only access to a .zip archive file
  // - can open directly a specified .zip file (will be memory mapped for fast access)
  // - can open a .zip archive file content from a resource (embedded in the executable)
  // - can open a .zip archive file content from memory
  TZipRead = class
  private
    fMap: TMemoryMap;
    fFirstFileHeader: PFileHeader;
    fReadOffset: cardinal;
    fResource: TExecutableResource;
    fFileName: TFileName;
    function UnZipStream(aIndex: integer; const aInfo: TFileInfo;
      aDest: TStream): boolean;
  public
    /// the number of files inside a .zip archive
    Count: integer;
    /// the files inside the .zip archive
    Entry: array of TZipEntry;

    /// open a .zip archive file as Read Only
    constructor Create(const aFileName: TFileName; ZipStartOffset: cardinal = 0;
      Size: cardinal = 0); overload;
    /// open a .zip archive file directly from a resource
    constructor Create(Instance: THandle; const ResName: string;
      ResType: PChar); overload;
    /// open a .zip archive file from its File Handle
    constructor Create(aFile: THandle; ZipStartOffset: cardinal = 0;
      Size: cardinal = 0; aFileOwned: boolean = false); overload;
    /// open a .zip archive file directly from memory
    constructor Create(BufZip: PByteArray; Size: cardinal); overload;
    /// release associated memory
    destructor Destroy; override;

    /// get the index of a file inside the .zip archive
    function NameToIndex(const aName: TFileName): integer;
    /// uncompress a file stored inside the .zip archive into memory
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
    /// uncompress all fields stored inside the .zip archive into the supplied
    // destination directory
    // - returns -1 on success, or the index in Entry[] of the failing file
    function UnZipAll(DestDir: TFileName): integer;
    /// retrieve information about a file
    // - in some cases (e.g. for a .zip created by latest Java JRE),
    // infoLocal^.zzipSize/zfullSize/zcrc32 may equal 0: this method is able
    // to retrieve the information either from the ending "central directory",
    // or by searching the "data descriptor" block
    // - returns TRUE if the Index is correct and the info was retrieved
    // - returns FALSE if the information was not successfully retrieved
    function RetrieveFileInfo(Index: integer; var Info: TFileInfo): boolean;
  end;

  /// abstract write-only access for creating a .zip archive
  TZipWriteAbstract = class
  protected
    fAppendOffset: cardinal;
    fMagic: cardinal;
    function InternalAdd(const zipName: TFileName; Buf: pointer; Size: integer): cardinal;
    function InternalWritePosition: cardinal; virtual; abstract;
    procedure InternalWrite(const buf; len: cardinal); virtual; abstract;
  public
    /// the total number of entries
    Count: integer;
    /// the resulting file entries, ready to be written as a .zip catalog
    // - those will be appended after the data blocks at the end of the .zip file
    Entry: array of record
      /// the file name, as stored in the .zip internal directory
      intName: RawByteString;
      /// the corresponding file header
      fhr: TFileHeader;
    end;
    /// initialize the .zip archive
    // - a new .zip file content is prepared
    constructor Create;
    /// compress (using the deflate method) a memory buffer, and add it to the zip file
    // - by default, current date/time is used if no FileAge is supplied; see also
    // DateTimeToWindowsFileTime() and FileAgeToWindowsTime()
    procedure AddDeflated(const aZipName: TFileName; Buf: pointer; Size: integer;
      CompressLevel: integer = 6; FileAge: integer = 0); overload;
    /// add a memory buffer to the zip file, without compression
    // - content is stored, not deflated
    // (in that case, no deflate code is added to the executable)
    // - by default, current date/time is used if no FileAge is supplied; see also
    // DateTimeToWindowsFileTime() and FileAgeToWindowsTime()
    procedure AddStored(const aZipName: TFileName; Buf: pointer; Size: integer;
      FileAge: integer = 0);
    /// append a file content into the destination file
    // - useful to add the initial Setup.exe file, e.g.
    procedure Append(const Content: RawByteString);
    /// release associated memory, and close destination archive
    destructor Destroy; override;
  end;

  /// write-only access for creating a .zip archive file
  // - not to be used to update a .zip file, but to create a new one
  // - update can be done manualy by using a TZipRead instance and the
  // AddFromZip() method
  TZipWrite = class(TZipWriteAbstract)
  protected
    fFileName: TFileName;
    function InternalWritePosition: cardinal; override;
    procedure InternalWrite(const buf; len: cardinal); override;
  public
    /// the associated file handle
    Handle: integer;
    /// initialize the .zip file
    // - a new .zip file content is created
    constructor Create(const aFileName: TFileName); overload;
    /// initialize an existing .zip file in order to add some content to it
    // - warning: AddStored/AddDeflated() won't check for duplicate zip entries
    // - this method is very fast, and will increase the .zip file in-place
    // (the old content is not copied, new data is appended at the file end)
    // - "dummy" parameter exists only to disambiguate constructors for C++
    constructor CreateFrom(const aFileName: TFileName; dummy: integer = 0);
    /// compress (using the deflate method) a file, and add it to the zip file
    procedure AddDeflated(const aFileName: TFileName;
      RemovePath: boolean = true; CompressLevel: integer = 6;
      ZipName: TFileName = ''); overload;
    /// compress (using the deflate method) all files within a folder, and
    // add it to the zip file
    // - if Recursive is TRUE, would include files from nested sub-folders
    procedure AddFolder(const FolderName: TFileName;
      const Mask: TFileName = FILES_ALL; Recursive: boolean = true;
      CompressLevel: integer = 6);
    /// add a file from an already compressed zip entry
    procedure AddFromZip(const ZipEntry: TZipEntry);
    /// release associated memory, and close destination file
    destructor Destroy; override;
  end;

  /// write-only access for creating a .zip archive into a stream
  TZipWriteToStream = class(TZipWriteAbstract)
  protected
    fDest: TStream;
    function InternalWritePosition: cardinal; override;
    procedure InternalWrite(const buf; len: cardinal); override;
  public
    /// initialize the .zip archive
    // - a new .zip file content is prepared
    constructor Create(aDest: TStream);
  end;


/// a TSynLogArchiveEvent handler which will compress older .log files
// into .zip archive files
// - resulting file will be named YYYYMM.zip and will be located in the
// aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM.zip'
function EventArchiveZip(const aOldLogFileName, aDestinationPath: TFileName): boolean;



/// (un)compress a data content using the gzip algorithm
// - as expected by THttpSocket.RegisterCompress
// - will use internaly a level compression of 1, i.e. fastest available (content
// of 4803 bytes is compressed into 700, and time is 440 us instead of 220 us)
function CompressGZip(var Data: RawByteString; Compress: boolean): RawUtf8;

/// (un)compress a data content using the Deflate algorithm (i.e. "raw deflate")
// - as expected by THttpSocket.RegisterCompress
// - will use internaly a level compression of 1, i.e. fastest available (content
// of 4803 bytes is compressed into 700, and time is 440 us instead of 220 us)
// - deflate content encoding is pretty inconsistent in practice, so slightly
// slower CompressGZip() is preferred - http://stackoverflow.com/a/9186091
function CompressDeflate(var Data: RawByteString; Compress: boolean): RawUtf8;

/// (un)compress a data content using the zlib algorithm
// - as expected by THttpSocket.RegisterCompress
// - will use internaly a level compression of 1, i.e. fastest available (content
// of 4803 bytes is compressed into 700, and time is 440 us instead of 220 us)
// - zlib content encoding is pretty inconsistent in practice, so slightly
// slower CompressGZip() is preferred - http://stackoverflow.com/a/9186091
function CompressZLib(var Data: RawByteString; Compress: boolean): RawUtf8;


{ ************ TAlgoDeflate and TAlgoDeflate High-Level Compression Algorithms }

var
  /// acccess to Zip Deflate compression in level 6 as a TAlgoCompress class
  AlgoDeflate: TAlgoCompress;

  /// acccess to Zip Deflate compression in level 1 as a TAlgoCompress class
  AlgoDeflateFast: TAlgoCompress;




implementation


{ ************ TSynZipCompressor Stream Class }

const
  GZHEAD_SIZE = 10;
  GZHEAD: array[0..2] of cardinal = (
    $088B1F, 0, 0);


{ TSynZipCompressor }

constructor TSynZipCompressor.Create(outStream: TStream; CompressionLevel: integer;
  Format: TSynZipCompressorFormat = szcfRaw);
begin
  fDestStream := outStream;
  fFormat := Format;
  if fFormat = szcfGZ then
    fDestStream.WriteBuffer(GZHEAD, GZHEAD_SIZE);
  Z.Init(nil, 0, outStream, nil, nil, 0, 128 shl 10);
  fInitialized := Z.CompressInit(CompressionLevel, fFormat = szcfZip);
end;

procedure TSynZipCompressor.Flush;
begin
  if not fInitialized then
    exit;
  while (Z.Check(Z.Compress(Z_FINISH),
          [Z_OK, Z_STREAM_END], 'TSynZipCompressor.Flush') <> Z_STREAM_END) and
        (Z.Stream.avail_out = 0) do
    Z.DoFlush;
  Z.DoFlush;
end;

function TSynZipCompressor.SizeIn: PtrUInt;
begin
  result := Z.Stream.total_in;
end;

function TSynZipCompressor.SizeOut: PtrUInt;
begin
  result := Z.Stream.total_out;
end;

destructor TSynZipCompressor.Destroy;
begin
  if fInitialized then
  begin
    Flush;
    Z.CompressEnd;
  end;
  if fFormat = szcfGZ then
  begin
    // .gz format expected a trailing header
    fDestStream.WriteBuffer(fCRC, 4); // CRC of the uncompressed data
    fDestStream.WriteBuffer(Z.Stream.total_in, 4); // truncated to 32-bit
  end;
  inherited Destroy;
end;

function TSynZipCompressor.{%H-}Read(var Buffer; Count: Longint): Longint;
begin
  {$ifdef DELPHI20062007}
  result := 0;
  {$endif DELPHI20062007}
  raise ESynZip.Create('TSynZipCompressor.Read is not supported');
end;

{$ifdef FPC}
function TSynZipCompressor.GetPosition: Int64;
begin
  result := Z.Stream.total_in;
end;
{$endif FPC}

function TSynZipCompressor.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if not fInitialized then
    result := 0
  else if (Offset = 0) and
          (Origin = soFromCurrent) then
    // for TStream.Position
    result := Z.Stream.total_in
  else
  begin
    result := 0;
    if (Offset <> 0) or
       (Origin <> soFromBeginning) or
       (Z.Stream.total_in <> 0) then
      raise ESynZip.CreateFmt('Unexpected %.Seek', [ClassNameShort(self)^]);
  end;
end;

function TSynZipCompressor.Write(const Buffer; Count: Longint): Longint;
begin
  if (self = nil) or
     not fInitialized or
     (Count <= 0) then
  begin
    result := 0;
    exit;
  end;
  Z.Stream.next_in := pointer(@Buffer);
  Z.Stream.avail_in := Count;
  fCRC := mormot.lib.z.crc32(fCRC, Z.Stream.next_in, Z.Stream.avail_in);
  while Z.Stream.avail_in > 0 do
  begin
    // compress pending data
    Z.Check(Z.Compress(Z_NO_FLUSH), [Z_OK], 'TSynZipCompressor.Write');
    if Z.Stream.avail_out = 0 then
      Z.DoFlush;
  end;
  Z.Stream.next_in := nil;
  Z.Stream.avail_in := 0;
  result := Count;
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
  f := TFileStream.Create(filename, fmCreate);
  try
    result := ToStream(f, tempBufSize);
  finally
    f.Free;
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
  ENTRY_SIGNATURE_INC = $02014b50 + 1;         // PK#1#2
  FIRSTHEADER_SIGNATURE_INC = $04034b50 + 1;  // PK#3#4
  LASTHEADER_SIGNATURE_INC = $06054b50 + 1;  // PK#5#6


{ TLocalFileHeader }

function TLocalFileHeader.LocalData: PAnsiChar;
begin
  result := @Self;
  inc(result, sizeof(TLocalFileHeader) + fileInfo.extraLen + fileInfo.nameLen);
end;


{ TFileHeader }

function TFileHeader.IsFolder: boolean;
begin
  result := extFileAttr and $00000010 <> 0;
end;

procedure TFileHeader.Init;
begin
  FillcharFast(self, sizeof(TFileHeader), 0);
  signature := ENTRY_SIGNATURE_INC;
  dec(signature); // constant was +1 to avoid finding it in the exe
  madeBy := $14;
  extFileAttr := $A0; // archive, normal
  fileInfo.neededVersion := $14;
end;


{ TFileInfo }

function TFileInfo.GetAlgoID: integer;
begin
  // in PKware appnote, bits 7..10 of general purpose bit flag are not used
  result := (flags shr 7) and 15; // proprietary flag for SynZipFiles.pas
end;

function TFileInfo.SameAs(aInfo: PFileInfo): boolean;
begin // tolerate a time change through a network: zcrc32 is accurate enough
  if (zzipSize = 0) or
     (aInfo.zzipSize = 0) then
    raise ESynZip.Create('SameAs() with crc+sizes in "data descriptor"');
  result := (zzipMethod = aInfo.zzipMethod) and
            (flags = aInfo.flags) and
            (zzipSize = aInfo.zzipSize) and
            (zfullSize = aInfo.zfullSize) and
            (zcrc32 = aInfo.zcrc32);
end;

procedure TFileInfo.SetAlgoID(Algorithm: integer);
begin
  zzipMethod := Z_STORED; // file is stored, accorging to .ZIP standard
  // in PKware appnote, bits 7..10 of general purpose bit flag are not used
  flags := (flags and $F87F) or
           (Algorithm and 15) shl 7; // proprietary flag for SynZipFiles.pas
end;

function TFileInfo.GetUTF8FileName: boolean;
begin
  // from PKware appnote, Bit 11: Language encoding flag (EFS)
  result := (flags and (1 shl 11)) <> 0;
end;

procedure TFileInfo.SetUTF8FileName;
begin
  flags := flags or (1 shl 11);
end;

procedure TFileInfo.UnSetUTF8FileName;
begin
  flags := flags and not (1 shl 11);
end;

function Is7BitAnsi(P: PChar): boolean;
begin
  result := false;
  if P <> nil then
    while true do
      if ord(P^) = 0 then
        break
      else if ord(P^) <= 126 then
        inc(P)
      else
        exit;
  result := true;
end;


{ TZipWriteAbstract }

constructor TZipWriteAbstract.Create;
begin
  fMagic := FIRSTHEADER_SIGNATURE_INC;
  dec(fMagic); // +1 to avoid finding it in the exe generated code
end;

function TZipWriteAbstract.InternalAdd(const zipName: TFileName; Buf: pointer;
  Size: integer): cardinal;
begin
  with Entry[Count] do
  begin
    fHr.signature := ENTRY_SIGNATURE_INC; // +1 to avoid finding it in the exe
    dec(fHr.signature);
    fHr.madeBy := $14;
    fHr.fileInfo.neededVersion := $14;
    result := InternalWritePosition;
    fHr.localHeadOff := result - fAppendOffset;
    if Is7BitAnsi(pointer(zipName)) then
    begin
      {$ifdef UNICODE}
      intName := AnsiString(zipName);
      {$else}  // intName := zipName -> error reference count under Delphi 6
      SetString(intName, PAnsiChar(pointer(zipName)), length(zipName));
      {$endif UNICODE}
      fHr.fileInfo.UnSetUTF8FileName;
    end
    else
    begin
      intName := StringToUtf8(zipName);
      fHr.fileInfo.SetUTF8FileName;
    end;
    fHr.fileInfo.nameLen := length(intName);
    InternalWrite(fMagic, sizeof(fMagic));
    InternalWrite(fhr.fileInfo, sizeof(fhr.fileInfo));
    InternalWrite(pointer(intName)^, fhr.fileInfo.nameLen);
  end;
  if Buf <> nil then
  begin
    InternalWrite(Buf^, Size); // write stored data
    inc(Count);
  end;
end;

procedure TZipWriteAbstract.AddDeflated(const aZipName: TFileName; Buf: pointer;
  Size, CompressLevel, FileAge: integer);
var
  tmp: pointer;
  tmpsize: integer;
begin
  if self = nil then
    exit;
  if Count >= length(Entry) then
    SetLength(Entry, length(Entry) + 20);
  with Entry[Count] do
  begin
    with fhr.fileInfo do
    begin
      zcrc32 := mormot.lib.z.crc32(0, Buf, Size);
      zfullSize := Size;
      zzipMethod := Z_DEFLATED;
      if FileAge = 0 then
        zlastMod := DateTimeToWindowsFileTime(Now)
      else
        zlastMod := FileAge;
      tmpsize := (Int64(Size) * 11) div 10 + 12;
      Getmem(tmp, tmpsize);
      zzipSize := CompressMem(Buf, tmp, Size, tmpsize, CompressLevel);
      InternalAdd(aZipName, tmp, zzipSize); // write stored data
      Freemem(tmp);
    end;
  end;
end;

procedure TZipWriteAbstract.AddStored(const aZipName: TFileName; Buf: pointer;
  Size, FileAge: integer);
begin
  if self = nil then
    exit;
  if Count >= length(Entry) then
    SetLength(Entry, length(Entry) + 20);
  with Entry[Count], fhr.fileInfo do
  begin
    zcrc32 := mormot.lib.z.crc32(0, Buf, Size);
    zfullSize := Size;
    zzipSize := Size;
    if FileAge = 0 then
      zlastMod := DateTimeToWindowsFileTime(Now)
    else
      zlastMod := FileAge;
    InternalAdd(aZipName, Buf, Size);
  end;
end;

procedure TZipWriteAbstract.Append(const Content: RawByteString);
begin
  if (self = nil) or
     (fAppendOffset <> 0) then
    exit;
  fAppendOffset := length(Content);
  InternalWrite(pointer(Content)^, fAppendOffset);
end;

destructor TZipWriteAbstract.Destroy;
var
  lhr: TLastHeader;
  i: integer;
begin
  FillcharFast(lhr, sizeof(lhr), 0);
  lhr.signature := LASTHEADER_SIGNATURE_INC;
  dec(lhr.signature); // +1 to avoid finding it in the exe
  lhr.thisFiles := Count;
  lhr.totalFiles := Count;
  lhr.headerOffset := InternalWritePosition - fAppendOffset;
  for i := 0 to Count - 1 do
    with Entry[i] do
    begin
      assert(fhr.fileInfo.nameLen = length(intName));
      inc(lhr.headerSize, sizeof(TFileHeader) + fhr.fileInfo.nameLen);
      InternalWrite(fhr, sizeof(fhr));
      InternalWrite(pointer(IntName)^, fhr.fileInfo.nameLen);
    end;
  InternalWrite(lhr, sizeof(lhr));
  inherited Destroy;
end;


{ TZipWrite }

function TZipWrite.InternalWritePosition: cardinal;
begin
  result := FileSeek64(Handle, 0, soFromCurrent);
end;

procedure TZipWrite.InternalWrite(const buf; len: cardinal);
begin
  FileWrite(Handle, buf, len);
end;

procedure TZipWrite.AddFolder(const FolderName: TFileName; const Mask: TFileName;
  Recursive: boolean; CompressLevel: integer);

  procedure RecursiveAdd(const fileDir, zipDir: TFileName);
  var
    f: TSearchRec;
  begin
    if Recursive then
      if FindFirst(fileDir + FILES_ALL, faDirectory, f) = 0 then
      begin
        repeat
          if SearchRecValidFolder(f) then
            RecursiveAdd(fileDir + f.Name + PathDelim, zipDir + f.Name + '\');
        until FindNext(f) <> 0;
        FindClose(f);
      end;
    if FindFirst(fileDir + Mask, faAnyfile - faDirectory, f) = 0 then
    begin
      repeat
        if SearchRecValidFile(f) then
          AddDeflated(fileDir + f.Name, false, CompressLevel, zipDir + f.Name);
      until FindNext(f) <> 0;
      FindClose(f);
    end;
  end;

begin
  RecursiveAdd(IncludeTrailingPathDelimiter(FolderName), '');
end;

procedure TZipWrite.AddDeflated(const aFileName: TFileName; RemovePath: boolean;
  CompressLevel: integer; ZipName: TFileName);
var
  Size: Int64;
  Size64: Int64Rec absolute Size;
  OffsHead, OffsEnd: cardinal;
  S: TFileStream;
  D: THandleStream;
  Z: TSynZipCompressor;
begin
  S := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    if ZipName = '' then
      if RemovePath then
        ZipName := ExtractFileName(aFileName)
      else
        {$ifdef MSWINDOWS}
        ZipName := aFileName;
        {$else}
        ZipName := StringReplace(aFileName, '/', '\', [rfReplaceAll]);
        {$endif MSWINDOWS}
    Size := S.Size;
    if Size64.Hi <> 0 then
      raise ESynZip.CreateFmt('%s file too big for .zip', [aFileName]);
    if Count >= length(Entry) then
      SetLength(Entry, length(Entry) + 20);
    OffsHead := InternalAdd(ZipName, nil, 0);
    D := THandleStream.Create(Handle);
    Z := TSynZipCompressor.Create(D, CompressLevel);
    try
      Z.CopyFrom(S, Size64.Lo);
      Z.Flush;
      assert(Z.SizeIn = Size64.Lo);
      with Entry[Count] do
      begin
        with fhr.fileInfo do
        begin
          zcrc32 := Z.CRC;
          zfullSize := Z.SizeIn;
          zzipSize := Z.SizeOut;
          zzipMethod := Z_DEFLATED;
          zlastMod := FileAgeToWindowsTime(aFileName);
        end;
        OffsEnd := D.Position;
        D.Position := OffsHead + sizeof(fMagic);
        D.WriteBuffer(fhr.fileInfo, sizeof(fhr.fileInfo));
        D.Position := OffsEnd;
      end;
      inc(Count);
    finally
      Z.Free;
      D.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TZipWrite.AddFromZip(const ZipEntry: TZipEntry);
begin
  if (self = nil) or
     (Handle <= 0) then
    exit;
  if Count >= length(Entry) then
    SetLength(Entry, length(Entry) + 20);
  with Entry[Count] do
  begin
    fhr.fileInfo := ZipEntry.infoLocal^;
    InternalAdd(ZipEntry.zipName, ZipEntry.data, fhr.fileInfo.zzipSize);
  end;
end;

constructor TZipWrite.Create(const aFileName: TFileName);
begin
  Create;
  fFileName := aFileName;
  if Handle = 0 then
    Handle := FileCreate(aFileName);
end;

constructor TZipWrite.CreateFrom(const aFileName: TFileName; dummy: integer);
var
  R: TZipRead;
  i: PtrInt;
begin
  Handle := FileOpen(aFileName, fmOpenReadWrite or fmShareDenyNone);
  if Handle < 0 then
  begin
    R := nil;
    Handle := 0;
  end
  else
    R := TZipRead.Create(Handle);
  Create(aFileName);
  if R <> nil then
  try
    Count := R.Count;
    SetLength(Entry, Count + 10);
    for i := 0 to Count - 1 do
      with Entry[i], R.Entry[i] do
      begin
        fhr.Init;
        fhr.localHeadOff := PtrUInt(infoLocal) - PtrUInt(R.Entry[0].infoLocal);
        R.RetrieveFileInfo(i, fhr.fileInfo);
        SetString(intName, storedName, infoLocal^.nameLen);
      end;
    FileSeek64(Handle, R.fReadOffset, soFromBeginning);
  finally
    R.Free;
  end;
end;

destructor TZipWrite.Destroy;
begin
  inherited Destroy; // will write TLastHeader content
  SetEndOfFile(Handle);
  FileClose(Handle);
end;


{ TZipWriteToStream }

constructor TZipWriteToStream.Create(aDest: TStream);
begin
  fDest := aDest;
  inherited Create;
end;

function TZipWriteToStream.InternalWritePosition: cardinal;
begin
  result := fDest.Seek(0, soCurrent);
end;

procedure TZipWriteToStream.InternalWrite(const buf; len: cardinal);
begin
  fDest.WriteBuffer(buf, len);
end;


{ TZipRead }

constructor TZipRead.Create(BufZip: PByteArray; Size: cardinal);
var
  lhr: PLastHeader;
  H: PFileHeader;
  lfhr: PLocalFileHeader;
  i, j: integer;
  tmp: RawByteString;
begin
  for i := 0 to 127 do
  begin
    // resources size may be rounded up -> search in trainling 128 bytes
    lhr := @BufZip[Size - sizeof(TLastHeader)];
    if lhr^.signature + 1 = LASTHEADER_SIGNATURE_INC then
      break;
    dec(Size);
    if Size <= sizeof(lhr^) then
      break;
  end;
  if lhr^.signature + 1 <> LASTHEADER_SIGNATURE_INC then
  begin
    fMap.UnMap;
    raise ESynZip.Create('ZIP format');
  end;
  SetLength(Entry, lhr^.totalFiles); // fill Entry[] with the Zip headers
  fReadOffset := lhr^.headerOffset;
  fFirstFileHeader := @BufZip[lhr^.headerOffset];
  H := fFirstFileHeader;
  for i := 1 to lhr^.totalFiles do
  begin
    if H^.signature + 1 <> ENTRY_SIGNATURE_INC then
    begin // +1 to avoid match in exe
      fMap.UnMap;
      raise ESynZip.Create('ZIP format');
    end;
    lfhr := @BufZip[H^.localHeadOff];
    with lfhr^.fileInfo do
      if flags and (1 shl 3) <> 0 then
      begin // crc+sizes in "data descriptor"
        if (zcrc32 <> 0) or
           (zzipSize <> 0) or
           (zfullSize <> 0) then
          raise ESynZip.Create('ZIP extended format');
        // UnZip() will call RetrieveFileInfo()
      end
      else if (zzipSize = cardinal(-1)) or
              (zfullSize = cardinal(-1)) then
        raise ESynZip.Create('ZIP64 format not supported');
    with Entry[Count] do
    begin
      infoLocal := @lfhr^.fileInfo;
      infoDirectory := H;
      storedName := PAnsiChar(lfhr) + sizeof(lfhr^);
      data := storedName + infoLocal^.NameLen + infoLocal^.extraLen; // data mapped in memory
      SetString(tmp, storedName, infoLocal^.nameLen);
      for j := 0 to infoLocal^.nameLen - 1 do
        if storedName[j] = '/' then // normalize path delimiter
          PAnsiChar(Pointer(tmp))[j] := '\';
      if infoLocal^.GetUTF8FileName then
        // decode UTF-8 file name into native string/TFileName type
        zipName := Utf8ToString(tmp)
      else
        // legacy Windows-OEM encoding - from mormot.core.os
        zipName := OemToFileName(tmp);
      inc(PByte(H), sizeof(H^) + infoLocal^.NameLen + H^.fileInfo.extraLen + H^.commentLen);
      if not (infoLocal^.zZipMethod in [Z_STORED, Z_DEFLATED]) then
        raise ESynZip.CreateFmt('Unsupported compression method %d for %s',
          [infoLocal^.zZipMethod, zipName]);
      if (zipName = '') or
         (zipName[length(zipName)] = '\') then
        continue; // ignore folder
      inc(Count); // add file to Entry[]
    end;
  end;
end;

constructor TZipRead.Create(Instance: THandle; const ResName: string; ResType: PChar);
// resources are memory maps of the executable -> direct access
begin
  if fResource.Open(ResName, ResType, Instance) then
    // warning: resources size may be aligned rounded up -> handled in Create()
    Create(fResource.Buffer, fResource.Size);
end;

constructor TZipRead.Create(aFile: THandle; ZipStartOffset, Size: cardinal;
  aFileOwned: boolean);
var
  i, ExeOffset: PtrInt;
begin
  if aFile <= 0 then
    exit;
  if not fMap.Map(aFile, Size, {offset=}0, aFileOwned) then
    raise ESynZip.Create('FileMap failed');
  ExeOffset := -1;
  for i := ZipStartOffset to fMap.Size - 5 do // search for first local header
    if PCardinal(@fMap.Buffer[i])^ + 1 = FIRSTHEADER_SIGNATURE_INC then
    begin
      // +1 above to avoid finding it in the exe part
      ExeOffset := i;
      break;
    end;
  if ExeOffset < 0 then // try if adding files to an empty archive
    for i := ZipStartOffset to fMap.Size - 5 do
      if PCardinal(@fMap.Buffer[i])^ + 1 = LASTHEADER_SIGNATURE_INC then
      begin
        // +1 avoids false positive
        ExeOffset := i;
        break;
      end;
  if ExeOffset < 0 then
  begin
    fMap.Unmap;
    raise ESynZip.Create('No ZIP header found');
  end;
  Create(@fMap.Buffer[ExeOffset], fMap.Size - PtrUInt(ExeOffset));
end;

constructor TZipRead.Create(const aFileName: TFileName; ZipStartOffset, Size: cardinal);
begin
  fFileName := aFileName;
  Create(FileOpen(aFileName, fmOpenRead or fmShareDenyNone),
    ZipStartOffset, Size, {owned=}true);
end;

destructor TZipRead.Destroy;
begin
  fMap.UnMap;
  fResource.Close;
  inherited Destroy;
end;

function TZipRead.NameToIndex(const aName: TFileName): integer;
begin
  if (self <> nil) and
     (aName <> '') then
    for result := 0 to Count - 1 do
      if SameText(Entry[result].zipName, aName) then
        exit;
  result := -1;
end;

type
  TDataDescriptor = packed record
    signature: cardinal;
    crc32: cardinal;
    zipSize: cardinal;
    fullSize: cardinal;
  end;

function TZipRead.RetrieveFileInfo(Index: integer; var Info: TFileInfo): boolean;
var
  P: ^TDataDescriptor;
  PDataStart: PtrUInt;
begin
  if (self = nil) or
     (cardinal(Index) >= cardinal(Count)) then
  begin
    result := false;
    exit;
  end;
  // copy information from "local file header"
  Info := Entry[Index].infoLocal^;
  if Info.flags and (1 shl 3) = 0 then
  begin
    result := true; // local information is correct
    exit;
  end;
  // get info from ending "central directory" (faster than "data descriptor")
  with Entry[Index].infoDirectory^.fileInfo do
    if (zzipSize <> cardinal(-1)) and
       (zfullSize <> cardinal(-1)) then
    begin
      // ZIP64 format not supported yet (sizes=-1)
      Info.zcrc32 := zcrc32;
      Info.zzipSize := zzipSize;
      Info.zfullSize := zfullSize;
      result := true;
      exit;
    end;
  // search manually the "data descriptor" from the binary local data
  if Index < Count - 2 then
    P := Pointer(Entry[Index + 1].infoLocal)
  else
    P := Pointer(fFirstFileHeader);
  dec(P);
  PDataStart := PtrUInt(Entry[Index].data);
  repeat
    // same pattern as ReadLocalItemDescriptor() in 7-Zip's ZipIn.cpp
    // but here, search is done backwards (much faster than 7-Zip algorithm)
    if P^.signature <> $08074b50 then
      if PtrUInt(P) > PDataStart then
        dec(PByte(P))
      else
        break
    else if P^.zipSize = PtrUInt(P) - PDataStart then
    begin
      if (P^.zipSize = 0) or
         (P^.fullSize = 0) or
         (P^.zipSize = cardinal(-1)) or
         (P^.fullSize = cardinal(-1)) then
        break; // we expect sizes to be available
      Info.zcrc32 := P^.crc32;
      Info.zzipSize := P^.zipSize;
      Info.zfullSize := P^.fullSize;
      result := true;
      exit;
    end
    else if PtrUInt(P) > PDataStart then
      dec(PByte(P))
    else
      break;
  until false;
  result := false; // data descriptor block not found
end;

function TZipRead.UnZip(aIndex: integer): RawByteString;
var
  len: cardinal;
  info: TFileInfo;
begin
  result := ''; // somewhat faster if memory is reallocated each time
  if not RetrieveFileInfo(aIndex, info) then
    exit;
  SetString(result, nil, info.zfullSize);
  case info.zZipMethod of
    Z_STORED:
      begin
        len := info.zfullsize;
        MoveFast(Entry[aIndex].data^, pointer(result)^, len);
      end;
    Z_DEFLATED:
      len := UnCompressMem(Entry[aIndex].data, pointer(result), info.zzipsize,
        info.zfullsize);
  else
    raise ESynZip.CreateFmt('Unsupported method %d for %s',
      [info.zZipMethod, Entry[aIndex].zipName]);
  end;
  if (len <> info.zfullsize) or
     (info.zcrc32 <> mormot.lib.z.crc32(0, pointer(result), info.zfullSize)) then
    raise ESynZip.CreateFmt('Error decompressing %s', [Entry[aIndex].zipName]);
end;

function TZipRead.UnZipStream(aIndex: integer; const aInfo: TFileInfo;
  aDest: TStream): boolean;
var
  crc: cardinal;
begin
  result := false;
  case aInfo.zZipMethod of
    Z_STORED:
      begin
        aDest.WriteBuffer(Entry[aIndex].data^, aInfo.zfullsize);
        crc := mormot.lib.z.crc32(0, Entry[aIndex].data, aInfo.zfullSize);
      end;
    Z_DEFLATED:
      if UnCompressStream(Entry[aIndex].data, aInfo.zzipsize,
           aDest, @crc) <> aInfo.zfullsize then
        exit;
  else
    raise ESynZip.CreateFmt('Unsupported method %d for %s',
      [aInfo.zZipMethod, Entry[aIndex].zipName]);
  end;
  result := crc = aInfo.zcrc32;
end;

function TZipRead.UnZip(aIndex: integer; aDest: TStream): boolean;
var
  info: TFileInfo;
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
  Path: TFileName;
  info: TFileInfo;
begin
  result := false;
  if not RetrieveFileInfo(aIndex, info) then
    exit;
  with Entry[aIndex] do
    if DestDirIsFileName then
      Path := DestDir
    else
    begin
      Path := EnsureDirectoryExists(DestDir + ExtractFilePath(zipName));
      if Path = '' then
        exit;
      Path := Path + ExtractFileName(zipName);
    end;
  FS := TFileStream.Create(Path, fmCreate);
  try
    result := UnZipStream(aIndex, info, FS);
  finally
    FS.Free;
  end;
  FileSetDateFromWindowsTime(Path, info.zlastMod);
end;

function TZipRead.UnZipAll(DestDir: TFileName): integer;
begin
  DestDir := EnsureDirectoryExists(DestDir);
  for result := 0 to Count - 1 do
    if not UnZip(result, DestDir) then
      exit;
  result := -1;
end;

function TZipRead.UnZip(const aName, DestDir: TFileName; DestDirIsFileName: boolean): boolean;
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
    FreeAndNil(EventArchiveZipWrite)
  else
  begin
    if not FileExists(aOldLogFileName) then
      exit;
    if EventArchiveZipWrite = nil then
      EventArchiveZipWrite := TZipWrite.CreateFrom(
        system.copy(aDestinationPath, 1, length(aDestinationPath) - 1) + '.zip');
    n := EventArchiveZipWrite.Count;
    EventArchiveZipWrite.AddDeflated(aOldLogFileName, True);
    if (EventArchiveZipWrite.Count = n + 1) and
       DeleteFile(aOldLogFileName) then
      result := True;
  end;
end;


const
  HTTP_LEVEL = 1; // 6 is standard, but 1 is enough and faster

function CompressGZip(var Data: RawByteString; Compress: boolean): RawUtf8;
var
  L: integer;
  P: PAnsiChar;
begin
  L := length(Data);
  if Compress then
  begin
    SetString(result, nil, L + 128 + L shr 3); // maximum possible memory required
    P := pointer(result);
    MoveFast(GZHEAD, P^, GZHEAD_SIZE);
    inc(P, GZHEAD_SIZE);
    inc(P, CompressMem(pointer(Data), P, L ,
      length(result) - (GZHEAD_SIZE + 8), HTTP_LEVEL));
    PCardinal(P)^ := crc32(0, pointer(Data), L);
    inc(P,4);
    PCardinal(P)^ := L;
    inc(P,4);
    SetString(Data, PAnsiChar(pointer(result)), P - pointer(result));
  end
  else
    Data := gzread(pointer(Data), length(Data));
  result := 'gzip';
end;

procedure CompressInternal(var Data: RawByteString; Compress, ZLib: boolean);
var
  tmp: RawByteString;
  DataLen: integer;
begin
  tmp := Data;
  DataLen := length(Data);
  if Compress then
  begin
    SetString(Data, nil, DataLen + 256 + DataLen shr 3); // max mem required
    DataLen := CompressMem(pointer(tmp), pointer(Data), DataLen, length(Data),
      HTTP_LEVEL, ZLib);
    if DataLen <= 0 then
      Data := ''
    else
      SetLength(Data, DataLen);
  end
  else
    Data := UnCompressZipString(pointer(tmp), DataLen, nil, ZLib, 0);
end;

function CompressDeflate(var Data: RawByteString; Compress: boolean): RawUtf8;
begin
  CompressInternal(Data, Compress, {zlib=}false);
  result := 'deflate';
end;

function CompressZLib(var Data: RawByteString; Compress: boolean): RawUtf8;
begin
  CompressInternal(Data, Compress, {zlib=}true);
  result := 'zlib';
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
    constructor Create; override;
    function AlgoID: byte; override;
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
  end;

constructor TAlgoDeflate.Create;
begin
  inherited Create;
  fDeflateLevel := 6;
end;

function TAlgoDeflate.AlgoID: byte;
begin
  result := 2;
end;

function TAlgoDeflate.RawProcess(src, dst: pointer; srcLen, dstLen,
  dstMax: integer; process: TAlgoCompressWithNoDestLenProcess): integer;
begin
  case process of
    doCompress:
      result := mormot.lib.z.CompressMem(
        src, dst, srcLen, dstLen, fDeflateLevel);
    doUnCompress, doUncompressPartial:
      result := mormot.lib.z.UnCompressMem(
        src, dst, srcLen, dstLen);
  else
    result := 0;
  end;
end;

function TAlgoDeflate.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  result := PlainLen + 256 + PlainLen shr 3;
end;


{ TAlgoDeflateFast }

type
  // implements the AlgoDeflateFast global variable
  TAlgoDeflateFast = class(TAlgoDeflate)
  public
    constructor Create; override;
    function AlgoID: byte; override;
  end;

function TAlgoDeflateFast.AlgoID: byte;
begin
  result := 3;
end;

constructor TAlgoDeflateFast.Create;
begin
  inherited Create;
  fDeflateLevel := 1;
end;


initialization
  AlgoDeflate := TAlgoDeflate.Create;
  AlgoDeflateFast := TAlgoDeflateFast.Create;
  
end.

