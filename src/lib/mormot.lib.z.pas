/// low-level access to the zlib API
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.z;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler zlib API
   - Low-Level ZLib Streaming Access
   - Simple Wrapper Functions for Deflate/ZLib Process

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$if not defined(ZLIBSTATIC) and not defined (ZLIBPAS) and
     not defined(ZLIBRTL) and not defined(ZLIBEXT)}

  // select best known choice if not overriden for the whole project
  {$ifdef FPC}
    {$ifdef MSWINDOWS}
      {$define ZLIBSTATIC}   // FPC Win32 Win64: static .o
    {$else}
      {$ifdef ANDROID}
        {$define ZLIBPAS}    // FPC Android: paszlib (Alf reported problems)
      {$else}
        {$define ZLIBEXT}    // FPC other POSIX: system's libz.so
      {$endif ANDROID}
    {$endif MSWINDOWS}
  {$else not FPC}
    {$ifdef WIN32}
      {$define ZLIBSTATIC}   // Delphi Win32: static .obj
    {$endif WIN32}
    {$ifdef WIN64}
      {$define ZLIBRTL}      // Delphi Win64: system.zlib.pas from Delphi RTL
    {$endif WIN64}
  {$endif FPC}

{$ifend}


uses
  sysutils,
  classes,
  {$ifdef ZLIBPAS}
  paszlib,
  {$endif ZLIBPAS}
  {$ifdef ZLIBRTL}
  zlib,
  {$endif ZLIBRTL}
  mormot.core.base;


{ ****************** Low-Level ZLib Streaming Access }

type
  // depending on the linked zlib version, sizes may be 32-bit or PtrInt...
  // so we publish the official z_stream type from paszlib/zlib

  {$ifdef ZLIBPAS}
  TZStream = paszlib.TZStream;
  TZCRC = cardinal;
  {$else}

  {$ifdef ZLIBRTL}
  TZStream = zlib.z_Stream;
  TZCRC = PtrUInt;
  {$else}

  {$ifdef ZLIBEXT}
  TZLong = PtrUInt;
  TZCRC = PtrUInt;
  {$endif ZLIBEXT}

  {$ifdef ZLIBSTATIC}
  // our statically linked library expects 32-bit long/crc even on FPC Win64
  TZLong = cardinal;
  TZCRC = cardinal;
  {$endif ZLIBSTATIC}

  /// raw structure used by external/static zlib during its stream process
  TZStream =  record
    next_in: PAnsiChar;
    avail_in: cardinal;
    total_in: TZLong;
    next_out: PAnsiChar;
    avail_out: cardinal;
    total_out: TZLong;
    msg: PAnsiChar;
    state: pointer;
    zalloc: pointer;
    zfree: pointer;
    opaque: pointer;
    data_type: integer;
    adler: TZLong;
    reserved: TZLong;
  end;

  {$endif ZLIBRTL}
  {$endif ZLIBPAS}


type
  /// class of Exceptions raised by ZCheck()
  ESynZip = class(Exception);

  /// main access to the zlib API for compression/uncompression
  // - we encapsulated all low-level C calls into this object-oriented structure
  // - see CompressStream/UncompressStream for actual use of its methods
  TZLib = object
  private
    FlushStream: TStream;
    FlushBuffer: pointer;
    FlushSize: integer;
    FlushCheckCRC: PCardinal;
    FlushBufferOwned: boolean;
  public
    /// raw zlib Stream information
    Stream: TZStream;
    /// reset the internal Stream structure
    procedure Clear;
    /// reset and prepare the internal Stream structure for two memory buffers
    procedure Init(src, dst: pointer; srcLen, dstLen: integer); overload;
    /// reset and prepare the internal Stream structure for a destination stream
    // - using a temporary buffer
    procedure Init(src: pointer; srcLen: integer; dst: TStream; checkcrc: PCardinal;
      temp: pointer; tempsize, expectedsize: integer); overload;
    /// prepare the Deflate Compression
    // - by default, will use the deflate/.zip header-less format, but you may set
    // ZlibFormat=true to add an header, as expected by zlib (and pdf)
    function CompressInit(CompressionLevel: integer; ZlibFormat: Boolean): Boolean;
    /// apply Deflate Compression over one memory block as defined in STream
    function Compress(Flush: integer): integer;
    /// finalize the Deflate Compression
    function CompressEnd: integer;
    /// prepare the Inflate Uncompression
    // - ZLibFormat defines the expected layout, and should match CompressInit()
    function UncompressInit(ZlibFormat: Boolean): Boolean;
    /// apply Inflate Uncompression over one memory block as defined in STream
    function Uncompress(Flush: integer): integer;
    /// finalize the Inflate Uncompression
    function UncompressEnd: integer;
    /// low-level flush of the compressed data pending in the internal buffer
    procedure DoFlush;
    /// low-level check of the code returned by the ZLib library
    // - raise ESynZipException on error
    function Check(const Code: Integer; const ValidCodes: array of Integer;
      const Context: string = ''): integer;
  end;

/// compute the crc32 checksum of the supplied memory buffer
function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC;
  {$ifdef ZLIBEXT} cdecl; {$else} {$ifdef ZLIBSTATIC} cdecl; {$else}
    {$ifdef FPC} inline; {$endif} {$endif} {$endif}

const
  ZLIB_VERSION = '1.2.3';

  Z_NO_FLUSH = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;
  Z_BLOCK = 5;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -1;
  Z_STREAM_ERROR = -2;
  Z_DATA_ERROR = -3;
  Z_MEM_ERROR = -4;
  Z_BUF_ERROR = -5;
  Z_VERSION_ERROR = -6;

  Z_NO_CompressION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_CompressION = 9;
  Z_DEFAULT_CompressION = -1;

  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_RLE = 3;
  Z_FIXED = 4;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;

  Z_STORED = 0;
  Z_DEFLATED = 8;
  MAX_WBITS = 15; // 32K LZ77 window
  DEF_MEM_LEVEL = 8;

  Z_NULL = 0;


{ ******************* Simple Wrapper Functions for Deflate/ZLib Process }

/// in-memory ZLib compression
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressMem(src, dst: pointer; srcLen, dstLen: integer;
  CompressionLevel: integer = 6; ZlibFormat: Boolean = false) : integer;

/// in-memory ZLib uncompression
// - ZLibFormat defines the expected layout, and should match CompressMem()
function UncompressMem(src, dst: pointer; srcLen, dstLen: integer;
  ZlibFormat: Boolean = false) : integer;

/// ZLib compression from memory into a stream
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressStream(src: pointer; srcLen: integer;
  tmp: TStream; CompressionLevel: integer = 6; ZlibFormat: Boolean = false;
  TempBufSize: integer = 0): cardinal;

/// ZLib decompression from memory into a stream
// - return the number of bytes written into the stream
// - if checkCRC if not nil, it will contain the crc32; if aStream is nil, it
// will only calculate the crc of the the Uncompressed memory block
// - ZLibFormat defines the expected layout, and should match CompressStream()
function UncompressStream(src: pointer; srcLen: integer; tmp: TStream;
  checkCRC: PCardinal = nil; ZlibFormat: Boolean = false;
  TempBufSize: integer = 0): cardinal;

/// ZLib compression from memory into a RawByteString variable
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressZipString(src: pointer; srcLen: integer;
  CompressionLevel: integer = 6; ZlibFormat: Boolean = false;
  TempBufSize: integer = 0): RawByteString; overload;

/// ZLib compression to and from RawByteString variables
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressZipString(const src: RawByteString;
  CompressionLevel: integer = 6; ZlibFormat: Boolean = false;
  TempBufSize: integer = 0): RawByteString; overload;

/// ZLib decompression from memory into a RawByteString variable
// - if checkCRC if not nil, it will contain the crc32; if aStream is nil, it
// will only calculate the crc of the the Uncompressed memory block
// - ZLibFormat defines the expected layout, and should match CompressZipString()
function UncompressZipString(src: pointer; srcLen: integer;
  checkCRC: PCardinal = nil; ZlibFormat: Boolean = false;
  TempBufSize: integer = 0): RawByteString; overload;

/// ZLib decompression to and from RawByteString variables
// - ZLibFormat defines the expected layout, and should match CompressZipString()
function UncompressZipString(const src: RawByteString;
  ZlibFormat: Boolean = false; TempBufSize: integer = 0): RawByteString; overload;

/// just hash aString with CRC32 algorithm
// - crc32 is better than adler32 for short strings
function CRC32string(const aString: RawByteString): cardinal;


implementation


{ ****************** Low-Level ZLib Streaming Access }

{$ifdef ZLIBSTATIC}

{$ifdef FPC} // we supply our own static zlib files in coff format for Windows

  {$ifdef WIN32}
    {$L ..\..\static\i386-win32\deflate.o}
    {$L ..\..\static\i386-win32\trees.o}
    {$L ..\..\static\i386-win32\zutil.o}
    {$L ..\..\static\i386-win32\inffast.o}
    {$L ..\..\static\i386-win32\inflate.o}
    {$L ..\..\static\i386-win32\inftrees.o}
    {$L ..\..\static\i386-win32\adler32.o}
    {$L ..\..\static\i386-win32\crc32.o}
    {$linklib ..\..\static\i386-win32\libmsvcrt.a}
  {$endif}

  {$ifdef WIN64}
    {$L ..\..\static\x86_64-win64\inffast.o}  
    {$L ..\..\static\x86_64-win64\inftrees.o}
    {$L ..\..\static\x86_64-win64\inflate.o}
    {$L ..\..\static\x86_64-win64\deflate.o}
    {$L ..\..\static\x86_64-win64\trees.o}
    {$L ..\..\static\x86_64-win64\zutil.o}
    {$L ..\..\static\x86_64-win64\adler32.o}
    {$L ..\..\static\x86_64-win64\crc32.o}
    {$linklib ..\..\static\x86_64-win64\libmsvcrt.a}
  {$endif}

{$else} // for Delphi Win32 - Delphi 7 has no reliable zlib.pas

  {$L ..\..\static\delphi\zlibdeflate.obj}
  {$L ..\..\static\delphi\zlibtrees.obj}
  {$L ..\..\static\delphi\zlibinflate.obj}
  {$L ..\..\static\delphi\zlibinftrees.obj}
  {$L ..\..\static\delphi\zlibadler32.obj}
  {$L ..\..\static\delphi\zlibcrc32.obj}
  {$L ..\..\static\delphi\zlibinffast.obj}

// inlined zutil.obj for Delphi Win32

function zcalloc(AppData: Pointer; Items, Size: cardinal): Pointer; cdecl;
begin // direct use of the (FastMM4) delphi heap for all zip memory allocation
  Getmem(result, Items * Size);
end;

procedure zcfree(AppData, Block: Pointer); cdecl;
begin // direct use of the (FastMM4) delphi heap for all zip memory allocation
  FreeMem(Block);
end;

const
  z_errmsg: array[0..9] of PAnsiChar = (
    'need dictionary',       // Z_NEED_DICT       2
    'stream end',            // Z_STREAM_END      1
    '',                      // Z_OK              0
    'file error',            // Z_ERRNO         (-1)
    'stream error',          // Z_STREAM_ERROR  (-2)
    'data error',            // Z_DATA_ERROR    (-3)
    'insufficient memory',   // Z_MEM_ERROR     (-4)
    'buffer error',          // Z_BUF_ERROR     (-5)
    'incompatible version',  // Z_VERSION_ERROR (-6)
    '');

function memset(P: Pointer; B: Integer; count: Integer): pointer; cdecl;
begin
  FillCharFast(P^, count, B);
  result := P;
end;

function memcpy(dest, source: Pointer; count: Integer): pointer; cdecl;
begin
  MoveFast(source^, dest^, count);
  result := dest;
end;

procedure _llmod; // this C++ Builder intrisinc is not cdecl
asm
  jmp System.@_llmod
end;

{$endif FPC}

function deflate(var strm: TZStream; flush: integer): integer; cdecl; external;
function deflateEnd(var strm: TZStream): integer; cdecl; external;
function inflate(var strm: TZStream; flush: integer): integer; cdecl; external;
function inflateEnd(var strm: TZStream): integer; cdecl; external;
function adler32(adler: TZCRC; buf: PAnsiChar; len: cardinal): TZCRC; cdecl; external;
function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC; cdecl; external;
function deflateInit_(var strm: TZStream; level: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external;
function inflateInit_(var strm: TZStream;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external;
function deflateInit2_(var strm: TZStream;
  level, method, windowBits, memLevel, strategy: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external;
function inflateInit2_(var strm: TZStream; windowBits: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external;
function get_crc_table: pointer; cdecl; external;

{$endif ZLIBSTATIC}


{$ifdef ZLIBEXT}

const
{$ifdef ANDROID}
  libz = '/usr/lib/libz.so';
{$else}
  {$ifdef FPC}
  libz = 'z';
  {$linklib libz}
  {$else}
  libz = 'libz.so'
  {$endif FPC}
{$endif ANDROID}

function deflate(var strm: TZStream; flush: integer): integer; cdecl; external libz;
function deflateEnd(var strm: TZStream): integer; cdecl; external libz;
function inflate(var strm: TZStream; flush: integer): integer; cdecl; external libz;
function inflateEnd(var strm: TZStream): integer; cdecl; external libz;
function adler32(adler: TZCRC; buf: PAnsiChar; len: cardinal): TZCRC; cdecl; external libz;
function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC; cdecl; external libz;
function deflateInit_(var strm: TZStream; level: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external libz;
function inflateInit_(var strm: TZStream;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external libz;
function deflateInit2_(var strm: TZStream;
  level, method, windowBits, memLevel, strategy: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external libz;
function inflateInit2_(var strm: TZStream; windowBits: integer;
  version: PAnsiChar; stream_size: integer): integer; cdecl; external libz;
function get_crc_table: pointer; cdecl; external libz;

{$endif ZLIBEXT}


{$ifdef ZLIBPAS}

function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := paszlib.crc32(crc, buf, len);
end;

{$endif ZLIBPAS}

{$ifdef ZLIBRTL}

function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := zlib.crc32(crc, buf, len);
end;

{$endif ZLIBRTL}


{ TZLib }

procedure TZLib.Clear;
begin
  FillCharFast(Stream, SizeOf(Stream), 0);
end;

{$ifndef ZLIBPAS}

function zlibAllocMem(AppData: Pointer; Items, Size: cardinal): Pointer; cdecl;
begin
  Getmem(result, Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer);  cdecl;
begin
  FreeMem(Block);
end;

{$endif ZLIBPAS}

procedure TZLib.Init(src, dst: pointer; srcLen, dstLen: integer);
begin
  Clear;
  Stream.next_in := src;
  Stream.avail_in := srcLen;
  Stream.next_out := dst;
  Stream.avail_out := dstLen;
  {$ifndef ZLIBPAS}
  Stream.zalloc := @zlibAllocMem; // even under Linux, use program heap
  Stream.zfree := @zlibFreeMem;
  {$endif ZLIBPAS}
end;

procedure TZLib.Init(src: pointer; srcLen: integer; dst: TStream;
  checkcrc: PCardinal; temp: pointer; tempsize, expectedsize: integer);
begin
  Init(src, temp, srcLen, tempsize);
  if tempsize < expectedsize then
  begin
    GetMem(FlushBuffer, expectedsize);
    FlushBufferOwned := true;
    FlushSize := expectedsize;
    Stream.next_out := FlushBuffer;
    Stream.avail_out := FlushSize;
  end
  else
  begin
    FlushBuffer := temp;
    FlushSize := tempsize;
  end;
end;

function TZLib.CompressInit(CompressionLevel: integer; ZlibFormat: Boolean): Boolean;
var
  bits: integer;
begin
  if ZlibFormat then
    bits := MAX_WBITS
  else
    bits := - MAX_WBITS;
  result := deflateInit2_(Stream, CompressionLevel, Z_DEFLATED, bits,
    DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, sizeof(Stream)) >= 0;
  if FlushBufferOwned and not result then
    FreeMem(FlushBuffer);
end;

function TZLib.Compress(Flush: integer): integer;
begin
  result := deflate(Stream, Flush);
end;

function TZLib.CompressEnd: integer;
begin
  result := deflateEnd(Stream);
  if FlushBufferOwned then
    FreeMem(FlushBuffer);
end;

function TZLib.UncompressInit(ZlibFormat: Boolean): Boolean;
var
  bits: integer;
begin
  if ZlibFormat then
    bits := MAX_WBITS
  else
    bits := - MAX_WBITS;
  result := inflateInit2_(Stream, bits, ZLIB_VERSION, SizeOf(Stream)) >= 0;
  if FlushBufferOwned and not result then
    FreeMem(FlushBuffer);
end;

function TZLib.Uncompress(Flush: integer): integer;
begin
  result := inflate(Stream, Flush);
end;

function TZLib.UncompressEnd: integer;
begin
  result := inflateEnd(Stream);
  if FlushBufferOwned then
    FreeMem(FlushBuffer);
end;

procedure TZLib.DoFlush;
var
  n: integer;
begin
  n := FlushSize - integer(Stream.avail_out);
  if n <> 0 then
  begin
    if FlushCheckCRC <> nil then
      FlushCheckCRC^ := crc32(FlushCheckCRC^, FlushBuffer, n);
    FlushStream.WriteBuffer(FlushBuffer^, n);
  end;
  Stream.next_out := FlushBuffer;
  Stream.avail_out := FlushSize;
end;

function TZLib.Check(const Code: Integer; const ValidCodes: array of Integer;
  const Context: string): integer;
var
  i: PtrInt;
begin
  if Code = Z_MEM_ERROR then
    OutOfMemoryError;
  result := code;
  for i := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[i] = Code then
      exit;
  raise ESynZip.CreateFmt('Error %d during %s process [%s]',
    [Code, Context, {$ifndef ZLIBPAS} PAnsiChar {$endif} (Stream.msg)]);
end;


{ ******************* Simple Wrapper Functions for Deflate/ZLib Process }

function CompressMem(src, dst: pointer; srcLen, dstLen: integer;
  CompressionLevel: integer; ZlibFormat: Boolean): integer;
var
  z: TZLib;
begin
  z.Init(src, dst, srcLen, dstLen);
  if z.CompressInit(CompressionLevel, ZlibFormat) then
    try
      z.Check(z.Compress(Z_FINISH), [Z_STREAM_END, Z_OK], 'CompressMem');
    finally
      z.CompressEnd;
    end;
  result := z.Stream.total_out;
end;

function UncompressMem(src, dst: pointer; srcLen, dstLen: integer;
  ZlibFormat: Boolean): integer;
var
  z: TZLib;
begin
  z.Init(src, dst, srcLen, dstLen);
  if z.UncompressInit(ZlibFormat) then
    try
      z.Check(z.Uncompress(Z_FINISH), [Z_STREAM_END, Z_OK], 'UncompressMem');
    finally
      z.UncompressEnd;
    end;
  result := z.Stream.total_out;
end;

function CompressStream(src: pointer; srcLen: integer; tmp: TStream;
  CompressionLevel: integer; ZlibFormat: Boolean; TempBufSize: integer): cardinal;
var
  z: TZLib;
  code: integer;
  temp: array[word] of word; // 128KB is good enough (fine for IIS e.g.)
begin
  z.Init(src, srcLen, tmp, nil, @temp, SizeOf(temp), TempBufSize);
  if z.CompressInit(CompressionLevel, ZlibFormat) then
    try
      repeat
        code := z.Check(z.Compress(Z_FINISH),
          [Z_OK, Z_STREAM_END, Z_BUF_ERROR], 'CompressStream');
        z.DoFlush;
      until code = Z_STREAM_END;
    finally
      z.CompressEnd;
    end;
  result := z.Stream.total_out;
end;

function UncompressStream(src: pointer; srcLen: integer; tmp: TStream;
  checkCRC: PCardinal; ZlibFormat: Boolean; TempBufSize: integer): cardinal;
var
  z: TZLib;
  code: integer;
  temp: array[word] of word; // 128KB
begin
  z.Init(src, srcLen, tmp, checkCRC, @temp, SizeOf(temp), TempBufSize);
  if z.UncompressInit(ZlibFormat) then
    try
      repeat
        code := z.Check(z.Uncompress(Z_FINISH),
          [Z_OK, Z_STREAM_END, Z_BUF_ERROR], 'UncompressStream');
        z.DoFlush;
      until code = Z_STREAM_END;
    finally
      z.UncompressEnd;
    end;
  result := z.Stream.total_out;
end;

function CompressZipString(src: pointer; srcLen: integer; CompressionLevel: integer;
  ZlibFormat: Boolean; TempBufSize: integer): RawByteString;
var
  s: TRawByteStringStream;
begin
  if (src = nil) or (srcLen <= 0) then
  begin
    result := '';
    exit;
  end;
  s := TRawByteStringStream.Create;
  try
    CompressStream(src, srcLen, s, CompressionLevel, ZlibFormat, TempBufSize);
    result := s.DataString;
  finally
    s.Free;
  end;
end;

function CompressZipString(const src: RawByteString; CompressionLevel: integer;
  ZlibFormat: Boolean; TempBufSize: integer): RawByteString;
begin
  result := CompressZipString(pointer(src), length(src), CompressionLevel,
    ZlibFormat, TempBufSize);
end;

function UncompressZipString(src: pointer; srcLen: integer;
  checkCRC: PCardinal; ZlibFormat: Boolean; TempBufSize: integer): RawByteString;
var
  s: TRawByteStringStream;
begin
  if (src = nil) or (srcLen <= 0) then
  begin
    result := '';
    exit;
  end;
  s := TRawByteStringStream.Create;
  try
    UncompressStream(src, srcLen, s, checkCRC, ZlibFormat, TempBufSize);
    result := s.DataString;
  finally
    s.Free;
  end;
end;

function UncompressZipString(const src: RawByteString; ZlibFormat: Boolean;
  TempBufSize: integer): RawByteString;
begin
  result := UncompressZipString(pointer(src), length(src), nil, ZlibFormat,
    TempBufSize);
end;

function CRC32string(const aString: RawByteString): cardinal;
begin
  result := length(aString);
  if result <> 0 then
    result := crc32(0, pointer(aString), result);
end;


end.

