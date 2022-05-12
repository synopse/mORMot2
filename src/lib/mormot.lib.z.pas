/// low-level access to the zlib/libdeflate API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.z;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler zlib API
   - Low-Level ZLib Streaming Access
   - Low-Level libdeflate in-memory Compression Library
   - Simple Wrapper Functions for Deflate/ZLib Process

  FPC Intel-Linux and Win32 use the faster libdeflate for in-memory compression

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$if not defined(ZLIBSTATIC) and
     not defined(ZLIBPAS) and
     not defined(ZLIBRTL) and
     not defined(ZLIBEXT)}

  // select best known choice if not overriden for the whole project
  {$ifdef FPC}
    {$ifdef OSWINDOWS}
      {$define ZLIBSTATIC} // FPC Win32 Win64: we supply our static .o
    {$else}
      {$ifdef OSANDROID}
        {$define ZLIBPAS}  // FPC Android: paszlib (Alf reported problems)
      {$else}
        {$define ZLIBEXT}  // FPC other POSIX: system's libz.so
      {$endif OSANDROID}
    {$endif OSWINDOWS}
  {$else not FPC}
    {$ifdef WIN32}
      {$define ZLIBSTATIC} // Delphi Win32: our static .obj
    {$endif WIN32}
    {$ifdef WIN64}
      {$define ZLIBRTL}    // Delphi Win64: system.zlib.pas from Delphi RTL
    {$endif WIN64}
  {$endif FPC}

{$ifend}


{.$define LIBDEFLATESTATIC}
// you may try to enable it e.g. for arm/aarch64-linux (not tested yet)

{$ifdef NOLIBDEFLATESTATIC}
  {$undef LIBDEFLATESTATIC}
{$endif NOLIBDEFLATESTATIC}

uses
  sysutils,
  classes,
  {$ifdef ZLIBPAS}
  paszlib,
  {$endif ZLIBPAS}
  {$ifdef ZLIBRTL}
  zlib,
  {$endif ZLIBRTL}
  {$if defined(LIBDEFLATESTATIC) or
       defined(ZLIBSTATIC)}
  mormot.lib.static, // some definitions to properly link libdeflate
  {$ifend}
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
  {$define ZLIBC}
  {$endif ZLIBEXT}

  {$ifdef ZLIBSTATIC}
  // our statically linked library expects 32-bit long/crc even on FPC Win64
  TZLong = cardinal;
  TZCRC = cardinal;
  {$define ZLIBC}
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
  EZLib = class(ExceptionWithProps);

  /// main access to the zlib API for compression/uncompression
  // - we encapsulated all low-level C calls into this object-oriented structure
  // - see CompressStream/UncompressStream for actual use of its methods
  TZLib = object
  private
    FlushStream: TStream;
    FlushBuffer: pointer;
    FlushSize: integer;
    FlushBufferOwned: boolean;
    FlushCheckCRC: PCardinal;
  public
    /// raw zlib Stream information
    Stream: TZStream;
    /// 64-bit number of bytes written to the output
    // - Stream.total_out may be 32-bit on some platforms
    Written: Int64;
    /// reset and prepare the internal Stream structure for two memory buffers
    procedure Init(src, dst: pointer; srcLen, dstLen: integer); overload;
    /// reset and prepare the internal Stream structure for a destination stream
    // - using a temporary buffer
    procedure Init(src: pointer; srcLen: integer; dst: TStream; checkcrc: PCardinal;
      temp: pointer; tempsize, expectedsize: integer); overload;
    /// prepare the Deflate Compression
    // - by default, will use the deflate/.zip header-less format, but you may set
    // ZlibFormat=true to add an header, as expected by zlib (and pdf)
    function CompressInit(CompressionLevel: integer; ZlibFormat: boolean): boolean;
    /// apply Deflate Compression over one memory block as defined in STream
    function Compress(Flush: integer): integer;
    /// finalize the Deflate Compression
    function CompressEnd: integer;
    /// prepare the Inflate Uncompression
    // - ZLibFormat defines the expected layout, and should match CompressInit()
    function UncompressInit(ZlibFormat: boolean): boolean;
    /// apply Inflate Uncompression over one memory block as defined in STream
    function Uncompress(Flush: integer): integer;
    /// finalize the Inflate Uncompression
    function UncompressEnd: integer;
    /// low-level flush of the compressed data pending in the internal buffer
    procedure DoFlush(Code: integer);
    /// low-level check of the code returned by the ZLib library
    // - raise EZLib Exception on error
    function Check(const Code: integer; const ValidCodes: array of integer;
      const Context: string = ''): integer;
  end;

/// compute the crc32 checksum of the supplied memory buffer
// - can use the much faster libdeflate (16GB/s) instead of plain zlib (800MB/s)
function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC;
  {$ifdef LIBDEFLATESTATIC} inline; {$else}
  {$ifdef ZLIBEXT} cdecl; {$else} {$ifdef ZLIBSTATIC} cdecl; {$else}
    {$ifdef FPC} inline; {$endif} {$endif} {$endif} {$endif}

/// compute the adler32 checksum of the supplied memory buffer
// - can use the much faster libdeflate (22GB/s) instead of plain zlib (1.8GB/s)
function adler32(adler: TZCRC; buf: pointer; len: cardinal): TZCRC;
  {$ifdef LIBDEFLATESTATIC} inline; {$else}
  {$ifdef ZLIBEXT} cdecl; {$else} {$ifdef ZLIBSTATIC} cdecl; {$else}
    {$ifdef FPC} inline; {$endif} {$endif} {$endif} {$endif}

/// compute the maximum potential compressed size of an uncompressed buffer
function zlibCompressMax(input: PtrUInt): PtrUInt;
  {$ifdef CPU64} inline; {$endif}


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

  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = -1;

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


{ *******************   Low-Level libdeflate in-memory Compression Library }

{$ifdef LIBDEFLATESTATIC}

{
  libdeflate is a library for fast, whole-buffer DEFLATE-based compression and
  decompression.

  libdeflate is heavily optimized.  It is significantly faster than the zlib
  library, both for compression and decompression, and especially on x86
  processors.  In addition, libdeflate provides optional high compression modes
  that provide a better compression ratio than the zlib's "level 9" (up to 12).

  Copyright 2016 Eric Biggers - MIT License

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation files
  (the "Software"), to deal in the Software without restriction,
  including without limitation the rights to use, copy, modify, merge,
  publish, distribute, sublicense, and/or sell copies of the Software,
  and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

type
  /// opaque pointer maintaing a libdeflate compressor instance
  PLibDeflateCompressor = type pointer;

/// allocates a new compressor that supports DEFLATE, zlib, and gzip compression
// - 'compression_level' is the compression level on a zlib-like scale but with
// a higher maximum value (1 = fastest, 6 = medium/default, 9 = slow, 12 = slowest)
// Level 0 is also supported and means "no compression", specifically "create
// a valid stream, but only emit uncompressed blocks" (this will expand the
// data slightly)
// - The return value is a pointer to the new compressor, or nil if out of
// memory or if the compression level is invalid (i.e. outside range [0, 12])
//-  Note: for compression, the sliding window size is defined at compilation
// time to 32768, the largest size permissible in the DEFLATE format.  It cannot
// be changed at runtime.
// - A single compressor is not safe to use by multiple threads concurrently.
// However, different threads may use different compressors concurrently
function libdeflate_alloc_compressor(
  compression_level: integer): PLibDeflateCompressor; cdecl;

/// performs raw DEFLATE compression on a buffer of data
// - The function attempts to compress 'in_nbytes' bytes of data located at
// 'in_buf' and write the results to 'out_buf', which has space for at least
// 'out_nbytes_avail' bytes
// - The return value is the compressed size in bytes, or 0 if the data
// could not be compressed to 'out_nbytes_avail' bytes or fewer.
function libdeflate_deflate_compress(
  libdeflate_compressor: PLibDeflateCompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt): PtrInt; cdecl;

/// returns a worst-case upper bound on the number of bytes of compressed data
// that may be produced by compressing any buffer of length less than or equal to
// 'in_nbytes' using libdeflate_deflate_compress() with the specified compressor
// - As a special case, 'compressor' may be nil.  This causes the bound to be
// taken across *any* libdeflate_compressor that could ever be allocated with
// this build of the library, with any options.
// -  Note that this function is not necessary in many applications.  With
// block-based compression, it is usually preferable to separately store the
// uncompressed size of each block and to store any blocks that did not compress
// to less than their original size uncompressed.  In that scenario, there is no
// need to know the worst-case compressed size, since the maximum number of
// bytes of compressed data that may be used would always be one less than the
// input length.  You can just pass a buffer of that size to
// libdeflate_deflate_compress() and store the data uncompressed if
// libdeflate_deflate_compress() returns 0, indicating that the compressed data
// did not fit into the provided output buffer.
function libdeflate_deflate_compress_bound(
  libdeflate_compressor: PLibDeflateCompressor; in_nbytes: PtrInt): PtrInt; cdecl;

/// Like libdeflate_deflate_compress(), but stores the data in the zlib format
function libdeflate_zlib_compress(
  libdeflate_compressor: PLibDeflateCompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt): PtrInt; cdecl;

/// Like libdeflate_deflate_compress_bound(), but assumes the data will be
// compressed with libdeflate_zlib_compress()
function libdeflate_zlib_compress_bound(
  libdeflate_compressor: PLibDeflateCompressor; in_nbytes: PtrInt): PtrInt; cdecl;

{
/// Like libdeflate_deflate_compress(), but stores the data in the gzip format
function libdeflate_gzip_compress(
  libdeflate_compressor: PLibDeflateCompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt): PtrInt; cdecl;

/// Like libdeflate_deflate_compress_bound(), but assumes the data will be
// compressed with libdeflate_gzip_compress()
function libdeflate_gzip_compress_bound(
  libdeflate_compressor: PLibDeflateCompressor; in_nbytes: PtrInt): PtrInt; cdecl;
}

/// frees a compressor that was allocated with libdeflate_alloc_compressor()
// - If a nil pointer is passed in, no action is taken
procedure libdeflate_free_compressor(
  libdeflate_compressor: PLibDeflateCompressor); cdecl;


type
  /// opaque pointer maintaing a libdeflate decompressor instance
  PLibDeflateDecompressor = type pointer;

/// allocates a new decompressor that can be used for DEFLATE, zlib, and gzip
// - The return value is a pointer to the new decompressor, or nil if out of memory
// - This function takes no parameters, and the returned decompressor is valid for
// decompressing data that was compressed at any compression level and with any
// sliding window size
// - A single decompressor is not safe to use by multiple threads concurrently.
// However, different threads may use different decompressors concurrently.
function libdeflate_alloc_decompressor: PLibDeflateDecompressor; cdecl;

/// Result of a call to libdeflate_deflate_decompress(),
// libdeflate_zlib_decompress(), or libdeflate_gzip_decompress().
// - LIBDEFLATE_BAD_DATA is returned on invalid input
// - LIBDEFLATE_SHORT_OUTPUT if 'actual_out_nbytes_ret' was not set, but
// 'out_nbytes_avail' didn't match the output size
// - LIBDEFLATE_INSUFFICIENT_SPACE if 'out_nbytes_avail' was not big enough
type
  TLibDeflateResult = (
    LIBDEFLATE_SUCCESS,
    LIBDEFLATE_BAD_DATA,
    LIBDEFLATE_SHORT_OUTPUT,
    LIBDEFLATE_INSUFFICIENT_SPACE);

/// decompresses the DEFLATE-compressed stream
// - from the buffer 'in_buf' with compressed size up to 'in_nbytes' bytes
// - uncompress data into 'outçbuf', a buffer with size 'out_nbytes_avail'
// - If decompression succeeds, then 0 (LIBDEFLATE_SUCCESS) is returned.
// Otherwise, a nonzero result code such as LIBDEFLATE_BAD_DATA is returned.  If
// a nonzero result code is returned, then the contents of the output buffer are
// undefined.
// - Decompression stops at the end of the DEFLATE stream (as indicated by the
// BFINAL flag), even if it is actually shorter than 'in_nbytes' bytes.
// - libdeflate_deflate_decompress() can be used in cases where the actual
// uncompressed size is known (recommended) or unknown (not recommended): - If
// the actual uncompressed size is known, then pass the actual
// uncompressed size as 'out_nbytes_avail' and pass nil for
// 'actual_out_nbytes_ret'.  This makes libdeflate_deflate_decompress() fail
// with LIBDEFLATE_SHORT_OUTPUT if the data decompressed to fewer than the
// specified number of bytes. - If the actual uncompressed size is unknown, then
// provide a non-nil 'actual_out_nbytes_ret' and provide a buffer with some size
// 'out_nbytes_avail' that you think is large enough to hold all the
// uncompressed data.  In this case, if the data decompresses to less than
// or equal to 'out_nbytes_avail' bytes, then libdeflate_deflate_decompress()
// will write the actual uncompressed size to actual_out_nbytes_ret and
// return 0 (LIBDEFLATE_SUCCESS).  Otherwise, it will return
// LIBDEFLATE_INSUFFICIENT_SPACE if  the provided buffer was
// not large enough but no other problems were encountered, or another
// nonzero result code if decompression failed for another reason.
function libdeflate_deflate_decompress(
  decompressor: PLibDeflateDecompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt;
  actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; cdecl;

/// libdeflate_deflate_decompress() with an 'actual_in_nbytes_ret' argument
// - If decompression succeeds and 'actual_in_nbytes_ret' is not nil,
// then the actual compressed size of the DEFLATE stream (aligned to the next
// byte boundary) is written to *actual_in_nbytes_ret.
function libdeflate_deflate_decompress_ex(
  decompressor: PLibDeflateDecompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt;
  actual_in_nbytes_ret, actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; cdecl;

/// Like libdeflate_deflate_decompress(), but assumes the zlib wrapper format
function libdeflate_zlib_decompress(
  decompressor: PLibDeflateDecompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt;
  actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; cdecl;

/// libdeflate_zlib_decompress() with an 'actual_in_nbytes_ret' argument
function libdeflate_zlib_decompress_ex(
  decompressor: PLibDeflateDecompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt;
  actual_in_nbytes_ret, actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; cdecl;

{
/// Like libdeflate_deflate_decompress(), but assumes the gzip wrapper format
function libdeflate_gzip_decompress(
  decompressor: PLibDeflateDecompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt;
  actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; cdecl;

/// libdeflate_gzip_decompress() with an 'actual_in_nbytes_ret' argument
function libdeflate_gzip_decompress_ex(
  decompressor: PLibDeflateDecompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt;
  actual_in_nbytes_ret, actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; cdecl;
}

/// frees a decompressor that was allocated with libdeflate_alloc_decompressor()
// - If a nil pointer is passed in, no action is taken.
procedure libdeflate_free_decompressor(
  decompressor: PLibDeflateDecompressor); cdecl;


/// updates a running Adler-32 checksum with 'len' bytes of data and returns the
// updated checksum
// - When starting a new checksum, the required initial value for 'adler' is 1
// - This value is also returned when 'buffer' is specified as nil.
// - on x86_64 with AVX, performance is more than 22GB/s whereas zlib is 1.8GB/s
function libdeflate_adler32(adler: cardinal;
  buffer: pointer; len: PtrInt): cardinal; cdecl;

/// updates a running CRC-32 checksum with 'len' bytes of data and returns the
// updated checksum
// - When starting a new checksum, the required initial value for 'crc' is 1
// - This value is also returned when 'buffer' is specified as nil.
// - on x86_64 with AVX, performance is more than 16GB/s whereas zlib is 800MB/s
function libdeflate_crc32(crc: cardinal;
  buffer: pointer; len: PtrInt): cardinal; cdecl;

{  to avoid GPF on i386-linux, we hardcoded libdeflate_malloc/free in utils.c
/// Install a custom memory allocator to be used by libdeflate
// - initialization section of this unit will assign the Delphi/FPC RTL heap
procedure libdeflate_set_memory_allocator(
  malloc_func: pointer; free_func: pointer);
}

{$endif LIBDEFLATESTATIC}


{ ******************* Simple Wrapper Functions for Deflate/ZLib Process }

/// in-memory ZLib compression
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
// - use faster libdeflate instead of plain zlib if available
function CompressMem(src, dst: pointer; srcLen, dstLen: PtrInt;
  CompressionLevel: integer = 6; ZlibFormat: boolean = false) : PtrInt;

/// in-memory ZLib uncompression
// - ZLibFormat defines the expected layout, and should match CompressMem()
// - use faster libdeflate instead of plain zlib if available
function UncompressMem(src, dst: pointer; srcLen, dstLen: PtrInt;
  ZlibFormat: boolean = false) : PtrInt;

/// ZLib compression from memory into a stream
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressStream(src: pointer; srcLen: integer;
  tmp: TStream; CompressionLevel: integer = 6; ZlibFormat: boolean = false;
  TempBufSize: integer = 0): cardinal;

/// ZLib decompression from memory into a stream
// - return the number of bytes written into the stream
// - if checkCRC if not nil, it will contain the crc32; if aStream is nil, it
// will only calculate the crc of the the Uncompressed memory block
// - ZLibFormat defines the expected layout, and should match CompressStream()
function UncompressStream(src: pointer; srcLen: integer; tmp: TStream;
  checkCRC: PCardinal = nil; ZlibFormat: boolean = false;
  TempBufSize: integer = 0): cardinal;

/// ZLib compression from memory into a RawByteString variable
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressZipString(src: pointer; srcLen: integer;
  CompressionLevel: integer = 6; ZlibFormat: boolean = false;
  TempBufSize: integer = 0): RawByteString; overload;

/// ZLib compression to and from RawByteString variables
// - by default, will use the deflate/.zip header-less format, but you may set
// ZlibFormat=true to add an header, as expected by zlib (and pdf)
function CompressZipString(const src: RawByteString;
  CompressionLevel: integer = 6; ZlibFormat: boolean = false;
  TempBufSize: integer = 0): RawByteString; overload;

/// ZLib decompression from memory into a RawByteString variable
// - if checkCRC if not nil, it will contain the crc32; if aStream is nil, it
// will only calculate the crc of the the Uncompressed memory block
// - ZLibFormat defines the expected layout, and should match CompressZipString()
function UncompressZipString(src: pointer; srcLen: integer;
  checkCRC: PCardinal = nil; ZlibFormat: boolean = false;
  TempBufSize: integer = 0): RawByteString; overload;

/// ZLib decompression to and from RawByteString variables
// - ZLibFormat defines the expected layout, and should match CompressZipString()
function UncompressZipString(const src: RawByteString;
  ZlibFormat: boolean = false; TempBufSize: integer = 0): RawByteString; overload;

/// just hash aString with CRC32 algorithm
// - crc32 is better than adler32 for short strings
// - use the much faster libdeflate instead of plain zlib if available
function CRC32string(const aString: RawByteString): cardinal;


implementation


{ ****************** Low-Level ZLib Streaming Access }

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

function ZLibErrorText(code: integer): PAnsiChar;
begin
  Code := 2 - Code;
  if (Code >= 0) and
     (Code <= 8) then
    result := z_errmsg[Code]
  else
    result := nil;
end;

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
begin
  // direct use of the (FastMM4) delphi heap for all zip memory allocation
  Getmem(result, Items * Size);
end;

procedure zcfree(AppData, Block: Pointer); cdecl;
begin
  // direct use of the (FastMM4) delphi heap for all zip memory allocation
  FreeMem(Block);
end;

procedure memset(P: Pointer; B: integer; count: integer); cdecl;
begin
  FillCharFast(P^, count, B);
end;

procedure memcpy(dest, source: Pointer; count: integer); cdecl;
begin
  MoveFast(source^, dest^, count);
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
{$ifndef LIBDEFLATESTATIC}
function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC; cdecl; external;
function adler32(adler: TZCRC; buf: pointer; len: cardinal): TZCRC; cdecl; external;
{$endif LIBDEFLATESTATIC}

{$endif ZLIBSTATIC}


{$ifdef ZLIBEXT}

const
{$ifdef OSANDROID}
  libz = '/usr/lib/libz.so';
{$else}
  {$ifdef FPC}
  libz = 'z';
  {$linklib libz}
  {$else}
  libz = 'libz.so'
  {$endif FPC}
{$endif OSANDROID}

function deflate(var strm: TZStream; flush: integer): integer; cdecl; external libz;
function deflateEnd(var strm: TZStream): integer; cdecl; external libz;
function inflate(var strm: TZStream; flush: integer): integer; cdecl; external libz;
function inflateEnd(var strm: TZStream): integer; cdecl; external libz;
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
{$ifndef LIBDEFLATESTATIC}
function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC; cdecl; external libz;
function adler32(adler: TZCRC; buf: pointer; len: cardinal): TZCRC; cdecl; external libz;
{$endif LIBDEFLATESTATIC}

{$endif ZLIBEXT}


{$ifdef ZLIBPAS}

function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := paszlib.crc32(crc, buf, len);
end;

function adler32(adler: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := paszlib.adler32(adler, buf, len);
end;

{$endif ZLIBPAS}

{$ifdef ZLIBRTL}

function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := zlib.crc32(crc, buf, len);
end;

function adler32(adler: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := zlib.adler32(adler, buf, len);
end;

{$endif ZLIBRTL}


{ TZLib }

{$ifndef ZLIBPAS}

function zlibAllocMem(AppData: Pointer; Items, Size: cardinal): Pointer; cdecl;
begin
  Getmem(result, Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

{$endif ZLIBPAS}

procedure TZLib.Init(src, dst: pointer; srcLen, dstLen: integer);
begin
  FillCharFast(Stream, SizeOf(Stream), 0);
  FlushStream := nil;
  FlushCheckCRC := nil;
  FlushBufferOwned := false;
  Stream.next_in := src;
  Stream.avail_in := srcLen;
  Stream.next_out := dst;
  Stream.avail_out := dstLen;
  {$ifndef ZLIBPAS}
  Stream.zalloc := @zlibAllocMem; // even under Linux, use program heap
  Stream.zfree  := @zlibFreeMem;
  {$endif ZLIBPAS}
  Written := 0;
end;

procedure TZLib.Init(src: pointer; srcLen: integer; dst: TStream;
  checkcrc: PCardinal; temp: pointer; tempsize, expectedsize: integer);
begin
  if tempsize < expectedsize then
  begin
    GetMem(FlushBuffer, expectedsize);
    FlushSize := expectedsize;
  end
  else
  begin
    FlushBuffer := temp;
    FlushSize := tempsize;
  end;
  Init(src, FlushBuffer, srcLen, FlushSize);
  FlushBufferOwned := FlushBuffer <> temp;
  FlushStream := dst;
  if checkcrc <> nil then
    checkcrc^ := 0;
  FlushCheckCRC := checkcrc;
end;

function TZLib.CompressInit(CompressionLevel: integer; ZlibFormat: boolean): boolean;
var
  bits: integer;
begin
  if ZlibFormat then
    bits := MAX_WBITS
  else
    bits := -MAX_WBITS;
  if CompressionLevel > 9 then
    CompressionLevel := 9; // libdeflate allows additional 10,11,12 level
  result := deflateInit2_(Stream, CompressionLevel, Z_DEFLATED, bits,
    DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, SizeOf(Stream)) >= 0;
  if FlushBufferOwned and
     not result then
  begin
    FreeMem(FlushBuffer);
    FlushBufferOwned := false;
  end;
end;

function TZLib.Compress(Flush: integer): integer;
begin
  result := deflate(Stream, Flush);
end;

function TZLib.CompressEnd: integer;
begin
  result := deflateEnd(Stream);
  if FlushBufferOwned then
  begin
    FreeMem(FlushBuffer);
    FlushBufferOwned := false;
  end;
end;

function TZLib.UncompressInit(ZlibFormat: boolean): boolean;
var
  bits: integer;
begin
  if ZlibFormat then
    bits := MAX_WBITS
  else
    bits := -MAX_WBITS;
  result := inflateInit2_(Stream, bits, ZLIB_VERSION, SizeOf(Stream)) >= 0;
  if FlushBufferOwned and
     not result then
  begin
    FreeMem(FlushBuffer);
    FlushBufferOwned := false;
  end;
end;

function TZLib.Uncompress(Flush: integer): integer;
begin
  result := inflate(Stream, Flush);
end;

function TZLib.UncompressEnd: integer;
begin
  result := inflateEnd(Stream);
  if FlushBufferOwned then
  begin
    FreeMem(FlushBuffer);
    FlushBufferOwned := false;
  end;
end;

procedure TZLib.DoFlush(Code: integer);
var
  n: integer;
begin
  n := FlushSize - integer(Stream.avail_out);
  if n <> 0 then
  begin
    if FlushCheckCRC <> nil then
      FlushCheckCRC^ := crc32(FlushCheckCRC^, FlushBuffer, n);
    FlushStream.WriteBuffer(FlushBuffer^, n);
    inc(Written, n);
  end
  else if Code = Z_BUF_ERROR then
    Check(Code, [], 'DoFlush with no output'); // may occur on corrupted input 
  Stream.next_out := FlushBuffer;
  Stream.avail_out := FlushSize;
end;

function TZLib.Check(const Code: integer; const ValidCodes: array of integer;
  const Context: string): integer;
var
  i: PtrInt;
begin
  if Code = Z_MEM_ERROR then
    OutOfMemoryError;
  result := Code;
  for i := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[i] = Code then
      exit;
  raise EZLib.CreateFmt('Error %d [%s] during %s process (avail in=%d out=%d)',
    [Code, ZLibErrorText(Code), Context, Stream.avail_in, Stream.avail_out]);
end;

function zlibCompressMax(input: PtrUInt): PtrUInt;
begin
  // zlib compresBound = len + (len >> 12) + (len >> 14) +  (len >> 25) + 13
  result := input + input shr 12 + input shr 14 + input shr 25 + 256;
end;


{ *******************   Low-Level libdeflate in-memory Compression Library }

{$ifdef LIBDEFLATESTATIC}

function libdeflate_alloc_compressor; external;
function libdeflate_deflate_compress; external;
function libdeflate_deflate_compress_bound; external;
function libdeflate_zlib_compress; external;
function libdeflate_zlib_compress_bound; external;
procedure libdeflate_free_compressor; external;

function libdeflate_alloc_decompressor; external;
function libdeflate_deflate_decompress; external;
function libdeflate_deflate_decompress_ex; external;
function libdeflate_zlib_decompress; external;
function libdeflate_zlib_decompress_ex; external;
procedure libdeflate_free_decompressor; external;

function libdeflate_adler32; external;
function libdeflate_crc32; external;

// original code is patched for proper linking and cdecl use
// - see res/static/libdeflate for patched source and build instructions
// - used libdeflatepas.a to avoid collision with libdeflate.a of libdeflate-dev
{$ifdef OSLINUX}
  {$ifdef CPUX86}
    {$linklib ..\..\static\i386-linux\libdeflatepas.a}
  {$endif CPUX86}
  {$ifdef CPUX64}
    {$linklib ..\..\static\x86_64-linux\libdeflatepas.a}
  {$endif CPUX64}
  {$ifdef CPUAARCH64}
    // compiles but untested yet
    {$L ..\..\static\aarch64-linux\libdeflate_u.o}  // utils.o
    {$L ..\..\static\aarch64-linux\libdeflate_cf.o} // cpu_features.o
    {$L ..\..\static\aarch64-linux\libdeflate_a.o}  // adler32.o
    {$L ..\..\static\aarch64-linux\libdeflate_c.o}  // crc32.o
    {$L ..\..\static\aarch64-linux\libdeflate_dc.o} // deflate_compress.o
    {$L ..\..\static\aarch64-linux\libdeflate_dd.o} // deflate_decompress.o
    {$L ..\..\static\aarch64-linux\libdeflate_zc.o} // zlib_compress.o
    {$L ..\..\static\aarch64-linux\libdeflate_zd.o} // zlib_decompress.o
    {$linklib ..\..\static\aarch64-linux\libgcc.a}
  {$endif CPUAARCH64}
  {$ifdef CPUARM}
    // current supplied .o don't link yet
    {$L ..\..\static\arm-linux\libdeflate_u.o}  // utils.o
    {$L ..\..\static\arm-linux\libdeflate_cf.o} // cpu_features.o
    {$L ..\..\static\arm-linux\libdeflate_a.o}  // adler32.o
    {$L ..\..\static\arm-linux\libdeflate_c.o}  // crc32.o
    {$L ..\..\static\arm-linux\libdeflate_dc.o} // deflate_compress.o
    {$L ..\..\static\arm-linux\libdeflate_dd.o} // deflate_decompress.o
    {$L ..\..\static\arm-linux\libdeflate_zc.o} // zlib_compress.o
    {$L ..\..\static\arm-linux\libdeflate_zd.o} // zlib_decompress.o
    {$linklib ..\..\static\arm-linux\libgcc.a}
  {$endif CPUARM}
  const
    _PU = '';
{$endif OSLINUX}

{$ifdef OSWINDOWS}
  {$ifdef CPUX86}
    {$linklib ..\..\static\i386-win32\libdeflatepas.a}
    const
      _PU = '_';
  {$endif CPUX86}
  // note: FPC 3.2 + Win64 internal linker makes internal error 200603061
  // - to compile on Win64, try the -Xe option or a newer FPC
  {$ifdef CPUX64}
    {$linklib ..\..\static\x86_64-win64\libdeflatepas.a}
    const
      _PU = '';
  {$endif CPUX64}
{$endif OSWINDOWS}


// utils.c source is patched to directly use those functions

function libdeflate_malloc(size: cardinal): Pointer;
  public name _PU + 'libdeflate_malloc'; cdecl;
begin
  result := GetMem(size); // use Delphi/FPC RTL heap
end;

procedure libdeflate_free(P: Pointer);
  public name _PU + 'libdeflate_free'; cdecl;
begin
  FreeMem(P);
end;

function crc32(crc: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := libdeflate_crc32(crc, buf, len);
end;

function adler32(adler: TZCRC; buf: pointer; len: cardinal): TZCRC;
begin
  result := libdeflate_adler32(adler, buf, len);
end;

{$endif LIBDEFLATESTATIC}



{ ******************* Simple Wrapper Functions for Deflate/ZLib Process }

{$ifdef LIBDEFLATESTATIC}

function CompressMem(src, dst: pointer; srcLen, dstLen: PtrInt;
  CompressionLevel: integer; ZlibFormat: boolean): PtrInt;
var
  comp: PLibDeflateCompressor;
begin
  comp := libdeflate_alloc_compressor(CompressionLevel);
  if comp = nil then
    raise EZLib.CreateFmt(
      'CompressMem: libdeflate_alloc_compressor(%d) failed', [CompressionLevel]);
  if ZlibFormat then
    result := libdeflate_zlib_compress(comp, src, srcLen, dst, dstLen)
  else
    result := libdeflate_deflate_compress(comp, src, srcLen, dst, dstLen);
  libdeflate_free_compressor(comp);
  if result = 0 then
    raise EZLib.Create('CompressMem: libdeflate failure');
end;

function UncompressMem(src, dst: pointer; srcLen, dstLen: PtrInt;
  ZlibFormat: boolean): PtrInt;
var
  dec: PLibDeflateDecompressor;
  res: TLibDeflateResult;
begin
  dec := libdeflate_alloc_decompressor;
  if dec = nil then
    raise EZLib.Create('UncompressMem: libdeflate_alloc_decompressor failed');
  if ZlibFormat then
    res := libdeflate_zlib_decompress(dec, src, srcLen, dst, dstLen, @result)
  else
    res := libdeflate_deflate_decompress(dec, src, srcLen, dst, dstLen, @result);
  libdeflate_free_decompressor(dec);
  if res <> LIBDEFLATE_SUCCESS  then
    raise EZLib.CreateFmt('UncompressMem: libdeflate = %d', [ord(res)]);
end;

{$else}

function CompressMem(src, dst: pointer; srcLen, dstLen: PtrInt;
  CompressionLevel: integer; ZlibFormat: boolean): PtrInt;
var
  z: TZLib;
begin
  z.Init(src, dst, srcLen, dstLen);
  if CompressionLevel > 9 then
    CompressionLevel := 9; // levels 10,11,12 are implemented by libdeflate
  if z.CompressInit(CompressionLevel, ZlibFormat) then
    try
      z.Check(z.Compress(Z_FINISH), [Z_STREAM_END, Z_OK], 'CompressMem');
    finally
      z.CompressEnd;
    end;
  result := z.Stream.total_out;
end;

function UncompressMem(src, dst: pointer; srcLen, dstLen: PtrInt;
  ZlibFormat: boolean): PtrInt;
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

{$endif LIBDEFLATESTATIC}

function CompressStream(src: pointer; srcLen: integer; tmp: TStream;
  CompressionLevel: integer; ZlibFormat: boolean; TempBufSize: integer): cardinal;
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
        z.DoFlush(code);
      until code = Z_STREAM_END;
    finally
      z.CompressEnd;
    end;
  result := z.Stream.total_out;
end;

function UncompressStream(src: pointer; srcLen: integer; tmp: TStream;
  checkCRC: PCardinal; ZlibFormat: boolean; TempBufSize: integer): cardinal;
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
        z.DoFlush(code);
      until code = Z_STREAM_END;
      z.DoFlush(code);
    finally
      z.UncompressEnd;
    end;
  result := z.Stream.total_out;
end;

function CompressZipString(src: pointer; srcLen: integer; CompressionLevel: integer;
  ZlibFormat: boolean; TempBufSize: integer): RawByteString;
var
  s: TRawByteStringStream;
begin
  if (src = nil) or
     (srcLen <= 0) then
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
  ZlibFormat: boolean; TempBufSize: integer): RawByteString;
begin
  result := CompressZipString(pointer(src), length(src), CompressionLevel,
    ZlibFormat, TempBufSize);
end;

function UncompressZipString(src: pointer; srcLen: integer;
  checkCRC: PCardinal; ZlibFormat: boolean; TempBufSize: integer): RawByteString;
var
  // we don't know the uncompressed size -> use a resizable TStream
  s: TRawByteStringStream;
begin
  if (src = nil) or
     (srcLen <= 0) then
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

function UncompressZipString(const src: RawByteString; ZlibFormat: boolean;
  TempBufSize: integer): RawByteString;
begin
  result := UncompressZipString(
    pointer(src), length(src), nil, ZlibFormat, TempBufSize);
end;

function CRC32string(const aString: RawByteString): cardinal;
begin
  result := length(aString);
  if result <> 0 then
    result := crc32(0, pointer(aString), result);
end;


// we need some wrappers to fix any parameter or ABI issue
{$ifdef LIBDEFLATESTATIC}

function crc(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := libdeflate_crc32(crc, buf, len);
end;

function adler(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := libdeflate_adler32(crc, buf, len);
end;

{$else}

function crc(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := crc32(crc, buf, len);
end;

function adler(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := adler32(crc, buf, len);
end;

{$endif LIBDEFLATESTATIC}

initialization
  mormot.core.base.crc32 := @crc;
  mormot.core.base.adler32 := @adler;

end.

