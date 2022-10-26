/// low-level access to the Lizard/LZ5 API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.lizard;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler Lizard (LZ5) API
   - Low-Level Lizard API Process
   - TAlgoLizard/TAlgoLizardFast/TAlgoLizardHuffman High-Level Algorithms

  *****************************************************************************


  Some numbers, for a 53MB log file (taken from a production server):
  FPC Win32
     TAlgoSynLz 53 MB->5 MB: comp 563.7 MB/s decomp 815.3 MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 54.6 MB/s decomp 1.1 GB/s
     TAlgoLizardFast 53 MB->6.9 MB: comp 493.8 MB/s decomp 1 GB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 28.2 MB/s decomp 212 MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 58.6 MB/s decomp 182.5 MB/s
  FPC Win64
     TAlgoSynLz 53 MB->5 MB: comp 635.4 MB/s decomp 923.5 MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 61 MB/s decomp 1.8 GB/s
     TAlgoLizardFast 53 MB->6.8 MB: comp 674.2 MB/s decomp 1.6 GB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 40.2 MB/s decomp 255.1 MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 81.2 MB/s decomp 219.9 MB/s
  FPC Linux32
     TAlgoSynLz 53 MB->5 MB: comp 533.4 MB/s decomp 472.5 MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 44.8 MB/s decomp 1.2 GB/s
     TAlgoLizardFast 53 MB->6.9 MB: comp 515.5 MB/s decomp 1 GB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 60.2 MB/s decomp 413.3 MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 121.8 MB/s decomp 336.7 MB/s
  FPC Linux64
     TAlgoSynLz 53 MB->5 MB: comp 626.4 MB/s decomp 906.8 MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 53.6 MB/s decomp 1.8 GB/s
     TAlgoLizardFast 53 MB->6.8 MB: comp 700.3 MB/s decomp 1.6 GB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 70.5 MB/s decomp 544.5 MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 141.4 MB/s decomp 420.2 MB/s
  Note: Deflate was system zlib, not our faster libdeflate
  Conclusion: SynLZ has the best compression ratio for its compression speed,
    but Lizard is much faster at decompression, when working with big log filesiles.
  For small files (<1MB), SynLZ is always faster, and uses less memory than Lizard.

  Some numbers, taken from regression tests on an AARCH64 Oracle Cloud VM:
    TAlgoSynLZ 3.8 MB->2 MB: comp 285:150MB/s decomp 215:410MB/s
    TAlgoLizard 3.8 MB->1.9 MB: comp 18:9MB/s decomp 838:1631MB/s
    TAlgoLizardFast 3.8 MB->2.3 MB: comp 329:197MB/s decomp 1268:2113MB/s
    TAlgoLizardHuffman 3.8 MB->1.8 MB: comp 85:40MB/s decomp 391:821MB/s
    TAlgoDeflate 3.8 MB->1.5 MB: comp 30:12MB/s decomp 78:196MB/s
    TAlgoDeflateFast 3.8 MB->1.6 MB: comp 48:20MB/s decomp 73:173MB/s

  NOTE:
  - FOR DELPHI PLEASE DOWNLOAD external Lizard1-32.dll / Lizard1-64.dll
    from https://synopse.info/files/SynLizardLibs.7z

}

interface

{$I ..\mormot.defines.inc}


{.$define LIZARD_EXTERNALONLY}
// will force to use an external Lizard1-32.dll/Lizard1-64.dll/liblizard.so.1
// as available from https://synopse.info/files/SynLizardLibs.7z

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,      // for TSynLibrary
  mormot.core.unicode,
  mormot.core.buffers; // for TAlgoCompress


{ ****************** Low-Level Lizard Process }

const
  /// default compression level for TSynLizard.compress
  // - 0 value will let the library use level 17 - slow but efficient - method
  // - as used by AlgoLizard global TSynCompress instance
  LIZARD_DEFAULT_CLEVEL = 0;
  /// minimum compression level for TSynLizard.compress
  // - as used by AlgoLizardFast global TSynCompress instance
  LIZARD_MIN_CLEVEL = 10;
  /// fast huffman compression level for TSynLizard.compress
  // - better compression ratio than LIZARD_DEFAULT_CLEVEL, better compression
  // speed, but slower decompression
  LIZARD_HUFFMAN_CLEVEL = 41;
  /// maximum compression level for TSynLizard.compress
  LIZARD_MAX_CLEVEL = 49;


  /// default TSynLizardDynamic file name
  // - mainly for Delphi, since FPC will use static linked .o files under
  // Windows and Linux Intel 32/64 bits
  // - to be downloaded from from https://synopse.info/files/SynLizardLibs.7z
  {$ifdef Win32}
  LIZARD_LIB_NAME = 'Lizard1-32.dll';
  {$endif Win32}
  {$ifdef Win64}
  LIZARD_LIB_NAME = 'Lizard1-64.dll';
  {$endif Win64}
  {$ifdef OSPOSIX}
  LIZARD_LIB_NAME = 'liblizard.so.1';
  {$endif OSPOSIX}

type
  /// Lizard (formerly LZ5) lossless compression algorithm
  // - provides efficient compression with very fast decompression
  // - this class implements direct low-level access to the Lizard API - consider
  // using AlgoLizard/AlgoLizardFast global instances for easier use
  TSynLizard = class
  public
    //// will initialize the library
    constructor Create; virtual;
  public
    /// version number of the linked Lizard library
    versionNumber: function: integer; cdecl;
    /// maximum size that Lizard compression may output in a "worst case" scenario
    compressBound: function(inputSize: integer): integer; cdecl;
    /// compresses srcSize bytes from src into already allocated dst buffer of
    // size maxDstSize - which should be >= Lizard_compressBound(srcSize)
    // - returns number of bytes written into dst (necessarily <= maxDstSize),
    // or 0 if compression fails due to too small maxDstSize, <0 on other failure
    // - compressionLevel is from LIZARD_MIN_CLEVEL (10) to LIZARD_MAX_CLEVEL(49),
    // any value <10 (e.g. 0) will use 17, and value >49 will use 49
    // $ Lev Comp     Decomp    CompSize Ratio
    // $    7332 MB/s 8719 MB/s 211947520 100.00 (move)
    // $ 10 346 MB/s  2610 MB/s 103402971 48.79
    // $ 12 103 MB/s  2458 MB/s 86232422 40.69
    // $ 15 50 MB/s   2552 MB/s 81187330 38.31
    // $ 19 3.04 MB/s 2497 MB/s 77416400 36.53
    // $ 21 157 MB/s  1795 MB/s 89239174 42.10
    // $ 23 30 MB/s   1778 MB/s 81097176 38.26
    // $ 26 6.63 MB/s 1734 MB/s 74503695 35.15
    // $ 29 1.37 MB/s 1634 MB/s 68694227 32.41
    // $ 30 246 MB/s  909 MB/s  85727429 40.45
    // $ 32 94 MB/s   1244 MB/s 76929454 36.30
    // $ 35 47 MB/s   1435 MB/s 73850400 34.84
    // $ 39 2.94 MB/s 1502 MB/s 69807522 32.94
    // $ 41 126 MB/s  961 MB/s  76100661 35.91
    // $ 43 28 MB/s   1101 MB/s 70955653 33.48
    // $ 46 6.25 MB/s 1073 MB/s 65413061 30.86
    // $ 49 1.27 MB/s 1064 MB/s 60679215 28.63
    compress: function(src, dst: pointer;
      srcSize, maxDstSize, compressionLevel: integer): integer; cdecl;
    /// how much memory must be allocated for compress_extState()
    sizeofState: function(compressionLevel: integer): integer; cdecl;
    /// compresses using an external pre-allocated state buffer
    compress_extState: function(state: pointer;
      src, dst: pointer; srcSize, maxDstSize,
      compressionLevel: integer): integer; cdecl;
    /// decompresses srcSize bytes from src into already allocated dst buffer
    // - returns number of bytes written to dst (<= maxDstSize), or <=0 on failure
    // - this function is protected against buffer overflow exploits
    decompress_safe: function(src, dst: pointer;
      srcSize, maxDstSize: integer): integer; cdecl;
    /// partial decompression srcSize bytes from src into already allocated dst buffer
    // - returns number of bytes written to dst (<= maxDstSize), or <=0 on failure
    // - number can be <targetDstSize should the compressed block to decode be smaller
    // - this function is protected against buffer overflow exploits
    decompress_safe_partial: function(src, dst: pointer;
      srcSize, targetDstSize, maxDstSize: integer): integer; cdecl;
  end;


type
  /// try to load Lizard as an external library
  // - static linking is currently available only on FPC Win32/64 and Linux32/64
  // - this class is expected to access Lizard1-32.dll/Lizard1-64.dll files for
  // Delphi, e.g. as such:
  // ! TSynLizardDynamic.AlgoRegister;
  TSynLizardDynamic = class(TSynLizard)
  protected
    // mormot.core.os.pas cross-platform support external library support
    fLibrary: TSynLibrary;
    fLoaded: boolean;
  public
    /// will first search in the executable folder, then within the system path
    // - raise an Exception if the library file is not found, or not valid -
    // unless aRaiseNoException is set to true
    constructor Create(const aLibraryFile: TFileName = '';
      aRaiseNoException: boolean = false); reintroduce;
    /// unload the external library
    destructor Destroy; override;
    /// ensure Lizard compression is available
    // - returns TRUE if Lizard compression is available
    // - if there is a local Lizard1-32.dll/Lizard1-64.dll file, try to load it
    class function AlgoRegister: boolean;
    /// set to TRUE if Create successed
    // - may be used if aRaiseNoException parameter has been defined
    property Loaded: boolean read fLoaded;
    /// the loaded library file name
    function LibraryName: TFileName;
  end;


var
  /// direct access to the low-level Lizard (LZ5) library API
  // - is defined by default if Lizard was statically linked (under FPC)
  // - otherwise, you should execute explicitly:
  // ! if Lizard = nil then
  // !   Lizard := TSynLizardDynamic.Create;
  Lizard: TSynLizard;


{ ****************** TAlgoLizard/TAlgoLizardFast/TAlgoLizardHuffman High-Level Algorithms }

var
  /// implement Lizard compression in level 17 (LIZARD_DEFAULT_CLEVEL) as AlgoID=4
  // - is set by TSynLizard.Create, so available e.g. if library is statically
  // linked, or once TSynLizardDynamic.Create has been successfully called
  AlgoLizard: TAlgoCompress;
  /// implement Lizard compression in level 10 (LIZARD_MIN_CLEVEL) as AlgoID=5
  // - is set by TSynLizard.Create, so available e.g. if library is statically
  // linked, or once TSynLizardDynamic.Create has been successfully called
  AlgoLizardFast: TAlgoCompress;
  /// implement Lizard compression in level 41 (LIZARD_HUFFMAN_CLEVEL) as AlgoID=6
  // - is set by TSynLizard.Create, so available e.g. if library is statically
  // linked, or once TSynLizardDynamic.Create has been successfully called
  AlgoLizardHuffman: TAlgoCompress;


/// a TSynLogArchiveEvent handler which will archive and compress .log files
// using our proprietary AlgoLizardFast format
// - resulting file will have the .synlz extension and will be located
// in the aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM\'
// - use UnSynLZ.dpr tool to uncompress it into .log textual file - it will use
// the TAlgoCompress.AlgoID mechanism to recognize the SynLZ or Lizard format
// - AlgoLizardFast has been identified as a good alternative to AlgoSynLZ for
// compressing .log content
function EventArchiveLizard(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;


implementation

{
    Lizard Library - Original C Source Code Under BSD 2-Clause license
      Copyright (C) 2011-2016, Yann Collet.
      Copyright (C) 2016-2017, Przemyslaw Skibinski <inikep@gmail.com>
    All rights reserved.
}

{ ****************** Low-Level Lizard Process }

{$ifndef LIZARD_EXTERNALONLY}

function Lizard_versionNumber: integer; cdecl; external;

function Lizard_compressBound(inputSize: integer): integer; cdecl; external;

function Lizard_compress(src, dst: pointer; srcSize, maxDstSize,
  compressionLevel: integer): integer; cdecl; external;

function Lizard_sizeofState(compressionLevel: integer): integer; cdecl; external;

function Lizard_compress_extState(state: pointer; src, dst: pointer;
  srcSize, maxDstSize, compressionLevel: integer): integer; cdecl; external;

{
function Lizard_createStream(compressionLevel: integer): pointer; cdecl; external;

function Lizard_freeStream(streamPtr: pointer): integer; cdecl; external;

function Lizard_resetStream(streamPtr: pointer;
  compressionLevel: integer): pointer; cdecl; external;

function Lizard_loadDict(streamPtr, dictionary: pointer;
  dictSize: integer): integer; cdecl; external;

function Lizard_compress_continue(streamPtr: pointer; src, dst: pointer;
  srcSize, maxDstSize: integer): integer; cdecl; external;

function Lizard_saveDict(streamPtr, safeBuffer: pointer;
  dictSize: integer): integer; cdecl; external;
}

function Lizard_decompress_safe(src, dst: pointer;
  srcSize, maxDstSize: integer): integer; cdecl; external;

function Lizard_decompress_safe_partial(src, dst: pointer;
  srcSize, targetDstSize, maxDstSize: integer): integer; cdecl; external;

{
function Lizard_createStreamDecode: pointer; cdecl; external;

function Lizard_freeStreamDecode(streamDec: pointer): integer; cdecl; external;

function Lizard_setStreamDecode(streamDec: pointer;
  dict: pointer; dictSize: integer): integer; cdecl; external;

function Lizard_decompress_safe_continue(streamDec, src, dst: pointer;
  srcSize, maxDstSize: integer): integer; cdecl; external;

function Lizard_decompress_safe_usingDict(src, dst: pointer;
  srcSize, maxDstSize: integer;
  dict: pointer; dictSize: integer): integer; cdecl; external;
}

// see res/static/liblizard for patched source and build instructions

{$ifdef CPUX64}
  {$ifdef FPC}
    {$ifdef OSWINDOWS}
    // note: gcc .o files don't work under Win64 for Delphi :(
    {$linklib ..\..\static\x86_64-win64\liblizard.a}
    {$else}
    {$linklib ../../static/x86_64-linux/liblizard.a}
    {$endif OSWINDOWS}
  {$endif FPC}
{$endif CPUX64}

{$ifdef CPUX86}
  {$ifdef FPC}
    {$ifdef OSWINDOWS}
    {$linklib ..\..\static\i386-win32\liblizard.a}
    {$else}
    {$linklib ../../static/i386-linux/liblizard.a}
    {$endif OSWINDOWS}
  {$endif FPC}
{$endif CPUX86}

{$ifdef CPUARM}
  {$ifdef FPC}
    {$linklib ../../static/arm-linux/liblizard.a}
  {$endif FPC}
{$endif CPUARM}

{$ifdef CPUAARCH64}
  {$ifdef FPC}
    {$linklib ../../static/aarch64-linux/liblizard.a}
  {$endif FPC}
{$endif CPUAARCH64}

{ TSynLizardStatic }

type
  TSynLizardStatic = class(TSynLizard)
  public
    constructor Create; override;
  end;

constructor TSynLizardStatic.Create;
begin
  versionNumber := Lizard_versionNumber;
  compressbound := Lizard_compressbound;
  compress := Lizard_compress;
  sizeofState := Lizard_sizeofState;
  compress_extState := Lizard_compress_extState;
  decompress_safe := Lizard_decompress_safe;
  decompress_safe_partial := Lizard_decompress_safe_partial;
  inherited Create; // register AlgoLizard/AlgoLizardFast/AlgoLizardHuff
end;

{$endif LIZARD_EXTERNALONLY}



{ TSynLizardDynamic }

const
  LIZARD_ENTRIES: array[0..6] of RawUtf8 = (
   'versionNumber',
   'compressBound',
   'compress',
   'sizeofState',
   'compress_extState',
   'decompress_safe',
   'decompress_safe_partial');

constructor TSynLizardDynamic.Create(const aLibraryFile: TFileName;
  aRaiseNoException: boolean);
var
  P: PPointer;
  i: PtrInt;
begin
  fLibrary := TSynLibrary.Create;
  if fLibrary.TryLoadLibrary(
    [aLibraryFile, LIZARD_LIB_NAME], nil) then
  begin
    P := @@versionNumber;
    for i := 0 to High(LIZARD_ENTRIES) do
      if fLibrary.Resolve('Lizard_', LIZARD_ENTRIES[i], P, EAlgoCompress) then
        inc(P);
    if versionNumber div 10000 <> 1 then
      if aRaiseNoException then
        exit
      else
        raise EAlgoCompress.CreateUtf8('% has unexpected versionNumber=%',
          [fLibrary.LibraryPath, versionNumber]);
    // register TAlgoLizard/TAlgoLizardFast/TAlgoLizardHuffman
    inherited Create;
    // if we reached here, the external library has been properly setup
    fLoaded := true;
  end
  else if not aRaiseNoException then
    raise EAlgoCompress.CreateUtf8('Unable to load % - %/'#13#10 +
      'Please download from https://synopse.info/files/SynLizardLibs.7z',
      [aLibraryFile, GetErrorText(GetLastError)]);
end;

destructor TSynLizardDynamic.Destroy;
begin
  fLibrary.Free;
  inherited;
end;

class function TSynLizardDynamic.AlgoRegister: boolean;
var
  lib: TSynLizardDynamic;
begin
  result := Lizard <> nil;
  if result then
    // already registered (maybe as TSynLizardStatic)
    exit;
  lib := TSynLizardDynamic.Create('', true);
  result := lib.Loaded;
  if result then
    Lizard := lib
  else
    lib.Free;
end;

function TSynLizardDynamic.LibraryName: TFileName;
begin
  result := fLibrary.LibraryPath;
end;


{ ****************** TAlgoLizard/TAlgoLizardFast/TAlgoLizardHuffman High-Level Algorithms }

type
  TAlgoLizard = class(TAlgoCompressWithNoDestLen)
  protected
    fCompressionLevel: integer;
    function RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer; override;
  public
    constructor Create; override;
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
  end;

  TAlgoLizardFast = class(TAlgoLizard)
  public
    constructor Create; override;
  end;

  TAlgoLizardHuffman = class(TAlgoLizard)
  public
    constructor Create; override;
  end;


{ TAlgoLizard }

constructor TAlgoLizard.Create;
begin
  if fAlgoID = 0 then
    fAlgoID := 4;
  inherited Create;
  fCompressionLevel := LIZARD_DEFAULT_CLEVEL;
end;

function TAlgoLizard.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  if Lizard = nil then
    result := 0
  else
    result := Lizard.compressBound(PlainLen);
end;

function TAlgoLizard.RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
  process: TAlgoCompressWithNoDestLenProcess): integer;
begin
  if Lizard = nil then
    result := 0
  else
  case process of
    doCompress:
      result := Lizard.compress(src, dst, srcLen, dstLen, fCompressionLevel);
    doUnCompress:
      result := Lizard.decompress_safe(src, dst, srcLen, dstLen);
    doUncompressPartial:
      result := Lizard.decompress_safe_partial(src, dst, srcLen, dstLen, dstMax);
  else
    result := 0;
  end;
end;


{ TAlgoLizardFast }

constructor TAlgoLizardFast.Create;
begin
  fAlgoID := 5;
  inherited Create;
  fCompressionLevel := LIZARD_MIN_CLEVEL;
end;


{ TAlgoLizardHuffman }

constructor TAlgoLizardHuffman.Create;
begin
  fAlgoID := 6;
  inherited Create;
  fCompressionLevel := LIZARD_HUFFMAN_CLEVEL;
end;


// this constructor uses TAlgoLizard* classes so is defined hereafter

{ TSynLizard }

constructor TSynLizard.Create;
begin
  if AlgoLizard = nil then
    AlgoLizard := TAlgoLizard.Create;
  if AlgoLizardFast = nil then
    AlgoLizardFast := TAlgoLizardFast.Create;
  if AlgoLizardHuffman = nil then
    AlgoLizardHuffman := TAlgoLizardHuffman.Create;
end;


{ ****************** TAlgoLizard/TAlgoLizardFast/TAlgoLizardHuffman High-Level Algorithms }

function EventArchiveLizard(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  result := AlgoLizardFast.EventArchive(
    LOG_MAGIC, aOldLogFileName, aDestinationPath, '.synlz');
end;


initialization
  {$ifndef LIZARD_EXTERNALONLY}
  Lizard := TSynLizardStatic.Create;
  {$endif LIZARD_EXTERNALONLY}

finalization
  Lizard.Free;

end.

