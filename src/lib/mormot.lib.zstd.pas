/// low-level access to the Zstandard/zstd API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.zstd;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler Zstandard (zstd) API
   - Low-Level Zstandard API Process
   - TAlgoZstd High-Level Algorithms

  *****************************************************************************

  WARNING: unfinished and untested unit - FPC-only, from external contributor
  Some numbers: Todo

}

interface

{$I ..\mormot.defines.inc}


{$define ZSTD_EXTERNALONLY}
// will force to use an external libzstd.dll

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,      // for TSynLibrary
  mormot.core.buffers; // for TAlgoCompress


  { ****************** Low-Level Zstandard Process }

type
  TZSTD_CCtx = type pointer;
  TZSTD_DCtx = type pointer;

const
  ZSTD_CLEVEL_DEFAULT = 3;

  /// compression levels
  ZSTD_fast = 1;
  ZSTD_dfast = 2;
  ZSTD_greedy = 3;
  ZSTD_lazy = 4;
  ZSTD_lazy2 = 5;
  ZSTD_btlazy2 = 6;
  ZSTD_btopt = 7;
  ZSTD_btultra = 8;
  ZSTD_btultra2 = 9;

  /// parameters
  ZSTD_c_compressionLevel = 100;
  ZSTD_c_windowLog = 101;
  ZSTD_c_hashLog = 102;
  ZSTD_c_chainLog = 103;
  ZSTD_c_searchLog = 104;
  ZSTD_c_minMatch = 105;
  ZSTD_c_targetLength = 106;
  ZSTD_c_strategy = 107;

  // getFrameContentSize errors
  ZSTD_CONTENTSIZE_UNKNOWN = UInt64(-1);
  ZSTD_CONTENTSIZE_ERROR = UInt64(-2);

  /// default TSynZstdDynamic file name
  // - mainly for Delphi, since FPC will use static linked .o files under
  // Windows and Linux Intel 32/64 bits
  // - to be downloaded from from https://synopse.info/files/XXX  //Todo: Set to correct value
  {$ifdef Win32}
  ZSTD_LIB_NAME = ''; //Todo: Set to correct value
  {$endif Win32}
  {$ifdef Win64}
  ZSTD_LIB_NAME = 'libzstd.dll';
  {$endif Win64}
  {$ifdef OSPOSIX}
  ZSTD_LIB_NAME = ''; //Todo: Set to correct value
  {$endif OSPOSIX}

type
  /// zstd lossless compression algorithm
  // - provides fast real-time compression algorithm
  // - this class implements direct low-level access to the zstd API - consider
  // using AlgoZSTD global instances for easier use
  TSynZstd = class
  public
    //// will initialize the library
    constructor Create; virtual;
  public
    /// version number of the linked zstd library
    versionNumber: function: cardinal; cdecl;
    /// version string of the linked zstd library
    versionString: function: PAnsiChar; cdecl;
    /// tells if a size_t function result is an error code
    isError: function(code: PtrUInt): cardinal; cdecl;
    /// maximum size that zstd compression may output in a "worst case" scenario
    compressBound: function(srcSize: PtrUInt): PtrUInt; cdecl;
    /// create a new instance of a compression context
    // re-use it for each successive compression operation
    createCCtx: function: TZSTD_CCtx; cdecl;
    /// free an instance of a compression context
    freeCCtx: function(cctx: TZSTD_CCtx): PtrUInt; cdecl;
    /// set one compression parameter
    // - providing a value beyond bound will either clamp it, or trigger an
    // error (depending on the actual parameter)
    CCtx_setParameter: function(cctx: TZSTD_CCtx; param, value: integer): PtrUInt; cdecl;
    /// get the requested compression parameter value
    CCtx_getParameter: function(cctx: TZSTD_CCtx; param: integer;
      out value: integer): PtrUInt; cdecl;
    /// compresses src content as a single zstd compressed frame into already allocated dst
    compress2: function(cctx: TZSTD_CCtx; dst: pointer; dstCapacity: PtrUInt;
      src: pointer; srcSize: PtrUInt): PtrUInt; cdecl;
    /// tells if the content of buffer starts with a valid Frame Identifier
    isFrame: function(buffer: pointer; size: PtrUInt): cardinal; cdecl;
    /// decompressed size of src frame content, if known
    // - returns ZSTD_CONTENTSIZE_UNKNOWN if the size cannot be determined
    // - returns ZSTD_CONTENTSIZE_ERROR if an error occurred (e.g. invalid magic
    // number, srcSize too small)
    getFrameContentSize: function(src: pointer; srcSize: PtrUInt): UInt64; cdecl;
    /// create a new instance of a decompression context
    // - re-use it for each successive decompression operation
    createDCtx: function: TZSTD_DCtx; cdecl;
    /// free an instance of a decompression context
    freeDCtx: function(dctx: TZSTD_DCtx): PtrUInt; cdecl;
    // compresses src content as a single zstd compressed frame into already allocated dst
    decompressDCtx: function(dctx: TZSTD_DCtx; dst: pointer; dstCapacity: PtrUInt;
      src: pointer; srcSize: PtrUInt): PtrUInt; cdecl;
  end;


type
  /// try to load zstd as an external library
  TSynZstdDynamic = class(TSynZstd)
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
    /// ensure zstd compression is available
    // - returns TRUE if zstd compression is available
    // - if there is a local libzstd.dll file, try to load it
    class function AlgoRegister: boolean;
    /// set to TRUE if Create successed
    // - may be used if aRaiseNoException parameter has been defined
    property Loaded: boolean read fLoaded;
    /// the loaded library file name
    function LibraryName: TFileName;
  end;


var
  /// direct access to the low-level zstd library API
  // - is defined by default if zstd was statically linked
  // - otherwise, you should execute explicitly:
  // ! if Zstd = nil then
  // !   Zstd := TSynZstdDynamic.Create;
  Zstd: TSynZstd;


  { ****************** TAlgoZstd High-Level Algorithms }

var
  /// implement Zstandard compression in level 3 (ZSTD_CLEVEL_DEFAULT)
  // - is set by TSynZstd.Create, so available e.g. if library is statically
  // linked, or once TSynZstdDynamic.Create has been successfully called
  AlgoZstd: TAlgoCompress;

implementation

{
    Zstandard is dual-licensed under BSD OR GPLv2.
}

{ ****************** Low-Level Zstandard Process }

{$ifndef ZSTD_EXTERNALONLY}

function ZSTD_versionNumber: cardinal; cdecl; external;
function ZSTD_versionString: PAnsiChar; cdecl; external;
function ZSTD_isError(code: PtrUInt): cardinal; cdecl; external;
function ZSTD_compressBound(srcSize: PtrUInt): PtrUInt; cdecl; external;
function ZSTD_createCCtx: TZSTD_CCtx; cdecl; external;
function ZSTD_freeCCtx(cctx: TZSTD_CCtx): PtrUInt; cdecl; external;
function ZSTD_CCtx_setParameter(cctx: TZSTD_CCtx; param: integer; value: integer): PtrUInt; cdecl; external;
function ZSTD_CCtx_getParameter(cctx: TZSTD_CCtx; param: integer; out value: integer): PtrUInt; cdecl; external;
function ZSTD_compress2(cctx: TZSTD_CCtx; dst: pointer; dstCapacity: PtrUInt; src: pointer; srcSize: PtrUInt): PtrUInt; cdecl; external;
function ZSTD_isFrame(buffer: pointer; size: PtrUInt): cardinal; cdecl; external;
function ZSTD_getFrameContentSize(src: pointer; srcSize: PtrUInt): UInt64; cdecl; external;
function ZSTD_createDCtx: TZSTD_DCtx; cdecl; external;
function ZSTD_freeDCtx(dctx: TZSTD_DCtx): PtrUInt; cdecl; external;
function ZSTD_decompressDCtx(dctx: TZSTD_DCtx; dst: pointer; dstCapacity: PtrUInt; src: pointer; srcSize: PtrUInt): PtrUInt; cdecl; external;

//Todo: Compile static

{ TSynZstdStatic }

type
  TSynZstdStatic = class(TSynZstd)
  public
    constructor Create; override;
  end;

constructor TSynZstdStatic.Create;
begin
  versionNumber := ZSTD_versionNumber;
  versionString := ZSTD_versionString;
  isError := ZSTD_isError;
  compressBound := ZSTD_compressBound;
  createCCtx := ZSTD_createCCtx;
  freeCCtx := ZSTD_freeCCtx;
  CCtx_setParameter := ZSTD_CCtx_setParameter;
  CCtx_getParameter := ZSTD_CCtx_getParameter;
  compress2 := ZSTD_compress2;
  isFrame := ZSTD_isFrame;
  getFrameContentSize := ZSTD_getFrameContentSize;
  createDCtx := ZSTD_createDCtx;
  freeDCtx := ZSTD_freeDCtx;
  decompressDCtx := ZSTD_decompressDCtx;
  inherited Create; // register AlgoZstd
end;

{$endif ZSTD_EXTERNALONLY}

{ TSynZstdDynamic }

const
  ZSTD_ENTRIES: array[0 .. 14] of PAnsiChar = (
    'versionNumber',
    'versionString',
    'isError',
    'compressBound',
    'createCCtx',
    'freeCCtx',
    'CCtx_setParameter',
    'CCtx_getParameter',
    'compress2',
    'isFrame',
    'getFrameContentSize',
    'createDCtx',
    'freeDCtx',
    'decompressDCtx',
    nil);

constructor TSynZstdDynamic.Create(const aLibraryFile: TFileName;
  aRaiseNoException: boolean);
begin
  fLibrary := TSynLibrary.Create;
  if fLibrary.TryLoadLibrary([aLibraryFile, ZSTD_LIB_NAME], nil) then
  begin
    fLibrary.ResolveAll(@ZSTD_ENTRIES, @@versionNumber, 'ZSTD_', EAlgoCompress);
    if versionNumber div 10000 <> 1 then
      if aRaiseNoException then
        exit
      else
        EAlgoCompress.RaiseUtf8('%.Create: % has unexpected versionNumber=%',
          [self, fLibrary.LibraryPath, versionNumber]);
    // register TAlgoZstd
    inherited Create;
    // if we reached here, the external library has been properly setup
    fLoaded := true;
  end
  else if not aRaiseNoException then
    EAlgoCompress.RaiseUtf8('%.Create: Unable to load % - %/'#13#10 +
      'Please download from https://synopse.info/files/XXX', //Todo: Set to correct value
      [self, aLibraryFile, GetErrorText(GetLastError)]);
end;

destructor TSynZstdDynamic.Destroy;
begin
  fLibrary.Free;
  inherited;
end;

class function TSynZstdDynamic.AlgoRegister: boolean;
var
  lib: TSynZstdDynamic;
begin
  result := Zstd <> nil;
  if result then
    // already registered (maybe as TSynZstdStatic)
    exit;
  lib := TSynZstdDynamic.Create('', true);
  result := lib.Loaded;
  if result then
    Zstd := lib
  else
    lib.Free;
end;

function TSynZstdDynamic.LibraryName: TFileName;
begin
  result := fLibrary.LibraryPath;
end;


{ ****************** TAlgoZstd High-Level Algorithms }

type
  /// implement the AlgoZstd global variable
  // - since .gz is a file-level algorithm, this class won't use the regular
  // TAlgoCompress file layout and only support raw buffer and file methods
  TAlgoZstd = class(TAlgoCompress)
  protected
    fCompressionLevel: integer;
    fCompressionContext: TZSTD_CCtx;
    fCompressionContextSafe: TLightLock;
    fDecompressionContext: TZSTD_DCtx;
    fDecompressionContextSafe: TLightLock;
  public
    /// set AlgoID = 11 as genuine byte identifier for zstd (even if not used)
    constructor Create; override;
    destructor Destroy; override;
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
    function AlgoCompress(Plain: pointer; PlainLen: integer;
      Comp: pointer): integer; override;
    function AlgoDecompressDestLen(Comp: pointer): integer; override;
    function AlgoDecompress(Comp: pointer; CompLen: integer;
      Plain: pointer): integer; override;
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; override;
  end;

  { TAlgoZstd }

constructor TAlgoZstd.Create;
begin
  if fAlgoID = 0 then
    fAlgoID := 11; //Todo: Check and note in TAlgoCompress
  fAlgoFileExt := '.zst';
  inherited Create;
  fCompressionLevel := ZSTD_CLEVEL_DEFAULT;
end;

destructor TAlgoZstd.Destroy;
begin
  if fCompressionContext <> nil then
    Zstd.freeCCtx(fCompressionContext);
  if fDecompressionContext <> nil then
    Zstd.freeDCtx(fDecompressionContext);
  inherited Destroy;
end;

function TAlgoZstd.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  if Zstd = nil then
    result := 0
  else
    result := Zstd.compressBound(PlainLen);
end;

function TAlgoZstd.AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer;
begin
  if Zstd = nil then
    result := 0
  else
  begin
    fCompressionContextSafe.Lock;
    try
      if fCompressionContext = nil then
        fCompressionContext := Zstd.createCCtx;
      Zstd.CCtx_setParameter(fCompressionContext,
        ZSTD_c_compressionLevel, fCompressionLevel);
      result := zstd.compress2(fCompressionContext,
        Comp, Zstd.compressBound(PlainLen) {Todo}, Plain, PlainLen);
    finally
      fCompressionContextSafe.UnLock;
    end;
  end;
end;

function TAlgoZstd.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  if Zstd = nil then
    result := 0
  else
    result := Zstd.getFrameContentSize(Comp, MemSize(Comp) {Todo});
end;

function TAlgoZstd.AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer;
begin
  if Zstd = nil then
    result := 0
  else
  begin
    fDecompressionContextSafe.Lock;
    try
      if fDecompressionContext = nil then
        fDecompressionContext := Zstd.createDCtx;
      result := zstd.decompressDCtx(fDecompressionContext, Plain,
        Zstd.getFrameContentSize(Comp, CompLen) {Todo}, Comp, CompLen);
    finally
      fDecompressionContextSafe.UnLock;
    end;
  end;
end;

function TAlgoZstd.AlgoDecompressPartial(Comp: pointer; CompLen: integer;
  Partial: pointer; PartialLen, PartialLenMax: integer): integer;
begin
  result := 0; // not supported by our zstd wrapper
end;


// this constructor uses TAlgoZstd* classes so is defined hereafter

{ TSynZstd }

constructor TSynZstd.Create;
begin
  if AlgoZstd = nil then
    AlgoZstd := TAlgoZstd.Create;
end;

initialization
  {$ifndef ZSTD_EXTERNALONLY}
  Zstd := TSynZstdStatic.Create;
  {$endif ZSTD_EXTERNALONLY}

finalization
  Zstd.Free;

end.
