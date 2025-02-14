/// low-level access to the Zstandard/ZSTD API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.zstd;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler Zstandard (ZSTD) API
   - Low-Level Zstandard API Process
   - TAlgoZSTD High-Level Algorithms

  *****************************************************************************


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


  { ****************** Low-Level ZSTD Process }

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

  /// default TSynZSTDDynamic file name
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
  /// ZSTD lossless compression algorithm
  // - provides fast real-time compression algorithm
  // - this class implements direct low-level access to the ZSTD API - consider
  // using AlgoZSTD global instances for easier use
  TSynZSTD = class
  public
    //// will initialize the library
    constructor Create; virtual;
  public
    /// version number of the linked ZSTD library
    versionNumber: function: cardinal; cdecl;
    /// version string of the linked ZSTD library
    versionString: function: PAnsiChar; cdecl;
    /// tells if a `size_t` function result is an error code
    isError: function(code: size_t): cardinal; cdecl;
    /// maximum size that ZSTD compression may output in a "worst case" scenario
    compressBound: function(srcSize: size_t): size_t; cdecl;
    /// create a new instance of a compression context
    // re-use it for each successive compression operation
    createCCtx: function: TZSTD_CCtx; cdecl;
    /// free an instance of a compression context
    freeCCtx: function(cctx: TZSTD_CCtx): size_t; cdecl;
    /// set one compression parameter
    // providing a value beyond bound will either clamp it, or trigger an error (depending on parameter).
    CCtx_setParameter: function(cctx: TZSTD_CCtx; param: integer; value: integer): size_t; cdecl;
    /// get the requested compression parameter value
    CCtx_getParameter: function(cctx: TZSTD_CCtx; param: integer; out value: integer): size_t; cdecl;
    /// compresses `src` content as a single zstd compressed frame into already allocated `dst`
    compress2: function(cctx: TZSTD_CCtx; dst: pointer; dstCapacity: size_t; src: pointer; srcSize: size_t): size_t; cdecl;
    /// tells if the content of `buffer` starts with a valid Frame Identifier
    isFrame: function(buffer: pointer; size: size_t): cardinal; cdecl;
    /// decompressed size of `src` frame content, if known
    // returns ZSTD_CONTENTSIZE_UNKNOWN if the size cannot be determined
    // returnsZSTD_CONTENTSIZE_ERROR if an error occurred (e.g. invalid magic number, srcSize too small)
    getFrameContentSize: function(src: pointer; srcSize: size_t): UInt64; cdecl;
    /// create a new instance of a decompression context
    // re-use it for each successive decompression operation
    createDCtx: function: TZSTD_DCtx; cdecl;
    // free an instance of a decompression context
    freeDCtx: function(dctx: TZSTD_DCtx): size_t; cdecl;
    // compresses `src` content as a single zstd compressed frame into already allocated `dst`.
    decompressDCtx: function(dctx: TZSTD_DCtx; dst: pointer; dstCapacity: size_t; src: pointer;
      srcSize: size_t): size_t; cdecl;
  end;


type
  /// try to load ZSTD as an external library
  TSynZSTDDynamic = class(TSynZSTD)
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
    /// ensure ZSTD compression is available
    // - returns TRUE if ZSTD compression is available
    // - if there is a local libzstd.dll file, try to load it
    class function AlgoRegister: boolean;
    /// set to TRUE if Create successed
    // - may be used if aRaiseNoException parameter has been defined
    property Loaded: boolean read fLoaded;
    /// the loaded library file name
    function LibraryName: TFileName;
  end;


var
  /// direct access to the low-level ZSTD library API
  // - is defined by default if ZSTD was statically linked
  // - otherwise, you should execute explicitly:
  // ! if ZSTD = nil then
  // !   ZSTD := TSynZSTDDynamic.Create;
  ZSTD: TSynZSTD;


  { ****************** TAlgoZSTD High-Level Algorithms }

var
  /// implement ZSTD compression in level 3 (ZSTD_CLEVEL_DEFAULT)
  // - is set by TSynZSTD.Create, so available e.g. if library is statically
  // linked, or once TSynZSTDDynamic.Create has been successfully called
  AlgoZSTD: TAlgoCompress;

implementation

{
    Zstandard is dual-licensed under BSD OR GPLv2.
}

{ ****************** Low-Level ZSTD Process }

{$ifndef ZSTD_EXTERNALONLY}

function ZSTD_versionNumber: cardinal; cdecl; external;
function ZSTD_versionString: PAnsiChar; cdecl; external;
function ZSTD_isError(code: size_t): cardinal; cdecl; external;
function ZSTD_compressBound(srcSize: size_t): size_t; cdecl; external;
function ZSTD_createCCtx: TZSTD_CCtx; cdecl; external;
function ZSTD_freeCCtx(cctx: TZSTD_CCtx): size_t; cdecl; external;
function ZSTD_CCtx_setParameter(cctx: TZSTD_CCtx; param: integer; value: integer): size_t; cdecl; external;
function ZSTD_CCtx_getParameter(cctx: TZSTD_CCtx; param: integer; out value: integer): size_t; cdecl; external;
function ZSTD_compress2(cctx: TZSTD_CCtx; dst: pointer; dstCapacity: size_t; src: pointer; srcSize: size_t): size_t; cdecl; external;
function ZSTD_isFrame(buffer: pointer; size: size_t): cardinal; cdecl; external;
function ZSTD_getFrameContentSize(src: pointer; srcSize: size_t): UInt64; cdecl; external;
function ZSTD_createDCtx: TZSTD_DCtx; cdecl; external;
function ZSTD_freeDCtx(dctx: TZSTD_DCtx): size_t; cdecl; external;
function ZSTD_decompressDCtx(dctx: TZSTD_DCtx; dst: pointer; dstCapacity: size_t; src: pointer; srcSize: size_t): size_t; cdecl; external;

//Todo: Compile static

{ TSynZSTDStatic }

type
  TSynZSTDStatic = class(TSynZSTD)
  public
    constructor Create; override;
  end;

constructor TSynZSTDStatic.Create;
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
  inherited Create; // register AlgoZSTD
end;

{$endif ZSTD_EXTERNALONLY}

{ TSynZSTDDynamic }

const
  ZSTD_ENTRIES: array[0..13] of RawUtf8 = (
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
    'decompressDCtx');

constructor TSynZSTDDynamic.Create(const aLibraryFile: TFileName;
  aRaiseNoException: boolean);
var
  P: PPointer;
  i: PtrInt;
begin
  fLibrary := TSynLibrary.Create;
  if fLibrary.TryLoadLibrary(
    [aLibraryFile, ZSTD_LIB_NAME], nil) then
  begin
    P := @@versionNumber;
    for i := 0 to High(ZSTD_ENTRIES) do
      if fLibrary.Resolve('ZSTD_', ZSTD_ENTRIES[i], P, EAlgoCompress) then
        inc(P);
    if versionNumber div 10000 <> 1 then
      if aRaiseNoException then
        exit
      else
        EAlgoCompress.RaiseUtf8('% has unexpected versionNumber=%',
          [fLibrary.LibraryPath, versionNumber]);
    // register TAlgoZSTD
    inherited Create;
    // if we reached here, the external library has been properly setup
    fLoaded := true;
  end
  else if not aRaiseNoException then
    EAlgoCompress.RaiseUtf8('Unable to load % - %/'#13#10 +
      'Please download from https://synopse.info/files/XXX', //Todo: Set to correct value
      [aLibraryFile, GetErrorText(GetLastError)]);
end;

destructor TSynZSTDDynamic.Destroy;
begin
  fLibrary.Free;
  inherited;
end;

class function TSynZSTDDynamic.AlgoRegister: boolean;
var
  lib: TSynZSTDDynamic;
begin
  result := ZSTD <> nil;
  if result then
    // already registered (maybe as TSynZSTDStatic)
    exit;
  lib := TSynZSTDDynamic.Create('', true);
  result := lib.Loaded;
  if result then
    ZSTD := lib
  else
    lib.Free;
end;

function TSynZSTDDynamic.LibraryName: TFileName;
begin
  result := fLibrary.LibraryPath;
end;


{ ****************** TAlgoZSTD High-Level Algorithms }

type
  TAlgoZSTD = class(TAlgoCompress)
  protected
    fCompressionLevel: integer;
    fCompressionContext: TZSTD_CCtx;
    fCompressionContextSafe: TLightLock;
    fDecompressionContext: TZSTD_DCtx;
    fDecompressionContextSafe: TLightLock;
  public
    /// set AlgoID = 11 as genuine byte identifier for ZSTD (even if not used)
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

  { TAlgoZSTD }

constructor TAlgoZSTD.Create;
begin
  if fAlgoID = 0 then
    fAlgoID := 11; //Todo: Check and note in TAlgoCompress
  fAlgoFileExt := '.zst';
  inherited Create;
  fCompressionLevel := ZSTD_CLEVEL_DEFAULT;
end;

destructor TAlgoZSTD.Destroy;
begin
  if fCompressionContext <> nil then
    ZSTD.freeCCtx(fCompressionContext);
  if fDecompressionContext <> nil then
    ZSTD.freeDCtx(fDecompressionContext);
  inherited Destroy;
end;

function TAlgoZSTD.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  if ZSTD = nil then
    result := 0
  else
    result := ZSTD.compressBound(PlainLen);
end;

function TAlgoZSTD.AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer;
begin
  if ZSTD = nil then
    result := 0
  else
  begin
    fCompressionContextSafe.Lock;
    try
      if fCompressionContext = nil then
        fCompressionContext := ZSTD.createCCtx;
      ZSTD.CCtx_setParameter(fCompressionContext, ZSTD_c_compressionLevel, fCompressionLevel);
      result := zstd.compress2(fCompressionContext, Comp, ZSTD.compressBound(PlainLen) {Todo}, Plain, PlainLen);
    finally
      fCompressionContextSafe.UnLock;
    end;
  end;
end;

function TAlgoZSTD.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  if ZSTD = nil then
    result := 0
  else
    result := ZSTD.getFrameContentSize(Comp, MemSize(Comp) {Todo});
end;

function TAlgoZSTD.AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer;
begin
  if ZSTD = nil then
    result := 0
  else
  begin
    fDecompressionContextSafe.Lock;
    try
      if fDecompressionContext = nil then
        fDecompressionContext := ZSTD.createDCtx;
      result := zstd.decompressDCtx(fDecompressionContext, Plain, ZSTD.getFrameContentSize(Comp, CompLen)
        {Todo}, Comp, CompLen);
    finally
      fDecompressionContextSafe.UnLock;
    end;
  end;
end;

function TAlgoZSTD.AlgoDecompressPartial(Comp: pointer; CompLen: integer; Partial: pointer; PartialLen,
  PartialLenMax: integer): integer;
begin
  result := 0; //Not supported by ZSTD
end;

// this constructor uses TAlgoZSTD* classes so is defined hereafter

{ TSynZSTD }

constructor TSynZSTD.Create;
begin
  if AlgoZSTD = nil then
    AlgoZSTD := TAlgoZSTD.Create;
end;

initialization
  {$ifndef ZSTD_EXTERNALONLY}
  ZSTD := TSynZSTDStatic.Create;
  {$endif ZSTD_EXTERNALONLY}

finalization
  ZSTD.Free;

end.
