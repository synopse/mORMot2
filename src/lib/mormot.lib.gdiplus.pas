/// low-level access to the GDI+ API for Win32/Win64
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.gdiplus;


{
  *****************************************************************************

   Windows GDI+ Graphics Device Interface Support
   - GDI+ Shared Types
   - GDI+ TImageAttributes wrapper
   - TGdiPlus class for Direct Access to the GDI+ Library
   - AntiAliased Rendering of GDI MetaFile

   See mormot.ui.gdiplus.pas for high-level LCL/VCL pictures support.

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

uses
  Windows,
  ActiveX,
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os;


{.$define GDIPLUS_USEENCODERS}
// if defined, the GDI+ encoder list will be used - seems not necessary
// - should be defined here and in mormot.ui.gdiplus (i.e. Projects options)

{.$define GDIPLUS_USEDPI}
// if defined, the DrawAt() method is available, which respect dpi on drawing
// - should not be useful on most applications
// - should be defined here and in mormot.ui.gdiplus (i.e. Projects options)


{ ****************** GDI+ Shared Types }

{$MINENUMSIZE 4}

type
  /// exception class raised during GDI+ process
  EGdiPlus = class(ExceptionWithProps);

  /// GDI+ line drawing smoothing types
  TSmoothingMode = (
    smDefault,
    smHighSpeed,
    smHighQuality,
    smNone,
    smAntiAlias
  );

  /// GDI+ text rendering smoothing types
  TTextRenderingHint = (
    trhDefault,
    trhSingleBitPerPixelGridFit,
    trhSingleBitPerPixel,
    trhAntiAliasGridFit,
    trhAntiAlias,
    trhClearTypeGridFit
  );

  /// GDI+ available coordinates units
  TUnit = (
    uWorld,
    uDisplay,
    uPixel,
    uPoint,
    uInch,
    uDocument,
    uMillimeter,
    uGdi
  );

  /// GDI+ types of conversion from EMF to EMF+
  TEmfType = (
   etEmf0, etEmf1, etEmf2, { unused }
   etEmfOnly,
   etEmfPlusOnly,
   etEmfPlusDual
  );

  /// GDI+ available filling modes
  TFillMode = (
    fmAlternate,
    fmWinding
  );

  /// GDI+ lock mode for GdipFull.BitmapLockBits
  TLockModeOption = (
    lmRead,
    lmWrite,
    lmUserInputBuf
  );

  /// GDI+ lock mode settings for GdipFull.BitmapLockBits
  TLockModeOptions = set of TLockModeOption;

  /// GDI+ error codes
  TGdipStatus = (
    stOk,
    stGenericError,
    stInvalidParameter,
    stOutOfMemory,
    stObjectBusy,
    stInsufficientBuffer,
    stNotImplemented,
    stWin32Error,
    stWrongState,
    stAborted,
    stFileNotFound,
    stValueOverflow,
    stAccessDenied,
    stUnknownImageFormat,
    stFontFamilyNotFound,
    stFontStyleNotFound,
    stNotTrueTypeFont,
    stUnsupportedGdiplusVersion,
    stGdiplusNotInitialized,
    stPropertyNotFound,
    stPropertyNotSupported);

  /// GDI+ integer coordinates rectangles
  // - use width and height instead of right and bottom
  TGdipRect = record
    X, Y, Width, Height: integer;
  end;
  PGdipRect = ^TGdipRect;

  /// GDI+ floating point coordinates rectangles
  // - use width and height instead of right and bottom
  TGdipRectF = record
    X, Y, Width, Height: Single;
  end;
  PGdipRectF = ^TGdipRectF;

  /// GDI+ floating point coordinates for a point
  TGdipPointF = record
    X, Y: Single;
  end;
  PGdipPointF = ^TGdipPointF;
  PGdipPointFArray = ^TGdipPointFArray;

  /// GDI+ floating point coordinates for an array of points
  TGdipPointFArray = array[0..1000] of TGdipPointF;

  /// data as retrieved by GdipFull.BitmapLockBits
  TGdipBitmapData = record
    Width: cardinal;
    Height: cardinal;
    Stride: integer;
    PixelFormat: integer;
    Scan0: pointer;
    Reserved: cardinal;
  end;
  PGdipBitmapData = ^TGdipBitmapData;

  TGpipImageAttributes = pointer;

  TColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny
  );

  TColorMatrix = packed array[0..4, 0..4] of Single;
  PColorMatrix = ^TColorMatrix;

  TColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );

  TColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );

  TColorMap = packed record
    oldColor: cardinal;
    newColor: cardinal;
  end;
  PColorMap = ^TColorMap;

  TWrapMode = (
    WrapModeTile,
    WrapModeTileFlipX,
    WrapModeTileFlipY,
    WrapModeTileFlipXY,
    WrapModeClamp
  );

  TColorPalette = packed record
    Flags: UINT;
    Count: UINT;
    Entries: array [0..0] of cardinal;
  end;
  PColorPalette = ^TColorPalette;

  // Region combine mode (used by SetClipRegion, etc.)
  TGdipCombineMode = (
    cmReplace,
    cmIntersect,
    cmUnion,
    cmXor,
    cmExclude,
    cmComplement
  );

  /// allowed types for image saving
  TGdipPictureType = (
    gptGIF,
    gptPNG,
    gptJPG,
    gptBMP,
    gptTIF
  );

  /// the optional TIFF compression levels
  // - use e.g. ord(evCompressionCCITT4) to save a TIFF picture as CCITT4
  TGdipPEncoderValue = (
    evColorTypeCMYK,
    evColorTypeYCCK,
    evCompressionLZW,
    evCompressionCCITT3,
    evCompressionCCITT4,
    evCompressionRle,
    evCompressionNone,
    evScanMethodInterlaced,
    evScanMethodNonInterlaced,
    evVersionGif87,
    evVersionGif89,
    evRenderProgressive,
    evRenderNonProgressive,
    evTransformRotate90,
    evTransformRotate180,
    evTransformRotate270,
    evTransformFlipHorizontal,
    evTransformFlipVertical,
    evMultiFrame,
    evLastFrame,
    evFlush,
    evFrameDimensionTime,
    evFrameDimensionResolution,
    evFrameDimensionPage
  );

  TEncoderParameter = record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // number of values for this parameter
    Type_          : ULONG;   // value type, like ValueTypeLONG  etc.
    Value          : pointer; // a pointer to the parameter values
  end;
  PEncoderParameter = ^TEncoderParameter;

  TEncoderParameters = record
    Count     : UINT;  // number of parameters in this structure
    Parameter : array[0..0] of TEncoderParameter;  // parameter values
  end;
  PEncoderParameters = ^TEncoderParameters;

  TGdiPlusHookProc = function(out token: THandle): integer; stdcall;
  TGdiPlusUnhookProc = procedure(token: THandle); stdcall;

const
  EncoderParameterValueTypeLong = 4;    // 32-bit unsigned int
  EncoderQuality: TGUID     = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  EncoderCompression: TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  FrameDimensionPage: TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';

const
  /// the corresponding file extension for every saving format type
  GdipPictureExt: array [TGdipPictureType] of TFileName = (
    '.gif',
    '.png',
    '.jpg',
    '.bmp',
    '.tif'
  );

{$ifdef GDIPLUS_USEENCODERS}

  PICTURE_MIME_TYPES: array[TGdipPictureType] of PAnsiChar = (
    'image/gif',
    'image/png',
    'image/jpeg',
    'image/bmp',
    'image/tiff'
  );

var
  ENCODERS_GUID: array[TGdipPictureType] of TGUID;

{$else}

const
  ENCODERS_GUID: array[TGdipPictureType] of TGUID = (
    '{557CF402-1A04-11D3-9A73-0000F81EF32E}',
    '{557CF406-1A04-11D3-9A73-0000F81EF32E}',
    '{557CF401-1A04-11D3-9A73-0000F81EF32E}',
    '{557CF400-1A04-11D3-9A73-0000F81EF32E}',
    '{557CF405-1A04-11D3-9A73-0000F81EF32E}'
  );

{$endif GDIPLUS_USEENCODERS}


{ ****************** GDI+ TImageAttributes wrapper }

type
  /// an object wrapper to handle gdi+ image attributes
  TImageAttributes = class
  protected
    fAttr: TGpipImageAttributes;
  public
    constructor Create; overload;
    constructor Create(clone: TImageAttributes); overload;
    destructor Destroy; override;
    function SetToIdentity(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function Reset(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetColorMatrix(const colormatrix: TColorMatrix;
      flags: TColorMatrixFlags = ColorMatrixFlagsDefault;
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearColorMatrix(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetThreshold(threshold: Single;
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearThreshold(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetGamma(gamma: Single;
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearGamma(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetNoOp(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearNoOp(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetColorKey(colorLow, colorHigh: cardinal;
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearColorKey(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetOutputChannel(channelFlags: TColorChannelFlags;
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearOutputChannel(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetOutputChannelColorProfile(colorProfileName: PWideChar;
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearOutputChannelColorProfile(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetRemapTable(mapSize: cardinal; map: PColorMap;
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function ClearRemapTable(
      adjusttype: TColorAdjustType = ColorAdjustTypeDefault): TGdipStatus;
    function SetWrapMode(wrap: TWrapMode; color: cardinal = $ff000000;
      clamp: Boolean = false): TGdipStatus;
    function GetAdjustedPalette(colorPalette: PColorPalette;
      colortype: TColorAdjustType): TGdipStatus;
    property Attr: TGpipImageAttributes
      read fAttr;
  end;


{ ****************** TGdiPlus class for Direct Access to the GDI+ Library }

type
  /// Direct access to the Windows GDI+ library
  // - wrap most used GDI+ library calls
  // - is dynamically loaded, so application could start e.g. on plain XP
  TGdiPlus = class(TSynLibrary)
  protected
    fToken: THandle;
    fStartupHook: record
      Hook: TGdiPlusHookProc;
      Unhook: TGdiPlusUnhookProc;
    end;
    fStartupHookToken: THandle;
    fLock: TRTLCriticalSection;
  public
    // Picture related API calls of the GDI+ class hierarchy
    Startup: function(var Token: THandle; var Input, Output): TGdipStatus; stdcall;
    Shutdown: procedure(Token: THandle); stdcall;
    DeleteGraphics: function(graphics: THandle): TGdipStatus; stdcall;
    CreateFromHDC: function(hdc: HDC; out Graphics: THandle): TGdipStatus; stdcall;
    LoadImageFromStream: function(stream: IStream; out image: THandle): TGdipStatus; stdcall;
    LoadImageFromFile: function(filename: PWideChar; out image: THandle): TGdipStatus; stdcall;
    DrawImageRect: function(graphics, image: THandle; x,y,width,height: integer): TGdipStatus; stdcall;
    DrawImageRectRect: function(graphics, image: THandle; xd,yd,wd,hd, xs,ys,ws,hs: integer;
      u: TUnit=uPixel; imageAttributes: TGpipImageAttributes=nil; callback: pointer=nil;
      calldata: pointer=nil): TGdipStatus; stdcall;
    {$ifdef GDIPLUS_USEDPI}
    DrawImage: function(graphics, image: THandle; x,y: integer): TGdipStatus; stdcall;
    {$endif GDIPLUS_USEDPI}
    DisposeImage: function(image: THandle): TGdipStatus; stdcall;
    GetImageRawFormat: function(image: THandle; var format: TGUID): TGdipStatus; stdcall;
    GetImageWidth: function(image: THandle; var width: cardinal): TGdipStatus; stdcall;
    GetImageHeight: function(image: THandle; var height: cardinal): TGdipStatus; stdcall;
    SaveImageToStream: function(image: THandle; stream: IStream;
      clsidEncoder: PGUID; encoderParams: pointer): TGdipStatus; stdcall;
    {$ifdef GDIPLUS_USEENCODERS}
    GetImageEncodersSize: function(out numEncoders: cardinal;
      out size: cardinal): TGdipStatus; stdcall;
    GetImageEncoders: function(numEncoders, size: cardinal;
      encoders: pointer): TGdipStatus; stdcall;
    {$endif GDIPLUS_USEENCODERS}
    CreateBitmapFromHBITMAP: function(hbm: HBITMAP; hpal: HPALETTE;
          out bitmap: THandle): TGdipStatus; stdcall;
    CreateBitmapFromGdiDib: function(bmi, bits: pointer; out bitmap: THandle): TGdipStatus; stdcall;
    BitmapSetResolution: function(bitmap: THandle; XDPI,YDPI: single): TGdipStatus; stdcall;
    GetFrameCount: function(image: THandle; dimensionID: PGUID; var count: UINT): TGdipStatus; stdcall;
    SelectActiveFrame: function(image: THandle; dimensionID: PGUID; frameIndex: UINT): TGdipStatus; stdcall;
    CreateImageAttributes: function(out imageattr: TGpipImageAttributes): TGdipStatus; stdcall;
    CloneImageAttributes: function(imageattr: TGpipImageAttributes; out cloneImageattr: TGpipImageAttributes): TGdipStatus; stdcall;
    DisposeImageAttributes: function(imageattr: TGpipImageAttributes): TGdipStatus; stdcall;
    SetImageAttributesToIdentity: function(imageattr: TGpipImageAttributes; adjusttype: TColorAdjustType): TGdipStatus; stdcall;
    ResetImageAttributes: function(imageattr: TGpipImageAttributes; adjusttype: TColorAdjustType): TGdipStatus; stdcall;
    SetImageAttributesColorMatrix: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool; colorMatrix: PColorMatrix; grayMatrix: PColorMatrix; flags: TColorMatrixFlags): TGdipStatus; stdcall;
    SetImageAttributesThreshold: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool; threshold: Single): TGdipStatus; stdcall;
    SetImageAttributesGamma: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool; gamma: Single): TGdipStatus; stdcall;
    SetImageAttributesNoOp: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool): TGdipStatus; stdcall;
    SetImageAttributesColorKeys: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool; colorLow, colorHigh: cardinal): TGdipStatus; stdcall;
    SetImageAttributesOutputChannel: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool; channelFlags: TColorChannelFlags): TGdipStatus; stdcall;
    SetImageAttributesOutputChannelColorProfile: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool; const colorProfileFilename: PWideChar): TGdipStatus; stdcall;
    SetImageAttributesRemapTable: function(imageattr: TGpipImageAttributes; colortype: TColorAdjustType; enableFlag: Bool; mapSize: UINT; const map: PColorMap): TGdipStatus; stdcall;
    SetImageAttributesWrapMode: function(imageattr: TGpipImageAttributes; wrap: TWrapMode; argb: cardinal; clamp: Bool): TGdipStatus; stdcall;
    GetImageAttributesAdjustedPalette: function(imageattr: TGpipImageAttributes; out colorPalette: PColorPalette; colortype: TColorAdjustType): TGdipStatus; stdcall;
    // drawing/extended API calls of the GDI+ class hierarchy
    DrawLine: function(graphics, pen: THandle; x1,y1,x2,y2: integer): TGdipStatus; stdcall;
    CreatePen: function(color: cardinal; width: Single; units: TUnit; out pen: THandle): TGdipStatus; stdcall;
    DeletePen: function(pen: THandle): TGdipStatus; stdcall;
    Flush: function(graphics: THandle; intention: integer=0): TGdipStatus; stdcall;
    SetSmoothingMode: function(graphics: THandle; mode: TSmoothingMode): TGdipStatus; stdcall;
    SetTextRenderingHint: function(graphics: THandle; mode: TTextRenderingHint): TGdipStatus; stdcall;
    SetPenBrushFill: function(Pen, Brush: THandle): TGdipStatus; stdcall;
    SetPenColor: function(Pen: THandle; Color: cardinal): TGdipStatus; stdcall;
    SetPenWidth: function(Pen: THandle; Width: Single): TGdipStatus; stdcall;
    DeleteBrush: function(brush: THandle): TGdipStatus; stdcall;
    CreateSolidFill: function(color: cardinal; var brush: THandle): TGdipStatus; stdcall;
    FillRectangle: function(graphics, brush: THandle; x, y, width, height: integer): TGdipStatus; stdcall;
    FillEllipse: function(graphics, brush: THandle; x, y, width, height: integer): TGdipStatus; stdcall;
    DrawEllipse: function(graphics, pen: THandle; x, y, width, height: integer): TGdipStatus; stdcall;
    DrawCurve: function(graphics, pen: THandle; Points: pointer; Count: integer): TGdipStatus; stdcall;
    GraphicsClear: function(Graphics: THandle; Color: cardinal): TGdipStatus; stdcall;
    SetPageUnit: function(Graphics: THandle; units: TUnit): TGdipStatus; stdcall;
    DrawRectangle: function(Graphics, Pen: THandle; X, Y, Width, Height: integer): TGdipStatus; stdcall;
    SetPenDashStyle: function(Pen: THandle; dashStyle: integer): TGdipStatus; stdcall;
    DrawPolygon: function(graphics, pen: THandle; points: pointer; count: integer): TGdipStatus; stdcall;
    FillPolygon: function(graphics, brush: THandle; points: pointer; count: integer; fillMode: TFillMode): TGdipStatus; stdcall;
    SetWorldTransform: function(graphics, matrix: THandle): TGdipStatus; stdcall;
    GetWorldTransform: function(graphics, matrix: THandle): TGdipStatus; stdcall;
    CreateMatrix: function(out matrix: THandle): TGdipStatus; stdcall;
    CreateMatrix2: function(m11,m12,m21,m22,dx,dy: Single; out matrix: THandle): TGdipStatus; stdcall;
    DeleteMatrix: function(matrix: THandle): TGdipStatus; stdcall;
    SetMatrixElements: function(matrix: THandle; m11,m12,m21,m22,dx,dy: Single): TGdipStatus; stdcall;
    MultiplyMatrix: function(matrix, matrix2: THandle; order: integer=0): TGdipStatus; stdcall;
    ScaleMatrix: function(matrix: THandle; scaleX, scaleY: Single; order: integer=0): TGdipStatus; stdcall;
    TranslateMatrix: function(matrix: THandle; offsetX, offsetY: Single; order: integer=0): TGdipStatus; stdcall;
    DrawLines: function(Graphics, Pen: THandle; Points: pointer; Count: integer): TGdipStatus; stdcall;
    RecordMetafile: function (DC: HDC; emfType: TEmfType; frameRect: PGdipRect;
      frameUnit: TUnit; description: PWideChar; var out_metafile: THandle): TGdipStatus; stdcall;
    RecordMetafileStream: function (strm: IStream; DC: HDC; emfType: TEmfType; const frameRect: TGdipRect;
      frameUnit: TUnit; description: PWideChar; var out_metafile: THandle): TGdipStatus; stdcall;
    PlayRecord: function(metafile: THandle; RecType, flags, RecSize: cardinal; Rec: pointer): TGdipStatus; stdcall;
    EnumerateMetaFile: function(graphics, metafile: THandle; Dest: PGdipRect;
      callback, data: pointer; imageAttributes: TGpipImageAttributes=nil): TGdipStatus; stdcall;
    ResetWorldTransform: function(graphics: THandle): TGdipStatus; stdcall;
    RotateTransform: function(graphics: THandle; angle: Single; order: integer=0): TGdipStatus; stdcall;
    TranslateTransform: function(graphics: THandle; dx,dy: Single; order: integer=0): TGdipStatus; stdcall;
    CreateFromImage: function(image: THandle; out graphics: THandle): TGdipStatus; stdcall;
    CreateFontFrom: function(aHDC: HDC; out font: THandle): TGdipStatus; stdcall;
    DeleteFont: function(font: THandle): TGdipStatus; stdcall;
    CreateFontFromLogfont: function(hdc: HDC; logfont: PLOGFONTW; out font: THandle): TGdipStatus; stdcall;
    DrawString: function(graphics: THandle; text: PWideChar; length: integer; font: THandle;
      Dest: PGdipRectF; format, brush: THandle): TGdipStatus; stdcall;
    MeasureString: function(graphics: THandle; text: PWideChar; length: integer; font: THandle;
      Dest: PGdipRectF; format: THandle; bound: PGdipRectF;
      codepointsFitted, linesFilled: PInteger): TGdipStatus; stdcall;
    DrawDriverString: function(graphics: THandle; text: PWideChar;
      length: integer; font, brush: THandle; positions: PGdipPointFArray; flag: integer; matrix: THandle): TGdipStatus; stdcall;
    CreatePath: function(brushmode: TFillMode; var path: THandle): TGdipStatus; stdcall;
    DeletePath: function(path: THandle): TGdipStatus; stdcall;
    DrawPath: function(graphics, pen, path: THandle): TGdipStatus; stdcall;
    FillPath: function(graphics, brush, path: THandle): TGdipStatus; stdcall;
    AddPathLine: function(path: THandle; X1,Y1,X2,Y2: integer): TGdipStatus; stdcall;
    AddPathLines: function(path: THandle; Points: pointer; Count: integer): TGdipStatus; stdcall;
    AddPathArc: function(path: THandle; X,Y,width,height: integer; StartAndle, SweepAngle: single): TGdipStatus; stdcall;
    AddPathCurve: function(path: THandle; Points: pointer; Count: integer): TGdipStatus; stdcall;
    AddPathClosedCurve: function(): TGdipStatus; stdcall;
    AddPathEllipse: function(path: THandle; X,Y,width,height: integer): TGdipStatus; stdcall;
    AddPathPolygon: function(path: THandle; Points: pointer; Count: integer): TGdipStatus; stdcall;
    AddPathRectangle: function(path: THandle; X,Y,width,height: integer): TGdipStatus; stdcall;
    ClosePath: function(path: THandle): TGdipStatus; stdcall;
    DrawArc: function(graphics, pen: THandle; X,Y,width,height: integer; StartAndle, SweepAngle: single): TGdipStatus; stdcall;
    DrawBezier: function(graphics, pen: THandle; X1,Y1,X2,Y2,X3,Y3,X4,Y4: integer): TGdipStatus; stdcall;
    DrawPie: function(graphics, pen: THandle; X,Y,width,height: integer; StartAndle, SweepAngle: single): TGdipStatus; stdcall;
    CreateBitmapFromScan0: function(width, height, stride, format: integer; scan0: PByte;
      out bitmap: THandle): TGdipStatus; stdcall;
    BitmapLockBits: function(Bitmap: THandle; const Rect: PGdipRect;
      Flags: TLockModeOptions; Format: integer; out LockedBitmapData: TGdipBitmapData): TGdipStatus; stdcall;
    BitmapUnlockBits: function(Bitmap: THandle; const LockedBitmapData: TGdipBitmapData): TGdipStatus; stdcall;
    GetClip: function (graphics: THandle; Region: THandle): TGdipStatus; stdcall;
    SetClipRegion: function(graphics: THandle; region: THandle; CombineMode: TGdipCombineMode): TGdipStatus; stdcall;
    SetClipRectI: function(graphics: THandle; X,Y,width,height: integer; CombineMode: TGdipCombineMode): TGdipStatus; stdcall;
    ResetClip: function(graphics: THandle): TGdipStatus; stdcall;
    CreateRegion: function(out Region: THandle): TGdipStatus; stdcall;
    DeleteRegion: function(Region: THandle): TGdipStatus; stdcall;
    /// this function is available only with GDI+ version 1.1 from MSOffice
    ConvertToEmfPlus11: function(graphics, image: THandle; flag: pointer;
      emftype: TEmfType; description: PWideChar; var out_metafile: THandle): TGdipStatus; stdcall;
    {$ifdef GDIPLUS_USEENCODERS}
    function GetEncoderClsid(format: PAnsiChar; out pClsid: TGUID): PtrInt;
    {$endif GDIPLUS_USEENCODERS}
  public
    /// load the GDI+ library and all needed procedures
    // - returns true on success
    // - if no DLL file name is supplied, will search for system-available GDI+,
    // starting from Microsoft Office GDI+ 1.1 with ConvertToEmfPlus API
    // - library is loaded dynamically, therefore the executable is able
    // to launch before Windows XP, but with no GDI + functions in such case
    constructor Create(aDllFileName: TFileName = ''); reintroduce;
    /// unload the GDI+ library
    destructor Destroy; override;
    /// GDI+ is not thread-safe, so use this mutex for proper multi-threading
    procedure Lock;
    /// GDI+ is not thread-safe, so use this mutex for proper multi-threading
    procedure UnLock;
  end;


var
  /// internal singleton pointer called when inlining Gdip global function
  // - you may free and override this instance with your own instance if needed
  _Gdip: TGdiPlus;

/// internal function called when inlining Gdip global function
function _GdipLoad: TGdiPlus;

/// raise an EGdiPlus if the GDI+ library was not successfully loaded
procedure EnsureGdipExists(const caller: shortstring);

/// raise an EGdiPlus if no GDI+ library is available, or call Gdip.Lock
// - the GDI+ API is not thread-safe, so Gdip.Lock/UnLock is mandatory 
procedure EnsureGdipExistsAndLock(const caller: shortstring);

/// access the GDI+ library instance
// - will try to load it if needed
// - Gdip.Exists return FALSE if the GDI+ library is not available in this
// operating system (e.g. on Windows 2000) nor the current executable folder
function Gdip: TGdiPlus;
  {$ifdef HASINLINE} inline; {$endif}



{ *************** AntiAliased Rendering of GDI MetaFile }

type
  /// define how ConvertToEmfPlus/DrawAntiAliased draw using GDI+
  TEmfConvertOption = (
    ecoNoGdiPlus,
    ecoDrawString,
    ecoInternalConvert
  );

  /// ConvertToEmfPlus/DrawAntiAliased drawing options
  TEmfConvertOptions = set of TEmfConvertOption;

/// conversion of an EMF metafile handle into a EMF+ image
// - i.e. allows antialiased drawing of the EMF metafile
// - if GDI+ 1.1 (from Office 2007+ or Vista+) is available, will use it -
// otherwise, it will fallback to our own converter, enumerating the EMF records
// - return 0 if GDI+ is not available or conversion failed
// - return an EMF+ metafile handle, to be drawing via gdip.DrawImageRect(), and
// to be eventually released after use by gdip.DisposeImage()
// - this procedure is thread-safe (protected by Gdip.Lock/UnLock)
function ConvertToEmfPlus(Source: HENHMETAFILE; Width, Height: integer;
  Dest: HDC; ConvertOptions: TEmfConvertOptions = [];
  Smoothing: TSmoothingMode = smAntiAlias;
  TextRendering: TTextRenderingHint = trhClearTypeGridFit): THandle;

/// draw an EMF metafile handle using GDI+ anti-aliased rendering
// - will fallback to plain GDI drawing if GDI+ is not available
// - this procedure is thread-safe (protected by Gdip.Lock/UnLock)
procedure DrawAntiAliased(Source: HENHMETAFILE; Width, Height: integer;
  Dest: HDC; DestRect: TRect; ConvertOptions: TEmfConvertOptions = [];
  Smoothing: TSmoothingMode = smAntiAlias;
  TextRendering: TTextRenderingHint = trhClearTypeGridFit); overload;

/// low-level internal function used e.g. by ConvertToEmfPlus()
function MetaFileToIStream(Source: HENHMETAFILE): IStream;

/// low-level internal function used e.g. by BitmapToRawByteString()
function IStreamSize(const S: IStream): Int64;


implementation


{ ****************** GDI+ TImageAttributes wrapper }

{ TImageAttributes }

constructor TImageAttributes.Create;
begin
  EnsureGdipExists('TImageAttributes.Create'); // check it once (paranoid)
  _Gdip.CreateImageAttributes(fAttr);
end;

constructor TImageAttributes.Create(clone: TImageAttributes);
begin
  inherited Create;
  _Gdip.CloneImageAttributes(clone.fAttr, fAttr)
end;

destructor TImageAttributes.Destroy;
begin
  _Gdip.DisposeImageAttributes(fAttr);
  inherited;
end;

function TImageAttributes.SetToIdentity(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesToIdentity(fAttr, adjusttype);
end;

function TImageAttributes.Reset(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.ResetImageAttributes(fAttr, adjusttype);
end;

function TImageAttributes.SetColorMatrix(const colormatrix: TColorMatrix;
  flags: TColorMatrixFlags; adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesColorMatrix(
   fAttr, adjusttype, true, @colormatrix, nil, flags);
end;

function TImageAttributes.ClearColorMatrix(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesColorMatrix(
    fAttr, adjusttype, false, nil, nil, ColorMatrixFlagsDefault);
end;

function TImageAttributes.SetThreshold(threshold: Single;
  adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesThreshold(
    fAttr, adjusttype, true, threshold);
end;

function TImageAttributes.ClearThreshold(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesThreshold(
    fAttr, adjusttype, false, 0.0);
end;

function TImageAttributes.SetGamma(gamma: Single;
  adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesGamma(
    fAttr, adjusttype, true, gamma);
end;

function TImageAttributes.ClearGamma(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesGamma(fAttr, adjusttype, false, 0.0);
end;

function TImageAttributes.SetNoOp(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesNoOp(fAttr, adjusttype, true);
end;

function TImageAttributes.ClearNoOp(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesNoOp(fAttr, adjusttype, false);
end;

function TImageAttributes.SetColorKey(colorLow, colorHigh: cardinal; adjusttype:
  TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesColorKeys(fAttr, adjusttype, true,
    colorLow, colorHigh);
end;

function TImageAttributes.ClearColorKey(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesColorKeys(
    fAttr, adjusttype, false, 0, 0);
end;

function TImageAttributes.SetOutputChannel(channelFlags: TColorChannelFlags;
  adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesOutputChannel(
    fAttr, adjusttype, true, channelFlags);
end;

function TImageAttributes.ClearOutputChannel(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesOutputChannel(
    fAttr, adjusttype, false, ColorChannelFlagsLast);
end;

function TImageAttributes.SetOutputChannelColorProfile(
  colorProfileName: PWideChar; adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesOutputChannelColorProfile(
    fAttr, adjusttype, true, colorProfileName);
end;

function TImageAttributes.ClearOutputChannelColorProfile(
  adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesOutputChannelColorProfile(
    fAttr, adjusttype, false, nil);
end;

function TImageAttributes.SetRemapTable(mapSize: cardinal; map: PColorMap;
  adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesRemapTable(
    fAttr, adjusttype, true, mapSize, map);
end;

function TImageAttributes.ClearRemapTable(adjusttype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesRemapTable(
    fAttr, adjusttype, false, 0, nil);
end;

function TImageAttributes.SetWrapMode(wrap: TWrapMode; color: cardinal;
  clamp: Boolean): TGdipStatus;
begin
  result := _Gdip.SetImageAttributesWrapMode(fAttr, wrap, color, clamp);
end;

function TImageAttributes.GetAdjustedPalette(colorPalette: PColorPalette;
  colortype: TColorAdjustType): TGdipStatus;
begin
  result := _Gdip.GetImageAttributesAdjustedPalette(
    fAttr, colorPalette, colortype);
end;


{ ****************** TGdiPlus class for Direct Access to the GDI+ Library }

{ TGdiPlus }

{$ifdef GDIPLUS_USEENCODERS}

type
  ImageCodecInfo = record
    Clsid: TGUID;
    FormatID: TGUID;
    CodecName: PWCHAR;
    DllName: PWCHAR;
    FormatDescription: PWCHAR;
    FilenameExtension: PWCHAR;
    MimeType: PWCHAR;
    flags: DWORD;
    Version: DWORD;
    SigCount: DWORD;
    SigSize: DWORD;
    SigPattern: PBYTE;
    SigMask: PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

  TImageCodecInfoArray = array[byte] of TImageCodecInfo;

function StrWideAnsiComp(W: PWord; A: PByte): integer;
begin
  // to avoid widestring usage + compatibility with Delphi 2009/2010/XE
  if pointer(W) <> pointer(A) then
    if W <> nil then
      if A <> nil then
      begin
        if W^ = A^ then
          repeat
            if W^ = 0 then
              break;
            inc(W);
            inc(A);
          until W^ <> A^;
        result := W^ - A^;
        exit;
      end
      else
        result := 1
    else  // A=''
      result := -1
  else // W=''
    result := 0;      // W=A
end;

function TGdiPlus.GetEncoderClsid(format: PAnsiChar; out pClsid: TGUID): PtrInt;
var
  num, size: cardinal;
  ImageCodecInfo: AnsiString;
  P: ^TImageCodecInfoArray;
begin
  num := 0; // number of image encoders
  size := 0; // size of the image encoder array in bytes
  result := -1;
  if (GetImageEncodersSize(num, size) <> stOk) or
     (size = 0) then
    exit;
  SetLength(ImageCodecInfo, size);
  P := pointer(ImageCodecInfo);
  if GetImageEncoders(num, size, P) <> stOk then
    exit;
  for result := 0 to num - 1 do
    if StrWideAnsiComp(pointer(P^[result].MimeType), pointer(format)) = 0 then
    begin
      pClsid := P^[result].Clsid;
      exit;
    end;
  result := -1;
end;

{$endif GDIPLUS_USEENCODERS}

const
  GDIP_API_NAME: array[ 0..103
    {$ifdef GDIPLUS_USEDPI}      + 1 {$endif GDIPLUS_USEDPI}
    {$ifdef GDIPLUS_USEENCODERS} + 2 {$endif GDIPLUS_USEENCODERS} ] of PAnsiChar = (
    'GdiplusStartup',
    'GdiplusShutdown',
    'GdipDeleteGraphics',
    'GdipCreateFromHDC',
    'GdipLoadImageFromStream',
    'GdipLoadImageFromFile',
    'GdipDrawImageRectI',
    'GdipDrawImageRectRectI',
    {$ifdef GDIPLUS_USEDPI}
    'GdipDrawImageI',
    {$endif GDIPLUS_USEDPI}
    'GdipDisposeImage',
    'GdipGetImageRawFormat',
    'GdipGetImageWidth',
    'GdipGetImageHeight',
    'GdipSaveImageToStream',
    {$ifdef GDIPLUS_USEENCODERS}
    'GdipGetImageEncodersSize',
    'GdipGetImageEncoders',
    {$endif GDIPLUS_USEENCODERS}
    'GdipCreateBitmapFromHBITMAP',
    'GdipCreateBitmapFromGdiDib',
    'GdipBitmapSetResolution',
    'GdipImageGetFrameCount',
    'GdipImageSelectActiveFrame',
    'GdipCreateImageAttributes',
    'GdipCloneImageAttributes',
    'GdipDisposeImageAttributes',
    'GdipSetImageAttributesToIdentity',
    'GdipResetImageAttributes',
    'GdipSetImageAttributesColorMatrix',
    'GdipSetImageAttributesThreshold',
    'GdipSetImageAttributesGamma',
    'GdipSetImageAttributesNoOp',
    'GdipSetImageAttributesColorKeys',
    'GdipSetImageAttributesOutputChannel',
    'GdipSetImageAttributesOutputChannelColorProfile',
    'GdipSetImageAttributesRemapTable',
    'GdipSetImageAttributesWrapMode',
    'GdipGetImageAttributesAdjustedPalette',
    'GdipDrawLineI',
    'GdipCreatePen1',
    'GdipDeletePen',
    'GdipFlush',
    'GdipSetSmoothingMode',
    'GdipSetTextRenderingHint',
    'GdipSetPenBrushFill',
    'GdipSetPenColor',
    'GdipSetPenWidth',
    'GdipDeleteBrush',
    'GdipCreateSolidFill',
    'GdipFillRectangleI',
    'GdipFillEllipseI',
    'GdipDrawEllipseI',
    'GdipDrawCurveI',
    'GdipGraphicsClear',
    'GdipSetPageUnit',
    'GdipDrawRectangleI',
    'GdipSetPenDashStyle',
    'GdipDrawPolygonI',
    'GdipFillPolygonI',
    'GdipSetWorldTransform',
    'GdipGetWorldTransform',
    'GdipCreateMatrix',
    'GdipCreateMatrix2',
    'GdipDeleteMatrix',
    'GdipSetMatrixElements',
    'GdipMultiplyMatrix',
    'GdipScaleMatrix',
    'GdipTranslateMatrix',
    'GdipDrawLinesI',
    'GdipRecordMetafileI',
    'GdipRecordMetafileStreamI',
    'GdipPlayMetafileRecord',
    'GdipEnumerateMetafileDestRectI',
    'GdipResetWorldTransform',
    'GdipRotateWorldTransform',
    'GdipTranslateWorldTransform',
    'GdipGetImageGraphicsContext',
    'GdipCreateFontFromDC',
    'GdipDeleteFont',
    'GdipCreateFontFromLogfontW',
    'GdipDrawString',
    'GdipMeasureString',
    'GdipDrawDriverString',
    'GdipCreatePath',
    'GdipDeletePath',
    'GdipDrawPath',
    'GdipFillPath',
    'GdipAddPathLineI',
    'GdipAddPathLine2I',
    'GdipAddPathArcI',
    'GdipAddPathCurveI',
    'GdipAddPathClosedCurveI',
    'GdipAddPathEllipseI',
    'GdipAddPathPolygonI',
    'GdipAddPathRectangleI',
    'GdipClosePathFigure',
    'GdipDrawArcI',
    'GdipDrawBezierI',
    'GdipDrawPieI',
    'GdipCreateBitmapFromScan0',
    'GdipBitmapLockBits',
    'GdipBitmapUnlockBits',
    'GdipGetClip',
    'GdipSetClipRegion',
    'GdipSetClipRectI',
    'GdipResetClip',
    'GdipCreateRegion',
    'GdipDeleteRegion',
    nil);

  Office2003Version = $B0000; // Office 2003 = Office 11 ($B)

constructor TGdiPlus.Create(aDllFileName: TFileName);
var
  Input: record
    Version: integer;               // Must be one
    DebugEventCallback: pointer;    // Only for debug builds
    SuppressBackgroundThread: Bool; // true if replacing GDI+ background processing
    SuppressExternalCodecs: Bool;   // true if only using internal codecs
  end;
  {$ifndef WIN64}
  i: PtrInt;
  {$endif WIN64}
  {$ifdef GDIPLUS_USEENCODERS}
  fmt: TGdipPictureType;
  {$endif GDIPLUS_USEENCODERS}
begin
  InitializeCriticalSection(fLock);
  // first try and search the best library name
  if (aDllFileName = '') or
     not FileExists(aDllFileName) then
  begin
    // first try and search gdiplus11.dll / gdiplus.dll in the same directory
    aDllFileName := Executable.ProgramFilePath + 'gdiplus11.dll';
    if not FileExists(aDllFileName) then
      aDllFileName := Executable.ProgramFilePath + 'gdiplus.dll';
    // if not available in the excutable folder, search for Office 2003/2007
    if not FileExists(aDllFileName) then
    begin
      {$ifdef WIN64}
      aDllFileName := ''; // Office is a 32 bit app with 32 bit dlls :)
      {$else}
      aDllFileName := ReadRegString(HKEY_CLASSES_ROOT,
        'Applications\Winword.exe\shell\edit\command', '');
      if aDllFileName <> '' then
      begin
        delete(aDllFileName, 1, 1);
        i := pos('"', aDllFileName);
        if i > 0 then
          SetLength(aDllFileName, i - 1); // 'WinWord.exe' with full path
        if GetFileVersion(aDllFileName) < Office2003Version then
          aDllFileName := ''
        else
        begin
          // no GDI+ 1.1 available in oldest Office
          aDllFileName := ExtractFilePath(aDllFileName) + 'gdiplus.dll';
          if not FileExists(aDllFileName) then
            aDllFileName := '';
        end;
      end;
      {$endif WIN64}
    end;
  end;
  if aDllFileName = '' then
    aDllFileName := 'gdiplus.dll'; // load default OS version
  // resolve all API calls
  if not TryLoadLibrary([aDllFileName], nil) or
     not ResolveAll(@GDIP_API_NAME, @@Startup) then
    exit;
  // EMF conversion API is available only on GDI+ 1.1
  ConvertToEmfPlus11 := GetProcAddress(fHandle, 'GdipConvertToEmfPlus');
  // setup the libray
  FillcharFast(Input, SizeOf(Input), 0);
  Input.Version := 1;
  // see http://mikevdm.com/BlogEntry/Key/GdiplusShutdown-Hangs-Mysteriously
  Input.SuppressBackgroundThread := true;
  if Startup(fToken, Input, fStartupHook) <> stOk then
  begin
    fToken := 0;
    FreeLib;
    exit;
  end;
  if Assigned(fStartupHook.Hook) then // may be nil e.g. for Win64
    fStartupHook.Hook(fStartupHookToken);
  {$ifdef GDIPLUS_USEENCODERS}
  for fmt := low(fmt) to high(fmt) do
    GetEncoderClsid(PICTURE_MIME_TYPES[fmt], ENCODERS_GUID[fmt]);
  {$endif GDIPLUS_USEENCODERS}
end;

destructor TGdiPlus.Destroy;
begin
  if fToken <> 0 then
  begin
    if Assigned(fStartupHook.UnHook) then // may be nil e.g. for Win64
      fStartupHook.UnHook(fStartupHookToken);
    Shutdown(fToken);
    fToken := 0;
  end;
  inherited Destroy;
  DeleteCriticalSection(fLock);
end;

procedure TGdiPlus.Lock;
begin
  EnterCriticalSection(fLock);
end;

procedure TGdiPlus.UnLock;
begin
  LeaveCriticalSection(fLock);
end;


function Gdip: TGdiPlus;
begin
  result := _Gdip;
  if result = nil then
    result := _GdipLoad;
end;

function _GdipLoad: TGdiPlus;
begin
  GlobalLock;
  try
    if _Gdip = nil then
      _Gdip := RegisterGlobalShutdownRelease(TGdiPlus.Create(''));
    result := _Gdip;
  finally
    GlobalUnLock;
  end;
end;

procedure EnsureGdipExists(const caller: shortstring);
begin
  if not Gdip.Exists then
    raise EGdiPlus.CreateFmt('%s: GDI+ not available on this system', [caller]);
end;

procedure EnsureGdipExistsAndLock(const caller: shortstring);
begin
  EnsureGdipExists(caller);
  _Gdip.Lock;
end;


{ *************** AntiAliased Rendering of GDI MetaFile }

{$ifdef FPC}

const
  // FPC does not define those (extracted from mormot.ui.controls.pas)
  EMR_HEADER = 1;
  EMR_POLYBEZIER = 2;
  EMR_POLYGON = 3;
  EMR_POLYLINE = 4;
  EMR_SETWINDOWEXTEX = 9;
  EMR_SETWINDOWORGEX = 10;
  EMR_SETVIEWPORTEXTEX = 11;
  EMR_SETVIEWPORTORGEX = 12;
  EMR_SETBKMODE = 18;
  EMR_SETTEXTALIGN = 22;
  EMR_SETTEXTCOLOR = 24;
  EMR_SETBKCOLOR = 25;
  EMR_OFFSETCLIPRGN = 26;
  EMR_MOVETOEX = 27;
  EMR_EXCLUDECLIPRECT = 29;
  EMR_INTERSECTCLIPRECT = 30;
  EMR_SAVEDC = 33;
  EMR_RESTOREDC = 34;
  EMR_SETWORLDTRANSFORM = 35;
  EMR_SELECTOBJECT = 37;
  EMR_CREATEPEN = 38;
  EMR_CREATEBRUSHINDIRECT = 39;
  EMR_DELETEOBJECT = 40;
  EMR_ELLIPSE = 42;
  EMR_RECTANGLE = 43;
  EMR_ROUNDRECT = 44;
  EMR_LINETO = 54;
  EMR_SELECTCLIPPATH = 67;
  EMR_EXTSELECTCLIPRGN = 75;
  EMR_BITBLT = 76;
  EMR_STRETCHBLT = 77;
  EMR_STRETCHDIBITS = 81;
  EMR_EXTCREATEFONTINDIRECTW = 82;
  EMR_EXTTEXTOUTW = 84;
  EMR_POLYBEZIER16 = 85;
  EMR_POLYGON16 = 86;
  EMR_POLYLINE16 = 87;

type
  TEMRExtTextOut = TEMREXTTEXTOUTW;
  PEMRExtTextOut = ^TEMRExtTextOut;
  PEMRExtCreateFontIndirect = PEMRExtCreateFontIndirectW;

{$else}

type
  TRectL = TRect;
  TPointL = TPoint;
  PPointL = ^TPointL;

{$endif FPC}

type
  /// expected font specifications
  TFontSpec = packed record
    angle: smallint; // -360..+360
    ascent, descent: word;
    underline: boolean;
    strikeout: boolean;
  end;

  /// one DC state properties
  TGdiplusEnumState = record
    pen, brush, font, ClipRegion: THandle;
    move: TPointL;
    WinSize, ViewSize: TSize;
    WinOrg, ViewOrg: TPointL;
    fontColor, fontAlign: integer;
    fontSpec: TFontSpec;
    BkMode, BkColor: cardinal;
  end;

  /// internal data used by EnumEmfCallback() callback function
  TGdiplusEnum = object
  public
    gdip: TGdiplus;
    gr: THandle;
    dest: HDC;
    destMatrix: THandle;
    // contains the GDI+ objects, corresponding to the GDI32 THandleTable
    obj: array of packed record
      // GDI+ handle
      handle: THandle;
      // either OBJ_PEN, OBJ_FONT or OBJ_BRUSH
      kind: integer;
    end;
    // caching pens could make drawing somewhat faster
    CachedPen: array of packed record
      color: cardinal;
      width: cardinal;
      handle: THandle;
    end;
    // caching brushes could make drawing somewhat faster
    CachedBrush: array of packed record
      color: cardinal;
      handle: THandle;
    end;
    // caching fonts could make drawing somewhat faster
    CachedFont: array of packed record
      handle: THandle;
      objfont: TFontSpec;
      logfont: TLogFontW;
    end;
    // the DC states, as stored by SaveDC / RestoreDC methods
    nDC: integer;
    DC: array[0..10] of TGdiplusEnumState;
    // if true, DrawText() will use DrawString and not DrawDriverString
    UseDrawString: boolean;
    // a temporary buffer used to reduce memory allocations
    Temp: TSynTempBuffer;
    procedure SaveDC;
    procedure RestoreDC;
    procedure ScaleMatrix(matrixOrg: THandle);
    procedure CreatePenObj(index: integer; Color, Width, Style: cardinal);
    procedure DeleteObj(index: integer);
    procedure EnumerateEnd;
    function GetCachedPen(color, width: cardinal): THandle;
    function GetCachedSolidBrush(color: cardinal): THandle;
    function GetCachedFontIndex(aLogFont: PLogFontW): integer;
    procedure SetCachedFontSpec(aHandle: THandle; out aObjFont: TFontSpec);
    /// helper function do draw directly a bitmap from *s to *d
    procedure DrawBitmap(
      xs, ys, ws, hs, xd, yd, wd, hd: integer; bmi, bits: pointer);
    /// helper function to draw some text
    procedure DrawText(var EMR: TEMRExtTextOut);
  end;

const
  GdipRectFNull: TGdipRectF = (
    X: 0;
    Y: 0;
    Width: 0;
    Height: 0
  );

function DXTextWidth(DX: PIntegerArray; n: PtrInt): integer;
var
  i: PtrInt;
begin
  result := 0;
  for i := 0 to n - 1 do
    inc(result, DX^[i]);
end;

procedure SetPositions(X, Y: single;
  D: PGdipPointFArray; DX: PIntegerArray; n: PtrInt);
var
  i: PtrInt;
begin
  for i := 0 to n - 1 do
  begin
    D^[i].X := X;
    D^[i].Y := Y;
    X := X + DX^[i];
  end;
end;

procedure NormalizeRect(var R: TRectL);
var
  tmp: integer;
begin
  // GDI+ can't draw twisted rects -> normalize such values
  if R.Right < R.Left then
  begin
    tmp := R.Left;
    R.Left := R.Right;
    R.Right := tmp;
  end;
  if R.Bottom < R.Top then
  begin
    tmp := R.Top;
    R.Top := R.Bottom;
    R.Bottom := tmp;
  end;
end;

function ColorRefToARGB(rgb: COLORREF): cardinal;
begin
  if integer(rgb) < 0 then
    rgb := GetSysColor(rgb and $ff);
  result := (rgb shr 16) or (rgb and $ff00) or (rgb and $ff) shl 16 or $FF000000;
end;

procedure Points16To32(PW: PWordArray; var temp: TSynTempBuffer; n: integer);
var
  i: PtrInt;
  PI: PIntegerArray;
begin
  PI := temp.Init(n * 8);
  for i := 0 to n * 2 - 1 do
    PI^[i] := PW^[i];
end;

// EMF enumeration callback function, called from GDI
// - draw most content with GDI+ functions via the TGdiplusEnum state machine
function EnumEmfCallback(DC: HDC; var Table: THandleTable; Rec: PEnhMetaRecord;
  NumObjects: DWord; var E: TGdiplusEnum): LongBool; stdcall;
var
  x: TXForm;
  mtx, path: THandle;
begin
  result := true;
  with E.DC[E.nDC] do
    case Rec^.iType of
      EMR_HEADER:
        begin
          if pointer(E.obj) = nil then
            SetLength(E.obj, PEnhMetaHeader(Rec)^.nHandles);
          GetWorldTransform(E.dest, x);
          E.Gdip.CreateMatrix2(
            x.eM11, x.eM12, x.eM21, x.eM22, x.eDx, x.eDy, E.destMatrix);
        end;
      EMR_SAVEDC:
        E.SaveDC;
      EMR_RESTOREDC:
        E.RestoreDC;
      EMR_SETWINDOWEXTEX:
        WinSize := PEMRSetWindowExtEx(Rec)^.szlExtent;
      EMR_SETWINDOWORGEX:
        WinOrg := PEMRSetWindowOrgEx(Rec)^.ptlOrigin;
      EMR_SETVIEWPORTEXTEX:
        ViewSize := PEMRSetViewPortExtEx(Rec)^.szlExtent;
      EMR_SETVIEWPORTORGEX:
        ViewOrg := PEMRSetViewPortOrgEx(Rec)^.ptlOrigin;
      EMR_SETBKMODE:
        BkMode := PEMRSetBkMode(Rec)^.iMode;
      EMR_SETBKCOLOR:
        BkColor := PEMRSetBkColor(Rec)^.crColor;
      EMR_SETWORLDTRANSFORM:
        begin
          with PEMRSetWorldTransform(Rec)^.xform do
            E.gdip.CreateMatrix2(eM11, eM12, eM21, eM22, eDx, eDy, mtx);
          E.ScaleMatrix(mtx);
          E.gdip.DeleteMatrix(mtx);
        end;
      EMR_EXTCREATEFONTINDIRECTW:
        with PEMRExtCreateFontIndirect(Rec)^ do
        begin
          E.DeleteObj(ihFont - 1);
          with E.obj[ihFont - 1] do
          begin
            kind := OBJ_FONT;
            with E.CachedFont[E.GetCachedFontIndex(@elfw.elfLogFont)] do
            begin
              font := handle;
              fontspec := objfont;
            end;
            handle := font;
          end;
        end;
      EMR_CREATEPEN:
        with PEMRCreatePen(Rec)^ do
        begin
          E.DeleteObj(ihPen - 1);
          E.CreatePenObj(
            ihPen - 1, lopn.lopnColor, lopn.lopnWidth.x, lopn.lopnStyle);
        end;
      EMR_CREATEBRUSHINDIRECT:
        with PEMRCreateBrushIndirect(Rec)^ do
        begin
          E.DeleteObj(ihBrush - 1);
          with E.obj[ihBrush - 1] do
          begin
            kind := OBJ_BRUSH;
            if lb.lbStyle = BS_NULL then
              brush := 0
            else
            begin
              handle := E.GetCachedSolidBrush(lb.lbColor);
              brush := handle;
            end;
          end;
        end;
      EMR_DELETEOBJECT:
        E.DeleteObj(PEMRDeleteObject(Rec)^.ihObject - 1);
      EMR_SELECTOBJECT:
        with PEMRSelectObject(Rec)^ do
          if integer(ihObject) < 0 then // stock object?
            case ihObject and $7fffffff of
              NULL_BRUSH:
                brush := 0;
              NULL_PEN:
                pen := 0;
            end
          else if ihObject - 1 < DWORD(length(E.obj)) then
            with E.Obj[ihObject - 1] do
              case Kind of
                OBJ_PEN:
                  pen := Handle;
                OBJ_BRUSH:
                  brush := Handle;
                OBJ_FONT:
                  begin
                    font := Handle;
                    E.SetCachedFontSpec(Handle, fontspec);
                  end;
              end;
      EMR_SETTEXTCOLOR:
        fontColor := PEMRSetTextColor(Rec)^.crColor;
      EMR_SETTEXTALIGN:
        fontAlign := PEMRSetTextAlign(Rec)^.iMode;
      EMR_EXTTEXTOUTW:
        E.DrawText(PEMRExtTextOut(Rec)^);
      EMR_MOVETOEX:
        move := PEMRMoveToEx(Rec)^.ptl;
      EMR_LINETO:
        begin
          with PEMRLineTo(Rec)^.ptl do
            E.gdip.DrawLine(E.gr, pen, move.x, move.Y, x, Y);
          move := PEMRLineTo(Rec)^.ptl;
        end;
      EMR_RECTANGLE:
        begin
          NormalizeRect(PEMRRectangle(Rec)^.rclBox);
          if brush <> 0 then
            with PEMRRectangle(Rec)^.rclBox do
              E.gdip.FillRectangle(
                E.gr, brush, Left, Top, Right - Left, Bottom - Top);
          with PEMRRectangle(Rec)^.rclBox do
            E.gdip.DrawRectangle(
              E.gr, pen, Left, Top, Right - Left, Bottom - Top);
        end;
      EMR_ROUNDRECT:
        // perform RoundRect by hand -> just saying: GDI+ does not work!
        with PEMRRoundRect(Rec)^ do
        begin
          NormalizeRect(rclBox);
          E.gdip.CreatePath(fmAlternate, path);
          E.gdip.AddPathArc(path, rclBox.Left, rclBox.Top, szlCorner.cx,
            szlCorner.cy, 180, 90);
          E.gdip.AddPathArc(path, rclBox.Right - szlCorner.cx, rclBox.Top,
            szlCorner.cx, szlCorner.cy, 270, 90);
          E.gdip.AddPathArc(path, rclBox.Right - szlCorner.cx,
            rclBox.Bottom - szlCorner.cy, szlCorner.cx, szlCorner.cy, 0, 90);
          E.gdip.AddPathArc(path, rclBox.Left, rclBox.Bottom - szlCorner.cy,
            szlCorner.cx, szlCorner.cy, 90, 90);
          E.gdip.ClosePath(path);
          if brush <> 0 then
            E.gdip.FillPath(E.gr, brush, path);
          if pen <> 0 then
            E.gdip.DrawPath(E.gr, pen, path);
          E.gdip.DeletePath(path);
        end;
      EMR_ELLIPSE:
        begin
          NormalizeRect(PEMREllipse(Rec)^.rclBox);
          if brush <> 0 then
            with PEMREllipse(Rec)^.rclBox do
              E.gdip.FillEllipse(
                E.gr, brush, Left, Top, Right - Left,Bottom - Top);
          with PEMREllipse(Rec)^.rclBox do
            E.gdip.DrawEllipse(
              E.gr, pen, Left, Top, Right - Left, Bottom - Top);
        end;
      EMR_POLYGON:
        with PEMRPolygon(Rec)^ do
        begin
          if brush <> 0 then
            E.gdip.FillPolygon(E.gr, Brush, @aptl, cptl, fmAlternate);
          if pen <> 0 then
            E.gdip.DrawPolygon(E.gr, Pen, @aptl, cptl);
        end;
      EMR_POLYGON16:
        with PEMRPolygon16(Rec)^ do
        begin
          Points16To32(@apts, E.Temp, cpts);
          if brush <> 0 then
            E.gdip.FillPolygon(E.gr, Brush, E.Temp.buf, cpts, fmAlternate);
          if pen <> 0 then
            E.gdip.DrawPolygon(E.gr, Pen, E.Temp.buf, cpts);
          E.Temp.Done;
        end;
      EMR_POLYLINE:
        with PEMRPolyLine(Rec)^ do
        begin
          E.gdip.DrawLines(E.gr, Pen, @aptl, cptl);
          move := aptl[cptl - 1];
        end;
      EMR_POLYLINE16:
        with PEMRPolyLine16(Rec)^ do
        begin
          Points16To32(@apts, E.Temp, cpts);
          E.gdip.DrawLines(E.gr, Pen, E.Temp.buf, cpts);
          move := PPointL(PAnsiChar(E.Temp.buf) + (cpts - 1) * 8)^;
          E.Temp.Done;
        end;
      EMR_POLYBEZIER:
        with PEMRPolyBezier(Rec)^ do
        begin
          E.gdip.DrawCurve(E.gr, Pen, @aptl, cptl);
          move := aptl[cptl - 1];
        end;
      EMR_POLYBEZIER16:
        with PEMRPolyBezier16(Rec)^ do
        begin
          Points16To32(@apts, E.Temp, cpts);
          E.gdip.DrawCurve(E.gr, Pen, E.Temp.buf, cpts);
          move := PPointL(PAnsiChar(E.Temp.buf) + (cpts - 1) * 8)^;
          E.Temp.Done;
        end;
      EMR_BITBLT:
        begin
          NormalizeRect(PEMRBitBlt(Rec)^.rclBounds);
          with PEMRBitBlt(Rec)^ do
            // only handle RGB bitmaps (no palette)
            if (offBmiSrc <> 0) and
               (offBitsSrc <> 0) and
               (iUsageSrc = DIB_RGB_COLORS) then
              E.DrawBitmap(
                xSrc, ySrc, cxDest, cyDest, xDest, yDest, cxDest, cyDest,
                pointer(PAnsiChar(Rec) + offBmiSrc),
                pointer(PAnsiChar(Rec) + offBitsSrc))
            else
              case PEMRBitBlt(Rec)^.dwRop of
                // we only handle PATCOPY = fillrect
                PATCOPY:
                  with PEMRBitBlt(Rec)^.rclBounds do
                    E.gdip.FillRectangle(E.gr, brush, Left, Top,
                      Right - Left + 1, Bottom - Top + 1);
              end;
        end;
      EMR_STRETCHBLT:
        begin
          NormalizeRect(PEMRStretchBlt(Rec)^.rclBounds);
          with PEMRStretchBlt(Rec)^ do
            // only handle RGB bitmaps (no palette)
            if (offBmiSrc <> 0) and
               (offBitsSrc <> 0) and
               (iUsageSrc = DIB_RGB_COLORS) then
              E.DrawBitmap(
                xSrc, ySrc, cxSrc, cySrc, xDest, yDest, cxDest, cyDest,
                pointer(PAnsiChar(Rec) + offBmiSrc),
                pointer(PAnsiChar(Rec) + offBitsSrc))
            else
              case PEMRStretchBlt(Rec)^.dwRop of
                // we only handle PATCOPY = fillrect
                PATCOPY:
                  with PEMRStretchBlt(Rec)^.rclBounds do
                    E.gdip.FillRectangle(E.gr, brush, Left, Top, Right -
                      Left + 1, Bottom - Top + 1);
              end;
        end;
      EMR_STRETCHDIBITS:
        begin
          NormalizeRect(PEMRStretchDIBits(Rec)^.rclBounds);
          with PEMRStretchDIBits(Rec)^ do
            if (offBmiSrc <> 0) and
               (offBitsSrc <> 0) and
               (iUsageSrc = DIB_RGB_COLORS) then
              E.DrawBitmap(
                xSrc, ySrc, cxSrc, cySrc, xDest, yDest, cxDest, cyDest,
                pointer(PAnsiChar(Rec) + offBmiSrc),
                pointer(PAnsiChar(Rec) + offBitsSrc));
        end;
      EMR_INTERSECTCLIPRECT:
        begin
          with PEMRIntersectClipRect(Rec)^.rclClip do
            E.Gdip.SetClipRectI(
              E.gr, Left, Top, Right - Left, Bottom - Top, cmIntersect);
        end;
      EMR_EXCLUDECLIPRECT:
        begin
          with PEMRExcludeClipRect(Rec)^.rclClip do
            E.Gdip.SetClipRectI(
              E.gr, Left, Top, Right - Left, Bottom - Top, cmExclude);
        end;
      EMR_OFFSETCLIPRGN,
      EMR_SELECTCLIPPATH,
      EMR_EXTSELECTCLIPRGN:
        begin
          // clipping functionality is not fully implemented yet, so when any
          // of the more advanced clipping operations are encountered, the
          // clipping region is simply discarded
          with PEMRExtSelectClipRgn(Rec)^ do
            E.Gdip.ResetClip(E.gr);
        end
    end;
  case Rec^.iType of
    EMR_HEADER,
    EMR_SETWINDOWEXTEX,
    EMR_SETWINDOWORGEX,
    EMR_SETVIEWPORTEXTEX,
    EMR_SETVIEWPORTORGEX:
      E.ScaleMatrix(0);
  end;
end;


{ TGdiplusEnum }

procedure TGdiplusEnum.CreatePenObj(index: integer; Color, Width, Style: cardinal);
begin
  if cardinal(index) <= cardinal(high(Obj)) then
    with Obj[index] do
    begin
      kind := OBJ_PEN;
      gdip.CreatePen(ColorRefToARGB(Color), Width, uWorld, handle);
      if Style in [PS_DASH..PS_DASHDOTDOT] then
        gdip.SetPenDashStyle(handle, PS_DASH); // force PS_DASH on GDI+
      DC[nDC].pen := handle;
    end;
end;

procedure TGdiplusEnum.DeleteObj(index: integer);
begin
  if cardinal(index) < cardinal(length(Obj)) then
    with Obj[index] do
    begin
      if handle <> 0 then
        case kind of
          OBJ_EXTPEN,
          OBJ_PEN:
            begin
              gdip.DeletePen(handle);
              with DC[nDC] do
                if pen = handle then
                  pen := 0;
            end;
          OBJ_BRUSH,
          OBJ_FONT:
            ; // brushs and font are taken from Cached*[] -> deleted in EnumerateEnd
        else
          exit;
        end;
      handle := 0;
      kind := 0;
    end;
end;

procedure TGdiplusEnum.DrawBitmap(xs, ys, ws, hs, xd, yd, wd, hd: integer;
  bmi, bits: pointer);
var
  img: THandle;
begin
  if not gdip.Exists or (gr = 0) then
    exit;
  if gdip.CreateBitmapFromGdiDib(bmi, bits, img) = stOk then
  try
    gdip.DrawImageRectRect(gr, img, xd, yd, wd, hd, xs, ys, ws, hs);
  finally
    gdip.DisposeImage(img);
  end;
end;

function Ceil(const X: single): integer; // avoid dependency to Math.pas unit
begin
  result := integer(Trunc(X));
  if Frac(X) > 0 then
    inc(result);
end;

procedure TGdiplusEnum.DrawText(var EMR: TEMRExtTextOut);
var
  df, rf: TGdipRectF;
  flags: integer;
  drawstring: boolean;
  i: PtrInt;
  mtx, prev: THandle;
  text: PWideChar;
  P: TPoint;
  siz: TSize;
begin
  text := PWideChar(PAnsiChar(@EMR) + EMR.emrtext.offString);
  drawstring := false;
  if UseDrawString then
    // DrawDriverString() does not implement font fallback
    for i := 0 to EMR.emrtext.nChars - 1 do
      if text[i] > #$5ff then
      begin
        drawstring := true;
        break;
      end;
  with DC[nDC], EMR do
  begin
    if drawstring or
       (emrtext.offDx = 0) then
    begin
      Temp.Init(SizeOf(TGdipPointF)); // = 1st glyph pos = drawstring
      // if emf content is not correct -> best guess
      gdip.MeasureString(
        gr, text, emrtext.nChars, font, @GdipRectFNull, 0, @rf, nil, nil);
      siz.cx := Ceil(rf.Width);
      flags := 5; // RealizedAdvance is set -> Temp.buf = 1st glyph position
    end
    else
    begin
      Temp.Init(emrtext.nChars * SizeOf(TGdipPointF));
      siz.cx := DXTextWidth(
        pointer(PAnsiChar(@EMR) + emrtext.offDx), emrText.nChars);
      if emrtext.fOptions and ETO_GLYPH_INDEX <> 0 then
        // Temp.buf is an array of glyph indexes
        flags := 0
      else
        // Temp.buf is an array of every individual glyph position
        flags := 1;
    end;
    // determine the text bounding rectangle
    df.Width := siz.cx;
    df.Height := fontspec.descent + fontspec.ascent;
    df.X := emrtext.ptlReference.X;
    df.Y := emrtext.ptlReference.Y;
    if fontAlign and TA_CENTER = TA_CENTER then
      df.X := df.X - siz.cx / 2
    else if fontAlign and TA_RIGHT <> 0 then
      df.X := df.X - siz.cx;
    if fontAlign and TA_BASELINE <> 0 then
      df.Y := df.Y - fontspec.ascent
    else if fontAlign and TA_BOTTOM <> 0 then
      df.Y := df.Y - fontspec.descent - fontspec.ascent;
    // determine the text baseline start coordinate
    rf := df;
    rf.Y := df.Y + fontspec.ascent;
    // determine the glyph coordinates
    if drawstring or
       (emrtext.offDx = 0) then
      PGdipPointF(Temp.buf)^ := PGdipPointF(@rf)^
    else
      SetPositions(rf.X, rf.Y, Temp.buf,
        pointer(PAnsiChar(@EMR) + emrtext.offDx), emrText.nChars);
    NormalizeRect(emrtext.rcl);
    if (emrtext.fOptions and ETO_CLIPPED <> 0) then
    begin
      // update the clipping region
      // the previous clipping region is saved so it can be restored later
      gdip.CreateRegion(prev);
      gdip.GetClip(gr, prev);
      gdip.SetClipRectI(gr, emrtext.rcl.Left, emrtext.rcl.Top,
        emrtext.rcl.Right - emrtext.rcl.Left,
        emrtext.rcl.Bottom - emrtext.rcl.Top, cmIntersect);
    end;
    //Fill the clipping rectangle if applicable
    if (emrtext.fOptions and ETO_OPAQUE <> 0) then
      gdip.FillRectangle(gr, GetCachedSolidBrush(bkColor),
        emrtext.rcl.Left, emrtext.rcl.Top, emrtext.rcl.Right - emrtext.rcl.Left,
        emrtext.rcl.Bottom - emrtext.rcl.Top);
    if fontspec.angle <> 0 then
    begin
      // manual rotate text -> GDI+ does not work :(
      gdip.CreateMatrix(mtx);
      gdip.GetWorldTransform(gr, mtx);
      gdip.TranslateTransform(gr, emrtext.ptlReference.X, emrtext.ptlReference.Y);
      gdip.RotateTransform(gr, -fontspec.angle);
      gdip.TranslateTransform(gr, -emrtext.ptlReference.X, -emrtext.ptlReference.Y);
    end;
    // if the background is opaque it must be filled with the background colour
    if BkMode = OPAQUE then
      gdip.FillRectangle(gr, GetCachedSolidBrush(bkColor), Trunc(df.X),
        Trunc(df.Y), Ceil(df.Width), Ceil(df.Height));
    if drawstring then
      gdip.DrawString(gr, text, emrtext.nChars, font, @df, 0,
        GetCachedSolidBrush(fontColor))
    else
      gdip.DrawDriverString(gr, text, emrtext.nChars, font,
        GetCachedSolidBrush(fontColor), Temp.buf, flags, 0);
    // Draw*String doesn't handle those -> GDI+ does not work :(
    if fontspec.underline or
       fontSpec.strikeout then
    begin
      siz.cy := fontspec.ascent shr 4;
      if siz.cy < 1 then
        siz.cy := 1;
      P.X := Trunc(rf.X);
      P.Y := Trunc(rf.Y);
      if fontSpec.strikeout then
        dec(P.Y, (fontspec.ascent * 6) shr 4)
      else
        inc(P.Y, siz.cy);
      if siz.cy < 4 then
        siz.cy := 1
      else
        siz.cy := siz.cy shr 1;
      gdip.DrawLine(gr, GetCachedPen(fontColor, siz.cy), P.X, P.Y, P.X +
        siz.Cx - 1, P.Y);
    end;
    if fontspec.angle <> 0 then
    begin
      gdip.SetWorldTransform(gr, {%H-}mtx); // restore previous
      gdip.DeleteMatrix(mtx{%H-});
    end;
    if emrtext.fOptions and ETO_CLIPPED <> 0 then
    begin
      //Restore the clipping region
      gdip.SetClipRegion(gr, {%H-}prev, cmReplace);
      gdip.DeleteRegion(prev{%H-});
    end;
  end;
  Temp.Done;
end;

procedure TGdiplusEnum.EnumerateEnd;
var
  i: PtrInt;
begin
  for i := 1 to nDC do
    gdip.DeleteRegion(DC[nDC].ClipRegion);
  for i := 0 to high(obj) do
    DeleteObj(i);
  for i := 0 to high(CachedPen) do
    gdip.DeletePen(CachedPen[i].handle);
  for i := 0 to high(CachedBrush) do
    gdip.DeleteBrush(CachedBrush[i].handle);
  for i := 0 to high(CachedFont) do
    gdip.DeleteFont(CachedFont[i].handle);
  gdip.DeleteMatrix(destMatrix);
  DeleteDC(dest);
  gdip.DeleteGraphics(gr);
end;

function AnsiICompW(u1, u2: PWideChar): integer;
var
  c1, c2: integer;
begin
  repeat
    c1 := integer(u1^);
    c2 := integer(u2^);
    result := c1 - c2;
    if result <> 0 then
    begin
      if c1 > ord('z') then
        exit
      else if c1 >= ord('a') then
        dec(c1, 32);
      if c2 > ord('z') then
        exit
      else if c2 >= ord('a') then
        dec(c2, 32);
      result := c1 - c2;
      if result <> 0 then
        exit;
    end;
    if (c1 = 0) or
       (c2 = 0) then
      break;
    inc(u1);
    inc(u2);
  until false;
end;

function TGdiplusEnum.GetCachedFontIndex(aLogFont: PLogFontW): integer;
var
  hf: HFONT;
  tm: TTextMetric;
  bak: HGDIOBJ;
  lf: TLogFontW;
  n: integer;
begin
  // DrawDriverString error with underline or strikeout -> GDI+ does not work :(
  MoveFast(aLogFont^, lf, SizeOf(lf)); // faster than lf := logfont
  lf.lfUnderline := 0;
  lf.lfStrikeOut := 0;
  // search if not already in cache
  n := length(CachedFont);
  for result := 0 to n - 1 do
    with CachedFont[result] do
      if CompareMem(@logfont, @lf, SizeOf(TLogFontW) - LF_FACESIZE) and
         (AnsiICompW(logfont.lfFaceName, lf.lfFaceName) = 0) and
         (objfont.underline = boolean(aLogFont^.lfUnderline)) and
         (objfont.strikeout = boolean(aLogFont^.lfStrikeOut)) then
        exit;
  // not available in cache -> create now
  result := n;
  SetLength(CachedFont, result + 1);
  with CachedFont[result] do
  begin
    logfont := lf;
    gdip.CreateFontFromLogfont(dest, @logfont, handle);
    hf := CreateFontIndirectW(logfont);
    bak := SelectObject(dest, hf);
    GetTextMetrics(dest, tm);
    SelectObject(dest, bak);
    DeleteObject(hf);
    objfont.ascent := tm.tmAscent;
    objfont.descent := tm.tmDescent;
    objfont.angle := logfont.lfOrientation div 10;
    objfont.underline := boolean(aLogFont^.lfUnderline);
    objfont.strikeout := boolean(aLogFont^.lfStrikeOut);
  end;
end;

function TGdiplusEnum.GetCachedPen(color, width: cardinal): THandle;
var
  i, n: PtrInt;
begin
  for i := 0 to high(CachedPen) do
    if (CachedPen[i].color = color) and
       (CachedPen[i].width = width) then
    begin
      result := CachedPen[i].handle;
      exit;
    end;
  gdip.CreatePen(ColorRefToARGB(color), width, uPixel, result);
  n := length(CachedPen);
  SetLength(CachedPen, n + 1);
  CachedPen[n].color := color;
  CachedPen[n].width := width;
  CachedPen[n].handle := result;
end;

function TGdiplusEnum.GetCachedSolidBrush(color: cardinal): THandle;
var
  i, n: PtrInt;
begin
  for i := 0 to high(CachedBrush) do
    if CachedBrush[i].color = color then
    begin
      result := CachedBrush[i].handle;
      exit;
    end;
  gdip.CreateSolidFill(ColorRefToARGB(color), result);
  n := length(CachedBrush);
  SetLength(CachedBrush, n + 1);
  CachedBrush[n].color := color;
  CachedBrush[n].handle := result;
end;

procedure TGdiplusEnum.RestoreDC;
begin
  assert(nDC > 0);
  with DC[nDC] do
  begin
    gdip.SetClipRegion(gr, ClipRegion, cmReplace);
    gdip.DeleteRegion(ClipRegion);
  end;
  dec(nDC);
  ScaleMatrix(0);
//  with DC[nDC] do
//    Gdip.SetWorldTransform(Graphics,destMatrix);
end;

procedure TGdiplusEnum.SaveDC;
begin
  Assert(nDC < high(DC));
  DC[nDC + 1] := DC[nDC];
  inc(nDC);
  with DC[nDC] do
  begin
    gdip.CreateRegion(ClipRegion);
    gdip.GetClip(gr, ClipRegion);
  end;
end;

procedure TGdiplusEnum.ScaleMatrix(matrixOrg: THandle);
var
  P: TPoint;
  mtx: THandle;
begin
  with DC[nDC] do
  begin
    P.X := MulDiv(ViewOrg.x, WinSize.cx, ViewSize.cx) - WinOrg.x;
    P.Y := MulDiv(ViewOrg.y, WinSize.cy, ViewSize.cy) - WinOrg.y;
    gdip.CreateMatrix2(
      ViewSize.cx / WinSize.cx, 0, 0, ViewSize.cy / WinSize.cy, P.X, P.Y, mtx);
    gdip.MultiplyMatrix(mtx, destMatrix);
    if matrixOrg <> 0 then
      gdip.MultiplyMatrix(mtx, matrixOrg);
    gdip.SetWorldTransform(gr, mtx);
    gdip.DeleteMatrix(mtx);
  end;
end;

procedure TGdiplusEnum.SetCachedFontSpec(aHandle: THandle;
  out aObjFont: TFontSpec);
var
  i: PtrInt;
begin
  for i := 0 to high(CachedFont) do
    if CachedFont[i].handle = aHandle then
    begin
      aObjFont := CachedFont[i].objfont;
      exit;
    end;
  Int64(aObjFont) := 0;
end;

function MetaFileToIStream(Source: HENHMETAFILE): IStream;
var
  bytes: cardinal;
  glob: THandle;
begin
  bytes := GetEnhMetaFileBits(Source, 0, nil);
  glob := GlobalAlloc(GMEM_MOVEABLE, bytes + 128);
  if GetEnhMetaFileBits(Source, bytes, Windows.GlobalLock(glob)) <> bytes then
    raise EGdiPlus.Create('MetaFileToStream: GetEnhMetaFileBits failure');
  Windows.GlobalUnlock(glob);
  CreateStreamOnHGlobal(glob, {ownglob=}true, result);
end;

function IStreamSize(const S: IStream): Int64;
var
  pos: {$ifdef FPC}QWord{$else}{$ifdef ISDELPHIXE8}LargeUInt{$else}Int64{$endif}{$endif};
begin
  S.Seek(0, STREAM_SEEK_END, pos);
  result := pos;
  S.Seek(0, STREAM_SEEK_SET, pos);
end;

function ConvertToEmfPlus(Source: HENHMETAFILE; Width, Height: integer;
  Dest: HDC; ConvertOptions: TEmfConvertOptions; Smoothing: TSmoothingMode;
  TextRendering: TTextRenderingHint): THandle;
var
  meta, res: THandle;
  istr: IStream;
  R: TGdipRect;
  E: TGdiplusEnum;
begin
  result := 0;
  if (ecoNoGdiPlus in ConvertOptions) or
     not Gdip.Exists or
     (Source = 0) or
     (Dest = 0) then
    exit;
  R.X := 0;
  R.Y := 0;
  R.Width := Width;
  R.Height := Height;
  FillcharFast(E, SizeOf(E), 0);
  E.gdip := _Gdip;
  if assigned(E.gdip.ConvertToEmfPlus11) and
     not (ecoInternalConvert in ConvertOptions) then
  begin
    // let GDI+ 1.1 make the conversion
    istr := MetaFileToIStream(Source);
    E.gdip.Lock;
    try
      if E.gdip.LoadImageFromStream(istr, meta) = stOk then
        try
          E.gdip.CreateFromHDC(Dest, E.gr);
          E.gdip.SetSmoothingMode(E.gr, Smoothing);
          E.gdip.SetTextRenderingHint(E.gr, TextRendering);
          try
            if E.gdip.ConvertToEmfPlus11(
               E.gr, meta, nil, etEmfPlusOnly, nil, res) = stOk then
              result := res;
          finally
            E.gdip.DeleteGraphics(E.gr);
          end;
        finally
          E.gdip.DisposeImage(meta);
        end;
    finally
      E.gdip.UnLock;
    end;
  end
  else
  begin
    // our manual (and not 100% complete yet) conversion
    E.UseDrawString := ecoDrawString in ConvertOptions;
    with E.DC[0] do
    begin
      PInt64(@WinSize)^ := PInt64(@R.Width)^;
      ViewSize := WinSize;
    end;
    E.dest := CreateCompatibleDC(Dest);
    E.gdip.Lock;
    try
      E.gdip.RecordMetafile(E.dest, etEmfPlusOnly, @R, uPixel, nil, result);
      E.gdip.CreateFromImage(result, E.gr);
      E.gdip.SetSmoothingMode(E.gr, Smoothing);
      E.gdip.SetTextRenderingHint(E.gr, TextRendering);
      EnumEnhMetaFile(E.dest, Source, @EnumEmfCallback, @E, TRect(R));
    finally
      E.EnumerateEnd;
      E.gdip.UnLock;
    end;
  end;
end;

procedure DrawAntiAliased(Source: HENHMETAFILE; Width, Height: integer;
  Dest: HDC; DestRect: TRect; ConvertOptions: TEmfConvertOptions;
  Smoothing: TSmoothingMode; TextRendering: TTextRenderingHint);
var
  img, gr: THandle;
begin
  img := ConvertToEmfPlus(
      Source, Width, Height, Dest, ConvertOptions, Smoothing, TextRendering);
  if img = 0 then
  begin
    // GDI Metafile rect includes right and bottom coords
    dec(DestRect.Right);
    dec(DestRect.Bottom);
    // fallback to regular GDI drawing if GDI+ is not available
    PlayEnhMetaFile(Dest, Source, DestRect);
  end
  else
    try
      // use GDI+ 1.0/1.1 anti-aliased rendering
      _Gdip.Lock;
      _Gdip.CreateFromHDC(Dest, gr);
      try
        with DestRect do
          _Gdip.DrawImageRect(gr, img, Left, Top, Right - Left, Bottom - Top);
      finally
        _Gdip.DeleteGraphics(gr);
      end;
    finally
      _Gdip.DisposeImage(img);
      _Gdip.UnLock;
    end;
end;

{$endif OSPOSIX}

end.

