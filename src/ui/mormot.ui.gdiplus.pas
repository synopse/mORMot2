/// high-level VCL/LCL access to GDI+ for Win32/Win64
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.gdiplus;


{
  *****************************************************************************

   VCL/LCL Windows GDI+ Graphics Device Interface Support
   - TSynPicture and associated GIF/PNG/TIFF/JPG classes
   - High-Level Function Wrappers to Manage Pictures

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

uses
  {$ifdef FPC}
  LCLType,
  LCLProc,
  LCLIntf,
  LMessages,
  mormot.ui.core, // for TMetaFile
  {$else}
  Types,
  {$endif FPC}
  Windows,
  ActiveX,
  sysutils,
  classes,
  {$ifdef NEEDVCLPREFIX}
  vcl.graphics,
  {$else}
  graphics,
  {$endif NEEDVCLPREFIX}
  mormot.core.base,
  mormot.core.os,
  mormot.lib.gdiplus;


{.$define GDIPLUS_USEENCODERS}
// if defined, the GDI+ encoder list will be used - seems not necessary
//  - should be defined here and in mormot.lib.gdiplus (i.e. Projects options)

{.$define GDIPLUS_USEDPI}
// if defined, the DrawAt() method is available, which respect dpi on drawing
//  - should not be useful on most applications
//  - should be defined here and in mormot.lib.gdiplus (i.e. Projects options)


{ ****************** TSynPicture and associated GIF/PNG/TIFF/JPG classes }

type
  /// GIF, PNG, TIFF and JPG pictures support using GDI+ library
  // - parent to TPngImage TJpegImage TGifImage TTiffImage classes
  // - cf @http://msdn.microsoft.com/en-us/library/ms536393
  // for available image formats
  TSynPicture = class(TGraphic)
  protected
    fHasContent: boolean;
    fHeight, fWidth: cardinal;
    fImage: THandle;
    fStream: IStream;
    fGlobal: THandle;
    fGlobalLen: integer;
    fAssignedFromBitmap: boolean;
    function GetEmpty: boolean; override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetHeight(Value: integer); override;
    procedure SetWidth(Value: integer); override;
    procedure ImageSet;
    procedure BitmapSetResolution(DPI: single);
    {$ifdef FPC}
    function GetTransparent: boolean; override;
    procedure SetTransparent(Value: boolean); override;
    {$else}
    procedure Clear;
    {$endif FPC}
    function SaveAsIStream(out Stream: IStream; Format: TGdipPictureType;
      CompressionQuality: integer = 80; IfBitmapSetResolution: single = 0): TGdipStatus;
  public
    constructor Create; override;
    constructor CreateFromFile(const FileName: string);
    constructor CreateFromBuffer(Buffer: pointer; Len: integer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); overload; override;
    procedure Draw(ACanvas: TCanvas; const dst, src: TRect;
      attributes: TImageAttributes = nil; u: TUnit = uPixel); reintroduce; overload;
    {$ifdef GDIPLUS_USEDPI}
    /// since method use dpi -> can drop content if drawing with different dpi
    procedure DrawAt(ACanvas: TCanvas; X, Y: integer);
    {$endif GDIPLUS_USEDPI}
    function LoadFromIStream(Stream: IStream): TGdipStatus;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromBuffer(Buffer: pointer; Len: integer);
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveInternalToStream(Stream: TStream);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    {$ifdef FPC} override;
    procedure Clear; override;
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat); override;
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat); override;
    {$else}
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    {$endif FPC}
    class function CanLoadFromMemory(first4: cardinal): boolean; virtual;
    class function CanLoadFromStream(pmStream: TStream): boolean;
      {$ifdef ISDELPHI102} override; {$endif}
    /// save the picture into any GIF/PNG/JPG/TIFF format
    // - CompressionQuality is used for gptJPG format saving
    // and is expected to be from 0 to 100; for gptTIF format, use
    // ord(TGdipPEncoderValue) to define the parameter; by default, will use
    // ord(evCompressionLZW) to save the TIFF picture with LZW - for gptTIF,
    // only valid values are ord(evCompressionLZW), ord(evCompressionCCITT3),
    // ord(evCompressionCCITT4), ord(evCompressionRle) and ord(evCompressionNone)
    function SaveAs(Stream: TStream; Format: TGdipPictureType;
      CompressionQuality: integer = 80; IfBitmapSetResolution: single = 0): TGdipStatus;
    /// create a bitmap from the corresponding picture
    // - kind of returned image is DIB (device-independent bitmap)
    function ToBitmap: TBitmap;
    /// guess the picture type from its internal format
    // - return gptBMP if no format is found
    function GetImageFormat: TGdipPictureType;
    /// return TRUE if the supplied filename is a picture handled by
    // TSynPicture
    class function IsPicture(const FileName: TFileName): TGraphicClass;
    /// calculate a TRect which fit the specified maximum pixel number
    // - if any side of the picture is bigger than the specified pixel number,
    // the TRect is sized down in order than the biggest size if this value
    function RectNotBiggerThan(MaxPixelsForBiggestSide: integer): TRect;
    /// return the GDI+ native image handle
    property NativeImage: THandle
      read fImage;
  end;

  /// sub class to handle .PNG file extension
  TPngImage = class(TSynPicture)
  public
    class function CanLoadFromMemory(first4: cardinal): boolean; override;
  end;

  /// sub class to handle .JPG file extension
  TJpegImage = class(TSynPicture)
  protected
    fCompressionQuality: integer;
  public
    constructor Create; override;
    class function CanLoadFromMemory(first4: cardinal): boolean; override;
    /// implements the saving feature
    procedure SaveToStream(Stream: TStream); override;
    /// the associated encoding quality (from 0 to 100)
    // - set to 80 by default
    property CompressionQuality: integer
      read fCompressionQuality write fCompressionQuality;
  end;

  /// sub class to handle .GIF file extension
  TGifImage = class(TSynPicture)
  public
    class function CanLoadFromMemory(first4: cardinal): boolean; override;
  end;

  /// sub class to handle .TIF file extension
  // - GDI+ seems not able to load all Tiff file formats, depending on the
  // Windows version and third-party libraries installed
  // - this overridden class implements multiple pages
  TTiffImage = class(TSynPicture)
  protected
    fActivePage: integer;
    procedure SelectPage(index: integer);
  public
    class function CanLoadFromMemory(first4: cardinal): boolean; override;
    /// extract a page from the TIFF and assign it to a bitmap
    procedure ExtractPage(index: integer; wBMP: TBitmap);
    /// retrieve the number of pages in the TIFF file
    function GetPageCount: integer;
    /// multi-page
    // - default Frame/Page Index is 0
    property ActivePageIndex: integer
      read fActivePage write SelectPage;
  end;


/// retrieve a ready to be displayed name of the supplied Graphic Class
function PictureName(Pic: TGraphicClass): string;

/// register TJpegImage TPngImage TGifImage TTiffImage classes to the LCL/VCL
// - will use those TSynPicture derivated classes via GDI+
procedure RegisterSynPictures;


{ ****************** High-Level Function Wrappers to Manage Pictures }

/// draw an EMF TMetaFile using GDI+ anti-aliased rendering
// - will fallback to plain GDI drawing if GDI+ is not available
// - this procedure is thread-safe (protected by Gdip.Lock/UnLock)
procedure DrawAntiAliased(Source: TMetafile; Dest: HDC; const DestRect: TRect;
  ConvertOptions: TEmfConvertOptions = []; Smoothing: TSmoothingMode = smAntiAlias;
  TextRendering: TTextRenderingHint = trhClearTypeGridFit); overload;

/// draw the corresponding EMF metafile into a bitmap created by the method
// - this default TGdiplus implementation uses GDI drawing only
// - use a TGdiplusFull instance for true GDI+ AntiAliaised drawing
// - you can specify a zoom factor by the ScaleX and ScaleY parameters in
// percent: e.g. 100 means 100%, i.e. no scaling
// - returned image is a DIB (device-independent bitmap)
function DrawAntiAliased(Source: TMetafile;
  ScaleX: integer = 100; ScaleY: integer = 100;
  ConvertOptions: TEmfConvertOptions = []; Smoothing: TSmoothingMode = smAntiAlias;
  TextRendering: TTextRenderingHint = trhClearTypeGridFit): TBitmap; overload;

/// helper to save a specified graphic into GIF/PNG/JPG/TIFF format
// - CompressionQuality is only used for gptJPG format saving
// and is expected to be from 0 to 100
// - if MaxPixelsForBiggestSide is set to something else than 0, the resulting
// picture biggest side won't exceed this pixel number
// - this method is thread-safe (using Gdip.Lock/UnLock)
procedure SaveAs(Graphic: TPersistent; Stream: TStream; Format: TGdipPictureType;
  CompressionQuality: integer = 80; MaxPixelsForBiggestSide: cardinal = 0;
  BitmapSetResolution: single = 0); overload;

/// helper to save a specified graphic into GIF/PNG/JPG/TIFF format
// - CompressionQuality is only used for gptJPG format saving
// and is expected to be from 0 to 100
// - if MaxPixelsForBiggestSide is set to something else than 0, the resulting
// picture biggest side won't exceed this pixel number
// - this method is thread-safe (using Gdip.Lock/UnLock)
procedure SaveAs(Graphic: TPersistent; const FileName: TFileName;
  Format: TGdipPictureType; CompressionQuality: integer = 80;
  MaxPixelsForBiggestSide: cardinal = 0; BitmapSetResolution: single = 0); overload;

/// helper to save a specified graphic into GIF/PNG/JPG/TIFF format
// - CompressionQuality is only used for gptJPG format saving
// and is expected to be from 0 to 100
// - if MaxPixelsForBiggestSide is set to something else than 0, the resulting
// picture biggest side won't exceed this pixel number
// - this method is thread-safe (using Gdip.Lock/UnLock)
procedure SaveAsRawByteString(Graphic: TPersistent; out Data: RawByteString;
  Format: TGdipPictureType; CompressionQuality: integer = 80;
  MaxPixelsForBiggestSide: cardinal = 0; BitmapSetResolution: single = 0);

/// helper to save a specified TBitmap into GIF/PNG/JPG/TIFF format
// - CompressionQuality is only used for gptJPG format saving
// and is expected to be from 0 to 100
// - if MaxPixelsForBiggestSide is set to something else than 0, the resulting
// picture biggest side won't exceed this pixel number
// - this method is thread-safe (using Gdip.Lock/UnLock)
function BitmapToRawByteString(Bitmap: TBitmap; out Data: RawByteString;
  Format: TGdipPictureType; CompressionQuality: integer = 80;
  MaxPixelsForBiggestSide: cardinal = 0; BitmapSetResolution: single = 0): TGdipStatus;

/// helper to load a specified graphic from GIF/PNG/JPG/TIFF format content
// - this method is thread-safe (using Gdip.Lock/UnLock)
function LoadFromRawByteString(const Picture: RawByteString): TBitmap;

/// helper function to create a bitmap from any GIF/PNG/JPG/TIFF/EMF/WMF file
// - if file extension if .EMF, the file is drawn with a special antialiased
// GDI+ drawing method (if the global Gdip var is a TGdiplusFull instance)
// - this method is thread-safe (using Gdip.Lock/UnLock)
function LoadFrom(const FileName: TFileName): TBitmap; overload;

/// helper function to create a bitmap from any EMF content
// - the file is drawn with a special antialiased
// GDI+ drawing method (if the global Gdip var is a TGdiplusFull instance)
// - this method is thread-safe (using Gdip.Lock/UnLock)
function LoadFrom(const MetaFile: TMetaFile): TBitmap; overload;

/// recompress a JPEG binary in-place
// - no sizing is done, but a bitmap is created from the supplied JPEG, and
// re-compressed as JPEG using the specified quality
// - may be used to ensure a JPEG binary is a JPEG is a JPEG
// - this method is thread-safe (using Gdip.Lock/UnLock)
function JpegRecompress(
  const jpeg: RawByteString; quality: integer = 80): RawByteString;



implementation



{ ****************** TSynPicture and associated GIF/PNG/TIFF/JPG classes }

{ TSynPicture }

constructor TSynPicture.Create;
begin
  EnsureGdipExists(ClassNameShort(self)^);
  inherited Create;
end;

constructor TSynPicture.CreateFromFile(const FileName: string);
begin
  Create;
  LoadFromFile(FileName);
end;

constructor TSynPicture.CreateFromBuffer(Buffer: pointer; Len: integer);
begin
  Create;
  LoadFromBuffer(Buffer, Len);
end;

destructor TSynPicture.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSynPicture.Assign(Source: TPersistent);
var
  S: TMemoryStream;
begin
  if (Source <> nil) and
     Source.InheritsFrom(TPicture) then
    Source := TPicture(Source).Graphic;
  if (Source = nil) or
     (Source.InheritsFrom(TSynPicture) and
    not TSynPicture(Source).fHasContent) then
    Clear
  else if Source.InheritsFrom(TBitmap) then
  begin
    // direct bitmap creation
    Clear;
    with TBitmap(Source) do
      if _Gdip.CreateBitmapFromHBITMAP(Handle, Palette, fImage) <> stOk then
      begin
        Clear;
        exit;
      end;
    fAssignedFromBitmap := true;
    ImageSet;
  end
  else if Source.InheritsFrom(TGraphic) then
  begin
    // loading from a temp stream
    S := TMemoryStream.Create;
    try
      TGraphic(Source).SaveToStream(S);
      S.Seek(0, soFromBeginning);
      LoadFromStream(S);
    finally
      S.Free;
    end;
  end
  else
    Clear;
end;

procedure TSynPicture.Clear;
begin
  {$ifdef FPC}
  inherited Clear;
  {$endif FPC}
  fHasContent := false;
  fAssignedFromBitmap := false;
  fWidth := 0;
  fHeight := 0;
  if fImage <> 0 then
  begin
    _Gdip.DisposeImage(fImage);
    fImage := 0;
  end;
  fStream := nil;
  if fGlobal <> 0 then
  begin
    Windows.GlobalFree(fGlobal); // fDeleteOnRelease=false -> manual release
    fGlobal := 0;
  end;
  fGlobalLen := 0;
end;

procedure TSynPicture.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  graphics: THandle;
begin
  if (self = nil) or
     not fHasContent or
     (fImage = 0) or
     (ACanvas = nil) then
    exit;
  if (_Gdip.CreateFromHDC(ACanvas.Handle, graphics) = stOk) and
     (graphics <> 0) then
  try
    _Gdip.DrawImageRect(graphics, fImage,
      Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  finally
    _Gdip.DeleteGraphics(graphics);
  end;
end;

procedure TSynPicture.Draw(ACanvas: TCanvas; const dst, src: TRect;
  attributes: TImageAttributes; u: TUnit);
var
  graphics: THandle;
  ia: TGpipImageAttributes;
begin
  if (self = nil) or
     not fHasContent or
     (fImage = 0) or
     (ACanvas = nil) then
    exit;
  if Assigned(attributes) then
    ia := attributes.Attr
  else
    ia := nil;
  if (_Gdip.CreateFromHDC(ACanvas.Handle, graphics) = stOk) and
     (graphics <> 0) then
  try
    _Gdip.DrawImageRectRect(graphics, fImage,
      dst.Left, dst.Top, dst.Right - dst.Left, dst.Bottom - dst.Top,
      src.Left, src.Top, src.Right - src.Left, src.Bottom - src.Top, u, ia);
  finally
    _Gdip.DeleteGraphics(graphics);
  end;
end;

{$ifdef GDIPLUS_USEDPI}
procedure TSynPicture.DrawAt(ACanvas: TCanvas; X, Y: integer);
var
  graphics: THandle;
begin
  if (self = nil) or
     not fHasContent or
     (fImage = 0) or
     (ACanvas = nil) then
    exit;
  graphics := 0;
  _Gdip.SetStatus(_Gdip.CreateFromHDC(ACanvas.Handle, graphics));
  if graphics <> 0 then
  try
    _Gdip.SetStatus(_Gdip.DrawImage(graphics, fImage, X, Y));
  finally
    _Gdip.SetStatus(_Gdip.DeleteGraphics(graphics));
  end;
end;
{$endif GDIPLUS_USEDPI}

procedure TSynPicture.BitmapSetResolution(DPI: single);
begin
  if (fImage <> 0) and
     fAssignedFromBitmap and
     (DPI <> 0) then
    _Gdip.BitmapSetResolution(fImage, DPI, DPI);
end;

procedure TSynPicture.ImageSet;
begin
  if fImage = 0 then
    exit;
  if (_Gdip.GetImageWidth(fImage, fWidth) <> stOk) or
     (_Gdip.GetImageHeight(fImage, fHeight) <> stOk) or
     (fWidth = 0) or
     (fHeight = 0) then
    Clear
  else
    fHasContent := true;
end;

function TSynPicture.GetEmpty: boolean;
begin
  result := not fHasContent;
end;

function TSynPicture.GetHeight: integer;
begin
  result := fHeight;
end;

function TSynPicture.GetImageFormat: TGdipPictureType;
const
  // only the TGuid.D1 is relevant here
  RawFormat: array[TGdipPictureType] of cardinal = (
    $b96b3cb0,
    $b96b3caf,
    $b96b3cae,
    $b96b3cab,
    $b96b3cb1
  );
var
  id: TGuid;
begin
  if fHasContent and
     (fImage <> 0) and
     (_Gdip.GetImageRawFormat(fImage, id) = stOk) then
    for result := low(result) to high(result) do
      if id.D1 = RawFormat[result] then
        exit;
  result := gptBMP; // by default, returns bitmap
end;

function TSynPicture.GetWidth: integer;
begin
  result := fWidth;
end;

const
  PicturesExt: array[0..5] of TFileName = (
    'jpg',
    'jpeg',
    'png',
    'gif',
    'tif',
    'tiff'
  );
  PictureClasses: array[0..5] of TGraphicClass = (
    TJpegImage,
    TJpegImage,
    TPngImage,
    TGifImage,
    TTiffImage,
    TTiffImage
  );

class function TSynPicture.IsPicture(const FileName: TFileName): TGraphicClass;
var
  ext: TFileName;
  i: PtrInt;
begin
  result := nil;
  ext := ExtractFileExt(FileName);
  if ext = '' then
    exit;
  Delete(ext, 1, 1); // '.bmp' -> 'bmp'
  if SameText(ext, 'BMP') then
    result := TBitmap
  else if SameText(ext, 'EMF') then
    result := TMetafile
  else if SameText(ext, 'WMF') then
    result := TMetafile
  else if SameText(ext, 'ICO') then
    result := TIcon
  else
    for i := 0 to high(PicturesExt) do
      if SameText(ext, PicturesExt[i]) then
      begin
        result := PictureClasses[i];
        exit;
      end;
end;

{$ifdef FPC}

procedure TSynPicture.LoadFromClipboardFormat(FormatID: TClipboardFormat);
begin // not implemented
end;

procedure TSynPicture.SaveToClipboardFormat(FormatID: TClipboardFormat);
begin // not implemented
end;

function TSynPicture.GetTransparent: boolean;
begin // not implemented
  result := false;
end;

procedure TSynPicture.SetTransparent(Value: boolean);
begin // not implemented
end;

{$else}

procedure TSynPicture.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin // not implemented
end;

procedure TSynPicture.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin // not implemented
end;

{$endif FPC}

procedure TSynPicture.LoadFromFile(const FileName: string);
var
  s: TStream;
begin
  // don't use direct GDI+ file oriented API: it's better having a local
  // copy of the untouched data in memory (e.g. for further jpeg saving)
  Clear;
  if not FileExists(FileName) then
    exit;
  s := TFileStreamEx.Create(FileName, fmOpenReadDenyNone);
  try
    LoadFromStream(s);
  finally
    s.Free;
  end;
end;

procedure TSynPicture.LoadFromResourceName(
  Instance: THandle; const ResName: string);
var
  s: TCustomMemoryStream;
begin
  if FindResource(Instance, PChar(ResName), RT_RCDATA) <> 0 then
  begin
    s := TResourceStream.Create(Instance, ResName, RT_RCDATA);
    try
      LoadFromStream(s);
    finally
      s.Free;
    end;
  end
  else
    Clear;
end;

function TSynPicture.LoadFromIStream(Stream: IStream): TGdipStatus;
begin
  result := _Gdip.LoadImageFromStream(Stream, fImage);
  if result = stOk then
  begin
    fStream := Stream;
    ImageSet;
  end
  else
    Clear;
end;

procedure TSynPicture.LoadFromStream(Stream: TStream);
var
  P: pointer;
begin
  Clear;
  if Stream = nil then
    exit;
  fGlobalLen := Stream.Size - Stream.Position;
  if fGlobalLen = 0 then
    exit;
  fGlobal := GlobalAlloc(GMEM_MOVEABLE, fGlobalLen);
  if fGlobal = 0 then
    exit;
  P := Windows.GlobalLock(fGlobal);
  Stream.Read(P^, fGlobalLen);
  Windows.GlobalUnlock(fGlobal);
  CreateStreamOnHGlobal(fGlobal, {fDeleteOnRelease=}false, fStream);
  LoadFromIStream(fStream);
end;

procedure TSynPicture.LoadFromBuffer(Buffer: pointer; Len: integer);
var
  P: pointer;
begin
  Clear;
  if (Buffer = nil) or
     (Len <= 0) then
    exit;
  fGlobalLen := Len;
  fGlobal := GlobalAlloc(GMEM_MOVEABLE, Len);
  if fGlobal = 0 then
    exit;
  P := Windows.GlobalLock(fGlobal);
  MoveFast(Buffer^, P^, Len);
  Windows.GlobalUnlock(fGlobal);
  CreateStreamOnHGlobal(fGlobal, false, fStream); // fDeleteOnRelease=false
  LoadFromIStream(fStream);
end;

function TSynPicture.RectNotBiggerThan(MaxPixelsForBiggestSide: integer): TRect;
begin
  result.Left := 0;
  result.Top := 0;
  result.Bottom := fHeight;
  result.Right := fWidth;
  if not fHasContent or
     (result.Bottom = 0) or
     (result.Right = 0) then
    exit;
  if result.Right > result.Bottom then
  begin
    if result.Right > MaxPixelsForBiggestSide then
    begin
      result.Bottom := (result.Bottom * MaxPixelsForBiggestSide) div result.Right;
      result.Right := MaxPixelsForBiggestSide;
    end;
  end
  else if result.Bottom > MaxPixelsForBiggestSide then
  begin
    result.Right := (result.Right * MaxPixelsForBiggestSide) div result.Bottom;
    result.Bottom := MaxPixelsForBiggestSide;
  end;
end;

function TSynPicture.SaveAsIStream(out Stream: IStream; Format: TGdipPictureType;
  CompressionQuality: integer; IfBitmapSetResolution: single): TGdipStatus;
var
  enc: TEncoderParameters;
  p: pointer;
begin
  if (self = nil) or
     (fImage = 0) then
  begin
    result := stInvalidParameter;
    exit;
  end;
  if IfBitmapSetResolution <> 0 then
    BitmapSetResolution(IfBitmapSetResolution);
  enc.Count := 1;
  enc.Parameter[0].Type_ := EncoderParameterValueTypeLong;
  enc.Parameter[0].NumberOfValues := 1;
  enc.Parameter[0].Value := @CompressionQuality;
  p := nil;
  case Format of
    gptJPG:
      if CompressionQuality >= 0 then
      begin
        enc.Parameter[0].Guid := EncoderQuality;
        p := @enc;
      end;
    gptTIF:
      begin
        if not (TGdipPEncoderValue(CompressionQuality) in
          [evCompressionLZW, evCompressionCCITT3, evCompressionCCITT4,
           evCompressionRle, evCompressionNone]) then
          // default tiff compression is LZW
          CompressionQuality := ord(evCompressionLZW);
        enc.Parameter[0].Guid := EncoderCompression;
        p := @enc;
      end;
  end;
  CreateStreamOnHGlobal(0, true, Stream); // fDeleteOnRelease=true
  result := _Gdip.SaveImageToStream(fImage, Stream, @ENCODERS_GUID[Format], p);
end;

function TSynPicture.SaveAs(Stream: TStream; Format: TGdipPictureType;
  CompressionQuality: integer; IfBitmapSetResolution: single): TGdipStatus;
var
  s: IStream;
  len: PtrInt;
  tmp: pointer;
begin
  if Stream = nil then
    result := stInvalidParameter
  else
    result := SaveAsIStream(s, Format, CompressionQuality, IfBitmapSetResolution);
  if result <> stOk then
    exit;
  len := IStreamSize(s);
  Getmem(tmp, len);
  try
    s.Read(tmp, len, nil);
    s := nil; // release ASAP
    Stream.WriteBuffer(tmp^, len);
  finally
    Freemem(tmp);
  end;
end;

procedure TSynPicture.SaveInternalToStream(Stream: TStream);
var
  P: pointer;
  f: TGdipPictureType;
begin
  if (Stream = nil) or
     (fImage = 0) then
    exit;
  if (fGlobal <> 0) and
     not fAssignedFromBitmap then
  begin
    // e.g. for a true .jpg file -> just save as it was loaded :)
    P := Windows.GlobalLock(fGlobal);
    Stream.WriteBuffer(P^, fGlobalLen);
    Windows.GlobalUnLock(fGlobal);
  end
  else
  begin
    // should come from a bitmap -> save in the expected format
    if InheritsFrom(TJpegImage) then
      f := gptJPG
    else if InheritsFrom(TGifImage) then
      f := gptGIF
    else if InheritsFrom(TPngImage) then
      f := gptPNG
    else if InheritsFrom(TTiffImage) then
      f := gptTIF
    else
      f := GetImageFormat;
    SaveAs(Stream, f);
  end;
end;

procedure TSynPicture.SaveToStream(Stream: TStream);
begin
  SaveInternalToStream(Stream);
end;

procedure TSynPicture.SetHeight(Value: integer);
begin // not implemented
end;

procedure TSynPicture.SetWidth(Value: integer);
begin // not implemented
end;

function TSynPicture.ToBitmap: TBitmap;
begin
  if not fHasContent then
    result := nil
  else
  begin
    result := TBitmap.Create;
    result.PixelFormat := pf24bit; // create as DIB (device-independent bitmap)
    result.Width := Width;
    result.Height := Height;
    result.Canvas.Lock;
    try
      result.Canvas.Draw(0, 0, self);
    finally
      result.Canvas.Unlock;
    end;
  end;

end;

class function TSynPicture.CanLoadFromMemory(first4: cardinal): boolean;
begin
  result := (first4 and $00ffffff = $ffd8ff) or // JPEG
            (first4 and $00ffffff = $492049) or // TIFF
            (first4 = $38464947) or  // GIF
            (first4 = $474e5089);   // PNG
end;

class function TSynPicture.CanLoadFromStream(pmStream: TStream): boolean;
var
  pos: Int64;
  v: cardinal;
begin
  pos := pmStream.Position;
  result := (pmStream.Read(v, SizeOf(v)) = SizeOf(v)) and
            CanLoadFromMemory(v);
  pmStream.Position := pos;
end;


{ TPngImage }

class function TPngImage.CanLoadFromMemory(first4: cardinal): boolean;
begin
  result := first4 = $474e5089; // PNG
end;


{ TJpegImage }

constructor TJpegImage.Create;
begin
  inherited;
  fCompressionQuality := 80; // default quality
end;

class function TJpegImage.CanLoadFromMemory(first4: cardinal): boolean;
begin
  result := first4 and $00ffffff = $ffd8ff; // JPEG
end;

procedure TJpegImage.SaveToStream(Stream: TStream);
begin
  SaveAs(Stream, gptJPG, fCompressionQuality);
end;


{ TGifImage }

class function TGifImage.CanLoadFromMemory(first4: cardinal): boolean;
begin
  result := first4 = $38464947; // GIF
end;


{ TTiffImage }

class function TTiffImage.CanLoadFromMemory(first4: cardinal): boolean;
begin
  result := first4 and $00ffffff = $492049; // TIFF
end;

procedure TTiffImage.ExtractPage(index: integer; wBMP: TBitmap);
var
  n: integer;
  s: TMemoryStream;
begin
  n := GetPageCount;
  if (n > 0) and
     (cardinal(index) < cardinal(n)) then
  begin
    try
      _Gdip.SelectActiveFrame(fImage, @FrameDimensionPage, index);
      s := TMemoryStream.Create;
      try
        SaveAs(s, gptBMP);
        s.Position := 0;
        wBMP.LoadFromStream(s);
      finally
        s.Free;
      end;
    finally
      _Gdip.SelectActiveFrame(fImage, @FrameDimensionPage, fActivePage);
    end;
  end
  else
    raise ERangeError.Create('Invalid Page Index');
end;

function TTiffImage.GetPageCount: integer;
var
  n: UINT;
begin
  if fImage <> 0 then
  begin
    _Gdip.GetFrameCount(fImage, @FrameDimensionPage, n);
    result := n;
  end
  else
    result := 0;
end;

procedure TTiffImage.SelectPage(index: integer);
var
  n: integer;
begin
  n := GetPageCount;
  if (n > 0) and
     (cardinal(index) < cardinal(n)) then
  begin
    _Gdip.SelectActiveFrame(fImage, @FrameDimensionPage, index);
    fActivePage := index;
  end
  else
    raise ERangeError.Create('Invalid Page Index');
end;

function PictureName(Pic: TGraphicClass): string;
var
  i: PtrInt;
begin
  result := '';
  if Pic <> nil then
    if Pic.InheritsFrom(TIcon) or
       Pic.InheritsFrom(TBitmap) or
       Pic.InheritsFrom(TMetaFile) then
      result := copy(Pic.ClassName, 2, maxInt)
    else
      for i := 0 to high(PictureClasses) do
        if Pic.InheritsFrom(PictureClasses[i]) then
          result := copy(Pic.ClassName, 2, length(Pic.ClassName) - 6);
end;

procedure RegisterSynPictures;
var
  i: PtrInt;
begin
  // initialize the GDI+ library if necessary
  if Gdip.Exists then
    // register JPG and PNG pictures as TGraphic
    if GetClass('TTiffImage') = nil then
    begin
      RegisterClass(TJpegImage);
      RegisterClass(TPngImage);
      RegisterClass(TGifImage);
      RegisterClass(TTiffImage);
      for i := 0 to high(PicturesExt) do
        TPicture.RegisterFileFormat(
          PicturesExt[i], PictureName(PictureClasses[i]), PictureClasses[i]);
    end;
end;


{ ****************** High-Level Function Wrappers to Manage Pictures }

procedure DrawAntiAliased(Source: TMetafile; Dest: HDC; const DestRect: TRect;
  ConvertOptions: TEmfConvertOptions; Smoothing: TSmoothingMode;
  TextRendering: TTextRenderingHint);
begin
  DrawAntiAliased(Source.Handle, Source.Width, Source.Height, Dest, DestRect,
    ConvertOptions, Smoothing, TextRendering);
end;

function DrawAntiAliased(Source: TMetafile; ScaleX, ScaleY: integer;
  ConvertOptions: TEmfConvertOptions; Smoothing: TSmoothingMode;
  TextRendering: TTextRenderingHint): TBitmap;
var
  R: TRect;
begin
  result := nil;
  if Source = nil then // self=nil is OK below
    Exit;
  R.Left := 0;
  R.Right := (Source.Width * ScaleX) div 100;
  R.Top := 0;
  R.Bottom := (Source.Height * ScaleY) div 100;
  result := TBitmap.Create;
  result.PixelFormat := pf24bit; // create as DIB (device-independent bitmap)
  result.Width := R.Right;
  result.Height := R.Bottom;
  DrawAntiAliased(Source.Handle, Source.Width, Source.Height, result.Canvas.Handle,
    R, ConvertOptions, Smoothing, TextRendering);
end;

procedure SaveAs(Graphic: TPersistent; Stream: TStream; Format: TGdipPictureType;
  CompressionQuality: integer; MaxPixelsForBiggestSide: cardinal;
  BitmapSetResolution: single);
var
  bmp: TBitmap;
  R: TRect;
  pic: TSynPicture;
begin
  EnsureGdipExistsAndLock('SaveAs');
  try
    if Graphic.InheritsFrom(TSynPicture) then
      pic := TSynPicture(Graphic)
    else
      pic := TSynPicture.Create;
    try
      if pic <> Graphic then
        pic.Assign(Graphic); // will do the conversion
      if (MaxPixelsForBiggestSide = 0) or
         ((pic.fWidth <= MaxPixelsForBiggestSide) and
          (pic.fHeight <= MaxPixelsForBiggestSide)) then
        // no resize necessary
        pic.SaveAs(Stream, Format, CompressionQuality, BitmapSetResolution)
      else
      begin
        // resize to the maximum side specified parameter
        bmp := TBitmap.Create;
        try
          bmp.PixelFormat := pf24bit; // create as DIB (device-independent bitmap)
          R := pic.RectNotBiggerThan(MaxPixelsForBiggestSide);
          bmp.Width := R.Right;
          bmp.Height := R.Bottom;
          bmp.Canvas.Lock;
          try
            pic.Draw(bmp.Canvas, R);
          finally
            bmp.Canvas.Unlock;
          end;
          mormot.ui.gdiplus.SaveAs(
            bmp, Stream, Format, CompressionQuality, 0, BitmapSetResolution);
        finally
          bmp.Free;
        end;
      end;
    finally
      if pic <> Graphic then
        pic.Free;
    end;
  finally
    _Gdip.UnLock;
  end;
end;

procedure SaveAs(Graphic: TPersistent; const FileName: TFileName;
  Format: TGdipPictureType; CompressionQuality: integer;
  MaxPixelsForBiggestSide: cardinal; BitmapSetResolution: single);
var
  s: TStream;
begin
  s := TFileStream.Create(FileName, fmCreate);
  try
    SaveAs(Graphic, s, Format,
      CompressionQuality, MaxPixelsForBiggestSide, BitmapSetResolution);
  finally
    s.Free;
  end;
end;

procedure SaveAsRawByteString(Graphic: TPersistent; out Data: RawByteString;
  Format: TGdipPictureType; CompressionQuality: integer;
  MaxPixelsForBiggestSide: cardinal; BitmapSetResolution: single);
var
  s: TRawByteStringStream;
begin
  EnsureGdipExistsAndLock('SaveAsRawByteString');
  try
    s := TRawByteStringStream.Create;
    try
      SaveAs(Graphic, s, Format, CompressionQuality, MaxPixelsForBiggestSide,
        BitmapSetResolution);
      Data := s.DataString;
    finally
      s.Free;
    end;
  finally
    _Gdip.UnLock;
  end;
end;

function BitmapToRawByteString(Bitmap: TBitmap; out Data: RawByteString;
  Format: TGdipPictureType; CompressionQuality: integer;
  MaxPixelsForBiggestSide: cardinal; BitmapSetResolution: single): TGdipStatus;
var
  pic: TSynPicture;
  s: IStream;
  len: PtrInt;
begin
  if Bitmap = nil then
  begin
    result := stInvalidParameter;
    exit;
  end;
  EnsureGdipExistsAndLock('BitmapToRawByteString');
  try
    pic := TSynPicture.Create;
    try
      pic.Assign(Bitmap); // will do the conversion
      result := pic.SaveAsIStream(s, Format, CompressionQuality,
        BitmapSetResolution);
      if result <> stOk then
        exit;
      len := IStreamSize(s);
      SetLength(Data, len);
      s.Read(pointer(Data), len, nil);
      s := nil; // release ASAP
    finally
      pic.Free;
    end;
  finally
    _Gdip.UnLock;
  end;
end;

function LoadFromRawByteString(const Picture: RawByteString): TBitmap;
begin
  result := nil;
  if Picture = '' then
    exit;
  EnsureGdipExistsAndLock('LoadFromRawByteString');
  try
    with TSynPicture.Create do
    try
      LoadFromBuffer(pointer(Picture), length(Picture));
      result := ToBitmap;
    finally
      Free;
    end;
  finally
    _Gdip.UnLock;
  end;
end;

function LoadFrom(const FileName: TFileName): TBitmap;
var
  P: TSynPicture;
  mf: TMetafile;
  ext: TFileName;
begin
  result := nil;
  if not FileExists(FileName) then
    exit;
  EnsureGdipExistsAndLock('LoadFrom');
  try
    ext := ExtractFileExt(FileName);
    if SameText(ext, '.WMF') or
       SameText(ext, '.EMF') then
    begin
      // EMF will be loaded and rendered using GDI+ anti-aliasing
      mf := TMetaFile.Create;
      try
        mf.LoadFromFile(FileName);
        result := LoadFrom(mf);
      finally
        mf.Free;
      end;
    end
    else
    begin
      // non vectorial pictures will be loaded via GDI+
      P := TSynPicture.Create;
      try
        P.LoadFromFile(FileName);
        result := P.ToBitmap;
      finally
        P.Free;
      end;
    end;
  finally
    _Gdip.Unlock;
  end;
end;

function LoadFrom(const MetaFile: TMetaFile): TBitmap;
begin
  EnsureGdipExistsAndLock('LoadFrom');
  try
    result := DrawAntiAliased(MetaFile);
  finally
    _Gdip.Unlock;
  end;
end;

function JpegRecompress(
  const jpeg: RawByteString; quality: integer): RawByteString;
var
  bitmap: TBitmap;
begin
  EnsureGdipExistsAndLock('SaveAsRawByteString');
  try
    bitmap := LoadFromRawByteString(jpeg);
    if bitmap <> nil then
      try
        SaveAsRawByteString(bitmap, result, gptJPG, quality);
      finally
        bitmap.Free;
      end;
  finally
    _Gdip.UnLock;
  end;
end;

{$endif OSPOSIX}


end.

