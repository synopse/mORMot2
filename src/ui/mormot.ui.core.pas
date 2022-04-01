/// Framework Core Visual Shared Types and Functions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.core;

{
  *****************************************************************************

   Basic types and reusable functions for VCL/LCL User Interface support
    - Some LCL/VCL cross-compatibility definitions
    - High-Level UI Wrapper Functions

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  {$ifdef OSWINDOWS}
  Windows,
  {$endif OSWINDOWS}
  {$ifdef FPC}
  LCLType,
  LCLProc,
  LCLIntf,
  LMessages,
  {$else}
  Types,
  Messages,
  {$endif FPC}
  {$ifdef NEEDVCLPREFIX}
  vcl.Graphics,
  vcl.Controls,
  vcl.Themes,
  {$else}
  Graphics,
  Controls,
  Themes,
  {$endif NEEDVCLPREFIX}
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode;


{************ Some LCL/VCL cross-compatibility definitions }

{$ifdef FPC}

type
  TWMTimer = TLMTimer;

const
  WM_TIMER = LM_TIMER;

{$ifdef OSWINDOWS}

type
  TMetaFile = class;

  /// FPC LCL is missing Windows MetaFile support: minimal Canvas wrapper
  // - just encapsulate the WinAPI - so is not cross platform yet
  TMetaFileCanvas = class(TCanvas)
  private
    fMetafile: TMetaFile;
  public
    constructor Create(AMetafile: TMetaFile; ReferenceDevice: HDC);
    constructor CreateWithComment(AMetafile: TMetaFile; ReferenceDevice: HDC;
      const CreatedBy, Description: string);
    destructor Destroy; override;
  end;

  /// FPC LCL is missing Windows MetaFile support: minimal MetaFile wrapper
  // - just encapsulate the WinAPI - so is not cross platform yet
  TMetaFile = class(TGraphic)
  private
    fImageHandle: HENHMETAFILE;
    fImageMMWidth: integer;  // in 0.01 mm logical pixels
    fImageMMHeight: integer; // in 0.01 mm logical pixels
    fImagePxWidth: integer;  // in device pixels
    fImagePxHeight: integer; // in device pixels
    fHeader: TEnhMetaHeader;
    function GetHeader: PENHMETAHEADER;
    procedure DeleteImage;
    function GetAuthor: string;
    function GetDescription: string;
    function GetHandle: HENHMETAFILE;
    function GetMMHeight: integer;
    function GetMMWidth: integer;
    procedure SetHandle(Value: HENHMETAFILE);
    procedure SetMMHeight(Value: integer);
    procedure SetMMWidth(Value: integer);
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: boolean; override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetHeight(Value: integer); override;
    procedure SetWidth(Value: integer); override;
    function GetTransparent: boolean; override;
    procedure SetTransparent(Value: boolean); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure LoadFromFile(const Filename: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToFile(const Filename: string); override;
    procedure SaveToStream(Stream: TStream); override;
    function ReleaseHandle: HENHMETAFILE;
    property Handle: HENHMETAFILE
      read GetHandle write SetHandle;
    property Empty: boolean
      read GetEmpty;
    property CreatedBy: string
      read GetAuthor;
    property Description: string
      read GetDescription;
    property MMWidth: integer
      read GetMMWidth write SetMMWidth;
    property MMHeight: integer
      read GetMMHeight write SetMMHeight;
  end;


const
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

{$endif OSWINDOWS}

{$endif FPC}


{************ High-Level UI Wrapper Functions }

const
  CheckBoxWidth = 13;

/// draw a CheckBox in the Canvas Handle of the Wwindow hWnd, in the middle
// of the Rect coordinates
// - use theming under XP, Vista and Seven
procedure DrawCheckBox(
  hWnd: THandle; Handle: HDC; const Rect: TRect; Checked: boolean);

/// draw a small vertical Arrow, e.g. to display grid sorting order
procedure DrawSortArrow(Canvas: TCanvas; X, Y: integer; Asc: boolean);

/// wrapper around DrawText() with proper BiDir and Unicode support
procedure DrawTextUtf8(Control: TCustomControl; Canvas: TCanvas;
  const Text: RawUtf8; var Rect: TRect; CalcOnly: boolean = false);

/// same as TControl.DrawTextBiDiModeFlagsReadingOnly but also for LCL
function DrawTextBiDiModeFlags(Control: TControl): integer;

/// compute the DrawText() flags for a given Control, with Bidir and word brakes
function DrawTextFlags(Control: TControl; CalcOnly: boolean): integer;

/// wrapper around ExtTextOutW() or TextRect() with alignment and Unicode support
procedure TextRectUtf8(const Rect: TRect; Canvas: TCanvas; X, Y: integer;
  Text: RawUtf8; Align: TAlignment = taLeftJustify; NoControlChar: boolean = false);

/// wrapper around ExtTextOutW() or TextRect() with alignment and Unicode support
// - X,Y are relative to the Rect.Left/Top corner
procedure TextRectString(const Rect: TRect; Canvas: TCanvas; X, Y: integer;
  const Text: string; Align: TAlignment = taLeftJustify; NoControlChar: boolean = false);

{$ifdef OSWINDOWS}

/// test if the ClearType is enabled for font display
// - ClearType is a software technology that improves the readability of text
// on liquid crystal display (LCD) monitors
function IsClearTypeEnabled: boolean;

/// enable the ClearType font display
// - under Windows 2000, standard font smoothing is forced, since Clear Type
// was introduced with XP
procedure ClearTypeEnable;

{$else}

/// some LCL compatibility function
function ExtTextOutW(DC: HDC; X, Y: integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: ObjPas.PInteger): boolean;

{$endif OSWINDOWS}



implementation


{************ Some LCL/VCL cross-compatibility definitions }


{$ifdef FPC}
{$ifdef OSWINDOWS}

// those types are not defined on LCL :( -> simple WinAPI encapsulation

{ TMetaFileCanvas }

constructor TMetaFileCanvas.Create(AMetafile: TMetaFile; ReferenceDevice: HDC);
begin
  CreateWithComment(AMetafile, ReferenceDevice, AMetafile.CreatedBy,
    AMetafile.Description);
end;

constructor TMetaFileCanvas.CreateWithComment(AMetafile: TMetaFile;
  ReferenceDevice: HDC; const CreatedBy, Description: string);
var
  R: TRect;
  ref, tmp: HDC;
  desc: SynUnicode;
begin
  inherited Create;
  fMetafile := AMetafile;
  ref := ReferenceDevice;
  if ref = 0 then
    ref := GetDC(0);
  try
    if fMetafile.MMWidth = 0 then
      if fMetafile.Width = 0 then
        fMetafile.MMWidth := GetDeviceCaps(ref, HORZSIZE) * 100
      else
        fMetafile.MMWidth := MulDiv(fMetafile.Width,
          GetDeviceCaps(ref, HORZSIZE) * 100, GetDeviceCaps(ref, HORZRES));
    if fMetafile.MMHeight = 0 then
      if fMetafile.Height = 0 then
        fMetafile.MMHeight := GetDeviceCaps(ref, VERTSIZE) * 100
      else
        fMetafile.MMHeight := MulDiv(fMetafile.Height,
          GetDeviceCaps(ref, VERTSIZE) * 100, GetDeviceCaps(ref, VERTRES));
    R := Rect(0, 0, fMetafile.MMWidth, fMetafile.MMHeight);
    if (CreatedBy <> '') or
       (Description <> '') then
      StringToSynUnicode(CreatedBy + #0 + Description + #0#0, desc);
    tmp := CreateEnhMetafileW(ref, nil, @R, pointer(desc));
    if tmp = 0 then
      raise EOutOfResources.Create('TMetaFileCanvas: Out of Resources');
    Handle := tmp;
  finally
    if ReferenceDevice = 0 then
      ReleaseDC(0, ref);
  end;
end;

destructor TMetaFileCanvas.Destroy;
begin
  fMetafile.Handle := CloseEnhMetafile(Handle);
  inherited Destroy;
end;


{ TMetaFile }

destructor TMetaFile.Destroy;
begin
  DeleteImage;
  inherited Destroy;
end;

procedure TMetaFile.Assign(Source: TPersistent);
begin
  if (Source = nil) or
     (Source is TMetaFile) then
  begin
    if fImageHandle <> 0 then
      DeleteImage;
    if Assigned(Source) then
    begin
      fImageHandle := TMetaFile(Source).Handle;
      fImageMMWidth := TMetaFile(Source).MMWidth;
      fImageMMHeight := TMetaFile(Source).MMHeight;
      fImagePxWidth := TMetaFile(Source).Width;
      fImagePxHeight := TMetaFile(Source).Height;
      fHeader.nSize := 0;
    end
  end
  else
    inherited Assign(Source);
end;

procedure TMetaFile.DeleteImage;
begin
  if fImageHandle <> 0 then
    DeleteEnhMetafile(fImageHandle);
  fImageHandle := 0;
  fHeader.nSize := 0;
end;

function GetRawDescription(H: HENHMETAFILE; var tmp: TSynTempBuffer): boolean;
var
  n: integer;
begin
  result := false;
  if H = 0 then
    exit;
  n := GetEnhMetafileDescriptionW(H, 0, nil);
  if n <= 0 then
    exit;
  tmp.Init(n * 2);
  GetEnhMetafileDescriptionW(H, n, tmp.buf);
  result := true;
  // tmp.buf contains UTF-16 encoded Author + #0 + Description + #0#0
end;

function TMetaFile.GetAuthor: string;
var
  tmp: TSynTempBuffer;
begin
  result := '';
  if not GetRawDescription(fImageHandle, tmp) then
    exit;
  RawUnicodeToString(tmp.buf, StrLenW(tmp.buf), result);
  tmp.Done;
end;

function TMetaFile.GetDescription: string;
var
  tmp: TSynTempBuffer;
  P: PWideChar;
begin
  result := '';
  if not GetRawDescription(fImageHandle, tmp) then
    exit;
  P := tmp.buf;
  inc(P, StrLenW(tmp.buf) + 1);
  RawUnicodeToString(P, StrLenW(P), result);
  tmp.Done;
end;

function TMetaFile.GetEmpty: boolean;
begin
  result := (fImageHandle = 0);
end;

function TMetaFile.GetHandle: HENHMETAFILE;
begin
  result := fImageHandle;
end;

function TMetaFile.GetMMHeight: integer;
begin
  result := fImageMMHeight;
end;

function TMetaFile.GetMMWidth: integer;
begin
  result := fImageMMWidth;
end;

procedure TMetaFile.SetHandle(Value: HENHMETAFILE);
var
  hdr: TEnhMetaHeader;
begin
  if (Value <> 0) and
     (GetEnhMetafileHeader(Value, SizeOf(hdr), @hdr) = 0) then
    raise EInvalidImage.Create('Invalid Metafile');
  if fImageHandle <> 0 then
    DeleteImage;
  fImageHandle := Value;
  fImagePxWidth := 0;
  fImagePxHeight := 0;
  fImageMMWidth := hdr.rclFrame.Right - hdr.rclFrame.Left;
  fImageMMHeight := hdr.rclFrame.Bottom - hdr.rclFrame.Top;
end;

procedure TMetaFile.SetMMHeight(Value: integer);
begin
  fImagePxHeight := 0;
  if fImageMMHeight <> Value then
    fImageMMHeight := Value;
end;

procedure TMetaFile.SetMMWidth(Value: integer);
begin
  fImagePxWidth := 0;
  if fImageMMWidth <> Value then
    fImageMMWidth := Value;
end;

procedure TMetaFile.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  R: TRect;
begin
  if fImageHandle = 0 then
    exit;
  R := Rect;
  PlayEnhMetaFile(ACanvas.Handle, fImageHandle, R);
end;

function TMetaFile.GetHeader: PENHMETAHEADER;
begin
  if fHeader.nSize = 0 then
    GetEnhMetaFileHeader(fImageHandle, SizeOf(fHeader), @fHeader);
  result := @fHeader;
end;

function TMetaFile.GetHeight: integer;
begin
  if fImageHandle = 0 then
    result := fImagePxHeight
  else
    with GetHeader^ do
      result := MulDiv(fImageMMHeight, szlDevice.cy, szlMillimeters.cy * 100);
end;

function TMetaFile.GetWidth: integer;
begin
  if fImageHandle = 0 then
    result := fImagePxWidth
  else
    with GetHeader^ do
      result := MulDiv(fImageMMWidth, szlDevice.cx, szlMillimeters.cx * 100);
end;

procedure TMetaFile.SetHeight(Value: integer);
begin
  if fImageHandle = 0 then
    fImagePxHeight := Value
  else
    with GetHeader^ do
      MMHeight := MulDiv(Value, szlMillimeters.cy * 100, szlDevice.cy);
end;

procedure TMetaFile.SetWidth(Value: integer);
begin
  if fImageHandle = 0 then
    fImagePxWidth := Value
  else
    with GetHeader^ do
      MMWidth := MulDiv(Value, szlMillimeters.cx * 100, szlDevice.cx);
end;

procedure TMetaFile.Clear;
begin
  DeleteImage;
end;

procedure TMetaFile.LoadFromFile(const Filename: string);
begin
  raise EComponentError.Create('Not Implemented');
end;

procedure TMetaFile.SaveToFile(const Filename: string);
begin
  raise EComponentError.Create('Not Implemented');
end;

procedure TMetaFile.LoadFromStream(Stream: TStream);
begin
  raise EComponentError.Create('Not Implemented');
end;

procedure TMetaFile.SaveToStream(Stream: TStream);
begin
  raise EComponentError.Create('Not Implemented');
end;

function TMetaFile.ReleaseHandle: HENHMETAFILE;
begin
  DeleteImage;
  result := 0;
end;

function TMetaFile.GetTransparent: boolean;
begin
  // not implemented
  result := false;
end;

procedure TMetaFile.SetTransparent(Value: boolean);
begin
  // not implemented
end;

{$endif OSWINDOWS}
{$endif FPC}


{************ High-Level UI Wrapper Functions }


{$WARN SYMBOL_DEPRECATED OFF} // for ThemeServices

const
  XPState: array[boolean] of TThemedButton = (
    tbCheckBoxUncheckedNormal,
    tbCheckBoxCheckedNormal);
  Win32State: array[boolean] of cardinal = (
    DFCS_BUTTONCHECK,
    DFCS_BUTTONCHECK or DFCS_CHECKED);

procedure DrawCheckBox(hWnd: THandle; Handle: HDC; const Rect: TRect; Checked: boolean);
var
  DrawRect: TRect;
begin
  DrawRect.Left := Rect.Left + (Rect.Right - Rect.Left - CheckBoxWidth) shr 1;
  DrawRect.Top := Rect.Top + 2;
  DrawRect.Right := DrawRect.Left + CheckBoxWidth;
  DrawRect.Bottom := DrawRect.Top + CheckBoxWidth;
  if ThemeServices.ThemesEnabled then
    // Windows XP and later: use theming
    ThemeServices.DrawElement(Handle,
      ThemeServices.GetElementDetails(XPState[Checked]), DrawRect)
  else
    DrawFrameControl(Handle, DrawRect, DFC_BUTTON, Win32State[Checked]);
end;

procedure DrawSortArrow(Canvas: TCanvas; X, Y: integer; Asc: boolean);
var
  Points: array[0..2] of TPoint;
begin
  if Canvas = nil then
    exit;
  if Asc then
  begin
    Points[0].X := X + 5; // ascending order arrow
    Points[1].X := X;
    Points[2].X := X - 5;
    Points[0].Y := Y - 5;
    Points[1].Y := Y + 5;
    Points[2].Y := Y - 5
  end
  else
  begin
    Points[0].X := X - 5; // descending order arrow
    Points[1].X := X + 5;
    Points[2].X := X;
    Points[0].Y := Y + 5;
    Points[1].Y := Y + 5;
    Points[2].Y := Y - 5
  end;
  Canvas.Brush.Color := clWhite; // fill the arrow content
  Canvas.Polygon(Points);
  Canvas.Pen.Color := clLtGray;  // draw the arrow border
  Canvas.MoveTo(Points[0].X, Points[0].Y);
  Canvas.LineTo(Points[1].X, Points[1].Y);
  Canvas.Pen.Color := clGray;
  Canvas.LineTo(Points[2].X, Points[2].Y);
  Canvas.LineTo(Points[0].X, Points[0].Y);
end;

{$ifdef OSWINDOWS} // ClearType is a really Windows specific feature

const // for Delphi 6 compilation
  SPI_GETFONTSMOOTHINGTYPE  = $200A;
  SPI_SETFONTSMOOTHINGTYPE  = $200B;
  FE_FONTSMOOTHINGSTANDARD  = $0001;
  FE_FONTSMOOTHINGCLEARTYPE = $0002;

function IsClearTypeEnabled: boolean;
// see http://blogs.msdn.com/michkap/archive/2008/03/01/7971061.aspx
var
  MType, SmoothFonts: DWORD;
begin
  SmoothFonts := 0;
  SystemParametersInfo(SPI_GETFONTSMOOTHING, 1, @SmoothFonts, 0);
  SystemParametersInfo(SPI_GETFONTSMOOTHINGTYPE, 0, @MType, 0);
  result := boolean(SmoothFonts) and
            (MType = FE_FONTSMOOTHINGCLEARTYPE);
end;

procedure ClearTypeEnable;
var
  MType: PtrUInt;
  SmoothFonts: DWORD;
begin
  if (Win32MajorVersion < 5) or
     IsClearTypeEnabled then
    exit; // no font smoothing before Win2K
  SystemParametersInfo(SPI_GETFONTSMOOTHING, 1, @SmoothFonts, 0);
  if not boolean(SmoothFonts) then
    SystemParametersInfo(SPI_SETFONTSMOOTHING, 1, nil,
      SPIF_UPDATEINIFILE or SPIF_SENDCHANGE);
  if (Win32MajorVersion = 5) and
     (Win32MinorVersion = 0) then
    // no Clear Type on Win2K
    MType := FE_FONTSMOOTHINGSTANDARD
  else
    MType := FE_FONTSMOOTHINGCLEARTYPE;
  SystemParametersInfo(SPI_SETFONTSMOOTHINGTYPE, 0, pointer(MType),
    SPIF_UPDATEINIFILE or SPIF_SENDCHANGE);
end;

const
  TEXT_ALIGN: array[TAlignment] of integer =
    (TA_LEFT,     // taLeftJustify
     TA_RIGHT,    // taRightJustify
     TA_CENTER);  // taCenter

{$else}

function ExtTextOutW(DC: HDC; X, Y: integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: ObjPas.PInteger): boolean;
var
  temp: Utf8String;
  L: integer; // no need to resize temp buffer, just get size
begin
  temp := RawUnicodeToUtf8(Str, Count, L);
  result := ExtTextOut(DC, X, Y, Options, Rect, pointer(temp), L, Dx);
end;

{$endif OSWINDOWS}

function DrawTextBiDiModeFlags(Control: TControl): integer;
begin
  if Assigned(Control) and
     Control.UseRightToLeftReading then
    result := DT_RTLREADING
  else
    result := 0;
end;

function DrawTextFlags(Control: TControl; CalcOnly: boolean): integer;
begin
  result := DT_LEFT or DT_WORDBREAK or DT_NOPREFIX;
  if CalcOnly then
    result := result or DT_CALCRECT;
  if Assigned(Control) and
     Control.UseRightToLeftReading then
    result := result or DT_RTLREADING
end;

{$ifdef OSWINDOWS}

procedure DrawTextUtf8(Control: TCustomControl; Canvas: TCanvas;
  const Text: RawUtf8; var Rect: TRect; CalcOnly: boolean);
var
  U: RawUnicode;
begin
  U := Utf8DecodeToRawUnicode(Text);
  DrawText(Canvas.Handle, pointer(U), length(U) shr 1, Rect,
    DrawTextFlags(Control, CalcOnly));
end;

{$else}

procedure DrawTextUtf8(Control: TCustomControl; Canvas: TCanvas;
  const Text: RawUtf8; var Rect: TRect; CalcOnly: boolean);
begin
  DrawText(Canvas.Handle, pointer(Text), length(Text), Rect, // LCL is UTF-8
    DrawTextFlags(Control, CalcOnly));
end;

{$endif OSWINDOWS}

procedure TextRectUtf8(const Rect: TRect; Canvas: TCanvas; X, Y: integer;
  Text: RawUtf8; Align: TAlignment; NoControlChar: boolean);
var
  i: PtrInt;
  {$ifdef OSWINDOWS}
  options, n: integer;
  tmp: TSynTempBuffer;
  {$else}
  style: TTextStyle;
  {$endif OSWINDOWS}
begin
  if (Text = '') or
     not Assigned(Canvas) then
    exit;
  if NoControlChar then
    for i := 1 to length(Text) do
      if Text[i] < ' ' then
        Text[i] := ' ';
  inc(X, Rect.Left);
  inc(Y, Rect.Top);
  {$ifdef OSWINDOWS}
  // direct Win32 API call
  options := ETO_CLIPPED {$ifndef FPC} or Canvas.TextFlags{$endif};
  if Canvas.Brush.Style <> bsClear then
    options := options or ETO_OPAQUE;
  if Align <> taLeftJustify then
    SetTextAlign(Canvas.Handle, TEXT_ALIGN[Align]);
  n := Utf8DecodeToUnicode(Text, tmp);
  ExtTextOutW(Canvas.Handle, X, Y, options, @Rect, tmp.buf, n, nil);
  tmp.Done;
  if Align <> taLeftJustify then
    SetTextAlign(Canvas.Handle, TA_LEFT);
  {$else}
  // LCL cross-platform display, expecting Text = string = Utf8String
  style := Canvas.TextStyle;
  style.Alignment := Align;
  Canvas.TextRect(Rect, X, Y, Text, style);
  {$endif OSWINDOWS}
end;

procedure TextRectString(const Rect: TRect; Canvas: TCanvas; X, Y: integer;
  const Text: string; Align: TAlignment; NoControlChar: boolean);
begin
  TextRectUtf8(Rect, Canvas, X, Y, StringToUtf8(Text), Align, NoControlChar);
end;



end.

