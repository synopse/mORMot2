/// Reusable User Interface Components
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.controls;

{
  *****************************************************************************

   Some Custom Visual Components
    - High-Level UI Wrapper Functions
    - THintWindowDelayed as auto-hiding THintWindow descendant
    - TSynLabeledEdit as extended TLabeledEdit
    - TUIComponentsPersist to persist UI components as JSON

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
  vcl.ExtCtrls,
  vcl.Grids,
  vcl.Forms,
  vcl.Themes,
  {$else}
  Graphics,
  Controls,
  ExtCtrls,
  Grids,
  Forms,
  Themes,
  {$endif NEEDVCLPREFIX}
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.json,
  mormot.core.variants;


{************ High-Level UI Wrapper Functions }

const
  CheckBoxWidth = 13;

{ some LCL/VCL cross-compatibility definitions }

{$ifdef FPC}

type
  TSelectCellEvent = TOnSelectCellEvent;
  TDrawCellEvent = TOnDrawCell;
  TWMTimer = TLMTimer;

const
  WM_TIMER = LM_TIMER;

{$ifdef OSWINDOWS}

type
  TMetaFile = class;

  /// FPC LCL is missing a Windows MetaFile support: minimal Canvas wrapper
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

  /// FPC LCL is missing a Windows MetaFile support: minimal MetaFile wrapper
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
function DrawTextBiDiModeFlags(C: TControl): integer;

/// compute the DrawText() flags for a given Control, with Bidir and word brakes
function DrawTextFlags(C: TControl; CalcOnly: boolean): integer;

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


/// register the TSynIntegerLabeledEdit component in the IDE toolbar
// - not necessary for the mORMot framework to run: since all User Interface
// is created from code, and not from the Delphi IDE, you don't have to register
// anything unless you define your own forms including those components
procedure Register;


{************ THintWindowDelayed as auto-hiding THintWindow descendant }

type
  /// a THintWindow descendant, with an internal delay to auto-hide
  // - this component can be used directly with the hint text to be displayed
  // (companion to the controls Hint properties and Application.ShowHint)
  // - you can specify a time interval for the popup window to be hidden
  // - this component expects UTF-8 encoded text, and displays it as Unicode
  THintWindowDelayed = class(THintWindow)
  protected
    fRow: integer;
    fCol: integer;
    fFontColor: TColor;
    fTimerEnabled: boolean;
    fUtf8Text: RawUtf8;
    /// called after a Hide call
    procedure VisibleChanging; override;
    /// used to hide the popup hint after a delay
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    /// overridden method, Unicode ready
    procedure Paint; override;
  public
    /// initializes the component
    constructor Create(aOwner: TComponent); override;
    /// releases component resources and memory
    destructor Destroy; override;
    /// displays the appropriate Hint Text at a specified screen position
    // - Text is decoded from UTF-8 to Unicode before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedUtf8(const Text: RawUtf8;
      X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean = false); overload;
    /// displays the appropriate Hint Text at a position relative to a control
    // - Text is decoded from UTF-8 to Unicode before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedUtf8(const Text: RawUtf8; Origin: TControl;
      X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean = false); overload;
    /// displays the appropriate Hint Text at a specified screen position
    // - if string is AnsiString (i.e. for Delphi 2 to 2007), Text is decoded into
    // Unicode (using the current i18n code page) before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedString(const Text: string;
      X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean = false); overload;
    /// displays the appropriate Hint Text at a position relative to a control
    // - Text is decoded from Ansi to Unicode (using the current i18n code page) before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedString(const Text: string; Origin: TControl;
      X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean = false); overload;
    /// overridden method, Unicode ready
    function CalcHintRect(MaxWidth: integer; const AHint: RawUtf8;
      AData: Pointer): TRect; reintroduce;
    /// the column number when the hint is displayed
    property Col: integer
      read fCol write fCol;
    /// the row number when the hint is displayed
    property Row: integer
      read fRow write fRow;
  end;


{************ TSynLabeledEdit as extended TLabeledEdit }

type
  /// exception class raised by TSynIntegerLabeledEdit
  ESynLabeledEdit = class(Exception);

  /// diverse kind of values which may be edited by a TSynLabeledEdit
  TSynLabeledEditKind = (
    sleInteger,
    sleInt64,
    sleCurrency,
    sleDouble);

  /// TLabeledEdit with optional type and boundaries check as a variant value
  TSynLabeledEdit = class(TLabeledEdit)
  protected
    FMaxValue: variant;
    FMinValue: variant;
    FAdditionalHint: string;
    FKind: TSynLabeledEditKind;
    FRangeChecking: boolean;
    function IsValid(const Txt: string; var ToValue: variant): boolean;
    procedure SetValue(const Value: variant);
    function GetValue: variant;
    procedure KeyPress(var Key: char); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    /// if true, GetValue() will raise an ESynVariantLabeledEdit exception on
    // any variant value range error, when the Value property is read
    RaiseExceptionOnError: boolean;
    /// convert the entered variant value into a textual representation
    function ToString(NumberOfDigits: integer): string; reintroduce;
    /// return TRUE if the entered value is inside the boundaries
    function ValidateValue: boolean;
    /// create the component instance
    constructor Create(AOwner: TComponent); override;
  published
    /// the kind of value which is currently edited by this TSynLabeledEdit
    property Kind: TSynLabeledEditKind
      read fKind write fKind default sleInteger;
    /// the entered value
    // - getting this property will check for in range according to the
    // current MinValue/MaxValue boundaries, if RangeChecking is set
    // - if RangeChecking is not set, could return a NULL variant for no data
    // - it will sound a beep in case of any out of range
    // - it will also raise a ESynVariantLabeledEdit exception if
    // RaiseExceptionOnError is set to TRUE (equals FALSE by default)
    property Value: variant
      read GetValue write SetValue;
    /// set to TRUE if MinValue/MaxValue properties must be checked when
    // reading Value property
    property RangeChecking: boolean
      read fRangeChecking write fRangeChecking;
    /// lowest allowed variant value
    property MinValue: variant
      read FMinValue write FMinValue;
    /// highest allowed variant value
    property MaxValue: variant
      read FMaxValue write FMaxValue;
    /// some additional popup hint to be displayed
    // - by default, the allowed range is displayed: 'Min. Value: #, Max. Value: #'
    // - you can specify here some additional text to be displayed when the mouse
    // is hover the component
    property AdditionalHint: string
      read FAdditionalHint write FAdditionalHint;
  end;


resourcestring
  SErrorFieldNotValid = 'Field "%s"'#13'does not contain a valid %s value';
  SErrorFieldTooSmall = 'Field "%s"'#13'is too small, value must be >= %s';
  SErrorFieldTooLarge = 'Field "%s"'#13'is too large, value must be <= %s';
  SMinMaxValue = 'Min. Value: %s, Max. Value: %s';


{************ TUIComponentsPersist to persist UI components as JSON }

type
  /// allow to track and load/save UI components as JSON
  // - may be used to persist TEdit / TCheckBox / TComboBox values on a form
  // when the application leaves
  TUIComponentsPersist = class
  protected
    fTracked: array of TComponent;
    fFileName: TFileName;
    fLoadedJson: RawUtf8;
    function GetFileName: TFileName;
  public
    /// would track .Text and .Checked properties only
    procedure TrackControls(const ctrls: array of TComponent);
    /// fill all tracked controls properties from the supplied JSON object
    procedure LoadFromVariant(const aDoc: variant);
    /// save all tracked controls properties as a JSON object
    function SaveToVariant: variant;
    /// fill all tracked controls properties from a local JSON file
    procedure LoadFromFile;
    /// save all tracked controls properties as JSON in a local file
    procedure SaveToFile;
    /// the local JSON file used for persistence
    // - is set to 'executablename.default' if none is specified
    property FileName: TFileName
      read GetFileName write fFileName;
  end;





implementation


{************ THintWindowDelayed as auto-hiding THintWindow descendant }

{ THintWindowDelayed }

constructor THintWindowDelayed.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Color := $C0FFFF;
  fFontColor := clBlack;
{  if aOwner.InheritsFrom(TSQLTableToGrid) and  // done by ParentFont := true
    not TSQLTableToGrid(aOwner).NotDefined then
     Canvas.Font := TDrawGrid(TSQLTableToGrid(aOwner).Owner).Canvas.Font; }
end;

destructor THintWindowDelayed.Destroy;
begin
  Hide;
  inherited;
end;

procedure THintWindowDelayed.ShowDelayedUtf8(const Text: RawUtf8;
  X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean);
var
  R: TRect;
begin
  if self = nil then
    exit;
  Hide;
  if Text = '' then
    exit; // no text to show
  R := CalcHintRect(512, Text, nil);
  if not AlignLeft then
    dec(X, R.Right - R.Left); // align right
  inc(R.Left, X);
  inc(R.Right, X);
  inc(R.Top, Y);
  inc(R.Bottom, Y);
  ActivateHint(R, Utf8ToString(Text)); // perform Caption := Text
  fUtf8Text := Text; // so it will work with Delphi 2009/2010
  if Time <> 0 then
  begin
    if fTimerEnabled then
      KillTimer(Handle, 1)
    else
      fTimerEnabled := true;
    SetTimer(Handle, 1, Time, nil);
  end;
  fFontColor := FontColor;
  Show;
end;

procedure THintWindowDelayed.ShowDelayedUtf8(const Text: RawUtf8;
  Origin: TControl; X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean);
begin
  with Origin.ClientToScreen(Point(X, Y)) do
    ShowDelayedUtf8(Text, X, Y, Time, FontColor, AlignLeft);
end;

function DrawTextBiDiModeFlags(C: TControl): integer;
begin
  if Assigned(C) and
     C.UseRightToLeftReading then
    result := DT_RTLREADING
  else
    result := 0;
end;

function DrawTextFlags(C: TControl; CalcOnly: boolean): integer;
begin
  result := DT_LEFT or DT_WORDBREAK or DT_NOPREFIX;
  if CalcOnly then
    result := result or DT_CALCRECT;
  if Assigned(C) and
     C.UseRightToLeftReading then
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

function THintWindowDelayed.CalcHintRect(MaxWidth: integer;
  const AHint: RawUtf8; AData: Pointer): TRect;
begin
  result := Rect(0, 0, MaxWidth, 0);
  DrawTextUtf8(self, Canvas, AHint, result, {calc=}true);
  Inc(result.Right, 6);
  Inc(result.Bottom, 2);
end;

procedure THintWindowDelayed.Paint;
var
  R: TRect;
begin // unicode version
  R := ClientRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Canvas.Font.Color := fFontColor;
  DrawTextUtf8(self, Canvas, fUtf8Text, R);
end;

procedure THintWindowDelayed.VisibleChanging;
begin
  try
    if fTimerEnabled and
       Visible then // are we in a Hide process?
    begin
      KillTimer(Handle, 1);
      fTimerEnabled := false;
      fRow := -1;
      if HandleAllocated then
        ReleaseHandle;
    end;
  finally
    inherited VisibleChanging;
  end;
end;

procedure THintWindowDelayed.WMTimer(var Msg: TWMTimer);
begin
  Hide;
  fRow := -1;
end;

procedure THintWindowDelayed.ShowDelayedString(const Text: string;
  X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean);
begin
  ShowDelayedUtf8(StringToUtf8(Text), X, Y, Time, FontColor, AlignLeft);
end;

procedure THintWindowDelayed.ShowDelayedString(const Text: string;
  Origin: TControl; X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean);
begin
  with Origin.ClientToScreen(Point(X, Y)) do
    ShowDelayedUtf8(StringToUtf8(Text), X, Y, Time, FontColor, AlignLeft);
end;



{************ TSynLabeledEdit as extended TLabeledEdit }

{ TSynLabeledEdit }

constructor TSynLabeledEdit.Create(AOwner: TComponent);
begin
  inherited;
  ShowHint := True;
  MaxValue := 100;
  MinValue := 1;
  Text := '';
end;

function TSynLabeledEdit.GetValue: variant;
var
  Txt: string;
begin
  Txt := sysutils.trim(Text);
  if Txt = '' then
    if RangeChecking then
    begin
      result := MinValue;
      Text := MinValue;
    end
    else
      VarClear(result)
  else
  begin
    if not IsValid(Txt, result) then
    begin
      if RaiseExceptionOnError then
        raise ESynLabeledEdit.CreateFmt(SErrorFieldNotValid,
          [EditLabel.Caption,
           GetCaptionFromEnum(TypeInfo(TSynLabeledEditKind), ord(Kind))]);
      if RangeChecking then
        result := MinValue
      else
        VarClear(result);
    end;
  end;
  if RangeChecking and
     (result < MinValue) then
  begin
    Text := MinValue;
    if RaiseExceptionOnError then
      raise ESynLabeledEdit.CreateFmt(SErrorFieldTooSmall,
        [EditLabel.Caption, string(MinValue)]);
  end;
  if RangeChecking and
     (result > MaxValue) then
  begin
    Text := MaxValue;
    if RaiseExceptionOnError then
      raise ESynLabeledEdit.CreateFmt(SErrorFieldTooLarge,
        [EditLabel.Caption, string(MaxValue)]);
  end;
end;

function TSynLabeledEdit.IsValid(const Txt: string; var ToValue: variant): boolean;
var
  err: integer;
  resInt32: integer;
  resInt64: Int64;
  resDouble: Double;
  resCurrency: Currency;
begin
  result := false;
  case Kind of
    sleInteger:
      begin
        val(Txt, resInt32, err);
        if err <> 0 then
          exit;
        ToValue := resInt32;
      end;
    sleInt64:
      begin
        val(Txt, resInt64, err);
        if err <> 0 then
          exit;
        ToValue := resInt64;
      end;
    sleCurrency:
      begin
        val(Txt, resDouble, err);
        if err <> 0 then
          exit;
        resCurrency := resDouble;
        ToValue := resCurrency;
      end;
    sleDouble:
      begin
        val(Txt, resDouble, err);
        if err <> 0 then
          exit;
        ToValue := resDouble;
      end;
  end;
  result := true;
end;

procedure TSynLabeledEdit.KeyPress(var Key: char);
var
  Temp: variant;
  TempString: string;
begin
  inherited;
  if Key = #8 then
    exit;
  if Key = ',' then
    Key := '.';
  if (Kind in [sleInteger, sleInt64]) and
     (Key = '.') then
    Key := #0;
  if ((Key < '0') or (Key > '9')) and
     (Key <> '.') then
  begin
    Key := #0;
    Beep;
    exit;
  end;
  TempString := Text;
  if (TempString = #0) or
     (Self.SelText = TempString) then
    exit;
  Insert(Key, TempString, Self.SelStart + 1);
  if IsValid(TempString, Temp) and
     RangeChecking and
     (Temp > MaxValue) then
  begin
    Key := #0;
    Beep;
  end;
end;

procedure TSynLabeledEdit.MouseMove(Shift: TShiftState; X, Y: integer);
var
  H: string;
begin
  inherited;
  if RangeChecking then
    H := format(SMinMaxValue, [string(FMinValue), string(FMaxValue)]);
  if FAdditionalHint <> '' then
    H := sysutils.trim(FAdditionalHint + #13#10 + H);
  Hint := H;
end;

procedure TSynLabeledEdit.SetValue(const Value: variant);
begin
  Text := Value;
end;

function TSynLabeledEdit.ToString(NumberOfDigits: integer): string;
var
  numberOfMissingDigits: integer;
begin
  result := Value;
  numberOfMissingDigits := NumberOfDigits - Length(result);
  if numberOfMissingDigits > 0 then
    result := StringOfChar('0', numberOfMissingDigits) + result;
end;

function TSynLabeledEdit.ValidateValue: boolean;
var
  V: variant;
begin
  result := IsValid(Text, V);
  if RangeChecking and result then
    result := (V >= MinValue) and (V <= MaxValue);
end;


{************ TUIComponentsPersist to persist UI components as JSON }


{ TUIComponentsPersist }

function TUIComponentsPersist.GetFileName: TFileName;
begin
  if fFileName = '' then
    fFileName := ChangeFileExt(Executable.ProgramFileName, '.default');
  result := fFileName;
end;

procedure TUIComponentsPersist.LoadFromFile;
begin
  fLoadedJson := StringFromFile(FileName);
  LoadFromVariant(_JsonFast(fLoadedJson));
end;

procedure TUIComponentsPersist.LoadFromVariant(const aDoc: variant);
var
  i: PtrInt;
  prop: PRttiProp;
  doc: PDocVariantData;
  v: PVariant;

  function HasProp(const PropName: ShortString): boolean;
  begin
    result := false;
    if not doc^.GetAsPVariant(ToUtf8(fTracked[i].Name), v) then
      exit;
    prop := ClassFieldPropWithParents(fTracked[i].ClassType, PropName);
    result := prop <> nil;
  end;

begin
  doc := _Safe(aDoc);
  if doc^.Count = 0 then
    exit;
  for i := 0 to high(fTracked) do
    if HasProp('Text') then
      {%H-}prop^.SetAsString(fTracked[i], VariantToUtf8({%H-}v^))
    else if HasProp('Checked') then
      prop^.SetOrdValue(fTracked[i], ord(boolean(v^)));
end;

procedure TUIComponentsPersist.SaveToFile;
var
  json: RawUtf8;
begin
  json := _Safe(SaveToVariant)^.ToJson('', '', jsonHumanReadable);
  if json <> fLoadedJson then
  begin
    FileFromString(json, FileName);
    fLoadedJson := json;
  end;
end;

function TUIComponentsPersist.SaveToVariant: variant;
var
  i: PtrInt;
  prop: PRttiProp;
  doc: TDocVariantData;
  name: RawUtf8;

  function HasProp(const PropName: ShortString): boolean;
  begin
    prop := ClassFieldPropWithParents(fTracked[i].ClassType, PropName);
    result := prop <> nil;
    if result then
      name := ToUtf8(fTracked[i].name);
  end;

begin
  doc.InitFast;
  for i := 0 to high(fTracked) do
    if HasProp('Text') then
      doc.AddValue(name, {%H-}prop^.GetAsString(fTracked[i]))
    else if HasProp('Checked') then
      doc.AddValue(name, boolean(prop^.GetOrdValue(fTracked[i])));
  result := variant(doc);
end;

procedure TUIComponentsPersist.TrackControls(const ctrls: array of TComponent);
var
  i: PtrInt;
begin
  for i := 0 to high(ctrls) do
    ObjArrayAddOnce(fTracked, ctrls[i]);
end;



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


procedure Register;
begin
  RegisterComponents('mORMot2', [TSynLabeledEdit]);
end;


end.

