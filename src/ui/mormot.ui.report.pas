/// Reporting unit with UI Preview and PDF Export
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.report;

(*
  *****************************************************************************

    Simple Report Engine with UI Preview and PDF Export
    - Shared Functions used during Report Rendering
    - TGdiPages Report Engine
    - TRenderPages Prototype - unfinished 

    Forked and heavily patched from TPages component (c) 2003 Angus Johnson

  *****************************************************************************
*)

interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

{.$define MOUSE_CLICK_PERFORM_ZOOM} // old not user-friendly behavior
{.$define RENDERPAGES} // TRenderBox and TRenderPages are not yet finished

{$define GDIPLUSDRAW}
// optionaly (if ForceNoAntiAliased=false) use GDI+ to draw for antialiasing:
// slower but smoother (need the GDI+ library, best with version 1.1)

{.$define USEPDFPRINTER}
// do not use the Synopse PDF engine, in Delphi code, but a doPdf virtual printer

{$define USE_UNISCRIBE}
// the same conditional as in mormot.ui.pdf
{$ifdef NO_USE_UNISCRIBE}
  // this special conditional can be set globaly for an application which does
  // not need the UniScribe features
  {$undef USE_UNISCRIBE}
{$endif USE_UNISCRIBE}

uses
  windows,
  messages,
  sysutils,
  classes,
  contnrs,
  graphics,
  controls,
  dialogs,
  forms,
  stdctrls,
  extctrls,
  winspool,
  printers,
  menus,
  shellapi,
  richedit,
  {$ifdef ISDELPHIXE3}
  system.uitypes,
  {$endif ISDELPHIXE3}
  mormot.core.base,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.zip,
  types,
  clipbrd,
  {$ifdef FPC}
  lcltype,
  lclproc,
  lclintf,
  rtlconsts,
  {$else}
  consts,
  {$endif FPC}
  {$ifndef USEPDFPRINTER}
  mormot.ui.pdf,
  {$endif USEPDFPRINTER}
  {$ifdef GDIPLUSDRAW}
  mormot.lib.gdiplus,
  mormot.ui.gdiplus,
  {$endif GDIPLUSDRAW}
  mormot.ui.core; // for TMetaFile definition


{ ****************** Shared Functions used during Report Rendering }

resourcestring
  sPDFFile = 'Acrobat File';
  sPageN = 'Page %d / %d';
  /// used to create the popup menu of the report
  // - should match TGdiPagePreviewButton order
  sReportPopupMenu1 = '&Next page,&Previous page,&Go to Page...,&Zoom...,'+
    '&Bookmarks,Copy Page as &Text,P&rint,PDF &Export,&Close,Page fit,Page width';
  /// used to create the pages browsing menu of the report
  sReportPopupMenu2 = 'Pages %d to %d,Page %d';


const
  /// minimum gray border with around preview page
  GRAY_MARGIN = 10;

  /// TGdiPages.Zoom property value for "Page width" layout during preview
  PAGE_WIDTH = -1;
  /// TGdiPages.Zoom property value for "Page fit" layout during preview
  PAGE_FIT   = -2;

  //TEXT FORMAT FLAGS...
  FORMAT_DEFAULT    = $0;
  //fontsize bits 0-7  .'. max = 255
  FORMAT_SIZE_MASK  = $FF;
  //alignment bits 8-9
  FORMAT_ALIGN_MASK = $300;
  FORMAT_LEFT       = $0;
  FORMAT_RIGHT      = $100;
  FORMAT_CENTER     = $200;
  FORMAT_JUSTIFIED  = $300;
  //fontstyle bits 10-12
  FORMAT_BOLD       = $400;
  FORMAT_UNDERLINE  = $800;
  FORMAT_ITALIC     = $1000;
  //undefined bit 13
  FORMAT_UNDEFINED  = $2000;
  //line flags bits 14-15
  FORMAT_SINGLELINE = $8000;
  FORMAT_DOUBLELINE = $4000;
  FORMAT_LINES      = $C000;
  //DrawTextAt XPos 16-30 bits  (max value = ~64000)
  FORMAT_XPOS_MASK  = $FFFF0000;

  PAPERSIZE_A4_WIDTH = 210;
  PAPERSIZE_A4_HEIGHT = 297;

procedure SetCurrentPrinterAsDefault;
function CurrentPrinterName: string;
function CurrentPrinterPaperSize: string;
procedure UseDefaultPrinter;

procedure Register;


{ ****************** TGdiPages Report Engine }

const
  MAXCOLS = 20;
  MAXTABS = 20;

  /// this constant can be used to be replaced by the page number in
  // the middle of any text
  PAGENUMBER = '<<pagenumber>>';

type
  /// Exception class raised by this unit
  EReport = class(ESynException);

  /// text paragraph alignment
  TTextAlign = (
    taLeft,
    taRight,
    taCenter,
    taJustified);

  /// text column alignment
  TColAlign = (
    caLeft,
    caRight,
    caCenter,
    caCurrency);

  /// text line spacing
  TLineSpacing = (
    lsSingle,
    lsOneAndHalf,
    lsDouble);

  /// available zoom mode
  // - zsPercent is used with a zoom percentage (e.g. 100% or 50%)
  // - zsPageFit fits the page to the report
  // - zsPageWidth zooms the page to fit the report width on screen
  TZoomStatus = (
    zsPercent,
    zsPageFit,
    zsPageWidth);

  /// Event triggered when a new page is added
  TNewPageEvent = procedure(Sender: TObject; PageNumber: integer) of object;

  /// Event triggered when the Zoom was changed
  TZoomChangedEvent = procedure(Sender: TObject;
    Zoom: integer; ZoomStatus: TZoomStatus) of object;

  /// Event triggered to allow custom unicode character display on the screen
  // - called for all text, whatever the alignment is
  // - Text content can be modified by this event handler to customize
  // some characters (e.g. '>=' can be converted to the one Unicode glyph)
  TOnStringToUnicodeEvent = function(const Text: SynUnicode): SynUnicode of object;

  /// available known paper size for NewPageLayout() method
  TGdiPagePaperSize = (
    psA4,
    psA5,
    psA3,
    psLetter,
    psLegal);

  TGdiPages = class;

  /// a report layout state, as used by SaveLayout/RestoreSavedLayout methods
  TSavedState = record
    FontName: string;
    FontColor: integer;
    Flags: integer;
    LeftMargin: integer;
    RightMargin: integer;
    BiDiMode: TBiDiMode;
  end;

  /// internal format of the header or footer text
  THeaderFooter = class
  public
    Text: SynUnicode;
    State: TSavedState;
    /// initialize the header or footer parameters with current report state
    constructor Create(Report: TGdiPages; doubleline: boolean;
      const aText: SynUnicode = ''; IsText: boolean = false);
  end;

  /// internal format of a text column
  TColRec = record
    ColLeft, ColRight: integer;
    ColAlign: TColAlign;
    ColBold: boolean;
  end;

  TPopupMenuClass = class of TPopupMenu;

  /// hack the TPaintBox to allow custom background erase
  TPagePaintBox = class(TPaintBox)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  end;

  /// internal structure used to store bookmarks or links
  TGdiPageReference = class
  public
    /// the associated page number (starting at 1)
    Page: integer;
    /// graphical coordinates of the hot zone
    // - for bookmarks, Top is the Y position
    // - for links, the TRect will describe the hot region
    // - for Outline, Top is the Y position and Bottom the outline tree level
    Rect: TRect;
    /// coordinates on screen of the hot zone
    Preview: TRect;
    /// initialize the structure with the current page
    constructor Create(PageNumber: integer; Left, Top, Right, Bottom: integer);
    /// compute the coordinates on screen into Preview
    procedure ToPreview(Pages: TGdiPages);
  end;

  /// contains one page
  TGdiPageContent = record
    /// SynLZ-compressed content of the page
    MetaFileCompressed: RawByteString;
    /// text equivalent of the page
    Text: string;
    /// the physical page size
    SizePx: TPoint;
    /// margin of the page
    MarginPx: TRect;
    /// non printable offset of the page
    OffsetPx: TPoint;
  end;

  /// used to store all pages of the report
  TGdiPageContentDynArray = array of TGdiPageContent;

  /// the available menu items
  TGdiPagePreviewButton = (
    rNone,
    rNextPage,
    rPreviousPage,
    rGotoPage,
    rZoom,
    rBookmarks,
    rPageAsText,
    rPrint,
    rExportPdf,
    rClose);

  /// set of menu items
  TGdiPagePreviewButtons = set of TGdiPagePreviewButton;

  /// Report class for generating documents from code
  // - data is drawn in memory, they displayed or printed as desired
  // - allow preview and printing, and direct pdf export
  // - handle bookmark, outlines and links inside the document
  // - page coordinates are in mm's
  TGdiPages = class(TScrollBox)
  protected
    fPreviewSurface: TPagePaintbox;
    fCanvas: TMetaFileCanvas;
    fCanvasText: string;
    fBeforeGroupText: string;
    fGroupPage: TMetaFile;
    fPages: TGdiPageContentDynArray;
    fHeaderLines: TObjectList;
    fFooterLines: TObjectList;
    fColumns: array of TColRec;
    fColumnHeaderList: array of record
      headers: TSynUnicodeDynArray;
      flags: integer;
    end;
    {$ifdef MOUSE_CLICK_PERFORM_ZOOM}
    fZoomTimer: TTimer;
    {$endif MOUSE_CLICK_PERFORM_ZOOM}
    fPtrHdl: THandle;
    fTabCount: integer;
    fCurrentPrinter: string;
    fOrientation: TPrinterOrientation;
    fDefaultLineWidth: integer;        //drawing line width (boxes etc)
    fVirtualPageNum: integer;
    fCurrPreviewPage: integer;
    fZoomIn: boolean;
    fLineHeight: integer;              //Text line height
    fLineSpacing: TLineSpacing;
    fCurrentYPos: integer;
    fCurrentTextTop, fCurrentTextPage: integer;
    fHeaderHeight: integer;
    fHangIndent: integer;
    fAlign: TTextAlign;
    fBiDiMode: TBiDiMode;
    fPageMarginsPx: TRect;
    fHasPrinterInstalled: boolean;
    {$ifdef USEPDFPRINTER}
    fHasPDFPrinterInstalled: boolean;
    fPDFPrinterIndex: integer;
    {$else}
    fForceJPEGCompression: integer;
    fExportPdfApplication: string;
    fExportPdfAuthor: string;
    fExportPdfSubject: string;
    fExportPdfKeywords: string;
    fExportPdfEmbeddedTTF: boolean;
    fExportPdfLevel: TPdfALevel;
    fExportPdfBackground: TGraphic;
    {$ifndef NO_USE_UNISCRIBE}
    fExportPdfUseUniscribe: boolean;
    {$endif NO_USE_UNISCRIBE}
    fExportPdfUseFontFallBack: boolean;
    fExportPdfFontFallBackName: string;
    fExportPdfEncryptionLevel: TPdfEncryptionLevel;
    fExportPdfEncryptionUserPassword: string;
    fExportPdfEncryptionOwnerPassword: string;
    fExportPdfEncryptionPermissions: TPdfEncryptionPermissions;
    fExportPdfGeneratePdf15File: boolean;
    {$endif USEPDFPRINTER}
    fPrinterPxPerInch: TPoint;
    fPhysicalSizePx: TPoint;           //size of page in printer pixels
    fPhysicalOffsetPx: TPoint;         //size of non-printing margins in pixels
    fCustomPxPerInch: TPoint;
    fCustomPageSize: TPoint;
    fCustomNonPrintableOffset: TPoint;
    fCustomPageMargins: TRect;
    fZoom: integer;
    fZoomStatus: TZoomStatus;
    fNegsToParenthesesInCurrCols: boolean;
    fWordWrapLeftCols: boolean;
    fUseOutlines: boolean;
    fForceScreenResolution: boolean;
    fHeaderDone: boolean;
    fFooterHeight: integer;
    fFooterGap: integer;
    fInHeaderOrFooter: boolean;
    fColumnHeaderPrinted: boolean;
    fColumnHeaderPrintedAtLeastOnce: boolean;
    fDrawTextAcrossColsDrawingHeader: boolean;
    fColumnHeaderInGroup: boolean;
    fColumnsUsedInGroup: boolean;
    fGroupVerticalSpace: integer;
    fGroupVerticalPos: integer;
    fZoomChangedEvent: TZoomChangedEvent;
    fPreviewPageChangedEvent: TNotifyEvent;
    fStartNewPage: TNewPageEvent;
    fStartPageHeader: TNotifyEvent;
    fEndPageHeader: TNotifyEvent;
    fStartPageFooter: TNotifyEvent;
    fEndPageFooter: TNotifyEvent;
    fStartColumnHeader: TNotifyEvent;
    fEndColumnHeader: TNotifyEvent;
    fSavedCount: integer;
    fSaved: array of TSavedState;
    fTab: array of integer;
    fColumnsWithBottomGrayLine: boolean;
    fColumnsRowLineHeight: integer;
    fOnDocumentProducedEvent: TNotifyEvent;
    PageRightButton, PageLeftButton: TPoint;
    fPagesToFooterText: string; // not SynUnicode, since calls format()
    fPagesToFooterAt: TPoint;
    fPagesToFooterState: TSavedState;
    fMetaFileForPage: TMetaFile;
    fCurrentMetaFile: TMetaFile;

    procedure GetPrinterParams;
    procedure SetAnyCustomPagePx;
    function  GetPaperSize: TSize;
    procedure FlushPageContent;
    function  PrinterPxToScreenPxX(PrinterPx: integer): integer;
    function  PrinterPxToScreenPxY(PrinterPx: integer): integer;
    procedure ResizeAndCenterPaintbox;
    function GetMetaFileForPage(PageIndex: integer): TMetaFile;
    procedure SetMetaFileForPage(PageIndex: integer; MetaFile: TMetaFile);

    function  GetOrientation: TPrinterOrientation;
    procedure SetOrientation(orientation: TPrinterOrientation);
    procedure SetTextAlign(Value: TTextAlign);
    procedure SetPage(NewPreviewPage: integer);
    function  GetPageCount: integer;
    function  GetLineHeight: integer;
    function  GetLineHeightMm: integer;
    procedure CheckYPos;               //ie: if not vertical room force new page
    function  GetYPos: integer;
    procedure SetYPos(YPos: integer);
    procedure NewPageInternal; virtual;
    function  CreateMetaFile(aWidth, aHeight: integer): TMetaFile;
    function  CreateMetafileCanvas(Page: TMetaFile): TMetaFileCanvas;
    procedure UpdateMetafileCanvasFont(aCanvas: TMetaFileCanvas);
    function  TextFormatsToFlags: integer;
    procedure SetFontWithFlags(flags: integer);
    function  GetPageMargins: TRect;
    procedure SetPageMargins(Rect: TRect);

    procedure DoHeader;
    procedure DoFooter;
    procedure DoHeaderFooterInternal(Lines: TObjectList);
    procedure CalcFooterGap;

    function  GetColumnCount: integer;
    function  GetColumnRec(col: integer): TColRec;
    procedure PrintColumnHeaders;

    procedure SetZoom(zoom: integer);
    procedure SetZoomStatus(aZoomStatus: TZoomStatus);
    procedure ZoomTimerInternal(X,Y: integer; ZoomIn: boolean);
    procedure ZoomTimer(Sender: TObject);

    procedure LineInternal(start, finish : integer; doubleline : boolean); overload;
    procedure LineInternal(aty, start, finish : integer; doubleline : boolean); overload;
    procedure PrintFormattedLine(s: SynUnicode; flags: integer;
      const aBookmark: string = ''; const aLink: string = '';
      withNewLine: boolean = true; aLinkNoBorder: boolean = false);
    procedure LeftOrJustifiedWrap(const s: SynUnicode; withNewLine: boolean = true);
    procedure RightOrCenterWrap(const s: SynUnicode);
    procedure GetTextLimitsPx(var LeftOffset, RightOffset: integer);
    procedure HandleTabsAndPrint(const leftstring: SynUnicode;
      var rightstring: SynUnicode; leftOffset, rightOffset: integer);
    procedure PreviewPaint(Sender: TObject);
    procedure PreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    function GetLeftMargin: integer;
    procedure SetLeftMargin(const Value: integer);
    function GetRightMarginPos: integer;
    function GetSavedState: TSavedState;
    procedure SetSavedState(const SavedState: TSavedState);
    /// can be used internaly (for instance by fPagesToFooterState)
    property SavedState: TSavedState
      read GetSavedState write SetSavedState;
  protected
    fMousePos: TPoint;
    {$ifndef MOUSE_CLICK_PERFORM_ZOOM}
    fButtonDown, fButtonDownScroll: TPoint;
    {$endif MOUSE_CLICK_PERFORM_ZOOM}
    /// Strings[] are the bookmark names, and Objects[] are TGdiPageReference
    // to get the Y position
    fBookmarks: TStringList;
    /// Strings[] are the bookmark names, and Objects[] are TGdiPageReference to
    // get the hot region
    fLinks: TStringList;
    fLinksCurrent: integer;
    /// Strings[] are the outline titles, and Objects[] are TGdiPageReference
    // to get the Y position of the destination
    fOutline: TStringList;
    fInternalUnicodeString: SynUnicode;
    fForcedLeftOffset : integer;
    PreviewForm: TForm;
    PreviewButtons: array of TButton;
    PreviewPageCountLabel: TLabel;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    {$ifndef FPC}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override; //no mousewheel support in Delphi 3
    {$endif FPC}
    procedure PopupMenuPopup(Sender: TObject);
    procedure CheckHeaderDone; virtual;
    // warning: PW buffer is overwritten at the next method call
    procedure InternalUnicodeString(const s: SynUnicode;
      var PW: PWideChar; var PWLen: integer; size: PSize);
  public
    /// Event triggered when the ReportPopupMenu is displayed
    // - default handling (i.e. leave this field nil) is to add Page naviguation
    // - you can override this method for adding items to the ReportPopupMenu
    OnPopupMenuPopup: TNotifyEvent;
    /// Event triggered when a ReportPopupMenu item is selected
    // - default handling (i.e. leave this field nil) is for Page navigation
    // - you can override this method for handling additionnal items to the menu
    // - the Tag component of the custom TMenuItem should be 0 or greater than
    // Report pages count: use 1000 as a start for custom TMenuItem.Tag values
    OnPopupMenuClick: TNotifyEvent;
    /// user can customize this class to create an advanced popup menu instance
    PopupMenuClass: TPopupMenuClass;
    /// the title of the report
    // - used for the preview caption form
    // - used for the printing document name
    Caption: string;
    /// if true, the PrintPages() method will use a temporary bitmap for printing
    // - some printer device drivers have problems with printing metafiles
    // which contains other metafiles; should have been fixed
    // - not useful, since slows the printing a lot and makes huge memory usage
    ForcePrintAsBitmap: boolean;
    /// if true the preview will not use GDI+ library to draw anti-aliaised graphics
    // - this may be slow on old computers, so caller can disable it on demand
    ForceNoAntiAliased: boolean;
    /// refine how GDI+ library rendering is done
    AntiAliasedOptions: TEmfConvertOptions;

    /// if true, the headers are copied only once to the text
    ForceCopyTextAsWholeContent: boolean;
    /// customize text conversion before drawing
    // - Text content can be modified by this event handler to customize
    // some characters (e.g. '>=' can be converted to its Unicode glyph)
    OnStringToUnicode: TOnStringToUnicodeEvent;
    /// set group page fill method
    // - if set to true, the groups will be forced to be placed on the same page
    // (this was the original default "Pages" component behavior, but this
    // is not usual in page composition, so is disabled by default in TGdiPages)
    // - if set to false, the groups will force a page feed if there is not
    // enough place for 20 lines on the current page (default behavior)
    GroupsMustBeOnSamePage: boolean;
    /// the bitmap used to draw the page
    PreviewSurfaceBitmap: TBitmap;

    /// creates the reporting component
    constructor Create(AOwner: TComponent); override;
    /// finalize the component, releasing all used memory
    destructor Destroy; override;
    /// customized invalidate
    procedure Invalidate; override;

    /// Begin a Report document
    // - Every report must start with BeginDoc and end with EndDoc
    // - note that Printers.SetPrinter() should be set BEFORE calling BeginDoc,
    // otherwise you may have a "canvas does not allow drawing" error
    procedure BeginDoc;
    /// Clear the current Report document
    procedure Clear; virtual;
    /// draw some text as a paragraph, with the current alignment
    // - this method does all word-wrapping and formating if necessary
    // - this method handle multiple paragraphs inside s (separated by newlines -
    // i.e. #13)
    // - by default, will write a paragraph, unless withNewLine is set to false,
    // so that the next DrawText() will continue drawing at the current position
    procedure DrawText(const s: string; withNewLine: boolean = true);
      {$ifdef HASINLINE}inline;{$endif}
    /// draw some UTF-8 text as a paragraph, with the current alignment
    // - this method does all word-wrapping and formating if necessary
    // - this method handle multiple paragraphs inside s (separated by newlines -
    // i.e. #13)
    // - by default, will write a paragraph, unless withNewLine is set to false,
    // so that the next DrawText() will continue drawing at the current position
    procedure DrawTextU(const s: RawUtf8; withNewLine: boolean = true);
      {$ifdef HASINLINE}inline;{$endif}
    /// draw some Unicode text as a paragraph, with the current alignment
    // - this method does all word-wrapping and formating if necessary
    // - this method handle multiple paragraphs inside s (separated by newlines -
    // i.e. #13)
    // - by default, will write a paragraph, unless withNewLine is set to false,
    // so that the next DrawText() will continue drawing at the current position
    procedure DrawTextW(const s: SynUnicode; withNewLine: boolean = true);
    /// draw some text as a paragraph, with the current alignment
    // - this method use format() like parameterss
    procedure DrawTextFmt(const s: string; const Args: array of const;
      withNewLine: boolean = true);
    /// get the formating flags associated to a Title
    function TitleFlags: integer;
    /// draw some text as a paragraph title
    // - the outline level can be specified, if UseOutline property is enabled
    // - if aBookmark is set, a bookmark is created at this position
    // - if aLink is set, a link to the specified bookmark name (in aLink) is made
    procedure DrawTitle(const s: SynUnicode; DrawBottomLine: boolean = false;
      OutlineLevel: integer = 0; const aBookmark: string = '';
      const aLink: string = ''; aLinkNoBorder: boolean = false);
    /// draw one line of text, with the current alignment
    procedure DrawTextAt(s: SynUnicode; XPos: integer; const aLink: string = '';
      CheckPageNumber: boolean = false; aLinkNoBorder: boolean = false);
    /// draw one line of text, with a specified Angle and X Position
    procedure DrawAngledTextAt(const s: SynUnicode; XPos, Angle: integer);
    /// draw a square box at the given coordinates
    procedure DrawBox(left,top,right,bottom: integer);
    /// draw a filled square box at the given coordinates
    procedure DrawBoxFilled(left,top,right,bottom: integer; Color: TColor);
    /// Stretch draws a bitmap image at the specified page coordinates in mm's
    procedure DrawBmp(rec: TRect; bmp: TBitmap); overload;
    /// add the bitmap at the specified X position
    // - if there is not enough place to draw the bitmap, go to next page
    // - then the current Y position is updated
    // - bLeft (in mm) is calculated in reference to the LeftMargin position
    // - if bLeft is maxInt, the bitmap is centered to the page width
    // - bitmap is stretched (keeping aspect ratio) for the resulting width to
    // match the bWidth parameter (in mm)
    procedure DrawBmp(bmp: TBitmap; bLeft, bWidth: integer;
      const Legend: string = ''); overload;
    /// Stretch draws a metafile image at the specified page coordinates in mm's
    procedure DrawMeta(rec: TRect; meta: TMetaFile);
    /// add the graphic (bitmap or metafile) at the specified X position
    // - handle only TBitmap and TMetaFile kind of TGraphic
    // - if there is not enough place to draw the bitmap, go to next page
    // - then the current Y position is updated
    // - bLeft (in mm) is calculated in reference to the LeftMargin position
    // - if bLeft is maxInt, the bitmap is centered to the page width
    // - bitmap is stretched (keeping aspect ratio) for the resulting width to
    // match the bWidth parameter (in mm)
    procedure DrawGraphic(graph: TGraphic; bLeft, bWidth: integer;
      const Legend: SynUnicode = '');
    /// draw an Arrow
    procedure DrawArrow(Point1, Point2: TPoint; HeadSize: integer; SolidHead: boolean);
    /// draw a Line, either simple or double, between the left & right margins
    procedure DrawLine(doubleline: boolean = false);
    /// draw a Dashed Line between the left & right margins
    procedure DrawDashedLine;
    /// draw a Line, following a column layout
    procedure DrawColumnLine(ColIndex: integer; aAtTop: boolean;
      aDoDoubleLine: boolean);
    /// append a Rich Edit content to the current report
    // - note that if you want the TRichEdit component to handle more than 64 KB
    // of RTF content, you have to set its MaxLength property as expected (this
    // is a limitation of the VCL, not of this method)
    // - you can specify optionally a pointer to a TIntegerDynArray variable,
    // which will be filled with the position of each page last char: it may
    // be handy e.g. to add some cross-reference table about the rendered content
    procedure AppendRichEdit(RichEditHandle: HWnd;
      EndOfPagePositions: PIntegerDynArray = nil);
    /// jump some line space between paragraphs
    // - Increments the current Y Position the equivalent of a single line
    // relative to the current font height and line spacing
    procedure NewLine;
    /// jump some half line space between paragraphs
    // - Increments the current Y Position the equivalent of an half single line
    // relative to the current font height and line spacing
    procedure NewHalfLine;
    /// jump some line space between paragraphs
    // - Increments the current Y Position the equivalent of 'count' lines
    // relative to the current font height and line spacing
    procedure NewLines(count: integer);
    /// save the current font and alignment
    procedure SaveLayout; virtual;
    /// restore last saved font and alignment
    procedure RestoreSavedLayout; virtual;
    /// jump to next page, i.e. force a page break
    procedure NewPage(ForceEndGroup: boolean = false);
    /// jump to next page, but only if some content is pending
    procedure NewPageIfAnyContent;
    /// change the page layout for the upcoming page
    // - will then force a page break by a call to NewPage(true) method
    // - can change the default margin if margin*>=0
    // - can change the default non-printable printer margin if nonPrintable*>=0
    procedure NewPageLayout(sizeWidthMM, sizeHeightMM: integer;
      nonPrintableWidthMM: integer = -1; nonPrintableHeightMM: integer = -1); overload;
    /// change the page layout for the upcoming page
    // - will then force a page break by a call to NewPage(true) method
    // - can change the default margin if margin*>=0
    // - can change the default non-printable printer margin if nonPrintable*>=0
    procedure NewPageLayout(paperSize: TGdiPagePaperSize;
      orientation: TPrinterOrientation = poPortrait;
      nonPrintableWidthMM: integer = -1; nonPrintableHeightMM: integer = -1); overload;
    /// begin a Group: stops the contents from being split across pages
    // - BeginGroup-EndGroup text blocks can't be nested
    procedure BeginGroup;
    /// end a previously defined Group
    // - BeginGroup-EndGroup text blocks can't be nested
    procedure EndGroup;
    /// End the Report document
    // - Every report must start with BeginDoc and end with EndDoc
    procedure EndDoc;
    /// Print the selected pages to the default printer of Printer unit
    // - if PrintFrom=0 and PrintTo=0, then all pages are printed
    // - if PrintFrom=-1 or PrintTo=-1, then a printer dialog is displayed
    function PrintPages(PrintFrom, PrintTo: integer): boolean;
    /// export the current report as PDF file
    {$ifdef USEPDFPRINTER}
    // - uses an external 'PDF' printer
    {$else}
    // - uses internal PDF code, from Synopse PDF engine (handle bookmarks,
    // outline and twin bitmaps) - in this case, a file name can be set
    {$endif USEPDFPRINTER}
    function ExportPdf(aPdfFileName: TFileName; ShowErrorOnScreen: boolean;
      LaunchAfter: boolean = true): boolean;
    {$ifndef USEPDFPRINTER}
    /// export the current report as PDF in a specified stream
    // - uses internal PDF code, from Synopse PDF engine (handle bookmarks,
    // outline and twin bitmaps) - in this case, a file name can be set
    function ExportPdfStream(aDest: TStream): boolean;
    {$endif USEPDFPRINTER}
    /// show a form with the preview, allowing the user to browse pages and
    // print the report
    // - you can customize the buttons and popup menu actions displayed on
    // the screen - by default, all buttons are visible
    procedure ShowPreviewForm(VisibleButtons: TGdiPagePreviewButtons =
      [rNextPage..High(TGdiPagePreviewButton)]);

    /// set the Tabs stops on every line
    // - if one value is provided, it will set the Tabs as every multiple of it
    // - if more than one value are provided, they will be the exact Tabs positions
    procedure SetTabStops(const tabs: array of integer);
    /// returns true if there is enough space in the current Report for Count lines
    // - Used to check if there's sufficient vertical space remaining on the page
    // for the specified number of lines based on the current Y position
    function  HasSpaceForLines(Count: integer): boolean;
    /// returns true if there is enough space in the current Report for a
    // vertical size, specified in mm
    function  HasSpaceFor(mm: integer): boolean;

    /// Clear all already predefined Headers
    procedure ClearHeaders;
    /// Adds either a single line or a double line (drawn between the left &
    // right page margins) to the page header
    procedure AddLineToHeader(doubleline: boolean);
    /// Adds text using to current font and alignment to the page header
    procedure AddTextToHeader(const s: SynUnicode);
    /// Adds text to the page header at the specified horizontal position and
    // using to current font.
    // - No Line feed will be triggered: this method doesn't increment the YPos,
    // so can be used to add multiple text on the same line
    // - if XPos=-1, will put the text at the current right margin
    procedure AddTextToHeaderAt(const s: SynUnicode; XPos: integer);

    /// Clear all already predefined Footers
    procedure ClearFooters;
    /// Adds either a single line or a double line (drawn between the left &
    // right page margins) to the page footer
    procedure AddLineToFooter(doubleline: boolean);
    /// Adds text using to current font and alignment to the page footer
    procedure AddTextToFooter(const s: SynUnicode);
    /// Adds text to the page footer at the specified horizontal position and
    // using to current font. No Line feed will be triggered.
    // - if XPos=-1, will put the text at the current right margin
    procedure AddTextToFooterAt(const s: SynUnicode; XPos: integer);
    /// Will add the current 'Page n/n' text at the specified position
    // - PageText must be of format 'Page %d/%d', in the desired language
    // - if XPos=-1, will put the text at the current right margin
    // - if the vertical position does not fit your need, you could set
    // YPosMultiplier to a value which will be multipled by fFooterHeight to
    // compute the YPos
    procedure AddPagesToFooterAt(const PageText: string; XPos: integer;
      YPosMultiplier: integer=1);

    /// register a column, with proper alignment
    procedure AddColumn(left, right: integer; align: TColAlign; bold: boolean);
    /// register same alignement columns, with percentage of page column width
    // - sum of all percent width should be 100, but can be of any value
    // - negative widths are converted into absolute values, but
    // corresponding alignment is set to right
    // - if a column need to be right aligned or currency aligned,
    // use SetColumnAlign() method below
    // - individual column may be printed in bold with SetColumnBold() method
    procedure AddColumns(const PercentWidth: array of integer;
      align: TColAlign = caLeft);
    /// register some column headers, with the current font formating
    // - Column headers will appear just above the first text output in
    // columns on each page
    // - you can call this method several times in order to have diverse
    // font formats across the column headers
    procedure AddColumnHeaders(const headers: array of SynUnicode;
      WithBottomGrayLine: boolean = false; BoldFont: boolean = false;
      RowLineHeight: integer = 0; flags: integer = 0);
    /// register some column headers, with the current font formating
    // - Column headers will appear just above the first text output in
    // columns on each page
    // - call this method once with all columns text as Csv
    procedure AddColumnHeadersFromCsv(var Csv: PWideChar;
      WithBottomGrayLine: boolean; BoldFont: boolean = false;
      RowLineHeight: integer = 0);
    /// draw some text, split across every columns
    // - if BackgroundColor is not clNone (i.e. clRed or clNavy or clBlack), the
    // row is printed on white with this background color (e.g. to highlight errors)
    procedure DrawTextAcrossCols(const StringArray: array of SynUnicode;
      BackgroundColor: TColor = clNone); overload;
    /// draw some text, split across every columns
    // - you can specify an optional bookmark name to be used to link a column
    // content via a AddLink() call
    // - if BackgroundColor is not clNone (i.e. clRed or clNavy or clBlack), the
    // row is printed on white with this background color (e.g. to highlight errors)
    procedure DrawTextAcrossCols(const StringArray, LinkArray: array of SynUnicode;
      BackgroundColor: TColor = clNone); overload;
    /// draw some text, split across every columns
    // - this method expect the text to be separated by commas
    // - if BackgroundColor is not clNone (i.e. clRed or clNavy or clBlack), the
    // row is printed on white with this background color (e.g. to highlight errors)
    procedure DrawTextAcrossColsFromCsv(var Csv: PWideChar;
      BackgroundColor: TColor = clNone);
    /// draw (double if specified) lines at the bottom of all currency columns
    procedure DrawLinesInCurrencyCols(doublelines: boolean);

    /// retrieve the current Column count
    property ColumnCount: integer
      read GetColumnCount;
    /// retrieve the attributes of a specified column
    function GetColumnInfo(index: integer): TColRec;
    /// individually set column alignment
    // - useful after habing used AddColumns([]) method e.g.
    procedure SetColumnAlign(index: integer; align: TColAlign);
    /// individually set column bold state
    // - useful after habing used AddColumns([]) method e.g.
    procedure SetColumnBold(index: integer);
    /// erase all columns and the associated headers
    procedure ClearColumns;
    /// clear the Headers associated to the Columns
    procedure ClearColumnHeaders;
    /// ColumnHeadersNeeded will force column headers to be drawn again just
    // prior to printing the next row of columned text
    // - Usually column headers are drawn once per page just above the first
    // column. ColumnHeadersNeeded is useful where columns of text have been
    // separated by a number of lines of non-columned text
    procedure ColumnHeadersNeeded;

    /// create a bookmark entry at the current position of the current page
    // - return false if this bookmark name was already existing, true on success
    // - if aYPosition is not 0, the current Y position will be used
    function AddBookMark(const aBookmarkName: string;
      aYPosition: integer = 0): boolean; virtual;
    /// go to the specified bookmark
    // - returns true if the bookmark name was existing and reached
    function GotoBookmark(const aBookmarkName: string): boolean; virtual;
    /// create an outline entry at the current position of the current page
    // - if aYPosition is not 0, the current Y position will be used
    procedure AddOutline(const aTitle: string; aLevel: integer;
      aYPosition: integer = 0; aPageNumber: integer = 0); virtual;
    /// create a link entry at the specified coordinates of the current page
    // - coordinates are specified in mm
    // - the bookmark name is not checked by this method: a bookmark can be
    // linked before being marked in the document
    procedure AddLink(const aBookmarkName: string; aRect: TRect;
      aPageNumber: integer = 0; aNoBorder: boolean = false); virtual;

    /// convert a rect of mm into pixel canvas units
    function MmToPrinter(const R: TRect): TRect;
    /// convert a rect of pixel canvas units into mm
    function PrinterToMM(const R: TRect): TRect;
    /// convert a mm X position into pixel canvas units
    function MmToPrinterPxX(mm: integer): integer;
    /// convert a mm Y position into pixel canvas units
    function MmToPrinterPxY(mm: integer): integer;
    /// convert a pixel canvas X position into mm
    function PrinterPxToMmX(px: integer): integer;
    /// convert a pixel canvas Y position into mm
    function PrinterPxToMmY(px: integer): integer;
    /// return the width of the specified text, in mm
    function TextWidth(const Text: SynUnicode): integer;
    /// the current Text Alignment, during text adding
    property TextAlign: TTextAlign
      read fAlign write SetTextAlign;
    /// specifies the reading order (bidirectional mode) of the box
    // - only bdLeftToRight and bdRightToLeft are handled
    // - this will be used by DrawText[At], DrawTitle, AddTextToHeader/Footer[At],
    // DrawTextAcrossCols, SaveLayout/RestoreSavedLayout methods
    property BiDiMode: TBiDiMode
      read fBiDiMode write fBiDiMode;
    /// create a meta file and its associated canvas for displaying a picture
    // - you must release manually both Objects after usage
    function CreatePictureMetaFile(Width, Height: integer;
      out MetaCanvas: TCanvas): TMetaFile;
    /// Distance (in mm's) from the top of the page to the top of the current group
    // - returns CurrentYPos if no group is in use
    function CurrentGroupPosStart: integer;
    /// go to the specified Y position on a given page
    // - used e.g. by GotoBookmark() method
    procedure GotoPosition(aPage: integer; aYPos: integer);
    /// access to all pages content
    // - numerotation begin with Pages[0] for page 1
    // - the Pages[] property should be rarely needed
    property Pages: TGdiPageContentDynArray
      read fPages;
    /// add an item to the popup menu
    // - used mostly internaly to add page browsing
    // - default OnClick event is to go to page set by the Tag property
    function NewPopupMenuItem(const aCaption: string; Tag: integer = 0;
      SubMenu: TMenuItem = nil; OnClick: TNotifyEvent = nil;
      ImageIndex: integer = -1): TMenuItem;
    /// this is the main popup menu item click event
    procedure PopupMenuItemClick(Sender: TObject);
    /// can be used to draw directly using GDI commands
    // - The Canvas property should be rarely needed
    property Canvas: TMetaFileCanvas
      read fCanvas;
    /// Distance (in mm's) from the top of the page to the top of the next line
    property CurrentYPos: integer
      read GetYPos write SetYPos;
    /// get current line height (mm)
    property LineHeight: integer
      read GetLineHeightMm;
    /// the name of the current selected printer
    // - note that Printers.SetPrinter() should be set BEFORE calling BeginDoc,
    // otherwise you may have a "canvas does not allow drawing" error
    property PrinterName: string
      read fCurrentPrinter;
    /// the index of the previewed page
    // - please note that the first page is 1 (not 0)
    property Page: integer
      read fCurrPreviewPage write SetPage;
    /// total number of pages
    property PageCount: integer
      read GetPageCount;
    /// Size of each margin relative to its corresponding edge in mm's
    property PageMargins: TRect
      read GetPageMargins write SetPageMargins;
    /// Size of the left margin relative to its corresponding edge in mm's
    property LeftMargin: integer
      read GetLeftMargin write SetLeftMargin;
    /// Position of the right margin, in mm
    property RightMarginPos: integer
      read GetRightMarginPos;
    /// get the current selected paper size, in mm's
    property PaperSize: TSize
      read GetPaperSize;
    /// number of pixel per inch, for X and Y directions
    property PrinterPxPerInch: TPoint
      read fPrinterPxPerInch;
    {$ifdef USEPDFPRINTER}
    /// true if any printer appears to be a PDF printer
    property HasPDFPrinterInstalled: boolean
      read fHasPDFPrinterInstalled;
    {$else}
    /// this property can force saving all bitmaps as JPEG in exported PDF
    // - by default, this property is set to 0 by the constructor of this class,
    // meaning that the JPEG compression is not forced, and the engine will use
    // the native resolution of the bitmap - in this case, the resulting
    // PDF file content will be bigger in size (e.g. use this for printing)
    // - 60 is the prefered way e.g. for publishing PDF over the internet
    // - 80/90 is a good ration if you want to have a nice PDF to see on screen
    // - of course, this doesn't affect vectorial (i.e. emf) pictures
    property ExportPdfForceJPEGCompression: integer
      read fForceJPEGCompression write fForceJPEGCompression;
    /// optional application name used during Export to PDF
    // - if not set, global Application.Title will be used
    property ExportPdfApplication: string
      read fExportPdfApplication write fExportPdfApplication;
    /// optional Author name used during Export to PDF
    property ExportPdfAuthor: string
      read fExportPdfAuthor write fExportPdfAuthor;
    /// optional Subject text used during Export to PDF
    property ExportPdfSubject: string
      read fExportPdfSubject write fExportPdfSubject;
    /// optional Keywords name used during Export to PDF
    property ExportPdfKeywords: string
      read fExportPdfKeywords write fExportPdfKeywords;
    /// if set to true, the used true Type fonts will be embedded to the exported PDF
    // - not set by default, to save disk space and produce tiny PDF
    property ExportPdfEmbeddedTTF: boolean
      read fExportPdfEmbeddedTTF write fExportPdfEmbeddedTTF;
    /// allow to export as PDF compatible with PDF/A requirements
    property ExportPdfLevel: TPdfALevel
      read fExportPdfLevel write fExportPdfLevel;
    /// an optional background image, to be exported on every pdf page
    // - note that no private copy of the TGraphic instance is made: the caller
    // has to manage it, and free it after the pdf is generated
    property ExportPdfBackground: TGraphic
      read fExportPdfBackground write fExportPdfBackground;
    {$ifndef NO_USE_UNISCRIBE}
    /// set if the exporting PDF engine must use the Windows Uniscribe API to
    // render Ordering and/or Shaping of the text
    // - useful for Hebrew, Arabic and some Asiatic languages handling
    // - set to false by default, for faster content generation
    property ExportPdfUseUniscribe: boolean
      read fExportPdfUseUniscribe write fExportPdfUseUniscribe;
    {$endif NO_USE_UNISCRIBE}
    /// used to define if the exported PDF document will handle "font fallback" for
    // characters not existing in the current font: it will avoid rendering
    // block/square symbols instead of the correct characters (e.g. for Chinese text)
    // - will use the font specified by FontFallBackName property to add any
    // Unicode glyph not existing in the currently selected font
    // - default value is true
    property ExportPdfUseFontFallBack: boolean
      read fExportPdfUseFontFallBack write fExportPdfUseFontFallBack;
    /// set the font name to be used for missing characters in exported PDF document
    // - used only if UseFontFallBack is true
    // - default value is 'Arial Unicode MS', if existing
    property ExportPdfFontFallBackName: string
      read fExportPdfFontFallBackName write fExportPdfFontFallBackName;
    /// set encryption level to be used in exporting PDF document
    property ExportPdfEncryptionLevel: TPdfEncryptionLevel
      read fExportPdfEncryptionLevel write fExportPdfEncryptionLevel;
    /// set encryption user password to be used in exporting PDF document
    // - leave it to '' unless you want the user to be asked for this password
    // at document opening
    // - ExportPdfEncryptionLevel = elRC4_40/elRC4_128 expects only ASCII-7 chars
    property ExportPdfEncryptionUserPassword: string
      read fExportPdfEncryptionUserPassword write fExportPdfEncryptionUserPassword;
    /// set encryption owner password to be used in exporting PDF document
    // - it is mandatory to set it to a non void value - by default, is set to
    // 'SynopsePDFEngine' by should be overridden for security
    // - ExportPdfEncryptionLevel = elRC4_40/elRC4_128 expects only ASCII-7 chars
    property ExportPdfEncryptionOwnerPassword: string
      read fExportPdfEncryptionOwnerPassword write fExportPdfEncryptionOwnerPassword;
    /// set encryption Permissions to be used in exporting PDF document
    // - can be either one of the PDF_PERMISSION_ALL / PDF_PERMISSION_NOMODIF /
    // PDF_PERSMISSION_NOPRINT / PDF_PERMISSION_NOCOPY /
    // PDF_PERMISSION_NOCOPYNORPRINT set of options
    // - default value is PDF_PERMISSION_ALL (i.e. no restriction)
    property ExportPdfEncryptionPermissions: TPdfEncryptionPermissions
      read fExportPdfEncryptionPermissions write fExportPdfEncryptionPermissions;
    /// set to true to export in PDF 1.5 format, which may produce smaller files
    property ExportPdfGeneratePdf15File: boolean
      read fExportPdfGeneratePdf15File write fExportPdfGeneratePdf15File;
    {$endif USEPDFPRINTER}
    /// the current page number, during text adding
    // - Page is used during preview, after text adding
    property VirtualPageNum: integer
      read fVirtualPageNum write fVirtualPageNum;
    /// true if any header as been drawn, that is if something is to be printed
    property HeaderDone: boolean
      read fHeaderDone;
{    /// used to set if columns must be delimited at their bottom with a gray line
    property ColumnsWithBottomGrayLine: boolean
      read fColumnsWithBottomGrayLine write fColumnsWithBottomGrayLine; }
  published
    /// accounting standard layout for caCurrency columns:
    // - convert all negative sign into parentheses
    // - using parentheses instead of negative numbers is used in financial
    // statement reporting (see e.g. http://en.wikipedia.org/wiki/Income_statement)
    // - align numbers on digits, not parentheses
    property NegsToParenthesesInCurrCols: boolean
      read fNegsToParenthesesInCurrCols write fNegsToParenthesesInCurrCols;
    /// word wrap (caLeft) left-aligned columns into multiple lines
    // - if the text is wider than the column width, its content
    // is wrapped to the next line
    // - if the text contains some #13/#10 characters, it will be splitted into
    // individual lines
    // - this is disabled by default
    property WordWrapLeftCols: boolean
      read fWordWrapLeftCols write fWordWrapLeftCols;
    /// if set, any DrawTitle() call will create an Outline entry
    // - used e.g. for PDF generation
    // - this is enabled by default
    property UseOutlines: boolean
      read fUseOutlines write fUseOutlines;
    /// left justification hang indentation
    property HangIndent: integer
      read fHangIndent write fHangIndent;
    /// Line spacing: can be lsSingle, lsOneAndHalf or lsDouble
    property LineSpacing: TLineSpacing
      read fLineSpacing write fLineSpacing;
    /// the paper orientation
    property Orientation: TPrinterOrientation
      read GetOrientation write SetOrientation;
    /// the current Zoom value, according to the zoom status
    // - you can use PAGE_WIDTH and PAGE_FIT constants to force the corresponding
    // zooming mode (similar to ZoomStatus property setter)
    // - set this property will work only when the report is already shown
    // in preview mode, not before ShowPreviewForm method call
    property Zoom: integer
      read fZoom write SetZoom;
    /// the current Zoom procedure, i.e. zsPercent, zsPageFit or zsPageWidth
    // - set this property will define the Zoom at PAGE_WIDTH or PAGE_FIT
    // special constant, if needed
    // - set this property will work only when the report is already shown
    // in preview mode, not before ShowPreviewForm method call
    property ZoomStatus: TZoomStatus
      read fZoomStatus write SetZoomStatus;
    /// if set to true, we reduce the precision for better screen display
    property ForceScreenResolution: boolean
      read fForceScreenResolution write fForceScreenResolution;

    /// Event triggered when each new page is created
    property OnNewPage: TNewPageEvent
      read fStartNewPage write fStartNewPage;
    /// Event triggered when each new header is about to be drawn
    property OnStartPageHeader: TNotifyEvent
      read fStartPageHeader write fStartPageHeader;
    /// Event triggered when each header was drawn
    property OnEndPageHeader: TNotifyEvent
      read fEndPageHeader write fEndPageHeader;
    /// Event triggered when each new footer is about to be drawn
    property OnStartPageFooter: TNotifyEvent
      read fStartPageFooter write fStartPageFooter;
    /// Event triggered when each footer was drawn
    property OnEndPageFooter: TNotifyEvent
      read fEndPageFooter write fEndPageFooter;
    /// Event triggered when each new column is about to be drawn
    property OnStartColumnHeader: TNotifyEvent
      read fStartColumnHeader write fStartColumnHeader;
    /// Event triggered when each column was drawn
    property OnEndColumnHeader: TNotifyEvent
      read fEndColumnHeader write fEndColumnHeader;

    /// Event triggered whenever the report document generation is done
    // - i.e. when the EndDoc method has just been called
    property OnDocumentProduced: TNotifyEvent
      read fOnDocumentProducedEvent write fOnDocumentProducedEvent;
    /// Event triggered whenever the current preview page is changed
    property OnPreviewPageChanged: TNotifyEvent
      read fPreviewPageChangedEvent write fPreviewPageChangedEvent;
    /// Event triggered whenever the preview page is zoomed in or out
    property OnZoomChanged: TZoomChangedEvent
      read fZoomChangedEvent write fZoomChangedEvent;
  end;

  
{ ****************** TRenderPages Prototype - unfinished }

{$ifdef RENDERPAGES}
  TRenderPages = class;

  /// a TRenderPages additional layout state
  // - used by the overridden SaveLayout/RestoreSavedLayout methods
  TSavedStateRender = record
    FirstLineIndent: integer;
    Before: integer;
    After: integer;
    RightIndent: integer;
    LeftIndent: integer;
  end;

  PRenderBoxWord = ^TRenderBoxWord;

  /// the internal "Word" box structure used by TRenderBox
  TRenderBoxWord = packed record
    /// offset in the fText[] array
    TextOffset: integer;
    /// PWideChar count starting from fText[TextOffset]
    TextLength: integer;
    /// size on the canvas
    Size: TSize;
    /// used to retrieve associated font attributes
    FontIndex: integer;
    /// space width from current font attribute
    FontSpaceWidth: integer;
    /// number of spaces at the right side of this "Word" box
    SpaceAfterCount: integer;
    /// associated link bookmark name
    // - from fLinksBookMarkName[LinkNumber-1], no link set for 0
    LinkNumber: integer;
  end;

  PRenderBoxLayout = ^TRenderBoxLayout;

  /// the internal "drawing" box structure used by TRenderBox
  // - TRenderBox.InternalRender populate fLayout[] with this structures,
  // ready to be drawn to the document Canvas
  TRenderBoxLayout = packed record
    /// pointer of the words in the fText[] array
    Text: PWideChar;
    /// number of PWideChar starting at Text^
    Length: integer;
    /// layout box X coordinate
    Left: integer;
    /// layout box Y coordinate
    Top: integer;
    /// layout box width (in pixels)
    Width: integer;
    /// layout box height (in pixels) - that is, the line height
    Height: integer;
    /// corresponding rendered line index (starting at 0)
    LineIndex: integer;
    /// used to retrieve associated font attributes and links e.g.
    LastBox: PRenderBoxWord;
    /// length of extra space, in pixels - as used by SetTextJustification()
    BreakExtra: integer;
    /// count of space characters in line of text - as used by SetTextJustification()
    BreakCount: integer;
  end;

  /// used to render a "box" of text
  // - will handle word adding, and formatting for a given width
  // - is used by TRenderPage for a whole paragraph, or a column inside a table
  TRenderBox = class
  protected
    fBiDiMode: TBiDiMode;
    fWidth: integer;
    fHeight: integer;
    /// an internal buffer containing the Unicode text of this box
    fText: array of WideChar;
    fTextLen: integer;
    /// word markers of the current text
    fBox: array of TRenderBoxWord;
    fBoxCount: integer;
    /// InternalRender will fill this ready to be rendered layout array
    fLayout: array of TRenderBoxLayout;
    fLayoutCount: integer;
    fOwner: TRenderPages;
    fOwnerFont: TFont;
    /// associated links: none set for 0, otherwise fLinksBookMarkName[number-1]
    fLinksBookMarkNameCurrent: integer;
    fLinksBookMarkName: array of string;
    /// populate fLayout[] from fBox[] and calculate fHeight
    procedure InternalRender;
    function GetHeight: integer;
    procedure Clear;
  public
    /// initialize the rendering "box"
    constructor Create(Owner: TRenderPages);
    /// add some text at the current position
    // - the text is converted to Unicode before adding (calling
    // Owner.OnStringToUnicode if was defined)
    // - the current Owner Font settings are used for the rendering
    // - warning: this method won't handle control chars (like #13 or #10), but
    // will replace them with a space: it's about the caller to
    procedure AddText(const s: string); overload;
    /// add some text at the current position
    // - the current Owner Font settings are used for the rendering
    // - warning: this method won't handle control chars (like #13 or #10), but
    // will replace them with a space: it's about the caller to
    procedure AddText(PW: PWideChar; PWLen: integer); overload;
    /// format the already inserted text into the TRenderPages owner
    // - this TRenderBox text content will be cleared at the end of this method
    // - you don't have to call it usualy: use Owner.RdrParagraph instead
    // - by default, will render top aligned to the X=Left/Y=Top pixels position
    // - for vertical alignment, specify an height in ForcedHeightBottomCentered
    // then will be centered if ForcedAtBottom=false, or bottom aligned if true
    // - if CurrentPageOnly is true, will only flush the content which will fit on
    // the current page - the fLayout[] array will contain remaining boxes;
    // - if CurrentPageOnly is false, this will flush all content to multiple pages
    procedure Flush(Left, Top: integer; CurrentPageOnly: boolean;
      ForcedHeightBottomCentered: integer; ForcedAtBottom: boolean);
    /// render the text paragraph, but go to the next line
    // - similar to the <br /> HTML tag or the \line RTF command
    procedure NewLine;
    /// mark that an hyperlink must begin at the current position
    // - use e.g. RdrAddText method to add some text for the link
    // - will cancel any previous LinkBegin with no LinkEnd: i.e. no nested
    // links are handled yet (how would want it anyway, in the HTML world?)
    procedure LinkBegin(const aBookmarkName: string);
    /// mark that an hyperlink must begin at the current position
    // - use e.g. RdrAddText method to add some text for the link
    // - return false on error (e.g. no hyperlink previously opened via LinkBegin)
    function LinkEnd: boolean;
    /// reset font (character) formatting properties to a default value
    // - default value have been set by RdrSetCurrentStateAsDefault
    // - if no previous call to RdrSetCurrentStateAsDefault has been made,
    // the font is reset to a 12 point, with no bold/italic/underline attributes
    // - similar to the \plain RTF command
    procedure Plain;
      {$ifdef HASINLINE}inline;{$endif}
    /// reset paragraph formatting properties to a default value
    // - similar to the \pard RTF command
    procedure Pard;
      {$ifdef HASINLINE}inline;{$endif}
    /// reset both paragraph and font formatting properties to a default value
    // - similar to the \pard\plain RTF command
    procedure PardPlain;
      {$ifdef HASINLINE}inline;{$endif}
    /// shortcut to the owner TRenderPages
    property Owner: TRenderPages
      read fOwner;
    /// shortcut to the owner TRenderPages.Font
    property Font: TFont
      read fOwnerFont;
    /// specifies the reading order (bidirectional mode) of the box
    // - only bdLeftToRight and bdRightToLeft are handled
    property BiDiMode: TBiDiMode
      read FBiDiMode write FBiDiMode;
    /// current width of the "box", in pixels
    // - must be set before any call to InternalRender
    property Width: integer
      read fWidth write fWidth;
    /// current resulting height of the "box", in pixels
    // - will be calculated from current text if necessary
    property Height: integer
      read GetHeight;
  end;

  /// Report class specified in high-quality document rendering
  // - this class add some methods for creating a document at the character
  // level (whereas standard TGdiPages allows reporting at paragraph level)
  // - can be used e.g. to render some RTF-like content
  // - column handling is much more sophisticated than AddColumn*() methods
  // - uses the Windows Uniscribe API to handle right-to-left scripting and
  // process complex scripts (like Arabic)
  // - uses internaly some TeX-like algorithms like widows and orphans, and
  // an optional external hyphenation engine (like our hyphen unit)
  TRenderPages = class(TGdiPages)
  protected
    fParagraphFirstLineIndent: integer;
    fParagraphBefore: integer;
    fParagraphAfter: integer;
    fParagraphRightIndent: integer;
    fParagraphLeftIndent: integer;
    fSavedRender: array of TSavedStateRender;
    fDefaultState: TSavedState;
    fDefaultStateRender: TSavedStateRender;
    fRdr: TRenderBox;
    fRdrCol: TObjectList;
    /// an array of TFont, used as cache
    fFontCache: TObjectList;
    fFontCacheSpace: array of TSize;
    procedure RdrPard;
    procedure RdrPardPlain;
    procedure RdrPlain;
    function GetCurrentFontCacheIndex: integer;
    function GetCurrentFontCacheIndexAndSelect: integer;
    function GetSavedRender: TSavedStateRender;
    procedure SetSavedRender(const State: TSavedStateRender);
    /// will close any pending paragraph (\page makes an implicit \par)
    procedure NewPageInternal; override;
  public
    /// will set the current Font and Paragraph properties to be used as default
    // - will be used by RdrPlain and RdrPard methods
    procedure RdrSetCurrentStateAsDefault;
    /// render the text paragraph, and begin a new one
    // - write the paragraph text as specified by all previous calls to the
    // Rdr TRenderBox methods, and begin a new paragraph, using a cleaned
    // TRenderBox instance
    // - will use the current TextAlign property value, and the current value
    // of all Paragraph* properties of this class
    // - similar to the </p> HTML tag or the \par RTF command
    procedure RdrParagraph;
    /// create a new table at the current position
    // - return false on error (e.g. a table was opened but not yet ended)
    function RdrTableBegin(const PercentWidth: array of integer): boolean;
    /// get a particular column
    // - return the 'box' handling the layout of the column: use its
    // AddText/NewLine/Link*/Plain/Pard methods  methods to add some formatted text
    function RdrTableColumn(aColumnIndex: integer): TRenderBox;
      {$ifdef HASINLINE}inline;{$endif}
    /// end a previously opened table
    // - will draw all columns to the documents
    // - return false on error (e.g. a table was not opened)
    function RdrTableEnd: boolean;
    /// the main paragraph 'box' of the document
    // - its AddText/NewLine/Link*/Plain/Pard methods  methods to add some
    // formatted text
    // - the paragraph will be flushed to the main document with the RdrParagraph
    // method will be called
    property Rdr: TRenderBox
      read fRdr;
  public 
    /// creates the reporting component
    constructor Create(AOwner: TComponent); override;
    /// finalize the component, releasing all used memory and associated TRenderBox
    destructor Destroy; override;
    /// Clear the current Report document
    procedure Clear; override;
    /// save the current font and alignment
    // - similar to a { character in some RTF content
    // - this version will save also Paragraph* properties values
    procedure SaveLayout; override;
    /// restore last saved font and alignment
    // - similar to a } character in some RTF content
    // - this version will restore also Paragraph* properties values
    procedure RestoreSavedLayout; override;
  public
    /// current paragraph "space before" spacing (in mm, the default is 0)
    property ParagraphBefore: integer
      read fParagraphBefore write fParagraphBefore;
    /// current paragraph "space after" spacing (in mm, the default is 0)
    property ParagraphAfter: integer
      read fParagraphAfter write fParagraphAfter;
    /// current paragraph first-line indent (in mm, the default is 0)
    property ParagraphFirstLineIndent: integer
      read fParagraphFirstLineIndent write fParagraphFirstLineIndent;
    /// current paragraph left indent (in mm, the default is 0)
    property ParagraphLeftIndent: integer
      read fParagraphLeftIndent write fParagraphLeftIndent;
    /// current paragraph right indent (in mm, the default is 0)
    property ParagraphRightIndent: integer
      read fParagraphRightIndent write fParagraphRightIndent;
  end;
{$endif RENDERPAGES}


implementation


{ ****************** Shared Functions used during Report Rendering }

function TextExtent(Canvas: TCanvas; const Text: SynUnicode; Len: integer = 0): TSize;
begin
  result.cX := 0;
  result.cY := 0;
  if Len = 0 then
    Len := length(Text);
  GetTextExtentPoint32W(Canvas.Handle, pointer(Text), Len, result);
end;

function TextWidthC(Canvas: TCanvas; const Text: SynUnicode): integer;
begin
  result := TextExtent(Canvas, Text).cX;
end;

procedure TextOut(Canvas: TCanvas; X, Y: integer; Text: PWideChar; Len: integer);
  overload;
begin
  ExtTextOutW(
    Canvas.Handle, X, Y, TextFlags(Canvas), nil, Text, Len, nil);
end;

procedure TextOut(Canvas: TCanvas; X, Y: integer; const Text: SynUnicode);
  overload;
begin
  ExtTextOutW(
    Canvas.Handle, X, Y, TextFlags(Canvas), nil, pointer(Text), Length(Text), nil);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TGdiPages]);
end;

function ConvertNegsToParentheses(const ValStr: SynUnicode): SynUnicode;
begin
  result := ValStr;
  if (result = '') or
     (result[1] <> '-') then
    exit;
  result[1] := '(';
  result := result + ')';
end;

function PrinterDriverExists: boolean;
var
  Flags, Count, NumInfo: dword;
  Level: Byte;
begin
  // avoid using fPrinter.printers.Count as this will raise an
  // exception if no printer driver is installed...
  Count := 0;
  try
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
      Level := 4;
    end
    else
    begin
      Flags := PRINTER_ENUM_LOCAL;
      Level := 5;
    end;
    EnumPrinters(Flags, nil, Level, nil, 0, Count, NumInfo);
  except
  end;
  result := (Count > 0);
end;

function RightTrim(const S: SynUnicode): SynUnicode;
var
  i: PtrInt;
begin
  i := Length(S);
  while (i > 0) and
        (ord(S[i]) <= 32) do
    dec(i);
  SetString(result, PWideChar(pointer(S)), i);
end;

function LowerCaseU(const S: SynUnicode): SynUnicode;
var
  i: PtrInt;
begin
  SetString(result, PWideChar(pointer(S)), length(S));
  for i := 0 to length(S) - 1 do
    if PWordArray(result)[i] in [ord('A')..ord('Z')] then
      dec(PWordArray(result)[i], 32);
end;

function Max(a, b: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if a > b then
    result := a
  else
    result := b;
end;

function Min(a, b: integer): integer;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if a < b then
    result := a
  else
    result := b;
end;

procedure UseDefaultPrinter;
begin
  Printers.Printer.PrinterIndex := -1;
end;

function GetDefaultPrinterName: string;
var
  Device: array[byte] of char;
  p, p2: PChar;
begin
  GetProfileString('windows', 'device', '', Device, 255);
  p2 := Device;
  while p2^ = ' ' do
    inc(p2);
  p := p2;
  while not (ord(p2^) in [0, ord(',')]) do
    inc(p2);
  SetLength(result, p2 - p);
  if p2 > p then
    MoveFast(p^, pointer(result)^, p2 - p);
end;

function GetDriverForPrinter(Device: PChar; Driver: PChar): boolean;
var
  PrintHandle: THandle;
  DriverInfo2: PDriverInfo2;
  cnt: dword;
  DriverPath: string;
begin
  result := false;
  if not OpenPrinter(Device, PrintHandle, nil) then
    exit;
  try
    getmem(DriverInfo2, 1024);
    try
      if GetPrinterDriver(PrintHandle, nil, 2, DriverInfo2, 1024, cnt) then
      begin
        DriverPath := changefileext(extractfilename(DriverInfo2.pDriverPath), '');
        strpcopy(Driver, DriverPath);
        result := true;
      end;
    finally
      freemem(DriverInfo2);
    end;
  finally
    ClosePrinter(PrintHandle);
  end;
end;

procedure SetCurrentPrinterAsDefault;
var
  Device, Driver, Port: array[byte] of char;
  DefaultPrinter: string;
  hDeviceMode: THandle;
begin
  DefaultPrinter := GetDefaultPrinterName;
  Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
  if DefaultPrinter = Device then
    exit;
  if Driver[0] = #0 then
    if not GetDriverForPrinter(Device, Driver) then
      exit;  // oops !
  DefaultPrinter := FormatString('%,%,%', [Device, Driver, Port]);
  WriteProfileString('windows', 'device', pointer(DefaultPrinter));
  Device := 'windows';
  SendMessage(HWND_BROADCAST, WM_WININICHANGE, 0, PtrInt(@Device));
end;

function CurrentPrinterName: string;
var
  Device, Driver, Port: array[byte] of char;
  hDeviceMode: THandle;
begin
  Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
  result := trim(Device);
end;

function CurrentPrinterPaperSize: string;
var
  PtrHdl: THandle;
  PtrPPI: TPoint;
  size: TSize;
begin
  try
    PtrHdl := Printer.Handle;
    PtrPPI.x := GetDeviceCaps(PtrHdl, LOGPIXELSX);
    PtrPPI.y := GetDeviceCaps(PtrHdl, LOGPIXELSY);
    size.cx := MulDiv(GetDeviceCaps(PtrHdl, PHYSICALWIDTH), 254, PtrPPI.x * 10);
    size.cy := MulDiv(GetDeviceCaps(PtrHdl, PHYSICALHEIGHT), 254, PtrPPI.y * 10);
  except
  end;
  with size do
  begin
    if cx > cy then
    begin
      //landscape ...
      case cy of
        148:
          if cx = 210 then
            result := 'A5 (210 x 148mm)';
        210:
          if cx = 297 then
            result := 'A4 (297 x 210mm)';
        216:
          if cx = 279 then
            result := 'Letter (11 x 8.5")'
          else if cx = 356 then
            result := 'Legal (14 x 8.5")';
        297:
          if cx = 420 then
            result := 'A3 (420 x 297mm)';
      end;
    end
    else
    begin
      //portrait ...
      case cx of
        148:
          if cy = 210 then
            result := 'A5 (148 x 210mm)';
        210:
          if cy = 297 then
            result := 'A4 (210 x 297mm)';
        216:
          if cy = 279 then
            result := 'Letter (8.5 x 11")'
          else if cy = 356 then
            result := 'Legal (8.5 x 14")';
        297:
          if cy = 420 then
            result := 'A3 (297 x 420mm)';
      end;
    end;
    if result = '' then
      result := FormatString('Custom (% x %mm)', [cx, cy]);
  end;
end;

function GetNextItemW(var P: PWideChar): SynUnicode;
var
  S: PWideChar;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while (S^ <> #0) and (S^ <> ',') do
      inc(S);
    SetString(result, P, S - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function GetNextItemS(var P: PChar): string;
var
  S: PChar;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while (S^ <> #0) and (S^ <> ',') do
      inc(S);
    SetString(result, P, S - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function CsvToArray(var Csv: PWideChar; n: PtrInt): TSynUnicodeDynArray;
var
  i: PtrInt;
begin
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i] := GetNextItemW(Csv);
end;

/// round inverted color to white or black
function clAlways(cl: TColor): TColor;
begin
  if ((GetRValue(cardinal(cl)) * 2) +
      (GetGValue(cardinal(cl)) * 3) +
      (GetBValue(cardinal(cl)) * 2)) < 600 then
    result := clWhite
  else
    result := clBlack;
end;

function HasCRLF(const s: SynUnicode): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to length(s) - 1 do
    if s[i + 1] < ' ' then
      exit;
  result := false;
end;

// This declaration modifies Delphi's declaration of GetTextExtentExPoint
// so that the variable to receive partial string extents (p6) is ignored ...
function GetTextExtentExPointNoPartialsW(DC: HDC; p2: PWideChar; p3, p4: integer;
  var p5: integer; p6: pointer; var p7: TSize): BOOL; stdcall;
    external gdi32 name 'GetTextExtentExPointW';

// TrimLine: Splits off from LS any characters beyond the allowed width
// breaking at the end of a word if possible. Leftover chars -> RS.
procedure TrimLine(Canvas: TCanvas; var ls: SynUnicode; out rs: SynUnicode;
  LineWidthInPxls: integer);
var
  i, len, NumCharWhichFit: integer;
  dummy: TSize;

  function Fits: boolean;
  begin
    result := GetTextExtentExPointNoPartialsW(Canvas.Handle, pointer(ls), len,
      LineWidthInPxls, NumCharWhichFit, nil, dummy);
  end;

begin
  len := length(ls);
  if len = 0 then
    exit;
  // get the number of characters which will fit within LineWidth...
  if len > 1024 then
    len := 1024; // speed up the API call: we expect only one line of text
  if not Fits then // fix API error (too big text) by rough binary approximation
    repeat
      len := len shr 1;
    until (len = 0) or
          Fits;
  if NumCharWhichFit = length(ls) then
    exit; // if everything fits then stop here
  // find the end of the last whole word which will fit...
  i := NumCharWhichFit;
  while (NumCharWhichFit > 0) and
        (ls[NumCharWhichFit] > ' ') do
    dec(NumCharWhichFit);
  if (NumCharWhichFit = 0) then
    NumCharWhichFit := i;
  i := NumCharWhichFit + 1;
  // ignore trailing blanks in LS...
  while ls[NumCharWhichFit] <= ' ' do
    dec(NumCharWhichFit);
  // ignore beginning blanks in RS...
  len := length(ls); // may have been reduced if len>1024 or on API error
  while (i < len) and
        (ls[i] <= ' ') do
    inc(i);
  rs := copy(ls, i, len);
  ls := copy(ls, 1, NumCharWhichFit);        //nb: assign ls AFTER rs here
end;


procedure PrintBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: TBitmap);
var
  BitmapHeader: pBitmapInfo;
  BitmapImage: pointer;
  HeaderSize, ImageSize: dword;
begin
  // we expect the bitmap to be stored as DIB in the TMetaFile content
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage, ImageSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
    // will create a EMR_STRETCHDIBITS record, ready for SynPdf and SynGdiPlus
    StretchDIBits(Canvas.Handle,
                  DestRect.Left, DestRect.Top,     // Destination Origin
                  DestRect.Right  - DestRect.Left, // Destination Width
                  DestRect.Bottom - DestRect.Top,  // Destination Height
                  0,0,                             // Source Origin
                  Bitmap.Width, Bitmap.Height,     // Source Width & Height
                  BitmapImage,
                  TBitmapInfo(BitmapHeader^),
                  DIB_RGB_COLORS,
                  SRCCOPY);
  finally
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  end;
end;


// This DrawArrow() function is based on code downloaded from
// http://www.efg2.com/Lab/Library/Delphi/Graphics/Algorithms.htm
// (The original author is unknown)
procedure DrawArrowInternal(Canvas: TCanvas; const FromPoint, ToPoint: TPoint;
  HeadSize: integer; SolidArrowHead: boolean);
var
  xbase: integer;
  xLineDelta: integer;
  xLineUnitDelta: double;
  xNormalDelta: integer;
  xNormalUnitDelta: double;
  ybase: integer;
  yLineDelta: integer;
  yLineUnitDelta: double;
  yNormalDelta: integer;
  yNormalUnitDelta: double;
  SavedBrushColor: TColor;
begin
  with FromPoint do
    Canvas.MoveTo(x, y);
  with ToPoint do
    Canvas.LineTo(x, y);
  xLineDelta := ToPoint.X - FromPoint.X;
  yLineDelta := ToPoint.Y - FromPoint.Y;
  xLineUnitDelta := xLineDelta / SQRT(SQR(xLineDelta) + SQR(yLineDelta));
  yLineUnitDelta := yLineDelta / SQRT(SQR(xLineDelta) + SQR(yLineDelta));
  // (xBase,yBase) is where arrow line is perpendicular to base of triangle
  xbase := ToPoint.X - round(HeadSize * xLineUnitDelta);
  ybase := ToPoint.Y - round(HeadSize * yLineUnitDelta);
  xNormalDelta := yLineDelta;
  yNormalDelta := -xLineDelta;
  xNormalUnitDelta := xNormalDelta / SQRT(SQR(xNormalDelta) + SQR(yNormalDelta));
  yNormalUnitDelta := yNormalDelta / SQRT(SQR(xNormalDelta) + SQR(yNormalDelta));
  SavedBrushColor := Canvas.Brush.Color;
  if SolidArrowHead then
    Canvas.Brush.Color := Canvas.Pen.Color;
  Canvas.Polygon([
    ToPoint,
    Point(xbase + round(HeadSize * xNormalUnitDelta),
         ybase + round(HeadSize * yNormalUnitDelta)),
    Point(xbase - round(HeadSize * xNormalUnitDelta),
          ybase - round(HeadSize * yNormalUnitDelta))]);
  Canvas.Brush.Color := SavedBrushColor;
end;


{ ****************** TGdiPages Report Engine }

{ TPagePaintBox }

procedure TPagePaintBox.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.result := 1; // no erasing is necessary after this method call
end;


{ TGdiPages }

procedure TGdiPages.SetAnyCustomPagePx;
begin
  if Int64(fCustomPageSize) <> -1 then
    fPhysicalSizePx := fCustomPageSize;
  if Int64(fCustomNonPrintableOffset) <> -1 then
    fPhysicalOffsetPx := fCustomNonPrintableOffset;
  if Int64(fCustomPageMargins.TopLeft) <> -1 then
    fPageMarginsPx := fCustomPageMargins;
  if Int64(fCustomPxPerInch) <> -1 then
    fPrinterPxPerInch := fCustomPxPerInch;
  fDefaultLineWidth := (fPrinterPxPerInch.y * 25) div 2540;
end;

procedure TGdiPages.GetPrinterParams;
var
  i: integer;
begin
  if self = nil then
    exit;
  if not fForceScreenResolution and
    fHasPrinterInstalled then
  try
    fCurrentPrinter := CurrentPrinterName;
    if Printer.orientation <> fOrientation then
      Printer.orientation := fOrientation;
    fPtrHdl := Printer.Handle;
    fPrinterPxPerInch.x := GetDeviceCaps(fPtrHdl, LOGPIXELSX);
    fPrinterPxPerInch.y := GetDeviceCaps(fPtrHdl, LOGPIXELSY);
    fPhysicalSizePx.x   := GetDeviceCaps(fPtrHdl, PHYSICALWIDTH);
    fPhysicalOffsetPx.x := GetDeviceCaps(fPtrHdl, PHYSICALOFFSETX);
    fPhysicalSizePx.y   := GetDeviceCaps(fPtrHdl, PHYSICALHEIGHT);
    fPhysicalOffsetPx.y := GetDeviceCaps(fPtrHdl, PHYSICALOFFSETY);
    fDefaultLineWidth   := (fPrinterPxPerInch.y * 25) div 2540; // 0.25 mm
    exit; // if a printer was found then that's all that's needed
  except
    fHasPrinterInstalled := false;
  end;
  // ForceScreenResolution or no Printer: use screen resolution
  if fHasPrinterInstalled then
  begin
    if (Printer.orientation <> fOrientation) then
      Printer.orientation := fOrientation;
    fPtrHdl := printer.Handle;
    fPhysicalSizePx.X := round(
      GetDeviceCaps(fPtrHdl, PHYSICALWIDTH) * Screen.PixelsPerInch /
      GetDeviceCaps(fPtrHdl, LOGPIXELSX));
    fPhysicalSizePx.Y := round(
      GetDeviceCaps(fPtrHdl, PHYSICALHEIGHT) * Screen.PixelsPerInch /
      GetDeviceCaps(fPtrHdl, LOGPIXELSY));
  end
  else
  begin
    // if no printer drivers installed use the screen as device context and
    // assume A4 page size...
    fPtrHdl := 0; //GetDC(0);
    fPhysicalSizePx.X := MulDiv(PAPERSIZE_A4_WIDTH * 10, Screen.PixelsPerInch, 254);
    fPhysicalSizePx.Y := MulDiv(PAPERSIZE_A4_HEIGHT * 10, Screen.PixelsPerInch, 254);
  end;
  //assume 6mm non-printing offsets...
  fPhysicalOffsetPx.X := MulDiv(60, Screen.PixelsPerInch, 254);
  fPhysicalOffsetPx.Y := MulDiv(60, Screen.PixelsPerInch, 254);
  fPrinterPxPerInch.X := Screen.PixelsPerInch;
  fPrinterPxPerInch.Y := Screen.PixelsPerInch;
  //fDefaultLineWidth ==> 0.3 mm
  fDefaultLineWidth := (fPrinterPxPerInch.y * 3) div 254;
  if not fHasPrinterInstalled and (fOrientation = poLandscape) then
  begin
    // no Printer.Orientation -> swap width & height if Landscape page layout
    i := fPhysicalSizePx.x;
    fPhysicalSizePx.x := fPhysicalSizePx.y;
    fPhysicalSizePx.y := i;
  end;
end;

procedure TGdiPages.SetMetaFileForPage(PageIndex: integer; MetaFile: TMetaFile);
var
  stream: TRawByteStringStream;
begin
  if cardinal(PageIndex) >= cardinal(length(fPages)) then
    exit;
  stream := TRawByteStringStream.Create;
  try
    MetaFile.SaveToStream(stream);
    fPages[PageIndex].MetaFileCompressed := stream.DataString;
    CompressSynLZ(fPages[PageIndex].MetaFileCompressed, true);
  finally
    stream.Free;
  end;
end;

function TGdiPages.GetMetaFileForPage(PageIndex: integer): TMetaFile;
var
  tmp: RawByteString;
  stream: TStream;
begin
  if fMetaFileForPage = nil then
    fMetaFileForPage := TMetaFile.Create
  else
    fMetaFileForPage.Clear;
  result := fMetaFileForPage;
  if cardinal(PageIndex) >= cardinal(length(fPages)) then
    exit;
  tmp := fPages[PageIndex].MetaFileCompressed;
  CompressSynLZ(tmp, false);
  stream := TRawByteStringStream.Create(tmp);
  try
    fMetaFileForPage.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

function TGdiPages.PrinterPxToScreenPxX(PrinterPx: integer): integer;
begin
  if (self = nil) or
     (fPrinterPxPerInch.x = 0) then
    result := 0
  else
    result := (PrinterPx * Screen.PixelsPerInch * fZoom) div
              (fPrinterPxPerInch.x * 100);
end;

function TGdiPages.PrinterPxToScreenPxY(PrinterPx: integer): integer;
begin
  if (self = nil) or
     (fPrinterPxPerInch.y = 0) then
    result := 0
  else
    result := (PrinterPx * Screen.PixelsPerInch * fZoom) div
              (fPrinterPxPerInch.y * 100);
end;

function TGdiPages.MmToPrinterPxX(mm: integer): integer;
begin
  if self = nil then
    result := 0
  else
    result := ((mm * 10) * fPrinterPxPerInch.x) div 254;
end;

function TGdiPages.MmToPrinterPxY(mm: integer): integer;
begin
  if self = nil then
    result := 0
  else
    result := ((mm * 10) * fPrinterPxPerInch.y) div 254;
end;

function TGdiPages.PrinterPxToMmX(px: integer): integer;
begin
  if (self = nil) or (fPrinterPxPerInch.x = 0) then
    result := 0
  else
    result := (px * 254) div (fPrinterPxPerInch.x * 10);
end;

function TGdiPages.PrinterPxToMmY(px: integer): integer;
begin
  if (self = nil) or (fPrinterPxPerInch.y = 0) then
    result := 0
  else
    result := (px * 254) div (fPrinterPxPerInch.y * 10);
end;

procedure TGdiPages.ResizeAndCenterPaintbox;
var
  l, t, i: integer;
  siz: TPoint;
begin
  if cardinal(page - 1) < cardinal(length(fPages)) then
    siz := fPages[page - 1].SizePx
  else
    siz := fPhysicalSizePx;
  // center the paintbox according to the new size
  with fPreviewSurface do
  begin
    siz.X := PrinterPxToScreenPxX(siz.X) + GRAY_MARGIN * 2;
    siz.Y := PrinterPxToScreenPxY(siz.Y) + GRAY_MARGIN * 2;
    l := Max((self.ClientWidth - siz.X) div 2, 0) - HorzScrollbar.Position;
    t := Max((self.ClientHeight - siz.Y) div 2, 0) - VertScrollbar.Position;
    SetBounds(l, t, siz.X, siz.Y);
  end;
  // resize any hot link
  for i := 0 to fLinks.Count - 1 do
    TGdiPageReference(fLinks.Objects[i]).ToPreview(self);
end;

function TGdiPages.GetOrientation: TPrinterOrientation;
begin
  if (self = nil) or
     (fPhysicalSizePx.x > fPhysicalSizePx.y) then
    result := poLandscape
  else
    result := poPortrait;
end;

procedure TGdiPages.SetTextAlign(Value: TTextAlign);
begin
  if self <> nil then
    fAlign := Value;
end;

procedure TGdiPages.SetOrientation(orientation: TPrinterOrientation);
begin
  if (self <> nil) and
     (fOrientation <> orientation) then
  begin
    fOrientation := orientation;
    if fPages <> nil then
    begin
      // changed orientation after start writing -> customize with inversed size
      fCustomPageSize.X := fPhysicalSizePx.Y;
      fCustomPageSize.Y := fPhysicalSizePx.X;
      fCustomPxPerInch.X := fPrinterPxPerInch.Y;
      fCustomPxPerInch.Y := fPrinterPxPerInch.X;
      fCustomNonPrintableOffset.X := fPhysicalOffsetPx.Y;
      fCustomNonPrintableOffset.Y := fPhysicalOffsetPx.X;
    end;
  end;
end;

procedure TGdiPages.NewPageLayout(sizeWidthMM, sizeHeightMM,
  nonPrintableWidthMM, nonPrintableHeightMM: integer);
begin
  if self = nil then
    exit;
  fCustomPageSize.X := MmToPrinterPxX(sizeWidthMM);
  fCustomPageSize.Y := MmToPrinterPxY(sizeHeightMM);
  if (nonPrintableWidthMM >= 0) and
     (nonPrintableHeightMM >= 0) then
  begin
    fCustomNonPrintableOffset.X := MmToPrinterPxX(nonPrintableWidthMM);
    fCustomNonPrintableOffset.Y := MmToPrinterPxY(nonPrintableHeightMM);
  end;
  if fPages <> nil then // force new page if already some content
    NewPage(true);
end;

const 
  SIZES: array[TGdiPagePaperSize] of TPoint = ((
    x: 210; // psA4
    y: 297
  ), (
    x: 148; // psA5
    y: 210
  ), (
    x: 297; // psA3
    y: 420
  ), (
    x: 216; // psLetter
    y: 279
  ), (
    x: 216; // psLegal
    y: 356
  ));

procedure TGdiPages.NewPageLayout(paperSize: TGdiPagePaperSize;
  orientation: TPrinterOrientation;
  nonPrintableWidthMM, nonPrintableHeightMM: integer);
var
  Siz, NonPrint: TPoint;
begin
  if orientation = poPortrait then
  begin
    Siz := SIZES[paperSize];
    NonPrint.X := nonPrintableWidthMM;
    NonPrint.Y := nonPrintableHeightMM;
  end
  else
  begin
    Siz.X := SIZES[paperSize].Y;
    Siz.Y := SIZES[paperSize].X;
    NonPrint.X := nonPrintableHeightMM;
    NonPrint.Y := nonPrintableWidthMM;
  end;
  NewPageLayout(Siz.X, Siz.Y, NonPrint.X, NonPrint.Y);
end;

procedure TGdiPages.SetPage(NewPreviewPage: integer);
begin
  if self = nil then
    exit;
  if NewPreviewPage > length(fPages) then
    NewPreviewPage := length(fPages)
  else if NewPreviewPage < 1 then
    NewPreviewPage := 1;
  if (Pages = nil) or
     (fCurrPreviewPage = NewPreviewPage) then
    exit;
  fCurrPreviewPage := NewPreviewPage;
  fLinksCurrent := -1;
  FreeAndNil(PreviewSurfaceBitmap); // force double buffering Bitmap recreate
  ResizeAndCenterPaintbox; // if page size changed
  PreviewPaint(self);
  if Assigned(fPreviewPageChangedEvent) then
    fPreviewPageChangedEvent(self);
  if PreviewForm <> nil then
  begin
    PreviewPageCountLabel.Caption := format(sPageN, [Page, PageCount]);
    PreviewButtons[ord(rNextPage) - 1].Enabled := Page < PageCount;
    PreviewButtons[ord(rPreviousPage) - 1].Enabled := Page > 1;
  end;
end;

function TGdiPages.GetPageCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(fPages);
end;

function TGdiPages.GetLineHeight: integer;
var
  tm: TTextMetric;
  DC: HDC;
begin
  if self = nil then
  begin
    result := 0;
    exit;
  end;
  if fLineHeight = 0 then
  begin
    if not Assigned(fCanvas) then
    begin
      // if no current fCanvas: use the Screen resolution (very fast)
      DC := GetDC(0);
      GetTextMetrics(DC, tm);
      ReleaseDC(0, DC);
    end
    else
      GetTextMetrics(fCanvas.Handle, tm);
    fLineHeight := ((tm.tmHeight + tm.tmExternalLeading) * 9) shr 3;
  end;
  result := fLineHeight;
  if not fInHeaderOrFooter then
    case fLineSpacing of
      lsOneAndHalf:
        result := (result * 3) shr 1;
      lsDouble:
        result := result * 2;
    end;
end;

function TGdiPages.GetLineHeightMm: integer;
begin
  if self = nil then
    result := 0
  else
    result := PrinterPxToMmY(GetLineHeight);
end;

procedure TGdiPages.CheckHeaderDone;
begin
  if not fHeaderDone then
    DoHeader;
end;

procedure TGdiPages.CheckYPos;
begin
  if self = nil then
    exit;
  if fInHeaderOrFooter then
    exit;
  CheckHeaderDone;
  if not HasSpaceForLines(1) then
  begin
    NewPageInternal;
    // nb: header is done inside a group, so we must check for it
    CheckHeaderDone;
  end;
end;

function TGdiPages.GetYPos: integer;
begin
  if (self = nil) or
     (fPrinterPxPerInch.y = 0) then
    result := 0
  else
    result := (fCurrentYPos * 254) div (fPrinterPxPerInch.y * 10);
end;

procedure TGdiPages.SetYPos(YPos: integer);
begin
  if self = nil then
    exit;
  if fCurrentYPos >= fPhysicalSizePx.y then
    NewPageInternal;
  fCurrentYPos := MmToPrinterPxY(YPos);
end;

function TGdiPages.GetSavedState: TSavedState;
begin
  with result do
  begin
    Flags := TextFormatsToFlags;
    FontName := Font.Name;
    FontColor := Font.Color;
    LeftMargin := fPageMarginsPx.Left;
    RightMargin := fPageMarginsPx.Right;
    BiDiMode := fBiDiMode;
  end;
end;

procedure TGdiPages.SetSavedState(const SavedState: TSavedState);
begin
  with SavedState do
  begin
    SetFontWithFlags(Flags);
    Font.Name := FontName;
    Font.Color := FontColor;
    fPageMarginsPx.Left := LeftMargin;
    fPageMarginsPx.Right := RightMargin;
    fBiDiMode := BiDiMode;
  end;
end;

procedure TGdiPages.SaveLayout;
begin
  if self = nil then
    exit; // avoid GPF
  if fSavedCount >= length(fSaved) then
    SetLength(fSaved, fSavedCount + 20);
  fSaved[fSavedCount] := SavedState;
  inc(fSavedCount);
end;

procedure TGdiPages.RestoreSavedLayout;
begin
  if self = nil then
    exit; // avoid GPF
  if fSavedCount <= 0 then
    exit;
  dec(fSavedCount);
  SavedState := fSaved[fSavedCount];
end;

function TGdiPages.CreateMetaFile(aWidth, aHeight: integer): TMetaFile;
begin
  result := TMetaFile.Create;
  if self = nil then
    exit;
  result.Enhanced := true;
  result.Width := aWidth;
  result.Height := aHeight;
end;

procedure TGdiPages.FlushPageContent;
var
  n: PtrInt;
begin
  n := length(fPages);
  if n > 0 then
  begin
    with fPages[n - 1] do
    begin
      Text := fCanvasText;
      SizePx := fPhysicalSizePx;
      MarginPx := fPageMarginsPx;
      OffsetPx := fPhysicalOffsetPx;
    end;
    if fCurrentMetaFile <> nil then
    begin
      SetMetaFileForPage(n - 1, fCurrentMetaFile);
      FreeAndNil(fCurrentMetaFile);
    end;
  end;
end;

procedure TGdiPages.NewPageInternal;
var
  n: integer;
  UsedGroupSpace: integer;
  InGroup: boolean;
  GroupText: string;
begin
  if self = nil then
    exit;
  UsedGroupSpace := 0; //stops a warning
  InGroup := Assigned(fGroupPage);
  if InGroup then
  begin
    // close the Group Canvas
    UsedGroupSpace := fCurrentYPos;
    FreeAndNil(fCanvas); // now recreate/redraw a fresh fCanvas for DoFooter
    fCanvas := CreateMetafileCanvas(fCurrentMetaFile);
    fCanvas.Draw(0, 0, fCurrentMetaFile); // re-draw last page
    GroupText := fCanvasText;
    fCanvasText := fBeforeGroupText;
  end;
  DoFooter;
  // create a new metafile and its canvas ...
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  FlushPageContent;
  SetAnyCustomPagePx;
  //NewPage.MMWidth := (fPhysicalSizePx.x*2540) div fPrinterPxPerInch.x;
  //NewPage.MMHeight := (fPhysicalSizePx.y*2540) div fPrinterPxPerInch.y;
  n := Length(fPages) + 1;
  SetLength(fPages, n);
  fCurrentMetaFile := CreateMetaFile(fPhysicalSizePx.x, fPhysicalSizePx.y);
  fCanvas := CreateMetafileCanvas(fCurrentMetaFile);
  fCanvasText := '';
  inc(fVirtualPageNum);
  fCurrentYPos := fPageMarginsPx.top;
  if Assigned(fStartNewPage) then
    fStartNewPage(self, n);
  fHeaderDone := false;
  fColumnHeaderPrinted := false; // when next col. started add header
  if InGroup then
  begin
    // draw the group at the begining of new page + EndGroup
    DoHeader;
    if fColumnsUsedInGroup then
    begin
      //The next line is a workaround to stop an endless loop. CheckYPos (called
      //via PrintColumnHeaders) thinks we're still drawing on fGroupPage as it's
      //still Assigned so can flag "out of room" and try to create another page.
      fGroupVerticalSpace := fPhysicalSizePx.y;
      if not fColumnHeaderInGroup then
        PrintColumnHeaders
      else
        fColumnHeaderPrinted := true;
    end;
    fCanvas.Draw(0, fCurrentYPos, fGroupPage);
    FreeAndNil(fGroupPage); // idem as EndGroup
    inc(fCurrentYPos, UsedGroupSpace);
    fCanvasText := fCanvasText + GroupText;
  end;
end;

function TGdiPages.CreateMetafileCanvas(Page: TMetaFile): TMetaFileCanvas;
begin
  result := TMetaFileCanvas.Create(Page, fPtrHdl);
  if self = nil then
    exit;
  UpdateMetafileCanvasFont(result);
  result.Pen.Width := fPrinterPxPerInch.y div Screen.PixelsPerInch;
end;

procedure TGdiPages.UpdateMetafileCanvasFont(aCanvas: TMetaFileCanvas);
begin
  // next 2 lines are a printer bug workaround - 23Mar2000
  aCanvas.Font.Size := Font.Size + 1;
  aCanvas.Font.PixelsPerInch := fPrinterPxPerInch.y;
  aCanvas.Font := Font;
end;

function TGdiPages.TextFormatsToFlags: integer;
begin
  result := min(max(font.size, 4), FORMAT_SIZE_MASK); // size between 4 and 255 
  case fAlign of
    taRight:
      result := result or FORMAT_RIGHT;
    taCenter:
      result := result or FORMAT_CENTER;
    taJustified:
      result := result or FORMAT_JUSTIFIED;
  end;
  if fsBold in font.style then
    result := result or FORMAT_BOLD;
  if fsUnderline in font.style then
    result := result or FORMAT_UNDERLINE;
  if fsItalic in font.style then
    result := result or FORMAT_ITALIC;
end;

procedure TGdiPages.SetFontWithFlags(flags: integer);
var
  fontstyle: TFontStyles;
begin
  if flags and FORMAT_SIZE_MASK <> Font.Size then
    Font.size := flags and FORMAT_SIZE_MASK;
  if (flags and FORMAT_BOLD) <> 0 then
    fontstyle := [fsBold]
  else
    fontstyle := [];
  if (flags and FORMAT_UNDERLINE) <> 0 then
    include(fontstyle, fsUnderline);
  if (flags and FORMAT_ITALIC) <> 0 then
    include(fontstyle, fsItalic);
  if Font.Style <> fontstyle then
    Font.Style := fontstyle;
  case flags and FORMAT_ALIGN_MASK of
    FORMAT_RIGHT:
      fAlign := taRight;
    FORMAT_CENTER:
      fAlign := taCenter;
    FORMAT_JUSTIFIED:
      fAlign := taJustified;
  else
    fAlign := taLeft;
  end;
end;

function TGdiPages.HasSpaceForLines(Count: integer): boolean;
begin
  if self = nil then
    result := false // avoid GPF
  else if Assigned(fGroupPage) then
    result := fCurrentYPos + GetLineHeight * Count < fGroupVerticalSpace
  else
    result := fCurrentYPos + GetLineHeight * Count <
                fPhysicalSizePx.y - fPageMarginsPx.bottom - fFooterHeight;
end;

function TGdiPages.HasSpaceFor(mm: integer): boolean;
begin
  if self = nil then
    result := false // avoid GPF
  else
  begin 
    mm := fCurrentYPos + MmToPrinterPxY(mm);
    if Assigned(fGroupPage) then
      result := mm < fGroupVerticalSpace
    else
      result := mm < fPhysicalSizePx.y - fPageMarginsPx.bottom - fFooterHeight;
  end;
end;

procedure TGdiPages.DoHeader;
begin
  fHeaderDone := true;
  if (fHeaderLines.Count = 0) then
    exit;
  SaveLayout;
  fInHeaderOrFooter := true;
  try
    if Assigned(fStartPageHeader) then
      fStartPageHeader(self);
    Font.Color := clBlack;
    DoHeaderFooterInternal(fHeaderLines);
    if Assigned(fEndPageHeader) then
      fEndPageHeader(self);
    GetLineHeight;
    inc(fCurrentYPos, fLineHeight shr 2); // add a small header gap
    fHeaderHeight := fCurrentYPos - fPageMarginsPx.Top;
  finally
    fInHeaderOrFooter := false;
    RestoreSavedLayout;
  end;
end;

procedure TGdiPages.DoFooter;
begin
  if (fFooterLines.Count = 0) then
    exit;
  SaveLayout;
  fInHeaderOrFooter := true;
  try
    fCurrentYPos := fPhysicalSizePx.y - fPageMarginsPx.bottom - fFooterHeight +
      fFooterGap;
    if Assigned(fStartPageFooter) then
      fStartPageFooter(self);
    DoHeaderFooterInternal(fFooterLines);
    if Assigned(fEndPageFooter) then
      fEndPageFooter(self);
  finally
    fInHeaderOrFooter := false;
    RestoreSavedLayout;
  end;
end;

procedure TGdiPages.DoHeaderFooterInternal(Lines: TObjectList);
var
  i: integer;
begin
  SaveLayout;
  try
    for i := 0 to Lines.Count - 1 do
      with THeaderFooter(Lines[i]) do
      begin
        SavedState := State;
        PrintFormattedLine(Text, state.Flags);
      end;
  finally
    RestoreSavedLayout;
  end;
end;

procedure TGdiPages.CalcFooterGap;
begin
  GetLineHeight;
  // make sure there's a gap of at least 1/4 of a lineheight
  // between the page body and the footer ...
  fFooterGap := fLineHeight shr 2;
  fFooterHeight := fFooterGap;
end;

function TGdiPages.GetColumnRec(col: integer): TColRec;
begin
  if cardinal(col) < cardinal(length(fColumns)) then
    result := fColumns[col]
  else
  begin
    result.ColLeft := 0;
    result.ColRight := 0;
  end;
end;

procedure TGdiPages.PrintColumnHeaders;
var
  i, SavedFontSize, FontCol: integer;
  SavedFontStyle: TFontStyles;
  SavedAlign: TTextAlign;
  SavedWordWrapLeftCols: boolean;
begin
  if (fColumnHeaderList = nil) or (fColumns = nil) then
    exit;
  CheckYPos;
  fColumnHeaderPrinted := true;   // stops an endless loop
  SavedFontSize := Font.size;
  SavedFontStyle := font.style;
  SavedAlign := fAlign;
  SavedWordWrapLeftCols := WordWrapLeftCols;
  WordWrapLeftCols := false;
  if Assigned(fStartColumnHeader) then
    fStartColumnHeader(self);
  FontCol := fCanvas.Font.Color;
  for i := 0 to High(fColumnHeaderList) do
  begin
    SetFontWithFlags(fColumnHeaderList[i].flags);
    fCanvas.Font.Color := clBlack;
    fDrawTextAcrossColsDrawingHeader := true;
    DrawTextAcrossCols(fColumnHeaderList[i].headers, [], clNone);
    fDrawTextAcrossColsDrawingHeader := false;
  end;
  fCanvas.Font.Color := FontCol;
  if Assigned(fEndColumnHeader) then
    fEndColumnHeader(self);
  // add a small space below the column headers
  // inc(fCurrentYPos,fLineHeight shr 2);
  Font.Size := SavedFontSize;
  Font.Style := SavedFontStyle;
  fAlign := SavedAlign;
  WordWrapLeftCols := SavedWordWrapLeftCols;
  if Assigned(fGroupPage) then
    fColumnHeaderInGroup := true;
  fColumnHeaderPrintedAtLeastOnce := ForceCopyTextAsWholeContent; // not on every page
end;

procedure TGdiPages.SetZoomStatus(aZoomStatus: TZoomStatus);
var
  zoom: integer;
begin
  if (self = nil) or
     (aZoomStatus = fZoomStatus) then
    exit;
  case aZoomStatus of
    zsPageFit:
      zoom := PAGE_FIT;
    zsPageWidth:
      zoom := PAGE_WIDTH;
  else
    zoom := fZoom;
  end;
  SetZoom(zoom);
end;

procedure TGdiPages.SetZoom(Zoom: integer);
var
  scrollIncrement, zoomW, zoomH: integer;
  siz: TPoint;
begin
  if (self = nil) or
     (Zoom < PAGE_FIT) or
     (Zoom in [0..9]) or
     (Zoom > 200) then
    exit;
  fLinksCurrent := -1;
  FreeAndNil(PreviewSurfaceBitmap);
  if (not handleallocated) or
     (fZoom = Zoom) or
     (cardinal(Page - 1) >= cardinal(length(fPages))) then
    exit;
  // calculate the new fZoom ...
  siz := fPages[Page - 1].SizePx;
  if (siz.x = 0) or
     (siz.y = 0) then
    exit // in case of potential div per 0 -> do it later
  else if Zoom = PAGE_FIT then
  begin
    zoomW := trunc((clientWidth - GRAY_MARGIN * 2) * fPrinterPxPerInch.x * 100 /
      siz.x / Screen.PixelsPerInch);
    zoomH := trunc((clientHeight - GRAY_MARGIN * 2) * fPrinterPxPerInch.y * 100 /
      siz.y / Screen.PixelsPerInch);
    // choose the smallest of width% and height% to fit on page (but min 10%)
    fZoom := Max(Min(zoomW, zoomH), 10);
  end
  else if Zoom = PAGE_WIDTH then
    fZoom := trunc((clientWidth - GRAY_MARGIN * 2) * fPrinterPxPerInch.x * 100 /
      siz.x / Screen.PixelsPerInch)
  else
    fZoom := Zoom;
  // ZoomStatus required when resizing...
  if Zoom = PAGE_FIT then
    fZoomStatus := zsPageFit
  else if Zoom = PAGE_WIDTH then
    fZoomStatus := zsPageWidth
  else
    fZoomStatus := zsPercent;
  scrollIncrement := PrinterPxToScreenPxY(GetLineHeight);
  HorzScrollbar.Increment := scrollIncrement;
  VertScrollbar.Increment := scrollIncrement;
  // resize and center preview surface...
  ResizeAndCenterPaintbox;
  // notify custom event
  if Assigned(fZoomChangedEvent) then
    fZoomChangedEvent(self, fZoom, fZoomStatus);
end;

const
  ZOOMSTEP = 20;

procedure TGdiPages.ZoomTimerInternal(X, Y: integer; ZoomIn: boolean);
var
  OldZoom: integer;
  pt, siz: TPoint;
begin
  if (self = nil) or
     (fPhysicalSizePx.x = 0) or
     (fPhysicalSizePx.y = 0) then
    exit;
  if page > 0 then
    siz := fPages[page - 1].SizePx
  else
    siz := fPhysicalSizePx;
  OldZoom := fZoom;
  sendmessage(handle, WM_SETREDRAW, 0, 0);
  try
    if ZoomIn then
    begin
    {$ifdef MOUSE_CLICK_PERFORM_ZOOM}
      if fZoom >= 200 then
        fZoomTimer.enabled := false
      else      //(maximum 200%)
    {$else}
      if fZoom < 200 then
    {$endif MOUSE_CLICK_PERFORM_ZOOM}
        Zoom := ((fZoom + ZOOMSTEP) div ZOOMSTEP) * ZOOMSTEP; // nearest 
    end
    else
    begin
      if (fZoom > 20) then
        Zoom := ((fZoom - ZOOMSTEP) div ZOOMSTEP) * ZOOMSTEP
      else //(minimum 20%)
    {$ifdef MOUSE_CLICK_PERFORM_ZOOM}
        fZoomTimer.enabled := false;
    {$endif MOUSE_CLICK_PERFORM_ZOOM}
    end;
    if fZoom = OldZoom then
      exit;
    // work out click pos relative to page (as x & y percentages)
    pt.x := ((X - fPreviewSurface.left - GRAY_MARGIN) * 100) div
      PrinterPxToScreenPxX(siz.x);
    pt.x := min(max(pt.x, 0), 100);
    pt.y := ((Y - fPreviewSurface.top - GRAY_MARGIN) * 100) div
      PrinterPxToScreenPxY(siz.y);
    pt.y := min(max(pt.y, 0), 100);
    // finally, adjust scrollbar positions based on click pos ...
    with HorzScrollbar do
      position := (pt.x * (range - clientwidth)) div 100;
    with VertScrollbar do
      position := (pt.y * (range - clientheight)) div 100;
  finally
    SendMessage(handle, WM_SETREDRAW, 1, 0);
  end;
  Invalidate;
end;

procedure TGdiPages.ZoomTimer(Sender: TObject);
var
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  CursorPos := ScreenToClient(CursorPos);
  ZoomTimerInternal(CursorPos.x, CursorPos.y, fZoomIn);
end;

procedure TGdiPages.LineInternal(start, finish : integer; doubleline : boolean);
begin
  LineInternal(fCurrentYPos + (GetLineHeight shr 1), start, finish, doubleline);
end;

procedure TGdiPages.LineInternal(aty, start, finish: integer; doubleline: boolean);
var
  Y: integer;
begin
  if (self <> nil) and (fCanvas <> nil) then
    with fCanvas do
    begin
      Pen.Width := MulDiv(fDefaultLineWidth, self.Font.size, 8);
      if fsBold in self.Font.style then
        Pen.Width := Pen.Width + 1;
      if doubleline then
      begin
        Y := aty - (Pen.Width);
        MoveTo(start, Y);
        LineTo(finish, Y);
        MoveTo(start, Y + (Pen.Width * 2));
        LineTo(finish, Y + (Pen.Width * 2));
      end
      else
      begin
        Y := aty - (Pen.Width shr 1);
        MoveTo(start, Y);
        LineTo(finish, Y);
      end;
    end;
end;

procedure TGdiPages.PrintFormattedLine(s: SynUnicode; flags: integer;
  const aBookmark: string; const aLink: string;
  withNewLine, aLinkNoBorder: boolean);
var
  i, xpos: integer;
  leftOffset, rightOffset: integer;
begin
  s := RightTrim(s);
  i := pos(PAGENUMBER, LowerCaseU(s));
  if i > 0 then
  begin
    delete(s, i, 14);
    insert(Utf8ToSynUnicode(Int32ToUtf8(fVirtualPageNum)), s, i);
  end;
  if flags <> FORMAT_DEFAULT then
    SetFontWithFlags(flags);
  CheckYPos;
  fCurrentTextTop := fCurrentYPos;
  fCurrentTextPage := PageCount;
  GetTextLimitsPx(leftOffset, rightOffset);
  if flags and (FORMAT_SINGLELINE or FORMAT_DOUBLELINE) <> 0 then
  begin
    LineInternal(leftOffset, rightOffset,
      flags and FORMAT_DOUBLELINE = FORMAT_DOUBLELINE);
    NewLine;
  end
  else if s = '' then
  begin
    if withNewLine then
      NewLine;
  end
  else if (flags and FORMAT_XPOS_MASK) <> 0 then
  begin
    xpos := ((flags and FORMAT_XPOS_MASK) shr 16) - 2;
    if xpos < 0 then
      xpos := RightMarginPos
    else
      inc(xpos);
    DrawTextAt(s, xpos);
  end
  else if fAlign in [taLeft, taJustified] then
    LeftOrJustifiedWrap(s, withNewLine)
  else
    RightOrCenterWrap(s);
  if aBookmark <> '' then
    AddBookMark(aBookmark, fCurrentTextTop);
  if aLink <> '' then
    AddLink(aLink, Rect(
        PrinterPxToMmX(leftOffset),
        PrinterPxToMmY(fCurrentTextTop),
        PrinterPxToMmX(rightOffset),
        PrinterPxToMmY(fCurrentTextTop + fLineHeight)),
      fCurrentTextPage, aLinkNoBorder);
    // first line of written text is added
end;

procedure TGdiPages.LeftOrJustifiedWrap(const s: SynUnicode; withNewLine: boolean);
var
  indent, leftOffset, rightOffset, LineWidth: integer;
  leftstring, rightstring: SynUnicode;
  firstLoop: boolean;
begin
  leftstring := s;
  indent := MmToPrinterPxX(fHangIndent);
  firstLoop := true;
  repeat
    CheckYPos;
    GetTextLimitsPx(leftOffset, rightOffset);
    LineWidth := rightOffset - leftOffset;
    // offset leftOffset if hang-indenting...
    if indent <> 0 then
      if firstLoop then
      begin
        firstLoop := false;
        if (indent < 0) then
        begin
          inc(leftOffset, -indent);
          dec(LineWidth, -indent);
        end;
      end
      else if (indent > 0) and (indent < LineWidth) then
      begin
        inc(leftOffset, indent);
        dec(LineWidth, indent);
      end;
    // dump overrun into rightstring...
    TrimLine(fCanvas, leftstring, rightstring, LineWidth);
    // HandleTabsAndPrint: prints leftstring after adjusting for tabs and
    // prepending any further text overrun into rightstring ...
    HandleTabsAndPrint(leftstring, rightstring, leftOffset, rightOffset);
    if rightstring = '' then
      break;
    leftstring := rightstring;
    NewLine;
  until false;
  if withNewLine then
    NewLine;
end;

procedure TGdiPages.RightOrCenterWrap(const s: SynUnicode);
var
  i, leftOffset, rightOffset, LineWidth: integer;
  leftstring, rightstring: SynUnicode;
  offset: integer;
begin
  leftstring := s;
  // remove tabs and replace by spaces
  i := pos(#9, leftstring);
  while i > 0 do
  begin
    delete(leftstring, i, 1);
    insert('    ', leftstring, i);
    i := pos(#9, leftstring);
  end;
  // write text
  SetBkMode(fCanvas.Handle, TRANSPARENT);
  repeat
    GetTextLimitsPx(leftOffset, rightOffset);
    LineWidth := rightOffset - leftOffset;
    TrimLine(fCanvas, leftstring, rightstring, LineWidth);
    case fAlign of
      taRight:
        offset := rightOffset - TextWidthC(fCanvas, leftstring) - 1;
      taCenter:
        offset := leftOffset +
          (rightOffset - leftOffset - TextWidthC(fCanvas, leftstring)) div 2;
    else
      offset := 0; // should never happen - ?? add assert
    end;
    CheckYPos;
    TextOut(fCanvas, offset, fCurrentYPos, leftstring);
    if rightstring = '' then
      break;
    leftstring := rightstring;
    NewLine;
  until false;
  NewLine;
end;

procedure TGdiPages.GetTextLimitsPx(var LeftOffset, RightOffset: integer);
begin
  // Offsets (in Printer pixels) based on current page margins
  LeftOffset := fPageMarginsPx.left;
  if fForcedLeftOffset <> -1 then
    LeftOffset := fForcedLeftOffset;
  RightOffset := fPhysicalSizePx.x - fPageMarginsPx.right;
  if RightOffset < LeftOffset then
    raise EReport.CreateUtf8('%.GetTextLimitsPx: wrong margins', [self]);
end;

procedure TGdiPages.HandleTabsAndPrint(const leftstring: SynUnicode;
  var rightstring: SynUnicode; leftOffset, rightOffset: integer);
const
  // if a tabstop is very close to the right margin, it may spoil justifying...
  MIN_CHAR_WIDTH_PX = 5;
var
  i, spacecount, linewidth, tabPos, tabIndex, PWLen: integer;
  ls, rs: SynUnicode;
  size: TSize;
  PW: PWideChar;
begin
  // handles tabs one at a time and prints text into the available space...
  // (unfortunately there's no equivalent GetTextExtentExPoint() for tabbed text
  // and using GetTabbedTextExtent() and TabbedDrawText() instead would appear
  // to be undesirable as there's no efficient way to determine the number of
  // chars that will fit within the specified space)
  ls := leftstring;
  linewidth := rightOffset - leftOffset;
  tabPos := pos(#9, ls);
  SetBkMode(fCanvas.Handle, TRANSPARENT);
  while tabPos > 0 do
  begin // and still room to print
    // split line at the tab ...
    if rs <> '' then
      rs := copy(ls, tabPos + 1, length(ls)) + ' ' + rs
    else
      rs := copy(ls, tabPos + 1, length(ls));
    // add a trailing space so next the tabstop is at least one space away ...
    ls := copy(ls, 1, tabPos - 1) + ' ';
    // get offset of next tabstop ...
    size := TextExtent(fCanvas, ls, tabPos);
    i := leftOffset + size.cx; //minimum pos for next tabstop
    tabIndex := 0;
    while tabIndex < MAXTABS do
      if fTab[tabIndex] > i then
        break
      else
        inc(tabIndex);
    if (tabIndex = MAXTABS) or
       (fTab[tabIndex] >= rightOffset - MIN_CHAR_WIDTH_PX) then
    begin
      // no tabstop found to align 'rs' to, so ...
      // rather than left aligning 'ls', remove its appended space and
      // break out ready to print it ? align left&right justified.
      SetLength(ls, length(ls) - 1);
      break;
    end;
    // tabstop found so DrawText 'ls' simply left aligned ...
    TextOut(fCanvas, leftOffset, fCurrentYPos, ls);
    leftOffset := fTab[tabIndex];
    linewidth := rightOffset - leftOffset;
    ls := rs;
    TrimLine(fCanvas, ls, rs, linewidth);
    tabPos := pos(#9, ls);
  end;
  if rs <> '' then
    rightstring := rs + ' ' + rightstring;
  // OK, no TABS now in ls...
  InternalUnicodeString(ls, PW, PWLen, @size);
  // print ls into (remaining) linewidth at (leftOffset, fCurrentYPos)
  if (fAlign = taLeft) or
     (rightstring = '') then
  begin
    // left aligned
    if BiDiMode = bdRightToLeft then
      leftOffset := rightOffset - size.cx;
    TextOut(fCanvas, leftOffset, fCurrentYPos, PW, PWLen);
    fForcedLeftOffset := leftOffset + size.cx;
    // don't care about line width: it should be always equal or smaller,
    // and we are left aligned
  end
  else
  begin
    // justified
    spacecount := 0;
    for i := 1 to length(ls) do
      if ls[i] = ' ' then
        inc(spacecount);
    if spacecount > 0 then
      SetTextJustification(fCanvas.Handle, linewidth - size.cx, spacecount);
    TextOut(fCanvas, leftOffset, fCurrentYPos, PW, PWLen);
    SetTextJustification(fCanvas.Handle, 0, 0);
  end;
end;

procedure TGdiPages.PreviewPaint(Sender: TObject);
var
  R: TRect;
  P1, P2: TPoint;
  metapage: TMetaFile;
begin
  if csDesigning in ComponentState then
  begin
    // no preview at design time
    R := fPreviewSurface.ClientRect;
    fPreviewSurface.Canvas.Brush.Color := Color;
    fPreviewSurface.Canvas.FillRect(R);
    exit;
  end;
  if not Visible then
  begin
    FreeAndNil(PreviewSurfaceBitmap);
    exit;
  end;
  if PreviewSurfaceBitmap <> nil then
    fPreviewSurface.Canvas.Draw(0, 0, PreviewSurfaceBitmap)
  else
    with fPreviewSurface do
    begin
      // paint the page white with a dark gray line around it
      R := ClientRect;
      PreviewSurfaceBitmap := TBitmap.Create;
      PreviewSurfaceBitmap.Width := R.Right;
      PreviewSurfaceBitmap.Height := R.Bottom;
      with PreviewSurfaceBitmap.Canvas do
      begin
        Brush.Color := Color; // background color
        FillRect(R);
        InflateRect(R, -GRAY_MARGIN, -GRAY_MARGIN);
        Brush.Color := clWhite;
        Pen.Width := 1;
        Pen.Color := clGray;
        Rectangle(R);
        Refresh;
      end;
      // draw the metafile on the page
      if (fPages <> nil) and (cardinal(Page - 1) <= cardinal(High(fPages))) and
        (fPages[Page - 1].MetaFileCompressed <> '') then
      begin
        // note: we must use a temporary TMetaFile, otherwise the Pages[] content
        // is changed (screen dpi is changed but not reset in nested emf) and the
        // resulting report is incorrect on most printers, due to a driver bug :(
        metapage := GetMetaFileForPage(Page - 1);
      {$ifdef GDIPLUSDRAW}
        // anti-aliased drawing using GDI+
        if not ForceNoAntiAliased then
          DrawAntiAliased(metapage, PreviewSurfaceBitmap.Canvas.Handle, R,
            AntiAliasedOptions)
        else
      {$endif GDIPLUSDRAW}
          // fast direct GDI painting, with no antialiaising:
          PreviewSurfaceBitmap.Canvas.StretchDraw(R, metapage);
        PreviewSurfaceBitmap.Canvas.Refresh;
      end;
      // draw the change-page grey "arrow" buttons
      if Page > 1 then
      begin
        P1.X := R.Left + 10;
        P2.X := R.Left + 1;
        PageLeftButton.X := P2.X;
        P1.Y := R.Top + 11;
        P2.Y := P1.Y;
        PageLeftButton.Y := P1.Y - 10;
        DrawArrowInternal(PreviewSurfaceBitmap.Canvas, P1, P2, 10, true);
      end
      else
        PageLeftButton.X := 0;
      if Page < PageCount then
      begin
        P1.X := R.Right - 10;
        PageRightButton.X := P1.X;
        P2.X := R.Right - 1;
        P1.Y := R.Top + 11;
        P2.Y := P1.Y;
        PageRightButton.Y := P1.Y - 10;
        DrawArrowInternal(PreviewSurfaceBitmap.Canvas, P1, P2, 10, true);
      end
      else
        PageRightButton.X := 0;
      //draw the page shadows
      R.Top := GRAY_MARGIN + 3;
      R.Left := ClientWidth - GRAY_MARGIN;
      R.Bottom := ClientHeight - GRAY_MARGIN + 3;
      R.Right := R.Left + 3;
      PreviewSurfaceBitmap.Canvas.brush.color := clGray;
      PreviewSurfaceBitmap.Canvas.FillRect(R);
      R.Top := ClientHeight - GRAY_MARGIN;
      R.Left := GRAY_MARGIN + 3;
      R.Bottom := R.Top + 3;
      R.Right := ClientWidth - GRAY_MARGIN + 3;
      PreviewSurfaceBitmap.Canvas.brush.color := clGray;
      PreviewSurfaceBitmap.Canvas.FillRect(R);
      Canvas.Draw(0, 0, PreviewSurfaceBitmap)
    end;
  if fLinksCurrent >= 0 then
    fPreviewSurface.Canvas.DrawFocusRect(
      TGdiPageReference(fLinks.Objects[fLinksCurrent]).Preview);
end;

procedure TGdiPages.PreviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  if Button = mbRight then
  begin
    if PopupMenu <> nil then
    begin
      with fPreviewSurface.ClientToScreen(Point(X, Y)) do
        PopupMenu.Popup(X, Y);
      exit;
    end;
  end
  else if Button = mbLeft then
  begin
    if fLinksCurrent >= 0 then
    begin
      fPreviewSurface.Canvas.DrawFocusRect(
        TGdiPageReference(fLinks.Objects[fLinksCurrent]).Preview);
      i := fLinksCurrent;
      fLinksCurrent := -1;
      GotoBookmark(fLinks[i]);
    end
    else if (PageLeftButton.X <> 0) and
            (cardinal(X - PageLeftButton.X) < 10) and
            (cardinal(Y - PageLeftButton.Y) < 20) then
    begin
      Page := Page - 1;
      exit;
    end
    else if (PageRightButton.X <> 0) and
            (cardinal(X - PageRightButton.X) < 10) and
            (cardinal(Y - PageRightButton.Y) < 20) then
    begin
      Page := Page + 1;
      exit;
    end;
  end;
  if (Button = mbLeft) and
      (ssDouble in Shift) then
    // allows dblclick to alternate between PAGE_FIT and PAGE_WIDTH
    if ZoomStatus = zsPageWidth then
      Zoom := PAGE_FIT
    else
      Zoom := PAGE_WIDTH
  else
{$ifndef MOUSE_CLICK_PERFORM_ZOOM}
  if Button = mbLeft then
  begin
    fButtonDown.X := (X shr 3) shl 3; // move 8 pixels by 8 pixels
    fButtonDown.Y := (Y shr 3) shl 3;
    fButtonDownScroll.X := HorzScrollBar.Position;
    fButtonDownScroll.Y := VertScrollBar.Position;
    Screen.Cursor := crHandPoint;
  end;
{$endif MOUSE_CLICK_PERFORM_ZOOM}
  // pass the TPaintbox mouse-down event messages to self (TScrollBox) ...
  MouseDown(Button, Shift, X + fPreviewSurface.left, Y + fPreviewSurface.Top);
end;

procedure TGdiPages.PreviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // pass the TPaintbox mouse-up event messages to self (TScrollBox) ...
  MouseUp(Button, Shift, X + fPreviewSurface.left, Y + fPreviewSurface.Top);
end;

procedure TGdiPages.PreviewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
{$ifndef MOUSE_CLICK_PERFORM_ZOOM}
var
  BX, V: integer;
{$endif MOUSE_CLICK_PERFORM_ZOOM}
var
  i: integer;
begin
  fMousePos.X := X + fPreviewSurface.left;
  fMousePos.Y := Y + fPreviewSurface.Top;
  if fLinksCurrent >= 0 then
  begin
    fPreviewSurface.Canvas.DrawFocusRect(
      TGdiPageReference(fLinks.Objects[fLinksCurrent]).Preview);
    fLinksCurrent := -1;
  end;
{$ifndef MOUSE_CLICK_PERFORM_ZOOM}
  if fButtonDown.X >= 0 then
  begin
    X := (X shr 3) shl 3; // move 8 pixels by 8 pixels
    Y := (Y shr 3) shl 3;
    BX := fButtonDown.X;
    fButtonDown.X := -1; // avoid endless recursive call
    V := fButtonDownScroll.X - X + BX;
    if (V >= 0) and
       (HorzScrollBar.Position <> V) and
       (V < HorzScrollBar.Range) then
    begin
      HorzScrollBar.Position := V;
      fButtonDownScroll.X := V;
    end;
    V := fButtonDownScroll.Y - Y + fButtonDown.Y;
    if (V >= 0) and
       (VertScrollBar.Position <> V) and
       (V < VertScrollBar.Range) then
    begin
      VertScrollBar.Position := V;
      fButtonDownScroll.Y := V;
    end;
    fButtonDown.X := BX;
    exit;
  end
  else
{$endif MOUSE_CLICK_PERFORM_ZOOM}
    for i := 0 to fLinks.Count - 1 do
      with TGdiPageReference(fLinks.Objects[i]) do
        if (Page = self.Page) and
           (X >= Preview.Left) and
           (X < Preview.Right) and
           (Y >= Preview.Top) and
           (Y < Preview.Bottom) then
        begin
          fLinksCurrent := i;
          fPreviewSurface.Canvas.DrawFocusRect(Preview);
          break;
        end;
end;

procedure TGdiPages.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(fCanvas) then
    UpdateMetafileCanvasFont(fCanvas);
  fLineHeight := 0; // force recalculation of lineheight
end;

procedure TGdiPages.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.result := DLGC_WANTARROWS;
end;

procedure TGdiPages.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SetPageAndPosition(newpage, newpos: integer);
  begin
    perform(WM_SETREDRAW, 0, 0);
    Page := newpage;
    VertScrollbar.position := newpos;
    perform(WM_SETREDRAW, 1, 0);
    refresh;
  end;

var
  OldPosition, lh: integer;
begin
  lh := PrinterPxToScreenPxY(GetLineHeight);
  case Key of
    VK_DOWN:
      with VertScrollbar do
      begin
        OldPosition := Position;
        position := position + lh;
        if (Position = OldPosition) and
           (Page < PageCount) then
          SetPageAndPosition(Page + 1, 0);
      end;
    VK_UP:
      with VertScrollbar do
      begin
        OldPosition := Position;
        position := position - lh;
        if (Position = OldPosition) and
           (Page > 1) then
          SetPageAndPosition(Page - 1, range);
      end;
    VK_RIGHT:
      with HorzScrollbar do
        position := position + max(lh, 0);
    VK_LEFT:
      with HorzScrollbar do
        position := position - min(lh, range);
    VK_NEXT:
      with VertScrollbar do
        if (Shift = [ssCtrl]) and
           (Page < PageCount) then
          SetPageAndPosition(PageCount, 0)
        else
        begin
          OldPosition := Position;
          position := position + max(clientheight - lh, 0);
          if (Position = OldPosition) and
             (Page < PageCount) then
            SetPageAndPosition(Page + 1, 0);
        end;
    VK_PRIOR:
      with VertScrollbar do
      begin
        if (Shift = [ssCtrl]) and (Page > 1) then
          SetPageAndPosition(1, 0)
        else
        begin
          OldPosition := Position;
          position := position - max(clientheight - lh, 0);
          if (Position = OldPosition) and
             (Page > 1) then
            SetPageAndPosition(Page - 1, range);
        end;
      end;
    VK_ADD,
    VK_SUBTRACT,
    187,
    189:
      if ssCtrl in Shift then
      begin
        fZoomIn := Key in [VK_ADD, 187]; // Ctrl+ Ctrl- are standard zoom IN/OUT
        ZoomTimer(nil);
      end;
    VK_ESCAPE:
      if PreviewForm <> nil then
        PreviewForm.Close; // ESC will close preview form (if any)
  end;
  inherited;
end;

procedure TGdiPages.CreateWnd;
begin
  inherited CreateWnd;
  // force page repositioning  +/-resizing
  case ZoomStatus of
    zsPercent:
      ResizeAndCenterPaintbox;
    zsPageWidth:
      zoom := PAGE_WIDTH;
  else
    zoom := PAGE_FIT;
  end;
end;

procedure TGdiPages.Resize;
begin
  // force page repositioning  +/-resizing
  case ZoomStatus of
    zsPercent:
      ResizeAndCenterPaintbox;
    zsPageWidth:
      zoom := PAGE_WIDTH;
  else
    zoom := PAGE_FIT;
  end;
  inherited Resize;
end;

procedure TGdiPages.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  // allow overriding of default mouse handling...
  if not Assigned(OnMouseDown) then
  begin
    fZoomIn := (Button = mbLeft);
    ZoomTimerInternal(X, Y, fZoomIn);
    fZoomTimer.Enabled := true;
  end;
{$endif MOUSE_CLICK_PERFORM_ZOOM}
  if Button = mbLeft then
  begin
    if PopupMenu <> nil then
    begin
      with fPreviewSurface do
        if (X < Left) or
           (X > Left + Width) then
          with self.ClientToScreen(Point(X, Y)) do
            self.PopupMenu.Popup(X, Y);
    end;
  end;
  if canfocus and
     not focused then
    Setfocus;
  inherited;
end;

procedure TGdiPages.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  fZoomTimer.enabled := false; 
{$else}
  fButtonDown.X := -1;
  // so MouseMoveFast() won't scroll paintbox
  Screen.Cursor := crDefault;
{$endif MOUSE_CLICK_PERFORM_ZOOM}
  inherited;
end;

{$ifndef FPC}
function TGdiPages.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
var
  key: word;
begin
  //treat mousewheel events as if a down-arrow or up-arrow event ...
  if Shift = [] then
  begin
    if WheelDelta < 0 then
      key := VK_DOWN
    else
      key := VK_UP;
    KeyDown(key, []);
  end
  else if Shift = [ssCtrl] then
    ZoomTimerInternal(fMousePos.X, fMousePos.Y, (WheelDelta > 0));
  result := true;
end;
{$endif FPC}

constructor TGdiPages.Create(AOwner: TComponent);
{$ifdef USEPDFPRINTER}
var
  i: integer;
  aName: string;
{$endif USEPDFPRINTER}
begin
  inherited Create(AOwner);
  SetLength(fTab, MAXTABS);
  PopupMenuClass := TPopupMenu;
  // DoubleBuffered := true; // avoiding flicker is done in Paint method
  Height := 150;
  width := 200;
  ControlStyle := ControlStyle - [csAcceptsControls];
  if (AOwner <> nil) and
     AOwner.InheritsFrom(TCustomForm) then
    Color := TCustomForm(AOwner).Color
  else
    Color := clLtGray;
  HorzScrollBar.Tracking := true;
  VertScrollBar.Tracking := true;
  tabstop := true;
  Font.Name := 'Tahoma';
  Font.Size := 12;
  fLineSpacing := lsSingle;
  fOrientation := poPortrait;
  fUseOutlines := true;
  fHeaderLines := TObjectList.Create;
  fFooterLines := TObjectList.Create;
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  fZoomTimer := TTimer.create(self);
  fZoomTimer.Interval := 200;
  fZoomTimer.OnTimer := ZoomTimer;
  fZoomTimer.enabled := false;
{$else}
  fButtonDown.X := -1; // so MouseMoveFast() won't scroll paintbox
{$endif MOUSE_CLICK_PERFORM_ZOOM}
  Int64(fCustomPxPerInch) := -1;
  Int64(fCustomPageSize) := -1;
  Int64(fCustomNonPrintableOffset) := -1;
  Int64(fCustomPageMargins.TopLeft) := -1;
  fHasPrinterInstalled := not (csDesigning in componentState) and
    PrinterDriverExists;
{$ifdef USEPDFPRINTER}
  fPDFPrinterIndex := -1;
  if fHasPrinterInstalled then
    for i := 0 to Printer.Printers.Count - 1 do
    begin
      aName := Printer.Printers[i];
      if pos('doPDF', aName) = 1 then
      begin
        fPDFPrinterIndex := i;
        break;
      end
      else if pos('PDF', aName) > 0 then
        fPDFPrinterIndex := i;
    end;
  fHasPDFPrinterInstalled := (fPDFPrinterIndex <> -1); 
{$else}
  fExportPdfUseFontFallBack := true;
  fExportPdfEncryptionPermissions := PDF_PERMISSION_ALL;
  fExportPdfEncryptionOwnerPassword := 'mORMot ' + SYNOPSE_FRAMEWORK_VERSION;
{$endif USEPDFPRINTER}
  GetPrinterParams; // necessary, but will also be updated in BeginDoc()
  fCanvas := nil;
  fPreviewSurface := TPagePaintbox.Create(self);
  fPreviewSurface.parent := self;
  fPreviewSurface.OnPaint := PreviewPaint;
  fPreviewSurface.OnMouseDown := PreviewMouseDown;
  fPreviewSurface.OnMouseUp := PreviewMouseUp;
  fPreviewSurface.OnMouseMove := PreviewMouseMove;
  fZoomStatus := zsPercent;
  fZoom := 100;
  fBookmarks := TStringList.Create;
  fLinks := TStringList.Create;
  fOutline := TStringList.Create;
  fForcedLeftOffset := -1;
end;

destructor TGdiPages.Destroy;
begin
  Clear;
  fHeaderLines.free;
  fFooterLines.free;
  fPreviewSurface.free;
  PreviewSurfaceBitmap.Free;
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  fZoomTimer.free;
{$endif MOUSE_CLICK_PERFORM_ZOOM}
  fOutline.Free;
  fLinks.Free;
  fBookmarks.Free;
  fMetaFileForPage.Free;
  fCurrentMetaFile.Free;
  inherited Destroy;
end;

procedure TGdiPages.Invalidate;
begin
  FreeAndNil(PreviewSurfaceBitmap); // invalidate custom double buffering
  inherited;
end;

procedure TGdiPages.BeginDoc;
begin
  if self = nil then
    exit; // avoid GPF
  Clear;
  GetPrinterParams; // essential as Printers.printer object may have changed
  fHangIndent := 0;
  fAlign := taLeft;
  SetPageMargins(Rect(10, 10, 10, 10));
  fVirtualPageNum := 0;
  Application.ProcessMessages;
  NewPageInternal;  // create a blank page
  // preview resize in case Printers.printer object has changed
  case ZoomStatus of
    zsPercent:
      SetZoom(fZoom);
    zsPageWidth:
      SetZoom(PAGE_WIDTH);
  else
    SetZoom(PAGE_FIT);
  end;
  fButtonDown.X := -1; // so MouseMoveFast() won't scroll paintbox
end;

procedure TGdiPages.DrawText(const s: string; withNewLine: boolean);
begin
  DrawTextW(StringToSynUnicode(s), withNewLine);
end;

procedure TGdiPages.DrawTextW(const s: SynUnicode; withNewLine: boolean);
var
  P, Start: PWideChar;
  tmpStr: SynUnicode;
begin
  if self = nil then
    exit;
  CheckYPos;
  if s = '' then
  begin
    if withNewLine then
      NewLine;
  end
  else
  begin
    // split NewLine characters (#13 or #13#10) into multi lines
    P := pointer(s);
    while P^ <> #0 do
    begin
      Start := P;
      while not (ord(P^) in [0, 10, 13]) do
        inc(P);
      SetString(tmpStr, Start, P - Start);
      if not fInHeaderOrFooter then
        fCanvasText := fCanvasText + SynUnicodeToString(tmpStr) + #13#10;
      PrintFormattedLine(tmpStr, FORMAT_DEFAULT, '', '', withNewLine);
      if P^ = #13 then
        inc(P);
      if P^ = #10 then
        inc(P);
    end;
  end;
end;

procedure TGdiPages.DrawTextU(const s: RawUtf8; withNewLine: boolean);
begin
  DrawTextW(Utf8ToSynUnicode(s), withNewLine);
end;

procedure TGdiPages.DrawTitle(const s: SynUnicode; DrawBottomLine: boolean;
  OutlineLevel: integer; const aBookmark, aLink: string; aLinkNoBorder: boolean);
var
  H: integer;
  str: string;
begin
  if self = nil then
    exit; // avoid GPF
  CheckYPos;
  SaveLayout;
  try
    str := SynUnicodeToString(s);
    if not fInHeaderOrFooter then
      fCanvasText := fCanvasText + str + #13#10; // copy as text
    PrintFormattedLine(s, TitleFlags, aBookmark, aLink, true, aLinkNoBorder);
    if UseOutlines then
      AddOutline(str, OutlineLevel, fCurrentTextTop, fCurrentTextPage);
    if DrawBottomLine then
    begin
      H := (GetLineHeight * 15) shr 5;
      dec(fCurrentYPos, H);
      LineInternal(fPageMarginsPx.left, fPhysicalSizePx.x - fPageMarginsPx.right, false);
      inc(fCurrentYPos, H * 2);
    end;
  finally
    RestoreSavedLayout;
  end;                    
end;

procedure TGdiPages.DrawTextAt(s: SynUnicode; XPos: integer; const aLink: string;
  CheckPageNumber, aLinkNoBorder: boolean);
var
  i: integer;
  R: TRect;
  Size: TSize;
begin
  if (self = nil) or
     (s = '') then
    exit;
  CheckYPos;
  if CheckPageNumber then
  begin
    i := pos(PAGENUMBER, LowerCaseU(s));
    if i > 0 then
    begin
      Delete(s, i, 14);
      Insert(Utf8ToSynUnicode(Int32ToUtf8(fVirtualPageNum)), s, i);
    end;
  end;
  SetBkMode(fCanvas.Handle, TRANSPARENT);
  Size := TextExtent(fCanvas, s);
  R.Left := MmToPrinterPxX(XPos);
  case fAlign of
    taRight:
      dec(R.Left, Size.cx + 1);
    taCenter:
      dec(R.Left, Size.cx shr 1 + 1);
  end;
  R.Top := fCurrentYPos;
  TextOut(fCanvas, R.Left, R.Top, s);
  if not fInHeaderOrFooter then // copy as text on a new line
    fCanvasText := fCanvasText + SynUnicodeToString(s) + #13#10;
  if aLink <> '' then
  begin
    R.Right := R.Left + Size.cx;
    R.Bottom := R.Top + Size.cy;
    AddLink(aLink, PrinterToMM(R), 0, aLinkNoBorder);
  end;
end;

procedure TGdiPages.DrawAngledTextAt(const s: SynUnicode; XPos, Angle: integer);
var
  lf: TLogFont;
  OldFontHdl, NewFontHdl: HFont;
begin
  if (s = '') or
     (self = nil) then
    exit; // avoid GPF
  CheckYPos;
  XPos := MmToPrinterPxX(XPos);
  SetBkMode(fCanvas.Handle, TRANSPARENT);
  with fCanvas do
  begin
    if GetObject(Font.Handle, SizeOf(lf), @lf) = 0 then
      exit;
    lf.lfEscapement := Angle * 10;
    lf.lfOrientation := Angle * 10;
    lf.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    NewFontHdl := CreateFontIndirect(lf);
    OldFontHdl := selectObject(handle, NewFontHdl);
  end;
  TextOut(fCanvas, XPos, fCurrentYPos, s);
  selectObject(fCanvas.handle, OldFontHdl);
  DeleteObject(NewFontHdl);
  if not fInHeaderOrFooter then
    fCanvasText := fCanvasText + s + #13#10; // copy as text on a new line
end;

function TGdiPages.MmToPrinter(const R: TRect): TRect;
begin
  if self = nil then
  begin
    FillCharFast(result, SizeOf(result), 0);
    exit; // avoid GPF
  end;
  result.left := MmToPrinterPxX(R.left);
  result.top := MmToPrinterPxY(R.top);
  result.right := MmToPrinterPxX(R.right);
  result.bottom := MmToPrinterPxY(R.bottom);
end;

function TGdiPages.PrinterToMM(const R: TRect): TRect;
begin
  if self = nil then
  begin
    FillCharFast(result, SizeOf(result), 0);
    exit; // avoid GPF
  end;
  result.left := PrinterPxToMmX(R.left);
  result.top := PrinterPxToMmY(R.top);
  result.right := PrinterPxToMmX(R.right);
  result.bottom := PrinterPxToMmY(R.bottom);
end;

procedure TGdiPages.DrawBox(left, top, right, bottom: integer);
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  left := MmToPrinterPxX(left);
  top := MmToPrinterPxY(top);
  right := MmToPrinterPxX(right);
  bottom := MmToPrinterPxY(bottom);
  with fCanvas do
  begin
    Pen.Width := MulDiv(fDefaultLineWidth, self.Font.Size, 8);
    if fsBold in self.Font.style then
      Pen.Width := Pen.Width + 1;
    MoveTo(left, top);
    LineTo(right, top);
    LineTo(right, bottom);
    LineTo(left, bottom);
    LineTo(left, top);
  end;
end;

procedure TGdiPages.DrawBoxFilled(left, top, right, bottom: integer; Color: TColor);
var
  SavedBrushColor: TColor;
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  left   := MmToPrinterPxX(left);
  top    := MmToPrinterPxY(top);
  right  := MmToPrinterPxX(right);
  bottom := MmToPrinterPxY(bottom);
  with fCanvas do
  begin
    Pen.Width := MulDiv(fDefaultLineWidth, self.Font.Size, 8);
    if fsBold in self.Font.style then
      Pen.Width := Pen.Width + 1;
    SavedBrushColor := Brush.Color;
    brush.Color := Color;
    rectangle(left, top, right, bottom);
    Brush.Color := SavedBrushColor;
  end;
end;

procedure TGdiPages.DrawBmp(rec: TRect; bmp: TBitmap);
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  PrintBitmap(fCanvas, MmToPrinter(rec), bmp);
end;

procedure TGdiPages.DrawBmp(bmp: TBitmap; bLeft, bWidth: integer;
  const Legend: string);
begin
  DrawGraphic(bmp, bLeft, bWidth, Legend);
end;

procedure TGdiPages.DrawGraphic(graph: TGraphic; bLeft, bWidth: integer;
 const Legend: SynUnicode);
var
  R: TRect;
  H: integer;
begin
  if (self = nil) or (graph = nil) or graph.Empty then
    exit; // avoid GPF
  // compute position and draw bitmap
  if bLeft = maxInt then
    // center graphic
    bLeft := PrinterPxToMmX(fPageMarginsPx.Left +
       (fPhysicalSizePx.x - fPageMarginsPx.Right - fPageMarginsPx.Left -
        MmToPrinterPxX(bWidth)) shr 1)
  else
    // draw left-aligned
    inc(bLeft, LeftMargin);
  R.Left := bLeft;
  R.Right := bLeft + bWidth;
  R.Bottom := (graph.Height * bWidth) div graph.Width;
  if Legend <> '' then
    H := LineHeight
  else
    H := 0;
  if not HasSpaceFor(R.Bottom + H) then
  begin
    NewPage;
    DoHeader;
    NewHalfLine;
  end;
  R.Top := CurrentYPos;
  inc(R.Bottom, R.Top);
  if graph.InheritsFrom(TBitmap) then
    DrawBmp(R, graph as TBitmap)
  else if graph.InheritsFrom(TMetaFile) then
    DrawMeta(R, graph as TMetaFile);
  CurrentYPos := R.Bottom;
  // draw optional caption bottom
  if Legend <> '' then
  begin
    SaveLayout;
    TextAlign := taCenter;
    Font.Style := [];
    Font.Size := (Font.Size * 3) shr 2; // smaller font for caption text
    DrawTextW(Legend);
    RestoreSavedLayout;
  end
  else
    NewHalfLine;
end;
                          
procedure TGdiPages.DrawMeta(rec: TRect; meta: TMetaFile);
var
  old: integer;
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  rec := MmToPrinter(rec);
  old := SaveDC(fCanvas.Handle);   // ensure safe metafile embedding
  PlayEnhMetaFile(fCanvas.Handle, meta.Handle, rec);
  RestoreDC(fCanvas.Handle, old);
end;

procedure TGdiPages.DrawArrow(Point1, Point2: TPoint; HeadSize: integer;
  SolidHead: boolean);
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  Point1.X := MmToPrinterPxX(Point1.X);
  Point1.Y := MmToPrinterPxY(Point1.Y);
  Point2.X := MmToPrinterPxX(Point2.X);
  Point2.Y := MmToPrinterPxY(Point2.Y);
  HeadSize := MmToPrinterPxX(max(HeadSize, 0));
  fCanvas.Pen.Width := MulDiv(fDefaultLineWidth, self.Font.Size, 8);
  DrawArrowInternal(fCanvas, Point1, Point2, HeadSize, SolidHead);
end;

procedure TGdiPages.DrawLine(doubleline: boolean);
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  LineInternal(
    fPageMarginsPx.left, fPhysicalSizePx.x - fPageMarginsPx.right, doubleline);
  NewLine;
end;

procedure TGdiPages.DrawDashedLine;
var
  Y: integer;
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  with fCanvas do
  begin
    Pen.Width := 1;
    Pen.Style := psDash;
    Y := fCurrentYPos + (GetLineHeight shr 1) - (Pen.Width shr 1);
    MoveTo(fPageMarginsPx.left, Y);
    LineTo(fPhysicalSizePx.x - fPageMarginsPx.right, Y);
    Pen.Style := psSolid;
  end;
  NewLine;
end;

procedure TGdiPages.DrawColumnLine(ColIndex: integer; aAtTop: boolean;
  aDoDoubleLine: boolean);
var
  Y: integer;
begin
  if aAtTop then
    Y := fCurrentYPos - 1
  else
    Y := fCurrentYPos + fLineHeight + 1;
  with fColumns[ColIndex] do
    LineInternal(Y, ColLeft, ColRight, aDoDoubleLine);
end;

procedure TGdiPages.NewLine;
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  inc(fCurrentYPos, GetLineHeight);
  fForcedLeftOffset := -1;
//  fCanvasText := fCanvasText+#13#10;
end;

procedure TGdiPages.NewHalfLine;
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  inc(fCurrentYPos, GetLineHeight shr 1);
//  fCanvasText := fCanvasText+#13#10;
end;

procedure TGdiPages.NewLines(count: integer);
begin
  if self = nil then
    exit; // avoid GPF
  CheckHeaderDone;
  if count < 1 then
    exit;
  inc(fCurrentYPos, GetLineHeight * count);
//  fCanvasText := fCanvasText+#13#10;
end;

procedure TGdiPages.NewPage(ForceEndGroup: boolean);
begin
  if self = nil then
    exit; // avoid GPF
  if ForceEndGroup then
    EndGroup
  else if Assigned(fGroupPage) then
    raise EReport.CreateUtf8(
      'Cannot call %.NewPage within a group block', [self]);
  CheckHeaderDone;
  NewPageInternal;
end;

procedure TGdiPages.NewPageIfAnyContent;
begin
  if self = nil then
    exit; // avoid GPF
  if fHeaderDone then
    NewPage;
end;

procedure TGdiPages.BeginGroup;
begin
  if self = nil then
    exit; // avoid GPF
  if not fHeaderDone then
    exit; // i.e. haven't even started a page yet
  if Assigned(fGroupPage) then
    raise EReport.CreateUtf8('%.BeginGroup: Group already started', [self]);
  if not GroupsMustBeOnSamePage then
  begin
    // Group "light" implementation
    if fHeaderDone and not HasSpaceForLines(20) then
      NewPageInternal;
    exit;
  end;
  // make sure there's room for at least 2 lines otherwise just start a new page
  // (a group surely contains at least 2 lines )
  if not HasSpaceForLines(2) then
  begin
    NewPageInternal;
    exit;
  end;
  fGroupVerticalSpace := fPhysicalSizePx.y - fCurrentYPos -
                          fPageMarginsPx.bottom - fFooterHeight;
  fColumnsUsedInGroup := false;
  fColumnHeaderInGroup := false;
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  fGroupPage := CreateMetaFile(
    fPhysicalSizePx.x, fGroupVerticalSpace + fPhysicalOffsetPx.Y);
  fCanvas := CreateMetafileCanvas(fGroupPage);
  fGroupVerticalPos := fCurrentYPos;
  fCurrentYPos := 0;
  fBeforeGroupText := fCanvasText;
  fCanvasText := '';
end;

procedure TGdiPages.EndGroup;
begin
  if self = nil then
    exit; // avoid GPF
  if not Assigned(fGroupPage) then
    exit;
  FreeAndNil(fCanvas); //closes fGroupPage canvas
  fCanvas := CreateMetafileCanvas(fCurrentMetaFile);
  fCanvas.Draw(0, 0, fCurrentMetaFile);     //re-draw the last page
  fCanvas.Draw(0, fGroupVerticalPos, fGroupPage); //add the Group data
  FreeAndNil(fGroupPage);                       //destroy Group metafile
  inc(fCurrentYPos, fGroupVerticalPos);
  fCanvasText := fBeforeGroupText + fCanvasText;
  fBeforeGroupText := '';
end;

function TGdiPages.CurrentGroupPosStart: integer;
begin
  if self = nil then
    result := 0
  else
  begin
    if Assigned(fGroupPage) then
      result := fGroupVerticalPos
    else
      result := fPageMarginsPx.top;
    result := PrinterPXtoMmY(result);
  end;
end;

const // zoom percentages for popup menu entries
  MenuZoom: array[0..6] of byte = (25, 50, 75, 100, 125, 150, 200);

procedure TGdiPages.EndDoc;
var
  PC: PChar;
  i, n, aX: integer;
  Men: TGdiPagePreviewButton;
  M, Root: TMenuItem;
  Page: TMetaFile;
  s: string;
begin
  if self = nil then
    exit; // avoid GPF
  fLinksCurrent := -1;
  EndGroup;
  DoFooter;
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  n := length(fPages);
  if (n > 1) and
     not HeaderDone then
  begin
    // cancel the last page if it hasn't been started ...
    FreeAndNil(fCurrentMetaFile);
    dec(n);
    SetLength(fPages, n);
  end
  else
    FlushPageContent;
  if (n > 0) and
     (fPagesToFooterText <> '') then
    // add 'Page #/#' caption at the specified position
    for i := 0 to n - 1 do
    begin
      Page := CreateMetaFile(fPages[i].SizePx.X, fPages[i].SizePx.Y);
      try
        fCanvas := CreateMetafileCanvas(Page);
        fCanvas.Draw(0, 0, GetMetaFileForPage(i)); // re-draw the original page
        s := format(fPagesToFooterText, [i + 1, n]); // add 'Page #/#' caption
        aX := fPagesToFooterAt.X;
        if aX < 0 then
          aX := fPages[i].SizePx.X - fPages[i].MarginPx.Right;
        SavedState := fPagesToFooterState;
        if TextAlign = taRight then
          dec(aX, fCanvas.TextWidth(s));
        with fPages[i] do
          fCanvas.TextOut(aX, SizePx.Y - MarginPx.bottom - fFooterHeight +
            fFooterGap + fPagesToFooterAt.Y, s);
        FreeAndNil(fCanvas);
        SetMetaFileForPage(i, Page); // replace page content
      finally
        Page.Free;
      end;
    end;
  // OK, all Metafile pages have now been created and added to Pages[]
  if Assigned(fOnDocumentProducedEvent) then
    fOnDocumentProducedEvent(self); // notify report just generated
  fCurrPreviewPage := 1;
  if Assigned(fPreviewPageChangedEvent) then
    fPreviewPageChangedEvent(self); // notify page changed
  Invalidate;
  // update popup menu content
  if PopupMenu = nil then // caller may have created a TPopupMenu instance
    PopupMenu := PopupMenuClass.Create(self)
  else
    PopupMenu.Items.Clear;
  PopupMenu.OnPopup := PopupMenuPopup;
  PC := pointer(string(sReportPopupMenu1));
  // 'Next,Previous,GotoPage,Zoom,Bookmarks,CopyasText,Print,PDF,Close,Pagefit,Pagewidth'
  for Men := rNextPage to rClose do
    NewPopupMenuItem(GetNextItemS(PC), -ord(Men)).Enabled :=
      (Men < rPrint) or
      (Men = rClose) or
      ((Men = rPrint) and fHasPrinterInstalled) or
      ((Men = rExportPdf) {$ifdef USEPDFPRINTER} and fHasPDFPrinterInstalled{$endif});
  PopupMenu.Items[ord(rClose) - 1].Visible := false;
  M := PopupMenu.Items[ord(rZoom) - 1];
  NewPopupMenuItem(GetNextItemS(PC), -1000 - PAGE_FIT, M);
  NewPopupMenuItem(GetNextItemS(PC), -1000 - PAGE_WIDTH, M);
  for i := 0 to high(MenuZoom) do
    NewPopupMenuItem(format('%d %%', [MenuZoom[i]]), -1000 - MenuZoom[i], M);
  Root := PopupMenu.Items[ord(rBookmarks) - 1];
  if UseOutlines and
     (fOutline.Count > 0) then
  begin
    Root.Enabled := true;
    M := Root;
    for i := 0 to fOutline.Count - 1 do
      with TGdiPageReference(fOutline.Objects[i]) do
      begin
        while (M <> Root) and
              (cardinal(-2000 - M.Tag) < cardinal(fOutline.Count)) and
              (Rect.Bottom <=
                TGdiPageReference(fOutline.Objects[-2000 - M.Tag]).Rect.Bottom) do
          M := M.Parent;
        M := NewPopupMenuItem(fOutline[i], -2000 - i, M);
      end;
  end
  else
    Root.Enabled := false;
end;

function TGdiPages.PrintPages(PrintFrom, PrintTo: integer): boolean;
var
  i: integer;
  rec: TRect;
  CheckCurrentPtr: string;
  UseStretchDraw: boolean;
  Bmp: TBitmap;
begin
  result := false;
  if self = nil then
    exit; // avoid GPF
  if not fHasPrinterInstalled then
    raise EReport.CreateUtf8(
      '%.PrintPages: no printer driver is currently installed', [self]);
  if PrintFrom < 0 then
    with TPrintDialog.Create(nil) do
    try
      Options := [poPageNums];
      MinPage := 1;
      MaxPage := PageCount;
      FromPage := 1;
      ToPage := PageCount;
      if not Execute then
        exit;
      PrintFrom := FromPage;
      PrintTo := ToPage;
    finally
      Free;
    end;
  result := true;
  // ideally, the user has changed printers BEFORE generating a report, but
  // if they want a report sent to a different printer then use StretchDraw ...
  CheckCurrentPtr := CurrentPrinterName;
  if CheckCurrentPtr <> fCurrentPrinter then
  begin
    GetPrinterParams; // also updates fCurrentPrinter
    UseStretchDraw := true;
  end
  else
    UseStretchDraw := false;
  PrintFrom := max(PrintFrom - 1, 0);
  if PrintTo = 0 then
    PrintTo := high(fPages)
  else
    PrintTo := min(PrintTo - 1, high(fPages));
  with Printer do
  begin
    if Caption = '' then
    {$ifndef USEPDFPRINTER}
      if ExportPdfApplication <> '' then
        Title := ExportPdfApplication
      else
    {$endif USEPDFPRINTER}
        Title := Application.Title
    else
      Title := Caption;
    Orientation := self.Orientation; // just in case fPrinter changed
    BeginDoc;
    try
      Screen.Cursor := crHourGlass;
      if ForcePrintAsBitmap then
      begin
        // slow/non-efficient printing as bitmap
        Bmp := TBitmap.Create;
        try
          Bmp.Width := GetDeviceCaps(handle, PHYSICALWIDTH);
          Bmp.Height := GetDeviceCaps(handle, PHYSICALHEIGHT);
          for i := PrintFrom to PrintTo do
            with fPages[i] do
            begin
              Bmp.Canvas.StretchDraw(Rect(0, 0, Bmp.Width, Bmp.Height),
                GetMetaFileForPage(i));
              Canvas.Draw(-OffsetPx.x, -OffsetPx.y, Bmp);
              if i < PrintTo then
                NewPage;
            end;
        finally
          Bmp.Free;
        end;
      end
      else
        for i := PrintFrom to PrintTo do
        begin
          // nb: the printer's page origin is fPhysicalOffsetPx so it's
          //     necessary to offset our rect by -OffsetPx ...
          if ForceScreenResolution then
          begin
            rec := Rect(0, 0, GetDeviceCaps(handle, PHYSICALWIDTH),
              GetDeviceCaps(handle, PHYSICALHEIGHT));
            OffsetRect(rec, -GetDeviceCaps(handle, PHYSICALOFFSETX), -
              GetDeviceCaps(handle, PHYSICALOFFSETY));
            Canvas.StretchDraw(rec, GetMetaFileForPage(i));
          end
          else
            with fPages[i] do
              if UseStretchDraw then
                Canvas.StretchDraw(Rect(-OffsetPx.x, -OffsetPx.y, SizePx.x -
                  OffsetPx.x, SizePx.y - OffsetPx.y), GetMetaFileForPage(i))
              else
                Canvas.Draw(-OffsetPx.x, -OffsetPx.y, GetMetaFileForPage(i));
          if i < PrintTo then
            NewPage;
        end;
      EndDoc;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TGdiPages.SetTabStops(const tabs: array of integer);
var
  i: PtrInt;
begin
  if self = nil then
    exit; // avoid GPF
  FillCharFast(fTab[0], MAXTABS * SizeOf(fTab[0]), 0);
  fTabCount := min(high(tabs) + 1, MAXTABS);
  // ignore trailing 0 tabs in array ...
  while (fTabCount > 0) and
        (tabs[fTabCount - 1] = 0) do
    dec(fTabCount);
  if fTabCount > 1 then
  begin
    if tabs[0] <= 0 then
      raise EReport.CreateUtf8(
        '%.SetTableStops(%): Tabs stops must be greater than 0',
        [self, tabs[0]]);
    fTab[0] := MmToPrinterPxX(tabs[0]);
    for i := 1 to fTabCount - 1 do
      if tabs[i] > tabs[i - 1] then
        fTab[i] := MmToPrinterPxX(tabs[i])
      else
        raise EReport.CreateUtf8(
          '%.SetTableStops(%): Tabs stops must be in ascending order',
          [self, tabs[i]]);
  end
  else if fTabCount = 1 then
  begin
    // if one tab set then use that tab as the interval for subsequent tabs
    for i := 0 to MAXTABS - 1 do
      fTab[i] := MmToPrinterPxX((i + 1) * tabs[0]);
    fTabCount := MAXTABS;
  end
  else
  begin
    // if no tabs set then default to tabs every 20mm
    for i := 0 to MAXTABS - 1 do
      fTab[i] := MmToPrinterPxX((i + 1) * 20);
    fTabCount := MAXTABS;
  end;
end;

function TGdiPages.GetPageMargins: TRect;
begin
  if self = nil then
    FillCharFast(result, SizeOf(result), 0)
  else
    with result do
    begin
      left := PrinterPxToMmX(fPageMarginsPx.left);
      top := PrinterPxToMmY(fPageMarginsPx.top);
      right := PrinterPxToMmX(fPageMarginsPx.right);
      bottom := PrinterPxToMmY(fPageMarginsPx.bottom);
    end;
end;

procedure TGdiPages.SetPageMargins(Rect: TRect);
begin
  fPageMarginsPx := MmToPrinter(Rect);
  if not fHeaderDone then                       
    fCurrentYPos := fPageMarginsPx.top;
end;

function TGdiPages.GetLeftMargin: integer;
begin
  if self = nil then
    result := 0
  else
    result := PrinterPxToMmX(fPageMarginsPx.left);
end;

procedure TGdiPages.SetLeftMargin(const Value: integer);
begin
  if self = nil then
    exit;
  fPageMarginsPx.Left := MmToPrinterPxX(Value);
end;

function TGdiPages.GetPaperSize: TSize;
begin
  if self = nil then
    FillCharFast(result, SizeOf(result), 0)
  else
  begin
    result.cx := PrinterPxToMmX(fPhysicalSizePx.X);
    result.cy := PrinterPxToMmY(fPhysicalSizePx.Y);
  end;
end;

procedure TGdiPages.AddLineToHeader(doubleline: boolean);
begin
  if self = nil then
    exit; // avoid GPF
  fHeaderLines.Add(THeaderFooter.Create(self, doubleline));
end;

procedure TGdiPages.AddLineToFooter(doubleline: boolean);
begin
  if self = nil then
    exit; // avoid GPF
  if fFooterLines.Count = 0 then
    CalcFooterGap;
  fFooterLines.Add(THeaderFooter.Create(self, doubleline));
  inc(fFooterHeight, GetLineHeight);
end;

procedure TGdiPages.AddTextToHeader(const s: SynUnicode);
begin
  if self <> nil then
    fHeaderLines.Add(THeaderFooter.Create(self, false, s, true));
end;

procedure TGdiPages.AddTextToHeaderAt(const s: SynUnicode; XPos: integer);
var
  Head: THeaderFooter;
begin
  if self = nil then
    exit; // avoid GPF
  Head := THeaderFooter.Create(self, false, s, true);
  Head.State.Flags := Head.State.Flags or ((XPos + 2) shl 16);
  fHeaderLines.Add(Head);
end;

procedure TGdiPages.AddTextToFooter(const s: SynUnicode);
begin
  if self = nil then
    exit; // avoid GPF
  if fFooterLines.Count = 0 then
    CalcFooterGap;
  fFooterLines.Add(THeaderFooter.Create(self, false, s, true));
  inc(fFooterHeight, GetLineHeight);
end;

procedure TGdiPages.AddTextToFooterAt(const s: SynUnicode; XPos: integer);
var
  Foot: THeaderFooter;
begin
  if self = nil then
    exit; // avoid GPF
  //todo - can't print at 0mm from left edge so raise exception
  if fFooterLines.Count = 0 then
    CalcFooterGap;
  Foot := THeaderFooter.Create(self, false, s, true);
  Foot.State.Flags := Foot.State.Flags or ((XPos + 2) shl 16);
  fFooterLines.Add(Foot);
end;

procedure TGdiPages.AddPagesToFooterAt(const PageText: string; XPos,
  YPosMultiplier: integer);
begin
  if fPagesToFooterText <> '' then
    exit; // only add once
  fPagesToFooterText := PageText;
  if XPos < 0 then
    fPagesToFooterAt.X := -1
  else
    fPagesToFooterAt.X := MmToPrinterPxX(XPos);
  fPagesToFooterAt.Y := fFooterHeight * YPosMultiplier;
  fPagesToFooterState := SavedState;
end;

function TGdiPages.GetColumnCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(fColumns);
end;

function TGdiPages.GetColumnInfo(index: integer): TColRec;
begin
  if self = nil then
  begin
    FillCharFast(result, SizeOf(result), 0);
    exit;
  end;
  if cardinal(index) >= cardinal(Length(fColumns)) then
    raise EReport.CreateUtf8(
      '%.GetColumnInfo(%): index out of range', [self, index]);
  with fColumns[index] do
  begin
    result.ColLeft := PrinterPxToMmX(ColLeft);
    result.ColRight := PrinterPxToMmX(ColRight);
    result.ColAlign := ColAlign;
    result.ColBold := ColBold;
  end;
end;

procedure TGdiPages.SetColumnAlign(index: integer; align: TColAlign);
begin
  if self = nil then
    exit; // avoid GPF
  if cardinal(index) >= cardinal(Length(fColumns)) then
    raise EReport.CreateUtf8(
      '%.SetColumnAlign(%): index out of range', [self, index])
  else
    fColumns[index].ColAlign := align;
end;

procedure TGdiPages.SetColumnBold(index: integer);
begin
  if self = nil then
    exit; // avoid GPF
  if cardinal(index) >= cardinal(Length(fColumns)) then
    raise EReport.CreateUtf8(
      '%.SetColumnBold(%): index out of range', [self, index])
  else
    fColumns[index].ColBold := true;
end;

procedure TGdiPages.AddColumn(left, right: integer; align: TColAlign; bold: boolean);
var
  n: PtrInt;
begin
  if self = nil then
    exit; // avoid GPF
  left := MmToPrinterPxX(left);
  right := MmToPrinterPxX(right);
  n := length(fColumns);
  if (n > 0) and
     (left < fColumns[n - 1].ColRight) then
    raise EReport.CreateUtf8(
      '%.AddColumn(%,%): columns overlap', [self, left, right]);
  SetLength(fColumns, n + 1);
  with fColumns[n] do
  begin
    ColLeft := left;
    ColRight := right;
    ColAlign := align;
    ColBold := bold;
  end;
end;

procedure TGdiPages.AddColumns(const PercentWidth: array of integer; align: TColAlign);
var
  i, sum, left, right, ww, n: integer;
begin
  if self = nil then
    exit; // avoid GPF
  ClearColumns;
  sum := 0;
  for i := 0 to high(PercentWidth) do
    inc(sum, abs(PercentWidth[i]));
  if sum <= 0 then
    exit;
  left := fPageMarginsPx.left;
  ww := fPhysicalSizePx.x - left - fPageMarginsPx.right;
  n := length(fColumns);
  SetLength(fColumns, n + length(PercentWidth));
  for i := 0 to high(PercentWidth) do
  begin
    right := left + (abs(PercentWidth[i]) * ww) div sum;
    // manual adding (no mm conversion -> exact width)
    with fColumns[i + n] do
    begin
      ColLeft := left;
      ColRight := right;
      if PercentWidth[i] < 0 then
        ColAlign := caCenter
      else
        ColAlign := align;
      ColBold := false;
    end;
    left := right;
  end;
end;

procedure TGdiPages.AddColumnHeaders(const headers: array of SynUnicode;
  WithBottomGrayLine, BoldFont: boolean; RowLineHeight, flags: integer);
var
  n, i: integer;
begin
  if self = nil then
    exit; // avoid GPF
  if flags = 0 then
  begin
    if BoldFont then
      Font.Style := [fsBold];
    flags := TextFormatsToFlags;
  end;
  n := length(fColumnHeaderList);
  SetLength(fColumnHeaderList, n + 1);
  fColumnHeaderList[n].flags := flags;
  SetLength(fColumnHeaderList[n].headers, Length(headers));
  for i := 0 to high(headers) do
    fColumnHeaderList[n].headers[i] := headers[i];
  fColumnHeaderPrinted := false;
  fColumnHeaderPrintedAtLeastOnce := false;
  fColumnsWithBottomGrayLine := WithBottomGrayLine;
  fColumnsRowLineHeight := RowLineHeight;
  if BoldFont then
    Font.Style := [];
end;

procedure TGdiPages.AddColumnHeadersFromCsv(var Csv: PWideChar;
  WithBottomGrayLine, BoldFont: boolean; RowLineHeight: integer);
begin
  if self <> nil then // avoid GPF
    AddColumnHeaders(CsvToArray(Csv, length(fColumns)), WithBottomGrayLine,
      BoldFont, RowLineHeight);
end;

procedure TGdiPages.DrawTextAcrossColsFromCsv(var Csv: PWideChar;
  BackgroundColor: TColor);
begin
  if self <> nil then // avoid GPF
    DrawTextAcrossCols(CsvToArray(Csv, length(fColumns)), [], BackgroundColor);
end;

procedure TGdiPages.DrawTextAcrossCols(const StringArray: array of SynUnicode;
  BackgroundColor: TColor);
begin
  DrawTextAcrossCols(StringArray, [], BackgroundColor);
end;

procedure TGdiPages.DrawTextAcrossCols(
  const StringArray, LinkArray: array of SynUnicode; BackgroundColor: TColor);

  function WrapText(s: SynUnicode; MaxWidth: integer;
    Lines: PSynUnicodeDynArray): integer;
  var
    j, k, sp: integer;
  begin
    result := 0; // returns the line count
    if Lines <> nil then
      SetLength(Lines^, 0);
    repeat
      if HasCRLF(s) or (TextWidthC(fCanvas, s) > MaxWidth) then
      begin
        j := 1;
        k := 1;
        sp := 0;
        while (j < length(s)) and
              (TextWidthC(fCanvas, copy(s, 1, j)) < MaxWidth) do
        begin
          k := j; // store last fitting character index
          if s[j] <= ' ' then
          begin
            sp := j; // mark space (=word delimiter) found
            if s[j] < ' ' then
              break; // #13,#10 will force word wrap here = next line
          end;
          inc(j);
        end;
        if sp = 0 then
          sp := k; // if no space found, use character wrapping
      end
      else
        sp := length(s) + 1;
      if sp <= 1 then
        sp := 2;
      if Lines <> nil then
      begin
        SetLength(Lines^, length(Lines^) + 1);
        Lines^[high(Lines^)] := copy(s, 1, sp - 1);
      end;
      inc(result); // update lines count
      s := trim(copy(s, sp, maxInt)); // trim ' ',#13,#10 for next line
    until s = '';
  end;

var
  RowRect: TRect;
  lh: integer;
  max, i, j, k, c, H, ParenthW, LinesCount, X: integer;
  s: SynUnicode;
  line: string;
  Lines: TSynUnicodeDynArray;
  PW: PWideChar;
  PWLen, Options: integer;
  size: TSize;
  r: TRect;
begin
  if self = nil then
    exit; // avoid GPF
  max := high(fColumns);
  if (max < 0) or
     (length(StringArray) = 0) then
    exit; // no column defined
  if High(StringArray) < max then
    max := High(StringArray);
  if max < 0 then
    exit; // nothing to draw
  // check enough place for this column content on the page
  lh := GetLineHeight;
  CheckYPos;
  LinesCount := 1; // by default, one line of text will be written
  if WordWrapLeftCols then
  begin
    // check if stay on current page after word wrap
    for j := 0 to max do
      with fColumns[j] do
        if (ColAlign = caLeft) and
           (ColRight > ColLeft) and
           (HasCRLF(StringArray[j]) or
            (TextWidthC(fCanvas, StringArray[j]) > ColRight - ColLeft)) then
        begin
          // calculate line counts
          k := WrapText(StringArray[j], ColRight - ColLeft, nil);
          if k > LinesCount then
            LinesCount := k; // calculate maximum line count
        end;
    if (LinesCount > 1) and not HasSpaceForLines(LinesCount) then
    begin
      NewPageInternal;
      CheckHeaderDone;
    end;
  end;
  if (fColumnHeaderList <> nil) and
     not fColumnHeaderPrinted then
  begin
    i := length(fColumnHeaderList) + 2;
    if not HasSpaceForLines(i) then
      NewPageInternal;
    PrintColumnHeaders;
  end;
  // prepare column write
  if Assigned(fGroupPage) then
    fColumnsUsedInGroup := true;
  ParenthW := fCanvas.TextWidth(')');
  RowRect.Top := fCurrentYPos;
  RowRect.Bottom := RowRect.Top + lh * LinesCount;
  RowRect.Right := fColumns[max].ColRight;
  if BackgroundColor <> clNone then
    with fCanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := BackgroundColor;
      RowRect.Left := fColumns[0].ColLeft;
      FillRect(RowRect);
      Brush.Style := bsClear;
      Font.Color := clAlways(BackgroundColor);
    end;
  // main loop, used to write column content
  line := '';
  for i := 0 to max do
  begin
    s := StringArray[i];
    line := line + SynUnicodeToString(s) + #9; // add column content + tab for report text
    if s <> '' then
      with fColumns[i], fCanvas do
        if ColRight > ColLeft then
        begin
          if ColBold then
            Font.Style := Font.Style + [fsBold];
          Options := ETO_CLIPPED {$ifndef FPC} or TextFlags{$endif};
          if Brush.Style <> bsClear then
            Options := Options or ETO_OPAQUE;
          InternalUnicodeString(s, PW, PWLen, @size);
          if (ColAlign = caCenter) and
             (size.cx > ColRight - ColLeft) then
            // overlapping centered -> draw right aligned
            RowRect.Left := ColRight - size.cx - ParenthW
          else
            case ColAlign of
              caLeft:
                begin
                  RowRect.Left := ColLeft;
                  if WordWrapLeftCols and
                     (ColRight > ColLeft) and
                     (HasCRLF(s) or
                      (size.cx > ColRight - ColLeft)) then
                  begin
                    // handle optional left aligned column content word wrap
                    WrapText(s, ColRight - ColLeft, @Lines); // word wrap s into Lines[]
                    dec(RowRect.Left, ParenthW);
                    for j := 0 to high(Lines) do
                    begin
                      InternalUnicodeString(Lines[j], PW, PWLen, @size);
                      if BiDiMode = bdRightToLeft then
                        X := ColRight - size.cx - ParenthW
                      else
                        X := ColLeft;
                      RowRect.Top := fCurrentYPos + lh * j;
                      ExtTextOutW(Handle, X, RowRect.Top, Options, @RowRect, PW,
                        PWLen, nil);
                    end;
                    RowRect.Top := fCurrentYPos;
                    if ColBold then
                      Font.Style := Font.Style - [fsBold];
                    Continue; // text was written as word-wrap -> write next column
                  end
                  else if BiDiMode = bdRightToleft then
                    RowRect.Left := ColRight - size.cx - ParenthW;
                end;
              caCenter:
                RowRect.Left := ColLeft + (ColRight - ColLeft - size.cx) shr 1;
              caRight:
                if BiDiMode = bdLeftToRight then
                  RowRect.Left := ColRight - size.cx - ParenthW;
              caCurrency:
                begin
                  if fNegsToParenthesesInCurrCols then
                    InternalUnicodeString(ConvertNegsToParentheses(s), PW, PWLen, @size);
                  RowRect.Left := ColRight - size.cx - ParenthW;
                  // no bdRightToleft handling necessary for caCurrency
                end;
            end;
          dec(RowRect.Left, ParenthW);
          ExtTextOutW(Handle, RowRect.Left + ParenthW, fCurrentYPos,
            Options, @RowRect, PW, PWLen, nil);
          if (i < length(LinkArray)) and
             (LinkArray[i] <> '') then
          begin
            r.Left := PrinterPxToMmX(RowRect.Left);
            r.Top := PrinterPxToMmX(RowRect.Top);
            r.right := PrinterPxToMmX(RowRect.left +
              (RowRect.right - fColumns[0].ColLeft) div (max + 1));
            r.Bottom := PrinterPxToMmX(RowRect.Bottom);
            AddLink(LinkArray[i], r);
          end;
          inc(RowRect.Left, size.cx + ParenthW);
          if ColBold then
            Font.Style := Font.Style - [fsBold];
        end;
  end;
  if not fDrawTextAcrossColsDrawingHeader or
     not fColumnHeaderPrintedAtLeastOnce then
  begin
    line[length(line)] := #13; // overwrite last #9
    line := line + #10;
    fCanvasText := fCanvasText + line; // append columns content to report text
  end;
  if BackgroundColor <> clNone then
    fCanvas.Font.Color := clBlack;
  if not fDrawTextAcrossColsDrawingHeader and
     (fColumnsRowLineHeight > LinesCount) then
    // custom space for Row before bottom gray line
    LinesCount := fColumnsRowLineHeight;
  for i := 2 to LinesCount do
    NewLine;
  if fColumnsWithBottomGrayLine and
     (RowRect.Right <> 0) then
  begin
    c := fCanvas.Pen.Color;
    fCanvas.Pen.Color := clLtGray;
    H := lh shr 1 - (lh * 15) shr 4;
    dec(fCurrentYPos, H);
    LineInternal(GetColumnRec(0).ColLeft, RowRect.Right, false);
    inc(fCurrentYPos, H);
    fCanvas.Pen.Color := c;
  end;
  NewLine;
end;

procedure TGdiPages.DrawLinesInCurrencyCols(doublelines: boolean);
var
  i: PtrInt;
begin
  if self = nil then
    exit; // avoid GPF
  CheckYPos;
  if (fColumnHeaderList <> nil) and
     not fColumnHeaderPrinted then
  begin
    i := length(fColumnHeaderList) + 2;
    if not HasSpaceForLines(i) then
      NewPageInternal;
    PrintColumnHeaders;
  end;
  for i := 0 to high(fColumns) do
    with fColumns[i] do
      if ColAlign = caCurrency then
        LineInternal(ColLeft, ColRight, doublelines);
  NewLine;
end;

procedure TGdiPages.ColumnHeadersNeeded;
begin
  if self = nil then
    exit; // avoid GPF
  fColumnHeaderPrinted := false;
end;

procedure ClearObjects(List: TStringList);
var
  i: PtrInt;
begin
  for i := 0 to List.Count - 1 do
    List.Objects[i].Free;
  List.Clear;
end;

procedure TGdiPages.Clear;
begin
  if self = nil then
    exit; // avoid GPF
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  if Assigned(fGroupPage) then
    FreeAndNil(fGroupPage);
  FreeAndNil(fCurrentMetaFile);
  SetLength(fPages, 0);
  ClearObjects(fBookmarks);
  ClearObjects(fLinks);
  ClearObjects(fOutline);
  ClearHeaders;
  ClearFooters;
  ClearColumns;
  SetTabStops([20]);
  fCanvasText := '';
  fLinksCurrent := -1;
  fSavedCount := 0;
end;

procedure TGdiPages.ClearHeaders;
begin
  if self = nil then
    exit; // avoid GPF
  fHeaderLines.Clear;
end;

procedure TGdiPages.ClearFooters;
begin
  if self = nil then
    exit; // avoid GPF
  fFooterLines.Clear;
  fPagesToFooterText := '';
end;

procedure TGdiPages.ClearColumns;
begin
  if self = nil then
    exit; // avoid GPF
  SetLength(fColumns, 0);
  ClearColumnHeaders;
end;

procedure TGdiPages.ClearColumnHeaders;
begin
  if self = nil then
    exit; // avoid GPF
  fColumnHeaderList := nil;
end;

function TGdiPages.CreatePictureMetaFile(Width, Height: integer;
  out MetaCanvas: TCanvas): TMetaFile;
begin
  if self = nil then
    result := nil
  else
  begin
    result := CreateMetaFile(MmToPrinterPxX(Width), MmToPrinterPxY(Height));
    MetaCanvas := CreateMetafileCanvas(result);
  end;
end;

procedure TGdiPages.DrawTextFmt(const s: string; const Args: array of const;
  withNewLine: boolean);
begin
  DrawText(format(s, Args), withNewLine);
end;

function TGdiPages.TitleFlags: integer;
begin
  result := ((Font.Size * 12) div 10) or FORMAT_BOLD or FORMAT_LEFT;
end;

function TGdiPages.TextWidth(const Text: SynUnicode): integer;
begin
  if self = nil then
    result := 0
  else
  begin
    if fCanvas = nil then
      result := TextWidthC(Canvas, Text)
    else
      result := TextWidthC(fCanvas, Text);
    result := PrinterPxToMmX(result);
  end;
end;

procedure CopyMenus(Owner: TComponent; Source, Dest: TMenuItem);
var
  i: integer;
  Sub: TMenuItem;
begin
  for i := 0 to Source.Count - 1 do
    with Source.Items[i] do
    begin
      Sub := TMenuItem.Create(Owner);
      Sub.Tag := Tag;
      Sub.OnClick := OnClick;
      Sub.Caption := Caption;
      Dest.Add(Sub);
      CopyMenus(Owner, Source.Items[i], Sub);
    end;
end;

procedure TGdiPages.ShowPreviewForm(VisibleButtons: TGdiPagePreviewButtons);
const
  PANELWIDTH = 128;
var
  OldParent: TWinControl;
  i, y, W: integer;
  M: TMenuItem;
  LeftPanel: TPanel;
begin
  if self = nil then
    exit; // avoid GPF
  PreviewForm := TForm.Create(nil);
  try
    PreviewForm.Position := poScreenCenter;
    PreviewForm.Height := Screen.Height - 64;
    PreviewForm.Caption := Caption;
    PreviewForm.Font.Name := 'Tahoma';
    with PaperSize do
    begin
      if cy = 0 then
        y := 1
      else
        y := cy;
      PreviewForm.Width := (cx * PreviewForm.Height) div y + (64 + PANELWIDTH);
    end;
    if PreviewForm.Width > Screen.WorkAreaWidth then
      PreviewForm.WindowState := wsMaximized;
    LeftPanel := TPanel.Create(PreviewForm);
    LeftPanel.Parent := PreviewForm;
    LeftPanel.Width := PANELWIDTH;
    LeftPanel.Align := alLeft;
    W := LeftPanel.ClientWidth - 8;
    PreviewPageCountLabel := TLabel.Create(PreviewForm);
    PreviewPageCountLabel.Transparent := true;
    PreviewPageCountLabel.Parent := LeftPanel;
    PreviewPageCountLabel.SetBounds(4, 24, W - 4, 24);
    PreviewPageCountLabel.Alignment := Classes.taCenter;
    PreviewPageCountLabel.AutoSize := false;
    PreviewPageCountLabel.Caption := format(sPageN, [Page, PageCount]);
    PopupMenuPopup(nil); // refresh PopupMenu.Items[]
    SetLength(PreviewButtons, PopupMenu.Items.Count);
    y := 48;
    for i := 0 to High(PreviewButtons) do
    begin
      M := PopupMenu.Items[i];
      PreviewButtons[i] := TButton.Create(PreviewForm);
      with PreviewButtons[i] do
      begin
        Parent := LeftPanel;
        SetBounds(4, y, W, 32);
        Enabled := M.Enabled;
        Caption := M.Caption;
        Tag := M.Tag;
        OnClick := PopupMenuItemClick;
        if M.Count > 0 then
        begin
          PopupMenu := PopupMenuClass.Create(PreviewForm);
          CopyMenus(PreviewForm, M, PopupMenu.Items);
        end;
        if TGdiPagePreviewButton(i + 1) in VisibleButtons then
          case TGdiPagePreviewButton(i + 1) of
            rPrint:
              begin
                Height := 60;
                inc(y, 64);
                Default := true;
              end;
            rClose,
            rNextPage,
            rPreviousPage:
              begin
                Height := 48;
                inc(y, 52);
              end;
            rGotoPage,
            rZoom,
            rBookmarks,
            rExportPdf:
              inc(y, 48);
          else
            inc(y, 36);
          end
        else
        begin
          M.Visible := false;
          Visible := false;
        end;
      end;
    end;
    OldParent := Parent;
    Parent := PreviewForm;
    align := alClient;
    Zoom := PAGE_FIT;
    try
      PreviewForm.ActiveControl := self;
      PreviewForm.ShowModal;
    finally
      Parent := OldParent;
    end;
  finally
    FreeAndNil(PreviewForm);
    Finalize(PreviewButtons);
  end;
end;

function TGdiPages.GetRightMarginPos: integer;
begin
  result := PrinterPxToMmX(fPhysicalSizePx.x - fPageMarginsPx.right);
end;

function TGdiPages.NewPopupMenuItem(const aCaption: string; Tag: integer;
  SubMenu: TMenuItem; OnClick: TNotifyEvent; ImageIndex: integer): TMenuItem;
begin
  if (self = nil) or
     (PopupMenu = nil) then
  begin
    result := nil;
    exit;
  end;
  result := TMenuItem.Create(PopupMenu);
  result.Caption := aCaption;
  result.Tag := Tag;
  if Assigned(OnClick) then
    result.OnClick := OnClick
  else
    result.OnClick := PopupMenuItemClick;
  if ImageIndex >= 0 then
    result.ImageIndex := ImageIndex;
  if SubMenu = nil then
    PopupMenu.Items.Add(result)
  else
    SubMenu.Add(result);
end;

procedure TGdiPages.PopupMenuItemClick(Sender: TObject);
var
  Comp: TComponent absolute Sender;
  i: integer;
begin
  if not Sender.InheritsFrom(TComponent) then
    exit;
  if Assigned(OnPopupMenuClick) then
    if (Comp.Tag = 0) or
       (Comp.Tag > PageCount) then
      OnPopupMenuClick(Sender); // only notify custom events
  case -Comp.Tag of
    ord(rNone):
      exit;
    ord(rNextPage):
      Page := Page + 1;
    ord(rPreviousPage):
      Page := Page - 1;
    ord(rPageAsText):
      if Page > 0 then
        Clipboard.AsText := fPages[Page - 1].Text;
    ord(rPrint):
      if PrintPages(-1, -1) then
        if PreviewForm <> nil then
          PreviewForm.Close;
    ord(rExportPdf):
      ExportPdf('', true);
    ord(rClose):
      if PreviewForm <> nil then
        PreviewForm.Close;
    ord(rGotoPage),
    ord(rZoom),
    ord(rBookmarks):
      if Sender.InheritsFrom(TButton) and
         (PreviewButtons <> nil) then
        with PreviewButtons[-1 - Comp.Tag], PreviewForm.ClientToScreen(
               Point(left, top + Height)) do
          PopupMenu.Popup(X, Y);
    991..1999:
      // handle -1000-PAGE_WIDTH
      Zoom := -1000 - Comp.Tag;
    2000..4000:
      begin
        // handle -2000-OutlineIndex
        i := -2000 - Comp.Tag;
        if cardinal(i) < cardinal(fOutline.Count) then
          with TGdiPageReference(fOutline.Objects[i]) do
            GotoPosition(Page, Rect.Top);
      end;
  else
    if cardinal(Comp.Tag) <= cardinal(PageCount) then
      Page := Comp.Tag;
  end;
  if PreviewForm <> nil then
    SetFocus;
end;

procedure TGdiPages.InternalUnicodeString(const s: SynUnicode; var PW: PWideChar;
  var PWLen: integer; size: PSize);
begin
  if Assigned(OnStringToUnicode) then
  begin
    fInternalUnicodeString := OnStringToUnicode(s);
    PW := pointer(fInternalUnicodeString);
    PWLen := length(fInternalUnicodeString);
  end
  else
  begin
    PW := pointer(s);
    PWLen := length(s);
  end;
  if size <> nil then
  begin
    size^.cx := 0;
    size^.cy := 0;
    GetTextExtentPoint32W(fCanvas.Handle, PW, PWLen, size^);
  end;
end;

procedure TGdiPages.PopupMenuPopup(Sender: TObject);
var
  P: PChar;
  PageFromTo, PageN: string;
  M, M2: TMenuItem;
  i, j, k: integer;

  procedure AddPage(Menu: TMenuItem);
  begin
    NewPopupMenuItem(format(PageN, [i]), i, Menu).Enabled := i <> Page;
  end;

begin
  with PopupMenu.Items do
    if count = 0 then
      exit
    else
      while count > ord(rClose) do
        Delete(ord(rClose)); // delete after "Close" entry
  PopupMenu.Items[Ord(rNextPage) - 1].Enabled := Page < PageCount;
  PopupMenu.Items[Ord(rPreviousPage) - 1].Enabled := Page > 1;
  M := PopupMenu.Items[Ord(rGoToPage) - 1];
  while M.Count > 0 do
    M.Delete(0);
  M.Enabled := PageCount > 1;
  if PageCount >= 1 then
  begin // add 'Go to Page' sub menus (group by 10 pages)
    P := pointer(string(sReportPopupMenu2));
    PageFromTo := GetNextItemS(P); // Pages %d to %d
    PageN := GetNextItemS(P);      // Page %d
    if PageCount > 10 then
    begin
      for j := 0 to PageCount div 10 do
      begin
        k := j * 10 + 1;
        if k > PageCount then
          break;
        M2 := NewPopupMenuItem(format(PageFromTo, [k, k + 9]), -800, M);
        // Tag=-800 -> no OnClick event triggered for this entry
        for i := k to k + 9 do
          if i > PageCount then
            break
          else
            AddPage(M2);
      end;
    end
    else
      for i := 1 to PageCount do
        AddPage(M);
  end;
  if Assigned(OnPopupMenuPopup) then
    OnPopupMenuPopup(Sender);
end;

{$ifndef USEPDFPRINTER}
function TGdiPages.ExportPdfStream(aDest: TStream): boolean;
var
  PDF: TPdfDocument;
  BackgroundImage: TPdfImage;
  page: TPdfPage;
  i: integer;
begin
  try
    PDF := TPdfDocument.Create(UseOutlines, 0, ExportPdfLevel,
      TPdfEncryption.New(ExportPdfEncryptionLevel,
        ExportPdfEncryptionUserPassword, ExportPdfEncryptionOwnerPassword,
        ExportPdfEncryptionPermissions));
    try
      PDF.GeneratePdf15File := ExportPdfGeneratePdf15File;
      //PDF.CompressionMethod := cmNone;
      with PDF.Info do
      begin
        Title := SysUtils.Trim(Caption);
        if ExportPdfApplication = '' then
          Creator := trim(Application.Title)
        else
          Creator := trim(ExportPdfApplication);
        Author := ExportPdfAuthor;
        Subject := ExportPdfSubject;
        Keywords := ExportPdfKeywords;
      end;
      PDF.EmbeddedTTF := ExportPdfEmbeddedTTF;
      {$ifndef NO_USE_UNISCRIBE}
      PDF.UseUniscribe := ExportPdfUseUniscribe;
      {$endif NO_USE_UNISCRIBE}
      PDF.UseFontFallBack := ExportPdfUseFontFallBack;
      if ExportPdfFontFallBackName <> '' then
        PDF.FontFallBackName := ExportPdfFontFallBackName;
      PDF.ForceJPEGCompression := ExportPdfForceJPEGCompression;
      if ExportPdfBackground = nil then
        BackgroundImage := nil
      else
      begin
        BackgroundImage := TPdfImage.Create(PDF, ExportPdfBackground, true);
        PDF.AddXObject('BackgroundImage', BackgroundImage);
      end;
      PDF.SaveToStreamDirectBegin(aDest);
      for i := 0 to PageCount - 1 do
        with Pages[i] do
        begin
        // this loop will do all the magic :)
          PDF.DefaultPageWidth := PdfCoord(25.4 * SizePx.X / fPrinterPxPerInch.x);
          PDF.DefaultPageHeight := PdfCoord(25.4 * SizePx.Y / fPrinterPxPerInch.y);
          page := PDF.AddPage;
          if BackgroundImage <> nil then
            PDF.Canvas.DrawXObject(0, 0, page.PageWidth, page.PageHeight,
              'BackgroundImage');
          RenderMetaFile(PDF.Canvas, GetMetaFileForPage(i), Screen.PixelsPerInch
            / fPrinterPxPerInch.x, Screen.PixelsPerInch / fPrinterPxPerInch.y);
          PDF.SaveToStreamDirectPageFlush;
        end;
      PDF.SaveToStreamDirectEnd;
    finally
      PDF.Free;
    end;
    result := true;
  except
    result := false;
  end;
end;
{$endif USEPDFPRINTER}

function TGdiPages.ExportPdf(aPdfFileName: TFileName; ShowErrorOnScreen,
  LaunchAfter: boolean): boolean;
{$ifdef USEPDFPRINTER}
var
  DefaultPrinter: integer;
{$else}

  function ValidFileName(const FN: TFileName): TFileName;
  var
    i: integer;
  begin
    result := FN;
    for i := length(result) downto 1 do
      if ord(result[i]) in [ord('/'), ord(':'), ord('\'), ord('.')] then
        delete(result,i,1);
    i := length(result);
    while (i > 0) and
          (ord(result[i]) in [ord(' '), ord('-')]) do
      dec(i);
    SetLength(result, i);
      result := trim(result);
  end;

var
  PDFFileName: TFileName;
  PDFFile:
  TFileStream;
  i: integer;
  Name: string;
  TempDir: TFileName;
{$endif USEPDFPRINTER}
begin
  result := false;
  if self = nil then
    exit;
  if PageCount > 10 then
    Screen.Cursor := crHourGlass;
{$ifdef USEPDFPRINTER}
  if HasPDFPrinterInstalled then
  begin
    DefaultPrinter := Printer.PrinterIndex;
    Printer.PrinterIndex := fPDFPrinterIndex;
    PrintPages(0, 0);
    Printer.PrinterIndex := DefaultPrinter;
  end; 
{$else}
  // use the Synopse PDF engine
  if aPdfFileName='' then
  with TSaveDialog.Create(nil) do
  try
    TempDir := GetCurrentDir;
    Filter := sPDFFile+' (*.pdf)|*.pdf';
    Title := Caption;
    FileName := ValidFileName(Caption);
    DefaultExt := 'pdf';
    Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];
    repeat
      if not Execute then
        exit;
      PDFFileName := FileName;
      i := FileCreate(PDFFileName); // test file create (pdf not already opened)
      if i > 0 then
        break;
      MessageBox(0, pointer(Format(SIniFileWriteError,[PDFFileName])),
        nil, MB_ICONERROR);
    until false;
    FileClose(i);
  finally
    SetCurrentDir(TempDir); // allow unplug e.g. any USB
    Free;
  end
  else
    PDFFileName := aPdfFileName;
  try
    PDFFile := TFileStream.Create(PDFFileName, fmCreate);
    try
      ExportPdfStream(PDFFile);
    finally
      PDFFile.Free;
    end;
    if LaunchAfter then
      ShellExecute(Application.MainForm.Handle, 'open',
        pointer(PDFFileName), nil, nil, SW_NORMAL);
  except
    on E: Exception do
    begin
      // show any error raised during PDF creation
      if ShowErrorOnScreen then
        MessageBox(0, pointer(E.Message), pointer(Name), MB_ICONERROR);
      exit;
    end;
  end;
{$endif USEPDFPRINTER}
  result := true;
  if PageCount > 10 then
    Screen.Cursor := crDefault;
end;

procedure TGdiPages.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  R: TRect;
begin
  Message.result := 1; // no erasing is necessary after this method call
  if Message.DC = 0 then
    exit;
  // erase outside the preview surface
  R.Left := 0;
  R.Right := fPreviewSurface.left;
  R.Top := 0;
  R.Bottom := Height;
  FillRect(Message.DC, R, Brush.Handle);
  R.Left := R.Right + fPreviewSurface.Width;
  R.Right := Width;
  FillRect(Message.DC, R, Brush.Handle);
  R.Left := 0;
  R.Bottom := fPreviewSurface.Top;
  FillRect(Message.DC, R, Brush.Handle);
  R.Top := fPreviewSurface.Top + fPreviewSurface.Height;
  R.Bottom := Height;
  FillRect(Message.DC, R, Brush.Handle);
end;

procedure TGdiPages.AppendRichEdit(RichEditHandle: HWnd;
  EndOfPagePositions: PIntegerDynArray);
var
  Range: TFormatRange;
  LogX, LogY, LastChar, MaxLen, OldMap: integer;
  TextLenEx: TGetTextLengthEx; // RichEdit 2.0 Window Class
begin
  if (self <> nil) and
     (fCanvas <> nil) then
    with Range do
    begin
      LogX := GetDeviceCaps(fCanvas.Handle, LOGPIXELSX);
      LogY := GetDeviceCaps(fCanvas.Handle, LOGPIXELSY);
      rcPage.Left := (fPageMarginsPx.Left * 1440) div LogX;
      rcPage.Right :=
        ((fPhysicalSizePx.x - fPageMarginsPx.Right) * 1440) div LogX;
      rcPage.Top := ((fPageMarginsPx.Top + fHeaderHeight) * 1440) div LogY;
      rcPage.Bottom :=
        ((fPhysicalSizePx.y - fPageMarginsPx.Bottom - fFooterHeight) * 1440) div LogY;
      CheckHeaderDone;
      rc := rcPage;
      rc.Top := (fCurrentYPos * 1440) div LogY;
      LastChar := 0;
      TextLenEx.flags := GTL_DEFAULT;
      TextLenEx.codepage := CP_ACP;
      MaxLen := SendMessage(
        RichEditHandle, EM_GETTEXTLENGTHEX, PtrInt(@TextLenEx), 0);
      chrg.cpMax := -1;
      OldMap := SetMapMode(hdc, MM_TEXT);
      try
        SendMessage(RichEditHandle, EM_FORMATRANGE, 0, 0);
        repeat
          chrg.cpMin := LastChar;
          hdc := fCanvas.Handle;
          hdcTarget := hdc;
          LastChar := SendMessage(
            RichEditHandle, EM_FORMATRANGE, 1, PtrInt(@Range));
          if EndOfPagePositions <> nil then
            AddInteger(EndOfPagePositions^, LastChar);
          if cardinal(LastChar) >= cardinal(MaxLen) then
            break;
          NewPageInternal;
          DoHeader;
          rc := rcPage;
        until false;
        fCurrentYPos := (rc.Bottom * LogY) div 1440;
      finally
        SendMessage(RichEditHandle, EM_FORMATRANGE, 0, 0);
        SetMapMode(hdc, OldMap);
      end;
    end;
end;

function TGdiPages.AddBookMark(const aBookmarkName: string;
  aYPosition: integer): boolean;
begin
  if fBookmarks.IndexOf(aBookmarkName) >= 0 then // avoid duplicate bookmarks
    result := false
  else
  begin
    if aYPosition = 0 then
    begin
      CheckYPos;
      aYPosition := fCurrentYPos;
    end;
    fBookMarks.AddObject(aBookmarkName,
      TGdiPageReference.Create(PageCount, 0, aYPosition, 0, 0));
    {$ifndef USEPDFPRINTER}
    fCanvas.MoveTo(0, aYPosition);
    GdiCommentBookmark(fCanvas.Handle, StringToUtf8(aBookmarkName));
    {$endif USEPDFPRINTER}
    result := true;
  end;
end;

procedure TGdiPages.GotoPosition(aPage, aYPos: integer);
begin
  Page := aPage;
  HorzScrollbar.Position := 0;
  VertScrollbar.Position := (aYPos * VertScrollbar.Range) div fPhysicalSizePx.y
end;

function TGdiPages.GotoBookmark(const aBookmarkName: string): boolean;
var
  i: integer;
begin
  i := fBookmarks.IndexOf(aBookmarkName);
  result := i >= 0;
  if result then
    with TGdiPageReference(fBookmarks.Objects[i]) do
      GotoPosition(Page, Rect.Top);
end;

procedure TGdiPages.AddOutline(const aTitle: string;
  aLevel, aYPosition, aPageNumber: integer);
begin
  if aPageNumber = 0 then
    aPageNumber := PageCount;
  if aYPosition = 0 then
  begin
    CheckYPos;
    aYPosition := fCurrentYPos;
  end;
  fOutline.AddObject(aTitle,
    TGdiPageReference.Create(aPageNumber, 0, aYPosition, 0, aLevel));
  {$ifndef USEPDFPRINTER}
  fCanvas.MoveTo(0, aYPosition);
  GdiCommentOutline(fCanvas.Handle, StringToUtf8(aTitle), aLevel);
  {$endif USEPDFPRINTER}
end;

procedure TGdiPages.AddLink(const aBookmarkName: string; aRect: TRect;
  aPageNumber: integer; aNoBorder: boolean);
begin
  if aPageNumber = 0 then
    aPageNumber := PageCount;
  aRect := MmToPrinter(aRect);
  with aRect do
    fLinks.AddObject(aBookmarkName, TGdiPageReference.Create(
      aPageNumber, left, top, right, bottom));
  {$ifndef USEPDFPRINTER}
  GdiCommentLink(fCanvas.Handle, StringToUtf8(aBookmarkName), aRect, aNoBorder);
  {$endif USEPDFPRINTER} 
end;


{ TGdiPageReference }

constructor TGdiPageReference.Create(
  PageNumber, Left, Top, Right, Bottom: integer);
begin
  inherited Create;
  Page := PageNumber;
  Rect.Left := Left;
  Rect.Top := Top;
  Rect.Right := Right;
  Rect.Bottom := Bottom;
end;

procedure TGdiPageReference.ToPreview(Pages: TGdiPages);
var
  W, H: integer;
begin // do it for all pages (zoom is not reset between page shift)
  if Page <> 0 then
    with Pages.fPreviewSurface do
    begin
      W := Width - GRAY_MARGIN * 2;
      H := Height - GRAY_MARGIN * 2;
      Preview.Left := GRAY_MARGIN + (Rect.Left * W) div Pages.fPhysicalSizePx.x;
      Preview.Right := GRAY_MARGIN + (Rect.Right * W) div Pages.fPhysicalSizePx.x;
      Preview.Top := GRAY_MARGIN + (Rect.Top * H) div Pages.fPhysicalSizePx.y;
      Preview.Bottom := GRAY_MARGIN + (Rect.Bottom * H) div Pages.fPhysicalSizePx.y;
    end;
end;


{ THeaderFooter }

constructor THeaderFooter.Create(Report: TGdiPages; doubleline: boolean;
  const aText: SynUnicode; IsText: boolean);
begin
  Text := aText;
  state := Report.SavedState;
  if not IsText then
    if doubleline then
      state.Flags := state.Flags or FORMAT_DOUBLELINE
    else
      state.Flags := state.Flags or FORMAT_SINGLELINE;
end;


{ ****************** TRenderPages Prototype - unfinished }

{$ifdef RENDERPAGES}

{ TRenderPages }

procedure TRenderPages.Clear;
begin
  inherited;
  fRdrCol.Clear;
  fFontCache.Clear;
end;

constructor TRenderPages.Create(AOwner: TComponent);
begin
  inherited;
  fRdr := TRenderBox.Create(self);
  fRdrCol := TObjectList.Create;
  fFontCache := TObjectList.Create;
end;

destructor TRenderPages.Destroy;
begin
  inherited;
  FreeAndNil(fRdrCol);
  FreeAndNil(fRdr);
  FreeAndNil(fFontCache);
end;

function TRenderPages.GetCurrentFontCacheIndex: integer;
var
  F: TFont;
begin
  for result := 0 to fFontCache.Count - 1 do
    with TFont(fFontCache.List[result]) do
      if (Color = Font.Color) and
         (Height = Font.Height) and
         (Style = Font.Style) and
         (Name = Font.Name) then
        exit;
  F := TFont.Create;
  F.Assign(Font);
  result := fFontCache.Add(F);
end;

function TRenderPages.GetCurrentFontCacheIndexAndSelect: integer;
var
  H: HDC;
begin
  result := GetCurrentFontCacheIndex;
  H := Canvas.Handle;
  with TFont(fFontCache[result]) do
  begin
    // same as TCanvas.CreateFont
    SelectObject(H, Handle);
    SetTextColor(H, ColorToRGB(Color));
    if length(fFontCacheSpace) < fFontCache.Count then
      SetLength(fFontCacheSpace, fFontCache.Count + 20);
    if fFontCacheSpace[result].cx = 0 then
      GetTextExtentPoint32W(H, ' ', 1, fFontCacheSpace[result]);
  end;
end;

function TRenderPages.GetSavedRender: TSavedStateRender;
begin
  with result do
  begin
    FirstLineIndent := ParagraphFirstLineIndent;
    Before := ParagraphBefore;
    After := ParagraphAfter;
    RightIndent := ParagraphRightIndent;
    LeftIndent := ParagraphLeftIndent;
  end;
end;

procedure TRenderPages.NewPageInternal;
begin
  { TODO : close any pending paragraph }
  inherited;
end;

procedure TRenderPages.RdrParagraph;
begin
  if ParagraphBefore <> 0 then
    CurrentYPos := CurrentYPos + ParagraphBefore;
  Rdr.Flush(fPageMarginsPx.left, fCurrentYPos, false, 0, false);
  if ParagraphAfter <> 0 then
    CurrentYPos := CurrentYPos + ParagraphAfter;
end;

procedure TRenderPages.RdrPard;
var
  State: TSavedState;
begin
  if self = nil then
    exit;
  fAlign := taLeft;
  SetSavedRender(fDefaultStateRender);
  State := SavedState;
  if State.Flags <> fDefaultState.Flags then
  begin
    State.Flags := fDefaultState.Flags;
    SavedState := State; // will set FORMAT_RIGHT/CENTER/JUSTIFIED
  end;
end;

procedure TRenderPages.RdrPardPlain;
begin
  if self = nil then
    exit;
  if fDefaultState.FontName = '' then
    RdrPlain
  else
    SavedState := fDefaultState;
  SetSavedRender(fDefaultStateRender);
end;

procedure TRenderPages.RdrPlain;
var
  State: TSavedState;
begin
  if self = nil then
    exit;
  if (fDefaultState.FontName = '') or
     (fDefaultState.Flags = 0) then
  begin
    Font.Size := 12;
    Font.Style := [];
    Font.Color := clBlack;
  end
  else
  begin
    State := fDefaultState;
    State.Flags :=
      // void FORMAT_RIGHT/CENTER/JUSTIFIED
      (State.Flags and not (FORMAT_RIGHT or FORMAT_CENTER or FORMAT_JUSTIFIED)) or
      // keep current FORMAT_RIGHT/CENTER/JUSTIFIED
      (TextFormatsToFlags and (FORMAT_RIGHT or FORMAT_CENTER or FORMAT_JUSTIFIED));
    SavedState := State;
  end;
end;

procedure TRenderPages.RdrSetCurrentStateAsDefault;
begin
  fDefaultState := SavedState;
  fDefaultStateRender := GetSavedRender;
end;

function TRenderPages.RdrTableBegin(const PercentWidth: array of integer): boolean;
var
  i, sum, w: integer;
  col: TRenderBox;
begin
  result := (self <> nil) and (fRdrCol.Count = 0);
  if not result then
    exit;
  sum := 0;
  for i := 0 to high(PercentWidth) do
    inc(sum, PercentWidth[i]);
  if sum <= 0 then
  begin
    result := false;
    exit;
  end;
  w := fPhysicalSizePx.x - fPageMarginsPx.Left - fPageMarginsPx.right;
  for i := 0 to high(PercentWidth) do
  begin
    col := TRenderBox.Create(self);
    col.Width := (w * 100) div sum;
    fRdrCol.Add(col);
  end;
end;

function TRenderPages.RdrTableColumn(aColumnIndex: integer): TRenderBox;
begin
  if (self = nil) or
     (cardinal(aColumnIndex) >= cardinal(fRdrCol.Count - 1)) then
    result := nil
  else
    result := TRenderBox(fRdrCol.List[aColumnIndex]);
end;

function TRenderPages.RdrTableEnd: boolean;
begin
  result := (self <> nil) and (fRdrCol.Count > 0);
  if not result then
    exit;
  fRdrCol.Clear;
end;

procedure TRenderPages.RestoreSavedLayout;
begin
  if self = nil then
    exit; // avoid GPF
  if fSavedCount >= length(fSavedRender) then
    Setlength(fSavedRender, fSavedCount + 20);
  fSavedRender[fSavedCount] := GetSavedRender;
  inherited;
end;

procedure TRenderPages.SaveLayout;
begin
  if (self = nil) or
     (fSavedCount <= 0) then
    exit;
  inherited;
  SetSavedRender(fSavedRender[fSavedCount]);
end;

procedure TRenderPages.SetSavedRender(const State: TSavedStateRender);
begin
  with State do
  begin
    ParagraphFirstLineIndent := FirstLineIndent;
    ParagraphBefore := Before;
    ParagraphAfter := After;
    ParagraphRightIndent := RightIndent;
    ParagraphLeftIndent := LeftIndent;
  end;
end;


{ TRenderBox }

procedure TRenderBox.AddText(const s: string);
var
  PW: PWideChar;
  PWLen: integer;
begin
  if (self = nil) or
     (Owner = nil) then
    exit; // avoid GPF
  // convert text to unicode and add to fText[] internal buffer
  Owner.InternalUnicodeString(StringToSynUnicode(s), PW, PWLen, nil);
  AddText(PW, PWLen);
end;

procedure TRenderBox.AddText(PW: PWideChar; PWLen: integer);
var
  PDBeg, PD: PWideChar;
  aFontIndex, aFontSpaceWidth: integer;
begin
  if (self = nil) or
     (Owner = nil) or
     (PWLen = 0) then
    exit; // avoid GPF
  if PWLen + fTextLen > length(fText) then
    SetLength(fText, length(fText) + PWLen + 1024);
  PD := @fText[fTextLen];
  inc(fTextLen, PWLen);
  // create associated word markers
  aFontIndex := Owner.GetCurrentFontCacheIndexAndSelect;
  aFontSpaceWidth := Owner.fFontCacheSpace[aFontIndex].cx;
  repeat
    PDBeg := PD;
    while true do
      case integer(PW^) of
        0, 32:
          break;
        1..31:
          if PD <> PDBeg then
            break
          else
            inc(PW);
      else
        begin
          PD^ := PW^;
          inc(PW);
          inc(PD);
        end;
      end;
    if fBoxCount >= Length(fBox) then
      SetLength(fBox, fBoxCount + 200);
    with fBox[fBoxCount] do
    begin
      TextOffset := PD - @fText[0];
      TextLength := PD - PDBeg;
      FontIndex := aFontIndex;
      FontSpaceWidth := aFontSpaceWidth;
      GetTextExtentPoint32W(Owner.Canvas.Handle, PDBeg, TextLength, Size);
      SpaceAfterCount := 0;
      while integer(PW^) in [1..32] do
      begin
        PD^ := ' ';
        inc(PD);
        inc(PW);
        inc(SpaceAfterCount);
      end;
      LinkNumber := fLinksBookMarkNameCurrent;
    end;
    inc(fBoxCount);
  until PW^ = #0;
end;

procedure TRenderBox.Clear;
begin
  if self = nil then
    exit;
  Finalize(fLinksBookMarkName);
  Finalize(fBox);
  fLayoutCount := 0;
  fBoxCount := 0;
  fTextLen := 0;
  fHeight := 0;
  fLinksBookMarkNameCurrent := 0;
end;

constructor TRenderBox.Create(Owner: TRenderPages);
begin
  fOwner := Owner;
  fBiDiMode := Owner.BiDiMode;
  fOwnerFont := Owner.Font;
end;

/// format the already inserted text into the TRenderPages owner
// - this TRenderBox text content will be cleared at the end of this method
// - you don't have to call it usualy: use Owner.RdrParagraph instead
// - by default, will render top aligned to the X=Left/Y=Top position
// - for vertical alignment, specify an height in ForcedHeightBottomCentered
// then will be centered if ForcedAtBottom=false, or bottom aligned if true
// - if CurrentPageOnly is true, will only flush the content which will fit on
// the current page - then the fLayout[] array will contain remaining boxes; otherwise,
// this will flush all content to multiple pages

procedure TRenderBox.Flush(Left, Top: integer; CurrentPageOnly: boolean;
  ForcedHeightBottomCentered: integer; ForcedAtBottom: boolean);
var
  H, Y, i, fitLayout: integer;
  WillBreak: boolean;
begin
  if (self = nil) or
     (Owner = nil) then
    exit; // avoid GPF
  H := GetHeight; // will populate fLayout[] from fBox[] if necessary
  { render on document Canvas }
  WillBreak := false;
  fitLayout := fLayoutCount - 1;
  for i := 0 to fitLayout do
    if fLayout[i].Top >= H then
    begin
      fitLayout := i - 1;
      WillBreak := true;
      break;
    end;
  { TODO : handle TGdiPageReference creation from fLayout[].LastBox.LinkNumber }
  // reset internal TRenderBox content
  Clear;
end;

function TRenderBox.GetHeight: integer;
begin
  if self = nil then
    result := 0
  else
  begin
    if fHeight = 0 then
      // need to recalculate the layout to refresh the resulting Height
      InternalRender;
    result := fHeight;
  end;
end;

procedure TRenderBox.InternalRender;
var
  ndx, ndxFirst: integer;
  X, Y, H, W, LineW: integer;
  txt: PWideChar;
  Box: PRenderBoxWord;
  LineLayout, LineNdx: integer;

  procedure AddLayout(DoLineFeed, LastLine: boolean);
  var
    nspace, Adjust, i, j, aLeft, n: integer;
    align: TTextAlign;
    TmpLayout: array of TRenderBoxLayout;
  begin
    if fLayoutCount >= length(fLayout) then
      SetLength(fLayout, fLayoutCount + 50);
    with fLayout[fLayoutCount] do
    begin
      Text := txt;
      with Box^ do
        txt := @fText[TextOffset + TextLength]; // txt^ points to ' ' after text
      Length := txt - Text;
      Left := X;
      Top := Y;
      Width := W;
      LineIndex := LineNdx;
      LastBox := Box;
      BreakExtra := 0;
      BreakCount := 0;
    end;
    if DoLineFeed then
    begin
      // we must handle the line feed layout
      align := Owner.TextAlign;
      Adjust := LineW - (X + W);
      if (Adjust <= 0) or
         // force left align if wider than expected (i.e. overpass right margin)
         (LastLine and
          (align = taJustified)) then
        // last line of justified paragraph is never justified
        align := taLeft;
      if BiDiMode = bdRightToLeft then
      begin
        case align of
          taLeft:
            align := taRight;
          taRight:
            align := taLeft;
        end;
        n := fLayoutCount - LineLayout + 1;
        if n > 1 then
        begin
          // multi layouts: change logical to visual order for RTL languages
          SetLength(TmpLayout, n);
          MoveFast(fLayout[LineLayout], TmpLayout[0], n * SizeOf(TmpLayout[0]));
          aLeft := fLayout[LineLayout].Left;
          for i := 0 to n - 1 do
          begin
            MoveFast(TmpLayout[i], fLayout[fLayoutCount - i], SizeOf(TmpLayout[0]));
            with fLayout[fLayoutCount - i] do
            begin
              Left := aLeft;
              inc(aLeft, Width + LastBox^.FontSpaceWidth * LastBox^.SpaceAfterCount);
            end;
          end;
        end;
      end;
      case align of
        taRight:
          for i := LineLayout to fLayoutCount do
            inc(fLayout[i].Left, Adjust);
        taCenter:
          begin
            Adjust := Adjust div 2;
            for i := LineLayout to fLayoutCount do
              inc(fLayout[i].Left, Adjust);
          end;
        taJustified:
          if Adjust > 0 then
          begin
            // compute SetTextJustification() values and update Left position
            aLeft := fLayout[LineLayout].Left;
            nspace := 0;
            for i := LineLayout to fLayoutCount do
              with fLayout[i] do
              begin
                for j := 0 to Length - 1 do
                  if Text[j] = ' ' then
                    inc(BreakCount);
                inc(nspace, BreakCount);
              end;
            if nspace > 0 then
              for i := LineLayout to fLayoutCount do
                with fLayout[i] do
                begin
                  Left := aLeft;
                  BreakExtra := (Adjust * BreakCount) div nspace;
                  dec(Width, LastBox^.FontSpaceWidth * BreakCount - BreakExtra);
                  inc(aLeft, Width);
                end;
          end;
      end;
      for i := LineLayout to fLayoutCount do
        fLayout[i].Height := H; // same height for all fLayout[] of this line
      with Owner do
        X := MmToPrinterPxX(ParagraphLeftIndent);
      inc(Y, H);
      H := 0; // force recalculate line height
      LineLayout := fLayoutCount;
      inc(LineNdx);
    end
    else
    begin
      // just append this "word" box to fLayout[fLayoutCount]
      with Box^ do
        // compute next position
        inc(X, W + FontSpaceWidth * SpaceAfterCount);
    end;
    inc(fLayoutCount);
    ndxFirst := ndx + 1;
    W := 0;
  end;

begin // compute TRenderBoxWord.X/Y and fHeight
  fHeight := 0;
  fLayoutCount := 0;
  SetLength(fLayout, fBoxCount shr 2);
  if fBoxCount = 0 then
    exit; // no text added
  with Owner do
  begin
    X := MmToPrinterPxX(ParagraphFirstLineIndent);
    LineW := self.fWidth - MmToPrinterPxX(ParagraphRightIndent);
  end;
  LineNdx := 0;
  Y := 0;
  H := 0;
  W := 0;
  LineLayout := 0;
  txt := @fText[0];
  ndxFirst := 0;
  for ndx := 0 to fBoxCount - 1 do
  begin
    Box := @fBox[ndx];
    if Box^.Size.cy > H then
      H := Box^.Size.cy;
    inc(W, Box^.Size.cx);
    if ndx = fBoxCount - 1 then
      // reached last box -> flush pending line content
      AddLayout(true, true)
    else
      with fBox[ndx + 1] do
        if X + W + Size.cx > LineW then
          // not enough space in current line -> flush and go to next line
          AddLayout(true, false)
        else if (FontIndex <> Box^.FontIndex) or
                (LinkNumber <> Box^.LinkNumber) then
          // text formatting or Link will change -> add a layout box
          AddLayout(false, false);
  end;
  fHeight := Y;
end;

procedure TRenderBox.LinkBegin(const aBookmarkName: string);
begin
  if (self = nil) or
     (Owner = nil) then
    exit; // avoid GPF
  LinkEnd; // no nested links
  fLinksBookMarkNameCurrent := Length(fLinksBookMarkName) + 1;
  SetLength(fLinksBookMarkName, fLinksBookMarkNameCurrent);
  fLinksBookMarkName[fLinksBookMarkNameCurrent - 1] := aBookmarkName;
end;

function TRenderBox.LinkEnd: boolean;
begin
  result := false;
  if (self = nil) or
     (Owner = nil) or
     (fLinksBookMarkNameCurrent = 0) then
    exit; // avoid GPF
  fLinksBookMarkNameCurrent := 0;
  result := true;
end;

procedure TRenderBox.NewLine;
begin
  if (self = nil) or
     (Owner = nil) then
    exit; // avoid GPF
end;

procedure TRenderBox.Pard;
begin
  if (self <> nil) and
     (Owner <> nil) then // avoid GPF
    Owner.RdrPard;
end;

procedure TRenderBox.PardPlain;
begin
  if (self <> nil) and
     (Owner <> nil) then // avoid GPF
    Owner.RdrPardPlain;
end;

procedure TRenderBox.Plain;
begin
  if (self <> nil) and
     (Owner <> nil) then // avoid GPF
    Owner.RdrPlain;
end;

{$endif RENDERPAGES}

{$endif OSPOSIX}

end.

