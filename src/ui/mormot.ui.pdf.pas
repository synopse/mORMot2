/// PDF file generation on Windows
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.pdf;

{
  *****************************************************************************

    High Performance PDF Engine for Windows
    - Shared types and functions
    - Internal classes mapping PDF objects
    - TPdfDocument TPdfPage main rendering classes
    - TPdfDocumentGdi for GDI/TCanvas rendering support

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

{$define USE_PDFSECURITY}
// if defined, the TPdfDocument*.Create() constructor will have an additional
// AEncryption: TPdfEncryption parameter able to create secured PDF files
// - this feature links mormot.crypt.core.pas unit for MD5 and RC4 algorithms
{$ifdef NO_USE_PDFSECURITY}
  // this special conditional can be set globaly for an application which doesn't
  //  need the security features, therefore dependency to mormot.crypt.core.pas
  {$undef USE_PDFSECURITY}
{$endif NO_USE_PDFSECURITY}

{$define USE_UNISCRIBE}
// if defined, the PDF engine will use the Windows Uniscribe API to
// render Ordering and Shaping of the text (useful for Hebrew, Arabic and
// some Asiatic languages)
// - this feature need the TPdfDocument.UseUniscribe property to be forced to true
// according to the language of the text you want to render
// - can be undefined to safe some KB if you're sure you won't need it

{$ifdef NO_USE_UNISCRIBE}
  // this special conditional can be set globaly for an application which does
  // not need the UniScribe features
  {$undef USE_UNISCRIBE}
{$endif USE_UNISCRIBE}

{$define USE_SYNGDIPLUS}
// if defined, the PDF engine will use SynGdiPlus to handle all
// JPG, TIF, PNG and GIF image types (prefered way, but need XP or later OS)
// - if you'd rather use the default jpeg unit (and add some more code to your
// executable), undefine this conditional
{$ifdef NO_USE_SYNGDIPLUS}
// this special conditional can be set globaly for an application which doesn't
// need the SynGdiPlus features (like TMetaFile drawing), and would rather
// use the default jpeg unit
  {$undef USE_SYNGDIPLUS}
{$endif USE_SYNGDIPLUS}

{$define USE_METAFILE}
// if defined, the PDF engine will support TMetaFile / TPdfDocumentGdi
{$ifdef NO_USE_METAFILE}
  // this special conditional can be set globaly for an application which
  // doesn't need the TMetaFile / TPdfDocumentGdi features
  {$undef USE_METAFILE}
{$endif USE_METAFILE}

{$define USE_GRAPHICS_UNIT} // VCL/LCL usage is mandatory by now at low level

uses
  {$ifdef OSWINDOWS}
  windows,
  winspool,
  {$ifdef USE_UNISCRIBE}
  mormot.lib.uniscribe,
  {$endif USE_UNISCRIBE}
  {$endif OSWINDOWS}
  {$ifdef USE_GRAPHICS_UNIT}
    {$ifdef FPC}
    lcltype,
    lclproc,
    lclintf,
    rtlconsts,
    {$ifdef USE_METAFILE}
    mormot.ui.core, // for TMetaFile definition
    {$endif USE_METAFILE}
    {$else}
    {$endif FPC}
    {$ifdef NEEDVCLPREFIX}
    vcl.graphics,
    {$else}
    graphics,
    {$endif NEEDVCLPREFIX}
  {$endif USE_GRAPHICS_UNIT}
  sysutils,
  types,
  classes,
  variants,
  math,
  {$ifdef USE_PDFSECURITY}
  mormot.crypt.core,
  {$endif USE_PDFSECURITY}
  {$ifdef USE_SYNGDIPLUS}
  mormot.ui.gdiplus,
  {$else}
  jpeg,
  {$endif USE_SYNGDIPLUS}
  mormot.core.base,
  mormot.core.os,
  mormot.lib.z,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.data;



{************ Shared types and functions }

type
  /// the PDF library uses internaly AnsiString text encoding
  // - the corresponding charset/codepage is the current system charset, or
  // the one supplied as a parameter to TPdfDocument.Create
  PdfString = RawByteString;

  /// a PDF date, encoded as 'D:20100414113241'
  TPdfDate = PdfString;

{$ifdef FPC}

{ some FPC/Delphi LCL/VCL compatibility definitions }

type
  // FPC wrappers are just pointless and confusing here
  TPoint = TPOINTL;
  PPoint = PPOINTL;
  TRect = packed record
    case integer of
      0: (Left, Top, Right, Bottom: integer);
      1: (TopLeft, BottomRight: TPoint);
      2: (Rect: Types.TRect);
  end;
  PRect = ^TRect;

function Rect(ALeft, ATop, ARight, ABottom: integer): TRect;
  {$ifdef HASINLINE} inline; {$endif}
function Point(X, Y: integer): TPoint;
  {$ifdef HASINLINE} inline; {$endif}

{$endif FPC}

type
  /// a PDF coordinates rectangle
  TPdfRect = record
    Left, Top, Right, Bottom: single;
  end;

  PPdfRect = ^TPdfRect;

  /// a PDF coordinates box
  TPdfBox = record
    Left, Top, Width, Height: single;
  end;

  PPdfBox = ^TPdfBox;

  /// the internal pdf file format
  TPdfFileFormat = (
    pdf13,
    pdf14,
    pdf15,
    pdf16,
    pdf17);

  /// the PDF/A level
  TPdfALevel = (
    pdfaNone,
    pdfa1A,
    pdfa1B,
    pdfa2A,
    pdfa2B,
    pdfa3A,
    pdfa3B);

  /// PDF exception, raised when an invalid value is given to a constructor
  EPdfInvalidValue = class(ESynException);

  /// PDF exception, raised when an invalid operation is triggered
  EPdfInvalidOperation = class(ESynException);

  /// Page mode determines how the document should appear when opened
  TPdfPageMode = (
    pmUseNone,
    pmUseOutlines,
    pmUseThumbs,
    pmFullScreen);

  /// Line cap style specifies the shape to be used at the ends of open
  // subpaths when they are stroked
  TLineCapStyle = (
    lcButt_End,
    lcRound_End,
    lcProjectingSquareEnd);

  /// The line join style specifies the shape to be used at the corners of paths
  // that are stroked
  TLineJoinStyle = (
    ljMiterJoin,
    ljRoundJoin,
    ljBevelJoin);

  /// PDF text paragraph alignment
  TPdfAlignment = (
    paLeftJustify,
    paRightJustify,
    paCenter);

  /// PDF gradient direction
  TGradientDirection = (
    gdHorizontal,
    gdVertical);

  /// allowed types for PDF objects (i.e. TPdfObject)
  TPdfObjectType = (
    otDirectObject,
    otIndirectObject,
    otVirtualObject);

  /// The text rendering mode determines whether text is stroked, filled, or used
  // as a clipping path
  TTextRenderingMode = (
    trFill,
    trStroke,
    trFillThenStroke,
    trInvisible,
    trFillClipping,
    trStrokeClipping,
    trFillStrokeClipping,
    trClipping);

  /// The annotation types determines the valid annotation subtype of TPdfDoc
  TPdfAnnotationSubType = (
    asTextNotes,
    asLink);

  /// The border style of an annotation
  TPdfAnnotationBorder = (
    abSolid,
    abDashed,
    abBeveled,
    abInset,
    abUnderline);

  /// Destination Type determines default user space coordinate system of
  // Explicit destinations
  TPdfDestinationType = (
    dtXYZ,
    dtFit,
    dtFitH,
    dtFitV,
    dtFitR,
    dtFitB,
    dtFitBH,
    dtFitBV);

  /// The page layout to be used when the document is opened
  TPdfPageLayout = (
    plSinglePage,
    plOneColumn,
    plTwoColumnLeft,
    plTwoColumnRight);

  /// Viewer preferences specifying how the reader User Interface must start
  // - vpEnforcePrintScaling will set the file version to be PDF 1.6
  TPdfViewerPreference = (
    vpHideToolbar,
    vpHideMenubar,
    vpHideWindowUI,
    vpFitWindow,
    vpCenterWindow,
    vpEnforcePrintScaling);

  /// set of Viewer preferences
  TPdfViewerPreferences = set of TPdfViewerPreference;

  /// available known paper size (psA4 is the default on TPdfDocument creation)
  TPdfPaperSize = (
    psA4,
    psA5,
    psA3,
    psA2,
    psA1,
    psA0,
    psLetter,
    psLegal,
    psUserDefined);

  /// define if streams must be compressed
  TPdfCompressionMethod = (
    cmNone,
    cmFlateDecode);

  /// the available PDF color range
  TPdfColor = -$7FFFFFFF - 1..$7FFFFFFF;

  /// the PDF color, as expressed in RGB terms
  // - maps COLORREF / TColorRef as used e.g. under windows
  TPdfColorRGB = cardinal;

  /// the recognized families of the Standard 14 Fonts
  TPdfFontStandard = (
    pfsTimes,
    pfsHelvetica,
    pfsCourier);

  /// numerical ID for every XObject
  TXObjectID = integer;

  /// is used to define how TMetaFile text positioning is rendered
  // - tpSetTextJustification will handle efficiently the fact that TMetaFileCanvas
  // used SetTextJustification() API calls to justify text: it will converted
  // to SetWordSpace() pdf rendering
  // - tpExactTextCharacterPositining will use the individual glyph positioning
  // information as specified within the TMetaFile content: resulting pdf size
  // will be bigger, but font kerning will be rendered as expected
  // - tpKerningFromAveragePosition will use global font kerning via
  // SetHorizontalScaling() pdf rendering
  TPdfCanvasRenderMetaFileTextPositioning = (
    tpKerningFromAveragePosition,
    tpSetTextJustification,
    tpExactTextCharacterPositining);

  /// is used to define how TMetaFile text is clipped
  // - by default, text will be clipped with the specified TEMRText.ptlReference
  // - you could set tcClipExplicit to clip following the specified rclBounds
  // - or tcAlwaysClip to use the current clipping region (if any)
  // - finally, tcNeverClip would disable whole text clipping process, which
  // has been reported to be preferred e.g. on Wine
  TPdfCanvasRenderMetaFileTextClipping = (
    tcClipReference,
    tcClipExplicit,
    tcAlwaysClip,
    tcNeverClip);

  /// is used to define the TMetaFile kind of arc to be drawn
  TPdfCanvasArcType = (
    acArc,
    acArcTo,
    acArcAngle,
    acPie,
    acChoord);

  /// potential font styles
  TPdfFontStyle = (
    pfsBold,
    pfsItalic,
    pfsUnderline,
    pfsStrikeOut);

  /// set of font styles
  TPdfFontStyles = set of TPdfFontStyle;

  /// defines the data stored inside a EMR_GDICOMMENT message
  // - pgcOutline can be used to add an outline at the current position (i.e.
  // the last Y parameter of a Move): the text is the associated title, UTF-8 encoded
  // and the outline tree is created from the number of leading spaces in the title
  // - pgcBookmark will create a destination at the current position (i.e.
  // the last Y parameter of a Move), with some text supplied as bookmark name
  // - pgcLink/pgcLinkNoBorder will create a asLink annotation, expecting the data
  // to be filled with TRect inclusive-inclusive bounding rectangle coordinates,
  // followed by the corresponding bookmark name
  // - pgcJpegDirect will include a JPEG image directly from its file content
  // - pgcBeginMarkContent/pgcEndMarkContent will map
  // BeginMarkedContent/EndMarkedContent sections
  // - use the GdiComment*() functions to append the corresponding
  // EMR_GDICOMMENT message to a metafile content
  TPdfGdiComment = (
    pgcOutline,
    pgcBookmark,
    pgcLink,
    pgcLinkNoBorder,
    pgcJpegDirect,
    pgcBeginMarkContent,
    pgcEndMarkContent);

{$ifdef USE_PDFSECURITY}

  /// the available encryption levels
  // - in current version only RC4 40-bit and RC4 128-bit are available, which
  // correspond respectively to PDF 1.3 and PDF 1.4 formats
  // - for RC4 40-bit and RC4 128-bit, associated password are restricted to a
  // maximum length of 32 characters and could contain only characters from the
  // Latin-1 encoding (i.e. no accent)
  TPdfEncryptionLevel = (
    elNone,
    elRC4_40,
    elRC4_128);

  /// PDF can encode various restrictions on document operations which can be
  // granted or denied individually (some settings depend on others, though):
  // - Printing: If printing is not allowed, the print button in Acrobat will be
  // disabled. Acrobat supports a distinction between high-resolution and
  // low-resolution printing. Low-resolution printing generates a bitmapped
  // image of the page which is suitable only for personal use, but prevents
  // high-quality reproduction and re-distilling. Note that bitmap printing
  // not only results in low output quality, but will also considerably slow
  // down the printing process.
  // - General Editing: If this is disabled, any document modification is
  // prohibited. Content extraction and printing are allowed.
  // - Content Copying and Extraction: If this is disabled, selecting document
  // contents and copying it to the clipboard for repurposing the contents is
  // prohibited. The accessibility interface also is disabled. If you need to
  // search such documents with Acrobat you must select the Certified Plugins
  // Only preference in Acrobat.
  // - Authoring Comments and Form Fields: If this is disabled, adding,
  // modifying, or deleting comments and form fields is prohibited. Form field
  // filling is allowed.
  // - Form Field Fill-in or Signing: If this is enabled, users can sign and
  // fill in forms, but not create form fields.
  // - Document Assembly: If this is disabled, inserting, deleting or rotating
  // pages, or creating bookmarks and thumbnails is prohibited.
  TPdfEncryptionPermission = (
    epPrinting,
    epGeneralEditing,
    epContentCopy,
    epAuthoringComment,
    epFillingForms,
    epContentExtraction,
    epDocumentAssembly,
    epPrintingHighResolution);

  /// set of restrictions on PDF document operations
  // - to be used as parameter for TPdfEncryption.New() class method
  // - see PDF_PERMISSION_ALL, PDF_PERMISSION_NOMODIF, PDF_PERSMISSION_NOPRINT,
  // PDF_PERMISSION_NOCOPY and PDF_PERMISSION_NOCOPYNORPRINT constants 
  TPdfEncryptionPermissions = set of TPdfEncryptionPermission;

const
  /// allow all actions for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_ALL: TPdfEncryptionPermissions =
    [Low(TPdfEncryptionPermission)..high(TPdfEncryptionPermission)];

  /// disable modification and annotation of a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_NOMODIF: TPdfEncryptionPermissions = [
    epPrinting,
    epContentCopy,
    epPrintingHighResolution,
    epFillingForms,
    epContentExtraction,
    epDocumentAssembly];

  /// disable printing for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERSMISSION_NOPRINT: TPdfEncryptionPermissions = [
    epGeneralEditing,
    epContentCopy,
    epAuthoringComment,
    epContentExtraction,
    epDocumentAssembly];

  /// disable content extraction or copy for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_NOCOPY: TPdfEncryptionPermissions = [
    epPrinting,
    epAuthoringComment,
    epPrintingHighResolution,
    epFillingForms];

  /// disable printing and content extraction or copy for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_NOCOPYNORPRINT: TPdfEncryptionPermissions = [];

{$endif USE_PDFSECURITY}

const
  /// used for an used xref entry
  PDF_IN_USE_ENTRY = 'n';
  /// used for an unused (free) xref entry, e.g. the root entry
  PDF_FREE_ENTRY = 'f';
  /// used e.g. for the root xref entry
  PDF_MAX_GENERATION_NUM = 65535;
  PDF_ENTRY_CLOSED = 0;
  PDF_ENTRY_OPENED = 1;

  /// the Carriage Return and Line Feed values used in the PDF file generation
  // - expect #13 and #10 under Windows, but #10 (e.g. only Line Feed) is enough
  // for the PDF standard, and will create somewhat smaller PDF files
  CRLF = #10;

  /// the "pure/posix" Line Feed value
  LF = #10;

  PDF_MIN_HORIZONTALSCALING = 10;
  PDF_MAX_HORIZONTALSCALING = 300;

  PDF_MAX_WORDSPACE = 300;

  PDF_MIN_CHARSPACE = -30;
  PDF_MAX_CHARSPACE = 300;

  PDF_MAX_FONTSIZE  = 2000;

  PDF_MAX_ZOOMSIZE  = 10;

  PDF_MAX_LEADING   = 300;

  /// list of common fonts available by default since Windows 2000
  // - to not embedd these fonts in the PDF document, and save some KB,
  // just use the EmbeddedTtfIgnore property of TPdfDocument/TPdfDocumentGdi:
  // !   PdfDocument.EmbeddedTtfIgnore.Text := MSWINDOWS_DEFAULT_FONTS;
  // - note that this is useful only if the EmbeddedTtf property was set to true
  MSWINDOWS_DEFAULT_FONTS: RawUtf8 =
    'Arial'#13#10#13#10 +
    'Courier New'#13#10 +
    'Georgia'#13#10 +
    'Impact'#13#10 +
    'Lucida Console'#13#10 +
    'Roman'#13#10 +
    'Symbol'#13#10 +
    'Tahoma'#13#10 +
    'Times New Roman'#13#10 +
    'Trebuchet'#13#10 +
    'Verdana'#13#10 +
    'WingDings';


/// this function returns true if the supplied text contain any MBCS character
// - typical call must check first if MBCS is currently enabled
// ! if SysLocale.FarEast and _HasMultiByteString(pointer(Text)) then ...
function HasMultiByteString(Value: PAnsiChar): boolean;

/// convert an unsigned integer into a PdfString text
function UInt32ToPdfString(Value: cardinal): PdfString;
  {$ifdef HASINLINE} inline; {$endif}

/// convert a date, into PDF string format, i.e. as 'D:20100414113241Z'
function DateTimeToPdfDate(ADate: TDateTime): TPdfDate;

/// decode PDF date, encoded as 'D:20100414113241'
function PdfDateToDateTime(const AText: TPdfDate; out AValue: TDateTime): boolean;

/// wrapper to create a temporary PDF coordinates rectangle
function PdfRect(Left, Top, Right, Bottom: single): TPdfRect; overload;
  {$ifdef HASINLINE} inline;{$endif}

/// wrapper to create a temporary PDF coordinates rectangle
function PdfRect(const Box: TPdfBox): TPdfRect; overload;
  {$ifdef HASINLINE} inline;{$endif}

/// wrapper to create a temporary PDF box
function PdfBox(Left, Top, Width, Height: single): TPdfBox;
  {$ifdef HASINLINE} inline;{$endif}

/// reverse char orders for every hebrew and arabic words
// - just reverse all the UTF-16 codepoints in the supplied buffer
procedure L2R(W: PWideChar; L: PtrInt);

/// convert some millimeters dimension to internal PDF twips value
function PdfCoord(MM: single): integer;
  {$ifdef HASINLINE} inline;{$endif}

/// retrieve the paper size used by the current selected printer
function CurrentPrinterPaperSize: TPdfPaperSize;

/// retrieve the current printer resolution
function CurrentPrinterRes: TPoint;


{************ Internal classes mapping PDF objects }

type
  TPdfObject = class;
  TPdfCanvas = class;
  TPdfFont = class;
  TPdfFontTrueType = class;
  TPdfDocument = class;


{$ifdef USE_PDFSECURITY}

  /// abstract class to handle PDF security
  TPdfEncryption = class
  protected
    fLevel: TPdfEncryptionLevel;
    fFlags: integer;
    fInternalKey: TByteDynArray;
    fPermissions: TPdfEncryptionPermissions;
    fUserPassword: string;
    fOwnerPassword: string;
    fDoc: TPdfDocument;
    procedure EncodeBuffer(const BufIn; var BufOut; Count: cardinal); virtual; abstract;
  public
    /// initialize the internal structures with the proper classes
    // - do not call this method directly, but class function TPdfEncryption.New()
    constructor Create(aLevel: TPdfEncryptionLevel;
      aPermissions: TPdfEncryptionPermissions;
      const aUserPassword, aOwnerPassword: string); virtual;
    /// prepare a specific document to be encrypted
    // - internally used by TPdfDocument.NewDoc method
    procedure AttachDocument(aDoc: TPdfDocument); virtual;
    /// will create the expected TPdfEncryption instance, depending on aLevel
    // - to be called as parameter of TPdfDocument/TPdfDocumentGdi.Create()
    // - currently, only elRC4_40 and elRC4_128 levels are implemented
    // - both passwords are expected to be ASCII-7 characters only
    // - aUserPassword will be asked at file opening: to be set to '' for not
    // blocking display, but optional permission
    // - aOwnerPassword shall not be '', and will be used internally to cypher
    // the pdf file content
    // - aPermissions can be either one of the PDF_PERMISSION_ALL /
    // PDF_PERMISSION_NOMODIF / PDF_PERSMISSION_NOPRINT / PDF_PERMISSION_NOCOPY /
    // PDF_PERMISSION_NOCOPYNORPRINT set of options
    // - typical use may be:
    // ! Doc := TPdfDocument.Create(false,0,false,
    // !   TPdfEncryption.New(elRC4_40,'','toto',PDF_PERMISSION_NOMODIF));
    // ! Doc := TPdfDocument.Create(false,0,false,
    // !   TPdfEncryption.New(elRC4_128,'','toto',PDF_PERMISSION_NOCOPYNORPRINT));
    class function New(aLevel: TPdfEncryptionLevel;
      const aUserPassword, aOwnerPassword: string;
      aPermissions: TPdfEncryptionPermissions): TPdfEncryption;
  end;

  /// internal 32 bytes buffer, used during encryption process
  TPdfBuffer32 = array[0..31] of byte;

  /// handle PDF security with RC4+MD5 scheme in 40-bit and 128-bit
  // - allowed aLevel parameters for Create() are only elRC4_40 and elRC4_128
  TPdfEncryptionRC4MD5 = class(TPdfEncryption)
  protected
    fLastObjectNumber: integer;
    fLastGenerationNumber: integer;
    fUserPass, fOwnerPass: TPdfBuffer32;
    fLastRC4Key: TRC4;
    procedure EncodeBuffer(const BufIn; var BufOut; Count: cardinal); override;
  public
    /// prepare a specific document to be encrypted
    // - will compute the internal keys
    procedure AttachDocument(aDoc: TPdfDocument); override;
  end;
{$endif USE_PDFSECURITY}

  /// buffered writer class, specialized for PDF encoding
  TPdfWrite = class
  protected
    B, BEnd, BEnd4: PAnsiChar;
    fDestStream: TStream;
    fDestStreamPosition: integer;
    fAddGlyphFont: (fNone, fMain, fFallBack);
    fDoc: TPdfDocument;
    fTmp: array[0..511] of AnsiChar;
    /// internal Ansi->Unicode conversion, using the CodePage used in Create()
    // - returned Dest.len is in WideChar count, not in bytes
    // - caller must release the returned memory via Dest.Done
    procedure ToWideChar(const Ansi: PdfString; out Dest: TSynTempBuffer);
    {$ifdef USE_UNISCRIBE}
    /// internal method using the Windows Uniscribe API
    // - return false if PW was not appened to the PDF content, true if OK
    function AddUnicodeHexTextUniScribe(PW: PWideChar; PWLen: integer;
      WinAnsiTtf: TPdfFontTrueType; NextLine: boolean; Canvas: TPdfCanvas): boolean;
    {$endif USE_UNISCRIBE}
    /// internal method NOT using the Windows Uniscribe API
    procedure AddUnicodeHexTextNoUniScribe(PW: PWideChar; Ttf: TPdfFontTrueType;
      NextLine: boolean; Canvas: TPdfCanvas);
    /// internal methods handling font fall-back
    procedure AddGlyphFromChar(Char: WideChar; Canvas: TPdfCanvas;
      Ttf: TPdfFontTrueType; NextLine: PBoolean);
    procedure AddGlyphFlush(Canvas: TPdfCanvas; Ttf: TPdfFontTrueType;
      NextLine: PBoolean);
  public
    /// create the buffered writer, for a specified destination stream
    constructor Create(Destination: TPdfDocument; DestStream: TStream);
    /// add a character to the buffer
    function Add(c: AnsiChar): TPdfWrite; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// add an integer numerical value to the buffer
    function Add(Value: integer): TPdfWrite; overload;
    /// add an integer numerical value to the buffer
    // - and append a trailing space
    function AddWithSpace(Value: integer): TPdfWrite; overload;
    /// add an integer numerical value to the buffer
    // - with a specified fixed number of digits (left filled by '0')
    function Add(Value, DigitCount: integer): TPdfWrite; overload;
    /// add a floating point numerical value to the buffer
    // - up to 2 decimals are written
    function Add(Value: double): TPdfWrite; overload;
    /// add a floating point numerical value to the buffer
    // - up to 2 decimals are written, together with a trailing space
    function AddWithSpace(Value: double): TPdfWrite; overload;
    /// add a floating point numerical value to the buffer
    // - this version handles a variable number of decimals, together with
    // a trailing space - this is used by ConcatToCTM e.g. or enhanced precision
    function AddWithSpace(Value: double; Decimals: cardinal): TPdfWrite; overload;
    /// direct raw write of some data
    // - no conversion is made
    function Add(Text: PAnsiChar; Len: PtrInt): TPdfWrite; overload;
    /// direct raw write of some data
    // - no conversion is made
    function Add(const Text: RawByteString): TPdfWrite; overload;
    /// direct raw write of some data
    // - conversion is forced to UTF-8 output from Text string encoding
    function AddS(const Text: string): TPdfWrite; overload;
    /// hexadecimal write of some row data
    // - row data is written as hexadecimal byte values, one by one
    function AddHex(const Bin: PdfString): TPdfWrite;
    /// add a word value, as Big-Endian 4 hexadecimal characters
    function AddHex4(aWordValue: cardinal): TPdfWrite;
    /// convert some text into unicode characters, then write it as as Big-Endian
    // 4 hexadecimal characters
    // - Ansi to Unicode conversion uses the CodePage set by Create() constructor
    function AddToUnicodeHex(const Text: PdfString): TPdfWrite;
    /// write some unicode text as as Big-Endian 4 hexadecimal characters
    function AddUnicodeHex(PW: PWideChar; WideCharCount: integer): TPdfWrite;
    /// convert some text into unicode characters, then write it as PDF Text
    // - Ansi to Unicode conversion uses the CodePage set by Create() constructor
    // - use (...) for all WinAnsi characters, or <..hexa..> for Unicode characters
    // - if NextLine is true, the first written PDF Text command is not Tj but '
    // - during the text process, corresponding TPdfTrueTypeFont properties are
    // updated (Unicode version created if necessary, indicate used glyphs for
    // further Font properties writing to the PDF file content...)
    // - if the current font is not true Type, all Unicode characters are
    // drawn as '?'
    function AddToUnicodeHexText(const Text: PdfString; NextLine: boolean;
      Canvas: TPdfCanvas): TPdfWrite;
    /// write some Unicode text, as PDF text
    // - incoming unicode text must end with a #0
    // - use (...) for all WinAnsi characters, or <..hexa..> for Unicode characters
    // - if NextLine is true, the first written PDF Text command is not Tj but '
    // - during the text process, corresponding TPdfTrueTypeFont properties are
    // updated (Unicode version created if necessary, indicate used glyphs for
    // further Font properties writing to the PDF file content...)
    // - if the current font is not true Type, all Unicode characters are
    // drawn as '?'
    function AddUnicodeHexText(PW: PWideChar; PWLen: integer; NextLine: boolean;
      Canvas: TPdfCanvas): TPdfWrite;
    /// write some Unicode text, encoded as Glyphs indexes, corresponding
    // to the current font
    function AddGlyphs(Glyphs: PWord; GlyphsCount: integer;
      Canvas: TPdfCanvas; AVisAttrsPtr: pointer = nil): TPdfWrite;
    /// add some WinAnsi text as PDF text
    // - used by TPdfText object
    // - will optionally encrypt the content
    function AddEscapeContent(const Text: RawByteString): TPdfWrite;
    /// add some WinAnsi text as PDF text
    // - used by TPdfText object
    function AddEscape(Text: PAnsiChar; TextLen: integer): TPdfWrite;
    /// add some WinAnsi text as PDF text
    // - used by TPdfCanvas.ShowText method for WinAnsi text
    function AddEscapeText(Text: PAnsiChar; Font: TPdfFont): TPdfWrite;
    /// add some PDF /property value
    function AddEscapeName(Text: PAnsiChar): TPdfWrite;
    /// add a PDF color, from its TPdfColorRGB RGB value
    function AddColorStr(Color: TPdfColorRGB): TPdfWrite;
    /// add a TBitmap.Scanline[] content into the stream
    procedure AddRGB(P: PAnsiChar; PInc, Count: integer);
    /// add an ISO 8601 encoded date time (e.g. '2010-06-16T15:06:59-07:00')
    function AddIso8601(DateTime: TDateTime): TPdfWrite;
    /// add an integer value as binary, specifying a storage size in bytes
    function AddIntegerBin(value: integer; bytesize: cardinal): TPdfWrite;
  public
    /// flush the internal buffer to the destination stream
    procedure Save;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the current position
    // - add the current internal buffer stream position to the destination
    // stream position
    function Position: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the data written to the Writer as a PdfString
    // - this method could not use Save to flush the data, if all input was
    // inside the internal buffer (save some CPU and memory): so don't intend
    // the destination stream to be flushed after having called this method
    function ToPdfString: PdfString;
  end;

  /// object manager is a virtual class to manage instance of indirect PDF objects
  TPdfObjectMgr = class
  public
    procedure AddObject(AObject: TPdfObject); virtual; abstract;
    function GetObject(ObjectID: integer): TPdfObject; virtual; abstract;
  end;

  /// master class for most PDF objects declaration
  TPdfObject = class
  protected
    fObjectType: TPdfObjectType;
    fObjectNumber: integer;
    fGenerationNumber: integer;
    fSaveAtTheEnd: boolean;
    procedure InternalWriteTo(W: TPdfWrite); virtual;
    procedure SetObjectNumber(Value: integer);
    function SpaceNotNeeded: boolean; virtual;
  public
    /// create the PDF object instance
    constructor Create; virtual;
    /// Write object to specified stream
    // - If object is indirect object then write references to stream
    procedure WriteTo(var W: TPdfWrite);
    /// write indirect object to specified stream
    // - this method called by parent object
    procedure WriteValueTo(var W: TPdfWrite);
    /// low-level force the object to be saved now
    // - you should not use this low-level method, unless you want to force
    // the fSaveAtTheEnd internal flag to be set to force, so that
    // TPdfDocument.SaveToStreamDirectPageFlush would flush the object content
    procedure ForceSaveNow;
    /// the associated PDF Object Number
    // - If you set an object number higher than zero, the object is considered
    // as indirect. Otherwise, the object is considered as direct object.
    property ObjectNumber: integer
      read fObjectNumber write SetObjectNumber;
    /// the associated PDF Generation Number
    property GenerationNumber: integer
      read fGenerationNumber;
    /// the corresponding type of this PDF object
    property ObjectType: TPdfObjectType
      read fObjectType;
  end;

  /// a virtual PDF object, with an associated PDF Object Number
  TPdfVirtualObject = class(TPdfObject)
  public
    constructor Create(AObjectId: integer); reintroduce;
  end;

  /// a PDF object, storing a boolean value
  TPdfBoolean = class(TPdfObject)
  protected
    fValue: boolean;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(AValue: boolean); reintroduce;
    property Value: boolean
      read fValue write fValue;
  end;

  /// a PDF object, storing a NULL value
  TPdfNull = class(TPdfObject)
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  end;

  /// a PDF object, storing a numerical (integer) value
  TPdfNumber = class(TPdfObject)
  protected
    fValue: integer;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(AValue: integer); reintroduce;
    property Value: integer
      read fValue write fValue;
  end;

  /// a PDF object, storing a numerical (floating point) value
  TPdfReal = class(TPdfObject)
  protected
    fValue: double;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(AValue: double); reintroduce;
    property Value: double
      read fValue write fValue;
  end;

  /// a PDF object, storing a textual value
  // - the value is specified as a PdfString
  // - this object is stored as '(escapedValue)'
  // - in case of MBCS, conversion is made into UTF-16 before writing, and
  // stored as '<FEFFHexUnicodeEncodedValue>' with an initial BOM_UTF16LE
  TPdfText = class(TPdfObject)
  protected
    fValue: RawByteString;
    procedure InternalWriteTo(W: TPdfWrite); override;
    function SpaceNotNeeded: boolean; override;
  public
    constructor Create(const AValue: RawByteString); reintroduce;
    property Value: RawByteString
      read fValue write fValue;
  end;

  /// a PDF object, storing a textual value
  // - the value is specified as an UTF-8 encoded string
  // - this object is stored as '(escapedValue)'
  // - in case characters with ANSI code higher than 8 Bits, conversion is made
  // into UTF-16 before writing, and '<FEFFHexUnicodeEncodedValue>'  with an
  // initial BOM_UTF16LE
  TPdfTextUtf8 = class(TPdfObject)
  protected
    fValue: RawUtf8;
    procedure InternalWriteTo(W: TPdfWrite); override;
    function SpaceNotNeeded: boolean; override;
  public
    constructor Create(const AValue: RawUtf8); reintroduce;
    property Value: RawUtf8
      read fValue write fValue;
  end;

  /// a PDF object, storing a textual value
  // - the value is specified as a RTL string
  // - this object is stored as '(escapedValue)'
  // - in case characters with ANSI code higher than 8 Bits, conversion is made
  // into Unicode before writing, and '<FEFFHexUnicodeEncodedValue>'
  TPdfTextString = class(TPdfTextUtf8)
  protected
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(const AValue: string); reintroduce;
    property Value: string
      read GetValue write SetValue;
  end;

  /// a PDF object, storing a raw PDF content
  // - this object is stored into the PDF stream as the defined Value
  TPdfRawText = class(TPdfText)
  protected
    function SpaceNotNeeded: boolean; override;
    procedure InternalWriteTo(W: TPdfWrite); override;
  end;

  /// a PDF object, storing a textual value with no encryption
  // - the value is specified as a memory buffer
  // - this object is stored as '(escapedValue)'
  TPdfClearText = class(TPdfText)
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(Buffer: pointer; Len: integer); reintroduce;
  end;

  /// a PDF object, storing a PDF name
  // - this object is stored as '/Value'
  TPdfName = class(TPdfText)
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// append the 'SUBSET+' prefix to the Value
    // - used e.g. to notify that a font is included as a subset
    function AppendPrefix: RawUtf8;
  end;

  /// used to store an array of PDF objects
  TPdfArray = class(TPdfObject)
  protected
    fArray: TSynList;
    fObjectMgr: TPdfObjectMgr;
    function GetItems(Index: integer): TPdfObject;
      {$ifdef HASINLINE}inline;{$endif}
    function GetItemCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalWriteTo(W: TPdfWrite); override;
    function SpaceNotNeeded: boolean; override;
  public
    /// create an array of PDF objects
    constructor Create(AObjectMgr: TPdfObjectMgr); reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfNumber values
    constructor Create(AObjectMgr: TPdfObjectMgr; const AArray: array of integer);
      reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfNumber values
    constructor Create(AObjectMgr: TPdfObjectMgr; AArray: PWordArray;
      AArrayCount: integer); reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfName values
    constructor CreateNames(AObjectMgr: TPdfObjectMgr;
      const AArray: array of PdfString); reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfReal values
    constructor CreateReals(AObjectMgr: TPdfObjectMgr;
      const AArray: array of double); reintroduce; overload;
    /// release the instance memory, and all embedded objects instances
    destructor Destroy; override;
    /// Add a PDF object to the array
    // - if AItem already exists, do nothing
    function AddItem(AItem: TPdfObject): integer;
    /// insert a PDF object to the array
    // - if AItem already exists, do nothing
    procedure InsertItem(Index: integer; AItem: TPdfObject);
    /// retrieve a TPdfName object stored in the array
    function FindName(const AName: PdfString): TPdfName;
    /// remove a specified TPdfName object stored in the array
    function RemoveName(const AName: PdfString): boolean;
    /// retrieve an object instance, stored in the array
    property Items[Index: integer]: TPdfObject
      read GetItems; default;
    /// retrieve the array size
    property ItemCount: integer
      read GetItemCount;
    /// the associated PDF Object Manager
    property ObjectMgr: TPdfObjectMgr
      read fObjectMgr;
    /// direct access to the internal TSynList instance
    // - not to be used normally
    property List: TSynList
      read fArray;
  end;

  /// PDF dictionary element definition
  TPdfDictionaryElement = class
  protected
    fKey: TPdfName;
    fValue: TPdfObject;
    fIsInternal: boolean;
    function GetKey: PdfString;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the corresponding Key / Value pair
    constructor Create(const AKey: PdfString; AValue: TPdfObject;
      AInternal: boolean = false);
    /// release the element instance, and both associated Key and Value
    destructor Destroy; override;
    /// the associated Key Name
    property Key: PdfString
      read GetKey;
    /// the associated Value stored in this element
    property Value: TPdfObject
      read fValue;
    /// if this element was created as internal, i.e. not to be saved to the PDF content
    property IsInternal: boolean
      read fIsInternal;
  end;

  /// a PDF Dictionary is used to manage Key / Value pairs
  TPdfDictionary = class(TPdfObject)
  protected
    fArray: TSynList;
    fObjectMgr: TPdfObjectMgr;
    function GetItems(Index: integer): TPdfDictionaryElement;
      {$ifdef HASINLINE}inline;{$endif}
    function GetItemCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    function GetTypeOf: PdfString;
    function SpaceNotNeeded: boolean; override;
    procedure DirectWriteto(W: TPdfWrite; Secondary: TPdfDictionary);
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the PDF dictionary
    constructor Create(AObjectMgr: TPdfObjectMgr); reintroduce;
    /// release the dictionay instance, and all associated elements
    destructor Destroy; override;
    /// fast find a value by its name
    function ValueByName(const AKey: PdfString): TPdfObject;
    /// fast find a boolean value by its name
    function PdfBooleanByName(const AKey: PdfString): TPdfBoolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a numerical (integer) value by its name
    function PdfNumberByName(const AKey: PdfString): TPdfNumber;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    function PdfTextByName(const AKey: PdfString): TPdfText;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    // - return '' if not found, the TPdfText.Value otherwise
    function PdfTextValueByName(const AKey: PdfString): PdfString;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    // - return '' if not found, the TPdfTextUtf8.Value otherwise
    function PdfTextUtf8ValueByName(const AKey: PdfString): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    // - return '' if not found, the TPdfTextString.Value otherwise
    function PdfTextStringValueByName(const AKey: PdfString): string;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a numerical (floating-point) value by its name
    function PdfRealByName(const AKey: PdfString): TPdfReal;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a name value by its name
    function PdfNameByName(const AKey: PdfString): TPdfName;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find a dictionary value by its name
    function PdfDictionaryByName(const AKey: PdfString): TPdfDictionary;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast find an array value by its name
    function PdfArrayByName(const AKey: PdfString): TPdfArray;
      {$ifdef HASINLINE}inline;{$endif}
    /// add a specified Key / Value pair to the dictionary
    // - create PdfDictionaryElement with given key and value, and add it to list
    // - if the element exists, replace value of element by given value
    // - internal items are local to the framework, and not to be saved to the PDF content
    procedure AddItem(const AKey: PdfString; AValue: TPdfObject;
      AInternal: boolean = false); overload;
    /// add a specified Key / Value pair (of type TPdfName) to the dictionary
    procedure AddItem(const AKey, AValue: PdfString); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfNumber) to the dictionary
    procedure AddItem(const AKey: PdfString; AValue: integer); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfText) to the dictionary
    procedure AddItemText(const AKey, AValue: PdfString); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfTextUtf8) to the dictionary
    // - the value can be any UTF-8 encoded text: it will be written as
    // Unicode hexadecimal to the PDF stream, if necessary
    procedure AddItemTextUtf8(const AKey: PdfString; const AValue: RawUtf8); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfTextUtf8) to the dictionary
    // - the value is a RTL string: it will be written as
    // Unicode hexadecimal to the PDF stream, if necessary
    procedure AddItemTextString(const AKey: PdfString; const AValue: string); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// remove the element specified by its Key from the dictionary
    // - if the element does not exist, do nothing
    procedure RemoveItem(const AKey: PdfString);
    /// retrieve any dictionary element
    property Items[Index: integer]: TPdfDictionaryElement
      read GetItems; default;
    /// retrieve the dictionary element count
    property ItemCount: integer
      read GetItemCount;
    /// retrieve the associated Object Manager
    property ObjectMgr: TPdfObjectMgr
      read fObjectMgr;
    /// retrieve the type of the pdfdictionary object, i.e. the 'Type' property name
    property TypeOf: PdfString
      read GetTypeOf;
    /// direct access to the internal TSynList instance
    // - not to be used normally
    property List: TSynList
      read fArray;
  end;

  /// a temporary memory stream, to be stored into the PDF content
  // - typicaly used for the page content
  // - can be compressed, if the FlateDecode filter is set
  TPdfStream = class(TPdfObject)
  protected
    fAttributes: TPdfDictionary;
    fSecondaryAttributes: TPdfDictionary;
    {$ifdef USE_PDFSECURITY}
    fDoNotEncrypt: boolean;
    {$endif USE_PDFSECURITY}
    fFilter: PdfString;
    fWriter: TPdfWrite;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the temporary memory stream
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor Create(ADoc: TPdfDocument; DontAddToFXref: boolean = false); reintroduce;
    /// release the memory stream
    destructor Destroy; override;
    /// retrieve the associated attributes, e.g. the stream Length
    property Attributes: TPdfDictionary
      read fAttributes;
    /// retrieve the associated buffered writer
    // - use this TPdfWrite instance to write some data into the stream
    property Writer: TPdfWrite
      read fWriter;
    /// retrieve the associated filter name
    property Filter: PdfString
      read fFilter write fFilter;
  end;

  /// used to handle object which are not defined in this library
  TPdfBinary = class(TPdfObject)
  protected
    fStream: TMemoryStream;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the instance, i.e. its associated stream
    constructor Create; override;
    /// release the instance
    destructor Destroy; override;
    /// the associated memory stream, used to store the corresponding data
    // - the content of this stream will be written to the resulting
    property Stream: TMemoryStream
      read fStream;
  end;

  TPdfXref = class;

  TPdfObjectStream = class;

  /// the Trailer of the PDF File
  TPdfTrailer = class
  protected
    fAttributes: TPdfDictionary;
    fXrefAddress: integer;
    fCrossReference: TPdfStream;
    fObjectStream: TPdfObjectStream;
    fXRef: TPdfXref;
    procedure WriteTo(var W: TPdfWrite);
  public
    constructor Create(AObjectMgr: TPdfObjectMgr);
    destructor Destroy; override;
    procedure ToCrossReference(Doc: TPdfDocument);
    property XrefAddress: integer
      read fXrefAddress write fXrefAddress;
    property Attributes: TPdfDictionary
      read fAttributes;
  end;

  /// store one entry in the XRef list of the PDF file
  TPdfXrefEntry = class
  protected
    fEntryType: PdfString;
    fByteOffset: integer;
    fGenerationNumber: integer;
    fObjectStreamIndex: integer;
    fValue: TPdfObject;
  public
    /// create the entry, with the specified value
    // - if the value is nil (e.g. root entry), the type is 'f' (PDF_FREE_ENTRY),
    // otherwise the entry type is 'n' (PDF_IN_USE_ENTRY)
    constructor Create(AValue: TPdfObject);
    /// release the memory, and the associated value, if any
    destructor Destroy; override;
    /// write the XRef list entry
    procedure SaveToPdfWrite(var W: TPdfWrite);
    /// return either 'f' (PDF_FREE_ENTRY), either 'n' (PDF_IN_USE_ENTRY)
    property EntryType: PdfString
      read fEntryType write fEntryType;
    /// the position (in bytes) in the PDF file content stream
    // - to be ignored if ObjectStreamIndex>=0
    property ByteOffset: integer
      read fByteOffset;
    /// the index of this object in the global compressed /ObjStm object stream
    // - equals -1 by default, i.e. if stored within the main file content stream
    property ObjectStreamIndex: integer
      read fObjectStreamIndex;
    /// the associated Generation Number
    // - mostly 0, or 65535 (PDF_MAX_GENERATION_NUM) for the root 'f' entry
    property GenerationNumber: integer
      read fGenerationNumber write fGenerationNumber;
    /// the associated PDF object
    property Value: TPdfObject
      read fValue;
  end;

  /// store the XRef list of the PDF file
  TPdfXref = class(TPdfObjectMgr)
  protected
    fXrefEntries: TSynList;
    function GetItem(ObjectID: integer): TPdfXrefEntry;
      {$ifdef HASINLINE}inline;{$endif}
    function GetItemCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    procedure WriteTo(var W: TPdfWrite);
  public
    /// initialize the XRef object list
    // - create first a void 'f' (PDF_FREE_ENTRY) as root
    constructor Create;
    /// release instance memory and all associated XRef objects
    destructor Destroy; override;
    /// register object to the xref table, and set corresponding object ID
    procedure AddObject(AObject: TPdfObject); override;
    /// retrieve an object from its object ID, nil if ObjectID is out of range
    function GetObject(ObjectID: integer): TPdfObject; override;
    /// retrieve a XRef object instance, from its object ID
    // - note that ObjectID is not checked, and should be within 0..ItemCount-1
    property Items[ObjectID: integer]: TPdfXrefEntry
      read GetItem; default;
    /// retrieve the XRef object count
    property ItemCount: integer
      read GetItemCount;
  end;


{************ TPdfDocument TPdfPage main rendering classes }

    /// any object stored to the PDF file
  // - these objects are the main unit of the PDF file content
  // - these objects are written in the PDF file, followed by a "xref" table
  TPdfXObject = class(TPdfStream);

  /// generic PDF Outlines entries, stored as a PDF dictionary
  TPdfOutlines = class(TPdfDictionary);

  /// generic PDF Optional Content entry
  TPdfOptionalContentGroup = class(TPdfDictionary);

  TPdfInfo = class;
  TPdfCatalog = class;
  TPdfDestination = class;
  TPdfOutlineEntry = class;
  TPdfOutlineRoot = class;
  TPdfPage = class;
  TPdfPageClass = class of TPdfPage;

  /// the main class of the PDF engine, processing the whole PDF document
  TPdfDocument = class
  protected
    fRoot: TPdfCatalog;
    fCurrentPages: TPdfDictionary;
    fOutputIntents: TPdfArray;
    fMetaData: TPdfStream;
    fCanvas: TPdfCanvas;
    fTrailer: TPdfTrailer;
    fXRef: TPdfXref;
    fInfo: TPdfInfo;
    fFontList: TSynList;
    fObjectList: TSynList;
    fOutlineRoot: TPdfOutlineRoot;
    fStructTree: TPdfDictionary;
    fXObjectList: TPdfArray;
    fDefaultPageWidth: cardinal;
    fDefaultPageHeight: cardinal;
    fDefaultPaperSize: TPdfPaperSize;
    fCompressionMethod: TPdfCompressionMethod;
    fCharSet: integer;
    fCodePage: cardinal;
    fEngine: TSynAnsiConvert;
    fTrueTypeFonts: TRawUtf8DynArray;
    fTrueTypeFontLastName: RawUtf8;
    fTrueTypeFontLastIndex: integer;
    fDC: HDC;
    fScreenLogPixels: integer;
    fPrinterPxPerInch: TPoint;
    fStandardFontsReplace: boolean;
    fEmbeddedTtf: boolean;
    fEmbeddedWholeTtf: boolean;
    fEmbeddedTtfIgnore: TRawUtf8List;
    fRawPages: TSynList;
    fSelectedDCFontOld: HDC;
    fForceJPEGCompression: integer;
    fUseOutlines: boolean;
    fUseOptionalContent: boolean;
    {$ifdef USE_UNISCRIBE}
    fUseUniscribe: boolean;
    {$endif USE_UNISCRIBE}
    fForceNoBitmapReuse: boolean;
    fUseFontFallBack: boolean;
    fFontFallBackIndex: integer;
    // a list of Bookmark text keys, associated to a TPdfDest object
    fBookMarks: TRawUtf8List;
    fMissingBookmarks: TRawUtf8List;
    fLastOutline: TPdfOutlineEntry; // used by CreateOutline
    fFileFormat: TPdfFileFormat;
    fPdfA: TPdfALevel;
    fSaveToStreamWriter: TPdfWrite;
    {$ifdef USE_PDFSECURITY}
    fEncryption: TPdfEncryption;
    fEncryptionObject: TPdfDictionary;
    fCurrentObjectNumber: integer;
    fCurrentGenerationNumber: integer;
    {$endif USE_PDFSECURITY}
    fFileID: THash128;
    function GetGeneratePdf15File: boolean;
    procedure SetGeneratePdf15File(Value: boolean);
    function GetInfo: TPdfInfo;
      {$ifdef HASINLINE}inline;{$endif}
    function GetOutlineRoot: TPdfOutlineRoot;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetStandardFontsReplace(Value: boolean);
    {$ifdef HASINLINE}inline;{$endif}
    function GetEmbeddedTtfIgnore: TRawUtf8List;
    procedure SetDefaultPaperSize(Value: TPdfPaperSize);
    procedure SetDefaultPageHeight(Value: cardinal);
    procedure SetDefaultPageWidth(Value: cardinal);
    procedure SetUseOptionalContent(Value: boolean);
    procedure SetPdfA(Value: TPdfALevel);
    function GetDefaultPageLandscape: boolean;
    procedure SetDefaultPageLandscape(Value: boolean);
    procedure SetFontFallBackName(const Value: string);
    function GetFontFallBackName: string;
  protected
    /// can be useful in descendant objects in other units
    fTPdfPageClass: TPdfPageClass;
    procedure RaiseInvalidOperation;
    procedure CreateInfo;
    /// get the PostScript Name of a TrueType Font
    // - use the Naming Table ('name') of the TTF content if not 7 bit ascii
    function TtfFontPostcriptName(aFontIndex: integer; AStyle: TPdfFontStyles;
      AFont: TPdfFontTrueType): PdfString;
    /// register the font in the font list
    procedure RegisterFont(aFont: TPdfFont);
    /// get the PDF font, from its internal PDF name (e.g. 'Helvetica-Bold')
    // - if the specified font exists in the font list, returns the corresponding object
    // - if the font doesn't exist yet, returns NIL
    function GetRegisteredNotTrueTypeFont(const APdfFontName: PdfString): TPdfFont;
    /// get the supplied TrueType Font from the internal font list
    // - warning: the font index is fTrueTypeFonts.IndexOf(AName)+1, since
    // font index 0 is reserved for all not true Type fonts
    // - if the true type font doesn't exist yet, returns NIL
    // - always return the WinAnsi version of the font: the caller has to
    // use the UnicodeFont property to get the corresponding Unicode aware
    // version, if it was used
    function GetRegisteredTrueTypeFont(AFontIndex: integer;
      AStyle: TPdfFontStyles; ACharSet: byte): TPdfFont; overload;
    /// get the supplied TrueType Font from the internal font list
    // - if the true type font doesn't exist yet, returns NIL
    function GetRegisteredTrueTypeFont(const AFontLog: TLogFontW): TPdfFont; overload;
    /// find an index of in fTrueTypeFonts[]
    function GetTrueTypeFontIndex(const AName: RawUtf8): integer;
    // select the specified font object, then return the fDC value
    function GetDCWithFont(Ttf: TPdfFontTrueType): HDC;
    /// release the current document content
    procedure FreeDoc;
  public
    /// create the PDF document instance, with a Canvas and a default A4 paper size
    // - the current charset and code page are retrieved from the SysLocale
    // value, so the PDF engine is MBCS ready
    // - note that only Win-Ansi encoding allows use of embedded standard fonts
    // - you can specify a Code Page to be used for the PdfString encoding;
    // by default (ACodePage left to 0), the current system code page is used
    // - you can create a PDF/A compliant document by setting APdfA level
    // - you can set an encryption instance, by using TPdfEncryption.New()
    constructor Create(AUseOutlines: boolean = false;
      ACodePage: integer = 0; APdfA: TPdfALevel = pdfaNone
      {$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption = nil{$endif}); reintroduce;
    /// release the PDF document instance
    destructor Destroy; override;
    /// create a new document
    // - this method is called first, by the Create constructor
    // - you can call it multiple time if you want to reset the whole document content
    procedure NewDoc;
    /// add a Page to the current PDF document
    function AddPage: TPdfPage; virtual;
    /// register a font to the internal TTF font list
    // - some fonts may not be enumerated in the system, e.g. after calling
    // AddFontMemResourceEx, so could be registered by this method
    // - to be called just after Create(), before anything is written
    function AddTrueTypeFont(const TtfName: RawUtf8): boolean;
    /// create a Pages object
    // - Pages objects can be nested, to save memory used by the Viewer
    // - only necessary if you have more than 8000 pages (this method is called
    // by TPdfDocument.NewDoc, so you shouldn't have to use it)
    function CreatePages(Parent: TPdfDictionary): TPdfDictionary;
    /// register an object (typicaly a TPdfImage) to the PDF document
    // - returns the internal index as added in fXObjectList[]
    function RegisterXObject(AObject: TPdfXObject; const AName: PdfString): integer;
    /// add then register an object (typicaly a TPdfImage) to the PDF document
    // - returns the internal index as added in fXObjectList[]
    function AddXObject(const AName: PdfString; AXObject: TPdfXObject): integer;
    /// save the PDF file content into a specified Stream
    procedure SaveToStream(AStream: TStream; ForceModDate: TDateTime = 0); virtual;
    /// prepare to save the PDF file content into a specified Stream
    // - is called by SaveToStream() method
    // - you can then append other individual pages with SaveToStreamCurrentPage
    // to avoid most resource usage (e.g. for report creation)
    // - shall be finished by a SaveToStreamDirectEnd call
    procedure SaveToStreamDirectBegin(AStream: TStream; ForceModDate: TDateTime = 0);
    /// save the current page content to the PDF file
    // - shall be made one or several times after a SaveToStreamDirectBegin() call
    // and before a final SaveToStreamDirectEnd call
    // - see TPdfDocumentGdi.SaveToStream() in this unit, and
    // TGDIPages.ExportPDFStream() in mORMotReport.pas for real use cases
    // - you can set FlushCurrentPageNow=true to force the current page to be
    // part of the flushed content
    procedure SaveToStreamDirectPageFlush(FlushCurrentPageNow: boolean = false); virtual;
    /// prepare to save the PDF file content into a specified Stream
    // - shall be made once after a SaveToStreamDirectBegin() call
    // - is called by SaveToStream() method
    procedure SaveToStreamDirectEnd;
    /// save the PDF file content into a specified file
    // - return false on any writing error (e.g. if the file is opened in the
    // Acrobar Reader)
    function SaveToFile(const aFileName: TFileName): boolean;

    /// convert a UTF-8 text into a PdfString of this document CodePage
    function ToPdfString(const Value: RawUtf8): PdfString;
    /// retrieve a XObject from its name
    // - this method will handle also the Virtual Objects
    function GetXObject(const AName: PdfString): TPdfXObject;
    /// retrieve a XObject index from its name
    // - this method won't handle the Virtual Objects
    function GetXObjectIndex(const AName: PdfString): integer;
    /// retrieve a XObject TPdfImage index from its picture attributes
    // - returns '' if this image is not already there
    // - uses 128-bit hashes of the TBitmap content to avoid false positives
    function GetXObjectImageName(
      const Hash: THash128Rec; Width, Height: integer): PdfString;
    /// wrapper to create an annotation
    // - the annotation is set to a specified position of the current page
    function CreateAnnotation(AType: TPdfAnnotationSubType;
      const ARect: TPdfRect; BorderStyle: TPdfAnnotationBorder = abSolid;
      BorderWidth: integer = 1): TPdfDictionary;
    /// wrapper to create a Link annotation, specified by a bookmark
    // - the link is set to a specified rectangular position of the current page
    // - if the bookmark name is not existing (i.e. if it no such name has been
    // defined yet via the CreateBookMark method), it's added to the internal
    // fMissingBookmarks list, and will be linked at CreateBookMark method call
    function CreateLink(const ARect: TPdfRect; const aBookmarkName: RawUtf8;
      BorderStyle: TPdfAnnotationBorder = abSolid;
      BorderWidth: integer = 1): TPdfDictionary;
    /// wrapper to create a hyper-link, with a specific URL value
    function CreateHyperLink(const ARect: TPdfRect; const url: RawUtf8;
      BorderStyle: TPdfAnnotationBorder = abSolid;
      BorderWidth: integer = 0): TPdfDictionary;
    /// create an Outline entry at a specified position of the current page
    // - the outline tree is created from the specified numerical level (0=root),
    // just after the item added via the previous CreateOutline call
    // - the title is a RTL string, to handle fully Unicode support
    function CreateOutline(const Title: string; Level: integer;
      TopPosition: single): TPdfOutlineEntry;
    /// create a Destination
    // - the current PDF Canvas page is associated with this destination object
    function CreateDestination: TPdfDestination;
    /// create an internal bookmark entry at a specified position of the current page
    // - the current PDF Canvas page is associated with the destination object
    // - a dtXYZ destination with the corresponding TopPosition Y value is defined
    // - the associated bookmark name must be unique, otherwise an exception is raised
    procedure CreateBookMark(TopPosition: single; const aBookmarkName: RawUtf8);
    /// create an image from a supplied bitmap
    // - returns the internal XObject name of the resulting TPdfImage
    // - if you specify a PPdfBox to draw the image at the given position/size
    // - if the same bitmap content is sent more than once, the TPdfImage will
    // be reused (it will therefore spare resulting pdf file space) - if the
    // ForceNoBitmapReuse is false
    // - if ForceCompression property is set, the picture will be stored as a JPEG
    // - you can specify a clipping rectangle region as ClipRc parameter
    function CreateOrGetImage(B: TBitmap; DrawAt: PPdfBox = nil;
      ClipRc: PPdfBox = nil): PdfString;
    /// create a new optional content group (layer)
    // - returns a TPdfOptionalContentGroup needed for
    // TPdfCanvas.BeginMarkedContent
    // - if ParentContentGroup is not nil, the new content group is a
    // subgroup to ParentContentGroup
    // - Title is the string shown in the PDF Viewer
    // - Visible controls the initial state of the content group
    function CreateOptionalContentGroup(ParentContentGroup: TPdfOptionalContentGroup;
      const Title: string; Visible: boolean = true): TPdfOptionalContentGroup;
    /// create a Radio Optional ContentGroup
    // - ContentGroups is a array of TPdfOptionalContentGroups which should behave like
    // radiobuttons, i.e. only one active at a time
    // - visibility must be set with CreateOptionalContentGroup, only one group should be visible
    procedure CreateOptionalContentRadioGroup(
      const ContentGroups: array of TPdfOptionalContentGroup);
    /// create an attached file from its name
    function CreateFileAttachment(const AttachFile: TFileName;
      const Description: string = ''): TPdfDictionary;
    /// create an attached file from its buffer content
    function CreateFileAttachmentFromBuffer(const Buffer: RawByteString;
      const Title, Description, MimeType: string;
      CreationDate, ModDate: TDateTime): TPdfDictionary;
    /// retrieve the current PDF Canvas, associated to the current page
    property Canvas: TPdfCanvas
      read fCanvas;
    /// retrieve the PDF information, associated to the PDF document
    property Info: TPdfInfo
      read GetInfo;
    // retrieve the PDF Document Catalog, as root of the document's object hierarchy
    property Root: TPdfCatalog
      read fRoot;
    /// retrieve the PDF Outline, associated to the PDF document
    // - UseOutlines must be set to true before any use of the OutlineRoot property
    property OutlineRoot: TPdfOutlineRoot
      read GetOutlineRoot;
    /// the default page width, used for new every page creation (i.e. AddPage method call)
    property DefaultPageWidth: cardinal
      read fDefaultPageWidth write SetDefaultPageWidth;
    /// the default page height, used for new every page creation (i.e. AddPage method call)
    property DefaultPageHeight: cardinal
      read fDefaultPageHeight write SetDefaultPageHeight;
    /// the default page orientation
    // - a call to this property will swap default page width and height if the
    // orientation is not correct
    property DefaultPageLandscape: boolean
      read GetDefaultPageLandscape write SetDefaultPageLandscape;
    /// the default page size, used for every new page creation (i.e. AddPage method call)
    // - a write to this property this will reset the default paper orientation
    // to Portrait: you must explicitly set DefaultPageLandscape to true, if needed
    property DefaultPaperSize: TPdfPaperSize
      read fDefaultPaperSize write SetDefaultPaperSize;
    /// the compression method used for page content storage
    // - is set by default to cmFlateDecode when the class instance is created
    property CompressionMethod: TPdfCompressionMethod
      read fCompressionMethod write fCompressionMethod;
    /// if set to true, the used true Type fonts will be embedded to the PDF content
    // - not set by default, to save disk space and produce tiny PDF
    property EmbeddedTtf: boolean
      read fEmbeddedTtf write fEmbeddedTtf;
    /// you can add some font names to this list, if you want these fonts
    // NEVER to be embedded to the PDF file, even if the EmbeddedTtf property is set
    // - if you want to ignore all standard windows fonts, use:
    // !   EmbeddedTtfIgnore.Text := MSWINDOWS_DEFAULT_FONTS;
    property EmbeddedTtfIgnore: TRawUtf8List
      read GetEmbeddedTtfIgnore;
    /// if set to true, the embedded true Type fonts will be totaly Embeddeded
    // - by default, is set to false, meaning that a subset of the TTF font is
    // stored into the PDF file, i.e. only the used glyphs are stored
    // - this option is only available if running on Windows XP or later
    property EmbeddedWholeTtf: boolean
      read fEmbeddedWholeTtf write fEmbeddedWholeTtf;
    /// used to define if the PDF document will use outlines
    // - must be set to true before any use of the OutlineRoot property
    property UseOutlines: boolean
      read fUseOutlines write fUseOutlines;
    // used to define if the PDF document will use optional content (layers)
    // - will also force PDF 1.5 as minimal file format
    // - must be set to true before calling NewDoc
    // - warning: setting a value to this propery after creation will call the
    // NewDoc method, therefore will erase all previous content and pages
    // (including Info properties)
    property UseOptionalContent: boolean
      read fUseOptionalContent write SetUseOptionalContent;
    /// the current Code Page encoding used for this PDF Document
    property CodePage: cardinal
      read fCodePage;
    /// the current Code Page encoder used for this PDF Document
    property Engine: TSynAnsiConvert
      read fEngine;
    /// the current CharSet used for this PDF Document
    property CharSet: integer
      read fCharSet;
    /// set if the PDF engine must use standard fonts substitution
    // - if true, 'Arial', 'Times New Roman' and 'Courier New' will be
    // replaced by the corresponding internal Type 1 fonts, defined in the Reader
    // - only works with current ANSI_CHARSET, i.e. if you want to display
    // some other unicode characters, don't enable this property: all non WinAnsi
    // glyphs would be replaced by a '?' sign
    // - default value is false (i.e. not embedded standard font)
    property StandardFontsReplace: boolean
      read fStandardFontsReplace write SetStandardFontsReplace;
    {$ifdef USE_UNISCRIBE}
    /// set if the PDF engine must use the Windows Uniscribe API to
    // render Ordering and/or Shaping of the text
    // - useful for Hebrew, Arabic and some Asiatic languages handling
    // - set to false by default, for faster content generation
    // - you can set this property temporary to true, when using the Canvas
    // property, but this property must be set appropriately before the content
    // generation if you use any TPdfDocumentGdi.VclCanvas text output with
    // such scripting (since the PDF rendering is done once just before the
    // saving, e.g. before SaveToFile() or SaveToStream() methods calls)
    // - the PDF engine don't handle Font Fallback yet: the font you use
    // must contain ALL glyphs necessary for the supplied unicode text - squares
    // or blanks will be drawn for any missing glyph/character
    property UseUniscribe: boolean
      read fUseUniscribe write fUseUniscribe;
    {$endif USE_UNISCRIBE}
    /// used to define if the PDF document will handle "font fallback" for
    // characters not existing in the current font: it will avoid rendering
    // block/square symbols instead of the correct characters (e.g. for Chinese text)
    // - will use the font specified by FontFallBackName property to add any
    // Unicode glyph not existing in the currently selected font
    // - default value is true
    property UseFontFallBack: boolean
      read fUseFontFallBack write fUseFontFallBack;
    /// set the font name to be used for missing characters
    // - used only if UseFontFallBack is true
    // - default value is 'Lucida Sans Unicode' or 'Arial Unicode MS', if
    // available - but you may also consider https://fonts.google.com/noto/fonts
    // - here is a list of usual fallback fonts which could potentially work
    // based on the language (any additionnal feedback is welcome):
    // $ Arabic: "Tahoma" or "Amiri"
    // $ Armenian: "Sylfaen"
    // $ Bengali: "Vrinda" or "Nirmala UI"
    // $ Chinese (Simplified): "Simsun" or "Microsoft YaHei"
    // $ Chinese (Traditional): "MingLiU" or "Microsoft JhengHei"
    // $ Cyrillic (Russian): "Arial" or "Times New Roman"
    // $ Ethiopic (Amharic): "Nyala" or "Ebrima"
    // $ Georgian: "Sylfaen"
    // $ Greek: "Arial" or "Georgia"
    // $ Hebrew: "David" or "Arial Hebrew"
    // $ Hindi/Devanagari: "Mangal" or "Nirmala UI"
    // $ Japanese: "MS Mincho" or "Meiryo"
    // $ Khmer: "Khmer UI"
    // $ Korean: "Malgun Gothic" or "Batang"
    // $ Lao: "Lao UI"
    // $ Tamil: "Latha" or "Nirmala UI"
    // $ Thai: "Tahoma" or "LilyUPC"
    // $ Vietnamese: "Arial" or "Tahoma"
    property FontFallBackName: string
      read GetFontFallBackName write SetFontFallBackName;

    /// this property can force saving all canvas bitmaps images as JPEG
    // - handle bitmaps added by VclCanvas/TMetaFile and bitmaps added as TPdfImage
    // - by default, this property is set to 0 by the constructor of this class,
    // meaning that the JPEG compression is not forced, and the engine will use
    // the native resolution of the bitmap - in this case, the resulting
    // PDF file content will be bigger in size (e.g. use this for printing)
    // - 60 is the prefered way e.g. for publishing PDF over the internet
    // - 80/90 is a good ratio if you want to have a nice PDF to see on screen
    // - of course, this doesn't affect vectorial (i.e. emf) pictures
    property ForceJPEGCompression: integer
      read fForceJPEGCompression write fForceJPEGCompression;
    /// this property can force all canvas bitmaps to be stored directly
    // - by default, the library will try to match an existing same bitmap
    // content, and reuse the existing pdf object - you can set this property
    // for a faster process, if you do not want to use this feature
    property ForceNoBitmapReuse: boolean
      read fForceNoBitmapReuse write fForceNoBitmapReuse;
    /// direct read-only access to all corresponding TPdfPage
    // - can be useful in inherited classe
    property RawPages: TSynList
      read fRawPages;
    /// the resolution used for pixel to PDF coordinates conversion
    // - by default, contains the Number of pixels per logical inch
    // along the screen width
    // - you can override this value if you really need additional resolution
    // for your bitmaps and such - this is useful only with TPdfDocumentGdi and
    // its associated TCanvas: all TPdfDocument native TPdfCanvas methods use
    // the native resolution of the PDF, i.e. more than 7200 DPI (since we
    // write coordinates with 2 decimals per point - which is 1/72 inch)
    property ScreenLogPixels: integer
      read fScreenLogPixels write fScreenLogPixels;
    /// is pdfaXXX if the file was created in order to be PDF/A compliant
    // - set PdfA parameter to a level for Create constructor in order to use it
    // - warning: setting a value to this propery after creation will call the
    // NewDoc method, therefore will erase all previous content and pages
    // (including Info properties)
    property PdfA: TPdfALevel
      read fPdfA write SetPdfA;
    /// set to true to force PDF 1.5 format, which may produce smaller files
    property GeneratePdf15File: boolean
      read GetGeneratePdf15File write SetGeneratePdf15File;
  end;

  /// a PDF page
  TPdfPage = class(TPdfDictionary)
  protected
    fDoc: TPdfDocument;
    fMediaBox: TPdfArray;
    fWordSpace: single;
    fCharSpace: single;
    fFontSize: single;
    fFont: TPdfFont;
    fLeading: single;
    fHorizontalScaling: single;
    function GetPageLandscape: boolean;
    procedure SetPageLandscape(Value: boolean);
    procedure SetWordSpace(Value: single);
    procedure SetCharSpace(Value: single);
    procedure SetFontSize(Value: single);
    procedure SetHorizontalScaling(Value: single);
    procedure SetLeading(Value: single);
    procedure SetPageWidth(AValue: integer); virtual;
    procedure SetPageHeight(AValue: integer); virtual;
    function GetPageWidth: integer;
    function GetPageHeight: integer;
    function GetResources(const AName: PdfString): TPdfDictionary;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the page with its internal VCL/LCL Canvas
    constructor Create(ADoc: TPdfDocument); reintroduce; virtual;
    /// calculate width of specified text according to current attributes
    // - this function is compatible with MBCS strings
    function TextWidth(const Text: PdfString): single;
    /// calculate the number of chars which can be displayed in the specified
    // width, according to current attributes
    // - this function is compatible with MBCS strings, and returns
    // the index in Text, not the glyphs index
    function MeasureText(const Text: PdfString; Width: single): integer;
  public
    /// retrieve or set the word Space attribute, in PDF coordinates of 1/72 inch
    property WordSpace: single
      read fWordSpace write SetWordSpace;
    /// retrieve or set the Char Space attribute, in PDF coordinates of 1/72 inch
    property CharSpace: single
      read fCharSpace write SetCharSpace;
    /// retrieve or set the Horizontal Scaling attribute, in PDF coordinates of 1/72 inch
    property HorizontalScaling: single
      read fHorizontalScaling write SetHorizontalScaling;
    /// retrieve or set the text Leading attribute, in PDF coordinates of 1/72 inch
    property Leading: single
      read fLeading write SetLeading;
    /// retrieve or set the font Size attribute, in system TFont.Size units
    property FontSize: single
      read fFontSize write SetFontSize;
    /// retrieve the current used font
    // - for TPdfFontTrueType, this points not always to the WinAnsi version of
    // the Font, but can also point to the Unicode Version, if the last
    // drawn character by ShowText() was unicode - see TPdfWrite.AddUnicodeHexText
    property Font: TPdfFont
      read fFont write fFont;
    /// retrieve or set the current page width, in PDF coordinates of 1/72 inch
    property PageWidth: integer
      read GetPageWidth write SetPageWidth;
    /// retrieve or set the current page height, in PDF coordinates of 1/72 inch
    property PageHeight: integer
      read GetPageHeight write SetPageHeight;
    /// retrieve or set the paper orientation
    property PageLandscape: boolean
      read GetPageLandscape write SetPageLandscape;
  end;

  /// access to the PDF Canvas, used to draw on the page
  TPdfCanvas = class
  protected
    fContents: TPdfStream;
    fPage: TPdfPage;
    fPageFontList: TPdfDictionary;
    fDoc: TPdfDocument;
    // = 72/fDoc.fScreenLogPixels
    fFactor: single;
    // = ViewSize.cx/WinSize.cx*fFactor
    fFactorX: single;
    // = ViewSize.cy/WinSize.cy*fFactor
    fFactorY: single;
    // = (MulDiv(ViewOrg.x, WinSize.cx, ViewSize.cx) - WinOrg.x)*fFactor
    fOffsetX: single;
    // = FHeight - (MulDiv(ViewOrg.y, WinSize.cy, ViewSize.cy) - WinOrg.y)*fFactor
    fOffsetY: single;
    // = XOff,YOff parameters specified in RenderMetaFile()
    fOffsetXDef, fOffsetYDef: single;
    // WorldTransform factor and offs
    fWorldFactorX, fWorldFactorY, fWorldOffsetX, fWorldOffsetY, fAngle,
      fWorldOffsetCos, fWorldOffsetSin: single;
    fDevScaleX, fDevScaleY: single;
    fWinSize, fViewSize: TSize;
    fWinOrg, fViewOrg: TPoint;
    fMappingMode: integer;
    fEmfBounds: TRect;
    fPrinterPxPerInch: TPoint;
    fNewPath: boolean;
    {$ifdef USE_UNISCRIBE}
    /// if Uniscribe-related methods must handle the text from right to left
    fRightToLeftText: boolean;
    {$endif USE_UNISCRIBE}
    /// parameters taken from RenderMetaFile() call
    fUseMetaFileTextPositioning: TPdfCanvasRenderMetaFileTextPositioning;
    fUseMetaFileTextClipping: TPdfCanvasRenderMetaFileTextClipping;
    fKerningHScaleBottom: single;
    fKerningHScaleTop: single;
    // some cache
    fPreviousRasterFontName: RawUtf8;
    fPreviousRasterFontIndex: integer;
    // result := fOffsetX + (X * fFactorX);
    function I2X(X: integer): single;
    function S2X(X: single): single;
    // result := fOffsetY - Y * fFactorY;
    function I2Y(Y: integer): single;
    function S2Y(Y: single): single;
    // wrapper calling I2X/S2X and I2Y/S2Y for conversion
    procedure LineToI(x, y: integer);
    procedure LineToS(x, y: single);
    procedure MoveToI(x, y: integer);
    procedure MoveToS(x, y: single);
    procedure CurveToCI(x1, y1, x2, y2, x3, y3: integer);
    procedure RoundRectI(x1, y1, x2, y2, cx, cy: integer);
    procedure ArcI(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer;
      clockwise: boolean; arctype: TPdfCanvasArcType; var position: TPoint);
    function BoxI(const Box: TRect; Normalize: boolean): TPdfBox;
      {$ifdef HASINLINE}inline;{$endif}
    procedure PointI(x, y: single);
      {$ifdef HASINLINE}inline;{$endif}
    function RectI(const Rect: TRect; Normalize: boolean): TPdfRect;
    procedure DrawXObjectPrepare(const AXObjectName: PdfString);
    // wrappers about offset calculation
    function ViewOffsetX(X: single): single;
    function ViewOffsetY(Y: single): single;
    property WorldFactorX: single
      read fWorldFactorX write fWorldFactorX;
    property WorldFactorY: single
      read fWorldFactorY write fWorldFactorY;
    // property getters
    function GetDoc: TPdfDocument;
      {$ifdef HASINLINE}inline;{$endif}
    function GetPage: TPdfPage;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the PDF canvas instance
    constructor Create(APdfDoc: TPdfDocument);

    /// pushes a copy of the entire graphics state onto the stack
    procedure GSave;                                             {  q   }
    /// restores the entire graphics state to its former value by popping
    // it from the stack
    procedure GRestore;                                          {  Q   }
    /// Modify the CTM by concatenating the specified matrix
    // - The current transformation matrix (CTM) maps positions from user
    // coordinates to device coordinates
    // - This matrix is modified by each application of the ConcatToCTM method
    // - CTM Initial value is  a matrix that transforms default user coordinates
    // to device coordinates
    // - since floating-point precision does make sense for a transformation
    // matrix, we added a custom decimal number parameter here
    procedure ConcatToCTM(a, b, c, d, e, f: single; Decimals: cardinal = 6); {  cm  }

    /// Set the flatness tolerance in the graphics state
    // - see Section 6.5.1, "Flatness Tolerance" of the PDF 1.3 reference:
    // The flatness tolerance controls the maximum permitted distance in
    // device pixels between the mathematically correct path and an
    // approximation constructed from straight line segments
    // - Flatness is a number in the range 0 to 100; a value of 0 specifies
    // the output device's default flatness tolerance
    procedure SetFlat(flatness: Byte);                           {  i   }
    /// Set the line cap style in the graphics state
    // - The line cap style specifies the shape to be used at the
    // ends of open subpaths (and dashes, if any) when they are stroked
    procedure SetLineCap(linecap: TLineCapStyle);                {  J   }
    /// Set the line dash pattern in the graphics state
    // - The line dash pattern controls the pattern of dashes and gaps
    // used to stroke paths. It is specified by a dash array and a dash phase.
    // The dash array's elements are numbers that specify the lengths of
    // alternating dashes and gaps; the dash phase specifies the distance into
    // the dash pattern at which to start the dash. The elements of both the
    // dash array and the dash phase are expressed in user space units.
    // Before beginning to stroke a path, the dash array is cycled through,
    // adding up the lengths of dashes and gaps. When the accumulated length
    // equals the value specified by the dash phase, stroking of the path begins,
    // using the dash array cyclically from that point onward.
    procedure SetDash(const aarray: array of integer; phase: integer = 0); {  d   }
    /// Set the line join style in the graphics state
    // - The  line join style specifies the shape to be used at the
    // corners of paths that are stroked
    procedure SetLineJoin(linejoin: TLineJoinStyle);             {  j   }
    /// Set the line width in the graphics state
    // - The line width parameter specifies the thickness of the line used
    // to stroke a path. It is a nonnegative number expressed in user space
    // units; stroking a path entails painting all points whose perpendicular
    // distance from the path in user space is less than or equal to half the
    // line width. The effect produced in device space depends on the current
    // transformation matrix (CTM) in effect at the time the path is stroked.
    // If the CTM specifies scaling by different factors in the x and y
    // dimensions, the thickness of stroked lines in device space will vary
    // according to their orientation. The actual line width achieved can differ
    // from the requested width by as much as 2 device pixels, depending on
    // the positions of lines with respect to the pixel grid.
    procedure SetLineWidth(linewidth: single);                   {  w   }
    /// Set the miter limit in the graphics state
    // - When two line segments meet at a sharp angle and mitered joins have been
    // specified as the line join style, it is possible for the miter to extend
    // far beyond the thickness of the line stroking the path. The miter limit
    // imposes a maximum on the ratio of the miter length to the line width.
    // When the limit is exceeded, the join is converted from a miter to a bevel
    procedure SetMiterLimit(miterlimit: single);                 {  M   }

    /// change the current coordinates position
    // - Begin a new subpath by moving the current point to coordinates
    // (x, y), omitting any connecting line segment. If the previous path
    // construction operator in the current path was also MoveTo(), the new MoveTo()
    // overrides it; no vestige of the previous MoveTo() call remains in the path.
    procedure MoveTo(x, y: single);                              {  m   }
    /// Append a straight line segment from the current point to the point (x, y).
    // -  The new current point is (x, y)
    procedure LineTo(x, y: single);                              {  l   }
    /// Append a cubic Bezier curve to the current path
    // - The curve extends from the current point to the point (x3, y3),
    // using (x1, y1) and (x2, y2) as the Bezier control points
    // - The new current point is (x3, y3)
    procedure CurveToC(x1, y1, x2, y2, x3, y3: single);          {  c   }
    /// Append a cubic Bezier curve to the current path
    // - The curve extends from the current point to the point (x3, y3),
    // using the current point and (x2, y2) as the Bezier control points
    // - The new current point is (x3, y3)
    procedure CurveToV(x2, y2, x3, y3: single);                  {  v   }
    /// Append a cubic Bezier curve to the current path
    // - The curve extends from the current point to the point (x3, y3),
    // using (x1, y1) and (x3, y3) as the Bezier control points
    // - The new current point is (x3, y3)
    procedure CurveToY(x1, y1, x3, y3: single);                  {  y   }
    /// Append a rectangle to the current path as a complete subpath, with
    // lower-left corner (x, y) and dimensions  width and  height in user space
    procedure Rectangle(x, y, width, height: single);            {  re  }
    /// Close the current subpath by appending a straight line segment
    // from the current point to the starting point of the subpath
    // - This operator terminates the current subpath; appending another
    // segment to the current path will begin a new subpath, even if the new
    // segment begins at the endpoint reached by the h operation
    // - If the current subpath is already closed or the current path is empty,
    // it does nothing
    procedure Closepath;                                         {  h   }
    /// End the path object without filling or stroking it
    // - This operator is a "path-painting no-op", used primarily for the
    // side effect of changing the clipping path
    procedure NewPath;                                           {  n   }
    /// Stroke the path
    procedure Stroke;                                            {  S   }
    /// Close and stroke the path
    // - This operator has the same effect as the sequence ClosePath; Stroke;
    procedure ClosePathStroke;                                   {  s   }
    /// Fill the path, using the nonzero winding number rule to determine
    // the region to fill
    procedure Fill;                                              {  f   }
    /// Fill the path, using the even-odd rule to determine the region to fill
    procedure EoFill;                                            {  f*  }
    /// Fill and then stroke the path, using the nonzero winding number rule
    // to determine the region to fill
    // - This produces the same result as constructing two identical path
    // objects, painting the first with Fill and the second with Stroke. Note,
    // however, that the filling and stroking portions of the operation consult
    // different values of several graphics state parameters, such as the color
    procedure FillStroke;                                        {  B   }
    /// Close, fill, and then stroke the path, using the nonzero winding number
    // rule to determine the region to fill
    // - This operator has the same effect as the sequence ClosePath; FillStroke;
    procedure ClosepathFillStroke;                               {  b   }
    /// Fill and then stroke the path, using the even-odd rule to determine
    // the region to fill
    // - This operator produces the same result as FillStroke, except that
    // the path is filled as if with Eofill instead of Fill
    procedure EofillStroke;                                      {  B*  }
    /// Close, fill, and then stroke the path, using the even-odd rule to
    // determine the region to fill
    // - This operator has the same effect as the sequence Closepath; EofillStroke;
    procedure ClosepathEofillStroke;                             {  b*  }
    /// Nonzero winding clipping path set
    // - Modify the current clipping path by intersecting it with the current path,
    // using the nonzero winding number rule to determine which regions
    // lie inside the clipping path
    // - The graphics state contains a clipping path that limits the regions of
    // the page affected by painting operators. The closed subpaths of this path
    // define the area that can be painted. Marks falling inside this area will
    // be applied to the page; those falling outside it will not. (Precisely what
    // is considered to be inside a path is discussed under "Filling", above.)
    // - The initial clipping path includes the entire page. Both clipping path
    // methods (Clip and EoClip) may appear after the last path construction operator
    // and before the path-painting operator that terminates a path object.
    // Although the clipping path operator appears before the painting operator,
    // it does not alter the clipping path at the point where it appears. Rather,
    // it modifies the effect of the succeeding painting operator. After the path
    // has been painted, the clipping path in the graphics state is set to the
    // intersection of the current clipping path and the newly constructed path.
    procedure Clip;                                              {  W   }
    /// Even-Odd winding clipping path set
    // - Modify the current clipping path by intersecting it with the current path,
    // using the even-odd rule to determine which regions lie inside the clipping path
    procedure EoClip;                                            {  W*  }

    /// Set the character spacing
    // - CharSpace is a number expressed in unscaled text space units.
    // - Character spacing is used by the ShowText and ShowTextNextLine methods
    // - Default value is 0
    procedure SetCharSpace(charSpace: single);                   {  Tc  }
    /// Set the word spacing
    // - WordSpace is a number expressed in unscaled text space units
    // - word spacing is used by the ShowText and ShowTextNextLine methods
    // - Default value is 0
    procedure SetWordSpace(wordSpace: single);                   {  Tw  }
    /// Set the horizontal scaling to (scale/100)
    // - hScaling is a number specifying the percentage of the normal width
    // - Default value is 100 (e.g. normal width)
    procedure SetHorizontalScaling(hScaling: single);              {  Tz  }
    /// Set the text leading, Tl, to the specified leading value
    // - leading which is a number expressed in unscaled text space units;
    // it specifies the vertical distance between the baselines of adjacent
    // lines of text
    // - Text leading is used only by the MoveToNextLine and ShowTextNextLine methods
    // - you can force the next line to be just below the current one by calling:
    // ! SetLeading(Attributes.FontSize);
    // - Default value is 0
    procedure SetLeading(leading: single);                       {  TL  }
    /// Set the font, Tf, to  font and the font size, Tfs , to size.
    // - font is the name of a font resource in the Font subdictionary of the
    // current resource dictionary (e.g. 'F0')
    // - size is a number representing a scale factor
    // - There is no default value for either font or size; they must be specified
    // using this method before any text is shown
    procedure SetFontAndSize(const fontshortcut: PdfString; size: single);    {  Tf  }
      {$ifdef HASINLINE}inline;{$endif}
    /// Set the text rendering mode
    // - the text rendering mode determines whether text is stroked, filled,
    // or used as a clipping path
    procedure SetTextRenderingMode(mode: TTextRenderingMode);    {  Tr  }
    /// Set the text rise, Trise, to the specified value
    // - rise is a number expressed in unscaled text space units, which
    // specifies the distance, in unscaled text space units, to move the
    // baseline up or down from its default location. Positive values of
    // text rise move the baseline up. Adjustments to the baseline are
    // useful for drawing superscripts or subscripts. The default location of
    // the baseline can be restored by setting the text rise to 0.
    // - Default value is 0
    procedure SetTextRise(rise: word);                           {  Ts  }
    /// Begin a text object
    // - Text objects cannot be nested
    procedure BeginText;                   {  BT  }
     {$ifdef HASINLINE}inline;{$endif}
    /// End a text object, discarding the text matrix
    procedure EndText;                     {  ET  }
      {$ifdef HASINLINE}inline;{$endif}
    /// Move to the start of the next line, offset from the start of the current
    // line by (tx ,ty)
    // - tx and ty are numbers expressed in unscaled text space units
    procedure MoveTextPoint(tx, ty: single); {  Td  }
      {$ifdef HASINLINE}inline;{$endif}
    /// set the Text Matrix to a,b,c,d and the text line Matrix x,y
    procedure SetTextMatrix(a, b, c, d, x, y: single);           {  Tm  }
    /// Move to the start of the next line
    procedure MoveToNextLine;                                    {  T*  }
    {$ifdef HASVARUSTRING}
    /// Show a text string
    // - text is expected to be Unicode encoded
    // - if NextLine is true, moves to the next line and show a text string;
    // in this case, method as the same effect as MoveToNextLine; ShowText(s);
    procedure ShowText(const text: UnicodeString; NextLine: boolean = false);
      overload; inline; {  Tj  or ' }
    {$endif HASVARUSTRING}
    /// Show a text string
    // - text is expected to be Ansi-Encoded, in the current CharSet; if
    // some Unicode or MBCS conversion is necessary, it will be notified to the
    // corresponding
    // - if NextLine is true, moves to the next line and show a text string;
    // in this case, method as the same effect as MoveToNextLine; ShowText(s);
    procedure ShowText(const text: PdfString; NextLine: boolean = false);
      overload; {  Tj  or ' }
    /// Show an Unicode Text string
    // - if NextLine is true, moves to the next line and show a text string;
    // in this case, method as the same effect as MoveToNextLine; ShowText(s);
    procedure ShowText(PW: PWideChar; NextLine: boolean = false); overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// Show an Unicode Text string, encoded as Glyphs or the current font
    // - PW must follow the ETO_GLYPH_INDEX layout, i.e. refers to an array as
    // returned from the GetCharacterPlacement: all glyph indexes are 16-bit values
    procedure ShowGlyph(PW: PWord; Count: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// Paint the specified XObject
    procedure ExecuteXObject(const xObject: PdfString);                   {  Do  }

    /// Set the color space to a Device-dependent RGB value
    // - this method set the color to use for nonstroking operations
    procedure SetRGBFillColor(Value: TPdfColor);                 {  rg  }
    /// Set the color space to a Device-dependent RGB value
    // - this method set the color to use for stroking operations
    procedure SetRGBStrokeColor(Value: TPdfColor);               {  RG  }
    /// Set the color space to a CMYK percent value
    // - this method set the color to use for nonstroking operations
    procedure SetCMYKFillColor(C, M, Y, K: integer);                 {  k  }
    /// Set the color space to a CMYK value
    // - this method set the color to use for stroking operations
    procedure SetCMYKStrokeColor(C, M, Y, K: integer);               {  K  }

    /// assign the canvas to the specified page
    procedure SetPage(APage: TPdfPage); virtual;
    /// set the current font for the PDF Canvas
    procedure SetPdfFont(AFont: TPdfFont; ASize: single);
    /// set the current font for the PDF Canvas
    // - expect the font name to be either a standard embedded font
    // ('Helvetica','Courier','Times') or its Windows equivalency (i.e.
    // 'Arial','Courier New','Times New Roman'), either a UTF-8 encoded
    // true Type font name available on the system
    // - if no CharSet is specified (i.e. if it remains -1), the current document
    // CharSet parameter is used
    function SetFont(const AName: RawUtf8; ASize: single; AStyle: TPdfFontStyles;
      ACharSet: integer = -1; AForceTtf: integer = -1;
      AIsFixedWidth: boolean = false): TPdfFont; overload;
    /// set the current font for the PDF Canvas
    // - this method use the Win32 structure that defines the characteristics
    // of the logical font
    function SetFont(
      ADC: HDC; const ALogFont: TLogFontW; ASize: single): TPdfFont; overload;

    /// show some text at a specified page position
    procedure TextOut(X, Y: single; const Text: PdfString);
    /// show some unicode text at a specified page position
    procedure TextOutW(X, Y: single; PW: PWideChar);
    /// show the text in the specified rectangle and alignment
    // - optional clipping can be applied
    procedure TextRect(ARect: TPdfRect; const Text: PdfString;
      Alignment: TPdfAlignment; Clipping: boolean);
    /// show the text in the specified rectangle and alignment
    // - text can be multiline, separated by CR + LF (i.e. #13#10)
    // - text can optionaly word wrap
    // - note: this method only work with embedded fonts by now, not true type
    // fonts (because it use text width measuring)
    procedure MultilineTextRect(ARect: TPdfRect; const Text: PdfString;
      WordWrap: boolean);
    /// draw the specified object (typicaly an image) with stretching
    procedure DrawXObject(X, Y, AWidth, AHeight: single;
      const AXObjectName: PdfString);
    /// draw the specified object (typicaly an image) with stretching and clipping
    procedure DrawXObjectEx(X, Y, AWidth, AHeight: single;
      ClipX, ClipY, ClipWidth, ClipHeight: single; const AXObjectName: PdfString);
    /// draw an ellipse
    // - use Bezier curves internaly to draw the ellipse
    procedure Ellipse(x, y, width, height: single);
    /// draw a rounded rectangle
    // - use Bezier curves internaly to draw the rounded rectangle
    procedure RoundRect(x1, y1, x2, y2, cx, cy: single);
    /// calculate width of specified text according to current Canvas attributes
    // - works with MBCS strings
    function TextWidth(const Text: PdfString): single;
    /// calculate width of specified text according to current Canvas attributes
    // - this function compute the raw width of the specified text, and won't
    // use HorizontalScaling, CharSpace nor WordSpace in its calculation
    function UnicodeTextWidth(PW: PWideChar): single;
    /// calculate the number of chars which can be displayed in the specified
    // width, according to current attributes
    // - this function is compatible with MBCS strings, and returns
    // the index in Text, not the glyphs index
    // - note: this method only work with embedded fonts by now, not true type
    // fonts (because text width measuring is not yet implemented for them)
    function MeasureText(const Text: PdfString; AWidth: single): integer;
    /// get the index of the next word in the supplied text
    // - this function is compatible with MBCS strings, and returns
    // the index in Text, not the glyphs index
    function GetNextWord(const S: PdfString; var Index: integer): PdfString;
    // starts optional content (layer)
    // - Group must be registered with TPdfDocument.CreateOptionalContentGroup
    // - each BeginMarkedContent must have a corresponding EndMarkedContent
    // - nested BeginMarkedContent/EndMarkedContent are possible
    procedure BeginMarkedContent(Group: TPdfOptionalContentGroup);
    // ends optional content (layer)
    procedure EndMarkedContent;
  public
    /// retrieve the current Canvas content stream, i.e. where the PDF
    // commands are to be written to
    property Contents: TPdfStream
      read fContents;
    /// retrieve the current Canvas Page
    property Page: TPdfPage
      read GetPage;
    /// retrieve the associated PDF document instance which created this Canvas
    property Doc: TPdfDocument
      read GetDoc;
    {$ifdef USE_UNISCRIBE}
    /// if Uniscribe-related methods must handle the text from right to left
    property RightToLeftText: boolean
      read fRightToLeftText write fRightToLeftText;
    {$endif USE_UNISCRIBE}
  end;

  {$M+}
  /// common ancestor to all dictionary wrapper classes
  TPdfDictionaryWrapper = class
  protected
    fData: TPdfDictionary;
    function GetHasData: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    procedure SetData(AData: TPdfDictionary);
  public
    /// the associated dictionary, containing all data
    property Data: TPdfDictionary
      read fData write SetData;
    /// return true if has any data stored within
    property HasData: boolean
      read GetHasData;
  end;
  {$M-}

  /// a dictionary wrapper class for the PDF document information fields
  // - all values use the RTL string type, and will be encoded
  // as Unicode if necessary
  TPdfInfo = class(TPdfDictionaryWrapper)
  protected
    fCustomMetadata: RawUtf8;
    function GetAuthor: string;
    procedure SetAuthor(const Value: string);
    function GetCreationDate: TDateTime;
    procedure SetCreationDate(Value: TDateTime);
    function GetCreator: string;
    procedure SetCreator(const Value: string);
    function GetKeywords: string;
    procedure SetKeywords(const Value: string);
    function GetSubject: string;
    procedure SetSubject(const Value: string);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetModDate: TDateTime;
    procedure SetModDate(Value: TDateTime);
  public
    /// the PDF document Author
    property Author: string
      read GetAuthor write SetAuthor;
    /// the PDF document Creation Date
    property CreationDate: TDateTime
      read GetCreationDate write SetCreationDate;
    /// the Software or Library name which created this PDF document
    property Creator: string
      read GetCreator write SetCreator;
    /// the PDF document associated key words
    property Keywords: string
      read GetKeywords write SetKeywords;
    /// the PDF document modification date
    property ModDate: TDateTime
      read GetModDate write SetModDate;
    /// the PDF document subject
    property Subject: string
      read GetSubject write SetSubject;
    /// the PDF document title
    property Title: string
      read GetTitle write SetTitle;
    /// allow to add some custom XMP metadata for PDFA/1
    // - may be used e.g. to generate ZUGFeRD-PDF content
    property CustomMetadata: RawUtf8
     read fCustomMetadata write fCustomMetadata;
  end;

  /// a dictionary wrapper class for the PDF document catalog fields
  // - It contains references to other objects defining the document's contents,
  // outline, article threads (PDF 1.1), named destinations, and other attributes.
  // In addition, it contains information about how the document should be displayed
  // on the screen, such as whether its outline and thumbnail page images should be
  // displayed automatically and whether some location other than the first page
  // should be shown when the document is opened
  TPdfCatalog = class(TPdfDictionaryWrapper)
  protected
    fOpenAction: TPdfDestination;
    fOwner: TPdfDocument;
    procedure SetPageLayout(Value: TPdfPageLayout);
    procedure SetPageMode(Value: TPdfPageMode);
    procedure SetNonFullScreenPageMode(Value: TPdfPageMode);
    procedure SetViewerPreference(Value: TPdfViewerPreferences);
    procedure SetPages(APages: TPdfDictionary);
    function GetPageLayout: TPdfPageLayout;
    function GetPageMode: TPdfPageMode;
    function GetNonFullScreenPageMode: TPdfPageMode;
    function GetViewerPreference: TPdfViewerPreferences;
    function GetPages: TPdfDictionary;
  protected
    procedure SaveOpenAction;
  public
    /// a Destination to be displayed when the document is opened
    property OpenAction: TPdfDestination
      read fOpenAction write fOpenAction;
    /// The page layout to be used when the document is opened
    property PageLayout: TPdfPageLayout
      read GetPageLayout write SetPageLayout;
    /// Page mode determines how the document should appear when opened
    property NonFullScreenPageMode: TPdfPageMode
      read GetNonFullScreenPageMode write SetNonFullScreenPageMode;
    /// Page mode determines how the document should appear when opened
    property PageMode: TPdfPageMode
      read GetPageMode write SetPageMode;
    /// A viewer preferences dictionary specifying the way the document is to be
    // displayed on the screen
    // - If this entry is absent, viewer applications should use their own current
    // user preference settings
    property ViewerPreference: TPdfViewerPreferences
      read GetViewerPreference write SetViewerPreference;
    /// The page tree node that is the root of the document's page tree
    // - Required, must be an indirect reference
    // - you can set a value to it in order to add some nested pages
    property Pages: TPdfDictionary
      read GetPages write SetPages;
  end;

  /// a generic PDF font object
  TPdfFont = class(TPdfDictionaryWrapper)
  protected
    fName: PdfString;
    fShortCut: PdfString;
    fFirstChar, fLastChar: integer;
    fDefaultWidth: cardinal;
    fAscent, fDescent: integer;
    fUnicode: boolean;
    /// index in TrueTypeFontsIndex[] + 1, 0 if not a TPdfFontTrueType
    // - same TPdfFontTrueType index may appear multiple times in the font list,
    // e.g. with normal, bold and/or italic attributes
    // - this hidden property is used by TPdfDocument for faster font list handling
    fTrueTypeFontsIndex: integer;
    /// contains a bit for every WinAnsi encoded char
    // - encoding in TPdfFont, even if used by TPdfFontWinAnsi descendent only
    fWinAnsiUsed: TSynAnsicharSet;
  public
    /// create the PDF font object instance
    constructor Create(AXref: TPdfXref; const AName: PdfString);
    /// mark some WinAnsi char as used
    procedure AddUsedWinAnsiChar(aChar: AnsiChar);
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the width of a specified character
    // - implementation of this method is either WinAnsi (by TPdfFontWinAnsi),
    // either compatible with MBCS strings (TPdfFontCIDFontType2)
    // - return 0 by default (descendant must handle the Ansi charset)
    function GetAnsiCharWidth(const AText: PdfString; APos: integer): integer; virtual;
    /// the internal PDF font name (e.g. 'Helvetica-Bold')
    // - postscript font names are inside the unit: these postscript names
    // could not match the "official" true Type font name, stored as
    // UTF-8 in fTrueTypeFonts
    property Name: PdfString
      read FName;
    /// the internal PDF shortcut (e.g. 'F3')
    property ShortCut: PdfString
      read FShortCut;
    /// is set to true if the font is dedicated to Unicode Chars
    property Unicode: boolean
      read fUnicode;
  end;

  PPdfWinAnsiWidth = ^TPdfWinAnsiWidth;
  TPdfWinAnsiWidth = array[#32..#255] of word;

  /// a generic PDF font object, handling at least WinAnsi encoding
  // - TPdfFontTrueType descendent will handle also Unicode chars,
  // for all WideChar which are outside the WinAnsi selection
  TPdfFontWinAnsi = class(TPdfFont)
  protected
    /// contain the Width array of the corresponding WinAnsi encoded char
    fWinAnsiWidth: PPdfWinAnsiWidth;
  public
    /// retrieve the width of a specified character
    // - implementation of this method expect WinAnsi encoding
    // - return the value contained in fWinAnsiWidth[] by default
    function GetAnsiCharWidth(const AText: PdfString; APos: integer): integer; override;
    /// release the used memory
    destructor Destroy; override;
  end;

  /// an embedded WinAnsi-Encoded standard Type 1 font
  // - handle Helvetica, Courier and Times font by now
  TPdfFontType1 = class(TPdfFontWinAnsi)
  protected
  public
    /// create a standard font instance, with a given name and char widths
    // - if WidthArray is nil, it will create a fixed-width font of 600 units
    // - WidthArray[0]=Ascent, WidthArray[1]=Descent, WidthArray[2..]=Width(#32..)
    constructor Create(AXref: TPdfXref; const AName: PdfString;
      WidthArray: PSmallIntArray); reintroduce;
  end;

  /// an embedded Composite CIDFontType2
  // - i.e. a CIDFont whose glyph descriptions are based on TrueType font technology
  // - typicaly handle Japan or Chinese standard fonts
  // - used with MBCS encoding, not WinAnsi
  TPdfFontCIDFontType2 = class(TPdfFont)
    { TODO: implement standard TPdfFontCIDFontType2 MBCS font }
  end;

  TPointArray = array[word] of TPoint;
  PPointArray = ^TPointArray;

  TSmallPointArray = array[word] of TSmallPoint;
  PSmallPointArray = ^TSmallPointArray;

  /// The 'cmap' table begins with an index containing the table version number
  // followed by the number of encoding tables. The encoding subtables follow.
  TCmapHeader = packed record
    /// Version number (Set to zero)
    version: word;
    /// Number of encoding subtables
    numberSubtables: word;
  end;
  /// points to every 'cmap' encoding subtables

  TCmapSubTableArray = packed array[byte] of packed record
    /// Platform identifier
    platformID: word;
    /// Platform-specific encoding identifier
    platformSpecificID: word;
    /// Offset of the mapping table
    offset: cardinal;
  end;

  /// The 'hhea' table contains information needed to layout fonts whose
  // characters are written horizontally, that is, either left to right or
  // right to left
  TCmapHHEA = packed record
    version: integer;
    ascent: word;
    descent: word;
    lineGap: word;
    advanceWidthMax: word;
    minLeftSideBearing: word;
    minRightSideBearing: word;
    xMaxExtent: word;
    caretSlopeRise: SmallInt;
    caretSlopeRun: SmallInt;
    caretOffset: SmallInt;
    reserved: Int64;
    metricDataFormat: SmallInt;
    numOfLongHorMetrics: word;
  end;

  /// The 'head' table contains global information about the font
  TCmapHEAD = packed record
    version: integer;
    fontRevision: integer;
    checkSumAdjustment: cardinal;
    magicNumber: cardinal;
    flags: word;
    unitsPerEm: word;
    createdDate: Int64;
    modifiedDate: Int64;
    xMin: SmallInt;
    yMin: SmallInt;
    xMax: SmallInt;
    yMax: SmallInt;
    macStyle: word;
    lowestRec: word;
    fontDirection: SmallInt;
    indexToLocFormat: SmallInt;
    glyphDataFormat: SmallInt
  end;

  /// header for the 'cmap' Format 4 table
  // - this is a two-byte encoding format
  TCmapFmt4 = packed record
    format: word;
    length: word;
    language: word;
    segCountX2: word;
    searchRange: word;
    entrySelector: word;
    rangeShift: word;
  end;

  /// handle Unicode glyph description for a true Type Font
  // - cf http://www.microsoft.com/typography/OTSPEC/otff.htm#otttables
  // - handle Microsoft cmap format 4 encoding (i.e. most used
  // true type fonts on Windows)
  TPdfTtf = class
  protected
    // we use TWordDynArray for auto garbage collection and generic handling
    // - since the Ttf file is big endian, we swap all words at loading, to
    // be used directly by the Intel x86 code; integer (32-bit) values
    // must take care of this byte swapping
    fcmap, fhead, fhhea, fhmtx: TWordDynArray;
  public
    // these are pointers to the useful data of the true Type Font:
    /// Font header
    head: ^TCmapHEAD;
    /// Horizontal header
    hhea: ^TCmapHHEA;
    /// Character to glyph mapping (cmap) table, in format 4
    fmt4: ^TCmapFmt4;
    /// Start character code for each cmap format 4 segment
    startCode: PWordArray;
    /// End characterCode for each cmap format 4 segment
    endCode: PWordArray;
    /// Delta for all character codes in each cmap format 4 segment
    idDelta: PSmallIntArray;
    /// Offsets into glyphIndexArray or 0
    idRangeOffset: PWordArray;
    /// Glyph index array (arbitrary length)
    glyphIndexArray: PWordArray;
  public
    /// create Unicode glyph description for a supplied true Type Font
    // - the HDC of its corresponding document must have selected the font first
    // - this constructor will fill fUsedWide[] and fUsedWideChar of aUnicodeTtf
    // with every available unicode value, and its corresponding glyph and width
    constructor Create(aUnicodeTtf: TPdfFontTrueType); reintroduce;
  end;

  /// this dynamic array stores details about used unicode characters
  // - every used unicode character has its own width and glyph index in the
  // true type font content
  // - Used maps Width and Glyph fields, to quickly search if set
  TUsedWide = array of packed record
    case byte of
      0:
        (Width: word;
         Glyph: word;);
      1:
        (Used: integer;);
  end;

  /// handle TrueType Font
  // - handle both WinAnsi text and Unicode characters in two separate
  // TPdfFontTrueType instances (since PDF need two separate fonts with
  // diverse encoding)
  TPdfFontTrueType = class(TPdfFontWinAnsi)
  protected
    function GetWideCharUsed: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  protected
    fStyle: TPdfFontStyles;
    fDoc: TPdfDocument;
    // note: fUsedWide[] and fUsedWideChar are used:
    // - in WinAnsi Fonts for glyphs used by ShowText
    // - in Unicode Fonts for all available glyphs from TPdfTtf values
    fUsedWideChar: TSortedWordArray;
    fUsedWide: TUsedWide;
    fHGDI: HGDIOBJ;
    fFixedWidth: boolean;
    fFontDescriptor: TPdfDictionary;
    fFontFile2: TPdfStream;
    fUnicodeFont: TPdfFontTrueType;
    fWinAnsiFont: TPdfFontTrueType;
    fIsSymbolFont: boolean;
    // below are some bigger structures
    fLogFont: TLogFontW;
    fM: TTextMetric;
    fOTM: TOutlineTextmetric;
    procedure CreateAssociatedUnicodeFont;
    // update font description from used chars
    procedure PrepareForSaving;
    // low level add glyph (returns the real glyph index found, aGlyph if none)
    function GetAndMarkGlyphAsUsed(aGlyph: word): word;
  public
    /// create the TrueType font object instance
    constructor Create(ADoc: TPdfDocument; AFontIndex: integer;
      AStyle: TPdfFontStyles; const ALogFont: TLogFontW;
      AWinAnsiFont: TPdfFontTrueType); reintroduce;
    /// release the associated memory and handles
    destructor Destroy; override;
    /// mark some UTF-16 codepoint as used
    // - return the index in fUsedWideChar[] and fUsedWide[]
    // - this index is the one just added, or the existing one if the value
    // was found to be already in the fUserWideChar[] array
    function FindOrAddUsedWideChar(aWideChar: WideChar): integer;
    /// retrieve the width of an UTF-16 codepoint
    // - WinAnsi characters are taken from fWinAnsiWidth[], unicode chars from
    // fUsedWide[].Width
    function GetWideCharWidth(aWideChar: WideChar): integer;
    /// is set to true if the PDF used any true type encoding
    property WideCharUsed: boolean
      read GetWideCharUsed;
    /// the associated Font Styles
    property Style: TPdfFontStyles
      read fStyle;
    /// is set to true if the font has a fixed width
    property FixedWidth: boolean
      read fFixedWidth;
    /// points to the corresponding Unicode font
    // - returns NIL if the Unicode font has not yet been created by the
    // CreateUnicodeFont method
    // - may return SELF if the font is itself the Unicode version
    property UnicodeFont: TPdfFontTrueType
      read fUnicodeFont;
    /// points to the corresponding WinAnsi font
    // - always return a value, whatever it is self
    property WinAnsiFont: TPdfFontTrueType
      read fWinAnsiFont;
  end;

  /// A destination defines a particular view of a document, consisting of the following:
  // - The page of the document to be displayed
  // - The location of the display window on that page
  // - The zoom factor to use when displaying the page
  TPdfDestination = class
  protected
    fDoc: TPdfDocument;
    fPage: TPdfPage;
    fType: TPdfDestinationType;
    fValues: array[0..3] of integer;
    fZoom: single;
    fReference: TObject;
    procedure SetElement(Index: integer; Value: integer);
    procedure SetZoom(Value: single);
    function GetElement(Index: integer): integer;
    function GetPageWidth: integer;
    function GetPageHeight: integer;
  public
    /// create the PDF destination object
    // - the current document page is associated with this destination
    constructor Create(APdfDoc: TPdfDocument);
    /// release the object
    destructor Destroy; override;
    /// retrieve the array containing the location of the display window
    // - the properties values which are not used are ignored
    function GetValue: TPdfArray;
    /// Destination Type determines default user space coordinate system of
    // Explicit destinations
    property DestinationType: TPdfDestinationType
      read fType write fType;
    /// the associated PDF document which created this Destination object
    property Doc: TPdfDocument
      read fDoc;
    /// the associated Page
    property Page: TPdfPage
      read fPage;
    /// retrieve the left coordinate of the location of the display window
    property Left: integer index 0
      read GetElement write SetElement;
    /// retrieve the top coordinate of the location of the display window
    property Top: integer index 1
      read GetElement write SetElement;
    /// retrieve the righ tcoordinate of the location of the display window
    property Right: integer index 2
      read GetElement write SetElement;
    /// retrieve the bottom coordinate of the location of the display window
    property Bottom: integer index 3
      read GetElement write SetElement;
    /// the page height of the current page
    // - return the corresponding MediaBox value
    property PageHeight: integer
      read GetPageHeight;
    /// the page width of the current page
    // - return the corresponding MediaBox value
    property PageWidth: integer
      read GetPageWidth;
    /// the associated Zoom factor
    // - by default, the Zoom factor is 1
    property Zoom: single
      read fZoom write SetZoom;
    /// an object associated to this destination, to be used for conveniance
    property Reference: TObject
      read fReference write fReference;
  end;

  /// an Outline entry in the PDF document
  TPdfOutlineEntry = class(TPdfDictionaryWrapper)
  protected
    fParent: TPdfOutlineEntry;
    fNext: TPdfOutlineEntry;
    fPrev: TPdfOutlineEntry;
    fFirst: TPdfOutlineEntry;
    fLast: TPdfOutlineEntry;
    fDest: TPdfDestination;
    fDoc: TPdfDocument;
    fTitle: string;
    fOpened: boolean;
    fCount: integer;
    fReference: TObject;
    fLevel: integer;
  protected
    procedure Save; virtual;
  public
    /// create the Outline entry instance
    // - if TopPosition is set, a corresponding destination is created
    // on the current PDF Canvas page, at this Y position
    constructor Create(AParent: TPdfOutlineEntry;
      TopPosition: integer = -1); reintroduce;
    /// release the associated memory and reference object
    destructor Destroy; override;
    /// create a new entry in the outline tree
    // - this is the main method to create a new entry
    function AddChild(TopPosition: integer = -1): TPdfOutlineEntry;
    /// the associated PDF document which created this Destination object
    property Doc: TPdfDocument
      read fDoc;
    /// the parent outline entry of this entry
    property Parent: TPdfOutlineEntry
      read fParent;
    /// the next outline entry of this entry
    property Next: TPdfOutlineEntry
      read fNext;
    /// the previous outline entry of this entry
    property Prev: TPdfOutlineEntry
      read fPrev;
    /// the first outline entry of this entry list
    property First: TPdfOutlineEntry
      read fFirst;
    /// the last outline entry of this entry list
    property Last: TPdfOutlineEntry
      read fLast;
    /// the associated destination
    property Dest: TPdfDestination
      read fDest write fDest;
    /// the associated title
    // - is a RTL string, so is Unicode ready
    property Title: string
      read fTitle write fTitle;
    /// if the outline must be opened
    property Opened: boolean
      read fOpened write fOpened;
    /// an object associated to this destination, to be used for conveniance
    property Reference: TObject
      read fReference write fReference;
    /// an internal property (not exported to PDF content)
    property Level: integer
      read fLevel write fLevel;
  end;

  /// Root entry for all Outlines of the PDF document
  // - this is a "fake" entry which must be used as parent for all true
  // TPdfOutlineEntry instances, but must not be used as a real outline entry
  TPdfOutlineRoot = class(TPdfOutlineEntry)
  public
    /// create the Root entry for all Outlines of the PDF document
    constructor Create(ADoc: TPdfDocument); reintroduce;
    /// update internal parameters (like outline entries count) before saving
    procedure Save; override;
  end;

  /// generic image object
  // - is either bitmap encoded or jpeg encoded
  TPdfImage = class(TPdfXObject)
  protected
    fPixelHeight: integer;
    fPixelWidth: integer;
    fHash: THash128Rec; // 128-bit hash of the TBitmap raw content
  public
    /// create the image from a supplied VCL/LCL TGraphic instance
    // - handle TBitmap and SynGdiPlus picture types, i.e. TJpegImage
    // (stored as jpeg), and TGifImage/TPngImage (stored as bitmap)
    // - use TPdfForm to handle TMetafile in vectorial format
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor Create(aDoc: TPdfDocument; aImage: TGraphic;
      DontAddToFXref: boolean); reintroduce;
    /// create an image from a supplied JPEG file name
    // - will raise an EFOpenError exception if the file doesn't exist
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor CreateJpegDirect(aDoc: TPdfDocument;
      const aJpegFileName: TFileName;
      DontAddToFXref: boolean = true); reintroduce; overload;
    /// create an image from a supplied JPEG content
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor CreateJpegDirect(aDoc: TPdfDocument; aJpegFile: TMemoryStream;
      DontAddToFXref: boolean = true); reintroduce; overload;
    /// width of the image, in pixels units
    property PixelWidth: integer
      read fPixelWidth;
    /// height of the image, in pixels units
    property PixelHeight: integer
      read fPixelHeight;
  end;

  /// a form XObject with a Canvas for drawing
  // - once created, you can create this XObject, then draw it anywhere on
  // any page - see sample
  TPdfFormWithCanvas = class(TPdfXObject)
  protected
    fFontList: TPdfDictionary;
    fPage: TPdfPage;
    fCanvas: TPdfCanvas;
  public
    /// create a form XObject with TPdfCanvas
    constructor Create(aDoc: TPdfDocument; W, H: integer); reintroduce;
    /// release used memory
    destructor Destroy; override;
    /// close the internal canvas
    procedure CloseCanvas;
    /// access to the private canvas associated with the PDF form XObject
    property Canvas: TPdfCanvas
      read fCanvas;
  end;

  /// used to handle compressed object stream (in PDF 1.5 format)
  TPdfObjectStream = class(TPdfXObject)
  protected
    fObjectCount: integer;
    fAddingStream: TPdfWrite;
    fObject: array of record
      Number: integer;
      Position: integer;
    end;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the instance, i.e. its associated stream
    constructor Create(aDoc: TPdfDocument); reintroduce;
    /// release internal memory structures
    destructor Destroy; override;
    /// add an object to this compressed object stream
    // - returns the object index in this object stream
    function AddObject(Value: TPdfObject): PtrInt;
    /// the number of compressed objects within this object stream
    property ObjectCount: integer
      read fObjectCount;
  end;


{************ TPdfDocumentGdi for GDI/TCanvas rendering support }


{$ifdef USE_METAFILE}

/// append a EMR_GDICOMMENT message for handling PDF bookmarks
// - will create a PDF destination at the current position (i.e. the last Y
// parameter of a Move), with some text supplied as bookmark name
procedure GdiCommentBookmark(MetaHandle: HDC; const aBookmarkName: RawUtf8);

/// append a EMR_GDICOMMENT message for handling PDF outline
// - used to add an outline at the current position (i.e. the last Y parameter of
// a Move): the text is the associated title, UTF-8 encoded and the outline tree
// is created from the specified numerical level (0=root)
procedure GdiCommentOutline(MetaHandle: HDC;
  const aTitle: RawUtf8; aLevel: integer);

/// append a EMR_GDICOMMENT message for creating a Link into a specified bookmark
procedure GdiCommentLink(MetaHandle: HDC; const aBookmarkName: RawUtf8;
  const aRect: TRect; NoBorder: boolean);

/// append a EMR_GDICOMMENT message for adding jpeg direct
procedure GdiCommentJpegDirect(MetaHandle: HDC; const aFileName: RawUtf8;
  const aRect: TRect);

/// append a EMR_GDICOMMENT message mapping BeginMarkedContent
// - associate optionally a CreateOptionalContentGroup() instance from the
// current PDF document
procedure GdiCommentBeginMarkContent(MetaHandle: HDC;
  Group: TPdfOptionalContentGroup = nil);

/// append a EMR_GDICOMMENT message mapping EndMarkedContent
procedure GdiCommentEndMarkContent(MetaHandle: HDC);

type
  /// a PDF page, with its corresponding Meta File and Canvas
  TPdfPageGdi = class(TPdfPage)
  private
    // don't use these fVCL* properties directly, but via TPdfDocumentGdi.VclCanvas
    fVclMetaFileCompressed: RawByteString;
    fVclCanvasSize: TSize;
    // it is in fact a TMetaFileCanvas instance from fVclCurrentMetaFile
    fVclCurrentCanvas: TCanvas;
    fVclCurrentMetaFile: TMetaFile;
    // allow to create the meta file and its canvas only if necessary, and
    // compress the page content using SynLZ to reduce memory usage
    procedure CreateVclCanvas;
    procedure SetVclCurrentMetaFile;
    procedure FlushVclCanvas;
  public
    /// release associated memory
    destructor Destroy; override;
  end;

  /// class handling PDF document creation using GDI commands
  // - this class allows using a VCL/LCL standard Canvas class
  // - handles also PDF creation directly from TMetaFile content
  TPdfDocumentGdi = class(TPdfDocument)
  private
    fUseMetaFileTextPositioning: TPdfCanvasRenderMetaFileTextPositioning;
    fUseMetaFileTextClipping: TPdfCanvasRenderMetaFileTextClipping;
    fKerningHScaleTop: single;
    fKerningHScaleBottom: single;
    function GetVclCanvas: TCanvas;
      {$ifdef HASINLINE}inline;{$endif}
    function GetVclCanvasSize: TSize;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the PDF document instance, with a VCL/LCL Canvas property
    // - see TPdfDocument.Create connstructor for the arguments expectations
    constructor Create(AUseOutlines: boolean = false; ACodePage: integer = 0;
      APdfA: TPdfALevel = pdfaNone
      {$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption = nil {$endif});
    /// add a Page to the current PDF document
    function AddPage: TPdfPage; override;
    /// save the PDF file content into a specified Stream
    // - this overridden method draw first the all VclCanvas content into the PDF
    procedure SaveToStream(AStream: TStream; ForceModDate: TDateTime = 0); override;
    /// save the current page content to the PDF file
    // - this overridden method flush the content from the VclCanvas into the PDF
    // - it will reduce the used memory as much as possible, by-passing page
    // content compression
    // - typical use may be:
    // ! with TPdfDocumentGdi.Create do
    // !   try
    // !     Stream := TFileStreamEx.Create(FileName, fmCreate);
    // !     try
    // !       SaveToStreamDirectBegin(Stream);
    // !       for i := 1 to 9 do
    // !       begin
    // !         AddPage;
    // !         with VclCanvas do
    // !         begin
    // !           Font.Name := 'Times new roman';
    // !           Font.Size := 150;
    // !           Font.Style := [fsBold, fsItalic];
    // !           Font.Color := clNavy;
    // !           TextOut(100, 100, 'Page ' + IntToStr(i));
    // !         end;
    // !         SaveToStreamDirectPageFlush; // direct writing
    // !       end;
    // !       SaveToStreamDirectEnd;
    // !     finally
    // !       Stream.Free;
    // !     end;
    // !   finally
    // !     Free;
    // !   end;
    procedure SaveToStreamDirectPageFlush(
      FlushCurrentPageNow: boolean = false); override;
    /// the VCL/LCL Canvas of the current page
    property VclCanvas: TCanvas
      read GetVclCanvas;
    /// the VCL/LCL Canvas size of the current page
    // - useful to calculate coordinates for the current page
    // - filled with (0,0) before first call to VclCanvas property
    property VclCanvasSize: TSize
      read GetVclCanvasSize;
    /// defines how TMetaFile text positioning is rendered
    // - default is tpSetTextJustification
    // - tpSetTextJustification if content used SetTextJustification() API calls
    // - tpExactTextCharacterPositining for exact font kerning, but resulting
    // in bigger pdf size
    // - tpKerningFromAveragePosition will compute average pdf Horizontal Scaling
    // in association with KerningHScaleBottom/KerningHScaleTop properties
    // - replace deprecated property UseSetTextJustification
    property UseMetaFileTextPositioning: TPdfCanvasRenderMetaFileTextPositioning
      read fUseMetaFileTextPositioning write fUseMetaFileTextPositioning;
    /// defines how TMetaFile text clipping should be applied
    // - tcNeverClip has been reported to work better e.g. when app is running
    // on Wine
    property UseMetaFileTextClipping: TPdfCanvasRenderMetaFileTextClipping
      read fUseMetaFileTextClipping write fUseMetaFileTextClipping;
    /// the % limit below which Font Kerning is transformed into PDF Horizontal
    // Scaling commands (when text positioning is tpKerningFromAveragePosition)
    // - set to 99.0 by default
    property KerningHScaleBottom: single
      read fKerningHScaleBottom write fKerningHScaleBottom;
    /// the % limit over which Font Kerning is transformed into PDF Horizontal
    // Scaling commands (when text positioning is tpKerningFromAveragePosition)
    // - set to 101.0 by default
    property KerningHScaleTop: single
      read fKerningHScaleTop write fKerningHScaleTop;
  end;

  /// handle any form XObject
  // - A form XObject (see Section 4.9, of PDF reference 1.3) is a self-contained
  // description of an arbitrary sequence of graphics objects, defined as a
  // PDF content stream
  TPdfForm = class(TPdfXObject)
  private
    fFontList: TPdfDictionary;
  public
    /// create a form XObject from a supplied TMetaFile
    constructor Create(aDoc: TPdfDocumentGdi; aMetaFile: TMetafile); reintroduce;
  end;


/// draw a metafile content into the PDF page
procedure RenderMetaFile(C: TPdfCanvas; MF: TMetaFile; ScaleX: single = 1.0;
  ScaleY: single = 0.0; XOff: single = 0.0; YOff: single = 0.0;
  TextPositioning: TPdfCanvasRenderMetaFileTextPositioning = tpSetTextJustification;
  KerningHScaleBottom: single = 99.0; KerningHScaleTop: single = 101.0;
  TextClipping: TPdfCanvasRenderMetaFileTextClipping = tcAlwaysClip);

{$endif USE_METAFILE}


implementation


{************ Shared types and functions }

{$ifdef FPC}

{ some FPC/Delphi LCL/VCL/LCL compatibility definitions }

type
  TEMRExtTextOut = TEMREXTTEXTOUTW;
  PEMRExtTextOut = ^TEMRExtTextOut;
  PEMRExtCreateFontIndirect = PEMRExtCreateFontIndirectW;

function Rect(ALeft, ATop, ARight, ABottom: integer): TRect;
begin
  result.Left   := ALeft;
  result.Top    := ATop;
  result.Right  := ARight;
  result.Bottom := ABottom;
end;

function Point(X, Y: integer): TPoint;
begin
  result.X := X;
  result.Y := Y;
end;

{$endif FPC}

// we define here basic MBCS low-level Ansi functions for FPC and Unicode Delphi

function StrCharLength(P: PAnsiChar): PtrUInt;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := PtrUInt(windows.CharNextA(P)) - PtrUInt(P);
end;

function NextCharIndex(const S: RawByteString; Index: cardinal): cardinal;
begin
  if S[Index] in LeadBytes then
    result := Index + StrCharLength(PAnsiChar(pointer(S)) + Index - 1)
  else
    result := Index + 1;
end;

function ByteType(P: PAnsiChar; Index: PtrInt): TMbcsByteType;
var
  i: PtrInt;
begin
  result := mbSingleByte;
  if (P = nil) or
     (P[Index] = #0) then
    exit;
  if Index = 0 then
  begin
    if P[0] in LeadBytes then
      result := mbLeadByte;
  end
  else
  begin
    i := Index - 1;
    while (i >= 0) and
          (P[i] in LeadBytes) do
      dec(i);
    if ((Index - i) and 1) = 0 then
      result := mbTrailByte
    else if P[Index] in LeadBytes then
      result := mbLeadByte;
  end;
end;

const
  MWT_IDENTITY      = 1;
  MWT_LEFTMULTIPLY  = 2;
  MWT_RIGHTMULTIPLY = 3;
  MWT_SET           = 4;
  PDF_PAGE_LAYOUT_NAMES: array[TPdfPageLayout] of PdfString = (
    'SinglePage', 'OneColumn', 'TwoColumnLeft', 'TwoColumnRight');
  PDF_PAGE_MODE_NAMES: array[TPdfPageMode] of PdfString = (
    'UseNone', 'UseOutlines', 'UseThumbs', 'FullScreen');
  PDF_ANNOTATION_TYPE_NAMES: array[0..12] of PdfString = (
    'Text', 'Link', 'Sound', 'FreeText', 'Stamp', 'Square', 'Circle',
    'StrikeOut', 'Highlight', 'Underline', 'Ink', 'FileAttachment', 'Popup');
  PDF_DESTINATION_TYPE_NAMES: array[TPdfDestinationType] of PdfString = (
    'XYZ', 'Fit', 'FitH', 'FitV', 'FitR', 'FitB', 'FitBH', 'FitBV');


{ utility functions }

function DateTimeToPdfDate(ADate: TDateTime): TPdfDate;
var
  D: array[2..8] of word;
  i: PtrInt;
begin
  DecodeDate(ADate, D[2], D[3], D[4]);
  DecodeTime(ADate, D[5], D[6], D[7], D[8]);
  SetLength(result{%H-}, 17);
  YearToPChar(D[2], pointer(PtrInt(result) + 2));
  PWord(result)^ := ord('D') + ord(':') shl 8;
  for i := 3 to 7 do
    PWordArray(pointer(result))^[i] := TwoDigitLookupW[D[i]];
  PByteArray(result)[16] := ord('Z');
//  Assert(abs(_PdfDateToDateTime(result)-ADate)<MilliSecsPerSec);
end;

function PdfDateToDateTime(const AText: TPdfDate; out AValue: TDateTime): boolean;
var
  Y, M, D, H, MI, SS: cardinal;
begin
  result := false;
  if Length(AText) < 16 then
    exit;
  Y := ord(AText[3]) * 1000 + ord(AText[4]) * 100 + ord(AText[5]) * 10 +
       ord(AText[6]) - (48 + 480 + 4800 + 48000);
  M := ord(AText[7]) * 10 + ord(AText[8]) - (48 + 480);
  D := ord(AText[9]) * 10 + ord(AText[10]) - (48 + 480);
  AValue := EncodeDate(Y, M, D);
  H  := ord(AText[11]) * 10 + ord(AText[12]) - (48 + 480);
  MI := ord(AText[13]) * 10 + ord(AText[14]) - (48 + 480);
  SS := ord(AText[15]) * 10 + ord(AText[16]) - (48 + 480);
  if (H < 24) and
    (MI < 60) and
    (SS < 60) then // inlined EncodeTime()
    AValue := AValue + (H * MilliSecsPerHour +
                        MI * MilliSecsPerMin +
                        SS * MilliSecsPerSec) / MilliSecsPerDay
  else
    exit;
  result := true;
end;

function HasMultiByteString(Value: PAnsiChar): boolean;
begin
  if Value <> nil then
    while true do
      if Value^ = #0 then
        Break
      else if not (Value^ in LeadBytes) then
        inc(Value)
      else
      begin
        result := true;
        exit;
      end;
  result := false;
end;

function UInt32ToPdfString(Value: cardinal): PdfString;
begin
  UInt32ToUtf8(Value, RawUtf8(result));
end;

function PdfRect(Left, Top, Right, Bottom: single): TPdfRect;
begin
  result.Left   := Left;
  result.Top    := Top;
  result.Right  := Right;
  result.Bottom := Bottom;
end;

function PdfRect(const Box: TPdfBox): TPdfRect;
begin
  result.Left    := Box.Left;
  result.Top    := Box.Top;
  result.Right  := Box.Left + Box.Width;
  result.Bottom := Box.Top - Box.Height;
end;

function PdfBox(Left, Top, Width, Height: single): TPdfBox;
begin
  result.Left   := Left;
  result.Top    := Top;
  result.Width  := Width;
  result.Height := Height;
end;

procedure L2R(W: PWideChar; L: PtrInt);
var
  tmp: TSynTempBuffer;
  i: PtrInt;
begin
  tmp.Init(W, L * 2);
  dec(L);
  for i := 0 to L do
    W[i] := PWideChar(tmp.buf)[L - i];
  tmp.Done;
end;

function PdfCoord(MM: single): integer;
begin
  result := round(2.8346456693 * MM);
end;

function PrinterDriverExists: boolean;
var
  flags, count, dummy: dword;
  level: Byte;
begin
  // avoid using fPrinter.printers.count as this will raise an
  // exception if no printer driver is installed...
  count := 0;
  flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
  level := 4;
  {$ifdef FPC}
  EnumPrinters(flags, nil, level, nil, 0, @count, @dummy);
  {$else}
  EnumPrinters(flags, nil, level, nil, 0, count, dummy);
  {$endif FPC}
  result := (count > 0);
end;

function ParseFetchedPrinterStr(Str: PChar): PChar;
var
  P: PChar;
begin
  result := Str;
  if Str = nil then
    exit;
  P := Str;
  while P^ = ' ' do
    inc(P);
  result := P;
  while (P^ <> #0) and
        (P^ <> ',') do
    inc(P);
  if P^ = ',' then
    P^ := #0;
end;

function CurrentPrinterPaperSize: TPdfPaperSize;
var
  h: THandle;
  pt: TPoint;
  logical, physical: TSize;
  tmp: integer;
  name: array[0..1023] of char;
  PC: PChar;
begin
  result := psUserDefined;
  if not PrinterDriverExists then
    exit;
  GetProfileString('windows', 'device', nil, name, SizeOf(name) - 1);
  PC := ParseFetchedPrinterStr(name);
  if (PC = nil) or
     (PC^ = #0) then
    exit;
  try
    h := CreateDC(nil, PC, nil, nil);
    try
      pt.x := GetDeviceCaps(h, LOGPIXELSX);
      pt.y := GetDeviceCaps(h, LOGPIXELSY);
      physical.cx := GetDeviceCaps(h, PHYSICALWIDTH);
      physical.cy := GetDeviceCaps(h, PHYSICALHEIGHT);
      logical.cx := mulDiv(physical.cx, 254, pt.x * 10);
      logical.cy := mulDiv(physical.cy, 254, pt.y * 10);
    finally
      DeleteDC(h);
    end;
  except
    on Exception do // raised e.g. if no Printer is existing
      exit;
  end;
  with logical do
  begin
    if cx < cy then
    begin // handle landscape or portrait at once
      tmp := cx;
      cx := cy;
      cy := tmp;
    end;
    case cy of
      148:
        result := psA5;
      210:
        result := psA4; // A4 (297 x 210mm)
      216:
        if cx = 279 then
          result := psLetter
        else if cx = 356 then
          result := psLegal;
      297:
        if cx = 420 then
          result := psA3;
    end;
  end;
end;

function CurrentPrinterRes: TPoint;
var
  name: array[0..1023] of Char;
  PC: PChar;
  h: THandle;
begin
  result.X := 300;
  result.Y := 300; // default standard printer resolution
  if not PrinterDriverExists then
    exit;
  GetProfileString('windows', 'device', nil, name, SizeOf(name) - 1);
  PC := ParseFetchedPrinterStr(name);
  if (PC = nil) or
     (PC^ = #0) then
    exit;
  try
    h := CreateDC(nil, PC, nil, nil);
    try
      result.x := GetDeviceCaps(h, LOGPIXELSX);
      result.y := GetDeviceCaps(h, LOGPIXELSY);
    finally
      DeleteDC(h);
    end;
  except
    on Exception do // raised e.g. if no Printer is existing
      exit;
  end;
end;

function CombineTransform(xform1, xform2: XFORM): XFORM;
begin
  result.eM11 := xform1.eM11 * xform2.eM11 + xform1.eM12 * xform2.eM21;
  result.eM12 := xform1.eM11 * xform2.eM12 + xform1.eM12 * xform2.eM22;
  result.eM21 := xform1.eM21 * xform2.eM11 + xform1.eM22 * xform2.eM21;
  result.eM22 := xform1.eM21 * xform2.eM12 + xform1.eM22 * xform2.eM22;
  result.eDx := xform1.eDx * xform2.eM11 + xform1.eDy * xform2.eM21 + xform2.eDx;
  result.eDy := xform1.eDx * xform2.eM12 + xform1.eDy * xform2.eM22 + xform2.eDy;
end;

procedure InitTransformation(x: PXForm;
  var fIntFactorX, fIntFactorY, fIntOffsetX, fIntOffsetY: single);
begin
  if Assigned(x) then
  begin
    fIntFactorX := x^.eM11;
    fIntFactorY := x^.eM22;
    fIntOffsetX := x^.eDx;
    fIntOffsetY := x^.eDy;
  end
  else
  begin
    fIntFactorX := 1;
    fIntFactorY := 1;
    fIntOffsetX := 0;
    fIntOffsetY := 0;
  end;
end;

function DefaultIdentityMatrix: XFORM;
begin
  result.eM11 := 1;
  result.eM12 := 0;
  result.eM21 := 0;
  result.eM22 := 1;
  result.eDx := 0;
  result.eDy := 0;
end;

function ScaleRect(r: TRect; fScaleX, fScaleY: single): TRect;
begin
  result.Left   := Trunc(r.Left * fScaleX);
  result.Top    := Trunc(r.Top * fScaleY);
  result.Right  := Trunc(r.Right * fScaleX);
  result.Bottom := Trunc(r.Bottom * fScaleY);
end;

procedure NormalizeRect(var Rect: TRect); overload;
var
  tmp: integer;
begin // PDF can't draw twisted rects -> normalize such values
  if Rect.Right < Rect.Left then
  begin
    tmp := Rect.Left;
    Rect.Left := Rect.Right;
    Rect.Right := tmp;
  end;
  if Rect.Bottom < Rect.Top then
  begin
    tmp := Rect.Top;
    Rect.Top := Rect.Bottom;
    Rect.Bottom := tmp;
  end;
end;

procedure NormalizeRect(var Rect: TPdfRect); overload;
var
  tmp: single;
begin // PDF can't draw twisted rects -> normalize such values
  if Rect.Right < Rect.Left then
  begin
    tmp := Rect.Left;
    Rect.Left := Rect.Right;
    Rect.Right := tmp;
  end;
  if Rect.Bottom < Rect.Top then
  begin
    tmp := Rect.Top;
    Rect.Top := Rect.Bottom;
    Rect.Bottom := tmp;
  end;
end;

function PrepareTransformation(
  fIntFactorX, fIntFactorY, fIntOffsetX, fIntOffsetY: single): XForm;
begin
  result.eM11 := fIntFactorX;
  result.eM12 := 0;
  result.eM21 := 0;
  result.eM22 := fIntFactorY;
  result.eDx := fIntOffsetX;
  result.eDy := fIntOffsetY;
end;

const
  // those constants are not defined in earlier Delphi revisions
  {$ifdef USE_METAFILE}
  cPI: single = 3.141592654;
  cPIdiv180: single = 0.017453292;
  c180divPI: single = 57.29577951;
  {$endif USE_METAFILE}
  c2PI: double = 6.283185307;
  cPIdiv2: double = 1.570796326;

function RGBA(r, g, b, a: cardinal): COLORREF;
  {$ifdef HASINLINE} inline;{$endif}
begin
  result := ((r shr 8) or ((g shr 8) shl 8) or ((b shr 8) shl 16) or ((a shr 8) shl 24));
end;

procedure SwapBuffer(P: PWordArray; PLen: PtrInt);
var
  i: PtrInt;
begin
  for i := 0 to PLen - 1 do
    P^[i] := bswap16(P^[i]);
end;

function GetTtfData(aDC: HDC; aTableName: PAnsiChar; var Ref: TWordDynArray): pointer;
var
  L: cardinal;
begin
  result := nil;
  L := windows.GetFontData(aDC, PCardinal(aTableName)^, 0, nil, 0);
  if L = GDI_ERROR then
    exit;
  SetLength(Ref, L shr 1 + 1);
  if windows.GetFontData(aDC, PCardinal(aTableName)^, 0, pointer(Ref), L) = GDI_ERROR then
    exit;
  result := pointer(Ref);
  SwapBuffer(result, L shr 1);
end;

function EnumFontsProcW(var LogFont: TLogFontW; var TextMetric: TTextMetric;
  FontType: integer; var List: TRawUtf8DynArray): integer; stdcall;
// we enumerate all available fonts, whatever the charset is, because
// we may miss to enumerate mandatory fonts (e.g. Arial or Times New Roman)
// if current fCharSet is Chinese e.g.
var
  u: RawUtf8;
begin
  with LogFont do
    if ((FontType = DEVICE_FONTTYPE) or
        (FontType = TRUETYPE_FONTTYPE)) and
       (lfFaceName[0] <> '@') then
    begin
      u := RawUnicodeToUtf8(lfFaceName, StrLenW(lfFaceName));
      if (pointer(List) = nil) or
         (List[high(List)] <> u) then
        AddRawUtf8(List, u, true, true);
    end;
  result := 1;
end;

function LCIDToCodePage(ALcid: LCID): integer;
var
  tmp: array[0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, tmp{%H-}, SizeOf(tmp));
  result := StrToIntDef(tmp, GetACP);
end;

// GetTtcIndex() is used by TPdfFontTrueType.PrepareForSaving()

function FindSynUnicode(const values: array of SynUnicode;
  const value: SynUnicode): PtrInt;
begin
  for result := 0 to high(values) do
    if values[result] = value then
      exit;
  result := -1;
end;

// Looks up ttcIndex from list of font names in known ttc font collections.
// For some locales, the lookup may fail
// result must not be greater than FontCount-1
function GetTtcIndex(const FontName: RawUtf8; var TtcIndex: word;
  FontCount: LongWord): boolean;
const
  // lowercased Font names for Simpl/Trad Chinese, Japanese, Korean locales
  BATANG_KO       = #48148#53461;
  BATANGCHE_KO    = BATANG_KO + #52404;
  GUNGSUH_KO      = #44417#49436;
  GUNGSUHCHE_KO   = GUNGSUH_KO + #52404;
  GULIM_KO        = #44404#47548;
  GULIMCHE_KO     = GULIM_KO + #52404;
  DOTUM_KO        = #46027#50880;
  DOTUMCHE_KO     = DOTUM_KO + #52404;
  MINGLIU_CH      = #32048#26126#39636;
  PMINGLIU_CH     = #26032 + MINGLIU_CH;
  MINGLIU_HK_CH   = MINGLIU_CH  + '_hkscs';
  MINGLIU_XB_CH   = MINGLIU_CH  + '-extb';
  PMINGLIU_XB_CH  = PMINGLIU_CH + '-extb';
  MINGLIU_XBHK_CH = MINGLIU_CH  + '-extb_hkscs';
  MSGOTHIC_JA     = #65357#65363#32#12468#12471#12483#12463;
  MSPGOTHIC_JA    = #65357#65363#32#65328#12468#12471#12483#12463;
  MSMINCHO_JA     = #65357#65363#32#26126#26397;
  MSPMINCHO_JA    = #65357#65363#32#65328#26126#26397;
  SIMSUN_CHS      = #23435#20307;
  NSIMSUN_CHS     = #26032#23435#20307;
var
  lcfn: SynUnicode;
begin
  result := true;
  if FindPropName(['batang', 'cambria', 'gulim', 'mingliu', 'mingliu-extb',
    'ms gothic', 'ms mincho', 'simsun'], FontName) >= 0 then
    TtcIndex := 0
  else if FindPropName(['batangche', 'cambria math', 'gulimche', 'pmingliu',
    'pmingliu-extb', 'ms pgothic', 'ms pmincho', 'nsimsun'], FontName) >= 0 then
    TtcIndex := 1
  else if FindPropName(['gungsuh', 'dotum', 'mingliu_hkscs',
    'mingliu_hkscs-extb', 'ms ui gothic'], FontName) >= 0 then
    TtcIndex := 2
  else if FindPropName(['gungsuhche', 'dotumche'], FontName) >= 0 then
    TtcIndex := 3
  else
  begin
    lcfn := LowerCaseSynUnicode(Utf8ToSynUnicode(FontName));
    if FindSynUnicode([BATANG_KO, GULIM_KO, MINGLIU_CH, MINGLIU_XB_CH,
       MSGOTHIC_JA, MSMINCHO_JA, SIMSUN_CHS], lcfn) >= 0 then
      TtcIndex := 0
    else if FindSynUnicode([BATANGCHE_KO, GULIMCHE_KO, MINGLIU_HK_CH,
       PMINGLIU_XB_CH, MSPGOTHIC_JA, MSPMINCHO_JA, NSIMSUN_CHS], lcfn) >= 0 then
      TtcIndex := 1
    else if FindSynUnicode([GUNGSUH_KO, DOTUM_KO, MINGLIU_HK_CH,
       MINGLIU_XBHK_CH], lcfn) >= 0 then
      TtcIndex := 2
    else if FindSynUnicode([GUNGSUHCHE_KO, DOTUMCHE_KO], lcfn) >= 0 then
      TtcIndex := 3
    else
      result := false;
  end;
  if result and
    (TtcIndex > (FontCount - 1)) then
    result := false;
end;


type
  tcaRes = (
    caMoveto,
    caLine,
    caCurve,
    caPosition);

  teaDrawtype = record
    res: tcaRes;
    pts: array[0..2] of record
      X, Y: single;
    end;
  end;

  teaDrawArray = array of teaDrawtype;

function CalcCurveArcData(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer;
  aClockWise: boolean; arctype: TPdfCanvasArcType; out res: teaDrawArray): boolean;
type
  TCoeff = array[0..3] of double;
  TCoeffArray = array[0..1, 0..3] of TCoeff;
const
  // coefficients for error estimation
  // while using cubic Bezier curves for approximation
  // 0 < b/a < 1/4
  coeffsLow: TCoeffArray =
    (((3.85268, -21.229, -0.330434, 0.0127842),
      (-1.61486, 0.706564, 0.225945, 0.263682),
      (-0.910164, 0.388383, 0.00551445, 0.00671814),
      (-0.630184, 0.192402, 0.0098871, 0.0102527)),
     ((-0.162211, 9.94329, 0.13723, 0.0124084),
      (-0.253135, 0.00187735, 0.0230286, 0.01264),
      (-0.0695069, - 0.0437594, 0.0120636, 0.0163087),
      (-0.0328856, -0.00926032, -0.00173573, 0.00527385)));
  // coefficients for error estimation
  // while using cubic Bezier curves for approximation
  // 1/4 <= b/a <= 1
  coeffsHigh: TCoeffArray =
      (((0.0899116, -19.2349, -4.11711, 0.183362),
        (0.138148, -1.45804, 1.32044, 1.38474),
        (0.230903, -0.450262, 0.219963, 0.414038),
        (0.0590565, -0.101062, 0.0430592, 0.0204699)),
       ((0.0164649, 9.89394, 0.0919496, 0.00760802),
        (0.0191603, -0.0322058, 0.0134667, -0.0825018),
        (0.0156192, -0.017535, 0.00326508, -0.228157),
        (-0.0236752, 0.0405821, -0.0173086, 0.176187)));
  // safety factor to convert the "best" error approximation
  // into a "max bound" error
  safety: TCoeff = (0.001, 4.98, 0.207, 0.0067);
var
  fcx, fcy: double;            // center of the ellipse
  faRad, fbRad: double;        // semi-major axis
  feta1, feta2: double;        // start end angle of the arc
  fx1, fy1, fx2, fy2: double;  // start and endpoint
  fArctype: TPdfCanvasArcType; // center to endpoints line inclusion
  fClockWise: boolean;

  procedure InitFuncData;
  var
    lambda1, lambda2: double;
  begin
    fcx := centerx;
    fcy := centery;
    faRad := (W - 1) / 2;
    fbRad := (H - 1) / 2;
    fArctype := arctype;
    // Calculate Rotation at Start and EndPoint
    fClockWise := aClockWise;
    if aClockWise then
    begin
      lambda1 := ArcTan2(Sy - fcy, Sx - fcx);
      lambda2 := ArcTan2(Ey - fcy, Ex - fcx);
    end
    else
    begin
      lambda2 := ArcTan2(Sy - fcy, Sx - fcx);
      lambda1 := ArcTan2(Ey - fcy, Ex - fcx);
    end;
    feta1 := ArcTan2(sin(lambda1) / fbRad, cos(lambda1) / faRad);
    feta2 := ArcTan2(sin(lambda2) / fbRad, cos(lambda2) / faRad);
    // make sure we have eta1 <= eta2 <= eta1 + 2 PI
    feta2 := feta2 - (c2PI * floor((feta2 - feta1) / c2PI));
    // the preceding correction fails if we have exactly et2 - eta1 = 2 PI
    // it reduces the interval to zero length
    if SameValue(feta1, feta2) then
      feta2 := feta2 + c2PI;
    // start point
    fx1 := fcx + (faRad * cos(feta1));
    fy1 := fcy + (fbRad * sin(feta1));
    // end point
    fx2 := fcx + (faRad * cos(feta2));
    fy2 := fcy + (fbRad * sin(feta2));
  end;

  function estimateError(etaA, etaB: double): double;
  var
    coeffs: ^TCoeffArray;
    c0, c1, cos2, cos4, cos6, dEta, eta, x: double;

    function rationalFunction(x: double; const c: TCoeff): double;
    begin
      result := (x * (x * c[0] + c[1]) + c[2]) / (x + c[3]);
    end;

  begin
    eta := 0.5 * (etaA + etaB);
    x := fbRad / faRad;
    dEta := etaB - etaA;
    cos2 := cos(2 * eta);
    cos4 := cos(4 * eta);
    cos6 := cos(6 * eta);
    // select the right coeficients set according to degree and b/a
    if x < 0.25 then
      coeffs := @coeffsLow
    else
      coeffs := @coeffsHigh;
    c0 := rationalFunction(x, coeffs[0][0]) +
          cos2 * rationalFunction(x, coeffs[0][1]) +
          cos4 * rationalFunction(x, coeffs[0][2]) +
          cos6 * rationalFunction(x, coeffs[0][3]);
    c1 := rationalFunction(x, coeffs[1][0]) +
          cos2 * rationalFunction(x, coeffs[1][1]) +
          cos4 * rationalFunction(x, coeffs[1][2]) +
          cos6 * rationalFunction(x, coeffs[1][3]);
    result := rationalFunction(x, safety) * faRad * exp(c0 + c1 * dEta);
  end;

  procedure BuildPathIterator;
  var
    alpha: double;
    found: boolean;
    n: integer;
    dEta, etaB, etaA: double;
    cosEtaB, sinEtaB, aCosEtaB, bSinEtaB, aSinEtaB, bCosEtaB: double;
    xB, yB, xBDot, yBDot: double;
    i: integer;
    t, xA, xADot, yA, yADot: double;
    ressize: integer;         // Index var for result Array
    r: ^teaDrawtype;
    lstartx, lstarty: double; // Start from
  const
    defaultFlatness = 0.5;    // half a pixel
  begin
    // find the number of Bezier curves needed
    found := false;
    n := 1;
    while (not found) and
          (n < 1024) do
    begin
      dEta := (feta2 - feta1) / n;
      if dEta <= cPIdiv2 then
      begin
        etaB := feta1;
        found := true;
        for i := 0 to n - 1 do
        begin
          etaA := etaB;
          etaB := etaB + dEta;
          found := (estimateError(etaA, etaB) <= defaultFlatness);
          if not found then
            break;
        end;
      end;
      n := n shl 1;
    end;
    dEta := (feta2 - feta1) / n;
    etaB := feta1;
    cosEtaB := cos(etaB);
    sinEtaB := sin(etaB);
    aCosEtaB := faRad * cosEtaB;
    bSinEtaB := fbRad * sinEtaB;
    aSinEtaB := faRad * sinEtaB;
    bCosEtaB := fbRad * cosEtaB;
    xB := fcx + aCosEtaB;
    yB := fcy + bSinEtaB;
    xBDot := -aSinEtaB;
    yBDot := +bCosEtaB;
    lstartx := xB;
      lstarty := yB;
    // calculate and reserve Space for the result
    ressize := n;
    case fArctype of
      acArc:
        inc(ressize, 1); // first move
      acArcTo:
        inc(ressize, 3); // first line and move
      acArcAngle:
        inc(ressize, 1); // first move
      acPie:
        inc(ressize, 3); // first and last Line
      acChoord:
        inc(ressize, 2);
    end;
    SetLength(res, ressize);
    r := pointer(res);
    case fArctype of
      acArc:
        begin   // start with move
          r^.res := caMoveto;
          r^.pts[0].x := xB;
          r^.pts[0].Y := yB;
          inc(r);
        end;
      acArcTo:
        begin   // start with line and move
          r^.res := caLine;
          if fClockWise then
          begin
            r^.pts[0].x := fx1;
            r^.pts[0].Y := fy1;
          end
          else
          begin
            r^.pts[0].x := fx2;
            r^.pts[0].Y := fy2;
          end;
          inc(r);
          r^.res := caMoveto;
          r^.pts[0].x := fx1;
          r^.pts[0].Y := fy1;
          inc(r);
        end;
      acArcAngle:
        ;
      acPie:
        begin
          r^.res := caMoveto;
          r^.pts[0].x := fcx;
          r^.pts[0].Y := fcy;
          inc(r);
          r^.res := caLine;
          r^.pts[0].x := xB;
          r^.pts[0].Y := yB;
          inc(r);
        end;
      acChoord:
        begin
          r^.res := caMoveto;
          r^.pts[0].x := xB;
          r^.pts[0].Y := yB;
          inc(r);
        end;
    end;
    t := tan(0.5 * dEta);
    alpha := sin(dEta) * (sqrt(4 + 3 * t * t) - 1) / 3;
    for i := 0 to n - 1 do
    begin
      xA := xB;
      yA := yB;
      xADot := xBDot;
      yADot := yBDot;
      etaB := etaB + dEta;
      cosEtaB := cos(etaB);
      sinEtaB := sin(etaB);
      aCosEtaB := faRad * cosEtaB;
      bSinEtaB := fbRad * sinEtaB;
      aSinEtaB := faRad * sinEtaB;
      bCosEtaB := fbRad * cosEtaB;
      xB := fcx + aCosEtaB;
      yB := fcy + bSinEtaB;
      xBDot := -aSinEtaB;
      yBDot := bCosEtaB;
      r^.res := caCurve;
      r^.pts[0].x := xA + alpha * xADot;
      r^.pts[0].Y := yA + alpha * yADot;
      r^.pts[1].x := xB - alpha * xBDot;
      r^.pts[1].Y := yB - alpha * yBDot;
      r^.pts[2].x := xB;
      r^.pts[2].Y := yB;
      inc(r);
    end; // Loop
    case fArctype of
      acArcTo:
        begin
          r^.res := caPosition;
          if fClockWise then
          begin
            r^.pts[0].x := fx2;
            r^.pts[0].Y := fy2;
          end
          else
          begin
            r^.pts[0].x := fx1;
            r^.pts[0].Y := fy1;
          end
        end;
      acPie:
        begin
          r^.res := caLine;
          r^.pts[0].x := fcx;
          r^.pts[0].Y := fcy;
        end;
      acChoord:
        begin
          r^.res := caLine;
          r^.pts[0].x := lstartx;
          r^.pts[0].Y := lstarty;
        end;
    end;
  end;

begin
  res := nil;
  InitFuncData;
  buildPathIterator;
  result := length(res) > 1;
end;


{************ Internal classes mapping PDF objects }

{ TPdfObject }

constructor TPdfObject.Create;
begin
  fObjectNumber := -1;
end;

procedure TPdfObject.ForceSaveNow;
begin
  fSaveAtTheEnd := false;
end;

procedure TPdfObject.InternalWriteTo(W: TPdfWrite);
begin
  {$ifdef USE_PDFSECURITY}
  if fObjectNumber > 0 then
    with W.fDoc do
    begin
      fCurrentObjectNumber := fObjectNumber;
      fCurrentGenerationNumber := fGenerationNumber;
    end;
  {$endif USE_PDFSECURITY}
end;

procedure TPdfObject.SetObjectNumber(Value: integer);
begin
  fObjectNumber := Value;
  if Value > 0 then
    fObjectType := otIndirectObject
  else
    fObjectType := otDirectObject;
end;

function TPdfObject.SpaceNotNeeded: boolean;
begin
  result := false;
end;

procedure TPdfObject.WriteTo(var W: TPdfWrite);
begin
  if fObjectType = otDirectObject then
    InternalWriteTo(W)
  else
    W.AddWithSpace(fObjectNumber).AddWithSpace(fGenerationNumber).Add('R');
end;

procedure TPdfObject.WriteValueTo(var W: TPdfWrite);
begin
  if fObjectType <> otIndirectObject then
    raise EPdfInvalidOperation.Create('WriteValueTo');
  W.AddWithSpace(fObjectNumber).AddWithSpace(fGenerationNumber).Add('obj' + CRLF);
  InternalWriteTo(W);
  W.Add(CRLF + 'endobj'#10);
end;


{ PdfVirtualObject }

constructor TPdfVirtualObject.Create(AObjectId: integer);
begin
  inherited Create;
  fObjectNumber := AObjectId;
  fObjectType := otVirtualObject;
end;


{ TPdfNull }

procedure TPdfNull.InternalWriteTo(W: TPdfWrite);
begin
  W.Add('null');
end;


{ TPdfBoolean }

procedure TPdfBoolean.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(BOOL_Utf8[Value]);
end;

constructor TPdfBoolean.Create(AValue: boolean);
begin
  inherited Create;
  Value := AValue;
end;


{ TPdfNumber }

procedure TPdfNumber.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(fValue);
end;

constructor TPdfNumber.Create(AValue: integer);
begin
  inherited Create;
  Value := AValue;
end;


{ TPdfReal }

procedure TPdfReal.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(Value);
end;

constructor TPdfReal.Create(AValue: double);
begin
  inherited Create;
  Value := AValue;
end;


{ TPdfText }

procedure TPdfText.InternalWriteTo(W: TPdfWrite);
begin
  // if the value has multibyte character, convert the value to hex unicode.
  // otherwise, escape characters.
  if SysLocale.FarEast and
    HasMultiByteString(pointer(fValue)) then
    W.Add('<FEFF').AddToUnicodeHex(fValue).Add('>')
  else
    W.Add('(').AddEscapeContent(fValue).Add(')');
end;

constructor TPdfText.Create(const AValue: RawByteString);
begin
  inherited Create;
  Value := AValue;
end;

function TPdfText.SpaceNotNeeded: boolean;
begin
  result := true;
end;


{ TPdfTextUtf8 }

constructor TPdfTextUtf8.Create(const AValue: RawUtf8);
begin
  inherited Create;
  Value := AValue;
end;

procedure TPdfTextUtf8.InternalWriteTo(W: TPdfWrite);
var
  utf16: RawByteString;
begin
  // if the value has multibyte character, convert the value to hex unicode.
  // otherwise, escape characters
  if IsWinAnsiU8Bit(pointer(fValue)) then
    W.Add('(').AddEscapeContent(Utf8ToWinAnsi(fValue)).Add(')')
  else
  begin
    utf16 := Utf8DecodeToUnicodeRawByteString(fValue);
    W.Add('<FEFF').AddUnicodeHex(pointer(utf16), Length(utf16) shr 1).Add('>');
  end;
end;

function TPdfTextUtf8.SpaceNotNeeded: boolean;
begin
  result := true;
end;


{ TPdfTextString }

constructor TPdfTextString.Create(const AValue: string);
begin
  inherited Create(StringToUtf8(AValue));
end;

function TPdfTextString.GetValue: string;
begin
  result := Utf8ToString(fValue);
end;

procedure TPdfTextString.SetValue(const Value: string);
begin
  fValue := StringToUtf8(Value);
end;


{ TPdfRawText }

procedure TPdfRawText.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(fValue);
end;

function TPdfRawText.SpaceNotNeeded: boolean;
begin
  result := false;
end;


{ TPdfClearText }

constructor TPdfClearText.Create(Buffer: pointer; Len: integer);
var
  tmp: RawByteString;
begin
  FastSetRawByteString(tmp, Buffer, Len);
  inherited Create(tmp);
end;

procedure TPdfClearText.InternalWriteTo(W: TPdfWrite);
begin
  W.Add('(').AddEscape(pointer(fValue), Length(fValue)).Add(')');
end;


{ TPdfName }

procedure TPdfName.InternalWriteTo(W: TPdfWrite);
begin
  W.Add('/').AddEscapeName(pointer(fValue));
end;

function TPdfName.AppendPrefix: RawUtf8;
var
  prefix: RawUtf8;
  c: cardinal;
  i: PtrInt;
begin
  if self = nil then
    exit;
  SetLength(prefix, 7);
  c := Random32; // we will consume only 24-bit of randomness
  for i := 1 to 6 do
  begin
    prefix[i] := AnsiChar((c and 15) + 65);
    c := c shr 4;
  end;
  prefix[7] := '+';
  fValue := prefix + fValue; // we ensured a single subset per font
  result := fValue;
end;


{ TPdfArray }

function TPdfArray.GetItems(Index: integer): TPdfObject;
begin
  result := TPdfObject(fArray[Index]);
  if result.ObjectType = otVirtualObject then
    if fObjectMgr <> nil then
      result := fObjectMgr.GetObject(result.ObjectNumber)
    else
      result := nil;
end;

function TPdfArray.GetItemCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fArray.Count;
end;

procedure TPdfArray.InternalWriteTo(W: TPdfWrite);
var
  i: PtrInt;
begin
  inherited;
  W.Add('[');
  for i := 0 to fArray.Count - 1 do
    with TPdfObject(fArray.List[i]) do
    begin
      if (i <> 0) and
         not SpaceNotNeeded then
        W.Add(' ');
      WriteTo(W);
    end;
  W.Add(']');
end;

constructor TPdfArray.Create(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  fArray := TSynList.Create;
  fObjectMgr := AObjectMgr;
end;

constructor TPdfArray.Create(AObjectMgr: TPdfObjectMgr;
  const AArray: array of integer);
var
  i: PtrInt;
begin
  Create(AObjectMgr);
  for i := 0 to High(AArray) do
    AddItem(TPdfNumber.Create(AArray[i]));
end;

constructor TPdfArray.Create(AObjectMgr: TPdfObjectMgr; AArray: PWordArray;
  AArrayCount: integer);
var
  i: PtrInt;
begin
  Create(AObjectMgr);
  for i := 0 to AArrayCount - 1 do
    AddItem(TPdfNumber.Create(AArray^[i]));
end;

constructor TPdfArray.CreateNames(AObjectMgr: TPdfObjectMgr;
  const AArray: array of PdfString);
var
  i: PtrInt;
begin
  Create(AObjectMgr);
  for i := 0 to high(AArray) do
    AddItem(TPdfName.Create(AArray[i]));
end;

constructor TPdfArray.CreateReals(AObjectMgr: TPdfObjectMgr;
  const AArray: array of double);
var
  i: PtrInt;
begin
  Create(AObjectMgr);
  for i := 0 to high(AArray) do
    AddItem(TPdfReal.Create(AArray[i]));
end;

destructor TPdfArray.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to fArray.Count - 1 do
    TPdfObject(fArray.List[i]).Free;
  fArray.Free;
  inherited;
end;

function TPdfArray.AddItem(AItem: TPdfObject): integer;
begin
  result := fArray.IndexOf(AItem);
  if result >= 0 then
    exit; // if AItem already exists, do nothing
  if AItem.ObjectType = otDirectObject then
    result := fArray.Add(AItem)
  else
    result := fArray.Add(TPdfVirtualObject.Create(AItem.ObjectNumber))
end;

procedure TPdfArray.InsertItem(Index: integer; AItem: TPdfObject);
begin
  if fArray.IndexOf(AItem) >= 0 then
    exit; // if AItem already exists, do nothing
  if AItem.ObjectType = otDirectObject then
    fArray.Insert(AItem, Index)
  else
    fArray.Insert(TPdfVirtualObject.Create(AItem.ObjectNumber), Index)
end;

function TPdfArray.FindName(const AName: PdfString): TPdfName;
var
  i: PtrInt;
begin
  for i := 0 to ItemCount - 1 do
  begin
    result := fArray.List[i];
    if (result <> nil) and
       result.InheritsFrom(TPdfName) and
       (result.Value = AName) then
      exit;
  end;
  result := nil;
end;

function TPdfArray.RemoveName(const AName: PdfString): boolean;
var
  o: TPdfObject;
begin
  result := false;
  o := FindName(AName);
  if o <> nil then
  begin
    fArray.Remove(o);
    if o.ObjectType = otDirectObject then
      o.Free;
    result := true;
  end;
end;

function TPdfArray.SpaceNotNeeded: boolean;
begin
  result := true;
end;


{ TPdfDictionaryElement }

function TPdfDictionaryElement.GetKey: PdfString;
begin
  if self = nil then
    result := ''
  else
    result := fKey.Value;
end;

constructor TPdfDictionaryElement.Create(const AKey: PdfString;
  AValue: TPdfObject; AInternal: boolean);
begin
  if not (AValue is TPdfObject) then
    EPdfInvalidValue.RaiseUtf8('TPdfDictionaryElement(%,%)', [AKey, AValue]);
  fKey := TPdfName.Create(AKey);
  fValue := AValue;
  fIsInternal := AInternal;
end;

destructor TPdfDictionaryElement.Destroy;
begin
  fKey.Free;
  fValue.Free;
  inherited;
end;


{ TPdfDictionary }

function TPdfDictionary.GetItems(Index: integer): TPdfDictionaryElement;
begin
  result := TPdfDictionaryElement(fArray[Index]);
end;

function TPdfDictionary.GetItemCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fArray.Count;
end;

procedure TPdfDictionary.DirectWriteto(W: TPdfWrite; Secondary: TPdfDictionary);

  procedure WriteArray(aArray: TSynList);
  var
    i: PtrInt;
    e: TPdfDictionaryElement;
  begin
    for i := 0 to aArray.Count - 1 do
    begin
      e := aArray.List[i];
      if not e.IsInternal then
      begin
        e.fKey.WriteTo(W);
        if not e.fValue.SpaceNotNeeded then
          W.Add(' ');
        e.fValue.WriteTo(W);
      end;
    end;
  end;

begin
  inherited InternalWriteTo(W);
  W.Add('<<');
  WriteArray(fArray);
  if Secondary <> nil then
    WriteArray(Secondary.fArray);
  W.Add('>>');
end;

procedure TPdfDictionary.InternalWriteTo(W: TPdfWrite);
begin
  DirectWriteto(W, nil);
end;

constructor TPdfDictionary.Create(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  fArray := TSynList.Create;
  fObjectMgr := AObjectMgr;
end;

destructor TPdfDictionary.Destroy;
var
  i: integer;
begin
  for i := 0 to fArray.Count - 1 do
    TPdfDictionaryElement(fArray[i]).Free;
  fArray.Free;
  inherited;
end;

function TPdfDictionary.ValueByName(const AKey: PdfString): TPdfObject;
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to fArray.Count - 1 do
      with TPdfDictionaryElement(fArray.List[i]) do
        if fKey.Value = AKey then
        begin
          result := Value;
          if result.ObjectType = otVirtualObject then
            if fObjectMgr <> nil then
              result := fObjectMgr.GetObject(result.ObjectNumber)
            else
              result := nil;
          exit;
        end;
  result := nil;
end;

function TPdfDictionary.PdfNumberByName(const AKey: PdfString): TPdfNumber;
begin
  result := TPdfNumber(ValueByName(AKey));
end;

function TPdfDictionary.PdfTextByName(const AKey: PdfString): TPdfText;
begin
  result := TPdfText(ValueByName(AKey));
end;

function TPdfDictionary.PdfTextValueByName(const AKey: PdfString): PdfString;
var
  P: TPdfText;
begin
  P := TPdfText(ValueByName(AKey));
  if P = nil then
    result := ''
  else
    result := P.Value;
end;

function TPdfDictionary.PdfTextStringValueByName(const AKey: PdfString): string;
var
  P: TPdfTextString;
begin
  P := TPdfTextString(ValueByName(AKey));
  if P = nil then
    result := ''
  else
    result := P.Value;
end;

function TPdfDictionary.PdfTextUtf8ValueByName(const AKey: PdfString): RawUtf8;
var
  P: TPdfTextUtf8;
begin
  P := TPdfTextUtf8(ValueByName(AKey));
  if P = nil then
    result := ''
  else
    result := P.Value;
end;

function TPdfDictionary.PdfRealByName(const AKey: PdfString): TPdfReal;
begin
  result := TPdfReal(ValueByName(AKey));
end;

function TPdfDictionary.PdfNameByName(const AKey: PdfString): TPdfName;
begin
  result := TPdfName(ValueByName(AKey));
end;

function TPdfDictionary.PdfDictionaryByName(const AKey: PdfString): TPdfDictionary;
begin
  result := TPdfDictionary(ValueByName(AKey));
end;

function TPdfDictionary.PdfArrayByName(const AKey: PdfString): TPdfArray;
begin
  result := TPdfArray(ValueByName(AKey));
end;

function TPdfDictionary.PdfBooleanByName(const AKey: PdfString): TPdfBoolean;
begin
  result := TPdfBoolean(ValueByName(AKey));
end;

procedure TPdfDictionary.AddItem(const AKey: PdfString; AValue: TPdfObject;
  AInternal: boolean);
var
  FItem: TPdfDictionaryElement;
begin
  if self = nil then
    exit;
  RemoveItem(AKey);
  if AValue.ObjectType = otDirectObject then
    FItem := TPdfDictionaryElement.Create(AKey, AValue, AInternal)
  else
    FItem := TPdfDictionaryElement.Create(
      AKey, TPdfVirtualObject.Create(AValue.ObjectNumber), AInternal);
  fArray.Add(FItem);
end;

procedure TPdfDictionary.AddItem(const AKey, AValue: PdfString);
begin
  AddItem(AKey, TPdfName.Create(AValue));
end;

procedure TPdfDictionary.AddItem(const AKey: PdfString; AValue: integer);
begin
  AddItem(AKey, TPdfNumber.Create(AValue));
end;

procedure TPdfDictionary.AddItemText(const AKey, AValue: PdfString);
begin
  AddItem(AKey, TPdfText.Create(AValue));
end;

procedure TPdfDictionary.AddItemTextUtf8(const AKey: PdfString; const AValue: RawUtf8);
begin
  AddItem(AKey, TPdfTextUtf8.Create(AValue));
end;

procedure TPdfDictionary.AddItemTextString(const AKey: PdfString; const AValue: string);
begin
  AddItem(AKey, TPdfTextString.Create(AValue));
end;

procedure TPdfDictionary.RemoveItem(const AKey: PdfString);
var
  i: PtrInt;
  e: TPdfDictionaryElement;
begin
  if Self <> nil then
    for i := 0 to fArray.Count - 1 do
    begin
      e := fArray.List[i];
      if e.fKey.Value = AKey then
      begin
        fArray.Remove(e);
        e.Free;
        Break;
      end;
    end;
end;

function TPdfDictionary.GetTypeOf: PdfString;
var
  n: TPdfName;
begin
  n := PdfNameByName('Type');
  if n <> nil then
    result := n.Value
  else
    result := '';
end;

function TPdfDictionary.SpaceNotNeeded: boolean;
begin
  result := true;
end;


{ TPdfStream }

procedure TPdfStream.InternalWriteTo(W: TPdfWrite);
var
  len: TPdfNumber;
  buflen: integer;
  tmp: TSynTempBuffer;
  buf: pointer;
begin
  inherited;
  len := fAttributes.PdfNumberByName('Length');
  fWriter.Save; // flush fWriter content
  buf := TMemoryStream(fWriter.fDestStream).Memory;
  buflen := fWriter.Position;
  tmp.buf := nil;
  try
    if fFilter = 'FlateDecode' then
      if buflen < 100 then // don't compress tiny blocks
        fFilter := ''
      else
      begin
        tmp.Init(zlibCompressMax(buflen));
        buflen := CompressMem(buf, tmp.buf, buflen, tmp.len, 7, true);
        buf := tmp.buf;
      end;
    len.Value := buflen;
    if fFilter <> '' then
      fAttributes.AddItem('Filter', fFilter);
    fAttributes.DirectWriteTo(W, fSecondaryAttributes);
    {$ifdef USE_PDFSECURITY}
    if (buflen > 0) and
       (W.fDoc.fEncryption <> nil) and
       not fDoNotEncrypt then
      W.fDoc.fEncryption.EncodeBuffer(buf^, buf^, buflen);
    {$endif USE_PDFSECURITY}
    W.Add(#10'stream'#10).Add(buf, buflen).Add(#10'endstream');
    fWriter.fDestStream.Size := 0; // release internal stream memory
  finally
    tmp.Done;
  end;
end;

constructor TPdfStream.Create(ADoc: TPdfDocument; DontAddToFXref: boolean);
var
  x: TPdfXRef;
begin
  inherited Create;
  if DontAddToFXref then
    x := nil
  else
  begin
    x := ADoc.fXRef;
    x.AddObject(self);
  end;
  fAttributes := TPdfDictionary.Create(x);
  fAttributes.AddItem('Length', TPdfNumber.Create(0));
  if ADoc.CompressionMethod = cmFlateDecode then
    fFilter := 'FlateDecode';
  fWriter := TPdfWrite.Create(ADoc, TMemoryStream.Create);
end;

destructor TPdfStream.Destroy;
begin
  fWriter.fDestStream.Free;
  fWriter.Free;
  fAttributes.Free;
  inherited;
end;


{ TPdfBinary }

procedure TPdfBinary.InternalWriteTo(W: TPdfWrite);
begin
  inherited;
  W.Add(Stream.Memory, fStream.Size);
end;

constructor TPdfBinary.Create;
begin
  inherited;
  fStream := TMemoryStream.Create;
end;

destructor TPdfBinary.Destroy;
begin
  fStream.Free;
  inherited;
end;


{ TPdfObjectStream }

function TPdfObjectStream.AddObject(Value: TPdfObject): PtrInt;
begin
  result := fObjectCount;
  inc(fObjectCount);
  if fObjectCount > length(fObject) then
    SetLength(fObject, fObjectCount + 15);
  fObject[result].Number := Value.ObjectNumber;
  fObject[result].Position := fAddingStream.Position;
  Value.InternalWriteTo(fAddingStream);
end;

constructor TPdfObjectStream.Create(aDoc: TPdfDocument);
begin
  inherited Create(aDoc, false);
  Attributes.AddItem('Type', 'ObjStm');
  fAddingStream := TPdfWrite.Create(aDoc, TMemoryStream.Create);
end;

destructor TPdfObjectStream.Destroy;
begin
  fAddingStream.fDestStream.Free;
  fAddingStream.Free;
  inherited;
end;

procedure TPdfObjectStream.InternalWriteTo(W: TPdfWrite);
var
  i: PtrInt;
begin
  Attributes.AddItem('N', fObjectCount);
  for i := 0 to fObjectCount - 1 do
    with fObject[i] do
      Writer.Add(Number).Add(' ').Add(Position).Add(' ');
  Attributes.AddItem('First', Writer.Position);
  Writer.Add(fAddingStream.ToPdfString);
  inherited;
end;


{ TPdfWrite }

function TPdfWrite.Add(c: AnsiChar): TPdfWrite;
begin
  if B >= BEnd then // avoid GPF
    Save;
  B^ := c;
  inc(B);
  result := self;
end;

function TPdfWrite.Add(Value: integer): TPdfWrite;
var
  t: TTemp24;
  P: PAnsiChar;
begin
  if BEnd - B <= 24 then
    Save;
  {$ifndef ASMINTEL} // our StrInt32 asm has less CPU cache pollution
  if cardinal(Value) < 1000 then
  {$endif ASMINTEL}
    if cardinal(Value) < 10 then
    begin
      B^ := AnsiChar(Value + 48);
      inc(B);
    end
    else if cardinal(Value) < 100 then
    begin
      PWord(B)^ := TwoDigitLookupW[Value];
      inc(B, 2);
    end
    {$ifndef ASMINTEL}
    else
    begin
      PCardinal(B)^ := PCardinal(SmallUInt32Utf8[Value])^;
      inc(B, 3);
    end
    {$endif ASMINTEL}
  else
  begin
    P := StrInt32(@t[23], Value);
    MoveFast(P^, B^, @t[23] - P);
    inc(B, @t[23] - P);
  end;
  result := self;
end;

function TPdfWrite.AddS(const Text: string): TPdfWrite;
begin
  Add(StringToUtf8(Text));
  result := self;
end;

function TPdfWrite.Add(const Text: RawByteString): TPdfWrite;
begin
  result := Add(pointer(Text), length(Text));
end;

function TPdfWrite.Add(Text: PAnsiChar; Len: PtrInt): TPdfWrite;
begin
  if BEnd - B <= Len then
  begin
    Save;
    inc(fDestStreamPosition, Len);
    fDestStream.WriteBuffer(Text^, Len);
  end
  else
  begin
    MoveFast(Text^, B^, Len);
    inc(B, Len);
  end;
  result := self;
end;

function TPdfWrite.Add(Value, DigitCount: integer): TPdfWrite;
var
  t: array[0..15] of AnsiChar;
  i64: array[0..1] of Int64 absolute t;
begin
//  assert(DigitCount<high(t));
  if BEnd - B <= 16 then
    Save;
  i64[0] := $3030303030303030; // t[0..14]='0'
  i64[1] := $2030303030303030; // t[15]=' '
  if Value < 0 then
    Value := 0;
  StrUInt32(@t[15], Value);
  inc(DigitCount); // append trailing t[15]=' '
  MoveFast(t[16 - DigitCount], B^, DigitCount);
  inc(B, DigitCount);
  result := self;
end;

function TPdfWrite.Add(Value: double): TPdfWrite;
var
  Buffer: ShortString;
  L: PtrInt;
begin
  if BEnd - B <= 32 then
    Save;
  {$ifdef DOUBLETOSHORT_USEGRISU}
  DoubleToAscii(0, 2, Value, @Buffer);
  {$else}
  Str(Value:0:2, Buffer);
  {$endif DOUBLETOSHORT_USEGRISU}
  L := ord(Buffer[0]);
  if Buffer[L] = '0' then
    if Buffer[L - 1] = '0' then // '3.00' -> '3'
      dec(L, 3)
    else
      dec(L); // '3.40' -> '3.4'
  MoveFast(Buffer[1], B^, L);
  inc(B, L);
  result := self;
end;

function TPdfWrite.AddColorStr(Color: TPdfColorRGB): TPdfWrite;
var
  X: array[0..3] of byte absolute Color;
begin
  if integer(Color) < 0 then
    Color := GetSysColor(Color and $ff);
  result := AddWithSpace(X[0] / 255).AddWithSpace(X[1] / 255).AddWithSpace(X[2] / 255);
end;

function TPdfWrite.AddEscapeContent(const Text: RawByteString): TPdfWrite;
{$ifdef USE_PDFSECURITY}
var
  tmp: TSynTempBuffer;
{$endif USE_PDFSECURITY}
begin
  if Text <> '' then
    {$ifdef USE_PDFSECURITY}
    if fDoc.fEncryption <> nil then
    begin
      tmp.Init(length(Text));
      fDoc.fEncryption.EncodeBuffer(pointer(Text)^, tmp.buf^, tmp.len);
      AddEscape(tmp.buf, tmp.len);
      tmp.Done;
    end
    else
    {$endif USE_PDFSECURITY}
      AddEscape(pointer(Text), length(Text));
  result := self;
end;

function TPdfWrite.AddEscape(Text: PAnsiChar; TextLen: integer): TPdfWrite;
var
  textend: PAnsiChar;
begin
  textend := Text + TextLen;
  while Text < textend do
  begin
    if B >= BEnd4 then
      Save;
    case Text^ of
      '(', ')', '\':
        PWord(B)^ := ord('\') + ord(Text^) shl 8;
      #0:
        begin
          PInteger(B)^ := ord('\') + ord('0') shl 8 + ord('0') shl 16 + ord('0') shl 24;
          inc(B, 2);
        end;
      #8:
        PWord(B)^ := ord('\') + ord('B') shl 8;
      #9:
        PWord(B)^ := ord('\') + ord('t') shl 8;
      #10:
        PWord(B)^ := ord('\') + ord('n') shl 8;
      #12:
        PWord(B)^ := ord('\') + ord('f') shl 8;
      #13:
        PWord(B)^ := ord('\') + ord('r') shl 8;
    else
      begin
        B^ := Text^;
        inc(B);
        inc(Text);
        continue;
      end;
    end;
    inc(B, 2);
    inc(Text);
  end;
  result := self;
end;

const // should be local for better code generation
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';
  ESCAPENAME: TSynAnsicharSet = [
    #1..#31, '%', '(', ')', '<', '>', '[', ']', '{', '}', '/', '#', #127..#255];

function TPdfWrite.AddEscapeName(Text: PAnsiChar): TPdfWrite;
var
  c: cardinal;
begin
  if Text <> nil then
    repeat
      if B >= BEnd4 then
        Save;
      c := ord(Text^);
      if c = 0 then
        break
      else if AnsiChar(c) in ESCAPENAME then
      begin
        B[0] := '#';
        B[1] := HexChars[c shr 4];
        B[2] := HexChars[c and $F];
        inc(B, 3);
        inc(Text);
      end
      else
      begin
        B^ := AnsiChar(c);
        inc(B);
        inc(Text);
      end;
    until false;
  result := self;
end;

function TPdfWrite.AddEscapeText(Text: PAnsiChar; Font: TPdfFont): TPdfWrite;
begin // this function is intented to use with Tj or '
  if Text <> nil then
    repeat
      if B >= BEnd4 then
        Save;
      if Font <> nil then
        include(Font.fWinAnsiUsed, Text^);
      case Text^ of
        #0:
          Break;
        #160:
          begin        // fixed space is written as normal space
            B^ := ' ';
            inc(B);
            inc(Text);
          end;
        #40, #41, #92:
          begin // see PDF 2nd ed. p. 290
            B[1] := Text^;
            B[0] := '\';
            inc(B, 2);
            inc(Text);
          end;
      else
        begin
          B^ := Text^;
          inc(B);
          inc(Text);
        end;
      end;
    until false;
  result := self;
end;

function TPdfWrite.AddHex(const Bin: PdfString): TPdfWrite;
var
  L, len: PtrInt;
  PW: pointer;
begin
  len := length(Bin);
  PW := pointer(Bin);
  repeat
    L := len;
    if PtrInt(BEnd - B) <= L * 2 then // note: PtrInt(BEnd - B) could be < 0
    begin
      Save;
      if L > high(fTmp) shr 1 then
        L := high(fTmp) shr 1;
    end;
    mormot.core.text.BinToHex(PW, B, L);
    inc(PtrInt(PW), L);
    inc(B, L * 2);
    dec(len, L);
  until len = 0;
  result := self;
end;

function TPdfWrite.AddHex4(aWordValue: cardinal): TPdfWrite;
var
  v: cardinal;
begin
  if B >= BEnd4 then
    Save;
  v := aWordValue shr 8;
  aWordValue := aWordValue and $ff;
  B[0] := HexChars[v shr 4];            // MSB stored first (BigEndian)
  B[1] := HexChars[v and $F];
  B[2] := HexChars[aWordValue shr 4];   // LSB stored last (BigEndian)
  B[3] := HexChars[aWordValue and $F];
  inc(B, 4);
  result := self;
end;

procedure TPdfWrite.AddRGB(P: PAnsiChar; PInc, Count: integer);
begin
  while Count > 0 do
  begin
    dec(Count);
    if B >= BEnd4 then
      Save;
    B[0] := P[2];  // write the RGB value in expected order
    B[1] := P[1];
    B[2] := P[0];
    inc(B, 3);
    inc(P, PInc);
  end;
end;

function TPdfWrite.AddIso8601(DateTime: TDateTime): TPdfWrite;
begin // add e.g. '2010-06-16T15:06:59'
  result := Add(DateTimeToIso8601(DateTime, true, 'T'));
end;

function TPdfWrite.AddWithSpace(Value: double): TPdfWrite;
var
  tmp: ShortString;
  L: integer;
begin
  if BEnd - B <= 32 then
    Save;
  // Value := Trunc(Value * 100 + 0.5) / 100; // 2 decim rounding done by str()
  if Abs(Value) < 1E-2 then
    Add('0 ')
  else
  begin
    {$ifdef DOUBLETOSHORT_USEGRISU}
    DoubleToAscii(0, 2, Value, @tmp);
    {$else}
    Str(Value:0:2, tmp);
    {$endif DOUBLETOSHORT_USEGRISU}
    L := ord(tmp[0]);
    if tmp[L] = '0' then
      if tmp[L - 1] = '0' then // '3.00' -> '3 '
        dec(L, 2)
      else // '3.40' -> '3.4 '
    else
      inc(L);  // '3.45' -> '3.45 '
    tmp[L] := ' '; // append space at the end
    MoveFast(tmp[1], B^, L);
    inc(B, L);
  end;
  result := self;
end;

function TPdfWrite.AddIntegerBin(value: integer; bytesize: cardinal): TPdfWrite;
var
  i: cardinal;
begin
  if BEnd - B <= 4 then
    Save;
  for i := 1 to bytesize do
    B[i - 1] := PAnsiChar(@value)[bytesize - i];
  inc(B, bytesize);
  result := self;
end;

function TPdfWrite.AddWithSpace(Value: double; Decimals: cardinal): TPdfWrite;
var
  tmp: ShortString;
  L: integer;
begin
  if BEnd - B <= 32 then
    Save;
  {$ifdef DOUBLETOSHORT_USEGRISU}
  DoubleToAscii(0, Decimals, Value, @tmp);
  {$else}
  Str(Value:0:Decimals, tmp);
  {$endif DOUBLETOSHORT_USEGRISU}
  L := ord(tmp[0]) + 1;
  tmp[L] := ' '; // append space at the end
  MoveFast(tmp[1], B^, L);
  inc(B, L);
  result := self;
end;

procedure TPdfWrite.ToWideChar(const Ansi: PdfString; out Dest: TSynTempBuffer);
begin
  Dest.Init(Length(Ansi) * 2 + 2); // maximum possible length
  Dest.len := fDoc.Engine.AnsiBufferToUnicode(
    Dest.buf, pointer(Ansi), Length(Ansi)) - Dest.buf;
end;

function TPdfWrite.AddToUnicodeHex(const Text: PdfString): TPdfWrite;
var
  tmp: TSynTempBuffer;
begin
  ToWideChar(Text, tmp);
  AddUnicodeHex(tmp.buf, tmp.len);
  tmp.Done;
  result := self;
end;

function TPdfWrite.AddToUnicodeHexText(const Text: PdfString; NextLine: boolean;
  Canvas: TPdfCanvas): TPdfWrite;
var
  tmp: TSynTempBuffer;
begin
  ToWideChar(Text, tmp);
  AddUnicodeHexText(tmp.buf, tmp.len, NextLine, Canvas);
  tmp.Done;
  result := self;
end;

function TPdfWrite.AddUnicodeHex(PW: PWideChar; WideCharCount: integer): TPdfWrite;

  procedure BinToHex4(Bin, Hex: PAnsiChar; BinWords: integer); // BigEndian order
  var
    j, v: cardinal;
  begin
    for j := 1 to BinWords do
    begin
      v := byte(Bin^);
      inc(Bin);
      Hex[2] := HexChars[v shr 4];   // LSB stored last (BigEndian)
      Hex[3] := HexChars[v and $F];
      v := byte(Bin^);
      inc(Bin);
      Hex[0] := HexChars[v shr 4];   // MSB stored first (BigEndian)
      Hex[1] := HexChars[v and $F];
      inc(Hex, 4);
    end;
  end;

var
  L: PtrInt;
  {$ifdef USE_PDFSECURITY}
  sectmp: TSynTempBuffer;
  {$endif USE_PDFSECURITY}
begin
  if WideCharCount > 0 then
  begin
    {$ifdef USE_PDFSECURITY}
    if fDoc.fEncryption <> nil then
    begin
      sectmp.Init(WideCharCount * 2);
      fDoc.fEncryption.EncodeBuffer(PW^, sectmp.buf^, WideCharCount * 2);
      PW := sectmp.buf;
    end;
    {$endif USE_PDFSECURITY}
    repeat
      L := WideCharCount;
      if BEnd - B <= L * 4 then
      begin
        Save;
        if L > high(fTmp) shr 2 then
          L := high(fTmp) shr 2; // max WideCharCount allowed in Tmp[]
      end;
      BinToHex4(pointer(PW), B, L);
      inc(PtrInt(PW), L * 2);
      inc(B, L * 4);
      dec(WideCharCount, L);
    until WideCharCount = 0;
    {$ifdef USE_PDFSECURITY}
    if fDoc.fEncryption <> nil then
      sectmp.Done;
    {$endif USE_PDFSECURITY}
  end;
  result := self;
end;

const
  SHOWTEXTCMD: array[boolean] of PdfString = (' Tj'#10, ' '''#10);

{$ifdef USE_UNISCRIBE}

function TPdfWrite.AddUnicodeHexTextUniScribe(PW: PWideChar; PWLen: integer;
  WinAnsiTtf: TPdfFontTrueType; NextLine: boolean; Canvas: TPdfCanvas): boolean;
// see http://msdn.microsoft.com/en-us/library/dd317792(v=VS.85).aspx
var
  L, i, j: integer;
  res: HRESULT;
  max, count, numSp: integer;
  Sp: PScriptPropertiesArray;
  items: array of TScriptItem;
  level: array of byte;
  VisualToLogical: array of integer;
  psc: pointer; // opaque Uniscribe font metric cache
  complex, R2L: boolean;
  glyphs: array of TScriptVisAttr;
  glyphsCount: integer;
  OutGlyphs, LogClust: array of word;
  AScriptControl: TScriptControl;
  AScriptState: TScriptState;

  procedure Append(i: integer);
  // local procedure used to add glyphs from items[i] to the PDF content stream
  var
    L: integer;
    W: PWideChar;

    procedure DefaultAppend;
    var
      tmp: TSynTempBuffer;
    begin
      tmp.Init(W, L * 2);
      PWordArray(tmp.buf)[L] := 0; // we need the text to be ending with #0
      AddUnicodeHexTextNoUniScribe(tmp.buf, WinAnsiTtf, false, Canvas);
      tmp.Done;
    end;

  begin
    L := items[i + 1].iCharPos - items[i].iCharPos; // length of this shapeable item
    if L = 0 then
      exit; // nothing to append
    W := PW + items[i].iCharPos;
    res := ScriptShape(0, psc, W, L, max, @items[i].a, pointer(OutGlyphs),
      pointer(LogClust), pointer(glyphs), glyphsCount);
    case res of
      E_OUTOFMEMORY:
        begin // max was not big enough (should never happen)
          DefaultAppend;
          exit;
        end;
      E_PENDING,
      USP_E_SCRIPT_NOT_IN_FONT:
        begin // need HDC and a selected font object
          res := ScriptShape(Canvas.fDoc.GetDCWithFont(WinAnsiTtf), psc, W, L,
            max, @items[i].a, pointer(OutGlyphs), pointer(LogClust),
            pointer(glyphs), glyphsCount);
          if res <> 0 then
          begin // we won't change font if necessary, sorry
            // we shall implement the complex technic as stated by
            // http://msdn.microsoft.com/en-us/library/dd374105(v=VS.85).aspx
            DefaultAppend;
            exit;
          end;
        end;
      0:
        ; // success -> will add glyphs just below
    else
      exit;
    end;
    // add glyphs to the PDF content
    // (NextLine has already been handled: not needed here)
    AddGlyphs(pointer(OutGlyphs), glyphsCount, Canvas, pointer(glyphs));
  end;

begin
  result := false; // on UniScribe error, handle as Unicode
  // 1. Breaks a Unicode string into individually shapeable items
  L := PWLen + 1; // include last #0
  max := L + 2; // should be big enough
  SetLength(items, max);
  count := 0;
  FillCharFast(AScriptControl, SizeOf(TScriptControl), 0);
  FillCharFast(AScriptState, SizeOf(TScriptState), 0);
  if ScriptApplyDigitSubstitution(nil, @AScriptControl, @AScriptState) <> 0 then
    exit;
  if Canvas.RightToLeftText then
    AScriptState.uBidiLevel := 1;
  if ScriptItemize(PW, L, max, @AScriptControl, @AScriptState,
       pointer(items), count) <> 0 then
    exit; // error trying processing Glyph Shaping -> fast return
  // 2. guess if requiring glyph shaping or layout
  ScriptGetProperties(Sp, numSp);
  complex := false;
  R2L := false;
  for i := 0 to count - 2 do // don't need Count-1 = Terminator
    if fComplex in Sp^[items[i].a.eScript and (1 shl 10 - 1)]^.fFlags then
      complex := true
    else if fRtl in items[i].a.fFlags then
      R2L := true;
  if not complex and
     not R2L then
    exit; // avoid slower UniScribe if content does not require it
  // 3. get Visual Order, i.e. how to render the content from left to right
  SetLength(level, count);
  for i := 0 to count - 1 do
    level[i] := items[i].a.s.uBidiLevel;
  SetLength(VisualToLogical, count);
  if ScriptLayout(count, pointer(level), pointer(VisualToLogical), nil) <> 0 then
    exit;
  // 4. now we have enough information to start drawing
  result := true;
  if NextLine then
    Canvas.MoveToNextLine; // manual NextLine handling
  // 5. add glyphs for all shapeable items
  max := (L * 3) shr 1 + 32; // should be big enough - allocate only once
  SetLength(glyphs, max);
  SetLength(OutGlyphs, max);
  SetLength(LogClust, max);
  psc := nil; // cached for the same character style used
  // append following logical order
  for j := 0 to count - 2 do // Count-2: ignore last ending item
    Append(VisualToLogical[j]);
end;

{$endif USE_UNISCRIBE}

procedure TPdfWrite.AddGlyphFromChar(Char: WideChar; Canvas: TPdfCanvas;
  Ttf: TPdfFontTrueType; NextLine: PBoolean);
var
  changed: boolean;
  fnt: TPdfFontTrueType;
  Glyph: word;
begin
  assert((Ttf <> nil) and not Ttf.Unicode);
  changed := fAddGlyphFont = fNone;
  Glyph := Ttf.fUsedWide[Ttf.FindOrAddUsedWideChar(Char)].Glyph;
  with Canvas.fDoc do
    if (fPdfA <> pdfaNone) and
       (Glyph = 0) and
       (fFontFallBackIndex < 0) then
      raise Exception.Create('PDF/A expects font fallback to be enabled, ' +
        'and the required font is not available on this system')
    else if (Glyph = 0) and
            fUseFontFallBack and
            (fFontFallBackIndex >= 0) then
    begin
      if fAddGlyphFont = fMain then
        AddGlyphFlush(Canvas, Ttf, NextLine);
      fAddGlyphFont := fFallBack;
      fnt := Canvas.SetFont('', Canvas.fPage.FontSize, Ttf.fStyle, -1,
        fFontFallBackIndex) as TPdfFontTrueType;
      assert(not fnt.Unicode);
      Glyph := fnt.fUsedWide[fnt.FindOrAddUsedWideChar(Char)].Glyph;
    end
    else
    begin
      if fAddGlyphFont = fFallBack then
      begin
        AddGlyphFlush(Canvas, Ttf, NextLine);
        changed := true;
      end;
      fAddGlyphFont := fMain;
      fnt := Ttf;
    end;
  if (Canvas.fPage.Font <> fnt.UnicodeFont) and
     (fnt.UnicodeFont = nil) then
    fnt.CreateAssociatedUnicodeFont;
  Canvas.SetPdfFont(fnt.UnicodeFont, Canvas.fPage.FontSize);
  if changed then
    Add('<');
  AddHex4(Glyph);
end;

procedure TPdfWrite.AddGlyphFlush(Canvas: TPdfCanvas; Ttf: TPdfFontTrueType;
  NextLine: PBoolean);
var
  nxtlin: boolean;
begin
  if fAddGlyphFont = fNone then
    exit;
  if NextLine = nil then
    nxtlin := false
  else
  begin
    nxtlin := NextLine^;
    NextLine^ := false;  // MoveToNextLine only once
  end;
  fAddGlyphFont := fNone;
  Add('>').Add(SHOWTEXTCMD[nxtlin]);
end;

procedure TPdfWrite.AddUnicodeHexTextNoUniScribe(PW: PWideChar;
  Ttf: TPdfFontTrueType; NextLine: boolean; Canvas: TPdfCanvas);
var
  ansi: integer;
  symbolfont: boolean;
begin
  if Ttf <> nil then
  begin
    if Ttf.UnicodeFont <> nil then
      symbolfont := Ttf.UnicodeFont.fIsSymbolFont
    else
      symbolfont := Ttf.fIsSymbolFont;
    Ttf := Ttf.WinAnsiFont; // we expect the WinAnsi font in the code below
  end
  else
    symbolfont := false;
  ansi := WideCharToWinAnsi(cardinal(PW^));
  if (Ttf = nil) and
     (ansi < 0) then
    ansi := ord('?'); // WinAnsi only font shows ? glyph for unicode chars
  while ansi <> 0 do
  begin
    if (ansi > 0) and
       not symbolfont then
    begin
      // add WinAnsi-encoded chars as such
      if (Ttf <> nil) and
         (Canvas.fPage.Font <> Ttf) then
        Canvas.SetPdfFont(Ttf, Canvas.fPage.FontSize);
      Add('(');
      repeat
        case ansi of
          40, 41, 92:
            Add('\');   // see PDF 2nd ed. p. 290
          160:
            ansi := 32; // fixed space is written as normal space
        end;
        Ttf.AddUsedWinAnsiChar(AnsiChar(ansi));
        Add(AnsiChar(ansi));
        inc(PW);
        ansi := WideCharToWinAnsi(cardinal(PW^));
        if (Ttf = nil) and
           (ansi < 0) then
          ansi := ord('?'); // WinAnsi only font shows ? glyph for unicode chars
      until ansi <= 0;
      Add(')').Add(SHOWTEXTCMD[NextLine]);
      NextLine := false; // MoveToNextLine only once
    end;
    if ansi = 0 then
      break;
    // here we know that PW^ is not a Win-ansi glyph, and that Ttf exists
    repeat
      AddGlyphFromChar(PW^, Canvas, Ttf, @NextLine);
      inc(PW);
      ansi := WideCharToWinAnsi(cardinal(PW^));
      if ansi = 160 then
        ansi := 32;
      if ansi = 32 then
        if WideCharToWinAnsi(cardinal(PW[1])) < 0 then
          continue; // we allow one space inside Unicode text
    until ansi >= 0;
    AddGlyphFlush(Canvas, Ttf, @NextLine);
  end;
end;

function TPdfWrite.AddUnicodeHexText(PW: PWideChar; PWLen: integer; NextLine: boolean;
  Canvas: TPdfCanvas): TPdfWrite;
var
  ttf: TPdfFontTrueType;
begin
  if PW <> nil then
  begin
    with Canvas.fPage do
      if fFont.FTrueTypeFontsIndex = 0 then
        ttf := nil
      else // mark we don't have an Unicode font, i.e. a ttf
        ttf := TPdfFontTrueType(fFont);
    {$ifdef USE_UNISCRIBE}
    // use the Windows Uniscribe API if required
    if not Canvas.fDoc.UseUniScribe or
       (ttf = nil) or
       not AddUnicodeHexTextUniScribe(PW, PWLen, ttf.WinAnsiFont, NextLine, Canvas) then
    {$endif USE_UNISCRIBE}
      // fastest version, without Ordering and/or Shaping of the text
      AddUnicodeHexTextNoUniScribe(PW, ttf, NextLine, Canvas);
  end;
  result := self;
end;

function TPdfWrite.AddGlyphs(Glyphs: PWord; GlyphsCount: integer;
  Canvas: TPdfCanvas; AVisAttrsPtr: pointer): TPdfWrite;
var
  ttf: TPdfFontTrueType;
  first: boolean;
  glyph: integer;
  {$ifdef USE_UNISCRIBE}
  attr: PScriptVisAttr;
  {$endif USE_UNISCRIBE}
begin
  if (Glyphs <> nil) and
     (GlyphsCount > 0) then
  begin
    with Canvas.fPage do
      if fFont.FTrueTypeFontsIndex = 0 then
        ttf := nil
      else // mark we don't have an Unicode font, i.e. a ttf
        ttf := TPdfFontTrueType(fFont);
    if ttf <> nil then
    begin // we need a ttf font
      if (Canvas.fPage.Font <> ttf.UnicodeFont) and
         (ttf.UnicodeFont = nil) then
        ttf.CreateAssociatedUnicodeFont;
      Canvas.SetPdfFont(ttf.UnicodeFont, Canvas.fPage.FontSize);
      first := true;
      {$ifdef USE_UNISCRIBE}
      attr := AVisAttrsPtr;
      {$endif USE_UNISCRIBE}
      while GlyphsCount > 0 do
      begin
        {$ifdef USE_UNISCRIBE}
        if (attr = nil) or
           not (attr^.fFlags * [fDiacritic, fZeroWidth] = [fZeroWidth]) then
        {$endif USE_UNISCRIBE}
        begin
          glyph := ttf.WinAnsiFont.GetAndMarkGlyphAsUsed(Glyphs^);
          // this font shall by definition contain all needed glyphs
          // -> no Font Fallback is to be implemented here
          if first then
          begin
            first := false;
            Add('<');
          end;
          AddHex4(glyph);
        end;
        inc(Glyphs);
        dec(GlyphsCount);
        {$ifdef USE_UNISCRIBE}
        if attr <> nil then
          inc(attr);
        {$endif USE_UNISCRIBE}
      end;
      if not first then
        Add('> Tj'#10);
    end;
  end;
  result := self;
end;

function TPdfWrite.AddWithSpace(Value: integer): TPdfWrite;
var
  t: array[0..25] of AnsiChar;
  P: PAnsiChar;
  L: integer;
begin
  if BEnd - B <= 16 then
    Save;
  if cardinal(Value) < 1000 then
    if cardinal(Value) < 10 then
    begin
      PWord(B)^ := Value + (48 + 32 shl 8);
      inc(B, 2);
    end
    else if cardinal(Value) < 100 then
    begin
      PCardinal(B)^ := TwoDigitLookupW[Value] + 32 shl 16;
      inc(B, 3);
    end
    else
    begin
      PCardinal(B)^ := PCardinal(SmallUInt32Utf8[Value])^ + 32 shl 24;
      inc(B, 4);
    end
  else
  begin
    t[24] := ' ';
    P := StrInt32(@t[24], Value);
    L := @t[25] - P;
    MoveFast(P^, B^, L);
    inc(B, @t[25] - P);
  end;
  result := self;
end;

constructor TPdfWrite.Create(Destination: TPdfDocument; DestStream: TStream);
begin
  fDoc := Destination;
  fDestStream := DestStream;
  fDestStreamPosition := fDestStream.Seek(0, soCurrent);
  B := @fTmp;
  BEnd := B + high(fTmp);
  BEnd4 := BEnd - 4;
end;

function TPdfWrite.Position: integer;
begin
  result := fDestStreamPosition + PtrInt((PtrUInt(B) - PtrUInt(@fTmp)));
end;

procedure TPdfWrite.Save;
var
  L: integer;
begin
  L := PtrUInt(B) - PtrUInt(@fTmp);
  inc(fDestStreamPosition, L);
  fDestStream.WriteBuffer(fTmp, L);
  B := @fTmp;
end;

function TPdfWrite.ToPdfString: PdfString;
begin
  if fDestStreamPosition = 0 then
    // we remained in the internal buffer -> not necessary to use stream
    FastSetRawByteString(result, @fTmp, PtrUInt(B) - PtrUInt(@fTmp))
  else
  begin
    // we used the stream -> flush remaining, and get whole data at once
    Save;
    result := '';
    SetLength(result, fDestStreamPosition);
    fDestStream.Seek(0, soBeginning);
    fDestStream.Read(pointer(result)^, fDestStreamPosition);
  end;
end;


{ TPdfTrailer }

constructor TPdfTrailer.Create(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  fAttributes := TPdfDictionary.Create(AObjectMgr);
  fAttributes.AddItem('Size', TPdfNumber.Create(0));
end;

procedure TPdfTrailer.WriteTo(var W: TPdfWrite);
type
  TXRefType = (xrefFree, xrefInUse, xrefInUseCompressed);
const
  TYPEWIDTH = 1;
var
  offsetWidth, genWidth, i: integer;
  WR: TPdfWrite;
begin
  if fCrossReference = nil then
  begin
    W.Add('trailer' + CRLF);
    fAttributes.WriteTo(W);
  end
  else
  begin
    if fXrefAddress < 60000 then
      offsetWidth := 2
    else if fXrefAddress < 13421772 then
      offsetWidth := 3
    else
      offsetWidth := 4;
    if (fObjectStream = nil) or
       (fObjectStream.ObjectCount < 255) then
      genWidth := 1
    else if fObjectStream.ObjectCount < 65535 then
      genWidth := 2
    else
      genWidth := 3;
    fAttributes.AddItem('W',
      TPdfArray.Create(nil, [TYPEWIDTH, offsetWidth, genWidth]));
    WR := fCrossReference.Writer;
    WR.AddIntegerBin(ord(xrefFree), TYPEWIDTH).
       AddIntegerBin(0, offsetWidth).
       AddIntegerBin(-1, genWidth);
    for i := 1 to fXRef.ItemCount - 1 do
      with fXRef.Items[i] do
      begin
        if ObjectStreamIndex >= 0 then
        begin
          assert(fObjectStream <> nil);
          WR.AddIntegerBin(ord(xrefInUseCompressed), TYPEWIDTH).
             AddIntegerBin(fObjectStream.ObjectNumber, offsetWidth).
             AddIntegerBin(ObjectStreamIndex, genWidth);
        end
        else
        begin
          WR.AddIntegerBin(ord(xrefInUse), TYPEWIDTH).
            AddIntegerBin(ByteOffset, offsetWidth).
            AddIntegerBin(GenerationNumber, genWidth);
        end;
      end;
    fCrossReference.WriteValueTo(W);
  end;
  W.Add(CRLF + 'startxref' + CRLF).
    Add(fXrefAddress).
    Add(CRLF + '%%EOF' + CRLF);
end;

destructor TPdfTrailer.Destroy;
begin
  fAttributes.Free;
  inherited;
end;

procedure TPdfTrailer.ToCrossReference(Doc: TPdfDocument);
var
  i: integer;
  {$ifdef USE_PDFSECURITY}
  enc: TPdfEncryption;
  {$endif USE_PDFSECURITY}
begin
  fXRef := Doc.fXRef;
  fCrossReference := TPdfStream.Create(Doc);
  fCrossReference.fSecondaryAttributes := fAttributes;
  fAttributes.AddItem('Type', 'XRef');
  {$ifdef USE_PDFSECURITY}
  fCrossReference.fDoNotEncrypt := true;
  if Doc.fEncryption <> nil then
    exit; // still a bug with encryption + objectstream
  {$endif USE_PDFSECURITY}
  fObjectStream := TPdfObjectStream.Create(Doc);
  {$ifdef USE_PDFSECURITY}
  fObjectStream.fDoNotEncrypt := true;
  enc := Doc.fEncryption;
  try
    Doc.fEncryption := nil; // force /ObjStm content not encrypted
  {$endif USE_PDFSECURITY}
    for i := 1 to fXRef.ItemCount - 1 do
      with fXRef.Items[i] do
        if (ByteOffset <= 0) and Value.InheritsFrom(TPdfDictionary) then
        begin
          fByteOffset := maxInt; // mark already handled
          fObjectStreamIndex := fObjectStream.AddObject(Value);
        end;
  {$ifdef USE_PDFSECURITY}
  finally
    Doc.fEncryption := enc;
  end;
  {$endif USE_PDFSECURITY}
end;


{ TPdfXrefEntry }

constructor TPdfXrefEntry.Create(AValue: TPdfObject);
begin
  fByteOffset := -1;
  fObjectStreamIndex := -1;
  if AValue <> nil then
  begin
    fEntryType := PDF_IN_USE_ENTRY;
    fGenerationNumber := AValue.GenerationNumber;
    fValue := AValue;
  end
  else
    fEntryType := PDF_FREE_ENTRY;
end;

destructor TPdfXrefEntry.Destroy;
begin
  if fEntryType = PDF_IN_USE_ENTRY then
    fValue.Free;
  inherited;
end;

procedure TPdfXrefEntry.SaveToPdfWrite(var W: TPdfWrite);
begin
  W.Add(fByteOffset, 10).Add(fGenerationNumber, 5).Add(fEntryType).Add(' '#10);
end;


{ TPdfXref }

constructor TPdfXref.Create;
var
  root: TPdfXrefEntry;
begin
  fXrefEntries := TSynList.Create;
  // create first a void PDF_FREE_ENTRY as root
  root := TPdfXrefEntry.Create(nil);
  root.GenerationNumber := PDF_MAX_GENERATION_NUM;
  fXrefEntries.Add(root);
end;

destructor TPdfXref.Destroy;
var
  i: integer;
begin
  for i := 0 to fXrefEntries.Count - 1 do
    GetItem(i).Free;
  fXrefEntries.Free;
  inherited;
end;

procedure TPdfXref.AddObject(AObject: TPdfObject);
var
  objnum: integer;
  root: TPdfXrefEntry;
begin
  if (AObject = nil) or
     (AObject.ObjectType <> otDirectObject) then
    raise EPdfInvalidOperation.Create('AddObject');
  root := TPdfXrefEntry.Create(AObject);
  objnum := fXrefEntries.Add(root);
  AObject.SetObjectNumber(objnum);
end;

function TPdfXref.GetItem(ObjectID: integer): TPdfXrefEntry;
begin
  result := TPdfXrefEntry(fXrefEntries[ObjectID]);
end;

function TPdfXref.GetItemCount: integer;
begin
  result := fXrefEntries.Count;
end;

function TPdfXref.GetObject(ObjectID: integer): TPdfObject;
begin
  if cardinal(ObjectID) < cardinal(fXrefEntries.Count) then
    result := TPdfXrefEntry(fXrefEntries.List[ObjectID]).Value
  else
    result := nil;
end;

procedure TPdfXref.WriteTo(var W: TPdfWrite);
var
  i: integer;
begin
  W.Add('xref' + CRLF + '0 ').Add(fXrefEntries.Count).Add(#10);
  for i := 0 to fXrefEntries.Count - 1 do
    Items[i].SaveToPdfWrite(W);
end;



{ TPdfFont }

function TPdfFont.GetAnsiCharWidth(const AText: PdfString; APos: integer): integer;
begin
  result := 0;
end;

constructor TPdfFont.Create(AXref: TPdfXref; const AName: PdfString);
begin
  inherited Create;
  FName := AName;
  Data := TPdfDictionary.Create(AXref);
  AXref.AddObject(fData);
end;

procedure TPdfFont.AddUsedWinAnsiChar(aChar: AnsiChar);
begin
  if Self <> nil then
    include(fWinAnsiUsed, aChar);
end;


{ TPdfFontWinAnsi }

destructor TPdfFontWinAnsi.Destroy;
begin
  FreeMem(fWinAnsiWidth);
  inherited;
end;

function TPdfFontWinAnsi.GetAnsiCharWidth(const AText: PdfString; APos: integer): integer;
begin
  if (fWinAnsiWidth <> nil) and
     (AText[APos] >= #32) then
    result := fWinAnsiWidth[AText[APos]]
  else
    result := fDefaultWidth;
end;


{ TPdfFontType1 }

const
  DEFAULT_PDF_WIDTH = 600;
  TTFCFP_MAC_PLATFORMID = 1;
  TTFCFP_MS_PLATFORMID = 3;
  TTFCFP_SYMBOL_CHAR_SET = 0;
  TTFCFP_UNICODE_CHAR_SET = 1;
  TTFCFP_DONT_CARE = 65535;
  TTFCFP_FLAGS_SUBSET = 1;
  TTFCFP_FLAGS_COMPRESS = 2;
  TTFMFP_SUBSET = 0;
  TTFCFP_FLAGS_TTC = 4;
  HEAD_TABLE = $64616568; // 'head'
  TTCF_TABLE = $66637474; // 'ttcf'

constructor TPdfFontType1.Create(AXref: TPdfXref; const AName: PdfString;
  WidthArray: PSmallIntArray);
var
  i: integer;
  c: AnsiChar;
  defwidth: cardinal;
  widths: TPdfArray;
begin
  inherited Create(AXref, AName);
  // adding standard element to the font definition
  Data.AddItem('Type', 'Font');
  Data.AddItem('Subtype', 'Type1');
  Data.AddItem('Encoding', 'WinAnsiEncoding');
  Data.AddItem('FirstChar', 32);
  Data.AddItem('LastChar', 255);
  Data.AddItem('BaseFont', FName);
  // register font
  if WidthArray = nil then
  begin
     // [] -> Courier fixed-width font
    fDefaultWidth := DEFAULT_PDF_WIDTH;
    fAscent := 833;
    fDescent := -300;
  end
  else
  begin
    // WidthArray[0]=Ascent, WidthArray[1]=Descent, WidthArray[2..]=Width(#32..)
    fAscent := WidthArray^[0];
    fDescent := WidthArray^[1];
    // create "Width" table of the font  (256-32=224)
    Data.AddItem('Widths', TPdfArray.Create(AXref, @WidthArray^[2], 224), true);
  end;
  // initialize char widths array by default value (if missing width parameter
  // is defined, use it as default value.)
  if Data.PdfNumberByName('MissingWidth') <> nil then
    defwidth := Data.PdfNumberByName('MissingWidth').Value
  else
    defwidth := fDefaultWidth; // typicaly 600 for Times
  GetMem(fWinAnsiWidth, SizeOf(fWinAnsiWidth^));
  for c := low(TPdfWinAnsiWidth) to high(TPdfWinAnsiWidth) do
    fWinAnsiWidth^[c] := defwidth;
  FFirstChar := Data.PdfNumberByName('FirstChar').Value;
  FLastChar := Data.PdfNumberByName('LastChar').Value;
  // fill width array with "Widths" table values.
  widths := Data.PdfArrayByName('Widths');
  if widths <> nil then
    for i := 0 to widths.ItemCount - 1 do
      if FFirstChar + i >= 32 then
        fWinAnsiWidth^[AnsiChar(FFirstChar + i)] := TPdfNumber(widths[i]).Value;
end;


{ TPdfTtf }

constructor TPdfTtf.Create(aUnicodeTtf: TPdfFontTrueType);
var
  P: pointer;
  SubTable: ^TCmapSubTableArray absolute P;
  Header: ^TCmapHeader;
  i, n, code, ndx: PtrInt;
  off: cardinal;
  glyph: integer;
  delta, offs: PtrInt;
  W, numOfLongHorMetrics: word;
  unitShr: cardinal;
begin
  // retrieve the 'cmap' (character code mapping) table
  // see http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html
  // and http://www.microsoft.com/typography/OTSPEC/cmap.htm
  P := GetTtfData(aUnicodeTtf.fDoc.fDC, 'cmap', fcmap);
  if P = nil then
    exit;
  Header := P;
  inc(PtrInt(P), SizeOf(TCmapHeader));
  off := 0;
  for i := 0 to Header^.numberSubtables - 1 do
    with SubTable^[i] do
      if platformID = TTFCFP_MS_PLATFORMID then
        if platformSpecificID = TTFCFP_SYMBOL_CHAR_SET then
        begin
          aUnicodeTtf.fIsSymbolFont := true;
          off := offset;
        end
        else if platformSpecificID = TTFCFP_UNICODE_CHAR_SET then
        begin
          aUnicodeTtf.fIsSymbolFont := false;
          off := offset;
          break; // prefered specific ID
        end;
  if (off = 0) or
     (off and 1 <> 0) then
    exit; // we handle only Microsoft platform
  i := LongRec(off).Lo; // offset swap to bswap conversion :)
  LongRec(off).Lo := LongRec(off).Hi;
  LongRec(off).Hi := i;
  if off > cardinal(Length(fcmap) * 2) then
    exit; // avoid GPF
  fmt4 := pointer(PtrUInt(fcmap) + off);
  with fmt4^ do
  begin
    if format <> 4 then
      exit; // we handle only cmap table format 4
    endCode := pointer(PtrUInt(@format) + SizeOf(TCmapFmt4));
    startCode := pointer(PtrUInt(endCode) + segCountX2 + 2); // +2 = reservedPad
    idDelta := pointer(PtrUInt(startCode) + segCountX2);
    idRangeOffset := pointer(PtrUInt(idDelta) + segCountX2);
    glyphIndexArray := pointer(PtrUInt(idRangeOffset) + segCountX2);
  end;
  // 'head', 'hmtx' (horizontal metrics) and 'hhea' (Horizontal Header) tables
  // see http://developer.apple.com/fonts/TTRefMan/RM06/Chap6hmtx.html
  head := GetTtfData(aUnicodeTtf.fDoc.fDC, 'head', fhead);
  if head = nil then
    exit;
  P := GetTtfData(aUnicodeTtf.fDoc.fDC, 'hmtx', fhmtx);
  if P = nil then
    exit;
  hhea := GetTtfData(aUnicodeTtf.fDoc.fDC, 'hhea', fhhea);
  if hhea = nil then
    exit;
  // fill aUnicodeTtf.fUsedWide[] and aUnicodeTtf.fUsedWideChar data
  n := fmt4^.segCountX2 shr 1;
  with aUnicodeTtf.fUsedWideChar do
  begin
    for i := 0 to n - 1 do
      inc(Count, endCode[i] - startCode[i] + 1);
    SetLength(Values, Count);
    SetLength(aUnicodeTtf.fUsedWide, Count);
  end;
  ndx := 0;
  for i := 0 to n - 1 do
  begin
    delta := idDelta[i];
    offs := idRangeOffset[i];
    if offs <> 0 then
      offs := offs shr 1 + i - n - startCode[i];
    for code := startCode[i] to endCode[i] do
    begin
      aUnicodeTtf.fUsedWideChar.Values[ndx] := code;
      if offs = 0 then
        glyph := code + delta
      else
      begin
        glyph := glyphIndexArray[offs + code];
        if glyph <> 0 then
          inc(glyph, delta);
      end;
      aUnicodeTtf.fUsedWide[ndx].Glyph := glyph;
      inc(ndx);
    end;
  end;
  // UnitsPerEm range is from 16 to 16384. This value should be a power of 2.
  // (from http://www.microsoft.com/typography/OTSPEC/head.htm)
  unitShr := 0; // fastest integer div for width calculating
  for i := 14 downto 4 do
    if GetBitPtr(@head^.UnitsPerEm, i) then
    begin
      unitShr := i;
      break;
    end;
  if unitShr <> 0 then
  begin
    W := (cardinal(fhmtx[0]) * 1000) shr unitShr;
    if aUnicodeTtf.FixedWidth then
      for i := 0 to aUnicodeTtf.fUsedWideChar.Count - 1 do
        aUnicodeTtf.fUsedWide[i].Width := W
    else
    begin
      numOfLongHorMetrics := fhhea[17];
      for i := 0 to aUnicodeTtf.fUsedWideChar.Count - 1 do
        with aUnicodeTtf.fUsedWide[i] do
          if Glyph <> 0 then
            if Glyph <= numOfLongHorMetrics then
              Width := (cardinal(fhmtx[Glyph * 2]) * 1000) shr unitShr
            else
              Width := W;
    end;
  end;
end;


{ TPdfFontTrueType }

const
  { collection of flags defining various characteristics of the font
    see PDF Reference 1.3 #5.7.1 }
  PDF_FONT_FIXED_WIDTH = 1;
  PDF_FONT_SERIF = 2;
  PDF_FONT_SYMBOLIC = 4;
  PDF_FONT_SCRIPT = 8;
  PDF_FONT_STD_CHARSET = 32;
  PDF_FONT_ITALIC = 64;
  PDF_FONT_ALL_CAP = 65536;
  PDF_FONT_SMALL_CAP = 131072;
  PDF_FONT_FORCE_BOLD = 262144;

type
  /// a Ttf name record used for the 'name' Format 4 table
  TNameRecord = packed record
    platformID: word;
    encodingID: word;
    languageID: word;
    nameID: word;
    length: word;
    offset: word;
  end;
  /// header for the 'name' Format 4 table

  TNameFmt4 = packed record
    /// Format selector (=0/1)
    format: word;
    /// Number of name records
    Count: word;
    /// Offset to start of string storage (from start of table)
    stringOffset: word;
    /// The name records where count is the number of records
    FirstNameRecord: TNameRecord;
  end;

function TPdfFontTrueType.FindOrAddUsedWideChar(aWideChar: WideChar): integer;
var
  n, i: integer;
  aSymbolAnsiChar: AnsiChar;
begin
  if fUnicode then // we need fUsedWide[] to be the used glyphs
  begin
    result := WinAnsiFont.FindOrAddUsedWideChar(aWideChar);
    exit;
  end;
  result := fUsedWideChar.Add(ord(aWideChar));
  if result < 0 then
  begin
    result := -(result + 1); // this WideChar was already existing -> return index
    exit;
  end;
  // this WideChar was just added -> reserve space in fUsedWide[]
  if length(fUsedWide) = fUsedWideChar.Count - 1 then
    SetLength(fUsedWide, fUsedWideChar.Count + 100);
  n := fUsedWideChar.Count - 1;
  if result < n then
    MoveFast(fUsedWide[result], fUsedWide[result + 1], (n - result) * 4);
  // create associated Unicode Font if necessary
  if UnicodeFont = nil then
    CreateAssociatedUnicodeFont;
  // update fUsedWide[result] for current glyph
  i := UnicodeFont.fUsedWideChar.IndexOf(ord(aWideChar));
  if (i < 0) and
     UnicodeFont.fIsSymbolFont then
  begin
    fDoc.Engine.UnicodeBufferToAnsi(@aSymbolAnsiChar, @aWideChar, 1);
    aWideChar := WideChar($f000 + ord(aSymbolAnsiChar));
    i := UnicodeFont.fUsedWideChar.IndexOf(ord(aWideChar));
  end;
  if i < 0 then // if this glyph doesn't exist in this font -> set to zero
    i := 0
  else
    i := UnicodeFont.fUsedWide[i].Used;
  fUsedWide[result].Used := i; // update Width and Glyph
end;

function TPdfFontTrueType.GetAndMarkGlyphAsUsed(aGlyph: word): word;
var
  i: PtrInt;
begin
  result := aGlyph; // fallback to raw glyph index if nothing explicit
  // 1. check if not already registered as used
  with WinAnsiFont do // WinAnsiFont.fUsedWide[] = glyphs used by ShowText
    for i := 0 to fUsedWideChar.Count - 1 do
      if fUsedWide[i].Glyph = aGlyph then
        exit; // fast return already existing glyph index
  // 2. register this glyph, and return Ttf glyph
  with UnicodeFont do // UnicodeFont.fUsedWide[] = available glyphs from TPdfTtf
    for i := 0 to fUsedWideChar.Count - 1 do
      if fUsedWide[i].Glyph = aGlyph then
      begin
        result := WinAnsiFont.fUsedWide[FindOrAddUsedWideChar(
          WideChar(fUsedWideChar.Values[i]))].Glyph;
        exit; // result may be 0 if this glyph doesn't exist in the CMAP content
      end;
end;

constructor TPdfFontTrueType.Create(ADoc: TPdfDocument; AFontIndex: integer;
  AStyle: TPdfFontStyles; const ALogFont: TLogFontW; AWinAnsiFont: TPdfFontTrueType);
var
  W: packed array of TABC;
  c: AnsiChar;
  nam: PdfString;
  flags: integer;
begin
  if AWinAnsiFont <> nil then
  begin
    fWinAnsiFont := AWinAnsiFont;
    fUnicode := true;
    fUnicodeFont := self;
    fHGDI := AWinAnsiFont.fHGDI; // only one GDI resource is used for both
  end
  else
  begin
    fWinAnsiFont := self;
    {$ifdef FPC}
    fHGDI := CreateFontIndirectW(@ALogFont); 
    {$else}
    fHGDI := CreateFontIndirectW(ALogFont);
    {$endif FPC}
  end;
  if AWinAnsiFont <> nil then // we use the Postscript Name here
    nam := AWinAnsiFont.fName
  else
    nam := ADoc.TtfFontPostcriptName(AFontIndex, AStyle, self);
  inherited Create(ADoc.fXRef, nam);
  fDoc := ADoc;
  fTrueTypeFontsIndex := AFontIndex + 1;
  fStyle := AStyle;
  // adding element to the dictionary
  Data.AddItem('Type', 'Font');
  Data.AddItem('BaseFont', FName);
  // retrieve font details
  fLogFont := ALogFont; // we always need our local copy
  if Unicode then
  begin
    // 1. Unicode Font
    Data.AddItem('Subtype', 'Type0');
    Data.AddItem('Encoding', 'Identity-H');
    // Retrieve some font details from WinAnsi version
    fFixedWidth := AWinAnsiFont.fFixedWidth;
    fDefaultWidth := AWinAnsiFont.fDefaultWidth;
    fM := AWinAnsiFont.fM;
    fOTM := AWinAnsiFont.fOTM;
    // get TrueType glyphs info
    fDoc.GetDCWithFont(self);
    TPdfTtf.Create(self).Free; // all the magic in one line :)
  end
  else
  begin
    // 2. WinAnsi Font
    Data.AddItem('Subtype', 'TrueType');
    Data.AddItem('Encoding', 'WinAnsiEncoding');
    // retrieve default WinAnsi characters widths
    fDoc.GetDCWithFont(self);
    GetTextMetrics(fDoc.fDC, fM);
    fOTM.otmSize := SizeOf(fOTM);
    GetOutlineTextMetrics(fDoc.fDC, SizeOf(fOTM), @fOTM);
    GetMem(fWinAnsiWidth, SizeOf(fWinAnsiWidth^));
    SetLength(W, 224);
    GetCharABCWidthsA(fDoc.fDC, 32, 255, W[0]);
    with W[0] do
      fDefaultWidth := cardinal(abcA + integer(abcB) + abcC);
    if fM.tmPitchAndFamily and TMPF_FIXED_PITCH = 0 then
    begin
      fFixedWidth := true;
      for c := #32 to #255 do
        fWinAnsiWidth[c] := fDefaultWidth;
    end
    else
      for c := #32 to #255 do
        with W[ord(c) - 32] do
          fWinAnsiWidth[c] := integer(abcA + integer(abcB) + abcC);
    // create font descriptor (the WinAnsi one is used also for unicode)
    FFontDescriptor := TPdfDictionary.Create(ADoc.fXRef);
    FFontDescriptor.fSaveAtTheEnd := true;
    ADoc.fXRef.AddObject(FFontDescriptor);
    FFontDescriptor.AddItem('Type', 'FontDescriptor');
    FFontDescriptor.AddItem('FontName', FName);
    FFontDescriptor.AddItem('Ascent', fOTM.otmAscent);
    FFontDescriptor.AddItem('CapHeight', 666);
    FFontDescriptor.AddItem('Descent', fOTM.otmDescent);
    FFontDescriptor.AddItem('ItalicAngle', fOTM.otmItalicAngle);
    FFontDescriptor.AddItem('StemV', 87);
{    if fFixedWidth then
      flags := PDF_FONT_FIXED_WIDTH else
      flags := 0;
    if (fsItalic in AStyle) and (fOTM.otmItalicAngle<>0) then
      flags := flags or PDF_FONT_ITALIC;
    if flags=0 then
      flags := PDF_FONT_STD_CHARSET;}
    if ALogFont.lfCharSet = SYMBOL_CHARSET then
      flags := PDF_FONT_SYMBOLIC
    else
      flags := PDF_FONT_STD_CHARSET;
    FFontDescriptor.AddItem('Flags', flags);
    with fOTM.otmrcFontBox do
      FFontDescriptor.AddItem('FontBBox', TPdfArray.Create(fDoc.fXRef, [Left,
        Bottom, Right, Top]));
    Data.AddItem('FontDescriptor', fFontDescriptor);
  end;
  fAscent := fOTM.otmAscent;
  fDescent := fOTM.otmDescent;
  fDoc.RegisterFont(self);
end;

destructor TPdfFontTrueType.Destroy;
begin
  if not Unicode then
    DeleteObject(fHGDI);
  inherited;
end;

function TPdfFontTrueType.GetWideCharUsed: boolean;
begin
  result := (fUsedWideChar.Count > 0);
end;

function TPdfFontTrueType.GetWideCharWidth(aWideChar: WideChar): integer;
begin
  if fUnicode then
  begin // we need fUsedWide[] to be the used glyphs
    result := WinAnsiFont.GetWideCharWidth(aWideChar);
    exit;
  end;
  result := WideCharToWinAnsi(ord(aWideChar));
  if result >= 0 then
    if (fWinAnsiWidth <> nil) and
       (result >= 32) then
      result := fWinAnsiWidth[AnsiChar(result)]
    else
      result := fDefaultWidth
  else
    result := fUsedWide[FindOrAddUsedWideChar(aWideChar)].Width;
end;

type
  TTtfTableDirectory = packed record
    sfntVersion: cardinal; // 0x00010000 for version 1.0
    numTables: word;       // number of tables
    searchRange: word;     // HighBit(NumTables) x 16
    entrySelector: word;   // Log2(HighBit(NumTables))
    rangeShift: word;      // NumTables x 16 - SearchRange
  end;
  PTtfTableDirectory = ^TTtfTableDirectory;

  TTtfTableEntry = packed record
    tag: cardinal;      // table identifier
    checksum: cardinal; // checksum for this table
    offset: cardinal;   // offset from start of font file
    length: cardinal;   // length of this table
  end;
  PTtfTableEntry = ^TTtfTableEntry;

const
  // see http://www.4real.gr/technical-documents-ttf-subset.html and
  // https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6.html
  TTF_SUBSET: array[0..9] of array[0..3] of AnsiChar = (
    'head', 'cvt ', 'fpgm', 'prep', 'hhea', 'maxp', 'hmtx', 'cmap', 'loca', 'glyf');

procedure ReduceTTF(out ttf: PdfString; SubSetData: pointer; SubSetSize: integer);
var
  dir: PTtfTableDirectory;
  d, e: PTtfTableEntry;
  head: ^TCmapHEAD;
  n, i, len: PtrInt;
  checksum: cardinal;
begin
  SetLength(ttf, SubSetSize); // maximum size
  d := pointer(ttf);
  inc(PTtfTableDirectory(d));
  // identify the tables to be included
  e := SubSetData;
  inc(PTtfTableDirectory(e));
  n := 0;
  if SubSetSize > SizeOf(PTtfTableDirectory) then
    for i := 1 to bswap16(PTtfTableDirectory(SubSetData)^.numTables) do
    begin
      if IntegerScanIndex(@TTF_SUBSET, length(TTF_SUBSET), e^.tag) >= 0 then
      begin
        d^ := e^;
        inc(d);
        inc(n);
      end;
      inc(e);
    end;
  if n < 8 then // pdf expects 10 tables, and 8..15 for our fixed dir^ values
  begin
    MoveFast(SubSetData^, pointer(ttf)^, SubSetSize); // paranoid
    exit;
  end;
  // update the main directory
  dir := pointer(ttf);
  dir^.sfntVersion := PTtfTableDirectory(SubSetData)^.sfntVersion;
  dir^.numTables := bswap16(n);
  //len := HighBit(n); // always 8 when n in 8..15
  //dir^.searchRange := bswap16(len * 16);
  //dir^.entrySelector := bswap16(Floor(log2(len))); // requires the Math unit
  //dir^.rangeShift := bswap16((integer(n) - len) * 16);
  dir^.searchRange := 32768; // pre-computed values for n in 8..15
  dir^.entrySelector := 768;
  dir^.rangeShift := 8192;
  // include the associated data
  checksum := 0;
  head := nil;
  e := pointer(ttf);
  inc(PTtfTableDirectory(e));
  for i := 1 to n do
  begin
    len := bswap32(e^.length);
    MoveFast(PByteArray(SubSetData)[bswap32(e^.offset)], d^, len);
    e^.offset := bswap32(PtrUInt(d) - PtrUInt(ttf));
    if e^.tag = HEAD_TABLE then // 'head'
      head := pointer(d);
    while len and 3 <> 0 do
    begin // 32-bit padding
      PByteArray(d)[len] := 0;
      inc(len);
    end;
    inc(checksum, bswap32(e^.checksum)); // we didn't change the table itself
    inc(PByte(d), len);
    inc(e);
  end;
  // finalize the generated content
  for i := 0 to ((SizeOf(dir^) + (n * SizeOf(e^))) shr 2) - 1 do
    inc(checksum, PCardinalArray(ttf)[i]);
  if head <> nil then
    head^.checkSumAdjustment := bswap32($B1B0AFBA - checksum);
  {%H-}PStrLen(PtrUInt(ttf) - _STRLEN)^ := PtrUInt(d) - PtrUInt(ttf); // resize
end;

procedure TPdfFontTrueType.PrepareForSaving;
var
  c: AnsiChar;
  i, n, L, ndx, count: integer;
  fonts: TPdfArray;
  font, info: TPdfDictionary;
  tounicode: TPdfStream;
  str: TStream;
  WR: TPdfWrite;
  ttfSize: cardinal;
  ttf: PdfString;
  {$ifdef USE_UNISCRIBE}
  subdata: PAnsiChar;
  submem: cardinal;
  subsize: cardinal;
  used: TSortedWordArray;
  uniflags: word;  // For CreateFontPackage
  {$endif USE_UNISCRIBE}
  ttcIndex: word; // For CreateFontPackage
  tableTag: LongWord;
  ttcNumFonts: LongWord;
begin
  str := TMemoryStream.Create;
  WR := TPdfWrite.Create(fDoc, str);
  try
    if Unicode then
    begin
      // 1. Unicode Font (see PDF 1.3 reference #5.9)
      // create font font
      font := TPdfDictionary.Create(fDoc.fXRef);
      font.AddItem('Type', 'Font');
      font.AddItem('Subtype', 'CIDFontType2');
      font.AddItem('BaseFont', // may have been prefixed
        TPdfName(WinAnsiFont.Data.ValueByName('BaseFont')).Value);
      if fDoc.fPdfA <> pdfaNone then
        font.AddItem('CIDToGIDMap', 'Identity');
      info := TPdfDictionary.Create(fDoc.fXRef);
      info.AddItem('Supplement', 0);
      info.AddItemText('Ordering', 'Identity');
      info.AddItemText('Registry', 'Adobe');
      font.AddItem('CIDSystemInfo', info);
      n := WinAnsiFont.fUsedWideChar.Count;
      if n > 0 then
      begin
        fFirstChar := WinAnsiFont.fUsedWide[0].Glyph;
        fLastChar := WinAnsiFont.fUsedWide[n - 1].Glyph;
      end;
      font.AddItem('DW', WinAnsiFont.fDefaultWidth);
      if (fDoc.fPdfA <> pdfaNone) or
         not WinAnsiFont.fFixedWidth then
      begin
        WR.Add('['); // fixed width will use /DW value
        // WinAnsiFont.fUsedWide[] contains glyphs used by ShowText
        for i := 0 to n - 1 do
          with WinAnsiFont.fUsedWide[i] do
            if Used <> 0 then
              WR.Add(Glyph).Add('[').Add(Width).Add(']');
        font.AddItem('W', TPdfRawText.Create(WR.Add(']').ToPdfString));
      end;
      font.AddItem('FontDescriptor', WinAnsiFont.fFontDescriptor);
      fDoc.fXRef.AddObject(font);
      // create and associate DescendantFonts array
      fonts := TPdfArray.Create(fDoc.fXRef);
      fonts.AddItem(font);
      Data.AddItem('DescendantFonts', fonts);
      // create tounicode CMaping
      tounicode := TPdfStream.Create(fDoc);
      tounicode.Writer.Add('/CIDInit/ProcSet findresource begin'#10 +
        '12 dict begin'#10'begincmap'#10'/CIDSystemInfo'#10'<<'#10'/Registry (').
        Add(ShortCut).
        Add('+0)'#10'/Ordering (UCS)'#10'/Supplement 0'#10'>> def'#10 +
        '/CMapName/').Add(ShortCut).Add('+0 def'#10'/CMapType 2 def'#10 +
        '1 begincodespacerange'#10'<').AddHex4(fFirstChar).
        Add('> <').AddHex4(fLastChar).Add('>'#10'endcodespacerange'#10);
      ndx := 0;
      while n > 0 do
      begin
        if n > 99 then
          L := 99
        else
          L := n;
        count := L; // calculate real count of items in this beginbfchar
        for i := ndx to ndx + L - 1 do
          if WinAnsiFont.fUsedWide[i].Used = 0 then
            dec(count);
        tounicode.Writer.Add(count).
                         Add(' beginbfchar'#10);
        for i := ndx to ndx + L - 1 do
          with WinAnsiFont.fUsedWide[i] do
            if Used <> 0 then
              tounicode.Writer.Add('<').AddHex4(Glyph).Add('> <').
                AddHex4(WinAnsiFont.fUsedWideChar.Values[i]).Add('>'#10);
        dec(n, L);
        inc(ndx, L);
        tounicode.Writer.Add('endbfchar'#10);
      end;
      tounicode.Writer.Add('endcmap'#10 +
        'CMapName currentdict /CMap defineresource pop'#10'end'#10'end');
      Data.AddItem('ToUnicode', tounicode);
    end
    else
    begin
      // 2. WinAnsi Font
      for c := #32 to #255 do
        if c in fWinAnsiUsed then
        begin
          fFirstChar := ord(c);
          Break;
        end;
      for c := #255 downto #32 do
        if c in fWinAnsiUsed then
        begin
          fLastChar := ord(c);
          Break;
        end;
      if fFirstChar <> 0 then
      begin
        Data.AddItem('FirstChar', fFirstChar);
        Data.AddItem('LastChar', fLastChar);
        WR.Add('[');
        for c := AnsiChar(fFirstChar) to AnsiChar(fLastChar) do
          if c in fWinAnsiUsed then
            WR.AddWithSpace(fWinAnsiWidth[c])
          else
            WR.Add('0 ');
        fData.AddItem('Widths', TPdfRawText.Create(WR.Add(']').ToPdfString));
      end;
      // embedd true Type font into the PDF file (allow subset of used glyph)
      if (fDoc.PdfA <> pdfaNone) or
         (fDoc.EmbeddedTtf and
          ((fDoc.fEmbeddedTtfIgnore = nil) or
           (fDoc.fEmbeddedTtfIgnore.IndexOf(
             fDoc.fTrueTypeFonts[fTrueTypeFontsIndex - 1]) < 0))) then
      begin
        fDoc.GetDCWithFont(self);
        // is the font in a .ttc collection?
        ttfSize := windows.GetFontData(fDoc.fDC, TTCF_TABLE, 0, nil, 0);
        if ttfSize <> GDI_ERROR then
        begin
          // Yes, the font is in a .ttc collection
          // find out how many fonts are included in the collection
          if windows.GetFontData(
               fDoc.fDC, TTCF_TABLE, 8, @ttcNumFonts, 4) <> GDI_ERROR then
            ttcNumFonts := bswap32(ttcNumFonts)
          else
            ttcNumFonts := 1;
          // we need to find out the index of the font within the ttc collection
          // (this is not easy, so GetTtcIndex uses lookup on known ttc fonts)
          if (ttcNumFonts < 2) or
             not GetTtcIndex(fDoc.fTrueTypeFonts[fTrueTypeFontsIndex - 1],
               ttcIndex, ttcNumFonts) then
            ttcIndex := 0;
          {$ifdef USE_UNISCRIBE}
          uniflags := TTFCFP_FLAGS_SUBSET or TTFCFP_FLAGS_TTC;
          {$endif USE_UNISCRIBE}
          tableTag := TTCF_TABLE;
        end
        else
        begin
          ttfSize := windows.GetFontData(fDoc.fDC, 0, 0, nil, 0);
          {$ifdef USE_UNISCRIBE}
          uniflags := TTFCFP_FLAGS_SUBSET;
          {$endif USE_UNISCRIBE}
          ttcIndex := 0;
          tableTag := 0;
        end;
        if ttfSize <> GDI_ERROR then
        begin
          SetLength(ttf, ttfSize);
          if windows.GetFontData(
               fDoc.fDC, tableTag, 0, pointer(ttf), ttfSize) <> GDI_ERROR then
          begin
            fFontFile2 := TPdfStream.Create(fDoc);
            {$ifdef USE_UNISCRIBE}
            if (not fDoc.fEmbeddedWholeTtf) and
              HasCreateFontPackage then
            begin
              // subset magic is done by Windows (API available since XP) :)
              used.Count := 0;
              for i := fFirstChar to fLastChar do
                if AnsiChar(i) in fWinAnsiUsed then
                  used.Add(WinAnsiConvert.AnsiToWide[i]);
              with fUsedWideChar do
                for i := 0 to count - 1 do
                  used.Add(Values[i]);
              if CreateFontPackage(pointer(ttf), ttfSize, subdata, submem,
                subsize, uniflags, ttcIndex, TTFMFP_SUBSET, 0,
                TTFCFP_MS_PLATFORMID, TTFCFP_DONT_CARE, pointer(used.Values),
                used.Count, @lpfnAllocate, @lpfnReAllocate, @lpfnFree, nil) = 0 then
              begin
                // subset was created successfully -> save to PDF file
                ReduceTTF(ttf, subdata, subsize);
                FreeMem(subdata);
                // see 5.5.3 Font Subsets: begins with a tag followed by a +
                TPdfName(Data.ValueByName('BaseFont')).Value :=
                  TPdfName(fFontDescriptor.ValueByName('FontName')).AppendPrefix;
              end;
            end;
            {$endif USE_UNISCRIBE}
            fFontFile2.Writer.Add(ttf);
            fFontFile2.fAttributes.AddItem('Length1', length(ttf));
            // /FontDescriptor is common to WinAnsi and Unicode fonts
            fFontDescriptor.AddItem('FontFile2', fFontFile2);
          end;
        end;
      end;
    end;
  finally
    WR.Free;
    str.Free;
  end;
end;

procedure TPdfFontTrueType.CreateAssociatedUnicodeFont;
begin
  fUnicodeFont := TPdfFontTrueType.Create(
    fDoc, fTrueTypeFontsIndex - 1, fStyle, fLogFont, self);
end;


{ TPdfDestination }

constructor TPdfDestination.Create(APdfDoc: TPdfDocument);
begin
  inherited Create;
  fDoc := APdfDoc;
  if fDoc = nil then
    raise EPdfInvalidOperation.Create('TPdfDestination');
  fPage := fDoc.Canvas.Page;
  fZoom := 1;
end;

destructor TPdfDestination.Destroy;
begin
  if fReference <> nil then
    fReference.Free;
  inherited;
end;

function TPdfDestination.GetElement(Index: integer): integer;
begin
  result := fValues[Index];
end;

procedure TPdfDestination.SetElement(Index: integer; Value: integer);
begin
  if fValues[Index] <> Value then
    if Value < 0 then
      fValues[Index] := -1
    else
      fValues[Index] := Value;
end;

procedure TPdfDestination.SetZoom(Value: single);
begin
  if Value <> fZoom then
    if Value < 0 then
      raise EPdfInvalidValue.Create('Zoom<0')
    else if Value > PDF_MAX_ZOOMSIZE then
      EPdfInvalidValue.RaiseUtf8('Zoom>%', [PDF_MAX_ZOOMSIZE])
    else
      fZoom := Value;
end;

function TPdfDestination.GetPageWidth: integer;
begin
  if fPage.fMediaBox <> nil then
    result := TPdfNumber(fPage.fMediaBox.Items[2]).Value
  else
    result := fDoc.DefaultPageWidth;
end;

function TPdfDestination.GetPageHeight: integer;
begin
  if fPage.fMediaBox <> nil then
    result := TPdfNumber(fPage.fMediaBox.Items[3]).Value
  else
    result := fDoc.DefaultPageHeight;
end;

function TPdfDestination.GetValue: TPdfArray;
const
  DEST_MAX_VALUE = 100;
begin
  // create TPdfArray object from the specified values.
  // the values which are not used are ignored.
  result := TPdfArray.Create(fDoc.fXRef);
  with result do
  begin
    AddItem(fPage);
    AddItem(TPdfName.Create(PDF_DESTINATION_TYPE_NAMES[fType]));
    case fType of
      // if the type is dtXYZ, only Left, Top and Zoom values are used,
      // other properties are ignored.
      dtXYZ:
        begin
          if fValues[0] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Left))
          else
            AddItem(TPdfNull.Create);
          if fValues[1] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Top))
          else
            AddItem(TPdfNull.Create);
          if fZoom < 0 then
            fZoom := 0;
          AddItem(TPdfReal.Create(fZoom));
        end;
      // if the type is dtFitR, all values except Zoom are used.
      dtFitR:
        begin
          if fValues[0] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Left))
          else
            AddItem(TPdfNull.Create);
          if fValues[1] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Bottom))
          else
            AddItem(TPdfNull.Create);
          if fValues[2] >= 0 then
            AddItem(TPdfNumber.Create(Right))
          else
            AddItem(TPdfNull.Create);
          if fValues[3] >= 0 then
            AddItem(TPdfNumber.Create(Top))
          else
            AddItem(TPdfNull.Create);
        end;
      // if the type is dtFitH or dtFitBH, only Top property is used.
      dtFitH,
      dtFitBH:
        if fValues[1] >= -DEST_MAX_VALUE then
          AddItem(TPdfNumber.Create(Top))
        else
          AddItem(TPdfNull.Create);
      // if the type is dtFitV or dtFitBV, only Top property is used.
      dtFitV,
      dtFitBV:
        if fValues[0] >= -DEST_MAX_VALUE then
          AddItem(TPdfNumber.Create(Left))
        else
          AddItem(TPdfNull.Create);
    end;
  end;
end;


{ TPdfOutlineEntry }

constructor TPdfOutlineEntry.Create(AParent: TPdfOutlineEntry; TopPosition: integer);
begin
  inherited Create;
  if AParent = nil then
    raise EPdfInvalidValue.Create('CreateEntry');
  fParent := AParent;
  fDoc := AParent.Doc;
  Data := TPdfDictionary.Create(fDoc.fXRef);
  fDoc.fXRef.AddObject(Data);
  fDoc.fObjectList.Add(Self);
  if TopPosition >= 0 then
  begin
    if fDoc.Canvas.fPage = nil then
      fDoc.RaiseInvalidOperation;
    fDest := fDoc.CreateDestination;
    fDest.DestinationType := dtXYZ;
    fDest.Zoom := 0; // will leave Zoom factor unchanged
    fDest.Left := 0; // go to left side of the page
    fDest.Top := TopPosition;
  end;
end;

destructor TPdfOutlineEntry.Destroy;
begin
  if fReference <> nil then
    fReference.Free;
  inherited;
end;

function TPdfOutlineEntry.AddChild(TopPosition: integer): TPdfOutlineEntry;
var
  TmpEntry: TPdfOutlineEntry;
begin
  // increment total Count variable
  inc(fCount);
  TmpEntry := Parent;
  while TmpEntry <> nil do
  begin
    TmpEntry.fCount := TmpEntry.fCount + 1;
    TmpEntry := TmpEntry.Parent;
  end;
  result := TPdfOutlineEntry.Create(Self, TopPosition);
  if fFirst = nil then
    fFirst := result;
  if fLast <> nil then
    fLast.fNext := result;
  result.fPrev := fLast;
  fLast := result;
end;

procedure TPdfOutlineEntry.Save;
begin
  Data.AddItem('Parent', fParent.Data);
  if Opened then
    Data.AddItem('Count', fCount)
  else
    Data.AddItem('Count', -fCount);
  Data.AddItemTextString('Title', fTitle);
  if fDest <> nil then
    Data.AddItem('Dest', fDest.GetValue);
  if fFirst <> nil then
  begin
    Data.AddItem('First', fFirst.Data);
    fFirst.Save;
  end;
  if fLast <> nil then
    Data.AddItem('Last', fLast.Data);
  if fPrev <> nil then
    Data.AddItem('Prev', fPrev.Data);
  if fNext <> nil then
  begin
    Data.AddItem('Next', fNext.Data);
    fNext.Save;
  end;
end;


{ TPdfOutlineRoot }

constructor TPdfOutlineRoot.Create(ADoc: TPdfDocument);
begin
  // no inherited Create() for this "fake" entry
  fDoc := ADoc;
  fOpened := true;
  Data := TPdfDictionary.Create(ADoc.fXRef);
  fDoc.fXRef.AddObject(Data);
  Data.AddItem('Type', 'Outlines');
  fDoc.fObjectList.Add(Self);
end;

procedure TPdfOutlineRoot.Save;
begin
  Data.AddItem('Count', fCount);
  if fFirst <> nil then
  begin
    Data.AddItem('First', fFirst.Data);
    fFirst.Save;
  end;
  if fLast <> nil then
    Data.AddItem('Last', fLast.Data);
end;


{************ TPdfDocument TPdfPage main rendering classes }

{ TPdfDocument }

constructor TPdfDocument.Create(AUseOutlines: boolean;
  ACodePage: integer; APdfA: TPdfALevel
  {$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption{$endif});
var
  LFont: TLogFontW; // TLogFontW to add to fTrueTypeFonts array as UTF-8
  i: integer;
begin
  fPdfA := APdfA;
  {$ifdef USE_PDFSECURITY}
  fEncryption := AEncryption;
  {$endif USE_PDFSECURITY}
  fTPdfPageClass := TPdfPage;
  if ACodePage = 0 then
    fCodePage := LCIDToCodePage(SysLocale.DefaultLCID)
  else // GetACP can be<>SysLocale
    fCodePage := ACodePage;
  fCharSet := CodePageToCharSet(fCodePage);
  fEngine := TSynAnsiConvert.Engine(fCodePage);
  DefaultPaperSize := psA4;
  fRawPages := TSynList.Create;
  // retrieve the current reference GDI parameters
  fDC := CreateCompatibleDC(0);
  fScreenLogPixels := GetDeviceCaps(fDC, LOGPIXELSY);
  fCanvas := TPdfCanvas.Create(Self); // need fScreenLogPixels
  // retrieve true type fonts available for all charsets
  FillCharFast(LFont, SizeOf(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET; // enumerate ALL fonts
  EnumFontFamiliesExW(fDC, LFont, @EnumFontsProcW, PtrInt(@fTrueTypeFonts), 0);
  QuickSortRawUtf8(fTrueTypeFonts, length(fTrueTypeFonts), nil, @StrIComp);
  fCompressionMethod := cmFlateDecode; // deflate by default
  fBookMarks := TRawUtf8List.CreateEx([fCaseSensitive, fNoDuplicate]);
  fMissingBookmarks := TRawUtf8List.Create;
  fUseOutlines := AUseOutlines;
  fUseFontFallBack := true;
  fFontFallBackIndex := GetTrueTypeFontIndex('Arial Unicode MS');
  if fFontFallBackIndex < 0 then // would handle at least greek/hebrew/cyrillic
    fFontFallBackIndex := GetTrueTypeFontIndex('Lucida Sans Unicode');
  if fFontFallBackIndex < 0 then
    for i := 0 to high(fTrueTypeFonts) do
      if PosEx('Unicode', fTrueTypeFonts[i]) > 0 then
      begin
        fFontFallBackIndex := i;
        break;
      end;
  NewDoc;
end;

function TPdfDocument.GetInfo: TPdfInfo;
begin
  if fInfo = nil then
    CreateInfo;
  result := fInfo;
end;

function TPdfDocument.GetOutlineRoot: TPdfOutlineRoot;
begin
  if not UseOutlines then
    RaiseInvalidOperation;
  result := fOutlineRoot;
end;

destructor TPdfDocument.Destroy;
begin
  FreeDoc;
  fCanvas.Free;
  if fSelectedDCFontOld <> 0 then
    SelectObject(fDC, fSelectedDCFontOld);
  DeleteDC(fDC);
  FEmbeddedTtfIgnore.Free;
  fRawPages.Free;
  fBookMarks.Free;
  fMissingBookmarks.Free;
  inherited;
  {$ifdef USE_PDFSECURITY}
  fEncryption.Free;
  {$endif USE_PDFSECURITY}
end;

function TPdfDocument.RegisterXObject(AObject: TPdfXObject;
  const AName: PdfString): integer;
begin
   // check object and register it
  if AObject = nil then
    raise EPdfInvalidValue.Create('RegisterXObject: no AObject');
  if AObject.Attributes.TypeOf <> 'XObject' then
    raise EPdfInvalidValue.Create('RegisterXObject: no XObject');
  if AObject.ObjectType <> otIndirectObject then
    fXRef.AddObject(AObject);
  if AObject.Attributes.ValueByName('Name') = nil then
  begin
    if GetXObject(AName) <> nil then
      EPdfInvalidValue.RaiseUtf8('RegisterXObject: dup name %', [AName]);
    result := fXObjectList.AddItem(AObject);
    AObject.Attributes.AddItem('Name', AName);
  end
  else
    result := -1;
end;

function TPdfDocument.ToPdfString(const Value: RawUtf8): PdfString;
var
  e: TSynAnsiConvert;
begin
  if self = nil then
    e := CurrentAnsiConvert
  else
    e := fEngine;
  result := e.Utf8ToAnsi(Value);
end;

const
  PDF_PRODUCER = SYNOPSE_FRAMEWORK_NAME + ' ' + SYNOPSE_FRAMEWORK_VERSION;

procedure TPdfDocument.CreateInfo;
var
  d: TPdfDictionary;
begin
  d := TPdfDictionary.Create(fXRef);
  fXRef.AddObject(d);
  d.AddItemText('Producer', PDF_PRODUCER);
  fTrailer.Attributes.AddItem('Info', d);
  fInfo := TPdfInfo.Create;
  fInfo.SetData(d);
  fObjectList.Add(fInfo);
end;

procedure AddKids(AParent, AKid: TPdfDictionary);
var
  a: TPdfArray;
begin
  // adding page object to the parent pages object.
  a := AParent.PdfArrayByName('Kids');
  a.AddItem(AKid);
  AParent.PdfNumberByName('Count').Value := a.ItemCount;
end;

function TPdfDocument.CreatePages(Parent: TPdfDictionary): TPdfDictionary;
begin
  // create pages object and register to xref.
  result := TPdfDictionary.Create(fXRef);
  result.fSaveAtTheEnd := true;
  fXRef.AddObject(result);
  with result do
  begin
    AddItem('Type', 'Pages');
    AddItem('Kids', TPdfArray.Create(fXRef));
    AddItem('Count', 0);
  end;
  if (Parent <> nil) and
     (Parent.TypeOf = 'Pages') then
    AddKids(Parent, result)
  else
    fRoot.Pages := result;
end;

function TPdfDocument.GetXObjectIndex(const AName: PdfString): integer;
begin
  for result := 0 to fXObjectList.ItemCount - 1 do
    with TPdfXObject(fXObjectList.fArray.List[result]) do
      if (fObjectType <> otVirtualObject) and
         (Attributes <> nil) and
         (TPdfName(Attributes.ValueByName('Name')).Value = AName) then
        exit;
  result := -1;
end;

function TPdfDocument.GetXObject(const AName: PdfString): TPdfXObject;
var
  i: PtrInt;
begin
  for i := 0 to fXObjectList.ItemCount - 1 do
  begin
    result := fXObjectList.fArray.List[i];
    if result.fObjectType = otVirtualObject then
    begin
      result := TPdfXObject(fXRef.GetObject(result.fObjectNumber));
      if (result = nil) or
         not result.InheritsFrom(TPdfXObject) then
        continue;
    end;
    if result.Attributes <> nil then
      if TPdfName(result.Attributes.ValueByName('Name')).Value = AName then
        exit;
  end;
  result := nil;
end;

function TPdfDocument.GetXObjectImageName(const Hash: THash128Rec;
  Width, Height: integer): PdfString;
var
  obj: TPdfXObject;
  img: TPdfImage absolute obj;
  i: PtrInt;
begin
  if not IsZero(Hash.b) then
    for i := 0 to fXObjectList.ItemCount - 1 do
    begin
      obj := fXObjectList.fArray.List[i];
      if obj.fObjectType = otVirtualObject then
        obj := TPdfXObject(fXRef.GetObject(obj.fObjectNumber));
      if (obj <> nil) and
         obj.InheritsFrom(TPdfImage) and
         (img.PixelWidth = Width) and
         (img.PixelHeight = Height) and
         IsEqual(img.fHash.b, Hash.b) and
         (obj.Attributes <> nil) then
      begin
        result := TPdfName(obj.Attributes.ValueByName('Name')).Value;
        if result <> '' then
          exit;
      end;
    end;
  result := '';
end;

function TPdfDocument.CreateAnnotation(AType: TPdfAnnotationSubType;
  const ARect: TPdfRect; BorderStyle: TPdfAnnotationBorder;
  BorderWidth: integer): TPdfDictionary;
var
  ann, bs: TPdfDictionary;
  a: TPdfArray;
  p: TPdfPage;
const
  FLAGS_PRINT = 4;
  BORDER: array[TPdfAnnotationBorder] of PdfString = ('S', 'D', 'B', 'I', 'U');
begin
  // create new annotation and set the properties
  ann := TPdfDictionary.Create(fXRef);
  ann.fSaveAtTheEnd := true;
  fXRef.AddObject(ann);
  ann.AddItem('Type', 'Annot');
  ann.AddItem('Subtype', PDF_ANNOTATION_TYPE_NAMES[ord(AType)]);
  ann.AddItem('F', FLAGS_PRINT);
  if (BorderStyle <> abSolid) or
     (BorderWidth <> 1) then
  begin
    bs := TPdfDictionary.Create(fXRef);
    if BorderStyle <> abSolid then
      bs.AddItem('S', BORDER[BorderStyle]);
    if BorderWidth <> 1 then
      bs.AddItem('W', BorderWidth);
    ann.AddItem('BS', bs);
  end;
  with ARect do
    ann.AddItem('Rect', TPdfArray.CreateReals(fXRef, [Left, Top, Right, Bottom]));
  // adding annotation to the current page
  p := fCanvas.Page;
  a := p.PdfArrayByName('Annots');
  if a = nil then
  begin
    a := TPdfArray.Create(fXRef);
    p.AddItem('Annots', a);
  end;
  a.AddItem(ann);
  result := ann;
end;

function TPdfDocument.CreateLink(const ARect: TPdfRect;
  const aBookmarkName: RawUtf8; BorderStyle: TPdfAnnotationBorder;
  BorderWidth: integer): TPdfDictionary;
var
  aDest: TPdfDestination;
begin
  result := CreateAnnotation(asLink, ARect, BorderStyle, BorderWidth);
  aDest := fBookmarks.GetObjectFrom(aBookmarkName);
  if aDest = nil then
    fMissingBookmarks.AddObject(aBookmarkName, result)
  else
    result.AddItem('Dest', aDest.GetValue);
end;

function TPdfDocument.CreateHyperLink(const ARect: TPdfRect; const url: RawUtf8;
  BorderStyle: TPdfAnnotationBorder; BorderWidth: integer): TPdfDictionary;
var
  aURIObj: TPdfDictionary;
begin
  result := CreateAnnotation(asLink, ARect, BorderStyle, BorderWidth);
  aURIObj := TPdfDictionary.Create(fXRef);
  aURIObj.fSaveAtTheEnd := true;
  aURIObj.AddItem('Type', 'Action');
  aURIObj.AddItem('S', 'URI');
  aURIObj.AddItemTextUtf8('URI', url);
  fXRef.AddObject(aURIObj);
  result.AddItem('A', aURIObj);
end;

function TPdfDocument.CreateDestination: TPdfDestination;
begin
  result := TPdfDestination.Create(Self);
  fObjectList.Add(result);
end;

procedure TPdfDocument.CreateBookMark(TopPosition: single;
  const aBookmarkName: RawUtf8);
var
  aDest: TPdfDestination;
  i: integer;
begin
  if Canvas.fPage = nil then
    RaiseInvalidOperation; // we need a page to refer to
  if (aBookmarkName = '') or
     (fBookMarks.IndexOf(aBookmarkName) >= 0) then
    EPdfInvalidValue.RaiseUtf8(
      'Duplicated or void bookmark name "%"', [aBookmarkName]);
  aDest := CreateDestination;
  aDest.DestinationType := dtXYZ;
  aDest.Zoom := 0; // will leave Zoom factor unchanged
  aDest.Left := 0; // go to left side of the page
  aDest.Top := Round(TopPosition);
  fBookMarks.AddObject(aBookmarkName, aDest);
  with fMissingBookmarks do
    for i := Count - 1 downto 0 do
      if Strings[i] = aBookmarkName then
      begin
        TPdfDictionary(Objects[i]).AddItem('Dest', aDest.GetValue);
        Delete(i);
      end;
end;

procedure TPdfDocument.NewDoc;
var
  cat: TPdfDictionary;
  dic: TPdfDictionary;
  dicD: TPdfDictionary;
  rgb: TPdfStream;
  ID: TPdfArray;
  hexFileID: PdfString;
  needFileID: boolean;
  P: PAnsiChar;
const
  ICC: array[0..139] of cardinal = (805437440, 1161970753, 4098, 1920233069,
    541214546, 542792024, 134270983, 318769920, 989868800, 1886610273,
    1280331841, 0, 1701736302, 0, 0, 0, 0, 3606446080, 256, 768802816,
    1161970753, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 167772160, 1953656931,
    4227858432, 838860800, 1668506980, 805371904, 1795162112, 1953526903,
    2617311232, 335544320, 1953524578, 2952855552, 335544320, 1129469042,
    3288399872, 234881024, 1129469031, 3556835328, 234881024, 1129469026,
    3825270784, 234881024, 1515804786, 4093706240, 335544320, 1515804775,
    134348800, 335544320, 1515804770, 469893120, 335544320, 1954047348, 0,
    2037411651, 1751607666, 808591476, 1092628528, 1700949860, 1937330976,
    1936549236, 1668172064, 1869640303, 1702125938, 100, 1668506980, 0,
    285212672, 1651467329, 1196564581, 824713282, 691550521, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 542792024, 0, 1374879744,
    256, 3423994112, 542792024, 0, 0, 0, 0, 1987212643, 0, 16777216, 13058,
    1987212643, 0, 16777216, 13058, 1987212643, 0, 16777216, 13058, 542792024,
    0, 412876800, 2773417984, 4228120576, 542792024, 0, 2368995328, 748683264,
    2500788224, 542792024, 0, 824573952, 789577728, 2629697536);
begin
  fLastOutline := nil;
  fRawPages.Clear;
  fBookMarks.Clear;
  fMissingBookmarks.Clear;
  FreeDoc;
  fXRef := TPdfXref.Create;
  fTrailer := TPdfTrailer.Create(fXRef);
  fFontList := TSynList.Create;
  fXObjectList := TPdfArray.Create(fXRef);
  fXObjectList.fSaveAtTheEnd := true;
  fObjectList := TSynList.Create;
  fRoot := TPdfCatalog.Create;
  fRoot.fOwner := self;
  cat := TPdfDictionary.Create(fXRef);
  fXRef.AddObject(cat);
  cat.AddItem('Type', 'Catalog');
  fTrailer.Attributes.AddItem('Root', cat);
  fRoot.SetData(cat);
  fRoot.PageLayout := plSinglePage;
  fObjectList.Add(fRoot);
  if UseOutlines then
  begin
    fOutlineRoot := TPdfOutlineRoot.Create(Self);
    fRoot.Data.AddItem('Outlines', fOutlineRoot.Data);
  end;
  CreateInfo;
  fInfo.CreationDate := now;
  fCurrentPages := CreatePages(nil); // nil -> create root Pages XObject
  fRoot.SetPages(fCurrentPages);
  needFileID := false;
  if fUseOptionalContent then
  begin
    if fFileFormat < pdf15 then
      fFileFormat := pdf15;
    dic := TPdfDictionary.Create(fXRef);
    dicD := TPdfDictionary.Create(fXRef);
    dicD.AddItem('BaseState', 'ON');  // must be ON in default configuration
    dicD.AddItem('OFF', TPdfArray.Create(fXRef));
    dicD.AddItem('Order', TPdfArray.Create(fXRef));
    dicD.AddItem('ListMode', 'AllPages');
    // default value but some viewers cause trouble when missing
    dicD.AddItem('RBGroups', TPdfArray.Create(fXRef));
    dic.AddItem('D', dicD);
    dic.AddItem('OCGs', TPdfArray.Create(fXRef));
    fRoot.Data.AddItem('OCProperties', dic);
  end;
  {$ifdef USE_PDFSECURITY}
  if fEncryption <> nil then
    needFileID := true;
  {$endif USE_PDFSECURITY}
  if fPdfA <> pdfaNone then
  begin
    if fPdfA >= pdfa3A then
    begin // PDF/A-3 is a subtype of 1.7 (ISO 32000-1:2008)
      if fFileFormat < pdf17 then
        fFileFormat := pdf17;
    end
    else if fFileFormat < pdf14 then
      fFileFormat := pdf14;
    {$ifdef USE_PDFSECURITY}
    if fEncryption <> nil then
      raise EPdfInvalidOperation.Create(
        'PDF/A not allowed when encryption is enabled');
    {$endif USE_PDFSECURITY}
    fUseFontFallBack := true;
    fOutputIntents := TPdfArray.Create(fXRef);
    dic := TPdfDictionary.Create(fXRef);
    dic.AddItem('Type', 'OutputIntent');
    dic.AddItem('S', 'GTS_PDFA1'); // there is no GTS_PdfA2 or GTS_PdfA3
    dic.AddItemText('OutputConditionIdentifier', 'sRGB');
    dic.AddItemText('Info', 'sRGB'); // to show the output intents on Acrobat-Reader tab "standards"
    dic.AddItemText('RegistryName', 'http://www.color.org');
    rgb := TPdfStream.Create(self);
    rgb.Attributes.AddItem('N', 3);
    rgb.Writer.Add(@ICC, SizeOf(ICC));
    dic.AddItem('DestOutputProfile', rgb);
    fOutputIntents.AddItem(dic);
    cat.AddItem('OutputIntents', fOutputIntents);
    fMetaData := TPdfStream.Create(Self);
    fMetaData.Attributes.AddItem('Subtype', 'XML');
    fMetaData.Attributes.AddItem('Type', 'Metadata');
    fMetaData.fFilter := '';
    cat.AddItem('MarkInfo', TPdfRawText.Create('<</Marked true>>'));
    cat.AddItem('Metadata', fMetaData);
    fStructTree := TPdfDictionary.Create(fXRef);
    fRoot.Data.AddItem('StructTreeRoot', fStructTree);
    needFileID := true;
  end;
  if needFileID then
  begin
    SharedRandom.Fill(@fFileID, SizeOf(fFileID));
    SetLength(hexFileID, SizeOf(fFileID) * 2 + 2);
    P := pointer(hexFileID);
    P[0] := '<';
    mormot.core.text.BinToHex(PAnsiChar(@fFileID), P + 1, 16);
    P[SizeOf(fFileID) * 2 + 1] := '>';
    ID := TPdfArray.Create(fXRef); // array of 2 strings, as file identifier
    ID.AddItem(TPdfRawText.Create(hexFileID)); // creation ID
    ID.AddItem(TPdfRawText.Create(hexFileID)); // modified ID
    fTrailer.Attributes.AddItem('ID', ID);
  end;
  {$ifdef USE_PDFSECURITY}
  if fEncryption <> nil then
    fEncryption.AttachDocument(self);
  {$endif USE_PDFSECURITY}
end;

function TPdfDocument.AddXObject(
  const AName: PdfString; AXObject: TPdfXObject): integer;
begin
  if GetXObject(AName) <> nil then
    EPdfInvalidValue.RaiseUtf8('AddXObject: dup name %', [AName]);
  // check whether AImage is valid PdfImage or not.
  if (AXObject = nil) or
     (AXObject.Attributes = nil) or
     (AXObject.Attributes.TypeOf <> 'XObject') or
     (AXObject.Attributes.PdfNameByName('Subtype') = nil) then
    EPdfInvalidValue.RaiseUtf8(
      'AddXObject: invalid TPdfImage %', [AName]);
  fXRef.AddObject(AXObject);
  result := RegisterXObject(AXObject, AName);
end;

function TPdfDocument.AddPage: TPdfPage;
var
  FResources: TPdfDictionary;
begin
  if fCurrentPages = nil then
    raise EPdfInvalidOperation.Create('AddPage');
  // create a new page object and add it to the current pages dictionary
  result := fTPdfPageClass.Create(self);
  fXRef.AddObject(result);
  fRawPages.Add(result); // pages may be nested
  AddKids(fCurrentPages, result);
  result.AddItem('Type', 'Page');
  result.AddItem('Parent', fCurrentPages);
  // create page resources
  FResources := TPdfDictionary.Create(fXRef);
  result.AddItem('Resources', FResources);
  FResources.AddItem('Font', TPdfDictionary.Create(fXRef));
  FResources.AddItem('XObject', TPdfDictionary.Create(fXRef));
  // create page content
  FResources.AddItem('ProcSet',
    TPdfArray.CreateNames(fXRef, ['PDF', 'Text', 'ImageC']));
  result.AddItem('Contents', TPdfStream.Create(self));
  // assign this page to the current PDF canvas
  fCanvas.SetPage(result);
end;

function TPdfDocument.AddTrueTypeFont(const TtfName: RawUtf8): boolean;
begin
  result := GetTrueTypeFontIndex(TtfName) < 0;
  if not result then
    exit;
  AddRawUtf8(fTrueTypeFonts, TtfName);
  QuickSortRawUtf8(fTrueTypeFonts, length(fTrueTypeFonts), nil, @StrIComp);
end;

procedure TPdfDocument.FreeDoc;
var
  i: PtrInt;
begin
  if fXObjectList <> nil then
  begin
    FreeAndNil(fXObjectList);
    for i := fFontList.Count - 1 downto 0 do
      TObject(fFontList.List[i]).Free;
    FreeAndNil(fFontList);
    for i := fObjectList.Count - 1 downto 0 do
      TObject(fObjectList.List[i]).Free;
    FreeAndNil(fObjectList);
    FreeAndNil(fXRef);
    FreeAndNil(fTrailer);
  end;
end;

procedure TPdfDocument.SaveToStream(AStream: TStream; ForceModDate: TDateTime = 0);
begin
  if fCanvas.Page = nil then
    RaiseInvalidOperation;
  SaveToStreamDirectBegin(AStream, ForceModDate);
  SaveToStreamDirectEnd;
end;

procedure TPdfDocument.RaiseInvalidOperation;
begin
  raise EPdfInvalidOperation.Create('TPdfDocument.Document is null');
end;

function TPdfDocument.SaveToFile(const aFileName: TFileName): boolean;
var
  str: TStream;
begin
  try
    str := TFileStreamEx.Create(aFileName, fmCreate);
    try
      SaveToStream(str);
      result := true;
    finally
      str.Free;
    end;
  except
    on E: Exception do // error on file creation (opened in reader?)
      result := false;
  end;
end;

const
  PDF_HEADER: array[TPdfFileFormat] of AnsiChar = (
    '3', '4', '5', '6', '7');
  PDFA_APART: array[TPdfALevel] of AnsiChar = (
    ' ', '1', '1', '2', '2', '3', '3');
  PDFA_CONFORMANCE: array[TPdfALevel] of AnsiChar = (
    ' ', 'A', 'B', 'A', 'B', 'A', 'B');

procedure TPdfDocument.SaveToStreamDirectBegin(AStream: TStream; ForceModDate: TDateTime);
begin
  if fSaveToStreamWriter <> nil then
    raise EPdfInvalidOperation.Create('SaveToStreamDirectBegin called twice');
  // write all objects to specified stream
  if ForceModDate = 0 then
    fInfo.ModDate := Now
  else
    fInfo.ModDate := ForceModDate;
  fRoot.SaveOpenAction;
  // some PDF/A-1 requirements
  if fPdfA <> pdfaNone then
  begin
    fMetaData.Writer.Add(RawUtf8(
      '<?xpacket begin="'#$EF#$BB#$BF'" id="W5M0MpCehiHzreSzNTczkc9d"?>' +
      '<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="mormot.ui.pdf">' +
      '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' +
      '<rdf:Description rdf:about="" xmlns:xmp="http://ns.adobe.com/xap/1.0/">' +
      '<xmp:CreateDate>')).AddIso8601(Info.CreationDate).
      Add('Z</xmp:CreateDate><xmp:ModifyDate>').
      AddIso8601(Info.ModDate).
      Add('Z</xmp:ModifyDate><xmp:CreatorTool>').
      AddS(Info.Creator).
      Add('</xmp:CreatorTool></rdf:Description>' +
      '<rdf:Description rdf:about="" xmlns:dc="http://purl.org/dc/elements/1.1/">' +
      '<dc:title><rdf:Alt><rdf:li xml:lang="x-default">').
      AddS(Info.Title).
      Add('</rdf:li></rdf:Alt></dc:title>' +
      '<dc:creator><rdf:Seq><rdf:li xml:lang="x-default">').
      AddS(Info.Author).
      Add('</rdf:li></rdf:Seq></dc:creator>' +
      '<dc:description><rdf:Alt><rdf:li xml:lang="x-default">').
      AddS(Info.Subject).
      Add('</rdf:li></rdf:Alt></dc:description>' +
      '</rdf:Description>').
      Add(Info.CustomMetadata).
      Add('<rdf:Description rdf:about="" xmlns:pdf="http://ns.adobe.com/pdf/1.3/">' +
      '<pdf:Keywords>').
      AddS(Info.Keywords).
      Add('</pdf:Keywords>' +
      '<pdf:Producer>' + PDF_PRODUCER + '</pdf:Producer></rdf:Description>' +
      '<rdf:Description rdf:about="" xmlns:pdfaid="http://www.aiim.org/pdfa/ns/id/">' +
      '<pdfaid:part>').
      Add(PDFA_APART[fPdfA]).
      Add('</pdfaid:part><pdfaid:conformance>').
      Add(PDFA_CONFORMANCE[fPdfA]).
      Add('</pdfaid:conformance>' +
      '</rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end="w"?>');
  end;
  // write beginning of the content
  fSaveToStreamWriter := TPdfWrite.Create(self, AStream);
  fSaveToStreamWriter.Add('%PDF-1.').Add(PDF_HEADER[fFileformat]).Add(#10);
  if fFileFormat > pdf13 then
    // PDF/A conformation requires at least four binary (>#128) characters
    fSaveToStreamWriter.Add(RawByteString('%'#237#238#239#240#10));
end;

procedure TPdfDocument.SaveToStreamDirectPageFlush(FlushCurrentPageNow: boolean);
var
  i: integer;
begin
  if (self = nil) or
     (fSaveToStreamWriter = nil) or
     (fCanvas.fPage = nil) then
    raise EPdfInvalidOperation.Create('SaveToStreamDirectPageFlush');
  if FlushCurrentPageNow then
    fCanvas.fPage.fSaveAtTheEnd := false;
  for i := 1 to fXRef.ItemCount - 1 do  // ignore fXRef[0] = root PDF_FREE_ENTRY
    with fXRef.Items[i] do
      if (ByteOffset <= 0) and
         not Value.fSaveAtTheEnd then
      begin
        fByteOffset := fSaveToStreamWriter.Position;
        Value.WriteValueTo(fSaveToStreamWriter);
      end;
end;

procedure TPdfDocument.SaveToStreamDirectEnd;
var
  i: PtrInt;
begin
  if (self = nil) or
     (fSaveToStreamWriter = nil) then
    raise EPdfInvalidOperation.Create('SaveToStreamDirectEnd');
  try
    // saving outline tree
    if UseOutlines then
      fOutlineRoot.Save;
    // update font details after all the pages are drawn
    for i := 0 to fFontList.Count - 1 do
      with TPdfFontTrueType(fFontList.List[i]) do
        if fTrueTypeFontsIndex <> 0 then
          PrepareForSaving;
    // write pending objects
    if fFileFormat >= pdf15 then
      fTrailer.ToCrossReference(self);
    for i := 1 to fXRef.ItemCount - 1 do
      with fXRef.Items[i] do
        if ByteOffset <= 0 then
        begin
          fByteOffset := fSaveToStreamWriter.Position;
          if Value <> fTrailer.fCrossReference then
            Value.WriteValueTo(fSaveToStreamWriter);
        end;
    fTrailer.XrefAddress := fSaveToStreamWriter.Position;
    if fFileFormat < pdf15 then
      fXRef.WriteTo(fSaveToStreamWriter);
    fTrailer.Attributes.PdfNumberByName('Size').Value := fXRef.ItemCount;
    fTrailer.WriteTo(fSaveToStreamWriter);
    fSaveToStreamWriter.Save; // flush TPdfWrite buffer into AStream
  finally
    FreeAndNil(fSaveToStreamWriter);
  end;
end;

procedure TPdfDocument.RegisterFont(aFont: TPdfFont);
begin
  // fonts are not registered as xref, but registered in FontList[]
  aFont.FShortCut := 'F' + UInt32ToPdfString(fFontList.Count);
  aFont.Data.AddItem('Name', aFont.FShortCut);
  fFontList.Add(aFont);
end;

function TPdfDocument.GetRegisteredNotTrueTypeFont(
  const APdfFontName: PdfString): TPdfFont;
var
  i: PtrInt;
begin
  // if specified standard font exists in fontlist, returns it
  with fFontList do
    for i := 0 to Count - 1 do
    begin
      result := TPdfFont(List[i]);
      if (result.FTrueTypeFontsIndex = 0) and
         (result.Name = APdfFontName) then
        exit;
    end;
  result := nil;
end;

function TPdfDocument.GetRegisteredTrueTypeFont(AFontIndex: integer;
  AStyle: TPdfFontStyles; ACharSet: byte): TPdfFont;
var
  i: PtrInt;
begin
  // if specified font exists in fontlist, returns the WinAnsi version
  if AFontIndex >= 0 then
    with fFontList do
      for i := 0 to Count - 1 do
      begin
        result := TPdfFont(List[i]);
        if (result.FTrueTypeFontsIndex = AFontIndex) and
           not TPdfFontTrueType(result).Unicode and
           (TPdfFontTrueType(result).Style = AStyle) and
           (TPdfFontTrueType(result).fLogFont.lfCharSet = ACharSet) then
          exit;
      end;
  result := nil;
end;

function CompareLogFontW(const L1, L2: TLogFontW): boolean;
  {$ifdef HASINLINE} inline;{$endif}
begin
  if (L1.lfWeight <> L2.lfWeight) or
     (L1.lfItalic <> L2.lfItalic) then
    // ignore lfHeight/lfUnderline/lfStrikeOut:
    // font size/underline/strike are internal to PDF graphics state
    result := false
  else
    result := (AnsiICompW(L1.lfFaceName, L2.lfFaceName) = 0);
end;

function TPdfDocument.GetTrueTypeFontIndex(const AName: RawUtf8): integer;
begin
  if StrIComp(pointer(fTrueTypeFontLastName), pointer(AName)) = 0 then
  begin
    result := fTrueTypeFontLastIndex; // simple but efficient cache
    exit;
  end;
  result := FastFindPUtf8CharSorted(
    pointer(fTrueTypeFonts), high(fTrueTypeFonts), pointer(AName), @StrIComp);
  if result >= 0 then
  begin
    fTrueTypeFontLastName := AName;
    fTrueTypeFontLastIndex := result;
  end;
end;

function TPdfDocument.GetRegisteredTrueTypeFont(const AFontLog: TLogFontW): TPdfFont;
var
  i: PtrInt;
begin
  // if specified font exists in fontlist, returns the WinAnsi version
  with fFontList do
    for i := 0 to Count - 1 do
    begin
      result := TPdfFont(List[i]);
      if (result.FTrueTypeFontsIndex <> 0) and
         not TPdfFontTrueType(result).Unicode and
         CompareLogFontW(TPdfFontTrueType(result).fLogFont, AFontLog) then
        exit;
    end;
  result := nil;
end;

procedure TPdfDocument.SetStandardFontsReplace(Value: boolean);
begin
  if fCharSet <> ANSI_CHARSET then
    fStandardFontsReplace := false
  else
    fStandardFontsReplace := Value;
end;

function TPdfDocument.GetEmbeddedTtfIgnore: TRawUtf8List;
begin
  if fEmbeddedTtfIgnore = nil then
    fEmbeddedTtfIgnore := TRawUtf8List.CreateEx([fCaseSensitive, fNoDuplicate]);
  result := fEmbeddedTtfIgnore;
end;

procedure TPdfDocument.SetDefaultPaperSize(Value: TPdfPaperSize);
const
  PAPERSIZE: array[TPdfPaperSize] of array[0..1] of integer = (
    (595, 842),    // psA4
    (419, 595),    // psA5
    (842, 1190),   // psA3
    (1190, 1683),  // psA2
    (1683, 2382),  // psA1
    (2382, 3369),  // psA0
    (612, 792),    // psLetter
    (612, 1008),   // psLegal
    (0, 0));       // psUserDefined
begin
  fDefaultPaperSize := Value;
  if Value <> psUserDefined then
  begin
    fDefaultPageWidth  := PAPERSIZE[Value, 0];
    fDefaultPageHeight := PAPERSIZE[Value, 1];
  end;
end;

procedure TPdfDocument.SetDefaultPageHeight(Value: cardinal);
begin
  fDefaultPageHeight := Value;
  fDefaultPaperSize  := psUserDefined;
end;

procedure TPdfDocument.SetDefaultPageWidth(Value: cardinal);
begin
  fDefaultPageWidth := Value;
  fDefaultPaperSize := psUserDefined;
end;

function TPdfDocument.GetDefaultPageLandscape: boolean;
begin
  result := fDefaultPageWidth > fDefaultPageHeight;
end;

procedure TPdfDocument.SetDefaultPageLandscape(Value: boolean);
var
  tmp: integer;
begin
  if Value <> DefaultPageLandscape then
  begin
    tmp := fDefaultPageHeight;
    fDefaultPageHeight := fDefaultPageWidth;
    fDefaultPageWidth := tmp;
  end;
end;

function TPdfDocument.GetDCWithFont(Ttf: TPdfFontTrueType): HDC;
begin
  if self = nil then
    result := 0
  else
  begin
    if fSelectedDCFontOld <> 0 then // prevent resource leak
      SelectObject(fDC, fSelectedDCFontOld);
    fSelectedDCFontOld := SelectObject(fDC, Ttf.fHGDI);
    result := fDC;
  end;
end;

function TrueTypeFontName(const aFontName: RawUtf8; AStyle: TPdfFontStyles): PdfString;
var
  i: PtrInt;
begin // from PDF 1.3 #5.5.2
  FastSetRawByteString(result, pointer(aFontName), length(aFontName));
  for i := length(result) downto 1 do
    if (result[i] <= ' ') or
       (result[i] >= #127) then
      Delete(result, i, 1); // spaces and not ASCII chars are removed
  if not IsAnsiCompatible(aFontName) then // unique non-void font name
    result := result + PdfString(CardinalToHexLower(DefaultHasher(
      0, pointer(aFontName), length(aFontName))));
  if pfsItalic in AStyle then
    if pfsBold in AStyle then
      result := result + ',BoldItalic'
    else
      result := result + ',Italic'
  else if pfsBold in AStyle then
    result := result + ',Bold';
end;

function TPdfDocument.TtfFontPostcriptName(aFontIndex: integer;
  AStyle: TPdfFontStyles; AFont: TPdfFontTrueType): PdfString;
// see http://www.microsoft.com/typography/OTSPEC/name.htm
const
  NAME_POSTCRIPT = 6;
var
  fName: TWordDynArray;
  name: ^TNameFmt4;
  aFontName: RawUtf8;
  i, L: integer;
  Rec: ^TNameRecord;
  PW: pointer;
begin
  aFontName := fTrueTypeFonts[aFontIndex];
  if IsAnsiCompatible(aFontName) or
     (AFont = nil) then
  begin
    result := TrueTypeFontName(aFontName, AStyle);
    exit; // no need to search for the PostScript name field in Ttf content
  end;
  name := GetTtfData(GetDCWithFont(AFont), 'name', fName);
  if (name = nil) or
     (name^.format <> 0) then
    exit;
  Rec := @name^.FirstNameRecord;
  for i := 0 to name^.Count - 1 do
    if (Rec^.nameID = NAME_POSTCRIPT) and
       (Rec^.platformID = TTFCFP_MS_PLATFORMID) and
       (Rec^.encodingID = 1) and
       (Rec^.languageID = $409) then
    begin
      PW := PAnsiChar(name) + name^.stringOffset + Rec^.offset;
      L := Rec^.length shr 1;
      if Rec^.offset and 1 <> 0 then
      begin // fix GetTtfData() wrong SwapBuffer()
        dec(PByte(PW));
        SwapBuffer(PW, L + 1); // restore big-endian original unaligned buffer
        inc(PByte(PW));
        SwapBuffer(PW, L);   // convert from big-endian at correct odd offset
      end;
      RawUnicodeToUtf8(PW, L, aFontName);
      result := TrueTypeFontName(aFontName, AStyle); // adjust name and style
      exit;
    end
    else
      inc(Rec);
end;

function TPdfDocument.CreateOutline(const Title: string; Level: integer;
  TopPosition: single): TPdfOutlineEntry;
begin
  result := nil;
  if self = nil then
    exit;
  if fLastOutline = nil then
  begin
    if not UseOutlines then
      exit; // will raise a GPF otherwise
    result := OutlineRoot;
  end
  else
  begin
    result := fLastOutline;
    while (Level <= result.Level) and
          (result.Parent <> nil) do
      result := result.Parent;
  end;
  result := result.AddChild(Trunc(TopPosition));
  result.Title := Title;
  result.Level := Level;
  fLastOutline := result;
end;

function TPdfDocument.CreateOrGetImage(
  B: TBitmap; DrawAt, ClipRc: PPdfBox): PdfString;
var
  jpg: TJpegImage;
  img: TPdfImage;
  hash: THash128Rec; // no DefaultHasher128() because AesNiHash128() makes GPF
  y, w, h, row: integer;
  palcount: cardinal;
  pal: array of TPaletteEntry;
const
  PERROW: array[TPixelFormat] of byte = (0, 1, 4, 8, 15, 16, 24, 32, 0);
begin
  result := '';
  if (self = nil) or
     (B = nil) then
    exit;
  w := B.Width;
  h := B.Height;
  FillZero(hash.b);
  if not ForceNoBitmapReuse then
  begin
    row := PERROW[B.PixelFormat];
    if row = 0 then
    begin
      B.PixelFormat := pf24bit; // convert any device or custom bitmap
      row := 24;
    end;
    if B.Palette <> 0 then
    begin
      palcount := 0;
      if (GetObject(B.Palette, SizeOf(palcount), @palcount) <> 0) and
         (palcount > 0) then
      begin
        SetLength(pal, palcount);
        if GetPaletteEntries(B.Palette, 0, palcount, pal[0]) = palcount then
          hash.c0 := crc32c(hash.c0, pointer(pal), palcount * SizeOf(pal[0]));
      end;
    end;
    row := (((w * row) + 31) and (not 31)) shr 3; // inlined BytesPerScanLine
    for y := 0 to h - 1 do
      hash.c[y and 3] := crc32c(hash.c[y and 3], B.{%H-}ScanLine[y], row);
    result := GetXObjectImageName(hash, w, h); // search for matching image
  end;
  if result = '' then
  begin
     // create new if no existing TPdfImage match
    if ForceJPEGCompression = 0 then
      img := TPdfImage.Create(Canvas.fDoc, B, true)
    else
    begin
      jpg := TJpegImage.Create;
      try
        jpg.Assign(B);
        img := TPdfImage.Create(Canvas.fDoc, jpg, false);
      finally
        jpg.Free;
      end;
    end;
    if not ForceNoBitmapReuse then
      img.fHash := hash;
    result := 'SynImg' + UInt32ToPdfString(fXObjectList.ItemCount);
    if ForceJPEGCompression = 0 then
      AddXObject(result, img)
    else
      RegisterXObject(img, result);
  end;
  // draw bitmap as XObject
  if DrawAt <> nil then
    if ClipRc <> nil then
      with DrawAt^ do
        Canvas.DrawXObjectEx(Left, Top, Width, Height,
          ClipRc^.Left, ClipRc^.Top, ClipRc^.Width, ClipRc^.Height, result)
    else
      with DrawAt^ do
        Canvas.DrawXObject(Left, Top, Width, Height, result);
end;

function TPdfDocument.CreateOptionalContentGroup(
  ParentContentGroup: TPdfOptionalContentGroup; const Title: string;
  Visible: boolean): TPdfOptionalContentGroup;
var
  d, dd: TPdfDictionary;
  arr: TPdfArray;

  function FindParentContentGroupArray(Current: TPdfArray): TPdfArray;
  var
    i: integer;
  begin
    result := nil;
    if Current = nil then
      exit;
    for i := 0 to Current.ItemCount - 1 do
      if Current.Items[i] = ParentContentGroup then
      begin
        if (i < Current.ItemCount - 1) and
           Current.Items[i + 1].InheritsFrom(TPdfArray)
          then
          result := TPdfArray(Current.Items[i + 1])
        else
        begin
          result := TPdfArray.Create(fXRef);
          Current.InsertItem(i + 1, result);
        end;
        exit;
      end;
    for i := 0 to Current.ItemCount - 1 do
      if Current.Items[i].InheritsFrom(TPdfArray) then
      begin
        result := FindParentContentGroupArray(TPdfArray(Current.Items[i]));
        if result <> nil then
          exit;
      end;
  end;

begin
  if fUseOptionalContent then
  begin
    result := TPdfOptionalContentGroup.Create(fXRef);
    fXRef.AddObject(result);
    result.AddItem('Type', 'OCG');
    result.AddItemTextString('Name', Title);
    d := fRoot.Data.PdfDictionaryByName('OCProperties');
    if d <> nil then
    begin
      dd := d.PdfDictionaryByName('D');
      if dd <> nil then
      begin
        arr := dd.PdfArrayByName('Order');
        if ParentContentGroup <> nil then
          arr := FindParentContentGroupArray(arr);
        if arr <> nil then
          arr.AddItem(result);
        if not Visible then
        begin
          arr := dd.PdfArrayByName('OFF');
          if arr <> nil then
            arr.AddItem(result);
        end;
      end;
      arr := d.PdfArrayByName('OCGs');
      if arr <> nil then
        arr.AddItem(result);
    end;
  end
  else
    result := nil;
end;

procedure TPdfDocument.CreateOptionalContentRadioGroup(
  const ContentGroups: array of TPdfOptionalContentGroup);
var
  i: integer;
  d, dd: TPdfDictionary;
  arr, rad: TPdfArray;
begin
  if fUseOptionalContent and
     (Length(ContentGroups) > 0) then
  begin
    d := fRoot.Data.PdfDictionaryByName('OCProperties');
    if d <> nil then
    begin
      dd := d.PdfDictionaryByName('D');
      if dd <> nil then
      begin
        arr := dd.PdfArrayByName('RBGroups');
        if arr <> nil then
        begin
          rad := TPdfArray.Create(fXRef);
          for i := Low(ContentGroups) to High(ContentGroups) do
            if ContentGroups[i] <> nil then
              rad.AddItem(ContentGroups[i]);
          if rad.ItemCount > 0 then
            arr.AddItem(rad)
          else
            FreeAndNil(rad);
        end;
      end;
    end;
  end;
end;

function TPdfDocument.CreateFileAttachment(const AttachFile: TFileName;
  const Description: string): TPdfDictionary;
var
  lw, fc: TUnixMSTime;
  buf: RawByteString;
begin
  result := nil;
  if not FileInfoByName(AttachFile, nil, nil, @lw, @fc) then
    exit;
  buf := StringFromFile(AttachFile);
  if buf <> '' then
    result := CreateFileAttachmentFromBuffer(buf, ExtractFileName(AttachFile),
      Description, '', UnixMSTimeToDateTimeZ(fc), UnixMSTimeToDateTimeZ(lw));
end;

function TPdfDocument.CreateFileAttachmentFromBuffer(const Buffer: RawByteString;
  const Title, Description, MimeType: string; CreationDate, ModDate: TDateTime): TPdfDictionary;
var
  fs, ef, ndic, efdic, parms: TPdfDictionary;
  arr: TPdfArray;
  str: TPdfStream;
  txt: TPdfTextString;
  mime: RawUtf8;
begin
  // create embedded file stream with main attributes
  str := TPdfStream.Create(Self);
  str.Writer.Add(pointer(Buffer), length(Buffer));
  str.Attributes.AddItem('Type', 'EmbeddedFile');
  if MimeType <> '' then
    StringToUtf8(MimeType, mime)
  else if Pos('.', Description) <> 0 then
    mime := GetMimeContentType(Buffer, Description)
  else
    mime := GetMimeContentType(Buffer);
  str.Attributes.AddItem('Subtype', mime);
  // file Params attribute
  parms := TPdfDictionary.Create(fXref);
  parms.AddItem('Size', length(Buffer));
  if CreationDate <> 0 then
    parms.AddItemText('CreationDate', DateTimeToPdfDate(CreationDate));
  if ModDate <> 0 then
    parms.AddItemText('ModDate', DateTimeToPdfDate(ModDate));
  str.Attributes.AddItem('Params', parms);
  // create Filespec
  fs := TPdfDictionary.Create(fXref);
  fXref.AddObject(fs);
  fs.AddItem('Type', 'Filespec');
  fs.AddItemTextString('F', Title);
  fs.AddItemTextString('UF', Title);
  fs.AddItemTextString('Desc', Description);
  fs.AddItem('AFRelationship', 'Alternative'); // alternative or source
  ef := TPdfDictionary.Create(fXref);
  ef.AddItem('F', str);
  ef.AddItem('UF', str);
  fs.AddItem('EF', ef);
  // ensure we have the needed Names and EmbeddedFiles dictionaries
  ndic := Root.Data.PdfDictionaryByName('Names');
  if ndic = nil then
  begin
    ndic := TPdfDictionary.Create(fXref);
    root.Data.AddItem('Names', ndic);
  end;
  efdic := ndic.PdfDictionaryByName('EmbeddedFiles');
  if efdic = nil then
  begin
    efdic := TPdfDictionary.Create(fXref);
    ndic.AddItem('EmbeddedFiles', efdic);
    arr := TPdfArray.Create(fXref);
    efdic.AddItem('Names', arr);
  end
  else
    arr := efdic.PdfArrayByName('Names');
  // add title and Filespec in Names array
  txt := TPdfTextString.Create(Title);
  arr.AddItem(txt);
  arr.AddItem(fs);
  // return the newly created Filespec dictionary
  result := fs;
end;

procedure TPdfDocument.SetUseOptionalContent(Value: boolean);
begin
  fUseOptionalContent := Value;
  NewDoc;
end;

procedure TPdfDocument.SetPdfA(Value: TPdfALevel);
begin
  fPdfA := Value;
  NewDoc;
end;

procedure TPdfDocument.SetFontFallBackName(const Value: string);
begin
  fFontFallBackIndex := GetTrueTypeFontIndex(StringToUtf8(Value));
end;

function TPdfDocument.GetFontFallBackName: string;
begin
  if fFontFallBackIndex >= 0 then
    result := Utf8ToString(fTrueTypeFonts[fFontFallBackIndex])
  else
    result := '';
end;

function TPdfDocument.GetGeneratePdf15File: boolean;
begin
  result := (self <> nil) and
            (fFileFormat >= pdf15);
end;

procedure TPdfDocument.SetGeneratePdf15File(Value: boolean);
begin
  if fFileFormat < pdf16 then
    if Value then
      fFileFormat := pdf15
    else
      fFileFormat := pdf14;
end;


{ TPdfPage }

constructor TPdfPage.Create(ADoc: TPdfDocument);
begin
  if ADoc = nil then // e.g. for TPdfForm.Create
    inherited Create(nil)
  else
  begin
    inherited Create(ADoc.fXRef);
    fDoc := ADoc;
    // set page size
    fMediaBox := TPdfArray.Create(ADoc.fXRef,
      [0, 0, ADoc.DefaultPageWidth, ADoc.DefaultPageHeight]);
    AddItem('MediaBox', fMediaBox);
  end;
  fSaveAtTheEnd := true;
end;

function TPdfPage.GetPageHeight: integer;
begin
  result := TPdfNumber(fMediaBox.Items[3]).Value;
end;

function TPdfPage.GetPageLandscape: boolean;
begin
  result := PageWidth > PageHeight;
end;

function TPdfPage.GetPageWidth: integer;
begin
  result := TPdfNumber(fMediaBox.Items[2]).Value;
end;

function TPdfPage.GetResources(const AName: PdfString): TPdfDictionary;
begin
  result := PdfDictionaryByName('Resources').
            PdfDictionaryByName(AName);
end;

function TPdfPage.MeasureText(const Text: PdfString; Width: single): integer;
var
  ch: AnsiChar;
  w, tot: single;
  i: integer;
begin
  result := 0;
  tot := 0;
  i := 1;
  while i <= Length(Text) do
  begin
    ch := Text[i];
    w := fFont.GetAnsiCharWidth(Text, i) * fFontSize / 1000;
    if (fHorizontalScaling <> 0) and
       (fHorizontalScaling <> 100) then
      w := w * fHorizontalScaling / 100;
    if w > 0 then
      w := w + fCharSpace
    else
      w := 0;
    if (ch = ' ') and
       (fWordSpace > 0) and
       (i <> Length(Text)) then
      w := w + fWordSpace;
    tot := tot + w;
    if tot > Width then
      Break;
    if SysLocale.FarEast then
      i := NextCharIndex(Text, i)
    else
      inc(i);
    result := i;
  end;
end;

procedure TPdfPage.SetCharSpace(Value: single);
begin
  if (Value < PDF_MIN_CHARSPACE) or
     (Value > PDF_MAX_CHARSPACE) then
    raise EPdfInvalidValue.Create('SetCharSpace');
  fCharSpace := Value;
end;

procedure TPdfPage.SetFontSize(Value: single);
begin
  if (Value < 0) or
     (Value > PDF_MAX_FONTSIZE) then
    raise EPdfInvalidValue.Create('SetFontSize');
  fFontSize := Value;
end;

procedure TPdfPage.SetHorizontalScaling(Value: single);
begin
  if Value < PDF_MIN_HORIZONTALSCALING then
    Value := PDF_MIN_HORIZONTALSCALING
  else if Value > PDF_MAX_HORIZONTALSCALING then
    Value := PDF_MAX_HORIZONTALSCALING;
  fHorizontalScaling := Value;
end;

procedure TPdfPage.SetLeading(Value: single);
begin
  if (Value < 0) or
     (Value > PDF_MAX_LEADING) then
    raise EPdfInvalidValue.Create('SetLeading');
  fLeading := Value;
end;

procedure TPdfPage.SetPageHeight(AValue: integer);
begin
  TPdfNumber(fMediaBox.Items[3]).Value := AValue
end;

procedure TPdfPage.SetPageLandscape(Value: boolean);
var
  tmp: integer;
begin
  if Value <> PageLandscape then
  begin
    tmp := PageHeight;
    PageHeight := PageWidth;
    PageWidth := tmp;
  end;
end;

procedure TPdfPage.SetPageWidth(AValue: integer);
begin
  TPdfNumber(fMediaBox.Items[2]).Value := AValue
end;

procedure TPdfPage.SetWordSpace(Value: single);
begin
  if Value < 0 then
    raise EPdfInvalidValue.Create('SetWordSpace');
  fWordSpace := Value;
end;

function TPdfPage.TextWidth(const Text: PdfString): single;
var
  i: integer;
  ch: AnsiChar;
  w: single;
begin
  result := 0;
  i := 1;
  while i <= Length(Text) do
  begin
    ch := Text[i];
    w := fFont.GetAnsiCharWidth(Text, i) * fFontSize / 1000;
    if (fHorizontalScaling <> 0) and
       (fHorizontalScaling <> 100) then
      w := w * fHorizontalScaling / 100;
    if w > 0 then
      w := w + fCharSpace
    else
      w := 0;
    if (ch = ' ') and
       (fWordSpace > 0) and
       (i <> Length(Text)) then
      w := w + fWordSpace;
    result := result + w;
    if SysLocale.FarEast then
      i := NextCharIndex(Text, i)
    else
      inc(i);
  end;
  result := result - fCharSpace;
end;


{ TPdfCanvas }

constructor TPdfCanvas.Create(APdfDoc: TPdfDocument);
begin
  fDoc := APdfDoc;
  fFactor := 72 / fDoc.fScreenLogPixels; // PDF expect 72 pixels per inch
  fFactorX := fFactor;
  fFactorY := fFactor;
  fDevScaleX := 1;
  fDevScaleY := 1;
  fMappingMode := MM_TEXT;
  fUseMetaFileTextPositioning := tpSetTextJustification;
  fKerningHScaleBottom := 99.0;
  fKerningHScaleTop := 101.0;
end;

function TPdfCanvas.GetPage: TPdfPage;
begin
  if (self <> nil) and
     (fPage <> nil) then
    result := fPage
  else
    raise EPdfInvalidOperation.Create('GetPage');
end;

procedure TPdfCanvas.SetPage(APage: TPdfPage);
begin
  fPage := APage;
  fPageFontList := fPage.GetResources('Font');
  fContents := TPdfStream(fPage.ValueByName('Contents'));
  fFactor := 72 / fDoc.fScreenLogPixels; // PDF expect 72 pixels per inch
end;

procedure TPdfCanvas.SetPdfFont(AFont: TPdfFont; ASize: single);
begin
  // check if this font is already the current font
  if (AFont = nil) or
     ((fPage.Font = AFont) and
      (fPage.FontSize = ASize)) then
    exit;
  // add this font to the resource array of the current page
  if fPageFontList.ValueByName(AFont.ShortCut) = nil then
    fPageFontList.AddItem(AFont.ShortCut, AFont.Data);
  // change the font
  if fContents <> nil then
    SetFontAndSize(AFont.ShortCut, ASize); // e.g. SetFontAndSize('F0',12)
  fPage.Font := AFont;
  fPage.FontSize := ASize;
end;

procedure InitializeLogFontW(const aFontName: RawUtf8; aStyle: TPdfFontStyles;
  var aFont: TLogFontW);
begin
  FillCharFast(aFont, SizeOf(aFont), 0);
  with aFont do
  begin
    lfHeight := -1000;
    if pfsBold in aStyle then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(pfsItalic in aStyle);
    lfUnderline := Byte(pfsUnderline in aStyle);
    lfStrikeOut := Byte(pfsStrikeOut in aStyle);
    Utf8ToWideChar(lfFaceName, pointer(aFontName));
  end;
end;

const
  // see PDF ref 9.6.2.2: Standard Type 1 Fonts
  // WidthArray[30]=Ascent, WidthArray[31]=Descent,
  // WidthArray[32..255]=Width(#32..#255)
  ARIAL_W_ARRAY: array[30..255] of SmallInt = (905, -212, 278, 278, 355, 556,
    556, 889, 667, 191, 333, 333, 389, 584, 278, 333, 278, 278, 556, 556, 556,
    556, 556, 556, 556, 556, 556, 556, 278, 278, 584, 584, 584, 556, 1015, 667,
    667, 722, 722, 667, 611, 778, 722, 278, 500, 667, 556, 833, 722, 778, 667,
    778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556,
    333, 556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833, 556,
    556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, 334, 260, 334,
    584, 0, 556, 0, 222, 556, 333, 1000, 556, 556, 333, 1000, 667, 333, 1000, 0,
    611, 0, 0, 222, 222, 333, 333, 350, 556, 1000, 333, 1000, 500, 333, 944, 0,
    500, 667, 0, 333, 556, 556, 556, 556, 260, 556, 333, 737, 370, 556, 584, 0,
    737, 333, 400, 584, 333, 333, 333, 556, 537, 278, 333, 333, 365, 556, 834,
    834, 834, 611, 667, 667, 667, 667, 667, 667, 1000, 722, 667, 667, 667, 667,
    278, 278, 278, 278, 722, 722, 778, 778, 778, 778, 778, 584, 778, 722, 722,
    722, 722, 667, 667, 611, 556, 556, 556, 556, 556, 556, 889, 500, 556, 556,
    556, 556, 278, 278, 278, 278, 556, 556, 556, 556, 556, 556, 556, 584, 611,
    556, 556, 556, 556, 500, 556, 500);
  ARIAL_BOLD_W_ARRAY: array[30..255] of SmallInt = (905, -212, 278, 333, 474,
    556, 556, 889, 722, 238, 333, 333, 389, 584, 278, 333, 278, 278, 556, 556,
    556, 556, 556, 556, 556, 556, 556, 556, 333, 333, 584, 584, 584, 611, 975,
    722, 722, 722, 722, 667, 611, 778, 722, 278, 556, 722, 611, 833, 722, 778,
    667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584,
    556, 333, 556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889,
    611, 611, 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500, 389, 280,
    389, 584, 0, 556, 0, 278, 556, 500, 1000, 556, 556, 333, 1000, 667, 333,
    1000, 0, 611, 0, 0, 278, 278, 500, 500, 350, 556, 1000, 333, 1000, 556, 333,
    944, 0, 500, 667, 0, 333, 556, 556, 556, 556, 280, 556, 333, 737, 370, 556,
    584, 0, 737, 333, 400, 584, 333, 333, 333, 611, 556, 278, 333, 333, 365, 556,
    834, 834, 834, 611, 722, 722, 722, 722, 722, 722, 1000, 722, 667, 667, 667,
    667, 278, 278, 278, 278, 722, 722, 778, 778, 778, 778, 778, 584, 778, 722,
    722, 722, 722, 667, 667, 611, 556, 556, 556, 556, 556, 556, 889, 556, 556,
    556, 556, 556, 278, 278, 278, 278, 611, 611, 611, 611, 611, 611, 611, 584,
    611, 611, 611, 611, 611, 556, 611, 556);
  ARIAL_ITALIC_W_ARRAY: array[30..255] of SmallInt = (905, -212, 278, 278, 355,
    556, 556, 889, 667, 191, 333, 333, 389, 584, 278, 333, 278, 278, 556, 556,
    556, 556, 556, 556, 556, 556, 556, 556, 278, 278, 584, 584, 584, 556, 1015,
    667, 667, 722, 722, 667, 611, 778, 722, 278, 500, 667, 556, 833, 722, 778,
    667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469,
    556, 333, 556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833,
    556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, 334, 260,
    334, 584, 0, 556, 0, 222, 556, 333, 1000, 556, 556, 333, 1000, 667, 333,
    1000, 0, 611, 0, 0, 222, 222, 333, 333, 350, 556, 1000, 333, 1000, 500, 333,
    944, 0, 500, 667, 0, 333, 556, 556, 556, 556, 260, 556, 333, 737, 370, 556,
    584, 0, 737, 333, 400, 584, 333, 333, 333, 556, 537, 278, 333, 333, 365, 556,
    834, 834, 834, 611, 667, 667, 667, 667, 667, 667, 1000, 722, 667, 667, 667,
    667, 278, 278, 278, 278, 722, 722, 778, 778, 778, 778, 778, 584, 778, 722,
    722, 722, 722, 667, 667, 611, 556, 556, 556, 556, 556, 556, 889, 500, 556,
    556, 556, 556, 278, 278, 278, 278, 556, 556, 556, 556, 556, 556, 556, 584,
    611, 556, 556, 556, 556, 500, 556, 500);
  ARIAL_BOLDITALIC_W_ARRAY: array[30..255] of SmallInt = (905, -212, 278, 333,
    474, 556, 556, 889, 722, 238, 333, 333, 389, 584, 278, 333, 278, 278, 556,
    556, 556, 556, 556, 556, 556, 556, 556, 556, 333, 333, 584, 584, 584, 611,
    975, 722, 722, 722, 722, 667, 611, 778, 722, 278, 556, 722, 611, 833, 722,
    778, 667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 333, 278, 333,
    584, 556, 333, 556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278,
    889, 611, 611, 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500, 389,
    280, 389, 584, 0, 556, 0, 278, 556, 500, 1000, 556, 556, 333, 1000, 667, 333,
    1000, 0, 611, 0, 0, 278, 278, 500, 500, 350, 556, 1000, 333, 1000, 556, 333,
    944, 0, 500, 667, 0, 333, 556, 556, 556, 556, 280, 556, 333, 737, 370, 556,
    584, 0, 737, 333, 400, 584, 333, 333, 333, 611, 556, 278, 333, 333, 365, 556,
    834, 834, 834, 611, 722, 722, 722, 722, 722, 722, 1000, 722, 667, 667, 667,
    667, 278, 278, 278, 278, 722, 722, 778, 778, 778, 778, 778, 584, 778, 722,
    722, 722, 722, 667, 667, 611, 556, 556, 556, 556, 556, 556, 889, 556, 556,
    556, 556, 556, 278, 278, 278, 278, 611, 611, 611, 611, 611, 611, 611, 584,
    611, 611, 611, 611, 611, 556, 611, 556);
  TIMES_ROMAN_W_ARRAY: array[30..255] of SmallInt = (891, -216, 250, 333, 408,
    500, 500, 833, 778, 180, 333, 333, 500, 564, 250, 333, 250, 278, 500, 500,
    500, 500, 500, 500, 500, 500, 500, 500, 278, 278, 564, 564, 564, 444, 921,
    722, 667, 667, 722, 611, 556, 722, 722, 333, 389, 722, 611, 889, 722, 722,
    556, 722, 667, 556, 611, 722, 722, 944, 722, 722, 611, 333, 278, 333, 469,
    500, 333, 444, 500, 444, 500, 444, 333, 500, 500, 278, 278, 500, 278, 778,
    500, 500, 500, 500, 333, 389, 278, 500, 500, 722, 500, 500, 444, 480, 200,
    480, 541, 0, 500, 0, 333, 500, 444, 1000, 500, 500, 333, 1000, 556, 333, 889,
    0, 611, 0, 0, 333, 333, 444, 444, 350, 500, 1000, 333, 980, 389, 333, 722, 0,
    444, 722, 0, 333, 500, 500, 500, 500, 200, 500, 333, 760, 276, 500, 564, 0,
    760, 333, 400, 564, 300, 300, 333, 500, 453, 250, 333, 300, 310, 500, 750,
    750, 750, 444, 722, 722, 722, 722, 722, 722, 889, 667, 611, 611, 611, 611,
    333, 333, 333, 333, 722, 722, 722, 722, 722, 722, 722, 564, 722, 722, 722,
    722, 722, 722, 556, 500, 444, 444, 444, 444, 444, 444, 667, 444, 444, 444,
    444, 444, 278, 278, 278, 278, 500, 500, 500, 500, 500, 500, 500, 564, 500,
    500, 500, 500, 500, 500, 500, 500);
  TIMES_ITALIC_W_ARRAY: array[30..255] of SmallInt = (891, -216, 250, 333, 420,
    500, 500, 833, 778, 214, 333, 333, 500, 675, 250, 333, 250, 278, 500, 500,
    500, 500, 500, 500, 500, 500, 500, 500, 333, 333, 675, 675, 675, 500, 920,
    611, 611, 667, 722, 611, 611, 722, 722, 333, 444, 667, 556, 833, 667, 722,
    611, 722, 611, 500, 556, 722, 611, 833, 611, 556, 556, 389, 278, 389, 422,
    500, 333, 500, 500, 444, 500, 444, 278, 500, 500, 278, 278, 444, 278, 722,
    500, 500, 500, 500, 389, 389, 278, 500, 444, 667, 444, 444, 389, 400, 275,
    400, 541, 0, 500, 0, 333, 500, 556, 889, 500, 500, 333, 1000, 500, 333, 944,
    0, 556, 0, 0, 333, 333, 556, 556, 350, 500, 889, 333, 980, 389, 333, 667, 0,
    389, 556, 0, 389, 500, 500, 500, 500, 275, 500, 333, 760, 276, 500, 675, 0,
    760, 333, 400, 675, 300, 300, 333, 500, 523, 250, 333, 300, 310, 500, 750,
    750, 750, 500, 611, 611, 611, 611, 611, 611, 889, 667, 611, 611, 611, 611,
    333, 333, 333, 333, 722, 667, 722, 722, 722, 722, 722, 675, 722, 722, 722,
    722, 722, 556, 611, 500, 500, 500, 500, 500, 500, 500, 667, 444, 444, 444,
    444, 444, 278, 278, 278, 278, 500, 500, 500, 500, 500, 500, 500, 675, 500,
    500, 500, 500, 500, 444, 500, 444);
  TIMES_BOLD_W_ARRAY: array[30..255] of SmallInt = (891, -216, 250, 333, 555,
    500, 500, 1000, 833, 278, 333, 333, 500, 570, 250, 333, 250, 278, 500, 500,
    500, 500, 500, 500, 500, 500, 500, 500, 333, 333, 570, 570, 570, 500, 930,
    722, 667, 722, 722, 667, 611, 778, 778, 389, 500, 778, 667, 944, 722, 778,
    611, 778, 722, 556, 667, 722, 722, 1000, 722, 722, 667, 333, 278, 333, 581,
    500, 333, 500, 556, 444, 556, 444, 333, 500, 556, 278, 333, 556, 278, 833,
    556, 500, 556, 556, 444, 389, 333, 556, 500, 722, 500, 500, 444, 394, 220,
    394, 520, 0, 500, 0, 333, 500, 500, 1000, 500, 500, 333, 1000, 556, 333,
    1000, 0, 667, 0, 0, 333, 333, 500, 500, 350, 500, 1000, 333, 1000, 389, 333,
    722, 0, 444, 722, 0, 333, 500, 500, 500, 500, 220, 500, 333, 747, 300, 500,
    570, 0, 747, 333, 400, 570, 300, 300, 333, 556, 540, 250, 333, 300, 330, 500,
    750, 750, 750, 500, 722, 722, 722, 722, 722, 722, 1000, 722, 667, 667, 667,
    667, 389, 389, 389, 389, 722, 722, 778, 778, 778, 778, 778, 570, 778, 722,
    722, 722, 722, 722, 611, 556, 500, 500, 500, 500, 500, 500, 722, 444, 444,
    444, 444, 444, 278, 278, 278, 278, 500, 556, 500, 500, 500, 500, 500, 570,
    500, 556, 556, 556, 556, 500, 556, 500);
  TIMES_BOLDITALIC_W_ARRAY: array[30..255] of SmallInt = (891, -216, 250, 389,
    555, 500, 500, 833, 778, 278, 333, 333, 500, 570, 250, 333, 250, 278, 500,
    500, 500, 500, 500, 500, 500, 500, 500, 500, 333, 333, 570, 570, 570, 500,
    832, 667, 667, 667, 722, 667, 667, 722, 778, 389, 500, 667, 611, 889, 722,
    722, 611, 722, 667, 556, 611, 722, 667, 889, 667, 611, 611, 333, 278, 333,
    570, 500, 333, 500, 500, 444, 500, 444, 333, 500, 556, 278, 278, 500, 278,
    778, 556, 500, 500, 500, 389, 389, 278, 556, 444, 667, 500, 444, 389, 348,
    220, 348, 570, 0, 500, 0, 333, 500, 500, 1000, 500, 500, 333, 1000, 556, 333,
    944, 0, 611, 0, 0, 333, 333, 500, 500, 350, 500, 1000, 333, 1000, 389, 333,
    722, 0, 389, 611, 0, 389, 500, 500, 500, 500, 220, 500, 333, 747, 266, 500,
    606, 0, 747, 333, 400, 570, 300, 300, 333, 576, 500, 250, 333, 300, 300, 500,
    750, 750, 750, 500, 667, 667, 667, 667, 667, 667, 944, 667, 667, 667, 667,
    667, 389, 389, 389, 389, 722, 722, 722, 722, 722, 722, 722, 570, 722, 722,
    722, 722, 722, 611, 611, 500, 500, 500, 500, 500, 500, 500, 722, 444, 444,
    444, 444, 444, 278, 278, 278, 278, 500, 556, 500, 500, 500, 500, 500, 570,
    500, 556, 556, 556, 556, 444, 500, 444);

  STANDARDFONTS: array[0..11] of record
    name: PdfString;
    Widths: PSmallIntArray;
  end = (
    (name: 'Times-Roman'; Widths: @TIMES_ROMAN_W_ARRAY),
    (name: 'Times-Bold'; Widths: @TIMES_BOLD_W_ARRAY),
    (name: 'Times-Italic'; Widths: @TIMES_ITALIC_W_ARRAY),
    (name: 'Times-BoldItalic'; Widths: @TIMES_BOLDITALIC_W_ARRAY),
    (name: 'Helvetica'; Widths: @ARIAL_W_ARRAY),
    (name: 'Helvetica-Bold'; Widths: @ARIAL_BOLD_W_ARRAY),
    (name: 'Helvetica-Oblique'; Widths: @ARIAL_ITALIC_W_ARRAY),
    (name: 'Helvetica-BoldOblique'; Widths: @ARIAL_BOLDITALIC_W_ARRAY),
    (name: 'Courier'; Widths: nil ), // Widths:nil -> set all widths to 600
    (name: 'Courier-Bold'; Widths: nil),
    (name: 'Courier-Oblique'; Widths: nil),
    (name: 'Courier-BoldOblique'; Widths: nil));

  STAND_FONTS_PDF: array[TPdfFontStandard] of RawUtf8 = (
    'Times', 'Helvetica', 'Courier');
  STAND_FONTS_WIN: array[TPdfFontStandard] of RawUtf8 = (
    'Times New Roman', 'Arial', 'Courier New');
  STAND_FONTS_UPPER: array[TPdfFontStandard] of PAnsiChar = (
    'TIMES', 'HELVETICA', 'COURIER');

function TPdfCanvas.SetFont(const AName: RawUtf8; ASize: single;
  AStyle: TPdfFontStyles; ACharSet, AForceTtf: integer;
  AIsFixedWidth: boolean): TPdfFont;

  procedure SetEmbeddedFont(Standard: TPdfFontStandard);
  var
    base: PtrInt;
  begin
    base := ord(Standard) * 4 + (byte(AStyle) and 3);
    result := fDoc.GetRegisteredNotTrueTypeFont(STANDARDFONTS[base].name);
    if result = nil then
    begin // font not already registered -> add now
      with STANDARDFONTS[base] do
        result := TPdfFontType1.Create(fDoc.fXRef, name, Widths);
      fDoc.RegisterFont(result);
    end;
    SetPdfFont(result, ASize);
  end;

var
  lf: TLogFontW;
  ndx: integer;
  f: TPdfFontStandard;
begin
  result := nil;
  if (self = nil) or
     (fDoc = nil) then
    exit; // avoid GPF
  if AForceTtf >= 0 then
    // an existing true type font has been specified
    ndx := AForceTtf
  else
  begin
    // handle use embedded fonts for standard fonts, if needed
    if (fDoc.fCharSet = ANSI_CHARSET) and
       fDoc.StandardFontsReplace then
    begin
      // standard/embedded fonts are WinAnsi only
      for f := low(f) to high(f) do
        if SameTextU(AName, STAND_FONTS_PDF[f]) or
           SameTextU(AName, STAND_FONTS_WIN[f]) then
        begin
          SetEmbeddedFont(f);
          if result <> nil then
            exit; // we got a standard/embedded font
        end;
    end;
    if (fPreviousRasterFontName <> '') and
       (fPreviousRasterFontName = AName) then
      ndx := fPreviousRasterFontIndex
    else
    begin
      // search the font in the global system-wide true type fonts list
      ndx := fDoc.GetTrueTypeFontIndex(AName);
      if ndx < 0 then
      begin // unknown, device or raster font
        if AIsFixedWidth then // sounds to be fixed-width -> set 'Courier New'
          ndx := fDoc.GetTrueTypeFontIndex(STAND_FONTS_WIN[pfsCourier]);
        // do not exist as is: find equivalency of some "standard" font
        for f := low(f) to high(f) do
          if (ndx < 0) and
             IdemPChar(pointer(AName), STAND_FONTS_UPPER[f]) then
            ndx := fDoc.GetTrueTypeFontIndex(STAND_FONTS_WIN[f]);
        if ndx < 0 then
        begin // use variable width default font
          ndx := fDoc.fFontFallBackIndex;
          if ndx < 0 then
            ndx := fDoc.GetTrueTypeFontIndex('Arial');
          if ndx < 0 then
            exit;
        end;
        fPreviousRasterFontName := AName;
        fPreviousRasterFontIndex := ndx;
      end;
    end;
  end;
  if ACharSet < 0 then
    ACharSet := fDoc.CharSet; // force the current PDF Document charset
  result := fDoc.GetRegisteredTrueTypeFont(ndx + 1, AStyle, ACharSet);
  if result = nil then
  begin
    // a font of this kind is not already registered -> create it
    FillCharFast(lf, SizeOf(lf), 0);
    with lf do
    begin
      lfHeight := -1000;
      if pfsBold in AStyle then
        lfWeight := FW_BOLD
      else
        lfWeight := FW_NORMAL;
      lfItalic := Byte(pfsItalic in AStyle);
      lfUnderline := Byte(pfsUnderline in AStyle);
      lfStrikeOut := Byte(pfsStrikeOut in AStyle);
      lfCharSet := ACharSet;
      Utf8ToWideChar(lfFaceName, pointer(fDoc.fTrueTypeFonts[ndx]));
    end;
    // we register now the WinAnsi font to the associated fDoc
    result := TPdfFontTrueType.Create(fDoc, ndx, AStyle, lf, nil);
  end;
  if AForceTtf < 0 then
    SetPdfFont(result, ASize);
end;

function TPdfCanvas.SetFont(ADC: HDC; const ALogFont: TLogFontW; ASize: single): TPdfFont;
var
  pfs: TPdfFontStyles;
  name: RawUtf8;
begin
  // try if the font is already registered
  result := fDoc.GetRegisteredTrueTypeFont(ALogFont);
  if result <> nil then
  begin
    SetPdfFont(result, ASize); // use the existing font, update size if necessary
    exit;
  end;
  // font is not existing -> create new
  name := RawUnicodeToUtf8(ALogFont.lfFaceName, StrLenW(ALogFont.lfFaceName));
  if ALogFont.lfItalic <> 0 then
    pfs := [pfsItalic]
  else
    byte(pfs) := 0;
  if ALogFont.lfUnderline <> 0 then
    include(pfs, pfsUnderline);
  if ALogFont.lfStrikeOut <> 0 then
    include(pfs, pfsStrikeOut);
  if ALogFont.lfWeight >= FW_SEMIBOLD then
    include(pfs, pfsBold);
  result := SetFont(name, ASize, pfs, ALogFont.lfCharSet, -1,
    ALogFont.lfPitchAndFamily and TMPF_FIXED_PITCH = 0);
end;

procedure TPdfCanvas.TextOut(X, Y: single; const Text: PdfString);
begin
  if fContents <> nil then
  begin
    fContents.Writer.Add('BT'#10).AddWithSpace(X).AddWithSpace(Y).Add('Td'#10);
    ShowText(Text);
    fContents.Writer.Add('ET'#10);
  end;
end;

procedure TPdfCanvas.TextOutW(X, Y: single; PW: PWideChar);
begin
  if fContents <> nil then
  begin
    fContents.Writer.Add('BT'#10).AddWithSpace(X).AddWithSpace(Y).Add('Td'#10);
    ShowText(PW);
    fContents.Writer.Add('ET'#10);
  end;
end;

procedure TPdfCanvas.TextRect(ARect: TPdfRect; const Text: PdfString;
  Alignment: TPdfAlignment; Clipping: boolean);
var
  w, x: single;
begin
  // calculate text width and corresponding X position according to alignment
  w := TextWidth(Text);
  case Alignment of
    paCenter:
      x := Round((ARect.Right - ARect.Left - w) / 2);
    paRightJustify:
      x := ARect.Right - ARect.Left - Round(w);
  else
    x := 0;
  end;
  // clipping client rect if needed
  if Clipping then
  begin
    GSave;
    with ARect do
    begin
      MoveTo(Left, Top);
      LineTo(Left, Bottom);
      LineTo(Right, Bottom);
      LineTo(Right, Top);
    end;
    ClosePath;
    Clip;
    NewPath;
  end;
  // show the text in the specified rectangle and alignment
  BeginText;
  MoveTextPoint(ARect.Left + x, ARect.Top - fPage.FontSize * 0.85);
  ShowText(Text);
  EndText;
  if Clipping then
    GRestore;
end;

procedure TPdfCanvas.MultilineTextRect(ARect: TPdfRect; const Text: PdfString;
  WordWrap: boolean);
var
  i, ln: integer;
  txt, s1, s2: PdfString;
  xp, yp, xp2, w: single;
  newline: boolean;

  procedure ShowTextClip(txt: PdfString; w: single);
  begin
    ShowText(copy(txt, 1, MeasureText(txt, w))); // simple clipping
  end;

begin
  yp := ARect.Top - fPage.FontSize * 0.85;
  xp := ARect.Left;
  txt := Text;
  BeginText;
  MoveTextPoint(xp, yp);
  i := 1;
  s2 := GetNextWord(txt, i);
  xp := xp + TextWidth(s2);
  if (s2 <> '') and
     (s2[Length(s2)] = ' ') then
    xp := xp + fPage.WordSpace;
  while i <= Length(txt) do
  begin
    ln := Length(s2);
    if (ln >= 2) and
       (s2[ln] = #10) and
       (s2[ln - 1] = #13) then
    begin
      s2 := copy(s2, 1, ln - 2);
      newline := true;
    end
    else
      newline := false;
    s1 := GetNextWord(txt, i);
    w := TextWidth(s1);
    xp2 := xp + w;
    if (WordWrap and (xp2 > ARect.Right)) or
       newline then
    begin
      if s2 <> '' then
        ShowTextClip(s2, ARect.Right - ARect.Left);
      s2 := '';
      MoveToNextLine;
      ARect.Top := ARect.Top - fPage.Leading;
      if ARect.Top < ARect.Bottom + fPage.FontSize then
        Break;
      xp := ARect.Left;
    end;
    xp := xp + w;
    if (s1 <> '') and
       (s1[Length(s1)] = ' ') then
      xp := xp + fPage.WordSpace;
    s2 := s2 + s1;
  end;
  if s2 <> '' then
    ShowTextClip(s2, ARect.Right - ARect.Left);
  EndText;
end;

procedure TPdfCanvas.DrawXObjectPrepare(const AXObjectName: PdfString);
var
  x: TPdfXObject;
  o: TPdfDictionary;
  {$ifdef USE_METAFILE}
  i: integer;
  {$endif USE_METAFILE}
begin
  // drawing object must be registered. check object name
  x := fDoc.GetXObject(AXObjectName);
  if x = nil then
    EPdfInvalidValue.RaiseUtf8('DrawXObject: unknown %', [AXObjectName]);
  o := fPage.GetResources('XObject');
  if o = nil then
    raise EPdfInvalidValue.Create('DrawXObject: no XObject');
  if o.ValueByName(AXObjectName) = nil then
    o.AddItem(AXObjectName, x);
  {$ifdef USE_METAFILE}
  if x.InheritsFrom(TPdfForm) then
    with TPdfForm(x).fFontList do
      for i := 0 to ItemCount - 1 do
        with Items[i] do
          if fPageFontList.ValueByName(Key) = nil then
            fPageFontList.AddItem(Key, Value);
  {$endif USE_METAFILE}
end;

procedure TPdfCanvas.DrawXObject(X, Y, AWidth, AHeight: single;
  const AXObjectName: PdfString);
begin
  DrawXObjectPrepare(AXObjectName);
  GSave;
  ConcatToCTM(AWidth, 0, 0, AHeight, X, Y);
  ExecuteXObject(AXObjectName);
  GRestore;
end;

procedure TPdfCanvas.DrawXObjectEx(X, Y, AWidth, AHeight: single;
  ClipX, ClipY, ClipWidth, ClipHeight: single; const AXObjectName: PdfString);
begin
  DrawXObjectPrepare(AXObjectName);
  GSave;
  Rectangle(ClipX, ClipY, ClipWidth, ClipHeight);
  Clip;
  NewPath;
  fNewPath := false;
  ConcatToCTM(AWidth, 0, 0, AHeight, X, Y);
  ExecuteXObject(AXObjectName);
  GRestore;
end;


// Special Graphics State

procedure TPdfCanvas.GSave;
begin
  if fContents <> nil then
    fContents.Writer.Add('q'#10);
end;

procedure TPdfCanvas.GRestore;
begin
  if fContents <> nil then
    fContents.Writer.Add('Q'#10);
end;

procedure TPdfCanvas.ConcatToCTM(a, b, c, d, e, f: single; Decimals: cardinal);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(a, Decimals).AddWithSpace(b, Decimals).
      AddWithSpace(c, Decimals).AddWithSpace(d, Decimals).
      AddWithSpace(e, Decimals).AddWithSpace(f, Decimals).Add('cm'#10);
end;


// General Graphics State

procedure TPdfCanvas.SetFlat(flatness: Byte);
begin
  if fContents <> nil then
    fContents.Writer.Add(flatness).Add(' i'#10);
end;

procedure TPdfCanvas.SetLineCap(linecap: TLineCapStyle);
begin
  if fContents <> nil then
    fContents.Writer.Add(ord(linecap)).Add(' J'#10);
end;

procedure TPdfCanvas.SetDash(const aarray: array of integer; phase: integer);
var
  i: integer;
begin
  if fContents = nil then
    exit;
  fContents.Writer.Add('[');
  if (High(aarray) >= 0) and
     (aarray[0] <> 0) then
    for i := 0 to High(aarray) do
      fContents.Writer.AddWithSpace(aarray[i]);
  fContents.Writer.Add('] ').Add(phase).Add(' d'#10);
end;

procedure TPdfCanvas.SetLineJoin(linejoin: TLineJoinStyle);
begin
  if fContents <> nil then
    fContents.Writer.Add(ord(linejoin)).Add(' j'#10);
end;

procedure TPdfCanvas.SetLineWidth(linewidth: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(linewidth).Add('w'#10);
end;

procedure TPdfCanvas.SetMiterLimit(miterlimit: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(miterlimit).Add('M'#10);
end;


// Paths

procedure TPdfCanvas.MoveTo(x, y: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(x).AddWithSpace(y).Add('m'#10);
end;

procedure TPdfCanvas.LineTo(x, y: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(x).AddWithSpace(y).Add('l'#10);
end;

procedure TPdfCanvas.CurveToC(x1, y1, x2, y2, x3, y3: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(x1).AddWithSpace(y1).
      AddWithSpace(x2).AddWithSpace(y2).
      AddWithSpace(x3).AddWithSpace(y3).Add('c'#10);
end;

procedure TPdfCanvas.CurveToV(x2, y2, x3, y3: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(x2).AddWithSpace(y2).
      AddWithSpace(x3).AddWithSpace(y3).Add('v'#10);
end;

procedure TPdfCanvas.CurveToY(x1, y1, x3, y3: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(x1).AddWithSpace(y1).
      AddWithSpace(x3).AddWithSpace(y3).Add('y'#10);
end;

procedure TPdfCanvas.Rectangle(x, y, width, height: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(x).AddWithSpace(y).
      AddWithSpace(width).AddWithSpace(height).Add('re'#10);
end;

procedure TPdfCanvas.Closepath;
begin
  if fContents <> nil then
    fContents.Writer.Add('h'#10);
end;

procedure TPdfCanvas.NewPath;
begin
  fNewPath := true;
  if fContents <> nil then
    fContents.Writer.Add('n'#10);
end;

procedure TPdfCanvas.Stroke;
begin
  if fContents <> nil then
    fContents.Writer.Add('S'#10);
end;

procedure TPdfCanvas.ClosePathStroke;
begin
  if fContents <> nil then
    fContents.Writer.Add('s'#10);
end;

procedure TPdfCanvas.Fill;
begin
  if fContents <> nil then
    fContents.Writer.Add('f'#10);
end;

procedure TPdfCanvas.Eofill;
begin
  if fContents <> nil then
    fContents.Writer.Add('f*'#10);
end;

procedure TPdfCanvas.FillStroke;
begin
  if fContents <> nil then
    fContents.Writer.Add('B'#10);
end;

procedure TPdfCanvas.ClosepathFillStroke;
begin
  if fContents <> nil then
    fContents.Writer.Add('b'#10);
end;

procedure TPdfCanvas.EofillStroke;
begin
  if fContents <> nil then
    fContents.Writer.Add('B*'#10);
end;

procedure TPdfCanvas.ClosepathEofillStroke;
begin
  if fContents <> nil then
    fContents.Writer.Add('b*'#10);
end;

procedure TPdfCanvas.Clip;
begin
  if fContents <> nil then
    fContents.Writer.Add('W'#10);
end;

procedure TPdfCanvas.Eoclip;
begin
  if fContents <> nil then
    fContents.Writer.Add('W*'#10);
end;


// Text handling

procedure TPdfCanvas.SetCharSpace(charSpace: single);
begin
  if fPage.CharSpace = charSpace then
    exit;
  fPage.SetCharSpace(charSpace);
  if fContents <> nil then
    fContents.Writer.AddWithSpace(charSpace).Add('Tc'#10);
end;

procedure TPdfCanvas.SetWordSpace(wordSpace: single);
begin
  if fPage.WordSpace = wordSpace then
    exit;
  fPage.SetWordSpace(wordSpace);
  if fContents <> nil then
    fContents.Writer.AddWithSpace(wordSpace).Add('Tw'#10);
end;

procedure TPdfCanvas.SetHorizontalScaling(hScaling: single);
begin
  if fPage.HorizontalScaling = hScaling then
    exit;
  fPage.SetHorizontalScaling(hScaling);
  if fContents <> nil then
    fContents.Writer.Add(hScaling).Add(' Tz'#10);
end;

procedure TPdfCanvas.SetLeading(leading: single);
begin
  if fPage.Leading = leading then
    exit;
  fPage.SetLeading(leading);
  if fContents <> nil then
    fContents.Writer.AddWithSpace(leading).Add('TL'#10);
end;

procedure TPdfCanvas.SetFontAndSize(const fontshortcut: PdfString; size: single);
begin
  if fContents <> nil then
    fContents.Writer.Add('/').Add(fontshortcut).
      Add(' ').AddWithSpace(size).Add('Tf'#10);
end;

procedure TPdfCanvas.SetTextRenderingMode(mode: TTextRenderingMode);
begin
  if fContents <> nil then
    fContents.Writer.Add(ord(mode)).Add(' Tr'#10);
end;

procedure TPdfCanvas.SetTextRise(rise: word);
begin
  if fContents <> nil then
    fContents.Writer.Add(rise).Add(' Ts'#10);
end;

procedure TPdfCanvas.BeginText;
begin
  if fContents <> nil then
    fContents.Writer.Add('BT'#10);
end;

procedure TPdfCanvas.EndText;
begin
  if fContents <> nil then
    fContents.Writer.Add('ET'#10);
end;

procedure TPdfCanvas.MoveTextPoint(tx, ty: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(tx).AddWithSpace(ty).Add('Td'#10);
end;

procedure TPdfCanvas.SetTextMatrix(a, b, c, d, x, y: single);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(a).AddWithSpace(b).AddWithSpace(c).
      AddWithSpace(d).AddWithSpace(x).AddWithSpace(y).Add('Tm'#10);
end;

procedure TPdfCanvas.MoveToNextLine;
begin
  if fContents <> nil then
    fContents.Writer.Add('T*'#10);
end;

{$ifdef HASVARUSTRING}

procedure TPdfCanvas.ShowText(const text: UnicodeString; NextLine: boolean);
begin // direct call of the unicode text drawing method below
  ShowText(pointer(text), NextLine);
end;

{$endif HASVARUSTRING}

procedure TPdfCanvas.ShowText(const text: PdfString; NextLine: boolean);
begin
  if (fContents <> nil) and
     (text <> '') then
    if (fDoc.fCharSet = ANSI_CHARSET) or
       IsAnsiCompatible(text) then
    begin
      if fPage.Font.Unicode and
         (fPage.fFont.FTrueTypeFontsIndex <> 0) then
        SetPdfFont(TPdfFontTrueType(fPage.Font).WinAnsiFont, fPage.FontSize);
      fContents.Writer.Add('(').AddEscapeText(pointer(text), fPage.Font).
        Add(')').Add(SHOWTEXTCMD[NextLine])
    end
    else
    begin
      if fPage.fFont.FTrueTypeFontsIndex <> 0 then
        // write TrueType text after conversion to unicode
        fContents.Writer.AddToUnicodeHexText(text, NextLine, self)
      else
        // this standard font should expect MBCS encoding
        fContents.Writer.Add('<').AddHex(text).Add('>').
          Add(SHOWTEXTCMD[NextLine]);
    end;
end;

procedure TPdfCanvas.ShowText(PW: PWideChar; NextLine: boolean);
begin
  if fContents <> nil then
    fContents.Writer.AddUnicodeHexText(PW, StrLenW(PW), NextLine, self);
end;

procedure TPdfCanvas.ShowGlyph(PW: PWord; Count: integer);
begin
  if fContents <> nil then
    fContents.Writer.AddGlyphs(PW, Count, self);
end;

procedure TPdfCanvas.ExecuteXObject(const xObject: PdfString);
begin
  if fContents <> nil then
    fContents.Writer.Add('/').Add(xObject).Add(' Do'#10);
end;

procedure TPdfCanvas.SetRGBFillColor(Value: TPdfColor);
begin
  if fContents <> nil then
    fContents.Writer.AddColorStr(Value).Add('rg'#10);
end;

procedure TPdfCanvas.SetRGBStrokeColor(Value: TPdfColor);
begin
  if fContents <> nil then
    fContents.Writer.AddColorStr(Value).Add('RG'#10);
end;

procedure TPdfCanvas.SetCMYKFillColor(C, M, Y, K: integer);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(C / 100).AddWithSpace(M / 100).
      AddWithSpace(Y / 100).AddWithSpace(K / 100).Add('k'#10);
end;

procedure TPdfCanvas.SetCMYKStrokeColor(C, M, Y, K: integer);
begin
  if fContents <> nil then
    fContents.Writer.AddWithSpace(C / 100).AddWithSpace(M / 100).
      AddWithSpace(Y / 100).AddWithSpace(K / 100).Add('K'#10);
end;

function TPdfCanvas.TextWidth(const Text: PdfString): single;
begin
  result := fPage.TextWidth(Text);
end;

function TPdfCanvas.UnicodeTextWidth(PW: PWideChar): single;
var
  s: PdfString;
  i, w, w2: integer;
begin
  w := 0;
  if PW <> nil then
    if fPage.fFont.FTrueTypeFontsIndex = 0 then
    begin
      fDoc.Engine.UnicodeBufferToAnsiVar(PW, StrLenW(PW), s);
      i := 1;
      while i <= length(s) do
      begin // loop is MBCS ready
        inc(w, fPage.fFont.GetAnsiCharWidth(s, i));
        if SysLocale.FarEast then
          i := NextCharIndex(s, i)
        else
          inc(i);
      end;
    end
    else
      with TPdfFontTrueType(fPage.fFont).WinAnsiFont do
        while PW^ <> #0 do
        begin
          w2 := GetWideCharWidth(PW^);
          if (w2 = 0) and
             (fDoc.fUseFontFallBack) and
             (fDoc.fFontFallBackIndex >= 0) then
            w2 := (SetFont('', fPage.FontSize, fStyle, -1,
              fDoc.fFontFallBackIndex) as TPdfFontTrueType).GetWideCharWidth(PW^);
          if w2 = 0 then
            w2 := DEFAULT_PDF_WIDTH;
          inc(w, w2);
          inc(PW);
        end;
  result := (w * fPage.fFontSize) / 1000;
end;

function TPdfCanvas.MeasureText(const Text: PdfString; AWidth: single): integer;
begin
  result := fPage.MeasureText(Text, AWidth);
end;

const
  // see http://paste.lisp.org/display/1105
  BEZIER: single = 0.55228477716; // = 4/3 * (sqrt(2) - 1);

procedure TPdfCanvas.Ellipse(x, y, width, height: single);
var
  w2, h2, xw2, yh2: single;
begin
  w2 := width / 2;
  h2 := height / 2;
  xw2 := x + w2;
  yh2 := y + h2;
  MoveTo(x, yh2);
  CurveToC(x, yh2 - h2 * BEZIER, xw2 - w2 * BEZIER, y, xw2, y);
  CurveToC(xw2 + w2 * BEZIER, y, x + width, yh2 - h2 * BEZIER, x + width, yh2);
  CurveToC(x + width, yh2 + h2 * BEZIER, xw2 + w2 * BEZIER, y + height, xw2, y + height);
  CurveToC(xw2 - w2 * BEZIER, y + height, x, yh2 + h2 * BEZIER, x, yh2);
end;

procedure TPdfCanvas.RoundRect(x1, y1, x2, y2, cx, cy: single);
begin
  cx := cx / 2;
  cy := cy / 2;
  MoveTo(x1 + cx, y1);
  LineTo(x2 - cx, y1);
  CurveToC(x2 - cx + BEZIER * cx, y1, x2, y1 + cy - BEZIER * cy, x2, y1 + cy);
  LineTo(x2, y2 - cy);
  CurveToC(x2, y2 - cy + BEZIER * cy, x2 - cx + BEZIER * cx, y2, x2 - cx, y2);
  LineTo(x1 + cx, y2);
  CurveToC(x1 + cx - BEZIER * cx, y2, x1, y2 - cy + BEZIER * cy, x1, y2 - cy);
  LineTo(x1, y1 + cy);
  CurveToC(x1, y1 + cy - BEZIER * cy, x1 + cx - BEZIER * cx, y1, x1 + cx, y1);
  ClosePath;
end;

function TPdfCanvas.GetNextWord(const S: PdfString; var Index: integer): PdfString;
var
  ln, i: PtrInt;
begin
  // getting a word from text
  result := '';
  ln := Length(S);
  if Index > ln then
    exit;
  i := Index;
  while true do
    if ((S[i] = #10) and
        (S[i - 1] = #13)) or
       (S[i] = ' ') then
    begin
      result := copy(S, Index, i - (Index - 1));
      break;
    end
    else if i >= ln then
    begin
      result := copy(S, Index, i - (Index - 1));
      break;
    end
    else if SysLocale.PriLangID = LANG_JAPANESE then
      if ByteType(pointer(S), i - 1) = mbTrailByte then
        if (S[i + 1] <> #129) or
           not (S[i + 2] in [#65, #66]) then
        begin
          result := copy(S, Index, i - (Index - 1));
          break;
        end
        else
          inc(i)
      else if (i < ln) and
              (ByteType(pointer(S), i) = mbLeadByte) then
      begin
        result := copy(S, Index, i - (Index - 1));
        break;
      end
      else
        inc(i)
    else
      inc(i);
  Index := i + 1;
end;

function TPdfCanvas.GetDoc: TPdfDocument;
begin
  if fDoc <> nil then
    result := fDoc
  else
    raise EPdfInvalidOperation.Create('GetDoc');
end;

function TPdfCanvas.ViewOffsetX(X: single): single;
begin
  result := (((X - fWinOrg.X) * fViewSize.cx / fWinSize.cx) + fViewOrg.X)
end;

function TPdfCanvas.ViewOffsetY(Y: single): single;
begin
  result := ((Y - fWinOrg.Y) * fViewSize.cy / fWinSize.cy) + fViewOrg.Y
end;

function TPdfCanvas.I2X(X: integer): single;
begin
  result := fOffsetXDef + (fWorldOffsetX + ViewOffsetX(X) * fWorldFactorX) * fDevScaleX
end;

function TPdfCanvas.I2Y(Y: integer): single;
begin
  result := fPage.GetPageHeight - fOffsetYDef -
            (fWorldOffsetY + ViewOffsetY(Y) * fWorldFactorY) * fDevScaleY
end;

function TPdfCanvas.S2X(X: single): single;
begin
  result := fOffsetXDef +
            (fWorldOffsetX + ViewOffsetX(X) * fWorldFactorX) * fDevScaleX;
end;

function TPdfCanvas.S2Y(Y: single): single;
begin
  result := fPage.GetPageHeight - fOffsetYDef -
            (fWorldOffsetY + ViewOffsetY(Y) * fWorldFactorY) * fDevScaleY;
end;

procedure TPdfCanvas.LineToI(x, y: integer);
begin
  LineTo(I2X(x), I2Y(y));
end;

procedure TPdfCanvas.MoveToI(x, y: integer);
begin
  MoveTo(I2X(x), I2Y(y));
end;

procedure TPdfCanvas.CurveToCI(x1, y1, x2, y2, x3, y3: integer);
begin
  CurveToC(I2X(x1), I2Y(y1), I2X(x2), I2Y(y2), I2X(x3), I2Y(y3));
end;

procedure TPdfCanvas.MoveToS(x, y: single);
begin
  MoveTo(S2X(x), S2Y(y));
end;

procedure TPdfCanvas.LineToS(x, y: single);
begin
  LineTo(S2X(x), S2Y(y));
end;

procedure TPdfCanvas.RoundRectI(x1, y1, x2, y2, cx, cy: integer);
begin
  RoundRect(I2X(x1), I2Y(y1), I2X(x2), I2Y(y2),
    cx * fDevScaleX * fWorldFactorX, -cy * fDevScaleY * fWorldFactorY);
end;

procedure TPdfCanvas.ArcI(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer;
  clockwise: boolean; arctype: TPdfCanvasArcType; var position: TPoint);
var
  res: teaDrawArray;
  i: PtrInt;
begin
  if CalcCurveArcData(
       centerx, centery, W, H, Sx, Sy, Ex, Ey, clockwise, arctype, res) then
    for i := 0 to High(res) do
      with res[i] do
        case res of
          caMoveto:
            MoveTo(S2X(pts[0].x), S2Y(pts[0].Y));
          caLine:
            LineTo(S2X(pts[0].x), S2Y(pts[0].Y));
          caCurve:
            CurveToC(S2X(pts[0].x), S2Y(pts[0].Y), S2X(pts[1].x), S2Y(pts[1].Y),
              S2X(pts[2].x), S2Y(pts[2].Y));
          caPosition:
            begin
              position.x := Round(pts[0].x);
              position.y := Round(pts[0].Y);
            end;
        end;
end;

procedure TPdfCanvas.PointI(x, y: single);
begin
  Rectangle(S2X(x), S2Y(y), 1E-2, 1E-2);
  // use small width/height size of 1E-2 because of rounding to two decimals
end;

function TPdfCanvas.BoxI(const Box: TRect; Normalize: boolean): TPdfBox;
var
  r: TPdfRect;
begin
  // to PDF coordinates conversion
  r := RectI(Box, Normalize);
  result.Width  := r.Right - r.Left;
  result.Height := r.Bottom - r.Top;
  result.Left   := r.Left;
  result.Top    := r.Top;
end;

function TPdfCanvas.RectI(const Rect: TRect; Normalize: boolean): TPdfRect;
begin
  result.Left   := I2X(Rect.Left);
  result.Right  := I2X(Rect.Right - 1);
  result.Top    := I2Y(Rect.Top);
  result.Bottom := I2Y(Rect.Bottom - 1);
  if Normalize then
    NormalizeRect(result);
end;

procedure TPdfCanvas.BeginMarkedContent(Group: TPdfOptionalContentGroup);
var
  r, p: TPdfDictionary;
  id: PdfString;
begin
  if (fContents = nil) or
     not fDoc.UseOptionalContent then
    exit;
  if Group <> nil then
  begin
    id := 'oc' + UInt32ToPdfString(Group.ObjectNumber);
    // register Group in page r p
    r := fPage.PdfDictionaryByName('Resources');
    if r <> nil then
    begin
      p := r.PdfDictionaryByName('Properties');
      if p = nil then
      begin
        p := TPdfDictionary.Create(fDoc.fXRef);
        r.AddItem('Properties', p);
      end;
      if p <> nil then
        p.AddItem(id, Group);
    end;
    fContents.Writer.Add('/OC /').Add(id).Add(' BDC'#10);
  end
  else
    fContents.Writer.Add('/OC BMC'#10);
end;

procedure TPdfCanvas.EndMarkedContent;
begin
  if (fContents <> nil) and
     fDoc.UseOptionalContent then
    fContents.Writer.Add('EMC'#10);
end;


{ TPdfDictionaryWrapper }

procedure TPdfDictionaryWrapper.SetData(AData: TPdfDictionary);
begin
  fData := AData;
  if fData <> nil then
    fData.fSaveAtTheEnd := true;
end;

function TPdfDictionaryWrapper.GetHasData: boolean;
begin
  result := (fData = nil);
end;


{ TPdfInfo }

procedure TPdfInfo.SetAuthor(const Value: string);
begin
  fData.AddItemTextString('Author', Value);
end;

procedure TPdfInfo.SetCreationDate(Value: TDateTime);
begin
  fData.AddItemText('CreationDate', DateTimeToPdfDate(Value));
end;

procedure TPdfInfo.SetModDate(Value: TDateTime);
begin
  fData.AddItemText('ModDate', DateTimeToPdfDate(Value));
end;

procedure TPdfInfo.SetCreator(const Value: string);
begin
  fData.AddItemTextString('Creator', Value);
end;

procedure TPdfInfo.SetTitle(const Value: string);
begin
  fData.AddItemTextString('Title', Value);
end;

procedure TPdfInfo.SetSubject(const Value: string);
begin
  fData.AddItemTextString('Subject', Value);
end;

procedure TPdfInfo.SetKeywords(const Value: string);
begin
  fData.AddItemTextString('Keywords', Value);
end;

function TPdfInfo.GetAuthor: string;
begin
  result := fData.PdfTextStringValueByName('Author');
end;

function TPdfInfo.GetCreationDate: TDateTime;
var
  p: TPdfText;
begin
  p := fData.PdfTextByName('CreationDate');
  if (p = nil) or
     not PdfDateToDateTime(p.Value, result) then
    result := 0;
end;

function TPdfInfo.GetModDate: TDateTime;
var
  p: TPdfText;
begin
  p := fData.PdfTextByName('ModDate');
  if (p = nil) or
     not PdfDateToDateTime(p.Value, result) then
    result := 0;
end;

function TPdfInfo.GetCreator: string;
begin
  result := fData.PdfTextStringValueByName('Creator');
end;

function TPdfInfo.GetTitle: string;
begin
  result := fData.PdfTextStringValueByName('Title');
end;

function TPdfInfo.GetSubject: string;
begin
  result := fData.PdfTextStringValueByName('Subject');
end;

function TPdfInfo.GetKeywords: string;
begin
  result := fData.PdfTextStringValueByName('Keywords');
end;


{ TPdfCatalog }

procedure TPdfCatalog.SaveOpenAction;
begin
  if (fOpenAction = nil) then
    fData.RemoveItem('OpenAction')
  else
    fData.AddItem('OpenAction', fOpenAction.GetValue);
end;

procedure TPdfCatalog.SetPageLayout(Value: TPdfPageLayout);
var
  p: TPdfName;
begin
  p := fData.PdfNameByName('PageLayout');
  if (p = nil) or
     not p.InheritsFrom(TPdfName) then
    fData.AddItem('PageLayout', PDF_PAGE_LAYOUT_NAMES[Value])
  else
    p.Value := PDF_PAGE_LAYOUT_NAMES[Value];
end;

function TPdfCatalog.GetPageLayout: TPdfPageLayout;
var
  p: TPdfName;
  s: PdfString;
begin
  result := plSinglePage;
  p := fData.PdfNameByName('PageLayout');
  if (p = nil) or
     not p.InheritsFrom(TPdfName) then
    exit;
  s := p.Value;
  for result := low(TPdfPageLayout) to high(TPdfPageLayout) do
    if PDF_PAGE_LAYOUT_NAMES[result] = s then
      exit;
  result := plSinglePage;
end;

function TPdfCatalog.GetNonFullScreenPageMode: TPdfPageMode;
var
  d: TPdfDictionary;
  m: TPdfName;
  s: PdfString;
begin
  result := pmUseNone;
  d := fData.PdfDictionaryByName('NonFullScreenPageMode');
  if d = nil then
    exit;
  m := d.PdfNameByName('NonFullScreenPageMode');
  if (m = nil) or
     not (m is TPdfName) then
    exit;
  s := m.Value;
  for result := Low(TPdfPageMode) to High(TPdfPageMode) do
    if PDF_PAGE_MODE_NAMES[result] = s then
      exit;
  result := pmUseNone;
end;

const
  PDF_PAGE_VIEWER_NAMES: array[TPdfViewerPreference] of PdfString = (
    'HideToolbar', 'HideMenubar', 'HideWindowUI', 'FitWindow',
    'CenterWindow', 'PrintScaling');

function TPdfCatalog.GetViewerPreference: TPdfViewerPreferences;
var
  d: TPdfDictionary;
  p: TPdfViewerPreference;
begin
  result := [];
  d := fData.PdfDictionaryByName('ViewerPreference');
  if d <> nil then
    for p := low(p) to high(p) do
      if d.PdfBooleanByName(PDF_PAGE_VIEWER_NAMES[p]) <> nil then
        include(result, p);
end;

procedure TPdfCatalog.SetPageMode(Value: TPdfPageMode);
var
  m: TPdfName;
begin
  m := fData.PdfNameByName('PageMode');
  if (m = nil) or
     not (m is TPdfName) then
    fData.AddItem('PageMode', PDF_PAGE_MODE_NAMES[Value])
  else
    m.Value := PDF_PAGE_MODE_NAMES[Value];
end;

procedure TPdfCatalog.SetNonFullScreenPageMode(Value: TPdfPageMode);
var
  d: TPdfDictionary;
  m: TPdfName;
begin
  d := fData.PdfDictionaryByName('ViewerPreferences');
  if d = nil then
  begin
    d := TPdfDictionary.Create(Data.ObjectMgr);
    Data.AddItem('ViewerPreferences', d);
  end;
  // if Value is pmFullScreen, remove 'PageMode' element (use default value)
  if (Value = pmFullScreen) or
     (Value = pmUseNone) then
    d.RemoveItem('NonFullScreenPageMode')
  else
  begin
    m := d.PdfNameByName('NonFullScreenPageMode');
    if (m = nil) or
       not (m is TPdfName) then
      d.AddItem('NonFullScreenPageMode', PDF_PAGE_MODE_NAMES[Value])
    else
      m.Value := PDF_PAGE_MODE_NAMES[Value];
  end;
end;

procedure TPdfCatalog.SetViewerPreference(Value: TPdfViewerPreferences);
var
  p: TPdfViewerPreference;
  d: TPdfDictionary;
begin
  d := fData.PdfDictionaryByName('ViewerPreferences');
  if (d = nil) and
     (Value <> []) then
  begin
    d := TPdfDictionary.Create(Data.ObjectMgr);
    fData.AddItem('ViewerPreferences', d);
  end;
  if d <> nil then
  begin
    for p := low(p) to high(p) do
      if p in Value then
        if p = vpEnforcePrintScaling then
          d.AddItem(PDF_PAGE_VIEWER_NAMES[p], TPdfName.Create('None'))
        else
          d.AddItem(PDF_PAGE_VIEWER_NAMES[p], TPdfBoolean.Create(true))
      else
        d.RemoveItem(PDF_PAGE_VIEWER_NAMES[p]);
    if vpEnforcePrintScaling in Value then
    begin
      d.AddItem('Enforce', TPdfArray.CreateNames(Data.ObjectMgr, ['PrintScaling']));
      if (fOwner <> nil) and
         (fOwner.fFileFormat < pdf16) then
        fOwner.fFileFormat := pdf16;
    end
    else
      d.RemoveItem('Enforce');
  end;
end;

function TPdfCatalog.GetPageMode: TPdfPageMode;
var
  m: TPdfName;
  s: PdfString;
begin
  result := pmUseNone;
  m := fData.PdfNameByName('PageMode');
  if (m = nil) or
     not m.InheritsFrom(TPdfName) then
    exit;
  s := m.Value;
  for result := Low(PDF_PAGE_MODE_NAMES) to High(PDF_PAGE_MODE_NAMES) do
    if PDF_PAGE_MODE_NAMES[result] = s then
      exit;
  result := pmUseNone;
end;

function TPdfCatalog.GetPages: TPdfDictionary;
begin
  result := fData.PdfDictionaryByName('Pages');
  if result = nil then
    raise EPdfInvalidOperation.Create('GetPages');
end;

procedure TPdfCatalog.SetPages(APages: TPdfDictionary);
begin
  if APages.TypeOf = 'Pages' then
    fData.AddItem('Pages', APages);
end;


{ TPdfImage }

constructor TPdfImage.Create(aDoc: TPdfDocument; aImage: TGraphic;
  DontAddToFXref: boolean);
var
  bmp: TBitmap;
  pinc, y: integer;
  pal: PdfString;
  entry: array of TPaletteEntry;
  ca: TPdfArray;
  transcolor: TPdfColorRGB;

  procedure NeedBitmap(PF: TPixelFormat);
  begin
    bmp := TBitmap.Create; // create a temp bitmap (pixelformat may change)
    bmp.PixelFormat := PF;
    bmp.Width := fPixelWidth;
    bmp.Height := fPixelHeight;
    bmp.Canvas.Draw(0, 0, aImage);
  end;

  procedure WritePal(P: PAnsiChar; pal: PPaletteEntry);
  var
    i: integer;
  begin
    P^ := '<';
    inc(P);
    for i := 0 to 255 do
      with pal^ do
      begin
        P[0] := HexChars[peRed shr 4];
        P[1] := HexChars[peRed and $F];
        P[2] := HexChars[peGreen shr 4];
        P[3] := HexChars[peGreen and $F];
        P[4] := HexChars[peBlue shr 4];
        P[5] := HexChars[peBlue and $F];
        P[6] := ' ';
        inc(P, 7);
        inc(pal);
      end;
    P^ := '>';
  end;

begin
  inherited Create(aDoc, DontAddToFXref);
  fPixelWidth := aImage.Width;
  fPixelHeight := aImage.Height;
  fAttributes.AddItem('Type', 'XObject');
  fAttributes.AddItem('Subtype', 'Image');
  if aImage.InheritsFrom(TJpegImage) then
  begin
    fAttributes.AddItem('ColorSpace', 'DeviceRGB');
    fFilter := 'DCTDecode';
    fWriter.Save; // flush to allow direct access to fDestStream
    with TJpegImage(aImage) do
    begin
      if aDoc.ForceJPEGCompression <> 0 then
        CompressionQuality := aDoc.ForceJPEGCompression;
      {$ifdef USE_SYNGDIPLUS}
      if aDoc.ForceJPEGCompression = 0 then // recompression only if necessary
        SaveInternalToStream(fWriter.fDestStream)
      else
      {$endif USE_SYNGDIPLUS}
        SaveToStream(fWriter.fDestStream); // with CompressionQuality recompress
    end;
    fWriter.fDestStreamPosition := fWriter.fDestStream.Seek(0, soCurrent);
  end
  else
  begin
    if aImage.InheritsFrom(TBitmap) then
      bmp := TBitmap(aImage)
    else
      NeedBitmap(pf24bit);
    try
      case bmp.PixelFormat of
        pf1bit,
        pf4bit,
        pf8bit:
          begin
            if bmp.PixelFormat <> pf8bit then
              NeedBitmap(pf8bit);
            SetLength(entry, 256);
            if GetPaletteEntries(bmp.Palette, 0, 256, entry[0]) <> 256 then
              raise EPdfInvalidValue.Create('TPdfImage');
            SetLength(pal, 7 * 256 + 2);
            WritePal(pointer(pal), pointer(entry));
            ca := TPdfArray.Create(nil);
            ca.AddItem(TPdfName.Create('Indexed'));
            ca.AddItem(TPdfName.Create('DeviceRGB'));
            ca.AddItem(TPdfNumber.Create(255));
            ca.AddItem(TPdfRawText.Create(pal));
            fAttributes.AddItem('ColorSpace', ca);
            for y := 0 to fPixelHeight - 1 do
              fWriter.Add(PAnsiChar(bmp.{%H-}ScanLine[y]), fPixelWidth);
          end;
      else
        begin
          fAttributes.AddItem('ColorSpace', 'DeviceRGB');
          if not (bmp.PixelFormat in [pf24bit, pf32bit]) then
            NeedBitmap(pf24bit);
          if bmp.PixelFormat = pf24bit then
            pinc := 3
          else
            pinc := 4;
          for y := 0 to fPixelHeight - 1 do
            fWriter.AddRGB(bmp.{%H-}ScanLine[y], pinc, fPixelWidth);
          if (pinc = 3) and
             (bmp.TransparentMode = tmFixed) then
          begin
            // [ min1 max1 ... minn maxn ]
            transcolor := bmp.TransparentColor;
            fAttributes.AddItem('Mask', TPdfArray.CreateReals(nil,
              [(transcolor and $ff), (transcolor and $ff),
               (transcolor shr 8 and $ff), (transcolor shr 8 and $ff),
               (transcolor shr 16 and $ff), (transcolor shr 16 and $ff)]));
          end;
        end;
      end;
    finally
      if bmp <> aImage then
        bmp.Free;
    end;
  end;
  fAttributes.AddItem('Width', fPixelWidth);
  fAttributes.AddItem('Height', fPixelHeight);
  fAttributes.AddItem('BitsPerComponent', 8);
end;

constructor TPdfImage.CreateJpegDirect(aDoc: TPdfDocument;
  const aJpegFileName: TFileName; DontAddToFXref: boolean);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(aJpegFileName);
    CreateJpegDirect(aDoc, MS, DontAddToFXref);
  finally
    MS.Free;
  end;
end;

constructor TPdfImage.CreateJpegDirect(aDoc: TPdfDocument;
  aJpegFile: TMemoryStream; DontAddToFXref: boolean);
var
  len, bits: integer;
begin
  inherited Create(aDoc, DontAddToFXref);
  len := aJpegFile.Size;
  if not GetJpegSize(aJpegFile.Memory, len, fPixelWidth, fPixelHeight, bits) then
    exit; // JPEG format expected
  fAttributes.AddItem('Type', 'XObject');
  fAttributes.AddItem('Subtype', 'Image');
  fFilter := 'DCTDecode';
  fWriter.Save; // flush to allow direct access to fDestStream
  fWriter.Add(aJpegFile.Memory, len);
  fWriter.fDestStreamPosition := fWriter.fDestStream.Seek(0, soCurrent);
  fAttributes.AddItem('Width', fPixelWidth);
  fAttributes.AddItem('Height', fPixelHeight);
  case bits of
    8:
      fAttributes.AddItem('ColorSpace', 'DeviceGray');
    24:
      fAttributes.AddItem('ColorSpace', 'DeviceRGB');
  end;
  fAttributes.AddItem('BitsPerComponent', 8);
end;


{ TPdfFormWithCanvas }

constructor TPdfFormWithCanvas.Create(aDoc: TPdfDocument; W, H: integer);
var
  res: TPdfDictionary;
begin
  inherited Create(aDoc, true);
  res := TPdfDictionary.Create(aDoc.fXRef);
  fFontList := TPdfDictionary.Create(nil);
  res.AddItem('Font', fFontList);
  res.AddItem('ProcSet',
    TPdfArray.CreateNames(nil, ['PDF', 'Text', 'ImageC']));
  fPage := TPdfPage.Create(nil);
  fCanvas := TPdfCanvas.Create(aDoc);
  fCanvas.fPage := fPage;
  fCanvas.fPageFontList := fFontList;
  fCanvas.fContents := self;
  fCanvas.fFactor := 1;
  fAttributes.AddItem('Type', 'XObject');
  fAttributes.AddItem('Subtype', 'Form');
  fAttributes.AddItem('BBox', TPdfArray.Create(nil, [0, 0, W, H]));
  fAttributes.AddItem('Matrix', TPdfRawText.Create('[1 0 0 1 0 0]'));
  fAttributes.AddItem('Resources', res);
end;

destructor TPdfFormWithCanvas.Destroy;
begin
  CloseCanvas;
  inherited;
end;

procedure TPdfFormWithCanvas.CloseCanvas;
begin
  FreeAndNil(fCanvas);
  FreeAndNil(fPage);
end;


{$ifdef USE_PDFSECURITY}

{ TPdfEncryption }

constructor TPdfEncryption.Create(aLevel: TPdfEncryptionLevel;
  aPermissions: TPdfEncryptionPermissions;
  const aUserPassword, aOwnerPassword: string);
begin
  fLevel := aLevel;
  fPermissions := aPermissions;
  fUserPassword := aUserPassword;
  if aOwnerPassword = '' then
    EPdfInvalidOperation.RaiseUtf8(
      '% expect a non void owner password', [self]);
  fOwnerPassword := aOwnerPassword;
end;

class function TPdfEncryption.New(aLevel: TPdfEncryptionLevel;
  const aUserPassword, aOwnerPassword: string;
  aPermissions: TPdfEncryptionPermissions): TPdfEncryption;
begin
  case aLevel of
    elRC4_40,
      elRC4_128:
      result := TPdfEncryptionRC4MD5.Create(aLevel, aPermissions, aUserPassword,
        aOwnerPassword);
  else
    result := nil;
  end;
end;

procedure TPdfEncryption.AttachDocument(aDoc: TPdfDocument);
begin
  fDoc := aDoc;
end;


{ TPdfEncryptionRC4MD5 }

const
  // see "Algorithm 3.2 Computing an encryption key" in the PDF reference doc
  PDF_PADDING: TPdfBuffer32 = (
    $28, $BF, $4E, $5E, $4E, $75, $8A, $41, $64, $00, $4E,
    $56, $FF, $FA, $01, $08, $2E, $2E, $00, $B6, $D0, $68,
    $3E, $80, $2F, $0C, $A9, $FE, $64, $53, $69, $7A);

procedure TPdfEncryptionRC4MD5.AttachDocument(aDoc: TPdfDocument);

  procedure Pad(const source: string; var dest: TPdfBuffer32);
  var
    L: integer;
    tmp: WinAnsiString;
  begin
    tmp := StringToWinAnsi(source);
    L := Length(tmp);
    if L > SizeOf(dest) then
      L := SizeOf(dest)
    else
      MoveFast(PDF_PADDING, dest[L], SizeOf(dest) - L);
    MoveFast(pointer(tmp)^, dest, L);
  end;

const
  HASHSIZE: array[elRC4_40..elRC4_128] of integer = (
    5, 16);
  DICT: array[elRC4_40..elRC4_128] of
    record
      v, r, L: integer
    end = (
      (v: 1; r: 2; L: 40),
      (v: 2; r: 3; L: 128));
  FLAGPATTERN: array[elRC4_40..elRC4_128] of cardinal = (
    $FFFFFFC0, $FFFFF0C0);
  FLAGBIT: array[TPdfEncryptionPermission] of byte = (
    2, 3, 4, 5, 8, 9, 10, 11);
var
  rc4: TRC4;
  md5: TMD5;
  f: TPdfEncryptionPermission;
  dig, dig2: TMD5Digest;
  i, j: integer;
  own, usr: TPdfBuffer32;
begin
  inherited;
  // compute corresponding flags
  fFlags := FLAGPATTERN[fLevel];
  for f := low(f) to high(f) do
    if f in fPermissions then
      fFlags := fFlags or (1 shl FLAGBIT[f]);
  if fDoc.fFileFormat < pdf14 then
    fDoc.fFileFormat := pdf14;
  // calc fOwnerPass key (Algorithm 3.3 in PDF reference doc)
  Pad(fUserPassword, usr);
  Pad(fOwnerPassword, own);
  md5.Full(@own, SizeOf(own), dig);
  if fLevel = elRC4_128 then
    for i := 1 to 50 do
      md5.Full(@dig, SizeOf(dig), dig);
  rc4.Init(dig, HASHSIZE[fLevel]);
  rc4.Encrypt(usr, fOwnerPass, SizeOf(fOwnerPass));
  if fLevel = elRC4_128 then
    for i := 1 to 19 do
    begin
      for j := 0 to high(dig2) do
        dig2[j] := dig[j] xor i;
      rc4.Init(dig2, SizeOf(dig2));
      rc4.Encrypt(fOwnerPass, fOwnerPass, SizeOf(fOwnerPass));
    end;
  // calc main file key (Algorithm 3.2 in PDF reference doc)
  md5.Init;
  md5.Update(usr, SizeOf(usr));
  md5.Update(fOwnerPass, SizeOf(fOwnerPass));
  md5.Update(fFlags, SizeOf(fFlags));
  md5.Update(aDoc.fFileID, SizeOf(aDoc.fFileID));
  md5.final(dig);
  if fLevel = elRC4_128 then
    for i := 1 to 50 do
      md5.Full(@dig, SizeOf(dig), dig);
  SetLength(fInternalKey, HASHSIZE[fLevel]);
  MoveFast(dig, fInternalKey[0], HASHSIZE[fLevel]);
  // calc fUserPass content
  case fLevel of
    elRC4_40:
      begin   // Algorithm 3.4 in PDF reference doc
        rc4.Init(fInternalKey[0], HASHSIZE[fLevel]);
        rc4.Encrypt(PDF_PADDING, fUserPass, SizeOf(PDF_PADDING));
      end;
    elRC4_128:
      begin  // Algorithm 3.5 in PDF reference doc
        md5.Init;
        md5.Update(PDF_PADDING, SizeOf(PDF_PADDING));
        md5.Update(aDoc.fFileID, SizeOf(aDoc.fFileID));
        md5.final(dig);
        for i := 0 to 19 do
        begin
          for j := 0 to high(dig2) do
            dig2[j] := fInternalKey[j] xor i;
          rc4.Init(dig2, SizeOf(dig2));
          rc4.Encrypt(dig, dig, SizeOf(dig));
        end;
        MoveFast(dig, fUserPass, SizeOf(dig));
        MoveFast(dig, fUserPass[SizeOf(dig)], SizeOf(dig));
      end;
  end;
  // add encryption dictionary object
  if aDoc.fEncryptionObject = nil then
    aDoc.fEncryptionObject := TPdfDictionary.Create(aDoc.fXRef);
  with aDoc.fEncryptionObject, DICT[fLevel] do
  begin
    AddItem('Filter', 'Standard');
    AddItem('V', v);
    AddItem('R', r);
    AddItem('Length', L);
    AddItem('P', fFlags); // expected to be written as signed integer
    AddItem('O', TPdfClearText.Create(@fOwnerPass, SizeOf(fOwnerPass)));
    AddItem('U', TPdfClearText.Create(@fUserPass, SizeOf(fUserPass)));
  end;
  aDoc.fTrailer.Attributes.AddItem('Encrypt', aDoc.fEncryptionObject);
end;

procedure TPdfEncryptionRC4MD5.EncodeBuffer(
  const BufIn; var BufOut; Count: cardinal);
// see http://www.cs.cmu.edu/~dst/Adobe/Gallery/anon21jul01-pdf-encryption.txt
// see "Algorithm 3.1 Encryption of data" in PDF Reference document

  procedure ComputeNewRC4Key;
  const
    KEYSIZE: array[elRC4_40..elRC4_128] of integer = (10, 16);
  var
    md5: TMD5;
    dig: TMD5Digest;
  begin
    md5.Init;
    md5.Update(fInternalKey[0], length(fInternalKey));
    md5.Update(fDoc.fCurrentObjectNumber, 3);
    md5.Update(fDoc.fCurrentGenerationNumber, 2);
    md5.final(dig);
    fLastRC4Key.Init(dig, KEYSIZE[fLevel]);
    fLastObjectNumber := fDoc.fCurrentObjectNumber;
    fLastGenerationNumber := fDoc.fCurrentGenerationNumber;
  end;

var
  work: TRC4; // Encrypt() changes the RC4 state -> local copy for reuse
begin
  if (fDoc.fCurrentObjectNumber <> fLastObjectNumber) or
     (fDoc.fCurrentGenerationNumber <> fLastGenerationNumber) then
    // a lot of string encodings have the same context
    ComputeNewRC4Key;
  work := fLastRC4Key;
  work.Encrypt(BufIn, BufOut, Count); // RC4 allows in-place encryption :)
end;

{$endif USE_PDFSECURITY}


{************ TPdfDocumentGdi for GDI/TCanvas rendering support }

{$ifdef USE_METAFILE}

procedure SetGdiComment(h: HDC; pgc: TPdfGdiComment; data: pointer; len: PtrInt;
  const last: RawByteString = '');
var
  tmp: TSynTempAdder;
begin
  tmp.Init;
  tmp.AddDirect(AnsiChar(pgc));
  tmp.Add(data, len);
  tmp.Add(last);
  {$ifdef FPC}
  Windows.GdiComment(h, tmp.Size, PByte(tmp.Buffer)^);
  {$else}
  Windows.GdiComment(h, tmp.Size, tmp.Buffer);
  {$endif FPC}
  tmp.Store.Done;
end;

procedure GdiCommentBookmark(MetaHandle: HDC; const aBookmarkName: RawUtf8);
begin
  // high(TPdfGdiComment)<$47 so it will never begin with GDICOMMENT_IDENTIFIER
  SetGdiComment(MetaHandle, pgcBookmark, nil, 0, aBookMarkName);
end;

procedure GdiCommentOutline(MetaHandle: HDC; const aTitle: RawUtf8; aLevel: integer);
begin
  SetGdiComment(MetaHandle, pgcOutline, @aLevel, 4, aTitle);
end;

procedure GdiCommentLink(MetaHandle: HDC; const aBookmarkName: RawUtf8;
  const aRect: TRect; NoBorder: boolean);
const
  pgc: array[boolean] of TPdfGdiComment = (pgcLink, pgcLinkNoBorder);
begin
  SetGdiComment(MetaHandle, pgc[NoBorder], @aRect, SizeOf(aRect), aBookmarkName);
end;

procedure GdiCommentJpegDirect(MetaHandle: HDC; const aFileName: RawUtf8;
  const aRect: TRect);
begin
  SetGdiComment(MetaHandle, pgcJpegDirect, @aRect, SizeOf(aRect), aFileName);
end;

procedure GdiCommentBeginMarkContent(MetaHandle: HDC;
  Group: TPdfOptionalContentGroup);
begin
  SetGdiComment(MetaHandle, pgcBeginMarkContent, @Group, SizeOf(Group));
end;

procedure GdiCommentEndMarkContent(MetaHandle: HDC);
begin
  SetGdiComment(MetaHandle, pgcEndMarkContent, nil, 0);
end;


{ TPdfDocumentGdi }

function TPdfDocumentGdi.AddPage: TPdfPage;
begin
  if (fCanvas <> nil) and
     (fCanvas.fPage <> nil) then
    TPdfPageGdi(fCanvas.fPage).FlushVclCanvas;
  result := inherited AddPage;
  fCanvas.fContents.fSaveAtTheEnd := true; // as expected in SaveToStream() below
end;

constructor TPdfDocumentGdi.Create(AUseOutlines: boolean; ACodePage: integer;
  APdfA: TPdfALevel
  {$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption{$endif});
begin
  inherited;
  fTPdfPageClass := TPdfPageGdi;
  fUseMetaFileTextPositioning := tpSetTextJustification;
  fKerningHScaleBottom := 99.0;
  fKerningHScaleTop := 101.0;
end;

function TPdfDocumentGdi.GetVclCanvas: TCanvas;
begin
  with TPdfPageGdi(fCanvas.fPage) do
  begin
    if fVclCurrentCanvas = nil then
      CreateVclCanvas;
    result := fVclCurrentCanvas;
  end;
end;

function TPdfDocumentGdi.GetVclCanvasSize: TSize;
begin
  if (fCanvas <> nil) and
     (fCanvas.fPage <> nil) then
    with TPdfPageGdi(fCanvas.fPage) do
    begin
      if fVclCurrentCanvas = nil then
        CreateVclCanvas;
      result := fVclCanvasSize;
    end
  else
    Int64(result) := 0;
end;

procedure TPdfDocumentGdi.SaveToStream(AStream: TStream; ForceModDate: TDateTime);
var
  i: PtrInt;
  P: TPdfPageGdi;
begin
  // write the file header
  SaveToStreamDirectBegin(AStream, ForceModDate);
  // then draw the pages VCL/LCL Canvas content on the fly (miminal memory use)
  for i := 0 to fRawPages.Count - 1 do
  begin
    P := fRawPages.List[i];
    P.FlushVclCanvas;
    if P.fVclMetaFileCompressed <> '' then
    begin
      P.SetVclCurrentMetaFile;
      try
        fCanvas.SetPage(P);
        RenderMetaFile(fCanvas, P.fVclCurrentMetaFile, 1, 1, 0, 0,
          fUseMetaFileTextPositioning, KerningHScaleBottom, KerningHScaleTop,
          fUseMetaFileTextClipping);
      finally
        FreeAndNil(P.fVclCurrentMetaFile);
      end;
      inherited SaveToStreamDirectPageFlush;
    end;
  end;
  // finish to write PDF content to destination stream
  SaveToStreamDirectEnd;
end;

procedure TPdfDocumentGdi.SaveToStreamDirectPageFlush(FlushCurrentPageNow: boolean);
var
  P: TPdfPageGdi;
begin
  if fRawPages.Count > 0 then
  begin
    P := fRawPages.List[fRawPages.Count - 1];
    if (P = fCanvas.fPage) and
       (P.fVclMetaFileCompressed = '') and
       (P.fVclCurrentMetaFile <> nil) and
       (P.fVclCurrentCanvas <> nil) then
    begin
      FreeAndNil(P.fVclCurrentCanvas); // manual P.SetVclCurrentMetaFile
      try
        fCanvas.fContents.fSaveAtTheEnd := false; // force flush NOW
        RenderMetaFile(fCanvas, P.fVclCurrentMetaFile, 1, 1, 0, 0,
          fUseMetaFileTextPositioning, KerningHScaleBottom, KerningHScaleTop,
          fUseMetaFileTextClipping);
      finally
        FreeAndNil(P.fVclCurrentMetaFile);
      end;
    end;
  end;
  inherited SaveToStreamDirectPageFlush;
end;


{ TPdfPageGdi }

procedure TPdfPageGdi.SetVclCurrentMetaFile;
var
  tmp: RawByteString;
  str: TStream;
begin
  assert(fVclCurrentMetaFile = nil);
  fVclCurrentMetaFile := TMetaFile.Create;
  fVclCanvasSize.cx := MulDiv(PageWidth, fDoc.fScreenLogPixels, 72);
  fVclCanvasSize.cy := MulDiv(PageHeight, fDoc.fScreenLogPixels, 72);
  fVclCurrentMetaFile.Width := fVclCanvasSize.cx;
  fVclCurrentMetaFile.Height := fVclCanvasSize.cy;
  if fVclMetaFileCompressed <> '' then
  begin
    SetLength(tmp, SynLZdecompressdestlen(pointer(fVclMetaFileCompressed)));
    SynLZdecompress1(pointer(fVclMetaFileCompressed),
      length(fVclMetaFileCompressed), pointer(tmp));
    str := TRawByteStringStream.Create(tmp);
    try
      fVclCurrentMetaFile.LoadFromStream(str);
    finally
      str.Free;
    end;
  end;
end;

procedure TPdfPageGdi.CreateVclCanvas;
begin
  SetVclCurrentMetaFile;
  fVclCurrentCanvas := TMetaFileCanvas.Create(fVclCurrentMetaFile, fDoc.fDC);
end;

procedure TPdfPageGdi.FlushVclCanvas;
var
  str: TRawByteStringStream;
  len: integer;
begin
  if (self = nil) or
     (fVclCurrentCanvas = nil) then
    exit;
  FreeAndNil(fVclCurrentCanvas);
  assert(fVclCurrentMetaFile <> nil);
  str := TRawByteStringStream.Create;
  try
    fVclCurrentMetaFile.SaveToStream(str);
    len := Length(str.DataString);
    SetLength(fVclMetaFileCompressed, SynLZcompressdestlen(len));
    SetLength(fVclMetaFileCompressed, SynLZcompress1(
      pointer(str.DataString), len, pointer(fVclMetaFileCompressed)));
  finally
    str.Free;
  end;
  FreeAndNil(fVclCurrentMetaFile);
end;

destructor TPdfPageGdi.Destroy;
begin
  FreeAndNil(fVclCurrentCanvas);
  FreeAndNil(fVclCurrentMetaFile);
  inherited;
end;


{ TPdfForm }

constructor TPdfForm.Create(aDoc: TPdfDocumentGdi; aMetaFile: TMetafile);
var
  P: TPdfPageGdi;
  res: TPdfDictionary;
  w, h: integer;
  old: TPdfPage;
begin
  inherited Create(aDoc, true);
  w := aMetaFile.Width;
  h := aMetaFile.Height;
  P := TPdfPageGdi.Create(nil);
  try
    res := TPdfDictionary.Create(aDoc.fXRef);
    fFontList := TPdfDictionary.Create(nil);
    res.AddItem('Font', fFontList);
    res.AddItem('ProcSet',
      TPdfArray.CreateNames(nil, ['PDF', 'Text', 'ImageC']));
    with aDoc.fCanvas do
    begin
      old := fPage;
      fPage := P;
      try
        fPageFontList := fFontList;
        fContents := self;
        fPage.SetPageHeight(h);
        fFactor := 1;
        RenderMetaFile(aDoc.fCanvas, aMetaFile);
      finally
        if old <> nil then
          SetPage(old);
      end;
    end;
    fAttributes.AddItem('Type', 'XObject');
    fAttributes.AddItem('Subtype', 'Form');
    fAttributes.AddItem('BBox', TPdfArray.Create(nil, [0, 0, w, h]));
    fAttributes.AddItem('Matrix', TPdfRawText.Create('[1 0 0 1 0 0]'));
    fAttributes.AddItem('Resources', res);
  finally
    P.Free;
  end;
end;

type
  TFontSpec = packed record
    angle: SmallInt; // -360..+360
    ascent, descent, cell: SmallInt;
  end;

  TPdfEnumStatePen = record
    Null: boolean;
    Color, style: integer;
    Width: single;
  end;

  /// a state of the EMF enumeration engine, for the PDF canvas
  // - used also for the SaveDC/RestoreDC stack
  TPdfEnumState = record
    Position: TPoint;
    Moved: boolean;
    WinSize, ViewSize: TSize;
    WinOrg, ViewOrg: TPoint;
    //transformation and clipping
    WorldTransform: XFORM; //current
    MetaRgn: TPdfBox;      //clipping
    ClipRgn: TPdfBox;      //clipping
    ClipRgnNull: boolean;  //clipping
    MappingMode: integer;
    PolyFillMode: integer;
    StretchBltMode: integer;
    ArcDirection: integer;
    // current selected pen
    Pen: TPdfEnumStatePen;
    // current selected brush
    Brush: record
      Null: boolean;
      Color: integer;
      Style: integer;
    end;
    // current selected font
    Font: record
      Color: integer;
      Align: integer;
      BkMode, BkColor: integer;
      Spec: TFontSpec;
      LogFont: TLogFontW; // better be the last entry in TPdfEnumState record
    end;
  end;

  /// internal state machine used during EMF drawing
  // - contain the EMF enumeration engine state parameters
  TPdfEnum = class
  private
    fStrokeColor: integer;
    fFillColor: integer;
    fPenStyle: integer;
    fPenWidth: single;
    fInLined: boolean;
    fInitTransformMatrix: XFORM;
    fInitMetaRgn: TPdfBox;
    procedure SetFillColor(Value: integer);
    procedure SetStrokeColor(Value: integer);
  protected
    Canvas: TPdfCanvas;
    // the pen/font/brush objects table, indexed like the THandleTable
    Obj: array of record
      case kind: integer of
        OBJ_PEN:
          (PenColor: integer;
           PenStyle: integer;
           PenWidth: single);
        OBJ_FONT:
          (FontSpec: TFontSpec;
           LogFont: TLogFontW);
        OBJ_BRUSH:
          (BrushColor: integer;
           BrushNull: boolean;
           BrushStyle: integer);
    end;
    // SaveDC/RestoreDC stack
    nDC: integer;
    DC: array[0..31] of TPdfEnumState;
  public
    constructor Create(ACanvas: TPdfCanvas);
    procedure SaveDC;
    procedure RestoreDC;
    procedure NeedPen;
    procedure NeedBrushAndPen;
    procedure FlushPenBrush;
    procedure SelectObjectFromIndex(iObject: integer);
    procedure TextOut(var r: TEMRExtTextOut);
    procedure ScaleMatrix(Custom: PXForm; iMode: integer);
    procedure HandleComment(Kind: TPdfGdiComment; P: PAnsiChar; Len: integer);
    procedure CreateFont(ALogFont: PEMRExtCreateFontIndirect);
    // if Canvas.Doc.JPEGCompression<>0, draw not as a bitmap but jpeg encoded
    procedure DrawBitmap(xs, ys, ws, hs, xd, yd, wd, hd, usage: integer;
      Bmi: PBitmapInfo; bits: pointer; clipRect: PRect; xSrcTransform: PXForm;
      dwRop: DWord; transparent: TPdfColorRGB = $FFFFFFFF);
    procedure FillRectangle(const Rect: TRect; ResetNewPath: boolean);
    // the current value set to SetRGBFillColor (rg)
    property FillColor: integer
      read fFillColor write SetFillColor;
    // the current value set to SetRGBStrokeColor (RG)
    property StrokeColor: integer
      read fStrokeColor write SetStrokeColor;
    // WorldTransform
    property InitTransformMatrix: XFORM
      read fInitTransformMatrix write fInitTransformMatrix;
    // MetaRgn - clipping
    procedure InitMetaRgn(const ClientRect: TRect);
    procedure SetMetaRgn;
    // intersect - clipping
    function IntersectClipRect(
      const ClpRect: TPdfBox; const CurrRect: TPdfBox): TPdfBox;
    procedure ExtSelectClipRgn(data: PEMRExtSelectClipRgn);
    // get current clipping area
    function GetClipRect: TPdfBox;
    procedure GradientFill(Data: PEMGradientFill);
    procedure PolyPoly(Data: PEMRPolyPolygon; iType: integer);
  end;

const
  STOCKBRUSHCOLOR: array[WHITE_BRUSH..BLACK_BRUSH] of integer = (
    clWhite, $AAAAAA, $808080, $666666, clBlack);
  STOCKPENCOLOR: array[WHITE_PEN..BLACK_PEN] of integer = (
    clWhite, clBlack);

function CenterPoint(const Rect: TRect): TPoint;
  {$ifdef HASINLINE} inline;{$endif}
begin
  result.X := (Rect.Right + Rect.Left) div 2;
  result.Y := (Rect.Bottom + Rect.Top) div 2;
end;

/// EMF enumeration callback function, called from GDI
// - draw most content on PDF canvas (do not render 100% GDI content yet)
function EnumEMFFunc(DC: HDC; var Table: THandleTable; R: PEnhMetaRecord;
  NumObjects: DWord; E: TPdfEnum): LongBool; stdcall;
var
  i: PtrInt;
  InitTransX: XForm;
  polytypes: PByteArray;
begin
  result := true;
  with E.DC[E.nDC] do
    case R^.iType of
      EMR_HEADER:
        begin
          SetLength(E.obj, PEnhMetaHeader(R)^.nHandles);
          WinOrg.X := 0;
          WinOrg.Y := 0;
          ViewOrg.X := 0;
          ViewOrg.Y := 0;
          MappingMode := GetMapMode(DC);
          PolyFillMode := GetPolyFillMode(DC);
          StretchBltMode := GetStretchBltMode(DC);
          ArcDirection := AD_COUNTERCLOCKWISE;
          InitTransX := DefaultIdentityMatrix;
          E.InitTransformMatrix := InitTransX;
          E.ScaleMatrix(@InitTransX, MWT_SET); // keep init
          E.InitMetaRgn(TRect(PEnhMetaHeader(R)^.rclBounds));
        end;
      EMR_SETWINDOWEXTEX:
        WinSize := PEMRSetWindowExtEx(R)^.szlExtent;
      EMR_SETWINDOWORGEX:
        WinOrg := PEMRSetWindowOrgEx(R)^.ptlOrigin;
      EMR_SETVIEWPORTEXTEX:
        ViewSize := PEMRSetViewPortExtEx(R)^.szlExtent;
      EMR_SETVIEWPORTORGEX:
        ViewOrg := PEMRSetViewPortOrgEx(R)^.ptlOrigin;
      EMR_SETBKMODE:
        Font.BkMode := PEMRSetBkMode(R)^.iMode;
      EMR_SETBKCOLOR:
        if PEMRSetBkColor(R)^.crColor = cardinal(clNone) then
          Font.BkColor := 0
        else
          Font.BkColor := PEMRSetBkColor(R)^.crColor;
      EMR_SETTEXTCOLOR:
        if PEMRSetTextColor(R)^.crColor = cardinal(clNone) then
          Font.Color := 0
        else
          Font.Color := PEMRSetTextColor(R)^.crColor;
      EMR_SETTEXTALIGN:
        Font.Align := PEMRSetTextAlign(R)^.iMode;
      EMR_EXTTEXTOUTA,
      EMR_EXTTEXTOUTW:
        E.TextOut(PEMRExtTextOut(R)^);
      EMR_SAVEDC:
        E.SaveDC;
      EMR_RESTOREDC:
        E.RestoreDC;
      EMR_SETWORLDTRANSFORM:
        E.ScaleMatrix(@PEMRSetWorldTransform(R)^.xform, MWT_SET);
      EMR_CREATEPEN:
        with PEMRCreatePen(R)^ do
          if ihPen - 1 < cardinal(length(E.Obj)) then
            with E.obj[ihPen - 1] do
            begin
              kind := OBJ_PEN;
              PenColor := lopn.lopnColor;
              PenWidth := lopn.lopnWidth.X;
              PenStyle := lopn.lopnStyle;
            end;
      EMR_CREATEBRUSHINDIRECT:
        with PEMRCreateBrushIndirect(R)^ do
          if ihBrush - 1 < cardinal(length(E.Obj)) then
            with E.obj[ihBrush - 1] do
            begin
              kind := OBJ_BRUSH;
              BrushColor := lb.lbColor;
              BrushNull := (lb.lbStyle = BS_NULL);
              BrushStyle := lb.lbStyle;
            end;
      EMR_EXTCREATEFONTINDIRECTW:
        E.CreateFont(PEMRExtCreateFontIndirect(R));
      EMR_DELETEOBJECT:
        with PEMRDeleteObject(R)^ do
          if ihObject - 1 < cardinal(length(E.Obj)) then // avoid GPF
            E.obj[ihObject - 1].kind := 0;
      EMR_SELECTOBJECT:
        E.SelectObjectFromIndex(PEMRSelectObject(R)^.ihObject);
      EMR_MOVETOEX:
        begin
          position := PEMRMoveToEx(R)^.ptl; // temp var to ignore unused moves
          if E.Canvas.fNewPath then
          begin
            E.Canvas.MoveToI(position.X, position.Y);
            Moved := true;
          end
          else
            Moved := false;
        end;
      EMR_LINETO:
        begin
          E.NeedPen;
          if not E.Canvas.fNewPath and
             not Moved then
            E.Canvas.MoveToI(position.X, position.Y);
          E.Canvas.LineToI(PEMRLineTo(R)^.ptl.X, PEMRLineTo(R)^.ptl.Y);
          position := PEMRLineTo(R)^.ptl;
          Moved := false;
          E.fInLined := true;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
        end;
      EMR_RECTANGLE,
      EMR_ELLIPSE:
        begin
          E.NeedBrushAndPen;
          with E.Canvas.BoxI(TRect(PEMRRectangle(R)^.rclBox), true) do
            case R^.iType of
              EMR_RECTANGLE:
                E.Canvas.Rectangle(Left, Top, Width, Height);
              EMR_ELLIPSE:
                E.Canvas.Ellipse(Left, Top, Width, Height);
            end;
          E.FlushPenBrush;
        end;
      EMR_ROUNDRECT:
        begin
          NormalizeRect(PRect(@PEMRRoundRect(R)^.rclBox)^);
          E.NeedBrushAndPen;
          with PEMRRoundRect(R)^ do
            E.Canvas.RoundRectI(rclBox.left, rclBox.top, rclBox.right,
              rclBox.bottom, szlCorner.cx, szlCorner.cy);
          E.FlushPenBrush;
        end;
      EMR_ARC:
        begin
          NormalizeRect(PRect(@PEMRARC(R)^.rclBox)^);
          E.NeedPen;
          with PEMRARC(R)^, CenterPoint(TRect(rclBox)) do
            E.Canvas.ArcI(X, Y, rclBox.Right - rclBox.Left,
              rclBox.Bottom - rclBox.Top, ptlStart.x, ptlStart.y,
              ptlEnd.x, ptlEnd.y, E.dc[E.nDC].ArcDirection = AD_CLOCKWISE,
              acArc, position);
          E.Canvas.Stroke;
        end;
      EMR_ARCTO:
        begin
          NormalizeRect(PRect(@PEMRARCTO(R)^.rclBox)^);
          E.NeedPen;
          if not E.Canvas.fNewPath and
             not Moved then
            E.Canvas.MoveToI(position.X, position.Y);
          with PEMRARC(R)^, CenterPoint(TRect(rclBox)) do
          begin
            // E.Canvas.LineTo(ptlStart.x, ptlStart.y);
            E.Canvas.ArcI(X, Y, rclBox.Right - rclBox.Left,
              rclBox.Bottom - rclBox.Top, ptlStart.x, ptlStart.y,
              ptlEnd.x, ptlEnd.y, E.dc[E.nDC].ArcDirection = AD_CLOCKWISE,
              acArcTo, position);
            Moved := false;
            E.fInLined := true;
            if not E.Canvas.fNewPath then
              if not pen.null then
                E.Canvas.Stroke;
          end;
        end;
      EMR_PIE:
        begin
          NormalizeRect(PRect(@PEMRPie(R)^.rclBox)^);
          E.NeedBrushAndPen;
          with PEMRPie(R)^, CenterPoint(TRect(rclBox)) do
            E.Canvas.ArcI(X, Y, rclBox.Right - rclBox.Left,
              rclBox.Bottom - rclBox.Top, ptlStart.x, ptlStart.y,
              ptlEnd.x, ptlEnd.y, E.dc[E.nDC].ArcDirection = AD_CLOCKWISE,
              acPie, position);
          if pen.null then
            E.Canvas.Fill
          else
            E.Canvas.FillStroke;
        end;
      EMR_CHORD:
        begin
          NormalizeRect(PRect(@PEMRChord(R)^.rclBox)^);
          E.NeedBrushAndPen;
          with PEMRChord(R)^, CenterPoint(TRect(rclBox)) do
            E.Canvas.ArcI(X, Y, rclBox.Right - rclBox.Left,
              rclBox.Bottom - rclBox.Top, ptlStart.x, ptlStart.y,
              ptlEnd.x, ptlEnd.y, E.dc[E.nDC].ArcDirection = AD_CLOCKWISE,
              acChoord, position);
          if pen.null then
            E.Canvas.Fill
          else
            E.Canvas.FillStroke;
        end;
      EMR_FILLRGN:
        begin
          E.SelectObjectFromIndex(PEMRFillRgn(R)^.ihBrush);
          E.NeedBrushAndPen;
          E.FillRectangle(
            TRect(PRgnDataHeader(@PEMRFillRgn(R)^.RgnData[0])^.rcBound), false);
        end;
      EMR_POLYGON,
      EMR_POLYLINE,
      EMR_POLYGON16,
      EMR_POLYLINE16:
        if not brush.null or
           not pen.null then
        begin
          if R^.iType in [EMR_POLYGON, EMR_POLYGON16] then
            E.NeedBrushAndPen
          else
            E.NeedPen;
          if R^.iType in [EMR_POLYGON, EMR_POLYLINE] then
          begin
            E.Canvas.MoveToI(PEMRPolyLine(R)^.aptl[0].x, PEMRPolyLine(R)^.aptl[0].Y);
            for i := 1 to PEMRPolyLine(R)^.cptl - 1 do
              E.Canvas.LineToI(PEMRPolyLine(R)^.aptl[i].x, PEMRPolyLine(R)^.aptl[i].Y);
            if PEMRPolyLine(R)^.cptl > 0 then
              position := PEMRPolyLine(R)^.aptl[PEMRPolyLine(R)^.cptl - 1]
            else
              position := PEMRPolyLine(R)^.aptl[0];
          end
          else
          begin
            E.Canvas.MoveToI(PEMRPolyLine16(R)^.apts[0].x, PEMRPolyLine16(R)^.apts[0].Y);
            if PEMRPolyLine16(R)^.cpts > 0 then
            begin
              for i := 1 to PEMRPolyLine16(R)^.cpts - 1 do
                E.Canvas.LineToI(
                  PEMRPolyLine16(R)^.apts[i].x, PEMRPolyLine16(R)^.apts[i].Y);
              with PEMRPolyLine16(R)^.apts[PEMRPolyLine16(R)^.cpts - 1] do
              begin
                position.X := x;
                position.Y := Y;
              end;
            end
            else
            begin
              position.X := PEMRPolyLine16(R)^.apts[0].x;
              position.Y := PEMRPolyLine16(R)^.apts[0].Y;
            end;
          end;
          Moved := false;
          if R^.iType in [EMR_POLYGON, EMR_POLYGON16] then
          begin
            E.Canvas.Closepath;
            E.FlushPenBrush;
          end
          else if not pen.null then
            E.Canvas.Stroke
          else // for lines
            E.Canvas.NewPath;
        end;
      EMR_POLYPOLYGON,
      EMR_POLYPOLYGON16,
      EMR_POLYPOLYLINE,
      EMR_POLYPOLYLINE16:
        E.PolyPoly(PEMRPolyPolygon(R), R^.iType);
      EMR_POLYBEZIER:
        begin
          if not pen.null then
            E.NeedPen;
          E.Canvas.MoveToI(
            PEMRPolyBezier(R)^.aptl[0].x, PEMRPolyBezier(R)^.aptl[0].Y);
          for i := 0 to (PEMRPolyBezier(R)^.cptl div 3) - 1 do
            E.Canvas.CurveToCI(
              PEMRPolyBezier(R)^.aptl[i * 3 + 1].X,
              PEMRPolyBezier(R)^.aptl[i * 3 + 1].Y,
              PEMRPolyBezier(R)^.aptl[i * 3 + 2].X,
              PEMRPolyBezier(R)^.aptl[i * 3 + 2].Y,
              PEMRPolyBezier(R)^.aptl[i * 3 + 3].X,
              PEMRPolyBezier(R)^.aptl[i * 3 + 3].Y);
          if PEMRPolyBezier(R)^.cptl > 0 then
            position := PEMRPolyBezier(R)^.aptl[PEMRPolyBezier(R)^.cptl - 1]
          else
            position := PEMRPolyBezier(R)^.aptl[0];
          Moved := false;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
            else
              E.Canvas.NewPath;
        end;
      EMR_POLYBEZIER16:
        begin
          if not pen.null then
            E.NeedPen;
          E.Canvas.MoveToI(
            PEMRPolyBezier16(R)^.apts[0].x, PEMRPolyBezier16(R)^.apts[0].Y);
          if PEMRPolyBezier16(R)^.cpts > 0 then
          begin
            for i := 0 to (PEMRPolyBezier16(R)^.cpts div 3) - 1 do
              E.Canvas.CurveToCI(
                PEMRPolyBezier16(R)^.apts[i * 3 + 1].X,
                PEMRPolyBezier16(R)^.apts[i * 3 + 1].Y,
                PEMRPolyBezier16(R)^.apts[i * 3 + 2].X,
                PEMRPolyBezier16(R)^.apts[i * 3 + 2].Y,
                PEMRPolyBezier16(R)^.apts[i * 3 + 3].X,
                PEMRPolyBezier16(R)^.apts[i * 3 + 3].Y);
            with PEMRPolyBezier16(R)^.apts[PEMRPolyBezier16(R)^.cpts - 1] do
            begin
              position.X := x;
              position.Y := Y;
            end;
          end
          else
          begin
            position.X := PEMRPolyBezier16(R)^.apts[0].x;
            position.Y := PEMRPolyBezier16(R)^.apts[0].Y;
          end;
          Moved := false;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
            else
              E.Canvas.NewPath;
        end;
      EMR_POLYBEZIERTO:
        begin
          if not pen.null then
            E.NeedPen;
          if not E.Canvas.fNewPath then
            if not Moved then
              E.Canvas.MoveToI(position.X, position.Y);
          if PEMRPolyBezierTo(R)^.cptl > 0 then
          begin
            for i := 0 to (PEMRPolyBezierTo(R)^.cptl div 3) - 1 do
              E.Canvas.CurveToCI(
                PEMRPolyBezierTo(R)^.aptl[i * 3].X,
                PEMRPolyBezierTo(R)^.aptl[i * 3].Y,
                PEMRPolyBezierTo(R)^.aptl[i * 3 + 1].X,
                PEMRPolyBezierTo(R)^.aptl[i * 3 + 1].Y,
                PEMRPolyBezierTo(R)^.aptl[i * 3 + 2].X,
                PEMRPolyBezierTo(R)^.aptl[i * 3 + 2].Y);
            position := PEMRPolyBezierTo(R)^.aptl[PEMRPolyBezierTo(R)^.cptl - 1];
          end;
          Moved := false;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
            else
              E.Canvas.NewPath;
        end;
      EMR_POLYBEZIERTO16:
        begin
          if not pen.null then
            E.NeedPen;
          if not E.Canvas.fNewPath then
            if not Moved then
              E.Canvas.MoveToI(position.X, position.Y);
          if PEMRPolyBezierTo16(R)^.cpts > 0 then
          begin
            for i := 0 to (PEMRPolyBezierTo16(R)^.cpts div 3) - 1 do
              E.Canvas.CurveToCI(
                PEMRPolyBezierTo16(R)^.apts[i * 3].X,
                PEMRPolyBezierTo16(R)^.apts[i * 3].Y,
                PEMRPolyBezierTo16(R)^.apts[i * 3 + 1].X,
                PEMRPolyBezierTo16(R)^.apts[i * 3 + 1].Y,
                PEMRPolyBezierTo16(R)^.apts[i * 3 + 2].X,
                PEMRPolyBezierTo16(R)^.apts[i * 3 + 2].Y);
            with PEMRPolyBezierTo16(R)^.apts[PEMRPolyBezierTo16(R)^.cpts - 1] do
            begin
              position.X := x;
              position.Y := Y;
            end;
          end;
          Moved := false;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
            else
              E.Canvas.NewPath;
        end;
      EMR_POLYLINETO,
      EMR_POLYLINETO16:
        begin
          if not pen.null then
            E.NeedPen;
          if not E.Canvas.fNewPath then
          begin
            E.Canvas.NewPath;
            if not Moved then
              E.Canvas.MoveToI(position.X, position.Y);
          end;
          if R^.iType = EMR_POLYLINETO then
          begin
            if PEMRPolyLineTo(R)^.cptl > 0 then
            begin
              for i := 0 to PEMRPolyLineTo(R)^.cptl - 1 do
                with PEMRPolyLineTo(R)^.aptl[i] do
                  E.Canvas.LineToI(X, Y);
              position := PEMRPolyLineTo(R)^.aptl[PEMRPolyLineTo(R)^.cptl - 1];
            end;
          end
          else
          // EMR_POLYLINETO16
          if PEMRPolyLineTo16(R)^.cpts > 0 then
          begin
            for i := 0 to PEMRPolyLineTo16(R)^.cpts - 1 do
              with PEMRPolyLineTo16(R)^.apts[i] do
                E.Canvas.LineToI(X, Y);
            with PEMRPolyLineTo16(R)^.apts[PEMRPolyLineTo16(R)^.cpts - 1] do
            begin
              position.X := x;
              position.Y := Y;
            end;
          end;
          Moved := false;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
            else
              E.Canvas.NewPath;
        end;
      EMR_POLYDRAW:
        if PEMRPolyDraw(R)^.cptl > 0 then
        begin
          if not pen.null then
            E.NeedPen;
          polytypes := @PEMRPolyDraw(R)^.aptl[PEMRPolyDraw(R)^.cptl];
          i := 0;
          while i < integer(PEMRPolyDraw(R)^.cptl) do
          begin
            case polytypes^[i] and not PT_CLOSEFIGURE of
              PT_LINETO:
                begin
                  with PEMRPolyDraw(R)^.aptl[i] do
                    E.Canvas.LineToI(X, Y);
                  if polytypes^[i] and PT_CLOSEFIGURE <> 0 then
                  begin
                    E.Canvas.LineToI(position.X, position.Y);
                    position := PEMRPolyDraw(R)^.aptl[i];
                  end;
                end;
              PT_BEZIERTO:
                begin
                  E.Canvas.CurveToCI(
                    PEMRPolyDraw(R)^.aptl[i + 1].X,
                    PEMRPolyDraw(R)^.aptl[i + 1].Y,
                    PEMRPolyDraw(R)^.aptl[i + 2].X,
                    PEMRPolyDraw(R)^.aptl[i + 2].Y,
                    PEMRPolyDraw(R)^.aptl[i + 3].X,
                    PEMRPolyDraw(R)^.aptl[i + 3].Y);
                  inc(i, 3);
                  if polytypes^[i] and PT_CLOSEFIGURE <> 0 then
                  begin
                    E.Canvas.LineToI(position.X, position.Y);
                    position := PEMRPolyDraw(R)^.aptl[i];
                  end;
                end;
              PT_MOVETO:
                begin
                  with PEMRPolyDraw(R)^.aptl[i] do
                    E.Canvas.MoveToI(X, Y);
                  position := PEMRPolyDraw(R)^.aptl[i];
                end;
            else
              break; // invalid type
            end;
            inc(i);
          end;
          position := PEMRPolyDraw(R)^.aptl[PEMRPolyDraw(R)^.cptl - 1];
          Moved := false;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
            else
              E.Canvas.NewPath;
        end;
      EMR_POLYDRAW16:
        if PEMRPolyDraw16(R)^.cpts > 0 then
        begin
          if not pen.null then
            E.NeedPen;
          polytypes := @PEMRPolyDraw16(R)^.apts[PEMRPolyDraw16(R)^.cpts];
          i := 0;
          while i < integer(PEMRPolyDraw16(R)^.cpts) do
          begin
            case polytypes^[i] and not PT_CLOSEFIGURE of
              PT_LINETO:
                begin
                  with PEMRPolyDraw16(R)^.apts[i] do
                    E.Canvas.LineToI(X, Y);
                  if polytypes^[i] and PT_CLOSEFIGURE <> 0 then
                  begin
                    E.Canvas.LineToI(position.X, position.Y);
                    with PEMRPolyDraw16(R)^.apts[i] do
                    begin
                      position.X := x;
                      position.Y := Y;
                    end;
                  end;
                end;
              PT_BEZIERTO:
                begin
                  E.Canvas.CurveToCI(
                    PEMRPolyDraw16(R)^.apts[i + 1].X,
                    PEMRPolyDraw16(R)^.apts[i + 1].Y,
                    PEMRPolyDraw16(R)^.apts[i + 2].X,
                    PEMRPolyDraw16(R)^.apts[i + 2].Y,
                    PEMRPolyDraw16(R)^.apts[i + 3].X,
                    PEMRPolyDraw16(R)^.apts[i + 3].Y);
                  inc(i, 3);
                  if polytypes^[i] and PT_CLOSEFIGURE <> 0 then
                  begin
                    E.Canvas.LineToI(position.X, position.Y);
                    with PEMRPolyDraw16(R)^.apts[i] do
                    begin
                      position.X := X;
                      position.Y := Y;
                    end;
                  end;
                end;
              PT_MOVETO:
                begin
                  with PEMRPolyDraw16(R)^.apts[i] do
                  begin
                    E.Canvas.MoveToI(X, Y);
                    position.X := X;
                    position.Y := Y;
                  end;
                end;
            else
              break; // invalid type
            end;
            inc(i);
          end;
          with PEMRPolyDraw16(R)^.apts[PEMRPolyDraw16(R)^.cpts - 1] do
          begin
            position.X := X;
            position.Y := Y;
          end;
          Moved := false;
          if not E.Canvas.fNewPath then
            if not pen.null then
              E.Canvas.Stroke
            else
              E.Canvas.NewPath;
        end;
      EMR_BITBLT:
        begin
          with PEMRBitBlt(R)^ do // only handle RGB bitmaps (no palette)
            if (offBmiSrc <> 0) and
               (offBitsSrc <> 0) then
              E.DrawBitmap(xSrc, ySrc, cxDest, cyDest, xDest, yDest,
                cxDest, cyDest, iUsageSrc, pointer(PtrUInt(R) + offBmiSrc),
                pointer(PtrUInt(R) + offBitsSrc), @rclBounds, @xformSrc, dwRop)
            else
              case dwRop of // we only handle PATCOPY = fillrect
                PATCOPY:
                  E.FillRectangle(Rect(xDest, yDest,
                    xDest + cxDest, yDest + cyDest), true);
              end;
        end;
      EMR_STRETCHBLT:
        begin
          with PEMRStretchBlt(R)^ do // only handle RGB bitmaps (no palette)
            if (offBmiSrc <> 0) and
               (offBitsSrc <> 0) then
              E.DrawBitmap(xSrc, ySrc, cxSrc, cySrc, xDest, yDest, cxDest,
                cyDest, iUsageSrc, pointer(PtrUInt(R) + offBmiSrc),
                pointer(PtrUInt(R) + offBitsSrc), @rclBounds, @xformSrc, dwRop)
            else
              case dwRop of // we only handle PATCOPY = fillrect
                PATCOPY:
                  E.FillRectangle(Rect(
                    xDest, yDest, xDest + cxDest, yDest + cyDest), true);
              end;
        end;
      EMR_STRETCHDIBITS:
        with PEMRStretchDIBits(R)^ do // only handle RGB bitmaps (no palette)
          if (offBmiSrc <> 0) and
             (offBitsSrc <> 0) then
          begin
            if WorldTransform.eM22 < 0 then
              with PBitmapInfo(PtrUInt(R) + offBmiSrc)^ do
                bmiHeader.biHeight := -bmiHeader.biHeight;
            E.DrawBitmap(xSrc, ySrc, cxSrc, cySrc, xDest, yDest, cxDest, cyDest,
              iUsageSrc, pointer(PtrUInt(R) + offBmiSrc),
              pointer(PtrUInt(R) + offBitsSrc), @rclBounds, nil, dwRop);
          end;
      EMR_TRANSPARENTBLT:
        with PEMRTransparentBLT(R)^ do // only handle RGB bitmaps (no palette)
          if (offBmiSrc <> 0) and
             (offBitsSrc <> 0) then
            E.DrawBitmap(xSrc, ySrc, cxSrc, cySrc, xDest, yDest, cxDest, cyDest,
              iUsageSrc, pointer(PtrUInt(R) + offBmiSrc),
              pointer(PtrUInt(R) + offBitsSrc), @rclBounds, @xformSrc, SRCCOPY,
              dwRop); // dwRop stores the transparent color
      EMR_ALPHABLEND:
        with PEMRAlphaBlend(R)^ do // only handle RGB bitmaps (no palette nor transparency)
          if (offBmiSrc <> 0) and
             (offBitsSrc <> 0) then
            E.DrawBitmap (xSrc, ySrc, cxSrc, cySrc, xDest, yDest, cxDest, cyDest,
              iUsageSrc, pointer(PtrUInt(R) + offBmiSrc), pointer(
               PtrUInt(R) + offBitsSrc), @rclBounds, @xformSrc, SRCCOPY, dwRop)
          else
            case dwRop of // we only handle PATCOPY = fillrect
              PATCOPY:
                E.FillRectangle(Rect(xDest, yDest,
                  xDest + cxDest, yDest + cyDest), true);
            end;
      EMR_GDICOMMENT:
        with PEMRGDIComment(R)^ do
          if cbData >= 1  then
            E.HandleComment(
              TPdfGdiComment(Data[0]), PAnsiChar(@Data) + 1, cbData - 1);
      EMR_MODIFYWORLDTRANSFORM:
        with PEMRModifyWorldTransform(R)^ do
          E.ScaleMatrix(@xform, iMode);
      EMR_EXTCREATEPEN: // approx. - fast solution
        with PEMRExtCreatePen(R)^ do
          if ihPen - 1 < cardinal(length(E.Obj)) then
            with E.obj[ihPen - 1] do
            begin
              kind := OBJ_PEN;
              PenColor := elp.elpColor;
              PenWidth := elp.elpWidth;
              PenStyle := elp.elpPenStyle and (PS_STYLE_MASK or PS_ENDCAP_MASK);
            end;
      EMR_SETMITERLIMIT:
        if PEMRSetMiterLimit(R)^.eMiterLimit > 0.1 then
          E.Canvas.SetMiterLimit(PEMRSetMiterLimit(R)^.eMiterLimit);
      EMR_SETMETARGN:
        E.SetMetaRgn;
      EMR_EXTSELECTCLIPRGN:
        E.ExtSelectClipRgn(PEMRExtSelectClipRgn(R));
      EMR_INTERSECTCLIPRECT:
        ClipRgn := E.IntersectClipRect(E.Canvas.BoxI(
          TRect(PEMRIntersectClipRect(R)^.rclClip), true), ClipRgn);
      EMR_SETMAPMODE:
        MappingMode := PEMRSetMapMode(R)^.iMode;
      EMR_BEGINPATH:
        begin
          E.Canvas.NewPath;
          if not Moved then
          begin
            E.Canvas.MoveToI(position.X, position.Y);
            Moved := true;
          end;
        end;
      EMR_ENDPATH:
        E.Canvas.fNewPath := false;
      EMR_ABORTPATH:
        begin
          E.Canvas.NewPath;
          E.Canvas.fNewPath := false;
        end;
      EMR_CLOSEFIGURE:
        E.Canvas.ClosePath;
      EMR_FILLPATH:
        begin
          if not brush.Null then
          begin
            E.FillColor := brush.color;
            E.Canvas.Fill;
          end;
          E.Canvas.NewPath;
          E.Canvas.fNewPath := false;
        end;
      EMR_STROKEPATH:
        begin
          if not pen.null then
          begin
            E.NeedPen;
            E.Canvas.Stroke;
          end;
          E.Canvas.NewPath;
          E.Canvas.fNewPath := false;
        end;
      EMR_STROKEANDFILLPATH:
        begin
          if not brush.Null then
          begin
            E.NeedPen;
            E.FillColor := brush.color;
            if not pen.null then
              if PolyFillMode = ALTERNATE then
                E.Canvas.EofillStroke
              else
                E.Canvas.FillStroke
            else if PolyFillMode = ALTERNATE then
              E.Canvas.EoFill
            else
              E.Canvas.Fill
          end
          else if not pen.null then
          begin
            E.NeedPen;
            E.Canvas.Stroke;
          end;
          E.Canvas.NewPath;
          E.Canvas.fNewPath := false;
        end;
      EMR_SETPOLYFILLMODE:
        PolyFillMode := PEMRSetPolyFillMode(R)^.iMode;
      EMR_GRADIENTFILL:
        E.GradientFill(PEMGradientFill(R));
      EMR_SETSTRETCHBLTMODE:
        StretchBltMode := PEMRSetStretchBltMode(R)^.iMode;
      EMR_SETARCDIRECTION:
        ArcDirection := PEMRSetArcDirection(R)^.iArcDirection;
      EMR_SETPIXELV:
        begin
          // prepare pixel size and color
          if pen.width <> 1 then
          begin
            E.fPenWidth := E.Canvas.fWorldFactorX * E.Canvas.fDevScaleX;
            E.Canvas.SetLineWidth(E.fPenWidth * E.Canvas.fFactorX);
          end;
          if PEMRSetPixelV(R)^.crColor <> cardinal(pen.color) then
            E.Canvas.SetRGBStrokeColor(PEMRSetPixelV(R)^.crColor);
          // draw point
          position := TPoint(Point(PEMRSetPixelV(R)^.ptlPixel.X, PEMRSetPixelV(R)
            ^.ptlPixel.Y));
          E.Canvas.PointI(position.X, position.Y);
          E.Canvas.Stroke;
          Moved := false;
          // rollback pixel size and color
          if pen.width <> 1 then
          begin
            E.fPenWidth := pen.width * E.Canvas.fWorldFactorX * E.Canvas.fDevScaleX;
            E.Canvas.SetLineWidth(E.fPenWidth * E.Canvas.fFactorX);
          end;
          if PEMRSetPixelV(R)^.crColor <> cardinal(pen.color) then
            E.Canvas.SetRGBStrokeColor(pen.color);
        end;
     // TBD
      EMR_SMALLTEXTOUT,
      EMR_SETROP2,
      EMR_ALPHADIBBLEND,
      EMR_SETBRUSHORGEX,
      EMR_SETICMMODE,
      EMR_SELECTPALETTE,
      EMR_CREATEPALETTE,
      EMR_SETPALETTEENTRIES,
      EMR_RESIZEPALETTE,
      EMR_REALIZEPALETTE,
      EMR_EOF:
        ; //do nothing
    else
      R^.iType := R^.iType; // for debug purpose (breakpoint)
    end;
  case R^.iType of
    EMR_RESTOREDC,
    EMR_SETWINDOWEXTEX,
    EMR_SETWINDOWORGEX,
    EMR_SETVIEWPORTEXTEX,
    EMR_SETVIEWPORTORGEX,
    EMR_SETMAPMODE:
      E.ScaleMatrix(nil, MWT_SET); //recalc new transformation
  end;
end;

procedure RenderMetaFile(C: TPdfCanvas; MF: TMetaFile; ScaleX, ScaleY,
  XOff, YOff: single; TextPositioning: TPdfCanvasRenderMetaFileTextPositioning;
  KerningHScaleBottom, KerningHScaleTop: single;
  TextClipping: TPdfCanvasRenderMetaFileTextClipping);
var
  E: TPdfEnum;
  R: TRect;
begin
  R.Left := 0;
  R.Top := 0;
  R.Right := MF.Width;
  R.Bottom := MF.Height;
  if ScaleY = 0 then
    ScaleY := ScaleX; // if ScaleY is ommited -> assume symmetric coordinates
  E := TPdfEnum.Create(C);
  try
    C.fOffsetXDef := XOff;
    C.fOffsetYDef := YOff;
    C.fDevScaleX := ScaleX * C.fFactor;
    C.fDevScaleY := ScaleY * C.fFactor;
    C.fEmfBounds := R; // keep device rect
    C.fUseMetaFileTextPositioning := TextPositioning;
    C.fUseMetaFileTextClipping := TextClipping;
    C.fKerningHScaleBottom := KerningHScaleBottom;
    C.fKerningHScaleTop := KerningHScaleTop;
    if C.fDoc.fPrinterPxPerInch.X = 0 then
      C.fDoc.fPrinterPxPerInch := CurrentPrinterRes; // caching for major speedup
    C.fPrinterPxPerInch := C.fDoc.fPrinterPxPerInch;
    with E.DC[0] do
    begin
      Int64(WinSize) := PInt64(@R.Right)^;
      ViewSize := WinSize;
    end;
    C.GSave;
    try
      {$ifdef FPC}
      EnumEnhMetaFile(C.fDoc.fDC, MF.Handle, @EnumEMFFunc, E, Windows.RECT(R));
      {$else}
      EnumEnhMetaFile(C.fDoc.fDC, MF.Handle, @EnumEMFFunc, E, TRect(R));
      {$endif FPC}
    finally
      C.GRestore;
    end;
  finally
    E.Free;
  end;
end;



{ TPdfEnum }

constructor TPdfEnum.Create(ACanvas: TPdfCanvas);
begin
  Canvas := ACanvas;
  // set invalid colors or style -> force paint
  fFillColor := -1;
  fStrokeColor := -1;
  fPenStyle := -1;
  fPenWidth := -1;
  DC[0].brush.null := true;
  fInitTransformMatrix := DefaultIdentityMatrix;
  DC[0].WorldTransform := fInitTransformMatrix;
  fInitMetaRgn := PdfBox(0, 0, 0, 0);
  DC[0].ClipRgnNull := true;
  DC[0].MappingMode := MM_TEXT;
  DC[0].PolyFillMode := ALTERNATE;
  DC[0].StretchBltMode := STRETCH_DELETESCANS;
end;

procedure TPdfEnum.CreateFont(aLogFont: PEMRExtCreateFontIndirect);
var
  hf: HFONT;
  tm: TTextMetric;
  old: HGDIOBJ;
  dest: HDC;
begin
  dest := Canvas.fDoc.fDC;
  hf := CreateFontIndirectW(aLogFont.elfw.elfLogFont);
  old := SelectObject(dest, hf);
  GetTextMetrics(dest, tm);
  SelectObject(dest, old);
  DeleteObject(hf);
  if aLogFont^.ihFont - 1 < cardinal(length(Obj)) then
    with Obj[aLogFont^.ihFont - 1] do
    begin
      kind := OBJ_FONT;
      MoveFast(aLogFont^.elfw.elfLogFont, LogFont, SizeOf(LogFont));
      LogFont.lfPitchAndFamily := tm.tmPitchAndFamily;
      if LogFont.lfOrientation <> 0 then
        FontSpec.angle := LogFont.lfOrientation div 10 // -360..+360
      else
        FontSpec.angle := LogFont.lfEscapement div 10;
      FontSpec.ascent := tm.tmAscent;
      FontSpec.descent := tm.tmDescent;
      FontSpec.cell := tm.tmHeight - tm.tmInternalLeading;
    end;
end;

procedure TPdfEnum.DrawBitmap(xs, ys, ws, hs, xd, yd, wd, hd, usage: integer;
  Bmi: PBitmapInfo; bits: pointer; clipRect: PRect; xSrcTransform: PXForm;
  dwRop: DWord; transparent: TPdfColorRGB);
var
  bmp: TBitmap;
  R: TRect;
  box, clp: TPdfBox;
  fx, fy, ox, oy: single;
begin
  bmp := TBitmap.Create;
  try
    InitTransformation(xSrcTransform, fx, fy, ox, oy);
    // create a TBitmap with (0,0,ws,hs) bounds from DIB bits and info
    if Bmi^.bmiHeader.biBitCount = 1 then
      bmp.Monochrome := true
    else
      bmp.PixelFormat := pf24bit;
    bmp.Width := ws;
    bmp.Height := hs;
    StretchDIBits(bmp.Canvas.Handle, 0, 0, ws, hs, Trunc(xs + ox),
      Trunc(ys + oy), Trunc(ws * fx), Trunc(hs * fy),
      bits, Bmi^, usage, dwRop);
    if transparent <> $FFFFFFFF then
    begin
      if integer(transparent) < 0 then
        transparent := GetSysColor(transparent and $ff);
      bmp.TransparentColor := transparent;
    end;
    // draw the bitmap on the PDF canvas
    with Canvas do
    begin
      R := TRect(Rect(xd, yd, wd + xd, hd + yd));
      NormalizeRect(R);
      inc(R.Bottom);
      inc(R.Right);
      box := BoxI(R, true);
      clp := GetClipRect;
      if (clp.Width > 0) and
         (clp.Height > 0) then
        Doc.CreateOrGetImage(bmp, @box, @clp) // use cliping
      else
        Doc.CreateOrGetImage(bmp, @box, nil);
      // Doc.CreateOrGetImage() will reuse any matching TPdfImage
      // don't send bmi and bits parameters here, because of StretchDIBits above
    end;
  finally
    bmp.Free;
  end;
end;

// simulate gradient (not finished)
procedure TPdfEnum.GradientFill(data: PEMGradientFill);
type
  PTriVertex = ^TTriVertex;
  TTriVertex = packed record // circumvent some bug in older Delphi
    x: integer;
    Y: integer;
    Red: word; // COLOR16 wrongly defined in Delphi 6/7 e.g.
    Green: word;
    Blue: word;
    alpha: word;
  end;
  PTriVertexArray = ^TTriVertexArray;
  TTriVertexArray = array[word] of TTriVertex;
  PGradientTriArray = ^TGradientTriArray;
  TGradientTriArray = array[word] of TGradientTriangle;
  PGradientRectArray = ^TGradientRectArray;
  TGradientRectArray = array[word] of TGradientRect;
var
  i: integer;
  vertex: PTriVertexArray;
  tri: PGradientTriArray;
  r: PGradientRectArray;
  pt1, pt2: PTriVertex;
//    Direction: TGradientDirection;
begin
  if data^.nVer > 0 then
  begin
    vertex := @data.Ver;
    case data^.ulMode of
      GRADIENT_FILL_RECT_H,
      GRADIENT_FILL_RECT_V:
        begin
          Canvas.NewPath;
          r := @vertex[data^.nVer];
{         Direction := gdHorizontal;
          if data^.ulMode = GRADIENT_FILL_RECT_V then
            Direction := gdVertical; }
          for i := 1 to data^.nTri do
            with r[i - 1] do
            begin
              pt1 := @vertex[UpperLeft];
              pt2 := @vertex[LowerRight];
              Canvas.MoveToI(pt1.X, pt1.Y);
              Canvas.LineToI(pt1.X, pt2.Y);
              Canvas.LineToI(pt2.X, pt2.Y);
              Canvas.LineToI(pt2.X, pt1.Y);
              Canvas.Closepath;
              Canvas.Fill;
            end;
        end;
      GRADIENT_FILL_TRIANGLE:
        begin
          Canvas.NewPath;
          tri := @vertex[data^.nVer];
          for i := 1 to data^.nTri do
            with tri[i - 1] do
            begin
              with vertex[Vertex1] do
              begin
                FillColor := RGBA(Red, Green, Blue, 0); // ignore Alpha
                Canvas.MoveToI(X, Y);
              end;
              with vertex[Vertex2] do
                Canvas.LineToI(X, Y);
              with vertex[Vertex3] do
                Canvas.LineToI(X, Y);
              with vertex[Vertex1] do
                Canvas.LineToI(X, Y);
              // DC[nDC].Moved := Point(pt1.X, pt1.Y);
              Canvas.Closepath;
              Canvas.Fill;
            end;
        end;
    end;
  end;
end;

procedure TPdfEnum.PolyPoly(data: PEMRPolyPolygon; iType: integer);
var
  i, j, o, f: DWord;
  a: PPointArray;
  a16: PSmallPointArray;
  data16: PEMRPolyPolygon16 absolute data;
begin
  NeedBrushAndPen;
  if not Canvas.fNewPath then
    Canvas.NewPath;
  case iType of
    EMR_POLYPOLYGON,
    EMR_POLYPOLYLINE:
      begin
        o := 0;
        a := {%H-}pointer(PtrUInt(data) +
             SizeOf(TEMRPolyPolyline) - SizeOf(TPoint) +
             (data^.nPolys - 1) * SizeOf(DWord));
        for i := 1 to data^.nPolys do
        begin
          f := o;
          Canvas.MoveToI(a[o].x, a[o].Y);
          inc(o);
          for j := 2 to data^.aPolyCounts[i - 1] do
          begin
            Canvas.LineToI(a[o].x, a[o].Y);
            DC[nDC].position := Point(a[o].x,
              a[o].Y);
            inc(o);
          end;
          Canvas.LineToI(a[f].x, a[f].Y);
          DC[nDC].Moved := false;
        end;
      end;
    EMR_POLYPOLYGON16,
    EMR_POLYPOLYLINE16:
      begin
        o := 0;
        a16 := {%H-}pointer(PtrUInt(data16) +
               SizeOf(TEMRPolyPolyline16) - SizeOf(TSmallPoint) +
               (data16^.nPolys - 1) * SizeOf(DWord));
        for i := 1 to data16^.nPolys do
        begin
          f := o;
          Canvas.MoveToI(a16[o].x, a16[o].Y);
          inc(o);
          for j := 2 to data16^.aPolyCounts[i - 1] do
          begin
            Canvas.LineToI(a16[o].x, a16[o].Y);
            DC[nDC].position := Point(a16[o].x,
              a16[o].Y);
            inc(o);
          end;
          Canvas.LineToI(a16[f].x, a16[f].Y);
          DC[nDC].Moved := false;
        end;
      end;
  end;
  if iType in [EMR_POLYPOLYLINE, EMR_POLYPOLYLINE16] then
  begin // stroke
    if not DC[nDC].pen.null then
      Canvas.Stroke
    else
      Canvas.NewPath;
  end
  else
  begin
    // fill
    if not DC[nDC].brush.null then
    begin
      if not DC[nDC].pen.null then
        if DC[nDC].PolyFillMode = ALTERNATE then
          Canvas.EofillStroke
        else
          Canvas.FillStroke
      else if DC[nDC].PolyFillMode = ALTERNATE then
        Canvas.EoFill
      else
        Canvas.Fill
    end
    else if not DC[nDC].pen.null then
      Canvas.Stroke
    else
      Canvas.NewPath;
  end;
end;

procedure TPdfEnum.FillRectangle(const Rect: TRect; ResetNewPath: boolean);
begin
  if DC[nDC].brush.null then
    exit;
  Canvas.NewPath;
  FillColor := DC[nDC].brush.color;
  with Canvas.BoxI(Rect, true) do
    Canvas.Rectangle(Left, Top, Width, Height);
  Canvas.Fill;
  if ResetNewPath then
    Canvas.fNewPath := false;
end;

procedure TPdfEnum.FlushPenBrush;
begin
  with DC[nDC] do
  begin
    if brush.null then
    begin
      if not pen.null then
        Canvas.Stroke
      else
        Canvas.NewPath;
    end
    else if pen.null then
      Canvas.Fill
    else
      Canvas.FillStroke;
  end;
end;

procedure TPdfEnum.SelectObjectFromIndex(iObject: integer);
begin
  with DC[nDC] do
  begin
    if iObject < 0 then
    begin // stock object?
      iObject := iObject and $7fffffff;
      case iObject of
        NULL_BRUSH:
          brush.null := true;
        WHITE_BRUSH..BLACK_BRUSH:
          begin
            brush.color := STOCKBRUSHCOLOR[iObject];
            brush.null := false;
          end;
        NULL_PEN:
          begin
            if fInLined and
               ((pen.style <> PS_NULL) or not pen.null) then
            begin
              fInLined := false;
              if not pen.null then
                Canvas.Stroke;
            end;
            pen.style := PS_NULL;
            pen.null := true;
          end;
        WHITE_PEN,
        BLACK_PEN:
          begin
            if fInLined and
               ((pen.color <> STOCKPENCOLOR[iObject]) or not pen.null) then
            begin
              fInLined := false;
              if not pen.null then
                Canvas.Stroke;
            end;
            pen.color := STOCKPENCOLOR[iObject];
            pen.null := false;
          end;
      end;
    end
    else if cardinal(iObject - 1) < cardinal(length(Obj)) then // avoid GPF
      with Obj[iObject - 1] do
        case Kind of // ignore any invalid reference
          OBJ_PEN:
            begin
              if fInLined and
                 ((pen.color <> PenColor) or
                  (pen.width <> PenWidth) or
                  (pen.style <> PenStyle)) then
              begin
                fInLined := false;
                if not pen.null then
                  Canvas.Stroke;
              end;
              pen.null := (PenWidth < 0) or
                          (PenStyle = PS_NULL); // !! 0 means as thick as possible
              pen.color := PenColor;
              pen.width := PenWidth;
              pen.style := PenStyle;
            end;
          OBJ_BRUSH:
            begin
              brush.null := BrushNull;
              brush.color := BrushColor;
              brush.style := BrushStyle;
            end;
          OBJ_FONT:
            begin
              Font.spec := FontSpec;
              MoveFast(LogFont, Font.LogFont, SizeOf(LogFont));
            end;
        end;
  end;
end;

procedure TPdfEnum.HandleComment(Kind: TPdfGdiComment; P: PAnsiChar; Len: integer);
var
  Text: RawUtf8;
  Img: TPdfImage;
  ImgName: PdfString;
  ImgRect: TPdfRect;
begin
  try
    case Kind of
      pgcOutline: // pgcOutline, @aLevel, 4, aTitle
        if Len > 4 then
        begin
          FastSetString(Text, P + 4, Len - 4);
          Canvas.Doc.CreateOutline(Utf8ToString(Trim(Text)), PInteger(P)^,
            Canvas.I2Y(DC[nDC].position.Y));
        end;
      pgcBookmark: // pgcBookmark, nil, 0, aBookMarkName
        begin
          FastSetString(Text, P, Len);
          Canvas.Doc.CreateBookMark(Canvas.I2Y(DC[nDC].position.Y), Text);
        end;
      pgcLink,
      pgcLinkNoBorder: // pgc[NoBorder], @aRect, SizeOf(aRect), aBookmarkName
        if Len > Sizeof(TRect) then
        begin
          FastSetString(Text, P + SizeOf(TRect), Len - SizeOf(TRect));
          Canvas.Doc.CreateLink(
            Canvas.RectI(PRect(P)^, true), Text, abSolid, ord(Kind = pgcLink));
        end;
      pgcJpegDirect: // pgcJpegDirect, @aRect, SizeOf(aRect), aFileName
        if Len > Sizeof(TRect) then
        begin
          FastSetString(Text, P + SizeOf(TRect), Len - SizeOf(TRect));
          ImgName := 'SynImgJpg' + PdfString(crc32cUtf8ToHex(Text));
          if Canvas.Doc.GetXObject(ImgName) = nil then
          begin
            Img := TPdfImage.CreateJpegDirect(Canvas.Doc, Utf8ToString(Text));
            Canvas.Doc.RegisterXObject(Img, ImgName);
          end;
          ImgRect := Canvas.RectI(PRect(P)^, true);
          Canvas.DrawXObject(ImgRect.Left, ImgRect.Top,
            ImgRect.Right - ImgRect.Left, ImgRect.Bottom - ImgRect.Top, ImgName);
        end;
      pgcBeginMarkContent: // pgcBeginMarkContent, @Group, SizeOf(Group)
        if Len = SizeOf(pointer) then
          Canvas.BeginMarkedContent(PPointer(P)^);
      pgcEndMarkContent: // pgcEndMarkContent, nil, 0
        Canvas.EndMarkedContent;
    end;
  except
    on Exception do
      ; // ignore any error (continue EMF enumeration)
  end;
end;

procedure TPdfEnum.NeedBrushAndPen;
begin
  if fInlined then
  begin
    fInlined := false;
    Canvas.Stroke;
  end;
  NeedPen;
  with DC[nDC] do
    if not brush.null then
      FillColor := brush.color;
end;

procedure TPdfEnum.NeedPen;
begin
  with DC[nDC] do
    if not pen.null then
    begin
      StrokeColor := pen.color;
      if pen.style <> fPenStyle then
      begin
        case pen.style and PS_STYLE_MASK of
          PS_DASH:
            Canvas.SetDash([4, 4]);
          PS_DOT:
            Canvas.SetDash([1, 1]);
          PS_DASHDOT:
            Canvas.SetDash([4, 1, 1, 1]);
          PS_DASHDOTDOT:
            Canvas.SetDash([4, 1, 1, 1, 1, 1]);
        else
          Canvas.SetDash([]);
        end;
        case Pen.style and PS_ENDCAP_MASK of
          PS_ENDCAP_ROUND:
            Canvas.SetLineCap(lcRound_End);
          PS_ENDCAP_SQUARE:
            Canvas.SetLineCap(lcProjectingSquareEnd);
          PS_ENDCAP_FLAT:
            Canvas.SetLineCap(lcButt_End);
        end;
        fPenStyle := pen.style;
      end;
      if pen.width * Canvas.fWorldFactorX * Canvas.fDevScaleX <> fPenWidth then
      begin
        if pen.width = 0 then
          fPenWidth := Canvas.fWorldFactorX * Canvas.fDevScaleX
        else
          fPenWidth := pen.width * Canvas.fWorldFactorX * Canvas.fDevScaleX;
        Canvas.SetLineWidth(fPenWidth * Canvas.fFactorX);
      end;
    end
    else
    begin
      // pen.null need reset values
      fStrokeColor := -1;
      fPenWidth := -1;
      fPenStyle := -1;
    end;
end;

procedure TPdfEnum.RestoreDC;
begin
  Assert(nDC > 0);
  dec(nDC);
end;

procedure TPdfEnum.SaveDC;
begin
  Assert(nDC < high(DC));
  DC[nDC + 1] := DC[nDC];
  inc(nDC);
end;

procedure TPdfEnum.ScaleMatrix(Custom: PXForm; iMode: integer);
var
  xf: XForm;
  xdim, ydim: single;
  mx, my: integer;
begin
  if fInlined then
  begin
    fInlined := false;
    if not DC[nDC].pen.null then
      Canvas.Stroke;
  end;
  with DC[nDC], Canvas do
  begin
    fViewSize := ViewSize;
    fViewOrg := ViewOrg;
    fWinSize := WinSize;
    fWinOrg := WinOrg;
    case MappingMode of
      MM_TEXT:
        begin
          fViewSize.cx := 1;
          fViewSize.cy := 1;
          fWinSize.cx := 1;
          fWinSize.cy := 1;
        end;
      MM_LOMETRIC:
        begin
          fViewSize.cx := fPrinterPxPerInch.X;
          fViewSize.cy := -fPrinterPxPerInch.Y;
          fWinSize.cx := WinSize.cx * 10;
          fWinSize.cy := WinSize.cy * 10;
        end;
      MM_HIMETRIC:
        begin
          fViewSize.cx := fPrinterPxPerInch.X;
          fViewSize.cy := -fPrinterPxPerInch.Y;
          fWinSize.cx := WinSize.cx * 100;
          fWinSize.cy := WinSize.cy * 100;
        end;
      MM_LOENGLISH:
        begin
          fViewSize.cx := fPrinterPxPerInch.X;
          fViewSize.cy := -fPrinterPxPerInch.Y;
          fWinSize.cx := MulDiv(1000, WinSize.cx, 254);
          fWinSize.cy := MulDiv(1000, WinSize.cy, 254);
        end;
      MM_HIENGLISH:
        begin
          fViewSize.cx := fPrinterPxPerInch.X;
          fViewSize.cy := -fPrinterPxPerInch.Y;
          fWinSize.cx := MulDiv(10000, WinSize.cx, 254);
          fWinSize.cy := MulDiv(10000, WinSize.cy, 254);
        end;
      MM_TWIPS:
        begin
          fViewSize.cx := fPrinterPxPerInch.X;
          fViewSize.cy := -fPrinterPxPerInch.Y;
          fWinSize.cx := MulDiv(14400, WinSize.cx, 254);
          fWinSize.cy := MulDiv(14400, WinSize.cy, 254);
        end;
      MM_ISOTROPIC:
        begin
          fViewSize.cx := fPrinterPxPerInch.X;
          fViewSize.cy := -fPrinterPxPerInch.Y;
          fWinSize.cx := WinSize.cx * 10;
          fWinSize.cy := WinSize.cy * 10;
          xdim := Abs(fViewSize.cx * WinSize.cx / (fPrinterPxPerInch.X * fWinSize.cx));
          ydim := Abs(fViewSize.cy * WinSize.cy / (fPrinterPxPerInch.Y * fWinSize.cy));
          if xdim > ydim then
          begin
            if fViewSize.cx >= 0 then
              mx := 1
            else
              mx := -1;
            fViewSize.cx := Trunc(fViewSize.cx * ydim / xdim + 0.5);
            if fViewSize.cx = 0 then
              fViewSize.cx := mx;
          end
          else
          begin
            if fViewSize.cy >= 0 then
              my := 1
            else
              my := -1;
            fViewSize.cy := Trunc(fViewSize.cy * xdim / ydim + 0.5);
            if fViewSize.cy = 0 then
              fViewSize.cy := my;
          end;
        end;
      MM_ANISOTROPIC:
        ;  // TBD
    end;
    if fWinSize.cx = 0 then // avoid EZeroDivide
      fFactorX := 1.0
    else
      fFactorX := Abs(fViewSize.cx / fWinSize.cx);
    if fWinSize.cy = 0 then // avoid EZeroDivide
      fFactorY := 1.0
    else
      fFactorY := Abs(fViewSize.cy / fWinSize.cy);
    if Custom <> nil then
    begin
      // S.eM11=fFactorX S.eM12=0 S.eM21=0 S.eM22=fFactorY multiplied by Custom^
      case iMode of
        MWT_IDENTITY: // reset identity matrix
          WorldTransform := DefaultIdentityMatrix;
        MWT_LEFTMULTIPLY:
          WorldTransform := CombineTransform(Custom^, WorldTransform);
        MWT_RIGHTMULTIPLY:
          WorldTransform := CombineTransform(WorldTransform, Custom^);
        MWT_SET:
          WorldTransform := Custom^;
      end;
    end;
    // use transformation
    xf := WorldTransform;
    if (xf.eM11 > 0) and
       (xf.eM22 > 0) and
       (xf.eM12 = 0) and
       (xf.eM21 = 0) then
    begin // Scale
      fWorldFactorX := xf.eM11;
      fWorldFactorY := xf.eM22;
      fWorldOffsetX := WorldTransform.eDx;
      fWorldOffsetY := WorldTransform.eDy;
    end
    else if (xf.eM22 = xf.eM11) and
            (xf.eM21 = -xf.eM12) then
    begin // Rotate
      fAngle := ArcSin(xf.eM12) * c180divPI;
      fWorldOffsetCos := xf.eM11;
      fWorldOffsetSin := xf.eM12;
    end
    else if (xf.eM11 = 0) and
            (xf.eM22 = 0) and
            ((xf.eM12 <> 0) or
             (xf.eM21 <> 0)) then
    begin //Shear

    end
    else if ((xf.eM11 < 0) or
             (xf.eM22 < 0)) and
            (xf.eM12 = 0) and
            (xf.eM21 = 0) then
    begin //Reflection

    end;
  end;
end;

procedure TPdfEnum.InitMetaRgn(const ClientRect: TRect);
begin
  fInitMetaRgn := Canvas.BoxI(ClientRect, true);
  DC[nDC].ClipRgnNull := true;
  DC[nDC].MetaRgn := fInitMetaRgn;
end;

procedure TPdfEnum.SetMetaRgn;
begin
  try
    with DC[nDC] do
      if not ClipRgnNull then
      begin
        MetaRgn := IntersectClipRect(ClipRgn, MetaRgn);
        FillCharFast(ClipRgn, SizeOf(ClipRgn), 0);
        ClipRgnNull := true;
      end;
  except
    on e: Exception do
      ; // ignore any error (continue EMF enumeration)
  end;
end;

function TPdfEnum.IntersectClipRect(const ClpRect: TPdfBox;
  const CurrRect: TPdfBox): TPdfBox;
begin
  result := CurrRect;
  if (ClpRect.Width <> 0) or
     (ClpRect.Height <> 0) then
  begin // ignore null clipping area
    if ClpRect.Left > result.Left then
      result.Left := ClpRect.Left;
    if ClpRect.Top > result.Top then
      result.Top := ClpRect.Top;
    if (ClpRect.Left + ClpRect.Width) < (result.Left + result.Width) then
      result.Width := (ClpRect.Left + ClpRect.Width) - result.Left;
    if (ClpRect.Top + ClpRect.Height) < (result.Top + result.Height) then
      result.Height := (ClpRect.Top + ClpRect.Height) - result.Top;
    // fix rect
    if result.Width < 0 then
      result.Width := 0;
    if result.Height < 0 then
      result.Height := 0;
  end;
end;

procedure TPdfEnum.ExtSelectClipRgn(data: PEMRExtSelectClipRgn);
var
  i: integer;
  d: PRgnData;
  pr: PRect;
  r: TRect;
begin
  // see http://www.codeproject.com/Articles/1944/Guide-to-WIN-Regions
  if data^.iMode <> RGN_COPY then
    exit; // we are handling RGN_COPY (5) only
  if not DC[nDC].ClipRgnNull then // if current clip then finish
  begin
    Canvas.GRestore;
    Canvas.NewPath;
    Canvas.fNewPath := false;
    DC[nDC].ClipRgnNull := true;
    fFillColor := -1;
  end;
  if Data^.cbRgnData > 0 then
  begin
    Canvas.GSave;
    Canvas.NewPath;
    DC[nDC].ClipRgnNull := false;
    d := @Data^.RgnData;
    pr := @d^.Buffer;
    for i := 1 to d^.rdh.nCount do
    begin
      r := pr^;
      inc(r.Bottom);
      inc(r.Right);
      with Canvas.BoxI(r, false) do
        Canvas.Rectangle(Left, Top, Width, Height);
      inc(pr);
    end;
    Canvas.Closepath;
    Canvas.Clip;
    Canvas.NewPath;
    Canvas.FNewPath := false;
  end;
end;

function TPdfEnum.GetClipRect: TPdfBox;
begin // get current clip area
  with DC[nDC] do
    if ClipRgnNull then
      result := MetaRgn
    else
      result := ClipRgn;
end;

procedure TPdfEnum.SetFillColor(Value: integer);
begin
  if fFillColor = Value then
    exit;
  Canvas.SetRGBFillColor(Value);
  fFillColor := Value;
end;

procedure TPdfEnum.SetStrokeColor(Value: integer);
begin
  if fStrokeColor = Value then
    exit;
  Canvas.SetRGBStrokeColor(Value);
  fStrokeColor := Value;
end;

function DXTextWidth(DX: PIntegerArray; n: PtrInt): integer;
var
  i: PtrInt;
begin
  result := 0;
  for i := 0 to n - 1 do
    inc(result, DX^[i]);
end;

procedure TPdfEnum.TextOut(var R: TEMRExtTextOut);
var
  sx, sy, nspace, i: integer;
  cur: cardinal;
  ws, ss, xs, ys, ww, mw, w, h, hscale: single;
  a, acos, asin, fscaleX, fscaleY: single;
  dx: PIntegerArray; // not handled during drawing yet
  posi: TPoint;
  tmp: array of WideChar; // R.emrtext is not #0 terminated -> use tmp[]
  hasdx, clipped, isopaque: boolean;
  tmp2: array[0..1] of WideChar;
  clip: TPdfBox;
  back: TRect;
  po: TPdfCanvasRenderMetaFileTextPositioning;
  {$ifdef USE_UNISCRIBE}
  fnt: TPdfFont;
  dest: HDC;
  siz: TSize;
  {$endif USE_UNISCRIBE}

  procedure DrawLine(var P: TPoint; aH: single);
  var
    tmp: TPdfEnumStatePen;
  begin
    with DC[nDC] do
    begin
      tmp := Pen;
      pen.color := Font.color;
      pen.width := ss / 15 / Canvas.fWorldFactorX / Canvas.fDevScaleX;
      pen.style := PS_SOLID;
      pen.null := false;
      NeedPen;
      if Font.spec.angle = 0 then
      begin
        // P = textout original coords
        // (-w,-h) = delta to text start pos (at baseline)
        // ww = text width
        // aH = delta h for drawed line (from baseline)
        Canvas.MoveToS(P.X - w, (P.Y - (h - aH)));
        //  deltax := -w     deltaY := (-h+aH)
        Canvas.LineToS(P.X - w + ww, (P.Y - (h - aH)));
        //  deltax := -w+ww  deltaY := (-h+aH)
      end
      else
      begin
        // rotation pattern:
        //   rdx = deltax * acos + deltay * asin
        //   rdy = deltay * acos - deltax * asin
        Canvas.MoveToS(P.X + ((-w) * acos + (-h + aH) * asin),
                       P.Y + ((-h + aH) * acos - (-w) * asin));
        Canvas.LineToS(P.X + ((-w + ww) * acos + (-h + aH) * asin),
                       P.Y + ((-h + aH) * acos - (-w + ww) * asin));
      end;
      Canvas.Stroke;
      Pen := tmp;
      NeedPen;
    end;
  end;

begin
  if R.emrtext.nChars > 0 then
    with DC[nDC] do
    begin
      SetLength(tmp, R.emrtext.nChars + 1); // faster than WideString for our purpose
      MoveFast(pointer(PtrUInt(@R) + R.emrtext.offString)^, tmp[0], R.emrtext.nChars * 2);
      sy := 1;
      sx := 1;
      if (Canvas.fWorldFactorY) < 0 then
        sy := -1;
      if (Canvas.fWorldFactorX) < 0 then
        sx := -1;
      fscaleY := Abs(Canvas.fFactorY * Canvas.fWorldFactorY * Canvas.fDevScaleY);
      fscaleX := Abs(Canvas.fFactorX * Canvas.fWorldFactorX * Canvas.fDevScaleX);
      // guess the font size
      if Font.LogFont.lfHeight < 0 then
        ss := Abs(Font.LogFont.lfHeight) * fscaleY
      else
        ss := Abs(Font.spec.cell) * fscaleY;
      // ensure this font is selected (very fast if was already selected)
      {$ifdef USE_UNISCRIBE}fnt :={$endif} Canvas.SetFont(Canvas.fDoc.fDC, Font.LogFont, ss);
      // calculate coordinates
      po := Canvas.fUseMetaFileTextPositioning;
      if (R.emrtext.fOptions and ETO_GLYPH_INDEX <> 0) then
        mw := 0
      else
      begin
        ws := 0;
        {$ifdef USE_UNISCRIBE}
        if Assigned(fnt) and Canvas.fDoc.UseUniScribe and
           fnt.InheritsFrom(TPdfFontTrueType) then
        begin
          dest := Canvas.fDoc.GetDCWithFont(TPdfFontTrueType(fnt));
          if GetTextExtentPoint32W(dest, pointer(tmp), R.emrtext.nChars, siz) then
            ws := (siz.cX * Canvas.fPage.fFontSize) / 1000;
        end;
        {$endif USE_UNISCRIBE}
        if ws = 0 then
          ws := Canvas.UnicodeTextWidth(pointer(tmp));
        mw := Round(ws / fscaleX);
      end;
      hasdx := R.emrtext.offDx > 0;
      {$ifdef USE_UNISCRIBE}
      if Canvas.fDoc.UseUniScribe then
        hasdx := hasdx and (R.emrtext.fOptions and ETO_GLYPH_INDEX <> 0);
      {$endif USE_UNISCRIBE}
      if hasdx then
      begin
        dx := pointer(PtrUInt(@R) + R.emrtext.offDx);
        w := DXTextWidth(dx, R.emrText.nChars);
        if w < R.rclBounds.Right - R.rclBounds.Left then // offDX=0 or within box
          dx := nil;
      end
      else
        dx := nil;
      if dx = nil then
      begin
        w := mw;
        if po = tpExactTextCharacterPositining then
          po := tpSetTextJustification; // exact position expects dx
      end;
      nspace := 0;
      hscale := 100;
      if mw <> 0 then
      begin
        for i := 0 to R.emrtext.nChars - 1 do
          if tmp[i] = ' ' then
            inc(nspace);
        if (po = tpSetTextJustification) and
           ((nspace = 0) or (({%H-}w - mw) < nspace)) then
          po := tpKerningFromAveragePosition;
        if (po = tpExactTextCharacterPositining) and
           (Font.spec.angle <> 0) then
          po := tpKerningFromAveragePosition;
        case po of
          tpSetTextJustification:
            // we should have had a SetTextJustification() call -> modify word space
            with Canvas do
              SetWordSpace(((w - mw) * fscaleX) / nspace);
          tpKerningFromAveragePosition:
            begin
              // check if dx[] width differs from PDF width
              hscale := (w * 100) / mw;
              // implement some global kerning if needed (allow hysteresis around 100%)
              if (hscale < Canvas.fKerningHScaleBottom) or
                 (hscale > Canvas.fKerningHScaleTop) then
                if Font.spec.angle = 0 then
                  Canvas.SetHorizontalScaling(hscale)
                else
                  hscale := 100
              else
                hscale := 100;
            end;
        end;
      end
      else
        po := tpSetTextJustification;
      ww := w;                                    // right x
      // h Align Mask = TA_CENTER or TA_RIGHT or TA_LEFT = TA_CENTER
      if (Font.Align and TA_CENTER) = TA_CENTER then
        w := w / 2  // center x
      else if (Font.Align and TA_CENTER) = TA_LEFT then
        w := 0;     // left x
      // V Align mask = TA_BASELINE or TA_BOTTOM or TA_TOP = TA_BASELINE
      if (Font.Align and TA_BASELINE) = TA_BASELINE then
      // always zero ?
        h := Abs(Font.LogFont.lfHeight) - Abs(Font.spec.cell)  // center y
      else if (Font.Align and TA_BASELINE) = TA_BOTTOM then
        h := Abs(Font.spec.descent)  // bottom y
      else
        // needs - vertical coords of baseline from top
        h := -abs(Font.spec.ascent); // top
      if sy < 0 then // inverted coordinates
        h := Abs(Font.LogFont.lfHeight) + h;
      if sx < 0 then
        w := w + ww;
      if (Font.align and TA_UPDATECP) = TA_UPDATECP then
        posi := position
      else
        posi := R.emrtext.ptlReference;
      // detect clipping
      if Canvas.fUseMetaFileTextClipping <> tcNeverClip then
      begin
        with R.emrtext.rcl do
          clipped := (Right > Left) and (Bottom > Top);
        if clipped then
          clip := Canvas.BoxI(TRect(R.emrtext.rcl), true)
        else
        begin
          if Canvas.fUseMetaFileTextClipping = tcClipExplicit then
            with R.rclBounds do
              clipped := (Right > Left) and (Bottom > Top);
          if clipped then
            clip := Canvas.BoxI(TRect(R.rclBounds), true)
          else
          begin
            clipped := not ClipRgnNull and
                        (Canvas.fUseMetaFileTextClipping = tcAlwaysClip);
            if clipped then
              clip := GetClipRect;
          end;
        end;
      end
      else
        clipped := false;
      isopaque := not brush.null and
                 (brush.Color <> clWhite) and
                 ((R.emrtext.fOptions and ETO_OPAQUE <> 0) or
                  ((Font.BkMode = OPAQUE) and
                   (Font.BkColor = brush.color)));
      if isopaque then
        if clipped then
          back := TRect(R.emrtext.rcl)
        else
        begin
          back.TopLeft := posi;
          back.BottomRight := posi;
          inc(back.Right, Trunc(ww));
          inc(back.Bottom, Abs(Font.LogFont.lfHeight));
        end;
      NormalizeRect(back);
      if clipped then
      begin
        Canvas.GSave;
        Canvas.NewPath;
        Canvas.Rectangle({%H-}clip.Left, {%H-}clip.Top,
          {%H-}clip.Width, {%H-}clip.Height);
        Canvas.ClosePath;
        Canvas.Clip;
        if isopaque then
        begin
          FillRectangle(back, false);
          isopaque := false; //do not handle more
        end
        else
          Canvas.NewPath;
        Canvas.fNewPath := false;
      end;
      // draw background (if any)
      if isopaque then
        // don't handle BkMode, since global to the page, but only specific text
        // don't handle rotation here, since should not be used much
        FillRectangle(back, true);
      // draw text
      FillColor := Font.color;
      {$ifdef USE_UNISCRIBE}
      Canvas.RightToLeftText := (R.emrtext.fOptions and ETO_RTLREADING) <> 0;
      {$endif USE_UNISCRIBE}
      Canvas.BeginText;
      if Font.spec.angle <> 0 then
      begin
        a := Font.spec.angle * cPIdiv180;
        acos := cos(a);
        asin := sin(a);
        xs := 0;
        ys := 0;
        Canvas.SetTextMatrix(acos, asin, -asin, acos,
          Canvas.I2X(posi.X - Round(w * acos + h * asin)),
          Canvas.I2Y(posi.Y - Round(h * acos - w * asin)));
      end
      else if (WorldTransform.eM11 = WorldTransform.eM22) and
              (WorldTransform.eM12 = -WorldTransform.eM21) and
              not SameValue(ArcCos(WorldTransform.eM11), 0, 0.0001) then
      begin
        xs := 0;
        ys := 0;
        if SameValue(ArcCos(WorldTransform.eM11), 0, 0.0001) or      // 0 grad
           SameValue(ArcCos(WorldTransform.eM11), cPI, 0.0001) then  // 180 grad
          Canvas.SetTextMatrix(WorldTransform.eM11, WorldTransform.eM12,
            WorldTransform.eM21, WorldTransform.eM22,
            Canvas.S2X(posi.X * WorldTransform.eM11 +
              posi.Y * WorldTransform.eM21 + WorldTransform.eDx),
            Canvas.S2Y(posi.X * WorldTransform.eM12 +
              posi.Y * WorldTransform.eM22 + WorldTransform.eDy))
        else
          Canvas.SetTextMatrix(-WorldTransform.eM11, -WorldTransform.eM12, -
            WorldTransform.eM21, -WorldTransform.eM22,
            Canvas.S2X(posi.X * WorldTransform.eM11 +
              posi.Y * WorldTransform.eM21 + WorldTransform.eDx),
            Canvas.S2Y(posi.X * WorldTransform.eM12 +
              posi.Y * WorldTransform.eM22 + WorldTransform.eDy));
      end
      else
      begin
        acos := 0;
        asin := 0;
        if Canvas.fViewSize.cx > 0 then
          xs := posi.X - w   // zero point left
        else
          xs := posi.X + w;  // right
        if Canvas.fViewSize.cy > 0 then
          ys := posi.Y - h   // zero point beyond
        else
          ys := posi.Y + h;  // above
        Canvas.MoveTextPoint(Canvas.S2X(xs), Canvas.S2Y(ys));
      end;
      if (R.emrtext.fOptions and ETO_GLYPH_INDEX) <> 0 then
        Canvas.ShowGlyph(pointer(tmp), R.emrtext.nChars)
      else if po = tpExactTextCharacterPositining then
      begin
        cur := 0;
        tmp2[1] := #0;
        repeat
          tmp2[0] := tmp[cur];
          Canvas.ShowText(@tmp2, false);
          if cur = R.emrtext.nChars - 1 then
            break;
          xs := xs + dx^[cur];
          Canvas.EndText;
          Canvas.BeginText;
          Canvas.MoveTextPoint(Canvas.S2X(xs), Canvas.S2Y(ys));
          inc(cur);
        until false;
      end
      else
        Canvas.ShowText(pointer(tmp));
      Canvas.EndText;
      // handle underline or strike out styles (direct draw PDF lines on canvas)
      if Font.LogFont.lfUnderline <> 0 then
        DrawLine(posi, ss / 8 / Canvas.fWorldFactorX / Canvas.fDevScaleX);
      if Font.LogFont.lfStrikeOut <> 0 then
        DrawLine(posi, -ss / 3 / Canvas.fWorldFactorX / Canvas.fDevScaleX);
      // end any pending clipped TextRect() region
      if clipped then
      begin
        Canvas.GRestore;
        fFillColor := -1; // force set drawing color
      end;
      // restore previous text justification (after GRestore if clipped)
      case po of
        tpSetTextJustification:
          if nspace > 0 then
            Canvas.SetWordSpace(0);
        tpKerningFromAveragePosition:
          if hscale <> 100 then
            Canvas.SetHorizontalScaling(100); // reset horizontal scaling
      end;
      if not Canvas.fNewPath then
      begin
        if clipped then
          if not DC[nDC].ClipRgnNull then
          begin
            clip := GetClipRect;
            Canvas.GSave;
            Canvas.Rectangle(
              clip.Left, clip.Top, clip.Width, clip.Height);
            Canvas.Clip;
            Canvas.GRestore;
            Canvas.NewPath;
            Canvas.fNewPath := false;
          end;
      end
      else
        Canvas.fNewPath := false;
      if (Font.align and TA_UPDATECP) = TA_UPDATECP then
      begin
        position.X := posi.X + Trunc(ww);
        position.Y := posi.Y;
      end;
    end;
end;

{$endif USE_METAFILE}


{$endif OSPOSIX}

end.

