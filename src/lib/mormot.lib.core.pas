/// abstract contracts shared by the library implementation units
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.core;

{
  *****************************************************************************

   Abstract Types and Interfaces Implemented by mormot.lib.* Units
   - Font Types: Specification, Metrics, Glyph Widths
   - Font Interfaces: Provider, Enumerator, Shaper, Subsetter
   - Font Services Registration

   No implementation here, and no user yet: the contracts of the font
   services planned for mormot.lib.uniscribe (Windows), mormot.lib.freetype
   and mormot.lib.harfbuzz (POSIX), as needed by the cross-platform PDF engine.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  mormot.core.base;


{ ****************** Font Types: Specification, Metrics, Glyph Widths }

type
  /// opaque font handle of an IFontProvider implementation
  // - e.g. a HFONT on Windows, a FT_Face with FreeType
  TFontHandle = type pointer;

  /// opaque device context of an IFontDC implementation
  // - transitional: a HDC on Windows, a dummy on POSIX - to be replaced by a
  // font face object owning its state
  TFontDC = type pointer;

  /// the requested font, as a Windows LOGFONTW gives it
  TFontRequest = record
    /// font family name, e.g. 'Calibri'
    FaceName: SynUnicode;
    /// character height in logical units - negative for the em height
    Height: integer;
    /// FW_NORMAL = 400, FW_BOLD = 700
    Weight: integer;
    /// 0 = upright, 1 = italic
    Italic: byte;
    /// 0 = ANSI_CHARSET
    CharSet: byte;
    /// FF_SWISS, FF_ROMAN... combined with the pitch
    PitchAndFamily: byte;
  end;

  /// basic metrics of a font, as a Windows TEXTMETRICW gives them
  TFontMetrics = record
    tmHeight: integer;
    tmAscent: integer;
    tmDescent: integer;
    tmInternalLeading: integer;
    tmExternalLeading: integer;
    tmAveCharWidth: integer;
    tmMaxCharWidth: integer;
    tmWeight: integer;
    tmOverhang: integer;
    tmFirstChar: WideChar;
    tmLastChar: WideChar;
    tmDefaultChar: WideChar;
    tmBreakChar: WideChar;
    tmItalic: byte;
    tmCharSet: byte;
    tmPitchAndFamily: byte;
  end;

  /// outline metrics of a font, as a Windows OUTLINETEXTMETRICW gives them
  TFontOutlineMetrics = record
    otmSize: cardinal;
    otmAscent: integer;
    otmDescent: integer;
    otmLineGap: integer;
    otmItalicAngle: integer;
    otmrcFontBox: record
      Left: integer;
      Top: integer;
      Right: integer;
      Bottom: integer;
    end;
    otmMacAscent: integer;
    otmMacDescent: integer;
    otmMacLineGap: cardinal;
    otmEMSquare: cardinal;
    otmCapEmHeight: integer;
    otmXHeight: integer;
    otmStrikeoutPosition: integer;
    otmStrikeoutSize: cardinal;
    otmUnderscorePosition: integer;
    otmUnderscoreSize: cardinal;
  end;

  /// the advance of one glyph in three parts, as a Windows ABC gives it
  // - the advance is abcA + abcB + abcC: an implementation scaling design
  // units has to scale the advance once and give abcB the remainder, so that
  // three roundings do not add up
  TFontCharAbc = record
    /// spacing before the glyph, may be negative
    abcA: integer;
    /// width of the glyph body
    abcB: cardinal;
    /// spacing after the glyph, may be negative
    abcC: integer;
  end;

  /// TFontCharAbc of consecutive characters
  TFontCharAbcArray = array of TFontCharAbc;

  /// what IFontShaper.Shape did with one part of the text
  // - fskShaped: Glyphs (and maybe Advances/Offsets) hold the result
  // - fskPlain: the part needs no shaping - draw it unshaped
  // - fskSkip: the part could not be shaped - draw nothing for it
  TFontShapeKind = (
    fskShaped,
    fskPlain,
    fskSkip);

  /// one part of a shaped text, in visual order
  // - positions count UTF-16 code units of the whole source text
  TFontShapedRun = record
    /// what the shaper did with this part
    Kind: TFontShapeKind;
    /// first code unit of the part in the source text, 0-based
    TextStart: integer;
    /// number of code units of the part
    TextLen: integer;
    /// glyph indexes of the font, in visual order, only glyphs to be drawn:
    // the shaper leaves out what draws nothing, e.g. a zero-width glyph which
    // is no diacritic, and keeps the other arrays aligned with Glyphs
    Glyphs: TWordDynArray;
    /// advance per glyph in 1/1000 em, positioned - empty when the advances
    // of the font apply
    Advances: TIntegerDynArray;
    /// horizontal offset per glyph in 1/1000 em - empty when there is none
    Offsets: TIntegerDynArray;
    /// source code unit of each glyph, in the whole text - empty when not known
    Clusters: TIntegerDynArray;
  end;

  /// the parts of a shaped text, in visual order
  TFontShapedRuns = array of TFontShapedRun;

  /// what IFontSubsetter.Subset has to keep
  TFontSubsetRequest = record
    /// code points whose cmap entries have to survive, e.g. the characters a
    // simple font reaches through the cmap
    Unicodes: TIntegerDynArray;
    /// glyph indexes which have to survive, e.g. glyphs addressed directly
    // or produced by shaping, which have no code point of their own
    Glyphs: TIntegerDynArray;
  end;


{ ****************** Font Interfaces: Provider, Enumerator, Shaper, Subsetter }

  /// create fonts and read their metrics and tables
  IFontProvider = interface
    ['{91DAB0EA-F533-40D2-9E47-BFA2111E88CF}']
    /// create a font handle, nil on failure
    function CreateFont(const Request: TFontRequest): TFontHandle;
    /// release a font handle returned by CreateFont
    procedure DeleteFont(Font: TFontHandle);
    /// select a font into a device context, returning the previous one
    // - transitional, as TFontDC
    function SelectFont(DC: TFontDC; Font: TFontHandle): TFontHandle;
    /// the metrics of the font selected into DC
    function GetTextMetrics(DC: TFontDC; out Metrics: TFontMetrics): boolean;
    /// the outline metrics of the font selected into DC
    function GetOutlineMetrics(DC: TFontDC;
      out Metrics: TFontOutlineMetrics): boolean;
    /// the advances of the characters FirstChar..LastChar
    // - FirstChar and LastChar are WinAnsi (code page 1252) bytes, so that
    // 128..159 are punctuation, not C1 controls
    function GetCharAbcWidths(DC: TFontDC; FirstChar, LastChar: cardinal;
      out Widths: TFontCharAbcArray): boolean;
    /// read the raw bytes of a TrueType/OpenType table, as Windows GetFontData
    // - TableTag is the 4-byte tag, e.g. 'cmap', or 0 for the whole font
    // - returns the number of bytes, or FontDataError
    function GetFontData(DC: TFontDC; TableTag, Offset: cardinal;
      Buffer: pointer; BufferSize: cardinal): cardinal;
    /// the value GetFontData returns on failure, i.e. $ffffffff
    function FontDataError: cardinal;
  end;

  /// list the fonts available on the system
  IFontEnumerator = interface
    ['{176128CF-BF4B-4B30-ADF1-25BC3B1249D7}']
    /// add the UTF-8 family names of the TrueType fonts to List
    procedure EnumTrueTypeFonts(DC: TFontDC; var List: TRawUtf8DynArray);
  end;

  /// device contexts for IFontProvider
  // - transitional: to be replaced by a font face object owning its state
  IFontDC = interface
    ['{F99740CC-609D-4868-8A86-D5E64AB32473}']
    /// create a device context to measure fonts with
    function CreateDC: TFontDC;
    /// release a device context returned by CreateDC
    procedure DeleteDC(DC: TFontDC);
    /// the screen resolution in pixels per inch (Y axis)
    function GetScreenLogPixels(DC: TFontDC): integer;
  end;

  /// shape Unicode text with the OpenType rules of a font
  // - for complex scripts and right-to-left text: Arabic, Hebrew, Indic...
  IFontShaper = interface
    ['{EFE07440-0B89-41ED-8E99-07106AB07ABF}']
    /// shape Len characters of Text with Font
    // - RightToLeft forces the direction; false lets the script decide
    // - returns false when the whole text should be drawn unshaped, e.g.
    // because no part of it needs shaping or the shaper failed
    function Shape(Text: PWideChar; Len: integer; Font: TFontHandle;
      RightToLeft: boolean; out Runs: TFontShapedRuns): boolean;
  end;

  /// make a subset of a TrueType/OpenType font
  IFontSubsetter = interface
    ['{90E129B6-D3B5-4B5A-B76B-55A988E2CDD3}']
    /// return the subset of Face which keeps what Request lists
    // - Face holds the font bytes, Font the handle they were read from
    // - glyph indexes are kept, so data built against Face stays valid
    // - returns false if the font cannot be subset: the caller keeps Face
    function Subset(const Face: RawByteString; const Request: TFontSubsetRequest;
      Font: TFontHandle; out Subset: RawByteString): boolean;
  end;


{ ****************** Font Services Registration }

var
  /// the registered font provider, nil until an implementation registers
  FontProvider: IFontProvider;
  /// the registered font enumerator
  FontEnumerator: IFontEnumerator;
  /// the registered device context provider - transitional
  FontDC: IFontDC;
  /// the registered text shaper, nil when no shaping library is available
  FontShaper: IFontShaper;
  /// the registered font subsetter, nil when none is available
  FontSubsetter: IFontSubsetter;

/// register the font services of an implementation unit
// - a nil parameter leaves the current registration unchanged
procedure RegisterFontPlatform(const Provider: IFontProvider;
  const Enumerator: IFontEnumerator; const DC: IFontDC);

/// true when a provider, an enumerator and a device context are registered
function FontPlatformRegistered: boolean;


implementation


{ ****************** Font Services Registration }

procedure RegisterFontPlatform(const Provider: IFontProvider;
  const Enumerator: IFontEnumerator; const DC: IFontDC);
begin
  if Provider <> nil then
    FontProvider := Provider;
  if Enumerator <> nil then
    FontEnumerator := Enumerator;
  if DC <> nil then
    FontDC := DC;
end;

function FontPlatformRegistered: boolean;
begin
  result := (FontProvider <> nil) and
            (FontEnumerator <> nil) and
            (FontDC <> nil);
end;


end.
