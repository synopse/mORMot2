/// low-level access to the Windows Uniscribe API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.uniscribe;


{
  *****************************************************************************

   Uniscribe typography and complex scripts processing Windows API
   - UniScribe Shared Types
   - UniScribe API Functions
   - FontSub API for font sunset embedding

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

uses
  windows,
  sysutils,
  mormot.core.os;


{ ****************** UniScribe Shared Types }

const
  /// the Uniscribe API library name
  Usp10 = 'usp10.dll';

  /// error returned by Uniscribe when the current selected font
  // does not contain sufficient glyphs or shaping tables
  USP_E_SCRIPT_NOT_IN_FONT =
    HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16)) or $200;

type
  /// UniScribe script state flag elements
  // - r0,r1,r2,r3,r4: map TScriptState.uBidiLevel bits (not real flags)
  // - fOverrideDirection: Set when in LRO/RLO embedding
  // - fInhibitSymSwap: Set by U+206A (ISS), cleared by U+206B (ASS)
  // - fCharShape: Set by U+206D (AAFS), cleared by U+206C (IAFS)
  // - fDigitSubstitute: Set by U+206E (NADS), cleared by U+206F (NODS)
  // - fInhibitLigate: Equiv !GCP_Ligate, no Unicode control chars yet
  // - fDisplayZWG: Equiv GCP_DisplayZWG, no Unicode control characters yet
  // - fArabicNumContext: For EN->AN Unicode rule
  // - fGcpClusters: For Generating Backward Compatible GCP Clusters (legacy Apps)
  TScriptState_enum = (
    r0, r1, r2, r3, r4,
    fOverrideDirection,
    fInhibitSymSwap,
    fCharShape,
    fDigitSubstitute,
    fInhibitLigate,
    fDisplayZWG,
    fArabicNumContext,
    fGcpClusters
  );

  /// a set of UniScribe script state flags
  TScriptState_set = set of TScriptState_enum;

  PScriptState = ^TScriptState;

  /// an UniScribe script state
  // - uBidiLevel: Unicode Bidi algorithm embedding level (0..16)
  // - fFlags: Script state flags
  TScriptState = packed record
   case Byte of
      0: (
        uBidiLevel: Byte {:5}
      );
      1: (
        fFlags: TScriptState_set
      )
  end;

  /// Uniscribe script analysis flag elements
  // - s0,s1,s2,s3,s4,s5,s6,s7,s8,s9: map TScriptAnalysis.eScript bits
  // - fRtl: Rendering direction - set for Right To Left scripting
  // - fLayoutRtl: Set for GCP classes ARABIC/HEBREW and LOCALNUMBER
  // - fLinkBefore: Implies there was a ZWJ before this item
  // - fLinkAfter: Implies there is a ZWJ following this item.
  // - fLogicalOrder: Set by client as input to ScriptShape/Place
  // - fNoGlyphIndex: Generated by ScriptShape/Place - this item does not use
  // glyph indices
  TScriptAnalysis_enum = (
    s0, s1, s2, s3, s4, s5, s6, s7, s8, s9,
    fRtl,
    fLayoutRtl,
    fLinkBefore,
    fLinkAfter,
    fLogicalOrder,
    fNoGlyphIndex
  );

  /// a set of Uniscribe script analysis flags
  TScriptAnalysis_set = set of TScriptAnalysis_enum;

  /// an Uniscribe script analysis
  // - eScript:  Shaping engine
  // - fFlags: Script analysis flags
  // - s: Script state
  TScriptAnalysis = packed record
   case Byte of
      0: (
        eScript: Word
      );
      1: (
        fFlags: TScriptAnalysis_set;
        s: TScriptState
      );
  end;
  PScriptAnalysis = ^TScriptAnalysis;

  /// a Uniscribe script item, after analysis of a unicode text
  TScriptItem = packed record
    /// Logical offset to first character in this item
    iCharPos: integer;
    /// corresponding Uniscribe script analysis
    a: TScriptAnalysis;
  end;
  PScriptItem = ^TScriptItem;

  /// all possible Uniscribe processing properties of a given language
  // - fNumeric:  if a script contains only digits
  // - fComplex: Script requires special shaping or layout
  // - fNeedsWordBreaking: Requires ScriptBreak for word breaking information
  // - fNeedsCaretInfo: Requires caret restriction to cluster boundaries
  // - bCharSet0 .. bCharSet7: Charset to use when creating font
  // - fControl: Contains only control characters
  // - fPrivateUseArea: This item is from the Unicode range U+E000 through U+F8FF
  // - fNeedsCharacterJustify: Requires inter-character justification
  // - fInvalidGlyph: Invalid combinations generate glyph wgInvalid in the glyph buffer
  // - fInvalidLogAttr: Invalid combinations are marked by fInvalid in the logical attributes
  // - fCDM: Contains Combining Diacritical Marks
  // - fAmbiguousCharSet: Script does not correspond 1// :1 with a charset
  // - fClusterSizeVaries: Measured cluster width depends on adjacent clusters
  // - fRejectInvalid: Invalid combinations should be rejected
  TScriptProperties_enum = (
    fNumeric,
    fComplex,
    fNeedsWordBreaking,
    fNeedsCaretInfo,
    bCharSet0, bCharSet1, bCharSet2, bCharSet3, bCharSet4, bCharSet5,
    bCharSet6, bCharSet7,
    fControl,
    fPrivateUseArea,
    fNeedsCharacterJustify,
    fInvalidGlyph,
    fInvalidLogAttr,
    fCDM,
    fAmbiguousCharSet,
    fClusterSizeVaries,
    fRejectInvalid
  );

  /// set of possible Uniscribe processing properties of a given language
  TScriptProperties_set = set of TScriptProperties_enum;
  PScriptProperties = ^TScriptProperties;

  /// Contains information about Uniscribe special processing for each script
  TScriptProperties = packed record
    /// Primary and sublanguage associated with script
    langid: Word;
    /// set of possible Uniscribe processing properties for a given language
    fFlags: TScriptProperties_set;
  end;

  /// an array of Uniscribe processing information
  PScriptPropertiesArray = ^TPScriptPropertiesArray;
  TPScriptPropertiesArray = array[byte] of PScriptProperties;

  /// Uniscribe visual (glyph) attributes
  // - a0 .. a3: map the Justification class number bits
  // - fClusterStart: First glyph of representation of cluster
  // - fDiacritic: Diacritic
  // - fZeroWidth: Blank, ZWJ, ZWNJ etc, with no width
  // - fReserved: General reserved bit
  TScriptVisAttr_enum = (
    a0, a1, a2, a3,
    fClusterStart,     {:1}  // First glyph of representation of cluster
    fDiacritic,        {:1}  // Diacritic
    fZeroWidth,        {:1}  // Blank, ZWJ, ZWNJ etc, with no width
    fReserved          {:1}  // General reserved
  );
  /// set of Uniscribe visual (glyph) attributes
  TScriptVisAttr_set = set of TScriptVisAttr_enum;

  /// Contains the visual (glyph) attributes that identify clusters and
  // justification points, as generated by ScriptShape
  // - uJustification: Justification class bits
  // - fFlags: Uniscribe visual (glyph) attributes
  // - fShapeReserved: Reserved for use by shaping engines
  TScriptVisAttr = packed record
   case Byte of
    0: (
      uJustification: Byte {:4}
    );
    1: (
      fFlags: TScriptVisAttr_set;
      fShapeReserved: Byte {:8}
    );
  end;
  PScriptVisAttr = ^TScriptVisAttr;

  TScriptControlAttr_enum = (
    fContextDigits,
    fInvertPreBoundDir,
    fInvertPostBoundDir,
    fLinkStringBefore,
    fLinkStringAfter,
    fNeutralOverride,
    fNumericOverride,
    fLegacyBidiClass,
    fScr0, fScr1, fScr2, fScr3, fScr4, fScr5, fScr6, fScr7);

  TScriptControlAttr_set = set of TScriptControlAttr_enum;

  TScriptControl = packed record
    uDefaultLanguage: Word;
    fFlags: TScriptControlAttr_set;
  end;
  PScriptControl = ^TScriptControl;


{ ****************** UniScribe API Functions }

/// Uniscribe function to break a Unicode string into individually shapeable items
// - pwcInChars: Pointer to a Unicode string to itemize.
// - cInChars: Number of characters in pwcInChars to itemize.
// - cMaxItems: Maximum number of SCRIPT_ITEM structures defining items to process.
// - psControl: Optional. Pointer to a SCRIPT_CONTROL structure indicating the
// type of itemization to perform. Alternatively, the application can set this
// parameter to NULL if no SCRIPT_CONTROL properties are needed.
// - psState: Optional. Pointer to a SCRIPT_STATE structure indicating
// the initial bidirectional algorithm state. Alternatively, the application
// can set this parameter to NULL if the script state is not needed.
// - pItems: Pointer to a buffer in which the function retrieves SCRIPT_ITEM
// structures representing the items that have been processed. The buffer
// should be cMaxItems*sizeof(SCRIPT_ITEM) + 1 bytes in length. It is invalid
// to call this function with a buffer to hold less than two SCRIPT_ITEM
// structures. The function always adds a terminal item to the item analysis
// array so that the length of the item with zero-based index "i" is
// always available as:
// ! pItems[i+1].iCharPos - pItems[i].iCharPos;
// - pcItems: Pointer to the number of SCRIPT_ITEM structures processed
function ScriptItemize(
    const pwcInChars: PWideChar; cInChars: integer; cMaxItems: integer;
    const psControl: pointer; const psState: pointer;
    pItems: PScriptItem; var pcItems: integer): HRESULT;
  stdcall; external Usp10;

/// Uniscribe function to retrieve information about the current scripts
// - ppSp: Pointer to an array of pointers to SCRIPT_PROPERTIES structures
// indexed by script.
// - piNumScripts: Pointer to the number of scripts. The valid range for this
// value is 0 through piNumScripts-1.
function ScriptGetProperties(out ppSp: PScriptPropertiesArray;
  out piNumScripts: integer): HRESULT;
  stdcall; external Usp10;

/// Uniscribe function to convert an array of run embedding levels to a map
// of visual-to-logical position and/or logical-to-visual position
// - cRuns: Number of runs to process
// - pbLevel: Array of run embedding levels
// - piVisualToLogical: List of run indices in visual order
// - piLogicalToVisual: List of visual run positions
function ScriptLayout(cRuns: integer; const pbLevel: PByte;
    piVisualToLogical: PInteger; piLogicalToVisual: PInteger): HRESULT;
  stdcall; external Usp10;

/// Uniscribe function to generate glyphs and visual attributes for an Unicode run
// - hdc: Optional (see under caching)
// - psc: Uniscribe font metric cache handle
// - pwcChars: Logical unicode run
// - cChars: Length of unicode run
// - cMaxGlyphs: Max glyphs to generate
// - psa: Result of ScriptItemize (may have fNoGlyphIndex set)
// - pwOutGlyphs: Output glyph buffer
// - pwLogClust: Logical clusters
// - psva: Visual glyph attributes
// - pcGlyphs: Count of glyphs generated
function ScriptShape(hdc: HDC; var psc: pointer; const pwcChars: PWideChar;
    cChars: integer; cMaxGlyphs: integer; psa: PScriptAnalysis;
    pwOutGlyphs: PWord; pwLogClust: PWord; psva: PScriptVisAttr;
    var pcGlyphs: integer): HRESULT;
  stdcall; external Usp10;

/// Uniscribe function to apply the specified digit substitution settings
// to the specified script control and script state structures
function ScriptApplyDigitSubstitution(
    const psds: pointer; const psControl: pointer;
    const psState: pointer): HRESULT;
  stdcall; external Usp10;


{ ****************** FontSub API for font sunset embedding }

var
  /// font subset embedding using Windows XP CreateFontPackage() FontSub.dll
  // - see http://msdn.microsoft.com/en-us/library/dd183502
  // - you should first call HasCreateFontPackage to resolve this API dynamically
  CreateFontPackage: function(puchSrcBuffer: pointer; ulSrcBufferSize: cardinal;
    var puchFontPackageBuffer: PAnsiChar; var pulFontPackageBufferSize: cardinal;
    var pulBytesWritten: cardinal; usFlags, usTTCIndex, usSubsetFormat,
    usSubsetLanguage, usSubsetPlatform, usSubsetEncoding: word;
    pusSubsetKeepList: pointer; usSubsetKeepListCount: word;
    lpfnAllocate, lpfnReAllocate, lpfnFree, reserved: pointer): cardinal; cdecl;

/// resolve CreateFontPackage() API call from FontSub.dll
function HasCreateFontPackage: boolean;

// some low-level functions compatible with lpfnAllocate,lpfnReAllocate,lpfnFree
// parameters of CreateFontPackage()
function lpfnAllocate(Size: integer): pointer; cdecl;
function lpfnReAllocate(Buffer: pointer; Size: integer): pointer; cdecl;
procedure lpfnFree(Buffer: pointer); cdecl;


implementation


{ ****************** FontSub API for font sunset embedding }

function lpfnAllocate(Size: integer): pointer; cdecl;
begin
  GetMem(result, Size);
end;

function lpfnReAllocate(Buffer: pointer; Size: integer): pointer; cdecl;
begin
  ReallocMem(Buffer, Size);
  result := Buffer;
end;

procedure lpfnFree(Buffer: pointer); cdecl;
begin
  FreeMem(Buffer);
end;

var
  FontSub: THandle = INVALID_HANDLE_VALUE;

function HasCreateFontPackage: boolean;
begin
  if FontSub = INVALID_HANDLE_VALUE then
  begin
    FontSub := SafeLoadLibrary('FontSub.dll');
    if FontSub <> 0 then
      CreateFontPackage := LibraryResolve(FontSub, 'CreateFontPackage');
  end;
  result := Assigned(@CreateFontPackage);
end;


initialization

finalization
  if (FontSub <> 0) and
     (FontSub <> INVALID_HANDLE_VALUE) then
    FreeLibrary(FontSub);

{$endif OSPOSIX}


end.

