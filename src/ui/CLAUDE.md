# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Overview

The `mORMot2/src/ui` folder contains **User Interface-related units** for VCL/LCL applications on Windows. These units provide cross-platform compatibility abstractions, custom visual components, ORM grid integration, GDI+ graphics support, and a complete PDF generation and reporting engine.

**Key Characteristic**: Most units are **Windows-only** or have limited cross-platform support. Units gracefully degrade to no-ops on POSIX systems.

---

## Architecture

### Unit Organization

```
mormot.ui.core       → Foundation (LCL/VCL compatibility, UI helpers)
     ↓
mormot.ui.controls   → Custom components (THintWindowDelayed, TSynLabeledEdit, JSON persistence)
mormot.ui.grid.orm   → ORM → Grid binding (TOrmTableToGrid)
mormot.ui.gdiplus    → GDI+ wrapper (TSynPicture: GIF/PNG/TIFF/JPG)
mormot.ui.pdf        → PDF engine (TPdfDocument, TPdfPage)
mormot.ui.report     → Report engine (TGdiPages: preview + PDF export)
```

### Unit Responsibilities

#### `mormot.ui.core` - Foundation Layer
- **LCL/FPC compatibility**: Bridges differences between Free Pascal's LCL and Delphi's VCL
- **Windows MetaFile support for FPC**: `TMetaFile` and `TMetaFileCanvas` classes (WinAPI wrappers)
- **High-level UI helpers**: Control manipulation, window positioning, theme support

#### `mormot.ui.controls` - Custom Components
- `THintWindowDelayed`: Auto-hiding hint window with UTF-8 support and timer-based dismissal
- `TSynLabeledEdit`: Extended `TLabeledEdit` with validation, formatting, and data binding
- `TUIComponentsPersist`: Serialize/deserialize UI component state to/from JSON

#### `mormot.ui.grid.orm` - ORM Grid Integration
- `TOrmTableToGrid`: Binds `TOrmTable` (ORM result set) to `TDrawGrid` for read-only display
- **Features**: Unicode support, column sorting, incremental search, marking/selection, custom rendering hooks
- **Standard actions**: Mark by date ranges (today/week/month, older than X), inverse marking
- **Events**: `TValueTextEvent`, `THintTextEvent`, `TRightClickCellEvent` for customization

#### `mormot.ui.gdiplus` - GDI+ Graphics (Windows Only)
- `TSynPicture`: Base class for GIF/PNG/TIFF/JPG using GDI+ library
- **Inheritors**: `TPngImage`, `TJpegImage`, `TGifImage`, `TTiffImage`
- **High-level API**: Load from file/stream/resource, save with compression options, convert to bitmap
- **Requires**: Windows XP+ with GDI+ (gdiplus.dll)

#### `mormot.ui.pdf` - PDF Engine (Windows Only)
- **Core classes**: `TPdfDocument` (document), `TPdfPage` (page rendering)
- **GDI support**: `TPdfDocumentGdi` for drawing via `TCanvas` (VCL/LCL compatibility)
- **Features**: PDF 1.3-1.7 support, PDF/A compliance (1A/1B, 2A/2B, 3A/3B), encryption (RC4), Uniscribe (Hebrew/Arabic/Asian text)
- **Conditionals**: `USE_PDFSECURITY`, `USE_UNISCRIBE`, `USE_SYNGDIPLUS`, `USE_METAFILE`

#### `mormot.ui.report` - Report Engine (Delphi + Windows Only)
- `TGdiPages`: Full report engine with UI preview and PDF export
- **Features**: Page headers/footers, columns, tables, images, metafiles, preview zoom, print, clipboard
- **Status**: Not compatible with FPC/LCL (extensive VCL dependencies)
- **Origin**: Forked from `TPages` (c) 2003 Angus Johnson with heavy modifications

---

## Platform Support

### Compilation Matrix

| Unit                | Windows (Delphi) | Windows (FPC) | POSIX (FPC) |
|---------------------|------------------|---------------|-------------|
| mormot.ui.core      | ✅ Full          | ✅ Full       | ⚠️ Limited  |
| mormot.ui.controls  | ✅ Full          | ✅ Full       | ⚠️ Limited  |
| mormot.ui.grid.orm  | ✅ Full          | ✅ Full       | ⚠️ Limited  |
| mormot.ui.gdiplus   | ✅ Full          | ✅ Full       | ❌ No-op    |
| mormot.ui.pdf       | ✅ Full          | ✅ Full       | ❌ No-op    |
| mormot.ui.report    | ✅ Full          | ❌ No-op      | ❌ No-op    |

**POSIX Behavior**: Units compile but provide minimal/no functionality (graceful degradation)

### Key Conditionals

```pascal
{$ifdef OSPOSIX}
  // Entire unit becomes no-op on POSIX
{$endif}

{$ifdef FPC}
  // Free Pascal specific code (LCL, compatibility types)
{$endif}

{$ifdef OSWINDOWS}
  // Windows-specific WinAPI calls
{$endif}

{$ifdef NEEDVCLPREFIX}
  // Delphi XE2+ requires "vcl." prefix
{$endif}
```

---

## Key Concepts

### VCL/LCL Cross-Compatibility

**Problem**: Free Pascal's LCL uses different type names and message constants than Delphi's VCL.

**Solution** (`mormot.ui.core`):
```pascal
{$ifdef FPC}
type
  TWMTimer = TLMTimer;
const
  WM_TIMER = LM_TIMER;
{$endif}
```

**FPC-specific MetaFile Support**:
- LCL lacks `TMetaFile`/`TMetaFileCanvas` on Windows
- Minimal WinAPI wrapper provided in `mormot.ui.core` (non-portable)

### ORM Table → Grid Binding Pattern

**Usage**:
```pascal
// Create association (Grid takes ownership of Table)
TOrmTableToGrid.Create(MyDrawGrid, MyOrmTable);

// Grid automatically:
// - Displays column headers from TOrmTable field names
// - Renders rows with proper Unicode handling
// - Sorts on column click
// - Provides incremental search (type to find)
// - Hides ID column by default
```

**Customization**:
- `OnValueText`: Override cell content dynamically
- `OnHintText`: Custom tooltips for truncated text
- `OnDrawCellBackground`: Custom cell rendering
- `MarkAllowed`: Enable checkbox column for row selection

### PDF Generation Workflow

**Simple API**:
```pascal
var
  PDF: TPdfDocument;
begin
  PDF := TPdfDocument.Create;
  try
    PDF.Info.Author := 'mORMot';
    with PDF.AddPage do begin
      SetFontAndSize(fnHelvetica, 12);
      TextOut(100, 700, 'Hello World');
    end;
    PDF.SaveToFile('output.pdf');
  finally
    PDF.Free;
  end;
end;
```

**Advanced (GDI integration)**:
```pascal
var
  PDFGDI: TPdfDocumentGdi;
begin
  PDFGDI := TPdfDocumentGdi.Create;
  try
    with PDFGDI.AddPage do begin
      Canvas.Font.Name := 'Arial';
      Canvas.TextOut(50, 50, 'Rendered via TCanvas');
    end;
    PDFGDI.SaveToFile('output.pdf');
  finally
    PDFGDI.Free;
  end;
end;
```

### Report Engine Workflow (`TGdiPages`)

**Typical Usage**:
```pascal
var
  Pages: TGdiPages;
begin
  Pages := TGdiPages.Create(Self);
  try
    Pages.BeginDoc;
    Pages.AddColumn(100, taLeft);
    Pages.AddColumn(200, taRight);
    Pages.DrawText('Column 1');
    Pages.DrawText('Column 2', [], True); // True = next line
    Pages.EndDoc;
    Pages.ShowPreview; // Or Pages.ExportPDF('report.pdf')
  finally
    Pages.Free;
  end;
end;
```

**Features**:
- Multi-column layouts with alignment
- Headers/footers with page numbering (use `PAGENUMBER` constant)
- Embedded images/metafiles
- Zoom modes: percent, page fit, page width
- Print with preview dialog

---

## Development Guidelines

### When Modifying UI Units

1. **Preserve cross-platform abstractions**: Test changes on both Delphi and FPC if possible
2. **Check conditional compilation**: Ensure `{$ifdef OSPOSIX}` blocks remain no-ops
3. **Respect Windows-only units**: Don't add POSIX code to `mormot.ui.pdf`/`mormot.ui.report` (architectural limitation)
4. **Unicode correctness**: All text should be UTF-8 aware (use `RawUtf8` internally, convert to `string` for VCL)

### Adding New Custom Controls

1. Add to `mormot.ui.controls.pas`
2. Implement both VCL and LCL compatibility paths
3. Use `{$ifdef FPC}` for LCL-specific adjustments
4. Register in `procedure Register` for IDE support (optional)

### PDF Engine Customization

**Compile-time flags** (in project options or unit defines):
- `NO_USE_PDFSECURITY`: Disable encryption (removes `mormot.crypt.core` dependency)
- `NO_USE_UNISCRIBE`: Disable Uniscribe (saves ~100KB, breaks Hebrew/Arabic)
- `NO_USE_SYNGDIPLUS`: Use default `jpeg` unit instead of GDI+ (limits image format support)
- `NO_USE_METAFILE`: Disable `TMetaFile` support (breaks `TPdfDocumentGdi`)

**Example** (lightweight PDF builds):
```pascal
// Add to project defines or create custom include:
{$define NO_USE_UNISCRIBE}
{$define NO_USE_METAFILE}
```

### Common Pitfalls

1. **Don't assume VCL on FPC**: Use `{$ifdef FPC}` / `{$ifndef FPC}` instead of `{$ifdef VCL}`
2. **POSIX units compile but don't work**: Check for `{$ifdef OSPOSIX}` before assuming functionality
3. **Report engine is Delphi-only**: `TGdiPages` won't work on FPC due to extensive VCL dependencies
4. **GDI+ requires runtime DLL**: `gdiplus.dll` must be available (Windows XP+)
5. **MetaFile on FPC is minimal**: Only basic WinAPI wrapper, not full cross-platform `TMetaFile`

---

## Dependencies

### Internal mORMot Units
- `mormot.core.*` - Foundation (base, os, unicode, text, datetime, buffers, rtti, data, json, variants)
- `mormot.db.core` - Database abstraction (for grid.orm)
- `mormot.orm.*` - ORM layer (for grid.orm)
- `mormot.rest.client` - REST client (for grid.orm)
- `mormot.lib.z` - Zlib compression (for PDF)
- `mormot.lib.gdiplus` - GDI+ low-level bindings
- `mormot.lib.uniscribe` - Uniscribe bindings (optional)
- `mormot.crypt.core`, `mormot.crypt.other` - Encryption (optional for PDF)

### External Dependencies
- **VCL/LCL**: `Graphics`, `Controls`, `Forms`, `Grids`, `ExtCtrls`, `Themes`
- **WinAPI**: `Windows`, `Messages`, `ActiveX`, `Winspool`, `Printers`
- **RTL**: `SysUtils`, `Classes`, `Variants`, `Math`, `DateUtils`

---

## Testing & Validation

### Build Verification
```bash
# Test compilation across environments:
cd /mnt/w/mORMot2/test

# Delphi 7
compilD7.bat

# Delphi 2007/2010
compilD2007.bat
compilD2010.bat

# FPC (cross-platform)
./build_fpc.sh
```

### Manual Testing Checklist
- [ ] ORM grid displays Unicode correctly
- [ ] PDF encryption produces readable files with password
- [ ] GDI+ loads PNG/JPG/GIF/TIFF images
- [ ] Report preview zoom modes work (fit/width/percent)
- [ ] MetaFile rendering in PDF (if enabled)
- [ ] FPC builds compile without errors (even if POSIX is no-op)

---

## Related Documentation

- **mORMot2 Main**: `/mnt/w/mORMot2/README.md` - Framework overview
- **Core Units**: `/mnt/w/mORMot2/src/core/` - Foundation layer
- **ORM Units**: `/mnt/w/mORMot2/src/orm/` - Database abstraction
- **Test Suite**: `/mnt/w/mORMot2/test/` - Regression tests

---

**Last Updated**: 2025-10-10
**Framework**: mORMot 2 (Open Source, MPL/GPL/LGPL)
**Compatibility**: Delphi 7-12, FPC 3.2.3, Lazarus 2.2.5
