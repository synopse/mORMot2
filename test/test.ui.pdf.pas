/// regression tests for mormot.ui.pdf
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.ui.pdf;

interface

{$I ..\src\mormot.defines.inc}

{$ifdef HAS_UI_PDF} // VCL/LCL + Windows

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.buffers,
  mormot.core.os,
  mormot.core.test,
  mormot.core.unicode,
  mormot.core.text,
  mormot.lib.z,
  mormot.ui.pdf;

type
  /// regression tests for mormot.ui.pdf
  TTestUiPdf = class(TSynTestCase)
  published
    procedure FontEmbeddingAndHashValidation;
    procedure MetafileRenderingAndOrientation;
    procedure SimplePDFCreation;
    procedure TextInsertion;
    procedure MultiplePages;
    procedure PDFMetadata;
    procedure UnicodeWidth;
    procedure EscapeBackspace;
    procedure EncryptedUnicodeString;
    procedure WinAnsiToUnicodeBlocks;
    procedure OutlineBeforePage;
    procedure AddFontKeepsIndexes;
    procedure NewDocDropsPage;
    procedure RasterFontCache;
    procedure NewDocRenewsKey;
    procedure NewDocEndsDirectSave;
    procedure CatalogGetters;
    procedure JpegColorSpace;
    procedure ZeroAdvanceMeasured;
    procedure ReducedFontChecksum;
    procedure BidiEmbeddingOrder;
    procedure EmfPlusClipRestored;
    procedure EmfPlusRestoreDepth;
    procedure EmfPlusNullPen;
    procedure EmfPlusViewportOrigin;
    procedure EmfPlusPlaybackOrder;
    procedure EmfPlusWindowOrigin;
    procedure EmfPlusRecordLayout;
    procedure EmfAlphaBlendOpaque;
    procedure EmfPolyPolylineOpen;
    procedure EmfRestoreDCDepth;
    procedure EmfScaledRotation;
    procedure EmfAnsiText;
    procedure EmfAnsiStockFont;
    procedure EmfDIBitsKept;
    procedure EmfPrinterResZero;
    procedure ReducedFontSearchFields;
  end;

implementation

uses
{$ifdef FPC}
  Windows,
  Graphics,
  mormot.ui.core,
  {$ifdef NO_USE_SYNGDIPLUS} jpeg {$else} mormot.ui.gdiplus {$endif},
  mormot.lib.gdiplus
{$else}
  {$ifdef NEEDVCLPREFIX}
  WinApi.Windows,
  VCL.Graphics,
  {$ifdef NO_USE_SYNGDIPLUS} jpeg {$else} mormot.ui.gdiplus {$endif},
  mormot.lib.gdiplus
  {$else}
  Windows,
  Graphics,
  {$ifdef NO_USE_SYNGDIPLUS} jpeg {$else} mormot.ui.gdiplus {$endif},
  mormot.lib.gdiplus
  {$endif NEEDVCLPREFIX}
{$endif FPC}
  ;

const
  FIXED_DATE = 40339.803675; // forced date to have the exact same Hash32 value

procedure TTestUiPdf.FontEmbeddingAndHashValidation;
var
  pdf: TPdfDocument;
  MS: TMemoryStream;
  i, y: integer;
  embed: boolean;
  h: cardinal;
  WS: SynUnicode;
const
  Hash: array[0 .. 5] of cardinal  = ( // depends on OS available font
    $8B40C230, $8DCCC22A, $753E2D40, $6870208B, $33E290AB, $17842091);
  Name: array[boolean] of PDFString = ('Arial', 'Helvetica');
begin
  MS := TMemoryStream.Create;
  pdf := TPdfDocument.Create;
  try
    for embed := false to true do
    begin
      // no FIXED_DATE nor creator variation in hashed value
      pdf.Info.CreationDate := FIXED_DATE;
      pdf.Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
      // draw something on the page
      pdf.StandardFontsReplace := embed;
      pdf.AddPage;
      pdf.Canvas.SetFont('arial', 10, []);
      CheckEqual(pdf.Canvas.Page.Font.Name, Name[embed]);
      y := 800;
      for i := 1 to 30 do
      begin
        pdf.Canvas.SetFont('Arial', 9 + i, []);
        Utf8ToSynUnicode(Make(['Texte accentue n.', i]), WS);
        PWordArray(WS)^[13] := 233;
        PWordArray(WS)^[16] := 176;
        pdf.Canvas.TextOutW(100, y, pointer(WS));
        dec(y, 9 + i);
      end;
      pdf.SaveToStream(MS, FIXED_DATE);
      h := Hash32(MS.Memory, PtrInt(MS.Position));
      CheckUtf8(IntegerScanExists(@Hash, length(Hash), h), '%=%',
        [Name[embed], CardinalToHexShort(h)]);
      if embed or
         (pdf.CharSet <> ANSI_CHARSET) then
        break; // StandardFontsReplace will work only with ANSI code page
      pdf.NewDoc;
      MS.Clear;
    end;
  finally
    pdf.Free;
    MS.Free;
  end;
end;

procedure TTestUiPdf.SimplePDFCreation;
var
  Pdf: TPdfDocument;
  PdfStream: TMemoryStream;
  PdfContent: RawByteString;
begin
  PdfStream := TMemoryStream.Create;
  try
    Pdf := TPdfDocument.Create;
    try
      // Create a new page and immediately save to confirm that
      // we can generate a valid empty (or near-empty) PDF.
      Pdf.AddPage;
      Pdf.SaveToStream(PdfStream);
    finally
      Pdf.Free;
    end;

    // Basic checks
    Check(PdfStream.Size > 0, 'PDF stream should not be empty');
    SetString(PdfContent, PAnsiChar(PdfStream.Memory), PdfStream.Size);
    Check(Copy(PdfContent, 1, 5) = '%PDF-', 'PDF header is missing or invalid');
    Check(Copy(PdfContent, Length(PdfContent) - 5, 6) = '%%EOF'#10,
      'PDF trailer is missing or invalid');
  finally
    PdfStream.Free;
  end;
end;

procedure TTestUiPdf.TextInsertion;
const
  EXPECTED_TEXT = 'Hello from mORMot PDF';
var
  Pdf: TPdfDocumentGdi;
  PdfStream: TMemoryStream;
  PdfContent: RawByteString;
begin
  PdfStream := TMemoryStream.Create;
  try
    Pdf := TPdfDocumentGdi.Create;
    try
      Pdf.CompressionMethod := cmNone;
      Pdf.AddPage;
      // Set the font, size
      Pdf.VclCanvas.Font.Name := 'Arial';
      Pdf.VclCanvas.Font.Size := 12;
      // Draw some text
      Pdf.VclCanvas.TextOut(100, 100, EXPECTED_TEXT);
      // Save the PDF to a stream
      Pdf.SaveToStream(PdfStream);
    finally
      Pdf.Free;
    end;
    // Basic checks on the resulting stream
    Check(PdfStream.Size > 0, 'PDF stream should not be empty after adding text');
    SetString(PdfContent, PAnsiChar(PdfStream.Memory), PdfStream.Size);
    Check(PosEx(EXPECTED_TEXT, PdfContent) <> 0,
      'Expected text not found in the PDF content');
  finally
    PdfStream.Free;
  end;
end;

const
  EMF: RawByteString = // some compressed EMF file content
  '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' +
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZig0sady5L92TWYpNw5NZik3Dk1mKTcOTWYpNw5NZik3Dk1mK' +
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZLDbWy3syS7GBJY0796V7MkuxaXgyi8XGenlPZik2DU9msdhY' +
  'L+/JLBYb6+U9maXYNDyZxWJjvbwnsxSbhiezFJuGJ7NYbKyX92SWYtPwZJZiA0saG+tL92QWi4318p7M' +
  'UmwansxSbBqezGKxsV7ek1mKTcOTWSw21st7MovFxnp5T2YpNg1PZrHYWC/vySzFpuHJLMWm4cksFhvr' +
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysl/dklmLT8GQWi41NWNLYWENsGp7MUmwansxi' +
  'sbFe3pNZLDbWy3syS7FpeDKLxcZ6eU9mKTYNT2YpNg1PZrHYWMYm4cksxabhySzFpuHJLBYb6+U9maXY' +
  'NDyZpdg0PJnFYmO9vCezFJuGJ7NYbGzCksbGGmLT8GSWYtPwZBaLjU1YwCbhySzFpuHJLMWm4cksFhvr' +
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hs' +
  'rIIns1hsrIInsxSbhiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb' +
  'hiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb' +
  'hiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezWGysgiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MU' +
  'W7GezMWwYjowNF/uYhwkLfNBa2urywyrfFtT6t0MN9vNc3PD+NCG6Y1TrprWiAIr0vlD3I6Fw9woP8Q1' +
  'jz/dDXej3ZClzk14r9LVXTW1kca7FgzLrfNaWG5KyG/40tuNtM7LvavpM665+Q+G+dsXdVoc3x5vK5fK' +
  'WP0ebanRbmgol+oYnDQrW+6mTLl3Hln9FZS7n5W7K+Gi8ZIw3vqLIZ2yZd44wufWeWzS1Ebaxn17Dfzy' +
  'CW6jv67RdXcuf1/o69I+T+fTdim0j3Ry8TccmqZuqswsX2i+d/n7Qnv9f1RSHw29k+n/Bw==';

// this function is not available in PUREMORMOT2 mode
function UncompressString(const data: RawByteString): RawByteString;
begin
  SetLength(result, PCardinal(data)^);
  SetLength(result, UncompressMem(@PByteArray(data)[12], pointer(result),
    length(data) - 12, length(result)));
end;

procedure TTestUiPdf.MetafileRenderingAndOrientation;
var S: RawByteString;
    MS: TMemoryStream;
    MF: TMetaFile;
    Doc: TPdfDocument;
    Page: TPdfPage;
    orientation: boolean;
    hashes: array[boolean] of cardinal;
    i,j: integer;
begin
  S := UncompressString(Base64ToBin(EMF));
  CheckHash(S, $5BB4C8B1);
  MS := TMemoryStream.Create;
  try
    with TPdfDocument.Create do
    try
      Info.CreationDate := FIXED_DATE; // force fixed date and creator for Hash32()
      Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
      AddPage;
      MF := TMetaFile.Create;
      try
        MS.Write(pointer(S)^, length(S));
        MS.Position := 0;
        MF.LoadFromStream(MS);
        RenderMetaFile(Canvas, MF);
        Check(Canvas.Page.Font.Name = 'Tahoma');
      finally
        MF.Free;
      end;
      MS.Clear;
      SaveToStream(MS, FIXED_DATE);
      // force constant Arial,Bold and Tahoma FontBBox
      SetString(S, PAnsiChar(MS.Memory), MS.Position);
      if (Unicode_CodePage <> 1252) {$ifdef CPU64} or true{$endif} then
        Check(length(S) > 6500)
      else
      begin
        i := PosEx('/FontBBox[', S);
        if CheckFailed(i <> 0) then
          exit;
        FillCharFast(S[i], 32, 32);
        j := PosEx('/FontBBox[', S);
        if CheckFailed(j <> 0) then
          exit;
        FillCharFast(S[j], 32, 32);
        i := PosEx('/FontBBox[', S);
        if CheckFailed(i <> 0) then
          exit;
        FillCharFast(S[i], 32, 32);
        CheckHash(S, $456748FC); // the boxes of #686
      end;
    finally
      Free;
    end;
    MF := TMetafile.Create;
    try
      // create test metafile
      MF.Width := 700;
      MF.Height := 700;
      with TMetafileCanvas.Create(MF, 0) do
      try
        MoveTo(0, 0);
        LineTo(700, 700);
        MoveTo(0, 700);
        LineTo(700, 0);
      finally
        Free;
      end;
      // create page in portrait/landscape orientation, and render metafile to it
      for orientation := false to true do
      begin
        Doc := TPdfDocument.Create;
        try
          Doc.GeneratePDF15File := True;
          Doc.Info.CreationDate := FIXED_DATE; // force fixed date for Hash32()
          Doc.Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
          Doc.CompressionMethod := cmNone;
          Doc.DefaultPaperSize := psA4;
          Page := Doc.AddPage;
          Page.PageLandscape := orientation;
          Check(Page.PageLandscape = orientation);
          if orientation then
          begin
            CheckEqual(842, Page.PageWidth);
            CheckEqual(595, Page.PageHeight);
          end
          else
          begin
            CheckEqual(595, Page.PageWidth);
            CheckEqual(842, Page.PageHeight);
          end;
          MS.Clear;
          RenderMetaFile(Doc.Canvas, MF);
          Doc.SaveToStream(MS, FIXED_DATE);
          Check(MS.Size > 0);
          SetString(S, PAnsiChar(MS.Memory), MS.Position);
          Check(CountOccurrences(' l'#10, S) >= 2,
            'Metafile lines were not rendered into the PDF content');
          Check(CountOccurrences('S'#10, S) >= 2,
            'Metafile strokes were not rendered into the PDF content');
          hashes[orientation] := Hash32(MS.Memory, integer(MS.Position));
        finally
          Doc.Free;
        end;
      end;
      CheckNotEqual(hashes[false], hashes[true]);
    finally
      MF.Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure TTestUiPdf.MultiplePages;
var
  Pdf: TPdfDocumentGdi;
  PdfStream: TMemoryStream;
  PdfContent: RawByteString;
  PageCount: integer;
begin
  PdfStream := TMemoryStream.Create;
  try
    Pdf := TPdfDocumentGdi.Create;
    try
      Pdf.GeneratePDF15File := false;
      // Add multiple pages
      Pdf.AddPage;
      Pdf.VclCanvas.TextOut(50, 50, 'Page 1');
      Pdf.AddPage;
      Pdf.VclCanvas.TextOut(50, 50, 'Page 2');
      Pdf.AddPage;
      Pdf.VclCanvas.TextOut(50, 50, 'Page 3');
      Pdf.SaveToStream(PdfStream);
    finally
      Pdf.Free;
    end;
    // Quick checks
    Check(PdfStream.Size > 0, 'PDF stream should not be empty after multiple pages');
    SetString(PdfContent, PAnsiChar(PdfStream.Memory), PdfStream.Size);
    PageCount := CountOccurrences('/Type/Page/Parent', PdfContent);
    CheckEqual(PageCount, 3, 'The number of pages in the PDF is incorrect');
  finally
    PdfStream.Free;
  end;
end;

procedure TTestUiPdf.PDFMetadata;
var
  Pdf: TPdfDocumentGDI;
  PdfStream: TMemoryStream;
  PdfContent: RawByteString;
begin
  PdfStream := TMemoryStream.Create;
  try
    Pdf := TPdfDocumentGDI.Create;
    try
      Pdf.GeneratePDF15File := false;
      Pdf.Info.Author := 'mORMot Test Framework';
      Pdf.Info.Title := 'PDF (Metadata) \ Test';
      Pdf.Info.Subject := 'Testing PDF Metadata Fields';
      Pdf.Info.Creator := 'mORMot2 tests';
      Pdf.Info.Keywords := 'pdf, mORMot';
      Check(Pdf.Info.Title = 'PDF (Metadata) \ Test');
      Pdf.AddPage;
      Pdf.SaveToStream(PdfStream);
    finally
      Pdf.Free;
    end;
    Check(PdfStream.Size > 0, 'PDF stream should not be empty after adding metadata');
    SetString(PdfContent, PAnsiChar(PdfStream.Memory), PdfStream.Size);
    Check(PosEx('/Author(mORMot Test Framework)', PdfContent) <> 0,
      'PDF metadata author field is incorrect or missing');
    Check(PosEx('/Title(PDF \(Metadata\) \\ Test)', PdfContent) <> 0,
      'PDF metadata title field is incorrect or missing');
    Check(PosEx('/Subject(Testing PDF Metadata Fields)', PdfContent) <> 0,
      'PDF metadata subject field is incorrect or missing');
    Check(PosEx('/Creator(mORMot2 tests)', PdfContent) <> 0,
      'PDF metadata creator field is incorrect or missing');
    Check(PosEx('/Keywords(pdf, mORMot)', PdfContent) <> 0,
      'PDF metadata keywords field is incorrect or missing');
  finally
    PdfStream.Free;
  end;
end;

function SavedPdf(Doc: TPdfDocument): RawByteString;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    Doc.SaveToStream(ms, FIXED_DATE);
    SetString(result, PAnsiChar(ms.Memory), ms.Size);
  finally
    ms.Free;
  end;
end;

function Nibble(c: AnsiChar): integer;
begin
  case c of
    '0'..'9':
      result := ord(c) - ord('0');
    'A'..'F':
      result := ord(c) - (ord('A') - 10);
    'a'..'f':
      result := ord(c) - (ord('a') - 10);
  else
    result := 0;
  end;
end;

// the bytes of the (literal) or <hex> string written after Key
function PdfStringAfter(const s, Key: RawByteString): RawByteString;
var
  p: integer;
begin
  result := '';
  p := PosEx(Key, s);
  if p = 0 then
    exit;
  inc(p, length(Key));
  while (p <= length(s)) and
        (s[p] = ' ') do
    inc(p);
  if p > length(s) then
    exit;
  if s[p] = '<' then
  begin
    inc(p);
    while (p < length(s)) and
          (s[p] <> '>') do
    begin
      result := result + AnsiChar(Nibble(s[p]) shl 4 + Nibble(s[p + 1]));
      inc(p, 2);
    end;
  end
  else if s[p] = '(' then
  begin
    inc(p);
    while (p <= length(s)) and
          (s[p] <> ')') do
    begin
      if s[p] = '\' then
      begin
        inc(p);
        case s[p] of
          'n':
            result := result + #10;
          'r':
            result := result + #13;
          't':
            result := result + #9;
          'b':
            result := result + #8;
          'f':
            result := result + #12;
          '0'..'7':
            begin
              result := result + AnsiChar((ord(s[p]) - 48) * 64 +
                (ord(s[p + 1]) - 48) * 8 + (ord(s[p + 2]) - 48));
              inc(p, 2);
            end;
        else
          result := result + s[p];
        end;
      end
      else
        result := result + s[p];
      inc(p);
    end;
  end;
end;

procedure TTestUiPdf.UnicodeWidth;
var
  pdf: TPdfDocument;
  w: array[0..1] of WideChar;
begin
  // FPC read fUsedWide[] before FindOrAddUsedWideChar reallocated it
  pdf := TPdfDocument.Create;
  try
    pdf.AddPage;
    pdf.Canvas.SetFont('Arial', 10, []);
    w[1] := #0;
    w[0] := WideChar($0418); // not WinAnsi: drawn before it was ever measured
    pdf.Canvas.TextOutW(20, 700, @w);
    w[0] := WideChar($0416);
    Check(pdf.Canvas.UnicodeTextWidth(@w) > 0, 'measured');
    Check(SavedPdf(pdf) <> '', 'saved');
  finally
    pdf.Free;
  end;
end;

procedure TTestUiPdf.EscapeBackspace;
var
  pdf: TPdfDocument;
  s: RawByteString;
begin
  pdf := TPdfDocument.Create;
  try
    pdf.GeneratePDF15File := false;
    pdf.CompressionMethod := cmNone;
    pdf.Info.Subject := 'a'#8'b';
    pdf.AddPage;
    s := SavedPdf(pdf);
  finally
    pdf.Free;
  end;
  // ISO 32000-1 7.3.4.2 defines \b - a reader takes \B as a plain B
  Check(PosEx('/Subject(a\bb)', s) > 0, 'backspace escaped as \b');
end;

procedure TTestUiPdf.EncryptedUnicodeString;
{$ifndef NO_USE_PDFSECURITY}
const
  KNOWN = 'ABCDEFGHIJKLMNOPQRSTUVWX';
var
  pdf: TPdfDocument;
  w: array[0..1] of WideChar;
  s, sub, tit: RawByteString;
  i: integer;
begin
  w[0] := WideChar($0416); // two Cyrillic letters: a <FEFF..> text string
  w[1] := WideChar($0417);
  pdf := TPdfDocument.Create(false, 0, pdfaNone,
    TPdfEncryption.New(elRC4_40, '', 'owner', PDF_PERMISSION_ALL));
  try
    pdf.GeneratePDF15File := false;
    pdf.CompressionMethod := cmNone;
    pdf.Info.Subject := KNOWN;
    pdf.Info.Data.AddItem('Title', TPdfTextUtf8.Create(RawUnicodeToUtf8(@w, 2)));
    pdf.AddPage;
    s := SavedPdf(pdf);
  finally
    pdf.Free;
  end;
  // RC4 restarts for each string of an object: the known Subject gives the
  // key stream, which decrypts the Title of the same Info dictionary
  sub := PdfStringAfter(s, '/Subject');
  tit := PdfStringAfter(s, '/Title');
  if not CheckEqual(length(sub), length(KNOWN), 'subject') or
     not CheckEqual(length(tit), 6, 'title: BOM and two characters') then
    exit;
  for i := 1 to length(tit) do
    tit[i] := AnsiChar(ord(tit[i]) xor ord(sub[i]) xor ord(KNOWN[i]));
  Check(tit = #$fe#$ff#$04#$16#$04#$17, 'BOM and UTF-16BE encrypted together');
end;
{$else}
begin
end;
{$endif NO_USE_PDFSECURITY}

procedure TTestUiPdf.WinAnsiToUnicodeBlocks;
var
  pdf: TPdfDocument;
  txt: PdfString;
  s: RawByteString;
  c, p, q, n, blocks: integer;
begin
  txt := '';
  for c := 33 to 255 do
    if (c < 127) or
       (c > 160) then
      txt := txt + AnsiChar(c);
  pdf := TPdfDocument.Create(false, 0, pdfa1B);
  try
    pdf.CompressionMethod := cmNone;
    pdf.AddPage;
    pdf.Canvas.SetFont('Arial', 10, []);
    pdf.Canvas.TextOut(20, 700, txt);
    s := SavedPdf(pdf);
  finally
    pdf.Free;
  end;
  // at most 100 entries per bfchar block (Adobe CMap specification, 7.4)
  blocks := 0;
  p := PosEx(' beginbfchar', s);
  while p > 0 do
  begin
    q := p;
    while (q > 1) and
          (s[q - 1] in ['0'..'9']) do
      dec(q);
    n := GetInteger(@s[q]);
    CheckUtf8(n <= 100, 'beginbfchar %', [n]);
    inc(blocks);
    p := PosEx(' beginbfchar', s, p + 1);
  end;
  Check(blocks > 1, 'the WinAnsi /ToUnicode needs several blocks');
end;

procedure TTestUiPdf.OutlineBeforePage;
var
  pdf: TPdfDocument;
  s: RawByteString;
  p, q: integer;
  raised: boolean;
begin
  pdf := TPdfDocument.Create(true);
  try
    pdf.GeneratePDF15File := false;
    pdf.CompressionMethod := cmNone;
    raised := false;
    try
      pdf.CreateOutline('no page yet', 0, 100);
    except
      on EPdfInvalidOperation do
        raised := true;
    end;
    Check(raised, 'an outline with a position needs a page');
    Check(pdf.OutlineRoot.First = nil, 'no entry left');
    pdf.AddPage;
    pdf.CreateOutline('on a page', 0, 100);
    Check(pdf.OutlineRoot.First <> nil);
    s := SavedPdf(pdf);
    // the root counts the one entry, not the failed one too
    p := PosEx('/Type/Outlines', s);
    q := PosEx('>>', s, p);
    if Check((p > 0) and (q > p), 'Outlines dictionary') then
      Check(PosEx('/Count 1', copy(s, p, q - p)) > 0, 'one entry counted');
  finally
    raised := false;
    try
      pdf.Free; // the failed entry must not be freed a second time
    except
      raised := true;
    end;
    Check(not raised, 'document freed');
  end;
end;

type
  TPdfDocumentAccess = class(TPdfDocument); // GetTrueTypeFontIndex

procedure TTestUiPdf.AddFontKeepsIndexes;
var
  pdf: TPdfDocumentAccess;
  verdana: TPdfFont;
  i: integer;
begin
  pdf := TPdfDocumentAccess.Create;
  try
    pdf.FontFallBackName := 'Tahoma';
    i := pdf.GetTrueTypeFontIndex('Verdana');
    if not Check(i >= 0, 'Verdana installed') then
      exit;
    pdf.AddPage;
    verdana := pdf.Canvas.SetFont('Verdana', 10, []); // created before the insert
    pdf.Canvas.SetFont('No Such mORMot Font', 10, []); // resolved and cached
    // sorts first: every index after it moves
    Check(pdf.AddTrueTypeFont('AAA mORMot Test Font'));
    CheckEqual(pdf.FontFallBackName, 'Tahoma', 'fallback kept');
    CheckEqual(pdf.GetTrueTypeFontIndex('Verdana'), i + 1, 'name cache');
    Check(pdf.Canvas.SetFont('Verdana', 10, []) = verdana, 'created font reused');
    // a style not created yet: made from the cached index of the raster font
    Check(pdf.Canvas.SetFont('No Such mORMot Font', 10, [pfsBold]) =
          pdf.Canvas.SetFont('Tahoma', 10, [pfsBold]), 'raster font');
  finally
    pdf.Free;
  end;
end;

procedure TTestUiPdf.NewDocDropsPage;
var
  pdf: TPdfDocument;
  page: TPdfPage;
  raised: boolean;
  s: RawByteString;
begin
  pdf := TPdfDocument.Create;
  try
    pdf.AddPage;
    Check(pdf.Canvas.Page <> nil);
    pdf.NewDoc; // frees the page and its content stream
    raised := false;
    try
      page := pdf.Canvas.Page; // raises without a page
      Check(page = nil);
    except
      on EPdfInvalidOperation do
        raised := true;
    end;
    Check(raised, 'no reference to the freed page');
  finally
    pdf.Free;
  end;
  {$ifndef NO_USE_PDFSECURITY}
  // the /Encrypt dictionary was freed with the trailer of the first document
  pdf := TPdfDocument.Create(false, 0, pdfaNone,
    TPdfEncryption.New(elRC4_40, '', 'owner', PDF_PERMISSION_ALL));
  try
    pdf.AddPage;
    pdf.NewDoc;
    pdf.AddPage;
    raised := false;
    try
      s := SavedPdf(pdf);
    except
      raised := true;
    end;
    Check(not raised and (PosEx('/Encrypt', s) > 0), 'encrypted after NewDoc');
  finally
    pdf.Free;
  end;
  {$endif NO_USE_PDFSECURITY}
end;

procedure TTestUiPdf.RasterFontCache;
const
  UNKNOWN = 'No Such mORMot Font';
var
  pdf: TPdfDocument;
  form: TPdfFormWithCanvas;
begin
  pdf := TPdfDocument.Create;
  try
    pdf.FontFallBackName := 'Tahoma';
    form := TPdfFormWithCanvas.Create(pdf, 200, 100);
    pdf.AddXObject('FormR', form); // owned by the document from here
    form.Canvas.SetFont(UNKNOWN, 10, []); // resolved and cached
    // sorts first: every index after it moves
    Check(pdf.AddTrueTypeFont('AAA mORMot Test Font'));
    Check(form.Canvas.SetFont(UNKNOWN, 10, [pfsItalic]) =
          form.Canvas.SetFont('Tahoma', 10, [pfsItalic]), 'form canvas');
    form.CloseCanvas;
    pdf.AddPage;
    pdf.Canvas.SetFont(UNKNOWN, 10, []); // resolved and cached
    pdf.FontFallBackName := 'Verdana';
    Check(pdf.Canvas.SetFont(UNKNOWN, 10, []) =
          pdf.Canvas.SetFont('Verdana', 10, []), 'new fallback');
    Check(pdf.Canvas.SetFont(UNKNOWN, 10, [], -1, -1, true) =
          pdf.Canvas.SetFont('Courier New', 10, []), 'fixed width');
  finally
    pdf.Free;
  end;
end;

procedure TTestUiPdf.NewDocRenewsKey;
{$ifndef NO_USE_PDFSECURITY}
var
  pdf: TPdfDocument;
  first, second: RawByteString;
begin
  pdf := TPdfDocument.Create(false, 0, pdfaNone,
    TPdfEncryption.New(elRC4_128, '', 'owner', PDF_PERMISSION_ALL));
  try
    pdf.CompressionMethod := cmNone;
    pdf.Info.Subject := 'AAAAAAAA';
    pdf.AddPage;
    first := PdfStringAfter(SavedPdf(pdf), '/Subject');
    pdf.NewDoc; // a new file ID, so a new key
    pdf.Info.Subject := 'AAAAAAAA';
    pdf.AddPage;
    second := PdfStringAfter(SavedPdf(pdf), '/Subject');
  finally
    pdf.Free;
  end;
  // with empty pages, the Info dictionary is the last object encrypted by one
  // save and the first by the next: the RC4 key cached for it was the old one
  CheckEqual(length(second), 8, 'subject');
  Check(second <> first, 'encrypted with the key of the new document');
end;
{$else}
begin
end;
{$endif NO_USE_PDFSECURITY}

procedure TTestUiPdf.NewDocEndsDirectSave;
var
  pdf: TPdfDocument;
  left: TMemoryStream;
  s: RawByteString;
begin
  left := TMemoryStream.Create;
  try
    pdf := TPdfDocument.Create;
    try
      pdf.AddPage;
      pdf.SaveToStreamDirectBegin(left); // never ended
      pdf.NewDoc;
      pdf.AddPage;
      s := SavedPdf(pdf); // raised "SaveToStreamDirectBegin called twice"
    finally
      pdf.Free;
    end;
  finally
    left.Free;
  end;
  Check(PosEx('%%EOF', s) > 0, 'saved after NewDoc');
end;

procedure TTestUiPdf.CatalogGetters;
var
  pdf: TPdfDocument;
begin
  pdf := TPdfDocument.Create;
  try
    Check(pdf.Info.HasData, 'HasData');
    pdf.Root.NonFullScreenPageMode := pmUseOutlines;
    Check(pdf.Root.NonFullScreenPageMode = pmUseOutlines,
      'NonFullScreenPageMode reads ViewerPreferences');
  finally
    pdf.Free;
  end;
end;

const
  // 8 x 8 pixels, one component (grayscale) and four (CMYK)
  JPEG_GRAY = '/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDABALDA4MChAODQ4SERATGCgaGBYWGDEj' +
    'JR0oOjM9PDkzODdASFxOQERXRTc4UG1RV19iZ2hnPk1xeXBkeFxlZ2P/wAALCAAIAAgBAREA' +
    '/8QAFAABAAAAAAAAAAAAAAAAAAAAAP/EABQQAQAAAAAAAAAAAAAAAAAAAAD/2gAIAQEAAD8A' +
    'P//Z';
  JPEG_CMYK = '/9j/7gAOQWRvYmUAZAAAAAAA/9sAQwAQCwwODAoQDg0OEhEQExgoGhgWFhgxIyUd' +
    'KDozPTw5Mzg3QEhcTkBEV0U3OFBtUVdfYmdoZz5NcXlwZHhcZWdj/8AAFAgACAAIBEMRAE0R' +
    'AFkRAEsRAP/EABUAAQEAAAAAAAAAAAAAAAAAAAcB/8QAFBABAAAAAAAAAAAAAAAAAAAAAP/a' +
    'AA4EQwBNAFkASwAAPwBAQfkB/9k=';

procedure TTestUiPdf.JpegColorSpace;
var
  pdf: TPdfDocument;
  jpg: TJpegImage;
  ms: TMemoryStream;
  img: TPdfImage;
  raised: boolean;
  nojpeg: RawByteString;

  procedure Load(const b64: RawUtf8);
  var
    bin: RawByteString;
  begin
    bin := Base64ToBin(b64);
    ms.Clear;
    ms.WriteBuffer(pointer(bin)^, length(bin));
    ms.Position := 0;
  end;

begin
  pdf := TPdfDocument.Create;
  ms := TMemoryStream.Create;
  try
    pdf.AddPage;
    // a grayscale TJpegImage is not RGB
    Load(JPEG_GRAY);
    jpg := TJpegImage.Create;
    try
      jpg.LoadFromStream(ms);
      img := TPdfImage.Create(pdf, jpg, true);
      try
        CheckEqual(img.Attributes.PdfNameByName('ColorSpace').Value,
          'DeviceGray', 'TJpegImage');
      finally
        img.Free;
      end;
    finally
      jpg.Free;
    end;
    // what cannot be written raises, nothing incomplete is left
    Load(JPEG_CMYK);
    jpg := TJpegImage.Create;
    try
      jpg.LoadFromStream(ms);
      raised := false;
      try
        TPdfImage.Create(pdf, jpg, true).Free;
      except
        on EPdfInvalidValue do
          raised := true;
      end;
      Check(raised, 'CMYK TJpegImage');
    finally
      jpg.Free;
    end;
    Load(JPEG_CMYK);
    raised := false;
    try
      TPdfImage.CreateJpegDirect(pdf, ms, true).Free;
    except
      on EPdfInvalidValue do
        raised := true;
    end;
    Check(raised, 'CMYK JPEG');
    nojpeg := 'some text, not a JPEG';
    ms.Clear;
    ms.WriteBuffer(pointer(nojpeg)^, length(nojpeg));
    raised := false;
    try
      TPdfImage.CreateJpegDirect(pdf, ms, true).Free;
    except
      on EPdfInvalidValue do
        raised := true;
    end;
    Check(raised, 'not a JPEG');
  finally
    ms.Free;
    pdf.Free;
  end;
end;

procedure TTestUiPdf.ZeroAdvanceMeasured;
var
  pdf: TPdfDocument;
  w: array[0..2] of WideChar;
  w1, w2: single;
begin
  pdf := TPdfDocument.Create;
  try
    pdf.AddPage;
    pdf.Canvas.SetFont('Arial', 10, []);
    w[0] := 'a';
    w[1] := #0;
    w1 := pdf.Canvas.UnicodeTextWidth(@w);
    w[1] := WideChar($0301); // combining acute accent: advances by 0
    w[2] := #0;
    w2 := pdf.Canvas.UnicodeTextWidth(@w);
    CheckSame(w1, w2, 1E-6, 'a combining mark adds no width');
  finally
    pdf.Free;
  end;
end;

procedure TTestUiPdf.ReducedFontChecksum;
{$ifndef NO_USE_UNISCRIBE}
var
  pdf: TPdfDocument;
  s: RawByteString;
  p, len, i: integer;
  sum: cardinal;
begin
  pdf := TPdfDocument.Create;
  try
    pdf.GeneratePDF15File := false;
    pdf.CompressionMethod := cmNone;
    pdf.EmbeddedTtf := true;
    pdf.AddPage;
    pdf.Canvas.SetFont('Arial', 10, []);
    pdf.Canvas.TextOut(20, 700, 'Hello');
    s := SavedPdf(pdf);
  finally
    pdf.Free;
  end;
  // the subset of a hinted face keeps 10 tables, so ReduceTTF rebuilds it
  p := PosEx('/Length1 ', s);
  if not Check(p > 0, 'FontFile2') then
    exit;
  len := GetInteger(@s[p + 9]);
  p := PosEx('stream', s, p);
  if not Check(p > 0, 'stream') then
    exit;
  inc(p, 6);
  if s[p] = #13 then
    inc(p);
  inc(p); // #10
  if not Check(p + len - 1 <= length(s), 'font data') then
    exit;
  // the whole font sums to $B1B0AFBA, its tables read as big-endian words
  sum := 0;
  i := 0;
  while i < len do
  begin
    sum := sum + cardinal(ord(s[p + i])) shl 24;
    if i + 1 < len then
      sum := sum + cardinal(ord(s[p + i + 1])) shl 16;
    if i + 2 < len then
      sum := sum + cardinal(ord(s[p + i + 2])) shl 8;
    if i + 3 < len then
      sum := sum + cardinal(ord(s[p + i + 3]));
    inc(i, 4);
  end;
  CheckEqual(sum, $B1B0AFBA, 'head.checkSumAdjustment');
end;
{$else}
begin
end;
{$endif NO_USE_UNISCRIBE}

function Hex4(v: word): RawUtf8;
const
  HEX: array[0..15] of AnsiChar = '0123456789ABCDEF';
begin
  SetLength(result, 4);
  result[1] := HEX[v shr 12];
  result[2] := HEX[(v shr 8) and 15];
  result[3] := HEX[(v shr 4) and 15];
  result[4] := HEX[v and 15];
end;

{$ifdef FPC} // not in the Windows unit of FPC
function GetGlyphIndicesW(DC: HDC; Text: PWideChar; Count: integer;
  Glyphs: PWord; Flags: cardinal): cardinal; stdcall; external 'gdi32.dll';
{$endif FPC}

procedure TTestUiPdf.BidiEmbeddingOrder;
{$ifndef NO_USE_UNISCRIBE}
const
  // x RLO ab LRE cd PDF ef PDF z: the embedding levels 0 1 2 1 0, where
  // Uniscribe also sets the override bit of the state above the level bits
  TXT: array[0..14] of WideChar = ('x', ' ', WideChar($202E), 'a', 'b',
    WideChar($202A), 'c', 'd', WideChar($202C), 'e', 'f', WideChar($202C),
    ' ', 'z', #0);
  ECA: array[0..2] of WideChar = ('e', 'c', 'a');
var
  pdf: TPdfDocument;
  s: RawByteString;
  dc: HDC;
  font, old: HFONT;
  g: array[0..2] of word;
  p, pe, pc, pa: integer;
begin
  dc := CreateCompatibleDC(0);
  font := CreateFont(-100, 0, 0, 0, FW_NORMAL, 0, 0, 0, ANSI_CHARSET,
    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, 0, 'Arial');
  old := SelectObject(dc, font);
  GetGlyphIndicesW(dc, @ECA, 3, @g, 0);
  SelectObject(dc, old);
  DeleteObject(font);
  DeleteDC(dc);
  pdf := TPdfDocument.Create;
  try
    pdf.GeneratePDF15File := false;
    pdf.CompressionMethod := cmNone;
    pdf.UseUniscribe := true;
    pdf.AddPage;
    pdf.Canvas.SetFont('Arial', 10, []);
    pdf.Canvas.TextOutW(20, 700, @TXT);
    s := SavedPdf(pdf);
  finally
    pdf.Free;
  end;
  // visual order: x, then the runs ef, cd, ab - in the text object only
  p := PosEx('BT'#10, s);
  if not Check(p > 0, 'text object') then
    exit;
  s := copy(s, p, PosEx('ET'#10, s, p) - p);
  pe := PosEx(Hex4(g[0]), s);
  pc := PosEx(Hex4(g[1]), s);
  pa := PosEx(Hex4(g[2]), s);
  CheckUtf8((pe > 0) and
            (pe < pc) and
            (pc < pa), 'visual order ef, cd, ab: % % %', [pe, pc, pa]);
end;
{$else}
begin
end;
{$endif NO_USE_UNISCRIBE}

const
  EMF_SIZE = 100;
  EMF_WHITE = $ffffff;
  EMF_RED = $ff0000; // in a 32-bit DIB: BGRX

// the pixels of Emf converted by our own EMF+ conversion, on white
function EmfPlusPixels(Emf: HENHMETAFILE): TCardinalDynArray;
var
  dc: HDC;
  bmp, old: HBITMAP;
  bi: TBitmapInfo;
  bits: PCardinalArray;
  r: TRect;
  i: PtrInt;
begin
  result := nil;
  FillCharFast(bi, SizeOf(bi), 0);
  bi.bmiHeader.biSize := SizeOf(bi.bmiHeader);
  bi.bmiHeader.biWidth := EMF_SIZE;
  bi.bmiHeader.biHeight := -EMF_SIZE; // top-down
  bi.bmiHeader.biPlanes := 1;
  bi.bmiHeader.biBitCount := 32;
  dc := CreateCompatibleDC(0);
  bmp := CreateDIBSection(dc, bi, DIB_RGB_COLORS, pointer(bits), 0, 0);
  old := SelectObject(dc, bmp);
  try
    FillCharFast(bits^, EMF_SIZE * EMF_SIZE * 4, $ff);
    r.Left := 0; // not Rect(): a type of the windows unit on FPC
    r.Top := 0;
    r.Right := EMF_SIZE;
    r.Bottom := EMF_SIZE;
    DrawAntiAliased(Emf, EMF_SIZE, EMF_SIZE, dc, r, [ecoInternalConvert], smNone);
    GdiFlush;
    SetLength(result, EMF_SIZE * EMF_SIZE);
    for i := 0 to high(result) do
      result[i] := bits[i] and $ffffff;
  finally
    SelectObject(dc, old);
    DeleteObject(bmp);
    DeleteDC(dc);
    DeleteEnhMetaFile(Emf);
  end;
end;

// a metafile DC in advanced graphics mode, its brush red and its pen null
// - two black corners fix the bounds of the picture: the conversion draws
// the metafile scaled to them
function NewEmf: HDC;
begin
  result := CreateEnhMetaFileW(0, nil, nil, nil);
  SetGraphicsMode(result, GM_ADVANCED);
  SelectObject(result, GetStockObject(NULL_PEN));
  SelectObject(result, GetStockObject(BLACK_BRUSH));
  Rectangle(result, 0, 0, 2, 2);
  Rectangle(result, EMF_SIZE - 2, EMF_SIZE - 2, EMF_SIZE, EMF_SIZE);
  SelectObject(result, CreateSolidBrush(RGB(255, 0, 0)));
end;

// the bytes of a metafile; frees it
function EmfBits(Emf: HENHMETAFILE): RawByteString;
var
  n: cardinal;
begin
  n := GetEnhMetaFileBits(Emf, 0, nil);
  SetLength(result, n);
  GetEnhMetaFileBits(Emf, n, pointer(result));
  DeleteEnhMetaFile(Emf);
end;

// close a metafile DC of NewEmf, freeing the brush and pen it selected
function CloseEmf(DC: HDC): HENHMETAFILE;
var
  brush, pen: HGDIOBJ;
begin
  brush := SelectObject(DC, GetStockObject(WHITE_BRUSH));
  pen := SelectObject(DC, GetStockObject(NULL_PEN));
  result := CloseEnhMetaFile(DC);
  DeleteObject(brush); // a stock object is ignored
  DeleteObject(pen);
end;

procedure ScaleEmf(DC: HDC; Factor: single);
var
  xf: TXForm;
begin
  FillCharFast(xf, SizeOf(xf), 0);
  xf.eM11 := Factor;
  xf.eM22 := Factor;
  ModifyWorldTransform(DC, xf, MWT_LEFTMULTIPLY);
end;

function HasRed(const Pixels: TCardinalDynArray): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to high(Pixels) do
    if Pixels[i] = EMF_RED then
      exit;
  result := false;
end;

// true if both metafiles convert to the same pixels; frees them
function SameEmfPlus(Emf, Expected: HENHMETAFILE): boolean;
var
  a, b: TCardinalDynArray;
begin
  a := EmfPlusPixels(Emf);
  b := EmfPlusPixels(Expected);
  result := (a <> nil) and
            (length(a) = length(b)) and
            CompareMem(pointer(a), pointer(b), length(a) * 4) and
            HasRed(b); // not two blank pictures of a broken conversion
end;

// the box of the red pixels of the converted metafile; frees it
function RedBounds(Emf: HENHMETAFILE): TRect;
var
  px: TCardinalDynArray;
  x, y: integer;
begin
  px := EmfPlusPixels(Emf);
  result.Left := EMF_SIZE;
  result.Top := EMF_SIZE;
  result.Right := -1;
  result.Bottom := -1;
  for y := 0 to EMF_SIZE - 1 do
    for x := 0 to EMF_SIZE - 1 do
      if px[y * EMF_SIZE + x] = EMF_RED then
      begin
        if x < result.Left then
          result.Left := x;
        if x > result.Right then
          result.Right := x;
        if y < result.Top then
          result.Top := y;
        if y > result.Bottom then
          result.Bottom := y;
      end;
end;

// true if the red pixels of both metafiles have the same box, to one pixel
function SameRedBounds(Emf, Expected: HENHMETAFILE): boolean;
var
  a, b: TRect;
begin
  a := RedBounds(Emf);
  b := RedBounds(Expected);
  result := (a.Right >= 0) and
            (abs(a.Left - b.Left) <= 1) and
            (abs(a.Top - b.Top) <= 1) and
            (abs(a.Right - b.Right) <= 1) and
            (abs(a.Bottom - b.Bottom) <= 1);
end;

// how many pixels of the converted metafile are red; frees it
function RedPixels(Emf: HENHMETAFILE): integer;
var
  px: TCardinalDynArray;
  i: PtrInt;
begin
  px := EmfPlusPixels(Emf);
  result := 0;
  for i := 0 to high(px) do
    if px[i] = EMF_RED then
      inc(result);
end;

// the metafile of one red rectangle, nothing else
function RectangleEmf(Left, Top, Right, Bottom: integer): HENHMETAFILE;
var
  dc: HDC;
begin
  dc := NewEmf;
  Rectangle(dc, Left, Top, Right, Bottom);
  result := CloseEmf(dc);
end;

procedure TTestUiPdf.EmfPlusClipRestored;
var
  dc: HDC;
begin
  // the clip saved by SaveDC was set again under the transform RestoreDC
  // was leaving, so a scale of 1/16 left it 1/16 of its size
  if not Check(Gdip.Exists, 'GDI+') then
    exit;
  dc := NewEmf;
  IntersectClipRect(dc, 0, 0, EMF_SIZE, EMF_SIZE);
  SaveDC(dc);
  ScaleEmf(dc, 1 / 16);
  RestoreDC(dc, -1);
  Rectangle(dc, 0, 0, EMF_SIZE, EMF_SIZE);
  Check(SameEmfPlus(CloseEmf(dc), RectangleEmf(0, 0, EMF_SIZE, EMF_SIZE)),
    'drawn within the restored clip');
end;

procedure TTestUiPdf.EmfPlusRestoreDepth;
var
  dc: HDC;
begin
  // EMR_RESTOREDC restored one state whatever its iRelative
  if not Check(Gdip.Exists, 'GDI+') then
    exit;
  dc := NewEmf;
  SaveDC(dc);
  ScaleEmf(dc, 2);
  SaveDC(dc);
  ScaleEmf(dc, 2);
  RestoreDC(dc, -2); // back to the identity
  Rectangle(dc, 0, 0, 40, 40);
  Check(SameEmfPlus(CloseEmf(dc), RectangleEmf(0, 0, 40, 40)),
    'not scaled');
end;

procedure TTestUiPdf.EmfPlusNullPen;
const
  EMR_CREATEPEN_ = 38; // the record types, by number
  EMR_EXTCREATEPEN_ = 95;
var
  dc: HDC;
  lb: TLogBrush;
  bits: RawByteString;
  emf: HENHMETAFILE;
  o, n, patched: PtrInt;
begin
  // a PS_NULL pen was created as a solid one, of the colour and width given;
  // Windows records CreatePen(PS_NULL) as the stock NULL_PEN, so the style of
  // two solid pens is set to PS_NULL in the records
  if not Check(Gdip.Exists, 'GDI+') then
    exit;
  dc := NewEmf;
  SelectObject(dc, CreatePen(PS_SOLID, 10, RGB(255, 0, 0)));
  MoveToEx(dc, 0, 30, nil);
  LineTo(dc, EMF_SIZE, 30);
  lb.lbStyle := BS_SOLID;
  lb.lbColor := RGB(255, 0, 0);
  lb.lbHatch := 0;
  DeleteObject(SelectObject(dc, // the first pen
    ExtCreatePen(PS_GEOMETRIC or PS_SOLID, 10, lb, 0, nil)));
  MoveToEx(dc, 0, 70, nil);
  LineTo(dc, EMF_SIZE, 70);
  bits := EmfBits(CloseEmf(dc));
  o := 0;
  patched := 0;
  while o + 8 <= length(bits) do
  begin
    case PCardinal(@PByteArray(bits)[o])^ of
      EMR_CREATEPEN_: // lopn.lopnStyle at 12
        begin
          PCardinal(@PByteArray(bits)[o + 12])^ := PS_NULL;
          inc(patched);
        end;
      EMR_EXTCREATEPEN_: // elp.elpPenStyle at 28
        begin
          PCardinal(@PByteArray(bits)[o + 28])^ := PS_GEOMETRIC or PS_NULL;
          inc(patched);
        end;
    end;
    n := PCardinal(@PByteArray(bits)[o + 4])^;
    if n < 8 then
      break;
    inc(o, n);
  end;
  CheckEqual(patched, 2, 'both pens recorded');
  emf := SetEnhMetaFileBits(length(bits), pointer(bits));
  if Check(emf <> 0, 'the patched metafile') then
    CheckEqual(RedPixels(emf), 0, 'nothing drawn');
end;

// a metafile DC drawing twice the size of its logical units
function DoubledEmf: HDC;
begin
  result := NewEmf;
  SetMapMode(result, MM_ANISOTROPIC);
  SetWindowExtEx(result, 50, 50, nil);
  SetViewportExtEx(result, 100, 100, nil);
end;

procedure TTestUiPdf.EmfPlusViewportOrigin;
var
  dc, expected: HDC;
begin
  // the viewport origin was taken in logical units: ViewOrg * Win / View
  // instead of ViewOrg - WinOrg * View / Win; compared with the same
  // rectangle moved by the logical units of the origin, rounded alike
  if not Check(Gdip.Exists, 'GDI+') then
    exit;
  dc := DoubledEmf;
  SetViewportOrgEx(dc, 30, 30, nil);
  Rectangle(dc, 0, 0, 20, 20); // the device pixels 30..70
  expected := DoubledEmf;
  Rectangle(expected, 15, 15, 35, 35);
  // GDI rounds the recorded right and bottom edges in logical units, so
  // the two may differ by one device pixel there: without the fix they are
  // 15 apart
  Check(SameRedBounds(CloseEmf(dc), CloseEmf(expected)),
    'at the viewport origin');
end;

procedure TTestUiPdf.EmfPlusPlaybackOrder;
var
  dc, expected: HDC;
begin
  // the transform EnumEnhMetaFile sets on the target - the bounds of the
  // drawing mapped to the picture - was applied before the window/viewport
  // mapping, not after it; without corners, the bounds are those of the
  // rectangle, and the same rectangle drawn in device units must match
  if not Check(Gdip.Exists, 'GDI+') then
    exit;
  dc := CreateEnhMetaFileW(0, nil, nil, nil);
  SelectObject(dc, GetStockObject(NULL_PEN));
  SelectObject(dc, CreateSolidBrush(RGB(255, 0, 0)));
  SetMapMode(dc, MM_ANISOTROPIC);
  SetWindowExtEx(dc, 50, 50, nil);
  SetViewportExtEx(dc, 100, 100, nil);
  SetViewportOrgEx(dc, 30, 30, nil);
  Rectangle(dc, 0, 0, 20, 20); // the device pixels 30..70
  expected := CreateEnhMetaFileW(0, nil, nil, nil);
  SelectObject(expected, GetStockObject(NULL_PEN));
  SelectObject(expected, CreateSolidBrush(RGB(255, 0, 0)));
  Rectangle(expected, 30, 30, 70, 70);
  Check(SameRedBounds(CloseEmf(dc), CloseEmf(expected)), 'mapped, then played');
end;

procedure TTestUiPdf.EmfPlusWindowOrigin;
var
  dc, expected: HDC;
begin
  // the window origin was rounded to device units before the playback
  // transform scaled it: with a third of the size and no corners, the
  // drawing is scaled 10 times to the picture, 2 / 3 rounded to 1 is 3 pixels
  // off
  if not Check(Gdip.Exists, 'GDI+') then
    exit;
  dc := CreateEnhMetaFileW(0, nil, nil, nil);
  SelectObject(dc, GetStockObject(NULL_PEN));
  SelectObject(dc, CreateSolidBrush(RGB(255, 0, 0)));
  SetMapMode(dc, MM_ANISOTROPIC);
  SetWindowExtEx(dc, 300, 300, nil);
  SetViewportExtEx(dc, 100, 100, nil);
  SetWindowOrgEx(dc, 2, 2, nil);
  Rectangle(dc, 2, 2, 32, 32); // the device pixels 0..10
  expected := CreateEnhMetaFileW(0, nil, nil, nil);
  SelectObject(expected, GetStockObject(NULL_PEN));
  SelectObject(expected, CreateSolidBrush(RGB(255, 0, 0)));
  Rectangle(expected, 0, 0, 10, 10);
  Check(SameRedBounds(CloseEmf(dc), CloseEmf(expected)), 'not rounded');
end;

procedure TTestUiPdf.EmfPlusRecordLayout;
var
  dc: HDC;
  lb: TLogBrush;
begin
  // FPC 3.2.2 declares EMR_CREATEBRUSHINDIRECT and EMR_EXTCREATEPEN with the
  // native LOGBRUSH and EXTLOGPEN, whose ULONG_PTR hatch moves the fields on
  // Win64: every brush and extended pen was read black
  if not Check(Gdip.Exists, 'GDI+') then
    exit;
  Check(RedPixels(RectangleEmf(10, 10, 50, 50)) > 0, 'the brush colour');
  dc := NewEmf;
  lb.lbStyle := BS_SOLID;
  lb.lbColor := RGB(255, 0, 0);
  lb.lbHatch := 0;
  DeleteObject(SelectObject(dc, // the null pen of NewEmf is a stock object
    ExtCreatePen(PS_GEOMETRIC or PS_SOLID, 10, lb, 0, nil)));
  MoveToEx(dc, 10, 50, nil);
  LineTo(dc, 90, 50);
  Check(RedPixels(CloseEmf(dc)) > 0, 'the extended pen colour');
end;

const
  // the record types, by number: the windows unit of FPC lacks some names
  R_POLYPOLYLINE = 7;
  R_SAVEDC = 33;
  R_RESTOREDC = 34;
  R_SELECTOBJECT = 37;
  R_CREATEBRUSHINDIRECT = 39;
  R_STRETCHDIBITS = 81;
  R_EXTTEXTOUTA = 83;
  R_EXTTEXTOUTW = 84;
  R_POLYPOLYLINE16 = 90;
  R_POLYPOLYGON16 = 91;
  R_EXTCREATEPEN = 95;
  R_ALPHABLEND = 114;

// AlphaBlend under its gdi32 name: the windows unit of FPC lacks it
function GdiAlphaBlend(Dest: HDC; X, Y, W, H: integer; Src: HDC;
  XS, YS, WS, HS: integer; Blend: DWord): BOOL; stdcall; external 'gdi32.dll';

// the bytes of a metafile, as GetEnhMetaFileBits gives them
function MetaFileBytes(Mf: TMetaFile): RawByteString;
var
  n: cardinal;
begin
  n := GetEnhMetaFileBits(Mf.Handle, 0, nil);
  SetLength(result, n);
  if n > 0 then
    GetEnhMetaFileBits(Mf.Handle, n, pointer(result));
end;

// the offsets of the records of type RecType in the bytes of a metafile
function EmfRecords(const Bytes: RawByteString; RecType: cardinal): TIntegerDynArray;
var
  o, n: PtrInt;
begin
  result := nil;
  o := 0;
  while o + 8 <= length(Bytes) do
  begin
    if PCardinal(@PByteArray(Bytes)[o])^ = RecType then
    begin
      n := length(result);
      SetLength(result, n + 1);
      result[n] := o;
    end;
    n := PCardinal(@PByteArray(Bytes)[o + 4])^;
    if n < 8 then
      break;
    inc(o, n);
  end;
end;

// the metafile rendered into one page of an uncompressed PDF
function RenderedEmf(Mf: TMetaFile; PxX: integer = 0; PxY: integer = 0): RawUtf8;
var
  doc: TPdfDocumentGdi;
begin
  doc := TPdfDocumentGdi.Create;
  try
    doc.Info.CreationDate := FIXED_DATE;
    doc.CompressionMethod := cmNone;
    doc.EmbeddedTTF := false;
    doc.StandardFontsReplace := true;
    TPdfDocumentAccess(TPdfDocument(doc)).fPrinterPxPerInch.X := PxX;
    TPdfDocumentAccess(TPdfDocument(doc)).fPrinterPxPerInch.Y := PxY;
    doc.AddPage;
    RenderMetaFile(doc.Canvas, Mf, 1, 1, 0, 0);
    result := SavedPdf(doc);
  finally
    doc.Free;
  end;
end;

// the operands of the last Op (' rg', ' Tm') before Marker in the content
function OperandsBefore(const Txt, Marker, Op: RawUtf8): RawUtf8;
var
  p, i, j, k: PtrInt;
begin
  result := '';
  p := PosEx(Marker, Txt);
  i := 1;
  repeat
    j := PosEx(Op, Txt, i);
    if (j = 0) or
       (j > p) then
      break;
    i := j + length(Op);
    k := j;
    while (k > 1) and
          not (Txt[k - 1] in [#10, #13]) do
      dec(k);
    result := copy(Txt, k, j - k);
  until false;
end;

function BinaryOf(const B: array of byte): RawByteString;
begin
  FastSetRawByteString(result, @B[0], length(B));
end;

procedure TTestUiPdf.EmfAlphaBlendOpaque;
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  mem: HDC;
  bmp, old: HBITMAP;
  bi: TBitmapInfo;
  bits: PCardinalArray;
  txt: RawUtf8;
  i: integer;
begin
  { the BLENDFUNCTION of EMR_ALPHABLEND was taken as the transparent colour:
    the usual AC_SRC_OVER with SourceConstantAlpha 255 is $00FF0000, blue }
  mf := TMetaFile.Create;
  try
    mf.Width := 100;
    mf.Height := 100;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      // a 32-bit DIB of blue pixels, whatever the depth of the screen
      FillChar(bi, SizeOf(bi), 0);
      bi.bmiHeader.biSize := SizeOf(bi.bmiHeader);
      bi.bmiHeader.biWidth := 8;
      bi.bmiHeader.biHeight := 8;
      bi.bmiHeader.biPlanes := 1;
      bi.bmiHeader.biBitCount := 32;
      mem := CreateCompatibleDC(0);
      bmp := CreateDIBSection(mem, bi, DIB_RGB_COLORS, pointer(bits), 0, 0);
      for i := 0 to 63 do
        bits[i] := $000000ff; // BGRX: blue
      old := SelectObject(mem, bmp);
      GdiAlphaBlend(mc.Handle, 10, 10, 64, 64, mem, 0, 0, 8, 8, $00FF0000);
      SelectObject(mem, old);
      DeleteObject(bmp);
      DeleteDC(mem);
    finally
      mc.Free;
    end;
    CheckEqual(length(EmfRecords(MetaFileBytes(mf), R_ALPHABLEND)), 1,
      'recorded as EMR_ALPHABLEND');
    txt := RenderedEmf(mf);
  finally
    mf.Free;
  end;
  CheckEqual(CountOccurrences('/Subtype/Image', txt), 1, 'the bitmap');
  CheckEqual(CountOccurrences('/Mask', txt), 0, 'no colour key');
end;

procedure TTestUiPdf.EmfPolyPolylineOpen;
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  pts: array[0..5] of TPoint;
  lines: array[0..1] of DWord;
  polys: array[0..1] of integer;
  bytes: RawByteString;
  txt: RawUtf8;
  i: integer;
begin
  { every polyline of EMR_POLYPOLYLINE/16 got a segment back to its first
    point; a polygon keeps it }
  mf := TMetaFile.Create;
  try
    mf.Width := 400;
    mf.Height := 400;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      lines[0] := 3;
      lines[1] := 3;
      polys[0] := 3;
      polys[1] := 3;
      for i := 0 to 5 do
      begin
        pts[i].X := 20 + i * 30;
        pts[i].Y := 20 + (i and 1) * 40;
      end;
      PolyPolyline(mc.Handle, pts[0], lines[0], 2); // 16-bit coordinates
      for i := 0 to 5 do
        inc(pts[i].Y, 100);
      PolyPolygon(mc.Handle, pts[0], polys[0], 2);
      for i := 0 to 5 do
        pts[i].X := 40000 + i * 30; // past 16 bits: the 32-bit record
      PolyPolyline(mc.Handle, pts[0], lines[0], 2);
    finally
      mc.Free;
    end;
    bytes := MetaFileBytes(mf);
    CheckEqual(length(EmfRecords(bytes, R_POLYPOLYLINE16)), 1, 'EMR_POLYPOLYLINE16');
    CheckEqual(length(EmfRecords(bytes, R_POLYPOLYLINE)), 1, 'EMR_POLYPOLYLINE');
    CheckEqual(length(EmfRecords(bytes, R_POLYPOLYGON16)), 1, 'EMR_POLYPOLYGON16');
    txt := RenderedEmf(mf);
  finally
    mf.Free;
  end;
  // two open polylines of two segments in each record, two closed triangles
  CheckEqual(CountOccurrences(' l'#10, txt), 2 * 2 + 2 * 2 + 2 * 3, 'line segments');
end;

procedure TTestUiPdf.EmfRestoreDCDepth;
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  bytes: RawByteString;
  recs: TIntegerDynArray;
  txt, rel: RawUtf8;
  i: integer;

  procedure Text(Y: integer; const Value: WideString);
  begin
    TextOutW(mc.Handle, 10, Y, pointer(Value), length(Value));
  end;

begin
  { EMR_RESTOREDC popped one state whatever its iRelative, and the saved
    states were an array of 32 guarded by Assert only }
  mf := TMetaFile.Create;
  try
    mf.Width := 400;
    mf.Height := 400;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      SetTextColor(mc.Handle, RGB(255, 0, 0));
      SaveDC(mc.Handle);
      SetTextColor(mc.Handle, RGB(0, 255, 0));
      SaveDC(mc.Handle);
      SetTextColor(mc.Handle, RGB(0, 0, 255));
      RestoreDC(mc.Handle, -2); // back to red
      Text(10, 'Red');
      for i := 1 to 40 do
      begin
        SetTextColor(mc.Handle, RGB(i * 6, 0, 0));
        if i = 37 then
          Text(40, 'Level');
        SaveDC(mc.Handle);
      end;
      RestoreDC(mc.Handle, -4); // the state saved after Level
      Text(70, 'Deep');
    finally
      mc.Free;
    end;
    bytes := MetaFileBytes(mf);
    CheckEqual(length(EmfRecords(bytes, R_SAVEDC)), 42, 'the saves');
    recs := EmfRecords(bytes, R_RESTOREDC); // the canvas adds one of its own
    rel := '';
    for i := 0 to high(recs) do
      rel := rel + Int32ToUtf8(PInteger(@PByteArray(bytes)[recs[i] + 8])^) + ' ';
    Check(PosEx('-2 -4 ', rel) = 1, 'the restores');
    txt := RenderedEmf(mf);
  finally
    mf.Free;
  end;
  CheckEqual(OperandsBefore(txt, '(Red)', ' rg'), '1 0 0', 'RestoreDC(-2)');
  Check(OperandsBefore(txt, '(Level)', ' rg') <> '', 'the colour of level 37');
  CheckEqual(OperandsBefore(txt, '(Deep)', ' rg'),
    OperandsBefore(txt, '(Level)', ' rg'), 'restored past 32 states');
end;

procedure TTestUiPdf.EmfScaledRotation;
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  xf: TXForm;
  txt, m: RawUtf8;
  raised: boolean;
begin
  { ScaleMatrix took ArcSin(eM12) and TextOut ArcCos(eM11): with a rotation
    scaled by 2 both get a value past 1 and raise (Delphi) or give NaN (FPC) }
  mf := TMetaFile.Create;
  try
    mf.Width := 400;
    mf.Height := 400;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      mc.Font.Name := 'Arial';
      mc.Font.Size := 10;
      SetGraphicsMode(mc.Handle, GM_ADVANCED);
      FillChar(xf, SizeOf(xf), 0);
      xf.eM12 := 2; // 90 degrees, twice the size
      xf.eM21 := -2;
      xf.eDx := 200;
      SetWorldTransform(mc.Handle, xf);
      mc.TextOut(10, 10, 'Ninety');
      xf.eM11 := -2; // 180 degrees, twice the size
      xf.eM12 := 0;
      xf.eM21 := 0;
      xf.eM22 := -2;
      xf.eDx := 300;
      xf.eDy := 300;
      SetWorldTransform(mc.Handle, xf);
      mc.TextOut(10, 10, 'Turned');
    finally
      mc.Free;
    end;
    raised := false;
    try
      txt := RenderedEmf(mf);
    except
      raised := true;
    end;
  finally
    mf.Free;
  end;
  Check(not raised, 'rendered');
  Check(PosEx('(Ninety)', txt) > 0, 'the text at 90 degrees');
  Check(PosEx('(Turned)', txt) > 0, 'the text at 180 degrees');
  // FPC's ArcCos(-2) is NaN: the text took the negated matrix, drawn upright
  m := OperandsBefore(txt, '(Turned)', ' Tm');
  Check((m <> '') and (m[1] = '-'), 'turned by 180 degrees: ' + Utf8ToString(m));
end;

procedure TTestUiPdf.EmfAnsiText;
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  bytes: RawByteString;
  recs: TIntegerDynArray;
  w, a: RawUtf8;
  i: integer;

  // draw Codes at Y in an Arial of CharSet
  procedure Text(Y, CharSet: integer; const Codes: array of word);
  var
    lf: TLogFontW;
    f, old: HFONT;
    ws: WideString;
    c: integer;
  begin
    FillChar(lf, SizeOf(lf), 0);
    lf.lfHeight := -16;
    lf.lfCharSet := CharSet;
    Utf8ToWideChar(@lf.lfFaceName, 'Arial');
    f := CreateFontIndirectW(lf);
    old := SelectObject(mc.Handle, f);
    SetLength(ws, length(Codes));
    for c := 0 to high(Codes) do
      ws[c + 1] := WideChar(Codes[c]);
    TextOutW(mc.Handle, 10, Y, pointer(ws), length(ws));
    SelectObject(mc.Handle, old);
    DeleteObject(f);
  end;

  // the W record at o as an A record of Ansi, its UTF-16 zeroed past the bytes
  procedure ToAnsi(o: integer; const Ansi: RawByteString);
  var
    str: PtrInt;
  begin
    PCardinal(@PByteArray(bytes)[o])^ := R_EXTTEXTOUTA;
    // EMRTEXT.nChars at 44, offString at 48 of the record: room for 4 bytes
    PCardinal(@PByteArray(bytes)[o + 44])^ := length(Ansi);
    str := o + PInteger(@PByteArray(bytes)[o + 48])^;
    FillChar(PByteArray(bytes)[str], 4, 0);
    MoveFast(pointer(Ansi)^, PByteArray(bytes)[str], length(Ansi));
  end;

  // a dx of 100 per byte after the 4 bytes of the A record at o, which held
  // the UTF-16 and the dx of 3 WideChars: wider than the text, so it is used
  procedure WideDx(o: integer);
  var
    j, str: PtrInt;
  begin
    str := PInteger(@PByteArray(bytes)[o + 48])^;
    PInteger(@PByteArray(bytes)[o + 72])^ := str + 4; // EMRTEXT.offDx
    for j := 0 to 3 do
      PInteger(@PByteArray(bytes)[o + str + 4 + j * 4])^ := 100;
  end;

begin
  { EMR_EXTTEXTOUTA went through the Unicode path: its bytes were read as
    WideChars, twice as many as the record holds. Windows records ExtTextOutA
    as EMR_EXTTEXTOUTW, so the A records are made from W ones, in the ANSI
    code page, in the one of a Greek font and in Shift-JIS, three characters
    of four bytes. The W records lose their dx, which an A record has per
    byte: the Shift-JIS one gets a dx of 4 entries, which cannot fit its 3
    WideChars and is dropped - all draw by the measured text }
  mf := TMetaFile.Create;
  try
    mf.Width := 400;
    mf.Height := 400;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      Text(10, ANSI_CHARSET, [ord('A'), ord('b')]);
      Text(40, GREEK_CHARSET, [$03B1, $03B2]); // alpha beta
      Text(70, SHIFTJIS_CHARSET, [ord('A'), $3042, ord('B')]); // hiragana a
    finally
      mc.Free;
    end;
    bytes := MetaFileBytes(mf);
    recs := EmfRecords(bytes, R_EXTTEXTOUTW);
    if CheckEqual(length(recs), 3, 'three W records') then
    begin
      for i := 0 to 2 do
      begin
        CheckEqual(PCardinal(@PByteArray(bytes)[recs[i] + 44])^, 2 + i shr 1,
          'the characters');
        PCardinal(@PByteArray(bytes)[recs[i] + 72])^ := 0; // EMRTEXT.offDx
      end;
      mf.Handle := SetEnhMetaFileBits(length(bytes), pointer(bytes));
      w := RenderedEmf(mf);
      ToAnsi(recs[0], 'Ab');
      ToAnsi(recs[1], BinaryOf([$E1, $E2])); // code page 1253
      ToAnsi(recs[2], BinaryOf([ord('A'), $82, $A0, ord('B')])); // code page 932
      WideDx(recs[2]);
      mf.Handle := SetEnhMetaFileBits(length(bytes), pointer(bytes));
      a := RenderedEmf(mf);
    end;
  finally
    mf.Free;
  end;
  Check(w <> '', 'the W metafile rendered');
  CheckEqual(a, w, 'the A records draw as the W ones');
end;

procedure TTestUiPdf.EmfAnsiStockFont;
const
  ANSI: array[0..1] of byte = ($E1, $E2);
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  lf: TLogFontW;
  f, old: HFONT;
  bytes: RawByteString;
  recs: TIntegerDynArray;
  w, a: RawUtf8;
  str: PtrInt;
  i, stock: integer;

  // the bytes as CodePage reads them, drawn as a W record after the stock
  // font - one character on a DBCS code page (932, 936, 950)
  procedure Text(Y: integer; StockFont, CodePage: cardinal);
  var
    ws: array[0..1] of WideChar;
    n: integer;
  begin
    SelectObject(mc.Handle, GetStockObject(StockFont));
    n := MultiByteToWideChar(CodePage, 0, @ANSI, 2, @ws, 2);
    TextOutW(mc.Handle, 10, Y, @ws, n);
  end;

begin
  { a stock font selected after a font of the metafile kept the code page of
    that font: an EMR_EXTTEXTOUTA drawn then decoded its bytes in it, not in
    the code page of the stock font - the system one, the OEM one for
    OEM_FIXED_FONT }
  mf := TMetaFile.Create;
  try
    mf.Width := 400;
    mf.Height := 400;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      FillChar(lf, SizeOf(lf), 0);
      lf.lfHeight := -16;
      lf.lfCharSet := GREEK_CHARSET;
      Utf8ToWideChar(@lf.lfFaceName, 'Arial');
      f := CreateFontIndirectW(lf);
      old := SelectObject(mc.Handle, f);
      Text(10, SYSTEM_FONT, CP_ACP);
      SelectObject(mc.Handle, f);
      Text(40, OEM_FIXED_FONT, CP_OEMCP);
      SelectObject(mc.Handle, old);
      DeleteObject(f);
    finally
      mc.Free;
    end;
    bytes := MetaFileBytes(mf);
    recs := EmfRecords(bytes, R_SELECTOBJECT);
    stock := 0; // the VCL canvas may select a stock font of its own
    for i := 0 to high(recs) do
      case PCardinal(@PByteArray(bytes)[recs[i] + 8])^ of
        $80000000 + SYSTEM_FONT:
          stock := stock or 1;
        $80000000 + OEM_FIXED_FONT:
          stock := stock or 2;
      end;
    CheckEqual(stock, 3, 'both stock fonts selected');
    recs := EmfRecords(bytes, R_EXTTEXTOUTW);
    if CheckEqual(length(recs), 2, 'two W records') then
    begin
      for i := 0 to 1 do
        PCardinal(@PByteArray(bytes)[recs[i] + 72])^ := 0; // no dx: see EmfAnsiText
      mf.Handle := SetEnhMetaFileBits(length(bytes), pointer(bytes));
      w := RenderedEmf(mf);
      // the W records as A records of the two bytes
      for i := 0 to 1 do
      begin
        PCardinal(@PByteArray(bytes)[recs[i]])^ := R_EXTTEXTOUTA;
        PCardinal(@PByteArray(bytes)[recs[i] + 44])^ := 2; // EMRTEXT.nChars
        str := recs[i] + PInteger(@PByteArray(bytes)[recs[i] + 48])^;
        FillChar(PByteArray(bytes)[str], 4, 0);
        MoveFast(ANSI, PByteArray(bytes)[str], 2);
      end;
      mf.Handle := SetEnhMetaFileBits(length(bytes), pointer(bytes));
      a := RenderedEmf(mf);
    end;
  finally
    mf.Free;
  end;
  Check(w <> '', 'the W metafile rendered');
  CheckEqual(a, w, 'decoded in the code page of the stock font');
end;

procedure TTestUiPdf.EmfDIBitsKept;
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  xf: TXForm;
  bmi: TBitmapInfo;
  pix: array[0..15] of cardinal;
  before: RawByteString;
  first, second: RawUtf8;
  i: integer;
begin
  { with eM22 < 0, EMR_STRETCHDIBITS negated biHeight inside the record,
    which EnumEnhMetaFile hands out read-only: a second rendering of the same
    metafile drew the bitmap the other way round }
  mf := TMetaFile.Create;
  try
    mf.Width := 200;
    mf.Height := 200;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      SetGraphicsMode(mc.Handle, GM_ADVANCED);
      FillChar(xf, SizeOf(xf), 0);
      xf.eM11 := 1;
      xf.eM22 := -1;
      xf.eDy := 150;
      SetWorldTransform(mc.Handle, xf);
      FillChar(bmi, SizeOf(bmi), 0);
      bmi.bmiHeader.biSize := SizeOf(bmi.bmiHeader);
      bmi.bmiHeader.biWidth := 4;
      bmi.bmiHeader.biHeight := 4;
      bmi.bmiHeader.biPlanes := 1;
      bmi.bmiHeader.biBitCount := 32;
      for i := 0 to high(pix) do
        pix[i] := cardinal(i) * $00101010; // a top-bottom gradient
      StretchDIBits(mc.Handle, 10, 10, 64, 64, 0, 0, 4, 4, @pix, bmi,
        DIB_RGB_COLORS, SRCCOPY);
    finally
      mc.Free;
    end;
    before := MetaFileBytes(mf);
    CheckEqual(length(EmfRecords(before, R_STRETCHDIBITS)), 1,
      'recorded as EMR_STRETCHDIBITS');
    first := RenderedEmf(mf);
    second := RenderedEmf(mf);
    Check(MetaFileBytes(mf) = before, 'the metafile unchanged');
  finally
    mf.Free;
  end;
  CheckEqual(CountOccurrences('/Subtype/Image', first), 1, 'the bitmap');
  CheckEqual(second, first, 'drawn the same twice');
end;

procedure TTestUiPdf.EmfPrinterResZero;
var
  mf: TMetaFile;
  mc: TMetaFileCanvas;
  auto, zero: RawUtf8;
  raised: boolean;
begin
  { MM_ISOTROPIC divides by the printer resolution: one of 0 - a printer DC
    that failed - was kept, as only its X was checked }
  mf := TMetaFile.Create;
  try
    mf.Width := 400;
    mf.Height := 400;
    mc := TMetaFileCanvas.Create(mf, 0);
    try
      SetMapMode(mc.Handle, MM_ISOTROPIC);
      SetWindowExtEx(mc.Handle, 1000, 1000, nil);
      SetViewportExtEx(mc.Handle, 300, 300, nil);
      Rectangle(mc.Handle, 100, 100, 600, 400);
    finally
      mc.Free;
    end;
    auto := RenderedEmf(mf); // the resolution asked from the printer
    raised := false;
    try
      zero := RenderedEmf(mf, 300, 0);
    except
      raised := true;
    end;
  finally
    mf.Free;
  end;
  Check(not raised, 'rendered with a resolution of 0');
  Check(auto <> '', 'rendered');
  CheckEqual(zero, auto, 'asked again');
end;

procedure TTestUiPdf.ReducedFontSearchFields;
{$ifndef NO_USE_UNISCRIBE}
var
  pdf: TPdfDocument;
  s: RawByteString;
  p: integer;
begin
  { ReduceTTF wrote the binary search fields of ten tables whatever their
    number: Mongolian Baiti has no 'cvt ', its subset keeps nine tables }
  pdf := TPdfDocument.Create;
  try
    if TPdfDocumentAccess(pdf).GetTrueTypeFontIndex('Mongolian Baiti') < 0 then
    begin
      Check(true, 'no Mongolian Baiti');
      exit;
    end;
    pdf.GeneratePDF15File := false;
    pdf.CompressionMethod := cmNone;
    pdf.EmbeddedTtf := true;
    pdf.AddPage;
    pdf.Canvas.SetFont('Mongolian Baiti', 10, []);
    pdf.Canvas.TextOut(20, 700, 'Hello');
    s := SavedPdf(pdf);
  finally
    pdf.Free;
  end;
  p := PosEx('/Length1 ', s);
  if not Check(p > 0, 'FontFile2') then
    exit;
  p := PosEx('stream', s, p);
  if not Check(p > 0, 'stream') then
    exit;
  inc(p, 6);
  if s[p] = #13 then
    inc(p);
  inc(p); // #10
  // numTables 9, searchRange 128, entrySelector 3, rangeShift 16
  CheckEqual(BinToHex(copy(s, p + 4, 8)), '0009008000030010', 'nine tables');
end;
{$else}
begin
end;
{$endif NO_USE_UNISCRIBE}

{$else}

implementation // void unit

{$endif HAS_UI_PDF}

end.
