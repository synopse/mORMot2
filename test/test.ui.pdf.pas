unit test.ui.pdf;

interface

{$I ..\src\mormot.defines.inc}

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  mormot.core.base,
  mormot.core.buffers,
  mormot.core.os,
  mormot.core.test,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.zip,
  mormot.ui.pdf;

type
  /// regression tests for mormot.ui.pdf
  TTestUiPdf = class(TSynTestCase)
  private
    function CountOccurrences(const SubStr, Text: RawUTF8): Integer;
  public

  published
    {$ifdef MSWINDOWS}
    {$ifndef FPC}
    {$ifndef LVCL}
    procedure TestFontEmbeddingAndHashValidation;
    procedure TestMetafileRenderingAndOrientation;
    procedure TestSimplePDFCreation;
    procedure TestTextInsertion;
    procedure TestMultiplePages;
    procedure TestPDFMetadata;
    {$endif}
    {$endif}
    {$endif}
  end;

implementation

uses
{$ifdef ISDELPHIXE2}
  WinApi.Windows,
  VCL.Graphics
{$else}
  Graphics
{$endif}
  ;

{$ifdef MSWINDOWS}
{$ifndef FPC}
{$ifndef LVCL}

function TTestUiPdf.CountOccurrences(const SubStr, Text: RawUTF8): Integer;
var
  PosIdx: Integer;
begin
  Result := 0;
  PosIdx := PosEx(SubStr, Text); // Use PosEx for RawUTF8
  while PosIdx > 0 do
  begin
    Inc(Result);
    PosIdx := PosEx(SubStr, Text, PosIdx + Length(SubStr));
  end;
end;

const
  FIXED_DATE = 40339.803675; // forced date to have the exact same Hash32 value

procedure TTestUiPdf.TestFontEmbeddingAndHashValidation;
var MS: TMemoryStream;
    i,y: integer;
    embed: boolean;
    expected: cardinal;
    WS: SynUnicode;
const
  Hash: array[boolean] of Cardinal =
    (2336277040,1967009088);
  Hash10: array[boolean] of Cardinal =
    (2379006506,1967009088);
  Name: array[boolean] of PDFString =
    ('Arial','Helvetica');
begin
  MS := TMemoryStream.Create;
  with TPdfDocument.Create do
  try
    for embed := false to true do begin
      Info.CreationDate := FIXED_DATE; // no FIXED_DATE nor creator variation in hashed value
      Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
      StandardFontsReplace := embed;
      AddPage;
      Canvas.SetFont('arial',10,[]);
      Check(Canvas.Page.Font.Name=Name[embed]);
      y := 800;
      for i := 1 to 30 do begin
        Canvas.SetFont('Arial',9+i,[]);
        WS := 'Texte accentue n.'+IntToString(i);
        PWordArray(WS)^[13] := 233;
        PWordArray(WS)^[16] := 176;
        Canvas.TextOutW(100,y,pointer(WS));
        dec(y,9+i);
      end;
      SaveToStream(MS,FIXED_DATE);
      //MS.SaveToFile(ChangeFileExt(ExeVersion.ProgramFileName,'.pdf'));
      if OSVersion<wTen then
        expected := Hash[embed] else
        expected := Hash10[embed];
      Check(Hash32(MS.Memory,MS.Position)=expected);
      if not embed then begin
        if CharSet<>ANSI_CHARSET then
          break; // StandardFontsReplace will work only with ANSI code page
        NewDoc;
        MS.Clear;
      end;
    end;
  finally
    Free;
    MS.Free;
  end;
end;

procedure TTestUiPdf.TestSimplePDFCreation;
var
  Pdf: TPdfDocument;
  PdfStream: TMemoryStream;
  Header: array[0..4] of AnsiChar;
  Trailer: array[0..5] of AnsiChar;
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
    PdfStream.Position := 0;


    FillChar(Header, SizeOf(Header), 0);
    PdfStream.Read(Header, SizeOf(Header));
    CheckEqual('%PDF-', RawUTF8(AnsiString(Header)), 'PDF header is missing or invalid');

    PdfStream.Position := PdfStream.Size - 6;
    PdfStream.Read(Trailer, SizeOf(Trailer));
    CheckEqual('%%EOF'#10, RawUTF8(AnsiString(Trailer)), 'PDF trailer is missing or invalid');
  finally
    PdfStream.Free;
  end;
end;

procedure TTestUiPdf.TestTextInsertion;
const
  EXPECTED_TEXT = 'Hello from mORMot PDF';
var
  Pdf: TPdfDocumentGdi;
  PdfStream: TMemoryStream;
  PdfBuffer: TBytes;
  PdfContent: string;
begin
  PdfStream := TMemoryStream.Create;
  try
    Pdf := TPdfDocumentGdi.Create;
    try
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

    SetLength(PdfBuffer, PdfStream.Size);
    PdfStream.Position := 0;
    PdfStream.ReadBuffer(PdfBuffer[0], PdfStream.Size);

    PdfContent := TEncoding.ANSI.GetString(PdfBuffer);
    Check(Pos(EXPECTED_TEXT, PdfContent) > 0, 'Expected text not found in the PDF content');
  finally
    PdfStream.Free;
  end;
end;

procedure TTestUiPdf.TestMetafileRenderingAndOrientation;
const
  EMF: RawByteString = // some compressed EMF file content
  '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'+
  'k/wlSd7FcsQ8mQ+lra1Opq0ansxSbBqezFJsGp7MUmwansxSbGAB2/WG2MCSxp270n3MpNg0PJml2MAC'+
  'NglPZik2DU9mKTawgG2RK93HTIoNLGCT8GSWYtPwZJZi0/BklmIDC9gkPJml2DQ8maXYNDyZpdjAArbb'+
  'DLGBJY07N8Sm4cksxQaWNO7c2TlnaXgyS7GBBWyrDbGBJY07N8Sm4cksxQYWsK0xxAaWNO7cEJuGJ7MU'+
  'G1jA9qCzc+wFSxp3bogNLGCT8GSWYtPwZJZiA0sad+5K9z2WYtPwZJZi0/BklmIDSxp3bohNw5NZig0s'+
  'adw5rWeEDSxp3Lkr3ZNZik3Dk1mKDSxp3Lkr3ZNZig0sYHuOdCNsYAGbhCezFJuGJ7MUG1jAJuHJLMWm'+
  '4cksxabhySzFBhawveJK92SWYgML2La70j2ZpdjAksadu9I9maXYwAI2CU9mKTYNT2YpNrCkceeudE9m'+
  'KTawgO0PrnRPZik2sKRx5650T2YpNg1PZik2sIBNwpNZik3Dk1mKTcOTWYpNw5NZig0sady5K92TWYoN'+
  'LGCj66VSPZml2MACNglPZik2DU9mKTYNT2YpNg1PZik2DU9mKTYNT2YpNg1PZik2DU9mKTYNT2YpNg1P'+
  'Zik2DU9mKTYNT2YpNg1PZik2DU9mKTawgE3Ck1mKTcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZik3Dk1mK'+
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZig0sady5L92TWYpNw5NZik3Dk1mKTcOTWYpNw5NZik3Dk1mK'+
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZLDbWy3syS7GBJY0796V7MkuxaXgyi8XGenlPZik2DU9msdhY'+
  'L+/JLBYb6+U9maXYNDyZxWJjvbwnsxSbhiezFJuGJ7NYbKyX92SWYtPwZJZiA0saG+tL92QWi4318p7M'+
  'UmwansxSbBqezGKxsV7ek1mKTcOTWSw21st7MovFxnp5T2YpNg1PZrHYWC/vySzFpuHJLMWm4cksFhvr'+
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysl/dklmLT8GQWi41NWNLYWENsGp7MUmwansxi'+
  'sbFe3pNZLDbWy3syS7FpeDKLxcZ6eU9mKTYNT2YpNg1PZrHYWMYm4cksxabhySzFpuHJLBYb6+U9maXY'+
  'NDyZpdg0PJnFYmO9vCezFJuGJ7NYbGzCksbGGmLT8GSWYtPwZBaLjU1YwCbhySzFpuHJLMWm4cksFhvr'+
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hs'+
  'rIIns1hsrIInsxSbhiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb'+
  'hiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb'+
  'hiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezWGysgiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MU'+
  'W7GezMWwYjowNF/uYhwkLfNBa2urywyrfFtT6t0MN9vNc3PD+NCG6Y1TrprWiAIr0vlD3I6Fw9woP8Q1'+
  'jz/dDXej3ZClzk14r9LVXTW1kca7FgzLrfNaWG5KyG/40tuNtM7LvavpM665+Q+G+dsXdVoc3x5vK5fK'+
  'WP0ebanRbmgol+oYnDQrW+6mTLl3Hln9FZS7n5W7K+Gi8ZIw3vqLIZ2yZd44wufWeWzS1Ebaxn17Dfzy'+
  'CW6jv67RdXcuf1/o69I+T+fTdim0j3Ry8TccmqZuqswsX2i+d/n7Qnv9f1RSHw29k+n/Bw==';
  METAFILE_HASH: array[boolean] of Cardinal = (3398175318, 1097442985);
var S: RawByteString;
    MS: TMemoryStream;
    MF: TMetaFile;
    Doc: TPdfDocument;
    Page: TPdfPage;
    orientation: boolean;
    H: cardinal;
    i,j: integer;
//    E: RawByteString; i,L,n: integer;
begin
{  S := SockBase64Encode(CompressString(StringFromFile('d:\temp\tmpCurve.emf')));
  E := '  EMF: RawByteString = // some compressed simple EMF file'#13#10;
  L := length(S);
  i := 1;
  while L>0 do begin
    if L>80 then
      n := 80 else
      n := L;
    E := E+'  '''+copy(S,i,n)+'''+'#13#10;
    dec(L,n);
    inc(i,n);
  end;
  FileFromString(E,'test.pas');}
  S := UncompressString(Base64ToBin(EMF));
  Check(Hash32(S)=$5BB4C8B1);
  MS := TMemoryStream.Create;
  try
    with TPdfDocument.Create do
    try
      Info.CreationDate := FIXED_DATE; // force fixed date and creator for Hash32()
      Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
      //CompressionMethod := cmNone; useful for debugg purposes of metafile enum
      AddPage;
      MF := TMetaFile.Create;
      try
        MS.Write(pointer(S)^,length(S));
        MS.Position := 0;
        MF.LoadFromStream(MS);
        RenderMetaFile(Canvas, MF);
        Check(Canvas.Page.Font.Name='Tahoma');
      finally
        MF.Free;
      end;
      MS.Clear;
      SaveToStream(MS,FIXED_DATE);
      // force constant Arial,Bold and Tahoma FontBBox
      SetString(s,PAnsiChar(MS.Memory),MS.Position);
      MS.SaveToFile(ChangeFileExt(ExeVersion.ProgramFileName,'.pdf'));
      if (Unicode_CodePage<>1252) {$ifdef CPU64}or true{$endif} then
        Check(length(s)>6500) else begin
        i := PosEx('/FontBBox[',s);
        if CheckFailed(i<>0) then exit;
        FillCharFast(s[i],32,32);
        j := PosEx('/FontBBox[',s);
        if CheckFailed(j<>0) then exit;
        FillCharFast(s[j],32,32);
        i := PosEx('/FontBBox[',s);
        if CheckFailed(i<>0)then exit;
        FillCharFast(s[i],32,32);
        H := Hash32(s);
        Check(H=$FE2D27CA);
      end;
    finally
      Free;
    end;
    MF := TMetafile.Create;
    try
      // create test metafile
      MF.Width := 700;
      MF.Height := 700;
      with TMetafileCanvas.Create(MF, GetDC(0)) do
      try
        MoveTo(0, 0);
        LineTo(700, 700);
        MoveTo(0, 700);
        LineTo(700, 0);
      finally
        Free;
      end;
      // create page in portrait/landscape orientation, and render metafile to it
      for orientation := false to true do begin
        Doc := TPdfDocument.Create;
        try
          Doc.GeneratePDF15File := True;
          Doc.Info.CreationDate := FIXED_DATE; // force fixed date for Hash32()
          Doc.Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
          Doc.DefaultPaperSize := psA4;
          Page := Doc.AddPage;
          Page.PageLandscape := orientation;
          MS.Clear;
          RenderMetaFile(Doc.Canvas, MF);
          Doc.SaveToStream(MS,FIXED_DATE);
          H := Hash32(MS.Memory,MS.Position);
          Check(H=METAFILE_HASH[orientation]);
        finally
          Doc.Free;
        end;
      end;
    finally
      MF.Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure TTestUiPdf.TestMultiplePages;
var
  Pdf: TPdfDocumentGdi;
  PdfStream: TMemoryStream;
  PdfBuffer: TBytes;
  PageCount: Integer;
begin
  PdfStream := TMemoryStream.Create;
  try
    Pdf := TPdfDocumentGdi.Create;
    try
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

    SetLength(PdfBuffer, PdfStream.Size);
    PdfStream.Position := 0;
    PdfStream.ReadBuffer(PdfBuffer[0], PdfStream.Size);

    PageCount := CountOccurrences('/Page/', StringToUTF8(TEncoding.ANSI.GetString(PdfBuffer)));
    CheckEqual(3, PageCount, 'The number of pages in the PDF is incorrect');
  finally
    PdfStream.Free;
  end;
end;

procedure TTestUiPdf.TestPDFMetadata;
var
  Pdf: TPdfDocumentGDI;
  PdfStream: TMemoryStream;
  PdfBuffer: TBytes;
  PdfContent: string;
begin
  PdfStream := TMemoryStream.Create;
  try
    Pdf := TPdfDocumentGDI.Create;
    try
      Pdf.Info.Author := 'mORMot Test Framework';
      Pdf.Info.Title := 'PDF Metadata Test';
      Pdf.Info.Subject := 'Testing PDF Metadata Fields';
      Pdf.SaveToStream(PdfStream);
    finally
      Pdf.Free;
    end;

    Check(PdfStream.Size > 0, 'PDF stream should not be empty after adding metadata');

    SetLength(PdfBuffer, PdfStream.Size);
    PdfStream.Position := 0;
    PdfStream.ReadBuffer(PdfBuffer[0], PdfStream.Size);

    PdfContent := TEncoding.ANSI.GetString(PdfBuffer);
    Check(Pos('/Author(mORMot Test Framework)', PdfContent) > 0, 'PDF metadata author field is incorrect or missing');
    Check(Pos('/Title(PDF Metadata Test)', PdfContent) > 0, 'PDF metadata title field is incorrect or missing');
    Check(Pos('/Subject(Testing PDF Metadata Fields)', PdfContent) > 0, 'PDF metadata subject field is incorrect or missing');
  finally
    PdfStream.Free;
  end;
end;

{$endif}
{$endif}
{$endif}

end.