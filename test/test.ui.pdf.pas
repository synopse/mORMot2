unit test.ui.pdf;

interface

{$I ..\src\mormot.defines.inc}

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  mormot.ui.pdf,
  mormot.core.test;

type
  /// regression tests for mormot.ui.pdf
  TTestUiPdf = class(TSynTestCase)
  private
    function CountOccurrences(const SubStr, Text: string): Integer;
  public

  published
    procedure TestSimplePDFCreation;
    procedure TestTextInsertion;
    procedure TestMultiplePages;
    procedure TestPDFMetadata;
  end;

implementation

function TTestUiPdf.CountOccurrences(const SubStr, Text: string): Integer;
var
  PosIdx: Integer;
begin
  Result := 0;
  PosIdx := Pos(SubStr, Text);
  while PosIdx > 0 do
  begin
    Inc(Result);
    PosIdx := PosEx(SubStr, Text, PosIdx + Length(SubStr));
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
    CheckEqual('%PDF-', string(Header), 'PDF header is missing or invalid');

    PdfStream.Position := PdfStream.Size - 6;
    PdfStream.Read(Trailer, SizeOf(Trailer));
    CheckEqual('%%EOF'#10, string(Trailer), 'PDF trailer is missing or invalid');
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

    PageCount := CountOccurrences('/Page/', TEncoding.ANSI.GetString(PdfBuffer));
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

end.
