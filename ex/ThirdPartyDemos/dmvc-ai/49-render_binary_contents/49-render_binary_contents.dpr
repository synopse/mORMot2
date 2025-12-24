program render_binary_contents;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}

{ mORMot2 Binary Content Rendering Sample

  This example demonstrates how to serve files (PDFs, Images) and raw
  streams efficiently - matching the original DMVC sample intention.

  TWO APPROACHES:
  1. RAW BINARY endpoints (recommended for file downloads)
     - GET /files/<filename>    - inline display
     - GET /download/<filename> - force download dialog

  2. JSON API endpoints (for programmatic access)
     - /BinaryContentSample/GetFileByName - base64 in JSON
     - /BinaryContentSample/UploadBinaryData - file upload

  The raw binary approach is more efficient and faithful to the
  original DMVC sample's goal of "serving files directly".
}

uses
  {$I mormot.uses.inc}
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  System.SysUtils,
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  server in 'src\server.pas';

var
  srv: TBinaryContentSampleServer;

begin
  try
    // Configure logging
    with TSynLog.Family do
    begin
      Level := LOG_VERBOSE;
      PerThreadLog := ptIdentifiedInOneFile;
      HighResolutionTimestamp := true;
    end;

    Writeln('mORMot2 Binary Content Rendering Sample');
    Writeln('========================================');
    Writeln('');
    Writeln('This sample demonstrates serving binary files with proper');
    Writeln('Content-Type headers for direct browser display/download.');
    Writeln('');

    // Create server on port 8080
    srv := TBinaryContentSampleServer.Create('8080');
    try
      Writeln('Server is running on http://localhost:8080');
      Writeln('Files directory: ', srv.FilesPath);
      Writeln('');
      Writeln('=== RAW BINARY ENDPOINTS (Recommended) ===');
      Writeln('');
      Writeln('Inline display (opens in browser):');
      Writeln('  GET http://localhost:8080/files/sample.pdf');
      Writeln('  GET http://localhost:8080/files/image.jpg');
      Writeln('');
      Writeln('Force download dialog:');
      Writeln('  GET http://localhost:8080/download/sample.pdf');
      Writeln('  GET http://localhost:8080/download/image.jpg');
      Writeln('');
      Writeln('=== JSON API ENDPOINTS ===');
      Writeln('');
      Writeln('Get file as base64 (for programmatic access):');
      Writeln('  curl "http://localhost:8080/root/BinaryContentSample.GetFileByName?FileName=test.txt"');
      Writeln('');
      Writeln('Upload file:');
      Writeln('  curl -X POST http://localhost:8080/root/BinaryContentSample.UploadBinaryData \');
      Writeln('    -H "Content-Type: application/json" \');
      Writeln('    -d ''{"fieldname":"file","filename":"test.txt","contenttype":"text/plain","data":"SGVsbG8="}''');
      Writeln('');
      Writeln('=== EXAMPLES ===');
      Writeln('');
      Writeln('1. Create a test file:');
      Writeln('   echo "Hello World" > ', srv.FilesPath, 'test.txt');
      Writeln('');
      Writeln('2. View it directly in browser:');
      Writeln('   http://localhost:8080/files/test.txt');
      Writeln('');
      Writeln('3. Force download:');
      Writeln('   http://localhost:8080/download/test.txt');
      Writeln('');
      Writeln('Press [Enter] to stop the server');
      Readln;
    finally
      srv.Free;
    end;

    Writeln('Server stopped.');
  except
    on E: Exception do
    begin
      Writeln('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
