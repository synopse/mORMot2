program RendersSample;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  System.SysUtils,
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  server in 'src\server.pas';

var
  srv: TRendersSampleServer;

begin
  try
    // Configure logging
    with TSynLog.Family do
    begin
      Level := LOG_VERBOSE;
      PerThreadLog := ptIdentifiedInOneFile;
      HighResolutionTimestamp := true;
    end;

    Writeln('mORMot2 Renders Sample');
    Writeln('======================');
    Writeln('');
    Writeln('This sample demonstrates different types of responses:');
    Writeln('- JSON objects and arrays');
    Writeln('- Plain text responses');
    Writeln('- CSV format');
    Writeln('- Binary data handling');
    Writeln('');

    // Create server on port 8080
    srv := TRendersSampleServer.Create('8080', 'renders.db3');
    try
      Writeln('Server is running on http://localhost:8080');
      Writeln('');
      Writeln('API Endpoints:');
      Writeln('');
      Writeln('JSON Responses:');
      Writeln('  GET  /RendersSample/GetPerson?id=1');
      Writeln('       Returns: Single person object as JSON');
      Writeln('');
      Writeln('  GET  /RendersSample/GetPeople');
      Writeln('       Returns: Array of persons as JSON');
      Writeln('');
      Writeln('  GET  /RendersSample/GetCustomer?id=1');
      Writeln('       Returns: Single customer object as JSON');
      Writeln('');
      Writeln('  GET  /RendersSample/GetCustomers');
      Writeln('       Returns: Array of customers as JSON');
      Writeln('');
      Writeln('  GET  /RendersSample/GetPeopleWithMetadata');
      Writeln('       Returns: People array with metadata (processing time, count)');
      Writeln('');
      Writeln('  GET  /RendersSample/GetSimpleArrays');
      Writeln('       Returns: Object with integer, string, and double arrays');
      Writeln('');
      Writeln('Plain Text Response:');
      Writeln('  GET  /RendersSample/GetPersonAsText?id=1');
      Writeln('       Returns: Person formatted as plain text');
      Writeln('');
      Writeln('CSV Response:');
      Writeln('  GET  /RendersSample/GetPeopleAsCSV');
      Writeln('       Returns: People list as CSV format');
      Writeln('');
      Writeln('Binary Data:');
      Writeln('  GET  /RendersSample/GetBinaryData?filename=<path>');
      Writeln('       Returns: Binary file content (base64 in JSON)');
      Writeln('');
      Writeln('  POST /RendersSample/UploadBinaryData');
      Writeln('       Accepts: fieldname, filename, contenttype, data (base64)');
      Writeln('       Returns: JSON with saved filename and reference');
      Writeln('');
      Writeln('Examples using curl:');
      Writeln('');
      Writeln('  # Get all people (JSON array)');
      Writeln('  curl http://localhost:8080/RendersSample/GetPeople');
      Writeln('');
      Writeln('  # Get person as plain text');
      Writeln('  curl http://localhost:8080/RendersSample/GetPersonAsText?id=1');
      Writeln('');
      Writeln('  # Get people as CSV');
      Writeln('  curl http://localhost:8080/RendersSample/GetPeopleAsCSV');
      Writeln('');
      Writeln('  # Get people with metadata');
      Writeln('  curl http://localhost:8080/RendersSample/GetPeopleWithMetadata');
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
