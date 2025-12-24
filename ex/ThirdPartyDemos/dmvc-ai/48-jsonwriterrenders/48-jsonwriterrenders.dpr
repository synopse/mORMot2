program jsonwriterrenders;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  System.SysUtils,
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  server in 'src\server.pas';

var
  srv: TJSONWriterSampleServer;

begin
  try
    // Configure logging
    with TSynLog.Family do
    begin
      Level := LOG_VERBOSE;
      PerThreadLog := ptIdentifiedInOneFile;
      HighResolutionTimestamp := true;
    end;

    Writeln('mORMot2 JSON Writer Renders Sample');
    Writeln('===================================');
    Writeln('');
    Writeln('This sample demonstrates custom JSON rendering using mORMot2''s');
    Writeln('automatic JSON serialization (equivalent to DMVC''s TJsonTextWriter)');
    Writeln('');

    // Create server on port 8080
    srv := TJSONWriterSampleServer.Create('8080');
    try
      Writeln('Server is running on http://localhost:8080');
      Writeln('');
      Writeln('API Endpoints:');
      Writeln('');
      Writeln('  GET  /root/JSONWriterSample.GetUsers');
      Writeln('       Returns: Users array with custom JSON structure');
      Writeln('');
      Writeln('Examples using curl:');
      Writeln('');
      Writeln('  # Get users list (JSON with custom structure)');
      Writeln('  curl http://localhost:8080/root/JSONWriterSample.GetUsers');
      Writeln('');
      Writeln('Expected output:');
      Writeln('  {');
      Writeln('    "Users": [');
      Writeln('      {"UserName": "Daniele"},');
      Writeln('      {"UserName": "Peter"},');
      Writeln('      {"UserName": "Scott"}');
      Writeln('    ]');
      Writeln('  }');
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
