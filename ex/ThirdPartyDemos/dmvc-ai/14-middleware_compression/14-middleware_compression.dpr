program middleware_compression;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  System.SysUtils,
  mormot.core.base,
  mormot.core.log,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  entities in 'src\entities.pas',
  data.generator in 'src\data.generator.pas';

var
  srv: TCompressionServer;

begin
  try
    // Initialize logging
    TSynLog.Family.Level := LOG_VERBOSE;
    TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

    // Create and start server
    srv := TCompressionServer.Create('8080');
    try
      srv.Start;

      // Wait for user to press Enter
      readln;
    finally
      srv.Free;
    end;

  except
    on E: Exception do
    begin
      writeln('ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
