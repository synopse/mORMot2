program SslServer;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  server in 'src\server.pas',
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TSslDemoServer;
  certFile, keyFile: TFileName;
  useSelfSigned: Boolean;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('=================================');
  WriteLn('mORMot2 SSL/HTTPS Server Demo');
  WriteLn('=================================');
  WriteLn('Port of DMVC ssl_server to mORMot2');
  WriteLn;

  try
    srv := TSslDemoServer.Create('8443');
    try
      // Check if certificate files are provided
      certFile := 'cacert.pem';
      keyFile := 'privkey.pem';

      if FileExists(certFile) and FileExists(keyFile) then
      begin
        WriteLn('Using provided SSL certificates:');
        WriteLn('  Certificate: ', certFile);
        WriteLn('  Private Key: ', keyFile);
        WriteLn;
        useSelfSigned := False;
      end
      else
      begin
        WriteLn('SSL certificate files not found.');
        WriteLn('Using self-signed certificate for testing.');
        WriteLn('(Not suitable for production!)');
        WriteLn;
        useSelfSigned := True;
      end;

      // Start the server
      if useSelfSigned then
        srv.StartSelfSigned
      else
        srv.Start(certFile, keyFile, ''); // Empty password

      WriteLn('HTTPS Server started successfully on port 8443');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET https://localhost:8443/         - Single person');
      WriteLn('  GET https://localhost:8443/people   - People list');
      WriteLn;
      WriteLn('Test with curl (use -k to skip certificate verification):');
      WriteLn('  curl -k https://localhost:8443/');
      WriteLn('  curl -k https://localhost:8443/people');
      WriteLn;
      WriteLn('Press [Enter] to quit');
      ReadLn;

      WriteLn('Shutting down server...');
    finally
      srv.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message]);
      ExitCode := 1;
    end;
  end;

  WriteLn('Server stopped.');
end.
