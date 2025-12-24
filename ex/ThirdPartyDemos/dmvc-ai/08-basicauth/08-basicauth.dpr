program BasicAuth;

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
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  authentication in 'src\authentication.pas';

var
  srv: TBasicAuthServer;

begin
  // Enable verbose logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Basic Authentication Demo');
  WriteLn('==================================');
  WriteLn('Port of DMVC middleware_basicauthentication to mORMot2');
  WriteLn;

  try
    srv := TBasicAuthServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Test credentials (username = password):');
      WriteLn('  user1 / user1  -> has role1');
      WriteLn('  user2 / user2  -> has role2');
      WriteLn('  user3 / user3  -> has role1 and role2');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  PUBLIC:');
      WriteLn('    GET  http://localhost:8080/root/BasicAuthApi.PublicSection');
      WriteLn('    GET  http://localhost:8080/root/BasicAuthApi.Index');
      WriteLn;
      WriteLn('  PROTECTED (require authentication):');
      WriteLn('    GET  http://localhost:8080/root/BasicAuthApi.OnlyRole1');
      WriteLn('         (requires role1 - use user1 or user3)');
      WriteLn('    GET  http://localhost:8080/root/BasicAuthApi.OnlyRole1Json?par1=test');
      WriteLn('         (requires role1 - use user1 or user3)');
      WriteLn('    GET  http://localhost:8080/root/BasicAuthApi.OnlyRole2');
      WriteLn('         (requires role2 - use user2 or user3)');
      WriteLn;
      WriteLn('Example curl commands:');
      WriteLn('  # Public endpoint (no auth):');
      WriteLn('  curl http://localhost:8080/root/BasicAuthApi.PublicSection');
      WriteLn;
      WriteLn('  # Protected endpoint with Basic Auth:');
      WriteLn('  curl -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole1');
      WriteLn('  curl -u user2:user2 http://localhost:8080/root/BasicAuthApi.OnlyRole2');
      WriteLn('  curl -u user3:user3 http://localhost:8080/root/BasicAuthApi.OnlyRole1Json?par1=hello');
      WriteLn;
      WriteLn('  # Test unauthorized access:');
      WriteLn('  curl -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole2');
      WriteLn('  (should return 403 Forbidden)');
      WriteLn;
      WriteLn('Press [Enter] to quit');
      ReadLn;
    finally
      srv.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message]);
    end;
  end;

end.
