program JwtLiveValidityServer;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  server in 'src\server.pas',
  auth in 'src\auth.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TJwtLiveValidityServer;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('mORMot2 JWT Live Validity Window Demo');
  WriteLn('=====================================');
  WriteLn;
  WriteLn('This sample demonstrates JWT token validation with SHORT expiration.');
  WriteLn('Tokens expire after 60 seconds to demonstrate live validity checking.');
  WriteLn;

  try
    // Create server with 60 second token validity
    srv := TJwtLiveValidityServer.Create('8080', 'my-secret-key-change-in-production', 60);
    try
      srv.Start;

      WriteLn;
      WriteLn('Test the API:');
      WriteLn('1. Login to get JWT token (valid for 60 seconds):');
      WriteLn('   curl -X POST http://localhost:8080/root/auth/login \');
      WriteLn('     -H "Content-Type: application/json" \');
      WriteLn('     -d ''{"username":"user1","password":"user1"}''');
      WriteLn;
      WriteLn('2. Access public endpoint (no auth):');
      WriteLn('   curl http://localhost:8080/root/PublicApi/PublicInfo');
      WriteLn;
      WriteLn('3. Access protected endpoint (use token from step 1):');
      WriteLn('   curl http://localhost:8080/root/AdminRole1Api/ProtectedRole1 \');
      WriteLn('     -H "Authorization: Bearer YOUR_TOKEN_HERE"');
      WriteLn;
      WriteLn('4. Wait 60+ seconds and try step 3 again - token will be EXPIRED!');
      WriteLn;
      WriteLn('Valid users: user1 (role1), user2 (role2), user3 (both roles)');
      WriteLn('Password = username for demo');
      WriteLn;

      {$ifdef OSWINDOWS}
      WriteLn('Press [Enter] to stop server');
      ReadLn;
      {$else}
      WriteLn('Press Ctrl+C to stop server');
      while true do
        Sleep(1000);
      {$endif}

      srv.Stop;
    finally
      srv.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
