program JwtServer;

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
  auth in 'src\auth.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TJwtServer;

procedure HandleShutdown;
begin
  WriteLn;
  WriteLn('Shutdown signal received. Stopping server...');
  TSynLog.Add.Log(sllInfo, 'Shutdown signal received');
  if Assigned(srv) then
  begin
    srv.Free;
    srv := nil;
  end;
  WriteLn('Server stopped. Goodbye!');
end;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('=======================================');
  WriteLn('mORMot2 JWT Authentication Demo');
  WriteLn('=======================================');
  WriteLn('Demonstrates:');
  WriteLn('  - JWT token generation (HS256)');
  WriteLn('  - Login endpoint issuing tokens');
  WriteLn('  - Protected endpoints with JWT verification');
  WriteLn('  - Role-based access control');
  WriteLn('  - Token expiration handling');
  WriteLn('=======================================');
  WriteLn;

  try
    srv := TJwtServer.Create('8080');
    try
      srv.Start;

      WriteLn('Server running on http://localhost:8080');
      WriteLn;
      WriteLn('Test Users (username=password):');
      WriteLn('  user1/user1 (role1)');
      WriteLn('  user2/user2 (role2)');
      WriteLn('  user3/user3 (role1, role2)');
      WriteLn;
      WriteLn('Try the API:');
      WriteLn;
      WriteLn('1. Access public endpoint (no auth):');
      WriteLn('   curl http://localhost:8080/PublicService/GetPublicMessage');
      WriteLn;
      WriteLn('2. Login and get JWT token:');
      WriteLn('   curl -X POST http://localhost:8080/auth/Login \');
      WriteLn('     -H "Content-Type: application/json" \');
      WriteLn('     -d ''{"username":"user1","password":"user1"}''');
      WriteLn;
      WriteLn('3. Use token to access protected endpoint:');
      WriteLn('   TOKEN="<token from step 2>"');
      WriteLn('   curl http://localhost:8080/ProtectedService/GetUserInfo \');
      WriteLn('     -H "Authorization: Bearer $TOKEN"');
      WriteLn;
      WriteLn('   curl http://localhost:8080/ProtectedService/GetAdminInfo \');
      WriteLn('     -H "Authorization: Bearer $TOKEN"');
      WriteLn;
      WriteLn('Press [Enter] to stop the server');
      WriteLn('=======================================');
      WriteLn;

      // Wait for Enter key
      ReadLn;

    except
      on E: Exception do
      begin
        WriteLn('Error during server operation: ', E.Message);
        TSynLog.Add.Log(sllError, 'Server operation error: %', [E.Message]);
        srv.Free;
        srv := nil;
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error creating server: ', E.Message);
      TSynLog.Add.Log(sllError, 'Server creation error: %', [E.Message]);
    end;
  end;

end.
