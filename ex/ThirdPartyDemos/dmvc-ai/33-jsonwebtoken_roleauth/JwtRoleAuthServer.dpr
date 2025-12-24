program JwtRoleAuthServer;

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
  srv: TJwtRoleAuthServer;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('mORMot2 JWT Role-Based Authorization Demo');
  WriteLn('==========================================');
  WriteLn;
  WriteLn('This sample demonstrates JWT authentication with role-based authorization.');
  WriteLn('Users have roles embedded in their JWT tokens, and endpoints enforce role requirements.');
  WriteLn;

  try
    // Create server
    srv := TJwtRoleAuthServer.Create('8080', 'my-secret-key-change-in-production');
    try
      srv.Start;

      WriteLn;
      WriteLn('Valid users (username = password):');
      WriteLn('  - user1 (role1)');
      WriteLn('  - user2 (role2)');
      WriteLn('  - user3 (role1 + role2)');
      WriteLn('  - manager_user (manager role)');
      WriteLn('  - admin_user (admin role)');
      WriteLn('  - finance_user (manager + reports roles)');
      WriteLn('  - audit_user (auditor role)');
      WriteLn;
      WriteLn('Example API calls:');
      WriteLn;
      WriteLn('1. Login and get token:');
      WriteLn('   curl -X POST http://localhost:8080/root/auth/login \');
      WriteLn('     -H "Content-Type: application/json" \');
      WriteLn('     -d ''{"username":"manager_user","password":"manager_user"}''');
      WriteLn;
      WriteLn('2. Use token (save from step 1 response):');
      WriteLn('   TOKEN="<your_token_here>"');
      WriteLn;
      WriteLn('3. Access public endpoint (no auth):');
      WriteLn('   curl http://localhost:8080/root/PublicService/GetPublicMessage');
      WriteLn;
      WriteLn('4. Get user profile (any authenticated user):');
      WriteLn('   curl http://localhost:8080/root/UserService/GetProfile \');
      WriteLn('     -H "Authorization: Bearer $TOKEN"');
      WriteLn;
      WriteLn('5. Manager endpoint (requires manager role):');
      WriteLn('   curl http://localhost:8080/root/ManagerService/GetTeamInfo \');
      WriteLn('     -H "Authorization: Bearer $TOKEN"');
      WriteLn;
      WriteLn('6. Admin endpoint (requires admin role):');
      WriteLn('   curl http://localhost:8080/root/AdminService/GetSystemInfo \');
      WriteLn('     -H "Authorization: Bearer $TOKEN"');
      WriteLn;
      WriteLn('7. Multi-role endpoint (requires manager AND reports):');
      WriteLn('   curl http://localhost:8080/root/MultiRoleService/GetFinancialReport \');
      WriteLn('     -H "Authorization: Bearer $TOKEN"');
      WriteLn;
      WriteLn('8. Either-role endpoint (requires admin OR auditor):');
      WriteLn('   curl http://localhost:8080/root/MultiRoleService/GetAuditLog \');
      WriteLn('     -H "Authorization: Bearer $TOKEN"');
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
