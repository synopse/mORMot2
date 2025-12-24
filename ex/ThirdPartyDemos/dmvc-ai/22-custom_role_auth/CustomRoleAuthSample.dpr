program CustomRoleAuthSample;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.log,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  auth.handler in 'src\auth.handler.pas';

var
  server: TCustomRoleAuthSampleServer;
  port: RawUtf8;

begin
  // Enable logging to console
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.EchoToConsole := LOG_VERBOSE;

  // Get port from command line or use default
  if ParamCount > 0 then
    port := StringToUtf8(ParamStr(1))
  else
    port := '8080';

  WriteLn('');
  WriteLn('=============================================================');
  WriteLn('  mORMot2 Custom Role-Based Authentication Sample');
  WriteLn('  Converted from DelphiMVCFramework custom_role_auth example');
  WriteLn('=============================================================');
  WriteLn('');
  WriteLn('This sample demonstrates role-based authorization in mORMot2:');
  WriteLn('  - Multiple users with different role combinations');
  WriteLn('  - Role-based access control (role1, role2, role3)');
  WriteLn('  - Per-method authorization checks');
  WriteLn('  - Public endpoints (no auth)');
  WriteLn('  - Protected endpoints (auth required)');
  WriteLn('  - AND/OR role logic');
  WriteLn('  - Dynamic role parameters');
  WriteLn('');
  WriteLn('Static Users:');
  WriteLn('  - admin / adminpass      (roles: admin, role1, role2)');
  WriteLn('  - user1 / user1pass      (roles: role1)');
  WriteLn('  - user2 / user2pass      (roles: role2)');
  WriteLn('  - user1_2 / user1_2pass  (roles: role1, role2)');
  WriteLn('  - user3 / user3pass      (roles: role3)');
  WriteLn('');
  WriteLn('Starting server on port ', port, '...');
  WriteLn('');

  try
    server := TCustomRoleAuthSampleServer.Create(port);
    try
      server.Start;
      WriteLn('');
      WriteLn('Server is running. Test endpoints:');
      WriteLn('');
      WriteLn('=== Public Endpoints (No Auth) ===');
      WriteLn('');
      WriteLn('# Public action in private controller:');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.PublicAction -H "Content-Type: application/json" -d "{}"');
      WriteLn('');
      WriteLn('=== Authentication ===');
      WriteLn('');
      WriteLn('# Login as admin (has admin, role1, role2):');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"admin\",\"password\":\"adminpass\"}"');
      WriteLn('');
      WriteLn('# Login as user1 (has role1):');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"user1\",\"password\":\"user1pass\"}"');
      WriteLn('');
      WriteLn('# Login as user2 (has role2):');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"user2\",\"password\":\"user2pass\"}"');
      WriteLn('');
      WriteLn('# Login as user1_2 (has role1 and role2):');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"user1_2\",\"password\":\"user1_2pass\"}"');
      WriteLn('');
      WriteLn('# Login as user3 (has role3):');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"user3\",\"password\":\"user3pass\"}"');
      WriteLn('');
      WriteLn('=== Protected Endpoints (Auth Required) ===');
      WriteLn('');
      WriteLn('After logging in, use the returned session_signature in subsequent requests:');
      WriteLn('');
      WriteLn('# Index (any authenticated user):');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.Index -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
      WriteLn('');
      WriteLn('# Role1-only endpoint (user1, user1_2, or admin):');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.OnlyRole1 -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
      WriteLn('');
      WriteLn('# Role2-only endpoint (user2, user1_2, or admin):');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.OnlyRole2 -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
      WriteLn('');
      WriteLn('# Role1 AND Role2 endpoint (user1_2 or admin only):');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.OnlyRole1And2 -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
      WriteLn('');
      WriteLn('# Role1 OR Role2 endpoint (user1, user2, user1_2, or admin):');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.OnlyRole1Or2 -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
      WriteLn('');
      WriteLn('# Dynamic role endpoint (role parameter):');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.AccessByRole -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{\"role\":\"role1\"}"');
      WriteLn('');
      WriteLn('=== Notes ===');
      WriteLn('');
      WriteLn('1. The Auth endpoint returns a JSON object with:');
      WriteLn('   - "result": session ID');
      WriteLn('   - "data": { "session_signature": "..." }');
      WriteLn('');
      WriteLn('2. Use session_signature in Authorization header for protected endpoints');
      WriteLn('');
      WriteLn('3. Admin role can access all endpoints');
      WriteLn('   role1 can access OnlyRole1, OnlyRole1Or2');
      WriteLn('   role2 can access OnlyRole2, OnlyRole1Or2');
      WriteLn('   role1+role2 (user1_2) can access all role-based endpoints');
      WriteLn('');
      WriteLn('4. OnlyRole1And2 requires BOTH roles (user1_2 or admin)');
      WriteLn('   OnlyRole1Or2 requires EITHER role (user1, user2, user1_2, or admin)');
      WriteLn('');
      WriteLn('Press ENTER to stop the server...');
      ReadLn;
    finally
      server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('Server stopped.');
end.
