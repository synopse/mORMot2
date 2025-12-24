program CustomAuthSample;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  auth.handler in 'src\auth.handler.pas';

var
  server: TCustomAuthSampleServer;
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
  WriteLn('  mORMot2 Custom Authentication Sample');
  WriteLn('  Converted from DelphiMVCFramework custom_auth example');
  WriteLn('=============================================================');
  WriteLn('');
  WriteLn('This sample demonstrates custom authentication in mORMot2:');
  WriteLn('  - Custom user validation (hardcoded users)');
  WriteLn('  - Role-based authorization (admin, role1, role2)');
  WriteLn('  - Per-method access control');
  WriteLn('  - Public endpoints (no auth)');
  WriteLn('  - Protected endpoints (auth required)');
  WriteLn('');
  WriteLn('Static Users:');
  WriteLn('  - admin / adminpass  (role: admin)');
  WriteLn('  - user1 / user1pass  (role: role1)');
  WriteLn('  - user2 / user2pass  (role: role2)');
  WriteLn('');
  WriteLn('Starting server on port ', port, '...');
  WriteLn('');

  try
    server := TCustomAuthSampleServer.Create(port);
    try
      server.Start;
      WriteLn('');
      WriteLn('Server is running. Test endpoints:');
      WriteLn('');
      WriteLn('=== Public Endpoints (No Auth) ===');
      WriteLn('');
      WriteLn('# Public index:');
      WriteLn('curl -X POST http://localhost:', port, '/root/PublicApi.Index -H "Content-Type: application/json" -d "{}"');
      WriteLn('');
      WriteLn('# Public action in private controller:');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.PublicAction -H "Content-Type: application/json" -d "{}"');
      WriteLn('');
      WriteLn('=== Authentication ===');
      WriteLn('');
      WriteLn('# Login as admin:');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"admin\",\"password\":\"adminpass\"}"');
      WriteLn('');
      WriteLn('# Login as user1:');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"user1\",\"password\":\"user1pass\"}"');
      WriteLn('');
      WriteLn('# Login as user2:');
      WriteLn('curl -X POST http://localhost:', port, '/root/Auth -H "Content-Type: application/json" -d "{\"userName\":\"user2\",\"password\":\"user2pass\"}"');
      WriteLn('');
      WriteLn('=== Protected Endpoints (Auth Required) ===');
      WriteLn('');
      WriteLn('After logging in, use the returned session_signature in subsequent requests:');
      WriteLn('');
      WriteLn('# Admin-only endpoint (requires admin role):');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.Index -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
      WriteLn('');
      WriteLn('# Role1-only endpoint:');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.OnlyRole1 -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
      WriteLn('');
      WriteLn('# Role2-only endpoint:');
      WriteLn('curl -X POST http://localhost:', port, '/root/PrivateApi.OnlyRole2 -H "Content-Type: application/json" -H "Authorization: Bearer SESSION_ID+SIGNATURE" -d "{}"');
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
      WriteLn('   role1 can access OnlyRole1');
      WriteLn('   role2 can access OnlyRole2');
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
