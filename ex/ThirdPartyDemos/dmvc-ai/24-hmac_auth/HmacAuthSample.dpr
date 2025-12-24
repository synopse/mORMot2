program HmacAuthSample;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

{ mORMot2 HMAC Authentication Demo

  This example demonstrates how to secure an API using HMAC signatures.
  Unlike simple password authentication, HMAC provides:
  - Request integrity (tampering detection)
  - Replay attack prevention (via timestamps)
  - No password transmission over the wire

  HOW IT WORKS:
  1. Client and server share a secret key (out-of-band)
  2. Client computes: HMAC-SHA256(timestamp + method + uri + body, secret)
  3. Client sends request with X-Timestamp and X-Signature headers
  4. Server recomputes signature and compares

  HEADERS REQUIRED:
    X-Timestamp: Current ISO8601 timestamp (e.g., "2024-01-15T10:30:00Z")
    X-Signature: HMAC-SHA256 hex digest

  SIGNATURE BASE:
    timestamp + method + uri + body
    Example: "2024-01-15T10:30:00ZGET/SecureApi/GetSecretData"
}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.crypt.core,
  mormot.crypt.secure,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

const
  SERVER_PORT = '8080';
  SECRET_KEY = 'my-super-secret-key-for-hmac-auth'; // Must match server

/// Compute signature the same way the server does
function ComputeClientSignature(const Timestamp, Method, Uri, Body: RawUtf8): RawUtf8;
var
  signatureBase: RawUtf8;
  digest: TSha256Digest;
begin
  signatureBase := Timestamp + Method + Uri + Body;
  HmacSha256(SECRET_KEY, signatureBase, digest);
  Result := LowerCase(Sha256DigestToString(digest));
end;

/// Demo: Show how to compute valid HMAC signature for API calls
procedure ShowSignatureExamples;
var
  timestamp: RawUtf8;
  signature: RawUtf8;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('HMAC Signature Computation Examples');
  WriteLn('========================================');
  WriteLn('');
  WriteLn('Secret Key: ', SECRET_KEY);
  WriteLn('');

  // Example 1: GET request
  timestamp := DateTimeToIso8601(NowUtc, True);
  signature := ComputeClientSignature(timestamp, 'GET', '/SecureApi/GetSecretData', '');

  WriteLn('Example 1: GET /SecureApi/GetSecretData');
  WriteLn('  Timestamp: ', timestamp);
  WriteLn('  Signature: ', signature);
  WriteLn('');
  WriteLn('  curl command:');
  WriteLn('  curl -X GET http://localhost:', SERVER_PORT, '/SecureApi/GetSecretData \');
  WriteLn('    -H "X-Timestamp: ', timestamp, '" \');
  WriteLn('    -H "X-Signature: ', signature, '"');
  WriteLn('');

  // Example 2: GET with parameter
  timestamp := DateTimeToIso8601(NowUtc, True);
  signature := ComputeClientSignature(timestamp, 'GET', '/SecureApi/GetUserInfo', '["user123"]');

  WriteLn('Example 2: GET /SecureApi/GetUserInfo("user123")');
  WriteLn('  Timestamp: ', timestamp);
  WriteLn('  Body: ["user123"]');
  WriteLn('  Signature: ', signature);
  WriteLn('');
  WriteLn('  curl command:');
  WriteLn('  curl -X GET http://localhost:', SERVER_PORT, '/SecureApi/GetUserInfo \');
  WriteLn('    -H "Content-Type: application/json" \');
  WriteLn('    -H "X-Timestamp: ', timestamp, '" \');
  WriteLn('    -H "X-Signature: ', signature, '" \');
  WriteLn('    -d ''["user123"]''');
  WriteLn('');

  // Example 3: POST with data
  timestamp := DateTimeToIso8601(NowUtc, True);
  signature := ComputeClientSignature(timestamp, 'POST', '/SecureApi/SubmitData', '["test data"]');

  WriteLn('Example 3: POST /SecureApi/SubmitData');
  WriteLn('  Timestamp: ', timestamp);
  WriteLn('  Body: ["test data"]');
  WriteLn('  Signature: ', signature);
  WriteLn('');
  WriteLn('  curl command:');
  WriteLn('  curl -X POST http://localhost:', SERVER_PORT, '/SecureApi/SubmitData \');
  WriteLn('    -H "Content-Type: application/json" \');
  WriteLn('    -H "X-Timestamp: ', timestamp, '" \');
  WriteLn('    -H "X-Signature: ', signature, '" \');
  WriteLn('    -d ''["test data"]''');
  WriteLn('');
  WriteLn('========================================');
  WriteLn('');
end;

var
  srv: THmacAuthServer;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('=======================================');
  WriteLn('mORMot2 HMAC Authentication Server');
  WriteLn('=======================================');
  WriteLn('');
  WriteLn('This server demonstrates HMAC-based API authentication:');
  WriteLn('');
  WriteLn('  - All requests to /SecureApi/* require valid HMAC signature');
  WriteLn('  - X-Timestamp header prevents replay attacks (5 min window)');
  WriteLn('  - X-Signature = HMAC-SHA256(timestamp+method+uri+body, secret)');
  WriteLn('');
  WriteLn('SECURITY FEATURES:');
  WriteLn('  [x] Request integrity verification');
  WriteLn('  [x] Replay attack prevention');
  WriteLn('  [x] Constant-time signature comparison');
  WriteLn('  [x] No secrets transmitted');
  WriteLn('');

  try
    srv := THmacAuthServer.Create(SERVER_PORT);
    try
      srv.Start;
      WriteLn('Server running on http://localhost:', SERVER_PORT);
      WriteLn('');

      // Show signature computation examples
      ShowSignatureExamples;

      WriteLn('Try calling the API without headers:');
      WriteLn('  curl http://localhost:', SERVER_PORT, '/SecureApi/GetSecretData');
      WriteLn('  -> Will return 401 Unauthorized');
      WriteLn('');
      WriteLn('Press [Enter] to quit');
      ReadLn;

    finally
      srv.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Server error: %', [E.Message]);
      ExitCode := 1;
    end;
  end;

end.
