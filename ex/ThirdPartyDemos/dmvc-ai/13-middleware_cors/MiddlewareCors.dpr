program MiddlewareCors;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  TypInfo,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  server in 'src\server.pas',
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TCorsSampleServer;
  corsMode: TCorsMode;

procedure ShowHelp;
begin
  WriteLn('CORS Middleware Sample - mORMot2 Port');
  WriteLn('======================================');
  WriteLn;
  WriteLn('Demonstrates CORS (Cross-Origin Resource Sharing) configuration in mORMot2');
  WriteLn('Equivalent to DMVCFramework TMVCCORSMiddleware');
  WriteLn;
  WriteLn('Usage: MiddlewareCors [mode]');
  WriteLn;
  WriteLn('Modes:');
  WriteLn('  all       - Allow all origins (*) [DEFAULT]');
  WriteLn('              DMVC: FMVC.AddMiddleware(TMVCCORSMiddleware.Create);');
  WriteLn;
  WriteLn('  specific  - Allow specific origins (localhost:9090, anotherserver.com)');
  WriteLn('              DMVC: TMVCCORSMiddleware.Create(''https://anotherserver.com,http://localhost:9090'')');
  WriteLn;
  WriteLn('  restrict  - Restrictive mode (single origin only)');
  WriteLn('              DMVC: TMVCCORSMiddleware.Create(''https://anotherserver.com'')');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  MiddlewareCors            # Allow all origins');
  WriteLn('  MiddlewareCors all        # Allow all origins');
  WriteLn('  MiddlewareCors specific   # Allow specific origins');
  WriteLn('  MiddlewareCors restrict   # Restrictive mode');
  WriteLn;
end;

function ParseCorsMode(const arg: string): TCorsMode;
var
  mode: string;
begin
  mode := LowerCase(arg);
  if (mode = 'all') or (mode = '') then
    Result := cmAllowAll
  else if mode = 'specific' then
    Result := cmSpecificOrigins
  else if mode = 'restrict' then
    Result := cmRestrictive
  else
  begin
    WriteLn('Error: Invalid mode "', arg, '"');
    WriteLn;
    ShowHelp;
    Halt(1);
  end;
end;

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
  WriteLn('mORMot2 CORS Middleware Sample');
  WriteLn('=======================================');
  WriteLn('Port of DMVCFramework middleware_cors sample');
  WriteLn('=======================================');
  WriteLn;

  // Parse command-line argument
  if ParamCount > 0 then
  begin
    if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') or (ParamStr(1) = '/?') then
    begin
      ShowHelp;
      Exit;
    end;
    corsMode := ParseCorsMode(ParamStr(1));
  end
  else
    corsMode := cmAllowAll;  // Default

  WriteLn('CORS Mode: ', GetEnumName(TypeInfo(TCorsMode), Ord(corsMode))^);
  WriteLn;

  try
    srv := TCorsSampleServer.Create('8080', 'cors_sample.db', corsMode);
    try
      srv.Start;

      WriteLn('Server is running on http://localhost:8080');
      WriteLn;
      WriteLn('CORS Configuration:');
      case corsMode of
        cmAllowAll:
          WriteLn('  ✓ Allows ALL origins (*)');
        cmSpecificOrigins:
          begin
            WriteLn('  ✓ Allows specific origins:');
            WriteLn('    - https://anotherserver.com');
            WriteLn('    - http://localhost:9090');
          end;
        cmRestrictive:
          begin
            WriteLn('  ✓ Restrictive mode:');
            WriteLn('    - https://anotherserver.com ONLY');
            WriteLn('    - http://localhost:9090 BLOCKED');
          end;
      end;
      WriteLn;
      WriteLn('Test the API:');
      WriteLn('  # Create customer (matches DMVC sample)');
      WriteLn('  curl -X POST http://localhost:8080/CustomerApi/CreateCustomer \');
      WriteLn('    -H "Content-Type: application/json" \');
      WriteLn('    -d ''{"hello":"world"}''');
      WriteLn;
      WriteLn('  # Get all customers');
      WriteLn('  curl -X POST http://localhost:8080/CustomerApi/GetAllCustomers \');
      WriteLn('    -H "Content-Type: application/json" \');
      WriteLn('    -d ''{}''');
      WriteLn;
      WriteLn('Test CORS from browser:');
      WriteLn('  1. Open www/index.html in a browser');
      WriteLn('  2. Serve it from http://localhost:9090 (SimpleHTTPServer or similar)');
      WriteLn('  3. Click the CORS buttons to test cross-origin requests');
      WriteLn;
      WriteLn('Press [Enter] to stop the server');
      WriteLn('=======================================');
      WriteLn;

      // Wait for Enter key
      ReadLn;

      HandleShutdown;

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
