program ConcurrencySpeedTest;

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
  mormot.core.rtti,
  mormot.core.unicode,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TConcurrencyTestServer;

begin
  // Port of DMVC logging setup
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Concurrency Speed Test');
  WriteLn('===============================');
  WriteLn('Port of DMVC concurrency_speed_test to mORMot2');
  WriteLn;

  try
    srv := TConcurrencyTestServer.Create('9999');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:9999');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:9999/api');
      WriteLn;
      WriteLn('This sample demonstrates concurrent request handling with mORMot2.');
      WriteLn('Use a load testing tool like Apache Bench, wrk, or Locust to test:');
      WriteLn;
      WriteLn('Example with Apache Bench (10000 requests, 100 concurrent):');
      WriteLn('  ab -n 10000 -c 100 http://localhost:9999/api');
      WriteLn;
      WriteLn('Example with wrk (10 threads, 100 connections, 30 seconds):');
      WriteLn('  wrk -t10 -c100 -d30s http://localhost:9999/api');
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
