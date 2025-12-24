program middleware_analytics;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  middleware.analytics in 'src\middleware.analytics.pas';

var
  srv: TAnalyticsServer;
  port: RawUtf8;

begin
  // Port of DMVC main program structure
  port := '8080';

  WriteLn('mORMot2 Analytics Middleware Sample');
  WriteLn('====================================');
  WriteLn('Port of DMVCFramework middleware_analytics sample');
  WriteLn('');

  try
    // Create and start server
    srv := TAnalyticsServer.Create(port);
    try
      srv.Start;

      WriteLn('Server started on port ', port);
      WriteLn('Analytics logging enabled - check logs directory');
      WriteLn('');
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:', port, '/root/AnalyticsApi/Index');
      WriteLn('  GET  http://localhost:', port, '/root/AnalyticsApi/GetReversedString?aValue=test');
      WriteLn('  GET  http://localhost:', port, '/root/AnalyticsApi/DoError');
      WriteLn('  GET  http://localhost:', port, '/root/AnalyticsApi/GetCustomers');
      WriteLn('  GET  http://localhost:', port, '/root/AnalyticsApi/GetCustomer?aId=1');
      WriteLn('');
      WriteLn('Press [Enter] to quit');
      ReadLn;

      WriteLn('Shutting down...');
    finally
      srv.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('Bye!');
end.
