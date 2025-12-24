program middleware_trace;

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
  middleware.trace in 'src\middleware.trace.pas';

var
  srv: TTraceServer;
  port: RawUtf8;

begin
  port := '8080';

  WriteLn('mORMot2 Trace Middleware Sample');
  WriteLn('================================');
  WriteLn('Port of DMVCFramework middleware_trace sample');
  WriteLn('');

  try
    srv := TTraceServer.Create(port);
    try
      srv.Start;

      WriteLn('Server started on port ', port);
      WriteLn('Trace logging enabled - check logs for detailed request/response traces');
      WriteLn('');
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:', port, '/root/TraceApi/Index');
      WriteLn('  GET  http://localhost:', port, '/root/TraceApi/GetReversedString?aValue=test');
      WriteLn('  GET  http://localhost:', port, '/root/TraceApi/DoError');
      WriteLn('  GET  http://localhost:', port, '/root/TraceApi/GetCustomers');
      WriteLn('  GET  http://localhost:', port, '/root/TraceApi/GetCustomer?aId=1');
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
