library ServerDLL;

{$I mormot.defines.inc}

{ mORMot2 port of DMVC server_in_dll sample

  This DLL exports a REST server that can be hosted in any application.
  The DLL provides a simple interface-based API for division operations.

  Original DMVC sample used ShareMem/BORLNDMM.DLL for string handling.
  mORMot2 uses RawUtf8 which doesn't require external memory managers.
}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.text,
  server in '..\src\server.pas',
  api.interfaces in '..\src\api.interfaces.pas',
  api.impl in '..\src\api.impl.pas';

{$R *.res}

var
  gServer: TDllRestServer;

/// Start the REST server on the specified port
procedure RunServer(const Port: Integer); stdcall;
begin
  try
    gServer := TDllRestServer.Create(UInt32ToUtf8(Port));
    gServer.Start;
    TSynLog.Add.Log(sllInfo, 'DLL server started on port %', [Port]);
  except
    on E: Exception do
      TSynLog.Add.Log(sllError, 'RunServer failed: %', [E.Message]);
  end;
end;

/// Stop the REST server
procedure StopServer; stdcall;
begin
  try
    if Assigned(gServer) then
    begin
      gServer.Stop;
      FreeAndNil(gServer);
      TSynLog.Add.Log(sllInfo, 'DLL server stopped');
    end;
  except
    on E: Exception do
      TSynLog.Add.Log(sllError, 'StopServer failed: %', [E.Message]);
  end;
end;

exports
  RunServer,
  StopServer;

begin
  // Initialize logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
end.
