program UsingServerInDLL;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

{ mORMot2 port of DMVC server_in_dll host application

  This application loads the ServerDLL.dll and starts/stops the REST server.
  It demonstrates how to use a REST server hosted in a DLL.
}

uses
  {$I mormot.uses.inc}
  SysUtils,
  Windows,
  mormot.core.base,
  mormot.core.os;

type
  TRunServerProc = procedure(const Port: Integer); stdcall;
  TStopServerProc = procedure; stdcall;

var
  DllHandle: THandle;
  RunServer: TRunServerProc;
  StopServer: TStopServerProc;

procedure LoadDll;
var
  DllPath: string;
begin
  // Try to find DLL in same directory
  DllPath := ExtractFilePath(ParamStr(0)) + 'ServerDLL.dll';

  if not FileExists(DllPath) then
  begin
    // Try DLL subdirectory
    DllPath := ExtractFilePath(ParamStr(0)) + '..\dll\Win32\Debug\ServerDLL.dll';
    if not FileExists(DllPath) then
      DllPath := ExtractFilePath(ParamStr(0)) + '..\dll\Win64\Debug\ServerDLL.dll';
  end;

  WriteLn('Loading DLL: ', DllPath);
  DllHandle := LoadLibrary(PChar(DllPath));

  if DllHandle = 0 then
    raise Exception.CreateFmt('Failed to load DLL: %s (Error: %d)', [DllPath, GetLastError]);

  @RunServer := GetProcAddress(DllHandle, 'RunServer');
  @StopServer := GetProcAddress(DllHandle, 'StopServer');

  if not Assigned(RunServer) or not Assigned(StopServer) then
    raise Exception.Create('Failed to load DLL functions');

  WriteLn('DLL loaded successfully');
end;

procedure UnloadDll;
begin
  if DllHandle <> 0 then
  begin
    FreeLibrary(DllHandle);
    WriteLn('DLL unloaded');
  end;
end;

begin
  WriteLn('mORMot2 Server in DLL - Host Application');
  WriteLn('=========================================');
  WriteLn('Port of DMVC server_in_dll to mORMot2');
  WriteLn;

  try
    LoadDll;
    try
      WriteLn('Starting server on port 8080...');
      RunServer(8080);

      WriteLn;
      WriteLn('Server is running!');
      WriteLn('Test with: http://localhost:8080/root/MainApi.GetMessage');
      WriteLn('           http://localhost:8080/root/MainApi.Divide?a=10&b=2');
      WriteLn;
      WriteLn('Press [Enter] to stop server and quit');
      ReadLn;

      WriteLn('Stopping server...');
      StopServer;
    finally
      UnloadDll;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('Application finished');
end.
