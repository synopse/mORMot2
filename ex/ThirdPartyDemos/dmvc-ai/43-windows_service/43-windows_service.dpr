program WindowsService;

{$I mormot.defines.inc}

{ mORMot2 port of DMVC windows_service sample

  This is a Windows Service that hosts a mORMot2 REST server using
  mORMot2's native TServiceSingle class from mormot.core.os.

  To install:   43-windows_service.exe /install
  To uninstall: 43-windows_service.exe /remove
  To start:     net start "mORMot2 REST Service"
  To stop:      net stop "mORMot2 REST Service"

  Or run as console:
    43-windows_service.exe /console

  Test endpoints:
    http://localhost:8080/root/ServiceApi.GetStatus
    http://localhost:8080/root/ServiceApi.Echo?aMessage=test
}

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
  service.impl in 'src\service.impl.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

{$R *.RES}

var
  Service: TRestServiceImpl;

begin
  // Initialize logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  {$ifdef OSWINDOWS}
  // Handle command line parameters
  if FindCmdLineSwitch('install', ['-', '/'], True) then
  begin
    Service := TRestServiceImpl.Create('mORMot2RestService', 'mORMot2 REST Service');
    try
      if Service.Install then
        WriteLn('Service installed successfully')
      else
        WriteLn('Failed to install service');
    finally
      Service.Free;
    end;
    Exit;
  end;

  if FindCmdLineSwitch('remove', ['-', '/'], True) or
     FindCmdLineSwitch('uninstall', ['-', '/'], True) then
  begin
    Service := TRestServiceImpl.Create('mORMot2RestService', 'mORMot2 REST Service');
    try
      Service.Remove;
      WriteLn('Service removed successfully');
    finally
      Service.Free;
    end;
    Exit;
  end;

  if FindCmdLineSwitch('console', ['-', '/'], True) then
  begin
    // Run as console application (for testing)
    WriteLn('mORMot2 Windows Service Sample - Console Mode');
    WriteLn('=============================================');
    WriteLn;
    Service := TRestServiceImpl.Create('mORMot2RestService', 'mORMot2 REST Service');
    try
      Service.StartRestServer;
      WriteLn('Service started. Press [Enter] to quit.');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET http://localhost:8080/root/ServiceApi.GetStatus');
      WriteLn('  GET http://localhost:8080/root/ServiceApi.Echo?aMessage=test');
      WriteLn;
      ReadLn;
      Service.StopRestServer;
    finally
      Service.Free;
    end;
  end
  else
  begin
    // Run as Windows Service
    Service := TRestServiceImpl.Create('mORMot2RestService', 'mORMot2 REST Service');
    try
      if ServiceSingleRun then
        TSynLog.Add.Log(sllInfo, 'Service terminated normally')
      else
        TSynLog.Add.Log(sllError, 'ServiceSingleRun failed');
    finally
      Service.Free;
    end;
  end;
  {$else}
  WriteLn('This sample requires Windows');
  {$endif OSWINDOWS}
end.
