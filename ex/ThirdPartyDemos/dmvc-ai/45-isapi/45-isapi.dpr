library IsapiSample;

{$I mormot.defines.inc}

{ mORMot2 ISAPI Sample

  This ISAPI extension demonstrates how to deploy a mORMot2 REST server
  as an IIS module.

  Unlike DMVC which uses Delphi's WebBroker framework, this sample creates
  a custom ISAPI-to-mORMot2 bridge that translates IIS requests to mORMot2's
  HTTP processing infrastructure.

  DEPLOYMENT:
  1. Copy 45-isapi.dll to IIS directory (e.g., C:\inetpub\wwwroot\api\)
  2. In IIS Manager: ISAPI and CGI Restrictions → Add → Allow
  3. Configure as ISAPI Filter or Virtual Directory
  4. Test: http://localhost/api/45-isapi.dll/timestamp
}

uses
  {$I mormot.uses.inc}
  Windows,
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  server in 'src\server.pas',
  isapi.handler in 'src\isapi.handler.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  entities in 'src\entities.pas';

{$R *.res}

const
  HSE_VERSION_INFO_SIZE = 64;
  HSE_STATUS_SUCCESS = 1;
  HSE_STATUS_SUCCESS_AND_KEEP_CONN = 3;
  HSE_STATUS_ERROR = 4;

type
  THSE_VERSION_INFO = record
    dwExtensionVersion: DWORD;
    lpszExtensionDesc: array[0..HSE_VERSION_INFO_SIZE-1] of AnsiChar;
  end;
  PHSE_VERSION_INFO = ^THSE_VERSION_INFO;

var
  gServer: TIsapiSampleServer;
  gHandler: TIsapiHandler;

/// ISAPI GetExtensionVersion entry point
function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
const
  DESCRIPTION = 'mORMot2 ISAPI Sample v1.0';
begin
  try
    Ver.dwExtensionVersion := $00010000; // Version 1.0
    StrPCopy(Ver.lpszExtensionDesc, DESCRIPTION);

    // Initialize logging
    TSynLog.Family.Level := LOG_VERBOSE;
    TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

    // Create server and handler (done once when IIS loads the DLL)
    if gServer = nil then
    begin
      gServer := TIsapiSampleServer.Create(':memory:');
      gHandler := TIsapiHandler.Create(gServer.RestServer);
      TSynLog.Add.Log(sllInfo, 'ISAPI extension initialized: %', [DESCRIPTION]);
    end;

    result := True;
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'GetExtensionVersion failed: %', [E.Message]);
      result := False;
    end;
  end;
end;

/// ISAPI HttpExtensionProc entry point - called for each request
function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
begin
  try
    if gHandler = nil then
    begin
      TSynLog.Add.Log(sllError, 'HttpExtensionProc: Handler not initialized');
      result := HSE_STATUS_ERROR;
      exit;
    end;

    // Process the request through mORMot2
    gHandler.ProcessRequest(@ECB);

    // Always return success (actual HTTP status is in response)
    result := HSE_STATUS_SUCCESS;

  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'HttpExtensionProc error: %', [E.Message]);
      result := HSE_STATUS_ERROR;
    end;
  end;
end;

/// ISAPI TerminateExtension entry point
function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;
begin
  try
    TSynLog.Add.Log(sllInfo, 'ISAPI extension terminating');

    FreeAndNil(gHandler);
    FreeAndNil(gServer);

    result := True;
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'TerminateExtension failed: %', [E.Message]);
      result := False;
    end;
  end;
end;

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  // DLL initialization
  IsMultiThread := True;
end.
