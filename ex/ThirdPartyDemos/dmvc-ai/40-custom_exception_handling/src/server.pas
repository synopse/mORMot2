unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.unicode,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  api.interfaces;

type
  /// Main server class with custom exception handling
  // Port of DMVC WebModuleU with custom exception handler
  TExceptionHandlingServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;

    /// Custom exception handler
    // Port of TMVCExceptionHandlerProc from DMVC WebModuleU
    function HandleException(aContext: TRestServerUriContext;
      aException: Exception): Boolean;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Server: TRestServerFullMemory read fServer;
    property HttpServer: TRestHttpServer read fHttpServer;
  end;

implementation

uses
  api.impl;

{ TExceptionHandlingServer }

constructor TExceptionHandlingServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Create data model (empty for service-only server)
  fModel := TOrmModel.Create([], 'exception');

  // Create REST server
  fServer := TRestServerFullMemory.Create(fModel, 'root', {authentication=}false);

  // Register the API service interface
  // Port of DMVC: fMVC.AddController(TMyController)
  fServer.ServiceDefine(TExceptionHandlingApiService, [IExceptionHandlingApi], sicShared);

  // Set up custom exception handler
  // Port of DMVC: fMVC.SetExceptionHandler(lExceptionHandler)
  fServer.OnErrorUri := HandleException;

  TSynLog.Add.Log(sllInfo, 'Custom exception handler registered');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS support
end;

destructor TExceptionHandlingServer.Destroy;
begin
  Stop;

  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);

  inherited;
end;

function TExceptionHandlingServer.HandleException(
  aContext: TRestServerUriContext; aException: Exception): Boolean;
var
  lColor: RawUtf8;
  lHtmlResponse: RawUtf8;
begin
  // Port of custom exception handler from DMVC WebModuleU
  // Similar to TMVCExceptionHandlerProc implementation

  TSynLog.Add.Log(sllWarning, 'Exception in request: % - %',
    [aException.ClassName, aException.Message], aException);

  Result := False; // Return False to use custom handling (True would use default)

  // Handle custom exception with rich error information
  if aException is EMyException then
  begin
    with EMyException(aException) do
    begin
      // Determine color based on severity
      case Severity of
        Fatal, Error:
          lColor := 'red';
        Warning:
          lColor := 'yellow';
        Information:
          lColor := 'blue';
      else
        lColor := 'black';
      end;

      // Build HTML response with custom exception details
      lHtmlResponse := FormatUtf8(
        '<html><body>' +
        '<h1>Error occurred</h1>' +
        '<h2 style="color: %">%</h2>' +
        '<p><strong>Code:</strong> %</p>' +
        '<p><strong>Severity:</strong> %</p>' +
        '<p><strong>Details:</strong> %</p>' +
        '<p><strong>Diagnostics:</strong> %</p>' +
        '<p><strong>Expression:</strong> %</p>' +
        '<p>your truly custom exception handler...</p>' +
        '</body></html>',
        [lColor,
         HtmlEscape(Message),
         Code,
         GetEnumName(TypeInfo(TMyExceptionSeverity), Ord(Severity))^,
         HtmlEscape(Details),
         HtmlEscape(Diagnostics),
         HtmlEscape(Expression)]
      );

      TSynLog.Add.Log(sllError, 'Custom exception - Code: %, Severity: %, Details: %',
        [Code, GetEnumName(TypeInfo(TMyExceptionSeverity), Ord(Severity))^, Details]);
    end;

    aContext.Returns(lHtmlResponse, HTTP_SERVERERROR, 'Content-Type: text/html');
  end
  // Handle standard exceptions
  else
  begin
    lHtmlResponse := FormatUtf8(
      '<html><body>' +
      '<h1>Error occurred</h1>' +
      '<h2 style="color: red">%</h2>' +
      '<p>your truly custom exception handler...</p>' +
      '</body></html>',
      [HtmlEscape(aException.Message)]
    );

    aContext.Returns(lHtmlResponse, HTTP_SERVERERROR, 'Content-Type: text/html');
  end;
end;

procedure TExceptionHandlingServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting HTTP server on port %',
    [fHttpServer.Port]);
end;

procedure TExceptionHandlingServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping HTTP server');
end;

end.
