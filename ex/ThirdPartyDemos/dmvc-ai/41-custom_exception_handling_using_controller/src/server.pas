unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.soa.server,
  api.interfaces;

type
  /// Main server class with controller-level exception handling
  // Port of DMVC WebModuleU without global exception handler
  // Exception handling is done in the controller's OnException method
  TExceptionHandlingServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;

    /// Service-specific exception handler
    // Port of TMyController.OnException
    // Registered for the IExceptionHandlingApi service only
    function HandleServiceException(Ctxt: TRestServerUriContext;
      E: Exception): Boolean;
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

  // Set service-specific exception handler (controller-level)
  // Port of DMVC: TMyController.OnException
  // Note: In mORMot2, this is done at server level but checks the service
  fServer.OnErrorUri := HandleServiceException;

  TSynLog.Add.Log(sllInfo, 'Service registered with controller-level exception handling');

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

function TExceptionHandlingServer.HandleServiceException(
  Ctxt: TRestServerUriContext; E: Exception): Boolean;
var
  lColor: RawUtf8;
  lStatusCode: Cardinal;
  lHtmlResponse: RawUtf8;
begin
  // Port of TMyController.OnException from DMVC
  // This provides service-specific exception handling

  TSynLog.Add.Log(sllWarning, 'Exception in service: % - %',
    [E.ClassName, E.Message], E);

  // Check if client prefers JSON
  if IdemPCharArray(pointer(Ctxt.InHeader['Accept']), ['APPLICATION/JSON']) >= 0 then
  begin
    // Let default mORMot2 error handler process it
    Result := False;
    Exit;
  end;

  Result := True; // We handle it with HTML

  // Determine status code and color based on exception type
  if E is EServiceException then
  begin
    lStatusCode := HTTP_SERVERERROR;  // EServiceException uses 500
    if lStatusCode >= 500 then
      lColor := 'red'
    else if lStatusCode >= 400 then
      lColor := 'yellow'
    else if lStatusCode > 200 then
      lColor := 'blue'
    else
      lColor := 'black';
  end
  else
  begin
    lStatusCode := HTTP_SERVERERROR;
    lColor := 'red';
  end;

  lHtmlResponse := FormatUtf8(
    '<html><body>' +
    '<h1>Error occurred</h1>' +
    '<h2 style="color: %">%</h2>' +
    '<p>your truly custom controller exception handler...</p>' +
    '</body></html>',
    [lColor, HtmlEscape(E.Message)]
  );

  Ctxt.Returns(lHtmlResponse, lStatusCode, 'Content-Type: text/html');
end;

end.
