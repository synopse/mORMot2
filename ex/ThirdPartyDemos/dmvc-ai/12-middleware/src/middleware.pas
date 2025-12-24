unit middleware;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  TypInfo,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.unicode,
  mormot.rest.core,
  mormot.rest.server;

type
  /// Custom middleware for adding response headers
  // Port of DMVC TMVCSalutationMiddleware
  // Demonstrates: OnAfterControllerAction -> OnAfterUri
  TCustomHeaderMiddleware = class
  private
    fServer: TRestServer;
    fPreviousOnAfterUri: TOnAfterUri;
  public
    constructor Create(aServer: TRestServer);

    /// Called after service method execution
    procedure OnAfterUri(Ctxt: TRestServerUriContext);
  end;

  /// Middleware for user-agent based redirection
  // Port of DMVC TMVCRedirectAndroidDeviceOnPlayStore
  // Demonstrates: OnBeforeRouting -> OnBeforeUri
  TUserAgentRedirectMiddleware = class
  private
    fServer: TRestServer;
    fPreviousOnBeforeUri: TOnBeforeUri;
  public
    constructor Create(aServer: TRestServer);

    /// Called before request routing
    function OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;
  end;

  /// Request logging middleware
  // Demonstrates: OnBeforeUri/OnAfterUri events for before/after hooks
  TRequestLoggerMiddleware = class
  private
    fServer: TRestServer;
    fRequestCount: Integer;
    fPreviousOnBeforeUri: TOnBeforeUri;
    fPreviousOnAfterUri: TOnAfterUri;
  public
    constructor Create(aServer: TRestServer);

    /// Called before request processing (returns true to continue)
    function OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;

    /// Called after request processing
    procedure OnAfterUri(Ctxt: TRestServerUriContext);

    property RequestCount: Integer read fRequestCount;
  end;

implementation

uses
  mormot.core.log,
  mormot.core.datetime;

{ TCustomHeaderMiddleware }

constructor TCustomHeaderMiddleware.Create(aServer: TRestServer);
begin
  inherited Create;
  fServer := aServer;

  // Save previous handler to maintain the chain
  fPreviousOnAfterUri := fServer.OnAfterUri;

  // Hook into after-service callback
  // This is the mORMot2 equivalent of DMVC's OnAfterControllerAction
  fServer.OnAfterUri := OnAfterUri;
end;

procedure TCustomHeaderMiddleware.OnAfterUri(Ctxt: TRestServerUriContext);
begin
  // Port of TMVCSalutationMiddleware.OnAfterControllerAction
  // Add custom header to ALL responses

  if Ctxt.Call^.OutHead = '' then
    Ctxt.Call^.OutHead := 'X-Powered-By: mORMot2 (https://github.com/synopse/mORMot2)'
  else
    Ctxt.Call^.OutHead := Ctxt.Call^.OutHead + #13#10 +
      'X-Powered-By: mORMot2 (https://github.com/synopse/mORMot2)';

  TSynLog.Add.Log(sllTrace, 'CustomHeaderMiddleware: Added X-Powered-By header');

  // Call previous handler in the chain
  if Assigned(fPreviousOnAfterUri) then
    fPreviousOnAfterUri(Ctxt);
end;

{ TUserAgentRedirectMiddleware }

constructor TUserAgentRedirectMiddleware.Create(aServer: TRestServer);
begin
  inherited Create;
  fServer := aServer;

  // Save previous handler to maintain the chain
  fPreviousOnBeforeUri := fServer.OnBeforeUri;

  // Hook into before-URI callback
  // This is the mORMot2 equivalent of DMVC's OnBeforeRouting
  fServer.OnBeforeUri := OnBeforeUri;
end;

function TUserAgentRedirectMiddleware.OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;
var
  userAgent: RawUtf8;
begin
  // Port of TMVCRedirectAndroidDeviceOnPlayStore.OnBeforeRouting
  // Extract User-Agent header
  FindNameValue(Ctxt.Call^.InHead, 'USER-AGENT:', userAgent);

  TSynLog.Add.Log(sllTrace, 'UserAgentRedirect: User-Agent=%', [userAgent]);

  // Check if Android device
  if PosEx('Android', userAgent) > 0 then
  begin
    // Redirect to Play Store
    Ctxt.Call^.OutStatus := HTTP_TEMPORARYREDIRECT; // 307
    Ctxt.Call^.OutHead := 'Location: https://play.google.com';

    TSynLog.Add.Log(sllInfo, 'UserAgentRedirect: Redirecting Android device to Play Store');

    Result := False; // Request handled, stop processing (return false to stop)
  end
  else
  begin
    // Call previous handler in the chain
    if Assigned(fPreviousOnBeforeUri) then
      Result := fPreviousOnBeforeUri(Ctxt)
    else
      Result := True; // Continue normal processing
  end;
end;

{ TRequestLoggerMiddleware }

constructor TRequestLoggerMiddleware.Create(aServer: TRestServer);
begin
  inherited Create;
  fServer := aServer;
  fRequestCount := 0;

  // Save previous handlers to maintain the chain
  fPreviousOnBeforeUri := fServer.OnBeforeUri;
  fPreviousOnAfterUri := fServer.OnAfterUri;

  // Hook both events for complete request/response logging
  fServer.OnBeforeUri := OnBeforeUri;
  fServer.OnAfterUri := OnAfterUri;
end;

function TRequestLoggerMiddleware.OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;
begin
  // Log incoming request (before processing)
  Inc(fRequestCount);

  TSynLog.Add.Log(sllInfo, 'REQUEST #% [%] % % - Headers: % bytes',
    [fRequestCount,
     NowToString,
     GetEnumName(TypeInfo(TUriMethod), Ord(Ctxt.Method))^,
     Ctxt.Call^.Url,
     Length(Ctxt.Call^.InHead)]);

  if Ctxt.Call^.InHead <> '' then
    TSynLog.Add.Log(sllTrace, 'Request Headers:'#13#10'%', [Ctxt.Call^.InHead]);

  // Call previous handler in the chain
  if Assigned(fPreviousOnBeforeUri) then
    Result := fPreviousOnBeforeUri(Ctxt)
  else
    Result := True; // Continue processing
end;

procedure TRequestLoggerMiddleware.OnAfterUri(Ctxt: TRestServerUriContext);
var
  duration: Int64;
begin
  // Log response (after processing)
  duration := Ctxt.MicroSecondsElapsed div 1000; // Convert to milliseconds

  TSynLog.Add.Log(sllInfo, 'RESPONSE [%] % % -> Status: % - Body: % bytes - Duration: % ms',
    [NowToString,
     GetEnumName(TypeInfo(TUriMethod), Ord(Ctxt.Method))^,
     Ctxt.Call^.Url,
     Ctxt.Call^.OutStatus,
     Length(Ctxt.Call^.OutBody),
     duration]);

  if Ctxt.Call^.OutHead <> '' then
    TSynLog.Add.Log(sllTrace, 'Response Headers:'#13#10'%', [Ctxt.Call^.OutHead]);

  // Call previous handler in the chain
  if Assigned(fPreviousOnAfterUri) then
    fPreviousOnAfterUri(Ctxt);
end;

end.
