unit middleware.analytics;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  TypInfo,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.unicode,
  mormot.rest.core,
  mormot.rest.server;

type
  /// Analytics middleware that logs request/response metrics to CSV file
  // Port of DMVC TMVCAnalyticsMiddleware
  TAnalyticsMiddleware = class
  private
    fServer: TRestServer;
    fLogFileName: TFileName;
    fRequestCount: Int64;

    /// OnBeforeUri - called BEFORE request execution
    function OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;

    /// OnAfterUri - called AFTER request execution
    procedure OnAfterUri(Ctxt: TRestServerUriContext);

    /// Logs the analytics data to CSV file
    procedure LogAnalytics(const aClientIP, aMethod, aPath: RawUtf8;
      aStatusCode: Integer; const aAction: RawUtf8; aContentLength: Int64;
      const aHost: RawUtf8);
  public
    /// Creates the analytics middleware and attaches to server events
    // Port of DMVC: .AddMiddleware(TMVCAnalyticsMiddleware.Create(...))
    constructor Create(aServer: TRestServer; const aLogFileName: TFileName);
    destructor Destroy; override;

    property RequestCount: Int64 read fRequestCount;
  end;

implementation

uses
  DateUtils;

{ TAnalyticsMiddleware }

constructor TAnalyticsMiddleware.Create(aServer: TRestServer; const aLogFileName: TFileName);
var
  logDir: TFileName;
  header: RawUtf8;
begin
  inherited Create;

  fServer := aServer;
  fLogFileName := aLogFileName;
  fRequestCount := 0;

  // Ensure log directory exists
  logDir := ExtractFilePath(fLogFileName);
  if logDir <> '' then
    CreateDir(logDir);

  // Create CSV header if file doesn't exist
  if not FileExists(fLogFileName) then
  begin
    header := 'timestamp;level;client_ip;method;path;status;action;content_length;host' + #13#10;
    FileFromString(header, fLogFileName);
  end;

  // Attach middleware events to server
  // Port of DMVC OnBeforeControllerAction/OnAfterRouting
  fServer.OnBeforeUri := OnBeforeUri;
  fServer.OnAfterUri := OnAfterUri;

  TSynLog.Add.Log(sllInfo, 'Analytics middleware initialized - logging to: %', [fLogFileName]);
end;

destructor TAnalyticsMiddleware.Destroy;
begin
  // Detach events
  if fServer <> nil then
  begin
    fServer.OnBeforeUri := nil;
    fServer.OnAfterUri := nil;
  end;

  inherited;
end;

function TAnalyticsMiddleware.OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;
begin
  // Port of DMVC OnBeforeControllerAction
  // Just count requests here - actual logging happens in OnAfterUri
  LockedInc64(@fRequestCount);

  // Don't block the request (return False = continue processing)
  Result := False;
end;

procedure TAnalyticsMiddleware.OnAfterUri(Ctxt: TRestServerUriContext);
var
  clientIP: RawUtf8;
  method: RawUtf8;
  path: RawUtf8;
  statusCode: Integer;
  action: RawUtf8;
  contentLength: Int64;
  host: RawUtf8;
begin
  // Port of DMVC OnAfterRouting
  // Extract request/response information
  clientIP := Ctxt.Call^.LowLevelRemoteIP;
  method := GetEnumName(TypeInfo(TUriMethod), Ord(Ctxt.Method))^;
  path := Ctxt.Call^.Url;
  statusCode := Ctxt.Call^.OutStatus;
  contentLength := Length(Ctxt.Call^.OutBody);

  // Extract host from headers
  FindNameValue(Ctxt.Call^.InHead, 'HOST:', host);

  // Build action name (service.method)
  // Port of DMVC: AControllerQualifiedClassName + '.' + AActionName
  if Ctxt.Service <> nil then
    action := FormatUtf8('%.%', [Ctxt.Service.ClassName, Ctxt.ServiceMethodIndex])
  else
    action := 'Unknown';

  // Log the analytics
  LogAnalytics(clientIP, method, path, statusCode, action, contentLength, host);
end;

procedure TAnalyticsMiddleware.LogAnalytics(const aClientIP, aMethod, aPath: RawUtf8;
  aStatusCode: Integer; const aAction: RawUtf8; aContentLength: Int64;
  const aHost: RawUtf8);
var
  timestamp: RawUtf8;
  level: RawUtf8;
  logLine: RawUtf8;
begin
  // Port of DMVC log format:
  // FormatDateTime('yyyy-mm-dd hh:nn:ss', LogItem.TimeStamp);
  // LogItem.LogTypeAsString;
  // LogItem.LogMessage
  timestamp := StringToUtf8(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  // Map status code to log level
  // Port of DMVC: LOG_LEVEL array
  case aStatusCode div 100 of
    1, 2, 3: level := 'info';
    4: level := 'warning';
    5: level := 'error';
  else
    level := 'info';
  end;

  // Build CSV log line
  // Port of DMVC OnAfterRouting log format
  logLine := FormatUtf8('%;%;%;%;%;%;%;%;%'#13#10,
    [timestamp, level, aClientIP, aMethod, aPath,
     aStatusCode, aAction, aContentLength, aHost]);

  // Append to log file (thread-safe)
  AppendToFile(logLine, fLogFileName);

  // Also log to mORMot log for debugging
  TSynLog.Add.Log(sllCustom1, 'ANALYTICS: % % % % -> %',
    [aMethod, aPath, aClientIP, aStatusCode, level]);
end;

end.
