unit server;

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.text,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  api.interfaces;

type
  /// Statistics for logging
  TLogStats = record
    TotalLogEvents: Int64;
    LoggedEvents: Int64;
    FilteredEvents: Int64;
  end;

  /// Custom log event handler for filtering
  // Port of DMVC log filter example - demonstrates EchoCustom callback
  TLogFilterServer = class
  private
    fServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
    fStats: TLogStats;
    /// Custom log filter callback - matches TOnTextWriterEcho signature
    // Returns TRUE to allow logging, FALSE to filter out
    function OnLogFilter(Sender: TEchoWriter; Level: TSynLogLevel;
      const Text: RawUtf8): boolean;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    property Stats: TLogStats read fStats;
  end;

implementation

uses
  SysUtils,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.rtti,
  mormot.orm.core,
  api.impl;

{ TLogFilterServer }

constructor TLogFilterServer.Create(const aPort: RawUtf8);
var
  logFamily: TSynLogFamily;
begin
  // Initialize stats first
  FillCharFast(fStats, SizeOf(fStats), 0);

  // Configure logging with filtering
  logFamily := TSynLog.Family;

  // Set log levels (VERBOSE includes trace, debug, info, warning, error)
  logFamily.Level := LOG_VERBOSE;

  // Enable high resolution timestamps for precise timing
  logFamily.HighResolutionTimestamp := true;

  // Use a single log file for all threads
  logFamily.PerThreadLog := ptIdentifiedInOneFile;

  // Enable console logging (unfiltered - shows everything)
  logFamily.EchoToConsole := LOG_VERBOSE;

  // CRITICAL: Assign our custom filter callback to EchoCustom
  // This callback is called for each log event before writing to file
  // Return TRUE to log, FALSE to filter out
  logFamily.EchoCustom := OnLogFilter;

  WriteLn('Log configuration:');
  WriteLn('  - Log file: LogFilterSample.log');
  WriteLn('  - Log levels: VERBOSE (trace/debug/info/warning/error)');
  WriteLn('  - Filter: Log entries containing "/NotLogged" are filtered');
  WriteLn('  - Console: unfiltered (all log events shown)');
  WriteLn('  - EchoCustom: assigned to OnLogFilter callback');
  WriteLn;

  // Create REST server
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([], false, 'root');

  // Register interface-based service
  fRestServer.ServiceDefine(TLogFilterApi, [ILogFilterApi], sicShared);

  // Create HTTP server
  fServer := TRestHttpServer.Create(aPort, [fRestServer], '+', useHttpSocket);
end;

destructor TLogFilterServer.Destroy;
begin
  // Clear the callback before destroying to avoid dangling reference
  TSynLog.Family.EchoCustom := nil;

  fServer.Free;
  fRestServer.Free;
  inherited;
end;

function TLogFilterServer.OnLogFilter(Sender: TEchoWriter; Level: TSynLogLevel;
  const Text: RawUtf8): boolean;
begin
  // This callback is called for each log event
  // Return TRUE to allow the log entry to be written
  // Return FALSE to filter it out (won't appear in log file)

  Inc(fStats.TotalLogEvents);

  // Filter out log entries containing '/NotLogged' in the URI
  // This demonstrates selective logging based on content
  if PosEx('/NotLogged', Text) > 0 then
  begin
    Inc(fStats.FilteredEvents);
    Result := False; // Filter out - don't write to log file
  end
  else
  begin
    Inc(fStats.LoggedEvents);
    Result := True; // Allow - write to log file
  end;
end;

procedure TLogFilterServer.Start;
begin
  WriteLn('Server initialization complete');
  WriteLn('REST API service registered: ILogFilterApi');
  WriteLn;

  // Log a startup message
  TSynLog.Add.Log(sllInfo, 'LogFilterServer started on port %',
                  [fServer.Port]);
end;

end.
