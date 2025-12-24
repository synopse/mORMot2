unit custom.logger;

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  mormot.core.os,
  mormot.core.rtti;

type
  /// Custom log class alias for clarity
  // Port of: DMVC CustomLoggerConfigU.GetLogger
  TCustomSynLog = TSynLog;

implementation

var
  customPath: TFileName;

procedure SetupCustomLogger;
begin
  // DMVC: TLoggerProFileAppender.Create(10, 1000, TPath.Combine('MyFolder', 'MyLogs'))
  // mORMot2: Use TSynLog.Family to configure custom logging

  // Set custom path 'MyFolder\MyLogs'
  customPath := IncludeTrailingPathDelimiter(GetCurrentDir) + 'MyFolder' +
    PathDelim + 'MyLogs' + PathDelim;

  // Ensure directory exists
  if not DirectoryExists(customPath) then
  begin
    if not DirectoryExists(ExtractFilePath(ExcludeTrailingPathDelimiter(customPath))) then
      CreateDir(ExtractFilePath(ExcludeTrailingPathDelimiter(customPath)));
    CreateDir(customPath);
  end;

  // Configure TSynLog.Family (the default log family)
  TSynLog.Family.DestinationPath := customPath;

  // DMVC: BuildLogWriter([...], nil, TLogType.Debug)
  // mORMot2 equivalent: Set verbose logging level
  TSynLog.Family.Level := LOG_VERBOSE;

  // Configure to write all events to a single file per thread
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  // Auto-flush every second (like LoggerPro)
  TSynLog.Family.AutoFlushTimeOut := 1;

  // Add OutputDebugString appender for Windows (like DMVC sample)
  {$ifdef OSWINDOWS}
  TSynLog.Family.EchoToConsole := LOG_VERBOSE; // Also echo to console/debugger
  {$endif}

  // Set custom log file rotation (10 files, ~1000 lines each - DMVC params)
  TSynLog.Family.RotateFileCount := 10;
  TSynLog.Family.RotateFileSizeKB := 100;  // Approx size for ~1000 lines

  // High resolution timestamps
  TSynLog.Family.HighResolutionTimestamp := true;
end;

initialization
  SetupCustomLogger;

end.
