
unit app_config;

{$I mormot.defines.inc}

interface

procedure EnableSynLogs;

implementation

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log;

procedure EnableSynLogs;
var
  LogPath: TFileName;
begin
  LogPath := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'logs' + PathDelim;
  ForceDirectories(LogPath);
  with TSynLog.Family do
  begin
    Level := LOG_VERBOSE;
    EchoToConsole := [sllWarning, sllError, sllLastError, sllException];
    PerThreadLog := ptMergedInOneFile;
    DestinationPath := LogPath;
    FileExistsAction := acAppend;
  end;
end;

end.
