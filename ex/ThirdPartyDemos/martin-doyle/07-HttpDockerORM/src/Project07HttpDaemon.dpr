program Project07HttpDaemon;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}
uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.app.daemon,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.db.raw.sqlite3,
  mormot.orm.core,
  mormot.rest.http.server,
  data in 'data.pas',
  server in 'server.pas';

type
  TSampleDaemonSettings = class(TSynDaemonSettings)
  public
    constructor Create; override;
  end;

  TSampleDaemon = class(TSynDaemon)
  protected
    FHttpServer: TRestHttpServer;
    FServer: TSampleServer;
  public
    constructor Create(aSettingsClass: TSynDaemonSettingsClass; const
        aWorkFolder, aSettingsFolder, aLogFolder: TFileName; const
        aSettingsExt: TFileName = '.settings'; const aSettingsName: TFileName =
        '');
    procedure Start; override;
    procedure Stop; override;
  end;


var
  LogFamily: TSynLogFamily;
  Model: TOrmModel;
  SampleServer: TSampleServer;
  HttpServer: TRestHttpServer;
  SampleDaemon: TSampleDaemon;
  ApplicationPath, DataPath, LogPath: string;

{
******************************** TSampleDaemonSettings *********************************
}
constructor TSampleDaemonSettings.Create;
begin
  inherited Create;
  Log := LOG_VERBOSE;
end;


{
******************************** TSampleDaemon *********************************
}
constructor TSampleDaemon.Create(aSettingsClass: TSynDaemonSettingsClass; const
    aWorkFolder, aSettingsFolder, aLogFolder: TFileName; const aSettingsExt:
    TFileName = '.settings'; const aSettingsName: TFileName = '');
begin
  inherited Create(aSettingsClass, aWorkFolder, aSettingsFolder, aLogFolder, aSettingsExt, aSettingsName);
end;

procedure TSampleDaemon.Start;
begin
  SQLite3Log.Enter(self);
  Model := CreateSampleModel;
  SampleServer := TSampleServer.Create(Model, DataPath + Executable.ProgramName + '.db');
  SampleServer.DB.Synchronous := smOff;
  SampleServer.DB.LockingMode := lmExclusive;
  SampleServer.Server.CreateMissingTables;
  HttpServer := TRestHttpServer.Create(HttpPort,[SampleServer],'+',HTTP_DEFAULT_MODE, 4);
  HttpServer.AccessControlAllowOrigin := '*';
  SQLite3Log.Add.Log(sllInfo, 'HttpServer started at Port: ' + HttpPort);
end;

procedure TSampleDaemon.Stop;
begin
  SQLite3Log.Enter(self);
  try
    try
      HttpServer.Free;
      SQLite3Log.Add.Log(sllInfo, 'HttpServer stopped');
    except
      SQLite3Log.Add.Log(sllWarning, 'Error shutting down HttpServer');
    end;
  finally
    try
      SampleServer.Free;
      SQLite3Log.Add.Log(sllInfo, 'Sample Server stopped');
    except
      SQLite3Log.Add.Log(sllWarning, 'Error shutting down Sample Server');
    end;
  Model.Free;
  end;
end;

begin
  LogFamily := SQLite3Log.Family;
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOnFile;
  LogFamily.EchoToConsole := LOG_VERBOSE;
  ApplicationPath := Executable.ProgramFilePath;
  DataPath := ExpandFileName(ApplicationPath + 'data\');
  LogPath := ExpandFileName(ApplicationPath + 'log\');
  LogFamily.DestinationPath := LogPath;
  SQLite3Log.Add.Log(sllInfo, DataPath);
  SQLite3Log.Add.Log(sllInfo, LogPath);
  SQLite3Log.Add.Log(sllInfo, LogFamily.DestinationPath);
  SampleDaemon := TSampleDaemon.Create(TSampleDaemonSettings, ApplicationPath, '', LogPath);
  SQLite3Log.Add.Log(sllInfo, 'Daemon started, listening on port '  + HttpPort);
  try
    SampleDaemon.CommandLine;
  finally
    SQLite3Log.Add.Log(sllInfo, 'Daemon shut down');
  end;

end.
