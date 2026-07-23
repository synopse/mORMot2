
unit app_settings;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.app.daemon;

type
  /// Daemon settings for the Task Manager service (mirrors §15.9's
  /// `TSFDaemonSettings` shape). Persists as JSON next to the executable.
  TTaskManagerSettings = class(TSynDaemonSettings)
  private
    fHttpPort: RawUtf8;
    fRoot: RawUtf8;
    fDBFileName: TFileName;
  public
    constructor Create; override;
  published
    /// HTTP port the public dispatcher will bind to (default '8080').
    property HttpPort: RawUtf8
      read fHttpPort write fHttpPort;
    /// URL root shared by the persistence server and the public dispatcher.
    property Root: RawUtf8
      read fRoot write fRoot;
    /// SQLite database file, resolved relative to the data folder.
    property DBFileName: TFileName
      read fDBFileName write fDBFileName;
  end;

implementation

constructor TTaskManagerSettings.Create;
begin
  inherited Create;
  fHttpPort := '8080';
  fRoot := 'taskmanager';
  fDBFileName := 'tasks.db3';
end;

end.
