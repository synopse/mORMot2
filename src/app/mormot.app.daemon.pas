///  Middle-Level POSIX Daemon / Windows Service Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.app.daemon;

{
  *****************************************************************************

   Daemon (e.g. Windows Service) Stand-Alone Background Executable Support
    - Parent Daemon Settings Class
    - Parent Daemon Application Class

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.data,
  mormot.core.log,
  mormot.crypt.core;



{ ************ Parent Daemon Settings Class }

type
  /// abstract parent containing information able to initialize a TSynDaemon class
  // - will handle persistence as JSON local files
  // - could fallback to read an .INI file if no valid JSON is found
  // - by default in this abstract parent class, no property is published to let
  // inherited classes define the values customizable from JSON serialization
  TSynDaemonAbstractSettings  = class(TSynJsonFileSettings)
  protected
    fServiceName: RawUtf8;
    fServiceDisplayName: RawUtf8;
    fServiceExecutable: TFileName;
    {$ifdef OSWINDOWS}
    fServiceDependencies: RawUtf8;
    {$endif OSWINDOWS}
    fLog: TSynLogInfos;
    fLogRotateFileCount: integer;
    fLogPath: TFileName;
    fLogClass: TSynLogClass;
  public
    /// initialize and set the default settings
    constructor Create; override;
    /// define the log information into the supplied TSynLog class
    // - if you don't call this method, the logging won't be initiated
    // - called by default in TSynDaemon.AfterCreate if RunFromSynTests=false
    procedure SetLog(aLogClass: TSynLogClass);
    /// returns user-friendly description of the service, including version
    // information and company copyright (if available)
    // - returns a string instance, just like Executable.Version methods
    function ServiceDescription: string; virtual;
    /// read-only access to the TSynLog class, if SetLog() has been called
    property LogClass: TSynLogClass
      read fLogClass;
    {$ifdef OSWINDOWS}
    /// optional service dependencies (Windows Only)
    // - not published by default: could be defined if needed, or e.g. set in
    // overriden constructor
    // - several depending services may be set by appending ';' between names
    property ServiceDependencies: RawUtf8
      read fServiceDependencies write fServiceDependencies;
    {$endif OSWINDOWS}
    /// the service name, as used internally by Windows or the TSynDaemon class
    // - default is the executable name
    // - the service name should better be simple enough to be used as part of a
    // short file name, if needed - use ServiceDisplayName for anything complex
    property ServiceName: RawUtf8
      read fServiceName write fServiceName;
    /// the service name, as displayed by Windows or at the console level
    // - default is the executable name
    property ServiceDisplayName: RawUtf8
      read fServiceDisplayName write fServiceDisplayName;
    /// the service executable path and parameters
    // - default is void '', so the executable name (with full path) will be used
    // - by definition, is available only on Windows
    property ServiceExecutable: TFileName
      read fServiceExecutable write fServiceExecutable;
    /// if not void, will enable the logs (default is LOG_STACKTRACE)
    property Log: TSynLogInfos
      read fLog write fLog;
    /// allow to customize where the logs should be written
    property LogPath: TFileName
      read fLogPath write fLogPath;
    /// how many files will be rotated
    // - default is 2
    property LogRotateFileCount: integer
      read fLogRotateFileCount write fLogRotateFileCount;
  end;

  /// abstract parent containing information able to initialize a TSynDaemon class
  // - by default, will publish the main properties to that RTTI will handle
  // properly persistence of those fields in the JSON local settings file
  TSynDaemonSettings  = class(TSynDaemonAbstractSettings)
  published
    /// the service name, as used internally by Windows or the TSynDaemon class
    // - default is the executable name
    property ServiceName;
    /// the service name, as displayed by Windows or at the console level
    // - default is the executable name
    property ServiceDisplayName;
    /// the service executable path and parameters
    // - default is none '', so the executable name (with full path) will be used
    property ServiceExecutable;
    /// if not void, will enable the logs (default is LOG_STACKTRACE)
    property Log: TSynLogInfos
      read fLog write fLog;
    /// allow to customize where the logs should be written
    property LogPath: TFileName
      read fLogPath write fLogPath;
    /// how many files will be rotated (default is 2)
    property LogRotateFileCount;
  end;

  /// meta-class of TSynDaemon settings information
  TSynDaemonSettingsClass = class of TSynDaemonAbstractSettings;



{ ************ Parent Daemon Application Class }

type
  /// exception raised during POSIX Daemon / Windows Service process
  EDaemon = class(ESynException);

  /// the commands recognized by TSynDaemon
  // - include commands on all systems, even not the current one
  TExecuteCommandLineCmd = (
     cNone,
     cVersion,
     cVerbose,
     cStart,
     cStop,
     cState,
     cSilentKill,
     cHelp,
     cInstall,
     cRun,
     cFork,
     cUninstall,
     cConsole,
     cKill);

  /// abstract parent to implements a POSIX Daemon / Windows Service
  // - inherit from this abstract class and override Start and Stop methods
  // - you may consider using TDDDAdministratedDaemon from dddInfraApps
  TSynDaemon = class(TSynPersistent)
  protected
    fConsoleMode: boolean;
    fShowExceptionWaitEnter: boolean;
    fWorkFolderName: TFileName;
    fSettings: TSynDaemonAbstractSettings;
    procedure AfterCreate; virtual; // call fSettings.SetLog() if not from tests
    {$ifdef OSWINDOWS}
    procedure DoStart(Sender: TService);
    procedure DoResume(Sender: TService);
    procedure DoStop(Sender: TService);
    {$endif OSWINDOWS}
    procedure WriteCopyright;
    function CustomParseCmd(P: PUtf8Char): boolean; virtual;
    function CustomCommandLineSyntax: string; virtual;
  public
    /// initialize the daemon, creating the associated settings
    // - TSynDaemonSettings instance will be owned and freed by the daemon
    // - any non supplied folder name will be replaced by a default value
    // (executable folder under Windows, or /etc /var/log on Linux)
    constructor Create(aSettingsClass: TSynDaemonSettingsClass;
      const aWorkFolder, aSettingsFolder, aLogFolder: TFileName;
      const aSettingsExt: TFileName = '.settings';
      const aSettingsName: TFileName = '';
      aSettingsOptions: TSynJsonFileSettingsOptions = [];
      const aSectionName: RawUtf8 = 'Main'); reintroduce;
    /// main entry point of the daemon, to process the command line switches
    // - aAutoStart is used only under Windows
    procedure CommandLine(aAutoStart: boolean = true);
    /// alternate entry point of the daemon, for direct execution
    // - param is used when called from CommandLine()
    // - aAutoStart is used only under Windows
    procedure Command(cmd: TExecuteCommandLineCmd; aAutoStart: boolean = true;
      const param: RawUtf8 = ''); virtual;
    /// retrieve the current state of this daemon/service
    function CurrentState: TServiceState;
    /// inherited class should override this abstract method with proper process
    procedure Start; virtual; abstract;
    /// inherited class should override this abstract method with proper process
    // - should do nothing if the daemon was already stopped
    procedure Stop; virtual; abstract;
    /// inherited class should override this abstract method with proper process
    // - do nothing by default, but could handle SERVICE_CONTROL_CONTINUE signal
    // - is currently called only on Windows
    procedure Resume; virtual;
    /// call Stop, finalize the instance, and its settings
    destructor Destroy; override;
  published
    /// if this instance was run as /console or /verb
    property ConsoleMode: boolean
      read fConsoleMode;
    /// the settings associated with this daemon
    // - will be allocated in Create constructor, and released in Destroy
    property Settings: TSynDaemonAbstractSettings
      read fSettings;
  end;


implementation


{ ************ Parent Daemon Settings Class }

{ TSynDaemonAbstractSettings }

constructor TSynDaemonAbstractSettings.Create;
begin
  inherited Create;
  fLog := LOG_STACKTRACE + [sllNewRun];
  fLogRotateFileCount := 2;
  fServiceName := Executable.ProgramName;
  fServiceDisplayName := fServiceName;
end;

function TSynDaemonAbstractSettings.ServiceDescription: string;
var
  versionnumber: string;
  v: TFileVersion;
begin
  Utf8ToStringVar(fServiceDisplayName, result{%H-});
  v := Executable.Version;
  versionnumber := v.DetailedOrVoid;
  if versionnumber <> '' then
    result := result + ' ' + versionnumber;
  if v.CompanyName <> '' then
    result := FormatString('% - (c)% %', [result, v.BuildYear, v.CompanyName]);
end;

procedure TSynDaemonAbstractSettings.SetLog(aLogClass: TSynLogClass);
var
  f: TSynLogFamily;
begin
  if RunFromSynTests then
    aLogClass := TSynLog; // don't overwrite the tests log options
  if (self = nil) or
     (Log = []) or
     (aLogClass = nil) then
    exit;
  fLogClass := aLogClass;
  f := aLogClass.Family;
  f.DestinationPath := fLogPath;
  f.PerThreadLog := ptIdentifiedInOneFile; // ease multi-threaded server debug
  f.RotateFileCount := fLogRotateFileCount;
  if fLogRotateFileCount > 0 then
  begin
    f.RotateFileSizeKB := 20 * 1024; // rotate by 20 MB logs
    f.FileExistsAction := acAppend;  // as expected in rotation mode
  end
  else
    f.HighResolutionTimestamp := true;
  f.Level := fLog;
end;



{ ************ Parent Daemon Application Class }

{ TSynDaemon }

constructor TSynDaemon.Create(aSettingsClass: TSynDaemonSettingsClass;
  const aWorkFolder, aSettingsFolder, aLogFolder,
        aSettingsExt, aSettingsName: TFileName;
        aSettingsOptions: TSynJsonFileSettingsOptions;
  const aSectionName: RawUtf8);
var
  fn: TFileName;
begin
  inherited Create; // may have been overriden
  if aWorkFolder = '' then
    fWorkFolderName := Executable.ProgramFilePath
  else
    fWorkFolderName := NormalizeDirectoryExists(aWorkFolder, true);
  if aSettingsClass = nil then
    aSettingsClass := TSynDaemonSettings;
  fSettings := aSettingsClass.Create;
  fSettings.SettingsOptions := aSettingsOptions;
  fn := aSettingsFolder;
  if fn = '' then
    fn := {$ifdef OSWINDOWS}fWorkFolderName{$else}'/etc/'{$endif};
  fn :=  NormalizeDirectoryExists(fn);
  if aSettingsName = '' then
    fn := fn + Utf8ToString(Executable.ProgramName)
  else
    fn := fn + aSettingsName;
  fSettings.LoadFromFile(fn + aSettingsExt, aSectionName);
  if fSettings.LogPath = '' then
    if aLogFolder = '' then
      fSettings.LogPath :=
        {$ifdef OSWINDOWS}fWorkFolderName{$else}GetSystemPath(spLog){$endif}
    else
      fSettings.LogPath := NormalizeDirectoryExists(aLogFolder);
  fShowExceptionWaitEnter := true; // default/legacy behavior
  AfterCreate;
end;

procedure TSynDaemon.AfterCreate;
begin
  if RunFromSynTests then
    fSettings.fLogClass := TSynLog // share the same TSynLog for all daemons
  else
    fSettings.SetLog(TSynLog); // real world logging
end;

destructor TSynDaemon.Destroy;
begin
  if fSettings <> nil then
    fSettings.SaveIfNeeded;
  Stop;
  inherited Destroy;
  FreeAndNil(fSettings);
end;

procedure TSynDaemon.Resume;
begin
  // nothing to be done by default - only called on Windows actually
end;

{$ifdef OSWINDOWS}
procedure TSynDaemon.DoStart(Sender: TService);
begin
  Start;
end;

procedure TSynDaemon.DoStop(Sender: TService);
begin
  Stop;
end;

procedure TSynDaemon.DoResume(Sender: TService);
begin
  Resume;
end;
{$endif OSWINDOWS}

function TSynDaemon.CustomCommandLineSyntax: string;
begin
  result := '';
end;

{$I-} // no error raised during write/writeln

procedure TSynDaemon.WriteCopyright;
var
  msg, name, copyright: string;
  i: integer;
begin
  msg := fSettings.ServiceDescription;
  i := Pos(' - ', msg);
  if i = 0 then
    name := msg
  else
  begin
    name := copy(msg, 1, i - 1);
    copyright := copy(msg, i + 3, 1000);
  end;
  TextColor(ccLightGreen);
  writeln(' ', name);
  writeln(StringOfChar('-', length(name) + 2));
  TextColor(ccGreen);
  if {%H-}copyright <> '' then
    writeln(' ', copyright);
  writeln;
  TextColor(ccLightGray);
end;

procedure TSynDaemon.Command(cmd: TExecuteCommandLineCmd; aAutoStart: boolean;
  const param: RawUtf8);
var
  exe: RawByteString;
  log: TSynLog;
  {$ifdef OSWINDOWS}
  service: TServiceSingle;
  ctrl: TServiceController;
  {$endif OSWINDOWS}

  procedure ShowState(state: TServiceState);
  begin
    writeln(Utf8ToConsole(fSettings.ServiceName), ' State=', ToText(state)^);
    ExitCode := ord(state); // transmit result to caller e.g. from a batch
  end;

  procedure Syntax;
  var
    spaces, custom: string;
  begin
    WriteCopyright;
    writeln('Try with one of the switches:');
    spaces := StringOfChar(' ', length(Executable.ProgramName) + 4);
    {$ifdef OSWINDOWS}
    writeln('   ', Executable.ProgramName,
            ' /console -c /verbose /help -h /version');
    writeln(spaces, '/install /uninstall /start /stop /state');
    {$else}
    writeln(' ./', Executable.ProgramName,
            ' --console -c --verbose --help -h --version');
    writeln(spaces, '--run -r --fork -f --kill -k --silentkill --state');
    {$endif OSWINDOWS}
    custom := CustomCommandLineSyntax;
    if custom <> '' then
      writeln(spaces, custom);
  end;

  function cmdText: RawUtf8;
  begin
    result := GetEnumNameTrimed(TypeInfo(TExecuteCommandLineCmd), ord(cmd));
  end;

  procedure Show(Success: boolean);
  var
    msg: RawUtf8;
    error: integer;
  begin
    WriteCopyright;
    if Success then
    begin
      msg := 'Successfully executed';
      TextColor(ccWhite);
    end
    else
    begin
      error := GetLastError;
      FormatUtf8('Error % [%] occurred with', [error, GetErrorText(error)], msg);
      TextColor(ccLightRed);
      ExitCode := 1; // notify error to caller batch
    end;
    if param = '' then
      msg := FormatUtf8('% (%) on Service ''%''',
        [msg, cmdText, fSettings.ServiceName])
    else
      msg := FormatUtf8('% [%] (%) on Service ''%''',
        [msg, param, cmdText, fSettings.ServiceName]);
    writeln(Utf8ToConsole(msg));
    TextColor(ccLightGray);
    log.Log(sllDebug, 'CommandLine: %', [msg], self);
  end;

begin
  if (self = nil) or
     (fSettings = nil) then
    exit;
  log := nil;
  try
    case cmd of
    cHelp:
      Syntax;
    cVersion:
      begin
        WriteCopyright;
        exe := StringFromFile(Executable.ProgramFileName);
        writeln(' ', Utf8ToConsole(fSettings.ServiceName),
          #13#10' Size:       ', length(exe), ' bytes (', KB(exe), ')' +
          #13#10' Build date: ', Executable.Version.BuildDateTimeString,
          #13#10' MD5:        ', Md5(exe),
          #13#10' SHA256:     ', Sha256(exe));
        writeln(' Running OS: ', OSVersionText);
        if Executable.Version.Version32 <> 0 then
          writeln(' Version:    ', Executable.Version.Detailed);
      end;
    cConsole,
    cVerbose:
      begin
        WriteCopyright;
        writeln('Launched in ', cmdText, ' mode'#10);
        TextColor(ccLightGray);
        log := fSettings.fLogClass.Add;
        if (cmd = cVerbose) and
           (log <> nil) then
        begin
          log.Family.Level := LOG_VERBOSE;
          log.Family.EchoToConsole := LOG_VERBOSE;
        end;
        try
          log.Log(sllNewRun, 'Start % /% %', [fSettings.ServiceName, cmdText,
            Executable.Version.DetailedOrVoid], self);
          fConsoleMode := true;
          Start;
          writeln('Press [Enter] or Ctrl+C to quit');
          ioresult;
          ConsoleWaitForEnterKey;
          writeln('Shutting down server');
        finally
          ioresult;
          log.Log(sllNewRun, 'Stop /%', [cmdText], self);
          Stop;
        end;
    end;
    {$ifdef OSWINDOWS}
    // implement the daemon as a Windows Service
    else if fSettings.ServiceName = '' then
      if cmd = cNone then
        Syntax
      else
      begin
        TextColor(ccLightRed);
        writeln('No ServiceName specified - please fix the settings');
      end
    else
    case cmd of
      cNone:
        if param = '' then
        begin
          // executed as a background service
          service := TServiceSingle.Create(
            fSettings.ServiceName, fSettings.ServiceDisplayName);
          try
            service.OnStart := DoStart;
            service.OnStop := DoStop;
            service.OnResume := DoResume;
            service.OnShutdown := DoStop; // sometimes, is called without Stop
            if ServiceSingleRun then
              // blocking until service shutdown
              Show(true)
            else if GetLastError = ERROR_FAILED_SERVICE_CONTROLLER_CONNECT then
              Syntax
            else
              Show(false);
          finally
            service.Free;
          end;
        end
        else
          Syntax;
      cInstall:
        with fSettings do
          Show(TServiceController.Install(
            ServiceName, ServiceDisplayName, StringToUtf8(ServiceDescription),
            aAutoStart, ServiceExecutable, ServiceDependencies) <> ssNotInstalled);
      cStart,
      cStop,
      cUninstall,
      cState:
        begin
          ctrl := TServiceController.CreateOpenService(
                    '', '', fSettings.ServiceName);
          try
            case cmd of
              cStart:
                Show(ctrl.Start([]));
              cStop:
                Show(ctrl.Stop);
              cUninstall:
                begin
                  ctrl.Stop;
                  Show(ctrl.Delete);
                end;
              cState:
                ShowState(ctrl.State);
            end;
          finally
            ctrl.Free;
          end;
        end;
    else
      Syntax;
    end;
    {$else}
    // POSIX Run/Fork background execution of the executable
    cRun,
    cFork,
    cStart: // /start = /fork
      RunUntilSigTerminated(self, {dofork=}(cmd in [cFork, cStart]), Start, Stop,
        fSettings.fLogClass.DoLog, fSettings.ServiceName);
    cKill,
    cStop,  // /stop = /kill
    cSilentKill:
      if RunUntilSigTerminatedForKill then
      begin
        if cmd <> cSilentKill then
          writeln('Forked process ',
            Executable.ProgramName, ' killed successfully');
      end
      else
        raise EDaemon.Create('No forked process found to be killed');
    cState:
      ShowState(RunUntilSigTerminatedState);
    else
      Syntax;
    {$endif OSWINDOWS}
    end;
  except
    on E: Exception do
    begin
      if cmd <> cSilentKill then
        ConsoleShowFatalException(E, fShowExceptionWaitEnter);
      ExitCode := 1; // indicates error
    end;
  end;
  if cmd <> cSilentKill then
    TextColor(ccLightGray);
  ioresult;
end;

function TSynDaemon.CurrentState: TServiceState;
begin
  if self = nil then
    result := ssErrorRetrievingState
  else
    {$ifdef OSWINDOWS}
    result := TServiceController.CurrentState(fSettings.ServiceName);
    {$else}
    result := RunUntilSigTerminatedState;
    {$endif OSWINDOWS}
end;

const
  // single char command switch (not V* S*)
  CMD_CHR: array[cHelp .. cKill] of AnsiChar = (
    'H',  // cHelp
    'I',  // cInstall
    'R',  // cRun
    'F',  // cFork
    'U',  // cUninstall
    'C',  // cConsole
    'K'); // cKill

function ParseCmd(p: PUtf8Char): TExecuteCommandLineCmd;
var
  ch: AnsiChar;
begin
  ch := NormToUpper[p^];
  for result := low(CMD_CHR) to high(CMD_CHR) do // parse H* I* R* F* U* C* K*
    if CMD_CHR[result] = ch then
      exit;
  byte(result) := ord(cVersion) + // parse V* and S* commands
    IdemPCharArray(p, ['VERS',      // cVersion
                       'VERB',      // cVerbose
                       'START',     // cStart
                       'STOP',      // cStop
                       'STAT',      // cState
                       'SILENTK']); // cSilentKill
end;

function TSynDaemon.CustomParseCmd(P: PUtf8Char): boolean;
begin
  // should return true if the command has been identified and processed
  result := false; // unknown command -> show syntax
end;

procedure TSynDaemon.CommandLine(aAutoStart: boolean);
var
  cmd: TExecuteCommandLineCmd;
  p: PUtf8Char;
  param: RawUtf8;
begin
  param := TrimU(StringToUtf8(paramstr(1)));
  cmd := cNone;
  if (param <> '') and
     (param[1] in ['/', '-']) then
  begin
    p := @param[2];
    if p^ = '-' then
      // allow e.g. --fork switch (idem to /f -f /fork -fork)
      inc(p);
    if CustomParseCmd(p) then
    begin
      TextColor(ccLightGray); // like Command()
      exit; // command has been identified and processed in overriden method
    end;
    cmd := ParseCmd(p);
  end;
  Command(cmd, aAutoStart, param);
end;


end.

