/// High-Level Angelize Logic to Manage Multiple Daemons
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.app.agl;

{
  *****************************************************************************

   Daemon (e.g. Windows Service) Stand-Alone Background Executable Support
    - TSynAngelize App-As-Service Launcher

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
  mormot.net.client,
  mormot.app.console,
  mormot.app.daemon;


{ ************ TSynAngelize App-As-Service Launcher }

type
  /// exception class raised by TSynAngelize
  ESynAngelize = class(ESynException);

  /// define how TSynAngelize handle a sub-process execution
  // - sasAuto will start it with the main agl daemon/service
  TSynAngelizeStarter = (
    sasAuto);
  /// define TSynAngelize sub-process handling
  TSynAngelizeStarters = set of TSynAngelizeStarter;

  /// define one TSynAngelizeService action
  // - depending on the context, as "method:param" pair
  TSynAngelizeAction = type RawUtf8;
  /// define one or several TSynAngelizeService action(s)
  // - stored as a JSON array in the settings
  TSynAngelizeActions = array of TSynAngelizeAction;

  /// one sub-process definition as recognized by TSynAngelize
  // - TSynAngelizeAction properties will expand %abc% place-holders when needed
  // - specifies how to start, stop and watch a given sub-process
  // - main idea is to let sub-processes remain simple process, not Operating
  // System daemons/services, for a cross-platform and user-friendly experience
  TSynAngelizeService = class(TSynJsonFileSettings)
  protected
    fName: RawUtf8;
    fDescription: RawUtf8;
    fStart, fStop, fWatch: TSynAngelizeActions;
    fState: TServiceState;
    fStarter: TSynAngelizeStarters;
    fLevel, fWatchDelaySec: integer;
  public
    /// initialize and set the default settings
    constructor Create; override;
    /// the current state of the service, as retrieved during the Watch phase
    property State: TServiceState
      read fState;
  published
    /// computer-friendly case-insensitive identifier of this sub-service
    // - as used internally by TSynAngelize to identify this instance
    // - should be a short, if possible ASCII and pascal-compatible, identifier
    // - default is the Executable file name without its path and extension
    property Name: RawUtf8
      read fName write fName;
    /// human-friendly Unicode text which could be displayed on Web or Console UI
    // - in addition to the Name short identifier
    // - default is the Executable file name without its path and extension
    property Description: RawUtf8
      read fDescription write fDescription;
    /// sub-services are started from their increasing Level
    // - allow to define dependencies between sub-services
    // - it could be a good idea to define Level by increments of 10, so that
    // intermediate services may be inserted easily in the rings
    // - will disable the entry if set to 0 or negative value
    property Level: integer
      read fLevel write fLevel;
    /// the action(s) executed to start the sub-process
    // - will be executed in-order
    // - could include %abc% place holders
    // - could be "run:/path/to/executable" for not waiting to its ending
    // - could be "wait:/path/to/executable" for waiting for its ending, to be
    // terminated later on with some "Stop": [ "xxx:/path/to/executable/" ]
    // - could be "http://127.0.0.1:8080/publish/on" for a local HTTP request
    // - or "sleep:1000" e.g. for 1 second wait between steps
    // - on Windows, "service:ServiceName" would call SERVICE_CONTROL_START
    property Start: TSynAngelizeActions
      read fStart write fStart;
    /// define how to start or restart the sub-process
    property Starter: TSynAngelizeStarters
      read fStarter write fStarter;
    /// the action(s) executed to stop the sub-process
    // - will be executed in-order
    // - could include %abc% place holders
    // - could be "run:/path/to/executable" for not waiting to its ending
    // - could be "wait:/path/to/executable" for waiting for its ending
    // - could be "http://127.0.0.1:8080/publish/off" for a local HTTP request
    // - on Windows, "ctrlc:/path/to/executable" send a Ctrl+C after "Start": [
    // ... "wait:/path/to/executable" ... ]
    // - on Windows, "wmquit:/path/to/executable" send a WM_QUIT message after
    // "Start": [ ... "wait:/path/to/executable" ... ]
    // - on Windows, 'terminate:/path/to/executable' call the TerminateProcess()
    // API after "Start": [ ... "wait:/path/to/executable" ... ]
    // - on Windows, 'service:ServiceName' would call SERVICE_CONTROL_STOP
    // - on POSIX, "sigint:/path/to/executable" send a SIGINT message after
    // "Start": [ ... "wait:/path/to/executable" ... ]
    // - on POSIX, "sigterm:/path/to/executable" send a SIGTERM message after
    // "Start": [ ... "wait:/path/to/executable" ... ]
    // - on POSIX, "sigkill:/path/to/executable" send a SIGKILL message after
    // "Start": [ ... "wait:/path/to/executable" ... ]
    // - or "sleep:1000" e.g. for 1 second wait between steps
    // - TSynAngelizeHttp could check for an URI instead, starting with 'http:'
    // - several executables or URIs could be specified as CSV
    property Stop: TSynAngelizeActions
      read fStop write fStop;
    // - will be executed in-order every at WatchDelaySec pace
    // - could include %abc% place holders
    // - could be "run:/path/to/executable" for not waiting to its ending
    // - could be "wait:/path/to/executable" for waiting for its ending, to be
    // - could be "http://127.0.0.1:8080/publish/watchme" for a local HTTP
    // request returning 200 on status success
    // - on Windows, 'service:ServiceName' would call SERVICE_CONTROL_INTERROGATE
    property Watch: TSynAngelizeActions
      read fWatch write fWatch;
    /// how many seconds should we wait between each Watch method step
    // - default is 60 seconds
    property WatchDelaySec: integer
      read fWatchDelaySec write fWatchDelaySec;
  end;

  TSynAngelizeServiceClass = class of TSynAngelizeService;

  /// define the main TSynAngelize daemon/service behavior
  TSynAngelizeSettings = class(TSynDaemonSettings)
  protected
    fFolder, fExt, fStateFile: TFileName;
    fOptions: TSynJsonFileSettingsOptions;
    fHttpTimeoutSec: integer;
  public
    /// set the default values
    constructor Create; override;
  published
    /// where the TSynAngelizeService settings are stored
    // - default is the 'services' sub-folder of the TSynAngelizeSettings
    property Folder: TFileName
      read fFolder write fFolder;
    /// the extension of the TSynAngelizeService settings files
    // - default is '.service'
    property Ext: TFileName
      read fExt write fExt;
    /// how TSynAngelizeService settings files are serialized
    property Options: TSynJsonFileSettingsOptions
      read fOptions write fOptions;
    /// timeout in seconds for "http://....." local HTTP requests
    // - default is 1
    property HttpTimeoutSec: integer
      read fHttpTimeoutSec write fHttpTimeoutSec;
    /// the local file used to communicate the current sub-process files
    // from running daemon to the state
    // - default is a TemporaryFileName instance
    property StateFile: TFileName
      read fStateFile write fStateFile;
  end;

  /// used to serialize the current state of the services in the executable
  TSynAngelizeState = packed record
    Service: array of record
      Name: RawUtf8;
      State: TServiceState;
    end;
  end;

  /// can run a set of executables as sub-process(es) from *.service definitions
  // - agl ("angelize") is an alternative to NSSM / SRVANY / WINSW
  // - at OS level, there will be a single agl daemon or service
  // - this main agl instance will manage one or several executables as
  // sub-process(es), and act as both Launcher and WatchDog
  // - in addition to TSynDaemon command line switches, you could use /list
  // to retrieve the state of services
  TSynAngelize = class(TSynDaemon)
  protected
    fAdditionalParams: TFileName;
    fSettingsClass: TSynAngelizeServiceClass;
    fExpandLevel: byte;
    fLastUpdateServicesFromSettingsFolder: cardinal;
    fSectionName: RawUtf8;
    fService: array of TSynAngelizeService;
    fLevels: TIntegerDynArray;
    fLastGetServicesStateFile: RawByteString;
    // TSynDaemon command line methods
    function CustomParseCmd(P: PUtf8Char): boolean; override;
    function CustomCommandLineSyntax: string; override;
    function LoadServicesState(out state: TSynAngelizeState): boolean;
    procedure ListServices;
    procedure StartServices;
    procedure StopServices;
    procedure StartWatching;
    procedure StopWatching;
    // sub-service support
    function FindService(const ServiceName: RawUtf8): PtrInt;
    function ComputeServicesStateFile: integer;
    procedure DoExpand(aService: TSynAngelizeService; const aInput: TSynAngelizeAction;
      out aOutput: TSynAngelizeAction);
  public
    /// initialize the main daemon/server redirection instance
    // - main TSynAngelizeSettings is loaded
    constructor Create(aSettingsClass: TSynAngelizeServiceClass = nil;
      const aSectionName: RawUtf8 = 'Main'; aLog: TSynLogClass = nil); reintroduce;
    /// finalize the stored information
    destructor Destroy; override;
    /// read and parse all *.service definitions from Settings.Folder
    // - as called by Start overriden method
    // - may be called before head to validate the execution settings
    // - raise ESynAngelize on invalid settings or dubious StateFile
    function LoadServicesFromSettingsFolder: integer;
    /// compute a path/action, replacing all %abc% place holders with their values
    // - %agl.base% is the location of the agl executable
    // - %agl.settings% is the location of the *.service files
    // - %agl.params% are the additional parameters supplied to the command line
    // - %agl.toto% is the "toto": property value in the .service settings,
    // e.g. %agl.servicename% is the service name
    // - TSystemPath values are available as %CommonData%, %UserData%,
    // %CommonDocuments%, %UserDocuments%, %TempFolder% and %Log%
    function Expand(aService: TSynAngelizeService;
      const aAction: TSynAngelizeAction): TSynAngelizeAction;
    /// overriden for proper sub-process starting
    procedure Start; override;
    /// overriden for proper sub-process stoping
    // - should do nothing if the daemon was already stopped
    procedure Stop; override;
  end;


implementation


{ ************ TSynAngelize App-As-Service Launcher }

{ TSynAngelizeService }

constructor TSynAngelizeService.Create;
begin
  inherited Create;
  fWatchDelaySec := 60;
end;


{ TSynAngelizeSettings }

constructor TSynAngelizeSettings.Create;
begin
  inherited Create;
  fHttpTimeoutSec := 1;
  fFolder := Executable.ProgramFilePath + 'services';
  fExt := '.service';
  fStateFile := TemporaryFileName;
end;


{ TSynAngelize }

constructor TSynAngelize.Create(aSettingsClass: TSynAngelizeServiceClass;
  const aSectionName: RawUtf8; aLog: TSynLogClass);
begin
  if aSettingsClass = nil then
    aSettingsClass := TSynAngelizeService;
  fSettingsClass := aSettingsClass;
  inherited Create(TSynAngelizeSettings, Executable.ProgramFilePath,
    Executable.ProgramFilePath,  Executable.ProgramFilePath + 'log');
end;

destructor TSynAngelize.Destroy;
begin
  inherited Destroy;
  ObjArrayClear(fService);
  fSettings.Free;
end;

// TSynDaemon command line methods

const
  AGL_CMD: array[0..1] of PAnsiChar = (
    'LIST',
    nil);

function TSynAngelize.CustomParseCmd(P: PUtf8Char): boolean;
begin
  result := true; // the command has been identified and processed
  case IdemPPChar(P, @AGL_CMD) of
    0:
      ListServices;
  else
    result := false; // display syntax
  end;
end;

function TSynAngelize.CustomCommandLineSyntax: string;
begin
  {$ifdef OSWINDOWS}
  result := '/list';
  {$else}
  result := '--list';
  {$endif OSWINDOWS}
end;

// sub-service support

function TSynAngelize.FindService(const ServiceName: RawUtf8): PtrInt;
begin
  if ServiceName <> '' then
    for result := 0 to high(fService) do
      if IdemPropNameU(fService[result].Name, ServiceName) then
        exit;
  result := -1;
end;

const
  _STATEMAGIC = $5131e3a6;

function SortByLevel(const A, B): integer; // to display by increasing Level
begin
  result := TSynAngelizeService(A).Level - TSynAngelizeService(B).Level;
end;

function TSynAngelize.LoadServicesFromSettingsFolder: integer;
var
  bin: RawByteString;
  fn: TFileName;
  r: TSearchRec;
  s: TSynAngelizeService;
  sas: TSynAngelizeSettings;
  i: PtrInt;
begin
  ObjArrayClear(fService);
  Finalize(fLevels);
  sas := fSettings as TSynAngelizeSettings;
  // remove any previous local state file
  bin := StringFromFile(sas.StateFile);
  if (bin = '') or
     (PCardinal(bin)^ <> _STATEMAGIC) then
  begin
    // this existing file is clearly invalid: store a new safe one in settings
    sas.StateFile := TemporaryFileName;
    // avoid deleting of a non valid file (may be used by malicious tools)
    raise ESynAngelize.CreateUtf8(
      'Invalid StateFile=% content', [sas.StateFile]);
  end;
  DeleteFile(sas.StateFile);
  // browse folder for settings files and generates fService[]
  fn := sas.Folder + '*' + sas.Ext;
  if FindFirst(fn, faAnyFile - faDirectory, r) = 0 then
  begin
    repeat
      if SearchRecValidFile(r) then
      begin
        s := fSettingsClass.Create;
        fn := sas.Folder + r.Name;
        if s.LoadFromFile(fn) and
           (s.Name <> '') and
           (s.Start <> nil) and
           (s.Stop <> nil) and
           (s.Level > 0) then
        begin
          i := FindService(s.Name);
          if i >= 0 then
            raise ESynAngelize.CreateUtf8(
              'GetServices: duplicated % name in % and %',
              [s.Name, s.FileName, fService[i].FileName]);
          // seems like a valid .service file
          ObjArrayAdd(fService, s);
          AddSortedInteger(fLevels, s.Level);
          s := nil; // don't Free - will be owned by fService[]
        end
        else
          fSettings.LogClass.Add.Log(
            sllWarning, 'GetServices: invalid % content', [r.Name], self);
        s.Free;
      end;
    until FindNext(r) <> 0;
    FindClose(r);
  end;
  ObjArraySort(fService, SortByLevel);
  result := length(fService);
end;

function TSynAngelize.ComputeServicesStateFile: integer;
var
  state: TSynAngelizeState;
  bin: RawByteString;
  i: PtrInt;
begin
  result := length(fService);
  SetLength(state.Service, result);
  for i := 0 to result - 1 do
  begin
    state.Service[i].Name := fService[i].Name;
    state.Service[i].State := fService[i].State;
  end;
  bin := 'xxxx' + RecordSave(state, TypeInfo(TSynAngelizeState));
  PCardinal(bin)^ := _STATEMAGIC;
  if bin <> fLastGetServicesStateFile then
  begin
    FileFromString(bin, TSynAngelizeSettings(fSettings).StateFile);
    fLastGetServicesStateFile := bin;
  end;
end;

function TSynAngelize.Expand(aService: TSynAngelizeService;
  const aAction: TSynAngelizeAction): TSynAngelizeAction;
begin
  fExpandLevel := 0;
  DoExpand(aService, aAction, result); // internal recursive method
end;

procedure TSynAngelize.DoExpand(aService: TSynAngelizeService;
  const aInput: TSynAngelizeAction; out aOutput: TSynAngelizeAction);
var
  o, i, j: PtrInt;
  p: PRttiCustomProp;
  id, v: RawUtf8;
begin
  aOutput := aInput;
  o := 1;
  repeat
    i := PosEx('%', aOutput, o);
    if i = 0 then
      exit;
    j := PosEx('%', aOutput, i + 1);
    if j = 0 then
      exit;
    dec(j, i + 1); // j = length abc
    if j = 0 then
    begin
      delete(aOutput, i, 1); // %% -> %
      o := i + 1;
      continue;
    end;
    id := copy(aOutput, i + 1, j);
    delete(aOutput, i, j + 2);
    o := i;
    if IdemPChar(pointer(id), 'AGL.') then
    begin
      delete(id, 1, 3);
      case FindPropName(['base',
                         'settings',
                         'params'], id) of
        0: // %agl.base% is the location of the agl executable
          StringToUtf8(Executable.ProgramFilePath, v);
        1: // %agl.settings% is the location of the *.service files
          StringToUtf8(TSynAngelizeSettings(fSettings).Folder, v);
        2: // %agl.params% are the additional parameters supplied to command line
          StringToUtf8(fAdditionalParams, v);
      else
        begin
          // %agl.toto% is the "toto": property value in the .service settings
          if aService = nil then
            p := nil
          else
            p := Rtti.RegisterClass(aService).Props.Find(id);
          if p = nil then
            raise ESynAngelize.CreateUtf8(
              'Expand: unknown %agl.%%', ['%', id, '%']);
          if fExpandLevel = 50 then
            raise ESynAngelize.CreateUtf8(
              'Expand infinite recursion for agl.%', [id]);
          inc(fExpandLevel); // to detect and avoid stack overflow error
          DoExpand(aService, p.Prop.GetAsString(aService), TSynAngelizeAction(v));
          dec(fExpandLevel);
        end;
      end;
    end
    else
    begin
      i := GetEnumNameValue(TypeInfo(TSystemPath), id, true);
      if i < 0 then
        continue;
      StringToUtf8(GetSystemPath(TSystemPath(i)), v);
    end;
    if v = '' then
      continue;
    insert(v, aOutput, o);
    inc(o, length(v));
    v := '';
  until false;
end;

procedure TSynAngelize.Start;
begin
  // should raise ESynAngelize on any issue, or let background work begin
  if fService = nil then
    LoadServicesFromSettingsFolder;
  StartServices;
  StartWatching;
end;

procedure TSynAngelize.Stop;
begin
  StopWatching;
  StopServices;
end;

function TSynAngelize.LoadServicesState(out state: TSynAngelizeState): boolean;
var
  bin: RawByteString;
begin
  result := false;
  bin := StringFromFile(TSynAngelizeSettings(fSettings).StateFile);
  if (bin = '') or
     (PCardinal(bin)^ <> _STATEMAGIC) then
    exit;
  delete(bin, 1, 4);
  result := RecordLoad(state, bin, TypeInfo(TSynAngelizeState));
end;

const
  _STATECOLOR: array[TServiceState] of TConsoleColor = (
    ccBlue,       // NotInstalled
    ccLightRed,   // Stopped
    ccGreen,      // Starting
    ccRed,        // Stopping
    ccLightGreen, // Running
    ccGreen,      // Resuming
    ccBrown,      // Pausing
    ccWhite,      // Paused
    ccMagenta);   // ErrorRetrievingState

procedure TSynAngelize.ListServices;
var
  ss: TServiceState;
  state: TSynAngelizeState;
  i: PtrInt;
begin
  ss := CurrentState;
  if ss <> ssRunning then
    ConsoleWrite('Main service state is %', [ToText(ss)^], ccMagenta)
  else if LoadServicesState(state) and
          ({%H-}state.Service <> nil) then
    for i := 0 to high(state.Service) do
      with state.Service[i] do
        ConsoleWrite('% %', [Name, ToText(State)^], _STATECOLOR[State])
  else
    ConsoleWrite('Unknown service state', ccMagenta)
end;

procedure TSynAngelize.StartServices;
begin

end;

procedure TSynAngelize.StopServices;
begin

  DeleteFile(TSynAngelizeSettings(fSettings).StateFile);
end;

procedure TSynAngelize.StartWatching;
begin

end;

procedure TSynAngelize.StopWatching;
begin

end;


end.

