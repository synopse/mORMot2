/// Command Line "mORMot GET" (mget) Tool
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program mget;

{
  *****************************************************************************

  The mORMot GET (mget) command-line tool retrieve files using HTTP or HTTPS
   - Similar to the homonymous GNU WGet tool, but with some unique features;
   - Can resume aborted downloads, using `RANGE` headers;
   - Can compute and verify the hash of the downloaded content;
   - Can brodcast and download from a local network peer cache.

  *****************************************************************************

  For PeerCache to work, please open port 8099 for TCP+UDP on the firewall:
    Debian:   sudo ufw allow from 192.168.0.0/24 to any port 8099
    Windows:  netsh firewall add portopening all 8099 peerCache
      (see the actual command supplied by "mget /help" response)
}

{$I ..\..\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I ..\..\mormot.uses.inc}
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.perf,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.zip,
  mormot.net.sock,
  mormot.net.client,
  mormot.net.server,
  mormot.crypt.secure,
  mormot.lib.openssl11,
  mormot.tools.mget;


type
  TGetParameters = (
    gpHelp,
    gpFailed,
    gpWithUrl,
    gpPromptMode);

function GetParameters(p: TMGetProcess; out url: RawUtf8): TGetParameters;
var
  mac: TMacAddress;
  logfolder: TFileName;
  dest: RawUtf8;
  port: integer;
begin
  // some good enough general default values
  p.CacheFolder := MakePath([Executable.ProgramFilePath, 'cache'], true);
  p.tcpTimeoutSec := 10;
  logfolder := StringToUtf8(Executable.ProgramFilePath);
  p.peerSecret := 'secret';
  p.PeerSettings.Options := [pcoVerboseLog, pcoTryLastPeer];
  p.PeerSettings.CacheTempPath := p.CacheFolder + 'temp';
  p.PeerSettings.CachePermPath := p.CacheFolder + 'perm';
  p.PeerSettings.LimitMBPerSec := 0; // no limit by default
  if GetMainMacAddress(mac, [mafLocalOnly, mafRequireBroadcast]) then
    p.PeerSettings.InterfaceName := mac.IP; // default interface by IP (easy)
  // define main processing switches
  with Executable.Command do
  begin
    CaseSensitiveNames := true;
    ExeDescription := FormatUtf8('mget %: retrieve files - and more' +
      '%proudly using mORMot 2 - synopse.info',
      [SYNOPSE_FRAMEWORK_VERSION, LineFeed]);
    if Arg(0, '[hash@]#http://uri resource address to retrieve') then
      url := Args[0];
    result := gpWithUrl;
    if Option(['P', 'prompt'],
        'run in prompt mode (end on void input)') then
      result := gpPromptMode;
    dest := MakePath([GetCurrentDir, ExtractResourceName(url{%H-})]);
    p.DestFile := ParamS(['o', 'output'],
       'destination #filename or existing foldername', dest);
    p.Silent := Option(['s', 'silent'],
       'generate no console output');
    p.NoResume := Option(['n', 'noresume'],
       'disable auto-resume of interrupted partial download');
    p.Cache := Option(['c', 'cache'],
       'enable local Cache in --cachePath');
    p.Peer := Option(['p', 'peer'],
       'enable peer Cache process - see --peer* params');
    p.TlsCertFile := ParamS(['t', 'tlsCert'],
       'optional client Certificate #filename');
    logfolder := ParamS(['logFolder'],
       '#folder to be used for --log output', logfolder);
    p.CacheFolder := ParamS(['cachePath'],
       '#folder to be used for local (not peer) --cache', p.CacheFolder);
    p.TlsIgnoreErrors  := Option(['w', 'weakTls'],
       'ignore TLS certificate errors');
    if Option(['l', 'log'],
       'enable logging in --logFolder') then
      p.Log := TSynLog;
    if Option('debug', 'raw debugging on the console') then
    begin
      p.Log := TSynLog; // force logging even if -l was not specified
      p.Log.Family.EchoToConsole := LOG_VERBOSE; // - [sllTrace];
    end;
    if Option(['?', 'help'], 'display this message') then
      result := gpHelp
    else if (result = gpWithUrl) and
            (Url = '') then
      result := gpFailed;
  end;
  // add Main and PeerCache params after all main settings using RTTI
  SetObjectFromExecutableCommandLine(p, '', ' for main process');
  SetObjectFromExecutableCommandLine(p.PeerSettings, 'peer', ' for peer Cache');
  // validate whole set of arguments
  case result of
    gpHelp:
      begin
        p.ToConsole('%', [Executable.Command.FullDescription]);
        port := p.PeerSettings.Port;
        p.ToConsole('For peer Cache to work, please open TCP+UDP port %.', [port]);
        {$ifdef OSLINUX}
        p.ToConsole('    e.g. sudo ufw allow from % to any port %',
          [IP4Subnet(mac.IP, mac.NetMask), port]);
        {$endif OSLINUX}
        {$ifdef OSWINDOWS}
        if OSVersion >= wVista then
          p.ToConsole('  netsh advfirewall firewall add rule name="' +
            'peerCache % tcp" dir=in protocol=tcp localport=% action=allow'#13#10 +
            '  netsh advfirewall firewall add rule name="peerCache % udp" dir=in' +
            ' protocol=udp localport=% action=allow', [port, port, port, port])
        else
          p.ToConsole('  netsh firewall add portopening all % peerCache', [port]);
        {$endif OSWINDOWS}
      end;
    gpFailed:
      with Executable.Command do
        p.ToConsole('%', [FullDescription('', '', LineFeed + 'mget ' +
          SwitchAsText('help') + ' to display full usage description')]);
  else if Executable.Command.ConsoleWriteUnknown then
    result := gpFailed;
  end;
  if result = gpFailed then
    exit;
  // setting the needed logging information
  if p.Log <> nil then
    with p.Log.Family do
    begin
      Level := LOG_VERBOSE;
      if logfolder <> '' then
      begin
        PerThreadLog := ptIdentifiedInOneFile;
        FileExistsAction := acAppend;
        AutoFlushTimeOut := 1;
        RotateFileCount := 2;
        RotateFileSizeKB := 2 shl 10;
        LogCompressAlgo := AlgoGZFast; // rotate as mget.1.gz every 2MB
        DestinationPath := EnsureDirectoryExists(logfolder);
      end;
    end;
end;

var
  p: TMGetProcess;
  url: RawUtf8;
begin
  try
    // initialize OpenSSL if needed
    {$ifdef USE_OPENSSL}
    OpenSslInitialize;
    {$endif USE_OPENSSL}
    // is executed as a service/daemon or as a command line tool
    p := TMGetProcess.Create;
    try
      case GetParameters(p, url) of
        gpFailed:
          ExitCode := 1; // error
        gpWithUrl:
          p.Execute(url);
        gpPromptMode:
          begin
            p.Start; // launch background THttpPeerCache e.g.
            repeat
              if url = '' then
              begin
                p.hashValue := '';
                p.DestFile := GetCurrentDir;
                p.ToConsole(
                  'Enter a [hash@]http://uri value (leave void to quit)', []);
                readln(url);
              end;
              if (url = '') or
                 (url = 'exit') then
                break;
              p.Execute(url);
              url := '';
            until false;
          end;
      end;
    finally
      p.Free;
    end;
  except
    on E: Exception do
    begin
      ConsoleShowFatalException(E, {waitforenterkey=}false);
      ExitCode := 2; // interrupted
    end;
  end;
end.

