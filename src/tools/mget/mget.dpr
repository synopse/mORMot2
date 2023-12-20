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

  For PeerCache to work, please open port 8089 for TCP+UDP on the computer:
    sudo ufw allow from 192.168.0.0/24 to any port 8089
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
  mormot.net.sock,
  mormot.net.client,
  mormot.net.server,
  mormot.crypt.secure,
  mormot.lib.openssl11,
  mormot.tools.mget;


type
  TGetParameters = (gpHelp, gpFailed, gpWithUrl, gpPromptMode);

function GetParameters(p: TMGetProcess; out url: RawUtf8): TGetParameters;
var
  mac: TMacAddress;
  logfolder: TFileName;
  dest: RawUtf8;
begin
  // some good enough general default values
  p.CacheFolder := MakePath([Executable.ProgramFilePath, 'cache'], true);
  p.tcpTimeoutSec := 10;
  logfolder := StringToUtf8(Executable.ProgramFilePath);
  p.peerSecret := 'secret';
  p.PeerSettings.CacheTempPath := p.CacheFolder + 'temp';
  p.PeerSettings.CachePermPath := p.CacheFolder + 'perm';
  if GetMainMacAddress(mac, [mafLocalOnly, mafRequireBroadcast]) then
    p.PeerSettings.InterfaceName := mac.IP; // default interface by IP (easy)
  // define main processing switches
  Executable.Command.ExeDescription := 'Retrieve files using HTTP(S) and more';
  if Executable.Command.Arg(0, '[hash@]#http://uri resource address to retrieve') then
    url := Executable.Command.Args[0];
  result := gpWithUrl;
  if Executable.Command.Option(['P', 'prompt'],
      'run in prompt mode (end on void input)') then
    result := gpPromptMode;
  dest := ExtractResourceName(url);
  p.DestFile := Utf8ToString(Executable.Command.Param(['o', 'output'],
     '#filename to be used as output', dest));
  p.Silent := Executable.Command.Option(['s', 'silent'],
     'generate no console output');
  p.NoResume := Executable.Command.Option(['n', 'noresume'],
     'disable auto-resume of interrupted partial download');
  p.Cache := Executable.Command.Option(['c', 'cache'],
     'enable local Cache in --cachePath');
  p.Peer := Executable.Command.Option(['p', 'peer'],
     'enable peer Cache process - see --peer* params');
  p.TlsCertFile := Utf8ToString(Executable.Command.Param(['t', 'tlsCert'],
     'optional client Certificate #filename'));
  logfolder := Utf8ToString(Executable.Command.Param(['logFolder'],
     '#folder to be used for --log output', logfolder));
  p.CacheFolder := Utf8ToString(Executable.Command.Param(['cachePath'],
     '#folder to be used for local (not peer) --cache',
     StringToUtf8(p.CacheFolder)));
  p.TlsIgnoreErrors  := Executable.Command.Option(['w', 'weakTls'],
     'ignore TLS certificate errors');
  if Executable.Command.Option(['l', 'log'],
     'enable logging in --logFolder') then
    p.Log := TSynLog;
  // setting the needed logging information
  if p.Log <> nil then
  with p.Log.Family do
    begin
      Level := LOG_VERBOSE;
      if logfolder <> '' then
      begin
        PerThreadLog := ptIdentifiedInOneFile;
        FileExistsAction := acAppend;
        RotateFileCount := 2;
        RotateFileSizeKB := 2 shl 10;
        DestinationPath := EnsureDirectoryExists(logfolder);
      end;
    end;
  // add Main and PeerCache params after all main settings using RTTI
  SetObjectFromExecutableCommandLine(p, '', ' for main process');
  SetObjectFromExecutableCommandLine(p.PeerSettings, 'peer', ' for peer Cache');
  // validate whole set of arguments
  if Executable.Command.Option(['?', 'help'], 'display this message') then
    result := gpHelp
  else if (result = gpWithUrl) and
          (Url = '') then
    result := gpFailed;
  if result in [gpHelp, gpFailed] then
    p.ToConsole('%', [Executable.Command.FullDescription])
  else if Executable.Command.ConsoleWriteUnknown then
    result := gpFailed;
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
            p.DestFile := '';
            repeat
              p.hashValue := '';
              if url = '' then
                readln(url);
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

