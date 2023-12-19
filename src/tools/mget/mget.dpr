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
  mormot.tools.mget;


function GetParameters(p: TMGetProcess; out url: RawUtf8): boolean;
var
  mac: TMacAddress;
  logfolder: TFileName;
  dest: RawUtf8;
begin
  // some good enough general default values
  p.CacheFolder := MakePath([Executable.ProgramFilePath, 'cache'], true);
  p.tcpTimeoutSec := 10;
  p.hashAlgo := hfSHA256;
  logfolder := StringToUtf8(Executable.ProgramFilePath + 'log');
  // use some good enough settings for default --peer process on command line
  p.peerSecret := 'secret';
  p.PeerSettings.CacheTempPath := p.CacheFolder + 'temp';
  p.PeerSettings.CachePermPath := p.CacheFolder + 'perm';
  if GetMainMacAddress(mac, [mafLocalOnly, mafRequireBroadcast]) then
    p.PeerSettings.InterfaceName := mac.IP; // default interface by IP (easy)
  // define main processing switches
  Executable.Command.ExeDescription := 'Retrieve files using HTTP(S) and more';
  if Executable.Command.Arg(0, '#http://uri resource address to retrieve') then
    url := Executable.Command.Args[0];
  dest := ExtractResourceName(url);
  p.DestFile := Utf8ToString(Executable.Command.Param(['o', 'output'],
     '#filename to be used as output', dest));
  p.Verbose := Executable.Command.Option(['v', 'verbose'],
     'generate verbose output');
  p.NoResume := Executable.Command.Option(['n', 'noresume'],
     'disable auto-resume of interrupted partial download');
  p.Hash := Executable.Command.Option(['h', 'hash'],
     'enable content hashing using --hashAlgo');
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
  begin
    p.Log.Family.Level := LOG_VERBOSE;
    if logfolder <> '' then
      p.Log.Family.DestinationPath := logfolder;
  end;
  // add Main and PeerCache params after all main settings using RTTI
  SetObjectFromExecutableCommandLine(p, '', ' for main process');
  SetObjectFromExecutableCommandLine(p.PeerSettings, 'peer', ' for peer Cache');
  // validate whole set of arguments
  result := false;
  if Executable.Command.Option(['?', 'help'], 'display this message') or
     (Url = '') then
    ConsoleWrite(Executable.Command.FullDescription)
  else if not Executable.Command.ConsoleWriteUnknown then
    result := true;
end;

var
  p: TMGetProcess;
  url: RawUtf8;
begin
  try
    // is executed as a service/daemon or as a command line tool
    p := TMGetProcess.Create;
    try
      if GetParameters(p, url) then
        ConsoleWrite('Loaded %', [p.Execute(url)])
      else
        ExitCode := 1; // error
    finally
      p.Free;
    end;
  except
    on E: Exception do
      ConsoleShowFatalException(E, {waitforenterkey=}false);
  end;
end.

