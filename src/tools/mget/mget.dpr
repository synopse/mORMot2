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
  mormot.tools.mget;

function GetParameters(p: TMGetProcess): boolean;
var
  mac: TMacAddress;
begin
  // use some good enough settings for default --peer process on command line
  p.PeerSettings.CacheTempPath := Executable.ProgramFilePath + 'cache-temp';
  p.PeerSettings.CachePermPath := Executable.ProgramFilePath + 'cache-perm';
  if GetMainMacAddress(mac, [mafLocalOnly, mafRequireBroadcast]) then
    p.PeerSettings.InterfaceName := mac.IP; // default interface by IP (easy)
  // define main processing switches
    Executable.Command.ExeDescription := 'Retrieve files using HTTP(S)';
    p.Verbose := Executable.Command.Option(['v', 'verbose'],
      'generate verbose output');
    p.Peer := Executable.Command.Option(['p', 'peer'],
      'enable PeerCache process - see --peer* options');
    //Get(['t', 'threads'], threads, '#number of threads to run', 5);
  // add PeerCache params after all main settings
  SetObjectFromExecutableCommandLine(
    p.PeerSettings, 'peer', ' for PeerCache');
  // main arguments
  if Executable.Command.Arg(0, '#http://uri resource address to retrieve') then
    p.Url := Executable.Command.Args[0];
  result := false;
  if Executable.Command.Option(['?', 'help'], 'display this message') or
    (p.Url = '') then
    ConsoleWrite(Executable.Command.FullDescription)
  else if not Executable.Command.ConsoleWriteUnknown then
    result := true;
end;

var
  p: TMGetProcess;
begin
  try
    // is executed as a service/daemon or as a command line tool
    p := TMGetProcess.Create;
    try
      if GetParameters(p) then
      begin

      end;
    finally
      p.Free;
    end;
  except
    on E: Exception do
      ConsoleShowFatalException(E, {waitforenterkey=}false);
  end;
end.

