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
  mormot.net.client,
  mormot.net.server,
  mormot.tools.mget;

procedure GetParameters;
begin
  with Executable.Command do
  begin
    ExeDescription := 'Retrieve files using HTTP(S)';
    verbose := Option(['v', 'verbose'], 'generate verbose output');
    Get(['t', 'threads'], threads, '#number of threads to run', 5);
  end;
end;

begin
  try
    // is executed as a service/daemon or as a command line tool
    GetParameters;
  except
    on E: Exception do
      ConsoleShowFatalException(E, {waitforenterkey=}false);
  end;
end.

