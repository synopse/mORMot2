/// Command Line Angelize Tool
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program agl;

{
  *****************************************************************************

  Angelize (agl) is able to run one or several executables as daemon/services
  - Implemented as a main standard OS service or daemon
  - Launches and stops sub-processes defined in JSON setting files
  - A WatchDog can check the availibility of a service on regular basis
  - Can redirect the console output, restart on problem, notify issues
  - Command line switches are available for status listing or main actions

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
  mormot.core.base         in '..\..\core\mormot.core.base.pas',
  mormot.core.os           in '..\..\core\mormot.core.os.pas',
  mormot.core.unicode      in '..\..\core\mormot.core.unicode.pas',
  mormot.core.text         in '..\..\core\mormot.core.text.pas',
  mormot.core.rtti         in '..\..\core\mormot.core.rtti.pas',
  mormot.core.datetime     in '..\..\core\mormot.core.datetime.pas',
  mormot.core.perf         in '..\..\core\mormot.core.perf.pas',
  mormot.core.buffers      in '..\..\core\mormot.core.buffers.pas',
  mormot.core.data         in '..\..\core\mormot.core.data.pas',
  mormot.core.variants     in '..\..\core\mormot.core.variants.pas',
  mormot.core.json         in '..\..\core\mormot.core.json.pas',
  mormot.core.log          in '..\..\core\mormot.core.log.pas',
  mormot.net.client        in '..\..\net\mormot.net.client.pas',
  mormot.app.console       in '..\..\app\mormot.app.console.pas',
  mormot.app.daemon        in '..\..\app\mormot.app.daemon.pas',
  mormot.app.agl           in '..\..\app\mormot.app.agl.pas';


{$R *.res}

var
  angelize: TSynAngelize;
begin
  try
    // is executed as a service/daemon or as a command line tool
    angelize := TSynAngelize.Create(nil, TSynLog);
    try
      angelize.CommandLine;
    finally
      angelize.Free;
    end;
  except
    on E: Exception do
      ConsoleShowFatalException(E, {waitforenterkey=}false);
  end;
end.

