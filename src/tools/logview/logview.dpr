/// Visual "mORMot LogView" Tool
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program logview;

{
  *****************************************************************************

  The mORMot LogView visual tool displays .log files as created by TSynLog
   - Ported from the mORMot 1 LogView sample as a FPC/Lazarus LCL application;
   - Browse, filter and search events, per thread and per day;
   - Methods profiler with merged or per-call timing;
   - Run as a HTTP server displaying remote logs echoed from mORMot clients.

  *****************************************************************************
}

{$I ..\..\mormot.defines.inc}

uses
  {$I ..\..\mormot.uses.inc}
  Interfaces, // the LCL widgetset
  Forms,
  mormot.tools.logview;

{$R *.res}

begin
  RequireDerivedFormResource := true;
  Application.Scaled := true;
  Application.Initialize;
  Application.CreateForm(TMainLogView, MainLogView);
  Application.Run;
end.

