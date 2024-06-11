program httpServerFiles;

{
  Simple HTTP/HTTPS Server of some static content.
}

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  //{$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc} // may include mormot.core.fpcx64mm.pas
  sysutils,
  mormot.core.text,
  httpServerMain in 'httpServerMain.pas';

begin

  try
    Main;
    {$ifdef FPC_X64MM}
    if (ExitCode = 0) and
       not silent then
      WriteHeapStatus(' ', 16, 8, {compileflags=}true);
    {$endif FPC_X64MM}
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;

end.

