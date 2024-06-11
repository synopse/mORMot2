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
  httpServerMain in 'httpServerMain.pas';

begin
  Main;
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}

end.

