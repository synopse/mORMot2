program httpServerUpload;

{
  HTTP Server receiving multipart/form-data file uploads without buffering
  the request body in memory - see https://github.com/synopse/mORMot2/issues/292
}

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\src\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc} // may include mormot.core.fpcx64mm.pas
  sysutils,
  mormot.core.os,
  mormot.core.text,
  uploadServerMain in 'uploadServerMain.pas';

begin
  try
    //ReportMemoryLeaksOnShutdown := true;
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
