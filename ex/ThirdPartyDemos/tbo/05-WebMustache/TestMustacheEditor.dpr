// Author: Thomas Bogenrieder
// The example is a proof of concept for working with the mORMot library. Source code is neither tested nor optimized.

program TestMustacheEditor;

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  Vcl.Forms,
  u_FileServer in 'u_FileServer.pas',
  frm_Main in 'frm_Main.pas' {frmMain};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
