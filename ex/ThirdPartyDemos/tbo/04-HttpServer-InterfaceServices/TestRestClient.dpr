// Author: Thomas Bogenrieder
// The example is a proof of concept for creating interface-based services with mORMot, the source code is neither tested nor optimized.

program TestRestClient;

{$I mormot.defines.inc}

uses
  Vcl.Forms,
  frm_Main in 'frm_Main.pas' {frmMain},
  u_ServiceInterfaces in 'u_ServiceInterfaces.pas';

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
