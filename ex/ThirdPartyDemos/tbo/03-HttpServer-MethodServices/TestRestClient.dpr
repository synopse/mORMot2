program TestRestClient;

uses
  Vcl.Forms,
  frm_Main in 'frm_Main.pas' {frmMain},
  u_SharedTypes in 'u_SharedTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
