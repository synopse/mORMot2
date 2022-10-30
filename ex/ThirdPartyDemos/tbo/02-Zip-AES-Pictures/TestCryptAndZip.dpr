program TestCryptAndZip;

uses
  Vcl.Forms,
  frm_Main in 'frm_Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
