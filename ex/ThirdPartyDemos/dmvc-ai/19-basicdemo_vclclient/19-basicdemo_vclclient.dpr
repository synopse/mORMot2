program BasicDemoVCLClient;

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  Forms,
  MainFormU in 'MainFormU.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
