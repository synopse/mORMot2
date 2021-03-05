program Project04InterfaceBasedClient;

{$I mormot.defines.inc}
uses
  {$I mormot.uses.inc}
  {$ifdef FPC}
  Interfaces,
  {$endif FPC}
  Forms,
  main in 'main.pas' {MainForm},
  data in 'data.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


