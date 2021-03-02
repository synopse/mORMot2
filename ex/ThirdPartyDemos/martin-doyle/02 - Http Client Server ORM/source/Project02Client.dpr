{:
Synopse mORMot framework 2

Sample 02 - Http Client Server ORM
purpose of this sample is to show the basic ORM usage of the framework:
}
program Project02Client;

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





