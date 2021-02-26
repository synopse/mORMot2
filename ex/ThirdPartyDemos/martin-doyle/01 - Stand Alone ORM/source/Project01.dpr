//1 One Liner
{:
Synopse mORMot framework 2

Sample 01 - In Memory ORM
purpose of this sample is to show the basic ORM usage of the framework:
}
program Project01;

{$I mormot.defines.inc}
{$define PUREMORMOT2}
uses
  {$I mormot.uses.inc}
  SysUtils,
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif FPC}
  main in 'main.pas' {MainForm},
  data in 'data.pas',
  client in 'client.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.





