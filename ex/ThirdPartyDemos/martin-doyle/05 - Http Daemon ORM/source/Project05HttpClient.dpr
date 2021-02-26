program Project05HttpClient;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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


