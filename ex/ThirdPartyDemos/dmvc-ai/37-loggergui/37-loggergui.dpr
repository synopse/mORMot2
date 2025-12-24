program LoggerGUISample;

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  Vcl.Forms,
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.log,
  MainFormU in 'src\MainFormU.pas' {MainForm};

{$R *.res}

begin
  // Configure logging before creating form
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
