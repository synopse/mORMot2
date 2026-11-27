program mormot2tests_android;

uses
  System.StartUpCopy,
  FMX.Forms,
  mormot2tests.android.form in 'mormot2tests.android.form.pas';

begin
  Application.Initialize;
  Application.CreateForm(TAndroidTestForm, AndroidTestForm);
  Application.Run;
end.
