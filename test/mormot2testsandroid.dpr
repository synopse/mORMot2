program mormot2testsandroid;

uses
  System.StartUpCopy,
  FMX.Forms,
  test.main.android in 'test.main.android.pas' {Form1},
  test.core.base in 'test.core.base.pas',
  test.soa.network in 'test.soa.network.pas',
  test.soa.core in 'test.soa.core.pas',
  test.orm.threads in 'test.orm.threads.pas',
  test.orm.sqlite3 in 'test.orm.sqlite3.pas',
  test.orm.network in 'test.orm.network.pas',
  test.orm.extdb in 'test.orm.extdb.pas',
  test.orm.core in 'test.orm.core.pas',
  test.net.proto in 'test.net.proto.pas',
  test.core.script in 'test.core.script.pas',
  test.core.ecc in 'test.core.ecc.pas',
  test.core.data in 'test.core.data.pas',
  test.core.crypt in 'test.core.crypt.pas',
  test.core.collections in 'test.core.collections.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
