/// framework unitary, performance and regression tests for continuous integration
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program mormot2tests;

{$I ..\src\mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$apptype console}
  {$R ..\src\mormot.win.default.manifest.res}
{$endif MSWINDOWS}

uses
  {$I ..\src\mormot.uses.inc}
  mormot.core.base         in '..\src\core\mormot.core.base.pas',
  mormot.core.rtti         in '..\src\core\mormot.core.rtti.pas',
  mormot.core.os           in '..\src\core\mormot.core.os.pas',
  mormot.core.unicode      in '..\src\core\mormot.core.unicode.pas',
  mormot.core.text         in '..\src\core\mormot.core.text.pas',
  mormot.core.datetime     in '..\src\core\mormot.core.datetime.pas',
  mormot.core.perf         in '..\src\core\mormot.core.perf.pas',
  mormot.core.test         in '..\src\core\mormot.core.test.pas',
  mormot.core.buffers      in '..\src\core\mormot.core.buffers.pas',
  mormot.core.data         in '..\src\core\mormot.core.data.pas',
  mormot.core.variants     in '..\src\core\mormot.core.variants.pas',
  mormot.core.json         in '..\src\core\mormot.core.json.pas',
  mormot.core.log          in '..\src\core\mormot.core.log.pas',
  mormot.core.crypto       in '..\src\core\mormot.core.crypto.pas',
  mormot.core.secure       in '..\src\core\mormot.core.secure.pas',
  mormot.core.ecc256r1     in '..\src\core\mormot.core.ecc256r1.pas',
  mormot.core.ecc          in '..\src\core\mormot.core.ecc.pas',
  mormot.core.jwt          in '..\src\core\mormot.core.jwt.pas',
  mormot.core.search       in '..\src\core\mormot.core.search.pas',
  mormot.core.threads      in '..\src\core\mormot.core.threads.pas',
  mormot.core.zip          in '..\src\core\mormot.core.zip.pas',
  mormot.lib.z             in '..\src\lib\mormot.lib.z.pas',
  mormot.net.sock          in '..\src\net\mormot.net.sock.pas',
  mormot.net.http          in '..\src\net\mormot.net.http.pas',
  mormot.lib.winhttp       in '..\src\lib\mormot.lib.winhttp.pas',
  mormot.lib.curl          in '..\src\lib\mormot.lib.curl.pas',
  mormot.net.client        in '..\src\net\mormot.net.client.pas',
  mormot.net.server        in '..\src\net\mormot.net.server.pas',
  mormot.db.core           in '..\src\db\mormot.db.core.pas',
  mormot.db.sql            in '..\src\db\mormot.db.sql.pas',
  mormot.db.proxy          in '..\src\db\mormot.db.proxy.pas',
  mormot.db.raw.oracle     in '..\src\db\mormot.db.raw.oracle.pas',
  mormot.db.sql.oracle     in '..\src\db\mormot.db.sql.oracle.pas',
  mormot.db.raw.postgres   in '..\src\db\mormot.db.raw.postgres.pas',
  mormot.db.sql.postgres   in '..\src\db\mormot.db.sql.postgres.pas',
  mormot.db.raw.odbc       in '..\src\db\mormot.db.raw.odbc.pas',
  mormot.db.sql.odbc       in '..\src\db\mormot.db.sql.odbc.pas',
  mormot.db.raw.oledb      in '..\src\db\mormot.db.raw.oledb.pas',
  mormot.db.sql.oledb      in '..\src\db\mormot.db.sql.oledb.pas',
  test.core.base           in '.\test.core.base.pas',
  test.core.crypto         in '.\test.core.crypto.pas';


{ TIntegrationTests }

type
  TIntegrationTests = class(TSynTestsLogged)
  published
    procedure CoreUnits;
  end;

procedure TIntegrationTests.CoreUnits;
begin
  AddCase([TTestCoreBase, TTestCoreCrypto]);
end;

begin
  TIntegrationTests.RunAsConsole('mORMot2 Regression Tests', LOG_VERBOSE);
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

