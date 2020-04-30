/// framework unitary, performance and regression tests for continuous integration
// - this program is a part of the freeware Synopse mORMot framework 2,
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
  mormot.core.jwt          in '..\src\core\mormot.core.jwt.pas',
  mormot.core.search       in '..\src\core\mormot.core.search.pas',
  mormot.lib.z             in '..\src\lib\mormot.lib.z.pas',
  test.core.base           in '.\test.core.base.pas',
  test.core.crypto         in '.\test.core.crypto.pas';

type
  TIntegrationTests = class(TSynTestsLogged)
  published
    procedure CoreUnits;
  end;

{ TIntegrationTests }

procedure TIntegrationTests.CoreUnits;
begin
  AddCase([TTestCoreBase, TTestCoreCrypto]);
end;

begin
  TIntegrationTests.RunAsConsole('mORMot2 Regression Tests', LOG_VERBOSE);
{%H-}end.

