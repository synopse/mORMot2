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
  mormot.core.base in '..\src\core\mormot.core.base.pas',
  mormot.core.os in '..\src\core\mormot.core.os.pas',
  mormot.core.text in '..\src\core\mormot.core.text.pas',
  mormot.core.data in '..\src\core\mormot.core.data.pas',
  mormot.core.datetime in '..\src\core\mormot.core.datetime.pas',
  mormot.core.rtti in '..\src\core\mormot.core.rtti.pas',
  mormot.core.json in '..\src\core\mormot.core.json.pas',
  mormot.core.variants in '..\src\core\mormot.core.variants.pas';

begin
  writeln('mORMot ' + SYNOPSE_FRAMEWORK_FULLVERSION + ' compiles!');
  readln;
end.

