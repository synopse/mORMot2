/// framework unit, performance and regression tests for continuous integration
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program mormot2tests;

{$I mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$apptype console}
  {$R ..\src\mormot.win.default.manifest.res}
{$endif MSWINDOWS}

uses 
  mormot.core.base;

begin
  writeln('mORMot ' + SYNOPSE_FRAMEWORK_FULLVERSION + ' compiles!');
  readln;
end.
