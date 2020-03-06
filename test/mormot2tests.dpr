program mormot2tests;

{$I mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$apptype console}
  {$R ../src/mormot.win.default.manifest.res}
{$endif MSWINDOWS}

uses 
  mormot.core.types;

begin
  writeln('mORMot ' + SYNOPSE_FRAMEWORK_FULLVERSION + ' compiles!');
  readln;
end.
