/// ORM + REST Benchmark Over Several DB backends
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program PerfTestConsole;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {../../src/$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  mormot.core.log,
  PerfTestCases in '.\PerfTestCases.pas';

begin
  TTestDatabaseBenchmark.RunAsConsole(
    'mORMot Framework Database Benchmark'{, LOG_VERBOSE});
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.
