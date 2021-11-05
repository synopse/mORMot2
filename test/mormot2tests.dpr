/// framework unitary, performance and regression tests for continuous integration
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program mormot2tests;

// ---------------------------------------------------------------------
//  NOTE: on FPC, please first install src/packages/lazarus/mormot2.lpk
// ---------------------------------------------------------------------

{$I ..\src\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\src\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I ..\src\mormot.uses.inc} // may include mormot.core.fpcx64mm.pas
  {$ifdef UNIX}
  cwstring, // needed as fallback if ICU is not available
  {$endif UNIX}
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.test,
  mormot.db.raw.sqlite3, // for the SQLite3 version below

  {$ifdef USEZEOS}
  mormot.db.sql.zeos,
  {$endif USEZEOS}
  {$ifdef FPC}
  //jsontools in '.\3party\jsontools.pas',
  //superobject in '.\3party\superobject.pas',
  //supertypes in '.\3party\supertypes.pas',
  //superdate in '.\3party\superdate.pas',
  {$else}
  //mormot.db.rad,
  //mormot.db.rad.bde,
  //mormot.db.rad.firedac,
  //mormot.db.rad.unidac,
  //mormot.db.rad.nexusdb,
  {$endif FPC}
  mormot.tools.ecc         in '..\src\tools\ecc\mormot.tools.ecc.pas',
  test.core.base           in '.\test.core.base.pas',
  test.core.data           in '.\test.core.data.pas',
  test.core.crypt          in '.\test.core.crypt.pas',
  test.core.ecc            in '.\test.core.ecc.pas',
  test.core.collections    in '.\test.core.collections.pas',
  {$ifdef LIBQUICKJSSTATIC}
  test.core.script         in '.\test.core.script.pas',
  {$endif LIBQUICKJSSTATIC}
  test.net.proto           in '.\test.net.proto.pas',
  test.orm.core            in '.\test.orm.core.pas',
  test.orm.sqlite3         in '.\test.orm.sqlite3.pas',
  test.orm.extdb           in '.\test.orm.extdb.pas',
  test.orm.threads         in '.\test.orm.threads.pas',
  test.orm.network         in '.\test.orm.network.pas',
  test.soa.core            in '.\test.soa.core.pas',
  test.soa.network         in '.\test.soa.network.pas';


{ TIntegrationTests }

type
  TIntegrationTests = class(TSynTestsLogged)
  public
    function Run: boolean; override;
  published
    procedure CoreUnits;
    procedure ORM;
    procedure SOA;
  end;

function TIntegrationTests.Run: boolean;
var
  cp: shortstring;
begin
  str(Unicode_CodePage, cp);
  if cp = '65001' then
    cp := 'utf8';
  CustomVersions := Format(#13#10#13#10'%s (cp %s)'#13#10 +
    '    %s'#13#10'    on %s'#13#10'Using mORMot %s'#13#10'    %s',
    [OSVersionText, cp, CpuInfoText, BiosInfoText,
     SYNOPSE_FRAMEWORK_FULLVERSION, sqlite3.Version]);
  result := inherited Run;
end;

procedure TIntegrationTests.CoreUnits;
begin
  //
  AddCase([
    //TTestCoreBase,
    //TTestCoreProcess
  ]);
  //exit;
  AddCase([
  //
    TTestCoreBase, TTestCoreProcess,
    {$ifdef HASGENERICS} // do-nothing on oldest compilers (e.g. <= Delphi XE7)
    TTestCoreCollections,
    {$endif HASGENERICS}
    TTestCoreCrypto, TTestCoreEcc,
    TTestCoreCompression, TNetworkProtocols
  ]);
end;

procedure TIntegrationTests.ORM;
begin
  //exit;
  AddCase([
    //
    TTestOrmCore, TTestSqliteFile, TTestSqliteFileWAL, TTestSqliteFileMemoryMap,
    TTestSqliteMemory, TTestExternalDatabase,
    TTestClientServerAccess, TTestMultiThreadProcess
  ]);
end;

procedure TIntegrationTests.SOA;
begin
  //exit;
  {$ifdef LIBQUICKJSSTATIC}
  AddCase(TTestCoreScript);
  {$endif LIBQUICKJSSTATIC}
  //exit;
  AddCase([
    //
    TTestServiceOrientedArchitecture,
    TTestBidirectionalRemoteConnection
  ]);
end;


begin
  TIntegrationTests.RunAsConsole('mORMot2 Regression Tests',
    //LOG_VERBOSE,
    LOG_FILTER[lfExceptions],
    [], Executable.ProgramFilePath + 'data');
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

