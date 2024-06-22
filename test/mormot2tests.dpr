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
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.datetime,
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
  mormot.lib.openssl11,
  mormot.crypt.x509,
  mormot.crypt.openssl,
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
  protected
    class procedure DescribeCommandLine; override;
  public
    function Run: boolean; override;
  published
    procedure CoreUnits;
    procedure ORM;
    procedure SOA;
  end;

class procedure TIntegrationTests.DescribeCommandLine;
begin
  with Executable.Command do
  begin
    ExeDescription := 'mORMot '+ SYNOPSE_FRAMEWORK_VERSION + ' Regression Tests';
    Param('dns', 'a DNS #server name/IP for LDAP tests via Kerberos ' +
      {$ifdef OSWINDOWS}
      'with current logged user');
      {$else}
      'after kinit');
      {$endif OSWINDOWS}
    Param('ldapusr', 'the LDAP #user for --dns, e.g. name@ad.company.com');
    Param('ldappwd', 'the LDAP #password for --dns');
    Param('ntp', 'a NTP/SNTP #server name/IP to use instead of time.google.com');
    Option('nontp', 'disable the NTP/SNTP server tests');
    {$ifdef USE_OPENSSL}
    // refine the OpenSSL library path - RegisterOpenSsl is done in Run method
    OpenSslDefaultCrypto := Utf8ToString(
      Param('libcrypto', 'the OpenSSL libcrypto #filename'));
    OpenSslDefaultSsl := Utf8ToString(
      Param('libssl', 'the OpenSSL libssl #filename'));
    {$endif USE_OPENSSL}
  end;
end;

function TIntegrationTests.Run: boolean;
var
  ssl: shortstring;
begin
  ssl[0] := #0;
  {$ifdef USE_OPENSSL}
  // warning: OpenSSL on Windows requires to download the right libraries
  RegisterOpenSsl;
  RegisterX509; // enable the additional CryptPublicKey[] algorithms for X.509
  if OpenSslIsAvailable then
    FormatShort(' and %', [OpenSslVersionText], ssl);
  {$endif USE_OPENSSL}
  CustomVersions := Format(CRLF + CRLF + '%s [%s %s %x]'+ CRLF +
    '    %s' + CRLF + '    on %s'+ CRLF + 'Using mORMot %s %s%s'+ CRLF + '    %s',
    [OSVersionText, CodePageToText(Unicode_CodePage), KBNoSpace(SystemMemorySize),
     OSVersionInt32, CpuInfoText, BiosInfoText, SYNOPSE_FRAMEWORK_FULLVERSION,
     UnixTimeToTextDateShort(FileAgeToUnixTimeUtc(Executable.ProgramFileName)),
     ssl, sqlite3.Version]);
  result := inherited Run;
end;

procedure TIntegrationTests.CoreUnits;
begin
  //exit;
  AddCase([
    TTestCoreBase,
    TTestCoreProcess,
    {$ifdef HASGENERICS} // do-nothing on oldest compilers (e.g. <= Delphi XE7)
    TTestCoreCollections,
    {$endif HASGENERICS}
    TTestCoreCrypto,
    TTestCoreEcc,
    TTestCoreCompression,
    TNetworkProtocols
  ]);
end;

procedure TIntegrationTests.ORM;
begin
  //exit;
  AddCase([
    TTestOrmCore,
    TTestSqliteFile,
    TTestSqliteFileWAL,
    TTestSqliteFileMemoryMap,
    TTestSqliteMemory,
    TTestExternalDatabase,
    TTestClientServerAccess,
    TTestMultiThreadProcess
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
    TTestServiceOrientedArchitecture,
    TTestBidirectionalRemoteConnection
  ]);
end;


begin
  SetExecutableVersion(SYNOPSE_FRAMEWORK_VERSION);
  TIntegrationTests.RunAsConsole('mORMot2 Regression Tests',
    //LOG_VERBOSE +
    LOG_FILTER[lfExceptions] // + [sllErrors, sllWarning]
    ,[], Executable.ProgramFilePath + 'data');
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

