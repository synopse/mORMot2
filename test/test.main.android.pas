unit test.main.android;

interface

{$I ..\src\mormot.defines.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    ButtonStart: TButton;
    Memo1: TMemo;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCollected: String;
    FTestThread: TThread;
    property Collected: String read FCollected write FCollected;
  public
    procedure DoColor(aColor: TColor);
    procedure DoText(const aText: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.test,
  {$ifdef USE_OPENSSL}
  mormot.crypt.openssl,
  mormot.crypt.X509,
  mormot.lib.openssl11,
  {$endif}
  test.core.base,
  test.core.data,
  test.core.crypt,
  test.core.ecc,
  test.core.collections ,
  {$ifdef LIBQUICKJSSTATIC}
  test.core.script         in '.\test.core.script.pas',
  {$endif LIBQUICKJSSTATIC}
  test.net.proto,
  test.orm.core,
  test.orm.sqlite3,
  test.orm.extdb,
  test.orm.threads,
  test.orm.network,
  test.soa.core,
  test.soa.network,
  uToolbox;


{ TIntegrationTests }

type
  TIntegrationTests = class(TSynTestsLogged)
  protected
    procedure DoLog(Level: TSynLogLevel; const TextFmt: RawUtf8;
      const TextArgs: array of const); override;
    procedure DoText(const value: RawUtf8); overload; override;
    procedure DoColor(aColor: TConsoleColor); override;
  public
    function Run: boolean; override;
  published
    procedure CoreUnits;
    procedure ORM;
    procedure SOA;
  end;


function TIntegrationTests.Run: boolean;
var
  ssl: shortstring;
begin
  ssl[0] := #0;
  CustomVersions := '';
  {$ifdef USE_OPENSSL}
  // warning: OpenSSL on Windows requires to download the right libraries
  RegisterOpenSsl;
  RegisterX509; // enable the additional CryptPublicKey[] algorithms for X.509
  if OpenSslIsAvailable then
    CustomVersions := Format(' and %s', [OpenSslVersionText]);
  {$endif USE_OPENSSL}
  // add addition version information about the system and the executable
//  CustomVersions := Format(CRLF + CRLF + '%s [%s %s %x]'+ CRLF +
//    '    %s' + CRLF + '    on %s'+ CRLF + 'Using mORMot %s %s%s'+ CRLF + '    %s',
//    [OSVersionText, CodePageToText(Unicode_CodePage), KBNoSpace(SystemMemorySize),
//     OSVersionInt32, CpuInfoText, BiosInfoText, SYNOPSE_FRAMEWORK_FULLVERSION,
//     // get compilation date of this executable: if moved to mormot.core.os, will
//     // return the compilation timestamp of this unit, not of the project itself
//     DateToTextDateShort({$ifdef FPC} Iso8601ToDateTime({$I %DATE%}) {$else}
//       Executable.Version.BuildDateTime {$endif}), ssl, sqlite3.Version]);
  CustomVersions := 'Mormot Android-Tests 1.0' + CustomVersions;
  result := inherited Run;
end;

procedure TIntegrationTests.CoreUnits;
begin
  //exit;
  AddCase([
    TTestCoreBase,
    TTestCoreProcess,
    {$ifdef HASGENERICS} // do-nothing on oldest compilers (e.g. <= Delphi XE7)
//    TTestCoreCollections,  ==> Successfull
    {$endif HASGENERICS}
    TTestCoreCrypto,
    TTestCoreEcc //,
//     TTestCoreCompression, ==> Successfull
//    TNetworkProtocols
  ]);
end;

procedure TIntegrationTests.DoColor(aColor: TConsoleColor);
var stdColor: TColor;
begin
  case aColor of
   ccBlack: stdColor:= TColorRec.Black;
   ccBlue: stdColor:= TColorRec.Blue;
   ccGreen: stdColor:= TColorRec.Green;
   ccCyan: stdColor:= TColorRec.Cyan;
   ccRed: stdColor:= TColorRec.Red;
   ccMagenta: stdColor:= TColorRec.Magenta;
   ccBrown: stdColor:= TColorRec.Brown;
   ccLightGray: stdColor:= TColorRec.LightGray;
   ccDarkGray: stdColor:= TColorRec.DarkGray;
   ccLightBlue: stdColor:= TColorRec.LightBlue;
   ccLightGreen: stdColor:= TColorRec.LightGreen;
   ccLightCyan: stdColor:= TColorRec.LightCyan;
   ccLightRed: stdColor:= TColorRec.Tomato;
   ccLightMagenta: stdColor:= TColorRec.Violet;
   ccYellow: stdColor:= TColorRec.Yellow;
   ccWhite: stdColor:= TColorRec.White;
   else
      stdColor:= TColorRec.Black;
  end;
//  TThread.Synchronize(TThread.CurrentThread, procedure begin Form1.DoColor(stdColor) end);
end;

procedure TIntegrationTests.DoLog(Level: TSynLogLevel; const TextFmt: RawUtf8; const TextArgs: array of const);
begin
  inherited;

end;

procedure TIntegrationTests.DoText(const value: RawUtf8);
begin
  TThread.Synchronize(TThread.CurrentThread, procedure begin Form1.DoText(value) end);
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

Type TTestThread = class(TThread)
       protected
         procedure Execute; override;
      end;

procedure TTestThread.Execute;
var test: TIntegrationTests;
begin
  test:= TIntegrationTests.Create('Mormot Android-Tests');
  try
    test.WorkDir:= OPCProgramDataPreferredBaseDir;
    test.Run;
  finally
    test.Free;
    Form1.FTestThread:= nil;
    Form1.ButtonStart.Enabled:= true;
  end;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  if assigned(FTestThread) then
     Exit;
  SetExecutableVersion(SYNOPSE_FRAMEWORK_VERSION);
  FTestThread:= TTestThread.Create(false);
  ButtonStart.Enabled:= false;
end;

procedure TForm1.DoColor(aColor: TColor);
begin
  memo1.FontColor:= aColor;
end;

procedure TForm1.DoText(const aText: string);
begin

  if aText.EndsWith(CRLF) then
     begin
     memo1.Lines.Add(Collected + Copy(aText, 1, Length(aText) - Length(CRLF)));
     memo1.ScrollBy(0, memo1.ViewportSize.cY, true);
//     ScrollToTop()
     Collected:= '';
     end
  else
     Collected:= Collected + aText;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ButtonStartClick(nil);
end;


end.
