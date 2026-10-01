unit mormot2tests.android.form;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Memo,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Types,
  mormot.core.base;

type
  TAndroidTestForm = class;

  TTestRunnerThread = class(TThread)
  private
    FForm: TAndroidTestForm;
    FResultText: string;
    FSuccess: Boolean;
    FAssertions: Integer;
    FFailed: Integer;
    FUiThreadId: NativeUInt;
    FRestriction: RawUtf8;
    procedure CaptureOutput(const AValue: RawUtf8);
    procedure ReportFinished;
    procedure CaptureUiThread;
  protected
    procedure Execute; override;
  public
    constructor Create(AForm: TAndroidTestForm; const ARestriction: RawUtf8);
  end;

  TAndroidTestForm = class(TForm)
  private
    FStartButton: TButton;
    FFocusedButton: TButton;
    FThreadButton: TButton;
    FShareButton: TButton;
    FStatusLabel: TLabel;
    FTerminalBackground: TRectangle;
    FOutputMemo: TMemo;
    FPendingOutput: string;
    FOutputTimer: TTimer;
    procedure AppendTerminal(const AText: string);
    procedure FlushTerminal(Sender: TObject);
    procedure ApplyTerminalStyle(Sender: TObject);
    procedure ShareResult(Sender: TObject);
    procedure StartTests(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure TestsFinished(const AText: string; ASuccess: Boolean;
      AAssertions, AFailed: Integer);
  end;

var
  AndroidTestForm: TAndroidTestForm;

implementation

uses
  mormot.core.test,
  mormot.core.os,
  mormot.core.unicode,
  mormot.lib.openssl11,
  mormot.crypt.x509,
  mormot.crypt.openssl,
  mormot.db.raw.sqlite3,
  mormot.tools.ecc,
  test.core.base,
  test.core.threads,
  test.core.data,
  test.core.crypt,
  test.core.ecc,
  test.core.collections,
  test.net.proto,
  test.orm.core,
  test.orm.sqlite3,
  test.orm.extdb,
  test.orm.threads,
  test.orm.network,
  test.soa.core,
  test.soa.network,
  {$IFDEF ANDROID}
  FMX.MediaLibrary.Android,
  Androidapi.Log,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  {$ENDIF}
  FMX.MediaLibrary,
  FMX.Platform;

procedure RunnerTrace(const Text: string);
var
  U: UTF8String;
begin
  {$IFDEF ANDROID}
  U := UTF8Encode(Text);
  __android_log_write(ANDROID_LOG_INFO, 'mormot-runner', PAnsiChar(U));
  {$ENDIF}
end;

procedure EnsureAndroidSQLite;
begin
  if sqlite3 = nil then
    sqlite3 := TSqlite3LibraryDynamic.Create(SQLITE_LIBRARY_DEFAULT_NAME);
end;

type
  TAndroidIntegrationTests = class(TSynTestsLogged)
  published
    procedure CoreUnits;
    procedure ORM;
    procedure SOA;
  end;

procedure TAndroidIntegrationTests.CoreUnits;
begin
  AddCase([
    TTestCoreBase,
    TTestCoreThreads,
    TTestCoreProcess,
    TTestCoreCollections,
    TTestCoreCrypto,
    TTestCoreEcc,
    TTestCoreCompression,
    TNetworkProtocols
  ]);
end;

procedure TAndroidIntegrationTests.ORM;
begin
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

procedure TAndroidIntegrationTests.SOA;
begin
  AddCase([
    TTestServiceOrientedArchitecture,
    TTestBidirectionalRemoteConnection
  ]);
end;

constructor TTestRunnerThread.Create(AForm: TAndroidTestForm;
  const ARestriction: RawUtf8);
begin
  inherited Create(True);
  FForm := AForm;
  FRestriction := ARestriction;
  FreeOnTerminate := True;
end;

procedure TTestRunnerThread.Execute;
var
  Tests: TSynTestsLogged;
begin
  Tests := nil;
  try
    try
      // Prove that the FMX UI thread services Synchronize from the test worker.
      // The SOA tests below still use this worker as their logical main thread;
      // they do not validate mORMot's actual UI-thread dispatch on Android.
      TThread.Synchronize(nil, CaptureUiThread);
      if (FUiThreadId <> NativeUInt(MainThreadID)) or
         (FUiThreadId = NativeUInt(GetCurrentThreadID)) then
        raise Exception.Create('FMX main-thread synchronization failed');
      RegisterOpenSsl;
      RegisterX509;
      if not OpenSslIsAvailable then
        raise Exception.Create('Android OpenSSL 1.1 static libraries unavailable');
      Tests := TAndroidIntegrationTests.Create('mORMot2 Android Regression Tests');
      if FRestriction = 'thread-checks' then
        Tests.Restrict := ['CoreBase._TSynQueue', 'CoreThreads.ExclusiveLocks']
      else if FRestriction <> '' then
        Tests.Restrict := [FRestriction];
      Tests.CustomOutput := CaptureOutput;
      FSuccess := Tests.Run;
      FAssertions := Tests.Assertions;
      FFailed := Tests.AssertionsFailed;
    except
      on E: Exception do
      begin
        FSuccess := False;
        FResultText := E.ClassName + ': ' + E.Message;
      end;
    end;
  finally
    Tests.Free;
  end;
  TThread.Synchronize(nil, ReportFinished);
end;

procedure TTestRunnerThread.CaptureUiThread;
begin
  FUiThreadId := NativeUInt(GetCurrentThreadID);
end;

procedure TTestRunnerThread.CaptureOutput(const AValue: RawUtf8);
var
  LForm: TAndroidTestForm;
  LText: string;
begin
  LForm := FForm;
  LText := Utf8ToString(AValue);
  RunnerTrace(LText);
  TThread.Queue(nil,
    procedure
    begin
      if LForm <> nil then
        LForm.AppendTerminal(LText);
    end);
end;

procedure TTestRunnerThread.ReportFinished;
begin
  FForm.TestsFinished(FResultText, FSuccess, FAssertions, FFailed);
end;

constructor TAndroidTestForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'mORMot2 Android Tests';
  Width := 720;
  Height := 1100;

  FStatusLabel := TLabel.Create(Self);
  FStatusLabel.Parent := Self;
  FStatusLabel.Align := TAlignLayout.Top;
  FStatusLabel.Height := 52;
  FStatusLabel.Text := 'Ready';
  FStatusLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FStatusLabel.TextSettings.VertAlign := TTextAlign.Center;

  FStartButton := TButton.Create(Self);
  FStartButton.Parent := Self;
  FStartButton.Align := TAlignLayout.Top;
  FStartButton.Height := 64;
  FStartButton.Text := 'Run tests';
  FStartButton.OnClick := StartTests;

  FFocusedButton := TButton.Create(Self);
  FFocusedButton.Parent := Self;
  FFocusedButton.Align := TAlignLayout.Top;
  FFocusedButton.Height := 56;
  FFocusedButton.Text := 'Run PSS certificate tests';
  FFocusedButton.OnClick := StartTests;

  FThreadButton := TButton.Create(Self);
  FThreadButton.Parent := Self;
  FThreadButton.Align := TAlignLayout.Top;
  FThreadButton.Height := 56;
  FThreadButton.Text := 'Run thread checks';
  FThreadButton.OnClick := StartTests;

  FShareButton := TButton.Create(Self);
  FShareButton.Parent := Self;
  FShareButton.Align := TAlignLayout.Top;
  FShareButton.Height := 56;
  FShareButton.Text := 'Share results';
  FShareButton.Enabled := False;
  FShareButton.OnClick := ShareResult;

  FTerminalBackground := TRectangle.Create(Self);
  FTerminalBackground.Parent := Self;
  FTerminalBackground.Align := TAlignLayout.Client;
  FTerminalBackground.Fill.Kind := TBrushKind.Solid;
  FTerminalBackground.Fill.Color := TAlphaColors.Black;
  FTerminalBackground.Stroke.Kind := TBrushKind.None;
  FTerminalBackground.HitTest := False;

  FOutputMemo := TMemo.Create(Self);
  FOutputMemo.ControlType := TControlType.Styled;
  FOutputMemo.OnApplyStyleLookup := ApplyTerminalStyle;
  FOutputMemo.Parent := Self;
  FOutputMemo.Align := TAlignLayout.Client;
  FOutputMemo.ReadOnly := True;
  FOutputMemo.WordWrap := False;
  FOutputMemo.StyledSettings := FOutputMemo.StyledSettings -
    [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.FontColor];
  FOutputMemo.TextSettings.Font.Family := 'monospace';
  FOutputMemo.TextSettings.Font.Size := 12;
  FOutputMemo.TextSettings.FontColor := TAlphaColors.Lime;
  FOutputMemo.Text :=
    'mORMot2 Android Test Terminal' + sLineBreak +
    'Ready. Tap "Run tests".';
  FOutputMemo.ApplyStyleLookup;
  ApplyTerminalStyle(FOutputMemo);

  FOutputTimer := TTimer.Create(Self);
  FOutputTimer.Interval := 60;
  FOutputTimer.Enabled := True;
  FOutputTimer.OnTimer := FlushTerminal;

end;

procedure TAndroidTestForm.ApplyTerminalStyle(Sender: TObject);
var
  Background: TFmxObject;
  Rectangle1: TRectangle;
  I: Integer;

  procedure PaintBlack(AObject: TFmxObject);
  var
    I: Integer;
  begin
    if AObject = nil then
      Exit;
    if AObject is TShape then
    begin
      TShape(AObject).Fill.Kind := TBrushKind.Solid;
      TShape(AObject).Fill.Color := TAlphaColors.Black;
      TShape(AObject).Stroke.Color := TAlphaColors.Dimgray;
    end
    else if AObject is TBrushObject then
    begin
      TBrushObject(AObject).Brush.Kind := TBrushKind.Solid;
      TBrushObject(AObject).Brush.Color := TAlphaColors.Black;
    end;
    for I := 0 to AObject.ChildrenCount - 1 do
      PaintBlack(AObject.Children[I]);
  end;

begin
  Background := FOutputMemo.FindStyleResource('background');
  PaintBlack(Background);
  if Background <> nil then
  begin
    Rectangle1 := nil;
    for I := 0 to Background.ChildrenCount - 1 do
      if (Background.Children[I] is TRectangle) and
         (Background.Children[I].Name = 'bgColorRect') then
      begin
        Rectangle1 := TRectangle(Background.Children[I]);
        Break;
      end;
    if Rectangle1 = nil then
    begin
      Rectangle1 := TRectangle.Create(Background);
      Rectangle1.Name := 'bgColorRect';
      Background.AddObject(Rectangle1);
      Rectangle1.Align := TAlignLayout.Client;
      Rectangle1.Margins := TBounds.Create(TRectF.Create(-2, -2, -2, -2));
      Rectangle1.HitTest := False;
      Rectangle1.SendToBack;
    end;
    Rectangle1.Fill.Kind := TBrushKind.Solid;
    Rectangle1.Fill.Color := TAlphaColors.Black;
    Rectangle1.Stroke.Kind := TBrushKind.None;
  end;
end;

procedure TAndroidTestForm.AppendTerminal(const AText: string);
begin
  if AText <> '' then
    FPendingOutput := FPendingOutput + AText;
end;

procedure TAndroidTestForm.FlushTerminal(Sender: TObject);
const
  MAX_TERMINAL_CHARS = 60000;
var
  S: string;
begin
  if FPendingOutput = '' then
    Exit;
  S := FOutputMemo.Text + FPendingOutput;
  FPendingOutput := '';
  if Length(S) > MAX_TERMINAL_CHARS then
    Delete(S, 1, Length(S) - MAX_TERMINAL_CHARS);
  FOutputMemo.Text := S;
  FOutputMemo.SelStart := Length(FOutputMemo.Text);
  FOutputMemo.SelLength := 0;
  FOutputMemo.GoToTextEnd;
end;

procedure TAndroidTestForm.ShareResult(Sender: TObject);
var
  ShareService: IFMXShareSheetActionsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
       IFMXShareSheetActionsService, ShareService) then
    ShareService.Share(FShareButton, FOutputMemo.Text, nil)
  else
    FStatusLabel.Text := 'Sharing is unavailable on this device';
end;

procedure TAndroidTestForm.StartTests(Sender: TObject);
var
  Runner: TTestRunnerThread;
begin
  try
    EnsureAndroidSQLite;
  except
    on E: Exception do
    begin
      FStatusLabel.Text := 'Could not load SQLite';
      AppendTerminal(sLineBreak + E.ClassName + ': ' + E.Message + sLineBreak);
      FStartButton.Enabled := True;
      FShareButton.Enabled := True;
      Exit;
    end;
  end;
  FStartButton.Enabled := False;
  FFocusedButton.Enabled := False;
  FThreadButton.Enabled := False;
  FShareButton.Enabled := False;
  FStatusLabel.Text := 'Tests running ...';
  FPendingOutput := '';
  FOutputMemo.Text :=
    'mORMot2 Android Test Terminal' + sLineBreak +
    'Tests started ...' + sLineBreak + sLineBreak;
  if Sender = FThreadButton then
    Runner := TTestRunnerThread.Create(Self, 'thread-checks')
  else if Sender = FFocusedButton then
    Runner := TTestRunnerThread.Create(Self, 'CoreCrypto.PssCertificates')
  else
    Runner := TTestRunnerThread.Create(Self, '');
  Runner.Start;
end;

procedure TAndroidTestForm.TestsFinished(const AText: string;
  ASuccess: Boolean; AAssertions, AFailed: Integer);
begin
  if AText <> '' then
    AppendTerminal(sLineBreak + AText + sLineBreak);
  if ASuccess then
    FStatusLabel.Text := Format('Passed: %d assertions', [AAssertions])
  else
    FStatusLabel.Text := Format('Failed: %d of %d assertions',
      [AFailed, AAssertions]);
  AppendTerminal(sLineBreak + FStatusLabel.Text + sLineBreak);
  FlushTerminal(nil);
  FStartButton.Enabled := True;
  FFocusedButton.Enabled := True;
  FThreadButton.Enabled := True;
  FShareButton.Enabled := True;
end;

end.
