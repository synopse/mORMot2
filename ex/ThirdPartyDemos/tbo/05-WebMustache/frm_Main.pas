// Author: Thomas Bogenrieder
// The example is a proof of concept for working with the mORMot library. Source code is neither tested nor optimized.

unit frm_Main;

{$I mormot.defines.inc}

{$DEFINE USE_SYNEDIT}

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.WebView2, Winapi.ActiveX, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Edge,
{$IFDEF USE_SYNEDIT}
  SynEdit, SynEditKeyCmds, SynEditHighlighter, SynHighlighterHtml, SynHighlighterJScript, SynCompletionProposal,
{$ENDIF}
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.perf,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.os,
  u_FileServer,
  u_Options;

const
  WM_SERVERSTARTED = WM_USER + 780;
  WM_SERVERSELECTMODE = WM_SERVERSTARTED + 1;

type
  TfrmMain = class(TForm)
    pnlToolBar: TPanel;
    pnlStatusBar: TPanel;
    btnProjectNew: TButton;
    btnProjectLoad: TButton;
    btnProjectSave: TButton;
    lblProject: TLabel;
    PageControl: TPageControl;
    tbsMustache: TTabSheet;
    tbsHtmlView: TTabSheet;
    Splitter: TSplitter;
    pnlMustache: TPanel;
    pnlAdditional: TPanel;
    lblMustacheEditor: TLabel;
    pnlMustacheEditor: TPanel;
    pnlMustacheDataFiles: TPanel;
    lblMustacheDataFilesInfo: TLabel;
    edtMustacheDataFiles: TEdit;
    lblMustacheDataFiles: TLabel;
    lblJavaScriptEditor: TLabel;
    pnlJavaScriptEditor: TPanel;
    pnlExternalCssJsFiles: TPanel;
    lblExternalCssJsFiles: TLabel;
    edtExternalCssJsFiles: TEdit;
    pnlBrowserArea: TPanel;
    EdgeBrowser: TEdgeBrowser;
    pnlBrowserBar: TPanel;
    btnReloadPage: TButton;
    btnStandalone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: TOleEnum);
    procedure PageControlChange(Sender: TObject);
    procedure btnProjectNewClick(Sender: TObject);
    procedure btnProjectLoadClick(Sender: TObject);
    procedure btnProjectSaveClick(Sender: TObject);
    procedure btnReloadPageClick(Sender: TObject);
    procedure btnStandaloneClick(Sender: TObject);
  private
    const
      PROJECTS_FOLDER = 'Projects';
      PROJECTDATA_EXTENSION = 'mjd';
  private
  {$IFDEF USE_SYNEDIT}
    FMustacheEditor: TSynEdit;
    FJavaScriptEditor: TSynEdit;
    procedure InitSynEditControls;
  {$ELSE}
    FMustacheEditor: TMemo;
    FJavaScriptEditor: TMemo;
    procedure InitMemoControls;
  {$ENDIF}
  private
    FFileServer: TFileServer;
    FServerStarted: Boolean;
    FProjectData: TProjectData;
    FProjectFileName: TFileName;
    FBtnReloadPageTix: Int64;
    FFrmBrowserStandalone: TForm;
    procedure DoBrowserNavigate(const pmcUri: String);
    procedure DoWebpagePrepared(const pmElapsedTime: TSynMonitorOneMicroSec);
    procedure DoBrowserStandaloneShow(pmSender: TObject);
    procedure DoBrowserStandaloneClose(pmSender: TObject; var pmvAction: TCloseAction);
    procedure StartServer(pmMode: THttpServerMode; const pmcCertFileName: TFileName = '';
      const pmcPrivKeyFileName: TFileName = ''; const pmcPrivKeyPassword: String = '');
    function CmdLineServerMode(out pmoMode: THttpServerMode): Boolean;
    function LoadProjectFromFile(const pmcFileName: TFileName): Boolean;
    procedure SaveProjectToFile(const pmcFileName: TFileName);
    procedure ExecuteBrowserStandalone;
    procedure CheckBtnReloadPageMaxTimeout;
    procedure UpdateEditorControls(const pmProjectData: TProjectData);
    procedure UpdateStatusBar(const pmcMessage: String);
    procedure UpdateProjectData;
    procedure WMServerSelectMode(var pmvMessage: TMessage);
      message WM_SERVERSELECTMODE;
    procedure WMServerStarted(var pmvMessage: TMessage);
      message WM_SERVERSTARTED;
  protected
    procedure UpdateActions; override;
  end;

var
  frmMain: TfrmMain;


implementation

uses
  u_Utilities,
  frm_ServerSelection;

{$R *.dfm}

//==============================================================================
// TfrmMain
//==============================================================================

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFileServer := TFileServer.Create;
  PageControl.ActivePage := tbsMustache;

{$IFDEF USE_SYNEDIT}
  InitSynEditControls;
{$ELSE}
  InitMemoControls;
{$ENDIF}
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Options.LastProjectFileName := FProjectFileName;
  Options.Save;

  FreeAndNil(FFrmBrowserStandalone);
  FFileServer.Free;
end;


procedure TfrmMain.FormShow(Sender: TObject);
var
  serverMode: THttpServerMode;
begin
  TFormStorage.Create(Self).Load(['pnlAdditional.Height']);

  // Start server with command line parameters
  if CmdLineServerMode(serverMode) then
    StartServer(serverMode)
  else
    PostMessage(Handle, WM_SERVERSELECTMODE, 0, 0);

  // Load last opened Project
  FProjectFileName := Options.LastProjectFileName;
  if FProjectFileName <> '' then
  begin
    if ExtractFilePath(FProjectFileName) = '' then
      FProjectFileName := MakePath([Executable.ProgramFilePath, PROJECTS_FOLDER, FProjectFileName]);

    if LoadProjectFromFile(FProjectFileName) then
      UpdateStatusBar('Project loaded')
    else
      FProjectFileName := '';
  end
  else
    LoadProjectFromFile(MakePath([Executable.ProgramFilePath, 'FirstStartProject.mjd']));

  EdgeBrowser.CreateWebView;
end;


procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then Exit; //=>

  case Key of
    VK_F5:
      if (FFrmBrowserStandalone <> Nil)
        or (PageControl.ActivePage = tbsHtmlView) then
      begin
        Key := 0;
        btnReloadPageClick(Sender);
      end;
    VK_F6:
      begin
        Key := 0;
        if FProjectFileName <> '' then
        begin
          SaveProjectToFile(FProjectFileName);
          UpdateStatusBar(Format('Project saved %s', [TimeToStr(Now)]));
        end
        else
          btnProjectSaveClick(Sender);
      end;
  end;
end;


{$IFDEF USE_SYNEDIT}
procedure TfrmMain.InitSynEditControls;
begin
  FMustacheEditor := TSynEdit.Create(pnlMustacheEditor);
  FMustacheEditor.Align := alClient;
  FMustacheEditor.Parent := pnlMustacheEditor;
  FMustacheEditor.Constraints.MinHeight := 30;
  FMustacheEditor.Font.Height := -15;
  FMustacheEditor.Gutter.ShowLineNumbers := True;
  FMustacheEditor.Gutter.TrackChanges.Visible := True;
  FMustacheEditor.Highlighter := TSynHTMLSyn.Create(FMustacheEditor);
  FMustacheEditor.RightEdge := 120;
  with TSynAutoComplete.Create(pnlMustacheEditor) do
  begin
    Editor := FMustacheEditor;
    Options := Options + [scoUseInsertList];
    AutoCompleteList.LoadFromFile(MakePath([Executable.ProgramFilePath, 'HtmlAutoComplete.dci']));
  end;

  FJavaScriptEditor := TSynEdit.Create(pnlJavaScriptEditor);
  FJavaScriptEditor.Align := alClient;
  FJavaScriptEditor.Parent := pnlJavaScriptEditor;
  FJavaScriptEditor.Constraints.MinHeight := 30;
  FJavaScriptEditor.Font.Height := -15;
  FJavaScriptEditor.Gutter.ShowLineNumbers := True;
  FJavaScriptEditor.Gutter.TrackChanges.Visible := True;
  FJavaScriptEditor.Highlighter := TSynJScriptSyn.Create(FMustacheEditor);
  FJavaScriptEditor.RightEdge := 120;
end;
{$ENDIF}


{$IFNDEF USE_SYNEDIT}
procedure TfrmMain.InitMemoControls;
begin
  FMustacheEditor := TMemo.Create(pnlMustacheEditor);
  FMustacheEditor.Align := alClient;
  FMustacheEditor.Parent := pnlMustacheEditor;
  FMustacheEditor.Constraints.MinHeight := 30;
  FMustacheEditor.Font.Height := -15;
  FMustacheEditor.ScrollBars := ssBoth;
  FMustacheEditor.WordWrap := False;

  FJavaScriptEditor := TMemo.Create(pnlJavaScriptEditor);
  FJavaScriptEditor.Align := alClient;
  FJavaScriptEditor.Parent := pnlJavaScriptEditor;
  FJavaScriptEditor.Constraints.MinHeight := 30;
  FJavaScriptEditor.Font.Height := -15;
  FJavaScriptEditor.ScrollBars := ssBoth;
  FJavaScriptEditor.WordWrap := False;
end;
{$ENDIF}


procedure TfrmMain.WMServerSelectMode(var pmvMessage: TMessage);
var
  serverMode: THttpServerMode;
  certFileName,
  privKeyFileName: TFileName;
  privKeyPassword: String;
begin
  if TfrmServerSelection.Execute(serverMode, certFileName, privKeyFileName, privKeyPassword) then
    StartServer(serverMode, certFileName, privKeyFileName, privKeyPassword)
  else
    Application.Terminate;
end;


procedure TfrmMain.WMServerStarted(var pmvMessage: TMessage);
resourcestring
  SErrServerServerIsDead =
      'Server could not be started.';
begin
  FServerStarted := False;
  case Boolean(pmvMessage.WParam) of
    True:
      begin
        FFileServer.OnStartNavigate := DoBrowserNavigate;
        FFileServer.OnPrepareFinished := DoWebpagePrepared;
        ForceDirectories(MakePath([Executable.ProgramFilePath, TFileServer.ASSETS_FOLDER], True));
        ForceDirectories(MakePath([Executable.ProgramFilePath, TFileServer.PARTIALS_FOLDER], True));
        ForceDirectories(MakePath([Executable.ProgramFilePath, PROJECTS_FOLDER], True));
        UpdateStatusBar('Server started');
        FServerStarted := True;
      end
    else
      ShowMessage(SErrServerServerIsDead);
  end;
end;


procedure TfrmMain.DoBrowserNavigate(const pmcUri: String);
begin
  EdgeBrowser.Navigate(pmcUri);
end;


procedure TfrmMain.DoWebpagePrepared(const pmElapsedTime: TSynMonitorOneMicroSec);
var
  time: TShort16;
begin
  MicroSecToString(pmElapsedTime, time);
  UpdateStatusBar(Format('Page delivered in %s', [String(time)]));
end;


procedure TfrmMain.DoBrowserStandaloneShow(pmSender: TObject);
begin
  TFormStorage.Create(FFrmBrowserStandalone).Load;
end;


procedure TfrmMain.DoBrowserStandaloneClose(pmSender: TObject; var pmvAction: TCloseAction);
begin
  pnlBrowserArea.Align := alClient;
  pnlBrowserArea.Parent := tbsHtmlView;
  btnStandalone.Caption := '&Standalone';
  tbsHtmlView.TabVisible := True;
  PageControl.ActivePage := tbsHtmlView;
  FFrmBrowserStandalone := Nil;
  pmvAction := caFree;
end;


procedure TfrmMain.StartServer(pmMode: THttpServerMode; const pmcCertFileName: TFileName;
  const pmcPrivKeyFileName: TFileName; const pmcPrivKeyPassword: String);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      var isStarted: Boolean := FFileServer.StartServer(
        Options.Server.Port, pmMode, pmcCertFileName, pmcPrivKeyFileName, StringToUtf8(pmcPrivKeyPassword));

      PostMessage(Handle, WM_SERVERSTARTED, Ord(isStarted), 0);
    end).Start;
end;


function TfrmMain.CmdLineServerMode(out pmoMode: THttpServerMode): Boolean;
var
  cmdValue: String;
begin
  Result := False;
  if FindCmdLineSwitch('sm', cmdValue) then
  begin
    var idx: Integer := IdemPCharArray(PUtf8Char(Pointer(StringToUtf8(cmdValue))), ['HSMHTTP10', 'HSMHTTP11', 'HSMHTTPSSELF']);
    if idx >= 0 then
    begin
      pmoMode := THttpServerMode(idx);
      Result := True;
    end;
  end;
end;


function TfrmMain.LoadProjectFromFile(const pmcFileName: TFileName): Boolean;
begin
  Result := FProjectData.LoadFromFile(pmcFileName);
  if not Result then
    FProjectData.Clear;

  UpdateEditorControls(FProjectData);

{$IFDEF USE_SYNEDIT}
  FMustacheEditor.ClearTrackChanges;
  FJavaScriptEditor.ClearTrackChanges;
{$ENDIF}
end;


procedure TfrmMain.SaveProjectToFile(const pmcFileName: TFileName);
begin
  UpdateProjectData;
  FProjectData.SaveToFile(pmcFileName);

{$IFDEF USE_SYNEDIT}
  FMustacheEditor.MarkSaved;
  FJavaScriptEditor.MarkSaved;
{$ENDIF}
end;


procedure TfrmMain.ExecuteBrowserStandalone;
begin
  if FFrmBrowserStandalone = Nil then
  begin
    FFrmBrowserStandalone := TForm.CreateNew(Nil);
    FFrmBrowserStandalone.Name := 'frmBrowserStandalone';
    FFrmBrowserStandalone.Caption := 'Browser Standalone';
    FFrmBrowserStandalone.Constraints.MinWidth := 250;
    FFrmBrowserStandalone.Constraints.MinHeight := 400;
    FFrmBrowserStandalone.BorderStyle := bsSizeToolWin;
    FFrmBrowserStandalone.KeyPreview := True;
    FFrmBrowserStandalone.OnShow := DoBrowserStandaloneShow;
    FFrmBrowserStandalone.OnClose := DoBrowserStandaloneClose;
    FFrmBrowserStandalone.OnKeyDown := FormKeyDown;
    pnlBrowserArea.Parent := FFrmBrowserStandalone;
    pnlBrowserArea.Align := alClient;
    btnStandalone.Caption := '&Close';
    FFrmBrowserStandalone.Show;

    PageControl.ActivePage := tbsMustache;
    tbsHtmlView.TabVisible := False;
  end
  else
    FFrmBrowserStandalone.Close;
end;


procedure TfrmMain.CheckBtnReloadPageMaxTimeout;
const
  INTERVAL = (1000 * 5);  // should be enough
begin
  if btnReloadPage.Enabled then Exit; //=>

  if FBtnReloadPageTix + INTERVAL < GetTickCount64 then
    btnReloadPage.Enabled := True;
end;


procedure TfrmMain.UpdateEditorControls(const pmProjectData: TProjectData);
begin
  FMustacheEditor.Text := Utf8ToString(pmProjectData.Mustache);
  FJavaScriptEditor.Text := Utf8ToString(pmProjectData.InlineJavaScript);
  edtMustacheDataFiles.Text := Utf8ToString(RawUtf8ArrayToCsv(pmProjectData.MustacheDataFiles, '|'));
  edtExternalCssJsFiles.Text := Utf8ToString(RawUtf8ArrayToCsv(pmProjectData.ExternalCssJsFiles, '|'));
end;


procedure TfrmMain.UpdateStatusBar(const pmcMessage: String);
var
  proName: String;
begin
  proName := ExtractFileName(FProjectFileName);
  if proName = '' then
    proName := 'not saved';

  pnlStatusBar.Caption := Format('Project: %s  -  Last message: %s', [proName, pmcMessage]);
end;


procedure TfrmMain.UpdateProjectData;
begin
  SetLength(FProjectData.MustacheDataFiles, 0);
  SetLength(FProjectData.ExternalCssJsFiles, 0);
  FProjectData.Mustache := StringToUtf8(FMustacheEditor.Text);
  FProjectData.InlineJavaScript := StringToUtf8(FJavaScriptEditor.Text);
  CsvToRawUtf8DynArray(Pointer(StringToUtf8(edtMustacheDataFiles.Text)), FProjectData.MustacheDataFiles, '|', True);
  CsvToRawUtf8DynArray(Pointer(StringToUtf8(edtExternalCssJsFiles.Text)), FProjectData.ExternalCssJsFiles, '|', True);
end;


procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  CheckBtnReloadPageMaxTimeout;
  tbsHtmlView.Enabled := FServerStarted;
end;


procedure TfrmMain.EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: TOleEnum);
begin
  // The event will not be fired in rare cases -> CheckBtnReloadPageMaxTimeout
  btnReloadPage.Enabled := True;
end;


procedure TfrmMain.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = tbsHtmlView then
    btnReloadPageClick(Sender);
end;


procedure TfrmMain.btnProjectNewClick(Sender: TObject);
begin
  FProjectData.Clear;
  FProjectFileName := '';
  UpdateEditorControls(FProjectData);
  UpdateStatusBar('Project created');
end;


procedure TfrmMain.btnProjectLoadClick(Sender: TObject);
begin
  with TOpenDialog.Create(Nil) do
  try
    Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
    InitialDir := MakePath([Executable.ProgramFilePath, PROJECTS_FOLDER], True);
    DefaultExt := Format('*.%s', [PROJECTDATA_EXTENSION]);
    Filter := Format('%s (*%.s)|*.%1:s', ['Project Mustache Editor', PROJECTDATA_EXTENSION]);
    if Execute
      and LoadProjectFromFile(FileName) then
    begin
      FProjectFileName := FileName;
      UpdateStatusBar('Project loaded');
    end;
  finally
    Free;
  end;
end;


procedure TfrmMain.btnProjectSaveClick(Sender: TObject);
begin
  with TSaveDialog.Create(Nil) do
  try
    Options := Options + [ofOverwritePrompt];
    InitialDir := MakePath([Executable.ProgramFilePath, PROJECTS_FOLDER], True);
    DefaultExt := Format('*.%s', [PROJECTDATA_EXTENSION]);
    Filter := Format('%s (*.%s)|*.%1:s', ['Project Mustache Editor', PROJECTDATA_EXTENSION]);
    if FProjectFileName = '' then
      FileName := Format('Project1.%s', [PROJECTDATA_EXTENSION])
    else
      FileName := FProjectFileName;

    if Execute then
    begin
      SaveProjectToFile(FileName);
      FProjectFileName := FileName;
      UpdateStatusBar(Format('Project saved %s', [TimeToStr(Now)]));
    end;
  finally
    Free;
  end;
end;


procedure TfrmMain.btnReloadPageClick(Sender: TObject);
begin
  if not btnReloadPage.Enabled then Exit; //=>

  FBtnReloadPageTix := GetTickCount64;
  btnReloadPage.Enabled := False;
  UpdateProjectData;
  FFileServer.PrepareAndExecute(FProjectData);
end;


procedure TfrmMain.btnStandaloneClick(Sender: TObject);
begin
  btnStandalone.Enabled := False;
  try
    ExecuteBrowserStandalone;
  finally
    btnStandalone.Enabled := True;
  end;
end;

end.
