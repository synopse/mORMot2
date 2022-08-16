// Author: Thomas Bogenrieder
// The example is a proof of concept for creating interface-based services with mORMot, the source code is neither tested nor optimized.

unit frm_Main;

{$I mormot.defines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.threads,
  mormot.core.variants,
  mormot.core.os,
  mormot.soa.core,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.core,
  mormot.net.client,
  mormot.rest.http.client,
  u_ServiceInterfaces;

const
  SERVER_URI = 'localhost';
  SERVER_PORT = '8080';

const
  WM_SERVER_CONNECTSTATUS = WM_USER + 1;

type
  TServerConnectStatus = (scsOk, scsErrSynchronizeTimeStamp, scsErrLoginAdminUser, scsErrInitializeServices);

  TDocumentService = class(TObject)
  strict private
    FClient: TRestHttpClient;
  public
    constructor Create(const pmcServerURI: RawUtf8; const pmcServerPort: RawUtf8);
    destructor Destroy; override;
    function InitializeServices: Boolean;
    function Load(pmEditor: TMemo; const pmcDocName: String): Boolean;
    function Save(pmEditor: TMemo; const pmcDocName: String): Boolean;
    procedure GetAllNames(pmFileNames: TStrings);
    property Client: TRestHttpClient
      read FClient;
  end;

type
  TfrmMain = class(TForm)
    pnlHeader: TPanel;
    PageControl: TPageControl;
    tbsMarkdown: TTabSheet;
    tbsHtmlView: TTabSheet;
    WebBrowser: TWebBrowser;
    pnlMarkdown: TPanel;
    memMarkdown: TMemo;
    lblUserName: TLabel;
    edtUserName: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    btnNewConnection: TButton;
    btnNewDocExecute: TButton;
    lblSaveDocName: TLabel;
    edtSaveDocName: TEdit;
    btnSaveDocExecute: TButton;
    lblLoadDocName: TLabel;
    cbbLoadDocName: TComboBox;
    btnLoadDocExecute: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure cbbLoadDocNameDropDown(Sender: TObject);
    procedure btnNewDocExecuteClick(Sender: TObject);
    procedure btnSaveDocExecuteClick(Sender: TObject);
    procedure btnLoadDocExecuteClick(Sender: TObject);
    procedure btnNewConnectionClick(Sender: TObject);
  private
    FServerConnected: Boolean;
    FDocumentService: TDocumentService;
    procedure ExecuteConnectServerInternal(const pmcUserName, pmcPassword: RawUtf8);
    procedure PrepareRestClient(const pmcUserName, pmcPassword: String);
    procedure WaitForWebBrowserModul;
    procedure WebBrowserLoadFromString(const pmcMarkdown: String);
    procedure WMDBEServerConnectStatus(var pmvMessage: TMessage);
      message WM_SERVER_CONNECTSTATUS;
  protected
    procedure UpdateActions; override;
  public
  end;

var
  frmMain: TfrmMain;


implementation

uses
  System.Win.IEInterfaces, System.Win.ComObj, Winapi.ActiveX;

{$R *.dfm}

//==============================================================================
// TDocumentService
//==============================================================================

constructor TDocumentService.Create(const pmcServerURI, pmcServerPort: RawUtf8);
const
  TIMEOUT: Cardinal = 2000;  // 2 sec
begin
  inherited Create;
  FClient := TRestHttpClient.Create(pmcServerURI, pmcServerPort, TOrmModel.Create([], ROOT_NAME_FILE), False, '', '', TIMEOUT, TIMEOUT, TIMEOUT);
  FClient.Model.Owner := FClient;
end;


destructor TDocumentService.Destroy;
begin
  FreeAndNil(FClient);
  inherited Destroy;
end;


function TDocumentService.InitializeServices: Boolean;
begin
  Result := False;
  if FClient.SessionID > 0 then
  try
    // The check before registering the service with ServiceDefine() is only necessary because the
    // user can be changed and the initialization is executed again. This is normally not the case.
    Result := ((FClient.ServiceContainer.Info(IDocument) <> Nil) or FClient.ServiceDefine([IDocument], sicShared));
  except
  end;
end;


function TDocumentService.Load(pmEditor: TMemo; const pmcDocName: String): Boolean;
var
  docData: RawBlob;
  service: IDocument;
begin
  Result := False;
  if pmEditor = Nil then Exit; //=>
  if pmcDocName = '' then Exit; //=>
  if not FClient.Resolve(IDocument, service) then Exit; //=>

  Result := service.Load(pmcDocName, docData);
  if Result and (docData <> '') then
    pmEditor.Lines.Text := Utf8ToString(docData);
end;


function TDocumentService.Save(pmEditor: TMemo; const pmcDocName: String): Boolean;
var
  docData: RawBlob;
  service: IDocument;
begin
  Result := False;
  if pmEditor = Nil then Exit; //=>
  if pmcDocName = '' then Exit; //=>
  if not FClient.Resolve(IDocument, service) then Exit; //=>

  docData := StringToUtf8(pmEditor.Lines.Text);
  if docData <> '' then
    Result := service.Save(pmcDocName, docData);
end;


procedure TDocumentService.GetAllNames(pmFileNames: TStrings);
var
  service: IDocument;
  fileNames: TFileNameDynArray;
begin
  if pmFileNames = Nil then Exit; //=>
  if not FClient.Resolve(IDocument, service) then Exit; //=>

  pmFileNames.BeginUpdate;
  try
    pmFileNames.Clear;
    service.GetAllNames(fileNames);
    for var i: Integer := 0 to High(fileNames) do
      pmFileNames.Add(fileNames[i]);
  finally
    pmFileNames.EndUpdate;
  end;
end;


//==============================================================================
// TfrmMain
//==============================================================================

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$IFDEF ISDELPHI104}
  WebBrowser.SelectedEngine := EdgeIfAvailable;
{$ENDIF}

  FDocumentService := TDocumentService.Create(SERVER_URI, SERVER_PORT);
  memMarkdown.Lines.LoadFromFile(MakePath([Executable.ProgramFilePath, 'TestMarkdown.txt']));
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FDocumentService.Free;
end;


procedure TfrmMain.FormShow(Sender: TObject);
begin
  PrepareRestClient(edtUserName.Text, edtPassword.Text);
end;


procedure TfrmMain.ExecuteConnectServerInternal(const pmcUserName, pmcPassword: RawUtf8);
var
  scsCode: TServerConnectStatus;
begin
  if FDocumentService.Client.ServerTimeStampSynchronize then
  begin
    if FDocumentService.Client.SetUser(pmcUserName, TAuthUser.ComputeHashedPassword(pmcPassword), True) then
    begin
      if FDocumentService.InitializeServices then
        scsCode := scsOk
      else
        scsCode := scsErrInitializeServices;
    end
    else
      scsCode := scsErrLoginAdminUser;
  end
  else
    scsCode := scsErrSynchronizeTimeStamp;

  PostMessage(Handle, WM_SERVER_CONNECTSTATUS, Ord(scsCode), 0);
end;


procedure TfrmMain.PrepareRestClient(const pmcUserName, pmcPassword: String);
begin
  if pmcUserName = '' then Exit; //=>
  if pmcPassword = '' then Exit; //=>

  TThread.CreateAnonymousThread(
    procedure
    begin
      ExecuteConnectServerInternal(StringToUtf8(edtUserName.Text), StringToUtf8(edtPassword.Text));
    end).Start;
end;


procedure TfrmMain.WaitForWebBrowserModul;
begin
  while WebBrowser.Busy = True do
  begin
    Application.ProcessMessages;
    Sleep(0);
  end;
end;


procedure TfrmMain.WebBrowserLoadFromString(const pmcMarkdown: String);
const
  HTML_HEAD: RawUtf8 =
      '<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"></head>';
  HTML_TEMPLATE: RawUtf8 =
      '<!doctype html><html lang="de">%<body>%</body></html>';
var
  doc: RawUtf8;
  docStream: IPersistStreamInit;
begin
  if WebBrowser.Document = Nil then
  begin
    WebBrowser.Navigate('about:blank');
    WaitForWebBrowserModul;
  end;

  if Supports(WebBrowser.Document, IPersistStreamInit, docStream) then
  begin
    if pmcMarkdown <> '' then
    begin
      doc := HtmlEscapeMarkdown(StringToUtf8(pmcMarkdown));  // Convert Markdown to HTML
      doc := FormatUtf8(HTML_TEMPLATE, [HTML_HEAD, doc]);    // Create full HTML document
    end;

    OleCheck(docStream.Load(TStreamAdapter.Create(TRawByteStringStream.Create(doc), soOwned) as IStream));
  end;
end;


procedure TfrmMain.WMDBEServerConnectStatus(var pmvMessage: TMessage);
resourcestring
  SErrNoServerConnection =
      'No connection could be established.';
  SErrServerLogInFailed =
      'Login to server failed.'#10#13'%s';
  SErrInitializeServices =
      'Services are not available.';
begin
  FServerConnected := False;
  case TServerConnectStatus(pmvMessage.WParam) of
    scsErrSynchronizeTimeStamp:
      ShowMessage(SErrNoServerConnection);
    scsErrLoginAdminUser:
      ShowMessage(Format(SErrServerLogInFailed, [Utf8ToString(FDocumentService.Client.LastErrorMessage)]));
    scsErrInitializeServices:
      ShowMessage(SErrInitializeServices);
  else
    FServerConnected := True;
  end;
end;


procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  edtSaveDocName.Enabled := FServerConnected;
  cbbLoadDocName.Enabled := FServerConnected;
  btnSaveDocExecute.Enabled := FServerConnected and (edtSaveDocName.Text <> '');
  btnLoadDocExecute.Enabled := FServerConnected and (cbbLoadDocName.Text <> '');
  btnNewConnection.Enabled := ((edtUserName.Text <> '') and (edtPassword.Text <> ''));
end;


procedure TfrmMain.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = tbsHtmlView then
    WebBrowserLoadFromString(memMarkdown.Lines.Text);
end;


procedure TfrmMain.cbbLoadDocNameDropDown(Sender: TObject);
begin
  FDocumentService.GetAllNames(cbbLoadDocName.Items);
end;


procedure TfrmMain.btnNewConnectionClick(Sender: TObject);
begin
  FServerConnected := False;
  PrepareRestClient(edtUserName.Text, edtPassword.Text);
end;


procedure TfrmMain.btnNewDocExecuteClick(Sender: TObject);
begin
  memMarkdown.Clear;
end;


procedure TfrmMain.btnSaveDocExecuteClick(Sender: TObject);
begin
  if FDocumentService.Save(memMarkdown, edtSaveDocName.Text) then
    edtSaveDocName.Clear
  else
    ShowMessage('Document not saved.');
end;


procedure TfrmMain.btnLoadDocExecuteClick(Sender: TObject);
begin
  if FDocumentService.Load(memMarkdown, cbbLoadDocName.Text) then
  begin
    PageControlChange(Nil);
    cbbLoadDocName.Clear;
  end
  else
    ShowMessage('No document found.');
end;

end.
