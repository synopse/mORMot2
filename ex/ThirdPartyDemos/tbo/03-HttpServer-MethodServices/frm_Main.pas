// Author: Thomas Bogenrieder
// The example is a proof of concept for creating method-based services with mORMot, the source code is neither tested nor optimized.

unit frm_Main;

{$I mormot.defines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
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
  mormot.ui.gdiplus,
  u_SharedTypes;

const
  SERVER_URI = 'localhost';
  SERVER_PORT = '8080';

const
  WM_SERVER_CONNECTSTATUS = WM_USER + 1;

type
  TFileHttpClient = class(TRestHttpClientWinHttp)
  public
    constructor Create(const pmcServerURI: RawUtf8 = SERVER_URI; const pmcServerPort: RawUtf8 = SERVER_PORT); reintroduce;
    function LoadImage(pmImage: TImage; const pmcImageName: String): Boolean;
    function SaveImage(pmImage: TImage; const pmcImageName: String): Boolean;
    procedure GetAllFileNames(pmFileNames: TStrings);
  end;

  TWinHttpProgressMode = (whpUpload, whpDownload);

  TCustomWinHttpProgressGuiHelper = class abstract(TComponent)
  protected
    FRestClient: TRestHttpClientWinHttp;
    FProgressMode: TWinHttpProgressMode;
    function DoUploadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal): Boolean; virtual; abstract;
    procedure DoDownloadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal); virtual; abstract;
  public
    procedure Prepare(pmRestClient: TRestHttpClientWinHttp; pmProgressMode: TWinHttpProgressMode); virtual; abstract;
    procedure Done; virtual; abstract;
  end;

  TfrmMain = class(TForm)
    Image: TImage;
    ScrollBox: TScrollBox;
    ProgressBar: TProgressBar;
    btnNewImage: TButton;
    lblUserName: TLabel;
    edtUserName: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    lblImageName: TLabel;
    edtImageName: TEdit;
    btnSaveImage: TButton;
    btnLoadImage: TButton;
    cbbAllImages: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNewImageClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure cbbAllImagesDropDown(Sender: TObject);
  private
    FRestClient: TFileHttpClient;
    FRestClientConnected: Boolean;
    FProgressHelper: TCustomWinHttpProgressGuiHelper;
    procedure ConnectServerExecuteProc;
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
  System.StrUtils;

{$R *.dfm}

//==============================================================================
// TFileHttpClient
//==============================================================================

constructor TFileHttpClient.Create(const pmcServerURI: RawUtf8; const pmcServerPort: RawUtf8);
const
  DEFAULT_TIMEOUT: Cardinal = 2000;  // 2 sec
var
  ormModel: TOrmModel;
begin
  ormModel := TOrmModel.Create([], ROOT_NAME_FILE);
  inherited Create(pmcServerURI, pmcServerPort, ormModel, {Https=} False, '', '', DEFAULT_TIMEOUT, DEFAULT_TIMEOUT, DEFAULT_TIMEOUT);
  ormModel.Owner := Self;
end;


function TFileHttpClient.LoadImage(pmImage: TImage; const pmcImageName: String): Boolean;
var
  data: RawByteString;
begin
  Result := False;
  if pmImage = Nil then Exit; //=>
  if pmcImageName = '' then Exit; //=>

  if CallBackGet(TFileServiceFunction.Name.LoadFile, [
    TFileServiceFunction.Param.LoadFile_ImageName, StringToUtf8(pmcImageName)], RawUtf8(data)) = HTTP_SUCCESS then
  begin
    var stream: TRawByteStringStream := TRawByteStringStream.Create(data);
    try
      pmImage.Picture.LoadFromStream(stream);
      Result := True;
    finally
      stream.Free
    end;
  end;
end;


function TFileHttpClient.SaveImage(pmImage: TImage; const pmcImageName: String): Boolean;
var
  return: RawUtf8;
begin
  Result := False;
  if pmImage = Nil then Exit; //=>
  if pmcImageName = '' then Exit; //=>

  var stream: TRawByteStringStream := TRawByteStringStream.Create;
  try
    pmImage.Picture.SaveToStream(stream);
    if stream.Position > 0 then
    begin
      var methodName: RawUtf8 := TFileServiceFunction.Name.SaveFile
        + UrlEncode([TFileServiceFunction.Param.SaveFile_ImageName, StringToUtf8(pmcImageName)]);

      Result := (CallBackPut(methodName, stream.DataString, return) = HTTP_SUCCESS);
    end;
  finally
    stream.Free;
  end;
end;


procedure TFileHttpClient.GetAllFileNames(pmFileNames: TStrings);
var
  json: RawUtf8;
  dirFiles: TFileNameDynArray;
begin
  if pmFileNames = Nil then Exit; //=>

  pmFileNames.BeginUpdate;
  try
    pmFileNames.Clear;
    if CallBackGet(TFileServiceFunction.Name.GetAllFileNames, [], json) = HTTP_SUCCESS then
    begin
      if IsValidJson(json)
        and (DynArrayLoadJson(dirFiles, Pointer(json), TypeInfo(TFileNameDynArray)) <> Nil) then
      begin
        for var i: Integer := 0 to High(dirFiles) do
          pmFileNames.Add(dirFiles[i]);
      end;
    end;
  finally
    pmFileNames.EndUpdate;
  end;
end;


//==============================================================================
// TWinHttpProgressGuiHelper
//==============================================================================

type
  TWinHttpProgressGuiHelper = class(TCustomWinHttpProgressGuiHelper)
  private
    FProgressBar: TProgressBar;
    FProgressStepWidth: Integer;
    procedure InternalProgress(pmCurrentSize, pmContentLength: Cardinal);
  protected
    function DoUploadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal): Boolean; override;
    procedure DoDownloadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal); override;
  public
    constructor Create(pmProgressBar: TProgressBar; pmStepWidth: Integer = 5); reintroduce;
    procedure Prepare(pmRestClient: TRestHttpClientWinHttp; pmProgressMode: TWinHttpProgressMode); override;
    procedure Done; override;
  end;


constructor TWinHttpProgressGuiHelper.Create(pmProgressBar: TProgressBar; pmStepWidth: Integer);
begin
  inherited Create(pmProgressBar);

  Assert(pmProgressBar <> Nil);
  FProgressBar := pmProgressBar;
  FProgressStepWidth := pmStepWidth;
end;


procedure TWinHttpProgressGuiHelper.InternalProgress(pmCurrentSize, pmContentLength: Cardinal);
var
  percentDone: Integer;
begin
  if pmContentLength <= 0 then Exit; //=>

  percentDone := Trunc(pmCurrentSize / (pmContentLength / 100));
  if (FProgressBar.Position + FProgressStepWidth) < percentDone then
    FProgressBar.Position := percentDone;
end;


function TWinHttpProgressGuiHelper.DoUploadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal): Boolean;
begin
  Result := True;
  if pmCurrentSize = 0 then
    FProgressBar.Position := 0
  else if pmCurrentSize >= pmContentLength then
    Done
  else
    InternalProgress(pmCurrentSize, pmContentLength);
end;


procedure TWinHttpProgressGuiHelper.DoDownloadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal);
begin
  if pmCurrentSize = 0 then
    FProgressBar.Position := 0
  else if pmCurrentSize >= pmContentLength then
    Done
  else
    InternalProgress(pmCurrentSize, pmContentLength);
end;


procedure TWinHttpProgressGuiHelper.Prepare(pmRestClient: TRestHttpClientWinHttp; pmProgressMode: TWinHttpProgressMode);
begin
  FRestClient := Nil;
  if pmRestClient = Nil then Exit; //=>
  if not (pmRestClient.Request is TWinHttpApi) then Exit; //=>

  FRestClient := pmRestClient;
  FProgressMode := pmProgressMode;

  FProgressBar.Min := 0;
  FProgressBar.Max := 100;
  FProgressBar.Position := 0;
  case FProgressMode of
    whpUpload:
      TWinHttpApi(FRestClient.Request).OnUpload := DoUploadProgress;
    whpDownload:
      TWinHttpApi(FRestClient.Request).OnProgress := DoDownloadProgress;
  end;
end;


procedure TWinHttpProgressGuiHelper.Done;
begin
  FProgressBar.Position := 0;
  if FRestClient <> Nil then
  begin
    case FProgressMode of
      whpUpload:
        TWinHttpApi(FRestClient.Request).OnUpload := Nil;
      whpDownload:
        TWinHttpApi(FRestClient.Request).OnProgress := Nil;
    end;

    FRestClient := Nil;
  end;
end;


//==============================================================================
// TfrmMain
//==============================================================================

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FRestClient := TFileHttpClient.Create;
  // The ProgressBar becomes active only if the action takes a little time.
  FProgressHelper := TWinHttpProgressGuiHelper.Create(ProgressBar);
  Image.Picture.LoadFromFile(MakePath([Executable.ProgramFilePath, 'TestImage.png']));
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FRestClient.Free;
end;


procedure TfrmMain.FormShow(Sender: TObject);
begin
  TThread.CreateAnonymousThread(ConnectServerExecuteProc).Start;
end;


procedure TfrmMain.ConnectServerExecuteProc;
var
  returnCode: Integer;
begin
  if FRestClient.ServerTimeStampSynchronize then
  begin
    if FRestClient.SetUser(StringToUtf8(edtUserName.Text), TAuthUser.ComputeHashedPassword(StringToUtf8(edtPassword.Text)), True) then
      returnCode := HTTP_SUCCESS
    else
      returnCode := HTTP_UNAUTHORIZED;
  end
  else
    returnCode := HTTP_BADREQUEST;

  PostMessage(Handle, WM_SERVER_CONNECTSTATUS, returnCode, 0);
end;


procedure TfrmMain.WMDBEServerConnectStatus(var pmvMessage: TMessage);
resourcestring
  SErrNoServerConnection =
      'No connection could be established.';
  SErrServerLogInFailed =
      'Login to server failed.'#10#13'%s';
begin
  FRestClientConnected := False;
  case pmvMessage.WParam of
    HTTP_BADREQUEST:
      ShowMessage(SErrNoServerConnection);
    HTTP_UNAUTHORIZED:
      ShowMessage(Format(SErrServerLogInFailed, [Utf8ToString(FRestClient.LastErrorMessage)]));
  else
    FRestClientConnected := True;
  end;
end;


procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  edtImageName.Enabled := FRestClientConnected;
  cbbAllImages.Enabled := FRestClientConnected;
  btnSaveImage.Enabled := FRestClientConnected and (edtImageName.Text <> '');
  btnLoadImage.Enabled := FRestClientConnected and (cbbAllImages.Text <> '');
end;


procedure TfrmMain.btnNewImageClick(Sender: TObject);
begin
  with TOpenDialog.Create(Nil) do
  try
    Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
    InitialDir := Executable.ProgramFilePath;
    DefaultExt := GraphicExtension(TGraphic);
    Filter := GraphicFilter(TGraphic);
    if Execute then
      Image.Picture.LoadFromFile(FileName);
  finally
    Free;
  end;
end;


procedure TfrmMain.btnSaveImageClick(Sender: TObject);
begin
  FProgressHelper.Prepare(FRestClient, whpUpload);
  try
    FRestClient.SaveImage(Image, edtImageName.Text);
  finally
    FProgressHelper.Done;
  end;
end;


procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  FProgressHelper.Prepare(FRestClient, whpDownload);
  try
    FRestClient.LoadImage(Image, cbbAllImages.Text);
  finally
    FProgressHelper.Done;
  end;
end;


procedure TfrmMain.cbbAllImagesDropDown(Sender: TObject);
begin
  if Sender is TComboBox then
    FRestClient.GetAllFileNames(TComboBox(Sender).Items);
end;


//==============================================================================

initialization
  RegisterSynPictures;

end.
