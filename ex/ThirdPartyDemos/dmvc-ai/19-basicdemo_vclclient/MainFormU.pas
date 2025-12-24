unit MainFormU;

{$I mormot.defines.inc}

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Messages,
  {$endif}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  mormot.core.base,
  mormot.core.text,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.core.rtti,
  mormot.core.data,
  mormot.orm.core,
  mormot.rest.client,
  mormot.rest.http.client;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    btnHelloWorld: TButton;
    btnDivide: TButton;
    btnHelloPost: TButton;
    lblResult: TLabel;
    memoResponse: TMemo;
    Label2: TLabel;
    edtNum1: TEdit;
    Label3: TLabel;
    edtNum2: TEdit;
    edtPostData: TEdit;
    Label4: TLabel;
    procedure btnHelloWorldClick(Sender: TObject);
    procedure btnDivideClick(Sender: TObject);
    procedure btnHelloPostClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fClient: TRestHttpClientWinHttp;
    procedure ShowResponse(const aText: RawUtf8);
    procedure ShowError(const aError: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  mormot.core.json,
  mormot.core.variants;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create HTTP client pointing to the basicdemo_server
  fClient := TRestHttpClientWinHttp.Create('localhost', '8080', TOrmModel.Create([]));
  memoResponse.Lines.Clear;
  lblResult.Caption := 'Ready';
  edtNum1.Text := '10';
  edtNum2.Text := '20';
  edtPostData.Text := '{"name":"Bob"}';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fClient.Free;
end;

procedure TMainForm.ShowResponse(const aText: RawUtf8);
begin
  memoResponse.Lines.Add(Utf8ToString(aText));
  lblResult.Caption := 'Success';
end;

procedure TMainForm.ShowError(const aError: string);
begin
  memoResponse.Lines.Add('ERROR: ' + aError);
  lblResult.Caption := 'Error';
  ShowMessage(aError);
end;

procedure TMainForm.btnHelloWorldClick(Sender: TObject);
var
  response: RawUtf8;
  status: integer;
  doc: TDocVariantData;
begin
  memoResponse.Lines.Clear;
  lblResult.Caption := 'Calling GET /root/BasicDemoApi/HelloWorld...';
  Application.ProcessMessages;

  try
    // Call GET /root/BasicDemoApi/HelloWorld
    status := fClient.CallBackGet('root/BasicDemoApi/HelloWorld', [], response);

    if status = 200 then
    begin
      // Parse JSON response
      doc.InitJson(response, JSON_FAST);
      memoResponse.Lines.Add('Message: ' + Utf8ToString(doc.U['message']));
      memoResponse.Lines.Add('Time: ' + Utf8ToString(doc.U['time']));
      memoResponse.Lines.Add('');
      memoResponse.Lines.Add('Raw JSON:');
      ShowResponse(response);
    end
    else
    begin
      ShowError(Format('HTTP Status: %d, Response: %s', [status, response]));
    end;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

procedure TMainForm.btnDivideClick(Sender: TObject);
var
  response: RawUtf8;
  status: integer;
  num1, num2: integer;
  doc: TDocVariantData;
  url: RawUtf8;
begin
  memoResponse.Lines.Clear;

  if not TryStrToInt(edtNum1.Text, num1) then
  begin
    ShowError('Invalid number in first field');
    Exit;
  end;

  if not TryStrToInt(edtNum2.Text, num2) then
  begin
    ShowError('Invalid number in second field');
    Exit;
  end;

  url := FormatUtf8('root/BasicDemoApi/Divide?par1=%&par2=%', [num1, num2]);
  lblResult.Caption := 'Calling GET /' + Utf8ToString(url) + '...';
  Application.ProcessMessages;

  try
    // Call GET /root/BasicDemoApi/Divide?par1=X&par2=Y
    status := fClient.CallBackGet(url, [], response);

    if status = 200 then
    begin
      // Parse JSON response
      doc.InitJson(response, JSON_FAST);
      memoResponse.Lines.Add(Format('Result: %d / %d = %.6f',
        [num1, num2, doc.D['result']]));
      memoResponse.Lines.Add('');
      memoResponse.Lines.Add('Raw JSON:');
      ShowResponse(response);
    end
    else
    begin
      ShowError(Format('HTTP Status: %d, Response: %s', [status, response]));
    end;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

procedure TMainForm.btnHelloPostClick(Sender: TObject);
var
  response: RawUtf8;
  status: integer;
  postData: RawUtf8;
  doc: TDocVariantData;
begin
  memoResponse.Lines.Clear;
  lblResult.Caption := 'Calling POST /root/BasicDemoApi/HelloWorldPost...';
  Application.ProcessMessages;

  try
    // Prepare POST data (ensure it's valid JSON)
    postData := StringToUtf8(edtPostData.Text);

    // Call POST /root/BasicDemoApi/HelloWorldPost
    status := fClient.CallBack(mPOST, 'root/BasicDemoApi/HelloWorldPost',
      postData, response);

    if status = 200 then
    begin
      // Parse JSON response
      doc.InitJson(response, JSON_FAST);
      memoResponse.Lines.Add('Data: ' + Utf8ToString(doc.U['data']));
      memoResponse.Lines.Add('Modified: ' + Utf8ToString(doc.U['modified']));
      memoResponse.Lines.Add('');
      memoResponse.Lines.Add('Raw JSON:');
      ShowResponse(response);
    end
    else
    begin
      ShowError(Format('HTTP Status: %d, Response: %s', [status, response]));
    end;
  except
    on E: Exception do
      ShowError(E.Message);
  end;
end;

end.
