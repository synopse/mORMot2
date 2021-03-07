unit main;

interface

{$I mormot.defines.inc}
uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif MSWINDOWS}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  mormot.core.base,
  mormot.core.data,
  mormot.core.json,
  mormot.core.unicode,
  mormot.orm.core,
  mormot.rest.http.client,
  data;

type
  TMainForm = class(TForm)
    ButtonAdd: TButton;
    ButtonFind: TButton;
    ButtonQuit: TButton;
    LabelEdit: TLabel;
    NameEdit: TEdit;
    QuestionMemo: TMemo;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonFindClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    HttpClient: TRestHttpClient;
    Model: TOrmModel;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{
********************************** TMainForm ***********************************
}
procedure TMainForm.ButtonAddClick(Sender: TObject);
var
  Response: RawUTF8;
  Sample: TSample;
begin
  Sample.Name := StringToUTF8(NameEdit.Text);
  Sample.Question := StringToUTF8(QuestionMemo.Text);

  if HttpClient.CallBackPut('example', RecordSave(Sample, TypeInfo(TSample)), Response) = HTTP_CREATED then
  begin
    NameEdit.Text := '';
    QuestionMemo.Text := '';
    NameEdit.SetFocus;
  end
  else
    ShowMessage('Error adding the data');
end;

procedure TMainForm.ButtonFindClick(Sender: TObject);
var
  Response, Question: RawUTF8;
begin
  if HttpClient.CallBackGet('example', ['Name', StringToUTF8(NameEdit.Text)], Response) = HTTP_SUCCESS then
  begin
    Question := JSONDecode(Response, 'Result', nil, false);
    QuestionMemo.Text := UTF8ToString(Question);
  end
  else
    QuestionMemo.Text := 'Not found';
end;

procedure TMainForm.ButtonQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel;
  HttpClient := TRestHttpClient.Create('localhost', HttpPort, Model);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  HttpClient.Free;
  Model.Free;
end;

end.










