unit main;

interface

uses
  {$I mormot.uses.inc}
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
  mormot.soa.core,
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
  private
    ExampleService: IExample;
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
  Sample: TSample;
begin
  Sample.Name := StringToUTF8(NameEdit.Text);
  Sample.Question := StringToUTF8(QuestionMemo.Text);
  if ExampleService.Add(Sample) = 0 then
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
  Sample: TSample;
begin
  Sample.Name := StringToUTF8(NameEdit.Text);
  if ExampleService.Find(Sample) = 0 then
  begin
    QuestionMemo.Text := UTF8ToString(Sample.Question);
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
  HttpClient.ServiceDefine([IExample], sicShared);
  HttpClient.Services['Example'].Get(ExampleService);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ExampleService := nil;
  HttpClient.Free;
  Model.Free;
end;

end.












