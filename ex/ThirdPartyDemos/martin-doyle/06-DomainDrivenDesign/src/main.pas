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
  mormot.soa.core,
  mormot.rest.http.client,
  DomTypes,
  DomServiceInterfaces,
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
    ExampleService: ISampleService;
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
  if ExampleService.AddSample(Sample) = sSuccess then
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
  if ExampleService.FindSample(Sample) = sSuccess then
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
  Model := TOrmModel.Create([]);
  HttpClient := TRestHttpClient.Create('localhost', HTTP_PORT, Model);
  HttpClient.ServiceDefine([ISampleService], sicShared, EXAMPLE_CONTRACT);
  HttpClient.Services[EXAMPLE_CONTRACT].Get(ExampleService);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ExampleService := nil;
  HttpClient.Free;
  Model.Free;
end;

end.













