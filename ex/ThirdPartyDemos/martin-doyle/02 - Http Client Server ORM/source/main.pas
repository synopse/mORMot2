unit main;

interface

{$I mormot.defines.inc}
uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif MSWINDOWS}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
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
  Rec: TOrmSample;
begin
  Rec := TOrmSample.Create;
  try
    Rec.Name := StringToUTF8(NameEdit.Text);
    Rec.Question := StringToUTF8(QuestionMemo.Text);
    if HttpClient.Orm.Add(Rec,true)=0 then
      ShowMessage('Error adding the data') else begin
      NameEdit.Text := '';
      QuestionMemo.Text := '';
      NameEdit.SetFocus;
    end;
  finally
    Rec.Free;
  end;
end;

procedure TMainForm.ButtonFindClick(Sender: TObject);
var
  Rec: TOrmSample;
begin
  Rec := TOrmSample.Create(HttpClient.Orm,'Name=?',[StringToUTF8(NameEdit.Text)]);
  try
    if Rec.ID=0 then
      QuestionMemo.Text := 'Not found' else
      QuestionMemo.Text := UTF8ToString(Rec.Question);
  finally
    Rec.Free;
  end;
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






