unit main;

interface

{$I mormot.defines.inc}
uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif MSWINDOWS}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  mormot.core.base, mormot.core.unicode,
  mormot.orm.core,
  mormot.rest.http.client,
  data;

type
  TMainForm = class(TForm)
    ButtonDelete: TButton;
    ButtonFind: TButton;
    ButtonNew: TButton;
    ButtonQuit: TButton;
    ButtonSave: TButton;
    LabelName: TLabel;
    LabelNames: TLabel;
    EditName: TEdit;
    ListNames: TListBox;
    MemoQuestion: TMemo;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonFindClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListNamesClick(Sender: TObject);
  private
    function GetSelectedID: TID;
    procedure RefreshNamesList;
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
function TMainForm.GetSelectedID: TID;
begin
  if ListNames.ItemIndex >= 0 then
    Result := TID(ListNames.Items.Objects[ListNames.ItemIndex])
  else
    Result := 0;
end;

procedure TMainForm.RefreshNamesList;
var
  Rec: TOrmSample;
begin
  ListNames.Items.Clear;
  Rec := TOrmSample.CreateAndFillPrepare(HttpClient.Orm, '', []);
  try
    while Rec.FillOne do
      ListNames.Items.AddObject(UTF8ToString(Rec.Name), TObject(Rec.ID));
  finally
    Rec.Free;
  end;
end;

procedure TMainForm.ButtonNewClick(Sender: TObject);
begin
  EditName.Text := '';
  MemoQuestion.Text := '';
  ListNames.ItemIndex := -1;
  EditName.SetFocus;
end;

procedure TMainForm.ButtonSaveClick(Sender: TObject);
var
  Rec: TOrmSample;
  RecID: TID;
begin
  if ListNames.ItemIndex >= 0 then
  begin
    // Update existing record
    RecID := GetSelectedID;
    Rec := TOrmSample.Create(HttpClient.Orm, RecID);
    try
      Rec.Name := StringToUTF8(EditName.Text);
      Rec.Question := StringToUTF8(MemoQuestion.Text);
      if not HttpClient.Orm.Update(Rec) then
        ShowMessage('Error updating the data');
    finally
      Rec.Free;
    end;
  end
  else
  begin
    // Add new record
    Rec := TOrmSample.Create;
    try
      Rec.Name := StringToUTF8(EditName.Text);
      Rec.Question := StringToUTF8(MemoQuestion.Text);
      if HttpClient.Orm.Add(Rec, true) = 0 then
        ShowMessage('Error adding the data');
    finally
      Rec.Free;
    end;
  end;
  RefreshNamesList;
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
var
  RecID: TID;
begin
  if ListNames.ItemIndex < 0 then
  begin
    ShowMessage('No record selected');
    Exit;
  end;
  if MessageDlg('Delete this record?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  RecID := GetSelectedID;
  if not HttpClient.Orm.Delete(TOrmSample, RecID) then
    ShowMessage('Error deleting the data')
  else
  begin
    EditName.Text := '';
    MemoQuestion.Text := '';
    RefreshNamesList;
  end;
end;

procedure TMainForm.ButtonFindClick(Sender: TObject);
var
  i: Integer;
  SearchName: string;
begin
  SearchName := EditName.Text;
  for i := 0 to ListNames.Items.Count - 1 do
    if SameText(ListNames.Items[i], SearchName) then
    begin
      ListNames.ItemIndex := i;
      ListNamesClick(nil);
      Exit;
    end;
  ShowMessage('Not found');
end;

procedure TMainForm.ButtonQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel;
  HttpClient := TRestHttpClient.Create('localhost', HttpPort, Model);
  RefreshNamesList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  HttpClient.Free;
  Model.Free;
end;

procedure TMainForm.ListNamesClick(Sender: TObject);
var
  Rec: TOrmSample;
  RecID: TID;
begin
  RecID := GetSelectedID;
  if RecID = 0 then
    Exit;
  Rec := TOrmSample.Create(HttpClient.Orm, RecID);
  try
    if Rec.ID > 0 then
    begin
      EditName.Text := UTF8ToString(Rec.Name);
      MemoQuestion.Text := UTF8ToString(Rec.Question);
    end;
  finally
    Rec.Free;
  end;
end;

end.
